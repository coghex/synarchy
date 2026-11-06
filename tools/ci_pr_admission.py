#!/usr/bin/env python3
"""Decide whether a pull request's CI may do expensive work (#2927).

The full CI pipeline takes about half an hour. Running it on every pull-request
push wasted most of that: every review round pushes a new head, the head's
result is superseded before anyone acts on it, and an approved head that fell
behind its base paid for a second full run after the branch update. The rule
this module implements is the owner's (2026-11-05): CI does not run until the
branch is up to date and the review is approved at that head.

What "admitted" means
---------------------
A pull-request head is ADMITTED, and only then do `test-and-audits`,
`headless-lanes`, `static-audits`, `behavior-probe-worker` (and the image
resolution in front of them) run, when ALL of these hold:

  1. the pull request is open and its live head is the head this run is for;
  2. it carries a live `reviewed:approve` label, read from GitHub, not from
     the event payload (a payload is a snapshot taken before anything else
     ran);
  3. every review-gate.yml run for that exact head has settled, and the
     newest real `dismiss-stale-approval` outcome (a synchronize's) has
     COMPLETED and succeeded; a synchronize must have one. A head with no
     registered review-gate run, or a run whose jobs are not listed yet, is
     pending, not absent. The label is read after that, because neither the
     payload's labels nor a green dismissal alone proves approval -- a
     successful STRIP is green too;
  4. the head contains the base branch's current tip, checked against the
     exact head SHA after a fresh fetch. The default pull_request checkout is
     a synthetic merge of head and base, which contains the base even when the
     branch itself is behind it, so `HEAD` of that checkout proves nothing.

A master push is always admitted. A `workflow_dispatch` run is admitted under
the same rules for the pull request it names, and only when the dispatched ref
IS that pull request's head -- dispatch is not a bypass.

Anything missing, failed, still pending after the wait, or unverifiable is NOT
admitted. Not admitted never reads as a pass: the always-running `build-test`
and `behavior-probes` required contexts fail with the reason.

Subcommands
-----------
  admit      the `admission` job's step: decide, wait for the dismissal, write
             `eligible`, `mode`, `reason`, `base_sha`, `head_sha` outputs.
  freshness  `review-approved`'s step: does this head contain the base tip?
  --self-test

Exit codes: `admit` exits 0 whether or not it admits (the verdict is the
`eligible` output; a crash is a non-zero exit and therefore not admitted).
"""

from __future__ import annotations

import argparse
import json
import os
import re
import subprocess
import sys
import tempfile
import time
from pathlib import Path
from typing import Callable, NamedTuple, Sequence

APPROVAL_LABEL = "reviewed:approve"

#: review-gate.yml's dismissal job; its check run carries this name.
DISMISSAL_CHECK = "dismiss-stale-approval"

CURRENT = "current"
BEHIND = "behind"
UNVERIFIED = "unverified"

MODE_PUSH = "push"
MODE_PR = "pr"
MODE_NONE = "none"

REPO_ROOT = Path(__file__).resolve().parent.parent
CI_WORKFLOW = REPO_ROOT / ".github" / "workflows" / "ci.yml"

#: The one predicate for "this event can create an admissible state". The
#: concurrency group, `admission`, and the two always-running required
#: contexts of ci.yml repeat it verbatim; `--self-test` pins that they do.
ADMITTING_EXPRESSION = (
    "github.event_name != 'pull_request' "
    "|| github.event.action == 'synchronize' "
    "|| (github.event.action == 'labeled' "
    "&& github.event.label.name == 'reviewed:approve')"
)

_SHA = re.compile(r"[0-9a-f]{40}")
_REF = re.compile(r"[A-Za-z0-9_][A-Za-z0-9_./-]*")


class Event(NamedTuple):
    name: str
    action: str
    label: str
    run_sha: str
    pr_number: str
    pr_head_sha: str
    pr_base_sha: str
    pr_base_ref: str


class PullRequest(NamedTuple):
    state: str
    head_sha: str
    base_sha: str
    base_ref: str
    labels: frozenset


class Dismissal(NamedTuple):
    status: str
    conclusion: str


class Admission(NamedTuple):
    eligible: bool
    mode: str
    code: str
    detail: str
    head_sha: str = ""
    base_sha: str = ""

    @property
    def reason(self) -> str:
        return f"[{self.code}] {self.detail}"


def is_admitting_event(event: Event) -> bool:
    """ADMITTING_EXPRESSION, in Python (the self-test checks they agree)."""
    return (event.name != "pull_request"
            or event.action == "synchronize"
            or (event.action == "labeled" and event.label == APPROVAL_LABEL))


def no(mode: str, code: str, detail: str, head: str = "", base: str = "") -> Admission:
    return Admission(False, mode, code, detail, head, base)


def decide(event: Event, pull: PullRequest | None,
           dismissal: Dismissal | None, freshness: str,
           dismissal_readable: bool = True) -> Admission:
    """The whole admission rule. Pure: every input is evidence already read.

    Every branch that cannot positively prove the green direction returns a
    NOT-admitted answer with its own reason code.
    """
    if event.name == "push":
        return Admission(True, MODE_PUSH, "master-push",
                         "a push to master always runs the full pipeline")
    if event.name == "pull_request":
        if not is_admitting_event(event):
            return no(MODE_NONE, "unrelated-event",
                      f"a {event.action!r} event cannot admit CI")
        run_head = event.pr_head_sha
    elif event.name == "workflow_dispatch":
        run_head = event.run_sha
    else:
        return no(MODE_NONE, "unsupported-event", f"event {event.name!r}")

    if not _SHA.fullmatch(run_head or ""):
        return no(MODE_PR, "head-unresolved",
                  f"the run's head {run_head!r} is not a commit SHA")
    if not event.pr_number.isdigit():
        return no(MODE_PR, "pr-identity-unresolved",
                  f"pull request number {event.pr_number!r} is not a number",
                  run_head)
    if pull is None:
        return no(MODE_PR, "pr-identity-unresolved",
                  f"pull request #{event.pr_number} could not be read from GitHub",
                  run_head)
    base_sha = event.pr_base_sha if event.name == "pull_request" else pull.base_sha
    if pull.state != "open":
        return no(MODE_PR, "pr-not-open",
                  f"pull request #{event.pr_number} is {pull.state!r}", run_head, base_sha)
    if pull.head_sha != run_head:
        code = ("head-moved" if event.name == "pull_request"
                else "dispatch-ref-not-pr-head")
        return no(MODE_PR, code,
                  f"pull request #{event.pr_number}'s head is {pull.head_sha[:12]}, "
                  f"but this run is for {run_head[:12]}"
                  + ("" if event.name == "pull_request"
                     else "; dispatch the pull request's own branch"),
                  run_head, base_sha)
    if APPROVAL_LABEL not in pull.labels:
        return no(MODE_PR, "not-approved",
                  f"{APPROVAL_LABEL} is not on this head: not approved at this head",
                  run_head, base_sha)

    is_sync = event.name == "pull_request" and event.action == "synchronize"
    if not dismissal_readable:
        # Unreadable evidence is not verified absence: on a labeled or
        # dispatch run "no dismissal run" is acceptable only when GitHub
        # actually answered that there is none.
        return no(MODE_PR, "dismissal-unreadable",
                  f"the {DISMISSAL_CHECK} state for this head could not be read "
                  "from GitHub, so it is unverified", run_head, base_sha)
    if dismissal is None:
        if is_sync:
            return no(MODE_PR, "dismissal-missing",
                      f"no completed {DISMISSAL_CHECK} run was found for this head; "
                      "the stale-approval outcome is unverifiable",
                      run_head, base_sha)
    elif dismissal.status != "completed":
        return no(MODE_PR, "dismissal-pending",
                  f"{DISMISSAL_CHECK} is still {dismissal.status!r} for this head",
                  run_head, base_sha)
    elif dismissal.conclusion != "success":
        return no(MODE_PR, "dismissal-failed",
                  f"{DISMISSAL_CHECK} concluded {dismissal.conclusion!r} for this head",
                  run_head, base_sha)

    if freshness == BEHIND:
        return no(MODE_PR, "behind-base",
                  "behind base: this head does not contain the base branch's current "
                  "tip; update the branch", run_head, base_sha)
    if freshness != CURRENT:
        return no(MODE_PR, "freshness-unverified",
                  "could not verify that this head contains the base branch's tip",
                  run_head, base_sha)
    return Admission(True, MODE_PR, "approved-up-to-date",
                     f"approved at {run_head[:12]} and up to date with the base",
                     run_head, base_sha)


# ---------------------------------------------------------------------------
# Evidence: the two seams (`gh`, `git`) and what is read through them.
# ---------------------------------------------------------------------------

Runner = Callable[..., "tuple[int, str, str]"]


def _run(program: str, *args: str) -> tuple[int, str, str]:
    try:
        done = subprocess.run((program, *args), capture_output=True, text=True,
                              check=False)
    except OSError as error:
        return 127, "", str(error)
    return done.returncode, done.stdout, done.stderr.strip()


def gh(*args: str) -> tuple[int, str, str]:
    return _run("gh", *args)


def git(*args: str) -> tuple[int, str, str]:
    return _run("git", *args)


def fetch_pull(cli: Runner, repo: str, number: str) -> PullRequest | None:
    """The pull request's CURRENT state; None when it cannot be read."""
    rc, out, _ = cli("api", f"repos/{repo}/pulls/{number}")
    if rc != 0:
        return None
    try:
        doc = json.loads(out)
        return PullRequest(
            state=str(doc["state"]),
            head_sha=str(doc["head"]["sha"]),
            base_sha=str(doc["base"]["sha"]),
            base_ref=str(doc["base"]["ref"]),
            labels=frozenset(str(item["name"]) for item in doc["labels"]))
    except (ValueError, KeyError, TypeError):
        return None


def fetch_dismissal(cli: Runner, repo: str, sha: str) -> tuple[bool, Dismissal | None]:
    """The stale-approval outcome for `sha`, from review-gate.yml's own runs.

    Answers (readable, outcome): a completed Dismissal; a not-completed one
    while any review-gate run for this head has not SETTLED; or None, which
    means every review-gate run for the head settled and none of them ran a
    dismissal (the head never went through a synchronize). An unreadable
    answer is never "none".

    It reads the workflow RUNS and their jobs rather than check runs, because
    a run exists the moment its event fires while its checks register a
    little later: an empty check-runs answer cannot tell "this head was never
    synchronized" from "its dismissal has not registered yet" (a labeled or
    dispatched run that read the label in that window would admit a head the
    pending dismissal was about to strip). A run whose jobs are not yet
    listed, or a head with no review-gate run at all, is therefore PENDING,
    not absent.

    The dismissal job runs only on `synchronize`; on `opened` and `labeled`
    events GitHub records it as a SKIPPED job. A skipped job is a settled
    absence of a dismissal and never an outcome, so it can neither mask a
    real result nor stand in for one.
    """
    rc, out, _ = cli("api", "-X", "GET",
                     f"repos/{repo}/actions/workflows/review-gate.yml/runs",
                     "-f", f"head_sha={sha}", "-f", "event=pull_request",
                     "-f", "per_page=100")
    if rc != 0:
        return False, None
    try:
        run_ids = [int(item["id"]) for item in json.loads(out)["workflow_runs"]]
    except (ValueError, KeyError, TypeError):
        return False, None
    unsettled = Dismissal("queued", "")
    if not run_ids:
        return True, unsettled
    outcomes: list[tuple[int, Dismissal]] = []
    for run_id in run_ids:
        rc, out, _ = cli("api", "-X", "GET",
                         f"repos/{repo}/actions/runs/{run_id}/jobs",
                         "-f", "per_page=100")
        if rc != 0:
            return False, None
        try:
            jobs = [job for job in json.loads(out)["jobs"]
                    if job.get("name") == DISMISSAL_CHECK]
        except (ValueError, KeyError, TypeError):
            return False, None
        if not jobs:
            return True, unsettled
        for job in jobs:
            if job.get("status") != "completed":
                return True, Dismissal(str(job.get("status") or "queued"), "")
            if job.get("conclusion") == "skipped":
                continue
            outcomes.append((int(job.get("id", 0)),
                             Dismissal("completed", str(job.get("conclusion") or ""))))
    if not outcomes:
        return True, None
    return True, max(outcomes, key=lambda pair: pair[0])[1]


def await_dismissal(cli: Runner, repo: str, sha: str, *, require: bool,
                    wait_seconds: float, poll_seconds: float,
                    sleep: Callable[[float], None] = time.sleep,
                    clock: Callable[[], float] = time.monotonic
                    ) -> tuple[bool, Dismissal | None]:
    """Wait for the stale-approval outcome for `sha` to be completed.

    On a synchronize the run must exist, so a missing one is waited for (it
    starts concurrently with CI). Elsewhere a missing run means the head never
    went through a synchronize and is accepted, but one that exists and is
    unfinished is still waited for. An unreadable API is retried like a
    missing run. Returns (readable, run): `readable` is whether the LAST poll
    could be read at all, and `run` the last seen state (completed, still
    pending at the deadline, or None for confirmed absence). Unreadable is
    never conflated with absent.
    """
    deadline = clock() + wait_seconds
    last: Dismissal | None = None
    while True:
        readable, run = fetch_dismissal(cli, repo, sha)
        if readable:
            last = run
            if run is not None and run.status == "completed":
                return True, run
            if run is None and not require:
                return True, None
        if clock() >= deadline:
            return readable, (last if readable else None)
        sleep(poll_seconds)


def base_freshness(run_git: Runner, head_sha: str, base_ref: str) -> str:
    """Does `head_sha` contain the base branch's CURRENT tip?

    Fetches the base ref fresh and asks git about the exact head commit,
    never about whatever HEAD the checkout happens to be.
    """
    if not _SHA.fullmatch(head_sha or "") or not _REF.fullmatch(base_ref or ""):
        return UNVERIFIED
    remote = f"refs/remotes/origin/{base_ref}"
    rc, _, _ = run_git("fetch", "--no-tags", "--quiet", "origin",
                       f"+refs/heads/{base_ref}:{remote}")
    if rc != 0:
        return UNVERIFIED
    if run_git("cat-file", "-e", f"{head_sha}^{{commit}}")[0] != 0:
        return UNVERIFIED
    rc, _, _ = run_git("merge-base", "--is-ancestor", remote, head_sha)
    return CURRENT if rc == 0 else BEHIND if rc == 1 else UNVERIFIED


def run_admit(event: Event, repo: str, *, cli: Runner = gh, run_git: Runner = git,
              wait_seconds: float = 900, poll_seconds: float = 15,
              sleep: Callable[[float], None] = time.sleep,
              clock: Callable[[], float] = time.monotonic) -> Admission:
    """Gather the evidence in the right order, then decide.

    The order is the point: the dismissal outcome first, the LIVE pull
    request (head and labels) after it, so the label read cannot predate the
    strip it is meant to reflect.
    """
    if event.name not in ("pull_request", "workflow_dispatch") \
            or (event.name == "pull_request" and not is_admitting_event(event)):
        return decide(event, None, None, UNVERIFIED)
    run_head = event.pr_head_sha if event.name == "pull_request" else event.run_sha
    dismissal, readable = None, True
    if _SHA.fullmatch(run_head or "") and event.pr_number.isdigit():
        readable, dismissal = await_dismissal(
            cli, repo, run_head,
            require=(event.name == "pull_request" and event.action == "synchronize"),
            wait_seconds=wait_seconds, poll_seconds=poll_seconds,
            sleep=sleep, clock=clock)
    pull = (fetch_pull(cli, repo, event.pr_number)
            if event.pr_number.isdigit() else None)
    base_ref = pull.base_ref if pull is not None else event.pr_base_ref
    freshness = base_freshness(run_git, run_head, base_ref)
    return decide(event, pull, dismissal, freshness, readable)


def _single_line(text: str) -> str:
    return " ".join(text.split())


def _emit(outputs: dict[str, str], destination: str) -> None:
    if not destination:
        return
    with open(destination, "a", encoding="utf-8") as handle:
        for key, value in outputs.items():
            handle.write(f"{key}={_single_line(value)}\n")


def _admit_main(args: argparse.Namespace) -> int:
    event = Event(args.event_name, args.action, args.label, args.run_sha,
                  args.pr_number, args.pr_head_sha, args.pr_base_sha,
                  args.pr_base_ref)
    result = run_admit(event, args.repo, wait_seconds=args.wait_seconds,
                       poll_seconds=args.poll_seconds)
    line = (f"CI admission: {'ADMITTED' if result.eligible else 'NOT admitted'} "
            f"(mode {result.mode}) {result.reason}")
    print(line)
    _emit({"eligible": "true" if result.eligible else "false",
           "mode": result.mode, "reason": result.reason,
           "base_sha": result.base_sha, "head_sha": result.head_sha},
          args.github_output)
    if args.github_summary:
        with open(args.github_summary, "a", encoding="utf-8") as handle:
            handle.write(f"### CI admission\n\n{line}\n")
    return 0


def _freshness_main(args: argparse.Namespace) -> int:
    verdict = base_freshness(git, args.head_sha, args.base_ref)
    print(f"base freshness of {args.head_sha[:12]} against {args.base_ref}: {verdict}")
    _emit({"freshness": verdict}, args.github_output)
    return 0


# ---------------------------------------------------------------------------
# Self-test
# ---------------------------------------------------------------------------

HEAD = "a" * 40
OTHER = "b" * 40
BASE = "c" * 40


def _event(name: str = "pull_request", action: str = "synchronize", *,
           label: str = "", pr: str = "7", head: str = HEAD,
           run_sha: str = "d" * 40) -> Event:
    return Event(name, action, label, run_sha if name != "workflow_dispatch" else head,
                 pr, head if name == "pull_request" else "", BASE, "master")


def _pull(**over: object) -> PullRequest:
    values: dict = dict(state="open", head_sha=HEAD, base_sha=BASE, base_ref="master",
                        labels=frozenset({APPROVAL_LABEL}))
    values.update(over)
    return PullRequest(**values)


_OK = Dismissal("completed", "success")


def _translate(text: str, ctx: dict) -> object:
    """Evaluate a GitHub Actions expression over `ctx` (self-test only).

    Covers exactly the grammar ci.yml's routing uses: string literals,
    `== != && || ! ( )`, dotted context paths and `format()`. `&&`/`||`
    return operand values, as in Actions, because the concurrency group
    relies on that.
    """
    text = text.strip()
    if text.startswith("${{") and text.endswith("}}"):
        text = text[3:-2]
    text = text.replace("always()", "true")  # status functions: scheduling is not modelled
    pieces: list[str] = []
    pattern = re.compile(r"'(?:[^']|'')*'|&&|\|\||==|!=|!|\(|\)|,|[A-Za-z_][\w.-]*|\d+|\s+")
    position = 0
    while position < len(text):
        match = pattern.match(text, position)
        if match is None:
            raise ValueError(f"cannot translate {text[position:position + 20]!r}")
        token = match.group(0)
        position = match.end()
        if token.isspace():
            pieces.append(" ")
        elif token == "&&":
            pieces.append(" and ")
        elif token == "||":
            pieces.append(" or ")
        elif token == "!":
            pieces.append(" not ")
        elif token == "format":
            pieces.append("_format")
        elif token in ("true", "false", "null"):
            pieces.append({"true": "True", "false": "False", "null": "None"}[token])
        elif re.fullmatch(r"[A-Za-z_][\w.-]*", token):
            pieces.append(f"_ctx.get({token!r}, '')")
        else:
            pieces.append(token)
    return eval("".join(pieces),  # noqa: S307 - self-test over our own workflow text
                {"__builtins__": {}, "_ctx": ctx,
                 "_format": lambda template, *a: template.format(*a)})


def _ci_document(path: Path = CI_WORKFLOW) -> dict:
    import yaml  # type: ignore
    return yaml.safe_load(path.read_text(encoding="utf-8"))


def _script(job: dict, step_name: str) -> tuple[str, dict]:
    for step in job["steps"]:
        if step.get("name") == step_name:
            return step["run"], step.get("env", {})
    raise KeyError(step_name)


def _verdict(script: str, env: dict) -> int:
    done = subprocess.run(["bash", "--noprofile", "--norc", "-eo", "pipefail", "-c", script],
                          env={"PATH": os.environ.get("PATH", ""), **env},
                          capture_output=True, text=True, check=False)
    return done.returncode


def _self_test(workflow: Path = CI_WORKFLOW) -> int:  # noqa: C901 - a flat list of cases reads best flat
    failures: list[str] = []

    def check(label: str, actual: object, expected: object) -> None:
        if actual != expected:
            failures.append(f"{label}: got {actual!r}, want {expected!r}")

    # -- decide: every case the issue lists, plus the corrections' ones ----
    cases: list[tuple[str, Event, PullRequest | None, Dismissal | None, str, bool, str]] = [
        ("unapproved head", _event("pull_request", "synchronize"),
         _pull(labels=frozenset({"tooling"})), _OK, CURRENT, False, "not-approved"),
        ("approved head behind its base", _event("pull_request", "labeled", label=APPROVAL_LABEL),
         _pull(), None, BEHIND, False, "behind-base"),
        ("approved, up to date, labeled", _event("pull_request", "labeled", label=APPROVAL_LABEL),
         _pull(), None, CURRENT, True, "approved-up-to-date"),
        ("synchronize that kept the label", _event("pull_request", "synchronize"),
         _pull(), _OK, CURRENT, True, "approved-up-to-date"),
        ("synchronize that stripped it", _event("pull_request", "synchronize"),
         _pull(labels=frozenset()), _OK, CURRENT, False, "not-approved"),
        ("synchronize, dismissal never seen", _event("pull_request", "synchronize"),
         _pull(), None, CURRENT, False, "dismissal-missing"),
        ("synchronize, dismissal still running", _event("pull_request", "synchronize"),
         _pull(), Dismissal("in_progress", ""), CURRENT, False, "dismissal-pending"),
        ("synchronize, dismissal failed", _event("pull_request", "synchronize"),
         _pull(), Dismissal("completed", "failure"), CURRENT, False, "dismissal-failed"),
        ("synchronize, dismissal cancelled", _event("pull_request", "synchronize"),
         _pull(), Dismissal("completed", "cancelled"), CURRENT, False, "dismissal-failed"),
        ("labeled head whose dismissal failed", _event("pull_request", "labeled", label=APPROVAL_LABEL),
         _pull(), Dismissal("completed", "failure"), CURRENT, False, "dismissal-failed"),
        ("labeled head whose dismissal is pending", _event("pull_request", "labeled", label=APPROVAL_LABEL),
         _pull(), Dismissal("queued", ""), CURRENT, False, "dismissal-pending"),
        ("labeled after a strip and re-approval", _event("pull_request", "labeled", label=APPROVAL_LABEL),
         _pull(), _OK, CURRENT, True, "approved-up-to-date"),
        ("head moved since the event", _event("pull_request", "labeled", label=APPROVAL_LABEL),
         _pull(head_sha=OTHER), None, CURRENT, False, "head-moved"),
        ("pull request closed", _event("pull_request", "synchronize"),
         _pull(state="closed"), _OK, CURRENT, False, "pr-not-open"),
        ("pull request unreadable", _event("pull_request", "synchronize"),
         None, _OK, CURRENT, False, "pr-identity-unresolved"),
        ("freshness unverifiable", _event("pull_request", "synchronize"),
         _pull(), _OK, UNVERIFIED, False, "freshness-unverified"),
        ("unrelated label event", _event("pull_request", "labeled", label="tooling"),
         _pull(), _OK, CURRENT, False, "unrelated-event"),
        ("master push", _event("push", ""), None, None, UNVERIFIED, True, "master-push"),
        ("dispatch, approved and current", _event("workflow_dispatch", ""),
         _pull(), None, CURRENT, True, "approved-up-to-date"),
        ("dispatch, unapproved", _event("workflow_dispatch", ""),
         _pull(labels=frozenset()), None, CURRENT, False, "not-approved"),
        ("dispatch, behind", _event("workflow_dispatch", ""),
         _pull(), None, BEHIND, False, "behind-base"),
        ("dispatch of a ref that is not the PR head", _event("workflow_dispatch", ""),
         _pull(head_sha=OTHER), None, CURRENT, False, "dispatch-ref-not-pr-head"),
        ("dispatch with no pull request number", _event("workflow_dispatch", "", pr=""),
         _pull(), None, CURRENT, False, "pr-identity-unresolved"),
        ("dispatch with a non-numeric number", _event("workflow_dispatch", "", pr="7; x"),
         _pull(), None, CURRENT, False, "pr-identity-unresolved"),
        ("dispatch naming a missing pull request", _event("workflow_dispatch", ""),
         None, None, CURRENT, False, "pr-identity-unresolved"),
        ("dispatch with a dismissal that is still running", _event("workflow_dispatch", ""),
         _pull(), Dismissal("in_progress", ""), CURRENT, False, "dismissal-pending"),
        ("unknown event", _event("schedule", ""), None, None, CURRENT, False, "unsupported-event"),
    ]
    for label, event in (
            ("labeled", _event("pull_request", "labeled", label=APPROVAL_LABEL)),
            ("dispatch", _event("workflow_dispatch", "")),
            ("synchronize", _event("pull_request", "synchronize"))):
        unread = decide(event, _pull(), None, CURRENT, False)
        check(f"decide/unreadable dismissal on {label}: eligible", unread.eligible, False)
        check(f"decide/unreadable dismissal on {label}: code", unread.code, "dismissal-unreadable")
    decided: dict[str, Admission] = {}
    for label, event, pull, dismissal, freshness, want, code in cases:
        result = decide(event, pull, dismissal, freshness)
        decided[label] = result
        check(f"decide/{label}: eligible", result.eligible, want)
        check(f"decide/{label}: code", result.code, code)
    check("decide: a PR run carries the payload base", decided["synchronize that kept the label"].base_sha, BASE)
    check("decide: dispatch uses the live base", decided["dispatch, approved and current"].base_sha, BASE)

    # -- the Python predicate and the expression agree on every event ------
    for name, action, label in (
            ("push", "", ""), ("workflow_dispatch", "", ""),
            ("pull_request", "synchronize", ""), ("pull_request", "opened", ""),
            ("pull_request", "labeled", APPROVAL_LABEL), ("pull_request", "labeled", "tooling"),
            ("pull_request", "labeled", "reviewed:changes")):
        context = {"github.event_name": name, "github.event.action": action,
                   "github.event.label.name": label}
        event = Event(name, action, label, "", "", "", "", "")
        check(f"predicate/{name}/{action}/{label}",
              bool(_translate(ADMITTING_EXPRESSION, context)), is_admitting_event(event))

    # -- await_dismissal / fetch_dismissal over a scripted review-gate ------
    def job(identifier: int, status: str, conclusion: str | None) -> dict:
        return {"id": identifier, "name": DISMISSAL_CHECK, "status": status,
                "conclusion": conclusion}

    def gate_run(run_id: int, *jobs: dict) -> tuple[int, list]:
        return run_id, list(jobs)  # no jobs: the run exists, its jobs are not listed yet

    class Gate:
        """A `gh` whose review-gate answers advance one snapshot per poll.

        A snapshot is the list of review-gate runs for the head at that
        moment, each with the jobs GitHub lists for it. `pull` is the live
        pull request; `snapshots=None` makes the Actions API unreadable.
        """

        def __init__(self, *snapshots: list, pull: str | None = None) -> None:
            self.snapshots = list(snapshots) if snapshots else None
            self.pull = pull
            self.polls = -1
            self.order: list[str] = []

        def current(self) -> list:
            return self.snapshots[min(self.polls, len(self.snapshots) - 1)]

        def __call__(self, *args: str) -> tuple[int, str, str]:
            text = " ".join(args)
            if "review-gate.yml/runs" in text:
                self.order.append("dismissal")
                if self.snapshots is None:
                    return 1, "", "boom"
                self.polls += 1
                return 0, json.dumps({"workflow_runs": [
                    {"id": run_id} for run_id, _ in self.current()]}), ""
            match = re.search(r"/actions/runs/(\d+)/jobs", text)
            if match:
                if self.snapshots is None:
                    return 1, "", "boom"
                jobs = dict(self.current())[int(match.group(1))]
                return 0, json.dumps({"jobs": jobs}), ""
            self.order.append("pull")
            return (0, self.pull, "") if self.pull is not None else (1, "", "boom")

    ticks = iter(range(10_000))
    fake_clock = lambda: float(next(ticks))  # noqa: E731
    nap = lambda _seconds: None  # noqa: E731

    def wait(gate: Gate, *, require: bool, limit: float = 12):
        return await_dismissal(gate, "o/r", HEAD, require=require, wait_seconds=limit,
                               poll_seconds=1, sleep=nap, clock=fake_clock)

    ok_job = job(10, "completed", "success")
    skip_job = job(20, "completed", "skipped")
    gate = Gate([], [gate_run(1)], [gate_run(1, job(10, "in_progress", None))],
                [gate_run(1, ok_job)])
    check("await: delayed dismissal is waited for", wait(gate, require=True), (True, _OK))
    check("await: it kept polling until completed", gate.polls + 1, 4)
    gate = Gate([])
    check("await: a head with no review-gate run yet is pending, never absent",
          wait(gate, require=True), (True, Dismissal("queued", "")))
    gate = Gate([])
    check("await: ... on a non-synchronize too",
          wait(gate, require=False), (True, Dismissal("queued", "")))
    gate = Gate([gate_run(1)])
    check("await: a run whose jobs are not registered yet is pending",
          wait(gate, require=False), (True, Dismissal("queued", "")))
    # The race round 2 of #2928 found: the labeled/dispatch run polls while
    # the synchronize's dismissal has not registered, then it appears.
    for require in (False, True):
        gate = Gate([], [gate_run(1)], [gate_run(1, job(10, "in_progress", None))], [gate_run(1, ok_job)])
        check(f"await: delayed registration is waited for (require={require})",
              wait(gate, require=require), (True, _OK))
        check(f"await: ... and was not read just once (require={require})", gate.polls > 0, True)
    gate = Gate([gate_run(1, skip_job)])
    check("await: initial approval (only skipped dismissals) is accepted at once",
          wait(gate, require=False), (True, None))
    check("await: ... without polling again", gate.polls + 1, 1)
    gate = Gate([gate_run(1, skip_job)])
    check("await: only skipped dismissals on a synchronize wait out as missing",
          wait(gate, require=True), (True, None))
    gate = Gate([gate_run(1, ok_job), gate_run(2, job(30, "completed", "skipped"))])
    check("await: a newer skipped run does not mask a successful synchronize",
          wait(gate, require=True), (True, _OK))
    gate = Gate([gate_run(1, ok_job), gate_run(2, job(30, "in_progress", None))])
    check("await: an unfinished run beside a finished one keeps the head pending",
          wait(gate, require=False), (True, Dismissal("in_progress", "")))
    gate = Gate([gate_run(1, skip_job), gate_run(2)])
    check("await: a skipped run beside an unregistered one is still pending",
          wait(gate, require=False), (True, Dismissal("queued", "")))
    gate = Gate([gate_run(1, job(10, "completed", "failure")), gate_run(2, job(11, "completed", "success"))])
    check("await: the newest job wins", wait(gate, require=True), (True, _OK))
    gate = Gate([gate_run(1, job(10, "completed", "success")), gate_run(2, job(11, "completed", "failure"))])
    check("await: a newer failed rerun wins over an older success",
          wait(gate, require=True), (True, Dismissal("completed", "failure")))
    gate = Gate([gate_run(1, job(10, "in_progress", None))])
    check("await: a run still pending at the deadline is returned pending",
          wait(gate, require=True), (True, Dismissal("in_progress", "")))
    gate = Gate()
    check("await: an unreadable API is reported unreadable, not absent",
          wait(gate, require=True), (False, None))
    gate = Gate()
    check("await: unreadable stays unreadable for a non-synchronize too",
          wait(gate, require=False), (False, None))

    # -- run_admit end to end over scripted gh/git, outputs written --------
    def pull_json(**over: object) -> str:
        pr = _pull(**over)
        return json.dumps({"state": pr.state, "head": {"sha": pr.head_sha},
                           "base": {"sha": pr.base_sha, "ref": pr.base_ref},
                           "labels": [{"name": n} for n in sorted(pr.labels)]})

    def admit(event: Event, *, pull: str | None, gate: Gate | None = None,
              freshness_rc: int = 0, fetch_rc: int = 0) -> Admission:
        gate = gate if gate is not None else Gate()
        gate.pull = pull

        def run_git(*args: str) -> tuple[int, str, str]:
            if args[0] == "fetch":
                return fetch_rc, "", ""
            if args[0] == "cat-file":
                return 0, "", ""
            return freshness_rc, "", ""
        return run_admit(event, "o/r", cli=gate, run_git=run_git, wait_seconds=12,
                         poll_seconds=1, sleep=nap, clock=fake_clock)

    def done() -> Gate:
        return Gate([gate_run(1, ok_job)], pull=None)

    check("admit: synchronize kept and current",
          admit(_event("pull_request", "synchronize"), pull=pull_json(), gate=done()).eligible, True)
    check("admit: ... but behind",
          admit(_event("pull_request", "synchronize"), pull=pull_json(), gate=done(),
                freshness_rc=1).code, "behind-base")
    check("admit: ... but the base fetch failed",
          admit(_event("pull_request", "synchronize"), pull=pull_json(), gate=done(),
                fetch_rc=1).code, "freshness-unverified")
    check("admit: ... but git errored",
          admit(_event("pull_request", "synchronize"), pull=pull_json(), gate=done(),
                freshness_rc=128).code, "freshness-unverified")
    check("admit: the dismissal strip (green) with the label gone",
          admit(_event("pull_request", "synchronize"), pull=pull_json(labels=frozenset()),
                gate=done()).code, "not-approved")
    check("admit: unreadable pull request",
          admit(_event("pull_request", "synchronize"), pull=None, gate=done()).code,
          "pr-identity-unresolved")
    check("admit: checks API down on a synchronize",
          admit(_event("pull_request", "synchronize"), pull=pull_json()).code,
          "dismissal-unreadable")
    approve = _event("pull_request", "labeled", label=APPROVAL_LABEL)
    check("admit: initial approval beside skipped dismissal jobs",
          admit(approve, pull=pull_json(),
                gate=Gate([gate_run(1, skip_job), gate_run(2, job(21, "completed", "skipped"))])).eligible,
          True)
    check("admit: approval after a successful synchronize despite a newer skipped job",
          admit(approve, pull=pull_json(),
                gate=Gate([gate_run(1, ok_job), gate_run(2, job(30, "completed", "skipped"))])).eligible,
          True)
    check("admit: dispatch recovery sees through a skipped job",
          admit(_event("workflow_dispatch", ""), pull=pull_json(),
                gate=Gate([gate_run(1, ok_job), gate_run(2, job(30, "completed", "skipped"))])).eligible,
          True)
    for label, event in (("labeled", approve), ("dispatch", _event("workflow_dispatch", ""))):
        check(f"admit: a {label} head whose Actions API is unreadable is not admitted",
              admit(event, pull=pull_json()).code, "dismissal-unreadable")
        # The round 2 race: the first read finds nothing registered, a
        # synchronize dismissal then appears and finishes. Admission must
        # wait for it, and read the label only afterwards.
        gate = Gate([], [gate_run(1)], [gate_run(1, job(10, "in_progress", None))],
                    [gate_run(1, ok_job)])
        result = admit(event, pull=pull_json(), gate=gate)
        check(f"admit: {label} waits out a delayed synchronize dismissal", result.eligible, True)
        check(f"admit: {label} polled more than once", gate.polls >= 3, True)
        gate = Gate([], [gate_run(1)], [gate_run(1, job(10, "in_progress", None))],
                    [gate_run(1, ok_job)])
        check(f"admit: {label}, the delayed dismissal stripped the label",
              admit(event, pull=pull_json(labels=frozenset()), gate=gate).code, "not-approved")
        check(f"admit: {label}, a dismissal that never finishes is not admitted",
              admit(event, pull=pull_json(),
                    gate=Gate([], [gate_run(1)], [gate_run(1, job(10, "in_progress", None))])).code,
              "dismissal-pending")
        check(f"admit: {label}, a head review-gate never registered is not admitted",
              admit(event, pull=pull_json(), gate=Gate([])).code, "dismissal-pending")
        check(f"admit: {label}, a failed synchronize dismissal is not admitted",
              admit(event, pull=pull_json(),
                    gate=Gate([gate_run(1, job(10, "completed", "failure"))])).code,
              "dismissal-failed")
    check("admit: master push needs no evidence",
          admit(_event("push", ""), pull=None).eligible, True)
    check("admit: unrelated label event reads nothing",
          admit(_event("pull_request", "labeled", label="tooling"), pull=None).code,
          "unrelated-event")
    check("admit: dispatch of the PR head",
          admit(_event("workflow_dispatch", ""), pull=pull_json(), gate=done()).eligible, True)
    check("admit: dispatch of another ref",
          admit(_event("workflow_dispatch", ""), pull=pull_json(head_sha=OTHER),
                gate=done()).code, "dispatch-ref-not-pr-head")
    gate = done()
    admit(_event("pull_request", "synchronize"), pull=pull_json(), gate=gate)
    check("admit: the dismissal is read before the live pull request",
          gate.order[:1] + gate.order[-1:], ["dismissal", "pull"])
    with tempfile.TemporaryDirectory(prefix="ci-admission-") as tmp:
        sink = Path(tmp) / "out"
        _emit({"eligible": "false", "reason": "[x] one\ntwo"}, str(sink))
        check("emit: a reason is one output line", sink.read_text(), "eligible=false\nreason=[x] one two\n")

    # -- base_freshness against real commit graphs --------------------------
    def git_in(repo: Path, *args: str) -> str:
        done_ = subprocess.run(
            ["git", "-c", "user.name=t", "-c", "user.email=t@t", "-c", "commit.gpgsign=false",
             "-C", str(repo), *args], capture_output=True, text=True, check=True)
        return done_.stdout.strip()

    with tempfile.TemporaryDirectory(prefix="ci-freshness-") as tmp:
        origin, clone = Path(tmp) / "origin", Path(tmp) / "clone"
        origin.mkdir()
        git_in(origin, "init", "-q", "-b", "master")
        (origin / "f").write_text("0\n")
        git_in(origin, "add", "f")
        git_in(origin, "commit", "-q", "-m", "root")
        subprocess.run(["git", "clone", "-q", str(origin), str(clone)], check=True)
        git_in(clone, "checkout", "-q", "-b", "topic")
        (clone / "g").write_text("1\n")
        git_in(clone, "add", "g")
        git_in(clone, "commit", "-q", "-m", "topic work")
        head = git_in(clone, "rev-parse", "HEAD")
        (origin / "h").write_text("2\n")
        git_in(origin, "add", "h")
        git_in(origin, "commit", "-q", "-m", "base moves on")

        def in_clone(*args: str) -> tuple[int, str, str]:
            done_ = subprocess.run(["git", "-C", str(clone), *args],
                                   capture_output=True, text=True, check=False)
            return done_.returncode, done_.stdout, done_.stderr
        check("freshness: a head behind the moved base", base_freshness(in_clone, head, "master"), BEHIND)
        # The pull_request checkout is a merge of head and base: HEAD of it
        # contains the base, which is exactly why HEAD must not be asked.
        git_in(clone, "checkout", "-q", "--detach", head)
        git_in(clone, "merge", "-q", "--no-edit", "origin/master")
        synthetic = git_in(clone, "rev-parse", "HEAD")
        check("freshness: the synthetic merge would have said current",
              base_freshness(in_clone, synthetic, "master"), CURRENT)
        check("freshness: ... but the exact head still says behind",
              base_freshness(in_clone, head, "master"), BEHIND)
        check("freshness: the branch updated to contain the base recovers",
              base_freshness(in_clone, synthetic, "master"), CURRENT)
        check("freshness: an unknown commit is unverified",
              base_freshness(in_clone, "f" * 40, "master"), UNVERIFIED)
        check("freshness: an unknown base ref is unverified",
              base_freshness(in_clone, head, "no-such-branch"), UNVERIFIED)
        check("freshness: a malformed base ref is unverified",
              base_freshness(in_clone, head, "--upload-pack=x"), UNVERIFIED)

    # -- the workflow wiring and the required checks' real verdicts --------
    try:
        document = _ci_document(workflow)
    except ImportError:
        failures.append("PyYAML is needed for the workflow checks "
                        "(python3 -m pip install -r tools/requirements-assets.txt)")
        document = None
    if document is not None:
        _check_workflow(document, decided, check)

    if failures:
        for failure in failures:
            print(f"  FAIL {failure}")
        print(f"\n{len(failures)} ci_pr_admission self-test check(s) failed")
        return 1
    print("ci_pr_admission self-test: all checks pass")
    return 0


def _check_workflow(document: dict, decided: dict, check: Callable[[str, object, object], None]) -> None:
    triggers = document.get("on", document.get(True)) or {}
    jobs = document.get("jobs") or {}
    squash = lambda text: " ".join(str(text).split())  # noqa: E731
    wrapped = "${{ " + ADMITTING_EXPRESSION + " }}"

    check("ci.yml: pull_request types",
          (triggers.get("pull_request") or {}).get("types"), ["synchronize", "labeled"])
    check("ci.yml: push stays master only", triggers.get("push"), {"branches": ["master"]})
    dispatch = ((triggers.get("workflow_dispatch") or {}).get("inputs") or {}).get("pr_number") or {}
    check("ci.yml: dispatch takes a required pr_number", dispatch.get("required"), True)

    admission = jobs.get("admission") or {}
    check("ci.yml: admission's condition is the admitting predicate",
          squash(admission.get("if", "")), squash(wrapped))
    for key in ("eligible", "mode", "reason", "base_sha", "head_sha"):
        check(f"ci.yml: admission exports {key}", key in (admission.get("outputs") or {}), True)
    check("ci.yml: admission runs the helper",
          any("tools/ci_pr_admission.py admit" in str(step.get("run", ""))
              for step in admission.get("steps") or []), True)
    check("ci.yml: admission checks out the exact PR head",
          any("github.event.pull_request.head.sha" in str((step.get("with") or {}).get("ref", ""))
              for step in admission.get("steps") or []), True)

    image = jobs.get("resolve-image") or {}
    check("ci.yml: resolve-image waits for admission", image.get("needs"), "admission")
    check("ci.yml: resolve-image runs only when admitted",
          squash(image.get("if", "")), "needs.admission.outputs.eligible == 'true'")

    def needs(job: dict) -> set:
        raw = job.get("needs")
        return {raw} if isinstance(raw, str) else set(raw or [])

    for worker in ("test-and-audits", "headless-lanes", "static-audits", "behavior-probe-worker"):
        job = jobs.get(worker) or {}
        check(f"ci.yml: {worker} needs admission and the image", needs(job) >= {"admission", "resolve-image"}, True)
        check(f"ci.yml: {worker} has no status function that could outrun admission",
              any(fn in str(job.get("if", "")) for fn in ("always()", "!cancelled()", "failure()")), False)
    check("ci.yml: the probe worker is PR-mode only",
          squash((jobs.get("behavior-probe-worker") or {}).get("if", "")),
          "needs.admission.outputs.mode == 'pr'")

    for required in ("build-test", "behavior-probes"):
        job = jobs.get(required) or {}
        check(f"ci.yml: {required} always runs on admitting events",
              squash(job.get("if", "")), squash("${{ always() && (" + ADMITTING_EXPRESSION + ") }}"))
        check(f"ci.yml: {required} needs admission", "admission" in needs(job), True)
        name = job.get("name", "")
        for event_label, context in (
                ("master push", {"github.event_name": "push"}),
                ("synchronize", {"github.event_name": "pull_request",
                                 "github.event.action": "synchronize"}),
                ("approve label", {"github.event_name": "pull_request",
                                   "github.event.action": "labeled",
                                   "github.event.label.name": APPROVAL_LABEL}),
                ("dispatch", {"github.event_name": "workflow_dispatch"})):
            check(f"ci.yml: {required} keeps its required name on a {event_label}",
                  _translate(name, context), required)
        stray = {"github.event_name": "pull_request", "github.event.action": "labeled",
                 "github.event.label.name": "tooling"}
        check(f"ci.yml: {required} never carries its required name on an unrelated label",
              _translate(name, stray) != required, True)
        check(f"ci.yml: {required} does not run on an unrelated label",
              bool(_translate(str(job.get("if", "")), stray)), False)

    # -- event routing through the concurrency block ------------------------
    concurrency = document.get("concurrency") or {}

    def route(**context: str) -> tuple[object, object]:
        base = {"github.event_name": "pull_request", "github.run_id": "900",
                "github.ref": "refs/pull/7/merge", "github.sha": "m" * 40}
        base.update(context)
        return (_translate(str(concurrency.get("group")), base),
                _translate(str(concurrency.get("cancel-in-progress")), base))

    sync = route(**{"github.event.action": "synchronize"})
    approve = route(**{"github.event.action": "labeled", "github.event.label.name": APPROVAL_LABEL})
    stray = route(**{"github.event.action": "labeled", "github.event.label.name": "tooling"})
    other_head = route(**{"github.event.action": "synchronize", "github.run_id": "901"})
    dispatch_route = route(**{"github.event_name": "workflow_dispatch", "github.ref": "refs/heads/topic",
                              "inputs.pr_number": "7", "github.event.action": ""})
    push_a = route(**{"github.event_name": "push", "github.sha": "1" * 40})
    push_b = route(**{"github.event_name": "push", "github.sha": "2" * 40})
    check("routing: approval and synchronize of one PR share a group", sync[0], approve[0])
    check("routing: an admitting event cancels what it supersedes", (sync[1], approve[1]), (True, True))
    check("routing: a newer head supersedes the older one", other_head[0], sync[0])
    check("routing: an unrelated label gets a group of its own", stray[0] not in (sync[0], approve[0]), True)
    check("routing: ... and never cancels", stray[1], False)
    check("routing: a dispatch shares the PR's group", dispatch_route[0], sync[0])
    check("routing: master pushes never share a group", push_a[0] != push_b[0], True)
    check("routing: master pushes never cancel", (push_a[1], push_b[1]), (False, False))

    # -- the required checks' real verdicts, by running their scripts -------
    build = jobs.get("build-test") or {}
    probes = jobs.get("behavior-probes") or {}
    try:
        build_script, build_env = _script(build, "Require parallel CI workers")
        probe_script, probe_env = _script(probes, "Require the behavior-probe worker")
    except KeyError as missing:
        check(f"ci.yml: verdict step {missing}", "missing", "present")
        return

    def build_env_for(admission: Admission | None, *, results: dict | None = None,
                      eligible: str | None = None, mode: str | None = None) -> dict:
        values = {"ADMITTED": eligible if eligible is not None else ("true" if admission and admission.eligible else "false"),
                  "ADMISSION_MODE": mode if mode is not None else (admission.mode if admission else ""),
                  "ADMISSION_REASON": admission.reason if admission else "",
                  "EVENT_NAME": "pull_request",
                  "TESTS_RESULT": "skipped", "AUDITS_RESULT": "skipped",
                  "LANES_RESULT": "skipped", "PROBES_RESULT": "skipped",
                  "WORKER_RESULT": "skipped"}
        values.update(results or {})
        return values

    def passes(script: str, env: dict) -> bool:
        return _verdict(script, env) == 0

    check("verdict: both scripts read only variables their env supplies",
          {"ADMITTED", "ADMISSION_MODE", "ADMISSION_REASON"} <= set(build_env) | set(probe_env), True)
    all_ok = {"TESTS_RESULT": "success", "AUDITS_RESULT": "success", "LANES_RESULT": "success"}
    for label, admission in decided.items():
        if admission.eligible:
            continue
        # Gated out: every worker was skipped. The required checks must FAIL.
        env = build_env_for(admission)
        check(f"verdict/{label}: build-test fails", passes(build_script, env), False)
        check(f"verdict/{label}: behavior-probes fails", passes(probe_script, env), False)
        # ... and no worker result can talk them into success.
        env = build_env_for(admission, results={**all_ok, "PROBES_RESULT": "success",
                                                "WORKER_RESULT": "success"})
        check(f"verdict/{label}: build-test fails even if workers claim success",
              passes(build_script, env), False)
        check(f"verdict/{label}: behavior-probes fails even if the worker claims success",
              passes(probe_script, env), False)
    check("verdict: a crashed admission fails build-test",
          passes(build_script, build_env_for(None, eligible="", results=all_ok)), False)
    check("verdict: a crashed admission fails behavior-probes",
          passes(probe_script, build_env_for(None, eligible="", results={"WORKER_RESULT": "success"})), False)
    pr_ok = decided["approved head behind its base"]._replace(eligible=True, mode=MODE_PR)
    check("verdict: an admitted PR head with every worker green passes build-test",
          passes(build_script, build_env_for(pr_ok, results={**all_ok, "PROBES_RESULT": "success"})), True)
    check("verdict: an admitted PR head with the probe worker green passes behavior-probes",
          passes(probe_script, build_env_for(pr_ok, results={"WORKER_RESULT": "success"})), True)
    check("verdict: an admitted PR head with a skipped probe worker fails build-test",
          passes(build_script, build_env_for(pr_ok, results={**all_ok, "PROBES_RESULT": "skipped"})), False)
    check("verdict: an admitted PR head with a skipped probe worker fails behavior-probes",
          passes(probe_script, build_env_for(pr_ok, results={"WORKER_RESULT": "skipped"})), False)
    for lane in ("TESTS_RESULT", "AUDITS_RESULT", "LANES_RESULT"):
        for bad in ("failure", "cancelled", "skipped"):
            check(f"verdict: {lane} {bad} fails build-test",
                  passes(build_script, build_env_for(
                      pr_ok, results={**all_ok, "PROBES_RESULT": "success", lane: bad})), False)
    push = decided["master push"]
    check("verdict: a master push with the probe worker skipped passes build-test",
          passes(build_script, build_env_for(push, results=all_ok)), True)
    check("verdict: a master push with the probe worker skipped passes behavior-probes",
          passes(probe_script, build_env_for(push)), True)
    check("verdict: a master push whose probe worker ran fails build-test",
          passes(build_script, build_env_for(push, results={**all_ok, "PROBES_RESULT": "success"})), False)


def main(argv: Sequence[str] | None = None) -> int:
    parser = argparse.ArgumentParser(description=__doc__,
                                     formatter_class=argparse.RawDescriptionHelpFormatter)
    parser.add_argument("--self-test", action="store_true")
    parser.add_argument("--workflow", default=str(CI_WORKFLOW),
                        help="the ci.yml the self-test pins (default: this checkout's)")
    sub = parser.add_subparsers(dest="command")
    admit_parser = sub.add_parser("admit", help="decide a run's admission")
    for flag in ("repo", "event-name", "action", "label", "run-sha", "pr-number",
                 "pr-head-sha", "pr-base-sha", "pr-base-ref"):
        admit_parser.add_argument(f"--{flag}", default="")
    admit_parser.add_argument("--github-output", default="")
    admit_parser.add_argument("--github-summary", default="")
    admit_parser.add_argument("--wait-seconds", type=float, default=900)
    admit_parser.add_argument("--poll-seconds", type=float, default=15)
    fresh = sub.add_parser("freshness", help="does a head contain its base tip?")
    fresh.add_argument("--head-sha", required=True)
    fresh.add_argument("--base-ref", required=True)
    fresh.add_argument("--github-output", default="")
    args = parser.parse_args(argv)
    if args.self_test:
        return _self_test(Path(args.workflow))
    if args.command == "admit":
        return _admit_main(args)
    if args.command == "freshness":
        return _freshness_main(args)
    parser.error("one of admit, freshness or --self-test is required")
    return 2


if __name__ == "__main__":
    sys.exit(main())
