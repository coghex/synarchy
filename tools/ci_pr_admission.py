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
#: A run that admitted nothing because an earlier run already ran CI for this
#: very head: it does no expensive work and its required checks MIRROR that
#: run's verdicts (#2927).
MODE_DUP = "dup"

#: ci.yml's `run-name` for a workflow_dispatch run (its API record carries no
#: pull-request association, so the title is how a superseded dispatch run is
#: attributed to its PR).
DISPATCH_TITLE = "CI dispatch PR {0}"

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
    head_ref: str = ""


class Dismissal(NamedTuple):
    status: str
    conclusion: str


def identity_for(pr_number: str, base_ref: str, base_sha: str, head_sha: str) -> str:
    """What a run validated: this PR, on this base, at this head.

    ci.yml publishes it as the NAME of its `ci-identity` job. A run record
    names no pull request for a dispatched run, and no base for any, so the
    job name is the one place a finished run's validated identity can be read
    back, for every kind of run alike.
    """
    return f"ci-identity PR {pr_number} base {base_ref}@{base_sha} head {head_sha}"


class Admission(NamedTuple):
    eligible: bool
    mode: str
    code: str
    detail: str
    head_sha: str = ""
    base_sha: str = ""
    mirror_build_test: str = ""
    mirror_probes: str = ""
    identity: str = ""

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
            labels=frozenset(str(item["name"]) for item in doc["labels"]),
            head_ref=str(doc["head"].get("ref", "")))
    except (ValueError, KeyError, TypeError):
        return None


def _paged(cli: Runner, endpoint: str, key: str | None,
           **filters: str) -> list[dict] | None:
    """Every item of a paginated list endpoint; None when unreadable.

    `--paginate --slurp` returns EVERY page, so an answer is the complete
    history, never the first 100 results presented as if it were all of it
    (100 skipped label runs on page one must not hide a failed dismissal on
    page two). `key` names the page's item list; None means the page itself
    is the list (the issue timeline).
    """
    flags: list[str] = []
    for name, value in filters.items():
        flags += ["-f", f"{name}={value}"]
    rc, out, _ = cli("api", "-X", "GET", endpoint, *flags, "-f", "per_page=100",
                     "--paginate", "--slurp")
    if rc != 0:
        return None
    try:
        pages = json.loads(out)
        if not isinstance(pages, list):
            return None
        items: list[dict] = []
        for page in pages:
            items.extend(page if key is None else page[key])
        return items
    except (ValueError, KeyError, TypeError):
        return None


def _runs(cli: Runner, repo: str, workflow: str, **filters: str
          ) -> list[dict] | None:
    """A workflow's runs for the given filters; None when unreadable."""
    items = _paged(cli, f"repos/{repo}/actions/workflows/{workflow}/runs",
                   "workflow_runs", **filters)
    if items is None:
        return None
    try:
        return [dict(item, id=int(item["id"])) for item in items]
    except (KeyError, TypeError, ValueError):
        return None


def _jobs(cli: Runner, repo: str, run_id: int) -> list[dict] | None:
    return _paged(cli, f"repos/{repo}/actions/runs/{run_id}/jobs", "jobs")


def _timestamp(text: object) -> float:
    """Epoch seconds of an ISO-8601 `...Z` stamp; -1 when absent or malformed."""
    from datetime import datetime
    try:
        return datetime.fromisoformat(str(text).replace("Z", "+00:00")).timestamp()
    except ValueError:
        return -1.0


def force_push_epoch(cli: Runner, repo: str, pr_number: str) -> tuple[bool, float]:
    """When this pull request's head was last FORCE-PUSHED (0.0: never).

    Answers (readable, epoch seconds). Any head that predates a force push can
    only be the current head again through a push made after it: the force
    push left some commit as the head, and getting back to the one in question
    (by a second force push, or by an ordinary fast-forward after rewinding to
    an ancestor) is a later transition. So a head's evidence counts only from
    the latest force push of the PR onward, whichever commit that push named.
    The `head_ref_force_pushed` timeline event records the push itself rather
    than inferring it from whichever review-gate runs are listed yet.
    """
    events = _paged(cli, f"repos/{repo}/issues/{pr_number}/timeline", None)
    if events is None:
        return False, 0.0
    stamps = [_timestamp(event.get("created_at")) for event in events
              if isinstance(event, dict)
              and event.get("event") == "head_ref_force_pushed"]
    return True, max(stamps, default=0.0)


def _run_prs(run: dict) -> set[str]:
    return {str(item.get("number")) for item in run.get("pull_requests") or []
            if isinstance(item, dict)}


def fetch_dismissal(cli: Runner, repo: str, pr_number: str, head_ref: str,
                    sha: str) -> tuple[bool, Dismissal | None]:
    """The stale-approval outcome for pull request `pr_number` at `sha`.

    Answers (readable, outcome): a completed Dismissal; a not-completed one
    while the head's evidence has not SETTLED; or None, which means the head
    is POSITIVELY the pull request's initial head and nothing ran a
    dismissal. An unreadable answer is never "none".

    Evidence comes from review-gate.yml's own workflow RUNS and their jobs,
    scoped to this pull request (its head branch, and the run's own pull
    request list), not from check runs and not repository-wide by SHA:

    * a run exists the moment its event fires while its checks register a
      little later, so an empty check-runs answer cannot tell "never
      synchronized" from "not registered yet";
    * two pull requests can share a commit, and one PR's failed dismissal
      must neither be hidden by, nor stand in for, the other's.

    The dismissal job runs only on `synchronize`; on `opened`/`labeled` GitHub
    records it as a SKIPPED job, a settled absence and never an outcome.

    ABSENCE is accepted only for the initial head, and that is proved
    positively: every review-gate run of this pull request is for `sha`.
    A head that was visited before and pushed back to is judged only on runs
    newer than the latest run for another head AND newer than the PR's latest
    recorded force push (the PR timeline, any commit: a rewind to an ancestor
    and a fast-forward back leaves a force push of the ANCESTOR), so an old
    success for it is never this transition's evidence even while the
    intervening runs are unregistered.
    A synchronize needs a different head to move to, so any head that was
    ever pushed over another leaves a run for the earlier head behind. A head
    with other heads in the PR's history and no dismissal outcome yet is
    PENDING (its synchronize run has not registered, or its jobs are not
    listed), however many skipped labeled/opened runs sit beside it.
    """
    runs = _runs(cli, repo, "review-gate.yml", branch=head_ref, event="pull_request")
    if runs is None:
        return False, None
    # Positive association only: a run that does not NAME this pull request
    # is not its evidence, whatever branch it ran on (forks share branch
    # names), and an unattributable run leaves the head unverified.
    scoped = [run for run in runs if pr_number in _run_prs(run)]
    readable, epoch = force_push_epoch(cli, repo, pr_number)
    if not readable:
        return False, None
    # A head can be revisited (H, then X, then a force push back to H). Only
    # runs NEWER than the latest run for any other head belong to the current
    # visit; an older success for H says nothing about this transition.
    latest_other = max((run["id"] for run in scoped if run.get("head_sha") != sha),
                       default=0)
    # ... and, when the timeline records any force push, only runs created at
    # or after the latest one: it is the transition being admitted, and
    # it is visible even while the runs it triggered are not yet listed.
    mine = [run for run in scoped
            if run.get("head_sha") == sha and run["id"] > latest_other
            and (not epoch or _timestamp(run.get("created_at")) >= epoch)]
    unsettled = Dismissal("queued", "")
    if not mine:
        return True, unsettled
    outcomes: list[tuple[int, Dismissal]] = []
    for run in mine:
        jobs = _jobs(cli, repo, run["id"])
        if jobs is None:
            return False, None
        jobs = [job for job in jobs if job.get("name") == DISMISSAL_CHECK]
        if not jobs:
            return True, unsettled
        for job in jobs:
            if job.get("status") != "completed":
                return True, Dismissal(str(job.get("status") or "queued"), "")
            if job.get("conclusion") == "skipped":
                continue
            # Timestamps have one-second precision, so a run created in the
            # same second as the force push cannot be ordered against it by
            # its own stamp. The OUTCOME is therefore taken only from a job
            # that started STRICTLY after the push (a job starts after its
            # run is created and a runner is allocated, so the push's own
            # dismissal does), and a job with no usable start time is no
            # evidence: ordering that cannot be proven is pending, not good.
            if epoch and _timestamp(job.get("started_at")) <= epoch:
                continue
            outcomes.append((int(job.get("id", 0)),
                             Dismissal("completed", str(job.get("conclusion") or ""))))
    if outcomes:
        return True, max(outcomes, key=lambda pair: pair[0])[1]
    if latest_other or epoch:
        return True, unsettled
    return True, None


def await_dismissal(cli: Runner, repo: str, pr_number: str, head_ref: str, sha: str,
                    *, require: bool,
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
        readable, run = fetch_dismissal(cli, repo, pr_number, head_ref, sha)
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


class Prior(NamedTuple):
    run_id: int
    build_test: str
    probes: str


def find_prior_run(cli: Runner, repo: str, sha: str, my_run_id: str,
                   identity: str) -> tuple[str, Prior | None]:
    """An earlier ci.yml run that already RAN CI for this exact identity.

    Answers ("ok" | "unreadable", prior). A run counts only when its
    `ci-identity` job is named exactly `identity` (this PR, this base ref and
    sha, this head): the same commit can head two pull requests, probe
    selection, the expensive gates and the save-schema comparison depend on
    the base, and a dispatched run's record names neither. Automatic and
    dispatched runs are judged alike, and the one that most recently FINISHED
    executing wins (a rerun counts from its rerun), so a manual recovery run
    supersedes the automatic failure it recovered. A run
    that is not finished is ignored: same-head admitting runs are serialized
    by the workflow's concurrency group, so one still running is an
    unrelated-label run in its own group, which skips every job.

    A run RAN CI when its `static-audits` job concluded success or failure;
    an un-admitted run, a mirror, or a cancelled one did not.
    """
    runs = _runs(cli, repo, "ci.yml", head_sha=sha)
    if runs is None:
        return "unreadable", None
    # Newest LATEST EXECUTION first: a rerun keeps its run id, so an older run
    # that was re-run later is the newest result, and the run id cannot order
    # results. `updated_at` is when the run last finished executing.
    def latest(item: dict) -> tuple[float, int]:
        return (max(_timestamp(item.get("updated_at")), _timestamp(item.get("created_at"))),
                item["id"])

    for run in sorted(runs, key=latest, reverse=True):
        if str(run["id"]) == str(my_run_id) or run.get("status") != "completed" \
                or run.get("event") not in ("pull_request", "workflow_dispatch"):
            continue
        jobs = _jobs(cli, repo, run["id"])
        if jobs is None:
            return "unreadable", None
        by_name = {job.get("name"): job for job in jobs}
        if identity not in by_name:
            continue
        if (by_name.get("static-audits") or {}).get("conclusion") not in ("success", "failure"):
            continue
        return "ok", Prior(run["id"],
                           str((by_name.get("build-test") or {}).get("conclusion") or ""),
                           str((by_name.get("behavior-probes") or {}).get("conclusion") or ""))
    return "ok", None


def cancel_superseded(cli: Runner, repo: str, pr_number: str, head_ref: str,
                      head_sha: str, my_run_id: str) -> list[int]:
    """Cancel this pull request's live CI runs for any OTHER head.

    The workflow's concurrency group is per head (so same-head events never
    cancel each other's expensive work), which makes superseding a head an
    explicit act: a newer head makes every older head's run obsolete. That
    covers pull_request runs (attributed by their own PR list) and dispatched
    runs (whose record names no pull request, so ci.yml's `run-name` carries
    it). Only runs OLDER than this one (smaller run id) are cancelled: a run
    for another head that started after this one belongs to a newer head, and
    cancelling it would leave the current head without its pipeline. Best
    effort, and never decisive for this run's own admission.
    """
    if not str(my_run_id).isdigit():
        return []
    cancelled: list[int] = []
    for event in ("pull_request", "workflow_dispatch"):
        for status in ("in_progress", "queued", "pending", "waiting"):
            runs = _runs(cli, repo, "ci.yml", branch=head_ref, event=event,
                         status=status) or []
            for run in runs:
                if run.get("head_sha") == head_sha or run["id"] >= int(my_run_id):
                    continue
                if event == "pull_request":
                    ours = pr_number in _run_prs(run)
                else:
                    ours = run.get("display_title") == DISPATCH_TITLE.format(pr_number)
                if not ours:
                    continue
                rc, _, _ = cli("api", "-X", "POST",
                               f"repos/{repo}/actions/runs/{run['id']}/cancel")
                if rc == 0:
                    cancelled.append(run["id"])
    return cancelled


def run_admit(event: Event, repo: str, *, run_id: str = "", cli: Runner = gh,
              run_git: Runner = git, wait_seconds: float = 900,
              poll_seconds: float = 15,
              sleep: Callable[[float], None] = time.sleep,
              clock: Callable[[], float] = time.monotonic) -> Admission:
    """Gather the evidence in the right order, then decide.

    The order is the point: the pull request's identity first, then the
    stale-approval outcome, then the LIVE pull request (head and labels), so
    the label read cannot predate the strip it is meant to reflect.
    """
    if event.name not in ("pull_request", "workflow_dispatch") \
            or (event.name == "pull_request" and not is_admitting_event(event)):
        return decide(event, None, None, UNVERIFIED)
    run_head = event.pr_head_sha if event.name == "pull_request" else event.run_sha
    identity_ok = _SHA.fullmatch(run_head or "") and event.pr_number.isdigit()
    first = fetch_pull(cli, repo, event.pr_number) if identity_ok else None
    dismissal, readable = None, True
    if first is not None:
        if first.head_sha == run_head:
            cancel_superseded(cli, repo, event.pr_number, first.head_ref, run_head, run_id)
        readable, dismissal = await_dismissal(
            cli, repo, event.pr_number, first.head_ref, run_head,
            require=(event.name == "pull_request" and event.action == "synchronize"),
            wait_seconds=wait_seconds, poll_seconds=poll_seconds,
            sleep=sleep, clock=clock)
    pull = fetch_pull(cli, repo, event.pr_number) if first is not None else None
    base_ref = pull.base_ref if pull is not None else event.pr_base_ref
    freshness = base_freshness(run_git, run_head, base_ref)
    admitted = decide(event, pull, dismissal, freshness, readable)
    if not admitted.eligible:
        return admitted
    identity = identity_for(event.pr_number, pull.base_ref if pull else "",
                            admitted.base_sha, admitted.head_sha)
    admitted = admitted._replace(identity=identity)
    if event.name != "pull_request":
        # An explicit dispatch is a deliberate re-run and is never suppressed.
        return admitted
    state, prior = find_prior_run(cli, repo, run_head, run_id, identity)
    if state == "unreadable":
        return no(MODE_PR, "prior-run-unreadable",
                  "could not read the earlier CI runs for this head, so a duplicate "
                  "cannot be ruled out", admitted.head_sha, admitted.base_sha)
    if prior is None:
        return admitted
    return Admission(
        False, MODE_DUP, "duplicate-head",
        f"run {prior.run_id} already ran CI for this head; its verdicts "
        f"(build-test {prior.build_test or 'unknown'}, behavior-probes "
        f"{prior.probes or 'unknown'}) are mirrored and nothing runs again",
        admitted.head_sha, admitted.base_sha, prior.build_test, prior.probes,
        identity)


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
    result = run_admit(event, args.repo, run_id=args.run_id,
                       wait_seconds=args.wait_seconds, poll_seconds=args.poll_seconds)
    line = (f"CI admission: {'ADMITTED' if result.eligible else 'NOT admitted'} "
            f"(mode {result.mode}) {result.reason}")
    print(line)
    _emit({"eligible": "true" if result.eligible else "false",
           "mode": result.mode, "reason": result.reason,
           "base_sha": result.base_sha, "head_sha": result.head_sha,
           "identity": result.identity,
           "mirror_build_test": result.mirror_build_test,
           "mirror_probes": result.mirror_probes},
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

    # -- await_dismissal / fetch_dismissal over a scripted GitHub ----------
    OLD = "e" * 40

    def job(identifier: int, status: str, conclusion: str | None, name: str = DISMISSAL_CHECK) -> dict:
        return {"id": identifier, "name": name, "status": status, "conclusion": conclusion}

    def stamp(seconds: int) -> str:
        return f"2026-01-01T{seconds // 3600:02d}:{seconds // 60 % 60:02d}:{seconds % 60:02d}Z"

    def gate_run(run_id: int, *jobs: dict, head: str = HEAD, prs: tuple = ("7",)) -> dict:
        """A review-gate run of a pull request at `head`. No jobs: not listed yet.

        Created `run_id` seconds into the scripted day, so ids order in time.
        """
        # A job starts after its run is created and a runner is allocated.
        started = [{**j, "started_at": j["started_at"] if "started_at" in j else stamp(run_id + 1)}
                   for j in jobs]
        return {"id": run_id, "head_sha": head, "jobs": started, "created_at": stamp(run_id),
                "pull_requests": [{"number": int(n)} for n in prs]}

    def ci_run(run_id: int, *, status: str = "completed", event: str = "pull_request",
               head: str = HEAD, prs: tuple = ("7",), audits: str | None = "success",
               build: str | None = "success", probes: str | None = "success",
               base_ref: str = "master", base_sha: str = BASE, title: str = "",
               pr: str = "7", identity: bool = True, updated: int | None = None) -> dict:
        jobs = [job(run_id * 10 + 1, "completed", audits, "static-audits"),
                job(run_id * 10 + 2, "completed", build, "build-test"),
                job(run_id * 10 + 3, "completed", probes, "behavior-probes")]
        if identity:
            jobs.append(job(run_id * 10 + 4, "completed", "success",
                            identity_for(pr, base_ref, base_sha, head)))
        return {"id": run_id, "event": event, "status": status, "head_sha": head,
                "jobs": jobs, "display_title": title, "created_at": stamp(run_id),
                "updated_at": stamp(updated if updated is not None else run_id),
                "pull_requests": [{"number": int(n), "base": {"ref": base_ref, "sha": base_sha}}
                                  for n in prs]}

    class Gate:
        """A `gh` over scripted review-gate and ci.yml runs and a PR timeline.

        Review-gate answers advance one snapshot per poll (a snapshot is the
        list of that workflow's runs for the branch at that moment).
        `snapshots=None` makes the Actions API unreadable. `ci` is the static
        list of ci.yml runs; `pull` the live pull request; `force_pushes` the
        timeline's (seconds, commit) force-push events. `page_size` splits
        every list into pages, and a call WITHOUT `--paginate --slurp` sees
        only the first page, as GitHub would.
        """

        def __init__(self, *snapshots: list, pull: str | None = None,
                     ci: list | None = None, ci_unreadable: bool = False,
                     force_pushes: list | None = None, timeline_unreadable: bool = False,
                     page_size: int | None = None) -> None:
            self.snapshots = list(snapshots) if snapshots else None
            self.pull, self.ci, self.ci_unreadable = pull, ci or [], ci_unreadable
            self.force_pushes = force_pushes or []
            self.timeline_unreadable, self.page_size = timeline_unreadable, page_size
            self.polls = -1
            self.order: list[str] = []
            self.cancelled: list[int] = []

        def current(self) -> list:
            return self.snapshots[min(self.polls, len(self.snapshots) - 1)]

        def reply(self, args: tuple, items: list, key: str | None) -> tuple[int, str, str]:
            size = self.page_size or max(len(items), 1)
            chunks = [items[i:i + size] for i in range(0, len(items), size)] or [[]]
            pages = [chunk if key is None else {"total_count": len(items), key: chunk}
                     for chunk in chunks]
            slurp = "--slurp" in args and "--paginate" in args
            return 0, json.dumps(pages if slurp else pages[0]), ""

        def __call__(self, *args: str) -> tuple[int, str, str]:
            text = " ".join(args)
            if "workflows/review-gate.yml/runs" in text:
                self.order.append("dismissal")
                if self.snapshots is None:
                    return 1, "", "boom"
                self.polls += 1
                return self.reply(args, [{k: v for k, v in run.items() if k != "jobs"}
                                         for run in self.current()], "workflow_runs")
            if "workflows/ci.yml/runs" in text:
                if self.ci_unreadable:
                    return 1, "", "boom"
                runs = [{k: v for k, v in run.items() if k != "jobs"} for run in self.ci]
                if "head_sha=" in text:
                    runs = [r for r in runs if f"head_sha={r['head_sha']}" in text]
                wanted = re.search(r"status=(\w+)", text)
                if wanted:
                    runs = [r for r in runs if r["status"] == wanted.group(1)]
                wanted = re.search(r"event=(\w+)", text)
                if wanted:
                    runs = [r for r in runs if r["event"] == wanted.group(1)]
                return self.reply(args, runs, "workflow_runs")
            if "/timeline" in text:
                if self.timeline_unreadable:
                    return 1, "", "boom"
                events = [{"event": "labeled"}] * 3 + [
                    {"event": "head_ref_force_pushed", "commit_id": sha, "created_at": stamp(at)}
                    for at, sha in self.force_pushes]
                return self.reply(args, events, None)
            match = re.search(r"/actions/runs/(\d+)/jobs", text)
            if match:
                run_id = int(match.group(1))
                if self.snapshots is None and not any(r["id"] == run_id for r in self.ci):
                    return 1, "", "boom"
                pool = list(self.ci) + (self.current() if self.snapshots else [])
                return self.reply(args, next(r["jobs"] for r in pool if r["id"] == run_id), "jobs")
            cancel = re.search(r"/actions/runs/(\d+)/cancel", text)
            if cancel:
                self.cancelled.append(int(cancel.group(1)))
                return 0, "", ""
            self.order.append("pull")
            return (0, self.pull, "") if self.pull is not None else (1, "", "boom")

    ticks = iter(range(100_000))
    fake_clock = lambda: float(next(ticks))  # noqa: E731
    nap = lambda _seconds: None  # noqa: E731

    def wait(gate: Gate, *, require: bool, pr: str = "7", limit: float = 12):
        return await_dismissal(gate, "o/r", pr, "topic", HEAD, require=require,
                               wait_seconds=limit, poll_seconds=1, sleep=nap, clock=fake_clock)

    ok_job = job(10, "completed", "success")
    skip_job = job(20, "completed", "skipped")
    opened_old = gate_run(1, skip_job, head=OLD)  # the PR was opened at an earlier head
    gate = Gate([opened_old], [opened_old, gate_run(2)],
                [opened_old, gate_run(2, job(10, "in_progress", None))], [opened_old, gate_run(2, ok_job)])
    check("await: delayed dismissal is waited for", wait(gate, require=True), (True, _OK))
    check("await: it kept polling until completed", gate.polls + 1, 4)
    gate = Gate([])
    check("await: a head with no review-gate run yet is pending, never absent",
          wait(gate, require=True), (True, Dismissal("queued", "")))
    check("await: ... on a non-synchronize too",
          wait(Gate([]), require=False), (True, Dismissal("queued", "")))
    check("await: a run whose jobs are not registered yet is pending",
          wait(Gate([gate_run(1)]), require=False), (True, Dismissal("queued", "")))
    # Round 2/3 of #2928: a labeled/dispatch run reads while the synchronize's
    # run or dismissal has not registered, beside SKIPPED labeled/opened runs.
    labeled_skip = gate_run(3, skip_job)
    for require in (False, True):
        gate = Gate([opened_old, labeled_skip],
                    [opened_old, labeled_skip, gate_run(2)],
                    [opened_old, labeled_skip, gate_run(2, job(10, "in_progress", None))],
                    [opened_old, labeled_skip, gate_run(2, ok_job)])
        check(f"await: skipped-only evidence beside an earlier head is pending (require={require})",
              wait(gate, require=require), (True, _OK))
        check(f"await: ... and was read more than once (require={require})", gate.polls >= 3, True)
        gate = Gate([opened_old, labeled_skip])
        check(f"await: ... and never accepted as absence (require={require})",
              wait(gate, require=require), (True, Dismissal("queued", "")))
    gate = Gate([gate_run(1, skip_job)])
    check("await: the INITIAL head (every PR run is for it, all skipped) is accepted at once",
          wait(gate, require=False), (True, None))
    check("await: ... without polling again", gate.polls + 1, 1)
    gate = Gate([gate_run(1, skip_job), gate_run(2, job(21, "completed", "skipped"))])
    check("await: ... including its labeled runs", wait(gate, require=False), (True, None))
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
    # Two pull requests sharing a commit keep their evidence apart.
    shared = [gate_run(1, job(10, "completed", "failure"), prs=("7",)),
              gate_run(2, job(11, "completed", "success"), prs=("8",))]
    check("await: PR 7 sees its own failed dismissal, not PR 8's success",
          wait(Gate(shared), require=True, pr="7"), (True, Dismissal("completed", "failure")))
    check("await: PR 8 sees its own success", wait(Gate(shared), require=True, pr="8"), (True, _OK))
    check("await: a PR's own initial head is not hidden by another PR's runs",
          wait(Gate([gate_run(1, skip_job, prs=("7",)),
                     gate_run(2, job(11, "completed", "failure"), prs=("8",), head=OLD)]),
               require=False, pr="7"), (True, None))
    # Round 4 of #2928: a head visited before and pushed back to (H, X, H).
    visited = [gate_run(1, job(10, "completed", "success")),
               gate_run(2, job(11, "completed", "success"), head=OLD)]
    check("await: an older success for a revisited head is not this transition's evidence",
          wait(Gate(visited), require=True), (True, Dismissal("queued", "")))
    check("await: ... on a non-synchronize too",
          wait(Gate(visited), require=False), (True, Dismissal("queued", "")))
    back = gate_run(3, job(12, "in_progress", None))
    check("await: ... and the new transition's pending dismissal is waited for",
          wait(Gate(visited, visited + [back], visited + [gate_run(3, job(12, "completed", "success"))]),
               require=True), (True, _OK))
    check("await: ... a failed new dismissal beats the old success",
          wait(Gate(visited + [gate_run(3, job(12, "completed", "failure"))]), require=True),
          (True, Dismissal("completed", "failure")))
    # Missing PR association is not matching association.
    unattributed = [gate_run(1, job(10, "completed", "failure"), prs=("7",)),
                    gate_run(2, job(11, "completed", "success"), prs=())]
    check("await: an unattributed newer success does not stand in for PR 7's failure",
          wait(Gate(unattributed), require=True), (True, Dismissal("completed", "failure")))
    check("await: a run naming no pull request is no evidence at all",
          wait(Gate([gate_run(1, job(11, "completed", "success"), prs=())]), require=False),
          (True, Dismissal("queued", "")))
    check("await: an unreadable API is reported unreadable, not absent",
          wait(Gate(), require=True), (False, None))
    check("await: unreadable stays unreadable for a non-synchronize too",
          wait(Gate(), require=False), (False, None))

    # -- run_admit end to end over scripted gh/git, outputs written --------
    def pull_json(**over: object) -> str:
        pr = _pull(**over)
        return json.dumps({"state": pr.state, "head": {"sha": pr.head_sha, "ref": "topic"},
                           "base": {"sha": pr.base_sha, "ref": pr.base_ref},
                           "labels": [{"name": n} for n in sorted(pr.labels)]})

    def admit(event: Event, *, pull: str | None, gate: Gate | None = None,
              freshness_rc: int = 0, fetch_rc: int = 0, run_id: str = "900") -> Admission:
        gate = gate if gate is not None else Gate()
        gate.pull = pull

        def run_git(*args: str) -> tuple[int, str, str]:
            if args[0] == "fetch":
                return fetch_rc, "", ""
            if args[0] == "cat-file":
                return 0, "", ""
            return freshness_rc, "", ""
        return run_admit(event, "o/r", run_id=run_id, cli=gate, run_git=run_git,
                         wait_seconds=12, poll_seconds=1, sleep=nap, clock=fake_clock)

    def done(**kw: object) -> Gate:
        return Gate([gate_run(1, ok_job)], **kw)

    sync = _event("pull_request", "synchronize")
    check("admit: synchronize kept and current",
          admit(sync, pull=pull_json(), gate=done()).eligible, True)
    check("admit: ... but behind",
          admit(sync, pull=pull_json(), gate=done(), freshness_rc=1).code, "behind-base")
    check("admit: ... but the base fetch failed",
          admit(sync, pull=pull_json(), gate=done(), fetch_rc=1).code, "freshness-unverified")
    check("admit: ... but git errored",
          admit(sync, pull=pull_json(), gate=done(), freshness_rc=128).code, "freshness-unverified")
    check("admit: the dismissal strip (green) with the label gone",
          admit(sync, pull=pull_json(labels=frozenset()), gate=done()).code, "not-approved")
    check("admit: unreadable pull request",
          admit(sync, pull=None, gate=done()).code, "pr-identity-unresolved")
    check("admit: checks API down on a synchronize",
          admit(sync, pull=pull_json()).code, "dismissal-unreadable")
    approve = _event("pull_request", "labeled", label=APPROVAL_LABEL)
    check("admit: initial approval beside skipped dismissal jobs",
          admit(approve, pull=pull_json(),
                gate=Gate([gate_run(1, skip_job), gate_run(2, job(21, "completed", "skipped"))])).eligible,
          True)
    after_sync = [opened_old, gate_run(2, ok_job), gate_run(3, job(30, "completed", "skipped"))]
    check("admit: approval after a successful synchronize despite a newer skipped job",
          admit(approve, pull=pull_json(), gate=Gate(after_sync)).eligible, True)
    check("admit: dispatch recovery sees through a skipped job",
          admit(_event("workflow_dispatch", ""), pull=pull_json(), gate=Gate(after_sync)).eligible, True)
    for label, event in (("labeled", approve), ("dispatch", _event("workflow_dispatch", ""))):
        check(f"admit: a {label} head whose Actions API is unreadable is not admitted",
              admit(event, pull=pull_json()).code, "dismissal-unreadable")
        delayed = [[opened_old, labeled_skip], [opened_old, labeled_skip, gate_run(2)],
                   [opened_old, labeled_skip, gate_run(2, job(10, "in_progress", None))],
                   [opened_old, labeled_skip, gate_run(2, ok_job)]]
        gate = Gate(*delayed)
        check(f"admit: {label} waits out a delayed synchronize dismissal",
              admit(event, pull=pull_json(), gate=gate).eligible, True)
        check(f"admit: {label} polled more than once", gate.polls >= 3, True)
        check(f"admit: {label}, the delayed dismissal stripped the label",
              admit(event, pull=pull_json(labels=frozenset()), gate=Gate(*delayed)).code, "not-approved")
        check(f"admit: {label}, a dismissal that never finishes is not admitted",
              admit(event, pull=pull_json(),
                    gate=Gate([opened_old], [opened_old, gate_run(2, job(10, "in_progress", None))])).code,
              "dismissal-pending")
        check(f"admit: {label}, a synchronize that never registers is not admitted",
              admit(event, pull=pull_json(), gate=Gate([opened_old, labeled_skip])).code,
              "dismissal-pending")
        check(f"admit: {label}, a head review-gate never registered is not admitted",
              admit(event, pull=pull_json(), gate=Gate([])).code, "dismissal-pending")
        check(f"admit: {label}, a failed synchronize dismissal is not admitted",
              admit(event, pull=pull_json(), gate=Gate([gate_run(1, job(10, "completed", "failure"))])).code,
              "dismissal-failed")
        check(f"admit: {label}, another PR's success does not stand in for this PR's failure",
              admit(event, pull=pull_json(), gate=Gate(shared)).code, "dismissal-failed")

    for label, event in (("synchronize", sync), ("labeled", approve),
                         ("dispatch", _event("workflow_dispatch", ""))):
        stale = [gate_run(1, ok_job), gate_run(2, job(11, "completed", "success"), head=OLD)]
        check(f"admit: {label} of a revisited head waits for the new transition",
              admit(event, pull=pull_json(), gate=Gate(stale, stale + [gate_run(3, job(12, "in_progress", None))],
                                                       stale + [gate_run(3, job(12, "completed", "success"))])).eligible,
              True)
        check(f"admit: {label} of a revisited head is not admitted on the old success",
              admit(event, pull=pull_json(), gate=Gate(stale)).code, "dismissal-pending")
        check(f"admit: {label}, an unattributed success does not admit over PR 7's failure",
              admit(event, pull=pull_json(), gate=Gate(unattributed)).code, "dismissal-failed")

    # Round 5 of #2928: H -> X -> H by force push, with NEITHER intervening run
    # registered yet. Only the old success for H is listed; the timeline's
    # force-push event is what shows that it is not this transition's.
    old_h = [gate_run(1, ok_job)]
    forced = dict(force_pushes=[(100, HEAD)])
    check("await: a force-pushed head's old success is not evidence while the new runs are unregistered",
          wait(Gate(old_h, **forced), require=True), (True, Dismissal("queued", "")))
    check("await: ... for a non-synchronize too",
          wait(Gate(old_h, **forced), require=False), (True, Dismissal("queued", "")))
    check("await: ... and the new transition's run is then waited for",
          wait(Gate(old_h, old_h + [gate_run(101, job(12, "in_progress", None))],
                    old_h + [gate_run(101, job(12, "completed", "success"))], **forced),
               require=True), (True, _OK))
    check("await: a failed new dismissal beats the old success",
          wait(Gate(old_h + [gate_run(101, job(12, "completed", "failure"))], **forced),
               require=True), (True, Dismissal("completed", "failure")))
    check("await: after a force push, skipped-only runs newer than it are still not absence",
          wait(Gate([gate_run(101, skip_job)], **forced), require=False),
          (True, Dismissal("queued", "")))
    # Round 6: H -> ancestor X (force) -> H (fast-forward): the timeline holds
    # a force push of the ANCESTOR, and that is what dates the transition.
    check("await: a force push of ANOTHER commit (a rewind) still dates the transition",
          wait(Gate(old_h, force_pushes=[(100, OLD)]), require=True),
          (True, Dismissal("queued", "")))
    check("await: ... and the fast-forward back is then waited for",
          wait(Gate(old_h, old_h + [gate_run(101, job(12, "in_progress", None))],
                    old_h + [gate_run(101, job(12, "completed", "success"))],
                    force_pushes=[(100, OLD)]), require=True), (True, _OK))
    # Round 7: one-second timestamps prove order only strictly; no slack.
    old100 = [gate_run(100, ok_job)]
    check("await: an old run 2s before a rewind is not this transition's evidence",
          wait(Gate(old100, force_pushes=[(102, OLD)]), require=True), (True, Dismissal("queued", "")))
    check("await: ... nor one 1s before",
          wait(Gate([gate_run(101, ok_job)], force_pushes=[(102, OLD)]), require=True),
          (True, Dismissal("queued", "")))
    # Round 8: a same-second tie cannot be ordered by the run's own stamp, so
    # the OUTCOME must come from a job that started strictly after the push.
    for label, started in (("in the same second as the push", stamp(102)),
                           ("before the push", stamp(90)),
                           ("with no start time", None),
                           ("with an unusable start time", "garbage")):
        check(f"await: a same-second run whose job started {label} is not evidence",
              wait(Gate([gate_run(102, {**ok_job, "started_at": started})],
                        force_pushes=[(102, HEAD)]), require=True),
              (True, Dismissal("queued", "")))
        check(f"await: ... even with the commit the push left as the head ({label})",
              wait(Gate([gate_run(102, {**ok_job, "started_at": started})],
                        force_pushes=[(102, OLD)]), require=False),
              (True, Dismissal("queued", "")))
    check("await: a same-second run whose job started after the push is the push's own run",
          wait(Gate([gate_run(102, ok_job)], force_pushes=[(102, OLD)]), require=True), (True, _OK))
    check("await: the old H run and the push back in the same second, new run unregistered, stay pending",
          wait(Gate([gate_run(102, {**ok_job, "started_at": stamp(102)})],
                    force_pushes=[(102, HEAD)]), require=False), (True, Dismissal("queued", "")))
    check("await: a run after the push is evidence",
          wait(Gate(old100 + [gate_run(103, ok_job)], force_pushes=[(102, OLD)]), require=True),
          (True, _OK))
    check("await: a run with no usable timestamp is never evidence after a force push",
          wait(Gate([{**gate_run(150, ok_job), "created_at": "garbage"}],
                    force_pushes=[(102, OLD)]), require=True), (True, Dismissal("queued", "")))
    for label, event in (("synchronize", sync), ("labeled", approve),
                         ("dispatch", _event("workflow_dispatch", ""))):
        check(f"admit: {label} with an old success 2s before the rewind is not admitted",
              admit(event, pull=pull_json(), gate=Gate(old100, force_pushes=[(102, OLD)])).code,
              "dismissal-pending")
    for label, event in (("synchronize", sync), ("labeled", approve),
                         ("dispatch", _event("workflow_dispatch", ""))):
        tie = Gate([gate_run(102, {**ok_job, "started_at": stamp(102)})], force_pushes=[(102, HEAD)])
        check(f"admit: {label}, a same-second old run beside a push back to the head is not admitted",
              admit(event, pull=pull_json(), gate=tie).code, "dismissal-pending")
        genuine = Gate([gate_run(102, ok_job)], force_pushes=[(102, HEAD)])
        check(f"admit: {label}, the push's own same-second run (job started later) is admitted",
              admit(event, pull=pull_json(), gate=genuine).eligible, True)
    check("await: a force push older than the head's own runs does not matter",
          wait(Gate([gate_run(150, ok_job)], force_pushes=[(100, OLD)]), require=True), (True, _OK))
    check("await: an unreadable timeline is unverified, never 'no force push'",
          wait(Gate(old_h, timeline_unreadable=True), require=True), (False, None))
    check("await: the force push is found beyond the first timeline page",
          wait(Gate(old_h, page_size=2, **forced), require=True), (True, Dismissal("queued", "")))
    for label, event in (("synchronize", sync), ("labeled", approve),
                         ("dispatch", _event("workflow_dispatch", ""))):
        check(f"admit: {label} of a force-pushed head is not admitted on the old success",
              admit(event, pull=pull_json(), gate=Gate(old_h, **forced)).code, "dismissal-pending")
        check(f"admit: {label} of a force-pushed head waits for the new transition",
              admit(event, pull=pull_json(),
                    gate=Gate(old_h, old_h + [gate_run(101, job(12, "in_progress", None))],
                              old_h + [gate_run(101, job(12, "completed", "success"))], **forced)).eligible,
              True)

    for label, event in (("synchronize", sync), ("labeled", approve),
                         ("dispatch", _event("workflow_dispatch", ""))):
        check(f"admit: {label} after a rewind to an ancestor and a fast-forward back is not admitted on the old success",
              admit(event, pull=pull_json(), gate=Gate(old_h, force_pushes=[(100, OLD)])).code,
              "dismissal-pending")

    # Round 5 of #2928: history is read to the end, never first-page-only.
    labels = [gate_run(i, job(i * 10, "completed", "skipped")) for i in range(1, 6)]
    failed_late = labels + [gate_run(6, job(60, "completed", "failure"))]
    check("await: a failed dismissal beyond the first page is found",
          wait(Gate(failed_late, page_size=2), require=False),
          (True, Dismissal("completed", "failure")))
    check("admit: ... and it denies a labeled head",
          admit(approve, pull=pull_json(), gate=Gate(failed_late, page_size=2)).code, "dismissal-failed")
    many_jobs = {**gate_run(7), "jobs": [job(70 + i, "completed", "success", f"other-{i}") for i in range(3)]
                 + [job(80, "completed", "failure")]}
    check("await: a dismissal job beyond the first page of jobs is found",
          wait(Gate([many_jobs], page_size=2), require=True),
          (True, Dismissal("completed", "failure")))
    paged_ci = [ci_run(11, prs=("9",)), ci_run(12, prs=("9",)), ci_run(13, prs=("9",)), ci_run(14)]
    check("duplicate: a prior CI run beyond the first page is still mirrored",
          admit(approve, pull=pull_json(), gate=done(ci=paged_ci, page_size=2)).mode, MODE_DUP)

    # -- supersession and same-head duplicates (round 3 of #2928) ----------
    live = [ci_run(41, status="in_progress", head=OLD), ci_run(42, status="queued", head=OLD),
            ci_run(43, status="in_progress", head=OLD, prs=("9",)),
            ci_run(44, status="in_progress", head=HEAD),
            ci_run(45, status="in_progress", event="workflow_dispatch", head=OLD, prs=(),
                   title="CI dispatch PR 7"),
            ci_run(46, status="in_progress", event="workflow_dispatch", head=OLD, prs=(),
                   title="CI dispatch PR 9"),
            ci_run(47, status="in_progress", event="workflow_dispatch", head=HEAD, prs=(),
                   title="CI dispatch PR 7"),
            ci_run(48, status="pending", head=OLD)]
    gate = done(ci=live)
    admit(sync, pull=pull_json(), gate=gate)
    check("supersede: this PR's runs for OTHER heads, dispatched ones included, are cancelled",
          sorted(gate.cancelled), [41, 42, 45, 48])
    gate = done(ci=live)
    admit(_event("workflow_dispatch", ""), pull=pull_json(), gate=gate)
    check("supersede: a dispatch run supersedes older heads the same way",
          sorted(gate.cancelled), [41, 42, 45, 48])
    gate = done(ci=live)
    admit(_event("pull_request", "synchronize", head=OTHER), pull=pull_json(), gate=gate)
    check("supersede: a run whose head is no longer the PR's cancels nothing",
          gate.cancelled, [])
    # Round 5: the PR advances to a newer head after this run's identity read.
    # Its run (a LARGER id) belongs to the newer head and must survive.
    newer = [ci_run(41, status="in_progress", head=OLD),
             ci_run(901, status="in_progress", head=OTHER),
             ci_run(902, status="queued", event="workflow_dispatch", head=OTHER, prs=(),
                    title="CI dispatch PR 7")]
    gate = done(ci=newer)
    admit(sync, pull=pull_json(), gate=gate, run_id="900")
    check("supersede: only runs older than this one are cancelled, never a newer head's",
          gate.cancelled, [41])
    gate = done(ci=newer)
    admit(sync, pull=pull_json(), gate=gate, run_id="")
    check("supersede: without a run id nothing is cancelled", gate.cancelled, [])

    ran = ci_run(50)
    check("duplicate: an earlier run that ran CI for this head is mirrored, not repeated",
          (lambda r: (r.eligible, r.mode, r.code, r.mirror_build_test, r.mirror_probes))(
              admit(approve, pull=pull_json(), gate=done(ci=[ran]))),
          (False, MODE_DUP, "duplicate-head", "success", "success"))
    r = admit(sync, pull=pull_json(), gate=done(ci=[ci_run(50, build="failure", probes="success")]))
    check("duplicate: a failed earlier verdict is mirrored as failed",
          (r.eligible, r.mirror_build_test, r.mirror_probes), (False, "failure", "success"))
    for label, prior in (("an un-admitted run", ci_run(50, audits="skipped", build="failure", probes="failure")),
                         ("a cancelled run", ci_run(50, audits="cancelled", build="cancelled", probes="cancelled")),
                         ("a mirror", ci_run(50, audits="skipped", build="success", probes="success")),
                         ("a run for another head", ci_run(50, head=OLD)),
                         ("this run itself", ci_run(900))):
        check(f"duplicate: {label} is not a prior CI run",
              admit(approve, pull=pull_json(), gate=done(ci=[prior])).eligible, True)
    check("duplicate: an unfinished run (an unrelated-label run in its own group) is ignored",
          admit(approve, pull=pull_json(), gate=done(ci=[ci_run(50, status="in_progress")])).eligible,
          True)
    check("duplicate: ... even beside a finished one it must still mirror",
          admit(approve, pull=pull_json(),
                gate=done(ci=[ci_run(49), ci_run(50, status="in_progress")])).mode, MODE_DUP)
    for label, prior in (("another pull request's run for the same commit", ci_run(50, pr="8", prs=("8",))),
                         ("a run on another base sha", ci_run(50, base_sha="9" * 40)),
                         ("a run on another base branch", ci_run(50, base_ref="release")),
                         ("a run that recorded no identity", ci_run(50, identity=False))):
        check(f"duplicate: {label} is not this PR's CI result",
              admit(approve, pull=pull_json(), gate=done(ci=[prior])).eligible, True)
    check("duplicate: unreadable earlier runs fail closed",
          admit(approve, pull=pull_json(), gate=done(ci_unreadable=True)).code, "prior-run-unreadable")
    check("duplicate: the newest run that ran CI wins",
          admit(approve, pull=pull_json(),
                gate=done(ci=[ci_run(50, build="failure"), ci_run(51, build="success"),
                              ci_run(52, audits="skipped", build="failure")])).mirror_build_test, "success")
    # Round 6 of #2928: a failed automatic run, a successful manual recovery,
    # then a later approval. The newest result for the identity wins, and a
    # dispatched run (whose record names no PR) is identified by its job.
    recovered = [ci_run(500, build="failure", probes="failure"),
                 ci_run(600, event="workflow_dispatch", prs=(), title="CI dispatch PR 7")]
    r = admit(approve, pull=pull_json(), gate=done(ci=recovered))
    check("duplicate: a successful dispatch recovery supersedes the automatic failure",
          (r.mode, r.mirror_build_test, r.mirror_probes), (MODE_DUP, "success", "success"))
    r = admit(approve, pull=pull_json(),
              gate=done(ci=[ci_run(500), ci_run(600, event="workflow_dispatch", prs=(),
                                                build="failure", probes="failure")]))
    # Round 7: reruns keep their run id, so results order by latest execution.
    rerun_dispatch = lambda i, build, updated: ci_run(  # noqa: E731
        i, event="workflow_dispatch", prs=(), build=build,
        probes=build, updated=updated)
    r = admit(approve, pull=pull_json(), gate=done(ci=[
        rerun_dispatch(500, "success", 900), rerun_dispatch(600, "failure", 700)]))
    check("duplicate: an older run re-run after a newer one is the newest result (success)",
          (r.mirror_build_test, r.mirror_probes), ("success", "success"))
    r = admit(approve, pull=pull_json(), gate=done(ci=[
        rerun_dispatch(500, "failure", 900), rerun_dispatch(600, "success", 700)]))
    check("duplicate: ... and a rerun's newer failure replaces an older success",
          (r.mirror_build_test, r.mirror_probes), ("failure", "failure"))
    check("duplicate: a later failed dispatch is the verdict mirrored",
          (r.mirror_build_test, r.mirror_probes), ("failure", "failure"))
    check("duplicate: a dispatch for another PR is not this PR's result",
          admit(approve, pull=pull_json(),
                gate=done(ci=[ci_run(600, event="workflow_dispatch", prs=(), pr="8")])).eligible, True)
    check("duplicate: the admitted run's identity is published for later runs",
          admit(approve, pull=pull_json(), gate=done()).identity,
          identity_for("7", "master", BASE, HEAD))
    check("duplicate: an explicit dispatch is never suppressed",
          admit(_event("workflow_dispatch", ""), pull=pull_json(), gate=done(ci=[ran])).eligible, True)
    check("duplicate: a head that is not admitted reports its own reason, not a mirror",
          admit(approve, pull=pull_json(labels=frozenset()), gate=done(ci=[ran])).code, "not-approved")
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
    admit(sync, pull=pull_json(), gate=gate)
    check("admit: the live pull request is read last, after the dismissal",
          gate.order[-1], "pull")
    check("admit: ... and the dismissal after the identity read",
          [x for x in gate.order if x == "pull"][:1] + gate.order[1:2] == ["pull", "dismissal"], True)
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

    run_name = str(document.get("run-name", ""))
    check("ci.yml: a dispatched run's title names its pull request (supersession reads it)",
          _translate(run_name, {"github.event_name": "workflow_dispatch", "inputs.pr_number": "7"}),
          DISPATCH_TITLE.format("7"))
    check("ci.yml: other events keep their usual titles",
          _translate(run_name, {"github.event_name": "pull_request",
                                "github.event.pull_request.title": "Fix x"}), "Fix x")
    check("ci.yml: admission may cancel superseded runs",
          (jobs.get("admission") or {}).get("permissions", {}).get("actions"), "write")

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

    identity_job = jobs.get("ci-identity") or {}
    check("ci.yml: ci-identity is named by the admission's identity output",
          str(identity_job.get("name", "")).strip(), "${{ needs.admission.outputs.identity }}")
    check("ci.yml: ci-identity exists only for an admitted run",
          squash(identity_job.get("if", "")), "needs.admission.outputs.eligible == 'true'")
    check("ci.yml: admission publishes the identity",
          "identity" in (admission.get("outputs") or {}), True)

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
                "github.ref": "refs/pull/7/merge", "github.sha": "m" * 40,
                "github.event.pull_request.head.sha": "a" * 40}
        base.update(context)
        cancel = concurrency.get("cancel-in-progress")
        return (_translate(str(concurrency.get("group")), base),
                cancel if isinstance(cancel, bool)
                else bool(_translate(str(cancel), base)))

    sync = route(**{"github.event.action": "synchronize"})
    approve = route(**{"github.event.action": "labeled", "github.event.label.name": APPROVAL_LABEL,
                       "github.run_id": "901"})
    stray = route(**{"github.event.action": "labeled", "github.event.label.name": "tooling"})
    other_head = route(**{"github.event.action": "synchronize",
                          "github.event.pull_request.head.sha": "b" * 40})
    dispatch_route = route(**{"github.event_name": "workflow_dispatch", "github.ref": "refs/heads/topic",
                              "inputs.pr_number": "7", "github.event.action": "", "github.sha": "a" * 40})
    push_a = route(**{"github.event_name": "push", "github.sha": "1" * 40})
    push_b = route(**{"github.event_name": "push", "github.sha": "2" * 40})
    check("routing: approval and synchronize of ONE head share a group", sync[0], approve[0])
    check("routing: ... and a dispatch of that head joins it", dispatch_route[0], sync[0])
    check("routing: same-head events never cancel each other's work",
          (sync[1], approve[1], dispatch_route[1]), (False, False, False))
    check("routing: another head is another group (superseding is explicit)",
          other_head[0] != sync[0], True)
    check("routing: an unrelated label gets a group of its own", stray[0] not in (sync[0], approve[0]), True)
    check("routing: ... and never cancels", stray[1], False)
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
                      eligible: str | None = None, mode: str | None = None,
                      mirror: str = "") -> dict:
        values = {"ADMITTED": eligible if eligible is not None else ("true" if admission and admission.eligible else "false"),
                  "ADMISSION_MODE": mode if mode is not None else (admission.mode if admission else ""),
                  "ADMISSION_REASON": admission.reason if admission else "",
                  "MIRROR_RESULT": mirror,
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
    # A duplicate head mirrors the earlier run's verdicts, and only a pass.
    dup = Admission(False, MODE_DUP, "duplicate-head", "run 50 already ran CI for this head")
    for mirror, want in (("success", True), ("failure", False), ("cancelled", False), ("", False)):
        check(f"verdict: a duplicate mirroring {mirror or '(nothing)'} on build-test",
              passes(build_script, build_env_for(dup, mirror=mirror)), want)
        check(f"verdict: a duplicate mirroring {mirror or '(nothing)'} on behavior-probes",
              passes(probe_script, build_env_for(dup, mirror=mirror)), want)
    check("verdict: a mirror cannot rescue a head that is not a duplicate",
          passes(build_script, build_env_for(decided["unapproved head"], mirror="success")), False)
    check("verdict: a mirror cannot rescue a crashed admission",
          passes(build_script, build_env_for(None, eligible="", mirror="success")), False)
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
    for flag in ("repo", "event-name", "action", "label", "run-sha", "run-id", "pr-number",
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
