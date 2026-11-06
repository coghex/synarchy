#!/usr/bin/env python3
"""Admission for a dispatched pull-request CI run (#2927).

Pull-request CI never starts on a pull-request event. `.github/workflows/ci.yml`
runs its expensive work for a pull request only through `workflow_dispatch`,
which the manager starts once the head is approved and up to date:

    gh workflow run ci.yml --ref <branch> -f pr=<N> -f sha=<head sha>

This helper is the first job of that run. It checks the INPUTS against the
pull request and the repository, and nothing else: it never reads another
workflow run's listing or result, and it never trusts the label. A master push
is admitted unconditionally (it is the post-merge backstop). A dispatch is
admitted only when every one of these holds, and any evidence it cannot read
fails closed:

  1. `pr` and `sha` are well formed, and the PR is open.
  2. `sha` is the pull request's CURRENT head.
  3. The run itself executes `sha` (`github.sha`). A valid PR/SHA pair
     dispatched with a different `--ref` fails: the checkouts of every job,
     the reusable image workflow, and the check runs the required contexts
     attach to all follow `github.sha`, so binding it here binds all of them.
  4. `sha` contains the base branch's current tip, after a fresh fetch
     (`git merge-base --is-ancestor <tip> <sha>`); never the synthetic merge
     ref, which can contain the base when the branch does not.
  5. The review service's head-bound marker approves it. The newest
     `<!-- pr-review:v2 ... head=<sha> verdict=... -->` record by the trusted
     review account is selected over ALL comment pages, then evaluated:
       (a) verdict APPROVE and `head=` names `sha`; or
       (b) verdict APPROVE and `head=` names an earlier head A that
           `tools/review_gate_decision.py` (A -> sha) decides only moved the
           branch's ancestry (a clean replay of the approved content onto a
           newer base) -- the rule review-gate.yml uses to keep the label.
     An older APPROVE is never searched for behind a newer trusted record,
     whatever that record says; a malformed newest record, a record by anyone
     else and a lingering `reviewed:approve` label grant nothing.

The decision is published as outputs. A refused dispatch still SUCCEEDS here:
`eligible=false` plus a reason. The workers are gated on it, and the required
contexts `build-test` and `behavior-probes` then FAIL with the reason, because
GitHub reports a skipped required job as passing.

`--self-test [--workflow PATH]` runs the decision tables, throwaway git
graphs, and the workflow's wiring (including the real verdict scripts of the
required contexts).
"""

from __future__ import annotations

import argparse
import json
import os
import re
import subprocess
import sys
import tempfile
from pathlib import Path
from typing import Callable, NamedTuple, Sequence

REPO_ROOT = Path(__file__).resolve().parent.parent
sys.path.insert(0, str(Path(__file__).resolve().parent))

import review_gate_decision  # noqa: E402  (same directory; the replay rule)

CI_WORKFLOW = REPO_ROOT / ".github" / "workflows" / "ci.yml"
CI_IMAGE_WORKFLOW = REPO_ROOT / ".github" / "workflows" / "ci-image.yml"

MODE_PUSH = "push"
MODE_PR = "pr"
MODE_NONE = "none"

_SHA = re.compile(r"[0-9a-f]{40}")
_NUMBER = re.compile(r"[1-9][0-9]{0,9}")
_REF = re.compile(r"[A-Za-z0-9_][A-Za-z0-9_./-]*")

# A review record is a line of its own that opens the marker. Whether it is a
# well-formed record is decided separately, so a damaged newest record fails
# closed instead of being skipped.
RECORD_LINE = re.compile(r"^<!-- pr-review:v2\b.*$", re.MULTILINE)
RECORD = re.compile(
    r"<!-- pr-review:v2 reviewers=(?P<reviewers>\S+) models=(?P<models>\S+) "
    r"head=(?P<head>[0-9a-f]{40}) verdict=(?P<verdict>APPROVE|CHANGES_REQUESTED) -->"
)


class Admission(NamedTuple):
    eligible: bool
    mode: str
    reason: str
    pr_number: str = ""
    head_sha: str = ""
    base_ref: str = ""
    base_sha: str = ""
    fork: bool = False


def refuse(reason: str, **kw: object) -> Admission:
    return Admission(False, MODE_NONE, reason, **kw)  # type: ignore[arg-type]


class EvidenceError(Exception):
    """Evidence that could not be read; admission fails closed on it."""


class Evidence(NamedTuple):
    """Everything admission reads, behind one seam so tests can script it."""

    pull: Callable[[str, str], dict]
    comments: Callable[[str, str], list]
    base_tip: Callable[[str], str]
    contains: Callable[[str, str], bool]
    replay_keeps: Callable[[str, str, str, str], bool]


# ---------------------------------------------------------------------------
# Decision
# ---------------------------------------------------------------------------

def latest_review_record(comments: list, author: str) -> tuple[str, str]:
    """(head, verdict) of the newest trusted review record.

    Raises EvidenceError when there is none or the newest is malformed. Only
    comments by `author` (a User account, compared case-insensitively) count;
    among those the newest by creation time then id is the one record, and its
    last marker line is the one read. Nothing older is consulted.
    """
    trusted = []
    for comment in comments:
        if not isinstance(comment, dict):
            raise EvidenceError("a comment entry is not an object")
        user = comment.get("user") or {}
        if str(user.get("login", "")).lower() != author.lower() or user.get("type") != "User":
            continue
        lines = RECORD_LINE.findall(str(comment.get("body") or ""))
        if not lines:
            continue
        trusted.append((str(comment.get("created_at") or ""), int(comment.get("id") or 0), lines[-1].strip()))
    if not trusted:
        raise EvidenceError(f"no review record by {author} on this pull request")
    trusted.sort(key=lambda item: (item[0], item[1]))
    match = RECORD.fullmatch(trusted[-1][2])
    if not match:
        raise EvidenceError("the newest review record is malformed")
    return match.group("head"), match.group("verdict")


def decide(event_name: str, run_sha: str, pr_number: str, sha: str, repo: str,
           author: str, ev: Evidence) -> Admission:
    run_sha = (run_sha or "").strip().lower()
    if event_name == "push":
        return Admission(True, MODE_PUSH, "master push: always admitted", head_sha=run_sha)
    if event_name != "workflow_dispatch":
        return refuse(f"event {event_name!r} does not run pull-request CI (dispatch it)")

    pr_number = (pr_number or "").strip()
    sha = (sha or "").strip().lower()
    if not _NUMBER.fullmatch(pr_number):
        return refuse(f"input pr {pr_number!r} is not a pull request number")
    if not _SHA.fullmatch(sha):
        return refuse(f"input sha {sha!r} is not a full 40-character commit SHA", pr_number=pr_number)
    if run_sha != sha:
        return refuse(f"this run executes {run_sha[:12] or '(unknown)'}, not the supplied {sha[:12]}: "
                      f"dispatch with --ref naming a branch whose tip is {sha}",
                      pr_number=pr_number, head_sha=sha)

    def no(reason: str, **kw: object) -> Admission:
        return refuse(reason, pr_number=pr_number, head_sha=sha, **kw)  # type: ignore[arg-type]

    try:
        pull = ev.pull(repo, pr_number)
        if not isinstance(pull, dict):
            raise EvidenceError("the pull request is unreadable")
        if pull.get("state") != "open":
            return no(f"pull request {pr_number} is not open")
        head, base = pull.get("head") or {}, pull.get("base") or {}
        head_sha = str(head.get("sha") or "").lower()
        base_ref = str(base.get("ref") or "")
        head_repo = (head.get("repo") or {}).get("full_name")
        if not _SHA.fullmatch(head_sha) or not _REF.fullmatch(base_ref) or not head_repo:
            raise EvidenceError("the pull request's head or base identity is unreadable")
        fork = str(head_repo).lower() != repo.lower()
        if head_sha != sha:
            return no(f"{sha[:12]} is not pull request {pr_number}'s current head ({head_sha[:12]})",
                      base_ref=base_ref, fork=fork)
        tip = ev.base_tip(base_ref)
        if not _SHA.fullmatch(tip):
            raise EvidenceError(f"the tip of {base_ref} is unreadable")
        known = {"base_ref": base_ref, "base_sha": tip, "fork": fork}
        if not ev.contains(tip, sha):
            return no(f"{sha[:12]} does not contain {base_ref}'s current tip {tip[:12]}: "
                      f"update the branch, get it re-approved if needed, then dispatch", **known)
        approved_head, verdict = latest_review_record(ev.comments(repo, pr_number), author)
    except Exception as error:  # noqa: BLE001 - any read failure fails closed
        return no(f"evidence unreadable, failing closed: {error}")

    if verdict != "APPROVE":
        return no(f"the newest review record is {verdict} (head {approved_head[:12]}): not approved", **known)
    if approved_head != sha:
        try:
            keeps = ev.replay_keeps(approved_head, sha, base_ref, tip)
        except Exception as error:  # noqa: BLE001
            return no(f"could not compare approved head {approved_head[:12]}: {error}", **known)
        if not keeps:
            return no(f"the approval names {approved_head[:12]}, and {sha[:12]} is not that head "
                      f"carried onto a newer base: not approved at this head", **known)
        how = f"approved at {approved_head[:12]}, carried onto the base"
    else:
        how = "approved at this head"
    return Admission(True, MODE_PR, f"{how}; contains {base_ref}@{tip[:12]}",
                     pr_number=pr_number, head_sha=sha, base_ref=base_ref, base_sha=tip, fork=fork)


# ---------------------------------------------------------------------------
# Live evidence
# ---------------------------------------------------------------------------

def _run(*args: str, cwd: str = ".") -> tuple[int, str, str]:
    try:
        done = subprocess.run(list(args), cwd=cwd, capture_output=True, text=True, check=False)
    except OSError as error:
        return -1, "", str(error)
    return done.returncode, done.stdout, done.stderr


def live_evidence(repo_dir: str = ".") -> Evidence:
    def gh_json(*args: str) -> object:
        code, out, err = _run("gh", "api", *args)
        if code != 0:
            raise EvidenceError(f"gh api {args[-1]} failed: {err.strip()[:200]}")
        try:
            return json.loads(out)
        except ValueError as error:
            raise EvidenceError(f"gh api {args[-1]} returned unreadable JSON") from error

    def pull(repo: str, number: str) -> dict:
        value = gh_json(f"repos/{repo}/pulls/{number}")
        if not isinstance(value, dict):
            raise EvidenceError("the pull request is unreadable")
        return value

    def comments(repo: str, number: str) -> list:
        pages = gh_json("--paginate", "--slurp", f"repos/{repo}/issues/{number}/comments?per_page=100")
        if not isinstance(pages, list):
            raise EvidenceError("the comment listing is unreadable")
        flat: list = []
        for page in pages:
            if not isinstance(page, list):
                raise EvidenceError("a comment page is unreadable")
            flat.extend(page)
        return flat

    def base_tip(base_ref: str) -> str:
        code, _, err = _run("git", "fetch", "--no-tags", "--quiet", "origin",
                            f"+refs/heads/{base_ref}:refs/remotes/origin/{base_ref}", cwd=repo_dir)
        if code != 0:
            raise EvidenceError(f"fetching {base_ref} failed: {err.strip()[:200]}")
        code, out, err = _run("git", "rev-parse", "--verify", f"refs/remotes/origin/{base_ref}^{{commit}}",
                              cwd=repo_dir)
        if code != 0:
            raise EvidenceError(f"{base_ref} is unreadable: {err.strip()[:200]}")
        return out.strip()

    def contains(tip: str, sha: str) -> bool:
        code, _, err = _run("git", "merge-base", "--is-ancestor", tip, sha, cwd=repo_dir)
        if code == 0:
            return True
        if code == 1:
            return False
        raise EvidenceError(f"ancestry of {sha[:12]} could not be determined: {err.strip()[:200]}")

    def replay_keeps(approved: str, sha: str, base_ref: str, tip: str) -> bool:
        # The approved head may have been force-pushed away; fetching it by
        # SHA is best effort, and an unreachable head simply is not kept.
        _run("git", "fetch", "--no-tags", "--quiet", "origin", approved, cwd=repo_dir)
        decision = review_gate_decision.decide(review_gate_decision.Git(repo_dir), approved, sha, base_ref, tip)
        return decision.keeps_label

    return Evidence(pull, comments, base_tip, contains, replay_keeps)


def _single_line(text: str) -> str:
    return " ".join(str(text).split())


def _emit(result: Admission, path: str) -> None:
    if not path:
        return
    outputs = {
        "eligible": "true" if result.eligible else "false",
        "mode": result.mode,
        "reason": result.reason,
        "pr_number": result.pr_number,
        "head_sha": result.head_sha,
        "base_ref": result.base_ref,
        "base_sha": result.base_sha,
        "fork": "true" if result.fork else "false",
    }
    with open(path, "a", encoding="utf-8") as handle:
        for key, value in outputs.items():
            handle.write(f"{key}={_single_line(value)}\n")


def _admit_main(args: argparse.Namespace) -> int:
    result = decide(args.event_name, args.run_sha, args.pr_number, args.sha, args.repo,
                    args.review_author, live_evidence(args.repo_dir))
    line = f"CI admission: {'ADMITTED' if result.eligible else 'NOT admitted'} (mode {result.mode}) {result.reason}"
    print(line)
    _emit(result, args.github_output)
    if args.github_summary:
        with open(args.github_summary, "a", encoding="utf-8") as handle:
            handle.write(f"### CI admission\n\n{line}\n")
    return 0


# ---------------------------------------------------------------------------
# Self-test
# ---------------------------------------------------------------------------

AUTHOR = "coghex"
REPO = "coghex/synarchy"
A, B, C, D = "a" * 40, "b" * 40, "c" * 40, "d" * 40


def marker(head: str, verdict: str = "APPROVE") -> str:
    return f"<!-- pr-review:v2 reviewers=codex models=unspecified head={head} verdict={verdict} -->"


def comment(cid: int, body: str, login: str = AUTHOR, kind: str = "User", created: str | None = None) -> dict:
    return {"id": cid, "created_at": created or f"2026-11-06T10:{cid % 60:02d}:00Z", "body": body,
            "user": {"login": login, "type": kind}}


def fake(*, head: str = A, state: str = "open", head_repo: str = REPO, base_ref: str = "master",
         tip: str = C, contained: bool = True, comments: object = None, keeps: bool = False,
         labels: Sequence[str] = ()) -> Evidence:
    pull = {"state": state, "labels": [{"name": n} for n in labels],
            "head": {"sha": head, "ref": "feature", "repo": {"full_name": head_repo} if head_repo else None},
            "base": {"ref": base_ref, "sha": tip}}

    def read(*_: object) -> object:
        if isinstance(comments, Exception):
            raise comments
        return comments if comments is not None else []

    return Evidence(lambda *_: pull, read, lambda ref: tip, lambda t, s: contained, lambda *a: keeps)


def _raise(error: Exception) -> Callable[..., object]:
    def thrower(*_: object) -> object:
        raise error
    return thrower


def _ci_document(path: Path) -> dict:
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


def _translate(text: str, ctx: dict) -> object:
    """Evaluate the GitHub Actions expression grammar ci.yml's routing uses."""
    text = text.strip()
    if text.startswith("${{") and text.endswith("}}"):
        text = text[3:-2]
    text = text.replace("always()", "true")
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


class _Graph:
    """A throwaway origin + clone, for the live git evidence."""

    def __init__(self, root: Path) -> None:
        self.origin = root / "origin.git"
        self.work = root / "work"
        self.env = {**os.environ, "GIT_AUTHOR_NAME": "t", "GIT_AUTHOR_EMAIL": "t@t",
                    "GIT_COMMITTER_NAME": "t", "GIT_COMMITTER_EMAIL": "t@t"}
        subprocess.run(["git", "init", "-q", "--bare", "-b", "master", str(self.origin)], check=True)
        subprocess.run(["git", "init", "-q", "-b", "master", str(self.work)], check=True)
        self.git("remote", "add", "origin", str(self.origin))

    def git(self, *args: str) -> str:
        done = subprocess.run(["git", "-C", str(self.work), *args], env=self.env, check=True,
                              capture_output=True, text=True)
        return done.stdout.strip()

    def commit(self, name: str, text: str, message: str) -> str:
        (self.work / name).write_text(text)
        self.git("add", name)
        self.git("commit", "-q", "-m", message)
        return self.git("rev-parse", "HEAD")


def _self_test(workflow: Path = CI_WORKFLOW, image_workflow: Path = CI_IMAGE_WORKFLOW) -> int:  # noqa: C901
    failures: list[str] = []

    def check(label: str, got: object, want: object) -> None:
        if got != want:
            failures.append(f"{label}: got {got!r}, want {want!r}")

    def run(sha: str = A, *, event: str = "workflow_dispatch", run_sha: str | None = None,
            pr: str = "7", ev: Evidence | None = None) -> Admission:
        return decide(event, sha if run_sha is None else run_sha, pr, sha, REPO, AUTHOR, ev or fake())

    approve = [comment(1, "x\n" + marker(A))]

    # -- the acceptance cases -----------------------------------------------------
    admitted = run(ev=fake(comments=approve))
    check("approved, up-to-date head is admitted", admitted.eligible, True)
    check("... as pr mode with the resolved base", (admitted.mode, admitted.base_sha, admitted.base_ref),
          ("pr", C, "master"))
    check("master push is admitted", run(event="push").mode, "push")
    check("a push is not asked for approval evidence",
          run(event="push", ev=fake(comments=RuntimeError("down"))).eligible, True)
    stale = run(B, ev=fake(head=A, comments=[comment(1, marker(B))]))
    check("stale SHA (no longer the head) is refused", stale.eligible, False)
    check("... naming the head", "current head" in stale.reason, True)
    check("unapproved head (no record) is refused", run(ev=fake(comments=[])).eligible, False)
    check("unapproved head (CHANGES_REQUESTED) is refused",
          run(ev=fake(comments=[comment(1, marker(A, "CHANGES_REQUESTED"))])).eligible, False)
    behind = run(ev=fake(comments=approve, contained=False))
    check("approved but behind the base is refused", behind.eligible, False)
    check("... naming the base", "does not contain" in behind.reason, True)
    check("a closed pull request is refused", run(ev=fake(comments=approve, state="closed")).eligible, False)
    failed = run(ev=fake(comments=RuntimeError("503")))
    check("unreadable comments fail closed", failed.eligible, False)
    check("... with the cause", "failing closed" in failed.reason, True)
    broken = fake(comments=approve)
    check("an unreadable base tip fails closed", run(ev=broken._replace(base_tip=lambda ref: "")).eligible, False)
    check("a base fetch error fails closed",
          run(ev=broken._replace(base_tip=_raise(EvidenceError("x")))).eligible, False)
    check("an ancestry error fails closed",
          run(ev=broken._replace(contains=_raise(EvidenceError("x")))).eligible, False)
    check("a pull request fetch error fails closed",
          run(ev=broken._replace(pull=_raise(EvidenceError("x")))).eligible, False)
    check("a pull request that is not an object fails closed",
          run(ev=broken._replace(pull=lambda *_: [])).eligible, False)
    check("a deleted fork fails closed", run(ev=fake(comments=approve, head_repo="")).eligible, False)
    check("a refusal never reports an eligible mode", failed.mode, "none")

    # -- input shape and run-revision binding ---------------------------------------
    check("a short sha is refused", run("abc123", run_sha="abc123", ev=fake(comments=approve)).eligible, False)
    check("a non-number pr is refused", run(pr="7; rm", ev=fake(comments=approve)).eligible, False)
    check("a leading-zero pr is refused", run(pr="07", ev=fake(comments=approve)).eligible, False)
    check("an empty pr is refused", run(pr="", ev=fake(comments=approve)).eligible, False)
    mismatch = run(run_sha=B, ev=fake(comments=approve))
    check("the run must execute the supplied sha (valid pair, different --ref)", mismatch.eligible, False)
    check("... naming --ref", "--ref" in mismatch.reason, True)
    check("an empty run revision is refused", run(run_sha="", ev=fake(comments=approve)).eligible, False)
    check("sha case does not matter", run(A.upper(), run_sha=A, ev=fake(comments=approve)).eligible, True)
    check("a pull_request event runs nothing", run(event="pull_request", ev=fake(comments=approve)).eligible, False)
    check("an unknown event runs nothing", run(event="schedule", ev=fake(comments=approve)).eligible, False)

    # -- the label is not evidence --------------------------------------------------
    check("a lingering label with no record is refused",
          run(ev=fake(comments=[], labels=["reviewed:approve"])).eligible, False)
    check("a lingering label after an unapproved push (record names the old head) is refused",
          run(B, ev=fake(head=B, comments=[comment(1, marker(A))], labels=["reviewed:approve"])).eligible, False)
    check("a lingering label beside a newer rejection is refused",
          run(ev=fake(comments=[comment(1, marker(A)), comment(2, marker(A, "CHANGES_REQUESTED"))],
                      labels=["reviewed:approve"])).eligible, False)

    # -- marker selection -------------------------------------------------------------
    check("a re-approval at a new head is admitted",
          run(B, ev=fake(head=B, comments=[comment(1, marker(A)), comment(2, marker(A, "CHANGES_REQUESTED")),
                                           comment(3, marker(B))])).eligible, True)
    check("an older APPROVE behind a newer CHANGES_REQUESTED is never used",
          run(ev=fake(comments=[comment(1, marker(A)), comment(2, marker(A, "CHANGES_REQUESTED"))])).eligible, False)
    malformed = run(ev=fake(comments=[comment(1, marker(A)), comment(2, "<!-- pr-review:v2 oops -->")]))
    check("a malformed newest record fails closed, even with an older APPROVE", malformed.eligible, False)
    check("... naming the malformed record", "malformed" in malformed.reason, True)
    check("a bad head in the newest record fails closed",
          run(ev=fake(comments=[comment(1, marker(A)),
                                comment(2, "<!-- pr-review:v2 reviewers=codex models=u head=nonsense "
                                           "verdict=APPROVE -->")])).eligible, False)
    check("a truncated head in the newest record fails closed",
          run(ev=fake(comments=[comment(1, marker(A)), comment(2, marker(A[:39]))])).eligible, False)
    check("an APPROVE by anyone else is ignored",
          run(ev=fake(comments=[comment(1, marker(A), login="mallory")])).eligible, False)
    check("newer records by anyone else cannot replace the trusted one",
          run(ev=fake(comments=[comment(1, marker(A)), comment(2, marker(A, "CHANGES_REQUESTED"), login="mallory"),
                                comment(3, marker(B), login="mallory")])).eligible, True)
    check("a lookalike login is ignored",
          run(ev=fake(comments=[comment(1, marker(A), login="coghex-bot")])).eligible, False)
    check("a Bot account of the same name is ignored",
          run(ev=fake(comments=[comment(1, marker(A), kind="Bot")])).eligible, False)
    check("the author compares case-insensitively",
          run(ev=fake(comments=[comment(1, marker(A), login="CogHex")])).eligible, True)
    check("a marker quoted mid-line is not a record",
          run(ev=fake(comments=[comment(1, marker(A)),
                                comment(2, "I saw " + marker(A, "CHANGES_REQUESTED"))])).eligible, True)
    check("the last marker of the newest comment decides",
          run(ev=fake(comments=[comment(1, marker(A, "CHANGES_REQUESTED") + "\n" + marker(A))])).eligible, True)
    check("order is by time, not list position",
          run(ev=fake(comments=[comment(2, marker(A, "CHANGES_REQUESTED"), created="2026-11-06T11:00:00Z"),
                                comment(1, marker(A), created="2026-11-06T10:00:00Z")])).eligible, False)
    check("a record after many unrelated comments is found",
          run(ev=fake(comments=[comment(i, "chatter") for i in range(1, 250)]
                      + [comment(300, marker(A), created="2026-11-07T00:00:00Z")])).eligible, True)
    check("an APPROVE on page 1 does not beat a rejection on page 3",
          run(ev=fake(comments=[comment(1, marker(A), created="2026-11-06T10:00:00Z")]
                      + [comment(i, "x", created=f"2026-11-06T11:{i % 60:02d}:00Z") for i in range(2, 230)]
                      + [comment(900, marker(A, "CHANGES_REQUESTED"), created="2026-11-06T12:00:00Z")])).eligible,
          False)
    check("a non-object comment entry fails closed", run(ev=fake(comments=["x"])).eligible, False)

    # -- approval carried onto a newer base ---------------------------------------------
    carried = run(B, ev=fake(head=B, comments=[comment(1, marker(A))], keeps=True))
    check("an earlier approved head that was only carried forward is admitted", carried.eligible, True)
    check("... and says so", "carried onto the base" in carried.reason, True)
    check("a content-changing update after approval is refused",
          run(B, ev=fake(head=B, comments=[comment(1, marker(A))], keeps=False)).eligible, False)
    check("a replay that cannot be evaluated fails closed",
          run(B, ev=fake(head=B, comments=[comment(1, marker(A))])._replace(
              replay_keeps=_raise(RuntimeError("x")))).eligible, False)
    check("CHANGES_REQUESTED is never carried by a clean replay",
          run(B, ev=fake(head=B, comments=[comment(1, marker(A, "CHANGES_REQUESTED"))], keeps=True)).eligible, False)

    # -- fork identity (by pull request number and SHA, never by branch name) -----------
    check("a same-repo head is not a fork", run(ev=fake(comments=approve)).fork, False)
    check("a fork head is a fork", run(ev=fake(comments=approve, head_repo="someone/synarchy")).fork, True)
    check("a fork is still approval-gated",
          run(ev=fake(comments=[], head_repo="someone/synarchy")).eligible, False)
    check("a fork admitted with an approval reports fork",
          run(ev=fake(comments=approve, head_repo="someone/synarchy")).fork, True)
    check("the fork comparison is case-insensitive",
          run(ev=fake(comments=approve, head_repo="CogHex/Synarchy")).fork, False)
    check("a stale fork SHA still reports the fork",
          run(B, ev=fake(head=A, comments=approve, head_repo="someone/synarchy")).fork, True)
    # Same branch name `feature` in two repositories: the fork PR's head repo,
    # not the branch name, decides, so a same-repo `feature` PR is not a fork.
    check("same-named branches in different repositories are told apart",
          (run(ev=fake(comments=approve, head_repo=REPO)).fork,
           run(ev=fake(comments=approve, head_repo="someone/synarchy")).fork), (False, True))

    # -- real git -------------------------------------------------------------------------
    with tempfile.TemporaryDirectory() as tmp:
        g = _Graph(Path(tmp))
        g.commit("base.txt", "one\n", "base0")
        g.git("push", "-q", "origin", "master")
        g.git("checkout", "-q", "-b", "feature")
        approved = g.commit("feature.txt", "feature\n", "feature")
        g.git("checkout", "-q", "master")
        base1 = g.commit("other.txt", "other\n", "base1")
        g.git("push", "-q", "origin", "master")
        g.git("checkout", "-q", "feature")
        g.git("merge", "-q", "--no-edit", "master")
        merged = g.git("rev-parse", "HEAD")
        edited = g.commit("feature.txt", "feature edited\n", "edit")
        live = live_evidence(str(g.work))
        check("git: the base tip is fetched fresh", live.base_tip("master"), base1)
        check("git: a head containing the tip is current", live.contains(base1, merged), True)
        check("git: a head behind the tip is not", live.contains(base1, approved), False)
        check("git: an unknown commit is an error, not 'behind'", _raises(lambda: live.contains(base1, D)), True)
        check("git: a clean base update keeps the approval",
              live.replay_keeps(approved, merged, "master", base1), True)
        check("git: a content change after approval does not",
              live.replay_keeps(approved, edited, "master", base1), False)
        check("git: an unreachable approved head does not", live.replay_keeps(D, merged, "master", base1), False)

        def ev_for(head: str, records: list) -> Evidence:
            return Evidence(lambda *_: {"state": "open", "head": {"sha": head, "repo": {"full_name": REPO}},
                                        "base": {"ref": "master"}},
                            lambda *_: records, live.base_tip, live.contains, live.replay_keeps)

        def real(head: str, records: list) -> Admission:
            return decide("workflow_dispatch", head, "7", head, REPO, AUTHOR, ev_for(head, records))

        check("git: approved A then a clean update to B admits",
              real(merged, [comment(1, marker(approved))]).eligible, True)
        check("git: approved at B itself admits", real(merged, [comment(1, marker(merged))]).eligible, True)
        check("git: a content edit after approval is refused",
              real(edited, [comment(1, marker(approved))]).eligible, False)
        check("git: an approved head behind the base is refused",
              real(approved, [comment(1, marker(approved))]).eligible, False)
        check("git: an approval of an edit that was then rejected is refused",
              real(edited, [comment(1, marker(edited)), comment(2, marker(edited, "CHANGES_REQUESTED"))]).eligible,
              False)
        nowhere = live_evidence(str(Path(tmp) / "nowhere"))
        check("git: a fetch failure is an error", _raises(lambda: nowhere.base_tip("master")), True)

    _check_workflow(workflow, image_workflow, check)

    if failures:
        print(f"ci_pr_admission self-test: {len(failures)} failure(s)")
        for line in failures:
            print(f"  FAIL {line}")
        return 1
    print("ci_pr_admission self-test: ok")
    return 0


def _raises(call: Callable[[], object]) -> bool:
    try:
        call()
    except Exception:  # noqa: BLE001
        return True
    return False


def _check_workflow(workflow: Path, image_workflow: Path, check: Callable[[str, object, object], None]) -> None:  # noqa: C901
    document = _ci_document(workflow)
    triggers = document.get("on", document.get(True)) or {}
    jobs = document.get("jobs") or {}
    squash = lambda text: " ".join(str(text).split())  # noqa: E731

    check("ci.yml: only push and workflow_dispatch trigger it", sorted(triggers), ["push", "workflow_dispatch"])
    check("ci.yml: push stays master only", triggers.get("push"), {"branches": ["master"]})
    inputs = (triggers.get("workflow_dispatch") or {}).get("inputs") or {}
    check("ci.yml: dispatch inputs are pr and sha", sorted(inputs), ["pr", "sha"])
    for name in ("pr", "sha"):
        check(f"ci.yml: input {name} is required", (inputs.get(name) or {}).get("required"), True)

    concurrency = document.get("concurrency") or {}
    group = str(concurrency.get("group", ""))
    cancel = str(concurrency.get("cancel-in-progress", ""))
    dispatch = {"github.event_name": "workflow_dispatch", "inputs.pr": "7", "github.sha": A}
    push = {"github.event_name": "push", "github.sha": A}
    check("ci.yml: a dispatch's group is keyed on the PR number only", _translate(group, dispatch), "ci-pr-7")
    check("ci.yml: a newer head's dispatch supersedes the PR's older run", _translate(cancel, dispatch), True)
    check("ci.yml: a push never cancels another master push (group per SHA)", _translate(group, push), f"ci-{A}")
    check("ci.yml: a push does not cancel", _translate(cancel, push), False)

    admission = jobs.get("admission") or {}
    check("ci.yml: admission always runs", "if" in admission, False)
    for key in ("eligible", "mode", "reason", "base_sha", "base_ref", "head_sha", "fork", "pr_number"):
        check(f"ci.yml: admission exports {key}", key in (admission.get("outputs") or {}), True)
    steps = admission.get("steps") or []
    helper_step = next((s for s in steps if "ci_pr_admission.py admit" in str(s.get("run", ""))), {})
    check("ci.yml: admission runs the helper", bool(helper_step), True)
    check("ci.yml: admission checks out the run's own revision (no ref override)",
          all("ref" not in (s.get("with") or {}) for s in steps if "checkout" in str(s.get("uses", ""))), True)
    env = helper_step.get("env") or {}
    check("ci.yml: admission binds the run revision", env.get("RUN_SHA"), "${{ github.sha }}")
    check("ci.yml: admission reads the dispatch inputs",
          (env.get("PR_NUMBER"), env.get("SHA")), ("${{ inputs.pr }}", "${{ inputs.sha }}"))
    check("ci.yml: the trusted review author is the repository owner",
          env.get("REVIEW_AUTHOR"), "${{ github.repository_owner }}")

    def needs(job: dict) -> set:
        raw = job.get("needs")
        return {raw} if isinstance(raw, str) else set(raw or [])

    image = jobs.get("resolve-image") or {}
    check("ci.yml: resolve-image waits for admission", needs(image), {"admission"})
    check("ci.yml: resolve-image runs only when admitted",
          squash(image.get("if", "")), "needs.admission.outputs.eligible == 'true'")
    check("ci.yml: the image workflow is told whether the head is a fork",
          (image.get("with") or {}).get("fork"), "${{ needs.admission.outputs.fork == 'true' }}")
    check("ci.yml: the image workflow gets the admitted base",
          (image.get("with") or {}).get("base_sha"), "${{ needs.admission.outputs.base_sha }}")

    for worker in ("test-and-audits", "headless-lanes", "static-audits", "behavior-probe-worker"):
        job = jobs.get(worker) or {}
        check(f"ci.yml: {worker} needs admission and the image", needs(job) >= {"admission", "resolve-image"}, True)
        check(f"ci.yml: {worker} has no status function that could outrun admission",
              any(fn in str(job.get("if", "")) for fn in ("always()", "!cancelled()", "failure()")), False)
    check("ci.yml: the probe worker is PR-mode only",
          squash((jobs.get("behavior-probe-worker") or {}).get("if", "")), "needs.admission.outputs.mode == 'pr'")

    # Every job runs the admitted revision: none reads a pull_request field or picks a ref.
    text = workflow.read_text(encoding="utf-8")
    code = "\n".join(line for line in text.splitlines() if not line.lstrip().startswith("#"))
    check("ci.yml: no step reads a pull_request event field", "github.event.pull_request" in code, False)
    check("ci.yml: no step selects a checkout ref of its own",
          bool(re.search(r"^\s+ref:", code, re.MULTILINE)), False)

    # -- required contexts: execute the real verdict scripts ------------------------
    names = ("TESTS_RESULT", "AUDITS_RESULT", "LANES_RESULT", "PROBES_RESULT")

    def env_for(base_env: dict, mode: str, **over: str) -> dict:
        out = {key: "" for key in base_env}
        out.update({"ADMITTED": "true", "ADMISSION_MODE": mode, "ADMISSION_REASON": "ok",
                    "EVENT_NAME": "push" if mode == "push" else "workflow_dispatch"})
        out.update(over)
        return out

    for context, step_name in (("build-test", "Require parallel CI workers"),
                               ("behavior-probes", "Require the behavior-probe worker")):
        job = jobs.get(context) or {}
        check(f"ci.yml: {context} keeps its required name", str(job.get("name", context)), context)
        check(f"ci.yml: {context} always runs",
              squash(job.get("if", "")).replace("${{", "").replace("}}", "").strip(), "always()")
        check(f"ci.yml: {context} needs admission", "admission" in needs(job), True)
        script, base_env = _script(job, step_name)
        check(f"ci.yml: {context} reads the admission decision",
              {"ADMITTED", "ADMISSION_MODE", "ADMISSION_REASON"} <= set(base_env), True)
        if context == "build-test":
            good_pr = env_for(base_env, "pr", **{n: "success" for n in names})
            good_push = env_for(base_env, "push", **{**{n: "success" for n in names}, "PROBES_RESULT": "skipped"})
            check("build-test: an admitted PR run with every worker green passes", _verdict(script, good_pr), 0)
            check("build-test: an admitted master push passes with probes skipped", _verdict(script, good_push), 0)
            for worker in names:
                for bad in ("failure", "cancelled", "skipped"):
                    check(f"build-test: {worker}={bad} fails an admitted PR run",
                          _verdict(script, {**good_pr, worker: bad}), 1)
            check("build-test: a probe result on a push must be skipped",
                  _verdict(script, {**good_push, "PROBES_RESULT": "success"}), 1)
            check("build-test: a refused dispatch FAILS (every worker skipped)",
                  _verdict(script, env_for(base_env, "none", ADMITTED="false", **{n: "skipped" for n in names})), 1)
            check("build-test: a refused dispatch fails even if every worker somehow succeeded",
                  _verdict(script, {**good_pr, "ADMITTED": "false", "ADMISSION_MODE": "none"}), 1)
            check("build-test: an admission that produced no decision fails",
                  _verdict(script, {**good_pr, "ADMITTED": "", "ADMISSION_MODE": ""}), 1)
        else:
            check("behavior-probes: an admitted PR run needs the worker to succeed",
                  _verdict(script, env_for(base_env, "pr", WORKER_RESULT="success")), 0)
            for bad in ("failure", "cancelled", "skipped"):
                check(f"behavior-probes: worker {bad} fails an admitted PR run",
                      _verdict(script, env_for(base_env, "pr", WORKER_RESULT=bad)), 1)
            check("behavior-probes: a master push passes with the worker skipped",
                  _verdict(script, env_for(base_env, "push", WORKER_RESULT="skipped")), 0)
            check("behavior-probes: a master push with a running worker fails",
                  _verdict(script, env_for(base_env, "push", WORKER_RESULT="success")), 1)
            check("behavior-probes: a refused dispatch FAILS rather than skips",
                  _verdict(script, env_for(base_env, "none", ADMITTED="false", WORKER_RESULT="skipped")), 1)
            check("behavior-probes: an admission that produced no decision fails",
                  _verdict(script, env_for(base_env, "pr", ADMITTED="", WORKER_RESULT="success")), 1)

    # -- the reusable image workflow --------------------------------------------------
    image_doc = _ci_document(image_workflow)
    call = ((image_doc.get("on", image_doc.get(True)) or {}).get("workflow_call") or {})
    call_inputs = call.get("inputs") or {}
    check("ci-image.yml: takes the fork flag and base sha from its caller", sorted(call_inputs), ["base_sha", "fork"])
    check("ci-image.yml: the fork flag is explicit (no default that could publish)",
          (call_inputs.get("fork") or {}).get("required"), True)
    steps = ((image_doc.get("jobs") or {}).get("resolve") or {}).get("steps") or []
    fork_step = next((s for s in steps if s.get("id") == "fork"), {})
    check("ci-image.yml: publish capability follows the caller's fork flag, not the event",
          "INPUT_FORK" in str(fork_step.get("run", "")) and "INPUT_FORK" in (fork_step.get("env") or {}), True)
    check("ci-image.yml: publish capability does not read pull_request fields",
          "github.event.pull_request" in str(fork_step.get("run", "")), False)
    fallback = next((s for s in steps if s.get("id") == "fallback"), {})
    check("ci-image.yml: the fork fallback uses the admitted base",
          "github.event.pull_request" in str(fallback.get("run", "")), False)
    # The fork flag must refuse publishing: execute the real step.
    script = str(fork_step.get("run", ""))
    for fork, want in (("true", "can_publish=false"), ("false", "can_publish=true"), ("", "can_publish=false")):
        with tempfile.NamedTemporaryFile("r+", suffix=".out") as out:
            _verdict(script, {"INPUT_FORK": fork, "GITHUB_OUTPUT": out.name})
            check(f"ci-image.yml: fork={fork!r} -> {want}", out.read().strip(), want)


# ---------------------------------------------------------------------------

def main(argv: Sequence[str] | None = None) -> int:
    parser = argparse.ArgumentParser(description=__doc__, formatter_class=argparse.RawDescriptionHelpFormatter)
    parser.add_argument("--self-test", action="store_true")
    parser.add_argument("--workflow", default=str(CI_WORKFLOW), help="ci.yml to check (self-test)")
    parser.add_argument("--image-workflow", default=str(CI_IMAGE_WORKFLOW), help="ci-image.yml to check (self-test)")
    sub = parser.add_subparsers(dest="command")
    admit = sub.add_parser("admit", help="decide admission for this run")
    admit.add_argument("--repo", required=True)
    admit.add_argument("--event-name", required=True)
    admit.add_argument("--run-sha", required=True)
    admit.add_argument("--pr-number", default="")
    admit.add_argument("--sha", default="")
    admit.add_argument("--review-author", required=True)
    admit.add_argument("--repo-dir", default=".")
    admit.add_argument("--github-output", default="")
    admit.add_argument("--github-summary", default="")
    args = parser.parse_args(argv)
    if args.self_test:
        return _self_test(Path(args.workflow), Path(args.image_workflow))
    if args.command == "admit":
        return _admit_main(args)
    parser.print_usage(sys.stderr)
    return 2


if __name__ == "__main__":
    raise SystemExit(main())
