#!/usr/bin/env python3
"""Decide whether a synchronize push invalidates `reviewed:approve` (#1679).

`.github/workflows/review-gate.yml`'s `dismiss-stale-approval` job keeps a
prior approval alive across a push that changes only the branch's
ANCESTRY -- a base-branch update merged forward -- and strips it for a
push that changes the PR's own CONTENT. PR #842 established that goal.
It decided it, however, by intersecting the paths the push touched with
the PR's file list read AFTER the push, and a file reverted to its base
content is absent from that list by definition. Reverting an approved
file therefore DELETED the evidence that it had been approved:

    approved revision R1 modifies A and B
    R2 fully reverts A, leaving B
    pushed paths = {A}; post-push PR files = {B}; overlap = {} -> KEEP

which carried an approval onto a materially different patch. Reverting
the PR's LAST change was worse still: the post-push file list was empty,
so nothing could ever intersect it.

The repair is to stop reasoning about file NAMES and compare the PR's own
PATCH on each side of the push:

    patch(X) = git diff --raw <merge-base(base, X)> X

`patch(BEFORE) == patch(AFTER)` is exactly "the reviewed content did not
change"; anything else is a content change, whether it added, edited,
deleted or REVERTED. A merge-forward that touches only paths this PR does
not own leaves both the source and the destination blob of every
PR-owned path alone, so the two raw diffs are byte-identical and the
approval survives -- #842's goal, now reached by a rule a revert cannot
erase.

Why the raw diff, and why these flags
-------------------------------------
`--raw` names, per changed path, the source blob, the destination blob,
both file modes and the change status. Comparing that is comparing
CONTENT: two patches agree only when every path they touch starts and
ends at the same bytes. `--abbrev=40` pins full object names so an
abbreviation-length change -- `core.abbrev`, or git's own auto-sizing as
a repository grows -- can never read as a content change. `--no-renames`
pins the record SHAPE: `diff.renames` is on by default but is
user-configurable, so leaving it unpinned would let the same patch be
reported as `R100 old new` on one machine and `D old` + `A new` on
another. Records are compared as an UNORDERED SET, because `git diff`
orders by path within one invocation but the two invocations need not
enumerate the same paths.

Fail-closed, and observably so
------------------------------
Every predicate below selects STRIP unless it can positively prove the
push was content-free, preserving the original job's rule that staleness
which cannot be ruled out is treated as real. Each returns its OWN reason
code rather than falling through to a neighbour's: that is what makes
requirement 8 checkable, since bypassing any single rule then changes the
reason a case reports even when a later rule would reach the same
verdict.

The decision needs no GitHub query at all -- the old `gh pr diff` call
existed only to read the post-push file list this module no longer uses --
so there is no PR-query failure branch left to preserve. That is
deliberate and is issue #1679's approved amendment to requirement 5.

This module still decides only; applying and VERIFYING the decision is
`tools/review_gate_label_policy.py`'s job (#2184). That separation is
unchanged, but the guarantee on the other side of it is not: the label
removal is no longer best effort. The workflow reads the label state back
from GitHub after the attempt, fails `dismiss-stale-approval` when a strip
cannot be shown to have taken effect, and gates the required
`review-approved` check on this decision rather than on the event
payload. So a STRIP exit code is now enforced end to end, not advisory.

Exit codes: 0 = KEEP the label, non-zero = STRIP it. A crash, a bad
argument, or a missing interpreter is therefore a STRIP too, which is the
correct direction for every one of them.

Usage:
  python3 tools/review_gate_decision.py --before SHA --after SHA \
      --base-ref master --base-sha SHA [--repo-dir .]
  python3 tools/review_gate_decision.py --self-test
  python3 tools/review_gate_decision.py --mutation-test
"""

from __future__ import annotations

import argparse
import os
import subprocess
import sys
import tempfile
from pathlib import Path
from typing import NamedTuple

#: The all-zero object name a push event uses for "no such commit".
NULL_SHA = "0" * 40

KEEP = "keep"
STRIP = "strip"

#: The frozen inspection command for one side's patch. Every term is
#: load-bearing and the self-test pins the tuple:
#:   --raw        content, not just names: both blobs, both modes, status
#:   --no-renames determinism -- `diff.renames` is on by default and is
#:                user-configurable, so leaving it unpinned makes the
#:                record SHAPE (`R100 old new` vs `D old` + `A new`)
#:                depend on the machine the job happens to run on
#:   --abbrev=40  full object names, so an abbreviation-length change
#:                (core.abbrev, or git's own auto-sizing as a repository
#:                grows) can never read as a content change
RAW_DIFF_FLAGS = ("diff", "--raw", "--no-renames", "--abbrev=40")


class Decision(NamedTuple):
    """One verdict, the rule that produced it, and a human-readable why."""

    verdict: str
    reason: str
    detail: str

    @property
    def keeps_label(self) -> bool:
        return self.verdict == KEEP


class Git:
    """The single seam every git invocation in this module goes through.

    `run` never raises: it answers (ok, text), so each caller decides what
    a failure means instead of an exception unwinding past a fail-closed
    branch. The self-test subclasses this to fail one specific invocation,
    which is how the branches for a git command that cannot be made to
    fail on a well-formed repository are exercised against the real
    decision code rather than a copy of it.
    """

    def __init__(self, repo_dir: str | os.PathLike[str]) -> None:
        self.repo_dir = str(repo_dir)

    def run(self, *args: str) -> tuple[bool, str]:
        try:
            completed = subprocess.run(
                ("git", "-C", self.repo_dir, *args),
                capture_output=True,
                text=True,
                check=False,
            )
        except OSError as error:  # git absent, repo_dir gone, ...
            return False, str(error)
        if completed.returncode != 0:
            return False, (completed.stderr or "").strip()
        return True, completed.stdout


def _resolve_commit(git: Git, revision: str) -> str:
    """The full object name `revision` names, or "" if it names none."""
    ok, text = git.run("rev-parse", "--verify", "--quiet", f"{revision}^{{commit}}")
    if not ok:
        return ""
    return text.strip()


def base_candidates(base_ref: str, base_sha: str) -> list[str]:
    """The base-branch revisions to try, most trustworthy first.

    `origin/<ref>` leads because it is the base branch as this checkout
    fetched it, which is at or ahead of whatever a merge-forward brought
    in. The event's `base.sha` follows: it is authoritative when present
    but can name an older tip than the branch actually reached, and a
    base that lags the merged-in commit would make the merge's own
    changes look like this PR's. A bare `<ref>` is last: in a detached
    CI checkout it usually does not exist at all, and where it does it is
    the likeliest of the three to be stale.
    """
    candidates = [
        f"origin/{base_ref}" if base_ref else "",
        base_sha or "",
        base_ref or "",
    ]
    ordered: list[str] = []
    for candidate in candidates:
        candidate = candidate.strip()
        if candidate and candidate not in ordered:
            ordered.append(candidate)
    return ordered


def patch_identity(raw_diff: str) -> frozenset[str]:
    """The content identity of a patch, from `git diff --raw` output.

    Each retained record already carries both blob names, both modes and
    the status, so equality of these sets is equality of the patch's
    effect. Order is discarded deliberately (see the module docstring).
    """
    return frozenset(line for line in raw_diff.splitlines() if line.strip())


def _patch_summary(before: frozenset[str], after: frozenset[str]) -> str:
    """Name a few paths that differ, so the CI log says what moved."""
    paths: list[str] = []
    for record in sorted(before ^ after):
        _, _, path = record.partition("\t")
        path = path.strip() or record
        if path not in paths:
            paths.append(path)
    shown = ", ".join(paths[:5])
    if len(paths) > 5:
        shown += f", ... (+{len(paths) - 5} more)"
    return shown


def decide(git: Git, before: str, after: str, base_ref: str, base_sha: str) -> Decision:
    """KEEP only for a push that provably changed no PR-owned content."""
    before = (before or "").strip()
    after = (after or "").strip()

    if not before:
        return Decision(STRIP, "before-missing",
                        "no 'before' SHA on the event -- staleness cannot be ruled out")
    if before == NULL_SHA:
        return Decision(STRIP, "before-null",
                        "'before' is the all-zero SHA -- staleness cannot be ruled out")
    resolved_before = _resolve_commit(git, before)
    if not resolved_before:
        return Decision(STRIP, "before-unreachable",
                        f"'before' {before} is not a commit in this checkout")

    if not after:
        return Decision(STRIP, "after-missing",
                        "no 'after' SHA on the event -- staleness cannot be ruled out")
    if after == NULL_SHA:
        return Decision(STRIP, "after-null",
                        "'after' is the all-zero SHA -- staleness cannot be ruled out")
    resolved_after = _resolve_commit(git, after)
    if not resolved_after:
        return Decision(STRIP, "after-unreachable",
                        f"'after' {after} is not a commit in this checkout")

    ok, pushed = git.run("diff", "--name-only", resolved_before, resolved_after)
    if not ok:
        return Decision(STRIP, "push-diff-failed",
                        f"could not diff 'before'..'after': {pushed}")
    if not pushed.strip():
        # Historical guard, preserved verbatim in spirit: a synchronize
        # push that changed no file at all is not a shape this job
        # understands, so it is not a shape it will vouch for.
        return Decision(STRIP, "push-empty",
                        "push introduced no file changes (unexpected -- being safe)")

    base = ""
    tried = base_candidates(base_ref, base_sha)
    for candidate in tried:
        base = _resolve_commit(git, candidate)
        if base:
            break
    if not base:
        return Decision(STRIP, "base-unresolvable",
                        "no base-branch revision resolved from: "
                        + (", ".join(tried) if tried else "(nothing supplied)"))

    ok, merge_base_before = git.run("merge-base", base, resolved_before)
    merge_base_before = merge_base_before.strip() if ok else ""
    if not merge_base_before:
        return Decision(STRIP, "before-merge-base-failed",
                        "no merge base between the base branch and 'before'")
    ok, merge_base_after = git.run("merge-base", base, resolved_after)
    merge_base_after = merge_base_after.strip() if ok else ""
    if not merge_base_after:
        return Decision(STRIP, "after-merge-base-failed",
                        "no merge base between the base branch and 'after'")

    ok, raw_before = git.run(*RAW_DIFF_FLAGS, merge_base_before, resolved_before)
    if not ok:
        return Decision(STRIP, "before-patch-failed",
                        f"could not read the PR's patch before the push: {raw_before}")
    ok, raw_after = git.run(*RAW_DIFF_FLAGS, merge_base_after, resolved_after)
    if not ok:
        return Decision(STRIP, "after-patch-failed",
                        f"could not read the PR's patch after the push: {raw_after}")

    patch_before = patch_identity(raw_before)
    patch_after = patch_identity(raw_after)
    if patch_before != patch_after:
        return Decision(STRIP, "patch-changed",
                        "this PR's own patch changed: " + _patch_summary(patch_before, patch_after))
    return Decision(KEEP, "ancestry-only",
                    "this PR's own patch is unchanged -- only the branch's ancestry moved")


# ---------------------------------------------------------------------------
# Self-test
# ---------------------------------------------------------------------------

class _FaultingGit(Git):
    """A `Git` that fails one chosen invocation and runs the rest for real.

    Some fail-closed branches guard a git command that a well-formed
    repository gives no way to make fail -- `git diff` between two commits
    it already resolved, for instance. Those branches are still policy, so
    they are exercised by faulting the invocation, not by re-implementing
    the decision. Everything above this line is the code the workflow runs.
    """

    def __init__(self, repo_dir, prefix: tuple[str, ...], occurrence: int = 1) -> None:
        super().__init__(repo_dir)
        self._prefix = prefix
        self._occurrence = occurrence
        self._seen = 0

    def run(self, *args: str) -> tuple[bool, str]:
        if args[: len(self._prefix)] == self._prefix:
            self._seen += 1
            if self._seen == self._occurrence:
                return False, "injected git failure"
        return super().run(*args)


class _Repo:
    """A throwaway git repository built commit by commit."""

    def __init__(self, path: Path) -> None:
        self.path = path
        self._git("init", "--quiet", "-b", "master", ".")
        self._git("config", "user.email", "gate@example.invalid")
        self._git("config", "user.name", "review gate self-test")
        self._git("config", "commit.gpgsign", "false")

    def _git(self, *args: str) -> str:
        completed = subprocess.run(
            ("git", "-C", str(self.path), *args),
            capture_output=True, text=True, check=True)
        return completed.stdout.strip()

    def write(self, name: str, text: str) -> None:
        (self.path / name).write_text(text, encoding="utf-8")

    def remove(self, name: str) -> None:
        (self.path / name).unlink()

    def commit(self, message: str, allow_empty: bool = False) -> str:
        self._git("add", "-A")
        args = ["commit", "--quiet", "-m", message]
        if allow_empty:
            args.append("--allow-empty")
        self._git(*args)
        return self._git("rev-parse", "HEAD")

    def checkout(self, ref: str, create: bool = False) -> None:
        self._git("checkout", "--quiet", *(("-b",) if create else ()), ref)

    def merge(self, ref: str, message: str) -> str:
        self._git("merge", "--quiet", "--no-ff", "-m", message, ref)
        return self._git("rev-parse", "HEAD")

    def set_remote_ref(self, name: str, sha: str) -> None:
        self._git("update-ref", f"refs/remotes/origin/{name}", sha)

    def head(self) -> str:
        return self._git("rev-parse", "HEAD")


#: `b.txt` is deliberately multi-line so a base advance and a PR edit can
#: touch the SAME file in regions git merges without a conflict -- the
#: shape that separates "the base moved under my patch" from "the base
#: moved elsewhere".
_B_LINES = [f"b{index}\n" for index in range(1, 10)]


def _base_world(repo: _Repo) -> str:
    """M0: two owned files plus one the PR never touches."""
    repo.write("a.txt", "a base\n")
    repo.write("b.txt", "".join(_B_LINES))
    repo.write("untouched.txt", "untouched base\n")
    return repo.commit("M0")


def _b_with(index: int, text: str) -> str:
    """`b.txt` with one line replaced, leaving every other line alone."""
    lines = list(_B_LINES)
    lines[index] = text
    return "".join(lines)


def _self_test() -> int:  # noqa: C901 - a flat list of cases reads best flat
    failures: list[str] = []

    def check(label: str, decision: Decision, verdict: str, reason: str) -> None:
        if (decision.verdict, decision.reason) != (verdict, reason):
            failures.append(
                f"{label}: expected {verdict}/{reason}, got "
                f"{decision.verdict}/{decision.reason} ({decision.detail})")

    with tempfile.TemporaryDirectory(prefix="review-gate-selftest-") as tmp:
        root = Path(tmp)

        def new_repo(name: str) -> _Repo:
            path = root / name
            path.mkdir()
            return _Repo(path)

        # ---- requirement 1: a full revert of ONE approved file strips ----
        repo = new_repo("partial-revert")
        m0 = _base_world(repo)
        repo.checkout("pr", create=True)
        repo.write("a.txt", "a from the PR\n")
        repo.write("b.txt", _b_with(0, "b1 from the PR\n"))
        before = repo.commit("R1: modify A and B")
        repo.write("a.txt", "a base\n")          # A reverted to base content
        after = repo.commit("R2: revert A, keep B")
        repo.set_remote_ref("master", m0)
        check("requirement 1 (one approved file fully reverted)",
              decide(Git(repo.path), before, after, "master", m0), STRIP, "patch-changed")

        # ---- requirement 2: reverting the LAST remaining change strips ----
        repo = new_repo("full-revert")
        m0 = _base_world(repo)
        repo.checkout("pr", create=True)
        repo.write("a.txt", "a from the PR\n")
        before = repo.commit("R1: modify A")
        repo.write("a.txt", "a base\n")
        after = repo.commit("R2: revert A -- the PR now has an empty patch")
        repo.set_remote_ref("master", m0)
        check("requirement 2 (PR's last change reverted, empty patch)",
              decide(Git(repo.path), before, after, "master", m0), STRIP, "patch-changed")

        # ---- requirement 3: add / edit / delete in the PR's own patch ----
        repo = new_repo("patch-edit")
        m0 = _base_world(repo)
        repo.checkout("pr", create=True)
        repo.write("a.txt", "a from the PR\n")
        before = repo.commit("R1: modify A")
        repo.write("a.txt", "a from the PR, revised\n")
        after = repo.commit("R2: edit A further")
        repo.set_remote_ref("master", m0)
        check("requirement 3a (an ordinary edit)",
              decide(Git(repo.path), before, after, "master", m0), STRIP, "patch-changed")

        repo = new_repo("patch-add")
        m0 = _base_world(repo)
        repo.checkout("pr", create=True)
        repo.write("a.txt", "a from the PR\n")
        before = repo.commit("R1: modify A")
        repo.write("added.txt", "a brand new file\n")
        after = repo.commit("R2: add a file to the patch")
        repo.set_remote_ref("master", m0)
        check("requirement 3b (an addition)",
              decide(Git(repo.path), before, after, "master", m0), STRIP, "patch-changed")

        repo = new_repo("patch-delete")
        m0 = _base_world(repo)
        repo.checkout("pr", create=True)
        repo.write("a.txt", "a from the PR\n")
        before = repo.commit("R1: modify A")
        repo.remove("b.txt")
        after = repo.commit("R2: delete a base file")
        repo.set_remote_ref("master", m0)
        check("requirement 3c (a deletion)",
              decide(Git(repo.path), before, after, "master", m0), STRIP, "patch-changed")

        repo = new_repo("patch-add-then-delete")
        m0 = _base_world(repo)
        repo.checkout("pr", create=True)
        repo.write("a.txt", "a from the PR\n")
        repo.write("added.txt", "added by the PR\n")
        before = repo.commit("R1: modify A and add a file")
        repo.remove("added.txt")
        after = repo.commit("R2: delete the file the PR itself added")
        repo.set_remote_ref("master", m0)
        check("requirement 3d (deleting a file the patch itself added)",
              decide(Git(repo.path), before, after, "master", m0), STRIP, "patch-changed")

        # ---- requirement 4: a real merge-forward commit graph KEEPS ----
        # The base advances on a path this PR does not own and is merged
        # into the branch. `master` is deliberately left behind at M0 so
        # that only the `origin/master` candidate names the advanced base:
        # a decision that consulted the stale local ref would see the
        # merge's own changes as this PR's and strip.
        repo = new_repo("merge-forward")
        m0 = _base_world(repo)
        repo.checkout("pr", create=True)
        repo.write("a.txt", "a from the PR\n")
        before = repo.commit("R1: modify A")
        repo._git("checkout", "--quiet", "-B", "upstream", m0)
        repo.write("untouched.txt", "untouched, advanced on master\n")
        m1 = repo.commit("M1: base advances on an unrelated path")
        repo.checkout("pr")
        after = repo.merge("upstream", "Merge master into the PR branch")
        repo.set_remote_ref("master", m1)
        check("requirement 4 (merge-forward on an unrelated path)",
              decide(Git(repo.path), before, after, "master", m1), KEEP, "ancestry-only")
        # ... and the same graph judged with only the event's base.sha,
        # proving the fallback candidate reaches the same answer.
        check("requirement 4 (base resolved from base.sha alone)",
              decide(Git(repo.path), before, after, "", m1), KEEP, "ancestry-only")

        # A rebase onto the advanced base, patch unchanged, keeps too.
        repo = new_repo("rebase-forward")
        m0 = _base_world(repo)
        repo.checkout("pr", create=True)
        repo.write("a.txt", "a from the PR\n")
        before = repo.commit("R1: modify A")
        repo._git("checkout", "--quiet", "-B", "upstream", m0)
        repo.write("untouched.txt", "untouched, advanced on master\n")
        m1 = repo.commit("M1: base advances on an unrelated path")
        repo.checkout("pr")
        repo._git("rebase", "--quiet", "upstream")
        after = repo.head()
        repo.set_remote_ref("master", m1)
        check("requirement 4 (rebase onto the advanced base)",
              decide(Git(repo.path), before, after, "master", m1), KEEP, "ancestry-only")

        # A rename-shaped patch survives a merge-forward too. This is the
        # case RAW_DIFF_FLAGS' --no-renames exists for: with rename
        # detection left to `diff.renames`, the record shape for this
        # patch is whatever the running machine is configured for, and
        # the two sides must agree regardless.
        repo = new_repo("rename-merge-forward")
        m0 = _base_world(repo)
        repo.checkout("pr", create=True)
        repo._git("mv", "a.txt", "renamed.txt")
        before = repo.commit("R1: rename A")
        repo._git("checkout", "--quiet", "-B", "upstream", m0)
        repo.write("untouched.txt", "untouched, advanced on master\n")
        m1 = repo.commit("M1: base advances on an unrelated path")
        repo.checkout("pr")
        after = repo.merge("upstream", "Merge master into the PR branch")
        repo.set_remote_ref("master", m1)
        check("a renamed file survives a merge-forward",
              decide(Git(repo.path), before, after, "master", m1), KEEP, "ancestry-only")

        # A merge-forward that also touches a PR-owned path is NOT
        # ancestry-only: the reviewed lines now sit on different base
        # content, so it strips.
        repo = new_repo("merge-forward-overlapping")
        m0 = _base_world(repo)
        repo.checkout("pr", create=True)
        repo.write("a.txt", "a from the PR\n")
        before = repo.commit("R1: modify A")
        repo._git("checkout", "--quiet", "-B", "upstream", m0)
        repo.write("b.txt", _b_with(8, "b9 advanced on master\n"))
        m1 = repo.commit("M1: base advances on a path the PR also owns")
        repo.checkout("pr")
        repo.write("b.txt", _b_with(0, "b1 from the PR\n"))
        before = repo.commit("R1b: the PR owns B too")
        after = repo.merge("upstream", "Merge master into the PR branch")
        repo.set_remote_ref("master", m1)
        check("a merge-forward onto a PR-owned path is not ancestry-only",
              decide(Git(repo.path), before, after, "master", m1), STRIP, "patch-changed")

        # ---- requirement 5: every fail-closed predicate selects STRIP ----
        repo = new_repo("fail-closed")
        m0 = _base_world(repo)
        repo.checkout("pr", create=True)
        repo.write("a.txt", "a from the PR\n")
        before = repo.commit("R1: modify A")
        repo.write("a.txt", "a from the PR, revised\n")
        after = repo.commit("R2: edit A further")
        repo.set_remote_ref("master", m0)
        git = Git(repo.path)
        missing = "1" * 40  # well formed, and no such object

        check("fail-closed: BEFORE missing",
              decide(git, "", after, "master", m0), STRIP, "before-missing")
        check("fail-closed: BEFORE all-zero",
              decide(git, NULL_SHA, after, "master", m0), STRIP, "before-null")
        check("fail-closed: BEFORE unreachable",
              decide(git, missing, after, "master", m0), STRIP, "before-unreachable")
        check("fail-closed: AFTER missing",
              decide(git, before, "", "master", m0), STRIP, "after-missing")
        check("fail-closed: AFTER all-zero",
              decide(git, before, NULL_SHA, "master", m0), STRIP, "after-null")
        check("fail-closed: AFTER unreachable",
              decide(git, before, missing, "master", m0), STRIP, "after-unreachable")
        check("fail-closed: the push diff fails",
              decide(_FaultingGit(repo.path, ("diff", "--name-only")),
                     before, after, "master", m0), STRIP, "push-diff-failed")
        check("fail-closed: the first patch inspection fails",
              decide(_FaultingGit(repo.path, ("diff", "--raw"), occurrence=1),
                     before, after, "master", m0), STRIP, "before-patch-failed")
        check("fail-closed: the second patch inspection fails",
              decide(_FaultingGit(repo.path, ("diff", "--raw"), occurrence=2),
                     before, after, "master", m0), STRIP, "after-patch-failed")
        check("fail-closed: no base revision resolves",
              decide(git, before, after, "no-such-branch", ""), STRIP, "base-unresolvable")

        # An empty push diff: two commits with identical trees.
        repo = new_repo("empty-push")
        m0 = _base_world(repo)
        repo.checkout("pr", create=True)
        repo.write("a.txt", "a from the PR\n")
        before = repo.commit("R1: modify A")
        after = repo.commit("R2: an empty commit", allow_empty=True)
        repo.set_remote_ref("master", m0)
        check("fail-closed: the push changed no file at all",
              decide(Git(repo.path), before, after, "master", m0), STRIP, "push-empty")

        # Unrelated histories: `git merge-base` genuinely fails.
        repo = new_repo("unrelated-before")
        m0 = _base_world(repo)
        repo._git("checkout", "--quiet", "--orphan", "orphan")
        repo._git("rm", "-rq", "--cached", ".")
        repo.write("orphan.txt", "an unrelated root\n")
        orphan_one = repo.commit("O1: an unrelated root commit")
        repo.write("orphan.txt", "an unrelated root, revised\n")
        orphan_two = repo.commit("O2")
        repo.set_remote_ref("master", m0)
        check("fail-closed: no merge base with 'before'",
              decide(Git(repo.path), orphan_one, orphan_two, "master", m0),
              STRIP, "before-merge-base-failed")

        repo = new_repo("unrelated-after")
        m0 = _base_world(repo)
        repo.checkout("pr", create=True)
        repo.write("a.txt", "a from the PR\n")
        before = repo.commit("R1: modify A")
        repo._git("checkout", "--quiet", "--orphan", "orphan")
        repo._git("rm", "-rq", "--cached", ".")
        repo.write("orphan.txt", "an unrelated root\n")
        orphan = repo.commit("O1: an unrelated root commit")
        repo.set_remote_ref("master", m0)
        check("fail-closed: no merge base with 'after'",
              decide(Git(repo.path), before, orphan, "master", m0),
              STRIP, "after-merge-base-failed")

        # ---- the base-candidate ladder ----
        got = base_candidates("master", "abc123")
        if got != ["origin/master", "abc123", "master"]:
            failures.append(f"base_candidates order changed: {got!r}")
        if base_candidates("", "") != []:
            failures.append("base_candidates should be empty when nothing is supplied")
        if base_candidates("master", "master") != ["origin/master", "master"]:
            failures.append("base_candidates should not repeat a candidate")

        # ---- the frozen inspection command ----
        if RAW_DIFF_FLAGS != ("diff", "--raw", "--no-renames", "--abbrev=40"):
            failures.append(
                "RAW_DIFF_FLAGS changed: --raw compares content rather than "
                "names, --no-renames pins the record shape against a "
                "machine's diff.renames setting, and --abbrev=40 pins full "
                f"object names. Got {RAW_DIFF_FLAGS!r}")

        # ---- patch_identity is order-insensitive and blank-tolerant ----
        if patch_identity("x\ny\n") != patch_identity("y\nx\n"):
            failures.append("patch_identity must not depend on record order")
        if patch_identity("x\n\n") != patch_identity("x\n"):
            failures.append("patch_identity must ignore blank records")
        if patch_identity("") != frozenset():
            failures.append("an empty patch must be the empty identity")

    for failure in failures:
        print(f"  FAIL: {failure}")
    if failures:
        print(f"\n{len(failures)} review_gate_decision self-test case(s) failed")
        return 1
    print("review_gate_decision self-test: all cases pass")
    return 0


# ---------------------------------------------------------------------------
# Mutation test (issue #1679 requirement 8)
# ---------------------------------------------------------------------------
#
# A self-test proves the decision agrees with the cases it was written
# beside; it does not prove the cases would NOTICE the decision changing.
# `--mutation-test` proves the second thing directly: it copies this file,
# defeats one rule in the copy, and requires `--self-test` on the copy to
# fail. Every KEEP/STRIP outcome and every fail-closed predicate is
# covered, which is what makes the reason codes worth carrying -- several
# predicates share a verdict with a later one, so bypassing them would be
# invisible if only the verdict were asserted.
#
# It is run by hand as issue #1679's acceptance evidence and is not part
# of the gate set: it re-runs the whole self-test once per mutation, and
# a mutation whose anchor stops matching is an error naming the anchor,
# never a quietly smaller run.


def _bypass_guard(source: str, reason: str) -> str:
    """Defeat the `if` that selects `reason`, leaving the rest intact."""
    needle = f'return Decision(STRIP, "{reason}"'
    lines = source.splitlines(keepends=True)
    index = next((n for n, line in enumerate(lines) if needle in line), None)
    if index is None:
        raise SystemExit(f"mutation anchor not found: {needle}")
    for cursor in range(index - 1, -1, -1):
        stripped = lines[cursor].strip()
        if stripped.startswith("if ") and stripped.endswith(":"):
            indent = " " * (len(lines[cursor]) - len(lines[cursor].lstrip()))
            lines[cursor] = f"{indent}if False:\n"
            return "".join(lines)
    raise SystemExit(f"no guard found above: {needle}")


def _replace_once(source: str, old: str, new: str) -> str:
    if source.count(old) != 1:
        raise SystemExit(
            f"mutation anchor matched {source.count(old)} times (want 1): {old!r}")
    return source.replace(old, new, 1)


#: (label, mutation). Each must make `--self-test` fail.
_MUTATIONS: tuple[tuple[str, object], ...] = (
    ("bypass the missing-BEFORE guard", lambda t: _bypass_guard(t, "before-missing")),
    ("bypass the all-zero-BEFORE guard", lambda t: _bypass_guard(t, "before-null")),
    ("bypass the unreachable-BEFORE guard", lambda t: _bypass_guard(t, "before-unreachable")),
    ("bypass the missing-AFTER guard", lambda t: _bypass_guard(t, "after-missing")),
    ("bypass the all-zero-AFTER guard", lambda t: _bypass_guard(t, "after-null")),
    ("bypass the unreachable-AFTER guard", lambda t: _bypass_guard(t, "after-unreachable")),
    ("bypass the failed-push-diff guard", lambda t: _bypass_guard(t, "push-diff-failed")),
    ("bypass the empty-push guard", lambda t: _bypass_guard(t, "push-empty")),
    ("bypass the unresolvable-base guard", lambda t: _bypass_guard(t, "base-unresolvable")),
    ("bypass the BEFORE merge-base guard",
     lambda t: _bypass_guard(t, "before-merge-base-failed")),
    ("bypass the AFTER merge-base guard",
     lambda t: _bypass_guard(t, "after-merge-base-failed")),
    ("bypass the BEFORE patch-inspection guard",
     lambda t: _bypass_guard(t, "before-patch-failed")),
    ("bypass the AFTER patch-inspection guard",
     lambda t: _bypass_guard(t, "after-patch-failed")),
    ("never strip on a changed patch", lambda t: _bypass_guard(t, "patch-changed")),
    ("always strip, never keep",
     lambda t: _replace_once(t, 'return Decision(KEEP, "ancestry-only",',
                             'return Decision(STRIP, "ancestry-only",')),
    ("invert the patch comparison",
     lambda t: _replace_once(t, "if patch_before != patch_after:",
                             "if patch_before == patch_after:")),
    ("compare file NAMES instead of content (the #1679 defect itself)",
     lambda t: _replace_once(
         t, "    return frozenset(line for line in raw_diff.splitlines() if line.strip())",
         "    return frozenset(line.partition(chr(9))[2].strip()\n"
         "                     for line in raw_diff.splitlines() if line.strip())")),
    ("drop --no-renames from the frozen inspection command",
     lambda t: _replace_once(t, 'RAW_DIFF_FLAGS = ("diff", "--raw", "--no-renames", "--abbrev=40")',
                             'RAW_DIFF_FLAGS = ("diff", "--raw", "--abbrev=40")')),
    ("drop --abbrev=40 from the frozen inspection command",
     lambda t: _replace_once(t, 'RAW_DIFF_FLAGS = ("diff", "--raw", "--no-renames", "--abbrev=40")',
                             'RAW_DIFF_FLAGS = ("diff", "--raw", "--no-renames")')),
    ("prefer a stale local base ref over origin/<ref>",
     lambda t: _replace_once(t, '        f"origin/{base_ref}" if base_ref else "",\n'
                                '        base_sha or "",\n'
                                '        base_ref or "",',
                             '        base_ref or "",\n'
                                '        f"origin/{base_ref}" if base_ref else "",\n'
                                '        base_sha or "",')),
    ("ignore the event's base.sha fallback",
     lambda t: _replace_once(t, '        base_sha or "",\n', "")),
    ("let record ORDER decide patch identity",
     lambda t: _replace_once(
         t, "    return frozenset(line for line in raw_diff.splitlines() if line.strip())",
         "    return tuple(line for line in raw_diff.splitlines() if line.strip())")),
)


#: Everything from this line down is the harness, not the decision. It is
#: split off before a mutation is applied and reattached afterwards, so an
#: anchor can never match the table that names it.
_HARNESS_MARKER = "# Mutation test (issue #1679 requirement 8)"


def _mutation_test() -> int:
    whole = Path(__file__).resolve().read_text(encoding="utf-8")
    head, marker, tail = whole.partition(_HARNESS_MARKER)
    if not marker:
        raise SystemExit(f"harness marker not found: {_HARNESS_MARKER}")
    survivors: list[str] = []
    with tempfile.TemporaryDirectory(prefix="review-gate-mutation-") as tmp:
        copy = Path(tmp) / "review_gate_decision.py"

        def run_self_test() -> int:
            return subprocess.run(
                (sys.executable, str(copy), "--self-test"),
                capture_output=True, text=True, check=False).returncode

        # The control: an unmutated copy must PASS, or every "mutation
        # killed" below would be reporting the copy being broken.
        copy.write_text(whole, encoding="utf-8")
        if run_self_test() != 0:
            print("  FAIL: the unmutated copy does not pass its own self-test")
            return 1
        print("  control (unmutated copy) ....... passes")

        for label, mutate in _MUTATIONS:
            copy.write_text(mutate(head) + marker + tail,  # type: ignore[operator]
                            encoding="utf-8")
            if run_self_test() == 0:
                survivors.append(label)
                print(f"  SURVIVED: {label}")
            else:
                print(f"  killed ......................... {label}")

    if survivors:
        print(f"\n{len(survivors)} mutation(s) survived the self-test:")
        for label in survivors:
            print(f"  - {label}")
        return 1
    print(f"\nreview_gate_decision mutation test: all {len(_MUTATIONS)} "
          "mutations killed")
    return 0


def main() -> int:
    parser = argparse.ArgumentParser(
        description="Decide whether a synchronize push invalidates a prior "
                    "reviewed:approve (#1679). Exit 0 = keep the label, "
                    "non-zero = strip it.")
    parser.add_argument("--before", default="",
                        help="github.event.before")
    parser.add_argument("--after", default="",
                        help="github.event.after")
    parser.add_argument("--base-ref", default="",
                        help="github.event.pull_request.base.ref")
    parser.add_argument("--base-sha", default="",
                        help="github.event.pull_request.base.sha")
    parser.add_argument("--repo-dir", default=".",
                        help="the checkout to inspect (default: cwd)")
    parser.add_argument("--self-test", action="store_true")
    parser.add_argument(
        "--mutation-test", action="store_true",
        help="prove every rule in the decision is covered: defeat each one "
             "in a copy of this file and require --self-test to fail "
             "(issue #1679 requirement 8)")
    args = parser.parse_args()

    if args.self_test:
        return _self_test()
    if args.mutation_test:
        return _mutation_test()

    decision = decide(Git(args.repo_dir), args.before, args.after,
                      args.base_ref, args.base_sha)
    print(f"{decision.verdict.upper()} [{decision.reason}] {decision.detail}")
    return 0 if decision.keeps_label else 1


if __name__ == "__main__":
    raise SystemExit(main())
