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

#1679 repaired that by comparing the PR's own PATCH on each side, as
`git diff --raw <merge-base(base, X)> X`. That fixed the revert, but the
raw record names the SOURCE blob as well as the destination, so moving
the merge base changed the record for every path the base had touched.
A clean, purely additive merge-forward into a file the PR also owns
therefore read as a content change (#2591):

    approved R1 adds a line to a shared manifest
    the base adds a DIFFERENT line to the same manifest and merges in
    source blob moved, destination blob moved -> "patch changed" -> STRIP

Nothing of the PR's own had changed. Two sibling pull requests that both
register a module in one manifest strip each other's approval every time
one of them merges first, and each then pays for a full opposite-brand
rereview of an unchanged patch.

The rule this module applies instead asks the question the gate actually
means, and asks it once:

    replay = merge(BEFORE, base-revision-incorporated-into-AFTER)
    KEEP iff that merge is conflict-free and replay's tree == AFTER's tree

"AFTER is nothing more than BEFORE carried onto the new base" is exactly
what an approval may survive. It is insensitive to where the base moved,
because the base's own changes are on BOTH sides of the comparison; and
it still catches an edit, an addition, a deletion and a REVERT, because
none of those can be reproduced by replaying the approved head.

How the replay runs, and which base
-----------------------------------
`git merge-tree --write-tree` would do it in one call, but it needs git
2.38 and this project's CI image is Ubuntu 22.04, whose git is 2.34 --
so the decision would strip every pull request there while passing on a
developer's newer machine. `_replay` uses git's oldest stable plumbing
instead, which behaves identically on both: `read-tree -i -m` for the
three-way INDEX merge, then `merge-index -o git-merge-one-file -a` to
run git's own content merge over whatever that left unmerged (the step
that makes two additive edits to one file merge rather than conflict --
the whole point of #2591), then `write-tree`.

The index and work tree are redirected into a temporary directory that
is removed on the way out, so nothing touches the CI checkout this job
shares with other steps. Comparing TREES compares content and file modes
together, over the whole repository, so a mode-only change is a mismatch
like any other.

The replay base is the base revision AFTER actually incorporated
(`merge-base(base, AFTER)`), never the base tip this checkout fetched.
`base_candidates` deliberately allows `origin/<ref>` to be AHEAD of what
the update merged in, so replaying onto the tip would pull in commits
AFTER never saw and read them as this PR's own work -- a sibling merging
between the branch update and this job would strip an approval it had
nothing to do with.

A conflicted replay is its own verdict, not a failure. It STRIPS -- a
resolution no reviewer saw is exactly what an approval must not carry --
and says which paths conflicted, even when AFTER itself contains a
perfectly good committed resolution.

Telling that from a failure takes care, because `merge-index` exits
non-zero for BOTH: a genuine content conflict, and an operational fault
such as a merge driver it could not run. The scratch INDEX is what
separates them and the only thing that can -- an unmerged entry exists
exactly when a path really conflicted -- so the paths come from
`ls-files -u`, and a non-zero exit that leaves nothing unmerged is a
`replay-failed`, never a conflict with no paths to name. The merge
driver's prose is not an interface and a non-empty stderr proves nothing
about which case occurred.

Fail-closed, and observably so
------------------------------
Every predicate below selects STRIP unless it can positively prove the
push was a clean replay of the approved head, preserving the original
job's rule that staleness which cannot be ruled out is treated as real.
A conflict, a replay whose tree differs from the pushed one, and any
failure to establish either are all strips. Each returns its OWN reason
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

#: The frozen replay commands. Every term is load-bearing and the
#: self-test pins both tuples.
#:
#: `git merge-tree --write-tree` would do this in one call, but it needs
#: git 2.38 and this project's CI image is Ubuntu 22.04, whose git is
#: 2.34 -- so the decision would strip every pull request there while
#: passing on a developer's newer machine. This recipe is git's own
#: three-way merge through its oldest stable plumbing, and it runs the
#: same on both:
#:   read-tree -i -m <base> <ours> <theirs>
#:                 the three-way INDEX merge, leaving whatever it cannot
#:                 resolve trivially at stages 1/2/3. `-i` because the
#:                 scratch work tree is empty and must not be consulted.
#:   merge-index -o git-merge-one-file -a
#:                 runs git's own content merge over exactly those
#:                 unmerged entries -- the step that makes two additive
#:                 edits to one file merge instead of conflicting, which
#:                 is the whole point of #2591. `-o` finishes every path
#:                 rather than stopping at the first failure, and a
#:                 non-zero exit means at least one truly conflicted.
READ_TREE_FLAGS = ("read-tree", "-i", "-m")
MERGE_INDEX_FLAGS = ("merge-index", "-o", "git-merge-one-file", "-a")


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
        code, out, err = self.capture(*args)
        if code != 0:
            return False, err.strip()
        return True, out

    def capture(self, *args: str, env: dict[str, str] | None = None
                ) -> tuple[int, str, str]:
        """(exit code, stdout, stderr) -- the whole result, unflattened.

        `run` collapses a non-zero exit to `False` and discards stdout,
        which is right for a command whose only useful answer is its
        output. `git merge-tree` is not such a command: it reports a
        CONFLICT as exit 1 while still writing the tree and the
        conflicted paths to stdout, so a caller that could only see the
        boolean could not tell a conflict from a crash, nor say which
        paths conflicted. Both callers go through one seam so the
        self-test's fault injection still reaches every invocation.
        """
        try:
            completed = subprocess.run(
                ("git", "-C", self.repo_dir, *args),
                capture_output=True,
                text=True,
                check=False,
                env={**os.environ, **env} if env else None,
            )
        except OSError as error:  # git absent, repo_dir gone, ...
            return -1, "", str(error)
        return completed.returncode, completed.stdout, completed.stderr or ""


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


def replay_tree(stdout: str) -> str:
    """The merged tree object name `git write-tree` printed.

    Anything else -- no output, or a line that is not an object name --
    is unreadable rather than a verdict, and the caller fails closed on
    it rather than comparing a string it does not understand.
    """
    first = stdout.strip().splitlines()[0].strip() if stdout.strip() else ""
    if len(first) == 40 and all(c in "0123456789abcdef" for c in first):
        return first
    return ""


def conflicted_paths(stdout: str) -> list[str]:
    """The paths still unmerged, from `git ls-files -u`, for the job log.

    One `<mode> <object> <stage>\t<path>` record per surviving stage, so
    a path appears up to three times. Reported de-duplicated, in
    first-seen order, and best-effort: a shape this cannot parse still
    leaves the decision a STRIP, only a less specific one.
    """
    paths: list[str] = []
    for line in stdout.splitlines():
        _, tab, path = line.partition("\t")
        path = path.strip()
        if tab and path and path not in paths:
            paths.append(path)
    return paths


def _paths_summary(paths: list[str]) -> str:
    """Name a few paths, so the CI log says what moved."""
    if not paths:
        return "(no paths reported)"
    shown = ", ".join(paths[:5])
    if len(paths) > 5:
        shown += f", ... (+{len(paths) - 5} more)"
    return shown


def _replay(git: Git, merge_base: str, ours: str, theirs: str
            ) -> tuple[str, str, str]:
    """Merge `ours` and `theirs` over `merge_base`, in scratch space.

    Returns (reason, detail, tree): an empty reason and the merged tree's
    object name on success, or a STRIP reason code and its explanation.
    The caller turns a reason into the `Decision`, so every reason code
    this module can produce is visible at ONE place. Nothing here
    touches the repository's own index or working tree -- both are
    redirected into a temporary directory that is removed on the way out,
    because this runs inside the same CI checkout other steps are using.

    The git directory is resolved rather than inherited, so redirecting
    the work tree cannot leave git guessing where the repository is.
    """
    ok, git_dir = git.run("rev-parse", "--absolute-git-dir")
    git_dir = git_dir.strip() if ok else ""
    if not git_dir:
        return ("replay-failed",
                "could not locate the repository's git directory", "")

    with tempfile.TemporaryDirectory(prefix="review-gate-replay-") as scratch:
        env = {
            "GIT_DIR": git_dir,
            "GIT_WORK_TREE": scratch,
            "GIT_INDEX_FILE": str(Path(scratch) / "index"),
        }
        code, _, stderr = git.capture(
            *READ_TREE_FLAGS, merge_base, ours, theirs, env=env)
        if code != 0:
            return ("replay-failed",
                    "could not replay the approved head onto the base: "
                    + (stderr.strip() or f"git exited {code}"), "")

        merged, _, merge_err = git.capture(*MERGE_INDEX_FLAGS, env=env)
        if merged != 0:
            # A non-zero exit means the merge did not complete, but NOT
            # yet why. `merge-index` exits non-zero both for a genuine
            # content conflict and for an operational failure -- a merge
            # driver that could not be run, a scratch directory that
            # vanished -- and the two are different verdicts: one says
            # the approved head cannot be replayed onto this base, the
            # other says this job could not find out.
            #
            # The scratch INDEX is what separates them, and it is the
            # only thing that can: an unmerged entry exists exactly when
            # a path really conflicted. The merge driver's prose is not
            # an interface, and a non-empty stderr proves nothing about
            # which case this is.
            code_u, unmerged, _ = git.capture("ls-files", "-u", env=env)
            paths = conflicted_paths(unmerged) if code_u == 0 else []
            if paths:
                return ("replay-conflicted",
                        "replaying the approved head onto the base conflicts "
                        "in: " + _paths_summary(paths), "")
            return ("replay-failed",
                    "the merge step failed without leaving an unmerged path: "
                    + (merge_err.strip() or f"git exited {merged}"), "")

        code, stdout, stderr = git.capture("write-tree", env=env)
        if code != 0:
            return ("replay-failed", "could not write the replayed tree: "
                    + (stderr.strip() or f"git exited {code}"), "")
        tree = replay_tree(stdout)
        if not tree:
            return ("replay-unreadable",
                    "the replay produced no readable tree object name", "")
        return ("", "", tree)


def decide(git: Git, before: str, after: str, base_ref: str, base_sha: str) -> Decision:
    """KEEP only for a push that provably replays the approved head."""
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

    # The REPLAY BASE: the base-branch revision actually incorporated
    # into AFTER, not the tip the checkout happens to have fetched.
    # `base` is explicitly allowed to be AHEAD of what the update merged
    # in (see base_candidates), so replaying onto `base` itself would
    # pull in base commits AFTER never saw and read them as this PR's
    # own work. The merge base is exactly the incorporated revision.
    ok, replay_base = git.run("merge-base", base, resolved_after)
    replay_base = replay_base.strip() if ok else ""
    if not replay_base:
        return Decision(STRIP, "after-merge-base-failed",
                        "no merge base between the base branch and 'after'")

    # A three-way merge needs the two sides to share history at all.
    # Checking it here rather than letting merge-tree refuse keeps the
    # specific diagnostic: "these histories are unrelated" is a different
    # thing for a maintainer to read than a generic replay failure.
    ok, shared = git.run("merge-base", replay_base, resolved_before)
    if not ok or not shared.strip():
        return Decision(STRIP, "before-merge-base-failed",
                        "no merge base between the base branch and 'before'")

    ok, after_tree = git.run("rev-parse", f"{resolved_after}^{{tree}}")
    after_tree = after_tree.strip() if ok else ""
    if not after_tree:
        return Decision(STRIP, "after-tree-failed",
                        "could not read the tree of 'after'")

    # Replay the APPROVED head onto the revision the update brought in.
    # What this asks is exactly the question the gate exists to answer:
    # "is AFTER nothing more than BEFORE carried onto the new base?" A
    # raw-diff comparison could not ask it, because moving the base
    # changes the source blob of every path the base touched -- so a
    # clean, additive merge into a file the PR also owns read as a
    # content change, which is what stripped approvals from sibling PRs
    # that both edit a shared manifest (#2591).
    reason, detail, replayed = _replay(git, shared.strip(), resolved_before,
                                       replay_base)
    if reason == "replay-failed":
        return Decision(STRIP, "replay-failed", detail)
    if reason == "replay-conflicted":
        return Decision(STRIP, "replay-conflicted", detail)
    if reason == "replay-unreadable":
        return Decision(STRIP, "replay-unreadable", detail)

    if replayed != after_tree:
        # Deliberately the SAME reason code an edit, an addition, a
        # deletion or a revert has always produced: each of those is one
        # way for the pushed tree to differ from the replay, and none of
        # them ever had a code of its own to preserve.
        return Decision(STRIP, "patch-changed",
                        "this push is not a clean replay of the approved head: "
                        f"replayed {replayed}, pushed {after_tree}")
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

    def capture(self, *args: str, env: dict[str, str] | None = None
                ) -> tuple[int, str, str]:
        # Faulted at the ONE seam every invocation now passes through,
        # so a fault reaches `run`'s callers and `capture`'s alike.
        if args[: len(self._prefix)] == self._prefix:
            self._seen += 1
            if self._seen == self._occurrence:
                return 128, "", "injected git failure"
        return super().capture(*args, env=env)


class _MuteGit(Git):
    """A `Git` whose chosen invocation SUCCEEDS with empty output.

    Distinct from `_FaultingGit`: a command that exits 0 and prints
    nothing usable is not a failure git will report, and the decision
    has to refuse it on its own. Nothing in a well-formed repository
    makes `merge-tree` do that, so the branch is reached this way rather
    than by re-implementing the decision.
    """

    def __init__(self, repo_dir, prefix: tuple[str, ...]) -> None:
        super().__init__(repo_dir)
        self._prefix = prefix

    def capture(self, *args: str, env: dict[str, str] | None = None
                ) -> tuple[int, str, str]:
        if args[: len(self._prefix)] == self._prefix:
            return 0, "", ""
        return super().capture(*args, env=env)


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

        # A merge-forward that also touches a PR-owned path, without
        # conflicting, IS ancestry-only (#2591). The reviewed lines sit
        # on different base content afterwards, but the PR contributed
        # nothing new: AFTER is exactly BEFORE replayed onto the base.
        # This is the shape two sibling PRs editing one shared manifest
        # produce every time the first of them merges, and stripping it
        # cost every such PR a rereview of an unchanged patch.
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
        check("a clean merge-forward onto a PR-owned path is ancestry-only",
              decide(Git(repo.path), before, after, "master", m1), KEEP, "ancestry-only")

        # ... and the same graph judged when origin has already advanced
        # PAST the revision the update actually brought in. The replay
        # base is the incorporated revision, never the fetched tip, so a
        # sibling merging between the update and this job must not make
        # its commits read as this PR's own work.
        repo.checkout("upstream")
        repo.write("untouched.txt", "untouched, advanced again\n")
        m2 = repo.commit("M2: the base advances again, after the update")
        repo.set_remote_ref("master", m2)
        check("origin ahead of the revision the update incorporated",
              decide(Git(repo.path), before, after, "master", m2), KEEP, "ancestry-only")

        # The same clean overlapping update, plus one extra PR edit: the
        # push is no longer only a replay, so it strips.
        repo = new_repo("overlapping-plus-edit")
        m0 = _base_world(repo)
        repo.checkout("pr", create=True)
        repo.write("a.txt", "a from the PR\n")
        repo.write("b.txt", _b_with(0, "b1 from the PR\n"))
        before = repo.commit("R1: the PR owns A and B")
        repo._git("checkout", "--quiet", "-B", "upstream", m0)
        repo.write("b.txt", _b_with(8, "b9 advanced on master\n"))
        m1 = repo.commit("M1: base advances on a path the PR also owns")
        repo.checkout("pr")
        repo.merge("upstream", "Merge master into the PR branch")
        repo.write("a.txt", "a from the PR, revised after the merge\n")
        after = repo.commit("R2: an edit rolled into the same push")
        repo.set_remote_ref("master", m1)
        check("a clean merge-forward carrying an extra edit strips",
              decide(Git(repo.path), before, after, "master", m1), STRIP, "patch-changed")

        # And the same shape where the extra change is a REVERT of
        # approved content -- the defect #1679 fixed, restated against
        # the replacement rule: a revert cannot reproduce the replay.
        repo = new_repo("overlapping-plus-revert")
        m0 = _base_world(repo)
        repo.checkout("pr", create=True)
        repo.write("a.txt", "a from the PR\n")
        repo.write("b.txt", _b_with(0, "b1 from the PR\n"))
        before = repo.commit("R1: the PR owns A and B")
        repo._git("checkout", "--quiet", "-B", "upstream", m0)
        repo.write("b.txt", _b_with(8, "b9 advanced on master\n"))
        m1 = repo.commit("M1: base advances on a path the PR also owns")
        repo.checkout("pr")
        repo.merge("upstream", "Merge master into the PR branch")
        repo.write("a.txt", "a base\n")  # back to M0's content
        after = repo.commit("R2: revert A to its base content")
        repo.set_remote_ref("master", m1)
        check("a clean merge-forward carrying a revert strips",
              decide(Git(repo.path), before, after, "master", m1), STRIP, "patch-changed")

        # A replay that genuinely conflicts strips and names the paths,
        # even though AFTER itself is a committed manual resolution and
        # so has nothing wrong with it. The gate cannot vouch for a
        # resolution no reviewer saw.
        repo = new_repo("replay-conflicted")
        m0 = _base_world(repo)
        repo.checkout("pr", create=True)
        repo.write("b.txt", _b_with(4, "b5 from the PR\n"))
        before = repo.commit("R1: the PR edits one line of B")
        repo._git("checkout", "--quiet", "-B", "upstream", m0)
        repo.write("b.txt", _b_with(4, "b5 advanced on master\n"))
        m1 = repo.commit("M1: the base edits the SAME line")
        repo.checkout("pr")
        # `-X ours` lets the harness build the merge without git
        # stopping on the conflict; the resolution is then written
        # explicitly, which is what a human resolving by hand produces.
        repo._git("merge", "--quiet", "--no-ff", "-X", "ours",
                  "-m", "Merge master into the PR branch", "upstream")
        repo.write("b.txt", _b_with(4, "b5 resolved by hand\n"))
        after = repo.commit("R2: a committed manual resolution")
        repo.set_remote_ref("master", m1)
        conflicted = decide(Git(repo.path), before, after, "master", m1)
        check("a conflicted replay strips even when AFTER resolved it",
              conflicted, STRIP, "replay-conflicted")
        if "b.txt" not in conflicted.detail:
            failures.append(
                "a conflicted replay must name the conflicted paths; got "
                f"{conflicted.detail!r}")

        # A file-MODE change with identical content: the trees differ, so
        # comparing tree object names catches what a content-only
        # comparison would wave through.
        repo = new_repo("mode-only")
        m0 = _base_world(repo)
        repo.checkout("pr", create=True)
        repo.write("a.txt", "a from the PR\n")
        before = repo.commit("R1: modify A")
        # Chmod on DISK, not just in the index: `commit` re-adds the
        # worktree, which would otherwise put the old mode straight back.
        (Path(repo.path) / "a.txt").chmod(0o755)
        after = repo.commit("R2: make A executable, same bytes")
        repo.set_remote_ref("master", m0)
        check("a mode-only change is not a clean replay",
              decide(Git(repo.path), before, after, "master", m0),
              STRIP, "patch-changed")

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
        check("fail-closed: AFTER's tree cannot be read",
              decide(_FaultingGit(repo.path, ("rev-parse", f"{after}^{{tree}}")),
                     before, after, "master", m0), STRIP, "after-tree-failed")
        check("fail-closed: the replay command fails",
              decide(_FaultingGit(repo.path, READ_TREE_FLAGS),
                     before, after, "master", m0), STRIP, "replay-failed")
        check("fail-closed: the replayed tree cannot be written",
              decide(_FaultingGit(repo.path, ("write-tree",)),
                     before, after, "master", m0), STRIP, "replay-failed")
        # The merge step failing OPERATIONALLY is a failure, not a
        # conflict: nothing conflicted, the job just could not find out.
        # Reporting it as a conflict would name no paths and tell a
        # maintainer the approved head cannot be replayed, which is a
        # different and wrong thing to have said.
        merge_failed = decide(_FaultingGit(repo.path, MERGE_INDEX_FLAGS),
                              before, after, "master", m0)
        check("fail-closed: the merge step fails with nothing unmerged",
              merge_failed, STRIP, "replay-failed")
        if "conflict" in merge_failed.detail.lower():
            failures.append(
                "an operational merge failure must not be described as a "
                f"conflict; got {merge_failed.detail!r}")
        # Exit 0 with unusable output is NOT a verdict: a merge-tree that
        # printed no object name has told us nothing, and the difference
        # between that and a crash is what the two reason codes carry.
        check("fail-closed: the replay output is unreadable",
              decide(_MuteGit(repo.path, ("write-tree",)),
                     before, after, "master", m0), STRIP, "replay-unreadable")
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
        # BEFORE is unrelated to the base while AFTER is not, so the
        # replay base resolves and it is the REPLAY's own base that
        # cannot be established. AFTER joins the two histories, which is
        # the only way to reach this guard now that the incorporated
        # revision is derived from AFTER.
        repo = new_repo("unrelated-before")
        m0 = _base_world(repo)
        repo._git("checkout", "--quiet", "--orphan", "orphan")
        # From the WORKTREE as well as the index: `commit` re-adds the
        # worktree, so a cached-only removal would leave the orphan root
        # holding the base's files and make the join an empty push.
        repo._git("rm", "-rqf", ".")
        repo.write("orphan.txt", "an unrelated root\n")
        orphan_one = repo.commit("O1: an unrelated root commit")
        repo._git("merge", "--quiet", "--no-ff", "--allow-unrelated-histories",
                  "-m", "Join the unrelated histories", m0)
        joined = repo.head()
        repo.set_remote_ref("master", m0)
        check("fail-closed: no merge base with 'before'",
              decide(Git(repo.path), orphan_one, joined, "master", m0),
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

        # ---- the frozen replay commands ----
        if READ_TREE_FLAGS != ("read-tree", "-i", "-m"):
            failures.append(
                "READ_TREE_FLAGS changed: -m is the three-way index merge "
                "and -i keeps the empty scratch work tree out of it. "
                f"Got {READ_TREE_FLAGS!r}")
        if MERGE_INDEX_FLAGS != ("merge-index", "-o", "git-merge-one-file", "-a"):
            failures.append(
                "MERGE_INDEX_FLAGS changed: git-merge-one-file is git's own "
                "content merge -- without it two additive edits to one file "
                "stay unmerged, which is the defect #2591 fixed -- -a runs "
                "it over every unmerged entry and -o finishes them all. "
                f"Got {MERGE_INDEX_FLAGS!r}")

        # ---- replay_tree reads only a real object name, off line one ----
        tree = "0" * 40
        if replay_tree(tree + "\n") != tree:
            failures.append("replay_tree must read a clean merge's tree")
        if replay_tree(tree + "\n\n100644 abc 1\tx\n") != tree:
            failures.append(
                "replay_tree must read the tree off line ONE, so a "
                "conflicted merge's own tree is still available")
        for bad, label in (("", "no output"),
                           ("not-a-tree\n", "a non-object first line"),
                           ("0" * 39 + "\n", "a short object name"),
                           ("z" * 40 + "\n", "a non-hex object name")):
            if replay_tree(bad) != "":
                failures.append(f"replay_tree must reject {label}")

        # ---- conflicted_paths de-duplicates the per-stage records ----
        conflicted = (tree + "\n\n"
                      "100644 aaa 1\tsrc/a.txt\n"
                      "100644 bbb 2\tsrc/a.txt\n"
                      "100644 ccc 3\tsrc/a.txt\n"
                      "100644 ddd 1\tsrc/b.txt\n")
        if conflicted_paths(conflicted) != ["src/a.txt", "src/b.txt"]:
            failures.append(
                "conflicted_paths must report each conflicted path once, in "
                f"first-seen order; got {conflicted_paths(conflicted)!r}")
        if conflicted_paths(tree + "\n") != []:
            failures.append("a clean merge names no conflicted path")

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
    ("bypass the AFTER tree-read guard",
     lambda t: _bypass_guard(t, "after-tree-failed")),
    ("bypass the replay-failure guard", lambda t: _bypass_guard(t, "replay-failed")),
    ("bypass the unreadable-replay guard",
     lambda t: _bypass_guard(t, "replay-unreadable")),
    ("bypass the conflicted-replay guard",
     lambda t: _bypass_guard(t, "replay-conflicted")),
    ("never strip on a changed patch", lambda t: _bypass_guard(t, "patch-changed")),
    ("always strip, never keep",
     lambda t: _replace_once(t, 'return Decision(KEEP, "ancestry-only",',
                             'return Decision(STRIP, "ancestry-only",')),
    ("invert the replay comparison",
     lambda t: _replace_once(t, "if replayed != after_tree:",
                             "if replayed == after_tree:")),
    ("report an operational merge failure as a conflict",
     lambda t: _replace_once(t, "            if paths:\n",
                             "            if True:\n")),
    ("trust the merge driver's stderr instead of the scratch index",
     lambda t: _replace_once(
         t, "            paths = conflicted_paths(unmerged) if code_u == 0 else []",
         "            paths = conflicted_paths(unmerged) if code_u == 0 else []\n"
         "            if merge_err.strip():\n"
         "                paths = paths or ['(unknown)']")),
    ("treat a conflicted replay as clean",
     lambda t: _replace_once(t, "        if merged != 0:\n",
                             "        if False:\n")),
    ("replay onto the FETCHED base tip instead of the incorporated one",
     lambda t: _replace_once(
         t, "    reason, detail, replayed = _replay(git, shared.strip(), resolved_before,\n"
            "                                       replay_base)",
         "    reason, detail, replayed = _replay(git, shared.strip(), resolved_before,\n"
         "                                       base)")),
    ("skip the content merge, leaving trivially-unmergeable paths conflicted",
     lambda t: _replace_once(
         t, "        merged, _, merge_err = git.capture(*MERGE_INDEX_FLAGS, env=env)",
         "        merged, merge_err = 0, ''")),
    ("redirect neither the index nor the work tree into scratch space",
     lambda t: _replace_once(
         t, '            "GIT_INDEX_FILE": str(Path(scratch) / "index"),\n', "")),
    ("accept any first line as the replay tree",
     lambda t: _replace_once(
         t, '    if len(first) == 40 and all(c in "0123456789abcdef" for c in first):\n'
            "        return first\n"
            '    return ""',
         "    return first")),
    ("compare only the replayed tree's PATHS, not the tree itself",
     lambda t: _replace_once(
         t, "    if replayed != after_tree:",
         "    if False and replayed != after_tree:")),
    ("drop -m from the frozen read-tree command",
     lambda t: _replace_once(t, 'READ_TREE_FLAGS = ("read-tree", "-i", "-m")',
                             'READ_TREE_FLAGS = ("read-tree", "-i")')),
    ("prefer a stale local base ref over origin/<ref>",
     lambda t: _replace_once(t, '        f"origin/{base_ref}" if base_ref else "",\n'
                                '        base_sha or "",\n'
                                '        base_ref or "",',
                             '        base_ref or "",\n'
                                '        f"origin/{base_ref}" if base_ref else "",\n'
                                '        base_sha or "",')),
    ("ignore the event's base.sha fallback",
     lambda t: _replace_once(t, '        base_sha or "",\n', "")),
    ("report no conflicted paths at all",
     lambda t: _replace_once(t, "    paths: list[str] = []\n"
                                "    for line in stdout.splitlines():",
                             "    paths: list[str] = []\n"
                             "    for line in []:")),
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
