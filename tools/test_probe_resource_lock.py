#!/usr/bin/env python3
"""Unit tests for the cross-process probe resource lock (#1436).

Deterministic, engine-free and GPU-free: no probe is ever run, no
Vulkan, no worldgen, no census and no docs worktree. What IS real here
is the concurrency — every mutual-exclusion case starts a separate
INTERPRETER, because the whole point of this module is coordination the
in-process `probe_runner_resources.ResourceLedger` cannot provide, and a test that
proved it with threads would be testing something else. One case
SIGKILLs a holder, because "a dead holder owns nothing" is the property
that makes waiting for a resource safe rather than a wedge.

Covered:
  * all four interest combinations — shared/shared coexisting, and
    shared/exclusive, exclusive/shared and exclusive/exclusive
    conflicting;
  * release making a refused interest available again;
  * a SIGKILLed holder releasing everything instantly;
  * complete-set acquisition: a partly-available set is refused whole
    and leaves nothing held, so no other acquirer is blocked by the
    wreckage;
  * deadlock-freedom: two acquirers asking for the same pair in
    opposite orders never block each other, because the plan is sorted
    and every attempt is non-blocking;
  * ownership-safe release: a late release from an abandoned hold
    cannot take a successor's resource away, and lock files are never
    unlinked;
  * repository-common identity: two linked worktrees of one repository
    share a namespace, and two different repositories do not;
  * the namespace is a controlled refusal, never a path-derived guess,
    where git cannot answer;
  * the scratch directory's three safety properties, and a planted
    non-regular lock file being refused loudly rather than treated as
    busy;
  * name validation, and an empty interest set being a legitimate hold.

Usage:
  python3 tools/test_probe_resource_lock.py
Exit codes: 0 = all tests passed, 1 = one or more failed.
"""
from __future__ import annotations

import json
import os
import shutil
import signal
import stat
import subprocess
import sys
import tempfile
import textwrap
import time
import uuid
from pathlib import Path

sys.path.insert(0, str(Path(__file__).resolve().parent))
import probe_resource_lock as lock  # type: ignore

TOOLS_DIR = str(Path(__file__).resolve().parent)
FAILURES: list[str] = []


def expect(cond: bool, msg: str) -> None:
    if not cond:
        FAILURES.append(msg)
        print(f"  FAIL: {msg}")
    else:
        print(f"  OK:   {msg}")


# --------------------------------------------------------------------------
# A separate process that takes an interest and holds it
# --------------------------------------------------------------------------
HOLDER_SRC = textwrap.dedent("""\
    import json, sys, time
    from pathlib import Path
    sys.path.insert(0, sys.argv[1])
    import probe_resource_lock as lock
    root, namespace, ready, release = sys.argv[2:6]
    plan = json.loads(sys.argv[6])
    try:
        hold = lock.acquire(exclusive=plan.get("exclusive", []),
                            shared=plan.get("shared", []),
                            namespace=namespace, root=Path(root),
                            purpose=plan.get("purpose", "holder"))
    except lock.ResourceBusy as busy:
        Path(ready).write_text(json.dumps({"held": False,
                                           "busy": busy.to_document()}))
        raise SystemExit(3)
    except lock.ResourceLockError as error:
        Path(ready).write_text(json.dumps({"held": False, "error": str(error)}))
        raise SystemExit(4)
    Path(ready).write_text(json.dumps({"held": True}))
    deadline = time.time() + 120
    while not Path(release).exists() and time.time() < deadline:
        time.sleep(0.02)
    hold.release()
""")


class Scratch:
    """A throwaway lock root with `/tmp`'s own mode.

    Sticky and owned by us, which is what `_check_shared_dir` insists
    on: redirecting the root is what keeps every case here away from the
    real repository's live locks, and the mode is what keeps the
    redirection honest rather than a way to skip the safety checks.
    """

    def __init__(self) -> None:
        self.root = Path(tempfile.mkdtemp(prefix="test_probe_resource_lock_"))
        os.chmod(self.root, lock.SHARED_DIR_MODE)
        self.namespace = f"selftest{uuid.uuid4().hex[:12]}"
        self.script = self.root / "holder.py"
        self.script.write_text(HOLDER_SRC)
        self._holders: list[Holder] = []

    def holder(self, *, exclusive=(), shared=(), purpose="holder") -> "Holder":
        holder = Holder(self, exclusive=exclusive, shared=shared,
                        purpose=purpose)
        self._holders.append(holder)
        return holder

    def cleanup(self) -> None:
        for holder in self._holders:
            holder.stop()
        shutil.rmtree(self.root, ignore_errors=True)


class Holder:
    def __init__(self, scratch: Scratch, *, exclusive=(), shared=(),
                 purpose="holder") -> None:
        self.scratch = scratch
        token = uuid.uuid4().hex[:8]
        self.ready = scratch.root / f"ready-{token}"
        self.release_flag = scratch.root / f"release-{token}"
        self.proc = subprocess.Popen(
            [sys.executable, str(scratch.script), TOOLS_DIR,
             str(scratch.root), scratch.namespace, str(self.ready),
             str(self.release_flag),
             json.dumps({"exclusive": sorted(exclusive),
                         "shared": sorted(shared), "purpose": purpose})])

    def outcome(self, seconds: float = 30.0) -> dict:
        deadline = time.monotonic() + seconds
        while time.monotonic() < deadline:
            try:
                return json.loads(self.ready.read_text())
            except (OSError, ValueError):
                time.sleep(0.02)
        return {}

    def took_it(self, seconds: float = 30.0) -> bool:
        return bool(self.outcome(seconds).get("held"))

    def kill(self) -> None:
        try:
            self.proc.kill()
            self.proc.wait(timeout=30)
        except (OSError, subprocess.TimeoutExpired):
            pass

    def stop(self) -> None:
        try:
            self.release_flag.write_text("go")
        except OSError:
            pass
        try:
            self.proc.wait(timeout=30)
        except subprocess.TimeoutExpired:
            self.kill()


def taking(scratch: Scratch, *, exclusive=(), shared=()):
    """Try one acquisition IN THIS PROCESS; return (hold, busy, error)."""
    try:
        return (lock.acquire(exclusive=exclusive, shared=shared,
                             namespace=scratch.namespace, root=scratch.root),
                None, None)
    except lock.ResourceBusy as busy:
        return None, busy, None
    except lock.ResourceLockError as error:
        return None, None, error


# --------------------------------------------------------------------------
# The four interest combinations
# --------------------------------------------------------------------------
def test_two_shared_holders_coexist() -> None:
    print("\n-- SHARED against SHARED: readers overlap freely")
    scratch = Scratch()
    try:
        first = scratch.holder(shared={"repo-config"})
        expect(first.took_it(), "the first shared holder took the resource")
        second = scratch.holder(shared={"repo-config"})
        expect(second.took_it(),
               "a second INDEPENDENT PROCESS takes the same shared interest")
        hold, busy, error = taking(scratch, shared={"repo-config"})
        expect(hold is not None and busy is None and error is None,
               f"and so does a third acquirer (busy={busy}, error={error})")
        if hold is not None:
            hold.release()
    finally:
        scratch.cleanup()


def test_a_shared_holder_blocks_an_exclusive_acquirer() -> None:
    print("\n-- SHARED against EXCLUSIVE: a reader blocks a writer")
    scratch = Scratch()
    try:
        reader = scratch.holder(shared={"repo-config"}, purpose="a reader")
        expect(reader.took_it(), "the shared holder took the resource")
        hold, busy, error = taking(scratch, exclusive={"repo-config"})
        expect(hold is None and busy is not None,
               f"the exclusive acquirer is refused (error={error})")
        if busy is not None:
            expect(busy.resource == "repo-config"
                   and busy.interest == lock.EXCLUSIVE,
                   f"and names the resource and the interest refused "
                   f"({busy.resource!r}, {busy.interest})")
            expect(any(entry.get("interest") == lock.SHARED
                       and entry.get("purpose") == "a reader"
                       for entry in busy.holders),
                   f"and identifies the shared holder ({busy.holders})")
        reader.stop()
        hold, busy, error = taking(scratch, exclusive={"repo-config"})
        expect(hold is not None,
               f"and the same acquisition succeeds once the reader releases "
               f"(busy={busy}, error={error})")
        if hold is not None:
            hold.release()
    finally:
        scratch.cleanup()


def test_an_exclusive_holder_blocks_a_shared_acquirer() -> None:
    print("\n-- EXCLUSIVE against SHARED: a writer blocks a reader")
    scratch = Scratch()
    try:
        writer = scratch.holder(exclusive={"repo-config"}, purpose="a writer")
        expect(writer.took_it(), "the exclusive holder took the resource")
        hold, busy, error = taking(scratch, shared={"repo-config"})
        expect(hold is None and busy is not None,
               f"the shared acquirer is refused (error={error})")
        if busy is not None:
            expect(busy.interest == lock.SHARED,
                   f"and the refused interest is reported as shared "
                   f"({busy.interest})")
            expect(any(entry.get("interest") == lock.EXCLUSIVE
                       for entry in busy.holders),
                   f"and the exclusive holder is identified ({busy.holders})")
        writer.stop()
        hold, busy, error = taking(scratch, shared={"repo-config"})
        expect(hold is not None,
               f"and it succeeds once the writer releases (busy={busy})")
        if hold is not None:
            hold.release()
    finally:
        scratch.cleanup()


def test_two_exclusive_holders_conflict() -> None:
    print("\n-- EXCLUSIVE against EXCLUSIVE: one writer at a time")
    scratch = Scratch()
    try:
        writer = scratch.holder(exclusive={"repo-config"})
        expect(writer.took_it(), "the first exclusive holder took the resource")
        second = scratch.holder(exclusive={"repo-config"})
        outcome = second.outcome()
        expect(outcome.get("held") is False,
               f"a second exclusive acquirer in its own process is refused "
               f"({outcome})")
        expect((outcome.get("busy") or {}).get("interest") == lock.EXCLUSIVE,
               f"and reports the exclusive conflict ({outcome.get('busy')})")
        writer.stop()
        hold, busy, error = taking(scratch, exclusive={"repo-config"})
        expect(hold is not None,
               f"and one succeeds once the first releases (busy={busy})")
        if hold is not None:
            hold.release()
    finally:
        scratch.cleanup()


def test_a_killed_holder_owns_nothing() -> None:
    print("\n-- a SIGKILLed holder releases everything immediately")
    scratch = Scratch()
    try:
        writer = scratch.holder(exclusive={"repo-config"})
        expect(writer.took_it(), "the exclusive holder took the resource")
        hold, busy, _error = taking(scratch, shared={"repo-config"})
        expect(hold is None and busy is not None,
               "it really is held before the kill")
        writer.kill()
        # No grace, no staleness horizon, no recovery pass: the kernel
        # released the flock with the open file description. This is why
        # `wait_acquire` can be unbounded without wedging.
        hold, busy, error = taking(scratch, exclusive={"repo-config"})
        expect(hold is not None,
               f"a killed holder's interest is available at once "
               f"(busy={busy}, error={error})")
        if hold is not None:
            hold.release()
    finally:
        scratch.cleanup()


# --------------------------------------------------------------------------
# Complete-set acquisition
# --------------------------------------------------------------------------
def test_a_partly_available_set_is_refused_whole() -> None:
    print("\n-- a set is taken whole or not at all, and a refusal leaves "
          "nothing held")
    scratch = Scratch()
    try:
        holder = scratch.holder(exclusive={"resource-b"})
        expect(holder.took_it(), "another process holds resource-b")
        hold, busy, error = taking(scratch,
                                   exclusive={"resource-a", "resource-b"})
        expect(hold is None and busy is not None,
               f"the two-resource acquisition is refused (error={error})")
        if busy is not None:
            expect(busy.resource == "resource-b",
                   f"and names the resource that was unavailable "
                   f"({busy.resource!r})")
        # The load-bearing half: `resource-a` was taken first, in sorted
        # order, and must have been given back. A leak here would block
        # every later acquirer of an unrelated resource forever.
        other, busy_a, error_a = taking(scratch, exclusive={"resource-a"})
        expect(other is not None,
               f"resource-a was rolled back, not left held (busy={busy_a}, "
               f"error={error_a})")
        if other is not None:
            other.release()
    finally:
        scratch.cleanup()


def test_opposite_orders_cannot_deadlock() -> None:
    print("\n-- two acquirers wanting the same pair in opposite orders never "
          "block each other")
    scratch = Scratch()
    try:
        # Both processes ask for the same two names, written in opposite
        # orders. A blocking implementation that honoured the caller's
        # order could have each hold one and wait on the other forever;
        # the sorted plan plus LOCK_NB makes that unrepresentable.
        first = scratch.holder(exclusive={"resource-a", "resource-b"})
        outcome_first = first.outcome()
        second = scratch.holder(exclusive={"resource-b", "resource-a"})
        outcome_second = second.outcome()
        held = [bool(outcome_first.get("held")), bool(outcome_second.get("held"))]
        expect(any(held),
               f"at least one acquirer got the whole set ({outcome_first}, "
               f"{outcome_second})")
        expect(not all(held),
               "and not both, since the interests are exclusive")
        expect(all(outcome.get("held") is not None
                   for outcome in (outcome_first, outcome_second)),
               "both processes answered rather than hanging")
        first.stop()
        second.stop()
        hold, busy, _error = taking(scratch,
                                    exclusive={"resource-a", "resource-b"})
        expect(hold is not None,
               f"and the whole set is available once both are gone "
               f"(busy={busy})")
        if hold is not None:
            hold.release()
    finally:
        scratch.cleanup()


def test_an_empty_interest_set_is_a_hold_that_owns_nothing() -> None:
    print("\n-- an empty interest set is a legitimate hold")
    scratch = Scratch()
    try:
        hold, busy, error = taking(scratch)
        expect(hold is not None,
               f"acquiring nothing succeeds (busy={busy}, error={error})")
        if hold is not None:
            expect(not hold.exclusive and not hold.shared,
                   "and owns nothing")
            expect(hold.interest("repo-config") is None,
                   "so it reports no interest in any resource")
            hold.release()
            hold.release()
            expect(hold.released, "releasing it twice is a no-op")
    finally:
        scratch.cleanup()


# --------------------------------------------------------------------------
# Ownership-safe release
# --------------------------------------------------------------------------
def test_a_late_release_cannot_take_a_successors_resource() -> None:
    print("\n-- a late release from an abandoned hold leaves a successor's "
          "resource alone")
    scratch = Scratch()
    try:
        stale, busy, _error = taking(scratch, exclusive={"repo-config"})
        expect(stale is not None, f"the first holder acquired it (busy={busy})")
        assert stale is not None
        stale.release()
        successor = scratch.holder(exclusive={"repo-config"},
                                   purpose="the successor")
        expect(successor.took_it(), "a successor process took it over")
        # The late cleanup: an abandoned hold object unwinding long after
        # its work was given up. It must be inert.
        stale.release()
        hold, busy, _error = taking(scratch, shared={"repo-config"})
        expect(hold is None and busy is not None,
               "the successor still holds the resource after the late release")
        if busy is not None:
            expect(any(entry.get("purpose") == "the successor"
                       for entry in busy.holders),
                   f"and it is still the successor holding it ({busy.holders})")
        if hold is not None:
            hold.release()
    finally:
        scratch.cleanup()


def test_a_lock_file_is_never_unlinked() -> None:
    print("\n-- releasing an interest never unlinks the lock file")
    scratch = Scratch()
    try:
        path = lock.lock_path("repo-config", namespace=scratch.namespace,
                              root=scratch.root)
        hold, _busy, _error = taking(scratch, exclusive={"repo-config"})
        expect(hold is not None and path.is_file(),
               "the lock file exists while the interest is held")
        if hold is not None:
            hold.release()
        # Unlinking is precisely the operation that reintroduces the race
        # the lock removed: a second process could create a fresh file at
        # the same name and lock THAT while the first still held the old
        # inode.
        expect(path.is_file(), "and it is still there after the release")
    finally:
        scratch.cleanup()


# --------------------------------------------------------------------------
# The namespace is the repository, not the worktree
# --------------------------------------------------------------------------
def _git(cwd: Path, *args: str) -> None:
    subprocess.run(["git", *args], cwd=str(cwd), check=True,
                   capture_output=True, text=True)


def _repository(root: Path) -> Path:
    root.mkdir(parents=True, exist_ok=True)
    _git(root, "init", "-q", "-b", "main")
    _git(root, "config", "user.email", "selftest@example.invalid")
    _git(root, "config", "user.name", "selftest")
    (root / "seed.txt").write_text("seed\n")
    _git(root, "add", "seed.txt")
    _git(root, "commit", "-qm", "seed")
    return root


def test_linked_worktrees_of_one_repository_share_a_namespace() -> None:
    print("\n-- two linked worktrees of ONE repository resolve the SAME "
          "namespace, and two repositories do not")
    base = Path(tempfile.mkdtemp(prefix="test_resource_namespace_"))
    try:
        primary = _repository(base / "primary")
        linked = base / "linked"
        _git(primary, "worktree", "add", "-q", "-b", "side", str(linked))
        other = _repository(base / "other")

        primary_token = lock.repository_namespace(primary)
        linked_token = lock.repository_namespace(linked)
        other_token = lock.repository_namespace(other)
        expect(primary_token == linked_token,
               f"a linked worktree namespaces with its primary checkout "
               f"({primary_token} vs {linked_token})")
        expect(primary_token != other_token,
               f"and a different repository does not ({primary_token} vs "
               f"{other_token})")
        # The failure mode this exists to close: a namespace derived from
        # the CHECKOUT would differ between these two, and the two
        # worktrees would coordinate nobody while driving the same
        # tracked tree.
        expect(str(primary) != str(linked),
               "the two worktrees really are different directories")
        expect(len(primary_token) == 16 and primary_token.isalnum(),
               f"the token is a short path-safe digest ({primary_token!r})")
        expect(lock.repository_common_dir(linked)
               == lock.repository_common_dir(primary),
               "both resolve the same common git directory")
    finally:
        shutil.rmtree(base, ignore_errors=True)


def test_a_checkout_git_cannot_answer_for_is_refused() -> None:
    print("\n-- a directory git cannot answer for is a refusal, never a "
          "path-derived namespace")
    base = Path(tempfile.mkdtemp(prefix="test_resource_nogit_"))
    try:
        # Not a repository, and with the search stopped here so an
        # enclosing checkout cannot answer on its behalf.
        (base / ".git").mkdir()
        (base / ".git" / "config").write_text("")
        raised = None
        try:
            lock.repository_namespace(base)
        except lock.ResourceLockError as error:
            raised = error
        except Exception as error:  # pragma: no cover - reported below
            raised = error
        expect(isinstance(raised, lock.ResourceLockError),
               f"an unusable checkout raises the controlled refusal "
               f"(got {raised!r})")
    finally:
        shutil.rmtree(base, ignore_errors=True)


def test_the_default_root_is_the_host_shared_directory() -> None:
    print("\n-- the default lock root is /tmp itself, flat")
    expect(lock.LOCK_ROOT == Path("/tmp"),
           f"the lock root is /tmp, not a subdirectory and not TMPDIR "
           f"(got {lock.LOCK_ROOT})")
    path = lock.lock_path("repo-config", namespace="abcdef0123456789")
    expect(path.parent == Path("/tmp"),
           f"so a lock file is a flat entry in it (got {path})")
    expect(path.name.startswith(lock.SHARED_PREFIX),
           f"named by the shared prefix (got {path.name})")


# --------------------------------------------------------------------------
# Refusals
# --------------------------------------------------------------------------
def test_a_planted_lock_file_is_refused_loudly() -> None:
    print("\n-- a lock path that is not a plain file is a hard refusal, not "
          "a busy resource")
    scratch = Scratch()
    try:
        path = lock.lock_path("repo-config", namespace=scratch.namespace,
                              root=scratch.root)
        target = scratch.root / "planted-target"
        target.write_text("")
        path.symlink_to(target)
        hold, busy, error = taking(scratch, exclusive={"repo-config"})
        expect(hold is None and error is not None,
               f"a symlinked lock path raises ResourceLockError (busy={busy})")
        expect(busy is None,
               "and is NOT reported as busy, which a caller would treat as "
               "an ordinary no-work success")
        if error is not None:
            expect(str(path) in str(error),
                   f"the diagnostic names the path ({error})")
    finally:
        scratch.cleanup()


def test_a_non_sticky_scratch_directory_is_refused() -> None:
    print("\n-- the scratch directory's safety properties are checked, never "
          "repaired")
    root = Path(tempfile.mkdtemp(prefix="test_resource_mode_"))
    try:
        os.chmod(root, 0o777)  # world-writable but NOT sticky
        raised = None
        try:
            lock.acquire(shared={"repo-config"}, namespace="abcdef0123456789",
                         root=root)
        except lock.ResourceLockError as error:
            raised = error
        expect(raised is not None,
               "a non-sticky world-writable directory is refused")
        expect(raised is not None and "sticky" in str(raised),
               f"and the diagnostic says why ({raised})")
        mode = stat.S_IMODE(root.stat().st_mode)
        expect(mode == 0o777,
               f"and nothing was chmodded to make it work (mode {mode:04o})")
    finally:
        shutil.rmtree(root, ignore_errors=True)


def test_names_are_validated_before_they_become_paths() -> None:
    print("\n-- resource and namespace names are validated, not trusted")
    scratch = Scratch()
    try:
        for bad in ("../escape", "with/slash", "", ".hidden", "a" * 100):
            raised = None
            try:
                lock.acquire(shared={bad}, namespace=scratch.namespace,
                             root=scratch.root)
            except lock.ResourceLockError as error:
                raised = error
            expect(raised is not None,
                   f"resource name {bad!r} is refused")
        raised = None
        try:
            lock.acquire(shared={"repo-config"}, namespace="../escape",
                         root=scratch.root)
        except lock.ResourceLockError as error:
            raised = error
        expect(raised is not None, "and so is a namespace with path structure")
    finally:
        scratch.cleanup()


def test_an_exclusive_declaration_wins_over_a_shared_one() -> None:
    print("\n-- a name given in both interests is held EXCLUSIVELY, once")
    scratch = Scratch()
    try:
        # `probe_runner_resources.shared_resources` already subtracts an exclusively
        # declared resource, so this should not arise -- but taking both
        # interests on one name would mean locking one file twice from
        # one process, which flock refuses against our own description.
        hold, busy, error = taking(scratch, exclusive={"repo-config"},
                                   shared={"repo-config"})
        expect(hold is not None,
               f"the overlapping request succeeds (busy={busy}, error={error})")
        if hold is not None:
            expect(hold.interest("repo-config") == lock.EXCLUSIVE,
                   f"and is held exclusively "
                   f"({hold.interest('repo-config')})")
            expect("repo-config" not in hold.shared,
                   "with no second shared interest to leak")
            hold.release()
    finally:
        scratch.cleanup()


def test_holders_is_best_effort_and_says_so() -> None:
    print("\n-- holder identification is diagnostics, never mutual exclusion")
    scratch = Scratch()
    try:
        holder = scratch.holder(shared={"repo-config"}, purpose="a reader")
        expect(holder.took_it(), "the holder took the resource")
        found = lock.holders("repo-config", namespace=scratch.namespace,
                             root=scratch.root)
        expect(len(found) == 1 and found[0]["interest"] == lock.SHARED,
               f"one live shared holder is reported ({found})")
        expect(found and found[0].get("pid") == holder.proc.pid,
               f"and it is the process that took it ({found})")
        # Removing the diagnostic note must not change what is HELD.
        for entry in scratch.root.glob(f"{lock.SHARED_PREFIX}-*-holder-*.json"):
            entry.unlink()
        hold, busy, _error = taking(scratch, exclusive={"repo-config"})
        expect(hold is None and busy is not None,
               "the resource is still held with no note to describe it")
        if busy is not None:
            expect(busy.holders == [],
                   f"and the refusal reports no identifiable holder "
                   f"({busy.holders})")
        if hold is not None:
            hold.release()
    finally:
        scratch.cleanup()


def main() -> int:
    test_two_shared_holders_coexist()
    test_a_shared_holder_blocks_an_exclusive_acquirer()
    test_an_exclusive_holder_blocks_a_shared_acquirer()
    test_two_exclusive_holders_conflict()
    test_a_killed_holder_owns_nothing()
    test_a_partly_available_set_is_refused_whole()
    test_opposite_orders_cannot_deadlock()
    test_an_empty_interest_set_is_a_hold_that_owns_nothing()
    test_a_late_release_cannot_take_a_successors_resource()
    test_a_lock_file_is_never_unlinked()
    test_linked_worktrees_of_one_repository_share_a_namespace()
    test_a_checkout_git_cannot_answer_for_is_refused()
    test_the_default_root_is_the_host_shared_directory()
    test_a_planted_lock_file_is_refused_loudly()
    test_a_non_sticky_scratch_directory_is_refused()
    test_names_are_validated_before_they_become_paths()
    test_an_exclusive_declaration_wins_over_a_shared_one()
    test_holders_is_best_effort_and_says_so()
    if FAILURES:
        print(f"\n{len(FAILURES)} test(s) failed:")
        for failure in FAILURES:
            print(f"  {failure}")
        return 1
    print("\nAll probe resource lock tests passed")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
