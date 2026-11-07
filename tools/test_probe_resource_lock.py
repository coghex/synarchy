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
  * conflict-aware queueing: a waiting writer stops later readers from
    barging, conflicting writers run in queue order, unrelated resources
    remain available, and killed or timed-out waiters leave no stale
    obstruction;
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
import probe_runner_resources as resources  # type: ignore

import selftestlib  # noqa: E402
from selftestlib import FAILURES, expect  # noqa: E402

TOOLS_DIR = str(Path(__file__).resolve().parent)


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

WAITER_SRC = textwrap.dedent("""\
    import json, sys, time
    from pathlib import Path
    sys.path.insert(0, sys.argv[1])
    import probe_resource_lock as lock
    root, namespace, ready, release = sys.argv[2:6]
    plan = json.loads(sys.argv[6])
    def announce(busy):
        Path(ready).write_text(json.dumps({"state": "queued",
                                           "busy": busy.to_document()}))
    try:
        hold = lock.wait_acquire(exclusive=plan.get("exclusive", []),
                                 shared=plan.get("shared", []),
                                 namespace=namespace, root=Path(root),
                                 purpose=plan.get("purpose", "waiter"),
                                 poll=0.02, announce=announce,
                                 announce_interval=0.0)
    except lock.ResourceLockError as error:
        Path(ready).write_text(json.dumps({"state": "error",
                                           "error": str(error)}))
        raise SystemExit(4)
    Path(ready).write_text(json.dumps({"state": "held"}))
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
        self.waiter_script = self.root / "waiter.py"
        self.waiter_script.write_text(WAITER_SRC)
        self._holders: list[Holder | Waiter] = []

    def holder(self, *, exclusive=(), shared=(), purpose="holder",
               namespace=None) -> "Holder":
        holder = Holder(self, exclusive=exclusive, shared=shared,
                        purpose=purpose, namespace=namespace)
        self._holders.append(holder)
        return holder

    def waiter(self, *, exclusive=(), shared=(), purpose="waiter",
               namespace=None) -> "Waiter":
        waiter = Waiter(self, exclusive=exclusive, shared=shared,
                        purpose=purpose, namespace=namespace)
        self._holders.append(waiter)
        return waiter

    def cleanup(self) -> None:
        for holder in self._holders:
            holder.stop()
        shutil.rmtree(self.root, ignore_errors=True)


class Holder:
    def __init__(self, scratch: Scratch, *, exclusive=(), shared=(),
                 purpose="holder", namespace=None) -> None:
        self.scratch = scratch
        token = uuid.uuid4().hex[:8]
        self.ready = scratch.root / f"ready-{token}"
        self.release_flag = scratch.root / f"release-{token}"
        self.proc = subprocess.Popen(
            [sys.executable, str(scratch.script), TOOLS_DIR,
             str(scratch.root), namespace or scratch.namespace, str(self.ready),
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


class Waiter:
    def __init__(self, scratch: Scratch, *, exclusive=(), shared=(),
                 purpose="waiter", namespace=None) -> None:
        self.scratch = scratch
        token = uuid.uuid4().hex[:8]
        self.ready = scratch.root / f"waiter-ready-{token}"
        self.release_flag = scratch.root / f"waiter-release-{token}"
        self.proc = subprocess.Popen(
            [sys.executable, str(scratch.waiter_script), TOOLS_DIR,
             str(scratch.root), namespace or scratch.namespace, str(self.ready),
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

    def queued(self, seconds: float = 30.0) -> bool:
        return self.outcome(seconds).get("state") == "queued"

    def took_it(self, seconds: float = 30.0) -> bool:
        deadline = time.monotonic() + seconds
        while time.monotonic() < deadline:
            if self.outcome(0.05).get("state") == "held":
                return True
            time.sleep(0.02)
        return False

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


def taking(scratch: Scratch, *, exclusive=(), shared=(), namespace=None):
    """Try one acquisition IN THIS PROCESS; return (hold, busy, error)."""
    try:
        return (lock.acquire(exclusive=exclusive, shared=shared,
                             namespace=namespace or scratch.namespace,
                             root=scratch.root),
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
# Conflict-aware admission queue
# --------------------------------------------------------------------------
def test_a_waiting_writer_stops_later_readers_from_barging() -> None:
    print("\n-- a queued writer closes admission to later readers, then runs")
    scratch = Scratch()
    try:
        reader = scratch.holder(shared={"cabal-build"},
                                purpose="existing reader")
        expect(reader.took_it(), "the existing reader took the resource")
        writer = scratch.waiter(exclusive={"cabal-build"},
                                purpose="queued writer")
        expect(writer.queued(), "the writer published its queued request")

        late, busy, error = taking(scratch, shared={"cabal-build"})
        expect(late is None and busy is not None,
               f"a later reader is refused while the writer waits "
               f"(busy={busy}, error={error})")
        if busy is not None:
            expect(any(holder.get("state") == "queued"
                       and holder.get("purpose") == "queued writer"
                       for holder in busy.holders),
                   f"and the refusal identifies the queued writer "
                   f"({busy.holders})")

        reader.stop()
        expect(writer.took_it(5.0),
               "the writer runs once the reader that preceded it drains")
        writer.stop()
        after, after_busy, after_error = taking(
            scratch, shared={"cabal-build"})
        expect(after is not None,
               f"readers enter again after the writer releases "
               f"(busy={after_busy}, error={after_error})")
        if after is not None:
            after.release()
    finally:
        scratch.cleanup()


def test_nested_reader_uses_ancestors_shared_hold_ahead_of_writer() -> None:
    print("\n-- a nested reader enters under its ancestor's shared hold "
          "while a foreign writer waits")
    scratch = Scratch()
    try:
        ancestor = scratch.holder(exclusive={"cabal-build"},
                                  shared={"repo-config"},
                                  purpose="persistence sweep ancestor")
        expect(ancestor.took_it(), "the ancestor holds both resources")
        writer = scratch.waiter(exclusive={"repo-config"},
                                purpose="foreign config writer")
        expect(writer.queued(), "the foreign writer waits for the ancestor")

        env = resources.descendant_hold_env(
            "persistence_contract_sweep", scratch.namespace)
        expect(env.get(resources.ENV_HELD_SHARED) == "repo-config",
               f"the ancestor exports its shared hold ({env!r})")
        exclusive, shared = resources.cross_process_interests(
            "chop", scratch.namespace, env)
        expect(not exclusive and not shared,
               f"the child inherits both holds ({exclusive}, {shared})")
        nested = lock.wait_acquire(
            exclusive=exclusive, shared=shared, namespace=scratch.namespace,
            root=scratch.root, purpose="nested child",
            deadline=time.monotonic() + 0.5, poll=0.01)
        nested.release()
        expect(writer.outcome(0.2).get("state") == "queued",
               "the writer remains queued while the ancestor holds shared")
        ancestor.stop()
        expect(writer.took_it(5.0),
               "the writer acquires when the ancestor and child finish")
        writer.stop()
    finally:
        scratch.cleanup()


def test_conflicting_waiters_run_in_queue_order() -> None:
    print("\n-- conflicting queued writers run oldest first")
    scratch = Scratch()
    try:
        reader = scratch.holder(shared={"cabal-build"})
        expect(reader.took_it(), "an existing reader keeps both writers queued")
        first = scratch.waiter(exclusive={"cabal-build"}, purpose="first")
        expect(first.queued(), "the first writer joined the queue")
        second = scratch.waiter(exclusive={"cabal-build"}, purpose="second")
        expect(second.queued(), "the second writer joined behind it")

        reader.stop()
        expect(first.took_it(5.0), "the first writer acquires first")
        expect(second.outcome(0.2).get("state") == "queued",
               "the second writer remains queued while the first holds")
        first.stop()
        expect(second.took_it(5.0),
               "the second writer acquires after the first releases")
        second.stop()
    finally:
        scratch.cleanup()


def test_a_queue_blocks_only_conflicting_resources() -> None:
    print("\n-- a queued writer does not serialize an unrelated resource")
    scratch = Scratch()
    try:
        reader = scratch.holder(shared={"cabal-build"})
        expect(reader.took_it(), "the existing reader took cabal-build")
        writer = scratch.waiter(exclusive={"cabal-build"})
        expect(writer.queued(), "the cabal-build writer is queued")
        unrelated, busy, error = taking(scratch, shared={"repo-config"})
        expect(unrelated is not None,
               f"unrelated repo-config work still starts "
               f"(busy={busy}, error={error})")
        if unrelated is not None:
            unrelated.release()
        unrelated_waiter = scratch.waiter(exclusive={"repo-config"})
        expect(unrelated_waiter.took_it(5.0),
               "a later non-conflicting queued request also bypasses it")
        unrelated_waiter.stop()
    finally:
        scratch.cleanup()


def test_a_killed_waiter_leaves_no_queue_obstruction() -> None:
    print("\n-- a killed waiter leaves no stale admission entry")
    scratch = Scratch()
    try:
        reader = scratch.holder(shared={"cabal-build"})
        expect(reader.took_it(), "the existing reader took the resource")
        writer = scratch.waiter(exclusive={"cabal-build"})
        expect(writer.queued(), "the writer is queued behind it")
        writer.kill()

        later, busy, error = taking(scratch, shared={"cabal-build"})
        expect(later is not None,
               f"a later reader ignores and reaps the dead waiter "
               f"(busy={busy}, error={error})")
        if later is not None:
            later.release()
    finally:
        scratch.cleanup()


def test_a_timed_out_waiter_withdraws_before_returning() -> None:
    print("\n-- a queued deadline preserves its timeout and leaves no priority")
    scratch = Scratch()
    try:
        reader = scratch.holder(shared={"cabal-build"})
        expect(reader.took_it(), "the existing reader took the resource")
        started = time.monotonic()
        raised = None
        try:
            lock.wait_acquire(
                exclusive={"cabal-build"}, namespace=scratch.namespace,
                root=scratch.root, poll=0.01,
                deadline=time.monotonic() + 0.1)
        except lock.ResourceBusy as busy:
            raised = busy
        elapsed = time.monotonic() - started
        expect(raised is not None,
               "the deadline returns the final resource conflict")
        expect(0.08 <= elapsed < 1.0,
               f"the original bounded-wait behavior is preserved "
               f"({elapsed:.3f} s)")

        later, busy, error = taking(scratch, shared={"cabal-build"})
        expect(later is not None,
               f"the timed-out writer no longer blocks later readers "
               f"(busy={busy}, error={error})")
        if later is not None:
            later.release()
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


def test_a_planted_queue_mutex_is_refused_loudly() -> None:
    print("\n-- an unsafe admission mutex refuses every resource acquisition")
    scratch = Scratch()
    try:
        path = lock.queue_mutex_path(namespace=scratch.namespace,
                                     root=scratch.root)
        target = scratch.root / "planted-queue-target"
        target.write_text("")
        path.symlink_to(target)
        hold, busy, error = taking(scratch, shared={"repo-config"})
        expect(hold is None and error is not None,
               f"a symlinked queue mutex raises ResourceLockError "
               f"(busy={busy})")
        expect(busy is None,
               "and is not downgraded to a transient resource conflict")
        if error is not None:
            expect(str(path) in str(error),
                   f"the diagnostic names the queue mutex ({error})")
    finally:
        scratch.cleanup()


def test_a_failed_wait_withdraws_its_queue_entry() -> None:
    print("\n-- a hard acquisition refusal withdraws the queued request")
    scratch = Scratch()
    try:
        path = lock.lock_path("repo-config", namespace=scratch.namespace,
                              root=scratch.root)
        target = scratch.root / "planted-resource-target"
        target.write_text("")
        path.symlink_to(target)
        raised = None
        try:
            lock.wait_acquire(shared={"repo-config"},
                              namespace=scratch.namespace,
                              root=scratch.root, poll=0.01)
        except lock.ResourceLockError as error:
            raised = error
        expect(raised is not None,
               "the unsafe resource path is still a hard refusal")
        queued = list(scratch.root.glob(
            lock._queue_entry_glob(scratch.namespace)))
        expect(queued == [],
               f"and the failed waiter withdrew its queue entry ({queued})")
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


# --------------------------------------------------------------------------
# The host-wide `gpu-desktop` resource (#2932)
# --------------------------------------------------------------------------
GPU = lock.GPU_DESKTOP
HEADLESS_SHARED = {"repo-config", "cabal-build"}


def _other_checkout(scratch: Scratch) -> str:
    """A namespace no worktree of `scratch`'s repository shares: an independent clone."""
    return f"clone{uuid.uuid4().hex[:12]}"


def test_two_gpu_probes_from_different_checkouts_serialize() -> None:
    print("\n-- gpu-desktop is HOST-wide: GPU probes in independent clones serialize")
    scratch = Scratch()
    try:
        clone = _other_checkout(scratch)
        first = scratch.holder(exclusive={GPU}, shared=HEADLESS_SHARED,
                               purpose="run_probes offscreen")
        expect(first.took_it(), "the first checkout's GPU probe took gpu-desktop")
        hold, busy, error = taking(scratch, exclusive={GPU}, namespace=clone)
        expect(hold is None and busy is not None and error is None,
               f"a second, independent checkout is refused (busy={busy}, error={error})")
        if busy is not None:
            expect(busy.resource == GPU and busy.interest == lock.EXCLUSIVE,
                   f"naming the resource and the refused interest ({busy.resource})")
            expect(busy.namespace == lock.HOST_NAMESPACE,
                   f"in the host namespace, not either repository's ({busy.namespace})")
        # The same repository's other worktrees share the namespace, and are refused too.
        hold, busy, _error = taking(scratch, exclusive={GPU})
        expect(hold is None and busy is not None,
               "and so is another worktree of the first checkout's repository")
        first.stop()
        hold, busy, error = taking(scratch, exclusive={GPU}, namespace=clone)
        expect(hold is not None,
               f"released, the other checkout gets it (busy={busy}, error={error})")
        if hold is not None:
            hold.release()
    finally:
        scratch.cleanup()


def test_a_headless_probe_runs_beside_a_gpu_probe() -> None:
    print("\n-- a headless probe takes no gpu-desktop and runs beside a GPU probe")
    scratch = Scratch()
    try:
        clone = _other_checkout(scratch)
        gpu_probe = scratch.holder(exclusive={GPU}, shared=HEADLESS_SHARED,
                                   purpose="a GPU probe")
        expect(gpu_probe.took_it(), "the GPU probe holds gpu-desktop")
        for label, namespace in (("same repository", None),
                                 ("independent clone", clone)):
            hold, busy, error = taking(scratch, shared=HEADLESS_SHARED,
                                       namespace=namespace)
            expect(hold is not None and busy is None and error is None,
                   f"a headless probe ({label}) is not delayed "
                   f"(busy={busy}, error={error})")
            if hold is not None:
                hold.release()
        # A waiting GPU probe in the queue does not hold headless work back either.
        waiter = scratch.waiter(exclusive={GPU}, shared=HEADLESS_SHARED,
                                purpose="a second GPU probe", namespace=clone)
        expect(waiter.queued(), "a second GPU probe queues")
        for namespace in (None, clone):
            hold, busy, error = taking(scratch, shared=HEADLESS_SHARED,
                                       namespace=namespace)
            expect(hold is not None,
                   f"headless work still starts while a GPU probe waits "
                   f"(busy={busy}, error={error})")
            if hold is not None:
                hold.release()
    finally:
        scratch.cleanup()


def test_a_gpu_waiter_waits_names_the_holder_and_then_runs() -> None:
    print("\n-- a GPU waiter waits (never fails), says who holds it and since when")
    scratch = Scratch()
    try:
        clone = _other_checkout(scratch)
        holder = scratch.holder(exclusive={GPU}, purpose="run_probes offscreen")
        expect(holder.took_it(), "the GPU probe holds gpu-desktop")
        waiter = scratch.waiter(exclusive={GPU}, purpose="a waiting probe",
                                namespace=clone)
        expect(waiter.queued(), "the second checkout's probe waits instead of failing")
        outcome = waiter.outcome()
        named = (outcome.get("busy") or {}).get("holders") or []
        expect(len(named) == 1 and named[0].get("pid") == holder.proc.pid
               and named[0].get("purpose") == "run_probes offscreen"
               and isinstance(named[0].get("acquired"), float),
               f"its announcement carries the holder's pid, purpose and acquisition time ({named})")
        text = lock.ResourceBusy(GPU, lock.EXCLUSIVE, namespace=lock.HOST_NAMESPACE,
                                 holders=named).describe()
        expect(f"pid {holder.proc.pid}" in text and "run_probes offscreen" in text
               and "since " in text and " ago)" in text,
               f"and the printed diagnostic says who, doing what, since when ({text})")
        expect(not waiter.took_it(0.3), "it does not run while the holder lives")
        holder.stop()
        expect(waiter.took_it(), "it runs once the holder is gone")
    finally:
        scratch.cleanup()


def test_a_dead_gpu_holder_is_reclaimed() -> None:
    print("\n-- a SIGKILLed GPU holder owns nothing, host-wide")
    scratch = Scratch()
    try:
        clone = _other_checkout(scratch)
        holder = scratch.holder(exclusive={GPU}, purpose="a doomed probe")
        expect(holder.took_it(), "the holder took gpu-desktop")
        hold, busy, _error = taking(scratch, exclusive={GPU}, namespace=clone)
        expect(hold is None and busy is not None, "it is held before the kill")
        holder.kill()
        hold, busy, error = taking(scratch, exclusive={GPU}, namespace=clone)
        expect(hold is not None, f"reclaimed at once (busy={busy}, error={error})")
        if hold is not None:
            hold.release()
        # Its note is dead weight, not a holder: no live holder is reported.
        expect(lock.holders(GPU, namespace=clone, root=scratch.root) == [],
               "and the dead process's note names no live holder")
    finally:
        scratch.cleanup()


def test_the_host_wide_holder_is_found_from_any_namespace() -> None:
    print("\n-- a host-wide holder is identified from every repository; others are not")
    scratch = Scratch()
    try:
        clone = _other_checkout(scratch)
        holder = scratch.holder(exclusive={GPU}, shared={"repo-config"},
                                purpose="a GPU probe")
        expect(holder.took_it(), "the holder took its set")
        for namespace in (scratch.namespace, clone):
            found = lock.holders(GPU, namespace=namespace, root=scratch.root)
            expect(len(found) == 1 and found[0]["pid"] == holder.proc.pid,
                   f"gpu-desktop's holder is visible from {namespace} ({found})")
        expect(lock.holders("repo-config", namespace=clone, root=scratch.root) == [],
               "a repository-scoped resource is NOT visible from another repository")
        hold, busy, error = taking(scratch, exclusive={"repo-config"}, namespace=clone)
        expect(hold is not None, f"and does not conflict across repositories "
               f"(busy={busy}, error={error})")
        if hold is not None:
            hold.release()
    finally:
        scratch.cleanup()


def test_a_gpu_hold_is_released_after_failure_and_interruption() -> None:
    print("\n-- gpu-desktop is released after an exception and after a context exit")
    scratch = Scratch()
    try:
        clone = _other_checkout(scratch)
        try:
            with lock.acquire(exclusive={GPU}, namespace=scratch.namespace,
                              root=scratch.root):
                raise KeyboardInterrupt
        except KeyboardInterrupt:
            pass
        hold, busy, error = taking(scratch, exclusive={GPU}, namespace=clone)
        expect(hold is not None, f"free after an interrupted probe (busy={busy}, error={error})")
        if hold is not None:
            hold.release()
        # A refused compound set takes nothing, host-wide or not.
        other = scratch.holder(shared={"cabal-build"})
        expect(other.took_it(), "a shared build-state holder")
        hold, busy, _error = taking(scratch, exclusive={GPU, "cabal-build"})
        expect(hold is None and busy is not None, "a set with one conflict is refused whole")
        hold, busy, error = taking(scratch, exclusive={GPU}, namespace=clone)
        expect(hold is not None, f"and gpu-desktop was not left held by the refusal "
               f"(busy={busy}, error={error})")
        if hold is not None:
            hold.release()
    finally:
        scratch.cleanup()


# --------------------------------------------------------------------------
# What takes gpu-desktop: derived from the classification, never listed here
# --------------------------------------------------------------------------
def test_the_gpu_probe_set_is_derived_from_the_registry_classification() -> None:
    print("\n-- the gpu-desktop set is ci_probes' needs-gpu classification, not a second list")
    import ci_probes  # type: ignore
    derived = ci_probes.gpu_probe_keys()
    expect(derived and derived <= ci_probes.ALL_KEYS,
           "the classification names registered probes")
    for key in sorted(ci_probes.ALL_KEYS):
        takes = resources.GPU_RESOURCE in resources.exclusive_resources(key)
        wants = any(r.category == ci_probes.NEEDS_GPU
                    for r in ci_probes.MANUAL_ONLY_REASONS.get(key, ()))
        expect(takes == wants,
               f"{key}: takes gpu-desktop exactly when it is classified needs-gpu "
               f"(takes={takes}, classified={wants})")
        expect(resources.GPU_RESOURCE not in resources.shared_resources(key),
               f"{key}: never holds it shared")
    synthetic = {"made_up": (ci_probes.Reason(ci_probes.NEEDS_GPU, "x"),
                              ci_probes.Reason(ci_probes.FLAKY, "y")),
                 "other": (ci_probes.Reason(ci_probes.SLOW_WORLDGEN, "z"),)}
    expect(ci_probes.gpu_probe_keys(synthetic) == frozenset({"made_up"}),
           "a probe excluded on several grounds is found by the needs-gpu one")
    expect(resources.GPU_RESOURCE in lock.HOST_SCOPED_RESOURCES,
           "and the resource really is host-scoped")


def test_a_runner_exports_the_host_hold_so_a_child_does_not_retake_it() -> None:
    print("\n-- the host-wide hold rides to a probe's children through the environment")
    import ci_probes  # type: ignore
    gpu_key = sorted(ci_probes.gpu_probe_keys())[0]
    plain = next(k for k in sorted(ci_probes.ALL_KEYS)
                 if k not in ci_probes.gpu_probe_keys()
                 and not resources.exclusive_resources(k))
    saved = {name: os.environ.pop(name, None) for name in resources.RUNNER_ENV_VARS}
    try:
        env = resources.descendant_hold_env(gpu_key, "ns")
        expect(env.get(resources.ENV_HELD_HOST) == GPU,
               f"a needs-gpu probe's child is told gpu-desktop is held ({env})")
        expect(GPU not in env.get(resources.ENV_HELD_EXCLUSIVE, ""),
               "and it is not smuggled into the namespace-scoped variable")
        expect(resources.ENV_HELD_HOST not in resources.descendant_hold_env(plain, "ns"),
               "a headless probe's child is told nothing about it")
        expect(resources.ENV_HELD_HOST in resources.RUNNER_ENV_VARS,
               "run_one strips a stale export before supplying its own")
        # An ancestor's host hold survives a nested runner and drops the request.
        probe_env = {resources.ENV_HELD_HOST: GPU}
        exclusive, _shared = resources.cross_process_interests(gpu_key, "ns", probe_env)
        expect(GPU not in exclusive, "an inherited hold removes gpu-desktop from the request")
        exclusive, _shared = resources.cross_process_interests(gpu_key, "ns", {})
        expect(GPU in exclusive, "and without one it is requested")
        # It is matched by name, not namespace: any repository's ancestor covers it.
        expect(resources.inherited_host_resources({resources.ENV_HELD_HOST: f"{GPU},bogus"})
               == {GPU}, "only host-scoped names are honoured from the environment")
    finally:
        for name, value in saved.items():
            if value is not None:
                os.environ[name] = value


# --------------------------------------------------------------------------
# The direct-launch guard: a probe run by hand takes the same hold (#2932)
# --------------------------------------------------------------------------
GUARD_SRC = textwrap.dedent("""\
    import json, os, sys, time
    from pathlib import Path
    sys.path.insert(0, sys.argv[1])
    root, namespace, ready, release = sys.argv[2:6]
    plan = json.loads(sys.argv[6])
    import probe_resource_lock as lock
    lock.LOCK_ROOT = Path(root)            # never the real /tmp locks
    import probe_runner_resources as resources
    resources.RESOURCE_NAMESPACE = namespace
    for name in resources.RUNNER_ENV_VARS:
        os.environ.pop(name, None)
    os.environ.update(plan.get("env", {}))
    import probe_gpu_hold
    report = {"calls": []}
    if plan.get("boot") is not None:
        import probelib, probe_engine
        # The real engine is never built or run: a stand-in that prints READY.
        engine = Path(root) / "stand-in-engine.sh"
        engine.write_text("#!/bin/sh\\necho READY\\nexec sleep 60\\n")
        engine.chmod(0o755)
        prepared = []
        if plan.get("real_prepare"):
            # `prepare_command` is the real one; only the Cabal contact is
            # replaced by what it asks of the lock: an EXCLUSIVE `cabal-build`,
            # taken without waiting so a deadlock shows up as a refusal.
            def prepare_executable(repo_root=None, **kw):
                prepared.append(1)
                lock.acquire(exclusive={"cabal-build"}, namespace=namespace,
                             purpose="stand-in preparation").release()
                return str(engine)
            probe_engine.prepare_executable = prepare_executable
        else:
            probe_engine.prepare_command = lambda args, **kw: [str(engine), *args]
        procs = []
        for index, mode in enumerate(plan["boot"]):
            procs.append(probelib.boot(9999, log=str(Path(root) / "engine.log"),
                                       mode=tuple(mode)))
            report["calls"].append(probe_gpu_hold.holding())
            if plan.get("gate") and index == 0:
                Path(ready + ".mid").write_text("first engine up")
                while not Path(release + ".go").exists():
                    time.sleep(0.02)
        report["prepared"] = len(prepared)
        report["engine_pids"] = [proc.pid for proc in procs]
        if not plan.get("leave_engines"):
            for proc in procs:
                proc.kill()
    for argv in plan.get("launch", []):
        hold = probe_gpu_hold.ensure_for_launch(argv)
        report["calls"].append(hold is not None)
    report["holding"] = probe_gpu_hold.holding()
    Path(ready).write_text(json.dumps(report))
    deadline = time.time() + 120
    while not Path(release).exists() and time.time() < deadline:
        time.sleep(0.02)
""")


class Guard:
    """A direct probe invocation: its own interpreter, the launch guard in play."""

    def __init__(self, scratch: Scratch, plan: dict, namespace=None) -> None:
        self.scratch = scratch
        token = uuid.uuid4().hex[:8]
        self.ready = scratch.root / f"guard-ready-{token}"
        self.release_flag = scratch.root / f"guard-release-{token}"
        script = scratch.root / f"guard-{token}.py"
        script.write_text(GUARD_SRC)
        self.proc = subprocess.Popen(
            [sys.executable, str(script), TOOLS_DIR, str(scratch.root),
             namespace or scratch.namespace, str(self.ready),
             str(self.release_flag), json.dumps(plan)])

    def report(self, seconds: float = 30.0) -> dict:
        deadline = time.monotonic() + seconds
        while time.monotonic() < deadline:
            try:
                return json.loads(self.ready.read_text())
            except (OSError, ValueError):
                time.sleep(0.02)
        return {}

    def stop(self) -> None:
        try:
            self.release_flag.write_text("go")
        except OSError:
            pass
        try:
            self.proc.wait(timeout=30)
        except subprocess.TimeoutExpired:
            self.proc.kill()
            self.proc.wait(timeout=30)


def test_which_engine_launches_use_the_gpu() -> None:
    print("\n-- the guard reads the EFFECTIVE launch arguments, in the engine's precedence")
    import probe_gpu_hold  # type: ignore
    cases = [
        (["--headless", "--port", "9"], False, "headless"),
        (["--dump=terrain", "--seed", "1"], False, "a --dump run"),
        (["--dump", "--offscreen"], False, "--dump beats --offscreen"),
        (["--language-report", "--preview", "x"], False, "--language-report beats --preview"),
        (["--offscreen", "--size", "320x240"], True, "--offscreen"),
        (["--offscreen", "--headless"], True, "--offscreen beats --headless"),
        (["--preview", "units/acolyte"], True, "--preview"),
        (["--port", "9"], True, "no selector is the graphical default"),
        ([], True, "the empty argument list is graphical"),
        (["--port", "--offscreen-check"], True, "only exact flags select a profile; no selector => graphical"),
        (["--headless", "--scenario", "--offscreenx"], False, "a lookalike flag is not --offscreen"),
    ]
    for argv, want, why in cases:
        expect(probe_gpu_hold.engine_uses_gpu(argv) is want, f"{why}: {argv} -> {want}")
    # Headless and dump launches never touch a lock at all.
    expect(probe_gpu_hold.ensure_for_launch(["--headless"]) is None
           and probe_gpu_hold.ensure_for_launch(["--dump=terrain"]) is None,
           "headless and --dump launches return without taking a hold")


def test_a_direct_probe_invocation_takes_the_hold() -> None:
    print("\n-- a probe run by hand takes gpu-desktop for its whole process, across engines")
    scratch = Scratch()
    try:
        clone = _other_checkout(scratch)
        guard = Guard(scratch, {"launch": [["--offscreen"], ["--offscreen"], ["--preview", "x"]]})
        report = guard.report()
        expect(report.get("calls") == [True, False, False] and report.get("holding"),
               f"the first graphical launch takes it; later engines of the same probe reuse it "
               f"without queueing behind themselves ({report})")
        hold, busy, _error = taking(scratch, exclusive={GPU}, namespace=clone)
        expect(hold is None and busy is not None,
               "while it runs, another checkout's GPU probe is refused")
        if busy is not None:
            expect(busy.holders and busy.holders[0].get("pid") == guard.proc.pid,
                   f"and told who holds it ({busy.holders})")
        guard.stop()
        hold, busy, error = taking(scratch, exclusive={GPU}, namespace=clone)
        expect(hold is not None,
               f"the process's exit gives it back (busy={busy}, error={error})")
        if hold is not None:
            hold.release()
        # A killed probe gives it back too.
        doomed = Guard(scratch, {"launch": [["--offscreen"]]})
        expect(doomed.report().get("calls") == [True], "a second probe takes it")
        doomed.proc.kill()
        doomed.proc.wait(timeout=30)
        hold, busy, error = taking(scratch, exclusive={GPU}, namespace=clone)
        expect(hold is not None, f"a killed probe's hold is gone (busy={busy}, error={error})")
        if hold is not None:
            hold.release()
    finally:
        scratch.cleanup()


def test_the_guard_serializes_direct_probes_and_leaves_headless_ones_alone() -> None:
    print("\n-- two direct GPU probes serialize; a direct headless one runs beside them")
    scratch = Scratch()
    try:
        clone = _other_checkout(scratch)
        first = Guard(scratch, {"launch": [["--offscreen"]]})
        expect(first.report().get("calls") == [True], "the first direct probe holds it")
        headless = Guard(scratch, {"launch": [["--headless"], ["--dump=terrain"]]},
                         namespace=clone)
        report = headless.report(10.0)
        expect(report.get("calls") == [False, False] and not report.get("holding"),
               f"a direct headless/--dump probe is not delayed ({report})")
        second = Guard(scratch, {"launch": [["--offscreen"]]}, namespace=clone)
        expect(second.report(1.0) == {},
               "a second direct GPU probe from another checkout WAITS, not fails")
        expect(second.proc.poll() is None, "and is still waiting, alive")
        first.stop()
        expect(second.report().get("calls") == [True],
               "and runs as soon as the first probe exits")
        second.stop()
        headless.stop()
    finally:
        scratch.cleanup()


def test_boot_takes_the_hold_from_the_effective_mode_not_the_probe_name() -> None:
    print("\n-- probelib.boot guards an offscreen boot of ANY probe, registered or not")
    scratch = Scratch()
    try:
        clone = _other_checkout(scratch)
        headless = Guard(scratch, {"boot": [["--headless"]]})
        report = headless.report()
        expect(report.get("calls") == [False],
               f"a --headless boot takes nothing ({report})")
        hold, busy, error = taking(scratch, exclusive={GPU}, namespace=clone)
        expect(hold is not None, f"and leaves gpu-desktop free (busy={busy}, error={error})")
        if hold is not None:
            hold.release()
        headless.stop()
        # Two engines in one probe (the offscreen probe boots two at once), then a third:
        # the probe never queues behind its own hold.
        graphical = Guard(scratch, {"boot": [["--offscreen"], ["--offscreen"], ["--preview", "x"]]})
        report = graphical.report()
        expect(report.get("calls") == [True, True, True],
               f"every graphical boot of one probe proceeds under one hold ({report})")
        hold, busy, _error = taking(scratch, exclusive={GPU}, namespace=clone)
        expect(hold is None and busy is not None, "and the hold is real while it runs")
        graphical.stop()
        # An interactive window (no profile flag at all) is graphical too.
        interactive = Guard(scratch, {"boot": [[]]})
        expect(interactive.report().get("calls") == [True],
               "a boot with no profile selector (audio_manual --interactive) takes it")
        interactive.stop()
    finally:
        scratch.cleanup()


def test_a_runner_launched_probe_inherits_the_hold_instead_of_retaking_it() -> None:
    print("\n-- a probe launched under a holder takes nothing and cannot deadlock on it")
    scratch = Scratch()
    try:
        runner = scratch.holder(exclusive={GPU}, purpose="run_probes offscreen")
        expect(runner.took_it(), "the runner holds gpu-desktop for the probe")
        child = Guard(scratch, {"env": {resources.ENV_HELD_HOST: GPU},
                                "launch": [["--offscreen"], ["--offscreen"]]})
        report = child.report(10.0)
        expect(report.get("calls") == [False, False] and report.get("holding"),
               f"the child sees it as held and never queues behind its own runner ({report})")
        # Without the marker the same child waits on the runner, as any other probe would.
        stranger = Guard(scratch, {"launch": [["--offscreen"]]})
        expect(stranger.report(1.0) == {}, "an unrelated probe waits for the runner's hold")
        runner.stop()
        expect(stranger.report().get("calls") == [True], "and runs once it is released")
        stranger.stop()
        child.stop()
    finally:
        scratch.cleanup()


def test_every_graphical_launch_site_is_guarded() -> None:
    print("\n-- the launch sites that open a window or GPU device all go through the guard")
    tools = Path(TOOLS_DIR)
    boot_src = (tools / "probelib.py").read_text()
    expect(boot_src.index("probe_gpu_hold.ensure_for_launch(")
           > boot_src.index("probe_engine.prepare_command(")
           and boot_src.index("probe_gpu_hold.ensure_for_launch(")
           < boot_src.index("subprocess.Popen(cmd, stdout=logf"),
           "probelib.boot guards after preparing the engine and before launching it")
    entry = (tools / "scenario_entrypoint_probe.py").read_text()
    expect(entry.index("probe_gpu_hold.ensure_for_launch(")
           < entry.index("subprocess.Popen(cmd, cwd=launch"),
           "scenario_entrypoint_probe guards its own launcher (its --offscreen half)")
    # Any other tool that spawns an engine itself with a graphical profile must too.
    for path in sorted(tools.glob("*.py")):
        if path.name.startswith("test_"):
            continue
        text = path.read_text()
        if "subprocess.Popen" not in text or "engine_command" not in text:
            continue
        # These boot an engine through `engine_command` only on paths that fail
        # before a window or device exists (argument validation, an occupied
        # port): they deliberately take no hold.
        expect(path.name in {"probe_engine.py", "debug_console_boot_probe.py",
                             "preview_cli_probe.py", "thermo_altitude_probe.py"}
               or "probe_gpu_hold" in text,
               f"{path.name} launches an engine through engine_command without the GPU guard")


def test_an_exported_host_hold_marks_the_environment_only_while_it_lasts() -> None:
    print("\n-- a holder that exports its host-wide hold restores the environment")
    scratch = Scratch()
    try:
        env: dict = {}
        hold = lock.acquire(exclusive={GPU}, shared={"repo-config"},
                            namespace=scratch.namespace, root=scratch.root)
        with resources.exported_host_hold(hold, env):
            expect(env.get(resources.ENV_HELD_HOST) == GPU,
                   f"the host-wide interest is exported ({env})")
            expect(resources.inherited_host_resources(env) == {GPU},
                   "and read back as inherited by a child")
        expect(resources.ENV_HELD_HOST not in env, "and removed once the block ends")
        hold.release()
        repo_only = lock.acquire(shared={"repo-config"}, namespace=scratch.namespace,
                                 root=scratch.root)
        with resources.exported_host_hold(repo_only, env):
            expect(resources.ENV_HELD_HOST not in env,
                   "a hold with no host-wide interest exports nothing")
        repo_only.release()
        env[resources.ENV_HELD_HOST] = GPU
        other = lock.acquire(shared={"repo-config"}, namespace=scratch.namespace,
                             root=scratch.root)
        with resources.exported_host_hold(other, env):
            pass
        expect(env.get(resources.ENV_HELD_HOST) == GPU,
               "an ancestor's own export is restored, not erased")
        other.release()
    finally:
        scratch.cleanup()


def test_a_second_engine_is_not_prepared_under_the_hold_beside_a_queued_runner() -> None:
    print("\n-- no preparation while holding gpu-desktop: a queued runner cannot wedge a "
          "multi-engine direct probe")
    scratch = Scratch()
    try:
        guard = Guard(scratch, {"boot": [["--offscreen"], ["--offscreen"]],
                                "real_prepare": True, "gate": True})
        mid = Path(str(guard.ready) + ".mid")
        deadline = time.monotonic() + 30
        while not mid.exists() and time.monotonic() < deadline:
            time.sleep(0.02)
        expect(mid.exists(), "the first offscreen engine is up and the probe holds gpu-desktop")
        # A sequential runner now queues for {gpu-desktop exclusive, cabal-build shared}.
        runner = scratch.waiter(exclusive={GPU}, shared={"cabal-build", "repo-config"},
                                purpose="run_probes queued behind the probe")
        expect(runner.queued(), "a runner queues behind the direct probe's hold")
        Path(str(guard.release_flag) + ".go").write_text("go")
        report = guard.report(30.0)
        expect(report.get("calls") == [True, True] and report.get("prepared") == 1,
               f"the second boot proceeds and reuses the engine prepared before the hold, "
               f"instead of asking for cabal-build exclusively behind the queued runner "
               f"({report})")
        guard.stop()
        expect(runner.took_it(), "and the runner runs once the probe is gone")
    finally:
        scratch.cleanup()


def test_the_hold_lasts_as_long_as_the_engine_even_if_the_probe_is_killed() -> None:
    print("\n-- killing the probe does not release gpu-desktop while its engine runs")
    scratch = Scratch()
    try:
        clone = _other_checkout(scratch)
        guard = Guard(scratch, {"boot": [["--offscreen"]], "leave_engines": True})
        report = guard.report()
        pids = report.get("engine_pids") or []
        expect(len(pids) == 1 and report.get("calls") == [True], f"the probe booted an engine ({report})")
        try:
            os.kill(guard.proc.pid, signal.SIGTERM)
            guard.proc.wait(timeout=30)
            expect(guard.proc.returncode is not None, "the probe was terminated without teardown")
            expect(_alive(pids[0]), "its engine outlived it")
            hold, busy, _error = taking(scratch, exclusive={GPU}, namespace=clone)
            expect(hold is None and busy is not None,
                   "and gpu-desktop is STILL held: the engine carries the lock")
            if hold is not None:
                hold.release()
        finally:
            for pid in pids:
                try:
                    os.kill(pid, signal.SIGKILL)
                except OSError:
                    pass
        deadline = time.monotonic() + 30
        while _alive(pids[0]) and time.monotonic() < deadline:
            time.sleep(0.05)
        hold, busy, error = taking(scratch, exclusive={GPU}, namespace=clone)
        expect(hold is not None, f"released once the engine is gone (busy={busy}, error={error})")
        if hold is not None:
            hold.release()
    finally:
        scratch.cleanup()


def _alive(pid: int) -> bool:
    try:
        done = subprocess.run(["ps", "-o", "stat=", "-p", str(pid)], capture_output=True, text=True)
    except OSError:
        return False
    state = done.stdout.strip()
    return bool(state) and not state.startswith("Z")


def main() -> int:
    selftestlib.parse_verbose()
    test_two_shared_holders_coexist()
    test_a_shared_holder_blocks_an_exclusive_acquirer()
    test_an_exclusive_holder_blocks_a_shared_acquirer()
    test_two_exclusive_holders_conflict()
    test_a_killed_holder_owns_nothing()
    test_a_waiting_writer_stops_later_readers_from_barging()
    test_nested_reader_uses_ancestors_shared_hold_ahead_of_writer()
    test_conflicting_waiters_run_in_queue_order()
    test_a_queue_blocks_only_conflicting_resources()
    test_a_killed_waiter_leaves_no_queue_obstruction()
    test_a_timed_out_waiter_withdraws_before_returning()
    test_a_partly_available_set_is_refused_whole()
    test_opposite_orders_cannot_deadlock()
    test_an_empty_interest_set_is_a_hold_that_owns_nothing()
    test_a_late_release_cannot_take_a_successors_resource()
    test_a_lock_file_is_never_unlinked()
    test_linked_worktrees_of_one_repository_share_a_namespace()
    test_a_checkout_git_cannot_answer_for_is_refused()
    test_the_default_root_is_the_host_shared_directory()
    test_a_planted_lock_file_is_refused_loudly()
    test_a_planted_queue_mutex_is_refused_loudly()
    test_a_failed_wait_withdraws_its_queue_entry()
    test_a_non_sticky_scratch_directory_is_refused()
    test_names_are_validated_before_they_become_paths()
    test_an_exclusive_declaration_wins_over_a_shared_one()
    test_holders_is_best_effort_and_says_so()
    test_two_gpu_probes_from_different_checkouts_serialize()
    test_a_headless_probe_runs_beside_a_gpu_probe()
    test_a_gpu_waiter_waits_names_the_holder_and_then_runs()
    test_a_dead_gpu_holder_is_reclaimed()
    test_the_host_wide_holder_is_found_from_any_namespace()
    test_a_gpu_hold_is_released_after_failure_and_interruption()
    test_the_gpu_probe_set_is_derived_from_the_registry_classification()
    test_a_runner_exports_the_host_hold_so_a_child_does_not_retake_it()
    test_which_engine_launches_use_the_gpu()
    test_a_direct_probe_invocation_takes_the_hold()
    test_the_guard_serializes_direct_probes_and_leaves_headless_ones_alone()
    test_boot_takes_the_hold_from_the_effective_mode_not_the_probe_name()
    test_a_runner_launched_probe_inherits_the_hold_instead_of_retaking_it()
    test_every_graphical_launch_site_is_guarded()
    test_an_exported_host_hold_marks_the_environment_only_while_it_lasts()
    test_a_second_engine_is_not_prepared_under_the_hold_beside_a_queued_runner()
    test_the_hold_lasts_as_long_as_the_engine_even_if_the_probe_is_killed()
    if FAILURES:
        print(f"\n{len(FAILURES)} test(s) failed:")
        for failure in FAILURES:
            print(f"  {failure}")
        return selftestlib.concluded(1)
    return selftestlib.concluded(0, "\nAll probe resource lock tests passed")


if __name__ == "__main__":
    raise SystemExit(main())
