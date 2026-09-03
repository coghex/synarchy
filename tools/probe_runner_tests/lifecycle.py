#!/usr/bin/env python3
"""One probe's lifecycle: launch, teardown, liveness and the reap (#2130).

Fourteen groups over `probe_runner_lifecycle`, the owner #2074 split out
of the runner:

  every completion path -- success, nonzero failure with its output
  tail, a clean probe, a timeout escalating to SIGKILL, and a stubborn
  engine killed without charging the probe -- reaps the whole process
  group;
  a runner already shutting down launches no further probe, and a probe
  launched INTO the shutdown window is killed promptly;
  an already-dead group and a zombie-only group are handled without
  spending the grace, and the liveness check does not count a zombie as
  running;
  a direct `run_one` leaves its child on the fallback;
  the engine ledger is durable BEFORE the handshake, so a Ctrl-C in the
  launch window leaves nothing behind;
  the reap returns only once the group is observed gone.

The two shutdown cases drive a `ProbeGroups` subclass that stops the
runner from inside `add`, which is this family's own fixture and stays
here rather than in shared support.
"""
from __future__ import annotations

import os
import signal
import subprocess
import sys
import time
from pathlib import Path

from .support import (
    TEST_GRACE,
    TOOLS_DIR,
    Tree,
    main_with,
    patched,
    pid_alive,
    process_state,
    wait_file,
    wait_pid_gone,
)

import probe_engine  # noqa: E402
import probe_runner_lifecycle  # noqa: E402
from selftestlib import expect  # noqa: E402


# --------------------------------------------------------------------------
# run_one: every completion path reaps the group
# --------------------------------------------------------------------------
def test_success_reaps_the_engine() -> None:
    print("\n-- an ordinary SUCCESS still reaps the probe's engine")
    tree = Tree()
    try:
        script = tree.add("success", exit_code=0)
        with patched(tree):
            ok, timed_out, elapsed, out = probe_runner_lifecycle.run_one(script, None, 120.0)
        pid = tree.engine_pid("success")
        expect(ok is True, "a zero-exit probe is still reported ok")
        expect(timed_out is False, "a zero-exit probe is not reported as timed out")
        expect(pid is not None, "the synthetic engine recorded its pid")
        expect(pid is not None and wait_pid_gone(pid),
               "the engine a passing probe left running is gone once run_one returns")
    finally:
        tree.cleanup()


def test_failure_reaps_the_engine_and_keeps_the_tail() -> None:
    print("\n-- an ordinary NONZERO exit reaps the engine, tail intact")
    tree = Tree()
    try:
        script = tree.add("failure", exit_code=1, tail_lines=30)
        with patched(tree):
            ok, timed_out, elapsed, out = probe_runner_lifecycle.run_one(script, None, 120.0)
        pid = tree.engine_pid("failure")
        expect(ok is False, "a nonzero-exit probe is still reported as failed")
        expect(timed_out is False, "a plain failure is not reported as a timeout")
        expect("diagnostic line 0" in out and "diagnostic line 29" in out,
               "the whole captured output survives teardown, first line to last")
        expect(pid is not None and wait_pid_gone(pid),
               "the engine a FAILING probe left running is gone once run_one returns")
    finally:
        tree.cleanup()


def test_clean_probe_is_unaffected() -> None:
    print("\n-- reaping an already-empty group changes nothing")
    tree = Tree()
    try:
        script = tree.add("clean", exit_code=0, tail_lines=3, descendant=False)
        with patched(tree):
            ok, timed_out, elapsed, out = probe_runner_lifecycle.run_one(script, None, 120.0)
        expect(ok is True, "a probe that tore down its own engine still passes")
        expect("diagnostic line 2" in out, "its output is untouched")
        # The grace is only ever spent on a group that is still alive, so a
        # clean probe must not pay it.
        expect(elapsed < TEST_GRACE,
               f"its elapsed time excludes the teardown grace (got {elapsed:.2f}s)")
    finally:
        tree.cleanup()


def test_timeout_escalates_to_sigkill() -> None:
    print("\n-- a TIMEOUT escalates SIGTERM -> SIGKILL across the whole group")
    tree = Tree()
    try:
        # Both the probe and its engine ignore SIGTERM, so only the
        # escalation can end them.
        script = tree.add("stuck", hang=True, ignore_term=True,
                          engine_ignores_term=True, exit_code=0)
        with patched(tree):
            ok, timed_out, elapsed, out = probe_runner_lifecycle.run_one(script, None, 3.0)
        pid = tree.engine_pid("stuck")
        expect(timed_out is True, "the hung probe is reported as a timeout")
        expect(ok is False, "a timed-out probe is not reported as ok")
        expect(pid is not None and wait_pid_gone(pid),
               "a SIGTERM-ignoring engine is SIGKILLed rather than left running")
    finally:
        tree.cleanup()


def test_stubborn_engine_is_killed_without_charging_the_probe() -> None:
    print("\n-- an engine that ignores SIGTERM is SIGKILLed, off the probe's clock")
    tree = Tree()
    try:
        # The probe exits NORMALLY, so the timeout path never runs: only
        # the after-every-completion reap can end this engine, and only by
        # escalating past the SIGTERM it ignores.
        script = tree.add("stubborn", exit_code=0, engine_ignores_term=True)
        with patched(tree):
            ok, timed_out, elapsed, out = probe_runner_lifecycle.run_one(script, None, 120.0)
        pid = tree.engine_pid("stubborn")
        expect(ok is True, "the probe itself is still reported as passing")
        expect(timed_out is False, "and not as a timeout")
        expect(pid is not None and wait_pid_gone(pid),
               "the SIGTERM-ignoring engine is SIGKILLed after the grace")
        # The correction to requirement 5: teardown may spend the grace,
        # but the probe's recorded elapsed time is the probe's own.
        expect(elapsed < TEST_GRACE,
               f"the grace spent killing it is NOT charged to the probe's "
               f"elapsed time (got {elapsed:.2f}s, grace {TEST_GRACE}s)")
    finally:
        tree.cleanup()


def test_a_stopping_runner_launches_no_further_probe() -> None:
    print("\n-- a shutting-down runner does not launch one more probe")
    # `fut.cancel()` cannot stop a work item a free worker has ALREADY
    # picked up, so the stop flag has to be checked where the process is
    # actually spawned. Asserted here directly: through the executor it
    # would depend on winning a race against cancel().
    tree = Tree()
    try:
        script = tree.add("never_run", exit_code=0)
        groups = probe_runner_lifecycle.ProbeGroups()
        groups.stopping.set()
        with patched(tree):
            ok, timed_out, elapsed, out = probe_runner_lifecycle.run_one(
                script, 9401, 120.0, groups)
        expect(tree.started("never_run") is False,
               "no probe process was spawned at all")
        expect(tree.engine_pid("never_run") is None,
               "and therefore no engine was booted")
        expect(ok is False and timed_out is False,
               "the un-run probe is reported as neither ok nor timed out")
        # NEVER LAUNCHED, not launched-and-killed: the shutdown branch
        # inside run_one would also end this probe, a moment later and
        # after a real Popen, which these two distinguish.
        expect(elapsed == 0.0,
               f"no probe was timed because none ran (got {elapsed!r})")
        expect("not started" in out,
               f"and it reports itself as never started (got {out!r})")
    finally:
        tree.cleanup()


def test_reap_group_on_a_dead_group_is_a_noop() -> None:
    print("\n-- reaping a group that is already gone is success, not an error")
    # A pid nothing can be running under: our own group with every member
    # already reaped is impossible to construct portably, so use a fresh
    # child we have already waited on.
    proc = subprocess.Popen([sys.executable, "-c", "pass"], start_new_session=True)
    proc.wait()
    started = time.monotonic()
    try:
        probe_runner_lifecycle.reap_group(proc.pid, grace=TEST_GRACE)
        raised = None
    except Exception as exc:  # pragma: no cover - the assertion reports it
        raised = exc
    expect(raised is None, f"reap_group on an empty group raises nothing (got {raised!r})")
    expect(time.monotonic() - started < TEST_GRACE,
           "and returns immediately rather than spending the grace period")
    expect(probe_runner_lifecycle._group_alive(proc.pid) is False,
           "_group_alive reports an empty group as gone")


def test_a_direct_run_one_leaves_the_child_on_the_fallback() -> None:
    print("\n-- run_one without a resolved executable hands the child nothing")
    tree = Tree()
    try:
        script = tree.add("bare", exit_code=0)
        with patched(tree):
            # `patched` clears the resolved executable; nothing calls the
            # preflight here. What matters is the STRIPPING: an operator's
            # stale export must not decide which binary the child runs, so
            # a child that was handed nothing is left to prepare its own
            # (#1913) rather than inheriting one nobody resolved.
            os.environ[probe_engine.ENV_ENGINE_EXE] = str(tree.executable)
            try:
                ok, _t, _e, _out = probe_runner_lifecycle.run_one(script, None, 120.0)
            finally:
                os.environ.pop(probe_engine.ENV_ENGINE_EXE, None)
        expect(ok, "the probe still ran")
        expect(tree.engine_exes("bare") == [""],
               f"and saw no runner-supplied executable, so probelib "
               f"prepares its own (got {tree.engine_exes('bare')})")
    finally:
        tree.cleanup()


def test_engine_ledger_is_durable_before_the_handshake() -> None:
    print("\n-- an engine's ledger line is durable before it releases its probe")
    tree = Tree()
    try:
        # The reap fires the instant the probe exits, and the probe exits
        # the instant the handshake file has content -- so anything the
        # engine writes AFTER the handshake races a SIGTERM. This spends
        # three quarters of a second on the safe side of that handshake:
        # with the ledger written first it changes nothing, and if the two
        # writes are ever swapped back the reap lands squarely in this
        # sleep and the ledger loses the line. That is the real Linux CI
        # failure, made deterministic instead of left to a filesystem
        # whose fsync happens to be slow that day.
        tree.add("ledger", exit_code=0, handshake_delay=0.75)
        rc, _ = main_with(tree, ["--only", "ledger", "--exact"])
        pids = tree.engine_pids("ledger")
        expect(rc == 0, f"the probe still passes (got {rc})")
        expect(len(pids) == 1,
               f"its engine recorded itself despite the delayed handshake "
               f"(got {pids})")
        expect(pids == [tree.engine_pid("ledger")],
               f"and the ledger names the same engine the handshake did "
               f"(ledger {pids}, handshake {tree.engine_pid('ledger')})")
        expect(all(wait_pid_gone(pid) for pid in pids),
               "and that engine is still gone once run_one returns")
    finally:
        tree.cleanup()


class StopOnAdd(probe_runner_lifecycle.ProbeGroups):
    """Forces the interleaving a natural run only hits by chance.

    Shutdown begins AFTER run_one's pre-Popen stop check and before
    `reap_all` could have snapshotted the group this call just created --
    so that group is reachable from nothing but run_one itself.

    ``ready_marker`` widens that window rather than changing its shape:
    production constrains only WHERE the flag may be set relative to the
    runner, never how far the probe has got by then, and a descheduled
    worker thread can leave it arbitrarily far along. Waiting until the
    probe is genuinely up is what makes the case discriminating -- signal
    it at t=0 instead and it dies to the default SIGTERM action before it
    has installed the handler under test, so a runner with no escalation
    at all would pass.
    """

    def __init__(self, ready_marker: Path | None = None) -> None:
        super().__init__()
        self._ready = ready_marker

    def add(self, pgid: int) -> None:
        super().add(pgid)
        if self._ready is not None:
            wait_file(self._ready)
        self.stopping.set()


def test_probe_launched_into_shutdown_is_killed_promptly() -> None:
    print("\n-- a probe launched into a starting shutdown is ended, not left to run")
    tree = Tree()
    try:
        # Ignores SIGTERM, so only a real escalation ends it. Without one it
        # would sit in communicate() for the whole --timeout (900 s in
        # production) while the interrupted runner waited on this worker.
        script = tree.add("late", hang=True, ignore_term=True,
                          engine_ignores_term=True)
        started = time.monotonic()
        with patched(tree):
            ok, timed_out, elapsed, out = probe_runner_lifecycle.run_one(
                script, None, 30.0,
                StopOnAdd(tree.root / "late.enginepid"))
        wall = time.monotonic() - started
        pid = tree.engine_pid("late")
        expect(ok is False, "the probe is not reported as ok")
        expect(timed_out is False, "and not as a timeout -- it was cut short")
        expect(wall < 12.0,
               f"run_one returns on the teardown grace, not the --timeout "
               f"(took {wall:.1f}s of a 30s timeout)")
        expect(pid is not None and wait_pid_gone(pid),
               f"the engine it had already booted is gone too (pid {pid})")
    finally:
        tree.cleanup()


def test_ctrl_c_in_the_launch_window_leaves_nothing() -> None:
    print("\n-- a Ctrl-C in the launch window still reaps the probe it spawned")
    tree = Tree()
    try:
        script = tree.add("launch", hang=True)
        launched: dict[str, int] = {}
        real_popen = subprocess.Popen

        def popen_then_interrupt(*a, **kw):
            # Deliver a REAL SIGINT in the exact window the concern names:
            # the probe is spawned, and run_one has not yet recorded its
            # pgid. Undeferred, the KeyboardInterrupt lands here and the
            # Popen object never reaches run_one at all, so nothing can
            # reap the probe or the engine it goes on to boot.
            #
            # ONCE, and only for the probe launch. This patches the shared
            # subprocess module, and the reap itself shells out to `ps`;
            # firing again there would interrupt the teardown being tested
            # and look exactly like the leak this case is checking for.
            proc = real_popen(*a, **kw)
            if not launched:
                launched["pgid"] = proc.pid
                os.kill(os.getpid(), signal.SIGINT)
            return proc

        raised = None
        probe_runner_lifecycle.subprocess.Popen = popen_then_interrupt
        try:
            with patched(tree):
                probe_runner_lifecycle.run_one(script, None, 120.0, probe_runner_lifecycle.ProbeGroups())
        except KeyboardInterrupt:
            raised = "KeyboardInterrupt"
        except BaseException as exc:  # pragma: no cover - reported below
            raised = repr(exc)
        finally:
            probe_runner_lifecycle.subprocess.Popen = real_popen
        expect(raised == "KeyboardInterrupt",
               f"the interrupt still reaches the caller (got {raised})")
        expect("pgid" in launched, "the probe really was spawned")
        pid = launched.get("pgid")
        expect(pid is not None and wait_pid_gone(pid),
               f"the probe process itself is gone (pid {pid})")
        engine = tree.engine_pid("launch")
        expect(engine is None or wait_pid_gone(engine),
               f"and so is anything it managed to boot (pid {engine})")
    finally:
        tree.cleanup()


def test_liveness_check_does_not_count_a_zombie_as_running() -> None:
    print("\n-- this suite's own liveness check reads a zombie as gone, not running")
    # Every "the engine is gone" assertion below rests on pid_alive, and a
    # pid_alive that answered "running" for a dead-but-unreaped process
    # would fail them all (CI, 2026-08-17), while one that answered "gone"
    # for a LIVE process would pass them all vacuously. Both directions are
    # pinned here so the predicate itself is gated rather than trusted.
    zombie = subprocess.Popen([sys.executable, "-c", "pass"])
    try:
        deadline = time.monotonic() + 15.0
        while time.monotonic() < deadline:
            if process_state(zombie.pid) == "Z":
                break
            time.sleep(0.05)
        expect(process_state(zombie.pid) == "Z",
               f"a real zombie was produced and its state is readable "
               f"(got {process_state(zombie.pid)!r})")
        naive_says_alive = True
        try:
            os.kill(zombie.pid, 0)
        except ProcessLookupError:
            naive_says_alive = False
        expect(naive_says_alive,
               "os.kill(pid, 0) still reports it -- so it alone cannot be "
               "the check, which is the CI failure this pins")
        expect(pid_alive(zombie.pid) is False,
               "pid_alive reports the zombie as NOT running")
    finally:
        zombie.wait()
    live = subprocess.Popen([sys.executable, "-c", "import time; time.sleep(60)"])
    try:
        expect(pid_alive(live.pid) is True,
               "and it still reports a genuinely running process as alive")
    finally:
        live.kill()
        live.wait()


def test_reap_returns_only_once_the_group_is_observed_gone() -> None:
    print("\n-- the reap does not return on the SIGKILL, but on the group going")
    # Sending SIGKILL is not the group being gone: delivery and teardown
    # are asynchronous. Asserted on the reap's OWN last observation rather
    # than by racing it, so the case is deterministic.
    tree = Tree()
    try:
        script = tree.add("kill_wait", exit_code=1, engine_ignores_term=True)
        seen: list[bool] = []
        real_running = probe_runner_lifecycle._group_running

        def watched(pgid: int) -> bool:
            answer = real_running(pgid)
            seen.append(answer)
            return answer

        # _group_running is the predicate the reap gates its return on: a
        # zombie is not a running member, so this is what "the group is
        # gone" actually means for a port about to be reused.
        probe_runner_lifecycle._group_running = watched
        try:
            with patched(tree):
                probe_runner_lifecycle.run_one(script, None, 120.0)
        finally:
            probe_runner_lifecycle._group_running = real_running
        expect(bool(seen), "the reap really did inspect the group")
        expect(True in seen,
               "it saw a live member first -- otherwise it reaped nothing and "
               "this case proves nothing")
        expect(seen and seen[-1] is False,
               f"its LAST look saw the group empty before returning "
               f"(observations ended {seen[-3:]})")
    finally:
        tree.cleanup()


def test_group_running_ignores_a_zombie_only_group() -> None:
    print("\n-- a group holding only a zombie does not count as running")
    # This is what lets the reap's waits finish promptly instead of
    # spending the whole grace, and then the post-SIGKILL settle, on an
    # engine that has already exited and released its port. Signals cannot
    # see the difference: a zombie-only group answers EPERM on macOS and
    # succeeds on Linux, so killpg alone calls it alive either way.
    #
    # Tested directly, because the platform decides whether it is even
    # reachable end to end: macOS reaps orphans through launchd almost at
    # once, while a container's PID 1 may never reap at all.
    child = subprocess.Popen([sys.executable, "-c", "pass"],
                             start_new_session=True)
    pgid = child.pid
    try:
        deadline = time.monotonic() + 15.0
        while time.monotonic() < deadline:
            if process_state(pgid) == "Z":
                break
            time.sleep(0.05)
        expect(process_state(pgid) == "Z",
               f"the child is a zombie and nothing has reaped it "
               f"(state {process_state(pgid)!r})")
        expect(probe_runner_lifecycle._group_alive(pgid) is True,
               "signals still report its group -- so the distinction below is "
               "real here, not an artifact of it having already gone")
        expect(probe_runner_lifecycle._group_running(pgid) is False,
               "but nothing in it is RUNNING")
    finally:
        child.wait()
    live = subprocess.Popen([sys.executable, "-c", "import time; time.sleep(60)"],
                            start_new_session=True)
    try:
        expect(probe_runner_lifecycle._group_running(live.pid) is True,
               "while a group with a live member still counts as running")
    finally:
        live.kill()
        live.wait()


#: The nine groups the aggregate runs together at the top of the sweep.
TESTS_LIVENESS_AND_TEARDOWN = (
    test_liveness_check_does_not_count_a_zombie_as_running,
    test_group_running_ignores_a_zombie_only_group,
    test_success_reaps_the_engine,
    test_failure_reaps_the_engine_and_keeps_the_tail,
    test_clean_probe_is_unaffected,
    test_timeout_escalates_to_sigkill,
    test_stubborn_engine_is_killed_without_charging_the_probe,
    test_a_stopping_runner_launches_no_further_probe,
    test_reap_group_on_a_dead_group_is_a_noop,
)

#: The direct `run_one` fallback, which the aggregate runs among the
#: preflight cases rather than beside its siblings here.
TESTS_DIRECT_RUN_ONE = (
    test_a_direct_run_one_leaves_the_child_on_the_fallback,
)

#: The ledger's durability, which the aggregate runs before the
#: scheduler's interruption block that depends on it.
TESTS_ENGINE_LEDGER = (
    test_engine_ledger_is_durable_before_the_handshake,
)

#: The launch window: what a shutdown arriving mid-launch must leave.
TESTS_LAUNCH_WINDOW = (
    test_probe_launched_into_shutdown_is_killed_promptly,
    test_ctrl_c_in_the_launch_window_leaves_nothing,
    test_reap_returns_only_once_the_group_is_observed_gone,
)

#: This family's complete ordered inventory: its fragments, in the order
#: the aggregate runs them, which is also the order `--family lifecycle`
#: runs them in.
TESTS = (TESTS_LIVENESS_AND_TEARDOWN + TESTS_DIRECT_RUN_ONE
         + TESTS_ENGINE_LEDGER + TESTS_LAUNCH_WINDOW)
