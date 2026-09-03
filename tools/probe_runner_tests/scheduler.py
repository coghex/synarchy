#!/usr/bin/env python3
"""Orchestration: aggregate exits, conflicts, retries, Ctrl-C (#2130).

Fourteen groups over `probe_runner_scheduler` and the aggregate command
it drives:

  the aggregate's exit codes and presentation are unchanged;
  a key-specific timeout reaches execution and a parallel retry reuses
  it;
  declared conflicts never overlap, a solo probe waits for work already
  running, and a conflict is released after a failure and after a
  timeout;
  a retry reaps between attempts and can rebind the port a killed engine
  held;
  a real SIGINT to a real runner leaves no engine behind, cancels queued
  parallel work, and starts nothing more mid-submission;
  a two-port probe never takes its neighbour's base;
  and every generated synthetic source compiles, which is where a
  fixture mistake announces itself instead of arriving as sixteen
  unrelated teardown failures.

`DRIVER_SRC` and `run_driver` -- the out-of-process runner a real SIGINT
is delivered to -- are this family's own fixture.
"""
from __future__ import annotations

import os
import signal
import subprocess
import sys
import textwrap
import uuid

from .support import (
    DESCENDANT_SRC,
    TEST_GRACE,
    TOOLS_DIR,
    Tree,
    clear_namespace,
    free_port,
    free_port_span,
    main_with,
    overlaps,
    probe_src,
    progress_lines,
    wait_file,
    wait_pid_gone,
)

from selftestlib import expect  # noqa: E402


def test_the_synthetic_fixtures_are_valid_python() -> None:
    print("\n-- the synthetic probe and engine sources are valid Python")
    # These are generated source strings, and a mistake in one does NOT
    # announce itself. An unescaped newline inside DESCENDANT_SRC once
    # defeated textwrap.dedent, leaving every line indented and the engine
    # unable to start at all: nothing booted, so nothing needed reaping,
    # and the suite reported sixteen "the engine is gone" failures instead
    # of one broken fixture. Compiling them here names that mistake.
    tree = Tree()
    try:
        problems = []
        try:
            compile(DESCENDANT_SRC, "<descendant>", "exec")
        except SyntaxError as exc:
            problems.append(f"DESCENDANT_SRC: {exc}")
        expect(DESCENDANT_SRC.splitlines()[0].startswith("#"),
               "DESCENDANT_SRC really was dedented (first line is flush left)")
        # One of every shape the cases below actually generate.
        variants = {
            "plain": {},
            "failing": {"exit_code": 1, "tail_lines": 3},
            "hanging": {"hang": True, "ignore_term": True},
            "stubborn engine": {"engine_ignores_term": True},
            "no engine": {"descendant": False},
            "port holder": {"hold_port": 9999},
            "dwelling": {"dwell": 0.25, "descendant": False},
            "progress": {"progress": (("phase", "engine A", "build it"),),
                         "tail_lines": 2, "descendant": False},
        }
        for label, kw in variants.items():
            try:
                compile(probe_src(tree.root, "fixture", **kw),
                        f"<probe {label}>", "exec")
            except SyntaxError as exc:
                problems.append(f"probe_src({label}): {exc}")
        expect(not problems, f"every generated source compiles ({problems})")
    finally:
        tree.cleanup()


def test_aggregate_exit_codes_unchanged() -> None:
    print("\n-- PASS/FAIL reporting and the aggregate exit code are unchanged")
    tree = Tree()
    try:
        tree.add("good", exit_code=0)
        rc, out = main_with(tree, ["--only", "good", "--exact"])
        expect(rc == 0, f"an all-passing selection still exits 0 (got {rc})")
        expect("PASS" in out, "and reports PASS")
    finally:
        tree.cleanup()

    tree = Tree()
    try:
        tree.add("bad", exit_code=1, tail_lines=5)
        rc, out = main_with(tree, ["--only", "bad", "--exact"])
        expect(rc == 1, f"a failing selection still exits 1 (got {rc})")
        expect("FAIL" in out, "and reports FAIL")
        expect("diagnostic line 4" in out,
               "and still prints the failing probe's output tail")
    finally:
        tree.cleanup()


def test_key_specific_timeout_and_explicit_override_reach_execution() -> None:
    print("\n-- key-specific defaults reach execution and explicit CLI wins")
    tree = Tree()
    try:
        tree.add("slow", dwell=0.25, descendant=False)
        rc, out = main_with(
            tree, ["--only", "slow", "--exact"],
            timeouts={"slow": 0.05})
        expect(rc == 1 and "TIMEOUT" in out,
               f"the short key-specific default terminates the probe ({out!r})")
        expect("timeout 0.05s" in out,
               f"the effective key-specific budget is reported ({out!r})")
    finally:
        tree.cleanup()

    tree = Tree()
    try:
        tree.add("slow", dwell=0.25, descendant=False)
        rc, out = main_with(
            tree, ["--only", "slow", "--exact", "--timeout", "2"],
            timeouts={"slow": 0.05})
        expect(rc == 0 and "PASS" in out,
               f"an explicit larger budget overrides the default ({out!r})")
        expect("timeout 2s" in out,
               f"the explicit effective budget is reported ({out!r})")
    finally:
        tree.cleanup()


def test_parallel_retry_reuses_the_key_specific_timeout() -> None:
    print("\n-- a parallel attempt and its solo retry share the key budget")
    tree = Tree()
    try:
        tree.add("slow", dwell=0.25, descendant=False)
        rc, out = main_with(
            tree,
            ["--only", "slow", "--exact", "--jobs", "2", "--retries", "1"],
            timeouts={"slow": 0.05})
        expect(rc == 1 and out.count("timeout 0.05s") >= 2,
               f"both attempts report the same key-specific budget ({out!r})")
        expect("solo retry 1/1" in out and "TIMEOUT" in out,
               f"the failed parallel attempt reached its solo retry ({out!r})")
    finally:
        tree.cleanup()


def test_declared_conflicts_never_overlap() -> None:
    print("\n-- --jobs runs an exclusive probe alone, and does not serialize "
          "the probes that declare nothing")
    tree = Tree()
    try:
        # No descendant: this is about SCHEDULING, and an extra process per
        # probe only adds teardown jitter to the windows being compared.
        #
        # DISPATCH ORDER A: both exclusive probes are dispatched before any
        # ordinary one, so this is the direction where the barrier must hold
        # NEW work back. Three jobs against four probes means a scheduler
        # that only serialized the two config declarations against each
        # other would happily run `unrelated_a` and `unrelated_b` alongside
        # `config_migration` -- which is exactly #1444's defect.
        tree.add("config_migration", dwell=0.8, descendant=False)
        tree.add("config_state", dwell=0.8, descendant=False)
        tree.add("unrelated_a", dwell=0.8, descendant=False)
        tree.add("unrelated_b", dwell=0.8, descendant=False)
        rc, out = main_with(tree, ["--jobs", "3"])
        expect(rc == 0, f"every probe still passes (exit {rc})")
        expect("4/4 passed" in out, "and the aggregate summary counts all four")

        # Exactly once each: a naive lock inside the worker would still run
        # them all, so this is the floor the overlap checks build on.
        names = ("config_migration", "config_state", "unrelated_a", "unrelated_b")
        for name in names:
            got = tree.intervals(name)
            expect(len(got) == 1 and got[0][1] is not None,
                   f"{name} ran exactly once and to completion "
                   f"(windows: {got})")
            expect(progress_lines(out, f"{name}_probe.py") == 1,
                   f"and the runner reported {name}'s verdict exactly once")

        migration = tree.window("config_migration")
        state = tree.window("config_state")
        first = tree.window("unrelated_a")
        second = tree.window("unrelated_b")
        expect(not overlaps(migration, state),
               f"the two declared-conflicting probes never overlap "
               f"(migration {migration}, state {state})")
        # Requirement 1: no OTHER probe's engine may boot while either
        # config probe runs, whether or not it declares anything.
        for label, solo in (("config_migration", migration),
                            ("config_state", state)):
            for other_label, other in (("unrelated_a", first),
                                       ("unrelated_b", second)):
                expect(not overlaps(solo, other),
                       f"{other_label} never overlaps {label} "
                       f"({other_label} {other}, {label} {solo})")
        # Requirement 3: the declaration must not cost the undeclared
        # probes their concurrency -- the suite is not serialized wholesale.
        expect(overlaps(first, second),
               f"two probes declaring nothing still run concurrently "
               f"(unrelated_a {first}, unrelated_b {second})")
    finally:
        tree.cleanup()


def test_a_solo_probe_waits_for_work_already_running() -> None:
    print("\n-- an exclusive probe waits for running work, without parking "
          "in a worker slot")
    tree = Tree()
    try:
        # DISPATCH ORDER B, the mirror of the test above: ordinary work is
        # already running when the exclusive probe becomes dispatchable, so
        # this is the direction where the barrier must hold the CONFIG probe
        # back. A scheduler that only blocked new work during a solo probe
        # would let `config_migration` start alongside `unrelated_a` here.
        #
        # Putting it in the MIDDLE of the registry order also proves it
        # yields its worker slot: `unrelated_b` is dispatched behind it,
        # into a two-job run, which can only happen if the blocked probe was
        # skipped rather than submitted and parked on a lock.
        tree.add("unrelated_a", dwell=1.0, descendant=False)
        tree.add("config_migration", dwell=0.5, descendant=False)
        tree.add("unrelated_b", dwell=1.0, descendant=False)
        rc, out = main_with(tree, ["--jobs", "2"])
        expect(rc == 0, f"every probe still passes (exit {rc})")
        expect("3/3 passed" in out, "and the aggregate summary counts all three")

        first = tree.window("unrelated_a")
        migration = tree.window("config_migration")
        second = tree.window("unrelated_b")
        expect(first[1] is not None and second[1] is not None,
               f"both undeclared probes ran to completion "
               f"(unrelated_a {first}, unrelated_b {second})")
        expect(overlaps(first, second),
               f"the blocked probe yielded its slot: the two undeclared "
               f"probes ran concurrently (unrelated_a {first}, "
               f"unrelated_b {second})")
        expect(not overlaps(migration, first) and not overlaps(migration, second),
               f"and the exclusive probe overlapped neither "
               f"(migration {migration}, unrelated_a {first}, "
               f"unrelated_b {second})")
        expect(migration[0] >= max(first[1], second[1]),
               f"it started only after both were reaped "
               f"(migration start {migration[0]}, latest end "
               f"{max(first[1], second[1])})")
    finally:
        tree.cleanup()


def test_conflict_is_released_after_a_failure() -> None:
    print("\n-- a FAILING exclusive probe still releases both interests")
    tree = Tree()
    try:
        tree.add("config_migration", exit_code=1, tail_lines=3,
                 dwell=0.6, descendant=False)
        tree.add("config_state", dwell=0.6, descendant=False)
        # The undeclared probe is here because the barrier it waits on is
        # the SHARED half of the ledger, released by the same code path but
        # counted separately -- a release that dropped only the exclusive
        # set would still pass the two-config-probe check below.
        tree.add("unrelated", dwell=0.6, descendant=False)
        rc, out = main_with(tree, ["--jobs", "3"])
        expect(rc == 1, f"the failing selection still exits 1 (got {rc})")
        expect("FAIL" in out and "PASS" in out,
               "and every probe reports its own verdict")
        expect("2/3 passed" in out,
               "with the aggregate counting the two that passed")
        migration = tree.window("config_migration")
        state = tree.window("config_state")
        other = tree.window("unrelated")
        expect(state[0] >= migration[0] and other[0] >= migration[0],
               "the other probes waited for the failing one")
        expect(not overlaps(migration, state) and not overlaps(migration, other),
               f"and neither overlapped it (migration {migration}, "
               f"state {state}, unrelated {other})")
    finally:
        tree.cleanup()


def test_conflict_is_released_after_a_timeout() -> None:
    print("\n-- a TIMED-OUT exclusive probe still releases both interests")
    tree = Tree()
    try:
        # Hangs until the runner's own --timeout kills it; the other probes
        # can only start after that, which is what the gaps below measure.
        tree.add("config_migration", hang=True, descendant=False)
        tree.add("config_state", dwell=0.3, descendant=False)
        tree.add("unrelated", dwell=0.3, descendant=False)
        rc, out = main_with(tree, ["--jobs", "3", "--timeout", "2"])
        expect(rc == 1, f"the timed-out selection exits 1 (got {rc})")
        expect("TIMEOUT" in out, "the hanging probe is reported as a TIMEOUT")
        expect(progress_lines(out, "config_state_probe.py") == 1
               and progress_lines(out, "unrelated_probe.py") == 1
               and "PASS" in out,
               "and both waiting probes still ran and reported PASS")
        migration = tree.window("config_migration")
        for name in ("config_state", "unrelated"):
            waited = tree.window(name)
            expect(waited[0] - migration[0] >= 1.5,
                   f"{name} started only after the timeout fired, not "
                   f"alongside it (gap {waited[0] - migration[0]:.2f}s of a "
                   f"2s timeout)")
        expect(migration[1] is None,
               "and the hanging probe never completed on its own")
    finally:
        tree.cleanup()


def test_retry_reaps_between_attempts() -> None:
    print("\n-- a retry never starts before the previous attempt's group is reaped")
    tree = Tree()
    try:
        tree.add("flaky", exit_code=1)
        rc, out = main_with(tree, ["--only", "flaky", "--exact", "--retries", "1"])
        pids = tree.engine_pids("flaky")
        expect(rc == 1, f"the probe still fails after its retry (got {rc})")
        expect(out.count("retrying solo") == 1, "exactly one retry was announced")
        expect(len(pids) == 2,
               f"both attempts really booted an engine (got {pids})")
        alive = [pid for pid in pids if not wait_pid_gone(pid)]
        expect(not alive,
               f"no engine from EITHER attempt is left running (alive: {alive})")
    finally:
        tree.cleanup()


# --------------------------------------------------------------------------
# Interruption: a real SIGINT to a real runner process
# --------------------------------------------------------------------------
DRIVER_SRC = textwrap.dedent("""\
    import sys, time
    sys.path.insert(0, {tools!r})
    import probe_engine
    import probe_runner_lifecycle
    import probe_runner_registry
    import probe_runner_resources
    import run_probes
    probe_engine.REPO_ROOT = {root!r}
    probe_runner_registry.PROBES = {probes!r}
    # Like the in-process fixture, a synthetic registry starts with no
    # shipped key-specific timeout declarations.
    probe_runner_registry.PROBE_TIMEOUT_OVERRIDES = {{}}
    probe_runner_lifecycle.GROUP_GRACE = {grace!r}
    # The synthetic tree is not a git checkout, so the cross-process
    # resource namespace (#1436) has to be supplied the same way the
    # in-process `patched` fixture supplies it -- otherwise the runner
    # refuses to start and the interrupt below has nothing to interrupt.
    probe_runner_resources.RESOURCE_NAMESPACE = {namespace!r}
    # ... and the engine-executable preflight (#1570) the same way, for
    # the same reason: the synthetic tree is no Cabal project, so a real
    # freshness build would refuse the run before the interrupt could
    # reach it. One build, one list-bin, both answered here.
    _synthetic_exe = {executable!r}

    def _preflight(argv, cwd=None, capture_output=False, text=False):
        import subprocess as _sp
        out = "" if "build" in tuple(argv) else _synthetic_exe + chr(10)
        return _sp.CompletedProcess(tuple(argv), 0, out, "")

    probe_runner_resources.ENGINE_PREFLIGHT_RUNNER = _preflight
    submit_delay = {submit_delay!r}
    if submit_delay:
        # Widen the SUBMISSION window so an interrupt can land inside it.
        # This slows submission down; it does not change what the runner
        # does with the futures, which is what the test is about.
        import concurrent.futures as _cf
        _real = _cf.ThreadPoolExecutor.submit

        def _slow_submit(self, fn, *a, **kw):
            time.sleep(submit_delay)
            return _real(self, fn, *a, **kw)

        _cf.ThreadPoolExecutor.submit = _slow_submit
    sys.argv = ["run_probes.py"] + {argv!r}
    sys.exit(run_probes.main())
    """)


def _run_driver(tree: Tree, argv: list[str], wait_for: list[str],
                grace: float = TEST_GRACE, exit_budget: float = 60.0,
                submit_delay: float = 0.0):
    """Start the real runner in its own session and SIGINT it mid-run."""
    driver = tree.root / "driver.py"
    namespace = f"selftest{uuid.uuid4().hex[:12]}"
    driver.write_text(DRIVER_SRC.format(
        tools=TOOLS_DIR, root=str(tree.root), probes=list(tree.probes),
        grace=grace, argv=argv, submit_delay=submit_delay,
        namespace=namespace, executable=str(tree.executable)))
    proc = subprocess.Popen(
        [sys.executable, str(driver)],
        stdout=subprocess.PIPE, stderr=subprocess.STDOUT, text=True,
        start_new_session=True)
    # Only interrupt once the probes we mean to catch have really booted
    # their engines -- otherwise the test proves nothing.
    ready = all(wait_file(tree.root / f"{name}.enginepid") for name in wait_for)
    # SIGINT the RUNNER's group only. Its probes are in their own sessions
    # (start_new_session=True), exactly as under a terminal Ctrl-C, so this
    # signal cannot reach them: the runner has to do it itself.
    os.killpg(os.getpgid(proc.pid), signal.SIGINT)
    try:
        out, _ = proc.communicate(timeout=exit_budget)
        rc = proc.returncode
    except subprocess.TimeoutExpired:
        # A runner that does not return from an interrupt is a FAILURE to
        # report, not a suite that hangs: the synthetic probes sleep for
        # ten minutes, so waiting it out proves nothing. rc None then
        # fails the exit-code expectation below.
        os.killpg(os.getpgid(proc.pid), signal.SIGKILL)
        out, _ = proc.communicate(timeout=30)
        rc = None
    # The runner process is gone by here, so nothing it held is still held.
    clear_namespace(namespace)
    return ready, rc, out


def test_ctrl_c_leaves_no_engine_behind() -> None:
    print("\n-- Ctrl-C mid-run terminates the running probe AND its engine")
    tree = Tree()
    try:
        tree.add("interrupted", hang=True)
        ready, rc, out = _run_driver(
            tree, ["--only", "interrupted", "--exact"], ["interrupted"])
        pid = tree.engine_pid("interrupted")
        expect(ready, "the probe booted its engine before the interrupt")
        expect(rc == 130, f"the interrupted runner exits 130 (got {rc})")
        expect(pid is not None and wait_pid_gone(pid),
               "the engine it had booted is gone once the runner exits")
    finally:
        tree.cleanup()


def test_ctrl_c_cancels_queued_parallel_work() -> None:
    print("\n-- Ctrl-C in --jobs mode reaps the running probes and starts no more")
    tree = Tree()
    try:
        for name in ("par_a", "par_b", "par_c", "par_d"):
            tree.add(name, hang=True)
        # jobs=2 with four probes: exactly two occupy the workers (they
        # hang), so the other two can only start if the interrupt fails to
        # stop the queue.
        ready, rc, out = _run_driver(
            tree, ["--jobs", "2"], ["par_a", "par_b"])
        expect(ready, "both concurrent probes booted their engines")
        expect(rc == 130, f"the interrupted parallel runner exits 130 (got {rc})")
        for name in ("par_a", "par_b"):
            pid = tree.engine_pid(name)
            expect(pid is not None and wait_pid_gone(pid),
                   f"{name}'s engine is gone once the runner exits")
        not_started = [n for n in ("par_c", "par_d") if not tree.started(n)]
        expect(len(not_started) == 2,
               f"neither queued probe was launched after the interrupt "
               f"(never started: {not_started})")
        for name in ("par_c", "par_d"):
            pid = tree.engine_pid(name)
            expect(pid is None, f"{name} booted no engine at all")
    finally:
        tree.cleanup()


def test_ctrl_c_during_submission_starts_nothing_more() -> None:
    print("\n-- Ctrl-C DURING future submission launches no further probe")
    tree = Tree()
    names = [f"sub_{i}" for i in range(8)]
    try:
        for name in names:
            tree.add(name, hang=True)
        # Submission is normally instantaneous, so it is slowed here to make
        # the window reachable. Interrupting inside it used to leave the
        # executor's own shutdown(wait=True) to run every future submitted
        # so far -- booting engines after the interrupt.
        ready, rc, out = _run_driver(
            tree, ["--jobs", "2"], ["sub_0"], submit_delay=0.35)
        expect(ready, "the first probe booted its engine while submission continued")
        expect(rc == 130, f"the runner still exits 130 (got {rc})")
        started = [n for n in names if tree.started(n)]
        expect(len(started) < len(names),
               f"submission really was interrupted partway "
               f"(started {len(started)}/{len(names)})")
        leaked = []
        for name in names:
            pid = tree.engine_pid(name)
            if pid is not None and not wait_pid_gone(pid):
                leaked.append(name)
        expect(not leaked,
               f"no engine survives the interrupt (still running: {leaked})")
    finally:
        tree.cleanup()


def test_retry_can_rebind_the_port_a_killed_engine_held() -> None:
    print("\n-- a retry can bind the port a SIGTERM-ignoring engine held")
    tree = Tree()
    try:
        port = free_port()
        # Attempt 1 leaks an engine that ignores SIGTERM and owns `port`.
        # Only a reap that SIGKILLs it AND waits for the port to be
        # released lets attempt 2's engine bind the same port; otherwise
        # the retry hits exactly the #1190 abort this PR is about.
        tree.add("rebind", exit_code=1, engine_ignores_term=True,
                 hold_port=port)
        rc, out = main_with(tree, ["--only", "rebind", "--exact",
                                    "--retries", "1", "--port", str(port)])
        binds = tree.binds("rebind")
        expect(rc == 1, f"the probe still fails on both attempts (got {rc})")
        expect(len(binds) == 2,
               f"both attempts really booted an engine that tried to bind "
               f"(got {binds})")
        expect(binds == ["bound", "bound"],
               f"the retry's engine bound the same port the first one held "
               f"(got {binds})")
    finally:
        tree.cleanup()


def test_a_two_port_probe_never_takes_its_neighbours_base() -> None:
    print("\n-- a two-port probe and its neighbour both bind, concurrently")
    # The #1571 defect, reproduced against real sockets rather than
    # asserted: `wide` binds base and base+1 and holds both; `narrow`
    # binds whatever base it was handed, AFTER `wide` has bound (the
    # delay makes the order deterministic instead of a race).
    tree = Tree()
    try:
        base = free_port_span(4)
        tree.add("wide", bind_span=2, dwell=2.0)
        tree.add("narrow", bind_span=1, bind_delay=0.7)
        rc, out = main_with(tree, ["--only", "wide,narrow", "--exact",
                                    "--jobs", "2", "--retries", "0",
                                    "--port", str(base)],
                             spans={"wide": 2})
        expect(rc == 0, f"both probes passed together (got {rc})\n{out}")
        expect(tree.binds("wide") == ["bound", "bound"],
               f"the two-port probe bound BOTH its ports "
               f"(got {tree.binds('wide')})")
        expect(tree.binds("narrow") == ["bound"],
               f"and its neighbour bound its own, uncontested "
               f"(got {tree.binds('narrow')})")
    finally:
        tree.cleanup()

    # The control: with the span UNDECLARED the allocator is back to
    # stride 1 and the same two probes collide. Without this the test
    # above could pass on a layout that never overlapped anyway.
    tree = Tree()
    try:
        base = free_port_span(4)
        tree.add("wide", bind_span=2, dwell=2.0)
        tree.add("narrow", bind_span=1, bind_delay=0.7)
        rc, out = main_with(tree, ["--only", "wide,narrow", "--exact",
                                    "--jobs", "2", "--retries", "0",
                                    "--port", str(base)],
                             spans={})
        expect(rc == 1,
               f"an undeclared two-port probe really does collide (got {rc})")
        expect(tree.ports("narrow") == [base + 1],
               f"because stride 1 hands the neighbour base+1 "
               f"(got {tree.ports('narrow')})")
        expect(tree.binds("narrow") == ["inuse"],
               f"which the two-port probe is already holding "
               f"(got {tree.binds('narrow')})")
    finally:
        tree.cleanup()


#: The generated sources compile -- first in the aggregate, because a
#: broken fixture invalidates everything after it.
TESTS_FIXTURE_VALIDATION = (
    test_the_synthetic_fixtures_are_valid_python,
)

#: The aggregate command's own exit codes and presentation.
TESTS_AGGREGATE_EXIT = (
    test_aggregate_exit_codes_unchanged,
)

#: Key-specific timeouts, from the registry through to a retry.
TESTS_KEY_TIMEOUTS = (
    test_key_specific_timeout_and_explicit_override_reach_execution,
    test_parallel_retry_reuses_the_key_specific_timeout,
)

#: In-process conflict scheduling and release.
TESTS_RESOURCE_SCHEDULING = (
    test_declared_conflicts_never_overlap,
    test_a_solo_probe_waits_for_work_already_running,
    test_conflict_is_released_after_a_failure,
    test_conflict_is_released_after_a_timeout,
)

#: Teardown between retry attempts.
TESTS_RETRY_TEARDOWN = (
    test_retry_reaps_between_attempts,
)

#: A real SIGINT to a real runner process.
TESTS_INTERRUPTION = (
    test_ctrl_c_leaves_no_engine_behind,
    test_ctrl_c_cancels_queued_parallel_work,
    test_ctrl_c_during_submission_starts_nothing_more,
)

#: A retry rebinding the port its killed engine held.
TESTS_PORT_REBINDING = (
    test_retry_can_rebind_the_port_a_killed_engine_held,
)

#: Live allocation beside a two-port neighbour.
TESTS_NEIGHBOUR_ALLOCATION = (
    test_a_two_port_probe_never_takes_its_neighbours_base,
)

#: This family's complete ordered inventory: its fragments, in the order
#: the aggregate runs them, which is also the order `--family scheduler`
#: runs them in.
TESTS = (TESTS_FIXTURE_VALIDATION + TESTS_AGGREGATE_EXIT
         + TESTS_KEY_TIMEOUTS + TESTS_RESOURCE_SCHEDULING
         + TESTS_RETRY_TEARDOWN + TESTS_INTERRUPTION
         + TESTS_PORT_REBINDING + TESTS_NEIGHBOUR_ALLOCATION)
