#!/usr/bin/env python3
"""Unit tests for run_probes.py's process-group teardown (issue #1323).

Deterministic and GPU-free: every "probe" here is a synthetic script in a
throwaway tree, and every "engine" a synthetic descendant that outlives
it. No Vulkan, no worldgen, no registered probe is ever run. The real
`tools/run_probes.py` is imported and driven, with `REPO_ROOT` and
`PROBES` pointed at the temp tree -- so this exercises the shipped code
paths (`run_one`, `run_with_retry`, `main`) rather than a copy.

The synthetic descendant reproduces exactly what makes the defect
invisible to `communicate()`: `probelib.boot` starts the engine WITHOUT
`start_new_session`, so it inherits the probe's process group, and
redirects its output to a log file rather than the runner's inherited
pipe. The pipe therefore reaches EOF the moment the probe itself exits,
whatever the descendant is still doing.

Covered: ordinary success, ordinary nonzero failure with its output tail
intact, an engine that ignores SIGTERM after an ORDINARY exit (the only
case that reaches the reap's own SIGKILL escalation, and the one that
proves the grace is not charged to the probe's elapsed time), timeout
with the SIGTERM-to-SIGKILL escalation, a shutting-down runner refusing
to launch one more probe, runner interruption (a real SIGINT to a real
runner process), and parallel cancellation.

Usage:
  python3 tools/test_run_probes.py
Exit codes: 0 = all tests passed, 1 = one or more failed.
"""
from __future__ import annotations

import os
import shutil
import signal
import subprocess
import sys
import tempfile
import textwrap
import time
from pathlib import Path

sys.path.insert(0, str(Path(__file__).resolve().parent))
import run_probes  # type: ignore

TOOLS_DIR = str(Path(__file__).resolve().parent)
FAILURES: list[str] = []

# Short enough to keep the suite quick, long enough that a correct
# SIGTERM-then-poll escalation is genuinely exercised rather than skipped.
TEST_GRACE = 1.5


def expect(cond: bool, msg: str) -> None:
    if not cond:
        FAILURES.append(msg)
        print(f"  FAIL: {msg}")
    else:
        print(f"  OK:   {msg}")


# --------------------------------------------------------------------------
# Synthetic tree
# --------------------------------------------------------------------------
DESCENDANT_SRC = textwrap.dedent("""\
    # Stands in for the engine a probe boots: records its pid so the test
    # can check it is gone, then outlives its parent unless signalled.
    import os, signal, sys, time
    pidfile, term_policy = sys.argv[1], sys.argv[2]
    if term_policy == "ignore-term":
        signal.signal(signal.SIGTERM, signal.SIG_IGN)
    with open(pidfile, "w") as fh:
        fh.write(str(os.getpid()))
        fh.flush()
        os.fsync(fh.fileno())
    time.sleep(600)
    """)


def probe_src(root: Path, name: str, *, exit_code: int = 0,
              tail_lines: int = 0, hang: bool = False,
              ignore_term: bool = False, engine_ignores_term: bool = False,
              descendant: bool = True) -> str:
    """A synthetic probe that boots a synthetic 'engine' the way probelib does.

    ``ignore_term`` and ``engine_ignores_term`` are independent on purpose:
    an engine that survives SIGTERM while its probe exits NORMALLY is the
    only thing that reaches `reap_group`'s own SIGKILL escalation, which
    the timeout path's separate escalation would otherwise mask.
    """
    pidfile = root / f"{name}.enginepid"
    startedfile = root / f"{name}.started"
    logfile = root / f"{name}.enginelog"
    term_policy = "ignore-term" if engine_ignores_term else "obey-term"
    lines = [
        "import argparse, os, signal, subprocess, sys, time",
        # run_probes always appends --port in the parallel path; accept it
        # the way every registered probe does (#723).
        "ap = argparse.ArgumentParser()",
        "ap.add_argument('--port', type=int, default=0)",
        "ap.parse_args()",
        f"open({str(startedfile)!r}, 'w').write('1')",
    ]
    if ignore_term:
        lines.append("signal.signal(signal.SIGTERM, signal.SIG_IGN)")
    if descendant:
        lines += [
            # probelib.boot's exact shape: NO new session (so it lands in
            # this probe's group) and output to a log file, NOT the pipe
            # run_probes is reading -- which is why communicate() returns
            # as soon as this probe exits.
            f"log = open({str(logfile)!r}, 'w')",
            f"subprocess.Popen([sys.executable, {str(root / '_descendant.py')!r},"
            f" {str(pidfile)!r}, {term_policy!r}], stdout=log,"
            " stderr=subprocess.STDOUT)",
            # Do not exit before the descendant has recorded its pid, or
            # the test would have nothing to look for.
            "deadline = time.time() + 30",
            "while time.time() < deadline:",
            f"    try:",
            f"        if open({str(pidfile)!r}).read().strip():",
            "            break",
            "    except OSError:",
            "        pass",
            "    time.sleep(0.02)",
        ]
    for i in range(tail_lines):
        lines.append(f"print('diagnostic line {i}')")
    lines.append("sys.stdout.flush()")
    if hang:
        lines.append("time.sleep(600)")
    lines.append(f"sys.exit({exit_code})")
    return "\n".join(lines) + "\n"


class Tree:
    """A throwaway REPO_ROOT holding synthetic probes under tools/."""

    def __init__(self) -> None:
        self.root = Path(tempfile.mkdtemp(prefix="test_run_probes_"))
        (self.root / "tools").mkdir()
        (self.root / "_descendant.py").write_text(DESCENDANT_SRC)
        self.probes: list[tuple[str, str, str]] = []

    def add(self, name: str, **kw) -> str:
        script = f"{name}_probe.py"
        (self.root / "tools" / script).write_text(
            probe_src(self.root, name, **kw))
        self.probes.append((name, script, f"synthetic {name}"))
        return script

    def engine_pid(self, name: str) -> int | None:
        try:
            raw = (self.root / f"{name}.enginepid").read_text().strip()
        except OSError:
            return None
        return int(raw) if raw else None

    def started(self, name: str) -> bool:
        return (self.root / f"{name}.started").exists()

    def cleanup(self) -> None:
        # Belt and braces: never leave a synthetic descendant behind even
        # if an assertion failed before the runner reaped it.
        for name, _, _ in self.probes:
            pid = self.engine_pid(name)
            if pid is not None:
                try:
                    os.kill(pid, signal.SIGKILL)
                except OSError:
                    pass
        shutil.rmtree(self.root, ignore_errors=True)


def pid_alive(pid: int) -> bool:
    try:
        os.kill(pid, 0)
    except ProcessLookupError:
        return False
    except PermissionError:
        return True
    return True


def wait_pid_gone(pid: int, seconds: float = 10.0) -> bool:
    deadline = time.monotonic() + seconds
    while time.monotonic() < deadline:
        if not pid_alive(pid):
            return True
        time.sleep(0.05)
    return not pid_alive(pid)


def wait_file(path: Path, seconds: float = 60.0) -> bool:
    deadline = time.monotonic() + seconds
    while time.monotonic() < deadline:
        if path.exists() and path.read_text().strip():
            return True
        time.sleep(0.05)
    return False


class patched:
    """Point the real runner at the synthetic tree for one test."""

    def __init__(self, tree: Tree, grace: float = TEST_GRACE) -> None:
        self.tree, self.grace = tree, grace

    def __enter__(self):
        self._saved = (run_probes.REPO_ROOT, run_probes.PROBES,
                       run_probes.GROUP_GRACE)
        run_probes.REPO_ROOT = str(self.tree.root)
        run_probes.PROBES = list(self.tree.probes)
        run_probes.GROUP_GRACE = self.grace
        return self

    def __exit__(self, *exc):
        (run_probes.REPO_ROOT, run_probes.PROBES,
         run_probes.GROUP_GRACE) = self._saved
        return False


# --------------------------------------------------------------------------
# run_one: every completion path reaps the group
# --------------------------------------------------------------------------
def test_success_reaps_the_engine() -> None:
    print("\n-- an ordinary SUCCESS still reaps the probe's engine")
    tree = Tree()
    try:
        script = tree.add("success", exit_code=0)
        with patched(tree):
            ok, timed_out, elapsed, out = run_probes.run_one(script, None, 120.0)
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
            ok, timed_out, elapsed, out = run_probes.run_one(script, None, 120.0)
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
            ok, timed_out, elapsed, out = run_probes.run_one(script, None, 120.0)
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
            ok, timed_out, elapsed, out = run_probes.run_one(script, None, 3.0)
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
            ok, timed_out, elapsed, out = run_probes.run_one(script, None, 120.0)
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
        groups = run_probes.ProbeGroups()
        groups.stopping.set()
        with patched(tree):
            ok, timed_out, elapsed, out = run_probes.run_one(
                script, 9401, 120.0, groups)
        expect(tree.started("never_run") is False,
               "no probe process was spawned at all")
        expect(tree.engine_pid("never_run") is None,
               "and therefore no engine was booted")
        expect(ok is False and timed_out is False,
               "the un-run probe is reported as neither ok nor timed out")
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
        run_probes.reap_group(proc.pid, grace=TEST_GRACE)
        raised = None
    except Exception as exc:  # pragma: no cover - the assertion reports it
        raised = exc
    expect(raised is None, f"reap_group on an empty group raises nothing (got {raised!r})")
    expect(time.monotonic() - started < TEST_GRACE,
           "and returns immediately rather than spending the grace period")
    expect(run_probes._group_alive(proc.pid) is False,
           "_group_alive reports an empty group as gone")


# --------------------------------------------------------------------------
# Aggregate behaviour: statuses and the aggregate exit code are unchanged
# --------------------------------------------------------------------------
def _main_with(tree: Tree, argv: list[str]) -> tuple[int, str]:
    import io
    import contextlib
    buf = io.StringIO()
    saved_argv = sys.argv
    sys.argv = ["run_probes.py"] + argv
    try:
        with patched(tree), contextlib.redirect_stdout(buf):
            rc = run_probes.main()
    finally:
        sys.argv = saved_argv
    return rc, buf.getvalue()


def test_aggregate_exit_codes_unchanged() -> None:
    print("\n-- PASS/FAIL reporting and the aggregate exit code are unchanged")
    tree = Tree()
    try:
        tree.add("good", exit_code=0)
        rc, out = _main_with(tree, ["--only", "good", "--exact"])
        expect(rc == 0, f"an all-passing selection still exits 0 (got {rc})")
        expect("PASS" in out, "and reports PASS")
    finally:
        tree.cleanup()

    tree = Tree()
    try:
        tree.add("bad", exit_code=1, tail_lines=5)
        rc, out = _main_with(tree, ["--only", "bad", "--exact"])
        expect(rc == 1, f"a failing selection still exits 1 (got {rc})")
        expect("FAIL" in out, "and reports FAIL")
        expect("diagnostic line 4" in out,
               "and still prints the failing probe's output tail")
    finally:
        tree.cleanup()


def test_retry_reaps_between_attempts() -> None:
    print("\n-- a retry never starts before the previous attempt's group is reaped")
    tree = Tree()
    try:
        tree.add("flaky", exit_code=1)
        rc, out = _main_with(tree, ["--only", "flaky", "--exact", "--retries", "1"])
        pid = tree.engine_pid("flaky")
        expect(rc == 1, f"the probe still fails after its retry (got {rc})")
        expect(out.count("retrying solo") == 1, "exactly one retry was announced")
        expect(pid is not None and wait_pid_gone(pid),
               "no engine from either attempt is left running")
    finally:
        tree.cleanup()


# --------------------------------------------------------------------------
# Interruption: a real SIGINT to a real runner process
# --------------------------------------------------------------------------
DRIVER_SRC = textwrap.dedent("""\
    import sys
    sys.path.insert(0, {tools!r})
    import run_probes
    run_probes.REPO_ROOT = {root!r}
    run_probes.PROBES = {probes!r}
    run_probes.GROUP_GRACE = {grace!r}
    sys.argv = ["run_probes.py"] + {argv!r}
    sys.exit(run_probes.main())
    """)


def _run_driver(tree: Tree, argv: list[str], wait_for: list[str],
                grace: float = TEST_GRACE, exit_budget: float = 60.0):
    """Start the real runner in its own session and SIGINT it mid-run."""
    driver = tree.root / "driver.py"
    driver.write_text(DRIVER_SRC.format(
        tools=TOOLS_DIR, root=str(tree.root), probes=list(tree.probes),
        grace=grace, argv=argv))
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


def main() -> int:
    test_success_reaps_the_engine()
    test_failure_reaps_the_engine_and_keeps_the_tail()
    test_clean_probe_is_unaffected()
    test_timeout_escalates_to_sigkill()
    test_stubborn_engine_is_killed_without_charging_the_probe()
    test_a_stopping_runner_launches_no_further_probe()
    test_reap_group_on_a_dead_group_is_a_noop()
    test_aggregate_exit_codes_unchanged()
    test_retry_reaps_between_attempts()
    test_ctrl_c_leaves_no_engine_behind()
    test_ctrl_c_cancels_queued_parallel_work()
    if FAILURES:
        print(f"\n{len(FAILURES)} test(s) failed:")
        for failure in FAILURES:
            print(f"  {failure}")
        return 1
    print("\nAll run_probes teardown tests passed")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
