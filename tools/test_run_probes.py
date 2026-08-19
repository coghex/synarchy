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
runner process), parallel cancellation, an interrupt landing DURING
future submission, a probe launched into an already-starting shutdown,
an interrupt landing in the launch window itself, and a retry rebinding
the port a SIGKILLed engine had held. Also covers `select`/`main`'s
`--exact` unknown-key rejection (issue #1321): a mixed valid/invalid
request, the pre-existing all-invalid empty-selection diagnostic, a
wholly valid request's registry-order and duplicate-collapse behavior,
and substring selection's unchanged permissiveness. And the parallel
scheduler's `EXCLUSIVE_RESOURCES` serialization (issue #1322): probes stamp
their own occupancy windows, so the two config declarations are proved never
to overlap while an undeclared probe still overlaps one of them, across a
conflicting probe that passes, one that fails, and one that times out.

Usage:
  python3 tools/test_run_probes.py
Exit codes: 0 = all tests passed, 1 = one or more failed.
"""
from __future__ import annotations

import os
import shutil
import signal
import socket
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
    #
    # Given a port it also BINDS it, the way a real engine's debug console
    # does, and appends whether the bind succeeded to a shared log. That
    # log is how a retry proves the port was genuinely released rather
    # than merely signalled -- #1190 aborts a boot that cannot bind.
    import os, signal, socket, sys, time
    pidfile, term_policy = sys.argv[1], sys.argv[2]
    port = int(sys.argv[3]) if len(sys.argv) > 3 else 0
    bindlog = sys.argv[4] if len(sys.argv) > 4 else None
    if term_policy == "ignore-term":
        signal.signal(signal.SIGTERM, signal.SIG_IGN)
    held = None
    if port:
        held = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
        try:
            held.bind(("127.0.0.1", port))
            held.listen(4)
            outcome = "bound"
        except OSError:
            outcome = "inuse"
        if bindlog:
            with open(bindlog, "a") as fh:
                print(outcome, file=fh)
                fh.flush()
                os.fsync(fh.fileno())
    with open(pidfile, "w") as fh:
        fh.write(str(os.getpid()))
        fh.flush()
        os.fsync(fh.fileno())
    with open(pidfile + ".all", "a") as fh:
        print(os.getpid(), file=fh)
        fh.flush()
        os.fsync(fh.fileno())
    time.sleep(600)
    """)


def probe_src(root: Path, name: str, *, exit_code: int = 0,
              tail_lines: int = 0, hang: bool = False,
              ignore_term: bool = False, engine_ignores_term: bool = False,
              descendant: bool = True, hold_port: int = 0,
              dwell: float = 0.0) -> str:
    """A synthetic probe that boots a synthetic 'engine' the way probelib does.

    ``ignore_term`` and ``engine_ignores_term`` are independent on purpose:
    an engine that survives SIGTERM while its probe exits NORMALLY is the
    only thing that reaches `reap_group`'s own SIGKILL escalation, which
    the timeout path's separate escalation would otherwise mask.

    ``dwell`` holds the probe alive for that many seconds, and every probe
    stamps ``start``/``end`` wall-clock times into ``<name>.interval`` — a
    measurable occupancy window, which is what lets the scheduler tests
    (#1322) prove which probes overlapped rather than merely that each ran.
    A probe killed before it finishes records only its ``start``.
    """
    pidfile = root / f"{name}.enginepid"
    startedfile = root / f"{name}.started"
    logfile = root / f"{name}.enginelog"
    interval = root / f"{name}.interval"
    term_policy = "ignore-term" if engine_ignores_term else "obey-term"
    lines = [
        "import argparse, os, signal, subprocess, sys, time",
        # run_probes always appends --port in the parallel path; accept it
        # the way every registered probe does (#723).
        "ap = argparse.ArgumentParser()",
        "ap.add_argument('--port', type=int, default=0)",
        "ap.parse_args()",
        f"open({str(startedfile)!r}, 'w').write('1')",
        # Appended, not truncated: --retries reuses these paths, so an
        # attempt count is readable from the file too.
        f"_iv = open({str(interval)!r}, 'a')",
        "def _stamp(kind):",
        "    print(kind, repr(time.time()), file=_iv)",
        "    _iv.flush()",
        "    os.fsync(_iv.fileno())",
        "_stamp('start')",
    ]
    if ignore_term:
        lines.append("signal.signal(signal.SIGTERM, signal.SIG_IGN)")
    if descendant:
        lines += [
            # probelib.boot's exact shape: NO new session (so it lands in
            # this probe's group) and output to a log file, NOT the pipe
            # run_probes is reading -- which is why communicate() returns
            # as soon as this probe exits.
            # Clear the previous attempt's marker first: `--retries` reuses
            # these paths, and waiting on a STALE pidfile would let this
            # probe exit before its own engine had started.
            f"try:\n    os.unlink({str(pidfile)!r})\nexcept OSError:\n    pass",
            f"log = open({str(logfile)!r}, 'w')",
            f"subprocess.Popen([sys.executable, {str(root / '_descendant.py')!r},"
            f" {str(pidfile)!r}, {term_policy!r}, {str(hold_port)!r},"
            f" {str(root / (name + '.binds'))!r}], stdout=log,"
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
    if dwell:
        lines.append(f"time.sleep({dwell!r})")
    if hang:
        lines.append("time.sleep(600)")
    lines.append("_stamp('end')")
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

    def engine_pids(self, name: str) -> list[int]:
        """Every engine pid this probe booted, one per attempt."""
        try:
            raw = (self.root / f"{name}.enginepid.all").read_text()
        except OSError:
            return []
        return [int(tok) for tok in raw.split() if tok.isdigit()]

    def intervals(self, name: str) -> list[tuple[float, float | None]]:
        """This probe's occupancy windows, one per attempt, in start order.

        The end is None for an attempt killed before it could stamp one
        (a timeout, or an interrupt) — the window is still open-ended, not
        absent, so a caller can still order it against another probe.
        """
        try:
            raw = (self.root / f"{name}.interval").read_text()
        except OSError:
            return []
        windows: list[list[float | None]] = []
        for line in raw.splitlines():
            parts = line.split()
            if len(parts) != 2:
                continue
            kind, stamp = parts[0], float(parts[1])
            if kind == "start":
                windows.append([stamp, None])
            elif kind == "end" and windows:
                windows[-1][1] = stamp
        return [(w[0], w[1]) for w in windows]

    def window(self, name: str) -> tuple[float, float | None]:
        """This probe's single occupancy window; raises if it did not run once."""
        got = self.intervals(name)
        assert len(got) == 1, f"{name} recorded {len(got)} window(s), expected 1"
        return got[0]

    def binds(self, name: str) -> list[str]:
        """One entry per attempt: "bound" or "inuse"."""
        try:
            raw = (self.root / f"{name}.binds").read_text()
        except OSError:
            return []
        return raw.split()

    def cleanup(self) -> None:
        # Belt and braces: never leave a synthetic descendant behind even
        # if an assertion failed before the runner reaped it.
        for name, _, _ in self.probes:
            for pid in self.engine_pids(name) or ([self.engine_pid(name)]
                                                  if self.engine_pid(name) else []):
                try:
                    os.kill(pid, signal.SIGKILL)
                except OSError:
                    pass
        shutil.rmtree(self.root, ignore_errors=True)


def process_state(pid: int) -> str | None:
    """The one-letter process state, or None when it cannot be read.

    Linux publishes it in /proc/<pid>/stat. That field follows the comm,
    which is parenthesized and may itself contain spaces and ')', so it is
    read from the LAST ')' rather than by splitting the whole line. macOS
    has no /proc, so `ps` answers there.
    """
    try:
        with open(f"/proc/{pid}/stat") as fh:
            raw = fh.read()
        return raw[raw.rindex(")") + 1:].split()[0]
    except (OSError, ValueError, IndexError):
        pass
    try:
        done = subprocess.run(["ps", "-o", "state=", "-p", str(pid)],
                              capture_output=True, text=True, timeout=15)
    except (OSError, subprocess.SubprocessError):
        return None
    state = done.stdout.strip()
    return state[0] if state else None


def pid_alive(pid: int) -> bool:
    """True while `pid` names a process that has not yet exited.

    A ZOMBIE does not count. It has already died and is only waiting for a
    parent that will never wait() for it -- which is precisely the state a
    correctly killed synthetic engine ends up in here, because it is
    orphaned the moment its probe exits. `os.kill(pid, 0)` cannot tell the
    two apart and reports a zombie as living, so on its own it fails every
    "the engine is gone" assertion in this suite wherever orphans are not
    reaped promptly: green on macOS, where launchd reaps at once, and red
    in a CI container whose PID 1 does not reap at all.

    An unreadable state is treated as alive, so a broken `process_state`
    fails these assertions rather than passing them vacuously.
    """
    try:
        os.kill(pid, 0)
    except ProcessLookupError:
        return False
    except PermissionError:
        return True
    return process_state(pid) != "Z"


def wait_pid_gone(pid: int, seconds: float = 15.0) -> bool:
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
        with patched(tree), contextlib.redirect_stdout(buf), \
             contextlib.redirect_stderr(buf):
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


# --------------------------------------------------------------------------
# Parallel scheduling honours the EXCLUSIVE_RESOURCES declaration (#1322)
#
# These drive the SHIPPED `run_probes.EXCLUSIVE_RESOURCES` -- only `PROBES`
# and `REPO_ROOT` are redirected at the synthetic tree, and the synthetic
# probes are deliberately named `config_migration` and `config_state` so the
# real declaration is what decides. A test-local conflict table would prove
# the mechanism while leaving the two probes the issue is about unguarded.
# --------------------------------------------------------------------------
def overlaps(a: tuple[float, float | None],
             b: tuple[float, float | None]) -> bool:
    """True when two occupancy windows share any instant.

    An open-ended window (a probe killed before it stamped its end) is
    treated as running until now, which is the conservative reading: it
    can only make a missed overlap MORE likely to be reported, never less.
    """
    a_end = a[1] if a[1] is not None else time.time()
    b_end = b[1] if b[1] is not None else time.time()
    return a[0] < b_end and b[0] < a_end


def progress_lines(out: str, script: str) -> int:
    """How many times the runner announced a verdict for `script`."""
    return sum(1 for line in out.splitlines()
               if line.lstrip().startswith("[") and f" {script} ... " in line)


def test_declared_conflicts_never_overlap() -> None:
    print("\n-- --jobs never overlaps the two config probes, and does not "
          "serialize anything else")
    tree = Tree()
    try:
        # No descendant: this is about SCHEDULING, and an extra process per
        # probe only adds teardown jitter to the windows being compared.
        #
        # Three probes into TWO jobs, with the conflicting pair adjacent, is
        # the discriminating shape. A scheduler that submitted everything and
        # took a lock INSIDE the worker would park `config_state` in the
        # second slot for the whole of `config_migration`, and `unrelated`
        # could not start until one of them finished -- so the overlap
        # asserted at the end fails for it exactly as it does for a runner
        # with no conflict handling at all.
        tree.add("config_migration", dwell=1.0, descendant=False)
        tree.add("config_state", dwell=1.0, descendant=False)
        tree.add("unrelated", dwell=2.5, descendant=False)
        rc, out = _main_with(tree, ["--jobs", "2"])
        expect(rc == 0, f"every probe still passes (exit {rc})")
        expect("3/3 passed" in out, "and the aggregate summary counts all three")

        # Exactly once each: a naive lock inside the worker would still run
        # them all, so this is the floor the overlap checks build on.
        for name in ("config_migration", "config_state", "unrelated"):
            got = tree.intervals(name)
            expect(len(got) == 1 and got[0][1] is not None,
                   f"{name} ran exactly once and to completion "
                   f"(windows: {got})")
            expect(progress_lines(out, f"{name}_probe.py") == 1,
                   f"and the runner reported its verdict exactly once")

        migration = tree.window("config_migration")
        state = tree.window("config_state")
        other = tree.window("unrelated")
        expect(not overlaps(migration, state),
               f"the two declared-conflicting probes never overlap "
               f"(migration {migration}, state {state})")
        # Requirement 3: the declaration must not cost unrelated probes
        # their concurrency, and a blocked probe must not hold a worker
        # slot. `unrelated` is third in a two-job run, so it can only
        # overlap `config_migration` if `config_state` was held back
        # WITHOUT being submitted.
        expect(overlaps(other, migration),
               f"a blocked probe yields its slot: the undeclared probe runs "
               f"concurrently with the first (unrelated {other}, "
               f"migration {migration})")
    finally:
        tree.cleanup()


def test_conflict_is_released_after_a_failure() -> None:
    print("\n-- a FAILING conflicting probe still releases its resource")
    tree = Tree()
    try:
        tree.add("config_migration", exit_code=1, tail_lines=3,
                 dwell=0.6, descendant=False)
        tree.add("config_state", dwell=0.6, descendant=False)
        rc, out = _main_with(tree, ["--jobs", "2"])
        expect(rc == 1, f"the failing selection still exits 1 (got {rc})")
        expect("FAIL" in out and "PASS" in out,
               "and both probes report their own verdict")
        migration = tree.window("config_migration")
        state = tree.window("config_state")
        expect(state[0] >= migration[0],
               "the second probe waited for the first")
        expect(not overlaps(migration, state),
               f"and never overlapped it (migration {migration}, "
               f"state {state})")
    finally:
        tree.cleanup()


def test_conflict_is_released_after_a_timeout() -> None:
    print("\n-- a TIMED-OUT conflicting probe still releases its resource")
    tree = Tree()
    try:
        # Hangs until the runner's own --timeout kills it; the second probe
        # can only start after that, which is what the gap below measures.
        tree.add("config_migration", hang=True, descendant=False)
        tree.add("config_state", dwell=0.3, descendant=False)
        rc, out = _main_with(tree, ["--jobs", "2", "--timeout", "2"])
        expect(rc == 1, f"the timed-out selection exits 1 (got {rc})")
        expect("TIMEOUT" in out, "the hanging probe is reported as a TIMEOUT")
        expect(progress_lines(out, "config_state_probe.py") == 1
               and "PASS" in out,
               "and the conflicting probe still ran and reported PASS")
        migration = tree.window("config_migration")
        state = tree.window("config_state")
        expect(state[0] - migration[0] >= 1.5,
               f"it started only after the timeout fired, not alongside it "
               f"(gap {state[0] - migration[0]:.2f}s of a 2s timeout)")
        expect(migration[1] is None,
               "and the hanging probe never completed on its own")
    finally:
        tree.cleanup()


def test_exclusive_resource_declaration_is_data_about_real_probes() -> None:
    print("\n-- the shipped EXCLUSIVE_RESOURCES table names registered probes")
    known = {p[0] for p in run_probes.PROBES}
    unknown = sorted(k for k in run_probes.EXCLUSIVE_RESOURCES if k not in known)
    expect(not unknown,
           f"every declared key names a registered probe (unknown: {unknown})")
    empty = sorted(k for k, v in run_probes.EXCLUSIVE_RESOURCES.items() if not v)
    expect(not empty,
           f"and every declaration names at least one resource (empty: {empty})")
    both = (run_probes.exclusive_resources("config_migration")
            & run_probes.exclusive_resources("config_state"))
    expect(bool(both),
           f"the two config probes still declare an intersecting resource "
           f"(shared: {sorted(both)})")
    expect(not run_probes.exclusive_resources("combat_anim"),
           "an undeclared probe needs nothing exclusively")


# --------------------------------------------------------------------------
# Exact selection: unknown keys are rejected rather than silently dropped
# (#1321)
# --------------------------------------------------------------------------
def test_exact_mixed_selection_is_rejected_before_listing() -> None:
    print("\n-- --exact + --list with unknown keys alongside a valid one is "
          "rejected, not partially listed")
    tree = Tree()
    try:
        tree.add("good", exit_code=0)
        rc, out = _main_with(
            tree, ["--only", "good,not_a_probe,also_bad", "--exact", "--list"])
        expect(rc != 0,
               f"a mixed valid/invalid --exact selection must fail (got {rc})")
        expect("not_a_probe" in out and "also_bad" in out,
               f"the diagnostic names every unknown key, got: {out!r}")
        expect("good_probe.py" not in out,
               f"no partial listing of the valid probe leaks through, got: {out!r}")
    finally:
        tree.cleanup()


def test_exact_mixed_selection_never_runs_the_valid_probe() -> None:
    print("\n-- the same rejection happens before RUNNING anything, not just listing")
    tree = Tree()
    try:
        tree.add("good", exit_code=0)
        rc, out = _main_with(tree, ["--only", "good,not_a_probe", "--exact"])
        expect(rc != 0, f"the mixed selection is rejected (got {rc})")
        expect(not tree.started("good"), "the valid probe never actually started")
    finally:
        tree.cleanup()


def test_exact_all_invalid_selection_keeps_existing_diagnostic() -> None:
    print("\n-- an all-invalid --exact selection keeps the pre-existing "
          "empty-selection error and exit code")
    tree = Tree()
    try:
        tree.add("good", exit_code=0)
        rc, out = _main_with(tree, ["--only", "not_a_probe", "--exact", "--list"])
        expect(rc == 2, f"an all-invalid --exact selection still exits 2 (got {rc})")
        expect("matched no probes" in out,
               f"and keeps the existing 'matched no probes' diagnostic, got: {out!r}")
    finally:
        tree.cleanup()


def test_exact_all_valid_selection_is_unaffected() -> None:
    print("\n-- a wholly valid --exact selection lists in registry order, unchanged")
    tree = Tree()
    try:
        tree.add("alpha", exit_code=0)
        tree.add("beta", exit_code=0)
        rc, out = _main_with(tree, ["--only", "beta,alpha", "--exact", "--list"])
        expect(rc == 0, f"a wholly valid --exact selection still exits 0 (got {rc})")
        expect(out.index("alpha_probe.py") < out.index("beta_probe.py"),
               f"registry order survives regardless of request order, got: {out!r}")
    finally:
        tree.cleanup()


def test_exact_duplicate_valid_keys_still_collapse() -> None:
    print("\n-- a wholly valid --exact selection with a duplicated key lists it once")
    tree = Tree()
    try:
        tree.add("alpha", exit_code=0)
        rc, out = _main_with(tree, ["--only", "alpha,alpha", "--exact", "--list"])
        expect(rc == 0, f"still exits 0 (got {rc})")
        expect(out.count("alpha_probe.py") == 1,
               f"a duplicated valid key is listed exactly once, got: {out!r}")
    finally:
        tree.cleanup()


def test_substring_selection_stays_permissive() -> None:
    print("\n-- substring (non --exact) selection still ignores an unmatched needle")
    tree = Tree()
    try:
        tree.add("craft", exit_code=0)
        rc, out = _main_with(tree, ["--only", "craft,not_a_probe", "--list"])
        expect(rc == 0,
               f"substring selection with one unmatched needle still succeeds (got {rc})")
        expect("craft_probe.py" in out, "the matching probe is still listed")
    finally:
        tree.cleanup()


def test_retry_reaps_between_attempts() -> None:
    print("\n-- a retry never starts before the previous attempt's group is reaped")
    tree = Tree()
    try:
        tree.add("flaky", exit_code=1)
        rc, out = _main_with(tree, ["--only", "flaky", "--exact", "--retries", "1"])
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
    import run_probes
    run_probes.REPO_ROOT = {root!r}
    run_probes.PROBES = {probes!r}
    run_probes.GROUP_GRACE = {grace!r}
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
    driver.write_text(DRIVER_SRC.format(
        tools=TOOLS_DIR, root=str(tree.root), probes=list(tree.probes),
        grace=grace, argv=argv, submit_delay=submit_delay))
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


class StopOnAdd(run_probes.ProbeGroups):
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
            ok, timed_out, elapsed, out = run_probes.run_one(
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
        run_probes.subprocess.Popen = popen_then_interrupt
        try:
            with patched(tree):
                run_probes.run_one(script, None, 120.0, run_probes.ProbeGroups())
        except KeyboardInterrupt:
            raised = "KeyboardInterrupt"
        except BaseException as exc:  # pragma: no cover - reported below
            raised = repr(exc)
        finally:
            run_probes.subprocess.Popen = real_popen
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


def free_port() -> int:
    """A port nothing is listening on right now."""
    with socket.socket(socket.AF_INET, socket.SOCK_STREAM) as probe:
        probe.bind(("127.0.0.1", 0))
        return probe.getsockname()[1]


def test_reap_returns_only_once_the_group_is_observed_gone() -> None:
    print("\n-- the reap does not return on the SIGKILL, but on the group going")
    # Sending SIGKILL is not the group being gone: delivery and teardown
    # are asynchronous. Asserted on the reap's OWN last observation rather
    # than by racing it, so the case is deterministic.
    tree = Tree()
    try:
        script = tree.add("kill_wait", exit_code=1, engine_ignores_term=True)
        seen: list[bool] = []
        real_running = run_probes._group_running

        def watched(pgid: int) -> bool:
            answer = real_running(pgid)
            seen.append(answer)
            return answer

        # _group_running is the predicate the reap gates its return on: a
        # zombie is not a running member, so this is what "the group is
        # gone" actually means for a port about to be reused.
        run_probes._group_running = watched
        try:
            with patched(tree):
                run_probes.run_one(script, None, 120.0)
        finally:
            run_probes._group_running = real_running
        expect(bool(seen), "the reap really did inspect the group")
        expect(True in seen,
               "it saw a live member first -- otherwise it reaped nothing and "
               "this case proves nothing")
        expect(seen and seen[-1] is False,
               f"its LAST look saw the group empty before returning "
               f"(observations ended {seen[-3:]})")
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
        rc, out = _main_with(tree, ["--only", "rebind", "--exact",
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
        expect(run_probes._group_alive(pgid) is True,
               "signals still report its group -- so the distinction below is "
               "real here, not an artifact of it having already gone")
        expect(run_probes._group_running(pgid) is False,
               "but nothing in it is RUNNING")
    finally:
        child.wait()
    live = subprocess.Popen([sys.executable, "-c", "import time; time.sleep(60)"],
                            start_new_session=True)
    try:
        expect(run_probes._group_running(live.pid) is True,
               "while a group with a live member still counts as running")
    finally:
        live.kill()
        live.wait()


def main() -> int:
    test_the_synthetic_fixtures_are_valid_python()
    test_liveness_check_does_not_count_a_zombie_as_running()
    test_group_running_ignores_a_zombie_only_group()
    test_success_reaps_the_engine()
    test_failure_reaps_the_engine_and_keeps_the_tail()
    test_clean_probe_is_unaffected()
    test_timeout_escalates_to_sigkill()
    test_stubborn_engine_is_killed_without_charging_the_probe()
    test_a_stopping_runner_launches_no_further_probe()
    test_reap_group_on_a_dead_group_is_a_noop()
    test_aggregate_exit_codes_unchanged()
    test_exclusive_resource_declaration_is_data_about_real_probes()
    test_declared_conflicts_never_overlap()
    test_conflict_is_released_after_a_failure()
    test_conflict_is_released_after_a_timeout()
    test_exact_mixed_selection_is_rejected_before_listing()
    test_exact_mixed_selection_never_runs_the_valid_probe()
    test_exact_all_invalid_selection_keeps_existing_diagnostic()
    test_exact_all_valid_selection_is_unaffected()
    test_exact_duplicate_valid_keys_still_collapse()
    test_substring_selection_stays_permissive()
    test_retry_reaps_between_attempts()
    test_ctrl_c_leaves_no_engine_behind()
    test_ctrl_c_cancels_queued_parallel_work()
    test_ctrl_c_during_submission_starts_nothing_more()
    test_probe_launched_into_shutdown_is_killed_promptly()
    test_ctrl_c_in_the_launch_window_leaves_nothing()
    test_reap_returns_only_once_the_group_is_observed_gone()
    test_retry_can_rebind_the_port_a_killed_engine_held()
    if FAILURES:
        print(f"\n{len(FAILURES)} test(s) failed:")
        for failure in FAILURES:
            print(f"  {failure}")
        return 1
    print("\nAll run_probes teardown tests passed")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
