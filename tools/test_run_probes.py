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
scheduler's reader/writer serialization (issues #1322 and #1444): probes
stamp their own occupancy windows, so a probe declaring an exclusive
interest is proved never to overlap ANY other probe -- the other config
declaration or an ordinary engine-booting one, in either dispatch order --
while two ordinary probes still overlap each other, across a conflicting
probe that passes, one that fails, and one that times out.

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
import threading
import time
import uuid
from pathlib import Path

sys.path.insert(0, str(Path(__file__).resolve().parent))
import probe_resource_lock  # type: ignore
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
    # Seconds to spend between the ledger write and the handshake write.
    # Zero for every ordinary probe; a test that wants the ORDER above
    # proven rather than asserted sets it (see
    # test_engine_ledger_is_durable_before_the_handshake).
    handshake_delay = float(sys.argv[5]) if len(sys.argv) > 5 else 0.0
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
    # ORDER IS LOAD-BEARING: every record this engine leaves must be
    # durable BEFORE the handshake file, because the handshake is what
    # releases the probe -- and the probe exiting is what fires
    # `run_one`'s `finally: reap_group(pgid)`, which SIGTERMs this
    # process. `.all` (the per-attempt ledger `engine_pids` counts) is
    # therefore written first, and `pidfile` (the single-slot handshake
    # `probe_src` polls) last. Written the other way round the probe can
    # observe pidfile's CONTENT as soon as the write is flushed -- the
    # fsync that follows is not what makes it readable -- and reap this
    # process before it ever reaches `.all`, losing that attempt's line
    # and failing `test_retry_reaps_between_attempts` with one pid where
    # the retry really did boot two. That is not hypothetical: it is a
    # real Linux CI failure, and it stayed invisible on a fast local
    # filesystem where the window is microseconds. The bind log above is
    # already on the safe side of the handshake for the same reason --
    # which is why it stayed intact in that failure while `.all` did not.
    with open(pidfile + ".all", "a") as fh:
        print(os.getpid(), file=fh)
        fh.flush()
        os.fsync(fh.fileno())
    if handshake_delay:
        time.sleep(handshake_delay)
    with open(pidfile, "w") as fh:
        fh.write(str(os.getpid()))
        fh.flush()
        os.fsync(fh.fileno())
    time.sleep(600)
    """)


def probe_src(root: Path, name: str, *, exit_code: int = 0,
              tail_lines: int = 0, hang: bool = False,
              ignore_term: bool = False, engine_ignores_term: bool = False,
              descendant: bool = True, hold_port: int = 0,
              dwell: float = 0.0,
              handshake_delay: float = 0.0) -> str:
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
            f" {str(root / (name + '.binds'))!r}, {str(handshake_delay)!r}],"
            " stdout=log, stderr=subprocess.STDOUT)",
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
    """Point the real runner at the synthetic tree for one test.

    The cross-process resource namespace (#1436) is redirected to a
    SYNTHETIC token as well, and that is not cosmetic: the synthetic
    probes are named `config_migration` and `config_state`, so under the
    real namespace this suite would take the repository's real EXCLUSIVE
    `repo-config` lock and stall — or be stalled by — a genuine probe
    sweep or `/deflake` measurement running beside it. A token per test
    also keeps the suite's own cases from coordinating with each other.
    """

    def __init__(self, tree: Tree, grace: float = TEST_GRACE,
                 namespace: str | None = None) -> None:
        self.tree, self.grace = tree, grace
        self.namespace = namespace or f"selftest{uuid.uuid4().hex[:12]}"

    def __enter__(self):
        self._saved = (run_probes.REPO_ROOT, run_probes.PROBES,
                       run_probes.GROUP_GRACE, run_probes.RESOURCE_NAMESPACE)
        run_probes.REPO_ROOT = str(self.tree.root)
        run_probes.PROBES = list(self.tree.probes)
        run_probes.GROUP_GRACE = self.grace
        run_probes.RESOURCE_NAMESPACE = self.namespace
        return self

    def __exit__(self, *exc):
        (run_probes.REPO_ROOT, run_probes.PROBES, run_probes.GROUP_GRACE,
         run_probes.RESOURCE_NAMESPACE) = self._saved
        clear_namespace(self.namespace)
        return False


def clear_namespace(namespace: str) -> None:
    """Remove one synthetic namespace's lock and note files from /tmp.

    Scoped to the literal `synarchy-probe-resource-<namespace>-` prefix of
    a token this suite minted, and run only once every hold it took has
    been released, so nothing another process could be holding is touched.
    The real runner never unlinks a lock file — see
    `probe_resource_lock`'s module docstring for why — but the tokens here
    are per-test and would otherwise accumulate in /tmp forever.
    """
    prefix = f"{probe_resource_lock.SHARED_PREFIX}-{namespace}-"
    try:
        entries = list(probe_resource_lock.LOCK_ROOT.glob(f"{prefix}*"))
    except OSError:
        return
    for entry in entries:
        if not entry.name.startswith(prefix):
            continue
        try:
            entry.unlink()
        except OSError:
            pass


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
def _main_with_open(tree: Tree, argv: list[str]) -> tuple[int, str]:
    """`_main_with`'s body WITHOUT entering `patched`.

    The cross-process cases hold `patched` open across a background
    thread, so they cannot have it entered a second time underneath them:
    the namespace would be restored by the inner exit while the sweep was
    still running.
    """
    import io
    import contextlib as _contextlib
    buf = io.StringIO()
    saved = sys.argv
    sys.argv = ["run_probes.py", *argv]
    try:
        with _contextlib.redirect_stdout(buf), _contextlib.redirect_stderr(buf):
            rc = run_probes.main()
    finally:
        sys.argv = saved
    return rc, buf.getvalue()


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
# Parallel scheduling honours the reader/writer resource model (#1322, #1444)
#
# These drive the SHIPPED `run_probes.EXCLUSIVE_RESOURCES` and
# `IMPLICIT_SHARED_RESOURCES` -- only `PROBES` and `REPO_ROOT` are redirected
# at the synthetic tree, and the synthetic probes are deliberately named
# `config_migration` and `config_state` so the real declaration is what
# decides. A test-local conflict table would prove the mechanism while
# leaving the two probes the issue is about unguarded.
#
# `unrelated_a`/`unrelated_b` stand in for the ~85 probes that declare
# nothing and simply boot an engine in the shared checkout. Under #1444
# those hold `repo-config` SHARED, so they overlap each other freely and
# neither of them may overlap a config probe.
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
        rc, out = _main_with(tree, ["--jobs", "3"])
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
        rc, out = _main_with(tree, ["--jobs", "2"])
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
        rc, out = _main_with(tree, ["--jobs", "3"])
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
        rc, out = _main_with(tree, ["--jobs", "3", "--timeout", "2"])
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


def test_every_probe_shares_what_the_config_probes_take_exclusively() -> None:
    print("\n-- the shipped declaration serializes BOTH config probes against "
          "the whole registry (#1444)")
    expect(bool(run_probes.IMPLICIT_SHARED_RESOURCES),
           "there is an implicit shared interest at all "
           f"(got {run_probes.IMPLICIT_SHARED_RESOURCES!r})")
    solo = {"config_migration", "config_state"}
    declared = set(run_probes.EXCLUSIVE_RESOURCES)
    expect(declared == solo,
           f"both config probes are the declared exclusive holders "
           f"(declared: {sorted(declared)})")
    taken: set[str] = set()
    for key in solo:
        taken |= run_probes.exclusive_resources(key)
        # An interest is one or the other, never both, or a release would
        # drop the exclusive half and leave the shared count behind.
        overlap = (run_probes.exclusive_resources(key)
                   & run_probes.shared_resources(key))
        expect(not overlap,
               f"{key} holds no resource in both interests "
               f"(both: {sorted(overlap)})")
    # The whole registry, not a sample: this is what makes requirement 1 --
    # "no probe's engine may boot while a config probe is running" -- a
    # property of the shipped data rather than of the three synthetic
    # probes the scheduling tests above drive.
    unguarded = sorted(key for key, _, _ in run_probes.PROBES
                       if key not in solo
                       and not taken <= run_probes.shared_resources(key))
    expect(not unguarded,
           f"every other registered probe holds every resource they take, "
           f"so none can be scheduled beside them (unguarded: {unguarded})")


def test_resource_ledger_is_a_reader_writer_lock() -> None:
    print("\n-- the ledger grants many readers at once and a writer only alone")
    ledger = run_probes.ResourceLedger()
    shared, exclusive = {"repo-config"}, {"repo-config"}
    expect(ledger.idle(), "a fresh ledger holds nothing")
    expect(not ledger.blocked(exclusive, set()),
           "so an exclusive interest is grantable")

    # Three readers, then a writer: the shared side must COUNT, not merely
    # remember that someone held it, or the writer starts after the first
    # release with two engines still up.
    for _ in range(3):
        expect(not ledger.blocked(set(), shared),
               "another reader may join while readers hold it")
        ledger.acquire(set(), shared)
    for held in (2, 1):
        expect(ledger.blocked(exclusive, set()),
               f"a writer is blocked while {held + 1} reader(s) hold it")
        ledger.release(set(), shared)
    expect(ledger.blocked(exclusive, set()),
           "still blocked with the last reader holding it")
    ledger.release(set(), shared)
    expect(ledger.idle() and not ledger.blocked(exclusive, set()),
           "and grantable only once every reader has released")

    # And the mirror: a writer excludes readers and writers alike.
    ledger.acquire(exclusive, set())
    expect(ledger.blocked(set(), shared),
           "a reader is blocked while a writer holds it")
    expect(ledger.blocked(exclusive, set()),
           "and so is a second writer")
    ledger.release(exclusive, set())
    expect(ledger.idle(), "releasing the writer empties the ledger")

    # A probe that declares nothing shared is never blocked by anything,
    # which is what keeps an unrelated resource out of this decision.
    ledger.acquire(exclusive, set())
    expect(not ledger.blocked(set(), {"some-other-resource"}),
           "an interest in a different resource is unaffected")
    ledger.release(exclusive, set())


# --------------------------------------------------------------------------
# Exact selection: unknown keys are rejected rather than silently dropped
# (#1321)
# --------------------------------------------------------------------------
# --------------------------------------------------------------------------
# The cross-process half of the same reader/writer model (#1436)
# --------------------------------------------------------------------------
FOREIGN_HOLDER_SRC = textwrap.dedent("""\
    # A separate PROCESS holding one probe resource, because that is the
    # thing the in-process ledger cannot see. Signals readiness by
    # creating a file, then holds until a release file appears.
    import sys, time
    from pathlib import Path
    sys.path.insert(0, sys.argv[1])
    import probe_resource_lock as lock
    namespace, interest, resource, ready, release = sys.argv[2:7]
    kwargs = ({"exclusive": {resource}} if interest == "exclusive"
              else {"shared": {resource}})
    hold = lock.acquire(namespace=namespace, purpose="foreign holder", **kwargs)
    Path(ready).write_text("held")
    deadline = time.time() + 120
    while not Path(release).exists() and time.time() < deadline:
        time.sleep(0.05)
    hold.release()
""")


class ForeignHolder:
    """A real second process holding one resource for the duration."""

    def __init__(self, namespace: str, interest: str,
                 resource: str = "repo-config") -> None:
        self.dir = Path(tempfile.mkdtemp(prefix="foreign_holder_"))
        script = self.dir / "holder.py"
        script.write_text(FOREIGN_HOLDER_SRC)
        self.ready = self.dir / "ready"
        self.release_flag = self.dir / "release"
        self.proc = subprocess.Popen(
            [sys.executable, str(script), TOOLS_DIR, namespace, interest,
             resource, str(self.ready), str(self.release_flag)])

    def wait_until_held(self, seconds: float = 30.0) -> bool:
        return wait_file(self.ready, seconds)

    def stop(self) -> None:
        try:
            self.release_flag.write_text("go")
        except OSError:
            pass
        try:
            self.proc.wait(timeout=30)
        except subprocess.TimeoutExpired:
            self.proc.kill()
            self.proc.wait(timeout=10)
        shutil.rmtree(self.dir, ignore_errors=True)


def test_a_foreign_exclusive_holder_makes_the_sweep_wait() -> None:
    print("\n-- a foreign EXCLUSIVE holder stalls every probe without "
          "crashing the scheduler")
    tree = Tree()
    namespace = f"selftest{uuid.uuid4().hex[:12]}"
    holder = ForeignHolder(namespace, "exclusive")
    seen: dict = {}
    try:
        seen["took_lock"] = holder.wait_until_held()
        # Every registered probe holds `repo-config` SHARED, so ONE foreign
        # exclusive holder conflicts with the whole roster. Before #1436 the
        # scheduler's "nothing running, work pending" guard raised
        # RuntimeError here and took the sweep down.
        tree.add("unrelated_a", dwell=0.2, descendant=False)
        tree.add("unrelated_b", dwell=0.2, descendant=False)
        saved_poll = run_probes.RESOURCE_WAIT_POLL
        run_probes.RESOURCE_WAIT_POLL = 0.2
        result: dict = {}

        def sweep() -> None:
            with patched(tree, namespace=namespace):
                result["rc"], result["out"] = _main_with_open(tree, ["--jobs", "2"])

        thread = threading.Thread(target=sweep, daemon=True)
        thread.start()
        # Nothing is asserted while the sweep runs: it has redirected
        # stdout, so a message printed here would land in its buffer.
        # Observations are recorded and judged after the join.
        time.sleep(2.0)
        seen["still_waiting"] = thread.is_alive()
        seen["nothing_started"] = (not tree.started("unrelated_a")
                                   and not tree.started("unrelated_b"))
        holder.stop()
        thread.join(timeout=90)
        run_probes.RESOURCE_WAIT_POLL = saved_poll

        expect(seen["took_lock"], "the foreign process took the lock")
        expect(seen["still_waiting"],
               "the sweep is still waiting rather than having crashed or "
               "finished")
        expect(seen["nothing_started"],
               "and no probe started while the foreign holder was in the way")
        expect(not thread.is_alive(), "the sweep finishes once the lock frees")
        expect(result.get("rc") == 0,
               f"and every probe then passes (exit {result.get('rc')})")
        expect("waiting for 'repo-config'" in (result.get("out") or "")
               and "exclusive" in (result.get("out") or ""),
               "the runner said WHICH resource it was waiting on and in which "
               "interest")
        for name in ("unrelated_a", "unrelated_b"):
            got = tree.intervals(name)
            expect(len(got) == 1 and got[0][1] is not None,
                   f"{name} ran exactly once, after the wait (windows: {got})")
    finally:
        holder.stop()
        clear_namespace(namespace)
        tree.cleanup()


def test_waiting_for_a_foreign_holder_is_not_charged_to_the_probe() -> None:
    print("\n-- a queued probe's elapsed time and timeout cover execution "
          "only, never the wait")
    tree = Tree()
    namespace = f"selftest{uuid.uuid4().hex[:12]}"
    holder = ForeignHolder(namespace, "exclusive")
    seen: dict = {}
    try:
        seen["took_lock"] = holder.wait_until_held()
        tree.add("unrelated_a", dwell=0.2, descendant=False)
        saved_poll = run_probes.RESOURCE_WAIT_POLL
        run_probes.RESOURCE_WAIT_POLL = 0.2
        result: dict = {}

        def sweep() -> None:
            with patched(tree, namespace=namespace):
                # SEQUENTIAL, and with a timeout far shorter than the wait
                # below: if the wait were inside the probe's own clock this
                # would be reported TIMEOUT instead of PASS.
                result["rc"], result["out"] = _main_with_open(
                    tree, ["--jobs", "1", "--timeout", "5"])

        thread = threading.Thread(target=sweep, daemon=True)
        thread.start()
        time.sleep(8.0)
        seen["still_waiting"] = thread.is_alive()
        holder.stop()
        thread.join(timeout=90)
        run_probes.RESOURCE_WAIT_POLL = saved_poll
        out = result.get("out") or ""

        expect(seen["took_lock"], "the foreign process took the lock")
        expect(seen["still_waiting"], "the sweep waited rather than running")
        expect(result.get("rc") == 0,
               f"the probe passes after an 8s wait against a 5s timeout "
               f"(exit {result.get('rc')})")
        expect("TIMEOUT" not in out,
               "and the wait is never reported as a TIMEOUT")
        window = tree.window("unrelated_a")
        expect(window[1] is not None and (window[1] - window[0]) < 5.0,
               f"the probe's own occupancy window is its execution alone "
               f"({window})")
    finally:
        holder.stop()
        clear_namespace(namespace)
        tree.cleanup()


def test_a_foreign_shared_holder_never_blocks_a_shared_probe() -> None:
    print("\n-- a foreign SHARED holder does not serialize ordinary probes")
    tree = Tree()
    namespace = f"selftest{uuid.uuid4().hex[:12]}"
    holder = ForeignHolder(namespace, "shared")
    try:
        took = holder.wait_until_held()
        tree.add("unrelated_a", dwell=0.8, descendant=False)
        tree.add("unrelated_b", dwell=0.8, descendant=False)
        with patched(tree, namespace=namespace):
            rc, out = _main_with_open(tree, ["--jobs", "2"])
        expect(took, "the foreign process took the lock")
        expect(rc == 0, f"both probes pass beside the shared holder (exit {rc})")
        first, second = tree.window("unrelated_a"), tree.window("unrelated_b")
        expect(overlaps(first, second),
               f"and they still run concurrently: shared holders coexist "
               f"(unrelated_a {first}, unrelated_b {second})")
        expect("waiting for" not in out, "nothing waited on anything")
    finally:
        holder.stop()
        clear_namespace(namespace)
        tree.cleanup()


def test_a_run_probes_exclusive_probe_blocks_a_foreign_shared_acquirer() -> None:
    print("\n-- the conflict is detected in the other direction too: a "
          "run_probes exclusive probe blocks a foreign shared acquirer")
    tree = Tree()
    namespace = f"selftest{uuid.uuid4().hex[:12]}"
    seen: dict = {}
    try:
        tree.add("config_state", dwell=3.0, descendant=False)
        result: dict = {}

        def sweep() -> None:
            with patched(tree, namespace=namespace):
                result["rc"], result["out"] = _main_with_open(tree, ["--jobs", "1"])

        thread = threading.Thread(target=sweep, daemon=True)
        thread.start()
        seen["started"] = wait_file(tree.root / "config_state.started", 60.0)
        # While it runs, a /deflake-shaped acquirer must be refused even for
        # a SHARED interest -- the direction the in-process ledger could
        # never enforce, because the acquirer is not in its process.
        try:
            spurious = probe_resource_lock.acquire(
                shared={"repo-config"}, namespace=namespace,
                purpose="foreign shared acquirer")
        except probe_resource_lock.ResourceBusy as busy:
            seen["busy"] = busy
        else:
            spurious.release()
        thread.join(timeout=120)

        expect(seen.get("started") is True, "the exclusive probe started")
        expect("busy" in seen,
               "a foreign SHARED acquirer is refused while run_probes holds "
               "the resource exclusively")
        busy = seen.get("busy")
        if busy is not None:
            expect(busy.resource == "repo-config" and busy.interest == "shared",
                   f"and the refusal names the resource and the interest "
                   f"({busy.resource!r}, {busy.interest})")
            expect(any(holder.get("interest") == "exclusive"
                       for holder in busy.holders),
                   f"and reports the exclusive holder ({busy.holders})")
        expect(result.get("rc") == 0,
               f"the sweep itself is unaffected (exit {result.get('rc')})")
        # And once it is over, the same acquisition succeeds.
        after = probe_resource_lock.acquire(shared={"repo-config"},
                                            namespace=namespace)
        after.release()
        expect(True, "the same acquisition succeeds once the probe is done")
    finally:
        clear_namespace(namespace)
        tree.cleanup()


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
    # The synthetic tree is not a git checkout, so the cross-process
    # resource namespace (#1436) has to be supplied the same way the
    # in-process `patched` fixture supplies it -- otherwise the runner
    # refuses to start and the interrupt below has nothing to interrupt.
    run_probes.RESOURCE_NAMESPACE = {namespace!r}
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
        namespace=namespace))
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
        rc, _ = _main_with(tree, ["--only", "ledger", "--exact"])
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
    test_every_probe_shares_what_the_config_probes_take_exclusively()
    test_resource_ledger_is_a_reader_writer_lock()
    test_declared_conflicts_never_overlap()
    test_a_solo_probe_waits_for_work_already_running()
    test_conflict_is_released_after_a_failure()
    test_conflict_is_released_after_a_timeout()
    test_a_foreign_exclusive_holder_makes_the_sweep_wait()
    test_waiting_for_a_foreign_holder_is_not_charged_to_the_probe()
    test_a_foreign_shared_holder_never_blocks_a_shared_probe()
    test_a_run_probes_exclusive_probe_blocks_a_foreign_shared_acquirer()
    test_exact_mixed_selection_is_rejected_before_listing()
    test_exact_mixed_selection_never_runs_the_valid_probe()
    test_exact_all_invalid_selection_keeps_existing_diagnostic()
    test_exact_all_valid_selection_is_unaffected()
    test_exact_duplicate_valid_keys_still_collapse()
    test_substring_selection_stays_permissive()
    test_retry_reaps_between_attempts()
    test_engine_ledger_is_durable_before_the_handshake()
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
