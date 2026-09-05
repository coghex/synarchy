#!/usr/bin/env python3
"""The process fixtures every owner in this package shares (#2130).

The synthetic tree and the machinery for driving the real runner at it,
and nothing case-specific: a helper only one family reads stays with
that family, which is what keeps this from becoming the catch-all
module the split exists to avoid.

  the generated sources -- `DESCENDANT_SRC` (a synthetic engine) and
  `probe_src` (a synthetic probe, in every shape the cases need);
  `Tree`, the throwaway checkout they are written into;
  the process, PID, file, socket and timing helpers the assertions read
  -- `process_state`, `pid_alive`, `wait_pid_gone`, `wait_file`,
  `free_port`, `free_port_span`, `overlaps`, `progress_lines`;
  `PreflightRecorder`, the deterministic stand-in for the preflight's
  one Cabal contact;
  `patched`, which points the production owners at the synthetic tree
  for the duration of one case -- including the port-span, timeout and
  expected-duration declaration tables, none of which name a synthetic
  key -- and `clear_namespace`, which it calls on the way out;
  `main_with_open`, `main_with` and `main_refusal`, the three ways a
  case invokes the real `run_probes.main` and captures what it printed.

`patched` mints a FRESH resource-namespace token per case and clears it
in `__exit__` on every outcome (#1436). Both properties are load-bearing
and survive the split unchanged: without the token this suite's
`config_migration`/`config_state` probes would take the repository's real
EXCLUSIVE `repo-config` lock against a live probe sweep, and without the
clearing every focused `--family` run would leave lock files in /tmp.
A focused run therefore cleans its own namespaces and never depends on a
sibling family having run.

`TOOLS_DIR` is also the one `sys.path` entry that lets a module INSIDE
this package reach the production owners: `python3 tools/<name>.py` puts
`tools/` on `sys.path`, but `Path(__file__).resolve().parent` here is the
package directory, so `--family registry` in a fresh interpreter needs
this resolved before any `import probe_runner_registry`.

Per requirement 11 this module imports no case owner, so the dependency
direction is one way: support, then the case owners, then the facade.
"""
from __future__ import annotations

import os
import random
import shutil
import signal
import socket
import subprocess
import sys
import tempfile
import textwrap
import time
import uuid
from pathlib import Path

#: `tools/` -- this package's parent, and where the production runner
#: owners, `probe_engine`, `probe_resource_lock` and `selftestlib` live.
TOOLS_DIR = str(Path(__file__).resolve().parent.parent)

if TOOLS_DIR not in sys.path:
    sys.path.insert(0, TOOLS_DIR)

import probe_engine  # noqa: E402
import probe_resource_lock  # noqa: E402
import probe_runner_lifecycle  # noqa: E402
import probe_runner_registry  # noqa: E402
import probe_runner_resources  # noqa: E402
import run_probes  # noqa: E402

# Short enough to keep the suite quick, long enough that a correct
# SIGTERM-then-poll escalation is genuinely exercised rather than skipped.
TEST_GRACE = 1.5


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
              handshake_delay: float = 0.0,
              bind_span: int = 0, bind_delay: float = 0.0,
              progress: "tuple[tuple[str, str, str], ...]" = (),
              failures: "tuple[tuple[str, str, str], ...]" = (),
              sentinel: str = "") -> str:
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

    ``bind_span`` makes the probe bind its OWN assigned span in-process --
    ``--port`` through ``--port + bind_span - 1`` -- appending "bound" or
    "inuse" per port to ``<name>.binds`` and exiting nonzero if any member
    was taken. That is what a real multi-port probe does
    (`debug_console_boot_probe.py` and `offscreen_probe.py` each keep a
    second listener on ``--port + 1``), and it is what makes an
    overlapping allocation observable rather than merely asserted (#1571).
    ``bind_delay`` holds that bind back, so a test can order two probes'
    binds deterministically instead of racing them.

    Every probe records the ``--port`` it was handed to ``<name>.port``,
    one line per attempt.

    ``progress`` is a sequence of ``(kind, identity, detail)`` triples the
    probe emits, in order, BEFORE its ``tail_lines`` -- through the real
    ``probe_runner_diagnostics.ProgressEmitter``, never a hand-copied string, so these
    cases exercise the shipped convention on both sides (#1768). Emitting
    them first is what lets a case bury them under more than ``--tail``
    ordinary lines and still require the failure presentation to surface
    them.

    ``failures`` is the same for #1982's durable failure records --
    ``(kind, identity, detail)`` triples emitted through the real
    ``probe_runner_diagnostics.FailureEmitter``. They are emitted FLUSHED, before the
    block-buffered ``tail_lines``, which is exactly the displacement the
    real probes suffer: their ``FAIL:`` lines go to an unbuffered stderr
    the runner merges into a piped, block-buffered stdout, so they
    overtake the buffered check output and land at the TOP of the merged
    capture, above whatever the ``--tail`` retains.

    ``sentinel`` is one ordinary line printed FIRST -- a non-diagnostic
    marker a case can require to stay OMITTED, which is what proves the
    failure presentation did not simply dump the complete capture.
    """
    pidfile = root / f"{name}.enginepid"
    startedfile = root / f"{name}.started"
    logfile = root / f"{name}.enginelog"
    interval = root / f"{name}.interval"
    term_policy = "ignore-term" if engine_ignores_term else "obey-term"
    lines = [
        "import argparse, os, signal, socket, subprocess, sys, time",
        # run_probes always appends --port in the parallel path; accept it
        # the way every registered probe does (#723).
        "ap = argparse.ArgumentParser()",
        "ap.add_argument('--port', type=int, default=0)",
        "_args = ap.parse_args()",
        # Appended, not truncated: `--retries` reuses this path, so the
        # port of every attempt is readable, in order.
        f"_pf = open({str(root / (name + '.port'))!r}, 'a')",
        "print(_args.port, file=_pf)",
        "_pf.flush()",
        "_pf.close()",
        # What the runner handed this attempt in the environment (#1570):
        # the resolved engine executable, and the resources an ancestor
        # already holds exclusively on its behalf. One line per attempt,
        # empty when the variable was absent -- which is itself the
        # assertion for a probe that must be left on the direct-invocation
        # fallback.
        f"_ef = open({str(root / (name + '.env'))!r}, 'a')",
        "print(os.environ.get('SYNARCHY_PROBE_ENGINE_EXE', ''),"
        " os.environ.get('SYNARCHY_PROBE_HELD_EXCLUSIVE', ''),"
        " os.environ.get('SYNARCHY_PROBE_HELD_NAMESPACE', ''),"
        " sep='|', file=_ef)",
        "_ef.flush()",
        "_ef.close()",
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
    if sentinel:
        lines.append(f"print({sentinel!r})")
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
    if bind_span:
        # The probe's own span, bound the way a real multi-port probe
        # binds it: base first, then each derived port, every socket kept
        # open for the probe's whole life.
        if bind_delay:
            lines.append(f"time.sleep({bind_delay!r})")
        lines += [
            "_held = []",
            "_clash = False",
            f"_blog = open({str(root / (name + '.binds'))!r}, 'a')",
            f"for _p in range(_args.port, _args.port + {bind_span}):",
            "    _s = socket.socket(socket.AF_INET, socket.SOCK_STREAM)",
            "    try:",
            "        _s.bind(('127.0.0.1', _p))",
            "        _s.listen(4)",
            "        _held.append(_s)",
            "        _outcome = 'bound'",
            "    except OSError:",
            "        _s.close()",
            "        _clash = True",
            "        _outcome = 'inuse'",
            "    print(_outcome, file=_blog)",
            "    _blog.flush()",
            "    os.fsync(_blog.fileno())",
        ]
    if progress:
        # The real module, imported from the REAL tools directory: a
        # synthetic copy of the record format would let the producer and
        # the consumer drift apart without a test noticing.
        lines += [
            f"sys.path.insert(0, {TOOLS_DIR!r})",
            "import probe_runner_diagnostics as _diag",
            "_progress = _diag.ProgressEmitter()",
        ]
        for kind, identity, detail in progress:
            lines.append(
                f"_progress.emit({kind!r}, {identity!r}, {detail!r})")
    if failures:
        # The real module again, for the same reason: a synthetic copy of
        # the record format would let producer and consumer drift apart
        # without a test noticing (#1982).
        lines += [
            f"sys.path.insert(0, {TOOLS_DIR!r})",
            "import probe_runner_diagnostics as _diag",
            "_failure = _diag.FailureEmitter('synthetic')",
        ]
        for kind, identity, detail in failures:
            lines.append(
                f"_failure.emit({kind!r}, {identity!r}, {detail!r})")
    for i in range(tail_lines):
        lines.append(f"print('diagnostic line {i}')")
    lines.append("sys.stdout.flush()")
    if dwell:
        lines.append(f"time.sleep({dwell!r})")
    if hang:
        lines.append("time.sleep(600)")
    lines.append("_stamp('end')")
    if bind_span:
        # A clash is the probe's OWN failure, exactly as a real engine's
        # "Address already in use" is; the declared exit code still wins
        # when every port was free.
        lines.append(f"sys.exit(1 if _clash else {exit_code})")
    else:
        lines.append(f"sys.exit({exit_code})")
    return "\n".join(lines) + "\n"


class Tree:
    """A throwaway REPO_ROOT holding synthetic probes under tools/."""

    def __init__(self) -> None:
        self.root = Path(tempfile.mkdtemp(prefix="test_run_probes_"))
        (self.root / "tools").mkdir()
        (self.root / "_descendant.py").write_text(DESCENDANT_SRC)
        self.probes: list[tuple[str, str, str]] = []
        # What the preflight double answers with (#1570). A real file,
        # executable, at an absolute path, so `probe_engine`'s validation
        # runs for real rather than being bypassed.
        self.executable = self.root / "synthetic-synarchy"
        self.executable.write_text("#!/bin/sh\nexit 0\n")
        self.executable.chmod(0o755)

    def env_lines(self, name: str) -> list[tuple[str, str, str]]:
        """`(engine exe, held-exclusive, held-namespace)` per attempt."""
        try:
            raw = (self.root / f"{name}.env").read_text()
        except OSError:
            return []
        out = []
        for line in raw.splitlines():
            parts = line.split("|")
            if len(parts) == 3:
                out.append((parts[0], parts[1], parts[2]))
        return out

    def engine_exes(self, name: str) -> list[str]:
        """The engine executable each attempt of this probe was handed."""
        return [exe for exe, _, _ in self.env_lines(name)]

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

    def ports(self, name: str) -> list[int]:
        """The `--port` this probe was handed, one per attempt, in order."""
        try:
            raw = (self.root / f"{name}.port").read_text()
        except OSError:
            return []
        return [int(tok) for tok in raw.split() if tok.lstrip("-").isdigit()]

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


def free_port() -> int:
    """A port nothing is listening on right now."""
    with socket.socket(socket.AF_INET, socket.SOCK_STREAM) as probe:
        probe.bind(("127.0.0.1", 0))
        return probe.getsockname()[1]


# --------------------------------------------------------------------------
# Reserved port spans (#1571)
# --------------------------------------------------------------------------
def free_port_span(width: int) -> int:
    """A base with `width` consecutive ports all bindable right now.

    An ephemeral port is one port; a multi-port probe needs a run of
    them, and nothing hands those out. So candidate bases are tried until
    the whole run binds at once — which is also the only way to know the
    run is really free rather than merely starting at a free port.
    """
    for _ in range(400):
        base = random.randint(20000, 59000)
        held: list[socket.socket] = []
        try:
            for port in range(base, base + width):
                sock = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
                sock.bind(("127.0.0.1", port))
                held.append(sock)
        except OSError:
            continue
        finally:
            for sock in held:
                sock.close()
        if len(held) == width:
            return base
    raise AssertionError(f"no free {width}-port span found")


# --------------------------------------------------------------------------
# Parallel scheduling honours the reader/writer resource model (#1322, #1444)
#
# These drive the SHIPPED `probe_runner_resources.EXCLUSIVE_RESOURCES` and
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


class PreflightRecorder:
    """A deterministic stand-in for the preflight's subprocess entry point.

    `probe_runner_resources.engine_preflight` makes the ONE Cabal contact an aggregate
    run is allowed (#1570): a freshness `cabal build` and then a read-only
    `cabal list-bin`. This answers both without a toolchain — the build
    succeeds silently, the query prints the synthetic tree's own
    executable — and records every argv it was handed, with the wall-clock
    instant of each call.

    The RECORD is the point. Counting the calls is what proves the
    preflight happened exactly once for a whole sweep, and timing them
    against the probes' own `<name>.interval` stamps is what proves it
    finished before any probe process existed, rather than merely that it
    happened.

    `fail` makes the named step exit nonzero, which is how the
    failure-before-anything-spawns case is driven.
    """

    def __init__(self, executable, *, fail: str | None = None,
                 message: str = "synthetic preflight failure") -> None:
        self.executable = str(executable)
        self.fail = fail
        self.message = message
        self.calls: list[tuple[tuple[str, ...], float]] = []

    def __call__(self, argv, cwd=None, capture_output=False, text=False):
        argv = tuple(argv)
        self.calls.append((argv, time.time()))
        step = "build" if "build" in argv else "locate"
        if self.fail == step:
            return subprocess.CompletedProcess(argv, 1, "", self.message)
        stdout = "" if step == "build" else f"{self.executable}\n"
        return subprocess.CompletedProcess(argv, 0, stdout, "")

    @property
    def argvs(self) -> list[tuple[str, ...]]:
        return [argv for argv, _ in self.calls]

    @property
    def finished_at(self) -> float:
        """When the LAST preflight call was made; 0.0 if none was."""
        return max((when for _, when in self.calls), default=0.0)


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
                 namespace: str | None = None,
                 spans: dict[str, int] | None = None,
                 timeouts: dict[str, float] | None = None,
                 durations: dict[str, float] | None = None,
                 preflight: "PreflightRecorder | None" = None) -> None:
        self.tree, self.grace = tree, grace
        # The engine-executable preflight (#1570) is doubled for the same
        # reason the namespace is: a synthetic tree is no Cabal project,
        # and a real `cabal build` here would make every main()-driven
        # case depend on a toolchain and a warm build directory.
        self.preflight = (PreflightRecorder(tree.executable)
                          if preflight is None else preflight)
        self.namespace = namespace or f"selftest{uuid.uuid4().hex[:12]}"
        # The synthetic probes have synthetic keys, so the shipped
        # `PROBE_PORT_SPANS` says nothing about them. A test declaring a
        # multi-port synthetic probe substitutes its own table (#1571);
        # passing none leaves every synthetic probe at the default one
        # port, which is what the shipped table would give them anyway.
        self.spans = {} if spans is None else dict(spans)
        # The shipped timeout table names shipped keys. Synthetic registries
        # start with no exceptions unless a case supplies its own declarations.
        self.timeouts = {} if timeouts is None else dict(timeouts)
        # Likewise the expected-duration table parallel dispatch orders by
        # (#2275). Supplying it INDEPENDENTLY of how long a synthetic probe
        # actually dwells is the point: a case can then assert that the
        # order came from the declarations rather than from the fixture
        # happening to finish in that sequence.
        self.durations = {} if durations is None else dict(durations)

    def __enter__(self):
        self._saved = (probe_engine.REPO_ROOT, probe_runner_registry.PROBES,
                       probe_runner_lifecycle.GROUP_GRACE, probe_runner_resources.RESOURCE_NAMESPACE,
                       probe_runner_registry.PROBE_PORT_SPANS,
                       probe_runner_registry.PROBE_TIMEOUT_OVERRIDES,
                       probe_runner_registry.PROBE_EXPECTED_SECONDS,
                       probe_runner_resources.ENGINE_EXECUTABLE,
                       probe_runner_resources.ENGINE_PREFLIGHT_RUNNER)
        # An operator's own export of the runner's variables must not
        # decide what a case here observes; `main` re-derives all of them.
        self._saved_env = {name: os.environ.pop(name, None)
                           for name in probe_runner_resources.RUNNER_ENV_VARS}
        probe_engine.REPO_ROOT = str(self.tree.root)
        probe_runner_registry.PROBES = list(self.tree.probes)
        probe_runner_lifecycle.GROUP_GRACE = self.grace
        probe_runner_resources.RESOURCE_NAMESPACE = self.namespace
        probe_runner_registry.PROBE_PORT_SPANS = self.spans
        probe_runner_registry.PROBE_TIMEOUT_OVERRIDES = self.timeouts
        probe_runner_registry.PROBE_EXPECTED_SECONDS = self.durations
        probe_runner_resources.ENGINE_EXECUTABLE = None
        probe_runner_resources.ENGINE_PREFLIGHT_RUNNER = self.preflight
        return self

    def __exit__(self, *exc):
        (probe_engine.REPO_ROOT, probe_runner_registry.PROBES, probe_runner_lifecycle.GROUP_GRACE,
         probe_runner_resources.RESOURCE_NAMESPACE,
         probe_runner_registry.PROBE_PORT_SPANS,
         probe_runner_registry.PROBE_TIMEOUT_OVERRIDES,
         probe_runner_registry.PROBE_EXPECTED_SECONDS,
         probe_runner_resources.ENGINE_EXECUTABLE,
         probe_runner_resources.ENGINE_PREFLIGHT_RUNNER) = self._saved
        for name, value in self._saved_env.items():
            if value is None:
                os.environ.pop(name, None)
            else:
                os.environ[name] = value
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
# Aggregate behaviour: statuses and the aggregate exit code are unchanged
# --------------------------------------------------------------------------
def main_with_open(tree: Tree, argv: list[str]) -> tuple[int, str]:
    """`main_with`'s body WITHOUT entering `patched`.

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


def main_with(tree: Tree, argv: list[str],
               spans: dict[str, int] | None = None,
               timeouts: dict[str, float] | None = None,
               durations: dict[str, float] | None = None) -> tuple[int, str]:
    import io
    import contextlib
    buf = io.StringIO()
    saved_argv = sys.argv
    sys.argv = ["run_probes.py"] + argv
    try:
        with patched(tree, spans=spans, timeouts=timeouts,
                     durations=durations), \
             contextlib.redirect_stdout(buf), \
             contextlib.redirect_stderr(buf):
            rc = run_probes.main()
    finally:
        sys.argv = saved_argv
    return rc, buf.getvalue()


def main_refusal(tree: Tree, argv: list[str],
                  spans: dict[str, int] | None = None) -> tuple[int, str]:
    """`main_with`, but reporting a `sys.exit` refusal as its own code.

    `main` refuses a bad port plan two ways: `sys.exit(message)` for the
    base-is-the-GUI-port case, which predates #1571, and `return 2` for
    the span-aware plan check. A test asserting the refusal happened at
    all should not have to know which.
    """
    try:
        return main_with(tree, argv, spans=spans)
    except SystemExit as leaving:
        code = leaving.code
        return (1 if isinstance(code, str) else (code or 0),
                code if isinstance(code, str) else "")


__all__ = [
    "DESCENDANT_SRC",
    "PreflightRecorder",
    "TEST_GRACE",
    "TOOLS_DIR",
    "Tree",
    "clear_namespace",
    "free_port",
    "free_port_span",
    "main_refusal",
    "main_with",
    "main_with_open",
    "overlaps",
    "patched",
    "pid_alive",
    "probe_src",
    "process_state",
    "progress_lines",
    "wait_file",
    "wait_pid_gone",
]
