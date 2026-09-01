#!/usr/bin/env python3
"""Launching ONE probe, and reaping everything it left behind.

The process-group teardown contract (#1323): every completion path —
success, ordinary nonzero exit, timeout, an exception in the runner, and
Ctrl-C — reaps the probe's complete process group, so the engine a probe
booted never outlives it holding the probe's port. Plus the deferred-SIGINT
launch window, the TERM-to-KILL escalation, the live-group registry the
interrupt path drains, and the `probe-result/v1` environment (#1425) a
child is handed.

Dependencies (#2074 requirement 11): the resource owner (for the
runner-owned environment variable names `run_one` scrubs, and for the
`ENGINE_EXECUTABLE` cell it hands the child), `probe_engine`, and
`probe_protocol`. Nothing here imports the scheduler or the runner
command, and nothing here takes a resource hold — a hold spans an
execution, which is the scheduler's unit, not this one's.
"""
from __future__ import annotations
import os
import signal
import subprocess
import sys
import threading
import time

import probe_engine
import probe_protocol
import probe_runner_resources as resources

# How long a signalled probe process group gets to leave on its own before
# the escalation to SIGKILL. One grace period shared by the timeout path
# (which has always spent it waiting on the probe leader) and the
# after-every-completion reap below (which spends it polling the GROUP,
# because by then the leader has already been reaped).
GROUP_GRACE = 10.0

# How long to wait for a SIGKILLed group to actually empty. Short by
# design: SIGKILL cannot be blocked, so this covers the asynchronous
# delivery and teardown — during which the engine still holds its port —
# rather than giving anything a second chance to shut down cleanly.
KILL_SETTLE = 5.0


# --------------------------------------------------------------------------
# Process-group teardown (#1323)
# --------------------------------------------------------------------------
def _signal_group(pgid: int, sig: int) -> bool:
    """Send ``sig`` to a whole process group; True if the group was there.

    ``ProcessLookupError`` (ESRCH) means the group is already empty, which
    is the ORDINARY outcome for a probe that tore its own engine down —
    success, not an error, and it must leave the probe's recorded result
    alone. ``PermissionError`` means a member exists that we may not
    signal, so the group is still alive.
    """
    try:
        os.killpg(pgid, sig)
    except ProcessLookupError:
        return False
    except PermissionError:
        return True
    return True


def _group_alive(pgid: int) -> bool:
    """True while any process — running OR zombie — remains in the group."""
    return _signal_group(pgid, 0)


def _group_states(pgid: int) -> list[str] | None:
    """Process state of every member of ``pgid``, or None if unreadable."""
    try:
        done = subprocess.run(["ps", "-eo", "pgid=,state="],
                              capture_output=True, text=True, timeout=15)
    except (OSError, subprocess.SubprocessError):
        return None
    if done.returncode != 0:
        return None
    states = []
    for line in done.stdout.splitlines():
        parts = line.split()
        if len(parts) >= 2 and parts[0].isdigit() and int(parts[0]) == pgid:
            states.append(parts[1])
    return states


def _group_running(pgid: int) -> bool:
    """True while the group holds a member that has NOT yet exited.

    A zombie does not count. It has already exited — releasing its port
    and every other resource — and is only waiting to be reaped, which
    nothing may ever do for a process orphaned under an init that does not
    reap (a container's PID 1, typically). Signals cannot tell the two
    apart: a zombie-only group answers EPERM on macOS and succeeds on
    Linux, so `killpg` alone reports a long-dead engine as live and would
    spend the entire grace, and then the post-SIGKILL settle, waiting for
    something that has already released the port.

    Falls back to the signal answer when states cannot be read, erring
    towards waiting rather than towards calling a live engine gone.
    """
    states = _group_states(pgid)
    if states is None:
        return _group_alive(pgid)
    return any(not state.startswith("Z") for state in states)


def reap_group(pgid: int, grace: float | None = None) -> None:
    """Terminate anything still running in a probe's process group.

    Called on EVERY completion path (#1323) — success, ordinary nonzero
    exit, timeout, and an exception in the runner itself — not just on the
    timeout, because a probe that dies of an unexpected exception after
    booting its engine never reaches its own ``quit_engine``. The engine
    then outlives it holding the probe's port, and `--retries` (and the
    parallel solo-retry, which reuses the allocation origin) re-runs onto that
    held port, where #1190 aborts the boot and reports the leak as an
    unrelated "exited before READY".

    ``communicate()`` cannot detect that: ``probelib.boot`` redirects the
    engine's output to a log file rather than the runner's inherited pipe,
    so the pipe reaches EOF the moment the probe itself exits.

    Reaping an empty group is a no-op. Once ``communicate()`` has reaped
    the probe leader there is nothing left to wait on, so the SIGTERM grace
    is spent polling the GROUP for liveness before escalating to SIGKILL.

    It does not return until the group is OBSERVED empty (or the budget
    runs out). Sending SIGKILL is not the same as the group being gone —
    delivery and teardown are asynchronous — and until its last member
    exits, the engine still owns the listening port. Returning early would
    hand that port straight to the next `--retries` attempt and recreate
    the #1190 boot abort this reap exists to prevent.
    """
    if grace is None:
        grace = GROUP_GRACE
    # Signal-only fast path: an empty group is the ordinary case, and
    # answering it without spawning `ps` keeps this free for every probe
    # that tore its own engine down. It also means a recycled pid is never
    # signalled.
    if not _group_alive(pgid):
        return
    if not _group_running(pgid):
        return
    if not _signal_group(pgid, signal.SIGTERM):
        return
    if _wait_group_stopped(pgid, grace):
        return
    _signal_group(pgid, signal.SIGKILL)
    # SIGKILL cannot be blocked, so this is a short settle rather than
    # another full grace — the members are already condemned.
    if _wait_group_stopped(pgid, KILL_SETTLE):
        return
    # Say so rather than let the retry fail as an unexplained "exited
    # before READY", which is the confusion #1323 is about.
    print(f"warning: process group {pgid} still had a running member after "
          f"SIGKILL; a retry reusing its port may fail to bind",
          file=sys.stderr)


def _wait_group_stopped(pgid: int, budget: float) -> bool:
    """Poll until nothing in the group is still running, or time runs out."""
    deadline = time.monotonic() + budget
    while True:
        if not _group_running(pgid):
            return True
        if time.monotonic() >= deadline:
            return False
        time.sleep(0.2)


class _DeferSigint:
    """Hold a Ctrl-C until a freshly spawned probe is trackable.

    The interpreter checks for signals BETWEEN bytecodes, so a
    KeyboardInterrupt can land after `Popen` has already forked and even
    between that call and the assignment naming its result. A probe
    spawned in that window is one nothing can reach: `groups` never
    learned its pgid, and the `finally` that reaps has not been entered —
    so Ctrl-C leaves it, and the engine it goes on to boot, running.

    Deferring by HANDLER rather than by `pthread_sigmask` is deliberate: a
    blocked mask survives fork/exec (measured), so masking here would hand
    every probe a SIGINT it could no longer receive. Swapping the handler
    leaves the child alone, since exec resets handlers anyway.

    Only the main thread may hold a signal handler, and only the main
    thread is ever sent KeyboardInterrupt — so off it this is a no-op,
    which is exactly the `--jobs` worker case.
    """

    def __init__(self) -> None:
        self._caught = False
        self._previous = None
        self._armed = False

    def _record(self, signum, frame) -> None:
        self._caught = True

    def __enter__(self) -> "_DeferSigint":
        if threading.current_thread() is threading.main_thread():
            try:
                self._previous = signal.signal(signal.SIGINT, self._record)
                self._armed = True
            except (ValueError, OSError):
                self._armed = False
        return self

    def __exit__(self, exc_type, exc, tb) -> bool:
        if self._armed:
            signal.signal(signal.SIGINT, self._previous)
            self._armed = False
        # Only synthesize the interrupt when nothing else is already
        # propagating: that exception reaches the same teardown, and
        # replacing it would hide why the launch failed.
        if self._caught and exc_type is None:
            raise KeyboardInterrupt
        return False


def _terminate_probe(proc: subprocess.Popen, pgid: int,
                     grace: float | None = None) -> str:
    """End a probe that is still running, and collect its output.

    SIGTERM the group, give it ``grace`` to leave, SIGKILL what is left.
    The LEADER is reaped here (via ``communicate``) as well as signalled,
    which is what lets `reap_group` afterwards find an empty group at once
    instead of waiting out its own grace on a zombie.

    Shared by the two paths that have to stop a live probe: the wall-clock
    timeout, and a probe launched into an already-starting shutdown.
    """
    if grace is None:
        grace = GROUP_GRACE
    _signal_group(pgid, signal.SIGTERM)
    try:
        out, _ = proc.communicate(timeout=grace)
    except subprocess.TimeoutExpired:
        _signal_group(pgid, signal.SIGKILL)
        out, _ = proc.communicate()
    return out


class ProbeGroups:
    """The probe process groups running right now, plus the stop flag.

    Ctrl-C reaches the RUNNER only: every probe is launched into its own
    session (``start_new_session=True``), so the terminal's SIGINT never
    touches a probe, let alone the engine it booted. The runner therefore
    has to signal them itself — in the sequential path and the ``--jobs``
    path alike — and has to stop worker threads from picking up the next
    queued probe while it does.
    """

    def __init__(self) -> None:
        self._lock = threading.Lock()
        self._pgids: set[int] = set()
        self.stopping = threading.Event()

    def add(self, pgid: int) -> None:
        with self._lock:
            self._pgids.add(pgid)

    def discard(self, pgid: int) -> None:
        with self._lock:
            self._pgids.discard(pgid)

    def reap_all(self) -> None:
        with self._lock:
            pgids = sorted(self._pgids)
        for pgid in pgids:
            reap_group(pgid)


def probe_protocol_env(event_path: str | None = None,
                       artifact_dir: str | None = None,
                       engine_log_dir: str | None = None,
                       rts_caps: int | None = None) -> dict[str, str]:
    """The `probe-result/v1` environment for one harnessed run (#1425).

    A migrated probe reads these to decide where its event stream,
    artifacts and engine logs go, and how many RTS capabilities every
    engine it boots gets. All four are optional and an empty result
    means "no protocol wiring", which is what an ordinary
    `run_probes.py` run passes.
    """
    env: dict[str, str] = {}
    if event_path is not None:
        env[probe_protocol.ENV_EVENTS] = str(event_path)
    if artifact_dir is not None:
        env[probe_protocol.ENV_ARTIFACT_DIR] = str(artifact_dir)
    if engine_log_dir is not None:
        env[probe_protocol.ENV_ENGINE_LOG_DIR] = str(engine_log_dir)
    if rts_caps is not None:
        env[probe_protocol.ENV_RTS_CAPS] = str(rts_caps)
    return env


def run_one(script: str, port: int | None, timeout: float,
            groups: ProbeGroups | None = None, *,
            event_path: str | None = None,
            artifact_dir: str | None = None,
            engine_log_dir: str | None = None,
            rts_caps: int | None = None,
            hold_env: dict[str, str] | None = None):
    """Launch one probe, capture it, and reap its whole process group.

    The four `probe-result/v1` keyword parameters are that protocol's
    wiring (#1425), handed to the child through the environment so a
    migrated probe needs no new command-line flags. `hold_env` is the
    same idea for the resources an ancestor holds exclusively on the
    child's behalf (#1570), which only matters to a probe that nests
    another runner. Every one defaults to None, which passes no
    environment override at all — so every pre-existing positional
    caller behaves exactly as it did.
    """
    cmd = ["python3", os.path.join("tools", script)]
    if port is not None:
        cmd += ["--port", str(port)]
    # `run_one` is the ONE authority on a child's protocol wiring: the
    # inherited environment's own SYNARCHY_PROBE_* variables are dropped
    # first, so a stale export in the operator's shell cannot silently
    # push an ordinary `run_probes.py` run into protocol mode (where a
    # probe would stop printing the human output the runner's failure
    # tail exists to show).
    protocol_env = probe_protocol_env(event_path, artifact_dir,
                                      engine_log_dir, rts_caps)
    child_env = {k: v for k, v in os.environ.items()
                 if k not in probe_protocol.PROTOCOL_ENV_VARS
                 and k not in resources.RUNNER_ENV_VARS}
    child_env.update(protocol_env)
    # The engine every probe launches (#1570), resolved once by
    # `tools/run_probes.py`'s preflight, and read from the resource owner's
    # single cell. Stripped-then-set for the same reason the protocol
    # variables are: an operator's stale export must not decide which
    # binary a sweep runs. A caller that resolved nothing leaves the child
    # to prepare its own executable (#1913) — still one probe at a time
    # and still no concurrent Cabal, because that preparation takes
    # `cabal-build` exclusively before it builds. `tools/deflake.py`
    # fills this in for the de-flake lab's own runs, ahead of the hold
    # its measurement takes, so no child of a measurement prepares
    # anything.
    engine_exe = resources.ENGINE_EXECUTABLE
    if engine_exe is not None:
        child_env[probe_engine.ENV_ENGINE_EXE] = engine_exe
    if hold_env:
        child_env.update(hold_env)
    if groups is not None and groups.stopping.is_set():
        # The runner is tearing down; a queued worker must not boot one
        # more engine on its way out.
        return False, False, 0.0, "(not started: the runner is shutting down)\n"
    start = time.time()
    proc = None
    pgid = None
    launched_into_shutdown = False
    timed_out = False
    try:
        # The launch window is held against Ctrl-C: a KeyboardInterrupt
        # between the spawn and `pgid` being recorded would leave a probe
        # nothing knows about — outside `groups`, and outside this
        # `finally` — surviving the interrupt with its engine.
        with _DeferSigint():
            proc = subprocess.Popen(
                cmd, cwd=probe_engine.REPO_ROOT,
                stdout=subprocess.PIPE, stderr=subprocess.STDOUT,
                text=True, start_new_session=True, env=child_env,
            )
            # `start_new_session=True` makes the child a session AND
            # process-group leader, so its pgid IS its pid. Capture it here
            # rather than calling os.getpgid(proc.pid) later, for two
            # reasons: once communicate() reaps the leader that call
            # raises, leaving the descendants we still have to reap
            # unaddressable; and calling it right now could race the
            # child's own setsid() and hand back the RUNNER's group.
            pgid = proc.pid
            # Did the runner begin shutting down between the check above
            # and this Popen? `reap_all` may then already have taken its
            # snapshot without this group in it, so this call has to own
            # the escalation itself.
            if groups is not None:
                groups.add(pgid)
                launched_into_shutdown = groups.stopping.is_set()
        # Leaving that block re-raises a deferred Ctrl-C — here, inside the
        # try, so the reap below owns it.
        if launched_into_shutdown:
            # A bare SIGTERM is not enough: a probe that ignores it would
            # otherwise sit in communicate() for the whole `--timeout`
            # (900 s by default) before the finally below could escalate,
            # while the interrupted runner waits on this worker.
            out = _terminate_probe(proc, pgid)
            rc = -1
        else:
            try:
                out, _ = proc.communicate(timeout=timeout)
                rc = proc.returncode
            except subprocess.TimeoutExpired:
                timed_out = True
                out = _terminate_probe(proc, pgid)
                rc = -1
        # Measured BEFORE the reap below, so a probe's recorded elapsed
        # time stays the probe's own and never absorbs teardown's grace.
        elapsed = time.time() - start
    finally:
        # Every exit from here on, including a KeyboardInterrupt raised in
        # the runner while communicate() was blocked, and one deferred
        # across the launch window above. `pgid` is None only when the
        # spawn itself failed, in which case there is nothing to reap.
        if pgid is not None:
            reap_group(pgid)
            if groups is not None:
                groups.discard(pgid)
    ok = (rc == 0) and not timed_out
    return ok, timed_out, elapsed, out or ""
