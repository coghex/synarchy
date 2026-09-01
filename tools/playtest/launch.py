"""Cold-boot setup and the PLAYER-READY boundary for the playtest harness (#1539).

A playtest starts from a cold worktree: `cabal` may compile the whole
game, the process then has to come up, and the real UI flow (loading
screen -> title/main menu) has to assemble before a screenshot means
anything to a player. None of that is play, and none of it may be
charged to the player's session budget.

This module owns that pre-ready sequence and the one observable
boundary that ends it. It is deliberately playtest-LOCAL rather than a
change to `probelib.boot`: ~85 behavior probes call `boot` directly and
its contract for them (a 180 s `ready_timeout` default, `sys.exit` on
failure, per-port log defaulting) is unchanged by this file.

Three phases, in order, each with its own name in the trace:

1. ``build``  — `cabal build exe:synarchy`, then `cabal list-bin` for the
   executable. `probelib.boot` runs `cabal run -v0`, which both hides the
   build output and counts compilation against the READY deadline; this
   phase separates the two, and its output lands in the trace's
   ``setup.log`` where `-v0` cannot suppress it.
2. ``engine`` — spawn the built executable and wait for the debug
   console's ``READY``. The process is recorded on the engine object the
   instant it exists, so even a failure in this phase can still reap it.
3. ``render`` — poll for POSITIVE evidence that a first frame could
   actually be handed to a player: an initialized menu surface, a
   non-empty widget registry, and a screenshot that really succeeds.
   Elapsed time never satisfies this.

The whole sequence runs under ONE budget (``--setup-timeout``), which is
neither `probelib`'s 180 s probe default nor derived from
``--max-seconds``: a setup longer than the entire session budget must
still leave that session budget whole. It exists only so a wedged
compiler or engine cannot hang forever — it is an infrastructure
watchdog, never a playtest timer, and tripping it is reported as a
setup failure, never as ``time_budget_exhausted``.

A pre-ready failure reaps the WHOLE spawned process group (the engine is
its own session leader, so `probe_runner_lifecycle.reap_group` addresses it even
when the immediate child is already gone) and waits for the listener
port to actually release, so the next attempt does not fail as an
unrelated "exited before READY" (#1190/#1323).
"""
from __future__ import annotations

import os
import socket
import subprocess
import sys
import time

HERE = os.path.dirname(os.path.abspath(__file__))
TOOLS = os.path.dirname(HERE)
REPO_ROOT = os.path.dirname(TOOLS)
for _path in (HERE, TOOLS):
    if _path not in sys.path:
        sys.path.insert(0, _path)

from engine import (CONSOLE_READ_TIMEOUT, EngineCrash,  # noqa: E402
                    SCREENSHOT_TIMEOUT)
from probelib import GUI_PORT  # noqa: E402
from probe_runner_lifecycle import _DeferSigint, reap_group  # noqa: E402

# The pre-ready budget. NOT probelib's 180 s probe READY default (which
# is exactly what killed the cold-worktree run in #1539) and never
# derived from --max-seconds: it has to admit a full from-scratch build
# of this repository. Overridable with --setup-timeout.
DEFAULT_SETUP_TIMEOUT = 1800.0

# How often the render phase re-probes for player-readiness.
READY_POLL_INTERVAL = 0.5

# How often the engine phase re-reads the log for READY. Both intervals
# are clamped to whatever is left of the setup deadline, so a budget
# smaller than one interval still ends on time.
CONSOLE_POLL_INTERVAL = 0.4

# How long a pre-ready teardown waits for the listener socket to free.
PORT_RELEASE_TIMEOUT = 10.0

# The three pre-ready phases, in order. Requirement 4 of #1539: a setup
# failure must say unambiguously which one it happened in.
SETUP_PHASES = ("build", "engine", "render")

# One stop_reason per phase. All three are SETUP outcomes: they are never
# time_budget_exhausted / decision_timeout / any other player-session
# reason, because no player session ever started.
SETUP_STOP_REASONS = {
    "build": "setup_build_failed",
    "engine": "setup_engine_failed",
    "render": "setup_render_failed",
}
SETUP_STOP_REASON_SET = frozenset(SETUP_STOP_REASONS.values())


class SetupFailure(Exception):
    """A failure BEFORE the player-ready boundary.

    ``phase`` is one of SETUP_PHASES and picks the stop reason. ``kind``
    is the finer classification requirement 6 asks the trace to preserve
    — a build failure, a build/setup timeout, an executable exit, a
    debug-console readiness failure and a rendered-readiness failure are
    each their own value, so the retained logs are read with the right
    question in mind.
    """

    def __init__(self, phase: str, kind: str, detail: str,
                 *, timed_out: bool = False):
        if phase not in SETUP_PHASES:
            raise ValueError(f"unknown setup phase {phase!r}")
        super().__init__(f"setup {phase} failed ({kind}): {detail}")
        self.phase = phase
        self.kind = kind
        self.detail = detail
        self.timed_out = timed_out
        self.stop_reason = SETUP_STOP_REASONS[phase]

    def as_meta(self, **extra) -> dict:
        """The trace record for this failure."""
        record = {
            "phase": self.phase,
            "kind": self.kind,
            "detail": self.detail,
            "timed_out": self.timed_out,
            "stop_reason": self.stop_reason,
        }
        record.update(extra)
        return record


def log_tail(path: str, lines: int = 80) -> str:
    """The last ``lines`` of a setup/engine log, or "" if unreadable."""
    try:
        with open(path, errors="replace") as f:
            return "".join(f.readlines()[-lines:])
    except OSError:
        return ""


def _remaining(deadline: float, clock) -> float:
    return deadline - clock()


# --------------------------------------------------------------------------
# Phase 1: build
# --------------------------------------------------------------------------
def _run_setup_command(cmd: list[str], repo_root: str, deadline: float, clock,
                       *, stdout=None, capture: bool = False):
    """Run one setup subprocess in its OWN process group, and reap that
    GROUP on timeout or interruption.

    `cabal` is a process TREE — it spawns `ghc`, which spawns more — so
    killing the immediate child leaves compilation running (and, on a
    Ctrl-C, running unattended after the harness is gone). Spawning into
    a new session makes the child's pid its process-group id, which is
    what lets `probe_runner_lifecycle.reap_group` address every descendant (#1323).
    The BaseException arm is what covers Ctrl-C: the build blocks inside
    `communicate`, so the interruption lands here rather than anywhere a
    later teardown could see it.

    Returns ``(returncode, stdout_text, stderr_text)``; the text values
    are empty unless ``capture`` is set.
    """
    left = _remaining(deadline, clock)
    label = " ".join(cmd[:2])
    if left <= 0:
        raise SetupFailure(
            "build", "build_timeout",
            f"the setup budget was already spent before `{label}` could run",
            timed_out=True)
    proc = None
    try:
        # The spawn window is held against Ctrl-C: the interpreter checks
        # for signals BETWEEN bytecodes, so an interrupt landing after
        # `Popen` forks but before `proc` names it would leave a build
        # tree — in its own session, so it survives our own death — that
        # nothing here or in `main()` could ever reap.
        with _DeferSigint():
            proc = subprocess.Popen(
                cmd, cwd=repo_root,
                stdout=subprocess.PIPE if capture else stdout,
                stderr=subprocess.PIPE if capture else subprocess.STDOUT,
                text=True if capture else None,
                start_new_session=True)
        out, err = proc.communicate(timeout=left)
    except subprocess.TimeoutExpired:
        _reap_setup_process(proc)
        raise SetupFailure(
            "build", "build_timeout",
            f"`{label}` did not finish inside the setup budget",
            timed_out=True) from None
    except OSError as e:
        _reap_setup_process(proc)
        raise SetupFailure("build", "build_failed",
                           f"could not run `{label}`: {e}") from e
    except BaseException:
        # A deferred Ctrl-C re-raised on leaving the block above, or
        # anything else: the build tree must not outlive us.
        _reap_setup_process(proc)
        raise
    return proc.returncode, out or "", err or ""


def _reap_setup_process(proc: subprocess.Popen | None) -> None:
    """Terminate a setup subprocess's whole group and drain its pipes.

    Tolerates ``None``: an exec that never produced a process has
    nothing to reap."""
    if proc is None:
        return
    try:
        reap_group(proc.pid)
    except (OSError, ValueError):
        pass
    try:
        proc.communicate(timeout=10)
    except (subprocess.SubprocessError, OSError, ValueError):
        pass


def build_executable(repo_root: str, log_path: str, deadline: float,
                     clock=time.monotonic) -> str:
    """Compile the game and answer with the executable's path.

    Runs at cabal's DEFAULT verbosity (not `probelib.boot`'s `-v0`) with
    stdout+stderr captured into ``log_path``, which is the trace's own
    ``setup.log``: a build failure or a build that ran out of budget
    leaves its real output behind instead of an unexplained empty engine
    log.
    """
    with open(log_path, "w") as logf:
        logf.write(f"$ cabal build exe:synarchy    (cwd {repo_root})\n")
        logf.flush()
        code, _, _ = _run_setup_command(
            ["cabal", "build", "exe:synarchy"], repo_root, deadline, clock,
            stdout=logf)
    if code != 0:
        raise SetupFailure(
            "build", "build_failed",
            f"cabal build exited {code}; see {os.path.basename(log_path)}")
    return _locate_executable(repo_root, log_path, deadline, clock)


def _locate_executable(repo_root: str, log_path: str, deadline: float,
                       clock) -> str:
    code, out, err = _run_setup_command(
        ["cabal", "list-bin", "exe:synarchy"], repo_root, deadline, clock,
        capture=True)
    with open(log_path, "a") as logf:
        logf.write(f"\n$ cabal list-bin exe:synarchy -> {code}\n")
        logf.write(out)
        logf.write(err)
    lines = [ln.strip() for ln in out.splitlines() if ln.strip()]
    exe = lines[-1] if lines else ""
    if code != 0 or not exe or not os.path.isfile(exe):
        raise SetupFailure(
            "build", "build_failed",
            f"cabal list-bin did not name a built executable (exit "
            f"{code}, got {exe!r}); see {os.path.basename(log_path)}")
    return exe


# --------------------------------------------------------------------------
# Phase 2: engine process + debug console
# --------------------------------------------------------------------------
def start_engine(eng, exe: str, deadline: float, *, repo_root: str = REPO_ROOT,
                 clock=time.monotonic, sleep=time.sleep) -> None:
    """Spawn the built executable on ``eng.port`` and block until READY.

    The process is recorded on ``eng.proc`` the instant `Popen` returns,
    BEFORE the READY wait, so a failure in this phase is still reapable
    by `teardown_setup` — the leak `probelib.boot`'s bare `proc.kill()`
    leaves behind (#1323).

    It is spawned into its OWN session (`start_new_session=True`), which
    makes its pid its process-group id and lets `reap_group` address
    every descendant, and it is given the resource root explicitly so a
    playtest launched from any working directory finds `scripts/`,
    `assets/`, `data/` and `config/`.
    """
    if eng.port == GUI_PORT:
        # `probelib.boot`'s own guard, kept: 8008 is the user's graphical
        # instance. `main()` rejects it at argument-parse time; this is
        # the backstop for any other caller.
        raise SetupFailure(
            "engine", "refused_port",
            f"refusing to boot on port {GUI_PORT} (the GUI port); "
            "pass a 9xxx --port")
    cmd = [exe, *eng.boot_mode(), "--port", str(eng.port),
           "--resource-root", repo_root]
    try:
        logf = open(eng.log_path, "w")
    except OSError as e:
        raise SetupFailure("engine", "engine_exited",
                           f"could not open the engine log: {e}") from e
    try:
        # Same held spawn window as the build: an interrupt between the
        # fork and `eng.proc` naming it would leave an engine — in its
        # own session — that neither this function nor `main()`'s
        # teardown could reach.
        with _DeferSigint():
            eng.proc = subprocess.Popen(cmd, cwd=repo_root, stdout=logf,
                                        stderr=subprocess.STDOUT,
                                        start_new_session=True)
    except OSError as e:
        raise SetupFailure("engine", "engine_exited",
                           f"could not start {exe}: {e}") from e
    except BaseException:
        # A deferred Ctrl-C, re-raised now that eng.proc is recorded.
        teardown_setup(eng)
        raise
    finally:
        # The child holds its own dup; the parent's copy would otherwise
        # stay open for the whole session.
        logf.close()
    while True:
        try:
            with open(eng.log_path, errors="replace") as f:
                ready = "READY" in f.read()
        except OSError:
            ready = False
        left = _remaining(deadline, clock)
        outcome, failure = console_wait_step(ready, eng.proc.poll(), left <= 0)
        if outcome == "accept":
            return
        if outcome == "fail":
            kind, detail = failure
            raise SetupFailure("engine", kind, detail,
                               timed_out=(kind == "console_timeout"))
        # Never sleep past the deadline: a --setup-timeout smaller than
        # one polling interval must still end on time.
        sleep(max(0.0, min(CONSOLE_POLL_INTERVAL, left)))


def console_wait_step(ready: bool, exit_code: int | None, expired: bool):
    """One iteration of the READY wait, as a pure decision.

    Returns ``("accept", None)``, ``("wait", None)``, or
    ``("fail", (kind, detail))``. Split out so the ORDERING — in
    particular that a READY arriving only after the setup budget expired
    is a timeout rather than an accepted boundary crossing — is covered
    offline, with no process to spawn and no clock to race.
    """
    if ready and not expired:
        return "accept", None
    if exit_code is not None:
        return "fail", ("engine_exited",
                        f"the engine exited {exit_code} before the debug "
                        "console printed READY; see engine.log")
    if ready:
        return "fail", ("console_timeout",
                        "the debug console printed READY only after the "
                        "setup budget had already expired; see engine.log")
    if expired:
        return "fail", ("console_timeout",
                        "the engine is still running but its debug console "
                        "never printed READY inside the setup budget; see "
                        "engine.log")
    return "wait", None


# --------------------------------------------------------------------------
# Phase 3: the player-ready boundary
# --------------------------------------------------------------------------
def probe_player_ready(eng, screenshot_path: str, deadline: float | None = None,
                       clock=time.monotonic) -> bool:
    """POSITIVE evidence that a first frame could be handed to a player.

    All of these must hold, and elapsed time counts for nothing:

    * startup boot has FINISHED and the main-menu surface itself is
      initialized (`ui_manager.startupBootDone` and
      `moduleReady.mainMenu`, both set by `finishStartupBoot`), and
    * `ui_manager.currentMenu` names a menu, and
    * a non-empty widget registry (`ui.registry.dumpWidgets()` — an
      interactive surface actually exists), and
    * a screenshot that really succeeds and reports a positive size.

    The first condition is what keeps the LOADING SCREEN out.
    `currentMenu` is initialized to `"main"` at module load, long
    before any UI exists, and the startup loading screen is shown
    without changing it — and it carries visible labels, so
    `dumpWidgets()` is non-empty there too. A menu name plus arbitrary
    visible widgets would therefore accept a progress bar as the
    player's first frame; only `moduleReady.mainMenu` says the main
    menu itself was built.

    Both console reads are harness-side ORACLE reads: they are recorded
    for the critic and never surfaced to the player, so using them here
    leaves the oracle-blindness contract exactly as it was — that rule
    forbids showing oracle data to the player, not reading it. The
    ui_manager singleton is read through `package.loaded`, not `require`:
    this runs DURING boot, and `require` would force-load a module the
    engine has not dofile'd yet (ui_manager.lua self-registers into
    package.loaded, and its own submodule requires would run early). A
    read that can only observe is the right shape for a readiness probe.
    The registry is a leaf helper with no such ordering, so it keeps the
    oracle's own spelling.

    ``screenshot_path`` is a SETUP artifact (`trace.setup_frame_path()`),
    deliberately not `frames/turn_0001.png`: this frame is never a turn
    observation, produces no `turns.jsonl`/`replay.jsonl` record, and is
    never shown to the player.

    ``deadline`` is the setup budget's own monotonic deadline, and every
    console read and the screenshot is bounded by whatever is LEFT of it
    — re-measured before each one. Their engine-side defaults are 15 s
    and 20 s, so a stalled-but-alive console would otherwise run one
    probe cycle 50 s past a smaller `--setup-timeout`. Readiness proven
    only after the deadline has passed does not count either: the probe
    answers False, and the caller reports the setup timeout.

    A console error means "not ready yet", so the caller keeps polling
    until its own budget decides; only the caller's liveness check ends
    the wait early.
    """
    def io_timeout(default: float) -> float | None:
        """The next read's timeout, or None when the budget is spent.

        ONE clock read decides both, deliberately: checking "is there
        budget left?" and then computing the timeout from a SECOND read
        leaves a window where the deadline expires between them and a
        non-positive timeout reaches `socket.create_connection`, which
        raises ValueError — an exception `launch_player_ready` does not
        classify, so it would escape as a generic error and skip the
        pre-ready teardown entirely.
        """
        if deadline is None:
            return default
        remaining = deadline - clock()
        return min(default, remaining) if remaining > 0 else None

    def spent() -> bool:
        return deadline is not None and deadline - clock() <= 0

    try:
        budget = io_timeout(CONSOLE_READ_TIMEOUT)
        if budget is None:
            return False
        state = eng.lua(
            'local m = package.loaded["scripts.ui_manager"]; '
            'if not m then return nil end; '
            'return {menu = m.currentMenu, '
            'bootDone = m.startupBootDone and true or false, '
            'mainMenuReady = (m.moduleReady and m.moduleReady.mainMenu) '
            'and true or false}', timeout=budget)
        if not isinstance(state, dict):
            return False
        if state.get("bootDone") is not True:
            return False
        if state.get("mainMenuReady") is not True:
            return False
        menu = state.get("menu")
        if not isinstance(menu, str) or not menu.strip():
            return False
        budget = io_timeout(CONSOLE_READ_TIMEOUT)
        if budget is None:
            return False
        widgets = eng.lua('return require("scripts.ui.registry").dumpWidgets()',
                          timeout=budget)
        if not isinstance(widgets, list) or not widgets:
            return False
        budget = io_timeout(SCREENSHOT_TIMEOUT)
        if budget is None:
            return False
        size = eng.screenshot(screenshot_path, timeout=budget)
    except EngineCrash:
        return False
    if spent():
        return False
    return bool(size) and size[0] > 0 and size[1] > 0


def launch_player_ready(eng, trace, *,
                        setup_timeout: float = DEFAULT_SETUP_TIMEOUT,
                        repo_root: str = REPO_ROOT,
                        build=None, start=None, ready=None,
                        clock=time.monotonic, sleep=time.sleep,
                        poll_interval: float = READY_POLL_INTERVAL,
                        log=print) -> float:
    """Take a cold instance through all three setup phases to player-ready.

    Returns the setup duration in seconds and stamps `trace.mark_loaded()`
    — the boundary itself. Every player-session budget starts strictly
    after this returns; nothing here consumes any of them.

    ``build`` / ``start`` / ``ready`` replace the three phases' real
    implementations and ``clock`` / ``sleep`` replace the wall clock, so
    the whole sequence — including a setup far longer than the session
    budget, and each phase's failure — is drivable offline by
    `run.py --selftest` with no build, no GPU, no window and no network.
    """
    if setup_timeout <= 0:
        raise ValueError("setup_timeout must be positive")
    started = clock()
    deadline = started + setup_timeout
    try:
        return _launch_player_ready(
            eng, trace, started=started, deadline=deadline,
            setup_timeout=setup_timeout,
            repo_root=repo_root, build=build, start=start, ready=ready,
            clock=clock, sleep=sleep, poll_interval=poll_interval, log=log)
    except SetupFailure:
        # Classified: the caller's own handler records it and tears down.
        raise
    except BaseException:
        # Nothing else should escape setup, but the launcher owns the
        # instance until the boundary, so anything that does must not
        # leave an engine holding the port behind it.
        teardown_setup(eng)
        raise


def _launch_player_ready(eng, trace, *, started: float, deadline: float,
                         setup_timeout: float,
                         repo_root: str, build, start, ready,
                         clock, sleep, poll_interval: float, log) -> float:
    """The three phases themselves — see `launch_player_ready`."""

    log(f"playtest: setup 1/3 — building (setup budget {setup_timeout:.0f}s, "
        "separate from the player-session budget)")
    exe = build() if build is not None else build_executable(
        repo_root, trace.setup_log_path(), deadline, clock=clock)

    log("playtest: setup 2/3 — engine process + debug console")
    if start is not None:
        start(exe)
    else:
        start_engine(eng, exe, deadline, repo_root=repo_root,
                     clock=clock, sleep=sleep)

    log("playtest: setup 3/3 — waiting for the first player-ready frame")
    probe = ready if ready is not None else (
        lambda: probe_player_ready(eng, trace.setup_frame_path(),
                                   deadline=deadline, clock=clock))
    while True:
        if not eng.alive():
            raise SetupFailure(
                "render", "render_engine_exited",
                "the engine exited after READY but before it could render a "
                "player-ready frame; see engine.log")
        # The budget is checked BEFORE probing and again after: a probe
        # cycle is itself I/O, so readiness that only arrives once the
        # watchdog has expired must not be allowed to cross the boundary
        # (the default probe additionally bounds each of its own reads by
        # what is left of this same deadline).
        if _remaining(deadline, clock) <= 0:
            raise SetupFailure(
                "render", "render_timeout",
                "the game never became player-ready (menu surface + widgets "
                "+ a successful screenshot) inside the setup budget",
                timed_out=True)
        if probe():
            if _remaining(deadline, clock) <= 0:
                raise SetupFailure(
                    "render", "render_timeout",
                    "the game became player-ready only after the setup "
                    "budget had already expired", timed_out=True)
            break
        # Never sleep past the deadline: a --setup-timeout smaller than
        # one poll interval must still report on time rather than
        # overshoot by a whole interval.
        sleep(max(0.0, min(poll_interval, _remaining(deadline, clock))))

    elapsed = clock() - started
    trace.mark_loaded()
    log(f"playtest: player-ready after {elapsed:.1f}s of setup — the "
        "player-session budgets start now")
    return elapsed


# --------------------------------------------------------------------------
# Pre-ready teardown
# --------------------------------------------------------------------------
def wait_port_released(port: int, timeout: float = PORT_RELEASE_TIMEOUT,
                       clock=time.monotonic, sleep=time.sleep) -> bool:
    """Block until nothing answers on ``port``. True if it actually freed.

    Killing the process is not the same as the listener being gone, and
    a port still held by a dying engine is what makes the NEXT attempt
    fail as an unrelated "exited before READY" under #1190.
    """
    deadline = clock() + timeout
    while True:
        # A timed BLOCKING connect. `socket.settimeout` + `connect_ex`
        # looks equivalent and is not: the timeout puts the socket in
        # non-blocking mode, so `connect_ex` answers EINPROGRESS at once
        # and every held port reads as free.
        try:
            with socket.create_connection(("localhost", port), timeout=0.5):
                pass
        except OSError:
            return True
        if clock() >= deadline:
            return False
        sleep(0.2)


def teardown_setup(eng, *, port_timeout: float = PORT_RELEASE_TIMEOUT) -> bool:
    """Tear a pre-ready instance down: reap the group, then free the port.

    `probelib.boot`'s failure path kills only the process it spawned; the
    engine below it survives holding the listener. This reaps the whole
    spawned process group (`probe_runner_lifecycle.reap_group`, #1323) and does not
    return until the port is observed released — or the wait runs out,
    which it reports rather than hiding.
    """
    proc = getattr(eng, "proc", None)
    released = True
    if proc is not None:
        try:
            reap_group(proc.pid)
        except (OSError, ValueError):
            pass
        try:
            proc.wait(timeout=5)
        except (subprocess.TimeoutExpired, OSError, ValueError):
            pass
        eng.proc = None
        if getattr(eng, "port", 0):
            released = wait_port_released(eng.port, port_timeout)
    return released
