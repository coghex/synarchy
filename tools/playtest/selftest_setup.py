#!/usr/bin/env python3
"""Self-test component: setup, readiness, deadlines, lifecycle
metadata and process-tree teardown (#2040).

Owns the player-ready boundary and the setup/session split (#1539) —
the setup watchdog, the readiness decision table, every pre-ready
failure kind, the spawn window, the process-tree reap, chronological
lifecycle metadata, and legacy-trace collation.

Offline: the three setup phases and the wall clock are injected, so
nothing builds, boots, opens a window, binds a real engine port, or
calls a model."""
from __future__ import annotations

import json
import os
import socket
import sys
import tempfile
import time

_HERE = os.path.dirname(os.path.abspath(__file__))
if _HERE not in sys.path:
    sys.path.insert(0, _HERE)

from engine import EngineCrash, FakeEngine  # noqa: E402
from preanalysis import write_inspection_plan  # noqa: E402
from session import run_session  # noqa: E402
from trace import SessionTrace, load_meta  # noqa: E402
from usage import update_usage_log  # noqa: E402
import agent as agent_mod  # noqa: E402
import launch as launch_mod  # noqa: E402

NAME = "setup"


def run(check) -> None:
    """Run every setup/readiness/teardown check into `check`."""
    with tempfile.TemporaryDirectory() as tmp:
        # ------------------------------------------------------------
        # 8. the player-ready boundary and the setup/session split
        # (#1539). All of it offline: the three setup phases and the
        # wall clock are injected, so none of this builds, boots, opens
        # a window, or calls a model.
        # ------------------------------------------------------------
        class FakeClock:
            """A monotonic clock the test advances by hand."""

            def __init__(self, step=10.0):
                self.now = 0.0
                self.step = step

            def __call__(self):
                return self.now

            def sleep(self, _seconds):
                self.now += self.step

        class SleepClock:
            """A clock that advances by whatever the code sleeps for —
            so a loop that clamps its sleep to a deadline converges,
            and one that does not overshoots visibly."""

            def __init__(self):
                self.now = 0.0
                self.slept: list = []

            def __call__(self):
                return self.now

            def sleep(self, seconds):
                self.slept.append(seconds)
                self.now += max(seconds, 0.01)

        class ReadyAfter:
            """Player-readiness that arrives only on the Nth probe —
            positive evidence, never elapsed time."""

            def __init__(self, probes):
                self.left = probes
                self.calls = 0

            def __call__(self):
                self.calls += 1
                self.left -= 1
                return self.left <= 0

        def fresh(name, meta=None):
            d = os.path.join(tmp, name)
            return d, SessionTrace(d, dict(meta or {"mode": "selftest-ready"}))

        def _tree_stub(name):
            """A stand-in setup process that leaves a background child.

            Killing only the immediate process would leave that child
            behind, which is exactly what the group reap has to prevent.
            The pid is published by ATOMIC RENAME so a reader never sees
            the empty file `>` creates before `echo` has written it."""
            path = os.path.join(tmp, f"{name}.sh")
            pid_file = os.path.join(tmp, f"{name}.pid")
            with open(path, "w") as f:
                f.write("#!/bin/sh\nsleep 120 &\nprintf '%s' \"$!\" > "
                        f"'{pid_file}.tmp'\nmv '{pid_file}.tmp' "
                        f"'{pid_file}'\nsleep 120\n")
            os.chmod(path, 0o755)
            return path, pid_file

        def _await_pid(pid_file, budget=10.0):
            end = time.monotonic() + budget
            while True:
                try:
                    with open(pid_file) as f:
                        return int(f.read().strip())
                except (OSError, ValueError):
                    if time.monotonic() >= end:
                        return None
                    time.sleep(0.05)

        def launch_offline(trace, *, ready, clock, setup_timeout=100000.0,
                           build=None, start=None, eng=None):
            return launch_mod.launch_player_ready(
                eng or FakeEngine(), trace, setup_timeout=setup_timeout,
                build=build or (lambda: "/nonexistent/synarchy"),
                start=start or (lambda exe: None),
                ready=ready, clock=clock, sleep=clock.sleep,
                poll_interval=0.0, log=lambda *a, **k: None)

        # 8a. a setup FAR longer than --max-seconds still hands the
        # session its complete budget. The fake clock burns 5000 s
        # before readiness; the session that follows is given 1 s and
        # must still run its whole turn budget, which it could not do if
        # the session clock had been anchored anywhere in setup.
        slow_dir, slow_trace = fresh("ready_slow_setup")
        slow_clock = FakeClock(step=1000.0)
        slow_setup = launch_offline(slow_trace, ready=ReadyAfter(5),
                                    clock=slow_clock)
        slow_reason = run_session(FakeEngine(), agent_mod.ScriptedAgent(
            [{"do": "wait"}]), slow_trace, turns=3, dt=0.0, max_seconds=1.0,
            memory_turns=4, stuck_k=99, settle=0.0)
        slow_trace.finish(slow_reason, time_budget_seconds=1.0)
        slow_meta = load_meta(slow_dir)
        check("setup longer than --max-seconds leaves the session budget whole",
              slow_setup > 1.0 and slow_reason == "turn_budget_exhausted"
              and slow_meta.get("turns") == 3,
              f"{slow_setup:.0f}s setup, {slow_reason}, "
              f"{slow_meta.get('turns')} turn(s)")

        # 8b. no player call before the boundary. The agent records what
        # the trace knew when it was asked; a non-null loaded_at proves
        # the boundary was already crossed on its very first decision.
        class BoundaryWitness(agent_mod.ScriptedAgent):
            def __init__(self, watched):
                super().__init__([{"do": "wait"}])
                self.watched = watched
                self.loaded_at_first_call = "never called"

            def decide(self, *a, **kw):
                if self.loaded_at_first_call == "never called":
                    self.loaded_at_first_call = self.watched.meta.get("loaded_at")
                return super().decide(*a, **kw)

        wit_dir, wit_trace = fresh("ready_witness")
        wit_clock = FakeClock()
        launch_offline(wit_trace, ready=ReadyAfter(3), clock=wit_clock)
        witness = BoundaryWitness(wit_trace)
        wit_reason = run_session(FakeEngine(), witness, wit_trace, turns=2,
                                 dt=0.0, max_seconds=None, memory_turns=4,
                                 stuck_k=99, settle=0.0)
        wit_trace.finish(wit_reason)
        check("no player decision happens before the player-ready boundary",
              isinstance(witness.loaded_at_first_call, float),
              str(witness.loaded_at_first_call))

        # 8c. chronological lifecycle metadata on a successful session,
        # and all four are unix epoch floats (the clock domain the
        # acceptance arithmetic depends on).
        life = load_meta(wit_dir)
        stamps = [life.get("setup_started_at"), life.get("loaded_at"),
                  life.get("session_started_at"), life.get("ended_at")]
        check("lifecycle stamps are four epoch floats in chronological order",
              all(isinstance(v, float) for v in stamps)
              and stamps == sorted(stamps)
              and abs(stamps[0] - time.time()) < 86400,
              str(stamps))
        check("setup and play durations are independently derivable",
              life["loaded_at"] - life["setup_started_at"] >= 0
              and life["ended_at"] - life["session_started_at"] >= 0)
        check("started_at is retained and still means the start of setup",
              life.get("started_at") == life.get("setup_started_at"))

        # 8d. readiness needs POSITIVE rendered/UI evidence. First: the
        # probe itself against stub engines that each withhold exactly
        # one of the three signals.
        class ReadyEngine(FakeEngine):
            """Boot finished + main menu built + widgets + a working
            screenshot: player-ready. `boot_done`/`main_menu` default to
            True; a LOADING SCREEN is `main_menu=False` — it still
            reports currentMenu "main" (the module-load default) and
            still has visible labels."""

            def __init__(self, menu="main", widgets=None, shoot=True,
                         boot_done=True, main_menu=True):
                super().__init__()
                self._menu = menu
                self._widgets = [{"name": "start"}] if widgets is None else widgets
                self._shoot = shoot
                self._boot_done = boot_done
                self._main_menu = main_menu
                self.shot_paths: list[str] = []
                self.shot_timeouts: list = []
                self.lua_timeouts: list = []

            def lua(self, code, timeout=0):
                self.lua_timeouts.append(timeout)
                if "currentMenu" in code:
                    return {"menu": self._menu,
                            "bootDone": self._boot_done,
                            "mainMenuReady": self._main_menu}
                if "dumpWidgets" in code:
                    return self._widgets
                return {"ok": True}

            def screenshot(self, path, timeout=None):
                if not self._shoot:
                    raise EngineCrash("screenshot failed: no swapchain yet")
                self.shot_paths.append(path)
                self.shot_timeouts.append(timeout)
                return super().screenshot(path)

        probe_dir, probe_trace = fresh("ready_probe")
        ok_eng = ReadyEngine()
        def _probe(**kw):
            return launch_mod.probe_player_ready(
                ReadyEngine(**kw), probe_trace.setup_frame_path())

        check("readiness demands a built main menu, a menu name, widgets "
              "AND a real frame",
              _probe(menu=None) is False
              and _probe(widgets=[]) is False
              and _probe(shoot=False) is False
              and _probe(boot_done=False) is False
              and launch_mod.probe_player_ready(
                  ok_eng, probe_trace.setup_frame_path()) is True)
        # The exact shape the startup LOADING SCREEN presents: currentMenu
        # is "main" (its module-load default, never changed while the
        # loading screen is up) and the screen's own labels make
        # dumpWidgets() non-empty. Accepting that would hand the player a
        # progress bar as its first frame.
        check("the startup loading screen is not player-ready",
              _probe(menu="main", widgets=[{"name": "loading_label"}],
                     main_menu=False) is False)
        check("the readiness frame is a setup artifact, never turn 1's frame",
              ok_eng.shot_paths == [probe_trace.setup_frame_path()]
              and probe_trace.setup_frame_path()
              != probe_trace.frame_path(1)
              and os.path.isfile(probe_trace.setup_frame_path())
              and not os.path.isfile(probe_trace.frame_path(1))
              and not os.path.isfile(os.path.join(probe_dir, "turns.jsonl"))
              and not os.path.isfile(os.path.join(probe_dir, "replay.jsonl")),
              str(ok_eng.shot_paths))

        # ...and the probe's own I/O is bounded by what is LEFT of the
        # setup deadline, not by the engine's 15 s / 20 s per-call
        # defaults: a stalled-but-alive console must not run one probe
        # cycle far past a small --setup-timeout.
        budget_eng = ReadyEngine()
        io_clock = FakeClock(step=0.0)
        launch_mod.probe_player_ready(
            budget_eng, probe_trace.setup_frame_path(),
            deadline=io_clock() + 2.0, clock=io_clock)
        io_timeouts = budget_eng.lua_timeouts + budget_eng.shot_timeouts
        check("every readiness read is bounded by the remaining setup budget",
              len(io_timeouts) == 3
              and all(0 < t <= 2.0 for t in io_timeouts),
              str(io_timeouts))
        spent_eng = ReadyEngine()
        spent_clock = FakeClock(step=0.0)
        check("an expired setup budget makes the probe answer False without "
              "issuing a single read",
              launch_mod.probe_player_ready(
                  spent_eng, probe_trace.setup_frame_path(),
                  deadline=spent_clock() - 1.0, clock=spent_clock) is False
              and spent_eng.lua_timeouts == []
              and spent_eng.shot_paths == [])

        # ...and the budget check and the timeout it produces come from
        # ONE clock read. A clock that expires BETWEEN two reads used to
        # hand `socket.create_connection` a non-positive timeout, which
        # raises ValueError — unclassified, so it escaped setup as a
        # generic error and skipped the pre-ready teardown entirely.
        class TickClock:
            """A clock that advances on every READ, not on sleeps."""

            def __init__(self, step):
                self.now = 0.0
                self.step = step

            def __call__(self):
                value = self.now
                self.now += self.step
                return value

            def sleep(self, _seconds):
                pass

        race_eng = ReadyEngine()
        race_clock = TickClock(step=1.5)     # one tick outlives the budget
        race_ready = launch_mod.probe_player_ready(
            race_eng, probe_trace.setup_frame_path(),
            deadline=race_clock.now + 1.0, clock=race_clock)
        race_timeouts = race_eng.lua_timeouts + race_eng.shot_timeouts
        check("an expiry between the budget check and the timeout it "
              "produces can never yield a non-positive timeout",
              race_ready is False and bool(race_timeouts)
              and all(t > 0 for t in race_timeouts),
              str(race_timeouts))

        # ...and if anything unclassified DID escape setup, the instance
        # is still torn down rather than left holding its port.
        escaped = []

        class TeardownWitness(FakeEngine):
            def __init__(self):
                super().__init__()
                self.torn_down = False

        witness_eng = TeardownWitness()
        witness_eng.proc = "sentinel"        # teardown clears this
        real_teardown = launch_mod.teardown_setup
        launch_mod.teardown_setup = lambda eng, **kw: (
            escaped.append(eng), True)[1]
        _, escape_trace = fresh("setup_escape")
        try:
            launch_offline(escape_trace,
                           ready=lambda: (_ for _ in ()).throw(
                               ValueError("unclassified setup failure")),
                           clock=FakeClock(), eng=witness_eng)
            escape_exc = None
        except BaseException as e:
            escape_exc = e
        finally:
            launch_mod.teardown_setup = real_teardown
        check("an unclassified setup failure still tears the instance down",
              isinstance(escape_exc, ValueError)
              and escaped == [witness_eng]
              and escape_trace.meta.get("loaded_at") is None,
              f"{type(escape_exc).__name__}, torn down {len(escaped)}")

        # ...and readiness that only arrives AFTER the watchdog expired
        # is rejected rather than allowed to cross the boundary.
        late_dir, late_trace = fresh("ready_late")
        late_clock = FakeClock(step=0.0)

        def _ready_but_late():
            late_clock.now += 500.0     # the probe itself outlives the budget
            return True

        try:
            launch_offline(late_trace, ready=_ready_but_late,
                           clock=late_clock, setup_timeout=10.0)
            late_exc = None
        except launch_mod.SetupFailure as e:
            late_exc = e
        late_trace.finish(late_exc.stop_reason if late_exc else "error")
        check("readiness proven after the setup budget expired never crosses "
              "the boundary",
              late_exc is not None and late_exc.kind == "render_timeout"
              and load_meta(late_dir).get("loaded_at") is None,
              str(late_exc))

        # ...and second: elapsed time alone never satisfies it. The
        # clock runs far past the budget while readiness stays False.
        never_dir, never_trace = fresh("ready_never")
        never_clock = FakeClock(step=50.0)
        try:
            launch_offline(never_trace, ready=lambda: False,
                           clock=never_clock, setup_timeout=120.0)
            never_exc = None
        except launch_mod.SetupFailure as e:
            never_exc = e
        never_trace.finish(never_exc.stop_reason if never_exc else "error")
        check("elapsed time alone never satisfies the player-ready boundary",
              never_exc is not None and never_exc.phase == "render"
              and never_exc.kind == "render_timeout"
              and never_exc.timed_out is True
              and never_clock.now > 120.0,
              str(never_exc))

        # ...and the render poll never sleeps past the deadline either:
        # a --setup-timeout smaller than READY_POLL_INTERVAL used to
        # overshoot by a whole interval before reporting render_timeout.
        tight_render = SleepClock()
        tight_render_budget = 0.05     # well under READY_POLL_INTERVAL
        _, tight_render_trace = fresh("ready_tight_poll")
        try:
            launch_mod.launch_player_ready(
                FakeEngine(), tight_render_trace,
                setup_timeout=tight_render_budget,
                build=lambda: "/nonexistent/synarchy",
                start=lambda exe: None, ready=lambda: False,
                clock=tight_render, sleep=tight_render.sleep,
                poll_interval=launch_mod.READY_POLL_INTERVAL,
                log=lambda *a, **k: None)
            tight_render_exc = None
        except launch_mod.SetupFailure as e:
            tight_render_exc = e
        check("the render poll never sleeps past the setup deadline",
              tight_render_exc is not None
              and tight_render_exc.kind == "render_timeout"
              and tight_render_budget < launch_mod.READY_POLL_INTERVAL
              and all(v <= tight_render_budget for v in tight_render.slept),
              f"{tight_render.slept} -> {tight_render_exc}")

        # 8e. every pre-ready failure: zero turns, no replay entries,
        # a phase-specific stop reason, and null lifecycle stamps.
        setup_cases = [
            ("build", launch_mod.SetupFailure(
                "build", "build_failed", "cabal build exited 1"),
             "setup_build_failed"),
            ("engine", launch_mod.SetupFailure(
                "engine", "engine_exited", "engine exited 1 before READY"),
             "setup_engine_failed"),
            ("render", None, "setup_render_failed"),
        ]
        articles = {"build": "a", "engine": "an", "render": "a"}
        for phase, planted, expected_reason in setup_cases:
            article = articles[phase]
            fdir, ftrace = fresh(f"setup_fail_{phase}")
            fclock = FakeClock(step=25.0)

            def boom(*_a, _planted=planted):
                raise _planted

            try:
                launch_offline(
                    ftrace,
                    build=(boom if phase == "build" else None),
                    start=(boom if phase == "engine" else None),
                    ready=(lambda: False) if phase == "render" else (lambda: True),
                    clock=fclock, setup_timeout=60.0)
                failure = None
            except launch_mod.SetupFailure as e:
                failure = e
            ftrace.meta["setup_failure"] = failure.as_meta() if failure else None
            ftrace.finish(failure.stop_reason if failure else "error")
            fmeta = load_meta(fdir)
            check(f"{article} {phase}-phase setup failure is an infrastructure outcome",
                  failure is not None
                  and fmeta.get("stop_reason") == expected_reason
                  and fmeta["stop_reason"] not in (
                      "time_budget_exhausted", "decision_timeout",
                      "token_budget_reserved", "stuck_loop")
                  and fmeta.get("turns") == 0
                  and not os.path.isfile(os.path.join(fdir, "turns.jsonl"))
                  and not os.path.isfile(os.path.join(fdir, "replay.jsonl")),
                  str(fmeta.get("stop_reason")))
            check(f"{article} {phase}-phase failure names its phase and kind",
                  (fmeta.get("setup_failure") or {}).get("phase") == phase
                  and bool((fmeta.get("setup_failure") or {}).get("kind"))
                  and bool((fmeta.get("setup_failure") or {}).get("detail")),
                  str(fmeta.get("setup_failure")))
            check(f"{article} {phase}-phase failure leaves both boundary stamps null",
                  fmeta.get("loaded_at") is None
                  and fmeta.get("session_started_at") is None
                  and isinstance(fmeta.get("setup_started_at"), float))

        # 8e-bis. a build/setup TIMEOUT is its own retained kind, told
        # apart from a build that ran and failed. Driven through the
        # real function with a spent budget, so nothing compiles.
        spent = FakeClock(step=1.0)
        try:
            launch_mod.build_executable(
                launch_mod.REPO_ROOT,
                os.path.join(tmp, "unused_setup.log"),
                deadline=spent() - 1.0, clock=spent)
            build_timeout = None
        except launch_mod.SetupFailure as e:
            build_timeout = e
        check("a setup timeout is distinguishable from a build failure",
              build_timeout is not None
              and build_timeout.phase == "build"
              and build_timeout.kind == "build_timeout"
              and build_timeout.timed_out is True
              and build_timeout.stop_reason == "setup_build_failed",
              str(build_timeout))

        # 8e-ter. the pre-ready teardown really reaps the whole spawned
        # GROUP and really waits for the port. A stub "engine" that
        # never prints READY is spawned with a background child of its
        # own; killing the immediate process alone would leave that
        # child (and, for a real engine, its listener) behind.
        import signal as _signal
        import subprocess

        stub, child_pid_file = _tree_stub("stub_engine")

        class StubEngine(FakeEngine):
            def __init__(self, log_path):
                super().__init__()
                self.port = 0          # no listener: teardown skips the wait
                self.log_path = log_path

            def boot_mode(self):
                return ()

            def alive(self):
                return self.proc is not None and self.proc.poll() is None

        stub_eng = StubEngine(os.path.join(tmp, "stub_engine.log"))
        stub_clock = FakeClock(step=1.0)
        try:
            launch_mod.start_engine(stub_eng, stub, deadline=stub_clock(),
                                    repo_root=tmp, clock=stub_clock,
                                    sleep=lambda _s: None)
            stub_exc = None
        except launch_mod.SetupFailure as e:
            stub_exc = e
        spawned = stub_eng.proc
        child_pid = _await_pid(child_pid_file)
        launch_mod.teardown_setup(stub_eng)

        def _not_running(pid):
            """True once `pid` holds no running process.

            A ZOMBIE counts as gone — it has already exited and released
            its port and every other resource, and only waits to be
            reaped by an init that may never do so. That is exactly the
            distinction `probe_runner_lifecycle` draws, and asserting on `os.kill(pid,
            0)` instead would read a not-yet-reaped orphan as alive."""
            if pid is None:
                return False
            done = subprocess.run(["ps", "-o", "state=", "-p", str(pid)],
                                  capture_output=True, text=True)
            state = done.stdout.strip()
            return not state or state.startswith("Z")

        def _settles(predicate, budget=30.0):
            """Poll a teardown predicate — a loaded machine (a parallel
            build, say) can take seconds to finish tearing a group
            down, and this gate must not be a race."""
            end = time.monotonic() + budget
            while True:
                if predicate():
                    return True
                if time.monotonic() >= end:
                    return False
                time.sleep(0.1)

        check("a console-readiness timeout is its own retained kind",
              stub_exc is not None and stub_exc.phase == "engine"
              and stub_exc.kind == "console_timeout"
              and stub_exc.timed_out is True,
              str(stub_exc))
        reaped_leader = spawned is not None and _settles(
            lambda: spawned.poll() is not None)
        reaped_child = _settles(lambda: _not_running(child_pid))
        check("pre-ready teardown reaps the whole spawned group, not just "
              "the process it started",
              reaped_leader and reaped_child and stub_eng.proc is None,
              f"child {child_pid}: leader reaped={reaped_leader}, "
              f"child reaped={reaped_child}")
        if not reaped_child and child_pid is not None:
            try:                        # never leave the stub behind
                os.kill(child_pid, _signal.SIGKILL)
            except OSError:
                pass

        # ...and the port wait is a real observation of the listener,
        # not a sleep. A live accept loop stands in for the engine's
        # debug console: while it answers, the port must read as held.
        import threading
        holder = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
        holder.setsockopt(socket.SOL_SOCKET, socket.SO_REUSEADDR, 1)
        holder.bind(("127.0.0.1", 0))
        holder.listen(8)
        holder.settimeout(0.1)
        held_port = holder.getsockname()[1]
        listening = threading.Event()
        listening.set()

        def _accept_loop():
            while listening.is_set():
                try:
                    conn, _ = holder.accept()
                    conn.close()
                except OSError:
                    pass

        accepter = threading.Thread(target=_accept_loop, daemon=True)
        accepter.start()
        still_held = launch_mod.wait_port_released(held_port, timeout=1.0)
        listening.clear()
        accepter.join(timeout=3.0)
        holder.close()
        check("the pre-ready teardown waits on the port itself",
              still_held is False
              and launch_mod.wait_port_released(held_port, timeout=5.0) is True,
              f"held -> {still_held}")

        # 8e-penta. the READY wait's ordering, as a pure decision table
        # — in particular that a READY arriving only AFTER the setup
        # budget expired is a timeout, not an accepted boundary
        # crossing. A --setup-timeout shorter than one polling interval
        # used to accept exactly that.
        step = launch_mod.console_wait_step
        check("a READY inside the budget is accepted",
              step(True, None, False) == ("accept", None))
        late_outcome, late_failure = step(True, None, True)
        check("a READY that only arrives after the budget expired is a "
              "console timeout, not an accepted boundary",
              late_outcome == "fail" and late_failure[0] == "console_timeout"
              and "after" in late_failure[1],
              str(late_failure))
        check("an exited engine is reported as the exit, in or out of budget",
              step(False, 1, False)[1][0] == "engine_exited"
              and step(True, 1, True)[1][0] == "engine_exited")
        check("no READY and budget left means keep waiting",
              step(False, None, False) == ("wait", None))
        check("no READY and no budget left is a console timeout",
              step(False, None, True)[1][0] == "console_timeout")

        # ...and the wait never sleeps past its own deadline: a
        # --setup-timeout smaller than CONSOLE_POLL_INTERVAL must still
        # end on time rather than overshoot by a whole interval.

        tight_eng = StubEngine(os.path.join(tmp, "tight_engine.log"))
        tight_stub, tight_pids = _tree_stub("tight_engine")
        tight_budget = 0.05          # well under CONSOLE_POLL_INTERVAL
        tight_clock = SleepClock()
        tight_exc = None
        try:
            launch_mod.start_engine(
                tight_eng, tight_stub, deadline=tight_clock() + tight_budget,
                repo_root=tmp, clock=tight_clock, sleep=tight_clock.sleep)
        except launch_mod.SetupFailure as e:
            tight_exc = e
        launch_mod.teardown_setup(tight_eng)
        check("the READY wait never sleeps past the setup deadline",
              tight_exc is not None and tight_exc.kind == "console_timeout"
              and tight_budget < launch_mod.CONSOLE_POLL_INTERVAL
              and bool(tight_clock.slept)
              and all(s <= tight_budget for s in tight_clock.slept),
              f"{tight_clock.slept} -> {tight_exc}")
        tight_child = _await_pid(tight_pids, budget=2.0)
        if tight_child is not None and not _not_running(tight_child):
            try:
                os.kill(tight_child, _signal.SIGKILL)
            except OSError:
                pass

        # 8e-quater. the BUILD is a process TREE (cabal spawns ghc,
        # which spawns more), so a pre-ready failure has to reap its
        # whole group too — on the setup timeout AND on a Ctrl-C, which
        # lands inside the build's own wait and never reaches the
        # engine-side teardown. Both paths are driven here with a stub
        # that leaves a background child behind.
        for case, interrupt in (("build_timeout", False),
                                ("build_interrupt", True)):
            stub_path, stub_pids = _tree_stub(case)
            real_communicate = subprocess.Popen.communicate

            def _interrupting_communicate(self, *a, **kw):
                # Only the first call: the reap below needs the real one.
                subprocess.Popen.communicate = real_communicate
                _await_pid(stub_pids)
                raise KeyboardInterrupt()

            if interrupt:
                subprocess.Popen.communicate = _interrupting_communicate
            raised = None
            try:
                with open(os.devnull, "w") as sink:
                    launch_mod._run_setup_command(
                        [stub_path], tmp,
                        deadline=time.monotonic() + (60.0 if interrupt else 3.0),
                        clock=time.monotonic, stdout=sink)
            except BaseException as e:
                raised = e
            finally:
                subprocess.Popen.communicate = real_communicate
            build_child = _await_pid(stub_pids)
            reaped = _settles(lambda: _not_running(build_child))
            if interrupt:
                ok = isinstance(raised, KeyboardInterrupt)
            else:
                ok = (isinstance(raised, launch_mod.SetupFailure)
                      and raised.kind == "build_timeout"
                      and raised.phase == "build")
            check(f"a setup {'interruption' if interrupt else 'timeout'} "
                  "reaps the build's whole process tree",
                  ok and reaped,
                  f"raised {type(raised).__name__}, child {build_child} "
                  f"reaped={reaped}")
            if not reaped and build_child is not None:
                try:
                    os.kill(build_child, _signal.SIGKILL)
                except OSError:
                    pass

        # 8e-sexta. the SPAWN WINDOW: the interpreter checks for signals
        # between bytecodes, so a Ctrl-C landing after Popen has forked
        # but before the local names its result would leave a setup
        # child — in its OWN session, so it outlives us — that nothing
        # could reap. A real SIGINT is delivered at exactly that moment;
        # deferring it is what lets the reap still find the group.
        def _group_idle(pgid):
            """True once nothing in ``pgid`` is still running (zombies,
            already exited, do not count)."""
            done = subprocess.run(["ps", "-eo", "pgid=,state="],
                                  capture_output=True, text=True)
            for line in done.stdout.splitlines():
                parts = line.split()
                if (len(parts) >= 2 and parts[0].isdigit()
                        and int(parts[0]) == pgid
                        and not parts[1].startswith("Z")):
                    return False
            return True

        window_stub, window_pids = _tree_stub("spawn_window")
        real_popen = subprocess.Popen
        window_group: list = []

        class _PopenThenSigint(subprocess.Popen):
            """Popen that sends a real SIGINT the instant it returns.

            The signal lands after the fork but BEFORE `__init__`
            returns, so before the caller's local can name the process —
            exactly the window under test. It waits for the stub to
            establish its background child first, so the group really
            has two members to reap, and it un-patches itself before
            signalling: the reap it provokes runs `ps` through this same
            module attribute, and a second SIGINT would interrupt the
            very teardown being tested."""

            def __init__(self, *a, **kw):
                super().__init__(*a, **kw)
                window_group.append(self.pid)   # == pgid (own session)
                _await_pid(window_pids, budget=10.0)
                launch_mod.subprocess.Popen = real_popen
                os.kill(os.getpid(), _signal.SIGINT)

        launch_mod.subprocess.Popen = _PopenThenSigint
        window_exc = None
        try:
            with open(os.devnull, "w") as sink:
                launch_mod._run_setup_command(
                    [window_stub], tmp, deadline=time.monotonic() + 60.0,
                    clock=time.monotonic, stdout=sink)
        except BaseException as e:
            window_exc = e
        finally:
            launch_mod.subprocess.Popen = real_popen
        window_child = _await_pid(window_pids, budget=5.0)
        window_reaped = bool(window_group) and _settles(
            lambda: _group_idle(window_group[0]))
        check("a Ctrl-C inside the spawn window still reaps the setup group",
              isinstance(window_exc, KeyboardInterrupt)
              and window_child is not None and window_reaped
              and _not_running(window_child),
              f"raised {type(window_exc).__name__}, group {window_group}, "
              f"child {window_child}, reaped={window_reaped}")
        if window_child is not None and not _not_running(window_child):
            try:
                os.kill(window_child, _signal.SIGKILL)
            except OSError:
                pass

        # 8f. older traces, which carry only started_at/ended_at, must
        # still load and still collate in the usage ledger.
        legacy_root = os.path.join(tmp, "legacy_artifacts")
        legacy_dir = os.path.join(legacy_root, "legacy-run")
        os.makedirs(os.path.join(legacy_dir, "frames"))
        with open(os.path.join(legacy_dir, "meta.json"), "w") as f:
            json.dump({"started_at": 1_700_000_000.0,
                       "ended_at": 1_700_000_030.0,
                       "turns": 1, "stop_reason": "turn_budget_exhausted",
                       "player_token_budget": 200_000,
                       "player_model": {"backend": "codex-cli",
                                        "model": "luna", "effort": "medium"},
                       "usage_totals": {"input_tokens": 900,
                                        "output_tokens": 100}}, f)
        with open(os.path.join(legacy_dir, "turns.jsonl"), "w") as f:
            f.write(json.dumps({"turn": 1, "screenshot": "frames/turn_0001.png",
                                "player": {"observation": "", "action":
                                           {"do": "wait"}, "expectation": "",
                                           "note": ""},
                                "injected": [], "acks": [], "oracle": {},
                                "stuck": False}) + "\n")
        legacy_meta = load_meta(legacy_dir)
        legacy_ledger = os.path.join(tmp, "legacy_usage.md")
        update_usage_log(legacy_ledger, legacy_root)
        with open(legacy_ledger) as f:
            legacy_text = f.read()
        legacy_plan = write_inspection_plan(legacy_dir)
        check("a pre-#1539 trace still loads, collates and pre-analyzes",
              "loaded_at" not in legacy_meta
              and "session_started_at" not in legacy_meta
              and "legacy-run" in legacy_text and "1K" in legacy_text
              and os.path.isfile(legacy_plan))
