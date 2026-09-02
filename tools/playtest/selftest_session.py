#!/usr/bin/env python3
"""Self-test component: session, replay, trace phases and trace-dir
allocation (#2040).

Owns the lockstep loop's own behavior — a scripted session end to
end, the token-budget reserve, replay against a fresh fake engine,
stuck-loop detection, every step-phase outcome (terminal turn, held
key, crash before/during/after the step, the repause and recovery
paths, legacy replay entries, the post-repause boundary) and the
collision-resistant default trace-dir allocation.

Offline: FakeEngine + ScriptedAgent/RepeatAgent only, no window, no
build, no engine boot, no network, no model call."""
from __future__ import annotations

import inspect
import json
import os
import sys
import tempfile
import time

_HERE = os.path.dirname(os.path.abspath(__file__))
if _HERE not in sys.path:
    sys.path.insert(0, _HERE)

from engine import EngineCrash, FakeEngine  # noqa: E402
from session import _allocate_trace_dir, run_replay, run_session  # noqa: E402
from trace import (SessionTrace, load_meta, load_replay,  # noqa: E402
                   load_turns)
import agent as agent_mod  # noqa: E402

NAME = "session"


def run(check) -> None:
    """Run every session/replay/trace check into `check`."""
    with tempfile.TemporaryDirectory() as tmp:
        # 1. scripted session end-to-end
        tdir = os.path.join(tmp, "session")
        trace = SessionTrace(tdir, {"mode": "selftest", "dt": 0.0})
        eng = FakeEngine()
        reason = run_session(eng, agent_mod.ScriptedAgent(), trace,
                             turns=5, dt=0.0, max_seconds=None,
                             memory_turns=4, stuck_k=3, settle=0.0)
        trace.finish(reason)
        check("scripted session runs its turn budget",
              reason == "turn_budget_exhausted", reason)
        turns = load_turns(tdir)
        check("turns.jsonl has one record per turn", len(turns) == 5,
              str(len(turns)))
        check("frames written",
              all(os.path.isfile(os.path.join(tdir, t["screenshot"]))
                  for t in turns))
        check("oracle captured and flagged player-invisible",
              all(t["oracle"].get("player_invisible") is True for t in turns))
        check("player fields recorded",
              all(set(t["player"]) >= {"observation", "action",
                                       "expectation", "note"} for t in turns))
        meta = load_meta(tdir)
        check("meta finished with stop reason",
              meta.get("stop_reason") == reason and meta.get("turns") == 5)
        replay_entries = load_replay(tdir)
        check("replay.jsonl has one entry per turn, no-input turns included",
              len(replay_entries) == 5
              and [e["turn"] for e in replay_entries] == [1, 2, 3, 4, 5],
              f"{len(replay_entries)} entries")
        check("held key records its keyUp in the post-step phase",
              any("keyDown" in c for e in replay_entries for c in e["pre"])
              and any("keyUp" in c for e in replay_entries for c in e["post"]))
        check("every executed call has its ack retained (post included)",
              all(len(t["acks"]) == len(t["injected"]) for t in turns))
        check("normal turns record step_phase=completed in trace and replay",
              all(t.get("step_phase") == "completed" for t in turns)
              and all(e["step_phase"] == "completed" for e in replay_entries))
        hold = [t for t in turns if any("keyUp" in c for c in t["injected"])]
        check("held-input turn marks its trailing keyUp as post-step",
              bool(hold) and all(t.get("post_injected") == 1 for t in hold),
              f"{len(hold)} hold turn(s)")
        check("meta captured the oracle's world seed",
              meta.get("world_seed") == 4242, str(meta.get("world_seed")))
        from preanalysis import write_inspection_plan
        inspection_path = write_inspection_plan(tdir)
        with open(inspection_path, encoding="utf-8") as f:
            inspection = json.load(f)
        check("deterministic preanalysis queues both session bookends",
              [e["turn"] for e in inspection["inspection_turns"]] == [1, 5],
              str(inspection.get("inspection_turns")))

        class MeteredAgent(agent_mod.ScriptedAgent):
            def __init__(self):
                super().__init__([{"do": "wait"},
                                  {"do": "key", "name": "Space"}])
                self.needs_llm = True
                self.decision_timeout = 90.0

            def decide(self, screenshot_path, fb_size, memory_lines, turn,
                       timeout_seconds=None):
                result = super().decide(
                    screenshot_path, fb_size, memory_lines, turn,
                    timeout_seconds=timeout_seconds)
                result["usage"] = {"input_tokens": 90, "output_tokens": 10}
                return result

        budget_dir = os.path.join(tmp, "token_budget")
        budget_trace = SessionTrace(budget_dir, {"mode": "selftest-budget"})
        budget_reason = run_session(
            FakeEngine(), MeteredAgent(), budget_trace, turns=10, dt=0.0,
            max_seconds=None, memory_turns=2, stuck_k=99, settle=0.0,
            max_player_tokens=250)
        budget_trace.finish(budget_reason)
        budget_meta = load_meta(budget_dir)
        check("projected reserve stops before a likely token overshoot",
              budget_reason == "token_budget_reserved"
              and budget_meta.get("turns") == 2,
              f"{budget_reason}, {budget_meta.get('turns')}")
        check("trace persists input+output usage totals incrementally",
              budget_meta.get("usage_totals") == {
                  "input_tokens": 180, "output_tokens": 20,
                  "total_tokens": 200, "turns_with_usage": 2},
              str(budget_meta.get("usage_totals")))

        # 2. replay against a fresh fake engine — same inputs, same
        # order (pre before the step, post after), same turn count
        rdir = os.path.join(tmp, "replay")
        rtrace = SessionTrace(rdir, {"mode": "selftest-replay"})
        reng = FakeEngine()
        rreason = run_replay(reng, tdir, rtrace, dt=0.0, settle=0.0)
        rtrace.finish(rreason)
        check("replay completes without an agent",
              rreason == "replay_complete", rreason)
        check("replay re-injected the identical call sequence",
              reng.injected == eng.injected,
              f"{reng.injected} vs {eng.injected}")
        check("replay stepped exactly as often as the session",
              reng.unpauses == eng.unpauses,
              f"{reng.unpauses} vs {eng.unpauses}")
        check("replay covers every turn", len(load_turns(rdir)) == 5,
              str(len(load_turns(rdir))))
        rmeta = load_meta(rdir)
        check("replay pinned the recorded seed into the create-world form",
              rmeta.get("replay_forced_seed") == 4242
              and any('pending.seed = "1092"' in c for c in reng.fired),
              str(reng.fired))
        check("replay seed verification recorded",
              rmeta.get("replay_seed_match") is True)

        # 3. stuck-loop detection: constant action + constant frame
        sdir = os.path.join(tmp, "stuck")
        strace = SessionTrace(sdir, {"mode": "selftest-stuck"})
        sreason = run_session(FakeEngine(), agent_mod.RepeatAgent(), strace,
                              turns=10, dt=0.0, max_seconds=None,
                              memory_turns=4, stuck_k=3, settle=0.0)
        strace.finish(sreason)
        sturns = load_turns(sdir)
        check("stuck loop detected and recorded",
              sreason == "stuck_loop" and sturns[-1]["stuck"] is True,
              f"{sreason} after {len(sturns)} turns")
        check("stuck turn records that it never stepped",
              sturns[-1].get("step_phase") == "not_started")

        # 3b. terminal turn (#698): 'done' ends the session before the
        # sim step — its trace/replay entry must say so, and replaying
        # the trace must not invent the missing step.
        ddir = os.path.join(tmp, "done")
        dtrace = SessionTrace(ddir, {"mode": "selftest-done"})
        deng = FakeEngine()
        dreason = run_session(deng, agent_mod.ScriptedAgent(
            [{"do": "hold", "name": "W"}, {"do": "done"}]), dtrace,
            turns=5, dt=0.0, max_seconds=None, memory_turns=4,
            stuck_k=3, settle=0.0)
        dtrace.finish(dreason)
        dturns = load_turns(ddir)
        dreplay = load_replay(ddir)
        check("done turn records no step and no inputs",
              dreason == "goal_reached_claimed" and len(dturns) == 2
              and dturns[-1].get("step_phase") == "not_started"
              and dturns[-1]["injected"] == []
              and dreplay[-1]["step_phase"] == "not_started"
              and dreplay[-1]["post"] == [])
        rdeng = FakeEngine()
        rdt = SessionTrace(os.path.join(tmp, "done_replay"), {})
        rdt.finish(run_replay(rdeng, ddir, rdt, dt=0.0, settle=0.0))
        check("terminal-trace replay: same calls, same step count",
              rdeng.injected == deng.injected
              and rdeng.unpauses == deng.unpauses,
              f"steps {rdeng.unpauses} vs {deng.unpauses}")

        # 3c. stuck turn holding a key (#698): the keyDown ran, the
        # session ended before the step, so the keyUp never did — the
        # trace must not claim it, and replay must not inject it.
        hdir = os.path.join(tmp, "stuck_hold")
        htrace = SessionTrace(hdir, {"mode": "selftest-stuck-hold"})
        heng = FakeEngine()
        hreason = run_session(heng, agent_mod.ScriptedAgent(
            [{"do": "hold", "name": "W"}]), htrace,
            turns=10, dt=0.0, max_seconds=None, memory_turns=4,
            stuck_k=3, settle=0.0)
        htrace.finish(hreason)
        hturns = load_turns(hdir)
        hreplay = load_replay(hdir)
        check("stuck held-key turn records only the keyDown that ran",
              hreason == "stuck_loop"
              and hturns[-1].get("step_phase") == "not_started"
              and hturns[-1]["injected"] == ['return input.keyDown("W")']
              and len(hturns[-1]["acks"]) == 1
              and hreplay[-1]["step_phase"] == "not_started"
              and hreplay[-1]["post"] == [])
        rheng = FakeEngine()
        rht = SessionTrace(os.path.join(tmp, "stuck_replay"), {})
        rht.finish(run_replay(rheng, hdir, rht, dt=0.0, settle=0.0))
        check("stuck-trace replay invents no step and no keyUp",
              rheng.injected == heng.injected
              and rheng.unpauses == heng.unpauses,
              f"{rheng.injected} vs {heng.injected}; "
              f"steps {rheng.unpauses} vs {heng.unpauses}")

        # 3d. interruption at post-inject (#698): the step itself
        # already completed cleanly — only the post phase dies — so
        # this must record step_phase "completed", not "interrupted".
        class CrashOnPost(FakeEngine):
            def inject(self, calls):
                if any("keyUp" in c for c in calls):
                    raise EngineCrash("console died at post-inject")
                return super().inject(calls)

        cdir = os.path.join(tmp, "crash_post")
        ctrace = SessionTrace(cdir, {"mode": "selftest-crash-post"})
        ceng = CrashOnPost()
        try:
            run_session(ceng, agent_mod.ScriptedAgent(
                [{"do": "hold", "name": "W"}]), ctrace,
                turns=3, dt=0.0, max_seconds=None, memory_turns=4,
                stuck_k=99, settle=0.0)
            crashed = False
        except EngineCrash:
            crashed = True
        ctrace.finish("engine_crash")
        cturns = load_turns(cdir)
        creplay = load_replay(cdir)
        check("crash at post-inject records a completed, unposted turn",
              crashed and len(cturns) == 1
              and cturns[0].get("step_phase") == "completed"
              and cturns[0]["injected"] == ['return input.keyDown("W")']
              and len(cturns[0]["acks"]) == 1
              and creplay[0]["step_phase"] == "completed"
              and creplay[0]["post"] == [])
        rceng = FakeEngine()
        rct = SessionTrace(os.path.join(tmp, "crash_post_replay"), {})
        rct.finish(run_replay(rceng, cdir, rct, dt=0.0, settle=0.0))
        check("post-interrupted replay steps but skips the unexecuted keyUp",
              rceng.injected == ceng.injected and rceng.unpauses == 1,
              f"{rceng.injected}; steps {rceng.unpauses}")

        # 3e. crash BEFORE the step begins (#698; relabeled by #728 — the
        # prior comment here mischaracterized this as "mid-step", but
        # set_paused(False) itself is what fails, so the step never
        # advances at all). Must record step_phase "not_started".
        class CrashOnUnpause(FakeEngine):
            def set_paused(self, paused):
                if not paused:
                    raise EngineCrash("console died before the step began")
                super().set_paused(paused)

        udir = os.path.join(tmp, "crash_step")
        utrace = SessionTrace(udir, {"mode": "selftest-crash-step"})
        ueng = CrashOnUnpause()
        try:
            run_session(ueng, agent_mod.ScriptedAgent(
                [{"do": "hold", "name": "W"}]), utrace,
                turns=3, dt=0.0, max_seconds=None, memory_turns=4,
                stuck_k=99, settle=0.0)
            ucrashed = False
        except EngineCrash:
            ucrashed = True
        utrace.finish("engine_crash")
        uturns = load_turns(udir)
        ureplay = load_replay(udir)
        check("crash before the step begins records not_started, no post call",
              ucrashed and len(uturns) == 1
              and uturns[0].get("step_phase") == "not_started"
              and uturns[0].get("post_injected") == 0
              and ureplay[0]["step_phase"] == "not_started"
              and ureplay[0]["post"] == [])
        rueng = FakeEngine()
        rut = SessionTrace(os.path.join(tmp, "crash_step_replay"), {})
        rut.finish(run_replay(rueng, udir, rut, dt=0.0, settle=0.0))
        check("never-began-step replay invents no step",
              rueng.injected == ueng.injected and rueng.unpauses == 0,
              f"steps {rueng.unpauses}")

        # 3f. crash DURING the pacing interval (#728, the issue's core
        # gap): set_paused(False) returns successfully — the step has
        # genuinely begun (proven below via unpauses>0, checked before
        # the trace) — then Ctrl-C/a crash arrives before repause is
        # even attempted. This must record step_phase "interrupted",
        # never "not_started", and the best-effort recovery must leave
        # the (still-reachable) fake engine paused without masking the
        # original KeyboardInterrupt.
        pdir = os.path.join(tmp, "crash_pacing")
        ptrace = SessionTrace(pdir, {"mode": "selftest-crash-pacing"})
        peng = FakeEngine()
        real_sleep = time.sleep

        def _raise_kbi(_seconds):
            raise KeyboardInterrupt()

        time.sleep = _raise_kbi
        try:
            try:
                run_session(peng, agent_mod.ScriptedAgent([{"do": "wait"}]),
                            ptrace, turns=1, dt=0.0, max_seconds=None,
                            memory_turns=4, stuck_k=99, settle=0.0)
                pcrashed = False
            except KeyboardInterrupt:
                pcrashed = True
        finally:
            time.sleep = real_sleep
        ptrace.finish("interrupted")
        check("pacing interruption: the fake engine actually advanced "
              "before the trace was checked",
              pcrashed and peng.unpauses == 1)
        pturns = load_turns(pdir)
        preplay = load_replay(pdir)
        check("pacing interruption is recorded interrupted, not never-started, "
              "and leaves the engine paused",
              pturns[0].get("step_phase") == "interrupted"
              and preplay[0]["step_phase"] == "interrupted"
              and peng.paused is True)
        rpeng = FakeEngine()
        rpt = SessionTrace(os.path.join(tmp, "crash_pacing_replay"), {})
        rpt.finish(run_replay(rpeng, pdir, rpt, dt=0.0, settle=0.0))
        rpturns = load_turns(os.path.join(tmp, "crash_pacing_replay"))
        check("replay of an interrupted-pacing turn performs one step",
              rpeng.unpauses == 1 and rpeng.injected == []
              and rpturns[0].get("step_phase") == "completed")

        # 3g. the repause call itself fails after a successful unpause
        # (#728, "repause failure after advancement"): the step
        # advanced and even finished its pacing wait — the failure is
        # in set_paused(True) alone. Same "interrupted" contract as
        # 3f; a best-effort recovery retries the repause and this time
        # it succeeds, proving the engine still ends paused and the
        # original EngineCrash is not masked or replaced.
        class CrashOnRepauseOnce(FakeEngine):
            def __init__(self):
                super().__init__()
                self._armed = False

            def set_paused(self, paused):
                if paused and self._armed:
                    self._armed = False
                    raise EngineCrash("console died at repause")
                if not paused:
                    self._armed = True
                super().set_paused(paused)

        qdir = os.path.join(tmp, "crash_repause")
        qtrace = SessionTrace(qdir, {"mode": "selftest-crash-repause"})
        qeng = CrashOnRepauseOnce()
        try:
            run_session(qeng, agent_mod.ScriptedAgent(
                [{"do": "hold", "name": "W"}]), qtrace,
                turns=1, dt=0.0, max_seconds=None, memory_turns=4,
                stuck_k=99, settle=0.0)
            qcrashed = False
        except EngineCrash:
            qcrashed = True
        qtrace.finish("engine_crash")
        check("repause failure: the fake engine actually advanced "
              "before the trace was checked",
              qcrashed and qeng.unpauses == 1)
        qturns = load_turns(qdir)
        qreplay = load_replay(qdir)
        check("repause failure is recorded interrupted (not never-started), "
              "no post call, and the recovery retry leaves it paused",
              qturns[0].get("step_phase") == "interrupted"
              and qturns[0]["injected"] == ['return input.keyDown("W")']
              and qturns[0].get("post_injected") == 0
              and qreplay[0]["step_phase"] == "interrupted"
              and qreplay[0]["post"] == []
              and qeng.paused is True)
        rqeng = FakeEngine()
        rqt = SessionTrace(os.path.join(tmp, "crash_repause_replay"), {})
        rqt.finish(run_replay(rqeng, qdir, rqt, dt=0.0, settle=0.0))
        rqturns = load_turns(os.path.join(tmp, "crash_repause_replay"))
        check("replay of a repause-interrupted turn steps but skips the "
              "unexecuted keyUp",
              rqeng.injected == ['return input.keyDown("W")']
              and rqeng.unpauses == 1
              and rqturns[0].get("step_phase") == "completed")

        # 3h. a SECOND failure during the best-effort recovery repause
        # itself must never replace the original interruption (#728
        # review): Ctrl-C interrupts the pacing sleep, and the recovery
        # repause this triggers also raises (a different exception).
        # The recovery attempt's own failure must be swallowed — the
        # ORIGINAL KeyboardInterrupt must still be what propagates.
        class CrashOnEveryRepause(FakeEngine):
            """Repause always fails after an unpause — including the
            recovery attempt itself — but the routine per-turn "ensure
            paused" call (no preceding unpause) still succeeds."""
            def __init__(self):
                super().__init__()
                self._armed = False

            def set_paused(self, paused):
                if paused and self._armed:
                    raise EngineCrash("console died at repause (recovery too)")
                if not paused:
                    self._armed = True
                super().set_paused(paused)

        sdir2 = os.path.join(tmp, "crash_recovery_masks")
        strace2 = SessionTrace(sdir2, {"mode": "selftest-crash-recovery-masks"})
        seng2 = CrashOnEveryRepause()
        real_sleep2 = time.sleep
        time.sleep = _raise_kbi
        try:
            try:
                run_session(seng2, agent_mod.ScriptedAgent([{"do": "wait"}]),
                            strace2, turns=1, dt=0.0, max_seconds=None,
                            memory_turns=4, stuck_k=99, settle=0.0)
                s2_exc = None
            except BaseException as e:
                s2_exc = e
        finally:
            time.sleep = real_sleep2
        strace2.finish("interrupted")
        s2turns = load_turns(sdir2)
        check("a failing recovery repause never replaces the original "
              "interruption",
              isinstance(s2_exc, KeyboardInterrupt)
              and s2turns[0].get("step_phase") == "interrupted")

        # 3i. crash AFTER an acknowledged pre-input (#698 review): the
        # oracle snapshot dies before the record used to be written —
        # the acked keyDown must still land in both trace and replay,
        # with no step and a null oracle.
        class CrashOnOracle(FakeEngine):
            def oracle_context(self):
                raise EngineCrash("console died at oracle snapshot")

        odir = os.path.join(tmp, "crash_oracle")
        otrace = SessionTrace(odir, {"mode": "selftest-crash-oracle"})
        oeng = CrashOnOracle()
        try:
            run_session(oeng, agent_mod.ScriptedAgent(
                [{"do": "hold", "name": "W"}]), otrace,
                turns=3, dt=0.0, max_seconds=None, memory_turns=4,
                stuck_k=99, settle=0.0)
            ocrashed = False
        except EngineCrash:
            ocrashed = True
        otrace.finish("engine_crash")
        oturns = load_turns(odir)
        oreplay = load_replay(odir)
        check("crash at oracle keeps the acknowledged keyDown on record",
              ocrashed and len(oturns) == 1
              and oturns[0]["injected"] == ['return input.keyDown("W")']
              and len(oturns[0]["acks"]) == 1
              and oturns[0].get("step_phase") == "not_started"
              and oturns[0].get("oracle") is None
              and oreplay[0]["pre"] == ['return input.keyDown("W")']
              and oreplay[0]["step_phase"] == "not_started"
              and oreplay[0]["post"] == [])
        roeng = FakeEngine()
        rot = SessionTrace(os.path.join(tmp, "crash_oracle_replay"), {})
        rot.finish(run_replay(roeng, odir, rot, dt=0.0, settle=0.0))
        check("oracle-interrupted replay re-injects the keyDown, no step",
              roeng.injected == oeng.injected and roeng.unpauses == 0,
              f"{roeng.injected}; steps {roeng.unpauses}")

        # replay has the same pre-to-post exposure: a crash during the
        # REPLAY's oracle snapshot must keep its acked pre calls too
        r2eng = CrashOnOracle()
        r2dir = os.path.join(tmp, "replay_crash_oracle")
        r2t = SessionTrace(r2dir, {"mode": "selftest-replay-crash"})
        try:
            run_replay(r2eng, hdir, r2t, dt=0.0, settle=0.0)
            r2crashed = False
        except EngineCrash:
            r2crashed = True
        r2t.finish("engine_crash")
        r2turns = load_turns(r2dir)
        check("replay records its acked pre calls when its oracle crashes",
              r2crashed and len(r2turns) == 1
              and r2turns[0]["injected"] == ['return input.keyDown("W")']
              and r2turns[0].get("step_phase") == "not_started")

        # 3i-bis (#1750). An engine whose input injection CHANGES modal
        # and widget state: before the click the HUD button sits under
        # an exclusive modal (out of pointer scope); the click's own
        # callback closes that modal, so by the time the post-inject
        # oracle samples, the very same button reads as in scope and
        # plainly clickable. The trace must record the PRE-injection
        # state as this click's routing context, otherwise the offline
        # critic correlates the click to a control the router could not
        # have reached at the moment it was routed.
        class ModalClosingEngine(FakeEngine):
            _HUD = {"id": "button:hud", "control": True, "visible": True,
                    "bounds": {"x": 0, "y": 0, "w": 100, "h": 100},
                    "pointerBlocking": True, "leftClickTarget": True,
                    "leftClickAffordance": True,
                    "paintKey": 100, "paintOrder": 0}

            def __init__(self):
                super().__init__()
                self.modal_open = True
                self.routing_reads: list[bool] = []

            def _hud(self, in_scope):
                return dict(self._HUD, inScope=in_scope)

            def inject(self, calls):
                # the click's Lua callback closes the modal
                if any("click" in c for c in calls):
                    self.modal_open = False
                return super().inject(calls)

            def oracle_routing(self):
                self.routing_reads.append(self.modal_open)
                return {"widgets": [self._hud(not self.modal_open)]}

            def oracle_context(self):
                snap = super().oracle_context()
                snap["widgets"] = [self._hud(not self.modal_open)]
                return snap

        cdir = os.path.join(tmp, "routing_capture")
        ctrace = SessionTrace(cdir, {"mode": "selftest-routing"})
        ceng = ModalClosingEngine()
        creason = run_session(
            ceng, agent_mod.ScriptedAgent([{"do": "click", "x": 10, "y": 10}]),
            ctrace, turns=1, dt=0.0, max_seconds=None, memory_turns=4,
            stuck_k=99, settle=0.0)
        ctrace.finish(creason)
        cturns = load_turns(cdir)
        c_oracle = cturns[0]["oracle"]
        check("the routing oracle is sampled before the turn's first "
              "injected call (#1750)",
              ceng.routing_reads == [True]
              and ceng.injected and "click" in ceng.injected[0],
              f"modal_open at each routing read: {ceng.routing_reads}")
        check("the recorded routing context is the PRE-injection state (#1750)",
              c_oracle["routing_widgets"][0]["inScope"] is False,
              str(c_oracle.get("routing_widgets")))
        check("...while #775's post-inject `widgets` keeps its own, changed "
              "sampling point (#1750)",
              c_oracle["widgets"][0]["inScope"] is True,
              str(c_oracle.get("widgets")))
        from critic_signals import build_signals as _build_signals
        c_signals = _build_signals(cdir, cturns)
        check("the critic correlates the click against the pre-injection "
              "context, not the post-callback one (#1750)",
              c_signals[0]["clicked_widget"] is None,
              str(c_signals[0]["clicked_widget"]))

        # Replay takes the identical pre-injection capture, so a
        # replayed trace carries the same correlation context.
        rcdir = os.path.join(tmp, "routing_capture_replay")
        rctrace = SessionTrace(rcdir, {"mode": "selftest-routing-replay"})
        rceng = ModalClosingEngine()
        rctrace.finish(run_replay(rceng, cdir, rctrace, dt=0.0, settle=0.0))
        rcturns = load_turns(rcdir)
        check("replay samples its routing oracle before injecting too (#1750)",
              rceng.routing_reads == [True]
              and rcturns[0]["oracle"]["routing_widgets"][0]["inScope"] is False
              and rcturns[0]["oracle"]["widgets"][0]["inScope"] is True,
              f"{rceng.routing_reads}; "
              f"{rcturns[0]['oracle'].get('routing_widgets')}")

        # 3j. crash mid multi-call action: the acknowledged prefix of a
        # drag survives in trace + replay; the unacked remainder is
        # never claimed.
        class CrashOnCall(FakeEngine):
            def inject(self, calls):
                if any("mouseUp" in c for c in calls):
                    raise EngineCrash("console died mid-drag")
                return super().inject(calls)

        mdir = os.path.join(tmp, "crash_mid_action")
        mtrace = SessionTrace(mdir, {"mode": "selftest-crash-mid"})
        meng = CrashOnCall()
        try:
            run_session(meng, agent_mod.ScriptedAgent(
                [{"do": "drag", "x1": 1, "y1": 2, "x2": 3, "y2": 4}]),
                mtrace, turns=3, dt=0.0, max_seconds=None, memory_turns=4,
                stuck_k=99, settle=0.0)
            mcrashed = False
        except EngineCrash:
            mcrashed = True
        mtrace.finish("engine_crash")
        mturns = load_turns(mdir)
        mreplay = load_replay(mdir)
        check("mid-action crash keeps the acknowledged call prefix",
              mcrashed and len(mturns) == 1
              and len(mturns[0]["injected"]) == 3
              and all("mouseUp" not in c for c in mturns[0]["injected"])
              and len(mturns[0]["acks"]) == 3
              and mturns[0].get("step_phase") == "not_started"
              and mreplay[0]["pre"] == mturns[0]["injected"]
              and mreplay[0]["post"] == []
              and mreplay[0]["step_phase"] == "not_started",
              f"{len(mturns[0]['injected']) if mturns else 0} call(s) kept")
        rmeng = FakeEngine()
        rmt = SessionTrace(os.path.join(tmp, "crash_mid_replay"), {})
        rmt.finish(run_replay(rmeng, mdir, rmt, dt=0.0, settle=0.0))
        check("mid-action replay re-injects exactly the acked prefix",
              rmeng.injected == meng.injected and rmeng.unpauses == 0,
              f"{rmeng.injected}")

        # 3k. legacy replay-entry compatibility (#728): pre-#698 entries
        # carry no "stepped" field at all (those traces only ever
        # recorded a step on every turn); #718-era entries carry a
        # boolean "stepped". Both must keep loading with their
        # historical mapping — missing/True as a completed step, False
        # as never-started (its true start state, never-began vs.
        # began-but-interrupted, is unrecoverable, so it conservatively
        # keeps the old no-step replay behavior) — and a real
        # "step_phase" entry must pass through unchanged.
        ldir = os.path.join(tmp, "legacy_missing")
        os.makedirs(ldir)
        with open(os.path.join(ldir, "replay.jsonl"), "w") as f:
            f.write(json.dumps({"turn": 1, "pre": [], "post": []}) + "\n")
        check("legacy replay entry with no stepped field maps to completed",
              load_replay(ldir)[0]["step_phase"] == "completed")

        ltdir = os.path.join(tmp, "legacy_true")
        os.makedirs(ltdir)
        with open(os.path.join(ltdir, "replay.jsonl"), "w") as f:
            f.write(json.dumps({"turn": 1, "pre": [], "post": [],
                                "stepped": True}) + "\n")
        check("legacy boolean stepped=True maps to completed",
              load_replay(ltdir)[0]["step_phase"] == "completed")

        lfdir = os.path.join(tmp, "legacy_false")
        os.makedirs(lfdir)
        with open(os.path.join(lfdir, "replay.jsonl"), "w") as f:
            f.write(json.dumps({"turn": 1, "pre": [], "post": [],
                                "stepped": False}) + "\n")
        check("legacy boolean stepped=False maps to not_started",
              load_replay(lfdir)[0]["step_phase"] == "not_started")

        ndir = os.path.join(tmp, "new_format")
        os.makedirs(ndir)
        with open(os.path.join(ndir, "replay.jsonl"), "w") as f:
            f.write(json.dumps({"turn": 1, "pre": [], "post": [],
                                "step_phase": "interrupted"}) + "\n")
        check("new-format step_phase entry passes through unchanged",
              load_replay(ndir)[0]["step_phase"] == "interrupted")

        # 3l. the post-repause boundary (#728 review): a step that
        # genuinely fully completed (unpause, sleep, and repause all
        # returned) must not be misrecorded as never-started just
        # because the caller happened to get interrupted on its way
        # back out. Unlike the post-unpause boundary (an accepted,
        # documented single-bytecode rough edge — see _run_step's
        # docstring), this one is now structurally closed: phase[0] is
        # written to "completed" by _run_step itself, from inside its
        # own protected try, before the caller ever regains control —
        # there is no separate caller-side "step_phase = completed"
        # statement left for an interruption to land after. A
        # line-level trace hook proves it by firing the instant control
        # returns to the caller right after _run_step(...), in both
        # run_session and run_replay.
        def _raise_after_return(func, snippet):
            src, start = inspect.getsourcelines(func)
            idx = next(i for i, line in enumerate(src) if snippet in line)
            target = start + idx + 1
            filename = func.__code__.co_filename

            def tracer(frame, event, _arg):
                if (event == "line" and frame.f_code.co_filename == filename
                        and frame.f_lineno == target):
                    raise KeyboardInterrupt()
                return tracer
            return tracer

        def _run_under_post_repause_interrupt(func, fn):
            old_trace = sys.gettrace()
            sys.settrace(_raise_after_return(func, "_run_step(eng, dt, phase)"))
            try:
                fn()
                return None
            except BaseException as e:
                return e
            finally:
                sys.settrace(old_trace)

        bdir = os.path.join(tmp, "post_repause_boundary")
        btrace = SessionTrace(bdir, {"mode": "selftest-post-repause-boundary"})
        beng = FakeEngine()
        b_exc = _run_under_post_repause_interrupt(run_session, lambda: run_session(
            beng, agent_mod.ScriptedAgent([{"do": "wait"}]), btrace,
            turns=1, dt=0.0, max_seconds=None, memory_turns=4,
            stuck_k=99, settle=0.0))
        btrace.finish("interrupted")
        bturns = load_turns(bdir)
        check("session: a fully completed step interrupted on the way out "
              "is still recorded completed, not never-started",
              isinstance(b_exc, KeyboardInterrupt)
              and bool(bturns) and bturns[0].get("step_phase") == "completed"
              and beng.paused is True)

        rbdir = os.path.join(tmp, "post_repause_boundary_replay")
        rbtrace = SessionTrace(rbdir, {"mode": "selftest-post-repause-boundary-replay"})
        rbeng = FakeEngine()
        rb_exc = _run_under_post_repause_interrupt(run_replay, lambda: run_replay(
            rbeng, tdir, rbtrace, dt=0.0, settle=0.0))
        rbtrace.finish("interrupted")
        rbturns = load_turns(rbdir)
        check("replay: a fully completed step interrupted on the way out "
              "is still recorded completed, not never-started",
              isinstance(rb_exc, KeyboardInterrupt)
              and bool(rbturns) and rbturns[0].get("step_phase") == "completed"
              and rbeng.paused is True)

        # 5. default trace-dir allocation is collision-resistant: two
        # same-second, same-persona allocations get distinct dirs, and
        # both exist afterward (mkdir is the reservation).
        base = os.path.join(tmp, "sessions", "20260709_120000_carl")
        d1 = _allocate_trace_dir(base)
        d2 = _allocate_trace_dir(base)
        d3 = _allocate_trace_dir(base)
        check("same-name trace dirs allocate distinctly",
              len({d1, d2, d3}) == 3, f"{d1}, {d2}, {d3}")
        check("allocated trace dirs all exist",
              all(os.path.isdir(d) for d in (d1, d2, d3)))
        check("first allocation keeps the clean timestamped name",
              d1 == base and d2 == base + "_2" and d3 == base + "_3")

