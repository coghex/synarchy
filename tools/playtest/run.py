#!/usr/bin/env python3
"""Naive-player UX playtest harness — lockstep runner (H1, #647/#641).

⚠️  BY DEFAULT LAUNCHES A WINDOWED GAME INSTANCE THAT TAKES OVER YOUR
SCREEN AND STEALS FOCUS (F1 screenshots + F2 input need a real render
pipeline — GPU-less --headless cannot host a playtest). Run it while
away from the machine, on a second display/machine — or pass
`--render-mode offscreen` (#650): the same full render + input
pipeline into offscreen images, no window, no focus steal, safe to run
unattended and in parallel on distinct ports. The windowed default is
the one sanctioned exception to the never-launch-graphical rule: the
graphical instance IS the system under test.

The lockstep loop per turn: pause -> screenshot (F1) -> player agent
decides from pixels alone -> inject its action (F2) -> record the
oracle snapshot (F3 widgets, event-log delta — stored for the critic,
never shown to the player) -> unpause for a fixed wall-clock dt ->
re-pause. Everything lands in a session-trace directory H2 consumes
(see trace.py). Wall-clock dt is a deliberate simplicity tradeoff:
--replay reproduces the input sequence, not bit-identical turns.

Usage:
  python3 tools/playtest/run.py                       # LLM player, defaults
  python3 tools/playtest/run.py --persona impatient_imogen --turns 60
  python3 tools/playtest/run.py --render-mode offscreen  # no window (#650)
  python3 tools/playtest/run.py --smoke               # 3 scripted turns, no LLM
  python3 tools/playtest/run.py --replay <trace_dir>  # re-inject a session
  python3 tools/playtest/run.py --selftest            # offline loop/trace check

The player model needs ANTHROPIC_API_KEY (or an `ant auth login`
profile); scripted/smoke/replay/selftest runs don't.
"""
from __future__ import annotations

import argparse
import hashlib
import json
import os
import sys
import time

HERE = os.path.dirname(os.path.abspath(__file__))
sys.path.insert(0, HERE)
sys.path.insert(0, os.path.dirname(HERE))

from engine import EngineCrash, FakeEngine, PlaytestEngine, translate_action  # noqa: E402
from personas import load_persona  # noqa: E402
from trace import SessionTrace, load_meta, load_replay, load_turns  # noqa: E402
import agent as agent_mod  # noqa: E402

DEFAULT_PORT = 9308
DEFAULT_MANUAL = os.path.join(os.path.dirname(os.path.dirname(HERE)),
                              "docs", "player_manual.md")
MANUAL_STUB = ("(No manual was provided. You know nothing about this game "
               "beyond what you can see on screen.)")


def _read_manual(path: str) -> str:
    try:
        with open(path, encoding="utf-8") as f:
            return f.read()
    except OSError:
        print(f"  [warn] manual not found at {path}; running with a stub")
        return MANUAL_STUB


def _file_hash(path: str) -> str:
    h = hashlib.sha256()
    with open(path, "rb") as f:
        h.update(f.read())
    return h.hexdigest()


def _action_sig(action: dict) -> str:
    return json.dumps(action, sort_keys=True)


def _allocate_trace_dir(base: str) -> str:
    """Atomically reserve a fresh default session directory. Parallel
    sessions — the offscreen mode's whole point (#650) — can start the
    same second with the same persona, so the timestamped name is only
    a preference: os.mkdir is the atomic reservation, and a taken name
    gets a _2/_3/... suffix instead of two sessions silently sharing
    (and corrupting) one trace directory."""
    parent = os.path.dirname(base)
    if parent:
        os.makedirs(parent, exist_ok=True)
    candidate = base
    for n in range(2, 1000):
        try:
            os.mkdir(candidate)
            return candidate
        except FileExistsError:
            candidate = f"{base}_{n}"
    raise RuntimeError(f"could not allocate a session trace dir near {base!r}")


def _promote_seed(trace: SessionTrace, oracle: dict) -> None:
    """First non-null world seed the oracle sees becomes session
    metadata — the world the player actually ended up in, even when
    the create-world screen randomized it."""
    if trace.meta.get("world_seed") is None and oracle.get("world_seed") is not None:
        trace.meta["world_seed"] = oracle["world_seed"]


def _count_f4_outcomes(trace: SessionTrace, oracle: dict) -> None:
    """Running total of F4 (#646) action-outcome records seen this
    session — a quick session-level summary; the per-turn records
    themselves live in each turn's oracle.action_outcomes."""
    trace.meta["f4_outcomes_total"] = (
        trace.meta.get("f4_outcomes_total", 0) + len(oracle.get("action_outcomes") or []))


def _run_step(eng: PlaytestEngine, dt: float, phase: list) -> None:
    """The whole step contract (#728): unpause, sleep dt, repause.
    Shared by run_session and run_replay so both follow the identical
    phase-accounting contract. `phase` is a one-element list the
    caller passes in as `["not_started"]`; this function is the ONLY
    writer of phase[0], always from within its own protected try, so
    there is no separate caller-side bookkeeping statement for an
    interruption landing after eng.set_paused(...) returns (either
    call, successful or not) to slip through — the caller reads
    phase[0] directly in its own `finally`, never assigns to a local
    "step_phase" of its own from a value handed back after this
    function returns (a #728-review fix: a caller-side assignment
    after a successful call — either "the step began" after unpause or
    "the step completed" after repause — used to be its own statement,
    and an interruption landing between the call returning and that
    statement running was misrecorded as "not_started", the former
    skipping recovery, the latter making replay skip a step that had
    genuinely fully completed).

    phase[0] becomes "interrupted" the instant eng.set_paused(False)
    returns — the very next bytecode, the tightest window achievable
    with an ordinary statement. A sub-bytecode async signal landing in
    that single instruction (before recording "interrupted" but after
    the unpause call has genuinely returned) is not something any pure-
    Python restructuring can close (moving the assignment, or
    classifying by which of two sibling try/except blocks catches the
    exception instead of using a flag, both just relocate the same
    irreducible gap — verified by hand while fixing this). Real Ctrl-C
    timing has no realistic chance of landing there; a human developer
    running this offline test harness is an accepted, rough-edges
    audience (unlike the fire-and-forget engine-acknowledgement gap
    this same issue already accepts as unrecoverable, see engine.py).
    Closing it for real would need OS-level signal masking
    (signal.pthread_sigmask around the critical section) — POSIX-only
    machinery not used anywhere else in this codebase, disproportionate
    for this single-bytecode gap.

    Once the step has begun, any interruption (BaseException, so
    Ctrl-C is covered) makes a best-effort recovery repause —
    swallowing its own failure — so the engine's pause state is left
    defined for outer cleanup instead of running unattended until
    shutdown, then re-raises the ORIGINAL interruption unchanged
    (never masked by a recovery failure)."""
    try:
        eng.set_paused(False)
        phase[0] = "interrupted"
        time.sleep(dt)
        eng.set_paused(True)
        phase[0] = "completed"
    except BaseException:
        if phase[0] != "not_started":
            try:
                eng.set_paused(True)
            except BaseException:
                pass
        raise


def run_session(eng: PlaytestEngine, player, trace: SessionTrace, *,
                turns: int, dt: float, max_seconds: float | None,
                memory_turns: int, stuck_k: int, settle: float = 0.3) -> str:
    """The lockstep loop. Returns the stop reason."""
    memory: list[str] = []
    prev_sig = None
    prev_frame_hash = None
    stuck_count = 0
    started = time.monotonic()
    stop_reason = "turn_budget_exhausted"

    for turn in range(1, turns + 1):
        if max_seconds is not None and time.monotonic() - started > max_seconds:
            stop_reason = "time_budget_exhausted"
            break

        # 1. ensure paused, 2. capture the frame
        eng.set_paused(True)
        frame = trace.frame_path(turn)
        fb_size = eng.screenshot(frame)
        frame_hash = _file_hash(frame)
        ts = time.time()

        # 3. the player decides — screenshot + its own memory ONLY
        decision = player.decide(frame, fb_size, memory, turn)
        action = decision["action"]

        # 4. translate + inject
        try:
            calls, post_calls = translate_action(action, fb_size)
        except Exception as e:  # unusable action: record it, inject nothing
            decision["note"] = (decision.get("note", "")
                                + f" [harness: {e}]").strip()
            calls, post_calls = [], []

        # Everything from the first injected call to the last post-step
        # call runs under one record-in-finally (#698): however the
        # turn ends — normally, done/stuck, or a crash/Ctrl-C anywhere
        # in between — the record holds exactly the acknowledged calls
        # (a multi-call action that dies mid-way keeps its successful
        # prefix), every ack received, and a step_phase distinguishing
        # a step that never began from one that began but was
        # interrupted from one that fully completed (#728). Replay
        # keys off exactly these fields. oracle stays None when the
        # snapshot never completed (the critic treats a null oracle as
        # absent).
        sent: list[str] = []       # acknowledged prefix of calls
        post_sent: list[str] = []  # acknowledged prefix of post_calls
        acks: list = []
        post_acks: list = []
        oracle = None
        stuck = False
        # phase[0] is read directly in `finally` below, never copied
        # into a separate local first — see _run_step's docstring for
        # why that matters (#728 review).
        phase = ["not_started"]
        try:
            # inject one call at a time so a mid-action crash still
            # records the acknowledged prefix
            for call in calls:
                acks.extend(eng.inject([call]))
                sent.append(call)
            if calls:
                time.sleep(settle)  # let the Lua-side click/key consequences land

            # 5. oracle snapshot — recorded, never shown to the player
            oracle = eng.oracle_snapshot()
            _promote_seed(trace, oracle)
            _count_f4_outcomes(trace, oracle)

            # stuck-loop detection: same action, same pixels, K times
            # in a row. A repeat-with-no-change loop is itself a strong
            # missing-feedback signal — record it, then stop.
            sig = _action_sig(action)
            if sig == prev_sig and frame_hash == prev_frame_hash:
                stuck_count += 1
            else:
                stuck_count = 0
            prev_sig, prev_frame_hash = sig, frame_hash
            stuck = stuck_count >= stuck_k - 1 and turn >= stuck_k

            note = decision.get("note") or ""
            print(f"  turn {turn:3d}: {action.get('do'):6s} "
                  f"{'' if not note else '— ' + note[:80]}")

            # A done/stuck turn ends the session without a sim step, so
            # its post-step calls (a held key's keyUp) never run either.
            terminal = action.get("do") == "done" or stuck

            # 6. step the sim by wall-clock dt, then the post-step
            # calls (a held key releases after riding through the
            # step). _run_step writes phase[0] itself throughout — the
            # code here never assigns its own "step_phase" from a
            # value handed back after a call returns (#728 review).
            if not terminal:
                _run_step(eng, dt, phase)
                for call in post_calls:
                    post_acks.extend(eng.inject([call]))
                    post_sent.append(call)
        finally:
            trace.record_turn({
                "turn": turn,
                "ts": ts,
                "screenshot": os.path.relpath(frame, trace.dir),
                "fb_size": list(fb_size),
                "player": {k: decision.get(k) for k in
                           ("observation", "action", "expectation", "note",
                            "raw", "usage")},
                "injected": sent + post_sent,
                "acks": acks + post_acks,
                "post_injected": len(post_sent),
                "step_phase": phase[0],
                "oracle": oracle,
                "stuck": stuck,
            })
            trace.record_replay(turn, sent, post_sent, step_phase=phase[0])

        if action.get("do") == "done":
            stop_reason = "goal_reached_claimed"
            break
        if stuck:
            print(f"  [stuck] same action with no visible change x{stuck_k} — stopping")
            stop_reason = "stuck_loop"
            break

        # rolling memory for the next turn (the player's own notes)
        memory.append(
            f"turn {turn}: saw: {decision.get('observation', '')[:160]} | "
            f"did: {json.dumps(action)} | expected: {decision.get('expectation', '')[:120]}"
            + (f" | note: {note[:120]}" if note else ""))
        memory[:] = memory[-memory_turns:]

    return stop_reason


def run_replay(eng: PlaytestEngine, source_dir: str, trace: SessionTrace,
               dt: float, settle: float = 0.3) -> str:
    """Re-inject a recorded session's inputs — no LLM. Faithful to the
    session's structure: every recorded turn is replayed (including
    no-input ones, so pacing matches), pre-step calls land before the
    dt step and post-step calls (a held key's keyUp) after it, exactly
    as they did live. A turn recorded step_phase "not_started"
    (done/stuck, or interrupted before a successful unpause, #698) is
    replayed without a step or post calls. A turn recorded
    "interrupted" (unpause succeeded live but the step didn't finish
    cleanly, #728) still replays one full unpause->dt->repause step —
    the truncated original duration isn't reproduced, an existing
    wall-clock-dt tradeoff — but its post calls never ran live either,
    so replay doesn't invent them; only a "completed" turn replays its
    post calls too. Wall-clock dt means it is still NOT guaranteed
    bit-identical (accepted tradeoff — see README)."""
    entries = load_replay(source_dir)
    if not entries:
        print(f"  [warn] {source_dir} has no replay.jsonl entries")
        return "replay_empty"
    src_meta = load_meta(source_dir)
    # Seed pinning: the create-world screen randomizes its seed box on a
    # fresh instance, so pure input reinjection would build a different
    # world. Until the replayed world exists, force the recorded seed
    # into the form state the Generate button reads
    # (createWorldMenu.pending.seed, hex — the seed box's onChange
    # writes the same field, so a session that TYPED its seed replays
    # identically too). This is how --replay reproduces "same seed" for
    # randomized sessions.
    force_seed = src_meta.get("world_seed")
    if force_seed is not None:
        trace.meta["replay_forced_seed"] = force_seed
    for entry in entries:
        turn = entry["turn"]
        eng.set_paused(True)
        if force_seed is not None and trace.meta.get("world_seed") is None:
            eng.lua_fire(
                'pcall(function() require("scripts.create_world_menu")'
                f'.pending.seed = "{force_seed:x}" end)')
        frame = trace.frame_path(turn)
        fb_size = eng.screenshot(frame)
        ts = time.time()
        # Step only when the ORIGINAL turn actually began one (#728):
        # step_phase "not_started" (done/stuck, or interrupted before a
        # successful unpause) skips the step entirely; "interrupted"
        # and "completed" both replay one full unpause->dt->repause
        # step, but post-step calls only ever ran live on a
        # "completed" turn, so only that state replays them. The whole
        # pre-to-post span runs under the same record-in-finally shape
        # as run_session, so an interrupted replay leaves a truthful
        # trace of its own — acknowledged calls only, successful
        # prefixes of multi-call turns preserved, and its own
        # step_phase reflecting THIS execution's outcome, not the
        # source's.
        sent: list[str] = []
        post_sent: list[str] = []
        acks: list = []
        post_acks: list = []
        oracle = None
        # phase[0] is read directly in `finally` below, never copied
        # into a separate local first — see _run_step's docstring for
        # why that matters (#728 review).
        phase = ["not_started"]
        try:
            for call in entry["pre"]:
                acks.extend(eng.inject([call]))
                sent.append(call)
            if entry["pre"]:
                time.sleep(settle)
            oracle = eng.oracle_snapshot()
            _promote_seed(trace, oracle)
            _count_f4_outcomes(trace, oracle)
            if entry["step_phase"] != "not_started":
                _run_step(eng, dt, phase)
                if entry["step_phase"] == "completed":
                    for call in entry["post"]:
                        post_acks.extend(eng.inject([call]))
                        post_sent.append(call)
        finally:
            trace.record_turn({
                "turn": turn, "ts": ts,
                "screenshot": os.path.relpath(frame, trace.dir),
                "fb_size": list(fb_size),
                "player": None,   # replay has no player — inputs come from the trace
                "injected": sent + post_sent,
                "acks": acks + post_acks,
                "post_injected": len(post_sent),
                "step_phase": phase[0],
                "oracle": oracle, "stuck": False,
            })
        print(f"  replay turn {turn:3d}: {len(sent)}+{len(post_sent)} call(s)"
              + {"not_started": " — no step (terminal turn)",
                 "interrupted": " — step replayed, no post calls "
                                "(source was interrupted)",
                 "completed": ""}[entry["step_phase"]])
    # Seed verification backstop: pinning should make these match; a
    # mismatch means the replayed run diverged (e.g. different menu
    # path) and world-dependent turns can't be trusted.
    got_seed = trace.meta.get("world_seed")
    trace.meta["replay_seed_match"] = (
        None if force_seed is None and got_seed is None
        else force_seed == got_seed)
    if trace.meta["replay_seed_match"] is False:
        print(f"  [warn] replayed world seed {got_seed} != session's "
              f"{force_seed} despite pinning — the replay diverged before "
              "world creation; world-dependent turns will not match")
    return "replay_complete"


def selftest() -> int:
    """Offline check of the loop, trace write, stuck detection, and
    replay — FakeEngine, ScriptedAgent, no window, no API, no build."""
    import inspect
    import tempfile
    failures = []

    def check(name, ok, detail=""):
        print(f"  [{'ok' if ok else 'FAIL'}] {name}"
              + (f" — {detail}" if detail else ""))
        if not ok:
            failures.append(name)

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
            def oracle_snapshot(self):
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

        # 4. render-mode threading (#650): the launcher maps each mode
        # to the right boot flags, rejects unknown modes, and the fake
        # engine (which never boots) stays mode-agnostic.
        from engine import PlaytestEngine as RealEngine
        check("windowed render mode boots with no mode flags",
              RealEngine(0, os.devnull).boot_mode() == ())
        check("offscreen render mode boots with --offscreen",
              RealEngine(0, os.devnull,
                         render_mode="offscreen").boot_mode() == ("--offscreen",))
        try:
            RealEngine(0, os.devnull, render_mode="fullscreen")
            check("unknown render mode rejected", False)
        except ValueError:
            check("unknown render mode rejected", True)

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

        # 6. persona + prompt assembly stays oracle-blind by shape:
        # build_system_prompt takes persona/manual/fb only
        params = list(inspect.signature(agent_mod.build_system_prompt).parameters)
        check("prompt assembly accepts no oracle inputs",
              params == ["persona", "manual", "fb_size"], str(params))
        p = load_persona("curious_carl")
        prompt = agent_mod.build_system_prompt(p, "MANUAL", (1280, 720))
        check("prompt contains persona + manual + size",
              "curious_carl" in prompt and "MANUAL" in prompt
              and "1280x720" in prompt)

        # 7. event-log snapshots (#699): the engine-side store appends,
        # moves coalesced updates to the tail, and drops from the head at
        # capacity. Exercise those transitions through the real oracle state
        # tracker rather than only testing the pure diff helper.
        class EventLogEngine(RealEngine):
            def __init__(self, logs):
                super().__init__(0, os.devnull)
                self.logs = iter(logs)

            def lua(self, code, timeout=0):
                if code == "return engine.getEventLog()":
                    return next(self.logs)
                if code == "return world.getSeed()":
                    return None
                if code == "return debug.drainActionOutcomes()":
                    return []
                if code == "return engine.isPaused()":
                    return False
                if "dumpWidgets" in code:
                    return []
                if "currentMenu" in code:
                    return "main"
                raise AssertionError(f"unexpected oracle call: {code}")

        repeat = {"text": "repeat", "gameTime": 1, "count": 1}
        stable = {"text": "stable", "gameTime": 2, "count": 1}
        appended = {"text": "appended", "gameTime": 3, "count": 1}
        coalesced = {"text": "repeat", "gameTime": 4, "count": 2}
        rollover = {"text": "rollover", "gameTime": 5, "count": 1}
        logs = [
            [repeat, stable],
            [repeat, stable, appended],
            [repeat, stable, appended],
            [stable, appended, coalesced],
            [appended, coalesced, rollover],
            [coalesced, rollover, appended],
        ]
        event_eng = EventLogEngine(logs)
        deltas = [event_eng.oracle_snapshot()["event_log_new"]
                  for _ in logs]
        check("event-log first snapshot explicitly reports its full baseline",
              deltas[0] == [repeat, stable], str(deltas[0]))
        check("event-log append reports only the appended row",
              deltas[1] == [appended], str(deltas[1]))
        check("event-log unchanged snapshot reports no duplicate rows",
              deltas[2] == [], str(deltas[2]))
        check("event-log coalesce reports only the updated row",
              deltas[3] == [coalesced], str(deltas[3]))
        check("event-log rollover reports the new tail row",
              deltas[4] == [rollover], str(deltas[4]))
        check("event-log rollover detects a new row matching an evicted row",
              deltas[5] == [appended], str(deltas[5]))

    if failures:
        print(f"selftest: FAILED ({len(failures)}): {', '.join(failures)}")
        return 1
    print("selftest: all checks passed")
    return 0


def main() -> int:
    ap = argparse.ArgumentParser(
        description=__doc__, formatter_class=argparse.RawDescriptionHelpFormatter)
    ap.add_argument("--port", type=int, default=DEFAULT_PORT)
    ap.add_argument("--render-mode", choices=["windowed", "offscreen"],
                    default="windowed",
                    help="windowed = real window (steals focus, the default "
                         "system-under-test substrate); offscreen = #650's "
                         "windowless render for unattended/parallel runs")
    ap.add_argument("--persona", default="curious_carl",
                    help="bundled persona name or a path to a persona file")
    ap.add_argument("--goal", default=None, help="override the persona's goal")
    ap.add_argument("--manual", default=DEFAULT_MANUAL,
                    help="player manual path (C1; stubbed if missing)")
    ap.add_argument("--model", default=agent_mod.DEFAULT_MODEL)
    ap.add_argument("--effort", default=agent_mod.DEFAULT_EFFORT,
                    choices=["low", "medium", "high"])
    ap.add_argument("--max-tokens", type=int, default=agent_mod.DEFAULT_MAX_TOKENS)
    ap.add_argument("--turns", type=int, default=40)
    ap.add_argument("--max-seconds", type=float, default=None,
                    help="wall-clock session budget")
    ap.add_argument("--dt", type=float, default=2.0,
                    help="unpaused wall-clock seconds per turn")
    ap.add_argument("--memory-turns", type=int, default=8)
    ap.add_argument("--stuck-k", type=int, default=3,
                    help="identical action+frame repeats that count as stuck")
    ap.add_argument("--trace-dir", default=None,
                    help="session output dir (default tools/playtest/sessions/<ts>_<label>)")
    ap.add_argument("--agent", choices=["llm", "scripted"], default="llm")
    ap.add_argument("--smoke", action="store_true",
                    help="tiny scripted session (3 turns, no LLM)")
    ap.add_argument("--replay", metavar="TRACE_DIR", default=None,
                    help="re-inject a recorded session (no LLM)")
    ap.add_argument("--selftest", action="store_true",
                    help="offline harness check (no window, no engine, no API)")
    args = ap.parse_args()

    if args.selftest:
        return selftest()

    if args.smoke:
        args.agent = "scripted"
        args.turns = min(args.turns, 3)

    # Assemble the run
    replaying = args.replay is not None
    label = "replay" if replaying else args.persona
    persona = None
    manual = None
    if not replaying:
        persona = load_persona(args.persona)
        if args.goal:
            persona = dict(persona, goal=args.goal)
        manual = _read_manual(args.manual)

    # An explicit --trace-dir is the caller's to manage; the DEFAULT is
    # atomically reserved so parallel same-persona sessions can't
    # collide on a same-second timestamp.
    trace_dir = args.trace_dir or _allocate_trace_dir(os.path.join(
        HERE, "sessions", time.strftime("%Y%m%d_%H%M%S") + f"_{os.path.basename(label)}"))
    from playtest import HARNESS_VERSION  # local package
    meta = {
        "harness_version": HARNESS_VERSION,
        "mode": "replay" if replaying else args.agent,
        "render_mode": args.render_mode,
        "port": args.port,
        "dt": args.dt,
        "turn_budget": args.turns,
        "time_budget_seconds": args.max_seconds,
        "stuck_k": args.stuck_k,
        "memory_turns": args.memory_turns,
        "persona": persona,
        "manual_path": None if replaying else args.manual,
        "player_model": None,
        "world_seed": None,  # promoted from the oracle's world.getSeed()
                             # the first turn a world exists — the seed the
                             # player actually got, randomized or typed
        "f4_outcomes_total": 0,  # running count; see _count_f4_outcomes
        "replay_of": os.path.abspath(args.replay) if replaying else None,
    }
    if not replaying and args.agent == "llm":
        meta["player_model"] = {"model": args.model, "effort": args.effort,
                                "max_tokens": args.max_tokens,
                                "thinking": "disabled"}

    if replaying:
        player = None
        src_meta = load_meta(args.replay)
        args.dt = src_meta.get("dt", args.dt)  # reproduce the pacing
        meta["dt"] = args.dt
    elif args.agent == "scripted":
        player = agent_mod.ScriptedAgent()
    else:
        player = agent_mod.PlayerAgent(persona, manual, model=args.model,
                                       effort=args.effort,
                                       max_tokens=args.max_tokens)

    if args.render_mode == "windowed":
        print("playtest: this launches a WINDOWED instance that will take "
              "over the screen and steal focus (--render-mode offscreen "
              "runs windowless).")
    else:
        print("playtest: offscreen instance (#650) — no window, no focus "
              "steal; needs a GPU.")
    print(f"playtest: trace -> {trace_dir}")
    trace = SessionTrace(trace_dir, meta)
    eng = PlaytestEngine(args.port,
                         log_path=os.path.join(trace_dir, "engine.raw.log"),
                         render_mode=args.render_mode)
    stop_reason = "error"
    try:
        eng.launch()
        # let the loading screen finish assembling the main menu
        time.sleep(3.0)
        if replaying:
            stop_reason = run_replay(eng, args.replay, trace, dt=args.dt)
        else:
            stop_reason = run_session(
                eng, player, trace, turns=args.turns, dt=args.dt,
                max_seconds=args.max_seconds, memory_turns=args.memory_turns,
                stuck_k=args.stuck_k)
    except EngineCrash as e:
        # a crash mid-session is a finding: keep the partial trace + logs
        print(f"  [crash] {e}")
        stop_reason = "engine_crash"
        trace.meta["crash_detail"] = str(e)
        trace.meta["engine_log_tail"] = eng.log_tail()
    except KeyboardInterrupt:
        stop_reason = "interrupted"
    finally:
        try:
            eng.quit()
        except Exception:
            pass
        trace.attach_engine_log(eng.log_path)
        trace.finish(stop_reason)

    print(f"playtest: session ended ({stop_reason}); trace at {trace_dir}")
    return 0 if stop_reason not in ("error",) else 1


if __name__ == "__main__":
    sys.exit(main())
