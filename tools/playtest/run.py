#!/usr/bin/env python3
"""Naive-player UX playtest harness — lockstep runner (H1, #647/#641).

⚠️  LAUNCHES A WINDOWED GAME INSTANCE THAT TAKES OVER YOUR SCREEN AND
STEALS FOCUS (F1 screenshots + F2 input need a real render pipeline —
GPU-less --headless cannot host a playtest). Run it while away from the
machine or on a second display/machine. Offscreen unattended mode is
P1 (#650). This is the one sanctioned exception to the never-launch-
graphical rule: the graphical instance IS the system under test.

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
        acks = eng.inject(calls)
        if calls:
            time.sleep(settle)  # let the Lua-side click/key consequences land

        # 5. oracle snapshot — recorded, never shown to the player
        oracle = eng.oracle_snapshot()
        _promote_seed(trace, oracle)
        _count_f4_outcomes(trace, oracle)

        # stuck-loop detection: same action, same pixels, K times in a
        # row. A repeat-with-no-change loop is itself a strong
        # missing-feedback signal — record it, then stop.
        sig = _action_sig(action)
        if sig == prev_sig and frame_hash == prev_frame_hash:
            stuck_count += 1
        else:
            stuck_count = 0
        prev_sig, prev_frame_hash = sig, frame_hash
        stuck = stuck_count >= stuck_k - 1 and turn >= stuck_k

        record = {
            "turn": turn,
            "ts": time.time(),
            "screenshot": os.path.relpath(frame, trace.dir),
            "fb_size": list(fb_size),
            "player": {k: decision.get(k) for k in
                       ("observation", "action", "expectation", "note",
                        "raw", "usage")},
            "injected": calls + post_calls,
            "acks": acks,
            "oracle": oracle,
            "stuck": stuck,
        }
        trace.record_turn(record)
        trace.record_replay(turn, calls, post_calls)

        note = decision.get("note") or ""
        print(f"  turn {turn:3d}: {action.get('do'):6s} "
              f"{'' if not note else '— ' + note[:80]}")

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

        # 6. step the sim by wall-clock dt, then the post-step calls
        # (a held key releases after riding through the step)
        eng.set_paused(False)
        time.sleep(dt)
        eng.set_paused(True)
        if post_calls:
            eng.inject(post_calls)

    return stop_reason


def run_replay(eng: PlaytestEngine, source_dir: str, trace: SessionTrace,
               dt: float, settle: float = 0.3) -> str:
    """Re-inject a recorded session's inputs — no LLM. Faithful to the
    session's structure: every recorded turn is replayed (including
    no-input ones, so pacing matches), pre-step calls land before the
    dt step and post-step calls (a held key's keyUp) after it, exactly
    as they did live. Wall-clock dt means it is still NOT guaranteed
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
        acks = eng.inject(entry["pre"])
        if entry["pre"]:
            time.sleep(settle)
        oracle = eng.oracle_snapshot()
        _promote_seed(trace, oracle)
        _count_f4_outcomes(trace, oracle)
        trace.record_turn({
            "turn": turn, "ts": time.time(),
            "screenshot": os.path.relpath(frame, trace.dir),
            "fb_size": list(fb_size),
            "player": None,   # replay has no player — inputs come from the trace
            "injected": entry["pre"] + entry["post"], "acks": acks,
            "oracle": oracle, "stuck": False,
        })
        print(f"  replay turn {turn:3d}: {len(entry['pre'])}+{len(entry['post'])} call(s)")
        eng.set_paused(False)
        time.sleep(dt)
        eng.set_paused(True)
        if entry["post"]:
            eng.inject(entry["post"])
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

        # 4. persona + prompt assembly stays oracle-blind by shape:
        # build_system_prompt takes persona/manual/fb only
        import inspect
        params = list(inspect.signature(agent_mod.build_system_prompt).parameters)
        check("prompt assembly accepts no oracle inputs",
              params == ["persona", "manual", "fb_size"], str(params))
        p = load_persona("curious_carl")
        prompt = agent_mod.build_system_prompt(p, "MANUAL", (1280, 720))
        check("prompt contains persona + manual + size",
              "curious_carl" in prompt and "MANUAL" in prompt
              and "1280x720" in prompt)

    if failures:
        print(f"selftest: FAILED ({len(failures)}): {', '.join(failures)}")
        return 1
    print("selftest: all checks passed")
    return 0


def main() -> int:
    ap = argparse.ArgumentParser(
        description=__doc__, formatter_class=argparse.RawDescriptionHelpFormatter)
    ap.add_argument("--port", type=int, default=DEFAULT_PORT)
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

    trace_dir = args.trace_dir or os.path.join(
        HERE, "sessions", time.strftime("%Y%m%d_%H%M%S") + f"_{os.path.basename(label)}")
    from playtest import HARNESS_VERSION  # local package
    meta = {
        "harness_version": HARNESS_VERSION,
        "mode": "replay" if replaying else args.agent,
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

    print("playtest: this launches a WINDOWED instance that will take over "
          "the screen and steal focus.")
    print(f"playtest: trace -> {trace_dir}")
    trace = SessionTrace(trace_dir, meta)
    eng = PlaytestEngine(args.port, log_path=os.path.join(trace_dir, "engine.raw.log"))
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
