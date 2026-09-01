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

A session is two halves separated by one observable boundary (#1539).
SETUP — build, engine + debug-console startup, and the wait for the
first frame that could really be handed to a player — runs under its
own `--setup-timeout` watchdog and is not play: however long it takes
(a cold worktree compiles the whole game), it consumes none of the
player-session budget, and a failure there is reported as the setup
phase it happened in, never as a player-session outcome. PLAY begins
only after that boundary, and every session budget — wall clock,
turns, decision timeout, player tokens, stuck detection, lockstep dt —
is anchored to it.

The lockstep loop per turn: pause -> screenshot (F1) -> player agent
decides from pixels alone -> inject its action (F2) -> record the
pre-step oracle context (F3 widgets, menu, pause state — stored for
the critic, never shown to the player) -> unpause for a fixed
wall-clock dt -> re-pause -> record the post-step oracle evidence
(event-log progress, F4 action outcomes, and the visible-change frame the
step itself produced, #775 — credited to THIS turn's action, never the
next turn's). Everything lands in a session-trace directory H2
consumes (see trace.py). Wall-clock dt is a deliberate simplicity
tradeoff: --replay reproduces the input sequence, not bit-identical
turns.

Usage:
  python3 tools/playtest/run.py                       # LLM player, defaults
  python3 tools/playtest/run.py --player claude-sonnet
  python3 tools/playtest/run.py --render-mode offscreen  # no window (#650)
  python3 tools/playtest/run.py --smoke               # 3 scripted turns, no LLM
  python3 tools/playtest/run.py --replay <trace_dir>  # re-inject a session
  python3 tools/playtest/run.py --selftest            # offline loop/trace check

The player uses one audited medium-effort profile through the installed Codex
or Claude CLI and its existing subscription login (`codex login status` or
`claude auth status`); scripted/smoke/replay/selftest runs don't.
"""
from __future__ import annotations

import argparse
import os
import sys
import time

HERE = os.path.dirname(os.path.abspath(__file__))
sys.path.insert(0, HERE)
sys.path.insert(0, os.path.dirname(HERE))

import launch as launch_mod  # noqa: E402
from engine import EngineCrash, PlaytestEngine  # noqa: E402
from personas import load_persona  # noqa: E402
# The session/replay implementation itself (#2040): this file is the
# command-line façade over it, and the offline self-test components
# import the SAME module rather than importing this one.
from session import _allocate_trace_dir, run_replay, run_session  # noqa: E402
from trace import SessionTrace, load_meta  # noqa: E402
from usage import (compact_tokens, default_artifacts_root,  # noqa: E402
                   default_usage_log, update_usage_log)
import agent as agent_mod  # noqa: E402

DEFAULT_PORT = 9308
DEFAULT_TURNS = 12
DEFAULT_MAX_SECONDS = 600.0
DEFAULT_PLAYER_TOKEN_BUDGET = 200_000
DEFAULT_MANUAL = os.path.join(os.path.dirname(os.path.dirname(HERE)),
                              "docs", "player_manual.md")
# Non-zero exit: the harness never got a session, either because setup
# failed before the player-ready boundary (#1539) or because something
# unclassified went wrong.
FAILED_STOP_REASONS = frozenset({"error"}) | launch_mod.SETUP_STOP_REASON_SET
MANUAL_STUB = ("(No manual was provided. You know nothing about this game "
               "beyond what you can see on screen.)")


def _read_manual(path: str) -> str:
    try:
        with open(path, encoding="utf-8") as f:
            return f.read()
    except OSError:
        print(f"  [warn] manual not found at {path}; running with a stub")
        return MANUAL_STUB


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
    ap.add_argument("--decision-timeout", type=float,
                    default=agent_mod.DEFAULT_DECISION_TIMEOUT,
                    help="maximum seconds for each player decision")
    ap.add_argument("--player", choices=sorted(agent_mod.PLAYER_PROFILES),
                    default=agent_mod.DEFAULT_PLAYER_PROFILE,
                    help="fixed medium-effort player profile")
    ap.add_argument("--turns", type=int, default=DEFAULT_TURNS)
    ap.add_argument("--max-seconds", type=float, default=DEFAULT_MAX_SECONDS,
                    help="wall-clock PLAYER-SESSION budget, counted from the "
                         "player-ready boundary (default: 600)")
    ap.add_argument("--setup-timeout", type=float,
                    default=launch_mod.DEFAULT_SETUP_TIMEOUT,
                    help="pre-ready setup watchdog in seconds, covering the "
                         "build, the engine/debug-console startup and the "
                         "wait for a first player-ready frame (default: "
                         f"{launch_mod.DEFAULT_SETUP_TIMEOUT:.0f}). An "
                         "infrastructure safety limit, NOT a playtest budget: "
                         "it is independent of --max-seconds and generous "
                         "enough for a full cold-worktree build")
    ap.add_argument("--max-player-tokens", type=int,
                    default=DEFAULT_PLAYER_TOKEN_BUDGET,
                    help="input+output token ceiling with projected next-turn "
                         "reserve (default: 200K)")
    ap.add_argument("--dt", type=float, default=2.0,
                    help="unpaused wall-clock seconds per turn")
    ap.add_argument("--memory-turns", type=int, default=8)
    ap.add_argument("--stuck-k", type=int, default=3,
                    help="identical action+frame repeats that count as stuck")
    ap.add_argument("--trace-dir", default=None,
                    help="session output dir (default tools/playtest/sessions/<ts>_<label>)")
    ap.add_argument("--usage-log", default=None,
                    help="aggregate Markdown usage ledger (default: shared "
                         ".git/codex-test/playtest-usage.md)")
    ap.add_argument("--agent", choices=["llm", "scripted"], default="llm")
    ap.add_argument("--smoke", action="store_true",
                    help="tiny scripted session (3 turns, no LLM)")
    ap.add_argument("--replay", metavar="TRACE_DIR", default=None,
                    help="re-inject a recorded session (no LLM)")
    ap.add_argument("--selftest", action="store_true",
                    help="offline harness check (no window, no engine, no API)")
    args = ap.parse_args()

    if args.selftest:
        # The offline suite lives in selftest.py and its per-domain
        # components (#2040); this branch stays the documented entry
        # point. Imported here, not at module load, so a production run
        # never pulls a test module (or FakeEngine) into the process.
        from selftest import selftest
        return selftest()

    if args.max_player_tokens <= 0:
        ap.error("--max-player-tokens must be positive")
    if args.max_seconds <= 0:
        ap.error("--max-seconds must be positive")
    if args.setup_timeout <= 0:
        ap.error("--setup-timeout must be positive")
    if args.port == launch_mod.GUI_PORT:
        ap.error(f"--port {launch_mod.GUI_PORT} is the graphical instance's "
                 "port; pass a 9xxx port")

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
    usage_log_path = args.usage_log or default_usage_log(os.getcwd())
    usage_artifacts_root = default_artifacts_root(os.getcwd())
    meta = {
        "harness_version": HARNESS_VERSION,
        "mode": "replay" if replaying else args.agent,
        "render_mode": args.render_mode,
        "port": args.port,
        "dt": args.dt,
        "turn_budget": args.turns,
        "time_budget_seconds": args.max_seconds,
        # The pre-ready watchdog, recorded beside the session budgets so a
        # trace says which limit could have ended it (#1539). It is not one
        # of the player-session budgets and never shortens them.
        "setup_timeout_seconds": args.setup_timeout,
        "player_token_budget": args.max_player_tokens,
        "usage_log_path": usage_log_path,
        "usage_artifacts_root": usage_artifacts_root,
        "account_remaining_tokens": "unavailable from noninteractive CLI",
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
        profile = agent_mod.PLAYER_PROFILES[args.player]
        meta["player_model"] = {
            "profile": args.player,
            "backend": profile["backend"],
            "model": profile["model"],
            "effort": profile["effort"],
            "decision_timeout_seconds": args.decision_timeout,
            "session_persistence": "ephemeral",
            "tools": ("disabled" if profile["backend"] == "codex-cli" else
                      agent_mod.CLAUDE_SCREENSHOT_READ_RULE + " only"),
        }

    if replaying:
        player = None
        src_meta = load_meta(args.replay)
        args.dt = src_meta.get("dt", args.dt)  # reproduce the pacing
        meta["dt"] = args.dt
    elif args.agent == "scripted":
        player = agent_mod.ScriptedAgent()
    else:
        player = agent_mod.PlayerAgent(
            persona, manual, player_profile=args.player,
            decision_timeout=args.decision_timeout)

    if args.render_mode == "windowed":
        print("playtest: this launches a WINDOWED instance that will take "
              "over the screen and steal focus (--render-mode offscreen "
              "runs windowless).")
    else:
        print("playtest: offscreen instance (#650) — no window, no focus "
              "steal; needs a GPU.")
    if player is not None and player.needs_llm:
        print(f"playtest: player -> {args.player}: {player.model} "
              f"({player.effort} effort)")
        print(f"playtest: bounds -> {args.turns} turns, "
              f"{compact_tokens(args.max_player_tokens)} player tokens, "
              f"{compact_tokens(int(args.max_seconds))}s wall clock")
        print("playtest: account-plan tokens remaining are not exposed by "
              "noninteractive Codex/Claude CLI output")
    print(f"playtest: trace -> {trace_dir}")
    trace = SessionTrace(trace_dir, meta)
    eng = PlaytestEngine(args.port,
                         log_path=os.path.join(trace_dir, "engine.raw.log"),
                         render_mode=args.render_mode)
    stop_reason = "error"
    try:
        # Build, boot and UI-load under their OWN watchdog, then cross the
        # player-ready boundary (#1539). Only after this returns does any
        # player-session budget begin, and no player call has happened yet.
        launch_mod.launch_player_ready(eng, trace,
                                       setup_timeout=args.setup_timeout)
        if replaying:
            stop_reason = run_replay(eng, args.replay, trace, dt=args.dt)
        else:
            stop_reason = run_session(
                eng, player, trace, turns=args.turns, dt=args.dt,
                max_seconds=args.max_seconds, memory_turns=args.memory_turns,
                stuck_k=args.stuck_k,
                max_player_tokens=args.max_player_tokens)
    except launch_mod.SetupFailure as e:
        # An INFRASTRUCTURE failure, never a player-session outcome: zero
        # turns, no replay entries, no player call, no tokens, and a
        # stop_reason naming which of the three setup phases failed. The
        # retained setup.log / engine.log are what tell a build failure,
        # a build timeout, an executable exit, a console-readiness
        # failure and a rendered-readiness failure apart.
        print(f"  [setup] {e}")
        stop_reason = e.stop_reason
        trace.meta["setup_failure"] = e.as_meta()
        trace.meta["setup_log_tail"] = launch_mod.log_tail(
            trace.setup_log_path()) or None
        trace.meta["engine_log_tail"] = eng.log_tail() or None
        if not launch_mod.teardown_setup(eng):
            print(f"  [setup] warning: port {args.port} was still held after "
                  "teardown; the next run on it may fail to bind")
    except EngineCrash as e:
        # a crash mid-session is a finding: keep the partial trace + logs
        print(f"  [crash] {e}")
        stop_reason = "engine_crash"
        trace.meta["crash_detail"] = str(e)
        trace.meta["engine_log_tail"] = eng.log_tail()
    except KeyboardInterrupt:
        stop_reason = "interrupted"
        if trace.meta.get("loaded_at") is None:
            # Interrupted before the boundary: the instance was never a
            # session, so tear it down the pre-ready way — reap the whole
            # group and wait for the port — rather than leaving an orphan
            # holding it (#1323).
            launch_mod.teardown_setup(eng)
    finally:
        try:
            eng.quit()
        except Exception:
            pass
        trace.attach_engine_log(eng.log_path)
        trace.finish(stop_reason)

    if not replaying and args.agent == "llm":
        try:
            from preanalysis import write_inspection_plan
            inspection_path = write_inspection_plan(trace_dir)
            print(f"playtest: deterministic inspection plan at {inspection_path}")
        except Exception as e:
            print(f"  [warn] could not write inspection plan: {e}")
        if usage_log_path:
            try:
                update_usage_log(usage_log_path, usage_artifacts_root,
                                 extra_trace_dir=trace_dir)
                print(f"playtest: usage ledger at {usage_log_path}")
            except Exception as e:
                print(f"  [warn] could not update usage ledger: {e}")

    if stop_reason in launch_mod.SETUP_STOP_REASON_SET:
        print(f"playtest: setup failed ({stop_reason}) before any player "
              f"session started; trace at {trace_dir}")
    else:
        print(f"playtest: session ended ({stop_reason}); trace at {trace_dir}")
    return 0 if stop_reason not in FAILED_STOP_REASONS else 1


if __name__ == "__main__":
    sys.exit(main())
