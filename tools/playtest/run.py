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
import hashlib
import json
import math
import os
import re
import sys
import time

HERE = os.path.dirname(os.path.abspath(__file__))
sys.path.insert(0, HERE)
sys.path.insert(0, os.path.dirname(HERE))

import launch as launch_mod  # noqa: E402
from engine import (ActionError, EngineCrash, FakeEngine,  # noqa: E402
                    PlaytestEngine, translate_action)
from personas import load_persona  # noqa: E402
from trace import SessionTrace, load_meta, load_replay, load_turns  # noqa: E402
from usage import (compact_tokens, default_artifacts_root, default_usage_log,
                   update_usage_log, usage_total)  # noqa: E402
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
    themselves live in each turn's oracle.action_outcomes. Callable
    with any dict carrying an `action_outcomes` key — including just
    one of a turn's two drains (#775) — since each drain's records are
    disjoint and the running total only needs to see each once."""
    trace.meta["f4_outcomes_total"] = (
        trace.meta.get("f4_outcomes_total", 0) + len(oracle.get("action_outcomes") or []))


def _merge_oracle(pre_ctx: dict, pre_events: dict, post_events: dict | None,
                  post_screenshot: str | None, visual_change: bool,
                  route_ctx: dict | None = None) -> dict:
    """Combine a turn's pre-step affordance context (widgets/menu/
    paused/seed, captured once after inject+settle — #775) with its
    event-log/action-outcome drains into the single oracle record the
    trace stores for that turn. `pre_events` is the drain taken at that
    same pre-step moment (whatever the action produced synchronously
    while still paused); `post_events` is the second drain taken right
    after the sim step, when one actually ran (whatever the unpaused
    `dt` interval itself produced) — concatenating rather than
    replacing keeps BOTH attributed to this turn: nothing synchronous
    is lost, and nothing step-produced leaks onto the next turn. `None`
    for `post_events` (a terminal turn with no step) leaves the merge
    as pre-only, matching there being no post-step evidence to report.

    `post_screenshot` is `None` (with `visual_change` False) whenever
    there is no post frame to name — either because no step ran, or
    because the frame could not be taken. #1752 makes the second case
    reachable: the caller merges the drained post-step events the
    moment it has them and only then attempts the screenshot, so a
    screenshot that raises still leaves the turn holding every event
    and outcome the destructive drain already consumed, with the
    missing frame stated as a null rather than as a turn that looks
    like it never stepped.

    `event_log_gaps` (#1714) concatenates the same way and for the same
    reason: each read reports the committed event rows it could NOT see,
    and both belong to this turn. They are deliberately not merged into
    `event_log_new` — a gap is the absence of evidence, and collapsing
    it into the row list would be indistinguishable from an unchanged
    log, which is the silent loss the sequence exists to expose.

    `route_ctx` (#1750) is the separate PRE-INJECTION widget read
    (`PlaytestEngine.oracle_routing`). It is stored under its own
    `routing_widgets` key rather than replacing `widgets`, because the
    two answer different questions: `widgets` is #775's post-inject,
    pre-step affordance context that the session digest and the seed
    promotion already consume with that meaning, while
    `routing_widgets` is the record set the click was actually routed
    against. The critic prefers `routing_widgets` for its click join
    and falls back to `widgets` when the key is absent, so a trace
    recorded before #1750 still correlates exactly as it did."""
    post_events = post_events or {}
    return {
        "player_invisible": True,
        "widgets": pre_ctx.get("widgets"),
        "routing_widgets": (route_ctx or {}).get("widgets"),
        "current_menu": pre_ctx.get("current_menu"),
        "paused": pre_ctx.get("paused"),
        "world_seed": pre_ctx.get("world_seed"),
        "event_log_new": (pre_events.get("event_log_new") or [])
                         + (post_events.get("event_log_new") or []),
        "event_log_gaps": (pre_events.get("event_log_gaps") or [])
                          + (post_events.get("event_log_gaps") or []),
        "action_outcomes": (pre_events.get("action_outcomes") or [])
                           + (post_events.get("action_outcomes") or []),
        "visual_change": visual_change,
        "post_screenshot": post_screenshot,
    }


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
                memory_turns: int, stuck_k: int, settle: float = 0.3,
                max_player_tokens: int | None = None) -> str:
    """The lockstep loop. Returns the stop reason.

    Every player-session budget below — wall clock, turns, decision
    timeout, player tokens and their projected reserve, stuck detection
    and the lockstep `dt` — is anchored HERE, at the first statement of
    the session (#1539), never at process start. Setup ran before this
    was ever called, so however long the build, boot and UI load took,
    the configured session budget arrives intact.
    """
    trace.mark_session_started()
    memory: list[str] = []
    prev_sig = None
    prev_frame_hash = None
    stuck_count = 0
    started = time.monotonic()
    stop_reason = "turn_budget_exhausted"
    usage_turn_totals: list[int] = []
    run_tokens = 0
    llm_player = bool(getattr(player, "needs_llm", False))

    for turn in range(1, turns + 1):
        elapsed = time.monotonic() - started
        if max_seconds is not None and elapsed >= max_seconds:
            stop_reason = "time_budget_exhausted"
            break
        if llm_player and max_player_tokens is not None and usage_turn_totals:
            projected = math.ceil(sum(usage_turn_totals)
                                  / len(usage_turn_totals))
            if run_tokens + projected > max_player_tokens:
                print("  [budget] reserving the projected next-turn cost "
                      f"({compact_tokens(projected)}); stopping at "
                      f"{compact_tokens(run_tokens)} of "
                      f"{compact_tokens(max_player_tokens)}")
                stop_reason = "token_budget_reserved"
                break

        # 1. ensure paused, 2. capture the frame
        eng.set_paused(True)
        frame = trace.frame_path(turn)
        fb_size = eng.screenshot(frame)
        frame_hash = _file_hash(frame)
        ts = time.time()

        # 3. the player decides — screenshot + its own memory ONLY
        remaining_seconds = (None if max_seconds is None else
                             max(0.0, max_seconds - (time.monotonic() - started)))
        bounded_by_session = (
            remaining_seconds is not None
            and remaining_seconds <= getattr(
                player, "decision_timeout", remaining_seconds))
        try:
            decision = player.decide(
                frame, fb_size, memory, turn,
                timeout_seconds=remaining_seconds)
        except agent_mod.DecisionTimeout as e:
            print(f"  [budget] {e}")
            stop_reason = ("time_budget_exhausted" if bounded_by_session
                           else "decision_timeout")
            break
        action = decision["action"]
        turn_tokens = usage_total(decision.get("usage"))
        usage_missing = llm_player and not trace.record_usage(
            decision.get("usage"))
        if turn_tokens is not None:
            usage_turn_totals.append(turn_tokens)
            run_tokens += turn_tokens

        # 4. translate + inject
        #
        # Two shapes of harness remark land in the SAME `[harness: ...]`
        # note, because the player reads that note back as its own memory
        # next turn and should not have to learn two vocabularies: a
        # raised ActionError means the request was refused and nothing was
        # injected, while a collected note means the request was honoured
        # in an adjusted form the note describes (#1980). Either way the
        # requested action itself stays in the record verbatim.
        harness_notes: list[str] = []
        try:
            calls, post_calls = translate_action(action, fb_size,
                                                 notes=harness_notes)
        except Exception as e:  # unusable action: record it, inject nothing
            decision["note"] = (decision.get("note", "")
                                + f" [harness: {e}]").strip()
            calls, post_calls = [], []
        for harness_note in harness_notes:
            decision["note"] = (decision.get("note", "")
                                + f" [harness: {harness_note}]").strip()

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
        route_ctx = None
        try:
            # 4a. routing oracle (#1750): the widget record set the
            # click is about to be routed against, read BEFORE the
            # first input call of this action lands. The action is
            # already chosen at this point, so this is the state the
            # player acted on AND the state the real pointer router
            # resolves against — a callback that opens, closes, or
            # replaces a modal, or that creates/destroys elements, can
            # no longer rewrite it. It rides into the record as its own
            # `routing_widgets` key; #775's post-inject `widgets` and
            # the menu/pause/seed fields keep their existing sampling
            # point below.
            route_ctx = eng.oracle_routing()
            # inject one call at a time so a mid-action crash still
            # records the acknowledged prefix
            for call in calls:
                acks.extend(eng.inject([call]))
                sent.append(call)
            if calls:
                time.sleep(settle)  # let the Lua-side click/key consequences land

            # 5. pre-step oracle (#775): the affordance context the
            # player actually acted on (widgets/menu/paused/seed) plus
            # whatever event/outcome evidence the action already
            # produced synchronously, while still paused — recorded
            # regardless of whether a sim step follows, never shown to
            # the player. Built into `oracle` immediately so a turn
            # that gets interrupted before its own step completes still
            # retains at least this much (rather than losing everything
            # to a step-scoped local that's never assigned).
            pre_ctx = eng.oracle_context()
            pre_events = eng.oracle_events()
            oracle = _merge_oracle(pre_ctx, pre_events, None, None, False,
                                   route_ctx)
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
            if llm_player:
                budget_left = (None if max_player_tokens is None else
                               max(0, max_player_tokens - run_tokens))
                print("             tokens: "
                      f"{compact_tokens(turn_tokens)} this turn · "
                      f"{compact_tokens(run_tokens)} run · "
                      f"{compact_tokens(budget_left)} budget left · "
                      "account left unavailable")

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

                # 7. post-step oracle (#775): whatever the unpaused dt
                # interval itself produced, attributed to THIS turn's
                # action — never deferred to the next turn's pre-step
                # capture (which is what silently misattributed it
                # before, and dropped it outright on a budget-limited
                # final turn, since there was no "next" iteration left
                # to do the deferred capture at all).
                post_events = eng.oracle_events()
                _count_f4_outcomes(trace, post_events)
                # The drain above is DESTRUCTIVE — the engine's cursor
                # has already advanced past those rows and no later
                # call can return them — so the merge happens HERE,
                # before the fallible screenshot, for exactly the
                # reason the pre-step merge does (#1752). Taking the
                # frame first meant a screenshot crash recorded the
                # pre-only merge over evidence the runner had already
                # consumed, while `_count_f4_outcomes` had ALREADY
                # counted those outcomes into the session total — a
                # reader saw counted outcomes with nothing behind them.
                # `post_screenshot=None`/`visual_change=False` is
                # _merge_oracle's existing "no post frame to report"
                # representation; it is upgraded in place below once
                # the frame is actually on disk and hashed.
                oracle = _merge_oracle(pre_ctx, pre_events, post_events,
                                       None, False, route_ctx)
                post_frame = trace.post_frame_path(turn)
                eng.screenshot(post_frame)
                visual_change = _file_hash(post_frame) != frame_hash
                oracle = _merge_oracle(
                    pre_ctx, pre_events, post_events,
                    os.path.relpath(post_frame, trace.dir), visual_change,
                    route_ctx)
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
        if usage_missing:
            print("  [budget] provider returned no token usage; stopping "
                  "rather than continuing unmetered")
            stop_reason = "usage_unavailable"
            break
        if max_player_tokens is not None and run_tokens >= max_player_tokens:
            stop_reason = "token_budget_exhausted"
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
    trace.mark_session_started()
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
        frame_hash = _file_hash(frame)
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
        route_ctx = None
        try:
            # Routing oracle (#1750) — see run_session's identical
            # comment: read before this turn's first replayed input
            # call, so a replay trace carries the same pre-injection
            # correlation context a live session does.
            route_ctx = eng.oracle_routing()
            for call in entry["pre"]:
                acks.extend(eng.inject([call]))
                sent.append(call)
            if entry["pre"]:
                time.sleep(settle)
            # Pre-step oracle (#775) — see run_session's identical
            # comment: built into `oracle` immediately so an interrupted
            # step still retains at least this much.
            pre_ctx = eng.oracle_context()
            pre_events = eng.oracle_events()
            oracle = _merge_oracle(pre_ctx, pre_events, None, None, False,
                                   route_ctx)
            _promote_seed(trace, oracle)
            _count_f4_outcomes(trace, oracle)
            if entry["step_phase"] != "not_started":
                _run_step(eng, dt, phase)
                if entry["step_phase"] == "completed":
                    for call in entry["post"]:
                        post_acks.extend(eng.inject([call]))
                        post_sent.append(call)
                # Post-step oracle (#775): what THIS replay's own step
                # produced, attributed to this turn — same reasoning as
                # run_session.
                post_events = eng.oracle_events()
                _count_f4_outcomes(trace, post_events)
                # Merged before the screenshot for the same reason as
                # run_session (#1752) — see its comment there: replay
                # drains the same destructive cursor, so a failing post
                # frame must not cost this turn its own drained
                # evidence either.
                oracle = _merge_oracle(pre_ctx, pre_events, post_events,
                                       None, False, route_ctx)
                post_frame = trace.post_frame_path(turn)
                eng.screenshot(post_frame)
                visual_change = _file_hash(post_frame) != frame_hash
                oracle = _merge_oracle(
                    pre_ctx, pre_events, post_events,
                    os.path.relpath(post_frame, trace.dir), visual_change,
                    route_ctx)
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
    import socket
    import subprocess
    import tempfile

    import engine as engine_mod

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
        from critic import build_signals as _build_signals
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

        # The naive player can select exactly one of two audited profiles. The
        # model and effort inside each profile remain hard pins, and both run
        # from an empty cwd without repository or network access.
        player_params = list(inspect.signature(agent_mod.PlayerAgent).parameters)
        check("naive player accepts only a complete profile selection",
              player_params == ["persona", "manual", "player_profile",
                                "decision_timeout"],
              str(player_params))
        check("approved player profiles pin both medium-effort models",
              agent_mod.PLAYER_PROFILES == {
                  "codex-luna": {
                      "backend": "codex-cli", "model": "gpt-5.6-luna",
                      "effort": "medium", "binary": "codex"},
                  "claude-sonnet": {
                      "backend": "claude-cli", "model": "claude-sonnet-5",
                      "effort": "medium", "binary": "claude"},
              })
        codex_cmd = agent_mod._build_codex_command(
            "/usr/bin/codex", "frame.png", os.path.join(tmp, "empty"),
            os.path.join(tmp, "turn.schema.json"), os.path.join(tmp, "turn.json"))
        check("Codex profile invokes gpt-5.6-luna medium",
              codex_cmd[:2] == ["/usr/bin/codex", "exec"]
              and "gpt-5.6-luna" in codex_cmd
              and 'model_reasoning_effort="medium"' in codex_cmd)
        check("Codex player cannot inspect the repo or acquire oracle data",
              "--ignore-user-config" in codex_cmd
              and "--ignore-rules" in codex_cmd
              and "--ephemeral" in codex_cmd
              and 'web_search="disabled"' in codex_cmd
              and all(feature in codex_cmd for feature in
                      ("shell_tool", "multi_agent", "plugins", "skill_search")))
        claude_cmd = agent_mod._build_claude_command(
            "/usr/bin/claude", os.path.join(tmp, "empty"), "SYSTEM")
        check("Claude profile invokes claude-sonnet-5 medium in safe mode",
              claude_cmd[:2] == ["/usr/bin/claude", "-p"]
              and "claude-sonnet-5" in claude_cmd
              and claude_cmd[claude_cmd.index("--effort") + 1] == "medium"
              and "--safe-mode" in claude_cmd
              and "--no-session-persistence" in claude_cmd)
        check("Claude player can read only the isolated screenshot",
              claude_cmd[claude_cmd.index("--tools") + 1] == "Read"
              and claude_cmd[claude_cmd.index("--allowedTools") + 1]
              == "Read(./screenshot.png)"
              and "--strict-mcp-config" in claude_cmd
              and "--disable-slash-commands" in claude_cmd)
        allowed_tool_values = [
            claude_cmd[i + 1] for i, value in enumerate(claude_cmd[:-1])
            if value == "--allowedTools"]
        check("Claude permission allowlist excludes every alternate read path",
              allowed_tool_values == ["Read(./screenshot.png)"]
              and "Read" not in allowed_tool_values
              and not any("**" in rule or "../" in rule
                          for rule in allowed_tool_values),
              str(allowed_tool_values))
        action_schema = agent_mod.TURN_SCHEMA["properties"]["action"]
        check("Codex strict action schema requires every declared field",
              set(action_schema["required"]) == set(action_schema["properties"])
              and all("null" in spec["type"] for name, spec in
                      action_schema["properties"].items() if name != "do"))
        normalized_nulls = agent_mod.normalize_turn({
            "observation": "",
            "action": {"do": "wait", "x": None, "name": None},
            "expectation": "", "note": ""})
        check("strict-schema null placeholders stay out of trace actions",
              normalized_nulls["action"] == {"do": "wait"},
              str(normalized_nulls["action"]))

        # --- the wheel contract (#1980) ---------------------------------
        # Two independent halves, both offline: the contract the player is
        # HANDED must agree with the engine's own sign convention, and the
        # contract the harness ENFORCES must not forward a delta the
        # published range excludes.
        camera_hs = os.path.join(launch_mod.REPO_ROOT, "src", "Engine",
                                 "Loop", "Camera.hs")
        camera_src = ""
        try:
            with open(camera_hs, encoding="utf-8") as f:
                camera_src = f.read()
        except OSError as e:
            check("Engine.Loop.Camera is readable for the polarity check",
                  False, str(e))
        # Derive which dy sign moves the camera toward the ground from the
        # checked-in Haskell rather than restating a Python constant. The
        # impulse is `zoomScrollScale * zoom * dy` with camZoom the viewport
        # half-height, and zoomMin is annotated as the CLOSEST zoom — so a
        # negative dy zooms in exactly when the scale is positive. Each
        # premise is matched explicitly, so a change to the formula, the
        # scale's sign, or which bound is closest fails this check loudly
        # instead of being derived past.
        impulse_ok = bool(re.search(
            r"scrollZoomImpulse\s+zoom\s+dy\s*=\s*zoomScrollScale\s*\*"
            r"\s*zoom\s*\*\s*dy", camera_src))
        scale_m = re.search(r"^zoomScrollScale\s*=\s*(-?[\d.]+)",
                            camera_src, re.M)
        min_m = re.search(r"^zoomMin\s*=\s*(-?[\d.]+)\s*--\s*closest zoom",
                          camera_src, re.M)
        max_m = re.search(r"^zoomMax\s*=\s*(-?[\d.]+)", camera_src, re.M)
        camera_premises = bool(impulse_ok and scale_m and min_m and max_m)
        check("Engine.Loop.Camera still states the premises the playtest "
              "wheel polarity is derived from",
              camera_premises,
              f"impulse={impulse_ok} scale={bool(scale_m)} "
              f"min={bool(min_m)} max={bool(max_m)}")
        engine_zoom_in_sign = None
        if camera_premises:
            scale = float(scale_m.group(1))
            zmin, zmax = float(min_m.group(1)), float(max_m.group(1))
            # zoom is a positive half-height, so sign(impulse) = sign(scale)
            # * sign(dy); a negative impulse walks camZoom toward the
            # smaller bound, which the source annotates as the closest one.
            if scale > 0 and zmin < zmax:
                engine_zoom_in_sign = -1
            elif scale < 0 and zmin < zmax:
                engine_zoom_in_sign = 1
        prompt = agent_mod.build_system_prompt(
            {"name": "n", "temperament": "t", "tendencies": ["x"],
             "goal": "g"}, "manual", (1280, 720))
        stated = re.findall(r"(?i)\b(negative|positive) dy zooms in", prompt)
        check("the player contract states the wheel polarity exactly once",
              len(stated) == 1, str(stated))
        stated_sign = ({"negative": -1, "positive": 1}[stated[0].lower()]
                       if len(stated) == 1 else None)
        check("the player contract's zoom-in sign matches "
              "Engine.Loop.Camera's own convention",
              stated_sign is not None
              and stated_sign == engine_zoom_in_sign,
              f"contract={stated_sign} engine={engine_zoom_in_sign}")
        stated_range = re.search(
            r"dy must be between (-?[\d.]+) and (-?[\d.]+)", prompt)
        check("the player contract publishes the enforced dy range",
              bool(stated_range)
              and float(stated_range.group(1)) == engine_mod.SCROLL_DY_MIN
              and float(stated_range.group(2)) == engine_mod.SCROLL_DY_MAX,
              stated_range.group(0) if stated_range else "absent")
        check("the player contract names one ordinary wheel notch",
              f"one notch is {engine_mod.SCROLL_DY_NOTCH:g}" in prompt)
        schema_dy = agent_mod.TURN_SCHEMA["properties"]["action"][
            "properties"]["dy"]
        schema_range = re.search(r"between (-?[\d.]+) and (-?[\d.]+)",
                                 schema_dy.get("description", ""))
        check("the structured schema publishes the same dy range",
              bool(schema_range)
              and float(schema_range.group(1)) == engine_mod.SCROLL_DY_MIN
              and float(schema_range.group(2)) == engine_mod.SCROLL_DY_MAX,
              schema_dy.get("description", "")[:60])

        def scroll_calls(act):
            collected: list[str] = []
            calls, post = translate_action(act, (1280, 720),
                                           notes=collected)
            return calls, post, collected

        def scroll_dy_of(calls):
            vals = [float(m.group(1)) for m in
                    (re.search(
                        r"input\.scroll\([^,]+,\s*"
                        r"(-?[\d.]+(?:[eE][-+]?\d+)?)\)", c)
                     for c in calls) if m]
            return vals

        notch, _, notch_notes = scroll_calls(
            {"do": "scroll", "dy": -engine_mod.SCROLL_DY_NOTCH})
        check("one ordinary notch is forwarded verbatim, unremarked",
              scroll_dy_of(notch) == [-engine_mod.SCROLL_DY_NOTCH]
              and notch_notes == [] and len(notch) == 1,
              str(notch))
        multi, _, multi_notes = scroll_calls({"do": "scroll", "dy": -4})
        check("a bounded multi-notch correction is one unremarked call",
              scroll_dy_of(multi) == [-4.0] and multi_notes == []
              and len(multi) == 1, str(multi))
        for edge in (engine_mod.SCROLL_DY_MIN, engine_mod.SCROLL_DY_MAX):
            edge_calls, _, edge_notes = scroll_calls(
                {"do": "scroll", "dy": edge})
            check(f"dy at the inclusive range edge {edge:g} is not clamped",
                  scroll_dy_of(edge_calls) == [edge] and edge_notes == [],
                  str(edge_calls) + str(edge_notes))
        # Requirement 4's headline is deliberately written against
        # translate_action's historical two-argument call, so it states a
        # property of the translation boundary itself rather than of this
        # revision's note plumbing: the same call forwarded dy=600 to the
        # engine verbatim before this contract existed.
        legacy_big, _ = translate_action({"do": "scroll", "dy": 600},
                                         (1280, 720))
        check("an oversized dy never reaches the engine verbatim",
              scroll_dy_of(legacy_big) == [engine_mod.SCROLL_DY_MAX]
              and len(legacy_big) == 1, str(legacy_big))
        big, _, big_notes = scroll_calls({"do": "scroll", "dy": 600})
        check("the turn records that the oversized dy was clamped, with "
              "both the requested and the effective value",
              len(big_notes) == 1 and "clamped" in big_notes[0]
              and "600" in big_notes[0]
              and f"{engine_mod.SCROLL_DY_MAX:g}" in big_notes[0],
              str(big_notes))
        for bad in (float("nan"), float("inf"), float("-inf")):
            rejected = None
            try:
                scroll_calls({"do": "scroll", "dy": bad})
            except ActionError as e:
                rejected = str(e)
            check(f"a non-finite dy ({bad}) is rejected, not forwarded",
                  rejected is not None and "rejected" in rejected,
                  str(rejected))
        # An in-range fraction must survive serialization: at any fixed
        # decimal width a small real gesture becomes a literal 0.0, and a
        # value just inside a bound rounds onto it while the turn records
        # no clamp — both of them the accepted no-op this contract exists
        # to stop.
        for fraction in (0.00001, -0.00001, 9.99999, -9.99999, 0.25):
            fcalls, _, fnotes = scroll_calls({"do": "scroll", "dy": fraction})
            check(f"an in-range fraction {fraction!r} is serialized "
                  "losslessly and unremarked",
                  scroll_dy_of(fcalls) == [fraction] and fnotes == [],
                  str(fcalls) + str(fnotes))
        # The translation boundary is the one that has to hold, so it
        # types dy itself instead of trusting the schema: a numeric string
        # and a bool are exactly what a lenient provider fallback and a
        # scripted agent produce, and float() would have accepted both.
        for bogus in ("5", "-1", True, False, [], {}, complex(1, 0)):
            typed = None
            try:
                scroll_calls({"do": "scroll", "dy": bogus})
            except ActionError as e:
                typed = str(e)
            check(f"a non-numeric dy ({bogus!r}) is rejected, not coerced",
                  typed is not None and "rejected" in typed, str(typed))
        # A schema-valid integer can be arbitrary precision and sit
        # entirely outside float range. It is still FINITE, so the
        # contract clamps it; converting first would raise OverflowError
        # and reject it instead.
        for huge, bound in ((10 ** 400, engine_mod.SCROLL_DY_MAX),
                            (-(10 ** 400), engine_mod.SCROLL_DY_MIN)):
            hcalls2, _, hnotes2 = scroll_calls({"do": "scroll", "dy": huge})
            check("an integer too large to be a float is clamped, not "
                  f"rejected (sign {'+' if huge > 0 else '-'})",
                  scroll_dy_of(hcalls2) == [bound] and len(hcalls2) == 1
                  and len(hnotes2) == 1 and "clamped" in hnotes2[0]
                  and f"({len(str(huge))} digits)" in hnotes2[0],
                  str(hcalls2) + str(hnotes2))
        # A clamp note must describe a scroll the engine actually
        # received. When a companion field fails to translate the turn
        # injects nothing, so claiming a clamp would put a false entry in
        # the trace and in the player's own memory.
        for companion in ({"dx": "invalid"}, {"x": "a", "y": 1}):
            bad_notes: list[str] = []
            raised = None
            try:
                translate_action({"do": "scroll", "dy": 600, **companion},
                                 (1280, 720), notes=bad_notes)
            except Exception as e:
                raised = e
            check("a scroll that fails to translate records no clamp "
                  f"note ({sorted(companion)})",
                  raised is not None and bad_notes == [],
                  f"{type(raised).__name__ if raised else None} {bad_notes}")
        absent, _, absent_notes = scroll_calls({"do": "scroll", "dx": 2})
        check("an absent dy still defaults to a zero vertical delta",
              scroll_dy_of(absent) == [0.0] and absent_notes == [],
              str(absent))
        aimed, _, _ = scroll_calls(
            {"do": "scroll", "dy": -2, "x": 640, "y": 360})
        check("cursor-aimed scrolling still pre-moves, then scrolls once",
              len(aimed) == 2 and aimed[0].startswith("return input.moveMouse")
              and "input.scroll" in aimed[1]
              and scroll_dy_of(aimed) == [-2.0], str(aimed))
        for horizontal in (3, 600, -7.5):
            hcalls, _, hnotes = scroll_calls(
                {"do": "scroll", "dx": horizontal, "dy": 0})
            check(f"horizontal dx {horizontal:g} keeps its verbatim "
                  "forwarding",
                  len(hcalls) == 1
                  and f"input.scroll({float(horizontal):.1f}," in hcalls[0]
                  and hnotes == [], str(hcalls))
        check("every scroll action generates exactly one input.scroll call",
              all(sum("input.scroll" in c for c in cs) == 1
                  for cs in (notch, multi, big, legacy_big, aimed)))

        # The clamp reaches the trace through the real recording path:
        # requested action retained, clamp recorded in the note, and only
        # the bounded call in injected/replay.
        cdir = os.path.join(tmp, "scroll-clamp")
        ctrace2 = SessionTrace(cdir, {"mode": "selftest-scroll-clamp"})
        run_session(FakeEngine(),
                    agent_mod.ScriptedAgent([{"do": "scroll", "dy": 600}]),
                    ctrace2, turns=1, dt=0.0, max_seconds=None,
                    memory_turns=4, stuck_k=99, settle=0.0)
        ctrace2.finish("turn_budget_exhausted")
        cturn = load_turns(cdir)[0]
        creplay = load_replay(cdir)[0]
        check("the clamped turn retains the action the player requested",
              cturn["player"]["action"] == {"do": "scroll", "dy": 600},
              str(cturn["player"]["action"]))
        check("the clamped turn's note says a clamp happened",
              "clamped" in cturn["player"]["note"],
              cturn["player"]["note"])
        check("only the bounded call lands in injected and replay data",
              cturn["injected"] == creplay["pre"]
              and len(cturn["injected"]) == 1
              and scroll_dy_of(cturn["injected"]) == [
                  engine_mod.SCROLL_DY_MAX]
              and "600" not in cturn["injected"][0],
              str(cturn["injected"]))
        usage = agent_mod._parse_codex_usage(
            '{"type":"thread.started"}\n'
            '{"type":"turn.completed","usage":{"input_tokens":123,'
            '"cached_input_tokens":45,"output_tokens":67}}\n')
        check("Codex JSONL token usage maps into the existing trace shape",
              usage == {"input_tokens": 123, "output_tokens": 67,
                        "cache_read_input_tokens": 45}, str(usage))
        (claude_turn, claude_usage,
         claude_fallback) = agent_mod._parse_claude_result(json.dumps({
            "structured_output": {
                "observation": "menu", "action": {"do": "wait"},
                "expectation": "", "note": ""},
            "modelUsage": {
                "claude-sonnet-5": {
                    "inputTokens": 2, "outputTokens": 52,
                    "cacheReadInputTokens": 1085,
                    "cacheCreationInputTokens": 0},
                "claude-haiku-4-5": {
                    "inputTokens": 897, "outputTokens": 12,
                    "cacheReadInputTokens": 0,
                    "cacheCreationInputTokens": 0},
            },
        }))
        check("Claude usage includes cached input and helper-model calls",
              claude_turn["action"] == {"do": "wait"}
              and claude_usage == {
                  "input_tokens": 1984, "output_tokens": 64,
                  "cache_read_input_tokens": 1085,
                  "cache_creation_input_tokens": 0}, str(claude_usage))
        check("a usable structured_output needs no fallback text",
              claude_fallback == "", repr(claude_fallback))

        # #1874: a player reply that parses as valid JSON but is not an
        # object is a confused turn, not a crash. Driven through
        # PlayerAgent.decide with only the provider process faked, so the
        # whole production path runs — including _parse_claude_result's
        # own fallback handoff — and the checks assert the observable
        # turn rather than a helper's return value.
        shot = os.path.join(tmp, "reply_shape_frame.png")
        with open(shot, "wb") as f:
            f.write(b"\x89PNG\r\n\x1a\n")
        codex_usage_stdout = (
            '{"type":"thread.started"}\n'
            '{"type":"turn.completed","usage":{"input_tokens":123,'
            '"cached_input_tokens":45,"output_tokens":67}}\n')
        codex_expected_usage = {"input_tokens": 123, "output_tokens": 67,
                                "cache_read_input_tokens": 45}
        claude_model_usage = {
            "claude-sonnet-5": {
                "inputTokens": 2, "outputTokens": 52,
                "cacheReadInputTokens": 1085, "cacheCreationInputTokens": 0}}
        claude_expected_usage = {
            "input_tokens": 1087, "output_tokens": 52,
            "cache_read_input_tokens": 1085, "cache_creation_input_tokens": 0}

        def decide_with_reply(backend, stdout="", file_text=None):
            """One real decide() turn against a faked provider process."""
            profile_name = ("codex-luna" if backend == "codex-cli"
                            else "claude-sonnet")
            player = object.__new__(agent_mod.PlayerAgent)
            player.provider_bin = "/nonexistent/provider"
            player.player_profile = profile_name
            player.backend = backend
            player.persona = p
            player.manual = "MANUAL"
            player.profile = dict(agent_mod.PLAYER_PROFILES[profile_name])
            player.model = player.profile["model"]
            player.effort = player.profile["effort"]
            player.decision_timeout = 30.0
            player.needs_llm = True

            def fake_run(command, **kwargs):
                if file_text is not None:
                    out_path = command[
                        command.index("--output-last-message") + 1]
                    with open(out_path, "w", encoding="utf-8") as handle:
                        handle.write(file_text)
                return subprocess.CompletedProcess(command, 0, stdout, "")

            saved_run = agent_mod.subprocess.run
            agent_mod.subprocess.run = fake_run
            try:
                return player.decide(shot, (1280, 720), [], 1)
            finally:
                agent_mod.subprocess.run = saved_run

        def check_unusable_reply(label, turn, raw, usage):
            check(label,
                  turn["action"] == {"do": "wait"}
                  and turn["observation"] == ""
                  and turn["expectation"] == ""
                  and turn["note"] == agent_mod.NON_OBJECT_NOTE
                  and turn["note"] != agent_mod.NOT_JSON_NOTE
                  and turn["raw"] == raw
                  and turn["usage"] == usage, str(turn))

        # Falsy non-object through the Codex path, which reaches
        # normalize_turn outside decide's parse-exception handler.
        check_unusable_reply(
            "a falsy non-object Codex reply is a recorded wait, not a crash",
            decide_with_reply("codex-cli", stdout=codex_usage_stdout,
                              file_text="[]"),
            "[]", codex_expected_usage)
        check_unusable_reply(
            "a scalar Codex reply is a recorded wait, not a crash",
            decide_with_reply("codex-cli", stdout=codex_usage_stdout,
                              file_text='"wait"'),
            '"wait"', codex_expected_usage)

        # Claude: a non-mapping structured_output falls back to `result`,
        # which is itself valid non-object JSON. Before #1874 this left
        # _parse_claude_result returning a list and crashed normalize_turn.
        check_unusable_reply(
            "a non-object Claude fallback is a recorded wait, not a crash",
            decide_with_reply("claude-cli", stdout=json.dumps({
                "structured_output": [],
                "result": "[1, 2]",
                "modelUsage": claude_model_usage})),
            "[1, 2]", claude_expected_usage)
        # A `null` fallback: valid JSON of the wrong type, which before
        # #1874 was misreported as malformed JSON with the text erased.
        check_unusable_reply(
            "a null Claude fallback keeps its text and its own note",
            decide_with_reply("claude-cli", stdout=json.dumps({
                "result": "null", "modelUsage": claude_model_usage})),
            "null", claude_expected_usage)

        # The narrow guard: text that is not JSON at all still gets the
        # existing malformed-JSON wait, and a well-formed object still
        # becomes its own turn with usage intact.
        not_json_turn = decide_with_reply(
            "codex-cli", stdout=codex_usage_stdout, file_text="sorry, no idea")
        check("unparseable reply text keeps the malformed-JSON wait",
              not_json_turn["action"] == {"do": "wait"}
              and not_json_turn["note"] == agent_mod.NOT_JSON_NOTE
              and not_json_turn["raw"] == "sorry, no idea"
              and not_json_turn["usage"] == codex_expected_usage,
              str(not_json_turn))
        good_turn = decide_with_reply("claude-cli", stdout=json.dumps({
            "structured_output": {
                "observation": "menu", "action": {"do": "scroll", "dy": -1},
                "expectation": "list moves", "note": "trying"},
            "modelUsage": claude_model_usage}))
        check("a well-formed structured reply still becomes its own turn",
              good_turn["action"] == {"do": "scroll", "dy": -1}
              and good_turn["observation"] == "menu"
              and good_turn["note"] == "trying"
              and good_turn["usage"] == claude_expected_usage,
              str(good_turn))
        non_object_fallback = agent_mod._parse_claude_result(
            json.dumps({"result": "[]"}))
        check("_parse_claude_result never hands back a non-mapping turn",
              non_object_fallback[0] is None
              and non_object_fallback[2] == "[]", str(non_object_fallback))

        # 7. event-log progress (#699, #1714): the engine-side store
        # appends, moves coalesced updates to the tail, and drops from the
        # head at capacity. Every row carries a store-assigned `sequence`
        # -- consecutive from 1, in commit order -- so progress is
        # arithmetic on identities rather than a guess from row values.
        # Exercised through the real oracle state tracker, not only the
        # pure helper.
        from engine import EVENT_LOG_PROGRESS_LUA

        class EventLogEngine(RealEngine):
            """Replays a scripted series of event-log observations.

            Each entry is either a list of rows -- whose own highest
            sequence is then the store's high-water mark, which is what
            an un-reset ring always reports -- or an explicit
            `(rows, high_water)` pair, for the load-publish shape where
            the counter has run ahead of the surviving rows.
            """

            def __init__(self, logs):
                self.logs = iter([self._shape(entry) for entry in logs])
                super().__init__(0, os.devnull)

            @staticmethod
            def _shape(entry):
                if not isinstance(entry, (list, tuple)):
                    # A raw reply, passed through verbatim: the
                    # malformed-shape cases below need the engine to
                    # answer with a Lua nil, an error string or a
                    # partial table, none of which this fixture should
                    # tidy up on the way past.
                    return entry
                if isinstance(entry, tuple):
                    rows, high_water = entry
                else:
                    rows = entry
                    # Only well-formed sequences count: the malformed-row
                    # cases below deliberately ship rows the contract
                    # rejects, and shaping them must not raise before
                    # the oracle gets its chance to.
                    high_water = max([0] + [r["sequence"] for r in rows
                                            if isinstance(r, dict)
                                            and isinstance(r.get("sequence"),
                                                           int)
                                            and not isinstance(
                                                r.get("sequence"), bool)])
                # An empty Lua array serializes as `{}`, and the empty
                # ring is the case that matters most here, so the
                # fixture reproduces that shape rather than smoothing it.
                return {"rows": rows if rows else {}, "highest": high_water}

            def lua(self, code, timeout=0):
                if code == EVENT_LOG_PROGRESS_LUA:
                    return next(self.logs)
                if code == "return debug.drainActionOutcomes()":
                    return []
                raise AssertionError(f"unexpected oracle call: {code}")

        def row(seq, text, game_time=0.0, count=1):
            """One `engine.getEventLog()` row, shaped like the real one."""
            return {"sequence": seq, "category": "probe", "text": text,
                    "gameTime": game_time, "source": "selftest",
                    "count": count}

        def observe(logs):
            """Drive one engine through `logs`, one oracle read each."""
            eng = EventLogEngine(logs)
            return [eng.oracle_events() for _ in logs]

        repeat = row(1, "repeat")
        stable = row(2, "stable")
        appended = row(3, "appended")
        coalesced = row(4, "repeat", count=2)
        rollover = row(5, "rollover")
        reappended = row(6, "appended")
        reads = observe([
            [repeat, stable],
            [repeat, stable, appended],
            [repeat, stable, appended],
            [stable, appended, coalesced],
            [appended, coalesced, rollover],
            [coalesced, rollover, reappended],
        ])
        deltas = [r["event_log_new"] for r in reads]
        gaps = [r["event_log_gaps"] for r in reads]
        check("event-log first snapshot explicitly reports its full baseline",
              deltas[0] == [repeat, stable] and gaps[0] == [], str(reads[0]))
        check("event-log append reports only the appended row",
              deltas[1] == [appended] and gaps[1] == [], str(reads[1]))
        check("event-log unchanged snapshot reports no duplicate rows",
              deltas[2] == [] and gaps[2] == [], str(reads[2]))
        check("event-log coalesce reports only the replacement row; the row "
              "it superseded was already delivered, so it is not a gap",
              deltas[3] == [coalesced] and gaps[3] == [], str(reads[3]))
        check("event-log rollover reports the new tail row",
              deltas[4] == [rollover] and gaps[4] == [], str(reads[4]))
        check("event-log rollover detects a new row matching an evicted row",
              deltas[5] == [reappended] and gaps[5] == [], str(reads[5]))

        # An INTERMEDIATE coalesced replacement — one whose superseded
        # row was itself committed since the last observation, so the
        # observer never saw it. The replacement is reported, and the
        # sequence it retired is a one-element gap: from the snapshot a
        # superseded sequence is indistinguishable from an evicted one,
        # and the gap object deliberately claims only absence.
        reads = observe([
            [row(1, "a"), row(2, "b")],
            [row(1, "a"), row(2, "b"), row(4, "c", count=2)],
        ])
        check("a coalesce that happened BETWEEN observations reports the "
              "replacement and the superseded sequence as a one-element gap",
              reads[1]["event_log_new"] == [row(4, "c", count=2)]
              and reads[1]["event_log_gaps"] == [
                  {"first_sequence": 3, "last_sequence": 3,
                   "missing_count": 1}],
              str(reads[1]))

        # 7a. #1714's two deterministic value-matching failures. Each is
        # a periodic FULL-CAPACITY snapshot where every row of the new
        # observation is byte-identical to a row of the previous one, so
        # the old longest-prefix-subsequence heuristic reported [] (first
        # case, both new rows lost) or only the last row (second case,
        # the new "A" lost). Sequence progress reports both, and names
        # the evicted sequences as a gap.
        a1, b1, a2, b2 = (row(1, "A"), row(2, "B"), row(3, "A"), row(4, "B"))
        a3, b3 = row(5, "A"), row(6, "B")
        reads = observe([[a1, b1, a2, b2], [a2, b2, a3, b3]])
        check("full-capacity snapshot of byte-identical rows reports both "
              "new rows (the [A,B,A,B] -> [A,B,A,B] loss)",
              reads[1]["event_log_new"] == [a3, b3]
              and reads[1]["event_log_gaps"] == [], str(reads[1]))
        a4, b4, a5, a6 = (row(1, "A"), row(2, "B"), row(3, "A"), row(4, "A"))
        a7, b5 = row(5, "A"), row(6, "B")
        reads = observe([[a4, b4, a5, a6], [a5, a6, a7, b5]])
        check("multi-row repeated rollover reports every new row "
              "(the [A,B,A,A] -> [A,A,A,B] loss)",
              reads[1]["event_log_new"] == [a7, b5]
              and reads[1]["event_log_gaps"] == [], str(reads[1]))

        # 7b. More than `eventStoreCap` mutations between two
        # observations. The ring cannot hold them, so the exactly-once
        # guarantee is bounded to what is still represented -- and every
        # unrepresented committed sequence is reported as an explicit
        # maximal-interval gap rather than silently vanishing.
        cap = 1000
        first = [row(n, f"r{n}") for n in (1, 2)]
        overflowed = [row(n, f"r{n}")
                      for n in range(1_500, 1_500 + cap)]
        reads = observe([first, overflowed])
        check("more than eventStoreCap mutations between observations "
              "reports every retained row exactly once",
              reads[1]["event_log_new"] == overflowed, str(len(overflowed)))
        check("more than eventStoreCap mutations between observations "
              "reports the unrepresented ones as one maximal gap",
              reads[1]["event_log_gaps"] == [
                  {"first_sequence": 3, "last_sequence": 1_499,
                   "missing_count": 1_497}],
              str(reads[1]["event_log_gaps"]))

        # 7c. Two disjoint losses in one interval must stay two
        # intervals, not be merged into one span that overstates the
        # damage between them.
        reads = observe([[row(1, "a")],
                         [row(3, "c"), row(4, "d"), row(7, "g")]])
        check("disjoint losses are reported as separate maximal intervals",
              reads[1]["event_log_gaps"] == [
                  {"first_sequence": 2, "last_sequence": 2, "missing_count": 1},
                  {"first_sequence": 5, "last_sequence": 6, "missing_count": 2}],
              str(reads[1]["event_log_gaps"]))

        # 7d. A load publish empties the ring WITHOUT resetting the
        # store's counter, so an emptied snapshot means one of two
        # different things and the high-water mark is the only thing
        # that tells them apart. Nothing committed since the last read:
        # nothing to report, and no invented gap.
        reads = observe([[row(1, "before"), row(2, "before two")],
                         ([], 2),
                         [row(3, "after")]])
        check("an emptied ring with nothing committed since the last read "
              "reports nothing and manufactures no gap",
              reads[1]["event_log_new"] == []
              and reads[1]["event_log_gaps"] == [], str(reads[1]))
        check("a row emitted after the ring was emptied is reported once, "
              "with no gap back to sequence 1",
              reads[2]["event_log_new"] == [row(3, "after")]
              and reads[2]["event_log_gaps"] == [], str(reads[2]))

        # ...and the case that makes the high-water mark necessary
        # (round-1 review): mutations 3-4 are committed between two
        # reads and then discarded by the load publish, leaving an EMPTY
        # ring. Inferring the ceiling from the surviving rows would find
        # none, report no change at all, and hide those two mutations
        # PERMANENTLY if no later row happened to arrive. The gap is
        # reported at the very next read, exactly once, and the read
        # after it — still empty, still high-water 4 — adds nothing.
        reads = observe([[row(1, "a"), row(2, "b")], ([], 4), ([], 4)])
        check("an emptied ring whose counter ran ahead reports the "
              "discarded mutations as a gap",
              reads[1]["event_log_new"] == []
              and reads[1]["event_log_gaps"] == [
                  {"first_sequence": 3, "last_sequence": 4,
                   "missing_count": 2}],
              str(reads[1]))
        check("that gap is reported exactly once, not on every later "
              "empty read",
              reads[2]["event_log_new"] == []
              and reads[2]["event_log_gaps"] == [], str(reads[2]))

        # A partial tail: rows survive, but the store committed further
        # than the newest of them. Truncating the gap at the last
        # surviving row would silently drop the tail.
        reads = observe([[row(1, "a")], ([row(2, "b")], 5)])
        check("a gap whose tail runs past the newest surviving row is "
              "reported to the high-water mark",
              reads[1]["event_log_new"] == [row(2, "b")]
              and reads[1]["event_log_gaps"] == [
                  {"first_sequence": 3, "last_sequence": 5,
                   "missing_count": 3}],
              str(reads[1]))

        # The same rule at the other end: a BASELINE claims nothing was
        # observed before it, so it reports no gap however far the
        # counter has already run — it simply adopts the high-water mark
        # and reports ordinary rows from there.
        reads = observe([[], [row(1, "first"), row(2, "second")]])
        check("an empty baseline reports no gap and lets the next read "
              "report ordinary rows",
              reads[0]["event_log_new"] == []
              and reads[0]["event_log_gaps"] == []
              and reads[1]["event_log_new"] == [row(1, "first"),
                                                row(2, "second")]
              and reads[1]["event_log_gaps"] == [], str(reads))
        reads = observe([([], 5), [row(6, "after")]])
        check("a baseline taken on an already-emptied ring adopts the "
              "high-water mark instead of reporting a pre-baseline gap",
              reads[0]["event_log_gaps"] == []
              and reads[1]["event_log_new"] == [row(6, "after")]
              and reads[1]["event_log_gaps"] == [], str(reads))

        # 7e. A row without a usable sequence is a LOUD failure. Falling
        # back to value matching here would quietly restore the very
        # behaviour #1714 removed, so the oracle refuses instead.
        from engine import OracleContractError
        for bad, label in ((({"text": "no sequence"},), "a missing sequence"),
                           (({"sequence": "3", "text": "stringy"},),
                            "a string sequence"),
                           (({"sequence": 0, "text": "zero"},),
                            "a non-positive sequence")):
            try:
                EventLogEngine([list(bad)]).oracle_events()
            except OracleContractError:
                raised = True
            else:
                raised = False
            check(f"an event-log row with {label} raises instead of "
                  "falling back to value matching", raised, str(bad))

        # The high-water mark gets the same treatment, and for the same
        # reason: it is the ONLY thing that distinguishes an emptied ring
        # from a store where nothing happened, so a missing or unusable
        # one must fail loudly rather than let a load publication's
        # discarded mutations read as "no change".
        for high_water, label in ((None, "a missing high-water mark"),
                                  ("3", "a string high-water mark"),
                                  (-1, "a negative high-water mark")):
            try:
                EventLogEngine([([row(1, "a")], high_water)]).oracle_events()
            except OracleContractError:
                raised = True
            else:
                raised = False
            check(f"an event-log read with {label} raises", raised,
                  str(high_water))
        try:
            EventLogEngine([("not a table", 1)]).oracle_events()
        except OracleContractError:
            raised = True
        else:
            raised = False
        check("an event-log read whose rows are not an array raises", raised)

        # The REPLY shape gets the same treatment (round-3 review). The
        # console is already known reachable -- `lua()` raises
        # EngineCrash otherwise -- so a reply that comes back and is not
        # a progress table means the API is missing or broken. Reading
        # any of these as "no events, no gaps" would leave the cursor
        # untouched and erase the turn's evidence silently, which is the
        # failure this whole change removes.
        for reply, label in ((None, "a nil reply (no such API)"),
                             ("error: attempt to call a nil value",
                              "a Lua error string"),
                             ({"highest": 2}, "a reply with no rows"),
                             ({"rows": []},
                              "a reply with no high-water mark")):
            try:
                EventLogEngine([reply]).oracle_events()
            except OracleContractError:
                raised = True
            else:
                raised = False
            check(f"an event-log progress read returning {label} raises "
                  "instead of reporting an empty observation", raised,
                  repr(reply))

        # An ARRAY reply cannot be scripted through the fixture (a list
        # entry IS its rows-list shorthand), so the unpacker takes it
        # directly.
        from engine import _event_log_reply
        try:
            _event_log_reply([])
        except OracleContractError:
            raised = True
        else:
            raised = False
        check("an event-log progress read returning a bare array raises",
              raised)

        # ...and a malformed reply must not quietly advance or reset the
        # cursor either: the read failed, so the next successful one
        # still reports everything since the last GOOD read.
        recovering = EventLogEngine([[row(1, "a")], None, [row(2, "b")]])
        first = recovering.oracle_events()
        try:
            recovering.oracle_events()
        except OracleContractError:
            pass
        after = recovering.oracle_events()
        check("a failed progress read leaves the cursor untouched, so the "
              "next good read still reports the row it missed",
              first["event_log_new"] == [row(1, "a")]
              and after["event_log_new"] == [row(2, "b")]
              and after["event_log_gaps"] == [], str(after))

        # 7f. A gap must SURVIVE the whole evidence path: both of a
        # turn's oracle reads (#775's pre-step and post-step drains) are
        # concatenated by `_merge_oracle`, written to the trace, and
        # surfaced to the critic as its own signal, its own friction
        # reason, and its own digest field. A loss the critic cannot see
        # is indistinguishable from an unchanged event log, which is the
        # whole defect.
        class GappyEngine(FakeEngine):
            """Reports one distinct gap on each of turn 1's two reads."""

            def __init__(self):
                super().__init__()
                self._calls = 0

            def oracle_events(self):
                self._calls += 1
                if self._calls == 1:
                    return {"event_log_new": [],
                            "event_log_gaps": [{"first_sequence": 11,
                                                "last_sequence": 14,
                                                "missing_count": 4}],
                            "action_outcomes": []}
                if self._calls == 2:
                    # A bad outcome alongside the loss: without the gap
                    # this turn would be reported as a SILENT failure
                    # ("no user-facing event"), which is an assertion
                    # the incomplete evidence cannot support.
                    return {"event_log_new": [],
                            "event_log_gaps": [{"first_sequence": 20,
                                                "last_sequence": 20,
                                                "missing_count": 1}],
                            "action_outcomes": [{"kind": "probe",
                                                 "outcome": "noop",
                                                 "reason": "nothing to do"}]}
                return {"event_log_new": [], "event_log_gaps": [],
                        "action_outcomes": []}

        gapdir = os.path.join(tmp, "gaps")
        gaptrace = SessionTrace(gapdir, {"mode": "selftest-gaps"})
        gapeng = GappyEngine()
        run_session(gapeng, agent_mod.ScriptedAgent(
            [{"do": "wait", "note": "nothing seemed to happen"}]), gaptrace,
            turns=1, dt=0.0, max_seconds=None, memory_turns=4,
            stuck_k=99, settle=0.0)
        gaptrace.finish("turn_budget_exhausted")
        gapturns = load_turns(gapdir)
        check("both oracle reads' gaps are merged onto the producing turn",
              gapturns[0]["oracle"]["event_log_gaps"] == [
                  {"first_sequence": 11, "last_sequence": 14,
                   "missing_count": 4},
                  {"first_sequence": 20, "last_sequence": 20,
                   "missing_count": 1}],
              str(gapturns[0]["oracle"].get("event_log_gaps")))

        from critic import build_signals, friction_candidates, build_digest
        gapsignals = build_signals(gapdir, gapturns)
        check("the critic reads event_log_gaps as its own signal",
              gapsignals[0]["event_log_gaps"] == [
                  {"first_sequence": 11, "last_sequence": 14,
                   "missing_count": 4},
                  {"first_sequence": 20, "last_sequence": 20,
                   "missing_count": 1}],
              str(gapsignals[0].get("event_log_gaps")))
        gapcands = friction_candidates({}, gapsignals)
        gapreasons = [r for c in gapcands for r in c["reasons"]]
        check("a gap with no surviving row raises its own friction reason "
              "naming the lost count",
              any(r.startswith("event-log-gap:") and "5 committed" in r
                  for r in gapreasons), str(gapreasons))
        # ...but a gap ALONGSIDE retained rows is ordinary coalescing
        # traffic, not friction: a burst of identical events supersedes
        # its own sequences every turn, and a candidate per burst would
        # bury the real losses.
        noisy = dict(gapsignals[0], events=[{"cat": "combat", "text": "hit"}])
        check("a gap alongside retained rows raises no standalone candidate",
              not any(r.startswith("event-log-gap:")
                      for c in friction_candidates({}, [noisy])
                      for r in c["reasons"]),
              str(friction_candidates({}, [noisy])))
        check("a gap downgrades the silent-failure claim to a judge-the-"
              "evidence one, carrying the incompleteness caution",
              not any(r.startswith("silent-failure-join:") for r in gapreasons)
              and any(r.startswith("bad-outcome-join:")
                      and "evidence here is incomplete" in r
                      for r in gapreasons),
              str(gapreasons))
        gapdigest = build_digest({}, gapsignals, gapcands)
        check("the digest the critic model reads carries the gaps",
              "event_log_gaps=" in gapdigest
              and '"first_sequence": 11' in gapdigest, gapdigest)

        # A trace recorded BEFORE #1714 carries no `event_log_gaps` key
        # at all; the critic must read that as "no gap was reported",
        # exactly as it already tolerates legacy `outcomes` and
        # pre-#775 `visual_change`.
        legacy_turn = dict(gapturns[0])
        legacy_turn["oracle"] = {k: v for k, v in gapturns[0]["oracle"].items()
                                 if k != "event_log_gaps"}
        legacy_signals = build_signals(gapdir, [legacy_turn])
        check("a pre-#1714 trace with no event_log_gaps key reads as no gaps",
              legacy_signals[0]["event_log_gaps"] == []
              and not any(r.startswith("event-log-gap:")
                          for c in friction_candidates({}, legacy_signals)
                          for r in c["reasons"]),
              str(legacy_signals[0].get("event_log_gaps")))

        # 8. #775: an event-log row and an F4 outcome that only become
        # readable once the sim step has genuinely run must land on the
        # turn whose action caused them, not the following turn. Planted
        # on turn 1's SECOND oracle_events() call — the post-step drain,
        # never the first (pre-step/settle) drain nor any later turn's.
        class StepEvidenceEngine(FakeEngine):
            def __init__(self):
                super().__init__()
                self._events_calls = 0

            def oracle_events(self):
                self._events_calls += 1
                if self._events_calls == 2:
                    return {"event_log_new": [{"cat": "world", "text": "step landed"}],
                            "action_outcomes": [{"kind": "probe", "outcome": "accepted"}]}
                return {"event_log_new": [], "action_outcomes": []}

        sedir = os.path.join(tmp, "step_evidence")
        setrace = SessionTrace(sedir, {"mode": "selftest-step-evidence"})
        seeng = StepEvidenceEngine()
        run_session(seeng, agent_mod.ScriptedAgent(
            [{"do": "wait"}, {"do": "wait"}]), setrace,
            turns=2, dt=0.0, max_seconds=None, memory_turns=4,
            stuck_k=99, settle=0.0)
        setrace.finish("turn_budget_exhausted")
        seturns = load_turns(sedir)
        check("event/outcome available only once the step ran lands on "
              "the producing turn, not the next",
              seturns[0]["oracle"]["event_log_new"]
              == [{"cat": "world", "text": "step landed"}]
              and seturns[0]["oracle"]["action_outcomes"]
              == [{"kind": "probe", "outcome": "accepted"}]
              and seturns[1]["oracle"]["event_log_new"] == []
              and seturns[1]["oracle"]["action_outcomes"] == [],
              str([t["oracle"] for t in seturns]))

        # 9. #775: a budget-limited final action must retain its OWN
        # post-step screenshot/oracle evidence — not lose it outright
        # for want of a "next turn" to (mis)capture it on. Writes
        # distinct bytes on exactly the 2nd screenshot call (turn 1's
        # post-step frame) so visual_change is checked against real
        # differing bytes, not FakeEngine's one constant PNG (always
        # equal to itself).
        class ChangingFrameEngine(FakeEngine):
            def __init__(self):
                super().__init__()
                self._shots = 0

            def screenshot(self, path, timeout=None):
                self._shots += 1
                data = self._PNG + (b"\x00" if self._shots == 2 else b"")
                with open(path, "wb") as f:
                    f.write(data)
                self.fb_size = (1280, 720)
                return self.fb_size

        fdir = os.path.join(tmp, "final_turn_evidence")
        ftrace = SessionTrace(fdir, {"mode": "selftest-final-turn-evidence"})
        feng = ChangingFrameEngine()
        freason = run_session(feng, agent_mod.ScriptedAgent(
            [{"do": "wait"}, {"do": "wait"}]), ftrace,
            turns=2, dt=0.0, max_seconds=None, memory_turns=4,
            stuck_k=99, settle=0.0)
        ftrace.finish(freason)
        fturns = load_turns(fdir)
        check("budget-limited final turn retains its own post-step "
              "screenshot and oracle evidence",
              freason == "turn_budget_exhausted"
              and fturns[-1]["oracle"].get("post_screenshot") is not None
              and os.path.isfile(os.path.join(
                  fdir, fturns[-1]["oracle"]["post_screenshot"])),
              str(fturns[-1]["oracle"]))
        check("visual_change is derived from THIS turn's own before/after "
              "frames, not a following turn that may not exist",
              fturns[0]["oracle"]["visual_change"] is True
              and fturns[-1]["oracle"]["visual_change"] is False,
              str([t["oracle"].get("visual_change") for t in fturns]))

        # replay gets the same fix: its own last turn must retain its
        # own post-step evidence too (reusing the scripted session's
        # trace already built in step 1/2 above).
        check("replay's own final turn also retains post-step evidence",
              load_turns(rdir)[-1]["oracle"].get("post_screenshot") is not None)

        # 9b. #1752: the post-step drain is DESTRUCTIVE, so a post frame
        # that raises must not cost the turn the evidence the runner
        # already consumed. Before the fix the merge happened only
        # after the screenshot, so a crash there persisted the pre-only
        # merge — while `_count_f4_outcomes` had already counted the
        # dropped outcomes into the session total, leaving the trace
        # internally inconsistent as well as lossy. The engine below
        # plants post-only evidence on turn 1's SECOND oracle_events()
        # call (the post-step drain) and then dies on turn 1's SECOND
        # screenshot() call (the post frame), in both the session and
        # the replay path.
        POST_FRAME_DEATH = "console died taking the post frame"
        POST_EVIDENCE = [{"cat": "world", "text": "step landed"}]
        POST_OUTCOMES = [{"kind": "probe", "outcome": "accepted"}]

        class CrashOnPostFrameEngine(FakeEngine):
            def __init__(self):
                super().__init__()
                self._events_calls = 0
                self._shots = 0

            def oracle_events(self):
                self._events_calls += 1
                if self._events_calls == 2:
                    return {"event_log_new": [{"cat": "world",
                                               "text": "step landed"}],
                            "event_log_gaps": [],
                            "action_outcomes": [{"kind": "probe",
                                                 "outcome": "accepted"}]}
                return {"event_log_new": [], "event_log_gaps": [],
                        "action_outcomes": []}

            def screenshot(self, path, timeout=None):
                self._shots += 1
                if self._shots == 2:
                    raise EngineCrash(POST_FRAME_DEATH)
                return super().screenshot(path)

        def check_retained_post_evidence(label, turns, meta, raised):
            """The one contract both paths hold (#1752): the drained
            evidence survives, the absent frame is stated as a null
            rather than as a turn that never stepped, the original
            crash is what propagated and ended the session, and the F4
            running total matches what the turn actually retained."""
            oracle = (turns[0].get("oracle") or {}) if turns else {}
            step_phase = turns[0].get("step_phase") if turns else None
            check(f"{label}: a failed post frame keeps the turn's drained "
                  "events and outcomes",
                  len(turns) == 1
                  and oracle.get("event_log_new") == POST_EVIDENCE
                  and oracle.get("action_outcomes") == POST_OUTCOMES,
                  str(oracle))
            check(f"{label}: the missing post frame is represented, and the "
                  "step still reads as completed",
                  oracle.get("post_screenshot") is None
                  and oracle.get("visual_change") is False
                  and step_phase == "completed",
                  f"{oracle}; step_phase {step_phase!r}")
            check(f"{label}: the original EngineCrash propagates and the "
                  "session ends as engine_crash",
                  isinstance(raised, EngineCrash)
                  and str(raised) == POST_FRAME_DEATH
                  and meta.get("stop_reason") == "engine_crash",
                  f"{raised!r}; {meta.get('stop_reason')!r}")
            check(f"{label}: the F4 running total equals the outcomes the "
                  "turn record actually retains",
                  meta.get("f4_outcomes_total")
                  == len(oracle.get("action_outcomes") or []),
                  f"{meta.get('f4_outcomes_total')} counted vs "
                  f"{len(oracle.get('action_outcomes') or [])} retained")

        pfdir = os.path.join(tmp, "crash_post_frame")
        pftrace = SessionTrace(pfdir, {"mode": "selftest-crash-post-frame"})
        pfraised = None
        try:
            run_session(CrashOnPostFrameEngine(), agent_mod.ScriptedAgent(
                [{"do": "wait"}, {"do": "wait"}]), pftrace,
                turns=2, dt=0.0, max_seconds=None, memory_turns=4,
                stuck_k=99, settle=0.0)
        except EngineCrash as e:
            pfraised = e
        pftrace.finish("engine_crash")
        check_retained_post_evidence("session", load_turns(pfdir),
                                     load_meta(pfdir), pfraised)

        # The same for replay, driven from the session trace just
        # recorded above: its one turn is "completed", so replay steps
        # and takes its own post frame — and dies on it the same way.
        rpfdir = os.path.join(tmp, "crash_post_frame_replay")
        rpftrace = SessionTrace(rpfdir,
                                {"mode": "selftest-crash-post-frame-replay"})
        rpfraised = None
        try:
            run_replay(CrashOnPostFrameEngine(), pfdir, rpftrace,
                       dt=0.0, settle=0.0)
        except EngineCrash as e:
            rpfraised = e
        rpftrace.finish("engine_crash")
        check_retained_post_evidence("replay", load_turns(rpfdir),
                                     load_meta(rpfdir), rpfraised)

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
            distinction `run_probes` draws, and asserting on `os.kill(pid,
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
