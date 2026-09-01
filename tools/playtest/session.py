#!/usr/bin/env python3
"""Playtest session + replay engine — the lockstep loop itself (#2040).

Extracted verbatim from run.py so BOTH the CLI façade and the offline
self-test components can import the session/replay implementation
without importing (and re-executing) the command-line entry point as a
second module. run.py stays the stable documented command surface; this
module is the importable production behavior behind it, and importing it
parses no arguments and runs no tests.

The lockstep contract, the phase accounting, the oracle merge and the
replay semantics all live here unchanged — see each definition's own
docstring, and run.py's module docstring for the session-level picture.
"""
from __future__ import annotations

import hashlib
import json
import math
import os
import sys
import time

_HERE = os.path.dirname(os.path.abspath(__file__))
if _HERE not in sys.path:
    sys.path.insert(0, _HERE)

from engine import PlaytestEngine, translate_action  # noqa: E402
from trace import SessionTrace, load_meta, load_replay  # noqa: E402
from usage import compact_tokens, usage_total  # noqa: E402
import agent as agent_mod  # noqa: E402


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

