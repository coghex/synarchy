"""Deterministic pre-analysis for the playtest critic (#648,
extracted by #2069): everything derived from a trace WITHOUT a model.

Pure Python, LLM-free and unit-testable; the same inputs always yield
the same signals, candidates, digest and batches. It owns:

  * `build_signals` — per-turn derived facts: frame hashing and the
    visual-change derivation (#775's own-turn field, with the cross-turn
    fallback for older traces), action outcomes (canonical
    `action_outcomes` with the legacy `outcomes` fallback, #726),
    events and event-log gaps (#1714), acknowledgment errors, the
    correlated click (through `critic_click`), widgets, pause and menu
    state, and the stuck flag;
  * `friction_candidates` — the stable, ordered candidate list with the
    canonical joins pre-tagged (silent failure, bad outcome masked by
    unrelated feedback, phantom affordance, feedback shown but missed,
    stuck loop, engine-rejected input, player-reported friction) plus
    the crash candidate, including the turn-zero one;
  * `build_digest` — the session digest the model reads;
  * `plan_batches` — pre/post-frame selection and batch planning, with
    the two-frame floor and its warning, so every candidate's own
    frames are in the call that adjudicates it.

Consumes `critic_click` only; never model transport. Production callers
are `critic_pipeline.run_critic` and `preanalysis.py`; the runner's own
self-test components import from here for the critic-facing evidence
path.
"""
from __future__ import annotations

import hashlib
import json
import os
import sys

_HERE = os.path.dirname(os.path.abspath(__file__))
if _HERE not in sys.path:
    sys.path.insert(0, _HERE)

from critic_click import is_left_click, routing_aware_records, widget_at  # noqa: E402


def _file_hash(path: str) -> str | None:
    try:
        with open(path, "rb") as f:
            return hashlib.sha256(f.read()).hexdigest()
    except OSError:
        return None


BAD_OUTCOMES = ("rejected", "noop", "deadclick", "partial")


def build_signals(trace_dir: str, turns: list[dict]) -> list[dict]:
    """Per-turn derived facts, all deterministic."""
    hashes = [_file_hash(os.path.join(trace_dir, t.get("screenshot", "")))
              for t in turns]
    signals = []
    for i, t in enumerate(turns):
        player = t.get("player") or {}
        action = player.get("action") or {}
        oracle = t.get("oracle") or {}
        events = oracle.get("event_log_new") or []
        # #1714: committed event rows the oracle could NOT observe at
        # this turn's read — each entry a maximal missing sequence
        # interval. Optional on the READ side: a trace recorded before
        # #1714 carries no such key, and an absent key means "no gap
        # was reported", never "gaps unknown" (the same tolerance the
        # legacy `outcomes` and pre-#775 `visual_change` keys get).
        event_log_gaps = oracle.get("event_log_gaps") or []
        # F4 (#646) action outcomes. The live producer
        # (PlaytestEngine.oracle_events) writes them under
        # `action_outcomes`; only the pre-live canned fixture ever used
        # the legacy `outcomes` spelling. Treat `action_outcomes` as
        # authoritative whenever the key is present — even when it is an
        # intentionally empty list — and fall back to legacy `outcomes`
        # only when the canonical key is absent. This reads a live trace
        # correctly, keeps an empty canonical list empty, and makes a
        # dual-key trace yield exactly one (non-duplicated) record list.
        if "action_outcomes" in oracle:
            outcomes = oracle.get("action_outcomes") or []
        else:
            outcomes = oracle.get("outcomes") or []
        acks = t.get("acks") or []
        ack_errors = [a for a in acks
                      if isinstance(a, dict) and "error" in a]
        # This turn's own visible effect (#775): the runner now records
        # a before/after comparison scoped to THIS turn's own sim step
        # (oracle.visual_change) rather than smearing it across the
        # NEXT turn's frame — which misattributed a step's effect to
        # whatever action happened to come after it, and had nothing at
        # all to compare against on the final turn. A trace recorded
        # before #775 carries no `visual_change` key; fall back to the
        # old cross-turn frame comparison for those.
        if "visual_change" in oracle:
            changed = oracle.get("visual_change")
        else:
            changed = (hashes[i + 1] != hashes[i]) if i + 1 < len(turns) else None
        clicked = None
        if action.get("do") in ("click", "drag") and action.get("x") is not None:
            # #1750: correlate against the PRE-INJECTION record set when
            # the trace has one (`routing_widgets`), so a callback that
            # changed modal or element state during this very action
            # can't rewrite the context the click was routed against.
            # A trace recorded before #1750 has no such key and falls
            # back to `widgets`, exactly as before.
            if "routing_widgets" in oracle and oracle.get("routing_widgets") is not None:
                click_records = oracle.get("routing_widgets")
            else:
                click_records = oracle.get("widgets")
            # Route-aware only for a default/left click over a record
            # set carrying the routing facts; drag and non-left buttons
            # keep the legacy join (requirement 8 — this issue
            # establishes no router-parity contract for them).
            route_aware = (is_left_click(action)
                           and routing_aware_records(click_records))
            clicked = widget_at(click_records,
                                float(action.get("x", -1)),
                                float(action.get("y", -1)),
                                route_aware=route_aware)
        bad = [o for o in outcomes
               if isinstance(o, dict) and o.get("outcome") in BAD_OUTCOMES]
        widgets = oracle.get("widgets")
        signals.append({
            "turn": t.get("turn", i + 1),
            "observation": (player.get("observation") or "").strip(),
            "note": (player.get("note") or "").strip(),
            "expectation": (player.get("expectation") or "").strip(),
            "action": action,
            "injected": t.get("injected") or [],
            "acks": acks,
            "events": events,
            "event_log_gaps": event_log_gaps,
            "outcomes": outcomes,
            "bad_outcomes": bad,
            "ack_errors": ack_errors,
            "visual_change": changed,
            "clicked_widget": clicked,
            "widgets": widgets if isinstance(widgets, list) else [],
            "current_menu": oracle.get("current_menu"),
            "paused": oracle.get("paused"),
            "stuck": bool(t.get("stuck")),
        })
    return signals


def friction_candidates(meta: dict, signals: list[dict]) -> list[dict]:
    """Everything the critic must adjudicate, with the joins pre-tagged
    as machine hints (the model judges; these steer and ground it)."""
    cands = []

    def add(turn, reasons, oracle_excerpt, player_note="", player_words=""):
        cands.append({"cid": f"C{len(cands) + 1}", "turn": turn,
                      "player_note": player_note,
                      "player_words": player_words or player_note,
                      "reasons": reasons, "oracle": oracle_excerpt})

    for s in signals:
        reasons = []
        # What feedback signals existed this turn — CONTEXT for the
        # critic, never a gate. In a live unpaused world the frame
        # almost always changes and unrelated events fire, so gating a
        # bad outcome on "no feedback anywhere" would suppress nearly
        # every real silent failure (the false-negative class flagged
        # in review). The critic judges whether the feedback actually
        # correlated with the action.
        feedback_bits = []
        if s["events"]:
            feedback_bits.append(f"events fired: {json.dumps(s['events'])}")
        if s["visual_change"]:
            feedback_bits.append("the frame changed after the step")
        feedback = ("; ".join(feedback_bits)
                    if feedback_bits else "NO feedback of any kind "
                    "(no events, frame byte-identical after the step)")
        # #1714: the event evidence for this turn is INCOMPLETE — some
        # committed rows were evicted, or superseded by a coalesced
        # replacement, before the oracle could read them. Every claim
        # below that leans on "no events" has to be qualified, or a real
        # silent failure hides behind lost evidence.
        missing_events = sum(int(g.get("missing_count") or 0)
                             for g in s["event_log_gaps"]
                             if isinstance(g, dict))
        if s["event_log_gaps"]:
            feedback += (f" — CAUTION: {missing_events} committed event row(s) "
                         "were NOT observable at this turn's read "
                         f"({json.dumps(s['event_log_gaps'])}), so the event "
                         "evidence here is incomplete")
        # A gap becomes a candidate on its own ONLY when the turn shows
        # no retained rows either: that is the case the sequence exists
        # to expose, an empty `events` list that is a LOSS rather than a
        # quiet log. A gap alongside retained rows is ordinary
        # coalescing traffic — a burst of identical events supersedes
        # its own sequences every turn — so it qualifies the reasoning
        # (above, and in the digest) without manufacturing friction.
        if s["event_log_gaps"] and not s["events"]:
            reasons.append(
                f"event-log-gap: {missing_events} committed event row(s) were "
                "lost before the oracle could read them "
                f"({json.dumps(s['event_log_gaps'])}), and NO row survived to "
                "be reported — an empty event list on this turn is evidence "
                "loss, not evidence the log was unchanged")
        if s["bad_outcomes"]:
            kind = s["bad_outcomes"][0].get("outcome")
            # A gap disqualifies the silent-failure claim: "no
            # user-facing event" is an assertion about the event log,
            # and with rows missing the oracle cannot make it (#1714).
            # The turn still becomes a candidate — through the join that
            # SHOWS the incomplete evidence and asks the critic to judge
            # it, rather than the one that states an absence as fact.
            if not feedback_bits and not s["event_log_gaps"]:
                reasons.append(f"silent-failure-join: action outcome {kind!r} "
                               "with no user-facing event and no visible "
                               "frame change")
            else:
                reasons.append(f"bad-outcome-join: action outcome {kind!r} "
                               f"({s['bad_outcomes'][0].get('reason', 'no reason')}); "
                               f"{feedback} — judge whether that feedback "
                               "actually informed the player about THIS action "
                               "or was unrelated (a silent failure can hide "
                               "behind unrelated noise)")
        # Phantom-affordance requires the F4 deadclick contract (or,
        # in traces without outcome records, an explicitly fed-back
        # nothing + a player note). F3 enumerates UI widgets, not the
        # game world — an ordinary successful world click hits no
        # widget and must NOT become a candidate.
        deadclicked = any(isinstance(o, dict) and o.get("outcome") == "deadclick"
                          for o in s["outcomes"])
        if s["action"].get("do") == "click" and s["clicked_widget"] is None \
                and (deadclicked
                     or (not s["outcomes"] and not feedback_bits and s["note"])):
            reasons.append("phantom-affordance-join: the click hit no widget "
                           f"(per the F3 dump); {feedback}")
        if s["note"] and ("noth" in s["note"].lower()
                          or "broken" in s["note"].lower()
                          or "work" in s["note"].lower()) \
                and feedback_bits:
            reasons.append("feedback-was-shown-join: the player claims no "
                           f"effect, but the oracle shows: {feedback}")
        if s["stuck"]:
            reasons.append("stuck-loop: same action with no visible change, "
                           "repeatedly — strong missing-feedback signal")
        if s["ack_errors"]:
            reasons.append(f"input rejected by the engine: {s['ack_errors']}")
        if s["note"] and not reasons:
            reasons.append("player-reported friction (note)")
        if reasons:
            add(s["turn"], reasons, {
                "events": s["events"], "outcomes": s["outcomes"],
                "event_log_gaps": s["event_log_gaps"],
                "clicked_widget": s["clicked_widget"],
                "visual_change": s["visual_change"],
            }, player_note=s["note"],
                player_words=(s["note"] or s["observation"]
                              or s["expectation"]))

    if meta.get("stop_reason") == "engine_crash":
        crash_turn = meta.get("turns", 0) or 0
        s_last = next((s for s in signals if s["turn"] == crash_turn), None)
        add(crash_turn,
            ["engine crash ended the session (crash/blocker)"],
            {"stop_reason": "engine_crash",
             "crash_detail": meta.get("crash_detail"),
             "engine_log_tail": (meta.get("engine_log_tail") or "")[-1500:]},
            player_words=(s_last and (s_last["note"] or s_last["observation"]
                                      or s_last["expectation"]) or ""))
    return cands


def build_digest(meta: dict, signals: list[dict],
                 candidates: list[dict]) -> str:
    persona = meta.get("persona") or {}
    lines = [
        "SESSION",
        f"  persona: {persona.get('name')} — goal: {persona.get('goal')}",
        f"  stop_reason: {meta.get('stop_reason')}  turns: {meta.get('turns')}"
        f"  world_seed: {meta.get('world_seed')}",
        "",
        "TURNS (player fields are what the naive player wrote; oracle/"
        "signals are ground truth it never saw)",
    ]
    prev_widgets_key = None
    for s in signals:
        lines.append(f"turn {s['turn']}:")
        if s["observation"]:
            lines.append(f"  observation: {s['observation']}")
        lines.append(f"  action: {json.dumps(s['action'], sort_keys=True)}")
        if s["expectation"]:
            lines.append(f"  expectation: {s['expectation']}")
        if s["note"]:
            lines.append(f"  note: {s['note']}")
        if s["injected"]:
            lines.append(f"  injected: {json.dumps(s['injected'])}")
            lines.append(f"  acks: {json.dumps(s['acks'])}")
        # Full, LOSSLESS F3 widget dump — state-bearing fields (value,
        # focused, hovered, ...) included, so a toggle flipping value
        # is a real change. Deduped only when the serialized records
        # are byte-identical to the previous turn's.
        key = json.dumps(s["widgets"], sort_keys=True)
        if key == prev_widgets_key:
            lines.append("  widgets: (unchanged from previous turn)")
        else:
            lines.append(f"  widgets: {key}")
            prev_widgets_key = key
        # #1714: only rendered when non-empty. An ordinary turn's
        # oracle line is unchanged, so the absence anchors below
        # ("events=[]") keep their exact spelling; a lossy turn gets an
        # extra, impossible-to-miss field.
        gaps = (f"event_log_gaps={json.dumps(s['event_log_gaps'])} "
                if s["event_log_gaps"] else "")
        lines.append(f"  oracle: menu={s['current_menu']!r} paused={s['paused']} "
                     f"events={json.dumps(s['events'])} "
                     f"{gaps}"
                     f"outcomes={json.dumps(s['outcomes'])} "
                     f"clicked_widget={json.dumps(s['clicked_widget'])} "
                     f"visual_change={s['visual_change']} "
                     f"stuck={s['stuck']}")
    lines.append("")
    lines.append("FRICTION CANDIDATES (adjudicate every id; quote the "
                 "player's own words when a note exists)")
    for c in candidates:
        lines.append(f"{c['cid']} (turn {c['turn']}):")
        if c.get("player_note"):
            lines.append(f"  player_note: {c['player_note']}")
        elif c.get("player_words"):
            lines.append(f"  player_words: {c['player_words']}")
        for r in c["reasons"]:
            lines.append(f"  - {r}")
    if not candidates:
        lines.append("(none — a clean session; say so in the summary)")
    return "\n".join(lines)


def _frame_path(trace_dir: str, by_turn: dict, n: int) -> str | None:
    t = by_turn.get(n)
    if not t:
        return None
    p = os.path.join(trace_dir, t.get("screenshot", ""))
    return p if os.path.isfile(p) else None


def _post_frame_path(trace_dir: str, by_turn: dict, n: int) -> str | None:
    """Turn n's OWN post-step frame (#775) — the visible result of
    THIS turn's action, retained even on the final turn (which has no
    following turn to have ever borrowed one from). None for a turn
    whose sim step never ran (terminal, or a trace predating #775)."""
    t = by_turn.get(n)
    if not t:
        return None
    rel = (t.get("oracle") or {}).get("post_screenshot")
    if not rel:
        return None
    p = os.path.join(trace_dir, rel)
    return p if os.path.isfile(p) else None


def plan_batches(trace_dir: str, turns: list[dict], candidates: list[dict],
                 max_frames: int
                 ) -> tuple[list[tuple[list[dict], list[tuple[int, str]]]],
                            list[str]]:
    """Split the adjudication into calls such that EVERY candidate's
    own-turn screenshot is actually shown in the call that adjudicates
    it (the H2 multimodal-evidence requirement) — a warning about an
    unseen frame is not a substitute. Greedy: pack candidates into a
    batch while their frames (own pre-step turn, then that SAME turn's
    own post-step frame for the visible effect, #775 — never a
    following turn's, which the last candidate has none of) fit
    --max-frames; overflow starts a new call. A candidate-free session
    gets one call with the session bookends. A post-step frame is
    tracked under `-turn` in the returned frame lists — distinct from
    that turn's own pre-step `turn` key — so both can coexist and later
    be told apart when attaching screenshots to a finding. Returns
    (batches, warnings)."""
    by_turn = {t.get("turn"): t for t in turns}
    batches: list[tuple[list[dict], list[tuple[int, str]]]] = []
    cur: list[dict] = []
    cur_frames: list[tuple[int, str]] = []
    warnings: list[str] = []

    # A single candidate can need up to two frames of its own — its
    # pre-step screenshot and that SAME turn's post-step frame (#775).
    # A budget below that floor could never show both for even one
    # candidate no matter how batches are split, so clamp rather than
    # accept a configuration that provably can't satisfy the
    # per-candidate evidence contract (pr-review round 2). Combined
    # with the total-need flush check below, this GUARANTEES every
    # candidate's own+post pair lands together in some call — the
    # per-candidate warning further down is now unreachable, kept only
    # as a documented invariant, not a live code path.
    if max_frames < 2:
        warnings.append(f"--max-frames {max_frames} is below the 2-frame "
                        "floor a single candidate's own pre+post pair "
                        "needs; raised to 2")
        max_frames = 2

    def flush():
        nonlocal cur, cur_frames
        if cur:
            batches.append((cur, sorted(cur_frames)))
        cur, cur_frames = [], []

    for c in candidates:
        own = _frame_path(trace_dir, by_turn, c["turn"])
        eff = _post_frame_path(trace_dir, by_turn, c["turn"])
        own_needed = (1 if own and all(n != c["turn"] for n, _ in cur_frames)
                      else 0)
        eff_needed = (1 if eff and all(n != -c["turn"] for n, _ in cur_frames)
                      else 0)
        # Flush on the candidate's FULL need (own + its own post-step
        # frame, #775), not just the own frame — otherwise a candidate
        # whose own frame just barely fits a near-full batch loses its
        # post frame to the same starvation the own-frame flush already
        # guards against, silently and with no warning.
        if cur and len(cur_frames) + own_needed + eff_needed > max_frames:
            flush()
            own_needed = 1 if own else 0
            eff_needed = 1 if eff else 0
        if own and own_needed and len(cur_frames) < max_frames:
            cur_frames.append((c["turn"], own))
        elif own and own_needed:
            # unreachable now that max_frames is clamped to >= 2 above;
            # kept as a defensive invariant guard, not a live path
            warnings.append(f"frame budget too small: candidate {c['cid']}"
                            f" adjudicated WITHOUT its turn-{c['turn']} "
                            "screenshot")
        cur.append(c)
        if eff and eff_needed and len(cur_frames) < max_frames:
            cur_frames.append((-c["turn"], eff))
        elif eff and eff_needed:
            # unreachable now that max_frames is clamped to >= 2 above;
            # kept as a defensive invariant guard, not a live path
            warnings.append(f"frame budget too small: candidate {c['cid']}"
                            f" adjudicated WITHOUT its turn-{c['turn']} "
                            "post-step screenshot")
    flush()

    if not batches:
        frames: list[tuple[int, str]] = []
        for n in ([turns[0].get("turn"), turns[-1].get("turn")]
                  if turns else []):
            p = _frame_path(trace_dir, by_turn, n)
            if p and len(frames) < max_frames \
                    and all(m != n for m, _ in frames):
                frames.append((n, p))
        batches = [([], sorted(frames))]
    return batches, warnings
