"""Synthetic session-trace fixture for the H2 critic (#648).

Builds a small, fully self-contained trace directory in the H1 format
with PLANTED issues, so the critic can be exercised (and judged)
offline against known ground truth:

  turn 1  — GENUINE SILENT FAILURE: the player clicks a real "Place
            Marker" button expecting success; the action-outcome
            record says rejected, no user-facing event fires, and the
            next frame is pixel-identical. The critic must flag this
            as a missing-feedback defect.
  turn 2  — the player repeats the click (stuck-ish follow-up noise).
  turn 3  — WORKING AS DESIGNED: the player clicks "Save Notes" and
            claims nothing happened — but the outcome is accepted, an
            event-log entry fired, and the frame visibly changed. The
            critic must adjudicate this intended (player missed the
            feedback; at most a minor discoverability note).
  turn 4  — PHANTOM AFFORDANCE: the player clicks empty space
            expecting an inventory; no widget is there, the outcome is
            a deadclick, nothing changes.
  turn 5  — quiet turn (no friction) — must NOT become a finding.
  turn 6  — SILENT FAILURE MASKED BY UNRELATED FEEDBACK: another
            rejected Place Marker click, but this time an unrelated
            survival warning fires in the same window and the frame
            changes (live-world motion). Naive "any feedback = fed
            back" gating would suppress this; the critic must still
            see the rejected outcome as a candidate and judge whether
            the feedback actually informed the player.
  turn 7  — NOTE-ONLY FRICTION: the player writes a lost/confused note
            while doing nothing; there are no outcome/event/widget
            records at all. The candidate's oracle anchors are the
            ABSENCE fragments (events=[], outcomes=[], ...) — a critic
            asserting invented oracle facts for it must be rejected.

The oracle snapshots carry F4-shaped `outcomes` records (the tap
itself, #646, hasn't landed in live traces yet — the critic reads them
when present, and this fixture proves that path).
"""
from __future__ import annotations

import json
import os

# Two tiny, distinct, valid PNGs (1x1 black / 1x1 white) so visual
# change/no-change between turns is real at the byte level and the
# frames stay sendable to a multimodal model.
PNG_BLACK = bytes.fromhex(
    "89504e470d0a1a0a0000000d49484452000000010000000108060000001f15c489"
    "0000000d49444154789c63606060f80f00010401005fe5c34b0000000049454e44ae426082")
PNG_WHITE = bytes.fromhex(
    "89504e470d0a1a0a0000000d49484452000000010000000108060000001f15c489"
    "0000000b49444154789c63f80f040009fb03fdfb5e6b2b0000000049454e44ae426082")

PLACE_MARKER = {
    "id": "button:place_marker", "name": "place_marker", "type": "button",
    "bounds": {"x": 100, "y": 100, "w": 200, "h": 60},
    "label": "Place Marker", "enabled": True, "visible": True,
}
SAVE_NOTES = {
    "id": "button:save_notes", "name": "save_notes", "type": "button",
    "bounds": {"x": 100, "y": 200, "w": 200, "h": 60},
    "label": "Save Notes", "enabled": True, "visible": True,
}
WIDGETS = [PLACE_MARKER, SAVE_NOTES]


def _oracle(events=None, outcomes=None):
    return {
        "player_invisible": True,
        "widgets": WIDGETS,
        "current_menu": "world_view",
        "paused": True,
        "event_log_new": events or [],
        "world_seed": 123,
        "outcomes": outcomes or [],
    }


def _turn(n, frame, player, injected, oracle, stuck=False):
    return {
        "turn": n, "ts": 1000.0 + n,
        "screenshot": f"frames/turn_{n:04d}.png",
        "fb_size": [1280, 720],
        "player": player, "injected": injected,
        "acks": [{"ok": True} for _ in injected],
        "post_injected": 0, "stepped": True,
        "oracle": oracle, "stuck": stuck,
        "_frame_bytes": frame,  # stripped before writing
    }


def build_canned_trace(trace_dir: str) -> str:
    """Write the fixture trace; returns trace_dir."""
    frames_dir = os.path.join(trace_dir, "frames")
    os.makedirs(frames_dir, exist_ok=True)

    turns = [
        _turn(1, PNG_BLACK,
              {"observation": "A screen with buttons: Place Marker and Save Notes.",
               "action": {"do": "click", "x": 200, "y": 130},
               "expectation": "A marker will be placed on the map.",
               "note": "Clicked Place Marker. Pretty sure that worked.",
               "raw": "", "usage": None},
              ['return input.click(200.0, 130.0)'],
              _oracle(outcomes=[{"verb": "marker.place", "outcome": "rejected",
                                 "reason": "insufficient materials"}])),
        _turn(2, PNG_BLACK,
              {"observation": "Same screen. I don't see a marker anywhere.",
               "action": {"do": "click", "x": 200, "y": 130},
               "expectation": "Maybe it needs a second click to confirm.",
               "note": "Did it work? Nothing looks different. Clicking again.",
               "raw": "", "usage": None},
              ['return input.click(200.0, 130.0)'],
              _oracle(outcomes=[{"verb": "marker.place", "outcome": "rejected",
                                 "reason": "insufficient materials"}])),
        _turn(3, PNG_BLACK,
              {"observation": "Trying the Save Notes button now.",
               "action": {"do": "click", "x": 200, "y": 230},
               "expectation": "My notes get saved and it tells me so.",
               "note": "Nothing happened when I clicked Save Notes. That seems broken.",
               "raw": "", "usage": None},
              ['return input.click(200.0, 230.0)'],
              _oracle(events=[{"cat": "ui", "text": "Notes saved"}],
                      outcomes=[{"verb": "notes.save", "outcome": "accepted"}])),
        _turn(4, PNG_WHITE,
              {"observation": "There is a bag-looking shape in the corner.",
               "action": {"do": "click", "x": 600, "y": 400},
               "expectation": "Opens my inventory.",
               "note": "I clicked what looked like a bag but nothing opened.",
               "raw": "", "usage": None},
              ['return input.click(600.0, 400.0)'],
              _oracle(outcomes=[{"verb": "click", "outcome": "deadclick"}])),
        _turn(5, PNG_WHITE,
              {"observation": "Just watching the screen for a moment.",
               "action": {"do": "wait"},
               "expectation": "Time passes.",
               "note": "",
               "raw": "", "usage": None},
              [],
              _oracle()),
        _turn(6, PNG_BLACK,
              {"observation": "One of the little people is walking around.",
               "action": {"do": "click", "x": 200, "y": 130},
               "expectation": "Placing a marker now that things settled.",
               "note": "",
               "raw": "", "usage": None},
              ['return input.click(200.0, 130.0)'],
              _oracle(events=[{"cat": "survival_warning",
                               "text": "Acolyte Renn is thirsty"}],
                      outcomes=[{"verb": "marker.place", "outcome": "rejected",
                                 "reason": "insufficient materials"}])),
        _turn(7, PNG_BLACK,
              {"observation": "Same screen as before, as far as I can tell.",
               "action": {"do": "wait"},
               "expectation": "Maybe something will happen on its own.",
               "note": "I feel a bit lost. Nothing on this screen tells me "
                       "what I should do next.",
               "raw": "", "usage": None},
              [],
              _oracle()),
    ]

    with open(os.path.join(trace_dir, "turns.jsonl"), "w") as f:
        for t in turns:
            frame = t.pop("_frame_bytes")
            with open(os.path.join(trace_dir, t["screenshot"]), "wb") as pf:
                pf.write(frame)
            f.write(json.dumps(t, sort_keys=True) + "\n")

    with open(os.path.join(trace_dir, "replay.jsonl"), "w") as f:
        for t in turns:
            f.write(json.dumps({"turn": t["turn"], "pre": t["injected"],
                                "post": [], "stepped": True}) + "\n")

    meta = {
        "harness_version": "fixture",
        "mode": "llm",
        "dt": 2.0,
        "turn_budget": 7,
        "turns": 7,
        "stop_reason": "turn_budget_exhausted",
        "persona": {
            "name": "canned_casey",
            "temperament": "A curious first-time player.",
            "goal": "Place a marker on the map and save your notes.",
            "tendencies": ["clicks first, reads later"],
        },
        "player_model": {"model": "fixture", "effort": "low"},
        "world_seed": 123,
        "started_at": 1000.0, "ended_at": 1010.0,
        "f4_outcomes": "synthetic outcome records planted in oracle.outcomes",
    }
    with open(os.path.join(trace_dir, "meta.json"), "w") as f:
        json.dump(meta, f, indent=2, sort_keys=True)
        f.write("\n")
    return trace_dir
