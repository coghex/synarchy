"""Session trace format for the playtest harness (#647).

One directory per session — the durable, replayable artifact H2
consumes. H1 records; it never analyzes.

    <trace_dir>/
    ├── meta.json        session metadata (persona, model, dt, versions,
    │                    lifecycle timestamps, stop_reason, fb size,
    │                    world_seed, ...)
    ├── turns.jsonl      one JSON object per turn (schema below)
    ├── replay.jsonl     one line PER TURN (no-input turns included, so
    │                    replay pacing is faithful):
    │                    {"turn": N, "pre": [lua...], "post": [lua...],
    │                     "step_phase": "not_started"|"interrupted"|"completed"}
    │                    pre = injected before the sim step; post = after
    │                    it (a held key's keyUp rides post). Only calls
    │                    that actually ran are recorded. step_phase
    │                    distinguishes a step that never began
    │                    (done/stuck, or interrupted before a successful
    │                    unpause) from one that began but didn't finish
    │                    cleanly (interrupted during pacing or repause)
    │                    from one that fully completed (#728, superseding
    │                    #698's boolean "stepped"). Replay performs the
    │                    unpause->dt->repause step for both "interrupted"
    │                    and "completed" source turns — only
    │                    "not_started" skips it — but only replays the
    │                    post calls for a "completed" turn, since a
    │                    began-but-interrupted turn's post calls never
    │                    ran live either. Legacy traces (a missing or
    │                    boolean "stepped" field) load with their
    │                    historical mapping: missing/True -> "completed",
    │                    False -> "not_started" (its true start state is
    │                    unrecoverable, so it conservatively keeps the
    │                    old no-step replay behavior)
    ├── frames/          turn_0001.png ... (F1 captures) and, for every
    │                    turn whose sim step actually ran,
    │                    turn_0001_post.png ... (#775: the frame right
    │                    after that turn's own unpause-dt-repause step,
    │                    so even the last, budget-limited turn has its
    │                    own retained visible-result evidence — never
    │                    borrowed from a following turn that might not
    │                    exist)
    ├── setup.log        build/preparation output, written live during
    │                    the pre-ready `build` phase (#1539). Unlike the
    │                    engine log this survives a failure BEFORE the
    │                    executable ever starts, which used to leave the
    │                    trace with nothing but an unexplained zero-byte
    │                    engine.log
    ├── setup_ready.png  the player-ready probe's screenshot (#1539) —
    │                    a SETUP artifact, deliberately not under
    │                    frames/ and never numbered as a turn: it is
    │                    never a turn observation, never shown to the
    │                    player, and produces no turns.jsonl or
    │                    replay.jsonl record
    └── engine.log       engine stdout/stderr copied at session end

Session lifecycle timestamps in meta.json (#1539) — all unix epoch
floats from `time.time()`, so they are directly comparable with each
other and with a budget expressed in seconds. Budget ENFORCEMENT is a
separate concern and stays on a monotonic clock:

    setup_started_at    the trace was created and setup began: build,
                        engine launch, UI loading. Always present.
    loaded_at           the PLAYER-READY boundary — the engine and its
                        debug console are reachable, the menu surface is
                        initialized, and a screenshot that could really
                        be handed to a player has succeeded. Null until
                        crossed, so it stays null on a setup failure.
    session_started_at  the player-session loop began. Never before
                        loaded_at; null if no session ever ran.
    ended_at            the session finished (`finish`).
    started_at          RETAINED, with its original meaning unchanged:
                        the moment the trace directory was created,
                        which is the start of SETUP. `setup_started_at`
                        is its explicit new name; both are written so
                        that readers of older traces (usage.py's ledger
                        timestamp among them) keep working untouched.

Setup duration is `loaded_at - setup_started_at` and the actual player
session is `ended_at - session_started_at`, each derivable without the
other. A trace written before #1539 carries only `started_at`/`ended_at`;
every reader here treats the new fields as optional.

Per-turn record (turns.jsonl):
    turn            int
    ts              float  (unix epoch at capture)
    screenshot      str    (path relative to the trace dir)
    player          {observation, action, expectation, note, raw, usage}
    injected        [lua strings actually sent] — ACKNOWLEDGED pre-step
                    calls followed by acknowledged post-step calls
                    only; a call that never ran is never listed, and a
                    multi-call action interrupted mid-way keeps its
                    successful prefix (#698)
    acks            [per-call ack replies]  (one per injected entry,
                    post-step acks included)
    post_injected   int   (how many trailing injected entries were
                    post-step calls; 0 on a turn whose post phase
                    never ran)
    step_phase      "not_started"|"interrupted"|"completed" — whether
                    the unpause-dt-repause sim step never began, began
                    but was interrupted before repause confirmed, or
                    fully completed (#728)
    oracle          {..., "player_invisible": true}  — captured for the
                    critic, NEVER shown to the player; null when the
                    turn was interrupted before the snapshot completed.
                    `widgets`/`current_menu`/`paused`/`world_seed` are
                    the pre-step affordance context (the state the
                    player actually acted on, captured once after
                    inject+settle).
                    `event_log_new`/`event_log_gaps`/`action_outcomes`
                    are the UNION of two drains — one taken at that
                    same pre-step point (whatever the action produced
                    synchronously while still paused) and, when the
                    turn's sim step actually ran, a second taken right
                    after it (whatever the unpaused `dt` interval
                    itself produced) — both credited to THIS turn's
                    action, never deferred to the next turn (#775).
                    `event_log_gaps` (#1714) names the committed event
                    rows each read could NOT see, as maximal missing
                    `sequence` intervals
                    (`{first_sequence, last_sequence, missing_count}`);
                    it asserts absence only and never a cause, and is
                    absent entirely from traces recorded before #1714.
                    `visual_change` (bool) and `post_screenshot` (path,
                    relative to the trace dir, or null) are this turn's
                    OWN before/after comparison and post-step frame;
                    both are only populated when a step ran (never for
                    a `done`/stuck terminal turn, which has no step to
                    produce them)
    stuck           bool  (this turn tripped the stuck-loop detector)
"""
from __future__ import annotations

import json
import os
import shutil
import time

from usage import usage_total

STEP_PHASES = ("not_started", "interrupted", "completed")


class SessionTrace:
    def __init__(self, trace_dir: str, meta: dict):
        self.dir = trace_dir
        os.makedirs(os.path.join(trace_dir, "frames"), exist_ok=True)
        self.meta = dict(meta)
        # `started_at` keeps its original meaning — trace creation, which
        # is where SETUP begins — and `setup_started_at` is that same
        # instant under its explicit new name (#1539). The two
        # boundary stamps below are present-but-null until they are
        # actually crossed, so a setup failure leaves them null rather
        # than implying a session that never ran.
        self.meta.setdefault("started_at", time.time())
        self.meta.setdefault("setup_started_at", self.meta["started_at"])
        self.meta.setdefault("loaded_at", None)
        self.meta.setdefault("session_started_at", None)
        self.turns = 0
        self._write_meta()

    def _write_meta(self) -> None:
        with open(os.path.join(self.dir, "meta.json"), "w") as f:
            json.dump(self.meta, f, indent=2, sort_keys=True)
            f.write("\n")

    def setup_log_path(self) -> str:
        """Where the pre-ready `build` phase writes its output (#1539).

        Inside the trace from the start, so a build failure — which
        happens before the executable exists and therefore before any
        engine log has a single byte in it — still leaves its real
        cause behind."""
        return os.path.join(self.dir, "setup.log")

    def setup_frame_path(self) -> str:
        """The player-ready probe's screenshot (#1539).

        A SETUP artifact: NOT `frames/turn_0001.png`, so it cannot
        disturb turn numbering or replay pacing, and it is never
        recorded as a turn or shown to the player."""
        return os.path.join(self.dir, "setup_ready.png")

    def mark_loaded(self, when: float | None = None) -> None:
        """Stamp the PLAYER-READY boundary (#1539).

        Called once, by the launcher, the moment the game can actually
        hand a player its first frame. Everything before it is setup;
        every player-session budget starts after it."""
        self.meta["loaded_at"] = time.time() if when is None else when
        self._write_meta()

    def mark_session_started(self, when: float | None = None) -> None:
        """Stamp the start of the player-session loop (#1539).

        Written by the session/replay loops themselves, so no player
        call can precede it. In a complete run it is at or after
        `loaded_at`."""
        self.meta["session_started_at"] = time.time() if when is None else when
        self._write_meta()

    def frame_path(self, turn: int) -> str:
        return os.path.join(self.dir, "frames", f"turn_{turn:04d}.png")

    def post_frame_path(self, turn: int) -> str:
        """The frame captured right after turn N's own sim step (#775)
        — this turn's OWN visible-result evidence, never the next
        turn's pre-step frame."""
        return os.path.join(self.dir, "frames", f"turn_{turn:04d}_post.png")

    def record_turn(self, record: dict) -> None:
        self.turns += 1
        with open(os.path.join(self.dir, "turns.jsonl"), "a") as f:
            f.write(json.dumps(record, sort_keys=True) + "\n")

    def record_usage(self, usage: dict | None) -> bool:
        """Persist normalized player usage immediately after a decision.

        Returns False when the provider supplied no usable usage payload. This
        lets the runner stop safely instead of continuing without accounting.
        """
        total = usage_total(usage)
        if total is None:
            return False
        totals = self.meta.setdefault("usage_totals", {
            "input_tokens": 0,
            "output_tokens": 0,
            "total_tokens": 0,
            "turns_with_usage": 0,
        })
        totals["input_tokens"] += max(0, int(usage.get("input_tokens") or 0))
        totals["output_tokens"] += max(0, int(usage.get("output_tokens") or 0))
        totals["total_tokens"] = (totals["input_tokens"]
                                  + totals["output_tokens"])
        totals["turns_with_usage"] += 1
        if usage.get("account_remaining_tokens") is not None:
            totals["account_remaining_tokens"] = max(
                0, int(usage["account_remaining_tokens"]))
        self._write_meta()
        return True

    def record_replay(self, turn: int, pre: list[str], post: list[str],
                      step_phase: str) -> None:
        """One line per turn — ALWAYS, even with no calls, so replay
        reproduces the turn count and pacing of the session (a run of
        'wait' turns is real elapsed game time, not dead trace). pre and
        post must contain only calls that actually ran, and step_phase
        one of "not_started"/"interrupted"/"completed" — replay executes
        exactly the phases that state implies, nothing more (#698, #728)."""
        if step_phase not in STEP_PHASES:
            raise ValueError(f"unknown step_phase {step_phase!r}")
        with open(os.path.join(self.dir, "replay.jsonl"), "a") as f:
            f.write(json.dumps({"turn": turn, "pre": pre, "post": post,
                                "step_phase": step_phase}) + "\n")

    def attach_engine_log(self, log_path: str) -> None:
        try:
            shutil.copyfile(log_path, os.path.join(self.dir, "engine.log"))
        except OSError:
            pass

    def finish(self, stop_reason: str, **extra) -> None:
        self.meta.update(extra)
        self.meta["stop_reason"] = stop_reason
        self.meta["ended_at"] = time.time()
        self.meta["turns"] = self.turns
        self._write_meta()


def load_meta(trace_dir: str) -> dict:
    with open(os.path.join(trace_dir, "meta.json")) as f:
        return json.load(f)


def load_replay(trace_dir: str) -> list[dict]:
    """The recorded session's turns, in order: each entry is
    {"turn": N, "pre": [lua...], "post": [lua...], "step_phase":
    "not_started"|"interrupted"|"completed"} — no-input turns included.
    A pre-#728 entry carries a boolean (or missing) "stepped" field
    instead: missing or True maps to "completed" (those traces only
    ever recorded a step on every turn); False maps to "not_started" —
    its actual start state (never began vs. began-but-interrupted) is
    unrecoverable, so it conservatively keeps its historical no-step
    replay behavior."""
    path = os.path.join(trace_dir, "replay.jsonl")
    entries: list[dict] = []
    if not os.path.isfile(path):
        return entries
    with open(path) as f:
        for line in f:
            line = line.strip()
            if line:
                obj = json.loads(line)
                phase = obj.get("step_phase")
                if phase not in STEP_PHASES:
                    phase = "completed" if bool(obj.get("stepped", True)) else "not_started"
                entries.append({"turn": int(obj["turn"]),
                                "pre": list(obj.get("pre") or []),
                                "post": list(obj.get("post") or []),
                                "step_phase": phase})
    entries.sort(key=lambda e: e["turn"])
    return entries


def load_turns(trace_dir: str) -> list[dict]:
    path = os.path.join(trace_dir, "turns.jsonl")
    out = []
    if os.path.isfile(path):
        with open(path) as f:
            for line in f:
                if line.strip():
                    out.append(json.loads(line))
    return out
