"""Session trace format for the playtest harness (#647).

One directory per session — the durable, replayable artifact H2
consumes. H1 records; it never analyzes.

    <trace_dir>/
    ├── meta.json        session metadata (persona, model, dt, versions,
    │                    timestamps, stop_reason, fb size, world_seed, ...)
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
    ├── frames/          turn_0001.png ... (F1 captures)
    └── engine.log       engine stdout/stderr copied at session end

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
                    turn was interrupted before the snapshot completed
    stuck           bool  (this turn tripped the stuck-loop detector)
"""
from __future__ import annotations

import json
import os
import shutil
import time

STEP_PHASES = ("not_started", "interrupted", "completed")


class SessionTrace:
    def __init__(self, trace_dir: str, meta: dict):
        self.dir = trace_dir
        os.makedirs(os.path.join(trace_dir, "frames"), exist_ok=True)
        self.meta = dict(meta)
        self.meta.setdefault("started_at", time.time())
        self.turns = 0
        self._write_meta()

    def _write_meta(self) -> None:
        with open(os.path.join(self.dir, "meta.json"), "w") as f:
            json.dump(self.meta, f, indent=2, sort_keys=True)
            f.write("\n")

    def frame_path(self, turn: int) -> str:
        return os.path.join(self.dir, "frames", f"turn_{turn:04d}.png")

    def record_turn(self, record: dict) -> None:
        self.turns += 1
        with open(os.path.join(self.dir, "turns.jsonl"), "a") as f:
            f.write(json.dumps(record, sort_keys=True) + "\n")

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
