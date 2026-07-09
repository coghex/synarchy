"""Session trace format for the playtest harness (#647).

One directory per session — the durable, replayable artifact H2
consumes. H1 records; it never analyzes.

    <trace_dir>/
    ├── meta.json        session metadata (persona, model, dt, versions,
    │                    timestamps, stop_reason, fb size, ...)
    ├── turns.jsonl      one JSON object per turn (schema below)
    ├── replay.jsonl     flat ordered inputs: {"turn": N, "lua": "..."}
    ├── frames/          turn_0001.png ... (F1 captures)
    └── engine.log       engine stdout/stderr copied at session end

Per-turn record (turns.jsonl):
    turn            int
    ts              float  (unix epoch at capture)
    screenshot      str    (path relative to the trace dir)
    player          {observation, action, expectation, note, raw, usage}
    injected        [lua strings actually sent]  (incl. post-step keyUp)
    acks            [per-call ack replies]
    oracle          {..., "player_invisible": true}  — captured for the
                    critic, NEVER shown to the player
    stuck           bool  (this turn tripped the stuck-loop detector)
"""
from __future__ import annotations

import json
import os
import shutil
import time


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

    def record_replay(self, turn: int, lua_calls: list[str]) -> None:
        if not lua_calls:
            return
        with open(os.path.join(self.dir, "replay.jsonl"), "a") as f:
            for call in lua_calls:
                f.write(json.dumps({"turn": turn, "lua": call}) + "\n")

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


def load_replay(trace_dir: str) -> list[tuple[int, str]]:
    """The ordered injected inputs of a recorded session."""
    path = os.path.join(trace_dir, "replay.jsonl")
    entries: list[tuple[int, str]] = []
    if not os.path.isfile(path):
        return entries
    with open(path) as f:
        for line in f:
            line = line.strip()
            if line:
                obj = json.loads(line)
                entries.append((int(obj["turn"]), str(obj["lua"])))
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
