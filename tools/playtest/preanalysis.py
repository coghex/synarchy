#!/usr/bin/env python3
"""Write a deterministic, LLM-free screenshot inspection plan for one trace."""
from __future__ import annotations

import argparse
import json
import os

from critic_signals import build_signals, friction_candidates
from trace import load_meta, load_turns


def write_inspection_plan(trace_dir: str) -> str:
    meta = load_meta(trace_dir)
    turns = load_turns(trace_dir)
    signals = build_signals(trace_dir, turns)
    candidates = friction_candidates(meta, signals)
    reasons: dict[int, list[str]] = {}

    def add(turn, reason):
        if turn:
            reasons.setdefault(int(turn), [])
            if reason not in reasons[int(turn)]:
                reasons[int(turn)].append(reason)

    if turns:
        add(turns[0].get("turn"), "session bookend: first frame")
        add(turns[-1].get("turn"), "session bookend: last frame")
    for candidate in candidates:
        for reason in candidate.get("reasons") or []:
            add(candidate.get("turn"), reason)
    by_turn = {int(t.get("turn")): t for t in turns if t.get("turn") is not None}
    entries = []
    for turn in sorted(reasons):
        record = by_turn.get(turn, {})
        oracle = record.get("oracle") or {}
        entries.append({
            "turn": turn,
            "reasons": reasons[turn],
            "pre_screenshot": record.get("screenshot"),
            "post_screenshot": oracle.get("post_screenshot"),
        })
    output = {
        "schema": "synarchy-playtest-inspection/v1",
        "candidate_count": len(candidates),
        "inspection_turns": entries,
        "note": ("This plan selects evidence; it does not judge whether an "
                 "observation is a defect. Inspect the listed images directly."),
    }
    path = os.path.join(trace_dir, "inspection-plan.json")
    with open(path, "w", encoding="utf-8") as f:
        json.dump(output, f, indent=2, sort_keys=True)
        f.write("\n")
    return path


def main() -> int:
    ap = argparse.ArgumentParser(description=__doc__)
    ap.add_argument("trace_dir")
    args = ap.parse_args()
    path = write_inspection_plan(args.trace_dir)
    print(f"playtest preanalysis: wrote {path}")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
