#!/usr/bin/env python3
"""World generation determinism checker.

Runs the synarchy --dump command N times for the same seed and verifies
that the output is byte-identical across runs. If any run differs, reports
which tiles differ and how.

Used to detect race conditions in chunk generation. A passing check is a
necessary precondition for the pure pipeline refactor.
"""

from __future__ import annotations

import argparse
import hashlib
import json
import subprocess
import sys
from collections import Counter
from pathlib import Path
from typing import Any


# ----- Dump runner ---------------------------------------------------------

REPO_ROOT = Path(__file__).resolve().parent.parent


def run_dump(seed: int, world_size: int,
             region: tuple[int, int, int, int]) -> list[dict[str, Any]]:
    cx1, cy1, cx2, cy2 = region
    cmd = [
        "cabal", "run", "exe:synarchy", "--",
        "--dump",
        "--seed", str(seed),
        "--worldSize", str(world_size),
        "--region", f"{cx1},{cy1},{cx2},{cy2}",
    ]
    result = subprocess.run(
        cmd, capture_output=True, text=True, check=False, cwd=str(REPO_ROOT),
    )
    if result.returncode != 0:
        raise RuntimeError(
            f"dump command failed (exit {result.returncode}):\n"
            f"{result.stderr[-2000:]}"
        )
    raw = result.stdout
    start = raw.find("[{")
    if start < 0:
        raise RuntimeError(
            f"no JSON array found in dump output ({len(raw)} bytes of stdout)"
        )
    return json.loads(raw[start:])


# ----- Canonical hashing ---------------------------------------------------

def canonical_dump(data: list[dict[str, Any]]) -> bytes:
    """Sort tiles by (x, y) and serialize with sorted keys for stable hashing."""
    sorted_tiles = sorted(data, key=lambda t: (t["x"], t["y"]))
    return json.dumps(sorted_tiles, sort_keys=True, separators=(",", ":")).encode("utf-8")


def hash_dump(data: list[dict[str, Any]]) -> str:
    return hashlib.sha256(canonical_dump(data)).hexdigest()


# ----- Diffing -------------------------------------------------------------

def diff_dumps(a: list[dict[str, Any]],
               b: list[dict[str, Any]]) -> list[tuple[tuple[int, int], dict, dict]]:
    """Return a list of (coord, tile_a, tile_b) for tiles that differ."""
    grid_a = {(t["x"], t["y"]): t for t in a}
    grid_b = {(t["x"], t["y"]): t for t in b}

    diffs = []
    all_coords = set(grid_a.keys()) | set(grid_b.keys())
    for coord in sorted(all_coords):
        ta = grid_a.get(coord)
        tb = grid_b.get(coord)
        if ta != tb:
            diffs.append((coord, ta, tb))
    return diffs


def field_diff_summary(
    diffs: list[tuple[tuple[int, int], dict, dict]],
) -> dict[str, int]:
    """Count which fields differ across all differing tiles."""
    field_counts: Counter[str] = Counter()
    for _, ta, tb in diffs:
        if ta is None or tb is None:
            field_counts["TILE_MISSING"] += 1
            continue
        for key in set(ta.keys()) | set(tb.keys()):
            if ta.get(key) != tb.get(key):
                field_counts[key] += 1
    return dict(sorted(field_counts.items()))


# ----- Main ----------------------------------------------------------------

def parse_region(s: str) -> tuple[int, int, int, int]:
    parts = s.split(",")
    if len(parts) != 4:
        raise argparse.ArgumentTypeError(
            f"region must be cx1,cy1,cx2,cy2 (got {s!r})"
        )
    try:
        return (int(parts[0]), int(parts[1]), int(parts[2]), int(parts[3]))
    except ValueError as e:
        raise argparse.ArgumentTypeError(f"region values must be ints: {e}")


def main() -> int:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--seed", type=int, default=42,
                        help="World seed (default: 42)")
    parser.add_argument("--worldSize", type=int, default=32,
                        help="World size (default: 32)")
    parser.add_argument("--region", type=parse_region,
                        default=(-4, -4, 4, 4),
                        help="Chunk region cx1,cy1,cx2,cy2 (default: -4,-4,4,4)")
    parser.add_argument("--runs", type=int, default=3,
                        help="Number of dump runs to compare (default: 3)")
    parser.add_argument("--verbose", "-v", action="store_true",
                        help="Show details of differing tiles")
    parser.add_argument("--max-examples", type=int, default=10,
                        help="Max example differing tiles to show (default: 10)")
    args = parser.parse_args()

    if args.runs < 2:
        print("error: --runs must be at least 2", file=sys.stderr)
        return 2

    print(f"Running dump {args.runs} times: seed={args.seed} "
          f"worldSize={args.worldSize} region={args.region}", file=sys.stderr)

    try:
        dumps = []
        hashes = []
        for i in range(args.runs):
            data = run_dump(args.seed, args.worldSize, args.region)
            h = hash_dump(data)
            dumps.append(data)
            hashes.append(h)
            print(f"  run {i+1}/{args.runs}: {h[:16]}... ({len(data)} tiles)",
                  file=sys.stderr)
    except RuntimeError as exc:
        print(f"error: {exc}", file=sys.stderr)
        return 1

    unique_hashes = set(hashes)

    if len(unique_hashes) == 1:
        print(f"PASS: all {args.runs} runs produced identical output")
        print(f"  hash: {hashes[0]}")
        return 0

    print(f"FAIL: {len(unique_hashes)} distinct outputs across {args.runs} runs",
          file=sys.stderr)
    print(f"  hashes: {[h[:16] + '...' for h in hashes]}", file=sys.stderr)

    # Diff first two distinct dumps
    distinct_indices = []
    seen_hashes: set[str] = set()
    for i, h in enumerate(hashes):
        if h not in seen_hashes:
            distinct_indices.append(i)
            seen_hashes.add(h)
        if len(distinct_indices) >= 2:
            break

    a_idx, b_idx = distinct_indices[0], distinct_indices[1]
    diffs = diff_dumps(dumps[a_idx], dumps[b_idx])
    field_summary = field_diff_summary(diffs)

    print(f"  differing tiles: {len(diffs)}", file=sys.stderr)
    print(f"  field changes: {field_summary}", file=sys.stderr)

    if args.verbose:
        print(f"\nFirst {min(args.max_examples, len(diffs))} differing tiles "
              f"(run {a_idx+1} → run {b_idx+1}):", file=sys.stderr)
        for coord, ta, tb in diffs[:args.max_examples]:
            print(f"  {coord}:", file=sys.stderr)
            for key in sorted(set((ta or {}).keys()) | set((tb or {}).keys())):
                va = (ta or {}).get(key)
                vb = (tb or {}).get(key)
                if va != vb:
                    print(f"    {key}: {va!r} → {vb!r}", file=sys.stderr)

    return 1


if __name__ == "__main__":
    raise SystemExit(main())
