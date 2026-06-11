#!/usr/bin/env python3
"""Ore deposition report — quality/measurement tool (no bug gating).

Dumps worlds across seeds with the `ore` layer and reports, per seed
and per ore material:
  - coverage: tiles with any ore in the stored strata band
  - volume: total ore cells (tile.z)
  - depth: distribution of (terrainZ - oreTopZ), i.e. how deep you must
    dig from the surface to hit the top of the ore band
  - exposure: tiles whose surface material IS ore (erosion-cut outcrops)
  - deposits: connected clusters (4-adjacency) with size stats, so
    "large flat sheets" is measurable, not vibes

Usage:
  python3 tools/ore_report.py                      # default seeds, w64
  python3 tools/ore_report.py --seeds 4,7,42 --worldSize 64
  python3 tools/ore_report.py --files a.json b.json  # pre-dumped files

Calibration knobs this report informs: fluxPerMy / depositFrac /
size factors in src/World/Geology/Ore.hs, and the resources: levers in
config/world_gen_default.yaml.
"""

import argparse
import json
import subprocess
import sys
from collections import defaultdict, deque

DEFAULT_SEEDS = [4, 7, 42, 99, 123, 555, 777, 1337]

ORE_NAMES = {
    80: "iron_ore",
    84: "copper_ore",
}


def run_dump(seed, world_size, plates=None):
    """Dump the full world (terrain+material+ore layers) for one seed."""
    half = world_size // 2
    region = f"{-half},{-half},{half - 1},{half - 1}"
    cmd = [
        "cabal", "run", "-v0", "exe:synarchy", "--",
        "--dump=terrain,material,ore",
        "--seed", str(seed),
        "--worldSize", str(world_size),
        "--region", region,
    ]
    if plates is not None:
        cmd += ["--plates", str(plates)]
    print(f"  generating seed {seed} (w{world_size})...",
          file=sys.stderr, flush=True)
    out = subprocess.run(cmd, stdout=subprocess.PIPE,
                         stderr=subprocess.DEVNULL, check=True)
    return json.loads(out.stdout)


def cluster_sizes(ore_tiles):
    """Connected components (4-adjacency) over tiles with ore."""
    todo = set(ore_tiles)
    sizes = []
    while todo:
        start = todo.pop()
        q = deque([start])
        size = 1
        while q:
            x, y = q.popleft()
            for nx, ny in ((x + 1, y), (x - 1, y), (x, y + 1), (x, y - 1)):
                if (nx, ny) in todo:
                    todo.remove((nx, ny))
                    q.append((nx, ny))
                    size += 1
        sizes.append(size)
    sizes.sort(reverse=True)
    return sizes


def depth_bucket(d):
    if d <= 0:
        return "exposed"
    if d <= 5:
        return "1-5"
    if d <= 15:
        return "6-15"
    if d <= 30:
        return "16-30"
    return "31+"


DEPTH_ORDER = ["exposed", "1-5", "6-15", "16-30", "31+"]


def analyze(tiles, label):
    per_mat = defaultdict(lambda: {
        "tiles": 0, "cells": 0, "exposed": 0,
        "depths": defaultdict(int), "coords": [],
    })
    land_tiles = 0
    total_tiles = 0
    for t in tiles:
        total_tiles += 1
        terrain_z = t.get("terrainZ")
        if terrain_z is not None and terrain_z > 0:
            land_tiles += 1
        oid = t.get("oreId")
        if oid is None:
            continue
        m = per_mat[oid]
        m["tiles"] += 1
        m["cells"] += t.get("oreCount", 0)
        m["coords"].append((t["x"], t["y"]))
        top = t.get("oreTopZ")
        if terrain_z is not None and top is not None:
            d = terrain_z - top
            m["depths"][depth_bucket(d)] += 1
            # surface material is ore -> genuine outcrop
            if t.get("matId") == oid:
                m["exposed"] += 1

    print(f"\n=== {label} ===")
    print(f"  tiles dumped: {total_tiles}  (land: {land_tiles})")
    if not per_mat:
        print("  NO ORE FOUND")
        return per_mat

    for oid in sorted(per_mat):
        m = per_mat[oid]
        name = ORE_NAMES.get(oid, f"mat{oid}")
        clusters = cluster_sizes(m["coords"])
        pct = 100.0 * m["tiles"] / max(1, land_tiles)
        depths = " ".join(
            f"{k}:{m['depths'][k]}" for k in DEPTH_ORDER if m["depths"][k])
        print(f"  {name:12s} tiles={m['tiles']:6d} ({pct:.2f}% of land)"
              f"  cells={m['cells']:7d}  outcrop_tiles={m['exposed']}")
        print(f"  {'':12s} deposits={len(clusters)}"
              f"  largest={clusters[0] if clusters else 0}"
              f"  >=50tiles={sum(1 for c in clusters if c >= 50)}")
        print(f"  {'':12s} depth-to-top {depths}")
    return per_mat


def main():
    ap = argparse.ArgumentParser(description="Ore deposition report")
    ap.add_argument("--seeds", default=None,
                    help="comma-separated seeds (default: canonical 8)")
    ap.add_argument("--worldSize", type=int, default=64)
    ap.add_argument("--plates", type=int, default=None,
                    help="plate count (default: engine default for size)")
    ap.add_argument("--files", nargs="*", default=None,
                    help="analyze pre-dumped JSON files instead of generating")
    args = ap.parse_args()

    summary = defaultdict(lambda: {"tiles": 0, "cells": 0,
                                   "exposed": 0, "seeds_with": 0})
    labels = []

    if args.files:
        runs = [(f, json.load(open(f))) for f in args.files]
    else:
        seeds = ([int(s) for s in args.seeds.split(",")]
                 if args.seeds else DEFAULT_SEEDS)
        runs = [(f"seed {s}", run_dump(s, args.worldSize, args.plates))
                for s in seeds]

    for label, tiles in runs:
        labels.append(label)
        per_mat = analyze(tiles, label)
        for oid, m in per_mat.items():
            summary[oid]["tiles"] += m["tiles"]
            summary[oid]["cells"] += m["cells"]
            summary[oid]["exposed"] += m["exposed"]
            if m["tiles"] > 0:
                summary[oid]["seeds_with"] += 1

    n = len(labels)
    print(f"\n=== SUMMARY ({n} worlds) ===")
    if not summary:
        print("  NO ORE IN ANY WORLD — deposition pass produced nothing.")
        sys.exit(0)
    for oid in sorted(summary):
        s = summary[oid]
        name = ORE_NAMES.get(oid, f"mat{oid}")
        print(f"  {name:12s} present in {s['seeds_with']}/{n} worlds"
              f"  avg_tiles={s['tiles'] // max(1, n)}"
              f"  avg_cells={s['cells'] // max(1, n)}"
              f"  total_outcrops={s['exposed']}")


if __name__ == "__main__":
    main()
