#!/usr/bin/env python3
"""Coastline variety report — quality/measurement tool (no bug gating).

The #220 instrument. Dumps worlds across seeds (terrain+material+fluid)
and reports, per seed:

  - elevation-vs-distance-from-ocean profile: p10/median/p90 of land
    terrainZ at each BFS distance from ocean tiles. A monotonous
    beach-to-cliff world shows a tight band jumping to the plateau by
    d~10; variety shows a wide p10/p90 spread.
  - coast-form census: 16x16-tile windows containing shoreline are
    classified as cliff / steep / ramp / beach_plain / marsh, so
    "almost everything is short-beach-to-cliff" is measurable.
  - sheer-cliff population: windows whose near-shore land (d<=2)
    already stands >= 15z above the sea.

Usage:
  python3 tools/coast_report.py                     # default seeds, w64
  python3 tools/coast_report.py --seeds 42,314 --worldSize 128
  python3 tools/coast_report.py --files a.json b.json

seaLevel is 0 in dump coordinates (terrainZ is height above sea).
"""

import argparse
import json
import subprocess
import sys
from collections import defaultdict, deque

DEFAULT_SEEDS = [4, 7, 42, 99, 123, 555, 777, 1337]

MAX_DIST = 30
WINDOW = 16
MARSH_MATS = {62, 63, 64}

PROFILE_DISTS = [1, 2, 3, 4, 6, 8, 10, 14, 18, 22, 26, 30]

FORM_ORDER = ["cliff", "steep", "ramp", "beach_plain", "marsh"]


def run_dump(seed, world_size, plates=None):
    half = world_size // 2
    region = f"{-half},{-half},{half - 1},{half - 1}"
    cmd = [
        "cabal", "run", "-v0", "exe:synarchy", "--",
        "--dump=terrain,material,fluid",
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


def percentile(sorted_vals, p):
    if not sorted_vals:
        return None
    k = min(len(sorted_vals) - 1, int(round(p * (len(sorted_vals) - 1))))
    return sorted_vals[k]


def bfs_dist_from_ocean(tiles):
    """Distance (4-adjacency, tile grid) from the nearest ocean tile.

    Returns {(x,y): dist} for non-ocean tiles within MAX_DIST.
    Glacier-zone / beyond-glacier tiles are excluded entirely (world
    edge, never eroded by the coastal pass).
    """
    grid = {}
    ocean = []
    for t in tiles:
        if t.get("glacierZone") or t.get("beyondGlacier"):
            continue
        xy = (t["x"], t["y"])
        grid[xy] = t
        if t.get("fluidType") == "ocean":
            ocean.append(xy)

    dist = {xy: 0 for xy in ocean}
    frontier = deque(ocean)
    while frontier:
        x, y = frontier.popleft()
        d = dist[(x, y)]
        if d >= MAX_DIST:
            continue
        for nxy in ((x + 1, y), (x - 1, y), (x, y + 1), (x, y - 1)):
            if nxy in grid and nxy not in dist:
                dist[nxy] = d + 1
                frontier.append(nxy)
    return grid, dist


def analyze(tiles, label):
    grid, dist = bfs_dist_from_ocean(tiles)

    # --- elevation profile over LAND tiles (above sea, no fluid) ---
    by_d = defaultdict(list)
    for xy, d in dist.items():
        if d == 0:
            continue
        t = grid[xy]
        if t.get("fluidType") is None and t["terrainZ"] > 0:
            by_d[d].append(t["terrainZ"])
    for d in by_d:
        by_d[d].sort()

    # --- window census ---
    windows = defaultdict(lambda: {"shore": 0, "marsh": 0,
                                   "z_near": [], "z_mid": [], "z_far": [],
                                   "z_edge": []})
    for xy, d in dist.items():
        if d == 0:
            continue
        t = grid[xy]
        if t.get("fluidType") is not None or t["terrainZ"] <= 0:
            continue
        w = (xy[0] // WINDOW, xy[1] // WINDOW)
        ww = windows[w]
        if d <= 2:
            ww["shore"] += 1
            ww["z_edge"].append(t["terrainZ"])
        if d <= 8 and t.get("matId") in MARSH_MATS:
            ww["marsh"] += 1
        if 2 <= d <= 4:
            ww["z_near"].append(t["terrainZ"])
        elif 9 <= d <= 12:
            ww["z_mid"].append(t["terrainZ"])
        elif 18 <= d <= 22:
            ww["z_far"].append(t["terrainZ"])

    forms = defaultdict(int)
    sheer = 0
    for ww in windows.values():
        if ww["shore"] < 4 or not ww["z_near"]:
            continue
        z_near = sorted(ww["z_near"])
        z_mid = sorted(ww["z_mid"])
        z_far = sorted(ww["z_far"])
        n = percentile(z_near, 0.5)
        m = percentile(z_mid, 0.5)
        f = percentile(z_far, 0.5)
        if ww["marsh"] >= 6:
            forms["marsh"] += 1
        elif n >= 12:
            forms["cliff"] += 1
        elif n >= 6:
            forms["steep"] += 1
        elif m is not None and m <= 5 and (f is None or f <= 8):
            forms["beach_plain"] += 1
        else:
            forms["ramp"] += 1
        if percentile(sorted(ww["z_edge"]), 0.9) is not None \
                and percentile(sorted(ww["z_edge"]), 0.9) >= 15:
            sheer += 1

    total = sum(forms.values())
    print(f"\n=== {label} ===")
    if total == 0:
        print("  NO COASTAL WINDOWS FOUND")
        return forms, 0
    frac = {k: forms[k] / total for k in FORM_ORDER}
    print("  coast windows: {}  |  ".format(total) + "  ".join(
        f"{k}={forms[k]:4d} ({100 * frac[k]:4.1f}%)" for k in FORM_ORDER))
    print(f"  sheer-cliff windows (p90 z@d<=2 >= 15): {sheer}")
    print("  dist:   " + " ".join(f"{d:5d}" for d in PROFILE_DISTS))
    print("  p10:    " + " ".join(
        f"{percentile(by_d.get(d, []), 0.1):5d}" if by_d.get(d) else "    -"
        for d in PROFILE_DISTS))
    print("  median: " + " ".join(
        f"{percentile(by_d.get(d, []), 0.5):5d}" if by_d.get(d) else "    -"
        for d in PROFILE_DISTS))
    print("  p90:    " + " ".join(
        f"{percentile(by_d.get(d, []), 0.9):5d}" if by_d.get(d) else "    -"
        for d in PROFILE_DISTS))
    return forms, total


def main():
    ap = argparse.ArgumentParser(description="Coastline variety report")
    ap.add_argument("--seeds", default=None,
                    help="comma-separated seeds (default: canonical 8)")
    ap.add_argument("--worldSize", type=int, default=64)
    ap.add_argument("--plates", type=int, default=None)
    ap.add_argument("--files", nargs="*", default=None,
                    help="analyze pre-dumped JSON files instead of generating")
    args = ap.parse_args()

    if args.files:
        runs = [(f, json.load(open(f))) for f in args.files]
    else:
        seeds = ([int(s) for s in args.seeds.split(",")]
                 if args.seeds else DEFAULT_SEEDS)
        runs = [(f"seed {s}", run_dump(s, args.worldSize, args.plates))
                for s in seeds]

    agg = defaultdict(int)
    grand = 0
    for label, tiles in runs:
        forms, total = analyze(tiles, label)
        for k, v in forms.items():
            agg[k] += v
        grand += total

    print(f"\n=== SUMMARY ({len(runs)} worlds, {grand} coast windows) ===")
    if grand == 0:
        sys.exit(0)
    for k in FORM_ORDER:
        print(f"  {k:12s} {agg[k]:5d}  ({100 * agg[k] / grand:5.1f}%)")


if __name__ == "__main__":
    main()
