#!/usr/bin/env python3
"""River/lake bed depth report — quality/measurement tool (no bug gating).

The #223 instrument. Dumps worlds across seeds (terrain+fluid) and
reports, per seed and in aggregate:

  - river bed depth histogram (water column at each river tile,
    fluidSurf - terrainZ): the pre-#223 world was ~95% exactly 1 z;
    canyon/rift reaches now deepen to 4-9 z where valley walls stand
    >= ~16 z over the water or the rift field is hot.
  - canyon mileage: fraction of river tiles >= 5 z deep, and the
    contiguous runs they form (a healthy world shows a few long runs,
    not speckle).
  - lake bodies by size class with max/median bed depth.
  - graben candidates: inland lakes (surface above sea level) whose
    interior reaches >= 8 z — the rift-lake signature (natural deep
    basins like calderas also land here; the point is variety exists).

Usage:
  python3 tools/bed_report.py                     # default seeds, w64
  python3 tools/bed_report.py --seeds 42,314 --worldSize 128
  python3 tools/bed_report.py --files a.json b.json

seaLevel is 0 in dump coordinates.
"""

import argparse
import json
import subprocess
import sys
from collections import Counter, deque

DEFAULT_SEEDS = [4, 7, 42, 99, 123, 555, 777, 1337]

DEEP_RIVER_Z = 5      # river tile counts as canyon mileage at this depth
GRABEN_LAKE_Z = 8     # inland lake counts as deep-bed at this depth

SIZE_CLASSES = [(1, 15, "tiny 1-15"), (16, 100, "small 16-100"),
                (101, 1000, "mid 101-1000"), (1001, 10**9, "large >1000")]


def run_dump(seed, world_size, plates=None):
    half = world_size // 2
    region = f"{-half},{-half},{half - 1},{half - 1}"
    cmd = [
        "cabal", "run", "-v0", "exe:synarchy", "--",
        "--dump=terrain,fluid",
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


def cluster_sizes(tiles, link_radius=2):
    """Sizes of connected clusters over a tile set, linking within
    Chebyshev link_radius (rivers are thin; radius 2 bridges the odd
    1-tile gap along a channel)."""
    seen = set()
    sizes = []
    for start in tiles:
        if start in seen:
            continue
        q = deque([start])
        seen.add(start)
        n = 0
        while q:
            x, y = q.popleft()
            n += 1
            for dx in range(-link_radius, link_radius + 1):
                for dy in range(-link_radius, link_radius + 1):
                    t = (x + dx, y + dy)
                    if t in tiles and t not in seen:
                        seen.add(t)
                        q.append(t)
        sizes.append(n)
    sizes.sort(reverse=True)
    return sizes


def lake_bodies(lake_depth):
    """Connected lake bodies (4-adjacency) -> list of sorted depth lists."""
    seen = set()
    bodies = []
    for start in lake_depth:
        if start in seen:
            continue
        q = deque([start])
        seen.add(start)
        depths = []
        while q:
            x, y = q.popleft()
            depths.append(lake_depth[(x, y)])
            for t in ((x + 1, y), (x - 1, y), (x, y + 1), (x, y - 1)):
                if t in lake_depth and t not in seen:
                    seen.add(t)
                    q.append(t)
        depths.sort()
        bodies.append(depths)
    return bodies


def analyze(tiles, label):
    river_hist = Counter()
    deep_river = set()
    lake_depth = {}
    lake_surf = {}
    for t in tiles:
        ft = t.get("fluidType")
        if ft not in ("river", "lake"):
            continue
        fs, tz = t.get("fluidSurf"), t.get("terrainZ")
        if fs is None or tz is None:
            continue
        d = fs - tz
        if ft == "river":
            river_hist[d] += 1
            if d >= DEEP_RIVER_Z:
                deep_river.add((t["x"], t["y"]))
        else:
            lake_depth[(t["x"], t["y"])] = d
            lake_surf[(t["x"], t["y"])] = fs

    print(f"\n--- {label} ---")

    n_river = sum(river_hist.values())
    if n_river:
        top = " ".join(f"{k}:{100 * v / n_river:.1f}%"
                       for k, v in sorted(river_hist.items()))
        print(f"rivers: {n_river} tiles | {top}")
        runs = cluster_sizes(deep_river)
        canyon_pct = 100 * len(deep_river) / n_river
        print(f"  canyon mileage (>= {DEEP_RIVER_Z} z): "
              f"{len(deep_river)} tiles ({canyon_pct:.1f}%) in "
              f"{len(runs)} runs, longest {runs[:5]}")
    else:
        print("rivers: none in region")

    bodies = lake_bodies(lake_depth)
    for lo, hi, name in SIZE_CLASSES:
        sel = [b for b in bodies if lo <= len(b) <= hi]
        if not sel:
            continue
        maxs = sorted(b[-1] for b in sel)
        meds = sorted(b[len(b) // 2] for b in sel)
        n = len(sel)
        print(f"  lakes {name:14s}: {n:4d} bodies | "
              f"max-depth p50={maxs[n // 2]:3d} p90={maxs[int(n * .9)]:3d} "
              f"| med-depth p50={meds[n // 2]:3d}")
    # deep-bed lakes (graben signature; includes natural deep basins)
    graben = sum(1 for depths in bodies if depths[-1] >= GRABEN_LAKE_Z)
    print(f"  deep-bed lakes (max >= {GRABEN_LAKE_Z} z): "
          f"{graben} of {len(bodies)} bodies")

    return {
        "river_tiles": n_river,
        "river_le1": sum(v for k, v in river_hist.items() if k <= 1),
        "canyon_tiles": len(deep_river),
        "lake_bodies": len(bodies),
        "deep_lakes": graben,
    }


def main():
    ap = argparse.ArgumentParser(description="River/lake bed depth report")
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

    agg = Counter()
    for label, tiles in runs:
        for k, v in analyze(tiles, label).items():
            agg[k] += v

    print(f"\n=== SUMMARY ({len(runs)} worlds) ===")
    rt = agg["river_tiles"]
    if rt:
        print(f"  river tiles {rt}: depth<=1 {100 * agg['river_le1'] / rt:.1f}%"
              f", canyon mileage {100 * agg['canyon_tiles'] / rt:.1f}%")
    print(f"  lake bodies {agg['lake_bodies']}, "
          f"deep-bed (>= {GRABEN_LAKE_Z} z) {agg['deep_lakes']}")


if __name__ == "__main__":
    main()
