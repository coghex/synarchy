#!/usr/bin/env python3
"""Multi-seed river placement stress test.

Runs a --dump per seed and aggregates the same four gated metrics
test_river_pour.py checks, reusing its coastal-run analysis so the two
tools cannot disagree about that metric. Fails if any seed exceeds
thresholds.

Usage:
    python3 tools/test_river_stress.py [--seeds N] [--worldSize N]
        [--max-visible-drops N] [--max-dry-gaps N] [--max-mask-dry N]
        [--max-coastal-parallel N]
"""
import json
import subprocess
import sys
import argparse
import os
import time

sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
import river_thresholds as rt
# The coastal metric is measured by exactly one implementation, shared
# with the single-seed gate, so the two tools gate the same quantity
# (#1952).
from test_river_pour import check_coastal_parallels, longest_coastal_parallel

def parse_args(argv=None):
    p = argparse.ArgumentParser(description="River placement stress test")
    p.add_argument("--seeds", type=int, default=20,
                   help="Number of seeds to test (default 20)")
    p.add_argument("--worldSize", type=int, default=64)
    p.add_argument("--region", type=str, default="-4,-4,4,4")
    p.add_argument("--start-seed", type=int, default=1,
                   help="First seed to test")
    # Thresholds shared with test_river_pour.py via tools/river_thresholds.py.
    p.add_argument("--max-visible-drops", type=int, default=rt.MAX_VISIBLE_DROPS)
    p.add_argument("--max-dry-gaps", type=int, default=rt.MAX_DRY_GAPS)
    p.add_argument("--max-mask-dry", type=int, default=rt.MAX_MASK_DRY)
    p.add_argument("--max-coastal-parallel", type=int,
                   default=rt.MAX_COASTAL_PARALLEL,
                   help="Longest allowed coastal parallel run, in tiles")
    return p.parse_args(argv)

def thresholds_from_args(args):
    """The active gate values, keyed as analyze_tiles expects."""
    return {
        "visible_drops": args.max_visible_drops,
        "dry_gaps": args.max_dry_gaps,
        "mask_dry": args.max_mask_dry,
        "coastal_parallel": args.max_coastal_parallel,
    }

def analyze_tiles(seed, tiles, thresholds):
    """Compute one seed's stats from --dump tiles and apply the gate.

    Split out of run_one_seed so the verdict can be exercised on a
    synthetic corpus without generating a world (#1952).
    """
    grid = {(t["x"], t["y"]): t for t in tiles}

    river_count = sum(1 for t in tiles if t.get("fluidType") == "river")
    body_count = sum(1 for t in tiles if t.get("fluidType") in ("ocean", "lake"))

    # Visible drops
    vis_drops = 0
    for (x, y), t in grid.items():
        if t.get("fluidType") != "river":
            continue
        fs = t["fluidSurf"]
        for nx, ny in [(x-1,y),(x+1,y),(x,y-1),(x,y+1)]:
            nt = grid.get((nx, ny))
            if nt and nt.get("fluidType") in ("ocean", "lake"):
                if nt["fluidSurf"] >= nt["terrainZ"]:
                    if fs - nt["fluidSurf"] >= 1:
                        vis_drops += 1
                break

    # Dry gaps
    dry_gaps = 0
    for (x, y), t in grid.items():
        if t.get("fluidType") is not None:
            continue
        terrZ = t.get("terrainZ", -99999)
        if terrZ <= -99999:
            continue
        nbrs = [(x-1,y),(x+1,y),(x,y-1),(x,y+1)]
        has_r = any(grid.get(n, {}).get("fluidType") == "river" for n in nbrs)
        has_b = any(grid.get(n, {}).get("fluidType") in ("ocean", "lake") for n in nbrs)
        if has_r and has_b:
            dry_gaps += 1

    # Mask consistency
    mask_dry = sum(1 for t in tiles
                   if t.get("riverMask", False)
                   and t.get("fluidType") is None
                   and t.get("terrainZ", -99999) > -99999)

    # Coastal parallels: the LONGEST run's tile count, not the number of
    # runs — the same quantity the single-seed gate reports.
    coastal = longest_coastal_parallel(check_coastal_parallels(grid))

    stats = {
        "seed": seed,
        "river": river_count,
        "body": body_count,
        "vis_drops": vis_drops,
        "dry_gaps": dry_gaps,
        "mask_dry": mask_dry,
        "coastal": coastal,
    }

    passed = (vis_drops <= thresholds["visible_drops"]
              and dry_gaps <= thresholds["dry_gaps"]
              and mask_dry <= thresholds["mask_dry"]
              and coastal <= thresholds["coastal_parallel"])
    return passed, stats

def run_one_seed(seed, worldSize, region, thresholds):
    """Run dump + analysis for one seed. Returns (passed, stats_dict)."""
    cmd = [
        "cabal", "run", "exe:synarchy", "--",
        "--dump=terrain,fluid",
        "--seed", str(seed),
        "--worldSize", str(worldSize),
        "--region", region,
    ]
    try:
        result = subprocess.run(cmd, capture_output=True, text=True, timeout=300)
    except subprocess.TimeoutExpired:
        return False, {"seed": seed, "error": "timeout"}

    if result.returncode != 0 or not result.stdout.strip():
        return False, {"seed": seed, "error": "dump_failed"}

    try:
        tiles = json.loads(result.stdout)
    except json.JSONDecodeError:
        return False, {"seed": seed, "error": "bad_json"}

    return analyze_tiles(seed, tiles, thresholds)

# ── Reporting surfaces ──────────────────────────────────────────
# Every surface that enumerates gated metrics names all four, so a
# metric cannot be gated invisibly.

def format_threshold_banner(thresholds):
    """The active-threshold banner."""
    return (f"Thresholds: drops<={thresholds['visible_drops']}  "
            f"dry_gaps<={thresholds['dry_gaps']}  "
            f"mask_dry<={thresholds['mask_dry']}  "
            f"coastal<={thresholds['coastal_parallel']} tiles")

def format_seed_line(index, total, seed, stats, passed, elapsed):
    """One seed's per-run statistics line."""
    status = "PASS" if passed else "FAIL"
    river = stats.get("river", "?")
    drops = stats.get("vis_drops", "?")
    gaps = stats.get("dry_gaps", "?")
    mask = stats.get("mask_dry", "?")
    coastal = stats.get("coastal", "?")
    err = stats.get("error", "")
    line = (f"  [{index:2d}/{total}] seed={seed:5d}  {status}  "
            f"river={river:5}  drops={drops}  gaps={gaps}  "
            f"mask_dry={mask}  coastal={coastal}  ({elapsed:.1f}s)")
    if err:
        line += f"  ERROR={err}"
    return line

def format_failure_line(stats):
    """One failing seed's detail line."""
    return (f"    seed={stats['seed']}: drops={stats.get('vis_drops','?')} "
            f"gaps={stats.get('dry_gaps','?')} "
            f"mask_dry={stats.get('mask_dry','?')} "
            f"coastal={stats.get('coastal','?')} "
            f"{stats.get('error','')}")

def format_aggregate(results):
    """The AGGREGATE RESULTS body, one line per reported metric."""
    lines = []
    all_rivers = [r["river"] for r in results if "river" in r]
    all_drops = [r["vis_drops"] for r in results if "vis_drops" in r]
    all_gaps = [r["dry_gaps"] for r in results if "dry_gaps" in r]
    all_mask = [r["mask_dry"] for r in results if "mask_dry" in r]
    all_coastal = [r["coastal"] for r in results if "coastal" in r]

    if all_rivers:
        lines.append(f"  River tiles:    min={min(all_rivers):5d}  max={max(all_rivers):5d}  "
                     f"avg={sum(all_rivers)/len(all_rivers):.0f}")
    if all_drops:
        lines.append(f"  Visible drops:  min={min(all_drops):5d}  max={max(all_drops):5d}  "
                     f"avg={sum(all_drops)/len(all_drops):.1f}  "
                     f"total={sum(all_drops)}")
    if all_gaps:
        lines.append(f"  Dry gaps:       min={min(all_gaps):5d}  max={max(all_gaps):5d}  "
                     f"avg={sum(all_gaps)/len(all_gaps):.1f}  "
                     f"total={sum(all_gaps)}")
    if all_mask:
        lines.append(f"  Mask-dry:       min={min(all_mask):5d}  max={max(all_mask):5d}  "
                     f"avg={sum(all_mask)/len(all_mask):.1f}")
    if all_coastal:
        # Each seed's value is already that seed's LONGEST coastal run, so
        # this row's max= is the worst run seen across the whole sweep.
        lines.append(f"  Coastal run:    min={min(all_coastal):5d}  max={max(all_coastal):5d}  "
                     f"avg={sum(all_coastal)/len(all_coastal):.1f}")
    return lines

def main():
    args = parse_args()
    seeds = list(range(args.start_seed, args.start_seed + args.seeds))
    thresholds = thresholds_from_args(args)

    print(f"River placement stress test: {len(seeds)} seeds, "
          f"worldSize={args.worldSize}, region={args.region}")
    print(format_threshold_banner(thresholds))
    print("=" * 70)

    results = []
    failures = []
    start_time = time.time()

    for i, seed in enumerate(seeds):
        t0 = time.time()
        passed, stats = run_one_seed(seed, args.worldSize, args.region, thresholds)
        elapsed = time.time() - t0

        print(format_seed_line(i + 1, len(seeds), seed, stats, passed, elapsed))

        results.append(stats)
        if not passed:
            failures.append(stats)

    total_time = time.time() - start_time

    print("\n" + "=" * 70)
    print("AGGREGATE RESULTS")
    print("=" * 70)
    for line in format_aggregate(results):
        print(line)

    print(f"\n  Time: {total_time:.0f}s ({total_time/len(seeds):.1f}s/seed)")
    print(f"\n  PASSED: {len(seeds) - len(failures)}/{len(seeds)}")

    if failures:
        print(f"  FAILED: {len(failures)}")
        for f in failures:
            print(format_failure_line(f))

    print("=" * 70)
    if failures:
        print("RESULT: FAIL")
        sys.exit(1)
    else:
        print("RESULT: PASS")
        sys.exit(0)

if __name__ == "__main__":
    main()
