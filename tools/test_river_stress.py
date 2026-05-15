#!/usr/bin/env python3
"""Multi-seed river placement stress test.

Runs test_river_pour.py across many seeds and aggregates results.
Fails if any seed exceeds thresholds.

Usage:
    python3 tools/test_river_stress.py [--seeds N] [--worldSize N]
"""
import json
import subprocess
import sys
import argparse
import time

def parse_args():
    p = argparse.ArgumentParser(description="River placement stress test")
    p.add_argument("--seeds", type=int, default=20,
                   help="Number of seeds to test (default 20)")
    p.add_argument("--worldSize", type=int, default=64)
    p.add_argument("--region", type=str, default="-4,-4,4,4")
    p.add_argument("--start-seed", type=int, default=1,
                   help="First seed to test")
    return p.parse_args()

def run_one_seed(seed, worldSize, region):
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

    stats = {
        "seed": seed,
        "river": river_count,
        "body": body_count,
        "vis_drops": vis_drops,
        "dry_gaps": dry_gaps,
        "mask_dry": mask_dry,
    }

    passed = vis_drops == 0 and dry_gaps <= 20 and mask_dry <= 30
    return passed, stats

def main():
    args = parse_args()
    seeds = list(range(args.start_seed, args.start_seed + args.seeds))

    print(f"River placement stress test: {len(seeds)} seeds, "
          f"worldSize={args.worldSize}, region={args.region}")
    print("=" * 70)

    results = []
    failures = []
    start_time = time.time()

    for i, seed in enumerate(seeds):
        t0 = time.time()
        passed, stats = run_one_seed(seed, args.worldSize, args.region)
        elapsed = time.time() - t0

        status = "PASS" if passed else "FAIL"
        river = stats.get("river", "?")
        drops = stats.get("vis_drops", "?")
        gaps = stats.get("dry_gaps", "?")
        mask = stats.get("mask_dry", "?")
        err = stats.get("error", "")

        line = (f"  [{i+1:2d}/{len(seeds)}] seed={seed:5d}  {status}  "
                f"river={river:5}  drops={drops}  gaps={gaps}  "
                f"mask_dry={mask}  ({elapsed:.1f}s)")
        if err:
            line += f"  ERROR={err}"
        print(line)

        results.append(stats)
        if not passed:
            failures.append(stats)

    total_time = time.time() - start_time

    # Aggregate stats
    all_rivers = [r["river"] for r in results if "river" in r]
    all_drops = [r["vis_drops"] for r in results if "vis_drops" in r]
    all_gaps = [r["dry_gaps"] for r in results if "dry_gaps" in r]
    all_mask = [r["mask_dry"] for r in results if "mask_dry" in r]

    print("\n" + "=" * 70)
    print("AGGREGATE RESULTS")
    print("=" * 70)
    if all_rivers:
        print(f"  River tiles:    min={min(all_rivers):5d}  max={max(all_rivers):5d}  "
              f"avg={sum(all_rivers)/len(all_rivers):.0f}")
    if all_drops:
        print(f"  Visible drops:  min={min(all_drops):5d}  max={max(all_drops):5d}  "
              f"avg={sum(all_drops)/len(all_drops):.1f}  "
              f"total={sum(all_drops)}")
    if all_gaps:
        print(f"  Dry gaps:       min={min(all_gaps):5d}  max={max(all_gaps):5d}  "
              f"avg={sum(all_gaps)/len(all_gaps):.1f}  "
              f"total={sum(all_gaps)}")
    if all_mask:
        print(f"  Mask-dry:       min={min(all_mask):5d}  max={max(all_mask):5d}  "
              f"avg={sum(all_mask)/len(all_mask):.1f}")

    print(f"\n  Time: {total_time:.0f}s ({total_time/len(seeds):.1f}s/seed)")
    print(f"\n  PASSED: {len(seeds) - len(failures)}/{len(seeds)}")

    if failures:
        print(f"  FAILED: {len(failures)}")
        for f in failures:
            print(f"    seed={f['seed']}: drops={f.get('vis_drops','?')} "
                  f"gaps={f.get('dry_gaps','?')} "
                  f"mask_dry={f.get('mask_dry','?')} "
                  f"{f.get('error','')}")

    print("=" * 70)
    if failures:
        print("RESULT: FAIL")
        sys.exit(1)
    else:
        print("RESULT: PASS")
        sys.exit(0)

if __name__ == "__main__":
    main()
