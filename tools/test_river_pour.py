#!/usr/bin/env python3
"""River fluid placement regression test.

Runs a --dump and checks river-to-body transitions for correctness.
Exit code 0 = all checks pass. Exit code 1 = failures found.

Checks:
  1. Visible drops:     river surface > body surface at rendered boundary
  2. Dry gaps:          no-fluid tile adjacent to BOTH river and body
  3. Mask consistency:  mask=true tiles should have fluid
  4. Monotonic descent: river surfaces should not increase downstream
  5. Cross-chunk seams: edge tiles should match neighbor within 2
  6. Coastal parallels: longest run of river tiles alongside ocean at
                        high elevation (a LENGTH, not a count of runs)

Usage:
    python3 tools/test_river_pour.py [path]
        path defaults to /tmp/dump.json; '-' reads from stdin.
    python3 tools/test_river_pour.py --seed N [--worldSize N] [--region cx1,cy1,cx2,cy2]
        Runs cabal --dump itself when --seed/--worldSize/--region are passed without a path.
"""
import json
import subprocess
import sys
import argparse
import os
from collections import defaultdict

sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
import river_thresholds as rt

CHUNK_SIZE = 16

def parse_args():
    p = argparse.ArgumentParser(description="River placement regression test")
    p.add_argument("path", nargs="?", default=None,
                   help="JSON dump file (default /tmp/dump.json; '-' for stdin)")
    p.add_argument("--seed", type=int, default=None,
                   help="If set without a path, runs cabal --dump with this seed")
    p.add_argument("--worldSize", type=int, default=None,
                   help="If set without a path, runs cabal --dump with this worldSize")
    p.add_argument("--region", type=str, default=None,
                   help="chunk region cx1,cy1,cx2,cy2 (triggers cabal --dump when set without a path)")
    p.add_argument("--verbose", "-v", action="store_true")
    # Thresholds for pass/fail (shared with test_river_stress.py via
    # tools/river_thresholds.py — keep them in sync there, not here).
    p.add_argument("--max-visible-drops", type=int, default=rt.MAX_VISIBLE_DROPS)
    p.add_argument("--max-dry-gaps", type=int, default=rt.MAX_DRY_GAPS)
    p.add_argument("--max-mask-dry", type=int, default=rt.MAX_MASK_DRY)
    p.add_argument("--max-coastal-parallel", type=int, default=rt.MAX_COASTAL_PARALLEL)
    return p.parse_args()

def run_dump(seed, worldSize, region):
    """Run the engine in dump mode and return parsed tile data."""
    cmd = [
        "cabal", "run", "exe:synarchy", "--",
        "--dump=terrain,fluid",
        f"--seed", str(seed),
        f"--worldSize", str(worldSize),
        f"--region", region,
    ]
    print(f"Running: {' '.join(cmd)}", file=sys.stderr)
    result = subprocess.run(cmd, capture_output=True, text=True, timeout=300)
    if result.returncode != 0:
        print(f"FAILED (exit {result.returncode})", file=sys.stderr)
        print(result.stderr[:2000], file=sys.stderr)
        sys.exit(1)
    return json.loads(result.stdout)

def build_grid(tiles):
    grid = {}
    for t in tiles:
        grid[(t["x"], t["y"])] = t
    return grid

def cardinal_neighbors(x, y):
    return [(x-1,y), (x+1,y), (x,y-1), (x,y+1)]

def is_river(tile):
    return tile and tile.get("fluidType") == "river"

def is_body(tile):
    return tile and tile.get("fluidType") in ("ocean", "lake")

def is_dry(tile):
    return tile and tile.get("fluidType") is None

# ── Check 1: Visible drops ──────────────────────────────────────
def check_visible_drops(grid):
    """River tiles whose surface is above an adjacent visible body."""
    drops = []
    for (x, y), t in grid.items():
        if t.get("fluidType") != "river":
            continue
        fs = t["fluidSurf"]
        for nx, ny in cardinal_neighbors(x, y):
            nt = grid.get((nx, ny))
            if not is_body(nt):
                continue
            if nt["fluidSurf"] < nt["terrainZ"]:
                continue  # body not rendered (underground)
            drop = fs - nt["fluidSurf"]
            if drop >= 1:
                drops.append({
                    "x": x, "y": y, "riverSurf": fs,
                    "terrainZ": t["terrainZ"],
                    "bodyType": nt["fluidType"],
                    "bodySurf": nt["fluidSurf"],
                    "drop": drop,
                })
            break
    return drops

# ── Check 2: Dry gaps ───────────────────────────────────────────
def check_dry_gaps(grid):
    """Dry tiles adjacent to BOTH river and body."""
    gaps = []
    for (x, y), t in grid.items():
        if t.get("fluidType") is not None:
            continue
        terrZ = t.get("terrainZ", -99999)
        if terrZ <= -99999:
            continue
        nbrs = cardinal_neighbors(x, y)
        has_river = any(is_river(grid.get(n)) for n in nbrs)
        has_body = any(is_body(grid.get(n)) for n in nbrs)
        if has_river and has_body:
            gaps.append({"x": x, "y": y, "terrainZ": terrZ})
    return gaps

# ── Check 3: Mask consistency ───────────────────────────────────
def check_mask_consistency(grid):
    """Tiles where riverMask=true but no fluid placed."""
    issues = []
    for (x, y), t in grid.items():
        if not t.get("riverMask", False):
            continue
        if t.get("fluidType") is None and t.get("terrainZ", -99999) > -99999:
            issues.append({"x": x, "y": y, "terrainZ": t["terrainZ"]})
    return issues

# ── Check 4: Monotonic descent ──────────────────────────────────
def check_monotonic(grid):
    """River tiles whose surface is HIGHER than all river neighbors.
    These are local maxima — water shouldn't pool uphill."""
    issues = []
    for (x, y), t in grid.items():
        if t.get("fluidType") != "river":
            continue
        fs = t["fluidSurf"]
        river_nbrs = []
        for nx, ny in cardinal_neighbors(x, y):
            nt = grid.get((nx, ny))
            if is_river(nt):
                river_nbrs.append(nt["fluidSurf"])
        if not river_nbrs:
            continue
        # A tile higher than ALL neighbors is a local max (bad)
        if all(fs > ns for ns in river_nbrs):
            max_nbr = max(river_nbrs)
            bump = fs - max_nbr
            if bump > 2:  # allow small bumps from terrain following
                issues.append({
                    "x": x, "y": y, "surf": fs,
                    "maxNbr": max_nbr, "bump": bump,
                })
    return issues

# ── Check 5: Cross-chunk seams ──────────────────────────────────
def check_chunk_seams(grid):
    """River tiles at chunk edges where the neighbor in the adjacent
    chunk has incompatible fluid (mismatch > 2 in surface)."""
    issues = []
    for (x, y), t in grid.items():
        if t.get("fluidType") != "river":
            continue
        lx = x % CHUNK_SIZE
        ly = y % CHUNK_SIZE
        if lx != 0 and lx != CHUNK_SIZE - 1 and ly != 0 and ly != CHUNK_SIZE - 1:
            continue  # not an edge tile
        fs = t["fluidSurf"]
        for nx, ny in cardinal_neighbors(x, y):
            # Only check neighbors in different chunks
            if nx // CHUNK_SIZE == x // CHUNK_SIZE and ny // CHUNK_SIZE == y // CHUNK_SIZE:
                continue
            nt = grid.get((nx, ny))
            if nt is None:
                continue
            if is_river(nt):
                diff = abs(fs - nt["fluidSurf"])
                if diff > 3:
                    issues.append({
                        "x": x, "y": y, "surf": fs,
                        "nx": nx, "ny": ny, "nbrSurf": nt["fluidSurf"],
                        "diff": diff,
                    })
                    break
    return issues

# ── Check 6: Coastal parallels ──────────────────────────────────
def check_coastal_parallels(grid):
    """Every run of river tiles at elevation > seaLevel+5 that are
    adjacent to ocean (including underground). These are rivers running
    parallel to the coast.

    A "run" is one cardinally connected component of such tiles, and its
    ``size`` is that run's LENGTH in tiles. EVERY component is returned,
    however short: the pass/fail decision belongs to the configurable
    ``river_thresholds.MAX_COASTAL_PARALLEL`` alone, so no minimum length
    is applied here (#1952). Longest run first."""
    SEA_LEVEL = 0
    # Find all river tiles adjacent to any ocean tile
    coastal_river = set()
    for (x, y), t in grid.items():
        if t.get("fluidType") != "river":
            continue
        if t["terrainZ"] <= SEA_LEVEL + 5:
            continue
        for nx, ny in cardinal_neighbors(x, y):
            nt = grid.get((nx, ny))
            if nt and nt.get("fluidType") == "ocean":
                coastal_river.add((x, y))
                break

    # Find connected runs
    visited = set()
    runs = []
    for start in coastal_river:
        if start in visited:
            continue
        # BFS to find connected component
        component = []
        queue = [start]
        visited.add(start)
        while queue:
            cx, cy = queue.pop(0)
            component.append((cx, cy))
            for nx, ny in cardinal_neighbors(cx, cy):
                if (nx, ny) in coastal_river and (nx, ny) not in visited:
                    visited.add((nx, ny))
                    queue.append((nx, ny))
        runs.append({
            "size": len(component),
            "sample": min(component),
            "terrainZ": grid[min(component)]["terrainZ"],
        })
    # Longest first, ties broken by position, so the FAIL detail leads
    # with the worst offender and the ordering does not depend on set
    # iteration order.
    runs.sort(key=lambda r: (-r["size"], r["sample"]))
    return runs


def longest_coastal_parallel(runs):
    """The observed coastal value: the longest run's tile count, 0 if none.

    This is the quantity ``river_thresholds.MAX_COASTAL_PARALLEL`` bounds.
    """
    return max((r["size"] for r in runs), default=0)

# ── Summary ─────────────────────────────────────────────────────
def summarize(results, total_river, total_body, args):
    """Print results and return (pass, fail_reasons)."""
    print("\n" + "=" * 60)
    print("RIVER PLACEMENT REGRESSION TEST")
    print("=" * 60)
    print(f"Tiles: {total_river} river, {total_body} ocean/lake")

    failures = []

    # `value` is the quantity the threshold beside it bounds. For the
    # first three that is the number of offending tiles; for the coastal
    # metric it is the longest run's LENGTH, which is why it is not
    # len(items) and why its printed line names its unit (#1952).
    coastal_runs = results["coastal"]
    for name, value, items, threshold, show_count, unit in [
        ("Visible drops",    len(results["drops"]),    results["drops"],
         args.max_visible_drops, 10, ""),
        ("Dry gaps",         len(results["gaps"]),     results["gaps"],
         args.max_dry_gaps,      10, ""),
        ("Mask consistency", len(results["mask_dry"]), results["mask_dry"],
         args.max_mask_dry,       5, ""),
        ("Longest coastal parallel", longest_coastal_parallel(coastal_runs),
         coastal_runs, args.max_coastal_parallel, 5, " tiles"),
    ]:
        status = "PASS" if value <= threshold else "FAIL"
        if status == "FAIL":
            failures.append(f"{name}: {value}{unit} > {threshold}")
        print(f"\n  {status}  {name}: {value}{unit} (max {threshold})")
        if items and (args.verbose or status == "FAIL"):
            for item in items[:show_count]:
                print(f"         {item}")
            if len(items) > show_count:
                print(f"         ... and {len(items) - show_count} more")

    # Informational (no threshold)
    mono = results["monotonic"]
    seams = results["seams"]
    print(f"\n  INFO  Monotonic bumps (>2): {len(mono)}")
    if mono and args.verbose:
        for m in mono[:5]:
            print(f"         {m}")
    print(f"  INFO  Chunk seam mismatches (>3): {len(seams)}")
    if seams and args.verbose:
        for s in seams[:5]:
            print(f"         {s}")

    print(f"\n{'=' * 60}")
    if failures:
        print(f"RESULT: FAIL ({', '.join(failures)})")
    else:
        print("RESULT: PASS")
    print("=" * 60)
    return len(failures) == 0

def main():
    args = parse_args()

    if args.path is not None:
        if args.path == "-":
            print("Reading from stdin...", file=sys.stderr)
            tiles = json.load(sys.stdin)
        else:
            print(f"Reading from {args.path}...", file=sys.stderr)
            with open(args.path) as f:
                tiles = json.load(f)
    elif args.seed is not None or args.worldSize is not None or args.region is not None:
        tiles = run_dump(args.seed or 42, args.worldSize or 64,
                         args.region or "-4,-4,4,4")
    else:
        print("Reading from /tmp/dump.json...", file=sys.stderr)
        with open("/tmp/dump.json") as f:
            tiles = json.load(f)

    print(f"Loaded {len(tiles)} tiles", file=sys.stderr)
    grid = build_grid(tiles)

    total_river = sum(1 for t in tiles if t.get("fluidType") == "river")
    total_body = sum(1 for t in tiles if t.get("fluidType") in ("ocean", "lake"))

    results = {
        "drops":     check_visible_drops(grid),
        "gaps":      check_dry_gaps(grid),
        "mask_dry":  check_mask_consistency(grid),
        "monotonic": check_monotonic(grid),
        "seams":     check_chunk_seams(grid),
        "coastal":   check_coastal_parallels(grid),
    }

    passed = summarize(results, total_river, total_body, args)
    sys.exit(0 if passed else 1)

if __name__ == "__main__":
    main()
