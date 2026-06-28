#!/usr/bin/env python3
"""slope_report.py — headless metric for terrain sloping (issue #224).

Drives the dump itself, then reports, per local-relief bin, what fraction
of DRY surface tiles render a non-flat slope — split into soft terrain
(hardness < 0.7, the unchanged terrace path) and exposed hard rock
(hardness >= 0.7, the rock-jaggedness path #224 added). Before #224 the
hard-rock column was 0% at every relief level (the hardness gate forced
those tiles flat); this tool shows the new distribution and checks the
two invariants the issue requires:

  * flat ground (relief 0) never slopes  -> flat biomes stay flat-topped
  * high-relief exposed rock slopes a lot -> mountains read as sloped/jagged

Usage:
  python3 tools/slope_report.py [--seed N] [--worldSize N] [--radius N]
"""
import argparse, json, subprocess, sys, os
from collections import defaultdict

HARD = 0.7  # slopeHardnessThreshold

def run_dump(seed, size, radius):
    r = radius
    cmd = ["cabal", "run", "-v0", "exe:synarchy", "--",
           "--dump=terrain,material,fluid,slope",
           "--seed", str(seed), "--worldSize", str(size),
           "--region", f"-{r},-{r},{r},{r}"]
    out = subprocess.run(cmd, capture_output=True, timeout=400)
    if out.returncode != 0:
        sys.stderr.write(out.stderr.decode()[-2000:])
        sys.exit(f"dump failed (exit {out.returncode})")
    return json.loads(out.stdout)

def main():
    ap = argparse.ArgumentParser()
    ap.add_argument("--seed", type=int, default=137)
    ap.add_argument("--worldSize", type=int, default=128)
    ap.add_argument("--radius", type=int, default=4)
    a = ap.parse_args()

    tiles = run_dump(a.seed, a.worldSize, a.radius)
    by_xy = {(t["x"], t["y"]): t for t in tiles}

    # local relief = steepest single-neighbour drop, mirrors Slope.hs
    def relief(t):
        z = t["terrainZ"]
        best = 0
        for dx, dy in ((0,-1),(1,0),(0,1),(-1,0)):
            n = by_xy.get((t["x"]+dx, t["y"]+dy))
            if n is not None:
                best = max(best, z - n["terrainZ"])
        return best

    # bins: relief level -> [soft_total, soft_sloped, hard_total, hard_sloped]
    bins = defaultdict(lambda: [0,0,0,0])
    flat_violations = 0
    considered = 0
    for t in tiles:
        # dry surface tiles only (slope/hardness fields require slope layer);
        # skip world-edge tiles whose neighbours fall outside the dumped region
        if "slope" not in t:
            continue
        # skip wet tiles: jaggedness is a dry-rock feature, and a submerged
        # bed's slope is the wet-tile rule, not what #224 changed
        if t.get("fluidType") is not None:
            continue
        # require full 4-neighbourhood inside the region for a valid relief
        if any((t["x"]+dx, t["y"]+dy) not in by_xy
               for dx,dy in ((0,-1),(1,0),(0,1),(-1,0))):
            continue
        considered += 1
        rel = relief(t)
        sloped = 1 if t["slope"] != 0 else 0
        hard = t["hardness"] >= HARD
        b = bins[min(rel, 8)]
        if hard: b[2]+=1; b[3]+=sloped
        else:    b[0]+=1; b[1]+=sloped
        if rel == 0 and sloped:
            flat_violations += 1

    print(f"seed={a.seed} worldSize={a.worldSize} "
          f"region=+/-{a.radius}  tiles_considered={considered}\n")
    print(f"{'relief':>6} | {'soft n':>8} {'soft slope%':>11} | "
          f"{'hard n':>8} {'hard slope%':>11}")
    print("-"*56)
    tot_hard=tot_hard_sl=0
    for rel in sorted(bins):
        st, ss, ht, hs = bins[rel]
        sp = f"{100*ss/st:.1f}%" if st else "  -"
        hp = f"{100*hs/ht:.1f}%" if ht else "  -"
        label = f"{rel}" + ("+" if rel==8 else "")
        print(f"{label:>6} | {st:>8} {sp:>11} | {ht:>8} {hp:>11}")
        if rel>=3: tot_hard+=ht; tot_hard_sl+=hs
    print("-"*56)

    ok = True
    # invariant 1: flat ground never slopes
    if flat_violations:
        print(f"FAIL: {flat_violations} relief-0 tiles are sloped "
              f"(flat biomes must stay flat)")
        ok = False
    else:
        print("PASS: no relief-0 (flat) tile is sloped")
    # invariant 2: high-relief exposed rock slopes substantially
    hi = (100*tot_hard_sl/tot_hard) if tot_hard else 0.0
    if tot_hard == 0:
        print("WARN: no high-relief (>=3) exposed rock in sample "
              "(try a more mountainous seed)")
    elif hi >= 50.0:
        print(f"PASS: high-relief (>=3) exposed rock slopes {hi:.1f}% "
              f"(n={tot_hard}); was 0% before #224")
    else:
        print(f"FAIL: high-relief exposed rock only {hi:.1f}% sloped "
              f"(n={tot_hard}); expected >=50%")
        ok = False

    sys.exit(0 if ok else 1)

if __name__ == "__main__":
    main()
