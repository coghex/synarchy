#!/usr/bin/env python3
"""
Report water tiles whose surface is visibly above adjacent dry land — the
"water floating above grass" appearance.

Reported:
  1. Every (water tile, direction) pair — river, lake or ocean — whose surface S
     stands more than 1 above the terrain z of the dry tile in that direction,
     so the water visibly floats above it.  Broken down by fluid type and by
     difference, with the count of distinct water tiles involved and the worst
     examples.
  2. Lake tiles with a dry 4-neighbor whose terrain is strictly below the lake
     surface — a bank the lake should have overflowed into.  Counted once per
     lake tile.  River and ocean tiles are not examined for this one.

This is an exploratory DIAGNOSTIC, not a gate: it reports what it measures and
never turns an anomaly count into a failure, so every analysis that completes
exits 0 no matter how many anomalies it just printed.  (A missing file, invalid
JSON, or other runtime error still fails the way it always has.)  The maintained
pass/fail worldgen gate over these anomaly classes is `tools/world_audit.py`,
enforced per seed by `tools/world_check.py`.  Its checks OVERLAP this report
rather than covering it exhaustively: this script counts every
(water tile, direction) pair, includes ocean tiles, and flags one-step dry
banks below a lake surface, all of which the audit's own checks exclude or
classify differently.

Overlapping `world_audit.py` categories:
  - ISSUE 1 (water floating above adjacent dry land): `WATER_ABOVE_LAND` and
    `WATER_CLIFF`.  Both require a ≥2 step, exclude ocean, and report once per
    water tile; `WATER_ABOVE_LAND` further requires the dry neighbor to be
    above sea level.
  - ISSUE 2 (lake tiles with a dry bank below the lake surface): `LAKE_HOLE`
    and `SUBMERGED_BUMP` cover narrower forms of the same shape.

`DRY_BELOW_SEA` and the `FLOATING_FLUID` family are NOT equivalents of either
breakdown: those key on ocean connectivity, or on fluid depth relative to the
same tile's own terrain, rather than on a neighbor comparison.
"""

import json
import sys
from collections import Counter


def main(path):
    with open(path) as f:
        data = json.load(f)

    tile_map = {(t['x'], t['y']): t for t in data}

    water_tiles = [t for t in data if t.get('fluidType') in ('river', 'lake', 'ocean')]
    dry_tiles = [t for t in data if t.get('fluidType') is None]

    print(f'Water: {len(water_tiles)}, Dry: {len(dry_tiles)}')

    # Issue 1: Water tile surface > adjacent dry tile's terrain + 1
    # (water visibly floats above adjacent dry tile)
    water_above_land = []
    for t in water_tiles:
        x, y = t['x'], t['y']
        ft = t['fluidType']
        s = t.get('fluidSurf', 0)
        for nx, ny in [(x+1,y),(x-1,y),(x,y+1),(x,y-1)]:
            if (nx, ny) not in tile_map:
                continue
            n = tile_map[(nx, ny)]
            if n.get('fluidType') is None:  # dry neighbor
                nterr = n.get('terrainZ', 0)
                diff = s - nterr
                if diff > 1:
                    water_above_land.append((x, y, ft, s, nx, ny, nterr, diff))

    print()
    print(f'ISSUE 1: Water tiles floating above adjacent dry land (diff > 1)')
    print(f'  Total (tile,dir) pairs: {len(water_above_land)}')
    if water_above_land:
        by_type = Counter(w[2] for w in water_above_land)
        for t, c in by_type.most_common():
            print(f'    {t}: {c}')
        diffs = Counter(w[7] for w in water_above_land)
        print('  Difference distribution:')
        for d in sorted(diffs.keys())[:15]:
            print(f'    diff={d}: {diffs[d]}')
        # Count UNIQUE water tiles affected (not dir pairs)
        unique_water = set((w[0], w[1]) for w in water_above_land)
        print(f'  Unique water tiles with floating issue: {len(unique_water)}')

    # Issue 2: Lake tiles with dry neighbor BELOW lake surface
    # (lake should have overflowed)
    lake_overflow = 0
    lake_overflow_ex = []
    for t in water_tiles:
        if t.get('fluidType') != 'lake': continue
        x, y = t['x'], t['y']
        s = t.get('fluidSurf', 0)
        for nx, ny in [(x+1,y),(x-1,y),(x,y+1),(x,y-1)]:
            if (nx, ny) not in tile_map:
                continue
            n = tile_map[(nx, ny)]
            if n.get('fluidType') is None:
                nterr = n.get('terrainZ', 0)
                if nterr < s:  # bank BELOW lake surface
                    lake_overflow += 1
                    if len(lake_overflow_ex) < 5:
                        lake_overflow_ex.append((x, y, s, nx, ny, nterr))
                    break

    print()
    print(f'ISSUE 2: Lake tiles with dry bank BELOW lake surface')
    print(f'  Total: {lake_overflow}')
    for ex in lake_overflow_ex:
        print(f'  lake({ex[0]},{ex[1]}) surf={ex[2]} | dry({ex[3]},{ex[4]}) terr={ex[5]}')

    # Show worst examples of Issue 1
    if water_above_land:
        print()
        print('Worst water-above-land examples:')
        worst = sorted(water_above_land, key=lambda w: -w[7])[:10]
        for w in worst:
            print(f'  {w[2]}({w[0]},{w[1]}) surf={w[3]} | dry({w[4]},{w[5]}) terr={w[6]} diff={w[7]}')


if __name__ == '__main__':
    main(sys.argv[1] if len(sys.argv) > 1 else '/tmp/dump.json')
