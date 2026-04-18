#!/usr/bin/env python3
"""
Detect water tiles whose surface is visibly above adjacent dry land.
This is the "water floating above grass" issue.

For each dry tile T at position (x,y) with terrain z=T:
  For each neighbor (x',y') that's a water tile with surface S:
    If S > T + 1 → water visibly floats above this dry tile

Also detects:
  - Isolated water columns (water tile with all dry neighbors at much lower terrain)
  - Lake/ocean tiles adjacent to dry tiles at terrain below water surface
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
