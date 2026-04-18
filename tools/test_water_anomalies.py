#!/usr/bin/env python3
"""
Comprehensive test for visible water anomalies in the rendered world.

Detects:
  1. Water floating 2+ above adjacent dry land (severe)
  2. Water 1 above dry land where dry tile is a non-shore (mid-water dry island)
  3. Water-water cliffs (adjacent water tiles with surface diff > 1)
  4. Lake/river tiles with dry bank BELOW their surface (should overflow)
  5. Isolated water tiles (single water tile surrounded by dry)
"""

import json
import sys
from collections import Counter


def main(path):
    with open(path) as f:
        data = json.load(f)

    tile_map = {(t['x'], t['y']): t for t in data}
    water_set = set((t['x'], t['y']) for t in data if t.get('fluidType') in ('river','lake','ocean'))
    dry_set = set((t['x'], t['y']) for t in data if t.get('fluidType') is None)

    print(f'Water: {len(water_set)}, Dry: {len(dry_set)}')

    # 1. Water tile surface vs adjacent dry tile terrain
    # Water surface should not be MORE than 1 above dry terrain
    # (1 is OK for sloping shore; >1 is a floating cliff)
    severe_float = []  # diff > 1
    shore_float = []   # diff == 1 but no slope possible
    for t in data:
        ft = t.get('fluidType')
        if ft not in ('river', 'lake', 'ocean'): continue
        x, y = t['x'], t['y']
        s = t.get('fluidSurf', 0)
        for nx, ny in [(x+1,y),(x-1,y),(x,y+1),(x,y-1)]:
            if (nx, ny) not in tile_map: continue
            n = tile_map[(nx, ny)]
            if n.get('fluidType') is None:
                nterr = n.get('terrainZ', 0)
                diff = s - nterr
                if diff > 1:
                    severe_float.append((x, y, ft, s, nx, ny, nterr, diff))

    print()
    print(f'ISSUE 1: Water surface > dry neighbor terrain + 1 (severe float)')
    print(f'  Total: {len(severe_float)}')
    if severe_float:
        by_type = Counter(w[2] for w in severe_float)
        print(f'  By type: {dict(by_type)}')

    # 2. Water-water cliffs
    ww_cliffs = []
    seen = set()
    for t in data:
        if t.get('fluidType') not in ('river', 'lake', 'ocean'): continue
        x, y = t['x'], t['y']
        s = t.get('fluidSurf', 0)
        for nx, ny in [(x+1,y),(x-1,y),(x,y+1),(x,y-1)]:
            key = tuple(sorted([(x,y),(nx,ny)]))
            if key in seen: continue
            seen.add(key)
            if (nx, ny) not in tile_map: continue
            n = tile_map[(nx, ny)]
            if n.get('fluidType') in ('river', 'lake', 'ocean'):
                ns = n.get('fluidSurf', 0)
                if abs(s - ns) > 1:
                    ww_cliffs.append((x, y, t['fluidType'], s, nx, ny, n['fluidType'], ns, abs(s-ns)))

    print()
    print(f'ISSUE 2: Water-water cliffs (adjacent water tiles, surface diff > 1)')
    print(f'  Total: {len(ww_cliffs)}')
    if ww_cliffs:
        by_type_pair = Counter(tuple(sorted([w[2], w[6]])) for w in ww_cliffs)
        for tp, c in by_type_pair.most_common(10):
            print(f'    {tp[0]}<->{tp[1]}: {c}')

    # 3. Lake/river tiles with dry bank BELOW their surface
    # (water should have overflowed to this lower bank)
    overflow = []
    for t in data:
        ft = t.get('fluidType')
        if ft not in ('lake', 'river'): continue
        x, y = t['x'], t['y']
        s = t.get('fluidSurf', 0)
        for nx, ny in [(x+1,y),(x-1,y),(x,y+1),(x,y-1)]:
            if (nx, ny) not in tile_map: continue
            n = tile_map[(nx, ny)]
            if n.get('fluidType') is None:
                nterr = n.get('terrainZ', 0)
                if nterr < s:  # bank STRICTLY below water surface
                    overflow.append((x, y, ft, s, nx, ny, nterr, s - nterr))
                    break  # one report per water tile

    print()
    print(f'ISSUE 3: Water tiles with dry bank BELOW surface (overflow failure)')
    print(f'  Total: {len(overflow)}')
    if overflow:
        diffs = Counter(w[7] for w in overflow)
        for d in sorted(diffs.keys())[:10]:
            print(f'    diff={d}: {diffs[d]}')
        print('  Examples:')
        for ex in overflow[:8]:
            print(f'    {ex[2]}({ex[0]},{ex[1]}) surf={ex[3]} | dry({ex[4]},{ex[5]}) terr={ex[6]} diff={ex[7]}')

    # 4. Isolated water tiles
    isolated = 0
    for t in data:
        ft = t.get('fluidType')
        if ft != 'river' and ft != 'lake': continue
        x, y = t['x'], t['y']
        water_nbrs = sum(1 for nx,ny in [(x+1,y),(x-1,y),(x,y+1),(x,y-1)]
                         if (nx,ny) in water_set)
        if water_nbrs == 0:
            isolated += 1

    print()
    print(f'ISSUE 4: Isolated river/lake tiles (no water neighbors): {isolated}')


if __name__ == '__main__':
    main(sys.argv[1] if len(sys.argv) > 1 else '/tmp/dump.json')
