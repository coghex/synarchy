#!/usr/bin/env python3
"""
Detect river-lake visual gaps where rivers run parallel/around lakes
without merging properly.

Usage:
  cabal run exe:synarchy -- --dump=terrain,fluid --seed N --worldSize 32 --region -8,-8,8,8 \
      > /tmp/dump.json 2>/dev/null
  python3 tools/test_river_lake_gaps.py /tmp/dump.json
"""

import json
import sys
from collections import deque, Counter


def main(path):
    with open(path) as f:
        data = json.load(f)

    tile_map = {(t['x'], t['y']): t for t in data}
    river_set = set((t['x'], t['y']) for t in data if t.get('fluidType') == 'river')
    lake_set = set((t['x'], t['y']) for t in data if t.get('fluidType') == 'lake')
    ocean_set = set((t['x'], t['y']) for t in data if t.get('fluidType') == 'ocean')

    print(f'Rivers: {len(river_set)}, Lakes: {len(lake_set)}, Ocean: {len(ocean_set)}')

    # ISSUE 1: river adjacent to lake with surface diff > 1 (visual cliff)
    cliffs = []
    for x, y in river_set:
        rs = tile_map[(x, y)].get('fluidSurf', 0)
        for nx, ny in [(x+1,y),(x-1,y),(x,y+1),(x,y-1)]:
            if (nx, ny) in lake_set:
                ls = tile_map[(nx, ny)].get('fluidSurf', 0)
                diff = rs - ls
                if diff > 1:
                    cliffs.append((x, y, rs, nx, ny, ls, diff))

    print()
    print(f'ISSUE 1: River-lake cliffs (river surf > lake surf + 1)')
    print(f'  Total: {len(cliffs)}')
    if cliffs:
        diffs = Counter(c[6] for c in cliffs)
        for d in sorted(diffs.keys()):
            print(f'  diff={d}: {diffs[d]} tiles')

    # ISSUE 2: rivers running parallel to lakes (river chain near lake border
    # for many tiles)
    parallel_count = 0
    seen = set()
    for x, y in river_set:
        if (x, y) in seen:
            continue
        # Check if there's a chain of river tiles all adjacent to lake
        chain = set()
        q = deque([(x, y)])
        while q:
            p = q.popleft()
            if p in chain:
                continue
            # Is this river tile adjacent to a lake?
            adj_lake = any((p[0]+dx, p[1]+dy) in lake_set
                           for dx, dy in [(1,0),(-1,0),(0,1),(0,-1)])
            if not adj_lake:
                continue
            chain.add(p)
            for dx, dy in [(1,0),(-1,0),(0,1),(0,-1)]:
                nb = (p[0]+dx, p[1]+dy)
                if nb in river_set and nb not in chain:
                    q.append(nb)

        if len(chain) >= 5:
            # This is a river chain hugging a lake - check if it has cliffs
            chain_cliffs = sum(1 for cx, cy in chain
                               if any((cx+dx, cy+dy) in lake_set
                                      and tile_map[(cx, cy)].get('fluidSurf', 0)
                                          > tile_map[(cx+dx, cy+dy)].get('fluidSurf', 0) + 1
                                      for dx, dy in [(1,0),(-1,0),(0,1),(0,-1)]))
            if chain_cliffs >= 3:
                parallel_count += 1
                if parallel_count <= 5:
                    xs = [c[0] for c in chain]
                    ys = [c[1] for c in chain]
                    print()
                    print(f'PARALLEL RIVER #{parallel_count}: {len(chain)} tiles, '
                          f'{chain_cliffs} cliffs, '
                          f'bbox=({min(xs)}..{max(xs)},{min(ys)}..{max(ys)})')
        seen |= chain

    print()
    print(f'ISSUE 2: River chains running parallel to lakes (5+ tiles, 3+ cliffs)')
    print(f'  Total: {parallel_count}')

    # Worst examples
    if cliffs:
        print()
        print('Worst cliffs:')
        for c in sorted(cliffs, key=lambda x: -x[6])[:5]:
            print(f'  river({c[0]},{c[1]}) surf={c[2]} | lake({c[3]},{c[4]}) surf={c[5]} diff={c[6]}')


if __name__ == '__main__':
    main(sys.argv[1] if len(sys.argv) > 1 else '/tmp/dump.json')
