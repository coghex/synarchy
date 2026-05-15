#!/usr/bin/env python3
"""
Detect river mouth cliffs: river tiles directly adjacent to
ocean/lake where the surface difference > 1. These create
visible water walls at the river-body junction.
"""
import json, sys
from collections import Counter


def main(path):
    with open(path) as f:
        data = json.load(f)

    tile_map = {(t['x'], t['y']): t for t in data}
    river_set = set((t['x'], t['y']) for t in data if t.get('fluidType') == 'river')
    ocean_set = set((t['x'], t['y']) for t in data if t.get('fluidType') == 'ocean')
    lake_set = set((t['x'], t['y']) for t in data if t.get('fluidType') == 'lake')
    body_set = ocean_set | lake_set

    cliffs = []
    for rx, ry in river_set:
        rt = tile_map[(rx, ry)]
        rs = rt.get('fluidSurf', 0)
        rterr = rt.get('terrainZ', 0)
        for dx, dy in [(1, 0), (-1, 0), (0, 1), (0, -1)]:
            bx, by = rx + dx, ry + dy
            if (bx, by) in body_set:
                bt = tile_map[(bx, by)]
                bs = bt.get('fluidSurf', 0)
                diff = rs - bs
                if diff > 1:
                    cliffs.append((rx, ry, rs, rterr, bx, by,
                                   bt.get('fluidType'), bs, diff))
                    break

    print(f'River mouth cliffs (river adj body, diff > 1): {len(cliffs)}')
    if cliffs:
        diffs = Counter(c[8] for c in cliffs)
        print('  Diff distribution:')
        for d in sorted(diffs.keys())[:10]:
            print(f'    diff={d}: {diffs[d]}')
        print('  Examples:')
        for c in cliffs[:5]:
            print(f'    river({c[0]},{c[1]}) s={c[2]} t={c[3]} | '
                  f'{c[6]}({c[4]},{c[5]}) s={c[7]} diff={c[8]}')


if __name__ == '__main__':
    main(sys.argv[1] if len(sys.argv) > 1 else '/tmp/dump.json')
