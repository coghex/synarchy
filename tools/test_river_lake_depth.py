#!/usr/bin/env python3
"""
Detect rivers that "flow into the ground" at lake edges.

Pattern: river tile adjacent to lake tile where the river's water
surface is significantly BELOW the lake's water surface. This looks
visually like the river is "sinking" into the ground at the lake
edge (water drops from the lake's higher level to the river's lower).
"""
import json, sys
from collections import Counter


def main(path):
    with open(path) as f:
        data = json.load(f)

    tile_map = {(t['x'], t['y']): t for t in data}
    river_tiles = [t for t in data if t.get('fluidType') == 'river']
    lake_set = set((t['x'], t['y']) for t in data if t.get('fluidType') == 'lake')

    print(f'Rivers: {len(river_tiles)}, Lakes: {len(lake_set)}')

    # Find river tiles adjacent to lake where river_surf < lake_surf - 1
    sinking = []
    for t in river_tiles:
        x, y = t['x'], t['y']
        rs = t.get('fluidSurf', 0)
        for nx, ny in [(x+1,y),(x-1,y),(x,y+1),(x,y-1)]:
            if (nx, ny) in lake_set:
                ls = tile_map[(nx, ny)].get('fluidSurf', 0)
                if ls > rs + 1:
                    sinking.append((x, y, rs, t.get('terrainZ', 0),
                                    nx, ny, ls, tile_map[(nx, ny)].get('terrainZ', 0)))
                    break

    print(f'\nRiver tiles "sinking" into adjacent lake (lake_surf > river_surf + 1): {len(sinking)}')
    if sinking:
        diffs = Counter(ex[6] - ex[2] for ex in sinking)
        print('  Surface diff distribution:')
        for d in sorted(diffs.keys()):
            print(f'    lake-river={d}: {diffs[d]}')
        print('\n  Examples:')
        for ex in sinking[:10]:
            rx, ry, rs, rt, lx, ly, ls, lt = ex
            print(f'    river({rx},{ry}) s={rs} t={rt} | lake({lx},{ly}) s={ls} t={lt}')


if __name__ == '__main__':
    main(sys.argv[1] if len(sys.argv) > 1 else '/tmp/dump.json')
