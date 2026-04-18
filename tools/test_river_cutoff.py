#!/usr/bin/env python3
"""
Detect dry tiles that sit BETWEEN river and ocean/lake, causing
visible river cutoff gaps. This is the core visual issue: rivers
end before reaching the water body, with grass visible in between.

A gap tile is: dry terrain with BOTH river and ocean/lake within
Manhattan distance 3, where the tile's terrain is below the river
surface (water SHOULD exist there but doesn't).
"""
import json, sys
from collections import deque


def main(path):
    with open(path) as f:
        data = json.load(f)

    tile_map = {(t['x'], t['y']): t for t in data}
    river_set = set((t['x'], t['y']) for t in data if t.get('fluidType') == 'river')
    ocean_set = set((t['x'], t['y']) for t in data if t.get('fluidType') == 'ocean')
    lake_set = set((t['x'], t['y']) for t in data if t.get('fluidType') == 'lake')
    body_set = ocean_set | lake_set
    water_set = river_set | body_set

    gap_tiles = []
    for t in data:
        x, y = t['x'], t['y']
        if (x, y) in water_set:
            continue
        terr = t.get('terrainZ', 999)

        # Find closest river and closest body within 3 tiles
        river_info = None  # (dist, surface)
        body_info = None   # (dist, surface, type)
        for d in range(1, 4):
            for dx in range(-d, d + 1):
                dy_abs = d - abs(dx)
                for dy in ([dy_abs, -dy_abs] if dy_abs > 0 else [0]):
                    nx, ny = x + dx, y + dy
                    if river_info is None and (nx, ny) in river_set:
                        rt = tile_map[(nx, ny)]
                        river_info = (d, rt.get('fluidSurf', 0))
                    if body_info is None and (nx, ny) in body_set:
                        bt = tile_map[(nx, ny)]
                        body_info = (d, bt.get('fluidSurf', 0), bt.get('fluidType'))

        if river_info and body_info:
            rdist, rsurf = river_info
            bdist, bsurf, btype = body_info
            # Is this tile's terrain below the higher water surface?
            # If so, water SHOULD flow here but doesn't.
            max_surf = max(rsurf, bsurf)
            if terr < max_surf:
                gap_tiles.append((x, y, terr, rdist, rsurf, bdist, bsurf, btype))

    print(f'Total tiles: {len(data)}, River: {len(river_set)}, '
          f'Ocean: {len(ocean_set)}, Lake: {len(lake_set)}')
    print(f'\nDry gap tiles (river AND body within 3, terrain < water surface): '
          f'{len(gap_tiles)}')

    if gap_tiles:
        # Group by gap severity (terrain vs water surface)
        from collections import Counter
        dists = Counter((g[3], g[5]) for g in gap_tiles)
        print('  By (river_dist, body_dist):')
        for k in sorted(dists.keys()):
            print(f'    r={k[0]} b={k[1]}: {dists[k]}')

        print('\n  Examples (first 10):')
        for g in gap_tiles[:10]:
            x, y, terr, rd, rs, bd, bs, bt = g
            print(f'    ({x},{y}) terr={terr} | river d={rd} s={rs} | '
                  f'{bt} d={bd} s={bs} | gap={max(rs,bs)-terr}')

        # Check chunk boundaries
        chunk_boundary = sum(1 for g in gap_tiles
                             if g[0] % 16 == 0 or g[0] % 16 == 15
                             or g[1] % 16 == 0 or g[1] % 16 == 15)
        print(f'\n  At chunk boundary: {chunk_boundary} '
              f'({100*chunk_boundary//max(1,len(gap_tiles))}%)')


if __name__ == '__main__':
    main(sys.argv[1] if len(sys.argv) > 1 else '/tmp/dump.json')
