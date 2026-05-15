#!/usr/bin/env python3
"""
Test for the river mouth gap: dry tiles between flowing river
water and a body of water (ocean/lake).

The visual pattern:
  - River water flows downhill, its surface slopes toward the coast
  - A 1-tile strip of dry terrain (grass) separates river from body
  - The body of water sits on the other side

Two categories:
  FILLABLE: terrain < adjacent river surface (water SHOULD flow here)
  BLOCKED:  terrain >= adjacent river surface (terrain prevents flow)

A fillable gap is a sim/mask bug (water should exist).
A blocked gap is a carving bug (terrain should be lower).

Usage:
  cabal run exe:synarchy -- --dump=terrain,fluid --seed N --worldSize 32 \\
      --region -3,-3,3,3 2>/dev/null | python3 tools/test_river_mouth_gap.py
"""

import json
import sys
from collections import Counter


def main():
    data = json.load(sys.stdin)
    tile_map = {(t['x'], t['y']): t for t in data}
    river_set = set((t['x'], t['y']) for t in data if t.get('fluidType') == 'river')
    ocean_set = set((t['x'], t['y']) for t in data if t.get('fluidType') == 'ocean')
    lake_set = set((t['x'], t['y']) for t in data if t.get('fluidType') == 'lake')
    body_set = ocean_set | lake_set
    mask_set = set((t['x'], t['y']) for t in data if t.get('riverMask'))

    fillable = []
    blocked = []

    for t in data:
        # Skip tiles that already have fluid
        if t.get('fluidType') is not None:
            continue

        x, y = t['x'], t['y']
        terr = t.get('terrainZ', 999)

        # Find adjacent river neighbors and their surfaces
        river_nbrs = []
        for dx, dy in [(1, 0), (-1, 0), (0, 1), (0, -1)]:
            nx, ny = x + dx, y + dy
            if (nx, ny) in river_set:
                rs = tile_map[(nx, ny)].get('fluidSurf', 0)
                river_nbrs.append((nx, ny, rs))

        if not river_nbrs:
            continue

        # Find adjacent or near-adjacent body neighbors
        body_nbrs = []
        for dx, dy in [(1, 0), (-1, 0), (0, 1), (0, -1)]:
            nx, ny = x + dx, y + dy
            if (nx, ny) in body_set:
                bs = tile_map[(nx, ny)].get('fluidSurf', 0)
                bt = tile_map[(nx, ny)].get('fluidType', '')
                body_nbrs.append((nx, ny, bs, bt))
        # Also check distance 2 for wider gaps
        if not body_nbrs:
            for dx in range(-2, 3):
                for dy in range(-2, 3):
                    if abs(dx) + abs(dy) != 2:
                        continue
                    nx, ny = x + dx, y + dy
                    if (nx, ny) in body_set:
                        bs = tile_map[(nx, ny)].get('fluidSurf', 0)
                        bt = tile_map[(nx, ny)].get('fluidType', '')
                        body_nbrs.append((nx, ny, bs, bt))
                        break
                if body_nbrs:
                    break

        if not body_nbrs:
            continue

        # This is a gap tile: dry, adj river, near body
        max_river_surf = max(rs for _, _, rs in river_nbrs)
        in_mask = (x, y) in mask_set

        entry = {
            'x': x, 'y': y, 'terr': terr,
            'river_surf': max_river_surf,
            'body_dist': 1 if any(abs(x-bx)+abs(y-by) == 1 for bx, by, _, _ in body_nbrs) else 2,
            'body_type': body_nbrs[0][3],
            'body_surf': body_nbrs[0][2],
            'in_mask': in_mask,
        }

        if terr < max_river_surf:
            fillable.append(entry)
        else:
            blocked.append(entry)

    print(f"River mouth gap analysis")
    print(f"========================")
    print(f"Total tiles: {len(data)}")
    print(f"Rivers: {len(river_set)}  Mask: {len(mask_set)}  "
          f"Ocean: {len(ocean_set)}  Lake: {len(lake_set)}")
    print()

    print(f"FILLABLE gaps (terrain < river surface — water should exist): {len(fillable)}")
    if fillable:
        in_mask_count = sum(1 for g in fillable if g['in_mask'])
        body_d1 = sum(1 for g in fillable if g['body_dist'] == 1)
        print(f"  In river mask: {in_mask_count}")
        print(f"  Body at distance 1: {body_d1}  distance 2: {len(fillable) - body_d1}")
        depths = Counter(g['river_surf'] - g['terr'] for g in fillable)
        print(f"  Depth distribution (river_surf - terrain):")
        for d in sorted(depths.keys()):
            print(f"    depth={d}: {depths[d]}")
        print(f"  Examples:")
        for g in fillable[:5]:
            print(f"    ({g['x']},{g['y']}) terr={g['terr']} river_s={g['river_surf']} "
                  f"{g['body_type']}_s={g['body_surf']} d={g['body_dist']} mask={g['in_mask']}")
    print()

    print(f"BLOCKED gaps (terrain >= river surface — needs carving): {len(blocked)}")
    if blocked:
        in_mask_count = sum(1 for g in blocked if g['in_mask'])
        body_d1 = sum(1 for g in blocked if g['body_dist'] == 1)
        print(f"  In river mask: {in_mask_count}")
        print(f"  Body at distance 1: {body_d1}  distance 2: {len(blocked) - body_d1}")
        print(f"  Examples:")
        for g in blocked[:5]:
            print(f"    ({g['x']},{g['y']}) terr={g['terr']} river_s={g['river_surf']} "
                  f"{g['body_type']}_s={g['body_surf']} d={g['body_dist']} mask={g['in_mask']}")


if __name__ == '__main__':
    main()
