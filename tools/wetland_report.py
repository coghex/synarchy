#!/usr/bin/env python3
"""Wetland-soil characterisation for a world dump.

Answers the question "is the muck-promotion arm worth implementing?" by
measuring, on a real dump, how much wetland soil already exists and how
many tiles the promotion arm WOULD convert (flat dry land in the
fresh-water halo that isn't already wetland soil).

Wetland soils (src/World/Material.hs): peat=62, mucky_peat=63, muck=64.
Promotion criterion mirrors `wetlandKeep` (Generate/Chunk.hs):
  flat  = max 4-neighbour |Δterrain| ≤ 2
  wet   = within the fresh-water halo (Chebyshev ≤ 3 of a lake/river tile)
The code also gates on `warm` (climate), which a dump can't see — so the
promotion-candidate count here is an UPPER BOUND; the warm gate only
shrinks it.

Usage:
  cabal run -v0 exe:synarchy -- --dump=terrain,material,fluid \\
      --seed 42 --worldSize 64 --region -32,-32,31,31 2>/dev/null > d.json
  python3 tools/wetland_report.py d.json
"""
import json
import sys
from collections import Counter

WETLAND = {62: "peat", 63: "mucky_peat", 64: "muck"}
SEA_LEVEL = 0
HALO = 3          # wtHaloRadius (Chebyshev)
FLAT_DELTA = 2    # wetlandKeep flat threshold
GLACIER = 250

MAT_NAMES = {  # for the "what would be overwritten" breakdown
    50: "clay", 54: "loamy_sand", 55: "sand", 56: "loam", 57: "clay_loam",
    58: "silty_clay", 59: "silt_loam", 60: "silt", 62: "peat",
    63: "mucky_peat", 64: "muck", 67: "salt_flat", 250: "glacier",
}


def load(path):
    raw = open(path).read()
    return json.loads(raw[raw.index("[{"):])


def main():
    if len(sys.argv) < 2:
        print(__doc__)
        sys.exit(1)
    data = load(sys.argv[1])
    idx = {(t["x"], t["y"]): t for t in data}

    def terr(t):
        return t.get("terrainZ", t.get("surfaceZ"))

    # fresh water = lake/river tiles
    fresh = {(t["x"], t["y"]) for t in data
             if t.get("fluidType") in ("lake", "river")}

    # tiles within Chebyshev HALO of any fresh-water tile
    near_fresh = set()
    for (x, y) in fresh:
        for dx in range(-HALO, HALO + 1):
            for dy in range(-HALO, HALO + 1):
                near_fresh.add((x + dx, y + dy))

    def is_land(t):
        return (t.get("fluidType") is None
                and terr(t) is not None and terr(t) > SEA_LEVEL
                and not t.get("beyondGlacier")
                and t.get("matId") != GLACIER)

    def is_flat(x, y, t):
        tz = terr(t)
        for dx, dy in ((1, 0), (-1, 0), (0, 1), (0, -1)):
            n = idx.get((x + dx, y + dy))
            if n is None or terr(n) is None:
                continue
            if abs(tz - terr(n)) > FLAT_DELTA:
                return False
        return True

    land = [(xy, t) for xy, t in idx.items() if is_land(t)]
    n_land = len(land) or 1

    wet_soil = [(xy, t) for xy, t in land if t.get("matId") in WETLAND]
    wet_near = [(xy, t) for xy, t in wet_soil if xy in near_fresh]

    # flat dry land tiles in the fresh-water halo
    flat_halo = [(xy, t) for (xy, t) in land
                 if xy in near_fresh and is_flat(xy[0], xy[1], t)]
    flat_halo_wet = [(xy, t) for (xy, t) in flat_halo
                     if t.get("matId") in WETLAND]
    # promotion candidates: flat halo land that ISN'T already wetland
    candidates = [(xy, t) for (xy, t) in flat_halo
                  if t.get("matId") not in WETLAND]

    def pct(n):
        return f"{100*n/n_land:.2f}%"

    print(f"=== wetland report: {sys.argv[1]} ===")
    print(f"land tiles (dry, above sea, non-glacier): {n_land}")
    print(f"fresh-water (lake/river) tiles:           {len(fresh)}")
    print()
    print("-- existing wetland soil --")
    soil_breakdown = Counter(WETLAND[t['matId']] for _, t in wet_soil)
    print(f"wetland soil tiles: {len(wet_soil)} ({pct(len(wet_soil))} of land)"
          f"  {dict(soil_breakdown)}")
    print(f"  of those, within fresh-water halo: {len(wet_near)} "
          f"({100*len(wet_near)/(len(wet_soil) or 1):.0f}% of wetland)")
    print()
    print("-- the riparian zone (flat dry land within Chebyshev 3 of "
          "lake/river) --")
    nf = len(flat_halo) or 1
    print(f"flat halo land tiles:        {len(flat_halo)}")
    print(f"  already wetland soil:      {len(flat_halo_wet)} "
          f"({100*len(flat_halo_wet)/nf:.0f}% of the zone)")
    print(f"  PROMOTION CANDIDATES:      {len(candidates)} "
          f"({100*len(candidates)/nf:.0f}% of the zone, {pct(len(candidates))} "
          f"of all land)")
    print("  (upper bound — the code's 'warm' climate gate would reduce this)")
    print()
    print("-- what promotion would overwrite --")
    cand_mats = Counter(MAT_NAMES.get(t["matId"], str(t["matId"]))
                        for _, t in candidates)
    for mat, c in cand_mats.most_common():
        print(f"    {mat:>12}: {c}")


if __name__ == "__main__":
    main()
