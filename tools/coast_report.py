#!/usr/bin/env python3
"""Coastline variety report — quality/measurement tool (no bug gating).

The #220 instrument. Dumps worlds across seeds (terrain+material+fluid+ice)
and reports, per seed:

  - elevation-vs-distance-from-ocean profile: p10/median/p90 of land
    terrainZ at each BFS distance from ocean tiles. A monotonous
    beach-to-cliff world shows a tight band jumping to the plateau by
    d~10; variety shows a wide p10/p90 spread.
  - coast-form census: 16x16-tile windows containing shoreline are
    classified as cliff / steep / ramp / beach_plain / marsh, so
    "almost everything is short-beach-to-cliff" is measurable.
  - sheer-cliff population: windows whose near-shore land (d<=2)
    already stands >= 15z above the sea.
  - latitude bands derived from the coastal pass's normalized
    ``abs(x+y) / halfWorld`` coordinate. Temperate is [0.0, 0.4),
    high_latitude is [0.4, 0.7), polar_margin starts at 0.7 exactly where
    Coastal.hs's polar steepness bias begins, and glacier_zone starts at
    ``halfWorld - 16`` exactly where World.Plate.Glacier does. A coast
    window uses the median absolute latitude of its d<=2 land; a fjord uses
    its deepest (most inland) qualifying tile, so a cross-band feature has
    one deterministic owner.
  - fjord-like inlets: ocean-connected ocean/lake water whose narrow arm
    starts outside open water (open water has radius >= 4 tiles), penetrates
    at least 12 cardinal steps inland, is at most 6 tiles wide, is at least
    twice as long as wide, and has terrain >= 12z on both opposing walls.
    Connectivity follows the cylindrical u=x-y seam with period
    ``worldSize * 16``; wrapped candidate components are counted once.
  - glacial-coast signals over a separate shoreline-window population that
    excludes beyondGlacier but retains glacierZone: glacier-zone shoreline,
    matGlacier (dumped matId 250), any ice, basin ice, and drape ice.

Usage:
  python3 tools/coast_report.py                     # default seeds, w64
  python3 tools/coast_report.py --seeds 42,314 --worldSize 128
  python3 tools/coast_report.py --files a.json b.json

seaLevel is 0 in dump coordinates (terrainZ is height above sea).
``--worldSize`` governs both latitude and seam calculations for generated and
pre-dumped files. Older terrain/material/fluid files remain valid; their ice
signals are reported as unavailable rather than zero.
"""

import argparse
import json
import subprocess
import sys
from collections import defaultdict, deque

DEFAULT_SEEDS = [4, 7, 42, 99, 123, 555, 777, 1337]

CHUNK_SIZE = 16
MAX_DIST = 30
WINDOW = 16
MARSH_MATS = {62, 63, 64}
GLACIER_MAT_ID = 250

PROFILE_DISTS = [1, 2, 3, 4, 6, 8, 10, 14, 18, 22, 26, 30]

FORM_ORDER = ["cliff", "steep", "ramp", "beach_plain", "marsh"]
LATITUDE_BANDS = [
    "temperate", "high_latitude", "polar_margin", "glacier_zone"
]
COAST_WATER_TYPES = {"ocean", "lake"}

OPEN_WATER_RADIUS = 4
FJORD_MIN_PENETRATION = 12
FJORD_MAX_WIDTH = 6
FJORD_MIN_ASPECT = 2
FJORD_MIN_WALL_Z = 12


def run_dump(seed, world_size, plates=None):
    half = world_size // 2
    region = f"{-half},{-half},{half - 1},{half - 1}"
    cmd = [
        "cabal", "run", "-v0", "exe:synarchy", "--",
        "--dump=terrain,material,fluid,ice",
        "--seed", str(seed),
        "--worldSize", str(world_size),
        "--region", region,
    ]
    if plates is not None:
        cmd += ["--plates", str(plates)]
    print(f"  generating seed {seed} (w{world_size})...",
          file=sys.stderr, flush=True)
    out = subprocess.run(cmd, stdout=subprocess.PIPE,
                         stderr=subprocess.DEVNULL, check=True)
    return json.loads(out.stdout)


def percentile(sorted_vals, p):
    if not sorted_vals:
        return None
    k = min(len(sorted_vals) - 1, int(round(p * (len(sorted_vals) - 1))))
    return sorted_vals[k]


def latitude_band_from_abs_v(abs_v, world_size):
    """Assign one fixed, exhaustive band from the coastal pass latitude."""
    half_world = world_size * CHUNK_SIZE / 2
    if half_world <= 0:
        raise ValueError("worldSize must be positive")
    glacier_edge = half_world - CHUNK_SIZE
    ratio = abs_v / half_world
    if ratio < 0.4:
        return "temperate"
    if ratio < 0.7:
        return "high_latitude"
    if abs_v < glacier_edge:
        return "polar_margin"
    return "glacier_zone"


def latitude_band(xy, world_size):
    return latitude_band_from_abs_v(abs(xy[0] + xy[1]), world_size)


def bfs_dist_from_ocean(tiles):
    """Distance (4-adjacency, tile grid) from the nearest ocean tile.

    Returns {(x,y): dist} for non-ocean tiles within MAX_DIST.
    Glacier-zone / beyond-glacier tiles are excluded entirely (world
    edge, never eroded by the coastal pass).

    This is the original #220 metric and deliberately remains unwrapped so
    its historical values and output stay comparable.
    """
    grid = {}
    ocean = []
    for t in tiles:
        if t.get("glacierZone") or t.get("beyondGlacier"):
            continue
        xy = (t["x"], t["y"])
        grid[xy] = t
        if t.get("fluidType") == "ocean":
            ocean.append(xy)

    dist = {xy: 0 for xy in ocean}
    frontier = deque(ocean)
    while frontier:
        x, y = frontier.popleft()
        d = dist[(x, y)]
        if d >= MAX_DIST:
            continue
        for nxy in ((x + 1, y), (x - 1, y), (x, y + 1), (x, y - 1)):
            if nxy in grid and nxy not in dist:
                dist[nxy] = d + 1
                frontier.append(nxy)
    return grid, dist


def legacy_metrics(tiles, world_size):
    """Compute the pre-#1947 metrics plus an additive latitude split."""
    grid, dist = bfs_dist_from_ocean(tiles)

    by_d = defaultdict(list)
    for xy, d in dist.items():
        if d == 0:
            continue
        t = grid[xy]
        if t.get("fluidType") is None and t["terrainZ"] > 0:
            by_d[d].append(t["terrainZ"])
    for d in by_d:
        by_d[d].sort()

    windows = defaultdict(lambda: {
        "shore": 0,
        "marsh": 0,
        "z_near": [],
        "z_mid": [],
        "z_far": [],
        "z_edge": [],
        "shore_abs_v": [],
    })
    for xy, d in dist.items():
        if d == 0:
            continue
        t = grid[xy]
        if t.get("fluidType") is not None or t["terrainZ"] <= 0:
            continue
        w = (xy[0] // WINDOW, xy[1] // WINDOW)
        ww = windows[w]
        if d <= 2:
            ww["shore"] += 1
            ww["z_edge"].append(t["terrainZ"])
            ww["shore_abs_v"].append(abs(xy[0] + xy[1]))
        if d <= 8 and t.get("matId") in MARSH_MATS:
            ww["marsh"] += 1
        if 2 <= d <= 4:
            ww["z_near"].append(t["terrainZ"])
        elif 9 <= d <= 12:
            ww["z_mid"].append(t["terrainZ"])
        elif 18 <= d <= 22:
            ww["z_far"].append(t["terrainZ"])

    forms = defaultdict(int)
    forms_by_band = {
        band: defaultdict(int) for band in LATITUDE_BANDS
    }
    sheer = 0
    for ww in windows.values():
        if ww["shore"] < 4 or not ww["z_near"]:
            continue
        z_near = sorted(ww["z_near"])
        z_mid = sorted(ww["z_mid"])
        z_far = sorted(ww["z_far"])
        n = percentile(z_near, 0.5)
        m = percentile(z_mid, 0.5)
        f = percentile(z_far, 0.5)
        if ww["marsh"] >= 6:
            form = "marsh"
        elif n >= 12:
            form = "cliff"
        elif n >= 6:
            form = "steep"
        elif m is not None and m <= 5 and (f is None or f <= 8):
            form = "beach_plain"
        else:
            form = "ramp"
        forms[form] += 1
        median_abs_v = percentile(sorted(ww["shore_abs_v"]), 0.5)
        band = latitude_band_from_abs_v(median_abs_v, world_size)
        forms_by_band[band][form] += 1
        if percentile(sorted(ww["z_edge"]), 0.9) is not None \
                and percentile(sorted(ww["z_edge"]), 0.9) >= 15:
            sheer += 1

    return {
        "grid": grid,
        "dist": dist,
        "by_d": by_d,
        "forms": forms,
        "forms_by_band": forms_by_band,
        "total": sum(forms.values()),
        "sheer": sheer,
    }


def _neighbor_for_step(grid, xy, delta, world_size):
    """Resolve one cardinal neighbour through the cylindrical u seam."""
    raw = (xy[0] + delta[0], xy[1] + delta[1])
    if raw in grid:
        return raw
    alias_step = world_size * CHUNK_SIZE // 2
    for sign in (-1, 1):
        alias = (raw[0] + sign * alias_step,
                 raw[1] - sign * alias_step)
        if alias in grid:
            return alias
    return None


def wrapped_neighbor_steps(grid, xy, world_size):
    seen = set()
    for delta in ((1, 0), (-1, 0), (0, 1), (0, -1)):
        nxy = _neighbor_for_step(grid, xy, delta, world_size)
        if nxy is not None and nxy not in seen:
            seen.add(nxy)
            yield nxy, delta


def coastal_topology(tiles, world_size):
    """Build the physical shoreline population under the real u wrap."""
    grid = {
        (t["x"], t["y"]): t
        for t in tiles
        if not t.get("beyondGlacier")
    }
    sea_water = {
        xy for xy, tile in grid.items()
        if tile.get("fluidType") in COAST_WATER_TYPES
    }
    ocean = {
        xy for xy in sea_water
        if grid[xy].get("fluidType") == "ocean"
    }
    connected = set(ocean)
    frontier = deque(sorted(ocean))
    while frontier:
        xy = frontier.popleft()
        for nxy, _ in wrapped_neighbor_steps(grid, xy, world_size):
            if nxy in sea_water and nxy not in connected:
                connected.add(nxy)
                frontier.append(nxy)
    return {
        "grid": grid,
        "sea_water": sea_water,
        "ocean_connected": connected,
    }


def _water_radii(topology, world_size):
    """Cardinal distance into ocean-connected water from explicit land."""
    grid = topology["grid"]
    connected = topology["ocean_connected"]
    shore_water = []
    for xy in connected:
        for nxy, _ in wrapped_neighbor_steps(grid, xy, world_size):
            tile = grid[nxy]
            if tile.get("fluidType") is None and tile.get("terrainZ", 0) > 0:
                shore_water.append(xy)
                break

    radius = {xy: 1 for xy in shore_water}
    frontier = deque(sorted(radius))
    while frontier:
        xy = frontier.popleft()
        for nxy, _ in wrapped_neighbor_steps(grid, xy, world_size):
            if nxy in connected and nxy not in radius:
                radius[nxy] = radius[xy] + 1
                frontier.append(nxy)
    return radius


def _penetration_from_open_water(topology, world_size):
    grid = topology["grid"]
    connected = topology["ocean_connected"]
    radius = _water_radii(topology, world_size)
    open_water = {
        xy for xy in connected
        if grid[xy].get("fluidType") == "ocean"
        and radius.get(xy, 0) >= OPEN_WATER_RADIUS
    }
    penetration = {xy: 0 for xy in open_water}
    parent_axis = {}
    frontier = deque(sorted(open_water))
    while frontier:
        xy = frontier.popleft()
        for nxy, delta in wrapped_neighbor_steps(grid, xy, world_size):
            if nxy in connected and nxy not in penetration:
                penetration[nxy] = penetration[xy] + 1
                parent_axis[nxy] = delta
                frontier.append(nxy)
    return penetration, parent_axis


def _find_wall(grid, xy, delta, world_size):
    current = xy
    for distance in range(1, FJORD_MAX_WIDTH + 2):
        current = _neighbor_for_step(grid, current, delta, world_size)
        if current is None:
            return None
        tile = grid[current]
        if tile.get("fluidType") in COAST_WATER_TYPES:
            continue
        if tile.get("fluidType") is not None or tile.get("terrainZ", 0) <= 0:
            return None
        return distance, tile["terrainZ"]
    return None


def _fjord_cross_section(grid, xy, axis, world_size):
    perpendicular = (0, 1) if axis[0] else (1, 0)
    left = _find_wall(grid, xy, perpendicular, world_size)
    right = _find_wall(grid, xy,
                       (-perpendicular[0], -perpendicular[1]), world_size)
    if left is None or right is None:
        return None
    width = left[0] + right[0] - 1
    return width, left[1], right[1]


def _wrapped_components(points, grid, world_size):
    remaining = set(points)
    components = []
    while remaining:
        start = min(remaining)
        remaining.remove(start)
        component = {start}
        frontier = deque([start])
        while frontier:
            xy = frontier.popleft()
            for nxy, _ in wrapped_neighbor_steps(grid, xy, world_size):
                if nxy in remaining:
                    remaining.remove(nxy)
                    component.add(nxy)
                    frontier.append(nxy)
        components.append(component)
    return components


def fjord_census(topology, world_size):
    """Return one record per wrapped, ocean-connected fjord-like inlet."""
    grid = topology["grid"]
    penetration, parent_axis = _penetration_from_open_water(
        topology, world_size)
    qualifying = {}
    for xy, length in penetration.items():
        if length < FJORD_MIN_PENETRATION or xy not in parent_axis:
            continue
        section = _fjord_cross_section(
            grid, xy, parent_axis[xy], world_size)
        if section is None:
            continue
        width, left_z, right_z = section
        if width > FJORD_MAX_WIDTH:
            continue
        if length < FJORD_MIN_ASPECT * width:
            continue
        if min(left_z, right_z) < FJORD_MIN_WALL_Z:
            continue
        qualifying[xy] = width

    inlets = []
    for component in _wrapped_components(qualifying, grid, world_size):
        deepest = max(component, key=lambda xy: (penetration[xy], xy))
        inlets.append({
            "tip": deepest,
            "penetration": penetration[deepest],
            "width": qualifying[deepest],
            "band": latitude_band(deepest, world_size),
        })
    return sorted(inlets, key=lambda inlet: inlet["tip"])


def glacial_coast_census(topology, world_size):
    """Aggregate glacial signals over ocean-connected shoreline windows."""
    grid = topology["grid"]
    connected = topology["ocean_connected"]
    ice_available = any(
        "iceSurf" in tile or "iceMode" in tile for tile in grid.values())
    windows = defaultdict(lambda: {
        "shore_abs_v": [],
        "glacier_zone": False,
        "glacier_material": False,
        "ice": False,
        "basin": False,
        "drape": False,
    })

    for water_xy in connected:
        water = grid[water_xy]
        for land_xy, _ in wrapped_neighbor_steps(
                grid, water_xy, world_size):
            land = grid[land_xy]
            if land.get("fluidType") is not None \
                    or land.get("terrainZ", 0) <= 0:
                continue
            ww = windows[(land_xy[0] // WINDOW, land_xy[1] // WINDOW)]
            ww["shore_abs_v"].append(abs(land_xy[0] + land_xy[1]))
            pair = (land, water)
            ww["glacier_zone"] |= any(
                tile.get("glacierZone", False) for tile in pair)
            ww["glacier_material"] |= any(
                tile.get("matId") == GLACIER_MAT_ID for tile in pair)
            ww["ice"] |= any(tile.get("iceSurf") is not None for tile in pair)
            ww["basin"] |= any(
                tile.get("iceMode") == "basin" for tile in pair)
            ww["drape"] |= any(
                tile.get("iceMode") == "drape" for tile in pair)

    bands = {
        band: {
            "total": 0,
            "glacier_zone": 0,
            "glacier_material": 0,
            "ice": 0,
            "basin": 0,
            "drape": 0,
        }
        for band in LATITUDE_BANDS
    }
    for ww in windows.values():
        median_abs_v = percentile(sorted(ww["shore_abs_v"]), 0.5)
        band = latitude_band_from_abs_v(median_abs_v, world_size)
        stats = bands[band]
        stats["total"] += 1
        for signal in ("glacier_zone", "glacier_material",
                       "ice", "basin", "drape"):
            stats[signal] += int(ww[signal])
    return {"ice_available": ice_available, "bands": bands}


def _distribution(values):
    if not values:
        return "-/-/-"
    ordered = sorted(values)
    return f"{ordered[0]}/{percentile(ordered, 0.5)}/{ordered[-1]}"


def _percent(count, total):
    return 0.0 if total == 0 else 100 * count / total


def print_latitude_forms(forms_by_band):
    print("  coast forms by latitude "
          "(window band = median |x+y| of d<=2 land):")
    for band in LATITUDE_BANDS:
        forms = forms_by_band[band]
        total = sum(forms.values())
        details = "  ".join(
            f"{form}={forms[form]:4d} ({_percent(forms[form], total):4.1f}%)"
            for form in FORM_ORDER)
        print(f"    {band:13s} windows={total:4d}  |  {details}")


def print_fjords(inlets):
    print("  fjord inlets: {}  |  penetration[min/median/max]={}  "
          "width[min/median/max]={}".format(
              len(inlets),
              _distribution([inlet["penetration"] for inlet in inlets]),
              _distribution([inlet["width"] for inlet in inlets])))
    counts = defaultdict(int)
    for inlet in inlets:
        counts[inlet["band"]] += 1
    print("    by latitude: " + "  ".join(
        f"{band}={counts[band]}" for band in LATITUDE_BANDS))


def print_glacial_coasts(census):
    print("  glacial coast windows "
          "(separate ocean-connected shoreline population):")
    for band in LATITUDE_BANDS:
        stats = census["bands"][band]
        total = stats["total"]
        prefix = (
            f"    {band:13s} windows={total:4d}  |  "
            f"glacier_zone={stats['glacier_zone']:4d} "
            f"({_percent(stats['glacier_zone'], total):4.1f}%)  "
            f"glacier_material={stats['glacier_material']:4d} "
            f"({_percent(stats['glacier_material'], total):4.1f}%)")
        if census["ice_available"]:
            print(prefix
                  + f"  ice={stats['ice']:4d} "
                    f"({_percent(stats['ice'], total):4.1f}%)"
                  + f"  basin={stats['basin']:4d} "
                    f"({_percent(stats['basin'], total):4.1f}%)"
                  + f"  drape={stats['drape']:4d} "
                    f"({_percent(stats['drape'], total):4.1f}%)")
        else:
            print(prefix + "  ice=unavailable  basin=unavailable  "
                  "drape=unavailable")


def analyze(tiles, label, world_size):
    legacy = legacy_metrics(tiles, world_size)
    forms = legacy["forms"]
    total = legacy["total"]

    print(f"\n=== {label} ===")
    if total == 0:
        print("  NO COASTAL WINDOWS FOUND")
    else:
        frac = {k: forms[k] / total for k in FORM_ORDER}
        print("  coast windows: {}  |  ".format(total) + "  ".join(
            f"{k}={forms[k]:4d} ({100 * frac[k]:4.1f}%)"
            for k in FORM_ORDER))
        print("  sheer-cliff windows (p90 z@d<=2 >= 15): "
              f"{legacy['sheer']}")
        by_d = legacy["by_d"]
        print("  dist:   " + " ".join(f"{d:5d}" for d in PROFILE_DISTS))
        print("  p10:    " + " ".join(
            f"{percentile(by_d.get(d, []), 0.1):5d}"
            if by_d.get(d) else "    -" for d in PROFILE_DISTS))
        print("  median: " + " ".join(
            f"{percentile(by_d.get(d, []), 0.5):5d}"
            if by_d.get(d) else "    -" for d in PROFILE_DISTS))
        print("  p90:    " + " ".join(
            f"{percentile(by_d.get(d, []), 0.9):5d}"
            if by_d.get(d) else "    -" for d in PROFILE_DISTS))

    topology = coastal_topology(tiles, world_size)
    print_latitude_forms(legacy["forms_by_band"])
    print_fjords(fjord_census(topology, world_size))
    print_glacial_coasts(glacial_coast_census(topology, world_size))
    return forms, total


def main():
    ap = argparse.ArgumentParser(description="Coastline variety report")
    ap.add_argument("--seeds", default=None,
                    help="comma-separated seeds (default: canonical 8)")
    ap.add_argument("--worldSize", type=int, default=64,
                    help="world width in chunks; also governs --files topology")
    ap.add_argument("--plates", type=int, default=None)
    ap.add_argument("--files", nargs="*", default=None,
                    help="analyze pre-dumped JSON files instead of generating")
    args = ap.parse_args()

    if args.files:
        runs = [(f, json.load(open(f))) for f in args.files]
    else:
        seeds = ([int(s) for s in args.seeds.split(",")]
                 if args.seeds else DEFAULT_SEEDS)
        runs = [(f"seed {s}", run_dump(s, args.worldSize, args.plates))
                for s in seeds]

    agg = defaultdict(int)
    grand = 0
    for label, tiles in runs:
        forms, total = analyze(tiles, label, args.worldSize)
        for k, v in forms.items():
            agg[k] += v
        grand += total

    print(f"\n=== SUMMARY ({len(runs)} worlds, {grand} coast windows) ===")
    if grand == 0:
        sys.exit(0)
    for k in FORM_ORDER:
        print(f"  {k:12s} {agg[k]:5d}  ({100 * agg[k] / grand:5.1f}%)")


if __name__ == "__main__":
    main()
