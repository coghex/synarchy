"""Region and topology checks for the world audit (#2224).

The seven checks that judge a tile against a whole neighbourhood or a
connected component: ocean connectivity, lake holes, submerged bumps,
isolated fluid, and small dry clusters inside water. `CHECKS` is this
owner's inventory; `tools/world_audit.py` composes it, with the other
owners', into `ALL_CHECKS`.

Imports the shared core and the standard library only: never a sibling
check owner, the classification policy, or the façade.
"""
from __future__ import annotations

from typing import Any

from world_audit_core import INT64_MIN, SEA_LEVEL, Issue, neighbors4


def check_dry_below_sea(grid: dict[tuple[int, int], dict[str, Any]],
                        issues: list[Issue]) -> None:
    """Dry tiles at or below sea level that are ocean-connected.

    Inland basins and below-sea cave systems are legitimate — a dry
    sub-sea tile is only a bug if it sits in a region that should be
    ocean. We classify "should be ocean" as: connected (via cardinal
    neighbors through other dry-or-ocean below-sea tiles) to a tile
    flagged with fluidType=ocean.

    Closed inland depressions that happen to dip below sea level are
    ignored — they're either dry valleys (rain-shadow inland) or
    represent caves the player would explore.
    """
    # Seed BFS from every ocean tile, expanding through any tile whose
    # surface is at or below sea level. We don't care if the expanding
    # tile is currently dry — the question is whether it CAN be reached
    # from the ocean at the seaLevel water plane.
    from collections import deque
    ocean_seeds = [(t["x"], t["y"]) for t in grid.values()
                   if t.get("fluidType") == "ocean"]
    reachable: set[tuple[int, int]] = set(ocean_seeds)
    queue = deque(ocean_seeds)
    while queue:
        x, y = queue.popleft()
        for nx, ny in neighbors4(x, y):
            if (nx, ny) in reachable:
                continue
            n = grid.get((nx, ny))
            if n is None:
                continue
            if n.get("beyondGlacier") or n["terrainZ"] <= INT64_MIN + 1:
                continue
            # Tile is ocean-reachable if its terrain is at/below sea level
            # (water at seaLevel could be there) OR if it's already ocean.
            if n["terrainZ"] <= SEA_LEVEL or n.get("fluidType") == "ocean":
                reachable.add((nx, ny))
                queue.append((nx, ny))

    for tile in grid.values():
        if tile["fluidType"] is not None:
            continue
        terr = tile["terrainZ"]
        if tile.get("beyondGlacier") or terr <= INT64_MIN + 1:
            continue
        if terr > SEA_LEVEL:
            continue
        if (tile["x"], tile["y"]) not in reachable:
            continue  # inland basin or sub-sea cave — legitimate dry tile
        issues.append(Issue(
            "DRY_BELOW_SEA", tile["x"], tile["y"],
            f"terrainZ={terr} surfaceZ={tile['surfaceZ']} (ocean-connected)",
        ))


def check_island_1tile(grid: dict[tuple[int, int], dict[str, Any]],
                       issues: list[Issue]) -> None:
    """Single dry tiles fully surrounded by ocean."""
    for (x, y), tile in grid.items():
        if tile["fluidType"] is not None:
            continue
        if tile.get("beyondGlacier") or tile["terrainZ"] <= INT64_MIN + 1:
            continue
        nbrs = []
        for nx, ny in neighbors4(x, y):
            n = grid.get((nx, ny))
            if n is None:
                continue
            nbrs.append(n)
        if len(nbrs) < 4:
            continue
        if all(n["fluidType"] == "ocean" for n in nbrs):
            issues.append(Issue(
                "ISLAND_1TILE", x, y,
                f"terrainZ={tile['terrainZ']} (surrounded by ocean)",
            ))


def check_lake_hole(grid: dict[tuple[int, int], dict[str, Any]],
                    issues: list[Issue]) -> None:
    """Single dry tiles fully surrounded by lake water whose terrain
    sits BELOW the lake surface — a genuine "hole" the lake failed to
    fill by equilibration. A tile whose terrain is AT (==) or above the
    surface is a flush islet/shoal poking up to the waterline, which is
    legitimate, not a defect — those are excluded to avoid false
    positives (a terrain bump that sits below water with only 3 water
    neighbours is still caught by check_submerged_bump)."""
    for (x, y), tile in grid.items():
        if tile["fluidType"] is not None:
            continue
        if tile.get("beyondGlacier") or tile["terrainZ"] <= INT64_MIN + 1:
            continue
        nbrs = []
        for nx, ny in neighbors4(x, y):
            n = grid.get((nx, ny))
            if n is None:
                continue
            nbrs.append(n)
        if len(nbrs) < 4:
            continue
        if all(n["fluidType"] == "lake" for n in nbrs):
            # A genuine hole is terrain below the LOWEST surrounding lake
            # surface — water at that level would still flood it. If the
            # terrain reaches any neighbour's waterline it's a flush
            # islet/shore/saddle, not a hole. Compare against all four
            # surfaces collectively so the result is independent of
            # neighbour ordering (mirrors check_submerged_bump's min()).
            surfs = [n["fluidSurf"] for n in nbrs if n["fluidSurf"] is not None]
            if not surfs:
                continue
            min_surf = min(surfs)
            if tile["terrainZ"] >= min_surf:
                continue
            issues.append(Issue(
                "LAKE_HOLE", x, y,
                f"terrainZ={tile['terrainZ']} surrounded by lake (minSurf={min_surf})",
            ))


def check_submerged_bump(grid: dict[tuple[int, int], dict[str, Any]],
                         issues: list[Issue]) -> None:
    """Dry tiles whose terrain is BELOW all surrounding water surfaces.
    These should be underwater but aren't — visible as terrain bumps
    poking through the water."""
    for (x, y), tile in grid.items():
        if tile["fluidType"] is not None:
            continue
        if tile.get("beyondGlacier") or tile["terrainZ"] <= INT64_MIN + 1:
            continue
        nbrs = []
        for nx, ny in neighbors4(x, y):
            n = grid.get((nx, ny))
            if n is None:
                continue
            nbrs.append(n)
        if len(nbrs) < 4:
            continue
        water_nbrs = [n for n in nbrs if n["fluidType"] is not None
                      and n["fluidSurf"] is not None]
        # Only flag if ≥3 water neighbors AND terrain is below all of them
        if len(water_nbrs) >= 3:
            min_water = min(n["fluidSurf"] for n in water_nbrs)
            if tile["terrainZ"] < min_water:
                types = sorted(set(n["fluidType"] for n in water_nbrs))
                issues.append(Issue(
                    "SUBMERGED_BUMP", x, y,
                    f"terrainZ={tile['terrainZ']} < min water surf {min_water} "
                    f"({len(water_nbrs)} water nbrs: {','.join(types)})",
                ))


def check_isolated_fluid(grid: dict[tuple[int, int], dict[str, Any]],
                         issues: list[Issue]) -> None:
    """Single non-ocean fluid tiles fully surrounded by dry tiles."""
    for (x, y), tile in grid.items():
        if tile["fluidType"] not in ("river", "lake", "lava"):
            continue
        nbrs = []
        for nx, ny in neighbors4(x, y):
            n = grid.get((nx, ny))
            if n is None:
                continue
            nbrs.append(n)
        if len(nbrs) < 4:
            continue
        if all(n["fluidType"] is None for n in nbrs):
            issues.append(Issue(
                "ISOLATED_FLUID", x, y,
                f"{tile['fluidType']} surf={tile['fluidSurf']} surrounded by dry",
            ))


def check_multi_island(grid: dict[tuple[int, int], dict[str, Any]],
                       issues: list[Issue]) -> None:
    """Small clusters (≤4 tiles) of dry tiles fully surrounded by
    water (any type). These are dry "islands" inside what should be
    a contiguous water body."""
    visited: set[tuple[int, int]] = set()
    max_size = 4
    for (x, y), tile in grid.items():
        if (x, y) in visited:
            continue
        if tile["fluidType"] is not None:
            continue
        if tile.get("beyondGlacier") or tile["terrainZ"] <= INT64_MIN + 1:
            continue
        # BFS to find dry cluster
        cluster = set()
        queue = [(x, y)]
        bounded = True
        while queue:
            cx, cy = queue.pop()
            if (cx, cy) in cluster:
                continue
            ct = grid.get((cx, cy))
            if ct is None:
                bounded = False
                continue
            if ct["fluidType"] is not None:
                continue
            if ct.get("beyondGlacier") or ct["terrainZ"] <= INT64_MIN + 1:
                bounded = False
                continue
            cluster.add((cx, cy))
            if len(cluster) > max_size:
                break
            for ncx, ncy in neighbors4(cx, cy):
                if (ncx, ncy) not in cluster:
                    queue.append((ncx, ncy))
        visited.update(cluster)
        if not bounded or len(cluster) > max_size or len(cluster) < 1:
            continue
        # Check that the entire boundary is water
        boundary_is_water = True
        boundary_water_types: set[str] = set()
        for cx, cy in cluster:
            for ncx, ncy in neighbors4(cx, cy):
                if (ncx, ncy) in cluster:
                    continue
                n = grid.get((ncx, ncy))
                if n is None or n["fluidType"] is None:
                    boundary_is_water = False
                    break
                boundary_water_types.add(n["fluidType"])
            if not boundary_is_water:
                break
        if boundary_is_water and len(cluster) > 1:
            cx, cy = sorted(cluster)[0]
            issues.append(Issue(
                "MULTI_ISLAND", cx, cy,
                f"size={len(cluster)} dry cluster surrounded by "
                f"{','.join(sorted(boundary_water_types))}",
            ))


def check_flat_isolated_water(grid: dict[tuple[int, int], dict[str, Any]],
                              issues: list[Issue]) -> None:
    """Water tile on approximately flat terrain with 0 or 1 water
    neighbors — water that should flow or drain but is stuck as a
    tiny pocket. Distinct from ISOLATED_FLUID (0 water nbrs): also
    catches 1-water-nbr pairs sitting on flat land."""
    for (x, y), tile in grid.items():
        if tile["fluidType"] not in ("river", "lake"):
            continue
        wsurf = tile["fluidSurf"]
        if wsurf is None:
            continue
        nbrs = []
        for nx, ny in neighbors4(x, y):
            n = grid.get((nx, ny))
            if n is None:
                continue
            nbrs.append(n)
        if len(nbrs) < 4:
            continue
        water_count = sum(1 for n in nbrs if n["fluidType"] is not None)
        if water_count > 1:
            continue
        # Check if terrain is approximately flat around this tile
        terr = tile["terrainZ"]
        nbr_terrs = [n["terrainZ"] for n in nbrs
                     if not n.get("beyondGlacier")
                     and n["terrainZ"] > INT64_MIN + 1]
        if not nbr_terrs:
            continue
        terr_range = max(nbr_terrs) - min(nbr_terrs)
        if terr_range <= 2:  # approximately flat
            issues.append(Issue(
                "FLAT_ISOLATED_WATER", x, y,
                f"{tile['fluidType']} surf={wsurf} terr={terr} "
                f"water_nbrs={water_count} terr_range={terr_range}",
            ))


#: This owner's checks, keyed by the registry key each carries in
#: `world_audit.ALL_CHECKS`. The façade composes that registry from
#: every owner's inventory; nothing here decides the run order.
CHECKS = {
    "DRY_BELOW_SEA": check_dry_below_sea,
    "ISLAND_1TILE": check_island_1tile,
    "LAKE_HOLE": check_lake_hole,
    "SUBMERGED_BUMP": check_submerged_bump,
    "MULTI_ISLAND": check_multi_island,
    "FLAT_ISOLATED_WATER": check_flat_isolated_water,
    "ISOLATED_FLUID": check_isolated_fluid,
}
