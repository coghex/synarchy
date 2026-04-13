#!/usr/bin/env python3
"""World generation audit tool.

Runs the synarchy --dump command (or reads a pre-generated dump) and
categorizes any anomalies found in the world data.

Output is structured JSON, sorted for stable diffing against baselines.
"""

from __future__ import annotations

import argparse
import json
import math
import statistics
import subprocess
import sys
from collections import Counter
from dataclasses import dataclass, field
from pathlib import Path
from typing import Any

# ----- Constants -----------------------------------------------------------

SEA_LEVEL = 0  # Must match World/Constants.hs
CHUNK_SIZE = 16  # Must match World/Chunk/Types.hs
INT64_MIN = -(2**63)

# Threshold for considering a terrain difference a "spike" or "pit"
SPIKE_THRESHOLD = 15

# Threshold for "floating" non-ocean fluid (surface much higher than terrain)
FLOATING_FLUID_DEPTH = 15

# Threshold for "ocean on land" (cascade bug)
OCEAN_ON_LAND_THRESHOLD = SEA_LEVEL + 5

# Threshold for "river mouth drop"
RIVER_MOUTH_DROP_THRESHOLD = 5


# ----- Data types ----------------------------------------------------------

@dataclass
class Issue:
    category: str
    x: int
    y: int
    details: str

    def to_dict(self) -> dict[str, Any]:
        return {"x": self.x, "y": self.y, "details": self.details}


@dataclass
class AuditResult:
    seed: int | None = None
    world_size: int | None = None
    region: tuple[int, int, int, int] | None = None
    tile_count: int = 0
    fluid_stats: dict[str, int] = field(default_factory=dict)
    elevation_stats: dict[str, Any] = field(default_factory=dict)
    issues: list[Issue] = field(default_factory=list)

    def summary(self) -> dict[str, int]:
        counts: Counter[str] = Counter()
        for issue in self.issues:
            counts[issue.category] += 1
        return dict(sorted(counts.items()))

    def to_dict(self) -> dict[str, Any]:
        # Sort issues for stable output: by category, then by (x, y)
        sorted_issues = sorted(self.issues, key=lambda i: (i.category, i.x, i.y))
        # Group by category
        grouped: dict[str, list[dict[str, Any]]] = {}
        for issue in sorted_issues:
            grouped.setdefault(issue.category, []).append(issue.to_dict())
        return {
            "seed": self.seed,
            "worldSize": self.world_size,
            "region": list(self.region) if self.region else None,
            "tileCount": self.tile_count,
            "fluidStats": dict(sorted(self.fluid_stats.items(),
                                       key=lambda kv: (kv[0] is None, kv[0] or ""))),
            "elevationStats": self.elevation_stats,
            "summary": self.summary(),
            "issues": grouped,
        }


# ----- Tile helpers --------------------------------------------------------

def chunk_of(v: int) -> int:
    """Floor-divide tile coord by chunk size to get chunk coord.

    Must match Haskell's `floorDiv` for negative values.
    """
    return math.floor(v / CHUNK_SIZE)


def crosses_chunk_boundary(x1: int, y1: int, x2: int, y2: int) -> bool:
    return chunk_of(x1) != chunk_of(x2) or chunk_of(y1) != chunk_of(y2)


def neighbors4(x: int, y: int) -> list[tuple[int, int]]:
    return [(x - 1, y), (x + 1, y), (x, y - 1), (x, y + 1)]


# ----- Audit checks --------------------------------------------------------

def check_dry_below_sea(grid: dict[tuple[int, int], dict[str, Any]],
                        issues: list[Issue]) -> None:
    """Tiles with terrainZ ≤ seaLevel but no fluid."""
    for tile in grid.values():
        if tile["fluidType"] is not None:
            continue
        terr = tile["terrainZ"]
        # Skip beyondGlacier sentinel and minBound leakage (handled separately)
        if tile.get("beyondGlacier") or terr <= INT64_MIN + 1:
            continue
        if terr <= SEA_LEVEL:
            issues.append(Issue(
                "DRY_BELOW_SEA", tile["x"], tile["y"],
                f"terrainZ={terr} surfaceZ={tile['surfaceZ']}",
            ))


def check_ocean_on_land(grid: dict[tuple[int, int], dict[str, Any]],
                        issues: list[Issue]) -> None:
    """Ocean tiles whose terrain is well above sea level (cascade bug)."""
    for tile in grid.values():
        if tile["fluidType"] != "ocean":
            continue
        if tile["terrainZ"] > OCEAN_ON_LAND_THRESHOLD:
            issues.append(Issue(
                "OCEAN_ON_LAND", tile["x"], tile["y"],
                f"terrainZ={tile['terrainZ']} (ocean fluid surface={tile['fluidSurf']})",
            ))


def check_fluid_under_terrain(grid: dict[tuple[int, int], dict[str, Any]],
                              issues: list[Issue]) -> None:
    """River/lake tiles where the water surface is below the terrain."""
    for tile in grid.values():
        ft = tile["fluidType"]
        if ft not in ("river", "lake"):
            continue
        if tile["fluidSurf"] is None:
            continue
        if tile["fluidSurf"] < tile["terrainZ"]:
            cat = "RIVER_UNDER_TERRAIN" if ft == "river" else "LAKE_UNDER_TERRAIN"
            issues.append(Issue(
                cat, tile["x"], tile["y"],
                f"terrainZ={tile['terrainZ']} > fluidSurf={tile['fluidSurf']}",
            ))


def check_floating_fluid(grid: dict[tuple[int, int], dict[str, Any]],
                         issues: list[Issue]) -> None:
    """Non-ocean fluid tiles with extreme depth (water/lava floating high above
    terrain). Ocean is excluded because it can naturally be any depth.

    Each fluid type gets its own category for clarity.
    """
    for tile in grid.values():
        ft = tile["fluidType"]
        if ft in (None, "ocean"):
            continue
        if tile["fluidSurf"] is None:
            continue
        depth = tile["fluidSurf"] - tile["terrainZ"]
        if depth > FLOATING_FLUID_DEPTH:
            cat = {
                "lava": "FLOATING_LAVA",
                "river": "FLOATING_RIVER",
                "lake": "FLOATING_LAKE",
            }.get(ft, "FLOATING_FLUID")
            issues.append(Issue(
                cat, tile["x"], tile["y"],
                f"{ft} fluidSurf={tile['fluidSurf']} terrainZ={tile['terrainZ']} depth={depth}",
            ))


def check_terrain_spikes_pits(grid: dict[tuple[int, int], dict[str, Any]],
                              issues: list[Issue]) -> None:
    """Terrain values that are far above or below ALL their cardinal neighbors."""
    for (x, y), tile in grid.items():
        nbr_terr = []
        for nx, ny in neighbors4(x, y):
            n = grid.get((nx, ny))
            if n is None:
                continue
            # Skip neighbors with sentinel values
            if n.get("beyondGlacier") or n["terrainZ"] <= INT64_MIN + 1:
                continue
            nbr_terr.append(n["terrainZ"])
        if len(nbr_terr) < 4:
            continue  # at edge of region, can't reliably classify
        terr = tile["terrainZ"]
        if tile.get("beyondGlacier") or terr <= INT64_MIN + 1:
            continue
        if terr > max(nbr_terr) + SPIKE_THRESHOLD:
            issues.append(Issue(
                "TERRAIN_SPIKE", x, y,
                f"terrainZ={terr} maxNbr={max(nbr_terr)} delta=+{terr - max(nbr_terr)}",
            ))
        if min(nbr_terr) > terr + SPIKE_THRESHOLD:
            issues.append(Issue(
                "TERRAIN_PIT", x, y,
                f"terrainZ={terr} minNbr={min(nbr_terr)} delta=-{min(nbr_terr) - terr}",
            ))


def check_river_chunk_gaps(grid: dict[tuple[int, int], dict[str, Any]],
                           issues: list[Issue]) -> None:
    """River tiles at chunk boundaries whose cross-chunk dry neighbor has
    terrain below the river surface (the river should have continued)."""
    for (x, y), tile in grid.items():
        if tile["fluidType"] != "river":
            continue
        rsurf = tile["fluidSurf"]
        if rsurf is None:
            continue
        for nx, ny in neighbors4(x, y):
            if not crosses_chunk_boundary(x, y, nx, ny):
                continue
            n = grid.get((nx, ny))
            if n is None or n["fluidType"] is not None:
                continue
            if n["terrainZ"] < rsurf:
                issues.append(Issue(
                    "RIVER_CHUNK_GAP", x, y,
                    f"river surf={rsurf} -> dry({nx},{ny}) terr={n['terrainZ']}",
                ))


def check_river_mouth_drop(grid: dict[tuple[int, int], dict[str, Any]],
                           issues: list[Issue]) -> None:
    """River tiles adjacent to ocean with a large surface drop."""
    for (x, y), tile in grid.items():
        if tile["fluidType"] != "river":
            continue
        rsurf = tile["fluidSurf"]
        if rsurf is None:
            continue
        for nx, ny in neighbors4(x, y):
            n = grid.get((nx, ny))
            if n is None or n["fluidType"] != "ocean":
                continue
            osurf = n["fluidSurf"]
            if osurf is None:
                continue
            drop = rsurf - osurf
            if drop > RIVER_MOUTH_DROP_THRESHOLD:
                issues.append(Issue(
                    "RIVER_MOUTH_DROP", x, y,
                    f"river surf={rsurf} ocean surf={osurf} drop={drop}",
                ))
                break  # one drop report per river tile is enough


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
    """Single dry tiles fully surrounded by lake water. These are
    "holes" in a lake that should have been filled by equilibration."""
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
            lake_surf = nbrs[0]["fluidSurf"]
            issues.append(Issue(
                "LAKE_HOLE", x, y,
                f"terrainZ={tile['terrainZ']} surrounded by lake (surf={lake_surf})",
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


def check_water_above_land(grid: dict[tuple[int, int], dict[str, Any]],
                           issues: list[Issue]) -> None:
    """River/lake tile whose surface is ≥2 above an adjacent dry tile
    that is above sea level (vegetated land). Visible as water
    floating on top of grass with blue cliff sides — the water
    should either drain or the terrain should be carved lower.
    This is the specific bug visible in screenshots: water sitting
    on top of vegetated land that it shouldn't be covering."""
    for (x, y), tile in grid.items():
        if tile["fluidType"] not in ("river", "lake"):
            continue
        wsurf = tile["fluidSurf"]
        if wsurf is None:
            continue
        for nx, ny in neighbors4(x, y):
            n = grid.get((nx, ny))
            if n is None or n["fluidType"] is not None:
                continue
            nterr = n["terrainZ"]
            cliff = wsurf - nterr
            if cliff >= 2 and nterr > SEA_LEVEL:
                issues.append(Issue(
                    "WATER_ABOVE_LAND", x, y,
                    f"{tile['fluidType']} surf={wsurf} terr={tile['terrainZ']} "
                    f"-> land({nx},{ny}) terr={nterr} cliff={cliff}",
                ))
                break


def check_water_cliff(grid: dict[tuple[int, int], dict[str, Any]],
                      issues: list[Issue]) -> None:
    """Water tile where the water surface is ≥2 above a dry neighbor's
    terrain. A 1-z cliff is natural (terrain just below water level)
    and excluded. ≥2-z cliffs are the visible multi-tile water edges
    the user sees as artifacts. Ocean excluded (renderer skips it)."""
    for (x, y), tile in grid.items():
        if tile["fluidType"] not in ("river", "lake"):
            continue
        wsurf = tile["fluidSurf"]
        if wsurf is None:
            continue
        for nx, ny in neighbors4(x, y):
            n = grid.get((nx, ny))
            if n is None or n["fluidType"] is not None:
                continue
            if n.get("beyondGlacier") or n["terrainZ"] <= INT64_MIN + 1:
                continue
            cliff_height = wsurf - n["terrainZ"]
            if cliff_height >= 2:
                issues.append(Issue(
                    "WATER_CLIFF", x, y,
                    f"{tile['fluidType']} surf={wsurf} -> dry({nx},{ny}) "
                    f"terr={n['terrainZ']} cliff={cliff_height}",
                ))
                break  # one report per water tile is enough


def check_mid_river_cliff(grid: dict[tuple[int, int], dict[str, Any]],
                          issues: list[Issue]) -> None:
    """Adjacent water tiles whose surface differs by ≥2 while their
    terrain is approximately flat (≤ 2 z apart). A 1-z surface diff
    is natural (gradual slope) and excluded. Terrain drop ≥3 is a
    real waterfall and excluded. What remains are the 2+-z stair-step
    artifacts the sim creates — visible as multi-tile water cliffs
    inside what should be a smooth river."""
    seen = set()
    for (x, y), tile in grid.items():
        if tile["fluidType"] not in ("river", "lake"):
            continue
        wsurf = tile["fluidSurf"]
        wterr = tile["terrainZ"]
        if wsurf is None:
            continue
        for nx, ny in neighbors4(x, y):
            pair = ((min(x, nx), min(y, ny)), (max(x, nx), max(y, ny)))
            if pair in seen:
                continue
            n = grid.get((nx, ny))
            if n is None or n["fluidType"] not in ("river", "lake"):
                continue
            nsurf = n["fluidSurf"]
            nterr = n["terrainZ"]
            if nsurf is None:
                continue
            terr_diff = abs(wterr - nterr)
            surf_diff = abs(wsurf - nsurf)
            # Bug: water surface differs MORE than the terrain
            # justifies.  surf_diff ≤ terr_diff + 1 is natural
            # (water at terrain+1 on both tiles).  Only flag when
            # the water step exceeds the terrain step.
            if surf_diff > terr_diff + 1 and terr_diff < 3:
                seen.add(pair)
                if wsurf > nsurf:
                    rx, ry, rsurf = x, y, wsurf
                    osurf = nsurf
                else:
                    rx, ry, rsurf = nx, ny, nsurf
                    osurf = wsurf
                issues.append(Issue(
                    "MID_RIVER_CLIFF", rx, ry,
                    f"{tile['fluidType']} surf={rsurf} (terr={wterr if rx == x else nterr}) "
                    f"-> water nbr surf={osurf} (terr={nterr if rx == x else wterr}) "
                    f"surf_diff={surf_diff} terr_diff={terr_diff}",
                ))


def check_water_water_cliff(grid: dict[tuple[int, int], dict[str, Any]],
                            issues: list[Issue]) -> None:
    """Adjacent water tiles with different surface heights. The
    renderer draws side faces between them, making the height
    difference visible as a water cliff inside what should be a
    flat water body. Ocean is excluded (the renderer skips it for
    side faces). Connected water bodies should have uniform surface."""
    seen = set()
    for (x, y), tile in grid.items():
        if tile["fluidType"] not in ("river", "lake"):
            continue
        wsurf = tile["fluidSurf"]
        if wsurf is None:
            continue
        for nx, ny in neighbors4(x, y):
            # Avoid double-reporting each pair
            pair = ((min(x, nx), min(y, ny)), (max(x, nx), max(y, ny)))
            if pair in seen:
                continue
            n = grid.get((nx, ny))
            if n is None or n["fluidType"] not in ("river", "lake"):
                continue
            nsurf = n["fluidSurf"]
            if nsurf is None:
                continue
            diff = abs(wsurf - nsurf)
            if diff > 0:
                seen.add(pair)
                # Report the higher tile (the side face is drawn on it)
                if wsurf > nsurf:
                    rx, ry, rsurf = x, y, wsurf
                    osurf = nsurf
                else:
                    rx, ry, rsurf = nx, ny, nsurf
                    osurf = wsurf
                issues.append(Issue(
                    "WATER_WATER_CLIFF", rx, ry,
                    f"{tile['fluidType']} surf={rsurf} -> water nbr surf={osurf} diff={diff}",
                ))


def check_floating_water(grid: dict[tuple[int, int], dict[str, Any]],
                         issues: list[Issue]) -> None:
    """Water tile whose terrain (channel bottom) is HIGHER than an
    adjacent dry tile's terrain. The water has nothing supporting it
    on that side — visible as a gap underneath the water column.
    Ocean excluded."""
    for (x, y), tile in grid.items():
        if tile["fluidType"] not in ("river", "lake"):
            continue
        wterr = tile["terrainZ"]
        if wterr <= INT64_MIN + 1 or tile.get("beyondGlacier"):
            continue
        for nx, ny in neighbors4(x, y):
            n = grid.get((nx, ny))
            if n is None or n["fluidType"] is not None:
                continue
            if n.get("beyondGlacier") or n["terrainZ"] <= INT64_MIN + 1:
                continue
            gap = wterr - n["terrainZ"]
            if gap > 0:
                issues.append(Issue(
                    "FLOATING_WATER", x, y,
                    f"{tile['fluidType']} terr={wterr} -> dry({nx},{ny}) "
                    f"terr={n['terrainZ']} gap={gap}",
                ))
                break


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


def check_minbound_leak(grid: dict[tuple[int, int], dict[str, Any]],
                        issues: list[Issue]) -> None:
    """Int64 minBound leaking outside the beyondGlacier zone."""
    for tile in grid.values():
        if tile.get("beyondGlacier"):
            continue
        if tile["terrainZ"] <= INT64_MIN + 1:
            issues.append(Issue(
                "MINBOUND_LEAK", tile["x"], tile["y"],
                f"terrainZ={tile['terrainZ']} (Int64 minBound leak outside beyondGlacier)",
            ))


def check_surface_inconsistent(grid: dict[tuple[int, int], dict[str, Any]],
                               issues: list[Issue]) -> None:
    """surfaceZ should equal max(terrainZ, fluidSurf or terrainZ)."""
    for tile in grid.values():
        if tile.get("beyondGlacier"):
            continue
        terr = tile["terrainZ"]
        fsurf = tile["fluidSurf"]
        expected = max(terr, fsurf) if fsurf is not None else terr
        if tile["surfaceZ"] != expected:
            issues.append(Issue(
                "SURFACE_INCONSISTENT", tile["x"], tile["y"],
                f"surfaceZ={tile['surfaceZ']} expected={expected} "
                f"(terrainZ={terr} fluidSurf={fsurf})",
            ))


# ----- Audit driver --------------------------------------------------------

ALL_CHECKS = {
    "DRY_BELOW_SEA": check_dry_below_sea,
    "OCEAN_ON_LAND": check_ocean_on_land,
    "RIVER_UNDER_TERRAIN": check_fluid_under_terrain,  # also covers LAKE
    "FLOATING_FLUID": check_floating_fluid,
    "TERRAIN_SPIKES_PITS": check_terrain_spikes_pits,
    "RIVER_CHUNK_GAP": check_river_chunk_gaps,
    "RIVER_MOUTH_DROP": check_river_mouth_drop,
    "ISLAND_1TILE": check_island_1tile,
    "LAKE_HOLE": check_lake_hole,
    "SUBMERGED_BUMP": check_submerged_bump,
    "WATER_ABOVE_LAND": check_water_above_land,
    "WATER_CLIFF": check_water_cliff,
    "WATER_WATER_CLIFF": check_water_water_cliff,
    "MID_RIVER_CLIFF": check_mid_river_cliff,
    "FLOATING_WATER": check_floating_water,
    "MULTI_ISLAND": check_multi_island,
    "FLAT_ISOLATED_WATER": check_flat_isolated_water,
    "ISOLATED_FLUID": check_isolated_fluid,
    "MINBOUND_LEAK": check_minbound_leak,
    "SURFACE_INCONSISTENT": check_surface_inconsistent,
}


def compute_stats(data: list[dict[str, Any]]) -> tuple[dict[str, int], dict[str, Any]]:
    fluid_counts: Counter[Any] = Counter(t["fluidType"] for t in data)
    fluid_stats = {
        ("dry" if k is None else k): v for k, v in fluid_counts.items()
    }

    # Filter out sentinel values for elevation stats
    real_terr = [t["terrainZ"] for t in data
                 if not t.get("beyondGlacier") and t["terrainZ"] > INT64_MIN + 1]
    if real_terr:
        elevation_stats = {
            "min": min(real_terr),
            "max": max(real_terr),
            "median": int(statistics.median(real_terr)),
            "count": len(real_terr),
        }
    else:
        elevation_stats = {"min": None, "max": None, "median": None, "count": 0}

    return fluid_stats, elevation_stats


def audit_dump(data: list[dict[str, Any]],
               seed: int | None = None,
               world_size: int | None = None,
               region: tuple[int, int, int, int] | None = None) -> AuditResult:
    grid: dict[tuple[int, int], dict[str, Any]] = {(t["x"], t["y"]): t for t in data}

    fluid_stats, elevation_stats = compute_stats(data)
    result = AuditResult(
        seed=seed, world_size=world_size, region=region,
        tile_count=len(data),
        fluid_stats=fluid_stats,
        elevation_stats=elevation_stats,
    )

    for check_fn in ALL_CHECKS.values():
        check_fn(grid, result.issues)

    return result


# ----- I/O -----------------------------------------------------------------

def run_dump(seed: int, world_size: int,
             region: tuple[int, int, int, int]) -> list[dict[str, Any]]:
    """Run the synarchy dump command and parse the JSON output."""
    cx1, cy1, cx2, cy2 = region
    cmd = [
        "cabal", "run", "exe:synarchy", "--",
        "--dump",
        f"--seed", str(seed),
        f"--worldSize", str(world_size),
        f"--region", f"{cx1},{cy1},{cx2},{cy2}",
    ]
    result = subprocess.run(
        cmd, capture_output=True, text=True, check=False,
        cwd=str(Path(__file__).resolve().parent.parent),
    )
    if result.returncode != 0:
        raise RuntimeError(
            f"dump command failed (exit {result.returncode}):\n{result.stderr[-2000:]}"
        )
    raw = result.stdout
    start = raw.find("[{")
    if start < 0:
        raise RuntimeError(
            f"no JSON array found in dump output (stdout had {len(raw)} bytes)"
        )
    return json.loads(raw[start:])


def load_dump_file(path: Path) -> list[dict[str, Any]]:
    raw = path.read_text()
    start = raw.find("[{")
    if start < 0:
        raise RuntimeError(f"no JSON array found in {path}")
    return json.loads(raw[start:])


def format_text(result: AuditResult) -> str:
    lines = []
    lines.append(f"World audit: seed={result.seed} worldSize={result.world_size} "
                 f"region={result.region}")
    lines.append(f"Tiles: {result.tile_count}")
    lines.append(f"Fluid: {result.fluid_stats}")
    lines.append(f"Elevation: {result.elevation_stats}")
    summary = result.summary()
    lines.append("")
    if summary:
        lines.append(f"Issues found: {sum(summary.values())}")
        for cat, cnt in sorted(summary.items()):
            lines.append(f"  {cat}: {cnt}")
    else:
        lines.append("Issues found: 0 (clean)")
    # Show first 5 of each category
    if result.issues:
        lines.append("")
        lines.append("Sample issues (first 5 per category):")
        per_cat: dict[str, int] = {}
        for issue in sorted(result.issues, key=lambda i: (i.category, i.x, i.y)):
            cnt = per_cat.get(issue.category, 0)
            if cnt < 5:
                lines.append(f"  {issue.category} ({issue.x},{issue.y}): {issue.details}")
                per_cat[issue.category] = cnt + 1
    return "\n".join(lines)


# ----- Main ----------------------------------------------------------------

def parse_region(s: str) -> tuple[int, int, int, int]:
    parts = s.split(",")
    if len(parts) != 4:
        raise argparse.ArgumentTypeError(
            f"region must be cx1,cy1,cx2,cy2 (got {s!r})"
        )
    try:
        return (int(parts[0]), int(parts[1]), int(parts[2]), int(parts[3]))
    except ValueError as e:
        raise argparse.ArgumentTypeError(f"region values must be ints: {e}")


def main() -> int:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--seed", type=int, default=42,
                        help="World seed (default: 42)")
    parser.add_argument("--worldSize", type=int, default=32,
                        help="World size (default: 32)")
    parser.add_argument("--region", type=parse_region,
                        default=(-4, -4, 4, 4),
                        help="Chunk region cx1,cy1,cx2,cy2 (default: -4,-4,4,4)")
    parser.add_argument("--input", type=Path,
                        help="Read pre-generated dump JSON from FILE instead of running cabal")
    parser.add_argument("--output", type=Path,
                        help="Write audit JSON to FILE (default: stdout)")
    parser.add_argument("--format", choices=("json", "text"), default="json",
                        help="Output format (default: json)")
    args = parser.parse_args()

    try:
        if args.input is not None:
            data = load_dump_file(args.input)
            # When reading from file, we don't know seed/size/region from data
            seed = args.seed
            world_size = args.worldSize
            region = args.region
        else:
            data = run_dump(args.seed, args.worldSize, args.region)
            seed = args.seed
            world_size = args.worldSize
            region = args.region

        result = audit_dump(data, seed=seed, world_size=world_size, region=region)

        if args.format == "json":
            output = json.dumps(result.to_dict(), indent=2, sort_keys=False) + "\n"
        else:
            output = format_text(result) + "\n"

        if args.output is not None:
            args.output.write_text(output)
        else:
            sys.stdout.write(output)

    except (RuntimeError, OSError) as exc:
        print(f"error: {exc}", file=sys.stderr)
        return 1

    return 0


if __name__ == "__main__":
    raise SystemExit(main())
