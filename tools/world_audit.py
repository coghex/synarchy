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
    """River/lake tiles where the water surface is below the terrain.

    NOT a strict bug — water legitimately exists underground (aquifers,
    flooded cave systems, underground rivers). This metric exists to
    track the proportion of below-terrain fluid as a quality score; a
    sudden spike may indicate a placement error, but a moderate count
    is expected on any world with cave or aquifer features.
    """
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
            # Submerged spikes are concealed the same way submerged
            # pits are (below): when the tile's own fluid surface is
            # at or above its terrain top, the water plane renders
            # flat over the protrusion — an ocean seamount, not a
            # visible render artifact. (First seen: basalt seamount
            # from an underwater vent, seed 4 w64 full-world scan.)
            fsurf = tile.get("fluidSurf")
            if fsurf is not None and fsurf >= terr:
                continue
            issues.append(Issue(
                "TERRAIN_SPIKE", x, y,
                f"terrainZ={terr} maxNbr={max(nbr_terr)} delta=+{terr - max(nbr_terr)}",
            ))
        if min(nbr_terr) > terr + SPIKE_THRESHOLD:
            # TERRAIN_PIT is an ABOVE-SEA dry-land check. At/below sea
            # level a deep tile is one of:
            #  (a) water-covered seabed/lakebed dip — the water plane
            #      renders over it (fully submerged, or a sub-sea floor
            #      with fluidSurf ≥ its terrain, e.g. a pond between
            #      basalt seamounts). Seabed smoothness is validated
            #      separately (flat-floor %, no straight shelf edges).
            #  (b) a DRY-below-sea tile — the chunk-vs-tile ocean
            #      classification mismatch (a below-sea region the
            #      coarse oceanic test left dry). That is tracked by
            #      the DRY_BELOW_SEA quality metric; the seabed fill can
            #      leave such a region's interior as a relative pit, but
            #      it is the same mismatch anomaly, not a new void.
            # So exempt anything at/below sea level; an above-sea land
            # pit (with or without a puddle) still flags.
            fsurf = tile.get("fluidSurf")
            submergedConceal = fsurf is not None and fsurf >= min(nbr_terr)
            if submergedConceal or terr <= SEA_LEVEL:
                continue
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
    """surfaceZ must match the engine's mkSurfaceMap rule:
      - River tiles: surfaceZ == fluidSurf (water plane renders flat,
        hiding any minor terrain protrusion in the carved channel).
      - Other fluid (Ocean/Lake/Lava): surfaceZ == max(terrainZ, fluidSurf).
      - Dry tiles: surfaceZ == terrainZ.

    This rule lives in `World/Generate/Chunk.hs::mkSurfaceMap` and is
    repeated in `Sim/Thread.hs` and the chunk-load seeding paths;
    keep them in sync if you change one.
    """
    for tile in grid.values():
        if tile.get("beyondGlacier"):
            continue
        terr = tile["terrainZ"]
        fsurf = tile["fluidSurf"]
        ftype = tile["fluidType"]
        if fsurf is None:
            expected = terr
        elif ftype == "river":
            expected = fsurf
        else:
            expected = max(terr, fsurf)
        if tile["surfaceZ"] != expected:
            issues.append(Issue(
                "SURFACE_INCONSISTENT", tile["x"], tile["y"],
                f"surfaceZ={tile['surfaceZ']} expected={expected} "
                f"(terrainZ={terr} fluidSurf={fsurf})",
            ))


WETLAND_MATS = {62, 63, 64}      # peat, mucky peat, muck
DESERT_MATS = {55: "sand", 67: "salt_flat"}


def _max_same_chunk_nbr_delta(grid: dict[tuple[int, int], dict[str, Any]],
                              x: int, y: int, tz: int) -> int:
    """Max |Δterrain| to same-chunk 4-neighbours (cross-chunk skipped,
    matching the in-chunk-only convention of the wetland post-pass)."""
    worst = 0
    for nx, ny in neighbors4(x, y):
        if crosses_chunk_boundary(x, y, nx, ny):
            continue
        n = grid.get((nx, ny))
        if n is None or n.get("beyondGlacier"):
            continue
        nz = n["terrainZ"]
        if nz <= INT64_MIN + 1:
            continue
        worst = max(worst, abs(tz - nz))
    return worst


def check_wetland_on_slope(grid: dict[tuple[int, int], dict[str, Any]],
                           issues: list[Issue]) -> None:
    """BUG: wetland soil (peat 62 / mucky peat 63 / muck 64) on a slope.

    The wetland post-pass (`Generate/Chunk.hs::wetlandKeep`) guarantees
    wetland soils survive only on near-flat tiles (4-neighbour max
    |Δterrain| ≤ 2). Since 2026-06-07 the gate reads cross-chunk
    neighbours from the bordered post-carve vector, so this check uses
    the FULL 4-neighbourhood (no same-chunk restriction). Any dry
    occurrence means the post-pass broke.

    Only the slope half of the gate is checkable here — the wet half
    (wt ≥ terrain−1) is covered by the hspec test
    (Test.Headless.WorldGen.Flatness), which reads lcWaterTableMap
    directly.
    """
    for (x, y), t in grid.items():
        if t.get("matId") not in WETLAND_MATS:
            continue
        if t.get("beyondGlacier") or t["terrainZ"] <= INT64_MIN + 1:
            continue
        # Sub-sea floor muck is placed by the seabed pass by design
        # (sand→silt→muck by depth); the continental slope and trench
        # walls are legitimately steep. This check targets wetland
        # soil on a LAND hillside, so exempt anything at or below sea
        # level. (A sub-sea tile that renders dry due to the chunk-vs-
        # tile ocean-classification mismatch is a separate, tracked
        # rendering limitation — not a wetland-gate violation.)
        if t["terrainZ"] <= SEA_LEVEL:
            continue
        # Submerged bed material is concealed by the flat water plane
        # (same principle as the submerged pit/spike exemptions): a
        # steep lake-bed pillar wearing muck is invisible. Verified
        # 2026-06-07: every flagged tile on seeds 42/7 w64 (44 + 8)
        # was underwater; dry-land violations are what this check is
        # for, and the border-aware wetlandKeep keeps those at 0.
        fsurf = t.get("fluidSurf")
        if fsurf is not None and fsurf >= t["terrainZ"]:
            continue
        tz = t["terrainZ"]
        worst = 0
        for nx, ny in neighbors4(x, y):
            n = grid.get((nx, ny))
            if n is None or n.get("beyondGlacier"):
                continue
            nz = n["terrainZ"]
            if nz <= INT64_MIN + 1:
                continue
            worst = max(worst, abs(tz - nz))
        if worst > 2:
            issues.append(Issue(
                "WETLAND_ON_SLOPE", x, y,
                f"matId={t['matId']} maxNbrDelta={worst}",
            ))


def check_desert_soil_on_slope(grid: dict[tuple[int, int], dict[str, Any]],
                               issues: list[Issue]) -> None:
    """QUALITY: sand (55) / salt flat (67) on a slope.

    Unlike wetland soils, these have no physical post-pass gate yet —
    `soilFromClimate` places them purely by climate. In practice they
    land on plateau-snapped lowlands, so on-slope occurrences are rare
    (measured 0.1% of sand tiles at w64 seed 42, 2026-06-05). Tracked
    as a quality score: drift upward means desert/evaporite soils are
    bleeding onto mountainsides. Salt flats especially are basin-floor
    evaporites and should essentially never tilt.
    """
    for (x, y), t in grid.items():
        name = DESERT_MATS.get(t.get("matId"))
        if name is None:
            continue
        if t.get("beyondGlacier") or t["terrainZ"] <= INT64_MIN + 1:
            continue
        # Sub-sea sand is seabed (the ocean-floor pass lays sand on the
        # shallow shelf ramp by design — see World.Fluid.Seabed), not a
        # desert soil bleeding onto a hillside. Exempt at/below sea
        # level so the shelf's natural slope doesn't trip this check.
        if t["terrainZ"] <= SEA_LEVEL:
            continue
        # Slope is measured over LAND neighbours only. A beach-sand
        # tile at the waterline naturally slopes down into the (now
        # deeper, post-seabed) sea floor — that's a beach, not desert
        # on a mountainside. Only a steep slope to another ABOVE-sea
        # tile means the desert soil is genuinely on a hillside.
        tz = t["terrainZ"]
        worst = 0
        for nx, ny in neighbors4(x, y):
            if crosses_chunk_boundary(x, y, nx, ny):
                continue
            n = grid.get((nx, ny))
            if n is None or n.get("beyondGlacier"):
                continue
            nz = n["terrainZ"]
            if nz <= INT64_MIN + 1 or nz <= SEA_LEVEL:
                continue
            worst = max(worst, abs(tz - nz))
        if worst > 2:
            issues.append(Issue(
                "DESERT_SOIL_ON_SLOPE", x, y,
                f"{name} maxNbrDelta={worst}",
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
    "WETLAND_ON_SLOPE": check_wetland_on_slope,
    "DESERT_SOIL_ON_SLOPE": check_desert_soil_on_slope,
}

# ----- Severity classification --------------------------------------------
#
# Every category produced by ALL_CHECKS belongs to one of two buckets:
#
#   BUG     — any occurrence is a real bug. Must be 0 in a healthy world.
#             world_check.py enforces this with a hard envelope of 0.
#
#   QUALITY — exists on a spectrum. Some occurrence is expected and
#             realistic (small islands, underground aquifers, rivers
#             drying up before the coast). Tracked as a quality score
#             against a threshold; failure means the metric drifted far
#             enough to indicate broken worldgen, not zero tolerance.
#
# See `feedback_testing_philosophy` in memory for the rationale.

# Bug categories — any occurrence is unambiguous corruption.
BUG_CATEGORIES = {
    "OCEAN_ON_LAND",        # Ocean fluid type leaked onto high terrain
    "TERRAIN_SPIKE",        # Despike pass should have removed
    "TERRAIN_PIT",          # Same
    "MINBOUND_LEAK",        # Int64 sentinel outside beyondGlacier zone
    "SURFACE_INCONSISTENT", # surfaceZ doesn't match the documented rule
    "WETLAND_ON_SLOPE",     # wetland post-pass gate violated (slope half)
}

# Quality categories — tracked as scores against thresholds, not bugs.
# A non-zero count can be legitimate; failure happens when the count
# drifts above the threshold for that metric.
QUALITY_CATEGORIES = {
    "DRY_BELOW_SEA",         # ocean-connected dry tile (after BFS filter)
    "RIVER_UNDER_TERRAIN",   # underground river/aquifer
    "LAKE_UNDER_TERRAIN",    # underground/cave lake
    "FLOATING_LAVA",
    "FLOATING_RIVER",
    "FLOATING_LAKE",
    "FLOATING_FLUID",
    "RIVER_CHUNK_GAP",       # cross-chunk seam mismatch or natural dry-up
    "RIVER_MOUTH_DROP",      # waterfall at coast — physical for steep rivers
    "ISLAND_1TILE",          # tiny isolated island — can be real
    "LAKE_HOLE",             # dry tile mid-lake — can be a tiny lake island
    "SUBMERGED_BUMP",        # terrain protrusion through water plane
    "ISOLATED_FLUID",        # singleton fluid tile — small puddle / artifact
    "WATER_ABOVE_LAND",      # river in valley with high banks
    "WATER_CLIFF",           # water against terrain cliff
    "WATER_WATER_CLIFF",     # downstream gradient stair-step
    "MID_RIVER_CLIFF",       # river surface step larger than terrain step
    "FLOATING_WATER",        # water-vs-dry side gap, often legitimate cliff
    "MULTI_ISLAND",          # small dry cluster in a water body
    "FLAT_ISOLATED_WATER",   # small puddle on flat terrain
    "DESERT_SOIL_ON_SLOPE",  # sand/salt-flat off the plateau — ungated, rare
}

# Quality thresholds — per-seed max occurrence count, calibrated against
# observed values across the 21-seed baseline set. A category whose count
# exceeds its threshold is flagged in world_check.py as a quality
# regression; under-threshold counts are tracked but don't fail.
#
# Calibration policy: set to ~1.5× the worst current value across the
# baseline set, so legitimate variance from new seeds doesn't trigger
# false fails, but a doubling of any metric does. Tighten downward as
# generation improves and the band of expected values narrows.
QUALITY_THRESHOLDS = {
    # Low-variance categories — should stay near zero.
    # Recalibrated 2026-06-07 for the volcanism default 1.0 → 1.25
    # (user-approved): rougher volcanic flanks pin more 1-tile
    # water-table puddles, so the puddle-flavored metrics shifted.
    # Differential vs the old constants confirmed counts move both
    # directions per seed (no new artifact class) and two seeds
    # already exceeded the old thresholds before the change.
    "DRY_BELOW_SEA":        200,  # observed max 155 coastal z=0 tiles
                                  # (seed 137, the known-bad seed —
                                  # 5803 before the wt rework)
    "DESERT_SOIL_ON_SLOPE": 250,  # observed max 150 (seed 123 w128); 1.5× policy
    "FLAT_ISOLATED_WATER":   90,  # observed max 59 (seed 5050)
    "FLOATING_WATER":       150,  # observed max 52
    "ISOLATED_FLUID":        90,  # observed max 74 (seed 2718; was 77
                                  # even with old constants)
    "LAKE_HOLE":             25,  # observed max 4
    "MULTI_ISLAND":          25,  # observed max 4
    "RIVER_CHUNK_GAP":       50,  # observed max 14
    "RIVER_MOUTH_DROP":      50,  # observed max 15
    "SUBMERGED_BUMP":        25,  # observed max 4
    # High-variance / by-design categories.
    "FLOATING_LAKE":       7000,  # observed max 5697 (seed 99) after the
                                  # continental-margin seabed (save v26)
                                  # deepens sea-surrounded clamped basins
                                  # into the slope — deep sea-connected
                                  # basins are SUPPOSED to be deep, so the
                                  # "floating lake" (deep water column)
                                  # count rises. High-variance metric,
                                  # recalibrated 2026-06-08.
    "FLOATING_LAVA":        100,  # not observed in baselines
    "FLOATING_RIVER":       300,  # not observed in baselines
    "FLOATING_FLUID":       300,  # generic fallback
    "ISLAND_1TILE":         100,  # not observed; small islands possible
    "LAKE_UNDER_TERRAIN":   500,  # not observed; underground lakes possible
    "MID_RIVER_CLIFF":     1500,  # observed max 1046 (downstream gradient)
    "RIVER_UNDER_TERRAIN":  500,  # observed max 251 (underground rivers OK)
    "WATER_ABOVE_LAND":     300,  # observed max 152 (steep valleys)
    "WATER_CLIFF":          300,  # observed max 152
    "WATER_WATER_CLIFF":   3500,  # observed max 2425 (stair-step gradient)
}


def severity_of(category: str) -> str:
    """Return 'BUG' or 'QUALITY' for an issue category.

    Unknown categories default to QUALITY (safe — they'll be
    threshold-checked rather than treated as hard fails).
    """
    if category in BUG_CATEGORIES:
        return "BUG"
    return "QUALITY"


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
