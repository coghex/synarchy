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

    This predicate inspects ONE tile's own column and no neighbour, so it
    measures fluid DEPTH, never floating geometry. The lava category is
    named DEEP_LAVA_COLUMN for exactly that reason (#1876): a deep pool
    sitting in a basin whose whole rim is at or above its surface is
    correctly grown lava, not an artifact. Whether a lava pool is actually
    supported by its rim is the separate, geometric
    check_lava_rim_containment below.
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
                "lava": "DEEP_LAVA_COLUMN",
                "river": "FLOATING_RIVER",
                "lake": "FLOATING_LAKE",
            }.get(ft, "FLOATING_FLUID")
            issues.append(Issue(
                cat, tile["x"], tile["y"],
                f"{ft} fluidSurf={tile['fluidSurf']} terrainZ={tile['terrainZ']} depth={depth}",
            ))


def check_lava_rim_containment(grid: dict[tuple[int, int], dict[str, Any]],
                               issues: list[Issue]) -> None:
    """Lava tiles whose rim cannot hold the pool at its own surface (#1876).

    A pool is supposed to be CONTAINED: every dry tile bordering it sits at
    or above its surface, so nothing is perched or spilling. This checks
    exactly that, on the tiles `--dump` actually emits.

    **What that does and does not observe.** Containment is established
    twice over, and only the SECOND is visible here. `World.Magma.Pool`'s
    `floodPool.grow` clamps a pool below its basin's spill saddle — but it
    also stops at the area cap (`n >= poolArea`) and the jittered radius
    bound (`withinRim`), either of which can end growth beside a lower dry
    tile. That raw rim never reaches a dump: `World.Generate.Chunk` raises
    every OUTERMOST pool tile to the pool surface as a basalt cap
    (`poolRimCaps` → `applyBasaltCaps`) and `applyLavaShell` strips the
    zero-depth lava film off it, leaving a basalt wall FLUSH with the lava.
    So a `grow` truncation is repaired before the audit can see it, and
    post-cap data cannot show whether one happened.

    What this check therefore guards is that SEALING pipeline: a regression
    in the rim caps, the terrain patch or the shell would leave a real rim
    tile below the surface and fail here. The zero it measures across the
    baselines is evidence the seal holds, not evidence `grow` never
    truncates. `tools/test_audit_world_audit.py`'s
    `test_lava_rim_on_real_generated_output` pins that reading — it drives a
    real seed-12321 dump, asserts the rim is present and flush, and then
    lowers one real rim tile to prove the predicate is a live guard rather
    than dead code. Auditing the PRE-cap rim would need the dump to expose
    it, which is engine-side work #1876 puts out of scope.

    Containment is a per-tile rim property, so no pool-connectivity pass is
    needed. One offending lava tile emits AT MOST one occurrence however
    many of its neighbours are lower.

    Neighbour classification mirrors `World.Magma.Pool.isWater`:

      * ocean/lake/river   — a water barrier the pool stops against, never
                             a breach.
      * the world-boundary sentinel (`beyondGlacier`, or terrainZ at the
        Int64 floor) — also a barrier, and not readable terrain besides.
      * lava               — a breach only when its own surface is STRICTLY
                             lower: a higher pool draining into a lower one
                             is the perched configuration this exists to
                             catch. The occurrence is filed against the
                             HIGHER tile.
      * dry land           — a breach when its terrainZ is STRICTLY below
                             this tile's fluidSurf. Equal elevation is
                             contained.

    A cardinal neighbour ABSENT from the dump window is not evidence of
    either verdict: the tile gets one LAVA_RIM_INCOMPLETE record saying the
    containment judgement could not be completed there. That record is
    independent of LAVA_RIM_BREACH — a tile can be provably breached on one
    present neighbour while another is off-window — but absence alone never
    emits a breach.
    """
    for (x, y), tile in grid.items():
        if tile["fluidType"] != "lava":
            continue
        surf = tile["fluidSurf"]
        if surf is None:
            continue
        # A sentinel tile carries no readable terrain of its own, so its
        # rim cannot be judged either — the same exclusion every other
        # neighbour-reading check applies.
        if tile.get("beyondGlacier") or tile["terrainZ"] <= INT64_MIN + 1:
            continue

        lower: list[str] = []
        absent = 0
        for nx, ny in neighbors4(x, y):
            n = grid.get((nx, ny))
            if n is None:
                absent += 1
                continue
            if n.get("beyondGlacier") or n["terrainZ"] <= INT64_MIN + 1:
                continue  # world-boundary sentinel — a barrier, per isWater
            nft = n["fluidType"]
            if nft == "lava":
                nsurf = n["fluidSurf"]
                if nsurf is not None and nsurf < surf:
                    lower.append(f"({nx},{ny})lavaSurf={nsurf}")
                continue
            if nft is not None:
                # ocean/lake/river — a water barrier, per isWater. Any
                # neighbour that is not dry land is not rim, so an
                # unrecognized fluid type is skipped the same way rather
                # than being read as land.
                continue
            if n["terrainZ"] < surf:
                lower.append(f"({nx},{ny})terrainZ={n['terrainZ']}")

        if lower:
            issues.append(Issue(
                "LAVA_RIM_BREACH", x, y,
                f"lava fluidSurf={surf} unsupported by {' '.join(lower)}",
            ))
        if absent:
            issues.append(Issue(
                "LAVA_RIM_INCOMPLETE", x, y,
                f"lava fluidSurf={surf} unjudged at {absent} off-region nbr(s)",
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
    side faces).

    A 1-z surface step is natural where water flows downhill, so it is
    excluded when a river is involved — consistent with WATER_CLIFF and
    MID_RIVER_CLIFF, which both treat a 1-z step as natural. Lakes are
    equilibrated flat, so a 1-z step between two lake tiles is a real
    (if minor) artifact and is still flagged."""
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
            # Natural downhill flow: a 1-z step involving a river is
            # expected, not a cliff. Lake-to-lake 1-z steps stay flagged.
            if diff == 1 and "river" in (tile["fluidType"], n["fluidType"]):
                continue
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

    The rule has ONE definition in the engine (#1112):
    `World/Fluid/Types.hs::renderedSurfaceZ`. Generation
    (`World/Generate/Chunk/Fluid.hs::mkSurfaceMap`), the sim writeback
    (`Sim/Thread.hs::emitWorldDirtyFluids`) and every player-edit path
    (`World/Edit/Apply.hs`) call it rather than restating it, so this
    check only needs to track that one function.
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

    Salt flat (67) now has a physical post-pass gate (`saltFlatKeep` in
    World.Generate.Chunk demotes a sloped salt flat to light gravel 66,
    2026-06-15), so 67-on-slope should be 0. Sand (55) is still ungated
    — `soilFromClimate` places it purely by climate, but in practice it
    lands on plateau-snapped lowlands so on-slope occurrences are rare
    (measured 0.1% of sand tiles at w64 seed 42, 2026-06-05). Tracked as
    a quality score: drift upward means desert soils are bleeding onto
    mountainsides.
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
    "LAVA_RIM_CONTAINMENT": check_lava_rim_containment,
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
# Every category the audit emits — the `category` argument of every
# `Issue(...)` construction, NOT the ALL_CHECKS keys, which are check-function
# labels and disagree with the categories in both directions — belongs to one
# of two buckets:
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
# The classification is CLOSED for the world_check.py gate: a category in
# neither set, or a QUALITY category with no QUALITY_THRESHOLDS entry, fails
# that seed's check by name rather than being tolerated under an implicit
# default (see classify_category below and world_check.py::check_issue_summary).
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
    "DEEP_LAVA_COLUMN",      # deep lava column — depth only, not geometry
    "LAVA_RIM_BREACH",       # lava unsupported by a lower dry/lava rim tile
    "LAVA_RIM_INCOMPLETE",   # rim judgement incomplete at the region edge
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
    "FLOATING_WATER":       500,  # observed max 319 (seed 12321) after
                                  # coastline variety (#220, save v69):
                                  # cliff coasts + coastal mountains kept
                                  # by the steepness field make legitimate
                                  # water-adjacent cliffs common by
                                  # design. Gap histogram is ≥95% 1-8z
                                  # bank steps; the deep outliers are
                                  # high-altitude tarns/rivers against
                                  # valley headwalls (surf 97-183), not
                                  # coastal artifacts. 1.5× obs-max per
                                  # the calibration policy. Previously
                                  # 300 (obs max 176, recalibrated
                                  # 2026-06-11; before that 150).
    "ISOLATED_FLUID":        90,  # observed max 74 (seed 2718; was 77
                                  # even with old constants)
    "LAKE_HOLE":             25,  # observed max 0 after the terrainZ<surf
                                  # refinement (#21): flush waterline
                                  # islets no longer counted, only genuine
                                  # depressions the lake failed to fill.
                                  # Threshold kept for headroom against a
                                  # real unfilled-hole regression.
    "MULTI_ISLAND":          25,  # observed max 4
    "RIVER_CHUNK_GAP":       50,  # observed max 14
    "RIVER_MOUTH_DROP":      50,  # observed max 15
    "SUBMERGED_BUMP":        25,  # observed max 4

    # The three lava metrics were measured across all 21 baselines on
    # 2026-08-30, after #1876 split rim containment out of the old
    # depth-only FLOATING_LAVA. The superseded 450/"observed max 301
    # (seed 1337)" rationale predated the current generator: seed 1337
    # carries no deep lava column at all today.
    "DEEP_LAVA_COLUMN":      20,  # observed max 11 (seed 12321; seed 250
                                  # is the only other non-zero, at 2).
                                  # Column DEPTH only — a contained pool
                                  # is supposed to be deep, exactly like
                                  # FLOATING_LAKE. 1.5× obs-max is 16.5,
                                  # rounded up per the policy above.
    "LAVA_RIM_BREACH":        0,  # observed max 0 — NO baseline seed
                                  # produces one, because Chunk.hs's rim
                                  # caps + lava shell seal every pool's
                                  # rim flush with its surface before the
                                  # dump exists (see the check). The zero
                                  # therefore measures that SEALING pass,
                                  # not World.Magma.Pool.grow's own
                                  # clamp. 1.5× obs-max is 0, so the cap
                                  # is 0 and any breach fails. It stays
                                  # QUALITY rather than BUG because the
                                  # seal is a generation TUNING pass, not
                                  # corruption — raising this is the
                                  # evidence-backed response to a
                                  # deliberate generator change, which
                                  # BUG would forbid.
    "LAVA_RIM_INCOMPLETE":   20,  # observed max 13 (seed 5050); 1.5× is
                                  # 19.5, rounded up. Counts lava tiles
                                  # whose containment could not be judged
                                  # because a cardinal neighbour lay
                                  # outside the dumped region, so it
                                  # scales with how much lava meets the
                                  # REQUESTED WINDOW's edge, not with
                                  # world health. Calibrated for the
                                  # baseline windows; a sweep over a
                                  # different region legitimately
                                  # measures a different value.
    # High-variance / by-design categories.
    "FLOATING_LAKE":       7000,  # observed max 5697 (seed 99) after the
                                  # continental-margin seabed (save v26)
                                  # deepens sea-surrounded clamped basins
                                  # into the slope — deep sea-connected
                                  # basins are SUPPOSED to be deep, so the
                                  # "floating lake" (deep water column)
                                  # count rises. High-variance metric,
                                  # recalibrated 2026-06-08.
    "FLOATING_RIVER":       300,  # not observed in baselines
    "FLOATING_FLUID":       300,  # generic fallback
    "ISLAND_1TILE":         100,  # not observed; small islands possible
    "LAKE_UNDER_TERRAIN":   500,  # not observed; underground lakes possible
    "MID_RIVER_CLIFF":     1500,  # observed max 1046 (downstream gradient)
    "RIVER_UNDER_TERRAIN":  500,  # observed max 251 (underground rivers OK)
    "WATER_ABOVE_LAND":     500,  # observed max 323 after #220 coastline
                                  # variety (was 152, steep valleys) —
                                  # same rationale + histogram as
                                  # FLOATING_WATER above.
    "WATER_CLIFF":          500,  # observed max 323 after #220 — see
                                  # FLOATING_WATER rationale.
    "WATER_WATER_CLIFF":   3500,  # observed max 2704 (seed 13579) after the
                                  # #21 refinement: river-involved 1-z steps
                                  # (natural downhill flow) excluded, lake-to-
                                  # lake 1-z steps still flagged. Threshold
                                  # kept; headroom retained against new seeds.
}


def severity_of(category: str) -> str:
    """Return 'BUG' or 'QUALITY' for an issue category.

    This is the COARSE bucketing used for reporting: an unknown category
    falls back to 'QUALITY' so a caller that only partitions counts into
    two piles keeps working. world_stress.py relies on that total function.

    It is deliberately NOT the gate's classifier. Anything that must fail
    closed on an unclassified category — world_check.py — uses
    classify_category() instead, which reports the absence rather than
    hiding it behind this fallback.
    """
    if category in BUG_CATEGORIES:
        return "BUG"
    return "QUALITY"


def classify_category(category: str) -> str | None:
    """Return 'BUG', 'QUALITY', or None when the category is unclassified.

    Unlike severity_of(), this never guesses: a category declared in
    neither BUG_CATEGORIES nor QUALITY_CATEGORIES yields None, so callers
    that gate on the audit can reject it by name instead of silently
    treating a brand-new corruption class as a tolerated quality metric.
    """
    if category in BUG_CATEGORIES:
        return "BUG"
    if category in QUALITY_CATEGORIES:
        return "QUALITY"
    return None


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
