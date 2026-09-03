"""Boundary checks for the world audit (#2224).

The nine checks that compare a tile against its cardinal neighbours:
lava rim containment, terrain steps, chunk-seam river continuity, river
mouths, and the water/terrain and water/water interfaces. `CHECKS` is
this owner's inventory; `tools/world_audit.py` composes it, with the
other owners', into `ALL_CHECKS`.

Imports the shared core and the standard library only: never a sibling
check owner, the classification policy, or the façade.
"""
from __future__ import annotations

from typing import Any

from world_audit_core import (
    INT64_MIN, SEA_LEVEL, Issue, crosses_chunk_boundary, neighbors4,
)


# Threshold for considering a terrain difference a "spike" or "pit"
SPIKE_THRESHOLD = 15


# Threshold for "river mouth drop"
RIVER_MOUTH_DROP_THRESHOLD = 5


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


#: This owner's checks, keyed by the registry key each carries in
#: `world_audit.ALL_CHECKS`. The façade composes that registry from
#: every owner's inventory; nothing here decides the run order.
CHECKS = {
    "LAVA_RIM_CONTAINMENT": check_lava_rim_containment,
    "TERRAIN_SPIKES_PITS": check_terrain_spikes_pits,
    "RIVER_CHUNK_GAP": check_river_chunk_gaps,
    "RIVER_MOUTH_DROP": check_river_mouth_drop,
    "WATER_ABOVE_LAND": check_water_above_land,
    "WATER_CLIFF": check_water_cliff,
    "WATER_WATER_CLIFF": check_water_water_cliff,
    "MID_RIVER_CLIFF": check_mid_river_cliff,
    "FLOATING_WATER": check_floating_water,
}
