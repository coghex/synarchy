"""Per-tile column-integrity checks for the world audit (#2224).

The five checks that judge a single tile's own column -- its fluid
against its terrain, its Int64 sentinel, and its rendered surface --
without consulting a neighbour. `CHECKS` is this owner's inventory;
`tools/world_audit.py` composes it, with the other owners', into
`ALL_CHECKS`.

Imports the shared core and the standard library only: never a sibling
check owner, the classification policy, or the façade.
"""
from __future__ import annotations

from typing import Any

from world_audit_core import INT64_MIN, SEA_LEVEL, Issue


# Threshold for "floating" non-ocean fluid (surface much higher than terrain)
FLOATING_FLUID_DEPTH = 15


# Threshold for "ocean on land" (cascade bug)
OCEAN_ON_LAND_THRESHOLD = SEA_LEVEL + 5


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


#: This owner's checks, keyed by the registry key each carries in
#: `world_audit.ALL_CHECKS`. The façade composes that registry from
#: every owner's inventory; nothing here decides the run order.
CHECKS = {
    "OCEAN_ON_LAND": check_ocean_on_land,
    "RIVER_UNDER_TERRAIN": check_fluid_under_terrain,  # also covers LAKE
    "FLOATING_FLUID": check_floating_fluid,
    "MINBOUND_LEAK": check_minbound_leak,
    "SURFACE_INCONSISTENT": check_surface_inconsistent,
}
