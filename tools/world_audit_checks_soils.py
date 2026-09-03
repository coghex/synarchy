"""Soil-placement checks for the world audit (#2224).

The two checks that read a tile's material id: wetland soil on a slope
and desert soil off the plateau. `CHECKS` is this owner's inventory;
`tools/world_audit.py` composes it, with the other owners', into
`ALL_CHECKS`.

The two checks deliberately do NOT share a slope helper. `#2224`:
`check_wetland_on_slope` reads the FULL 4-neighbourhood per its
2026-06-07 note, while `check_desert_soil_on_slope` skips both
cross-chunk and at-or-below-sea neighbours. `_max_same_chunk_nbr_delta`
below is the older same-chunk-only form neither check calls; it is kept
verbatim beside them rather than routed into either, because routing
`check_wetland_on_slope` through it would silently narrow that
predicate.

Imports the shared core and the standard library only: never a sibling
check owner, the classification policy, or the façade.
"""
from __future__ import annotations

from typing import Any

from world_audit_core import (
    INT64_MIN, SEA_LEVEL, Issue, crosses_chunk_boundary, neighbors4,
)


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


#: This owner's checks, keyed by the registry key each carries in
#: `world_audit.ALL_CHECKS`. The façade composes that registry from
#: every owner's inventory; nothing here decides the run order.
CHECKS = {
    "WETLAND_ON_SLOPE": check_wetland_on_slope,
    "DESERT_SOIL_ON_SLOPE": check_desert_soil_on_slope,
}
