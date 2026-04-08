#!/usr/bin/env python3
"""Unit tests for world_audit.py.

Constructs synthetic tile grids that exercise each check, verifies the
audit correctly identifies the issues.

Exit codes:
  0 = all tests passed
  1 = one or more tests failed
"""

from __future__ import annotations

import json
import sys
from pathlib import Path
from typing import Any

sys.path.insert(0, str(Path(__file__).resolve().parent))
from world_audit import audit_dump, INT64_MIN  # type: ignore


# ----- Helpers -------------------------------------------------------------

def tile(x: int, y: int, terrainZ: int = 1,
         fluidType: str | None = None, fluidSurf: int | None = None,
         matId: int = 64,
         glacierZone: bool = False, beyondGlacier: bool = False) -> dict[str, Any]:
    if fluidType is None:
        surfaceZ = terrainZ
    else:
        surfaceZ = max(terrainZ, fluidSurf if fluidSurf is not None else terrainZ)
    return {
        "x": x, "y": y, "v": x + y,
        "terrainZ": terrainZ,
        "surfaceZ": surfaceZ,
        "matId": matId,
        "fluidType": fluidType,
        "fluidSurf": fluidSurf,
        "iceSurf": None,
        "iceMode": None,
        "glacierZone": glacierZone,
        "beyondGlacier": beyondGlacier,
    }


def flat_grid(w: int, h: int, x0: int = 0, y0: int = 0,
              terrainZ: int = 1,
              fluidType: str | None = None,
              fluidSurf: int | None = None) -> list[dict[str, Any]]:
    return [
        tile(x0 + dx, y0 + dy, terrainZ=terrainZ,
             fluidType=fluidType, fluidSurf=fluidSurf)
        for dy in range(h) for dx in range(w)
    ]


def make_tiles(tiles: list[dict[str, Any]]) -> list[dict[str, Any]]:
    """Normalize: drop duplicates (later overrides earlier)."""
    seen = {}
    for t in tiles:
        seen[(t["x"], t["y"])] = t
    return list(seen.values())


def count_category(result_dict: dict[str, Any], cat: str) -> int:
    return result_dict["summary"].get(cat, 0)


# ----- Tests ---------------------------------------------------------------

FAILURES: list[str] = []


def expect(cond: bool, msg: str) -> None:
    if not cond:
        FAILURES.append(msg)
        print(f"  FAIL: {msg}")
    else:
        print(f"  OK:   {msg}")


def test_dry_below_sea() -> None:
    print("test_dry_below_sea")
    tiles = flat_grid(5, 5, -2, -2, terrainZ=-1, fluidType="ocean", fluidSurf=0)
    # Turn one tile into dry with terrainZ=-1
    tiles[12] = tile(0, 0, terrainZ=-1, fluidType=None)
    result = audit_dump(tiles).to_dict()
    expect(count_category(result, "DRY_BELOW_SEA") == 1,
           f"DRY_BELOW_SEA count: {count_category(result, 'DRY_BELOW_SEA')}, expected 1")


def test_ocean_on_land() -> None:
    print("test_ocean_on_land")
    # Cascade bug: ocean tile with terrainZ > 5
    tiles = flat_grid(3, 3, -1, -1, terrainZ=1, fluidType="ocean", fluidSurf=0)
    tiles[4] = tile(0, 0, terrainZ=100, fluidType="ocean", fluidSurf=0)
    result = audit_dump(tiles).to_dict()
    expect(count_category(result, "OCEAN_ON_LAND") == 1,
           f"OCEAN_ON_LAND count: {count_category(result, 'OCEAN_ON_LAND')}, expected 1")


def test_river_under_terrain() -> None:
    print("test_river_under_terrain")
    # River tile where fluidSurf < terrainZ
    tiles = flat_grid(3, 3, -1, -1, terrainZ=5, fluidType=None)
    tiles[4] = tile(0, 0, terrainZ=10, fluidType="river", fluidSurf=5)
    result = audit_dump(tiles).to_dict()
    expect(count_category(result, "RIVER_UNDER_TERRAIN") == 1,
           f"RIVER_UNDER_TERRAIN count: {count_category(result, 'RIVER_UNDER_TERRAIN')}, expected 1")


def test_lake_under_terrain() -> None:
    print("test_lake_under_terrain")
    tiles = flat_grid(3, 3, -1, -1, terrainZ=5, fluidType=None)
    tiles[4] = tile(0, 0, terrainZ=10, fluidType="lake", fluidSurf=5)
    result = audit_dump(tiles).to_dict()
    expect(count_category(result, "LAKE_UNDER_TERRAIN") == 1,
           f"LAKE_UNDER_TERRAIN count: {count_category(result, 'LAKE_UNDER_TERRAIN')}, expected 1")


def test_floating_fluid() -> None:
    print("test_floating_fluid")
    # Lava with fluidSurf - terrainZ > 15
    tiles = flat_grid(3, 3, -1, -1, terrainZ=-50, fluidType="lava", fluidSurf=5)
    result = audit_dump(tiles).to_dict()
    expect(count_category(result, "FLOATING_LAVA") == 9,
           f"FLOATING_LAVA count: {count_category(result, 'FLOATING_LAVA')}, expected 9")

    # River with high depth
    tiles = flat_grid(3, 3, -1, -1, terrainZ=-50, fluidType="river", fluidSurf=5)
    result = audit_dump(tiles).to_dict()
    expect(count_category(result, "FLOATING_RIVER") == 9,
           f"FLOATING_RIVER count: {count_category(result, 'FLOATING_RIVER')}, expected 9")

    # Ocean at any depth should NOT trigger floating
    tiles = flat_grid(3, 3, -1, -1, terrainZ=-100, fluidType="ocean", fluidSurf=0)
    result = audit_dump(tiles).to_dict()
    expect(count_category(result, "FLOATING_LAVA") == 0
           and count_category(result, "FLOATING_RIVER") == 0
           and count_category(result, "FLOATING_LAKE") == 0,
           "deep ocean should not trigger FLOATING_*")


def test_terrain_spike() -> None:
    print("test_terrain_spike")
    # Flat terrain with one tile spike
    tiles = flat_grid(5, 5, -2, -2, terrainZ=1, fluidType=None)
    # Replace center with a spike
    tiles = make_tiles(tiles)
    for i, t in enumerate(tiles):
        if t["x"] == 0 and t["y"] == 0:
            tiles[i] = tile(0, 0, terrainZ=100)
            break
    result = audit_dump(tiles).to_dict()
    expect(count_category(result, "TERRAIN_SPIKE") == 1,
           f"TERRAIN_SPIKE count: {count_category(result, 'TERRAIN_SPIKE')}, expected 1")


def test_terrain_pit() -> None:
    print("test_terrain_pit")
    tiles = flat_grid(5, 5, -2, -2, terrainZ=100, fluidType=None)
    tiles = make_tiles(tiles)
    for i, t in enumerate(tiles):
        if t["x"] == 0 and t["y"] == 0:
            tiles[i] = tile(0, 0, terrainZ=1)
            break
    result = audit_dump(tiles).to_dict()
    expect(count_category(result, "TERRAIN_PIT") == 1,
           f"TERRAIN_PIT count: {count_category(result, 'TERRAIN_PIT')}, expected 1")


def test_river_chunk_gap() -> None:
    print("test_river_chunk_gap")
    # River at x=15 (chunk edge), dry at x=16 (next chunk), terrain low
    tiles = [
        tile(15, 0, terrainZ=2, fluidType="river", fluidSurf=5),
        tile(16, 0, terrainZ=2, fluidType=None),
    ]
    result = audit_dump(tiles).to_dict()
    expect(count_category(result, "RIVER_CHUNK_GAP") == 1,
           f"RIVER_CHUNK_GAP count: {count_category(result, 'RIVER_CHUNK_GAP')}, expected 1")


def test_river_mouth_drop() -> None:
    print("test_river_mouth_drop")
    tiles = [
        tile(0, 0, terrainZ=5, fluidType="river", fluidSurf=15),
        tile(1, 0, terrainZ=-5, fluidType="ocean", fluidSurf=0),
    ]
    result = audit_dump(tiles).to_dict()
    expect(count_category(result, "RIVER_MOUTH_DROP") == 1,
           f"RIVER_MOUTH_DROP count: {count_category(result, 'RIVER_MOUTH_DROP')}, expected 1")


def test_island_1tile() -> None:
    print("test_island_1tile")
    # 1 dry tile surrounded by ocean
    tiles = [
        tile(-1, 0, terrainZ=-1, fluidType="ocean", fluidSurf=0),
        tile(1, 0, terrainZ=-1, fluidType="ocean", fluidSurf=0),
        tile(0, -1, terrainZ=-1, fluidType="ocean", fluidSurf=0),
        tile(0, 1, terrainZ=-1, fluidType="ocean", fluidSurf=0),
        tile(0, 0, terrainZ=-1, fluidType=None),
    ]
    result = audit_dump(tiles).to_dict()
    expect(count_category(result, "ISLAND_1TILE") == 1,
           f"ISLAND_1TILE count: {count_category(result, 'ISLAND_1TILE')}, expected 1")


def test_isolated_fluid() -> None:
    print("test_isolated_fluid")
    # 1 river tile surrounded by dry
    tiles = [
        tile(-1, 0, terrainZ=10, fluidType=None),
        tile(1, 0, terrainZ=10, fluidType=None),
        tile(0, -1, terrainZ=10, fluidType=None),
        tile(0, 1, terrainZ=10, fluidType=None),
        tile(0, 0, terrainZ=5, fluidType="river", fluidSurf=8),
    ]
    result = audit_dump(tiles).to_dict()
    expect(count_category(result, "ISOLATED_FLUID") == 1,
           f"ISOLATED_FLUID count: {count_category(result, 'ISOLATED_FLUID')}, expected 1")


def test_minbound_leak() -> None:
    print("test_minbound_leak")
    tiles = [
        tile(0, 0, terrainZ=INT64_MIN),  # beyondGlacier=False
    ]
    # Override surfaceZ since our helper doesn't handle minBound right
    tiles[0]["surfaceZ"] = INT64_MIN
    result = audit_dump(tiles).to_dict()
    expect(count_category(result, "MINBOUND_LEAK") == 1,
           f"MINBOUND_LEAK count: {count_category(result, 'MINBOUND_LEAK')}, expected 1")


def test_surface_inconsistent() -> None:
    print("test_surface_inconsistent")
    tiles = [
        {"x": 0, "y": 0, "v": 0,
         "terrainZ": 10, "surfaceZ": 5,  # wrong: should be max(10, None) = 10
         "matId": 64,
         "fluidType": None, "fluidSurf": None,
         "iceSurf": None, "iceMode": None,
         "glacierZone": False, "beyondGlacier": False},
    ]
    result = audit_dump(tiles).to_dict()
    expect(count_category(result, "SURFACE_INCONSISTENT") == 1,
           f"SURFACE_INCONSISTENT count: {count_category(result, 'SURFACE_INCONSISTENT')}, expected 1")


def test_clean_grid() -> None:
    """A clean grid with no issues should return zero bugs."""
    print("test_clean_grid")
    # All ocean below sea level, no issues
    tiles = flat_grid(5, 5, -2, -2, terrainZ=-5, fluidType="ocean", fluidSurf=0)
    result = audit_dump(tiles).to_dict()
    total = sum(result["summary"].values())
    expect(total == 0, f"clean grid should have 0 issues, got {total}: {result['summary']}")


def test_stats() -> None:
    """Fluid stats and elevation stats should be computed correctly."""
    print("test_stats")
    tiles = (
        flat_grid(3, 3, 0, 0, terrainZ=10, fluidType=None) +
        flat_grid(3, 3, 10, 0, terrainZ=-5, fluidType="ocean", fluidSurf=0) +
        flat_grid(3, 3, 20, 0, terrainZ=5, fluidType="river", fluidSurf=8)
    )
    result = audit_dump(tiles)
    d = result.to_dict()
    expect(d["tileCount"] == 27, f"tileCount: {d['tileCount']}, expected 27")
    expect(d["fluidStats"]["dry"] == 9, f"fluidStats.dry: {d['fluidStats']}")
    expect(d["fluidStats"]["ocean"] == 9, f"fluidStats.ocean: {d['fluidStats']}")
    expect(d["fluidStats"]["river"] == 9, f"fluidStats.river: {d['fluidStats']}")
    expect(d["elevationStats"]["min"] == -5, f"min: {d['elevationStats']}")
    expect(d["elevationStats"]["max"] == 10, f"max: {d['elevationStats']}")


def test_determinism_of_audit() -> None:
    """Same input must produce byte-identical audit output."""
    print("test_determinism_of_audit")
    tiles = flat_grid(10, 10, -5, -5, terrainZ=-5, fluidType="ocean", fluidSurf=0)
    a = json.dumps(audit_dump(tiles).to_dict(), sort_keys=True)
    b = json.dumps(audit_dump(tiles).to_dict(), sort_keys=True)
    c = json.dumps(audit_dump(tiles).to_dict(), sort_keys=True)
    expect(a == b and b == c, "audit output not deterministic")


# ----- Runner --------------------------------------------------------------

def main() -> int:
    tests = [
        test_clean_grid,
        test_stats,
        test_determinism_of_audit,
        test_dry_below_sea,
        test_ocean_on_land,
        test_river_under_terrain,
        test_lake_under_terrain,
        test_floating_fluid,
        test_terrain_spike,
        test_terrain_pit,
        test_river_chunk_gap,
        test_river_mouth_drop,
        test_island_1tile,
        test_isolated_fluid,
        test_minbound_leak,
        test_surface_inconsistent,
    ]

    for t in tests:
        t()
        print()

    if FAILURES:
        print(f"\n{len(FAILURES)} test failure(s):")
        for f in FAILURES:
            print(f"  {f}")
        return 1

    print(f"\nAll {len(tests)} test groups passed")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
