#!/usr/bin/env python3
"""World generation audit tool.

Runs the synarchy --dump command (or reads a pre-generated dump) and
categorizes any anomalies found in the world data.

Output is structured JSON, sorted for stable diffing against baselines.

Composition (#2224)
-------------------
This module is the command and the import-compatible facade. It holds
orchestration, statistics, dump I/O, text formatting, argument parsing
and `main()`; the audit's model, its classification policy and its 23
checks live with five owners:

  `world_audit_core`               the constants, `Issue`/`AuditResult`,
                                   and the tile-neighbourhood helpers;
  `world_audit_policy`             BUG/QUALITY classification and the
                                   calibrated quality thresholds;
  `world_audit_checks_columns`     per-tile column integrity (5);
  `world_audit_checks_boundaries`  neighbour-pair boundaries (9);
  `world_audit_checks_regions`     connectivity and topology (7);
  `world_audit_checks_soils`       material placement on slopes (2).

Dependencies run one way: core imports no other owner, each check owner
imports core alone, policy imports no check owner, and only this facade
imports all five. Consumers -- `world_check.py`, `world_stress.py`,
`world_baseline.py` and the audit self-tests -- import this facade, so
every public name below stays importable from `world_audit` whichever
owner now defines it.

`compose_checks()` builds `ALL_CHECKS` from the owners' inventories in
the historical key order and refuses, at import, any arrangement that
would run a shortened audit: an absent or empty owner, a key two owners
both declare, one function registered under two keys, an ordered key no
owner declares, and an owner-declared check the order leaves out.
"""

from __future__ import annotations

import argparse
import json
import statistics
import subprocess
import sys
from collections import Counter
from pathlib import Path
from typing import Any

sys.path.insert(0, str(Path(__file__).resolve().parent))

# ----- Re-exported public surface ------------------------------------------
#
# Every public name this module exposed before the #2224 split stays
# importable from `world_audit`, bound to the same object its owner
# defines. Consumers never import an owner module directly.

from world_audit_core import (  # noqa: E402,F401
    SEA_LEVEL, CHUNK_SIZE, INT64_MIN,
    Issue, AuditResult,
    chunk_of, crosses_chunk_boundary, neighbors4,
)
from world_audit_policy import (  # noqa: E402,F401
    BUG_CATEGORIES, QUALITY_CATEGORIES, QUALITY_THRESHOLDS,
    severity_of, classify_category,
)
from world_audit_checks_columns import (  # noqa: E402,F401
    FLOATING_FLUID_DEPTH, OCEAN_ON_LAND_THRESHOLD,
    check_ocean_on_land, check_fluid_under_terrain, check_floating_fluid,
    check_minbound_leak, check_surface_inconsistent,
)
from world_audit_checks_boundaries import (  # noqa: E402,F401
    SPIKE_THRESHOLD, RIVER_MOUTH_DROP_THRESHOLD,
    check_lava_rim_containment, check_terrain_spikes_pits,
    check_river_chunk_gaps, check_river_mouth_drop,
    check_water_above_land, check_water_cliff, check_water_water_cliff,
    check_mid_river_cliff, check_floating_water,
)
from world_audit_checks_regions import (  # noqa: E402,F401
    check_dry_below_sea, check_island_1tile, check_lake_hole,
    check_submerged_bump, check_multi_island, check_flat_isolated_water,
    check_isolated_fluid,
)
from world_audit_checks_soils import (  # noqa: E402,F401
    WETLAND_MATS, DESERT_MATS,
    check_wetland_on_slope, check_desert_soil_on_slope,
)

import world_audit_checks_boundaries as _checks_boundaries  # noqa: E402
import world_audit_checks_columns as _checks_columns  # noqa: E402
import world_audit_checks_regions as _checks_regions  # noqa: E402
import world_audit_checks_soils as _checks_soils  # noqa: E402


# ----- Audit driver --------------------------------------------------------

#: The four check owners by name. Every one must be present and must
#: declare a non-empty `CHECKS` inventory for the registry to compose.
CHECK_OWNERS = {
    "columns": _checks_columns,
    "boundaries": _checks_boundaries,
    "regions": _checks_regions,
    "soils": _checks_soils,
}

#: The registry key order `ALL_CHECKS` has always run in. It predates the
#: #2224 split and interleaves the owners, so the order lives here rather
#: than falling out of the order the owners are composed in. Baselines and
#: the real-output fixture are diffed against runs in this order.
CHECK_ORDER = (
    "DRY_BELOW_SEA",
    "OCEAN_ON_LAND",
    "RIVER_UNDER_TERRAIN",
    "FLOATING_FLUID",
    "LAVA_RIM_CONTAINMENT",
    "TERRAIN_SPIKES_PITS",
    "RIVER_CHUNK_GAP",
    "RIVER_MOUTH_DROP",
    "ISLAND_1TILE",
    "LAKE_HOLE",
    "SUBMERGED_BUMP",
    "WATER_ABOVE_LAND",
    "WATER_CLIFF",
    "WATER_WATER_CLIFF",
    "MID_RIVER_CLIFF",
    "FLOATING_WATER",
    "MULTI_ISLAND",
    "FLAT_ISOLATED_WATER",
    "ISOLATED_FLUID",
    "MINBOUND_LEAK",
    "SURFACE_INCONSISTENT",
    "WETLAND_ON_SLOPE",
    "DESERT_SOIL_ON_SLOPE",
)


class RegistryError(Exception):
    """The check-owner inventories no longer compose into the full audit."""


def check_inventories(
        owners: dict[str, Any] | None = None) -> dict[str, dict[str, Any]]:
    """Every owner's declared `CHECKS`, refusing an absent or empty one.

    An owner missing from the mapping, one that has stopped declaring
    `CHECKS`, and one whose inventory is empty are all refused here --
    before the order is consulted -- so a vanished owner is reported as
    itself, by name, rather than as whatever the order notices second.
    """
    owners = CHECK_OWNERS if owners is None else owners
    found: dict[str, dict[str, Any]] = {}
    for name in CHECK_OWNERS:
        module = owners.get(name)
        if module is None:
            raise RegistryError(f"required check owner {name!r} is absent")
        checks = getattr(module, "CHECKS", None)
        if checks is None:
            raise RegistryError(
                f"check owner {name!r} ({module.__name__}) declares no "
                f"CHECKS inventory")
        if not checks:
            raise RegistryError(
                f"check owner {name!r} ({module.__name__}) declares an empty "
                f"CHECKS inventory -- refusing to run a shortened audit")
        found[name] = dict(checks)
    return found


def compose_checks(owners: dict[str, Any] | None = None,
                   order: tuple[str, ...] = CHECK_ORDER) -> dict[str, Any]:
    """The full check registry, checked against every owner's inventory.

    Checks both directions, because either drift silently shortens the
    audit: a key two owners both declare, one check function registered
    under two keys, an ordered key no owner declares, a key the order
    repeats, and an owner-declared check the order never runs all fail
    here. Every check belongs to exactly one owner and runs exactly once.
    """
    by_owner = check_inventories(owners)

    declared: dict[str, str] = {}
    registered: dict[Any, str] = {}
    for name, checks in by_owner.items():
        for key, check_fn in checks.items():
            if key in declared:
                raise RegistryError(
                    f"check key {key!r} is declared by both "
                    f"{declared[key]!r} and {name!r}")
            declared[key] = name
            prior = registered.get(check_fn)
            if prior is not None:
                raise RegistryError(
                    f"check keys {prior!r} and {key!r} both register "
                    f"{getattr(check_fn, '__qualname__', check_fn)!r}")
            registered[check_fn] = key

    registry: dict[str, Any] = {}
    for key in order:
        owner = declared.get(key)
        if owner is None:
            raise RegistryError(
                f"the registry order includes {key!r}, which no owner "
                f"declares in its CHECKS")
        if key in registry:
            raise RegistryError(
                f"the registry order includes {key!r} more than once")
        registry[key] = by_owner[owner][key]

    omitted = sorted(f"{owner}:{key}" for key, owner in declared.items()
                     if key not in registry)
    if omitted:
        raise RegistryError(
            f"owner-declared checks the registry order never runs: {omitted}")
    return registry


ALL_CHECKS = compose_checks()


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
