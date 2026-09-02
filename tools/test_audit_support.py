#!/usr/bin/env python3
"""Shared support for `tools/test_audit.py`'s six case owners (#2070).

This is the ONE source of what two or more of the owner modules --
`test_audit_categories`, `test_audit_world_audit`, `test_audit_world_check`,
`test_audit_content_hash`, `test_audit_strict_capture` and
`test_audit_missing_baseline` -- share:

* the assertion facility. `FAILURES` is the ONE list `expect` appends to,
  and since #1922 both are `tools/selftestlib.py`'s, re-exported here so
  the owners import them from the single place they already import
  everything else shared from. Six owners each holding a private
  accumulator would let the aggregate exit 0 while a sibling owner had
  recorded a failure;
* the audit-clean hash fixtures -- `HASH_ENTRY`, `hash_tile`,
  `hash_dump_fixture` and `capture_hash_baseline`, which build a
  baseline the way `world_baseline.py` really builds one. The
  content-hash owner drives its gate over them, the strict-capture owner
  proves a hash-racy seed is still captured over them, and the
  missing-baseline owner puts a real drifting seed beside a missing one
  with them;
* `expect_exit` and `expect_output_contains`, the two assertions over a
  captured tool run's exit status and output, which the strict-capture
  and missing-baseline owners both make after driving a real `main()`.

Deliberately NOT here: anything with exactly one consumer. The synthetic
grid builders (`tile`, `flat_grid`, `make_tiles`, `count_category`,
`issue_coords`) serve only the world_audit owner and live there; the
strict-capture fixtures and the two `main()` harnesses
(`run_world_baseline_main`, `run_world_check_main`) are structurally
distinct and each stays with its owner. A shared module whose contents
serve one owner is a catch-all, not support.

Nothing here runs a test group and this module is not a gate of its own:
`python3 tools/test_audit.py` remains the only invocation, in CI and in
`make ci` alike.
"""
from __future__ import annotations

import sys
from pathlib import Path
from typing import Any

sys.path.insert(0, str(Path(__file__).resolve().parent))
import world_baseline  # type: ignore  # noqa: E402
from selftestlib import FAILURES, expect  # noqa: E402

__all__ = [
    "FAILURES",
    "HASH_ENTRY",
    "capture_hash_baseline",
    "expect",
    "expect_exit",
    "expect_output_contains",
    "hash_dump_fixture",
    "hash_tile",
]


HASH_ENTRY = {"seed": 4242, "world_size": 32, "region": [-1, -1, 1, 1]}


def hash_tile(x: int, y: int, matId: int = 64) -> dict[str, Any]:
    """A dry, flat, audit-clean land tile."""
    return {
        "x": x, "y": y, "v": x + y,
        "terrainZ": 10, "surfaceZ": 10,
        "matId": matId,
        "fluidType": None, "fluidSurf": None,
        "iceSurf": None, "iceMode": None,
        "glacierZone": False, "beyondGlacier": False,
    }


def hash_dump_fixture(matId_at_index: tuple[int, int] | None = None
                      ) -> list[dict[str, Any]]:
    """A 6x6 flat dry grid, optionally with one tile's matId changed.

    matId is the point: 64 (muck) and 70 both sit on flat terrain, so
    neither trips WETLAND_ON_SLOPE or DESERT_SOIL_ON_SLOPE, and matId
    appears in no statistic world_check compares. A one-tile change is
    therefore invisible to tileCount, elevationStats, fluidStats and the
    audit summary alike — exactly the drift class the content hash
    exists to catch.
    """
    tiles = [hash_tile(x, y) for y in range(6) for x in range(6)]
    if matId_at_index is not None:
        index, matId = matId_at_index
        tiles[index] = dict(tiles[index], matId=matId)
    return tiles


def capture_hash_baseline(dumps: list[list[dict[str, Any]]]) -> dict[str, Any]:
    """Build a baseline the way world_baseline.py really builds one."""
    pending = list(dumps)
    original = world_baseline.run_dump
    world_baseline.run_dump = lambda *a, **k: pending.pop(0)
    try:
        return world_baseline.capture_seed(
            HASH_ENTRY["seed"], HASH_ENTRY["world_size"],
            tuple(HASH_ENTRY["region"]), len(dumps),
        )
    finally:
        world_baseline.run_dump = original


# A captured run is attached to the message only when the assertion
# actually fails: under --verbose expect() narrates every passing
# message, and these cases would otherwise bury the rest of the
# suite's output.

def expect_exit(code: int, expected: int, output: str, label: str) -> None:
    if code == expected:
        expect(True, f"{label}: exit {expected}")
    else:
        expect(False, f"{label}: expected exit {expected}, got {code}. "
                      f"Run output: {output!r}")


def expect_output_contains(needle: str, output: str, label: str) -> None:
    if needle in output:
        expect(True, f"{label}: output names {needle}")
    else:
        expect(False, f"{label}: output must name {needle}. "
                      f"Run output: {output!r}")


