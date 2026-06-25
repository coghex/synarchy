#!/usr/bin/env python3
"""Unit tests for world_determinism.py's content-identity check.

Pins the documented contract: the determinism checker treats two dumps
as identical when their tile *content* matches, independent of the order
tiles appear in the array or the order keys appear in each tile object.
A genuine content change must be detected and reported.

Exit codes:
  0 = all tests passed
  1 = one or more tests failed
"""

from __future__ import annotations

import sys
from pathlib import Path
from typing import Any

sys.path.insert(0, str(Path(__file__).resolve().parent))
from world_determinism import (  # type: ignore
    canonical_dump, hash_dump, diff_dumps, field_diff_summary,
)


# ----- Helpers -------------------------------------------------------------

def tile(x: int, y: int, **fields: Any) -> dict[str, Any]:
    base = {"x": x, "y": y, "terrainZ": 1, "matId": 56, "fluidType": None}
    base.update(fields)
    return base


def grid(w: int, h: int) -> list[dict[str, Any]]:
    return [tile(x, y, terrainZ=x + y) for y in range(h) for x in range(w)]


FAILURES: list[str] = []


def expect(cond: bool, msg: str) -> None:
    if not cond:
        FAILURES.append(msg)
        print(f"  FAIL: {msg}")
    else:
        print(f"  OK:   {msg}")


# ----- Tests ---------------------------------------------------------------

def test_array_order_is_ignored() -> None:
    print("test_array_order_is_ignored")
    a = grid(4, 4)
    b = list(reversed(a))  # same tiles, different array order
    expect(a != b, "the two lists really are in a different order")
    expect(hash_dump(a) == hash_dump(b),
           "same tiles in a different array order hash content-identical")


def test_key_order_is_ignored() -> None:
    print("test_key_order_is_ignored")
    a = [tile(0, 0, terrainZ=5, matId=56, fluidType="river", fluidSurf=4)]
    # Same tile, keys inserted in a different order.
    b = [{"fluidSurf": 4, "fluidType": "river", "matId": 56,
          "terrainZ": 5, "y": 0, "x": 0}]
    expect(hash_dump(a) == hash_dump(b),
           "same tile with reordered JSON keys hashes content-identical")


def test_content_change_is_detected() -> None:
    print("test_content_change_is_detected")
    a = grid(4, 4)
    b = grid(4, 4)
    b[5] = tile(b[5]["x"], b[5]["y"], terrainZ=999)  # flip one field
    expect(hash_dump(a) != hash_dump(b),
           "a single differing field changes the content hash")
    diffs = diff_dumps(a, b)
    expect(len(diffs) == 1,
           f"diff reports exactly the one changed tile (got {len(diffs)})")
    summary = field_diff_summary(diffs)
    expect(summary.get("terrainZ") == 1,
           f"field summary attributes the change to terrainZ (got {summary})")


def test_missing_tile_is_detected() -> None:
    print("test_missing_tile_is_detected")
    a = grid(4, 4)
    b = grid(4, 4)[:-1]  # drop one tile
    expect(hash_dump(a) != hash_dump(b),
           "a missing tile changes the content hash")
    summary = field_diff_summary(diff_dumps(a, b))
    expect(summary.get("TILE_MISSING") == 1,
           f"field summary flags the missing tile (got {summary})")


def test_canonical_form_is_stable() -> None:
    print("test_canonical_form_is_stable")
    a = grid(3, 3)
    b = list(reversed(a))
    expect(canonical_dump(a) == canonical_dump(b),
           "canonical_dump is byte-stable across array order")


def main() -> int:
    tests = [
        test_array_order_is_ignored,
        test_key_order_is_ignored,
        test_content_change_is_detected,
        test_missing_tile_is_detected,
        test_canonical_form_is_stable,
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
