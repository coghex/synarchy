#!/usr/bin/env python3
"""Headless portal-location-exclusion probe (#778).

A starting building (`bdIsStarting` — the acolyte_portal) must not be
placeable inside a placed location's absolute bounds (#777); ordinary
construction is unaffected. Boots a real generated world, finds a real
`ruin_small` from the world-gen overlay, and proves end to end:

  1. the ruin is known from world.listPlacedLocations()'s overlay-derived
     bounds;
  2. building.canPlaceAt("acolyte_portal", ...) at those bounds is
     rejected with the "inside a location's bounds" reason;
  3. building.spawn cannot bypass the rejection — it shares the exact
     same Building.Placement.canPlaceAt the preview call validates
     through;
  4. an adjacent, non-overlapping, unoccupied tile remains eligible;
  5. an ordinary non-starting building (cargo_hold_S) is NOT rejected at
     the same bounds-overlapping coordinate;
  6. none of the above changes the ruin's own content or stamped
     geometry.

Pure canPlaceAt logic (boundary/corner/adjacency/seam coverage, the four
distinct rejection reasons, and the ghost-tint validity contract) is
covered by the "Portal location exclusion" Hspec group
(test-headless/Test/Headless/Building/Placement.hs) — this probe is the
real-engine/real-worldgen end-to-end complement, reusing
tools/location_content_probe.py's world/ruin bootstrap.

Usage:
  python3 tools/portal_location_probe.py
  python3 tools/portal_location_probe.py --seed 42 --size 64 --port 9192

Exit code 0 = all checks passed.
"""
from __future__ import annotations

import argparse
import sys

from probelib import quit_engine, boot, send
from location_content_probe import (
    load_defs, gen_world, placed_ready, wait_floor, ruin_geometry,
    spawn_counts,
)

LOG = "/tmp/portal_location_engine.log"

PORTAL = "acolyte_portal"
ORDINARY = "cargo_hold_S"


def can_place_at(port: int, def_name: str, gx: int, gy: int) -> tuple[bool, str | None]:
    r = send(port,
              f"local v,r = building.canPlaceAt('{def_name}', {gx}, {gy}); "
              f"return tostring(v) .. '|' .. tostring(r)").strip('"')
    valid_s, _, reason_s = r.partition("|")
    valid = valid_s == "true"
    reason = None if reason_s in ("nil", "") else reason_s
    return valid, reason


def try_spawn(port: int, def_name: str, gx: int, gy: int) -> str | None:
    r = send(port, f"local id = building.spawn('{def_name}', {gx}, {gy}); "
                   f"return tostring(id)").strip('"')
    return None if r == "nil" else r


def main() -> int:
    ap = argparse.ArgumentParser(description=__doc__)
    ap.add_argument("--seed", type=int, default=42)
    ap.add_argument("--size", type=int, default=64)
    ap.add_argument("--port", type=int, default=9192)
    args = ap.parse_args()

    failures: list[str] = []

    proc = boot(args.port, log=LOG)
    try:
        load_defs(args.port)
        gen_world(args.port, "pw", args.seed, args.size)
        located = placed_ready(args.port)
        ruins = [e for e in located if e.get("id") == "ruin_small" and "bounds" in e]
        print(f"world (seed {args.seed}): {len(ruins)} ruin_small placed with known bounds")
        if not ruins:
            failures.append(
                "no ruin_small with resolvable bounds placed — cannot test exclusion")
        else:
            ruin = ruins[0]
            bounds = ruin["bounds"]
            gx, gy, cx, cy = ruin["gx"], ruin["gy"], ruin["cx"], ruin["cy"]
            print(f"PASS: ruin_small known from the overlay at ({gx},{gy}), "
                  f"bounds {bounds}")

            # Load the ruin's own chunk (so it stamps + spawns content, for
            # the before/after snapshot below) plus a small halo of
            # neighbouring chunks so an "adjacent" candidate tile below is
            # never rejected merely for an unloaded chunk.
            send(args.port,
                 f"return world.loadChunksInRegion({cx-1},{cy-1},{cx+1},{cy+1})")
            send(args.port, "return world.waitForChunks(60)", timeout=65)

            if not wait_floor(args.port, gx, gy):
                failures.append(f"ruin at ({gx},{gy}) never stamped its geometry")

            geom_before = ruin_geometry(args.port, gx, gy)
            counts_before = spawn_counts(args.port)

            # ---- portal exclusion at the ruin's own anchor tile (deep
            #      inside bounds, on the room's stamped floor). ----
            valid, reason = can_place_at(args.port, PORTAL, gx, gy)
            if not valid and reason == "inside a location's bounds":
                print(f"PASS: building.canPlaceAt('{PORTAL}', {gx}, {gy}) "
                      f"rejected — reason: {reason}")
            else:
                failures.append(
                    f"expected portal rejection at the ruin anchor, got "
                    f"valid={valid} reason={reason}")

            # ---- direct building.spawn cannot bypass the rejection —
            #      it shares the exact validator the preview call above
            #      just used. ----
            spawned = try_spawn(args.port, PORTAL, gx, gy)
            if spawned is None:
                print(f"PASS: building.spawn('{PORTAL}', {gx}, {gy}) also "
                      f"refused (no bypass via the authoritative path)")
            else:
                failures.append(
                    f"building.spawn bypassed the location rejection: id={spawned}")

            # ---- an ordinary non-starting building is NOT rejected at the
            #      same bounds-overlapping coordinate. ----
            ord_valid, ord_reason = can_place_at(args.port, ORDINARY, gx, gy)
            if ord_valid:
                print(f"PASS: non-starting '{ORDINARY}' is placeable inside "
                      f"the same location bounds (locations remain "
                      f"occupiable/repairable by ordinary construction)")
            else:
                failures.append(
                    f"non-starting '{ORDINARY}' unexpectedly rejected inside "
                    f"location bounds: {ord_reason}")

            # ---- an adjacent, non-overlapping, unoccupied tile remains
            #      eligible. acolyte_portal's footprint is 1x1, so terrain
            #      flatness can never fail it here — the only way this
            #      could wrongly reject is a stale/incorrect bounds check,
            #      so any of these candidates failing is a real bug. ----
            max_x, max_y = bounds["max_x"], bounds["max_y"]
            candidates = [(max_x + 3, gy), (max_x + 4, gy),
                          (gx, max_y + 3), (gx, max_y + 4)]
            adjacent_ok = None
            for agx, agy in candidates:
                v, r = can_place_at(args.port, PORTAL, agx, agy)
                if v:
                    adjacent_ok = (agx, agy)
                    break
            if adjacent_ok:
                print(f"PASS: an adjacent non-overlapping tile "
                      f"{adjacent_ok} remains eligible")
            else:
                failures.append(
                    f"no adjacent non-overlapping candidate out of "
                    f"{candidates} was eligible")

            # ---- none of the above touched the ruin's own content or
            #      geometry. ----
            geom_after = ruin_geometry(args.port, gx, gy)
            counts_after = spawn_counts(args.port)
            if geom_after == geom_before:
                print(f"PASS: ruin geometry unchanged ({geom_after})")
            else:
                failures.append(
                    f"ruin geometry changed: before={geom_before} after={geom_after}")
            if counts_after == counts_before:
                print(f"PASS: ruin content unchanged ({counts_after})")
            else:
                failures.append(
                    f"ruin content changed: before={counts_before} "
                    f"after={counts_after}")
    finally:
        quit_engine(args.port, proc)

    print("-" * 56)
    if failures:
        for f in failures:
            print(f"FAIL: {f}", file=sys.stderr)
        return 1
    print("ALL CHECKS PASSED")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
