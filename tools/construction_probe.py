#!/usr/bin/env python3
"""Headless construction build-job probe (#96).

Verifies the construct_job AI end-to-end on a flat arena world, WITHOUT a
GPU or a human watching: acolytes claim construction designations (#95),
source materials (inventory → ground items → technomule), pour progress
into the job (ghost solidifies), place the structure piece, and stake
designated buildings for the existing deliver/build machinery.

Phases (each spawns its own units and destroys them afterwards so the
next phase's utility scan is clean):
  1. inventory : two floor designations; an acolyte carrying the plates
                 claims (status observable as "claimed"), builds, places
                 both floors, and the designations clear.
  2. ground    : one wall designation; the bars are GROUND items — the
                 acolyte hauls them first (sourcing ladder rung 2).
  3. release   : the claimant is destroyed mid-job; the sweep releases
                 the claim ("pending" again — observable), and a second
                 acolyte picks the job up and finishes it.
  4. occupied  : #805 — re-designating an already-built floor's slot is
                 rejected (no job, outcome stream says so), a DIFFERENT
                 slot on that same tile still designates/builds normally
                 (coexistence), and a slot that fills in AFTER
                 designation but BEFORE a claimant pays materials is
                 cancelled rather than paid for.
  5. stake     : a building designation; the acolyte walks up and stakes
                 it (building.spawn) and the blueprint completes. Runs
                 LAST: the staked portal spawns its roster, which would
                 contaminate later phases.

Usage:
  python3 tools/construction_probe.py [--port 9377] [--phase all]

Exit code 0 = all checks passed.
"""
from __future__ import annotations

import argparse
import glob
import json
import socket
import subprocess
import sys
import time
from probelib import clear_find_water, quit_engine, boot, send, send_json

LOG = "/tmp/construction_probe_engine.log"


def bootstrap(port: int) -> None:
    """Load defs + the flat arena (the loading screen doesn't run headless).
    unit_ai is auto-loaded at boot and IS the machinery under test, so it
    stays live (unlike movement_probe, which neutralises it)."""
    loaders = [
        ("data/substances/*.yaml", "engine.loadSubstanceYaml"),
        ("data/items/*.yaml",      "engine.loadItemYaml"),
        ("data/equipment/*.yaml",  "engine.loadEquipmentYaml"),
        ("data/materials/*.yaml",  "engine.loadMaterialYaml"),
        ("data/units/*.yaml",      "engine.loadUnitYaml"),
        ("data/buildings/*.yaml",  "engine.loadBuildingYaml"),
    ]
    for pattern, fn in loaders:
        for path in sorted(glob.glob(pattern)):
            send(port, f"{fn}('{path}'); return 'ok'")
    send(port,
         "return require('scripts.movement_arena').buildCourse('flat').name")
    # world.show is queued on the world thread — wait until the arena is
    # actually the active world before designating (a designate against a
    # not-yet-active page id is a silent no-op).
    if not poll_until(port, 30, lambda: wid(port)):
        sys.exit("arena page never became the active world")
    # Make sure the chunks around the test sites exist before designating
    # (designation skips unloaded-chunk tiles).
    send(port, "return world.loadChunksInRegion(-1, -1, 2, 2)")
    send(port, "return world.waitForChunks(60)", timeout=65.0)


def wid(port: int) -> str | None:
    """Active world page id, or None while world.show is still queued.
    The console JSON-encodes strings (quotes included) and prints null
    for nil — both need unwrapping before the id is re-usable in Lua."""
    raw = send(port, "return world.getActiveWorldId()")
    raw = raw.strip().strip('"')
    return raw if raw and raw not in ("null", "nil") else None


def designation_at(port: int, x: int, y: int):
    return send_json(
        port,
        f"return construction.getDesignationAt(world.getActiveWorldId(), {x}, {y})")


def spawn_acolyte(port: int, x: float, y: float) -> int:
    uid = send(port, f"return unit.spawn('acolyte', {x}, {y})")
    try:
        n = int(float(uid))
    except ValueError:
        sys.exit(f"unit.spawn failed: {uid!r}")
    # Acolytes spawn at (or beyond) carrying capacity — the engine
    # already drops kit at spawn to fit. Shed the heavy tools so the
    # probe's added build materials don't push the unit over capacity
    # (an over-capacity unit can't haul, which reads as a stuck job).
    for it in ("pick_steel", "shovel_steel", "axe_steel",
               "rations", "rations"):
        send(port, f"unit.removeItem({n}, '{it}'); return 'ok'")
    # Retire the spawn-seeded find_water goal: the arena has no water,
    # so the goal never completes and its search utility (3.0) can edge
    # out a construct job a few tiles away — an artifact of the
    # waterless test world, not the arbitration under test.
    if not clear_find_water(port, n):
        sys.exit(f"unit {n} never got AI state")
    return n


def destroy_unit(port: int, uid: int) -> None:
    send(port, f"unit.destroy({uid}); return 'ok'")


def poll_until(port: int, seconds: float, fn):
    """Poll fn() every 0.3 s until truthy or the budget runs out."""
    deadline = time.time() + seconds
    while time.time() < deadline:
        v = fn()
        if v:
            return v
        time.sleep(0.3)
    return None


CHECKS: list[tuple[str, bool]] = []


def check(label: str, ok: bool) -> None:
    CHECKS.append((label, bool(ok)))
    print(f"  [{'PASS' if ok else 'FAIL'}] {label}")


# --- phases ------------------------------------------------------------


def phase_inventory(port: int) -> None:
    """Two floors, materials already in the builder's inventory."""
    print("\n[phase 1] structure job from INVENTORY (2 floors)")
    w = wid(port)
    send(port, f"construction.designate('{w}', 8, 8, 9, 8, "
               "'structure', 'dungeon_1', 'floor'); return 'ok'")
    time.sleep(0.5)
    check("both tiles designated",
          designation_at(port, 8, 8) is not None
          and designation_at(port, 9, 8) is not None)

    uid = spawn_acolyte(port, 5.5, 8.5)
    send(port, f"unit.addItem({uid}, 'steel_plate', 0); "
               f"unit.addItem({uid}, 'steel_plate', 0); return 'ok'")

    claimed = poll_until(port, 20, lambda: (
        (designation_at(port, 8, 8) or {}).get("status") == "claimed"
        or (designation_at(port, 9, 8) or {}).get("status") == "claimed"))
    check("a designation became 'claimed'", claimed is not None)

    progressed = poll_until(port, 30, lambda: any(
        (designation_at(port, x, 8) or {}).get("progress", 0) > 0
        for x in (8, 9)))
    check("build progress accrued on the designation", progressed is not None)

    # The mining-style corner-progress display: once ≥ 2 corners have
    # drained (past ~40% of the job) the tile's slope mask goes
    # non-zero, exactly like a mid-dig tile.
    sloped = poll_until(port, 30, lambda: any(
        send(port, f"return world.getSlopeAt({x}, 8)") not in ("0", "nil")
        for x in (8, 9)))
    check("corner-progress display visible mid-build (slope mask set)",
          sloped is not None)

    done = poll_until(port, 60, lambda: (
        send(port, "return structure.hasAt(8, 8, 'floor')") == "true"
        and send(port, "return structure.hasAt(9, 8, 'floor')") == "true"
        and designation_at(port, 8, 8) is None
        and designation_at(port, 9, 8) is None))
    check("both floors placed and designations cleared", done is not None)
    check("tiles returned to flat after completion",
          send(port, "return world.getSlopeAt(8, 8)") == "0"
          and send(port, "return world.getSlopeAt(9, 8)") == "0")
    destroy_unit(port, uid)


def phase_ground(port: int) -> None:
    """One wall; the two steel bars are ground items the builder hauls."""
    print("\n[phase 2] structure job from GROUND items (1 wall)")
    w = wid(port)
    send(port, "item.spawnGround('steel_bar', 12.5, 14.5); "
               "item.spawnGround('steel_bar', 13.5, 15.5); return 'ok'")
    send(port, f"construction.designate('{w}', 8, 14, 8, 14, "
               "'structure', 'dungeon_1', 'wall', 'ne'); return 'ok'")
    time.sleep(0.5)
    check("wall tile designated", designation_at(port, 8, 14) is not None)

    uid = spawn_acolyte(port, 10.5, 14.5)
    done = poll_until(port, 90, lambda: (
        send(port, "return structure.hasAt(8, 14, 'wall_ne')") == "true"
        and designation_at(port, 8, 14) is None))
    check("wall placed from ground-sourced bars", done is not None)
    check("ground bars were consumed",
          send(port,
               "local n = 0; "
               "for _, g in ipairs(item.listGround() or {}) do "
               "if g.defName == 'steel_bar' then n = n + 1 end end; "
               "return n") == "0")
    destroy_unit(port, uid)


def phase_occupied(port: int) -> None:
    """#805: an already-occupied structure slot must not be re-designated
    (or paid for if it fills in mid-job), while a compatible slot on the
    same tile keeps working normally."""
    print("\n[phase 5] occupied structure slots (#805)")
    w = wid(port)

    # --- a slot that's already built must not accept a re-designation.
    send(port, "require('scripts.structures').floor(8, 30); return 'ok'")
    built = poll_until(port, 10, lambda: send(
        port, "return structure.hasAt(8, 30, 'floor')") == "true")
    check("fixture floor placed directly", built is not None)

    send(port, "return debug.drainActionOutcomes()")  # clear the slate
    send(port, f"construction.designate('{w}', 8, 30, 8, 30, "
               "'structure', 'dungeon_1', 'floor'); return 'ok'")
    time.sleep(0.5)
    check("re-designating an already-built floor creates no job",
          designation_at(port, 8, 30) is None)
    check("the existing floor is untouched",
          send(port, "return structure.hasAt(8, 30, 'floor')") == "true")
    outcomes = send_json(port, "return debug.drainActionOutcomes()")
    if not isinstance(outcomes, list):
        outcomes = []
    matches = [o for o in outcomes
               if isinstance(o, dict) and o.get("kind") == "construction.designate"
               and (o.get("where") or {}).get("x") == 8
               and (o.get("where") or {}).get("y") == 30]
    check("occupied-slot designation reports a non-accepted outcome",
          bool(matches) and all(o.get("outcome") != "accepted" for o in matches))

    # --- coexistence: a DIFFERENT slot on the same tile still designates
    # and builds normally, and never disturbs the existing floor.
    send(port, f"construction.designate('{w}', 8, 30, 8, 30, "
               "'structure', 'dungeon_1', 'wall', 'ne'); return 'ok'")
    time.sleep(0.5)
    check("a compatible slot on the same tile still designates",
          designation_at(port, 8, 30) is not None)

    coUid = spawn_acolyte(port, 5.5, 30.5)
    send(port, f"unit.addItem({coUid}, 'steel_bar', 0); "
               f"unit.addItem({coUid}, 'steel_bar', 0); return 'ok'")
    done = poll_until(port, 60, lambda: (
        send(port, "return structure.hasAt(8, 30, 'wall_ne')") == "true"
        and designation_at(port, 8, 30) is None))
    check("wall built alongside the pre-existing floor", done is not None)
    check("floor still present after the coexisting wall build",
          send(port, "return structure.hasAt(8, 30, 'floor')") == "true")
    destroy_unit(port, coUid)

    # --- race: the requested slot fills in AFTER designation but BEFORE
    # the claimant pays materials — must resolve without paying/overwriting.
    send(port, f"construction.designate('{w}', 8, 36, 8, 36, "
               "'structure', 'dungeon_1', 'floor'); return 'ok'")
    time.sleep(0.5)
    check("race-case tile designated", designation_at(port, 8, 36) is not None)

    racer = spawn_acolyte(port, 1.5, 36.5)
    send(port, f"unit.addItem({racer}, 'steel_plate', 0); return 'ok'")
    claimed = poll_until(port, 20, lambda: (
        (designation_at(port, 8, 36) or {}).get("status") == "claimed"))
    check("racer claimed the tile before the slot filled", claimed is not None)

    # Fill the slot out from under the claimant while it's still walking
    # over — simulates a second worker (or a re-designation) winning the
    # race for the same slot.
    send(port, "require('scripts.structures').floor(8, 36); return 'ok'")
    filled = poll_until(port, 10, lambda: send(
        port, "return structure.hasAt(8, 36, 'floor')") == "true")
    check("competing floor landed mid-job", filled is not None)

    resolved = poll_until(port, 60, lambda: designation_at(port, 8, 36) is None)
    check("raced designation resolves (cancelled), no stuck job",
          resolved is not None)
    check("claimant never paid its material",
          send(port, f"local n = 0; for _, it in ipairs(unit.getInventory({racer}) "
                     "or {}) do if it.defName == 'steel_plate' then n = n + 1 end "
                     "end; return n") == "1")
    destroy_unit(port, racer)


def phase_stake(port: int) -> None:
    """A building designation gets staked into a real Appearing building."""
    print("\n[phase 3] building blueprint STAKED (acolyte_portal)")
    w = wid(port)
    send(port, f"construction.designate('{w}', 20, 8, 20, 8, "
               "'building', 'acolyte_portal'); return 'ok'")
    time.sleep(0.5)
    check("building tile designated", designation_at(port, 20, 8) is not None)

    uid = spawn_acolyte(port, 16.5, 8.5)
    # Booleans come back unquoted from the console; strings are
    # JSON-quoted — so return a boolean.
    staked = poll_until(port, 45, lambda: (
        designation_at(port, 20, 8) is None
        and send(port,
                 "local ok = false; "
                 "for _, b in ipairs(building.getActiveIds() or {}) do "
                 "local i = building.getInfo(b); "
                 "if i and i.defName == 'acolyte_portal' then ok = true end "
                 "end; return ok") == "true"))
    check("blueprint became a real building and cleared", staked is not None)
    destroy_unit(port, uid)


def phase_release(port: int) -> None:
    """Kill the claimant mid-job; the claim must release and another
    acolyte must finish the tile."""
    print("\n[phase 4] dead claimant RELEASES the job (floor)")
    w = wid(port)
    send(port, f"construction.designate('{w}', 8, 22, 8, 22, "
               "'structure', 'dungeon_1', 'floor'); return 'ok'")
    time.sleep(0.5)

    a = spawn_acolyte(port, 3.5, 22.5)   # a few tiles out: claim precedes arrival
    send(port, f"unit.addItem({a}, 'steel_plate', 0); return 'ok'")
    claimed = poll_until(port, 20, lambda: (
        (designation_at(port, 8, 22) or {}).get("status") == "claimed"))
    check("first acolyte claimed the job", claimed is not None)
    if claimed is None:
        print("  (debug) A aiState:",
              send(port, f"local s = require('scripts.unit_ai').getState({a}); "
                         f"return s and {{action=s.currentAction, "
                         f"job=s.constructJob ~= nil, "
                         f"cand=s.constructCandidate ~= nil}} or 'none'"))
        print("  (debug) designation:", designation_at(port, 8, 22))

    destroy_unit(port, a)
    # A materials-less scout triggers the sweep (any scanning acolyte
    # releases a dead claimant's job) but can't take the job itself, so
    # the released "pending" state is observable.
    scout = spawn_acolyte(port, 12.5, 22.5)
    released = poll_until(port, 30, lambda: (
        (designation_at(port, 8, 22) or {}).get("status") == "pending"))
    check("claim released back to 'pending' after claimant death",
          released is not None)

    send(port, f"unit.addItem({scout}, 'steel_plate', 0); return 'ok'")
    done = poll_until(port, 60, lambda: (
        send(port, "return structure.hasAt(8, 22, 'floor')") == "true"
        and designation_at(port, 8, 22) is None))
    check("second acolyte finished the released job", done is not None)
    destroy_unit(port, scout)


# Order matters: the stake phase runs LAST — the staked portal spawns
# its starting roster (building_spawn.lua auto-loads at boot), and six
# fresh acolytes + a stocked technomule would contaminate any phase
# that runs after it (they claim jobs and source from the mule).
PHASES = {
    "inventory": phase_inventory,
    "ground": phase_ground,
    "release": phase_release,
    "occupied": phase_occupied,
    "stake": phase_stake,
}


def main() -> int:
    ap = argparse.ArgumentParser(description=__doc__)
    ap.add_argument("--port", type=int, default=9377)
    ap.add_argument("--phase", default="all", choices=["all"] + list(PHASES))
    args = ap.parse_args()

    proc = boot(args.port, log=LOG)
    try:
        bootstrap(args.port)
        if not wid(args.port):
            print("FAIL: no active world after arena build", file=sys.stderr)
            return 2
        todo = PHASES.values() if args.phase == "all" else [PHASES[args.phase]]
        for phase in todo:
            phase(args.port)
    finally:
        quit_engine(args.port, proc)
        try:
            proc.wait(timeout=10)
        except subprocess.TimeoutExpired:
            proc.kill()

    failed = [label for label, ok in CHECKS if not ok]
    print(f"\n{len(CHECKS) - len(failed)}/{len(CHECKS)} checks passed"
          + (f"; FAILED: {failed}" if failed else ""))
    return 1 if failed else 0


if __name__ == "__main__":
    sys.exit(main())
