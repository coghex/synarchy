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
  6. paid_death: #799 — a claimant that dies AFTER paying (progress > 0)
                 leaves the designation's durable 'paid' marker set; a
                 materials-less replacement still finishes the job,
                 proving the cost isn't charged a second time.
  7. preemption: #799 — an ordinary mid-build interruption (the same
                 reset constructOnExit performs) must not re-attempt
                 payment; the claimant carries exactly the required
                 amount with no ground/mule fallback, so a duplicate
                 charge would stall the job outright.
  8. save_load : #799 — a paid, partially-built designation survives a
                 save/load round-trip (paid + progress intact) even
                 though the paying unit does NOT survive to reload with
                 it; a materials-less unit finishes it after load.
  9. cancel    : #799 — the build-tool's right-click cancel refunds an
                 already-PAID structure's materials to the ground (and
                 never fabricates materials for an unpaid one); a
                 placement failure (a post's floor pulled out from under
                 it) refunds too, plus a player-observable notification,
                 instead of completing with the materials simply gone.
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
    # (designation skips unloaded-chunk tiles). The flat arena's footprint
    # is a fixed 5x5 chunk grid centred on the origin (arenaRadius = 2,
    # World.Generate.Arena) — chunks -2..2 (tiles -32..47). Requesting
    # chunks OUTSIDE that footprint falls through to the real terrain
    # generator against the arena's synthetic gen params and crashes the
    # world thread, so this loads the arena's FULL footprint (needed for
    # the #799 phases' negative-Y sites below) and no further.
    send(port, "return world.loadChunksInRegion(-2, -2, 2, 2)")
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


def pick_tile(port: int, sx: int, sy: int) -> tuple[int, int]:
    """world.pickTile(screenX, screenY) -> (gx, gy) — the SAME hit test
    buildTool.handleMouseDown runs on a real click (mirrors wire_probe.py's
    helper of the same name)."""
    raw = send(port, f"return world.pickTile({sx}, {sy})")
    parts = raw.split()
    return int(float(parts[0])), int(float(parts[1]))


def ground_count(port: int, def_name: str) -> int:
    raw = send(port,
                "local n=0; for _,g in ipairs(item.listGround() or {}) do "
                f"if g.defName=='{def_name}' then n=n+1 end end; return n")
    return int(float(raw))


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


def poll_fast(seconds: float, fn):
    """Like poll_until, but at a 0.05 s cadence — for the #799
    cancel/placement-failure checks that must act on the FIRST tick a
    designation shows paid=true, before further build progress lands."""
    deadline = time.time() + seconds
    while time.time() < deadline:
        v = fn()
        if v:
            return v
        time.sleep(0.05)
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

    # --- the ACTUAL UI path (build_tool.lua), not just the lower-level
    # construction.designate call above, must not claim "accepted" for a
    # fully-occupied commit either (review round 1).
    send(port, "camera.setPosition(0, 0); return 'ok'")
    send(port, "local bt = require('scripts.build_tool'); "
               f"bt.hud = {{ worldId = '{w}' }}; return 'ok'")
    px, py = 960, 540
    ux, uy = pick_tile(port, px, py)
    send(port, f"require('scripts.structures').floor({ux}, {uy}); return 'ok'")
    uiBuilt = poll_until(port, 10, lambda: send(
        port, f"return structure.hasAt({ux}, {uy}, 'floor')") == "true")
    check("UI-path fixture floor placed", uiBuilt is not None)
    send(port, "local bt = require('scripts.build_tool'); "
               "bt.enterPlacement({kind='structure', pack='dungeon_1', "
               "piece='floor', edge=nil, displayName='Floor'}); return 'ok'")
    send(port, "return debug.drainActionOutcomes()")  # clear the slate
    send(port, f"local bt = require('scripts.build_tool'); "
               f"return bt.handleMouseDown(1, {px}, {py})")   # anchor
    send(port, f"local bt = require('scripts.build_tool'); "
               f"return bt.handleMouseDown(1, {px}, {py})")   # commit (same tile)
    time.sleep(0.5)
    check("UI commit over an occupied floor creates no job",
          designation_at(port, ux, uy) is None)
    uiOutcomes = send_json(port, "return debug.drainActionOutcomes()")
    if not isinstance(uiOutcomes, list):
        uiOutcomes = []
    uiMatches = [o for o in uiOutcomes
                 if isinstance(o, dict) and o.get("kind") == "buildTool.commitPlacement"]
    check("UI outcome for the occupied commit is not 'accepted'",
          bool(uiMatches) and all(o.get("outcome") != "accepted" for o in uiMatches))
    send(port, "local bt = require('scripts.build_tool'); "
               "bt.exitPlacement(); return 'ok'")

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
    send(port, "return debug.drainActionOutcomes()")  # clear the slate
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
    raceOutcomes = send_json(port, "return debug.drainActionOutcomes()")
    if not isinstance(raceOutcomes, list):
        raceOutcomes = []
    raceMatches = [o for o in raceOutcomes
                   if isinstance(o, dict) and o.get("kind") == "construction.designate"
                   and (o.get("where") or {}).get("x") == 8
                   and (o.get("where") or {}).get("y") == 36]
    check("mid-job cancellation reports an observable non-accepted outcome",
          bool(raceMatches) and all(o.get("outcome") != "accepted" for o in raceMatches))
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


def phase_paid_death(port: int) -> None:
    """#799: a claimant that dies AFTER paying leaves the durable 'paid'
    marker on the designation — a materials-less replacement must still
    finish the job, proving it's never charged the cost a second time.

    (phase_release above already covers a claimant dying BEFORE payment;
    once #799's fix makes a paid job claimable by a materials-less
    worker too, a normal nearby scout would sweep AND re-claim the job
    in close succession, too transient to poll for reliably — so the
    release is instead observed via a scout placed OUTSIDE
    construct_scan_range (30 tiles) but still INSIDE construct_scan_chunks
    (getPendingJobs' wider chunk-radius query, ~2 chunks/32 tiles): its
    scan triggers the sweep (releasing the stale claim) but it can never
    be picked as the claim's own 'best' candidate, since that requires
    distance <= construct_scan_range. This holds 'pending' stationary
    long enough to observe before a real, nearby replacement claims it.)"""
    print("\n[phase 6] paid-then-dead claimant: no second payment (floor)")
    w = wid(port)
    send(port, f"construction.designate('{w}', 8, -8, 8, -8, "
               "'structure', 'dungeon_1', 'floor'); return 'ok'")
    time.sleep(0.5)
    check("tile designated", designation_at(port, 8, -8) is not None)

    a = spawn_acolyte(port, 5.5, -7.5)
    send(port, f"unit.addItem({a}, 'steel_plate', 0); return 'ok'")
    paid = poll_until(port, 90, lambda: (
        (designation_at(port, 8, -8) or {}).get("paid") is True
        and (designation_at(port, 8, -8) or {}).get("progress", 0) > 0))
    check("materials paid and progress accrued before death", paid is not None)
    check("payer's inventory shows the material spent",
          send(port, f"local n=0; for _,it in ipairs(unit.getInventory({a}) "
                     "or {}) do if it.defName=='steel_plate' then "
                     "n=n+1 end end; return n") == "0")

    destroy_unit(port, a)

    # Far scout: 32 tiles out (> construct_scan_range=30, so it can never
    # claim) but still inside construct_scan_chunks' getPendingJobs query,
    # so its scan sweeps the dead claim back to "pending" without racing
    # to re-claim it itself.
    farScout = spawn_acolyte(port, 40.5, -7.5)
    released = poll_until(port, 60, lambda: (
        (designation_at(port, 8, -8) or {}).get("status") == "pending"))
    check("claim released back to 'pending' after claimant death",
          released is not None)
    check("payment marker survives the claimant's death",
          (designation_at(port, 8, -8) or {}).get("paid") is True)
    destroy_unit(port, farScout)

    # Replacement carries ZERO materials, with no ground/mule stock
    # anywhere nearby — it must still claim and finish the job.
    b = spawn_acolyte(port, 12.5, -7.5)
    done = poll_until(port, 120, lambda: (
        send(port, "return structure.hasAt(8, -8, 'floor')") == "true"
        and designation_at(port, 8, -8) is None))
    check("materials-less replacement finished the already-paid job "
          "after the original claimant died", done is not None)
    destroy_unit(port, b)


def phase_preemption(port: int) -> None:
    """#799: an ordinary mid-build interruption (constructOnExit resets
    phase back to 'walking' for thirst / combat / a player order) must
    not re-attempt payment. The claimant carries EXACTLY the required
    material with no ground/mule fallback, so a duplicate charge attempt
    would stall the job outright rather than silently succeed."""
    print("\n[phase 7] preempted mid-build: no duplicate consumption (floor)")
    w = wid(port)
    send(port, f"construction.designate('{w}', 8, -16, 8, -16, "
               "'structure', 'dungeon_1', 'floor'); return 'ok'")
    time.sleep(0.5)
    check("tile designated", designation_at(port, 8, -16) is not None)

    uid = spawn_acolyte(port, 5.5, -15.5)
    send(port, f"unit.addItem({uid}, 'steel_plate', 0); return 'ok'")
    building = poll_until(port, 90, lambda: (
        (designation_at(port, 8, -16) or {}).get("progress", 0) > 0))
    check("payment made and progress underway", building is not None)
    check("exactly one payment taken (inventory now empty)",
          send(port, f"local n=0; for _,it in ipairs(unit.getInventory({uid}) "
                     "or {}) do if it.defName=='steel_plate' then "
                     "n=n+1 end end; return n") == "0")

    # Simulate the exact reset constructOnExit performs on preemption,
    # without needing to engineer a real interrupt.
    send(port, f"local s = require('scripts.unit_ai').getState({uid}); "
               f"s.constructJob.phase = 'walking'; return s.constructJob.phase")

    done = poll_until(port, 120, lambda: (
        send(port, "return structure.hasAt(8, -16, 'floor')") == "true"
        and designation_at(port, 8, -16) is None))
    check("job completed after preemption with no re-payment attempt "
          "(a duplicate charge would stall: no spare material, no "
          "ground/mule stock)", done is not None)
    destroy_unit(port, uid)


def phase_save_load(port: int) -> None:
    """#799: the durable payment marker (and progress) must survive a
    save/load round-trip, even when the paying unit does not."""
    print("\n[phase 8] paid designation survives save/load (floor)")
    w = wid(port)
    send(port, f"construction.designate('{w}', 8, -24, 8, -24, "
               "'structure', 'dungeon_1', 'floor'); return 'ok'")
    time.sleep(0.5)
    check("tile designated", designation_at(port, 8, -24) is not None)

    a = spawn_acolyte(port, 5.5, -23.5)
    send(port, f"unit.addItem({a}, 'steel_plate', 0); return 'ok'")
    paid = poll_until(port, 90, lambda: (
        (designation_at(port, 8, -24) or {}).get("paid") is True
        and (designation_at(port, 8, -24) or {}).get("progress", 0) > 0))
    check("materials paid and progress accrued before save", paid is not None)
    destroy_unit(port, a)   # the paying unit does NOT survive to save

    send(port, f"engine.saveWorld('{w}', 'construct_payment_check'); "
               "return 'ok'")
    time.sleep(5.0)
    send(port, "engine.loadSave('construct_payment_check'); return 'ok'")
    time.sleep(25.0)
    send(port, "world.show('main_world'); return 'ok'")
    send(port, "engine.setPaused(false); return 'ok'")
    send(port, "return world.loadChunksInRegion(-2, -2, 2, 2)", timeout=45)
    send(port, "return world.waitForChunks(90)", timeout=95)

    # The chunk/designation refs can take a beat to settle right after a
    # load on a loaded/slow machine — poll rather than a single read.
    def paid_reload():
        d = designation_at(port, 8, -24)
        return d if isinstance(d, dict) and d.get("paid") is True else None
    reloaded = poll_until(port, 60, paid_reload)
    check("designation survives load with paid=true and progress intact",
          isinstance(reloaded, dict) and reloaded.get("paid") is True
          and reloaded.get("progress", 0) > 0)

    b = spawn_acolyte(port, 12.5, -23.5)   # zero materials
    done = poll_until(port, 120, lambda: (
        send(port, "return structure.hasAt(8, -24, 'floor')") == "true"
        and designation_at(port, 8, -24) is None))
    check("materials-less unit finished the reloaded paid job", done is not None)
    destroy_unit(port, b)


def phase_cancel_refund(port: int) -> None:
    """#799 no-silent-loss policy: the build-tool's right-click cancel
    refunds an already-PAID structure's materials to the ground and
    never fabricates materials for an unpaid one; a placement failure
    (a post's floor pulled out from under it mid-job) refunds too,
    alongside a player-observable notification, instead of completing
    with the paid materials simply gone."""
    print("\n[phase 9] cancellation / placement-failure material policy")
    w = wid(port)
    send(port, "camera.setPosition(0, 0); return 'ok'")
    send(port, "local bt = require('scripts.build_tool'); "
               f"bt.hud = {{ worldId = '{w}' }}; return 'ok'")

    def ui_right_click(px: int, py: int) -> None:
        send(port, "local bt = require('scripts.build_tool'); "
                   "bt.enterPlacement({kind='structure', pack='dungeon_1', "
                   "piece='floor', edge=nil, displayName='Floor'}); "
                   "return bt.state.mode")
        send(port, f"local bt = require('scripts.build_tool'); "
                   f"return bt.handleMouseDown(2, {px}, {py})")

    # --- (a) an UNPAID designation cancelled via the UI creates no
    # ground item — the refund gate must not fire for free.
    px1, py1 = 200, 500
    ux1, uy1 = pick_tile(port, px1, py1)
    send(port, f"construction.designate('{w}', {ux1}, {uy1}, {ux1}, {uy1}, "
               "'structure', 'dungeon_1', 'floor'); return 'ok'")
    time.sleep(0.5)
    check("unpaid fixture tile designated", designation_at(port, ux1, uy1) is not None)
    before1 = ground_count(port, "steel_plate")
    ui_right_click(px1, py1)
    time.sleep(0.5)
    check("UI cancel of an unpaid designation creates no ground item",
          designation_at(port, ux1, uy1) is None
          and ground_count(port, "steel_plate") == before1)

    # --- (b) a PAID designation cancelled via the UI refunds its
    # material to the ground instead of deleting it. Acts on the FIRST
    # tick 'paid' is observed, before further progress could complete
    # the job out from under the cancel.
    px2, py2 = 500, 800
    ux2, uy2 = pick_tile(port, px2, py2)
    send(port, f"construction.designate('{w}', {ux2}, {uy2}, {ux2}, {uy2}, "
               "'structure', 'dungeon_1', 'floor'); return 'ok'")
    time.sleep(0.5)
    # NB: construct job pacing runs off engine.gameTime() (paused/real-
    # time, World.Engine.Core.Init's gameTimeRef), NOT world.setTimeScale
    # (a PER-PAGE calendar rate flora/power read) — so there's no lever
    # to slow this down; poll_fast's tight 0.05s cadence is what gives
    # the check a real shot at catching 'paid' before the job completes.
    payer = spawn_acolyte(port, ux2 - 3 + 0.5, uy2 + 0.5)
    send(port, f"unit.addItem({payer}, 'steel_plate', 0); return 'ok'")
    paid2 = poll_fast(90, lambda: (
        (designation_at(port, ux2, uy2) or {}).get("paid") is True))
    check("second fixture tile paid before cancel", paid2 is not None)
    before2 = ground_count(port, "steel_plate")
    ui_right_click(px2, py2)
    time.sleep(0.5)
    check("UI cancel of a PAID designation refunds the material to the "
          "ground (not placed, not deleted)",
          designation_at(port, ux2, uy2) is None
          and send(port, f"return structure.hasAt({ux2}, {uy2}, 'floor')") == "false"
          and ground_count(port, "steel_plate") == before2 + 1)
    destroy_unit(port, payer)

    # --- (c) placement failure: a post's floor vanishes mid-job. The
    # already-paid wood_log is refunded, the post never lands, and the
    # existing "site changed" warning is player-observable in the event
    # log (#799 adds the refund; the warning itself predates it).
    px3, py3 = 1400, 500
    ux3, uy3 = pick_tile(port, px3, py3)
    send(port, f"require('scripts.structures').floor({ux3}, {uy3}); return 'ok'")
    built = poll_until(port, 20, lambda: send(
        port, f"return structure.hasAt({ux3}, {uy3}, 'floor')") == "true")
    check("placement-failure fixture floor placed", built is not None)
    send(port, f"construction.designate('{w}', {ux3}, {uy3}, {ux3}, {uy3}, "
               "'structure', 'dungeon_1', 'post'); return 'ok'")
    time.sleep(0.5)
    check("post job designated (floor present)",
          designation_at(port, ux3, uy3) is not None)

    # A post's build_work (2.0) is lower than a floor's (3.0), so it has
    # LESS real-time margin between "paid" and "placed" than sub-case (b)
    # above; poll_fast's tight cadence is the only lever here (see the
    # engine.gameTime()/setTimeScale note in sub-case (b)).
    poster = spawn_acolyte(port, ux3 - 3 + 0.5, uy3 + 0.5)
    send(port, f"unit.addItem({poster}, 'wood_log', 0); return 'ok'")
    paid3 = poll_fast(90, lambda: (
        (designation_at(port, ux3, uy3) or {}).get("paid") is True))
    check("post job paid before its floor is pulled", paid3 is not None)

    beforeLogs = ground_count(port, "wood_log")
    send(port, f"structure.clear({ux3}, {uy3}, 'floor'); return 'ok'")
    done3 = poll_until(port, 90, lambda: designation_at(port, ux3, uy3) is None)
    check("job resolved (removed) after the placement failure", done3 is not None)
    check("post was never actually placed",
          send(port, f"return structure.hasAt({ux3}, {uy3}, 'post_n')") == "false")
    check("its material was refunded to the ground, not lost",
          ground_count(port, "wood_log") == beforeLogs + 1)
    logs = send_json(port, "return engine.getEventLog()")
    notified = any(isinstance(e, dict)
                   and "returned to the ground" in (e.get("text") or "")
                   for e in (logs or []))
    check("a player-observable failure notification was logged", notified)
    destroy_unit(port, poster)

    # --- (d) a claimant mid-build on a PAID job must not still place the
    # structure after the designation is cancelled out from under it
    # (#799 review round 5): abandonClaim interrupts the claimant's own
    # cached job immediately rather than waiting for its next decision
    # tick to notice the designation is gone.
    px4, py4 = 700, 180
    ux4, uy4 = pick_tile(port, px4, py4)
    send(port, f"construction.designate('{w}', {ux4}, {uy4}, {ux4}, {uy4}, "
               "'structure', 'dungeon_1', 'floor'); return 'ok'")
    time.sleep(0.5)
    builder = spawn_acolyte(port, ux4 - 3 + 0.5, uy4 + 0.5)
    send(port, f"unit.addItem({builder}, 'steel_plate', 0); return 'ok'")

    def mid_build():
        d = designation_at(port, ux4, uy4)
        if not isinstance(d, dict):
            return None
        p = d.get("progress", 0)
        return d if d.get("paid") is True and 0 < p < 0.9 else None
    caught = poll_fast(90, mid_build)
    check("caught the job mid-build (paid, partial progress)", caught is not None)
    beforePlate = ground_count(port, "steel_plate")
    ui_right_click(px4, py4)
    time.sleep(2.0)   # comfortably longer than the job would take to finish
    check("mid-build cancel refunds the material",
          ground_count(port, "steel_plate") == beforePlate + 1)
    check("structure never placed after a mid-build cancel",
          send(port, f"return structure.hasAt({ux4}, {uy4}, 'floor')") == "false")
    destroy_unit(port, builder)

    # --- (e) non-post placement failure (#799 review round 5 nit):
    # structures.floor/ceiling/wall and wire.place all now propagate
    # structure.place's own success/failure — previously only post's
    # floorZAt-gated failure was ever detected. Tiles far outside the
    # flat arena's 5x5 chunk footprint are never loaded, so placement
    # there deterministically fails without needing a real mid-job race.
    check("structures.floor returns false for an unloaded-chunk target",
          send(port, "return require('scripts.structures').floor(500, 500)") == "false")
    check("structures.ceiling returns false for an unloaded-chunk target",
          send(port, "return require('scripts.structures').ceiling(501, 500)") == "false")
    check("structures.wall returns false for an unloaded-chunk target",
          send(port, "return require('scripts.structures').wall(502, 500, 'ne')") == "false")
    check("wire.place returns false for an unloaded-chunk target",
          send(port, "return require('scripts.wire').place(503, 500)") == "false")

    send(port, "local bt = require('scripts.build_tool'); "
               "bt.exitPlacement(); return 'ok'")


# Order matters: the stake phase runs LAST — the staked portal spawns
# its starting roster (building_spawn.lua auto-loads at boot), and six
# fresh acolytes + a stocked technomule would contaminate any phase
# that runs after it (they claim jobs and source from the mule).
PHASES = {
    "inventory": phase_inventory,
    "ground": phase_ground,
    "release": phase_release,
    "paid_death": phase_paid_death,
    "preemption": phase_preemption,
    "save_load": phase_save_load,
    "cancel": phase_cancel_refund,
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
