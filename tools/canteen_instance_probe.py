#!/usr/bin/env python3
"""Canteen instance-targeting probe (#1220).

The two water AI actions each SELECT a specific canteen instance, compute
their effect from that instance's fill, and used to MUTATE by definition
name — and ``unit.modifyItemFill`` adjusts the FIRST inventory item
matching the name, clamped to ``[0, capacity]``. With two same-def
canteens in a unit's inventory that silently hit the wrong one:

  * ``drinkExecute`` (scripts/unit_ai_needs.lua) sips from the first
    canteen WITH WATER, so an earlier empty canteen swallowed the drain
    as a clamp-to-zero no-op while hydration was still credited — water
    conjured, sipped canteen still full.
  * ``refillExecute`` (scripts/unit_ai_water.lua) measures headroom on
    the first canteen WITH HEADROOM, so an earlier FULL canteen absorbed
    the write as a clamped no-op and the empty one never filled — while
    the pickup anim played and the AI moved on believing it refilled.

The two fixtures are deliberately opposite orderings, because each bug
needs its own: empty-before-full reproduces the drink bug (and would let
first-match refill succeed), full-before-empty reproduces the refill bug
(and would let first-match drink succeed).

Both phases drive the PRODUCTION execute functions against a real engine
and assert per-INSTANCE fills by ``instanceId``, so a regression to
name-based mutation at either call site fails here:

  1. Drink — inventory [empty A, full B]. ``drinkExecute`` drains B by
     exactly the sip, leaves A at zero, and credits hydration by
     sip * drink_hydration_per_litre.
  2. Refill — inventory [full C, empty D], standing one tile from a lake.
     ``refillExecute`` fills D to capacity by exactly the measured
     headroom and leaves C untouched.

Usage: python3 tools/canteen_instance_probe.py [--port 9220]
"""
import argparse, glob, sys

from probelib import boot, quit_engine, send, send_json, load_ai_stack, \
                     poll_until, spawn_acolyte

SPROOT = "/tmp"
CANTEEN = "canteen_steel_2l"
PAGE = "move_test"          # scripts/movement_arena.lua's page id
# Floating-point tolerance: fills round-trip Float → Lua double → JSON.
EPS = 1e-4


def bootstrap(port):
    """Load the defs + flat arena the loading screen would set up in the GUI."""
    for pattern, fn in [
        ("data/substances/*.yaml", "engine.loadSubstanceYaml"),
        ("data/items/*.yaml",      "engine.loadItemYaml"),
        ("data/equipment/*.yaml",  "engine.loadEquipmentYaml"),
        ("data/materials/*.yaml",  "engine.loadMaterialYaml"),
        ("data/vegetation/*.yaml", "engine.loadVegetationYaml"),
        ("data/units/*.yaml",      "engine.loadUnitYaml"),
    ]:
        for path in sorted(glob.glob(pattern)):
            send(port, f"{fn}('{path}'); return 'ok'")
    send(port,
         "return require('scripts.movement_arena').buildCourse('flat').name")
    if not poll_until(30, lambda: send(
            port, "return world.getActiveWorldId()"
            ).strip().strip('"') not in ("", "null", "nil")):
        sys.exit("arena page never became the active world")
    send(port, "return world.loadChunksInRegion(-2, -2, 2, 2)")
    send(port, "return world.waitForChunks(60)", timeout=65.0)
    load_ai_stack(port)
    # Same trick as cooking_probe/movement_probe: neutralise the unit_ai
    # tick so its own arbitration can't drink/refill behind the test.
    send(port,
         "pcall(function() require('scripts.unit_ai').update = function() end end); "
         "return 'ai-off'")


def canteens(port, uid):
    """This unit's canteen instances, in inventory order."""
    rows = send_json(port,
        f"local out={{}}; for _,it in ipairs(unit.getInventory({uid}) or {{}}) do "
        f"if it.defName=='{CANTEEN}' then out[#out+1]={{id=it.instanceId,"
        f"fill=it.currentFill or -1,cap=it.capacity or -1}} end end; return out")
    return rows if isinstance(rows, list) else []


def fnum(port, lua, timeout=10.0):
    raw = send(port, lua, timeout).strip('"')
    try:
        return float(raw)
    except ValueError:
        sys.exit(f"expected a number from {lua!r}, got {raw!r}")


def close(a, b):
    return abs(a - b) <= EPS


def report(ok, text):
    print(f"  [{'PASS' if ok else 'FAIL'}] {text}")
    return ok


def phase_drink(port, params):
    """Empty canteen BEFORE a full one: the sip must drain the FULL one."""
    print("\n--- 1. drink_from_canteen drains the selected instance ---")
    # clear_water=False: the find_water goal is only ever consulted by
    # the unit_ai tick, which bootstrap() has already neutralised — and
    # with no tick there is no AI state for the clear to write to.
    uid = spawn_acolyte(port, 0, 0, clear_water=False)
    if not poll_until(15, lambda: send(
            port, f"return unit.getInfo({uid}) and 'yes' or 'no'"
            ).strip('"') == "yes"):
        sys.exit("acolyte never appeared")
    # Freeze the sim so unit_resources' hydration drain can't move the
    # numbers between the before/after reads. setFrozen is NOT a hold —
    # engine.setPaused is (see CLAUDE.md / #923).
    send(port, "engine.setPaused(true); return 'ok'")

    # Stage [empty, full]: the acolyte def ships ONE full canteen first,
    # so drain that one and append a full second instance.
    own = canteens(port, uid)
    if len(own) != 1:
        sys.exit(f"expected exactly one starting canteen, got {own}")
    empty_id = own[0]["id"]
    send(port, f"return unit.modifyItemFillById({uid}, {empty_id}, -99)")
    send(port, f"return unit.addItem({uid}, '{CANTEEN}', 2.0)")

    inv = canteens(port, uid)
    ok_order = (len(inv) == 2 and inv[0]["id"] == empty_id
                and close(inv[0]["fill"], 0.0) and inv[1]["fill"] > 0)
    if not report(ok_order, f"staged inventory is [empty, full]: {inv}"):
        return False, uid
    full_id, full_before = inv[1]["id"], inv[1]["fill"]

    # A deficit deep enough that the sip is capped by drink_sip_litres
    # (the interesting case), not by deficit / hydration-per-litre.
    max_hyd = fnum(port,
        f"return require('scripts.unit_stats').get({uid}, 'max_hydration')")
    send(port, f"unit.setStat({uid}, 'hydration', {max_hyd * 0.5}); return 'ok'")
    hyd_before = fnum(port, f"return unit.getStat({uid}, 'hydration')")

    sip = min(params["drink_sip_litres"], full_before,
              (max_hyd - hyd_before) / params["drink_hydration_per_litre"])

    send(port,
         f"require('scripts.unit_ai_needs').drinkExecute({uid}, {{}}, "
         f"require('scripts.unit_ai_tunables').acolyte); return 'ok'")

    after = {c["id"]: c["fill"] for c in canteens(port, uid)}
    hyd_after = fnum(port, f"return unit.getStat({uid}, 'hydration')")

    ok_a = report(close(after.get(empty_id, -1), 0.0),
                  f"the earlier EMPTY canteen (#{empty_id}) is untouched: "
                  f"{after.get(empty_id)} (expected 0.0)")
    ok_b = report(close(after.get(full_id, -1), full_before - sip),
                  f"the SELECTED canteen (#{full_id}) lost exactly the sip: "
                  f"{after.get(full_id)} (expected {full_before - sip})")
    credited = hyd_after - hyd_before
    removed = full_before - after.get(full_id, full_before)
    expected = removed * params["drink_hydration_per_litre"]
    ok_c = report(close(credited, expected),
                  f"hydration credited equals water actually removed: "
                  f"+{credited:.4f} vs {removed:.4f} L * "
                  f"{params['drink_hydration_per_litre']} = {expected:.4f}")
    return ok_a and ok_b and ok_c, uid


def phase_refill(port):
    """Full canteen BEFORE an empty one: the fill must land on the EMPTY one."""
    print("\n--- 2. refill_canteen fills the selected instance ---")
    # A lake tile with a dry tile beside it to stand on.
    wx, wy = 6, 0
    send(port, f"world.setFluidTile('{PAGE}', {wx}, {wy}, 'water'); return 'ok'")
    if not poll_until(20, lambda: send(
            port, f"return tostring(world.getFluidAt({wx},{wy}))"
            ).strip('"') == "lake"):
        sys.exit(f"lake tile at ({wx},{wy}) never materialised")
    dry = send(port, f"return tostring(world.getFluidAt({wx + 1},{wy}))").strip('"')
    if dry not in ("nil", "null"):
        sys.exit(f"the stand-on tile ({wx + 1},{wy}) is fluid ({dry})")

    uid = spawn_acolyte(port, wx + 1, wy, clear_water=False)
    if not poll_until(15, lambda: send(
            port, f"return unit.getInfo({uid}) and 'yes' or 'no'"
            ).strip('"') == "yes"):
        sys.exit("acolyte never appeared")
    send(port, "engine.setPaused(true); return 'ok'")

    # Stage [full, empty]: the def's own canteen ships FULL and first —
    # exactly the ordering first-match refill would clamp away on — so
    # only the appended empty instance needs staging.
    send(port, f"return unit.addItem({uid}, '{CANTEEN}', 0.0)")
    inv = canteens(port, uid)
    ok_order = (len(inv) == 2
                and close(inv[0]["fill"], inv[0]["cap"])
                and close(inv[1]["fill"], 0.0))
    if not report(ok_order, f"staged inventory is [full, empty]: {inv}"):
        return False, uid
    full_id, full_before = inv[0]["id"], inv[0]["fill"]
    empty_id, empty_cap = inv[1]["id"], inv[1]["cap"]
    headroom = empty_cap - inv[1]["fill"]

    # The unit must actually be one tile from the remembered source and
    # off the water — refillExecute's own adjacency gate.
    info = send_json(port, f"return unit.getInfo({uid})")
    utx, uty = int(info["gridX"] // 1), int(info["gridY"] // 1)
    cheb = max(abs(utx - wx), abs(uty - wy))
    if not report(cheb == 1, f"unit stands one tile from the lake: "
                             f"({utx},{uty}) vs ({wx},{wy}), chebyshev {cheb}"):
        return False, uid

    send(port,
         f"local core = require('scripts.unit_ai_core'); local s = {{}}; "
         f"core.addWaterSource(s, {wx}, {wy}); "
         f"require('scripts.unit_ai_water').refillExecute({uid}, s, "
         f"require('scripts.unit_ai_tunables').acolyte); return 'ok'")

    after = {c["id"]: c["fill"] for c in canteens(port, uid)}
    ok_a = report(close(after.get(full_id, -1), full_before),
                  f"the earlier FULL canteen (#{full_id}) is untouched: "
                  f"{after.get(full_id)} (expected {full_before})")
    ok_b = report(close(after.get(empty_id, -1), empty_cap),
                  f"the SELECTED empty canteen (#{empty_id}) gained the "
                  f"measured headroom {headroom}: {after.get(empty_id)} "
                  f"(expected {empty_cap})")
    return ok_a and ok_b, uid


def main():
    ap = argparse.ArgumentParser()
    ap.add_argument("--port", type=int, default=9220)
    args = ap.parse_args()
    port = args.port

    proc = boot(port, f"{SPROOT}/canteen_instance_probe_engine.log")
    passed = True
    try:
        bootstrap(port)
        params = send_json(port,
            "local t = require('scripts.unit_ai_tunables').acolyte; "
            "return { drink_sip_litres = t.drink_sip_litres, "
            "drink_hydration_per_litre = t.drink_hydration_per_litre, "
            "canteen_def = t.canteen_def, drink_canteen_def = t.drink_canteen_def }")
        # Both call sites must be aimed at the def this probe stages, or
        # the fixtures below prove nothing about them.
        if not isinstance(params, dict) \
           or params.get("drink_canteen_def") != CANTEEN \
           or params.get("canteen_def") != CANTEEN:
            sys.exit(f"unexpected acolyte AI tunables: {params}")

        ok1, _ = phase_drink(port, params)
        # Each phase gets its own acolyte: the two fixtures need opposite
        # inventory orderings, and reusing one would leave the first
        # phase's drained canteen in the way.
        send(port, "engine.setPaused(false); return 'ok'")
        ok2, _ = phase_refill(port)
        passed = ok1 and ok2
    finally:
        quit_engine(port, proc)

    print(f"\n{'ALL CHECKS PASSED' if passed else 'SOME CHECKS FAILED'}")
    return 0 if passed else 1


if __name__ == "__main__":
    sys.exit(main())
