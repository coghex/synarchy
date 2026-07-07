#!/usr/bin/env python3
"""Repair probe (#301) — the policy layer on top of unit.repairItem (#300).

Boots a headless engine on a flat arena, loads defs + data/recipes +
data/buildings, spawns an acolyte, degrades a weapon's condition and
sharpness directly via unit.repairItem (the #300 primitive — the only
way to simulate wear headless without a real combat pass), then checks
the repair.* Lua API end-to-end:

  1. Catalogue: repair.getNames lists exactly the two shipped repair
     recipes (repair_condition, repair_sharpness) and NOT the ordinary
     craft recipes; repair.get returns the repairAxis field craft.get
     also exposes but ordinary recipes omit.
  2. craft.execute/executeAt refuse repair-tagged recipes with a clear
     "use repair.repairAt" reason, leaving inventory untouched.
  3. repair.repairAt refuses: unknown recipe id, a non-repair recipe
     id, an unbuilt station, the WRONG station for the axis (a
     condition recipe at the workbench and vice versa — the split
     operations from #301 must route correctly), a non-adjacent unit,
     and an unknown item instance — all leaving inventory + item state
     untouched.
  4. Station-gated success: repair_condition at the BUILT furnace
     consumes 1 lignite_chunk and restores a degraded weapon's
     condition fully to 100 (sharpness untouched); repair_sharpness at
     the BUILT workbench consumes 1 whetstone and restores sharpness
     fully to 100 (condition untouched). Instance id is preserved.
  5. A fully broken (condition 0) item repairs the same way as a
     lightly worn one (full restore, same cost).
  6. An axis already at 100 refuses ("already at full ...") before any
     cost is consumed.
  7. Short cost (no lignite_chunk / no whetstone) refuses with nothing
     consumed and the item unchanged.

Usage: python3 tools/repair_probe.py [--port 9319]
"""
import argparse, glob, json, socket, subprocess, sys, time
from probelib import quit_engine, boot, send

SPROOT = "/tmp"


def jget(port, lua, timeout=10.0):
    raw = send(port, lua, timeout)
    try:
        return json.loads(raw)
    except json.JSONDecodeError:
        return raw.strip('"')


def bootstrap(port):
    """Load defs + the flat arena (the loading screen doesn't run headless)."""
    for pattern, fn in [
        ("data/substances/*.yaml", "engine.loadSubstanceYaml"),
        ("data/items/*.yaml",      "engine.loadItemYaml"),
        ("data/equipment/*.yaml",  "engine.loadEquipmentYaml"),
        ("data/materials/*.yaml",  "engine.loadMaterialYaml"),
        ("data/vegetation/*.yaml", "engine.loadVegetationYaml"),
        ("data/units/*.yaml",      "engine.loadUnitYaml"),
        ("data/recipes/*.yaml",    "engine.loadRecipeYaml"),
        ("data/buildings/*.yaml",  "engine.loadBuildingYaml"),
    ]:
        for path in sorted(glob.glob(pattern)):
            send(port, f"{fn}('{path}'); return 'ok'")
    send(port,
         "return require('scripts.movement_arena').buildCourse('flat').name")
    for _ in range(60):
        raw = send(port, "return world.getActiveWorldId()").strip().strip('"')
        if raw and raw not in ("null", "nil"):
            break
        time.sleep(0.5)
    else:
        sys.exit("arena page never became the active world")
    send(port, "return world.loadChunksInRegion(-1, -1, 2, 2)")
    send(port, "return world.waitForChunks(60)", timeout=65.0)
    # unit_ai is auto-loaded at engine boot and its update() tick wanders
    # every spawned unit headless. The station checks assume the unit
    # STAYS on its spawn tile for footprint adjacency (same trick as
    # craft_probe / movement_probe) — nothing under test needs the AI.
    send(port,
         "pcall(function() require('scripts.unit_ai').update = function() end end); "
         "return 'ai-off'")


def count_item(port, uid, name):
    return int(float(send(port,
        f"local c=0; for _,it in ipairs(unit.getInventory({uid}) or {{}}) do "
        f"if it.defName=='{name}' then c=c+1 end end; return c")))


def spawn_station(port, uid, def_name, gx, gy, materials):
    """Spawn def_name at (gx, gy), deliver build materials from the unit
    through the real machinery, and build it fully. Returns the built
    building id."""
    raw = send(port, f"return building.spawn('{def_name}', {gx}, {gy})")
    try:
        bid = int(float(raw))
    except ValueError:
        sys.exit(f"building.spawn('{def_name}') failed: {raw}")
    for _ in range(50):
        if send(port, f"return building.getInfo({bid}) and 'yes' or 'no'"
                ).strip('"') == "yes":
            break
        time.sleep(0.1)
    else:
        sys.exit(f"{def_name} instance never appeared")
    for item, count in materials.items():
        send(port,
             f"for i=1,{count} do unit.addItem({uid},'{item}'); "
             f"unit.transferItemToBuilding({uid},{bid},'{item}') end; "
             f"return 'ok'")
    sat = send(port, f"return building.areMaterialsSatisfied({bid}) "
                     f"and 'yes' or 'no'").strip('"')
    if sat != "yes":
        sys.exit(f"{def_name} materials not satisfied after delivery")
    send(port, f"building.addBuildProgress({bid}, 100000); return 'ok'")
    act = send(port, f"return building.getActivity({bid})").strip('"')
    if act != "built":
        sys.exit(f"{def_name} never reached built (got {act})")
    return bid


def spawn_weapon(port, uid, cond=100.0, sharp=100.0):
    """Add a fresh axe_steel and force its wear axes to exact values,
    then return its instanceId. axe_steel's condition rolls in [80,100]
    at spawn (idConditionSpec), so setting an exact target needs two
    repairItem deltas: a large negative one to floor both axes at 0
    regardless of the roll, then a precise positive one from that known
    floor (unit.repairItem's delta is ADDITIVE + clamped, not a set)."""
    send(port, f"unit.addItem({uid}, 'axe_steel'); return 'ok'")
    axes = jget(port,
        f"local out={{}}; for _,it in ipairs(unit.getInventory({uid}) or {{}}) do "
        f"if it.defName=='axe_steel' then out[#out+1]=it.instanceId end end; "
        f"return out")
    iid = int(axes[-1])
    send(port, f"unit.repairItem({uid}, {iid}, -1000, -1000); return 'ok'")
    send(port, f"unit.repairItem({uid}, {iid}, {cond}, {sharp}); return 'ok'")
    return iid


def axe_state(port, uid, iid):
    r = jget(port,
        f"for _,it in ipairs(unit.getInventory({uid}) or {{}}) do "
        f"if it.instanceId=={iid} then return {{cond=it.condition,"
        f"sharp=it.sharpness}} end end; return nil")
    return r


def repair_at(port, uid, recipe_id, iid, bid):
    """→ (result: dict|None, err: str)."""
    raw = jget(port,
        f"local r,err = repair.repairAt({uid}, '{recipe_id}', {iid}, {bid}); "
        f"if r then return r end; return 'ERR:'..tostring(err)")
    if isinstance(raw, dict):
        return raw, ""
    return None, str(raw)


def check(passed, ok, label, detail=""):
    print(f"  [{'PASS' if ok else 'FAIL'}] {label}" + (f": {detail}" if detail else ""))
    return passed and ok


def main():
    ap = argparse.ArgumentParser()
    ap.add_argument("--port", type=int, default=9319)
    args = ap.parse_args()
    port = args.port
    passed = True

    proc = boot(port, f"{SPROOT}/repair_probe_engine.log")
    try:
        bootstrap(port)
        uid = int(float(send(port, "return unit.spawn('acolyte', 2, 2)")))
        if uid < 0:
            sys.exit("unit.spawn failed")
        time.sleep(1.0)

        # --- 1. Catalogue ---
        names = jget(port, "return repair.getNames()")
        ok = (isinstance(names, list)
              and set(names) == {"repair_condition", "repair_sharpness"})
        passed = check(passed, ok, "getNames lists only the repair recipes", names)
        r = jget(port, "return repair.get('repair_condition')")
        ok = (isinstance(r, dict) and r.get("station") == "repair_condition"
              and r.get("repairAxis") == "condition"
              and r.get("inputs") == [{"item": "lignite_chunk", "count": 1}])
        passed = check(passed, ok, "repair.get('repair_condition') shape", r)
        r = jget(port, "return repair.get('repair_sharpness')")
        ok = (isinstance(r, dict) and r.get("station") == "repair_sharpness"
              and r.get("repairAxis") == "sharpness"
              and r.get("inputs") == [{"item": "whetstone", "count": 1}])
        passed = check(passed, ok, "repair.get('repair_sharpness') shape", r)
        ok = send(port, "return repair.get('forge_steel_dagger') and 'yes' or 'nil'"
                  ).strip('"') == "nil"
        passed = check(passed, ok, "repair.get refuses an ordinary craft recipe")
        r = jget(port, "return craft.get('repair_condition')")
        ok = isinstance(r, dict) and "repairAxis" in r
        passed = check(passed, ok, "craft.get also exposes repairAxis", r)

        # --- 2. craft.execute/executeAt refuse repair recipes ---
        bars0 = count_item(port, uid, "lignite_chunk")
        ok_e, msg = jget(port,
            f"local ok,err = craft.execute({uid}, 'repair_condition'); "
            f"return {{ok, tostring(err)}}")
        ok = (ok_e is False and "repair recipe" in msg
              and count_item(port, uid, "lignite_chunk") == bars0)
        passed = check(passed, ok, "craft.execute refuses a repair recipe", msg)

        # --- Build both stations, fully. ---
        bid_f = spawn_station(port, uid, "furnace", 3, 2,
                              {"granite_chunk": 6, "steel_bar": 2})
        bid_w = spawn_station(port, uid, "workbench", 1, 2,
                              {"wood_log": 4, "steel_hardware": 4,
                               "steel_bar": 2})

        # --- 3. Refusals ---
        iid = spawn_weapon(port, uid, cond=40.0, sharp=100.0)
        ok_e, msg = repair_at(port, uid, "no_such_recipe", iid, bid_f)
        passed = check(passed, ok_e is None and "unknown recipe" in msg,
                       "unknown recipe refused", msg)
        ok_e, msg = repair_at(port, uid, "forge_steel_dagger", iid, bid_f)
        passed = check(passed, ok_e is None and "not a repair recipe" in msg,
                       "non-repair recipe id refused", msg)
        ok_e, msg = repair_at(port, uid, "repair_condition", iid, 99999)
        passed = check(passed, ok_e is None and "no such building" in msg,
                       "unknown building refused", msg)
        bid_ghost = int(float(send(port,
            "return building.spawn('furnace', 6, 2)")))
        for _ in range(50):        # spawn is queued to the building thread
            if send(port, f"return building.getInfo({bid_ghost}) and 'yes' or 'no'"
                    ).strip('"') == "yes":
                break
            time.sleep(0.1)
        else:
            sys.exit("ghost furnace instance never appeared")
        ok_e, msg = repair_at(port, uid, "repair_condition", iid, bid_ghost)
        passed = check(passed, ok_e is None and "not built" in msg,
                       "unbuilt station refused", msg)
        # Wrong station for the axis: condition recipe at the workbench,
        # sharpness recipe at the furnace — the #301 split must hold.
        ok_e, msg = repair_at(port, uid, "repair_condition", iid, bid_w)
        passed = check(passed, ok_e is None and "does not offer" in msg,
                       "condition recipe refused at the workbench", msg)
        ok_e, msg = repair_at(port, uid, "repair_sharpness", iid, bid_f)
        passed = check(passed, ok_e is None and "does not offer" in msg,
                       "sharpness recipe refused at the furnace", msg)
        # Non-adjacent unit.
        uid2 = int(float(send(port, "return unit.spawn('acolyte', 12, 12)")))
        time.sleep(0.5)
        iid2 = spawn_weapon(port, uid2, cond=40.0, sharp=100.0)
        ok_e, msg = repair_at(port, uid2, "repair_condition", iid2, bid_f)
        passed = check(passed, ok_e is None and "not adjacent" in msg,
                       "far unit refused (not adjacent)", msg)
        # Unknown item instance on the (adjacent) probe unit.
        ok_e, msg = repair_at(port, uid, "repair_condition", 999999, bid_f)
        passed = check(passed, ok_e is None and "no such item instance" in msg,
                       "unknown instance refused", msg)

        # --- 4. Station-gated success: condition at the furnace ---
        send(port, f"unit.addItem({uid}, 'lignite_chunk'); return 'ok'")
        fuel0 = count_item(port, uid, "lignite_chunk")
        before = axe_state(port, uid, iid)
        r, msg = repair_at(port, uid, "repair_condition", iid, bid_f)
        after = axe_state(port, uid, iid)
        ok = (r is not None and r["condition"] == 100 and r["sharpness"] == 100
              and abs(r["conditionApplied"] - 60.0) < 0.01
              and r["sharpnessApplied"] == 0
              and count_item(port, uid, "lignite_chunk") == fuel0 - 1
              and before["cond"] == 40 and after["cond"] == 100
              and after["sharp"] == before["sharp"])
        passed = check(passed, ok,
                       "repair_condition at furnace: fuel consumed, condition->100, sharpness untouched",
                       f"r={r} before={before} after={after}")

        # --- Sharpness at the workbench ---
        iid_s = spawn_weapon(port, uid, cond=100.0, sharp=25.0)
        send(port, f"unit.addItem({uid}, 'whetstone'); return 'ok'")
        stone0 = count_item(port, uid, "whetstone")
        r, msg = repair_at(port, uid, "repair_sharpness", iid_s, bid_w)
        after_s = axe_state(port, uid, iid_s)
        ok = (r is not None and r["sharpness"] == 100 and r["condition"] == 100
              and abs(r["sharpnessApplied"] - 75.0) < 0.01
              and r["conditionApplied"] == 0
              and count_item(port, uid, "whetstone") == stone0 - 1
              and after_s["sharp"] == 100 and after_s["cond"] == 100)
        passed = check(passed, ok,
                       "repair_sharpness at workbench: whetstone consumed, sharpness->100, condition untouched",
                       f"r={r} after={after_s}")

        # --- 5. Fully broken (condition 0) repairs the same way ---
        iid_broken = spawn_weapon(port, uid, cond=0.0, sharp=100.0)
        send(port, f"unit.addItem({uid}, 'lignite_chunk'); return 'ok'")
        r, msg = repair_at(port, uid, "repair_condition", iid_broken, bid_f)
        ok = (r is not None and r["condition"] == 100
              and abs(r["conditionApplied"] - 100.0) < 0.01)
        passed = check(passed, ok, "broken (condition 0) item repairs fully", f"r={r}")

        # --- 6. Already-full axis refuses before consuming anything ---
        fuel_before = count_item(port, uid, "lignite_chunk")
        ok_e, msg = repair_at(port, uid, "repair_condition", iid_broken, bid_f)
        ok = (ok_e is None and "already at full" in msg
              and count_item(port, uid, "lignite_chunk") == fuel_before)
        passed = check(passed, ok, "already-full axis refused, nothing consumed", msg)

        # --- 7. Short cost refuses, nothing consumed ---
        iid_poor = spawn_weapon(port, uid, cond=10.0, sharp=100.0)
        while count_item(port, uid, "lignite_chunk") > 0:
            send(port, f"unit.removeItem({uid}, 'lignite_chunk'); return 'ok'")
        ok_e, msg = repair_at(port, uid, "repair_condition", iid_poor, bid_f)
        state = axe_state(port, uid, iid_poor)
        ok = (ok_e is None and "missing" in msg and "lignite_chunk" in msg
              and state["cond"] == 10)
        passed = check(passed, ok, "short fuel refused, item untouched", msg)

        print("\n" + ("ALL REPAIR CHECKS PASSED" if passed else "SOME FAILED"))
        return 0 if passed else 1
    finally:
        quit_engine(port, proc)


if __name__ == "__main__":
    sys.exit(main())
