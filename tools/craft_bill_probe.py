#!/usr/bin/env python3
"""Craft bill + craft AI probe (#329) — the bill backend and the
craft_job worker loop, end to end.

Boots a headless engine on a flat arena, loads defs + recipes +
buildings, builds a furnace through the real delivery + build-progress
machinery, then checks:

  1. Bill backend (craft.* verbs, AI neutralised):
     - addBill validation: unknown recipe, repair-tagged recipe, a
       station that doesn't offer the recipe's operation, unknown
       building — all refused with a reason; a valid bill returns an
       id and the expected shape (pending, no claimant, progress 0).
     - getBills lists it (globally and per station).
     - Claims: claim wins, a rival's claim against a fresh holder is
       refused, the holder can refresh, an expired claim is taken
       over, release hands the bill back with progress kept.
     - Progress clamps to [0,1]; completeBillCycle counts down and
       removes the bill at 0; cancelBill removes outright.
  2. Craft AI (unit_ai craft_job, AI live): with steel bars lying near
     the built furnace and a 2-count bill queued, an acolyte claims
     the bill (observable via getBill().claimant), sources the bars
     from the ground, works the station, and produces — the FRESH
     output instances (ids returned by craft.executeAt) are laid down
     at the station as ground items while a same-def item the crafter
     already carried stays carried, the bill is removed when its count
     runs out, and the crafter earns trade-skill XP ("smithing" for
     untagged recipes).
  3. Knowledge gate: a knowledge-gated bill draws no worker while the
     acolyte doesn't know the theory, and is worked once
     unit.setKnowledge grants it.
  4. Cargo rung: with the only steel bar deposited in a built cargo
     hold (no loose/mule stock), the crafter withdraws it from storage
     (the last rung of the sourcing ladder) and works the bill to
     completion, emptying the store.

Usage: python3 tools/craft_bill_probe.py [--port 9319]
"""
import argparse, glob, json, socket, subprocess, sys, time
from probelib import clear_find_water, quit_engine, boot, send

SPROOT = "/tmp"
TEST_YAML = f"{SPROOT}/craft_bill_probe_recipes.yaml"

# Probe recipes: tiny work values so a cycle completes in seconds
# (shipped smelts are work 30 ≈ a minute at skill 0). Inputs/outputs
# use shipped item defs.
TEST_RECIPES = """\
recipes:
  - id: bill_probe_smelt
    station: smelt
    inputs:
      - item: steel_bar
    work: 2
    outputs:
      - item: granite_chunk
        count: 2
  - id: bill_probe_gated
    station: smelt
    knowledge: metallurgy
    inputs:
      - item: steel_bar
    work: 1
    outputs:
      - item: granite_chunk
"""


def jget(port, lua, timeout=10.0):
    raw = send(port, lua, timeout)
    try:
        return json.loads(raw)
    except json.JSONDecodeError:
        return raw.strip('"')


def bootstrap(port):
    """Load defs + the flat arena (the loading screen doesn't run
    headless). unit_ai stays live — it's half of what's under test —
    but is toggled off around the backend phase (see ai_off/ai_on)."""
    for pattern, fn in [
        ("data/substances/*.yaml", "engine.loadSubstanceYaml"),
        ("data/items/*.yaml",      "engine.loadItemYaml"),
        ("data/equipment/*.yaml",  "engine.loadEquipmentYaml"),
        ("data/materials/*.yaml",  "engine.loadMaterialYaml"),
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


# The stash lives on the module table (NOT _G): debug-console lines
# don't share a global env, but the loaded module table persists.
def ai_off(port):
    send(port,
         "local ai = require('scripts.unit_ai'); "
         "if not ai.__probe_orig_update then "
         "ai.__probe_orig_update = ai.update end; "
         "ai.update = function() end; return 'ai-off'")


def ai_on(port):
    send(port,
         "local ai = require('scripts.unit_ai'); "
         "if ai.__probe_orig_update then "
         "ai.update = ai.__probe_orig_update end; "
         "return 'ai-on'")


def spawn_acolyte(port, x, y):
    uid = int(float(send(port, f"return unit.spawn('acolyte', {x}, {y})")))
    if uid < 0:
        sys.exit("unit.spawn failed")
    time.sleep(0.5)
    # Retire the spawn-seeded find_water goal: the arena has no water,
    # and a scouting acolyte walks off-course instead of crafting.
    clear_find_water(port, uid)
    return uid


def spawn_station(port, uid, def_name, gx, gy, materials, progress=500):
    """building.spawn + deliver build materials through the real
    machinery, then addBuildProgress to Built."""
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
    if send(port, f"return building.areMaterialsSatisfied({bid}) "
                  f"and 'yes' or 'no'").strip('"') != "yes":
        sys.exit(f"{def_name} materials not satisfied after delivery")
    send(port, f"building.addBuildProgress({bid}, {progress}); return 'ok'")
    act = send(port, f"return building.getActivity({bid})").strip('"')
    if act != "built":
        sys.exit(f"{def_name} never reached built (activity={act})")
    return bid


def inv_instance_ids(port, uid, name):
    """Instance ids of all top-level inventory items with defName."""
    raw = send(port,
        f"local out={{}}; for _,it in ipairs(unit.getInventory({uid}) "
        f"or {{}}) do if it.defName=='{name}' then "
        f"out[#out+1]=it.instanceId end end; return out")
    try:
        ids = json.loads(raw)
        return ids if isinstance(ids, list) else []
    except json.JSONDecodeError:
        return []


def add_bill(port, bid, recipe, count=None):
    """→ (billId or None, err)."""
    arg = f", {count}" if count is not None else ""
    raw = send(port,
               f"local id,err = craft.addBill({bid}, '{recipe}'{arg}); "
               f"return id and ('ID:'..id) or ('ERR:'..tostring(err))"
               ).strip('"')
    if raw.startswith("ID:"):
        return int(float(raw[3:])), ""
    return None, raw


def ground_count_near(port, name, gx, gy, radius):
    return int(float(send(port,
        f"local n=0; for _,g in ipairs(item.listGround() or {{}}) do "
        f"if g.defName=='{name}' and math.abs(g.x-{gx})<={radius} "
        f"and math.abs(g.y-{gy})<={radius} then n=n+1 end end; return n")))


def check(passed, ok, label, detail=""):
    print(f"  [{'PASS' if ok else 'FAIL'}] {label}"
          + (f": {detail}" if detail else ""))
    return passed and ok


def poll(port, seconds, fn, interval=1.0):
    """Poll fn until true, defensively unpausing each pass: notification
    categories can be user-configured to auto-pause (config/
    notifications.yaml), and e.g. a stuck-walk unit_warning mid-fetch
    would otherwise freeze the whole sim under the probe."""
    deadline = time.time() + seconds
    while time.time() < deadline:
        send(port, "engine.setPaused(false); return 'ok'")
        if fn():
            return True
        time.sleep(interval)
    return False


def main():
    ap = argparse.ArgumentParser()
    ap.add_argument("--port", type=int, default=9319)
    args = ap.parse_args()
    port = args.port
    passed = True

    proc = boot(port, f"{SPROOT}/craft_bill_probe_engine.log")
    try:
        bootstrap(port)
        with open(TEST_YAML, "w") as f:
            f.write(TEST_RECIPES)
        n = int(float(send(port, f"return engine.loadRecipeYaml('{TEST_YAML}')")))
        passed = check(passed, n == 2, "probe recipes loaded", f"count={n}")

        # Backend phase runs with the AI off so no acolyte races the
        # scripted claim/progress calls.
        ai_off(port)
        uid = spawn_acolyte(port, 2, 2)
        bid_f = spawn_station(port, uid, "furnace", 6, 2,
                              {"granite_chunk": 6, "steel_bar": 2})

        # --- 1. Bill backend ---
        b, msg = add_bill(port, bid_f, "no_such_recipe")
        passed = check(passed, b is None and "unknown recipe" in msg,
                       "addBill(unknown recipe) refused", msg)
        b, msg = add_bill(port, bid_f, "repair_condition")
        passed = check(passed, b is None and "repair recipe" in msg,
                       "addBill(repair recipe) refused", msg)
        b, msg = add_bill(port, bid_f, "forge_steel_dagger")
        passed = check(passed, b is None and "does not offer" in msg,
                       "addBill(forge recipe at furnace) refused", msg)
        b, msg = add_bill(port, 99999, "bill_probe_smelt")
        passed = check(passed, b is None and "no such building" in msg,
                       "addBill(unknown building) refused", msg)

        bill1, msg = add_bill(port, bid_f, "bill_probe_smelt", 3)
        passed = check(passed, bill1 is not None and bill1 >= 1,
                       "addBill ok returns an id", msg or f"id={bill1}")
        shape = jget(port, f"return craft.getBill({bill1})")
        ok = (isinstance(shape, dict) and shape.get("station") == bid_f
              and shape.get("recipe") == "bill_probe_smelt"
              and shape.get("remaining") == 3
              and shape.get("progress") == 0
              and "claimant" not in shape)
        passed = check(passed, ok, "getBill shape (pending, no claimant)",
                       shape)
        allb = jget(port, "return craft.getBills()")
        perb = jget(port, f"return craft.getBills({bid_f})")
        ok = (isinstance(allb, list) and isinstance(perb, list)
              and any(x.get("id") == bill1 for x in allb)
              and any(x.get("id") == bill1 for x in perb))
        passed = check(passed, ok, "getBills lists it (global + station)")

        uid2 = spawn_acolyte(port, 12, 12)
        ok = send(port, f"return craft.claimBill({bill1}, {uid}, 30) "
                        f"and 'y' or 'n'").strip('"') == "y"
        passed = check(passed, ok, "claimBill wins on a pending bill")
        ok = send(port, f"return craft.claimBill({bill1}, {uid2}, 30) "
                        f"and 'y' or 'n'").strip('"') == "n"
        passed = check(passed, ok, "rival claim against a fresh holder refused")
        claimant = jget(port, f"return craft.getBill({bill1})").get("claimant")
        passed = check(passed, claimant == uid, "claimant observable via getBill",
                       f"claimant={claimant}")
        ok = send(port, f"return craft.claimBill({bill1}, {uid}, 30) "
                        f"and 'y' or 'n'").strip('"') == "y"
        passed = check(passed, ok, "holder refresh succeeds")
        time.sleep(0.4)
        ok = send(port, f"return craft.claimBill({bill1}, {uid2}, 0.1) "
                        f"and 'y' or 'n'").strip('"') == "y"
        passed = check(passed, ok, "expired claim taken over (timeout 0.1s)")

        prog = jget(port, f"return craft.addBillProgress({bill1}, 0.6)")
        passed = check(passed, abs(float(prog) - 0.6) < 1e-6,
                       "addBillProgress 0.6", prog)
        prog = jget(port, f"return craft.addBillProgress({bill1}, 0.6)")
        passed = check(passed, abs(float(prog) - 1.0) < 1e-6,
                       "progress clamps at 1.0", prog)
        ok = send(port, f"return craft.releaseBill({bill1}) and 'y' or 'n'"
                  ).strip('"') == "y"
        after = jget(port, f"return craft.getBill({bill1})")
        ok = (ok and isinstance(after, dict) and "claimant" not in after
              and abs(after.get("progress", 0) - 1.0) < 1e-6)
        passed = check(passed, ok, "release keeps progress, clears claimant",
                       after)

        rem = jget(port, f"return craft.completeBillCycle({bill1})")
        passed = check(passed, rem == 2, "completeBillCycle → remaining 2", rem)
        after = jget(port, f"return craft.getBill({bill1})")
        ok = (isinstance(after, dict) and after.get("remaining") == 2
              and after.get("progress") == 0)
        passed = check(passed, ok, "cycle reset progress + decremented", after)
        jget(port, f"return craft.completeBillCycle({bill1})")
        rem = jget(port, f"return craft.completeBillCycle({bill1})")
        gone = send(port, f"return craft.getBill({bill1}) and 'y' or 'n'"
                    ).strip('"')
        passed = check(passed, rem == 0 and gone == "n",
                       "count exhausted removes the bill",
                       f"rem={rem} exists={gone}")

        bill2, _ = add_bill(port, bid_f, "bill_probe_smelt")   # repeat mode
        shape = jget(port, f"return craft.getBill({bill2})")
        passed = check(passed,
                       isinstance(shape, dict) and shape.get("remaining") == -1,
                       "count omitted → repeat forever (-1)", shape)
        ok = send(port, f"return craft.cancelBill({bill2}) and 'y' or 'n'"
                  ).strip('"') == "y"
        gone = send(port, f"return craft.getBill({bill2}) and 'y' or 'n'"
                    ).strip('"')
        passed = check(passed, ok and gone == "n", "cancelBill removes")

        # uid2 was only a rival claimant; remove it so the AI phase has
        # exactly one worker.
        send(port, f"unit.destroy({uid2}); return 'ok'")

        # --- 2. Craft AI end to end ---
        # 3 bars on the ground near the furnace: 2 cycles × 1 input,
        # plus one spare for the knowledge-gate phase below.
        for i in range(3):
            send(port, f"item.spawnGround('steel_bar', {7.5 + 0.3*i}, 2.5); "
                       f"return 'ok'")
        # Output-identity fixture: the crafter carries a granite_chunk
        # of its own (same def as the bill's OUTPUT). The deposit must
        # drop the freshly crafted instances (dropItemById on the ids
        # executeAt returns), never this carried one.
        send(port, f"unit.addItem({uid}, 'granite_chunk'); return 'ok'")
        keep_ids = inv_instance_ids(port, uid, "granite_chunk")
        passed = check(passed, len(keep_ids) == 1,
                       "identity fixture: crafter carries one granite chunk",
                       keep_ids)
        xp0 = float(send(port, f"return unit.getSkill({uid}, 'smithing') or 0"))
        bill3, msg = add_bill(port, bid_f, "bill_probe_smelt", 2)
        passed = check(passed, bill3 is not None, "AI bill queued", msg)
        ai_on(port)
        # Retire the find_water spawn goal AGAIN now that the AI is
        # live: the first real tick seeds spawn goals, overwriting a
        # retirement issued while the AI was off — and the water-scout
        # goal floor (~3.0) outranks the craft entry utility.
        time.sleep(1.5)
        clear_find_water(port, uid)

        claimed = poll(port, 30, lambda: jget(
            port, f"return craft.getBill({bill3})") in ("nil", "", None, "null")
            or jget(port,
                    f"local b = craft.getBill({bill3}); "
                    f"return b and b.claimant or -1") == uid)
        passed = check(passed, claimed, "AI claims the bill (or already done)")

        done = poll(port, 150, lambda: send(
            port, f"return craft.getBill({bill3}) and 'y' or 'n'"
            ).strip('"') == "n")
        passed = check(passed, done, "2-count bill worked to completion")

        outs = ground_count_near(port, "granite_chunk", 6, 2, 3)
        passed = check(passed, outs >= 4,
                       "outputs laid down at the station (≥4 granite chunks)",
                       f"found={outs}")
        kept = inv_instance_ids(port, uid, "granite_chunk")
        passed = check(passed, kept == keep_ids,
                       "carried same-def item kept; only fresh outputs dropped",
                       f"kept={kept} expected={keep_ids}")
        xp1 = float(send(port, f"return unit.getSkill({uid}, 'smithing') or 0"))
        passed = check(passed, xp1 > xp0,
                       "crafter earned smithing XP", f"{xp0} → {xp1}")

        # --- 3. Knowledge gate ---
        bill4, msg = add_bill(port, bid_f, "bill_probe_gated", 1)
        passed = check(passed, bill4 is not None, "gated bill queued", msg)
        poll(port, 8, lambda: False)   # let the AI tick, unpaused
        untouched = send(port,
            f"local b = craft.getBill({bill4}); "
            f"return (b and not b.claimant) and 'y' or 'n'").strip('"') == "y"
        passed = check(passed, untouched,
                       "unknowing acolyte leaves the gated bill alone")
        send(port, f"unit.setKnowledge({uid}, 'metallurgy', 50); return 'ok'")
        done = poll(port, 90, lambda: send(
            port, f"return craft.getBill({bill4}) and 'y' or 'n'"
            ).strip('"') == "n")
        passed = check(passed, done, "granted knowledge unlocks the bill")

        # --- 4. Cargo rung of the sourcing ladder ---
        # Every loose bar is consumed by now; stock the ONLY remaining
        # steel_bar inside a built cargo hold. The crafter must source
        # it from storage (inventory → ground → mule → cargo) to work
        # the bill. AI off for the scripted build + stocking so the
        # delivery AI doesn't race the fixture setup.
        ai_off(port)
        bid_c = spawn_station(port, uid, "cargo_hold_S", 2, 6,
                              {"steel_plate": 10, "steel_bar": 24,
                               "steel_hardware": 10, "electric_motor": 2,
                               "processing_unit": 2}, progress=5000)
        send(port, f"unit.addItem({uid}, 'steel_bar'); "
                   f"unit.depositToCargo({uid}, {bid_c}, 'steel_bar'); "
                   f"return 'ok'")
        stored = int(float(send(port,
            f"local n=0; for _,it in ipairs(building.getStorage({bid_c}) "
            f"or {{}}) do if it.defName=='steel_bar' then n=n+1 end end; "
            f"return n")))
        loose = ground_count_near(port, "steel_bar", 5, 4, 40)
        passed = check(passed, stored == 1 and loose == 0,
                       "fixture: the only bar lives in cargo storage",
                       f"stored={stored} loose={loose}")
        bill5, msg = add_bill(port, bid_f, "bill_probe_smelt", 1)
        passed = check(passed, bill5 is not None, "cargo-sourced bill queued",
                       msg)
        ai_on(port)
        done = poll(port, 150, lambda: send(
            port, f"return craft.getBill({bill5}) and 'y' or 'n'"
            ).strip('"') == "n")
        emptied = int(float(send(port,
            f"local n=0; for _,it in ipairs(building.getStorage({bid_c}) "
            f"or {{}}) do if it.defName=='steel_bar' then n=n+1 end end; "
            f"return n")))
        passed = check(passed, done and emptied == 0,
                       "bill sourced from cargo storage worked to completion",
                       f"done={done} left_in_store={emptied}")

        print("\n" + ("ALL CRAFT BILL CHECKS PASSED" if passed
                      else "SOME FAILED"))
        return 0 if passed else 1
    finally:
        quit_engine(port, proc)


if __name__ == "__main__":
    sys.exit(main())
