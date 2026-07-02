#!/usr/bin/env python3
"""Crafting probe (#325 + #326 + #343) — catalogue, execute, stations, quality.

Boots a headless engine on a flat arena, loads defs + data/recipes +
data/buildings, spawns an acolyte, then checks the craft.* Lua API
end-to-end:

  1. Catalogue: craft.getNames lists the shipped recipe; craft.get returns
     the full shape (station / work / inputs / outputs).
  2. Refusals: unknown recipe id; missing inputs (inventory untouched).
  3. Execute: with 2 steel_bar carried, forge_steel_dagger consumes both
     bars and appends a factory-new dagger (condition 100, sharpness 100).
  4. Fuel + knowledge (probe-authored YAML loaded at runtime): execution
     is refused without the knowledge, refused short of fuel (nothing
     consumed), and succeeds once knowledge + inputs + fuel are all
     present — consuming inputs AND fuel, producing a count>1 output.
  5. Work stations (#326): furnace + workbench are built through the real
     material-delivery + build-progress machinery; each advertises its
     operations (building.getOperations); building.findStation routes by
     operation and only matches BUILT stations; craft.executeAt is
     refused at an unbuilt station, at the wrong station, from a
     non-adjacent unit and for an unknown building — and succeeds at the
     right built station with the unit adjacent, consuming like execute.
  6. Quality (#343): a skill-tagged recipe's output iiQuality tracks the
     crafter deterministically — 0.7*skill + 0.3*knowledge when the
     recipe is knowledge-gated, plain skill level when not (the shipped
     smithing-tagged dagger) — verified across two crafter builds via
     unit.setSkill / unit.setKnowledge, through both craft.execute and
     craft.executeAt at a built station.

Usage: python3 tools/craft_probe.py [--port 9317]
"""
import argparse, glob, json, socket, subprocess, sys, time

SPROOT = "/tmp"
TEST_YAML = f"{SPROOT}/craft_probe_recipes.yaml"

# Station kind "smelt" = the furnace's operation (#326). Before stations
# landed this said "furnace"; the recipe station vocabulary is the
# operation names (smelt / forge / assemble).
TEST_RECIPES = """\
recipes:
  - id: craft_test_fuelled
    station: smelt
    inputs:
      - item: steel_bar
    fuel:
      item: rations
      count: 2
    outputs:
      - item: granite_chunk
        count: 3
    knowledge: metallurgy
  - id: craft_test_quality
    station: forge
    skill: probe_smith
    knowledge: metallurgy
    inputs:
      - item: steel_bar
    outputs:
      - item: steel_dagger
"""


def send(port, lua, timeout=10.0):
    with socket.create_connection(("localhost", port), timeout=timeout) as s:
        s.settimeout(timeout)
        f = s.makefile("rw")
        f.readline()              # banner
        f.write(lua + "\n")
        f.flush()
        return f.readline().strip().lstrip("> ").strip()


def jget(port, lua, timeout=10.0):
    raw = send(port, lua, timeout)
    try:
        return json.loads(raw)
    except json.JSONDecodeError:
        return raw.strip('"')


def boot(port, log_path):
    log = open(log_path, "w")
    proc = subprocess.Popen(
        ["cabal", "run", "-v0", "exe:synarchy", "--",
         "--headless", "--port", str(port)],
        stdout=log, stderr=subprocess.STDOUT)
    for _ in range(300):
        time.sleep(0.2)
        if proc.poll() is not None:
            sys.exit(f"engine exited before READY; see {log_path}")
        try:
            if "READY" in open(log_path).read():
                return proc
        except FileNotFoundError:
            pass
    proc.kill()
    sys.exit("engine never printed READY")


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
    # every spawned unit headless. The station checks (phase 5+) assume
    # the crafter STAYS on its spawn tile for footprint adjacency, so
    # neutralise the AI tick (same trick as movement_probe) — nothing
    # under test here needs the AI.
    send(port,
         "pcall(function() require('scripts.unit_ai').update = function() end end); "
         "return 'ai-off'")


def count_item(port, uid, name):
    return int(float(send(port,
        f"local c=0; for _,it in ipairs(unit.getInventory({uid}) or {{}}) do "
        f"if it.defName=='{name}' then c=c+1 end end; return c")))


def instances_of(port, uid, name):
    r = jget(port,
        f"local out={{}}; for _,it in ipairs(unit.getInventory({uid}) or {{}}) do "
        f"if it.defName=='{name}' then out[#out+1]={{id=it.instanceId,"
        f"cond=it.condition or -1,sharp=it.sharpness,"
        f"qual=it.quality or -1}} end end; return out")
    return r if isinstance(r, list) else []


def execute(port, uid, recipe_id):
    """→ (ok: bool, err: str)."""
    raw = send(port,
        f"local ok,err = craft.execute({uid}, '{recipe_id}'); "
        f"return (ok and 'OK') or ('ERR:'..tostring(err))").strip('"')
    return raw == "OK", raw


def execute_at(port, uid, recipe_id, bid):
    """→ (ok: bool, err: str)."""
    raw = send(port,
        f"local ok,err = craft.executeAt({uid}, '{recipe_id}', {bid}); "
        f"return (ok and 'OK') or ('ERR:'..tostring(err))").strip('"')
    return raw == "OK", raw


def spawn_station(port, uid, def_name, gx, gy, materials):
    """Spawn def_name at (gx, gy) and deliver its build materials from
    the unit through the real machinery (unit.addItem →
    unit.transferItemToBuilding). Returns the building id, still
    UNBUILT (no build progress accrued yet)."""
    raw = send(port, f"return building.spawn('{def_name}', {gx}, {gy})")
    try:
        bid = int(float(raw))
    except ValueError:
        sys.exit(f"building.spawn('{def_name}') failed: {raw}")
    for _ in range(50):        # spawn is queued to the building thread
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
    return bid


def check(passed, ok, label, detail=""):
    print(f"  [{'PASS' if ok else 'FAIL'}] {label}" + (f": {detail}" if detail else ""))
    return passed and ok


def main():
    ap = argparse.ArgumentParser()
    ap.add_argument("--port", type=int, default=9317)
    args = ap.parse_args()
    port = args.port
    passed = True

    proc = boot(port, f"{SPROOT}/craft_probe_engine.log")
    try:
        bootstrap(port)
        uid = int(float(send(port, "return unit.spawn('acolyte', 2, 2)")))
        if uid < 0:
            sys.exit("unit.spawn failed")
        time.sleep(1.0)

        # --- 1. Catalogue ---
        names = jget(port, "return craft.getNames()")
        ok = isinstance(names, list) and "forge_steel_dagger" in names
        passed = check(passed, ok, "getNames lists the shipped recipe", names)
        r = jget(port, "return craft.get('forge_steel_dagger')")
        ok = (isinstance(r, dict) and r.get("station") == "forge"
              and r.get("work") == 20
              and r.get("inputs") == [{"item": "steel_bar", "count": 2}]
              and r.get("outputs") == [{"item": "steel_dagger", "count": 1}]
              and r.get("skill") == "smithing"
              and "fuel" not in r and "knowledge" not in r)
        passed = check(passed, ok, "craft.get returns the recipe shape", r)
        ok = send(port, "return craft.get('nope') and 'yes' or 'nil'").strip('"') == "nil"
        passed = check(passed, ok, "craft.get(unknown) → nil")

        # --- 2. Refusals ---
        ok_e, msg = execute(port, uid, "no_such_recipe")
        passed = check(passed, not ok_e and "unknown recipe" in msg,
                       "unknown recipe refused", msg)
        bars0 = count_item(port, uid, "steel_bar")
        daggers0 = count_item(port, uid, "steel_dagger")
        ok_e, msg = execute(port, uid, "forge_steel_dagger")
        untouched = (count_item(port, uid, "steel_bar") == bars0
                     and count_item(port, uid, "steel_dagger") == daggers0)
        passed = check(passed, not ok_e and "missing" in msg and untouched,
                       "short inputs refused, inventory untouched", msg)

        # --- 3. Execute the shipped recipe ---
        send(port, f"unit.addItem({uid},'steel_bar'); "
                   f"unit.addItem({uid},'steel_bar'); return 'ok'")
        before_ids = {d["id"] for d in instances_of(port, uid, "steel_dagger")}
        ok_e, msg = execute(port, uid, "forge_steel_dagger")
        bars_after = count_item(port, uid, "steel_bar")
        new = [d for d in instances_of(port, uid, "steel_dagger")
               if d["id"] not in before_ids]
        ok = (ok_e and bars_after == bars0 and len(new) == 1
              and new[0]["cond"] == 100 and new[0]["sharp"] == 100)
        passed = check(passed, ok,
                       "craft consumes 2 bars, appends a factory-new dagger",
                       f"ok={ok_e} bars {bars0 + 2}->{bars_after} new={new}")

        # --- 4. Fuel + knowledge (probe-authored recipe) ---
        with open(TEST_YAML, "w") as f:
            f.write(TEST_RECIPES)
        n = int(float(send(port, f"return engine.loadRecipeYaml('{TEST_YAML}')")))
        passed = check(passed, n == 2, "runtime loadRecipeYaml", f"count={n}")
        # Deterministic pantry: strip spawn-loadout rations, then stock
        # exactly 1 bar + 1 ration (fuel demands 2).
        while count_item(port, uid, "rations") > 0:
            send(port, f"unit.removeItem({uid},'rations'); return 'ok'")
        send(port, f"unit.addItem({uid},'steel_bar'); "
                   f"unit.addItem({uid},'rations'); return 'ok'")
        ok_e, msg = execute(port, uid, "craft_test_fuelled")
        passed = check(passed, not ok_e and "missing knowledge" in msg,
                       "knowledge gate refuses the unknowing", msg)
        send(port, f"unit.setKnowledge({uid},'metallurgy',1.0); return 'ok'")
        bars1 = count_item(port, uid, "steel_bar")
        ok_e, msg = execute(port, uid, "craft_test_fuelled")
        ok = (not ok_e and "missing" in msg and "rations" in msg
              and count_item(port, uid, "steel_bar") == bars1)
        passed = check(passed, ok, "short fuel refused, nothing consumed", msg)
        send(port, f"unit.addItem({uid},'rations'); return 'ok'")
        chunks0 = count_item(port, uid, "granite_chunk")
        ok_e, msg = execute(port, uid, "craft_test_fuelled")
        ok = (ok_e and count_item(port, uid, "steel_bar") == bars1 - 1
              and count_item(port, uid, "rations") == 0
              and count_item(port, uid, "granite_chunk") == chunks0 + 3)
        passed = check(passed, ok,
                       "fuelled craft consumes inputs+fuel, outputs count 3",
                       f"ok={ok_e} {msg}")

        # --- 5. Work stations (#326) ---
        defs = jget(port, "return building.listDefs()")
        names5 = {d["name"] for d in defs} if isinstance(defs, list) else set()
        passed = check(passed, {"furnace", "workbench"} <= names5,
                       "station defs loaded", sorted(names5))
        ok = send(port, "return building.findStation('smelt') and 'hit' or 'nil'"
                  ).strip('"') == "nil"
        passed = check(passed, ok, "findStation with nothing built → nil")

        # Furnace one tile east of the unit at (2,2), materials through
        # the real delivery machinery — but no build progress yet.
        bid_f = spawn_station(port, uid, "furnace", 3, 2,
                              {"granite_chunk": 6, "steel_bar": 2})
        ok_e, msg = execute_at(port, uid, "craft_test_fuelled", bid_f)
        passed = check(passed, not ok_e and "not built" in msg,
                       "executeAt refused at unbuilt station", msg)
        ops = jget(port, f"return building.getOperations({bid_f})")
        ok = isinstance(ops, list) and sorted(ops) == ["repair", "smelt"]
        passed = check(passed, ok, "furnace advertises smelt+repair", ops)
        send(port, f"building.addBuildProgress({bid_f}, 200); return 'ok'")
        act = send(port, f"return building.getActivity({bid_f})").strip('"')
        passed = check(passed, act == "built", "furnace reaches built", act)
        hit = send(port, f"local b=building.findStation('smelt'); return b or -1")
        passed = check(passed, int(float(hit)) == bid_f,
                       "findStation('smelt') → furnace", hit)
        ok = send(port, "return building.findStation('no_such_op') and 'hit' or 'nil'"
                  ).strip('"') == "nil"
        passed = check(passed, ok, "findStation(unknown op) → nil")
        ok_e, msg = execute_at(port, uid, "forge_steel_dagger", bid_f)
        passed = check(passed, not ok_e and "does not offer" in msg,
                       "wrong station refused (forge recipe at furnace)", msg)

        # Workbench on the unit's other side.
        bid_w = spawn_station(port, uid, "workbench", 1, 2,
                              {"wood_log": 4, "steel_hardware": 4,
                               "steel_bar": 2})
        send(port, f"building.addBuildProgress({bid_w}, 150); return 'ok'")
        hit = send(port, f"local b=building.findStation('forge'); return b or -1")
        passed = check(passed, int(float(hit)) == bid_w,
                       "findStation('forge') → workbench", hit)
        # Both stations advertise "repair" — the routing hook the repair
        # flows (#301/#302) plug into.
        hit = send(port,
                   f"local b=building.findStation('repair', 2, 2); return b or -1")
        passed = check(passed, int(float(hit)) in (bid_f, bid_w),
                       "findStation('repair') finds a station", hit)

        # Success at the right station: same consumption as craft.execute.
        send(port, f"unit.addItem({uid},'steel_bar'); "
                   f"unit.addItem({uid},'steel_bar'); return 'ok'")
        bars5 = count_item(port, uid, "steel_bar")
        before_ids = {d["id"] for d in instances_of(port, uid, "steel_dagger")}
        ok_e, msg = execute_at(port, uid, "forge_steel_dagger", bid_w)
        new = [d for d in instances_of(port, uid, "steel_dagger")
               if d["id"] not in before_ids]
        ok = (ok_e and count_item(port, uid, "steel_bar") == bars5 - 2
              and len(new) == 1 and new[0]["cond"] == 100)
        passed = check(passed, ok, "executeAt at workbench crafts the dagger",
                       f"{msg} new={new}")

        # Smelt at the furnace (fuel + knowledge recipe from phase 4).
        send(port, f"unit.addItem({uid},'steel_bar'); "
                   f"unit.addItem({uid},'rations'); "
                   f"unit.addItem({uid},'rations'); return 'ok'")
        chunks5 = count_item(port, uid, "granite_chunk")
        ok_e, msg = execute_at(port, uid, "craft_test_fuelled", bid_f)
        ok = ok_e and count_item(port, uid, "granite_chunk") == chunks5 + 3
        passed = check(passed, ok, "executeAt at furnace runs the smelt", msg)

        # Adjacency: a far unit is refused before any consumption.
        uid2 = int(float(send(port, "return unit.spawn('acolyte', 12, 12)")))
        time.sleep(0.5)
        ok_e, msg = execute_at(port, uid2, "forge_steel_dagger", bid_w)
        passed = check(passed, not ok_e and "not adjacent" in msg,
                       "far unit refused (not adjacent)", msg)
        ok_e, msg = execute_at(port, uid, "forge_steel_dagger", 99999)
        passed = check(passed, not ok_e and "no such building" in msg,
                       "unknown building refused", msg)

        # --- 6. Quality tracks the crafter (#343) ---
        def craft_quality(recipe_id, bid=None):
            """Craft one dagger, return the new instance's quality.
            bid=None uses the station-less craft.execute debug verb;
            a bid routes through craft.executeAt at that station."""
            send(port, f"unit.addItem({uid},'steel_bar'); return 'ok'")
            if recipe_id == "forge_steel_dagger":
                send(port, f"unit.addItem({uid},'steel_bar'); return 'ok'")
            prior = {d["id"] for d in instances_of(port, uid, "steel_dagger")}
            if bid is None:
                ok_e, msg = execute(port, uid, recipe_id)
            else:
                ok_e, msg = execute_at(port, uid, recipe_id, bid)
            fresh = [d for d in instances_of(port, uid, "steel_dagger")
                     if d["id"] not in prior]
            if not ok_e or len(fresh) != 1:
                return None, f"ok={ok_e} msg={msg} fresh={fresh}"
            return fresh[0]["qual"], ""
        # Skilled + learned: 0.7*90 + 0.3*80 = 87.
        send(port, f"unit.setSkill({uid},'probe_smith',90); "
                   f"unit.setKnowledge({uid},'metallurgy',80); return 'ok'")
        q, detail = craft_quality("craft_test_quality")
        ok = q is not None and abs(q - 87.0) < 0.01
        passed = check(passed, ok, "skill 90 + knowledge 80 -> quality 87",
                       detail or f"qual={q}")
        # Clumsy + barely read the manual: 0.7*10 + 0.3*20 = 13.
        send(port, f"unit.setSkill({uid},'probe_smith',10); "
                   f"unit.setKnowledge({uid},'metallurgy',20); return 'ok'")
        q, detail = craft_quality("craft_test_quality")
        ok = q is not None and abs(q - 13.0) < 0.01
        passed = check(passed, ok, "skill 10 + knowledge 20 -> quality 13",
                       detail or f"qual={q}")
        # Shipped recipe: skill-tagged, no knowledge gate -> plain skill.
        send(port, f"unit.setSkill({uid},'smithing',55); return 'ok'")
        q, detail = craft_quality("forge_steel_dagger")
        ok = q is not None and abs(q - 55.0) < 0.01
        passed = check(passed, ok,
                       "shipped dagger quality = smithing level 55",
                       detail or f"qual={q}")
        # Same formula through the station path (#326 + #343): executeAt
        # at the built workbench from phase 5.
        q, detail = craft_quality("forge_steel_dagger", bid_w)
        ok = q is not None and abs(q - 55.0) < 0.01
        passed = check(passed, ok,
                       "executeAt at workbench carries quality 55",
                       detail or f"qual={q}")

        print("\n" + ("ALL CRAFT CHECKS PASSED" if passed else "SOME FAILED"))
        return 0 if passed else 1
    finally:
        try:
            send(port, "engine.quit()")
        except Exception:
            pass
        time.sleep(1.0)
        proc.kill()


if __name__ == "__main__":
    sys.exit(main())
