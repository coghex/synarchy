#!/usr/bin/env python3
"""Cooking probe (#346) — kitchen workshop + cooking skill/knowledge +
basic_food/coffee content, on top of the generic craft/quality/
temperature machinery already gated by craft_probe.py / item_temp_probe.py.

Boots a headless engine on a flat arena, loads defs + data/recipes +
data/buildings, spawns an acolyte, then checks:

  1. Content shape: brew_coffee is in the catalogue with the documented
     station/inputs/outputs/skill/knowledge/outputTemp; the acolyte def
     spawns with the cooking skill and basic_cuisine knowledge; the
     kitchen building offers the "cooking" operation.
  2. All-or-nothing: short of one input (water) with the other on hand
     (coffee_grounds) is refused, pantry untouched.
  3. Brew: at a built kitchen, water + coffee_grounds are consumed and a
     factory-new coffee_pot appears, already full (currentFill 1.0).
  4. Quality (#343): the pot's quality tracks 0.7*cooking + 0.3*
     basic_cuisine, same formula as every other skill-tagged recipe.
  5. Temperature (#344/#346): the fresh pot reads 100 °C via
     unit.getItemTemp — the recipe's output_temp hook.

Usage: python3 tools/cooking_probe.py [--port 9346]
"""
import argparse, glob, json, socket, subprocess, sys, time

SPROOT = "/tmp"


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
    # Same trick as craft_probe/movement_probe: the station checks assume
    # the crafter stays put for footprint adjacency, so neutralise the
    # auto-loaded unit_ai wander tick.
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
        f"fill=it.currentFill or -1,qual=it.quality or -1}} end end; return out")
    return r if isinstance(r, list) else []


def execute_at(port, uid, recipe_id, bid):
    raw = send(port,
        f"local ok,err = craft.executeAt({uid}, '{recipe_id}', {bid}); "
        f"return (ok and 'OK') or ('ERR:'..tostring(err))").strip('"')
    return raw == "OK", raw


def spawn_station(port, uid, def_name, gx, gy, materials):
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
    send(port, f"building.addBuildProgress({bid}, 500); return 'ok'")
    act = send(port, f"return building.getActivity({bid})").strip('"')
    if act != "built":
        sys.exit(f"{def_name} never reached built (activity={act})")
    return bid


def check(passed, ok, label, detail=""):
    print(f"  [{'PASS' if ok else 'FAIL'}] {label}" + (f": {detail}" if detail else ""))
    return passed and ok


def main():
    ap = argparse.ArgumentParser()
    ap.add_argument("--port", type=int, default=9346)
    args = ap.parse_args()
    port = args.port
    passed = True

    proc = boot(port, f"{SPROOT}/cooking_probe_engine.log")
    try:
        bootstrap(port)
        uid = int(float(send(port, "return unit.spawn('acolyte', 2, 2)")))
        if uid < 0:
            sys.exit("unit.spawn failed")
        time.sleep(1.0)

        # --- 1. Content shape ---
        names = jget(port, "return craft.getNames()")
        ok = isinstance(names, list) and "brew_coffee" in names
        passed = check(passed, ok, "getNames lists brew_coffee", names)
        r = jget(port, "return craft.get('brew_coffee')")
        ok = (isinstance(r, dict) and r.get("station") == "cooking"
              and r.get("skill") == "cooking"
              and r.get("knowledge") == "basic_cuisine"
              and r.get("outputTemp") == 100
              and {i["item"] for i in r.get("inputs", [])}
                  == {"water", "coffee_grounds"}
              and r.get("outputs") == [{"item": "coffee_pot", "count": 1}])
        passed = check(passed, ok, "craft.get('brew_coffee') shape", r)

        cooking_skill = send(port, f"return unit.getSkill({uid}, 'cooking')")
        ok = float(cooking_skill.strip('"')) > 0
        passed = check(passed, ok, "acolyte spawns with a cooking skill",
                       cooking_skill)
        know = send(port, f"return unit.getKnowledge({uid}, 'basic_cuisine')")
        ok = float(know.strip('"')) > 0
        passed = check(passed, ok,
                       "acolyte spawns knowing basic_cuisine", know)

        bid = spawn_station(port, uid, "kitchen", 3, 2,
                             {"granite_chunk": 4, "wood_log": 2,
                              "steel_hardware": 2})
        ops = jget(port, f"return building.getOperations({bid})")
        ok = ops == ["cooking"]
        passed = check(passed, ok, "kitchen advertises cooking", ops)
        hit = send(port, "local b=building.findStation('cooking'); return b or -1")
        passed = check(passed, int(float(hit)) == bid,
                       "findStation('cooking') -> kitchen", hit)

        # --- 2. All-or-nothing: coffee_grounds alone is short of water ---
        # (the knowledge gate itself — refused when a crafter doesn't KNOW
        # the recipe's knowledge — is the generic mechanic craft_probe.py
        # already exercises; acolytes spawn already knowing basic_cuisine,
        # so there's no way to un-know it through the public API to
        # re-probe that same gate against this content specifically.)
        send(port, f"unit.addItem({uid},'coffee_grounds'); return 'ok'")
        ok_e, msg = execute_at(port, uid, "brew_coffee", bid)
        untouched = count_item(port, uid, "coffee_grounds") == 1
        passed = check(passed, not ok_e and "missing" in msg
                       and "water" in msg and untouched,
                       "short water refused, coffee_grounds untouched", msg)

        # --- 3. Brew ---
        send(port, f"unit.addItem({uid},'water'); return 'ok'")
        send(port, f"unit.setSkill({uid}, 'cooking', 80); "
                   f"unit.setKnowledge({uid}, 'basic_cuisine', 60); return 'ok'")
        before_ids = {p["id"] for p in instances_of(port, uid, "coffee_pot")}
        ok_e, msg = execute_at(port, uid, "brew_coffee", bid)
        fresh = [p for p in instances_of(port, uid, "coffee_pot")
                 if p["id"] not in before_ids]
        ok = (ok_e and count_item(port, uid, "water") == 0
              and count_item(port, uid, "coffee_grounds") == 0
              and len(fresh) == 1 and abs(fresh[0]["fill"] - 1.0) < 0.001)
        passed = check(passed, ok,
                       "brew consumes water+grounds, pot spawns full",
                       f"ok={ok_e} {msg} fresh={fresh}")

        # --- 4. Quality (#343): 0.7*80 + 0.3*60 = 74 ---
        if fresh:
            q = fresh[0]["qual"]
            ok = abs(q - 74.0) < 0.01
            passed = check(passed, ok,
                           "cooking 80 + basic_cuisine 60 -> quality 74",
                           f"qual={q}")
            iid = fresh[0]["id"]
            # --- 5. Temperature (#344/#346) ---
            # A little cooling happens between craft and query (the
            # world tick advances between round-trips), so allow a small
            # margin rather than pinning exactly 100 — well within it
            # unless the tracked temp were absent entirely (ambient,
            # nowhere near 100).
            temp = send(port, f"return unit.getItemTemp({uid}, {iid})")
            ok = abs(float(temp.strip('"')) - 100.0) < 2.0
            passed = check(passed, ok, "fresh pot reads ~100 C", temp)

        print("\n" + ("ALL COOKING CHECKS PASSED" if passed else "SOME FAILED"))
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
