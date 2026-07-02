#!/usr/bin/env python3
"""Crafting recipe probe (#325) — the recipe-catalogue + craft.execute gate.

Boots a headless engine on a flat arena, loads defs + data/recipes, spawns
an acolyte, then checks the craft.* Lua API end-to-end:

  1. Catalogue: craft.getNames lists the shipped recipe; craft.get returns
     the full shape (station / work / inputs / outputs).
  2. Refusals: unknown recipe id; missing inputs (inventory untouched).
  3. Execute: with 2 steel_bar carried, forge_steel_dagger consumes both
     bars and appends a factory-new dagger (condition 100, sharpness 100).
  4. Fuel + knowledge (probe-authored YAML loaded at runtime): execution
     is refused without the knowledge, refused short of fuel (nothing
     consumed), and succeeds once knowledge + inputs + fuel are all
     present — consuming inputs AND fuel, producing a count>1 output.
  5. Quality (#343): a skill-tagged recipe's output iiQuality tracks the
     crafter deterministically — 0.7*skill + 0.3*knowledge when the
     recipe is knowledge-gated, plain skill level when not (the shipped
     smithing-tagged dagger) — verified across two crafter builds via
     unit.setSkill / unit.setKnowledge.

Usage: python3 tools/craft_probe.py [--port 9317]
"""
import argparse, glob, json, socket, subprocess, sys, time

SPROOT = "/tmp"
TEST_YAML = f"{SPROOT}/craft_probe_recipes.yaml"

TEST_RECIPES = """\
recipes:
  - id: craft_test_fuelled
    station: furnace
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

        # --- 5. Quality tracks the crafter (#343) ---
        def craft_quality(recipe_id):
            """Craft one dagger, return the new instance's quality."""
            send(port, f"unit.addItem({uid},'steel_bar'); return 'ok'")
            if recipe_id == "forge_steel_dagger":
                send(port, f"unit.addItem({uid},'steel_bar'); return 'ok'")
            prior = {d["id"] for d in instances_of(port, uid, "steel_dagger")}
            ok_e, msg = execute(port, uid, recipe_id)
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
