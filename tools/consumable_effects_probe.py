#!/usr/bin/env python3
"""Consumable effects probe (#347) — drinking coffee applies hydration +
caffeine + mood + warmth scaled by the specific cup's quality (#343) and
temperature (#344), on top of the cooking content already gated by
cooking_probe.py.

Boots a headless engine on a flat arena, loads defs + data/recipes +
data/buildings, brews real coffee_pot instances (skill/knowledge dialed to
known qualities, same formula cooking_probe.py already gates), then checks:

  1. consumable.drink(uid, 'coffee_pot') applies hydration/caffeine/mood
     scaled by quality: a fresh (hot), high-quality cup restores more
     hydration/caffeine and a POSITIVE mood delta; a fresh, low-quality cup
     restores less and gives a NEGATIVE mood delta.
  2. Temperature scales warmth/caffeine independently of quality: the same
     high-quality cup, force-cooled via unit.setItemTemp, reports a lower
     `warmth` and a smaller caffeine gain on its next sip than it did hot,
     with the mood delta essentially unchanged (mood is quality-only).
  3. The sip actually drains the pot's fill (unit.modifyItemFill) and an
     empty pot is skipped by consumable.drink.
  4. The caffeine meter (brain.lua) persists on the unit as `caffeine`,
     boosts `concentration` (alertness), and decays over time.
  5. The stamina fatigue-offset (unit_resources.lua's caffeine_regen_bonus)
     measurably speeds up idle stamina regen when caffeine is present.

Usage: python3 tools/consumable_effects_probe.py [--port 9347]
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


def fget(port, lua, timeout=10.0):
    return float(send(port, lua, timeout).strip('"'))


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
    # Same trick as cooking_probe/movement_probe: neutralise the
    # auto-loaded unit_ai wander tick so units hold still.
    send(port,
         "pcall(function() require('scripts.unit_ai').update = function() end end); "
         "return 'ai-off'")


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


def brew(port, uid, mule, bid, skill, knowledge):
    """Fetch a fresh water+coffee_grounds dose from the mule and brew one
    coffee_pot at a dialed-in quality (0.7*skill + 0.3*basic_cuisine,
    same formula cooking_probe.py gates)."""
    for item in ("coffee_grounds", "water"):
        send(port, f"return unit.transferItemToUnit({mule}, {uid}, '{item}')")
    send(port, f"unit.setSkill({uid}, 'cooking', {skill}); "
               f"unit.setKnowledge({uid}, 'basic_cuisine', {knowledge}); "
               f"return 'ok'")
    before = {p["id"] for p in instances_of(port, uid, "coffee_pot")}
    ok, msg = execute_at(port, uid, "brew_coffee", bid)
    if not ok:
        sys.exit(f"brew_coffee failed: {msg}")
    fresh = [p for p in instances_of(port, uid, "coffee_pot")
             if p["id"] not in before]
    if len(fresh) != 1:
        sys.exit(f"expected exactly one fresh coffee_pot, got {fresh}")
    return fresh[0]["id"]


def drink(port, uid):
    r = jget(port, f"return require('scripts.consumable').drink({uid}, 'coffee_pot')")
    return r if isinstance(r, dict) else None


def check(passed, ok, label, detail=""):
    print(f"  [{'PASS' if ok else 'FAIL'}] {label}" + (f": {detail}" if detail else ""))
    return passed and ok


def main():
    ap = argparse.ArgumentParser()
    ap.add_argument("--port", type=int, default=9347)
    args = ap.parse_args()
    port = args.port
    passed = True

    proc = boot(port, f"{SPROOT}/consumable_effects_probe_engine.log")
    try:
        bootstrap(port)
        uid = int(float(send(port, "return unit.spawn('acolyte', 2, 2)")))
        mule = int(float(send(port, "return unit.spawn('technomule', 5, 2)")))
        if uid < 0 or mule < 0:
            sys.exit("spawn failed")
        time.sleep(1.0)
        bid = spawn_station(port, uid, "kitchen", 3, 2,
                             {"granite_chunk": 4, "wood_log": 2,
                              "steel_hardware": 2})

        # --- 0. Unknown / empty inputs are refused cleanly ---
        reason = send(port,
            f"local r,e = require('scripts.consumable').drink({uid}, 'not_a_thing'); "
            f"return (r == nil) and e or ('FAIL:got-a-result:'..tostring(r))"
            ).strip('"')
        ok = reason == "no consumable config for not_a_thing"
        passed = check(passed, ok, "unregistered defName refused with a reason", reason)
        r = drink(port, uid)
        passed = check(passed, r is None, "drink with nothing to drink -> nil", r)

        # --- 1. Fresh, HIGH quality cup: positive mood, strong effects ---
        hot_hi_id = brew(port, uid, mule, bid, skill=100, knowledge=100)
        temp = fget(port, f"return unit.getItemTemp({uid}, {hot_hi_id})")
        ok = abs(temp - 100.0) < 2.0
        passed = check(passed, ok, "fresh brew reads ~100 C", temp)

        hot_hi = drink(port, uid)
        ok = (hot_hi is not None and abs(hot_hi["quality"] - 100.0) < 0.01
              and hot_hi["warmth"] > 0.99 and hot_hi["mood"] > 0
              and hot_hi["hydration"] > 0 and hot_hi["caffeine"] > 0)
        passed = check(passed, ok,
                       "hot excellent cup: full warmth, positive mood, real hydration+caffeine",
                       hot_hi)

        fill_after = instances_of(port, uid, "coffee_pot")
        this_pot = next((p for p in fill_after if p["id"] == hot_hi_id), None)
        ok = this_pot is not None and abs(this_pot["fill"] - 0.75) < 0.001
        passed = check(passed, ok, "sip drained the pot's fill by 0.25 L",
                       this_pot)

        # --- 2. Same (still-hot-quality) pot, forced COLD: warmth + caffeine
        #        drop, mood stays governed by the same quality (still positive,
        #        roughly the same magnitude per sip). ---
        send(port, f"return unit.setItemTemp({uid}, {hot_hi_id}, 20)")
        cold_hi = drink(port, uid)
        ok = (cold_hi is not None and cold_hi["warmth"] < hot_hi["warmth"]
              and cold_hi["caffeine"] < hot_hi["caffeine"])
        passed = check(passed, ok,
                       "cooled cup: lower warmth -> lower caffeine gain than the hot sip",
                       {"hot": hot_hi, "cold": cold_hi})
        ok = cold_hi is not None and abs(cold_hi["mood"] - hot_hi["mood"]) < 1e-6
        passed = check(passed, ok,
                       "mood delta unaffected by temperature (quality-only)",
                       {"hot_mood": hot_hi["mood"], "cold_mood": cold_hi["mood"]})

        # Fully drain this pot now (leaving it in inventory, EMPTY, ahead
        # of the fresh pot brewed in step 3) so that step 3's checks below
        # exercise the exact "earlier empty same-def pot precedes a full
        # one" shape unit.modifyItemFillById exists to get right.
        send(port, f"return unit.modifyItemFill({uid}, 'coffee_pot', -10)")
        r = drink(port, uid)
        passed = check(passed, r is None,
                        "pot emptied by modifyItemFill -> drink() finds nothing", r)

        # --- 3. Fresh, LOW quality cup, brewed while an earlier, now-EMPTY
        #        coffee_pot (hot_hi_id) still sits in inventory ahead of it:
        #        negative mood, weaker hydration/caffeine than the
        #        excellent cup, AND the fill drop must land on the fresh
        #        pot itself, not get silently clamped against the empty
        #        one that precedes it (the free-refill bug: findFirstWithFill
        #        correctly picks the full pot for quality/temp, but a naive
        #        modifyItemFill-by-defName drain would hit the empty one
        #        first and clamp at zero, leaving the full pot undrained
        #        and endlessly re-sippable). ---
        hot_lo_id = brew(port, uid, mule, bid, skill=10, knowledge=10)
        before_pots = {p["id"]: p["fill"] for p in instances_of(port, uid, "coffee_pot")}
        hot_lo = drink(port, uid)
        after_pots = {p["id"]: p["fill"] for p in instances_of(port, uid, "coffee_pot")}
        ok = (hot_lo is not None and hot_lo["quality"] < 50
              and hot_lo["mood"] < 0
              and hot_lo["hydration"] < hot_hi["hydration"]
              and hot_lo["caffeine"] < hot_hi["caffeine"])
        passed = check(passed, ok,
                       "hot atrocious cup: negative mood, less hydration+caffeine than excellent",
                       hot_lo)

        ok = (hot_lo is not None
              and abs(after_pots.get(hot_lo_id, -1)
                       - (before_pots[hot_lo_id] - hot_lo["sip"])) < 0.001
              and after_pots.get(hot_hi_id, -1) == 0)
        passed = check(passed, ok,
                       "fill drop lands on the sipped instance, not the earlier empty pot",
                       {"before": before_pots, "after": after_pots,
                        "hot_hi_id": hot_hi_id, "hot_lo_id": hot_lo_id})

        # Repeated drinks must keep draining THIS SAME pot (no free
        # infinite sips from it re-reading as "full" every time) until it
        # is genuinely exhausted.
        for _ in range(3):   # 0.75 L left / 0.25 L per sip
            drink(port, uid)
        final_pots = {p["id"]: p["fill"] for p in instances_of(port, uid, "coffee_pot")}
        ok = final_pots.get(hot_lo_id, 1) <= 0.001
        passed = check(passed, ok,
                       "repeated drinks actually exhaust the pot (no free infinite sips)",
                       final_pots)
        r = drink(port, uid)
        passed = check(passed, r is None,
                       "exhausted pot -> drink() finds nothing (no more free sips)", r)

        # --- 4. Caffeine meter + concentration + decay (brain.lua) ---
        caf = fget(port, f"return unit.getStat({uid}, 'caffeine')")
        ok = caf > 0
        passed = check(passed, ok, "caffeine stat set on the unit", caf)

        maxStam = fget(port, f"return require('scripts.unit_stats').get({uid}, 'max_stamina')")
        # Concentration saturates at 1.0 near full stamina/no pain, leaving
        # no headroom for the caffeine bonus to show — knock stamina down
        # first (brain.tick's own next pass recomputes concentration off
        # this, hence the sleeps below before each read).
        send(port, f"return unit.setStat({uid}, 'stamina', {maxStam * 0.2})")
        send(port, f"return unit.setStat({uid}, 'caffeine', 1.0)")
        time.sleep(0.3)
        conc_with = fget(port, f"return require('scripts.brain').concentration({uid})")
        send(port, f"return unit.setStat({uid}, 'caffeine', 0)")
        time.sleep(0.3)
        conc_without = fget(port, f"return require('scripts.brain').concentration({uid})")
        ok = conc_with > conc_without
        passed = check(passed, ok,
                       "caffeine boosts concentration (alertness)",
                       {"with": conc_with, "without": conc_without})

        send(port, f"return unit.setStat({uid}, 'caffeine', 1.0)")
        time.sleep(2.0)
        caf_after = fget(port, f"return unit.getStat({uid}, 'caffeine')")
        ok = 0 < caf_after < 1.0
        passed = check(passed, ok, "caffeine decays over time", caf_after)

        # --- 5. Stamina fatigue-offset ---
        send(port, f"return unit.setStat({uid}, 'caffeine', 0)")
        send(port, f"return unit.setStat({uid}, 'stamina', {maxStam * 0.3})")
        time.sleep(2.0)
        stam_baseline = fget(port, f"return unit.getStat({uid}, 'stamina')")

        send(port, f"return unit.setStat({uid}, 'caffeine', 1.0)")
        send(port, f"return unit.setStat({uid}, 'stamina', {maxStam * 0.3})")
        time.sleep(2.0)
        stam_caffeinated = fget(port, f"return unit.getStat({uid}, 'stamina')")
        ok = stam_caffeinated > stam_baseline
        passed = check(passed, ok,
                       "caffeine speeds up idle stamina regen (fatigue offset)",
                       {"baseline": stam_baseline, "caffeinated": stam_caffeinated})

        print("\n" + ("ALL CONSUMABLE-EFFECTS CHECKS PASSED" if passed else "SOME FAILED"))
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
