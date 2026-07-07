#!/usr/bin/env python3
"""Foraging / interactive-flora probe (#94).

Boots a headless engine on a real generated world (flora placement needs
worldgen; the arena has no plants), then checks:

  1. API: world.findHarvestableFlora locates a harvestable tile in the
     loaded region; world.getFloraAt reports it harvestable.
  2. Harvest: world.harvestFlora spawns yield ground items, flips the
     tile to harvestable=false with a live regrowthRemaining timer, and
     a second harvest is refused (nil).
  3. Regrowth: at a cranked time scale the timer counts down and the
     tile returns to harvestable.
  4. Save/load: a fresh harvest's regrowth timer survives
     save → loadSave (world-page map wpsFloraHarvests, v66).
  5. AI: an acolyte with an empty stomach and no carried food forages a
     nearby plant autonomously (real unit_ai stack, not neutralised)
     and ends up with food eaten or in hand.

Usage: python3 tools/foraging_probe.py [--port 9173] [--seed 42]
       [--size 64] [--plates 3]
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
    for pattern, fn in [
        ("data/substances/*.yaml", "engine.loadSubstanceYaml"),
        ("data/infections/*.yaml", "engine.loadInfectionYaml"),
        ("data/items/*.yaml",      "engine.loadItemYaml"),
        ("data/equipment/*.yaml",  "engine.loadEquipmentYaml"),
        ("data/materials/*.yaml",  "engine.loadMaterialYaml"),
        ("data/flora/*.yaml",      "engine.loadFloraYaml"),
        ("data/units/*.yaml",      "engine.loadUnitYaml"),
    ]:
        for path in sorted(glob.glob(pattern)):
            send(port, f"{fn}('{path}'); return 'ok'")


def find_harvestable(port, span=4):
    """Scan sample points across the loaded region for the nearest
    harvestable tile; returns (gx, gy, species) or None."""
    for sx in range(-span * 16, span * 16 + 1, 32):
        for sy in range(-span * 16, span * 16 + 1, 32):
            r = jget(port, f"return world.findHarvestableFlora({sx},{sy},64)")
            if isinstance(r, dict):
                return r["gx"], r["gy"], r["id"]
    return None


def main():
    ap = argparse.ArgumentParser()
    ap.add_argument("--port", type=int, default=9173)
    ap.add_argument("--seed", type=int, default=42)
    ap.add_argument("--size", type=int, default=64)
    ap.add_argument("--plates", type=int, default=3)
    args = ap.parse_args()
    port = args.port
    passed = True

    proc = boot(port, f"{SPROOT}/foraging_probe_engine.log")
    try:
        bootstrap(port)
        send(port, f"world.init('probe', {args.seed}, {args.size}, {args.plates}); return 'ok'")
        send(port, "return world.waitForInit(300)", timeout=310)
        send(port, "world.show('probe'); return 'ok'")
        send(port, "return world.loadChunksInRegion(-4, -4, 4, 4)", timeout=30)
        send(port, "return world.waitForChunks(120)", timeout=125)

        # --- 1. Find + query ---
        found = find_harvestable(port)
        if not found:
            print("  [FAIL] no harvestable flora found in the loaded region "
                  "(seed/climate has no raspberry/clover here — try another seed)")
            return 1
        gx, gy, species = found
        fl = jget(port, f"return world.getFloraAt({gx},{gy})")
        ok1 = isinstance(fl, dict) and fl.get("harvestable") is True \
              and fl.get("regrowthRemaining", -1) == 0
        passed &= ok1
        print(f"  [{'PASS' if ok1 else 'FAIL'}] getFloraAt reports harvestable: "
              f"{species} at ({gx},{gy}) → {fl}")

        # --- 2. Harvest: yields spawn, tile flips, re-harvest refused ---
        yields = jget(port, f"return world.harvestFlora({gx},{gy})")
        ok2 = isinstance(yields, list) and len(yields) >= 1 \
              and all("gid" in y and "id" in y for y in yields)
        fl2 = jget(port, f"return world.getFloraAt({gx},{gy})")
        ok2b = isinstance(fl2, dict) and fl2.get("harvestable") is False \
               and fl2.get("regrowthRemaining", 0) > 0
        again = send(port, f"return world.harvestFlora({gx},{gy}) and 'yes' or 'nil'")
        ok2c = again.strip('"') == "nil"
        passed &= ok2 and ok2b and ok2c
        print(f"  [{'PASS' if ok2 else 'FAIL'}] harvest spawns ground yields: {yields}")
        print(f"  [{'PASS' if ok2b else 'FAIL'}] tile regrowing after harvest: {fl2}")
        print(f"  [{'PASS' if ok2c else 'FAIL'}] double-harvest refused: {again}")

        # --- 3. Regrowth under cranked clock ---
        # clover 43200 gs / raspberry 86400 gs; at timeScale 3000
        # (game-min/real-sec) that's 0.24 / 0.48 real-seconds.
        send(port, "world.setTimeScale('probe', 3000); return 'ok'")
        time.sleep(3.0)
        send(port, "world.setTimeScale('probe', 1); return 'ok'")
        fl3 = jget(port, f"return world.getFloraAt({gx},{gy})")
        ok3 = isinstance(fl3, dict) and fl3.get("harvestable") is True
        passed &= ok3
        print(f"  [{'PASS' if ok3 else 'FAIL'}] regrowth completes on the game clock: {fl3}")

        # --- 4. Save/load round-trip of a live timer ---
        yields2 = jget(port, f"return world.harvestFlora({gx},{gy})")
        if not isinstance(yields2, list):
            print("  [FAIL] re-harvest for the save test failed")
            passed = False
        send(port, "engine.saveWorld('probe', 'foraging_v66_check'); return 'ok'")
        time.sleep(3.0)
        send(port, "engine.loadSave('foraging_v66_check'); return 'ok'")
        time.sleep(15.0)
        send(port, "world.show('main_world'); return 'ok'")
        # A loaded world comes up PAUSED (auto-pause-on-save) with only
        # the center chunk generated — resume the clock and pull in the
        # region around the harvested tile so getFloraAt can see it.
        send(port, "engine.setPaused(false); return 'ok'")
        send(port, "return world.loadChunksInRegion(-4, -4, 4, 4)", timeout=30)
        send(port, "return world.waitForChunks(120)", timeout=125)
        fl4 = jget(port, f"return world.getFloraAt({gx},{gy})")
        ok4 = isinstance(fl4, dict) and fl4.get("harvestable") is False \
              and fl4.get("regrowthRemaining", 0) > 0
        passed &= ok4
        print(f"  [{'PASS' if ok4 else 'FAIL'}] harvest timer survives save/load: {fl4}")

        # --- 5. Autonomous foraging (real AI stack) ---
        send(port, "engine.loadScript('scripts/unit_stats.lua', 0.1); return 'ok'")
        send(port, "engine.loadScript('scripts/unit_resources.lua', 0.2); return 'ok'")
        send(port, "engine.loadScript('scripts/unit_ai.lua', 0.1); return 'ok'")
        # A fresh target (the save-test tile is regrowing): find one on
        # the RELOADED world and spawn a hungry acolyte two tiles away.
        found2 = find_harvestable(port)
        if not found2:
            print("  [FAIL] no harvestable flora on the reloaded world")
            return 1
        fgx, fgy, fspecies = found2
        uid_s = send(port, f"local u=unit.spawn('acolyte',{fgx + 2},{fgy}); return u")
        uid = int(float(uid_s.strip('"')))
        if uid < 0:
            print("  [FAIL] could not spawn forager")
            return 1
        time.sleep(2.0)
        # Hungry, carrying nothing edible: strip rations, empty the
        # stomach, halve the store (need ≈ 0.6 → forage well above
        # wander; nothing else competes on a fresh spawn).
        send(port, f"local u={uid}; unit.removeItem(u,'rations'); "
                   f"unit.removeItem(u,'rations'); "
                   f"unit.setStat(u,'hunger',0); "
                   f"unit.setStat(u,'calories',unit.getStat(u,'max_calories')*0.5); "
                   f"return 'ok'")
        deadline = time.time() + 45.0
        foraged = eaten = False
        while time.time() < deadline:
            time.sleep(2.0)
            fl5 = jget(port, f"return world.getFloraAt({fgx},{fgy})")
            if isinstance(fl5, dict) and not fl5.get("harvestable"):
                foraged = True
            st = float(send(port, f"return unit.getStat({uid},'hunger') or -1"))
            inv_food = send(port,
                f"local inv=unit.getInventory({uid}) or {{}}; "
                f"for _,it in ipairs(inv) do if it.food then return 'yes' end end; "
                f"return 'no'").strip('"')
            if st > 10 or inv_food == "yes":
                eaten = True
            if foraged and eaten:
                break
        ok5 = foraged and eaten
        passed &= ok5
        print(f"  [{'PASS' if ok5 else 'FAIL'}] hungry acolyte forages autonomously: "
              f"tile_harvested={foraged} food_acquired={eaten}")

        print("\n" + ("ALL FORAGING CHECKS PASSED" if passed else "SOME FAILED"))
        return 0 if passed else 1
    finally:
        quit_engine(port, proc)


if __name__ == "__main__":
    sys.exit(main())
