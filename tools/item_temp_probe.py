#!/usr/bin/env python3
"""Item temperature probe (#344).

Boots a headless engine on a real generated world and checks the
iiTemp / cooling-tick stack end-to-end:

  1. Ambient default: a bare-spawned ground item reads the tile's
     ambient (world.getAmbientAt) through item.getGroundTemp.
  2. Cooling: an item spawned hot ({temp=100}) cools monotonically
     toward ambient on the game clock; a cold one (-40) warms.
  3. Newtonian rate: of two items on the same tile, the one further
     from ambient closes more °C in the same interval.
  4. Pause: the pause flag freezes cooling (same gate as flora
     regrowth).
  5. Held items: unit.setItemTemp / unit.getItemTemp round-trip on a
     carried item, the tracked temp surfaces in unit.getInventory, and
     the item cools in a unit's inventory too.
  6. Save/load: a tracked temperature survives save → loadSave (v68).

Usage: python3 tools/item_temp_probe.py [--port 9177] [--seed 42]
       [--size 64] [--plates 3]
"""
import argparse, glob, json, socket, subprocess, sys, time
from probelib import boot, send

SPROOT = "/tmp"


def num(port, lua, timeout=10.0):
    raw = send(port, lua, timeout).strip('"')
    try:
        return float(raw)
    except ValueError:
        return None


def jget(port, lua, timeout=10.0):
    raw = send(port, lua, timeout)
    try:
        return json.loads(raw)
    except json.JSONDecodeError:
        return raw.strip('"')


def bootstrap(port):
    for pattern, fn in [
        ("data/substances/*.yaml", "engine.loadSubstanceYaml"),
        ("data/items/*.yaml",      "engine.loadItemYaml"),
        ("data/equipment/*.yaml",  "engine.loadEquipmentYaml"),
        ("data/materials/*.yaml",  "engine.loadMaterialYaml"),
        ("data/units/*.yaml",      "engine.loadUnitYaml"),
    ]:
        for path in sorted(glob.glob(pattern)):
            send(port, f"{fn}('{path}'); return 'ok'")


def main():
    ap = argparse.ArgumentParser()
    ap.add_argument("--port", type=int, default=9177)
    ap.add_argument("--seed", type=int, default=42)
    ap.add_argument("--size", type=int, default=64)
    ap.add_argument("--plates", type=int, default=3)
    args = ap.parse_args()
    port = args.port
    passed = True

    proc = boot(port, f"{SPROOT}/item_temp_probe_engine.log")
    try:
        bootstrap(port)
        send(port, f"world.init('probe', {args.seed}, {args.size}, "
                   f"{args.plates}); return 'ok'")
        send(port, "return world.waitForInit(300)", timeout=310)
        send(port, "world.show('probe'); return 'ok'")
        send(port, "return world.loadChunksInRegion(-2, -2, 2, 2)", timeout=30)
        send(port, "return world.waitForChunks(120)", timeout=125)

        # --- 1. Bare spawn reads the tile's ambient ---
        amb = num(port, "return world.getAmbientAt(2, 2)")
        gid_plain = int(num(port,
            "return item.spawnGround('steel_bar', 2.5, 2.5)"))
        t_plain = num(port, f"return item.getGroundTemp({gid_plain})")
        ok1 = amb is not None and t_plain is not None \
              and abs(t_plain - amb) < 0.01
        passed &= ok1
        print(f"  [{'PASS' if ok1 else 'FAIL'}] bare spawn reads ambient: "
              f"getGroundTemp={t_plain} ambient={amb}")

        # --- 2. Hot cools / cold warms on the game clock ---
        gid_hot = int(num(port,
            "return item.spawnGround('steel_bar', 2.5, 2.5, {temp=100})"))
        gid_cold = int(num(port,
            "return item.spawnGround('steel_bar', 2.5, 2.5, {temp=-40})"))
        gid_warm = int(num(port,
            "return item.spawnGround('steel_bar', 2.5, 2.5, {temp=60})"))
        t0_hot = num(port, f"return item.getGroundTemp({gid_hot})")
        t0_warm = num(port, f"return item.getGroundTemp({gid_warm})")
        # steel_bar = 0.5 kg → tau = 1800 game-sec; timeScale 10 ticks
        # 600 game-sec per real-second, so ~1 tau every 3 s of polling.
        send(port, "world.setTimeScale('probe', 10); return 'ok'")
        hot_series = [t0_hot]
        for _ in range(6):
            time.sleep(1.0)
            hot_series.append(num(port, f"return item.getGroundTemp({gid_hot})"))
        t1_cold = num(port, f"return item.getGroundTemp({gid_cold})")
        t1_warm = num(port, f"return item.getGroundTemp({gid_warm})")
        ok2 = all(b < a for a, b in zip(hot_series, hot_series[1:])) \
              and hot_series[-1] > amb
        ok2b = t1_cold > -40 and t1_cold < amb
        passed &= ok2 and ok2b
        print(f"  [{'PASS' if ok2 else 'FAIL'}] hot item cools monotonically "
              f"toward ambient: {[round(t, 1) for t in hot_series]}")
        print(f"  [{'PASS' if ok2b else 'FAIL'}] cold item warms toward "
              f"ambient: -40 → {t1_cold}")

        # --- 3. Newtonian rate: bigger ΔT closes more °C ---
        drop_hot = t0_hot - hot_series[-1]
        drop_warm = t0_warm - t1_warm
        if amb < 45:
            ok3 = drop_hot > drop_warm > 0
            passed &= ok3
            print(f"  [{'PASS' if ok3 else 'FAIL'}] hotter item sheds more "
                  f"°C in the same time: {drop_hot:.1f} vs {drop_warm:.1f}")
        else:
            print(f"  [SKIP] ambient {amb} too warm for the ΔT comparison")

        # --- 4. Pause freezes cooling ---
        send(port, "engine.setPaused(true); return 'ok'")
        p0 = num(port, f"return item.getGroundTemp({gid_hot})")
        time.sleep(2.0)
        p1 = num(port, f"return item.getGroundTemp({gid_hot})")
        ok4 = p0 is not None and p0 == p1
        passed &= ok4
        print(f"  [{'PASS' if ok4 else 'FAIL'}] pause freezes cooling: "
              f"{p0} == {p1}")
        send(port, "engine.setPaused(false); return 'ok'")

        # --- 5. Held item: set / get / getInventory / cools ---
        send(port, "world.setTimeScale('probe', 10); return 'ok'")
        uid = int(num(port, "local u=unit.spawn('acolyte', 2, 2); return u"))
        if uid < 0:
            print("  [FAIL] could not spawn unit")
            return 1
        time.sleep(1.0)
        send(port, f"unit.addItem({uid}, 'steel_bar'); return 'ok'")
        iid = int(num(port,
            f"local inv=unit.getInventory({uid}); "
            f"for _,it in ipairs(inv) do "
            f"if it.defName=='steel_bar' then return it.instanceId end end; "
            f"return -1"))
        amb_u = num(port, "return world.getAmbientAt(2, 2)")
        t_held0 = num(port, f"return unit.getItemTemp({uid}, {iid})")
        ok5a = iid > 0 and t_held0 is not None \
               and abs(t_held0 - amb_u) < 0.01
        send(port, f"unit.setItemTemp({uid}, {iid}, 90); return 'ok'")
        row_t = num(port,
            f"local inv=unit.getInventory({uid}); "
            f"for _,it in ipairs(inv) do "
            f"if it.instanceId=={iid} then return it.temp or -999 end end; "
            f"return -999")
        ok5b = row_t is not None and 80 <= row_t <= 90
        time.sleep(4.0)
        t_held1 = num(port, f"return unit.getItemTemp({uid}, {iid})")
        ok5c = t_held1 is not None and amb_u < t_held1 < 88
        passed &= ok5a and ok5b and ok5c
        print(f"  [{'PASS' if ok5a else 'FAIL'}] untracked held item reads "
              f"holder-tile ambient: {t_held0} vs {amb_u}")
        print(f"  [{'PASS' if ok5b else 'FAIL'}] setItemTemp surfaces in "
              f"getInventory row: temp={row_t}")
        print(f"  [{'PASS' if ok5c else 'FAIL'}] held item cools in "
              f"inventory: 90 → {t_held1}")

        # --- 6. Tracked temp survives save/load ---
        send(port, "engine.setPaused(true); return 'ok'")
        gid_save = int(num(port,
            "return item.spawnGround('steel_bar', 3.5, 3.5, {temp=100})"))
        pre = num(port, f"return item.getGroundTemp({gid_save})")
        send(port, "engine.saveWorld('probe', 'item_temp_v68_check'); "
                   "return 'ok'")
        time.sleep(3.0)
        send(port, "engine.loadSave('item_temp_v68_check'); return 'ok'")
        time.sleep(15.0)
        send(port, "world.show('main_world'); return 'ok'")
        post = num(port, f"return item.getGroundTemp({gid_save})")
        # Loaded worlds come up paused, so the tracked value should be
        # exactly what the pre-save (paused) read saw.
        ok6 = pre is not None and post is not None and abs(post - pre) < 0.5
        passed &= ok6
        print(f"  [{'PASS' if ok6 else 'FAIL'}] tracked temp survives "
              f"save/load: {pre} → {post}")

        print("\n" + ("ALL ITEM-TEMP CHECKS PASSED" if passed
                      else "SOME FAILED"))
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
