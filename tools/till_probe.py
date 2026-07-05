#!/usr/bin/env python3
"""Tilling / till-designation probe (#333).

Boots a headless engine on a real generated world (the arena has no
natural ground cover to exercise the tillable-tile filter against), then
checks:

  1. Designation: till.designate commits a rectangle down to tillable
     tiles at the anchor's surface z (till.getDesignationAt /
     getDesignationCount); till.cancelDesignation removes one.
  2. Untillable exclusion: a tile under fluid is never designated even
     when swept in the same rectangle.
  3. Save/load: designations survive save → loadSave with their z
     (WorldPageSave wpsTillDesignations, v76).
  4. AI: an acolyte (real unit_ai stack, no tool required) autonomously
     claims the designated tile, walks over, tills it, the tile's
     vegetation id flips to the tilled-soil id (world.getVegAt) and the
     designation clears.
  5. Idempotent re-sweep: designating the same rectangle again after
     tilling does not re-mark the now-tilled tile.

Usage: python3 tools/till_probe.py [--port 9178] [--seed 42]
       [--size 64] [--plates 3]
"""
import argparse, glob, json, socket, subprocess, sys, time

SPROOT = "/tmp"
TILLED_SOIL_VEG_ID = 77


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
        try:
            if "READY" in open(log_path).read():
                return proc
        except FileNotFoundError:
            pass
    sys.exit("engine never printed READY")


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


def find_tillable(port, span=4):
    """Scan sample points across the loaded region for a flat, dry,
    flora-free tile; returns (gx, gy) or None."""
    for sx in range(-span * 16, span * 16 + 1, 4):
        for sy in range(-span * 16, span * 16 + 1, 4):
            slope = jget(port, f"return world.getSlopeAt({sx},{sy})")
            if slope != 0:
                continue
            fluid = jget(port, f"return world.getFluidAt({sx},{sy})")
            if isinstance(fluid, dict) and fluid.get("type"):
                continue
            flora = jget(port, f"return world.getFloraAt({sx},{sy})")
            if isinstance(flora, dict):
                continue
            return sx, sy
    return None


def find_fluid_tile(port, span=4):
    """Scan for a fluid-covered tile, for the untillable-exclusion check."""
    for sx in range(-span * 16, span * 16 + 1, 4):
        for sy in range(-span * 16, span * 16 + 1, 4):
            fluid = jget(port, f"return world.getFluidAt({sx},{sy})")
            if isinstance(fluid, dict) and fluid.get("type"):
                return sx, sy
    return None


def main():
    ap = argparse.ArgumentParser()
    ap.add_argument("--port", type=int, default=9178)
    ap.add_argument("--seed", type=int, default=42)
    ap.add_argument("--size", type=int, default=64)
    ap.add_argument("--plates", type=int, default=3)
    args = ap.parse_args()
    port = args.port
    passed = True

    proc = boot(port, f"{SPROOT}/till_probe_engine.log")
    try:
        bootstrap(port)
        send(port, f"world.init('probe', {args.seed}, {args.size}, "
                   f"{args.plates}); return 'ok'")
        send(port, "return world.waitForInit(300)", timeout=310)
        send(port, "world.show('probe'); return 'ok'")
        send(port, "return world.loadChunksInRegion(-4, -4, 4, 4)", timeout=30)
        send(port, "return world.waitForChunks(120)", timeout=125)

        # --- 1. Find a tillable tile + designate / query / cancel ---
        found = find_tillable(port)
        if not found:
            print("  [FAIL] no flat, dry, flora-free tile in the loaded "
                  "region (try another seed)")
            return 1
        tx, ty = found
        print(f"  tillable tile at ({tx},{ty})")

        send(port, f"till.designate('probe',{tx},{ty},{tx},{ty}); "
                   f"return 'ok'")
        time.sleep(0.5)
        n = jget(port, "return till.getDesignationCount('probe')")
        d = jget(port, f"return till.getDesignationAt('probe',{tx},{ty})")
        ok1 = isinstance(n, (int, float)) and n >= 1 \
              and isinstance(d, dict) and isinstance(d.get("z"), (int, float))
        passed &= ok1
        print(f"  [{'PASS' if ok1 else 'FAIL'}] designate marks the tile: "
              f"count={n} at-tile={d}")

        send(port, f"till.cancelDesignation({tx},{ty}); return 'ok'")
        time.sleep(0.5)
        d2 = jget(port, f"return till.getDesignationAt('probe',{tx},{ty})")
        ok1b = not isinstance(d2, dict)
        passed &= ok1b
        print(f"  [{'PASS' if ok1b else 'FAIL'}] cancelDesignation clears "
              f"it: {d2}")

        # --- 2. Untillable exclusion: fluid tile never designated ---
        fluid_tile = find_fluid_tile(port)
        if fluid_tile:
            fx, fy = fluid_tile
            send(port, f"till.designate('probe',{fx},{fy},{fx},{fy}); "
                       f"return 'ok'")
            time.sleep(0.5)
            df = jget(port, f"return till.getDesignationAt('probe',{fx},{fy})")
            ok2 = not isinstance(df, dict)
            passed &= ok2
            print(f"  [{'PASS' if ok2 else 'FAIL'}] fluid tile excluded "
                  f"from designation ({fx},{fy}): {df}")
        else:
            print("  [SKIP] no fluid tile found in the loaded region to "
                  "test exclusion against")

        # Re-designate the tillable tile for the save + AI phases.
        send(port, f"till.designate('probe',{tx},{ty},{tx},{ty}); "
                   f"return 'ok'")
        time.sleep(0.5)

        # --- 3. Save/load round-trip ---
        send(port, "engine.saveWorld('probe', 'till_v76_check'); "
                   "return 'ok'")
        time.sleep(3.0)
        send(port, "engine.loadSave('till_v76_check'); return 'ok'")
        time.sleep(15.0)
        send(port, "world.show('main_world'); return 'ok'")
        send(port, "engine.setPaused(false); return 'ok'")
        send(port, "return world.loadChunksInRegion(-4, -4, 4, 4)", timeout=30)
        send(port, "return world.waitForChunks(120)", timeout=125)
        d3 = jget(port, f"return till.getDesignationAt('main_world',{tx},{ty})")
        ok3 = isinstance(d3, dict) and isinstance(d3.get("z"), (int, float))
        passed &= ok3
        print(f"  [{'PASS' if ok3 else 'FAIL'}] designation survives "
              f"save/load: {d3}")

        # --- 4. Autonomous tilling (real AI stack) ---
        send(port, "engine.loadScript('scripts/unit_stats.lua', 0.1); "
                   "return 'ok'")
        send(port, "engine.loadScript('scripts/unit_resources.lua', 0.2); "
                   "return 'ok'")
        send(port, "engine.loadScript('scripts/unit_ai.lua', 0.1); "
                   "return 'ok'")
        uid_s = send(port, f"local u=unit.spawn('acolyte',{tx + 2},{ty}); "
                           f"return u")
        try:
            uid = int(float(uid_s.strip('"')))
        except ValueError:
            uid = -1
        if uid < 0:
            print(f"  [FAIL] could not spawn tiller: {uid_s}")
            return 1
        time.sleep(2.0)
        # Fresh acolytes spawn with the standing "find_water" goal
        # (DEFAULT_GOALS), whose search floor outranks menial work and
        # can walk the unit off-course on unlucky seeds. The probe tests
        # TILLING, not hydration scouting — quiet it first (same
        # convention as chop_probe.py / role_probe.py).
        quiet = send(port,
                     f"local ai=require('scripts.unit_ai'); "
                     f"local s=ai.getState({uid}); "
                     f"if s then ai.markGoalAccomplished(s,'find_water'); "
                     f"unit.stop({uid}); return 'ok' "
                     f"else return 'nostate' end").strip('"')
        if quiet != "ok":
            print(f"  [FAIL] could not quiet find_water goal: {quiet}")
            passed = False

        deadline = time.time() + 90.0
        tilled = cleared = False
        while time.time() < deadline:
            time.sleep(2.0)
            d4 = jget(port,
                      f"return till.getDesignationAt('main_world',{tx},{ty})")
            if not isinstance(d4, dict):
                cleared = True
            veg = jget(port, f"return world.getVegAt({tx},{ty})")
            if veg == TILLED_SOIL_VEG_ID:
                tilled = True
            if tilled and cleared:
                break
        ok4 = tilled and cleared
        passed &= ok4
        print(f"  [{'PASS' if ok4 else 'FAIL'}] acolyte tills the tile "
              f"autonomously: veg_flipped={tilled} designation_cleared="
              f"{cleared}")
        if not ok4:
            print("\nSOME FAILED")
            return 1

        # --- 5. Idempotent re-sweep: already-tilled tile skipped ---
        send(port, f"till.designate('main_world',{tx},{ty},{tx},{ty}); "
                   f"return 'ok'")
        time.sleep(0.5)
        d5 = jget(port, f"return till.getDesignationAt('main_world',{tx},{ty})")
        ok5 = not isinstance(d5, dict)
        passed &= ok5
        print(f"  [{'PASS' if ok5 else 'FAIL'}] re-sweep skips the "
              f"already-tilled tile: {d5}")

        print("\n" + ("ALL TILL CHECKS PASSED" if passed else "SOME FAILED"))
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
