#!/usr/bin/env python3
"""Tree-felling / chop-designation probe (#97).

Boots a headless engine on a real generated world (tree placement needs
worldgen; the arena has no flora), then checks:

  1. API: world.findHarvestableFlora(..., 'wood') locates a tree;
     world.getFloraAt reports it with a 'wood' tag and no live
     regrowth timer (its 'harvestable' flag is the FORAGE signal,
     gated on the #332 growth window — the chop path keys on
     tags + regrowthRemaining instead); a BARE findHarvestableFlora
     (the foraging AI's food search) does NOT return the tree tile —
     wood yields are inedible, so the #97 tag split must keep hungry
     units from felling oaks for dinner.
  2. Designation: chop.designate commits a rectangle down to the tree
     tiles (chop.getDesignationAt / getDesignationCount);
     chop.cancelDesignation removes one.
  3. Save/load: designations survive save → loadSave with their z
     (WorldPageSave wpsChopDesignations, v67).
  4. AI: an acolyte with an axe autonomously claims the designated
     tree (real unit_ai stack), walks over, chops it, wood_log ground
     items appear, the designation clears, and the tile flips to
     regrowing (long timer).
  5. Construction: the chopped logs satisfy a build job — a dungeon_1
     POST (materials: wood_log, #96 build: block) designated beside
     the stump gets built by the same acolyte, consuming a log off
     the ground (the #97 acceptance that wood is a usable build
     material). Posts need a floor under them (findConstructJob's
     floorZAt guard), so a steel floor goes down first.

Usage: python3 tools/chop_probe.py [--port 9177] [--seed 42]
       [--size 64] [--plates 3]
"""
import argparse, glob, json, socket, subprocess, sys, time
from probelib import boot, send

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


def find_wood(port, span=4):
    """Scan sample points across the loaded region for the nearest
    wood-tagged harvestable tile; returns (gx, gy, species) or None."""
    for sx in range(-span * 16, span * 16 + 1, 32):
        for sy in range(-span * 16, span * 16 + 1, 32):
            r = jget(port,
                     f"return world.findHarvestableFlora({sx},{sy},64,'wood')")
            if isinstance(r, dict):
                return r["gx"], r["gy"], r["id"]
    return None


def count_logs_near(port, gx, gy, radius=4):
    ground = jget(port, "return item.listGround()")
    if not isinstance(ground, list):
        return 0
    return sum(1 for g in ground
               if g.get("defName") == "wood_log"
               and abs(g.get("x", 1e9) - gx) <= radius
               and abs(g.get("y", 1e9) - gy) <= radius)


def main():
    ap = argparse.ArgumentParser()
    ap.add_argument("--port", type=int, default=9177)
    ap.add_argument("--seed", type=int, default=42)
    ap.add_argument("--size", type=int, default=64)
    ap.add_argument("--plates", type=int, default=3)
    args = ap.parse_args()
    port = args.port
    passed = True

    proc = boot(port, f"{SPROOT}/chop_probe_engine.log")
    try:
        bootstrap(port)
        send(port, f"world.init('probe', {args.seed}, {args.size}, "
                   f"{args.plates}); return 'ok'")
        send(port, "return world.waitForInit(300)", timeout=310)
        send(port, "world.show('probe'); return 'ok'")
        send(port, "return world.loadChunksInRegion(-4, -4, 4, 4)", timeout=30)
        send(port, "return world.waitForChunks(120)", timeout=125)

        # --- 1. Find a tree + tag reporting + forage-tag guard ---
        found = find_wood(port)
        if not found:
            print("  [FAIL] no wood-harvestable flora in the loaded region "
                  "(seed/climate has no oak/birch/maple here — try another "
                  "seed)")
            return 1
        tx, ty, species = found
        fl = jget(port, f"return world.getFloraAt({tx},{ty})")
        ok1 = isinstance(fl, dict) and "wood" in (fl.get("tags") or []) \
              and fl.get("regrowthRemaining", -1) == 0
        passed &= ok1
        print(f"  [{'PASS' if ok1 else 'FAIL'}] getFloraAt reports a "
              f"choppable tree (wood tag, no regrowth timer): "
              f"{species} at ({tx},{ty}) → {fl}")

        bare = jget(port,
                    f"return world.findHarvestableFlora({tx},{ty},2)")
        ok1b = not (isinstance(bare, dict)
                    and bare.get("gx") == tx and bare.get("gy") == ty)
        passed &= ok1b
        print(f"  [{'PASS' if ok1b else 'FAIL'}] bare (food) search skips "
              f"the tree tile: {bare}")

        # --- 2. Designate / query / cancel ---
        send(port, f"chop.designate('probe',{tx-1},{ty-1},{tx+1},{ty+1}); "
                   f"return 'ok'")
        time.sleep(0.5)
        n = jget(port, "return chop.getDesignationCount('probe')")
        d = jget(port, f"return chop.getDesignationAt('probe',{tx},{ty})")
        ok2 = isinstance(n, (int, float)) and n >= 1 \
              and isinstance(d, dict) and isinstance(d.get("z"), (int, float))
        passed &= ok2
        print(f"  [{'PASS' if ok2 else 'FAIL'}] designate marks the tree: "
              f"count={n} at-tile={d}")

        send(port, f"chop.cancelDesignation({tx},{ty}); return 'ok'")
        time.sleep(0.5)
        d2 = jget(port, f"return chop.getDesignationAt('probe',{tx},{ty})")
        ok2b = not isinstance(d2, dict)
        passed &= ok2b
        print(f"  [{'PASS' if ok2b else 'FAIL'}] cancelDesignation clears "
              f"it: {d2}")

        # Re-designate for the save + AI phases.
        send(port, f"chop.designate('probe',{tx},{ty},{tx},{ty}); "
                   f"return 'ok'")
        time.sleep(0.5)

        # --- 3. Save/load round-trip ---
        send(port, "engine.saveWorld('probe', 'chop_v67_check'); "
                   "return 'ok'")
        time.sleep(3.0)
        send(port, "engine.loadSave('chop_v67_check'); return 'ok'")
        time.sleep(15.0)
        send(port, "world.show('main_world'); return 'ok'")
        send(port, "engine.setPaused(false); return 'ok'")
        send(port, "return world.loadChunksInRegion(-4, -4, 4, 4)", timeout=30)
        send(port, "return world.waitForChunks(120)", timeout=125)
        d3 = jget(port, f"return chop.getDesignationAt('main_world',{tx},{ty})")
        ok3 = isinstance(d3, dict) and isinstance(d3.get("z"), (int, float))
        passed &= ok3
        print(f"  [{'PASS' if ok3 else 'FAIL'}] designation survives "
              f"save/load: {d3}")

        # --- 4. Autonomous felling (real AI stack) ---
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
            print(f"  [FAIL] could not spawn woodcutter: {uid_s}")
            return 1
        time.sleep(2.0)
        # Acolytes spawn at/over carrying capacity with a full toolkit
        # (pick, shovel, axe, radio, rations, canteen) plus worn kit
        # that unit.getCarryingWeight counts too. Shed everything but
        # the axe so hauling a wood_log for the phase-5 build job can't
        # brush carrying_capacity — the capacity STAT is a per-spawn
        # roll (observed 17.9–24.2 kg), and the fetch capacity gate
        # refusing the log reads as a stuck job (same shedding the
        # construction probe does).
        for it in ("pick_steel", "shovel_steel", "rations", "rations",
                   "canteen_steel_2l", "radio"):
            send(port, f"unit.removeItem({uid},'{it}'); return 'ok'")
        # carrying_capacity is a per-spawn body-composition roll
        # (observed 12.3–24.2 kg); on a weak roll even a bare unit in
        # worn kit can't shoulder an 8 kg log and the phase-5 haul
        # stalls at the fetch capacity gate. Pin the fixture: bulk the
        # woodcutter up and recompute the derived stats — the probe
        # tests chopping/hauling/building, not the physique lottery.
        cap = send(port,
                   f"local lean=unit.getStat({uid},'lean_mass'); "
                   f"local body=unit.getStat({uid},'body_mass'); "
                   f"if lean and body then "
                   f"unit.setStat({uid},'lean_mass', lean*1.6); "
                   f"unit.setStat({uid},'body_mass', body+lean*0.6); "
                   f"unit.recomputeBody({uid}) end; "
                   f"return unit.getStat({uid},'carrying_capacity') or -1")
        try:
            cap_ok = float(cap) >= 18.0
        except ValueError:
            cap_ok = False
        if not cap_ok:
            print(f"  [FAIL] could not pin carrying capacity "
                  f"(got {cap})")
            passed = False
        has_axe = send(port,
                       f"for _,it in ipairs(unit.getInventory({uid}) or {{}})"
                       f" do if it.defName=='axe_steel' then return 'yes' end"
                       f" end; return 'no'").strip('"')
        print(f"  [{'PASS' if has_axe == 'yes' else 'FAIL'}] woodcutter "
              f"carries an axe: {has_axe}")
        passed &= has_axe == "yes"
        # Fresh acolytes spawn with the standing "find_water" goal
        # (DEFAULT_GOALS), whose search floor (~3.0) outranks menial
        # work (#306 bands) — and the water-search spiral happily walks
        # scouts off cliffs on unlucky seeds. The probe tests CHOPPING,
        # not hydration scouting, so mark the goal accomplished through
        # the canonical goal API before the spiral leads it anywhere.
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
        felled = logs = regrowing = False
        while time.time() < deadline:
            time.sleep(2.0)
            d4 = jget(port,
                      f"return chop.getDesignationAt('main_world',{tx},{ty})")
            if not isinstance(d4, dict):
                felled = True
            if count_logs_near(port, tx, ty) >= 1:
                logs = True
            fl4 = jget(port, f"return world.getFloraAt({tx},{ty})")
            if isinstance(fl4, dict) and not fl4.get("harvestable") \
               and fl4.get("regrowthRemaining", 0) > 0:
                regrowing = True
            if felled and logs and regrowing:
                break
        ok4 = felled and logs and regrowing
        passed &= ok4
        print(f"  [{'PASS' if ok4 else 'FAIL'}] acolyte fells the tree "
              f"autonomously: designation_cleared={felled} "
              f"logs_on_ground={logs} tile_regrowing={regrowing}")
        if not ok4:
            print("\nSOME FAILED")
            return 1

        # --- 5. Chopped logs satisfy construction (#96 wiring) ---
        # A dungeon_1 POST costs 1 wood_log (its build: block). Find a
        # flat, dry, flora-free tile near the stump; the post needs a
        # floor under it (findConstructJob skips floorless posts), so
        # the acolyte first lays a steel floor from inventory, then
        # builds the post from a felled log it hauls off the ground.
        spot = send(port,
                    f"for r=2,5 do for dx=-r,r do for dy=-r,r do "
                    f"local x,y={tx}+dx,{ty}+dy; "
                    f"if world.getSlopeAt(x,y)==0 and not world.getFluidAt(x,y)"
                    f" and not world.getFloraAt(x,y)"
                    f" and not chop.getDesignationAt('main_world',x,y) then "
                    f"return x..','..y end end end end; return 'none'"
                    ).strip('"')
        if spot == "none":
            print("  [FAIL] no flat buildable tile near the stump")
            print("\nSOME FAILED")
            return 1
        px, py = (int(v) for v in spot.split(","))
        send(port, f"unit.addItem({uid},'steel_plate',0); return 'ok'")
        send(port, f"construction.designate('main_world',{px},{py},{px},{py},"
                   f"'structure','dungeon_1','floor'); return 'ok'")
        deadline = time.time() + 90.0
        floored = False
        while time.time() < deadline:
            time.sleep(2.0)
            if send(port,
                    f"return structure.hasAt({px},{py},'floor')") == "true":
                floored = True
                break
        if not floored:
            print(f"  [FAIL] prerequisite floor never built at ({px},{py})")
            print("\nSOME FAILED")
            return 1

        logs_before = count_logs_near(port, tx, ty, radius=8)
        send(port, f"construction.designate('main_world',{px},{py},{px},{py},"
                   f"'structure','dungeon_1','post'); return 'ok'")
        time.sleep(0.5)
        deadline = time.time() + 120.0
        built = cleared = False
        while time.time() < deadline:
            time.sleep(2.0)
            # The AI-placed post defaults to corner "n" and is stored
            # under the composite kind (like wall_ne).
            built = send(port, f"return structure.hasAt({px},{py},"
                               f"'post_n')") == "true"
            cleared = not isinstance(
                jget(port, f"return construction.getDesignationAt("
                           f"'main_world',{px},{py})"), dict)
            if built and cleared:
                break
        logs_after = count_logs_near(port, tx, ty, radius=8)
        consumed = logs_after < logs_before
        ok5 = built and cleared and consumed
        passed &= ok5
        print(f"  [{'PASS' if ok5 else 'FAIL'}] post built from chopped "
              f"logs at ({px},{py}): built={built} designation_cleared="
              f"{cleared} log_consumed={consumed} "
              f"(ground logs {logs_before}→{logs_after})")

        print("\n" + ("ALL CHOP CHECKS PASSED" if passed else "SOME FAILED"))
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
