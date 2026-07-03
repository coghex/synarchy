#!/usr/bin/env python3
"""Derived unit-role probe (#265) — the gate for scripts/unit_roles.lua.

Boots a headless engine on a real generated world (the steering phase
needs a choppable tree, and the arena has no flora), then checks:

  1. Derivation: a fresh acolyte derives SOME role within a few thought
     ticks (spawn skill rolls land on laborer or a natural specialist);
     forcing mining high derives "miner"; a challenger inside the
     SWITCH_MARGIN does NOT flip the role (hysteresis) while one past
     the margin does; dropping every work skill below the threshold
     demotes to "laborer"; a technomule (no work skills) derives no
     role at all.
  2. Steering: with a chop designation (real tree) and a mine
     designation live at comparable distance, a woodcutting-skilled
     unit picks chop_designation as its first work action and a
     mining-skilled unit picks dig_designation — the ON_ROLE/OFF_ROLE
     entry weights (1.4 / 0.7) beat the geometry, and the two runs
     share the same geometry so a distance bias can't fake a pass.
  3. XP: felling the tree grows the feller's woodcutting skill
     (chop_xp_per_fell through grantWorkXP), and completing a structure
     floor grows construction (construct_xp_per_piece) — including the
     lazy seeding path for units that lack the key.

Usage: python3 tools/role_probe.py [--port 9265] [--seed 42]
       [--size 64] [--plates 3]
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


def find_wood(port, span=4):
    """Nearest wood-tagged harvestable tile; (gx, gy, species) or None."""
    for sx in range(-span * 16, span * 16 + 1, 32):
        for sy in range(-span * 16, span * 16 + 1, 32):
            r = jget(port,
                     f"return world.findHarvestableFlora({sx},{sy},64,'wood')")
            if isinstance(r, dict):
                return r["gx"], r["gy"], r["id"]
    return None


def spawn_worker(port, x, y):
    """Spawn an acolyte, quiet its find_water goal (the water-search
    spiral outranks menial work and walks scouts off cliffs), and
    return its uid (or -1)."""
    uid_s = send(port, f"local u=unit.spawn('acolyte',{x},{y}); return u")
    try:
        uid = int(float(uid_s.strip('"')))
    except ValueError:
        return -1
    time.sleep(2.0)
    quiet = send(port,
                 f"local ai=require('scripts.unit_ai'); "
                 f"local s=ai.getState({uid}); "
                 f"if s then ai.markGoalAccomplished(s,'find_water'); "
                 f"unit.stop({uid}); return 'ok' "
                 f"else return 'nostate' end").strip('"')
    return uid if quiet == "ok" else -1


def set_work_skills(port, uid, mining, woodcutting, construction, smithing):
    send(port,
         f"unit.setSkill({uid},'mining',{mining}); "
         f"unit.setSkill({uid},'woodcutting',{woodcutting}); "
         f"unit.setSkill({uid},'construction',{construction}); "
         f"unit.setSkill({uid},'smithing',{smithing}); return 'ok'")


def get_role(port, uid):
    return send(port,
                f"local ai=require('scripts.unit_ai'); "
                f"return ai.getRole({uid}) or 'none'").strip('"')


def wait_role(port, uid, want, seconds=8.0):
    """Poll until the derived role equals `want` (a couple of thought
    ticks); returns the final observed role."""
    deadline = time.time() + seconds
    role = get_role(port, uid)
    while time.time() < deadline:
        if role == want:
            return role
        time.sleep(1.0)
        role = get_role(port, uid)
    return role


def first_work_action(port, uid, seconds=45.0):
    """Poll currentAction until it becomes a designation work action."""
    deadline = time.time() + seconds
    while time.time() < deadline:
        act = send(port,
                   f"local ai=require('scripts.unit_ai'); "
                   f"local s=ai.getState({uid}); "
                   f"return s and s.currentAction or 'none'").strip('"')
        if act in ("chop_designation", "dig_designation"):
            return act
        time.sleep(1.0)
    return "timeout"


def get_skill(port, uid, name):
    v = jget(port, f"return unit.getSkill({uid},'{name}') or -1")
    return float(v) if isinstance(v, (int, float)) else -1.0


def main():
    ap = argparse.ArgumentParser()
    ap.add_argument("--port", type=int, default=9265)
    ap.add_argument("--seed", type=int, default=42)
    ap.add_argument("--size", type=int, default=64)
    ap.add_argument("--plates", type=int, default=3)
    args = ap.parse_args()
    port = args.port
    passed = True

    proc = boot(port, f"{SPROOT}/role_probe_engine.log")
    try:
        bootstrap(port)
        send(port, f"world.init('probe', {args.seed}, {args.size}, "
                   f"{args.plates}); return 'ok'")
        send(port, "return world.waitForInit(300)", timeout=310)
        send(port, "world.show('probe'); return 'ok'")
        send(port, "return world.loadChunksInRegion(-4, -4, 4, 4)", timeout=30)
        send(port, "return world.waitForChunks(120)", timeout=125)
        send(port, "engine.loadScript('scripts/unit_stats.lua', 0.1); "
                   "return 'ok'")
        send(port, "engine.loadScript('scripts/unit_resources.lua', 0.2); "
                   "return 'ok'")
        send(port, "engine.loadScript('scripts/unit_ai.lua', 0.1); "
                   "return 'ok'")

        found = find_wood(port)
        if not found:
            print("  [FAIL] no wood-harvestable flora in the loaded region "
                  "(try another seed)")
            return 1
        tx, ty, species = found

        # --- 1. Derivation + hysteresis (unit a, near the tree) ---
        ua = spawn_worker(port, tx + 3, ty)
        if ua < 0:
            print("  [FAIL] could not spawn derivation unit")
            return 1

        deadline = time.time() + 8.0
        role0 = get_role(port, ua)
        while role0 == "none" and time.time() < deadline:
            time.sleep(1.0)
            role0 = get_role(port, ua)
        ok = role0 != "none"
        passed &= ok
        print(f"  [{'PASS' if ok else 'FAIL'}] fresh acolyte derives a "
              f"role from spawn skill rolls: {role0}")

        set_work_skills(port, ua, 60, 10, 10, 10)
        r = wait_role(port, ua, "miner")
        passed &= r == "miner"
        print(f"  [{'PASS' if r == 'miner' else 'FAIL'}] mining 60 → miner: "
              f"{r}")

        # Challenger inside the switch margin (63 < 60 + 5): stays miner.
        send(port, f"unit.setSkill({ua},'woodcutting',63); return 'ok'")
        time.sleep(4.0)
        r = get_role(port, ua)
        passed &= r == "miner"
        print(f"  [{'PASS' if r == 'miner' else 'FAIL'}] woodcutting 63 vs "
              f"mining 60 stays miner (hysteresis): {r}")

        # Challenger past the margin (70 ≥ 65): flips.
        send(port, f"unit.setSkill({ua},'woodcutting',70); return 'ok'")
        r = wait_role(port, ua, "woodcutter")
        passed &= r == "woodcutter"
        print(f"  [{'PASS' if r == 'woodcutter' else 'FAIL'}] woodcutting 70 "
              f"→ woodcutter: {r}")

        # Everything below threshold: generalist.
        set_work_skills(port, ua, 10, 10, 10, 10)
        r = wait_role(port, ua, "laborer")
        passed &= r == "laborer"
        print(f"  [{'PASS' if r == 'laborer' else 'FAIL'}] all skills 10 → "
              f"laborer: {r}")

        send(port, f"unit.destroy({ua}); return 'ok'")

        # Technomule: balance/climbing only — no work skills, no role.
        um_s = send(port, f"local u=unit.spawn('technomule',{tx + 4},{ty}); "
                          f"return u")
        try:
            um = int(float(um_s.strip('"')))
        except ValueError:
            um = -1
        time.sleep(4.0)
        r = get_role(port, um) if um >= 0 else "spawnfail"
        passed &= r == "none"
        print(f"  [{'PASS' if r == 'none' else 'FAIL'}] technomule has no "
              f"role: {r}")
        if um >= 0:
            send(port, f"unit.destroy({um}); return 'ok'")

        # --- 2. Steering: same geometry, opposite roles ---
        # Chop designation on the tree; mine designation on a flat dry
        # tile nearby. Workers spawn between the two.
        spot = send(port,
                    f"for r=2,5 do for dx=-r,r do for dy=-r,r do "
                    f"local x,y={tx}+dx,{ty}+dy; "
                    f"if world.getSlopeAt(x,y)==0 and not world.getFluidAt(x,y)"
                    f" and not world.getFloraAt(x,y) then "
                    f"return x..','..y end end end end; return 'none'"
                    ).strip('"')
        if spot == "none":
            print("  [FAIL] no flat diggable tile near the tree")
            return 1
        dx, dy = (int(v) for v in spot.split(","))
        send(port, f"chop.designate('probe',{tx},{ty},{tx},{ty}); "
                   f"return 'ok'")
        send(port, f"world.designateMine('probe',{dx},{dy},{dx},{dy}); "
                   f"return 'ok'")
        time.sleep(0.5)
        mx, my = (tx + dx) // 2, (ty + dy) // 2

        picks = {}
        for label, skills in [("woodcutter", (10, 60, 10, 10)),
                              ("miner",      (60, 10, 10, 10))]:
            u = spawn_worker(port, mx + 1, my + 1)
            if u < 0:
                print(f"  [FAIL] could not spawn {label} steering unit")
                return 1
            set_work_skills(port, u, *skills)
            picks[label] = first_work_action(port, u)
            send(port, f"unit.destroy({u}); return 'ok'")
            time.sleep(1.0)

        ok = picks["woodcutter"] == "chop_designation"
        passed &= ok
        print(f"  [{'PASS' if ok else 'FAIL'}] woodcutter picks the chop "
              f"job first: {picks['woodcutter']}")
        ok = picks["miner"] == "dig_designation"
        passed &= ok
        print(f"  [{'PASS' if ok else 'FAIL'}] miner picks the dig job "
              f"first (same geometry): {picks['miner']}")

        # --- 3. XP growth (unit c fells the tree, then lays a floor) ---
        uc = spawn_worker(port, tx + 2, ty)
        if uc < 0:
            print("  [FAIL] could not spawn XP unit")
            return 1
        set_work_skills(port, uc, 5, 55, 40, 10)
        wc_before = get_skill(port, uc, "woodcutting")

        deadline = time.time() + 90.0
        felled = False
        while time.time() < deadline:
            time.sleep(2.0)
            d = jget(port, f"return chop.getDesignationAt('probe',{tx},{ty})")
            if not isinstance(d, dict):
                felled = True
                break
        wc_after = get_skill(port, uc, "woodcutting")
        ok = felled and wc_after > wc_before
        passed &= ok
        print(f"  [{'PASS' if ok else 'FAIL'}] felling grants woodcutting "
              f"XP: felled={felled} {wc_before} → {wc_after}")

        # Floor build: material from inventory, then construction XP.
        # Re-skill the feller into a builder first (construction 60 →
        # derives "builder", construct job on-role) so the still-live
        # dig designation can't outbid the floor.
        set_work_skills(port, uc, 5, 10, 60, 10)
        con_before = get_skill(port, uc, "construction")
        fspot = send(port,
                     f"for r=2,5 do for fx=-r,r do for fy=-r,r do "
                     f"local x,y={tx}+fx,{ty}+fy; "
                     f"if world.getSlopeAt(x,y)==0 and not world.getFluidAt(x,y)"
                     f" and not world.getFloraAt(x,y)"
                     f" and not world.getMineDesignationAt('probe',x,y) then "
                     f"return x..','..y end end end end; return 'none'"
                     ).strip('"')
        if fspot == "none":
            print("  [FAIL] no flat buildable tile for the floor")
            return 1
        px, py = (int(v) for v in fspot.split(","))
        send(port, f"unit.addItem({uc},'steel_plate',0); return 'ok'")
        send(port, f"construction.designate('probe',{px},{py},{px},{py},"
                   f"'structure','dungeon_1','floor'); return 'ok'")
        deadline = time.time() + 90.0
        floored = False
        while time.time() < deadline:
            time.sleep(2.0)
            if send(port,
                    f"return structure.hasAt({px},{py},'floor')") == "true":
                floored = True
                break
        con_after = get_skill(port, uc, "construction")
        ok = floored and con_after > con_before
        passed &= ok
        print(f"  [{'PASS' if ok else 'FAIL'}] placing a floor grants "
              f"construction XP: built={floored} {con_before} → {con_after}")

        print("\n" + ("ALL ROLE CHECKS PASSED" if passed else "SOME FAILED"))
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
