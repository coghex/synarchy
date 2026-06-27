#!/usr/bin/env python3
"""Headless probe for issue #193: disabled-hand auto-drop must re-fire.

A weapon held in a severed/maimed grip slot should be auto-dropped on
the injury tick. The bug: a one-shot guard dropped it exactly once, so a
weapon RE-EQUIPPED into the same still-disabled hand afterward was never
dropped again. This probe verifies BOTH the first drop and the re-drop.
"""
from __future__ import annotations
import glob, socket, subprocess, sys, time

PORT = 9193
LOG = "/tmp/disarm_probe_engine.log"


def send(lua: str, timeout: float = 10.0) -> str:
    with socket.create_connection(("localhost", PORT), timeout=timeout) as s:
        s.sendall((lua + "\n").encode())
        chunks = []
        s.settimeout(0.3)
        try:
            while True:
                b = s.recv(4096)
                if not b:
                    break
                chunks.append(b)
        except socket.timeout:
            pass
    out = b"".join(chunks).decode(errors="replace")
    results = [ln[2:].strip() for ln in out.splitlines() if ln.startswith("> ")]
    results = [r for r in results if r]
    return results[-1] if results else out.strip()


def boot() -> subprocess.Popen:
    log = open(LOG, "w")
    proc = subprocess.Popen(
        ["cabal", "run", "-v0", "exe:synarchy", "--", "--headless", "--port", str(PORT)],
        stdout=log, stderr=subprocess.STDOUT,
    )
    deadline = time.time() + 240
    while time.time() < deadline:
        try:
            if "READY" in open(LOG).read():
                return proc
        except FileNotFoundError:
            pass
        if proc.poll() is not None:
            sys.exit(f"engine exited before READY; see {LOG}")
        time.sleep(0.4)
    proc.kill()
    sys.exit("engine never printed READY")


def bootstrap():
    loaders = [
        ("data/substances/*.yaml", "engine.loadSubstanceYaml"),
        ("data/items/*.yaml",      "engine.loadItemYaml"),
        ("data/equipment/*.yaml",  "engine.loadEquipmentYaml"),
        ("data/materials/*.yaml",  "engine.loadMaterialYaml"),
        ("data/units/*.yaml",      "engine.loadUnitYaml"),
    ]
    for pattern, fn in loaders:
        for path in sorted(glob.glob(pattern)):
            send(f"{fn}('{path}'); return 'ok'")
    for script, dt in [("unit_stats", 0.1), ("unit_resources", 0.2), ("unit_ai", 0.1)]:
        send(f"engine.loadScript('scripts/{script}.lua', {dt}); return 'ok'")


def find_flat():
    lua = (
        "local function f() for gy=-8,8 do for gx=-8,8 do "
        "local z=world.getTerrainAt(gx,gy) local fl=world.getFluidAt(gx,gy) "
        "if z and not fl then return gx..','..gy..','..z end end end return 'none' end return f()"
    )
    for _ in range(8):
        res = send(lua).strip('"')
        if res and res != "none" and res.count(",") == 2:
            return tuple(int(v) for v in res.split(","))
        time.sleep(0.75)
    return None


def held_right(uid: int) -> str:
    r = send(f"local lo=equipment.getLoadout({uid}); local h=lo and lo['right_hand']; "
             f"return h and (h.defName or 'yes') or 'EMPTY'")
    return r.strip('"')


def main() -> int:
    proc = boot()
    try:
        bootstrap()
        send("world.init('arena', 42, 64, 3); return 'ok'")
        send("return world.waitForInit(180)", timeout=190)
        send("world.show('arena'); return 'ok'")
        send("return world.loadChunksInRegion(-1,-1,1,1)")
        send("return world.waitForChunks(120)", timeout=125)

        flat = find_flat()
        if not flat:
            print("FAIL: no flat dry ground", file=sys.stderr); return 2
        gx, gy, z = flat
        uid = int(float(send(f"return unit.spawn('acolyte', {gx}, {gy})")))
        print(f"spawned acolyte #{uid} at ({gx},{gy}) z={z}")
        time.sleep(1.0)

        def arm():
            send(f"unit.addItem({uid}, 'steel_dagger'); return 'ok'")
            return send(f"return equipment.equip({uid}, 'right_hand', 'steel_dagger')").strip()

        def ground_daggers():
            r = send("local n=0; for _,it in ipairs(item.listGround() or {}) do "
                     "if (it.defName or '')=='steel_dagger' then n=n+1 end end; return n")
            try:
                return int(float(r))
            except ValueError:
                return -1

        def wait_empty(timeout=8.0):
            """Poll until right_hand is empty (the weapon got dropped)."""
            deadline = time.time() + timeout
            while time.time() < deadline:
                if held_right(uid) == "EMPTY":
                    return True
                time.sleep(0.3)
            return False

        base_ground = ground_daggers()
        print(f"ground daggers (baseline): {base_ground}")

        # Phase 1: equip a dagger, sever the hand, expect the first drop.
        eq1 = arm()
        print(f"phase1: equip()={eq1}, right_hand = {held_right(uid)}")
        send(f"return unit.injure({uid}, 'r_hand', 'severed', 1.0)")
        print("phase1: severed r_hand")
        p1 = wait_empty()
        g1 = ground_daggers()
        print(f"  first drop: right_hand empty={p1}, ground daggers={g1}")
        p1 = p1 and g1 == base_ground + 1

        # Phase 2: re-equip into the SAME still-severed hand. equip() must
        # succeed (the engine doesn't block equipping a maimed hand), and
        # the next injury tick must drop it AGAIN — the #193 fix. With the
        # old one-shot guard, equip()==true but the dagger stays held and
        # the ground count never increments a second time.
        eq2 = arm()
        print(f"phase2: equip()={eq2}")
        if eq2 != "true":
            print("INCONCLUSIVE: re-equip into severed hand was rejected "
                  f"(equip returned {eq2}); can't exercise the re-drop path",
                  file=sys.stderr)
            return 3
        p2 = wait_empty()
        g2 = ground_daggers()
        print(f"  re-drop: right_hand empty={p2}, ground daggers={g2}")
        p2 = p2 and g2 == base_ground + 2

        print("\n--- result ---")
        print(f"  first drop : {'PASS' if p1 else 'FAIL'}")
        print(f"  re-drop    : {'PASS' if p2 else 'FAIL'}  (issue #193)")
        return 0 if (p1 and p2) else 1
    finally:
        try:
            send("engine.quit()", timeout=2)
        except OSError:
            pass
        time.sleep(1)
        if proc.poll() is None:
            proc.kill()


if __name__ == "__main__":
    sys.exit(main())
