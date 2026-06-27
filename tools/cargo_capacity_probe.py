#!/usr/bin/env python3
"""Headless probe for issue #189: depositToCargo capacity check must weigh
the actual ItemInstance (fill + nested contents), not the def base weight.

Setup: flat arena, one unit, one cargo_hold_S (capacity 200 kg). Repeatedly
give the unit a FULL 2 L steel canteen (real instance weight ~2.2 kg, def base
weight ~0.2 kg) and deposit it. The pre-check that uses base weight (the bug)
keeps accepting deposits until real stored weight overshoots capacity; a
pre-check that uses the real instance weight (the fix) stops at <= capacity.

PASS  = final stored weight <= capacity (fix).
FAIL  = final stored weight  > capacity (bug overfilled the cargo).
"""
from __future__ import annotations
import glob, json, socket, subprocess, sys, time

PORT = 9009
LOG = "/tmp/cargo_capacity_probe_engine.log"


def send(lua: str, timeout: float = 10.0) -> str:
    with socket.create_connection(("localhost", PORT), timeout=timeout) as s:
        s.sendall((lua + "\n").encode())
        chunks, = ([],)
        s.settimeout(0.4)
        try:
            while True:
                b = s.recv(4096)
                if not b:
                    break
                chunks.append(b)
        except socket.timeout:
            pass
    out = b"".join(chunks).decode(errors="replace")
    res = [ln[2:].strip() for ln in out.splitlines() if ln.startswith("> ")]
    res = [r for r in res if r]
    return res[-1] if res else out.strip()


def num(lua: str):
    r = send(lua)
    try:
        return float(r)
    except (TypeError, ValueError):
        return None


def boot() -> subprocess.Popen:
    log = open(LOG, "w")
    proc = subprocess.Popen(
        ["cabal", "run", "-v0", "exe:synarchy", "--",
         "--headless", "--port", str(PORT)],
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
        ("data/buildings/*.yaml",  "engine.loadBuildingYaml"),
    ]
    for pattern, fn in loaders:
        for path in sorted(glob.glob(pattern)):
            send(f"{fn}('{path}'); return 'ok'")
    send("pcall(function() require('scripts.unit_ai').update=function() end end); "
         "return 'ai-off'")
    send("require('scripts.movement_arena'); return 'ok'")


def main() -> int:
    proc = boot()
    try:
        bootstrap()
        course = json.loads(send(
            "return require('scripts.movement_arena').buildCourse('flat')"))
        sx, sy = int(course["sx"]), int(course["sy"])
        # wait for the arena centre chunk to load
        for _ in range(40):
            r = send("return world.getChunkInfo(0,0)")
            try:
                if json.loads(r).get("loaded"):
                    break
            except Exception:
                pass
            time.sleep(0.25)

        uid = int(num(f"return unit.spawn('acolyte', {sx}, {sy})"))
        bid = num(f"return building.spawn('cargo_hold_S', {sx+2}, {sy})")
        if bid is None:
            print("FAIL: could not spawn cargo_hold_S")
            return 2
        bid = int(bid)
        # wait for the queued BuildingSpawn to materialise
        cap = None
        for _ in range(40):
            cap = num(f"return building.getStorageCapacity({bid})")
            if cap and cap > 0:
                break
            time.sleep(0.25)
        if not cap:
            print("FAIL: cargo building never became queryable")
            return 2

        DEF = "canteen_steel_2l"
        accepted = 0
        for _ in range(200):
            send(f"unit.addItem({uid}, '{DEF}', 2); return 'ok'")  # full 2 L
            ok = send(f"return unit.depositToCargo({uid}, {bid}, '{DEF}')")
            if ok.lower() == "true":
                accepted += 1
            else:
                # remove the rejected canteen so it can't accumulate weight
                send(f"unit.removeItem({uid}, '{DEF}'); return 'ok'")
                break

        stored = num(f"return building.getStorageWeight({bid})")
        print(f"capacity      = {cap:.2f} kg")
        print(f"canteens kept = {accepted}  (each full = ~2.2 kg real / ~0.2 kg base)")
        print(f"stored weight = {stored:.2f} kg")

        overfilled = stored is not None and stored > cap + 1e-3
        if overfilled:
            print(f"\nFAIL: cargo overfilled — stored {stored:.2f} > capacity {cap:.2f}")
            print("      (base-weight pre-check let heavy instances slip past)")
            return 1
        print(f"\nPASS: stored {stored:.2f} kg <= capacity {cap:.2f} kg "
              "(instance weight respected)")
        return 0
    finally:
        try:
            send("engine.quit(); return 'bye'", timeout=3)
        except Exception:
            pass
        time.sleep(1)
        if proc.poll() is None:
            proc.kill()


if __name__ == "__main__":
    sys.exit(main())
