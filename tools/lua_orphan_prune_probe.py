#!/usr/bin/env python3
"""Headless regression probe for #195 — Lua per-id state surviving a load.

Lua save-module blobs (unit_ai aiState, building_spawn state) are
restored BEFORE the engine load path, which can drop units/buildings
whose defs are no longer registered. The Lua side used to keep state for
those dropped ids, so a later id reuse could inherit stale AI/spawn
state. The fix has the engine emit a post-load signal (LuaSaveLoaded →
broadcast `onSaveLoaded`); the modules reconcile their per-id state
against live entities (unit.exists / building.getInfo, both GLOBAL).

This probe reproduces the leak with a REAL save/load and asserts the
post-load reconcile prunes it:

  1. spawn unit A on flat ground, let the AI tick so aiState[A] exists,
  2. destroy A  → A no longer exists, but aiState[A] lingers (the leak),
  3. spawn unit B and keep it alive (control — its state must survive),
  4. saveWorld + loadSave  → the engine fires onSaveLoaded after the
     load settles,
  5. assert aiState[A] was pruned and aiState[B] preserved.

`unitAi.getState(uid)` is the public observation hook (returns the live
aiState entry or nil). Exit 0 = fix verified.

Usage:  python3 tools/lua_orphan_prune_probe.py [--port 9008] [--seed 42]
"""
from __future__ import annotations

import argparse
import glob
import socket
import subprocess
import sys
import time

LOG = "/tmp/orphan_prune_engine.log"


def send(port: int, lua: str, timeout: float = 10.0) -> str:
    with socket.create_connection(("localhost", port), timeout=timeout) as s:
        s.sendall((lua + "\n").encode())
        chunks: list[bytes] = []
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


def boot(port: int) -> subprocess.Popen:
    log = open(LOG, "w")
    proc = subprocess.Popen(
        ["cabal", "run", "-v0", "exe:synarchy", "--",
         "--headless", "--port", str(port)],
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


def bootstrap_defs(port: int) -> None:
    loaders = [
        ("data/substances/*.yaml", "engine.loadSubstanceYaml"),
        ("data/items/*.yaml",      "engine.loadItemYaml"),
        ("data/equipment/*.yaml",  "engine.loadEquipmentYaml"),
        ("data/materials/*.yaml",  "engine.loadMaterialYaml"),
        ("data/units/*.yaml",      "engine.loadUnitYaml"),
    ]
    for pattern, fn in loaders:
        for path in sorted(glob.glob(pattern)):
            send(port, f"{fn}('{path}'); return 'ok'")
    for script, dt in [("unit_stats", 0.1), ("unit_resources", 0.2),
                       ("unit_ai", 0.1)]:
        send(port, f"engine.loadScript('scripts/{script}.lua', {dt}); return 'ok'")


def find_flat(port: int) -> tuple[int, int] | None:
    """Find two adjacent dry equal-z land tiles for two spawns."""
    lua = (
        "local function f() for gy=-8,8 do for gx=-8,6 do "
        "local za=world.getTerrainAt(gx,gy) local zb=world.getTerrainAt(gx+1,gy) "
        "if za and zb and za==zb and za>0 then "
        "local fa=world.getFluidAt(gx,gy) local fb=world.getFluidAt(gx+1,gy) "
        "if (not fa or fa.type=='none') and (not fb or fb.type=='none') then "
        "return gx..','..gy end end end end return 'none' end return f()"
    )
    for _ in range(8):
        r = send(port, lua).strip().strip('"')
        if r and r != "none" and "," in r:
            gx, gy = r.split(",")
            return int(gx), int(gy)
        time.sleep(1.0)
    return None


def getstate(port: int, uid: int) -> str:
    return send(port,
        f"local s=require('scripts.unit_ai').getState({uid}); "
        f"return s and 'present' or 'nil'").strip().strip('"')


def exists(port: int, uid: int) -> bool:
    r = send(port, f"return unit.exists({uid}) and 'yes' or 'no'").strip().strip('"')
    return r == "yes"


def main() -> int:
    ap = argparse.ArgumentParser()
    ap.add_argument("--port", type=int, default=9008)
    ap.add_argument("--seed", type=int, default=42)
    ap.add_argument("--size", type=int, default=64)
    args = ap.parse_args()

    proc = boot(args.port)
    ok = True
    try:
        bootstrap_defs(args.port)
        send(args.port, f"world.init('arena', {args.seed}, {args.size}, 3); return 'ok'")
        send(args.port, "return world.waitForInit(180)", timeout=190)
        send(args.port, "world.show('arena'); return 'ok'")
        send(args.port, "return world.loadChunksInRegion(-2,-2,2,2)")
        send(args.port, "return world.waitForChunks(60)", timeout=70)

        spot = find_flat(args.port)
        if not spot:
            print("FAIL: no flat dry ground found to spawn on")
            return 1
        gx, gy = spot
        print(f"Spawning on flat ground at ({gx},{gy})")

        a = int(float(send(args.port, f"return unit.spawn('acolyte', {gx}, {gy})")))
        b = int(float(send(args.port, f"return unit.spawn('acolyte', {gx+1}, {gy})")))
        print(f"Spawned A={a} (to be orphaned), B={b} (control)")

        # Let the AI tick so aiState[A]/[B] are created.
        time.sleep(3.0)
        sa, sb = getstate(args.port, a), getstate(args.port, b)
        print(f"After tick: aiState[A]={sa}, aiState[B]={sb}")
        if sa != "present" or sb != "present":
            print("FAIL: AI state was not created for spawned units "
                  "(can't exercise the leak)")
            return 1

        # Destroy A. It's queued — wait until the unit thread drops it.
        send(args.port, f"unit.destroy({a}); return 'ok'")
        for _ in range(20):
            if not exists(args.port, a):
                break
            time.sleep(0.3)
        if exists(args.port, a):
            print("FAIL: unit A never got destroyed")
            return 1
        leaked = getstate(args.port, a)
        print(f"After destroy: unit.exists(A)=no, aiState[A]={leaked} "
              f"(lingering = the leak)")

        # Save + load. The engine fires onSaveLoaded after the load
        # settles; the reconcile should prune A while keeping B.
        send(args.port, "return engine.saveWorld('arena','orphan_test')")
        send(args.port, "return engine.loadSave('orphan_test')")
        # Wait for the load to settle (world thread restores units, then
        # enqueues the LuaSaveLoaded broadcast). Re-show the loaded page.
        time.sleep(2.0)
        send(args.port, "world.show('main_world'); return 'ok'")

        # Poll until the reconcile has run (aiState[A] pruned) or timeout.
        pruned = False
        for _ in range(40):
            if getstate(args.port, a) == "nil":
                pruned = True
                break
            time.sleep(0.5)
        final_a = getstate(args.port, a)
        final_b = getstate(args.port, b)
        print(f"After load: aiState[A]={final_a}, aiState[B]={final_b}")

        if not pruned or final_a != "nil":
            print("FAIL: stale aiState[A] for the dropped unit was NOT "
                  "pruned after load (#195 not fixed)")
            ok = False
        elif final_b != "present":
            print("FAIL: aiState[B] for a LIVE unit was wrongly pruned "
                  "(reconcile too aggressive)")
            ok = False
        else:
            print("PASS: dropped-unit AI state pruned, live-unit state kept")
    finally:
        try:
            send(args.port, "engine.quit(); return 'bye'", timeout=3)
        except Exception:
            pass
        try:
            proc.wait(timeout=10)
        except Exception:
            proc.kill()

    return 0 if ok else 1


if __name__ == "__main__":
    sys.exit(main())
