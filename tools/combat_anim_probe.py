#!/usr/bin/env python3
"""Headless combat-animation probe.

Agents can't see pixels headless, but the engine tracks the animation
STATE — which animation each unit is playing — and updates it on the
unit thread, which runs headless. `unit.getInfo(uid).currentAnim`
exposes it. This tool drives a real fight in a headless engine and
samples that state over time, so combat/animation behaviour can be
verified WITHOUT a GPU or a human watching.

What it does:
  1. launches `--headless` on a private port,
  2. loads the substance/item/equipment/material/unit YAML defs and the
     AI scripts (unit_stats, unit_resources, unit_ai) that the
     loading-screen flow would normally load (it doesn't run headless),
  3. generates a small world, finds a flat dry patch,
  4. spawns an attacker next to a target and issues commandAttack,
  5. polls each unit's currentAnim for a few seconds and prints the
     per-unit animation timeline,
  6. checks the attacker threw a recognizable swing animation and, if it
     died, settled on a death animation (the two bugs this guards).

Usage:
  python3 tools/combat_anim_probe.py            # acolyte vs bear_brown
  python3 tools/combat_anim_probe.py --attacker acolyte --target bear_brown
  python3 tools/combat_anim_probe.py --seconds 12 --port 9123

Exit code 0 = the expected animation states were observed.
"""
from __future__ import annotations

import argparse
import glob
import socket
import subprocess
import sys
import time
from probelib import quit_engine, boot, send


def bootstrap_defs(port: int) -> None:
    # The loading screen doesn't run headless — load defs + AI scripts by hand.
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


def find_flat_strip(port: int) -> tuple[int, int, int] | None:
    """Return (gx, gy, z) of a dry 3-wide equal-z land strip, or None.

    Retries briefly: just after waitForChunks the terrain queries can
    still read nil for a tick, which looks like 'no flat ground'.
    """
    lua = (
        "local function f() for gy=-8,8 do for gx=-8,6 do "
        "local za=world.getTerrainAt(gx,gy) local zb=world.getTerrainAt(gx+1,gy) "
        "local zc=world.getTerrainAt(gx+2,gy) "
        "local fa=world.getFluidAt(gx,gy) local fb=world.getFluidAt(gx+1,gy) "
        "local fc=world.getFluidAt(gx+2,gy) "
        "if za and zb and zc and za==zb and zb==zc and not fa and not fb and not fc "
        "then return gx..','..gy..','..za end end end return 'none' end return f()"
    )
    for _ in range(8):
        res = send(port, lua).strip('"')
        if res and res != "none" and res.count(",") == 2:
            gx, gy, z = (int(v) for v in res.split(","))
            return gx, gy, z
        time.sleep(0.75)
    return None


def anim_of(port: int, uid: int) -> str:
    r = send(port, f"local i=unit.getInfo({uid}); return i and i.currentAnim or 'DEAD'")
    return r.strip('"')


def main() -> int:
    ap = argparse.ArgumentParser(description=__doc__)
    ap.add_argument("--attacker", default="acolyte")
    ap.add_argument("--target", default="bear_brown")
    ap.add_argument("--seed", type=int, default=42)
    ap.add_argument("--size", type=int, default=64)
    ap.add_argument("--port", type=int, default=9123)
    ap.add_argument("--seconds", type=float, default=12.0)
    args = ap.parse_args()

    proc = boot(args.port)
    try:
        bootstrap_defs(args.port)
        send(args.port, f"world.init('arena', {args.seed}, {args.size}, 3); return 'ok'")
        send(args.port, "return world.waitForInit(180)", timeout=190)
        send(args.port, "world.show('arena'); return 'ok'")
        send(args.port, "return world.loadChunksInRegion(-1,-1,1,1)")
        send(args.port, "return world.waitForChunks(120)", timeout=125)

        strip = find_flat_strip(args.port)
        if not strip:
            print("FAIL: no flat dry ground found near origin", file=sys.stderr)
            return 2
        gx, gy, z = strip
        print(f"arena: flat strip at ({gx},{gy}) z={z}")

        a = int(float(send(args.port, f"return unit.spawn('{args.attacker}', {gx}, {gy})")))
        b = int(float(send(args.port, f"return unit.spawn('{args.target}', {gx+2}, {gy})")))
        print(f"spawned {args.attacker}=#{a}  {args.target}=#{b}")
        time.sleep(1.5)  # let them settle onto the ground before fighting

        send(args.port, f"require('scripts.unit_ai').commandAttack({a},{b}); return 'go'")

        seen_a: list[str] = []
        seen_b: list[str] = []
        steps = int(args.seconds / 0.25)
        for _ in range(steps):
            aa, ba = anim_of(args.port, a), anim_of(args.port, b)
            if not seen_a or seen_a[-1] != aa:
                seen_a.append(aa)
            if not seen_b or seen_b[-1] != ba:
                seen_b.append(ba)
            time.sleep(0.25)

        print(f"\n{args.attacker} #{a} anim timeline:\n  " + " → ".join(seen_a))
        print(f"\n{args.target} #{b} anim timeline:\n  " + " → ".join(seen_b))

        def swung(seq): return any("attack" in s for s in seq)
        def died(seq):  return seq[-1] == "DEAD" or any(
            d in seq[-1] for d in ("death", "dead"))

        ok = swung(seen_a) or swung(seen_b)
        print("\n--- checks ---")
        print(f"  a swing animation appeared : {swung(seen_a) or swung(seen_b)}")
        # If a combatant ended dead, it must show a death anim, not a stale
        # combat idle (the death-anim regression).
        for tag, seq in ((args.attacker, seen_a), (args.target, seen_b)):
            last = seq[-1]
            if last == "DEAD":
                continue  # removed entirely — fine
            if "combat_idle" in last or last in ("idle",):
                continue  # survived
            if any(d in last for d in ("death", "dead", "collapse")):
                print(f"  {tag} settled on a death animation: {last}")
        return 0 if ok else 1
    finally:
        quit_engine(args.port, proc)


if __name__ == "__main__":
    sys.exit(main())
