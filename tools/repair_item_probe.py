#!/usr/bin/env python3
"""Headless probe for the repair primitive (issue #300).

`unit.repairItem(uid, instanceId, conditionDelta[, sharpnessDelta])` is the
low-level verb that ADJUSTS an item instance's two wear axes — `iiCondition`
(structural, gates breakage) and `iiSharpness` (edge keenness, gates
penetration) — in place, clamped to 0..100, preserving the physical item's
`iiInstanceId` (#67). Positive restores; negative wears. Everything else in
the repair arc (#301 stations, #302 AI, #303 UI) sits on top of it.

This verifies, headless and without a GPU:

  1. INVENTORY     — degrade a stowed weapon by a negative delta, then repair
                     it; both axes track, the returned applied-deltas match,
                     and the instanceId is unchanged. An independent
                     getInventory re-read confirms the mutation persisted.
  2. CLAMP         — repairing past 100 / wearing past 0 saturates at the
                     bound, and the reported applied-delta is the partial
                     amount actually moved (0 when already at the bound).
  3. EQUIPMENT     — the same verb finds and repairs a WIELDED instance, not
                     just a stowed one; equipment.getLoadout confirms both
                     restored axes.
  4. IDENTITY/MISS — repair targets the exact instance by id (a second
                     same-def item is untouched); a bad id or bad uid returns
                     nil and mutates nothing.

Exit 0 = all checks passed.

Usage:
  python3 tools/repair_item_probe.py
  python3 tools/repair_item_probe.py --port 9172
"""
from __future__ import annotations

import argparse
import glob
import json
import socket
import subprocess
import sys
import time
from probelib import quit_engine, boot, send

LOG = "/tmp/repair_item_engine.log"
WEAPON = "pick_steel"   # kind: weapon — matches the humanoid right_hand slot
SLOT = "right_hand"


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
    lua = (
        "local function f() for gy=-8,8 do for gx=-8,8 do "
        "local z=world.getTerrainAt(gx,gy) local fl=world.getFluidAt(gx,gy) "
        "if z and not fl then return gx..','..gy end end end return 'none' end "
        "return f()"
    )
    for _ in range(8):
        res = send(port, lua).strip('"')
        if res and res != "none" and res.count(",") == 1:
            gx, gy = (int(v) for v in res.split(","))
            return gx, gy
        time.sleep(0.75)
    return None


def inventory(port: int, uid: int) -> list[dict]:
    raw = send(port,
               f"local t=unit.getInventory({uid}) or {{}}; "
               "local o={}; for i,it in ipairs(t) do o[i]={defName=it.defName,"
               "instanceId=it.instanceId,sharpness=it.sharpness,"
               "condition=it.condition} end; return o").strip()
    try:
        data = json.loads(raw)
    except json.JSONDecodeError:
        return []
    return data if isinstance(data, list) else []


def repair(port: int, uid: int, iid, cond_d, sharp_d=None) -> dict | None:
    """Call unit.repairItem and parse its result table, or None on nil."""
    args = f"{uid}, {iid}, {cond_d}"
    if sharp_d is not None:
        args += f", {sharp_d}"
    raw = send(port, f"return unit.repairItem({args})").strip()
    if not raw or raw == "nil" or raw.strip('"') == "nil":
        return None
    try:
        data = json.loads(raw)
    except json.JSONDecodeError:
        return None
    return data if isinstance(data, dict) else None


def as_int(s: str) -> int:
    return int(float(s.strip().strip('"')))


def picks(inv: list[dict]) -> list[dict]:
    return [it for it in inv if it.get("defName") == WEAPON]


CHECKS: list[tuple[str, bool]] = []


def check(name: str, ok: bool, detail: str = "") -> None:
    CHECKS.append((name, ok))
    mark = "PASS" if ok else "FAIL"
    print(f"  [{mark}] {name}" + (f" — {detail}" if detail else ""))


def approx(a: float, b: float, eps: float = 0.5) -> bool:
    return abs(a - b) <= eps


def main() -> int:
    ap = argparse.ArgumentParser(description=__doc__)
    ap.add_argument("--seed", type=int, default=42)
    ap.add_argument("--size", type=int, default=64)
    ap.add_argument("--port", type=int, default=9172)
    args = ap.parse_args()

    proc = boot(args.port, log=LOG)
    try:
        bootstrap_defs(args.port)
        send(args.port, f"world.init('arena', {args.seed}, {args.size}, 3); return 'ok'")
        send(args.port, "return world.waitForInit(180)", timeout=190)
        send(args.port, "world.show('arena'); return 'ok'")
        send(args.port, "return world.loadChunksInRegion(-1,-1,1,1)")
        send(args.port, "return world.waitForChunks(120)", timeout=125)

        flat = find_flat(args.port)
        if not flat:
            print("could not find flat ground", file=sys.stderr)
            return 2
        gx, gy = flat
        uid = as_int(send(args.port,
                          f"return unit.spawn('acolyte', {gx}+0.5, {gy}+0.5, nil, 'debug')"))
        if uid <= 0:
            print(f"spawn failed (uid={uid})", file=sys.stderr)
            return 2
        print(f"spawned acolyte uid={uid} at ({gx},{gy})")

        # Two same-def weapons: one is the repair target, one is the
        # untouched control for the identity check.
        send(args.port, f"return unit.addItem({uid}, '{WEAPON}', 0)")
        send(args.port, f"return unit.addItem({uid}, '{WEAPON}', 0)")
        ps = picks(inventory(args.port, uid))
        if len(ps) < 2:
            check("two weapons in inventory", False, f"found {len(ps)}")
            return summarize()
        target, control = ps[0], ps[1]
        iid = target["instanceId"]
        ctrl_id = control["instanceId"]
        ctrl_sharp0 = control["sharpness"]
        print(f"target instanceId={iid} (fresh sharpness={target['sharpness']}, "
              f"condition={target['condition']})")

        print("\n== INVENTORY: degrade then repair ==")
        # Bring both axes to a known mid value so the deltas are exact.
        # Fresh adds start at sharpness 100; condition is rolled, so first
        # floor it to 0 then raise both to 50 for a clean baseline.
        repair(args.port, uid, iid, -1000, -1000)  # floor both
        base = repair(args.port, uid, iid, 50, 50)
        check("set baseline 50/50", base is not None
              and approx(base["condition"], 50) and approx(base["sharpness"], 50),
              f"{base}")

        worn = repair(args.port, uid, iid, -20, -30)
        check("negative delta wears both axes (50→30 cond, 50→20 sharp)",
              worn is not None and approx(worn["condition"], 30)
              and approx(worn["sharpness"], 20), f"{worn}")
        check("applied deltas reported (-20 cond, -30 sharp)",
              worn is not None and approx(worn["conditionApplied"], -20)
              and approx(worn["sharpnessApplied"], -30), f"{worn}")

        fixed = repair(args.port, uid, iid, 25, 40)
        check("positive delta restores both axes (30→55 cond, 20→60 sharp)",
              fixed is not None and approx(fixed["condition"], 55)
              and approx(fixed["sharpness"], 60), f"{fixed}")

        # Independent re-read: the mutation must be visible through a
        # different API, not just echoed back by repairItem.
        after = next((it for it in picks(inventory(args.port, uid))
                      if it["instanceId"] == iid), None)
        check("getInventory re-read confirms restored sharpness (~60)",
              after is not None and approx(after["sharpness"], 60),
              f"sharpness={after and after['sharpness']}")
        check("getInventory re-read confirms restored condition (~55)",
              after is not None and approx(after["condition"], 55),
              f"condition={after and after['condition']}")

        print("\n== CLAMP at 0 and 100 ==")
        hi = repair(args.port, uid, iid, 1000, 1000)
        check("repair past 100 saturates at 100/100",
              hi is not None and approx(hi["condition"], 100)
              and approx(hi["sharpness"], 100), f"{hi}")
        # From 55/60 the applied amounts are the partial gaps (45 / 40).
        check("applied delta is the partial gap to the cap",
              hi is not None and approx(hi["conditionApplied"], 45)
              and approx(hi["sharpnessApplied"], 40), f"{hi}")
        nop = repair(args.port, uid, iid, 1000, 1000)
        check("repairing an already-full item applies 0",
              nop is not None and approx(nop["conditionApplied"], 0)
              and approx(nop["sharpnessApplied"], 0), f"{nop}")
        lo = repair(args.port, uid, iid, -1000, -1000)
        check("wear past 0 saturates at 0/0",
              lo is not None and approx(lo["condition"], 0)
              and approx(lo["sharpness"], 0), f"{lo}")

        print("\n== IDENTITY: instance preserved, control untouched ==")
        still = next((it for it in picks(inventory(args.port, uid))
                      if it["instanceId"] == iid), None)
        check("target keeps its instanceId through every repair",
              still is not None, f"iid={iid} present={still is not None}")
        ctrl = next((it for it in picks(inventory(args.port, uid))
                     if it["instanceId"] == ctrl_id), None)
        check("the OTHER same-def instance was not touched",
              ctrl is not None and approx(ctrl["sharpness"], ctrl_sharp0),
              f"control sharpness {ctrl and ctrl['sharpness']} (was {ctrl_sharp0})")

        print("\n== EQUIPMENT path ==")
        # Equip the target, degrade + repair it while wielded.
        send(args.port, f"return equipment.equip({uid}, '{SLOT}', '{WEAPON}', {iid})")
        eq_id = as_int(send(args.port,
            f"local lo=equipment.getLoadout({uid}); "
            f"local s=lo and lo['{SLOT}']; return s and s.instanceId or -1"))
        check("target is now equipped", eq_id == iid, f"slot id {eq_id}")
        eqr = repair(args.port, uid, iid, 70, 80)  # from 0/0
        check("repairItem finds the WIELDED instance",
              eqr is not None and approx(eqr["condition"], 70)
              and approx(eqr["sharpness"], 80), f"{eqr}")
        # getLoadout exposes the equipped instance's live axes.
        ls = send(args.port,
            f"local lo=equipment.getLoadout({uid}); local s=lo and lo['{SLOT}']; "
            "if not s then return -1 end; return s.sharpness")
        lc = send(args.port,
            f"local lo=equipment.getLoadout({uid}); local s=lo and lo['{SLOT}']; "
            "if not s then return -1 end; return s.condition")
        check("getLoadout confirms equipped sharpness (~80)",
              approx(float(ls.strip().strip('"')), 80), f"sharpness={ls}")
        check("getLoadout confirms equipped condition (~70)",
              approx(float(lc.strip().strip('"')), 70), f"condition={lc}")

        print("\n== ACCESSORY path (worn, condition-scaled buff) ==")
        # Worn accessories live in a SEPARATE collection (uiAccessories) and
        # their buffs are baked into uiModifiers at equip time, scaled by
        # condition (technogoggles: +1 perception × condition/100). Repair
        # must reach them AND refresh the modifier, or the buff stays stale.
        def perception() -> float:
            return float(send(args.port,
                f"return unit.getStat({uid}, 'perception')").strip().strip('"'))

        # NB: acolytes already spawn wearing a technogoggles, and the buff
        # dedups by source name, so an ABSOLUTE perception baseline is
        # confounded. The robust signal is the DIFFERENCE in perception
        # across two conditions of the SAME worn target: the +1.0/100-per-
        # condition buff means cond 100 vs 50 must differ by exactly 0.5. If
        # repair did NOT refresh uiModifiers, perception would not move at
        # all when condition changes.
        send(args.port, f"return unit.addItem({uid}, 'technogoggles', 0)")
        goggles = next((it for it in inventory(args.port, uid)
                        if it.get("defName") == "technogoggles"), None)
        if goggles is None:
            check("technogoggles in inventory", False, "missing")
        else:
            acc_id = goggles["instanceId"]
            send(args.port,
                 f"return equipment.equipAccessory({uid}, 'technogoggles', {acc_id})")
            worn = send(args.port,
                f"local a=equipment.getAccessories({uid}) or {{}}; "
                f"for _,it in ipairs(a) do if it.instanceId=={acc_id} then "
                "return 1 end end; return 0").strip().strip('"')
            check("technogoggles is now worn (in accessories)", worn == "1", worn)

            full = repair(args.port, uid, acc_id, 1000, 0)  # condition → 100
            check("repairItem reaches the WORN accessory",
                  full is not None and approx(full["condition"], 100), f"{full}")
            perc_full = perception()

            # Drop to 50% and re-read: the buff must re-scale.
            repair(args.port, uid, acc_id, -1000, 0)  # floor to 0
            repair(args.port, uid, acc_id, 50, 0)     # → 50
            perc_half = perception()
            check("worn-accessory buff RE-SCALES with repaired condition "
                  "(cond 100→50 drops perception by 0.5)",
                  approx(perc_full - perc_half, 0.5, 0.05),
                  f"perc@100={perc_full} perc@50={perc_half} "
                  f"delta={perc_full - perc_half:.3f}")
            check("the repaired buff actually moved (refresh fired)",
                  perc_full > perc_half + 0.01,
                  f"perc@100={perc_full} perc@50={perc_half}")

            # DUPLICATE source: wear a SECOND technogoggles (now the live one,
            # since equip order = last wins). Repairing the OLDER pair must NOT
            # hijack the active buff — the refresh re-derives from the whole
            # worn list in order, so the newer pair stays live.
            send(args.port, f"return unit.addItem({uid}, 'technogoggles', 0)")
            inv_g = [it for it in inventory(args.port, uid)
                     if it.get("defName") == "technogoggles"]
            acc_id2 = inv_g[0]["instanceId"] if inv_g else None
            if acc_id2 is None:
                check("second technogoggles in inventory", False, "missing")
            else:
                send(args.port,
                     f"return equipment.equipAccessory({uid}, 'technogoggles', {acc_id2})")
                # Both pairs to 100 → known baseline (live = the newer acc_id2).
                repair(args.port, uid, acc_id, 1000, 0)
                repair(args.port, uid, acc_id2, 1000, 0)
                perc_base = perception()
                # Wreck the OLDER pair (acc_id) to 0; the live newer pair is
                # still 100, so perception must be UNCHANGED.
                repair(args.port, uid, acc_id, -1000, 0)
                perc_after_old = perception()
                check("repairing an OLDER duplicate does NOT hijack the live buff",
                      approx(perc_after_old, perc_base, 0.05),
                      f"base={perc_base} after_older_repair={perc_after_old}")
                # Sanity: the LIVE pair still drives the buff (drop it to 50).
                repair(args.port, uid, acc_id2, -1000, 0)
                repair(args.port, uid, acc_id2, 50, 0)
                perc_live50 = perception()
                check("the live (last-equipped) duplicate still controls the buff",
                      approx(perc_base - perc_live50, 0.5, 0.05),
                      f"base={perc_base} live@50={perc_live50}")

        print("\n== MISS: bad id / bad uid → nil ==")
        miss = repair(args.port, uid, 999999999, 50, 50)
        check("unknown instanceId returns nil", miss is None, f"{miss}")
        badu = repair(args.port, 999999, iid, 50, 50)
        check("unknown uid returns nil", badu is None, f"{badu}")
        # The equipped target is still intact after the failed calls.
        ls2 = send(args.port,
            f"local lo=equipment.getLoadout({uid}); local s=lo and lo['{SLOT}']; "
            "if not s then return -1 end; return s.sharpness")
        check("a missed repair mutated nothing (still ~80 sharp)",
              approx(float(ls2.strip().strip('"')), 80), f"sharpness={ls2}")

        return summarize()
    finally:
        quit_engine(args.port, proc)


def summarize() -> int:
    passed = sum(1 for _, ok in CHECKS if ok)
    total = len(CHECKS)
    print(f"\n{passed}/{total} checks passed")
    return 0 if passed == total else 1


if __name__ == "__main__":
    sys.exit(main())
