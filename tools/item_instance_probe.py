#!/usr/bin/env python3
"""Headless probe for per-instance item identity (issue #67).

Same-def item instances used to be keyed/targeted by defName only, so the
UI could act on a DIFFERENT instance than the one the player clicked. The
fix gives every ItemInstance a process-unique `iiInstanceId` and lets the
item APIs target by it. This probe verifies, headless and without a GPU:

  1. IDENTITY  — two same-def items get DISTINCT instanceIds, exposed via
                 unit.getInventory.
  2. TARGETING — equipment.equip(uid, slot, defName, instanceId) equips the
                 EXACT instance asked for (the other same-def item stays in
                 inventory), not the first defName match. This is the #67D
                 "Equip hits wrong instance" case, the headline of the bug.
  3. FALLBACK  — equip with NO instanceId still works (AI/legacy callers),
                 removing the first defName match.
  4. PERSIST   — save + load preserves instanceIds and the allocator
                 continues above every loaded id (a fresh item gets a new,
                 non-colliding id). Best-effort; skipped with --no-save.

Exit 0 = all enabled checks passed.

Usage:
  python3 tools/item_instance_probe.py
  python3 tools/item_instance_probe.py --port 9171 --no-save
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

LOG = "/tmp/item_instance_engine.log"
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
    """unit.getInventory(uid) as a list of dicts (defName, instanceId, ...).

    The debug console auto-serializes a returned Lua table to JSON, so we
    just return a trimmed copy with the fields we care about.
    """
    raw = send(port,
               f"local t=unit.getInventory({uid}) or {{}}; "
               "local o={}; for i,it in ipairs(t) do o[i]={defName=it.defName,"
               "instanceId=it.instanceId,quality=it.quality,"
               "currentFill=it.currentFill,contentsKey=it.contentsKey,"
               "weight=it.weight} end; return o").strip()
    try:
        data = json.loads(raw)
    except json.JSONDecodeError:
        return []
    return data if isinstance(data, list) else []


def as_int(s: str) -> int:
    """Coerce a console numeric reply ('1', '1.0', '"2"') to int."""
    return int(float(s.strip().strip('"')))


def picks(inv: list[dict]) -> list[dict]:
    return [it for it in inv if it.get("defName") == WEAPON]


def contents_rows(port: int, uid: int, def_name: str, inst_id=None) -> int:
    """unit.getItemContents row count: -1 if nil (no such container), 0 if
    empty, N>0 if stocked."""
    arg = f", {inst_id}" if inst_id is not None else ""
    return as_int(send(port,
        f"local r=unit.getItemContents({uid}, '{def_name}'{arg}); "
        "if not r then return -1 end; return #r"))


def content_weight(port: int, uid: int, cont_def: str, inst_id: int,
                   content_def: str) -> float:
    """Weight (kg) the Contents view reports for one content type, or -1."""
    raw = send(port,
        f"local r=unit.getItemContents({uid}, '{cont_def}', {inst_id}) or {{}}; "
        f"for _,it in ipairs(r) do if it.defName=='{content_def}' then "
        "return it.weight end end; return -1")
    return float(raw.strip().strip('"'))


CHECKS: list[tuple[str, bool]] = []


def check(name: str, ok: bool, detail: str = "") -> None:
    CHECKS.append((name, ok))
    mark = "PASS" if ok else "FAIL"
    print(f"  [{mark}] {name}" + (f" — {detail}" if detail else ""))


def main() -> int:
    ap = argparse.ArgumentParser(description=__doc__)
    ap.add_argument("--seed", type=int, default=42)
    ap.add_argument("--size", type=int, default=64)
    ap.add_argument("--port", type=int, default=9171)
    ap.add_argument("--no-save", action="store_true")
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

        # Add two identical-def weapons. Each is a genuine creation → distinct id.
        send(args.port, f"return unit.addItem({uid}, '{WEAPON}', 0)")
        send(args.port, f"return unit.addItem({uid}, '{WEAPON}', 0)")
        inv = inventory(args.port, uid)
        ps = picks(inv)

        print("\n== IDENTITY ==")
        check(f"two '{WEAPON}' in inventory", len(ps) >= 2,
              f"found {len(ps)}")
        if len(ps) < 2:
            return summarize()
        idA, idB = ps[0]["instanceId"], ps[1]["instanceId"]
        qA, qB = ps[0].get("quality"), ps[1].get("quality")
        check("instanceIds are distinct", idA != idB, f"idA={idA} idB={idB}")
        check("instanceIds are non-zero", bool(idA) and bool(idB),
              f"idA={idA} idB={idB}")

        print("\n== TARGETING (equip the 2nd by id) ==")
        ok = send(args.port,
                  f"return equipment.equip({uid}, '{SLOT}', '{WEAPON}', {idB})")
        check("equip(...instanceId=idB) returned true", ok.strip() == "true", ok)
        inv2 = inventory(args.port, uid)
        ids2 = {it["instanceId"] for it in picks(inv2)}
        check("the targeted instance (idB) left inventory", idB not in ids2,
              f"remaining {WEAPON} ids={sorted(ids2)}")
        check("the non-targeted instance (idA) stayed", idA in ids2,
              f"remaining {WEAPON} ids={sorted(ids2)}")

        # Confirm the equipped slot holds idB exactly.
        eq_id = as_int(send(args.port,
                            f"local lo=equipment.getLoadout({uid}); "
                            f"local s=lo and lo['{SLOT}']; return s and s.instanceId or -1"))
        check("equipped slot holds idB", eq_id == idB,
              f"slot instanceId={eq_id} want {idB}")

        print("\n== FALLBACK (equip with no id → first defName match) ==")
        # idA is still loose; equipping by defName with no id should take it
        # (and swap the currently-equipped idB back into inventory).
        send(args.port, f"return equipment.equip({uid}, '{SLOT}', '{WEAPON}')")
        eq_id2 = as_int(send(args.port,
                             f"local lo=equipment.getLoadout({uid}); "
                             f"local s=lo and lo['{SLOT}']; return s and s.instanceId or -1"))
        check("no-id equip moved a real instance into the slot",
              eq_id2 > 0, f"slot now {eq_id2}")

        print("\n== MISMATCH GUARD (weapon defName + non-weapon id) ==")
        # Add a canteen (kind: container) and try to equip it into the
        # weapon slot using a WEAPON defName but the canteen's id. The kind
        # gate must validate the popped instance, not the defName arg.
        send(args.port, f"return unit.addItem({uid}, 'canteen_steel_2l', 0.5)")
        inv3 = inventory(args.port, uid)
        cans = [it for it in inv3 if it.get("defName") == "canteen_steel_2l"]
        if not cans:
            check("canteen present for mismatch test", False, "no canteen")
        else:
            can_id = cans[0]["instanceId"]
            slot_before = as_int(send(args.port,
                f"local lo=equipment.getLoadout({uid}); "
                f"local s=lo and lo['{SLOT}']; return s and s.instanceId or -1"))
            res = send(args.port,
                       f"return equipment.equip({uid}, '{SLOT}', '{WEAPON}', {can_id})")
            check("equip(weapon defName, canteen id) returns false",
                  res.strip() == "false", res)
            slot_after = as_int(send(args.port,
                f"local lo=equipment.getLoadout({uid}); "
                f"local s=lo and lo['{SLOT}']; return s and s.instanceId or -1"))
            check("canteen did NOT enter the weapon slot", slot_after != can_id,
                  f"slot now {slot_after}, canteen {can_id}")
            check("weapon slot unchanged by the rejected equip",
                  slot_after == slot_before,
                  f"before {slot_before} after {slot_after}")
            still = {it["instanceId"] for it in inventory(args.port, uid)
                     if it.get("defName") == "canteen_steel_2l"}
            check("canteen stayed in inventory after rejection", can_id in still,
                  f"canteen ids {sorted(still)}")

            # Accessory analogue: equipAccessory must also refuse a
            # non-accessory id (only kind: accessory belongs on uiAccessories).
            acc_before = as_int(send(args.port,
                f"return #(equipment.getAccessories({uid}) or {{}})"))
            ares = send(args.port,
                f"return equipment.equipAccessory({uid}, 'technogoggles', {can_id})")
            check("equipAccessory(accessory defName, canteen id) returns false",
                  ares.strip() == "false", ares)
            acc_after = as_int(send(args.port,
                f"return #(equipment.getAccessories({uid}) or {{}})"))
            check("canteen did NOT enter the accessory list",
                  acc_after == acc_before, f"accessories {acc_before} -> {acc_after}")
            still2 = {it["instanceId"] for it in inventory(args.port, uid)
                      if it.get("defName") == "canteen_steel_2l"}
            check("canteen still in inventory after accessory rejection",
                  can_id in still2, f"canteen ids {sorted(still2)}")

        print("\n== CONTAINER DIVERGENCE (#67A) ==")
        # The technomule spawns with a PRE-STOCKED first_aid_kit. Add a
        # second (empty) kit and confirm the two same-def containers stay
        # distinct: different contentsKey, and getItemContents-by-id returns
        # each kit's own contents instead of collapsing onto a representative.
        muid = as_int(send(args.port,
            f"return unit.spawn('technomule', {gx}+0.5, {gy}+0.5, nil, 'debug')"))
        if muid <= 0:
            check("spawn technomule", False, f"uid={muid}")
        else:
            kits = [it for it in inventory(args.port, muid)
                    if it.get("defName") == "first_aid_kit"]
            stocked = kits[0] if kits else None
            sk = (stocked or {}).get("contentsKey") or ""
            check("technomule carries a stocked first_aid_kit",
                  stocked is not None and sk != "",
                  f"kits={len(kits)} contentsKey={sk[:16]!r}")
            if stocked and sk != "":
                stocked_id = stocked["instanceId"]
                send(args.port, f"return unit.addItem({muid}, 'first_aid_kit', 0)")
                kits2 = [it for it in inventory(args.port, muid)
                         if it.get("defName") == "first_aid_kit"]
                empties = [it for it in kits2 if (it.get("contentsKey") or "") == ""]
                check("two same-def kits expose DIFFERENT contentsKey",
                      len(kits2) >= 2 and len(empties) >= 1,
                      f"keys={[(k['instanceId'], (k.get('contentsKey') or '')[:8]) for k in kits2]}")
                if empties:
                    empty_id = empties[0]["instanceId"]
                    sc = contents_rows(args.port, muid, "first_aid_kit", stocked_id)
                    ec = contents_rows(args.port, muid, "first_aid_kit", empty_id)
                    check("getItemContents(stocked id) returns its supplies",
                          sc > 0, f"rows={sc}")
                    check("getItemContents(empty id) returns the EMPTY kit",
                          ec == 0, f"rows={ec}")
                    check("the diverged kits keep distinct instance ids",
                          stocked_id != empty_id, f"{stocked_id} vs {empty_id}")
                    # The 1 L antiseptic bottle must report its FILLED mass
                    # (0.12 empty + 1.0 L × 1.0 ≈ 1.12 kg), not the empty-bottle
                    # def weight (0.12).
                    aw = content_weight(args.port, muid, "first_aid_kit",
                                        stocked_id, "antiseptic")
                    check("Contents weight includes bottle fill (~1.12, not 0.12)",
                          aw > 1.0, f"antiseptic weight={aw}")

        if not args.no_save:
            print("\n== PERSIST (save / load) ==")
            ids_before = sorted({it["instanceId"] for it in picks(inventory(args.port, uid))})
            send(args.port, "engine.saveWorld('arena', 'issue67_probe'); return 'ok'")
            send(args.port, "engine.loadSave('issue67_probe'); return 'ok'")
            # Units (and their item instances) restore early, but give the
            # load handler a moment to swap pages and rebuild managers.
            time.sleep(16)
            send(args.port, "world.show('main_world'); return 'ok'")
            # Unit ids are preserved across save/load (UnitSnapshot is keyed
            # by UnitId), so the same uid still addresses the acolyte.
            ids_after = sorted({it["instanceId"] for it in picks(inventory(args.port, uid))})
            check("loaded inventory preserves instanceIds",
                  ids_after == ids_before and len(ids_after) > 0,
                  f"before={ids_before} after={ids_after}")
            # A fresh item after load must get an id above every loaded one.
            send(args.port, f"return unit.addItem({uid}, '{WEAPON}', 0)")
            new_ids = sorted({it["instanceId"] for it in picks(inventory(args.port, uid))})
            fresh = [i for i in new_ids if i not in ids_after]
            allmax = max(ids_after) if ids_after else 0
            check("post-load fresh item id continues above loaded ids",
                  bool(fresh) and min(fresh) > allmax,
                  f"fresh={fresh} loaded_max={allmax}")

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
