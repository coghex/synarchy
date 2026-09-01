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
  3b. DIVERGENCE — the #67A container rule, now that EVERY creation path
                 materializes a container def's authored contents (#1418):
                 two freshly stocked kits are INTERCHANGEABLE and share one
                 contentsKey, and only drawing a supply out of one splits
                 them, after which getItemContents-by-id answers about the
                 exact kit asked for.
  4. PERSIST   — save + load preserves instanceIds and the allocator
                 continues above every loaded id (a fresh item gets a new,
                 non-colliding id). The save and the load are each tied to
                 THEIR OWN request id, and run against a throwaway resource
                 root, so the assertions cannot read a generation this
                 invocation did not write. Skipped with --no-save.

Exit 0 = all enabled checks passed.

Usage:
  python3 tools/item_instance_probe.py
  python3 tools/item_instance_probe.py --port 9171 --no-save
"""
from __future__ import annotations

import argparse
import glob
import json
import os
import shutil
import socket
import stat
import subprocess
import sys
import tempfile
import time
from probelib import (boot, capture_request_id, quit_engine, send,
                      wait_load_published, wait_save_complete)

REPO = os.path.dirname(os.path.dirname(os.path.abspath(__file__)))
LOG = "/tmp/item_instance_engine.log"
WEAPON = "pick_steel"   # kind: weapon — matches the humanoid right_hand slot
SLOT = "right_hand"
PAGE = "arena"          # an ORDINARY generated page that merely carries that
                        # name (world.init, not world.initArena), so NB #365's
                        # arena-save prohibition does not apply.


def _make_owner_writable(top: str) -> None:
    """Add owner write (and directory search) permission throughout a
    freshly copied tree.

    `shutil.copytree` reproduces the SOURCE's mode bits, so a checkout
    whose `config/` is read-only -- a CI cache restored read-only, a
    read-only mount, an archive unpacked without write bits -- yields a
    private `config/` this run cannot use and cannot delete: unlinking a
    child needs owner write+search on its parent directory, so
    `remove_run_root` would report residue and leave the invocation's
    whole tree, engine log and save slot behind after a run that did
    nothing wrong (#1912). The copy is THIS invocation's, so it is made
    writable regardless of what the source happened to be; the source
    itself is never touched, and a symlink is skipped rather than
    followed, so the content families it names keep their own modes.
    Same treatment `tools/flora_growth_probe.py` and the four location
    probes give their own copies.
    """
    for path, dirs, files in os.walk(top):
        for name in [None, *dirs, *files]:
            target = path if name is None else os.path.join(path, name)
            try:
                mode = os.lstat(target).st_mode
                if stat.S_ISLNK(mode):
                    continue
                extra = stat.S_IRWXU if stat.S_ISDIR(mode) \
                    else stat.S_IRUSR | stat.S_IWUSR
                os.chmod(target, stat.S_IMODE(mode) | extra)
            except OSError:
                # Best effort: a mode this process cannot change is
                # reported by the cleanup that actually trips over it,
                # with the path it failed on, rather than here.
                pass


def make_isolated_root(base: str) -> str:
    """A throwaway resource root under `base`: the read-only content
    families symlinked, `config/` COPIED without `*.local.yaml` (so the
    run starts from the tracked defaults rather than this developer's
    settings, and cannot write into theirs), and its OWN empty `saves/`.

    The PERSIST phase below saves and loads for real. Without this the
    engine would resolve the repository as its resource root and write
    `saves/<slot>/` into the developer's live one, where the slot is
    reachable — and rotatable — by an ordinary `cabal run`. The whole
    root is created by this probe and deleted by it, so nothing the
    probe did not create is ever removed (`shutil.rmtree` unlinks the
    symlinks themselves, never walking into the repository they name).
    Pattern from
    `tools/unified_transfer_probe_support.py:make_isolated_root`.
    """
    root = os.path.join(base, "root")
    os.makedirs(root, exist_ok=True)
    for family in ("scripts", "assets", "data"):
        target = os.path.join(root, family)
        if not os.path.exists(target):
            os.symlink(os.path.join(REPO, family), target)
    config_dst = os.path.join(root, "config")
    if not os.path.exists(config_dst):
        shutil.copytree(os.path.join(REPO, "config"), config_dst,
                        ignore=shutil.ignore_patterns("*.local.yaml"))
        _make_owner_writable(config_dst)
    os.makedirs(os.path.join(root, "saves"), exist_ok=True)
    return root


def remove_run_root(base: str) -> bool:
    """Delete this invocation's own throwaway tree, save artifacts and
    all, and say whether it is really gone.

    Only ever removes the directory THIS process made with
    `tempfile.mkdtemp`, so nothing pre-existing is at risk; `rmtree`
    unlinks the symlinked content families rather than recursing into
    them, so the real `scripts/`, `assets/` and `data/` are never
    followed. A survivor makes the run non-zero: a green result sitting
    beside leftover saves is precisely the outcome this isolation
    exists to prevent, so it must not be reported as a pass (#1912).
    Before that this probe removed the tree with `ignore_errors=True`
    beneath an already-computed `return`, so a refused deletion was
    swallowed and reported as every check passing. Same shape the three
    sibling isolated-root probes already use.
    """
    try:
        shutil.rmtree(base)
    except OSError as exc:
        print(f"  [FAIL] could not remove this run's resource root "
              f"{base}: {exc}")
        return False
    if os.path.exists(base):
        print(f"  [FAIL] this run's resource root survived removal: {base}")
        return False
    return True


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


def content_count(port: int, uid: int, cont_def: str, inst_id: int,
                  content_def: str) -> int:
    """How many of one content type the Contents view reports, or -1.

    Rows are GROUPED by def name (pushGroupedContents), so ten bandages
    are one row of count 10 -- drawing a few out changes the count, never
    the row total, which is why the divergence check reads this and not
    `contents_rows`.
    """
    raw = send(port,
        f"local r=unit.getItemContents({uid}, '{cont_def}', {inst_id}) or {{}}; "
        f"for _,it in ipairs(r) do if it.defName=='{content_def}' then "
        "return it.count end end; return -1")
    return int(float(raw.strip().strip('"')))


CHECKS: list[tuple[str, bool]] = []


def check(name: str, ok: bool, detail: str = "") -> None:
    CHECKS.append((name, ok))
    mark = "PASS" if ok else "FAIL"
    print(f"  [{mark}] {name}" + (f" — {detail}" if detail else ""))


def persist_phase(port: int, uid: int, slot: str) -> None:
    """PERSIST: a save and a load, each tied to ITS OWN request id.

    Both `engine.saveWorld` and `engine.loadSave` only ACCEPT
    synchronously; the encode + disk write (issue #758) and the
    per-page reconstruction + publication (issue #763) both run
    afterwards. So the acceptance booleans have to be READ, and
    completion has to come from `engine.getSaveStatus()` /
    `engine.getLoadStatus()` reaching a terminal phase carrying THIS
    invocation's request id — a fixed-slot, sleep-paced sequence could
    otherwise assert against whatever generation happened to be on disk.

    Every step short-circuits: a rejected request, an uncaptured request
    id, or an unsuccessful/timed-out terminal status stops the phase
    before the next transaction or state assertion, so the two identity
    assertions below only ever run against a session this call knows it
    saved and loaded.
    """
    print("\n== PERSIST (save / load) ==")
    ids_before = sorted({it["instanceId"] for it in picks(inventory(port, uid))})

    accepted = send(port, f"return engine.saveWorld('{PAGE}', '{slot}')").strip()
    check(f"engine.saveWorld('{PAGE}', '{slot}') accepted the request",
          accepted == "true",
          f"returned {accepted!r}" + ("" if accepted == "true" else
                                      f"; the validation reason is logged in {LOG}"))
    if accepted != "true":
        return
    save_id = capture_request_id(port, "return engine.getSaveStatus()")
    check("engine.getSaveStatus() reports this save's own request id",
          save_id is not None, f"request id={save_id}")
    if save_id is None:
        return
    saved, save_status = wait_save_complete(port, save_id)
    check(f"save request {save_id} reached SaveCaptureComplete", saved,
          f"terminal status={save_status}")
    if not saved:
        return

    # Issue #763: engine.loadSave only ACCEPTS synchronously -- the
    # saved page (PAGE, its own id verbatim -- no more main_world remap)
    # doesn't exist live until the transaction actually publishes.
    load_accepted = send(port, f"return engine.loadSave('{slot}')").strip()
    check(f"engine.loadSave('{slot}') accepted the request",
          load_accepted == "true",
          f"returned {load_accepted!r}" + ("" if load_accepted == "true" else
                                           f"; the reason is logged in {LOG}"))
    if load_accepted != "true":
        return
    load_id = capture_request_id(port, "return engine.getLoadStatus()")
    check("engine.getLoadStatus() reports this load's own request id",
          load_id is not None, f"request id={load_id}")
    if load_id is None:
        return
    published, load_status = wait_load_published(port, 60, request_id=load_id)
    check(f"load request {load_id} published", published,
          f"terminal status={load_status}")
    if not published:
        return

    send(port, f"world.show('{PAGE}'); return 'ok'")
    # Unit ids are preserved across save/load (UnitSnapshot is keyed
    # by UnitId), so the same uid still addresses the acolyte.
    ids_after = sorted({it["instanceId"] for it in picks(inventory(port, uid))})
    check("loaded inventory preserves instanceIds",
          ids_after == ids_before and len(ids_after) > 0,
          f"before={ids_before} after={ids_after}")
    # A fresh item after load must get an id above every loaded one.
    send(port, f"return unit.addItem({uid}, '{WEAPON}', 0)")
    new_ids = sorted({it["instanceId"] for it in picks(inventory(port, uid))})
    fresh = [i for i in new_ids if i not in ids_after]
    allmax = max(ids_after) if ids_after else 0
    check("post-load fresh item id continues above loaded ids",
          bool(fresh) and min(fresh) > allmax,
          f"fresh={fresh} loaded_max={allmax}")


def run_probe(args, tmpdir: str, slot: str, adopt) -> int:
    """Every check in this probe, against a real engine rooted at
    `tmpdir`, answered as an exit status.

    `adopt` publishes the booted process to `main` the instant `boot`
    hands one over, because `main` -- not this function -- owns shutting
    the engine down and only then removing the tree it is running out
    of. Answering with a status instead of exiting is what lets that
    cleanup's own verdict override a passing run (#1912).
    """
    root = make_isolated_root(tmpdir)
    proc = boot(args.port, log=LOG, args=["--resource-root", root])
    adopt(proc)
    bootstrap_defs(args.port)
    send(args.port, f"world.init('{PAGE}', {args.seed}, {args.size}, 3); return 'ok'")
    send(args.port, "return world.waitForInit(180)", timeout=190)
    send(args.port, f"world.show('{PAGE}'); return 'ok'")
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
    # The technomule spawns with a PRE-STOCKED first_aid_kit, and since
    # #1418 EVERY creation path materializes a container def's authored
    # `contents:` -- `unit.addItem` included. So a second kit arrives
    # stocked too, and the two are genuinely INTERCHANGEABLE.
    #
    # That makes one shared contentsKey the CORRECT answer, not a
    # collapse: #67A's rule is that same-def containers merge until
    # their internal state diverges (itemContentsSig hashes each
    # child's defName/fill/quality/condition/weight/sharpness and
    # never the instance id or tracked temperature (#1597), so two
    # kits freshly minted from one definition MUST hash alike).
    # What has to separate them is a real divergence, so this
    # block draws bandages out of one kit through the shipped medical
    # path and re-reads both.
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
            send(args.port, f"return unit.addItem({muid}, 'first_aid_kit')")
            kits2 = [it for it in inventory(args.port, muid)
                     if it.get("defName") == "first_aid_kit"]
            keys2 = {(k.get("contentsKey") or "") for k in kits2}
            ids2 = [k["instanceId"] for k in kits2]
            check("unit.addItem now mints a STOCKED kit too (#1418)",
                  len(kits2) >= 2 and "" not in keys2,
                  f"keys={[(k['instanceId'], (k.get('contentsKey') or '')[:8]) for k in kits2]}")
            check("two IDENTICALLY stocked kits share ONE contentsKey -- "
                  "interchangeable until they diverge (#67A)",
                  len(kits2) >= 2 and len(keys2) == 1,
                  f"distinct keys={len(keys2)}")
            check("...while still being distinct physical items",
                  len(set(ids2)) == len(ids2) and len(ids2) >= 2,
                  f"ids={sorted(ids2)}")
            # The 1 L antiseptic bottle must report its FILLED mass
            # (0.12 empty + 1.0 L × 1.0 ≈ 1.12 kg), not the empty-bottle
            # def weight (0.12). Unconditional now: every kit is stocked,
            # so nothing can skip this by there being no empty twin --
            # and it runs BEFORE the treatment below, which spends a
            # dose of that very bottle.
            aw = content_weight(args.port, muid, "first_aid_kit",
                                stocked_id, "antiseptic")
            check("Contents weight includes bottle fill (~1.12, not 0.12)",
                  aw > 1.1, f"antiseptic weight={aw}")

            new_id = next((i for i in ids2 if i != stocked_id), None)
            # NB: `check` reports, it does not answer -- it returns
            # None, so it can never gate a block.
            check("the second kit is located by its own id",
                  new_id is not None, f"ids={sorted(ids2)}")
            if new_id is not None:
                rows = [contents_rows(args.port, muid, "first_aid_kit", i)
                        for i in (stocked_id, new_id)]
                check("getItemContents(either id) returns that kit's "
                      "supplies", rows[0] == rows[1] > 0, f"rows={rows}")
                before = [content_count(args.port, muid, "first_aid_kit",
                                        i, "bandage")
                          for i in (stocked_id, new_id)]
                check("both kits report the same authored bandage count",
                      before[0] == before[1] > 0, f"counts={before}")

                # -- The divergence, through the shipped path that
                #    actually draws a supply out of a kit: an acolyte
                #    (the unit that KNOWS bleed_control) dresses a
                #    wound, naming the mule as the explicit kit owner
                #    so the bandages come out of the pair above.
                #    consumeBandages spends from the FIRST carried kit
                #    holding any, which is the mule's starting one.
                #
                #    A SEPARATE acolyte carries the wound: the one
                #    under test above is the subject of the PERSIST
                #    block below, and leaving it bleeding would put a
                #    survival tick between that block and its own
                #    assertions.
                med = as_int(send(args.port,
                    f"return unit.spawn('acolyte', {gx}+0.5, {gy}+0.5,"
                    " nil, 'debug')"))
                check("a second acolyte is spawned to carry the wound",
                      med > 0, f"uid={med}")
                send(args.port,
                     f"return unit.injure({med}, 'head', 'cut', 0.4, 1.0)")
                used = as_int(send(args.port,
                    f"local r=unit.treatBleeding({med}, {med}, {muid}); "
                    "return (r and r.bandagesUsed) or 0"))
                check("treating a wound out of the mule's kit really "
                      "spent bandages", used > 0, f"bandagesUsed={used}")

                after = [content_count(args.port, muid, "first_aid_kit",
                                       i, "bandage")
                         for i in (stocked_id, new_id)]
                check("getItemContents(id) answers about the EXACT kit "
                      "asked for -- the drawn-from one lost supplies, "
                      "its twin lost none",
                      after[0] == before[0] - used and after[1] == before[1],
                      f"before={before} after={after} used={used}")
                kits3 = [it for it in inventory(args.port, muid)
                         if it.get("defName") == "first_aid_kit"]
                keys3 = {(k.get("contentsKey") or "") for k in kits3}
                check("a REAL divergence splits the contentsKey, so the "
                      "two kits stop merging in the UI",
                      len(keys3) == 2,
                      f"keys={[(k['instanceId'], (k.get('contentsKey') or '')[:8]) for k in kits3]}")
                check("the diverged kits keep their distinct instance ids",
                      sorted(k["instanceId"] for k in kits3)
                          == sorted(ids2),
                      f"{sorted(k['instanceId'] for k in kits3)} vs {sorted(ids2)}")

    if not args.no_save:
        persist_phase(args.port, uid, slot)

    return summarize()


def main() -> int:
    ap = argparse.ArgumentParser(description=__doc__)
    ap.add_argument("--seed", type=int, default=42)
    ap.add_argument("--size", type=int, default=64)
    ap.add_argument("--port", type=int, default=9171)
    ap.add_argument("--no-save", action="store_true")
    args = ap.parse_args()

    # The PERSIST phase saves and loads for real, so this run gets its
    # OWN resource root and its OWN per-invocation slot: nothing it
    # writes lands in the developer's live `saves/`, two concurrent runs
    # cannot collide, and the whole root (the only thing this probe
    # created) goes away below whether the run passes or fails. `proc`
    # is bound BEFORE the boundary because `boot` itself can `SystemExit`
    # -- the root must still be cleaned up when it does.
    tmpdir = tempfile.mkdtemp(prefix="item_instance_probe_")
    slot = f"issue67_probe_{os.getpid()}"
    proc = None

    def adopt(process) -> None:
        nonlocal proc
        proc = process

    try:
        rc = run_probe(args, tmpdir, slot, adopt)
    finally:
        # Shut the engine down BEFORE deleting the root it is running
        # out of, then remove it -- passing or failing, that leaves no
        # save artifact this run created behind. Nothing is returned
        # from here: an exception on the way out (a `SystemExit` from
        # `boot` included) keeps its own status rather than being
        # masked by the cleanup verdict, which only ever applies to a
        # run that actually reached an answer.
        if proc is not None:
            quit_engine(args.port, proc)
        cleaned = remove_run_root(tmpdir)
    # A run whose own tree survives is not a pass, whatever the checks
    # found: `remove_run_root` has already named the survivor (#1912).
    return rc if cleaned else 1


def summarize() -> int:
    passed = sum(1 for _, ok in CHECKS if ok)
    total = len(CHECKS)
    print(f"\n{passed}/{total} checks passed")
    return 0 if passed == total else 1


if __name__ == "__main__":
    sys.exit(main())
