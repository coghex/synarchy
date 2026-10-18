#!/usr/bin/env python3
"""Fresh-process gameplay-tag persistence probe (issue #2701, SCN-03 of
epic #2698; physical items added by #2703, SCN-04; terrain cells and
plants by #2705, SCN-05).

General gameplay tags on units, buildings and locations live with their
owners and ride the ordinary `units` (v5), `buildings` (v3) and
`world-pages` (v14) save components. The hspec group
`Gameplay.Tags.Persistence` proves the in-process contract; this probe
proves what only separate processes can: that saved assignments, with
their target identities, survive ordinary load -> save -> fresh-process
load for a GENERATED world and an ARENA, and through the AUTOSAVE path.

#2703 adds PHYSICAL-ITEM assignments, session-scoped and keyed by item
instance id (`core-session` v2): carried, nested, equipped, worn,
building-stored and ground items. Between the loads the generated leg
moves tagged items between owners (unit to unit, unit to ground),
consumes one and destroys a ground one through the real Lua verbs, and
every later generation must carry exactly the survivors' assignments,
with their ids, and nothing for the destroyed items.

#2705 adds terrain-CELL, flora-OCCURRENCE (wild and planted) and
groundcover-PLOT assignments, page-owned and keyed by canonical cell,
occurrence id and plot tile (`world-edits` v5). The place leg tags real
targets in a far chunk and near the camera, saves a fresh-process load
while the far chunk is ABSENT from the tile store (its assignments must
survive), readmits it, destroys a tagged top cell and a tagged plant's
tile through the real edit verb, and every later generation must carry
exactly the survivors' assignments, including after an autosave.

There is no public tag verb yet (SCN-06), so assignments are seeded the
only way that exists: `exe:synarchy-save-codec seed-tags`, which decodes
a real engine save through the real session codec, sets the assignments
and re-encodes it through the same encoder an ordinary save uses. Every
comparison reads the compiled codec's canonical summary
(`dump_canonical_summary`, whose `gameplayTags` lists each page's
assignments by target id), never Lua.

Flow, all in one isolated resource root (no scenario file exists in it,
which the probe asserts, so nothing here can come from scenario YAML):

  Generated-world leg
    A  boot, world.init a small real world, spawn two units and one
       building, save 'base'.
       Seed 'base' twice: 'seeded' (nonempty unit/building/location
       assignments, two units sharing a tag, PLUS a dangling unit and a
       dangling location assignment) and 'outgoing' (different tags).
    B  fresh process: load 'outgoing', then load 'seeded' over it, save
       'gen1'. The outgoing session's tags must not survive the
       replacement, and the load must report and drop both dangling
       assignments.
    C  fresh process: load 'gen1', save 'gen2', then run one real
       autosave cycle (engine.prepareAutosaveCycle -> engine.saveWorld
       {autosave = true} -> engine.finalizeAutosaveRotation), with no
       interval dwell, producing 'autosave-1'.
    D  fresh process: load 'autosave-1', save 'gen3'.
    Every generation's assignments equal the seeded ones minus the two
    dangling entries.

  Arena leg
    E  boot, world.initArena, spawn a unit and a building, save
       'arena0'; seed it as 'arena_seeded'.
    F  fresh process: load 'arena_seeded', save 'arena1'.
    G  fresh process: load 'arena1', save 'arena2'.
    Both generations carry exactly the seeded assignments.

Usage:
  python3 tools/gameplay_tag_persistence_probe.py [--port 9282]
"""
from __future__ import annotations

import argparse
import glob
import json
import os
import shutil
import sys
import tempfile
import time
from pathlib import Path

from probelib import (boot, quit_engine, send, send_json, wait_load_published,
                      wait_save_complete, capture_request_id)
from persistence_snapshot import prepare_decoder
from save_compat_audit_codec import dump_canonical_summary, seed_fixture_tags

REPO = Path(__file__).resolve().parent.parent

PAGE = "tag_page"
ARENA = "tag_arena"
SEED = 42
WORLD_SIZE = 16
PLATE_COUNT = 3
AUTOSAVE_DEPTH = 3


class Checks:
    def __init__(self) -> None:
        self.failed = 0

    def ok(self, cond: bool, label: str) -> None:
        print(f"  [{'PASS' if cond else 'FAIL'}] {label}")
        if not cond:
            self.failed += 1


def make_isolated_root(base: str) -> str:
    """Real read-only content symlinked in, plus this probe's OWN empty
    saves/ (the developer's real saves/ are never touched)."""
    root = os.path.join(base, "root")
    os.makedirs(root, exist_ok=True)
    for family in ("scripts", "assets", "data", "config"):
        target = os.path.join(root, family)
        if not os.path.exists(target):
            os.symlink(os.path.join(REPO, family), target)
    os.makedirs(os.path.join(root, "saves"), exist_ok=True)
    return root


def scenario_files(root: str) -> list[str]:
    """Every file under the root whose name says it is a scenario. The
    probe requires none: restored tags must come from the save alone."""
    found = []
    for dirpath, _dirs, files in os.walk(root, followlinks=True):
        if "saves" in Path(dirpath).relative_to(root).parts[:1]:
            continue
        found += [os.path.join(dirpath, f) for f in files
                  if "scenario" in f.lower()]
    return found


def bootstrap_defs(port: int) -> None:
    loaders = [
        ("data/substances/*.yaml", "engine.loadSubstanceYaml"),
        ("data/items/*.yaml",      "engine.loadItemYaml"),
        ("data/equipment/*.yaml",  "engine.loadEquipmentYaml"),
        ("data/materials/*.yaml",  "engine.loadMaterialYaml"),
        ("data/factions/*.yaml",   "engine.loadFactionYaml"),
        ("data/physiology/*.yaml", "engine.loadPhysiologyYaml"),
        ("data/units/*.yaml",      "engine.loadUnitYaml"),
        ("data/buildings/*.yaml",  "engine.loadBuildingYaml"),
        # #2705: the place leg tags real flora and plants a real crop.
        ("data/flora/*.yaml",      "engine.loadFloraYaml"),
    ]
    for pattern, fn in loaders:
        for path in sorted(glob.glob(str(REPO / pattern))):
            rel = os.path.relpath(path, REPO)
            send(port, f"{fn}('{rel}'); return 'ok'")
    # Locations are placed AT world.init from the registry, so they must
    # be registered first or the generated page carries no location to
    # tag (mirrors save_compat_audit_generate's _bootstrap_gen_defs).
    for path in sorted(glob.glob(str(REPO / "data/locations/*.yaml"))):
        rel = os.path.relpath(path, REPO)
        send(port, f"engine.loadLocationYaml('{rel}'); return 'ok'")


def as_int(s: str):
    try:
        return int(float(s))
    except (TypeError, ValueError):
        return None


def slot_path(root: str, slot: str) -> str:
    return os.path.join(root, "saves", slot, "world.synworld")


def save_and_wait(chk: Checks, port: int, page: str, slot: str,
                  options: str = "") -> bool:
    arg = f", {options}" if options else ""
    saved = send(port, f"return engine.saveWorld('{page}', '{slot}'{arg})")
    chk.ok(saved.strip() == "true", f"engine.saveWorld('{slot}') accepted (got {saved!r})")
    request_id = capture_request_id(port, "return engine.getSaveStatus()")
    ok, status = wait_save_complete(port, request_id)
    chk.ok(ok, f"save '{slot}' (request {request_id}) completed (got {status!r})")
    return ok


def load_and_wait(chk: Checks, port: int, slot: str) -> bool:
    loaded = send(port, f"return engine.loadSave('{slot}')")
    chk.ok(loaded.strip() == "true", f"engine.loadSave('{slot}') accepted (got {loaded!r})")
    load_id = capture_request_id(port, "return engine.getLoadStatus()")
    published, status = wait_load_published(port, request_id=load_id)
    chk.ok(published, f"load of '{slot}' (request {load_id}) published (status={status})")
    return published


def show_and_wait(chk: Checks, port: int, page: str) -> None:
    send(port, f"world.show('{page}'); return 'ok'")
    deadline = time.time() + 30.0
    while time.time() < deadline:
        if send(port, "return world.getActiveWorldId()").strip('"') == page:
            return
        time.sleep(0.2)
    chk.ok(False, f"'{page}' never became the active world")


def seed(chk: Checks, root: str, tmp: str, src_slot: str, dst_slot: str,
         spec: dict) -> None:
    spec_path = Path(tmp) / f"{dst_slot}.seed.json"
    spec_path.write_text(json.dumps(spec))
    os.makedirs(os.path.dirname(slot_path(root, dst_slot)), exist_ok=True)
    ok, tail = seed_fixture_tags(Path(slot_path(root, src_slot)), spec_path,
                                 Path(slot_path(root, dst_slot)))
    chk.ok(ok, f"seeded '{dst_slot}' from '{src_slot}' through the real codec"
               + ("" if ok else f": {tail}"))


def page_tags(chk: Checks, tmp: str, root: str, slot: str, page: str) -> dict:
    """One page's canonical assignments, as the compiled codec reads them
    off the save on disk: {"units": {id: [tags]}, ...}."""
    out = Path(tmp) / f"{slot}.summary.json"
    ok, tail = dump_canonical_summary(Path(slot_path(root, slot)), out)
    chk.ok(ok, f"canonical summary of '{slot}'" + ("" if ok else f": {tail}"))
    if not ok:
        return {}
    for p in json.loads(out.read_text()).get("pages", []):
        if p.get("pageId") == page:
            tags = p.get("gameplayTags", {})
            return {cat: {e["id"]: e["tags"] for e in tags.get(cat, [])}
                    for cat in ("units", "buildings", "locations")}
    chk.ok(False, f"'{slot}' has no page '{page}'")
    return {}


def item_tags(chk: Checks, tmp: str, root: str, slot: str) -> dict:
    """The session's item assignments (#2703), as the compiled codec
    reads them off the save on disk: {instance id: [tags]}."""
    out = Path(tmp) / f"{slot}.items.json"
    ok, tail = dump_canonical_summary(Path(slot_path(root, slot)), out)
    chk.ok(ok, f"canonical summary of '{slot}'" + ("" if ok else f": {tail}"))
    if not ok:
        return {}
    summary = json.loads(out.read_text())
    return {e["id"]: e["tags"] for e in summary.get("itemTags", [])}


def summary_inventory(tmp: str, root: str, slot: str, page: str, uid: int) -> list:
    """One unit's inventory rows (instanceId, defName, contents) off the
    save's canonical summary."""
    out = Path(tmp) / f"{slot}.inv.json"
    ok, _tail = dump_canonical_summary(Path(slot_path(root, slot)), out)
    if not ok:
        return []
    for p in json.loads(out.read_text()).get("pages", []):
        if p.get("pageId") == page:
            for u in p.get("units", []):
                if u.get("id") == uid:
                    return u.get("inventory", [])
    return []


def first_id(rows: list, def_name: str, nth: int = 0):
    ids = [r["instanceId"] for r in rows if r.get("defName") == def_name]
    return ids[nth] if len(ids) > nth else None


def lua_rows(port: int, expr: str) -> list:
    """[(instanceId, defName, extra)] from a Lua array of item tables,
    flattened to a string so an empty array is unambiguous."""
    raw = send(port, "local out = {} for _, r in ipairs((" + expr + ") or {}) do "
                     "out[#out + 1] = tostring(r.instanceId) .. ':' .. tostring(r.defName) "
                     ".. ':' .. tostring(r.id) end return table.concat(out, ';')")
    rows = []
    for part in raw.strip('"').split(";"):
        bits = part.split(":")
        if len(bits) == 3 and as_int(bits[0]) is not None:
            rows.append((as_int(bits[0]), bits[1], as_int(bits[2])))
    return rows


def spec_tags(spec: dict, page: str) -> dict:
    return {cat: {e["id"]: sorted(e["tags"]) for e in spec.get(cat, [])
                  if e["page"] == page and e["tags"]}
            for cat in ("units", "buildings", "locations")}


def all_tag_names(tags: dict) -> set[str]:
    return {t for cat in tags.values() for ts in cat.values() for t in ts}


def generated_leg(chk: Checks, port: int, root: str, tmp: str) -> None:
    print("=== engine A: generated world, two units and a building, save 'base' ===")
    proc = boot(port, log=os.path.join(tmp, "engineA.log"),
                args=["--resource-root", root], ready_timeout=180)
    try:
        bootstrap_defs(port)
        inited = send(port, f"world.init('{PAGE}', {SEED}, {WORLD_SIZE}, "
                            f"{PLATE_COUNT}); return 'ok'")
        chk.ok("ok" in inited, f"world.init accepted (got {inited!r})")
        show_and_wait(chk, port, PAGE)
        send(port, "return world.waitForInit(60)", timeout=70)
        bid = as_int(send(port, "return building.spawn('cargo_hold_S', 0, 0)"))
        u1 = as_int(send(port, "return unit.spawn('acolyte', 0, 0, 0, 'player')"))
        u2 = as_int(send(port, "return unit.spawn('acolyte', 3, 2, 0, 'player')"))
        chk.ok(None not in (bid, u1, u2) and min(bid, u1, u2) >= 0,
               f"spawned building {bid} and units {u1}, {u2}")
        # A REAL placed instance to tag: its instance id, read back off the
        # page's own table (a "~= nil" on a lookup reply cannot tell a
        # missing location from a found one).
        lid = None
        deadline = time.time() + 30.0
        while time.time() < deadline and lid is None:
            lid = as_int(send(port, f"local ls = world.listPlacedLocations('{PAGE}') "
                                    f"or {{}}; return ls[1] and ls[1].instance_id"))
            if lid is None:
                time.sleep(0.5)
        chk.ok(lid is not None and lid >= 1,
               f"page '{PAGE}' has a placed location to tag (instance {lid})")
        # #2703: items in every owner category. A kit brings nested
        # contents; the radio goes into the cargo hold's storage; a
        # ration lies on the ground; the dagger is unequipped and
        # re-equipped so its id is readable, and stays equipped.
        chk.ok(send(port, f"return unit.addItem({u1}, 'first_aid_kit', 0)").strip() == "true",
               "the acolyte received a first-aid kit with its default contents")
        chk.ok(send(port, f"return unit.depositToCargo({u1}, {bid}, 'radio')").strip() == "true",
               "the radio was deposited into the cargo hold's storage")
        chk.ok(send(port, f"return equipment.unequip({u1}, 'right_hand')").strip() == "true",
               "the dagger was unequipped")
        inv0 = lua_rows(port, f"unit.getInventory({u1})")
        dagger = first_id([{"instanceId": i, "defName": d} for i, d, _ in inv0], "steel_dagger")
        chk.ok(send(port, f"return equipment.equip({u1}, 'right_hand', 'steel_dagger')").strip() == "true",
               "the dagger was re-equipped")
        gid = as_int(send(port, f"return item.spawnGround('rations', 1.5, 1.5, nil, '{PAGE}')"))
        ground = [r for r in lua_rows(port, "item.listGround()") if r[2] == gid]
        ground_id = ground[0][0] if ground else None
        worn = lua_rows(port, f"equipment.getAccessories({u1})")
        stored = lua_rows(port, f"building.getStorage({bid})")
        save_and_wait(chk, port, PAGE, "base")
    finally:
        quit_engine(port, proc)

    inv = summary_inventory(tmp, root, "base", PAGE, u1)
    kit = first_id(inv, "first_aid_kit")
    kit_row = next((r for r in inv if r.get("instanceId") == kit), {})
    bandage = first_id(kit_row.get("contents", []), "bandage")
    items = {
        "rationsA": first_id(inv, "rations", 0), "rationsB": first_id(inv, "rations", 1),
        "pick": first_id(inv, "pick_steel"), "axe": first_id(inv, "axe_steel"),
        "kit": kit, "bandage": bandage, "dagger": dagger,
        "worn": worn[0][0] if worn else None,
        "stored": first_id([{"instanceId": i, "defName": d} for i, d, _ in stored], "radio"),
        "ground": ground_id,
    }
    chk.ok(all(v is not None for v in items.values()),
           f"every owner category holds an item to tag: {items}")

    seeded = {
        "units": [{"page": PAGE, "id": u1, "tags": ["defender", "meeting_point"]},
                  {"page": PAGE, "id": u2, "tags": ["defender"]},
                  {"page": PAGE, "id": 9999, "tags": ["ghost"]}],
        "buildings": [{"page": PAGE, "id": bid, "tags": ["depot"]}],
        "locations": [{"page": PAGE, "id": lid, "tags": ["landmark"]},
                      {"page": PAGE, "id": 777, "tags": ["ghost"]}],
        "items": [{"id": items["rationsA"], "tags": ["food", "consumed_later"]},
                  {"id": items["rationsB"], "tags": ["food"]},
                  {"id": items["pick"], "tags": ["tool", "moved_to_unit"]},
                  {"id": items["axe"], "tags": ["tool", "dropped"]},
                  {"id": items["kit"], "tags": ["medical"]},
                  {"id": items["bandage"], "tags": ["medical", "nested"]},
                  {"id": items["dagger"], "tags": ["weapon"]},
                  {"id": items["worn"], "tags": ["worn"]},
                  {"id": items["stored"], "tags": ["stored"]},
                  {"id": items["ground"], "tags": ["ground", "destroyed_later"]},
                  {"id": 99999, "tags": ["ghost"]}],
    }
    outgoing = {
        "units": [{"page": PAGE, "id": u1, "tags": ["outgoing"]}],
        "buildings": [{"page": PAGE, "id": bid, "tags": ["outgoing"]}],
        "locations": [{"page": PAGE, "id": lid, "tags": ["outgoing"]}],
        "items": [{"id": items["rationsB"], "tags": ["outgoing"]},
                  {"id": items["kit"], "tags": ["outgoing"]}],
    }
    seed(chk, root, tmp, "base", "seeded", seeded)
    seed(chk, root, tmp, "base", "outgoing", outgoing)
    expected = spec_tags(seeded, PAGE)
    expected["units"].pop(9999)
    expected["locations"].pop(777)
    chk.ok(all(expected[c] for c in expected),
           f"the expected assignments cover every category: {expected}")
    as_written = page_tags(chk, tmp, root, "seeded", PAGE)
    chk.ok(as_written.get("units", {}).get(9999) == ["ghost"],
           "the seeded save really carries the dangling unit assignment")
    seeded_items = item_tags(chk, tmp, root, "seeded")
    chk.ok(seeded_items.get(99999) == ["ghost"] and len(seeded_items) == 11,
           f"the seeded save carries all eleven item assignments, the dangling "
           f"one included (got {seeded_items})")
    # Survivors after engine B's changes: the consumed ration and the
    # destroyed ground ration lose theirs; the dangling one is dropped.
    gone = {items["rationsA"], items["ground"], 99999}
    expected_items = {e["id"]: sorted(e["tags"]) for e in seeded["items"]
                      if e["id"] not in gone}

    print("=== engine B: fresh process, load 'outgoing', load 'seeded' over it, save 'gen1' ===")
    log_b = os.path.join(tmp, "engineB.log")
    proc = boot(port, log=log_b, args=["--resource-root", root], ready_timeout=180)
    try:
        bootstrap_defs(port)
        load_and_wait(chk, port, "outgoing")
        load_and_wait(chk, port, "seeded")
        # #2703: ownership changes and cleanup through the real verbs.
        moved = send(port, f"return unit.transferItemToUnit({u1}, {u2}, 'pick_steel', "
                           f"{items['pick']})")
        chk.ok(moved.strip() == "true", f"the tagged pick moved unit to unit (got {moved!r})")
        dropped_axe = send(port, f"return unit.dropItemById({u1}, {items['axe']})")
        chk.ok(dropped_axe.strip() == "true", f"the tagged axe was dropped (got {dropped_axe!r})")
        eaten = send(port, f"return unit.removeItem({u1}, 'rations')")
        chk.ok(eaten.strip() == "true", f"the first tagged ration was consumed (got {eaten!r})")
        row = [r for r in lua_rows(port, "item.listGround()") if r[0] == items["ground"]]
        removed = send(port, f"return item.removeGround({row[0][2]})") if row else "no row"
        chk.ok(removed.strip() == "true",
               f"the tagged ground ration was destroyed (got {removed!r})")
        save_and_wait(chk, port, PAGE, "gen1")
    finally:
        quit_engine(port, proc)
    engine_log = Path(log_b).read_text(errors="replace")
    dropped = [line for line in engine_log.splitlines()
               if "dropped gameplay tags" in line]
    chk.ok(any("unit id" in line and "9999" in line for line in dropped)
           and any("location id" in line and "777" in line for line in dropped)
           and any("item id" in line and "99999" in line for line in dropped),
           f"the load reported dropping the dangling unit, location and item "
           f"assignments (diagnostics: {dropped})")
    moved_to = [r.get("instanceId") for r in summary_inventory(tmp, root, "gen1", PAGE, u2)]
    chk.ok(items["pick"] in moved_to,
           f"'gen1' holds the tagged pick in the second unit's inventory (got {moved_to})")

    print("=== engine C: fresh process, load 'gen1', save 'gen2', one autosave cycle ===")
    proc = boot(port, log=os.path.join(tmp, "engineC.log"),
                args=["--resource-root", root], ready_timeout=180)
    try:
        bootstrap_defs(port)
        load_and_wait(chk, port, "gen1")
        save_and_wait(chk, port, PAGE, "gen2")
        prepared = send(port, f"return engine.prepareAutosaveCycle({AUTOSAVE_DEPTH})")
        chk.ok(prepared.strip() == "true",
               f"engine.prepareAutosaveCycle accepted (got {prepared!r})")
        save_and_wait(chk, port, PAGE, "autosave-incoming", "{ autosave = true }")
        rotated = send(port, f"return engine.finalizeAutosaveRotation({AUTOSAVE_DEPTH})")
        chk.ok(rotated.strip() == "true",
               f"engine.finalizeAutosaveRotation accepted (got {rotated!r})")
        listing = send_json(port, "return engine.listSaves()")
        entry = next((s for s in listing if s.get("name") == "autosave-1"), None) \
            if isinstance(listing, list) else None
        chk.ok(bool(entry) and entry.get("autosave") is True,
               f"'autosave-1' is listed as an autosave (got {entry!r})")
    finally:
        quit_engine(port, proc)

    print("=== engine D: fresh process, load 'autosave-1', save 'gen3' ===")
    proc = boot(port, log=os.path.join(tmp, "engineD.log"),
                args=["--resource-root", root], ready_timeout=180)
    try:
        bootstrap_defs(port)
        load_and_wait(chk, port, "autosave-1")
        save_and_wait(chk, port, PAGE, "gen3")
    finally:
        quit_engine(port, proc)

    for slot in ("gen1", "gen2", "autosave-1", "gen3"):
        got = page_tags(chk, tmp, root, slot, PAGE)
        chk.ok(got == expected,
               f"'{slot}' carries exactly the saved assignments and targets "
               f"(got {got}, expected {expected})")
        chk.ok(not (all_tag_names(got) & {"outgoing", "ghost"}),
               f"'{slot}' carries no outgoing-session and no dangling tag")
        got_items = {i: sorted(ts) for i, ts in item_tags(chk, tmp, root, slot).items()}
        chk.ok(got_items == expected_items,
               f"'{slot}' carries exactly the surviving items' assignments, by id "
               f"(got {got_items}, expected {expected_items})")


def arena_leg(chk: Checks, port: int, root: str, tmp: str) -> None:
    print("=== engine E: arena, a unit and a building, save 'arena0' ===")
    proc = boot(port, log=os.path.join(tmp, "engineE.log"),
                args=["--resource-root", root], ready_timeout=180)
    try:
        bootstrap_defs(port)
        send(port, f"world.initArena('{ARENA}'); return 'ok'")
        send(port, "return world.waitForInit(60)", timeout=70)
        show_and_wait(chk, port, ARENA)
        bid = as_int(send(port, "return building.spawn('cargo_hold_S', 1, 1)"))
        uid = as_int(send(port, "return unit.spawn('acolyte', 2, 2, 0, 'player')"))
        chk.ok(None not in (bid, uid) and min(bid, uid) >= 0,
               f"spawned arena building {bid} and unit {uid}")
        time.sleep(1.0)
        save_and_wait(chk, port, ARENA, "arena0")
    finally:
        quit_engine(port, proc)

    arena_inv = summary_inventory(tmp, root, "arena0", ARENA, uid)
    arena_items = [r["instanceId"] for r in arena_inv[:2]]
    chk.ok(len(arena_items) == 2, f"the arena unit carries items to tag (got {arena_items})")
    spec = {"units": [{"page": ARENA, "id": uid, "tags": ["guard", "shared"]}],
            "buildings": [{"page": ARENA, "id": bid, "tags": ["shared"]}],
            "items": [{"id": i, "tags": ["shared", f"arena_item_{n}"]}
                      for n, i in enumerate(arena_items)]}
    seed(chk, root, tmp, "arena0", "arena_seeded", spec)
    expected = spec_tags(spec, ARENA)
    expected_items = {e["id"]: sorted(e["tags"]) for e in spec["items"]}

    prev = "arena_seeded"
    for letter, nxt in (("F", "arena1"), ("G", "arena2")):
        print(f"=== engine {letter}: fresh process, load '{prev}', save '{nxt}' ===")
        proc = boot(port, log=os.path.join(tmp, f"engine{letter}.log"),
                    args=["--resource-root", root], ready_timeout=180)
        try:
            bootstrap_defs(port)
            load_and_wait(chk, port, prev)
            save_and_wait(chk, port, ARENA, nxt)
        finally:
            quit_engine(port, proc)
        got = page_tags(chk, tmp, root, nxt, ARENA)
        chk.ok(got == expected,
               f"'{nxt}' carries exactly the arena's saved assignments "
               f"(got {got}, expected {expected})")
        got_items = {i: sorted(ts) for i, ts in item_tags(chk, tmp, root, nxt).items()}
        chk.ok(got_items == expected_items,
               f"'{nxt}' carries exactly the arena's item assignments "
               f"(got {got_items}, expected {expected_items})")
        prev = nxt


PLACE = "tag_place"


def place_tags(chk: Checks, tmp: str, root: str, slot: str) -> dict:
    """The place page's cell / plant / plot assignments off the save:
    {"tiles": {(x, y, z): tags}, "plants": {id: (tile, tags)},
    "plots": {(x, y): tags}}."""
    out = Path(tmp) / f"{slot}.place.json"
    ok, tail = dump_canonical_summary(Path(slot_path(root, slot)), out)
    chk.ok(ok, f"canonical summary of '{slot}'" + ("" if ok else f": {tail}"))
    if not ok:
        return {}
    for p in json.loads(out.read_text()).get("pages", []):
        if p.get("pageId") == PLACE:
            g = p.get("gameplayTags", {})
            return {
                "tiles": {(e["x"], e["y"], e["z"]): sorted(e["tags"]) for e in g.get("tiles", [])},
                "plants": {e["id"]: (tuple(e["tile"]) if e.get("tile") else None, sorted(e["tags"]))
                           for e in g.get("plants", [])},
                "plots": {(e["x"], e["y"]): sorted(e["tags"]) for e in g.get("plots", [])},
            }
    chk.ok(False, f"'{slot}' has no page '{PLACE}'")
    return {}


def surface_terrain_z(port: int, gx: int, gy: int):
    r = send(port, f"local sz, tz = world.getSurfaceAt({gx}, {gy}); return tostring(tz)")
    return as_int(r.strip('"'))


def chunk_loaded(port: int, cx: int, cy: int) -> bool:
    r = send(port, f"local i = world.getChunkInfo({cx}, {cy}); return tostring(i ~= nil and i.loaded == true)")
    return r.strip('"') == "true"


def find_in_chunks(port: int, chunks: list, want_flora: bool, exclude: set):
    """The first suitable tile in these chunks: a dry, real-terrain column
    with no flora (want_flora False), or one carrying a flora occurrence
    (want_flora True). Answers ((gx, gy), terrain z, occurrence id or
    None), or None. A sentinel (void) column is never suitable."""
    cs = ",".join(f"{{{cx},{cy}}}" for cx, cy in chunks)
    ex = " or ".join(f"(gx=={a} and gy=={b})" for a, b in exclude) or "false"
    cond = "f and f.instanceId" if want_flora else "not f"
    r = send(port, f"for _, c in ipairs({{{cs}}}) do "
                   f"for lx=0,15 do for ly=0,15 do "
                   f"local gx, gy = c[1]*16+lx, c[2]*16+ly; "
                   f"if not ({ex}) then "
                   f"local sz, tz, ft = world.getSurfaceAt(gx, gy); "
                   f"local f = world.getFloraAt(gx, gy); "
                   f"if sz and tz and tz > -100000 and not ft and {cond} then "
                   f"return gx..','..gy..','..tz..','..tostring(f and f.instanceId or 0) end "
                   f"end end end end return 'none'",
             timeout=120.0).strip('"')
    if r == "none":
        return None
    gx, gy, tz, iid = r.split(",")
    return (int(gx), int(gy)), int(tz), (int(iid) or None)


def ring(r: int) -> list:
    return [(cx, cy) for cx in range(-r, r + 1) for cy in range(-r, r + 1)
            if max(abs(cx), abs(cy)) == r]


def place_leg(chk: Checks, port: int, root: str, tmp: str) -> None:
    print("=== engine P1: tagged cells, plants and a plot in a far chunk and near the camera ===")
    proc = boot(port, log=os.path.join(tmp, "engineP1.log"),
                args=["--resource-root", root], ready_timeout=180)
    far_chunk = None
    far_row = None
    try:
        bootstrap_defs(port)
        send(port, f"world.init('{PLACE}', {SEED}, {WORLD_SIZE}, {PLATE_COUNT}); return 'ok'")
        show_and_wait(chk, port, PLACE)
        send(port, "return world.waitForInit(60)", timeout=70)
        send(port, f"return world.loadChunksInRegion(-5, -5, 5, 5, '{PLACE}')")
        send(port, "return world.waitForChunks(240)", timeout=250)
        # The FAR targets: a dry column and a plant in one chunk on the
        # outer ring, which a fresh-process load leaves absent; the NEAR
        # ones anywhere nearer. Searched rather than assumed: a seed's
        # coast and void edge decide where land is.
        far_col = far_plant = None
        for r in (5, 4):
            for c in ring(r):
                col = find_in_chunks(port, [c], False, set())
                pl = find_in_chunks(port, [c], True, set()) if col else None
                if col and pl:
                    far_chunk, far_col, far_plant = c, col, pl
                    break
            if far_chunk:
                break
        inner = [c for r in (0, 1, 2) for c in ring(r)]
        near_col = find_in_chunks(port, inner, False, set())
        near_plant = find_in_chunks(port, inner, True, set())
        plot_col = find_in_chunks(port, inner, False, {near_col[0]} if near_col else set())
        chk.ok(None not in (far_chunk, near_col, near_plant, plot_col),
               f"found real targets: far chunk {far_chunk} column {far_col} plant {far_plant}, "
               f"near column {near_col}, near plant {near_plant}, plot tile {plot_col}")
        if None in (far_chunk, near_col, near_plant, plot_col):
            return
        # A planted ROW CROP in the far chunk too: tilled, planted through
        # the farm verb's WePlaceFlora edit, so readmission rebuilds it by
        # the production replay of that edit.
        row_col = find_in_chunks(port, [far_chunk], False, {far_col[0]})
        far_row = None
        if row_col:
            (rx, ry), rz = row_col[0], row_col[1]
            send(port, f"world.setVegAt('{PLACE}', {rx}, {ry}, {rz}, 77); return 'ok'")
            deadline = time.time() + 20.0
            while time.time() < deadline and send(port, f"return world.isPlantable({rx},{ry})").strip() != "true":
                time.sleep(0.2)
            send(port, f"return world.plantRowCropAt('{PLACE}', {rx}, {ry}, 'tomato_plant')")
            deadline = time.time() + 20.0
            while time.time() < deadline and far_row is None:
                r = send(port, f"local f = world.getFloraAt({rx}, {ry}); "
                               f"return tostring(f and f.instanceId)").strip('"')
                far_row = ((rx, ry), as_int(r)) if as_int(r) else None
                if far_row is None:
                    time.sleep(0.2)
        chk.ok(far_row is not None, f"a row crop was planted in the far chunk (got {far_row})")
        if far_row is None:
            return
        far, far_z = far_col[0], far_col[1]
        fpt, fpid = far_plant[0], far_plant[2]
        near = (near_col[0][0], near_col[0][1], near_col[1])
        npt, npid = near_plant[0], near_plant[2]
        plot = (plot_col[0][0], plot_col[0][1], plot_col[1])
        send(port, f"world.setVegAt('{PLACE}', {plot[0]}, {plot[1]}, {plot[2]}, 77); return 'ok'")
        deadline = time.time() + 20.0
        while time.time() < deadline and send(port, f"return world.isPlantable({plot[0]},{plot[1]})").strip() != "true":
            time.sleep(0.2)
        planted = send(port, f"return world.plantCropAt({plot[0]}, {plot[1]}, 'wheat')")
        chk.ok(planted.strip() == "true", f"a wheat plot was planted (got {planted!r})")
        save_and_wait(chk, port, PLACE, "place_base")
    finally:
        quit_engine(port, proc)

    if far_chunk is None:
        return
    fcx, fcy = far_chunk
    (rpt, rpid) = far_row
    spec = {
        "tiles": [{"page": PLACE, "x": far[0], "y": far[1], "z": far_z, "tags": ["far_top", "shared"]},
                  # Not a target: air above the far column. Kept while its
                  # chunk is absent (absence is not destruction), then
                  # dropped, and logged, by the real readmission.
                  {"page": PLACE, "x": far[0], "y": far[1], "z": far_z + 3, "tags": ["far_air"]},
                  {"page": PLACE, "x": near[0], "y": near[1], "z": near[2], "tags": ["dug_away", "shared"]},
                  {"page": PLACE, "x": near[0], "y": near[1], "z": near[2] - 1, "tags": ["exposed"]}],
        "plants": [{"page": PLACE, "x": fpt[0], "y": fpt[1], "id": fpid, "tags": ["far_plant", "shared"]},
                   {"page": PLACE, "x": rpt[0], "y": rpt[1], "id": rpid, "tags": ["far_row_crop"]},
                   {"page": PLACE, "x": npt[0], "y": npt[1], "id": npid, "tags": ["uprooted"]}],
        "plots": [{"page": PLACE, "x": plot[0], "y": plot[1], "tags": ["field"]},
                  {"page": PLACE, "x": plot[0] + 1, "y": plot[1] + 7, "tags": ["ghost"]}],
    }
    seed(chk, root, tmp, "place_base", "place_seeded", spec)
    seeded = place_tags(chk, tmp, root, "place_seeded")
    chk.ok(len(seeded.get("tiles", {})) == 4 and len(seeded.get("plants", {})) == 3
           and len(seeded.get("plots", {})) == 2,
           f"the seeded save carries every cell, plant and plot assignment (got {seeded})")
    kept = {"tiles": {k: v for k, v in seeded.get("tiles", {}).items()},
            "plants": dict(seeded.get("plants", {})),
            "plots": {k: v for k, v in seeded.get("plots", {}).items() if "ghost" not in v}}
    after_dig = {"tiles": {k: v for k, v in kept["tiles"].items()
                           if "dug_away" not in v and "far_air" not in v},
                 "plants": {k: v for k, v in kept["plants"].items() if "uprooted" not in v[1]},
                 "plots": kept["plots"]}

    print("=== engine P2: fresh process, load, save while the far chunk is ABSENT ===")
    log_p2 = os.path.join(tmp, "engineP2.log")
    proc = boot(port, log=log_p2, args=["--resource-root", root], ready_timeout=180)
    try:
        bootstrap_defs(port)
        load_and_wait(chk, port, "place_seeded")
        absent = not chunk_loaded(port, fcx, fcy)
        chk.ok(absent, f"the far chunk {far_chunk} is absent from the tile store at save time")
        save_and_wait(chk, port, PLACE, "place_gen1")
    finally:
        quit_engine(port, proc)
    p2 = Path(log_p2).read_text(errors="replace")
    chk.ok("crop plot tile(s) that hold no plot" in p2,
           "the load reported dropping the plot assignment that names no plot")
    got = place_tags(chk, tmp, root, "place_gen1")
    chk.ok(got == kept, f"'place_gen1' (saved with the far chunk absent) keeps every real "
                        f"assignment (got {got}, expected {kept})")

    print("=== engine P3: fresh process, readmit the far chunk, dig, save, autosave ===")
    log_p3 = os.path.join(tmp, "engineP3.log")
    proc = boot(port, log=log_p3,
                args=["--resource-root", root], ready_timeout=180)
    try:
        bootstrap_defs(port)
        load_and_wait(chk, port, "place_gen1")
        send(port, f"return world.loadChunksInRegion({fcx}, {fcy}, {fcx}, {fcy}, '{PLACE}')")
        # The near targets' chunks, which a fresh load need not hold.
        for gx, gy in (near[:2], npt):
            cx, cy = gx // 16, gy // 16
            send(port, f"return world.loadChunksInRegion({cx}, {cy}, {cx}, {cy}, '{PLACE}')")
        send(port, "return world.waitForChunks(120)", timeout=125)
        chk.ok(chunk_loaded(port, fcx, fcy), f"the far chunk {far_chunk} was readmitted")
        for (gx, gy), want in ((near[:2], near[2] - 1), (npt, None)):
            before = surface_terrain_z(port, gx, gy)
            send(port, f"return world.deleteTile('{PLACE}', {gx}, {gy})")
            target = want if want is not None else (before - 1 if before is not None else None)
            deadline = time.time() + 20.0
            while time.time() < deadline and surface_terrain_z(port, gx, gy) != target:
                time.sleep(0.2)
            chk.ok(surface_terrain_z(port, gx, gy) == target,
                   f"the top cell at ({gx}, {gy}) was dug away (terrain z now {target})")
        save_and_wait(chk, port, PLACE, "place_gen2")
        prepared = send(port, f"return engine.prepareAutosaveCycle({AUTOSAVE_DEPTH})")
        chk.ok(prepared.strip() == "true", "engine.prepareAutosaveCycle accepted")
        save_and_wait(chk, port, PLACE, "autosave-incoming", "{ autosave = true }")
        rotated = send(port, f"return engine.finalizeAutosaveRotation({AUTOSAVE_DEPTH})")
        chk.ok(rotated.strip() == "true", "engine.finalizeAutosaveRotation accepted")
    finally:
        quit_engine(port, proc)

    p3 = Path(log_p3).read_text(errors="replace")
    air = f"({far[0]},{far[1]},{far_z + 3})"
    chk.ok(any("dropped tags on" in line and air in line for line in p3.splitlines()),
           f"the readmission of the far chunk reported dropping the non-target cell {air}")
    print("=== engine P4: fresh process, load the autosave, save ===")
    proc = boot(port, log=os.path.join(tmp, "engineP4.log"),
                args=["--resource-root", root], ready_timeout=180)
    try:
        bootstrap_defs(port)
        load_and_wait(chk, port, "autosave-1")
        save_and_wait(chk, port, PLACE, "place_gen3")
    finally:
        quit_engine(port, proc)

    for slot in ("place_gen2", "autosave-1", "place_gen3"):
        got = place_tags(chk, tmp, root, slot)
        chk.ok(got == after_dig,
               f"'{slot}' carries exactly the survivors: the dug cell and the uprooted "
               f"plant lost theirs, the exposed cell and the far targets kept theirs "
               f"(got {got}, expected {after_dig})")


def main() -> int:
    ap = argparse.ArgumentParser(description=__doc__,
                                 formatter_class=argparse.RawDescriptionHelpFormatter)
    ap.add_argument("--port", type=int, default=9282)
    args = ap.parse_args()
    chk = Checks()
    tmp = tempfile.mkdtemp(prefix="gameplay_tag_persistence_probe_")
    try:
        root = make_isolated_root(tmp)
        # Every decode and every seed below execs the compiled codec;
        # resolve it before the first boot.
        prepare_decoder()
        found = scenario_files(root)
        chk.ok(found == [], f"the resource root holds no scenario file (got {found})")
        generated_leg(chk, args.port, root, tmp)
        arena_leg(chk, args.port, root, tmp)
        place_leg(chk, args.port, root, tmp)
    finally:
        if chk.failed:
            print(f"  logs kept at {tmp}")
        else:
            shutil.rmtree(tmp, ignore_errors=True)
    if chk.failed:
        print(f"\n    SOME FAILED ({chk.failed})")
        return 1
    print("\n    ALL PASSED")
    return 0


if __name__ == "__main__":
    sys.exit(main())
