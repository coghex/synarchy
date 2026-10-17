#!/usr/bin/env python3
"""Fresh-process gameplay-tag persistence probe (issue #2701, SCN-03 of
epic #2698).

General gameplay tags on units, buildings and locations live with their
owners and ride the ordinary `units` (v5), `buildings` (v3) and
`world-pages` (v14) save components. The hspec group
`Gameplay.Tags.Persistence` proves the in-process contract; this probe
proves what only separate processes can: that saved assignments, with
their target identities, survive ordinary load -> save -> fresh-process
load for a GENERATED world and an ARENA, and through the AUTOSAVE path.

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
        save_and_wait(chk, port, PAGE, "base")
    finally:
        quit_engine(port, proc)

    seeded = {
        "units": [{"page": PAGE, "id": u1, "tags": ["defender", "meeting_point"]},
                  {"page": PAGE, "id": u2, "tags": ["defender"]},
                  {"page": PAGE, "id": 9999, "tags": ["ghost"]}],
        "buildings": [{"page": PAGE, "id": bid, "tags": ["depot"]}],
        "locations": [{"page": PAGE, "id": lid, "tags": ["landmark"]},
                      {"page": PAGE, "id": 777, "tags": ["ghost"]}],
    }
    outgoing = {
        "units": [{"page": PAGE, "id": u1, "tags": ["outgoing"]}],
        "buildings": [{"page": PAGE, "id": bid, "tags": ["outgoing"]}],
        "locations": [{"page": PAGE, "id": lid, "tags": ["outgoing"]}],
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

    print("=== engine B: fresh process, load 'outgoing', load 'seeded' over it, save 'gen1' ===")
    log_b = os.path.join(tmp, "engineB.log")
    proc = boot(port, log=log_b, args=["--resource-root", root], ready_timeout=180)
    try:
        bootstrap_defs(port)
        load_and_wait(chk, port, "outgoing")
        load_and_wait(chk, port, "seeded")
        save_and_wait(chk, port, PAGE, "gen1")
    finally:
        quit_engine(port, proc)
    engine_log = Path(log_b).read_text(errors="replace")
    dropped = [line for line in engine_log.splitlines()
               if "dropped gameplay tags" in line]
    chk.ok(any("unit id" in line and "9999" in line for line in dropped)
           and any("location id" in line and "777" in line for line in dropped),
           f"the load reported dropping both dangling assignments "
           f"(diagnostics: {dropped})")

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

    spec = {"units": [{"page": ARENA, "id": uid, "tags": ["guard", "shared"]}],
            "buildings": [{"page": ARENA, "id": bid, "tags": ["shared"]}]}
    seed(chk, root, tmp, "arena0", "arena_seeded", spec)
    expected = spec_tags(spec, ARENA)

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
        prev = nxt


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
