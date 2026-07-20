#!/usr/bin/env python3
"""Headless multi-world save/load regression probe (issue #219, epic #214).

A single save persists and restores EVERY live world page, not just the
active one (#214). Before that fix, loading a save dropped every
secondary page's units and buildings on the floor — they vanished. This
probe is the end-to-end regression gate for that guarantee.

It can't be an hspec test: the round-trip spans the Lua thread, the world
thread, the unit/building threads, and the on-disk save file, and the
gold-standard check is "save -> QUIT -> fresh restart -> load" so that a
restored page provably came from disk (the fresh engine has zero pre-load
worlds). That is inherently a multi-process, disk-touching test, so it
lives here next to save_pause_probe.py rather than in test-headless/.

What it does:
  1. Boots a headless engine (engine A) and loads the unit/building defs
     the loading screen would normally load (it doesn't run headless).
  2. Generates TWO real worlds: the active page "main_world" (its own
     saved id -- issue #763's whole-session load transaction preserves
     every saved page id verbatim, no remap) and a second page
     "second_world". Two generated pages is the canonical "secondary
     page would vanish in a naive load" scenario.
  3. Spawns a player unit AND a cargo-hold building on EACH page.
  4. Saves (engine.saveWorld("main_world", ...)), asserts the save file
     was actually written, then QUITS engine A.
  5. Boots a SECOND fresh engine (engine B), reloads the defs, and loads
     the save.
  6. Asserts BOTH pages' units and buildings survived AND landed on the
     right page: unit.getAllIds()/building.getActiveIds() are
     active-page-scoped, so showing a page and reading them proves
     membership; the cross-page negative checks (main_world's unit is NOT
     on second_world, and vice-versa) prove the pages didn't get merged.

Arena pages (#365): loading a save containing a world.initArena page used
to hang the world thread (the arena's synthetic gen params wedge the
regenerate-from-params load path). Fixed by rebuilding arena pages through
the shared flat builder on load; pass --arena to run this probe with an
arena as the secondary page — it additionally asserts a pre-save
world.addTile edit is replayed onto the rebuilt arena. The default run
keeps two real generated worlds (the #219 gate) unchanged.

World identity (#707): the probe also gates the player-facing identity
layer end to end. Both pages are created through the extended Lua
contract — world.init(pageId, seed, size, plates, displayName[, gloss])
— with a whitespace-padded name (proving trim) and a whitespace-only
gloss (proving gloss omission); world.getIdentity is checked on engine A
(named pages, missing page -> nil), and after the fresh-restart load the
primary identity must follow its page to main_world, the secondary
identity must stay on second_world, engine.listSaves() must report the
save-slot name SEPARATELY from worldName/worldGloss, and a 4-argument
world.init page must come up unnamed. In --arena mode the secondary page
is an arena and must be unnamed instead.

Note on visibility ordering: world.show does not promote an
ALREADY-visible page to the head of the visible stack (a documented
world-thread quirk), so the probe leaves only main_world visible at save
time. After load, main_world comes up active; the probe checks it first,
then world.show("second_world") (hidden -> promotes cleanly) to check the
secondary page.

Usage:
  python3 tools/multiworld_save_probe.py            # seeds 42 / 7, size 64
  python3 tools/multiworld_save_probe.py --port 9219 --seed 42 --seed2 7

Exit 0 = every saved entity survived on its correct page.
"""
from __future__ import annotations

import argparse
import glob
import json
import os
import shutil
import socket
import subprocess
import sys
import time
import uuid
from probelib import quit_engine, boot, send, send_json, wait_load_published

SAVE_PREFIX = "mw_probe_"  # save dirs this probe owns (cleanup is scoped to it)

# Player-facing identities (#707). The primary name is passed to
# world.init with whitespace padding to prove the trim rule; these are
# the values that must come back from world.getIdentity / listSaves.
MW_NAME = "Aldermoor Deep"
MW_GLOSS = "the deep home"
SW_NAME = "Squally Isles"


def get_identity(port: int, page: str):
    """world.getIdentity(page) -> dict | None (Lua nil)."""
    raw = send(port, f"return world.getIdentity('{page}')")
    if not raw or raw in ("nil", "null"):
        return None
    try:
        return json.loads(raw)
    except ValueError:
        return raw


def bootstrap_defs(port: int) -> None:
    """Load the defs the headless engine needs to spawn AND to re-resolve
    saved entities on load (fromUnitSnapshot/fromBuildingSnapshot drop any
    entity whose def isn't loaded — so the load side needs these too)."""
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
            send(port, f"{fn}('{path}'); return 'ok'")


def wait_active(port: int, page: str, secs: float = 10.0) -> bool:
    """Poll until the given page is the active (head-of-visible) world."""
    deadline = time.time() + secs
    while time.time() < deadline:
        if send(port, "return world.getActiveWorldId()").strip('"') == page:
            return True
        time.sleep(0.2)
    return False


def find_flat_strip(port: int) -> tuple[int, int, int] | None:
    """Return (gx, gy, z) of a dry 3-wide equal-z land strip, or None.

    Same shape as combat_anim_probe.find_flat_strip — a unit goes at the
    west end and a building two tiles east, both on flat dry ground.
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


def as_int(s: str) -> int | None:
    try:
        return int(float(s))
    except (TypeError, ValueError):
        return None


def id_list(port: int, expr: str) -> list[int]:
    """Read a Lua array of integer ids returned by `expr`. The debug
    console serializes an int-keyed array as JSON [..] and an empty table
    as {}, so anything that isn't a [ is treated as 'no ids'."""
    raw = send(port, expr).strip()
    if not raw.startswith("["):
        return []
    inner = raw.strip("[]").strip()
    if not inner:
        return []
    out = []
    for tok in inner.split(","):
        v = as_int(tok.strip())
        if v is not None:
            out.append(v)
    return out


def populate_world(port: int, page: str, seed: int, size: int, plates: int,
                   name: str | None = None, gloss: str | None = None,
                   ) -> tuple[int, int]:
    """Generate `page`, show it, and spawn a unit + cargo-hold building on
    a flat dry strip. Returns (unitId, buildingId). Exits the probe on a
    setup failure (no flat ground / spawn rejected) — setup must be solid
    for the post-load assertions to mean anything.

    `name`/`gloss` ride as world.init's optional 5th/6th arguments — the
    page's player-facing identity (#707)."""
    init_args = f"'{page}', {seed}, {size}, {plates}"
    if name is not None:
        init_args += f", '{name}'"
        if gloss is not None:
            init_args += f", '{gloss}'"
    send(port, f"world.init({init_args}); return 'ok'")
    send(port, "return world.waitForInit(180)", timeout=190)
    # Must SHOW (not just init): building.spawn/canPlaceAt read
    # snapshotVisibleWorldTiles, which needs the page in wmVisible.
    # getActiveWorldId() falls back to the wmWorlds head, so it can report a
    # page "active" before any page is actually visible.
    send(port, f"world.show('{page}'); return 'ok'")
    if not wait_active(port, page):
        sys.exit(f"FAIL: {page} never became active")
    send(port, "return world.loadChunksInRegion(-2,-2,2,2)")
    send(port, "return world.waitForChunks(120)", timeout=125)

    strip = find_flat_strip(port)
    if not strip:
        sys.exit(f"FAIL: no flat dry ground found on {page}")
    gx, gy, z = strip
    uid = as_int(send(port,
        f"return unit.spawn('acolyte', {gx}, {gy}, {z}, 'player')"))
    bid = as_int(send(port,
        f"return building.spawn('cargo_hold_S', {gx + 2}, {gy})"))
    print(f"{page}: flat strip ({gx},{gy}) z={z}  unit=#{uid}  building=#{bid}")
    if uid is None or uid < 0:
        sys.exit(f"FAIL: unit.spawn rejected on {page}")
    if bid is None or bid < 0:
        sys.exit(f"FAIL: building.spawn rejected on {page}")
    return uid, bid


ARENA_EDIT_TILE = (6, 6)  # tile raised pre-save; must survive the reload


def populate_arena(port: int, page: str) -> tuple[int, int, int]:
    """world.initArena `page`, show it, spawn a unit + cargo-hold building
    on the flat ground, and raise one tile so the load path's edit replay
    is exercised. Returns (unitId, buildingId, editedTileZ)."""
    send(port, f"world.initArena('{page}'); return 'ok'")
    send(port, f"world.show('{page}'); return 'ok'")
    if not wait_active(port, page):
        sys.exit(f"FAIL: {page} never became active")

    gx, gy = 2, 2
    # No explicit z: unit.spawn resolves the surface of the page it lands
    # on, which is the arena's flat top.
    uid = as_int(send(port,
        f"return unit.spawn('acolyte', {gx}, {gy}, 'player')"))
    bid = as_int(send(port,
        f"return building.spawn('cargo_hold_S', {gx + 2}, {gy})"))
    print(f"{page}: arena  unit=#{uid}  building=#{bid}")
    if uid is None or uid < 0:
        sys.exit(f"FAIL: unit.spawn rejected on arena {page}")
    if bid is None or bid < 0:
        sys.exit(f"FAIL: building.spawn rejected on arena {page}")

    # Raise one tile (granite, material id 1). addTile runs on the world
    # thread — poll until the surface reflects it.
    ex, ey = ARENA_EDIT_TILE
    base = as_int(send(port, f"local s = world.getTerrainAt({ex}, {ey}); return s"))
    if base is None:
        sys.exit(f"FAIL: could not read arena terrain at ({ex},{ey})")
    send(port, f"return world.addTile('{page}', {ex}, {ey}, 1)")
    for _ in range(50):
        cur = as_int(send(port, f"local s = world.getTerrainAt({ex}, {ey}); return s"))
        if cur == base + 1:
            break
        time.sleep(0.1)
    else:
        sys.exit(f"FAIL: arena addTile at ({ex},{ey}) never landed")
    print(f"{page}: raised tile ({ex},{ey}) to z={base + 1}")
    return uid, bid, base + 1


class Checks:
    def __init__(self) -> None:
        self.failed = 0

    def ok(self, cond: bool, label: str) -> None:
        print(f"  [{'PASS' if cond else 'FAIL'}] {label}")
        if not cond:
            self.failed += 1


def main() -> int:
    ap = argparse.ArgumentParser(description=__doc__)
    ap.add_argument("--port", type=int, default=9219)
    ap.add_argument("--seed", type=int, default=42, help="main_world seed")
    ap.add_argument("--seed2", type=int, default=7, help="second_world seed")
    ap.add_argument("--size", type=int, default=64)
    ap.add_argument("--plates", type=int, default=3)
    ap.add_argument("--arena", action="store_true",
                    help="second_world is a world.initArena page "
                         "(#365 regression: arena pages must survive "
                         "the save -> restart -> load round-trip)")
    args = ap.parse_args()

    # Unique per run (random, NOT pid-derived): a reused pid could collide
    # with a stale dir left by an interrupted run, making engine B load OLD
    # data (false pass) and the cleanup delete a dir this run didn't create.
    save_name = f"{SAVE_PREFIX}{uuid.uuid4().hex[:12]}"
    save_dir = os.path.join("saves", save_name)
    save_file = os.path.join(save_dir, "world.synworld")
    # Belt-and-suspenders on top of the random name: never reuse/clobber an
    # existing directory, so file existence below is proof THIS run wrote it.
    if os.path.exists(save_dir):
        sys.exit(f"refusing to run: {save_dir} already exists")
    logA = "/tmp/mw_save_probe_A.log"
    logB = "/tmp/mw_save_probe_B.log"
    procA = procB = None
    chk = Checks()

    try:
        # ── Engine A: build the two pages, populate them, save ──────────
        procA = boot(args.port, log=logA, label="engine A")
        bootstrap_defs(args.port)

        # Identity args (#707): padded name proves the trim rule; the
        # whitespace-only gloss on second_world proves gloss omission.
        u_mw, b_mw = populate_world(args.port, "main_world",
                                    args.seed, args.size, args.plates,
                                    name=f"  {MW_NAME}  ", gloss=MW_GLOSS)
        arena_z = None
        if args.arena:
            u_sw, b_sw, arena_z = populate_arena(args.port, "second_world")
        else:
            u_sw, b_sw = populate_world(args.port, "second_world",
                                        args.seed2, args.size, args.plates,
                                        name=SW_NAME, gloss="   ")

        print("\n--- world identity checks (engine A, #707) ---")
        ident = get_identity(args.port, "main_world")
        chk.ok(isinstance(ident, dict) and ident.get("name") == MW_NAME,
               f"main_world display name stored trimmed ({ident})")
        chk.ok(isinstance(ident, dict) and ident.get("gloss") == MW_GLOSS,
               f"main_world gloss stored ({ident})")
        if args.arena:
            chk.ok(get_identity(args.port, "second_world") is None,
                   "arena second_world is unnamed (getIdentity nil)")
        else:
            ident2 = get_identity(args.port, "second_world")
            chk.ok(isinstance(ident2, dict) and ident2.get("name") == SW_NAME,
                   f"second_world display name stored ({ident2})")
            chk.ok(isinstance(ident2, dict) and "gloss" not in ident2,
                   f"whitespace-only gloss omitted on second_world ({ident2})")
        chk.ok(get_identity(args.port, "no_such_page") is None,
               "getIdentity of a missing page is nil")

        # Leave only main_world visible so the post-load show toggles are
        # clean (see module docstring). Then save with main_world primary.
        send(args.port, "world.hide('second_world'); return 'ok'")
        send(args.port, "world.show('main_world'); return 'ok'")
        wait_active(args.port, "main_world")
        # Settle: spawn commands run on the unit/building threads.
        for _ in range(40):
            if (send(args.port, f"return unit.exists({u_mw})") == "true" and
                    send(args.port, f"return unit.exists({u_sw})") == "true"):
                break
            time.sleep(0.1)

        saved = send(args.port, f"return engine.saveWorld('main_world', '{save_name}')")
        if saved.strip() != "true":
            print(f"FAIL: engine.saveWorld returned {saved!r}", file=sys.stderr)
            return 2
        # The world thread writes the file asynchronously after the API
        # call returns — wait for it (and guard against a dead/stale engine
        # silently swallowing the command).
        for _ in range(100):
            if os.path.exists(save_file):
                break
            time.sleep(0.1)
        if not os.path.exists(save_file):
            print(f"FAIL: save file never appeared at {save_file}", file=sys.stderr)
            return 2
        print(f"saved -> {save_file} ({os.path.getsize(save_file)} bytes)")

        quit_engine(args.port, procA)
        procA = None

        # ── Engine B: fresh process, load, assert survival ─────────────
        procB = boot(args.port, log=logB, label="engine B")
        bootstrap_defs(args.port)
        # A truly fresh engine: prove there are zero pre-load worlds, so
        # anything we see after the load provably came from disk.
        pre = send(args.port, "return world.getActiveWorldId()")
        print(f"engine B pre-load active world: {pre}")

        loaded = send(args.port, f"return engine.loadSave('{save_name}')")
        if loaded.strip() != "true":
            print(f"FAIL: engine.loadSave returned {loaded!r}", file=sys.stderr)
            return 2
        published, status = wait_load_published(args.port)
        if not published:
            print(f"FAIL: load transaction did not publish: {status}",
                  file=sys.stderr)
            return 2
        send(args.port, "return world.waitForInit(180)", timeout=190)
        time.sleep(3)  # let the secondary page + queued chunks settle

        print("\n--- multi-world restore checks ---")
        # Both units survive (global existence).
        chk.ok(send(args.port, f"return unit.exists({u_mw})") == "true",
               f"main_world unit #{u_mw} survived the load")
        chk.ok(send(args.port, f"return unit.exists({u_sw})") == "true",
               f"second_world unit #{u_sw} survived the load")

        # main_world should be the active page right after load.
        chk.ok(wait_active(args.port, "main_world"),
               "main_world is the active page after load")
        mw_units = id_list(args.port, "return unit.getAllIds()")
        mw_bldgs = id_list(args.port, "return building.getActiveIds()")
        chk.ok(u_mw in mw_units,
               f"main_world unit #{u_mw} is on main_world ({mw_units})")
        chk.ok(u_sw not in mw_units,
               f"second_world unit #{u_sw} is NOT on main_world ({mw_units})")
        chk.ok(b_mw in mw_bldgs,
               f"main_world building #{b_mw} is on main_world ({mw_bldgs})")

        # Switch to the secondary page (hidden -> show promotes cleanly).
        send(args.port, "world.show('second_world'); return 'ok'")
        chk.ok(wait_active(args.port, "second_world"),
               "second_world restored and can be shown")
        sw_units = id_list(args.port, "return unit.getAllIds()")
        sw_bldgs = id_list(args.port, "return building.getActiveIds()")
        chk.ok(u_sw in sw_units,
               f"second_world unit #{u_sw} is on second_world ({sw_units})")
        chk.ok(u_mw not in sw_units,
               f"main_world unit #{u_mw} is NOT on second_world ({sw_units})")
        chk.ok(b_sw in sw_bldgs,
               f"second_world building #{b_sw} is on second_world ({sw_bldgs})")

        if args.arena and arena_z is not None:
            # The pre-save addTile edit must have been replayed onto the
            # rebuilt arena chunks (#365).
            ex, ey = ARENA_EDIT_TILE
            got = as_int(send(args.port,
                f"local s = world.getTerrainAt({ex}, {ey}); return s"))
            chk.ok(got == arena_z,
                   f"arena edit at ({ex},{ey}) replayed on load "
                   f"(z={got}, expected {arena_z})")

        print("\n--- world identity restore checks (#707) ---")
        ident = get_identity(args.port, "main_world")
        chk.ok(isinstance(ident, dict) and ident.get("name") == MW_NAME
               and ident.get("gloss") == MW_GLOSS,
               f"primary identity followed its page to main_world ({ident})")
        if args.arena:
            chk.ok(get_identity(args.port, "second_world") is None,
                   "arena second_world restored unnamed")
        else:
            ident2 = get_identity(args.port, "second_world")
            chk.ok(isinstance(ident2, dict) and ident2.get("name") == SW_NAME
                   and "gloss" not in ident2,
                   f"secondary identity stayed on second_world ({ident2})")

        # Save-slot name vs world name vs gloss: three distinct values in
        # the save listing. `name` stays the slot id; the identity rides
        # in the optional worldName/worldGloss fields.
        saves = send_json(args.port, "return engine.listSaves()")
        entry = None
        if isinstance(saves, list):
            entry = next((s for s in saves if isinstance(s, dict)
                          and s.get("name") == save_name), None)
        chk.ok(entry is not None,
               f"listSaves has an entry for slot '{save_name}'")
        if entry is not None:
            chk.ok(entry.get("worldName") == MW_NAME,
                   f"listSaves worldName is the display name "
                   f"({entry.get('worldName')!r})")
            chk.ok(entry.get("worldGloss") == MW_GLOSS,
                   f"listSaves worldGloss is the gloss "
                   f"({entry.get('worldGloss')!r})")
            chk.ok(entry.get("name") != entry.get("worldName"),
                   "save-slot name and world name are distinct")

        # A 4-argument world.init page stays unnamed end to end. Tiny w8
        # world; poll registration via getDate (nil until the page
        # exists) rather than waitForInit, which tracks the ACTIVE page.
        send(args.port, "world.init('unnamed_w8', 5, 8, 3); return 'ok'")
        for _ in range(150):
            if send(args.port,
                    "return world.getDate('unnamed_w8')") not in ("nil", "null", ""):
                break
            time.sleep(0.2)
        else:
            sys.exit("FAIL: unnamed_w8 page never registered")
        chk.ok(get_identity(args.port, "unnamed_w8") is None,
               "4-argument world.init page is unnamed (getIdentity nil)")

        print(f"\n{'PASS' if chk.failed == 0 else 'FAIL'}: "
              f"{chk.failed} check(s) failed")
        return 0 if chk.failed == 0 else 1

    finally:
        if procA is not None:
            quit_engine(args.port, procA)
        if procB is not None:
            quit_engine(args.port, procB)
        # Scoped cleanup: only ever remove a dir this probe created.
        if os.path.basename(save_dir).startswith(SAVE_PREFIX) and os.path.isdir(save_dir):
            shutil.rmtree(save_dir, ignore_errors=True)


if __name__ == "__main__":
    sys.exit(main())
