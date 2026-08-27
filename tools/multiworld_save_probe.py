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

Arena base seeding (#1718): rebuilding is not restoring. The save stores
generation parameters and the edit overlay, never the arena's base tile
grid, so every surface tile no edit touched is REGENERATED on load from
the page's recorded seed. --arena therefore also captures the complete
256-position surface-vegetation vector of an untouched chunk before the
save and compares it after the load. The whole vector, not a tile: the
arena generator picks among four grass/moss variants per column, so a
single coordinate agrees by chance one time in four even when every
other tile was re-rolled.

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
from probelib import (quit_engine, boot, send, send_json, poll_until,
                      wait_load_published)

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


# Setup budgets for a page this probe has just ASKED for. They are
# deliberately not wait_active's default: that 10 s is the right budget
# for activating a page that already exists (the post-load checks below),
# and the wrong one for a page still being generated (#1447).
#
# world.init and world.show are FIFO world-thread commands drained
# synchronously by processAllCommands (src/World/Thread.hs:107-115), so a
# queued show cannot run until the preceding generation returns:
# ACTIVATION is what has to sit out the whole world generation, and it
# gets the same budget the probe already allowed generation elsewhere.
# REGISTRATION is cheap by comparison — handleWorldInitCommand puts the
# page into wmWorlds before generating ("register early so lua can read
# the loading phase", src/World/Thread/Command/Init.hs:112-118), as does
# the arena path (Init.hs:420-426).
PAGE_READY_SECS = 60.0
PAGE_ACTIVE_SECS = 180.0


def page_registered(port: int, page: str) -> bool:
    """True once `page` is registered in wmWorlds.

    world.getDate(pageId) returns nil until the lookup in wmWorlds
    succeeds (src/Engine/Scripting/Lua/API/World/Clock.hs:116-145), which
    makes it the one per-page observable that exists BEFORE the page is
    active — there is no per-page load-phase accessor in the Lua API, and
    #1447 is a probe repair, so none may be added.
    """
    return send(port, f"return world.getDate('{page}')").strip() not in (
        "nil", "null", "")


def wait_page_setup(port: int, page: str,
                    ready_secs: float = PAGE_READY_SECS,
                    active_secs: float = PAGE_ACTIVE_SECS) -> None:
    """Block until the REQUESTED page is registered and then active (#1447).

    Both waits are tied to `page` itself, so an already-complete,
    already-active OTHER page cannot satisfy either one — which is the
    bug this replaced: `world.waitForInit` polls only the active world
    (src/Engine/Scripting/Lua/Thread/Console.hs:88-101), so a finished
    main_world satisfied second_world's wait and left the 10 s activation
    window as the real generation budget.

    Exits the probe on a timeout, naming the page AND which of the two
    boundaries stalled: never-registered means the world thread never ran
    the init command, whereas registered-but-never-active means the
    generation queued ahead of the show did not finish in budget.
    """
    if not poll_until(ready_secs, lambda: page_registered(port, page)):
        sys.exit(f"FAIL: setup for page '{page}' stalled at READY: it was "
                 f"never registered (world.getDate('{page}') stayed nil) "
                 f"within {ready_secs:.0f}s of world.init — the world thread "
                 f"never ran the init command for this page")
    if not poll_until(active_secs, lambda: send(
            port, "return world.getActiveWorldId()") == page):
        active = send(port, "return world.getActiveWorldId()")
        sys.exit(f"FAIL: setup for page '{page}' stalled at ACTIVE: it "
                 f"registered but never became the active world within "
                 f"{active_secs:.0f}s of world.show (active is {active!r}) — "
                 f"the generation queued ahead of that show did not finish")


def find_flat_strip(port: int) -> tuple[int, int, int] | None:
    """Return (gx, gy, z) of a dry 3-wide equal-z land strip, or None.

    A unit goes at the west end and a building two tiles east, both on
    flat dry ground. combat_anim_probe.py used to carry the same helper;
    #1396 moved it to a verified flat arena instead, because accepting
    any three adjacent equal-height dry tiles in a generated world can
    land the strip at the lip of a lethal drop.
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
    # Must SHOW (not just init): building.canPlaceAt answers only for the
    # VISIBLE page, so it needs the page in wmVisible.
    # getActiveWorldId() falls back to the wmWorlds head, so it can report a
    # page "active" before any page is actually visible.
    send(port, f"world.show('{page}'); return 'ok'")
    wait_page_setup(port, page)
    # ONLY NOW is waitForInit meaningful for this page: it polls the ACTIVE
    # world's load phase, so before the show landed it could only ever
    # observe the previously active page (#1447). Activation does not imply
    # generation finished — handleWorldInitCommand returns at LoadPhase2 and
    # the initial chunks drain on later world-thread ticks — so this is the
    # real completion wait, and for the FIRST page (activated the instant it
    # registers, via the wmWorlds-head fallback above) it is the only one.
    # It is also what makes the spawns below safe on that first page: they
    # need wmVisible, and reaching LoadDone here proves the show queued
    # above already drained, since it sits ahead of the chunk work in the
    # world thread's FIFO.
    send(port, "return world.waitForInit(180)", timeout=190)
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

# Chunk (1,1) of the 5x5 arena — global tiles 16..31 on both axes. Far
# from every spawn and from ARENA_EDIT_TILE, so nothing this probe does
# ever writes an edit into it: its surface is pure generator output on
# both sides of the round trip.
ARENA_VEG_CHUNK = (16, 16)


def read_arena_veg(port: int) -> list[int] | None:
    """The COMPLETE 256-position surface-vegetation vector of the active
    page's ARENA_VEG_CHUNK, in column order.

    Not a sampled tile (#1718): the generator picks among four grass/moss
    variants, so a single coordinate agrees by chance one time in four and
    a one-tile check would miss three quarters of a reseeding regression.
    A tile the engine cannot answer for (unloaded chunk -> nil) is
    recorded as -1 rather than skipped, so a short or hole-punched read
    fails the comparison instead of silently shrinking it.
    """
    x0, y0 = ARENA_VEG_CHUNK
    lua = (
        "local t={} "
        f"for y={y0},{y0 + 15} do for x={x0},{x0 + 15} do "
        "local v=world.getVegAt(x,y) "
        "t[#t+1] = (v==nil) and -1 or v end end return t"
    )
    got = send_json(port, lua, timeout=15.0)
    if not isinstance(got, list):
        return None
    return [as_int(str(v)) if not isinstance(v, int) else v for v in got]


def populate_arena(port: int, page: str) -> tuple[int, int, int, list[int]]:
    """world.initArena `page`, show it, spawn a unit + cargo-hold building
    on the flat ground, and raise one tile so the load path's edit replay
    is exercised. Returns (unitId, buildingId, editedTileZ, baseVegVector)
    — the last being the untouched chunk's whole surface-vegetation vector
    as this page was FRESHLY generated (#1718)."""
    send(port, f"world.initArena('{page}'); return 'ok'")
    send(port, f"world.show('{page}'); return 'ok'")
    # Same page-scoped, boundary-naming setup wait as populate_world: the
    # arena registers early too (Init.hs:420-426) and writes LoadDone before
    # its command returns, so activation already implies a finished arena.
    wait_page_setup(port, page)

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

    # #1718: snapshot the untouched chunk's full surface vegetation while
    # this page is still the freshly generated one. The save stores gen
    # params and the edit overlay, never the base tile grid, so this
    # vector is RECONSTRUCTED on load from the page's recorded seed —
    # which is exactly what the post-load read below re-checks.
    veg = read_arena_veg(port)
    if veg is None or len(veg) != 256 or -1 in veg:
        sys.exit(f"FAIL: could not read arena base vegetation on {page} "
                 f"({'nil' if veg is None else f'{len(veg)} values'})")
    print(f"{page}: base vegetation captured "
          f"({len(veg)} tiles, ids {sorted(set(veg))})")
    return uid, bid, base + 1, veg


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
        arena_veg = None
        if args.arena:
            u_sw, b_sw, arena_z, arena_veg = populate_arena(
                args.port, "second_world")
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

        if args.arena and arena_veg is not None:
            # #1718: the untouched chunk's base vegetation is regenerated
            # from the page's recorded seed, so the WHOLE 256-position
            # vector must come back identical. A short read, a nil tile
            # (-1), or one differing variant fails.
            after = read_arena_veg(args.port)
            chk.ok(isinstance(after, list) and len(after) == 256
                   and -1 not in after,
                   "arena base vegetation is fully readable after load "
                   f"({'nil' if after is None else f'{len(after)} values'})")
            if isinstance(after, list):
                differing = sum(1 for a, b in zip(arena_veg, after) if a != b)
                chk.ok(after == arena_veg,
                       "arena base vegetation survives the save/load round "
                       f"trip unchanged ({differing}/256 tiles differ)")

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
        if not poll_until(30.0,
                          lambda: page_registered(args.port, 'unnamed_w8'),
                          interval=0.2):
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
