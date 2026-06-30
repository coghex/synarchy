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
  2. Generates TWO real worlds: the active page "main_world" (which the
     load path remaps every active page onto) and a second page
     "second_world". Two generated pages is the canonical "active page
     remaps to main_world, secondary page would vanish" scenario.
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

Why two real worlds and not world.initArena: the issue text suggested an
arena as the secondary page, but loading a save that contains an arena
page hangs the world thread (the arena's degenerate gen params break the
regenerate-from-params load path) — tracked as #365. The feature this
guards is multi-PAGE persistence of real generated worlds, so the test
uses two of those.

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
import os
import shutil
import socket
import subprocess
import sys
import time
import uuid

SAVE_PREFIX = "mw_probe_"  # save dirs this probe owns (cleanup is scoped to it)


def _results(raw: bytes) -> list[str]:
    """Non-empty "> value" lines from the console stream. The console emits a
    "synarchy debug console\\n> " banner on connect and a trailing "> "
    prompt; both yield EMPTY "> " lines, so filter those out."""
    out = raw.decode(errors="replace")
    return [ln[2:].strip() for ln in out.splitlines()
            if ln.startswith("> ") and ln[2:].strip()]


def send(port: int, lua: str, timeout: float = 10.0,
         expect_result: bool = True) -> str:
    """Run one Lua line over the debug console and return its result.

    With expect_result (a `return ...` command) we read until a non-empty
    "> value" line appears, waiting up to `timeout`. This is what makes the
    BLOCKING builtins work: world.waitForInit / world.waitForChunks emit
    nothing until they unblock, so the read must wait the full timeout for
    the result rather than idling out early (a fixed short idle timeout
    would return before generation/chunk-loading finished and race partially
    initialized terrain). Fire-and-forget commands (no return) pass
    expect_result=False and just drain briefly. (Mirrors save_pause_probe.py.)
    """
    with socket.create_connection(("localhost", port), timeout=timeout) as s:
        s.sendall((lua + "\n").encode())
        chunks: list[bytes] = []
        s.settimeout(timeout)
        try:
            while True:
                b = s.recv(4096)
                if not b:
                    break
                chunks.append(b)
                if expect_result and _results(b"".join(chunks)):
                    break               # got the real result past the banner
                if not expect_result:
                    s.settimeout(0.3)   # drain remainder then idle out
        except socket.timeout:
            pass
    vals = _results(b"".join(chunks))
    return (vals[-1] if vals else "").strip('"')


def boot(port: int, logpath: str, tag: str) -> subprocess.Popen:
    log = open(logpath, "w")
    proc = subprocess.Popen(
        ["cabal", "run", "-v0", "exe:synarchy", "--",
         "--headless", "--port", str(port)],
        stdout=log, stderr=subprocess.STDOUT,
    )
    deadline = time.time() + 180
    while time.time() < deadline:
        try:
            if "READY" in open(logpath).read():
                return proc
        except FileNotFoundError:
            pass
        if proc.poll() is not None:
            sys.exit(f"{tag}: engine exited before READY; see {logpath}")
        time.sleep(0.4)
    proc.kill()
    sys.exit(f"{tag}: engine never printed READY; see {logpath}")


def shutdown(proc: subprocess.Popen, port: int) -> None:
    """Quit the engine, then make sure the process is gone and the port
    is free (so the next boot on the same port can bind)."""
    try:
        send(port, "engine.quit()", timeout=2)
    except OSError:
        pass
    for _ in range(50):  # up to ~5 s
        if proc.poll() is not None:
            break
        time.sleep(0.1)
    if proc.poll() is None:
        proc.kill()
        proc.wait(timeout=5)
    for _ in range(50):  # wait for the listener socket to release the port
        with socket.socket(socket.AF_INET, socket.SOCK_STREAM) as s:
            if s.connect_ex(("localhost", port)) != 0:
                return
        time.sleep(0.1)


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


def populate_world(port: int, page: str, seed: int, size: int, plates: int
                   ) -> tuple[int, int]:
    """Generate `page`, show it, and spawn a unit + cargo-hold building on
    a flat dry strip. Returns (unitId, buildingId). Exits the probe on a
    setup failure (no flat ground / spawn rejected) — setup must be solid
    for the post-load assertions to mean anything."""
    send(port, f"world.init('{page}', {seed}, {size}, {plates}); return 'ok'")
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
        procA = boot(args.port, logA, "engine A")
        bootstrap_defs(args.port)

        u_mw, b_mw = populate_world(args.port, "main_world",
                                    args.seed, args.size, args.plates)
        u_sw, b_sw = populate_world(args.port, "second_world",
                                    args.seed2, args.size, args.plates)

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

        shutdown(procA, args.port)
        procA = None

        # ── Engine B: fresh process, load, assert survival ─────────────
        procB = boot(args.port, logB, "engine B")
        bootstrap_defs(args.port)
        # A truly fresh engine: prove there are zero pre-load worlds, so
        # anything we see after the load provably came from disk.
        pre = send(args.port, "return world.getActiveWorldId()")
        print(f"engine B pre-load active world: {pre}")

        loaded = send(args.port, f"return engine.loadSave('{save_name}')")
        if loaded.strip() != "true":
            print(f"FAIL: engine.loadSave returned {loaded!r}", file=sys.stderr)
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

        print(f"\n{'PASS' if chk.failed == 0 else 'FAIL'}: "
              f"{chk.failed} check(s) failed")
        return 0 if chk.failed == 0 else 1

    finally:
        if procA is not None:
            shutdown(procA, args.port)
        if procB is not None:
            shutdown(procB, args.port)
        # Scoped cleanup: only ever remove a dir this probe created.
        if os.path.basename(save_dir).startswith(SAVE_PREFIX) and os.path.isdir(save_dir):
            shutil.rmtree(save_dir, ignore_errors=True)


if __name__ == "__main__":
    sys.exit(main())
