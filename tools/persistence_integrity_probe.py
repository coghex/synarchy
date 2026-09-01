#!/usr/bin/env python3
"""Headless persistence-reference-integrity probe (issue #764,
save-overhaul C3).

Real-engine, real-restart coverage of the shared save/load integrity
graph ("World.Save.Integrity") on top of what the pure hspec gate
(`Test.Headless.World.Save.Integrity`, "persistence reference
integrity") already proves about the algorithm in isolation. The
Haskell-side wrong-page bill/power-node checks are exhaustively covered
there (a wrong-page craft-bill/power-node reference cannot be produced
through the ordinary game API at all -- stations/host buildings are
always resolved same-page by the AI/build tooling -- so hand-built DTOs
are the only way to exercise that path, and hspec is the right place
for it). What ONLY a real engine can prove, and what this probe covers
instead:

  1. A genuinely dangling Lua AI reference (a unit's `attackTargetUid`
     pointing at a unit destroyed before the save) survives a real
     save -> quit -> fresh restart -> load round trip WITHOUT blocking
     the load (the #761-established tolerated-dangling-reference
     contract) -- and the new cross-validation
     (`Engine.Scripting.Lua.API.Save.Integrity`'s `knownEntitiesFromSaveData` /
     `World.Save.Integrity.luaReferenceErrors`) actually reports it as
     a diagnostic, naming the component, the reference kind, and the
     destroyed unit's id, AT BOTH the pre-save boundary (over the same
     live snapshot `saveModules.snapshotAll()` captured) and the
     pre-load boundary of the fresh restart -- proving save and load
     share one integrity graph rather than only load being checked.
  2. A genuinely corrupted (truncated) save file is rejected with a
     real `LoadFailed` status, the engine stays paused, and the
     ALREADY-LOADED live session (page/unit state) is left completely
     unchanged -- the probe-level analogue of what
     `tools/save_barrier_probe.py`/`tools/save_storage_probe.py` already
     prove for storage-level failures, exercised here specifically
     through the new integrity-aware load path.

  3. The OTHER side of check 1 (issue #1484): a cached AI reference
     whose target died is expected to be gone by the save boundary, not
     tolerated there. A real acolyte acquires a real construction site
     as `s.buildTarget`, the site is destroyed, the AI ticks past the
     point where resolution finds no replacement -- and the save that
     follows carries NO `lua.unit_ai` `dangling-reference` diagnostic
     naming that unit's `buildTarget` and that building. Check 1's
     deliberately-dangling `attackTargetUid` rides the SAME save, so the
     absence is proven against a log that demonstrably does report this
     class of diagnostic rather than against a silent one.

Runs against an ISOLATED temporary resource root (symlinked
scripts/assets/data/config, a throwaway saves/ dir) so it never touches
a real player's saves/ directory.

Usage:
  python3 tools/persistence_integrity_probe.py
  python3 tools/persistence_integrity_probe.py --port 9264 --seed 42

Exit 0 = every check above passed.
"""
from __future__ import annotations

import argparse
import glob
import os
import re
import shutil
import sys
import tempfile
import time
from pathlib import Path

from probelib import (boot, clear_find_water, load_ai_stack, poll_until,
                      quit_engine, send, send_json, wait_load_published)

REPO = Path(__file__).resolve().parent.parent
SLOT = "probe_integrity_slot"
CORRUPT_SLOT = "probe_integrity_corrupt"
BUILD_SITE_DEF = "cargo_hold_S"
BUILD_SITE_YAML = f"data/buildings/{BUILD_SITE_DEF}.yaml"


def make_isolated_root(base: str) -> str:
    """A throwaway resource root: real scripts/assets/data/config
    (symlinked -- read-only content, safe to share) plus its OWN empty
    saves/ directory, so this probe never touches a real player's saves.
    """
    root = os.path.join(base, "root")
    os.makedirs(root, exist_ok=True)
    for family in ("scripts", "assets", "data", "config"):
        target = os.path.join(root, family)
        if not os.path.exists(target):
            os.symlink(os.path.join(REPO, family), target)
    os.makedirs(os.path.join(root, "saves"), exist_ok=True)
    return root


def boot_probe(root: str, port: int, log: str):
    return boot(port, log=log, args=["--resource-root", root], ready_timeout=180)


def bootstrap_defs(port: int) -> None:
    loaders = [
        ("data/substances/*.yaml", "engine.loadSubstanceYaml"),
        ("data/items/*.yaml",      "engine.loadItemYaml"),
        ("data/equipment/*.yaml",  "engine.loadEquipmentYaml"),
        ("data/materials/*.yaml",  "engine.loadMaterialYaml"),
        ("data/units/*.yaml",      "engine.loadUnitYaml"),
        # Just the one def the #1484 scenario stakes -- a shipped
        # `build_work: 240.0` building, so a `building.spawn`ed instance
        # reports "constructing" with real build work outstanding and stays
        # there (no construction tick runs in this probe), which is
        # exactly what `findNearestUnbuilt` looks for.
        (BUILD_SITE_YAML,          "engine.loadBuildingYaml"),
    ]
    for pattern, fn in loaders:
        for path in sorted(glob.glob(pattern)):
            send(port, f"{fn}('{path}'); return 'ok'")


def find_flat_strip(port: int) -> tuple[int, int, int] | None:
    """Return (gx, gy, z) of a dry 3-wide equal-z land strip, or None."""
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


def find_build_site(port: int, gx: int, gy: int) -> tuple[int, int] | None:
    """First tile near (gx, gy) the ENGINE's own placement validator
    accepts for BUILD_SITE_DEF, plus an adjacent tile with the same
    terrain z (where the acolyte will stand). Asking
    `building.canPlaceAt` beats guessing at flat ground: it is the same
    check `building.spawn` runs, so a hit here cannot spawn-fail.
    """
    lua = (
        "local function f() for dy=-6,6 do for dx=-6,6 do "
        "local bx,by = %d+dx, %d+dy "
        f"local ok = building.canPlaceAt('{BUILD_SITE_DEF}', bx, by) "
        "if ok then "
        "local zb = world.getTerrainAt(bx,by) local zu = world.getTerrainAt(bx+1,by) "
        "if zb and zu and zb==zu and not world.getFluidAt(bx+1,by) "
        "then return bx..','..by end end end end return 'none' end return f()"
    ) % (gx, gy)
    for _ in range(8):
        res = send(port, lua).strip('"')
        if res and res != "none" and res.count(",") == 1:
            bx, by = (int(v) for v in res.split(","))
            return bx, by
        time.sleep(0.75)
    return None


def ai_build_target(port: int, uid: int) -> str:
    """`s.buildTarget` for `uid`, as the raw console string ("nil" when
    the field is absent)."""
    return send(port, f"local s = require('scripts.unit_ai').getState({uid}); "
                      f"if not s or s.buildTarget == nil then return 'nil' end "
                      f"return tostring(s.buildTarget)").strip('"')


def dangling_build_target_lines(log_text: str, uid: int, bid: int) -> list[str]:
    """Every rendered integrity line that is THE #1484 diagnostic: the
    `lua.unit_ai` component, this unit's `buildTarget` path, the
    `dangling-reference` code, and the destroyed building as the
    reference value.

    Matching only the component and the code (which is all
    `persistence_integrity_probe`'s check-1 assertion needs, since it
    is asserting PRESENCE) would let this absence assertion pass on an
    unrelated unit_ai reference -- or fail on check 1's own deliberately
    dangling `attackTargetUid`, which rides the same save.
    """
    pattern = re.compile(
        r"\[lua\.unit_ai v\d+ unit\[" + str(uid) + r"\]\.buildTarget\] "
        r"dangling-reference:[^\n]*references building " + str(bid) + r"\b")
    return [ln for ln in log_text.splitlines() if pattern.search(ln)]


class Checks:
    def __init__(self) -> None:
        self.failed = 0

    def ok(self, cond: bool, label: str) -> None:
        print(f"  [{'PASS' if cond else 'FAIL'}] {label}")
        if not cond:
            self.failed += 1


def read_log(path: str) -> str:
    if not os.path.exists(path):
        return ""
    with open(path, encoding="utf-8", errors="replace") as f:
        return f.read()


def main() -> int:
    ap = argparse.ArgumentParser(description=__doc__,
                                  formatter_class=argparse.RawDescriptionHelpFormatter)
    ap.add_argument("--port", type=int, default=9264)
    ap.add_argument("--seed", type=int, default=42)
    ap.add_argument("--size", type=int, default=48)
    args = ap.parse_args()

    tmp = tempfile.mkdtemp(prefix="synarchy_persistence_integrity_probe_")
    root = make_isolated_root(tmp)
    log_a = os.path.join(tmp, "engineA.log")
    log_b = os.path.join(tmp, "engineB.log")
    chk = Checks()
    proc_a = proc_b = proc_c = None

    try:
        # ── Engine A: build a valid session carrying a genuinely dangling
        #    Lua AI reference, save it ──────────────────────────────────
        proc_a = boot_probe(root, args.port, log_a)
        bootstrap_defs(args.port)
        load_ai_stack(args.port)
        send(args.port,
             f"world.init('probe_world', {args.seed}, {args.size}, 3); return 'ok'")
        send(args.port, "return world.waitForInit(180)", timeout=190)
        send(args.port, "world.show('probe_world'); return 'ok'")
        send(args.port, "return world.loadChunksInRegion(-2,-2,2,2)")
        send(args.port, "return world.waitForChunks(120)", timeout=125)

        strip = find_flat_strip(args.port)
        if not strip:
            sys.exit("FAIL: no flat dry ground found near origin")
        gx, gy, z = strip
        print(f"probe_world: flat strip at ({gx},{gy}) z={z}")

        a = int(float(send(args.port,
            f"return unit.spawn('acolyte', {gx}, {gy}, {z}, 'player')")))
        b = int(float(send(args.port,
            f"return unit.spawn('acolyte', {gx + 2}, {gy}, {z}, 'player')")))
        if a < 0 or b < 0:
            sys.exit(f"FAIL: unit spawn rejected (a={a}, b={b})")
        print(f"spawned unit #{a} (attacker) and unit #{b} (soon-destroyed target)")
        time.sleep(1.0)  # let both units settle onto the ground

        # ── #1484: a cached buildTarget whose site died must be gone
        #    from the LIVE AI state before the save, not tolerated at
        #    the boundary. Runs BEFORE the pause below, because it is
        #    the AI's own ticking that has to do the clearing.
        print("\n--- a cached build target clears when its site dies (#1484) ---")
        site = find_build_site(args.port, gx, gy)
        if not site:
            sys.exit("FAIL (setup): no placeable build-site tile found near "
                      f"({gx},{gy})")
        bx, by = site
        bid_raw = send(args.port,
            f"return building.spawn('{BUILD_SITE_DEF}', {bx}, {by})")
        try:
            bid = int(float(bid_raw))
        except (TypeError, ValueError):
            sys.exit(f"FAIL (setup): building.spawn returned {bid_raw!r}")
        # The AI only targets a site that is still APPEARING with build
        # work outstanding; assert that rather than assuming it, so a
        # def change that made this instant-built fails loudly here
        # instead of making the whole scenario vacuous.
        #
        # `building.spawn` only QUEUES a BuildingSpawn, so until the
        # building thread drains it both accessors answer nil -- and
        # "nil,nil" is a perfectly truthy Python string. Poll on the
        # WANTED shape rather than on any answer at all, or the very
        # first sample ends the wait and the assertion below aborts a
        # setup that was merely not ready yet.
        def site_state() -> str:
            return send(args.port,
                f"return tostring(building.getActivity({bid})) .. ',' .. "
                f"tostring(building.getBuildRequired({bid}))").strip('"')

        def site_is_a_build_target() -> str | None:
            """`site_state()` once it reads "constructing,<work>" with the
            work above zero -- the exact shape `findNearestUnbuilt`
            accepts -- and None on every other answer, so the wait keeps
            going instead of ending on the first sample."""
            v = site_state()
            activity, _, work = v.partition(",")
            if activity != "constructing":
                return None
            try:
                return v if float(work) > 0 else None
            except ValueError:
                return None

        appearing = poll_until(20.0, site_is_a_build_target)
        if not appearing:
            sys.exit(f"FAIL (setup): building #{bid} is {site_state()!r}, "
                      f"expected an appearing site with build work outstanding")
        # Bind to a local first: getTerrainAt yields more than one value,
        # and only the first (surface z) is wanted here.
        bz = int(float(send(args.port,
            f"local z = world.getTerrainAt({bx + 1}, {by}); return z")))
        c = int(float(send(args.port,
            f"return unit.spawn('acolyte', {bx + 1}, {by}, {bz}, 'player')")))
        if c < 0:
            sys.exit(f"FAIL (setup): builder spawn rejected ({c})")
        clear_find_water(args.port, c)
        print(f"staked build site #{bid} at ({bx},{by}); builder is unit #{c}")

        # buildNearbyUtility runs for every candidate on every thought
        # tick, so the cache is populated whether or not build_nearby
        # WINS arbitration -- no need to steer the unit into building.
        acquired = poll_until(45.0,
            lambda: ai_build_target(args.port, c) == str(bid))
        if not acquired:
            sys.exit(f"FAIL (setup): unit #{c} never cached build site #{bid} "
                      f"(buildTarget={ai_build_target(args.port, c)!r})")
        print(f"unit #{c} cached buildTarget={bid}")

        destroyed_b = send(args.port, f"return building.destroy({bid})")
        if destroyed_b.strip() != "true":
            sys.exit(f"FAIL: building.destroy({bid}) returned {destroyed_b!r}")
        # building.destroy only QUEUES a BuildingDestroy; wait until the
        # building is observably gone before counting the cleanup tick,
        # or the poll below can be satisfied (or not) by ticks that ran
        # while the site still resolved.
        gone = poll_until(30.0, lambda: send(
            args.port, f"return building.getInfo({bid}) == nil") == "true")
        chk.ok(bool(gone), f"build site #{bid} is observably destroyed")

        cleared = poll_until(45.0,
            lambda: ai_build_target(args.port, c) == "nil")
        chk.ok(bool(cleared),
               f"unit #{c}'s cached buildTarget is cleared at runtime, without "
               f"waiting for a save and load "
               f"(buildTarget={ai_build_target(args.port, c)!r})")

        # Pause BEFORE committing the target: unit_ai.update's own tick
        # already self-heals a dead attackTargetUid the instant it next
        # runs ("the AI already runs when a target legitimately
        # vanishes" -- scripts/unit_ai.lua's scrubStaleRefs comment),
        # which would race unit.destroy()+the save below and sometimes
        # save a CLEAN aiState instead of the dangling one this scenario
        # needs. Pausing makes unitAi.update() a no-op for the whole
        # window (it checks pause.isPaused() first), so the reference
        # provably survives to the save unmodified.
        send(args.port, "engine.setPaused(true); return 'ok'")
        send(args.port, f"require('scripts.unit_ai').commandAttack({a},{b}); return 'go'")
        target = send(args.port,
            f"local s = require('scripts.unit_ai').getState({a}); "
            f"return s and s.attackTargetUid or -1")
        if target.strip() != str(b):
            sys.exit(f"FAIL (setup): unit #{a}'s attackTargetUid is "
                      f"{target!r}, expected {b}")

        destroyed = send(args.port, f"return unit.destroy({b})")
        if destroyed.strip() != "true":
            sys.exit(f"FAIL: unit.destroy({b}) returned {destroyed!r}")
        print(f"unit #{b} destroyed -- unit #{a} now carries a genuinely "
              f"dangling AI reference")

        saved = send(args.port, f"return engine.saveWorld('probe_world', '{SLOT}')")
        if saved.strip() != "true":
            sys.exit(f"FAIL: engine.saveWorld returned {saved!r}")
        save_file = os.path.join(root, "saves", SLOT, "world.synworld")
        for _ in range(100):
            if os.path.exists(save_file):
                break
            time.sleep(0.1)
        if not os.path.exists(save_file):
            sys.exit(f"FAIL: save file never appeared at {save_file}")
        with open(save_file, "rb") as f:
            clean_bytes = f.read()
        print(f"saved -> {save_file} ({len(clean_bytes)} bytes)")

        quit_engine(args.port, proc_a)
        proc_a = None

        print("\n--- pre-save boundary sees the same dangling reference ---")
        # The dangling reference already existed at SAVE time (unit #b
        # was destroyed before engine.saveWorld ran), so the pre-save
        # integrity check (World.Thread.Command.Save.WriteWorld, over
        # the SAME live snapshot saveModules.snapshotAll() captured)
        # must have logged the identical diagnostic engine B's load-side
        # check reports -- proving save and load share one graph rather
        # than only the load boundary being checked.
        log_a_text = read_log(log_a)
        chk.ok("integrity diagnostic" in log_a_text,
               "engine A's log records an integrity diagnostic AT SAVE TIME")
        chk.ok("unit_ai" in log_a_text and "dangling-reference" in log_a_text,
               "the save-time diagnostic names unit_ai and is coded "
               "'dangling-reference'")

        # #1484's assertion, made against that same log: the two checks
        # above prove this save DOES report dangling unit_ai references,
        # so the absence below is a real absence rather than a silent
        # log. Matched on the full (component, path, code, kind, value)
        # tuple -- check 1's own attackTargetUid diagnostic is in here
        # too and must not satisfy it.
        stale = dangling_build_target_lines(log_a_text, c, bid)
        chk.ok(not stale,
               f"the save carries NO 'unit[{c}].buildTarget -> building "
               f"{bid}' dangling-reference diagnostic"
               + (f" (found: {stale[0]!r})" if stale else ""))

        # ── Engine B: fresh restart. Load the valid save; the dangling
        #    reference must be diagnosed, never load-blocking ─────────
        proc_b = boot_probe(root, args.port, log_b)
        bootstrap_defs(args.port)
        load_ai_stack(args.port)

        print("\n--- tolerated dangling Lua reference ---")
        loaded = send(args.port, f"return engine.loadSave('{SLOT}')")
        chk.ok(loaded.strip() == "true",
               f"engine.loadSave accepted the request ({loaded!r})")
        published, status = wait_load_published(args.port)
        chk.ok(published,
               f"a dangling (tolerated) Lua reference does NOT block the "
               f"load ({status})")
        chk.ok(send(args.port, f"return unit.exists({a})") == "true",
               f"unit #{a} survived the load despite carrying a dangling "
               f"reference")

        # engine B's log is only GUARANTEED flushed to disk once the
        # process shuts down (its handle has no per-line flush -- a
        # plain redirected-to-file stdio handle block-buffers) -- quit
        # it now and read the log AFTER, rather than racing an
        # in-process read against however much output has flushed so
        # far (which flakes: the diagnostic line's own buffer segment
        # may not flush again until more output accumulates).
        quit_engine(args.port, proc_b)
        proc_b = None
        log_text = read_log(log_b)
        chk.ok("integrity diagnostic" in log_text,
               "engine B's log records at least one integrity diagnostic")
        chk.ok("unit_ai" in log_text and "dangling-reference" in log_text,
               "the diagnostic names the unit_ai component and is coded "
               "'dangling-reference'")
        chk.ok(f" {b} " in log_text or f" {b}\n" in log_text or f"unit {b}" in log_text,
               f"the diagnostic names the destroyed unit's id (#{b})")

        # ── Engine C: fresh restart, re-establish the SAME live session
        #    (the valid save loads cleanly again), then prove a corrupted
        #    save is rejected WITHOUT touching that already-loaded
        #    session — leaving it unchanged and paused ─────────────────
        print("\n--- corrupted save is rejected without touching the live session ---")
        log_c = os.path.join(tmp, "engineC.log")
        proc_c = boot_probe(root, args.port, log_c)
        bootstrap_defs(args.port)
        load_ai_stack(args.port)
        loaded_c = send(args.port, f"return engine.loadSave('{SLOT}')")
        if loaded_c.strip() != "true":
            sys.exit(f"FAIL (setup): engine.loadSave returned {loaded_c!r}")
        published_c, status_c = wait_load_published(args.port)
        if not published_c:
            sys.exit(f"FAIL (setup): valid load did not publish: {status_c}")

        marker_active = send(args.port, "return world.getActiveWorldId()")
        marker_unit_a = send(args.port, f"return unit.exists({a})")

        corrupt_dir = os.path.join(root, "saves", CORRUPT_SLOT)
        os.makedirs(corrupt_dir, exist_ok=True)
        with open(os.path.join(corrupt_dir, "world.synworld"), "wb") as f:
            f.write(clean_bytes[: len(clean_bytes) // 2])

        bad_load = send(args.port, f"return engine.loadSave('{CORRUPT_SLOT}')")
        bad_status = send_json(args.port, "return engine.getLoadStatus()")
        # A structural decode failure is detected synchronously inside
        # engine.loadSave itself (World.Save.Serialize.loadWorld runs
        # before anything is queued to the world thread) -- but poll
        # briefly in case this build's failure mode is async instead, so
        # the assertion below doesn't race a slower failure path.
        if bad_load.strip() == "true":
            _, bad_status = wait_load_published(args.port)
        chk.ok(bad_load.strip() != "true"
               or (isinstance(bad_status, dict) and bad_status.get("phase") == "LoadFailed"),
               f"engine.loadSave rejects a truncated save (loadSave={bad_load!r}, "
               f"status={bad_status})")
        chk.ok(isinstance(bad_status, dict) and bad_status.get("phase") == "LoadFailed",
               f"engine.getLoadStatus() reports LoadFailed ({bad_status})")
        chk.ok(send(args.port, "return engine.isPaused()") == "true",
               "the engine stays paused after a rejected load")
        chk.ok(send(args.port, "return world.getActiveWorldId()") == marker_active,
               "the live session's active page is UNCHANGED after the "
               "rejected load")
        chk.ok(send(args.port, f"return unit.exists({a})") == marker_unit_a,
               "the live session's unit state is UNCHANGED after the "
               "rejected load")

        print(f"\n{'PASS' if chk.failed == 0 else 'FAIL'}: "
              f"{chk.failed} check(s) failed")
        return 0 if chk.failed == 0 else 1

    finally:
        if proc_a is not None:
            quit_engine(args.port, proc_a)
        if proc_b is not None:
            quit_engine(args.port, proc_b)
        if proc_c is not None:
            quit_engine(args.port, proc_c)
        shutil.rmtree(tmp, ignore_errors=True)


if __name__ == "__main__":
    sys.exit(main())
