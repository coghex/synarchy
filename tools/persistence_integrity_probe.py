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
     (`Engine.Scripting.Lua.API.Save`'s `knownEntitiesFromSaveData` /
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
import shutil
import sys
import tempfile
import time
from pathlib import Path

from probelib import boot, quit_engine, send, send_json, wait_load_published, load_ai_stack

REPO = Path(__file__).resolve().parent.parent
SLOT = "probe_integrity_slot"
CORRUPT_SLOT = "probe_integrity_corrupt"


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
