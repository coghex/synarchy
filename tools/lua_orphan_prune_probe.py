#!/usr/bin/env python3
"""Headless regression probe for #195 — Lua per-id state surviving a load.

Lua save-module blobs (unit_ai aiState, building_spawn state) are
restored BEFORE the engine load path, which can drop units/buildings
whose defs are no longer registered. The Lua side used to keep state for
those dropped ids, so a later id reuse could inherit stale AI/spawn
state. The fix has the engine emit a post-load signal (LuaSaveLoaded →
broadcast `onSaveLoaded`); the modules reconcile their per-id state
against live entities (unit.exists / building.getInfo, both GLOBAL).

This probe reproduces the leak with a REAL save/load and asserts the
post-load reconcile prunes it:

  1. spawn unit A on flat ground, let the AI tick so aiState[A] exists,
  2. destroy A  → A no longer exists, but aiState[A] lingers (the leak),
  3. spawn unit B and keep it alive (control — its state must survive),
  4. saveWorld + loadSave  → the engine fires onSaveLoaded after the
     load settles,
  5. assert aiState[A] was pruned and aiState[B] preserved.

`unitAi.getState(uid)` is the public observation hook (returns the live
aiState entry or nil). Exit 0 = fix verified.

Usage:  python3 tools/lua_orphan_prune_probe.py [--port 9008] [--seed 42]
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

LOG = "/tmp/orphan_prune_engine.log"
# Unique per run, deleted on exit. A fixed name could clobber a real save
# and a stale dir from an interrupted run could make a later run falsely
# pass by loading old data; main() also refuses to run if the dir exists.
SAVE_NAME = "probe_orphan_" + uuid.uuid4().hex[:12]


def _results(raw: bytes) -> list[str]:
    # The console emits a "synarchy debug console\n> " banner on connect and
    # a trailing "> " prompt; both yield EMPTY "> " lines. The real answer is
    # the non-empty "> <value>" line, so filter the empties out.
    out = raw.decode(errors="replace")
    return [ln[2:].strip() for ln in out.splitlines()
            if ln.startswith("> ") and ln[2:].strip()]


def send(port: int, lua: str, timeout: float = 10.0,
         expect_result: bool = True) -> str:
    """Run one Lua line over the debug console and return its result.

    With expect_result (a `return ...` command) we read until a non-empty
    "> value" line appears, waiting up to `timeout` — this skips the banner
    and survives server-side BLOCKING calls like world.waitForInit /
    waitForChunks, which emit nothing until they unblock. Fire-and-forget
    commands (no return) pass expect_result=False and just drain briefly.
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


def boot(port: int) -> subprocess.Popen:
    log = open(LOG, "w")
    proc = subprocess.Popen(
        ["cabal", "run", "-v0", "exe:synarchy", "--",
         "--headless", "--port", str(port)],
        stdout=log, stderr=subprocess.STDOUT,
    )
    deadline = time.time() + 240
    while time.time() < deadline:
        try:
            if "READY" in open(LOG).read():
                return proc
        except FileNotFoundError:
            pass
        if proc.poll() is not None:
            sys.exit(f"engine exited before READY; see {LOG}")
        time.sleep(0.4)
    proc.kill()
    sys.exit("engine never printed READY")


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


def wait_save_written(name: str, timeout: float = 30.0) -> bool:
    """Block until the save file lands on disk. saveWorld() returns as soon
    as the WorldSave command is QUEUED; loading before the file exists
    would 'Save not found' or read a stale dir from a previous run."""
    path = os.path.join("saves", name, "world.synworld")
    deadline = time.time() + timeout
    while time.time() < deadline:
        if os.path.isfile(path):
            return True
        time.sleep(0.3)
    return False


def find_flat(port: int) -> tuple[int, int] | None:
    """Find two adjacent dry equal-z land tiles for two spawns."""
    lua = (
        "local function f() for gy=-8,8 do for gx=-8,6 do "
        "local za=world.getTerrainAt(gx,gy) local zb=world.getTerrainAt(gx+1,gy) "
        "if za and zb and za==zb and za>0 then "
        "local fa=world.getFluidAt(gx,gy) local fb=world.getFluidAt(gx+1,gy) "
        "if (not fa or fa.type=='none') and (not fb or fb.type=='none') then "
        "return gx..','..gy end end end end return 'none' end return f()"
    )
    for _ in range(8):
        r = send(port, lua).strip().strip('"')
        if r and r != "none" and "," in r:
            gx, gy = r.split(",")
            return int(gx), int(gy)
        time.sleep(1.0)
    return None


def getstate(port: int, uid: int) -> str:
    return send(port,
        f"local s=require('scripts.unit_ai').getState({uid}); "
        f"return s and 'present' or 'nil'").strip().strip('"')


def exists(port: int, uid: int) -> bool:
    r = send(port, f"return unit.exists({uid}) and 'yes' or 'no'").strip().strip('"')
    return r == "yes"


def main() -> int:
    ap = argparse.ArgumentParser()
    ap.add_argument("--port", type=int, default=9008)
    ap.add_argument("--seed", type=int, default=42)
    ap.add_argument("--size", type=int, default=64)
    args = ap.parse_args()

    save_dir = os.path.join("saves", SAVE_NAME)
    if os.path.exists(save_dir):
        sys.exit(f"refusing to run: {save_dir} already exists")

    proc = boot(args.port)
    ok = True
    try:
        bootstrap_defs(args.port)
        send(args.port, f"world.init('arena', {args.seed}, {args.size}, 3); return 'ok'")
        send(args.port, "return world.waitForInit(180)", timeout=190)
        send(args.port, "world.show('arena'); return 'ok'", expect_result=False)
        send(args.port, "return world.loadChunksInRegion(-2,-2,2,2)")
        send(args.port, "return world.waitForChunks(60)", timeout=70)

        spot = find_flat(args.port)
        if not spot:
            print("FAIL: no flat dry ground found to spawn on")
            return 1
        gx, gy = spot
        print(f"Spawning on flat ground at ({gx},{gy})")

        a = int(float(send(args.port, f"return unit.spawn('acolyte', {gx}, {gy})")))
        b = int(float(send(args.port, f"return unit.spawn('acolyte', {gx+1}, {gy})")))
        print(f"Spawned A={a} (to be orphaned), B={b} (control)")

        # Let the AI tick so aiState[A]/[B] are created.
        time.sleep(3.0)
        sa, sb = getstate(args.port, a), getstate(args.port, b)
        print(f"After tick: aiState[A]={sa}, aiState[B]={sb}")
        if sa != "present" or sb != "present":
            print("FAIL: AI state was not created for spawned units "
                  "(can't exercise the leak)")
            return 1

        # Destroy A. It's queued — wait until the unit thread drops it.
        send(args.port, f"unit.destroy({a}); return 'ok'", expect_result=False)
        for _ in range(20):
            if not exists(args.port, a):
                break
            time.sleep(0.3)
        if exists(args.port, a):
            print("FAIL: unit A never got destroyed")
            return 1
        leaked = getstate(args.port, a)
        print(f"After destroy: unit.exists(A)=no, aiState[A]={leaked} "
              f"(lingering in memory = the leak)")

        # Serializer filter: even though aiState[A] lingers in memory, the
        # SAVE blob must exclude it. Otherwise, on a cross-session load, A's
        # dead loaded-page id could collide with a live off-page unit and be
        # misattributed (onSaveLoaded can't tell stale loaded-page leftovers
        # from off-page state — the blob isn't page-keyed). Run the registered
        # serializer and confirm A is filtered out while B is kept.
        blobcheck = send(args.port,
            "local sm=require('scripts.lib.save_modules'); "
            "local ser=require('scripts.lib.serialize'); "
            "local r=ser.deserialize(sm.serializeAll().unit_ai) or {}; "
            "local ha,hb=false,false; "
            f"for k in pairs(r) do local n=tonumber(k); "
            f"if n=={a} then ha=true elseif n=={b} then hb=true end end; "
            "return (ha and 'present' or 'absent')..','..(hb and 'present' or 'absent')")
        print(f"Serialized blob: A={blobcheck.split(',')[0]}, "
              f"B={blobcheck.split(',')[-1]}")
        if blobcheck != "absent,present":
            print("FAIL: destroyed unit A leaked into the save blob (or B "
                  "missing) — serializer filter not working")
            ok = False

        # Save + load. The engine fires onSaveLoaded after the load
        # settles; the reconcile should prune A while keeping B.
        print(f"saveWorld -> {send(args.port, f'return engine.saveWorld(\"arena\",\"{SAVE_NAME}\")')}")
        # saveWorld returns on enqueue — wait for the file to actually land
        # before loading, or loadSave races the write / reads a stale dir.
        if not wait_save_written(SAVE_NAME):
            print(f"FAIL: save file for '{SAVE_NAME}' never appeared on disk")
            return 1
        print(f"loadSave -> {send(args.port, f'return engine.loadSave(\"{SAVE_NAME}\")')}")
        # Block on init, then let the load settle past LoadDone (the world
        # thread restores units, then enqueues the LuaSaveLoaded broadcast).
        send(args.port, "return world.waitForInit(180)", timeout=190)
        time.sleep(2.0)
        send(args.port, "world.show('main_world'); return 'ok'", expect_result=False)

        # Poll until the reconcile has run (aiState[A] pruned) or timeout.
        pruned = False
        for _ in range(40):
            if getstate(args.port, a) == "nil":
                pruned = True
                break
            time.sleep(0.5)
        final_a = getstate(args.port, a)
        final_b = getstate(args.port, b)
        print(f"After load: aiState[A]={final_a}, aiState[B]={final_b}")

        if not pruned or final_a != "nil":
            print("FAIL: stale aiState[A] for the dropped unit was NOT "
                  "pruned after load (#195 not fixed)")
            ok = False
        elif final_b != "present":
            print("FAIL: aiState[B] for a LIVE unit was wrongly pruned "
                  "(reconcile too aggressive)")
            ok = False
        else:
            print("PASS: dropped-unit AI state pruned, live-unit state kept")

        # onSaveLoaded signature: (survUnitIds, survBuildingIds).
        ai = "require('scripts.unit_ai')"

        # (i) Nested-reference scrub on a loaded-page survivor: a survivor can
        # embed a stale id in a nested field (attackTargetUid / treatPending).
        # A loaded-page unit can only validly reference a page-mate, so an id
        # outside the survivor set is stale and must be scrubbed. Plant fakes
        # in B, call onSaveLoaded with B as the only survivor, assert refs
        # cleared while B's entry stays intact.
        FAKE = 987654
        if ok:
            send(args.port,
                 f"local s={ai}.getState({b}); "
                 f"if s then s.attackTargetUid={FAKE}; "
                 f"s.treatPending={{uid={FAKE}}} end; return 'ok'",
                 expect_result=False)
            send(args.port, f"{ai}.onSaveLoaded({{{b}}}, {{}}); return 'ok'",
                 expect_result=False)
            ref = send(args.port, f"local s={ai}.getState({b}); "
                 f"return s and tostring(s.attackTargetUid) or 'NOSTATE'")
            tp = send(args.port, f"local s={ai}.getState({b}); "
                 f"return s and tostring(s.treatPending) or 'NOSTATE'")
            print(f"Nested scrub: B.attackTargetUid -> {ref}, B.treatPending -> {tp}")
            if ref != "nil" or tp != "nil":
                print("FAIL: stale id in a survivor's nested field NOT scrubbed")
                ok = False
            elif getstate(args.port, b) != "present":
                print("FAIL: B's own state was wrongly dropped (B is a survivor)")
                ok = False
            else:
                print("PASS: stale nested references scrubbed, survivor kept")

        # (ii) Off-page state must NOT be rolled back by an older save, and a
        # stale blob id must NOT be kept. This drives the REAL blob-restore
        # path (the registered deserializer), not just onSaveLoaded:
        #   * mark B's live state (probeMarker),
        #   * run the deserializer with a STALE blob (B without the marker +
        #     a fake dead id) — it snapshots pre-load, clobbers, loads stale,
        #   * onSaveLoaded with B as NON-survivor (i.e. B is "off-page").
        # Expect: B keeps its pre-load marker (not the blob's stale copy), and
        # the fake dead id from the blob is dropped.
        DEAD = 998877
        if ok:
            send(args.port,
                 f"local s={ai}.getState({b}); if s then s.probeMarker=777 end; "
                 f"return 'ok'", expect_result=False)
            send(args.port,
                 "local sm=require('scripts.lib.save_modules'); "
                 "local ser=require('scripts.lib.serialize'); "
                 f"local stale=ser.serialize({{[{b}]={{currentAction='idle'}},"
                 f"[{DEAD}]={{currentAction='attack'}}}}); "
                 "sm.registry.unit_ai.deserialize(stale); return 'ok'",
                 expect_result=False)
            # B is live but NOT a loaded-page survivor → treated as off-page.
            send(args.port, f"{ai}.onSaveLoaded({{}}, {{}}); return 'ok'",
                 expect_result=False)
            mk = send(args.port, f"local s={ai}.getState({b}); "
                 f"return s and tostring(s.probeMarker) or 'NOSTATE'")
            dead = send(args.port, f"local s={ai}.getState({DEAD}); "
                 f"return s and 'present' or 'nil'")
            be = exists(args.port, b)
            print(f"Blob-restore reconcile: B.probeMarker -> {mk}, "
                  f"dead-id state -> {dead}, B alive -> {'yes' if be else 'no'}")
            if mk != "777":
                print("FAIL: live off-page unit's state was rolled back to the "
                      "save blob (lost pre-load state, review)")
                ok = False
            elif dead != "nil":
                print("FAIL: a stale blob id with no live entity was kept")
                ok = False
            elif not be:
                print("FAIL: reconcile unexpectedly destroyed the unit")
                ok = False
            else:
                print("PASS: off-page pre-load state preserved, stale blob id dropped")
    finally:
        try:
            send(args.port, "engine.quit()", timeout=3, expect_result=False)
        except OSError:
            pass
        try:
            proc.wait(timeout=15)
        except subprocess.TimeoutExpired:
            proc.kill()
        # Clean up the throwaway save. Safe: the name is unique to this run
        # and main() refused to start if the dir already existed.
        if os.path.isdir(save_dir):
            shutil.rmtree(save_dir, ignore_errors=True)

    return 0 if ok else 1


if __name__ == "__main__":
    sys.exit(main())
