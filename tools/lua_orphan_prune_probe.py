#!/usr/bin/env python3
"""Headless regression probe for #195 — Lua per-id state surviving a load.

Lua save-module blobs (unit_ai aiState, building_spawn state) are
restored BEFORE the engine load path settles. A unit destroyed before a
save is never in that save's survivor set, so its stale Lua-side AI/spawn
state must not linger past the load (a later id reuse could otherwise
inherit it). The fix has the engine emit a post-load signal (LuaSaveLoaded
→ broadcast `onSaveLoaded`); the modules reconcile their per-id state
against live entities (unit.exists / building.getInfo, both GLOBAL).

Issue #763 (save-overhaul C2) replaced the incrementally-merging load path
this reconcile was originally built against: a load now REPLACES THE
COMPLETE SESSION, so `onSaveLoaded`'s survivor lists always name every
unit/building in the whole new session — there is no more "off-page"
subset. Issue #900 then retired the off-page-preservation branch (and the
`_preLoadState` snapshot it read) outright, replacing it with per-entity
application: each row is resolved against the restored session's own
entity set at apply time, and a row whose owner is absent is dropped with
a diagnostic. Part (ii) below now covers THAT contract against the real
unit_ai registration; the destroy-before-save leak (parts 1-5) and the
nested-ref scrub on survivors (part (i)) remain exactly as live as
before. Separately, #763 also changed a MISSING GAMEPLAY
DEFINITION (unlike a destroyed unit, which is simply absent from the
save) from "silently pruned" to "rejects the whole load, old session
untouched" — that scenario is covered by
tools/transactional_load_probe.py instead, not here.

This probe reproduces the leak with a REAL save/load and asserts the
post-load reconcile prunes it:

  1. spawn unit A on flat ground, let the AI tick so aiState[A] exists,
  2. destroy A  → A no longer exists, but aiState[A] lingers (the leak),
  3. spawn unit B and keep it alive (control — its state must survive),
  4. plant a stale reference from EVERY family unit_ai_save_refs.lua
     declares onto B, while paused so the AI cannot self-heal them,
  5. saveWorld + loadSave  → the engine fires onSaveLoaded after the
     load settles,
  6. assert aiState[A] was pruned, aiState[B] preserved, and every
     planted stale reference cleared.

Step 4/6 is issue #1589. Before it, this probe planted only
attackTargetUid and treatPending and still printed a general "stale
nested references scrubbed" pass, while six declared families
(craftJob, repairJob, pickupOrder, a ground forageTarget, forageLoot
and harvestLoot) crossed the load boundary untouched. The planting
happens BEFORE the save and the assertions read the state the engine's
own automatic broadcast left behind — calling unitAi.onSaveLoaded by
hand afterwards would skip the new Haskell→Lua reconciliation-context
carrier entirely, which is the half that decides a PER-PAGE id against
the owning unit's page.

`unitAi.getState(uid)` is the public observation hook (returns the live
aiState entry or nil). Exit 0 = fix verified.

Usage:  python3 tools/lua_orphan_prune_probe.py [--port 9008] [--seed 42]

This probe implements the shared `probe-result/v1` contract: `--describe`
prints its ordered stable checks without booting an engine, and a harnessed
run writes structured events while a standalone run keeps its human-readable
per-check output.
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
import probe_protocol
from probelib import quit_engine, boot, send, wait_load_published

LOG = "/tmp/orphan_prune_engine.log"
LOG_NAME = "orphan_prune_engine.log"
PROBE_KEY = "lua_orphan_prune"
CHECKS = [
    ("snapshot_filters_orphan", "the save snapshot excludes the destroyed unit and keeps the survivor"),
    ("load_pauses_immediately", "loadSave pauses the engine before the load transaction runs"),
    ("load_reconcile_prunes_orphan", "post-load reconcile prunes the orphan and keeps survivor state"),
    ("nested_references_scrubbed", "stale nested references and their collection phases are scrubbed"),
    ("per_entity_apply", "per-entity apply drops an absent owner and exactly replaces the survivor row"),
]
DESCRIPTOR = probe_protocol.build_descriptor(PROBE_KEY, CHECKS)
# Unique per run, deleted on exit. A fixed name could clobber a real save
# and a stale dir from an interrupted run could make a later run falsely
# pass by loading old data; main() also refuses to run if the dir exists.
SAVE_NAME = "probe_orphan_" + uuid.uuid4().hex[:12]


def bootstrap_defs(port: int) -> None:
    loaders = [
        ("data/substances/*.yaml", "engine.loadSubstanceYaml"),
        ("data/items/*.yaml",      "engine.loadItemYaml"),
        ("data/equipment/*.yaml",  "engine.loadEquipmentYaml"),
        ("data/materials/*.yaml",  "engine.loadMaterialYaml"),
        ("data/units/*.yaml",      "engine.loadUnitYaml"),
        # #1589: a planted craftJob/repairJob persists a RECIPE id, and
        # prepareLoad rejects the whole load if it no longer resolves.
        ("data/recipes/*.yaml",    "engine.loadRecipeYaml"),
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


# #1589: the stale ENTITY ids planted on the survivor before the save.
# Deliberately far above every allocator this scenario can reach, so no
# real entity in the restored session can ever answer to one of them.
FAKE_UNIT = 987654
FAKE_BILL = 987655
FAKE_BUILDING = 987656
FAKE_INSTANCE = 987657
# Ground-item ids are a ZERO-based per-page allocator (Item.Ground), so
# a low number here would be a plausible real gid rather than a stale
# one. FAKE_GID..FAKE_GID+4 are used.
FAKE_GID = 987700


# Every declared reference family planted on the survivor, in the order
# present_families() reports them. The pre-save check requires exactly
# this string (an unplantable family would make the post-load all-clear
# vacuous); the post-load check requires the empty string.
PLANTED_FAMILIES = ("attackTargetUid,treatPending,craftJob,repairJob,"
                    "pickupOrder,forageTarget,forageLoot,harvestLoot")


def present_families(port: int, uid: int) -> str:
    """Which of the planted stale families are still present on `uid`?

    A comma-separated list in PLANTED_FAMILIES order, "" when none is,
    or "NOSTATE" when the row itself is gone. Used twice: before the
    save (proving the plant took) and after the load (proving the
    reconcile cleared every one).
    """
    return send(port,
        f"local s=require('scripts.unit_ai').getState({uid}); "
        "if not s then return 'NOSTATE' end; "
        "local held={}; "
        "local function chk(name, present) "
        "  if present then held[#held+1]=name end end; "
        "chk('attackTargetUid', s.attackTargetUid ~= nil); "
        "chk('treatPending', s.treatPending ~= nil); "
        "chk('craftJob', s.craftJob ~= nil); "
        "chk('repairJob', s.repairJob ~= nil); "
        "chk('pickupOrder', s.pickupOrder ~= nil); "
        "chk('forageTarget', s.forageTarget ~= nil); "
        "chk('forageLoot', s.forageLoot ~= nil); "
        "chk('harvestLoot', s.harvestLoot ~= nil); "
        "return table.concat(held, ',')").strip().strip('"')


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
    ap.add_argument("--describe", action="store_true")
    args = ap.parse_args()
    if args.describe:
        print(DESCRIPTOR.to_json())
        return 0
    rep = probe_protocol.reporter_from_env(DESCRIPTOR)
    try:
        return _run(args, rep)
    finally:
        rep.close()


def _run(args, rep: probe_protocol.Reporter) -> int:
    save_dir = os.path.join("saves", SAVE_NAME)
    if os.path.exists(save_dir):
        rep.abort(f"refusing to run: {save_dir} already exists")
        return 2

    proc = boot(args.port, log=rep.engine_log_path(LOG_NAME, LOG),
                args=rep.engine_args())
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
            rep.abort("no flat dry ground found to spawn on")
            return 2
        gx, gy = spot
        rep.note(f"Spawning on flat ground at ({gx},{gy})")

        a = int(float(send(args.port, f"return unit.spawn('acolyte', {gx}, {gy})")))
        b = int(float(send(args.port, f"return unit.spawn('acolyte', {gx+1}, {gy})")))
        rep.note(f"Spawned A={a} (to be orphaned), B={b} (control)")

        # Let the AI tick so aiState[A]/[B] are created.
        time.sleep(3.0)
        sa, sb = getstate(args.port, a), getstate(args.port, b)
        rep.note(f"After tick: aiState[A]={sa}, aiState[B]={sb}")
        if sa != "present" or sb != "present":
            rep.abort("AI state was not created for both spawned units",
                      {"orphan": sa, "survivor": sb})
            return 2

        # Destroy A. It's queued — wait until the unit thread drops it.
        send(args.port, f"unit.destroy({a}); return 'ok'", expect_result=False)
        for _ in range(20):
            if not exists(args.port, a):
                break
            time.sleep(0.3)
        if exists(args.port, a):
            rep.abort("unit A never got destroyed")
            return 2
        leaked = getstate(args.port, a)
        rep.note(f"After destroy: unit.exists(A)=no, aiState[A]={leaked} "
                 f"(lingering in memory = the leak)")

        # Snapshot filter: even though aiState[A] lingers in memory, the
        # SAVE payload must exclude it. Otherwise, on a cross-session load, A's
        # dead loaded-page id could collide with a live off-page unit and be
        # misattributed (onSaveLoaded can't tell stale loaded-page leftovers
        # from off-page state — the payload isn't page-keyed). Run the
        # registered unit_ai component's snapshot() (via saveModules.snapshotAll,
        # issue #761) and confirm A is filtered out while B is kept.
        blobcheck = send(args.port,
            "local sm=require('scripts.lib.save_modules'); "
            "local codec=require('scripts.lib.data_codec'); "
            "local snap=sm.snapshotAll(); "
            "local payload; "
            "for _,c in ipairs(snap.components) do "
            "if c.id=='unit_ai' then payload=c.payload end end; "
            "local r=codec.decode(payload) or {}; "
            "local ha,hb=false,false; "
            f"for k in pairs(r) do local n=tonumber(k); "
            f"if n=={a} then ha=true elseif n=={b} then hb=true end end; "
            "return (ha and 'present' or 'absent')..','..(hb and 'present' or 'absent')")
        rep.note(f"Snapshotted unit_ai component: A={blobcheck.split(',')[0]}, "
                 f"B={blobcheck.split(',')[-1]}")
        snapshot_ok = blobcheck == "absent,present"
        ok &= rep.check("snapshot_filters_orphan", snapshot_ok,
                        ("destroyed unit excluded and survivor retained"
                         if snapshot_ok else
                         "destroyed unit leaked into the snapshot or survivor was missing"),
                        {"snapshot": blobcheck})

        # #1589: plant one stale reference from EVERY declared family on
        # the SURVIVOR, before the save, so the post-load assertions read
        # what the engine's own automatic onSaveLoaded broadcast did.
        #
        # Paused first, and this matters: several of these families
        # self-heal on the very next thought tick (craftUtility drops a
        # job whose bill has vanished, pickupUtility retires an order
        # whose ground item is gone), so an unpaused plant would be
        # cleared before it ever reached the save and the whole check
        # would pass vacuously.
        #
        # The content ids are REAL (a live recipe, live item defs) while
        # the ENTITY ids are not: a content reference that no longer
        # resolves is rejected by prepareLoad before any live state is
        # touched, whereas a dangling entity reference is exactly the
        # tolerated case this probe exists to see cleared at reconcile.
        ai = "require('scripts.unit_ai')"
        if ok:
            send(args.port, "engine.setPaused(true); return 'ok'",
                 expect_result=False)
            send(args.port,
                 f"local s={ai}.getState({b}); "
                 f"s.attackTargetUid={FAKE_UNIT}; "
                 f"s.treatPending={{uid={FAKE_UNIT}}}; "
                 f"s.craftJob={{billId={FAKE_BILL},bid={FAKE_BUILDING},"
                 f"recipeId='forge_steel_dagger'}}; "
                 f"s.repairJob={{instanceId={FAKE_INSTANCE},"
                 f"recipeId='repair_sharpness',defName='axe_steel',"
                 f"consumable='whetstone'}}; "
                 f"s.pickupOrder={{gid={FAKE_GID},issuedAt=engine.gameTime()}}; "
                 f"s.forageTarget={{kind='ground',gid={FAKE_GID + 1},x=0,y=0}}; "
                 f"s.forageLoot={{{FAKE_GID + 2},{FAKE_GID + 3}}}; "
                 f"s.foragePhase='collecting'; "
                 f"s.harvestLoot={{{FAKE_GID + 4}}}; "
                 f"s.harvestPhase='collecting'; return 'ok'",
                 expect_result=False)
            planted = present_families(args.port, b)
            rep.note(f"Planted stale families on B: {planted}")
            if planted != PLANTED_FAMILIES:
                rep.abort("could not plant every stale reference family",
                          {"actual": planted, "expected": PLANTED_FAMILIES})
                return 2

        # Save + load. The engine fires onSaveLoaded after the load
        # settles; the reconcile should prune A while keeping B.
        save_cmd = f'return engine.saveWorld("arena","{SAVE_NAME}")'
        rep.note(f"saveWorld -> {send(args.port, save_cmd)}")
        # saveWorld returns on enqueue — wait for the file to actually land
        # before loading, or loadSave races the write / reads a stale dir.
        if not wait_save_written(SAVE_NAME):
            rep.abort(f"save file for '{SAVE_NAME}' never appeared on disk")
            return 2
        # Unpause first so the load-time freeze is observable: loadSave must
        # pause the engine synchronously (before queueing WorldLoadSave) so
        # the Lua loop can't tick script update()s against the half-restored
        # singletons during the load window.
        send(args.port, "engine.setPaused(false); return 'ok'", expect_result=False)
        load_cmd = f'return engine.loadSave("{SAVE_NAME}")'
        rep.note(f"loadSave -> {send(args.port, load_cmd)}")
        # Right after loadSave returns (world thread hasn't finished the load),
        # the engine must already be paused — frozen for the load window.
        load_paused = send(args.port, "return engine.isPaused()")
        rep.note(f"engine.isPaused() immediately after loadSave -> {load_paused}")
        paused_ok = load_paused.strip().lower() in ("true", "1", "1.0")
        ok &= rep.check("load_pauses_immediately", paused_ok,
                        ("loadSave paused the engine immediately"
                         if paused_ok else
                         "loadSave did not pause before the load transaction"),
                        {"paused": load_paused})
        # Issue #763: loadSave only ACCEPTS synchronously -- the saved page
        # ("arena", its own id verbatim -- no more main_world remap)
        # doesn't exist live until the transaction publishes.
        published, status = wait_load_published(args.port, 180)
        rep.note(f"load transaction published -> {published} ({status})")
        if not published:
            rep.abort("load transaction did not publish", {"status": status})
            return 2
        # Block on init, then let the load settle past LoadDone (the world
        # thread restores units, then enqueues the LuaSaveLoaded broadcast).
        send(args.port, "return world.waitForInit(180)", timeout=190)
        time.sleep(2.0)
        send(args.port, "world.show('arena'); return 'ok'", expect_result=False)

        # Poll until the reconcile has run (aiState[A] pruned) or timeout.
        pruned = False
        for _ in range(40):
            if getstate(args.port, a) == "nil":
                pruned = True
                break
            time.sleep(0.5)
        final_a = getstate(args.port, a)
        final_b = getstate(args.port, b)
        rep.note(f"After load: aiState[A]={final_a}, aiState[B]={final_b}")
        reconcile_ok = pruned and final_a == "nil" and final_b == "present"
        ok &= rep.check("load_reconcile_prunes_orphan", reconcile_ok,
                        ("dropped-unit AI state pruned and survivor state kept"
                         if reconcile_ok else
                         "post-load reconcile did not preserve exactly the survivor state"),
                        {"orphan": final_a, "survivor": final_b,
                         "pruned": pruned})

        # (i) Nested-reference scrub on a loaded-page survivor (#195,
        # extended to every declared family by #1589). A survivor can
        # embed a stale id in any of the reference families
        # unit_ai_save_refs.lua's REF_SCHEMA declares; a loaded-page unit
        # can only validly reference a page-mate (and, for the per-page
        # kinds, an entity on its OWN page), so every id planted above is
        # stale and must be gone. These read the state the engine's own
        # automatic onSaveLoaded broadcast left — nothing here calls the
        # callback by hand, which is what makes the Haskell-side
        # reconciliation context part of what is under test.
        if ok:
            remaining = present_families(args.port, b)
            rep.note(f"Nested scrub: stale families still present -> {remaining}")
            phases = None
            if remaining == "":
                # The collection families clear their owning phase on the
                # same rule their own exhaustion path uses; a phase left
                # behind with no list is the malformed leftover #1589
                # requirement 3 forbids.
                phases = send(args.port,
                    f"local s=require('scripts.unit_ai').getState({b}); "
                    "return tostring(s.foragePhase) .. ',' "
                    ".. tostring(s.harvestPhase)").strip().strip('"')
                rep.note(f"Collection phases after scrub -> {phases}")
            nested_ok = remaining == "" and phases == "nil,nil"
            ok &= rep.check("nested_references_scrubbed", nested_ok,
                            ("stale nested references and collection phases were scrubbed"
                             if nested_ok else
                             "stale nested references or collection phases survived"),
                            {"remaining": remaining, "phases": phases})

        # (ii) Per-entity apply (issue #900). The registered component's
        # apply() now resolves EACH ROW against the restored session's own
        # entity set instead of clobbering the singleton wholesale and
        # reconciling afterward. This drives the REAL apply path, against
        # the REAL unit_ai registration in a real engine -- the generic
        # mechanism itself is covered by the "per-entity component
        # application" hspec cases, which run in a bare Lua VM that cannot
        # load unit_ai at all.
        #   * mark B's live state (probeMarker),
        #   * apply() a payload carrying B plus a fake DEAD id, with an
        #     entity context naming B but NOT the dead id.
        # Expect: the dead id's row is DROPPED rather than applied, B holds
        # exactly the payload's row, and the pre-load marker is gone --
        # per-entity does not mean merge-into-live, which is what stops a
        # reused id from inheriting the previous session's state.
        #
        # This replaces the pre-#900 off-page-preservation case: that
        # branch tested `_preLoadState`, which is retired. #763 had already
        # made it dead code in normal operation (a load replaces the
        # complete session), and its guarantee is now carried by apply-time
        # ownership instead.
        DEAD = 998877
        if ok:
            send(args.port,
                 f"local s={ai}.getState({b}); if s then s.probeMarker=777 end; "
                 f"return 'ok'", expect_result=False)
            send(args.port,
                 "local sm=require('scripts.lib.save_modules'); "
                 "local reg=sm.registry.unit_ai; "
                 f"local stale={{[{b}]={{currentAction='idle'}},"
                 f"[{DEAD}]={{currentAction='attack'}}}}; "
                 f"reg.apply(reg.decode(reg.version, stale), "
                 f"{{unit={{[{b}]=true}}, building={{}}}}); return 'ok'",
                 expect_result=False)
            dead = send(args.port, f"local s={ai}.getState({DEAD}); "
                 f"return s and 'present' or 'nil'")
            act = send(args.port, f"local s={ai}.getState({b}); "
                 f"return s and tostring(s.currentAction) or 'NOSTATE'")
            mk = send(args.port, f"local s={ai}.getState({b}); "
                 f"return s and tostring(s.probeMarker) or 'NOSTATE'")
            be = exists(args.port, b)
            rep.note(f"Per-entity apply: dead-id state -> {dead}, "
                     f"B.currentAction -> {act}, B.probeMarker -> {mk}, "
                     f"B alive -> {'yes' if be else 'no'}")
            apply_ok = dead == "nil" and act == "idle" and mk == "nil" and be
            ok &= rep.check("per_entity_apply", apply_ok,
                            ("absent-owner row dropped and survivor row applied exactly"
                             if apply_ok else
                             "per-entity apply did not exactly replace the survivor row"),
                            {"dead_owner": dead, "survivor_action": act,
                             "survivor_marker": mk, "survivor_alive": be})
    finally:
        quit_engine(args.port, proc)
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
