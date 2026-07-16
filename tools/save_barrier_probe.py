#!/usr/bin/env python3
"""Manual real-thread smoke probe for the #757 save boundary, extended
(#758) to also prove the immutable-snapshot capture properties:
a mutation completed before capture appears in the saved snapshot
(the pre-existing fluid-spread check below); a mutation made
immediately after the barrier releases does NOT alter that
already-captured save (only reachable now that #758 releases the
barrier before encode+disk I/O rather than after); and a later save
captures that later mutation as its own, distinct boundary. Also
proves (#758 review round 2 follow-up) that a genuine disk-level write
failure surfaces as a real SaveFailed outcome rather than crashing the
world thread or wedging the barrier open forever."""
from __future__ import annotations
import argparse, json, os, shutil, subprocess, sys, time, uuid
from probelib import boot, quit_engine, send

SAVE = "probe_barrier_" + uuid.uuid4().hex[:12]
RESAVE = SAVE + "_resave"
SAVE2 = SAVE + "_later"

def wait(predicate, what, timeout=30):
    end = time.time() + timeout
    while time.time() < end:
        value = predicate()
        if value: return value
        time.sleep(.2)
    raise RuntimeError("timed out waiting for " + what)

def fluid_at(port, gx, gy):
    """Fluid state at one tile -- {} when dry (or unloaded). Uses
    getAreaFluid (JSON-friendly table) rather than getFluidAt, whose
    documented arity is a raw multi-value/nil Lua return, not a table."""
    cells = json.loads(send(port, f"return world.getAreaFluid({gx},{gy},0)"))
    return next((c for c in cells if c["x"] == gx and c["y"] == gy), {})

def other_kind(natural_type):
    """A setFluidTile kind string guaranteed to produce an OBSERVABLE
    change from whatever a tile's natural type already is -- this
    seed's worldgen isn't guaranteed dry land (or any specific type) at
    a fixed offset, so rather than assuming a starting state, just pick
    a target different from it. setFluidTile's kind->type mapping:
    "river"->River, "ocean"->Ocean, anything else (incl. "water")->Lake."""
    return "river" if natural_type == "lake" else "water"

def main():
    ap = argparse.ArgumentParser(); ap.add_argument("--port", type=int, default=9143); ap.add_argument("--seed", type=int, default=42)
    a = ap.parse_args(); path = os.path.join("saves", SAVE); resave_path = os.path.join("saves", RESAVE); path2 = os.path.join("saves", SAVE2); p = boot(a.port, log="/tmp/save_barrier_probe.log")
    try:
        send(a.port, f'world.init("barrier",{a.seed},64,3)', expect_result=False); send(a.port, "return world.waitForInit(300)", timeout=305); send(a.port, 'world.show("barrier")', expect_result=False)
        # The post-release-mutation check below touches tiles well
        # outside the auto-loaded spawn area -- load their chunks
        # explicitly first (chunk, not tile, coordinates).
        send(a.port, "return world.loadChunksInRegion(-2,-2,3,3)")
        send(a.port, "return world.waitForChunks(120)", timeout=125)
        # This is a real World -> simulation -> World path: the edit is
        # accepted by worldQueue, synchronizes its chunk into simQueue, and
        # fluid settling publishes a WorldApplyFluids writeback.  The source
        # cell itself is changed synchronously by World, so assert a distinct
        # neighboring cell whose fluid state changes only after the sim's
        # writeback instead.
        def area_fluid():
            return {
                (cell["x"], cell["y"]): cell
                for cell in json.loads(send(a.port, "return world.getAreaFluid(0,0,3)"))
            }

        area_before = area_fluid()
        send(a.port, 'world.setFluidTile("barrier", 0, 0, "water")', expect_result=False)

        def spread():
            after = area_fluid()
            return next(
                (
                    (coord, cell)
                    for coord, cell in after.items()
                    if coord != (0, 0) and area_before.get(coord) != cell
                ),
                None,
            )

        spread_coord, spread_before = wait(spread, "simulation fluid spread writeback")
        if send(a.port, f'return engine.saveWorld("barrier","{SAVE}")').strip() != "true": raise RuntimeError("save rejected")
        def state():
            raw = send(a.port, "return engine.getSaveStatus()"); return json.loads(raw) if raw != "nil" else None
        s = wait(state, "save status", 10)
        if s["ownerCount"] != s["acknowledgedOwners"]: raise RuntimeError("save reached capture without all owners")
        wait(lambda: os.path.isfile(os.path.join(path, "world.synworld")), "save file")

        # #758: the barrier releases as soon as the snapshot is captured
        # and validated -- BEFORE encoding/disk I/O. A mutation issued the
        # instant the file appears (i.e. after the whole save, capture
        # included, has demonstrably finished) must never have been able
        # to reach the ALREADY-CAPTURED snapshot that produced that file.
        # The mutation coordinate is well outside the (0,0)-radius-3
        # area_fluid() window above, so it can't collide with the
        # spread-detection dict.
        mx, my = 20, 20
        natural = fluid_at(a.port, mx, my).get("type")
        mutate_kind = other_kind(natural)
        mutated_type = "river" if mutate_kind == "river" else "lake"
        send(a.port, f'world.setFluidTile("barrier", {mx}, {my}, "{mutate_kind}")', expect_result=False)
        wait(lambda: fluid_at(a.port, mx, my).get("type") == mutated_type,
             "post-release mutation to take effect")

        # A LATER save, after the mutation, must capture it as ITS OWN
        # distinct boundary -- neither save shares captured state with
        # the other.
        if send(a.port, f'return engine.saveWorld("barrier","{SAVE2}")').strip() != "true": raise RuntimeError("second save rejected")
        wait(lambda: os.path.isfile(os.path.join(path2, "world.synworld")), "second save file")

        # #758 review round 2 follow-up: a genuine disk-level write failure
        # (the save's own directory PATH already occupied by a plain file,
        # so createDirectoryIfMissing inside writeSaveFiles must fail) must
        # surface as a real SaveFailed outcome via engine.getSaveStatus(),
        # not crash the world thread or leave the barrier stuck open
        # forever (saveInProgress permanently True, refusing every later
        # save) -- the exact risk an uncaught IO exception in writeSaveFiles
        # would create, since it runs AFTER the barrier's capture lock has
        # already released.
        WFAIL = SAVE + "_writefail"
        wfail_path = os.path.join("saves", WFAIL)
        if os.path.isdir(wfail_path): shutil.rmtree(wfail_path)
        elif os.path.exists(wfail_path): os.remove(wfail_path)
        with open(wfail_path, "w") as f: f.write("occupying this path with a plain file")
        try:
            if send(a.port, f'return engine.saveWorld("barrier","{WFAIL}")').strip() != "true":
                raise RuntimeError("write-failure save rejected before it ever reached disk I/O")
            def failed_status():
                raw = send(a.port, "return engine.getSaveStatus()")
                st = json.loads(raw) if raw != "nil" else None
                return st if st and st.get("phase") == "SaveFailed" else None
            fs = wait(failed_status, "disk write failure to surface as SaveFailed", 15)
            if "SaveAborted" not in fs.get("outcome", ""):
                raise RuntimeError(f"expected a SaveAborted outcome, got {fs!r}")
        finally:
            os.remove(wfail_path)
        # The world thread must still be alive and the barrier must have
        # unblocked itself (saveInProgress back to False): an ordinary save
        # issued right after must still be accepted and actually complete.
        WFAIL_FOLLOWUP = SAVE + "_writefail_followup"
        followup_path = os.path.join("saves", WFAIL_FOLLOWUP)
        if send(a.port, f'return engine.saveWorld("barrier","{WFAIL_FOLLOWUP}")').strip() != "true":
            raise RuntimeError("save rejected right after a prior write failure -- barrier stuck open?")
        wait(lambda: os.path.isfile(os.path.join(followup_path, "world.synworld")), "follow-up save file")
        shutil.rmtree(followup_path, ignore_errors=True)
    finally:
        quit_engine(a.port, p)
        try: p.wait(timeout=15)
        except subprocess.TimeoutExpired: p.kill()
    p = boot(a.port, log="/tmp/save_barrier_probe_reload.log")
    try:
        if send(a.port, f'return engine.loadSave("{SAVE}")').strip() != "true": raise RuntimeError("load rejected")
        send(a.port, "return world.waitForInit(300)", timeout=305); send(a.port, 'world.show("main_world")', expect_result=False); time.sleep(1)
        if send(a.port, "return engine.isPaused()").strip() != "true": raise RuntimeError("load was not paused")
        reloaded_spread = area_fluid().get(spread_coord)
        if reloaded_spread != spread_before:
            raise RuntimeError(
                "pre-boundary World->Sim->World spread was not saved: "
                f"{spread_coord}: expected {spread_before!r}, got {reloaded_spread!r}"
            )
        paused_fluid = reloaded_spread
        time.sleep(2)
        if area_fluid().get(spread_coord) != paused_fluid:
            raise RuntimeError("loaded world spread mutated while paused")
        # The mutation issued right after the FIRST save's barrier
        # released must be ABSENT from that already-captured save --
        # the tile's type must still match its PRE-mutation (natural)
        # state, not the mutated one.
        loaded_far = fluid_at(a.port, mx, my).get("type")
        if loaded_far != natural:
            raise RuntimeError(
                "a mutation made after barrier release altered an "
                f"already-captured save: expected natural type {natural!r} "
                f"at ({mx},{my}), got {loaded_far!r}"
            )
        first_size = os.path.getsize(os.path.join(path, "world.synworld"))
        if send(a.port, f'return engine.saveWorld("main_world","{RESAVE}")').strip() != "true": raise RuntimeError("resave rejected")
        wait(lambda: os.path.isfile(os.path.join(resave_path, "world.synworld")), "resave file")
        second_size = os.path.getsize(os.path.join(resave_path, "world.synworld"))
        if second_size > first_size + 1024:
            raise RuntimeError(
                "fluid snapshot grew across an unchanged save/load/save cycle: "
                f"first={first_size}, second={second_size}"
            )
    finally:
        quit_engine(a.port, p); shutil.rmtree(path, ignore_errors=True); shutil.rmtree(resave_path, ignore_errors=True)
    p = boot(a.port, log="/tmp/save_barrier_probe_reload2.log")
    try:
        # The SECOND save -- taken AFTER the post-release mutation --
        # must capture it as its own distinct boundary.
        if send(a.port, f'return engine.loadSave("{SAVE2}")').strip() != "true": raise RuntimeError("second-save load rejected")
        send(a.port, "return world.waitForInit(300)", timeout=305); send(a.port, 'world.show("main_world")', expect_result=False); time.sleep(1)
        loaded_far2 = fluid_at(a.port, mx, my).get("type")
        if loaded_far2 != mutated_type:
            raise RuntimeError(
                "a later save did not capture a mutation made before its "
                f"own boundary: expected {mutated_type!r} at ({mx},{my}), "
                f"got {loaded_far2!r}"
            )
    finally:
        quit_engine(a.port, p); shutil.rmtree(path2, ignore_errors=True)
    print("PASS: save owners acknowledged, post-release mutation isolated, "
          "later save captured its own boundary, a disk write failure "
          "surfaced as SaveFailed without wedging the barrier, and loaded "
          "session stayed paused")

if __name__ == "__main__": main()
