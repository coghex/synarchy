#!/usr/bin/env python3
"""Manual real-thread smoke probe for the #757 save boundary."""
from __future__ import annotations
import argparse, json, os, shutil, subprocess, sys, time, uuid
from probelib import boot, quit_engine, send

SAVE = "probe_barrier_" + uuid.uuid4().hex[:12]
RESAVE = SAVE + "_resave"

def wait(predicate, what, timeout=30):
    end = time.time() + timeout
    while time.time() < end:
        value = predicate()
        if value: return value
        time.sleep(.2)
    raise RuntimeError("timed out waiting for " + what)

def main():
    ap = argparse.ArgumentParser(); ap.add_argument("--port", type=int, default=9143); ap.add_argument("--seed", type=int, default=42)
    a = ap.parse_args(); path = os.path.join("saves", SAVE); resave_path = os.path.join("saves", RESAVE); p = boot(a.port, log="/tmp/save_barrier_probe.log")
    try:
        send(a.port, f'world.init("barrier",{a.seed},64,3)', expect_result=False); send(a.port, "return world.waitForInit(300)", timeout=305); send(a.port, 'world.show("barrier")', expect_result=False)
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
    print("PASS: save owners acknowledged and loaded session stayed paused")

if __name__ == "__main__": main()
