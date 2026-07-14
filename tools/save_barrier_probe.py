#!/usr/bin/env python3
"""Manual real-thread smoke probe for the #757 save boundary."""
from __future__ import annotations
import argparse, json, os, shutil, subprocess, sys, time, uuid
from probelib import boot, quit_engine, send

SAVE = "probe_barrier_" + uuid.uuid4().hex[:12]

def wait(predicate, what, timeout=30):
    end = time.time() + timeout
    while time.time() < end:
        value = predicate()
        if value: return value
        time.sleep(.2)
    raise RuntimeError("timed out waiting for " + what)

def main():
    ap = argparse.ArgumentParser(); ap.add_argument("--port", type=int, default=9143); ap.add_argument("--seed", type=int, default=42)
    a = ap.parse_args(); path = os.path.join("saves", SAVE); p = boot(a.port, log="/tmp/save_barrier_probe.log")
    try:
        send(a.port, f'world.init("barrier",{a.seed},64,3)', expect_result=False); send(a.port, "return world.waitForInit(300)", timeout=305); send(a.port, 'world.show("barrier")', expect_result=False)
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
    finally:
        quit_engine(a.port, p); shutil.rmtree(path, ignore_errors=True)
    print("PASS: save owners acknowledged and loaded session stayed paused")

if __name__ == "__main__": main()
