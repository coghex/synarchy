#!/usr/bin/env python3
"""Headless world-gen location-overlay probe (#89).

The engine places data-driven locations (#88) into chunks during world
generation: a deterministic pass, run from the seed + plate/ocean data,
produces a sparse chunk -> location-id overlay that is carried in the
world's gen params and serialized into the save. `world.listPlaced-
Locations()` (Lua `locations.listPlaced()`) reads that overlay back.

This drives the full integration headless and checks the four
acceptance criteria of #89:

  1. Generating a world with `ruin_small` defined produces >= 1 ruin
     somewhere (via listPlacedLocations).
  2. Same seed -> same overlay (two independent generations match).
  3. Overlay survives save -> quit -> fresh restart -> load, WITHOUT the
     reload engine re-loading the location YAML (so the placements can
     only have come from the save, never a recompute).
  4. Suitability respects anchor tags: every ruin_small (anchor [flat])
     sits on a land chunk, never an ocean one.

Usage:
  python3 tools/location_overlay_probe.py
  python3 tools/location_overlay_probe.py --seed 7 --size 64 --port 9189

Exit code 0 = all checks passed.
"""
from __future__ import annotations

import argparse
import json
import socket
import subprocess
import sys
import time

LOG = "/tmp/location_overlay_engine.log"


def send(port: int, lua: str, timeout: float = 10.0) -> str:
    """Run one line of Lua in the debug console, return the result text."""
    with socket.create_connection(("localhost", port), timeout=timeout) as s:
        s.sendall((lua + "\n").encode())
        chunks: list[bytes] = []
        s.settimeout(0.3)
        try:
            while True:
                b = s.recv(4096)
                if not b:
                    break
                chunks.append(b)
        except socket.timeout:
            pass
    out = b"".join(chunks).decode(errors="replace")
    results = [ln[2:].strip() for ln in out.splitlines() if ln.startswith("> ")]
    results = [r for r in results if r]
    return results[-1] if results else out.strip()


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


def shutdown(port: int, proc: subprocess.Popen) -> None:
    try:
        send(port, "engine.quit(); return 'bye'", timeout=3.0)
    except OSError:
        pass
    try:
        proc.wait(timeout=15)
    except subprocess.TimeoutExpired:
        proc.kill()


def placed(port: int) -> list[dict]:
    """The active world's placed-location list, parsed from JSON."""
    raw = send(port, "return world.listPlacedLocations()").strip()
    if not raw or raw in ("nil", "{}", "[]"):
        return []
    try:
        data = json.loads(raw)
    except json.JSONDecodeError:
        return []
    return data if isinstance(data, list) else []


def placed_ready(port: int, tries: int = 30) -> list[dict]:
    """Read the overlay once the shown world is actually active.

    world.show queues a command processed by the world thread; the
    active-world query can race ahead of it and read the previous (or no)
    active page. Poll until a non-empty result settles. The test inputs
    (ruin_small, max_count 6) always place several, so non-empty is the
    ready signal; fall through after the cap so a genuinely empty world
    still returns rather than hanging.
    """
    last: list[dict] = []
    for _ in range(tries):
        last = placed(port)
        if last:
            return last
        time.sleep(0.5)
    return last


def key(entries: list[dict]) -> list[tuple]:
    """Stable, comparable signature of a placement set."""
    return sorted((e["cx"], e["cy"], e["id"]) for e in entries)


def is_ocean(port: int, gx: int, gy: int) -> bool:
    r = send(port, f"local f=world.getFluidAt({gx},{gy}); return f or 'dry'")
    return r.strip('"') == "ocean"


def gen_world(port: int, page: str, seed: int, size: int) -> None:
    send(port, f"world.init('{page}', {seed}, {size}, 3); return 'ok'")
    send(port, "return world.waitForInit(240)", timeout=250)
    send(port, f"world.show('{page}'); return 'ok'")
    # world.show queues a command processed by the world thread; load the
    # centre region and wait so the page is the active one before we read
    # the overlay (the overlay is in gen params, not the chunks, but this
    # is a reliable sync point that the show has taken effect).
    send(port, "return world.loadChunksInRegion(-1,-1,1,1)")
    send(port, "return world.waitForChunks(60)", timeout=65)


def main() -> int:
    ap = argparse.ArgumentParser(description=__doc__)
    ap.add_argument("--seed", type=int, default=42)
    ap.add_argument("--size", type=int, default=64)
    ap.add_argument("--port", type=int, default=9189)
    args = ap.parse_args()

    failures: list[str] = []

    # ---- Phase 1: placement + determinism + anchor (one engine) ----
    proc = boot(args.port)
    try:
        send(args.port, "engine.loadLocationYaml('data/locations/ruin_small.yaml'); return 'ok'")

        gen_world(args.port, "wa", args.seed, args.size)
        la = placed_ready(args.port)
        print(f"world A (seed {args.seed}): {len(la)} placed location(s)")
        for e in la:
            print(f"  {e['id']:14s} chunk ({e['cx']},{e['cy']})  tile ({e['gx']},{e['gy']})")

        ruins = [e for e in la if e["id"] == "ruin_small"]
        if ruins:
            print(f"PASS: {len(ruins)} ruin_small placed (>= 1)")
        else:
            failures.append("no ruin_small placed in world A")

        # Determinism: a second independent generation with the same seed.
        gen_world(args.port, "wb", args.seed, args.size)
        lb = placed_ready(args.port)
        if key(la) == key(lb) and la:
            print("PASS: same seed -> identical overlay (A == B)")
        else:
            failures.append(f"overlay not deterministic: A={key(la)} B={key(lb)}")

        # Anchor: flat-anchored ruins must never sit on an ocean tile.
        # Load each ruin's own chunk on demand (cheaper than the whole
        # world) and read the fluid at its anchor tile.
        send(args.port, "world.show('wa'); return 'ok'")
        ocean_hits = []
        for e in ruins:
            cx, cy = e["cx"], e["cy"]
            send(args.port, f"return world.loadChunksInRegion({cx},{cy},{cx},{cy})")
            send(args.port, "return world.waitForChunks(30)", timeout=35)
            if is_ocean(args.port, e["gx"], e["gy"]):
                ocean_hits.append(e)
        if ruins and not ocean_hits:
            print(f"PASS: all {len(ruins)} ruin(s) on land (anchor [flat] respected)")
        elif ocean_hits:
            failures.append(f"{len(ocean_hits)} ruin(s) placed on ocean tiles")

        # Save world A for the reload phase.
        send(args.port, "engine.saveWorld('wa', 'loc_overlay_probe'); return 'saved'")
        time.sleep(1.0)
    finally:
        shutdown(args.port, proc)

    # ---- Phase 2: save survives a fresh restart + load ----
    # Deliberately do NOT load the location YAML on this engine: if the
    # placements still appear, they came from the save, not a recompute.
    proc = boot(args.port)
    try:
        send(args.port, "engine.loadSave('loc_overlay_probe'); return 'queued'")
        # The overlay rides gen params, restored immediately on load — no
        # need to wait for chunks. A short settle covers the load queue.
        time.sleep(6.0)
        send(args.port, "world.show('main_world'); return 'ok'")
        time.sleep(1.0)
        lc = placed_ready(args.port)
        if key(lc) == key(la) and lc:
            print(f"PASS: overlay survived save/load ({len(lc)} placements, no YAML reload)")
        else:
            failures.append(f"overlay lost/changed across save-load: before={key(la)} after={key(lc)}")
    finally:
        shutdown(args.port, proc)

    print("-" * 56)
    if failures:
        for f in failures:
            print(f"FAIL: {f}", file=sys.stderr)
        return 1
    print("ALL CHECKS PASSED")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
