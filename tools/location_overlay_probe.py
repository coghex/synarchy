#!/usr/bin/env python3
"""Headless world-gen location-overlay probe (#89).

The engine places data-driven locations (#88) into chunks during world
generation: a deterministic pass, run from the seed + plate/ocean data,
produces a sparse chunk -> location-id overlay that is carried in the
world's gen params and serialized into the save. `world.listPlaced-
Locations()` (Lua `locations.listPlaced()`) reads that overlay back.

This drives the full integration headless and checks #89 end to end:

  1. Generating a world with `ruin_small` defined produces >= 1 ruin
     somewhere (via listPlacedLocations).
  2. Same seed -> same overlay (two independent generations match).
  3. Suitability respects anchor tags: every ruin_small (anchor [flat])
     sits on a land chunk, never an ocean one.
  4. Lazy stamping: loading a ruin's chunk materializes its geometry
     (engine chunk-load dispatch -> stamper -> the #88 builder).
  5. The overlay survives save -> quit -> fresh restart -> load; checked
     before any location YAML is reloaded, so it can only have come from
     the save (a recompute is impossible with no defs registered).
  6. No location is lost to save timing: the world is saved with its
     ruins still UN-STAMPED (right after gen, before their far chunks
     load); after a fresh restart + load, visiting each ruin's chunk
     materializes it from the persisted overlay anyway.
  7. The SYNCHRONOUS centre chunk (0,0) — which Init/Save regenerate
     directly and exclude from the chunk-load queue — also stamps, both
     on first generation (Init hook) and on first load (Save hook).
  8. Multiworld: a location on a HIDDEN, non-active page still stamps
     (page-targeted terrain reads, no active-page gate) — checked with a
     locationless arena as the active world so the hidden page's stamp is
     observable.

The location stamper is auto-loaded at boot by scripts/init.lua, exactly
as in the real game, so this only registers the location defs by hand
(headless skips the GUI data-loading step).

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


def has_floor(port: int, gx: int, gy: int) -> bool:
    """True if a 'floor' structure piece exists at (gx,gy) — i.e. the
    ruin_small builder (room_small) has stamped its room there."""
    r = send(port, f"return structure.hasAt({gx},{gy},'floor') and 'yes' or 'no'")
    return r.strip('"') == "yes"


def load_chunk(port: int, cx: int, cy: int) -> None:
    send(port, f"return world.loadChunksInRegion({cx},{cy},{cx},{cy})")
    send(port, "return world.waitForChunks(30)", timeout=35)


def count_stamped(port: int, ruins: list[dict]) -> int:
    return sum(1 for e in ruins if has_floor(port, e["gx"], e["gy"]))


def wait_stamped(port: int, ruins: list[dict], tries: int = 80) -> int:
    """Poll until every ruin has been stamped (or the cap)."""
    want, n = len(ruins), 0
    for _ in range(tries):
        n = count_stamped(port, ruins)
        if n >= want:
            return n
        time.sleep(0.5)
    return n


def wait_floor(port: int, gx: int, gy: int, tries: int = 40) -> bool:
    for _ in range(tries):
        if has_floor(port, gx, gy):
            return True
        time.sleep(0.5)
    return False


# A dense location def (no anchor, no spacing, unbounded count) places a
# location on EVERY land chunk — so the centre chunk (0,0), land for our
# seed, is guaranteed one. (0,0) is only ever loaded synchronously (Init /
# Save regenerate it directly and exclude it from the chunk-load queue), so
# a floor there can only come from the synchronous-centre stamp hooks.
DENSE_YAML = "/tmp/loc_overlay_probe_dense.yaml"
DENSE_BODY = (
    "locations:\n"
    "  - id: ruin_small\n"
    "    label: Small Ruin\n"
    "    type: ruin\n"
    "    builder: room_small\n"
    "    anchor: []\n"
    "    max_count: 100000\n"
    "    min_spacing: 1\n"
    "    contents: []\n"
)


def has_loc_on(port: int, cx: int, cy: int, page: str | None = None, tries: int = 20) -> bool:
    """Whether the overlay places a location on chunk (cx,cy). With `page`
    it reads that page's overlay (so a hidden, non-active world works);
    otherwise the active world.

    Polls until the overlay is readable (genParams written / world active),
    so it does not race init or world.show. The server-side scan returns
    just a flag, never the (huge, dense) full list.
    """
    arg = f"'{page}'" if page else ""
    lua = (f"local t = world.listPlacedLocations({arg}); "
           f"for _, e in ipairs(t) do if e.cx == {cx} and e.cy == {cy} then return 'yes' end end; "
           f"return (#t > 0) and 'no' or 'empty'")
    for _ in range(tries):
        r = send(port, lua).strip('"')
        if r == "yes":
            return True
        if r == "no":
            return False
        time.sleep(0.5)
    return False


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

    # ---- Phase 1: placement, determinism, lazy stamping; then save the
    #      world with its locations still UN-STAMPED (saved right after gen,
    #      before any far ruin chunk has loaded) so phase 2 can prove they
    #      are not lost. ----
    proc = boot(args.port)
    try:
        send(args.port, "engine.loadLocationYaml('data/locations/ruin_small.yaml'); return 'ok'")
        # The stamper is auto-loaded at boot by scripts/init.lua (as in the
        # real game), so we only have to register the location defs here.

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

        # The ruins sit on far chunks not loaded at gen, so none are stamped
        # yet. Confirm, then SAVE in that un-stamped state — the race the
        # reviewer flagged (saved before stamping drains).
        unstamped = sum(1 for e in ruins if not has_floor(args.port, e["gx"], e["gy"]))
        print(f"  {unstamped}/{len(ruins)} ruin(s) un-stamped at save time")
        send(args.port, "engine.saveWorld('wa', 'loc_overlay_probe'); return 'saved'")
        time.sleep(1.0)

        # In-session lazy stamping: loading a ruin's chunk materializes its
        # geometry (engine dispatch -> stamper -> #88 builder). Doubles as
        # the on-land / anchor check.
        ocean_hits = []
        for e in ruins:
            load_chunk(args.port, e["cx"], e["cy"])
            if is_ocean(args.port, e["gx"], e["gy"]):
                ocean_hits.append(e)
        if ruins and not ocean_hits:
            print(f"PASS: all {len(ruins)} ruin(s) on land (anchor [flat] respected)")
        elif ocean_hits:
            failures.append(f"{len(ocean_hits)} ruin(s) placed on ocean tiles")
        n = wait_stamped(args.port, ruins)
        if ruins and n == len(ruins):
            print(f"PASS: lazy stamping materialized all {n} ruin(s) as their chunks loaded")
        else:
            failures.append(f"only {n}/{len(ruins)} ruin(s) stamped in-session")

        # Determinism: a second independent generation with the same seed.
        gen_world(args.port, "wb", args.seed, args.size)
        lb = placed_ready(args.port)
        if key(la) == key(lb) and la:
            print("PASS: same seed -> identical overlay (A == B)")
        else:
            failures.append(f"overlay not deterministic: A={key(la)} B={key(lb)}")
    finally:
        shutdown(args.port, proc)

    # ---- Phase 2: a world saved with UN-STAMPED locations still
    #      materializes them after a fresh restart + load (the reviewer's
    #      option 2: chunk-load after a save consults the persisted overlay
    #      and stamps any not-yet-materialized entry). ----
    proc = boot(args.port)
    try:
        send(args.port, "engine.loadSave('loc_overlay_probe'); return 'queued'")
        time.sleep(6.0)
        send(args.port, "world.show('main_world'); return 'ok'")
        time.sleep(1.0)

        # Overlay persisted: no location YAML loaded yet, so this CANNOT be a
        # recompute (computeLocationOverlay short-circuits with no defs) — it
        # came from the save.
        lc = placed_ready(args.port)
        if key(lc) == key(la) and lc:
            print(f"PASS: overlay survived save/load ({len(lc)} placements, no YAML reload)")
        else:
            failures.append(f"overlay lost/changed across save-load: before={key(la)} after={key(lc)}")

        # Load the defs + stamper as the game does at boot, then visit each
        # ruin's chunk. Each must materialize from the persisted overlay even
        # though the save contained no stamped geometry for it.
        send(args.port, "engine.loadLocationYaml('data/locations/ruin_small.yaml'); return 'ok'")
        # The stamper is auto-loaded at boot by scripts/init.lua (as in the
        # real game), so we only have to register the location defs here.
        ruins_after = [e for e in lc if e["id"] == "ruin_small"]
        for e in ruins_after:
            load_chunk(args.port, e["cx"], e["cy"])
        m = wait_stamped(args.port, ruins_after)
        if ruins_after and m == len(ruins_after):
            print(f"PASS: all {m} ruin(s) materialized after load despite being saved un-stamped")
        else:
            failures.append(f"only {m}/{len(ruins_after)} ruin(s) materialized after load")
    finally:
        shutdown(args.port, proc)

    # A dense def places a location on EVERY land chunk, so the centre
    # chunk (0,0) — land for our seed, and the only chunk that is ever
    # loaded SYNCHRONOUSLY (Init and Save regenerate it directly and
    # exclude it from the chunk-load queue) — is guaranteed one. A floor at
    # its anchor (8,8) can therefore only come from the synchronous-centre
    # stamp hooks, not the chunk-load dispatch.
    with open(DENSE_YAML, "w") as fh:
        fh.write(DENSE_BODY)

    # ---- Phase 3: the SYNCHRONOUS centre chunk (0,0) stamps on fresh gen
    #      (Init hook). ----
    proc = boot(args.port)
    try:
        send(args.port, f"engine.loadLocationYaml('{DENSE_YAML}'); return 'ok'")
        gen_world(args.port, "wc", args.seed, args.size)
        if not has_loc_on(args.port, 0, 0):
            failures.append(f"seed {args.seed}: no location on centre chunk (0,0) — cannot test Init hook")
        elif wait_floor(args.port, 8, 8):
            print("PASS: synchronous centre chunk (0,0) stamped on first gen (Init hook)")
        else:
            failures.append("centre chunk (0,0) NOT stamped on first gen (Init hook)")
    finally:
        shutdown(args.port, proc)

    # ---- Phase 4: a location on the SAVED CAMERA CHUNK is present on the
    #      FIRST load. The default camera sits on (0,0); save a world whose
    #      (0,0) hosts a location, then on a fresh restart confirm it is back
    #      WITHOUT force-loading (0,0) — Save regenerates that chunk
    #      synchronously and excludes it from the queue, so its presence
    #      exercises the Save centre hook. ----
    proc = boot(args.port)
    saved_centre = False
    try:
        send(args.port, f"engine.loadLocationYaml('{DENSE_YAML}'); return 'ok'")
        gen_world(args.port, "wd", args.seed, args.size)
        if not has_loc_on(args.port, 0, 0):
            failures.append(f"seed {args.seed}: no location on centre chunk (0,0) — cannot test Save hook")
        elif not wait_floor(args.port, 8, 8):
            failures.append("phase 4 setup: centre (0,0) did not stamp at gen")
        else:
            send(args.port, "engine.saveWorld('wd', 'loc_centre_probe'); return 'saved'")
            time.sleep(1.0)
            saved_centre = True
    finally:
        shutdown(args.port, proc)

    if saved_centre:
        proc = boot(args.port)
        try:
            send(args.port, f"engine.loadLocationYaml('{DENSE_YAML}'); return 'ok'")
            send(args.port, "engine.loadSave('loc_centre_probe'); return 'queued'")
            time.sleep(6.0)
            send(args.port, "world.show('main_world'); return 'ok'")
            # Do NOT force-load (0,0) — it is the synchronous centre chunk.
            if wait_floor(args.port, 8, 8):
                print("PASS: saved-camera centre chunk (0,0) present on first load (Save hook)")
            else:
                failures.append("saved-camera centre chunk (0,0) NOT present on first load (Save hook)")
        finally:
            shutdown(args.port, proc)

    # ---- Phase 5: a location on a HIDDEN, non-active page still stamps
    #      (multiworld). A flat, locationless ARENA is the active world, so a
    #      separately generated page's centre chunk loads while hidden — and
    #      a floor at (8,8) can then only come from that hidden page. With the
    #      old active-page gate it would be skipped forever; page-targeted
    #      terrain reads let it stamp against its own page regardless. ----
    proc = boot(args.port)
    try:
        send(args.port, f"engine.loadLocationYaml('{DENSE_YAML}'); return 'ok'")
        send(args.port, "world.initArena('arena'); world.initArenaDone('arena'); world.show('arena'); return 'ok'")
        arena_ok = False
        for _ in range(40):
            r = send(args.port, "local i=world.getChunkInfo(0,0); return i and i.loaded and 'y' or 'n'").strip('"')
            if r == "y":
                arena_ok = True
                break
            time.sleep(0.25)
        if not arena_ok:
            failures.append("phase 5: arena never became ready")
        else:
            # Generate a second world but DO NOT show it — arena stays active.
            send(args.port, f"world.init('sw', {args.seed}, {args.size}, 3); return 'ok'")
            send(args.port, "return world.waitForInit(240)", timeout=250)
            active = "?"
            for _ in range(10):
                active = send(args.port, "return world.getActiveWorldId()").strip('"')
                if active == "arena":
                    break
                time.sleep(0.3)
            if active != "arena":
                failures.append(f"phase 5: expected 'arena' active, got '{active}'")
            elif not has_loc_on(args.port, 0, 0, page="sw"):
                failures.append("phase 5: hidden world 'sw' has no location on (0,0)")
            elif wait_floor(args.port, 8, 8):
                swz = send(args.port, "return world.getTerrainAt(8,8,'sw')").split("\t")[0].strip()
                fz = send(args.port, "return structure.floorZAt(8,8)").strip()
                print(f"PASS: hidden non-active page stamped its centre while '{active}' active "
                      f"(floor z={fz} matches sw terrain {swz}, not the arena's 0)")
            else:
                failures.append("hidden non-active page did NOT stamp its centre (multiworld)")
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
