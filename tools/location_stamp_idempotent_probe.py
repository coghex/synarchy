#!/usr/bin/env python3
"""Headless location geometry-stamp idempotency probe (#424).

The lazy location stamper (scripts/location_stamper.lua) used to infer
"already materialized" from `structure.hasAt(gx, gy, "floor", pageId)` at
the location's anchor tile. That check is fooled by a player who later
clears just the anchor floor: the location has still been stamped, but the
guard sees "no floor" and re-runs the builder on the NEXT chunk load,
restoring/clobbering whatever the player edited. The fix (#424) replaces
the inference with a dedicated persisted marker
(`world.hasStampedLocation` / `world.markLocationStamped`,
`WorldGenParams.wgpLocationStamped`) that is set once, on first stamp, and
is never revisited by structure edits.

This checks, end to end:

  1. A location's chunk loading for the first time stamps its geometry
     (the anchor floor + full room appear).
  2. Clearing ONLY the anchor floor tile, then saving -> quitting ->
     restarting -> loading -> reloading the same chunk (a genuine "load"
     in a fresh process, not a no-op) does NOT re-run the builder: the
     anchor floor stays absent (the player's edit persists) and the rest
     of the room's geometry is unchanged.
  3. A location whose chunk was NEVER loaded before the save (so its
     geometry-stamp flag was never set) still stamps correctly the first
     time its chunk loads after a save -> restart -> load.

Usage:
  python3 tools/location_stamp_idempotent_probe.py
  python3 tools/location_stamp_idempotent_probe.py --seed 42 --size 64 --port 9191

Exit code 0 = all checks passed.
"""
from __future__ import annotations

import argparse
import json
import os
import shutil
import socket
import subprocess
import sys
import tempfile
import time
from pathlib import Path
from probelib import (FixtureNotRegistered, capture_request_id, quit_engine,
                      boot, load_fixture_yaml, send, wait_load_published,
                      wait_save_complete)

LOG = "/tmp/location_stamp_idempotent_engine.log"
LOCATION_YAML = "/tmp/location_stamp_idempotent_probe_loc.yaml"
LOCATION_ID = "stamp_probe_room"
REPO = Path(__file__).resolve().parent.parent
#: Prefix of this invocation's throwaway resource root. The random
#: suffix mkdtemp appends to it is also the per-invocation save-slot
#: token, so the root and the slots it holds share one identity.
ROOT_PREFIX = "location_stamp_idempotent_probe_"


# --------------------------------------------------------------------------
# Isolated resource root + request-specific save/load completion (#1620)
# --------------------------------------------------------------------------
def make_isolated_root(base: str) -> str:
    """A throwaway resource root for THIS invocation: the repository's
    real scripts/assets/data/config symlinked in (read-only content, safe
    to share) plus its OWN empty saves/ directory.

    Every boot below is handed this root, so the fixtures this probe
    saves and loads are never written into, and can never be satisfied
    by, a slot reachable from a normal `cabal run` (#1620 requirement 5;
    the tools/chop_probe.py pattern). The families are SYMLINKS, and
    shutil.rmtree unlinks a symlink rather than descending it, so
    teardown can never reach the repository's own directories.
    """
    root = os.path.join(base, "root")
    os.makedirs(root, exist_ok=True)
    for family in ("scripts", "assets", "data", "config"):
        target = os.path.join(root, family)
        if not os.path.exists(target):
            os.symlink(os.path.join(REPO, family), target)
    os.makedirs(os.path.join(root, "saves"), exist_ok=True)
    return root


def remove_isolated_root(base: str) -> str | None:
    """Remove this invocation's root, REPORTING a failure instead of
    swallowing it — the "no save artifact the run created remains on
    disk" guarantee (#1620 requirement 6) is only real if a removal that
    did not happen is visible. Returns None on success, else the message
    to fail with.
    """
    try:
        shutil.rmtree(base)
    except OSError as exc:
        return (f"could not remove this run's temporary resource root "
                f"{base}: {exc}")
    if os.path.exists(base):
        return (f"this run's temporary resource root {base} still exists "
                f"after removal")
    return None


class _PhaseAborted(Exception):
    """A phase's save or load never reached its own terminal successful
    status, so the phase must STOP rather than assert against whatever
    session happens to be live — the reader is exactly what must not
    start (#1620 requirements 2-4). The failure is already recorded; the
    surrounding `finally` still shuts the engine down in order."""


def boot_isolated(port: int, root: str):
    return boot(port, log=LOG, args=["--resource-root", root])


def save_and_wait(port: int, page: str, slot: str,
                  failures: list[str]) -> bool:
    """engine.saveWorld, then tie completion to THIS request's own id.

    saveWorld only ACCEPTS synchronously — it returns false on a
    validation failure (the detailed reason goes to the engine log) and
    true once the command is queued, while the encode and disk write run
    afterwards. So a sleep proves nothing, and a fixed slot name means a
    PRIOR generation of the same slot could satisfy the reader
    (#1620). Every reader here starts only after this returns True.
    """
    accepted = send(port, f"return engine.saveWorld('{page}', '{slot}')").strip()
    if accepted != "true":
        failures.append(
            f"engine.saveWorld(page '{page}', slot '{slot}') was not accepted "
            f"(returned {accepted!r}); the validation reason is logged in {LOG}")
        return False
    request_id = capture_request_id(port, "return engine.getSaveStatus()")
    if request_id is None:
        failures.append(
            f"engine.getSaveStatus() never reported a request id for "
            f"saveWorld(page '{page}', slot '{slot}')")
        return False
    ok, status = wait_save_complete(port, request_id)
    if not ok:
        failures.append(
            f"save of page '{page}' to slot '{slot}' (request {request_id}) "
            f"did not reach SaveCaptureComplete: {status}")
        return False
    print(f"  saved '{slot}' (request {request_id}, {status.get('phase')})")
    return True


def load_and_wait(port: int, slot: str, failures: list[str],
                  seconds: float = 60.0) -> bool:
    """engine.loadSave, then wait for THAT request id to publish.

    Issue #763: loadSave only ACCEPTS synchronously, and the saved page
    does not exist live until the transaction publishes. Passing the
    captured id to wait_load_published is what stops a terminal status
    left behind by an earlier transaction from satisfying this wait.
    """
    accepted = send(port, f"return engine.loadSave('{slot}')").strip()
    if accepted != "true":
        failures.append(
            f"engine.loadSave('{slot}') was not accepted (returned "
            f"{accepted!r}); the reason is logged in {LOG}")
        return False
    request_id = capture_request_id(port, "return engine.getLoadStatus()")
    if request_id is None:
        failures.append(
            f"engine.getLoadStatus() never reported a request id for "
            f"loadSave('{slot}')")
        return False
    published, status = wait_load_published(port, seconds,
                                            request_id=request_id)
    if not published:
        failures.append(f"load of '{slot}' (request {request_id}) did not "
                        f"publish: {status}")
        return False
    return True


def load_yaml_dir(port: int, directory: str, loader: str) -> None:
    lua = (f"local fs = engine.listFiles('{directory}', '.yaml') or {{}}; "
           f"for _, f in ipairs(fs) do {loader}('{directory}/' .. f) end; "
           f"return #fs")
    send(port, lua, timeout=20.0)


def load_defs(port: int) -> None:
    load_yaml_dir(port, "data/items", "engine.loadItemYaml")
    load_yaml_dir(port, "data/units", "engine.loadUnitYaml")
    load_yaml_dir(port, "data/buildings", "engine.loadBuildingYaml")
    load_fixture_yaml(port, "engine.loadLocationYaml", LOCATION_YAML)


def write_location_yaml() -> None:
    with open(LOCATION_YAML, "w") as fh:
        fh.write(
            "locations:\n"
            f"  - id: {LOCATION_ID}\n"
            "    label: Stamp Probe Room\n"
            "    type: test\n"
            "    builder: room_small\n"
            "    anchor: []\n"
            "    max_count: 20\n"
            "    min_spacing: 3\n"
            "    bounds: { min_x: -2, min_y: -2, max_x: 2, max_y: 2 }\n"
            "    naming: { heads: [KEEP], modifiers: [ASH] }\n"
            "    contents: []\n"
        )


def gen_world(port: int, page: str, seed: int, size: int) -> None:
    send(port, f"world.init('{page}', {seed}, {size}, 3); return 'ok'")
    send(port, "return world.waitForInit(240)", timeout=250)
    send(port, f"world.show('{page}'); return 'ok'")
    send(port, "return world.loadChunksInRegion(-1,-1,1,1)")
    send(port, "return world.waitForChunks(60)", timeout=65)


def placed(port: int, page: str | None = None) -> list[dict]:
    arg = f"'{page}'" if page else ""
    raw = send(port, f"return world.listPlacedLocations({arg})").strip()
    if not raw or raw in ("nil", "null", "{}", "[]"):
        return []
    try:
        data = json.loads(raw)
    except json.JSONDecodeError:
        return []
    return data if isinstance(data, list) else []


def placed_ready(port: int, tries: int = 30) -> list[dict]:
    last: list[dict] = []
    for _ in range(tries):
        last = [e for e in placed(port) if e.get("id") == LOCATION_ID]
        if last:
            return last
        time.sleep(0.5)
    return last


def load_chunk(port: int, cx: int, cy: int) -> None:
    send(port, f"return world.loadChunksInRegion({cx},{cy},{cx},{cy})")
    send(port, "return world.waitForChunks(30)", timeout=35)


def has_floor(port: int, gx: int, gy: int, page: str | None = None) -> bool:
    arg = f",'{page}'" if page else ""
    r = send(port, f"return structure.hasAt({gx},{gy},'floor'{arg}) and 'yes' or 'no'")
    return r.strip('"') == "yes"


def wait_floor(port: int, gx: int, gy: int, page: str | None = None, tries: int = 40) -> bool:
    for _ in range(tries):
        if has_floor(port, gx, gy, page):
            return True
        time.sleep(0.5)
    return False


def room_geometry(port: int, gx: int, gy: int, page: str | None = None) -> tuple[int, int, int]:
    """(floors, walls, posts) of the 5x5 room_small anchored at (gx, gy).
    25 floor tiles, the 20 perimeter wall segments, 4 corner posts —
    see scripts/locations.lua builders.room_small."""
    arg = f",'{page}'" if page else ""
    lua = (
        f"local f,w,p=0,0,0; "
        f"for x={gx-2},{gx+2} do for y={gy-2},{gy+2} do "
        f"if structure.hasAt(x,y,'floor'{arg}) then f=f+1 end end end; "
        f"for y={gy-2},{gy+2} do "
        f"if structure.hasAt({gx-2},y,'wall_nw'{arg}) then w=w+1 end "
        f"if structure.hasAt({gx+2},y,'wall_se'{arg}) then w=w+1 end end; "
        f"for x={gx-2},{gx+2} do "
        f"if structure.hasAt(x,{gy-2},'wall_ne'{arg}) then w=w+1 end "
        f"if structure.hasAt(x,{gy+2},'wall_sw'{arg}) then w=w+1 end end; "
        f"for _,c in ipairs({{{{{gx-2},{gy-2},'post_n'}},{{{gx+2},{gy-2},'post_e'}},"
        f"{{{gx+2},{gy+2},'post_s'}},{{{gx-2},{gy+2},'post_w'}}}}) do "
        f"if structure.hasAt(c[1],c[2],c[3]{arg}) then p=p+1 end end; "
        f"return f .. ',' .. w .. ',' .. p")
    r = send(port, lua).strip('"')
    try:
        f, w, p = (int(v) for v in r.split(","))
        return f, w, p
    except ValueError:
        return -1, -1, -1


def has_stamped(port: int, gx: int, gy: int, page: str | None = None) -> bool:
    arg = f",'{page}'" if page else ""
    r = send(port, f"return world.hasStampedLocation({gx},{gy}{arg}) and 'yes' or 'no'")
    return r.strip('"') == "yes"


def main() -> int:
    ap = argparse.ArgumentParser(description=__doc__)
    ap.add_argument("--seed", type=int, default=42)
    ap.add_argument("--size", type=int, default=64)
    ap.add_argument("--port", type=int, default=9191)
    args = ap.parse_args()

    # One throwaway resource root per invocation, and slot names carrying
    # that root's own random token, so two concurrent runs cannot collide
    # and no developer-visible save slot is created, mutated or rotated
    # (#1620 requirement 5).
    base = tempfile.mkdtemp(prefix=ROOT_PREFIX)
    # The WHOLE random suffix, not the text after the last underscore:
    # mkdtemp's alphabet includes '_', so splitting on it can throw most
    # of the entropy away (and can leave nothing at all).
    token = os.path.basename(base)[len(ROOT_PREFIX):]
    try:
        rc = run(args, make_isolated_root(base), token)
    finally:
        # Reported, never swallowed, and reported even when `run` is
        # leaving by an exception (boot() exits the process on a dead
        # engine) — a root that survived is exactly the artifact #1620
        # requirement 6 forbids.
        leftover = remove_isolated_root(base)
        if leftover:
            print(f"FAIL: {leftover}", file=sys.stderr)
    return 1 if leftover else rc


def run(args, root: str, token: str) -> int:
    slot_main = f"stamp_idempotent_probe_{token}"
    slot_fresh = f"stamp_idempotent_probe_fresh_{token}"

    failures: list[str] = []
    write_location_yaml()

    # ---- Phase 1: first load stamps the room; clear only the anchor
    #      floor, save, and quit. ----
    gx = gy = cx = cy = None
    geom_before = (-1, -1, -1)
    saved_main = False
    saved_fresh = False
    proc = boot_isolated(args.port, root)
    try:
        load_defs(args.port)
        gen_world(args.port, "sa", args.seed, args.size)
        rooms = placed_ready(args.port)
        print(f"world (seed {args.seed}): {len(rooms)} {LOCATION_ID} placed")
        if not rooms:
            failures.append(f"no {LOCATION_ID} placed — cannot test idempotency")
        else:
            e = rooms[0]
            cx, cy, gx, gy = e["cx"], e["cy"], e["gx"], e["gy"]
            load_chunk(args.port, cx, cy)
            if not wait_floor(args.port, gx, gy):
                failures.append(f"room at ({gx},{gy}) never stamped on first load")
            else:
                print(f"PASS: room at ({gx},{gy}) stamped on first chunk load")
                geom_before = room_geometry(args.port, gx, gy)
                if geom_before != (25, 20, 4):
                    failures.append(
                        f"unexpected initial geometry {geom_before} (want (25, 20, 4))")
                if not has_stamped(args.port, gx, gy):
                    failures.append(
                        f"world.hasStampedLocation false right after a successful stamp")
                else:
                    print("PASS: world.hasStampedLocation is true after stamping")

                # Player clears ONLY the anchor floor tile.
                send(args.port, f"return structure.clear({gx},{gy},'floor')")
                if has_floor(args.port, gx, gy):
                    failures.append("structure.clear did not remove the anchor floor")
                else:
                    print(f"PASS: anchor floor cleared at ({gx},{gy})")

                # The fresh process below reads this fixture, so the
                # save must be COMPLETE — not merely accepted — before
                # phase 2 boots (#1620).
                saved_main = save_and_wait(args.port, "sa", slot_main,
                                           failures)
    finally:
        quit_engine(args.port, proc)

    # ---- Phase 2: restart -> load -> reload the same chunk. A real
    #      chunk LOAD (fresh process, nothing cached) must NOT re-run the
    #      builder: the anchor floor must stay absent and the rest of the
    #      room's geometry must be unchanged. ----
    if gx is not None and saved_main and not failures:
        proc = boot_isolated(args.port, root)
        try:
            load_defs(args.port)
            # Issue #763: the saved page ("sa", its own id verbatim -- no
            # more main_world remap) doesn't exist live until published.
            if not load_and_wait(args.port, slot_main, failures):
                raise _PhaseAborted
            send(args.port, "world.show('sa'); return 'ok'")
            time.sleep(1.0)
            load_chunk(args.port, cx, cy)
            time.sleep(2.0)

            if has_floor(args.port, gx, gy):
                failures.append(
                    "BUG: anchor floor reappeared after reload — the builder "
                    "re-ran despite the geometry-stamp flag")
            else:
                print("PASS: anchor floor stays absent after chunk reload "
                      "(builder did not re-run)")

            geom_after = room_geometry(args.port, gx, gy)
            want = (geom_before[0] - 1, geom_before[1], geom_before[2])
            if geom_after == want:
                print(f"PASS: rest of the room's geometry unchanged after reload "
                      f"({geom_after}, floors down by exactly the cleared tile)")
            else:
                failures.append(
                    f"room geometry changed on reload: before-clear={geom_before}, "
                    f"expected-after={want}, actual-after={geom_after}")

            if not has_stamped(args.port, gx, gy):
                failures.append(
                    "world.hasStampedLocation false after reload — the flag did not "
                    "survive save/load")
            else:
                print("PASS: geometry-stamp flag survived save/load")
        except _PhaseAborted:
            pass
        finally:
            quit_engine(args.port, proc)
    elif gx is None:
        failures.append("phase 2 skipped: no room from phase 1")

    # ---- Phase 3: a location placed but never visited before the save
    #      (its geometry-stamp flag was never set) still stamps correctly
    #      on its first-ever chunk load, post save/restart/load. ----
    proc = boot_isolated(args.port, root)
    gx2 = gy2 = cx2 = cy2 = None
    try:
        load_defs(args.port)
        gen_world(args.port, "sb", args.seed + 1, args.size)
        rooms2 = placed_ready(args.port)
        if not rooms2:
            failures.append("phase 3: no room placed on the second world")
        else:
            # Pick one NOT among the already-loaded centre chunks (-1..1 x -1..1
            # was force-loaded by gen_world) so it is genuinely never-visited.
            candidates = [e for e in rooms2 if not (-1 <= e["cx"] <= 1 and -1 <= e["cy"] <= 1)]
            if not candidates:
                failures.append("phase 3: every placed room fell inside the pre-loaded region")
            else:
                e2 = candidates[0]
                cx2, cy2, gx2, gy2 = e2["cx"], e2["cy"], e2["gx"], e2["gy"]
                if has_floor(args.port, gx2, gy2):
                    failures.append("phase 3: room appears stamped before its chunk ever loaded")
                saved_fresh = save_and_wait(args.port, "sb", slot_fresh,
                                            failures)
    finally:
        quit_engine(args.port, proc)

    if gx2 is not None and saved_fresh and not failures:
        proc = boot_isolated(args.port, root)
        try:
            load_defs(args.port)
            # Issue #763: the saved page ("sb", its own id verbatim -- no
            # more main_world remap) doesn't exist live until published.
            if not load_and_wait(args.port, slot_fresh, failures):
                raise _PhaseAborted
            send(args.port, "world.show('sb'); return 'ok'")
            time.sleep(1.0)
            load_chunk(args.port, cx2, cy2)
            if wait_floor(args.port, gx2, gy2):
                print(f"PASS: a never-before-loaded location's chunk still stamps "
                      f"correctly on first load after save/restart/load "
                      f"(room at {gx2},{gy2})")
            else:
                failures.append(
                    f"a location saved before first materialization did NOT stamp "
                    f"on its first post-load chunk load ({gx2},{gy2})")
        except _PhaseAborted:
            pass
        finally:
            quit_engine(args.port, proc)

    print("-" * 56)
    if failures:
        for f in failures:
            print(f"FAIL: {f}", file=sys.stderr)
        return 1
    print("ALL CHECKS PASSED")
    return 0


if __name__ == "__main__":
    try:
        raise SystemExit(main())
    except FixtureNotRegistered as exc:
        print(f"\n{exc}")
        raise SystemExit(1)
