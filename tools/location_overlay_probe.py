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
     sits on a land chunk, never an ocean one, and its footprint is clear
     of lakes / rivers / ocean (#414 — no carving a room next to water).
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
  8. Multiworld: a location on a HIDDEN, non-active page still stamps onto
     its OWN page, even when the active page already has a floor at the same
     tile (page-targeted writes + reads + idempotency guard). Checked with
     an arena as the active world that is given a room at (8,8); the hidden
     page must still stamp there, at its own terrain z, not the arena's.
  9. Placement matrix (#997): every world in a small, explicit
     (seed, size, plates) matrix gets at least one ruin_small. Phases 1-8
     all run on ONE tuple (seed 42 / size 64 / 3 plates), which is not
     the configuration players generate from — the GUI defaults to size
     128 with 10 plates. This phase samples that space too.

The location stamper is auto-loaded at boot by scripts/init.lua, exactly
as in the real game, so this only registers the location defs by hand
(headless skips the GUI data-loading step).

Usage:
  python3 tools/location_overlay_probe.py
  python3 tools/location_overlay_probe.py --seed 7 --size 64 --plates 3 --port 9189

Exit code 0 = all checks passed.
"""
from __future__ import annotations

import argparse
import json
import os
import shutil
import socket
import stat
import subprocess
import tempfile
import time
from pathlib import Path
from probelib import (FixtureNotRegistered, capture_request_id, quit_engine,
                      boot, load_fixture_yaml, send, wait_load_published,
                      wait_save_complete)
from probe_runner_diagnostics import FailureEmitter   # durable failure records (#1982)

LOG = "/tmp/location_overlay_engine.log"
#: #1982 — this run's durable failure records, built at import so the
#: offset each carries is measured from the probe's own start.
FAILURE = FailureEmitter("location_overlay_probe")
REPO = Path(__file__).resolve().parent.parent
#: Prefix of this invocation's throwaway resource root. The random
#: suffix mkdtemp appends to it is also the per-invocation save-slot
#: token, so the root and the slots it holds share one identity.
ROOT_PREFIX = "location_overlay_probe_"


# --------------------------------------------------------------------------
# Isolated resource root + request-specific save/load completion (#1620)
# --------------------------------------------------------------------------
class _PhaseAborted(Exception):
    """A phase's save or load never reached its own terminal successful
    status, so the phase must STOP rather than assert against whatever
    session happens to be live — the reader is exactly what must not
    start (#1620 requirements 2-4). The failure is already recorded; the
    surrounding `finally` still shuts the engine down in order."""


def _make_owner_writable(top: str) -> None:
    """Add owner write (and directory search) permission throughout the
    freshly copied `config/` tree.

    `shutil.copytree` reproduces the SOURCE's mode bits, so a checkout
    whose `config/` is read-only — a read-only mount, a CI cache
    restored read-only, an archive unpacked without write bits — would
    otherwise hand this run a private copy it can neither write nor
    delete: a directory needs owner write+search before any of its
    entries can be unlinked, so `remove_isolated_root` would report
    residue on a run that did nothing wrong. Only THIS invocation's copy
    is relaxed; the source's own mode bits are never touched (#1729; the
    tools/location_embark_probe.py pattern from #1569).
    """
    for path, dirs, files in os.walk(top):
        for name in [None, *dirs, *files]:
            target = path if name is None else os.path.join(path, name)
            try:
                mode = os.lstat(target).st_mode
                if stat.S_ISLNK(mode):
                    continue
                extra = (stat.S_IRWXU if stat.S_ISDIR(mode)
                         else stat.S_IRUSR | stat.S_IWUSR)
                os.chmod(target, stat.S_IMODE(mode) | extra)
            except OSError:
                # Best effort: a mode this process cannot change is
                # reported by the cleanup that actually trips over it,
                # naming the path it failed on, rather than here.
                pass


def make_isolated_root(base: str) -> str:
    """A throwaway resource root for THIS invocation: the repository's
    real scripts/assets/data symlinked in (read-only content, safe to
    share), its `config/` COPIED without the developer's `*.local.yaml`
    overrides, plus its OWN empty saves/ directory.

    Every boot below is handed this root, so the fixtures saved and
    loaded here are never written into, and can never be satisfied by, a
    slot reachable from a normal `cabal run` (#1620 requirement 5; the
    tools/chop_probe.py pattern). The CONTENT families are SYMLINKS, and
    shutil.rmtree unlinks a symlink rather than descending it, so
    teardown can never reach the repository's own directories.

    `config/` is NOT one of those families and must never be symlinked
    (#1729). Engine initialization is itself a `config/` writer:
    src/Engine/Asset/YamlNotifications.hs materializes
    `notifications.local.yaml` from registry defaults whenever that file
    is absent, and src/Engine/Core/Init.hs migrates tracked legacy
    configuration into absent local files. Through an aliased `config/`
    those writes land in the developer's checkout, and teardown — which
    only unlinks the alias — leaves them there. Copying also keeps a
    personal override from deciding what this run observes. The copy is
    made owner-writable so a read-only source cannot produce a tree this
    run is unable to remove.
    """
    root = os.path.join(base, "root")
    os.makedirs(root, exist_ok=True)
    for family in ("scripts", "assets", "data"):
        target = os.path.join(root, family)
        if not os.path.exists(target):
            os.symlink(os.path.join(REPO, family), target)
    config_dst = os.path.join(root, "config")
    if not os.path.exists(config_dst):
        shutil.copytree(os.path.join(REPO, "config"), config_dst,
                        ignore=shutil.ignore_patterns("*.local.yaml"))
        _make_owner_writable(config_dst)
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


def placed_on_page(port: int, page: str, tries: int = 20) -> list[dict]:
    """A specific page's placed-location list, without showing the page.

    The overlay lives in that page's gen params, so this works for a
    world that was generated and left hidden. Polls because world.init
    is asynchronous — an empty read right after waitForInit can mean
    "gen params not published yet" rather than "no locations".
    """
    last: list[dict] = []
    for _ in range(tries):
        raw = send(port, f"return world.listPlacedLocations('{page}')", timeout=30).strip()
        if raw and raw not in ("nil", "{}", "[]"):
            try:
                data = json.loads(raw)
            except json.JSONDecodeError:
                data = []
            if isinstance(data, list) and data:
                return data
            last = data if isinstance(data, list) else []
        time.sleep(0.5)
    return last


def key(entries: list[dict]) -> list[tuple]:
    """Stable, comparable signature of a placement set."""
    return sorted((e["cx"], e["cy"], e["id"]) for e in entries)


def is_ocean(port: int, gx: int, gy: int) -> bool:
    r = send(port, f"local f=world.getFluidAt({gx},{gy}); return f or 'dry'")
    return r.strip('"') == "ocean"


def footprint_water(port: int, gx: int, gy: int, r: int = 3) -> str:
    """Scan the room footprint + margin around (gx,gy) for any fluid tile
    (lake/river/ocean/lava). Returns 'x,y,type' for the first wet tile, or
    'dry'. One server-side call; the region stays inside the ruin's chunk."""
    lua = (f"return (function() for y={gy - r},{gy + r} do for x={gx - r},{gx + r} do "
           f"local f = world.getFluidAt(x, y); if f then return x..','..y..','..f end "
           f"end end return 'dry' end)()")
    return send(port, lua).strip('"')


def has_floor(port: int, gx: int, gy: int, page: str | None = None) -> bool:
    """True if a 'floor' structure piece exists at (gx,gy) on the given page
    (or the active world) — i.e. room_small has stamped its room there."""
    arg = f",'{page}'" if page else ""
    r = send(port, f"return structure.hasAt({gx},{gy},'floor'{arg}) and 'yes' or 'no'")
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


def wait_floor(port: int, gx: int, gy: int, page: str | None = None, tries: int = 40) -> bool:
    for _ in range(tries):
        if has_floor(port, gx, gy, page):
            return True
        time.sleep(0.5)
    return False


# A dense location def places a location on EVERY land chunk — so the centre
# chunk (0,0), land for our seed, is guaranteed one. (0,0) is only ever loaded
# synchronously (Init / Save regenerate it directly and exclude it from the
# chunk-load queue), so a floor there can only come from the synchronous-centre
# stamp hooks. The `waterside` anchor opts out of the #414 dry-ground filter
# (without any terrain constraint), so (0,0) is covered even when it sits near
# water — what we need to exercise the centre-chunk hooks regardless of seed.
DENSE_YAML = "/tmp/loc_overlay_probe_dense.yaml"
DENSE_BODY = (
    "locations:\n"
    "  - id: ruin_small\n"
    "    label: Small Ruin\n"
    "    type: ruin\n"
    "    builder: room_small\n"
    "    anchor: [waterside]\n"
    "    max_count: 100000\n"
    "    min_spacing: 1\n"
    "    bounds: { min_x: -2, min_y: -2, max_x: 2, max_y: 2 }\n"
    "    naming: { heads: [KEEP], modifiers: [ASH] }\n"
    "    contents: []\n"
)


# A THIN ruin_small: the shipped id and builder, but max_count 1. Used in
# phase 2 so the load's content validation finds the definition registered
# while a recompute against it still could not reproduce the saved
# placement set (see the phase-2 comment).
THIN_YAML = "/tmp/loc_overlay_probe_thin.yaml"
THIN_BODY = (
    "locations:\n"
    "  - id: ruin_small\n"
    "    label: Small Ruin\n"
    "    type: ruin\n"
    "    builder: room_small\n"
    "    anchor: [flat]\n"
    "    max_count: 1\n"
    "    min_spacing: 5\n"
    "    bounds: { min_x: -2, min_y: -2, max_x: 2, max_y: 2 }\n"
    "    naming: { heads: [KEEP], modifiers: [ASH] }\n"
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


# ---- Phase 9 placement matrix (#997) -------------------------------------
#
# Kept deliberately small: this probe is classified slow / worldgen-heavy
# and manual-only (tools/ci_probes.py), and phases 1-5 already spend five
# engine boots. The matrix generates at most MAX_MATRIX_ENTRIES worlds —
# one per boot, see the phase-9 comment — and skips the save/load,
# centre-chunk and hidden-page phases entirely, since those assume seed
# 42's particular geography and prove things placement does not.
#
# The exhaustive frequency measurement is NOT here: it is the one-off
# tools/location_placement_sweep.py (21 distinct worlds), recorded in
# docs/location_placement_sweep.md.
MAX_MATRIX_ENTRIES = 4
PLACEMENT_MATRIX = [
    # (seed, size, plates, why)
    (42, 64, 3, "the tuple phases 1-8 use — pinned here too, explicitly 3 plates"),
    (0, 128, 10, "GUI default size + plate count"),
    (1, 128, 10, "GUI default size + plate count, second seed"),
    (2, 64, 10, "smallest GUI size at the default plate count"),
]


def gen_world(port: int, page: str, seed: int, size: int, plates: int = 3) -> None:
    send(port, f"world.init('{page}', {seed}, {size}, {plates}); return 'ok'")
    send(port, "return world.waitForInit(600)", timeout=620)
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
    ap.add_argument("--plates", type=int, default=3,
                    help="tectonic plate count for phases 1-8 (the GUI "
                         "default is 10; this probe's fixed-geography "
                         "phases are calibrated for 3)")
    ap.add_argument("--port", type=int, default=9189)
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
            FAILURE.check(leftover)
    return 1 if leftover else rc


def run(args, root: str, token: str) -> int:
    slot_overlay = f"loc_overlay_probe_{token}"
    slot_centre = f"loc_centre_probe_{token}"

    failures: list[str] = []
    saved_overlay = False

    # ---- Phase 1: placement, determinism, lazy stamping; then save the
    #      world with its locations still UN-STAMPED (saved right after gen,
    #      before any far ruin chunk has loaded) so phase 2 can prove they
    #      are not lost. ----
    proc = boot_isolated(args.port, root)
    try:
        send(args.port, "engine.loadLocationYaml('data/locations/ruin_small.yaml'); return 'ok'")
        # The stamper is auto-loaded at boot by scripts/init.lua (as in the
        # real game), so we only have to register the location defs here.

        gen_world(args.port, "wa", args.seed, args.size, args.plates)
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
        # Phase 2 reads this fixture from a FRESH process, so the save
        # must be COMPLETE — not merely accepted — before that process
        # boots (#1620).
        saved_overlay = save_and_wait(args.port, "wa", slot_overlay, failures)

        # In-session lazy stamping: loading a ruin's chunk materializes its
        # geometry (engine dispatch -> stamper -> #88 builder). Doubles as
        # the on-land / anchor check.
        ocean_hits = []
        wet = []
        for e in ruins:
            load_chunk(args.port, e["cx"], e["cy"])
            if is_ocean(args.port, e["gx"], e["gy"]):
                ocean_hits.append(e)
            w = footprint_water(args.port, e["gx"], e["gy"])
            if w != "dry":
                wet.append((e["cx"], e["cy"], w))
        if ruins and not ocean_hits:
            print(f"PASS: all {len(ruins)} ruin(s) on land (anchor [flat] respected)")
        elif ocean_hits:
            failures.append(f"{len(ocean_hits)} ruin(s) placed on ocean tiles")
        if ruins and not wet:
            print(f"PASS: all {len(ruins)} ruin footprint(s) clear of water (#414)")
        elif wet:
            failures.append(f"water in/near {len(wet)} ruin footprint(s): {wet[:3]}")
        n = wait_stamped(args.port, ruins)
        if ruins and n == len(ruins):
            print(f"PASS: lazy stamping materialized all {n} ruin(s) as their chunks loaded")
        else:
            failures.append(f"only {n}/{len(ruins)} ruin(s) stamped in-session")

        # Determinism: a second independent generation with the same seed.
        gen_world(args.port, "wb", args.seed, args.size, args.plates)
        lb = placed_ready(args.port)
        if key(la) == key(lb) and la:
            print("PASS: same seed -> identical overlay (A == B)")
        else:
            failures.append(f"overlay not deterministic: A={key(la)} B={key(lb)}")
    finally:
        quit_engine(args.port, proc)

    # ---- Phase 2: a world saved with UN-STAMPED locations still
    #      materializes them after a fresh restart + load (the reviewer's
    #      option 2: chunk-load after a save consults the persisted overlay
    #      and stamps any not-yet-materialized entry). ----
    with open(THIN_YAML, "w") as fh:
        fh.write(THIN_BODY)

    proc = boot_isolated(args.port, root) if saved_overlay else None
    try:
        if proc is None:
            # Phase 1's save never reached its own terminal successful
            # status (save_and_wait already recorded why), so the fresh
            # reader process must not start at all — a stale generation
            # of this slot is exactly what it would otherwise read
            # (#1620 requirement 2).
            raise _PhaseAborted
        # This phase used to load with NO location YAML registered at all,
        # and argued from that that the overlay it read back could not be
        # a recompute. The load transaction's content-validation stage
        # (#763/#911) now REFUSES a save whose overlay names an
        # unregistered definition, so that setup aborts the load outright
        # — the probe has been failing here on master.
        #
        # Register a THIN ruin_small instead: same id (so validation is
        # satisfied) but max_count 1. The saved world placed 6, and a
        # recompute against this registry could only ever place 1, so
        # reading 6 back still proves the overlay came from the save.
        load_fixture_yaml(args.port, "engine.loadLocationYaml", THIN_YAML)
        # Issue #763: the saved page ("wa", its own id verbatim -- no more
        # main_world remap) doesn't exist live until the transaction
        # publishes.
        if not load_and_wait(args.port, slot_overlay, failures):
            raise _PhaseAborted
        send(args.port, "world.show('wa'); return 'ok'")
        time.sleep(1.0)

        lc = placed_ready(args.port)
        if key(lc) == key(la) and lc and len(lc) > 1:
            print(f"PASS: overlay survived save/load ({len(lc)} placements, "
                  f"more than a recompute against max_count 1 could produce)")
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
    except _PhaseAborted:
        pass
    finally:
        if proc is not None:
            quit_engine(args.port, proc)

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
    proc = boot_isolated(args.port, root)
    try:
        load_fixture_yaml(args.port, "engine.loadLocationYaml", DENSE_YAML)
        gen_world(args.port, "wc", args.seed, args.size, args.plates)
        if not has_loc_on(args.port, 0, 0):
            failures.append(f"seed {args.seed}: no location on centre chunk (0,0) — cannot test Init hook")
        elif wait_floor(args.port, 8, 8):
            print("PASS: synchronous centre chunk (0,0) stamped on first gen (Init hook)")
        else:
            failures.append("centre chunk (0,0) NOT stamped on first gen (Init hook)")
    finally:
        quit_engine(args.port, proc)

    # ---- Phase 4: a location on the SAVED CAMERA CHUNK is present on the
    #      FIRST load. The default camera sits on (0,0); save a world whose
    #      (0,0) hosts a location, then on a fresh restart confirm it is back
    #      WITHOUT force-loading (0,0) — Save regenerates that chunk
    #      synchronously and excludes it from the queue, so its presence
    #      exercises the Save centre hook. ----
    proc = boot_isolated(args.port, root)
    saved_centre = False
    try:
        load_fixture_yaml(args.port, "engine.loadLocationYaml", DENSE_YAML)
        gen_world(args.port, "wd", args.seed, args.size, args.plates)
        if not has_loc_on(args.port, 0, 0):
            failures.append(f"seed {args.seed}: no location on centre chunk (0,0) — cannot test Save hook")
        elif not wait_floor(args.port, 8, 8):
            failures.append("phase 4 setup: centre (0,0) did not stamp at gen")
        else:
            saved_centre = save_and_wait(args.port, "wd", slot_centre,
                                         failures)
    finally:
        quit_engine(args.port, proc)

    if saved_centre:
        proc = boot_isolated(args.port, root)
        try:
            load_fixture_yaml(args.port, "engine.loadLocationYaml", DENSE_YAML)
            # Issue #763: the saved page ("wd", its own id verbatim -- no
            # more main_world remap) doesn't exist live until published.
            if not load_and_wait(args.port, slot_centre, failures):
                raise _PhaseAborted
            send(args.port, "world.show('wd'); return 'ok'")
            # Do NOT force-load (0,0) — it is the synchronous centre chunk.
            if wait_floor(args.port, 8, 8):
                print("PASS: saved-camera centre chunk (0,0) present on first load (Save hook)")
            else:
                failures.append("saved-camera centre chunk (0,0) NOT present on first load (Save hook)")
        except _PhaseAborted:
            pass
        finally:
            quit_engine(args.port, proc)

    # ---- Phase 5: a location on a HIDDEN, non-active page still stamps,
    #      EVEN when the active page already has a floor at the same tile
    #      (multiworld). The active world is an arena that we deliberately
    #      give a room at (8,8); a second page is then generated hidden, also
    #      with a location at (8,8). The page-targeted hasAt guard must not
    #      let the arena's floor suppress the hidden page's stamp, and the
    #      write must land on the hidden page at ITS terrain z, not the
    #      arena's. ----
    proc = boot_isolated(args.port, root)
    try:
        load_fixture_yaml(args.port, "engine.loadLocationYaml", DENSE_YAML)
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
            # Put a room on the ACTIVE arena at (8,8) (arena terrain z=0 ->
            # floor z=1). This is the unrelated geometry that must NOT
            # suppress the hidden page's stamp at the same tile.
            send(args.port, "require('scripts.locations').stamp('ruin_small', 8, 8, 'arena'); return 'ok'")
            if not wait_floor(args.port, 8, 8, page="arena"):
                failures.append("phase 5 setup: could not place a floor on the active arena")
            else:
                # Generate a second world but DO NOT show it — arena stays active.
                send(args.port, f"world.init('sw', {args.seed}, {args.size}, {args.plates}); return 'ok'")
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
                elif wait_floor(args.port, 8, 8, page="sw"):
                    # sw stamped despite arena already having a floor at (8,8).
                    # Confirm it's sw's own room at sw's terrain z (29), not
                    # the arena's (1) — proving page-targeted write + guard.
                    swz = send(args.port, "return world.getTerrainAt(8,8,'sw')").split("\t")[0].strip()
                    fz_sw = send(args.port, "return structure.floorZAt(8,8,'sw')").strip()
                    fz_ar = send(args.port, "return structure.floorZAt(8,8,'arena')").strip()
                    if fz_sw == fz_ar:
                        failures.append(f"phase 5: sw floor z ({fz_sw}) == arena floor z ({fz_ar}) — pages not isolated")
                    else:
                        print(f"PASS: hidden page 'sw' stamped at (8,8) despite the active "
                              f"'arena' already having a floor there (sw floor z={fz_sw}=sw "
                              f"terrain {swz}+1; arena floor z={fz_ar})")
                else:
                    failures.append("hidden page suppressed by active-world geometry at same tile (multiworld)")
    finally:
        quit_engine(args.port, proc)

    # ---- Phase 9: placement matrix (#997). Placement only — one boot,
    #      one page per tuple, generated but never shown (the overlay
    #      lives in gen params, so listPlacedLocations takes the page id
    #      directly). Every world here must get at least one ruin_small:
    #      after #997 an empty list can only mean the world has no land
    #      at all, and none of these tuples is a waterworld. ----
    assert len(PLACEMENT_MATRIX) <= MAX_MATRIX_ENTRIES
    for seed, size, plates, why in PLACEMENT_MATRIX:
        label = f"seed {seed} / size {size} / {plates} plates"
        # One engine per entry. `world.waitForInit` (like getInitProgress)
        # waits on the ACTIVE world, which with nothing shown is just the
        # head of wmWorlds — NOT the page just handed to world.init. Run
        # several entries in one engine and the second wait can answer for
        # the first, already-LoadDone world before the queued init has even
        # registered, returning phase 3 instantly and making a perfectly
        # normal slow w128 generation read as zero. With exactly one page
        # per process the query cannot refer to anything else. The extra
        # boots are seconds against these generations, and each entry is
        # then independently reproducible from its own command line.
        proc = boot_isolated(args.port, root)
        try:
            send(args.port,
                 "engine.loadLocationYaml('data/locations/ruin_small.yaml'); return 'ok'")
            send(args.port, f"world.init('mx', {seed}, {size}, {plates}); return 'ok'")
            # waitForInit reports where generation GOT TO, timeout or not, so
            # check the phase before reading placements — otherwise a timed-out
            # generation reads as empty and gets misreported below as the
            # guarantee failing to fire. `local phase = ...` keeps the first of
            # its four return values; 3 == done.
            phase = send(args.port, "local phase = world.waitForInit(900); return phase",
                         timeout=920).strip()
            if phase != "3":
                failures.append(f"{label}: generation did not finish "
                                f"(waitForInit phase {phase or '<no reply>'}, 3 = done)")
                continue
            entries = placed_on_page(args.port, "mx")
            ruins = [e for e in entries if e["id"] == "ruin_small"]
            if ruins:
                print(f"PASS: {label}: {len(ruins)} ruin_small ({why})")
            else:
                failures.append(
                    f"{label}: ZERO ruin_small placed — the #997 guarantee "
                    f"did not fire (or the world has no land at all)")
        finally:
            quit_engine(args.port, proc)

    print("-" * 56)
    if failures:
        # Durable records rather than the unflushed stderr print this was
        # (#1982): `run_probes.py` merges this probe's stderr into a
        # block-buffered stdout pipe and prints only its last 25 lines, so
        # a printed `FAIL:` overtook the buffered checks and landed above
        # the retained tail. These are read back from the COMPLETE capture.
        FAILURE.report(failures)
        FAILURE.context_log(LOG)
        return 1
    print("ALL CHECKS PASSED")
    return 0


if __name__ == "__main__":
    try:
        raise SystemExit(main())
    except FixtureNotRegistered as exc:
        print(f"\n{exc}")
        raise SystemExit(1)
