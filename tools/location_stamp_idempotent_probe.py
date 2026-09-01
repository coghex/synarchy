#!/usr/bin/env python3
r"""Headless location geometry-stamp idempotency probe (#424).

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

Materialized footprint (#1575)
------------------------------
The structure counts above cannot see the GROUND. `builders.room_small`
places its 25 floors, 20 walls and 4 posts at the explicit `baseZ` that
`locations.flattenFootprint` returned, so they register whether or not
the terrain under them was ever levelled — and the two verbs that level
it, `world.setCell` and `world.setSlope`, return true as soon as the
edit is QUEUED and only warn when the world thread later refuses it
(src/World/Thread/Command/Edit/Terrain.hs). This probe therefore also
asserts, for the room it tests:

  4. every tile of the 5x5 footprint reports the same terrain surface z
     (`world.getTerrainAt`'s SECOND return, terrainSurfaceZ — the value
     flattenFootprint itself levels against), naming the offending tiles
     when they do not; and
  5. the engine rejected no footprint-levelling edit while stamping it,
     read from each boot's own complete log.

`anchor: [flat]` (#1575 requirement 1) is the constraint the shipped 5x5
room content declares (data/locations/ruin_small.yaml). It is a CHUNK
constraint, not a footprint one, and a weak one: `flat` is
`cmElevRange cm <= flatCut cuts` where flatCut is the MEDIAN elevation
range over the world's land chunks (src/Location/Overlay.hs). A chunk at
or below that median still routinely carries a 5x5 whose corners sit two
or three z apart, which flattenFootprint's single pass cannot level. So
the probe SELECTS the site it asserts on (see `scout_site`) instead of
taking whatever the overlay listed first, and a run that cannot obtain a
conforming site says so as a SETUP FAILURE — distinct from a
materialization failure on a site it did obtain.

Usage:
  python3 tools/location_stamp_idempotent_probe.py
  python3 tools/location_stamp_idempotent_probe.py --seed 42 --size 64 --port 9191

Acceptance (#1575):
  python3 tools/location_stamp_idempotent_probe.py --port 9191
  grep -c 'Set cell z below column floor\|Set slope z out of column range' \
    /tmp/location_stamp_idempotent_engine.log \
    /tmp/location_stamp_idempotent_engine.phase2.log \
    /tmp/location_stamp_idempotent_engine.phase3.log \
    /tmp/location_stamp_idempotent_engine.phase4.log      # expect 0 each

Every boot gets its OWN log, because probelib.boot opens its log with
mode "w": one shared path would let a later phase truncate the stamping
diagnostics an earlier one is judged on. The four above are the boots
that run the phases, and they carry no refused levelling edit at all.
The two `...scout_a.log` / `...scout_b.log` siblings are deliberately
NOT in that list: the scout stamps candidate rooms precisely to find out
which of them cannot be levelled, so refusals naming those rooms are its
expected output. Whether the SHIPPED flat-anchored locations should be
placed so that never happens is #1575's out-of-scope bullet 3; this
probe fails on a refusal inside the footprint it tests, and on nothing
else.

Exit code 0 = all checks passed.
"""
from __future__ import annotations

import argparse
import json
import os
import re
import shutil
import socket
import stat
import subprocess
import tempfile
import time
from collections import Counter
from pathlib import Path
from probelib import (FixtureNotRegistered, capture_request_id, quit_engine,
                      boot, load_fixture_yaml, send, wait_load_published,
                      wait_save_complete)
from probe_runner_diagnostics import FailureEmitter   # durable failure records (#1982)

LOG = "/tmp/location_stamp_idempotent_engine.log"
#: The engine log the phase now running writes to. `report` reads a
#: bounded tail of it, which is what tells a fixture failure apart from
#: a product failure without rerunning the probe (#1982).
_current_log: list[str] = [LOG]
#: #1982 — this run's durable failure records, built at import so the
#: offset each carries is measured from the probe's own start.
FAILURE = FailureEmitter("location_stamp_idempotent_probe")
LOCATION_YAML = "/tmp/location_stamp_idempotent_probe_loc.yaml"
LOCATION_ID = "stamp_probe_room"
REPO = Path(__file__).resolve().parent.parent
#: Half-width of the fixture's footprint, i.e. its `bounds` below. The
#: 5x5 room is [gx-2..gx+2] x [gy-2..gy+2], and both the terrain oracle
#: and the rejected-edit filter are scoped to exactly that box.
FOOTPRINT_RADIUS = 2
#: How many placed rooms `scout_site` will stamp looking for a
#: conforming one, and how many level ones it needs before it stops
#: early. Roughly three `[flat]` sites in four conform, so three level
#: candidates is a comfortable margin at a bounded cost.
SCOUT_MAX_CANDIDATES = 6
SCOUT_WANTED_LEVEL = 3

#: The world thread's two refusals of a footprint-levelling edit
#: (src/World/Thread/Command/Edit/Terrain.hs). Both name the tile, which
#: is what lets this probe hold itself to the room it is testing rather
#: than to every room a boot happens to stamp.
REJECTED_EDIT_RE = re.compile(
    r"Set (?:cell z below column floor|slope z out of column range) "
    r"at (-?\d+),(-?\d+)")
#: src/World/Thread/Command/Init.hs's diagnostics for the two placement
#: outcomes that are not a strict, anchor-tag-satisfying placement. The
#: #997 fallback is explicitly permitted to violate anchor tags
#: (src/Location/Overlay.hs), so a location it supplied is NOT a `[flat]`
#: site and cannot be held to the materialization contract.
GUARANTEED_PLACEMENT_MARKER = "placed one guaranteed location"
NO_LAND_MARKER = "World contains no land"
#: Prefix of this invocation's throwaway resource root. The random
#: suffix mkdtemp appends to it is also the per-invocation save-slot
#: token, so the root and the slots it holds share one identity.
ROOT_PREFIX = "location_stamp_idempotent_probe_"


# --------------------------------------------------------------------------
# Isolated resource root + request-specific save/load completion (#1620)
# --------------------------------------------------------------------------
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


class _PhaseAborted(Exception):
    """A phase's save or load never reached its own terminal successful
    status, so the phase must STOP rather than assert against whatever
    session happens to be live — the reader is exactly what must not
    start (#1620 requirements 2-4). The failure is already recorded; the
    surrounding `finally` still shuts the engine down in order."""


def boot_isolated(port: int, root: str, log: str = LOG):
    """Boot one engine with its OWN log path.

    probelib.boot opens the log with mode "w", so two boots sharing a
    path leave only the LAST one's output on disk. This probe now judges
    each boot on its own stamping diagnostics, which makes a per-boot
    path load-bearing rather than cosmetic.

    The path is also recorded as the CURRENT log, so `report` can retain
    a bounded excerpt of whichever phase's engine was last live (#1982).
    """
    _current_log[0] = log
    return boot(port, log=log, args=["--resource-root", root])


def phase_log(name: str) -> str:
    """This run's log path for one boot. Phase 1 keeps the historical
    path so an operator's habits (and the acceptance grep) still name a
    real file; every other boot gets a sibling."""
    return LOG if name == "phase1" else LOG[:-len(".log")] + f".{name}.log"


def save_and_wait(port: int, page: str, slot: str,
                  failures: list[str], log: str = LOG) -> bool:
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
            f"(returned {accepted!r}); the validation reason is logged in {log}")
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
                  seconds: float = 60.0, log: str = LOG) -> bool:
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
            f"{accepted!r}); the reason is logged in {log}")
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
            # #1575 requirement 1: the SAME class of terrain constraint
            # the shipped 5x5 room content declares
            # (data/locations/ruin_small.yaml `anchor: [flat]`, "The room
            # wants flat ground"). An empty list is not a weaker
            # constraint but NO constraint — Location.Overlay.anchorOk is
            # `all ok tags`, so `[]` is vacuously true and the overlay
            # accepts any dry land chunk, including ones the shipped
            # placement rules would never choose.
            "    anchor: [flat]\n"
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


def placed_ready(port: int, page: str | None = None,
                 tries: int = 30) -> list[dict]:
    last: list[dict] = []
    for _ in range(tries):
        last = [e for e in placed(port, page) if e.get("id") == LOCATION_ID]
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


# --------------------------------------------------------------------------
# Materialized-footprint oracle (#1575)
# --------------------------------------------------------------------------
def footprint_terrain(port: int, gx: int, gy: int,
                      page: str) -> dict[tuple[int, int], int | None] | None:
    """terrainSurfaceZ for every tile of the 5x5 footprint at (gx, gy).

    The SECOND value world.getTerrainAt returns
    (src/Engine/Scripting/Lua/API/WorldQuery/Terrain.hs): the terrain-only
    surface, which is the value `locations.flattenFootprint` itself reads
    and levels against. The first return, surfaceZ, is max(terrain,
    fluid) and would report water rather than the ground the room's
    pieces sit on.

    The page id is passed EXPLICITLY: the stamper authors geometry
    against a named page, and reading the active world instead would
    silently answer about a different one. A tile whose chunk is not
    loaded answers nil, which comes back as an ABSENT value (None) and is
    never confused with a height. Returns None if the reply could not be
    parsed at all.
    """
    r = FOOTPRINT_RADIUS
    lua = (f"local o={{}}; "
           f"for x={gx - r},{gx + r} do for y={gy - r},{gy + r} do "
           f"local _, tz = world.getTerrainAt(x, y, '{page}'); "
           f"o[#o+1] = x .. ':' .. y .. ':' .. (tz and tostring(tz) or 'nil') "
           f"end end; return table.concat(o, ',')")
    raw = send(port, lua).strip('"')
    out: dict[tuple[int, int], int | None] = {}
    for part in raw.split(","):
        bits = part.split(":")
        if len(bits) != 3:
            return None
        try:
            key = (int(bits[0]), int(bits[1]))
        except ValueError:
            return None
        if bits[2] == "nil":
            out[key] = None
        else:
            try:
                out[key] = int(bits[2])
            except ValueError:
                return None
    expected = (2 * r + 1) ** 2
    return out if len(out) == expected else None


def settle_footprint(port: int, gx: int, gy: int, page: str,
                     seconds: float = 20.0
                     ) -> tuple[bool, dict[tuple[int, int], int | None]]:
    """Read the footprint until it stops changing, or the deadline passes.

    flattenFootprint's world.setCell / world.setSlope calls are QUEUED to
    the world thread and applied afterwards (scripts/locations.lua), so a
    single read can catch the footprint mid-levelling. Two consecutive
    identical, fully populated readings is the bounded synchronization
    this needs; it is a real wait, not a fixed sleep, and it distinguishes
    "still settling" from "settled and not level".

    Returns (settled, values). `settled` False means the deadline passed
    with the readings still moving or a tile still unreadable — on a site
    that was already established as conforming, that is a materialization
    failure, not a setup one.
    """
    deadline = time.monotonic() + seconds
    prev: dict[tuple[int, int], int | None] | None = None
    cur: dict[tuple[int, int], int | None] | None = None
    while True:
        cur = footprint_terrain(port, gx, gy, page)
        if (cur is not None and cur == prev
                and all(v is not None for v in cur.values())):
            return True, cur
        prev = cur
        if time.monotonic() >= deadline:
            return False, (cur if cur is not None else {})
        time.sleep(0.5)


def _tiles(keys) -> str:
    return ", ".join(f"({x},{y})" for x, y in keys)


def level_problem(gx: int, gy: int,
                  values: dict[tuple[int, int], int | None]) -> str | None:
    """None when the footprint materialized level, else the message.

    "Level" is every one of the 25 tiles reporting the SAME
    terrainSurfaceZ — what flattenFootprint carves the whole box down to,
    and what the structure counts cannot observe. The message names the
    offending tiles and their observed values so a failure points at the
    ground rather than at the assertion.
    """
    expected = (2 * FOOTPRINT_RADIUS + 1) ** 2
    if len(values) != expected:
        return (f"the footprint at ({gx},{gy}) answered for {len(values)} of "
                f"its {expected} tiles")
    missing = sorted(k for k, v in values.items() if v is None)
    if missing:
        return (f"the footprint at ({gx},{gy}) reported no terrain surface z "
                f"for {len(missing)} of its {expected} tiles (chunk not "
                f"loaded there?): {_tiles(missing)}")
    zs = sorted({v for v in values.values()})
    if len(zs) == 1:
        return None
    base = Counter(values.values()).most_common(1)[0][0]
    off = sorted(k for k, v in values.items() if v != base)
    return (f"the footprint at ({gx},{gy}) did NOT materialize level: "
            f"{len(zs)} distinct terrain surface z values {zs} across its "
            f"{expected} tiles; {len(off)} differ from the majority z={base}: "
            + ", ".join(f"({x},{y})=z{values[(x, y)]}" for x, y in off))


def check_level(port: int, gx: int, gy: int, page: str, label: str,
                failures: list[str]) -> None:
    """Assert the footprint materialized level, after a bounded settle."""
    settled, values = settle_footprint(port, gx, gy, page)
    if not settled:
        failures.append(
            f"{label}: the footprint at ({gx},{gy}) on page '{page}' never "
            f"settled within the levelling deadline — flattenFootprint's "
            f"queued edits never came to rest. Last reading: "
            f"{_reading(values)}")
        return
    problem = level_problem(gx, gy, values)
    if problem:
        failures.append(f"{label}: {problem}")
        return
    z = next(iter(values.values()))
    print(f"PASS: {label} — all {len(values)} footprint tiles level at "
          f"terrain z={z}")


def _reading(values: dict[tuple[int, int], int | None]) -> str:
    if not values:
        return "no readable reply"
    return ", ".join(f"({x},{y})={'nil' if values[(x, y)] is None else values[(x, y)]}"
                     for x, y in sorted(values))


# --------------------------------------------------------------------------
# Per-boot engine log (#1575)
# --------------------------------------------------------------------------
def read_phase_log(path: str) -> tuple[str | None, str | None]:
    """(text, error) for one boot's complete log.

    Read only AFTER that engine has exited: a redirected stdio handle
    block-buffers, so an in-process read races whatever has flushed so
    far. An unreadable, empty, or truncated log is an ERROR — the
    rejected-edit check below must never read one of those as "zero
    rejections", which is the exact false green #1575 exists to remove.
    READY is the marker probelib.boot itself waited for, so its absence
    means this is not the file that boot observed.
    """
    try:
        with open(path, encoding="utf-8", errors="replace") as fh:
            text = fh.read()
    except OSError as exc:
        return None, f"could not read the engine log {path}: {exc}"
    if not text.strip():
        return None, f"the engine log {path} is empty"
    if "READY" not in text:
        return None, (f"the engine log {path} does not carry its own boot's "
                      f"READY marker, so it is truncated or was replaced and "
                      f"its stamping diagnostics cannot be trusted")
    return text, None


def rejected_footprint_edits(text: str, gx: int, gy: int) -> list[str]:
    """The log's refusals of a levelling edit INSIDE this footprint.

    Scoped to the tested room on purpose. A boot stamps whatever rooms
    its chunk loads touch — the scout deliberately stamps several — and
    whether OTHER placed rooms level is #1575's out-of-scope bullet 3.
    """
    out = []
    for line in text.splitlines():
        m = REJECTED_EDIT_RE.search(line)
        if not m:
            continue
        x, y = int(m.group(1)), int(m.group(2))
        if (abs(x - gx) <= FOOTPRINT_RADIUS
                and abs(y - gy) <= FOOTPRINT_RADIUS):
            out.append(line.strip())
    return out


def check_no_rejected_edits(path: str, label: str, gx: int, gy: int,
                            failures: list[str]) -> None:
    """Fail if this boot refused any levelling edit under the tested room.

    world.setCell / world.setSlope answer true as soon as the edit parses
    and is queued, and the world thread only WARNS when it later refuses
    it, so this log is the only place the refusal exists. Every boot in
    which the tested location may stamp is checked.
    """
    text, err = read_phase_log(path)
    if err:
        failures.append(f"{label}: {err}")
        return
    lines = rejected_footprint_edits(text, gx, gy)
    if not lines:
        print(f"PASS: {label} — no rejected footprint-levelling edit under "
              f"the room at ({gx},{gy})")
        return
    shown = " | ".join(lines[:6])
    more = f" | ... and {len(lines) - 6} more" if len(lines) > 6 else ""
    failures.append(
        f"{label}: the engine REJECTED {len(lines)} footprint-levelling "
        f"edit(s) under the room at ({gx},{gy}) — its pieces were placed on "
        f"ground that was never levelled ({path}): {shown}{more}")


# --------------------------------------------------------------------------
# Site selection (#1575 requirement 4)
# --------------------------------------------------------------------------
def scout_site(args, root: str, page: str, seed: int, log: str,
               setup_failures: list[str]) -> dict | None:
    """Choose the site the phases below will hold to the contract.

    `anchor: [flat]` is a CHUNK constraint — `cmElevRange cm <= flatCut
    cuts`, flatCut being the MEDIAN elevation range over the world's land
    chunks (src/Location/Overlay.hs) — so a conforming chunk still
    routinely carries a 5x5 footprint whose corners sit two or three z
    apart. `locations.flattenFootprint` levels in ONE pass: it carves
    lo+1..hi to air and writes a flat slope at `lo`, and the world thread
    refuses both for a column whose stored range does not reach down to
    `lo` (src/World/Thread/Command/Edit/Terrain.hs). Measured on this
    fixture, roughly one `[flat]` site in four cannot be levelled that
    way. Whether the SHIPPED flat-anchored locations should be placed
    differently is #1575's out-of-scope bullet 3; what this probe needs
    is to stop that being a coin flip.

    So: boot an engine of this run's own, generate the very world the
    phase under test will generate (worldgen and location placement are
    pure functions of the generation tuple, so the sites and their
    terrain are identical), stamp candidate rooms until enough have come
    out level, and — once that engine has EXITED, so its log is complete
    rather than half-buffered — return the first candidate that both
    materialized level and drew no rejected levelling edit.

    Candidates are restricted to chunks OUTSIDE the -1..1 region
    `gen_world` force-loads, for two reasons: phase 1 must observe its
    room stamp on a chunk load of its own, and phase 3 must be able to
    save a session in which the room was never visited.

    Returns None, having recorded a SETUP failure naming the cause, when
    this run cannot obtain a conforming site: no room placed, only a #997
    guaranteed-fallback room (which src/Location/Overlay.hs explicitly
    permits to violate anchor tags, so it is not a `[flat]` site at all),
    no room outside the pre-loaded region, or no scanned candidate
    conforming.
    """
    rooms: list[dict] = []
    candidates: list[dict] = []
    scanned: list[dict] = []
    proc = boot_isolated(args.port, root, log)
    try:
        load_defs(args.port)
        gen_world(args.port, page, seed, args.size)
        rooms = placed_ready(args.port, page)
        candidates = [e for e in rooms
                      if not (-1 <= e["cx"] <= 1 and -1 <= e["cy"] <= 1)]
        print(f"scout '{page}' (seed {seed}): {len(rooms)} {LOCATION_ID} "
              f"placed, {len(candidates)} outside the pre-loaded region")
        level_seen = 0
        for e in candidates[:SCOUT_MAX_CANDIDATES]:
            gx, gy = e["gx"], e["gy"]
            load_chunk(args.port, e["cx"], e["cy"])
            if not wait_floor(args.port, gx, gy, page):
                problem = "never stamped on its first chunk load"
            else:
                settled, values = settle_footprint(args.port, gx, gy, page)
                problem = (None if settled
                           else "its footprint terrain never settled")
                if problem is None:
                    problem = level_problem(gx, gy, values)
            scanned.append({**e, "problem": problem})
            print(f"  candidate ({gx},{gy}): "
                  + ("level" if problem is None else problem))
            if problem is None:
                level_seen += 1
                if level_seen >= SCOUT_WANTED_LEVEL:
                    break
    finally:
        quit_engine(args.port, proc)

    text, err = read_phase_log(log)
    if err:
        setup_failures.append(f"scout of world '{page}' (seed {seed}): {err}")
        return None
    if GUARANTEED_PLACEMENT_MARKER in text:
        setup_failures.append(
            f"world '{page}' (seed {seed}) reported the #997 guaranteed "
            f"placement fallback, which src/Location/Overlay.hs explicitly "
            f"permits to violate a definition's anchor tags — so this run "
            f"never obtained a strictly placed `[flat]` site to hold to the "
            f"materialization contract")
        return None
    if NO_LAND_MARKER in text or not rooms:
        setup_failures.append(
            f"world '{page}' (seed {seed}) placed no {LOCATION_ID} at all, so "
            f"there is no site to test; try another --seed")
        return None
    if not candidates:
        setup_failures.append(
            f"world '{page}' (seed {seed}) placed every {LOCATION_ID} inside "
            f"the chunk region gen_world force-loads, so no room can be "
            f"observed stamping on a chunk load of its own; try another "
            f"--seed")
        return None

    for e in scanned:
        if e["problem"] is not None:
            continue
        rejected = rejected_footprint_edits(text, e["gx"], e["gy"])
        if rejected:
            print(f"  candidate ({e['gx']},{e['gy']}): level, but the engine "
                  f"refused {len(rejected)} levelling edit(s) under it — "
                  f"skipped")
            continue
        print(f"scout '{page}': testing the room at ({e['gx']},{e['gy']}) "
              f"in chunk ({e['cx']},{e['cy']})")
        return e

    setup_failures.append(
        f"no conforming site on world '{page}' (seed {seed}): none of the "
        f"{len(scanned)} scanned `[flat]` room(s) both materialized a level "
        f"5x5 footprint and drew no rejected levelling edit. `flat` is a "
        f"chunk-level constraint (elevation range at or below the world "
        f"median), so it does not by itself guarantee a footprint "
        f"flattenFootprint can level; try another --seed, or raise "
        f"SCOUT_MAX_CANDIDATES")
    return None


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
            FAILURE.check(leftover)
    return 1 if leftover else rc


def report(failures: list[str], setup_failures: list[str]) -> int:
    """One exit convention, two DISTINCT vocabularies (#1575 requirement 4).

    A setup failure means this run never obtained the thing it tests — no
    conforming `[flat]` site, a #997 fallback placement, an unreadable
    log. A plain failure means it did obtain one and the engine then
    behaved wrongly on it. Both exit 1, as every probe here does; only
    the label separates "try another seed" from "there is a bug".
    """
    print("-" * 56)
    if setup_failures or failures:
        # Durable records rather than the unflushed stderr prints these
        # were (#1982). `run_probes.py` merges this probe's stderr into a
        # block-buffered stdout pipe and prints only its last 25 lines, so
        # a printed `FAIL:` overtook the buffered checks and landed above
        # the retained tail. These are read back from the COMPLETE
        # capture, and the two vocabularies survive as distinct kinds.
        FAILURE.report(failures, setup_failures)
        FAILURE.context_log(_current_log[0])
        return 1
    print("ALL CHECKS PASSED")
    return 0


def run(args, root: str, token: str) -> int:
    slot_main = f"stamp_idempotent_probe_{token}"
    slot_fresh = f"stamp_idempotent_probe_fresh_{token}"

    failures: list[str] = []
    setup_failures: list[str] = []
    write_location_yaml()

    # ---- Phase 0: choose the two sites this run will assert on, one per
    #      world, each proven to be a strictly placed `[flat]` room whose
    #      footprint the builder can actually level (#1575). Each scout
    #      boots and exits on its own so its log — the only record of a
    #      refused levelling edit — is complete when it is read. ----
    log_scout_a = phase_log("scout_a")
    log_scout_b = phase_log("scout_b")
    site_a = scout_site(args, root, "sa", args.seed, log_scout_a,
                        setup_failures)
    site_b = (scout_site(args, root, "sb", args.seed + 1, log_scout_b,
                         setup_failures)
              if site_a is not None else None)
    if site_a is None or site_b is None:
        return report(failures, setup_failures)

    cx, cy, gx, gy = site_a["cx"], site_a["cy"], site_a["gx"], site_a["gy"]
    cx2, cy2, gx2, gy2 = (site_b["cx"], site_b["cy"],
                          site_b["gx"], site_b["gy"])

    log1, log2 = phase_log("phase1"), phase_log("phase2")
    log3, log4 = phase_log("phase3"), phase_log("phase4")

    # ---- Phase 1: first load stamps the room; the footprint under it is
    #      really levelled; clear only the anchor floor, save, and quit. ----
    geom_before = (-1, -1, -1)
    saved_main = False
    saved_fresh = False
    proc = boot_isolated(args.port, root, log1)
    try:
        load_defs(args.port)
        gen_world(args.port, "sa", args.seed, args.size)
        rooms = placed_ready(args.port, "sa")
        print(f"world (seed {args.seed}): {len(rooms)} {LOCATION_ID} placed")
        # Placement is a pure function of the generation tuple, so the
        # scouted site must reappear here verbatim. If it does not, the
        # scout measured a different world and nothing below is about the
        # site it cleared.
        if not any(e["gx"] == gx and e["gy"] == gy for e in rooms):
            setup_failures.append(
                f"the scouted site ({gx},{gy}) is absent from world 'sa' "
                f"(seed {args.seed}) on this boot, so location placement is "
                f"not reproducing across boots and the scouted site cannot "
                f"be tested")
            raise _PhaseAborted
        load_chunk(args.port, cx, cy)
        if not wait_floor(args.port, gx, gy, "sa"):
            failures.append(f"room at ({gx},{gy}) never stamped on first load")
        else:
            print(f"PASS: room at ({gx},{gy}) stamped on first chunk load")
            # The pieces are placed at the explicit baseZ flattenFootprint
            # returned, so they appear whether or not the ground beneath
            # them was ever levelled: the counts and the terrain are two
            # independent claims and both are checked (#1575).
            geom_before = room_geometry(args.port, gx, gy)
            if geom_before != (25, 20, 4):
                failures.append(
                    f"unexpected initial geometry {geom_before} (want (25, 20, 4))")
            check_level(args.port, gx, gy, "sa",
                        "footprint levelled by the first stamp", failures)
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
                                       failures, log=log1)
    except _PhaseAborted:
        pass
    finally:
        quit_engine(args.port, proc)
    # A setup failure here means nothing stamped, so the log check would
    # print a PASS about a phase that never ran.
    if setup_failures:
        return report(failures, setup_failures)
    check_no_rejected_edits(log1, "phase 1", gx, gy, failures)

    # ---- Phase 2: restart -> load -> reload the same chunk. A real
    #      chunk LOAD (fresh process, nothing cached) must NOT re-run the
    #      builder: the anchor floor must stay absent and the rest of the
    #      room's geometry must be unchanged. ----
    if saved_main and not failures:
        proc = boot_isolated(args.port, root, log2)
        try:
            load_defs(args.port)
            # Issue #763: the saved page ("sa", its own id verbatim -- no
            # more main_world remap) doesn't exist live until published.
            if not load_and_wait(args.port, slot_main, failures, log=log2):
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

            # The levelled ground is edit-log state like the pieces are,
            # so it must come back with them.
            check_level(args.port, gx, gy, "sa",
                        "footprint still level after save/restart/reload",
                        failures)

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
        check_no_rejected_edits(log2, "phase 2", gx, gy, failures)

    # ---- Phase 3: a location placed but never visited before the save
    #      (its geometry-stamp flag was never set) still stamps correctly
    #      on its first-ever chunk load, post save/restart/load. ----
    proc = boot_isolated(args.port, root, log3)
    try:
        load_defs(args.port)
        gen_world(args.port, "sb", args.seed + 1, args.size)
        rooms2 = placed_ready(args.port, "sb")
        if not any(e["gx"] == gx2 and e["gy"] == gy2 for e in rooms2):
            setup_failures.append(
                f"the scouted site ({gx2},{gy2}) is absent from world 'sb' "
                f"(seed {args.seed + 1}) on this boot, so location placement "
                f"is not reproducing across boots")
            raise _PhaseAborted
        # Scouted OUTSIDE the region gen_world force-loads, so this room
        # is genuinely never-visited in the session about to be saved.
        if has_floor(args.port, gx2, gy2, "sb"):
            failures.append("phase 3: room appears stamped before its chunk ever loaded")
        saved_fresh = save_and_wait(args.port, "sb", slot_fresh,
                                    failures, log=log3)
    except _PhaseAborted:
        pass
    finally:
        quit_engine(args.port, proc)
    if setup_failures:
        return report(failures, setup_failures)
    check_no_rejected_edits(log3, "phase 3", gx2, gy2, failures)

    if saved_fresh and not failures:
        proc = boot_isolated(args.port, root, log4)
        try:
            load_defs(args.port)
            # Issue #763: the saved page ("sb", its own id verbatim -- no
            # more main_world remap) doesn't exist live until published.
            if not load_and_wait(args.port, slot_fresh, failures, log=log4):
                raise _PhaseAborted
            send(args.port, "world.show('sb'); return 'ok'")
            time.sleep(1.0)
            load_chunk(args.port, cx2, cy2)
            if wait_floor(args.port, gx2, gy2, "sb"):
                print(f"PASS: a never-before-loaded location's chunk still stamps "
                      f"correctly on first load after save/restart/load "
                      f"(room at {gx2},{gy2})")
                # This stamp is the first in its lineage and runs against
                # regenerated terrain, so its levelling is a fresh claim.
                check_level(args.port, gx2, gy2, "sb",
                            "footprint levelled by the post-load first stamp",
                            failures)
            else:
                failures.append(
                    f"a location saved before first materialization did NOT stamp "
                    f"on its first post-load chunk load ({gx2},{gy2})")
        except _PhaseAborted:
            pass
        finally:
            quit_engine(args.port, proc)
        check_no_rejected_edits(log4, "phase 4", gx2, gy2, failures)

    return report(failures, setup_failures)


if __name__ == "__main__":
    try:
        raise SystemExit(main())
    except FixtureNotRegistered as exc:
        print(f"\n{exc}")
        raise SystemExit(1)
