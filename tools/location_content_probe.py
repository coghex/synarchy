#!/usr/bin/env python3
"""Headless location content-spawning probe (#90) + ruin probe (#91).

Issues #88/#89 give locations a definition and a place in the world;
this checks the `contents` list actually spawns things when a
location's chunk loads, end to end:

  1. Visiting a `ruin_small` (#91: a partially-collapsed room) spawns
     its contents — two `ruin_common` loot-table rolls, as ground
     items — plus its persisted uniform 0..3 `nomad_primitive` encounter
     roll per ruin, and no buildings. #921 removed the two fixed-position `radio` /
     `canteen_steel_2l` entries this used to also expect: a ruin
     guarantees NO specific item, so the only assertion left about
     WHICH items appear is that each resolves to a registered def. The
     geometry is a damaged `room_small`: all 25 floors present, a
     breached perimeter (some but not all of the 20 wall segments),
     exactly 3 corner posts, and every piece carrying the pack's
     "damaged" variant texture path.
  1b/1c. Loot selection is seed-stable per placed instance (#948): two
     further INDEPENDENT fresh processes regenerate the same seed from
     scratch — one visiting the ruins in the same order as check 1, one
     in the exact reverse — and every stable ruin instance ends up with
     the same loot multiset it got the first time. Reversing the load
     order must not swap or shift which ruin owns which reward.
  2. The one-time content-spawn flag AND the damaged geometry survive a
     save -> quit -> fresh restart -> load: revisiting the same chunk
     does NOT respawn (counts stay exactly the same, not doubled), each
     instance keeps the exact loot it was first given (#948 — nothing is
     re-rolled across save/load or chunk eviction), the breach pattern
     replays identically, and the pieces still resolve to the damaged
     variant art (the #91 variant round-trip).
  3. An unknown content `id` — an unregistered unit, and a loot table
     rolling an unregistered item — logs a warning and is skipped
     rather than crashing the engine. An unknown content `kind` is NOT
     part of this: since #1708 the vocabulary is closed at the YAML
     boundary, so such a file fails to decode and registers nothing;
     that rejection is pinned by hspec "Location spatial bounds", and
     the fixture here is checked to LOAD so the runtime id paths below
     are actually reached. Also covers the fixed-position `kind: item`
     dispatch path, which #921 left no SHIPPED location exercising: a
     probe-local def spawns one at a declared `position` and it must
     land on exactly that tile.
  4. Location discovery (#780): stamping a ruin's geometry and spawning
     its contents do NOT discover it; a hostile unit standing on it
     doesn't either; a player-faction unit that SEES it does (#1230),
     flipping `world.listPlacedLocations()`'s `discovered`
     field and emitting exactly one `location_discovery` player event;
     re-checking without the unit moving emits no duplicate; the
     discovered state survives save -> quit -> fresh restart -> load
     alongside the geometry/contents from check 2.
  5. Per-unit location knowledge (#915): the unit that can see it
     gains its OWN memory of the location — keyed by the
     (page, instance id) identity, recorded while the sim is PAUSED
     (acquisition mirrors the discovery tick's pause independence) — a
     second player unit standing elsewhere does not, a unit arriving at
     an ALREADY-discovered location still learns it without a second
     lifecycle transition or event, and the memory survives the same
     save -> restart -> load round trip. A save also carries two
     RESOLVING sibling memories plus one naming an instance id no page
     ever allocated: the load still succeeds, the integrity graph logs
     exactly one lua.unit_ai/location_instance diagnostic naming the
     field and page/id, the real onSaveLoaded reconcile drops only that
     entry, and no lifecycle changes. Phase 4 generates two same-seed
     pages so the SAME instance id names a different real location on
     each, and checks the two units' memories stay distinct.

  6. Location naming (#1101): a world with a #1092 language provenance
     names every placed ruin in that language -- a generated native
     name (never the `ldLabel` "Small Ruin") plus a non-empty English
     gloss, both reaching Lua through `world.listPlacedLocations`. The
     SAME terrain seed with NO provenance falls back to `ldLabel` with
     the `gloss` key absent entirely, so the generated case is a real
     outcome rather than a field that is always populated. Both names
     and glosses then survive save -> quit -> fresh process -> load
     byte-exact, and re-generating the identical seed + language in a
     fresh process reproduces them exactly.

Headless skips the GUI data-loading step, so defs are registered by
hand here (items/units/buildings/loot_tables/locations), same as
tools/location_overlay_probe.py does for locations alone.

EVERY file this invocation creates -- its five fixture YAMLs, its
engine log, and the throwaway resource root with the save slots #1620
already moved into it -- lives under ONE directory this process owns,
and goes away again on every handled exit (#1884). Before that the
fixtures and the log were fixed `/tmp` names no run cleaned up, which
two concurrent runs collided on: `tools/run_probes.py --jobs N` and
`tools/probe_flake.py`'s machine-wide port lease both make that
concurrency a supported mode. `--keep-artifacts` retains the directory
instead, and names it, for diagnosing a failure -- which matters more
here than for an ordinary artifact, because the engine log is not only
diagnostics: two checks below ASSERT against it.

Usage:
  python3 tools/location_content_probe.py
  python3 tools/location_content_probe.py --seed 42 --size 64 --port 9190
  python3 tools/location_content_probe.py --keep-artifacts

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
from pathlib import Path
from probelib import (FixtureNotRegistered, capture_request_id, quit_engine,
                      boot, load_fixture_yaml, send, wait_load_published,
                      wait_save_complete, load_ai_stack)
from run_probes import FailureEmitter   # durable failure records (#1982)

REPO = Path(__file__).resolve().parent.parent
#: #1982 — this run's durable failure records, built at import so the
#: offset each carries is measured from the probe's own start.
FAILURE = FailureEmitter("location_content_probe")
#: How a save/load failure message refers to the engine log when the
#: caller supplied no path. Spelled as a constant so the f-strings below
#: need no escaped quote inside their expression, which only Python 3.12
#: onwards accepts (PEP 701).
THIS_RUNS_LOG = "this run's engine log"
#: Prefix of this invocation's throwaway resource root. The random
#: suffix mkdtemp appends to it is also the per-invocation save-slot
#: token, so the root and the slots it holds share one identity.
ROOT_PREFIX = "location_content_probe_"


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

    Every boot belonging to the invocation is handed this root, so the
    fixtures saved and loaded here are never written into, and can never
    be satisfied by, a slot reachable from a normal `cabal run` (#1620
    requirement 5; the tools/chop_probe.py pattern). The CONTENT
    families are SYMLINKS, and shutil.rmtree unlinks a symlink rather
    than descending it, so teardown can never reach the repository's own
    directories.

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


class RunArtifacts:
    """Every file one invocation of this probe creates, under a single
    directory that invocation owns (#1884).

    `base` comes from `tempfile.mkdtemp`, so it is this process's alone
    and disjoint from every other invocation's -- which is what makes
    the logical names inside it (`engine.log`, `bogus.yaml`,
    `quinoa.yaml`, ...) safe to keep fixed. #1620 had already moved the
    SAVE slots into `root`; the fixtures and the log stayed behind as
    fixed `/tmp` names -- `/tmp/location_content_engine.log` and the
    five `/tmp/loc_content_probe_*.yaml` -- each written with a
    truncating `open(..., "w")` (`probelib.boot` opens the log the same
    way) and removed by nothing. Two concurrent runs collided on all
    six: one could overwrite a fixture between another's write and the
    engine-side read of it, and both interleaved into one truncated
    log. That last one is not merely untidy here, because the log is
    ASSERTED against -- the integrity-diagnostic read in phase 2 and
    the warning read in phase 3 -- so a foreign truncation could turn a
    passing phase into a failure or a failure into a pass.

    Nothing this process did not create is ever named, so a file of the
    same name outside the tree -- a developer's own
    `/tmp/loc_content_probe_bogus.yaml` -- is not opened for writing,
    truncated, modified or removed.
    """

    def __init__(self, base: str) -> None:
        self.base = base
        self.root = os.path.join(base, "root")
        self.logs = os.path.join(base, "logs")
        self.fixtures = os.path.join(base, "fixtures")
        #: Every engine `boot_isolated` has LAUNCHED, registered the
        #: statement after `probelib.boot`'s own `Popen` rather than
        #: only once it returns -- which on a hung boot is
        #: `ready_timeout` (three minutes) later. `main`'s guard walks
        #: this before it removes anything, so no engine is still
        #: writing into a tree that is being deleted.
        self.launched: list = []

    def build(self) -> str:
        """Stage this invocation's throwaway resource root (#1620) and
        the two artifact directories beside it, and answer with the root.

        The root itself is still `make_isolated_root`'s -- unchanged, and
        still the builder `tools/portal_ghost_probe.py` imports and
        `tools/test_location_probe_config_isolation.py` pins.
        """
        root = make_isolated_root(self.base)
        os.makedirs(self.logs, exist_ok=True)
        os.makedirs(self.fixtures, exist_ok=True)
        return root

    @property
    def engine_log(self) -> str:
        """The engine's stdout/stderr capture, shared by every boot of
        this invocation and truncated by each of them.

        `probelib.boot` opens it `"w"`, so a boot still starts the log
        empty -- the per-boot isolation phase 2's diagnostic read
        depends on is exactly what it was. What changed is WHOSE log it
        is: a second invocation now truncates its own, not this one's.
        """
        return os.path.join(self.logs, "engine.log")

    def fixture(self, name: str) -> str:
        """One of the probe's own inline YAML fixtures. The path is
        handed to the ENGINE, which has chdir'd into `root`
        (`App.ResourceRoot`), so it stays absolute for the same reason
        it always was."""
        return os.path.join(self.fixtures, f"{name}.yaml")


def _describe_held(path: str) -> str:
    """How the retention summary describes one artifact directory.

    A path that does not EXIST is not the same as one that is empty: a
    run that failed part-way through staging never created it, and
    calling it empty would send the reader looking for a directory that
    is not there.
    """
    if not os.path.exists(path):
        return " (never created -- the run ended before staging reached it)"
    try:
        held = sorted(os.listdir(path))
    except OSError as exc:
        return f" (unreadable: {exc})"
    return f" ({', '.join(held)})" if held else " (empty)"


def abandon_engine(proc) -> None:
    """Make sure an engine this run launched is dead, without talking to
    the port. A no-op on a handle that has already exited.

    Every phase below already shuts its own engine down through
    `quit_engine` in a `finally`. This is the backstop for the windows
    that reach past one: `probelib.boot` hands the process over the
    moment it exists (`on_launch`) and only decides about READY up to
    three minutes later, so an interrupt in that span leaves a live
    engine no `proc = boot_isolated(...)` assignment will ever name; and
    `quit_engine` is itself interruptible -- it sends, waits, then
    hard-kills -- so a Ctrl-C inside it unwinds with the engine possibly
    still running. Either way the process would still be writing into
    the tree `main` is about to delete.

    Deliberately NOT `quit_engine`: that sends `engine.quit()` to the
    PORT, and a boot fails on a busy port precisely because the port
    belongs to somebody else's instance. `kill()` is a single syscall
    and SIGKILL cannot be caught, so once it has landed the process is
    dead whatever happens to the reap that follows.
    """
    if proc.poll() is not None:
        return
    proc.kill()
    try:
        proc.wait(timeout=10)
    except subprocess.TimeoutExpired:
        FAILURE.check(f"the engine this run launched (pid {proc.pid}) did "
                      f"not die when killed")


def release_artifacts(art: RunArtifacts, keep: bool) -> str | None:
    """Retire this invocation's artifact directory -- fixtures, engine
    log and resource root together -- and report a failure to remove it
    rather than swallowing one. Returns None when the run may still
    report its own result, else the message to fail with.

    `--keep-artifacts` is the intentional exception to that removal
    (#1884 requirement 5): the tree is retained and named, and the run
    keeps whatever result its own checks produced. Without the flag the
    removal is mandatory and #1620 requirement 6's reporting is exactly
    as it was, because this delegates to `remove_isolated_root`.
    """
    if keep:
        # Each line names what this run ACTUALLY produced. A run that
        # died before READY holds no fixtures and no save slot, and
        # saying otherwise would send the reader looking for files the
        # failure is the reason they do not have.
        print(f"\nretained this run's artifacts (--keep-artifacts): "
              f"{art.base}")
        for label, path in (("engine log", art.logs),
                            ("fixtures", art.fixtures),
                            ("saves", os.path.join(art.root, "saves"))):
            print(f"  {label:14} {path}{_describe_held(path)}")
        print(f"  {'resource root':14} {art.root}"
              + ("" if os.path.isdir(art.root) else " (never created)"))
        return None
    return remove_isolated_root(art.base)


def boot_isolated(port: int, art: RunArtifacts, **kwargs):
    """The one funnel every boot in this file goes through, so the log
    path and the launched-engine registration are decided once."""
    return boot(port, log=art.engine_log,
                args=["--resource-root", art.root],
                on_launch=art.launched.append, **kwargs)


def save_and_wait(port: int, page: str, slot: str,
                  failures: list[str], log: str | None = None) -> bool:
    """engine.saveWorld, then tie completion to THIS request's own id.

    saveWorld only ACCEPTS synchronously — it returns false on a
    validation failure (the detailed reason goes to the engine log) and
    true once the command is queued, while the encode and disk write run
    afterwards. So a sleep proves nothing, and a fixed slot name means a
    PRIOR generation of the same slot could satisfy the reader
    (#1620). Every reader here starts only after this returns True.

    `log` names the engine log the failure message points at. It stays a
    caller-supplied path -- `tools/portal_ghost_probe.py` passes its own
    -- but no longer defaults to a module-global one, because since
    #1884 the log belongs to the INVOCATION and only a caller holding
    its `RunArtifacts` knows where it is.
    """
    accepted = send(port, f"return engine.saveWorld('{page}', '{slot}')").strip()
    if accepted != "true":
        failures.append(
            f"engine.saveWorld(page '{page}', slot '{slot}') was not accepted "
            f"(returned {accepted!r}); the validation reason is logged in "
            f"{log or THIS_RUNS_LOG}")
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
                  seconds: float = 60.0, log: str | None = None) -> bool:
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
            f"{accepted!r}); the reason is logged in "
            f"{log or THIS_RUNS_LOG}")
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

#: The probe's own inline YAML fixtures, as the exact bytes that reach
#: disk (#1884). They were inline `fh.write(...)` calls at the phases
#: that use them; only WHERE they are written moved, never WHAT they
#: say. Placement and loot draws are order- and content-sensitive, so
#: `tools/test_location_content_probe.py` pins these bodies by digest
#: and pins the registration order of the calls that load them.

#: Phase 3's unknown-content-id fixture. Deliberately full of unknown
#: IDS, but no unknown KIND: #1708 closed that vocabulary at the YAML
#: boundary, so an entry naming one would fail the whole file's decode
#: and leave `bogus_ruin` unregistered.
BOGUS_LOCATION_YAML = (
    "locations:\n"
    "  - id: bogus_ruin\n"
    "    label: Bogus Ruin\n"
    "    type: ruin\n"
    "    builder: room_small\n"
    "    anchor: []\n"
    "    max_count: 0\n"
    "    bounds: { min_x: -2, min_y: -2, max_x: 2, max_y: 2 }\n"
    "    naming: { heads: [KEEP], modifiers: [ASH] }\n"
    "    contents:\n"
    "      - { kind: unit, id: does_not_exist, count: 1 }\n"
    "      - { kind: loot_table, id: bogus_table, rolls: 1 }\n"
)

#: …and the loot table it rolls, whose only entry is an unregistered
#: item id.
BOGUS_LOOT_YAML = (
    "id: bogus_table\n"
    "entries:\n"
    "  - id: item_that_does_not_exist\n"
    "    weight: 1\n"
)

#: The fixed-position `kind: item` content phase 3 asserts to the exact
#: tile. It keeps the `spawnItemContent` dispatch branch
#: (scripts/locations.lua) under test: #921 removed the last SHIPPED use
#: of it, and an untested branch is one edit from silently breaking for
#: the loot-container work that will want it back. `position` is the
#: part with no other coverage — a scattered entry lands anywhere in
#: bounds, so only a fixed one can be asserted to the exact tile.
FIXED_DEF, FIXED_OX, FIXED_OY = "radio", -1, 2

#: A single-entry loot table forces quinoa_sack to spawn through the
#: real content-spawn path (locations.spawnContents -> loot.rollFor ->
#: item.spawnGround) whatever the roll context, rather than depending on
#: whether ruin_common's 2/13-weight entry happens to be the one this
#: instance's draw selects (#800). #948 made that draw seed-stable
#: rather than random, but it is still weight-dependent — which entry a
#: given instance lands on is not something to assert on here.
QUINOA_LOCATION_YAML = (
    "locations:\n"
    "  - id: probe_quinoa_ruin\n"
    "    label: Quinoa Probe Ruin\n"
    "    type: ruin\n"
    "    builder: room_small\n"
    "    anchor: []\n"
    "    max_count: 0\n"
    "    bounds: { min_x: -2, min_y: -2, max_x: 2, max_y: 2 }\n"
    "    naming: { heads: [KEEP], modifiers: [ASH] }\n"
    "    contents:\n"
    "      - { kind: loot_table, id: probe_quinoa_table, rolls: 1 }\n"
    f"      - {{ kind: item, id: {FIXED_DEF}, count: 1, "
    f"position: {{x: {FIXED_OX}, y: {FIXED_OY}}} }}\n"
)

QUINOA_LOOT_YAML = (
    "id: probe_quinoa_table\n"
    "entries:\n"
    "  - id: quinoa_sack\n"
    "    weight: 1\n"
)

#: Phase 4's DENSE location (one per land chunk, like
#: tools/location_overlay_probe.py's DENSE_YAML), which guarantees
#: content at the SYNCHRONOUS centre chunk (0,0).
DENSE_LOCATION_YAML = (
    "locations:\n"
    "  - id: dense_ruin\n"
    "    label: Dense Ruin\n"
    "    type: ruin\n"
    "    builder: room_small\n"
    "    anchor: [waterside]\n"
    "    max_count: 100000\n"
    "    min_spacing: 1\n"
    "    bounds: { min_x: -2, min_y: -2, max_x: 2, max_y: 2 }\n"
    "    naming: { heads: [KEEP], modifiers: [ASH] }\n"
    "    contents:\n"
    "      - { kind: building, id: cargo_hold_S, count: 1, position: {x: 0, y: 0} }\n"
    "      - { kind: unit, id: acolyte, count: 1, faction: hostile, position: {x: 1, y: 1} }\n"
)

#: A location-instance id no page will ever have allocated (#915) —
#: used to stage a memory whose (page, id) cannot resolve after a load.
DANGLING_ID = 99999

#: Ground items one ruin_small spawns: its `ruin_common` loot_table
#: entry's 2 rolls, and nothing else. #921 removed the two fixed-position
#: items that used to make this 4 — the count is now purely the roll
#: count in data/locations/ruin_small.yaml.
GROUND_PER_RUIN = 2


def load_yaml_dir(port: int, directory: str, loader: str) -> None:
    lua = (f"local fs = engine.listFiles('{directory}', '.yaml') or {{}}; "
           f"for _, f in ipairs(fs) do {loader}('{directory}/' .. f) end; "
           f"return #fs")
    send(port, lua, timeout=20.0)


def load_registries(port: int) -> None:
    load_yaml_dir(port, "data/items", "engine.loadItemYaml")
    load_yaml_dir(port, "data/units", "engine.loadUnitYaml")
    load_yaml_dir(port, "data/buildings", "engine.loadBuildingYaml")
    load_yaml_dir(port, "data/loot_tables", "engine.loadLootTableYaml")


def load_defs(port: int) -> None:
    load_registries(port)
    send(port, "engine.loadLocationYaml('data/locations/ruin_small.yaml'); return 'ok'")


#: The language seed the #1101 phase names its world from. An arbitrary
#: but fixed unsigned 64-bit value, deliberately above 2^63-1 so the
#: decimal-string carrier world.init/world.getLanguageProvenance use is
#: exercised over a range a Lua integer could not hold.
NAMED_WORLD_LANG_SEED = "12345678901234567890"


def gen_named_world(port: int, page: str, seed: int, size: int) -> None:
    """A world with a #1092 language provenance, so its placed
    locations get generated names (#1101) instead of ldLabel."""
    send(port, f"world.init('{page}', {seed}, {size}, 3, 'Vashenkoro', "
               f"'Ashen Land', '{NAMED_WORLD_LANG_SEED}'); return 'ok'")
    send(port, "return world.waitForInit(240)", timeout=250)
    send(port, f"world.show('{page}'); return 'ok'")
    send(port, "return world.loadChunksInRegion(-1,-1,1,1)")
    send(port, "return world.waitForChunks(60)", timeout=65)


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


def loc_at(port: int, cx: int, cy: int, page: str, tries: int = 120) -> tuple[int, int] | None:
    """(gx, gy) of the location placed at chunk (cx, cy) on `page`, or
    None. Server-side scan, never ships the full list to Python — needed
    for a DENSE def (one location per land chunk; #90 phase 4), where the
    full list is thousands of entries and JSON round-tripping it is the
    kind of thing tools/location_overlay_probe.py deliberately avoids.

    Polls: world.waitForInit always reads the ACTIVE world's load phase
    (Engine/Scripting/Lua/API/World.hs worldWaitForInitFn), so it cannot
    be used to wait for a HIDDEN page's init to finish — the caller can't
    know when `page`'s gen params (and thus its overlay) become readable
    other than by retrying this query."""
    lua = (f"local t = world.listPlacedLocations('{page}'); "
           f"for _, e in ipairs(t) do if e.cx == {cx} and e.cy == {cy} then "
           f"return e.gx .. ',' .. e.gy end end; return 'none'")
    r = "none"
    for _ in range(tries):
        r = send(port, lua, timeout=20.0).strip('"')
        if r != "none":
            break
        time.sleep(0.5)
    if r == "none" or "," not in r:
        return None
    gx_s, gy_s = r.split(",", 1)
    return int(gx_s), int(gy_s)


def placed_ready(port: int, tries: int = 30) -> list[dict]:
    last: list[dict] = []
    for _ in range(tries):
        last = placed(port)
        if last:
            return last
        time.sleep(0.5)
    return last


def ruins_ready(port: int, page: str, tries: int = 40) -> list[dict]:
    """The `ruin_small` rows on `page`, polled until non-empty (#1101).

    A second world.init in a session already holding a live page cannot
    be waited on with world.waitForInit -- it reads the ACTIVE world's
    phase (worldWaitForInitFn), which is already `done` -- so the page's
    gen params, and with them its overlay, become readable only some
    time after the call returns. Same reason `loc_at` polls.
    """
    last: list[dict] = []
    for _ in range(tries):
        last = [e for e in placed(port, page) if e["id"] == "ruin_small"]
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


def ruin_geometry(port: int, gx: int, gy: int, page: str | None = None) -> tuple[int, int, int]:
    """(floors, walls, posts) of the 5x5 ruin anchored at (gx, gy).
    Counted server-side over the room footprint: 25 floor tiles, the 20
    perimeter wall segments (nw/se run along x0/x1, ne/sw along y0/y1),
    and the 4 corner posts."""
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


def floor_tex(port: int, gx: int, gy: int, page: str | None = None) -> str:
    """Texture path of the floor piece at (gx, gy) — the persisted
    variant identity (#91)."""
    arg = f",'{page}'" if page else ""
    r = send(port, f"local t=structure.getAt({gx},{gy},'floor'{arg}); "
                   f"return t and t.tex or 'none'")
    return r.strip('"')


def unit_count(port: int, def_name: str) -> int:
    r = send(port, "return unit.list()")
    return len(re.findall(re.escape(def_name), r))


def building_count(port: int, def_name: str) -> int:
    r = send(port, "return building.list()")
    return len(re.findall(re.escape(def_name), r))


def ground_items(port: int) -> list[dict]:
    raw = send(port, "return item.listGround()").strip()
    if not raw or raw in ("nil", "null", "{}", "[]"):
        return []
    try:
        data = json.loads(raw)
    except json.JSONDecodeError:
        return []
    return data if isinstance(data, list) else []


def loot_by_instance(port: int, page: str) -> dict[int, list[str]]:
    """Ground-item defName multiset per placed-location INSTANCE (#948),
    attributed by the instance's own absolute bounds (#777).

    Attribution by bounds is unambiguous here: ruin_small declares
    min_spacing 5 chunks (data/locations/ruin_small.yaml), so no two
    ruin footprints can overlap. Multiset, not sequence: ground items
    carry no roll order, and their scatter COORDINATES are still
    math.random-driven by design — this issue pins the selected
    item-definition sequence, which the hspec fixed vectors cover
    directly. Keyed by the stable instance id so the comparison is
    immune to placement/query ORDER."""
    items = ground_items(port)
    out: dict[int, list[str]] = {}
    for e in placed(port, page):
        b = e.get("bounds") or {}
        if not b or "instance_id" not in e:
            continue
        out[e["instance_id"]] = sorted(
            it.get("defName", "?") for it in items
            if b["min_x"] <= it.get("x", 1e9) <= b["max_x"]
            and b["min_y"] <= it.get("y", 1e9) <= b["max_y"])
    return out


def stamp_ruins(port: int, ruins: list[dict], reverse: bool = False) -> None:
    """Load every ruin's chunk and wait for its geometry + contents."""
    order = list(reversed(ruins)) if reverse else list(ruins)
    for e in order:
        load_chunk(port, e["cx"], e["cy"])
    for _ in range(60):
        if all(has_floor(port, e["gx"], e["gy"]) for e in order):
            break
        time.sleep(0.5)
    want = GROUND_PER_RUIN * len(order)
    for _ in range(20):
        if spawn_counts(port)["ground_total"] >= want:
            break
        time.sleep(0.5)


def spawn_counts(port: int) -> dict:
    items = ground_items(port)
    counts: dict[str, int] = {}
    for it in items:
        name = it.get("defName", "?")
        counts[name] = counts.get(name, 0) + 1
    return {
        "acolyte": unit_count(port, "acolyte"),
        "nomad_primitive": unit_count(port, "nomad_primitive"),
        "cargo_hold_S": building_count(port, "cargo_hold_S"),
        "ground_total": len(items),
        "ground_by_name": counts,
    }


def discovered_flags(port: int, page: str) -> dict[tuple[int, int], bool]:
    """(cx, cy) -> discovered, for every placed location on `page` (#780)."""
    return {(e["cx"], e["cy"]): bool(e.get("discovered")) for e in placed(port, page)}


def event_log(port: int) -> list[dict]:
    raw = send(port, "return engine.getEventLog()").strip()
    if not raw or raw in ("nil", "null", "{}", "[]"):
        return []
    try:
        data = json.loads(raw)
    except json.JSONDecodeError:
        return []
    return data if isinstance(data, list) else []


def discovery_events(port: int, label: str) -> list[dict]:
    """Every logged `location_discovery` event naming `label` (#780)."""
    text = f"Discovered: {label}"
    return [e for e in event_log(port)
            if e.get("category") == "location_discovery" and e.get("text") == text]


def loc_instance_at(port: int, cx: int, cy: int, page: str) -> int:
    """The stable instance id (#911) of the location at chunk (cx, cy) on
    `page`, or -1. Scanned SERVER-side like `loc_at`, for the same
    reason: on a dense page the full list is thousands of entries and
    JSON round-tripping it is what this file deliberately avoids."""
    lua = (f"local t = world.listPlacedLocations('{page}'); "
           f"for _, e in ipairs(t) do if e.cx == {cx} and e.cy == {cy} then "
           f"return e.instance_id end end; return -1")
    try:
        return int(float(send(port, lua, timeout=20.0).strip('"')))
    except ValueError:
        return -1


def known_locations(port: int, uid: int) -> set[str]:
    """(#915) The per-unit location memories `uid` holds, as a set of
    "<page>#<instance id>" keys. Read through unitAi.getKnownLocations —
    the public query surface AI candidates use — and flattened to a
    string so an empty result is unambiguous (an empty Lua table would
    serialize identically to an empty object)."""
    lua = (f"local ai = require('scripts.unit_ai'); "
           f"local out = {{}}; "
           f"for _, k in ipairs(ai.getKnownLocations({uid})) do "
           f"out[#out+1] = k.page .. '#' .. tostring(k.id) end; "
           f"return table.concat(out, ',')")
    raw = send(port, lua).strip().strip('"')
    return {p for p in raw.split(",") if p}


# The widest a unit's night-aware sight radius can reach (#1230). A
# unit sees at most perception * awareRangeTiles tiles
# (Unit.LineOfSight.awareRangeTiles = 6.0), and the page-local night
# factor only ever SHRINKS that. No shipped unit carries a perception
# above 2.0, so 12 tiles bounds every sightline this probe can produce;
# the slack below then puts an "ignorant" unit comfortably past it.
#
# This replaces the removed 6-tile discovery halo. Re-deriving it from
# the sight radius rather than rewriting the old constant is the point:
# a unit that must NOT reveal a location has to be outside the RADIUS,
# outside the facing cone, or behind blocking terrain, and a 6-tile box
# no longer describes that boundary at all.
MAX_SIGHT_TILES = 12


def sight_box(e: dict) -> tuple[int, int, int, int]:
    """The region from which a placed location could be revealed: its
    stored bounds (#777) — the footprint Location.Discovery tests sight
    against since #1230 — grown by the widest reachable sight radius, so
    a tile outside this box cannot see any tile of the location."""
    b = e.get("bounds") or {}
    m = MAX_SIGHT_TILES
    return (int(b.get("min_x", e["gx"])) - m, int(b.get("min_y", e["gy"])) - m,
            int(b.get("max_x", e["gx"])) + m, int(b.get("max_y", e["gy"])) + m)


def pick_far_tile(la: list[dict], origin: tuple[int, int],
                  slack: int = 6) -> tuple[int, int] | None:
    """A tile comfortably outside EVERY placed location's sight box —
    where a second unit can stand and stay ignorant (#915)."""
    ox, oy = origin
    for d in range(24, 400, 8):
        for cand in ((ox + d, oy), (ox, oy + d), (ox + d, oy + d),
                     (ox - d, oy), (ox, oy - d)):
            if all(not (x0 - slack <= cand[0] <= x1 + slack
                        and y0 - slack <= cand[1] <= y1 + slack)
                   for x0, y0, x1, y1 in map(sight_box, la)):
                return cand
    return None


def wait_knows(port: int, uid: int, key: str, tries: int = 40) -> bool:
    for _ in range(tries):
        if key in known_locations(port, uid):
            return True
        time.sleep(0.25)
    return False


def spawn_unit(port: int, def_name: str, gx: int, gy: int, faction: str, page: str) -> int:
    """unit.spawn(...) returns the new unit's numeric id, or -1 on failure."""
    r = send(port, f"return unit.spawn('{def_name}', {gx}, {gy}, nil, '{faction}', '{page}')")
    try:
        return int(float(r.strip('"')))
    except ValueError:
        return -1


def registered_item_names(port: int) -> set[str]:
    """The live item registry (item.listDefs()) — #800 replaces the stale
    hardcoded loot_names allowlist with this as the authoritative source,
    so a valid new loot entry (e.g. quinoa_sack, #458) is accepted without
    the probe needing to be updated by hand."""
    raw = send(port, "return item.listDefs()").strip()
    if not raw or raw in ("nil", "null", "{}", "[]"):
        return set()
    try:
        data = json.loads(raw)
    except json.JSONDecodeError:
        return set()
    return {d["name"] for d in data if isinstance(d, dict) and "name" in d}


def unregistered_item_ids(names: set[str], registered: set[str]) -> set[str]:
    """Pure check: which of `names` aren't in the live item registry.
    Kept as a standalone function so it can be exercised directly against
    a synthetic id, independent of whatever a real spawn happens to
    produce (#800)."""
    return set(names) - registered


def main() -> int:
    ap = argparse.ArgumentParser(description=__doc__)
    ap.add_argument("--seed", type=int, default=42)
    ap.add_argument("--size", type=int, default=64)
    ap.add_argument("--port", type=int, default=9190)
    ap.add_argument("--keep-artifacts", action="store_true",
                    help="keep this run's artifact directory (its five "
                         "fixture YAMLs, the engine log, and the isolated "
                         "resource root with its save slots) instead of "
                         "deleting it, and name it in the summary — for "
                         "diagnosing a failure")
    args = ap.parse_args()

    # One artifact directory per invocation, holding the throwaway
    # resource root (#1620 requirement 5 — slot names carry that root's
    # own random token, so no developer-visible save slot is created,
    # mutated or rotated) AND, since #1884, the five fixture YAMLs and
    # the engine log that used to be fixed /tmp names.
    #
    # The guard starts HERE, one statement after that directory exists,
    # because everything between this point and the release below can
    # fail with invocation-owned state already on disk: `build` stages
    # incrementally — the root, three symlinks, a copied `config/`,
    # `saves/`, then the log and fixture directories — so a permission,
    # source or disk-space failure part-way through leaves a partial
    # tree that nothing outside this guard would remove.
    art = RunArtifacts(tempfile.mkdtemp(prefix=ROOT_PREFIX))
    # The WHOLE random suffix, not the text after the last underscore:
    # mkdtemp's alphabet includes '_', so splitting on it can throw most
    # of the entropy away (and can leave nothing at all).
    token = os.path.basename(art.base)[len(ROOT_PREFIX):]
    rc = 1
    try:
        art.build()
        rc = run(args, art, token)
    except SystemExit as exc:
        # How `probelib.boot` ends a run whose engine died before READY
        # or never printed it. Reported rather than allowed to exit, so
        # the release below stays on the path and the summary names the
        # abort. A `finally` alone would still release, but the run's
        # own result would be lost with the propagating exit.
        # Durable (#1982), and the one path where it matters most: `run`
        # never reached its own report, so this record is the ONLY thing
        # naming why the run ended. The engine log is read here, before
        # the release below removes the tree holding it.
        FAILURE.check(f"the run aborted before finishing: {exc}")
        FAILURE.context_log(art.engine_log)
        FAILURE.context("artifact root", art.base)
    finally:
        # Every engine this run LAUNCHED must be dead before its files
        # go: each phase already quits its own in a `finally`, but that
        # leaves two windows — an interrupt inside `probelib.boot`'s
        # three-minute READY wait, and one inside `quit_engine` itself,
        # which sends, waits, then hard-kills. `abandon_engine` is a
        # no-op on a handle that has already exited, so the orderly
        # shutdowns above are untouched.
        for proc in art.launched:
            abandon_engine(proc)
        # Reported, never swallowed, and reported even when `run` is
        # leaving by an exception — a root that survived is exactly the
        # artifact #1620 requirement 6 forbids, and since #1884 the
        # fixtures and log inside it are covered by the same removal.
        leftover = release_artifacts(art, args.keep_artifacts)
        if leftover:
            FAILURE.check(leftover)
        elif rc != 0 and not args.keep_artifacts:
            # A failure's primary evidence is the engine log, and some
            # paths above have already named it — `probelib.boot`'s
            # abort message quotes the path verbatim. It has just been
            # deleted with the rest of the tree, so say so here rather
            # than leave the operator chasing a path that is gone.
            print("  (this run's engine log and fixture YAMLs went with its "
                  "artifact directory — re-run with --keep-artifacts to keep "
                  "them)")
    return 1 if leftover else rc


def run(args, art: RunArtifacts, token: str) -> int:
    slot_content = f"loc_content_probe_{token}"
    slot_naming = f"loc_naming_probe_{token}"

    failures: list[str] = []
    saved_content = False
    saved_naming = False
    ruins: list[dict] = []
    counts1: dict = {}
    geoms1: dict = {}
    loot1: dict[int, list[str]] = {}
    # #915: the "<page>#<instance id>" memory key phase 1 proves a unit
    # learned, and the units that hold it — re-checked after the load.
    r0mem_key: str = ""
    mem_uids: tuple[int, ...] = ()
    # …plus the unit phase 1 stages the dangling-memory case on, and the
    # two resolving siblings that must survive alongside its removal.
    dangling_uid: int = -1
    sibling_keys: tuple[str, ...] = ()

    # ---- Phase 1: content spawns when a ruin's chunk loads. ----
    proc = boot_isolated(args.port, art)
    try:
        load_defs(args.port)
        gen_world(args.port, "wa", args.seed, args.size)
        la = placed_ready(args.port)
        ruins = [e for e in la if e["id"] == "ruin_small"]
        print(f"world (seed {args.seed}): {len(ruins)} ruin_small placed")
        if not ruins:
            failures.append("no ruin_small placed — cannot test content spawning")
        else:
            for e in ruins:
                load_chunk(args.port, e["cx"], e["cy"])
            n = 0
            for _ in range(60):
                n = sum(1 for e in ruins if has_floor(args.port, e["gx"], e["gy"]))
                if n == len(ruins):
                    break
                time.sleep(0.5)
            if n != len(ruins):
                failures.append(f"only {n}/{len(ruins)} ruin(s) stamped")

            # Content spawning has its own settle time — poll briefly
            # for the expected ground-item count.
            # Each ruin (#91, #921, #916): 2 loot-table ground items and
            # its one persisted uniform 0..3 nomad roll; no fixed items or
            # buildings.
            want_ground = GROUND_PER_RUIN * len(ruins)
            want_nomads = sum(int((e.get("encounter") or {}).get(
                "rolled_count", 0)) for e in ruins)
            counts1 = {}
            for _ in range(20):
                counts1 = spawn_counts(args.port)
                current = {int(e["instance_id"]): e for e in placed(args.port, "wa")}
                rosters_ready = all(
                    bool((current.get(int(e["instance_id"]), {}).get("encounter")
                          or {}).get("roster_complete")) for e in ruins)
                if (counts1["ground_total"] >= want_ground
                        and counts1["nomad_primitive"] >= want_nomads
                        and rosters_ready):
                    break
                time.sleep(0.5)
            print(f"  spawned: {counts1}")

            if counts1["ground_total"] == want_ground:
                print(f"PASS: {want_ground} ground item(s) spawned "
                      f"({GROUND_PER_RUIN} loot_table roll(s) per ruin, "
                      f"no guaranteed item)")
            else:
                failures.append(
                    f"expected {want_ground} ground item(s), got "
                    f"{counts1['ground_total']} ({counts1['ground_by_name']})")

            current = {int(e["instance_id"]): e for e in placed(args.port, "wa")}
            roster_errors = []
            for ruin in ruins:
                iid = int(ruin["instance_id"])
                encounter = (current.get(iid, {}).get("encounter") or {})
                rolled = int(encounter.get("rolled_count", -1))
                occupants = encounter.get("occupants") or []
                homes = {(o.get("home_x"), o.get("home_y"))
                         for o in occupants}
                bounds = current.get(iid, {}).get("bounds") or {}
                homes_in_bounds = all(
                    bounds.get("min_x") <= o.get("home_x") <= bounds.get("max_x")
                    and bounds.get("min_y") <= o.get("home_y") <= bounds.get("max_y")
                    for o in occupants)
                if (not encounter.get("roster_complete")
                        or len(occupants) != rolled
                        or len(homes) != rolled
                        or not homes_in_bounds):
                    roster_errors.append((iid, rolled, len(occupants),
                                          len(homes), homes_in_bounds,
                                          encounter.get("roster_complete")))
            if (counts1["acolyte"] == 0
                    and counts1["nomad_primitive"] == want_nomads
                    and counts1["cargo_hold_S"] == 0
                    and not roster_errors):
                print(f"PASS: persisted encounter rolls spawned exactly "
                      f"{want_nomads} nomad(s), with complete per-ruin rosters "
                      f"on distinct in-bounds home tiles and no unrelated "
                      f"units/buildings")
            else:
                failures.append(
                    f"ruin_small encounter mismatch: expected {want_nomads} nomads "
                    f"and complete rosters, got counts={counts1}, "
                    f"roster_errors={roster_errors}")

            action_policy = send(
                args.port,
                "local A=require('scripts.unit_ai_actions'); return "
                "tostring(A.has('nomad_primitive','ruin_engage')) .. ',' .. "
                "tostring(A.has('nomad_primitive','engage')) .. ',' .. "
                "tostring(A.has('nomad_primitive','attack_target'))")
            if action_policy.strip('"') == "true,false,true":
                print("PASS: nomads acquire targets only through ruin_engage "
                      "while retaining universal attack execution")
            else:
                failures.append(
                    "nomad action inventory bypasses encounter acquisition: "
                    f"{action_policy!r}")

            # #921: the ruin guarantees NOTHING specific. `radio` and
            # `canteen_steel_2l` (spawn-only starting equipment) were the
            # two entries removed, and they are absent from ruin_common
            # too — so no ruin content on this page may be either. This
            # is the direct inverse of the assertion that used to REQUIRE
            # one of each per ruin; it fails if they are reinstated as
            # fixed entries or quietly added to the loot table.
            spawn_only = {d: counts1["ground_by_name"][d]
                          for d in ("radio", "canteen_steel_2l")
                          if counts1["ground_by_name"].get(d)}
            if not spawn_only:
                print("PASS: no spawn-only equipment (radio, canteen_steel_2l) "
                      "in ruin content — nothing is guaranteed")
            else:
                failures.append(
                    f"spawn-only equipment appeared in ruin content: {spawn_only}")

            # #948 baseline: which loot each STABLE ruin instance owns.
            # Captured before the synthetic discovery units below (they
            # are units, not ground items) so phases 1b/1c compare a
            # pure content-spawn result.
            loot1 = loot_by_instance(args.port, "wa")
            print(f"  loot by instance: {loot1}")
            if len(loot1) == len(ruins) and all(loot1.values()):
                print(f"PASS: every ruin instance owns an attributable "
                      f"loot multiset ({len(loot1)} instance(s))")
            else:
                failures.append(
                    f"could not attribute loot to every ruin instance: {loot1}")

            registered = registered_item_names(args.port)
            unexpected = unregistered_item_ids(set(counts1["ground_by_name"]), registered)
            if not unexpected:
                print("PASS: all spawned ground items resolve to registered "
                      "item definitions (item.listDefs(), every loot-table "
                      "roll)")
            else:
                failures.append(
                    f"unexpected ground item id(s) not in the item registry: {unexpected}")

            # #91 geometry: a ruin is a BREACHED room — all 25 floors,
            # some but not all of the 20 perimeter wall segments, and
            # exactly 3 of the 4 corner posts.
            geoms1 = {}
            for e in ruins:
                f, w, p = ruin_geometry(args.port, e["gx"], e["gy"])
                geoms1[(e["gx"], e["gy"])] = (f, w, p)
                if f == 25 and 1 <= w <= 18 and p == 3:
                    print(f"PASS: ruin at ({e['gx']},{e['gy']}) is breached "
                          f"(floors {f}/25, walls {w}/20, posts {p}/4)")
                else:
                    failures.append(
                        f"ruin at ({e['gx']},{e['gy']}) geometry wrong: "
                        f"floors {f}/25 (want 25), walls {w}/20 (want 1..18), "
                        f"posts {p}/4 (want 3)")

            # #91 variant: the pieces persist the damaged texture path.
            tex = floor_tex(args.port, ruins[0]["gx"], ruins[0]["gy"])
            if "/damaged/" in tex:
                print(f"PASS: ruin floor carries the damaged variant art ({tex})")
            else:
                failures.append(f"ruin floor texture is not the damaged variant: {tex}")

            # ---- Discovery (#780): stamping + content-spawning above did
            #      NOT discover the ruin; a hostile unit standing on it
            #      doesn't either; a player-faction unit that SEES it
            #      does (#1230 — standing on the anchor is the strongest
            #      case, since a unit's own tile is always in its visible
            #      set), exactly once, flipping
            #      world.listPlacedLocations()'s `discovered` field. ----
            ruin0 = ruins[0]
            r0key = (ruin0["cx"], ruin0["cy"])
            ruin_label = "Small Ruin"  # data/locations/ruin_small.yaml label

            disc0 = discovered_flags(args.port, "wa")
            if disc0.get(r0key) is False:
                print("PASS: stamping + content-spawning did not discover the ruin")
            else:
                failures.append(
                    f"expected discovered:false after stamping, got {disc0.get(r0key)!r}")

            hostile_uid = spawn_unit(args.port, "acolyte", ruin0["gx"], ruin0["gy"],
                                      "hostile", "wa")
            time.sleep(0.5)
            disc_hostile = discovered_flags(args.port, "wa")
            if hostile_uid >= 0 and disc_hostile.get(r0key) is False:
                print("PASS: a hostile unit standing on the ruin did not discover it")
            else:
                failures.append(
                    f"hostile presence discovery check failed: uid={hostile_uid} "
                    f"discovered={disc_hostile.get(r0key)!r}")

            player_uid = spawn_unit(args.port, "acolyte", ruin0["gx"], ruin0["gy"],
                                     "player", "wa")
            discovered_ok = False
            for _ in range(20):
                if discovered_flags(args.port, "wa").get(r0key):
                    discovered_ok = True
                    break
                time.sleep(0.25)
            if player_uid >= 0 and discovered_ok:
                print(f"PASS: a player-faction unit ({player_uid}) that can see "
                      f"the ruin flips world.listPlacedLocations() to discovered:true")
            else:
                failures.append(
                    f"player presence did not discover the ruin: uid={player_uid}")

            evs = discovery_events(args.port, ruin_label)
            if len(evs) == 1 and evs[0].get("uid") == player_uid and evs[0].get("page") == "wa":
                print(f"PASS: exactly one location_discovery event, attributed to "
                      f"unit {player_uid} on page 'wa'")
            else:
                failures.append(
                    f"expected exactly one attributed discovery event, got {evs}")

            # Leaving (teleport away, well out of sight of it) and
            # returning must not emit a second event.
            send(args.port,
                 f"unit.setPos({player_uid}, "
                 f"{ruin0['gx'] + MAX_SIGHT_TILES + 8}, {ruin0['gy']}); return 'ok'")
            time.sleep(0.5)
            send(args.port,
                 f"unit.setPos({player_uid}, {ruin0['gx']}, {ruin0['gy']}); return 'ok'")
            time.sleep(0.5)
            evs_again = discovery_events(args.port, ruin_label)
            if len(evs_again) == 1:
                print("PASS: leaving and returning emits no duplicate discovery event")
            else:
                failures.append(
                    f"expected still exactly one event after leave+return, got {evs_again}")

            # ---- Per-unit location knowledge (#915): the EXPERIENTIAL
            #      layer beside the player-wide CARTOGRAPHIC state above.
            #      The unit AI stack owns that memory, so load it here —
            #      after every check above, and with the sim PAUSED so
            #      the AI's own decisions (wander, forage, water-seeking)
            #      can never move a unit or pick up one of the ground
            #      items phase 2 re-counts. Pausing is not a workaround
            #      here, it is part of the contract under test: awareness
            #      is ingested BEFORE unitAi.update's pause guard,
            #      mirroring World.Thread.Discovery's own pause
            #      independence, so a paused session still learns. ----
            send(args.port, "engine.setPaused(true); return 'ok'")
            load_ai_stack(args.port)
            r0inst = next((e for e in placed(args.port, "wa")
                           if (e["cx"], e["cy"]) == r0key), None)
            far = pick_far_tile(la, (ruin0["gx"], ruin0["gy"]))
            if r0inst is None or far is None:
                failures.append(
                    f"#915 setup failed: instance={r0inst!r} far_tile={far!r}")
            else:
                r0mem = f"wa#{r0inst['instance_id']}"
                if wait_knows(args.port, player_uid, r0mem):
                    print(f"PASS: the unit that can see the ruin gained its "
                          f"own memory of it ({r0mem}) — while PAUSED")
                else:
                    failures.append(
                        f"unit {player_uid} that can see the ruin never learned "
                        f"{r0mem}: {known_locations(args.port, player_uid)}")

                load_chunk(args.port, far[0] // 16, far[1] // 16)
                far_uid = spawn_unit(args.port, "acolyte", far[0], far[1],
                                     "player", "wa")
                time.sleep(1.5)
                if far_uid >= 0 and r0mem not in known_locations(args.port, far_uid):
                    print(f"PASS: a second player unit ({far_uid}) elsewhere did "
                          f"NOT learn the ruin — knowledge is not shared for free")
                else:
                    failures.append(
                        f"remote unit {far_uid} learned {r0mem} without going "
                        f"there: {known_locations(args.port, far_uid)}")

                # The player-wide layer is untouched by any of this: still
                # discovered, still exactly one event.
                evs_915 = discovery_events(args.port, ruin_label)
                if discovered_flags(args.port, "wa").get(r0key) is True \
                        and len(evs_915) == 1:
                    print("PASS: per-unit memory changed neither the "
                          "discovered lifecycle nor the event count")
                else:
                    failures.append(
                        f"#915 disturbed the player-wide layer: "
                        f"discovered={discovered_flags(args.port, 'wa').get(r0key)!r} "
                        f"events={evs_915}")

                # …and a unit arriving at an ALREADY-discovered location
                # still learns it: acquisition is not gated on the
                # one-time lifecycle promotion or its event.
                send(args.port, f"unit.setPos({far_uid}, {ruin0['gx']}, "
                                f"{ruin0['gy']}); return 'ok'")
                if wait_knows(args.port, far_uid, r0mem):
                    print(f"PASS: unit {far_uid} arriving at an already-"
                          f"discovered ruin still learned it")
                else:
                    failures.append(
                        f"unit {far_uid} seeing an already-discovered location "
                        f"never learned {r0mem}: "
                        f"{known_locations(args.port, far_uid)}")
                evs_late = discovery_events(args.port, ruin_label)
                if len(evs_late) == 1:
                    print("PASS: that later arrival emitted no second "
                          "location_discovery event")
                else:
                    failures.append(
                        f"a later arrival re-emitted discovery event(s): {evs_late}")
                mem_uids = (player_uid, far_uid)
                r0mem_key = r0mem

                # Stage the dangling-memory scenario for phase 2: the
                # discoverer walks to a SECOND ruin (so it holds two
                # genuinely-learned, resolving memories), then gets one
                # more naming an instance id that does not exist. A
                # never-allocated id cannot be produced by walking
                # anywhere, so it is injected through the module's own
                # public helper — the same call the ingest path makes.
                ruin1 = next((e for e in ruins
                              if (e["cx"], e["cy"]) != r0key), None)
                r1inst = next((e for e in placed(args.port, "wa")
                               if (e["cx"], e["cy"])
                               == (ruin1["cx"], ruin1["cy"])), None) \
                    if ruin1 else None
                if r1inst is None:
                    failures.append(
                        "#915: need a SECOND ruin to stage two resolving "
                        "sibling memories")
                else:
                    send(args.port, f"unit.setPos({player_uid}, "
                                    f"{ruin1['gx']}, {ruin1['gy']}); return 'ok'")
                    r1mem = f"wa#{r1inst['instance_id']}"
                    if not wait_knows(args.port, player_uid, r1mem):
                        failures.append(
                            f"unit {player_uid} never learned the second ruin "
                            f"{r1mem}: {known_locations(args.port, player_uid)}")
                    send(args.port,
                         f"local L = require('scripts.unit_ai_locations'); "
                         f"local ai = require('scripts.unit_ai'); "
                         f"L.addKnownLocation(ai.getState({player_uid}), 'wa', "
                         f"{DANGLING_ID}, {ruin0['gx']}, {ruin0['gy']}); "
                         f"return 'ok'")
                    staged = known_locations(args.port, player_uid)
                    want = {r0mem, r1mem, f"wa#{DANGLING_ID}"}
                    if want <= staged:
                        print(f"PASS: staged two resolving memories plus one "
                              f"naming a nonexistent instance ({sorted(want)})")
                        dangling_uid = player_uid
                        sibling_keys = (r0mem, r1mem)
                    else:
                        failures.append(
                            f"#915 could not stage the dangling-memory case: "
                            f"want {sorted(want)}, got {sorted(staged)}")

            # Deliberately still PAUSED through the save below: a load
            # comes up paused anyway (#763), so this keeps phase 1 and
            # phase 2 in the same sim state, and keeps the AI from moving
            # units or picking up the ground items phase 2 re-counts.

            # The synthetic units above are now part of 'wa' — refresh
            # counts1 so phase 2's "reload does not respawn contents"
            # comparison accounts for them too (they persist like any
            # other unit, unrelated to the ruin's one-time content flag).
            counts1 = spawn_counts(args.port)

            # Phase 2 reads this fixture from a FRESH process, so the
            # save must be COMPLETE — not merely accepted — before that
            # process boots (#1620).
            saved_content = save_and_wait(args.port, "wa", slot_content,
                                          failures, log=art.engine_log)
    finally:
        quit_engine(args.port, proc)

    # ---- Phase 1b/1c (#948): loot selection is seed-stable per placed
    #      instance. Two more INDEPENDENT fresh processes generate the
    #      same seed from scratch — one visiting the ruins in the same
    #      order as phase 1, one in the exact reverse — and each ruin
    #      instance must end up with the same loot multiset it got in
    #      phase 1. Before this issue the rolls came off the shared,
    #      entropy-seeded stat RNG, so both runs would disagree with
    #      phase 1 and the reversed run would additionally SWAP which
    #      ruin got which reward. ----
    if ruins and loot1 and not failures:
        for label, reverse in (("same order", False), ("reversed order", True)):
            proc = boot_isolated(args.port, art)
            try:
                load_defs(args.port)
                gen_world(args.port, "wa", args.seed, args.size)
                again = [e for e in placed_ready(args.port) if e["id"] == "ruin_small"]
                if len(again) != len(ruins):
                    failures.append(
                        f"#948 ({label}): fresh process placed {len(again)} "
                        f"ruin(s), phase 1 placed {len(ruins)}")
                    continue
                stamp_ruins(args.port, again, reverse=reverse)
                loot_n = loot_by_instance(args.port, "wa")
                if loot_n == loot1:
                    print(f"PASS: #948 fresh process, same seed, {label} — every "
                          f"ruin instance owns the same loot ({loot_n})")
                else:
                    failures.append(
                        f"#948 ({label}): per-instance loot differs from phase 1: "
                        f"phase1={loot1} now={loot_n}")
            finally:
                quit_engine(args.port, proc)

    # ---- Phase 2: save -> quit -> fresh restart -> load -> revisit does
    #      NOT respawn (one-time flag persisted, independent of the
    #      structure.hasAt geometry check). ----
    if ruins and saved_content and not failures:
        proc = boot_isolated(args.port, art)
        try:
            load_defs(args.port)
            # #915: the unit AI stack must be loaded BEFORE the load, so
            # the (required) lua.unit_ai component has a registered
            # reader — and so onSaveLoaded's reconcile, which scrubs a
            # location memory whose instance is absent from the restored
            # session, actually runs.
            load_ai_stack(args.port)
            # Issue #763: engine.loadSave only ACCEPTS synchronously -- the
            # saved page ("wa", its own id verbatim -- no more main_world
            # remap) doesn't exist live until the transaction publishes.
            if not load_and_wait(args.port, slot_content, failures,
                                 log=art.engine_log):
                raise _PhaseAborted
            send(args.port, "world.show('wa'); return 'ok'")
            time.sleep(1.0)
            for e in ruins:
                load_chunk(args.port, e["cx"], e["cy"])
            # No settle-time poll needed here: a respawn would be immediate
            # and permanent, unlike the initial spawn's queue latency.
            time.sleep(2.0)
            counts2 = spawn_counts(args.port)
            print(f"  after reload: {counts2}")
            if counts2 == counts1:
                print("PASS: reload does not respawn contents (counts unchanged)")
            else:
                failures.append(
                    f"contents respawned on reload: before={counts1} after={counts2}")

            # #948 + #90: the one-time flag means nothing is re-rolled,
            # so each instance keeps the EXACT loot it was first given —
            # through save -> quit -> fresh process -> load -> chunk
            # reload. (The chunks above were evicted with the process and
            # re-loaded after the transaction published.)
            loot2 = loot_by_instance(args.port, "wa")
            if loot2 == loot1:
                print("PASS: per-instance loot survived save -> quit -> restart "
                      "-> load -> chunk reload unchanged (never re-rolled)")
            else:
                failures.append(
                    f"per-instance loot changed across save/load: "
                    f"before={loot1} after={loot2}")

            # #780: discovered state survives save -> quit -> restart ->
            # load; the event itself does NOT (player events are
            # per-session, never saved), so a fresh process reloading an
            # already-discovered location must emit zero events for it.
            disc_reload = discovered_flags(args.port, "wa")
            if disc_reload.get(r0key) is True:
                print("PASS: discovered state survived save -> quit -> restart -> load")
            else:
                failures.append(
                    f"discovered state lost on reload: {disc_reload.get(r0key)!r}")
            evs_reload = discovery_events(args.port, ruin_label)
            if not evs_reload:
                print("PASS: reloading an already-discovered location re-emits no event")
            else:
                failures.append(
                    f"reload incorrectly re-emitted discovery event(s): {evs_reload}")

            # #91: the damaged geometry replays identically from the edit
            # log (same breach pattern — the builder did NOT re-run and
            # re-roll), and the pieces still resolve to the damaged
            # variant art (texture identity rides the structure palette).
            for e in ruins:
                g2 = ruin_geometry(args.port, e["gx"], e["gy"])
                g1 = geoms1.get((e["gx"], e["gy"]))
                if g2 == g1:
                    print(f"PASS: ruin at ({e['gx']},{e['gy']}) replayed its "
                          f"breach pattern exactly (floors/walls/posts {g2})")
                else:
                    failures.append(
                        f"ruin at ({e['gx']},{e['gy']}) changed shape on "
                        f"reload: before={g1} after={g2}")
            tex = floor_tex(args.port, ruins[0]["gx"], ruins[0]["gy"])
            if "/damaged/" in tex:
                print(f"PASS: damaged variant survived save/load ({tex})")
            else:
                failures.append(
                    f"ruin floor texture lost the damaged variant on reload: {tex}")

            # #915: per-unit location memory rides the lua.unit_ai
            # component (now v4) through the same round trip, and its
            # (page, instance id) reference still resolves — so the
            # reconcile pass keeps it rather than scrubbing it.
            if r0mem_key and mem_uids:
                still = {uid: known_locations(args.port, uid) for uid in mem_uids}
                if all(r0mem_key in ks for ks in still.values()):
                    print(f"PASS: per-unit location memory ({r0mem_key}) survived "
                          f"save -> quit -> restart -> load for units {mem_uids}")
                else:
                    failures.append(
                        f"per-unit location memory lost on reload: {still}")
                # The engine-side integrity graph must report EXACTLY the
                # one memory phase 1 made unresolvable, and no other: a
                # VALID memory is only ever resolvable if its page
                # survives every hop from the references() hook to
                # World.Save.Integrity (the save_modules flatteners
                # rebuild each edge field by field, and an id alone
                # resolves against nothing for a per-page kind). The
                # log is this INVOCATION's (#1884) and probelib.boot
                # truncates it per boot, so this names only this load —
                # and no concurrent run can interleave into it.
                try:
                    with open(art.engine_log, encoding="utf-8",
                              errors="replace") as fh:
                        diags = [ln.strip() for ln in fh
                                 if "integrity diagnostic" in ln
                                 and "location_instance" in ln]
                except OSError as e:
                    diags = [f"could not read {art.engine_log}: {e}"]
                want_bits = ("lua.unit_ai", f"page=wa,id={DANGLING_ID}",
                             "knownLocations", "location_instance")
                if len(diags) == 1 and all(b in diags[0] for b in want_bits):
                    print(f"PASS: exactly one location_instance diagnostic, "
                          f"naming lua.unit_ai + the knownLocations field + "
                          f"page=wa,id={DANGLING_ID} — every VALID memory "
                          f"resolved")
                else:
                    failures.append(
                        f"expected exactly one dangling diagnostic naming "
                        f"{want_bits}, got {diags}")

                # …and the load SUCCEEDED anyway (already asserted via
                # wait_load_published above), with the real
                # apply/onSaveLoaded reconcile dropping ONLY the
                # unresolvable entry — its resolving siblings intact.
                if dangling_uid >= 0:
                    after = known_locations(args.port, dangling_uid)
                    if f"wa#{DANGLING_ID}" not in after \
                            and all(k in after for k in sibling_keys):
                        print(f"PASS: onSaveLoaded dropped ONLY the "
                              f"unresolvable memory; unit {dangling_uid} kept "
                              f"{sorted(sibling_keys)}")
                    else:
                        failures.append(
                            f"dangling-memory scrub wrong for unit "
                            f"{dangling_uid}: kept {sorted(after)}, expected "
                            f"{sorted(sibling_keys)} without wa#{DANGLING_ID}")
                    # Dropping a memory is a per-unit act: it must not
                    # touch the player-wide layer either of its siblings
                    # names.
                    lifecycles = discovered_flags(args.port, "wa")
                    sib_keys = {tuple(int(n) for n in k.split("#")[1:])
                                for k in sibling_keys}
                    undiscovered = [e for e in placed(args.port, "wa")
                                    if (e["instance_id"],) in sib_keys
                                    and not e.get("discovered")]
                    if not undiscovered and any(lifecycles.values()):
                        print("PASS: scrubbing a memory left every remembered "
                              "location's player-wide lifecycle untouched")
                    else:
                        failures.append(
                            f"lifecycle changed while scrubbing a memory: "
                            f"{undiscovered}")
                else:
                    failures.append(
                        "phase 2 could not re-check the dangling-memory case: "
                        "phase 1 never staged one")
            else:
                failures.append(
                    "phase 2 could not re-check per-unit location memory: "
                    "phase 1 never established one")
        except _PhaseAborted:
            pass
        finally:
            quit_engine(args.port, proc)
    elif not ruins:
        failures.append("phase 2 skipped: no ruins from phase 1")

    # ---- Phase 3: an unknown content id logs a warning and is skipped,
    #      not a crash. Also covers a loot_table rolling an item id that
    #      isn't registered. An unknown content KIND is deliberately
    #      absent from this fixture: #1708 closed that vocabulary at the
    #      YAML boundary, so an entry naming one would fail the whole
    #      file's decode and leave bogus_ruin unregistered, taking the
    #      unknown-ID checks below down with it. ----
    bogus_yaml = art.fixture("bogus")
    with open(bogus_yaml, "w") as fh:
        fh.write(BOGUS_LOCATION_YAML)
    bogus_loot_yaml = art.fixture("bogus_loot")
    with open(bogus_loot_yaml, "w") as fh:
        fh.write(BOGUS_LOOT_YAML)
    # Why this fixture's single-entry loot table and fixed-position item
    # are what they are: see QUINOA_LOCATION_YAML above.
    quinoa_yaml = art.fixture("quinoa")
    with open(quinoa_yaml, "w") as fh:
        fh.write(QUINOA_LOCATION_YAML)
    quinoa_loot_yaml = art.fixture("quinoa_loot")
    with open(quinoa_loot_yaml, "w") as fh:
        fh.write(QUINOA_LOOT_YAML)
    proc = boot_isolated(args.port, art)
    try:
        load_defs(args.port)
        # These four fixtures are DELIBERATELY full of unknown IDS, but the
        # files themselves must still register: phase 3 is about what
        # spawnContents does with an unresolvable content id, which it can
        # only reach once the location and loot-table defs exist. That is
        # exactly why no entry here names a bogus KIND — since #1708 the
        # kind vocabulary is closed at load, so one would make
        # load_fixture_yaml's zero-count rejection fire here instead.
        load_fixture_yaml(args.port, "engine.loadLocationYaml", bogus_yaml)
        load_fixture_yaml(args.port, "engine.loadLootTableYaml", bogus_loot_yaml)
        load_fixture_yaml(args.port, "engine.loadLocationYaml", quinoa_yaml)
        load_fixture_yaml(args.port, "engine.loadLootTableYaml", quinoa_loot_yaml)
        gen_world(args.port, "wc", args.seed, args.size)
        # Stamp directly (bogus_ruin has max_count 0, so it never places via
        # the overlay) — content-spawning is the concern here, not overlay
        # placement. spawnContents dispatches to unit/kind lookups directly.
        r = send(args.port,
                  "local locations = require('scripts.locations'); "
                  "locations.spawnContents('bogus_ruin', 40, 40, 'wc'); "
                  "return 'ok'")
        alive = send(args.port, "return engine.getFPS() ~= nil and 'alive' or 'dead'")
        if r.strip('"') == "ok" and "alive" in alive:
            print("PASS: unknown unit id + unknown loot roll did not crash "
                  "the engine")
        else:
            failures.append(f"spawnContents with bogus content misbehaved: {r!r} / {alive!r}")
        log_text = open(art.engine_log, errors="replace").read()
        if ("unknown unit content" in log_text
                and "rolled unknown item id" in log_text):
            print("PASS: the unknown unit id AND the "
                  "loot-table-rolled-unknown-item-id both logged a warning")
        else:
            failures.append(
                "expected warnings for unknown unit id AND unknown loot "
                f"roll not both found in {art.engine_log}")

        # #800: the registry-based validation replacing the old hardcoded
        # loot_names allowlist. First, force quinoa_sack through the real
        # content-spawn path via the single-entry loot table above.
        # world.hasSpawnedLocationContents/markLocationContentsSpawned track
        # a one-time flag per CHUNK (chunkSize=16 tiles), not per exact tile
        # — this anchor must land in a different chunk than bogus_ruin's
        # (40,40) (chunk 2,2), or it would see that chunk already marked
        # spawned and silently no-op.
        send(args.port,
             "local locations = require('scripts.locations'); "
             "locations.spawnContents('probe_quinoa_ruin', 400, 400, 'wc'); "
             "return 'ok'")
        registered = registered_item_names(args.port)
        counts3 = spawn_counts(args.port)

        # The fixed-position `kind: item` branch: exactly one instance,
        # on the anchor + declared offset tile and no other. Checked by
        # coordinate, so a scatter regression (ignoring `position`) fails
        # here even though the item count would still be right.
        fixed_at = [g for g in ground_items(args.port)
                    if g.get("defName") == FIXED_DEF]
        want_xy = (400 + FIXED_OX, 400 + FIXED_OY)
        got_xy = [(round(g["x"]), round(g["y"])) for g in fixed_at]
        if got_xy == [want_xy]:
            print(f"PASS: the fixed-position 'kind: item' entry spawned one "
                  f"{FIXED_DEF} at exactly {want_xy} (anchor + declared "
                  f"offset), the branch #921 left no shipped location using")
        else:
            failures.append(
                f"fixed-position item content wrong: expected one {FIXED_DEF} "
                f"at {want_xy}, got {got_xy}")

        got_quinoa = counts3["ground_by_name"].get("quinoa_sack", 0)
        if got_quinoa >= 1:
            print(f"PASS: a forced single-entry loot table deterministically "
                  f"spawned quinoa_sack ({got_quinoa}), independent of "
                  f"ruin_common's 2/13-weight entry")
        else:
            failures.append(
                f"probe_quinoa_ruin's loot table did not spawn quinoa_sack: {counts3}")
        accepted = unregistered_item_ids(set(counts3["ground_by_name"]), registered)
        if not accepted:
            print("PASS: the registry check accepts the deterministically "
                  "forced quinoa_sack (data/items/quinoa_sack.yaml is a "
                  "registered def)")
        else:
            failures.append(
                f"registry check rejected valid spawned item(s): {accepted}")

        # The engine already skips + warns an unregistered loot roll before
        # it becomes a ground item (asserted above), so a real spawn can
        # never surface one for the new registry check to reject — drive
        # the check function directly with a synthetic unregistered id
        # instead (issue #800 review amendment).
        bogus_name = "item_that_does_not_exist"
        rejected = unregistered_item_ids({bogus_name}, registered)
        if rejected == {bogus_name}:
            print(f"PASS: the registry check rejects a synthetic "
                  f"unregistered item id ({bogus_name!r})")
        else:
            failures.append(
                f"registry check did not reject synthetic unregistered id "
                f"{bogus_name!r}: got {rejected}")
    finally:
        quit_engine(args.port, proc)

    # ---- Phase 4: a building AND a unit content entry spawn correctly
    #      on a HIDDEN,
    #      non-active page (#90 review fix — building.spawn now takes an
    #      explicit pageId, mirroring unit.spawn/item.spawnGround, and its
    #      occupancy/terrain-Z check is scoped to THAT page, not a snapshot
    #      of the visible worlds). A DENSE location (one per land chunk,
    #      like tools/location_overlay_probe.py's DENSE_YAML) guarantees
    #      content at the SYNCHRONOUS centre chunk (0,0), which stamps at
    #      world.init time via Init.hs's centre-chunk hook regardless of
    #      active/visible status — so this needs no chunk loading on the
    #      hidden page (world.loadChunksInRegion only targets the active
    #      world, so a hidden page can't otherwise be force-loaded here). ----
    dense_yaml = art.fixture("dense")
    with open(dense_yaml, "w") as fh:
        fh.write(DENSE_LOCATION_YAML)
    proc = boot_isolated(args.port, art)
    try:
        # Registries only — NOT ruin_small.yaml, which would contend with
        # dense_ruin for chunk (0,0) and make the placement non-deterministic
        # (mirrors tools/location_overlay_probe.py's isolated DENSE_YAML use).
        load_registries(args.port)
        load_fixture_yaml(args.port, "engine.loadLocationYaml", dense_yaml)
        send(args.port, "world.initArena('arena'); world.initArenaDone('arena'); "
                        "world.show('arena'); return 'ok'")
        arena_ok = False
        for _ in range(40):
            r = send(args.port, "local i=world.getChunkInfo(0,0); return i and i.loaded and 'y' or 'n'").strip('"')
            if r == "y":
                arena_ok = True
                break
            time.sleep(0.25)
        if not arena_ok:
            failures.append("phase 4: arena never became ready")
        else:
            # Generate 'sw2' but NEVER show it — arena stays active throughout.
            # NB world.waitForInit always polls the ACTIVE world (arena,
            # already done) — it can't wait for a hidden page, so loc_at's
            # own retry loop is what actually waits for 'sw2' to be ready.
            send(args.port, f"world.init('sw2', {args.seed}, {args.size}, 3); return 'ok'")
            active = send(args.port, "return world.getActiveWorldId()").strip('"')
            if active != "arena":
                failures.append(f"phase 4: expected 'arena' active throughout, got '{active}'")
            else:
                gxgy = loc_at(args.port, 0, 0, "sw2")
                if gxgy is None:
                    failures.append(
                        "phase 4: no location on centre chunk (0,0) of hidden page 'sw2'")
                else:
                    gx, gy = gxgy
                    if not wait_floor(args.port, gx, gy, page="sw2"):
                        failures.append(
                            f"phase 4: centre chunk (0,0)/({gx},{gy}) on 'sw2' never stamped")
                    else:
                        blist = send(args.port, "return building.list()")
                        if f"({gx}, {gy}," in blist:
                            print(f"PASS: building content spawned at ({gx},{gy}) on hidden "
                                  f"page 'sw2' while 'arena' stayed active (multiworld fix)")
                        else:
                            failures.append(
                                f"phase 4: no cargo_hold_S building at ({gx},{gy}) on "
                                f"hidden page 'sw2' — building.list() returned: {blist!r}")
                        # unit content (a KNOWN id) spawns too — the
                        # unit-kind dispatch path, moved here now that
                        # This fixture's fixed acolyte exercises the ordinary
                        # unit-kind dispatch in addition to ruin_small's ranged
                        # nomad entries. The spawn happened while 'sw2' was hidden;
                        # unit.list is
                        # active-world-only (#377), so show sw2 to observe
                        # it — the hidden-spawn property is already proven.
                        send(args.port, "world.show('sw2'); return 'ok'")
                        n_units = 0
                        for _ in range(20):
                            n_units = unit_count(args.port, "acolyte")
                            if n_units >= 1:
                                break
                            time.sleep(0.5)
                        if n_units >= 1:
                            print(f"PASS: unit content spawned on hidden page 'sw2' "
                                  f"({n_units} acolyte)")
                        else:
                            failures.append(
                                "phase 4: no acolyte unit spawned from dense_ruin "
                                "unit content on hidden page 'sw2'")

                        # #915 multi-page COLLISION: instance ids are
                        # allocated PER PAGE, so the SAME number names
                        # different real locations on different worlds.
                        # 'sw3' is generated from the same seed/size as
                        # 'sw2', so its dense placement is identical and
                        # the centre-chunk location gets the identical
                        # id — the case a page-blind memory would get
                        # wrong. Both pages stay loaded throughout.
                        send(args.port, "engine.setPaused(true); return 'ok'")
                        load_ai_stack(args.port)
                        send(args.port, f"world.init('sw3', {args.seed}, "
                                        f"{args.size}, 3); return 'ok'")
                        sw3xy = loc_at(args.port, 0, 0, "sw3")
                        iid2 = wait_floor(args.port, gx, gy, page="sw3") \
                            and loc_instance_at(args.port, 0, 0, "sw3")
                        iid = loc_instance_at(args.port, 0, 0, "sw2")
                        if sw3xy != (gx, gy) or iid < 0 or iid2 != iid:
                            failures.append(
                                f"phase 4 (#915) setup failed: same-seed pages "
                                f"did not collide — sw2 ({gx},{gy})#{iid} vs "
                                f"sw3 {sw3xy}#{iid2}")
                        else:
                            print(f"PASS: 'sw2' and 'sw3' both carry instance "
                                  f"id {iid} at ({gx},{gy}) — a genuine "
                                  f"cross-page id collision to test against")
                            u2 = spawn_unit(args.port, "acolyte", gx, gy,
                                            "player", "sw2")
                            u3 = spawn_unit(args.port, "acolyte", gx, gy,
                                            "player", "sw3")
                            ok2 = u2 >= 0 and wait_knows(args.port, u2, f"sw2#{iid}")
                            ok3 = u3 >= 0 and wait_knows(args.port, u3, f"sw3#{iid}")
                            k2 = known_locations(args.port, u2)
                            k3 = known_locations(args.port, u3)
                            if ok2 and ok3 and f"sw3#{iid}" not in k2 \
                                    and f"sw2#{iid}" not in k3:
                                print(f"PASS: each unit learned ONLY its own "
                                      f"page's instance {iid} — equal ids on "
                                      f"two pages did not alias")
                            else:
                                failures.append(
                                    f"phase 4 (#915): cross-page aliasing — "
                                    f"unit {u2} on sw2 knows {sorted(k2)}, "
                                    f"unit {u3} on sw3 knows {sorted(k3)}")
                            resolves = {
                                p: send(args.port,
                                        f"return world.getLocationInstance("
                                        f"{iid}, '{p}') and 'y' or 'n'").strip('"')
                                for p in ("sw2", "sw3")}
                            if resolves == {"sw2": "y", "sw3": "y"}:
                                print(f"PASS: instance id {iid} resolves on BOTH "
                                      f"pages — the memories stayed distinct "
                                      f"because each carries its own page, not "
                                      f"because only one page had that id")
                            else:
                                failures.append(
                                    f"phase 4 (#915): expected instance {iid} on "
                                    f"both pages, got {resolves}")
    finally:
        quit_engine(args.port, proc)

    # ---- Phase 5 (#1101): a placed location is named in its world's
    #      own generated language, falls back to the definition label
    #      when the world has none, and both survive save/load. ----
    named: dict[int, tuple[str, str | None]] = {}
    proc = boot_isolated(args.port, art)
    try:
        load_defs(args.port)
        gen_named_world(args.port, "ln", args.seed, args.size)
        prov = send(args.port, "return world.getLanguageProvenance('ln')")
        if NAMED_WORLD_LANG_SEED not in prov:
            failures.append(
                f"phase 5 (#1101): the page recorded no language provenance "
                f"— every assertion below would only be testing the "
                f"no-language fallback; got {prov!r}")
        rows = ruins_ready(args.port, "ln")
        if not rows:
            failures.append("phase 5 (#1101): no ruin_small placed on the "
                            "named world")
        else:
            named = {e["instance_id"]: (e["name"], e.get("gloss"))
                     for e in rows}
            bad = [(i, n, g) for i, (n, g) in named.items()
                   if not n or n == "Small Ruin" or not g]
            if bad:
                failures.append(
                    f"phase 5 (#1101): expected a generated name + gloss on "
                    f"every ruin, got {bad}")
            else:
                print(f"PASS: {len(named)} ruin(s) named in the world's own "
                      f"language: "
                      + ", ".join(f"{n} ({g})" for n, g in named.values()))

            # The SAME terrain seed with no language: the fallback, and
            # the gloss key absent rather than empty.
            gen_world(args.port, "lp", args.seed, args.size)
            plain = ruins_ready(args.port, "lp")
            wrong = [e for e in plain
                     if e["name"] != "Small Ruin" or "gloss" in e]
            if not plain:
                failures.append("phase 5 (#1101): no ruin_small placed on "
                                "the unnamed world")
            elif wrong:
                failures.append(
                    f"phase 5 (#1101): an unnamed world must fall back to "
                    f"ldLabel with NO gloss key, got "
                    f"{[(e['name'], e.get('gloss')) for e in wrong]}")
            else:
                print(f"PASS: the same seed with no language falls back to "
                      f"'Small Ruin' on all {len(plain)} ruin(s), no gloss")

            # The fresh process below reads this fixture (#1620).
            saved_naming = save_and_wait(args.port, "ln", slot_naming,
                                         failures, log=art.engine_log)
    finally:
        quit_engine(args.port, proc)

    if named and saved_naming and not failures:
        proc = boot_isolated(args.port, art)
        try:
            load_defs(args.port)
            load_ai_stack(args.port)
            if not load_and_wait(args.port, slot_naming, failures,
                                 log=art.engine_log):
                raise _PhaseAborted
            send(args.port, "world.show('ln'); return 'ok'")
            time.sleep(1.0)
            after = {e["instance_id"]: (e["name"], e.get("gloss"))
                     for e in ruins_ready(args.port, "ln")}
            if after == named:
                print("PASS: every location name AND gloss survived "
                      "save -> fresh process -> load byte-exact")
            else:
                failures.append(
                    f"phase 5 (#1101): names/glosses changed across "
                    f"save/load: before={named} after={after}")

            # Same seed, same language, fresh process: identical
            # names. Write-once storage would hide a nondeterministic
            # namer, so this regenerates rather than reloading.
            gen_named_world(args.port, "ln2", args.seed, args.size)
            regen = {e["instance_id"]: (e["name"], e.get("gloss"))
                     for e in ruins_ready(args.port, "ln2")}
            if regen == named:
                print("PASS: regenerating the same seed + language in a "
                      "fresh process reproduces every name and gloss")
            else:
                failures.append(
                    f"phase 5 (#1101): regeneration is not deterministic: "
                    f"first={named} regenerated={regen}")
        except _PhaseAborted:
            pass
        finally:
            quit_engine(args.port, proc)

    print("-" * 56)
    if failures:
        # Durable records rather than the unflushed stderr print this was
        # (#1982): `run_probes.py` merges this probe's stderr into a
        # block-buffered stdout pipe and prints only its last 25 lines, so
        # a printed `FAIL:` overtook the buffered checks and landed above
        # the retained tail. These are read back from the COMPLETE
        # capture. Emitted here, inside `run`, because `main`'s cleanup
        # removes the artifact tree the engine log lives in.
        FAILURE.report(failures)
        FAILURE.context_log(art.engine_log)
        FAILURE.context("artifact root", art.base)
        return 1
    print("ALL CHECKS PASSED")
    return 0


if __name__ == "__main__":
    try:
        raise SystemExit(main())
    except FixtureNotRegistered as exc:
        print(f"\n{exc}")
        raise SystemExit(1)
