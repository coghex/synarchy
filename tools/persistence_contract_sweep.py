#!/usr/bin/env python3
"""The broader manual persistence-contract sweep (issue #767, save-
overhaul D1, requirement 15's "broader manual persistence sweep").

Where ``tools/persistence_contract_probe.py`` is a compact, CI-eligible
smoke test (a tiny worldSize-8 page, one building, one unit), this sweep
exercises a REAL generated world at a representative size and populates
every category ``docs/persistence_state_inventory.md``'s SS12 coverage
map names in ONE session: a meaningful world-generation seed + player-
facing identity, a built station with a real running craft bill, a mine
designation, a non-default camera position and map mode -- then runs the
SAME real fresh-process save -> load -> save cycle (three times,
requirement 9) and the SAME
``tools/persistence_snapshot.compare_session_files`` structural
comparison (requirement 1/2/5) the compact probe uses, just against a
richer scenario and a real (not tiny) world size.

This sweep does NOT re-implement the domain-specific scenarios (chop/
till/crop/plant designations, power-node placement, the assembled
failure contract's individual cases) that ALREADY have their own
maintained, real-process regression probes -- requirement 14 explicitly
asks to avoid "retaining multiple expensive probes that test the same
final behavior". Instead it actually RUNS them (via the existing
``tools/run_probes.py --only ... --exact`` aggregate runner -- round-1
review: a mere "does this file exist" check proves nothing and was
replaced) and propagates any failure, so requirement 11 (the assembled
failure contract) and requirement 13 (every maintained migration
baseline) genuinely stay gated, not merely documented:

  tools/chop_probe.py, tools/till_probe.py, tools/crop_probe.py,
  tools/plant_probe.py, tools/construction_probe.py (designations),
  tools/craft_bill_probe.py, tools/power_probe.py (craft economy / power
  nodes at full domain depth), tools/transactional_load_probe.py (whole-
  session replacement, requirement 10), tools/save_storage_probe.py
  (interrupted write / corrupt envelope / restart-and-select fallback),
  tools/save_barrier_probe.py (write-failure surfacing),
  tools/persistence_integrity_probe.py (dangling Haskell/Lua reference
  tolerance), tools/save_compat_migration_probe.py (every maintained
  migration baseline, requirement 13).

This is genuinely slow (each cross-referenced probe pays its own real
engine-boot/worldgen cost on top of this sweep's own scenario) -- that
is the whole point of the compact/broad two-tier split (requirement 15):
the compact probe stays CI-eligible, this sweep does not.
``--cross-probe-keys`` (comma-separated, ``--exact`` semantics) can
narrow the set for local iteration on this script's OWN scenario;
``--skip-cross-probes`` skips them entirely (loudly reported as reduced
coverage, never silently).

This sweep's OWN scenario, AND every cross-referenced probe it can
invoke, always uses an isolated resource root, a unique port, and a
fresh process, and never touches the developer's real saves/
(requirement 15). Round-5 found only 3 of the 12 cross-referenced
probes satisfied that (save_storage/persistence_integrity/
save_compat_migration) and made the other 9 entirely uninvocable from
this script rather than risk them touching real saves/ -- but that left
this sweep unable to fulfil its own stated requirement-11/13 coverage.
Round-6's real fix: those 9 probes (chop/till/crop/plant/construction/
power/transactional_load/save_barrier/craft_bill) now take
--resource-root themselves and use an isolated root exactly like this
sweep's own scenario (verified individually against real engines --
none of them touch this repo's real saves/ any more). All 12 are
therefore SELECTABLE via ``--cross-probe-keys``
(SELECTABLE_CROSS_REFERENCED_PROBE_KEYS) -- an unrecognized key exits
with an error rather than silently running it. craft_bill is the one
exception kept OUT of the DEFAULT set (not out of selectability): it's
independently flaky per `tools/ci_probes.py --status` ("craft_job AI
claim/work timing flakes run-to-run on CI"), unrelated to persistence,
and would make this sweep spuriously fail on unrelated AI timing if
defaulted on.

Usage:
  python3 tools/persistence_contract_sweep.py [--port 9278] \\
      [--world-size 64] [--seed 20260721] [--cross-probe-jobs 2]
"""
from __future__ import annotations

import argparse
import glob
import os
import shutil
import subprocess
import sys
import tempfile
import time
from pathlib import Path

from probelib import (boot, quit_engine, send, send_json, wait_load_published,
                       wait_save_complete, capture_request_id)
from persistence_snapshot import compare_session_files
from save_compat_audit import dump_canonical_summary

REPO = Path(__file__).resolve().parent.parent
PAGE = "contract_sweep_page"
# A page that only ever exists in a fresh, PRE-load live session -- never
# part of any save this suite writes -- used once (round-5 review) to
# prove a load replaces the whole session rather than merging into it.
GHOST_PAGE = "contract_sweep_preload_ghost"
# Round-6 review: a SECOND real, saved page with its own identity and
# visibility (hidden, not active, unlike PAGE) -- proves a multi-page
# session round-trips through save/load with each page's own state and
# the active/visible relationship between them intact, not just a
# single-page shape (multiworld_save_probe.py's own gate, exercised here
# inside this suite's own isolated scenario too). Deliberately NOT a
# camera check: camera is a single GLOBAL live value duplicated across
# every page's own field at save time (see this repo's own documented
# save-overhaul notes: "no per-page zoom/facing exists in that format")
# -- a hidden page's camera is not an independently-preserved guarantee
# to test (confirmed live while validating this fix: it read back
# {0,0}, not the value set on the page before saving).
BETA_PAGE = "contract_sweep_page_beta"

# The AI/stats script stack the loading screen would load in a real GUI
# session -- headless boots skip it entirely (CLAUDE.md), so any probe
# driving unit_ai (commandAttack/getState) must load these itself.
AI_SCRIPTS = (
    ("scripts/unit_stats.lua", 0.1),
    ("scripts/unit_resources.lua", 0.2),
    ("scripts/unit_ai.lua", 0.1),
    ("scripts/building_spawn.lua", 0.2),
)

# Domain probes ALREADY covering a category this sweep's own scenario
# does not reconstruct from scratch (requirement 14) -- actually RUN via
# tools/run_probes.py --only <these keys> --exact, not merely documented
# (round-1 review: an existence-only check proves nothing). Keys, not
# filenames, since that is what run_probes.py --exact matches against.
#
# Round-6 review: previously only save_storage/persistence_integrity/
# save_compat_migration passed their own --resource-root, and the other
# 9 (chop/till/crop/plant/construction/power/transactional_load/
# save_barrier/craft_bill) called engine.saveWorld/loadSave straight
# against this repo's real saves/ directory -- so round-5 made them
# entirely uninvocable from this script (no opt-in escape hatch), which
# left this sweep unable to fulfil its own stated requirement-11/13
# coverage for anything past the 3 isolated probes. The real fix (this
# round): those 9 probes themselves now take --resource-root and use an
# isolated root exactly like this sweep's own scenario (verified live,
# each individually, against a real engine -- none of them touch this
# repo's real saves/ any more). All 12 are therefore SELECTABLE via
# --cross-probe-keys now. craft_bill is still excluded from the DEFAULT
# (not from selectability): it's independently flaky per
# `tools/ci_probes.py --status` ("craft_job AI claim/work timing flakes
# run-to-run on CI"), unrelated to persistence, and would make this
# sweep spuriously fail on unrelated AI timing if defaulted on.
SELECTABLE_CROSS_REFERENCED_PROBE_KEYS = [
    "chop", "till", "crop", "plant", "construction", "power",
    "transactional_load", "save_barrier", "craft_bill",
    "save_storage", "persistence_integrity", "save_compat_migration",
]
FLAKY_CROSS_REFERENCED_PROBE_KEYS = ["craft_bill"]
DEFAULT_CROSS_REFERENCED_PROBE_KEYS = [
    k for k in SELECTABLE_CROSS_REFERENCED_PROBE_KEYS
    if k not in FLAKY_CROSS_REFERENCED_PROBE_KEYS
]


class Checks:
    def __init__(self) -> None:
        self.failed = 0

    def ok(self, cond: bool, label: str) -> None:
        print(f"  [{'PASS' if cond else 'FAIL'}] {label}")
        if not cond:
            self.failed += 1


def make_isolated_root(base: str) -> str:
    root = os.path.join(base, "root")
    os.makedirs(root, exist_ok=True)
    for family in ("scripts", "assets", "data", "config"):
        target = os.path.join(root, family)
        if not os.path.exists(target):
            os.symlink(os.path.join(REPO, family), target)
    os.makedirs(os.path.join(root, "saves"), exist_ok=True)
    return root


def bootstrap_defs(port: int) -> None:
    loaders = [
        ("data/substances/*.yaml", "engine.loadSubstanceYaml"),
        ("data/items/*.yaml",      "engine.loadItemYaml"),
        ("data/equipment/*.yaml",  "engine.loadEquipmentYaml"),
        ("data/materials/*.yaml",  "engine.loadMaterialYaml"),
        ("data/units/*.yaml",      "engine.loadUnitYaml"),
        ("data/buildings/*.yaml",  "engine.loadBuildingYaml"),
        ("data/recipes/*.yaml",    "engine.loadRecipeYaml"),
    ]
    for pattern, fn in loaders:
        for path in sorted(glob.glob(pattern)):
            send(port, f"{fn}('{path}'); return 'ok'")


def load_ai_stack(port: int) -> None:
    for path, delay in AI_SCRIPTS:
        send(port, f"engine.loadScript('{path}', {delay}); return 'ok'")


def get_attack_target(port: int, uid: int):
    raw = send(port, f"local s = require('scripts.unit_ai').getState({uid}); "
                      f"return s and s.attackTargetUid or nil")
    return as_int(raw)


def assert_nondefault_map_mode(chk: "Checks", tmpdir: str, save_path: str, page: str) -> None:
    """world.setMapMode has no live query counterpart, so the non-default
    value is verified through the same dump_canonical_summary decode
    tools/save_compat_migration_probe.py already uses."""
    import json
    out_path = os.path.join(tmpdir, "map_mode_check.json")
    ok, tail = dump_canonical_summary(Path(save_path), Path(out_path))
    if not ok:
        chk.ok(False, f"dump_canonical_summary failed for map-mode check: {tail}")
        return
    summary = json.loads(Path(out_path).read_text(encoding="utf-8"))
    page_entry = next((p for p in summary.get("pages", []) if p.get("pageId") == page), None)
    chk.ok(bool(page_entry) and page_entry.get("mapMode") == "ZMPressure",
           f"world.setMapMode('{page}', 'map_pressure') actually took effect "
           f"(decoded pgsMapMode = {page_entry.get('mapMode') if page_entry else None!r}, "
           f"expected 'ZMPressure')")


def boot_probe(root: str, port: int, log: str):
    return boot(port, log=log, args=["--resource-root", root], ready_timeout=240)


def wait_for_file(path: str, seconds: float = 60.0) -> bool:
    deadline = time.time() + seconds
    while time.time() < deadline:
        if os.path.isfile(path):
            return True
        time.sleep(0.2)
    return False


def save_and_wait(chk: Checks, port: int, page: str, slot: str) -> str:
    """engine.saveWorld, then tie completion to THIS save's own request id
    via engine.getSaveStatus() reaching SaveCaptureComplete (round-4
    review, mirrors persistence_contract_probe.py's identical fix: the
    appearance of world.synworld on disk is a proxy, not the
    authoritative completion signal -- see probelib.wait_save_complete)."""
    saved = send(port, f"return engine.saveWorld('{page}', '{slot}')")
    chk.ok(saved.strip() == "true", f"engine.saveWorld('{slot}') accepted (got {saved!r})")
    request_id = capture_request_id(port, "return engine.getSaveStatus()")
    chk.ok(request_id is not None,
           f"engine.getSaveStatus() reports a request id right after saveWorld('{slot}')")
    ok, final_status = wait_save_complete(port, request_id)
    chk.ok(ok, f"save '{slot}' (request {request_id}) reached SaveCaptureComplete "
                f"(got {final_status!r})")
    return final_status


def as_int(s: str):
    try:
        return int(float(s))
    except (TypeError, ValueError):
        return None


def page_exists(port: int, page: str) -> bool:
    """A page's registration oracle (mirrors transactional_load_probe.py's
    identical helper): world.getDate returns nil for a page that isn't
    (or is no longer) registered."""
    r = send(port, f"return world.getDate('{page}')")
    return r not in ("nil", "null", "")


def build_rich_scenario(chk: Checks, port: int, seed: int, size: int, plates: int) -> tuple[int, int, int, int]:
    """A REAL generated world (not a tiny arena) with a meaningful seed +
    identity, a built craft station running a real bill, an acolyte_portal
    with a real roster countdown (non-vacuous lua.building_spawn state), a
    unitAi.commandAttack between two units (non-vacuous lua.unit_ai
    state), a mine designation, a non-default map mode, and (round-6
    review) a SECOND real saved page with its own identity/camera/
    visibility -- the representative scenario requirement 4 asks for, at
    real worldgen scale. Returns (portal_bid, attacker_uid, target_uid,
    beta_uid)."""
    bootstrap_defs(port)
    load_ai_stack(port)
    inited = send(
        port,
        f"world.init('{PAGE}', {seed}, {size}, {plates}, "
        f"'Sweep Test World', 'the persistence sweep test world'); return 'ok'")
    chk.ok("ok" in inited, f"world.init accepted (got {inited!r})")
    send(port, f"world.show('{PAGE}'); return 'ok'")
    deadline = time.time() + 60.0
    while time.time() < deadline:
        if send(port, "return world.getActiveWorldId()").strip('"') == PAGE:
            break
        time.sleep(0.2)
    else:
        chk.ok(False, f"'{PAGE}' never became the active world")
    published = send(port, "return world.waitForInit(300)", timeout=305)
    print(f"  worldgen finished: {published}")

    bid = as_int(send(port, "return building.spawn('furnace', 0, 0)"))
    chk.ok(bid is not None and bid >= 0, f"building.spawn('furnace') succeeded (got {bid!r})")

    if bid is not None:
        bill = as_int(send(port, f"local id,err = craft.addBill({bid}, 'smelt_steel_lignite'); return id"))
        chk.ok(bill is not None and bill >= 0,
               f"craft.addBill('smelt_steel_lignite') on the built furnace succeeded (got {bill!r})")

    # A real, non-vacuous lua.building_spawn state (round-1 review: an
    # ordinary building like furnace creates none at all -- only
    # acolyte_portal is building_spawn.lua-configured).
    portal_bid = as_int(send(port, "return building.spawn('acolyte_portal', 3, 3)"))
    chk.ok(portal_bid is not None and portal_bid >= 0,
           f"building.spawn('acolyte_portal') succeeded (got {portal_bid!r})")
    send(port, f"building.setSpawnRemaining({portal_bid}, 6); return 'ok'")
    remaining_before = as_int(send(port, f"return building.getSpawnRemaining({portal_bid})"))
    deadline = time.time() + 10.0
    remaining_after = remaining_before
    while time.time() < deadline:
        remaining_after = as_int(send(port, f"return building.getSpawnRemaining({portal_bid})"))
        if remaining_after is not None and remaining_before is not None and remaining_after < remaining_before:
            break
        time.sleep(0.3)
    chk.ok(remaining_after is not None and remaining_before is not None
           and remaining_after < remaining_before,
           f"acolyte_portal's roster sequencer spawned at least one unit "
           f"(remaining {remaining_before} -> {remaining_after})")

    # A real, non-vacuous lua.unit_ai state (round-1 review: a freshly
    # spawned, idle unit's AI state is otherwise near-empty).
    atk = as_int(send(port, "return unit.spawn('acolyte', 2, 2, 0, 'player')"))
    chk.ok(atk is not None and atk >= 0, f"attacker unit.spawn succeeded (got {atk!r})")
    tgt = as_int(send(port, "return unit.spawn('acolyte', 6, 6, 0, 'wildlife')"))
    chk.ok(tgt is not None and tgt >= 0, f"target unit.spawn succeeded (got {tgt!r})")
    send(port, f"require('scripts.unit_ai').commandAttack({atk}, {tgt}); return 'ok'")
    time.sleep(0.5)
    attack_target = get_attack_target(port, atk)
    chk.ok(attack_target == tgt,
           f"commandAttack set a real, live attackTargetUid ({attack_target} "
           f"vs expected {tgt})")

    mined = send(port, f"world.designateMine('{PAGE}', 1, 1, 2, 2); return 'ok'")
    chk.ok("ok" in mined, f"world.designateMine accepted (got {mined!r})")
    # world.setMapMode(pageId, mode) -- a bare-mode call (missing pageId)
    # is a silent no-op (round-1 review finding); verified post-save via
    # dump_canonical_summary since there is no live world.getMapMode query.
    send(port, f"world.setMapMode('{PAGE}', 'map_pressure'); return 'ok'")
    time.sleep(0.5)

    # Round-6 review: a second REAL saved page (not an arena -- world.init's
    # own identity args, unlike world.initArena, are what let this page's
    # identity genuinely differ from PAGE's). A small fixed size keeps this
    # affordable regardless of --world-size. It ends up hidden-but-loaded
    # (not destroyed) when the save runs -- engine.saveWorld captures every
    # loaded page, not just the active one (mirrors multiworld_save_probe.py's
    # own "beta WAS live (hidden, not destroyed)" pattern) -- so its
    # identity/visibility relationship to PAGE is a real thing to round-trip,
    # not a merge artifact.
    beta_inited = send(
        port,
        f"world.init('{BETA_PAGE}', {seed + 1}, 8, 3, "
        f"'Sweep Beta World', 'the second persisted page'); return 'ok'")
    chk.ok("ok" in beta_inited, f"world.init('{BETA_PAGE}') accepted (got {beta_inited!r})")
    send(port, f"world.show('{BETA_PAGE}'); return 'ok'")
    deadline = time.time() + 60.0
    while time.time() < deadline:
        if send(port, "return world.getActiveWorldId()").strip('"') == BETA_PAGE:
            break
        time.sleep(0.2)
    else:
        chk.ok(False, f"'{BETA_PAGE}' never became the active world")
    send(port, "return world.waitForInit(60)", timeout=65)
    beta_uid = as_int(send(port, "return unit.spawn('acolyte', 2, 2, 0, 'player')"))
    chk.ok(beta_uid is not None and beta_uid >= 0,
           f"beta page unit.spawn succeeded (got {beta_uid!r})")
    # Distinct VISIBILITY: beta ends up hidden, PAGE ends up active again --
    # the relationship a load must restore, not just each page in isolation.
    send(port, f"world.hide('{BETA_PAGE}'); return 'ok'")
    send(port, f"world.show('{PAGE}'); return 'ok'")
    deadline = time.time() + 60.0
    while time.time() < deadline:
        if send(port, "return world.getActiveWorldId()").strip('"') == PAGE:
            break
        time.sleep(0.2)
    else:
        chk.ok(False, f"'{PAGE}' did not become active again after building beta")

    # Round-2/3 review: the reset-policy check only proves something if
    # REAL non-default selections/tool exist before the save -- seed all
    # three selection classes requirement 6 names (unit, building, tile)
    # plus the tool mode, so "cleared after load" is a meaningful
    # assertion, not a vacuous one. Each selection is verified IMMEDIATELY
    # after it's set (matching persistence_contract_probe.py's identical
    # fix): with a real acolyte_ai combat loop and a roster sequencer both
    # actively ticking in the background, umSelected/bmSelected are live,
    # frequently-read engine state that a slow batch of several
    # round-tripped debug-console calls can observe mid-flux -- checking
    # right after each individual set keeps the window between "set" and
    # "verify" as small as a single request/response round trip.
    selected = send(port, f"return unit.select({atk})").strip()
    chk.ok(selected == "true", f"unit.select({atk}) succeeded before saving (got {selected!r})")
    sel_before = send(port, "return unit.getSelected()")
    chk.ok(atk_in_selection(sel_before, atk),
           f"unit {atk} is genuinely selected before saving (got {sel_before!r})")

    # building.select(bid) returns no Lua value (0 results) -- verify via
    # building.getSelected() instead, which reports a bare bid (or nil),
    # NOT an array like unit.getSelected().
    send(port, f"building.select({portal_bid}); return 'ok'")
    bsel_before = as_int(send(port, "return building.getSelected()"))
    chk.ok(bsel_before == portal_bid,
           f"building {portal_bid} is genuinely selected before saving (got {bsel_before!r})")

    send(port, f"world.selectTile('{PAGE}', 3, 4, 2); return 'ok'")
    tile_before = send(port, f"return world.getSelectedTile('{PAGE}')")
    chk.ok(tile_before.strip() not in ("nil", "null", ""),
           f"a tile is genuinely selected before saving (got {tile_before!r})")

    send(port, f"world.setToolMode('{PAGE}', 'tool_mine'); return 'ok'")
    tool_before = send(port, "return world.getToolMode()").strip().strip('"')
    chk.ok(tool_before == "mine",
           f"tool mode is genuinely non-default before saving (got {tool_before!r})")

    return portal_bid, atk, tgt, beta_uid


def atk_in_selection(raw: str, uid: int) -> bool:
    import json
    try:
        parsed = json.loads(raw)
        return isinstance(parsed, list) and float(uid) in [float(x) for x in parsed]
    except (TypeError, ValueError):
        return False


def sample_live_state(port: int, portal_bid: int, atk: int) -> dict:
    """See persistence_contract_probe.py's identical helper for why this
    scenario never trips the one documented pause-independent mutator
    (#780 location discovery)."""
    return {
        "date": send(port, f"return world.getDate('{PAGE}')"),
        "activePage": send(port, "return world.getActiveWorldId()"),
        "paused": send(port, "return engine.isPaused()"),
        "toolMode": send(port, "return world.getToolMode()"),
        "mineDesignationCount": send(port, f"return world.getMineDesignationCount('{PAGE}')"),
        "spawnRemaining": send(port, f"return building.getSpawnRemaining({portal_bid})"),
        "attackTargetUid": get_attack_target(port, atk),
    }


def assert_reset_policy(chk: Checks, port: int, when: str) -> None:
    chk.ok(send(port, "return engine.isPaused()").strip() == "true",
           f"{when}: session begins paused")
    tool_mode = send(port, "return world.getToolMode()").strip().strip('"')
    chk.ok(tool_mode == "default",
           f"{when}: tool resets to the default tool (#103), got {tool_mode!r}")
    sel = send(port, "return unit.getSelected()")
    chk.ok(sel.strip() in ("nil", "null", "[]", "{}", ""),
           f"{when}: unit selection is empty (got {sel!r})")
    # Round-3 review: requirement 6 names building and tile selections
    # too, not just unit selection.
    bsel = send(port, "return building.getSelected()")
    chk.ok(bsel.strip() in ("nil", "null", ""),
           f"{when}: building selection is empty (got {bsel!r})")
    tile = send(port, f"return world.getSelectedTile('{PAGE}')")
    chk.ok(tile.strip() in ("nil", "null", ""),
           f"{when}: tile selection is empty (got {tile!r})")


def run_cross_referenced_probes(chk: Checks, keys: list[str], jobs: int) -> None:
    """Actually RUN the domain-specific and assembled-failure-contract
    probes this sweep cross-references (requirement 11/13), via the
    existing aggregate runner -- round-1 review: a file-existence check
    alone proves nothing about whether they still pass."""
    if not keys:
        chk.ok(False, "cross-referenced probes were skipped (--skip-cross-probes) "
                       "-- requirement 11/13 coverage is NOT exercised by this run")
        return
    print(f"=== running {len(keys)} cross-referenced probe(s) via "
          f"run_probes.py --only ... --exact --jobs {jobs} --retries 1 "
          f"(this is slow) ===")
    # --retries 1 matches CI's own convention for a parallel --jobs run
    # (see CLAUDE.md's CI probe-gate section): running probes pairwise
    # risks the SAME parallel-engine-contention flake CI's own gate
    # absorbs with a solo re-run, so this sweep must absorb it the same
    # way rather than spuriously failing on contention unrelated to
    # persistence (observed live while validating this fix: a probe
    # failed "engine exited before READY" under --jobs 2, then passed
    # cleanly run alone).
    proc = subprocess.run(
        [sys.executable, str(REPO / "tools" / "run_probes.py"),
         "--only", ",".join(keys), "--exact", "--jobs", str(jobs),
         "--retries", "1"],
        cwd=REPO)
    chk.ok(proc.returncode == 0,
           f"cross-referenced probes ({', '.join(keys)}) all passed via "
           f"run_probes.py (exit code {proc.returncode})")


def main() -> int:
    ap = argparse.ArgumentParser(description=__doc__,
                                 formatter_class=argparse.RawDescriptionHelpFormatter)
    ap.add_argument("--port", type=int, default=9278)
    ap.add_argument("--seed", type=int, default=20260721)
    ap.add_argument("--world-size", type=int, default=64)
    ap.add_argument("--plates", type=int, default=5)
    ap.add_argument("--cross-probe-keys",
                     default=",".join(DEFAULT_CROSS_REFERENCED_PROBE_KEYS),
                     help="comma-separated exact tools/run_probes.py probe keys to "
                          "actually run for requirement 11/13 coverage -- ONLY keys "
                          "from " + ",".join(SELECTABLE_CROSS_REFERENCED_PROBE_KEYS) +
                          " are accepted (every one isolated, requirement 15); "
                          "default: all of them except craft_bill, which is isolated "
                          "but independently flaky per `ci_probes.py --status` and "
                          "must be listed explicitly to opt in")
    ap.add_argument("--cross-probe-jobs", type=int, default=2,
                     help="run_probes.py --jobs for the cross-referenced probes")
    ap.add_argument("--skip-cross-probes", action="store_true",
                     help="skip the cross-referenced probes entirely (loudly reported "
                          "as reduced coverage, never the default)")
    args = ap.parse_args()
    port = args.port
    chk = Checks()

    if not args.skip_cross_probes:
        requested_keys = [k for k in args.cross_probe_keys.split(",") if k.strip()]
        unrecognized = [k for k in requested_keys
                         if k not in SELECTABLE_CROSS_REFERENCED_PROBE_KEYS]
        if unrecognized:
            sys.exit(
                f"--cross-probe-keys names {unrecognized!r}, which this script does "
                f"not recognize: only {SELECTABLE_CROSS_REFERENCED_PROBE_KEYS!r} are "
                f"registered cross-referenced probes.")

    tmpdir = tempfile.mkdtemp(prefix="persistence_contract_sweep_")
    proc = None
    try:
        root = make_isolated_root(tmpdir)

        print("=== engine A: build the representative scenario, save 'gen1' ===")
        proc = boot_probe(root, port, os.path.join(tmpdir, "engineA.log"))
        portal_bid, atk, tgt, beta_uid = build_rich_scenario(
            chk, port, args.seed, args.world_size, args.plates)
        save_and_wait(chk, port, PAGE, "gen1")
        gen1_path = os.path.join(root, "saves", "gen1", "world.synworld")
        chk.ok(wait_for_file(gen1_path, seconds=5.0),
               f"save file appeared at {gen1_path}")
        assert_nondefault_map_mode(chk, tmpdir, gen1_path, PAGE)
        quit_engine(port, proc)
        proc = None

        # ── Requirement 9: at least THREE fresh-process save->load->save
        # cycles. Engine A's save above is the INITIAL save, not itself a
        # cycle -- each of the three engines below is one complete fresh-
        # process "load prior generation -> save the next one" cycle
        # (round-2 review: two engines only complete two cycles, not
        # three) -----------------------------------------------------────
        gen_paths = [gen1_path]
        prev_slot = "gen1"
        for i, letter in enumerate("BCD", start=2):
            next_slot = f"gen{i}"
            print(f"=== engine {letter}: fresh process, load '{prev_slot}', "
                  f"save '{next_slot}' ===")
            proc = boot_probe(root, port, os.path.join(tmpdir, f"engine{letter}.log"))
            bootstrap_defs(port)
            load_ai_stack(port)

            if letter == "B":
                # Round-5 review: prove a load REPLACES the whole session
                # rather than merging into it -- a page that exists only
                # in this fresh, pre-load session (never part of the
                # save being loaded) must NOT survive publication (#763,
                # mirrors transactional_load_probe.py's "complete
                # replacement (not a merge)" scenario, but exercised here
                # inside THIS suite's own isolated resource root/scenario
                # rather than relying solely on that excluded-by-default
                # cross-probe). A no-generator arena is enough -- its
                # own content is irrelevant, only its registration is.
                send(port, f"world.initArena('{GHOST_PAGE}'); return 'ok'")
                chk.ok(page_exists(port, GHOST_PAGE),
                       f"setup: pre-load-only page '{GHOST_PAGE}' is live "
                       f"before the load")

            loaded = send(port, f"return engine.loadSave('{prev_slot}')")
            chk.ok(loaded.strip() == "true",
                   f"engine.loadSave('{prev_slot}') accepted (got {loaded!r})")
            load_id = capture_request_id(port, "return engine.getLoadStatus()")
            chk.ok(load_id is not None,
                   f"engine.getLoadStatus() reports a request id right after "
                   f"loadSave('{prev_slot}')")
            published, status = wait_load_published(port, seconds=300, request_id=load_id)
            chk.ok(published, f"load transaction {load_id} published (status={status})")

            if letter == "B":
                chk.ok(not page_exists(port, GHOST_PAGE),
                       f"pre-load-only page '{GHOST_PAGE}' (never part of "
                       f"'{prev_slot}') did NOT survive the load -- a load "
                       f"replaces the whole session, it does not merge")

                # Round-6 review: the multi-page relationship -- PAGE
                # active, BETA_PAGE hidden-but-loaded, EXACTLY as saved --
                # must survive the load, not just each page's own state in
                # isolation. Checked without ever switching the active
                # page away from PAGE (world.getIdentity/unit.exists both
                # take an explicit page/id argument): the checks below
                # this block assume PAGE stays active throughout, and a
                # real page switch to a full-sized generated world can
                # take longer than is worth risking here.
                active_after_load = send(port, "return world.getActiveWorldId()").strip('"')
                chk.ok(active_after_load == PAGE,
                       f"'{PAGE}' (not '{BETA_PAGE}') is active immediately after "
                       f"the load, matching the visibility relationship as saved "
                       f"(got {active_after_load!r})")
                chk.ok(page_exists(port, BETA_PAGE),
                       f"beta page '{BETA_PAGE}' (part of the save) survived the load")
                beta_exists = send(port, f"return unit.exists({beta_uid})").strip()
                chk.ok(beta_exists == "true",
                       f"beta page's unit {beta_uid} survived the load "
                       f"(got {beta_exists!r})")
                beta_identity = send_json(port, f"return world.getIdentity('{BETA_PAGE}')")
                chk.ok(isinstance(beta_identity, dict)
                       and beta_identity.get("name") == "Sweep Beta World",
                       f"beta page's own identity (distinct from '{PAGE}''s) survived "
                       f"the load (got {beta_identity!r})")

            assert_reset_policy(chk, port, f"after loading {prev_slot}")
            attack_target = get_attack_target(port, atk)
            chk.ok(attack_target == tgt,
                   f"lua.unit_ai's attackTargetUid survived the fresh-process "
                   f"load of '{prev_slot}' ({attack_target} vs expected {tgt})")

            if letter == "B":
                before = sample_live_state(port, portal_bid, atk)
                time.sleep(2.0)
                after = sample_live_state(port, portal_bid, atk)
                chk.ok(before == after,
                       f"requirement 7: the live inspection is unchanged "
                       f"across a 2s paused dwell ({before} -> {after})")

            save_and_wait(chk, port, PAGE, next_slot)
            next_path = os.path.join(root, "saves", next_slot, "world.synworld")
            chk.ok(wait_for_file(next_path, seconds=5.0),
                   f"save file appeared at {next_path}")
            gen_paths.append(next_path)

            if letter == "D":
                send(port, "require('scripts.pause').set(false); return 'ok'",
                     expect_result=False)
                time.sleep(0.5)
                ts = send(port, f"return world.getTimeScale('{PAGE}')")
                chk.ok(ts.strip() in ("1", "1.0"),
                       f"unpausing resumes at the default simulation speed "
                       f"(got {ts})")

            quit_engine(port, proc)
            proc = None
            prev_slot = next_slot

        print(f"=== comparing {len(gen_paths)} generations through the real "
              f"production codec ===")
        ok, detail = compare_session_files([Path(p) for p in gen_paths])
        chk.ok(ok, f"all {len(gen_paths)} generations are structurally IDENTICAL "
                   f"across three real fresh-process save->load->save cycles of "
                   f"the representative scenario"
               + (f" -- {detail}" if not ok else ""))

    finally:
        if proc is not None:
            quit_engine(port, proc)
        shutil.rmtree(tmpdir, ignore_errors=True)

    if args.skip_cross_probes:
        keys: list[str] = []
    else:
        # Already validated above (before ANY engine boot) to be a subset
        # of SELECTABLE_CROSS_REFERENCED_PROBE_KEYS.
        keys = requested_keys
        skipped_flaky = [k for k in FLAKY_CROSS_REFERENCED_PROBE_KEYS if k not in keys]
        if skipped_flaky:
            print(f"  (not run: {', '.join(skipped_flaky)} -- isolated but "
                  f"independently flaky per `tools/ci_probes.py --status`; "
                  f"list it explicitly in --cross-probe-keys to also run it)")
    run_cross_referenced_probes(chk, keys, args.cross_probe_jobs)

    print(f"\n{'PASS' if chk.failed == 0 else 'FAIL'}: {chk.failed} check(s) failed")
    return 0 if chk.failed == 0 else 1


if __name__ == "__main__":
    sys.exit(main())
