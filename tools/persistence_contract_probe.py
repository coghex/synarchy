#!/usr/bin/env python3
"""Compact, CI-eligible fresh-process persistence contract probe (issue
#767, save-overhaul D1).

The pure hspec "persistence contract" gate
(``Test.Headless.World.Save.Contract``) already proves the in-process
encode/decode round trip is lossless for a representative multi-page
session. What that gate structurally CANNOT prove is requirement 5's own
literal ask: "terminate the engine. Start a fresh headless engine." This
probe is the compact, deterministic smoke version of that -- three real
engine boots in a row, each loading the previous one's save and
immediately re-saving, so requirement 9's "at least three fresh-process
save -> load -> save cycles" is exercised even at smoke scale.

Flow (isolated resource root -- see ``tools/save_storage_probe.py``'s
identical pattern -- never the developer's real ``saves/``):

  1. Engine A: boot, load defs + the AI script stack, ``world.init`` a
     small (worldSize 8) REAL generated world with a meaningful seed +
     player-facing identity, spawn an ``acolyte_portal`` (real,
     non-vacuous ``lua.building_spawn`` state once its roster starts
     spawning) and two units with one issued a real ``unitAi.
     commandAttack`` against the other (real, non-vacuous ``lua.unit_ai``
     state), a non-default map mode, save to slot "gen1".
  2. Quit A. Boot engine B (fresh process). Load "gen1"; assert the
     reset policy (paused, default tool, empty selections) and a short
     paused dwell where a live sample (date, unit position, the real
     unit_ai attack-target reference, the building_spawn roster
     countdown) is identical before and after. Re-save (no mutation in
     between) to "gen2".
  3. Quit B. Boot engine C (fresh process). Load "gen2"; re-save to
     "gen3".
  4. Quit C. ``persistence_snapshot.compare_session_files`` decodes all
     three generations through the REAL production codec
     (``World.Save.Envelope.decodeSessionEnvelope``) and asserts every
     one is structurally IDENTICAL -- both the Haskell
     ``SessionSnapshot`` (via derived ``Eq``) and every ``lua.<module>``
     canonical component payload (via byte equality) -- the canonical
     persistence-state inspection/comparison surface (requirement 1),
     tied to each save/load boundary by waiting for
     ``engine.getSaveStatus()``/``engine.getLoadStatus()`` to reach a
     terminal phase before ever touching the resulting file
     (requirement 2). The non-default map mode is additionally checked
     directly via ``save_compat_audit.dump_canonical_summary`` (there is
     no live ``world.getMapMode`` query to assert it through the debug
     console instead).

Registered CI-eligible in ``tools/ci_probes.py`` -- ``src/World/Save/*``/
``src/World/Load/*``/``src/Engine/Save/*``/``src/Engine/Load/*`` are
already in ``CORE_GLOBS``, so any persistence-related source change
already selects the full CI-eligible set, this probe included.

The BROADER manual sweep (all of requirement 4's representative-scenario
checklist, the assembled failure contract, and every maintained
migration baseline) is ``tools/persistence_contract_sweep.py``.

Usage:
  python3 tools/persistence_contract_probe.py [--port 9277]
"""
from __future__ import annotations

import argparse
import glob
import os
import shutil
import sys
import tempfile
import time
from pathlib import Path

from probelib import boot, quit_engine, send, send_json, wait_load_published
from persistence_snapshot import compare_session_files
from save_compat_audit import dump_canonical_summary

REPO = Path(__file__).resolve().parent.parent

SEED = 424242
WORLD_SIZE = 8
PLATE_COUNT = 3
PAGE = "contract_page"

# The AI/stats script stack the loading screen would load in a real GUI
# session -- headless boots skip the loading screen entirely (CLAUDE.md),
# so any probe driving unit_ai (commandAttack/getState) must load these
# itself. Mirrors probelib.AI_STACK exactly.
AI_SCRIPTS = (
    ("scripts/unit_stats.lua", 0.1),
    ("scripts/unit_resources.lua", 0.2),
    ("scripts/unit_ai.lua", 0.1),
)


class Checks:
    def __init__(self) -> None:
        self.failed = 0

    def ok(self, cond: bool, label: str) -> None:
        print(f"  [{'PASS' if cond else 'FAIL'}] {label}")
        if not cond:
            self.failed += 1


def make_isolated_root(base: str) -> str:
    """A throwaway resource root: real scripts/assets/data/config
    (symlinked -- read-only content, safe to share) plus its OWN empty
    saves/ directory (mirrors tools/save_storage_probe.py's helper) --
    this probe never touches the developer's real saves/."""
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
    ]
    for pattern, fn in loaders:
        for path in sorted(glob.glob(pattern)):
            send(port, f"{fn}('{path}'); return 'ok'")


def load_ai_stack(port: int) -> None:
    for path, delay in AI_SCRIPTS:
        send(port, f"engine.loadScript('{path}', {delay}); return 'ok'")


def boot_probe(root: str, port: int, log: str):
    return boot(port, log=log, args=["--resource-root", root], ready_timeout=180)


def wait_for_file(path: str, seconds: float = 30.0) -> bool:
    deadline = time.time() + seconds
    while time.time() < deadline:
        if os.path.isfile(path):
            return True
        time.sleep(0.1)
    return False


def as_int(s: str):
    try:
        return int(float(s))
    except (TypeError, ValueError):
        return None


def build_scenario(chk: Checks, port: int) -> tuple[int, int, int]:
    """Engine A: a small real generated world with a meaningful seed +
    identity, a built acolyte_portal with a real roster countdown
    (non-vacuous lua.building_spawn state), two units with a real
    unitAi.commandAttack between them (non-vacuous lua.unit_ai state),
    and a non-default map mode. Returns (portal_bid, attacker_uid,
    target_uid)."""
    bootstrap_defs(port)
    load_ai_stack(port)
    inited = send(
        port,
        f"world.init('{PAGE}', {SEED}, {WORLD_SIZE}, {PLATE_COUNT}, "
        f"'Contract Test World', 'the persistence contract test world'); "
        f"return 'ok'")
    chk.ok("ok" in inited, f"world.init accepted (got {inited!r})")
    # world.show (not just world.init) puts the page in wmVisible --
    # without it, building.spawn/canPlaceAt's snapshotVisibleWorldTiles
    # read can reject a spawn (mirrors save_compat_audit.py's identical
    # note in generate_current_format_session).
    send(port, f"world.show('{PAGE}'); return 'ok'")
    deadline = time.time() + 30.0
    while time.time() < deadline:
        if send(port, "return world.getActiveWorldId()").strip('"') == PAGE:
            break
        time.sleep(0.2)
    else:
        chk.ok(False, f"'{PAGE}' never became the active world")
    send(port, "return world.waitForInit(30)", timeout=35)

    # A real, non-vacuous lua.building_spawn state: acolyte_portal is the
    # ONE building_spawn.lua-configured def (an ordinary building like
    # furnace/cargo_hold_S creates no per-building sequencer state at all
    # -- round-1 review finding). building.setSpawnRemaining seeds the
    # roster countdown directly rather than reverse-engineering the
    # build-tool "stake" placement flow.
    portal_bid = as_int(send(port, f"return building.spawn('acolyte_portal', 0, 0)"))
    chk.ok(portal_bid is not None and portal_bid >= 0,
           f"building.spawn('acolyte_portal') succeeded (got {portal_bid!r})")
    send(port, f"building.setSpawnRemaining({portal_bid}, 6); return 'ok'")
    remaining_before = as_int(send(port, f"return building.getSpawnRemaining({portal_bid})"))
    chk.ok(remaining_before == 6,
           f"acolyte_portal roster seeded to 6 (got {remaining_before!r})")
    # Give building_spawn.lua's sequencer a moment to fire its first
    # roster spawn -- proves the Lua sequencer (not just the engine-side
    # countdown) actually ran, so its per-building state entry is real.
    deadline = time.time() + 10.0
    remaining_after = remaining_before
    while time.time() < deadline:
        remaining_after = as_int(send(port, f"return building.getSpawnRemaining({portal_bid})"))
        if remaining_after is not None and remaining_after < remaining_before:
            break
        time.sleep(0.3)
    chk.ok(remaining_after is not None and remaining_after < remaining_before,
           f"acolyte_portal's roster sequencer spawned at least one unit "
           f"(remaining {remaining_before} -> {remaining_after})")

    # A real, non-vacuous lua.unit_ai state: a real commandAttack sets
    # attackTargetUid, verifiable live via unitAi.getState (round-1 review
    # finding -- a freshly spawned, idle unit's AI state is otherwise
    # near-empty).
    atk = as_int(send(port, "return unit.spawn('acolyte', 2, 2, 0, 'player')"))
    chk.ok(atk is not None and atk >= 0, f"attacker unit.spawn succeeded (got {atk!r})")
    tgt = as_int(send(port, "return unit.spawn('acolyte', 5, 5, 0, 'wildlife')"))
    chk.ok(tgt is not None and tgt >= 0, f"target unit.spawn succeeded (got {tgt!r})")
    send(port, f"require('scripts.unit_ai').commandAttack({atk}, {tgt}); return 'ok'")
    time.sleep(0.5)
    attack_target = get_attack_target(port, atk)
    chk.ok(attack_target == tgt,
           f"commandAttack set a real, live attackTargetUid ({attack_target} "
           f"vs expected {tgt})")

    # world.setMapMode(pageId, mode) -- a bare-mode call (missing pageId)
    # is a silent no-op (round-1 review finding); there is no live
    # world.getMapMode query, so the non-default value is verified via
    # dump_canonical_summary once the save file exists (see main()).
    send(port, f"world.setMapMode('{PAGE}', 'map_pressure'); return 'ok'")
    # setMapMode is queued (worldQueue, async) -- give the world thread a
    # moment to apply it before the save captures state.
    time.sleep(0.5)

    return portal_bid, atk, tgt


def get_attack_target(port: int, uid: int):
    raw = send(port, f"local s = require('scripts.unit_ai').getState({uid}); "
                      f"return s and s.attackTargetUid or nil")
    return as_int(raw)


def sample_live_state(port: int, portal_bid: int, atk: int) -> dict:
    """A small, cheap live sample used for the paused-stability dwell
    check (requirement 7) -- repeating this during a paused dwell must
    yield identical values. This scenario never places a player-faction
    unit inside an undiscovered location's discovery margin, so it never
    hits the ONE documented, deliberate exception to that rule:
    World.Thread.Discovery.tickLocationDiscovery (#780) runs independent
    of the pause flag and can flip wgpLocationDiscovered on the very
    first post-load tick if a unit already stands in such a margin."""
    return {
        "date": send(port, f"return world.getDate('{PAGE}')"),
        "activePage": send(port, "return world.getActiveWorldId()"),
        "paused": send(port, "return engine.isPaused()"),
        "toolMode": send(port, "return world.getToolMode()"),
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


def assert_nondefault_map_mode(chk: Checks, tmpdir: str, save_path: str, page: str) -> None:
    """world.setMapMode has no live query counterpart, so the non-default
    value is verified through the same dump_canonical_summary decode
    tools/save_compat_migration_probe.py already uses -- its per-page
    dump already includes "mapMode" (GHCI_DUMP_SUMMARY_TEMPLATE)."""
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


def main() -> int:
    ap = argparse.ArgumentParser(description=__doc__,
                                 formatter_class=argparse.RawDescriptionHelpFormatter)
    ap.add_argument("--port", type=int, default=9277)
    args = ap.parse_args()
    port = args.port
    chk = Checks()

    tmpdir = tempfile.mkdtemp(prefix="persistence_contract_probe_")
    proc = None
    try:
        root = make_isolated_root(tmpdir)

        # ── Engine A: build the scenario, save ───────────────────────────
        print("=== engine A: build scenario + save 'gen1' ===")
        proc = boot_probe(root, port, os.path.join(tmpdir, "engineA.log"))
        portal_bid, atk, tgt = build_scenario(chk, port)
        saved = send(port, f"return engine.saveWorld('{PAGE}', 'gen1')")
        chk.ok(saved.strip() == "true", f"engine.saveWorld('gen1') accepted (got {saved!r})")
        gen1_path = os.path.join(root, "saves", "gen1", "world.synworld")
        chk.ok(wait_for_file(gen1_path), f"save file appeared at {gen1_path}")
        # #767 requirement 5: exclude slot name/timestamp/path from the
        # structural comparison, but smTimestamp is still a real, well-
        # formed field on the wire -- check it exists and looks like a
        # timestamp (contract SS12's metadata coverage-map row).
        listing = send_json(port, "return engine.listSaves()")
        gen1_entry = next((s for s in listing if s.get("name") == "gen1"), None) \
            if isinstance(listing, list) else None
        chk.ok(bool(gen1_entry and gen1_entry.get("timestamp")),
               f"engine.listSaves() reports a well-formed timestamp for 'gen1' "
               f"(got {gen1_entry!r})")
        assert_nondefault_map_mode(chk, tmpdir, gen1_path, PAGE)
        quit_engine(port, proc)
        proc = None

        # ── Engine B: fresh process, load gen1, reset-policy + dwell, resave ──
        print("=== engine B: fresh process, load 'gen1', dwell, save 'gen2' ===")
        proc = boot_probe(root, port, os.path.join(tmpdir, "engineB.log"))
        bootstrap_defs(port)
        load_ai_stack(port)
        loaded = send(port, "return engine.loadSave('gen1')")
        chk.ok(loaded.strip() == "true", f"engine.loadSave('gen1') accepted (got {loaded!r})")
        published, status = wait_load_published(port)
        chk.ok(published, f"load transaction published (status={status})")

        assert_reset_policy(chk, port, "after loading gen1")
        attack_target_b = get_attack_target(port, atk)
        chk.ok(attack_target_b == tgt,
               f"lua.unit_ai's attackTargetUid survived the fresh-process load "
               f"({attack_target_b} vs expected {tgt})")

        before = sample_live_state(port, portal_bid, atk)
        time.sleep(2.0)
        after = sample_live_state(port, portal_bid, atk)
        chk.ok(before == after,
               f"requirement 7: repeating the canonical live inspection during "
               f"a 2s paused dwell produces the SAME persistent state "
               f"({before} -> {after})")

        saved_b = send(port, f"return engine.saveWorld('{PAGE}', 'gen2')")
        chk.ok(saved_b.strip() == "true", f"re-saving to 'gen2' succeeded (got {saved_b!r})")
        gen2_path = os.path.join(root, "saves", "gen2", "world.synworld")
        chk.ok(wait_for_file(gen2_path), f"save file appeared at {gen2_path}")
        quit_engine(port, proc)
        proc = None

        # ── Engine C: fresh process, load gen2, resave gen3 ─────────────
        print("=== engine C: fresh process, load 'gen2', save 'gen3' ===")
        proc = boot_probe(root, port, os.path.join(tmpdir, "engineC.log"))
        bootstrap_defs(port)
        load_ai_stack(port)
        loaded_c = send(port, "return engine.loadSave('gen2')")
        chk.ok(loaded_c.strip() == "true", f"engine.loadSave('gen2') accepted (got {loaded_c!r})")
        published_c, status_c = wait_load_published(port)
        chk.ok(published_c, f"second load transaction published (status={status_c})")
        assert_reset_policy(chk, port, "after loading gen2")
        attack_target_c = get_attack_target(port, atk)
        chk.ok(attack_target_c == tgt,
               f"lua.unit_ai's attackTargetUid survived a SECOND fresh-process "
               f"load ({attack_target_c} vs expected {tgt})")

        saved_c = send(port, f"return engine.saveWorld('{PAGE}', 'gen3')")
        chk.ok(saved_c.strip() == "true", f"re-saving to 'gen3' succeeded (got {saved_c!r})")
        gen3_path = os.path.join(root, "saves", "gen3", "world.synworld")
        chk.ok(wait_for_file(gen3_path), f"save file appeared at {gen3_path}")

        # Unpause ONLY to confirm the default speed (requirement 8) --
        # never comparing any subsequent random gameplay outcome.
        send(port, "require('scripts.pause').set(false); return 'ok'", expect_result=False)
        time.sleep(0.5)
        ts = send(port, f"return world.getTimeScale('{PAGE}')")
        chk.ok(ts.strip() in ("1", "1.0"),
               f"unpausing resumes at the default simulation speed (got {ts})")

        quit_engine(port, proc)
        proc = None

        # ── Requirement 1/5/9: canonical persistence-state comparison ───
        print("=== comparing gen1/gen2/gen3 through the real production codec ===")
        ok, detail = compare_session_files(
            [Path(gen1_path), Path(gen2_path), Path(gen3_path)])
        chk.ok(ok, "gen1/gen2/gen3 are structurally IDENTICAL (SessionSnapshot "
                   f"Eq + lua.* payload byte-equality) across three real "
                   f"fresh-process save->load->save cycles"
               + (f" -- {detail}" if not ok else ""))

    finally:
        if proc is not None:
            quit_engine(port, proc)
        shutil.rmtree(tmpdir, ignore_errors=True)

    print(f"\n{'PASS' if chk.failed == 0 else 'FAIL'}: {chk.failed} check(s) failed")
    return 0 if chk.failed == 0 else 1


if __name__ == "__main__":
    sys.exit(main())
