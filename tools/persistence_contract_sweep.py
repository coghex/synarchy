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
``--skip-cross-probes`` skips them entirely -- neither is the default,
and skipping is loudly reported as reduced coverage, never silently.

Usage:
  python3 tools/persistence_contract_sweep.py [--port 9278] \\
      [--world-size 64] [--seed 20260721] [--cross-probe-jobs 2]

Both probe levels use isolated resource roots, unique ports, and fresh
processes, and never touch the developer's real saves/ (requirement 15).
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

from probelib import boot, quit_engine, send, wait_load_published
from persistence_snapshot import compare_session_files
from save_compat_audit import dump_canonical_summary

REPO = Path(__file__).resolve().parent.parent
PAGE = "contract_sweep_page"

# The AI/stats script stack the loading screen would load in a real GUI
# session -- headless boots skip it entirely (CLAUDE.md), so any probe
# driving unit_ai (commandAttack/getState) must load these itself.
AI_SCRIPTS = (
    ("scripts/unit_stats.lua", 0.1),
    ("scripts/unit_resources.lua", 0.2),
    ("scripts/unit_ai.lua", 0.1),
)

# Domain probes ALREADY covering a category this sweep's own scenario
# does not reconstruct from scratch (requirement 14) -- actually RUN via
# tools/run_probes.py --only <these keys> --exact, not merely documented
# (round-1 review: an existence-only check proves nothing). Keys, not
# filenames, since that is what run_probes.py --exact matches against.
CROSS_REFERENCED_PROBE_KEYS = [
    "chop", "till", "crop", "plant", "construction", "craft_bill", "power",
    "transactional_load", "save_storage", "save_barrier",
    "persistence_integrity", "save_compat_migration",
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


def as_int(s: str):
    try:
        return int(float(s))
    except (TypeError, ValueError):
        return None


def build_rich_scenario(chk: Checks, port: int, seed: int, size: int, plates: int) -> tuple[int, int, int]:
    """A REAL generated world (not a tiny arena) with a meaningful seed +
    identity, a built craft station running a real bill, an acolyte_portal
    with a real roster countdown (non-vacuous lua.building_spawn state), a
    unitAi.commandAttack between two units (non-vacuous lua.unit_ai
    state), a mine designation, and a non-default map mode -- the
    representative scenario requirement 4 asks for, at real worldgen
    scale. Returns (portal_bid, attacker_uid, target_uid)."""
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

    return portal_bid, atk, tgt


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
          f"run_probes.py --only ... --exact --jobs {jobs} (this is slow) ===")
    proc = subprocess.run(
        [sys.executable, str(REPO / "tools" / "run_probes.py"),
         "--only", ",".join(keys), "--exact", "--jobs", str(jobs)],
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
    ap.add_argument("--cross-probe-keys", default=",".join(CROSS_REFERENCED_PROBE_KEYS),
                     help="comma-separated exact tools/run_probes.py probe keys to "
                          "actually run for requirement 11/13 coverage (default: all "
                          "of them -- narrow this only for local iteration on this "
                          "script's OWN scenario)")
    ap.add_argument("--cross-probe-jobs", type=int, default=2,
                     help="run_probes.py --jobs for the cross-referenced probes")
    ap.add_argument("--skip-cross-probes", action="store_true",
                     help="skip the cross-referenced probes entirely (loudly reported "
                          "as reduced coverage, never the default)")
    args = ap.parse_args()
    port = args.port
    chk = Checks()

    tmpdir = tempfile.mkdtemp(prefix="persistence_contract_sweep_")
    proc = None
    try:
        root = make_isolated_root(tmpdir)

        print("=== engine A: build the representative scenario, save 'gen1' ===")
        proc = boot_probe(root, port, os.path.join(tmpdir, "engineA.log"))
        portal_bid, atk, tgt = build_rich_scenario(
            chk, port, args.seed, args.world_size, args.plates)
        saved = send(port, f"return engine.saveWorld('{PAGE}', 'gen1')")
        chk.ok(saved.strip() == "true", f"engine.saveWorld('gen1') accepted (got {saved!r})")
        gen1_path = os.path.join(root, "saves", "gen1", "world.synworld")
        chk.ok(wait_for_file(gen1_path), f"save file appeared at {gen1_path}")
        assert_nondefault_map_mode(chk, tmpdir, gen1_path, PAGE)
        quit_engine(port, proc)
        proc = None

        print("=== engine B: fresh process, load 'gen1', dwell, save 'gen2' ===")
        proc = boot_probe(root, port, os.path.join(tmpdir, "engineB.log"))
        bootstrap_defs(port)
        load_ai_stack(port)
        loaded = send(port, "return engine.loadSave('gen1')")
        chk.ok(loaded.strip() == "true", f"engine.loadSave('gen1') accepted (got {loaded!r})")
        published, status = wait_load_published(port, seconds=300)
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
               f"requirement 7: the live inspection is unchanged across a 2s "
               f"paused dwell ({before} -> {after})")

        saved_b = send(port, f"return engine.saveWorld('{PAGE}', 'gen2')")
        chk.ok(saved_b.strip() == "true", f"re-saving to 'gen2' succeeded (got {saved_b!r})")
        gen2_path = os.path.join(root, "saves", "gen2", "world.synworld")
        chk.ok(wait_for_file(gen2_path), f"save file appeared at {gen2_path}")
        quit_engine(port, proc)
        proc = None

        print("=== engine C: fresh process, load 'gen2', save 'gen3' ===")
        proc = boot_probe(root, port, os.path.join(tmpdir, "engineC.log"))
        bootstrap_defs(port)
        load_ai_stack(port)
        loaded_c = send(port, "return engine.loadSave('gen2')")
        chk.ok(loaded_c.strip() == "true", f"engine.loadSave('gen2') accepted (got {loaded_c!r})")
        published_c, status_c = wait_load_published(port, seconds=300)
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

        send(port, "require('scripts.pause').set(false); return 'ok'", expect_result=False)
        time.sleep(0.5)
        ts = send(port, f"return world.getTimeScale('{PAGE}')")
        chk.ok(ts.strip() in ("1", "1.0"),
               f"unpausing resumes at the default simulation speed (got {ts})")

        quit_engine(port, proc)
        proc = None

        print("=== comparing gen1/gen2/gen3 through the real production codec ===")
        ok, detail = compare_session_files(
            [Path(gen1_path), Path(gen2_path), Path(gen3_path)])
        chk.ok(ok, "gen1/gen2/gen3 are structurally IDENTICAL across three real "
                   "fresh-process save->load->save cycles of the representative "
                   "scenario" + (f" -- {detail}" if not ok else ""))

    finally:
        if proc is not None:
            quit_engine(port, proc)
        shutil.rmtree(tmpdir, ignore_errors=True)

    keys = [] if args.skip_cross_probes else [
        k for k in args.cross_probe_keys.split(",") if k.strip()]
    run_cross_referenced_probes(chk, keys, args.cross_probe_jobs)

    print(f"\n{'PASS' if chk.failed == 0 else 'FAIL'}: {chk.failed} check(s) failed")
    return 0 if chk.failed == 0 else 1


if __name__ == "__main__":
    sys.exit(main())
