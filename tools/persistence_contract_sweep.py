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
final behavior". Those remain the authoritative, already-comprehensive
coverage for their own domains (see the cross-reference table this
script prints at the end, and
``docs/persistence_state_inventory.md``'s SS12 "Focused test" column):

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

Usage:
  python3 tools/persistence_contract_sweep.py [--port 9278] \\
      [--world-size 64] [--seed 20260721]

Both probe levels use isolated resource roots, unique ports, and fresh
processes, and never touch the developer's real saves/ (requirement 15).
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

from probelib import boot, quit_engine, send, wait_load_published
from persistence_snapshot import compare_session_files

REPO = Path(__file__).resolve().parent.parent
PAGE = "contract_sweep_page"

# Domain probes ALREADY covering a category this sweep's own scenario
# does not reconstruct from scratch (requirement 14: don't duplicate an
# already-comprehensive real-process gate). Documentation only -- run
# them directly (`python3 tools/<probe>.py`) when touching that domain;
# this sweep does not invoke them as subprocesses, since each already
# pays its own multi-minute engine-boot cost and is independently
# registered in tools/ci_probes.py's MANUAL_ONLY_REASONS.
CROSS_REFERENCED_PROBES = [
    ("chop_probe.py", "chop designations (wsChopDesignationsRef)"),
    ("till_probe.py", "till designations (wsTillDesignationsRef)"),
    ("crop_probe.py", "crop plots (wsCropPlotsRef)"),
    ("plant_probe.py", "plant designations (wsPlantDesignationsRef)"),
    ("construction_probe.py", "construct designations + build AI"),
    ("craft_bill_probe.py", "craft bills at full domain depth (queue/claim/progress)"),
    ("power_probe.py", "power nodes + network balance at full domain depth"),
    ("transactional_load_probe.py", "whole-session replacement (requirement 10)"),
    ("save_storage_probe.py", "interrupted write / corrupt envelope / restart-and-select (requirement 11)"),
    ("save_barrier_probe.py", "disk write-failure surfacing (requirement 11)"),
    ("persistence_integrity_probe.py", "dangling Haskell/Lua reference tolerance (requirement 11)"),
    ("save_compat_migration_probe.py", "every maintained migration baseline (requirement 13)"),
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


def build_rich_scenario(chk: Checks, port: int, seed: int, size: int, plates: int) -> None:
    """A REAL generated world (not a tiny arena) with a meaningful seed +
    identity, a built craft station running a real bill, a spawned unit,
    a mine designation, and a non-default map mode -- the representative
    scenario requirement 4 asks for, at real worldgen scale."""
    bootstrap_defs(port)
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
    uid = as_int(send(port, "return unit.spawn('acolyte', 2, 2, 0, 'player')"))
    chk.ok(uid is not None and uid >= 0, f"unit.spawn succeeded (got {uid!r})")

    if bid is not None:
        bill = as_int(send(port, f"local id,err = craft.addBill({bid}, 'smelt_steel_lignite'); return id"))
        chk.ok(bill is not None and bill >= 0,
               f"craft.addBill('smelt_steel_lignite') on the built furnace succeeded (got {bill!r})")

    mined = send(port, f"world.designateMine('{PAGE}', 1, 1, 2, 2); return 'ok'")
    chk.ok("ok" in mined, f"world.designateMine accepted (got {mined!r})")
    send(port, "world.setMapMode('map_pressure'); return 'ok'")


def sample_live_state(port: int) -> dict:
    """See persistence_contract_probe.py's identical helper for why this
    scenario never trips the one documented pause-independent mutator
    (#780 location discovery)."""
    return {
        "date": send(port, f"return world.getDate('{PAGE}')"),
        "activePage": send(port, "return world.getActiveWorldId()"),
        "paused": send(port, "return engine.isPaused()"),
        "toolMode": send(port, "return world.getToolMode()"),
        "mineDesignationCount": send(port, f"return world.getMineDesignationCount('{PAGE}')"),
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


def main() -> int:
    ap = argparse.ArgumentParser(description=__doc__,
                                 formatter_class=argparse.RawDescriptionHelpFormatter)
    ap.add_argument("--port", type=int, default=9278)
    ap.add_argument("--seed", type=int, default=20260721)
    ap.add_argument("--world-size", type=int, default=64)
    ap.add_argument("--plates", type=int, default=5)
    args = ap.parse_args()
    port = args.port
    chk = Checks()

    tmpdir = tempfile.mkdtemp(prefix="persistence_contract_sweep_")
    proc = None
    try:
        root = make_isolated_root(tmpdir)

        print("=== engine A: build the representative scenario, save 'gen1' ===")
        proc = boot_probe(root, port, os.path.join(tmpdir, "engineA.log"))
        build_rich_scenario(chk, port, args.seed, args.world_size, args.plates)
        saved = send(port, f"return engine.saveWorld('{PAGE}', 'gen1')")
        chk.ok(saved.strip() == "true", f"engine.saveWorld('gen1') accepted (got {saved!r})")
        gen1_path = os.path.join(root, "saves", "gen1", "world.synworld")
        chk.ok(wait_for_file(gen1_path), f"save file appeared at {gen1_path}")
        quit_engine(port, proc)
        proc = None

        print("=== engine B: fresh process, load 'gen1', dwell, save 'gen2' ===")
        proc = boot_probe(root, port, os.path.join(tmpdir, "engineB.log"))
        bootstrap_defs(port)
        loaded = send(port, "return engine.loadSave('gen1')")
        chk.ok(loaded.strip() == "true", f"engine.loadSave('gen1') accepted (got {loaded!r})")
        published, status = wait_load_published(port, seconds=300)
        chk.ok(published, f"load transaction published (status={status})")
        assert_reset_policy(chk, port, "after loading gen1")

        before = sample_live_state(port)
        time.sleep(2.0)
        after = sample_live_state(port)
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
        loaded_c = send(port, "return engine.loadSave('gen2')")
        chk.ok(loaded_c.strip() == "true", f"engine.loadSave('gen2') accepted (got {loaded_c!r})")
        published_c, status_c = wait_load_published(port, seconds=300)
        chk.ok(published_c, f"second load transaction published (status={status_c})")
        assert_reset_policy(chk, port, "after loading gen2")

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

    print("\n-- domains covered by their own already-comprehensive probes "
          "(not re-run here; requirement 14) --")
    for name, covers in CROSS_REFERENCED_PROBES:
        exists = (REPO / "tools" / name).exists()
        print(f"  {'[present]' if exists else '[MISSING]'} tools/{name}: {covers}")
        if not exists:
            chk.failed += 1

    print(f"\n{'PASS' if chk.failed == 0 else 'FAIL'}: {chk.failed} check(s) failed")
    return 0 if chk.failed == 0 else 1


if __name__ == "__main__":
    sys.exit(main())
