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
narrow or widen the set for local iteration on this script's OWN
scenario; ``--skip-cross-probes`` skips them entirely (loudly reported
as reduced coverage, never silently).

This sweep's OWN scenario always uses an isolated resource root, a
unique port, and a fresh process, and never touches the developer's
real saves/ (requirement 15). The 12 cross-referenced probes are NOT
all isolated the same way, though: only save_storage/
persistence_integrity/save_compat_migration pass their own
--resource-root -- those 3 are the DEFAULT ``--cross-probe-keys`` set.
The other 8 (chop/till/crop/plant/construction/power/
transactional_load/save_barrier) call engine.saveWorld/loadSave against
this repo's real saves/ directory (cleaned up afterward, but
transiently present) and are opt-in only, via
``--include-unisolated-probes`` or by listing their keys explicitly in
``--cross-probe-keys``. craft_bill_probe.py never touches saves/ at
all, but is ALSO excluded from the default set: it's independently
flaky (`tools/ci_probes.py --status` classifies it `manual-only
[flaky]`, confirmed live while validating this exclusion) for reasons
unrelated to persistence, so running it by default would make this
sweep spuriously fail on unrelated AI timing; list it explicitly in
``--cross-probe-keys`` to opt in anyway.

Usage:
  python3 tools/persistence_contract_sweep.py [--port 9278] \\
      [--world-size 64] [--seed 20260721] [--cross-probe-jobs 2] \\
      [--include-unisolated-probes]
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
    ("scripts/building_spawn.lua", 0.2),
)

# Domain probes ALREADY covering a category this sweep's own scenario
# does not reconstruct from scratch (requirement 14) -- actually RUN via
# tools/run_probes.py --only <these keys> --exact, not merely documented
# (round-1 review: an existence-only check proves nothing). Keys, not
# filenames, since that is what run_probes.py --exact matches against.
#
# These 12 probes are NOT all isolated the way this sweep's own scenario
# is (`make_isolated_root` + `--resource-root`, requirement 15): only
# save_storage/persistence_integrity/save_compat_migration pass their own
# `--resource-root`. The other 9 -- chop/till/crop/plant/construction/
# power/transactional_load/save_barrier/craft_bill -- call
# engine.saveWorld/loadSave straight against this repo's real saves/
# directory (a randomly-named or uuid-suffixed slot, cleaned up in a
# `finally`, but still transiently present there while the probe runs)
# -- EXCEPT craft_bill_probe.py, which never touches saves/ at all (its
# own docstring: "Not covered here: a real save/quit/restart/load
# round-trip") but IS independently flaky (`tools/ci_probes.py --status`
# classifies it `manual-only [flaky]`: "craft_job AI claim/work timing
# flakes run-to-run on CI" -- confirmed live while validating this fix,
# unrelated to anything this issue touches). Auto-running the
# saves/-touching 8 by default would violate requirement 15's "never
# touch the developer's real saves/" for a routine sweep invocation, and
# auto-running the flaky craft_bill by default would make this sweep
# spuriously fail on unrelated AI timing -- both are SEPARATED from the
# default set (round-3/round-4 review) rather than silently accepted.
ISOLATED_CROSS_REFERENCED_PROBE_KEYS = [
    "save_storage", "persistence_integrity", "save_compat_migration",
]
FLAKY_ISOLATED_PROBE_KEYS = ["craft_bill"]
UNISOLATED_CROSS_REFERENCED_PROBE_KEYS = [
    "chop", "till", "crop", "plant", "construction", "power",
    "transactional_load", "save_barrier",
]
CROSS_REFERENCED_PROBE_KEYS = (
    ISOLATED_CROSS_REFERENCED_PROBE_KEYS + FLAKY_ISOLATED_PROBE_KEYS
    + UNISOLATED_CROSS_REFERENCED_PROBE_KEYS
)


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

    return portal_bid, atk, tgt


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
                     default=",".join(ISOLATED_CROSS_REFERENCED_PROBE_KEYS),
                     help="comma-separated exact tools/run_probes.py probe keys to "
                          "actually run for requirement 11/13 coverage (default: only "
                          "the 3 that are isolated AND deterministic -- save_storage/"
                          "persistence_integrity/save_compat_migration; pass "
                          "--include-unisolated-probes for the 8 that touch this "
                          "repo's real saves/, or list keys explicitly to also add "
                          "craft_bill, which is isolated but independently flaky)")
    ap.add_argument("--cross-probe-jobs", type=int, default=2,
                     help="run_probes.py --jobs for the cross-referenced probes")
    ap.add_argument("--include-unisolated-probes", action="store_true",
                     help="also run chop/till/crop/plant/construction/power/"
                          "transactional_load/save_barrier -- these are NOT "
                          "isolated (they call engine.saveWorld/loadSave against "
                          "this repo's real saves/ directory, requirement 15), so "
                          "this is opt-in, not the default; only pass it if you "
                          "understand it transiently touches real project state")
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
            loaded = send(port, f"return engine.loadSave('{prev_slot}')")
            chk.ok(loaded.strip() == "true",
                   f"engine.loadSave('{prev_slot}') accepted (got {loaded!r})")
            published, status = wait_load_published(port, seconds=300)
            chk.ok(published, f"load transaction published (status={status})")
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

            saved = send(port, f"return engine.saveWorld('{PAGE}', '{next_slot}')")
            chk.ok(saved.strip() == "true",
                   f"re-saving to '{next_slot}' succeeded (got {saved!r})")
            next_path = os.path.join(root, "saves", next_slot, "world.synworld")
            chk.ok(wait_for_file(next_path), f"save file appeared at {next_path}")
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
        keys = [k for k in args.cross_probe_keys.split(",") if k.strip()]
        if args.include_unisolated_probes:
            keys += [k for k in UNISOLATED_CROSS_REFERENCED_PROBE_KEYS
                     if k not in keys]
        else:
            skipped_unisolated = [k for k in UNISOLATED_CROSS_REFERENCED_PROBE_KEYS
                                   if k not in keys]
            if skipped_unisolated:
                print(f"  (not run: {', '.join(skipped_unisolated)} -- these are NOT "
                      f"isolated from this repo's real saves/ directory; pass "
                      f"--include-unisolated-probes to also run them)")
        skipped_flaky = [k for k in FLAKY_ISOLATED_PROBE_KEYS if k not in keys]
        if skipped_flaky:
            print(f"  (not run: {', '.join(skipped_flaky)} -- isolated but "
                  f"independently flaky per `tools/ci_probes.py --status`; "
                  f"list it explicitly in --cross-probe-keys to also run it)")
    run_cross_referenced_probes(chk, keys, args.cross_probe_jobs)

    print(f"\n{'PASS' if chk.failed == 0 else 'FAIL'}: {chk.failed} check(s) failed")
    return 0 if chk.failed == 0 else 1


if __name__ == "__main__":
    sys.exit(main())
