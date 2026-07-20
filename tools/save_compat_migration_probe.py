#!/usr/bin/env python3
"""Fresh-process save-compatibility migration probe (issue #766,
save-overhaul C4).

Real-engine, real-restart coverage of the ONE thing the pure hspec gates
("save components", "save migrations") cannot prove: that every REAL
tracked complete-session fixture on disk actually loads through the
normal #763 whole-session transaction, publishes, survives a paused
dwell, and re-saves/reloads in current format across a real process
restart -- end to end, not merely "the pure decode function returns
Right".

Iterates EVERY "complete-session" fixture declared across
docs/save_compat/manifest.json's baselines (currently the legacy B1
baseline's fixture AND the #764 raw-to-typed-reference v1 fixture,
which is a MODERN envelope decoded through the ordinary registry-driven
path, never the legacy fallback -- proving that baseline's real-engine
round trip too, not just the B1 one).

Flow per fixture (isolated resource root -- never touches a real
player's saves, see make_isolated_root):
  1. Place the fixture's bytes directly at saves/<slot>/world.synworld.
  2. Boot engine A, engine.loadSave(slot), wait for the load transaction
     to publish through the normal #763 staging/publish path (proving
     the migrated/decoded session is NOT special-cased at that
     boundary).
  3. Assert the resulting session's structural state matches the
     fixture's OWN expectedCanonicalSummary: active page, paused, and a
     short paused dwell advances no gameplay date.
  4. Save the session under a NEW slot name -- the only way to observe
     "this now writes current-format bytes going forward" without
     inspecting cereal internals directly.
  5. Quit engine A, boot a FRESH engine B (same isolated root),
     engine.loadSave the NEW current-format slot, wait for publish.
  6. Compare structural state across the restart: active page
     unchanged, still begins paused.
  7. Unpause (via scripts.pause, the paired
     engine.setPaused+world.setTimeScale contract every other save/load
     probe already uses) ONLY to confirm the default time scale --
     never comparing any subsequent random gameplay outcome.

Usage:
  python3 tools/save_compat_migration_probe.py [--port 9276]

Exit 0 = every check above passed, for every declared complete-session
fixture.
"""
from __future__ import annotations

import argparse
import glob
import json
import os
import shutil
import sys
import tempfile
import time
from pathlib import Path

from probelib import boot, quit_engine, send, wait_load_published

REPO = Path(__file__).resolve().parent.parent
MANIFEST_PATH = REPO / "docs" / "save_compat" / "manifest.json"


def bootstrap_defs(port: int) -> None:
    """Load the defs a headless boot skips (no loading screen) but the
    load path's content-reference validation (issue #760 requirement 9)
    demands -- mirrors tools/multiworld_save_probe.py's helper, plus
    recipes (this probe's #764 fixture carries a real craft bill)."""
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


def make_isolated_root(base: str) -> str:
    """A throwaway resource root: real scripts/assets/data/config
    (symlinked -- read-only content, safe to share) plus its OWN empty
    saves/ directory, mirroring tools/save_storage_probe.py's helper."""
    root = os.path.join(base, "root")
    os.makedirs(root, exist_ok=True)
    for family in ("scripts", "assets", "data", "config"):
        target = os.path.join(root, family)
        if not os.path.exists(target):
            os.symlink(os.path.join(REPO, family), target)
    os.makedirs(os.path.join(root, "saves"), exist_ok=True)
    return root


def declared_complete_session_fixtures() -> list[dict]:
    """Every "complete-session" fixture declared across every baseline in
    the manifest, each paired with its OWN expected canonical summary
    (active page, in particular, differs per fixture -- the B1 baseline's
    is "main_world", the #764 baseline's is "page1")."""
    manifest = json.loads(MANIFEST_PATH.read_text(encoding="utf-8"))
    out = []
    for baseline in manifest["baselines"]:
        for fixture in baseline.get("fixtures", []):
            if fixture.get("kind") != "complete-session":
                continue
            summary_path = fixture.get("expectedCanonicalSummary")
            if not summary_path:
                sys.exit(f"FAIL: complete-session fixture "
                          f"'{fixture.get('id')}' has no "
                          f"expectedCanonicalSummary")
            summary = json.loads((REPO / summary_path).read_text(encoding="utf-8"))
            out.append({
                "baseline_id": baseline["id"],
                "fixture_id": fixture["id"],
                "path": REPO / fixture["path"],
                "active_page": summary["activePage"],
            })
    if not out:
        sys.exit("FAIL: docs/save_compat/manifest.json declares no "
                  "complete-session fixtures")
    return out


def boot_probe(root: str, port: int, log: str):
    return boot(port, log=log, args=["--resource-root", root], ready_timeout=180)


class Checks:
    def __init__(self) -> None:
        self.failed = 0

    def ok(self, cond: bool, label: str) -> None:
        print(f"  [{'PASS' if cond else 'FAIL'}] {label}")
        if not cond:
            self.failed += 1


def run_one_fixture(chk: Checks, port: int, fixture: dict) -> None:
    fixture_bytes = fixture["path"].read_bytes()
    active_page = fixture["active_page"]
    legacy_slot = f"probe_{fixture['fixture_id']}_legacy"
    resaved_slot = f"probe_{fixture['fixture_id']}_resaved"
    tmpdir = tempfile.mkdtemp(prefix="save_compat_migration_probe_")
    logA = f"/tmp/save_compat_migration_probe_{fixture['fixture_id']}_A.log"
    logB = f"/tmp/save_compat_migration_probe_{fixture['fixture_id']}_B.log"
    procA = procB = None

    print(f"\n=== {fixture['baseline_id']} / {fixture['fixture_id']} "
          f"(expected active page: {active_page}) ===")
    try:
        root = make_isolated_root(tmpdir)
        legacy_dir = os.path.join(root, "saves", legacy_slot)
        os.makedirs(legacy_dir, exist_ok=True)
        with open(os.path.join(legacy_dir, "world.synworld"), "wb") as f:
            f.write(fixture_bytes)
        print(f"placed {len(fixture_bytes)}-byte fixture at "
              f"{legacy_dir}/world.synworld")

        # ── Engine A: load the fixture, verify, re-save ─────────────────
        procA = boot_probe(root, port, logA)
        bootstrap_defs(port)
        loaded = send(port, f"return engine.loadSave('{legacy_slot}')")
        chk.ok(loaded.strip() == "true",
               f"engine.loadSave('{legacy_slot}') accepted the fixture "
               f"(got {loaded!r})")
        published, status = wait_load_published(port)
        chk.ok(published,
               f"fixture's load transaction published through the normal "
               f"#763 staging/publish path (status={status})")

        active = send(port, "return world.getActiveWorldId()").strip('"')
        chk.ok(active == active_page,
               f"session's active page is '{active_page}' (got {active!r})")
        chk.ok(send(port, "return engine.isPaused()") == "true",
               "session begins paused, same as any loaded session")

        date_a = send(port, f"return world.getDate('{active_page}')")
        time.sleep(2.0)
        date_b = send(port, f"return world.getDate('{active_page}')")
        chk.ok(date_a == date_b,
               f"no gameplay time advanced during a 2s paused dwell "
               f"({date_a} -> {date_b})")

        saved = send(port, f"return engine.saveWorld('{active_page}', '{resaved_slot}')")
        chk.ok(saved.strip() == "true",
               f"re-saving the session under a new slot succeeded "
               f"(got {saved!r})")
        resaved_path = os.path.join(root, "saves", resaved_slot, "world.synworld")
        for _ in range(100):
            if os.path.isfile(resaved_path):
                break
            time.sleep(0.1)
        chk.ok(os.path.isfile(resaved_path),
               f"re-saved current-format file appeared at {resaved_path}")
        if os.path.isfile(resaved_path):
            resaved_bytes = open(resaved_path, "rb").read()
            # The re-saved bytes must be a genuinely CURRENT-format
            # encode -- never a byte-for-byte copy of the input fixture
            # (that would mean nothing actually re-encoded it).
            chk.ok(resaved_bytes != fixture_bytes,
                   "re-saved file is a real current-format re-encode, not "
                   "a copy of the input fixture's bytes")

        quit_engine(port, procA)
        procA = None

        # ── Engine B: fresh process, load the RE-SAVED current-format
        #    file, compare structural state across the restart ─────────
        procB = boot_probe(root, port, logB)
        bootstrap_defs(port)
        pre = send(port, "return world.getActiveWorldId()")
        chk.ok(pre in ("nil", "null", '""', ""),
               f"fresh engine B has no pre-load active world (got {pre!r})")

        loaded_b = send(port, f"return engine.loadSave('{resaved_slot}')")
        chk.ok(loaded_b.strip() == "true",
               f"engine.loadSave('{resaved_slot}') accepted the re-saved "
               f"current-format file (got {loaded_b!r})")
        published_b, status_b = wait_load_published(port)
        chk.ok(published_b,
               f"re-saved file's load transaction published "
               f"(status={status_b})")

        active_b = send(port, "return world.getActiveWorldId()").strip('"')
        chk.ok(active_b == active_page,
               f"active page survived the restart -> reload round trip "
               f"(got {active_b!r}, expected {active_page!r})")
        chk.ok(send(port, "return engine.isPaused()") == "true",
               "reloaded session begins paused")

        # Unpause ONLY to confirm the default time scale -- never
        # comparing any subsequent random gameplay outcome.
        send(port, "require('scripts.pause').set(false); return 'ok'",
             expect_result=False)
        time.sleep(0.5)
        ts = send(port, f"return world.getTimeScale('{active_page}')")
        chk.ok(ts.strip() in ("1", "1.0"),
               f"unpausing the reloaded session uses the default time "
               f"scale (got {ts})")

    finally:
        if procA is not None:
            quit_engine(port, procA)
        if procB is not None:
            quit_engine(port, procB)
        shutil.rmtree(tmpdir, ignore_errors=True)


def main() -> int:
    ap = argparse.ArgumentParser(description=__doc__,
                                 formatter_class=argparse.RawDescriptionHelpFormatter)
    ap.add_argument("--port", type=int, default=9276)
    args = ap.parse_args()

    fixtures = declared_complete_session_fixtures()
    chk = Checks()
    for fixture in fixtures:
        run_one_fixture(chk, args.port, fixture)

    print(f"\n{'PASS' if chk.failed == 0 else 'FAIL'}: "
          f"{chk.failed} check(s) failed across {len(fixtures)} "
          f"complete-session fixture(s)")
    return 0 if chk.failed == 0 else 1


if __name__ == "__main__":
    sys.exit(main())
