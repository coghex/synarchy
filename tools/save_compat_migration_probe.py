#!/usr/bin/env python3
"""Fresh-process save-compatibility migration probe (issue #766,
save-overhaul C4).

Real-engine, real-restart coverage of the ONE thing the pure hspec gates
("save components", "save compatibility") cannot prove: that a REAL
tracked legacy-baseline fixture on disk actually loads through the
normal #763 whole-session transaction, publishes, survives a paused
dwell, and re-saves/reloads in current format across a real process
restart -- end to end, not merely "the pure decode function returns
Right".

Flow (isolated resource root -- never touches a real player's saves,
see make_isolated_root):
  1. Place docs/save_compat/manifest.json's declared
     "b1-initial-session-minimal" fixture bytes directly at
     saves/<slot>/world.synworld (a legacy generation this build never
     writes itself, only recognizes on load).
  2. Boot engine A, engine.loadSave(slot), wait for the load transaction
     to publish through the normal #763 staging/publish path (proving
     the migrated session is NOT special-cased at that boundary).
  3. Assert the migrated session's structural state: active page is
     "main_world" (issue #766's requirement 7 default), the session
     begins paused, and a short paused dwell advances no gameplay date
     (a migrated session dwells exactly like an ordinary loaded one).
  4. Save the migrated session under a NEW slot name -- this is the
     ONLY way to observe "the migrated session now writes current-format
     bytes going forward" without inspecting cereal internals directly.
  5. Quit engine A, boot a FRESH engine B (same isolated root),
     engine.loadSave the NEW current-format slot, wait for publish.
  6. Compare persistent state across the restart: active page still
     "main_world", still begins paused.
  7. Unpause (via scripts.pause, the paired
     engine.setPaused+world.setTimeScale contract every other save/load
     probe already uses) ONLY to confirm the default time scale --
     never comparing any subsequent random gameplay outcome.

Usage:
  python3 tools/save_compat_migration_probe.py [--port 9276]

Exit 0 = every check above passed.
"""
from __future__ import annotations

import argparse
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
LEGACY_SLOT = "probe_b1_legacy"
RESAVED_SLOT = "probe_b1_resaved"


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


def declared_fixture_path() -> Path:
    manifest = json.loads(MANIFEST_PATH.read_text(encoding="utf-8"))
    for baseline in manifest["baselines"]:
        if baseline["id"] == "b1-initial-session":
            fixtures = baseline["fixtures"]
            assert len(fixtures) == 1, (
                "expected exactly one fixture on the b1-initial-session "
                "baseline; update this probe if that ever changes")
            return REPO / fixtures[0]["path"]
    sys.exit("FAIL: docs/save_compat/manifest.json has no "
              "'b1-initial-session' baseline -- has it been renamed?")


def boot_probe(root: str, port: int, log: str):
    return boot(port, log=log, args=["--resource-root", root], ready_timeout=180)


class Checks:
    def __init__(self) -> None:
        self.failed = 0

    def ok(self, cond: bool, label: str) -> None:
        print(f"  [{'PASS' if cond else 'FAIL'}] {label}")
        if not cond:
            self.failed += 1


def main() -> int:
    ap = argparse.ArgumentParser(description=__doc__,
                                 formatter_class=argparse.RawDescriptionHelpFormatter)
    ap.add_argument("--port", type=int, default=9276)
    args = ap.parse_args()

    fixture_bytes = declared_fixture_path().read_bytes()
    chk = Checks()
    procA = procB = None
    tmpdir = tempfile.mkdtemp(prefix="save_compat_migration_probe_")
    logA = "/tmp/save_compat_migration_probe_A.log"
    logB = "/tmp/save_compat_migration_probe_B.log"

    try:
        root = make_isolated_root(tmpdir)
        legacy_dir = os.path.join(root, "saves", LEGACY_SLOT)
        os.makedirs(legacy_dir, exist_ok=True)
        with open(os.path.join(legacy_dir, "world.synworld"), "wb") as f:
            f.write(fixture_bytes)
        print(f"placed {len(fixture_bytes)}-byte legacy fixture at "
              f"{legacy_dir}/world.synworld")

        # ── Engine A: load the legacy fixture, verify, re-save ──────────
        procA = boot_probe(root, args.port, logA)
        loaded = send(args.port, f"return engine.loadSave('{LEGACY_SLOT}')")
        chk.ok(loaded.strip() == "true",
               f"engine.loadSave('{LEGACY_SLOT}') accepted the legacy fixture "
               f"(got {loaded!r})")
        published, status = wait_load_published(args.port)
        chk.ok(published,
               f"legacy fixture's load transaction published through the "
               f"normal #763 staging/publish path (status={status})")

        active = send(args.port, "return world.getActiveWorldId()").strip('"')
        chk.ok(active == "main_world",
               f"migrated session's active page is 'main_world' "
               f"(got {active!r})")
        chk.ok(send(args.port, "return engine.isPaused()") == "true",
               "migrated session begins paused, same as any loaded session")

        date_a = send(args.port, "return world.getDate('main_world')")
        time.sleep(2.0)
        date_b = send(args.port, "return world.getDate('main_world')")
        chk.ok(date_a == date_b,
               f"no gameplay time advanced during a 2s paused dwell "
               f"({date_a} -> {date_b})")

        saved = send(args.port, f"return engine.saveWorld('main_world', '{RESAVED_SLOT}')")
        chk.ok(saved.strip() == "true",
               f"re-saving the migrated session under a new slot succeeded "
               f"(got {saved!r})")
        resaved_path = os.path.join(root, "saves", RESAVED_SLOT, "world.synworld")
        for _ in range(100):
            if os.path.isfile(resaved_path):
                break
            time.sleep(0.1)
        chk.ok(os.path.isfile(resaved_path),
               f"re-saved current-format file appeared at {resaved_path}")
        if os.path.isfile(resaved_path):
            resaved_bytes = open(resaved_path, "rb").read()
            # The re-saved bytes must be a genuinely CURRENT-format
            # encode -- never a byte-for-byte copy of the legacy fixture
            # (that would mean the migration never actually ran).
            chk.ok(resaved_bytes != fixture_bytes,
                   "re-saved file is a real current-format re-encode, not "
                   "a copy of the legacy fixture's bytes")

        quit_engine(args.port, procA)
        procA = None

        # ── Engine B: fresh process, load the RE-SAVED current-format
        #    file, compare structural state across the restart ─────────
        procB = boot_probe(root, args.port, logB)
        pre = send(args.port, "return world.getActiveWorldId()")
        chk.ok(pre in ("nil", "null", '""', ""),
               f"fresh engine B has no pre-load active world (got {pre!r})")

        loaded_b = send(args.port, f"return engine.loadSave('{RESAVED_SLOT}')")
        chk.ok(loaded_b.strip() == "true",
               f"engine.loadSave('{RESAVED_SLOT}') accepted the re-saved "
               f"current-format file (got {loaded_b!r})")
        published_b, status_b = wait_load_published(args.port)
        chk.ok(published_b,
               f"re-saved file's load transaction published "
               f"(status={status_b})")

        active_b = send(args.port, "return world.getActiveWorldId()").strip('"')
        chk.ok(active_b == "main_world",
               f"active page survived the restart -> reload round trip "
               f"(got {active_b!r})")
        chk.ok(send(args.port, "return engine.isPaused()") == "true",
               "reloaded session begins paused")

        # Unpause ONLY to confirm the default time scale -- never
        # comparing any subsequent random gameplay outcome.
        send(args.port, "require('scripts.pause').set(false); return 'ok'",
             expect_result=False)
        time.sleep(0.5)
        ts = send(args.port, "return world.getTimeScale('main_world')")
        chk.ok(ts.strip() in ("1", "1.0"),
               f"unpausing the reloaded session uses the default time "
               f"scale (got {ts})")

        print(f"\n{'PASS' if chk.failed == 0 else 'FAIL'}: "
              f"{chk.failed} check(s) failed")
        return 0 if chk.failed == 0 else 1

    finally:
        if procA is not None:
            quit_engine(args.port, procA)
        if procB is not None:
            quit_engine(args.port, procB)
        shutil.rmtree(tmpdir, ignore_errors=True)


if __name__ == "__main__":
    sys.exit(main())
