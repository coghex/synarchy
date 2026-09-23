"""Integrity diagnostics retain ordered failures and all three engine logs."""
from __future__ import annotations

import argparse
from contextlib import ExitStack
from pathlib import Path
from unittest.mock import patch

from . import support
import persistence_integrity_probe as probe

PROBE = "persistence_integrity"


def test_persistence_integrity_descriptor():
    support.migration_descriptor("persistence_integrity_probe.py", PROBE, (
        "build_site_destroyed", "build_target_cleared", "save_integrity_diagnostic",
        "save_dangling_reference", "no_stale_build_target", "load_accepted",
        "load_published", "unit_restored", "load_integrity_diagnostic",
        "load_dangling_reference", "destroyed_unit_named", "corrupt_load_rejected",
        "corrupt_load_failed", "rejected_load_paused", "active_page_unchanged", "unit_state_unchanged"))


def test_persistence_integrity_three_boots_and_failure():
    def exercise(rep):
        launches = []
        uid = 0
        root = None
        def boot(port, **kwargs):
            nonlocal root
            root = Path(kwargs["args"][1])
            launches.append(kwargs)
            Path(kwargs["log"]).write_text("integrity diagnostic unit_ai dangling-reference unit 2 \n")
            return object()
        def send(_port, lua, **_kwargs):
            nonlocal uid
            if "unit.spawn" in lua:
                uid += 1
                return str(uid)
            if "building.spawn" in lua:
                return "11"
            if "getTerrainAt" in lua:
                return "0"
            if "attackTargetUid" in lua:
                return "2"
            if "engine.saveWorld" in lua:
                path = root / "saves" / probe.SLOT / "world.synworld"
                path.parent.mkdir(parents=True, exist_ok=True)
                path.write_bytes(b"fixture codec bytes")
            if "engine.loadSave" in lua and probe.CORRUPT_SLOT in lua:
                return "false"
            return "true"
        # Use a dedicated artifact directory even standalone so this fixture
        # can inspect all logs after the probe removes its isolated save root.
        import tempfile
        with tempfile.TemporaryDirectory() as tmp, ExitStack() as stack:
            rep.engine_log_dir = tmp
            rep.rts_caps = 3
            for name in ("bootstrap_defs", "load_ai_stack", "clear_find_water"):
                stack.enter_context(patch.object(probe, name))
            stack.enter_context(patch.object(probe, "boot", side_effect=boot))
            stack.enter_context(patch.object(probe, "send", side_effect=send))
            stack.enter_context(patch.object(probe, "send_json", return_value=dict(phase="LoadFailed")))
            stack.enter_context(patch.object(probe, "find_flat_strip", return_value=(0, 0, 0)))
            stack.enter_context(patch.object(probe, "find_build_site", return_value=(2, 2)))
            stack.enter_context(patch.object(probe, "ai_build_target", return_value="11"))
            stack.enter_context(patch.object(probe, "poll_until", side_effect=["constructing,1", True, True, False]))
            stack.enter_context(patch.object(probe, "wait_load_published", return_value=(True, {})))
            stack.enter_context(patch.object(probe.time, "sleep"))
            quit_owned = stack.enter_context(patch.object(probe, "quit_engine"))
            rc = probe._run(argparse.Namespace(port=8911, seed=42, size=48), rep)
            support.expect(rc == 1 and quit_owned.call_count == 3,
                           "integrity stale build target remains failed through both fresh reloads and cleanup")
            support.expect(len(launches) == 3 and all(row["args"][-3:] == ["+RTS", "-N3", "-RTS"] for row in launches),
                           "all three integrity engine boots receive harness RTS caps")
            support.expect(sorted(p.name for p in Path(tmp).glob("*.log")) == [
                "persistence_integrity_a.log", "persistence_integrity_b.log", "persistence_integrity_c.log"],
                "all three integrity logs survive isolated save-root teardown")
    outcomes = support.captured_outcomes(probe, exercise)
    support.expect(outcomes["build_target_cleared"] == "FAIL"
                   and len(outcomes) == len(probe.DESCRIPTOR.ids)
                   and sum(v == "PASS" for v in outcomes.values()) == 15,
                   "integrity reports one real stale-target assertion failure independently of later checks")


TESTS = (test_persistence_integrity_descriptor, test_persistence_integrity_three_boots_and_failure)
