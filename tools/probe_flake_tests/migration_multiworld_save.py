"""Fresh-process reloads and conditional save-list/arena checks."""
from __future__ import annotations

import argparse
from contextlib import ExitStack
from unittest.mock import patch

from . import support
import multiworld_save_probe as probe

PROBE = "multiworld_save"
FIELDS = dict(seed=42, seed2=7, size=64, plates=3, arena=False)


def test_multiworld_save_launch():
    support.isolated_launch_contract(PROBE, fields=FIELDS,
        logs=["multiworld_save_a.log", "multiworld_save_b.log"],
        default_log="/tmp/mw_save_probe_A.log")


def drive(rep, *, arena=False, missing_entry=False, missing_vegetation=False):
    launches = []
    def boot(port, **kwargs):
        launches.append(kwargs)
        return object()
    def identity(_port, page):
        if page == "main_world":
            return dict(name=probe.MW_NAME, gloss=probe.MW_GLOSS)
        if page == "second_world" and not arena:
            return dict(name=probe.SW_NAME)
        return None
    def send(_port, lua, **_kwargs):
        return "0" if "getTerrainAt" in lua else "true"
    with ExitStack() as stack:
        for name in ("bootstrap_defs",):
            stack.enter_context(patch.object(probe, name))
        stack.enter_context(patch.object(probe, "boot", side_effect=boot))
        stack.enter_context(patch.object(probe, "get_identity", side_effect=identity))
        stack.enter_context(patch.object(probe, "populate_world", side_effect=[(1, 2), (3, 4)]))
        stack.enter_context(patch.object(probe, "populate_arena", return_value=(3, 4, 0, [7] * 256)))
        stack.enter_context(patch.object(probe, "read_arena_veg", return_value=None if missing_vegetation else [7] * 256))
        stack.enter_context(patch.object(probe, "send", side_effect=send))
        stack.enter_context(patch.object(probe, "send_json", return_value=[] if missing_entry else [dict(name=probe.SAVE_PREFIX + "fixture", worldName=probe.MW_NAME, worldGloss=probe.MW_GLOSS)]))
        stack.enter_context(patch.object(probe, "id_list", side_effect=[[1], [2], [3], [4]]))
        stack.enter_context(patch.object(probe, "wait_active", return_value=True))
        stack.enter_context(patch.object(probe, "wait_load_published", return_value=(True, {})))
        stack.enter_context(patch.object(probe, "poll_until", return_value=True))
        stack.enter_context(patch.object(probe.time, "sleep"))
        stack.enter_context(patch.object(probe.uuid, "uuid4", return_value=argparse.Namespace(hex="fixture")))
        stack.enter_context(patch.object(probe.os.path, "exists", side_effect=lambda p: str(p).endswith("world.synworld")))
        stack.enter_context(patch.object(probe.os.path, "getsize", return_value=10))
        stack.enter_context(patch.object(probe.os.path, "isdir", return_value=False))
        quit_owned = stack.enter_context(patch.object(probe, "quit_engine"))
        rc = probe._run(argparse.Namespace(port=8911, **dict(FIELDS, arena=arena)), rep)
        support.expect(quit_owned.call_count == 2, "multiworld closes both owned engines")
    support.expect(len(launches) == 2 and all(row["args"] == rep.engine_args() for row in launches),
                   "both multiworld boots receive the requested RTS arguments")
    support.expect(len({row["log"] for row in launches}) == 2,
                   "multiworld boots retain distinct logs")
    return rc


def test_multiworld_save_missing_save_entry():
    def exercise(rep):
        support.expect(drive(rep, missing_entry=True) == 1, "missing save listing preserves failure exit")
    outcomes = support.captured_outcomes(probe, exercise)
    support.expect(outcomes["save_listed"] == "FAIL" and len(outcomes) == 18,
                   "missing listSaves entry retains the exact ordered prefix and stops before dependent checks")


def test_multiworld_save_arena_descriptor_and_gap():
    import contextlib
    import io
    import tempfile
    from pathlib import Path
    with tempfile.TemporaryDirectory() as tmp:
        path = Path(tmp) / "events"
        descriptor = probe.descriptor(True)
        rep = support.probe_protocol.Reporter(descriptor, events_path=str(path), engine_log_dir=tmp, rts_caps=3)
        try:
            with contextlib.redirect_stdout(io.StringIO()):
                rc = drive(rep, arena=True, missing_vegetation=True)
        finally:
            rep.close()
        _, outcomes = support.probe_protocol.parse_event_stream(path.read_text(), descriptor)
        support.expect(rc == 1 and outcomes["arena_vegetation_readable"] == "FAIL"
                       and outcomes["arena_vegetation_restored"] == "MISSING"
                       and outcomes["primary_identity_restored"] == "MISSING",
                       "unreadable arena vegetation leaves a missing suffix rather than an out-of-order event")
        support.expect("secondary_name" not in outcomes and "arena_unnamed" in outcomes,
                       "arena descriptor declares its own conditional assertion sequence")


TESTS = (test_multiworld_save_launch, test_multiworld_save_missing_save_entry, test_multiworld_save_arena_descriptor_and_gap)
