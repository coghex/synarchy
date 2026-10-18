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


FINITE_IDS = ("finite_rectangle_restored", "finite_edge_edits_restored",
              "finite_unit_contained", "finite_fluid_closed",
              "finite_expansion_refused", "finite_load_preserves_save_bytes")


def finite_checks_pass(_port, _page, _state, chk, _save_file, _save_digest):
    """Stand-in for the live finite-arena probe: every check it owns, in
    its own order, reported as passing."""
    for check_id in FINITE_IDS:
        chk.ok(check_id, True, check_id)


def drive(rep, *, arena=False, missing_entry=False, missing_vegetation=False,
          save_changed=False):
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
        stack.enter_context(patch.object(probe, "populate_arena", return_value=(3, 4, 0, [7] * 256, [9] * 256, 0)))
        stack.enter_context(patch.object(probe, "expand_arena", return_value=True))
        stack.enter_context(patch.object(probe, "populate_finite", return_value={
            "rect": (-50, 49, -19, 20), "base": {}, "uid": 9, "wet": 2}))
        stack.enter_context(patch.object(
            probe, "check_finite_restored", side_effect=finite_checks_pass))
        digests = iter(["before", "after" if save_changed else "before"])
        stack.enter_context(patch.object(probe, "file_sha256", side_effect=lambda _p: next(digests)))
        stack.enter_context(patch.object(probe, "read_arena_veg", side_effect=lambda _port, origin=None: None if missing_vegetation else ([9] * 256 if origin == probe.EXPANDED_VEG_CHUNK else [7] * 256)))
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
        support.expect(all(outcomes[check] == "MISSING" for check in (
                            "arena_expanded_readable", "arena_expanded_vegetation_restored",
                            "arena_expanded_edit_restored", "arena_load_preserves_save_bytes")),
                       "the expanded-arena checks stay a missing suffix when the central vegetation is unreadable")
        support.expect("secondary_name" not in outcomes and "arena_unnamed" in outcomes,
                       "arena descriptor declares its own conditional assertion sequence")


def test_multiworld_save_arena_expansion_checks():
    """#2707: the expanded-chunk checks pass on a faithful run, and a load
    that rewrote the save file fails exactly the byte-preservation check."""
    import contextlib
    import io
    import tempfile
    from pathlib import Path
    def run(**kwargs):
        with tempfile.TemporaryDirectory() as tmp:
            path = Path(tmp) / "events"
            descriptor = probe.descriptor(True)
            rep = support.probe_protocol.Reporter(descriptor, events_path=str(path), engine_log_dir=tmp, rts_caps=3)
            try:
                with contextlib.redirect_stdout(io.StringIO()):
                    rc = drive(rep, arena=True, **kwargs)
            finally:
                rep.close()
            _, outcomes = support.probe_protocol.parse_event_stream(path.read_text(), descriptor)
            return rc, outcomes
    rc, outcomes = run()
    expanded = ("arena_expanded_readable", "arena_expanded_vegetation_restored",
                "arena_expanded_edit_restored", "arena_load_preserves_save_bytes") \
        + FINITE_IDS
    support.expect(rc == 0 and all(outcomes[check] == "PASS" for check in expanded),
                   "every expanded-arena check passes on a faithful round trip")
    rc, outcomes = run(save_changed=True)
    support.expect(rc == 1 and outcomes["arena_load_preserves_save_bytes"] == "FAIL"
                   and all(outcomes[check] == "PASS" for check in expanded[:3]),
                   "a rewritten save file fails only the byte-preservation check")


def test_multiworld_save_finite_arena_contract():
    """#2708: the probe's finite-arena rectangle is the issue's formula, and
    its probe points are exactly the corners, edge midpoints and their
    outside neighbours — literals, not read back from the engine."""
    support.expect(probe.finite_rect(20, 10) == (-10, 9, -4, 5), "20x10 rectangle")
    support.expect(probe.finite_rect(21, 11) == (-10, 10, -5, 5), "21x11 rectangle")
    support.expect(probe.finite_rect(1, 1) == (0, 0, 0, 0), "1x1 rectangle")
    support.expect(probe.finite_rect(*probe.FINITE_SIZE) == (-50, 49, -19, 20),
                   "the probe's 100x40 rectangle")
    inside, outside = probe.finite_probe_points((-50, 49, -19, 20))
    support.expect(inside == [(-50, -19), (49, -19), (-50, 20), (49, 20),
                              (-50, 0), (49, 0), (-1, -19), (-1, 20)],
                   "eight inside points")
    support.expect(outside == [(-51, -20), (50, -20), (-51, 21), (50, 21),
                               (-51, 0), (50, 0), (-1, -20), (-1, 21)],
                   "eight outside points")
    support.expect(all(i in probe.descriptor(True).ids for i in FINITE_IDS),
                   "the arena descriptor declares every finite-arena check")
    support.expect(not any(i in probe.descriptor(False).ids for i in FINITE_IDS),
                   "the generated-world descriptor does not")


TESTS = (test_multiworld_save_launch, test_multiworld_save_missing_save_entry, test_multiworld_save_arena_descriptor_and_gap,
         test_multiworld_save_arena_expansion_checks,
         test_multiworld_save_finite_arena_contract)
