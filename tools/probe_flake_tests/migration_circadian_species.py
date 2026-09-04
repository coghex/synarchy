#!/usr/bin/env python3
"""circadian_species's `probe-result/v1` migration contract (#2087).

Two groups, because this probe carries two: the batch contract every
batch-migrated probe shares, and its own protocol contract, which
drives the real `_run` through a faked console for both a clean sweep
and each failure shape it has to attribute.
"""
from __future__ import annotations

import os
import tempfile
from pathlib import Path

from . import support
from .support import probe_protocol
from .support import expect

PROBE = "circadian_species"

def test_circadian_species_migration() -> None:
    support.batch_contract(
        PROBE, 'circadian_species_probe.py', 9016,
        ("species_urge_phases", "utility_crossover", "wake_boundaries",
         "bear_selects_sleep", "bear_reaches_sleeping",
         "public_wake_standing", "sleeps_through_dawn",
         "wakes_at_own_boundary"))


def _drive_circadian_species(rep, *, observation_failure=None,
                             setup_failure=False):
    import circadian_species_probe as circadian_species  # type: ignore

    launches = {}
    calls = {"urge": 0, "utility": 0, "wake": 0, "pressure": 0}
    spawned = 0

    class FakeProc:
        pass

    def fake_boot(port, log=None, args=None, **_kwargs):
        launches["engine"] = {
            "port": port,
            "log": log,
            "args": list(args or []),
        }
        return FakeProc()

    def fake_bootstrap(_port):
        if setup_failure:
            raise circadian_species.ProbeSetupError("unit definitions unavailable")

    def fake_spawn(*_args, **_kwargs):
        nonlocal spawned
        spawned += 1
        return spawned

    def fake_send(_port, command, **_kwargs):
        if "getCircadianUrge" in command:
            index = calls["urge"]
            calls["urge"] += 1
            if observation_failure == "urge" and index == 0:
                return "not-a-number"
            return (1.0, 0.0, 0.0, 1.0)[index]
        if "sleepGoal.sleepUtility" in command:
            index = calls["utility"]
            calls["utility"] += 1
            if observation_failure == "utility" and index == 0:
                return "not-a-number"
            return (2.0, 0.0, 0.0, 2.0)[index]
        if "unit_stats').get" in command:
            return 100.0
        if "wakeAngleFor" in command:
            index = calls["wake"]
            calls["wake"] += 1
            if observation_failure == "wake" and index == 0:
                return "not-a-number"
            if "bear_brown" in command:
                return 0.75
            return 0.25
        if "unit.getStat" in command:
            index = calls["pressure"]
            calls["pressure"] += 1
            if ((observation_failure == "pressure_dawn" and index == 0)
                    or (observation_failure == "pressure_dusk" and index == 1)):
                return "not-a-number"
            return 40.0
        return "ok"

    def fake_ai_field(_uid, field):
        return {
            "currentAction": "go_to_sleep",
            "sleepPhase": "sleeping",
            "sleepLastSunAngle": 0.2,
            "sleepWakeRequested": "nil",
        }.get(field)

    saved = (
        circadian_species.boot, circadian_species.quit_engine,
        circadian_species.bootstrap_defs, circadian_species.init_arena,
        circadian_species.spawn_acolyte, circadian_species.send,
        circadian_species.poll_until, circadian_species.get_ai_field,
        circadian_species.get_pose, circadian_species.wait_for_pose,
        circadian_species.time.sleep, circadian_species.PORT,
    )
    circadian_species.boot = fake_boot
    circadian_species.quit_engine = lambda *_args, **_kwargs: None
    circadian_species.bootstrap_defs = fake_bootstrap
    circadian_species.init_arena = lambda *_args, **_kwargs: None
    circadian_species.spawn_acolyte = fake_spawn
    circadian_species.send = fake_send
    circadian_species.poll_until = lambda _timeout, _fn: True
    circadian_species.get_ai_field = fake_ai_field
    circadian_species.get_pose = lambda _uid: "sleeping"
    circadian_species.wait_for_pose = lambda *_args, **_kwargs: True
    circadian_species.time.sleep = lambda _seconds: None
    try:
        rc = circadian_species._run(9016, rep)
    finally:
        (
            circadian_species.boot, circadian_species.quit_engine,
            circadian_species.bootstrap_defs, circadian_species.init_arena,
            circadian_species.spawn_acolyte, circadian_species.send,
            circadian_species.poll_until, circadian_species.get_ai_field,
            circadian_species.get_pose, circadian_species.wait_for_pose,
            circadian_species.time.sleep, circadian_species.PORT,
        ) = saved
    return rc, launches


def test_circadian_species_protocol_contract() -> None:
    """Exercise a migrated probe's real structured-result path."""
    print("\n-- circadian_species protocol contract --")
    import io
    import circadian_species_probe as circadian_species  # type: ignore

    ids = circadian_species.DESCRIPTOR.ids

    standalone = io.StringIO()
    rep = probe_protocol.Reporter(circadian_species.DESCRIPTOR,
                                  stream=standalone)
    rc, launches = _drive_circadian_species(rep)
    rep.close()
    expect(rc == 0, f"circadian_species standalone exits 0 (got {rc})")
    expect(standalone.getvalue().count("[PASS]") == len(ids),
           "circadian_species standalone reports every declared check")
    expect(launches["engine"] == {
        "port": 9016, "log": circadian_species.LOG, "args": []},
        "circadian_species preserves standalone engine launch behavior")

    with tempfile.TemporaryDirectory() as tmp:
        events = Path(tmp) / "events.jsonl"
        stream = io.StringIO()
        rep = probe_protocol.Reporter(
            circadian_species.DESCRIPTOR, events_path=str(events),
            engine_log_dir=tmp, rts_caps=4, stream=stream)
        rc, launches = _drive_circadian_species(rep)
        rep.close()
        event_text = events.read_text(encoding="utf-8")
        _seen, outcomes = probe_protocol.parse_event_stream(
            event_text, circadian_species.DESCRIPTOR)
        expect(rc == 0 and all(value == probe_protocol.PASS
                               for value in outcomes.values()),
               f"circadian_species reports all checks in order on success "
               f"(got rc={rc}, {outcomes})")
        expect(stream.getvalue() == "",
               "circadian_species protocol success prints nothing to stdout")
        expect(probe_protocol.forbidden_marker_lines(stream.getvalue()) == [],
               "circadian_species protocol stdout has no bracketed markers")
        expect(launches["engine"] == {
            "port": 9016,
            "log": os.path.join(tmp, circadian_species.LOG_NAME),
            "args": ["+RTS", "-N4", "-RTS"],
        }, "circadian_species uses harness RTS and isolated engine log")

    observation_cases = (
        ("urge", "species_urge_phases"),
        ("utility", "utility_crossover"),
        ("wake", "wake_boundaries"),
        ("pressure_dawn", "sleeps_through_dawn"),
        ("pressure_dusk", "wakes_at_own_boundary"),
    )
    for failure, failed_id in observation_cases:
        with tempfile.TemporaryDirectory() as tmp:
            events = Path(tmp) / "events.jsonl"
            stream = io.StringIO()
            rep = probe_protocol.Reporter(
                circadian_species.DESCRIPTOR, events_path=str(events),
                stream=stream)
            rc, _launches = _drive_circadian_species(
                rep, observation_failure=failure)
            rep.close()
            _seen, outcomes = probe_protocol.parse_event_stream(
                events.read_text(encoding="utf-8"),
                circadian_species.DESCRIPTOR)
            failed_index = ids.index(failed_id)
            expect(rc == 1
                   and all(outcomes[cid] == probe_protocol.PASS
                           for cid in ids[:failed_index])
                   and outcomes[failed_id] == probe_protocol.FAIL
                   and all(outcomes[cid] == probe_protocol.MISSING
                           for cid in ids[failed_index + 1:]),
                   f"circadian_species attributes {failure} observation "
                   f"failure to {failed_id} (got rc={rc}, {outcomes})")
            expect(stream.getvalue() == "",
                   f"circadian_species {failure} protocol failure prints "
                   "nothing to stdout")

    with tempfile.TemporaryDirectory() as tmp:
        events = Path(tmp) / "events.jsonl"
        stream = io.StringIO()
        rep = probe_protocol.Reporter(
            circadian_species.DESCRIPTOR, events_path=str(events),
            stream=stream)
        rc, _launches = _drive_circadian_species(rep, setup_failure=True)
        rep.close()
        seen, outcomes = probe_protocol.parse_event_stream(
            events.read_text(encoding="utf-8"), circadian_species.DESCRIPTOR)
        expect(rc == 2
               and all(value == probe_protocol.MISSING
                       for value in outcomes.values()),
               "circadian_species setup abort exits 2 with every check missing")
        expect(any(isinstance(event, probe_protocol.DiagnosticEvent)
                   and event.level == "WARN" for event in seen),
               "circadian_species setup abort records a WARN diagnostic")
        expect(stream.getvalue() == "",
               "circadian_species protocol setup abort prints nothing to stdout")


TESTS_BATCH = (test_circadian_species_migration,)
TESTS_PROTOCOL = (test_circadian_species_protocol_contract,)
TESTS = TESTS_BATCH + TESTS_PROTOCOL
