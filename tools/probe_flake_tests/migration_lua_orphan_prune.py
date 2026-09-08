#!/usr/bin/env python3
"""lua_orphan_prune's `probe-result/v1` migration contract (#2087)."""
from __future__ import annotations

import argparse
import io
import tempfile
from pathlib import Path
from unittest.mock import Mock, patch

from . import support

PROBE = "lua_orphan_prune"


def test_lua_orphan_prune_migration() -> None:
    support.batch_contract(
        PROBE, "lua_orphan_prune_probe.py", 9008,
        ("snapshot_filters_orphan", "load_pauses_immediately",
         "load_reconcile_prunes_orphan", "nested_references_scrubbed",
         "per_entity_apply"),
        invoke=support.namespace_invoke(seed=42, size=64))


def _drive_load_boundary(observation, *, published=True):
    """Drive the real probe with a console that rejects a post-load query.

    The load's original command may observe request-time state, but once
    it returns the session cutover cancels separately queued observations.
    This reproduces the retained ten-run failure without an engine or sleeps.
    """
    import lua_orphan_prune_probe as probe

    loaded = False
    spawned = 0
    family_reads = 0
    commands = []

    def send(_port, command, **_kwargs):
        nonlocal spawned, loaded
        commands.append(command)
        if "engine.loadSave(" in command:
            loaded = True
            return "true"
        if command == "return engine.isPaused()" and loaded:
            return ("REJECTED: a load transaction replaced the session "
                    "while this command was queued")
        if "unit.spawn(" in command:
            spawned += 1
            return str(spawned)
        if "sm.snapshotAll()" in command:
            return "absent,present"
        if "tostring(s.foragePhase)" in command:
            return "nil,nil"
        if "tostring(s.currentAction)" in command:
            return "idle"
        if "tostring(s.probeMarker)" in command or "getState(998877)" in command:
            return "nil"
        return "ok"

    def send_json(_port, command, **_kwargs):
        nonlocal loaded
        commands.append(command)
        # A split request/observation is rejected, just as in the real log.
        if loaded or "engine.loadSave(" not in command:
            return "REJECTED: a load transaction replaced the session"
        loaded = True
        return observation

    def present_families(*_args):
        nonlocal family_reads
        family_reads += 1
        return probe.PLANTED_FAMILIES if family_reads == 1 else ""

    wait = Mock(return_value=(published, {"id": 17,
                                         "phase": "LoadPublished" if published
                                         else "LoadReconciliationFailed"}))
    quit_engine = Mock()
    proc = Mock()
    with tempfile.TemporaryDirectory(prefix="orphan-pause-test-") as tmp:
        events = Path(tmp) / "events.jsonl"
        rep = support.probe_protocol.Reporter(
            probe.DESCRIPTOR, events_path=str(events), stream=io.StringIO())
        with patch.multiple(
            probe, boot=Mock(return_value=proc), quit_engine=quit_engine,
            bootstrap_defs=Mock(), find_flat=Mock(return_value=(0, 0)),
            send=send, send_json=send_json,
            getstate=lambda _port, uid: "nil" if loaded and uid == 1 else "present",
            exists=lambda _port, uid: uid == 2,
            present_families=present_families,
            wait_save_written=Mock(return_value=True), wait_load_published=wait,
            SAVE_NAME=str(Path(tmp) / "unused-save"),
        ), patch.object(probe.time, "sleep"):
            try:
                rc = probe._run(argparse.Namespace(port=9008, seed=42, size=64), rep)
            finally:
                rep.close()
        _, outcomes = support.probe_protocol.parse_event_stream(
            events.read_text(), probe.DESCRIPTOR)
    support.expect(quit_engine.call_count == 1 and proc.wait.call_count == 1,
                   "orphan probe tears down its engine on every outcome")
    return rc, outcomes, commands, wait


def test_lua_orphan_prune_load_boundary() -> None:
    print("\n-- lua_orphan_prune request-time pause observation --")
    good = {"accepted": True, "before": False, "paused": True, "request_id": 17}
    rc, outcomes, commands, wait = _drive_load_boundary(good)
    support.expect(rc == 0 and all(v == "PASS" for v in outcomes.values()),
                   "session cutover does not prevent any of the five checks")
    load_commands = [c for c in commands if "engine.loadSave(" in c]
    support.expect(len(load_commands) == 1,
                   "the probe requests the load exactly once")
    command = load_commands[0]
    support.expect(
        command.index("engine.setPaused(false)")
        < command.index("engine.isPaused()")
        < command.index("engine.loadSave(")
        < command.rindex("engine.isPaused()"),
        "one command observes unpaused state before load and paused state after it")
    support.expect("return engine.isPaused()" not in commands,
                   "no separate pause query can be cancelled at cutover")
    support.expect(wait.call_args.kwargs.get("request_id") == 17,
                   "completion waits for the request captured at acceptance")

    for label, observation in (
        ("load did not pause", dict(good, paused=False)),
        ("unpause precondition failed", dict(good, before=True)),
    ):
        rc, outcomes, _, _ = _drive_load_boundary(observation)
        support.expect(rc == 1 and outcomes["load_pauses_immediately"] == "FAIL",
                       f"{label} remains a pause-check failure")

    for observation in (
        dict(good, accepted=False), None, "REJECTED", {},
        dict(good, request_id=None), dict(good, request_id=True),
    ):
        rc, outcomes, commands, wait = _drive_load_boundary(observation)
        support.expect(rc == 2 and not wait.called
                       and outcomes["load_pauses_immediately"] == "MISSING",
                       f"invalid acceptance aborts without claiming a pause result: {observation!r}")
        support.expect(sum("engine.loadSave(" in c for c in commands) == 1,
                       "invalid acceptance never retries the load")

    rc, outcomes, _, _ = _drive_load_boundary(good, published=False)
    support.expect(rc == 2 and outcomes["load_pauses_immediately"] == "PASS"
                   and outcomes["load_reconcile_prunes_orphan"] == "MISSING",
                   "a request-time pause does not hide unsuccessful reconciliation")


TESTS = (test_lua_orphan_prune_migration, test_lua_orphan_prune_load_boundary)
