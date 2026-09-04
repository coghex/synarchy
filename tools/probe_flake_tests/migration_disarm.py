#!/usr/bin/env python3
"""disarm's `probe-result/v1` migration contract (#2087).

Drives the real `tools/disarm_probe.py` through a faked console so the
probe's standalone behaviour, its structured-result parity and its
failure attribution are pinned without booting an engine.
"""
from __future__ import annotations

import os
import tempfile
from pathlib import Path

from . import support
from .support import probe_protocol
from .support import expect

PROBE = "disarm"

def _drive_disarm(rep, *, second_equip="true"):
    import disarm_probe as disarm  # type: ignore

    launches = {}
    ground = iter((0, 1, 2))
    held = iter(("steel_dagger", "EMPTY", "EMPTY"))
    equip_calls = 0

    class FakeProc:
        pass

    def fake_boot(port, log=None, args=None, **_kw):
        launches["engine"] = {"port": port, "log": log,
                              "args": list(args or [])}
        return FakeProc()

    def fake_send(_port, lua, **_kw):
        nonlocal equip_calls
        if "unit.spawn" in lua:
            return "1"
        if "equipment.equip" in lua:
            equip_calls += 1
            return "true" if equip_calls == 1 else second_equip
        if "item.listGround" in lua:
            return str(next(ground))
        return "ok"

    saved = (disarm.boot, disarm.quit_engine, disarm.bootstrap,
             disarm.send, disarm.held_right, disarm.time.sleep)
    disarm.boot = fake_boot
    disarm.quit_engine = lambda *a, **k: None
    disarm.bootstrap = lambda _port: None
    disarm.send = fake_send
    disarm.held_right = lambda _port, _uid: next(held)
    disarm.time.sleep = lambda _seconds: None
    try:
        rc = disarm._run(9193, rep)
    finally:
        (disarm.boot, disarm.quit_engine, disarm.bootstrap,
         disarm.send, disarm.held_right, disarm.time.sleep) = saved
    return rc, launches


def test_disarm_standalone() -> None:
    print("\n-- disarm probe migration --")
    ids = ("initial_drop", "repeat_drop")
    if support.migration_descriptor("disarm_probe.py", "disarm", ids) is None:
        return

    import io
    import disarm_probe as disarm  # type: ignore

    standalone = io.StringIO()
    rc, launches = _drive_disarm(
        probe_protocol.Reporter(disarm.DESCRIPTOR, stream=standalone))
    expect(rc == 0, f"disarm standalone exits 0 (got {rc})")
    expect(standalone.getvalue().count("[PASS]") == 2,
           "disarm standalone prints two human PASS lines")
    expect(launches["engine"] == {"port": 9193, "log": disarm.LOG, "args": []},
           "disarm standalone preserves its port and engine-log behavior")

    with tempfile.TemporaryDirectory() as tmp:
        events = Path(tmp) / "events.jsonl"
        stream = io.StringIO()
        rep = probe_protocol.Reporter(
            disarm.DESCRIPTOR, events_path=str(events),
            engine_log_dir=tmp, rts_caps=4, stream=stream)
        rc, launches = _drive_disarm(rep, second_equip="false")
        rep.close()
        event_text = events.read_text(encoding="utf-8")
        _seen, outcomes = probe_protocol.parse_event_stream(
            event_text, disarm.DESCRIPTOR)
        expect(rc == 3, f"disarm preserves inconclusive exit 3 (got {rc})")
        expect(outcomes == {"initial_drop": "PASS", "repeat_drop": "MISSING"},
               f"disarm retains the first result and leaves the rejected "
               f"re-equip path MISSING (got {outcomes})")
        expect('"level": "SKIP"' in event_text,
               "disarm diagnoses its unexercised repeat drop as SKIP")
        expect(stream.getvalue() == "",
               "disarm protocol mode prints nothing to stdout")
        expect(launches["engine"]["args"] == ["+RTS", "-N4", "-RTS"]
               and launches["engine"]["log"]
               == os.path.join(tmp, disarm.LOG_NAME),
               "disarm uses harness RTS and isolated engine log")


TESTS = (test_disarm_standalone,)
