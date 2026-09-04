#!/usr/bin/env python3
"""circadian's `probe-result/v1` migration contract (#2087).

Drives the real `tools/circadian_probe.py` through a faked console so the
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

PROBE = "circadian"

def _drive_circadian(rep, *, midnight=0.0):
    import circadian_probe as circadian  # type: ignore

    launches = {}
    urges = iter((midnight, 0.0, 1.0, 0.5))
    pressure = iter((100.0, 99.98, 99.96, 99.94, 99.92))

    class FakeProc:
        pass

    def fake_boot(port, log=None, args=None, **_kw):
        launches["engine"] = {"port": port, "log": log,
                              "args": list(args or [])}
        return FakeProc()

    def fake_send(_port, lua, **_kw):
        if "max_sleep_pressure" in lua:
            return "100"
        if "sleep_pressure" in lua:
            return str(next(pressure))
        return "ok"

    saved = (circadian.boot, circadian.quit_engine,
             circadian.bootstrap_defs, circadian.init_arena,
             circadian.spawn_acolyte, circadian.urge_at,
             circadian.send, circadian.poll_until,
             circadian.time.sleep, circadian.PORT)
    circadian.boot = fake_boot
    circadian.quit_engine = lambda *a, **k: None
    circadian.bootstrap_defs = lambda _port: None
    circadian.init_arena = lambda _port, name=None: None
    circadian.spawn_acolyte = lambda *a, **k: 1
    circadian.urge_at = lambda *a, **k: next(urges)
    circadian.send = fake_send
    circadian.poll_until = lambda _timeout, fn: fn()
    circadian.time.sleep = lambda _seconds: None
    try:
        rc = circadian._run(9013, rep)
    finally:
        (circadian.boot, circadian.quit_engine,
         circadian.bootstrap_defs, circadian.init_arena,
         circadian.spawn_acolyte, circadian.urge_at,
         circadian.send, circadian.poll_until,
         circadian.time.sleep, circadian.PORT) = saved
    return rc, launches


def test_circadian_standalone() -> None:
    print("\n-- circadian probe migration --")
    import io
    import circadian_probe as circadian  # type: ignore

    ids = ("urge_midnight_flat", "urge_noon_flat", "urge_dusk_peak",
           "urge_evening_rising", "sleep_pressure_seeded",
           "sleep_pressure_monotonic", "sleep_pressure_drain_rate")
    if support.migration_descriptor("circadian_probe.py", "circadian", ids) is None:
        return

    standalone = io.StringIO()
    rc, launches = _drive_circadian(
        probe_protocol.Reporter(circadian.DESCRIPTOR, stream=standalone))
    expect(rc == 0, f"circadian standalone exits 0 (got {rc})")
    expect(standalone.getvalue().count("[PASS]") == 7,
           "circadian standalone prints seven human PASS lines")
    expect(launches["engine"] == {"port": 9013, "log": circadian.LOG,
                                  "args": []},
           "circadian preserves standalone launch behavior")

    with tempfile.TemporaryDirectory() as tmp:
        events = Path(tmp) / "events.jsonl"
        stream = io.StringIO()
        rep = probe_protocol.Reporter(
            circadian.DESCRIPTOR, events_path=str(events),
            engine_log_dir=tmp, rts_caps=4, stream=stream)
        rc, launches = _drive_circadian(rep, midnight=0.5)
        rep.close()
        _seen, outcomes = probe_protocol.parse_event_stream(
            events.read_text(encoding="utf-8"), circadian.DESCRIPTOR)
        expect(rc == 1 and outcomes["urge_midnight_flat"] == "FAIL",
               "circadian attributes its first failed urge check and exits 1")
        expect(all(outcomes[key] == "MISSING" for key in ids[1:]),
               "circadian preserves first-failure early stop as trailing MISSING")
        expect(stream.getvalue() == "",
               "circadian protocol mode prints nothing to stdout")
        expect(launches["engine"]["args"] == ["+RTS", "-N4", "-RTS"]
               and launches["engine"]["log"]
               == os.path.join(tmp, circadian.LOG_NAME),
               "circadian uses harness RTS and isolated engine log")


TESTS = (test_circadian_standalone,)
