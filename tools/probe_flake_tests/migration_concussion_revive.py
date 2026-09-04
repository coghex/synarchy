#!/usr/bin/env python3
"""concussion_revive's `probe-result/v1` migration contract (#2087).

Drives the real `tools/concussion_revive_probe.py` through a faked console so the
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

PROBE = "concussion_revive"

def _drive_concussion_revive(rep, *, second_pass=True):
    import concussion_revive_probe as concussion  # type: ignore

    launches = {}

    class FakeProc:
        pass

    def fake_boot(port, log=None, args=None, **_kw):
        launches["engine"] = {"port": port, "log": log,
                              "args": list(args or [])}
        return FakeProc()

    def fake_case(_port, _idx, _concussion, _expect, check_id, reporter):
        passed = second_pass or check_id == "in_band_stays_collapsed"
        return reporter.check(check_id, passed,
                              f"synthetic {check_id} {'passed' if passed else 'failed'}",
                              {"synthetic": True})

    saved = (concussion.boot, concussion.quit_engine, concussion.bootstrap,
             concussion.run_case)
    concussion.boot = fake_boot
    concussion.quit_engine = lambda *a, **k: None
    concussion.bootstrap = lambda _port: None
    concussion.run_case = fake_case
    try:
        rc = concussion._run(9304, rep)
    finally:
        (concussion.boot, concussion.quit_engine, concussion.bootstrap,
         concussion.run_case) = saved
    return rc, launches


def test_concussion_revive_standalone() -> None:
    print("\n-- concussion_revive probe migration --")
    ids = ("in_band_stays_collapsed", "below_band_rises")
    if support.migration_descriptor("concussion_revive_probe.py",
                             "concussion_revive", ids) is None:
        return

    import io
    import concussion_revive_probe as concussion  # type: ignore

    standalone = io.StringIO()
    rc, launches = _drive_concussion_revive(
        probe_protocol.Reporter(concussion.DESCRIPTOR, stream=standalone))
    expect(rc == 0, f"concussion_revive standalone exits 0 (got {rc})")
    expect(standalone.getvalue().count("[PASS]") == 2,
           "concussion_revive standalone prints two human PASS lines")
    expect(launches["engine"]["args"] == [],
           "concussion_revive standalone passes no RTS override")
    expect(launches["engine"]["log"] == "/tmp/synarchy_probe_9304.log",
           "concussion_revive preserves its historical per-port log fallback")

    with tempfile.TemporaryDirectory() as tmp:
        events = Path(tmp) / "events.jsonl"
        stream = io.StringIO()
        rep = probe_protocol.Reporter(
            concussion.DESCRIPTOR, events_path=str(events),
            engine_log_dir=tmp, rts_caps=4, stream=stream)
        rc, launches = _drive_concussion_revive(rep, second_pass=False)
        rep.close()
        _seen, outcomes = probe_protocol.parse_event_stream(
            events.read_text(encoding="utf-8"), concussion.DESCRIPTOR)
        expect(rc == 1 and outcomes == {
                   "in_band_stays_collapsed": "PASS",
                   "below_band_rises": "FAIL"},
               f"concussion_revive attributes one failed case and exits 1 "
               f"(got rc={rc}, {outcomes})")
        expect(stream.getvalue() == "",
               "concussion_revive protocol mode prints nothing to stdout")
        expect(launches["engine"]["args"] == ["+RTS", "-N4", "-RTS"]
               and launches["engine"]["log"]
               == os.path.join(tmp, concussion.LOG_NAME),
               "concussion_revive uses harness RTS and isolated engine log")


TESTS = (test_concussion_revive_standalone,)
