#!/usr/bin/env python3
"""blood_impact's `probe-result/v1` migration contract (#2087).

Drives the real `tools/blood_impact_probe.py` through a faked console so the
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

PROBE = "blood_impact"

def _drive_blood_impact(rep, *, high_opacity=0.9, setup_failure=False):
    import blood_impact_probe as blood_impact  # type: ignore

    launches = {}

    class FakeProc:
        pass

    def fake_boot(port, log=None, args=None, **_kw):
        launches["engine"] = {"port": port, "log": log,
                              "args": list(args or [])}
        return FakeProc()

    def fake_reset():
        if setup_failure:
            raise blood_impact.ProbeSetupError(
                "blood.clear() returned synthetic setup failure")
        return {"cleared": True, "remaining_count": 0}

    def fake_blood(kind, severity, _label, _rep, styles=None):
        fake_reset()
        opacity = (high_opacity if kind == "stab" and severity > 0.5
                   else 0.2)
        return {
            "woundKind": kind,
            "severity": ("major" if kind in ("arterial", "severed")
                         else "moderate"),
            "opacity": opacity,
            "style": (styles or ("pool",))[0],
        }

    def fake_no_blood(kind, severity, _label, _rep):
        fake_reset()
        return {"kind": kind, "severity": severity, "decal_count": 0}

    saved = (blood_impact.boot, blood_impact.quit_engine,
             blood_impact.bootstrap_defs, blood_impact.init_arena,
             blood_impact.expect_blood, blood_impact.expect_no_blood,
             blood_impact.reset_blood)
    blood_impact.boot = fake_boot
    blood_impact.quit_engine = lambda *a, **k: None
    blood_impact.bootstrap_defs = lambda _port: None
    blood_impact.init_arena = lambda _port: None
    blood_impact.expect_blood = fake_blood
    blood_impact.expect_no_blood = fake_no_blood
    blood_impact.reset_blood = fake_reset
    try:
        rc = blood_impact._run(9010, rep)
    finally:
        (blood_impact.boot, blood_impact.quit_engine,
         blood_impact.bootstrap_defs, blood_impact.init_arena,
         blood_impact.expect_blood, blood_impact.expect_no_blood,
         blood_impact.reset_blood) = saved
    return rc, launches


def test_blood_impact_standalone() -> None:
    print("\n-- blood_impact probe migration --")
    ids = ("stab_style", "stab_severity_scaling", "slash_style",
           "ordinary_blunt_dry", "ordinary_fracture_concussion_dry",
           "catastrophic_blunt_family_blood",
           "arterial_severed_volume_floor", "internal_dry",
           "clear_removes_decals")
    if support.migration_descriptor("blood_impact_probe.py", "blood_impact",
                             ids) is None:
        return

    import io
    import blood_impact_probe as blood_impact  # type: ignore

    standalone = io.StringIO()
    rc, launches = _drive_blood_impact(
        probe_protocol.Reporter(blood_impact.DESCRIPTOR, stream=standalone))
    expect(rc == 0, f"blood_impact standalone exits 0 (got {rc})")
    expect(standalone.getvalue().count("[PASS]") == len(ids),
           "blood_impact standalone prints one human PASS line per stable check")
    expect(launches["engine"]["args"] == [],
           "blood_impact standalone passes no RTS override")
    expect(launches["engine"]["log"] == blood_impact.LOG,
           "blood_impact standalone preserves its historical engine-log path")

    with tempfile.TemporaryDirectory() as tmp:
        events = Path(tmp) / "events.jsonl"
        stream = io.StringIO()
        rep = probe_protocol.Reporter(
            blood_impact.DESCRIPTOR, events_path=str(events),
            engine_log_dir=tmp, rts_caps=4, stream=stream)
        rc, launches = _drive_blood_impact(rep, high_opacity=0.1)
        rep.close()
        _seen, outcomes = probe_protocol.parse_event_stream(
            events.read_text(encoding="utf-8"), blood_impact.DESCRIPTOR)
        expect(rc == 1
               and outcomes["stab_style"] == "PASS"
               and outcomes["stab_severity_scaling"] == "FAIL"
               and all(outcomes[cid] == "MISSING" for cid in ids[2:]),
               f"blood_impact attributes severity scaling and leaves only "
               f"unreached successors missing (got rc={rc}, {outcomes})")
        expect(stream.getvalue() == "",
               "blood_impact protocol mode prints nothing to stdout")
        expect(launches["engine"]["args"] == ["+RTS", "-N4", "-RTS"]
               and launches["engine"]["log"]
               == os.path.join(tmp, blood_impact.LOG_NAME),
               "blood_impact uses harness RTS and isolated engine log")

    with tempfile.TemporaryDirectory() as tmp:
        events = Path(tmp) / "events.jsonl"
        rep = probe_protocol.Reporter(
            blood_impact.DESCRIPTOR, events_path=str(events),
            engine_log_dir=tmp, rts_caps=4)
        rc, _launches = _drive_blood_impact(rep, setup_failure=True)
        rep.close()
        seen, outcomes = probe_protocol.parse_event_stream(
            events.read_text(encoding="utf-8"), blood_impact.DESCRIPTOR)
        expect(rc == 2 and all(value == "MISSING" for value in outcomes.values()),
               "blood_impact preserves exit 2 and leaves checks missing on setup abort")
        expect(any(isinstance(event, probe_protocol.DiagnosticEvent)
                   and event.level == "WARN" for event in seen),
               "blood_impact records a setup abort as a protocol diagnostic")


TESTS = (test_blood_impact_standalone,)
