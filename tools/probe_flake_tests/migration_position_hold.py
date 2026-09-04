#!/usr/bin/env python3
"""position_hold's `probe-result/v1` migration contract (#2087).

Drives the real `tools/position_hold_probe.py` through a faked console so the
probe's standalone behaviour, its structured-result parity and its
failure attribution are pinned without booting an engine.
"""
from __future__ import annotations

import os
import subprocess
import sys
import tempfile
from pathlib import Path

from . import support
from .support import probe_protocol
from .support import expect

PROBE = "position_hold"

def test_position_hold_standalone() -> None:
    print("\n-- position_hold probe migration --")
    done = subprocess.run(
        [sys.executable, "tools/position_hold_probe.py", "--describe"],
        cwd=support.REPO_ROOT, text=True, capture_output=True, timeout=60)
    expect(done.returncode == 0,
           "position_hold --describe exits 0 without booting anything")
    try:
        descriptor = probe_protocol.parse_descriptor(
            done.stdout, expected_probe="position_hold")
    except probe_protocol.ProtocolError as error:
        expect(False,
               f"position_hold's descriptor is valid probe-result/v1 ({error})")
        return
    expect(len(descriptor.ids) == 12,
           f"position_hold declares its twelve checks (got {len(descriptor.ids)})")
    expect(len(set(descriptor.ids)) == len(descriptor.ids),
           "position_hold's check identifiers are unique")
    expect(all(probe_protocol.CHECK_ID_RE.match(cid) for cid in descriptor.ids),
           "position_hold's identifiers are all stable, word-like identifiers")
    # The human labels these replaced interpolate observed tiles and
    # elapsed seconds; identity must carry none of that.
    expect(not any(any(ch.isdigit() for ch in cid) for cid in descriptor.ids),
           "position_hold's identifiers carry no runtime values")

    # Standalone mode still prints the bracketed human markers, and
    # protocol mode never does.
    import position_hold_probe  # type: ignore
    import io
    stream = io.StringIO()
    rep = probe_protocol.Reporter(position_hold_probe.DESCRIPTOR, stream=stream)
    rep.check("hold_sustained", True, "stayed within 0.05 tiles of the anchor")
    rep.abort("the commanded acolyte never arrived and never held")
    expect("[PASS] stayed within 0.05 tiles of the anchor" in stream.getvalue(),
           "standalone position_hold still prints its bracketed [PASS] line")
    expect("[FAIL] the commanded acolyte never arrived and never held"
           in stream.getvalue(),
           "standalone position_hold still prints a setup abort as [FAIL]")
    expect(rep.engine_args() == [],
           "standalone position_hold passes no RTS override")
    expect(rep.engine_log_path("position_hold_engine.log", "/tmp/x.log")
           == "/tmp/x.log",
           "standalone position_hold keeps its own engine-log path")

    with tempfile.TemporaryDirectory() as tmp:
        events = Path(tmp) / "events.jsonl"
        protocol_rep = probe_protocol.Reporter(
            position_hold_probe.DESCRIPTOR, events_path=str(events),
            engine_log_dir=tmp, rts_caps=4, stream=stream)
        before = stream.getvalue()
        protocol_rep.check("hold_created", True, "human text",
                           {"anchor": [4, 4]})
        protocol_rep.abort("setup failed")
        protocol_rep.close()
        expect(stream.getvalue() == before,
               "protocol mode prints nothing to stdout")
        expect(protocol_rep.engine_args() == ["+RTS", "-N4", "-RTS"],
               "protocol mode pins the engine to the harness's RTS capabilities")
        expect(protocol_rep.engine_log_path(
                   "position_hold_engine.log", "/tmp/x.log")
               == os.path.join(tmp, "position_hold_engine.log"),
               "protocol mode stops position_hold overwriting its shared /tmp "
               "engine log")
        text = events.read_text(encoding="utf-8")
        _events, outcomes = probe_protocol.parse_event_stream(
            text, position_hold_probe.DESCRIPTOR)
        expect(outcomes["hold_created"] == "PASS",
               "the protocol event stream carries the check outcome")
        expect(probe_protocol.forbidden_marker_lines(text) == [],
               "the event stream itself holds no bracketed marker lines")
        expect('"level": "WARN"' in text,
               "a setup abort is a WARN diagnostic in protocol mode, so the "
               "checks it prevented stay MISSING")


TESTS = (test_position_hold_standalone,)
