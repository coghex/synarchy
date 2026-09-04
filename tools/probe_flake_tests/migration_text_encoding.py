#!/usr/bin/env python3
"""text_encoding's `probe-result/v1` migration contract (#2087).

Drives the real `tools/text_encoding_probe.py` through a faked console so the
probe's standalone behaviour, its structured-result parity and its
failure attribution are pinned without booting an engine.
"""
from __future__ import annotations

import argparse
import os
import subprocess
import sys
import tempfile
from pathlib import Path

from . import support
from .support import probe_protocol
from .support import expect

PROBE = "text_encoding"

def _drive_text_encoding(rep, *, malformed_call_result="no_error",
                         alive=True):
    """Drive the real text-encoding control flow without booting an engine."""
    import text_encoding_probe as text_encoding  # type: ignore

    launches: dict = {"commands": []}

    class FakeProc:
        def poll(self):
            return None if alive else 1

    def fake_boot(port, log=None, args=None, **_kw):
        launches["engine"] = {"port": port, "log": log,
                              "args": list(args or [])}
        return FakeProc()

    def fake_send(_port, lua, **_kw):
        launches["commands"].append(lua)
        if "engine.setText(1" in lua:
            return "no_error"
        # Neither id names a scene node, so post-#1961 the handler
        # caches nothing and getText answers the console's nil rendering.
        if "engine.getText(1)" in lua:
            return "null"
        if "engine.setText(2" in lua:
            return malformed_call_result
        if "engine.getText(2)" in lua:
            return "null"
        if 'world.show("no_such_page")' in lua:
            return "no_error"
        if 'world.show("caf' in lua and "no_such_page" not in lua:
            return "no_error"
        if "return 1+1" in lua:
            return "2"
        raise AssertionError(f"unexpected console command: {lua!r}")

    saved = (text_encoding.boot, text_encoding.quit_engine,
             text_encoding.send)
    text_encoding.boot = fake_boot
    text_encoding.quit_engine = lambda *a, **k: None
    text_encoding.send = fake_send
    try:
        args = argparse.Namespace(port=9618, describe=False)
        rc = text_encoding._run(args, args.port, rep)
    finally:
        (text_encoding.boot, text_encoding.quit_engine,
         text_encoding.send) = saved
    return rc, launches


def test_text_encoding_standalone() -> None:
    print("\n-- text_encoding probe migration --")
    done = subprocess.run(
        [sys.executable, "tools/text_encoding_probe.py", "--describe"],
        cwd=support.REPO_ROOT, text=True, capture_output=True, timeout=60)
    expect(done.returncode == 0,
           "text_encoding --describe exits 0 without booting anything")
    try:
        descriptor = probe_protocol.parse_descriptor(
            done.stdout, expected_probe="text_encoding")
    except probe_protocol.ProtocolError as error:
        expect(False, f"text_encoding's descriptor is valid "
                      f"probe-result/v1 ({error})")
        return
    expect(len(descriptor.ids) == 8,
           f"text_encoding declares its eight checks "
           f"(got {len(descriptor.ids)})")
    expect(len(set(descriptor.ids)) == len(descriptor.ids),
           "text_encoding's check identifiers are unique")
    expect(all(probe_protocol.CHECK_ID_RE.match(cid) for cid in descriptor.ids),
           "text_encoding's identifiers are stable word-like identifiers")
    expect(not any(any(ch.isdigit() for ch in cid) for cid in descriptor.ids),
           "text_encoding's identifiers carry no runtime values")

    import io
    import text_encoding_probe as text_encoding  # type: ignore

    standalone = io.StringIO()
    standalone_rep = probe_protocol.Reporter(text_encoding.DESCRIPTOR,
                                             stream=standalone)
    rc, launches = _drive_text_encoding(standalone_rep)
    expect(rc == 0, f"the standalone probe still exits 0 (got {rc})")
    expect(standalone.getvalue().count("[PASS]") == 8,
           "standalone mode prints one human [PASS] line per check")
    expect(launches["engine"]["args"] == [],
           "standalone mode passes no RTS override")
    expect(launches["engine"]["log"] == text_encoding.LOG,
           "standalone mode keeps its historical engine-log path")

    with tempfile.TemporaryDirectory() as tmp:
        events = Path(tmp) / "events.jsonl"
        protocol_stream = io.StringIO()
        protocol_rep = probe_protocol.Reporter(
            text_encoding.DESCRIPTOR, events_path=str(events),
            engine_log_dir=tmp, rts_caps=4, stream=protocol_stream)
        rc, launches = _drive_text_encoding(protocol_rep)
        protocol_rep.close()
        expect(rc == 0, f"a clean protocol run exits 0 (got {rc})")
        expect(protocol_stream.getvalue() == "",
               "protocol mode prints nothing to stdout")
        expect(launches["engine"]["args"] == ["+RTS", "-N4", "-RTS"],
               "the harness's RTS capabilities reach the engine")
        expect(launches["engine"]["log"]
               == os.path.join(tmp, text_encoding.LOG_NAME),
               "the engine log is isolated inside the harness run")
        event_text = events.read_text(encoding="utf-8")
        _events, outcomes = probe_protocol.parse_event_stream(
            event_text, text_encoding.DESCRIPTOR)
        expect(all(outcome == "PASS" for outcome in outcomes.values()),
               f"the real probe flow reports all eight checks in order "
               f"(got {outcomes})")
        expect(probe_protocol.forbidden_marker_lines(event_text) == [],
               "the protocol event stream has no bracketed result markers")

    # One behavior failure is a check failure, not a harness error, and
    # does not prevent later independent checks from being reported.
    with tempfile.TemporaryDirectory() as tmp:
        events = Path(tmp) / "events.jsonl"
        failing_rep = probe_protocol.Reporter(
            text_encoding.DESCRIPTOR, events_path=str(events),
            engine_log_dir=tmp, rts_caps=4)
        rc, _launches = _drive_text_encoding(
            failing_rep, malformed_call_result="lua_error")
        failing_rep.close()
        _events, outcomes = probe_protocol.parse_event_stream(
            events.read_text(encoding="utf-8"), text_encoding.DESCRIPTOR)
        expect(rc == 1, f"a malformed-setText regression exits 1 (got {rc})")
        expect(outcomes["malformed_text_call"] == "FAIL",
               "the malformed setText regression is attributed to its check")
        expect(outcomes["console_responsive"] == "PASS",
               "later independent checks are still reported after a failure")

    # If the process has died, the liveness check fails and the dependent
    # final responsiveness check honestly remains MISSING.
    with tempfile.TemporaryDirectory() as tmp:
        events = Path(tmp) / "events.jsonl"
        dead_rep = probe_protocol.Reporter(
            text_encoding.DESCRIPTOR, events_path=str(events),
            engine_log_dir=tmp, rts_caps=4)
        rc, launches = _drive_text_encoding(dead_rep, alive=False)
        dead_rep.close()
        event_text = events.read_text(encoding="utf-8")
        _events, outcomes = probe_protocol.parse_event_stream(
            event_text, text_encoding.DESCRIPTOR)
        expect(rc == 1, f"an exited engine makes the probe fail (got {rc})")
        expect(outcomes["engine_alive"] == "FAIL"
               and outcomes["console_responsive"] == "MISSING",
               f"dead-engine attribution is FAIL then MISSING (got {outcomes})")
        expect(not any("return 1+1" in cmd for cmd in launches["commands"]),
               "the probe does not query a console after its engine exited")
        expect('"level": "SKIP"' in event_text,
               "the uncheckable responsiveness assertion is diagnosed as SKIP")


TESTS = (test_text_encoding_standalone,)
