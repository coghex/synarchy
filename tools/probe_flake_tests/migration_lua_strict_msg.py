#!/usr/bin/env python3
"""lua_strict_msg's `probe-result/v1` migration contract (#2087).

Drives the real `tools/lua_strict_msg_probe.py` through a faked console so the
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

PROBE = "lua_strict_msg"

def _drive_lua_strict_msg(rep, *, alive=True, console_error=None):
    """Drive the real strict-message control flow without booting an engine."""
    import lua_strict_msg_probe as strict_msg  # type: ignore

    launches: dict = {"commands": [], "sleeps": []}

    class FakeProc:
        def poll(self):
            return None if alive else 1

    def fake_boot(port, log=None, args=None, **_kw):
        launches["engine"] = {"port": port, "log": log,
                              "args": list(args or [])}
        return FakeProc()

    def fake_send(_port, lua, **kwargs):
        launches["commands"].append({"lua": lua, "kwargs": kwargs})
        if "engine.setText" in lua:
            return ""
        if "return 1+1" in lua:
            if console_error is not None:
                raise OSError(console_error)
            return "2"
        raise AssertionError(f"unexpected console command: {lua!r}")

    saved = (strict_msg.boot, strict_msg.quit_engine, strict_msg.send,
             strict_msg.time.sleep)
    strict_msg.boot = fake_boot
    strict_msg.quit_engine = lambda *a, **k: None
    strict_msg.send = fake_send
    strict_msg.time.sleep = lambda seconds: launches["sleeps"].append(seconds)
    try:
        rc = strict_msg._run(9622, rep)
    finally:
        (strict_msg.boot, strict_msg.quit_engine, strict_msg.send,
         strict_msg.time.sleep) = saved
    return rc, launches


def test_lua_strict_msg_standalone() -> None:
    print("\n-- lua_strict_msg probe migration --")
    done = subprocess.run(
        [sys.executable, "tools/lua_strict_msg_probe.py", "--describe"],
        cwd=support.REPO_ROOT, text=True, capture_output=True, timeout=60)
    expect(done.returncode == 0,
           "lua_strict_msg --describe exits 0 without booting anything")
    try:
        descriptor = probe_protocol.parse_descriptor(
            done.stdout, expected_probe="lua_strict_msg")
    except probe_protocol.ProtocolError as error:
        expect(False, f"lua_strict_msg's descriptor is valid "
                      f"probe-result/v1 ({error})")
        return
    expect(descriptor.ids == ("engine_alive", "console_responsive"),
           f"lua_strict_msg declares its two stable checks in dependency "
           f"order (got {descriptor.ids})")

    import io
    import lua_strict_msg_probe as strict_msg  # type: ignore

    standalone = io.StringIO()
    standalone_rep = probe_protocol.Reporter(strict_msg.DESCRIPTOR,
                                             stream=standalone)
    rc, launches = _drive_lua_strict_msg(standalone_rep)
    expect(rc == 0, f"the standalone probe still exits 0 (got {rc})")
    expect(standalone.getvalue().count("[PASS]") == 2,
           "standalone mode prints one human [PASS] line per check")
    expect(launches["engine"]["args"] == [],
           "standalone mode passes no RTS override")
    expect(launches["engine"]["log"] == strict_msg.LOG,
           "standalone mode keeps its historical engine-log path")
    expect(launches["sleeps"] == [1.0],
           "the post-message crash window remains one second")
    malformed = launches["commands"][0]
    expect("caf\\195" in malformed["lua"]
           and malformed["kwargs"].get("expect_result") is False,
           "the malformed fire-and-forget message is unchanged")

    with tempfile.TemporaryDirectory() as tmp:
        events = Path(tmp) / "events.jsonl"
        protocol_stream = io.StringIO()
        protocol_rep = probe_protocol.Reporter(
            strict_msg.DESCRIPTOR, events_path=str(events),
            engine_log_dir=tmp, rts_caps=4, stream=protocol_stream)
        rc, launches = _drive_lua_strict_msg(protocol_rep)
        protocol_rep.close()
        expect(rc == 0, f"a clean protocol run exits 0 (got {rc})")
        expect(protocol_stream.getvalue() == "",
               "protocol mode prints nothing to stdout")
        expect(launches["engine"]["args"] == ["+RTS", "-N4", "-RTS"],
               "the harness's RTS capabilities reach the engine")
        expect(launches["engine"]["log"]
               == os.path.join(tmp, strict_msg.LOG_NAME),
               "the engine log is isolated inside the harness run")
        event_text = events.read_text(encoding="utf-8")
        _events, outcomes = probe_protocol.parse_event_stream(
            event_text, strict_msg.DESCRIPTOR)
        expect(outcomes == {"engine_alive": "PASS",
                            "console_responsive": "PASS"},
               f"the real probe flow reports both checks in order "
               f"(got {outcomes})")
        expect(probe_protocol.forbidden_marker_lines(event_text) == [],
               "the protocol event stream has no bracketed result markers")

    with tempfile.TemporaryDirectory() as tmp:
        events = Path(tmp) / "events.jsonl"
        dead_rep = probe_protocol.Reporter(
            strict_msg.DESCRIPTOR, events_path=str(events),
            engine_log_dir=tmp, rts_caps=4)
        rc, launches = _drive_lua_strict_msg(dead_rep, alive=False)
        dead_rep.close()
        event_text = events.read_text(encoding="utf-8")
        _events, outcomes = probe_protocol.parse_event_stream(
            event_text, strict_msg.DESCRIPTOR)
        expect(rc == 1, f"an exited engine makes the probe fail (got {rc})")
        expect(outcomes == {"engine_alive": "FAIL",
                            "console_responsive": "MISSING"},
               f"dead-engine attribution is FAIL then MISSING "
               f"(got {outcomes})")
        expect(len(launches["commands"]) == 1,
               "the probe does not query a console after its engine exited")
        expect('"level": "SKIP"' in event_text,
               "the uncheckable responsiveness assertion is diagnosed as SKIP")

    with tempfile.TemporaryDirectory() as tmp:
        events = Path(tmp) / "events.jsonl"
        unreachable_rep = probe_protocol.Reporter(
            strict_msg.DESCRIPTOR, events_path=str(events),
            engine_log_dir=tmp, rts_caps=4)
        rc, _launches = _drive_lua_strict_msg(
            unreachable_rep, console_error="connection dropped")
        unreachable_rep.close()
        _events, outcomes = probe_protocol.parse_event_stream(
            events.read_text(encoding="utf-8"), strict_msg.DESCRIPTOR)
        expect(rc == 1, f"an unreachable console makes the probe fail (got {rc})")
        expect(outcomes == {"engine_alive": "PASS",
                            "console_responsive": "FAIL"},
               f"console failure is attributed after liveness passes "
               f"(got {outcomes})")


TESTS = (test_lua_strict_msg_standalone,)
