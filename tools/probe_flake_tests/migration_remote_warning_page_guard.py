#!/usr/bin/env python3
"""remote_warning_page_guard's `probe-result/v1` migration contract (#2087).

Drives the real `tools/remote_warning_page_guard_probe.py` through a faked console so the
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

PROBE = "remote_warning_page_guard"

def _drive_remote_warning(rep, *, remote_ok=True):
    import remote_warning_page_guard_probe as remote  # type: ignore

    launches = {}
    drains = iter((
        [],
        [{"outcome": "presented"}],
        [{"outcome": "confirmed"},
         {"outcome": "revalidationRejected",
          "reason": "active world changed"}],
        [],
        [{"kind": "buildTool.commitPlacement", "outcome": "accepted"}],
        [],
        [{"outcome": "canceled"}],
    ))

    class FakeProc:
        pass

    def fake_boot(port, log=None, args=None, **_kw):
        launches["engine"] = {"port": port, "log": log,
                              "args": list(args or [])}
        return FakeProc()

    def fake_send(_port, lua, **_kw):
        if "building.list" in lua:
            count = launches.setdefault("list_calls", 0)
            launches["list_calls"] = count + 1
            return "" if count == 0 else remote.PORTAL
        return "ok"

    def fake_send_json(_port, lua, **_kw):
        if "building.canPlaceAt" in lua:
            return True
        if "building.remoteCheck" in lua:
            return remote_ok
        if "rw.isOpen" in lua:
            return "rw.open" in lua
        raise AssertionError(f"unexpected send_json command: {lua!r}")

    saved = (remote.boot, remote.quit_engine, remote.load_defs,
             remote.init_arena, remote.show_and_wait, remote.drain,
             remote.send, remote.send_json)
    remote.boot = fake_boot
    remote.quit_engine = lambda *a, **k: None
    remote.load_defs = lambda _port: None
    remote.init_arena = lambda _port, _name: None
    remote.show_and_wait = lambda _port, _name: True
    remote.drain = lambda _port: next(drains)
    remote.send = fake_send
    remote.send_json = fake_send_json
    try:
        rc = remote._run(9421, rep)
    finally:
        (remote.boot, remote.quit_engine, remote.load_defs,
         remote.init_arena, remote.show_and_wait, remote.drain,
         remote.send, remote.send_json) = saved
    return rc, launches


def test_remote_warning_page_guard_standalone() -> None:
    print("\n-- remote_warning_page_guard probe migration --")
    import io
    import remote_warning_page_guard_probe as remote  # type: ignore

    ids = tuple(check_id for check_id, _label in remote.CHECKS)
    if support.migration_descriptor("remote_warning_page_guard_probe.py",
                             "remote_warning_page_guard", ids) is None:
        return
    expect(len(ids) == 18 and len(set(ids)) == 18,
           "remote_warning_page_guard declares all 18 unique checks")

    standalone = io.StringIO()
    rc, launches = _drive_remote_warning(
        probe_protocol.Reporter(remote.DESCRIPTOR, stream=standalone))
    expect(rc == 0, f"remote_warning_page_guard standalone exits 0 (got {rc})")
    expect(standalone.getvalue().count("[PASS]") == 18,
           "remote_warning_page_guard standalone prints 18 human PASS lines")
    expect(launches["engine"] == {"port": 9421, "log": remote.LOG, "args": []},
           "remote_warning_page_guard preserves standalone launch behavior")

    with tempfile.TemporaryDirectory() as tmp:
        events = Path(tmp) / "events.jsonl"
        stream = io.StringIO()
        rep = probe_protocol.Reporter(
            remote.DESCRIPTOR, events_path=str(events),
            engine_log_dir=tmp, rts_caps=4, stream=stream)
        rc, launches = _drive_remote_warning(rep, remote_ok=False)
        rep.close()
        _seen, outcomes = probe_protocol.parse_event_stream(
            events.read_text(encoding="utf-8"), remote.DESCRIPTOR)
        expect(rc == 1 and outcomes["remote_position_valid"] == "FAIL",
               "remote_warning_page_guard attributes an invalid remote fixture")
        expect(all(value == "PASS" for key, value in outcomes.items()
                   if key != "remote_position_valid"),
               "remote_warning_page_guard continues reporting after one failure")
        expect(stream.getvalue() == "",
               "remote_warning_page_guard protocol mode prints nothing to stdout")
        expect(launches["engine"]["args"] == ["+RTS", "-N4", "-RTS"]
               and launches["engine"]["log"]
               == os.path.join(tmp, remote.LOG_NAME),
               "remote_warning_page_guard uses harness RTS and isolated log")


TESTS = (test_remote_warning_page_guard_standalone,)
