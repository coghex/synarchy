#!/usr/bin/env python3
"""role's `probe-result/v1` migration contract (#2087).

Drives the real `tools/role_probe.py` through a faked console so the
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

PROBE = "role"

def test_role_standalone() -> None:
    print("\n-- role probe migration --")
    done = subprocess.run(
        [sys.executable, "tools/role_probe.py", "--describe"],
        cwd=support.REPO_ROOT, text=True, capture_output=True, timeout=60)
    expect(done.returncode == 0,
           "role --describe exits 0 without booting anything")
    try:
        descriptor = probe_protocol.parse_descriptor(done.stdout,
                                                     expected_probe="role")
    except probe_protocol.ProtocolError as error:
        expect(False, f"role's descriptor is valid probe-result/v1 ({error})")
        return
    expect(len(descriptor.ids) == 10,
           f"role declares its ten checks (got {len(descriptor.ids)})")
    expect(len(set(descriptor.ids)) == len(descriptor.ids),
           "role's check identifiers are unique")
    expect(all(probe_protocol.CHECK_ID_RE.match(cid) for cid in descriptor.ids),
           "role's identifiers are all stable, word-like identifiers")
    # The labels these replaced interpolated skill numbers and observed
    # roles; identity must carry none of that.
    expect(not any(any(ch.isdigit() for ch in cid) for cid in descriptor.ids),
           "role's identifiers carry no runtime values")

    # Standalone mode still prints the bracketed human markers, and
    # protocol mode never does.
    import role_probe  # type: ignore
    import io
    stream = io.StringIO()
    rep = probe_protocol.Reporter(role_probe.DESCRIPTOR, stream=stream)
    rep.check("derive_miner", True, "mining 60 -> miner: miner")
    rep.abort("no wood-harvestable flora")
    expect("[PASS] mining 60 -> miner: miner" in stream.getvalue(),
           "standalone role still prints its bracketed [PASS] line")
    expect("[FAIL] no wood-harvestable flora" in stream.getvalue(),
           "standalone role still prints a setup abort as [FAIL]")
    expect(rep.engine_args() == [],
           "standalone role passes no RTS override")
    expect(rep.engine_log_path("role_probe_engine.log", "/tmp/x.log")
           == "/tmp/x.log",
           "standalone role keeps its own engine-log path")

    with tempfile.TemporaryDirectory() as tmp:
        events = Path(tmp) / "events.jsonl"
        protocol_rep = probe_protocol.Reporter(
            role_probe.DESCRIPTOR, events_path=str(events),
            engine_log_dir=tmp, rts_caps=4, stream=stream)
        before = stream.getvalue()
        protocol_rep.check("derive_initial", True, "human text", {"role": "miner"})
        protocol_rep.abort("setup failed")
        protocol_rep.close()
        expect(stream.getvalue() == before,
               "protocol mode prints nothing to stdout")
        expect(protocol_rep.engine_args() == ["+RTS", "-N4", "-RTS"],
               "protocol mode pins the engine to the harness's RTS capabilities")
        expect(protocol_rep.engine_log_path("role_probe_engine.log", "/tmp/x.log")
               == os.path.join(tmp, "role_probe_engine.log"),
               "protocol mode stops role overwriting its shared /tmp engine log")
        text = events.read_text(encoding="utf-8")
        _events, outcomes = probe_protocol.parse_event_stream(
            text, role_probe.DESCRIPTOR)
        expect(outcomes["derive_initial"] == "PASS",
               "the protocol event stream carries the check outcome")
        expect(probe_protocol.forbidden_marker_lines(text) == [],
               "the event stream itself holds no bracketed marker lines")
        expect('"level": "WARN"' in text,
               "a setup abort is a WARN diagnostic in protocol mode, so the "
               "checks it prevented stay MISSING")


TESTS = (test_role_standalone,)
