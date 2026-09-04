#!/usr/bin/env python3
"""state_of_mind's `probe-result/v1` migration contract (#2087).

Drives the real `tools/state_of_mind_probe.py` through a faked console so the
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

PROBE = "state_of_mind"

def _drive_state_of_mind(rep, *, fail_delirious=False):
    import state_of_mind_probe as state  # type: ignore

    launches = {}
    summary_calls = 0

    class FakeProc:
        pass

    def fake_boot(port, log=None, args=None, **_kw):
        launches["engine"] = {"port": port, "log": log,
                              "args": list(args or [])}
        return FakeProc()

    baseline = {"state": "alert", "consciousness": 1.0, "mood": 1.0,
                "emotionalPain": 0.0, "concentration": 1.0,
                "stateOfMind": 1.0}

    def fake_summary(_port, _uid):
        nonlocal summary_calls
        summary_calls += 1
        if summary_calls <= 2:
            return dict(baseline)
        if summary_calls == 3:
            value = dict(baseline)
            value.update({"concentration": 0.88, "mood": 0.99,
                          "emotionalPain": 0.10, "stateOfMind": 0.94})
            return value
        if summary_calls <= 11:
            step = summary_calls - 4
            value = dict(baseline)
            value.update({"mood": 0.99 - step * 0.002,
                          "emotionalPain": 0.10 + step * 0.01,
                          "stateOfMind": 0.90})
            return value
        step = summary_calls - 12
        value = dict(baseline)
        value.update({"mood": 1.0 - step * 0.004})
        return value

    def fake_send(_port, lua, **_kw):
        if "unit.getPain" in lua:
            return "1.0"
        if "brain').awareness" in lua:
            return "0.2"
        if "unit.spawn" in lua:
            if "bear_brown',5" in lua:
                return "2"
            if "acolyte',13" in lua:
                return "3"
            if "bear_brown',9" in lua:
                return "4"
            return "1"
        return "ok"

    def alert_gate(som=0.9):
        return {"c": 1.0, "som": som, "mood": 0.8, "ep": 0.2,
                "pose": "standing", "uncon": False, "delir": False,
                "conf": False, "state": "alert"}

    def fake_band(_port, _uid, mood, emotional_pain, timeout=8.0):
        if mood == 0.75:
            return alert_gate(0.55), None
        if mood == 0.57:
            gate = alert_gate(0.27)
            if fail_delirious:
                gate.update({"delir": True, "state": "delirious"})
            return gate, None
        return alert_gate(0.06), None

    saved = (state.boot, state.quit_engine, state.bootstrap, state.send,
             state.summary, state.gate_observation, state.band_fixture,
             state.time.sleep)
    state.boot = fake_boot
    state.quit_engine = lambda *a, **k: None
    state.bootstrap = lambda _port: None
    state.send = fake_send
    state.summary = fake_summary
    state.gate_observation = lambda _port, _uid: alert_gate(0.9)
    state.band_fixture = fake_band
    state.time.sleep = lambda _seconds: None
    try:
        rc = state._run(9350, rep)
    finally:
        (state.boot, state.quit_engine, state.bootstrap, state.send,
         state.summary, state.gate_observation, state.band_fixture,
         state.time.sleep) = saved
    return rc, launches


def test_state_of_mind_standalone() -> None:
    print("\n-- state_of_mind probe migration --")
    import io
    import state_of_mind_probe as state  # type: ignore

    ids = tuple(check_id for check_id, _label in state.CHECKS)
    if support.migration_descriptor("state_of_mind_probe.py", "state_of_mind",
                             ids) is None:
        return
    expect(len(ids) == 11 and len(set(ids)) == 11,
           "state_of_mind declares all 11 unique checks")

    standalone = io.StringIO()
    rc, launches = _drive_state_of_mind(
        probe_protocol.Reporter(state.DESCRIPTOR, stream=standalone))
    expect(rc == 0, f"state_of_mind standalone exits 0 (got {rc})")
    expect(standalone.getvalue().count("[PASS]") == 11,
           "state_of_mind standalone prints 11 human PASS lines")
    expect(launches["engine"] == {"port": 9350, "log": state.LOG,
                                  "args": []},
           "state_of_mind preserves standalone launch behavior")

    with tempfile.TemporaryDirectory() as tmp:
        events = Path(tmp) / "events.jsonl"
        stream = io.StringIO()
        rep = probe_protocol.Reporter(
            state.DESCRIPTOR, events_path=str(events),
            engine_log_dir=tmp, rts_caps=4, stream=stream)
        rc, launches = _drive_state_of_mind(rep, fail_delirious=True)
        rep.close()
        _seen, outcomes = probe_protocol.parse_event_stream(
            events.read_text(encoding="utf-8"), state.DESCRIPTOR)
        expect(rc == 1
               and outcomes["delirious_band_gate_isolation"] == "FAIL",
               "state_of_mind attributes the failed delirious-band guard")
        expect(all(value == "PASS" for key, value in outcomes.items()
                   if key != "delirious_band_gate_isolation"),
               "state_of_mind preserves its accumulate-all behavior")
        expect(stream.getvalue() == "",
               "state_of_mind protocol mode prints nothing to stdout")
        expect(launches["engine"]["args"] == ["+RTS", "-N4", "-RTS"]
               and launches["engine"]["log"]
               == os.path.join(tmp, state.LOG_NAME),
               "state_of_mind uses harness RTS and isolated engine log")


TESTS = (test_state_of_mind_standalone,)
