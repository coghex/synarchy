#!/usr/bin/env python3
"""meal_waste's `probe-result/v1` migration contract (#2087).

Drives the real `tools/meal_waste_probe.py` through a faked console so the
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

PROBE = "meal_waste"

def _drive_meal_waste(rep, *, fail_first_item=False):
    import meal_waste_probe as meal_waste  # type: ignore

    launches = {}
    spawned = 0
    reports = [
        {"ok": True, "calls": 2, "fed": 2, "salted": 2,
         "rations": 1, "sacks": 0, "fill": 0.0,
         "maxHunger": 713.0, "startHunger": 153.0, "hunger": 653.0},
        {"ok": True, "calls": 3, "fed": 3, "salted": 3,
         "rations": 1, "sacks": 1, "fill": 0.0,
         "maxHunger": 713.0, "startHunger": 153.0, "hunger": 710.0},
        {"ok": True, "calls": 1, "fed": 1, "salted": 1,
         "rations": 2, "sacks": 0, "fill": 0.0,
         "maxHunger": 713.0, "startHunger": 613.0, "hunger": 713.0},
        {"ok": True, "calls": 3, "fed": 3, "salted": 3,
         "rations": 0, "sacks": 0, "fill": 0.0,
         "maxHunger": 713.0, "startHunger": 0.0, "hunger": 713.0},
        {"ok": True, "calls": 10, "fed": 10, "salted": 10,
         "rations": 0, "sacks": 2, "fill": 0.01,
         "maxHunger": 713.0, "startHunger": 0.0, "hunger": 184.0},
    ]

    class FakeProc:
        pass

    def fake_boot(port, log=None, args=None, **_kw):
        launches["engine"] = {"port": port, "log": log,
                              "args": list(args or [])}
        return FakeProc()

    def fake_spawn(_port, _x, _y):
        nonlocal spawned
        spawned += 1
        return spawned

    def fake_meal(_port, _uid, _give, _set_hunger):
        index = fake_meal.calls
        fake_meal.calls += 1
        result = dict(reports[index])
        if fail_first_item and index == 2:
            result["fed"] = 0
        return result

    fake_meal.calls = 0

    def fake_send_json(_port, _lua, timeout=None):
        return {"eat": 4.0, "expected": 4.0, "forageBlocked": True}

    saved = (meal_waste.boot, meal_waste.quit_engine,
             meal_waste.bootstrap, meal_waste.init_arena,
             meal_waste.spawn, meal_waste.meal, meal_waste.send_json)
    meal_waste.boot = fake_boot
    meal_waste.quit_engine = lambda *a, **k: None
    meal_waste.bootstrap = lambda _port: None
    meal_waste.init_arena = lambda _port: None
    meal_waste.spawn = fake_spawn
    meal_waste.meal = fake_meal
    meal_waste.send_json = fake_send_json
    try:
        rc = meal_waste._run(9192, rep)
    finally:
        (meal_waste.boot, meal_waste.quit_engine,
         meal_waste.bootstrap, meal_waste.init_arena,
         meal_waste.spawn, meal_waste.meal, meal_waste.send_json) = saved
    return rc, launches


def test_meal_waste_standalone() -> None:
    print("\n-- meal_waste probe migration --")
    ids = ("withholds_marginal_ration", "bulk_finishes_meal",
           "first_item_exempt", "starving_eats_rations",
           "feed_bound_preserved", "entry_gates_unchanged")
    if support.migration_descriptor("meal_waste_probe.py", "meal_waste", ids) is None:
        return

    import io
    import meal_waste_probe as meal_waste  # type: ignore

    standalone = io.StringIO()
    rc, launches = _drive_meal_waste(
        probe_protocol.Reporter(meal_waste.DESCRIPTOR, stream=standalone))
    expect(rc == 0, f"meal_waste standalone exits 0 (got {rc})")
    expect(standalone.getvalue().count("[PASS]") == len(ids),
           "meal_waste standalone prints one human PASS line per stable check")
    expect("ALL MEAL-WASTE CHECKS PASSED" in standalone.getvalue(),
           "meal_waste standalone preserves its human completion summary")
    expect(launches["engine"]["args"] == [],
           "meal_waste standalone passes no RTS override")
    expect(launches["engine"]["log"] == meal_waste.LOG,
           "meal_waste standalone preserves its historical engine-log path")

    with tempfile.TemporaryDirectory() as tmp:
        events = Path(tmp) / "events.jsonl"
        stream = io.StringIO()
        rep = probe_protocol.Reporter(
            meal_waste.DESCRIPTOR, events_path=str(events),
            engine_log_dir=tmp, rts_caps=4, stream=stream)
        rc, launches = _drive_meal_waste(rep, fail_first_item=True)
        rep.close()
        _seen, outcomes = probe_protocol.parse_event_stream(
            events.read_text(encoding="utf-8"), meal_waste.DESCRIPTOR)
        expect(rc == 1
               and outcomes["first_item_exempt"] == "FAIL"
               and all(outcomes[cid] == "PASS"
                       for cid in ids if cid != "first_item_exempt"),
               f"meal_waste attributes one failed check without losing its "
               f"independent successors (got rc={rc}, {outcomes})")
        expect(stream.getvalue() == "",
               "meal_waste protocol mode prints nothing to stdout")
        expect(launches["engine"]["args"] == ["+RTS", "-N4", "-RTS"]
               and launches["engine"]["log"]
               == os.path.join(tmp, meal_waste.LOG_NAME),
               "meal_waste uses harness RTS and isolated engine log")


TESTS = (test_meal_waste_standalone,)
