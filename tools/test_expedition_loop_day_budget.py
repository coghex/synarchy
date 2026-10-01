#!/usr/bin/env python3
"""Unit tests for the expedition gate's day budget (#2755).

`tools/expedition_loop_probe.py` runs on the world's own clock and never
moves it, so a live run can only show the budget's PASS side: a normal run
is home well before dusk. Everything a run that overruns its day would
print — the overrun failure, the timeout diagnostics, a sleeper named as
such, a console that has gone away — is exercised here instead, over
synthetic stage timings and a fake console. Nothing boots an engine.

Pinned:

  * the derivation from the shipped tunables (1260 s to eligibility, the
    15:00 window opening), and that the Lua sources still ship them;
  * the boundary immediately before / at / after the deadline;
  * the midnight wrap, and day numbering across it;
  * a wait that begins inside the budget and expires past it;
  * sleeping and non-sleeping actors, a missing unit, and a console that
    does not answer (reported, never raised);
  * the one deadline surviving the fresh process's load;
  * an overrun recording a failing stage and a nonzero exit through the
    real `Checks` accounting and the facade's own `main`.
"""
from __future__ import annotations

import contextlib
import io
import os
import re
import sys
import unittest
from unittest import mock

TOOLS = os.path.dirname(os.path.abspath(__file__))
REPO = os.path.dirname(TOOLS)
sys.path.insert(0, TOOLS)

from expedition_loop import day_budget as db  # noqa: E402
from expedition_loop.harness import Checks  # noqa: E402

SHIPPED = db.SleepTunables(min_deficit=0.35, drain_frac=1 / 3600,
                           center=0.75, width=0.125)
TEN_AM = 10 / 24


class FakeConsole:
    """Answers the DayClock's queries from settable values."""

    def __init__(self, t: float = 0.0, phase0: float = TEN_AM,
                 day_seconds: float = 1440.0) -> None:
        self.t = t
        self.t0 = t
        self.phase0 = phase0
        self.day_seconds = day_seconds
        self.actors: dict = {}
        self.dead = False

    def __call__(self, port, lua, *args, **kwargs) -> str:
        if self.dead:
            raise ConnectionRefusedError("[Errno 61] Connection refused")
        if lua == db.TUNABLES_LUA:
            return f"0.35,{1 / 3600!r},0.75,0.125,{1440.0 / self.day_seconds}"
        if lua == "return engine.gameTime()":
            return repr(self.t)
        if lua.startswith("return world.getSunAngleAt("):
            return repr((self.phase0 + (self.t - self.t0)
                         / self.day_seconds) % 1.0)
        m = re.search(r"unit\.exists\((\d+)\)", lua)
        if m:
            return self.actors.get(int(m.group(1)), "missing")
        raise AssertionError(f"unexpected console query: {lua}")


def quiet(fn, *args, **kwargs):
    """Run `fn`, returning (result, captured stdout)."""
    buf = io.StringIO()
    with contextlib.redirect_stdout(buf):
        out = fn(*args, **kwargs)
    return out, buf.getvalue()


def established(console: FakeConsole):
    chk = Checks()
    clock = db.DayClock(port=0, send=console)
    ok, _ = quiet(clock.establish, chk, (5, 5))
    assert ok and clock.budget is not None and chk.failed == 0
    chk.on_enter.append(clock.boundary)
    return chk, clock


class Derivation(unittest.TestCase):

    def test_lua_sources_still_ship_these_tunables(self):
        with open(os.path.join(REPO, "scripts/unit_ai_tunables.lua")) as fh:
            tun = fh.read()
        with open(os.path.join(REPO, "scripts/unit_resource_config.lua")) as fh:
            cfg = fh.read()
        self.assertRegex(tun, r"sleep_min_deficit\s*=\s*0\.35\b")
        # The acolyte's sleep_pressure block is the first in the file.
        block = cfg[cfg.index("sleep_pressure = {"):]
        block = block[:block.index("}")]
        self.assertRegex(block, r"drain_constant_frac\s*=\s*1 / 3600\b")
        self.assertRegex(block, r"circadian_center\s*=\s*0\.75\b")
        self.assertRegex(block, r"circadian_width\s*=\s*0\.125\b")

    def test_eligibility_delay_and_window(self):
        self.assertAlmostEqual(SHIPPED.awake_delay, 1260.0)
        self.assertAlmostEqual(SHIPPED.window_start, 0.625)
        self.assertEqual(db.clock_text(SHIPPED.window_start), "15:00")
        self.assertFalse(SHIPPED.in_window(0.625))   # the edge: urge 0
        self.assertTrue(SHIPPED.in_window(0.626))
        self.assertTrue(SHIPPED.in_window(0.874))
        self.assertFalse(SHIPPED.in_window(0.875))

    def test_world_start_lands_on_day_two_window(self):
        # 10:00 + 1260 s = 07:00 next morning; the window opens at 15:00.
        b = db.derive_budget(100.0, TEN_AM, 1440.0, SHIPPED)
        self.assertAlmostEqual(b.deadline, 100.0 + 1260.0 + 480.0)
        self.assertEqual(b.day_of(b.deadline), 2)
        self.assertAlmostEqual(b.phase_at(b.deadline), 0.625)
        self.assertIn("day 2's sleep window", b.describe())

    def test_eligible_inside_the_window_is_the_deadline(self):
        # Eligibility lands at colony sun 0.70: already inside the window.
        b = db.derive_budget(0.0, (0.70 - 0.875) % 1.0, 1440.0, SHIPPED)
        self.assertAlmostEqual(b.deadline, 1260.0)

    def test_eligible_just_after_the_window_waits_for_the_next(self):
        b = db.derive_budget(0.0, (0.876 - 0.875) % 1.0, 1440.0, SHIPPED)
        self.assertAlmostEqual(b.deadline, 1260.0 + (0.625 - 0.876 + 1) * 1440)

    def test_time_scale_shortens_the_day_not_the_awake_delay(self):
        b = db.derive_budget(0.0, TEN_AM, 720.0, SHIPPED)
        elig_phase = (TEN_AM + 1260.0 / 720.0) % 1.0
        self.assertAlmostEqual(b.deadline,
                               1260.0 + ((0.625 - elig_phase) % 1.0) * 720.0)

    def test_midnight_wrap(self):
        # Reference at 22:00: eligibility crosses midnight and lands at
        # 19:00 next day, inside that day's window; the phase wraps into
        # [0, 1) and the day number carries.
        b = db.derive_budget(0.0, 22 / 24, 1440.0, SHIPPED)
        self.assertEqual(b.day_of(0.0), 1)
        self.assertEqual(b.day_of(2 * 60.0 - 0.1), 1)
        self.assertEqual(b.day_of(2 * 60.0 + 0.1), 2)
        self.assertAlmostEqual(b.phase_at(2 * 60.0 + 60.0), 1 / 24)
        self.assertAlmostEqual(b.deadline, 1260.0)
        self.assertAlmostEqual(b.phase_at(b.deadline), 19 / 24)
        self.assertEqual(b.day_of(b.deadline), 2)
        # Reference at 23:30: eligibility at 20:30 next day, the last
        # half-hour of that day's window.
        b = db.derive_budget(0.0, 23.5 / 24, 1440.0, SHIPPED)
        self.assertAlmostEqual(b.phase_at(b.t_ref + 1260.0), 20.5 / 24)
        self.assertAlmostEqual(b.deadline, 1260.0)
        # Reference at 01:00: eligibility at 22:00 the same day, after
        # its window closed, so the deadline is past the next midnight.
        b = db.derive_budget(0.0, 1 / 24, 1440.0, SHIPPED)
        self.assertAlmostEqual(b.phase_at(b.t_ref + 1260.0), 22 / 24)
        self.assertEqual(b.day_of(b.t_ref + 1260.0), 1)
        self.assertAlmostEqual(b.deadline, 1260.0 + 17 * 60.0)
        self.assertEqual(b.day_of(b.deadline), 2)
        self.assertAlmostEqual(b.phase_at(b.deadline), 0.625)


class Boundary(unittest.TestCase):

    def test_before_at_and_after_the_deadline(self):
        console = FakeConsole()
        chk, clock = established(console)
        deadline = clock.budget.deadline
        for stage, t, fails in (("prepare", deadline - 0.1, False),
                                ("travel", deadline, True),
                                ("extract", deadline + 0.1, True)):
            console.t = t
            before = chk.failed
            _, out = quiet(chk.enter, stage, stage)
            self.assertIn(f"clock [enter {stage}]", out)
            self.assertIn("colony sun", out)
            self.assertEqual(chk.failed - before, 1 if fails else 0, stage)
            if fails:
                self.assertIn("overran its day", out)
            else:
                self.assertIn("inside the day budget", out)

    def test_hooked_entry_records_a_failing_stage(self):
        console = FakeConsole()
        chk, clock = established(console)
        console.t = clock.budget.deadline + 30.0
        quiet(chk.enter, "return", "walk home")
        self.assertEqual(chk.outcomes()["return"], "fail")
        self.assertEqual(chk.failed, 1)

    def test_setup_entry_before_the_colony_reports_unavailable(self):
        clock = db.DayClock(port=0, send=FakeConsole())
        chk = Checks()
        _, out = quiet(clock.boundary, chk, "setup", "setup")
        self.assertIn("no colony yet", out)
        self.assertEqual(chk.failed, 0)

    def test_unestablishable_budget_is_a_failing_check(self):
        console = FakeConsole()
        console.dead = True
        chk = Checks()
        ok, _ = quiet(db.DayClock(port=0, send=console).establish, chk, (1, 1))
        self.assertFalse(ok)
        self.assertEqual(chk.failed, 1)


class Timeouts(unittest.TestCase):

    def test_wait_beginning_inside_and_expiring_past(self):
        console = FakeConsole()
        _, clock = established(console)
        console.actors = {7: "follow_command|standing|nil"}
        started = clock.budget.deadline - 100.0
        console.t = clock.budget.deadline + 20.0
        _, out = quiet(clock.timed_out, "the walk home", [7], started)
        self.assertIn("PAST the day budget by 20 s", out)
        self.assertIn("began inside the day and expired after it", out)
        self.assertIn("unit 7: action follow_command, pose standing — awake",
                      out)
        self.assertIn("past the day budget, but no involved unit is observed "
                      "asleep", out)

    def test_sleeping_actors_are_named(self):
        console = FakeConsole()
        _, clock = established(console)
        console.actors = {1: "go_to_sleep|crouching|lying_down",
                          2: "idle|sleeping|nil",
                          3: "follow_command|walking|nil"}
        console.t = clock.budget.deadline + 5.0
        _, out = quiet(clock.timed_out, "the gather", [1, 2, 3], 0.0)
        self.assertIn("unit 1: action go_to_sleep, pose crouching, sleep "
                      "phase lying_down — ASLEEP", out)
        self.assertIn("unit 2: action idle, pose sleeping — ASLEEP", out)
        self.assertIn("unit 3: action follow_command, pose walking — awake",
                      out)
        self.assertIn("unit(s) [1, 2] observed ASLEEP — the run overran its "
                      "day", out)

    def test_inside_the_budget_is_not_an_overrun(self):
        console = FakeConsole()
        _, clock = established(console)
        console.actors = {4: "pickup_ground|standing|nil"}
        console.t = clock.budget.deadline - 600.0
        _, out = quiet(clock.timed_out, "the pickup", [4], console.t - 180)
        self.assertIn("inside the day budget, 600 s left", out)
        self.assertIn("not a day overrun", out)
        self.assertNotIn("ASLEEP", out)

    def test_missing_unit_and_no_actor_waits(self):
        console = FakeConsole()
        _, clock = established(console)
        _, out = quiet(clock.timed_out, "the fight", [99], None)
        self.assertIn("unit 99: missing — it no longer exists", out)
        _, out = quiet(clock.timed_out, "the save capture", None, None)
        self.assertIn("no specific unit is involved in this wait", out)

    def test_unavailable_console_is_reported_not_raised(self):
        console = FakeConsole()
        _, clock = established(console)
        console.dead = True
        _, out = quiet(clock.timed_out, "the walk home", [5, 6], 10.0)
        self.assertIn("game time unavailable", out)
        self.assertIn("unit 5: unreadable (console unavailable "
                      "(ConnectionRefusedError", out)
        chk = Checks()
        _, out = quiet(clock.boundary, chk, "save", "save")
        self.assertIn("console unavailable", out)
        self.assertEqual(chk.failed, 0)

    def test_classify(self):
        self.assertEqual(db.classify("go_to_sleep", "standing", "nil"),
                         "ASLEEP")
        self.assertEqual(db.classify("nil", "sleeping", "nil"), "ASLEEP")
        self.assertEqual(db.classify("idle", "crawling", "waking"), "ASLEEP")
        self.assertEqual(db.classify("eat_from_inventory", "standing", "nil"),
                         "awake")


class AcrossTheLoad(unittest.TestCase):

    def test_one_deadline_survives_the_fresh_process(self):
        console = FakeConsole(t=50.0)
        chk, clock = established(console)
        budget = clock.budget
        clock.live = False                      # engine A has quit
        _, out = quiet(chk.enter, "load", "load")
        self.assertIn("no engine is running", out)
        _, out = quiet(clock.timed_out, "the load", None, None)
        self.assertIn("game time unavailable — no restored game time", out)
        self.assertIn("no specific unit is involved", out)
        # Engine B publishes the save: its game time is the save's own,
        # and the deadline fixed in engine A is the one applied.
        console.t = budget.deadline + 1.0
        _, out = quiet(clock.loaded, chk)
        self.assertIs(clock.budget, budget)
        self.assertTrue(clock.live)
        self.assertIn("overran its day: stage 'load'", out)
        self.assertEqual(chk.outcomes()["load"], "fail")

    def test_load_inside_the_budget_passes(self):
        console = FakeConsole(t=50.0)
        chk, clock = established(console)
        clock.live = False
        quiet(chk.enter, "load", "load")
        console.t = clock.budget.deadline - 1.0
        quiet(clock.loaded, chk)
        self.assertEqual(chk.failed, 0)


class Facade(unittest.TestCase):
    """The overrun reaches the probe's exit status through `main`."""

    def run_main(self, late: bool):
        import expedition_loop_probe as probe
        console = FakeConsole(t=10.0)

        def setup_run(chk, st):
            chk.enter("setup", "setup")
            st.day.establish(chk, (3, 3))

        def stage(name):
            def run(chk, st):
                console.t += 100.0
                if late and name == "travel":
                    console.t = st.day.budget.deadline + 60.0
                chk.enter(name, name)
                chk.ok(True, f"{name} did its work")
            return run

        noop = lambda *a, **k: None  # noqa: E731
        patches = [
            mock.patch("probelib.send", console),
            mock.patch.object(probe, "boot_probe", lambda *a, **k: object()),
            mock.patch.object(probe, "bootstrap", noop),
            mock.patch.object(probe, "quit_engine", noop),
            mock.patch.object(probe, "make_isolated_root",
                              lambda base: base),
            mock.patch.object(probe, "remove_root", noop),
            mock.patch.object(probe.setup, "run", setup_run),
            mock.patch.object(probe.prepare, "run", stage("prepare")),
            mock.patch.object(probe.travel, "run", stage("travel")),
            mock.patch.object(probe.encounter, "set_out", noop),
            mock.patch.object(probe.extract, "run", stage("extract")),
            mock.patch.object(probe.extract, "deliver", stage("return")),
            mock.patch.object(probe.encounter, "run", stage("encounter")),
            mock.patch.object(probe.encounter, "reward", stage("reward")),
            mock.patch.object(probe.encounter, "deliver_home", noop),
            mock.patch.object(probe.persistence, "save", stage("save")),
            mock.patch.object(probe.travel, "measure_control",
                              stage("control")),
            mock.patch.object(probe.persistence, "load",
                              lambda chk, st: (st.day.loaded(chk),
                                               chk.ok(True, "loaded"))),
            mock.patch.object(sys, "argv", ["expedition_loop_probe.py"]),
        ]
        with contextlib.ExitStack() as stack:
            for p in patches:
                stack.enter_context(p)
            return quiet(probe.main)

    def test_on_time_run_exits_zero(self):
        code, out = self.run_main(late=False)
        self.assertEqual(code, 0, out)
        self.assertIn("--- PASS", out)
        self.assertIn("day budget: deadline", out)
        # Readings stay out of the fingerprint.
        line = next(ln for ln in out.splitlines()
                    if ln.startswith("FINGERPRINT"))
        self.assertNotIn("game", line)
        self.assertNotIn("sun", line)

    def test_overrun_fails_its_stage_and_exits_nonzero(self):
        code, out = self.run_main(late=True)
        self.assertEqual(code, 1, out)
        self.assertIn("overran its day: stage 'travel'", out)
        self.assertIn("broke at stage 'travel'", out)
        self.assertIn('"travel": "fail"', out)


if __name__ == "__main__":
    unittest.main()
