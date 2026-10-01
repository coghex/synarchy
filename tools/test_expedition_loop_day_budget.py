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
        self.assertIn("verdict: no involved unit observed asleep; the run is "
                      "PAST its day budget — it overran its day", out)

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
        self.assertIn("verdict: unit(s) [1, 2] observed ASLEEP; the run is "
                      "PAST its day budget", out)

    def test_inside_the_budget_is_not_an_overrun(self):
        console = FakeConsole()
        _, clock = established(console)
        console.actors = {4: "pickup_ground|standing|nil"}
        console.t = clock.budget.deadline - 600.0
        _, out = quiet(clock.timed_out, "the pickup", [4], console.t - 180)
        self.assertIn("inside the day budget, 600 s left", out)
        self.assertIn("verdict: no involved unit observed asleep; the run is "
                      "inside its day budget", out)
        self.assertNotIn("ASLEEP", out)
        self.assertNotIn("overran", out)

    def test_asleep_before_the_colony_deadline_is_not_an_overrun(self):
        # A unit's sleep window follows its OWN longitude, so it can lie
        # down while the run is still inside the colony-local budget.
        console = FakeConsole()
        _, clock = established(console)
        console.actors = {8: "go_to_sleep|sleeping|sleeping"}
        console.t = clock.budget.deadline - 120.0
        _, out = quiet(clock.timed_out, "the far post", [8], console.t - 60)
        self.assertIn("unit 8: action go_to_sleep, pose sleeping", out)
        self.assertIn("verdict: unit(s) [8] observed ASLEEP; the run is "
                      "inside its day budget", out)
        self.assertNotIn("overran", out)
        self.assertNotIn("PAST", out)

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
        # The established deadline is still stated, with the current
        # status declared undeterminable rather than omitted.
        self.assertIn(f"the day budget's deadline is "
                      f"{clock.budget.deadline:.1f} (day 2, colony 15:00); "
                      f"whether the run is past it now cannot be determined",
                      out)
        self.assertIn("the wait began at 10.0, inside the budget", out)
        self.assertIn("verdict: no involved unit observed asleep; the day "
                      "budget's status cannot be determined", out)
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
        self.assertIn("no session clock is live", out)
        _, out = quiet(clock.timed_out, "the load", None, None)
        self.assertIn("game time unavailable — no session clock is live", out)
        self.assertIn(f"the day budget's deadline is {budget.deadline:.1f}",
                      out)
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


class CallSites(unittest.TestCase):
    """A real stage owner's wait expires and the console stops answering
    in the same moment: the wait's own failure must still be recorded,
    with the diagnostics beneath it, before anything raises."""

    def run_owner(self, fn, console, chk, st):
        from expedition_loop import encounter, extract, readers
        patches = [mock.patch.object(m, name, console)
                   for m in (extract, encounter, readers)
                   for name in ("send", "send_json")]
        with contextlib.ExitStack() as stack:
            for p in patches:
                stack.enter_context(p)
            buf = io.StringIO()
            raised = None
            with contextlib.redirect_stdout(buf):
                try:
                    fn(chk, st)
                except Exception as exc:  # noqa: BLE001
                    raised = exc
        return buf.getvalue(), raised

    def state(self, console):
        from expedition_loop.harness import ExpeditionState, Fingerprint
        chk, clock = established(console)
        st = ExpeditionState(port=0, fp=Fingerprint(1, 2, 3), seed=1,
                             size=2, plates=3, day=clock)
        st.prepared, st.foot, st.deposit_spot = 11, (0, 0, 2, 2), (3, 3)
        st.storage_bid, st.instance_id, st.occ_sig_phys = 5, 77, 88
        st.occ_carrier = 21
        st.recovered = {"defName": "first_aid_kit", "instanceId": 77}
        return chk, st

    def failing_label(self, chk, out: str, needle: str) -> str:
        lines = [ln for ln in out.splitlines()
                 if ln.lstrip().startswith("[FAIL][return]") and needle in ln]
        self.assertEqual(len(lines), 1, out)
        return lines[0]

    def test_walk_home_expiring_into_a_dead_console(self):
        from expedition_loop import extract
        console = FakeConsole()
        chk, st = self.state(console)
        console.t = st.day.budget.deadline - 50.0

        def walk_expires(*_a, **_k):
            console.t = st.day.budget.deadline + 10.0
            console.dead = True
            return False

        # The order home is accepted on a live console; the walk then
        # expires as the console dies.
        def send(port, lua, *a, **k):
            if "commandMove" in lua and not console.dead:
                return "ok"
            return console(port, lua, *a, **k)

        with mock.patch.object(extract, "walk_until_adjacent", walk_expires):
            out, raised = self.run_owner(extract.deliver, send, chk, st)
        line = self.failing_label(chk, out, "walks the whole way home")
        self.assertIn("<unreadable: ConnectionRefusedError", line)
        self.assertIn("timeout diagnostics [the walk home]", out)
        self.assertIn("game time unavailable — the console did not answer",
                      out)
        self.assertIn("unit 11: unreadable (console unavailable", out)
        self.assertGreater(out.index("timeout diagnostics [the walk home]"),
                           out.index(line))
        # Whatever raises afterwards does so after the wait's own failure.
        self.assertIsInstance(raised, ConnectionRefusedError)

    def test_occupied_item_walk_home_expiring_into_a_dead_console(self):
        from expedition_loop import encounter
        console = FakeConsole()
        chk, st = self.state(console)

        def bank_expires(*_a, **_k):
            console.dead = True
            return None

        with mock.patch.object(encounter, "bank_home", bank_expires):
            out, raised = self.run_owner(encounter.deliver_home, console,
                                         chk, st)
        line = self.failing_label(chk, out, "carried home and banked")
        self.assertIn("now <unreadable: ConnectionRefusedError", line)
        self.assertIn("timeout diagnostics [the occupied ruin's item's walk "
                      "home]", out)
        # The carrier the reward stage chose is still named on a dead
        # console, its action and pose reported as unreadable.
        self.assertIn("unit 21: unreadable (console unavailable", out)
        self.assertNotIn("no specific unit", out)
        self.assertIsInstance(raised, ConnectionRefusedError)

    def test_last_observed_carrier_survives_a_dead_console(self):
        from expedition_loop import extract
        console = FakeConsole()
        _, st = self.state(console)
        st.last_carrier[88] = 9
        console.dead = True
        with mock.patch.object(extract, "holder_of",
                               mock.Mock(side_effect=ConnectionRefusedError)):
            self.assertEqual(extract.carrier_of(0, st, 88, 21), [9, 21])
            self.assertEqual(extract.carrier_of(0, st, 99), None)
        with mock.patch.object(extract, "holder_of", lambda *_a: 4):
            self.assertEqual(extract.carrier_of(0, st, 88, 21), [4, 9, 21])

    def test_clearance_wait_reports_its_own_start(self):
        # The taken latch arrives after 90 s; the clearance wait then
        # starts 10 s before the deadline and expires 50 s after it. Its
        # diagnostics must name ITS start, not the taken-latch wait's.
        from expedition_loop import encounter
        console = FakeConsole()
        chk, st = self.state(console)
        deadline = st.day.budget.deadline
        console.t = deadline - 100.0
        calls = []

        def poll(seconds, _fn, interval=0.5):
            calls.append(console.t)
            if len(calls) == 1:
                console.t = deadline - 10.0
                return [{"item_instance_id": 88, "taken": True}]
            console.t = deadline + 50.0
            return None

        with mock.patch.object(encounter, "poll_until", poll):
            got, out = quiet(encounter.await_taken_and_cleared, chk, st, 88)
        self.assertIsNone(got)
        self.assertEqual(calls, [deadline - 100.0, deadline - 10.0])
        self.assertIn("timeout diagnostics [the occupied ruin's clearance", out)
        self.assertIn(f"the wait began at {deadline - 10.0:.1f}, inside the "
                      f"budget — it began inside the day and expired after it",
                      out)
        self.assertNotIn(f"{deadline - 100.0:.1f}", out)
        self.assertNotIn("taken latch (the world", out)

    def test_find_water_retirement_reports_an_expired_wait(self):
        from expedition_loop import setup
        console = FakeConsole()
        chk, st = self.state(console)
        console.actors = {3: "nil|standing|nil"}
        console.t = st.day.budget.deadline - 300.0

        def poll(_seconds, fn, **_k):
            return None if fn.__defaults__ == (3,) else True

        with mock.patch.object(setup, "poll_until", poll):
            ok, out = quiet(setup.retire_find_water, chk, st, [2, 3, 4])
        self.assertFalse(ok)
        self.assertEqual(chk.failed, 1)
        self.assertIn("[FAIL][setup] acolyte 3's standing find_water goal is "
                      "retired (no AI state within 10 s)", out)
        self.assertIn("timeout diagnostics [acolyte 3's AI state", out)
        self.assertIn("unit 3: action nil, pose standing — awake", out)
        self.assertIn("inside the day budget, 300 s left", out)
        self.assertNotIn("unit 2:", out)

    def test_find_water_expiries_are_reported_as_each_ends(self):
        # Unit 2's wait expires inside the budget; unit 3's poll then
        # carries the clock past it. Unit 2's diagnostics must carry its
        # own end time and state, not unit 3's.
        from expedition_loop import setup
        console = FakeConsole()
        chk, st = self.state(console)
        deadline = st.day.budget.deadline
        console.t = deadline - 25.0
        console.actors = {2: "idle|standing|nil",
                          3: "go_to_sleep|sleeping|sleeping"}

        def poll(_seconds, fn, **_k):
            console.t += 10.0
            if fn.__defaults__ == (3,):
                console.t = deadline + 25.0
            return None

        with mock.patch.object(setup, "poll_until", poll):
            ok, out = quiet(setup.retire_find_water, chk, st, [2, 3])
        self.assertFalse(ok)
        self.assertEqual(chk.failed, 2)
        first, second = out.split("timeout diagnostics [acolyte 3")
        self.assertIn("timeout diagnostics [acolyte 2", first)
        self.assertIn(f"game time {deadline - 15.0:.1f}", first)
        self.assertIn("inside the day budget, 15 s left", first)
        self.assertNotIn("expired after it", first)
        self.assertIn("unit 2: action idle, pose standing — awake", first)
        self.assertNotIn("unit 3", first)
        self.assertIn(f"game time {deadline + 25.0:.1f}", second)
        self.assertIn("unit 3: action go_to_sleep", second)
        # Each failure is recorded before its own diagnostics.
        self.assertLess(out.index("[FAIL][setup] acolyte 2"),
                        out.index("timeout diagnostics [acolyte 2"))
        self.assertLess(out.index("timeout diagnostics [acolyte 2"),
                        out.index("[FAIL][setup] acolyte 3"))

    def test_find_water_retirement_on_time_passes_quietly(self):
        from expedition_loop import setup
        console = FakeConsole()
        chk, st = self.state(console)
        with mock.patch.object(setup, "poll_until", lambda *_a, **_k: True):
            ok, out = quiet(setup.retire_find_water, chk, st, [2, 3])
        self.assertTrue(ok)
        self.assertEqual(chk.failed, 0)
        self.assertNotIn("timeout diagnostics", out)

    def test_world_generation_expiry_is_a_failing_refusal(self):
        from expedition_loop import setup
        from expedition_loop.harness import StageAbort
        console = FakeConsole()
        chk, st = self.state(console)
        phase = {"value": "2"}

        sent = []

        def send(port, lua, *a, **k):
            if "waitForInit" in lua:
                sent.append(lua)
                # The console built-in's reply: getInitProgress's four
                # tab-separated values.
                # Shapes as observed from the live built-in.
                return {"2": '2\t10\t40\t"chunks',
                        "3": '3\t1\t1\t"done'}[phase["value"]]
            return console(port, lua, *a, **k)

        with mock.patch.object(setup, "send", send):
            buf = io.StringIO()
            with contextlib.redirect_stdout(buf), \
                    self.assertRaises(StageAbort):
                setup.await_world_init(chk, st)
            out = buf.getvalue()
            self.assertEqual(chk.failed, 1)
            self.assertIn("load phase 3 is done", out)
            self.assertIn("2\\t10\\t40", out)
            self.assertIn("timeout diagnostics [world generation", out)
            phase["value"] = "3"
            _, out = quiet(setup.await_world_init, chk, st)
            self.assertEqual(chk.failed, 1)
            self.assertNotIn("timeout diagnostics", out)
        # Exactly the literal the console serves off the Lua thread with
        # no 30 s cap — and that literal is what the engine matches.
        self.assertEqual(sent, ["return world.waitForInit(400)"] * 2)
        with open(os.path.join(REPO, "src/Engine/Scripting/Lua/Thread/"
                                     "Console.hs")) as fh:
            console_src = fh.read()
        self.assertIn('stripPrefix "return " t0', console_src)
        self.assertIn('matchCall "world.waitForInit" t2', console_src)

    def test_chunk_wait_reports_pending_chunks(self):
        from expedition_loop import readers
        console = FakeConsole()
        _, st = self.state(console)
        replies = {"return world.waitForChunks(180)": "3"}

        def send(port, lua, *a, **k):
            if lua.startswith("return world.loadChunksInRegion("):
                return "true"
            return replies.get(lua) or console(port, lua, *a, **k)

        with mock.patch.object(readers, "send", send):
            pending, out = quiet(readers.load_region, 0, 4, 5, day=st.day)
            self.assertEqual(pending, 3)
            self.assertIn("timeout diagnostics [chunk loading around chunk "
                          "(4,5): 3 chunk(s) still pending", out)
            self.assertIn("no specific unit is involved in this wait", out)
            replies["return world.waitForChunks(180)"] = "0"
            pending, out = quiet(readers.load_region, 0, 4, 5, day=st.day)
            self.assertEqual(pending, 0)
            self.assertNotIn("timeout diagnostics", out)

    def test_latches_on_time_return_the_cleared_row(self):
        from expedition_loop import encounter
        console = FakeConsole()
        chk, st = self.state(console)
        row = {"lifecycle": "cleared", "clearance_satisfied": True,
               "clear_event_emitted": True, "name": "Ruin"}
        answers = iter([[{"item_instance_id": 88, "taken": True}], row])
        with mock.patch.object(encounter, "poll_until",
                               lambda *_a, **_k: next(answers)):
            got, out = quiet(encounter.await_taken_and_cleared, chk, st, 88)
        self.assertIs(got, row)
        self.assertEqual(chk.failed, 0, out)
        self.assertNotIn("timeout diagnostics", out)

    def facade_raising(self, exc):
        import expedition_loop_probe as probe
        console = FakeConsole()

        def setup_raises(chk, st):
            chk.enter("setup", "setup")
            raise exc

        noop = lambda *a, **k: None  # noqa: E731
        with contextlib.ExitStack() as stack:
            for p in (mock.patch("probelib.send", console),
                      mock.patch.object(probe, "boot_probe",
                                        lambda *a, **k: object()),
                      mock.patch.object(probe, "bootstrap", noop),
                      mock.patch.object(probe, "quit_engine", noop),
                      mock.patch.object(probe.setup, "run", setup_raises),
                      mock.patch.object(probe, "make_isolated_root",
                                        lambda base: base),
                      mock.patch.object(probe, "remove_root", noop),
                      mock.patch.object(sys, "argv", ["probe"]),
                      contextlib.redirect_stderr(io.StringIO())):
                stack.enter_context(p)
            return quiet(probe.main)

    def test_blocking_wait_timeout_reports_through_the_facade(self):
        code, out = self.facade_raising(TimeoutError("timed out"))
        self.assertEqual(code, 1, out)
        self.assertIn("unexpected TimeoutError while running stage 'setup'",
                      out)
        self.assertIn("timeout diagnostics [the console round trip or "
                      "blocking engine wait that raised TimeoutError", out)

    def test_a_probe_defect_is_not_reported_as_a_wait(self):
        code, out = self.facade_raising(NameError("name 'x' is not defined"))
        self.assertEqual(code, 1, out)
        self.assertIn("unexpected NameError", out)
        self.assertNotIn("timeout diagnostics", out)

    def test_engine_ready_timeout_reports_through_the_facade(self):
        import expedition_loop_probe as probe
        console = FakeConsole()
        console.dead = True

        def boot_fails(*_a, **_k):
            raise SystemExit("[engine A] never printed READY")

        noop = lambda *a, **k: None  # noqa: E731
        with contextlib.ExitStack() as stack:
            for p in (mock.patch("probelib.send", console),
                      mock.patch.object(probe, "boot_probe", boot_fails),
                      mock.patch.object(probe, "make_isolated_root",
                                        lambda base: base),
                      mock.patch.object(probe, "remove_root", noop),
                      mock.patch.object(sys, "argv", ["probe"])):
                stack.enter_context(p)
            code, out = quiet(probe.main)
        self.assertEqual(code, 1, out)
        self.assertIn("never printed READY", out)
        self.assertIn("timeout diagnostics [the engine's boot or life", out)
        self.assertIn("no specific unit is involved in this wait", out)
        self.assertLess(out.index("never printed READY"),
                        out.index("timeout diagnostics"))


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
