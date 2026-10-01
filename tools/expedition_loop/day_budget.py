#!/usr/bin/env python3
"""The run's day budget: when the scenario has overrun its day (#2755).

The expedition is fixed-seed, but it runs on the world's own clock, and
that clock carries a survival mechanic the gate deliberately tests rather
than masks (owner direction on #2755): an acolyte that has been awake
long enough gets sleepy at dusk, and from there `go_to_sleep` can outrank
a player move order (`scripts/unit_ai_sleep.lua`, `follow_command` 7.0).
Once lying down, a unit is locked in until it wakes. A run that is still
walking when that happens does not stall for an unknown reason — it has
run past its day — and this module is what makes it SAY so.

THE BUDGET
----------
A conservative SCENARIO budget, not a prediction of any one unit's sleep
onset. It names the earliest instant from which sleep can compete with an
order at all, derived from the shipped tunables the engine is running —
read live from `scripts/unit_ai_tunables.lua` and
`scripts/unit_resource_config.lua`, never restated here:

  * Reference instant: the engine game time and the COLONY-LOCAL sun
    angle read immediately before the acolyte portal is placed. Every
    party member spawns after it, so it is no later than any of their
    spawns.
  * Sleep pressure starts FULL: `scripts/unit_resource_tick.lua` fills a
    resource to its max on a unit's first tick. It drains at
    `drain_constant_frac` of max per awake game-second, so `go_to_sleep`
    becomes eligible after `sleep_min_deficit / drain_constant_frac`
    game-seconds (0.35 / (1/3600) = 1260 for the shipped acolyte).
  * The circadian window: the dusk urge (`scripts/circadian.lua`) is
    nonzero only within `circadian_width` of `circadian_center` on the
    sun-angle circle (0.75 +/- 0.125: colony-local 15:00 to 21:00).
    Outside it a unit at the eligibility floor scores 9 x 0.35 = 3.15,
    plus at most 2.0 for full exhaustion — below `follow_command`.
  * The deadline is the first instant at which both hold: eligibility
    has been reached AND the colony-local sun angle is inside the
    window. It is ONE absolute engine game time, fixed when the colony is
    planted and never recomputed — not after a midnight wrap, and not by
    the fresh process, whose `engine.gameTime()` is the save's own.
  * Day numbering: day 1 is the colony-local day (midnight to midnight)
    containing the reference instant. Starting at the world's 10:00, the
    deadline is the opening of DAY 2's sleep window.

What the budget is not: eligibility alone does not decide arbitration —
the urge ramps from zero at the window's edge, exhaustion adds its own
term, and `scripts/circadian.lua` reads each unit's OWN longitude, so a
unit far east or west of the colony enters its window earlier or later
than the colony does. The budget is therefore the scenario's line, and
the per-unit diagnostics below report what each unit was actually doing.
Nothing here sets the clock (`world.setTime` / `world.setSunAngle`) or
touches any unit's sleep or needs.

CLOCK SEMANTICS
---------------
`engine.gameTime()` is elapsed game time: it advances only while the
simulation runs (`src/Unit/Thread.hs`) and a load installs the save's own
value. The world clock and its sun angle pause with it
(`src/World/Thread/Time.hs`). A game day is 1440 clock minutes and the
time scale is game-minutes per second, so a day lasts
`1440 / world.getTimeScale(page)` game-seconds. `world.getSunAngleAt` is
longitude-local, read here at the colony tile.

Readings are printed and kept OUT of the `FINGERPRINT`: a game time is a
measurement, and two honest runs of the same seed disagree on its last
digits.
"""
from __future__ import annotations

import math
from dataclasses import dataclass, field
from typing import Callable

from .constants import PAGE

#: The world clock's minutes per day (`World.Time.Scale`), fixed
#: regardless of time scale.
CLOCK_MINUTES_PER_DAY = 1440.0


# --------------------------------------------------------------------------
# The derivation — pure, so the boundary cases are unit-tested
# (`tools/test_expedition_loop_day_budget.py`)
# --------------------------------------------------------------------------
@dataclass(frozen=True)
class SleepTunables:
    """The shipped acolyte values the budget is derived from."""

    #: `unit_ai_tunables.acolyte.sleep_min_deficit`.
    min_deficit: float
    #: `unit_resource_config.acolyte.sleep_pressure.drain_constant_frac`.
    drain_frac: float
    #: `...sleep_pressure.circadian_center` / `circadian_width`.
    center: float
    width: float

    @property
    def awake_delay(self) -> float:
        """Game-seconds awake, from full pressure, until eligible."""
        return self.min_deficit / self.drain_frac

    @property
    def window_start(self) -> float:
        return (self.center - self.width) % 1.0

    def in_window(self, phase: float) -> bool:
        d = abs(phase - self.center) % 1.0
        return min(d, 1.0 - d) < self.width


@dataclass(frozen=True)
class DayBudget:
    """One absolute deadline, and the reference it was derived from."""

    t_ref: float
    phase_ref: float
    day_seconds: float
    tunables: SleepTunables
    deadline: float

    def phase_at(self, t: float) -> float:
        """The colony-local sun angle the reference extrapolates to."""
        return (self.phase_ref + (t - self.t_ref) / self.day_seconds) % 1.0

    def day_of(self, t: float) -> int:
        return 1 + math.floor(self.phase_ref
                              + (t - self.t_ref) / self.day_seconds)

    def overran(self, t: float) -> bool:
        return t >= self.deadline

    def status(self, t: float) -> str:
        if self.overran(t):
            return (f"PAST the day budget by {t - self.deadline:.0f} s "
                    f"(deadline {self.deadline:.1f})")
        return (f"inside the day budget, {self.deadline - t:.0f} s left "
                f"(deadline {self.deadline:.1f})")

    def describe(self) -> str:
        tun = self.tunables
        return (f"day budget: deadline at game time {self.deadline:.1f} — "
                f"day {self.day_of(self.deadline)}'s sleep window opening at "
                f"colony sun {tun.window_start:.3f} "
                f"({clock_text(tun.window_start)}), once "
                f"{tun.awake_delay:.0f} s awake from full sleep pressure has "
                f"made go_to_sleep eligible (sleep_min_deficit "
                f"{tun.min_deficit} / drain_constant_frac "
                f"{tun.drain_frac:.6g}; circadian {tun.center} +/- "
                f"{tun.width}); reference game time {self.t_ref:.1f}, colony "
                f"sun {self.phase_ref:.3f} ({clock_text(self.phase_ref)}, "
                f"day 1), {self.day_seconds:.0f} s per day")


def derive_budget(t_ref: float, phase_ref: float, day_seconds: float,
                  tunables: SleepTunables) -> DayBudget:
    """The first instant at or after eligibility whose colony-local sun
    angle is inside the circadian window."""
    eligible = t_ref + tunables.awake_delay
    phase = (phase_ref + tunables.awake_delay / day_seconds) % 1.0
    if tunables.in_window(phase):
        deadline = eligible
    else:
        deadline = eligible + ((tunables.window_start - phase) % 1.0) * day_seconds
    return DayBudget(t_ref, phase_ref, day_seconds, tunables, deadline)


def clock_text(phase: float) -> str:
    """A sun angle as a 24-hour local clock reading."""
    minutes = int(round((phase % 1.0) * CLOCK_MINUTES_PER_DAY)) % 1440
    return f"{minutes // 60:02d}:{minutes % 60:02d}"


# --------------------------------------------------------------------------
# Best-effort reads around an expired wait
# --------------------------------------------------------------------------
def safe(read, *fallback):
    """`read()`, or — when the console has stopped answering — the given
    fallback, else a text naming what failed.

    A wait's failure message is built from live reads, and it has to be
    RECORDED even when the console dies as the wait expires: a read that
    raised inside the message would replace the wait's own failing check
    with an unexpected-exception one. So every read in an expired wait's
    message, and in the condition it checks, goes through here."""
    try:
        return read()
    except Exception as exc:  # noqa: BLE001 - diagnostics never raise
        if fallback:
            return fallback[0]
        return f"<unreadable: {type(exc).__name__}: {exc}>"


# --------------------------------------------------------------------------
# What a unit was doing when a wait expired
# --------------------------------------------------------------------------
SLEEP_ACTION = "go_to_sleep"
SLEEP_POSE = "sleeping"


def classify(action: str, pose: str, sleep_phase: str) -> str:
    """ASLEEP for a unit observed in the sleep goal or pose, else awake."""
    if action == SLEEP_ACTION or pose == SLEEP_POSE \
            or sleep_phase not in ("", "nil", "None"):
        return "ASLEEP"
    return "awake"


def actor_line(uid: int, row) -> str:
    """One unit's line. `row` is (action, pose, sleep_phase), the string
    'missing' for a unit that no longer exists, or an error string."""
    if row == "missing":
        return f"unit {uid}: missing — it no longer exists"
    if isinstance(row, str):
        return f"unit {uid}: unreadable ({row})"
    action, pose, phase = row
    verdict = classify(action, pose, phase)
    text = f"unit {uid}: action {action}, pose {pose}"
    if phase not in ("", "nil", "None"):
        text += f", sleep phase {phase}"
    if verdict == "ASLEEP":
        return (text + " — ASLEEP: the day's sleep mechanic has taken it "
                       "(a survival outcome under test, not masked)")
    return text + " — awake"


def timeout_lines(what: str, budget, now, started, actors,
                  unread: str = "the console did not answer") -> list[str]:
    """The diagnostic block printed after a wait's own failing check.

    `now` / `started` are game times or None when unreadable; `actors` is
    [(uid, row)] as `actor_line` takes it, or None when the wait has no
    specific units; `unread` says why `now` is None. It never replaces
    the original failure — it is printed beneath it."""
    out = [f"timeout diagnostics [{what}]:"]
    if now is None:
        out.append(f"  game time unavailable — {unread}")
        if budget is not None:
            out.append(f"  the day budget's deadline is {budget.deadline:.1f} "
                       f"(day {budget.day_of(budget.deadline)}, colony "
                       f"{clock_text(budget.phase_at(budget.deadline))}); "
                       f"whether the run is past it now cannot be "
                       f"determined")
            if started is not None:
                began = "inside" if not budget.overran(started) else "past"
                out.append(f"  the wait began at {started:.1f}, {began} the "
                           f"budget")
    elif budget is None:
        out.append(f"  game time {now:.1f}; no day budget established yet")
    else:
        out.append(f"  game time {now:.1f} (colony sun "
                   f"{budget.phase_at(now):.3f}, "
                   f"{clock_text(budget.phase_at(now))}, day "
                   f"{budget.day_of(now)}), {budget.status(now)}")
        if started is not None:
            began = "inside" if not budget.overran(started) else "past"
            out.append(f"  the wait began at {started:.1f}, {began} the "
                       f"budget"
                       + (" — it began inside the day and expired after it"
                          if began == "inside" and budget.overran(now)
                          else ""))
    if actors is None:
        out.append("  no specific unit is involved in this wait")
        asleep = []
    else:
        out.extend("  " + actor_line(uid, row) for uid, row in actors)
        asleep = [uid for uid, row in actors
                  if isinstance(row, tuple) and classify(*row) == "ASLEEP"]
    # Observed sleep and the budget are reported independently: a unit's
    # sleep window follows its OWN longitude, so it can lie down before
    # the colony-local deadline, and a run can be past the deadline with
    # nobody asleep yet.
    if budget is None or now is None:
        budget_part = "the day budget's status cannot be determined"
    elif budget.overran(now):
        budget_part = "the run is PAST its day budget — it overran its day"
    else:
        budget_part = "the run is inside its day budget"
    if asleep:
        sleep_part = f"unit(s) {asleep} observed ASLEEP"
    elif actors is None:
        sleep_part = "no unit to observe"
    else:
        sleep_part = "no involved unit observed asleep"
    out.append(f"  verdict: {sleep_part}; {budget_part}")
    return out


def overrun_message(stage: str, budget: DayBudget, t: float) -> str:
    return (f"the run overran its day: stage '{stage}' starts at game time "
            f"{t:.1f} (day {budget.day_of(t)}, colony "
            f"{clock_text(budget.phase_at(t))}), "
            f"{t - budget.deadline:.0f} s after the day budget's deadline "
            f"{budget.deadline:.1f} — sleep can now outrank the party's "
            f"orders, a survival mechanic this gate tests rather than "
            f"masks")


# --------------------------------------------------------------------------
# The live tracker
# --------------------------------------------------------------------------
TUNABLES_LUA = (
    "local t=require('scripts.unit_ai_tunables').acolyte; "
    "local c=require('scripts.unit_resource_config').acolyte.sleep_pressure; "
    "return t.sleep_min_deficit..','..c.drain_constant_frac..','.."
    "c.circadian_center..','..c.circadian_width..','.."
    f"world.getTimeScale('{PAGE}')")


#: Why no clock is read while a `DayClock` is not `live`.
NOT_LIVE = ("no session clock is live — engine A has exited and the fresh "
            "process is read only once its save has loaded")


def actor_lua(uid: int) -> str:
    return (f"if not unit.exists({uid}) then return 'missing' end; "
            f"local s=require('scripts.unit_ai').getState({uid}); "
            f"return tostring(s and s.currentAction)..'|'.."
            f"tostring(unit.getPose({uid}))..'|'.."
            f"tostring(s and s.sleepPhase)")


@dataclass
class DayClock:
    """Readings at every stage boundary, the one budget, and the
    timeout diagnostics. Never raises: an unreadable console is reported
    as unavailable, beside whatever failure is already being recorded."""

    port: int
    send: Callable = None
    #: The colony tile the sun angle is read at, once it exists.
    home: tuple | None = None
    budget: DayBudget | None = None
    #: False between engine A's exit and the fresh process's load.
    live: bool = True
    readings: list = field(default_factory=list)

    def __post_init__(self) -> None:
        if self.send is None:
            from probelib import send
            self.send = send

    # ---- reads ---------------------------------------------------------
    def _ask(self, lua: str):
        try:
            return self.send(self.port, lua).strip().strip('"'), None
        except Exception as exc:  # noqa: BLE001 - diagnostics never raise
            return None, f"{type(exc).__name__}: {exc}"

    def now(self):
        """The current game time, or None."""
        if not self.live:
            return None
        raw, _err = self._ask("return engine.gameTime()")
        try:
            return float(raw)
        except (TypeError, ValueError):
            return None

    def read(self) -> tuple:
        """(game time, colony sun angle, note) — either value None with
        the note saying why."""
        if not self.live:
            return None, None, NOT_LIVE
        raw, err = self._ask("return engine.gameTime()")
        if err:
            return None, None, f"console unavailable ({err})"
        try:
            t = float(raw)
        except (TypeError, ValueError):
            return None, None, f"unreadable game time {raw!r}"
        if self.home is None:
            return t, None, "no colony yet — the sun angle is read at the colony"
        raw, err = self._ask(f"return world.getSunAngleAt({self.home[0]},"
                             f"{self.home[1]})")
        try:
            return t, float(raw), ""
        except (TypeError, ValueError):
            return t, None, f"sun angle unreadable ({err or repr(raw)})"

    def _print(self, label: str, t, sun, note: str) -> None:
        parts = ["game time n/a" if t is None else f"game time {t:.1f}"]
        if sun is not None:
            parts.append(f"colony sun {sun:.3f} ({clock_text(sun)})")
        if t is not None and self.budget is not None:
            parts.append(f"day {self.budget.day_of(t)}, "
                         f"{self.budget.status(t)}")
        if note:
            parts.append(note)
        self.readings.append((label, t, sun))
        print(f"  clock [{label}]: {'; '.join(parts)}", flush=True)

    # ---- the stage boundaries -----------------------------------------
    def boundary(self, chk, stage: str, title: str) -> None:
        """`Checks.enter`'s hook: read, print, and fail an overrun."""
        t, sun, note = self.read()
        self._print(f"enter {stage}", t, sun, note)
        if t is not None and self.budget is not None and self.budget.overran(t):
            chk.ok(False, overrun_message(stage, self.budget, t))

    def establish(self, chk, home) -> bool:
        """Fix the budget at the colony, immediately before the portal
        spawns the party."""
        self.home = (int(home[0]), int(home[1]))
        t, sun, note = self.read()
        raw, err = self._ask(TUNABLES_LUA)
        try:
            vals = [float(v) for v in (raw or "").split(",")]
            tun = SleepTunables(*vals[:4])
            scale = vals[4]
        except (TypeError, ValueError, IndexError) as exc:
            vals, tun, scale = None, None, None
            err = err or f"{type(exc).__name__}: {raw!r}"
        if t is None or sun is None or tun is None or not scale or scale <= 0:
            return chk.ok(False, f"the day budget could be established at the "
                                 f"colony {self.home} (game time {t}, sun "
                                 f"{sun}, tunables {raw!r}; {note or err})")
        self.budget = derive_budget(t, sun, CLOCK_MINUTES_PER_DAY / scale, tun)
        self._print("colony planted", t, sun, "")
        print(f"  {self.budget.describe()}", flush=True)
        return True

    def loaded(self, chk) -> None:
        """The fresh process's boundary reading, once its save has
        published: the loaded game time is the save's own, so the one
        deadline still applies."""
        self.live = True
        t, sun, note = self.read()
        self._print("load: session published", t, sun, note)
        if t is not None and self.budget is not None and self.budget.overran(t):
            chk.ok(False, overrun_message("load", self.budget, t))

    def close(self, label: str) -> None:
        """The completion or failure boundary of an engine's run."""
        t, sun, note = self.read()
        self._print(label, t, sun, note)

    # ---- an expired wait ----------------------------------------------
    def timed_out(self, what: str, uids=None, started=None) -> None:
        """Print each involved unit's action and pose, the game time and
        the budget status, beneath the wait's own failing check."""
        now = self.now()
        actors = None
        if uids is not None:
            actors = []
            for uid in uids:
                if not self.live:
                    actors.append((uid, NOT_LIVE))
                    continue
                raw, err = self._ask(actor_lua(uid))
                if err:
                    actors.append((uid, f"console unavailable ({err})"))
                elif raw == "missing":
                    actors.append((uid, "missing"))
                else:
                    parts = raw.split("|")
                    actors.append((uid, tuple(parts) if len(parts) == 3
                                   else f"unexpected reply {raw!r}"))
        unread = "the console did not answer" if self.live else NOT_LIVE
        for line in timeout_lines(what, self.budget, now, started, actors,
                                  unread):
            print(f"    {line}", flush=True)
