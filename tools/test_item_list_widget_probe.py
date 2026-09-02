#!/usr/bin/env python3
"""The escort fixture's setup gate, driven deterministically (issue #1911).

`tools/item_list_widget_probe.py` is manual-only and needs a GPU, and one
run is about fifteen minutes of real offscreen Vulkan. Its #1251
unit-to-unit scenario stages an escort and a target that must be OUTSIDE
the transfer contract's reach (Chebyshev > 1) at the instant a Mode A
session is created — that separation is the whole reason the checks after
it mean anything. Staging it depends on four live retries over generated
terrain, so the path where it FAILS is the one an ordinary run cannot be
relied on to take. That is exactly the path this file executes, against a
scripted console rather than an engine.

The defect it pins: the reach precondition used to record its result and
throw the Boolean away, and `transfer_session.create` ran on the very next
statement. Five checks were then graded against a pair the probe had
already reported invalid — "the pair opens" passes with no approach at
all when the two are already in reach, and "the target did not move for
the whole of the approach" measures a walk that never happened. The
retained coordinated run of 2026-08-26 did precisely that at a
maximum-axis gap of exactly 1.0, which is why its other observation had
to be filed inconclusive.

What is pinned here:

  * An exhausted staging loop fails at SETUP. The failure is labeled
    `setup:`, counted once, and turns the run's accumulator non-zero —
    so the fixture the probe could not establish is a red run, never a
    green one.
  * `transfer_session.create` is never sent on that path, and none of the
    five separation-dependent checks is printed.
  * The engine is left as a completed run leaves it: running, with the
    session cleared.
  * The failure detail names EVERY attempted destination and both paused
    positions — four attempts, four distinct snapshots, where one pair of
    overwritten variables could only ever produce the last — so an
    exhausted fixture is attributable without a rerun.
  * The positive control is the same scenario over a console that lets
    the gap open: the gate admits the pair and `transfer_session.create`
    IS reached, so the negative result above is a real difference rather
    than a scenario that stops early whatever happens.

No engine, no world, no GPU, no socket: every case here runs in well
under a second.

Usage:
  python3 tools/test_item_list_widget_probe.py
Exit codes: 0 = all tests passed, 1 = one or more failed.
"""
from __future__ import annotations

import contextlib
import io
import json
import re
import sys
import types
from pathlib import Path

TOOLS = Path(__file__).resolve().parent
sys.path.insert(0, str(TOOLS))
import item_list_widget_probe as probe  # type: ignore  # noqa: E402

import selftestlib  # noqa: E402
from selftestlib import FAILURES, expect  # noqa: E402

PORT = 9428
ANCHOR = (25, -151)

# The five checks that are claims about an approach. None of them may be
# printed by a run whose pair was already in reach (#1911 requirement 4).
SEPARATION_DEPENDENT = (
    "the target stopped where it stood",
    "the source walks over to the held target",
    "the target's position did not move for the WHOLE",
    "both units are held, on their two sides of the one session",
    "the unit-to-unit pair is ONE escort level with two panes",
)

SPAWN_RE = re.compile(r"unit\.spawn\('acolyte',\s*(-?\d+),\s*(-?\d+)")
INFO_RE = re.compile(r"unit\.getInfo\((\d+)\)")
MOVE_RE = re.compile(r"commandMove\((\d+),\s*(-?\d+),\s*(-?\d+)\)")


class FakeConsole:
    """A scripted debug console for the escort scenario's setup path.

    Answers only what that path actually asks and records every
    statement, so "`transfer_session.create` was never sent" is an
    observation about traffic rather than an inference from control flow.

    `close_the_gap` is the whole fixture. With it set, every move order
    aimed at the TARGET parks it at a maximum-axis gap of exactly 1.0
    from the escort — inside the contract's reach, reproducing the
    recorded run — with a per-attempt x offset so the four attempts get
    four distinct bearings and the retry diagnostics have something to
    distinguish. Without it a move order simply arrives, which is the
    separation the scenario needs."""

    def __init__(self, close_the_gap: bool, creates: str = "true") -> None:
        self.close_the_gap = close_the_gap
        self.creates = creates
        self.traffic: list[str] = []
        self.paused = False
        self.session_live = False
        self.pos: dict[int, tuple[float, float]] = {}
        self.spawned: list[int] = []
        self.parked = 0

    # -- the two console entry points the probe reaches the engine through
    def send(self, port: int, lua: str, timeout: float = 10.0,
             expect_result: bool = True, idle: float | None = None) -> str:
        self.traffic.append(lua)
        return self.answer(lua)

    def send_json(self, port: int, lua: str, timeout: float = 10.0,
                  idle: float | None = None):
        raw = self.send(port, lua, timeout=timeout)
        try:
            return json.loads(raw)
        except (TypeError, ValueError):
            return raw

    def set_paused(self, port: int, on: bool, settle: float = 0.3) -> None:
        self.traffic.append(f"engine.setPaused({'true' if on else 'false'})")
        self.paused = bool(on)

    # -- the fixture
    @property
    def source_uid(self) -> int | None:
        return self.spawned[0] if self.spawned else None

    @property
    def target_uid(self) -> int | None:
        return self.spawned[1] if len(self.spawned) > 1 else None

    def apply_move(self, uid: int, tx: float, ty: float) -> None:
        if self.close_the_gap and uid == self.target_uid:
            self.parked += 1
            ex, ey = self.pos[self.source_uid]
            # max(|dx|, |dy|) == 1.0 exactly: in reach, and NOT > 1.0.
            self.pos[uid] = (ex - 0.1 * self.parked, ey - 1.0)
        else:
            self.pos[uid] = (float(tx), float(ty))

    def answer(self, lua: str) -> str:
        spawn = SPAWN_RE.search(lua)
        if spawn:
            uid = 101 + len(self.spawned)
            self.spawned.append(uid)
            self.pos[uid] = (float(spawn.group(1)), float(spawn.group(2)))
            return str(uid)
        info = INFO_RE.search(lua)
        if info:
            x, y = self.pos[int(info.group(1))]
            return json.dumps({"gridX": x, "gridY": y})
        move = MOVE_RE.search(lua)
        if move:
            self.apply_move(int(move.group(1)), float(move.group(2)),
                            float(move.group(3)))
            return "ok"
        if "world.getSurfaceAt(" in lua:
            # `tile_surface` wants >= 3 whitespace-separated fields, a
            # numeric z and a nil fluid.
            return "12 stone null"
        if "currentAction" in lua:
            return "follow_command"
        if "unit.getKnowledge(" in lua:
            return "0.0"
        if "transfer_session" in lua and ".create(" in lua:
            self.session_live = self.creates == "true"
            return self.creates
        if "transfer_session" in lua and ".clear()" in lua:
            self.session_live = False
            return "ok"
        return "ok"


def instant_poll(seconds: float, fn, interval: float = 0.3):
    """`probelib.poll_until` with the waiting taken out: the same
    poll-until-truthy-or-budget-exhausted answer, in no wall clock.

    Deliberately still BOUNDED. A regression that let the gate admit an
    in-reach pair would run the rendered measurement behind it against
    this console, and every wait in there is a minute of real polling —
    so an unbounded stand-in would make such a mutation hang the
    self-test instead of failing it."""
    for _ in range(max(1, int(seconds / max(interval, 0.05)))):
        got = fn()
        if got:
            return got
    return None


@contextlib.contextmanager
def scripted(console: FakeConsole):
    """Point the probe's console, its pause verb, its find_water
    retirement and its waits at `console`, and take the wall clock out.

    `clear_find_water`, `set_paused` and `poll_until` come from
    `probelib` and call ITS `send`, not the probe's, so patching the
    console alone would leave all three reaching for a real socket or a
    real minute."""
    saved = {name: getattr(probe, name)
             for name in ("send", "send_json", "set_paused",
                          "clear_find_water", "poll_until", "time")}
    probe.send = console.send
    probe.send_json = console.send_json
    probe.set_paused = console.set_paused
    probe.clear_find_water = lambda port, uid, seconds=10.0: True
    probe.poll_until = instant_poll
    probe.time = types.SimpleNamespace(sleep=lambda *_a, **_k: None,
                                       time=saved["time"].time)
    try:
        yield console
    finally:
        for name, value in saved.items():
            setattr(probe, name, value)


def run_scenario(console: FakeConsole) -> tuple[str, int]:
    """Drive the real scenario against `console`; answer its printed
    output and how many checks it failed.

    The probe's accumulator is a module global, so it is zeroed for the
    run and restored afterwards: a case here must count its OWN failures
    rather than whatever the case before it left behind."""
    saved = probe.failures
    probe.failures = 0
    out = io.StringIO()
    try:
        with scripted(console), contextlib.redirect_stdout(out):
            probe.unit_escort_session_scenario(PORT, *ANCHOR)
        return out.getvalue(), probe.failures
    finally:
        probe.failures = saved


def failed_lines(output: str) -> list[str]:
    return [line for line in output.splitlines() if "[FAIL]" in line]


# --------------------------------------------------------------------------
# The negative path: a fixture that never separates
# --------------------------------------------------------------------------
def test_an_unestablished_fixture_fails_at_setup() -> None:
    print("\na staging loop that never opens the gap")
    console = FakeConsole(close_the_gap=True)
    output, failed = run_scenario(console)
    fails = failed_lines(output)

    expect(failed == 1,
           f"exactly one check is counted as failed (got {failed})")
    expect(len(fails) == 1 and "setup:" in fails[0],
           f"and it is labeled a SETUP failure ({fails!r})")
    expect("outside the contract's own reach rule" in "\n".join(fails),
           f"it is the reach precondition that failed ({fails!r})")


def test_the_setup_failure_creates_no_session_and_grades_nothing() -> None:
    print("\nwhat the setup failure did NOT do")
    console = FakeConsole(close_the_gap=True)
    output, _ = run_scenario(console)
    sent = "\n".join(console.traffic)

    expect(".create(" not in sent,
           "transfer_session.create was never sent")
    graded = [name for name in SEPARATION_DEPENDENT if name in output]
    expect(not graded,
           f"no separation-dependent check was graded ({graded!r})")
    expect(console.paused is False,
           "the simulation is left running, not stopped mid-measurement")
    expect(console.session_live is False
           and any(".clear()" in lua for lua in console.traffic),
           "no session is left behind — the scenario cleared one on its "
           "way out, exactly as its later exits do")


def test_the_setup_failure_retains_every_attempt() -> None:
    print("\nthe retry evidence in the failure output")
    console = FakeConsole(close_the_gap=True)
    output, _ = run_scenario(console)
    detail = failed_lines(output)[0]

    for n in (1, 2, 3, 4):
        expect(f"#{n} sent to " in detail,
               f"attempt {n}'s destination is retained ({detail!r})")
    bearings = re.findall(r"#\d sent to \((-?[\d.]+), (-?[\d.]+)\)", detail)
    expect(len(bearings) == 4 and len(set(bearings)) > 1,
           f"the destinations are the ones each attempt really used, not "
           f"one value repeated ({bearings!r})")
    expect(detail.count("escort at (") == 4
           and detail.count("target at (") == 4,
           f"both paused endpoint positions are retained per attempt "
           f"({detail!r})")
    # The four snapshots are the sharp half: before #1911 a single pair of
    # variables held only the LAST one, so four distinct target positions
    # in one line is what a per-attempt record looks like and an
    # overwritten one cannot produce.
    snapshots = re.findall(r"target at \((-?[\d.]+), (-?[\d.]+)\)", detail)
    expect(len(snapshots) == 4 and len(set(snapshots)) == 4,
           f"the four paused snapshots are four DISTINCT observations "
           f"({snapshots!r})")
    expect(detail.count("Chebyshev 1.000") == 4,
           f"each attempt's measured gap is retained ({detail!r})")


def test_the_loop_is_bounded_and_tries_no_new_heuristic() -> None:
    print("\nthe staging loop itself")
    console = FakeConsole(close_the_gap=True)
    with scripted(console), contextlib.redirect_stdout(io.StringIO()):
        console.spawned.extend((101, 102))
        console.pos[101] = (25.0, -151.0)
        console.pos[102] = (25.0, -151.0)
        staging = probe.stage_escort_separation(PORT, 101, 102)
    expect(len(staging.attempts) == 4,
           f"it stops after its four attempts ({len(staging.attempts)})")
    expect(staging.ordered and not staging.separated,
           "the order was taken up every time and the gap never opened")
    expect(staging.last["gap"] == 1.0,
           f"a gap of exactly 1.0 is IN reach, not out of it "
           f"({staging.last['gap']!r})")
    expect(console.paused is True,
           "an exhausted loop hands its LAST pause back to the caller, so "
           "the teardown is what restores the simulation rather than the "
           "loop happening to have resumed it")


# --------------------------------------------------------------------------
# The positive control: the same scenario over a console that separates
# --------------------------------------------------------------------------
def test_a_separated_fixture_reaches_the_measurement() -> None:
    print("\na staging loop that does open the gap")
    # `creates="false"` stops the run at the first check past the gate,
    # which is the point: what is asserted is that the gate ADMITTED the
    # pair and the create statement was reached, without this test having
    # to fake the whole rendered measurement behind it.
    console = FakeConsole(close_the_gap=False, creates="false")
    output, failed = run_scenario(console)
    fails = failed_lines(output)

    expect(not any("setup:" in line for line in fails),
           f"no setup precondition failed ({fails!r})")
    expect(any(".create(" in lua for lua in console.traffic),
           "transfer_session.create IS reached once the pair is separated")
    expect(failed == 1 and "a unit-to-unit Mode A session is created"
           in "\n".join(fails),
           f"the run stops at the refused session, past the gate ({fails!r})")
    expect(console.paused is False and console.session_live is False,
           "that exit restores the engine the same way the setup one does")


def test_the_separation_measure_is_the_contract_s_own() -> None:
    print("\nthe reach measure")
    staging = probe.EscortSeparation()
    staging.record((9.0, 9.0), (0.0, 0.0), (1.0, 1.0))
    expect(not staging.separated, "Chebyshev 1.0 is in reach")
    staging.record((9.0, 9.0), (0.0, 0.0), (0.0, 1.5))
    expect(staging.separated, "Chebyshev 1.5 is out of reach")
    staging.record(None, None, None)
    expect(not staging.ordered and not staging.separated,
           "an order the AI never took up is neither ordered nor separated")
    expect(probe.EscortSeparation().detail() == "no attempt was made",
           "an empty staging says so rather than raising")


def test_the_exit_status_follows_the_accumulator() -> None:
    """A setup failure is only terminal for the SCENARIO; what makes it
    terminal for the RUN is this, so the two are pinned together."""
    print("\nthe run's exit status")
    saved, out = probe.failures, io.StringIO()
    try:
        for count, expected in ((0, 0), (1, 1), (7, 1)):
            probe.failures = count
            with contextlib.redirect_stdout(out):
                got = probe.probe_result()
            expect(got == expected,
                   f"{count} failed check(s) exits {expected} (got {got})")
    finally:
        probe.failures = saved
    expect("1 check(s) FAILED" in out.getvalue()
           and "all checks passed" in out.getvalue(),
           f"each outcome says which it was ({out.getvalue()!r})")


def main() -> int:
    selftestlib.parse_verbose()
    test_an_unestablished_fixture_fails_at_setup()
    test_the_setup_failure_creates_no_session_and_grades_nothing()
    test_the_setup_failure_retains_every_attempt()
    test_the_loop_is_bounded_and_tries_no_new_heuristic()
    test_a_separated_fixture_reaches_the_measurement()
    test_the_separation_measure_is_the_contract_s_own()
    test_the_exit_status_follows_the_accumulator()
    if FAILURES:
        print(f"\n{len(FAILURES)} test(s) failed:")
        for failure in FAILURES:
            print(f"  {failure}")
        return selftestlib.concluded(1)
    return selftestlib.concluded(
        0, "\nAll item_list_widget_probe escort-setup tests passed")


if __name__ == "__main__":
    raise SystemExit(main())
