#!/usr/bin/env python3
"""Headless probe for issue #1713: a launched lunge must reach its
LANDING STRIKE, not its timeout.

THE DEFECT. `scripts/unit_ai_combat_lunge.lua`'s phase 2 (before #1713 it
lived in `unit_ai_combat_attack.lua`) only strikes once it has seen the
leap in the air -- `lungeSawAir`. But a leap is `TransitioningTo Falling`,
which `activityLabel` (`src/Unit/Thread.hs`) reports to Lua as
`activity == "transitioning"`, and `unit_ai.lua`'s `tickOne` returns on
exactly that activity so an AI command cannot clobber a pose transition.
The only writer of `lungeSawAir` therefore sat downstream of a guard that
was guaranteed to be closed for the whole flight. The flag could never
become true, the strike could never fire, and every lunge ended at
`LUNGE_TIMEOUT_SEC` having delivered nothing -- discarding the reach bonus
and impact speed the lunge exists to apply.

WHY THE EXISTING PROBES DON'T COVER IT. `tools/mental_state_probe.py`'s
lash-out stages spawn ACOLYTES, whose ordinary melee reach never sends
them down the lunge path at all, and it asserts that *an* attack landed
rather than that the lunge's strike did -- an ordinary close-range swing
after the timeout satisfies it. `tools/combat_anim_probe.py` mentions the
lunge only in a comment about telling elevation from pose. Nothing in the
tree drove a SHORT-REACH species through leap -> land -> strike.

THE SPECIES MATTERS. The subject here is a `red_squirrel`: attack range
~0.12 tiles, so it can essentially never be "in reach" of anything and
`tryLunge` is the path it takes, and intelligence ~0.2, which puts it in
`shouldLunge`'s instinct regime (propensity 0.85) rather than the
skill-gated technique one. An acolyte fixture would prove nothing here.

WHAT IS OBSERVED, AND WHY IT IS THE `combat.attack` BOUNDARY. The claim
under test is about the ARGUMENTS of one call: the launched target, the
lunge's stored mode, its stored strike-reach and its stored impact speed.
`unit.getLastAttacker` cannot establish any of that -- it records only
that some attacker landed something -- and `Combat.Resolution.Events`
exposes a boolean `lunge` marker, not the reach value. So this probe
wraps `combat.attack` in Lua and grades the recorded argument tuples,
which is the exact record the assertion needs.

The wrapper RECORDS AND DOES NOT DELEGATE. That is deliberate on both
counts. Resolving the strike would kill the squirrel within a couple of
seconds (an acolyte target retaliates and wins), and it would stamp
`uiLastAttackerUid`, opening the mid-fight retaliation branch and
replacing the target under the very binding the cancellation cases exist
to test. Neither is a property of the lunge. What IS a property of the
lunge is which call it makes with which arguments, and that is upstream of
resolution.

IDENTITY, NOT COUNTING. Every case snapshots the seven lunge fields the
moment the launch is observed and then grades against that lunge's own
identity triple -- (launched target, stored reach, stored impact speed).
That is what separates the landing strike from the two other calls this
same code makes with a positive reach: the `d < 0.5` in-place pounce
(reach = the full `jr.height`) and an ordinary close-range swing (reach
absent). A bare "some attack happened" count could not tell them apart,
which is precisely the weakness of the coverage that already existed.

THE FIVE FIELD-CLEARING CASES. `lungeTarget` is a PERSISTED typed
reference (`scripts/unit_ai_ref_schema.lua`), so a lunge that ends
without clearing its bookkeeping strands a stale unit reference where a
save can pick it up. Every case therefore also asserts that all seven
fields -- read from the module's own `LUNGE_FIELDS` list, so the probe
cannot drift from the code -- are nil once the lunge is over.

  A. success   -- the launched lunge lands and strikes exactly once with
                  its own stored arguments; continued ticks add no second
                  call; the seven fields are cleared.
  B. replaced  -- the attack target is swapped mid-flight. The stored
                  reach and impact speed belong to a leap at the ORIGINAL
                  unit, so the lunge must cancel rather than hand a
                  substitute a free momentum strike.
  C. timeout   -- the lunge's own clock is aged past LUNGE_TIMEOUT_SEC
                  mid-flight (the engine exposes no way to stall a leap,
                  and a real 3 s wait would still have to be forced to
                  miss its landing). No strike, seven fields cleared.
  D. unlifted  -- `unit.jump` returns true without a leap ever starting.
                  A true return proves only that the command was ENQUEUED
                  (`Engine.Scripting.Lua.API.Units.Spawn`;
                  `Unit.Thread.Command.Motion` can still reject it), so
                  the strike must stay gated on OBSERVED air: no
                  `lungeSawAir`, no strike, and the timeout cleans up.

Exit codes: 0 = every declared check passed, 1 = a behavioural failure,
2 = the fixture could not be established (checks left NOT RUN).

Usage:
  python3 tools/lunge_probe.py [--port 9361]
  python3 tools/lunge_probe.py --self-test     # no engine
"""
from __future__ import annotations

import argparse
import glob
import time

from probelib import (boot, init_arena, load_ai_stack, poll_until,
                      quit_engine, send, send_json, spawn_acolyte)

LOG = "/tmp/lunge_probe_engine.log"

#: Every check this probe intends to grade, in report order. A fixture
#: failure reports the ones it never reached as NOT RUN rather than
#: silently omitting them (the accounting `retaliation_swap_probe.py`
#: introduced for the same reason).
DECLARED_CHECKS: tuple[tuple[str, str], ...] = (
    ("strike", "a landed lunge fires its strike instead of timing out"),
    ("args", "the strike carries the launched target, the stored mode, and a "
             "positive stored reach and impact speed"),
    ("once", "the strike resolves at most once — continued ticks add no second call"),
    ("clear_success", "all seven lunge fields are cleared after the strike"),
    ("replaced_nostrike", "a target replaced mid-flight fires no lunge strike"),
    ("replaced_clear", "…and all seven lunge fields are cleared"),
    ("timeout_nostrike", "a lunge aged past its timeout fires no lunge strike"),
    ("timeout_clear", "…and all seven lunge fields are cleared"),
    ("unlifted_noair", "a launch the engine never lifts never observes air"),
    ("unlifted_clear", "…fires no strike and clears all seven lunge fields"),
)

#: How long to keep ticking after the strike while asserting no second
#: call. Phase 2 has NO cooldown gate of its own — a failure to clear the
#: bookkeeping re-fires it on EVERY tick — so a window of this size sees
#: many ticks and does not depend on the attack cooldown's length.
SECOND_CALL_WINDOW = 1.5

#: Bounded re-staging: `shouldLunge` is a probability roll (0.85 for this
#: species) and a leap taken at very close to maximum distance produces a
#: strike-reach near zero, which is a degenerate fixture for the "positive
#: stored reach" assertion rather than a result. Both are re-staged.
LAUNCH_ATTEMPTS = 6


class FixtureFailure(RuntimeError):
    """The scenario could not be established, so nothing was graded."""


class Ledger:
    """Declared-vs-recorded check accounting."""

    def __init__(self, declared: tuple[tuple[str, str], ...] = DECLARED_CHECKS):
        self._names = dict(declared)
        self._order = [k for k, _ in declared]
        self._results: dict[str, tuple[bool, str]] = {}

    def record(self, key: str, passed: bool, detail: str = "") -> None:
        if key not in self._names:
            raise KeyError(f"undeclared check {key!r}")
        self._results[key] = (passed, detail)
        mark = "pass" if passed else "FAIL"
        suffix = f" — {detail}" if detail else ""
        print(f"  [{mark}] {self._names[key]}{suffix}")

    def unrun(self) -> list[str]:
        return [k for k in self._order if k not in self._results]

    def failed(self) -> list[str]:
        return [k for k in self._order if k in self._results
                and not self._results[k][0]]

    def report(self) -> int:
        unrun, failed = self.unrun(), self.failed()
        if unrun:
            print(f"\nNOT RUN ({len(unrun)}): "
                  + ", ".join(self._names[k] for k in unrun))
        if failed:
            print(f"\n{len(failed)} check(s) FAILED")
            return 1
        if unrun:
            return 2
        print(f"\nall {len(self._order)} lunge checks passed")
        return 0


# --------------------------------------------------------------------------
# Engine helpers
# --------------------------------------------------------------------------
def lua(*statements: str) -> str:
    """One console line. The debug console is single-line only."""
    return " ".join(statements)


def bootstrap(port: int) -> None:
    """Register the catalogs the loading screen would, then arena + AI."""
    for pattern, fn in (
        ("data/substances/*.yaml", "engine.loadSubstanceYaml"),
        ("data/infections/*.yaml", "engine.loadInfectionYaml"),
        ("data/items/*.yaml", "engine.loadItemYaml"),
        ("data/equipment/*.yaml", "engine.loadEquipmentYaml"),
        ("data/materials/*.yaml", "engine.loadMaterialYaml"),
        ("data/units/*.yaml", "engine.loadUnitYaml"),
    ):
        for path in sorted(glob.glob(pattern)):
            send(port, f"{fn}('{path}'); return 'ok'")
    load_ai_stack(port)
    init_arena(port)


def install_recorder(port: int) -> None:
    """Wrap `combat.attack` so every call's arguments are captured.

    Record-only, never delegating — see the module docstring: resolving
    the strike would kill the subject and would open the retaliation
    branch that replaces the target under the binding cases B tests.
    """
    send(port, lua(
        "_G.__lg = {};",
        "if not _G.__lg_atk then _G.__lg_atk = combat.attack end;",
        "combat.attack = function(a, t, m, r, sp)",
        " _G.__lg[#_G.__lg + 1] = { atk = a, tgt = t, mode = m or '',",
        "                           reach = r or 0, speed = sp or 0 };",
        " return true end;",
        "return 'ok'"))


def reset_recorder(port: int) -> None:
    send(port, "_G.__lg = {}; return 'ok'")


def calls(port: int) -> list[dict]:
    got = send_json(port, "return _G.__lg")
    return got if isinstance(got, list) else []


def restrict_ai(port: int, ids: list[int]) -> None:
    """Make `unitAi.update` tick exactly `ids`.

    Everything left out is off the AI tick entirely, so a fixture target
    cannot wander out of leap range, fight back, or take its own turn
    while a launch is being staged or graded. `unit.getAllIds` is built
    from `HashMap.keys` with no ordering contract, so spawn order alone
    could not establish this.
    """
    listing = ", ".join(str(i) for i in ids)
    send(port, lua(
        "if not _G.__lg_ids then _G.__lg_ids = unit.getAllIds end;",
        f"unit.getAllIds = function() return {{ {listing} }} end;",
        "return 'ok'"))


def unrestrict_ai(port: int) -> None:
    """Restore the engine's own `unit.getAllIds`.

    A freshly spawned unit only gets AI state from its own `tickOne`, so a
    scenario must lift the previous scenario's restriction before it
    spawns -- otherwise its units never tick and never acquire the state
    every later read depends on.
    """
    send(port, lua(
        "if _G.__lg_ids then unit.getAllIds = _G.__lg_ids;",
        " _G.__lg_ids = nil end;",
        "return 'ok'"))


def stub_jump(port: int, uid: int) -> None:
    """Accept the jump command for `uid` and never lift the unit.

    This is what `Unit.Thread.Command.Motion` rejecting a queued jump
    looks like from Lua: `unit.jump` already returned true.
    """
    send(port, lua(
        "if not _G.__lg_jump then _G.__lg_jump = unit.jump end;",
        f"unit.jump = function(u, ...) if u == {uid} then return true end;",
        " return _G.__lg_jump(u, ...) end;",
        "return 'ok'"))


def deny_jump(port: int, uid: int) -> None:
    """Refuse `uid` any FURTHER jump, leaving the in-flight one alone.

    Case D would otherwise never observe its lunge end: the moment the
    timeout clears the bookkeeping, phase 1 re-launches against the same
    still-accepting stub and the phase reads "air" again. Denying the
    command outright is what the engine does when a leap is impossible, so
    the cancelled lunge is the last one.
    """
    send(port, lua(
        "if not _G.__lg_jump then _G.__lg_jump = unit.jump end;",
        f"unit.jump = function(u, ...) if u == {uid} then return false end;",
        " return _G.__lg_jump(u, ...) end;",
        "return 'ok'"))


def unstub_jump(port: int) -> None:
    send(port, lua(
        "if _G.__lg_jump then unit.jump = _G.__lg_jump; _G.__lg_jump = nil end;",
        "return 'ok'"))


def lunge_fields(port: int) -> list[str]:
    """The module's own field list, so this probe cannot drift from it."""
    got = send_json(port, lua(
        "local L = require('scripts.unit_ai_combat_lunge');",
        "return L.LUNGE_FIELDS"))
    if not isinstance(got, list) or not got:
        raise FixtureFailure(
            "scripts/unit_ai_combat_lunge.lua exposes no LUNGE_FIELDS list, so "
            "the seven-field cleanup cannot be graded against the code's own "
            "definition")
    return [str(f) for f in got]


def lunge_state(port: int, uid: int) -> dict:
    """The whole lunge bookkeeping set plus the pose/activity around it."""
    got = send_json(port, lua(
        "local L = require('scripts.unit_ai_combat_lunge');",
        f"local s = require('scripts.unit_ai').getState({uid});",
        "local left = {};",
        "if s then for _, f in ipairs(L.LUNGE_FIELDS) do",
        " if s[f] ~= nil then left[#left + 1] = f end end end;",
        "return { phase = (s and s.lungePhase) or '',",
        "         saw = (s and s.lungeSawAir) == true,",
        "         target = (s and s.lungeTarget) or -1,",
        "         mode = (s and s.lungeMode) or '',",
        "         reach = (s and s.lungeReach) or -1,",
        "         speed = (s and s.lungeImpactSpeed) or -1,",
        "         startAt = (s and s.lungeStartAt) or -1,",
        "         attackTarget = (s and s.attackTargetUid) or -1,",
        "         left = table.concat(left, ','),",
        f"         pose = unit.getPose({uid}), activity = unit.getActivity({uid}),",
        "         calls = #(_G.__lg or {}) }"))
    if not isinstance(got, dict):
        raise FixtureFailure(f"could not read unit {uid}'s lunge state: {got!r}")
    return got


def leftover_fields(port: int, uid: int) -> str:
    return str(lunge_state(port, uid).get("left", "?"))


def lunge_strikes(recorded: list[dict], launch: dict) -> list[dict]:
    """The recorded calls that ARE this launch's landing strike.

    Matched on the launch's own identity triple rather than counted: the
    same module also calls `combat.attack` for the in-place pounce (a
    different, larger reach) and for ordinary swings (no reach at all),
    and a count could not tell those from the strike under test.
    """
    return [c for c in recorded
            if c.get("tgt") == launch["target"]
            and _close(c.get("reach"), launch["reach"])
            and _close(c.get("speed"), launch["speed"])]


def _close(a, b, tol: float = 1e-6) -> bool:
    try:
        return abs(float(a) - float(b)) <= tol
    except (TypeError, ValueError):
        return False


# --------------------------------------------------------------------------
# Fixture
# --------------------------------------------------------------------------
class Fixture:
    """One scenario's own subject, target and (optionally) decoy.

    Every scenario stages a FRESH set at its own arena row. Sharing one
    subject across cases would let an earlier case's landing position,
    stamina spend and swing cooldown decide whether a later one can even
    launch.
    """

    def __init__(self, label: str, row: int, sep: int = 2, decoy: bool = False):
        self.label = label
        self.row = row
        self.sep = sep
        self.wants_decoy = decoy
        self.subject = -1
        self.target = -1
        self.decoy = -1

    def spawn(self, port: int) -> None:
        unrestrict_ai(port)
        self.subject = spawn_acolyte(port, 0, self.row, unit="red_squirrel")
        self.target = spawn_acolyte(port, self.sep, self.row, clear_water=False)
        if self.wants_decoy:
            self.decoy = spawn_acolyte(port, -self.sep, self.row,
                                       clear_water=False)
        # The subject alone: everything else stays off the AI tick, so no
        # fixture unit walks out of leap range or takes a turn of its own
        # while a launch is being staged or graded.
        restrict_ai(port, [self.subject])

    def reach_precondition(self, port: int) -> None:
        """Refuse to grade a subject the lunge path does not apply to."""
        got = send_json(port, lua(
            f"local jr = unit.getJumpReach({self.subject});",
            f"return {{ range = unit.getAttackRange({self.subject}) or -1,",
            f"          intel = unit.getStat({self.subject}, 'intelligence') or -1,",
            "          dist = (jr and jr.dist) or -1,",
            "          height = (jr and jr.height) or -1 }"))
        if not isinstance(got, dict):
            raise FixtureFailure(f"{self.label}: could not read the subject's reach")
        # The whole point of the species choice: a unit whose melee reach
        # already covers the separation would swing, never lunge.
        if got["range"] >= self.sep:
            raise FixtureFailure(
                f"{self.label}: subject melee reach {got['range']:.3f} covers the "
                f"{self.sep}-tile separation, so tryLunge is not the path taken")
        if got["dist"] <= 1.0 or got["height"] <= 0.0:
            raise FixtureFailure(
                f"{self.label}: jump reach dist={got['dist']:.3f} "
                f"height={got['height']:.3f} cannot produce a positive "
                "strike-reach envelope")

    def restage(self, port: int) -> None:
        """Put the subject back at its launch position, stopped and idle."""
        send(port, lua(
            f"unit.stop({self.subject});",
            f"unit.setPos({self.subject}, 0, {self.row});",
            f"unit.setPos({self.target}, {self.sep}, {self.row});",
            "return 'ok'"))

    def command(self, port: int) -> None:
        send(port, lua(
            "local ai = require('scripts.unit_ai');",
            f"ai.commandAttack({self.subject}, {self.target});",
            "return 'ok'"))


def await_launch(port: int, fx: Fixture, want_positive: bool = True) -> dict:
    """Drive the subject until a lunge is genuinely in flight; snapshot it.

    Returns the launch's stored bookkeeping — the identity every later
    assertion is made against.
    """
    fx.reach_precondition(port)
    for attempt in range(LAUNCH_ATTEMPTS):
        fx.restage(port)
        fx.command(port)
        reset_recorder(port)
        got = poll_until(4.0, lambda: _airborne_snapshot(port, fx.subject),
                         interval=0.1)
        if got is None:
            continue
        if want_positive and (got["reach"] <= 0.0 or got["speed"] <= 0.0):
            # A leap taken at essentially maximum distance flattens the
            # reach envelope to ~0. Re-stage rather than grade "positive"
            # against a degenerate launch.
            poll_until(4.0, lambda: lunge_state(port, fx.subject)["phase"] != "air",
                       interval=0.1)
            continue
        return got
    raise FixtureFailure(
        f"{fx.label}: no lunge launched with usable stored values in "
        f"{LAUNCH_ATTEMPTS} attempts — the subject never entered phase 'air' "
        "with a positive stored reach and impact speed")


def _airborne_snapshot(port: int, uid: int):
    st = lunge_state(port, uid)
    return st if st.get("phase") == "air" else None


# --------------------------------------------------------------------------
# Scenarios
# --------------------------------------------------------------------------
def case_success(port: int, ledger: Ledger) -> None:
    fx = Fixture("success", row=0)
    fx.spawn(port)
    launch = await_launch(port, fx)
    print(f"  launched: target={launch['target']} mode={launch['mode']!r} "
          f"reach={launch['reach']:.4f} speed={launch['speed']:.4f}")

    landed = await_strike(port, fx.subject, launch)
    if not landed:
        ledger.record("strike", False,
                      "no call carrying the launch's stored reach and impact "
                      "speed appeared before the lunge cleared — the "
                      "pre-#1713 behaviour, where the phase ends at "
                      "LUNGE_TIMEOUT_SEC having struck nothing")
        ledger.record("args", False, "no strike to inspect")
    else:
        ledger.record("strike", True)
        strike = landed[0]
        ok = (strike["tgt"] == launch["target"]
              and strike["mode"] == launch["mode"]
              and launch["reach"] > 0.0 and launch["speed"] > 0.0)
        ledger.record("args", ok,
                      f"tgt={strike['tgt']} mode={strike['mode']!r} "
                      f"reach={strike['reach']:.4f} speed={strike['speed']:.4f}")

    # Continued ticks must not re-fire it. Phase 2 carries no cooldown of
    # its own, so a failure to clear the bookkeeping would strike on every
    # tick in this window.
    time.sleep(SECOND_CALL_WINDOW)
    again = lunge_strikes(calls(port), launch)
    ledger.record("once", len(again) <= 1,
                  f"{len(again)} call(s) carrying this lunge's identity over "
                  f"{SECOND_CALL_WINDOW:.1f}s of continued ticks")

    left = leftover_fields(port, fx.subject)
    ledger.record("clear_success", left == "",
                  f"left set: {left or '(none)'}")


def case_replaced(port: int, ledger: Ledger) -> None:
    fx = Fixture("replaced target", row=30, decoy=True)
    fx.spawn(port)
    launch = await_launch(port, fx)
    # Swap the goal's target mid-flight, exactly as the mid-fight
    # retaliation branch does. The stored reach and impact speed describe
    # a leap at the ORIGINAL unit.
    send(port, lua(
        f"local s = require('scripts.unit_ai').getState({fx.subject});",
        f"if s then s.attackTargetUid = {fx.decoy} end;",
        "return 'ok'"))
    _grade_cancellation(port, ledger, fx, launch,
                        clear_key="replaced_clear",
                        nostrike_key="replaced_nostrike")


def case_timeout(port: int, ledger: Ledger) -> None:
    fx = Fixture("timeout", row=60)
    fx.spawn(port)
    launch = await_launch(port, fx)
    # The engine offers no way to stall a leap in the air, so age the
    # lunge's OWN clock instead: the field the timeout is measured from.
    send(port, lua(
        "local L = require('scripts.unit_ai_combat_lunge');",
        f"local s = require('scripts.unit_ai').getState({fx.subject});",
        "if s then s.lungeStartAt = engine.gameTime()",
        "                          - (L.LUNGE_TIMEOUT_SEC + 1.0) end;",
        "return 'ok'"))
    _grade_cancellation(port, ledger, fx, launch,
                        clear_key="timeout_clear",
                        nostrike_key="timeout_nostrike")


def case_unlifted(port: int, ledger: Ledger) -> None:
    fx = Fixture("unlifted launch", row=90)
    fx.spawn(port)
    stub_jump(port, fx.subject)
    try:
        # No leap ever happens, so the reach envelope is whatever the
        # staged geometry implies; `want_positive` still holds because the
        # stored values are computed before the jump is issued.
        launch = await_launch(port, fx)
        deny_jump(port, fx.subject)
        saw_air = poll_until(
            1.5, lambda: lunge_state(port, fx.subject)["saw"] or None,
            interval=0.1)
        ledger.record("unlifted_noair", saw_air is None,
                      "lungeSawAir stayed false while the unit never left "
                      "the ground" if saw_air is None
                      else "lungeSawAir became true with no leap in progress")
        _grade_cancellation(port, ledger, fx, launch,
                            clear_key="unlifted_clear")
    finally:
        unstub_jump(port)


def await_strike(port: int, uid: int, launch: dict,
                 seconds: float = 6.0) -> list[dict]:
    """Poll for this launch's landing strike until the lunge is over.

    Returns as soon as a matching call appears, and also as soon as the
    lunge leaves phase "air" without one -- the pre-#1713 outcome, which
    should report promptly rather than sit out the whole budget.
    """
    deadline = time.time() + seconds
    while time.time() < deadline:
        hits = lunge_strikes(calls(port), launch)
        if hits:
            return hits
        if lunge_state(port, uid)["phase"] != "air":
            return lunge_strikes(calls(port), launch)
        time.sleep(0.1)
    return lunge_strikes(calls(port), launch)


def _grade_cancellation(port: int, ledger: Ledger, fx: Fixture, launch: dict,
                        clear_key: str, nostrike_key: str | None = None,
                        settle: float = SECOND_CALL_WINDOW) -> None:
    """Wait out the cancellation, then grade "no strike" + "fields cleared".

    `settle` keeps ticking past the clear: a cancellation that dropped the
    phase but left the rest of the bookkeeping behind would be caught by
    the leftover read, and a strike arriving late would be caught by the
    identity match.
    """
    cleared = poll_until(
        float(settle) + 8.0,
        lambda: lunge_state(port, fx.subject)["phase"] != "air" or None,
        interval=0.1)
    if cleared is None:
        raise FixtureFailure(
            f"{fx.label}: the lunge never left phase 'air', so neither the "
            "no-strike nor the cleanup assertion was reached")
    left = leftover_fields(port, fx.subject)
    hits = lunge_strikes(calls(port), launch)
    if nostrike_key is not None:
        ledger.record(nostrike_key, not hits,
                      f"{len(hits)} call(s) carrying the cancelled lunge's "
                      "identity")
        ledger.record(clear_key, left == "", f"left set: {left or '(none)'}")
        return
    ledger.record(clear_key, not hits and left == "",
                  f"{len(hits)} lunge strike(s); left set: {left or '(none)'}")


# --------------------------------------------------------------------------
# Entry points
# --------------------------------------------------------------------------
def self_test() -> int:
    """Exercise the accounting and the identity match with no engine."""
    failures: list[str] = []

    launch = {"target": 7, "reach": 1.25, "speed": 4.5, "mode": "quick"}
    recorded = [
        {"atk": 1, "tgt": 7, "mode": "quick", "reach": 0.0, "speed": 0.0},
        {"atk": 1, "tgt": 7, "mode": "quick", "reach": 2.07, "speed": 3.1},
        {"atk": 1, "tgt": 7, "mode": "quick", "reach": 1.25, "speed": 4.5},
    ]
    hits = lunge_strikes(recorded, launch)
    if len(hits) != 1 or hits[0]["reach"] != 1.25:
        failures.append("lunge_strikes did not isolate the landing strike from "
                        "the in-place pounce and the ordinary swing")
    if lunge_strikes(recorded, {**launch, "target": 8}):
        failures.append("lunge_strikes matched a call against a different target")

    led = Ledger((("a", "first"), ("b", "second")))
    if led.report() != 2:
        failures.append("a ledger with nothing recorded must report NOT RUN (2)")
    led.record("a", True)
    led.record("b", False)
    if led.report() != 1:
        failures.append("a ledger with a failure must report 1")
    led2 = Ledger((("a", "first"),))
    led2.record("a", True)
    if led2.report() != 0:
        failures.append("a fully-passing ledger must report 0")
    try:
        led2.record("nope", True)
    except KeyError:
        pass
    else:
        failures.append("recording an undeclared check must raise")

    for f in failures:
        print(f"  [FAIL] {f}")
    if failures:
        print(f"\n{len(failures)} self-test failure(s)")
        return 1
    print("self-test: all checks passed")
    return 0


def main() -> int:
    ap = argparse.ArgumentParser()
    ap.add_argument("--port", type=int, default=9361)
    ap.add_argument("--self-test", action="store_true",
                    help="run the no-engine accounting checks and exit")
    args = ap.parse_args()
    if args.self_test:
        return self_test()

    port = args.port
    ledger = Ledger()
    proc = boot(port, log=LOG)
    failure: FixtureFailure | None = None
    try:
        bootstrap(port)
        install_recorder(port)
        fields = lunge_fields(port)
        if len(fields) != 7:
            raise FixtureFailure(
                "scripts/unit_ai_combat_lunge.lua declares "
                f"{len(fields)} lunge field(s), not the seven issue #1713's "
                f"requirement 3 names: {fields}")
        print(f"lunge bookkeeping fields: {', '.join(fields)}")

        print("\n-- A. a launched lunge lands and strikes --")
        case_success(port, ledger)
        print("\n-- B. the target is replaced mid-flight --")
        case_replaced(port, ledger)
        print("\n-- C. the lunge outlives its timeout --")
        case_timeout(port, ledger)
        print("\n-- D. the engine never lifts the launch --")
        case_unlifted(port, ledger)
    except FixtureFailure as exc:
        failure = exc
        print(f"\nFIXTURE FAILURE: {exc}")
    finally:
        quit_engine(port, proc)

    code = ledger.report()
    if failure is not None:
        return 2
    return code


if __name__ == "__main__":
    raise SystemExit(main())
