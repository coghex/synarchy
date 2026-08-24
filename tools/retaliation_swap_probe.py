#!/usr/bin/env python3
"""Headless probe for issue #1483: the mid-fight retaliation swap must
not abort the unit-AI tick.

`scripts/unit_ai_combat_attack.lua`'s `attack_target` execute holds a
mid-fight RETALIATION branch: when someone other than the current target
hit us inside `RETALIATE_WINDOW_SEC` and stands within melee reach, the
attacker rounds on them. The #538 split moved that comparison into the
attack module and left the constant `local` in `unit_ai_combat.lua`, so
the name read as an unassigned GLOBAL — `number <= nil`, which raises
*attempt to compare number with nil*.

Nothing inside `unitAi.update` catches it. `Engine.Scripting.Lua.Script`'s
`callModuleFunctionReportingError` wraps the WHOLE module callback in one
`Lua.pcall`, so the Lua thread survives and the engine logs
`Lua error in update(): ...` — but every unit ordered after the erroring
one in that tick's `unit.getAllIds()` is skipped.

WHY THE EXISTING PROBES DON'T COVER IT. `tools/mental_state_probe.py`
samples this same 3-second window, but it stages a COLLAPSED attacker and
the branch's `attPose ~= "collapsed"` term short-circuits the `and` chain
before the comparison is ever reached. An excluded attacker (dead,
collapsed, technomule) can never exercise the defect.

WHAT THIS PROBE PINS. Two cases, differing only in how old the staged hit
is, each graded from ONE atomic console chunk that re-establishes the
branch's preconditions and then drives a single `unitAi.update(dt)` under
`pcall` — so nothing can drift between the precondition and the grade.

  1. one definition — `scripts.unit_ai_combat` exports the window and the
     duration is still three seconds, so the consumer module can see the
     single definition instead of reading an unassigned global. Graded,
     never a fixture gate: a missing export IS the pre-#1483 state, and
     stopping on it would leave every behavioural case below unrun against
     the code they exist to fail on.
  2. setup, ONCE PER WINDOW — a REAL hit from that window's own third live
     unit makes it that window's subject's recorded recent attacker
     (`unit.getLastAttacker`).

  Then, per case, the preconditions captured inside the graded chunk: the
  subject targets the ORIGINAL target on an `attack` goal, the recent
  attacker is the third unit at the age this case staged, both units are
  live and non-collapsed, the subject's blood is above the rise threshold,
  the attacker is not a technomule, and its CURRENT Chebyshev distance is
  within `unit.getAttackRange(subject) + 0.5`. Anything else is a FIXTURE
  FAILURE (exit 2), not a behavioural result — which is precisely how the
  existing collapsed-attacker case passes today without reaching the
  comparison.

  3. fresh window — no `Lua error in update()` entry appears in the
     captured engine log across a window of NATURAL engine ticks with
     those preconditions standing.
  4. fresh window — the subject's attack target becomes the third unit.
  5. fresh window — the subject's tick reaches a POST-EXECUTE completion
     point: `s.nextActionAt` advances, which `unit_ai.lua`'s `tickOne`
     only does via `core.scheduleNext` after the execute returns.
  6. fresh window — a SENTINEL unit ordered AFTER the subject is reached
     by the SAME `unitAi.update()` invocation (its own `nextActionAt`
     advances from the zero set immediately before). That ordering is
     FORCED by wrapping `unit.getAllIds`: the engine builds the real list
     from `HashMap.keys` (`Engine.Scripting.Lua.API.Units.List`) with no
     spawn-order contract, so spawn order would prove nothing.
  7. stale window — the same log check, with the hit staged far outside
     the window.
  8. stale window — no swap: the subject keeps its original target.
  9. stale window — and its tick still completes without raising.
     Asserting only 8 would pass on the pre-#1483 code, where the swap
     never happens because the tick died.

The two cases stage their hit at a fixed 0 s and 60 s, chosen to sit
unambiguously inside and outside the window WITHOUT restating its
duration. That is what lets them run — and fail — against code that does
not export the constant at all; only check 1 reads the value, as an
expectation of the module's one definition.

ONE FIXTURE PER WINDOW (#1578). Each window owns a WHOLE quartet —
subject, original target, recent attacker, sentinel — and stages its own
real hit immediately before it is graded. Sharing one subject made the
stale window's preconditions depend on what the fresh window's real
combat had already cost it, and a subject driven under the collapse
threshold made `check_preconditions` raise BEFORE either stale result was
recorded: a run that reported a fixture failure and every fresh check
passing, with the two declared stale results simply absent. Nothing could
undo that damage either — blood does not passively regenerate
(`scripts/unit_resource_tick.lua`), the engine exposes no verb to restore
it, and `unit.revive` only transitions a unit that is ALREADY collapsed.

HOW THE FIXTURE IS HELD STILL. The staged hit is real, so the staging leg
is real combat; everything after it is arranged so that combat cannot
disqualify the recent attacker through a DIFFERENT term of the branch's
`and` chain than the comparison under test:

* the staging leg's damage is bounded on BOTH axes, because neither
  bounds it alone. Per swing, `neuter` writes the one strength input
  nothing re-derives (`strength_base`), so a landed hit is a scratch
  rather than the kill or concussion a full-strength acolyte deals. Across
  swings, `patch_bounded_attack` stops the leg at the FIRST swing that
  lands: a miss simply swings again, but the swing that stamps
  `uiLastAttackerUid` (`Combat.Resolution`, the same write that appends
  the wounds) makes every later call a no-op. Unbounded, the leg swings
  for as long as the poll below runs — and none of it can be undone
  afterwards, because blood does not come back.
* for the graded section `combat.attack` is a plain no-op
  (`patch_no_damage`), which is installed only once each window's own real
  hit has been observed.
* `unit.getAllIds` is narrowed to the units a leg actually needs, which
  also holds every other fixture unit off both the AI tick and
  `unit_resources`' injury tick.
* `unit.getLastAttacker` reports the staged hit at a FIXED age for the
  SUBJECT ONLY, delegating for every other unit — the same
  wrap-and-delegate technique `tools/mental_state_probe.py` uses on this
  very API. Without it the hit ages out of the window between console
  round trips. It goes in only AFTER that window's real hit has been
  observed, so the wrap fixes the age of a genuine record rather than
  inventing one.
* Before each case every fixture unit has its bleeding dressed FROM A
  STOCKED KIT (an improvised tourniquet only halves a seep, however many
  times it is applied) and its survival needs topped up, is WATCHED back
  onto its feet rather than merely sent a revive (`wait_upright` — the
  verb only queues, and a knockdown gets up on its own clock), and the
  flanking geometry is re-established against the subject's current tile
  and VERIFIED.

FAILURE ACCOUNTING (#1578). Every check the probe declares is listed up
front (`DECLARED_CHECKS`), and the closing report names all of them — a
check the run never reached prints `NOT RUN` rather than vanishing, so a
suppressed result can never be mistaken for an absent one. `--self-test`
drives that accounting with no engine at all.

Usage: python3 tools/retaliation_swap_probe.py [--port 9483]
       python3 tools/retaliation_swap_probe.py --self-test
Exit 0 = pass, 1 = a check failed, 2 = the fixture was never established.
"""
from __future__ import annotations
import argparse
import contextlib
import glob
import io
import os
import sys
import time

from probelib import (boot, clear_find_water, init_arena, load_ai_stack,
                      poll_until, quit_engine, send, send_json, spawn_acolyte)

LOG = "/tmp/retaliation_swap_probe_engine.log"

# The engine's own log line for a callback that raised
# (Engine.Scripting.Lua.Script.callModuleFunctionReportingError).
UPDATE_ERROR_MARK = "Lua error in update()"

# How long each case lets NATURAL engine ticks run with its preconditions
# standing, for its log check. The unit AI re-decides at combat cadence
# (0.1 s), so this is many ticks — the pre-#1483 code logs ~20 per window.
NATURAL_TICK_WINDOW_SEC = 2.0

# How far either flanker is placed from the subject. Deliberately well
# INSIDE a full tile: an acolyte's `unit.getAttackRange` is under 1.0, so
# a flanker a whole tile away is out of range and the subject walks — and
# that walk is itself what drifts the recent attacker back out of
# `getAttackRange + 0.5` before the graded invocation reads the distance.
# At this offset the subject is already in range of what it is attacking,
# so it stands and swings and the geometry holds.
FLANK_OFFSET = 0.6

# The two staged hit ages, in seconds. Both are chosen to sit unambiguously
# INSIDE and OUTSIDE the window without restating its duration, so the
# behavioural cases grade the branch on code that does not export the
# constant at all — which is the pre-#1483 state they must fail on.
FRESH_AGE_SEC = 0.0
STALE_AGE_SEC = 60.0

# #1483's requirement 4: the duration itself stays three seconds. This is the
# probe's EXPECTATION of the module's one definition, asserted against the
# live export — not a second definition of the window, which lives (once)
# in `scripts/unit_ai_combat.lua`.
EXPECTED_WINDOW_SEC = 3.0

# The blood fraction a graded window's subject must still hold. This is
# `scripts/unit_resource_tick.lua`'s RISE threshold, the upper half of the
# product's own collapse hysteresis (collapse at 30 %, rise at 50 %): a
# subject below it is one that could not stand back up even after every
# repair this harness can apply, so grading it would measure blood loss
# rather than the retaliation branch.
MIN_BLOOD_FRACTION = 0.5

# What `neuter` writes into `strength_base`, from which the combat
# resolver's `strength` is derived (baseline 1.0). Both ends of this were
# measured, and both ends fail:
#
#   * too high and one swing kills or concusses the subject outright — a
#     full-strength acolyte did exactly that, and nothing afterwards can
#     undo it;
#   * too low and the swing never lands at all. At 0.005 a staging leg
#     went the whole 90 s poll without the resolver ever stamping a
#     recent attacker, and stripping the attacker's kit (softer still)
#     never landed one either.
#
# This value landed every staged hit measured, most of them leaving the
# subject at a full blood meter, and the few that did open a bleeding
# wound are dressable — which is only true because `provision_medical`
# supplies real bandages.
NEUTERED_STRENGTH_BASE = 0.02

# The runaway bound on `stanch`'s dressing loop — NOT its termination
# condition. It stops on the two real signals below: the subject's
# `bleedRate` settling, or treatments that stop lowering it.
# `unit.treatBleeding` reports `ok` for a dressing it APPLIED, never for a
# subject it FINISHED, so a fixed count never terminated anything — and
# the old bound of 12 was reached on every observed run, which is what
# left wounds seeping through the window that followed.
STANCH_ATTEMPTS = 64

# How many treatments in a row may fail to lower the bleed rate before
# `stanch` gives up. The improvised path ALWAYS reports success and always
# sets the same seep, so "it said ok" is not progress — this is what stops
# a fallback from spinning the loop to its bound while nothing improves.
STANCH_STALLED_PASSES = 3

# The bleed rate `stanch` treats as "no longer bleeding". A dressed wound
# settles to a residual seep of order 1e-4 L/s rather than exactly zero,
# which as a termination condition is no termination at all.
STANCH_SETTLED_RATE = 1e-3

# Stocked first-aid kits handed to every fixture unit. `unit.addItem`
# mints a container def's authored `contents:` (#1418), so each one
# arrives with real bandages — which is the difference between dressing a
# wound and improvising at it; see `stanch`. Enough of them that a whole
# wound distribution cannot run the supply out and drop the medic back
# onto the improvised path, which was measured leaving a subject seeping
# at 0.66 L/s after four hundred "successful" treatments.
MEDICAL_KITS = 8

# How long the staging leg waits for its real hit. Generous because it is
# waiting on the product's own hit/miss rolls, not on anything the harness
# controls: landing times of 5 s and 38 s were both observed from the same
# fixture, and a leg that times out costs the whole run.
STAGED_HIT_TIMEOUT_SEC = 90

# Poses a graded window cannot start from. Everything else — standing,
# crouching, climbing, sleeping — is a unit on its feet as far as the
# branch's `attPose ~= "collapsed"` term is concerned.
DOWN_POSES = ("collapsed", "crawling", "falling", "dead")

# Each fixture's quartet is laid out along one row, and the two rows are
# far enough apart that neither window's combat can reach into the other's
# — the id-list wrap holds the idle row off every tick, and this holds it
# out of reach even so.
ROW_SPACING = 12


class FixtureFailure(RuntimeError):
    """The scenario the probe needs was never established. Nothing
    downstream can be a behavioural result, so the run stops here."""


# Every check this probe declares, in report order. The run records each
# one by KEY, so the closing report can name the ones a fixture failure
# left unreached instead of silently omitting them (#1578).
DECLARED_CHECKS: tuple[tuple[str, str], ...] = (
    ("export",
     "the consumer's module can see one exported retaliation window of "
     f"{EXPECTED_WINDOW_SEC:g}s"),
    ("fresh:staged-hit",
     "fresh window: a real hit makes the third unit the recorded recent "
     "attacker"),
    ("fresh:log",
     "fresh window: no Lua error is logged across a window of natural ticks"),
    ("fresh:swap",
     "the retaliation swap retargets the subject onto the recent attacker"),
    ("fresh:complete",
     "the subject's tick reaches its post-execute completion point"),
    ("fresh:sentinel",
     "a unit ordered after the subject is reached by the SAME update "
     "invocation"),
    ("stale:staged-hit",
     "stale window: a real hit makes the third unit the recorded recent "
     "attacker"),
    ("stale:log",
     "stale window: no Lua error is logged across a window of natural ticks"),
    ("stale:no-swap",
     "a hit older than the window triggers no swap"),
    ("stale:complete",
     "the stale-window tick completes without raising"),
)


class Ledger:
    """Which of the declared checks this run reached, and what they said.

    The probe used to append results to a bare list, so a check the run
    never reached left no trace at all — and every stale result sat after
    a `check_preconditions` that could raise, which is exactly how a run
    came to report a fixture failure while looking like it had merely
    stopped a little early (#1578). Declaring the checks up front makes an
    unreached one reportable.
    """

    def __init__(self, declared: tuple[tuple[str, str], ...] = DECLARED_CHECKS):
        self._names = dict(declared)
        self._order = [key for key, _ in declared]
        self._results: dict[str, tuple[bool, str]] = {}

    def record(self, key: str, passed: bool, detail: str = "") -> None:
        if key not in self._names:
            raise KeyError(f"{key!r} is not a declared check")
        if key in self._results:
            raise KeyError(f"{key!r} was already recorded")
        self._results[key] = (bool(passed), detail)
        print(f"  [{'PASS' if passed else 'FAIL'}] {self._names[key]}"
              + (f" — {detail}" if detail else ""))

    def order(self) -> list[str]:
        return list(self._order)

    def name(self, key: str) -> str:
        return self._names[key]

    def outcome(self, key: str) -> str:
        if key not in self._results:
            return "NOT RUN"
        return "PASS" if self._results[key][0] else "FAIL"

    def unrun(self) -> list[str]:
        return [k for k in self._order if k not in self._results]

    def failed(self) -> list[str]:
        return [k for k in self._order
                if k in self._results and not self._results[k][0]]


def outcome_line(outcome: str, name: str) -> str:
    """One report row. Shared with `--self-test`, so the accounting it
    reads back is the exact text a real run prints."""
    return f"{outcome:<7}  {name}"


def finish(ledger: Ledger, failure: FixtureFailure | None = None) -> int:
    """Print every declared check's outcome and return the exit status.

    The one place a run's verdict is decided, so a fixture failure and a
    clean finish report the same ledger in the same shape — a check that
    never ran is named `NOT RUN`, never dropped.
    """
    print("\n--- result ---")
    for key in ledger.order():
        print("  " + outcome_line(ledger.outcome(key), ledger.name(key)))
    unrun, failed = ledger.unrun(), ledger.failed()
    total = len(ledger.order())
    if unrun:
        print(f"\n{len(unrun)} of {total} declared checks never ran:")
        for key in unrun:
            print("  " + outcome_line("NOT RUN", ledger.name(key)))
    if failure is not None:
        print(f"\nFIXTURE FAILURE: {failure}")
        return 2
    if unrun:
        # No fixture failure and a check still missing is a defect in the
        # probe itself, not a result — report it as a fixture failure
        # rather than letting the run read as a pass.
        print("\nthe run ended without a fixture failure yet left declared "
              "checks unreached")
        return 2
    if failed:
        print(f"\n{len(failed)} of {total} checks failed (#1483)")
        return 1
    print(f"\nall {total} checks passed (#1483)")
    return 0


class Fixture:
    """One window's own four units, and where its row sits."""

    def __init__(self, label: str, row: int):
        self.label = label
        self.row = row
        self.subject = 0
        self.attacker = 0
        self.target = 0
        self.sentinel = 0

    def spawn(self, port: int) -> None:
        y = self.row
        self.subject = spawn_acolyte(port, 0, y)
        self.attacker = spawn_acolyte(port, 1, y)
        self.target = spawn_acolyte(port, -1, y)
        self.sentinel = spawn_acolyte(port, -8, y)
        print(f"{self.label}: subject={self.subject} attacker={self.attacker} "
              f"target={self.target} sentinel={self.sentinel}")

    def units(self) -> tuple[int, int, int, int]:
        return (self.subject, self.attacker, self.target, self.sentinel)


def lua(*statements: str) -> str:
    """Join Lua statements into ONE console line.

    The debug console is single-line only, so the chunk carries no `--`
    comments (one would swallow the rest of the program).
    """
    return " ".join(s.strip() for s in statements)


def bootstrap(port: int) -> None:
    for pattern, fn in [("data/substances/*.yaml", "engine.loadSubstanceYaml"),
                        ("data/items/*.yaml", "engine.loadItemYaml"),
                        ("data/equipment/*.yaml", "engine.loadEquipmentYaml"),
                        ("data/materials/*.yaml", "engine.loadMaterialYaml"),
                        ("data/units/*.yaml", "engine.loadUnitYaml")]:
        for path in sorted(glob.glob(pattern)):
            send(port, f"{fn}('{path}'); return 'ok'")
    load_ai_stack(port)
    init_arena(port)
    send(port, "engine.setPaused(false); return 'ok'")


def sustain(port: int, uid: int) -> None:
    """Top one unit's survival needs up to their OWN maxima.

    The survival ladder scales PAST the 8.0 combat band, so a thirsty or
    starving subject would correctly stop fighting and this probe would
    measure that instead of the retaliation branch. `sleep_pressure` is a
    RESOURCE like the other three — a full meter is the un-sleepy one.
    """
    send(port, lua(
        f"local u = {uid};",
        "for _, pair in ipairs({ {'hunger','max_hunger'},",
        " {'hydration','max_hydration'}, {'calories','max_calories'} }) do",
        " local m = unit.getStat(u, pair[2]);",
        " if m then unit.setStat(u, pair[1], m) end end;",
        "local us = require('scripts.unit_stats');",
        "local msp = us.get(u, 'max_sleep_pressure');",
        "if msp then unit.setStat(u, 'sleep_pressure', msp) end;",
        "return 'ok'"))


def calm(port: int, uid: int) -> None:
    """Retire every standing drive that could outscore attack_target."""
    clear_find_water(port, uid)
    # treat_ally also scores 8.0 and its patient scan reaches 60 tiles;
    # the staged hit leaves a bleeding ally in range of exactly that scan.
    # attack_target wins the tie on list order, but taking the medic drive
    # out removes the coincidence from the fixture.
    send(port, lua(
        f"unit.setKnowledge({uid}, 'bleed_control', 0);",
        f"local s = require('scripts.unit_ai').getState({uid});",
        "if s then s.treatClaim = nil; s.treatPending = nil end;",
        "return 'ok'"))
    sustain(port, uid)


def neuter(port: int, uid: int) -> None:
    """Soften this unit's swings, and harden it against the ones it takes.

    A full-strength acolyte can one-shot another, and a kill or a collapse
    disqualifies the recent attacker through a DIFFERENT branch term than
    the one under test — observed live as a subject read `dead` with 0 %
    blood, and as one read `collapsed` at 73 % blood from the concussion a
    single full-strength swing left (#1578).

    `strength` is the stat the combat resolver reads
    (`Combat.Resolution.Damage`), but writing it does nothing that lasts:
    `Unit.Thread.Command.Body.recomputeBodyDerivedStats` re-derives it from
    body composition on every composition change, and every physiology pass
    then calls `starvation.refreshStrength`, which re-derives it AGAIN from
    the `strength_body` mirror that recompute wrote. Both derive from
    `strength_base`, which is the one input NEITHER of them overwrites —
    so that is what this writes, and `unit.recomputeBody` is what makes the
    new base take effect now rather than whenever body mass next drifts.

    Deliberately small rather than zero: a swing that cannot land would
    never stamp `uiLastAttackerUid`, and the staged hit has to be real.
    `toughness` is the other half and caps out early — the resolver clamps
    its contribution to a 50 % cut — so it hardens the subject a little but
    could never bound the hit on its own.
    """
    send(port, lua(f"unit.setStat({uid},'strength_base',{NEUTERED_STRENGTH_BASE});",
                   f"unit.setStat({uid},'toughness',100);",
                   f"unit.recomputeBody({uid});",
                   "return 'ok'"))


def stanch(port: int, uid: int) -> dict:
    """Dress every bleeding wound the staged hit left on `uid`.

    The staging leg is REAL combat, and a real wound keeps seeping long
    after it lands. Left alone the subject collapses from blood loss
    partway through the graded windows — observed live, and only on some
    runs — which disqualifies it through `tickOne`'s pose short-circuit
    rather than through anything this probe is testing.

    Self-treatment, so no second unit is involved: `unit.treatBleeding`
    needs only `bleed_control` knowledge on the medic
    (`Engine.Scripting.Lua.API.Units.Medical`). The knowledge goes back to
    zero afterwards, because `calm` deliberately took this unit out of the
    medic squad — `treat_ally` scores 8.0 and would tie with the action
    under test.

    It DOES need supplies, though, and that is not a detail (#1578). With
    an empty inventory the medic improvises a tourniquet, and that path is
    explicitly only "somewhat" effective: it clamps the dressed seep to
    0.4–0.58 and always reports success, so re-treating the same wound
    hundreds of times changes nothing — measured live, four hundred
    tourniquets left a torso wound seeping at 0.14 L/s and the subject
    bled out through the window that followed. Drawing from the kit
    `provision_medical` hands out instead dresses it in ONE attempt, to a
    seep of order 1e-4.

    NB this stops the seeping; it cannot put blood back. Blood does not
    passively regenerate and no verb sets it, which is why the staging
    leg's damage has to be bounded BEFORE it is inflicted.
    """
    got = send_json(port, lua(
        f"local u = {uid};",
        "unit.setKnowledge(u, 'bleed_control', 100);",
        "local n = 0; local stalled = 0;",
        "local function rate() local b = unit.getBlood(u);",
        " return b and b.bleedRate or 0 end;",
        "local last = rate();",
        f"for _ = 1, {STANCH_ATTEMPTS} do",
        f" if rate() <= {STANCH_SETTLED_RATE} then break end;",
        " local r = unit.treatBleeding(u, u);",
        " if not r or not r.ok then break end;",
        " n = n + 1;",
        " local now = rate();",
        " if now < last - 1e-6 then stalled = 0 else stalled = stalled + 1 end;",
        " last = now;",
        f" if stalled >= {STANCH_STALLED_PASSES} then break end end;",
        "unit.setKnowledge(u, 'bleed_control', 0);",
        "return { dressed = n, bleedRate = rate() }"),
        timeout=30.0)
    if not isinstance(got, dict):
        raise FixtureFailure(f"dressing {uid}'s wounds returned {got!r}")
    return got


def provision_medical(port: int, uid: int) -> None:
    """Give `uid` its own stocked first-aid kits.

    `stanch` treats the unit as its own medic AND its own kit owner, so
    the supplies have to sit in this unit's own inventory. Handed out at
    fixture setup rather than at treatment time, because the wound being
    dressed is already seeping by then and every console round trip costs
    blood that never comes back.
    """
    for _ in range(MEDICAL_KITS):
        send(port, f"unit.addItem({uid}, 'first_aid_kit'); return 'ok'")


def restore(port: int, uid: int, label: str) -> None:
    """Undo what real combat cost this unit, as far as anything can, and
    do not return until it is actually back on its feet.

    Applied before the graded window rather than only at spawn, so the
    window starts from a unit that is standing, dressed and fed.
    """
    stanch(port, uid)
    sustain(port, uid)
    wait_upright(port, uid, label)


def wait_upright(port: int, uid: int, label: str,
                 timeout: float = 25.0) -> str:
    """Poll until `uid` is on its feet, asking it to get up as it goes.

    Firing `unit.revive` once and reading the pose on the next round trip
    grades whatever the unit happened to be doing: the verb only QUEUES a
    `UnitRevive`, and it is a plain no-op unless the unit is ALREADY
    Collapsed (`Engine.Scripting.Lua.API.Units.Spawn`). A knockdown has
    its own self-timed getup clock on top of that
    (`Unit.Thread.Movement.Timers` — the physics injury model turns every
    landing into one), so the only reliable way to start a window from a
    standing subject is to watch it stand.

    Failing here is honest and load-bearing: a unit that cannot rise
    inside the timeout is one the product's own rules are holding down —
    below the blood rise threshold, concussed, or dead — and grading it
    would measure that instead of the retaliation branch.
    """
    deadline = time.time() + timeout
    pose = ""
    while time.time() < deadline:
        pose = send(port, f"return unit.getPose({uid})")
        if pose not in DOWN_POSES:
            return pose
        if pose == "dead":
            break
        send(port, f"unit.revive({uid}); return 'ok'")
        time.sleep(0.25)
    frac = blood_fraction(port, uid)
    raise FixtureFailure(
        f"{label}: unit {uid} never came back to its feet within "
        f"{timeout:g}s (pose {pose!r}, blood "
        + ("unreadable" if frac is None else f"{100.0 * frac:.1f}% of max")
        + ")")


def blood_fraction(port: int, uid: int) -> float | None:
    """`uid`'s blood as a fraction of its own maximum, or `None`."""
    got = send_json(port, f"local b = unit.getBlood({uid}); "
                          "if not b then return { ok = 0 } end; "
                          "return { ok = 1, current = b.current, max = b.max }")
    if not isinstance(got, dict) or got.get("ok") != 1:
        return None
    mx = got.get("max", 0)
    if not mx or mx <= 0:
        return None
    return got.get("current", 0) / mx


def window_seconds(port: int) -> float | None:
    """`RETALIATE_WINDOW_SEC` as the live module exports it, or `None`.

    Deliberately NOT a fixture gate. A missing export is exactly the
    pre-#1483 state, and stopping here would leave the behavioural cases
    below unrun against the very code they exist to fail on — the probe
    would only ever prove that an export is absent, never that the branch
    raises. So it is graded as its own check and the scenario continues.
    """
    raw = send(port, "return require('scripts.unit_ai_combat')"
                     ".RETALIATE_WINDOW_SEC")
    try:
        return float(raw)
    except (TypeError, ValueError):
        return None


def pin_next_to(port: int, uid: int, anchor: int, dx: float = 1.0) -> None:
    """Teleport `uid` to `dx` tiles from `anchor` and CONFIRM it landed.

    `unit.setPos` is applied by the unit thread, so a probe that fires it
    and reads immediately can grade against the old position. Knockback
    from the staged hits is exactly what makes this necessary.
    """
    pos = send_json(port, f"local i = unit.getInfo({anchor}); "
                          "return {x = i.gridX, y = i.gridY}")
    if not isinstance(pos, dict):
        raise FixtureFailure(f"unit.getInfo({anchor}) returned {pos!r}")
    tx, ty = pos["x"] + dx, pos["y"]
    send(port, f"unit.stop({uid}); unit.setPos({uid}, {tx}, {ty}); return 'ok'")

    def landed():
        p = send_json(port, f"local i = unit.getInfo({uid}); "
                            "return {x = i.gridX, y = i.gridY}")
        if not isinstance(p, dict):
            return None
        return abs(p["x"] - tx) < 0.25 and abs(p["y"] - ty) < 0.25

    if not poll_until(6, landed):
        raise FixtureFailure(
            f"teleporting {uid} to ({tx}, {ty}) beside {anchor} never took "
            "effect, so the melee-reach precondition cannot be established")


def patch_ids(port: int, ids: list[int]) -> None:
    """Make `unit.getAllIds` return exactly `ids`, in that order.

    Two jobs. It is what puts the sentinel AFTER the subject — the engine
    builds the real list from `HashMap.keys`
    (`Engine.Scripting.Lua.API.Units.List`) with no spawn-order contract,
    so nothing else could establish that ordering. And every unit left OUT
    is off both the AI tick and `unit_resources`' injury tick, so no
    fixture unit walks out of reach, bleeds out, or fights back while a
    window is being staged or graded.
    """
    listing = ", ".join(str(i) for i in ids)
    send(port, lua(
        "if not _G.__ret_ids then _G.__ret_ids = unit.getAllIds end;",
        f"unit.getAllIds = function() return {{ {listing} }} end;",
        "return 'ok'"))


def patch_last_attacker(port: int, subject: int, attacker: int,
                        age: float) -> None:
    """Report the (real, already-observed) hit at a FIXED age, for the
    SUBJECT only — delegating for every other unit.

    The engine exposes no setter for `uiLastAttackerAt`, so without this
    the staged hit ages out of the window between console round trips and
    the case decays into a short-circuiting fixture failure rather than a
    result. `tools/mental_state_probe.py` wraps this same API the same way.

    Installed only AFTER this window's own real hit has been observed, so
    what it fixes is the age of a genuine record — never a substitute for
    one.
    """
    send(port, lua(
        "if not _G.__ret_gla then _G.__ret_gla = unit.getLastAttacker end;",
        "unit.getLastAttacker = function(u)",
        f" if u == {subject} then",
        f"  return {{ uid = {attacker}, at = engine.gameTime() - {age} }} end;",
        " return _G.__ret_gla(u) end;",
        "return 'ok'"))


def patch_bounded_attack(port: int, subject: int, attacker: int) -> None:
    """Let the staging leg swing until ONE swing WOUNDS, then stop.

    `Combat.Resolution` stamps `uiLastAttackerUid` in the very same write
    that appends the wounds, so "the subject's recorded recent attacker is
    now `attacker`" IS "a swing has landed and wounded". Swinging past
    that point buys the fixture nothing and costs it everything: the
    staging poll below runs for up to 45 s, every landed swing opens more
    bleeding wounds, and blood never comes back — a subject driven under
    the 30 % collapse threshold can no longer be stood back up, which
    disqualifies it through `tickOne`'s pose short-circuit rather than
    through anything under test (#1578).

    A miss simply swings again, so this bounds the DAMAGE without making
    the staged hit any less real: the hit that lands is a genuine one,
    resolved by the product's own combat path against the product's own
    injury rules. Nothing here patches or bypasses those rules — the
    harness stops asking for more swings, and that is all. `neuter` bounds
    the other axis, what a single swing costs; this one bounds how many
    there are.
    """
    send(port, lua(
        "if not _G.__ret_atk then _G.__ret_atk = combat.attack end;",
        f"local subject, attacker = {subject}, {attacker};",
        "combat.attack = function(...)",
        " local raw = _G.__ret_gla or unit.getLastAttacker;",
        " local rec = raw(subject);",
        " if rec and rec.uid == attacker then return false end;",
        " return _G.__ret_atk(...) end;",
        "return 'ok'"))


def patch_no_damage(port: int) -> None:
    """Make swings land no damage for the graded section.

    The staged hit is REAL and already recorded by the time this goes in;
    from here `unit.getLastAttacker` is wrapped anyway, so no further
    damage is needed by anything under test. What further damage DOES do
    is kill the recent attacker and knock it out of melee reach across a
    couple of seconds of ticks — both of which disqualify it through a
    DIFFERENT term of the branch's `and` chain than the comparison this
    probe exists to reach, turning every later case into a fixture
    failure.

    The branch under test runs BEFORE any swing in `attackTargetExecute`,
    so suppressing the swing's effect changes nothing it reads.
    """
    send(port, lua(
        "if not _G.__ret_atk then _G.__ret_atk = combat.attack end;",
        "combat.attack = function() end;",
        "return 'ok'"))


def remove_patches(port: int) -> None:
    send(port, lua(
        "if _G.__ret_gla then unit.getLastAttacker = _G.__ret_gla;",
        " _G.__ret_gla = nil end;",
        "if _G.__ret_ids then unit.getAllIds = _G.__ret_ids;",
        " _G.__ret_ids = nil end;",
        "if _G.__ret_atk then combat.attack = _G.__ret_atk;",
        " _G.__ret_atk = nil end;",
        "return 'ok'"))


def graded_update(port: int, subject: int, target: int, attacker: int,
                  sentinel: int) -> dict:
    """One atomic chunk: re-establish, capture preconditions, tick, report.

    Everything from `commandAttack` to the post-state read happens inside a
    single Lua chunk, so no engine-driven tick can interleave between the
    precondition capture and the invocation being graded. The subject's
    BLOOD is captured in that same chunk for the same reason: read
    separately it could drift across a console round trip and describe a
    subject other than the one the tick below graded.

    `commandAttack` is what puts the subject back on the ORIGINAL target
    (and zeroes its `nextActionAt`), so a swap observed afterwards can only
    have happened during this invocation.
    """
    got = send_json(port, lua(
        "local ai = require('scripts.unit_ai');",
        f"local s = ai.getState({subject});",
        f"local ss = ai.getState({sentinel});",
        "if not s or not ss then return { fixture = 'missing-ai-state' } end;",
        f"ai.commandAttack({subject}, {target});",
        "ss.nextActionAt = 0;",
        f"local me = unit.getInfo({subject});",
        f"local at = unit.getInfo({attacker});",
        "if not me or not at then return { fixture = 'missing-unit-info' } end;",
        f"local rec = unit.getLastAttacker({subject});",
        f"local bl = unit.getBlood({subject});",
        "local pre = { target = s.attackTargetUid,",
        " recent = rec and rec.uid or -1,",
        " age = rec and (engine.gameTime() - (rec.at or 0)) or -1,",
        " dist = math.max(math.abs(me.gridX - at.gridX),",
        "                 math.abs(me.gridY - at.gridY)),",
        f" reach = (unit.getAttackRange({subject}) or 1.0) + 0.5,",
        f" subjectPose = unit.getPose({subject}),",
        f" attackerPose = unit.getPose({attacker}),",
        " bloodCurrent = bl and bl.current or -1,",
        " bloodMax = bl and bl.max or -1,",
        " attackerDef = at.defName,",
        f" attackerExists = unit.exists({attacker}) and 1 or 0,",
        " goal = s.activeGoal };",
        "local t0 = engine.gameTime();",
        "local ok, err = pcall(ai.update, 0.1);",
        "return { fixture = 'ok', pre = pre, t0 = t0,",
        " ok = ok and 1 or 0, err = ok and '' or tostring(err),",
        " postTarget = s.attackTargetUid or -1,",
        " subjectNext = s.nextActionAt, sentinelNext = ss.nextActionAt }"),
        timeout=20.0)
    if not isinstance(got, dict):
        raise FixtureFailure(f"the graded chunk returned {got!r}, not a table")
    if got.get("fixture") != "ok":
        raise FixtureFailure(f"the graded chunk reported {got.get('fixture')!r}")
    return got


def flank_distances(port: int, subject: int, target: int,
                    attacker: int) -> dict:
    """Live Chebyshev distances from the subject to both flankers, with
    the subject's own reach for comparison."""
    got = send_json(port, lua(
        f"local m = unit.getInfo({subject});",
        f"local a = unit.getInfo({attacker});",
        f"local g = unit.getInfo({target});",
        "if not m or not a or not g then return { ok = 0 } end;",
        "local function cheb(p, q)",
        " return math.max(math.abs(p.gridX - q.gridX),",
        "                 math.abs(p.gridY - q.gridY)) end;",
        "return { ok = 1, attacker = cheb(m, a), target = cheb(m, g),",
        f" range = unit.getAttackRange({subject}) or 1.0 }}"))
    return got if isinstance(got, dict) else {"ok": 0}


def stage_geometry(port: int, subject: int, target: int,
                   attacker: int) -> None:
    """Flank the (stopped) subject: original target west, recent attacker
    east, both inside its melee reach.

    Each pin is taken against the subject's CURRENT tile, so this is
    equally the way to undo a previous window's drift. It is VERIFIED and
    retried rather than fired once: the subject is under the live AI the
    whole time, so a tick landing between the two pins can start it
    walking and leave the second one measured against a tile it has
    already left.
    """
    for _ in range(6):
        send(port, f"unit.stop({subject}); return 'ok'")
        pin_next_to(port, target, subject, dx=-FLANK_OFFSET)
        pin_next_to(port, attacker, subject, dx=FLANK_OFFSET)
        d = flank_distances(port, subject, target, attacker)
        if (d.get("ok") == 1
                and d["attacker"] <= d["range"] + 0.5
                and d["target"] <= d["range"]):
            return
    raise FixtureFailure(
        "could not settle the subject between its target and its recent "
        f"attacker inside melee reach (last read {d})")


def stage_hit(port, ledger: Ledger, key: str, fx: Fixture) -> None:
    """Land this window's OWN real hit, bounded to a single wounding swing.

    Only the ATTACKER is AI-ticked for this leg: a subject that fought
    back would wound the attacker, and the resulting bleed-out disqualifies
    it later through a DIFFERENT branch term than the one under test.
    """
    patch_ids(port, [fx.attacker])
    patch_bounded_attack(port, fx.subject, fx.attacker)
    send(port, f"require('scripts.unit_ai').commandAttack({fx.attacker}, "
               f"{fx.subject}); return 'ok'")
    landed = poll_until(STAGED_HIT_TIMEOUT_SEC, lambda: send(
        port, lua("local raw = _G.__ret_gla or unit.getLastAttacker;",
                  f"local a = raw({fx.subject});",
                  "return a and a.uid or 'nil'")) == str(fx.attacker))
    ledger.record(key, bool(landed),
                  f"unit.getLastAttacker({fx.subject}).uid == {fx.attacker}"
                  if landed else "no hit ever landed")
    if not landed:
        raise FixtureFailure(
            f"{fx.attacker} never landed a hit on {fx.subject}; the branch's "
            "recent-attacker precondition cannot be established")

    # Retire the staging order. The recorded hit stays; only the standing
    # orders go, and dressing both units' wounds immediately is what keeps
    # the single staged hit from seeping the subject's blood away while the
    # window is set up.
    send(port, lua(
        "local ai = require('scripts.unit_ai');",
        f"local s = ai.getState({fx.attacker});",
        "if s then ai.markGoalAccomplished(s, 'attack');",
        " s.attackTargetUid = nil end;",
        f"unit.revive({fx.attacker}); unit.stop({fx.attacker});",
        f"unit.revive({fx.subject}); unit.stop({fx.subject});",
        "return 'ok'"))
    for who, uid in (("subject", fx.subject), ("attacker", fx.attacker)):
        dressed = stanch(port, uid)
        print(f"  {who} {uid} dressed {dressed.get('dressed')} wound(s), "
              f"bleed rate now {dressed.get('bleedRate')}")
    cost = staging_cost(port, fx)
    print(f"  staged hit cost: attacker strength "
          f"{cost.get('attackerStrength', float('nan')):.3f}, subject left "
          f"{100.0 * cost.get('subjectBlood', 0.0):.1f}% blood with "
          f"{cost.get('subjectWounds')} wound(s), pose "
          f"{cost.get('subjectPose')!r}")
    for uid in (fx.subject, fx.attacker):
        wait_upright(port, uid, f"{fx.label} staging")


def staging_cost(port: int, fx: Fixture) -> dict:
    """What the staged hit actually cost, and what the swing that dealt it
    was worth.

    Reported rather than merely bounded: `strength` is the input the
    resolver scales its swing energy by, and the subject's blood and wound
    count are the outputs — printing all three is what makes a run's own
    log say whether the bound held, instead of leaving it to be inferred
    from whichever precondition failed afterwards.
    """
    got = send_json(port, lua(
        f"local b = unit.getBlood({fx.subject});",
        f"return {{ attackerStrength = unit.getStat({fx.attacker}, 'strength'),",
        f" subjectBlood = b and (b.current / b.max) or -1,",
        f" subjectWounds = #(unit.getWounds({fx.subject}) or {{}}),",
        f" subjectPose = unit.getPose({fx.subject}) }}"))
    if not isinstance(got, dict) or "subjectBlood" not in got:
        raise FixtureFailure(
            f"{fx.label}: reading what the staged hit cost {fx.subject} "
            f"returned {got!r}")
    return got


def run_case(port, ledger: Ledger, log_key: str, fx: Fixture,
             age: float) -> dict:
    """Stage one window, let NATURAL ticks run through it, then grade.

    The natural-tick leg is what produces the log evidence: the subject is
    put back on the original target and the branch's preconditions stand
    for the whole of it, so the engine's own `unitAi.update` callback
    reaches the comparison many times over. On the pre-#1483 code every
    one of those ticks raises and
    `Engine.Scripting.Lua.Script.callModuleFunctionReportingError` logs it.
    """
    print(f"\n--- {fx.label} (staged hit age {age:g}s) ---")
    patch_last_attacker(port, fx.subject, fx.attacker, age)
    patch_ids(port, [fx.subject, fx.sentinel])
    patch_no_damage(port)
    for uid in (fx.subject, fx.attacker, fx.target):
        restore(port, uid, f"{fx.label} setup")
    stage_geometry(port, fx.subject, fx.target, fx.attacker)
    send(port, f"require('scripts.unit_ai').commandAttack({fx.subject}, "
               f"{fx.target}); return 'ok'")

    mark = log_size()
    time.sleep(NATURAL_TICK_WINDOW_SEC)
    errors = log_errors_since(mark)

    # Stand both units back up BEFORE re-pinning, so the geometry is
    # settled against a subject that is already on its feet. A window of
    # live ticks can put one down for reasons this fixture does not
    # control, and a pose is a precondition the graded chunk reads — so
    # the fixture waits it out here rather than grading a unit that was
    # about to stand anyway. The chunk still captures the pose itself, so
    # nothing here can pass a subject that stayed down.
    for uid in (fx.subject, fx.attacker):
        wait_upright(port, uid, f"{fx.label} pre-grade")

    # Re-pin before grading. The window that just ran is real combat: the
    # subject closes on whatever it is targeting and its swings knock the
    # other two around, so the flanking geometry has to be re-established
    # against the subject's CURRENT tile — the branch reads the distance
    # as it stands when the graded invocation starts.
    stage_geometry(port, fx.subject, fx.target, fx.attacker)
    graded = graded_update(port, fx.subject, fx.target, fx.attacker,
                           fx.sentinel)
    # Reported BEFORE the gate, so the observation is on the record even on
    # the run where it is the thing that fails.
    print(f"  {fx.label} subject observed: pose "
          f"{graded['pre'].get('subjectPose')!r}, blood "
          f"{describe_blood(graded['pre'])}")
    check_preconditions(graded["pre"], age, f"{fx.label} case")
    print(f"  preconditions: {graded['pre']}")
    ledger.record(log_key, not errors,
                  f"{len(errors)} entr{'y' if len(errors) == 1 else 'ies'}"
                  + (f": {errors[0]}" if errors else ""))
    return graded


def describe_blood(pre: dict) -> str:
    """The captured blood reading as a percentage of the subject's own
    maximum, or why there is no fraction to state."""
    cur, mx = pre.get("bloodCurrent", -1), pre.get("bloodMax", -1)
    if mx is None or mx <= 0:
        return f"unreadable (current={cur!r}, max={mx!r})"
    return f"{100.0 * cur / mx:.1f}% of max ({cur:.2f}/{mx:.2f})"


def check_preconditions(pre: dict, staged_age: float, label: str) -> None:
    """Every term the branch's `and` chain reads, verified as CAPTURED.

    A case that fails here never reached the comparison under test, so it
    is a fixture failure rather than a passing behavioural result — which
    is exactly how the existing collapsed-attacker case passes today.
    """
    problems = []
    if pre.get("recent") == pre.get("target"):
        problems.append("the recent attacker IS the current target")
    if pre.get("attackerExists") != 1:
        problems.append("the recent attacker does not exist")
    for who in ("subjectPose", "attackerPose"):
        if pre.get(who) in ("dead", "collapsed"):
            problems.append(f"{who} is {pre.get(who)!r}")
    # The subject must be one that could stand back up, not merely one that
    # happens to be standing this instant: below the rise threshold the
    # next wound tick puts it down for good and nothing this harness can do
    # brings it back (#1578).
    blood_max = pre.get("bloodMax", -1)
    if blood_max is None or blood_max <= 0:
        problems.append(f"the subject's blood is unreadable ({describe_blood(pre)})")
    elif pre.get("bloodCurrent", -1) / blood_max <= MIN_BLOOD_FRACTION:
        problems.append(
            f"the subject's blood is {describe_blood(pre)}, at or below the "
            f"{100.0 * MIN_BLOOD_FRACTION:.0f}% rise threshold")
    if pre.get("attackerDef") == "technomule":
        problems.append("the recent attacker is a technomule")
    if pre.get("dist", 1e9) > pre.get("reach", 0):
        problems.append(f"distance {pre.get('dist')} exceeds reach "
                        f"{pre.get('reach')}")
    if pre.get("goal") != "attack":
        problems.append(f"the subject's goal is {pre.get('goal')!r}, not 'attack'")
    # The wrap holds the hit at exactly the staged age, so a drift here
    # means the wrap was not in force and the case is not the one intended.
    age = pre.get("age", -1)
    if abs(age - staged_age) > 0.5:
        problems.append(f"the staged hit reads as {age}s old, not the "
                        f"{staged_age}s this case staged")
    if problems:
        raise FixtureFailure(f"{label}: " + "; ".join(problems))


def log_errors_since(offset: int) -> list[str]:
    """`Lua error in update()` lines written to the engine log since
    `offset`."""
    try:
        with open(LOG, errors="replace") as fh:
            fh.seek(offset)
            tail = fh.read()
    except OSError:
        return []
    return [ln.strip() for ln in tail.splitlines() if UPDATE_ERROR_MARK in ln]


def log_size() -> int:
    try:
        return os.path.getsize(LOG)
    except OSError:
        return 0


def main() -> int:
    ap = argparse.ArgumentParser(
        description=__doc__,
        formatter_class=argparse.RawDescriptionHelpFormatter)
    ap.add_argument("--port", type=int, default=9483)
    ap.add_argument("--self-test", action="store_true",
                    help="check the declared-check accounting with no engine")
    args = ap.parse_args()
    if args.self_test:
        return self_test()
    port = args.port

    proc = boot(port, log=LOG)
    ledger = Ledger()

    try:
        try:
            bootstrap(port)
            window = window_seconds(port)
            ledger.record(
                "export", window == EXPECTED_WINDOW_SEC,
                "scripts.unit_ai_combat.RETALIATE_WINDOW_SEC = "
                + ("absent — the attack module reads an unassigned global"
                   if window is None else f"{window:g}"))

            # ONE fixture per window. `target` and `attacker` flank the
            # subject, both inside melee reach: with the original target
            # already in range the subject stands and swings instead of
            # walking off, so the attacker stays within
            # `getAttackRange + 0.5` across a whole window of natural ticks
            # without being re-pinned mid-flight.
            fresh_fx = Fixture("fresh window", 0)
            stale_fx = Fixture("stale window", ROW_SPACING)
            for fx in (fresh_fx, stale_fx):
                fx.spawn(port)
                for uid in fx.units():
                    neuter(port, uid)
                    calm(port, uid)
                    provision_medical(port, uid)

            try:
                grade_windows(port, ledger, fresh_fx, stale_fx)
            finally:
                remove_patches(port)
        except FixtureFailure as exc:
            return finish(ledger, exc)
        return finish(ledger)
    finally:
        quit_engine(port, proc)


def grade_windows(port, ledger: Ledger, fresh_fx: Fixture,
                  stale_fx: Fixture) -> None:
    """Stage and grade both windows, each from its own untouched fixture.

    Split out of `main` only so the wraps each leg installs are released
    by ONE `finally` covering every path out of them, including a fixture
    failure raised mid-staging.
    """
    stage_hit(port, ledger, "fresh:staged-hit", fresh_fx)
    fresh = run_case(port, ledger, "fresh:log", fresh_fx, FRESH_AGE_SEC)
    ledger.record("fresh:swap", fresh["postTarget"] == fresh_fx.attacker,
                  f"attackTargetUid {fresh['pre']['target']} -> "
                  f"{fresh['postTarget']} (wanted {fresh_fx.attacker})")
    ledger.record("fresh:complete",
                  fresh["ok"] == 1 and fresh["subjectNext"] > fresh["t0"],
                  f"nextActionAt {fresh['subjectNext']} vs t0 {fresh['t0']}"
                  + (f"; update raised {fresh['err']}"
                     if fresh["ok"] != 1 else ""))
    ledger.record("fresh:sentinel", fresh["sentinelNext"] > fresh["t0"],
                  f"sentinel nextActionAt {fresh['sentinelNext']} vs t0 "
                  f"{fresh['t0']} (0 = never ticked)")

    # The stale window's own real hit, on its own subject. `patch_no_damage`
    # is still in force from the fresh window, so the bounded staging wrap
    # `stage_hit` installs is what puts real swings back — over the SAME
    # saved original, so `remove_patches` still restores it exactly.
    stage_hit(port, ledger, "stale:staged-hit", stale_fx)
    stale = run_case(port, ledger, "stale:log", stale_fx, STALE_AGE_SEC)
    ledger.record("stale:no-swap", stale["postTarget"] == stale_fx.target,
                  f"attackTargetUid stayed {stale['postTarget']} "
                  f"(wanted {stale_fx.target}, not {stale_fx.attacker})")
    ledger.record("stale:complete",
                  stale["ok"] == 1 and stale["subjectNext"] > stale["t0"],
                  f"pcall ok={stale['ok']}, nextActionAt "
                  f"{stale['subjectNext']} vs t0 {stale['t0']}"
                  + (f"; update raised {stale['err']}"
                     if stale["ok"] != 1 else ""))


def self_test() -> int:
    """Drive the declared-check accounting with no engine at all.

    Five green engine runs cannot exercise the path this covers: it is
    what a run does when the fixture FAILS, which is precisely the run
    that used to drop its unreached results on the floor (#1578). So the
    failure is constructed here instead, after a check has already been
    recorded, and the report is read back.
    """
    problems: list[str] = []

    def report(ledger: Ledger, failure: FixtureFailure | None) -> tuple[int, str]:
        buf = io.StringIO()
        with contextlib.redirect_stdout(buf):
            status = finish(ledger, failure)
        return status, buf.getvalue()

    # (a) a fixture failure after one recorded check still names every
    #     declared check that never ran, and keeps exit status 2.
    ledger = Ledger()
    first_key, first_name = DECLARED_CHECKS[0]
    with contextlib.redirect_stdout(io.StringIO()):
        ledger.record(first_key, True, "synthetic")
    status, out = report(ledger, FixtureFailure("synthetic staging failure"))
    if status != 2:
        problems.append(f"a fixture failure returned {status}, not 2")
    if outcome_line("PASS", first_name) not in out:
        problems.append(f"the recorded check {first_key!r} is missing from "
                        "the report")
    for key, name in DECLARED_CHECKS[1:]:
        if outcome_line("NOT RUN", name) not in out:
            problems.append(f"the unreached check {key!r} is not reported as "
                            "NOT RUN")
    if "synthetic staging failure" not in out:
        problems.append("the report does not name the fixture failure")

    # (b) every declared check recorded and passing is a clean exit 0 with
    #     nothing reported as unreached.
    ledger = Ledger()
    with contextlib.redirect_stdout(io.StringIO()):
        for key, _ in DECLARED_CHECKS:
            ledger.record(key, True)
    status, out = report(ledger, None)
    if status != 0:
        problems.append(f"an all-passing ledger returned {status}, not 0")
    if "NOT RUN" in out:
        problems.append("an all-passing ledger reported a check as NOT RUN")

    # (c) a recorded FAILURE is a behavioural result (1), never conflated
    #     with a check that never ran.
    ledger = Ledger()
    with contextlib.redirect_stdout(io.StringIO()):
        for index, (key, _) in enumerate(DECLARED_CHECKS):
            ledger.record(key, index != 2)
    status, out = report(ledger, None)
    if status != 1:
        problems.append(f"a failing ledger returned {status}, not 1")
    if outcome_line("FAIL", DECLARED_CHECKS[2][1]) not in out:
        problems.append("the failing check is not reported as FAIL")

    # (d) an incomplete ledger with no fixture failure is a defect, not a
    #     pass — the shape a future edit that forgets to record a check
    #     would take.
    ledger = Ledger()
    with contextlib.redirect_stdout(io.StringIO()):
        for key, _ in DECLARED_CHECKS[:-1]:
            ledger.record(key, True)
    status, out = report(ledger, None)
    if status != 2:
        problems.append(f"a silently incomplete ledger returned {status}, "
                        "not 2")
    if outcome_line("NOT RUN", DECLARED_CHECKS[-1][1]) not in out:
        problems.append("a silently incomplete ledger does not name the "
                        "missing check")

    # (e) the ledger refuses an undeclared key and a duplicate record, so a
    #     typo cannot quietly create a check nothing declared.
    ledger = Ledger()
    try:
        ledger.record("not-a-declared-check", True)
    except KeyError:
        pass
    else:
        problems.append("the ledger accepted an undeclared key")
    with contextlib.redirect_stdout(io.StringIO()):
        ledger.record(first_key, True)
    try:
        ledger.record(first_key, True)
    except KeyError:
        pass
    else:
        problems.append("the ledger accepted a duplicate record")

    for problem in problems:
        print(f"  [FAIL] {problem}")
    if problems:
        print(f"\n{len(problems)} self-test checks failed")
        return 1
    print("  [PASS] a fixture failure names every declared check that did "
          "not run, and exits 2")
    print("  [PASS] a complete, all-passing ledger exits 0 with nothing "
          "unreached")
    print("  [PASS] a recorded failure exits 1 and is not conflated with an "
          "unreached check")
    print("  [PASS] an incomplete ledger with no fixture failure exits 2 and "
          "names the gap")
    print("  [PASS] the ledger refuses undeclared and duplicate records")
    print("\nall 5 self-test checks passed")
    return 0


if __name__ == "__main__":
    try:
        sys.exit(main())
    except FixtureFailure as exc:
        print(f"\nFIXTURE FAILURE: {exc}", file=sys.stderr)
        print(f"engine log: {LOG}", file=sys.stderr)
        sys.exit(2)
