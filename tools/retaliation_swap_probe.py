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
  2. setup — a REAL hit from a third live unit makes it the subject's
     recorded recent attacker (`unit.getLastAttacker`).

  Then, per case, the preconditions captured inside the graded chunk: the
  subject targets the ORIGINAL target on an `attack` goal, the recent
  attacker is the third unit at the age this case staged, both units are
  live and non-collapsed, the attacker is not a technomule, and its
  CURRENT Chebyshev distance is within `unit.getAttackRange(subject) +
  0.5`. Anything else is a FIXTURE FAILURE (exit 2), not a behavioural
  result — which is precisely how the existing collapsed-attacker case
  passes today without reaching the comparison.

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

HOW THE FIXTURE IS HELD STILL. The staged hit is real, so the staging leg
is real combat; everything after it is arranged so that combat cannot
disqualify the recent attacker through a DIFFERENT term of the branch's
`and` chain than the comparison under test:

* `combat.attack` stops swinging as soon as one swing WOUNDS (a miss
  simply swings again), so the subject collects one hit's wounds rather
  than however many land before the probe notices — then it is a plain
  no-op for the whole graded section.
* `unit.getAllIds` is narrowed to the units a leg actually needs, which
  also holds every other fixture unit off both the AI tick and
  `unit_resources`' injury tick.
* `unit.getLastAttacker` reports the staged hit at a FIXED age for the
  SUBJECT ONLY, delegating for every other unit — the same
  wrap-and-delegate technique `tools/mental_state_probe.py` uses on this
  very API. Without it the hit ages out of the window between console
  round trips.
* Before each case every fixture unit is revived, its bleeding dressed
  and its survival needs topped up, and the flanking geometry is
  re-established against the subject's current tile and VERIFIED.

Usage: python3 tools/retaliation_swap_probe.py [--port 9483]
Exit 0 = pass, 1 = a check failed, 2 = the fixture was never established.
"""
from __future__ import annotations
import argparse
import glob
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

# Requirement 4: the duration itself stays three seconds. This is the
# probe's EXPECTATION of the module's one definition, asserted against the
# live export — not a second definition of the window, which lives (once)
# in `scripts/unit_ai_combat.lua`.
EXPECTED_WINDOW_SEC = 3.0


class FixtureFailure(RuntimeError):
    """The scenario the probe needs was never established. Nothing
    downstream can be a behavioural result, so the run stops here."""


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
    """Make this unit's swings survivable, for the one real hit staged
    below.

    A full-strength acolyte can one-shot another, and a kill or a collapse
    disqualifies the recent attacker through a DIFFERENT branch term than
    the one under test. Strength is never 0 — that is the universal death
    rule — so this bounds the staged hit rather than removing it; the
    graded section suppresses swings outright instead (`patch_no_damage`).
    """
    send(port, lua(f"unit.setStat({uid},'strength',0.05);",
                   f"unit.setStat({uid},'toughness',100);",
                   "return 'ok'"))


def stanch(port: int, uid: int) -> str:
    """Dress every bleeding wound the staged hit left on `uid`.

    The staging leg is REAL combat, and a real wound keeps seeping long
    after it lands. Left alone the subject collapses from blood loss
    partway through the graded windows — observed live, and only on some
    runs — which disqualifies it through `tickOne`'s pose short-circuit
    rather than through anything this probe is testing.

    Self-treatment, so no kit, no second unit and no items are involved:
    `unit.treatBleeding` needs only `bleed_control` knowledge on the medic
    and improvises a tourniquet when the kit owner has no supplies
    (`Engine.Scripting.Lua.API.Units.Medical`). The knowledge goes back to
    zero afterwards, because `calm` deliberately took this unit out of the
    medic squad — `treat_ally` scores 8.0 and would tie with the action
    under test.
    """
    return send(port, lua(
        f"local u = {uid};",
        "unit.setKnowledge(u, 'bleed_control', 100);",
        "local n = 0;",
        "for _ = 1, 12 do",
        " local r = unit.treatBleeding(u, u);",
        " if not r or not r.ok then break end;",
        " n = n + 1 end;",
        "unit.setKnowledge(u, 'bleed_control', 0);",
        "return n"))


def restore(port: int, uid: int) -> None:
    """Undo what the previous window's real combat cost this unit.

    Applied before EACH graded window rather than once, so the second one
    starts from the same fixture the first did.
    """
    send(port, f"unit.revive({uid}); return 'ok'")
    stanch(port, uid)
    sustain(port, uid)


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
    """
    send(port, lua(
        "if not _G.__ret_gla then _G.__ret_gla = unit.getLastAttacker end;",
        "unit.getLastAttacker = function(u)",
        f" if u == {subject} then",
        f"  return {{ uid = {attacker}, at = engine.gameTime() - {age} }} end;",
        " return _G.__ret_gla(u) end;",
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
    failure. Neutered strength alone is not enough; repeated hits
    accumulate.

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
    precondition capture and the invocation being graded.

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
        "local pre = { target = s.attackTargetUid,",
        " recent = rec and rec.uid or -1,",
        " age = rec and (engine.gameTime() - (rec.at or 0)) or -1,",
        " dist = math.max(math.abs(me.gridX - at.gridX),",
        "                 math.abs(me.gridY - at.gridY)),",
        f" reach = (unit.getAttackRange({subject}) or 1.0) + 0.5,",
        f" subjectPose = unit.getPose({subject}),",
        f" attackerPose = unit.getPose({attacker}),",
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


def run_case(port, record, label: str, subject: int, target: int,
             attacker: int, sentinel: int, age: float) -> dict:
    """Stage one window, let NATURAL ticks run through it, then grade.

    The natural-tick leg is what produces the log evidence: the subject is
    put back on the original target and the branch's preconditions stand
    for the whole of it, so the engine's own `unitAi.update` callback
    reaches the comparison many times over. On the pre-#1483 code every
    one of those ticks raises and
    `Engine.Scripting.Lua.Script.callModuleFunctionReportingError` logs it.
    """
    print(f"\n--- {label} (staged hit age {age:g}s) ---")
    patch_last_attacker(port, subject, attacker, age)
    for uid in (subject, attacker, target):
        restore(port, uid)
    stage_geometry(port, subject, target, attacker)
    send(port, f"require('scripts.unit_ai').commandAttack({subject}, "
               f"{target}); return 'ok'")

    mark = log_size()
    time.sleep(NATURAL_TICK_WINDOW_SEC)
    errors = log_errors_since(mark)

    # Re-pin before grading. The window that just ran is real combat: the
    # subject closes on whatever it is targeting and its swings knock the
    # other two around, so the flanking geometry has to be re-established
    # against the subject's CURRENT tile — the branch reads the distance
    # as it stands when the graded invocation starts.
    stage_geometry(port, subject, target, attacker)
    graded = graded_update(port, subject, target, attacker, sentinel)
    check_preconditions(graded["pre"], age, f"{label} case")
    print(f"  preconditions: {graded['pre']}")
    record(f"{label}: no Lua error is logged across a window of natural ticks",
           not errors,
           f"{len(errors)} entr{'y' if len(errors) == 1 else 'ies'}"
           + (f": {errors[0]}" if errors else ""))
    return graded


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
    args = ap.parse_args()
    port = args.port

    proc = boot(port, log=LOG)
    results: list[tuple[str, bool, str]] = []

    def record(name: str, passed: bool, detail: str = "") -> None:
        results.append((name, passed, detail))
        print(f"  [{'PASS' if passed else 'FAIL'}] {name}"
              + (f" — {detail}" if detail else ""))

    try:
        bootstrap(port)
        window = window_seconds(port)
        record("the consumer's module can see one exported retaliation "
               f"window of {EXPECTED_WINDOW_SEC:g}s",
               window == EXPECTED_WINDOW_SEC,
               "scripts.unit_ai_combat.RETALIATE_WINDOW_SEC = "
               + ("absent — the attack module reads an unassigned global"
                  if window is None else f"{window:g}"))

        # The fixture. `target` and `attacker` flank the subject, both
        # inside melee reach: with the original target already in range
        # the subject stands and swings instead of walking off, so the
        # attacker stays within `getAttackRange + 0.5` across a whole
        # window of natural ticks without being re-pinned mid-flight.
        subject = spawn_acolyte(port, 0, 0)
        attacker = spawn_acolyte(port, 1, 0)
        target = spawn_acolyte(port, -1, 0)
        sentinel = spawn_acolyte(port, -8, 0)
        print(f"subject={subject} attacker={attacker} "
              f"target={target} sentinel={sentinel}")
        for uid in (subject, attacker, target, sentinel):
            neuter(port, uid)
            calm(port, uid)

        # A REAL hit, so the third unit genuinely becomes the recorded
        # recent attacker before anything is wrapped. Only the ATTACKER is
        # AI-ticked for this leg: a subject that fought back would wound
        # the attacker, and the resulting bleed-out disqualifies it later
        # through a DIFFERENT branch term than the one under test.
        patch_ids(port, [attacker])
        try:
            stage_and_grade(port, record, subject, target, attacker, sentinel)
        finally:
            remove_patches(port)

        print("\n--- result ---")
        failed = [n for n, ok, _ in results if not ok]
        for name, ok, _ in results:
            print(f"  {'PASS' if ok else 'FAIL'}  {name}")
        if failed:
            print(f"\n{len(failed)} of {len(results)} checks failed (#1483)")
            return 1
        print(f"\nall {len(results)} checks passed (#1483)")
        return 0
    finally:
        quit_engine(port, proc)


def stage_and_grade(port, record, subject: int, target: int,
                    attacker: int, sentinel: int) -> None:
    """Land the real hit, then grade both windows.

    Split out of `main` only so the id-list wrap installed for the staging
    leg is released by ONE `finally` covering every path out of it,
    including a fixture failure raised mid-staging.
    """
    send(port, f"require('scripts.unit_ai').commandAttack({attacker}, "
               f"{subject}); return 'ok'")
    landed = poll_until(45, lambda: send(
        port, f"local a = unit.getLastAttacker({subject}); "
              "return a and a.uid or 'nil'") == str(attacker))
    record("a real hit makes the third unit the recorded recent attacker",
           bool(landed),
           f"unit.getLastAttacker({subject}).uid == {attacker}"
           if landed else "no hit ever landed")
    if not landed:
        raise FixtureFailure(
            f"{attacker} never landed a hit on {subject}; the branch's "
            "recent-attacker precondition cannot be established")

    # Retire the staging order. The recorded hit stays; only the
    # standing orders go, and from here the id list holds the attacker
    # off every tick so it cannot move, heal, or bleed.
    send(port, lua(
        "local ai = require('scripts.unit_ai');",
        f"local s = ai.getState({attacker});",
        "if s then ai.markGoalAccomplished(s, 'attack');",
        " s.attackTargetUid = nil end;",
        f"unit.revive({attacker}); unit.stop({attacker});",
        f"unit.revive({subject}); unit.stop({subject});",
        "return 'ok'"))
    print(f"  staged wounds dressed: subject {stanch(port, subject)}, "
          f"attacker {stanch(port, attacker)}")
    patch_ids(port, [subject, sentinel])
    patch_no_damage(port)

    fresh = run_case(port, record, "fresh window", subject, target,
                     attacker, sentinel, FRESH_AGE_SEC)
    record("the retaliation swap retargets the subject onto the "
           "recent attacker",
           fresh["postTarget"] == attacker,
           f"attackTargetUid {fresh['pre']['target']} -> "
           f"{fresh['postTarget']} (wanted {attacker})")
    record("the subject's tick reaches its post-execute completion "
           "point",
           fresh["ok"] == 1 and fresh["subjectNext"] > fresh["t0"],
           f"nextActionAt {fresh['subjectNext']} vs t0 {fresh['t0']}"
           + (f"; update raised {fresh['err']}"
              if fresh["ok"] != 1 else ""))
    record("a unit ordered after the subject is reached by the SAME "
           "update invocation",
           fresh["sentinelNext"] > fresh["t0"],
           f"sentinel nextActionAt {fresh['sentinelNext']} vs t0 "
           f"{fresh['t0']} (0 = never ticked)")

    stale = run_case(port, record, "stale window", subject, target,
                     attacker, sentinel, STALE_AGE_SEC)
    record("a hit older than the window triggers no swap",
           stale["postTarget"] == target,
           f"attackTargetUid stayed {stale['postTarget']} "
           f"(wanted {target}, not {attacker})")
    record("the stale-window tick completes without raising",
           stale["ok"] == 1 and stale["subjectNext"] > stale["t0"],
           f"pcall ok={stale['ok']}, nextActionAt "
           f"{stale['subjectNext']} vs t0 {stale['t0']}"
           + (f"; update raised {stale['err']}"
              if stale["ok"] != 1 else ""))


if __name__ == "__main__":
    try:
        sys.exit(main())
    except FixtureFailure as exc:
        print(f"\nFIXTURE FAILURE: {exc}", file=sys.stderr)
        print(f"engine log: {LOG}", file=sys.stderr)
        sys.exit(2)
