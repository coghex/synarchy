#!/usr/bin/env python3
"""Mental-states probe (#352).

Drives the REAL engine's scripts/mental_state.lua — the threshold
state-machine over the unified state_of_mind (#350) — plus its unit-AI
short-circuit (scripts/unit_ai_mental.lua) to confirm:

  1. A fresh unit reads as mentally "stable".
  2. Tanked wellbeing (mood + emotional_pain pinned low) flips the unit
     to "stressed"; the recovery band has real hysteresis — a state of
     mind parked INSIDE the 0.35..0.45 dead band neither recovers a
     stressed unit nor stresses a stable one (the #304 flicker lesson).
  3. With the rolls pinned deterministic (mental.TUNE.BREAK_CHANCE_MAX
     = 1.0), sustained stress tips into a break EPISODE: the AI
     short-circuits (currentAction == "mental_break"), the event log
     narrates it, the episode ends on its own after its rolled
     duration, and the cooldown then blocks an immediate re-break even
     though the state of mind is still on the floor.
  4. Forced behaviours: a "wander" break actually moves the unit; a
     "flee" break increases its distance from the nearest other unit.
  5. Euphoria: a sustained near-content state of mind (chance pinned)
     enters a euphoria episode; concentration reads ~EUPHORIA_
     CONCENTRATION_BONUS above its physiological base; the exit band
     (0.90 in / 0.80 out) holds inside the dead band and releases
     below it.
  6. REGRESSION GUARD: none of it touches the physiological ladder —
     mid-break the unit is standing, and brain.isUnconscious/
     isDelirious/isConfused stay false (they key on consciousness
     alone).
  7. Deterministic break-behaviour roll (#717): mental.rollBehavior(draw)
     hits the exact 35/35/15/15 boundaries without statistical sampling.
  8. Forced catatonia (#717): stops an already-moving unit in place,
     produces no displacement or replacement action for the whole
     episode, leaves it standing and physiologically lucid, and exits
     through the normal cooldown path. Then (#1709) the same break
     forced MID-POSE-TRANSITION — on a real one-tile leap, on both the
     airborne arc and the chained landing step that follows it — must
     leave the unit grounded rather than suspended above its grid layer:
     once its activity stops reading "transitioning", realZ equals
     gridZ and the pose is standing.
  9. Forced lash-out (#717): prefers a recent eligible attacker (staged
     via a real landed hit — there's no Lua setter for the last-attacker
     memory, so this uses combat_anim_probe.py's spawn-adjacent +
     commandAttack + wait-for-a-swing-to-land pattern) over a closer
     decoy; falls back to the nearest eligible unit (an ally — every
     spawned acolyte shares one faction) when there's no eligible
     attacker; excludes dead, collapsed, self, and the technomule;
     replaces a lost target (dies mid-episode) with another eligible
     unit; wanders and keeps searching when nothing is eligible;
     produces a real landed attack through the short-circuit; clears
     every lash-out-owned goal/target once the episode ends — even if
     delirium overlaps the exact tick the episode expires; ranks
     nearest-eligible by Chebyshev distance (not Euclidean); and stops
     a stale pursuit immediately (rather than walking it out) when a
     target leaves range with no replacement available.
     The attacker preference (9a) is graded ONLY on a selection whose
     preconditions held when it was made (#2773): the selection's own
     reads are observed at the decision boundary — the hit and its
     game-time age, both candidates' eligibility by the production
     predicate, and the decoy strictly closer — and a setup that missed
     them is discarded, named and restaged (bounded) rather than graded
     as a policy failure. The hit age at selection is printed.

Usage: python3 tools/mental_state_probe.py [--port 9352]
       python3 tools/mental_state_probe.py --lashout-case stale|ineligible
         (#2773: run only 9a with its setup pushed off the preconditions —
         `stale` ages the first attempt's hit past the window, so it is
         restaged; `ineligible` moves every attempt's attacker out of
         range, so 9a ends in a named setup failure, exit 1)
Exit 0 = pass.
"""
from __future__ import annotations
import argparse, glob, json, sys, time
from probelib import (boot, init_arena, load_ai_stack, poll_until,
                      quit_engine, send, send_json, spawn_acolyte)

LOG = "/tmp/mental_state_probe_engine.log"


def bootstrap(port):
    loaders = [
        ("data/substances/*.yaml", "engine.loadSubstanceYaml"),
        ("data/infections/*.yaml", "engine.loadInfectionYaml"),
        ("data/items/*.yaml",      "engine.loadItemYaml"),
        ("data/equipment/*.yaml",  "engine.loadEquipmentYaml"),
        ("data/materials/*.yaml",  "engine.loadMaterialYaml"),
        ("data/factions/*.yaml", "engine.loadFactionYaml"),
        ("data/units/*.yaml",      "engine.loadUnitYaml"),
    ]
    for pattern, fn in loaders:
        for path in sorted(glob.glob(pattern)):
            send(port, f"{fn}('{path}'); return 'ok'")
    load_ai_stack(port)
    init_arena(port)


def msummary(port, uid):
    raw = send(port, f"return require('scripts.mental_state').summary({uid})")
    try:
        return json.loads(raw)
    except json.JSONDecodeError:
        return {"_raw": raw}


def mstate(port, uid):
    return msummary(port, uid).get("state")


def set_wellbeing(port, uid, mood, ep=0.0):
    """Pin the psychological inputs; brain.tick folds them into
    state_of_mind on its next pass (wellbeing = mood - 0.5*ep)."""
    send(port, f"unit.setStat({uid},'mood',{mood}); "
               f"unit.setStat({uid},'emotional_pain',{ep}); return 'ok'")


def tune(port, **kv):
    stmts = "; ".join(
        f"m.TUNE.{k} = {v}" for k, v in kv.items())
    send(port, f"local m = require('scripts.mental_state'); {stmts}; return 'ok'")


def unit_pos(port, uid):
    raw = send(port, f"local i = unit.getInfo({uid}); "
                     f"return {{x = i.gridX, y = i.gridY}}")
    p = json.loads(raw)
    return p["x"], p["y"]


def lash_target(port, uid):
    """Lash-out's episode-owned attack target, or 'nil'. The 'x or nil'
    idiom (see e.g. follow_command_priority_probe.py) round-trips a Lua
    nil through the console reliably; a bare nil return doesn't."""
    return send(port, f"local s=require('scripts.unit_ai').getState({uid}); "
                      f"return s and s.attackTargetUid or 'nil'")


def lash_ai_flags(port, uid):
    """{tgt, goal, committed} off unit_ai's per-unit state — used to prove
    lash-out's episode-owned combat state is fully cleared at episode end."""
    return json.loads(send(port,
        f"local s=require('scripts.unit_ai').getState({uid}); "
        f"return {{tgt = (s and s.attackTargetUid) or false, "
        f"goal = (s and s.activeGoal) or false, "
        f"committed = (s and s.committed) or false}}"))


# Vertical-agreement tolerance for #1709. usRealZ is a Float widened to
# a Lua double, so a landed unit's continuous Z reads as its integer grid
# Z exactly; this only absorbs that widening. It is nowhere near a real
# arc — a one-tile leap peaks 0.8 z above the launch.
Z_EPS = 1e-4


def leap_sample(port, uid):
    """One reading of the four fields #1709 is about.

    ACTIVITY IS READ FIRST and the position LAST, deliberately. The unit
    thread publishes to the render mirror asynchronously, so if the reads
    straddle a tick the position is the LATER of the two: a unit that has
    genuinely finished its transition can then only look MORE settled,
    never less. An interpolated realZ reported against a
    non-transitioning activity is therefore the defect, not the sampling.
    """
    return json.loads(send(port,
        f"local a = unit.getActivity({uid}); local p = unit.getPose({uid}); "
        f"local i = unit.getInfo({uid}); "
        f"return {{activity = a, pose = p, gridZ = i.gridZ, realZ = i.realZ}}"))


def z_agrees(sample):
    return abs(sample["realZ"] - sample["gridZ"]) <= Z_EPS


def leap_break_case(port, uid, leg):
    """Force catatonia partway through a REAL leap and report the outcome.

    `leg` selects where the break lands:

      "airborne" -- the Standing->Falling arc. The unit is genuinely off
        its grid layer here (usRealZ arcs above usGridZ, which does not
        move until the landing snap), which is the state the pre-#1709
        `unit.stop` froze forever.
      "landing"  -- the chained Falling->Standing step that starts once
        the arc touches down. Position is already snapped by then, but it
        is a SECOND transition with its own timer, and clearing it left
        the unit stuck in the falling pose. Told apart from the arc by
        the pose: the arc keeps `standing`, the chain reads `falling`.

    Returns (detail, samples); detail is None when the case passed.
    """
    ready = poll_until(10, lambda: (lambda smp: smp if (
        smp["pose"] == "standing" and smp["activity"] != "transitioning"
    ) else None)(leap_sample(port, uid)))
    if not ready:
        return ("unit never reached a standing, non-transitioning state "
                "to leap from"), []

    # Stop and leap in ONE console line, off ONE position read: the AI is
    # free to wander between round trips, and the target must stay inside
    # the unit's own reach (jumpMaxTiles). The unit thread consumes the
    # two commands in order, so the leap always launches from a stopped
    # unit at the tile that read named.
    target = send(port,
        f"local i = unit.getInfo({uid}); unit.stop({uid}); "
        f"local tx = math.floor(i.gridX) + 1; local ty = math.floor(i.gridY); "
        f"unit.jump({uid}, tx, ty); return tx .. ',' .. ty")

    # `unit.jump` only reports that the command was ENQUEUED — the unit
    # thread refuses an out-of-reach or non-standing leap silently — so
    # the run is only meaningful once a coherent sample shows the leap
    # actually in the requested leg.
    if leg == "airborne":
        def wanted(smp):
            return (smp["activity"] == "transitioning"
                    and smp["pose"] == "standing"
                    and smp["realZ"] > smp["gridZ"] + 0.05)
    else:
        def wanted(smp):
            return (smp["activity"] == "transitioning"
                    and smp["pose"] == "falling")
    entered = poll_until(10, lambda: (lambda smp: smp if wanted(smp) else None)(
        leap_sample(port, uid)), interval=0.02)
    if not entered:
        return (f"leap toward {target} never reached its {leg} leg "
                f"(unit.jump only enqueues; the unit thread refuses an "
                f"out-of-reach leap)"), []

    send(port, f"require('scripts.mental_state').forceBreak({uid},'catatonia'); "
               f"return 'ok'")

    # Every sample from break entry onward: while the arc is still in
    # flight the activity reads "transitioning" and an interpolated realZ
    # is correct; the moment it does not, realZ must agree with gridZ.
    samples = [entered]
    settled = None
    deadline = time.time() + 15
    while time.time() < deadline:
        smp = leap_sample(port, uid)
        samples.append(smp)
        if smp["activity"] != "transitioning":
            settled = smp
            break
        time.sleep(0.05)
    if settled is None:
        return "never left the transition after the break", samples
    floating = [smp for smp in samples
                if smp["activity"] != "transitioning" and not z_agrees(smp)]
    if floating:
        return f"suspended above its grid layer: {floating[0]}", samples

    # The issue's own repro sampled again 1.5 s later, where the original
    # defect was still visible.
    time.sleep(1.5)
    later = leap_sample(port, uid)
    samples.append(later)
    if later["activity"] == "transitioning":
        return f"still transitioning 1.5 s after the break: {later}", samples
    if not z_agrees(later):
        return f"still suspended 1.5 s after the break: {later}", samples
    if later["pose"] != "standing":
        return f"never reached the grounded standing pose: {later}", samples

    # Catatonia freezes the unit, so nothing walked it back onto its grid
    # layer (a walk step rewrites usRealZ, PathAdvance.hs) -- but only
    # while the episode is live. Prove it still was.
    if mstate(port, uid) != "break":
        return ("episode ended before the check landed, so a resumed walk "
                "could have normalised realZ on its own"), samples
    return None, samples


#: Where 9a stages its lash/attacker/decoy cluster, one per attempt. Each
#: is more than 8 tiles (LASHOUT_RANGE) from every other cluster this
#: probe uses, so a discarded attempt's units can never be candidates in
#: the next one, and every one is inside the 5x5-chunk arena.
LASHOUT_CLUSTERS = ((-28, -25), (35, 0), (35, 15))

#: Dressing the staged hit's wound (#2773), ported from
#: retaliation_swap_probe.py (#1578) with its bounds unchanged; see
#: `stanch` there for why each one is what it is. STANCH_ATTEMPTS is the
#: runaway bound, not the termination condition: the loop stops when the
#: bleed rate settles below STANCH_SETTLED_RATE (a dressed wound seeps at
#: order 1e-4 L/s, never exactly zero) or after STANCH_STALLED_PASSES
#: treatments in a row fail to lower it. MEDICAL_KITS stocked first-aid
#: kits supply real bandages, without which the medic only improvises.
STANCH_ATTEMPTS = 64
STANCH_STALLED_PASSES = 3
STANCH_SETTLED_RATE = 1e-3
MEDICAL_KITS = 8

#: The 9a/9b fighters' strength_base (#2773), retaliation_swap_probe's
#: value: small rather than zero, because a swing that cannot land never
#: stamps the attacker record the staged hit exists to create.
NEUTERED_STRENGTH_BASE = 0.02

#: The lash-out episode length scoped to 9a/9b (#2773). Phase 9's 6 s
#: tune is shorter than staging, grading, dressing and a probabilistic
#: landed hit together: traced 9b failures had the episode's expiry clear
#: the target before any strike landed. 9a's grade is taken at the first
#: selection, so the length cannot affect it.
LASHOUT_9AB_EPISODE = 30.0

#: Where 9a puts the attacker (east) and the decoy (west) relative to the
#: subject before the break: the decoy strictly closer, both comfortably
#: inside LASHOUT_RANGE.
ATTACKER_OFFSET = 3.0
DECOY_OFFSET = 1.5


def stage_lashout_cluster(port, x, y):
    """Spawn a lash-out subject at (x, y), stage a REAL landed hit on it
    from an attacker, stand the attacker down and snap it back to exactly
    ATTACKER_OFFSET tiles east, then spawn a decoy DECOY_OFFSET tiles
    west. Answers (lash, attacker, decoy, problem): `problem` names a
    setup step that did not take effect, else None.

    Both offsets sit clear of the roughly one-tile spacing units settle
    into on their own — a decoy spawned half a tile away was pushed out
    to about 1.1, past an attacker at 1.0 — and well inside lash-out's
    8-tile range. The decoy spawns LAST, so it has no time to wander.

    The hit is a real one — there is no Lua setter for the last-attacker
    memory (only unit.getLastAttacker) — via combat_anim_probe.py's
    spawn-adjacent + commandAttack + wait-for-a-swing-to-land pattern.

    The subject's OWN reaction to that hit is suppressed until the break
    (#2773): its unit.getLastAttacker reads nil to every caller during
    setup — the 9c isolation idiom — so its ordinary incoming_hit
    engagement and retaliation never walk it toward the attacker, which
    is what used to leave the attacker NEAREST at selection (so the old
    check rarely tested the preference at all). The engine still records
    the hit. observe_first_lashout_selection lifts the mask in the same
    console chunk that forces the break, and the mental short-circuit
    runs before any ordinary candidate, so the selection reads the real
    memory. The geometry is still not trusted from here: it is observed
    at the selection.
    """
    lash = spawn_acolyte(port, x, y)
    set_wellbeing(port, lash, 1.0, 0.0)
    poll_until(5, lambda: mstate(port, lash) == "stable")
    # Its own kits, BEFORE the staged hit: dress_staged_wound treats it as
    # its own medic, and by the time a wound is seeping every console round
    # trip costs blood that never comes back.
    provision_medical(port, lash)
    send(port, f"_G.__probe_lash_real_gla = _G.__probe_lash_real_gla "
               f"or unit.getLastAttacker; "
               f"unit.getLastAttacker = function(u) "
               f"if u == {lash} then return nil end "
               f"return _G.__probe_lash_real_gla(u) end; "
               f"_G.__probe_lash_unmask = function() "
               f"unit.getLastAttacker = _G.__probe_lash_real_gla; "
               f"_G.__probe_lash_real_gla = nil; "
               f"_G.__probe_lash_unmask = nil end; return 'ok'")
    attacker = spawn_acolyte(port, x + 1, y)
    # Neuter BOTH fighters' damage output BEFORE the staged hit — a
    # full-strength acolyte swing landed on an unarmored target can be
    # lethal in one blow (Combat.Resolution's E_swing scales with
    # strength), and a dead unit stops ticking mental_state entirely
    # (unit_resources skips dead/collapsed units), which would wedge every
    # check below forever. That includes lash's own lash-out swings in 9b.
    #
    # Written to `strength_base`, then unit.recomputeBody (#2773, after
    # retaliation_swap_probe's `neuter`, #1578). Writing `strength` itself
    # did not last: Unit.Thread.Command.Body.recomputeBodyDerivedStats
    # re-derives it from strength_base, and every physiology pass's
    # starvation.refreshStrength re-derives it again from strength_body, so
    # the old 0.05 was gone before the hit and the staged hits landed at
    # ordinary strength (a 0.37 slash; a heavy head stab with a skull
    # fracture). toughness=100 is kept: it caps Combat.Resolution's
    # energy-transfer reduction at its 50% max (clamp(toughness*0.05, 0,
    # 0.5)), a second, independent line of defense.
    send(port, f"unit.setStat({attacker},'strength_base',{NEUTERED_STRENGTH_BASE}); "
               f"unit.setStat({attacker},'toughness',100); "
               f"unit.recomputeBody({attacker}); "
               f"unit.setStat({lash},'strength_base',{NEUTERED_STRENGTH_BASE}); "
               f"unit.setStat({lash},'toughness',100); "
               f"unit.recomputeBody({lash}); return 'ok'")
    send(port, f"require('scripts.unit_ai').commandAttack({attacker},{lash}); "
               f"return 'ok'")
    hit = poll_until(35, lambda: send(
        port, f"local a=_G.__probe_lash_real_gla({lash}); "
              f"return a and a.uid or 'nil'"
    ) == str(attacker))
    if not hit:
        return lash, attacker, None, (f"attacker {attacker} never landed a "
                                      f"hit on {lash} — can't test attacker "
                                      f"preference")
    print(f"  [pass] staged a real hit: attacker {attacker} landed on "
          f"lash-out subject {lash}")
    stand_down_attacker(port, lash, attacker)
    # Snap the attacker back to a fixed, known distance — its approach
    # and swing leave it wherever combat physics did. unit.setPos just
    # enqueues a UnitTeleport on the unit thread, so confirm it landed.
    lx, ly = unit_pos(port, lash)
    send(port, f"unit.setPos({attacker}, {lx + ATTACKER_OFFSET}, {ly}); "
               f"return 'ok'")
    if not poll_until(5, lambda: (lambda ax, ay:
            (ax - (lx + ATTACKER_OFFSET)) ** 2 + (ay - ly) ** 2 < 0.05)(
                *unit_pos(port, attacker))):
        return lash, attacker, None, (f"teleporting {attacker} back to "
                                      f"{ATTACKER_OFFSET:g} tiles from {lash} "
                                      f"never took effect")
    decoy = spawn_acolyte(port, lx - DECOY_OFFSET, ly)
    return lash, attacker, decoy, None


def stand_down_attacker(port, lash, attacker):
    """End the attacker's own fight and keep both fighters standing.

    Left running, the attacker's attack order keeps it on top of the
    subject, and sustained combat can collapse either side from
    accumulated blood loss even with strength/toughness reduced (those
    shrink per-hit severity, not how long the fight runs) — a collapsed
    unit is excluded by lash-out's eligibility, or stops ticking
    altogether.

    Its stamina is deliberately LEFT ALONE (#2773). The old setup drained
    it to 0.1 of max to stop its ambient wander, but a unit left that low
    after a fight was traced Collapsed by the time the setup finished —
    and a collapsed attacker is excluded from lash-out, so the subject
    took the decoy instead and 9b's "lands an attack on the attacker"
    could never happen. Where the attacker stands is observed at the
    selection, so a wander is a setup failure rather than a silent one.
    """
    send(port, f"local ai=require('scripts.unit_ai') "
               f"local s=ai.getState({attacker}); "
               f"if s then ai.markGoalAccomplished(s,'attack'); "
               f"s.attackTargetUid=nil end; unit.stop({attacker}) "
               f"unit.revive({attacker}); unit.revive({lash}); return 'ok'")


def observe_first_lashout_selection(port, lash, attacker, decoy, timeout=10):
    """Force a lash-out break on `lash` and record its FIRST target
    selection AT the decision boundary (#2773).

    pickLashoutTarget (scripts/unit_ai_mental.lua) opens with
    unit.getLastAttacker(uid) and engine.gameTime(), and lashOutExecute
    hands the result straight to combatAttack.attackTargetExecute in the
    same synchronous call. So, installed in the SAME console chunk that
    forces the break (the Lua thread runs it whole, before the next AI
    tick):

      * a getLastAttacker wrapper snapshots, on every read for `lash`,
        the hit, its game-time age, and both candidates' existence, pose,
        Chebyshev distance and eligibility — judged by the production
        predicate itself (unit_ai_mental.lashoutPolicy.eligible) — and
      * an attackTargetExecute wrapper, on the first lash-out-owned call
        for `lash`, binds the latest snapshot (the one that selection
        just read) to the target it chose, then restores both originals.

    Later target polls cannot reconstruct those historical
    preconditions; this reads them where the decision reads them.
    Answers the recorded selection dict, or None when no selection
    happened within `timeout` seconds. The wrappers are always removed.
    """
    # One line: the console reads newline-terminated commands, and the
    # chunk carries no comments, so joining its lines changes nothing.
    send(port, " ".join(line.strip() for line in f"""
local lash, attacker, decoy = {lash}, {attacker}, {decoy}
if _G.__probe_lash_unmask then _G.__probe_lash_unmask() end
local policy = require('scripts.unit_ai_mental').lashoutPolicy
local atk = require('scripts.unit_ai_combat_attack')
local origGLA, origATE = unit.getLastAttacker, atk.attackTargetExecute
local rec = {{}}
_G.__probe_lash_rec = rec
_G.__probe_lash_restore = function()
  unit.getLastAttacker = origGLA
  atk.attackTargetExecute = origATE
  _G.__probe_lash_restore = nil
end
local function candidate(me, oid)
  local info = unit.getInfo(oid)
  local d = -1
  if me and info then
    d = math.max(math.abs(me.gridX - info.gridX), math.abs(me.gridY - info.gridY))
  end
  return {{ uid = oid, exists = unit.exists(oid), pose = unit.getPose(oid) or 'none',
           dist = d, eligible = (me ~= nil) and policy.eligible(lash, me, oid) }}
end
unit.getLastAttacker = function(u)
  local a = origGLA(u)
  if u == lash then
    local me = unit.getInfo(lash)
    local now = engine.gameTime()
    rec.pending = {{ now = now, window = policy.attackerWindow, range = policy.range,
      hitBy = a and a.uid or -1, hitAt = a and a.at or -1,
      age = a and (now - (a.at or 0)) or -1,
      attacker = candidate(me, attacker), decoy = candidate(me, decoy) }}
  end
  return a
end
atk.attackTargetExecute = function(u, s, params)
  if u == lash and s and s.mentalLashoutActive and rec.pending and not rec.selection then
    rec.selection = rec.pending
    rec.selection.target = s.attackTargetUid or -1
    _G.__probe_lash_restore()
  end
  return origATE(u, s, params)
end
require('scripts.mental_state').forceBreak(lash, 'lash_out')
return 'ok'""".splitlines()))

    def selection():
        raw = send(port, "local r=_G.__probe_lash_rec; "
                         "return r and r.selection or 'nil'")
        try:
            sel = json.loads(raw)
        except (json.JSONDecodeError, TypeError):
            return None
        return sel if isinstance(sel, dict) else None

    try:
        sel = poll_until(timeout, selection)
        if sel is None:
            # Name what the subject was doing instead, so a missing
            # selection is diagnosable rather than a bare timeout.
            raw = send(port, f"local r=_G.__probe_lash_rec; "
                             f"local s=require('scripts.unit_ai').getState({lash}); "
                             f"local m=require('scripts.mental_state').summary({lash}); "
                             f"return {{ read = (r and r.pending) and true or false, "
                             f"state = m and m.state or 'nil', "
                             f"target = s and s.attackTargetUid or -1, "
                             f"action = s and s.currentAction or 'nil', "
                             f"pose = unit.getPose({lash}) or 'nil' }}")
            print(f"  [setup] no lash-out selection observed for {lash}: {raw}")
        return sel
    finally:
        send(port, "if _G.__probe_lash_restore then _G.__probe_lash_restore() end; "
                   "_G.__probe_lash_rec = nil; return 'ok'")


def lashout_setup_problems(sel, attacker, decoy):
    """Every precondition the graded selection did NOT meet, each named
    with the value observed at selection; empty when it is a fair test.

    A fair test of "prefers the recent attacker over a closer decoy":
    the hit the selection read is `attacker`'s and within the production
    window (inclusive), the attacker is eligible (exists, not dead or
    collapsed, within range — the production predicate), and the decoy is
    eligible too and STRICTLY closer under the production Chebyshev
    metric. Anything else makes the decoy, or the attacker, the correct
    pick for a reason that is not the preference under test.
    """
    a, d = sel["attacker"], sel["decoy"]
    problems = []
    if sel["hitBy"] != attacker:
        problems.append(f"the hit read at selection was by {sel['hitBy']}, "
                        f"not attacker {attacker}")
    elif not (0 <= sel["age"] <= sel["window"]):
        problems.append(f"hit age at selection {sel['age']:.2f}s is outside "
                        f"the {sel['window']:g}s attacker window")
    if not a["eligible"]:
        problems.append(f"attacker {attacker} ineligible at selection "
                        f"(exists={a['exists']}, pose={a['pose']}, "
                        f"distance={a['dist']:.2f}, range={sel['range']:g})")
    if not d["eligible"]:
        problems.append(f"decoy {decoy} ineligible at selection "
                        f"(exists={d['exists']}, pose={d['pose']}, "
                        f"distance={d['dist']:.2f})")
    elif a["eligible"] and not d["dist"] < a["dist"]:
        problems.append(f"decoy {decoy} at {d['dist']:.2f} is not strictly "
                        f"closer than attacker {attacker} at {a['dist']:.2f}")
    return problems


def perturb_lashout_case(port, case, attempt, lash, attacker):
    """`--lashout-case` (#2773): push ONE staged setup off the fair-test
    preconditions before the break, through the real engine, so the
    classification can be shown on the actual selection path. Staging
    has already stood the attacker down, so its hit can age and it stays
    where it is put.

      * stale      — first attempt only: wait until the staged hit is
                     older than the attacker window (game time keeps
                     running; nothing is frozen), so that attempt must be
                     discarded and RESTAGED;
      * ineligible — every attempt: teleport the attacker beyond
                     lash-out range, so no attempt is gradable and 9a
                     must end in a named SETUP failure.
    """
    if case == "stale" and attempt == 1:
        poll_until(30, lambda: send(
            port, f"local a=_G.__probe_lash_real_gla({lash}); "
                  f"return (a and engine.gameTime()-a.at > 10.5) and 'yes' or 'no'"
        ) == "yes", interval=0.5)
    elif case == "ineligible":
        lx, ly = unit_pos(port, lash)
        send(port, f"unit.setPos({attacker}, {lx + 12}, {ly}); return 'ok'")
        poll_until(5, lambda: abs(unit_pos(port, attacker)[0] - (lx + 12)) < 0.25)


def provision_medical(port, uid):
    """Give `uid` its own stocked first-aid kits (retaliation_swap_probe's
    helper, #1578). `unit.addItem` mints a container def's authored
    contents (#1418), so each kit arrives holding real bandages."""
    for _ in range(MEDICAL_KITS):
        send(port, f"unit.addItem({uid}, 'first_aid_kit'); return 'ok'")


def stanch(port, uid):
    """Dress every bleeding wound on `uid` by self-treatment
    (retaliation_swap_probe's helper, #1578, same loop and bounds).

    `unit.treatBleeding` needs only `bleed_control` knowledge on the
    medic; it is raised for the treatment and put back to 0 afterwards,
    so the subject is not left a medic. This stops the seeping; it cannot
    put blood back. Answers {dressed, bleedRate}, or None if the console
    returned something else.
    """
    got = send_json(port, " ".join((
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
        "return { dressed = n, bleedRate = rate() }")), timeout=30.0)
    return got if isinstance(got, dict) else None


def dress_staged_wound(port, lash):
    """After 9a is graded, before 9b's window: dress the wound the staged
    hit left on the subject (#2773).

    Without it 9b raced the subject's bleed-out against probabilistic
    strikes. A traced failure had every strike admitted at reach but
    missed or dodged, and the subject collapsed 'bleeding from r_bicep' 9 s
    after the staged hit, before one landed — so 9b graded whether the
    subject outlived a bleed, not whether lash-out produces a landed
    attack.

    The dressing must not touch what lash-out reads: the subject's real
    attacker record — who, and when — is read before and after, and any
    change is a fixture failure. Answers True when the record is
    unchanged.
    """
    record = (f"local a=unit.getLastAttacker({lash}); "
              f"return a and string.format('%s@%.6f', tostring(a.uid), a.at or -1) "
              f"or 'nil'")
    before = send(port, record)
    dressed = stanch(port, lash)
    after = send(port, record)
    if before != after:
        print(f"  [FAIL] setup: dressing {lash}'s staged wound changed its "
              f"attacker record ({before} -> {after})")
        return False
    print(f"  [setup] dressed {lash}'s staged wound before 9b: {dressed}; "
          f"attacker record unchanged ({after})")
    return True


def lashout_attacker_preference(port, case=None):
    """Phase 9a (#717, #2773): lash-out prefers a recent eligible attacker
    over a closer decoy — graded ONLY on a selection whose preconditions
    held when it was made (see lashout_setup_problems).

    A setup that misses them is not a policy result: it is discarded,
    named, and RESTAGED on a fresh cluster, at most len(LASHOUT_CLUSTERS)
    times. The first gradable selection decides — a wrong target there is
    a policy failure, and no later attempt runs to erase it. Running out
    of attempts is a SETUP failure, and fails the probe all the same.

    Answers (ok, lash, attacker, decoy) for the attempt that was graded
    (or the last one staged), which 9b keeps using.
    """
    reasons = []
    lash = attacker = decoy = None
    for attempt, (x, y) in enumerate(LASHOUT_CLUSTERS, 1):
        if attempt > 1:
            # The discarded cluster's units go away entirely, so nothing
            # of that setup can take part in this one.
            send(port, "if _G.__probe_lash_unmask then _G.__probe_lash_unmask() end; "
                       "return 'ok'")
            for u in (lash, attacker, decoy):
                if u is not None:
                    send(port, f"unit.destroy({u}); return 'ok'")
        lash, attacker, decoy, problem = stage_lashout_cluster(port, x, y)
        problems = [problem] if problem else []
        sel = None
        if not problems:
            if case:
                perturb_lashout_case(port, case, attempt, lash, attacker)
            sel = observe_first_lashout_selection(port, lash, attacker, decoy)
            if sel is None:
                problems = ["no lash-out target selection within 10s of the break"]
            else:
                problems = lashout_setup_problems(sel, attacker, decoy)
        if problems:
            reasons.append(f"attempt {attempt}: " + "; ".join(problems))
            print(f"  [setup] lash-out attempt {attempt} at ({x},{y}) "
                  f"discarded before grading: {'; '.join(problems)}")
            continue
        # Graded below on the recorded selection, so nothing that moves
        # from here on can change it. Put the attacker back next to the
        # subject for 9b, the arrangement that check was written against:
        # a three-tile walk across the arena's uneven terrain could end
        # in a fall, and a fallen unit is Collapsed, which 9b cannot
        # survive.
        lx, ly = unit_pos(port, lash)
        send(port, f"unit.setPos({attacker}, {lx + 1}, {ly}); return 'ok'")
        poll_until(5, lambda: (lambda ax, ay:
                (ax - (lx + 1)) ** 2 + (ay - ly) ** 2 < 0.05)(
                    *unit_pos(port, attacker)))
        detail = (f"hit age at selection {sel['age']:.2f}s "
                  f"(window {sel['window']:g}s), attacker at "
                  f"{sel['attacker']['dist']:.2f}, decoy at "
                  f"{sel['decoy']['dist']:.2f}")
        if sel["target"] == attacker:
            print(f"  [pass] lash-out prefers the recent attacker {attacker} "
                  f"over the closer decoy {decoy} — {detail}")
            return True, lash, attacker, decoy
        print(f"  [FAIL] lash-out target={sel['target']}, expected attacker="
              f"{attacker} (decoy={decoy}) — preconditions held: {detail}")
        return False, lash, attacker, decoy
    send(port, "if _G.__probe_lash_unmask then _G.__probe_lash_unmask() end; "
               "return 'ok'")
    print(f"  [FAIL] setup: lash-out attacker preference could not be graded "
          f"— no attempt established its preconditions ({' | '.join(reasons)})")
    return False, lash, attacker, decoy


def install_swap_observer(port, lashB, victimB, attackerB, then_lua=""):
    """Wrap unit_ai_combat_attack.attackTargetExecute — the shared execute
    lash-out drives and the retaliation swap lives in — to record, for
    every call on `lashB`, the facts 9c2's check depends on (#2773):
    target before and after the call, lashB's recorded last attacker (uid,
    at) and its age in GAME seconds, the Chebyshev distance to that
    attacker against getAttackRange(lashB) + 0.5 (the swap's own reach
    test), the attacker's pose as wrapped and as it really is, whether
    the victim is lash-out eligible, and whether lashB is alive. Pure
    observation: the original runs unchanged. collect_swap_calls removes
    the wrap. `then_lua` runs in the SAME console chunk, after the wrap is
    in place — the Lua thread runs a chunk whole, so nothing it triggers
    can reach the execute unobserved."""
    send(port, " ".join((
        f"local L, V, A = {lashB}, {victimB}, {attackerB};",
        "local atk = require('scripts.unit_ai_combat_attack');",
        "local pol = require('scripts.unit_ai_mental').lashoutPolicy;",
        "local orig = atk.attackTargetExecute;",
        "local calls = {};",
        "_G.__probe_swap_calls = calls;",
        "_G.__probe_swap_restore = function() atk.attackTargetExecute = orig;",
        " _G.__probe_swap_restore = nil end;",
        "atk.attackTargetExecute = function(u, s, params)",
        " if u ~= L or not s then return orig(u, s, params) end;",
        " local rp = _G.__probe_orig_getPose or unit.getPose;",
        " local me = unit.getInfo(L); local la = unit.getLastAttacker(L);",
        " local now = engine.gameTime(); local d = -1;",
        " local ai = la and unit.getInfo(la.uid);",
        " if me and ai then d = math.max(math.abs(me.gridX-ai.gridX), math.abs(me.gridY-ai.gridY)) end;",
        " local rec = { g = now, pre = s.attackTargetUid or -1,",
        "  la = la and la.uid or -1, at = la and la.at or -1,",
        "  age = la and (now - (la.at or 0)) or -1, d = d,",
        "  reach = (unit.getAttackRange(L) or 1.0) + 0.5,",
        "  seen = tostring(la and unit.getPose(la.uid)),",
        "  real = tostring(la and rp(la.uid)),",
        "  victimElig = (me ~= nil) and pol.eligible(L, me, V) or false,",
        "  alive = rp(L) ~= 'dead' };",
        " local r = orig(u, s, params);",
        " rec.post = s.attackTargetUid or -1;",
        " if #calls < 200 then calls[#calls+1] = rec end;",
        " return r end;",
        then_lua,
        "return 'ok'")))


def collect_swap_calls(port):
    """Remove the attackTargetExecute wrap and answer the recorded calls."""
    raw = send(port, "local c = _G.__probe_swap_calls or {}; "
                     "if _G.__probe_swap_restore then _G.__probe_swap_restore() end; "
                     "_G.__probe_swap_calls = nil; "
                     "return #c == 0 and 'none' or c")
    try:
        calls = json.loads(raw)
    except (json.JSONDecodeError, TypeError):
        return []
    return calls if isinstance(calls, list) else []


def swap_exercised(calls, victimB, attackerB):
    """9c2's gate on the recorded lash-out attack executes (#2773).

    INPUT validity and POLICY outcome are kept apart. A VALID-INPUT call
    is one whose inputs make the retaliation swap reachable: target
    before the call is the victim, the victim is lash-out eligible,
    lashB's recorded last attacker is attackerB, hit no older than the
    swap's 3.0 game-second window, within the swap's reach
    (getAttackRange + 0.5), reading 'collapsed' while really not dead,
    with lashB alive. What the call then did (the target after it) is the
    policy outcome and is not part of that test.

    Answers (verdict, detail):
      * ('setup', closest misses) — no valid-input call, so the swap was
        never reachable and the check proves nothing;
      * ('policy', the offending call) — some valid-input call left a
        target other than the victim (attackerB, none, or anyone else);
        no other call can excuse it;
      * ('pass', a summary) — at least one valid-input call, and every
        one kept the victim.
    """
    if not calls:
        return "setup", "no lash-out attackTargetExecute call on the subject during the window"
    valid, best = [], None
    for c in calls:
        misses = []
        if c.get("pre") != victimB:
            misses.append(f"target {c.get('pre')} not victim {victimB}")
        if not c.get("victimElig"):
            misses.append("victim not lash-out eligible")
        if c.get("la") != attackerB:
            misses.append(f"last attacker {c.get('la')} not {attackerB}")
        if not (0 <= c.get("age", -1) <= 3.0):
            misses.append(f"hit age {c.get('age', -1):.2f} game-s outside 3.0")
        if not (0 <= c.get("d", -1) <= c.get("reach", 0)):
            misses.append(f"attacker at {c.get('d', -1):.2f} beyond reach {c.get('reach', 0):.2f}")
        if c.get("seen") != "collapsed":
            misses.append(f"attacker reads {c.get('seen')}, not collapsed")
        if c.get("real") == "dead":
            misses.append("attacker really dead")
        if not c.get("alive"):
            misses.append("subject dead")
        if misses:
            if best is None or len(misses) < len(best):
                best = misses
        else:
            valid.append(c)
    if not valid:
        return "setup", f"{len(calls)} call(s); closest missed: " + "; ".join(best)
    for c in valid:
        if c.get("post") != victimB:
            return "policy", (f"g={c['g']:.2f} target {c['pre']} -> {c.get('post')} "
                              f"with attacker {c['la']} at age {c['age']:.2f} "
                              f"d={c['d']:.2f}/{c['reach']:.2f} seen={c['seen']}")
    first = valid[0]
    return "pass", (f"{len(valid)} valid-input call(s), all kept {victimB}; "
                    f"first g={first['g']:.2f} attacker={first['la']} "
                    f"age={first['age']:.2f} d={first['d']:.2f}/{first['reach']:.2f} "
                    f"seen={first['seen']} real={first['real']}")

def main():
    ap = argparse.ArgumentParser()
    ap.add_argument("--port", type=int, default=9352)
    ap.add_argument("--lashout-case", choices=("stale", "ineligible"),
                    help="#2773 demonstration: run ONLY phase 9a, with its "
                         "setup pushed off the fair-test preconditions "
                         "(see perturb_lashout_case)")
    args = ap.parse_args()
    P = args.port

    proc = boot(P, log=LOG)
    ok = True
    try:
        bootstrap(P)

        if args.lashout_case:
            tune(P, EPISODE_MIN=LASHOUT_9AB_EPISODE, EPISODE_MAX=LASHOUT_9AB_EPISODE)
            graded_ok, _, _, _ = lashout_attacker_preference(
                P, case=args.lashout_case)
            print(f"\n{'PASS' if graded_ok else 'FAIL'} — phase 9a only "
                  f"(--lashout-case {args.lashout_case})")
            return 0 if graded_ok else 1

        # ---- 1. Fresh unit is mentally stable. ----
        uid = spawn_acolyte(P, 0, 0)
        print(f"spawned acolyte uid={uid}")
        if poll_until(5, lambda: mstate(P, uid) == "stable"):
            print("  [pass] fresh unit reads as stable")
        else:
            ok = False
            print(f"  [FAIL] fresh unit never read stable: {msummary(P, uid)}")

        # ---- 2. Stressed entry + the hysteresis dead band. ----
        # Suppress episode rolls entirely while exercising the label.
        tune(P, BREAK_CHANCE_MAX=0.0, EUPHORIA_CHANCE=0.0)

        set_wellbeing(P, uid, 0.05, 0.9)   # wellbeing ~0 -> som ~0
        if poll_until(5, lambda: mstate(P, uid) == "stressed"):
            print("  [pass] tanked wellbeing -> stressed")
        else:
            ok = False
            print(f"  [FAIL] never went stressed: {msummary(P, uid)}")

        # Dead band from the stressed side: som pinned at 0.40 (between
        # STRESSED_BELOW 0.35 and STRESS_RECOVER_AT 0.45) must NOT
        # recover. Re-pin each sample (mood drifts up on its own).
        flicker = []
        for _ in range(4):
            set_wellbeing(P, uid, 0.40, 0.0)
            time.sleep(0.4)
            flicker.append(mstate(P, uid))
        if all(s == "stressed" for s in flicker):
            print(f"  [pass] dead band (som=0.40) holds stressed: {flicker}")
        else:
            ok = False
            print(f"  [FAIL] stressed flickered inside the dead band: {flicker}")

        set_wellbeing(P, uid, 0.60, 0.0)
        if poll_until(5, lambda: mstate(P, uid) == "stable"):
            print("  [pass] som above 0.45 recovers to stable")
        else:
            ok = False
            print(f"  [FAIL] never recovered: {msummary(P, uid)}")

        # Dead band from the stable side: 0.40 must NOT (re-)stress.
        flicker = []
        for _ in range(4):
            set_wellbeing(P, uid, 0.40, 0.0)
            time.sleep(0.4)
            flicker.append(mstate(P, uid))
        if all(s == "stable" for s in flicker):
            print(f"  [pass] dead band (som=0.40) holds stable: {flicker}")
        else:
            ok = False
            print(f"  [FAIL] stable flickered inside the dead band: {flicker}")

        # ---- 3. Deterministic rolled break + duration + cooldown. ----
        tune(P, BREAK_CHANCE_MAX=1.0, SUSTAIN=2.0, CHECK_INTERVAL=0.5,
             EPISODE_MIN=6.0, EPISODE_MAX=6.0, COOLDOWN=60.0)
        set_wellbeing(P, uid, 0.0, 1.0)
        if poll_until(15, lambda: mstate(P, uid) == "break"):
            s = msummary(P, uid)
            print(f"  [pass] sustained stress rolled into a break: {s}")
        else:
            ok = False
            print(f"  [FAIL] never broke: {msummary(P, uid)}")

        # AI short-circuit: the dispatch loop must be on mental_break.
        if poll_until(5, lambda: send(
                P, f"return require('scripts.unit_ai').getState({uid})"
                   f".currentAction") == "mental_break"):
            print("  [pass] AI short-circuits on the break (currentAction=mental_break)")
        else:
            ok = False
            print("  [FAIL] AI never short-circuited on the break")

        # 6. Physiological guard, taken MID-break.
        gate = json.loads(send(P,
            f"local u={uid} local b=require('scripts.brain') "
            f"return {{pose=unit.getPose(u), uncon=b.isUnconscious(u), "
            f"delir=b.isDelirious(u), conf=b.isConfused(u), state=b.state(u)}}"))
        if (gate["pose"] == "standing" and not gate["uncon"]
                and not gate["delir"] and not gate["conf"]
                and gate["state"] == "alert"):
            print("  [pass] REGRESSION GUARD: mid-break the physiological ladder "
                  f"is untouched: {gate}")
        else:
            ok = False
            print(f"  [FAIL] break leaked into physiological gating: {gate}")

        # Event-log narration.
        evs = send(P, "return engine.getEventLog()")
        if "mental break" in evs:
            print("  [pass] event log narrates the break")
        else:
            ok = False
            print(f"  [FAIL] no break event in the log: {evs[:200]}")

        # Episode ends on its own (6s duration), back down the ladder —
        # wellbeing is still on the floor, so it lands on stressed.
        if poll_until(15, lambda: mstate(P, uid) == "stressed"):
            s = msummary(P, uid)
            print(f"  [pass] break ended by duration -> stressed; cooldown={s.get('cooldownUntil')}")
        else:
            ok = False
            print(f"  [FAIL] break never ended: {msummary(P, uid)}")

        # Cooldown: chance is still 1.0 and som still ~0, but no re-break.
        held = []
        for _ in range(6):
            time.sleep(0.5)
            held.append(mstate(P, uid))
        if "break" not in held:
            print(f"  [pass] cooldown blocks an immediate re-break: {held}")
        else:
            ok = False
            print(f"  [FAIL] re-broke inside the cooldown: {held}")

        # ---- 4a. Forced wander break moves the unit. ----
        set_wellbeing(P, uid, 1.0, 0.0)   # heal the ladder back up
        poll_until(5, lambda: mstate(P, uid) == "stable")
        send(P, f"require('scripts.mental_state').forceBreak({uid},'wander'); "
                f"return 'ok'")
        s = msummary(P, uid)
        x0, y0 = unit_pos(P, uid)
        moved = poll_until(12, lambda: (lambda x, y:
                (x - x0) ** 2 + (y - y0) ** 2 > 0.8)(*unit_pos(P, uid)))
        if s.get("state") == "break" and s.get("behavior") == "wander" and moved:
            print(f"  [pass] forced wander break moves the unit (from {x0:.1f},{y0:.1f})")
        else:
            ok = False
            print(f"  [FAIL] wander break didn't move: state={s} moved={bool(moved)}")
        send(P, f"unit.setStat({uid},'mental_until',0); return 'ok'")
        poll_until(5, lambda: mstate(P, uid) != "break")

        # ---- 4b. Forced flee break runs from the nearest unit. ----
        # A technomule stands still (no stamina stat -> wander self-
        # disables), so the distance read isn't confounded by its AI.
        # Top the acolyte's stamina back up first — the flee runs at
        # ordered speed, which scales with stamina, and the unit has
        # been walking all probe. (NEVER to 0 or a fresh full-set here:
        # stamina == 0 is the engine's universal death rule.)
        send(P, f"local st=require('scripts.unit_stats') "
                f"unit.setStat({uid},'stamina', st.get({uid},'max_stamina')*0.9); "
                f"return 'ok'")
        mule = spawn_acolyte(P, 6, 0, unit="technomule", clear_water=False)
        send(P, f"require('scripts.mental_state').forceBreak({uid},'flee'); "
                f"return 'ok'")
        s = msummary(P, uid)
        mx, my = unit_pos(P, mule)
        x0, y0 = unit_pos(P, uid)
        d0 = ((x0 - mx) ** 2 + (y0 - my) ** 2) ** 0.5

        def fled():
            x, y = unit_pos(P, uid)
            return ((x - mx) ** 2 + (y - my) ** 2) ** 0.5 > d0 + 2.0
        away = poll_until(25, fled)
        if s.get("state") == "break" and s.get("behavior") == "flee" and away:
            print(f"  [pass] forced flee break runs from the technomule (d0={d0:.1f})")
        else:
            ok = False
            print(f"  [FAIL] flee break didn't flee: state={s} d0={d0:.1f} "
                  f"now={unit_pos(P, uid)}")
        send(P, f"unit.setStat({uid},'mental_until',0); return 'ok'")
        poll_until(5, lambda: mstate(P, uid) != "break")

        # ---- 4c. Entering a break PREEMPTS active work (onExit fires).
        # Plant a real in-progress construct_job phase machine and force
        # the break in the SAME console line — the Lua thread serialises
        # console commands against script updates, so no AI tick can
        # interleave, and once the break is set every subsequent tick
        # short-circuits before scoring. The ONLY path that can demote
        # the planted "building" phase is the short-circuit's preempt
        # calling constructOnExit (building -> walking, the accumulator
        # reset that stops a 60-120 s break landing as instant progress).
        send(P, f"local ai=require('scripts.unit_ai') "
                f"local s=ai.getState({uid}) "
                f"s.currentAction='construct_job' "
                f"s.constructJob={{phase='building', work=10, progress=0}} "
                f"require('scripts.mental_state').forceBreak({uid},'wander'); "
                f"return 'ok'")

        def preempted():
            d = json.loads(send(
                P, f"local s=require('scripts.unit_ai').getState({uid}) "
                   f"return {{ca=s.currentAction, "
                   f"ph=s.constructJob and s.constructJob.phase}}"))
            return d if (d.get("ca") == "mental_break"
                         and d.get("ph") == "walking") else None
        if poll_until(8, preempted):
            print("  [pass] break preempts active work: constructOnExit fired "
                  "(phase building -> walking)")
        else:
            d = json.loads(send(
                P, f"local s=require('scripts.unit_ai').getState({uid}) "
                   f"return {{ca=s.currentAction, "
                   f"ph=s.constructJob and s.constructJob.phase}}"))
            ok = False
            print(f"  [FAIL] break didn't preempt the running action: {d}")
        send(P, f"local s=require('scripts.unit_ai').getState({uid}) "
                f"s.constructJob=nil unit.setStat({uid},'mental_until',0); "
                f"return 'ok'")
        poll_until(5, lambda: mstate(P, uid) != "break")

        # ---- 5. Euphoria: entry, concentration bonus, exit band. ----
        tune(P, EUPHORIA_CHANCE=1.0, BREAK_CHANCE_MAX=0.0)
        # Drain stamina so the concentration base sits well below the
        # 1.0 clamp and the +bonus is visible. LOW, never 0: stamina
        # == 0 is the engine's universal death rule (unit_resource_
        # tick.lua), which would silently freeze every check below.
        send(P, f"local st=require('scripts.unit_stats') "
                f"unit.setStat({uid},'stamina', st.get({uid},'max_stamina')*0.25); "
                f"return 'ok'")
        send(P, f"unit.setStat({uid},'mental_cooldown_until',0); return 'ok'")
        set_wellbeing(P, uid, 1.0, 0.0)

        def euphoric():
            set_wellbeing(P, uid, 1.0, 0.0)   # hold som >= 0.90 through SUSTAIN
            return mstate(P, uid) == "euphoric"
        if poll_until(15, euphoric):
            print("  [pass] sustained near-content som entered euphoria")
        else:
            ok = False
            print(f"  [FAIL] never euphoric: {msummary(P, uid)}")

        # Concentration = base + bonus, where base is recomputed each
        # tick from live stamina — read both in one console round trip.
        probe = json.loads(send(P,
            f"local u={uid} local b=require('scripts.brain') "
            f"local sf=b.staminaFrac(u) "
            f"return {{conc=b.concentration(u), sf=sf, pain=b.painFrac(u)}}"))
        base = (1.0 - 0.6 * probe["pain"]) * (0.4 + 0.6 * probe["sf"])
        expected = min(1.0, base + 0.10)
        if abs(probe["conc"] - expected) <= 0.06:
            print(f"  [pass] euphoric concentration ~{expected:.3f} "
                  f"(base {base:.3f} + 0.10 bonus): {probe['conc']:.3f}")
        else:
            ok = False
            print(f"  [FAIL] concentration {probe['conc']:.3f} != "
                  f"expected ~{expected:.3f} (base {base:.3f})")

        # Exit dead band: som pinned at 0.85 (between EUPHORIC_EXIT 0.80
        # and EUPHORIC_ABOVE 0.90) must hold the episode.
        flicker = []
        for _ in range(4):
            set_wellbeing(P, uid, 0.85, 0.0)
            time.sleep(0.4)
            flicker.append(mstate(P, uid))
        if all(s == "euphoric" for s in flicker):
            print(f"  [pass] euphoria dead band (som=0.85) holds: {flicker}")
        else:
            ok = False
            print(f"  [FAIL] euphoria flickered inside the dead band: {flicker}")

        set_wellbeing(P, uid, 0.5, 0.0)
        if poll_until(5, lambda: mstate(P, uid) == "stable"):
            print("  [pass] som below 0.80 exits euphoria early")
        else:
            ok = False
            print(f"  [FAIL] euphoria never exited: {msummary(P, uid)}")

        # ---- 7. Deterministic break-behaviour roll (#717): exact
        # 35/35/15/15 boundaries, no statistical sampling. ----
        def roll(draw):
            return send(P, f"return require('scripts.mental_state')"
                           f".rollBehavior({draw})")

        draws  = [0.0, 0.349999, 0.35, 0.699999, 0.70, 0.849999, 0.85, 0.999999]
        expect = ["0", "0",      "1",  "1",      "2",  "2",      "3",  "3"]
        got = [roll(d) for d in draws]
        if got == expect:
            print(f"  [pass] break-behaviour roll hits the 35/35/15/15 "
                  f"boundaries: {list(zip(draws, got))}")
        else:
            ok = False
            print(f"  [FAIL] roll boundaries wrong: draws={draws} got={got} "
                  f"want={expect}")

        # ---- 8. Forced catatonia (#717): stops an already-moving unit,
        # no displacement/replacement action for the episode, standing +
        # lucid throughout, exits through the normal cooldown path. ----
        set_wellbeing(P, uid, 1.0, 0.0)
        poll_until(5, lambda: mstate(P, uid) == "stable")
        cx, cy = unit_pos(P, uid)
        send(P, f"require('scripts.unit_ai').commandMove({uid}, {cx + 20}, {cy}); "
                f"return 'ok'")
        if not poll_until(5, lambda: send(P, f"return unit.getActivity({uid})")
                          in ("walking", "running")):
            ok = False
            print("  [FAIL] setup: unit never started moving before the catatonia check")

        send(P, f"require('scripts.mental_state').forceBreak({uid},'catatonia'); "
                f"return 'ok'")
        s = msummary(P, uid)
        stopped = poll_until(5, lambda: send(P, f"return unit.getActivity({uid})")
                             not in ("walking", "running"))
        if s.get("state") == "break" and s.get("behavior") == "catatonia" and stopped:
            print("  [pass] forced catatonia stops the moving unit")
        else:
            ok = False
            print(f"  [FAIL] catatonia didn't stop the unit: state={s} stopped={bool(stopped)}")

        x0, y0 = unit_pos(P, uid)
        time.sleep(3.0)
        x1, y1 = unit_pos(P, uid)
        no_disp = (x1 - x0) ** 2 + (y1 - y0) ** 2 < 0.05
        gate = json.loads(send(P,
            f"local u={uid} local b=require('scripts.brain') "
            f"return {{pose=unit.getPose(u), activity=unit.getActivity(u), "
            f"uncon=b.isUnconscious(u), delir=b.isDelirious(u)}}"))
        if (no_disp and gate["pose"] == "standing"
                and gate["activity"] not in ("walking", "running")
                and not gate["uncon"] and not gate["delir"]):
            print(f"  [pass] catatonia holds position, standing, physiologically "
                  f"lucid: {gate}")
        else:
            ok = False
            print(f"  [FAIL] catatonia leaked movement or physiological state: "
                  f"disp=({x1 - x0:.2f},{y1 - y0:.2f}) gate={gate}")

        # Narration distinguishable from wander/flee (#717 requirement 7).
        evs = send(P, "return engine.getEventLog()")
        if "catatonic mental break" in evs:
            print("  [pass] catatonia narrates distinctly in the event log")
        else:
            ok = False
            print(f"  [FAIL] no catatonia-specific narration in the log: {evs[-400:]}")

        send(P, f"unit.setStat({uid},'mental_until',0); return 'ok'")
        if poll_until(5, lambda: mstate(P, uid) != "break"):
            print("  [pass] catatonia exits through the normal cooldown path")
        else:
            ok = False
            print("  [FAIL] catatonia episode never ended")

        # ---- 8b. A break entered MID-POSE-TRANSITION (#1709). The walk
        # setup above is deliberately left intact as its own scenario:
        # it proves immediate preemption, which is what must NOT change.
        # This one proves the other half — a leap that the break must not
        # cancel partway through, leaving the unit rendered above its
        # grid layer for the rest of the episode (and saved that way).
        #
        # An episode long enough to outlast the whole arc plus the
        # samples: stage 3 pinned it at 6 s, and once the episode ends
        # the AI resumes and the first walk step rewrites usRealZ
        # (PathAdvance.hs), which would turn the defect into a pass.
        # Restored afterwards — stage 9 relies on the short duration.
        tune(P, EPISODE_MIN=60.0, EPISODE_MAX=60.0)
        # Acolyte stats are ROLLED per spawn and leap reach is derived
        # from agility/strength/fat (jumpMaxTiles), so pin the one input
        # that decides whether a one-tile leap is even accepted. uid is
        # not used past this stage.
        send(P, f"unit.setStat({uid},'agility',2.0); return 'ok'")

        for leg, what in (("airborne", "the airborne arc"),
                          ("landing", "the chained landing step")):
            send(P, f"unit.setStat({uid},'mental_until',0); return 'ok'")
            poll_until(5, lambda: mstate(P, uid) != "break")
            detail, samples = leap_break_case(P, leg=leg, uid=uid)
            if detail is None:
                print(f"  [pass] catatonia forced during {what} lands the "
                      f"unit grounded and standing ({len(samples)} samples)")
            else:
                ok = False
                print(f"  [FAIL] catatonia during {what}: {detail}")

        send(P, f"unit.setStat({uid},'mental_until',0); return 'ok'")
        poll_until(5, lambda: mstate(P, uid) != "break")
        tune(P, EPISODE_MIN=6.0, EPISODE_MAX=6.0)

        # ---- 9. Lash-out (#717): target policy, exclusions, target
        # loss/replacement, no-target wander, real attack behavior, and
        # episode-end cleanup. Each scenario uses a fresh, isolated unit
        # cluster (>8 tiles from every other cluster) so LASHOUT_RANGE
        # never bridges them.

        # 9a/9b run under their own, longer episode (LASHOUT_9AB_EPISODE),
        # restored to phase 9's 6 s on EVERY path before 9c.
        tune(P, EPISODE_MIN=LASHOUT_9AB_EPISODE, EPISODE_MAX=LASHOUT_9AB_EPISODE)
        try:
            # 9a. Prefers a recent eligible attacker over a closer decoy —
            # graded only on a selection whose preconditions held at the
            # moment it was made (#2773). See lashout_attacker_preference.
            graded_ok, lash, attacker, decoy = lashout_attacker_preference(
                P, case=args.lashout_case)
            if not graded_ok:
                ok = False
            if lash is not None and not dress_staged_wound(P, lash):
                ok = False

            # 9b. Produces real attack behavior through the short-circuit —
            # confirm lash actually lands a swing on its chosen target.
            landed = poll_until(20, lambda: send(
                P, f"local a=unit.getLastAttacker({attacker}); "
                   f"return a and a.uid or 'nil'"
            ) == str(lash))
            if landed:
                print(f"  [pass] lash-out produced a real landed attack on {attacker}")
            else:
                ok = False
                print(f"  [FAIL] lash-out never landed an attack on {attacker}")

            # Narration distinguishable from wander/flee (#717 requirement 7).
            evs = send(P, "return engine.getEventLog()")
            if "violent mental break" in evs:
                print("  [pass] lash-out narrates distinctly in the event log")
            else:
                ok = False
                print(f"  [FAIL] no lash-out-specific narration in the log: {evs[-400:]}")

            # Done with lash/attacker/decoy — end lash's episode; no further
            # assertions need them (the dedicated cleanup test below uses its
            # own isolated pair, see the note there for why).
            send(P, f"unit.setStat({lash},'mental_until',0); return 'ok'")
            poll_until(5, lambda: mstate(P, lash) != "break")
            # …and remove all three (#2773). Nothing pins them in place any
            # more — the attacker's stamina is no longer drained, and the
            # decoy never was — so left alive they wander into the isolated
            # clusters the later scenarios stage, and turn up there as
            # unplanned lash-out candidates.
            for u in (lash, attacker, decoy):
                if u is not None:
                    send(P, f"unit.destroy({u}); return 'ok'")
        finally:
            tune(P, EPISODE_MIN=6.0, EPISODE_MAX=6.0)

        # 9c. Episode end clears every lash-out-owned goal/target.
        # Isolated from ordinary re-engagement: attacker's original staged
        # hit is still within the ordinary combat system's 10s incoming_
        # hit engage window, so ending the mental episode against THAT
        # pair also lets ordinary AI legitimately re-pick attacker via
        # engage/attack_target moments later — externally indistinguishable
        # from a leak (both converge on the same tgt/goal). Pin lashC's own
        # unit.getLastAttacker to nil instead, so once the episode ends,
        # ordinary combat AI has no incoming-hit reason to re-target
        # whoever lash-out was fighting, isolating what this check assert on.
        lashC = spawn_acolyte(P, 10, 40)
        set_wellbeing(P, lashC, 1.0, 0.0)
        poll_until(5, lambda: mstate(P, lashC) == "stable")
        targetC = spawn_acolyte(P, 11, 40)
        # Neither side needs to land a meaningful blow here (only the
        # goal/target bookkeeping matters), so keep this pair non-lethal
        # too — same rationale as the 9a setup above.
        send(P, f"unit.setStat({lashC},'strength',0.05); "
                f"unit.setStat({lashC},'toughness',100); "
                f"unit.setStat({targetC},'strength',0.05); "
                f"unit.setStat({targetC},'toughness',100); return 'ok'")

        send(P, f"if not _G.__probe_orig_getLastAttacker then "
                f"_G.__probe_orig_getLastAttacker = unit.getLastAttacker end; "
                f"unit.getLastAttacker = function(u) "
                f"if u == {lashC} then return nil end "
                f"return _G.__probe_orig_getLastAttacker(u) end; return 'ok'")

        send(P, f"require('scripts.mental_state').forceBreak({lashC},'lash_out'); "
                f"return 'ok'")

        def lashC_target():
            t = lash_target(P, lashC)
            return t if t != "nil" else None
        gotC = poll_until(10, lashC_target)
        if gotC != str(targetC):
            ok = False
            print(f"  [FAIL] setup: lashC never targeted {targetC} (got {gotC})")

        send(P, f"unit.setStat({lashC},'mental_until',0); return 'ok'")

        def clearedC():
            f = lash_ai_flags(P, lashC)
            return f if (not f["tgt"] and f["goal"] != "attack"
                         and not f["committed"]) else None
        cleared = poll_until(6, clearedC)
        send(P, "if _G.__probe_orig_getLastAttacker then "
                "unit.getLastAttacker = _G.__probe_orig_getLastAttacker; "
                "_G.__probe_orig_getLastAttacker = nil end; return 'ok'")
        if cleared:
            print(f"  [pass] episode end cleared lash-out combat state: {cleared}")
        else:
            ok = False
            print(f"  [FAIL] lash-out combat state leaked past episode end: "
                  f"{lash_ai_flags(P, lashC)}")
        poll_until(5, lambda: mstate(P, lashC) != "break")

        # 9c2. The shared attack_target retaliation-swap (unit_ai_combat_
        # attack.lua's mid-fight retarget) must not hand lash-out a
        # COLLAPSED recent attacker — it only excludes "dead" on its own,
        # so a collapsed unit that hit us moments ago and stands in
        # melee range could otherwise bypass lash-out's own eligibility
        # policy via that shared path.
        lashB = spawn_acolyte(P, 0, 20)
        set_wellbeing(P, lashB, 1.0, 0.0)
        poll_until(5, lambda: mstate(P, lashB) == "stable")
        victimB = spawn_acolyte(P, 1, 20)     # eligible target, stays live
        attackerB = spawn_acolyte(P, -1, 20)  # becomes the disqualified
                                               # "recent attacker"
        # See the neutered-strength note in the 9a setup above — same
        # one-shot-kill risk applies here, in both directions, and the
        # same fix (#2773): strength_base + unit.recomputeBody, because a
        # 'strength' write is re-derived away before the hit (a traced run
        # had this staged hit bleed lashB out before the break).
        send(P, f"unit.setStat({attackerB},'strength_base',{NEUTERED_STRENGTH_BASE}); "
                f"unit.setStat({attackerB},'toughness',100); "
                f"unit.recomputeBody({attackerB}); "
                f"unit.setStat({lashB},'strength_base',{NEUTERED_STRENGTH_BASE}); "
                f"unit.setStat({lashB},'toughness',100); "
                f"unit.recomputeBody({lashB}); return 'ok'")
        send(P, f"require('scripts.unit_ai').commandAttack({attackerB},{lashB}); "
                f"return 'ok'")
        hitB = poll_until(35, lambda: send(
            P, f"local a=unit.getLastAttacker({lashB}); return a and a.uid or 'nil'"
        ) == str(attackerB))
        if not hitB:
            ok = False
            print(f"  [FAIL] setup: {attackerB} never landed a hit on {lashB} "
                  f"— can't test the retaliation-swap exclusion")

        # The swap this check exercises only lives within unit_ai_combat's
        # 3 s RETALIATE_WINDOW_SEC of the hit, so everything between the
        # hit and the break is ONE console chunk here and ONE below
        # (#2773): a traced run spent 3.4 s on separate round trips and
        # reached lash-out with the hit already 3.49 s old, so the swap
        # was never reachable.
        #
        # Chunk 1, sent as soon as the hit is observed:
        # * stop BOTH sides' ordinary combat AI — see the identical note
        #   in the 9a setup above; this test wants attackerB's REPORTED
        #   pose patched to 'collapsed' below while it stays actually
        #   healthy underneath, and real mutual combat risks genuinely
        #   collapsing lashB instead;
        # * revive both, and drain attackerB's stamina below the acolyte
        #   config's wander_min_stamina_fraction (0.2 — never to 0, the
        #   universal death rule) so revival doesn't send it drifting off
        #   before the teleport below is confirmed;
        # * snap attackerB one tile east of lashB and victimB one tile
        #   west, from lashB's position read IN this chunk. Knockback can
        #   drift the attacker beyond LASHOUT_RANGE, and the subject can
        #   wander or run during the hit staging, so neither spawn spot
        #   says anything about range when the break is forced (a traced
        #   run had the victim 19 tiles away).
        # It answers lashB's position and the hit record, for diagnostics.
        # victimB's goals and state are not touched.
        stagedB = send_json(P, " ".join((
            f"local A, L, V = {attackerB}, {lashB}, {victimB};",
            "local ai = require('scripts.unit_ai');",
            "for _, u in ipairs({A, L}) do local s = ai.getState(u);",
            " if s then ai.markGoalAccomplished(s, 'attack'); s.attackTargetUid = nil end;",
            " unit.stop(u) end;",
            "unit.revive(A); unit.revive(L);",
            "local st = require('scripts.unit_stats');",
            "unit.setStat(A, 'stamina', st.get(A, 'max_stamina') * 0.1);",
            "local i = unit.getInfo(L); local h = unit.getLastAttacker(L);",
            "unit.setPos(A, i.gridX + 1, i.gridY); unit.setPos(V, i.gridX - 1, i.gridY);",
            "return { lx = i.gridX, ly = i.gridY, g = engine.gameTime(),",
            " hitBy = h and h.uid or -1, hitAt = h and h.at or -1 }")))
        if not isinstance(stagedB, dict):
            stagedB = {}
        lbx, lby = stagedB.get("lx", 0.0), stagedB.get("ly", 0.0)
        print(f"  [setup] 9c2 staged after the hit: {stagedB}")
        # Confirm the (async) teleports actually landed — see the 9a note —
        # with ONE console read per poll returning all three positions.
        def landedB():
            pos = send_json(P, " ".join((
                f"local a, v, l = unit.getInfo({attackerB}), unit.getInfo({victimB}), "
                f"unit.getInfo({lashB});",
                "return { ax = a.gridX, ay = a.gridY, vx = v.gridX, vy = v.gridY,",
                " lx = l.gridX, ly = l.gridY }")))
            if not isinstance(pos, dict):
                return False
            return ((pos["ax"] - (lbx + 1)) ** 2 + (pos["ay"] - lby) ** 2 < 0.05
                    and (pos["vx"] - (lbx - 1)) ** 2 + (pos["vy"] - lby) ** 2 < 0.05)
        if not poll_until(5, landedB):
            ok = False
            print(f"  [FAIL] setup: teleporting {attackerB} and {victimB} "
                  f"next to {lashB} never took effect")

        # Chunk 2: pin attackerB's reported pose to 'collapsed', observe
        # every lash-out attack execute of lashB through the window
        # (#2773; see install_swap_observer), and force the break — all in
        # ONE send, so the first execute is seen. The pose pin is a
        # wrap-and-delegate because unit.collapse() alone only holds while
        # every gating resource sits below its revive threshold (see the
        # dead/collapsed/technomule test below).
        install_swap_observer(
            P, lashB, victimB, attackerB,
            then_lua=(f"if not _G.__probe_orig_getPose then "
                      f"_G.__probe_orig_getPose = unit.getPose end; "
                      f"unit.getPose = function(u) "
                      f"if u == {attackerB} then return 'collapsed' end "
                      f"return _G.__probe_orig_getPose(u) end; "
                      f"require('scripts.mental_state').forceBreak({lashB},'lash_out');"))

        # Sample rapidly through the retaliation-swap's own 3s window
        # (RETALIATE_WINDOW_SEC, timed from the hit staged above) —
        # lash-out must land on the eligible victimB and never once show
        # the collapsed attackerB sneaking in via the shared swap.
        samplesB = []
        try:
            deadline = time.time() + 3.0
            while time.time() < deadline:
                samplesB.append(lash_target(P, lashB))
                time.sleep(0.15)
        finally:
            callsB = collect_swap_calls(P)
        send(P, "if _G.__probe_orig_getPose then "
                "unit.getPose = _G.__probe_orig_getPose; "
                "_G.__probe_orig_getPose = nil end; return 'ok'")

        if str(victimB) in samplesB and str(attackerB) not in samplesB:
            print(f"  [pass] lash-out targeted the eligible {victimB} and never "
                  f"the collapsed recent attacker {attackerB}: "
                  f"{samplesB[:4]}...")
        else:
            ok = False
            print(f"  [FAIL] expected only {victimB}, saw: {samplesB}")
        # The check above only means something if the swap it guards was
        # actually reachable during the window (#2773).
        verdictB, whyB = swap_exercised(callsB, victimB, attackerB)
        if verdictB == "pass":
            print(f"  [setup] 9c2 swap precondition exercised: {whyB}")
        elif verdictB == "policy":
            ok = False
            print(f"  [FAIL] 9c2 retaliation swap moved lash-out off the "
                  f"eligible victim {victimB}: {whyB}")
        else:
            ok = False
            print(f"  [FAIL] setup: 9c2 swap precondition not exercised ({whyB})")

        send(P, f"unit.setStat({lashB},'mental_until',0); return 'ok'")
        poll_until(5, lambda: mstate(P, lashB) != "break")

        # 9d. No eligible attacker: nearest eligible unit (an ally — every
        # spawned acolyte shares one faction, so this doubles as the
        # ally-targeting check).
        subj2 = spawn_acolyte(P, -15, -25)
        set_wellbeing(P, subj2, 1.0, 0.0)
        poll_until(5, lambda: mstate(P, subj2) == "stable")
        near_ally = spawn_acolyte(P, -14, -25)
        far_ally  = spawn_acolyte(P, -10, -25)
        send(P, f"require('scripts.mental_state').forceBreak({subj2},'lash_out'); "
                f"return 'ok'")

        def t2_target():
            t = lash_target(P, subj2)
            return t if t != "nil" else None
        target2 = poll_until(10, t2_target)
        if target2 == str(near_ally):
            print(f"  [pass] no eligible attacker -> nearest eligible ally "
                  f"{near_ally} (over farther {far_ally})")
        else:
            ok = False
            print(f"  [FAIL] expected nearest ally {near_ally}, got "
                  f"target={target2} (far_ally={far_ally})")

        # Clean up: near_ally/far_ally can drift under ordinary AI
        # (wander, retreat from subj2's lash-out) into later clusters'
        # LASHOUT_RANGE — see the identical note after 9f below. Kill
        # them and end subj2's episode so nothing from this cluster
        # wanders further.
        send(P, f"unit.kill({near_ally}); unit.kill({far_ally}); "
                f"unit.setStat({subj2},'mental_until',0); return 'ok'")
        poll_until(5, lambda: mstate(P, subj2) != "break")

        # 9e. Dead, collapsed, self, and the technomule are excluded —
        # all placed CLOSER than the one live unit, which must still win.
        subj3 = spawn_acolyte(P, 0, -25)
        set_wellbeing(P, subj3, 1.0, 0.0)
        poll_until(5, lambda: mstate(P, subj3) == "stable")
        mule3 = spawn_acolyte(P, 0.2, -25, unit="technomule", clear_water=False)
        dead3 = spawn_acolyte(P, 0.3, -25)
        send(P, f"unit.kill({dead3}); return 'ok'")
        poll_until(5, lambda: send(P, f"return unit.getPose({dead3})") == "dead")
        collapsed3 = spawn_acolyte(P, 0.4, -25)
        # unit.collapse() only holds while every gating resource sits below
        # its own revive threshold (Test.Headless / unit_resource_tick.lua
        # checkRevive) — a healthy fresh spawn auto-revives within a tick
        # or two, well before the target-policy check below runs. Pin
        # unit.getPose's report for this one uid instead, so the exclusion
        # is exercised deterministically regardless of the physiology sim's
        # timing — the same wrap-and-delegate technique movement_probe.py
        # uses to neutralise unit_ai's wander tick.
        send(P, f"if not _G.__probe_orig_getPose then "
                f"_G.__probe_orig_getPose = unit.getPose end; "
                f"unit.getPose = function(u) "
                f"if u == {collapsed3} then return 'collapsed' end "
                f"return _G.__probe_orig_getPose(u) end; return 'ok'")
        live3 = spawn_acolyte(P, 4, -25)
        send(P, f"require('scripts.mental_state').forceBreak({subj3},'lash_out'); "
                f"return 'ok'")

        def t3_target():
            t = lash_target(P, subj3)
            return t if t != "nil" else None
        target3 = poll_until(10, t3_target)
        send(P, "if _G.__probe_orig_getPose then "
                "unit.getPose = _G.__probe_orig_getPose; "
                "_G.__probe_orig_getPose = nil end; return 'ok'")
        if target3 == str(live3):
            print(f"  [pass] dead/collapsed/technomule excluded -> live unit "
                  f"{live3} chosen (mule={mule3} dead={dead3} "
                  f"collapsed={collapsed3})")
        else:
            ok = False
            print(f"  [FAIL] expected live unit {live3}, got target={target3}")

        # 9f. A lost target (dies mid-episode) is replaced with another
        # eligible unit.
        subj4 = spawn_acolyte(P, 15, -25)
        set_wellbeing(P, subj4, 1.0, 0.0)
        poll_until(5, lambda: mstate(P, subj4) == "stable")
        t1 = spawn_acolyte(P, 16, -25)
        t2 = spawn_acolyte(P, 20, -25)
        send(P, f"require('scripts.mental_state').forceBreak({subj4},'lash_out'); "
                f"return 'ok'")

        def t4_first():
            t = lash_target(P, subj4)
            return t if t != "nil" else None
        first = poll_until(10, t4_first)
        if first == str(t1):
            print(f"  [pass] lash-out picked the nearest target {t1} first")
        else:
            ok = False
            print(f"  [FAIL] lash-out never targeted {t1} first (got {first})")

        send(P, f"unit.kill({t1}); return 'ok'")

        def t4_replaced():
            t = lash_target(P, subj4)
            return t if t == str(t2) else None
        replaced = poll_until(10, t4_replaced)
        if replaced:
            print(f"  [pass] lost target {t1} replaced with remaining eligible "
                  f"unit {t2}")
        else:
            ok = False
            print(f"  [FAIL] lash-out never replaced lost target {t1} with "
                  f"{t2} (current={lash_target(P, subj4)})")

        # Clean up: t2 flees subj4 under ordinary retreat mechanics (up to
        # RETREAT_SAFE_DIST=12 tiles), which could otherwise drift it into
        # the "isolated" unit's LASHOUT_RANGE below. Kill it and end
        # subj4's episode so nothing from this cluster wanders further.
        send(P, f"unit.kill({t2}); unit.setStat({subj4},'mental_until',0); "
                f"return 'ok'")
        poll_until(5, lambda: mstate(P, subj4) != "break")

        # 9h. Nearest-target ranking uses Chebyshev distance — matching
        # eligibility's own metric — not squared Euclidean. (6,6) is
        # nearer than (8,0) under Chebyshev (6 vs 8) despite (8,0) being
        # nearer under squared Euclidean (64 vs 72).
        subjH = spawn_acolyte(P, -15, 40)
        set_wellbeing(P, subjH, 1.0, 0.0)
        poll_until(5, lambda: mstate(P, subjH) == "stable")
        nearChebyshev = spawn_acolyte(P, -9, 46)   # +6,+6
        nearEuclidean = spawn_acolyte(P, -7, 40)   # +8,0
        send(P, f"require('scripts.mental_state').forceBreak({subjH},'lash_out'); "
                f"return 'ok'")

        def h_target():
            t = lash_target(P, subjH)
            return t if t != "nil" else None
        targetH = poll_until(10, h_target)
        if targetH == str(nearChebyshev):
            print(f"  [pass] nearest-target ranking uses Chebyshev distance "
                  f"({nearChebyshev} over the Euclidean-nearer {nearEuclidean})")
        else:
            ok = False
            print(f"  [FAIL] expected Chebyshev-nearer {nearChebyshev}, got "
                  f"{targetH} (Euclidean-nearer would be {nearEuclidean})")
        send(P, f"unit.setStat({subjH},'mental_until',0); return 'ok'")
        poll_until(5, lambda: mstate(P, subjH) != "break")

        # 9i. A lost target with no replacement stops the stale pursuit
        # immediately rather than walking out the old leg toward the
        # target's vacated position.
        subjI = spawn_acolyte(P, -25, 10)
        set_wellbeing(P, subjI, 1.0, 0.0)
        poll_until(5, lambda: mstate(P, subjI) == "stable")
        targetI = spawn_acolyte(P, -19, 10)   # 6 tiles off — needs a real walk
        send(P, f"require('scripts.mental_state').forceBreak({subjI},'lash_out'); "
                f"return 'ok'")

        def i_pursuing():
            t = lash_target(P, subjI)
            act = send(P, f"return unit.getActivity({subjI})")
            return True if (t == str(targetI) and act in ("walking", "running")) else None
        if not poll_until(10, i_pursuing):
            ok = False
            print(f"  [FAIL] setup: {subjI} never pursued {targetI}")

        # lashOutExecute runs every world tick (~60Hz, well before the
        # thought_interval cadence gate), so the single tick where
        # unit.stop() takes hold and getActivity briefly reads "idle" is
        # far too narrow a window for external polling (0.1-0.3s
        # intervals) to reliably observe directly. Assert the outcome
        # that actually distinguishes stopped-and-rewandered from
        # walked-out-the-old-leg instead: how much of the ~6-tile gap to
        # the target's ORIGINAL position closes after it's teleported
        # away. Continuing the stale pursuit would close most of it
        # (~2 tiles/s at ordered speed); stopping and wandering
        # independently should close only a small, bounded amount.
        old_target_x, old_target_y = -19, 10
        x0, y0 = unit_pos(P, subjI)
        d_to_old_start = ((x0 - old_target_x) ** 2 + (y0 - old_target_y) ** 2) ** 0.5

        # Teleport the target out of LASHOUT_RANGE (8) mid-pursuit.
        send(P, f"unit.setPos({targetI}, -19, 25); return 'ok'")
        poll_until(5, lambda: (lambda x, y:
                (x - (-19)) ** 2 + (y - 25) ** 2 < 0.25)(*unit_pos(P, targetI)))

        time.sleep(2.0)
        x1, y1 = unit_pos(P, subjI)
        d_to_old_end = ((x1 - old_target_x) ** 2 + (y1 - old_target_y) ** 2) ** 0.5
        closed_gap = d_to_old_start - d_to_old_end
        cleared_i = lash_target(P, subjI) == "nil"
        if cleared_i and closed_gap < 2.0:
            print(f"  [pass] lost target with no replacement stops the stale "
                  f"pursuit (closed only {closed_gap:.2f} tiles of the old "
                  f"gap in 2s, target cleared)")
        else:
            ok = False
            print(f"  [FAIL] stale pursuit continued toward the target's old "
                  f"position: closed_gap={closed_gap:.2f} cleared={cleared_i}")
        send(P, f"unit.setStat({subjI},'mental_until',0); return 'ok'")
        poll_until(5, lambda: mstate(P, subjI) != "break")

        # 9j. Delirium overlapping episode expiry must not skip
        # episode-end cleanup. Cleanup is tracked via s.mentalLashoutActive
        # rather than inferred from s.currentAction, which delirium
        # preempts to "delirious" the instant it overlaps a break.
        subjJ = spawn_acolyte(P, -25, -10)
        set_wellbeing(P, subjJ, 1.0, 0.0)
        poll_until(5, lambda: mstate(P, subjJ) == "stable")
        targetJ = spawn_acolyte(P, -19, -10)   # 6 tiles off — a real pursuit,
                                                # not already-in-melee-range
        send(P, f"require('scripts.mental_state').forceBreak({subjJ},'lash_out'); "
                f"return 'ok'")

        def j_pursuing():
            t = lash_target(P, subjJ)
            act = send(P, f"return unit.getActivity({subjJ})")
            return True if (t == str(targetJ) and act in ("walking", "running")) else None
        if not poll_until(10, j_pursuing):
            ok = False
            print(f"  [FAIL] setup: {subjJ} never pursued {targetJ}")

        jx0, jy0 = unit_pos(P, subjJ)
        d_to_target_start = ((jx0 - (-19)) ** 2 + (jy0 - (-10)) ** 2) ** 0.5

        # Drive consciousness into the delirious band (brain.lua:
        # 0.15..0.40) via blood_oxygen — the same technique
        # collapse_crawl_probe.py uses. brain.tick recomputes
        # consciousness from blood_oxygen every physiology tick and
        # blood_oxygen itself drifts back on its own, so this needs
        # RE-pinning on every poll (like set_wellbeing's own dead-band
        # samples elsewhere in this file) to actually hold the unit
        # delirious through the whole window this test needs, rather
        # than recovering before the overlap it's testing occurs.
        def j_pin_and_check_delirious():
            send(P, f"unit.setStat({subjJ},'blood_oxygen',0.59); return 'ok'")
            return json.loads(send(P,
                f"local b=require('scripts.brain'); "
                f"return {{d=b.isDelirious({subjJ})}}"))["d"]
        if not poll_until(10, j_pin_and_check_delirious):
            ok = False
            print(f"  [FAIL] setup: {subjJ} never became delirious")

        # End the episode, re-pinning blood_oxygen on every poll so
        # delirium doesn't clear out from under this before mental_tick
        # actually ends the episode.
        send(P, f"unit.setStat({subjJ},'mental_until',0); return 'ok'")
        if not poll_until(5, lambda: (j_pin_and_check_delirious(),
                                       mstate(P, subjJ) != "break")[-1]):
            ok = False
            print(f"  [FAIL] setup: {subjJ}'s episode never ended")

        still_delirious = j_pin_and_check_delirious()

        def j_cleaned():
            send(P, f"unit.setStat({subjJ},'blood_oxygen',0.59); return 'ok'")
            f = lash_ai_flags(P, subjJ)
            return f if (not f["tgt"] and f["goal"] != "attack"
                         and not f["committed"]) else None
        cleared_j = poll_until(10, j_cleaned)

        # The Lua-side target/goal clearing above isn't the whole story:
        # the in-flight moveTo pursuit toward targetJ must actually stop
        # too, not just ride out its old leg while delirium's own no-spam
        # gate (already walking -> no replacement move) leaves it alone.
        time.sleep(1.5)
        jx1, jy1 = unit_pos(P, subjJ)
        d_to_target_end = ((jx1 - (-19)) ** 2 + (jy1 - (-10)) ** 2) ** 0.5
        closed_gap_j = d_to_target_start - d_to_target_end

        if cleared_j and still_delirious and closed_gap_j < 2.0:
            print(f"  [pass] episode-end cleanup fires even while still "
                  f"delirious AND stops the in-flight pursuit "
                  f"(still_delirious={still_delirious}, "
                  f"closed only {closed_gap_j:.2f} tiles toward the old "
                  f"target): {cleared_j}")
        else:
            ok = False
            print(f"  [FAIL] lash-out state or pursuit leaked while delirium "
                  f"overlapped episode end: {lash_ai_flags(P, subjJ)} "
                  f"(still_delirious={still_delirious}, "
                  f"closed_gap={closed_gap_j:.2f})")

        send(P, f"unit.setStat({subjJ},'blood_oxygen',1.0); return 'ok'")
        poll_until(10, lambda: not json.loads(send(P,
            f"local b=require('scripts.brain'); "
            f"return {{d=b.isDelirious({subjJ})}}"))["d"])

        # 9k. The shared retaliation-swap must not redirect lash-out onto
        # a technomule either — mirrors the collapsed-attacker test, but
        # for the technomule exclusion. A real hit FROM a technomule
        # isn't a reliably testable event (unclear it can ever land one
        # in practice), so pin unit.getLastAttacker's report for subjK
        # instead — the same wrap-and-delegate technique the collapsed-
        # attacker test uses, applied to the attacker-memory getter this
        # time rather than getPose.
        subjK = spawn_acolyte(P, -25, 25)
        set_wellbeing(P, subjK, 1.0, 0.0)
        poll_until(5, lambda: mstate(P, subjK) == "stable")
        targetK = spawn_acolyte(P, -24, 25)   # eligible, adjacent
        muleK = spawn_acolyte(P, -26, 25, unit="technomule", clear_water=False)
        send(P, f"require('scripts.mental_state').forceBreak({subjK},'lash_out'); "
                f"return 'ok'")

        def k_target():
            t = lash_target(P, subjK)
            return t if t != "nil" else None
        if poll_until(10, k_target) != str(targetK):
            ok = False
            print(f"  [FAIL] setup: {subjK} never targeted {targetK}")

        send(P, f"if not _G.__probe_orig_getLastAttacker then "
                f"_G.__probe_orig_getLastAttacker = unit.getLastAttacker end; "
                f"unit.getLastAttacker = function(u) "
                f"if u == {subjK} then return {{uid={muleK}, at=engine.gameTime()}} end "
                f"return _G.__probe_orig_getLastAttacker(u) end; return 'ok'")

        # Sample rapidly through the retaliation-swap's own 3s window —
        # subjK must stay on targetK and never swap onto the technomule.
        samplesK = []
        deadline = time.time() + 3.0
        while time.time() < deadline:
            samplesK.append(lash_target(P, subjK))
            time.sleep(0.15)
        send(P, "if _G.__probe_orig_getLastAttacker then "
                "unit.getLastAttacker = _G.__probe_orig_getLastAttacker; "
                "_G.__probe_orig_getLastAttacker = nil end; return 'ok'")

        if str(targetK) in samplesK and str(muleK) not in samplesK:
            print(f"  [pass] lash-out stayed on {targetK} and never swapped "
                  f"onto the technomule {muleK}: {samplesK[:4]}...")
        else:
            ok = False
            print(f"  [FAIL] expected only {targetK}, saw: {samplesK}")

        send(P, f"unit.setStat({subjK},'mental_until',0); return 'ok'")
        poll_until(5, lambda: mstate(P, subjK) != "break")

        # 9g. No eligible target: agitated wander, keep searching.
        subj5 = spawn_acolyte(P, 30, -25)   # isolated — nothing within 8 tiles
        set_wellbeing(P, subj5, 1.0, 0.0)
        poll_until(5, lambda: mstate(P, subj5) == "stable")
        wx0, wy0 = unit_pos(P, subj5)
        send(P, f"require('scripts.mental_state').forceBreak({subj5},'lash_out'); "
                f"return 'ok'")
        moved = poll_until(15, lambda: (lambda x, y:
                (x - wx0) ** 2 + (y - wy0) ** 2 > 0.8)(*unit_pos(P, subj5)))
        target5 = lash_target(P, subj5)
        if moved and target5 == "nil":
            print(f"  [pass] no eligible target -> agitated wander, no attack "
                  f"goal (from {wx0:.1f},{wy0:.1f})")
        else:
            ok = False
            print(f"  [FAIL] expected wander w/ no target: moved={bool(moved)} "
                  f"target={target5!r}")
        send(P, f"unit.setStat({subj5},'mental_until',0); return 'ok'")
        poll_until(5, lambda: mstate(P, subj5) != "break")

        # Contamination guard: every check above assumed a live unit —
        # a dead one freezes its stats and passes exit checks vacuously.
        pose = send(P, f"return unit.getPose({uid})")
        if pose != "dead":
            print(f"  [pass] probe unit alive at the end (pose={pose})")
        else:
            ok = False
            print("  [FAIL] probe unit died — earlier results are contaminated")

        print(f"\n{'PASS' if ok else 'FAIL'} — mental states (#352)")
        return 0 if ok else 1
    finally:
        quit_engine(P, proc)


if __name__ == "__main__":
    sys.exit(main())
