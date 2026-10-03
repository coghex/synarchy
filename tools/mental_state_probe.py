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
import argparse, glob, json, math, sys, time
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
    #
    # Verified, not trusted (#2773): the same chunk answers every setter's
    # and recomputeBody's boolean and a typed snapshot of both fighters,
    # and neuter_snapshot_problems refuses anything short of a neuter that
    # took — BEFORE the attack order, so an unbounded hit never happens.
    neutered = send_json(port, neuter_chunk((attacker, lash)), timeout=15.0)
    problems = neuter_snapshot_problems(neutered, (attacker, lash))
    if problems:
        return lash, attacker, None, ("neuter not established: "
                                      + "; ".join(problems))
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


def lashout_observer_lua(lash, attacker, decoy):
    """The console chunk observe_first_lashout_selection sends, kept as
    text so --self-test can run it against the production policy with no
    engine (lashout_policy_harness).

    The decision cannot be read atomically. The Unit thread advances
    engine.gameTime() (Unit.Thread.unitTickWith) and moves units while
    the Lua thread runs, so two reads in one chunk can differ.
    pickLashoutTarget reads the hit, then its OWN clock, then the
    attacker's live existence, pose and position, against the subject
    position `me` that lashOutExecute read just before. So the chunk:

      * records the subject's own position from lashOutExecute's read:
        the exact `me` the policy measures from (unit.getInfo wrapper);
        that read also drops any earlier snapshot, so a snapshot can
        only belong to this call of the execute;
      * keeps the hit record the policy is handed (the same table) and
        takes reading BEFORE: clock plus both candidates, judged against
        that `me` by the production predicate. The policy's clock read
        comes after it (getLastAttacker wrapper);
      * from there until the execute is called, records the FIRST
        unit.exists / getPose / getInfo answer the decision itself got
        for the attacker. Those are the reads the window-passing branch
        of pickLashoutTarget's eligibility test makes (unit.exists /
        getPose / getInfo wrappers; the probe's own reads are excluded);
      * when lashOutExecute calls attackTargetExecute straight after the
        pick: takes reading AFTER, replays the production predicate on
        the attacker reads the decision actually got, binds all of it to
        the chosen target, and restores every original.

    The clock is bracketed, not captured. It never decreases between
    BEFORE and AFTER unless a load or a session reset writes it in
    between (classify_lashout_selection rejects a decreasing pair; the
    probe triggers neither). So, under that condition, the age the
    policy tested lies between the two.

    Every boundary comparison is made HERE, in Lua, on the original
    values, and travels as a boolean:
      * each reading's window, stale and closer tests;
      * the clock order;
      * the typing of the hit and the target.
    The console prints numbers with Lua's tostring (Lua 5.4, "%.14g";
    src/Engine/Scripting/Lua/API/Shell.hs). That rounds, so for example
    10.000000000000002 arrives as 10.0. Python therefore never re-derives
    a cutoff from a printed number.
    """
    return f"""
local lash, attacker, decoy = {lash}, {attacker}, {decoy}
if _G.__probe_lash_unmask then _G.__probe_lash_unmask() end
local policy = require('scripts.unit_ai_mental').lashoutPolicy
local atk = require('scripts.unit_ai_combat_attack')
local origGLA, origATE, origGI = unit.getLastAttacker, atk.attackTargetExecute, unit.getInfo
local origEx, origPose = unit.exists, unit.getPose
local function fin(v) return type(v) == 'number' and v == v and v ~= math.huge and v ~= -math.huge end
local rec = {{ observing = false, armed = false, decision = {{}} }}
_G.__probe_lash_rec = rec
_G.__probe_lash_restore = function()
  unit.getLastAttacker = origGLA
  atk.attackTargetExecute = origATE
  unit.getInfo = origGI
  unit.exists = origEx
  unit.getPose = origPose
  _G.__probe_lash_restore = nil
end
local function infoCopy(r)
  if not r then return nil end
  return {{ gridX = r.gridX, gridY = r.gridY, defName = r.defName }}
end
local function decisionRead(kind, u, v)
  if rec.armed and not rec.observing and not rec.selection and u == attacker
     and rec.decision[kind] == nil then
    rec.decision[kind] = {{ v = v }}
  end
end
local function candidate(me, oid)
  local info = origGI(oid)
  local d = -1
  if me and info then
    d = math.max(math.abs(me.gridX - info.gridX), math.abs(me.gridY - info.gridY))
  end
  return {{ uid = oid, exists = origEx(oid), pose = origPose(oid) or 'none',
           dist = d, eligible = (me ~= nil) and policy.eligible(lash, me, oid) }}
end
local function reading(me, a)
  rec.observing = true
  local ok, r = pcall(function()
    local now = engine.gameTime()
    local ac, dc = candidate(me, attacker), candidate(me, decoy)
    local hit = a ~= nil and a.uid == attacker
    local age = (a ~= nil and fin(a.at) and fin(now)) and (now - a.at) or nil
    return {{ now = now, nowOk = fin(now) and now >= 0, attacker = ac, decoy = dc,
             window = hit and age ~= nil and age >= 0 and age <= policy.attackerWindow,
             stale = hit and age ~= nil and age > policy.attackerWindow,
             closer = fin(dc.dist) and fin(ac.dist) and dc.dist < ac.dist }}
  end)
  rec.observing = false
  if ok then return r end
  return {{ err = tostring(r) }}
end
local function replay(me)
  local d = rec.decision
  if me == nil or d.exists == nil then return 'missing' end
  local missing = false
  local function val(k)
    local r = d[k]
    if r == nil then missing = true; return nil end
    return r.v
  end
  local sx, sp, si = unit.exists, unit.getPose, unit.getInfo
  rec.observing = true
  unit.exists = function(u) if u == attacker then return val('exists') end; return origEx(u) end
  unit.getPose = function(u) if u == attacker then return val('pose') end; return origPose(u) end
  unit.getInfo = function(u) if u == attacker then return val('info') end; return origGI(u) end
  local ok, res = pcall(policy.eligible, lash, me, attacker)
  unit.exists, unit.getPose, unit.getInfo = sx, sp, si
  rec.observing = false
  if not ok then return 'error' end
  if missing then return 'missing' end
  return res and true or false
end
unit.getInfo = function(u)
  local r = origGI(u)
  if u == lash and not rec.observing and not rec.selection then
    rec.me = r and {{ gridX = r.gridX, gridY = r.gridY }} or nil
    rec.pending = nil
    rec.armed = false
  end
  decisionRead('info', u, infoCopy(r))
  return r
end
unit.exists = function(u)
  local r = origEx(u)
  decisionRead('exists', u, r)
  return r
end
unit.getPose = function(u)
  local r = origPose(u)
  decisionRead('pose', u, r)
  return r
end
unit.getLastAttacker = function(u)
  local a = origGLA(u)
  if u == lash and not rec.observing and not rec.selection then
    rec.armed = false
    rec.hit = a
    rec.pending = {{ window = policy.attackerWindow, range = policy.range,
      meCaptured = rec.me ~= nil, me = rec.me,
      hitBy = a and a.uid or -1, hitAt = a and a.at or -1,
      hitTyped = a ~= nil and math.type(a.uid) == 'integer' and a.uid > 0
                 and fin(a.at) and a.at >= 0,
      before = reading(rec.me, a) }}
    rec.decision = {{}}
    rec.armed = true
  end
  return a
end
atk.attackTargetExecute = function(u, s, params)
  if u == lash and s and s.mentalLashoutActive and rec.pending and rec.armed
     and not rec.selection then
    rec.armed = false
    local sel = rec.pending
    sel.after = reading(rec.me, rec.hit)
    sel.ordered = fin(sel.before.now) and fin(sel.after.now)
                  and sel.after.now >= sel.before.now
    sel.target = s.attackTargetUid or -1
    sel.targetTyped = math.type(sel.target) == 'integer' and sel.target > 0
    local d = rec.decision
    sel.decision = {{ exists = d.exists, pose = d.pose, info = d.info,
                     attackerEligible = replay(rec.me) }}
    rec.selection = sel
    _G.__probe_lash_restore()
  end
  return origATE(u, s, params)
end
require('scripts.mental_state').forceBreak(lash, 'lash_out')
return 'ok'"""


def observe_first_lashout_selection(port, lash, attacker, decoy, timeout=10):
    """Force a lash-out break on `lash` and record its FIRST target
    selection, observed around and inside the decision (#2773;
    lashout_observer_lua). The wrappers are installed in the SAME console
    chunk that forces the break, and the Lua thread runs a chunk whole,
    before the next AI tick.

    Later target polls cannot reconstruct those historical
    preconditions; this records them where the decision is made.
    Answers the recorded selection dict, or None when no selection
    happened within `timeout` seconds. The wrappers are always removed.
    """
    # One line: the console reads newline-terminated commands, and the
    # chunk carries no comments, so joining its lines changes nothing.
    send(port, " ".join(line.strip() for line in
                        lashout_observer_lua(lash, attacker, decoy).splitlines()))

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


#: replay()'s answers in lashout_observer_lua: the production predicate
#: on the decision's own attacker reads, or why it could not be replayed.
_DECISION_REPLAY = (True, False, "missing", "error")


def _uid(v):
    """A unit id as it arrives from the console: a JSON integer (Lua
    integers print without a decimal point), positive; never a bool."""
    return isinstance(v, int) and not isinstance(v, bool) and v > 0


def lashout_record_problems(sel, attacker, decoy):
    """Everything malformed in a recorded selection, checked BEFORE any
    use, so a partial or mistyped record fails closed with a name instead
    of raising or passing on a default. Requires:
      * finite positive window and range;
      * a hit that is either absent (hitBy -1) or typed: a positive
        integer hitBy and a finite, non-negative hitAt, confirmed by
        Lua's own hitTyped (a missing 'at' that defaulted to -1 fails);
      * the selected target, a positive integer uid (Lua's targetTyped).
        Any genuine target is allowed, so a wrong target stays observable;
      * the captured subject position `me`, with finite grid coordinates;
      * both bracket readings: a finite, non-negative clock (Lua's
        nowOk); boolean window, stale and closer tests; and both
        candidates with the expected integer uid, boolean exists, string
        pose, finite distance and boolean eligibility;
      * a clock that did not go backwards (Lua's `ordered`, compared on
        the unrounded values);
      * the decision replay's answer, one of _DECISION_REPLAY."""
    if not isinstance(sel, dict):
        return [f"selection record is {sel!r}"]
    problems = []
    for k in ("window", "range"):
        if not (_finite_num(sel.get(k)) and sel[k] > 0):
            problems.append(f"{k} {sel.get(k)!r} is not a positive number")
    hit_by = sel.get("hitBy")
    if hit_by == -1 and not isinstance(hit_by, bool):
        if not _finite_num(sel.get("hitAt")):
            problems.append(f"hitAt {sel.get('hitAt')!r} is not a finite number")
    elif not _uid(hit_by):
        problems.append(f"hitBy {hit_by!r} is neither -1 nor a positive integer uid")
    elif not (_finite_num(sel.get("hitAt")) and sel["hitAt"] >= 0
              and sel.get("hitTyped") is True):
        problems.append(f"hit by {hit_by} has no valid timestamp "
                        f"(hitAt {sel.get('hitAt')!r}, hitTyped {sel.get('hitTyped')!r})")
    if not (_uid(sel.get("target")) and sel.get("targetTyped") is True):
        problems.append(f"selected target {sel.get('target')!r} is not a positive "
                        f"integer uid (targetTyped {sel.get('targetTyped')!r})")
    me = sel.get("me")
    if sel.get("meCaptured") is not True or not isinstance(me, dict):
        problems.append("the subject position the decision measured from "
                        "was not captured")
    elif not (_finite_num(me.get("gridX")) and _finite_num(me.get("gridY"))):
        problems.append(f"captured subject position {me!r} is malformed")
    for side in ("before", "after"):
        r = sel.get(side)
        if not isinstance(r, dict):
            problems.append(f"no {side} reading")
            continue
        if "err" in r:
            problems.append(f"{side} reading raised: {r['err']}")
            continue
        if not (_finite_num(r.get("now")) and r["now"] >= 0 and r.get("nowOk") is True):
            problems.append(f"{side} clock {r.get('now')!r} is not a finite "
                            f"non-negative number (nowOk {r.get('nowOk')!r})")
        for k in ("window", "stale", "closer"):
            if not isinstance(r.get(k), bool):
                problems.append(f"{side} {k} test {r.get(k)!r} is not a boolean")
        for role, uid in (("attacker", attacker), ("decoy", decoy)):
            c = r.get(role)
            if not isinstance(c, dict):
                problems.append(f"{side} {role} reading missing")
                continue
            if not (_uid(c.get("uid")) and c["uid"] == uid):
                problems.append(f"{side} {role} reading is for {c.get('uid')!r}, "
                                f"not integer uid {uid}")
            if not isinstance(c.get("exists"), bool):
                problems.append(f"{side} {role} exists {c.get('exists')!r} is not a boolean")
            if not isinstance(c.get("pose"), str):
                problems.append(f"{side} {role} pose {c.get('pose')!r} is not a string")
            if not _finite_num(c.get("dist")):
                problems.append(f"{side} {role} distance {c.get('dist')!r} is not a finite number")
            if not isinstance(c.get("eligible"), bool):
                problems.append(f"{side} {role} eligible {c.get('eligible')!r} is not a boolean")
    if sel.get("ordered") is not True:
        problems.append(f"clock not ordered across the decision (ordered "
                        f"{sel.get('ordered')!r}): a load or session reset, or a "
                        f"missing reading, so the bracket proves nothing")
    dec = sel.get("decision")
    if not isinstance(dec, dict) or not any(
            dec.get("attackerEligible") is v if isinstance(v, bool)
            else dec.get("attackerEligible") == v for v in _DECISION_REPLAY):
        problems.append(f"decision replay {dec!r} is malformed")
    return problems


def lashout_decision_view(sel, side):
    """One bracket side of a well-formed selection ('before' or 'after'):
    the hit record the policy was handed, with that side's Lua-computed
    tests and candidates. `age` is for messages only: it is recomputed
    from printed numbers, so no decision uses it."""
    r = sel[side]
    has_hit = sel["hitBy"] != -1
    return {"window": sel["window"], "range": sel["range"],
            "hitBy": sel["hitBy"], "hitAt": sel["hitAt"],
            "hitOk": has_hit and sel.get("hitTyped") is True,
            "windowOk": r["window"], "stale": r["stale"], "closer": r["closer"],
            "age": (r["now"] - sel["hitAt"]) if has_hit else -1,
            "attacker": r["attacker"], "decoy": r["decoy"]}


def lashout_predicates(view, attacker, decoy):
    """The fair-test preconditions on one reading, as (name, holds,
    message) in a fixed order. Every `holds` is the boolean Lua computed
    on the unrounded values (lashout_observer_lua); the numbers appear in
    the messages only.

      * hit      — the hit the decision read is `attacker`'s, typed;
      * window   — and its age is inside the production window
                   (inclusive);
      * attacker — the attacker is eligible (exists, not dead or
                   collapsed, within range: the production predicate);
      * decoy    — the decoy is eligible too;
      * closer   — the decoy is STRICTLY closer under the production
                   Chebyshev metric.

    Without every one of them, the decoy, or the attacker, would be the
    correct pick for a reason that is not the preference under test."""
    a, d = view["attacker"], view["decoy"]
    hit = view["hitOk"] and view["hitBy"] == attacker
    return [
        ("hit", hit, f"the hit read at selection was by {view['hitBy']}, "
                     f"not attacker {attacker}"),
        ("window", hit and view["windowOk"],
         f"hit age at selection ~{view['age']:.2f}s is outside the "
         f"{view['window']:g}s attacker window"),
        ("attacker", a["eligible"],
         f"attacker {attacker} ineligible at selection (exists={a['exists']}, "
         f"pose={a['pose']}, distance={a['dist']:.2f}, range={view['range']:g})"),
        ("decoy", d["eligible"],
         f"decoy {decoy} ineligible at selection (exists={d['exists']}, "
         f"pose={d['pose']}, distance={d['dist']:.2f})"),
        ("closer", view["closer"],
         f"decoy {decoy} at {d['dist']:.2f} is not strictly closer than "
         f"attacker {attacker} at {a['dist']:.2f}"),
    ]


def lashout_setup_problems(view, attacker, decoy):
    """The failing preconditions on one reading, each named with its value."""
    return [msg for _, holds, msg in lashout_predicates(view, attacker, decoy)
            if not holds]


def classify_lashout_selection(sel, attacker, decoy):
    """('fair' | 'setup' | 'ambiguous', problems) for a recorded selection.

      * setup     — the record is malformed (lashout_record_problems), or
                    the preconditions failed, with every predicate giving
                    the SAME answer before and after; or the decision's own
                    attacker reads could not be replayed. Discarded and
                    restaged.
      * ambiguous — any single predicate answered differently before and
                    after. This also covers sides failing for DIFFERENT
                    reasons, and a hit aging past the window while the
                    policy read it. It also covers the attacker reading
                    eligible on both sides but INELIGIBLE on the reads the
                    decision itself got (a there-and-back in range or pose).
                    What the policy saw is then unknown or unfair, so it
                    is a NAMED boundary-ambiguous setup discard, restaged
                    and never graded.
      * fair      — every predicate holds before and after, and the
                    production predicate holds on the decision's OWN
                    attacker reads. The clock is bracketed: monotone
                    between the two readings, given no load or reset. So
                    the policy's age test passed and its eligibility test
                    saw an eligible attacker. A decoy target is then a
                    POLICY failure, graded and never retried. The decoy's
                    side is never read by a decision that keeps the
                    attacker, so its predicates rest on the bracket. They
                    make a pass meaningful; they cannot turn a correct
                    fallback into a failure.
    """
    malformed = lashout_record_problems(sel, attacker, decoy)
    if malformed:
        return "setup", ["malformed selection record: " + "; ".join(malformed)]
    pb = lashout_predicates(lashout_decision_view(sel, "before"), attacker, decoy)
    pa = lashout_predicates(lashout_decision_view(sel, "after"), attacker, decoy)
    crossed = [nb for (nb, hb, _), (_, ha, _) in zip(pb, pa) if hb != ha]
    if crossed:
        def failing(ps):
            return "; ".join(m for _, h, m in ps if not h) or "all held"
        return "ambiguous", [
            f"boundary-ambiguous: {', '.join(crossed)} changed while the "
            f"decision read it (before: {failing(pb)}; after: {failing(pa)})"]
    fails = [m for _, h, m in pb if not h]
    if fails:
        return "setup", fails
    replayed = sel["decision"]["attackerEligible"]
    if replayed is True:
        return "fair", []
    if replayed is False:
        return "ambiguous", [
            f"boundary-ambiguous: attacker {attacker} was eligible before and "
            f"after the decision but INELIGIBLE on the decision's own reads "
            f"(exists={sel['decision'].get('exists')}, "
            f"pose={sel['decision'].get('pose')}, "
            f"info={sel['decision'].get('info')})"]
    return "setup", [f"the decision's own attacker reads could not be "
                     f"replayed ({replayed})"]


def stale_selection_observed(sel, attacker, decoy):
    """True only for an UNAMBIGUOUS stale selection. The record is
    well-formed, the selection classifies as a setup discard, the policy
    was handed the attacker's typed hit, and Lua found that hit already
    past the window BEFORE the decision (its unrounded `stale` test). The clock does not decrease, so the
    policy's own read saw it stale too. A hit crossing the window during
    the decision, or any other predicate crossing, is ambiguous and is
    not stale evidence."""
    if lashout_record_problems(sel, attacker, decoy):
        return False
    if classify_lashout_selection(sel, attacker, decoy)[0] != "setup":
        return False
    view = lashout_decision_view(sel, "before")
    return (view["hitOk"] and view["hitBy"] == attacker
            and view["stale"] is True)


def perturb_lashout_case(port, case, attempt, lash, attacker):
    """`--lashout-case` (#2773): push ONE staged setup off the fair-test
    preconditions before the break, through the real engine, so the
    classification can be shown on the actual selection path. Staging
    has already stood the attacker down, so its hit can age and it stays
    where it is put. Answers a named setup problem, or None.

      * stale      — first attempt only: dress the staged wound
                     (stale_dress_chunk), then wait until the staged hit
                     is older than the attacker window, checking at every
                     poll that the subject is alive and its real attacker
                     record unchanged (stale_wait_chunk). Game time keeps
                     running; nothing is frozen. That attempt must then
                     be discarded and RESTAGED. Without the dressing, a
                     traced run had the subject dead by the observer
                     timeout (the staged neck slash bled it out), so no
                     stale selection ever happened;
      * ineligible — every attempt: teleport the attacker beyond
                     lash-out range, so no attempt is gradable and 9a
                     must end in a named SETUP failure.
    """
    if case == "stale" and attempt == 1:
        dressed = send_json(port, stale_dress_chunk(lash, attacker), timeout=30.0)
        problems = stale_dressing_problems(dressed, attacker)
        if problems:
            return "stale dressing not established: " + "; ".join(problems)
        last = {}

        def aged():
            got = send_json(port, stale_wait_chunk(lash))
            last["v"] = got
            verdict = stale_wait_verdict(got)
            return verdict if verdict != "waiting" else None
        verdict = poll_until(30, aged, interval=0.5)
        if verdict is None:
            return ("stale wait timed out before the hit aged past 10.5 s "
                    f"({last.get('v')})")
        if verdict != "aged":
            return f"stale wait invalid: {verdict}"
    elif case == "ineligible":
        lx, ly = unit_pos(port, lash)
        send(port, f"unit.setPos({attacker}, {lx + 12}, {ly}); return 'ok'")
        poll_until(5, lambda: abs(unit_pos(port, attacker)[0] - (lx + 12)) < 0.25)
    return None


# ---- #2773 setup verification. The pure validators below are exercised
# ---- with no engine by `--self-test`.

#: The finite-number test, spelled once for every Lua chunk below.
_LUA_FINITE = ("local function fin(v) return type(v) == 'number' and v == v "
               "and v ~= math.huge and v ~= -math.huge end;")


def neuter_chunk(uids):
    """ONE console chunk: neuter each fighter (strength_base, toughness,
    unit.recomputeBody) and answer every call's boolean plus a typed
    snapshot read back after the recompute. All three verbs write the
    unit manager synchronously, so the snapshot reflects them."""
    per = " ".join(
        f"do local u = {u}; local r = {{}};"
        f" r.setBase = unit.setStat(u, 'strength_base', {NEUTERED_STRENGTH_BASE});"
        f" r.setTough = unit.setStat(u, 'toughness', 100);"
        f" r.recompute = unit.recomputeBody(u);"
        f" local function g(k) return unit.getStatBase(u, k) end;"
        f" r.raw = g('strength'); r.eff = unit.getStat(u, 'strength');"
        f" r.base = g('strength_base'); r.body = g('strength_body');"
        f" r.tough = g('toughness'); r.height = g('height');"
        f" r.lean = g('lean_mass'); r.mass = g('body_mass');"
        f" r.finite = fin(r.raw) and fin(r.eff) and fin(r.base) and fin(r.body)"
        f" and fin(r.tough) and fin(r.height) and fin(r.lean) and fin(r.mass);"
        f" out['u{u}'] = r end"
        for u in uids)
    return ("local ok, res = pcall(function() " + _LUA_FINITE
            + " local out = {}; " + per + " return out end);"
            " if not ok then return { err = tostring(res) } end; return res")


def neuter_snapshot_problems(snap, uids):
    """Everything wrong with a neuter_chunk answer; empty when the neuter
    took on every fighter: all three calls true, every snapshot value a
    finite number, strength_base exactly NEUTERED_STRENGTH_BASE, toughness
    100, and the raw strength (and strength_body, which mirrors it) equal
    to recomputeBodyDerivedStats's strength_base*(lean/(8.8*height^2))^0.7
    within tolerance (Unit.Thread.Command.Body)."""
    if not isinstance(snap, dict):
        return [f"neuter chunk answered {snap!r}"]
    if "err" in snap:
        return [f"neuter chunk raised: {snap['err']}"]
    problems = []
    for u in uids:
        r = snap.get(f"u{u}")
        if not isinstance(r, dict):
            problems.append(f"{u}: no snapshot")
            continue
        for k in ("setBase", "setTough", "recompute"):
            if r.get(k) is not True:
                problems.append(f"{u}: {k} returned {r.get(k)!r}")
        keys = ("raw", "eff", "base", "body", "tough", "height", "lean", "mass")
        bad = [k for k in keys
               if not (isinstance(r.get(k), (int, float))
                       and not isinstance(r.get(k), bool)
                       and math.isfinite(r[k]))]
        if bad or r.get("finite") is not True:
            problems.append(f"{u}: missing or non-finite {bad or ['(lua)']}")
            continue
        if abs(r["base"] - NEUTERED_STRENGTH_BASE) > 1e-6:
            problems.append(f"{u}: strength_base {r['base']} != "
                            f"{NEUTERED_STRENGTH_BASE}")
        if r["height"] <= 0 or r["lean"] <= 0 or r["mass"] <= 0:
            problems.append(f"{u}: height {r['height']} / lean_mass "
                            f"{r['lean']} / body_mass {r['mass']} not positive")
            continue
        expected = (NEUTERED_STRENGTH_BASE
                    * (r["lean"] / (8.8 * r["height"] ** 2)) ** 0.7)
        tol = 1e-5 + 1e-3 * expected
        if abs(r["raw"] - expected) > tol:
            problems.append(f"{u}: raw strength {r['raw']:.6g} != expected "
                            f"{expected:.6g}")
        if abs(r["body"] - r["raw"]) > tol:
            problems.append(f"{u}: strength_body {r['body']:.6g} != raw "
                            f"strength {r['raw']:.6g}")
        if abs(r["tough"] - 100) > 1e-6:
            problems.append(f"{u}: toughness {r['tough']} != 100")
    return problems


def stale_dress_chunk(lash, attacker):
    """ONE console chunk for the stale demo's first attempt: read the
    subject's REAL attacker record through the setup mask's own saved
    original (_G.__probe_lash_real_gla — the mask stays in place and is
    neither lifted nor rewritten), dress its wounds by self-treatment
    with stanch's loop and bounds, read the record again, and answer
    both records (compared here, in Lua, on the raw values), the
    treatment results, the subject's pose and its blood."""
    return " ".join((
        "local ok, res = pcall(function()",
        _LUA_FINITE,
        f"local u, A = {lash}, {attacker};",
        "local real = _G.__probe_lash_real_gla;",
        "if type(real) ~= 'function' then return { err = 'setup mask not installed' } end;",
        "local function rec() local a = real(u);",
        " if not a then return nil end;",
        " return { uid = a.uid, at = a.at, typed = fin(a.uid) and fin(a.at) } end;",
        "local before = rec(); _G.__probe_stale_rec = before;",
        "unit.setKnowledge(u, 'bleed_control', 100);",
        "local n, stalled, typed, reason = 0, 0, true, 'bound';",
        "local function rate() local b = unit.getBlood(u);",
        " return b and b.bleedRate or nil end;",
        "local last = rate();",
        f"for _ = 1, {STANCH_ATTEMPTS} do",
        " local now0 = rate(); if not fin(now0) then reason = 'rate_unreadable'; break end;",
        f" if now0 <= {STANCH_SETTLED_RATE} then reason = 'settled'; break end;",
        " local r = unit.treatBleeding(u, u);",
        " if type(r) ~= 'table' or type(r.ok) ~= 'boolean' then",
        "  typed = false; reason = 'untyped'; break end;",
        " if not r.ok then reason = 'treatment_failed'; break end;",
        " n = n + 1;",
        " local now = rate();",
        " if fin(now) and fin(last) and now < last - 1e-6 then stalled = 0"
        " else stalled = stalled + 1 end;",
        " last = now;",
        f" if stalled >= {STANCH_STALLED_PASSES} then reason = 'stalled'; break end end;",
        "unit.setKnowledge(u, 'bleed_control', 0);",
        "local after = rec(); local b = unit.getBlood(u);",
        "return { before = before, after = after, attacker = A,",
        " same = (before ~= nil and after ~= nil and before.typed and after.typed",
        "  and before.uid == after.uid and before.at == after.at),",
        " dressed = n, stalled = stalled, typed = typed, reason = reason,",
        " bleedRate = b and b.bleedRate or nil, blood = b and b.current or nil,",
        " finite = b ~= nil and fin(b.bleedRate) and fin(b.current),",
        " pose = tostring(unit.getPose(u)) }",
        "end);",
        "if not ok then return { err = tostring(res) } end; return res"))


def _finite_num(v):
    """A real, finite number (bool excluded)."""
    return (isinstance(v, (int, float)) and not isinstance(v, bool)
            and math.isfinite(v))


def _record_problems(rec, attacker, label):
    """A real attacker record must be present, typed (finite uid and 'at')
    and name the expected attacker."""
    if not isinstance(rec, dict):
        return [f"no real attacker record {label}"]
    if (rec.get("typed") is not True or not _finite_num(rec.get("uid"))
            or not _finite_num(rec.get("at"))):
        return [f"real attacker record {label} untyped "
                f"(uid {rec.get('uid')!r}, at {rec.get('at')!r})"]
    if rec.get("uid") != attacker:
        return [f"real record {label} names {rec.get('uid')}, not "
                f"attacker {attacker}"]
    return []


def _blood_problems(blood, rate):
    """Blood and bleed rate must be present, finite and non-negative, with
    blood left."""
    if not _finite_num(blood) or not _finite_num(rate):
        return [f"blood unreadable ({blood!r}, {rate!r})"]
    problems = []
    if not blood > 0:
        problems.append(f"blood {blood} not > 0")
    if rate < 0:
        problems.append(f"bleed rate {rate} negative")
    return problems


def stale_dressing_problems(d, attacker):
    """Everything wrong with a stale_dress_chunk answer; empty only when:
    the loop stopped because the subject SETTLED (an explicit treatment
    failure, an untyped result, a stall, an unreadable rate or running
    out of attempts each fail on their own, whatever the final readings);
    the real record exists, is typed and names the attacker before AND
    after, and was identical across the dressing (compared in Lua); the
    subject is standing; and blood and rate are present, finite and
    non-negative with blood left and bleedRate <= STANCH_SETTLED_RATE."""
    if not isinstance(d, dict):
        return [f"dressing chunk answered {d!r}"]
    if "err" in d:
        return [f"dressing chunk raised: {d['err']}"]
    problems = []
    reason = d.get("reason")
    if reason != "settled":
        problems.append(f"dressing stopped by {reason!r} after "
                        f"{d.get('dressed')} treatment(s) (stalled "
                        f"{d.get('stalled')})")
    if d.get("typed") is not True:
        problems.append("a treatment returned an untyped result")
    problems += _record_problems(d.get("before"), attacker, "before dressing")
    problems += _record_problems(d.get("after"), attacker, "after dressing")
    if d.get("same") is not True:
        problems.append(f"real record changed across dressing "
                        f"({d.get('before')} -> {d.get('after')})")
    if d.get("pose") != "standing":
        problems.append(f"subject pose {d.get('pose')!r}, not standing")
    blood = _blood_problems(d.get("blood"), d.get("bleedRate"))
    if not blood and d.get("finite") is not True:
        blood = ["blood unreadable in Lua"]
    problems += blood
    if not blood and not d["bleedRate"] <= STANCH_SETTLED_RATE:
        problems.append(f"still bleeding {d['bleedRate']:.4g}")
    return problems


def stale_wait_chunk(lash):
    """One age-poll read: the hit's age, plus the subject's survival and
    whether its REAL attacker record is still exactly the one the dressing
    chunk saved (_G.__probe_stale_rec) — compared in Lua, on the raw
    values, so no float round-trips through JSON."""
    return " ".join((
        "local ok, res = pcall(function()",
        f"local a = _G.__probe_lash_real_gla and _G.__probe_lash_real_gla({lash});",
        "local r = _G.__probe_stale_rec;",
        _LUA_FINITE,
        "local now = engine.gameTime();",
        f"local b = unit.getBlood({lash});",
        f"return {{ pose = unit.getPose({lash}),",
        " same = (a ~= nil and r ~= nil and r.typed == true and fin(a.uid)",
        "  and fin(a.at) and a.uid == r.uid and a.at == r.at),",
        " age = (a ~= nil and fin(a.at)) and (now - a.at) or nil,",
        " blood = b and b.current or nil, bleedRate = b and b.bleedRate or nil }",
        "end);",
        "if not ok then return { err = tostring(res) } end; return res"))


def stale_wait_verdict(w):
    """'aged' once the unchanged, typed hit is older than 10.5 game-s with
    the subject standing and its blood readable; 'waiting' before that;
    otherwise the named reason the wait is invalid (a missing, unknown or
    non-standing pose included)."""
    if not isinstance(w, dict):
        return f"age poll answered {w!r}"
    if "err" in w:
        return f"age poll raised: {w['err']}"
    if w.get("pose") != "standing":
        return f"subject pose {w.get('pose')!r} during the wait, not standing"
    if w.get("same") is not True:
        return "real attacker record missing, untyped or changed during the wait"
    blood = _blood_problems(w.get("blood"), w.get("bleedRate"))
    if blood:
        return "; ".join(blood) + " during the wait"
    age = w.get("age")
    if not _finite_num(age):
        return f"hit age unreadable ({age!r})"
    return "aged" if age > 10.5 else "waiting"


def stale_demo_verdict(stale_observed, graded_ok):
    """--lashout-case stale passes ONLY when an observed AI selection with
    the hit past the attacker window got its named stale discard AND a
    valid restage then graded; a fresh pass alone is not the stale
    demonstration."""
    return bool(stale_observed and graded_ok)


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
    held on both sides of the decision (classify_lashout_selection).

    A setup that misses them is not a policy result: it is discarded,
    named, and RESTAGED on a fresh cluster, at most len(LASHOUT_CLUSTERS)
    times. The first gradable selection decides — a wrong target there is
    a policy failure, and no later attempt runs to erase it. Running out
    of attempts is a SETUP failure, and fails the probe all the same.

    Answers (ok, lash, attacker, decoy, stale_observed) for the attempt
    that was graded (or the last one staged), which 9b keeps using;
    stale_observed is True when --lashout-case stale's first attempt was
    discarded on an OBSERVED selection with the hit past the window.
    """
    reasons = []
    lash = attacker = decoy = None
    stale_observed = False
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
        if not problems and case:
            perturbed = perturb_lashout_case(port, case, attempt, lash, attacker)
            if perturbed:
                problems = [perturbed]
        if not problems:
            sel = observe_first_lashout_selection(port, lash, attacker, decoy)
            if sel is None:
                problems = ["no lash-out target selection within 10s of the break"]
            else:
                kind, problems = classify_lashout_selection(sel, attacker, decoy)
                if (case == "stale" and attempt == 1 and kind == "setup"
                        and stale_selection_observed(sel, attacker, decoy)):
                    stale_observed = True
                    age = lashout_decision_view(sel, "before")["age"]
                    print(f"  [setup] stale demonstration: the AI selected with "
                          f"the hit {age:.2f} s old before the decision (window "
                          f"{sel['window']:g} s) — discarded, restaging")
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
        pre = lashout_decision_view(sel, "before")
        post = lashout_decision_view(sel, "after")
        detail = (f"hit age {pre['age']:.2f}-{post['age']:.2f}s across the "
                  f"decision (window {sel['window']:g}s), attacker at "
                  f"{pre['attacker']['dist']:.2f}, decoy at "
                  f"{pre['decoy']['dist']:.2f}")
        if sel["target"] == attacker:
            print(f"  [pass] lash-out prefers the recent attacker {attacker} "
                  f"over the closer decoy {decoy} — {detail}")
            return True, lash, attacker, decoy, stale_observed
        print(f"  [FAIL] lash-out target={sel['target']}, expected attacker="
              f"{attacker} (decoy={decoy}) — preconditions held: {detail}")
        return False, lash, attacker, decoy, stale_observed
    send(port, "if _G.__probe_lash_unmask then _G.__probe_lash_unmask() end; "
               "return 'ok'")
    print(f"  [FAIL] setup: lash-out attacker preference could not be graded "
          f"— no attempt established its preconditions ({' | '.join(reasons)})")
    return False, lash, attacker, decoy, stale_observed


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

#: --self-test's no-engine harness: the PRODUCTION scripts/unit_ai_mental.lua
#: (or a deliberately broken copy of it) driven through M.shortCircuit, with
#: the probe's own observer chunk installed, against stubbed engine
#: bindings. Answers depend on WHO reads:
#:   * the clock gives the observer's two readings BEFORE and AFTER in
#:     turn, and the policy's own read (pickLashoutTarget) POLICY — the
#:     reviewer's case (observer 9.99, production 10.01) exactly;
#:   * the attacker's position and pose read by the probe (its readings
#:     and replay, found on the call stack) can differ from what the
#:     decision itself reads, so a there-and-back the bracket cannot see
#:     is reproduced at production's ACTUAL eligibility reads;
#:   * the result is serialized the way the production console does
#:     (Lua 5.4 tostring, "%.14g"), so boundary values arrive rounded
#:     exactly as they would from the engine.
_HARNESS_LUA = r"""
local POLICY, OBSERVER = ...
local CLOCK = { before = %(before)r, policy = %(policy)r, after = %(after)r }
local seen = {}
local obs = 0
local function probeRead()
  for lvl = 3, 40 do
    local i = debug.getinfo(lvl, 'n')
    if not i then break end
    if i.name == 'candidate' or i.name == 'reading' or i.name == 'replay' then return true end
  end
  return false
end
engine = { gameTime = function()
  local caller = debug.getinfo(2, 'n')
  local name = caller and caller.name or '?'
  if name == 'pickLashoutTarget' then
    seen[#seen + 1] = name
    return CLOCK.policy
  end
  if probeRead() then
    seen[#seen + 1] = 'reading'
    obs = obs + 1
    return obs == 1 and CLOCK.before or CLOCK.after
  end
  seen[#seen + 1] = name
  return 0
end }
local ATT_X, ATT_X_DEC, ATT_POSE_DEC = %(attacker_x)d, %(attacker_x_decision)d, %(attacker_pose_decision)r
unit = {
  getInfo = function(u)
    if u == 1 then return { gridX = 0, gridY = 0, defName = 'acolyte' } end
    if u == 2 then return { gridX = probeRead() and ATT_X or ATT_X_DEC, gridY = 0, defName = 'acolyte' } end
    if u == 3 then return { gridX = %(decoy_x)d, gridY = 0, defName = 'acolyte' } end
    return nil
  end,
  exists = function(u) return u == 1 or u == 2 or u == 3 end,
  getPose = function(u)
    if u == 2 and not probeRead() then return ATT_POSE_DEC end
    return 'standing'
  end,
  getLastAttacker = function(u) if u == 1 then return { uid = 2, at = 0.0 } end end,
  getAllIds = function() return { 1, 2, 3 } end,
  clearAnimOverride = function() end, stop = function() end,
  getActivity = function() return 'idle' end,
}
local function stub(t) return function() return t end end
package.preload['scripts.brain'] = stub({ isDelirious = function() return false end })
package.preload['scripts.mental_state'] = stub({ isBreaking = function() return true end,
  breakBehavior = function() return 'lash_out' end, forceBreak = function() end })
package.preload['scripts.unit_ai_needs'] = stub({ wanderExecute = function() end })
package.preload['scripts.movement_speed'] = stub({})
package.preload['scripts.unit_ai_core'] = stub({ suspendOrders = function() end,
  setGoal = function() end, markGoalAccomplished = function() end })
package.preload['scripts.unit_ai_combat_attack'] = stub({ attackTargetExecute = function() end })
package.preload['scripts.unit_ai_combat_lunge'] = stub({ clear = function() end })
package.preload['scripts.unit_ai_mental'] = function() return assert(loadfile(POLICY))() end
assert(load(OBSERVER))()
require('scripts.unit_ai_mental').shortCircuit(1, {}, {}, 'idle', {})
local function enc(v)
  local t = type(v)
  if t == 'table' then
    local parts = {}
    for k, x in pairs(v) do parts[#parts + 1] = string.format('%%q:%%s', tostring(k), enc(x)) end
    return '{' .. table.concat(parts, ',') .. '}'
  elseif t == 'string' then return string.format('%%q', v)
  elseif t == 'boolean' then return tostring(v)
  elseif t == 'number' then
    -- Exactly what the console sends (luaValueToText,
    -- src/Engine/Scripting/Lua/API/Shell.hs): Lua 5.4's tostring, i.e.
    -- integers in full, floats as "%%.14g" plus ".0" when that looks
    -- integral, and quoted stand-ins for inf and nan.
    if v ~= v then return '"nan"' end
    if v == math.huge then return '"inf"' end
    if v == -math.huge then return '"-inf"' end
    if math.type(v) == 'integer' then return string.format('%%d', v) end
    local s = string.format('%%.14g', v)
    if not s:find('[^%%-0-9]') then s = s .. '.0' end
    return s
  end
  return 'null'
end
local rec = _G.__probe_lash_rec
io.write(enc({ selection = rec and rec.selection or false, clockCallers = seen }))
"""


def lashout_policy_harness(before, policy, after, attacker_x=3, decoy_x=-1,
                           attacker_x_decision=None,
                           attacker_pose_decision="standing",
                           policy_patch=None):
    """Run the lash-out policy with the probe's observer chunk and no
    engine (needs a `lua` interpreter).

    Setup: subject 1 at x=0; attacker 2, whose hit is stamped at game time
    0, at `attacker_x`; decoy 3 at `decoy_x`. The decision's OWN reads of
    the attacker see `attacker_x_decision` (default: the same) and
    `attacker_pose_decision`. `policy_patch=(old, new)` runs a copy of
    scripts/unit_ai_mental.lua with that one substitution (a broken
    policy).

    Answers (selection, clock callers), or None when no lua is installed."""
    import shutil, subprocess, os, tempfile
    lua = shutil.which("lua")
    if lua is None:
        return None
    root = os.path.dirname(os.path.dirname(os.path.abspath(__file__)))
    with open(os.path.join(root, "scripts", "unit_ai_mental.lua")) as fh:
        policy_src = fh.read()
    if policy_patch is not None:
        old, new = policy_patch
        if policy_src.count(old) != 1:
            raise RuntimeError(f"policy patch target not unique: {old!r}")
        policy_src = policy_src.replace(old, new)
    observer = " ".join(line.strip() for line in
                        lashout_observer_lua(1, 2, 3).splitlines())
    src = _HARNESS_LUA % {
        "before": before, "policy": policy, "after": after,
        "attacker_x": attacker_x, "decoy_x": decoy_x,
        "attacker_x_decision": (attacker_x if attacker_x_decision is None
                                else attacker_x_decision),
        "attacker_pose_decision": attacker_pose_decision}
    with tempfile.TemporaryDirectory() as tmp:
        harness = os.path.join(tmp, "harness.lua")
        policy_path = os.path.join(tmp, "unit_ai_mental.lua")
        with open(harness, "w") as fh:
            fh.write(src)
        with open(policy_path, "w") as fh:
            fh.write(policy_src)
        out = subprocess.run([lua, harness, policy_path, observer],
                             capture_output=True, text=True, timeout=30)
    if out.returncode != 0:
        raise RuntimeError(f"policy harness failed: {out.stderr.strip()}")
    got = json.loads(out.stdout)
    return got["selection"] or None, got["clockCallers"]


def self_test():
    """No-engine regression cases for #2773's pure setup validators and
    gates (`--self-test`). Exit 0 when every case holds."""
    fails = []

    def check(name, cond):
        print(("  ok   " if cond else "  FAIL ") + name)
        if not cond:
            fails.append(name)

    # neuter_snapshot_problems
    h, lean = 1.8, 28.5
    raw = NEUTERED_STRENGTH_BASE * (lean / (8.8 * h * h)) ** 0.7
    good = {"setBase": True, "setTough": True, "recompute": True, "raw": raw,
            "eff": raw, "base": NEUTERED_STRENGTH_BASE, "body": raw,
            "tough": 100.0, "height": h, "lean": lean, "mass": 71.0,
            "finite": True}
    snap = {"u2": dict(good), "u1": dict(good)}
    check("neuter: a neuter that took passes", neuter_snapshot_problems(snap, (2, 1)) == [])
    check("neuter: a false recomputeBody fails",
          neuter_snapshot_problems({"u2": dict(good, recompute=False), "u1": good}, (2, 1)) != [])
    check("neuter: a missing fighter fails", neuter_snapshot_problems({"u2": good}, (2, 1)) != [])
    check("neuter: a non-finite value fails",
          neuter_snapshot_problems({"u2": dict(good, raw=float("inf")), "u1": good}, (2, 1)) != [])
    check("neuter: a Lua-side non-finite flag fails",
          neuter_snapshot_problems({"u2": dict(good, finite=False), "u1": good}, (2, 1)) != [])
    check("neuter: an un-neutered base fails",
          neuter_snapshot_problems({"u2": dict(good, base=1.0), "u1": good}, (2, 1)) != [])
    check("neuter: raw strength left at the old value fails (setter after recompute)",
          neuter_snapshot_problems({"u2": dict(good, raw=1.0, body=1.0), "u1": good}, (2, 1)) != [])
    check("neuter: a raised chunk fails", neuter_snapshot_problems({"err": "boom"}, (2, 1)) != [])
    check("neuter: a malformed setter result fails",
          neuter_snapshot_problems({"u2": dict(good, setBase="ok"), "u1": good}, (2, 1)) != [])
    check("neuter: a non-positive body_mass fails",
          neuter_snapshot_problems({"u2": dict(good, mass=0.0), "u1": good}, (2, 1)) != [])
    check("neuter: no answer fails", neuter_snapshot_problems(None, (2, 1)) != [])

    # stale_dressing_problems
    rgood = {"uid": 2, "at": 10.5, "typed": True}
    dgood = {"before": dict(rgood), "after": dict(rgood), "same": True,
             "typed": True, "reason": "settled", "pose": "standing",
             "finite": True, "blood": 4.9, "bleedRate": 0.0, "dressed": 1,
             "stalled": 0}
    check("dressing: a dressed, unchanged, standing subject passes",
          stale_dressing_problems(dgood, 2) == [])
    check("dressing: still bleeding fails",
          stale_dressing_problems(dict(dgood, bleedRate=0.0012), 2) != [])
    check("dressing: a changed record fails", stale_dressing_problems(dict(dgood, same=False), 2) != [])
    check("dressing: a record naming someone else fails", stale_dressing_problems(dgood, 7) != [])
    check("dressing: no record fails",
          stale_dressing_problems(dict(dgood, before=None, same=False), 2) != [])
    check("dressing: an untyped treatment fails", stale_dressing_problems(dict(dgood, typed=False), 2) != [])
    check("dressing: a dead subject fails", stale_dressing_problems(dict(dgood, pose="dead"), 2) != [])
    check("dressing: no blood left fails", stale_dressing_problems(dict(dgood, blood=0.0), 2) != [])
    check("dressing: unreadable blood fails", stale_dressing_problems(dict(dgood, finite=False), 2) != [])
    check("dressing: no answer fails", stale_dressing_problems(None, 2) != [])
    check("dressing: an explicit treatment failure fails even with a low final rate",
          stale_dressing_problems(dict(dgood, reason="treatment_failed", bleedRate=0.0), 2) != [])
    check("dressing: a stall fails even with a low final rate",
          stale_dressing_problems(dict(dgood, reason="stalled", stalled=3, bleedRate=0.0), 2) != [])
    check("dressing: running out of attempts fails",
          stale_dressing_problems(dict(dgood, reason="bound"), 2) != [])
    check("dressing: a missing stop reason fails",
          stale_dressing_problems({k: v for k, v in dgood.items() if k != "reason"}, 2) != [])
    check("dressing: a missing timestamp fails",
          stale_dressing_problems(dict(dgood, before={"uid": 2, "at": None, "typed": False}), 2) != [])
    check("dressing: nil == nil never passes",
          stale_dressing_problems(dict(dgood, before=None, after=None, same=True), 2) != [])
    check("dressing: a negative bleed rate fails",
          stale_dressing_problems(dict(dgood, bleedRate=-0.1), 2) != [])
    check("dressing: missing blood fails",
          stale_dressing_problems(dict(dgood, blood=None), 2) != [])

    # stale_wait_verdict
    wgood = {"pose": "standing", "same": True, "age": 10.6, "blood": 4.9,
             "bleedRate": 0.0}
    check("wait: an aged, unchanged hit on a live subject is 'aged'",
          stale_wait_verdict(wgood) == "aged")
    check("wait: a young hit is 'waiting'",
          stale_wait_verdict(dict(wgood, age=4.0)) == "waiting")
    check("wait: a missing pose is invalid",
          stale_wait_verdict({k: v for k, v in wgood.items() if k != "pose"})
          not in ("aged", "waiting"))
    check("wait: a malformed pose is invalid",
          stale_wait_verdict(dict(wgood, pose=7)) not in ("aged", "waiting"))
    check("wait: an unknown pose is invalid",
          stale_wait_verdict(dict(wgood, pose="crawling")) not in ("aged", "waiting"))
    check("wait: missing blood is invalid",
          stale_wait_verdict(dict(wgood, blood=None)) not in ("aged", "waiting"))
    check("wait: a negative bleed rate is invalid",
          stale_wait_verdict(dict(wgood, bleedRate=-1.0)) not in ("aged", "waiting"))
    check("wait: a dead subject is invalid",
          stale_wait_verdict(dict(wgood, pose="dead")) not in ("aged", "waiting"))
    check("wait: a collapsed subject is invalid",
          stale_wait_verdict(dict(wgood, pose="collapsed")) not in ("aged", "waiting"))
    check("wait: a changed record is invalid",
          stale_wait_verdict(dict(wgood, same=False)) not in ("aged", "waiting"))
    check("wait: an unreadable age is invalid",
          stale_wait_verdict(dict(wgood, age=None)) not in ("aged", "waiting"))
    check("wait: no answer is invalid", stale_wait_verdict(None) not in ("aged", "waiting"))

    # stale_demo_verdict
    check("stale demo: observed stale discard + valid restage passes", stale_demo_verdict(True, True))
    check("stale demo: a fresh pass alone fails", not stale_demo_verdict(False, True))
    check("stale demo: an observed discard without a valid restage fails",
          not stale_demo_verdict(True, False))

    # classify_lashout_selection / stale_selection_observed: a complete,
    # typed record is required before any arithmetic; each predicate is
    # compared across the bracket; the attacker is judged on the
    # decision's own reads.
    def cand(uid, dist, eligible=True, exists=True, pose="standing"):
        return {"uid": uid, "exists": exists, "pose": pose, "dist": dist,
                "eligible": eligible}

    def side(t, a, d, ae, hit_by):
        # The booleans Lua would compute from these (unrounded) values.
        hit = hit_by == 2
        return {"now": t, "nowOk": True, "attacker": cand(2, a, ae), "decoy": cand(3, d),
                "window": hit and 0 <= t <= 10.0, "stale": hit and t > 10.0,
                "closer": d < a}

    def mksel(t0, t1, a0=3, a1=3, d0=1, d1=1, ae0=True, ae1=True,
              hit_by=2, target=2, replay=True):
        return {"window": 10.0, "range": 8.0, "meCaptured": True,
                "me": {"gridX": 0, "gridY": 0},
                "hitBy": hit_by, "hitAt": 0.0, "hitTyped": True,
                "target": target, "targetTyped": True, "ordered": t1 >= t0,
                "before": side(t0, a0, d0, ae0, hit_by),
                "after": side(t1, a1, d1, ae1, hit_by),
                "decision": {"attackerEligible": replay}}

    def kind(sel):
        return classify_lashout_selection(sel, 2, 3)[0]

    def with_(sel, path, value):
        import copy
        out = copy.deepcopy(sel)
        node = out
        for k in path[:-1]:
            node = node[k]
        if value is KeyError:
            node.pop(path[-1], None)
        else:
            node[path[-1]] = value
        return out

    base = mksel(5.0, 5.01)
    check("record: a complete fair record is fair", kind(base) == "fair")
    check("record: empty bracket sides fail closed, no exception",
          kind({"before": {}, "after": {}, "meCaptured": True}) == "setup")
    check("record: None fails closed", kind(None) == "setup")
    for name, path, value in (
            ("a missing hit timestamp", ("hitAt",), KeyError),
            ("a hit timestamp defaulted to -1", ("hitAt",), -1),
            ("a hit Lua found untyped", ("hitTyped",), False),
            ("a negative hit timestamp", ("hitAt",), -0.5),
            ("hitBy as a float", ("hitBy",), 2.0),
            ("hitBy as a bool", ("hitBy",), True),
            ("no selected target (-1)", ("target",), -1),
            ("a float target", ("target",), 3.0),
            ("a target Lua found untyped", ("targetTyped",), False),
            ("a negative clock", ("before", "now"), -1.0),
            ("a clock Lua found invalid", ("after", "nowOk"), False),
            ("a window test that is not a boolean", ("before", "window"), "yes"),
            ("a missing closer test", ("after", "closer"), KeyError),
            ("a float candidate uid", ("before", "attacker", "uid"), 2.0),
            ("a missing clock order", ("ordered",), KeyError),
            ("a non-numeric hit timestamp", ("hitAt",), "10"),
            ("an infinite hit timestamp", ("hitAt",), float("inf")),
            ("a missing hitBy", ("hitBy",), KeyError),
            ("a missing target", ("target",), KeyError),
            ("a missing window", ("window",), KeyError),
            ("a zero range", ("range",), 0),
            ("an uncaptured subject position", ("meCaptured",), False),
            ("a malformed subject position", ("me", "gridX"), None),
            ("a missing before clock", ("before", "now"), KeyError),
            ("a boolean clock", ("after", "now"), True),
            ("eligibility as a string", ("before", "attacker", "eligible"), "true"),
            ("exists as a number", ("after", "decoy", "exists"), 1),
            ("a non-string pose", ("before", "attacker", "pose"), None),
            ("a NaN distance", ("after", "attacker", "dist"), float("nan")),
            ("a candidate for the wrong unit", ("before", "decoy", "uid"), 9),
            ("a missing candidate", ("after", "attacker"), KeyError),
            ("a raised reading", ("after",), {"err": "boom"}),
            ("a missing decision replay", ("decision",), KeyError),
            ("a malformed decision replay", ("decision", "attackerEligible"), 1),
            ("an absent decision replay value", ("decision", "attackerEligible"), KeyError)):
        bad = with_(base, path, value)
        check(f"record: {name} is a named setup failure",
              kind(bad) == "setup" and not stale_selection_observed(bad, 2, 3))
    backwards = mksel(10.6, 10.5, target=3)
    check("record: a clock going backwards (load or reset) is a setup failure",
          classify_lashout_selection(backwards, 2, 3)[0] == "setup"
          and "not ordered" in classify_lashout_selection(backwards, 2, 3)[1][0])
    check("record: a genuine wrong target (any positive uid) stays observable",
          kind(mksel(5.0, 5.01, target=7)) == "fair")
    rounded = with_(mksel(10.0, 10.0, target=3), ("after", "window"), False)
    rounded = with_(rounded, ("after", "stale"), True)
    check("precision: Lua's unrounded test outranks the printed number "
          "(after prints 10.0 but Lua found it past the window) -> ambiguous",
          kind(rounded) == "ambiguous")
    unordered = with_(mksel(10.0, 10.0), ("ordered",), False)
    check("precision: equal printed clocks Lua found DEcreasing -> setup",
          kind(unordered) == "setup")
    check("record: ... and is never stale evidence",
          not stale_selection_observed(backwards, 2, 3))

    crossing = mksel(9.99, 10.01, target=3)
    check("bracket: 9.99 -> 10.01 across the decision is ambiguous", kind(crossing) == "ambiguous")
    check("bracket: a window crossing is not stale evidence",
          not stale_selection_observed(crossing, 2, 3))
    check("bracket: the old single reading would have graded the crossing (documents the bug)",
          lashout_setup_problems(lashout_decision_view(crossing, "before"), 2, 3) == [])
    check("bracket: a valid wrong target is still graded (a policy failure, never retried)",
          kind(mksel(5.0, 5.01, target=3)) == "fair")
    stale = mksel(10.5, 10.52, target=3)
    check("bracket: past the window on both sides is a setup discard", kind(stale) == "setup")
    check("bracket: past the window BEFORE the decision is stale evidence",
          stale_selection_observed(stale, 2, 3))
    check("bracket: exactly at the window on both sides is fair (inclusive, as the policy)",
          kind(mksel(10.0, 10.0)) == "fair")
    check("bracket: attacker crossing the range during the decision is ambiguous",
          kind(mksel(5.0, 5.01, a0=8, a1=9, ae1=False)) == "ambiguous")
    check("bracket: attacker out of range on both sides is a setup discard",
          kind(mksel(5.0, 5.01, a0=9, a1=9, ae0=False, ae1=False)) == "setup")
    check("bracket: decoy losing 'strictly closer' during the decision is ambiguous",
          kind(mksel(5.0, 5.01, d0=2, d1=3)) == "ambiguous")
    diff = mksel(9.5, 10.5, a0=9, a1=3, ae0=False, ae1=True, target=3)
    check("bracket: DIFFERENT failures on the two sides are ambiguous, not setup",
          kind(diff) == "ambiguous" and not stale_selection_observed(diff, 2, 3))
    check("bracket: stale + a crossing elsewhere is ambiguous, not stale evidence",
          not stale_selection_observed(mksel(10.5, 10.6, a0=8, a1=9, ae1=False, target=3), 2, 3))
    check("bracket: a hit by someone else is a setup discard, not stale evidence",
          kind(mksel(10.5, 10.6, hit_by=9)) == "setup"
          and not stale_selection_observed(mksel(10.5, 10.6, hit_by=9), 2, 3))
    check("decision: eligible on both sides but ineligible on the decision's own reads is ambiguous",
          kind(mksel(5.0, 5.01, target=3, replay=False)) == "ambiguous")
    check("decision: unreplayable decision reads are a setup discard",
          kind(mksel(5.0, 5.01, replay="missing")) == "setup"
          and kind(mksel(5.0, 5.01, replay="error")) == "setup")

    # The PRODUCTION policy, no engine.
    ran = lashout_policy_harness(9.99, 10.01, 10.02)
    if ran is None:
        print("  skip harness: no `lua` interpreter installed")
    else:
        sel, callers = ran
        order = [callers[k] for k in sorted(callers, key=int)
                 if callers[k] in ("reading", "pickLashoutTarget")] \
            if isinstance(callers, dict) else None
        check("harness: the policy reads the clock once, between the two observer readings",
              order == ["reading", "pickLashoutTarget", "reading"])
        check("harness: observer 9.99 / production 10.01 -> production falls back to the decoy",
              isinstance(sel, dict) and sel.get("target") == 3)
        check("harness: ... and the probe classifies it ambiguous, not a policy failure",
              kind(sel) == "ambiguous")
        sel, _ = lashout_policy_harness(5.0, 5.01, 5.02)
        check("harness: a fair decision picks the attacker, replays eligible, and is graded",
              sel.get("target") == 2 and sel["decision"]["attackerEligible"] is True
              and kind(sel) == "fair")
        sel, _ = lashout_policy_harness(10.5, 10.51, 10.52)
        check("harness: a hit stale before the decision -> decoy, setup discard, stale evidence",
              sel.get("target") == 3 and kind(sel) == "setup"
              and stale_selection_observed(sel, 2, 3))
        sel, _ = lashout_policy_harness(5.0, 5.01, 5.02, attacker_x=9)
        check("harness: attacker out of range on every read -> decoy, setup discard",
              sel.get("target") == 3 and kind(sel) == "setup")
        sel, _ = lashout_policy_harness(5.0, 5.01, 5.02, attacker_x_decision=9)
        both = (lashout_setup_problems(lashout_decision_view(sel, "before"), 2, 3) == []
                and lashout_setup_problems(lashout_decision_view(sel, "after"), 2, 3) == [])
        check("harness: range there-and-back at production's own read -> decoy, though both "
              "observer readings are fair (the bracket alone would grade it)",
              sel.get("target") == 3 and both)
        check("harness: ... and the decision replay classifies it ambiguous, not a policy failure",
              sel["decision"]["attackerEligible"] is False and kind(sel) == "ambiguous")
        sel, _ = lashout_policy_harness(5.0, 5.01, 5.02, attacker_pose_decision="collapsed")
        check("harness: pose there-and-back (collapsed at production's read) -> decoy, ambiguous",
              sel.get("target") == 3 and kind(sel) == "ambiguous")
        eps = 10.000000000000002
        sel, _ = lashout_policy_harness(10.0, eps, eps)
        check("harness precision: 10+eps arrives printed as 10.0 (production tostring)",
              sel["after"]["now"] == 10.0 and sel["before"]["now"] == 10.0)
        check("harness precision: production saw 10+eps > window and fell back to the decoy",
              sel.get("target") == 3)
        check("harness precision: the printed numbers alone would read the window as held",
              0 <= sel["after"]["now"] - sel["hitAt"] <= sel["window"])
        check("harness precision: ... but Lua's own test says after is past the window "
              "-> ambiguous, not a policy failure",
              sel["after"]["window"] is False and kind(sel) == "ambiguous")
        sel, _ = lashout_policy_harness(eps, eps, eps)
        check("harness precision: 10+eps on every read -> decoy, setup, stale evidence "
              "(printed as 10.0)",
              sel.get("target") == 3 and sel["before"]["now"] == 10.0
              and kind(sel) == "setup" and stale_selection_observed(sel, 2, 3))
        sel, _ = lashout_policy_harness(10.0, 10.0, 10.0)
        check("harness precision: exactly 10.0 everywhere -> attacker, fair (inclusive window)",
              sel.get("target") == 2 and kind(sel) == "fair")
        sel, _ = lashout_policy_harness(
            5.0, 5.01, 5.02,
            policy_patch=("<= LASHOUT_ATTACKER_WINDOW", "< -1"))
        check("harness: a BROKEN policy picking the decoy under fair conditions is graded "
              "fair, i.e. a policy failure",
              sel.get("target") == 3 and kind(sel) == "fair")

    # swap_exercised (9c2)
    V, A = 9, 10
    sw = {"g": 10.0, "pre": V, "post": V, "la": A, "at": 8.5, "age": 1.5, "d": 1.0,
          "reach": 1.4, "seen": "collapsed", "real": "standing", "victimElig": True,
          "alive": True}
    for name, calls, want in (
            ("all valid calls keep the victim -> pass", [sw, dict(sw, g=10.1)], "pass"),
            ("a valid swap then a good call -> policy", [dict(sw, post=A), dict(sw, g=10.2)], "policy"),
            ("post nil -> policy", [dict(sw, post=-1)], "policy"),
            ("post another uid -> policy", [dict(sw, post=42)], "policy"),
            ("no valid calls -> setup", [dict(sw, age=3.5), dict(sw, la=-1), dict(sw, alive=False)], "setup"),
            ("no calls -> setup", [], "setup"),
            ("an invalid (stale) swap -> setup", [dict(sw, age=4.0, post=A)], "setup"),
            ("valid good + invalid swap -> pass", [sw, dict(sw, seen="standing", post=A)], "pass")):
        check(f"9c2 gate: {name}", swap_exercised(calls, V, A)[0] == want)

    print(f"mental_state_probe self-test: "
          f"{'all pass' if not fails else str(len(fails)) + ' FAIL'}")
    return 1 if fails else 0


def main():
    ap = argparse.ArgumentParser()
    ap.add_argument("--port", type=int, default=9352)
    ap.add_argument("--self-test", action="store_true",
                    help="#2773: run the no-engine regression cases for the "
                         "setup validators and gates, then exit")
    ap.add_argument("--lashout-case", choices=("stale", "ineligible"),
                    help="#2773 demonstration: run ONLY phase 9a, with its "
                         "setup pushed off the fair-test preconditions "
                         "(see perturb_lashout_case)")
    args = ap.parse_args()
    if args.self_test:
        return self_test()
    P = args.port

    proc = boot(P, log=LOG)
    ok = True
    try:
        bootstrap(P)

        if args.lashout_case:
            tune(P, EPISODE_MIN=LASHOUT_9AB_EPISODE, EPISODE_MAX=LASHOUT_9AB_EPISODE)
            graded_ok, _, _, _, stale_observed = lashout_attacker_preference(
                P, case=args.lashout_case)
            if args.lashout_case == "stale":
                passed = stale_demo_verdict(stale_observed, graded_ok)
                if not stale_observed:
                    print("  [FAIL] stale demonstration not observed: no AI "
                          "selection with the hit past the window was discarded")
            else:
                passed = graded_ok
            print(f"\n{'PASS' if passed else 'FAIL'} — phase 9a only "
                  f"(--lashout-case {args.lashout_case})")
            return 0 if passed else 1

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
            graded_ok, lash, attacker, decoy, _ = lashout_attacker_preference(
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
