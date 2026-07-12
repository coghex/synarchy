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
     through the normal cooldown path.
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

Usage: python3 tools/mental_state_probe.py [--port 9352]
Exit 0 = pass.
"""
from __future__ import annotations
import argparse, glob, json, sys, time
from probelib import (boot, init_arena, load_ai_stack, poll_until,
                      quit_engine, send, spawn_acolyte)

LOG = "/tmp/mental_state_probe_engine.log"


def bootstrap(port):
    loaders = [
        ("data/substances/*.yaml", "engine.loadSubstanceYaml"),
        ("data/infections/*.yaml", "engine.loadInfectionYaml"),
        ("data/items/*.yaml",      "engine.loadItemYaml"),
        ("data/equipment/*.yaml",  "engine.loadEquipmentYaml"),
        ("data/materials/*.yaml",  "engine.loadMaterialYaml"),
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


def main():
    ap = argparse.ArgumentParser()
    ap.add_argument("--port", type=int, default=9352)
    args = ap.parse_args()
    P = args.port

    proc = boot(P, log=LOG)
    ok = True
    try:
        bootstrap(P)

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

        # ---- 9. Lash-out (#717): target policy, exclusions, target
        # loss/replacement, no-target wander, real attack behavior, and
        # episode-end cleanup. Each scenario uses a fresh, isolated unit
        # cluster (>8 tiles from every other cluster) so LASHOUT_RANGE
        # never bridges them.

        # 9a. Prefers a recent eligible attacker over a closer decoy.
        # There's no Lua setter for the last-attacker memory (only
        # unit.getLastAttacker), so stage a REAL landed hit —
        # combat_anim_probe.py's spawn-adjacent + commandAttack +
        # wait-for-a-swing-to-land pattern.
        lash = spawn_acolyte(P, -30, -25)
        set_wellbeing(P, lash, 1.0, 0.0)
        poll_until(5, lambda: mstate(P, lash) == "stable")
        attacker = spawn_acolyte(P, -29, -25)
        # Neuter BOTH units' damage output before staging the hit — a
        # full-strength acolyte swing landed on an unarmored target can
        # be lethal in one blow (Combat.Resolution's E_swing scales with
        # strength), and a dead unit stops ticking mental_state entirely
        # (unit_resources skips dead/collapsed units), which would wedge
        # every check below forever. This includes lash's own retaliation
        # once it's hit (ordinary combat AI fights back before lash-out
        # takes over below) — either direction landing a full-strength
        # hit risks killing the other side. We only need landed hits to
        # register, not a realistic fight. toughness=100 caps
        # Combat.Resolution's energy-transfer reduction at its 50% max
        # (clamp(toughness*0.05, 0, 0.5)) regardless of roll variance —
        # a second, independent line of defense on top of the strength
        # nerf (strength alone left a small residual death chance).
        send(P, f"unit.setStat({attacker},'strength',0.05); "
                f"unit.setStat({attacker},'toughness',100); "
                f"unit.setStat({lash},'strength',0.05); "
                f"unit.setStat({lash},'toughness',100); return 'ok'")
        send(P, f"require('scripts.unit_ai').commandAttack({attacker},{lash}); "
                f"return 'ok'")
        hit = poll_until(35, lambda: send(
            P, f"local a=unit.getLastAttacker({lash}); return a and a.uid or 'nil'"
        ) == str(attacker))
        if hit:
            print(f"  [pass] staged a real hit: attacker {attacker} landed on "
                  f"lash-out subject {lash}")
        else:
            ok = False
            print(f"  [FAIL] attacker {attacker} never landed a hit on {lash} "
                  f"— can't test attacker preference")

        # Stop BOTH sides' ordinary combat AI immediately. Lash's own AI
        # retaliates against attacker via the normal incoming_hit/engage
        # mechanism the instant it's hit — left running for the several
        # seconds the rest of this setup takes, sustained mutual combat
        # can collapse either side from accumulated blood loss even with
        # toughness/strength reduced (those shrink per-hit severity, not
        # how long the fight runs), and a collapsed attacker is exactly
        # what lash-out's own eligibility correctly excludes — silently
        # invalidating this "prefers a live attacker" scenario. Draining
        # attacker's stamina below the acolyte config's
        # wander_min_stamina_fraction (0.2 — never to 0, the engine's
        # universal death rule) also disables its ambient wander, so it
        # stays a stationary target once its goal is cleared.
        send(P, f"local ai=require('scripts.unit_ai') "
                f"local ids={{{attacker},{lash}}} "
                f"for _,u in ipairs(ids) do "
                f"local s=ai.getState(u); "
                f"if s then ai.markGoalAccomplished(s,'attack'); "
                f"s.attackTargetUid=nil end; unit.stop(u) end "
                f"local st=require('scripts.unit_stats') "
                f"unit.setStat({attacker},'stamina', "
                f"st.get({attacker},'max_stamina')*0.1); return 'ok'")

        # Unconditionally revive both — whatever the brief exchange above
        # left them at, this scenario needs attacker standing to test
        # "prefers a live recent attacker" at all (a collapsed one is
        # correctly excluded by lash-out's own eligibility, invalidating
        # the check for an unrelated reason), and needs lash conscious
        # since a collapsed unit's AI doesn't tick at all.
        send(P, f"unit.revive({attacker}); unit.revive({lash}); return 'ok'")

        # Snap attacker back to a fixed, known distance from lash — the
        # swing that landed above can knock either side back by an
        # unpredictable amount, and the stop command above only takes
        # effect on the next tick, leaving a brief window where it could
        # still drift. teleporting removes that uncertainty entirely
        # rather than trusting wherever combat physics left it.
        lx, ly = unit_pos(P, lash)
        send(P, f"unit.setPos({attacker}, {lx + 1}, {ly}); return 'ok'")
        # unit.setPos just enqueues a UnitTeleport — confirm it actually
        # landed before trusting the new distance (it's processed on the
        # unit thread, asynchronously from this console command).
        if not poll_until(5, lambda: (lambda x, y:
                (x - (lx + 1)) ** 2 + (y - ly) ** 2 < 0.05)(*unit_pos(P, attacker))):
            ok = False
            print(f"  [FAIL] setup: teleporting {attacker} back next to "
                  f"{lash} never took effect")
        d_att = 1.0
        # Place the decoy at HALF that distance — always strictly closer.
        decoy_dist = d_att / 2
        decoy = spawn_acolyte(P, lx + decoy_dist, ly)

        send(P, f"require('scripts.mental_state').forceBreak({lash},'lash_out'); "
                f"return 'ok'")

        def first_target():
            t = lash_target(P, lash)
            return t if t != "nil" else None
        target = poll_until(10, first_target)
        if target == str(attacker):
            print(f"  [pass] lash-out prefers the recent attacker {attacker} "
                  f"over the closer decoy {decoy}")
        else:
            ok = False
            print(f"  [FAIL] lash-out target={target}, expected attacker="
                  f"{attacker} (decoy={decoy})")

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
        # one-shot-kill risk applies here, in both directions.
        send(P, f"unit.setStat({attackerB},'strength',0.05); "
                f"unit.setStat({attackerB},'toughness',100); "
                f"unit.setStat({lashB},'strength',0.05); "
                f"unit.setStat({lashB},'toughness',100); return 'ok'")
        send(P, f"require('scripts.unit_ai').commandAttack({attackerB},{lashB}); "
                f"return 'ok'")
        hitB = poll_until(35, lambda: send(
            P, f"local a=unit.getLastAttacker({lashB}); return a and a.uid or 'nil'"
        ) == str(attackerB))
        if not hitB:
            ok = False
            print(f"  [FAIL] setup: {attackerB} never landed a hit on {lashB} "
                  f"— can't test the retaliation-swap exclusion")

        # Stop BOTH sides' ordinary combat AI immediately — see the
        # identical note in the 9a setup above. This test wants
        # attackerB's REPORTED pose patched to 'collapsed' below while it
        # stays actually healthy underneath; letting real mutual combat
        # run first risks genuinely collapsing lashB instead.
        send(P, f"local ai=require('scripts.unit_ai') "
                f"local ids={{{attackerB},{lashB}}} "
                f"for _,u in ipairs(ids) do "
                f"local s=ai.getState(u); "
                f"if s then ai.markGoalAccomplished(s,'attack'); "
                f"s.attackTargetUid=nil end; unit.stop(u) end; return 'ok'")
        # Unconditionally revive both (see the identical note in the 9a
        # setup) — this test wants attackerB's REPORTED pose patched to
        # 'collapsed' below while actually healthy underneath. Draining
        # its stamina below the acolyte config's wander_min_stamina_
        # fraction (0.2 — never to 0, the universal death rule) disables
        # ambient wander too, so revival doesn't send it drifting off
        # again before the teleport below is confirmed.
        send(P, f"unit.revive({attackerB}); unit.revive({lashB}); "
                f"local st=require('scripts.unit_stats') "
                f"unit.setStat({attackerB},'stamina', "
                f"st.get({attackerB},'max_stamina')*0.1); return 'ok'")

        # Snap attackerB back to a fixed, known distance from lashB — see
        # the identical note in the 9a setup above (knockback can drift
        # them beyond LASHOUT_RANGE, which would invalidate this check
        # for the wrong reason).
        lbx, lby = unit_pos(P, lashB)
        send(P, f"unit.setPos({attackerB}, {lbx + 1}, {lby}); return 'ok'")
        # Confirm the (async) teleport actually landed — see the 9a note.
        if not poll_until(5, lambda: (lambda x, y:
                (x - (lbx + 1)) ** 2 + (y - lby) ** 2 < 0.05)(*unit_pos(P, attackerB))):
            ok = False
            print(f"  [FAIL] setup: teleporting {attackerB} back next to "
                  f"{lashB} never took effect")

        # Pin attackerB's reported pose to 'collapsed' — unit.collapse()
        # alone only holds while every gating resource sits below its
        # revive threshold (see the dead/collapsed/technomule test
        # below), so this reuses the same wrap-and-delegate technique
        # for a reliable, deterministic pose throughout this check.
        send(P, f"if not _G.__probe_orig_getPose then "
                f"_G.__probe_orig_getPose = unit.getPose end; "
                f"unit.getPose = function(u) "
                f"if u == {attackerB} then return 'collapsed' end "
                f"return _G.__probe_orig_getPose(u) end; return 'ok'")

        send(P, f"require('scripts.mental_state').forceBreak({lashB},'lash_out'); "
                f"return 'ok'")

        # Sample rapidly through the retaliation-swap's own 3s window
        # (RETALIATE_WINDOW_SEC, timed from the hit staged above) —
        # lash-out must land on the eligible victimB and never once show
        # the collapsed attackerB sneaking in via the shared swap.
        samplesB = []
        deadline = time.time() + 3.0
        while time.time() < deadline:
            samplesB.append(lash_target(P, lashB))
            time.sleep(0.15)
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
