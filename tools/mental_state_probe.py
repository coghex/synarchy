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
     produces a real landed attack through the short-circuit; and
     clears every lash-out-owned goal/target once the episode ends.

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
        send(P, f"require('scripts.unit_ai').commandAttack({attacker},{lash}); "
                f"return 'ok'")
        hit = poll_until(20, lambda: send(
            P, f"local a=unit.getLastAttacker({lash}); return a and a.uid or 'nil'"
        ) == str(attacker))
        if hit:
            print(f"  [pass] staged a real hit: attacker {attacker} landed on "
                  f"lash-out subject {lash}")
        else:
            ok = False
            print(f"  [FAIL] attacker {attacker} never landed a hit on {lash} "
                  f"— can't test attacker preference")

        # Deliberately leave attacker's own commandAttack(lash) running —
        # it keeps both units engaged (no ambient-wander drift risk
        # between here and forcing the episode below) and doesn't affect
        # lash's own state, which is all the checks below assert on.
        lx, ly = unit_pos(P, lash)
        ax, ay = unit_pos(P, attacker)
        d_att = ((lx - ax) ** 2 + (ly - ay) ** 2) ** 0.5
        # Place the decoy at HALF the current attacker distance (rather
        # than a fixed offset) — attacker is still actively fighting lash
        # (see above), so its distance from lash varies run to run;
        # halving guarantees the decoy is strictly closer whenever
        # d_att > 0, regardless of how close combat has already drawn them.
        decoy_dist = max(0.02, d_att / 2)
        decoy = spawn_acolyte(P, lx + decoy_dist, ly)
        if decoy_dist >= d_att:
            ok = False
            print(f"  [FAIL] setup: decoy ({decoy_dist:.2f} away) not closer "
                  f"than attacker (d_att={d_att:.2f})")

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

        # 9c. Episode end clears every lash-out-owned goal/target.
        send(P, f"unit.setStat({lash},'mental_until',0); return 'ok'")
        poll_until(5, lambda: mstate(P, lash) != "break")

        def cleaned():
            f = lash_ai_flags(P, lash)
            return f if (not f["tgt"] and f["goal"] != "attack"
                         and not f["committed"]) else None
        cleared = poll_until(5, cleaned)
        if cleared:
            print(f"  [pass] episode end cleared lash-out combat state: {cleared}")
        else:
            ok = False
            print(f"  [FAIL] lash-out combat state leaked past episode end: "
                  f"{lash_ai_flags(P, lash)}")

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
