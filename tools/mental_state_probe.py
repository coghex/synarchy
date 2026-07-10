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
