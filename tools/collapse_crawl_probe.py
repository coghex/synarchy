#!/usr/bin/env python3
"""Collapse↔crawl hysteresis probe (#304).

Drives the REAL engine locomotor state machine (unit_resources.tickInjuries)
to confirm the consciousness-hysteresis fix end-to-end.

Setup: an acolyte on flat ground with both legs broken — so a CONSCIOUS unit
crawls and an UNCONSCIOUS one collapses (the collapse↔crawl boundary we test).

Consciousness = min(temp, blood-oxygen, salt) (brain.lua). In a temperate arena
the temp/salt terms sit at 1.0, so blood_oxygen alone drives consciousness
(of = (o2-0.5)/0.3). Cardio drives blood_oxygen toward lungCap×delivery; with
HEALTHY lungs a collapsed unit's equilibrium lands right at consciousness ≈
RISE_AT (0.40) — the knife-edge that makes #304 flap. A small `internal` lungs
wound (pulmonaryFailure = severity) lowers that cap so the collapsed-state
equilibrium settles safely INSIDE the band, where physiology holds it.

Test:
  1. Collapse the unit (drive blood_oxygen to 0) and adaptively add lung damage
     until the COLLAPSED-state equilibrium consciousness lands mid-band — i.e.
     the unit STAYS collapsed there instead of rising back to crawling.
  2. HOLD: poll without writing. With the fix the unit stays collapsed across
     the whole band; before the fix it popped to crawling the instant
     consciousness cleared 0.15. (The lung wound heals slowly, so consciousness
     drifts gently UP through the band during the hold — a continuous sweep.)
  3. RISE: once consciousness reaches RISE_AT (0.40) the unit must finally crawl
     (legs broken → crawl, not stand).

Pass requires: ≥3 collapsed samples observed inside the band (hold exercised);
NO sample crawling while consciousness < RISE_AT (the fix — no premature crawl);
and the unit DOES crawl once consciousness ≥ RISE_AT (the rise gate releases).
The pure decision logic is covered exhaustively by the engine-free unit test
tools/test_collapse_hysteresis.lua.

Usage: python3 tools/collapse_crawl_probe.py [--port 9304]
Exit 0 = pass.
"""
from __future__ import annotations
import argparse, glob, json, socket, subprocess, sys, time
from probelib import boot, send

LOG = "/tmp/collapse_crawl_probe_engine.log"
RISE_AT = 0.40            # brain.lua RISE_AT — collapsed→up gate
UNCONSCIOUS_BELOW = 0.15  # brain.lua collapse trigger


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
    send(port, "engine.loadScript('scripts/unit_stats.lua', 0.1); return 'ok'")
    send(port, "engine.loadScript('scripts/unit_resources.lua', 0.2); return 'ok'")
    send(port, "pcall(function() require('scripts.unit_ai').update = "
               "function() end end); return 'ok'")
    send(port, "local t=require('scripts.thermo'); t.debugAmbient=22; "
               "t.debugHumidity=0.5; return 'ok'")
    send(port, "require('scripts.movement_arena').buildCourse('flat'); return 'ok'")


SNAP = (
    "local u=_U local b=require('scripts.brain') local i=require('scripts.injuries') "
    "return {pose=unit.getPose(u) or 'nil', c=b.consciousness(u), "
    "cw=(i.cannotWalk(u) and 1 or 0)}"
)


def snap(port):
    raw = send(port, SNAP)
    try:
        return json.loads(raw)
    except json.JSONDecodeError:
        return {"_raw": raw}


def main():
    ap = argparse.ArgumentParser()
    ap.add_argument("--port", type=int, default=9304)
    args = ap.parse_args()
    P = args.port

    proc = boot(P, log=LOG)
    try:
        bootstrap(P)
        uid = send(P, "local u=unit.spawn('acolyte',1,0); _U=u; return u")
        try:
            uid = int(float(uid))
        except ValueError:
            print(f"[FAIL] could not spawn acolyte: {uid}")
            return 1
        print(f"spawned acolyte uid={uid}")

        # Break both legs → cannotWalk (bandaged so they don't bleed out).
        for part in ("l_thigh", "r_thigh"):
            send(P, f"return unit.injure({uid},'{part}','fracture',0.9,0.0)")
        if send(P, "return require('scripts.injuries').cannotWalk(_U) and 1 or 0") != "1":
            print("[FAIL] unit not cannotWalk after leg breaks")
            return 1
        print("legs broken: cannotWalk = true")

        def force_collapse():
            """Drive blood_oxygen hard to 0 until the unit is collapsed."""
            for _ in range(20):
                send(P, "unit.setStat(_U,'blood_oxygen',0.0); return 'ok'")
                time.sleep(0.1)
                if snap(P).get("pose") == "collapsed":
                    return True
            return False

        def lift_and_settle(seconds):
            # Lift blood_oxygen off 0 quickly (a few writes BELOW the rise edge
            # so it can't trip an early rise), then stop writing and let cardio
            # settle O2 to its collapsed-state equilibrium (capped by any lung
            # damage). Avoids the very slow drift up from 0.
            for _ in range(6):
                send(P, "unit.setStat(_U,'blood_oxygen',0.55); return 'ok'")
                time.sleep(0.1)
            time.sleep(seconds)

        # ---- 1+2. Pin the COLLAPSED-state consciousness inside the band. ----
        # Cardio drives blood_oxygen to lungCap×delivery. With healthy lungs the
        # collapsed-state equilibrium lands at consciousness ≈ RISE_AT (0.40) —
        # right on the rise edge (this knife-edge is exactly why #304 flaps). A
        # small `internal` lungs wound (pulmonaryFailure = severity) lowers the
        # cap so the equilibrium settles safely INSIDE the band. Approach from
        # below: collapse, settle, read. If it rose to crawling the cap is still
        # too high → add a little lung damage and recollapse. Once it STAYS
        # collapsed with consciousness in (0.15, 0.40) the cap has landed.
        # Start with a lung wound near the expected landing severity (~0.3 on
        # this body plan) to skip the obviously-still-conscious low-cap
        # attempts; the loop refines from there.
        sev, landed = 0.21, False
        send(P, f"return unit.injure({uid},'lungs','internal',{sev:.2f},0.0)")
        if not force_collapse():
            print("[FAIL] unit never collapsed under forced deep dip")
            return 1
        for attempt in range(16):
            lift_and_settle(5.0)
            s = snap(P)
            c_eq, pose = float(s.get("c", 1.0)), s.get("pose")
            print(f"    cap attempt {attempt}: lung_sev={sev:.2f} c={c_eq:.3f} pose={pose}")
            # Require comfortable margin from BOTH band edges so the hold has
            # headroom (the sev=0 equilibrium asymptotes right at ~0.39).
            if pose == "collapsed" and 0.18 < c_eq < 0.36:
                landed = True
                break
            if pose == "collapsed" and c_eq <= UNCONSCIOUS_BELOW:
                print(f"[FAIL] overshot — collapsed-state c stuck at {c_eq:.3f}")
                return 1
            sev = round(sev + 0.03, 2)
            send(P, f"return unit.injure({uid},'lungs','internal',{sev:.2f},0.0)")
            force_collapse()
        if not landed:
            print("[FAIL] could not pin a collapsed unit's consciousness in the band")
            return 1
        print(f"  pinned: COLLAPSED with consciousness in band (lung_sev={sev:.2f})")

        # ---- 2b. HOLD: poll without writing; cardio holds consciousness at the
        # capped band value. With the fix the unit STAYS collapsed; the old code
        # would pop it to crawling the instant consciousness cleared 0.15.
        hold = []
        for _ in range(20):
            s = snap(P)
            hold.append((s.get("pose"), float(s.get("c", 0.0))))
            time.sleep(0.4)
        cs = [c for _, c in hold]
        premature = [round(c, 3) for p, c in hold if p == "crawling" and c < RISE_AT]
        band_held = [c for p, c in hold
                     if p == "collapsed" and UNCONSCIOUS_BELOW < c < RISE_AT]
        print(f"  band hold: {len(hold)} samples, consciousness {min(cs):.3f}..{max(cs):.3f}, "
              f"poses={sorted(set(p for p,_ in hold))}")

        # ---- 3. RISE: drive consciousness above RISE_AT; the unit must finally
        # crawl (legs broken → crawl, not stand). Confirms the gate releases.
        rise_ok, rise_c = False, None
        for _ in range(40):
            send(P, "unit.setStat(_U,'blood_oxygen',1.0); return 'ok'")
            s = snap(P)
            if s.get("pose") == "crawling" and float(s.get("c", 0.0)) >= RISE_AT:
                rise_ok, rise_c = True, float(s.get("c"))
                break
            time.sleep(0.25)

        ok = True
        if len(band_held) < 3:
            ok = False
            print(f"  [FAIL] only {len(band_held)} collapsed sample(s) in band "
                  f"— hold not exercised")
        else:
            print(f"  [pass] hold exercised: {len(band_held)} collapsed sample(s) in band "
                  f"(c {min(band_held):.3f}..{max(band_held):.3f})")

        if premature:
            ok = False
            print(f"  [FAIL] {len(premature)} sample(s) crawling while consciousness "
                  f"< {RISE_AT} (the #304 flap): c={premature[:5]}")
        else:
            print(f"  [pass] stayed collapsed across the whole band — no premature crawl")

        if not rise_ok:
            ok = False
            print(f"  [FAIL] unit never crawled even with consciousness ≥ {RISE_AT} "
                  f"(rise gate stuck)")
        else:
            print(f"  [pass] crawls once consciousness ≥ {RISE_AT} (rise works, c={rise_c:.3f})")

        print(f"\n{'PASS' if ok else 'FAIL'} — collapse↔crawl hysteresis (#304)")
        return 0 if ok else 1
    finally:
        send(P, "engine.quit(); return 'bye'")
        time.sleep(0.5)
        if proc.poll() is None:
            proc.kill()


if __name__ == "__main__":
    sys.exit(main())
