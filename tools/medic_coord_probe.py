#!/usr/bin/env python3
"""Headless medic-coordination probe.

Verifies the squad-medic auto-treat coordination fix in scripts/unit_ai.lua
(bestMedicFor / medicAvailable / distance-discounted selection). The bug:
a free LESSER medic refused to treat a second bleeding ally because it
deferred to the most-CAPABLE medic — even when that medic was already busy
treating someone else (only an in-combat best medic freed a lesser one).
Two simultaneously-bleeding allies therefore serialised onto one medic.

Scenario (flat arena, no kits → makeshift-tourniquet treat path):
    cluster A:  M1 (best medic, bleed_control 90) next to P1
    cluster B:  M2 (lesser medic, bleed_control 30) next to P2
  with a wide gap between the clusters. P1/P2 have bleed_control 0 so they
  are NOT medics. P1 gets THREE separate wounds, P2 gets one (see below).

Expected with the FIX:
  * M1 claims P1; M2 claims P2 (M1 is committed elsewhere, so M2 — the only
    other available allied medic — steps in for P2).
  * BOTH patients get dressed, roughly in parallel.
  * M2 moves from its spawn and engages (the crisp tell: under the bug M2
    defers entirely and never moves; P2 only gets treated much later, by M1
    walking the whole gap).

Determinism notes (#589 — stabilized for the blocking CI gate):
  * Fresh acolytes carry a standing find_water goal that can walk them off
    their mark before the wounds even land, adding run-to-run position/
    timing noise; every spawned unit gets it quieted (+ any resulting walk
    cancelled) up front.
  * P1's single wound let M1 dress it (and release its patient claim) in
    ONE think-tick — M1 is adjacent to P1, and the no-kit tourniquet path
    always succeeds on the first attempt. That's too short a window: once
    M1 is free again, bestMedicFor ranks the far-but-far-more-capable M1
    over the near M2 for ANY patient (the capability gap swamps the
    gentle distance discount), so a slow M2 think-tick could lose P2 back
    to M1 — reproducing the exact bug this probe exists to catch. THREE
    separate wounds force M1 through several sequential treatBleeding
    calls (one per think-tick) before it's free, giving M2 a comfortable
    multi-second margin to lock its own claim on P2 first.
  * A stray `unit_warning` (e.g. an unrelated stuck-walk watchdog) auto-
    pauses the whole sim per `config/notifications.yaml`; the poll loop
    defensively un-pauses every iteration so a passing hiccup can't freeze
    the scenario for the rest of the run.

Usage:
  python3 tools/medic_coord_probe.py
  python3 tools/medic_coord_probe.py --port 9131 --seconds 40

Exit 0 = both patients treated AND the lesser medic engaged (fix works).
"""
from __future__ import annotations

import argparse
import ast
import glob
import socket
import subprocess
import sys
import time
from probelib import quit_engine, boot, send, clear_find_water

LOG = "/tmp/medic_coord_engine.log"


def bootstrap(port: int) -> None:
    """Load defs + the stat/resource/AI ticks the loading screen would.
    unit_ai is left ACTIVE — its update() tick is exactly what we test."""
    loaders = [
        ("data/substances/*.yaml", "engine.loadSubstanceYaml"),
        ("data/items/*.yaml",      "engine.loadItemYaml"),
        ("data/equipment/*.yaml",  "engine.loadEquipmentYaml"),
        ("data/materials/*.yaml",  "engine.loadMaterialYaml"),
        ("data/units/*.yaml",      "engine.loadUnitYaml"),
    ]
    for pattern, fn in loaders:
        for path in sorted(glob.glob(pattern)):
            send(port, f"{fn}('{path}'); return 'ok'")
    for script, dt in [("unit_stats", 0.1), ("unit_resources", 0.2),
                       ("unit_ai", 0.1)]:
        send(port, f"engine.loadScript('scripts/{script}.lua', {dt}); return 'ok'")
    send(port, "require('scripts.movement_arena'); return 'ok'")


def num(s: str) -> float:
    try:
        return float(s.strip().strip('"'))
    except ValueError:
        return float("nan")


def pos(port: int, uid: int):
    """(gx, gy) of a unit, or None if gone."""
    r = send(port, f"local i=unit.getInfo({uid}); if not i then return 'none' end; "
                   f"return i.gridX..','..i.gridY")
    r = r.strip('"')
    if "," not in r:
        return None
    a, b = r.split(",")
    return (float(a), float(b))


def worst_dressing(port: int, uid: int):
    """(dressing, seep) of the unit's most-dressed wound, ('', 1.0) if none/
    untreated, or None if the unit is gone. seep<1 or dressing!='' = treated."""
    lua = (
        f"local ws=unit.getWounds({uid}); if type(ws)~='table' then return 'none' end; "
        f"local d,seep='',1.0; for _,w in ipairs(ws) do "
        f"if (w.bandage or 1)<seep then seep=w.bandage end; "
        f"if (w.dressing or '')~='' then d=w.dressing end end; "
        f"return d..'|'..seep"
    )
    r = send(port, lua).strip('"')
    if r == "none":
        return None
    if "|" not in r:
        return ("", 1.0)
    d, seep = r.split("|")
    return (d, num(seep))


def dist(a, b) -> float:
    return ((a[0] - b[0]) ** 2 + (a[1] - b[1]) ** 2) ** 0.5


def spawn_settled(port: int, x: float, y: float, label: str) -> int:
    """Spawn an acolyte and quiet its find_water default goal (+ cancel any
    walk it already started), so the four scenario units hold their spawn
    marks instead of scouting for water before the wounds land."""
    uid = int(num(send(port, f"return unit.spawn('acolyte', {x}, {y})")))
    if uid <= 0:
        sys.exit(f"unit.spawn failed for {label}: {uid}")
    if not clear_find_water(port, uid):
        sys.exit(f"could not quiet find_water for {label} (uid {uid})")
    send(port, f"unit.stop({uid})", expect_result=False)
    return uid


def main() -> int:
    ap = argparse.ArgumentParser(description=__doc__)
    ap.add_argument("--port", type=int, default=9131)
    ap.add_argument("--seconds", type=float, default=40.0)
    ap.add_argument("--gap", type=int, default=20, help="tiles between clusters")
    args = ap.parse_args()

    proc = boot(args.port, log=LOG)
    try:
        bootstrap(args.port)
        # Flat arena (z=0 loam everywhere) via the movement_arena module.
        send(args.port, "return require('scripts.movement_arena').buildCourse('flat')")
        time.sleep(0.5)

        g = args.gap
        # cluster A near origin, cluster B `gap` tiles east.
        m1 = spawn_settled(args.port, 0, 0, "M1")
        p1 = spawn_settled(args.port, 1, 0, "P1")
        m2 = spawn_settled(args.port, g, 0, "M2")
        p2 = spawn_settled(args.port, g + 1, 0, "P2")
        print(f"spawned  M1=#{m1}@(0,0)  P1=#{p1}@(1,0)  "
              f"M2=#{m2}@({g},0)  P2=#{p2}@({g+1},0)")

        # Make M1 clearly the best medic, M2 a lesser one, and disable the
        # patients as medics (level 0 → zero capability).
        send(args.port, f"unit.setKnowledge({m1},'bleed_control',90); return 'ok'")
        send(args.port, f"unit.setKnowledge({m2},'bleed_control',30); return 'ok'")
        send(args.port, f"unit.setKnowledge({p1},'bleed_control',0);  return 'ok'")
        send(args.port, f"unit.setKnowledge({p2},'bleed_control',0);  return 'ok'")
        time.sleep(1.0)  # let units settle onto the ground

        # Wound P1 with THREE separate bleeders (distinct body parts) so M1
        # needs several sequential treatBleeding calls — one per think-tick
        # — before its patient claim releases (see the determinism note in
        # the module docstring). P2 gets a single wound as before.
        send(args.port, f"return unit.injure({p1},'l_thigh','stab',0.25,1.0)")
        send(args.port, f"return unit.injure({p1},'r_thigh','stab',0.25,1.0)")
        send(args.port, f"return unit.injure({p1},'l_shin','stab',0.25,1.0)")
        send(args.port, f"return unit.injure({p2},'l_thigh','stab',0.25,1.0)")
        print("injured P1 (3 wounds) and P2 (1 wound), stab sev 0.25")

        p2_spawn = pos(args.port, p2) or (float(g + 1), 0.0)

        p1_treated_at = None
        p2_treated_at = None
        # Distance from M1 to P2 at the moment P2 is first treated. If M1 is
        # still far from P2 then a SECOND medic (M2) must have done it — the
        # whole point of the fix. Under the bug M2 defers, so P2 is only
        # dressed once M1 walks the gap (M1 adjacent to P2, distance ~1).
        m1_dist_when_p2_treated = None
        timeline = []
        t0 = time.time()
        while time.time() - t0 < args.seconds:
            t = time.time() - t0
            # A stray unit_warning (e.g. an unrelated stuck-walk watchdog)
            # auto-pauses the whole sim per config/notifications.yaml —
            # defensively keep unpausing so a passing hiccup elsewhere
            # can't freeze this scenario for the rest of the run.
            send(args.port, "engine.setPaused(false)", expect_result=False)
            pm1 = pos(args.port, m1)
            pm2 = pos(args.port, m2)
            d1 = worst_dressing(args.port, p1)
            d2 = worst_dressing(args.port, p2)
            if p1_treated_at is None and d1 and (d1[0] != "" or d1[1] < 0.95):
                p1_treated_at = t
            if p2_treated_at is None and d2 and (d2[0] != "" or d2[1] < 0.95):
                p2_treated_at = t
                if pm1:
                    m1_dist_when_p2_treated = dist(pm1, p2_spawn)
            timeline.append((t, pm1, pm2, d1, d2))
            if p1_treated_at is not None and p2_treated_at is not None:
                break
            time.sleep(0.5)

        print("\n--- timeline (t  M1pos  M2pos  P1[dress|seep]  P2[dress|seep]) ---")
        for t, pm1, pm2, d1, d2 in timeline[::max(1, len(timeline) // 16)]:
            def f(p): return f"({p[0]:.0f},{p[1]:.0f})" if p else "gone"
            def fd(d): return f"{d[0] or '-'}|{d[1]:.2f}" if d else "gone"
            print(f"  {t:5.1f}  {f(pm1):>8}  {f(pm2):>8}  {fd(d1):>14}  {fd(d2):>14}")

        print("\n--- results ---")
        print("  P1 treated at      : " +
              (f"{p1_treated_at:.1f}s" if p1_treated_at is not None else "NEVER"))
        print("  P2 treated at      : " +
              (f"{p2_treated_at:.1f}s" if p2_treated_at is not None else "NEVER"))
        print("  M1 dist→P2 @P2-treat: " +
              (f"{m1_dist_when_p2_treated:.1f} tiles"
               if m1_dist_when_p2_treated is not None else "n/a"))

        both = p1_treated_at is not None and p2_treated_at is not None
        # P2 treated by a DIFFERENT medic than M1: M1 was far from P2 when it
        # happened (couldn't have been M1). gap=20 → fix gives ~20, bug ~1.
        second_medic = (m1_dist_when_p2_treated is not None
                        and m1_dist_when_p2_treated > 5.0)

        print("\n--- checks ---")
        print(f"  both patients treated            : {both}")
        print(f"  P2 treated by the LESSER medic   : {second_medic} "
              f"(M1 was {m1_dist_when_p2_treated:.1f} tiles away > 5.0)"
              if m1_dist_when_p2_treated is not None else
              f"  P2 treated by the LESSER medic   : {second_medic}")
        ok = both and second_medic
        print(f"\n  {'PASS' if ok else 'FAIL'}: "
              f"{'two medics treated in parallel' if ok else 'coordination broken'}")
        return 0 if ok else 1
    finally:
        quit_engine(args.port, proc)


if __name__ == "__main__":
    sys.exit(main())
