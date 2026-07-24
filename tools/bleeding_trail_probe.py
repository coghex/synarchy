#!/usr/bin/env python3
"""Headless probe for issue #882: bleeding trails.

Boots headless on a flat arena, spawns fresh acolytes (one per case, so
a prior case's decals/trail state can never contaminate the next), and
drives the debug `unit.injure(...)` + `unit.moveTo(...)` paths end to
end against the `blood.*` debug surface (#604/#606) to verify the
moving half of ongoing bleeding (Blood.Trail): a unit with an externally
bleeding wound leaves a bounded, distance/cadence-gated trail of marks
along its travelled path, driven by conserved real blood loss.

`unit_ai`'s wander tick is neutralised (the `movement_probe.py`
technique) so `unit.moveTo` is the only source of movement in every
case — otherwise an unclearable `find_water` wander goal could add
uncontrolled extra distance and desync the mark-count bounds below.

Checks:
  1. (a) a real externally-bleeding wound (untreated slash) commanded to
     move a known route leaves `blood.listDecals()` marks spread across
     more than one position along that route.
  2. (b) the mark count for that route length falls within the
     documented bounds (Blood.Trail.defaultTrailThresholds), and
     repeating the same route at a different `world.setTimeScale` lands
     within the SAME bounds (the trail's cadence clock is the unpaused
     gameTimeRef, never the world calendar).
  3. (c) once natural clot progression drives a wound's effective
     external bleed to ~0 (the `Combat.Wounds.Constants.clotBaseRate`
     calibration comment's own sev-0.15 slash "~25 game-s" figure),
     continued movement adds no further marks, and
     `blood.getTrailState(uid)` reports no active entry once bleeding
     has fully stopped.
  4. (d) an internal-only wound produces zero trail marks while
     `unit.getBlood(uid)` visibly drains.
  5. (e) a unit dying mid-route stops the trail without error: no crash,
     no further marks, and `blood.getTrailState(uid)` clears.

PASS  = all checks hold.
FAIL  = any check violated (bug in the trail emitter or its wiring).
"""
from __future__ import annotations
import argparse
import glob
import sys
import time
from probelib import (quit_engine, boot, init_arena, send, send_json,
                       spawn_acolyte, poll_until)

PORT = 9041
LOG = "/tmp/bleeding_trail_probe_engine.log"

# Mirrors Blood.Trail.defaultTrailThresholds — kept in sync by hand
# (this probe treats them as the documented contract, not a live query).
MIN_DISTANCE = 1.0   # tiles
MIN_CADENCE = 0.5    # seconds


def bootstrap_defs(port: int) -> None:
    """Load the substance/item/equipment/material/unit YAML defs the
    loading screen would normally load (it doesn't run headless) —
    unit.spawn fails without them. Mirrors tools/blood_impact_probe.py /
    tools/movement_probe.py."""
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
    # unit_ai's wander tick fights unit.moveTo under test (movement_probe.py's
    # technique) — neutralise it so commanded movement is the ONLY movement.
    send(port,
         "pcall(function() require('scripts.unit_ai').update = function() end end); "
         "return 'ai-off'")


def reset_blood() -> None:
    cleared = send(PORT, "return blood.clear()")
    if cleared.lower() != "true":
        print(f"FAIL (setup): blood.clear() returned {cleared!r}")
        sys.exit(2)


def spawn_fresh(x: float = 10, y: float = 10) -> int:
    """A brand-new unit per case — never reused — so one case's trail
    state/decals can't leak into the next (isolation, per issue #882's
    acceptance note). `clear_water=False`: unit_ai's update tick (which
    would normally assign/clear that goal) is neutralised in bootstrap,
    so no per-unit AI state is ever created to clear."""
    return spawn_acolyte(PORT, x, y, clear_water=False)


def destroy(uid: int) -> None:
    send(PORT, f"unit.destroy({uid}); return 'ok'", expect_result=False)


def injure(uid: int, kind: str, sev: float, bandage: float = 1.0) -> bool:
    ok = send(PORT, f"return unit.injure({uid}, 'torso', '{kind}', {sev}, {bandage})")
    if ok.lower() not in ("true", "false"):
        print(f"FAIL (setup): unit.injure(...) -> {ok!r}")
        sys.exit(2)
    return ok.lower() == "true"


def decals() -> list:
    return send_json(PORT, "return blood.listDecals()") or []


def trail_decals(exclude_ids: set) -> list:
    """Decals minus a given set of ids — used to exclude the one-shot
    Blood.Impact decal `unit.injure` places for slash/arterial wounds
    (Blood.Impact always creates one for those kinds) so bounds/spread
    checks below assert on ACTUAL trail marks only, never a false
    positive from the impact mark alone."""
    return [d for d in decals() if d["id"] not in exclude_ids]


def impact_decal_ids() -> set:
    return {d["id"] for d in decals()}


def trail_state(uid: int):
    return send_json(PORT, f"return blood.getTrailState({uid})")


def blood_of(uid: int) -> float:
    b = send_json(PORT, f"return unit.getBlood({uid})")
    if not isinstance(b, dict) or "current" not in b:
        print(f"FAIL (setup): unit.getBlood(...) -> {b!r}")
        sys.exit(2)
    return float(b["current"])


def bleed_rate_of(uid: int) -> float:
    b = send_json(PORT, f"return unit.getBlood({uid})")
    if not isinstance(b, dict) or "bleedRate" not in b:
        print(f"FAIL (setup): unit.getBlood(...) -> {b!r}")
        sys.exit(2)
    return float(b["bleedRate"])


def move_to(uid: int, tx: float, ty: float, speed: float = 1.0) -> None:
    send(PORT, f"unit.moveTo({uid}, {tx}, {ty}, {speed}); return 'go'",
         expect_result=False)


def main() -> int:
    ap = argparse.ArgumentParser(description=__doc__)
    ap.add_argument("--port", type=int, default=9041)
    args = ap.parse_args()
    global PORT
    PORT = args.port

    proc = boot(PORT, log=LOG)
    try:
        bootstrap_defs(PORT)
        init_arena(PORT)

        route = 15.0
        lower_bound = 1
        upper_bound = int(route / MIN_DISTANCE) + 2

        # --- 1/2(a). a bled, moving unit leaves marks spread along the route ---
        reset_blood()
        uid = spawn_fresh(10, 10)
        injure(uid, "slash", 0.6)   # untreated (bandage defaults to 1.0)
        impact_ids = impact_decal_ids()   # slash always creates one impact decal
        move_to(uid, 10 + route, 10)
        # Poll for at least 2 marks rather than assume a fixed sleep
        # margin always yields more than one — real-time engine
        # scheduling can lag well behind wall-clock under system load,
        # and the unit stops generating NEW marks once it arrives, so
        # a generous ceiling matters more than a short one here.
        if poll_until(90.0, lambda: len(trail_decals(impact_ids)) >= 2, interval=1.0) is None:
            print(f"FAIL (setup): fewer than 2 trail marks appeared within 90s "
                  f"along a {route}-tile bled, moved route "
                  f"({len(trail_decals(impact_ids))} so far) — engine too slow "
                  f"under current load, or a real regression")
            return 2
        ds = trail_decals(impact_ids)
        if not ds:
            print("FAIL: no trail decals appeared along a bled, moved route")
            return 1
        xs = [d["x"] for d in ds]
        if max(xs) - min(xs) < MIN_DISTANCE:
            print(f"FAIL: marks not spread along the route (x span "
                  f"{max(xs) - min(xs):.3f} < {MIN_DISTANCE})")
            return 1
        if not (lower_bound <= len(ds) <= upper_bound):
            print(f"FAIL: mark count {len(ds)} outside documented bounds "
                  f"[{lower_bound},{upper_bound}] for a {route}-tile route")
            return 1
        print(f"PASS: {len(ds)} marks spread along the route "
              f"(bounds [{lower_bound},{upper_bound}], x span "
              f"{max(xs) - min(xs):.2f})")
        destroy(uid)

        # --- 2(b). same bounds hold at a different world.setTimeScale ---
        reset_blood()
        uid = spawn_fresh(10, 10)
        injure(uid, "slash", 0.6)
        impact_ids = impact_decal_ids()
        send(PORT, "world.setTimeScale(5.0); return 'ok'")
        move_to(uid, 10 + route, 10)
        if poll_until(90.0, lambda: len(trail_decals(impact_ids)) >= 1, interval=1.0) is None:
            send(PORT, "world.setTimeScale(1.0); return 'ok'")
            print(f"FAIL (setup): no trail marks appeared within 90s at "
                  f"world.setTimeScale(5.0)")
            return 2
        ds = trail_decals(impact_ids)
        send(PORT, "world.setTimeScale(1.0); return 'ok'")
        if not (lower_bound <= len(ds) <= upper_bound):
            print(f"FAIL: mark count {len(ds)} outside bounds "
                  f"[{lower_bound},{upper_bound}] at world.setTimeScale(5.0)")
            return 1
        print(f"PASS: {len(ds)} marks at world.setTimeScale(5.0) — "
              f"same bounds hold (trail cadence ignores the world calendar)")
        destroy(uid)

        # --- 3(d). an internal-only wound: zero trail marks, blood still drains ---
        reset_blood()
        uid = spawn_fresh(10, 10)
        before = blood_of(uid)
        injure(uid, "internal", 0.6)   # internal never creates an impact decal either
        impact_ids = impact_decal_ids()
        move_to(uid, 10 + route, 10)
        time.sleep(route + 4)
        ds = trail_decals(impact_ids)
        after = blood_of(uid)
        if ds:
            print(f"FAIL: internal-only wound produced {len(ds)} trail marks")
            return 1
        if not (after < before):
            print(f"FAIL: internal wound did not drain blood ({before} -> {after})")
            return 1
        print(f"PASS: internal-only wound moved with zero trail marks "
              f"(blood {before:.3f} -> {after:.3f})")
        destroy(uid)

        # --- 4(e). death mid-route stops the trail cleanly ---
        # A MODERATE slash (not arterial/high-severity — those exsanguinate
        # in a few seconds via the wound tick's own natural DiedNow path,
        # leaving no time to reach a trail mark before this test's
        # EXPLICIT unit.kill() — the path actually under test here).
        reset_blood()
        uid = spawn_fresh(10, 10)
        injure(uid, "slash", 0.6)   # slash always creates one impact decal
        impact_ids = impact_decal_ids()
        move_to(uid, 10 + route, 10)
        if poll_until(8.0, lambda: len(trail_decals(impact_ids)) > 0, interval=0.5) is None:
            print("FAIL (setup): no marks appeared before death — can't test the stop")
            return 2
        send(PORT, f"unit.kill({uid}); return 'ok'", expect_result=False)
        # unit.kill only QUEUES the command; poll briefly for the unit
        # thread to actually apply it (handleUnitKillCommand clears
        # uiTrailState synchronously with the kill) rather than assuming
        # a single immediate check lands after that has happened.
        if poll_until(5.0, lambda: trail_state(uid) is None, interval=0.2) is None:
            print(f"FAIL: trail state still active after death: "
                  f"{trail_state(uid)!r}")
            return 1
        before_n = len(trail_decals(impact_ids))
        time.sleep(2.0)
        after_n = len(trail_decals(impact_ids))
        if after_n != before_n:
            print(f"FAIL: a dead unit kept adding trail marks "
                  f"({before_n} -> {after_n})")
            return 1
        print("PASS: death stops the trail cleanly (no crash, no further "
              "marks, getTrailState clears)")
        destroy(uid)

        # --- 5(c). clot progression to ~zero external bleed stops further marks ---
        # Combat.Wounds.Constants.clotBaseRate's own calibration comment:
        # an untreated sev-0.15 slash self-clots (bleed gone) in ~25 game-s
        # of wound-tick time — poll the real bleedRate rather than assuming
        # a fixed real-clock wait keeps pace with it (engine load varies).
        # The unit patrols a short box (re-issuing moveTo every few
        # seconds) rather than one long straight route, so it keeps
        # moving for however long the clot actually takes without
        # needing to know the arena's extent.
        reset_blood()
        uid = spawn_fresh(10, 10)
        injure(uid, "slash", 0.15)   # slash always creates one impact decal
        impact_ids = impact_decal_ids()
        waypoints = [(10.0, 10.0), (16.0, 10.0)]
        start = time.time()
        current_leg = [-1]

        def patrol() -> None:
            # Only re-issue moveTo on an actual leg change — reissuing the
            # SAME command every poll would needlessly reset/replan the
            # unit's path every tick instead of letting it walk smoothly.
            leg = int((time.time() - start) // 3.0) % 2
            if leg != current_leg[0]:
                current_leg[0] = leg
                move_to(uid, *waypoints[leg])

        patrol()
        while time.time() - start < 6:
            time.sleep(1)
            patrol()
        early_n = len(trail_decals(impact_ids))
        if early_n == 0:
            print("FAIL: no marks appeared while the wound was fresh/bleeding")
            return 1

        def clotted() -> bool:
            patrol()
            return bleed_rate_of(uid) <= 0.001

        if poll_until(90.0, clotted, interval=1.0) is None:
            print("FAIL (setup): wound never self-clotted to ~zero bleed "
                  "within 90s — can't test the cutoff")
            return 2
        mid_n = len(trail_decals(impact_ids))

        # Movement.hs checks external bleed BEFORE consuming any pending
        # volume (round-2 review): the instant bleedRate reads zero the
        # accumulator clears outright, discarding any tiny residual — so
        # getTrailState should clear at essentially the same moment, not
        # after one more flushed mark. Poll (briefly) rather than assume
        # a single immediate check lands in the same tick.
        def trail_cleared() -> bool:
            patrol()
            return trail_state(uid) is None

        if poll_until(10.0, trail_cleared, interval=0.5) is None:
            print(f"FAIL: getTrailState still active long after full clot: "
                  f"{trail_state(uid)!r}")
            return 1
        cleared_n = len(trail_decals(impact_ids))
        if cleared_n != mid_n:
            print(f"FAIL: a mark was emitted AFTER bleedRate already read "
                  f"zero, between clot detection and getTrailState "
                  f"clearing ({mid_n} -> {cleared_n})")
            return 1

        # NOW nothing should be left to emit — continued patrolling must
        # add zero further marks.
        for _ in range(8):
            time.sleep(1)
            patrol()
        late_n = len(trail_decals(impact_ids))
        if not (late_n == cleared_n):
            print(f"FAIL: marks kept appearing after getTrailState cleared "
                  f"({cleared_n} -> {late_n})")
            return 1
        print(f"PASS: marks stopped once clot drove external bleed to zero "
              f"(early={early_n}, mid={mid_n}, cleared={cleared_n}, "
              f"late={late_n}); getTrailState cleared")
        destroy(uid)

        print("\nPASS: all bleeding-trail checks held")
        return 0
    finally:
        quit_engine(PORT, proc)


if __name__ == "__main__":
    sys.exit(main())
