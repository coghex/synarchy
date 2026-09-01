#!/usr/bin/env python3
"""Headless probe for issue #193: disabled-hand auto-drop must re-fire.

A weapon held in a severed/maimed grip slot should be auto-dropped on
the injury tick. The bug: a one-shot guard dropped it exactly once, so a
weapon RE-EQUIPPED into the same still-disabled hand afterward was never
dropped again. This probe verifies BOTH the first drop and the re-drop.

dropDisabledHandWeapons (scripts/unit_resources.lua) is pure injury-tick
logic — it has nothing to do with AI decision-making. The probe used to
leave unit_ai's tick live, and a freshly-spawned acolyte's default
find_water goal competed with the injury tick for real time; that
unrelated AI-timing noise was one source of flakiness. Neutralising
unit_ai.update (same pattern as collapse_crawl_probe.py /
concussion_revive_probe.py, which test the same tickInjuries machinery
and are CI-eligible) removes it, and running on the flat no-generator
arena (instead of a real generated world) drops the worldgen wait and
any chance of the unit standing near a cliff/fluid tile.

The bigger hidden hazard: severing 'r_hand' at severity 1.0 crosses
Combat.Wounds' `destroyThreshold` (1.0), which auto-cascades a SEVERED
wound onto every child part (palm + 5 fingers) via propagateSevering —
each hardcoded to `woundBandage = 1.0` (full bleed) regardless of what
bandage this probe passes on its own injure call. Six extra full-bleed
wounds can exsanguinate the unit within the first tick or two (worse
under a slow/loaded tick scheduler, since the first tick after a fresh
wound has zero clot built up yet), and once dead `tickInjuries` short-
circuits before ever reaching dropDisabledHandWeapons again — so the
re-drop (the actual #193 regression) silently stops being exercised.
`dropDisabledHandWeapons` treats ANY severity as disabling for kind
"severed" (no severity gate, unlike its fracture branch), so severity
just under 1.0 keeps the same "severed" semantics while staying under
destroyThreshold: no cascade, no extra unbandaged bleeders. Bandaging
the single wound (bandage=0.0, same convention as collapse_crawl_probe's
leg fractures) then zeroes even that one wound's bleed.

This probe implements the shared `probe-result/v1` contract: `--describe`
prints its ordered stable checks without booting an engine, and a harnessed
run writes structured events while a standalone run keeps human-readable
per-check output.

Usage: python3 tools/disarm_probe.py [--port 9193]
       python3 tools/disarm_probe.py --describe
Exit 0 = pass.
"""
from __future__ import annotations
import argparse, glob, sys, time
import probe_protocol
from probelib import quit_engine, boot, send

LOG = "/tmp/disarm_probe_engine.log"
LOG_NAME = "disarm_probe_engine.log"
PROBE_KEY = "disarm"

CHECKS = [
    ("initial_drop", "a dagger is dropped from the newly severed hand"),
    ("repeat_drop",
     "a re-equipped dagger is dropped again from the still-severed hand"),
]

DESCRIPTOR = probe_protocol.build_descriptor(PROBE_KEY, CHECKS)


def bootstrap(port: int) -> None:
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
    for script, dt in [("unit_stats", 0.1), ("unit_resources", 0.2), ("unit_ai", 0.1)]:
        send(port, f"engine.loadScript('scripts/{script}.lua', {dt}); return 'ok'")
    # Neutralise the AI wander/goal tick — dropDisabledHandWeapons lives
    # entirely in unit_resources' injury tick and doesn't need unit_ai at
    # all, so a live AI update is only a source of unrelated timing noise.
    send(port, "pcall(function() require('scripts.unit_ai').update = "
               "function() end end); return 'ok'")
    send(port, "require('scripts.movement_arena').buildCourse('flat')")


def held_right(port: int, uid: int) -> str:
    r = send(port, f"local lo=equipment.getLoadout({uid}); local h=lo and lo['right_hand']; "
             f"return h and (h.defName or 'yes') or 'EMPTY'")
    return r.strip('"')


def main() -> int:
    ap = argparse.ArgumentParser(description=__doc__,
                                  formatter_class=argparse.RawDescriptionHelpFormatter)
    ap.add_argument("--port", type=int, default=9193)
    ap.add_argument("--describe", action="store_true",
                    help="print the probe-result/v1 check declaration and "
                         "exit without booting an engine")
    args = ap.parse_args()
    if args.describe:
        print(DESCRIPTOR.to_json())
        return 0
    rep = probe_protocol.reporter_from_env(DESCRIPTOR)
    try:
        return _run(args.port, rep)
    finally:
        rep.close()


def _run(port, rep) -> int:
    proc = boot(port, log=rep.engine_log_path(LOG_NAME, LOG),
                args=rep.engine_args())
    try:
        bootstrap(port)
        uid = int(float(send(port, "return unit.spawn('acolyte', 0, 0)")))
        rep.note(f"spawned acolyte #{uid}")
        time.sleep(1.0)

        def arm():
            send(port, f"unit.addItem({uid}, 'steel_dagger'); return 'ok'")
            return send(port, f"return equipment.equip({uid}, 'right_hand', 'steel_dagger')").strip()

        def ground_daggers():
            r = send(port, "local n=0; for _,it in ipairs(item.listGround() or {}) do "
                     "if (it.defName or '')=='steel_dagger' then n=n+1 end end; return n")
            try:
                return int(float(r))
            except ValueError:
                return -1

        def wait_empty(attempts=40, interval=0.3):
            """Poll until right_hand is empty (the weapon got dropped).

            Iteration-count bounded (not a wall-clock deadline) so a
            slower/busier CI runner naturally gets more real time to let
            the injury tick fire, rather than a fixed deadline eating
            into round-trip latency budget.
            """
            for _ in range(attempts):
                if held_right(port, uid) == "EMPTY":
                    return True
                time.sleep(interval)
            return False

        base_ground = ground_daggers()
        rep.note(f"ground daggers (baseline): {base_ground}")

        # Phase 1: equip a dagger, sever the hand, expect the first drop.
        # severity 0.99 (not 1.0) stays under Combat.Wounds' destroyThreshold
        # so it doesn't cascade-sever the palm/fingers (each of which would
        # bleed at a hardcoded full rate regardless of our bandage arg,
        # risking exsanguination before phase 2 runs); bandage=0.0 zeroes
        # this single wound's own bleed. dropDisabledHandWeapons treats any
        # severity as disabling for kind "severed", so this is still the
        # same disabling condition the mechanism looks for.
        eq1 = arm()
        held_after_first_equip = held_right(port, uid)
        rep.note(f"phase1: equip()={eq1}, right_hand = {held_after_first_equip}")
        send(port, f"return unit.injure({uid}, 'r_hand', 'severed', 0.99, 0.0)")
        rep.note("phase1: severed r_hand")
        first_empty = wait_empty()
        g1 = ground_daggers()
        rep.note(f"  first drop: right_hand empty={first_empty}, ground daggers={g1}")
        p1 = first_empty and g1 == base_ground + 1
        first_detail = {
            "equip_result": eq1,
            "held_after_equip": held_after_first_equip,
            "right_hand_empty": first_empty,
            "baseline_ground_daggers": base_ground,
            "ground_daggers": g1,
        }

        # Phase 2: re-equip into the SAME still-severed hand. equip() must
        # succeed (the engine doesn't block equipping a maimed hand), and
        # the next injury tick must drop it AGAIN — the #193 fix. With the
        # old one-shot guard, equip()==true but the dagger stays held and
        # the ground count never increments a second time.
        eq2 = arm()
        rep.note(f"phase2: equip()={eq2}")
        if eq2 != "true":
            inconclusive = ("INCONCLUSIVE: re-equip into severed hand was "
                            f"rejected (equip returned {eq2}); can't exercise "
                            "the re-drop path")
            # Preserve the historical exit 3 while retaining the first
            # completed observation. The unexercised second check remains
            # MISSING in protocol mode rather than masquerading as a failure.
            rep.check(
                "initial_drop", p1,
                ("first drop left the hand empty and added one ground dagger"
                 if p1 else
                 "first drop did not leave the expected hand/ground state"),
                first_detail)
            if rep.protocol_mode:
                rep.skip(
                    "repeat drop was not exercised because re-equipping the "
                    "severed hand was rejected",
                    {"equip_result": eq2})
            else:
                print(inconclusive, file=sys.stderr)
            return 3
        second_empty = wait_empty()
        g2 = ground_daggers()
        rep.note(f"  re-drop: right_hand empty={second_empty}, ground daggers={g2}")
        p2 = second_empty and g2 == base_ground + 2

        rep.note("\n--- result ---")
        rep.check(
            "initial_drop", p1,
            ("first drop left the hand empty and added one ground dagger"
             if p1 else
             "first drop did not leave the expected hand/ground state"),
            first_detail)
        rep.check(
            "repeat_drop", p2,
            ("re-drop left the hand empty and added a second ground dagger "
             "(issue #193)" if p2 else
             "re-drop did not leave the expected hand/ground state "
             "(issue #193)"),
            {"equip_result": eq2,
             "right_hand_empty": second_empty,
             "baseline_ground_daggers": base_ground,
             "ground_daggers": g2})
        return 0 if (p1 and p2) else 1
    finally:
        quit_engine(port, proc)


if __name__ == "__main__":
    sys.exit(main())
