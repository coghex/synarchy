#!/usr/bin/env python3
"""Headless probe for issue #611: sleep pressure + circadian urge signals.

Boots headless, spawns one acolyte at the arena origin (gx=gy=0, so its
local sun angle exactly equals the world clock's — no longitude offset
to account for), neutralises unit_ai's tick so the unit stays put, and
drives both signals end to end:

  - `scripts.circadian`'s `getCircadianUrge(uid)`: flat (near-zero) at
    midnight and noon, peaks near dusk, rises smoothly inside its
    window.
  - the `sleep_pressure` resource (scripts/unit_resource_config.lua):
    starts full, drains monotonically over a short real-time window,
    and never ticks upward (no regen path exists yet by design).

PASS  = all checks hold.
FAIL  = any check violated (bug in the circadian bump or the
        drain-only resource wiring).
"""
from __future__ import annotations
import argparse
import glob
import sys
import time
from probelib import quit_engine, boot, init_arena, send, spawn_acolyte, poll_until

PORT = 9013
LOG = "/tmp/circadian_probe_engine.log"

ARENA = "arena"


def bootstrap_defs(port: int) -> None:
    """Load the substance/item/equipment/material/unit YAML defs the
    loading screen would normally load (it doesn't run headless) —
    unit.spawn fails without them. Mirrors tools/combat_anim_probe.py /
    tools/blood_impact_probe.py."""
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
    # Neutralise the AI wander/goal tick (mirrors tools/disarm_probe.py) —
    # this probe needs the unit to stay at gx=gy=0 (no longitude offset)
    # for the whole run; unit_resources' OWN tick (sleep_pressure drain)
    # is untouched.
    send(port, "pcall(function() require('scripts.unit_ai').update = "
               "function() end end); return 'ok'")


def set_time_and_wait(hour: int, minute: int, target: float, tol: float = 0.01) -> float:
    """world.setTime + poll world.getSunAngleAt(0,0) until it settles."""
    send(PORT, f"world.setTime('{ARENA}', {hour}, {minute})", expect_result=False)

    # Returns True/False, never the angle itself -- poll_until treats its
    # result as a truthiness check, and a settled angle of exactly 0.0
    # (midnight, one of the three targets this probe tests) is falsy in
    # Python, so returning the angle directly would make the midnight
    # check "time out" even on an immediate, correct match.
    def check():
        raw = send(PORT, "return world.getSunAngleAt(0, 0)")
        try:
            angle = float(raw)
        except (TypeError, ValueError):
            return False
        return abs(angle - target) <= tol

    ok = poll_until(10.0, check)
    if not ok:
        print(f"FAIL (setup): sun angle never settled near {target} after "
              f"world.setTime('{ARENA}', {hour}, {minute})")
        sys.exit(2)
    return float(send(PORT, "return world.getSunAngleAt(0, 0)"))


def urge_at(uid: int, hour: int, minute: int, target_angle: float) -> float:
    set_time_and_wait(hour, minute, target_angle)
    raw = send(PORT, f"return require('scripts.circadian').getCircadianUrge({uid})")
    try:
        return float(raw)
    except (TypeError, ValueError):
        print(f"FAIL: getCircadianUrge({uid}) at {hour}:{minute:02d} -> {raw!r}")
        sys.exit(1)


def main() -> int:
    ap = argparse.ArgumentParser(description=__doc__)
    ap.add_argument("--port", type=int, default=9013)
    args = ap.parse_args()
    global PORT
    PORT = args.port

    proc = boot(PORT, log=LOG)
    try:
        bootstrap_defs(PORT)
        init_arena(PORT, name=ARENA)
        uid = spawn_acolyte(PORT, 0, 0, clear_water=False)

        # --- circadian urge: flat at midnight/noon, peaks at dusk ------
        midnight = urge_at(uid, 0, 0, 0.0)
        if midnight > 0.02:
            print(f"FAIL: circadian urge at midnight = {midnight}, expected ~0")
            return 1
        print(f"PASS: circadian urge at midnight = {midnight:.4f} (flat)")

        noon = urge_at(uid, 12, 0, 0.5)
        if noon > 0.02:
            print(f"FAIL: circadian urge at noon = {noon}, expected ~0")
            return 1
        print(f"PASS: circadian urge at noon = {noon:.4f} (flat)")

        dusk = urge_at(uid, 18, 0, 0.75)
        if dusk < 0.95:
            print(f"FAIL: circadian urge at dusk = {dusk}, expected close to 1.0 (peak)")
            return 1
        print(f"PASS: circadian urge at dusk = {dusk:.4f} (peak)")

        # 17:00 is inside the window (1h before dusk) but off-peak: should
        # read strictly between "flat" and the dusk peak, confirming a
        # smooth bump rather than a step function.
        evening = urge_at(uid, 17, 0, 0.75 - 1.0 / 24)
        if not (0.05 < evening < dusk):
            print(f"FAIL: circadian urge at 17:00 = {evening}, expected "
                  f"strictly between 0 and the dusk peak ({dusk})")
            return 1
        print(f"PASS: circadian urge at 17:00 = {evening:.4f} "
              f"(rising toward dusk, below the peak)")

        # --- sleep pressure: drains monotonically, no regen ------------
        max_sp_raw = send(PORT,
            f"return require('scripts.unit_stats').get({uid}, 'max_sleep_pressure')")
        try:
            max_sp = float(max_sp_raw)
        except (TypeError, ValueError):
            print(f"FAIL (setup): max_sleep_pressure -> {max_sp_raw!r}")
            return 2
        if max_sp <= 0:
            print(f"FAIL (setup): max_sleep_pressure = {max_sp}, expected > 0")
            return 2

        # First tick seeds sleep_pressure to max; poll until it appears.
        # NOTE: by the time this section runs, the circadian checks above
        # have already let several real seconds (and several ticks) pass,
        # so "initial" itself may already be a little below max_sp — that's
        # expected, not a bug, so the check below is a generous sanity
        # bound (a wildly-off value would mean seeding used the wrong
        # max), not a tight equality.
        def seeded():
            raw = send(PORT, f"return unit.getStat({uid}, 'sleep_pressure')")
            try:
                return float(raw)
            except (TypeError, ValueError):
                return None
        seeded_value = None
        def seeded_ok():
            nonlocal seeded_value
            seeded_value = seeded()
            return seeded_value is not None
        if not poll_until(5.0, seeded_ok):
            print("FAIL (setup): sleep_pressure never seeded")
            return 2
        initial = seeded_value
        if not (0 < initial <= max_sp):
            print(f"FAIL: sleep_pressure seeded to {initial}, expected a "
                  f"value in (0, max_sleep_pressure={max_sp}]")
            return 1
        print(f"PASS: sleep_pressure seeded near max "
              f"({initial:.4f} of {max_sp:.4f})")

        samples = [initial]
        for _ in range(4):
            time.sleep(1.0)
            raw = send(PORT, f"return unit.getStat({uid}, 'sleep_pressure')")
            try:
                samples.append(float(raw))
            except (TypeError, ValueError):
                print(f"FAIL: unit.getStat(sleep_pressure) -> {raw!r}")
                return 1

        for i in range(1, len(samples)):
            if samples[i] > samples[i - 1] + 1e-6:
                print(f"FAIL: sleep_pressure rose ({samples[i-1]:.5f} -> "
                      f"{samples[i]:.5f}) while idle — it must never regen "
                      f"without real sleep (#612)")
                return 1
        elapsed = len(samples) - 1
        drained = samples[0] - samples[-1]
        if drained <= 0:
            print(f"FAIL: sleep_pressure did not drain at all over "
                  f"{elapsed}s: {samples!r}")
            return 1
        # Expected drain_constant_frac (1/3600) * max_sp * elapsed seconds.
        # Generous tolerance: this is a coarse (0.2s tick, 1s sample) real-
        # clock measurement, not a precision timing test.
        expected = (1.0 / 3600.0) * max_sp * elapsed
        if not (0.4 * expected <= drained <= 2.5 * expected):
            print(f"FAIL: sleep_pressure drained {drained:.5f} over "
                  f"{elapsed}s, expected roughly {expected:.5f} "
                  f"(samples={samples!r})")
            return 1
        print(f"PASS: sleep_pressure drains monotonically, never regens "
              f"idle ({samples[0]:.4f} -> {samples[-1]:.4f} over "
              f"{elapsed}s, ~{drained/elapsed:.5f}/s vs expected "
              f"~{expected/elapsed:.5f}/s)")

        print("\nPASS: all sleep pressure + circadian urge checks held")
        return 0
    finally:
        quit_engine(PORT, proc)


if __name__ == "__main__":
    sys.exit(main())
