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

This probe implements the shared `probe-result/v1` contract: `--describe`
prints its ordered stable checks without booting an engine, and a harnessed
run writes structured events while a standalone run keeps human-readable
per-check output.
"""
from __future__ import annotations
import argparse
import glob
import sys
import time

import probe_protocol
from probelib import quit_engine, boot, init_arena, send, spawn_acolyte, poll_until

PORT = 9013
LOG = "/tmp/circadian_probe_engine.log"
LOG_NAME = "circadian_probe_engine.log"
PROBE_KEY = "circadian"

ARENA = "arena"

CHECKS = [
    ("urge_midnight_flat", "circadian urge is flat at midnight"),
    ("urge_noon_flat", "circadian urge is flat at noon"),
    ("urge_dusk_peak", "circadian urge peaks at dusk"),
    ("urge_evening_rising", "circadian urge rises smoothly toward dusk"),
    ("sleep_pressure_seeded", "sleep pressure seeds within its valid range"),
    ("sleep_pressure_monotonic", "idle sleep pressure never regenerates"),
    ("sleep_pressure_drain_rate", "idle sleep pressure drains at the expected rate"),
]

DESCRIPTOR = probe_protocol.build_descriptor(PROBE_KEY, CHECKS)


class ProbeSetupError(RuntimeError):
    """A fixture failure that preserves this probe's standalone exit 2."""


class ProbeObservationError(RuntimeError):
    """A malformed value belonging to the currently active check."""


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
        raise ProbeSetupError(
            f"sun angle never settled near {target} after "
            f"world.setTime('{ARENA}', {hour}, {minute})")
    return float(send(PORT, "return world.getSunAngleAt(0, 0)"))


def urge_at(uid: int, hour: int, minute: int, target_angle: float) -> float:
    set_time_and_wait(hour, minute, target_angle)
    raw = send(PORT, f"return require('scripts.circadian').getCircadianUrge({uid})")
    try:
        return float(raw)
    except (TypeError, ValueError):
        raise ProbeObservationError(
            f"getCircadianUrge({uid}) at {hour}:{minute:02d} -> {raw!r}") from None


def main() -> int:
    ap = argparse.ArgumentParser(description=__doc__)
    ap.add_argument("--port", type=int, default=9013)
    ap.add_argument("--describe", action="store_true",
                    help="print the probe-result/v1 check declaration and "
                         "exit without booting an engine")
    args = ap.parse_args()
    if args.describe:
        print(DESCRIPTOR.to_json())
        return 0
    global PORT
    PORT = args.port
    rep = probe_protocol.reporter_from_env(DESCRIPTOR)
    try:
        return _run(PORT, rep)
    finally:
        rep.close()


def _run(port: int, rep: probe_protocol.Reporter) -> int:
    global PORT
    PORT = port

    proc = boot(PORT, log=rep.engine_log_path(LOG_NAME, LOG),
                args=rep.engine_args())
    try:
        bootstrap_defs(PORT)
        init_arena(PORT, name=ARENA)
        uid = spawn_acolyte(PORT, 0, 0, clear_water=False)

        # --- circadian urge: flat at midnight/noon, peaks at dusk ------
        try:
            midnight = urge_at(uid, 0, 0, 0.0)
        except ProbeSetupError as error:
            rep.abort(str(error), {"hour": 0, "minute": 0, "target_angle": 0.0})
            return 2
        except ProbeObservationError as error:
            rep.check("urge_midnight_flat", False, str(error), {"error": str(error)})
            return 1
        midnight_ok = midnight <= 0.02
        rep.check(
            "urge_midnight_flat", midnight_ok,
            (f"circadian urge at midnight = {midnight:.4f} (flat)" if midnight_ok
             else f"circadian urge at midnight = {midnight}, expected ~0"),
            {"urge": midnight, "maximum": 0.02})
        if not midnight_ok:
            return 1

        try:
            noon = urge_at(uid, 12, 0, 0.5)
        except ProbeSetupError as error:
            rep.abort(str(error), {"hour": 12, "minute": 0, "target_angle": 0.5})
            return 2
        except ProbeObservationError as error:
            rep.check("urge_noon_flat", False, str(error), {"error": str(error)})
            return 1
        noon_ok = noon <= 0.02
        rep.check(
            "urge_noon_flat", noon_ok,
            (f"circadian urge at noon = {noon:.4f} (flat)" if noon_ok
             else f"circadian urge at noon = {noon}, expected ~0"),
            {"urge": noon, "maximum": 0.02})
        if not noon_ok:
            return 1

        try:
            dusk = urge_at(uid, 18, 0, 0.75)
        except ProbeSetupError as error:
            rep.abort(str(error), {"hour": 18, "minute": 0, "target_angle": 0.75})
            return 2
        except ProbeObservationError as error:
            rep.check("urge_dusk_peak", False, str(error), {"error": str(error)})
            return 1
        dusk_ok = dusk >= 0.95
        rep.check(
            "urge_dusk_peak", dusk_ok,
            (f"circadian urge at dusk = {dusk:.4f} (peak)" if dusk_ok else
             f"circadian urge at dusk = {dusk}, expected close to 1.0 (peak)"),
            {"urge": dusk, "minimum": 0.95})
        if not dusk_ok:
            return 1

        # 17:00 is inside the window (1h before dusk) but off-peak: should
        # read strictly between "flat" and the dusk peak, confirming a
        # smooth bump rather than a step function.
        try:
            evening = urge_at(uid, 17, 0, 0.75 - 1.0 / 24)
        except ProbeSetupError as error:
            rep.abort(str(error),
                      {"hour": 17, "minute": 0,
                       "target_angle": 0.75 - 1.0 / 24})
            return 2
        except ProbeObservationError as error:
            rep.check("urge_evening_rising", False, str(error), {"error": str(error)})
            return 1
        evening_ok = 0.05 < evening < dusk
        rep.check(
            "urge_evening_rising", evening_ok,
            (f"circadian urge at 17:00 = {evening:.4f} "
             f"(rising toward dusk, below the peak)" if evening_ok else
             f"circadian urge at 17:00 = {evening}, expected strictly "
             f"between 0 and the dusk peak ({dusk})"),
            {"urge": evening, "dusk_urge": dusk, "minimum": 0.05})
        if not evening_ok:
            return 1

        # --- sleep pressure: drains monotonically, no regen ------------
        max_sp_raw = send(PORT,
            f"return require('scripts.unit_stats').get({uid}, 'max_sleep_pressure')")
        try:
            max_sp = float(max_sp_raw)
        except (TypeError, ValueError):
            rep.abort("max_sleep_pressure was not numeric",
                      {"observed": max_sp_raw})
            return 2
        if max_sp <= 0:
            rep.abort(f"max_sleep_pressure = {max_sp}, expected > 0",
                      {"max_sleep_pressure": max_sp})
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
            rep.abort("sleep_pressure never seeded")
            return 2
        initial = seeded_value
        seeded_range_ok = 0 < initial <= max_sp
        rep.check(
            "sleep_pressure_seeded", seeded_range_ok,
            (f"sleep_pressure seeded near max ({initial:.4f} of {max_sp:.4f})"
             if seeded_range_ok else
             f"sleep_pressure seeded to {initial}, expected a value in "
             f"(0, max_sleep_pressure={max_sp}]"),
            {"initial": initial, "max_sleep_pressure": max_sp})
        if not seeded_range_ok:
            return 1

        samples = [initial]
        for _ in range(4):
            time.sleep(1.0)
            raw = send(PORT, f"return unit.getStat({uid}, 'sleep_pressure')")
            try:
                samples.append(float(raw))
            except (TypeError, ValueError):
                rep.check(
                    "sleep_pressure_monotonic", False,
                    f"unit.getStat(sleep_pressure) -> {raw!r}",
                    {"observed": raw, "samples": samples})
                return 1

        rise = next(((samples[i - 1], samples[i])
                     for i in range(1, len(samples))
                     if samples[i] > samples[i - 1] + 1e-6), None)
        monotonic_ok = rise is None
        rep.check(
            "sleep_pressure_monotonic", monotonic_ok,
            ("sleep_pressure never regenerated while idle" if monotonic_ok else
             f"sleep_pressure rose ({rise[0]:.5f} -> {rise[1]:.5f}) while "
             "idle — it must never regen without real sleep (#612)"),
            {"samples": samples})
        if not monotonic_ok:
            return 1

        elapsed = len(samples) - 1
        drained = samples[0] - samples[-1]
        # Expected drain_constant_frac (1/3600) * max_sp * elapsed seconds.
        # Generous tolerance: this is a coarse (0.2s tick, 1s sample) real-
        # clock measurement, not a precision timing test.
        expected = (1.0 / 3600.0) * max_sp * elapsed
        drain_ok = drained > 0 and 0.4 * expected <= drained <= 2.5 * expected
        if drained <= 0:
            human = (f"sleep_pressure did not drain at all over {elapsed}s: "
                     f"{samples!r}")
        elif not drain_ok:
            human = (f"sleep_pressure drained {drained:.5f} over {elapsed}s, "
                     f"expected roughly {expected:.5f} (samples={samples!r})")
        else:
            human = (f"sleep_pressure drains monotonically, never regens idle "
                     f"({samples[0]:.4f} -> {samples[-1]:.4f} over {elapsed}s, "
                     f"~{drained/elapsed:.5f}/s vs expected "
                     f"~{expected/elapsed:.5f}/s)")
        rep.check(
            "sleep_pressure_drain_rate", drain_ok, human,
            {"samples": samples, "drained": drained, "elapsed_seconds": elapsed,
             "expected_drain": expected})
        if not drain_ok:
            return 1

        rep.note("\nPASS: all sleep pressure + circadian urge checks held")
        return 0
    finally:
        quit_engine(PORT, proc)


if __name__ == "__main__":
    sys.exit(main())
