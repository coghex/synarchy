#!/usr/bin/env python3
"""Headless probe for issue #613: species-specific circadian curves.

#611 built the circadian urge bump (scripts/circadian.lua) and #612 the
"go to sleep" AI goal (scripts/unit_ai_sleep.lua) generically over any
defName — both already read their phase shape from the per-def
sleep_pressure block in scripts/unit_resource_config.lua. #613's job was
to actually USE that per-species knob: bear_brown now centers its
circadian urge on DAWN (0.25) instead of the acolyte/red_squirrel
default DUSK (0.75), and scripts/bear_ai.lua now wires the real
go_to_sleep goal (not just its own cosmetic bear_rest nap cycle) onto
bear_brown, reusing its existing sit/lie/sleep art for the engine's real
Standing->Crouching->Crawling->Sleeping pose chain (data/units/
bear_brown.yaml's new state_animations aliases).

#1945 then closed the other half of the same asymmetry: the automatic
time-of-day WAKE boundary was a single module constant (dawn, 0.25)
applied to every species, so the dawn-centered bear was driven to bed in
exactly the window it was then woken in. `scripts/unit_ai_sleep.lua` now
derives the boundary per species as half a day past the def's own
`circadian_center` — 0.25 for the dusk-centered acolyte (and for any def
that configures no center at all, via circadian.lua's DEFAULT_CENTER),
0.75 for `bear_brown`. Layers D-F below are that regression.

Six layers, cheapest/most isolated first:

  A. Raw urge (scripts.circadian.getCircadianUrge): at a fixed time,
     acolyte and bear read OPPOSITE phases — acolyte peaks at dusk /
     flat at dawn, bear peaks at dawn / flat at dusk.
  B. sleepUtility (scripts.unit_ai_sleep.sleepUtility), called directly
     like sleep_probe.py's exhaustion check: with sleep_pressure deficit
     and exhaustion held identical and fixed for both units, only the
     urge term can move the score — and it moves in opposite directions
     for the two species across dusk/dawn.
  C. The derived wake boundary itself (#1945), read through the module's
     own lookup: acolyte 0.25 (unchanged), bear_brown 0.75, and a def
     carrying no circadian_center at all still 0.25.
  D. End to end: a fresh bear_brown, sleep-deprived and released into a
     dawn clock, is actually PICKED by the AI (go_to_sleep) and walks
     the real pose chain down to Sleeping — proving the reused-art
     state_animations aliases actually resolve — then wakes via the
     public wake API back to standing.
  E. #1945 regression: that same bear, asleep with a sampled pre-0.25
     baseline and sleep_pressure held well below WAKE_PRESSURE_FRAC, is
     carried ACROSS sun angle 0.25 and must stay asleep — neither the
     waking phase nor the reverse pose chain may begin. This is the case
     that fails against the old fixed-dawn module, and it establishes its
     own below-boundary baseline so it does not depend on #1939.
  F. …and is then carried across 0.75, its own new boundary, where it
     must wake on its own — no wakeUnit call, pressure still below
     WAKE_PRESSURE_FRAC — and climb back toward standing.

PASS = every check holds. FAIL = a concrete mismatch.

This probe implements the shared `probe-result/v1` contract: `--describe`
prints its ordered stable checks without booting an engine, and a harnessed
run writes structured events while a standalone run keeps its human-readable
per-check output.
"""
from __future__ import annotations
import argparse
import glob
import sys
import time
import probe_protocol
from probelib import boot, quit_engine, send, init_arena, spawn_acolyte, poll_until, load_ai_stack

PORT = 9016
LOG = "/tmp/circadian_species_probe_engine.log"
LOG_NAME = "circadian_species_probe_engine.log"
PROBE_KEY = "circadian_species"
CHECKS = [
    ("species_urge_phases", "raw circadian urge is phase-shifted per species"),
    ("utility_crossover", "go_to_sleep utility crosses over between species"),
    ("wake_boundaries", "the automatic wake boundary is derived per species"),
    ("bear_selects_sleep", "bear AI selects go_to_sleep at its dawn circadian peak"),
    ("bear_reaches_sleeping", "bear reaches the real Sleeping pose through the reused art chain"),
    ("public_wake_standing", "the public wake API returns the bear to standing"),
    ("sleeps_through_dawn", "the dawn-centered bear sleeps through the old dawn wake boundary"),
    ("wakes_at_own_boundary", "the bear wakes automatically at its own dusk boundary"),
]
DESCRIPTOR = probe_protocol.build_descriptor(PROBE_KEY, CHECKS)
ARENA = "arena"


class ProbeSetupError(RuntimeError):
    pass

# Mirrors scripts/unit_ai_sleep.lua's own WAKE_PRESSURE_FRAC. Layers E/F
# must keep the sleeper strictly below it, or a pressure wake would be
# mistaken for a time-of-day wake (and would mask its absence).
WAKE_PRESSURE_FRAC = 0.98

# scripts/unit_resource_config.lua configures a circadian_center for
# every def that has a sleep_pressure block, so requirement 4's
# "unconfigured def" case needs a def with no block at all.
# white_tailed_deer is a real registered unit with none, so shapeFor
# falls back to circadian.lua's DEFAULT_CENTER = 0.75 for it.
UNCONFIGURED_DEF = "white_tailed_deer"


def bootstrap_defs(port: int) -> None:
    """Mirrors tools/circadian_probe.py / tools/sleep_probe.py."""
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
    load_ai_stack(port)


def set_time_and_wait(hour: int, minute: int, target: float, tol: float = 0.01) -> None:
    """world.setTime + poll world.getSunAngleAt(0,0) until it settles."""
    send(PORT, f"world.setTime('{ARENA}', {hour}, {minute})", expect_result=False)

    def check():
        raw = send(PORT, "return world.getSunAngleAt(0, 0)")
        try:
            return abs(float(raw) - target) <= tol
        except (TypeError, ValueError):
            return False

    if not poll_until(10.0, check):
        raise ProbeSetupError(
            f"sun angle never settled near {target} after "
            f"world.setTime('{ARENA}', {hour}, {minute})")


def urge(uid: int) -> float:
    raw = send(PORT, f"return require('scripts.circadian').getCircadianUrge({uid})")
    try:
        return float(raw)
    except (TypeError, ValueError):
        raise ProbeSetupError(f"getCircadianUrge({uid}) -> {raw!r}")


def sleep_utility(uid: int, cfg_key: str) -> float:
    raw = send(PORT,
        f"local sleepGoal = require('scripts.unit_ai_sleep'); "
        f"local ai = require('scripts.unit_ai'); "
        f"local cfg = require('scripts.unit_ai_tunables'); "
        f"return sleepGoal.sleepUtility({uid}, ai.getState({uid}), cfg.{cfg_key})")
    try:
        return float(raw)
    except (TypeError, ValueError):
        raise ProbeSetupError(f"sleepUtility({uid}, {cfg_key}) -> {raw!r}")


def max_stat(uid: int, name: str) -> float:
    raw = send(PORT, f"return require('scripts.unit_stats').get({uid}, '{name}')")
    try:
        v = float(raw)
    except (TypeError, ValueError):
        raise ProbeSetupError(f"{name} -> {raw!r}")
    if v <= 0:
        raise ProbeSetupError(f"{name} = {v}, expected > 0")
    return v


def get_pose(uid: int) -> str:
    return send(PORT, f"return unit.getPose({uid})")


def wait_for_pose(uid: int, target: str, timeout: float = 10.0) -> bool:
    return poll_until(timeout, lambda: get_pose(uid) == target) is not None


def get_ai_field(uid: int, field: str):
    return send(PORT,
        f"local ai = require('scripts.unit_ai'); "
        f"local s = ai.getState({uid}); "
        f"if not s then return nil end; return s.{field}")


def ai_field_number(uid: int, field: str):
    """``get_ai_field`` as a float, or None when the field is unset.

    An absent Lua field arrives over the debug console as text, not as
    Python None (a table return serializes to JSON, so it reads as
    "null"; a bare nil as "nil"), so the float conversion is what
    actually tests existence here.
    """
    try:
        return float(get_ai_field(uid, field))
    except (TypeError, ValueError):
        return None


def ai_field_is_unset(uid: int, field: str) -> bool:
    return get_ai_field(uid, field) in (None, "", "nil", "null")


def wake_angle_for(def_name: str) -> float:
    """scripts.unit_ai_sleep's own derived wake boundary for a def name
    (#1945) — the same lookup a sleeping unit of that def runs."""
    raw = send(PORT,
        f"return require('scripts.unit_ai_sleep').wakeAngleFor('{def_name}')")
    try:
        return float(raw)
    except (TypeError, ValueError):
        raise ProbeSetupError(f"wakeAngleFor({def_name!r}) -> {raw!r}")


def hold_pressure(uid: int, max_sp: float, frac: float = 0.4) -> None:
    """Pin sleep_pressure well below WAKE_PRESSURE_FRAC (0.98).

    regen_factor_sleeping refills the pool from empty in ~480 real
    seconds, so a long sleeping phase would eventually trip the
    pressure wake condition and make a boundary assertion meaningless.
    Re-holding before each timed step keeps the ONLY live wake condition
    the time-of-day one.
    """
    send(PORT, f"unit.setStat({uid}, 'sleep_pressure', {frac * max_sp})",
         expect_result=False)


def pressure_frac(uid: int, max_sp: float) -> float:
    raw = send(PORT, f"return unit.getStat({uid}, 'sleep_pressure')")
    try:
        return float(raw) / max_sp
    except (TypeError, ValueError):
        raise ProbeSetupError(f"sleep_pressure({uid}) -> {raw!r}")


def wait_asleep(uid: int, timeout: float = 60.0) -> bool:
    """Block until the unit is in the held Sleeping phase AND pose."""
    return poll_until(timeout, lambda: get_ai_field(uid, "sleepPhase") == "sleeping"
                      and get_pose(uid) == "sleeping") is not None


def wait_for_ai_state(uid: int, timeout: float = 10.0) -> bool:
    """Block until aiState[uid] exists (seeded by the unit's first real
    tick). Needed before neutralising unit_ai.update, and before any
    direct sleepUtility(uid, ai.getState(uid), ...) call — indexing a
    nil state errors. Mirrors probelib.clear_find_water's server-side
    truthiness check (a Lua nil renders as the STRING "nil" over the
    debug console, not Python None, so the existence check must run
    IN Lua and return a real true/false)."""
    return poll_until(timeout, lambda: send(
        PORT, f"return require('scripts.unit_ai').getState({uid}) ~= nil")
        == "true") is not None


def main() -> int:
    ap = argparse.ArgumentParser(description=__doc__)
    ap.add_argument("--port", type=int, default=9016)
    ap.add_argument("--describe", action="store_true")
    args = ap.parse_args()
    if args.describe:
        print(DESCRIPTOR.to_json())
        return 0
    rep = probe_protocol.reporter_from_env(DESCRIPTOR)
    try:
        return _run(args.port, rep)
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

        # ---- A + B setup: two units pinned at (0,0) (no longitude
        # offset), AI tick neutralised so neither wanders off-tile
        # while we probe fixed times. ----------------------------------
        aid = spawn_acolyte(PORT, 0, 0)
        bid = spawn_acolyte(PORT, 0, 0, unit="bear_brown", clear_water=False)
        if not wait_for_ai_state(bid):
            rep.abort("bear_brown never got AI state")
            return 2
        # Save the real update closure (mirrors tools/sleep_probe.py) so
        # phase C below can restore genuine AI-driven decisions after
        # phases A/B neutralise the tick to keep both units pinned at
        # (0,0) — no longitude offset — across the fixed-time reads.
        send(PORT, "_G.__realAiUpdate = require('scripts.unit_ai').update; "
                   "return 'ok'")
        send(PORT, "require('scripts.unit_ai').update = function() end; return 'ok'")

        DUSK, DAWN = 0.75, 0.25

        # ---- A. Raw urge: opposite phases ------------------------------
        set_time_and_wait(18, 0, DUSK)
        a_urge_dusk, b_urge_dusk = urge(aid), urge(bid)
        set_time_and_wait(6, 0, DAWN)
        a_urge_dawn, b_urge_dawn = urge(aid), urge(bid)

        urge_ok = (a_urge_dusk >= 0.95 and b_urge_dusk <= 0.05
                   and a_urge_dawn <= 0.05 and b_urge_dawn >= 0.95)
        if not rep.check("species_urge_phases", urge_ok,
                         "raw circadian urge is phase-shifted per species",
                         {"acolyte_dusk": a_urge_dusk, "bear_dusk": b_urge_dusk,
                          "acolyte_dawn": a_urge_dawn, "bear_dawn": b_urge_dawn}):
            return 1

        # ---- B. sleepUtility: hold deficit + exhaustion fixed and
        # identical for both units so only the urge term can move the
        # score (mirrors sleep_probe.py's isolate-one-variable pattern).
        # ------------------------------------------------------------
        a_max_sp, b_max_sp = max_stat(aid, "max_sleep_pressure"), max_stat(bid, "max_sleep_pressure")
        a_max_exh, b_max_exh = max_stat(aid, "max_exhaustion"), max_stat(bid, "max_exhaustion")
        for uid, max_sp, max_exh in ((aid, a_max_sp, a_max_exh), (bid, b_max_sp, b_max_exh)):
            send(PORT, f"unit.setStat({uid}, 'sleep_pressure', {0.5 * max_sp})",
                 expect_result=False)  # deficit = 0.5, above both species' sleep_min_deficit (0.35)
            send(PORT, f"unit.setStat({uid}, 'exhaustion', {max_exh})",
                 expect_result=False)  # fully rested -> exhaustionDeficit term = 0

        set_time_and_wait(18, 0, DUSK)
        a_util_dusk = sleep_utility(aid, "acolyte")
        b_util_dusk = sleep_utility(bid, "bear_brown")
        set_time_and_wait(6, 0, DAWN)
        a_util_dawn = sleep_utility(aid, "acolyte")
        b_util_dawn = sleep_utility(bid, "bear_brown")

        utility_ok = (a_util_dusk > a_util_dawn + 1.0
                      and b_util_dawn > b_util_dusk + 1.0
                      and a_util_dusk > b_util_dusk
                      and b_util_dawn > a_util_dawn)
        if not rep.check("utility_crossover", utility_ok,
                         "go_to_sleep utility crosses over between species",
                         {"acolyte_dusk": a_util_dusk, "bear_dusk": b_util_dusk,
                          "acolyte_dawn": a_util_dawn, "bear_dawn": b_util_dawn}):
            return 1

        # ---- C. #1945: the automatic wake boundary is derived from the
        # def's own circadian phase, through scripts/circadian.lua's
        # shapeFor (the single source of truth, DEFAULT_CENTER included)
        # rather than one module constant shared by every species. Read
        # here through the module's own lookup so the three cases —
        # unchanged acolyte, moved bear, unconfigured def — are checked
        # against the same code the sleeping unit runs. ------------------
        a_wake = wake_angle_for("acolyte")
        b_wake = wake_angle_for("bear_brown")
        d_wake = wake_angle_for(UNCONFIGURED_DEF)
        wake_ok = (abs(a_wake - DAWN) <= 1e-9
                   and abs(b_wake - DUSK) <= 1e-9
                   and abs(d_wake - DAWN) <= 1e-9)
        if not rep.check("wake_boundaries", wake_ok,
                         "the automatic wake boundary is per-species",
                         {"acolyte": a_wake, "bear": b_wake,
                          "unconfigured": d_wake}):
            return 1

        # ---- D. End to end: a fresh bear actually seeks + reaches real
        # sleep at its circadian peak, exercising the new bear_brown.yaml
        # pose-chain aliases, then wakes via the public API. -------------
        send(PORT, "require('scripts.unit_ai').update = _G.__realAiUpdate; "
                   "return 'ok'")

        bid2 = spawn_acolyte(PORT, 20, 20, unit="bear_brown", clear_water=False)
        max_sp2 = max_stat(bid2, "max_sleep_pressure")
        send(PORT, f"unit.setStat({bid2}, 'sleep_pressure', {0.4 * max_sp2})",
             expect_result=False)  # deficit = 0.6, comfortably above the 0.35 floor
        set_time_and_wait(6, 0, DAWN)  # bear's circadian peak

        selected = poll_until(15.0, lambda: get_ai_field(bid2, "currentAction") == "go_to_sleep")
        if not rep.check("bear_selects_sleep", bool(selected),
                         "bear_brown's AI selected go_to_sleep at its dawn peak",
                         {"current_action": get_ai_field(bid2, "currentAction")}):
            return 1

        if not poll_until(30.0, lambda: get_ai_field(bid2, "sleepPhase")
                          in ("lying_down", "sleeping")):
            rep.check("bear_reaches_sleeping", False,
                      "bear sleep phase never reached lying_down",
                      {"sleep_phase": get_ai_field(bid2, "sleepPhase"),
                       "pose": get_pose(bid2)})
            return 1
        sleeping = wait_for_pose(bid2, "sleeping", timeout=10.0)
        if not rep.check("bear_reaches_sleeping", sleeping,
                         "bear_brown reached the real Sleeping pose via the reused art",
                         {"pose": get_pose(bid2)}):
            return 1

        send(PORT, f"require('scripts.unit_ai').wakeUnit({bid2})", expect_result=False)
        standing = wait_for_pose(bid2, "standing", timeout=15.0)
        if not rep.check("public_wake_standing", standing,
                         "bear_brown woke via the public API back to standing",
                         {"pose": get_pose(bid2)}):
            return 1

        # ---- E. #1945 regression: carry the sleeping bear ACROSS 0.25
        # (the old universal dawn wake, and its own urge peak) and
        # require it to stay asleep. -------------------------------------
        #
        # Deliberately independent of #1939 (the missing baseline at the
        # Sleeping phase transition): this waits for a real sleeping-phase
        # tick to SAMPLE a pre-0.25 angle into sleepLastSunAngle before
        # moving the clock, so the crossing is one the edge detector can
        # actually see. Against the old fixed-dawn module that sample is
        # exactly what makes the bear wake here.
        hold_pressure(bid2, max_sp2)
        set_time_and_wait(4, 48, 0.20)  # pre-0.25, inside the bear's urge window
        if not wait_asleep(bid2):
            rep.abort("bear never returned to sleep before dawn",
                      {"sleep_phase": get_ai_field(bid2, "sleepPhase"),
                       "pose": get_pose(bid2)})
            return 2
        hold_pressure(bid2, max_sp2)
        if not poll_until(15.0, lambda: (lambda a: a is not None
                                         and 0.10 <= a < DAWN)(
                              ai_field_number(bid2, "sleepLastSunAngle"))):
            rep.abort("no sleeping-phase tick sampled a pre-dawn sun angle",
                      {"sun_angle": get_ai_field(bid2, "sleepLastSunAngle")})
            return 2
        if not ai_field_is_unset(bid2, "sleepWakeRequested"):
            rep.abort("a wake request was still pending before the dawn crossing")
            return 2
        baseline = ai_field_number(bid2, "sleepLastSunAngle")

        set_time_and_wait(7, 12, 0.30)  # past 0.25, still short of 0.75
        if not poll_until(15.0, lambda: (lambda a: a is not None and a >= DAWN)(
                              ai_field_number(bid2, "sleepLastSunAngle"))):
            rep.check("sleeps_through_dawn", False,
                      "the dawn crossing was never presented to the detector",
                      {"sun_angle": get_ai_field(bid2, "sleepLastSunAngle")})
            return 1
        # Give the reverse pose chain a couple of ticks to show itself, so
        # a wake that HAS started cannot be missed by reading too early.
        time.sleep(3.0)
        frac = pressure_frac(bid2, max_sp2)
        phase, pose = get_ai_field(bid2, "sleepPhase"), get_pose(bid2)
        if frac >= WAKE_PRESSURE_FRAC:
            rep.abort("sleep pressure reached the wake threshold during the dawn check",
                      {"fraction": frac, "threshold": WAKE_PRESSURE_FRAC})
            return 2
        stayed_asleep = phase == "sleeping" and pose == "sleeping"
        if not rep.check("sleeps_through_dawn", stayed_asleep,
                         "bear_brown slept through the dawn crossing",
                         {"baseline": baseline, "fraction": frac,
                          "sleep_phase": phase, "pose": pose}):
            return 1

        # ---- F. …and wakes on its OWN boundary (0.75) with no wakeUnit
        # call and pressure still below WAKE_PRESSURE_FRAC. --------------
        hold_pressure(bid2, max_sp2)
        set_time_and_wait(16, 48, 0.70)  # pre-0.75 baseline
        if not poll_until(15.0, lambda: (lambda a: a is not None
                                         and 0.55 <= a < DUSK)(
                              ai_field_number(bid2, "sleepLastSunAngle"))):
            rep.abort("no sleeping-phase tick sampled a pre-dusk sun angle",
                      {"sun_angle": get_ai_field(bid2, "sleepLastSunAngle"),
                       "sleep_phase": get_ai_field(bid2, "sleepPhase")})
            return 2
        if not ai_field_is_unset(bid2, "sleepWakeRequested"):
            rep.abort("a wake request was pending before the automatic dusk wake")
            return 2

        set_time_and_wait(19, 12, 0.80)  # across 0.75
        if not poll_until(20.0, lambda: get_ai_field(bid2, "sleepPhase") == "waking"):
            rep.check("wakes_at_own_boundary", False,
                      "bear_brown did not begin waking at its own dusk boundary",
                      {"sleep_phase": get_ai_field(bid2, "sleepPhase"),
                       "pose": get_pose(bid2),
                       "sun_angle": get_ai_field(bid2, "sleepLastSunAngle")})
            return 1
        frac = pressure_frac(bid2, max_sp2)
        if frac >= WAKE_PRESSURE_FRAC:
            rep.check("wakes_at_own_boundary", False,
                      "sleep pressure could have caused the dusk wake",
                      {"fraction": frac, "threshold": WAKE_PRESSURE_FRAC})
            return 1
        if not ai_field_is_unset(bid2, "sleepWakeRequested"):
            rep.check("wakes_at_own_boundary", False,
                      "a public wake request appeared during the automatic wake")
            return 1
        # Full pressure now, so the reverse pose chain can finish without
        # go_to_sleep immediately re-committing at the top of it.
        send(PORT, f"unit.setStat({bid2}, 'sleep_pressure', {max_sp2})",
             expect_result=False)
        returned = wait_for_pose(bid2, "standing", timeout=20.0)
        if not rep.check("wakes_at_own_boundary", returned,
                         "bear_brown woke automatically at dusk and returned to standing",
                         {"fraction": frac, "pose": get_pose(bid2)}):
            return 1

        rep.note("\nPASS: all #613 species-specific circadian curve and #1945 "
                 "per-species wake-boundary checks held")
        return 0
    except ProbeSetupError as error:
        rep.abort(str(error))
        return 2
    finally:
        quit_engine(PORT, proc)


if __name__ == "__main__":
    sys.exit(main())
