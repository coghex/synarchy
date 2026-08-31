#!/usr/bin/env python3
"""Headless probe for issue #612: the "go to sleep" AI goal + Sleeping pose.

Boots headless, spawns an acolyte on a flat arena, and exercises the
whole chain end to end:

  - Low-level pose wiring (Unit.Sim.Types' new `Sleeping` constructor +
    the acolyte.yaml `crawling-to-sleeping` / `sleeping-idle` state
    animations): unit.transitionTo can reach "sleeping" from "crawling"
    with a real (non-instant) transition, and reverse back.
  - The `go_to_sleep` AI goal (scripts/unit_ai_sleep.lua): once
    sleep_pressure deficit + circadian urge cross the threshold, the AI
    autonomously walks to a spot and plays the full multi-hop lie-down
    chain (standing -> crouching -> crawling -> sleeping), one pose per
    AI tick, exactly as unit_ai_water.lua's drink_from_source does.
  - sleep_pressure regen while actually asleep (unit_resource_tick's
    regen_factor_sleeping) — the loop #611 deliberately left open.
  - All three wake conditions: the wake API (unitAi.wakeUnit), the
    sleep-pressure-near-full auto-wake, and the wake-boundary-crossing
    auto-wake — each reverses the same chain back to standing.
  - Both sides of the sleeping-PHASE BOUNDARY (#1939), driven
    deterministically: the AI tick and the world clock are both frozen,
    and sleepExecute is called by hand, so nothing can slip a wake check
    in between the phase transition and the clock move. A unit whose
    Sleeping pose completes just BEFORE its wake boundary must wake on
    its very first sleeping-phase check once the clock is carried past
    it; a unit that enters Sleeping already just PAST the boundary must
    stay asleep and wait for the next crossing.

PASS = every check holds. FAIL = a concrete mismatch (bug in the Pose
wiring, the goal utility/phase machine, the regen path, or a wake
condition).
"""
from __future__ import annotations
import argparse
import glob
import sys
import time
from probelib import (boot, quit_engine, send, init_arena,
                       spawn_acolyte, poll_until, load_ai_stack)

PORT = 9014
LOG = "/tmp/sleep_probe_engine.log"
ARENA = "arena"


def bootstrap_defs(port: int) -> None:
    """Load the defs the loading screen would normally load (it doesn't
    run headless) plus the unit AI stack. Mirrors tools/circadian_probe.py."""
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


def get_pose(uid: int) -> str:
    return send(PORT, f"return unit.getPose({uid})")


def get_activity(uid: int) -> str:
    return send(PORT, f"return unit.getActivity({uid})")


def get_stat(uid: int, name: str):
    raw = send(PORT, f"return unit.getStat({uid}, '{name}')")
    try:
        return float(raw)
    except (TypeError, ValueError):
        return None


def get_ai_field(uid: int, field: str):
    return send(PORT,
        f"local ai = require('scripts.unit_ai'); "
        f"local s = ai.getState({uid}); "
        f"if not s then return nil end; return s.{field}")


def wait_for_pose(uid: int, target: str, timeout: float = 10.0) -> bool:
    return poll_until(timeout, lambda: get_pose(uid) == target) is not None


def wait_for_ai_field(uid: int, field: str, target: str, timeout: float = 10.0) -> bool:
    return poll_until(timeout, lambda: get_ai_field(uid, field) == target) is not None


def ai_field_number(uid: int, field: str):
    """``get_ai_field`` as a float, or None when the field is unset.

    An absent Lua field arrives over the debug console as TEXT, not as
    Python None (a bare nil reads as "nil"), so the float conversion is
    what actually tests existence here.
    """
    try:
        return float(get_ai_field(uid, field))
    except (TypeError, ValueError):
        return None


def ai_field_is_unset(uid: int, field: str) -> bool:
    return get_ai_field(uid, field) in (None, "", "nil", "null")


def set_ai_fields(uid: int, assignments: str) -> None:
    send(PORT,
         f"local s = require('scripts.unit_ai').getState({uid}); "
         f"if not s then return 'no-state' end; {assignments} return 'ok'")


def forward_arc(frm: float, to: float) -> float:
    """Distance from ``frm`` forward to ``to`` on the circular 0..1 sun
    domain — the same ``(to - frm) % 1`` arc scripts/unit_ai_sleep.lua's
    detector measures, so a case can state which SIDE of the boundary a
    sample sits on without a linear comparison that breaks at the
    midnight wrap."""
    return (to - frm) % 1.0


def global_sun_angle():
    """The page's global clock angle. ``world.getSunAngleAt(0, 0)`` has
    u = gx - gy = 0, so it carries no longitude offset at all."""
    raw = send(PORT, "return world.getSunAngleAt(0, 0)")
    try:
        return float(raw)
    except (TypeError, ValueError):
        return None


def local_sun_angle(uid: int):
    """The unit's OWN longitude-local sun angle, read exactly the way
    scripts/unit_ai_sleep.lua reads it (floored unit position, the
    longitude-aware world.getSunAngleAt)."""
    raw = send(PORT,
        f"local i = unit.getInfo({uid}); "
        f"if not i then return nil end; "
        f"return world.getSunAngleAt(math.floor(i.gridX), math.floor(i.gridY))")
    try:
        return float(raw)
    except (TypeError, ValueError):
        return None


def wake_boundary(uid: int) -> float:
    """The unit's own derived wake boundary (#1945), through the same
    exported lookup the sleeping unit itself runs."""
    raw = send(PORT,
        f"local i = unit.getInfo({uid}); "
        f"if not i then return nil end; "
        f"return require('scripts.unit_ai_sleep').wakeAngleFor(i.defName)")
    try:
        return float(raw)
    except (TypeError, ValueError):
        print(f"FAIL (setup): wakeAngleFor for unit {uid} -> {raw!r}")
        sys.exit(2)


def freeze_clock(scale: float) -> bool:
    """Set the arena's time scale (0 = stopped) and wait for the world
    thread to actually apply the queued command."""
    send(PORT, f"world.setTimeScale('{ARENA}', {scale})", expect_result=False)

    def applied():
        raw = send(PORT, f"return world.getTimeScale('{ARENA}')")
        try:
            return abs(float(raw) - scale) < 1e-6
        except (TypeError, ValueError):
            return False

    return poll_until(10.0, applied) is not None


def set_local_sun_angle(uid: int, target: float, tol: float = 0.004):
    """Move the world clock so ``uid``'s longitude-local sun angle reads
    ``target``; return what it actually reads back, or None on timeout.

    The global angle at (0, 0) is just minutes/1440
    (World.Time.Types.worldTimeToSunAngle) and a tile's local angle is
    that plus a FIXED longitude offset (World.Time.Local.localSunAngle),
    so one paired read gives the offset and the required clock follows
    directly. Measuring the offset beats assuming it: it holds wherever
    the unit happens to stand.
    """
    g, l = global_sun_angle(), local_sun_angle(uid)
    if g is None or l is None:
        return None
    offset  = forward_arc(g, l)
    minutes = int(round(forward_arc(offset, target) * 1440.0)) % 1440
    send(PORT, f"world.setTime('{ARENA}', {minutes // 60}, {minutes % 60})",
         expect_result=False)

    def settled():
        a = local_sun_angle(uid)
        if a is None:
            return None
        d = forward_arc(a, target)
        # Box the reading, so a legitimate 0.0 still reads as success.
        return (a,) if min(d, 1.0 - d) <= tol else None

    got = poll_until(10.0, settled)
    return got[0] if got else None


def sleep_execute_once(uid: int) -> str:
    """Run EXACTLY one scripts/unit_ai_sleep.sleepExecute tick by hand and
    report the resulting phase. The live AI update is neutralised while
    these cases run, so this is the only thing that advances the sleep
    state machine — which is what makes the phase boundary observable at
    all."""
    return send(PORT,
        f"local sleepGoal = require('scripts.unit_ai_sleep'); "
        f"local ai = require('scripts.unit_ai'); "
        f"local cfg = require('scripts.unit_ai_tunables'); "
        f"local s = ai.getState({uid}); "
        f"if not s then return 'no-state' end; "
        f"sleepGoal.sleepExecute({uid}, s, cfg.acolyte); "
        f"return tostring(s.sleepPhase)")


def max_sleep_pressure(uid: int) -> float:
    raw = send(PORT,
        f"return require('scripts.unit_stats').get({uid}, 'max_sleep_pressure')")
    try:
        v = float(raw)
    except (TypeError, ValueError):
        print(f"FAIL (setup): max_sleep_pressure -> {raw!r}")
        sys.exit(2)
    if v <= 0:
        print(f"FAIL (setup): max_sleep_pressure = {v}, expected > 0")
        sys.exit(2)
    return v


def enter_sleep_cycle(uid: int, label: str) -> None:
    """Wait for the AI to pick go_to_sleep and lie all the way down to
    the sleeping pose, checking every hop of the chain along the way."""
    if not wait_for_ai_field(uid, "currentAction", "go_to_sleep", timeout=10.0):
        print(f"FAIL ({label}): AI never picked go_to_sleep "
              f"(currentAction={get_ai_field(uid, 'currentAction')!r})")
        sys.exit(1)
    print(f"PASS ({label}): AI selected go_to_sleep")

    # Generous: the walk-to-spot leg can cover up to sleep_spot_radius
    # (6 tiles) at meander speed (~0.25x max_speed) before lying_down
    # even starts. Accept "sleeping" too, not just "lying_down" — on a
    # re-entry (e.g. right after a forced wake, deficit/urge still high)
    # the whole lie-down chain can complete between two 0.3s polls, so
    # "lying_down" itself may never be the LATEST sampled value.
    if not poll_until(30.0, lambda: get_ai_field(uid, "sleepPhase")
                       in ("lying_down", "sleeping")):
        print(f"FAIL ({label}): sleepPhase never reached lying_down "
              f"(sleepPhase={get_ai_field(uid, 'sleepPhase')!r} "
              f"sleepSpot={get_ai_field(uid, 'sleepSpot')!r} "
              f"pose={get_pose(uid)!r} info={send(PORT, f'return unit.getInfo({uid})')!r})")
        sys.exit(1)

    # Individual pose-hop checks (crouching, crawling) are best-effort —
    # a fast re-entry can skip through them between polls exactly like
    # sleepPhase above. The hop mechanism itself is already rigorously
    # proven by the low-level pose-wiring checks and cycle 1's own first
    # (necessarily-fresh, so slower) descent; here only the OUTCOME
    # (genuinely reaches sleeping) is load-bearing.
    wait_for_pose(uid, "crouching", timeout=3.0)
    wait_for_pose(uid, "crawling", timeout=3.0)
    if not wait_for_pose(uid, "sleeping", timeout=8.0):
        print(f"FAIL ({label}): pose never reached sleeping")
        sys.exit(1)
    if not wait_for_ai_field(uid, "sleepPhase", "sleeping", timeout=5.0):
        print(f"FAIL ({label}): sleepPhase never settled to 'sleeping' "
              f"after pose arrived")
        sys.exit(1)
    print(f"PASS ({label}): lie-down chain standing -> crouching -> "
          f"crawling -> sleeping completed, sleepPhase == 'sleeping'")


def wait_for_wake(uid: int, label: str) -> None:
    """Wait for the reverse chain (sleeping -> crawling -> crouching ->
    standing). Doesn't linger on "sleepPhase cleared" afterward — with
    the deficit/urge that drove the unit to sleep in the first place
    still high (a short forced nap barely dents sleep_pressure, and the
    clock is still in the same dusk window), the AI can legitimately
    re-decide to go straight back to sleep the instant it reaches
    standing, racing that assertion. Reaching standing IS the proof the
    wake-reverse-chain ran correctly."""
    if not wait_for_pose(uid, "crawling", timeout=8.0):
        print(f"FAIL ({label}): pose never reached crawling while waking")
        sys.exit(1)
    if not wait_for_pose(uid, "crouching", timeout=5.0):
        print(f"FAIL ({label}): pose never reached crouching while waking")
        sys.exit(1)
    if not wait_for_pose(uid, "standing", timeout=5.0):
        print(f"FAIL ({label}): pose never returned to standing")
        sys.exit(1)
    print(f"PASS ({label}): wake chain sleeping -> crawling -> crouching -> "
          f"standing completed")


# scripts/unit_ai_sleep.lua's own wake-pressure fraction — mirrored so a
# boundary case can PROVE the pressure condition was not what woke (or
# could have woken) the unit.
WAKE_PRESSURE_FRAC = 0.98

# How far either side of the wake boundary the phase-boundary cases park
# the clock: ~29 game-minutes, far wider than the 1/1440 granularity of
# world.setTime and than any drift a stopped clock could accumulate.
BOUNDARY_MARGIN = 0.02


def phase_boundary_case(uid: int, max_sp: float, label: str,
                        entry_offset: float, cross_offset: float,
                        expect_wake: bool) -> None:
    """One side of the sleeping-phase boundary (#1939), deterministically.

    Both offsets are relative to the unit's OWN derived wake boundary
    (#1945), so the case says "just before" / "just past" rather than
    hard-coding 0.25. The caller must already have neutralised the live
    AI and resource updates and stopped the clock: the point of this case
    is that NO sleeping-phase wake evaluation happens between the phase
    transition sampling its baseline and the clock being carried across
    the boundary, which is precisely the window a live 0.5-1.5 s thought
    cadence cannot pin down.
    """
    boundary     = wake_boundary(uid)
    entry_target = (boundary + entry_offset) % 1.0
    cross_target = (boundary + cross_offset) % 1.0

    # Clean slate. Pressure far below WAKE_PRESSURE_FRAC and no pending
    # wake request, so the boundary is the ONLY condition that can fire.
    set_ai_fields(uid, "s.sleepPhase = nil; s.sleepLastSunAngle = nil; "
                       "s.sleepWakeRequested = nil;")
    send(PORT, f"unit.setStat({uid}, 'sleep_pressure', {0.40 * max_sp})",
         expect_result=False)

    # Real pose chain down to Sleeping, with the phase already marked
    # lying_down (that is the state sleepExecute's transition branch
    # reads, and it is also what exempts the crawling hop from
    # unit_resource_injury's locomotor watchdog).
    set_ai_fields(uid, "s.sleepPhase = 'lying_down';")
    for pose in ("crouching", "crawling", "sleeping"):
        send(PORT, f"unit.transitionTo({uid}, '{pose}', 2)", expect_result=False)
        if not wait_for_pose(uid, pose, timeout=12.0):
            print(f"FAIL (setup, {label}): pose never reached {pose!r} "
                  f"(pose={get_pose(uid)!r})")
            sys.exit(2)

    # Park the clock on the intended side of the boundary BEFORE the
    # transition runs.
    entry_angle = set_local_sun_angle(uid, entry_target)
    if entry_angle is None:
        print(f"FAIL (setup, {label}): could not park the unit's local sun "
              f"angle at {entry_target:.4f}")
        sys.exit(2)

    # THE transition: lying_down -> sleeping, exactly once.
    phase = sleep_execute_once(uid)
    if phase != "sleeping":
        print(f"FAIL (setup, {label}): the lying_down -> sleeping transition "
              f"did not run (sleepPhase={phase!r} pose={get_pose(uid)!r})")
        sys.exit(2)

    baseline = ai_field_number(uid, "sleepLastSunAngle")
    if baseline is None:
        print(f"FAIL ({label}): entering the sleeping phase left "
              f"sleepLastSunAngle={get_ai_field(uid, 'sleepLastSunAngle')!r} — "
              f"nil is the ABSENCE of a dawn-crossing baseline, so the first "
              f"sleeping-phase check has nothing to cross from and the "
              f"boundary swept before it is lost for the whole day (#1939)")
        sys.exit(1)

    # The baseline must sit on the side the case asked for, stated as the
    # forward arc the detector itself measures rather than a linear
    # comparison that would break at the midnight wrap.
    arc_to_boundary = forward_arc(baseline, boundary)
    expected_arc    = (-entry_offset) % 1.0
    if abs(arc_to_boundary - expected_arc) > 0.01:
        print(f"FAIL (setup, {label}): the seeded baseline {baseline:.4f} is "
              f"{arc_to_boundary:.4f} of a day short of the boundary "
              f"{boundary:.4f}, expected ~{expected_arc:.4f} — the clock was "
              f"parked at {entry_angle:.4f}, so the transition did not sample "
              f"the unit's own angle at that moment")
        sys.exit(2)
    print(f"PASS ({label}): entering the sleeping phase seeded "
          f"sleepLastSunAngle = {baseline:.4f} (boundary {boundary:.4f} is "
          f"{arc_to_boundary:.4f} of a day forward)")

    # Carry the clock to the other target. Still no AI tick, so this is
    # the FIRST sleeping-phase wake check the unit ever gets.
    cross_angle = set_local_sun_angle(uid, cross_target)
    if cross_angle is None:
        print(f"FAIL (setup, {label}): could not move the unit's local sun "
              f"angle to {cross_target:.4f}")
        sys.exit(2)

    sp = get_stat(uid, "sleep_pressure")
    if sp is None or sp / max_sp >= WAKE_PRESSURE_FRAC:
        print(f"FAIL (setup, {label}): sleep_pressure is {sp!r} "
              f"({'?' if sp is None else f'{sp / max_sp:.3f}'} of max), at or "
              f"past WAKE_PRESSURE_FRAC — the pressure condition would "
              f"decide this case instead of the boundary")
        sys.exit(2)
    if not ai_field_is_unset(uid, "sleepWakeRequested"):
        print(f"FAIL (setup, {label}): a wake request is pending, so this "
              f"case would prove the public API rather than the boundary")
        sys.exit(2)

    after = sleep_execute_once(uid)
    if expect_wake and after != "waking":
        print(f"FAIL ({label}): the first sleeping-phase check after the sun "
              f"crossed the boundary left sleepPhase={after!r}, expected "
              f"'waking' (baseline {baseline:.4f} -> {cross_angle:.4f}, "
              f"boundary {boundary:.4f})")
        sys.exit(1)
    if not expect_wake and after != "sleeping":
        print(f"FAIL ({label}): a unit that entered the sleeping phase "
              f"already PAST its boundary left sleepPhase={after!r}, expected "
              f"'sleeping' — it must wait for the NEXT crossing "
              f"(baseline {baseline:.4f} -> {cross_angle:.4f}, boundary "
              f"{boundary:.4f})")
        sys.exit(1)
    verdict = "woke on its first check" if expect_wake else "stayed asleep"
    print(f"PASS ({label}): {verdict} "
          f"({baseline:.4f} -> {cross_angle:.4f}, boundary {boundary:.4f})")


def main() -> int:
    ap = argparse.ArgumentParser(description=__doc__)
    ap.add_argument("--port", type=int, default=9014)
    args = ap.parse_args()
    global PORT
    PORT = args.port

    proc = boot(PORT, log=LOG)
    try:
        bootstrap_defs(PORT)
        init_arena(PORT, name=ARENA)

        # ---- Low-level pose wiring: crawling <-> sleeping is a REAL
        # (non-instant) transition, not a silent snap from a missing
        # YAML key. spawn_acolyte needs one real AI tick to seed AI
        # state (find_water clear), so neutralise the wander tick
        # (mirrors tools/movement_probe.py) only AFTER spawning —
        # otherwise unit_ai's own wander decision races these manual
        # transitionTo calls and can cancel an in-flight hop with a
        # moveTo of its own.
        # ----------------------------------------------------
        uid1 = spawn_acolyte(PORT, 0, 0)
        # Save the real update closures in Lua globals so they can be
        # restored exactly (reloading the script file to "restore" it
        # doesn't reliably re-attach the engine's per-frame update hook —
        # patch-in-place / restore-from-saved-ref is the safe pattern).
        send(PORT, "_G.__realAiUpdate = require('scripts.unit_ai').update; "
                   "_G.__realResUpdate = require('scripts.unit_resources').update; "
                   "return 'ok'")
        send(PORT, "require('scripts.unit_ai').update = function() end; return 'ok'")
        # Also neutralise unit_resources' tick: its locomotor state
        # machine (scripts/unit_resource_injury.lua) auto-revives any
        # unit it finds resting in "crawling" pose with healthy legs
        # (crawling normally means "legs disabled") — which would
        # immediately undo these raw, no-injury manual transitions. The
        # #612 sleep goal exempts itself from that watchdog via
        # unitAi.getState(uid).sleepPhase; this raw low-level check
        # bypasses the AI entirely, so it needs the watchdog off instead.
        send(PORT, "require('scripts.unit_resources').update = function() end; return 'ok'")
        send(PORT, f"unit.transitionTo({uid1}, 'crouching', 1)", expect_result=False)
        if not wait_for_pose(uid1, "crouching"):
            print("FAIL (setup): unit1 never reached crouching")
            return 1
        send(PORT, f"unit.transitionTo({uid1}, 'crawling', 1)", expect_result=False)
        if not wait_for_pose(uid1, "crawling"):
            print("FAIL (setup): unit1 never reached crawling")
            return 1

        send(PORT, f"unit.transitionTo({uid1}, 'sleeping', 1)", expect_result=False)
        # Immediately after issuing the command the unit should be mid
        # transition, not already at the target — proves a real
        # crawling_to_sleeping animation was found (duration > 0), not
        # the "no asset -> instant snap" fallback path.
        activity = get_activity(uid1)
        if activity != "transitioning":
            print(f"FAIL: crawling->sleeping transitionTo was instant "
                  f"(activity={activity!r} right after issuing it, expected "
                  f"'transitioning') — crawling_to_sleeping asset not wired?")
            return 1
        print("PASS: crawling->sleeping is a real (non-instant) transition")
        if not wait_for_pose(uid1, "sleeping"):
            print("FAIL: unit1 never settled into the sleeping pose")
            return 1
        print("PASS: unit.getPose reports 'sleeping' after the transition completes")

        # Reverse: sleeping -> crawling should play the SAME asset backward.
        send(PORT, f"unit.transitionTo({uid1}, 'crawling', 1)", expect_result=False)
        if not wait_for_pose(uid1, "crawling"):
            print("FAIL: unit1 never reversed sleeping -> crawling")
            return 1
        print("PASS: sleeping->crawling reverses the shared asset")

        # ---- sleepUtility blends exhaustion (scripts/exhaustion.lua,
        # #610), not just sleep_pressure deficit + circadian urge. Calls
        # sleepUtility directly (deterministic — no AI-timing dependency)
        # at a fixed deficit/time so only exhaustion varies between the
        # two reads. Still runs with unit_ai.update neutralised (from the
        # low-level section above) so the real AI can't also act on uid1
        # in the background and race sleepPhase out from under this. ----
        max_sp1 = max_sleep_pressure(uid1)
        send(PORT, f"unit.setStat({uid1}, 'sleep_pressure', {0.60 * max_sp1})",
             expect_result=False)  # deficit = 0.40, comfortably clear of the
                                    # 0.35 sleep_min_deficit gate — not exactly
                                    # on it, which is float-boundary-fragile
                                    # across the Python->Lua round-trip
        send(PORT, f"world.setTime('{ARENA}', 12, 0)", expect_result=False)  # noon: urge ~= 0
        max_exh = send(PORT,
            f"return require('scripts.unit_stats').get({uid1}, 'max_exhaustion')")
        try:
            max_exh = float(max_exh)
        except (TypeError, ValueError):
            print(f"FAIL (setup): max_exhaustion -> {max_exh!r}")
            return 2

        def sleep_utility() -> float:
            raw = send(PORT,
                f"local sleepGoal = require('scripts.unit_ai_sleep'); "
                f"local ai = require('scripts.unit_ai'); "
                f"local cfg = require('scripts.unit_ai_tunables'); "
                f"return sleepGoal.sleepUtility({uid1}, ai.getState({uid1}), cfg.acolyte)")
            try:
                return float(raw)
            except (TypeError, ValueError):
                print(f"FAIL: sleepUtility -> {raw!r}")
                sys.exit(1)

        send(PORT, f"unit.setStat({uid1}, 'exhaustion', {max_exh})", expect_result=False)
        rested_utility = sleep_utility()
        send(PORT, f"unit.setStat({uid1}, 'exhaustion', 0)", expect_result=False)
        exhausted_utility = sleep_utility()
        if not (exhausted_utility > rested_utility + 1.0):
            print(f"FAIL: exhaustion did not raise sleepUtility (rested="
                  f"{rested_utility:.4f}, exhausted={exhausted_utility:.4f}) — "
                  f"same sleep_pressure deficit and circadian urge throughout, "
                  f"so the gap should come from exhaustion alone")
            return 1
        print(f"PASS: exhaustion raises go_to_sleep utility at a fixed "
              f"deficit/time ({rested_utility:.4f} -> {exhausted_utility:.4f})")

        # Restore the real update closures saved above — phase 2 needs
        # genuine AI-driven decisions AND the real resource tick
        # (sleep_pressure regen, the locomotor watchdog's #612 exemption).
        send(PORT, "require('scripts.unit_ai').update = _G.__realAiUpdate; "
                   "require('scripts.unit_resources').update = _G.__realResUpdate; "
                   "return 'ok'")

        # ---- High-level: the go_to_sleep AI goal, end to end ----------
        uid2 = spawn_acolyte(PORT, 20, 20)
        max_sp = max_sleep_pressure(uid2)

        def set_deficit(frac: float) -> None:
            # deficit = 1 - sp/max, so sp = (1-frac)*max.
            sp = (1.0 - frac) * max_sp
            send(PORT, f"unit.setStat({uid2}, 'sleep_pressure', {sp})",
                 expect_result=False)

        # Comfortably above sleep_min_deficit (0.35) so go_to_sleep wins
        # outright regardless of the exact circadian urge reading.
        set_deficit(0.6)
        # Dusk: circadian urge near its peak (matches circadian_probe's
        # own validated target angle for 18:00).
        send(PORT, f"world.setTime('{ARENA}', 18, 0)", expect_result=False)

        enter_sleep_cycle(uid2, "cycle 1 (wake API)")

        # Regen check: sleep_pressure must actually rise while asleep —
        # the loop #611 deliberately left open.
        before = get_stat(uid2, "sleep_pressure")
        time.sleep(2.0)
        after = get_stat(uid2, "sleep_pressure")
        if before is None or after is None:
            print(f"FAIL: sleep_pressure unreadable while asleep "
                  f"(before={before!r}, after={after!r})")
            return 1
        if after <= before:
            print(f"FAIL: sleep_pressure did not rise while sleeping "
                  f"({before:.4f} -> {after:.4f}) — regen_factor_sleeping "
                  f"not applying?")
            return 1
        print(f"PASS: sleep_pressure regens while asleep "
              f"({before:.4f} -> {after:.4f} over ~2s)")

        # Wake condition 1: the public wake API.
        send(PORT, f"require('scripts.unit_ai').wakeUnit({uid2})",
             expect_result=False)
        wait_for_wake(uid2, "cycle 1 wake (API)")

        # ---- Wake condition 2: sleep_pressure near-full auto-wake -----
        # Deficit is still well above the floor (only ~2s of regen), so
        # the AI immediately re-commits to sleep on its own.
        enter_sleep_cycle(uid2, "cycle 2 (pressure auto-wake)")
        send(PORT, f"unit.setStat({uid2}, 'sleep_pressure', {0.99 * max_sp})",
             expect_result=False)
        wait_for_wake(uid2, "cycle 2 wake (pressure)")
        print("PASS: sleep_pressure-near-full auto-wake fired without "
              "the wake API")

        # ---- Wake condition 3: dawn-crossing auto-wake -----------------
        # Reset pressure back down (so the pressure auto-wake can't fire
        # first) and anchor the clock safely before dawn so the module's
        # crossing-detector has a baseline sample below the threshold.
        set_deficit(0.6)
        send(PORT, f"world.setTime('{ARENA}', 3, 0)", expect_result=False)
        enter_sleep_cycle(uid2, "cycle 3 (dawn auto-wake)")
        # Let at least one "sleeping"-phase tick sample the pre-dawn
        # angle as its baseline before crossing.
        time.sleep(2.0)
        send(PORT, f"world.setTime('{ARENA}', 6, 10)", expect_result=False)
        wait_for_wake(uid2, "cycle 3 wake (dawn)")
        print("PASS: wake-boundary crossing auto-wake fired without the "
              "wake API or sleep-pressure exhaustion")

        # ---- Wake condition 3, both sides of the PHASE boundary (#1939)
        # Cycle 3 above proves the ORDINARY sampled crossing: two
        # sleeping-phase checks straddling the boundary, with the earlier
        # one deliberately waited for. That is precisely the case the
        # missing phase-entry baseline never affected. These two cases
        # cover the boundary itself, and they cannot be run against a
        # live AI: whether a wake check lands inside the 0.5-1.5 s gap
        # between the phase transition and the clock move is exactly what
        # the defect is about, so the whole point is that no such check
        # happens. So both the AI tick and the world clock are stopped
        # and sleepExecute is called by hand, one tick at a time.
        # -----------------------------------------------------------
        uid3 = spawn_acolyte(PORT, 36, 36)
        uid4 = spawn_acolyte(PORT, 40, 40)
        max_sp3, max_sp4 = max_sleep_pressure(uid3), max_sleep_pressure(uid4)

        send(PORT, "require('scripts.unit_ai').update = function() end; "
                   "require('scripts.unit_resources').update = function() end; "
                   "return 'ok'")
        if not freeze_clock(0.0):
            print(f"FAIL (setup): the arena clock never stopped "
                  f"(getTimeScale -> "
                  f"{send(PORT, f'return world.getTimeScale({ARENA!r})')!r})")
            return 2
        print("PASS: AI tick and world clock both stopped — the phase "
              "boundary is now directly observable")

        # (a) Enters Sleeping just BEFORE its boundary, and the very first
        # sleeping-phase check it ever gets is already past it. This is
        # the case that fails on an unfixed module: the transition stored
        # no baseline, so that first check only RECORDS one and the
        # crossing is gone until the next day.
        phase_boundary_case(uid3, max_sp3, "phase boundary (enters pre-boundary)",
                            entry_offset=-BOUNDARY_MARGIN,
                            cross_offset=+BOUNDARY_MARGIN,
                            expect_wake=True)

        # (b) The other side: entering Sleeping ALREADY past the boundary
        # (the "forced by exhaustion at noon" case the module header
        # describes) must not wake immediately just because a baseline
        # now exists. It waits for the NEXT crossing.
        phase_boundary_case(uid4, max_sp4, "phase boundary (enters post-boundary)",
                            entry_offset=+BOUNDARY_MARGIN,
                            cross_offset=+3 * BOUNDARY_MARGIN,
                            expect_wake=False)

        # Hand the engine back the way it was found.
        send(PORT, "require('scripts.unit_ai').update = _G.__realAiUpdate; "
                   "require('scripts.unit_resources').update = _G.__realResUpdate; "
                   "return 'ok'")
        freeze_clock(1.0)

        print("\nPASS: all #612 sleep goal + Sleeping pose checks held")
        return 0
    finally:
        quit_engine(PORT, proc)


if __name__ == "__main__":
    sys.exit(main())
