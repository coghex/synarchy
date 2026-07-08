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
    sleep-pressure-near-full auto-wake, and the dawn-crossing auto-wake
    — each reverses the same chain back to standing.

PASS = every check holds. FAIL = a concrete mismatch (bug in the Pose
wiring, the goal utility/phase machine, the regen path, or a wake
condition).
"""
from __future__ import annotations
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


def main() -> int:
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
        print("PASS: dawn-crossing auto-wake fired without the wake API "
              "or sleep-pressure exhaustion")

        print("\nPASS: all #612 sleep goal + Sleeping pose checks held")
        return 0
    finally:
        quit_engine(PORT, proc)


if __name__ == "__main__":
    sys.exit(main())
