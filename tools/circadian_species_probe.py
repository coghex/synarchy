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

Three layers, cheapest/most isolated first:

  A. Raw urge (scripts.circadian.getCircadianUrge): at a fixed time,
     acolyte and bear read OPPOSITE phases — acolyte peaks at dusk /
     flat at dawn, bear peaks at dawn / flat at dusk.
  B. sleepUtility (scripts.unit_ai_sleep.sleepUtility), called directly
     like sleep_probe.py's exhaustion check: with sleep_pressure deficit
     and exhaustion held identical and fixed for both units, only the
     urge term can move the score — and it moves in opposite directions
     for the two species across dusk/dawn.
  C. End to end: a fresh bear_brown, sleep-deprived and released into a
     dawn clock, is actually PICKED by the AI (go_to_sleep) and walks
     the real pose chain down to Sleeping — proving the reused-art
     state_animations aliases actually resolve — then wakes via the
     public wake API back to standing.

PASS = every check holds. FAIL = a concrete mismatch.
"""
from __future__ import annotations
import glob
import sys
from probelib import boot, quit_engine, send, init_arena, spawn_acolyte, poll_until, load_ai_stack

PORT = 9016
LOG = "/tmp/circadian_species_probe_engine.log"
ARENA = "arena"


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
        print(f"FAIL (setup): sun angle never settled near {target} after "
              f"world.setTime('{ARENA}', {hour}, {minute})")
        sys.exit(2)


def urge(uid: int) -> float:
    raw = send(PORT, f"return require('scripts.circadian').getCircadianUrge({uid})")
    try:
        return float(raw)
    except (TypeError, ValueError):
        print(f"FAIL: getCircadianUrge({uid}) -> {raw!r}")
        sys.exit(1)


def sleep_utility(uid: int, cfg_key: str) -> float:
    raw = send(PORT,
        f"local sleepGoal = require('scripts.unit_ai_sleep'); "
        f"local ai = require('scripts.unit_ai'); "
        f"local cfg = require('scripts.unit_ai_tunables'); "
        f"return sleepGoal.sleepUtility({uid}, ai.getState({uid}), cfg.{cfg_key})")
    try:
        return float(raw)
    except (TypeError, ValueError):
        print(f"FAIL: sleepUtility({uid}, {cfg_key}) -> {raw!r}")
        sys.exit(1)


def max_stat(uid: int, name: str) -> float:
    raw = send(PORT, f"return require('scripts.unit_stats').get({uid}, '{name}')")
    try:
        v = float(raw)
    except (TypeError, ValueError):
        print(f"FAIL (setup): {name} -> {raw!r}")
        sys.exit(2)
    if v <= 0:
        print(f"FAIL (setup): {name} = {v}, expected > 0")
        sys.exit(2)
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
    proc = boot(PORT, log=LOG)
    try:
        bootstrap_defs(PORT)
        init_arena(PORT, name=ARENA)

        # ---- A + B setup: two units pinned at (0,0) (no longitude
        # offset), AI tick neutralised so neither wanders off-tile
        # while we probe fixed times. ----------------------------------
        aid = spawn_acolyte(PORT, 0, 0)
        bid = spawn_acolyte(PORT, 0, 0, unit="bear_brown", clear_water=False)
        if not wait_for_ai_state(bid):
            print("FAIL (setup): bear_brown never got AI state")
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

        if a_urge_dusk < 0.95:
            print(f"FAIL: acolyte urge at dusk = {a_urge_dusk}, expected ~peak")
            return 1
        if b_urge_dusk > 0.05:
            print(f"FAIL: bear urge at dusk = {b_urge_dusk}, expected ~flat "
                  f"(bear's circadian_center is dawn, not dusk)")
            return 1
        if a_urge_dawn > 0.05:
            print(f"FAIL: acolyte urge at dawn = {a_urge_dawn}, expected ~flat")
            return 1
        if b_urge_dawn < 0.95:
            print(f"FAIL: bear urge at dawn = {b_urge_dawn}, expected ~peak "
                  f"(bear_brown.circadian_center = 0.25 in unit_resource_config.lua)")
            return 1
        print(f"PASS: raw circadian urge is phase-shifted per species — "
              f"dusk: acolyte={a_urge_dusk:.3f} bear={b_urge_dusk:.3f}; "
              f"dawn: acolyte={a_urge_dawn:.3f} bear={b_urge_dawn:.3f}")

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

        if not (a_util_dusk > a_util_dawn + 1.0):
            print(f"FAIL: acolyte sleepUtility should be higher at its own "
                  f"dusk peak than at dawn (dusk={a_util_dusk:.3f}, "
                  f"dawn={a_util_dawn:.3f})")
            return 1
        if not (b_util_dawn > b_util_dusk + 1.0):
            print(f"FAIL: bear sleepUtility should be higher at its own "
                  f"dawn peak than at dusk (dawn={b_util_dawn:.3f}, "
                  f"dusk={b_util_dusk:.3f})")
            return 1
        if not (a_util_dusk > b_util_dusk):
            print(f"FAIL: at dusk, acolyte's own-peak utility ({a_util_dusk:.3f}) "
                  f"should exceed bear's off-peak utility ({b_util_dusk:.3f})")
            return 1
        if not (b_util_dawn > a_util_dawn):
            print(f"FAIL: at dawn, bear's own-peak utility ({b_util_dawn:.3f}) "
                  f"should exceed acolyte's off-peak utility ({a_util_dawn:.3f})")
            return 1
        print(f"PASS: go_to_sleep utility crosses over between species — "
              f"dusk: acolyte={a_util_dusk:.3f} bear={b_util_dusk:.3f}; "
              f"dawn: acolyte={a_util_dawn:.3f} bear={b_util_dawn:.3f}")

        # ---- C. End to end: a fresh bear actually seeks + reaches real
        # sleep at its circadian peak, exercising the new bear_brown.yaml
        # pose-chain aliases, then wakes via the public API. -------------
        send(PORT, "require('scripts.unit_ai').update = _G.__realAiUpdate; "
                   "return 'ok'")

        bid2 = spawn_acolyte(PORT, 20, 20, unit="bear_brown", clear_water=False)
        max_sp2 = max_stat(bid2, "max_sleep_pressure")
        send(PORT, f"unit.setStat({bid2}, 'sleep_pressure', {0.4 * max_sp2})",
             expect_result=False)  # deficit = 0.6, comfortably above the 0.35 floor
        set_time_and_wait(6, 0, DAWN)  # bear's circadian peak

        if not poll_until(15.0, lambda: get_ai_field(bid2, "currentAction") == "go_to_sleep"):
            print(f"FAIL: bear never picked go_to_sleep at its dawn peak "
                  f"(currentAction={get_ai_field(bid2, 'currentAction')!r})")
            return 1
        print("PASS: bear_brown's AI selected go_to_sleep at its circadian peak (dawn)")

        if not poll_until(30.0, lambda: get_ai_field(bid2, "sleepPhase")
                          in ("lying_down", "sleeping")):
            print(f"FAIL: bear's sleepPhase never reached lying_down "
                  f"(sleepPhase={get_ai_field(bid2, 'sleepPhase')!r} "
                  f"pose={get_pose(bid2)!r})")
            return 1
        if not wait_for_pose(bid2, "sleeping", timeout=10.0):
            print(f"FAIL: bear never reached the real Sleeping pose "
                  f"(pose={get_pose(bid2)!r}) — check bear_brown.yaml's "
                  f"crouching/crawling/sleeping state_animations aliases")
            return 1
        print("PASS: bear_brown reached the real Sleeping pose via the "
              "reused sit/lie/sleep art (standing -> crouching -> "
              "crawling -> sleeping)")

        send(PORT, f"require('scripts.unit_ai').wakeUnit({bid2})", expect_result=False)
        if not wait_for_pose(bid2, "standing", timeout=15.0):
            print(f"FAIL: bear never woke back to standing "
                  f"(pose={get_pose(bid2)!r})")
            return 1
        print("PASS: bear_brown woke via the public wake API back to standing")

        print("\nPASS: all #613 species-specific circadian curve checks held")
        return 0
    finally:
        quit_engine(PORT, proc)


if __name__ == "__main__":
    sys.exit(main())
