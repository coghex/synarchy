#!/usr/bin/env python3
"""Position-hold probe (#1216, SURV-4) — the live gate for
`scripts/unit_ai_hold.lua`.

The project-owner decision this exists to prove: an acolyte that
COMPLETES a player-issued move order stands at the destination instead
of drifting off on ambient wander, until an accepted player command or
an explicit release supersedes the hold — while survival interrupts
stay live and RETURN it to the anchor afterwards.

Everything runs unpaused on one flat `world.initArena` page with the
REAL AI dispatch loop (`scripts/unit_ai.lua`) driving every unit, and
every window spans many ordinary thought cadences (`thought_interval`
1.0 s ± 0.5 jitter), because a single post-arrival sample cannot tell a
hold from a unit that simply has not re-decided yet.

Three units share the page: the one whose stillness the probe is
about, and the two controls that make that stillness mean something:

  * `held`     — commanded, so it holds.
  * `control`  — never commanded, so it must visibly wander and take
                 its own work while `held` does neither. Without it a
                 dead AI tick would pass every containment check.
  * `internal` — moved through `commandMove(..., internal=true)`, the
                 call `scripts/building_spawn.lua` makes for a portal
                 walk-out. A fresh acolyte must not end up pinned where
                 the roster walked it.

The work checks are a same-unit A/B, which is the only version that
proves anything: ONE mine designation row sits between the two anchors,
`held` ignores it for the whole hold, and the SAME unit takes the SAME
row within seconds of `releaseHold`. Nothing about the job changed —
only the hold did.

The displacing interrupt is `refill_canteen`, not thirst. It peaks at
7.5 (above the hold's 7.0) on a dry canteen alone, so it needs no
hydration deficit — and `docs/engine_contracts.md` records why seeding
one is a trap (the electrolyte imbalance knocks the unit unconscious
before it prefers drinking). The unit walks ~5 tiles off its anchor to
a real lake tile, really refills, and must come back: an interrupt that
drank in place would not prove the return at all.

Requirement 5's other half — an order that STALLS OUT on
`TASK_TIMEOUT_SEC` leaves no hold — is deliberately not here. It is
owned by the `--match "position hold"` hspec gate, which drives the real
`maintainTask` against a stubbed clock and proves it in milliseconds; a
live version would need a target no acolyte can approach, and this
engine's units climb walls, wade oceans and outrun a zeroed `max_speed`
(all three were tried), so every terrain fixture for it was really a
test of the pathfinder.

The one thing staged rather than observed is what the unit KNOWS: the
lake is written into `knownWaterSources` instead of waiting on a
facing-cone FOV scan to find it. What the unit DOES with that knowledge
— leave, refill, return, resume holding — is entirely the shipped AI's.

Exit 0 = every check passed.

Usage: python3 tools/position_hold_probe.py [--port 9216]
       python3 tools/position_hold_probe.py --describe   # no engine
"""
from __future__ import annotations

import argparse
import glob
import math
import sys
import time

import probe_protocol
from probelib import boot, init_arena, quit_engine, send, send_json, spawn_acolyte

LOG = "/tmp/position_hold_engine.log"
PAGE = "arena"
AI = "require('scripts.unit_ai')"

PROBE_KEY = "position_hold"

# Geometry. Everything lives on three well-separated rows so no unit's
# errands reach another's: y=0 is the held unit's, y=-24 the control's,
# y=24 the internal walk-out's.
ANCHOR = (12.5, 0.5)          # first commanded destination
ANCHOR2 = (6.5, 0.5)          # the superseding order's destination
LAKE = (18, 0)                # real fluid tile the refill walks to
MINE_ROW = (4, 0, 14, 0)      # designation row spanning both anchors
CTL_SPAWN = (0.5, -24.5)
# The control gets a row of its own, far from the held unit's and long
# enough that it never runs out and goes looking: a control that
# finished its work would roam onto the held unit's row and claim the
# very jobs the release check needs to still be there.
CTL_MINE = (-2, -24, 4, -24)
INTERNAL_SPAWN = (0.5, 24.5)
INTERNAL_TARGET = (8.5, 24.5)

# How long the containment window runs. TASK_TIMEOUT_SEC is 60 s, so a
# window this long also outlives any single order's whole budget.
HOLD_WINDOW_SEC = 32.0
HOLD_SAMPLE_SEC = 1.0
# Settle time between the hold being created (which happens while the
# unit is still walking, inside the arrival radius) and the first
# containment sample.
HOLD_SETTLE_SEC = 3.0
# The unit is standing still, so the whole window's spread should be
# indistinguishable from zero; a quarter tile is generous.
MAX_DRIFT_TILES = 0.25
# The refill excursion: ~5.5 tiles out at comfort pace, a refill, and the
# same distance back, with the AI re-deciding on its own cadence.
EXCURSION_BUDGET_SEC = 90.0
# The internal walk-out's post-arrival window, in the same units as the
# containment window: several ordinary thought cadences.
INTERNAL_WINDOW_SEC = 15.0

CHECKS = [
    ("hold_created",
     "a completed player move order anchors the unit at the commanded tile"),
    ("hold_sustained",
     "the unit stays inside the arrival radius for a sustained unpaused window"),
    ("control_autonomous",
     "a never-commanded acolyte moves freely in the same world"),
    ("work_suppressed",
     "the held unit never enters a work action with a designation in reach"),
    ("control_works",
     "the never-commanded acolyte does take its own designation"),
    ("interrupt_displaces",
     "a dry canteen outranks the hold: the unit leaves the anchor and refills"),
    ("return_to_anchor",
     "and then walks back inside the arrival radius and holds again"),
    ("command_supersedes",
     "a new player move order clears the hold and re-anchors on arrival"),
    ("release_verb",
     "releaseHold clears the hold without issuing any movement"),
    ("work_resumes",
     "the released unit takes the designation it ignored while holding"),
    ("internal_move_no_hold",
     "a completed internal move (the portal walk-out) creates no hold"),
    ("control_never_holds",
     "a never-commanded acolyte never acquires a hold"),
]

DESCRIPTOR = probe_protocol.build_descriptor(PROBE_KEY, CHECKS)


def bootstrap(port):
    for pattern, fn in [
        ("data/substances/*.yaml", "engine.loadSubstanceYaml"),
        ("data/items/*.yaml",      "engine.loadItemYaml"),
        ("data/equipment/*.yaml",  "engine.loadEquipmentYaml"),
        ("data/materials/*.yaml",  "engine.loadMaterialYaml"),
        ("data/units/*.yaml",      "engine.loadUnitYaml"),
    ]:
        for path in sorted(glob.glob(pattern)):
            send(port, f"{fn}('{path}'); return 'ok'")


def state(port, uid):
    """Position, current action, hold anchor and canteen water for `uid`."""
    return send_json(port, (
        f"local ai={AI}; local i=unit.getInfo({uid}); "
        f"if not i then return {{gone=true}} end; "
        f"local s=ai.getState({uid}) or {{}}; local h=ai.getHold({uid}); "
        f"local fill=0; "
        f"for _,it in ipairs(unit.getInventory({uid}) or {{}}) do "
        f"  if it.defName=='canteen_steel_2l' then fill=fill+(it.currentFill or 0) end "
        f"end; "
        f"return {{x=i.gridX, y=i.gridY, act=s.currentAction or 'none', "
        f"         holdX=h and h.x, holdY=h and h.y, fill=fill, "
        f"         pending=(s.commandedTask ~= nil)}}"))


def dist(s, x, y):
    return math.hypot(s["x"] - x, s["y"] - y)


def wait_for(port, uid, pred, seconds, interval=1.0):
    """Poll `state` until `pred(state)`; returns the state or None."""
    deadline = time.time() + seconds
    last = None
    while time.time() < deadline:
        last = state(port, uid)
        if isinstance(last, dict) and pred(last):
            return last
        time.sleep(interval)
    return None


def main():
    ap = argparse.ArgumentParser()
    ap.add_argument("--port", type=int, default=9216)
    ap.add_argument("--describe", action="store_true",
                    help="print this probe's probe-result/v1 check "
                         "declaration and exit; boots no engine (#1425)")
    args = ap.parse_args()
    if args.describe:
        print(DESCRIPTOR.to_json())
        return 0
    rep = probe_protocol.reporter_from_env(DESCRIPTOR)
    try:
        return _run(args.port, rep)
    finally:
        rep.close()


def _run(port, rep):
    passed = True
    proc = boot(port, rep.engine_log_path("position_hold_engine.log", LOG),
                args=rep.engine_args())
    try:
        bootstrap(port)
        init_arena(port, PAGE)
        send(port, f"world.setFluidTile('{PAGE}', {LAKE[0]}, {LAKE[1]}, 'lake')",
             expect_result=False)
        for script, order in (("unit_stats", 0.1), ("unit_resources", 0.2),
                              ("unit_ai", 0.1)):
            send(port, f"engine.loadScript('scripts/{script}.lua', {order}); "
                       f"return 'ok'")

        held = spawn_acolyte(port, 0.5, 0.5)
        control = spawn_acolyte(port, *CTL_SPAWN)
        rep.info("units spawned", {"held": held, "control": control})

        radius = float(send(port, "return require('scripts.unit_ai_stall')"
                                  ".TASK_ARRIVAL_TILES"))
        timeout_sec = float(send(port, "return require('scripts.unit_ai_stall')"
                                       ".TASK_TIMEOUT_SEC"))
        rep.info("arrival radius and stall budget read from the shipped "
                 "constants", {"tiles": radius, "timeout_sec": timeout_sec})

        # --- 1. A completed player move order creates the hold -------------
        send(port, f"{AI}.commandMove({held}, {ANCHOR[0]}, {ANCHOR[1]})",
             expect_result=False)
        s = wait_for(port, held, lambda v: v.get("holdX") is not None, 60.0)
        if s is None:
            rep.abort("the commanded acolyte never arrived and never held")
            return 1
        anchored = (s["holdX"] == ANCHOR[0] and s["holdY"] == ANCHOR[1])
        passed &= rep.check(
            "hold_created", anchored,
            f"a completed player move order anchors the unit at the commanded "
            f"tile: ({s['holdX']}, {s['holdY']})",
            {"anchor": [s["holdX"], s["holdY"]], "expected": list(ANCHOR)})

        # --- 2-5. Containment window, with work on offer -------------------
        mx1, my1, mx2, my2 = MINE_ROW
        send(port, f"world.designateMine('{PAGE}', {mx1}, {my1}, {mx2}, {my2})",
             expect_result=False)
        cx1, cy1, cx2, cy2 = CTL_MINE
        send(port, f"world.designateMine('{PAGE}', {cx1}, {cy1}, {cx2}, {cy2})",
             expect_result=False)
        time.sleep(HOLD_SETTLE_SEC)

        held_samples, ctl_samples = [], []
        deadline = time.time() + HOLD_WINDOW_SEC
        while time.time() < deadline:
            held_samples.append(state(port, held))
            ctl_samples.append(state(port, control))
            time.sleep(HOLD_SAMPLE_SEC)

        far = max(dist(v, *ANCHOR) for v in held_samples)
        drift = max(math.hypot(a["x"] - b["x"], a["y"] - b["y"])
                    for a in held_samples for b in held_samples)
        kept = all(v.get("holdX") is not None for v in held_samples)
        passed &= rep.check(
            "hold_sustained",
            kept and far <= radius and drift <= MAX_DRIFT_TILES,
            f"the unit stays inside the arrival radius for {HOLD_WINDOW_SEC:.0f}s "
            f"({len(held_samples)} samples): max {far:.3f} <= {radius}, "
            f"drift {drift:.3f}, hold kept={kept}",
            {"samples": len(held_samples), "max_distance": far,
             "radius": radius, "drift": drift, "hold_kept": kept})

        ctl_moved = max(math.hypot(a["x"] - b["x"], a["y"] - b["y"])
                        for a in ctl_samples for b in ctl_samples)
        passed &= rep.check(
            "control_autonomous", ctl_moved > 1.0,
            f"a never-commanded acolyte moves freely over the same window: "
            f"{ctl_moved:.2f} tiles",
            {"spread": ctl_moved})

        held_actions = sorted({v["act"] for v in held_samples})
        passed &= rep.check(
            "work_suppressed", held_actions == ["hold_position"],
            f"the held unit never enters a work action with a designation "
            f"in reach: {held_actions}",
            {"actions": held_actions})

        ctl_actions = sorted({v["act"] for v in ctl_samples})
        passed &= rep.check(
            "control_works", "dig_designation" in ctl_actions,
            f"the never-commanded acolyte does take its own designation: "
            f"{ctl_actions}",
            {"actions": ctl_actions})

        # --- 6-7. A displacing survival interrupt, and the return ----------
        drained = send(port, (
            f"local n=0; for _,it in ipairs(unit.getInventory({held}) or {{}}) do "
            f"  if it.defName=='canteen_steel_2l' and (it.currentFill or 0) > 0 then "
            f"    unit.modifyItemFillById({held}, it.instanceId, -it.currentFill); "
            f"    n=n+1 end end; return n"))
        send(port, f"local s={AI}.getState({held}); "
                   f"s.knownWaterSources={{{{x={LAKE[0]}, y={LAKE[1]}}}}}; "
                   f"s.nextActionAt=0; return #s.knownWaterSources")
        rep.info("canteens drained and the lake written into water memory",
                 {"canteens": drained, "lake": list(LAKE)})

        # ONE traced excursion answers both halves: how far the interrupt
        # actually carried the unit off its anchor (a refill drunk in
        # place would prove nothing about the return), whether it really
        # refilled, and whether it ended up holding the anchor again.
        excursion, refilled, returned = 0.0, False, False
        deadline = time.time() + EXCURSION_BUDGET_SEC
        while time.time() < deadline:
            v = state(port, held)
            d = dist(v, *ANCHOR)
            excursion = max(excursion, d)
            refilled = refilled or v["fill"] > 0
            if (excursion > radius and refilled and d <= radius
                    and v["act"] == "hold_position"
                    and v.get("holdX") is not None):
                returned = True
                break
            time.sleep(0.5)
        passed &= rep.check(
            "interrupt_displaces", excursion > radius and refilled,
            f"a dry canteen outranks the hold: the unit left the anchor by "
            f"{excursion:.2f} tiles (radius {radius}) and refilled={refilled}",
            {"excursion": excursion, "radius": radius, "refilled": refilled})
        passed &= rep.check(
            "return_to_anchor", returned,
            f"and then walks back inside the arrival radius and holds again: "
            f"{returned}",
            {"returned": returned})

        # --- 8. A new player command supersedes ---------------------------
        send(port, f"{AI}.commandMove({held}, {ANCHOR2[0]}, {ANCHOR2[1]})",
             expect_result=False)
        cleared = state(port, held)
        rean = wait_for(port, held,
                        lambda v: (v.get("holdX") == ANCHOR2[0]
                                   and v.get("holdY") == ANCHOR2[1]), 60.0)
        passed &= rep.check(
            "command_supersedes",
            cleared.get("holdX") is None and rean is not None,
            f"a new player move order clears the hold at once "
            f"(hold={cleared.get('holdX')}) and re-anchors on arrival "
            f"(reanchored={rean is not None})",
            {"cleared_immediately": cleared.get("holdX") is None,
             "reanchored": rean is not None})

        # --- 9-10. The release verb, and the same job it had ignored ------
        before = state(port, held)
        released = send(port, f"return tostring({AI}.releaseHold({held}))")
        after = state(port, held)
        moved = math.hypot(after["x"] - before["x"], after["y"] - before["y"])
        passed &= rep.check(
            "release_verb",
            released == "true" and after.get("holdX") is None,
            f"releaseHold clears the hold without issuing any movement: "
            f"returned {released}, moved {moved:.3f} tiles",
            {"returned": released, "moved": moved})

        working = wait_for(port, held, lambda v: v["act"] == "dig_designation",
                           45.0)
        passed &= rep.check(
            "work_resumes", working is not None,
            "the released unit takes the designation it ignored while holding"
            if working is not None else
            "the released unit never took the designation it had ignored",
            {"action": working["act"] if working else None})

        # --- 11-12. The two units that must never hold --------------------
        # Spawned and commanded HERE rather than up front, for the same
        # reason twice over: an internal move leaves the unit autonomous,
        # so it both wanders away from the target within a thought tick
        # or two (an arrival polled minutes later would already have been
        # walked off) and would have spent the whole run before this
        # wandering somewhere unknown, turning a fixed 8-tile leg into
        # however far ambient drift had carried it.
        internal = spawn_acolyte(port, *INTERNAL_SPAWN)
        rep.info("walk-out unit spawned", {"internal": internal})
        send(port, f"{AI}.commandMove({internal}, {INTERNAL_TARGET[0]}, "
                   f"{INTERNAL_TARGET[1]}, nil, true)", expect_result=False)
        # ARRIVAL first: a fresh portal acolyte that never got there
        # would trivially have no hold. Arrival is judged by WHEN the
        # order ended, not by catching the unit inside the radius: an
        # internal move leaves nothing holding it there, so it is only
        # briefly inside, and a poll can miss that window. The two ways
        # an order can end are arrival and the TASK_TIMEOUT_SEC stall
        # budget, and the budget cannot possibly be spent in under its
        # own length -- so an order that ended sooner than that ended by
        # arriving. The closest approach is reported alongside.
        started = time.time()
        closest, ended = float("inf"), False
        while time.time() - started < timeout_sec + 60.0:
            v = state(port, internal)
            closest = min(closest, dist(v, *INTERNAL_TARGET))
            if not v["pending"]:
                ended = True
                break
        elapsed = time.time() - started
        if not (ended and elapsed < timeout_sec):
            rep.abort("the internal walk-out's order never ended by arriving",
                      {"ended": ended, "elapsed": elapsed,
                       "closest": closest, "budget": timeout_sec})
            return 1
        rep.info("the walk-out arrived", {"elapsed": elapsed,
                                          "closest": closest})
        internal_samples = []
        deadline = time.time() + INTERNAL_WINDOW_SEC
        while time.time() < deadline:
            internal_samples.append(state(port, internal))
            time.sleep(HOLD_SAMPLE_SEC)
        never_held = all(v.get("holdX") is None for v in internal_samples)
        acts = sorted({v["act"] for v in internal_samples})
        passed &= rep.check(
            "internal_move_no_hold",
            never_held and "hold_position" not in acts,
            f"a completed internal move (the portal walk-out) creates no "
            f"hold: hold-free={never_held}, actions={acts}",
            {"hold_free": never_held, "actions": acts,
             "samples": len(internal_samples)})

        cv = state(port, control)
        passed &= rep.check(
            "control_never_holds", cv.get("holdX") is None,
            f"a never-commanded acolyte never acquires a hold: "
            f"{cv.get('holdX')}",
            {"hold": cv.get("holdX")})

        rep.note("\n" + ("ALL POSITION-HOLD CHECKS PASSED"
                         if passed else "SOME POSITION-HOLD CHECKS FAILED"))
        return 0 if passed else 1
    finally:
        quit_engine(port, proc)


if __name__ == "__main__":
    sys.exit(main())
