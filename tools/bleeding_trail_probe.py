#!/usr/bin/env python3
"""Headless probe for issues #882 (bleeding trails) and #883 (pooling).

Boots headless on a flat arena, spawns fresh acolytes (one per case, so
a prior case's decals/trail state can never contaminate the next), and
drives the debug `unit.injure(...)` + `unit.moveTo(...)` paths end to
end against the `blood.*` debug surface (#604/#606) to verify BOTH
halves of ongoing bleeding, which share one per-unit accumulator:

  * the MOVING half (Blood.Trail, #882) — a unit with an externally
    bleeding wound leaves a bounded, distance/cadence-gated trail of
    marks along its travelled path, driven by conserved real blood loss;
  * the STATIONARY half (Blood.Pool, #883) — a unit bleeding in place
    (standing, shuffling, or collapsed) instead grows a LOCAL pool from
    layered bounded spawns, up to a documented per-cluster bound.

`unit_ai`'s wander tick is neutralised (the `movement_probe.py`
technique) so `unit.moveTo` is the only source of movement in every
case — otherwise an unclearable `find_water` wander goal could add
uncontrolled extra distance and desync the mark-count bounds below.

Trail checks (#882):
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

Pooling checks (#883, lettered per that issue's acceptance list):
  6. (a) a stationary externally-bleeding unit accumulates marks
     clustered within POOL_JITTER_RADIUS of the anchor `getTrailState`
     reports, count growing over time, every mark carrying the unit's
     own `sourceUnit`, and every one drawn from the pool/drops style
     family at a small footprint (checked through `blood.listTextures`,
     which is the only surface exposing style/footprint at all).
     (g) growth is strictly ADDITIVE: every decal id present before the
     pool grew still reports identical stable fields afterwards.
     NB the exact whole-record non-mutation claim is asserted in hspec
     (`Blood.Pool growth is strictly ADDITIVE`) against real BloodDecal
     values — `blood.listDecals` cannot see `bdeInitialWetness`, and
     recomputes age/wetness/dryness on every call.
  7. (b) a long dwell stops at exactly POOL_MAX_LAYERS: the count stays
     flat while the wound is demonstrably still bleeding. Timed against
     REAL elapsed seconds — `world.setTimeScale` advances only the world
     calendar, never the unpaused `gameTimeRef` this cadence uses, so it
     cannot fast-forward a dwell.
  8. (c) clot progression to zero effective external bleed stops
     layering BEFORE the bound is reached.
  9. (d) walk-then-stop: one accumulator produces trail marks along the
     route AND a growing cluster at the stop point.
 10. (e) a collapsed unit pools exactly like a standing one, and its
     cluster state is dropped at death while the marks persist and age.
 11. (f) two adjacent bleeders grow two independent, individually
     bounded clusters.
 12. (#883 requirement 7) pool density is invariant to
     `world.setTimeScale`, for the same reason the trail's is.

PASS  = all checks hold.
FAIL  = any check violated (bug in the emitters or their wiring).
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
PAGE = "arena"

# Mirrors Blood.Trail.defaultTrailThresholds — kept in sync by hand
# (this probe treats them as the documented contract, not a live query).
MIN_DISTANCE = 1.0   # tiles
MIN_CADENCE = 0.5    # seconds

# Mirrors Blood.Pool.defaultPoolThresholds, same hand-sync convention.
POOL_CLUSTER_RADIUS = 1.0    # tiles — leaving this of the anchor ends the cluster
POOL_MAX_LAYERS = 12         # per-cluster layer bound
POOL_MIN_CADENCE = 1.5       # real seconds between layers
POOL_MIN_VOLUME = 0.015      # litres of external loss per layer
POOL_JITTER_RADIUS = 0.35    # tiles — layers land within this of the anchor
POOL_STYLES = ("pool", "drops")

# Float slop for comparing positions the engine reports back as JSON
# numbers (they round-trip through a Lua double and a text encoding).
EPS = 1e-3


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
    # unit_resources' stamina drain would otherwise exhaust-collapse a
    # unit sustaining a full-speed 15-tile route long before it arrives
    # — movement_probe.py's own documented gotcha for its "move mode"
    # ("the auto-loaded resource tick would otherwise exhaust-collapse a
    # unit..."). This probe tests bleeding-trail emission, not stamina;
    # neutralise it the same way.
    send(port,
         "pcall(function() require('scripts.unit_resources').update = function() end end); "
         "return 'resources-off'")


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
    so no per-unit AI state is ever created to clear.

    Pins constitution/body_mass so bleed rate and blood capacity are
    deterministic: acolyte spawns roll RANDOM stats, and an unpinned low
    constitution + low body_mass roll can bleed a unit out from a
    moderate wound (sev 0.6) in as little as ~3 real seconds — well
    before the route completes — making mark counts flaky across runs
    for reasons unrelated to the trail emitter itself.
    """
    uid = spawn_acolyte(PORT, x, y, clear_water=False)
    send(PORT, f"unit.setStat({uid}, 'constitution', 2.0); return 'ok'")
    send(PORT, f"unit.setStat({uid}, 'body_mass', 70.0); return 'ok'")
    return uid


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


def route_marks(uid: int, exclude_ids: set) -> list:
    """Trail marks laid along a travelled route, EXCLUDING the pool
    cluster that #883 grows the moment the unit stops at the far end
    (the route bounds below are a statement about trail spacing, not
    about how long the probe happened to take to notice the arrival).
    The cluster is located by the anchor the engine itself reports, so
    nothing here hardcodes a coordinate."""
    anchor = cluster_anchor(uid)
    ds = trail_decals(exclude_ids)
    if anchor is None:
        return ds
    return [d for d in ds
            if dist(d["x"], d["y"], anchor[0], anchor[1]) > POOL_JITTER_RADIUS + EPS]


def trail_state(uid: int):
    return send_json(PORT, f"return blood.getTrailState({uid})")


def textures() -> dict:
    """id -> descriptor. `blood.listDecals` deliberately does NOT expose
    style/footprint (they belong to the texture, not the mark), so the
    style-family and footprint assertions have to join through here."""
    return {t["id"]: t for t in (send_json(PORT, "return blood.listTextures()") or [])}


def cluster_anchor(uid: int):
    """The pool cluster's own anchor as the engine reports it — never a
    coordinate this probe guessed. None until one has been anchored."""
    ts = trail_state(uid)
    if not ts or "clusterX" not in ts:
        return None
    return (float(ts["clusterX"]), float(ts["clusterY"]))


def cluster_layers(uid: int) -> int:
    ts = trail_state(uid)
    return int(ts["clusterLayers"]) if ts else 0


def dist(ax: float, ay: float, bx: float, by: float) -> float:
    return ((ax - bx) ** 2 + (ay - by) ** 2) ** 0.5


def pool_marks(anchor, exclude_ids: set, uid: int | None = None) -> list:
    """Marks belonging to the cluster at `anchor`: within the documented
    jitter radius of it, minus the one-shot impact decal, and (when uid
    is given) grouped by `sourceUnit` so an adjacent bleeder's cluster
    can never be counted into this one."""
    out = []
    for d in decals():
        if d["id"] in exclude_ids:
            continue
        if uid is not None and d.get("sourceUnit") != uid:
            continue
        if dist(d["x"], d["y"], anchor[0], anchor[1]) <= POOL_JITTER_RADIUS + EPS:
            out.append(d)
    return out


STABLE_DECAL_FIELDS = ("texture", "page", "x", "y", "surfaceZ", "offsetX",
                       "offsetY", "rotation", "scale", "createdAt",
                       "woundKind", "severity", "sourceUnit", "opacity")


def stable_snapshot(ds: list) -> dict:
    """id -> the fields a decal record NEVER recomputes at query time.
    `age`/`wetness`/`dryness` are deliberately excluded: they are derived
    from the caller's current game time (Blood.Types.wetnessAt), so they
    move on every call without anything being mutated."""
    return {d["id"]: tuple(d.get(k) for k in STABLE_DECAL_FIELDS) for d in ds}


def set_time_scale(scale: float, timeout: float = 5.0) -> None:
    """`world.setTimeScale(pageId, scale)` takes BOTH arguments and is
    queued to the world thread — a one-argument call is a silent no-op,
    and even a correct call is not effective until the thread applies
    it. Set, then wait for `world.getTimeScale` to actually report it."""
    send(PORT, f"world.setTimeScale('{PAGE}', {scale}); return 'ok'",
         expect_result=False)

    def applied() -> bool:
        got = send_json(PORT, f"return world.getTimeScale('{PAGE}')")
        try:
            return abs(float(got) - scale) < 1e-6
        except (TypeError, ValueError):
            return False

    if poll_until(timeout, applied, interval=0.2) is None:
        print(f"FAIL (setup): world.setTimeScale('{PAGE}', {scale}) never took "
              f"effect (world.getTimeScale reports "
              f"{send_json(PORT, f'return world.getTimeScale({PAGE!r})')!r})")
        sys.exit(2)


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


def grid_x(uid: int):
    info = send_json(PORT, f"return unit.getInfo({uid})")
    return info.get("gridX") if isinstance(info, dict) else None


def wait_arrival(uid: int, target_x: float, timeout: float = 90.0,
                  epsilon: float = 0.5) -> bool:
    """Poll until the unit's gridX is within epsilon of target_x (arrived
    and stopped) — the full-route acceptance criterion (mark count/
    distribution bounded for the WHOLE route) only means something once
    the unit has actually finished travelling it; stopping early at the
    first couple of marks would leave most of the documented upper bound
    unexercised."""
    def arrived() -> bool:
        x = grid_x(uid)
        return x is not None and abs(x - target_x) < epsilon
    return poll_until(timeout, arrived, interval=1.0) is not None


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
        # A LOW severity: even with constitution/body_mass pinned above,
        # a moderate-or-higher slash on this unit's torso empirically
        # exsanguinates in well under 15s (its bpBleedFactor is steep) —
        # this case needs the unit ALIVE for the whole route, not testing
        # death (that's case 4/e, on its own explicit unit.kill()). Since
        # this case now waits for actual arrival (round-5 review) against
        # a generous worst-case-scheduling ceiling (up to 90 REAL
        # seconds — blood drain runs on wall-clock time via the combat
        # thread regardless of how slowly movement itself progresses),
        # severity must stay safe across that whole window, not just a
        # nominal ~15 sim-second traversal.
        reset_blood()
        uid = spawn_fresh(10, 10)
        injure(uid, "slash", 0.1)   # untreated (bandage defaults to 1.0)
        impact_ids = impact_decal_ids()   # slash always creates one impact decal
        move_to(uid, 10 + route, 10)
        # Wait for the unit to actually FINISH the route (not just "at
        # least 2 marks") — the documented bounds are a route-LENGTH
        # acceptance criterion, so evaluating them the moment a couple
        # of marks appear would leave most of the upper bound and the
        # tail of the route unexercised (round-5 review). A generous
        # ceiling matters more than a short one — real-time engine
        # scheduling can lag well behind wall-clock under system load.
        if not wait_arrival(uid, 10 + route, timeout=90.0):
            print(f"FAIL (setup): unit never completed the {route}-tile route "
                  f"within 90s (stuck at gridX={grid_x(uid)}) — engine too slow "
                  f"under current load, or a real regression")
            return 2
        ds = route_marks(uid, impact_ids)
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
        injure(uid, "slash", 0.1)
        impact_ids = impact_decal_ids()
        set_time_scale(5.0)
        move_to(uid, 10 + route, 10)
        # Same "wait for the whole route" reasoning as case 1(a) above —
        # otherwise this only ever proves a partial-route mark count is
        # in bounds, which world.setTimeScale couldn't desync anyway.
        arrived = wait_arrival(uid, 10 + route, timeout=90.0)
        ds = route_marks(uid, impact_ids)
        set_time_scale(1.0)
        if not arrived:
            print(f"FAIL (setup): unit never completed the {route}-tile route "
                  f"within 90s at world.setTimeScale(5.0) (stuck at "
                  f"gridX={grid_x(uid)})")
            return 2
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
        injure(uid, "slash", 0.2)   # slash always creates one impact decal
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

        # ===================================================================
        # #883: stationary and collapsed-unit pooling
        # ===================================================================

        # --- 6(a)+(g). a stationary bleeder grows a clustered, additive pool
        # A MODERATE slash: heavy enough that the volume floor never gates
        # (so the layer cadence, not the bleed rate, sets the pace) and
        # that the wound keeps bleeding past the layer bound, light enough
        # that the unit survives the whole dwell.
        reset_blood()
        uid = spawn_fresh(30, 30)
        injure(uid, "slash", 0.2)
        impact_ids = impact_decal_ids()

        if poll_until(20.0, lambda: cluster_layers(uid) >= 2, interval=0.5) is None:
            print(f"FAIL: a stationary bleeding unit never grew a pool "
                  f"(getTrailState={trail_state(uid)!r})")
            return 1
        anchor = cluster_anchor(uid)
        if anchor is None:
            print("FAIL: getTrailState reports no cluster anchor while layering")
            return 1
        # The anchor is where the unit is standing, not somewhere else.
        ux, uy = 30.0, 30.0
        if dist(anchor[0], anchor[1], ux, uy) > POOL_CLUSTER_RADIUS + EPS:
            print(f"FAIL: cluster anchored {dist(anchor[0], anchor[1], ux, uy):.3f} "
                  f"tiles from the unit (> {POOL_CLUSTER_RADIUS})")
            return 1
        early_layers = cluster_layers(uid)
        early_marks = pool_marks(anchor, impact_ids, uid)
        before = stable_snapshot(decals())

        # Count grows with real elapsed game time.
        target = early_layers + 2
        if poll_until(20.0, lambda: cluster_layers(uid) >= target,
                       interval=0.5) is None:
            print(f"FAIL: pool stopped growing at {cluster_layers(uid)} layers "
                  f"well before the {POOL_MAX_LAYERS}-layer bound")
            return 1
        later_marks = pool_marks(anchor, impact_ids, uid)
        if len(later_marks) <= len(early_marks):
            print(f"FAIL: pool mark count did not grow "
                  f"({len(early_marks)} -> {len(later_marks)})")
            return 1

        # Every mark is the unit's own, inside the documented radius, and
        # drawn from the pool/drops family at a small footprint.
        txs = textures()
        for d in later_marks:
            if d.get("sourceUnit") != uid:
                print(f"FAIL: pool mark {d['id']} has sourceUnit="
                      f"{d.get('sourceUnit')!r}, expected {uid}")
                return 1
            tex = txs.get(d["texture"])
            if tex is None:
                print(f"FAIL: pool mark {d['id']} references unknown texture "
                      f"{d['texture']}")
                return 1
            if tex["style"] not in POOL_STYLES:
                print(f"FAIL: pool mark {d['id']} style={tex['style']!r}, "
                      f"expected one of {POOL_STYLES}")
                return 1
            if tex["footprint"] != "small":
                print(f"FAIL: pool mark {d['id']} footprint="
                      f"{tex['footprint']!r}, expected 'small' (a pool grows "
                      f"by layering, never by one bigger mark)")
                return 1

        # (g) Growth is strictly additive: every id that already existed
        # still reports identical stable fields, and only NEW ids appeared.
        after = stable_snapshot(decals())
        for did, fields in before.items():
            if did not in after:
                print(f"FAIL: decal {did} vanished while the pool grew")
                return 1
            if after[did] != fields:
                print(f"FAIL: decal {did} was MUTATED by pool growth: "
                      f"{fields!r} -> {after[did]!r}")
                return 1
        if len(after) <= len(before):
            print(f"FAIL: pool growth added no new decal ids "
                  f"({len(before)} -> {len(after)})")
            return 1
        print(f"PASS: stationary bleeder grew a clustered pool "
              f"({len(early_marks)} -> {len(later_marks)} marks within "
              f"{POOL_JITTER_RADIUS} of anchor {anchor}); growth strictly "
              f"additive ({len(before)} -> {len(after)} decals, none mutated)")

        # --- 7(b). the bound: layering stops at exactly POOL_MAX_LAYERS ----
        # Real elapsed seconds only — world.setTimeScale advances the world
        # calendar, NOT the unpaused gameTimeRef this cadence runs on, so it
        # cannot fast-forward the dwell.
        if poll_until(60.0, lambda: (trail_state(uid) or {}).get("clusterAtBound"),
                       interval=0.5) is None:
            print(f"FAIL: pool never reached the {POOL_MAX_LAYERS}-layer bound "
                  f"within 60s (getTrailState={trail_state(uid)!r})")
            return 1
        at_bound = trail_state(uid) or {}
        if at_bound.get("clusterLayers") != POOL_MAX_LAYERS:
            print(f"FAIL: clusterAtBound is set at "
                  f"{at_bound.get('clusterLayers')} layers, expected exactly "
                  f"{POOL_MAX_LAYERS}")
            return 1
        bound_n = len(pool_marks(anchor, impact_ids, uid))

        # Hold at the bound: the count must stay flat, and we must observe
        # it staying flat while the wound is DEMONSTRABLY still bleeding
        # (otherwise "no more marks" would be vacuous — a clotted wound
        # emits nothing either way).
        bleeding_samples = 0
        t0 = time.time()
        while time.time() - t0 < 8.0:
            time.sleep(0.5)
            n = len(pool_marks(anchor, impact_ids, uid))
            if n != bound_n:
                print(f"FAIL: pool kept growing past the bound "
                      f"({bound_n} -> {n} marks, layers="
                      f"{cluster_layers(uid)})")
                return 1
            if bleed_rate_of(uid) > 0:
                bleeding_samples += 1
        if bleeding_samples < 4:
            print(f"FAIL (setup): only {bleeding_samples} samples had a live "
                  f"external bleed after the bound — the wound clotted too "
                  f"fast to prove the bound actually holds it back")
            return 2
        print(f"PASS: pooling saturates at exactly {POOL_MAX_LAYERS} layers "
              f"({bound_n} marks) and adds nothing further across "
              f"{bleeding_samples} samples with a live bleed")
        destroy(uid)

        # --- 8(c). clot to zero external bleed stops layering pre-bound ---
        # A LOW severity: it self-clots well before the layer bound, which
        # is exactly the claim — bandaging/clotting stops layering with
        # nothing left to clean up.
        reset_blood()
        uid = spawn_fresh(30, 30)
        injure(uid, "slash", 0.06)
        impact_ids = impact_decal_ids()
        if poll_until(25.0, lambda: cluster_layers(uid) >= 1, interval=0.5) is None:
            print("FAIL: a fresh low-severity bleed never pooled at all")
            return 1
        peak = [cluster_layers(uid)]

        def clot_cleared() -> bool:
            ts = trail_state(uid)
            if ts is None:
                return True
            peak[0] = max(peak[0], int(ts["clusterLayers"]))
            return False

        if poll_until(90.0, clot_cleared, interval=0.5) is None:
            print(f"FAIL (setup): the wound never self-clotted within 90s "
                  f"(getTrailState={trail_state(uid)!r})")
            return 2
        # Strictly between "pooled at all" and "hit the bound": a peak of
        # 0 would make the no-further-marks check below vacuous (the
        # cluster would have cleared before it ever layered), and a peak
        # AT the bound would prove the bound stopped it, not the clot.
        if not (1 <= peak[0] < POOL_MAX_LAYERS):
            print(f"FAIL: clot case peaked at {peak[0]} layers, outside "
                  f"[1, {POOL_MAX_LAYERS}) — the clot, not the bound, has to "
                  f"be what stopped the layering here")
            return 1
        cleared_n = len(trail_decals(impact_ids))
        time.sleep(6.0)
        if len(trail_decals(impact_ids)) != cleared_n:
            print(f"FAIL: marks kept appearing after the bleed clotted to zero "
                  f"({cleared_n} -> {len(trail_decals(impact_ids))})")
            return 1
        print(f"PASS: clot stopped layering at {peak[0]} layers, short of the "
              f"{POOL_MAX_LAYERS}-layer bound; getTrailState cleared and no "
              f"further marks appeared")
        destroy(uid)

        # --- 9(d). walk-then-stop: trail along the route, pool at the stop --
        reset_blood()
        walk = 8.0
        uid = spawn_fresh(10, 40)
        injure(uid, "slash", 0.15)
        impact_ids = impact_decal_ids()
        move_to(uid, 10 + walk, 40)
        if not wait_arrival(uid, 10 + walk, timeout=60.0):
            print(f"FAIL (setup): unit never completed the {walk}-tile walk "
                  f"(stuck at gridX={grid_x(uid)})")
            return 2
        stop_anchor = cluster_anchor(uid)
        if stop_anchor is None:
            print("FAIL: no cluster anchored after the unit stopped")
            return 1
        route_ds = route_marks(uid, impact_ids)
        route_xs = [d["x"] for d in route_ds]
        if len(route_ds) < 2 or max(route_xs) - min(route_xs) < walk / 2:
            print(f"FAIL: the walked leg left no real trail "
                  f"({len(route_ds)} marks, x span "
                  f"{(max(route_xs) - min(route_xs)) if route_xs else 0:.2f})")
            return 1
        before_stop = len(pool_marks(stop_anchor, impact_ids, uid))
        if poll_until(20.0,
                       lambda: len(pool_marks(stop_anchor, impact_ids, uid))
                               > before_stop + 1,
                       interval=0.5) is None:
            print(f"FAIL: no pool grew at the stop point after the walk "
                  f"(getTrailState={trail_state(uid)!r})")
            return 1
        stop_n = len(pool_marks(stop_anchor, impact_ids, uid))
        print(f"PASS: walk-then-stop used ONE accumulator — {len(route_ds)} "
              f"trail marks spanning "
              f"{max(route_xs) - min(route_xs):.2f} tiles, then a "
              f"{stop_n}-mark cluster at the stop point {stop_anchor}")
        destroy(uid)

        # --- 10(e). a collapsed unit pools; death drops the cluster --------
        reset_blood()
        uid = spawn_fresh(30, 50)
        send(PORT, f"unit.collapse({uid}); return 'ok'", expect_result=False)
        if poll_until(10.0,
                       lambda: send(PORT, f"return unit.getPose({uid})") == "collapsed",
                       interval=0.2) is None:
            print(f"FAIL (setup): unit never collapsed "
                  f"(pose={send(PORT, f'return unit.getPose({uid})')!r})")
            return 2
        injure(uid, "slash", 0.2)
        impact_ids = impact_decal_ids()
        if poll_until(25.0, lambda: cluster_layers(uid) >= 3, interval=0.5) is None:
            print(f"FAIL: a COLLAPSED bleeding unit did not pool "
                  f"(getTrailState={trail_state(uid)!r})")
            return 1
        pose_now = send(PORT, f"return unit.getPose({uid})")
        if pose_now != "collapsed":
            print(f"FAIL (setup): unit left the collapsed pose mid-case "
                  f"({pose_now!r}) — this case must test pooling while down")
            return 2
        collapsed_anchor = cluster_anchor(uid)
        collapsed_n = len(pool_marks(collapsed_anchor, impact_ids, uid))

        send(PORT, f"unit.kill({uid}); return 'ok'", expect_result=False)
        # unit.kill only QUEUES the command; handleUnitKillCommand clears
        # the accumulator synchronously with the kill, so the "stopped
        # growing" baseline has to be taken AFTER that has happened —
        # a layer legitimately landing between the pre-kill read and the
        # command being processed is the feature working, not a leak.
        if poll_until(6.0, lambda: trail_state(uid) is None, interval=0.2) is None:
            print(f"FAIL: cluster state survived death: {trail_state(uid)!r}")
            return 1
        dead_n = len(pool_marks(collapsed_anchor, impact_ids, uid))
        sample = decals()[-1]
        time.sleep(3.0 * POOL_MIN_CADENCE)
        after_death = len(pool_marks(collapsed_anchor, impact_ids, uid))
        if after_death != dead_n:
            print(f"FAIL: a dead unit's pool kept growing "
                  f"({dead_n} -> {after_death})")
            return 1
        # The marks themselves persist and keep aging.
        aged = [d for d in decals() if d["id"] == sample["id"]]
        if not aged:
            print(f"FAIL: pool mark {sample['id']} disappeared when its source "
                  f"unit died — marks must outlive their source")
            return 1
        if not (aged[0]["age"] > sample["age"]):
            print(f"FAIL: pool mark {sample['id']} stopped aging after its "
                  f"source died ({sample['age']} -> {aged[0]['age']})")
            return 1
        print(f"PASS: a collapsed unit pooled ({collapsed_n} marks, {dead_n} by "
              f"the time the kill landed); death dropped the cluster state with "
              f"no leak, and its marks persist and keep aging "
              f"({sample['age']:.2f}s -> {aged[0]['age']:.2f}s)")
        destroy(uid)

        # --- 11(f). two adjacent bleeders, two independent bounded clusters -
        reset_blood()
        uid_a = spawn_fresh(20, 60)
        uid_b = spawn_fresh(22, 60)   # > POOL_CLUSTER_RADIUS apart
        injure(uid_a, "slash", 0.2)
        injure(uid_b, "slash", 0.2)
        impact_ids = impact_decal_ids()

        def both_at_bound() -> bool:
            return all((trail_state(u) or {}).get("clusterAtBound")
                       for u in (uid_a, uid_b))

        if poll_until(70.0, both_at_bound, interval=0.5) is None:
            print(f"FAIL: two adjacent bleeders did not both reach the bound "
                  f"(A={trail_state(uid_a)!r}, B={trail_state(uid_b)!r})")
            return 1
        anchor_a = cluster_anchor(uid_a)
        anchor_b = cluster_anchor(uid_b)
        if anchor_a is None or anchor_b is None:
            print("FAIL: one of the two bleeders reports no cluster anchor")
            return 1
        if dist(*anchor_a, *anchor_b) <= POOL_JITTER_RADIUS:
            print(f"FAIL: the two clusters share an anchor "
                  f"({anchor_a} vs {anchor_b}) — they must be independent")
            return 1
        marks_a = pool_marks(anchor_a, impact_ids, uid_a)
        marks_b = pool_marks(anchor_b, impact_ids, uid_b)
        for label, ms in (("A", marks_a), ("B", marks_b)):
            if not (0 < len(ms) <= POOL_MAX_LAYERS):
                print(f"FAIL: bleeder {label}'s cluster holds {len(ms)} marks, "
                      f"outside (0, {POOL_MAX_LAYERS}]")
                return 1
        if {d["id"] for d in marks_a} & {d["id"] for d in marks_b}:
            print("FAIL: the two clusters share marks — grouping by sourceUnit "
                  "and anchor should make them disjoint")
            return 1
        print(f"PASS: two adjacent bleeders grew two independent bounded "
              f"clusters (A: {len(marks_a)} marks at {anchor_a}, "
              f"B: {len(marks_b)} marks at {anchor_b})")
        destroy(uid_a)
        destroy(uid_b)

        # --- 12. pool density ignores world.setTimeScale (requirement 7) ---
        # Same wound, same REAL dwell, two very different world calendars:
        # the pool cadence clock is the unpaused gameTimeRef, so the layer
        # counts must agree.
        dwell_secs = 9.0
        counts = {}
        for scale in (1.0, 5.0):
            set_time_scale(scale)
            reset_blood()
            uid = spawn_fresh(40, 60)
            injure(uid, "slash", 0.2)
            impact_ids = impact_decal_ids()
            if poll_until(15.0, lambda: cluster_anchor(uid) is not None,
                           interval=0.2) is None:
                print(f"FAIL (setup): no cluster anchored at "
                      f"world.setTimeScale({scale})")
                return 2
            start_layers = cluster_layers(uid)
            time.sleep(dwell_secs)
            counts[scale] = cluster_layers(uid) - start_layers
            destroy(uid)
        set_time_scale(1.0)
        # Expected layers in a `dwell_secs` window, +/- one for where the
        # window happened to fall relative to the cadence boundaries.
        expected = int(dwell_secs / POOL_MIN_CADENCE)
        for scale, n in counts.items():
            if not (expected - 1 <= n <= expected + 1):
                print(f"FAIL: {n} layers in {dwell_secs}s at "
                      f"world.setTimeScale({scale}), expected "
                      f"{expected}+/-1 — pool cadence is following the world "
                      f"calendar instead of the unpaused game clock")
                return 1
        print(f"PASS: pool density is time-scale invariant "
              f"({counts[1.0]} layers at 1x vs {counts[5.0]} at 5x over "
              f"{dwell_secs}s real, expected {expected}+/-1)")

        print("\nPASS: all bleeding-trail and pooling checks held")
        return 0
    finally:
        quit_engine(PORT, proc)


if __name__ == "__main__":
    sys.exit(main())
