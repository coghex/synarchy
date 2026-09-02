#!/usr/bin/env python3
"""[prepare] — water, provisioning, capacity, and the shared muster
(#2092).

What a party does before it leaves, and the three levelling steps that
keep supplies the ONLY difference between the two travellers: the
control is stripped of food and nothing else, both are shed to inside
their carrying capacity with room for the loot, and both are gathered at
one staging tile and held there by the pause.

MUSTERING lives here, not with the travel owner, because requirement 4
assigns "party mustering" to this owner — and because it is departure
CALIBRATION rather than travel: it establishes the shared origin the
control experiment is scored against. `muster_travellers` and
`seed_departure_deficit` are therefore this module's documented public
interface, called by `travel.run` in the same paused window it hands
back. Nothing else here is called from outside.

Every mutation in this module is fixture setup, clearly separated from
the loop under test: retiring the control's rations, shedding tools, and
seeding one shared hunger deficit while PAUSED. No stage is satisfied by
one.
"""
from __future__ import annotations

import time

from probelib import poll_until, send

from .constants import (DEPART_STOMACH_FRAC, EXPECTED_COMPLETED,
                        MAX_START_SEPARATION, MAX_START_SPREAD, RATIONS_DEF,
                        STAGING_RADIUS, SUB_FOOD, SUB_WATER)
from .harness import Checks, ExpeditionState, StageAbort
from .readers import (_as_float, bearing_gap, carried, current_action, dist,
                      paired_positions, progress, stat_fraction, unit_pos)


# --------------------------------------------------------------------------
# Water and provisioning
# --------------------------------------------------------------------------
def secure_water(chk: Checks, port: int, scout: int, shore) -> bool:
    """One acolyte walks to the water and finds it with its OWN FOV scan.

    Nothing writes knownWaterSources here: scripts/unit_ai_water.lua's
    scan runs every tick from the unit's real `unit.getVisibleTiles`, so
    the memory (and therefore the tutorial's water objective) is earned
    by the walk."""
    sx, sy = shore
    before = _as_float(send(port, f"local s=require('scripts.unit_ai').getState({scout}); "
                                  f"return s and #(s.knownWaterSources or {{}}) or -1"))
    chk.ok((before or 0) == 0,
           f"the scout starts with no remembered water source (count {before})")
    send(port, f"require('scripts.unit_ai').commandMove({scout},{sx},{sy}); "
               f"return 'ok'")
    found = poll_until(240.0, lambda: (_as_float(send(
        port, f"local s=require('scripts.unit_ai').getState({scout}); "
              f"return s and #(s.knownWaterSources or {{}}) or 0")) or 0) > 0,
        interval=1.0)
    return chk.ok(bool(found),
                  f"the scout reaches the water and registers it through its own "
                  f"FOV scan (at {unit_pos(port, scout)}, target shore {shore}, "
                  f"action {current_action(port, scout)})")


def provision(chk: Checks, port: int, mule: int, traveller: int) -> bool:
    """Top the traveller up off the technomule — the colony's supply
    point — through `unit.transferItemToUnit`, the atomic engine path
    the fetch AI itself uses. Bounded by the receiver's real carrying
    capacity, exactly as the Lua callers gate it."""
    before = carried(port, traveller)
    moved = _as_float(send(
        port, f"local n=0; for _=1,2 do "
              f"local w=unit.getCarryingWeight({traveller}) or 0; "
              f"local cap=unit.getStat({traveller},'carrying_capacity'); "
              f"if cap and w>=cap then break end; "
              f"if unit.transferItemToUnit({mule},{traveller},'{RATIONS_DEF}') "
              f"then n=n+1 else break end end; return n", timeout=30.0)) or 0
    after = carried(port, traveller)
    chk.ok(moved > 0 and after[1] > before[1],
           f"the traveller is provisioned off the technomule through the normal "
           f"inventory-transfer surface ({int(moved)}x {RATIONS_DEF} moved; "
           f"rations {before[1]} -> {after[1]})")
    return chk.ok(after[0] >= 2.0 and after[1] >= 1,
                  f"the prepared traveller leaves with real supplies "
                  f"({after[0]:.2f} L of water, {after[1]} rations)")


#: Personal tools an acolyte spawns with, in the order a departing
#: traveller sheds them. Deliberately the reverse of what the expedition
#: needs, and the same order `data/units/acolyte.yaml`'s own
#: `drop_priority` uses for its spawn-time shed.
SHEDDABLE = ("pick_steel", "axe_steel", "shovel_steel")


# --------------------------------------------------------------------------
# Capacity levelling, and the departure calibration travel consumes
# --------------------------------------------------------------------------
def shed_to_capacity(port: int, uid: int, headroom: float = 0.0) -> int:
    """Shed personal tools until the traveller is inside its carrying
    capacity WITH `headroom` kg to spare. Returns how many items were
    shed.

    The headroom is the extraction target's own weight, and it is not
    optional: `unitAi.commandPickup` refuses at command time when
    `getCarryingWeight + the instance's weight` exceeds capacity (#920),
    so a carrier that merely fits itself can still be turned away at the
    ruin. Observed: a traveller that departed at 91% of capacity had its
    retrieval order refused outright on arrival — the loop's own
    capacity contract working exactly as #920 specifies, against a
    fixture that had not left room for the loot.

    This is what a player does before an expedition, and skipping it is
    a real flake rather than a nicety: `docs/expedition_survival_calibration.md`
    observation E1 recorded a small acolyte walking a whole route at
    121% of capacity, where the encumbrance penalty roughly halved its
    speed and pinned its stamina at ~2/8.5. A traveller that slow makes
    no new closest approach inside `pickup_timeout`, so the stall timer
    correctly retires its order and it reverts to `wander` — observed
    here as an outbound leg that covered 15 tiles in 420 s and never
    reached the ruin. Applied to BOTH travellers, so encumbrance is not
    a second variable alongside supplies."""
    shed = 0
    for defname in SHEDDABLE:
        carried_kg = _as_float(send(port, f"return unit.getCarryingWeight({uid})"))
        cap = _as_float(send(port, f"return unit.getStat({uid},"
                                   f"'carrying_capacity')"))
        if carried_kg is None or not cap or cap <= 0:
            break
        if carried_kg + headroom <= cap:
            break
        # `unit.removeItem` reports success truthily; compare against the
        # falsy spellings rather than assuming a boolean.
        got = send(port, f"return tostring(unit.removeItem({uid},'{defname}'))"
                   ).strip().strip('"')
        if got not in ("nil", "false", ""):
            shed += 1
    return shed


def strip_rations(port: int, uid: int) -> None:
    """The control's ONLY difference: every ration removed. Its canteen
    is deliberately left FULL.

    Emptying the canteen too would look like a stronger control and is
    actually a second variable. A dry canteen puts `refill_canteen`'s
    quadratic urgency ramp at its 7.5 peak, above `follow_command`'s
    7.0, so an unwatered traveller correctly abandons its orders and
    walks to the nearest known water — and the scout's `notify_allies`
    radio broadcast has already told the whole colony where that is.
    Observed exactly that: a control that left the muster and was found
    10.7 tiles off-route beside the lake, 43.6 tiles from the ruin.
    That is the game working, but it is a difference in BEHAVIOUR rather
    than in supplies, and the gated metric is food.

    Leaving both canteens full is therefore the tighter experiment, not
    the softer one: the single difference between the two travellers is
    the thing being measured."""
    send(port, f"while unit.removeItem({uid},'{RATIONS_DEF}') do end; "
               f"return 'ok'", timeout=20.0)


def origin_ok(pos, uids, staging, ruin_xy):
    """(ok, separation, distance-spread, bearing-gap) for a candidate
    departure pair. The shared origin is a PLACE, so separation is the
    binding term: a radial distance band is satisfied anywhere on a
    circle, and two travellers on opposite bearings score a spread of
    0.0 while standing 64 tiles apart."""
    a, b = uids
    if not (pos.get(a) and pos.get(b)):
        return False, -1.0, -1.0, -1.0
    sep = dist(pos[a], pos[b])
    spread = abs(dist(pos[a], ruin_xy) - dist(pos[b], ruin_xy))
    gap = bearing_gap(pos[a], pos[b], ruin_xy)
    ok = (sep <= MAX_START_SEPARATION and spread <= MAX_START_SPREAD
          and all(dist(pos[u], staging) <= STAGING_RADIUS for u in uids))
    return ok, sep, spread, gap


def muster_travellers(port: int, uids, staging, ruin_xy, seconds: float = 420.0):
    """Gather both travellers at ONE staging position and HOLD them there
    with the pause, so the paired legs share an origin — and therefore a
    route, not merely a length.

    Returns (positions, sep, spread, gap) with the session left PAUSED on
    success, or (None, ...) with it left running.

    The hold is `engine.setPaused`, and it has to be. `unit.setFrozen`
    looks like the obvious tool and is the wrong one: `uiFrozen` only
    makes the unit thread's `publishToRender` skip the sim-derived
    update (`src/Unit/Thread.hs`), so the AI and the simulation keep
    moving the unit while `unit.getInfo` keeps reporting the position it
    had when the flag went up. Mustering on those coordinates reads
    stale numbers and then releases two travellers from wherever the sim
    has actually carried them. Pausing stops the simulation itself,
    which is what "hold still" has to mean here.

    A completed PLAYER move order does hold position since #1216
    (SURV-4), which retires observation E3 and makes the muster far more
    likely to converge — but it is not a substitute for the pause, and
    the driven shape below stays. The hold sits at `follow_command`'s own
    utility, so every interrupt that outranked the order still carries a
    held unit off its anchor, and the muster's own history is what a
    coincidence hunt costs when that happens: observed twice, once with
    both units 40+ tiles out and 3.4 tiles apart after a 300 s wait, and
    once with the muster expiring on a pair 10.0 tiles apart.
    Hence the shape below: re-order anyone who has stopped following and
    drifted outside the radius (convergence is driven, not awaited),
    poll for a sample satisfying the origin contract, pause the instant
    it appears, then RE-READ and re-check on the now-stable positions. A
    sample that goes stale in the round trip between the two just
    unpauses and keeps looking."""
    for uid in uids:
        send(port, f"require('scripts.unit_ai').commandMove({uid},"
                   f"{staging[0]},{staging[1]}); return 'ok'")
    deadline = time.time() + seconds
    while time.time() < deadline:
        live = paired_positions(port, uids[0], uids[1])
        # Convergence has to be ACTIVE, not awaited. An arrived unit
        # holds its destination since #1216, but a survival interrupt
        # still carries it off and its return is its own to schedule, so
        # polling for a moment when both happen to be at the tile is a
        # coincidence hunt that can time out — observed: a muster that
        # expired with the two 10.0 tiles apart, one of them 9.4 tiles
        # further from the ruin than the other. Re-order anyone who has
        # stopped following and is outside the radius; that is also
        # exactly what a player does when a colonist wanders off.
        for uid in uids:
            p = live.get(uid)
            if (p is None or dist(p, staging) > STAGING_RADIUS) \
                    and current_action(port, uid) != "follow_command":
                send(port, f"require('scripts.unit_ai').commandMove({uid},"
                           f"{staging[0]},{staging[1]}); return 'ok'")
        if origin_ok(live, uids, staging, ruin_xy)[0]:
            send(port, "engine.setPaused(true); return 'ok'")
            # Re-read while the simulation is stopped: these are the
            # positions the measured leg actually starts from.
            held = {u: unit_pos(port, u) for u in uids}
            ok, sep, spread, gap = origin_ok(held, uids, staging, ruin_xy)
            if ok:
                return held, sep, spread, gap
            send(port, "engine.setPaused(false); return 'ok'")
        time.sleep(0.5)
    live = {u: unit_pos(port, u) for u in uids}
    _ok, sep, spread, gap = origin_ok(live, uids, staging, ruin_xy)
    return None, sep, spread, gap


def seed_departure_deficit(port: int, uids) -> bool:
    """The shared, symmetric fixture: both travellers set out equally
    hungry, as a fraction of their OWN max_hunger (body mass varies
    ~3.6x across acolytes, so an absolute stomach value would not be
    comparable between them).

    Applied while PAUSED. `scripts/unit_ai.lua` is pause-gated, so this
    window changes the one gameplay fact under test without the AI
    reacting to it first — otherwise the provisioned traveller eats
    within a second and the two would no longer be departing from the
    same state.

    Returns True once both units read back the seeded value."""
    for uid in uids:
        maxn = _as_float(send(port, f"return unit.getStat({uid},'max_hunger')"))
        if not maxn:
            return False
        send(port, f"unit.setStat({uid},'hunger',"
                   f"{maxn * DEPART_STOMACH_FRAC:.4f}); return 'ok'")

    def settled():
        for uid in uids:
            frac = stat_fraction(port, uid, "hunger", "max_hunger")
            if frac is None or abs(frac - DEPART_STOMACH_FRAC) > 0.02:
                return None
        return True

    return bool(poll_until(20.0, settled, interval=0.5))


# --------------------------------------------------------------------------
# The stage
# --------------------------------------------------------------------------
def run(chk: Checks, st: ExpeditionState) -> None:
    """Secure water, provision the party, read the objective state."""
    port = st.port
    prepared, control = st.prepared, st.control
    target = st.target

    chk.enter("prepare", "secure water, provision the party, "
                         "read the objective state")
    if not secure_water(chk, port, st.scout, st.site["shore"]):
        raise StageAbort("the scout did not secure a water source")
    if not provision(chk, port, st.mule, prepared):
        raise StageAbort("the traveller was not provisioned")
    strip_rations(port, control)
    c_water, c_rations = carried(port, control)
    p_water, p_rations = carried(port, prepared)
    chk.ok(c_rations == 0 and p_rations > 0
           and abs(c_water - p_water) < 0.01,
           f"the control party leaves with NO FOOD and the SAME water "
           f"as the prepared one — food is the single difference "
           f"between the two travellers (prepared {p_water:.2f} L / "
           f"{p_rations} rations, control {c_water:.2f} L / "
           f"{c_rations} rations)")
    # Headroom for the loot: commandPickup refuses at command
    # time when the instance would not fit (#920), so a carrier
    # that merely fits ITSELF is turned away at the ruin.
    headroom = float(target.get("weight") or 0.0)
    shed = {u: shed_to_capacity(port, u, headroom)
            for u in (prepared, control)}
    room = {}
    for u in (prepared, control):
        carried_kg = _as_float(send(
            port, f"return unit.getCarryingWeight({u})")) or 0.0
        cap = _as_float(send(
            port, f"return unit.getStat({u},'carrying_capacity')")) or 0.0
        room[u] = (carried_kg, cap)
    chk.ok(all(room[u][0] + headroom <= room[u][1]
               for u in (prepared, control)),
           f"both travellers set out INSIDE their carrying capacity with "
           f"room for the {target['defName']} ({headroom:.2f} kg) — an "
           f"over-encumbered acolyte walks at a fraction of comfort "
           f"speed and has its orders stall-timed-out, and one with no "
           f"headroom has its retrieval order refused outright (prepared "
           f"{room[prepared][0]:.1f}+{headroom:.2f} of "
           f"{room[prepared][1]:.1f} kg after shedding {shed[prepared]} "
           f"tool(s), control {room[control][0]:.1f}+{headroom:.2f} of "
           f"{room[control][1]:.1f} kg after {shed[control]})")

    completed, checked = poll_until(
        45.0, lambda: (lambda p: p if EXPECTED_COMPLETED <= p[0] else None)(
            progress(port)), interval=1.0) or progress(port)
    chk.ok(completed == EXPECTED_COMPLETED,
           f"the shipped first_session tree stands at exactly its expected "
           f"completed set {sorted(EXPECTED_COMPLETED)} (got "
           f"{sorted(completed)})")
    chk.ok({SUB_WATER, SUB_FOOD} <= checked,
           f"both live preparation subobjectives are checked while a "
           f"provisioned traveller is standing in the colony "
           f"({sorted(checked)})")
    st.fp["objectives"] = sorted(completed)
