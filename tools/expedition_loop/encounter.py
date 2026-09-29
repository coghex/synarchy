#!/usr/bin/env python3
"""[encounter] and [reward] — confront the occupied ruin, then clear it
(#2640).

The survival-control leg ends at a zero-occupant ruin by design, so it
never exercises the other half of the arc: #916's persistent hostile
occupants, #1230's reveal by sight, #917's compound clearance and
#2301's trip objectives. This owner carries the SAME session on from
there to a SECOND `ruin_small`, chosen by `setup` because its persisted
encounter roll is at least one, and proves the natural clearing order
end to end:

  encounter  a party of the colony's other acolytes gathers at the first
             ruin and walks on from it to the occupied one; it was
             `unknown` when the leg began and is discovered by sight
             exactly once; its assigned occupants acquire the party
             through their own sight/aggression path (the encounter
             activates; exactly one aggression notice per episode);
             while every occupant is
             alive the location is `active` and nowhere near cleared;
             the party kills every occupant under ordinary player attack
             orders; the encounter half latches, and the location is
             STILL not cleared because its guaranteed item lies where it
             spawned.
  reward     the player's pickup gesture moves that exact physical item
             into a living acolyte's pack; its taken latch is set; the
             location promotes to `cleared` exactly once with exactly
             one notice; the four trip objectives have latched through
             the real evaluator in authored order; the carrier heads
             home (its deposit is asserted by `deliver_home`, called
             from the `return` stage).

WHAT IS NEVER DONE HERE. No lifecycle is written
(`world.setLocationLifecycle` is never called), no encounter, occupant,
health or item state is mutated, nothing is staged, and no tutorial
latch is written. The only fixture mutation this session makes stays
where it was: `find_water` retired, foraging disabled, the seeded
departure deficit, and colony storage finished at setup. The one
instrument installed — `notices.install_latch_recorder` — observes the
evaluator's write boundary without changing what it writes.

WHO GOES (owner directive on #2640). The scout and the stay-at-home
colonists, each carrying the canteen and rations it spawned with —
prepared by the tutorial's own predicate. The prepared traveller has
already carried its loot home on the calibrated return leg: sent on
another ~180 tiles and a fight after its seeded hunger, it was observed
crawling at the muster and falling asleep, starving, on the way home.
The control traveller takes no part; it is left holding the first ruin
exactly as the travel stage left it, and the stage asserts it never
comes near the occupied one.
"""
from __future__ import annotations

import math
import time

from probelib import poll_until, send, send_json

from .constants import (ACOLYTE_DEF, DEATH_BLOW_SECONDS, ENCOUNTER_SECONDS,
                        FAR_POST_TILES, FIGHT_SECONDS, MUSTER_SECONDS,
                        OCCUPIED_RETURN_SECONDS, PAGE, PARTY_WATER_L,
                        RATIONS_DEF, RECON_SECONDS, TRIP_OBJECTIVES)
from .extract import bank_home, locate
from .harness import Checks, ExpeditionState, StageAbort, assert_real_travel
from .notices import EventLedger, latch_passes
from .readers import (_as_float, carried, current_action, dist, ground_items,
                      in_arrival_box, instance_by_id, inventory,
                      known_locations, load_region, pose, progress, roster,
                      significant_rows, unit_pos)


#: The failure meters (scripts/unit_resource_failure.lua) that kill by
#: the occupant's OWN physiology rather than by trauma, and the causes
#: their deaths and the resource deaths (scripts/unit_resource_alerts.lua
#: `deathCauseFor`) report.
PHYSIOLOGICAL_METERS = ("salt_imbalance", "hypothermia", "hyperthermia")
PHYSIOLOGICAL_CAUSES = {"electrolyte imbalance", "hypothermia", "heat stroke",
                        "dehydration", "starvation", "exhaustion"}

#: The widest the activation edge may be: game-seconds between the last
#: sample reading "not activated" and the first reading "activated". A
#: closed episode sends its guard home and it re-engages only from its
#: post, which takes longer than this.
ACTIVATION_EDGE_SECONDS = 1.0

#: Actions that mean a party member has stopped following its order and
#: is doing nothing in particular, so re-ordering it overrides nothing.
IDLE_ACTIONS = ("idle", "wander", "hold_position")

#: `Location.Types` lifecycle order; a promotion only ever moves right.
LIFECYCLE_ORDER = ["unknown", "hinted", "discovered", "active", "cleared",
                   "depleted"]


# --------------------------------------------------------------------------
# Readers private to this stage
# --------------------------------------------------------------------------
def occupants_of(inst) -> list:
    enc = (inst or {}).get("encounter") or {}
    return sorted((o for o in enc.get("occupants") or []),
                  key=lambda o: int(o.get("uid", 0)))


def living(port: int, uids) -> list:
    """Occupants still capable of anything at all. `death_only` is the
    clearance policy, so a collapsed or crawling nomad is alive here."""
    out = []
    for u in uids:
        exists = send(port, f"return tostring(unit.exists({u}))") == "true"
        if exists and pose(port, u) != "dead":
            out.append(u)
    return out


def encounter_state(port: int, occ_id: int):
    """The fields the activation edge is judged on, and the game time,
    in ONE console round trip."""
    raw = send(port,
               f"local i=world.getLocationInstance({occ_id},'{PAGE}'); "
               f"local e=i and i.encounter; if not e then return 'nil' end; "
               f"return tostring(i.lifecycle)..','..tostring(e.activated)..','"
               f"..tostring(e.episode_active)..','"
               f"..tostring(e.aggression_announced)..','"
               f"..tostring(engine.gameTime())").strip().strip('"')
    parts = raw.split(",")
    if len(parts) != 5:
        return None
    t = _as_float(parts[4])
    return {"lifecycle": parts[0], "activated": parts[1] == "true",
            "episode_active": parts[2] == "true",
            "aggression_announced": parts[3] == "true", "t": t or 0.0}


def death_physiology(port: int, uid: int) -> dict:
    """The state that says which kill path took `uid`, read at the moment
    it is first seen dead: its physiological failure meters and how many
    wounds its corpse carries."""
    stats = ",".join(f"'{k}'" for k in PHYSIOLOGICAL_METERS)
    got = send_json(
        port, f"local o={{}}; for _,k in ipairs({{{stats}}}) do "
              f"o[k]=unit.getStat({uid},k) end; "
              f"local w=unit.getWounds({uid}); "
              f"o.wounds=type(w)=='table' and #w or 0; return o")
    return got if isinstance(got, dict) else {}


def death_cause(ledger, uid: int):
    """The cause `uid`'s own death notice names ("X died of <cause>"),
    or None — which, for a combat kill, is the expected answer."""
    for r in ledger.matching(lambda r: r.get("category") == "survival_critical"
                             and r.get("uid") == uid):
        text = r.get("text") or ""
        if " died of " in text:
            return text.split(" died of ", 1)[1].rstrip(".")
    return None


def party_status(port: int, uids, tile) -> dict:
    """uid -> (position, current action, already ordered to `tile`) for
    the whole party, in ONE console round trip."""
    ids = ",".join(str(u) for u in uids)
    raw = send(port,
               f"local ai=require('scripts.unit_ai'); local o={{}}; "
               f"for _,u in ipairs({{{ids}}}) do local i=unit.getInfo(u); "
               f"local s=ai.getState(u); local t=s and s.commandedTask; "
               f"o[#o+1]=u..':'..(i and (i.gridX..'/'..i.gridY) or 'nil')..':'"
               f"..tostring(s and s.currentAction)..':'"
               f"..tostring(t~=nil and math.floor(t.x)=={tile[0]} "
               f"and math.floor(t.y)=={tile[1]}) end; "
               f"return table.concat(o,';')").strip().strip('"')
    out = {}
    for part in raw.split(";"):
        bits = part.split(":")
        if len(bits) != 4:
            continue
        try:
            uid = int(bits[0])
        except ValueError:
            continue
        pos = None
        if "/" in bits[1]:
            x, y = bits[1].split("/")
            pos = (_as_float(x), _as_float(y))
            pos = pos if None not in pos else None
        out[uid] = (pos, bits[2], bits[3] == "true")
    return out


def line_point(start, end, back: float):
    """The tile on the straight line from `start` to `end`, `back` tiles
    short of `end`."""
    span = dist(start, end)
    k = max(0.0, (span - back) / span) if span else 0.0
    return (int(start[0] + (end[0] - start[0]) * k),
            int(start[1] + (end[1] - start[1]) * k))


def night_factor(sun: float) -> float:
    """`Unit.LineOfSight.nightPerceptionFactor`: 1.0 at noon, 0.5 at
    midnight."""
    height = math.cos((sun - 0.5) * 2 * math.pi)
    return 0.5 + 0.5 * (height + 1.0) / 2.0


def sight_radius(perception: float, night: float) -> int:
    """`Unit.LineOfSight.visibleTilesOnPage`'s binary radius."""
    return max(1, int(math.floor(perception * 6.0 * night)))


def observation_post(port: int, st, fighters, members, first, anchor) -> dict:
    """The farthest tile on the line in from which the party's sharpest
    eye reaches the ruin's bounds while every occupant's sight falls
    short of it by a tile's margin — or no tile, if the occupants
    out-see the party today."""
    sun = _as_float(send(port, f"return world.getSunAngleAt({anchor[0]},"
                               f"{anchor[1]})")) or 0.5
    night = night_factor(sun)

    def perception(u):
        return _as_float(send(port, f"return unit.getStat({u},'perception')")) or 1.0
    radii = sorted(sight_radius(perception(u), night) for u in fighters)
    inst = instance_by_id(port, PAGE, st.occ_id) or {}
    homes = [(int(math.floor(float(o["home_x"]))),
              int(math.floor(float(o["home_y"]))),
              sight_radius(perception(int(o["uid"])), night))
             for o in occupants_of(inst)]
    b = st.occ.get("bounds") or {}
    bounds = [(x, y) for x in range(int(b["min_x"]), int(b["max_x"]) + 1)
              for y in range(int(b["min_y"]), int(b["max_y"]) + 1)]
    span = int(dist(first, anchor))

    def post_for(rp):
        """The farthest tile from which an eye of radius `rp` reaches
        the bounds, and whether every occupant is blind to it."""
        for back in range(min(span, FAR_POST_TILES), 0, -1):
            px, py = line_point(first, anchor, back)
            if min((bx - px) ** 2 + (by - py) ** 2
                   for bx, by in bounds) > rp * rp:
                continue
            unseen = all((px - hx) ** 2 + (py - hy) ** 2 > (rn + 1) ** 2
                         for hx, hy, rn in homes)
            return (px, py), back, unseen
        return None, None, False

    # The weakest eye first, so EVERY member standing at the post can see
    # the ruin; the sharpest only if the weakest's post would be seen.
    for rp in (radii[0], radii[-1]):
        tile, back, unseen = post_for(rp)
        if tile is not None and unseen:
            break
    return {"tile": tile if unseen else None, "back": back,
            "party_radius": rp, "party_radii": radii,
            "occupant_radii": [rn for _x, _y, rn in homes],
            "night": round(night, 2)}


def last_attacker(port: int, uid: int):
    """`unit.getLastAttacker` — {uid, at} for the last hit that landed on
    `uid`, or None."""
    got = send_json(port, f"return unit.getLastAttacker({uid})")
    return got if isinstance(got, dict) and "uid" in got else None


def sig_on_ground(port: int, phys) -> dict | None:
    for g in ground_items(port):
        if g.get("instanceId") == phys:
            return g
    return None


def party_state(port: int, uids) -> dict:
    """uid -> (position, pose, action), for a failure message."""
    return {u: (unit_pos(port, u), pose(port, u), current_action(port, u))
            for u in uids}


def notice_trail(ledger) -> list:
    """Every notice the ledger retained, as (sequence, uid, text) — the
    story of a leg that went wrong, for a failure message."""
    return [(r.get("sequence"), round(float(r.get("gameTime") or 0), 1),
             r.get("uid"), r.get("text"))
            for r in ledger.matching(lambda r: True)]


def notice_at(row: dict, page: str, anchor) -> bool:
    """A location notice about THIS placed instance: the page it names
    and the anchor coordinates it carries. Both ruins share the def
    label in a world with no generated language, so the text alone
    cannot tell them apart."""
    coords = row.get("coords") or {}
    return (row.get("page") == page
            and int(coords.get("x", -1e9)) == int(anchor[0])
            and int(coords.get("y", -1e9)) == int(anchor[1]))


# --------------------------------------------------------------------------
# [encounter]
# --------------------------------------------------------------------------
def muster(port: int, party, tile, seconds: float, observe,
           samples: dict) -> set:
    """Gather `party` at `tile` under ordinary move orders; return who
    stood there in the last sample.

    Bounded, and it does not wait on a straggler: the occupants' clock is
    running once the ruin is paged in (a neglected occupant dies of its
    own physiology — docs/engine_contracts.md §The expedition loop). A
    member an interrupt has taken off its order is ordered again, as a
    player would; one still following is left to walk."""
    for u in party:
        send(port, f"require('scripts.unit_ai').commandMove({u},"
                   f"{tile[0]},{tile[1]}); return 'ok'")
    deadline = time.time() + seconds
    arrived: set = set()
    while time.time() < deadline:
        observe()
        pos = {u: unit_pos(port, u) for u in party}
        # Who is there in THIS sample: the party counts as gathered only
        # when every member stands there at once, not when each has
        # passed through at some point.
        arrived = {u for u, p in pos.items()
                   if p and dist(p, tile) <= 4.0}
        for u, p in pos.items():
            if p:
                samples.setdefault(u, []).append(p)
        if arrived >= set(party):
            break
        for u, p in pos.items():
            if (p is None or dist(p, tile) > 4.0) \
                    and current_action(port, u) != "follow_command":
                send(port, f"require('scripts.unit_ai').commandMove({u},"
                           f"{tile[0]},{tile[1]}); return 'ok'")
        time.sleep(1.0)
    return arrived


def set_out(st: ExpeditionState) -> None:
    """Order the confrontation party to the first ruin — called by the
    facade the moment `travel` has finished with the stay-at-home
    colonists as its never-went-there control, so the party walks while
    `extract` and the prepared traveller's `return` run.

    Orders only; nothing is asserted until `run`. The occupants' clock is
    world time, not time since the ruin was paged in (see
    docs/engine_contracts.md §The expedition loop), so every leg the
    party can walk in parallel is margin the fight keeps.

    The party (owner directive on #2640): the colony's other acolytes,
    each carrying the canteen and rations it spawned with — prepared by
    the tutorial's own predicate. NOT the prepared traveller, which
    carries its loot home on the calibrated return leg (sent on instead,
    after its seeded hunger, it was observed falling asleep, starving, on
    the way home), and never the control."""
    port = st.port
    # Every one of them, as directed: none is silently dropped here, and
    # `run` asserts they are all alive and together before going on.
    st.fighters = [st.scout] + list(st.stay_home)
    # Prepared as the directive describes — a full canteen and at least
    # one ration each. Anyone who has eaten their rations while the
    # survival leg ran is topped up off the technomule before leaving
    # the colony, through the same inventory-transfer surface `prepare`
    # provisions the traveller with; nothing is set directly.
    for u in st.fighters:
        if carried(port, u)[1] < 1:
            send(port, f"return tostring(unit.transferItemToUnit("
                       f"{st.mule},{u},'{RATIONS_DEF}'))")
    # What each member carries as it LEAVES the colony — the moment the
    # tutorial's "prepared" predicate is about. (A hungry colonist eats
    # its rations on the road, so the same reading at the far end of
    # the walk would measure appetite, not preparation.)
    st.party_kit = {u: carried(port, u) for u in st.fighters}
    first = (int(st.ruin_xy[0]), int(st.ruin_xy[1]))
    for u in st.fighters:
        send(port, f"require('scripts.unit_ai').commandMove({u},"
                   f"{first[0]},{first[1]}); return 'ok'")


def run(chk: Checks, st: ExpeditionState) -> None:
    """Gather the party at the first ruin, carry it to the occupied one,
    and defeat its occupants."""
    port = st.port
    occ_id, occ_xy = st.occ_id, st.occ_xy
    fighters = st.fighters

    chk.enter("encounter", "a party gathers at the first ruin, walks on to "
                           "the occupied one; its occupants engage, and are "
                           "defeated")
    # The notice evidence is baselined HERE, where the ruin is about to be
    # proven still unknown: no notice about it can predate the baseline,
    # and a ring eviction during the survival leg (which nothing polls)
    # is not evidence about this one.
    st.ledger = ledger = EventLedger()
    ledger.poll(port)
    inst0 = instance_by_id(port, PAGE, occ_id) or {}
    known_by = [u for u in roster(port).get(ACOLYTE_DEF, [])
                if f"{PAGE}#{occ_id}" in known_locations(port, u)]
    if not chk.ok(inst0.get("lifecycle") == "unknown"
                  and inst0.get("discovered") is False and not known_by,
                  f"the occupied ruin is still UNKNOWN as the confrontation "
                  f"leg begins — no player-owned unit has seen it "
                  f"(lifecycle {inst0.get('lifecycle')!r}, known by "
                  f"{known_by or 'nobody'})"):
        raise StageAbort("the occupied ruin was revealed before its leg")
    chk.ok(fighters == [st.scout] + list(st.stay_home)
           and all(pose(port, u) not in ("dead", "collapsed")
                   for u in fighters),
           f"the party is exactly the scout and the stay-at-home colonists "
           f"{fighters}, every one on its feet — NOT the control traveller "
           f"{st.control}, and not the prepared traveller {st.prepared}, "
           f"whose loot is already home (poses "
           f"{ {u: pose(port, u) for u in fighters} })")

    samples: dict[int, list] = {}
    box = st.occ_box
    control_near = False
    lifecycles: list[str] = []

    def observe():
        nonlocal control_near
        ledger.poll(port)
        i = instance_by_id(port, PAGE, occ_id) or {}
        lc = i.get("lifecycle")
        if not lifecycles or lifecycles[-1] != lc:
            lifecycles.append(lc)
        c = unit_pos(port, st.control)
        if c and in_arrival_box(c, box):
            control_near = True
        return i

    # The leg proceeds FROM the first ruin: the whole party gathers
    # there, together in one sample, before anyone goes on.
    first = (int(st.ruin_xy[0]), int(st.ruin_xy[1]))
    gathered = muster(port, fighters, first, MUSTER_SECONDS, observe, {})
    together = gathered >= set(fighters)
    if not chk.ok(together,
                  f"the whole party stands together at the zero-occupant "
                  f"ruin {first} to set out from it ({sorted(gathered)} of "
                  f"{fighters} there within {MUSTER_SECONDS:.0f} s; "
                  f"{party_state(port, fighters)})"):
        raise StageAbort("the confrontation party never gathered")
    kit = st.party_kit
    chk.ok(set(kit) == set(fighters)
           and all(litres >= PARTY_WATER_L and rations >= 1
                   for litres, rations in kit.values()),
           f"and every member left the colony prepared as directed — at "
           f"least {PARTY_WATER_L:.1f} L of water and a ration each, the "
           f"tutorial's own expedition predicate, read as it set out "
           f"(uid -> (litres, rations): {kit}; now "
           f"{ {u: carried(port, u) for u in fighters} })")

    # Page the ruin in NOW, not at setup: its contents spawn the first
    # time its chunk loads, and that starts its occupants' clock (see
    # setup.pick_occupied). Default padding, so the approach the party
    # is about to walk is paged in with it.
    already = inst0.get("contents_spawned")
    load_region(port, int(st.occ["cx"]), int(st.occ["cy"]))
    inst0 = poll_until(60.0, lambda: (lambda i: i if isinstance(i, dict)
                                      and i.get("contents_spawned") else None)(
        instance_by_id(port, PAGE, occ_id)), interval=1.0) or {}
    st.occ_members = members = [int(o["uid"]) for o in occupants_of(inst0)]
    enc0 = inst0.get("encounter") or {}
    sig0 = significant_rows(port, occ_id)
    chk.ok(len(members) == int(enc0.get("rolled_count", -1)) == st.occ_rolled
           and enc0.get("activated") is False
           and enc0.get("cleared") is False
           and len(living(port, members)) == len(members)
           and inst0.get("lifecycle") == "unknown"
           and len(sig0) == 1 and sig0[0].get("item_instance_id") is not None
           and sig0[0].get("taken") is False,
           f"paged in as the party sets out (contents already spawned "
           f"before: {already!r}), its persisted roll is spawned in full and "
           f"untouched: {len(members)} assigned occupant(s) {members} for a "
           f"roll of {enc0.get('rolled_count')!r}, all alive, encounter not "
           f"yet activated or cleared, still unknown, and its guaranteed item "
           f"in place ({sig0})")
    st.fp["occupant_uids"] = sorted(members)

    # The leg: ordinary moves from the first ruin to the occupied one,
    # and nothing else — its occupants have to FIND the party. A member
    # an interrupt has left idle is ordered on again, as a player would;
    # one that is busy (treating an ally, fighting) is left to it.
    ox, oy = occ_xy
    anchor = (int(ox), int(oy))
    st.page_in_time = _as_float(send(port, "return engine.gameTime()")) or 0.0

    def walk(tile, seconds, until):
        """Order the party to `tile` and keep sampling (once a second,
        in two round trips) until `until()` holds or time runs out."""
        for u in fighters:
            send(port, f"require('scripts.unit_ai').commandMove({u},"
                       f"{tile[0]},{tile[1]}); return 'ok'")
        stop = time.time() + seconds
        while time.time() < stop:
            observe()
            status = party_status(port, fighters, tile)
            for u, (p, action, ordered) in status.items():
                if p:
                    samples.setdefault(u, []).append(p)
                if action in IDLE_ACTIONS and not ordered:
                    send(port, f"require('scripts.unit_ai').commandMove({u},"
                               f"{tile[0]},{tile[1]}); return 'ok'")
            if until(status):
                return True
            time.sleep(1.0)
        return False

    def anyone_within(tile, r):
        return lambda status: any(p and dist(p, tile) <= r
                                  for p, _a, _o in status.values())

    # 1. A far post on the line in, FAR_POST_TILES from the ruin: beyond
    #    anything an occupant can see (radius <= 6 x the highest shipped
    #    perception), so the party arrives unseen.
    far = line_point(first, anchor, FAR_POST_TILES)
    walk(far, ENCOUNTER_SECONDS, anyone_within(far, 2.0))
    # 2. RECONNOITRE. The occupants' notice of an acquisition is emitted
    #    only if the ruin is already discovered, and the world thread's
    #    discovery pass can lag an occupant's quarter-second AI tick —
    #    observed: an occupant acquiring the party while the ruin still
    #    read `unknown`, whose first episode was never announced. So the
    #    party does what a player does: it stops where it can see the
    #    ruin's bounds but its occupants cannot see it, and advances only
    #    once the ruin is discovered. The post is computed from the
    #    shipped sight rule (radius = floor(perception x 6 x night
    #    factor), `Unit.LineOfSight`) with every unit's own perception
    #    and the ruin's local sun angle; the ruin's 5x5 bounds give the
    #    watchers a two-tile head start.
    post = observation_post(port, st, fighters, members, first, anchor)
    st.recon = post
    if post["tile"] is not None:
        # The whole party at the post, not just its first arrival: the
        # post was chosen for an eye a straggler may be carrying.
        walk(post["tile"], ENCOUNTER_SECONDS / 2,
             lambda status: all(p and dist(p, post["tile"]) <= 1.5
                                for p, _a, _o in status.values()))
        stop = time.time() + RECON_SECONDS
        while time.time() < stop:
            if (encounter_state(port, occ_id) or {}).get("lifecycle") \
                    in ("discovered", "active"):
                break
            for u, (p, _a, _o) in party_status(port, fighters,
                                               post["tile"]).items():
                if p:
                    samples.setdefault(u, []).append(p)
            time.sleep(0.5)
    print(f"  reconnaissance: {post}; ruin now "
          f"{(encounter_state(port, occ_id) or {}).get('lifecycle')!r}",
          flush=True)

    # 3. The advance. The activation EDGE is watched at a fine cadence —
    #    one round trip a sample (`encounter_state`) — and the heavier
    #    bookkeeping (ledger, positions, re-orders; two round trips)
    #    only once a second, so the last sample that still says "not
    #    activated" and the first that says "activated" are well under a
    #    game-second apart. `activated` latches, so the only way an
    #    unobserved (and possibly unannounced) episode could have come
    #    first is to open AND close inside that gap — and a closing
    #    episode sends its guard home, from where it re-engages only once
    #    it stands at its post again.
    for u in fighters:
        send(port, f"require('scripts.unit_ai').commandMove({u},"
                   f"{anchor[0]},{anchor[1]}); return 'ok'")
    active = None
    prev_state = None
    edge = None
    next_slow = 0.0
    deadline = time.time() + ENCOUNTER_SECONDS
    while time.time() < deadline:
        now = encounter_state(port, occ_id)
        if now and now["activated"]:
            edge = (prev_state, now)
            active = observe()
            break
        if now:
            prev_state = now
            if not lifecycles or lifecycles[-1] != now["lifecycle"]:
                lifecycles.append(now["lifecycle"])
        if time.time() >= next_slow:
            next_slow = time.time() + 1.0
            ledger.poll(port)
            status = party_status(port, fighters, anchor)
            for u, (p, action, ordered) in status.items():
                if p:
                    samples.setdefault(u, []).append(p)
                if action in IDLE_ACTIONS and not ordered:
                    send(port, f"require('scripts.unit_ai').commandMove({u},"
                               f"{anchor[0]},{anchor[1]}); return 'ok'")
        time.sleep(0.1)
    chk.ok(active is not None,
           f"the occupants acquire the approaching party through their own "
           f"sight/aggression path — encounter activated with an episode "
           f"running (lifecycles seen {lifecycles}, encounter "
           f"{(instance_by_id(port, PAGE, occ_id) or {}).get('encounter')}"
           f"{'' if active else '; party ' + str(party_state(port, fighters)) + '; occupants ' + str(party_state(port, members)) + '; game time ' + str(send(port, 'return engine.gameTime()')) + ' (paged in at ' + str(st.page_in_time) + '); notices ' + str(notice_trail(ledger))})")
    # Asserted on a member that set out from the first ruin.
    walked = sorted(u for u in gathered if len(samples.get(u, [])) >= 2)
    if not walked:
        chk.ok(False, "some party member that gathered at the first ruin "
                      "walked the leg to the occupied one")
    for u in walked[:1]:
        assert_real_travel(chk, samples[u], occ_xy,
                           "the party's leg from the first ruin to the "
                           "occupied one", min_samples=10, min_closed=10.0)
    if active is None:
        raise StageAbort("the occupied ruin's encounter never activated")
    # The activation EDGE, pinned to the notice it must have produced.
    # `unit_ai_encounter.engageExecute` emits the aggression notice
    # before it queues the episode state, so by the time the instance
    # reports the activation the notice is committed; the ledger cursor
    # read now bounds its sequence, and `reward` requires the trail's
    # first aggression notice to fall inside it.
    ledger.poll(port)
    st.activation_cursor = ledger.cursor or 0
    st.activation_time = _as_float(send(port, "return engine.gameTime()")) or 0.0
    # No episode can hide in the gap between those two samples. Either
    # the ruin was ALREADY visible at the last not-activated sample —
    # the reconnaissance's purpose — so every episode that could have
    # opened after it opened on a visible ruin and announced itself; or,
    # failing that, the gap is too short for an episode to open, close
    # and send its guard home before another opens.
    before, after = edge
    gap = (after["t"] - before["t"]) if before else None
    seen_first = bool(before) and before["lifecycle"] in ("discovered",
                                                          "active")
    chk.ok(before is not None and before["activated"] is False
           and (seen_first or (gap is not None
                               and gap < ACTIVATION_EDGE_SECONDS))
           and after["episode_active"] is True
           and after["aggression_announced"] is True
           and after["lifecycle"] in ("discovered", "active"),
           f"that activation is the FIRST episode's own opening, caught at "
           f"its edge: the last sample before it read not-activated "
           f"{gap if gap is None else round(gap, 2)} game-s earlier with "
           f"the ruin {'already visible' if seen_first else 'still unknown'}"
           f" (a visible ruin, or a gap under {ACTIVATION_EDGE_SECONDS} s, "
           f"leaves no room for an unannounced episode), and the episode "
           f"that activated it is running, was announced, and opened on a "
           f"visible ruin (before {before}, after {after})")

    disc = [r for r in ledger.matching(
                lambda r: r.get("category") == "location_discovery")
            if notice_at(r, PAGE, occ_xy)]
    chk.ok(ledger.emissions(
               lambda r: r.get("category") == "location_discovery"
               and notice_at(r, PAGE, occ_xy)) == 1
           and disc and disc[0].get("uid") in fighters,
           f"its discovery is announced exactly once, attributed to the "
           f"party member that first saw it "
           f"({[(r.get('sequence'), r.get('uid'), r.get('text')) for r in disc]})")
    ranks = [LIFECYCLE_ORDER.index(x) if x in LIFECYCLE_ORDER else -1
             for x in lifecycles]
    chk.ok(lifecycles[:1] == ["unknown"] and -1 not in ranks
           and ranks == sorted(ranks),
           f"the lifecycle only ever moves forward from unknown "
           f"({' -> '.join(str(x) for x in lifecycles)})")

    # Requirement 5, while every assigned occupant is alive.
    alive_now = living(port, members)
    chk.ok(len(alive_now) == len(members)
           and active.get("lifecycle") == "active"
           and (active.get("encounter") or {}).get("cleared") is False
           and active.get("clearance_satisfied") is False,
           f"while the encounter runs with every occupant alive "
           f"({alive_now}), the location is 'active' and neither its "
           f"encounter nor its clearance is satisfied "
           f"(lifecycle {active.get('lifecycle')!r}, encounter.cleared "
           f"{(active.get('encounter') or {}).get('cleared')!r}, "
           f"clearance_satisfied {active.get('clearance_satisfied')!r})")

    # The player's answer: an ordinary attack order per party member,
    # the one `init_context_menu.lua`'s Attack entry issues (committed).
    # Re-issued only when its target has died, so every order is one a
    # player would give.
    ordered: dict[int, int] = {}
    accepted: list[tuple] = []
    rejected: list[tuple] = []
    #: occupant uid -> (game time first seen dead, its last attacker).
    deaths: dict[int, tuple] = {}
    cleared_seen: list[bool] = []
    uncleared_while_alive = True
    deadline = time.time() + FIGHT_SECONDS
    while time.time() < deadline:
        i = observe()
        enc = i.get("encounter") or {}
        cleared_seen.append(bool(enc.get("cleared")))
        alive = living(port, members)
        now_t = _as_float(send(port, "return engine.gameTime()")) or 0.0
        for m in members:
            if m not in alive and m not in deaths:
                deaths[m] = (now_t, last_attacker(port, m),
                             death_physiology(port, m))
        if alive and (enc.get("cleared") or i.get("lifecycle") == "cleared"
                      or i.get("clearance_satisfied")):
            uncleared_while_alive = False
        if not alive:
            break
        for u in fighters:
            if pose(port, u) == "dead":
                continue
            if ordered.get(u) not in alive:
                p = unit_pos(port, u)
                target = min(alive, key=lambda t: dist(
                    p or occ_xy, unit_pos(port, t) or occ_xy))
                send(port, f"require('scripts.unit_ai').commandAttack("
                           f"{u},{target},true); return 'ok'")
                # commandAttack returns nothing, so the order is read
                # back off the unit's own AI state: target and goal set.
                # An order the AI dropped at once (a straggler still out
                # of reach) is not recorded as given, so the next pass
                # gives it again — as a player re-clicks.
                held = send(
                    port, f"local ai=require('scripts.unit_ai'); "
                          f"local s=ai.getState({u}); "
                          f"return tostring(s and s.attackTargetUid)..','.."
                          f"tostring(s and ai.isGoalActive(s,'attack'))")
                if held == f"{target},true":
                    ordered[u] = target
                    accepted.append((u, target, held))
                else:
                    rejected.append((u, target, held))
        time.sleep(0.5)
    dead = [u for u in members if u not in living(port, members)]
    chk.ok(len(dead) == len(members),
           f"every assigned occupant is dead ({len(dead)} of {len(members)}; "
           f"party poses { {u: pose(port, u) for u in fighters} }"
           f"{'' if len(dead) == len(members) else '; notices ' + str(notice_trail(ledger))})")
    if len(dead) != len(members):
        raise StageAbort("an assigned occupant survived the fight")
    ordered_on = {m: sorted({u for u, t, _h in accepted if t == m})
                  for m in members}
    chk.ok(all(ordered_on[m] for m in members),
           f"every assigned occupant was attacked under a player order that "
           f"took — read back off the attacker's AI state, target set under "
           f"an active attack goal (occupant -> ordered attackers "
           f"{ordered_on}; orders the AI dropped at once and that were "
           f"re-given: {rejected})")
    # The PARTY killed them, not the occupants' own physiology (a
    # neglected occupant dies of electrolyte imbalance on its own — see
    # docs/engine_contracts.md §The expedition loop): each occupant's
    # last recorded hit came from a party member, after the activation,
    # and within DEATH_BLOW_SECONDS of when it was first seen dead.
    # ...and by a COMBAT cause, not a physiological one. A hit, however
    # recent, only proves a hit landed; the independent physiological
    # kill paths (salt imbalance, hypo/hyperthermia, dehydration,
    # starvation) each leave their own mark, so each is ruled out at the
    # death itself: the occupant's physiological failure meters are
    # below 1, its corpse carries wounds, and it has no death notice
    # naming a physiological cause. Every physiological death — the
    # meters, and the meterless resource deaths (dehydration, starvation,
    # exhaustion) — announces itself "X died of <cause>"; a combat kill
    # (`Combat.Resolution.setDead`) announces nothing on the event log at
    # all, so with the ledger proven complete below, silence is evidence.
    ledger.poll(port)
    blows = {}
    for m in members:
        seen_at, att, phys = deaths.get(m, (None, None, {}))
        cause = death_cause(ledger, m)
        blows[m] = {"last_attacker": att, "seen_dead": seen_at,
                    "physiology": phys, "cause": cause}
    by_party = all(
        b["last_attacker"] is not None
        and b["last_attacker"].get("uid") in ordered_on.get(m, [])
        and float(b["last_attacker"].get("at") or -1) >= st.activation_time
        and b["seen_dead"] is not None
        and b["seen_dead"] - float(b["last_attacker"].get("at") or -1)
        <= DEATH_BLOW_SECONDS
        for m, b in blows.items())
    by_wounds = all(
        b["physiology"].get("wounds", 0) > 0
        and all(float(b["physiology"].get(k) or 0) < 1.0
                for k in PHYSIOLOGICAL_METERS)
        and b["cause"] not in PHYSIOLOGICAL_CAUSES
        for b in blows.values())
    chk.ok(by_party,
           f"and the party's combat is what felled each one: its last hit "
           f"came from a party member holding a player attack order on it, "
           f"after the activation (game time "
           f"{st.activation_time:.1f}) and within {DEATH_BLOW_SECONDS:.0f} s "
           f"of its death ({blows})")
    chk.ok(by_wounds,
           f"...and each died of its wounds, not of its own physiology: "
           f"wounds on the corpse, every physiological failure meter "
           f"{PHYSIOLOGICAL_METERS} below 1 at the death, and no death "
           f"notice naming a physiological cause "
           f"{sorted(PHYSIOLOGICAL_CAUSES)} ({blows})")
    chk.ok(uncleared_while_alive,
           "at no point while any occupant lived was the encounter, the "
           "clearance predicate or the lifecycle reported cleared")

    # The encounter half latches once the world thread has processed the
    # last death: one false -> true transition, and then it stays.
    settled = poll_until(30.0, lambda: (lambda i: i if (
        (i.get("encounter") or {}).get("cleared") is True) else None)(
            observe()), interval=0.5)
    cleared_seen.append(bool(((settled or {}).get("encounter") or {})
                             .get("cleared")))
    flips = sum(1 for a, b in zip(cleared_seen, cleared_seen[1:]) if a != b)
    chk.ok(settled is not None and cleared_seen[0] is False
           and cleared_seen[-1] is True and flips == 1,
           f"after the last death encounter.cleared becomes true in ONE "
           f"false -> true transition and stays latched "
           f"({flips} transition(s) over {len(cleared_seen)} samples)")

    # The natural clearing order's middle state, observed and not
    # inferred: hostiles down, reward still lying where it spawned.
    sig = significant_rows(port, occ_id)
    st.occ_sig_phys = phys = sig[0].get("item_instance_id") if sig else None
    ground = sig_on_ground(port, phys) if phys is not None else None
    now = instance_by_id(port, PAGE, occ_id) or {}
    chk.ok(len(sig) == 1 and phys is not None and sig[0].get("taken") is False
           and ground is not None
           and now.get("clearance_satisfied") is False
           and now.get("lifecycle") != "cleared",
           f"with every occupant dead the location is STILL not cleared, "
           f"because its guaranteed item is still on the ground untaken "
           f"(significant {sig}, on ground "
           f"{ {k: ground.get(k) for k in ('id', 'defName', 'x', 'y')} if ground else None}, "
           f"lifecycle {now.get('lifecycle')!r}, clearance_satisfied "
           f"{now.get('clearance_satisfied')!r})")
    if ground is None:
        raise StageAbort("the guaranteed item was not on the ground when "
                         "the fight ended")
    st.occ_sig_gid = int(ground["id"])
    st.occ_lifecycles = lifecycles
    st.control_near_occ = control_near
    st.occ_sig_def = sig[0].get("item")
    st.fp.update(occupied_significant_def=sig[0].get("item"),
                 occupied_significant_instance=phys)


# --------------------------------------------------------------------------
# [reward]
# --------------------------------------------------------------------------
def reward(chk: Checks, st: ExpeditionState) -> None:
    """Recover the guaranteed item by the player's gesture; the location
    clears, exactly once."""
    port, ledger = st.port, st.ledger
    occ_id, occ_xy = st.occ_id, st.occ_xy
    phys, gid = st.occ_sig_phys, st.occ_sig_gid

    chk.enter("reward", "the guaranteed item is recovered by the player's "
                        "gesture and the ruin clears")
    ledger.poll(port)
    # The carrier is the first living party member the pickup order is
    # ACCEPTED for, nearest to the item first: commandPickup refuses at command time
    # when the item would not fit (#920), and a refusal stores no order.
    carrier, accepted = None, []
    item_xy = next(((g.get("x"), g.get("y")) for g in ground_items(port)
                    if g.get("id") == gid), occ_xy)
    nearest_first = sorted(st.fighters, key=lambda u: (
        dist(unit_pos(port, u) or (1e9, 1e9), item_xy), u))
    for u in nearest_first:
        if pose(port, u) in ("dead", "collapsed"):
            continue
        got = send(port, f"return require('scripts.unit_ai')"
                         f".commandPickup({u},{gid})").strip()
        accepted.append((u, got))
        if got == "true":
            carrier = u
            break
    if not chk.ok(carrier is not None,
                  f"the player's pickup order for the guaranteed item is "
                  f"accepted by a living party member ({accepted})"):
        raise StageAbort("no party member could take the guaranteed item")
    st.occ_carrier = carrier

    saw_pickup, held = False, None
    deadline = time.time() + 180.0
    while time.time() < deadline:
        ledger.poll(port)
        if current_action(port, carrier) == "pickup_ground":
            saw_pickup = True
        held = next((it for it in inventory(port, carrier)
                     if it.get("instanceId") == phys), None)
        if held:
            break
        time.sleep(0.5)
    chk.ok(saw_pickup and held is not None,
           f"the carrier ({carrier}, {pose(port, carrier)}) walks to it and "
           f"takes THAT physical instance ({phys}) into its own pack through "
           f"the real pickup_ground action (seen={saw_pickup}, held "
           f"{ {k: (held or {}).get(k) for k in ('defName', 'instanceId')} })")
    if held is None:
        raise StageAbort("the guaranteed item never reached the carrier")

    rows = poll_until(60.0, lambda: (lambda r: r if r and all(
        x.get("taken") for x in r) else None)(significant_rows(port, occ_id)),
        interval=0.5)
    chk.ok(rows is not None and rows[0].get("item_instance_id") == phys,
           f"its taken latch is set for that physical instance id ({rows})")
    cleared = poll_until(60.0, lambda: (lambda i: i if isinstance(i, dict)
                                        and i.get("lifecycle") == "cleared"
                                        else None)(
        instance_by_id(port, PAGE, occ_id)), interval=0.5)
    chk.ok(cleared is not None and cleared.get("clearance_satisfied") is True
           and cleared.get("clear_event_emitted") is True,
           f"and THAT promotes the location to 'cleared' "
           f"({(cleared or {}).get('lifecycle')!r}, satisfied "
           f"{(cleared or {}).get('clearance_satisfied')!r})")

    # Settle, then read the retained notice evidence for this ruin.
    time.sleep(3.0)
    ledger.poll(port)

    def is_clear(r):
        return (r.get("category") == "location_clearance"
                and notice_at(r, PAGE, occ_xy))

    name = (cleared or {}).get("name") or ""

    def from_occupant(r, verb):
        return (r.get("category") == "unit_event"
                and r.get("uid") in st.occ_members
                and f" at {name}" in (r.get("text") or "")
                and verb in (r.get("text") or ""))
    clear_rows = ledger.matching(is_clear)
    chk.ok(ledger.emissions(is_clear) == 1,
           f"exactly one clearance notice for the occupied ruin "
           f"({[(r.get('sequence'), r.get('count'), r.get('text')) for r in clear_rows]})")

    # Aggression: exactly once PER EPISODE (owner directive on #2640). A
    # lone nomad wounded by a stronger acolyte retreats; the ruin guard
    # then walks it home, which ends the episode with a disengage notice,
    # and on re-acquiring the party it opens a NEW episode with its own
    # aggression notice — shipped #916 behaviour. So the notices, in
    # sequence order, must alternate aggression / disengage starting with
    # aggression, each emitted once (no coalesced repeat): the
    # acquisition that activated the encounter announced itself exactly
    # once, and so did every later episode.
    trail = sorted(
        [("A", r) for r in ledger.matching(
            lambda r: from_occupant(r, " attacks "))]
        + [("D", r) for r in ledger.matching(
            lambda r: from_occupant(r, " disengaged at "))],
        key=lambda kr: kr[1].get("sequence", 0))
    kinds = [k for k, _ in trail]
    episodes = kinds.count("A")
    first_seq = trail[0][1].get("sequence", 1 << 60) if trail else 1 << 60
    chk.ok(kinds[:1] == ["A"] and first_seq <= st.activation_cursor,
           f"the acquisition that activated the encounter announced itself: "
           f"the first occupant notice is an aggression notice, committed by "
           f"the time the activation was observed (sequence {first_seq if trail else None} "
           f"<= cursor {st.activation_cursor})")
    chk.ok(kinds[:1] == ["A"]
           and all(a != b for a, b in zip(kinds, kinds[1:]))
           and all(int(r.get("count") or 1) == 1 for _, r in trail),
           f"its occupants announced aggression exactly once per episode — "
           f"once for that acquisition, and once more only for each episode "
           f"reopened after its own disengage notice ({episodes} episode(s): "
           f"{[(k, r.get('sequence'), r.get('count'), r.get('text')) for k, r in trail]})")
    st.occ_episodes = episodes
    lost = ledger.unexplained()
    chk.ok(not lost and ledger.polls > 10,
           f"and that notice evidence is complete: {ledger.polls} polls "
           f"of the event log from the moment the leg began, retained by "
           f"sequence, with no committed interval that could have been "
           f"evicted unseen ({len(ledger.missing)} coalescing gap(s), "
           f"{len(lost)} unexplained{': ' + str(lost[:3]) if lost else ''})")

    # The four trip objectives, through the real evaluator, in authored
    # order — same-pass latches allowed (#2640 review correction).
    completed, _checked = progress(port)
    passes = latch_passes(port)
    order = [passes.get(o) for o in TRIP_OBJECTIVES]
    chk.ok(set(TRIP_OBJECTIVES) <= completed and None not in order
           and order == sorted(order),
           f"the four trip objectives have latched through the real "
           f"evaluator in authored order "
           f"({list(zip(TRIP_OBJECTIVES, order))}; evaluation pass numbers "
           f"may repeat, never decrease)")
    st.fp["objectives"] = sorted(completed)

    chk.ok(not st.control_near_occ,
           f"the control traveller {st.control} took no part — it was never "
           f"observed at the occupied ruin")

    # Home, the whole party, ordered now so it walks together. The
    # deposits are asserted from the `return` stage, beside the first
    # ruin's items.
    for u in st.fighters:
        if pose(port, u) != "dead":
            send(port, f"require('scripts.unit_ai').commandMove({u},"
                       f"{st.deposit_spot[0]},{st.deposit_spot[1]}); "
                       f"return 'ok'")


def deliver_home(chk: Checks, st: ExpeditionState) -> None:
    """[return], continued: the occupied ruin's guaranteed item is banked
    in colony storage as that exact physical instance.

    `processing_unit` is a Materials def, so `store_materials` may bank
    it autonomously once its carrier is in reach of the colony cargo —
    `extract.bank_home` follows the item, and the assertion is on the
    outcome, exactly as for the first ruin's."""
    port, phys = st.port, st.occ_sig_phys
    # Re-entered: the first ruin's items came home, under this same
    # stage, before the confrontation leg set out.
    chk.enter("return", "the occupied ruin's guaranteed item comes home too")
    got = bank_home(port, st, phys, OCCUPIED_RETURN_SECONDS)
    chk.ok(got is not None and got.get("defName") == st.occ_sig_def,
           f"the occupied ruin's guaranteed item is carried home and banked "
           f"in colony storage as that exact physical instance ({phys}, "
           f"{(got or {}).get('defName')!r}"
           f"{'' if got else '; now ' + locate(port, phys)})")
    chk.ok(all(r.get("taken") for r in significant_rows(port, st.occ_id)),
           "and its taken latch is unmoved by the walk and the deposit")
