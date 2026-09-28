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

  encounter  the party walks from the first ruin to the occupied one; it
             was `unknown` when the leg began and is discovered by sight
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

The control traveller takes no part. It is left holding the first ruin
exactly as the travel stage left it, and the stage asserts it never
comes near the occupied one.
"""
from __future__ import annotations

import time

from probelib import poll_until, send

from .constants import (ACOLYTE_DEF, ENCOUNTER_SECONDS, FIGHT_SECONDS, PAGE,
                        STAGING_BACKOFF_TILES, TRIP_OBJECTIVES)
from .extract import bank_home, locate
from .harness import Checks, ExpeditionState, StageAbort, assert_real_travel
from .notices import EventLedger, latch_passes
from .readers import (current_action, dist, ground_items, in_arrival_box,
                      instance_by_id, inventory, known_locations, pose,
                      progress, roster, significant_rows, unit_pos)


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
    return [(r.get("sequence"), r.get("uid"), r.get("text"))
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
def run(chk: Checks, st: ExpeditionState) -> None:
    """Carry the party to the occupied ruin and defeat its occupants."""
    port = st.port
    occ_id, occ_xy = st.occ_id, st.occ_xy
    fighters = st.fighters = [u for u in [st.prepared, st.scout]
                              + list(st.stay_home)
                              if pose(port, u) not in ("dead", "collapsed")]

    chk.enter("encounter", "the party walks to the occupied ruin; its "
                           "occupants engage, and are defeated")
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
    st.occ_members = members = [int(o["uid"]) for o in occupants_of(inst0)]
    enc0 = inst0.get("encounter") or {}
    chk.ok(len(members) == int(enc0.get("rolled_count", -1)) >= 1
           and enc0.get("activated") is False
           and enc0.get("cleared") is False
           and len(living(port, members)) == len(members),
           f"its persisted roll is spawned in full and untouched: "
           f"{len(members)} assigned occupant(s) {members} for a roll of "
           f"{enc0.get('rolled_count')!r}, all alive, encounter not yet "
           f"activated or cleared")
    chk.ok(st.control not in fighters and len(fighters) >= 2,
           f"the party is {fighters} — player acolytes from the colony "
           f"roster, and NOT the control traveller {st.control}")

    # The party gathers out of sight of the ruin: a tile on the straight
    # line from the first ruin, STAGING_BACKOFF_TILES short of the
    # occupied anchor. It is a muster, not a hold — its only job is that
    # the party arrives together rather than strung out over a hundred
    # tiles, so the first acolyte in does not fight alone.
    sx, sy = st.ruin_xy
    ox, oy = occ_xy
    span = dist((sx, sy), (ox, oy))
    k = max(0.0, (span - STAGING_BACKOFF_TILES) / span) if span else 0.0
    staging = (int(sx + (ox - sx) * k), int(sy + (oy - sy) * k))
    print(f"  party {fighters} musters at {staging}, "
          f"{STAGING_BACKOFF_TILES} tiles short of the occupied ruin "
          f"{occ_id} at {occ_xy}", flush=True)
    for u in fighters:
        send(port, f"require('scripts.unit_ai').commandMove({u},"
                   f"{staging[0]},{staging[1]}); return 'ok'")
    samples: dict[int, list] = {u: [] for u in fighters}
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

    deadline = time.time() + ENCOUNTER_SECONDS
    mustered = False
    while time.time() < deadline:
        observe()
        pos = {u: unit_pos(port, u) for u in fighters}
        for u, p in pos.items():
            if p:
                samples[u].append(p)
        if all(p and dist(p, staging) <= 4.0 for p in pos.values()):
            mustered = True
            break
        for u, p in pos.items():
            if (p is None or dist(p, staging) > 4.0) \
                    and current_action(port, u) != "follow_command":
                send(port, f"require('scripts.unit_ai').commandMove({u},"
                           f"{staging[0]},{staging[1]}); return 'ok'")
        time.sleep(1.0)
    chk.ok(mustered,
           f"the party walks from the first ruin to the muster short of the "
           f"occupied one ({party_state(port, fighters)}"
           f"{'' if mustered else '; notices ' + str(notice_trail(ledger))})")
    # Asserted on the prepared traveller, the one party member that
    # starts this leg at the first ruin.
    for u in fighters[:1]:
        assert_real_travel(chk, samples[u], staging,
                           "the party's leg to the occupied ruin",
                           min_samples=10, min_closed=10.0)

    # The approach: an ordinary move to the ruin, and nothing else. Its
    # occupants have to FIND the party themselves. A member an interrupt
    # has left idle is ordered on again, as a player would; one that is
    # busy (treating an ally, fighting) is left to it.
    for u in fighters:
        send(port, f"require('scripts.unit_ai').commandMove({u},"
                   f"{int(ox)},{int(oy)}); return 'ok'")
    active = None
    deadline = time.time() + ENCOUNTER_SECONDS
    while time.time() < deadline:
        i = observe()
        enc = i.get("encounter") or {}
        if enc.get("activated") and enc.get("episode_active"):
            active = i
            break
        for u in fighters:
            if current_action(port, u) in IDLE_ACTIONS:
                send(port, f"require('scripts.unit_ai').commandMove({u},"
                           f"{int(ox)},{int(oy)}); return 'ok'")
        time.sleep(0.5)
    chk.ok(active is not None,
           f"the occupants acquire the approaching party through their own "
           f"sight/aggression path — encounter activated with an episode "
           f"running (lifecycles seen {lifecycles}, encounter "
           f"{(instance_by_id(port, PAGE, occ_id) or {}).get('encounter')}"
           f"{'' if active else '; party ' + str(party_state(port, fighters)) + '; occupants ' + str(party_state(port, members)) + '; notices ' + str(notice_trail(ledger))})")
    if active is None:
        raise StageAbort("the occupied ruin's encounter never activated")

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
    cleared_seen: list[bool] = []
    uncleared_while_alive = True
    deadline = time.time() + FIGHT_SECONDS
    while time.time() < deadline:
        i = observe()
        enc = i.get("encounter") or {}
        cleared_seen.append(bool(enc.get("cleared")))
        alive = living(port, members)
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
                ordered[u] = target
        time.sleep(0.5)
    dead = [u for u in members if u not in living(port, members)]
    chk.ok(len(dead) == len(members),
           f"the party defeats every assigned occupant through ordinary "
           f"attack orders ({len(dead)} of {len(members)} dead; party "
           f"poses { {u: pose(port, u) for u in fighters} })")
    if len(dead) != len(members):
        raise StageAbort("an assigned occupant survived the fight")
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
    # ACCEPTED for, in party order: commandPickup refuses at command time
    # when the item would not fit (#920), and a refusal stores no order.
    carrier, accepted = None, []
    for u in st.fighters:
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
    chk.ok(kinds[:1] == ["A"]
           and all(a != b for a, b in zip(kinds, kinds[1:]))
           and all(int(r.get("count") or 1) == 1 for _, r in trail),
           f"its occupants announced aggression exactly once per episode — "
           f"once for the acquisition that activated the encounter, and "
           f"once more only for each episode reopened after its own "
           f"disengage notice ({episodes} episode(s): "
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
        if u != st.prepared and pose(port, u) != "dead":
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
    got = bank_home(port, st, phys)
    chk.ok(got is not None and got.get("defName") == st.occ_sig_def,
           f"the occupied ruin's guaranteed item is carried home and banked "
           f"in colony storage as that exact physical instance ({phys}, "
           f"{(got or {}).get('defName')!r}"
           f"{'' if got else '; now ' + locate(port, phys)})")
    chk.ok(all(r.get("taken") for r in significant_rows(port, st.occ_id)),
           "and its taken latch is unmoved by the walk and the deposit")
