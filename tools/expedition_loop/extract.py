#!/usr/bin/env python3
"""[extract] and [return] — recover it, carry it home, bank it (#2092).

One owner for the two stages that move a physical item: the retrieval
orders issued at the ruin (the measured loot roll, and #917's guaranteed
significant item), the walk home, and the deposit into colony storage
that is what "invest" means for this slice.

They are one owner because they are one continuous custody chain — the
exact instance `run` puts into the carrier's inventory is the one
`deliver` asserts into colony storage, and the significant item's taken
latch is asserted at both ends. Splitting them would put the two halves
of one identity check in different modules.

Every gesture here is the shipped player one: `unitAi.commandPickup`
acted on through the real `pickup_ground` action, `unitAi.commandMove`
home, and the lax `unit.depositToCargo` AI verb with this probe's own
adjacency assertion beside it (D-7 — that verb has no adjacency gate of
its own).
"""
from __future__ import annotations

import time

from probelib import poll_until, send, send_json

from .constants import ACOLYTE_DEF, PAGE, RETURN_SECONDS
from .harness import Checks, ExpeditionState, StageAbort, assert_real_travel
from .readers import (clearance_events, current_action, event_log,
                      find_instance, find_instance_by_def, fmt_vitals,
                      ground_items, instance_by_id, inventory, is_adjacent,
                      pose, properties, roster, significant_rows, unit_pos,
                      vitals, carried)


# --------------------------------------------------------------------------
# Driving one walk, with its samples recorded for the travel check
# --------------------------------------------------------------------------
def walk_until_adjacent(port: int, uid: int, foot, seconds: float,
                        samples: list):
    """Poll a walking unit until it stands adjacent to `foot`, recording
    every position sample on the way. Returns True if it arrived."""
    deadline = time.time() + seconds
    while time.time() < deadline:
        p = unit_pos(port, uid)
        if p:
            samples.append(p)
            if is_adjacent(p, foot):
                return True
        time.sleep(1.0)
    return False


def holder_of(port: int, phys: int):
    """The live player acolyte carrying physical instance `phys`, or
    None."""
    for uid in roster(port).get(ACOLYTE_DEF, []):
        if find_instance(inventory(port, uid), phys) is not None:
            return uid
    return None


def locate(port: int, phys: int) -> str:
    """Where physical instance `phys` is right now, for a failure
    message: a carrier, the ground, or nowhere this probe can see."""
    uid = holder_of(port, phys)
    if uid is not None:
        # What the carrier is DOING, not just where: a survival
        # interrupt (a canteen refill at the colony lake, sleep) takes
        # over a move order home, and that is a finding, not noise.
        task = send(port, f"local s=require('scripts.unit_ai').getState({uid}); "
                          f"local t=s and s.commandedTask; "
                          f"return t and (math.floor(t.x)..','..math.floor(t.y)) "
                          f"or 'none'").strip().strip('"')
        return (f"carried by {uid} at {unit_pos(port, uid)}, action "
                f"{current_action(port, uid)}, pose {pose(port, uid)}, order "
                f"{task}, water/rations {carried(port, uid)}, "
                f"{fmt_vitals(vitals(port, uid))}")
    g = next((g for g in ground_items(port) if g.get("instanceId") == phys),
             None)
    if g is not None:
        return f"on the ground at ({g.get('x')},{g.get('y')})"
    for uid in roster(port).get("technomule", []):
        if find_instance(inventory(port, uid), phys) is not None:
            return f"carried by the technomule {uid}"
    return "not carried by a live acolyte, not on the ground"


def ordered_to(port: int, uid: int, tile) -> bool:
    """Whether `uid` already holds a pending player move order to `tile`."""
    got = send(port, f"local s=require('scripts.unit_ai').getState({uid}); "
                     f"local t=s and s.commandedTask; "
                     f"return t and (math.floor(t.x)..','..math.floor(t.y)) "
                     f"or 'none'").strip().strip('"')
    return got == f"{int(tile[0])},{int(tile[1])}"


def bank_home(port: int, st: ExpeditionState, phys: int,
              seconds: float = RETURN_SECONDS):
    """Bring physical instance `phys` home and into colony storage,
    whoever carries it, and return its storage row (or None).

    A guaranteed item is a Materials def, so it can change hands without
    a player order — a colonist in the ruin may pick it up of its own
    accord, and `store_materials` may bank it once its carrier is in
    reach of the cargo. So this follows the ITEM, not a named carrier:
    whoever holds it is walked home, and deposits it by the same lax
    `unit.depositToCargo` verb the measured item uses, only from a tile
    adjacent to the storage footprint."""
    deadline = time.time() + seconds
    while time.time() < deadline:
        stored = send_json(port, f"return building.getStorage({st.storage_bid})")
        row = find_instance(stored if isinstance(stored, list) else [], phys)
        if row is not None:
            return row
        uid = holder_of(port, phys)
        if uid is not None:
            p = unit_pos(port, uid)
            if p and is_adjacent(p, st.foot):
                item = find_instance(inventory(port, uid), phys) or {}
                send(port, f"return unit.depositToCargo({uid},{st.storage_bid},"
                           f"'{item.get('defName', '')}',{phys})")
            elif current_action(port, uid) != "follow_command" \
                    and not ordered_to(port, uid, st.deposit_spot):
                # Only when no order home is pending: re-issuing one
                # resets the unit's path, and a long path can take longer
                # to plan than this loop's second — observed: a carrier
                # re-ordered every second never left the occupied ruin.
                send(port, f"require('scripts.unit_ai').commandMove({uid},"
                           f"{st.deposit_spot[0]},{st.deposit_spot[1]}); "
                           f"return 'ok'")
        time.sleep(1.0)
    return None


def run(chk: Checks, st: ExpeditionState) -> None:
    """Recover the ruin's own loot-table output."""
    port = st.port
    prepared = st.prepared
    ruin_id, target, already = st.ruin_id, st.target, st.already

    chk.enter("extract", "recover the ruin's own loot-table output")
    # Issued only now, so the shared travel leg above was the
    # same verb at the same speed for both travellers. The
    # carrier is already standing at the ruin; this is
    # the "Pick up" the player clicks once the party has
    # arrived.
    acc_p = send(port, f"return require('scripts.unit_ai').commandPickup("
                       f"{prepared},{int(target['id'])})")
    chk.ok(acc_p.strip() == "true",
           f"the retrieval order is accepted at the ruin "
           f"(commandPickup -> {acc_p!r})")
    saw_pickup = False
    picked = None
    deadline = time.time() + 180.0
    while time.time() < deadline:
        if current_action(port, prepared) == "pickup_ground":
            saw_pickup = True
        picked = find_instance_by_def(inventory(port, prepared),
                                      target["defName"], already)
        if picked:
            break
        time.sleep(1.0)
    chk.ok(saw_pickup,
           f"the carrier acts on the order through the real "
           f"pickup_ground AI action (last action "
           f"{current_action(port, prepared)})")
    if not chk.ok(picked is not None,
                  f"the carrier picks up the {target['defName']} the ruin "
                  f"itself rolled (action "
                  f"{current_action(port, prepared)}, pose "
                  f"{pose(port, prepared)})"):
        raise StageAbort("the carrier never picked the target up")
    st.recovered = recovered = picked
    st.instance_id = instance_id = recovered["instanceId"]
    chk.ok(not any(g.get("id") == int(target["id"])
                   for g in ground_items(port)),
           f"the ruin's ground item (gid {target['id']}) is gone from the "
           f"world — it MOVED into the carrier, it was not copied")
    name = send(port, f"local i=unit.getInfo({prepared}); "
                      f"return i and i.name or ''")
    disp = recovered.get("displayName") or target["defName"]
    hits = [e for e in event_log(port)
            if e.get("category") == "unit_event"
            and e.get("uid") == prepared
            and disp in (e.get("text") or "")
            and name and name in (e.get("text") or "")]
    chk.ok(bool(hits),
           f"the recovery is reported on a player-facing surface naming "
           f"the item and its carrier: "
           f"{hits[-1]['text'] if hits else '(no event)'}")
    print(f"  recovered instance {instance_id}: {properties(recovered)}",
          flush=True)
    st.fp["recovered_def"] = recovered.get("defName")

    # --- #917: the ruin's GUARANTEED significant item, which is
    # what its cleared state actually waits on.
    #
    # WHO carries it out is deliberately not asserted. It is a
    # Materials def, so `store_materials` fires on any colonist
    # holding one with the colony cargo in reach — and a
    # colonist standing in the ruin will pick a loose Materials
    # item up of its own accord. That is ordinary shipped
    # behaviour, not a defect, and an observed run had the
    # travelling acolyte recover it during the leg. What #917
    # promises is that the location does not clear until the
    # item is RECOVERED, not that a particular gesture recovers
    # it, so the assertions below are about the outcome. The
    # "still outstanding" half is proved at `setup`, at the only
    # moment it is guaranteed observable: before anyone has been
    # near the ruin.
    sig_now = significant_rows(port, ruin_id)
    st.sig_phys = sig_phys = (
        sig_now[0].get("item_instance_id") if sig_now else None)
    if not chk.ok(len(sig_now) == 1 and sig_phys is not None,
                  f"the ruin still owes exactly one guaranteed "
                  f"significant item, bound to its spawned instance "
                  f"({sig_now})"):
        raise StageAbort("the ruin's guaranteed obligation is unreadable")

    # Issue the player gesture only if it is still there to take;
    # otherwise a colonist has already recovered it, which
    # satisfies the loop just as well.
    sig_gid = next((int(g["id"]) for g in ground_items(port)
                    if int(g.get("instanceId", -1)) == sig_phys), None)
    if sig_gid is not None:
        acc_s = send(port,
                     f"return require('scripts.unit_ai').commandPickup("
                     f"{prepared},{sig_gid})")
        chk.ok(acc_s.strip() == "true",
               f"the retrieval order for it is accepted (commandPickup "
               f"-> {acc_s!r})")
    else:
        print("  the guaranteed item was already recovered by the "
              "colony's own AI before the player gesture — the loop "
              "is unaffected, only who carried it", flush=True)

    sig_after = poll_until(
        180.0,
        lambda: (significant_rows(port, ruin_id)
                 if all(r.get("taken")
                        for r in significant_rows(port, ruin_id))
                 else None),
        interval=1.0)
    chk.ok(sig_after is not None
           and sig_after[0].get("item_instance_id") == sig_phys,
           f"recovering it latches THAT physical item as taken, keeping "
           f"its provenance ({sig_after})")
    cleared_inst = poll_until(
        60.0,
        lambda: (lambda i: i if isinstance(i, dict)
                 and i.get("lifecycle") == "cleared" else None)(
                     instance_by_id(port, PAGE, ruin_id)),
        interval=1.0)
    chk.ok(cleared_inst is not None
           and cleared_inst.get("clearance_satisfied") is True,
           f"and THAT is what clears the ruin — the last outstanding "
           f"condition ({(cleared_inst or {}).get('lifecycle')!r})")
    # Exactly one notice for THIS ruin across the whole run,
    # counted by its own name rather than by a delta, since the
    # recovery may have happened before this stage.
    ruin_name = (cleared_inst or {}).get("name") or ""
    clear_evs = [e for e in clearance_events(port)
                 if ruin_name and ruin_name in (e.get("text") or "")]
    chk.ok(len(clear_evs) == 1,
           f"exactly one clearance notice is emitted for it across the "
           f"whole run, not zero and not two "
           f"({[e.get('text') for e in clear_evs]})")
    st.fp.update(significant_def=sig_after[0].get("item")
                 if sig_after else None,
                 significant_instance=sig_phys)


def deliver(chk: Checks, st: ExpeditionState) -> None:
    """[return] — walk home and bank it in colony storage."""
    port = st.port
    prepared = st.prepared
    ruin_id, storage_bid = st.ruin_id, st.storage_bid
    deposit_spot, foot = st.deposit_spot, st.foot
    recovered, instance_id, sig_phys = (st.recovered, st.instance_id,
                                        st.sig_phys)

    chk.enter("return", "walk home and bank it in colony storage")
    send(port, f"require('scripts.unit_ai').commandMove({prepared},"
               f"{deposit_spot[0]},{deposit_spot[1]}); return 'ok'")
    r_samples: list = []
    arrived = walk_until_adjacent(port, prepared, foot, RETURN_SECONDS,
                                  r_samples)
    chk.ok(bool(arrived),
           f"the carrier walks the whole way home and arrives adjacent to "
           f"colony storage (at {unit_pos(port, prepared)}, footprint "
           f"{foot}, action {current_action(port, prepared)}, "
           f"{fmt_vitals(vitals(port, prepared))})")
    assert_real_travel(chk, r_samples, deposit_spot, "the return leg",
                       min_samples=10, min_closed=10.0)
    chk.ok(find_instance(inventory(port, prepared), instance_id) is not None,
           "the recovered item is still carried at the end of the return leg")

    # A lax AI verb (D-7) with no adjacency gate of its own, so
    # the adjacency asserted beside it is this probe's own rule.
    # It used to be the call the "Store in <cargo>" menu entry
    # made; #1249 retired that entry for a queued order, and this
    # step stays direct so "invest" does not wait on the transfer
    # executor's own timing.
    at_deposit = unit_pos(port, prepared)
    adj = bool(at_deposit) and is_adjacent(at_deposit, foot)
    ok = send(port, f"return unit.depositToCargo({prepared},{storage_bid},"
                    f"'{recovered['defName']}',{instance_id})")
    chk.ok(adj and ok.strip() == "true",
           f"the carrier banks it in colony storage from an adjacent tile "
           f"(adjacent={adj} at {at_deposit}, returned {ok!r})")
    stored = send_json(port, f"return building.getStorage({storage_bid})")
    chk.ok(find_instance(stored if isinstance(stored, list) else [],
                         instance_id) is not None,
           f"the exact recovered instance is in colony storage "
           f"(bid {storage_bid})")

    # #917: the guaranteed item makes the same trip, whoever
    # ended up carrying it. It may already have been banked
    # autonomously — `processing_unit` is a Materials def, and
    # `store_materials` fires on any Materials in inventory with
    # the colony's cargo in reach — and since #2640 its carrier may
    # be a colonist who went on to the occupied ruin, so `bank_home`
    # follows the item rather than this carrier, and the assertion is
    # on the OUTCOME: that exact physical instance ends up in colony
    # storage.
    banked = bank_home(port, st, sig_phys)
    chk.ok(banked is not None,
           f"the guaranteed item is banked in colony storage as that "
           f"exact physical instance ({sig_phys}"
           f"{'' if banked else '; now ' + locate(port, sig_phys)})")
    # Taking it out of the ruin and moving it around cannot undo
    # the latch: the ruin was looted, and that does not become
    # untrue.
    chk.ok(all(r.get("taken") for r in significant_rows(port, ruin_id)),
           f"and the taken latch is unmoved by the return, the deposit "
           f"and every transfer in between "
           f"({significant_rows(port, ruin_id)})")
