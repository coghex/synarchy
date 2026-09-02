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

from .constants import PAGE
from .harness import Checks, ExpeditionState, StageAbort, assert_real_travel
from .readers import (clearance_events, current_action, event_log,
                      find_instance, find_instance_by_def, fmt_vitals,
                      ground_items, instance_by_id, inventory, is_adjacent,
                      pose, properties, significant_rows, unit_pos, vitals)


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
    arrived = walk_until_adjacent(port, prepared, foot, 420.0, r_samples)
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
    # the colony's cargo in reach — so the deposit is issued
    # only if this carrier still holds it, and the assertion is
    # on the OUTCOME either way: that exact physical instance
    # ends up in colony storage.
    held_sig = find_instance(inventory(port, prepared), sig_phys)
    if held_sig is not None:
        send(port, f"return unit.depositToCargo({prepared},"
                   f"{storage_bid},'{held_sig['defName']}',{sig_phys})")
    banked = poll_until(
        60.0,
        lambda: find_instance(
            (lambda v: v if isinstance(v, list) else [])(
                send_json(port,
                          f"return building.getStorage({storage_bid})")),
            sig_phys),
        interval=1.0)
    chk.ok(banked is not None,
           f"the guaranteed item is banked in colony storage as that "
           f"exact physical instance ({sig_phys})")
    # Taking it out of the ruin and moving it around cannot undo
    # the latch: the ruin was looted, and that does not become
    # untrue.
    chk.ok(all(r.get("taken") for r in significant_rows(port, ruin_id)),
           f"and the taken latch is unmoved by the return, the deposit "
           f"and every transfer in between "
           f"({significant_rows(port, ruin_id)})")
