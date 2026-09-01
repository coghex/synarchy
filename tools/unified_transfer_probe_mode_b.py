#!/usr/bin/env python3
"""Mode B - the queued-gesture half of the arc (#1249), for
`tools/unified_transfer_probe.py` (#2048).

One directed leg is: seed the source, send the carrier home so the
gesture really does fire from out of reach, right-click the rendered row,
activate the located `Store 1` / `Retrieve 1` entry, and then let the
REAL unit AI walk the order to the arrival that IS the commit.

What the leg proves is that the click queued a durable order naming an
EXACT instance and moved nothing, and that arrival then moved that same
instance once - never that an item ended up somewhere plausible.
"""
from __future__ import annotations

import os
import sys
import time

sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
from probelib import (poll_until, send, set_paused)
from unified_transfer_probe_support import (CARGO_LIST_ID, Checks,
                                            LEG_DEFS_B, UNIT_INV_LIST_ID)
from unified_transfer_probe_world import (ViewLedger, ai_action,
                                          assert_structured_move, calm,
                                          check_transfer,
                                          click_widget_center, close_menu,
                                          close_window, entry_states,
                                          ep_ids, event_total, find_row,
                                          find_widget, footprint_gap,
                                          knowledge, menu_labels,
                                          open_window,
                                          open_window_by_right_click,
                                          order_identity, orders,
                                          retire_leg_item,
                                          right_click_widget_center,
                                          seed_source, send_home)


def mode_b_leg(chk: Checks, port: int, ledger: ViewLedger, fp: dict, key: str,
               src, dst, def_name: str, verb: str, stager: int,
               home=None) -> bool:
    """One directed leg through #1249's queued gesture.

    Store fires from the SOURCE unit's own info row into the open
    window's active endpoint; Retrieve fires from a window row into the
    unit the shared selection rule resolves. Which unit walks is the
    other one each time, and it is the order — not the click — that
    moves anything."""
    label = f"modeB {key}"
    display = f"Probe UT {def_name}"
    # The acting unit, the endpoint the window opens on, and the row's
    # host are all decided by the VERB, never by comparing an executor's
    # uid to an endpoint id: unit ids and building ids are separate
    # spaces, so `acolyte 1` and `hold 1` compare equal.
    executor = src[1] if verb == "Store" else dst[1]
    window_ep = dst if verb == "Store" else src
    rows_id = UNIT_INV_LIST_ID if verb == "Store" else CARGO_LIST_ID

    set_paused(port, True)
    # The carrier is about to make two real trips; a starving one makes
    # neither.
    calm(port, executor)
    # `window_ep` is also the endpoint the carrier is NOT, so it is what
    # it has to start away from.
    send_home(port, executor, home, window_ep)
    seed_source(port, src, def_name, stager)
    ids = ep_ids(port, src, def_name)
    if not chk.ok(len(ids) == 1,
                  f"{label}: the source holds exactly one {def_name}, so the "
                  f"row is single-membered and its gesture names an EXACT "
                  f"instance", f"got {ids!r}"):
        return False
    iid = ids[0]
    if not chk.ok(iid not in ep_ids(port, dst, def_name),
                  f"{label}: the destination does not already hold instance "
                  f"{iid}"):
        return False

    # The acting unit IS the selection: Store reads the row off the
    # selected unit's own info panel, and Retrieve resolves its
    # executor out of the selection.
    send(port, "unit.deselectAll(); return 'ok'")
    send(port, f"return unit.select({executor})")
    time.sleep(0.7)
    if not open_window(chk, port, window_ep, label):
        return False
    ledger.record(port, f"container window on a {window_ep[0]} endpoint",
                  CARGO_LIST_ID)
    if verb == "Store":
        ledger.record(port, "unit-info inventory section", UNIT_INV_LIST_ID)

    gap = footprint_gap(port, src, dst)
    chk.ok(gap is not None and gap > 1,
           f"{label}: the two endpoints are OUT of the contract's reach when "
           f"the gesture fires (footprint Chebyshev {gap}) — Mode B requires "
           f"no adjacency, which is the whole promotion", f"gap={gap!r}")

    row = find_row(port, rows_id, def_name)
    if not chk.ok(bool(row),
                  f"{label}: the {def_name} row is located on the rendered "
                  f"{'unit-info inventory' if verb == 'Store' else 'container window'}"):
        close_window(port)
        return False
    chk.ok(row.get("instanceIds") == [iid],
           f"{label}: the rendered row stands for exactly instance {iid}",
           f"got {row.get('instanceIds')!r}")

    before_ids = {o.get("id") for o in orders(port, executor)}
    before_ev = event_total(port, executor, "unit_event", display)
    before_check = check_transfer(port, src, dst, def_name, [iid])
    right_click_widget_center(port, row)
    time.sleep(0.5)
    labels = menu_labels(port)
    chk.ok(f"{verb} 1" in labels,
           f"{label}: the rendered row menu offers '{verb} 1'",
           f"menu labels: {labels!r}")
    chk.ok(f"{verb} all" not in labels,
           f"{label}: a single-instance row shows the singular entry alone",
           f"menu labels: {labels!r}")
    entry = find_widget(port, f"{verb} 1")
    if not chk.ok(bool(entry), f"{label}: the '{verb} 1' entry is clickable"):
        close_menu(port)
        close_window(port)
        return False
    click_widget_center(port, entry)
    time.sleep(0.9)

    new = [o for o in orders(port, executor) if o.get("id") not in before_ids]
    if not chk.ok(len(new) == 1,
                  f"{label}: firing '{verb} 1' queues exactly one durable "
                  f"transfer order", f"got {[order_identity(o) for o in new]!r}"):
        close_window(port)
        return False
    ident = order_identity(new[0])
    fp.setdefault("orders", {})[f"modeB:{key}"] = ident
    chk.ok(ident["source"] == f"{src[0]}:{src[1]}"
           and ident["destination"] == f"{dst[0]}:{dst[1]}"
           and ident["items"] == [iid],
           f"{label}: the queued order runs from {src[0]}:{src[1]} to "
           f"{dst[0]}:{dst[1]} naming exactly instance {iid}",
           f"got {ident!r}")
    chk.ok(iid in ep_ids(port, src, def_name),
           f"{label}: nothing moved at click time — the source still owns "
           f"instance {iid} while the carrier has yet to walk")
    close_window(port)

    # -- the walk, and the commit that arrival IS.
    # The order's own ENTRY states are the structured record of a Mode B
    # commit, and #1253 PRUNES a terminal order on the tick that ends it
    # — so the last states observed while it still existed are the only
    # place that record can be read, and the poll captures them as it
    # goes rather than looking after the fact.
    seen_states: list = []

    def landed_and_recorded():
        for o in orders(port, executor):
            if o.get("id") == ident["id"]:
                states = entry_states(o)
                if states and (not seen_states or seen_states[-1] != states):
                    seen_states.append(states)
        return iid in ep_ids(port, dst, def_name)

    set_paused(port, False)
    landed = poll_until(300.0, landed_and_recorded, interval=1.0)
    set_paused(port, True)
    time.sleep(0.4)
    if not chk.ok(bool(landed),
                  f"{label}: the carrier walks the order and commits on "
                  f"arrival — instance {iid} reaches {dst[0]}:{dst[1]}",
                  f"carrier {executor} running {ai_action(port, executor)!r} "
                  f"at footprint Chebyshev "
                  f"{footprint_gap(port, ('unit', executor), dst)!r} from the "
                  f"destination, orders "
                  f"{[order_identity(o) for o in orders(port, executor)]!r}"):
        return False
    chk.ok(iid not in ep_ids(port, src, def_name),
           f"{label}: and left the source — the instance is in exactly one "
           f"endpoint, never both")
    chk.ok(not any(o.get("id") == ident["id"]
                   for o in orders(port, executor)),
           f"{label}: the completed order was pruned on the tick that ended it")
    chk.ok(event_total(port, executor, "unit_event", display) - before_ev == 1,
           f"{label}: the completion reached the player exactly once",
           f"delta {event_total(port, executor, 'unit_event', display) - before_ev}")
    chk.ok(bool(seen_states) and all(st == ["queued"] or st == ["in_transit"]
                                     or st == ["ready_to_commit"]
                                     or st == ["completed"]
                                     for st in seen_states)
           and seen_states[0] in (["queued"], ["in_transit"]),
           f"{label}: the order's own entry ran the lifecycle and never "
           f"reported a failure state — observed {seen_states!r}",
           f"got {seen_states!r}")
    assert_structured_move(chk, port, label, src, dst, def_name, [iid],
                           before_check, deferred_reach=True)
    fp.setdefault("legs", {})[f"modeB:{key}"] = iid
    retire_leg_item(port, dst, def_name)
    return True


def stage_mode_b(chk: Checks, port: int, ledger: ViewLedger, ids: dict,
                 fp: dict, vp: dict) -> None:
    """All six directed legs through #1249's queued gestures."""
    acolyte, mule, hold = ids["acolyte"], ids["technomule"], ids["hold"]
    U_A, U_M, B_H = ("unit", acolyte), ("unit", mule), ("building", hold)
    # Targeting is measured against where things ARE, so the world stops
    # while the camera is placed and the hit test is asked.
    set_paused(port, True)

    # The REAL right-click route, once: locate and confirm the building
    # through the production hit test, then activate the rendered
    # "Contents" row. Every later open calls the same entry point that
    # row's callback calls.
    #
    # Deliberately building-only. A UNIT endpoint's container window has
    # no world right-click route at all — `scripts/init_context_menu.lua`'s
    # unit menu offers Info / Attack / Transfer / Cancel transfer and no
    # "Contents" — so the window manager's own entry point IS the route
    # for one, and asserting a menu row that does not exist would be
    # asserting a feature this arc did not ship.
    open_window_by_right_click(chk, port, B_H, vp, "modeB building route")
    close_window(port)

    before = knowledge(port, hold).get("revealedAt")
    legs = [
        ("acolyte->storage", U_A, B_H, LEG_DEFS_B[0], "Store"),
        ("storage->acolyte", B_H, U_A, LEG_DEFS_B[1], "Retrieve"),
        ("technomule->storage", U_M, B_H, LEG_DEFS_B[2], "Store"),
        ("storage->technomule", B_H, U_M, LEG_DEFS_B[3], "Retrieve"),
        ("acolyte->technomule", U_A, U_M, LEG_DEFS_B[4], "Store"),
        ("technomule->acolyte", U_M, U_A, LEG_DEFS_B[5], "Retrieve"),
    ]
    homes = ids.get("homes") or {}
    for key, src, dst, def_name, verb in legs:
        executor = src[1] if verb == "Store" else dst[1]
        mode_b_leg(chk, port, ledger, fp, key, src, dst, def_name, verb,
                   acolyte, home=homes.get(executor))
    after = knowledge(port, hold).get("revealedAt")
    chk.ok(isinstance(after, (int, float)) and isinstance(before, (int, float))
           and after > before,
           "a COMPLETED movement refreshes the container's record (D-2's "
           "other reveal trigger)", f"{before!r} -> {after!r}")
