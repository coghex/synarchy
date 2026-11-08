#!/usr/bin/env python3
"""The two GESTURE stages `tools/transfer_context_menu_probe.py` runs after
its menu-route scenarios: the part of the retired end-to-end transfer probe
(#1255) that no Hspec group and no other probe could replace, because it
needs real Vulkan-rendered rows (`docs/archive/transfer_end_to_end_probe_coverage_map.md`).

What only this can show is that a REAL right-click on a REAL rendered
item-list row, then a REAL click on the located menu entry, moves (or
queues) one EXACT item instance between every ordered pair of the three
endpoint CLASSES - acolyte, technomule, built storage - in both player
modes. The stub-VM Hspec suites (`Transfer context menu`, `Item list
widget`) fire the same callbacks directly and never render a row.

  modeB   (#1249) - six directed legs. The gesture fires from OUT of the
          contract's reach and must QUEUE exactly one durable order naming
          the exact instance and both endpoint identities, and move
          NOTHING at click time. Walking the order to its arrival commit is
          `tools/transfer_order_probe.py`'s and `Unit transfer Lua API
          (orders, #1247)`'s, so each leg cancels its order once it has
          been read.
  modeA   (#1250/#1251) - the same six directed legs through three real
          escort sessions. The gesture commits ON THE SPOT from whichever
          pane was clicked, and the contract's structured answer is read
          before (it accepts the exact request) and after (it refuses the
          identical request on IDENTITY, `instance_missing`, not on range).

Every rendered control is located through `ui.dumpWidgets()` and the
item-list widget's own `dump()`; a world entity is located through the
production hit tests. Never a hardcoded coordinate.

The Mode A sessions' escorts are spawned BESIDE their endpoint rather than
walked there: a real walk across generated terrain is `Transfer escort
lifecycle`'s ("a distant carrier WALKS toward the endpoint's footprint")
and `transfer_order_probe.py`'s business, and what the world is unpaused
for here is the few ticks the real unit AI needs to open the pair.
"""
from __future__ import annotations

import os
import sys
import time
import traceback

sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
from probelib import (camera_state, pin_camera_to_tile, send, send_json,
                      set_paused)
from transfer_gesture_lib import (CARGO_LIST_ID, LEG_DEFS_A, LEG_DEFS_B,
                                  UNIT_INV_LIST_ID, adjacent_dry_tiles,
                                  assert_structured_move, calm,
                                  check_transfer, click_widget_center,
                                  close_menu, close_session, close_window,
                                  create_session, await_session_open, ep_ids,
                                  ep_info, event_total, find_row, find_widget,
                                  footprint_gap, load_item_defs, menu_labels,
                                  open_window,
                                  order_identity, orders, outcome_ids,
                                  outcome_states, pane_list_id,
                                  right_click_widget_center, seed_source,
                                  session_phase, spawn_unit, stack_dump)
from transfer_probe_checks import Checks


def _cancel_order(port: int, uid: int) -> None:
    """The player's own "Cancel transfer": the shipped Lua entry cancels
    every pending order of the unit AND prunes it (#1253). The raw
    `unit.cancelTransferOrder` verb alone only marks the entries cancelled."""
    send(port, "return tostring(require('scripts.unit_ai')"
               f".cancelTransferOrder({uid}))")
    time.sleep(0.4)


def mode_b_leg(chk: Checks, port: int, key: str, src, dst, def_name: str,
               verb: str, stager: int) -> bool:
    """One directed leg through #1249's queued gesture.

    Store fires from the SOURCE unit's own info row into the open window's
    endpoint; Retrieve fires from a window row into the unit the shared
    selection rule resolves. The acting unit, the endpoint the window opens
    on and the row's host are all decided by the VERB, never by comparing
    an executor's uid to an endpoint id: unit ids and building ids are
    separate spaces, so `acolyte 1` and `hold 1` compare equal."""
    label = f"modeB {key}"
    executor = src[1] if verb == "Store" else dst[1]
    window_ep = dst if verb == "Store" else src
    rows_id = UNIT_INV_LIST_ID if verb == "Store" else CARGO_LIST_ID

    set_paused(port, True)
    calm(port, executor)
    seed_source(port, src, def_name, stager)
    held = ep_ids(port, src, def_name)
    if not chk.ok(len(held) == 1,
                  f"{label}: the source holds exactly one {def_name}, so the "
                  f"row is single-membered and its gesture names an EXACT "
                  f"instance", f"got {held!r}"):
        return False
    iid = held[0]
    if not chk.ok(iid not in ep_ids(port, dst, def_name),
                  f"{label}: the destination does not already hold "
                  f"instance {iid}"):
        return False

    # The acting unit IS the selection: Store reads the row off the
    # selected unit's own info panel, and Retrieve resolves its executor
    # out of the selection.
    send(port, "unit.deselectAll(); return 'ok'")
    send(port, f"return unit.select({executor})")
    time.sleep(0.7)
    if not open_window(chk, port, window_ep, label):
        return False
    gap = footprint_gap(port, src, dst)
    chk.ok(gap is not None and gap > 1,
           f"{label}: the two endpoints are OUT of the contract's reach when "
           f"the gesture fires (footprint Chebyshev {gap}) - Mode B requires "
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
                  f"transfer order",
                  f"got {[order_identity(o) for o in new]!r}"):
        close_window(port)
        return False
    ident = order_identity(new[0])
    chk.ok(ident["source"] == f"{src[0]}:{src[1]}"
           and ident["destination"] == f"{dst[0]}:{dst[1]}"
           and ident["items"] == [iid],
           f"{label}: the queued order runs from {src[0]}:{src[1]} to "
           f"{dst[0]}:{dst[1]} naming exactly instance {iid}",
           f"got {ident!r}")
    chk.ok(iid in ep_ids(port, src, def_name)
           and iid not in ep_ids(port, dst, def_name),
           f"{label}: nothing moved at click time - the source still owns "
           f"instance {iid} while the carrier has yet to walk")
    # `checkTransfer` still REQUIRES adjacency (only order creation defers
    # it), so before the gesture the contract refuses this exact request
    # for RANGE alone: the sharpest statement of what Mode B's order defers.
    states = outcome_states(before_check)
    chk.ok(isinstance(before_check, dict)
           and outcome_ids(before_check) == [iid] and states
           and all(st == "failed:out_of_range" for st in states),
           f"{label}: before the gesture the contract refuses this exact "
           f"request for RANGE alone (`out_of_range`), which is precisely "
           f"what Mode B's order defers", f"got {before_check!r}")
    close_window(port)

    # Read, so retire it: the walk to the arrival commit is the order
    # probe's, and a live order would leave this carrier walking later.
    _cancel_order(port, executor)
    chk.ok(not any(o.get("id") == ident["id"] for o in orders(port, executor))
           and iid in ep_ids(port, src, def_name),
           f"{label}: cancelling the order retires it and still moves "
           f"nothing - the instance stays on the source")
    return True


def stage_mode_b(chk: Checks, port: int, ids: dict) -> None:
    """All six directed legs through #1249's queued gestures."""
    acolyte, mule, hold = ids["acolyte"], ids["technomule"], ids["hold"]
    U_A, U_M, B_H = ("unit", acolyte), ("unit", mule), ("building", hold)
    set_paused(port, True)
    legs = [
        ("acolyte->storage", U_A, B_H, LEG_DEFS_B[0], "Store"),
        ("storage->acolyte", B_H, U_A, LEG_DEFS_B[1], "Retrieve"),
        ("technomule->storage", U_M, B_H, LEG_DEFS_B[2], "Store"),
        ("storage->technomule", B_H, U_M, LEG_DEFS_B[3], "Retrieve"),
        ("acolyte->technomule", U_A, U_M, LEG_DEFS_B[4], "Store"),
        ("technomule->acolyte", U_M, U_A, LEG_DEFS_B[5], "Retrieve"),
    ]
    for key, src, dst, def_name, verb in legs:
        mode_b_leg(chk, port, key, src, dst, def_name, verb, acolyte)


def mode_a_leg(chk: Checks, port: int, key: str, src, dst, def_name: str,
               verb: str, pane: str, iid: int, held_uid: int) -> bool:
    """One directed leg inside an open session: a real right-click on the
    real rendered pane row, then the located entry. The direction comes
    from WHICH pane was clicked, so a swapped pair is unrepresentable."""
    label = f"modeA {key}"
    display = f"Probe UT {def_name}"
    other = "Retrieve" if verb == "Store" else "Store"
    row = find_row(port, pane_list_id(pane), def_name)
    if not chk.ok(bool(row),
                  f"{label}: the {def_name} row is located on the rendered "
                  f"{pane} pane"):
        return False
    chk.ok(row.get("instanceIds") == [iid],
           f"{label}: the rendered row stands for exactly instance {iid}",
           f"got {row.get('instanceIds')!r}")

    before_ev = event_total(port, held_uid, "unit_event", display)
    before_check = check_transfer(port, src, dst, def_name, [iid])
    right_click_widget_center(port, row)
    time.sleep(0.5)
    labels = menu_labels(port)
    chk.ok(f"{verb} 1" in labels,
           f"{label}: the {pane} pane's row menu offers '{verb} 1'",
           f"menu labels: {labels!r}")
    chk.ok(not any((l or "").startswith(other) for l in labels),
           f"{label}: and offers no '{other}' - direction comes from WHICH "
           f"pane was clicked", f"menu labels: {labels!r}")
    entry = find_widget(port, f"{verb} 1")
    if not chk.ok(bool(entry), f"{label}: the '{verb} 1' entry is clickable"):
        close_menu(port)
        return False
    click_widget_center(port, entry)
    time.sleep(1.0)

    chk.ok(iid in ep_ids(port, dst, def_name),
           f"{label}: the gesture commits ON THE SPOT - instance {iid} is at "
           f"{dst[0]}:{dst[1]} immediately, with no order and no walk",
           f"destination now holds {ep_ids(port, dst, def_name)!r}")
    chk.ok(iid not in ep_ids(port, src, def_name),
           f"{label}: and left {src[0]}:{src[1]} - the instance is in "
           f"exactly one endpoint")
    chk.ok(event_total(port, held_uid, "unit_event", display) - before_ev == 1,
           f"{label}: the commit reached the player exactly once")
    chk.ok(session_phase(port) == "open",
           f"{label}: the session stays open and repeatable after a commit",
           f"phase {session_phase(port)!r}")
    assert_structured_move(chk, port, label, src, dst, def_name, [iid],
                           before_check, deferred_reach=False)
    return True


def _spawn_beside(chk: Checks, port: int, kind: str, ep, label: str,
                  slot: int):
    """Spawn one player unit on a dry tile BESIDE an endpoint, never on
    its footprint: a unit on a building's own tile is wounded by it and
    ends a session through the incapacitated-source rule, and
    `scripts/init_context_menu.lua` tries the BUILDING menu first."""
    info = ep_info(port, ep)
    tiles = adjacent_dry_tiles(port, int(info.get("gridX", 0)),
                               int(info.get("gridY", 0)))
    if not chk.ok(len(tiles) > slot,
                  f"{label}: a dry tile beside {ep[0]}:{ep[1]}",
                  f"got {tiles!r}"):
        return None
    return spawn_unit(chk, port, kind, tiles[slot][0], tiles[slot][1], label)


def _revealed_at(port: int, bid: int):
    got = send_json(port, f"return building.getContainerKnowledge({bid})")
    return got.get("revealedAt") if isinstance(got, dict) else None


def stage_mode_a(chk: Checks, port: int, ids: dict, vp: dict) -> None:
    """The same six directed legs through three real escort sessions."""
    acolyte, mule, hold = ids["acolyte"], ids["technomule"], ids["hold"]
    U_M, B_H = ("unit", mule), ("building", hold)
    set_paused(port, True)
    hinfo = send_json(port, f"return building.getInfo({hold})") or {}
    # Settle the camera into the tile zoom band while nothing is open: a
    # session's arrival snap crossing a zoom band is one of
    # `view_teardown`'s triggers and clears the session outright.
    pin_camera_to_tile(port, int(hinfo.get("gridX", 0)),
                       int(hinfo.get("gridY", 0)), int(hinfo.get("gridZ", 0)))
    cam = camera_state(port)
    chk.ok(isinstance(cam.get("zoom"), (int, float)) and cam["zoom"] < 1.2,
           "modeA: the camera is settled in the zoomed-in tile band before "
           "any session exists, so no arrival snap crosses a band and tears "
           "one down", f"got {cam!r}")

    escort_a = _spawn_beside(chk, port, "acolyte", B_H, "modeA escort acolyte", 0)
    escort_m = _spawn_beside(chk, port, "technomule", B_H,
                             "modeA escort technomule", 1)
    escort_p = _spawn_beside(chk, port, "acolyte", U_M, "modeA unit-pair escort",
                             0)
    if None in (escort_a, escort_m, escort_p):
        return
    sessions = [
        # (session name, escort uid, destination endpoint, legs)
        ("acolyte<->storage", escort_a, B_H,
         [("acolyte->storage", ("unit", escort_a), B_H, LEG_DEFS_A[0],
           "Store", "source"),
          ("storage->acolyte", B_H, ("unit", escort_a), LEG_DEFS_A[1],
           "Retrieve", "destination")]),
        ("technomule<->storage", escort_m, B_H,
         [("technomule->storage", ("unit", escort_m), B_H, LEG_DEFS_A[2],
           "Store", "source"),
          ("storage->technomule", B_H, ("unit", escort_m), LEG_DEFS_A[3],
           "Retrieve", "destination")]),
        ("acolyte<->technomule", escort_p, U_M,
         [("acolyte->technomule", ("unit", escort_p), U_M, LEG_DEFS_A[4],
           "Store", "source"),
          ("technomule->acolyte", U_M, ("unit", escort_p), LEG_DEFS_A[5],
           "Retrieve", "destination")]),
    ]
    for name, escort, dst_ep, legs in sessions:
        label = f"modeA {name}"
        set_paused(port, True)
        calm(port, escort)
        if dst_ep[0] == "unit":
            calm(port, dst_ep[1])
        seeded = {}
        for leg_key, src, dst, def_name, verb, pane in legs:
            seed_source(port, src, def_name, acolyte)
            got = ep_ids(port, src, def_name)
            if not chk.ok(len(got) == 1,
                          f"{label}: {src[0]}:{src[1]} holds exactly one "
                          f"{def_name} for the {leg_key} leg", f"got {got!r}"):
                seeded = {}
                break
            seeded[leg_key] = got[0]
        if not seeded:
            continue
        revealed_before = _revealed_at(port, hold)
        if not create_session(chk, port, escort, dst_ep, vp, label,
                              via_menu=False):
            send(port, "require('scripts.transfer_session').clear(); return 'ok'")
            continue
        if not await_session_open(chk, port, escort, label, seconds=60.0):
            send(port, "require('scripts.transfer_session').clear(); return 'ok'")
            continue
        if dst_ep == B_H and revealed_before is not None:
            # D-2's other reveal trigger: a Mode A session OPENING on a
            # container is an interaction that refreshes what the player
            # remembers of it. The refresh itself is `Container knowledge`'s;
            # that the session's open really reaches it is only seen here.
            after = _revealed_at(port, hold)
            chk.ok(isinstance(after, (int, float))
                   and after > float(revealed_before),
                   f"{label}: a Mode A session OPENING on the container is "
                   f"what refreshes it - a fresh observation time",
                   f"{revealed_before!r} -> {after!r}")
            revealed_before = None
        d = stack_dump(port)
        lv = (d.get("levels") or [{}])[0]
        chk.ok(d.get("depth") == 1 and lv.get("kind") == "escort"
               and lv.get("paneCount") == 2,
               f"{label}: the pair is ONE stack level of the escort kind "
               f"owning two panes", f"got {d!r}")
        held = [escort]
        if dst_ep[0] == "unit":
            held.append(dst_ep[1])
            roles = {uid: send(port, "return tostring(require("
                                     "'scripts.transfer_session')"
                                     f".roleOf({uid}))").strip().strip('"')
                     for uid in held}
            chk.ok(roles.get(escort) == "source"
                   and roles.get(dst_ep[1]) == "target",
                   f"{label}: unit-to-unit holds BOTH ends - the escort as "
                   f"'source', the destination as 'target'", f"got {roles!r}")
        for leg_key, src, dst, def_name, verb, pane in legs:
            mode_a_leg(chk, port, leg_key, src, dst, def_name, verb, pane,
                       seeded[leg_key], escort)
        close_session(chk, port, held, label)


STAGES = ["setup", "modeB", "modeA"]
PRODUCER = "transfer_context_menu_probe"


def run_stages(port: int, vp: dict, ids: dict, base: str,
               chk: Checks | None = None) -> int:
    """Run the gesture stages in order against a booted world whose three
    fixtures `ids` (`acolyte`, `technomule`, `hold`) already exist, report
    them stage by stage, and answer how many checks failed.

    An operational failure - a dead engine, a socket timeout, a malformed
    console response - is a real probe failure and must name its stage like
    any other. Left to propagate it would exit non-zero with a traceback
    but NO recorded failing check. KeyboardInterrupt is deliberately still
    allowed to propagate."""
    chk = Checks(PRODUCER, STAGES) if chk is None else chk
    try:
        chk.enter("setup", "the leg item defs")
        if load_item_defs(chk, port, base):
            chk.enter("modeB", "six directed legs through the queued "
                               "gestures, each read and cancelled")
            stage_mode_b(chk, port, ids)
            chk.enter("modeA", "the same six legs through three real escort "
                               "sessions")
            stage_mode_a(chk, port, ids, vp)
    except Exception as exc:  # noqa: BLE001
        chk.ok(False, f"unexpected {type(exc).__name__} while running stage "
                      f"'{chk.stage}': {exc}")
        traceback.print_exc()
    finally:
        chk.report("the transfer gestures")
    return chk.failed
