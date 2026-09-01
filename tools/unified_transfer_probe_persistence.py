#!/usr/bin/env python3
"""The save and load stages for `tools/unified_transfer_probe.py`
(#2048).

D-3, across a real process boundary: one save taken with a Mode B order
IN FLIGHT and a Mode A session OPEN on a different pair, then a FRESH
process in which the order survives with its exact identity and completes
exactly once while the session is gone and both units it held are free.

`stage_save` runs in engine A and `stage_load` in engine B; the only
thing that crosses between them is the explicitly captured save-state
contract `stage_save` returns.
"""
from __future__ import annotations

import os
import sys
import time

sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
from probelib import (capture_request_id, poll_until, send, send_json,
                      set_paused, wait_load_published, wait_save_complete)
from unified_transfer_probe_support import (Checks, DEF_SAVE, SLOT,
                                            UNIT_INV_LIST_ID)
from unified_transfer_probe_world import (accepts_movement, adjacent_dry,
                                          ai_action, calm,
                                          click_widget_center, close_menu,
                                          close_window, ep_ids, ep_rect,
                                          event_total, find_row,
                                          find_widget, footprint_gap,
                                          load_fixtures, menu_labels,
                                          open_window, order_identity,
                                          orders,
                                          right_click_widget_center,
                                          seed_source, session, spawn_unit)
from unified_transfer_probe_mode_a import (await_session_open,
                                           create_session, stack_dump)


def stage_save(chk: Checks, port: int, ids: dict, fp: dict, vp: dict):
    """D-3: one save taken with a Mode B order IN FLIGHT and a Mode A
    session OPEN on a DIFFERENT pair.

    Order matters and is not incidental: the Mode B gesture needs the
    container window, and the escort pair IS the base level — opening a
    container at that level would replace it and end the session. So the
    order is queued first, while paused, and the session is created
    afterwards. The only unpaused window between them is the one the
    escort needs to arrive, which is why its partner is spawned already
    beside it.

    Returns the state the fresh process must re-check, or None."""
    acolyte, mule, stale = ids["acolyte"], ids["technomule"], ids["stale"]
    U_A, B_T = ("unit", acolyte), ("building", stale)
    label = "save"
    set_paused(port, True)
    calm(port, acolyte)

    # Put a real TRIP between the carrier and its destination first. The
    # session below needs the world running for the few seconds its
    # escort takes to arrive, and the carrier's order is walking during
    # exactly those seconds — from the far end of the fixture shelf it
    # cannot arrive within them, which is what leaves the order genuinely
    # in flight rather than racing the save.
    rect = ep_rect(port, B_T)
    if rect is not None:
        far = max(ids["sites"],
                  key=lambda p: max(abs(p[0] - rect[0]), abs(p[1] - rect[1])))
        send(port, f"unit.stop({acolyte}); require('scripts.unit_ai')"
                   f".commandMove({acolyte}, {far[0]}, {far[1]}); return 'ok'")
        set_paused(port, False)
        poll_until(120.0,
                   lambda: (footprint_gap(port, U_A, B_T) or 0) >= 10,
                   interval=0.5)
        set_paused(port, True)
        time.sleep(0.4)
    chk.ok((footprint_gap(port, U_A, B_T) or 0) >= 4,
           f"{label}: the carrier starts its order well away from the "
           f"destination (footprint Chebyshev "
           f"{footprint_gap(port, U_A, B_T)!r})")

    # -- the Mode B order, queued through the real gesture and left in flight.
    if not mode_b_queue_only(chk, port, U_A, B_T, DEF_SAVE, acolyte, label):
        return None
    live = [o for o in orders(port, acolyte) if not o.get("terminal")]
    if not chk.ok(len(live) == 1, f"{label}: the acolyte carries exactly one "
                                  f"live order", f"got {live!r}"):
        return None
    ident = order_identity(live[0])
    iid = ident["items"][0] if ident["items"] else None

    # -- the Mode A session, on a pair that shares nothing with that order.
    info = send_json(port, f"return unit.getInfo({mule})")
    if not isinstance(info, dict):
        chk.ok(False, f"{label}: the technomule still resolves")
        return None
    beside = adjacent_dry(port, int(info.get("gridX", 0)),
                          int(info.get("gridY", 0)))
    if not chk.ok(beside is not None,
                  f"{label}: a dry tile beside the technomule for its escort "
                  f"partner"):
        return None
    partner = spawn_unit(chk, port, "acolyte", beside[0], beside[1],
                         f"{label} escort partner")
    if partner is None:
        return None
    ids["partner"] = partner
    calm(port, mule)
    calm(port, partner)
    if not create_session(chk, port, mule, ("unit", partner), vp, label,
                          via_menu=False):
        return None
    if not await_session_open(chk, port, mule, label, seconds=90.0):
        send(port, "require('scripts.transfer_session').clear(); return 'ok'")
        return None
    sess = session(port)

    # -- and the order must still be going somewhere.
    live = [o for o in orders(port, acolyte) if not o.get("terminal")]
    if not chk.ok(len(live) == 1 and order_identity(live[0])["id"]
                  == ident["id"],
                  f"{label}: the Mode B order is still IN FLIGHT when the save "
                  f"is taken — non-terminal, nothing committed",
                  f"got {[order_identity(o) for o in orders(port, acolyte)]!r}"):
        return None
    states = [e.get("state") for e in (live[0].get("entries") or [])]
    chk.ok(all(s not in ("completed", "failed") for s in states),
           f"{label}: none of its entries has reached a terminal state",
           f"got {states!r}")
    chk.ok(iid is not None and iid in ep_ids(port, U_A, DEF_SAVE),
           f"{label}: its instance is still on the carrier")

    held = [mule, partner]
    page = send(port, "return world.getActiveWorldId()").strip().strip('"')
    rid_pre = send(port, f"return engine.saveWorld('{page}', '{SLOT}')"
                   ).strip().strip('"')
    chk.ok(rid_pre == "true", f"{label}: engine.saveWorld accepted the request",
           f"got {rid_pre!r}")
    rid = capture_request_id(port, "return engine.getSaveStatus()")
    okay, status = wait_save_complete(port, rid, seconds=180.0) if rid else (False, None)
    if not chk.ok(bool(okay), f"{label}: the save completes and is durable",
                  f"status {status!r}"):
        return None
    fp["persisted"] = {"order": ident, "held": sorted(held),
                       "sessionId": (sess or {}).get("id")}
    chk.ok(True, f"{label}: captured order #{ident['id']} in flight and a Mode "
                 f"A session holding {sorted(held)}")
    return {"order": ident, "instance": iid, "held": sorted(held),
            "carrier": acolyte, "destination": B_T, "page": page}


def mode_b_queue_only(chk: Checks, port: int, src, dst, def_name: str,
                      stager: int, label: str) -> bool:
    """Fire one real Store gesture and leave the order queued — the
    save stage's own leg, which deliberately never runs to completion."""
    seed_source(port, src, def_name, stager)
    ids = ep_ids(port, src, def_name)
    if not chk.ok(len(ids) == 1,
                  f"{label}: the carrier holds exactly one {def_name}",
                  f"got {ids!r}"):
        return False
    iid = ids[0]
    send(port, "unit.deselectAll(); return 'ok'")
    send(port, f"return unit.select({src[1]})")
    time.sleep(0.7)
    if not open_window(chk, port, dst, label):
        return False
    gap = footprint_gap(port, src, dst)
    chk.ok(gap is not None and gap > 1,
           f"{label}: the carrier is far from the destination, so the order "
           f"has a real trip left to make (footprint Chebyshev {gap})",
           f"gap={gap!r}")
    row = find_row(port, UNIT_INV_LIST_ID, def_name)
    if not chk.ok(bool(row), f"{label}: the {def_name} row is located"):
        close_window(port)
        return False
    right_click_widget_center(port, row)
    time.sleep(0.5)
    entry = find_widget(port, "Store 1")
    if not chk.ok(bool(entry), f"{label}: the 'Store 1' entry is clickable",
                  f"menu labels: {menu_labels(port)!r}"):
        close_menu(port)
        close_window(port)
        return False
    click_widget_center(port, entry)
    time.sleep(0.9)
    close_window(port)
    got = [o for o in orders(port, src[1])
           if [e.get("instanceId") for e in (o.get("entries") or [])] == [iid]]
    return chk.ok(len(got) == 1,
                  f"{label}: the gesture queued one durable order naming "
                  f"instance {iid}",
                  f"got {[order_identity(o) for o in orders(port, src[1])]!r}")


def stage_load(chk: Checks, port: int, fp: dict, base: str, state: dict,
               args) -> None:
    """A FRESH PROCESS: the Mode B order survives with its exact identity
    and completes; the Mode A session does not survive at all."""
    label = "load"
    if not chk.ok(bool(poll_until(120.0,
                                  lambda: find_widget(port, "Create World"))),
                  f"{label}: the fresh process reaches the main menu"):
        return
    # The save names the probe's own defs, so they have to exist again
    # before the load validates its content.
    if not load_fixtures(chk, port, base):
        return
    accepted = send(port, "require('scripts.main_menu').loadAndShowSave("
                          f"'{SLOT}'); return 'ok'", timeout=30.0)
    rid = capture_request_id(port, "return engine.getLoadStatus()")
    published, status = wait_load_published(port, seconds=300.0, request_id=rid)
    if not chk.ok(bool(published),
                  f"{label}: the save loads and publishes in the fresh process",
                  f"accepted={accepted!r} status={status!r}"):
        return
    ready = poll_until(90.0,
                       lambda: send(port, "return world.getActiveWorldId()"
                                    ).strip().strip('"') == state["page"],
                       interval=0.5)
    chk.ok(bool(ready), f"{label}: the loaded page is the one that was saved",
           f"got {send(port, 'return world.getActiveWorldId()')!r}")
    # Chunks queue progressively after publication.
    time.sleep(15.0)
    set_paused(port, True)

    carrier = state["carrier"]
    want = state["order"]
    got = [order_identity(o) for o in orders(port, carrier)]
    chk.ok(got == [want],
           f"{label}: the Mode B order survives with its EXACT identity — same "
           f"order id, same endpoint identities, same instance ids",
           f"got {got!r} want {[want]!r}")
    live = [o for o in orders(port, carrier) if not o.get("terminal")]
    chk.ok(len(live) == 1,
           f"{label}: and is still non-terminal, with work left to do",
           f"got {[order_identity(o) for o in orders(port, carrier)]!r}")
    chk.ok(state["instance"] in ep_ids(port, ("unit", carrier), DEF_SAVE),
           f"{label}: the carrier still holds the exact instance the order "
           f"names")

    # Ask whether the module is even loaded first: "no session" is only
    # a statement about the load if the module that would hold one is
    # live in this process. It is (the offscreen HUD boots the whole
    # script stack), and asserting it keeps the check from passing
    # vacuously in some future profile where it is not.
    loaded = send(port, "return tostring(package.loaded['scripts."
                        "transfer_session'] ~= nil)").strip().strip('"')
    chk.ok(loaded == "true",
           f"{label}: the session module is live in the fresh process, so the "
           f"next check is about the LOAD and not about an unloaded module",
           f"package.loaded = {loaded!r}")
    chk.ok(session(port) is None,
           f"{label}: the Mode A session did NOT survive — a session is "
           f"transient by design and a load must never restore one pointing "
           f"at endpoints the replacement session may not have",
           f"got {session(port)!r}")
    chk.ok(stack_dump(port).get("depth") in (0, None),
           f"{label}: and its panes are gone with it",
           f"got {stack_dump(port)!r}")
    for uid in state["held"]:
        exists = send(port, f"return tostring(unit.exists({uid}))"
                      ).strip().strip('"')
        if not chk.ok(exists == "true",
                      f"{label}: formerly held unit {uid} came back",
                      f"unit.exists = {exists!r}"):
            continue
        holds = send(port, "return tostring(require('scripts.transfer_session')"
                           f".holdsUnit({uid}))").strip().strip('"')
        chk.ok(holds == "false",
               f"{label}: formerly held unit {uid} is not held by anything")

    set_paused(port, False)
    for uid in state["held"]:
        chk.ok(accepts_movement(port, uid),
               f"{label}: formerly held unit {uid} takes an ordinary player "
               f"order again — the hold really is released, not merely absent "
               f"from a table")

    before_ev = event_total(port, carrier, "unit_event",
                            f"Probe UT {DEF_SAVE}")
    landed = poll_until(300.0,
                        lambda: state["instance"]
                        in ep_ids(port, state["destination"], DEF_SAVE),
                        interval=1.0)
    set_paused(port, True)
    time.sleep(0.5)
    if not chk.ok(bool(landed),
                  f"{label}: the resumed carrier walks the surviving order and "
                  f"commits it",
                  f"carrier running {ai_action(port, carrier)!r}, orders "
                  f"{[order_identity(o) for o in orders(port, carrier)]!r}"):
        return
    chk.ok(state["instance"] not in ep_ids(port, ("unit", carrier), DEF_SAVE),
           f"{label}: and the instance left the carrier")
    chk.ok(not orders(port, carrier),
           f"{label}: the completed order was pruned",
           f"got {[order_identity(o) for o in orders(port, carrier)]!r}")
    chk.ok(event_total(port, carrier, "unit_event", f"Probe UT {DEF_SAVE}")
           - before_ev == 1,
           f"{label}: it completed EXACTLY ONCE across the process boundary")
