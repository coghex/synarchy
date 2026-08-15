#!/usr/bin/env python3
"""Headless transfer-order execution probe (issue #1247, epic #1013
slice UIT-2B).

Gates the UNIT JOB that drives a durable transfer order (#1246's store)
through `queued -> in_transit -> ready_to_commit -> completed`: the real
`scripts/unit_ai_transfer.lua` action, running inside the real
`unit_ai.update` tick, against a real engine.

What only a real engine can prove, and what the hspec gates deliberately
do not reach:

  * `Test.Headless.Unit.Transfer` ("Unit transfer contract") drives the
    pure policy, including the #1247 `ReachDeferred` split.
  * `Test.Headless.Unit.TransferOrderApi` ("Unit transfer Lua API
    (orders, #1247)") drives the five order verbs against real manager
    refs and a real per-page store — creation at a distance, the stored
    lifecycle, arrival reconciliation, footprint-aware commit,
    exactly-once, and self-termination.

Neither of those makes a unit WALK. This probe does, and everything it
checks is downstream of that:

  1. A queued order makes its carrier walk and commit exactly once on
     arrival, arriving by the contract's FOOTPRINT rule (it approaches a
     3x1 hold from the far end, three tiles from the anchor), advancing
     the stored lifecycle as it goes, and emitting exactly ONE attributed
     `unit_event`. The wander tick never takes it away mid-order, and ten
     further seconds of ticking after completion commit nothing more.
  2. The COMMAND-TIME capacity gate refuses an order that cannot fit,
     queueing nothing and warning the player by unit and item name.
  3. A partial batch (twelve into room for eight) commits eight and
     reports the rest.
  4. The ARRIVAL gate: a hold that fills during the walk refuses with
     `became_stale/receiver_full`, and the player is told.
  5. An instance that leaves the carrier's hands mid-walk records
     `became_stale/instance_missing` while its sibling still lands.
  6. A counterpart that VANISHES mid-walk retires the order quietly —
     `became_stale/receiver_missing`, and no new warning.
  7. A blocked approach (an ocean ring the carrier cannot cross) stalls
     out and records a terminal `out_of_range` failure with a warning.
  8. A trip that keeps making progress for LONGER than the nominal
     timeout still completes — the closest-approach reset is what
     distinguishes a stall timer from a trip budget (#920).
  9. An order in flight survives a real save -> quit -> fresh restart ->
     load, and the carrier resumes and completes it.

Phases 1-8 run on an ARENA (flat, no worldgen, deterministic geometry).
Phase 9 needs a real world page: a save containing an arena page hangs
the world thread on load (#365), so it runs on its own pair of engines
against a small generated world.

Usage:
  python3 tools/transfer_order_probe.py
  python3 tools/transfer_order_probe.py --port 9271 --seed 42 --size 48

Exit 0 = every check passed.
"""
from __future__ import annotations

import argparse
import glob
import os
import shutil
import sys
import tempfile
import time
from pathlib import Path

from probelib import (boot, clear_find_water, init_arena, init_world,
                      poll_until, quit_engine, send, send_json,
                      wait_load_published, load_ai_stack)

REPO = Path(__file__).resolve().parent.parent
SLOT = "probe_transfer_order_slot"

# Throwaway content, registered at runtime and never shipped.
#
# The ingot is deliberately FEATHERLIGHT. An acolyte's carrying_capacity
# is body-derived and lands around 23 kg (as low as ~11 kg on a weak
# roll) against a ~12 kg starting kit, and load above a quarter of it
# slows the unit down (movement_speed.encumbranceMultiplier) — so a
# heavy fixture item would not test the transfer job, it would test how
# an over-encumbered acolyte crawls. At 0.25 kg, twelve of them are 3 kg
# and change nothing about how the carrier walks.
#
# The hold capacities are then chosen against that weight so each
# scenario is EXACT rather than approximate: 2.0 kg is room for precisely
# eight ingots (D-1's twelve-into-eight), and 0.125 kg is room for none.
#
# Every one of those numbers is a BINARY fraction on purpose. Capacity is
# a 32-bit Float and the gate is `load + weight <= capacity`, summed one
# accepted item at a time, so a decimal-friendly fixture (0.2 kg into
# 1.6 kg) lands a few ulps over and fits SEVEN — a fixture arithmetic
# artefact that reads exactly like a partial-batch bug. 0.25 and 2.0 are
# exact, and eight of the former sum to precisely the latter.
DEFS_YAML_ITEMS = """\
items:
  - name: "probe_ingot"
    display_name: "Probe Ingot"
    sprite: "assets/textures/items/material/bar_steel.png"
    weight: 0.25
    category: Materials
"""

DEFS_YAML_BUILDINGS = """\
buildings:
  - name: "probe_wide_hold"
    display_name: "Probe Wide Hold"
    category: "Cargo"
    description: "Throwaway #1247 test fixture - not shipped content."
    sprite: "assets/textures/buildings/cargo_hold_S/default.png"
    tile_size: { x: 3, y: 1 }
    placement: "flat_ground"
    race: "acolyte_cult"
    build_work: 0.0
    storage_capacity: 200.0
  - name: "probe_small_hold"
    display_name: "Probe Small Hold"
    category: "Cargo"
    description: "Throwaway #1247 test fixture - not shipped content."
    sprite: "assets/textures/buildings/cargo_hold_S/default.png"
    tile_size: { x: 1, y: 1 }
    placement: "flat_ground"
    race: "acolyte_cult"
    build_work: 0.0
    storage_capacity: 200.0
  - name: "probe_partial_hold"
    display_name: "Probe Partial Hold"
    category: "Cargo"
    description: "Throwaway #1247 test fixture - not shipped content."
    sprite: "assets/textures/buildings/cargo_hold_S/default.png"
    tile_size: { x: 1, y: 1 }
    placement: "flat_ground"
    race: "acolyte_cult"
    build_work: 0.0
    storage_capacity: 2.0
  - name: "probe_tiny_hold"
    display_name: "Probe Tiny Hold"
    category: "Cargo"
    description: "Throwaway #1247 test fixture - not shipped content."
    sprite: "assets/textures/buildings/cargo_hold_S/default.png"
    tile_size: { x: 1, y: 1 }
    placement: "flat_ground"
    race: "acolyte_cult"
    build_work: 0.0
    storage_capacity: 0.125
"""


# THE ARENA IS FINITE, and asking for a chunk outside it is fatal: an
# arena page is exactly (2*arenaRadius+1)^2 = 5x5 chunks of 16 tiles
# (World.Generate.Arena), all stamped eagerly at init, so tiles
# -32..47 on both axes exist and NOTHING else does. A
# world.loadChunksInRegion reaching past that falls through to the real
# generator, which has no plates for an arena's synthetic gen params and
# CRASHES THE WORLD THREAD ("twoNearestPlates: no plates") — the engine
# then dies mid-probe with a bare connection refused. So: no chunk
# preload at all (init already materialised every chunk), and every
# coordinate below stays inside the box.
ARENA_MIN, ARENA_MAX = -32, 47

# One y-lane per phase, ten tiles apart so a finished phase's leftovers
# cannot drift into the next one (a carrier is destroyed when its phase
# ends; wander_radius is 5). Every hold is anchored at the WEST end of
# its lane and every carrier starts east of it, which is what makes the
# 3x1 hold in phase 1 get approached from its FAR end.
LANES = {1: -28, 2: -18, 3: -8, 4: 2, 5: 12, 6: 22, 7: 32, 8: 42}
HOLD_X = -28


class Checks:
    def __init__(self) -> None:
        self.failed = 0

    def ok(self, cond: bool, label: str, detail: str = "") -> bool:
        print(f"  [{'PASS' if cond else 'FAIL'}] {label}"
              + (f"  ({detail})" if detail and not cond else ""))
        if not cond:
            self.failed += 1
        return bool(cond)


# --------------------------------------------------------------------------
# Engine helpers
# --------------------------------------------------------------------------

def bootstrap_defs(port: int, tmp: str) -> None:
    for pattern, fn in [
        ("data/substances/*.yaml", "engine.loadSubstanceYaml"),
        ("data/items/*.yaml",      "engine.loadItemYaml"),
        ("data/equipment/*.yaml",  "engine.loadEquipmentYaml"),
        ("data/materials/*.yaml",  "engine.loadMaterialYaml"),
        ("data/units/*.yaml",      "engine.loadUnitYaml"),
        ("data/buildings/*.yaml",  "engine.loadBuildingYaml"),
    ]:
        for path in sorted(glob.glob(pattern)):
            send(port, f"{fn}('{path}'); return 'ok'")
    items = os.path.join(tmp, "probe_transfer_items.yaml")
    builds = os.path.join(tmp, "probe_transfer_buildings.yaml")
    with open(items, "w", encoding="utf-8") as f:
        f.write(DEFS_YAML_ITEMS)
    with open(builds, "w", encoding="utf-8") as f:
        f.write(DEFS_YAML_BUILDINGS)
    send(port, f"engine.loadItemYaml('{items}'); return 'ok'")
    send(port, f"engine.loadBuildingYaml('{builds}'); return 'ok'")


def spawn_hold(port: int, def_name: str, gx: int, gy: int) -> int | None:
    """building.spawn + wait for the queued BuildingSpawn to materialise."""
    raw = send(port, f"return building.spawn('{def_name}', {gx}, {gy})")
    try:
        bid = int(float(raw))
    except (TypeError, ValueError):
        return None
    if bid < 0:
        return None
    ok = poll_until(20.0, lambda: send(
        port, f"return tostring(building.getStorageCapacity({bid}))") not in
        ("nil", "0.0", "0"))
    return bid if ok is not None else None


def spawn_carrier(port: int, gx: float, gy: float) -> int:
    raw = send(port, f"return unit.spawn('acolyte', {gx}, {gy}, nil, 'player')")
    uid = int(float(raw))
    clear_find_water(port, uid)
    return uid


def give(port: int, uid: int, def_name: str, n: int) -> None:
    for _ in range(n):
        send(port, f"unit.addItem({uid}, '{def_name}'); return 'ok'")


def loose(port: int, uid: int) -> list:
    """A carrier's loose inventory as the transfer contract projects it."""
    info = send_json(
        port, f"return unit.transferEndpointInfo({{kind='unit', id={uid}}})")
    return (info or {}).get("contents") or []


def ingots(port: int, uid: int) -> list:
    """Only the probe's OWN fixture items.

    An acolyte spawns with a real starting kit (pick, shovel, canteen,
    rations), so "the carrier's loose inventory" is never just what this
    probe handed it. Every assertion and every request below is about the
    ingots specifically — addressing the kit by accident would move a
    canteen and pass anyway."""
    return [i for i in loose(port, uid) if i.get("defName") == "probe_ingot"]


def stored(port: int, bid: int) -> list:
    info = send_json(
        port, f"return unit.transferEndpointInfo({{kind='building', id={bid}}})")
    return (info or {}).get("contents") or []


def order_request(port: int, uid: int, bid: int, n: int) -> str:
    """A Lua request literal moving the carrier's first `n` loose items
    into `bid`, addressed by EXACT instance id (never by def name)."""
    items = ", ".join(
        "{ instanceId = %d, defName = '%s' }" % (it["instanceId"], it["defName"])
        for it in ingots(port, uid)[:n])
    return ("{ source = { kind = 'unit', id = %d }, "
            "destination = { kind = 'building', id = %d }, "
            "items = { %s } }" % (uid, bid, items))


def command_order(port: int, uid: int, request: str) -> str:
    return send(port, "return tostring(require('scripts.unit_ai')"
                      f".commandTransferOrder({uid}, {request}))")


def orders(port: int, uid: int) -> list:
    data = send_json(port, f"return unit.getTransferOrders({uid})")
    return data if isinstance(data, list) else []


def entry_states(order: dict) -> list:
    out = []
    for e in order.get("entries") or []:
        s = e.get("state")
        if e.get("reason"):
            s += ":" + e["reason"]
        if e.get("cause"):
            s += "/" + e["cause"]
        out.append(s)
    return out


def event_log(port: int) -> list:
    data = send_json(port, "return engine.getEventLog()")
    return data if isinstance(data, list) else []


def events_since(port: int, mark: int, category: str, uid: int) -> list:
    return [e for e in event_log(port)[mark:]
            if e.get("category") == category and e.get("uid") == uid]


def transfer_events(port: int, mark: int, uid: int) -> list:
    """Completion events THIS job emitted, not every ambient one.

    `unit_event` is the shared low-priority per-unit category ("dropped
    items, minor incidents"), so an idle acolyte files them for reasons
    that have nothing to do with a transfer. Asserting on the whole
    category would make "the order completed exactly once" fail
    whenever the carrier happened to do something else in the window —
    a flake that reads as an exactly-once violation while the hold's
    contents say otherwise."""
    return [e for e in events_since(port, mark, "unit_event", uid)
            if "transferred" in e.get("text", "")]


def current_action(port: int, uid: int) -> str:
    return send(port, "local s = require('scripts.unit_ai').getState"
                      f"({uid}); return tostring(s and s.currentAction)")


def unit_tile(port: int, uid: int) -> tuple[float, float] | None:
    info = send_json(port, f"return unit.getInfo({uid})")
    if not isinstance(info, dict):
        return None
    return info.get("gridX"), info.get("gridY")


def wait_terminal(port: int, uid: int, oid: int, seconds: float):
    """Poll until the order reaches a terminal state; return it (or None)."""
    def look():
        for o in orders(port, uid):
            if o.get("id") == oid and o.get("terminal"):
                return o
        return None
    return poll_until(seconds, look, interval=0.5)


def wait_state(port: int, uid: int, oid: int, state: str, seconds: float):
    def look():
        for o in orders(port, uid):
            if o.get("id") == oid and state in entry_states(o):
                return o
        return None
    return poll_until(seconds, look, interval=0.25)


# --------------------------------------------------------------------------
# Arena phases
# --------------------------------------------------------------------------

def phase_walk_and_commit(port: int, chk: Checks) -> None:
    """1. The whole happy path, plus the three properties that only a real
    walk can show: the footprint arrival, the lock, and exactly-once."""
    print("\n--- 1. walk -> arrive on the footprint -> commit exactly once ---")
    # The hold occupies (HOLD_X .. HOLD_X+2, lane); the carrier starts
    # EAST of it, so it arrives beside HOLD_X+3 — one tile from the
    # RECTANGLE and three from the ANCHOR. An anchor-distance arrival
    # rule would walk it straight through the building.
    lane = LANES[1]
    bid = spawn_hold(port, "probe_wide_hold", HOLD_X, lane)
    if not chk.ok(bid is not None, "3x1 probe hold spawned"):
        return
    uid = spawn_carrier(port, HOLD_X + 14, lane)
    give(port, uid, "probe_ingot", 2)
    mark = len(event_log(port))

    request = order_request(port, uid, bid, 2)
    oid_raw = command_order(port, uid, request)
    try:
        oid = int(float(oid_raw))
    except (TypeError, ValueError):
        chk.ok(False, "commandTransferOrder returned an order id", oid_raw)
        return
    chk.ok(oid > 0, "commandTransferOrder queued an order", oid_raw)

    # The lifecycle really is walked through, not skipped: in_transit
    # must be observable BEFORE the commit.
    chk.ok(wait_state(port, uid, oid, "in_transit", 20.0) is not None,
           "the order reaches in_transit while the carrier walks")

    # Sample the running action for the whole trip. `transfer_order` is
    # the in-progress lock; `wander` winning even once would mean the
    # ambient tick took the carrier off its order.
    stole = []
    def sample_until_done():
        for o in orders(port, uid):
            if o.get("id") == oid and o.get("terminal"):
                return o
        act = current_action(port, uid)
        if act == "wander":
            stole.append(act)
        return None
    done = poll_until(90.0, sample_until_done, interval=0.4)
    chk.ok(done is not None, "the order completes")
    chk.ok(not stole, "the wander tick never took the carrier mid-order",
           f"{len(stole)} wander sample(s)")
    if done is None:
        return
    chk.ok(entry_states(done) == ["completed", "completed"],
           "both entries recorded completed", str(entry_states(done)))

    pos = unit_tile(port, uid)
    if pos and pos[0] is not None:
        anchor_cheb = max(abs(pos[0] - HOLD_X), abs(pos[1] - lane))
        chk.ok(anchor_cheb > 1.0,
               "arrival was footprint-aware (further than 1 from the ANCHOR "
               "of the 3x1 hold)", f"carrier at {pos}, anchor cheb {anchor_cheb}")

    held = stored(port, bid)
    chk.ok(len(held) == 2, "both instances are in the hold", str(held))
    chk.ok(ingots(port, uid) == [],
           "the carrier is no longer holding either ingot")

    # count, too: the event log DEDUPES identical consecutive entries
    # into one row carrying a count, so "one row" alone would not rule
    # out the same completion having been emitted twice.
    evs = transfer_events(port, mark, uid)
    chk.ok(len(evs) == 1 and evs[0].get("count", 1) == 1,
           "exactly ONE attributed unit_event for the completed order",
           str(evs))
    if evs:
        chk.ok("Probe Ingot" in evs[0].get("text", ""),
               "the completion event names the item", evs[0].get("text", ""))

    # Exactly-once, after the fact: keep the AI ticking and prove the
    # terminal order is never re-run.
    before = len(event_log(port))
    time.sleep(10.0)
    chk.ok(len(stored(port, bid)) == 2,
           "ten more seconds of ticking commit nothing further")
    chk.ok(not transfer_events(port, before, uid),
           "and emit no second completion event",
           str(transfer_events(port, before, uid)))
    send(port, f"unit.destroy({uid}); return 'ok'")


def phase_command_time_refusal(port: int, chk: Checks) -> None:
    """2. Refused before anybody walks anywhere."""
    print("\n--- 2. command-time capacity refusal ---")
    lane = LANES[2]
    bid = spawn_hold(port, "probe_tiny_hold", HOLD_X, lane)
    if not chk.ok(bid is not None, "no-room probe hold spawned"):
        return
    uid = spawn_carrier(port, HOLD_X + 14, lane)
    give(port, uid, "probe_ingot", 1)
    mark = len(event_log(port))
    res = command_order(port, uid, order_request(port, uid, bid, 1))
    chk.ok(res == "false", "the order is REFUSED up front", res)
    chk.ok(orders(port, uid) == [], "nothing was queued")
    warns = events_since(port, mark, "unit_warning", uid)
    chk.ok(bool(warns), "the player is warned", str(warns))
    if warns:
        text = warns[0].get("text", "")
        chk.ok("Probe Ingot" in text, "the warning names the item", text)
    send(port, f"unit.destroy({uid}); return 'ok'")


def phase_partial_batch(port: int, chk: Checks) -> None:
    """3. D-1: twelve into room for eight."""
    print("\n--- 3. partial batch: twelve into room for eight ---")
    lane = LANES[3]
    bid = spawn_hold(port, "probe_partial_hold", HOLD_X, lane)
    if not chk.ok(bid is not None, "room-for-eight probe hold spawned"):
        return
    uid = spawn_carrier(port, HOLD_X + 12, lane)
    give(port, uid, "probe_ingot", 12)
    mark = len(event_log(port))
    oid_raw = command_order(port, uid, order_request(port, uid, bid, 12))
    try:
        oid = int(float(oid_raw))
    except (TypeError, ValueError):
        chk.ok(False, "the partial order was accepted", oid_raw)
        return
    chk.ok(oid > 0, "a partial batch is an ACCEPTANCE, not a refusal", oid_raw)
    queued = [o for o in orders(port, uid) if o.get("id") == oid]
    if chk.ok(bool(queued), "the order is in the store"):
        states = entry_states(queued[0])
        # PENDING, not literally "queued": the carrier's very first tick
        # advances the order to in_transit, so a read here races it. What
        # D-1 actually claims is that eight entries made it into the trip
        # and four did not — which the order's own non-terminal count
        # states without depending on when this read lands.
        chk.ok(queued[0].get("pending") == 8,
               "exactly eight entries are pending the trip", str(states))
        chk.ok(states.count("failed:receiver_full") == 4,
               "the four that did not fit are terminal with their reason",
               str(states))
    chk.ok(bool(events_since(port, mark, "unit_warning", uid)),
           "the player is told what did not fit")
    done = wait_terminal(port, uid, oid, 90.0)
    if chk.ok(done is not None, "the order completes"):
        states = entry_states(done)
        chk.ok(states.count("completed") == 8, "eight moved", str(states))
        chk.ok(len(stored(port, bid)) == 8, "eight are in the hold")
        chk.ok(len(ingots(port, uid)) == 4, "four stayed with the carrier")
    send(port, f"unit.destroy({uid}); return 'ok'")


def phase_arrival_refusal(port: int, chk: Checks) -> None:
    """4. The hold fills during the walk."""
    print("\n--- 4. arrival-time capacity refusal ---")
    lane = LANES[4]
    bid = spawn_hold(port, "probe_partial_hold", HOLD_X, lane)
    if not chk.ok(bid is not None, "room-for-eight probe hold spawned"):
        return
    uid = spawn_carrier(port, HOLD_X + 14, lane)
    give(port, uid, "probe_ingot", 1)
    oid_raw = command_order(port, uid, order_request(port, uid, bid, 1))
    try:
        oid = int(float(oid_raw))
    except (TypeError, ValueError):
        chk.ok(False, "the order was accepted", oid_raw)
        return
    chk.ok(wait_state(port, uid, oid, "in_transit", 20.0) is not None,
           "the carrier sets off")
    # Fill the hold behind its back, through a second carrier standing on
    # top of it — a real deposit, not a poke at engine state.
    filler = spawn_carrier(port, HOLD_X + 1, lane + 1)
    give(port, filler, "probe_ingot", 8)
    for _ in range(8):
        send(port, f"unit.depositToCargo({filler}, {bid}, 'probe_ingot'); "
                   "return 'ok'")
    chk.ok(len(stored(port, bid)) == 8, "the hold is full when the carrier arrives")
    mark = len(event_log(port))
    done = wait_terminal(port, uid, oid, 90.0)
    if chk.ok(done is not None, "the order terminalises"):
        chk.ok(entry_states(done) == ["failed:became_stale/receiver_full"],
               "the refusal is recorded as became_stale with its cause",
               str(entry_states(done)))
    chk.ok(len(ingots(port, uid)) == 1,
           "nothing moved — no item half-moved")
    chk.ok(bool(events_since(port, mark, "unit_warning", uid)),
           "the player is warned at arrival too")
    send(port, f"unit.destroy({uid}); return 'ok'")
    send(port, f"unit.destroy({filler}); return 'ok'")


def phase_stale_instance(port: int, chk: Checks) -> None:
    """5. An instance leaves the carrier's hands mid-walk."""
    print("\n--- 5. an instance that vanished during the walk ---")
    lane = LANES[5]
    bid = spawn_hold(port, "probe_wide_hold", HOLD_X, lane)
    if not chk.ok(bid is not None, "3x1 probe hold spawned"):
        return
    uid = spawn_carrier(port, HOLD_X + 14, lane)
    give(port, uid, "probe_ingot", 2)
    oid_raw = command_order(port, uid, order_request(port, uid, bid, 2))
    try:
        oid = int(float(oid_raw))
    except (TypeError, ValueError):
        chk.ok(False, "the order was accepted", oid_raw)
        return
    chk.ok(wait_state(port, uid, oid, "in_transit", 20.0) is not None,
           "the carrier sets off")
    send(port, f"unit.removeItem({uid}, 'probe_ingot'); return 'ok'")
    chk.ok(len(ingots(port, uid)) == 1, "one of the two instances is gone")
    done = wait_terminal(port, uid, oid, 90.0)
    if chk.ok(done is not None, "the order terminalises"):
        states = entry_states(done)
        chk.ok(states.count("failed:became_stale/instance_missing") == 1,
               "the missing instance is recorded with its structured cause",
               str(states))
        chk.ok(states.count("completed") == 1,
               "its sibling still lands — one refusal neither rolls back nor "
               "blocks another item", str(states))
    chk.ok(len(stored(port, bid)) == 1, "exactly one instance reached the hold")
    send(port, f"unit.destroy({uid}); return 'ok'")


def phase_vanished_counterpart(port: int, chk: Checks) -> None:
    """6. The destination is demolished mid-walk: retire QUIETLY."""
    print("\n--- 6. a counterpart that vanished retires quietly ---")
    lane = LANES[6]
    bid = spawn_hold(port, "probe_wide_hold", HOLD_X, lane)
    if not chk.ok(bid is not None, "3x1 probe hold spawned"):
        return
    uid = spawn_carrier(port, HOLD_X + 14, lane)
    give(port, uid, "probe_ingot", 1)
    oid_raw = command_order(port, uid, order_request(port, uid, bid, 1))
    try:
        oid = int(float(oid_raw))
    except (TypeError, ValueError):
        chk.ok(False, "the order was accepted", oid_raw)
        return
    chk.ok(wait_state(port, uid, oid, "in_transit", 20.0) is not None,
           "the carrier sets off")
    mark = len(event_log(port))
    send(port, f"building.destroy({bid}); return 'ok'")
    done = wait_terminal(port, uid, oid, 60.0)
    if chk.ok(done is not None, "the order retires"):
        chk.ok(entry_states(done) == ["failed:became_stale/receiver_missing"],
               "recorded as became_stale, naming the side that went missing",
               str(entry_states(done)))
    noisy = [e for e in events_since(port, mark, "unit_warning", uid)
             if "transfer" in e.get("text", "").lower()
             or "Probe Ingot" in e.get("text", "")]
    chk.ok(not noisy,
           "and NO transfer warning — a demolished destination is attrition, "
           "not a failure worth interrupting the player over", str(noisy))
    chk.ok(len(ingots(port, uid)) == 1, "the carrier keeps its cargo")
    send(port, f"unit.destroy({uid}); return 'ok'")


def phase_blocked_approach(port: int, chk: Checks) -> None:
    """7. A blocked approach stalls out and records a terminal failure."""
    print("\n--- 7. a blocked approach stalls out (~60 s) ---")
    lane = LANES[7]
    bid = spawn_hold(port, "probe_small_hold", HOLD_X, lane)
    if not chk.ok(bid is not None, "1x1 probe hold with ROOM spawned"):
        return
    # A closed ocean ring at Chebyshev 2 from the hold. Ocean is
    # impassable (movement_arena's own note), so the carrier can never
    # reach Chebyshev 1 of the hold however long it tries — while the
    # hold itself stays perfectly alive, which is what separates this
    # from phase 6.
    ring = ("local p='move_test'; local bx,by=%d,%d; for d=-2,2 do "
            "world.setFluidTile(p, bx+d, by-2, 'ocean'); "
            "world.setFluidTile(p, bx+d, by+2, 'ocean'); "
            "world.setFluidTile(p, bx-2, by+d, 'ocean'); "
            "world.setFluidTile(p, bx+2, by+d, 'ocean'); end; return 'ok'"
            % (HOLD_X, lane))
    send(port, ring)
    uid = spawn_carrier(port, HOLD_X + 14, lane)
    give(port, uid, "probe_ingot", 1)
    mark = len(event_log(port))
    started = time.time()
    oid_raw = command_order(port, uid, order_request(port, uid, bid, 1))
    try:
        oid = int(float(oid_raw))
    except (TypeError, ValueError):
        chk.ok(False, "the order was accepted", oid_raw)
        return
    done = wait_terminal(port, uid, oid, 180.0)
    elapsed = time.time() - started
    if chk.ok(done is not None, f"the order gives up (after {elapsed:.0f} s)"):
        chk.ok(entry_states(done) == ["failed:out_of_range"],
               "recorded as a terminal out_of_range failure — the carrier "
               "never got in range", str(entry_states(done)))
    chk.ok(elapsed > 30.0,
           "it persisted rather than giving up immediately", f"{elapsed:.0f} s")
    reach = [e for e in events_since(port, mark, "unit_warning", uid)
             if "transfer destination" in e.get("text", "")]
    # Specifically THIS warning: unit_ai.lua's own stuck-walk watchdog
    # also files a unit_warning while the carrier bumps against the
    # ocean, so "some warning appeared" would pass without the order
    # ever having reported anything.
    chk.ok(bool(reach),
           "the player is warned that the transfer destination could not "
           "be reached", str(events_since(port, mark, "unit_warning", uid)))
    chk.ok(len(ingots(port, uid)) == 1, "the cargo stayed put")
    send(port, f"unit.destroy({uid}); return 'ok'")


def phase_long_trip(port: int, chk: Checks) -> None:
    """8. A progressing trip longer than the nominal timeout completes.

    This is the property #920 named and that a from-issue trip budget
    breaks: the deadline resets on every new closest approach, so only a
    carrier that stops getting closer expires. The assertion has to be
    that the trip REALLY outlasted the budget, or it proves nothing.
    """
    print("\n--- 8. a long but progressing trip outlives the 60 s budget ---")
    lane = LANES[8]
    bid = spawn_hold(port, "probe_wide_hold", HOLD_X, lane)
    if not chk.ok(bid is not None, "3x1 probe hold spawned"):
        return
    # ~70 tiles at an acolyte's comfort pace (~0.5 tiles/s under a light
    # load) is a little over two minutes — comfortably past the 60 s
    # stall budget, and still inside the arena's east edge.
    uid = spawn_carrier(port, ARENA_MAX - 3, lane)
    give(port, uid, "probe_ingot", 1)
    started = time.time()
    oid_raw = command_order(port, uid, order_request(port, uid, bid, 1))
    try:
        oid = int(float(oid_raw))
    except (TypeError, ValueError):
        chk.ok(False, "the order was accepted", oid_raw)
        return
    done = wait_terminal(port, uid, oid, 400.0)
    elapsed = time.time() - started
    if chk.ok(done is not None, f"the long haul completes (after {elapsed:.0f} s)"):
        chk.ok(entry_states(done) == ["completed"],
               "and commits on arrival", str(entry_states(done)))
    chk.ok(elapsed > 60.0,
           "the trip really did outlast the 60 s stall budget — a total-trip "
           "budget would have abandoned it", f"{elapsed:.0f} s")
    send(port, f"unit.destroy({uid}); return 'ok'")


# --------------------------------------------------------------------------
# Save/load phase (real world page — never an arena, #365)
# --------------------------------------------------------------------------

def make_isolated_root(base: str) -> str:
    root = os.path.join(base, "root")
    os.makedirs(root, exist_ok=True)
    for family in ("scripts", "assets", "data", "config"):
        target = os.path.join(root, family)
        if not os.path.exists(target):
            os.symlink(os.path.join(REPO, family), target)
    os.makedirs(os.path.join(root, "saves"), exist_ok=True)
    return root


def find_hold_site(port: int) -> tuple[int, int, int] | None:
    """A real-terrain site for the 3x1 hold plus a walkable corridor east.

    Real worldgen guarantees neither. This scans the whole LOADED box for
    an anchor whose own 3 tiles share one elevation and are dry, and
    whose next thirteen tiles east are dry and within +/-2 z of it — so
    the carrier has somewhere to stand twelve tiles away AND a corridor
    it can actually walk down. Without the corridor check a site can pass
    and then strand the carrier behind a cliff or a lake, which reads as
    "the transfer job stalled" rather than as a bad fixture.
    """
    lua = (
        "local function f() "
        "for gy=-30,44 do for gx=-30,30 do "
        "  local z0 = world.getTerrainAt(gx, gy) "
        "  local ok = z0 ~= nil "
        "  if ok then for d=0,2 do local z=world.getTerrainAt(gx+d,gy) "
        "    if (not z) or z~=z0 or world.getFluidAt(gx+d,gy) then ok=false end end end "
        "  if ok then for d=3,15 do local z=world.getTerrainAt(gx+d,gy) "
        "    if (not z) or world.getFluidAt(gx+d,gy) or math.abs(z-z0)>2 "
        "    then ok=false end end end "
        "  if ok then return gx..','..gy..','..z0 end "
        "end end return 'none' end return f()"
    )
    for _ in range(8):
        res = send(port, lua, timeout=60.0).strip('"')
        if res and res != "none" and res.count(",") == 2:
            gx, gy, z = (int(v) for v in res.split(","))
            return gx, gy, z
        time.sleep(0.75)
    return None


def phase_save_load(chk: Checks, root: str, tmp: str, port: int,
                    seed: int, size: int) -> None:
    print("\n--- 9. an order in flight survives save -> restart -> load ---")
    log_b = os.path.join(tmp, "engineB.log")
    log_c = os.path.join(tmp, "engineC.log")
    proc = boot(port, log=log_b, args=["--resource-root", root],
                ready_timeout=180)
    uid = bid = oid = None
    try:
        bootstrap_defs(port, tmp)
        load_ai_stack(port)
        init_world(port, name="probe_world", seed=seed, size=size, plates=3)
        send(port, "return world.loadChunksInRegion(-2,-2,2,2)")
        send(port, "return world.waitForChunks(180)", timeout=185)
        site = find_hold_site(port)
        if not chk.ok(site is not None,
                      "found flat dry ground with a walkable corridor east"):
            return
        gx, gy, _z = site
        bid = spawn_hold(port, "probe_wide_hold", gx, gy)
        if not chk.ok(bid is not None, "3x1 probe hold spawned on real terrain"):
            return
        uid = spawn_carrier(port, gx + 12, gy)
        give(port, uid, "probe_ingot", 1)
        oid_raw = command_order(port, uid, order_request(port, uid, bid, 1))
        try:
            oid = int(float(oid_raw))
        except (TypeError, ValueError):
            chk.ok(False, "the order was accepted", oid_raw)
            return
        chk.ok(wait_state(port, uid, oid, "in_transit", 30.0) is not None,
               "the order is IN FLIGHT when the save is taken")
        saved = send(port, f"return engine.saveWorld('probe_world', '{SLOT}')")
        chk.ok(saved.strip() == "true", "engine.saveWorld accepted", saved)
        save_file = os.path.join(root, "saves", SLOT, "world.synworld")
        chk.ok(poll_until(30.0, lambda: os.path.exists(save_file) or None)
               is not None, "the save file appeared")
    finally:
        quit_engine(port, proc)

    proc = boot(port, log=log_c, args=["--resource-root", root],
                ready_timeout=180)
    try:
        bootstrap_defs(port, tmp)
        load_ai_stack(port)
        loaded = send(port, f"return engine.loadSave('{SLOT}')")
        chk.ok(loaded.strip() == "true", "engine.loadSave accepted", loaded)
        published, status = wait_load_published(port)
        if not chk.ok(published, f"the load published ({status})"):
            return
        restored = [o for o in orders(port, uid) if o.get("id") == oid]
        if chk.ok(bool(restored),
                  "the in-flight order came back in a FRESH process"):
            chk.ok(not restored[0].get("terminal"),
                   "still non-terminal, so there is a trip left to resume",
                   str(entry_states(restored[0])))
        send(port, "engine.setPaused(false); return 'ok'")
        done = wait_terminal(port, uid, oid, 180.0)
        if chk.ok(done is not None, "the carrier resumes and the order completes"):
            chk.ok(entry_states(done) == ["completed"],
                   "committed after the round trip", str(entry_states(done)))
        chk.ok(len(stored(port, bid)) == 1, "the item is in the hold")
    finally:
        quit_engine(port, proc)


# --------------------------------------------------------------------------

def main() -> int:
    ap = argparse.ArgumentParser(
        description=__doc__,
        formatter_class=argparse.RawDescriptionHelpFormatter)
    ap.add_argument("--port", type=int, default=9271)
    # Seed 7 is not arbitrary: phase 9 needs a flat dry 3x1 site with a
    # walkable corridor beside it INSIDE the region it loads, and seed 42
    # (this repo's usual default) drops the origin in open ocean at
    # worldSize 48. find_hold_site still searches rather than trusting
    # it, so another seed that happens to work is fine.
    ap.add_argument("--seed", type=int, default=7)
    ap.add_argument("--size", type=int, default=48)
    ap.add_argument("--only", default="",
                    help="comma-separated phase numbers (default: all)")
    args = ap.parse_args()

    all_phases = set(range(1, 10))
    wanted = {int(p) for p in args.only.split(",") if p.strip()} or all_phases
    # A selection that names no real phase must FAIL, not report a clean
    # run of nothing: an exit-0 probe that executed zero checks is
    # indistinguishable from one that passed.
    if not wanted & all_phases:
        print(f"no such phase(s): {sorted(wanted)} (valid: 1-9)")
        return 2
    tmp = tempfile.mkdtemp(prefix="synarchy_transfer_order_probe_")
    chk = Checks()
    proc = None
    try:
        if wanted & set(range(1, 9)):
            print(f"== arena engine (port {args.port}) ==")
            proc = boot(args.port, log=os.path.join(tmp, "engineA.log"))
            bootstrap_defs(args.port, tmp)
            load_ai_stack(args.port)
            init_arena(args.port, name="move_test")
            phases = [
                (1, phase_walk_and_commit),
                (2, phase_command_time_refusal),
                (3, phase_partial_batch),
                (4, phase_arrival_refusal),
                (5, phase_stale_instance),
                (6, phase_vanished_counterpart),
                (7, phase_blocked_approach),
                (8, phase_long_trip),
            ]
            for n, fn in phases:
                if n in wanted:
                    fn(args.port, chk)
            quit_engine(args.port, proc)
            proc = None
        if 9 in wanted:
            root = make_isolated_root(tmp)
            phase_save_load(chk, root, tmp, args.port, args.seed, args.size)

        print(f"\n{'PASS' if chk.failed == 0 else 'FAIL'}: "
              f"{chk.failed} check(s) failed")
        return 0 if chk.failed == 0 else 1
    finally:
        if proc is not None:
            quit_engine(args.port, proc)
        shutil.rmtree(tmp, ignore_errors=True)


if __name__ == "__main__":
    sys.exit(main())
