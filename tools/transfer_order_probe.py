#!/usr/bin/env python3
"""Headless transfer-order execution probe (issues #1247 and #1253, epic
#1013 slices UIT-2B and UIT-5A).

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
  6. A counterpart that VANISHES mid-walk retires the order, telling the
     player the destination is gone.
  7. A blocked approach (an ocean ring the carrier cannot cross) stalls
     out and warns ONCE, naming the unit and the target.
  8. A trip that keeps making progress for LONGER than the nominal
     timeout still completes — the closest-approach reset is what
     distinguishes a stall timer from a trip budget (#920).
  9. An order in flight survives a real save -> quit -> fresh restart ->
     load, and the carrier resumes and completes it.
 10. The player cancels a haul mid-walk: nothing moves and the carrier
     is released.
 11. The carrier DIES mid-walk: its order is retired rather than left
     pending forever on a unit that will never tick again.

#1253's terminal cleanup runs through every one of those. Whichever way
an order ends — committed, refused, retired, stalled or cancelled — its
outcome reaches the player exactly once and the order is then PRUNED, so
each phase checks the store is left holding no terminal order, and the
save/load phase additionally proves the integrity sweep has nothing to
report about a carrier that died mid-order.

Phases 1-8, 10 and 11 run on an ARENA (flat, no worldgen, deterministic
geometry). Phase 9 needs a real world page: a save containing an arena
page hangs the world thread on load (#365), so it runs on its own pair of
engines against a small generated world.

Usage:
  python3 tools/transfer_order_probe.py
  python3 tools/transfer_order_probe.py --port 9271 --seed 42 --size 48
  python3 tools/transfer_order_probe.py --only 10,11

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

from probelib import (FixtureNotRegistered, boot, clear_find_water,
                      init_arena, init_world, load_fixture_yaml, poll_until,
                      quit_engine, send, send_json, wait_load_published,
                      load_ai_stack)

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
#
# `bulk` is REQUIRED — every entry under `items:` is a physical item, so
# `Engine.Asset.YamlItems` rejects a definition whose bulk is missing or
# not a finite positive number, and one bad definition rejects the whole
# FILE. It cannot perturb any number above: the transfer capacity gate is
# weight-only (`Unit.Transfer` never reads bulk, and `Item.Types`'
# weight computation does not either), so bulk is carried on the instance
# and consumed by nothing this probe exercises.
DEFS_YAML_ITEMS = """\
items:
  - name: "probe_ingot"
    display_name: "Probe Ingot"
    sprite: "assets/textures/items/material/bar_steel.png"
    weight: 0.25
    bulk: 0.25
    category: Materials
"""

DEFS_YAML_BUILDINGS = """\
buildings:
  - name: "probe_wide_hold"
    display_name: "Probe Wide Hold"
    category: "Cargo"
    description: "Throwaway #1247 test fixture - not shipped content."
    sprite: "assets/textures/buildings/cargo_hold_S/default.png"
    visual_class: "freestanding_installation"
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
    visual_class: "freestanding_installation"
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
    visual_class: "freestanding_installation"
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
    visual_class: "freestanding_installation"
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

# The cancel phase (10) shares lane 1's row rather than claiming a ninth
# one: the eight lanes already span -28..42 at the 10-tile spacing that
# keeps a wanderer out of the next phase, and both remaining edges of the
# arena are within that spacing of an existing lane. It is separated
# along X instead, 38 tiles east of lane 1's hold and far inside the
# arena box, with lane 1's own carrier destroyed by the time it runs.
CANCEL_HOLD_X = 10
# The dead-carrier phase (11) shares that row too, another 20 tiles east
# and still inside the arena's +47 edge with room for a 14-tile approach.
KILL_HOLD_X = 30


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
    load_fixture_yaml(port, "engine.loadItemYaml", items)
    load_fixture_yaml(port, "engine.loadBuildingYaml", builds)


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


def wait_settled(port: int, uid: int, oid: int, seconds: float) -> bool:
    """Poll until `oid` has left the store.

    Since #1253 an order that reaches a terminal state has its outcome
    surfaced and is then PRUNED, in the same tick -- so "the order
    finished" is observed as its DISAPPEARANCE, not as a terminal
    snapshot. There is deliberately no attempt to catch the terminal
    state in flight: the window is a fraction of one AI tick, and a probe
    that raced for it would be flaky in exactly the direction that hides
    a missing prune. What actually happened is asserted from the two
    surfaces that outlive the order -- the event log, and the world."""
    return poll_until(seconds, lambda: True if not any(
        o.get("id") == oid for o in orders(port, uid)) else None,
        interval=0.5) is not None


def terminal_orders(port: int, uid: int) -> list:
    """Terminal orders still sitting in the store -- always [] once a
    scenario has settled (requirement 5). A leftover here is the whole
    failure mode pruning exists to prevent: it rides the next save and
    keeps reporting its dead endpoint to the integrity sweep."""
    return [o for o in orders(port, uid) if o.get("terminal")]


def event_total(port: int, uid: int, category: str, needle: str) -> int:
    """How many times a matching event has been emitted for `uid`.

    The event log COALESCES identical consecutive entries into one row
    carrying a `count` (Engine.PlayerEvent.Emit), so counting rows
    reports one for two emissions. Summing `count` over the WHOLE log and
    taking a before/after delta is what makes "warned exactly once" mean
    it -- a slice from a mark would also miss a pre-existing row whose
    count was bumped."""
    total = 0
    for e in event_log(port):
        if (e.get("category") == category and e.get("uid") == uid
                and needle in e.get("text", "")):
            total += int(e.get("count", 1) or 1)
    return total


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
        if not any(o.get("id") == oid for o in orders(port, uid)):
            return True
        act = current_action(port, uid)
        if act == "wander":
            stole.append(act)
        return None
    done = poll_until(90.0, sample_until_done, interval=0.4)
    chk.ok(done is not None,
           "the order completes and is PRUNED from the store (#1253 "
           "requirement 5 -- a completed haul does not linger)")
    chk.ok(not stole, "the wander tick never took the carrier mid-order",
           f"{len(stole)} wander sample(s)")
    if done is None:
        return
    chk.ok(orders(port, uid) == [],
           "the carrier carries no order at all afterwards",
           str(orders(port, uid)))

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
    before = event_total(port, uid, "unit_warning", "Probe Ingot")
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
    if chk.ok(wait_settled(port, uid, oid, 90.0),
              "the order completes and is pruned"):
        chk.ok(len(stored(port, bid)) == 8, "eight are in the hold")
        chk.ok(len(ingots(port, uid)) == 4, "four stayed with the carrier")
    # EXACTLY once for the whole order, across BOTH moments that report
    # on it. A commit result carries an outcome for every requested item,
    # the four create-time refusals included, so an arrival report that
    # did not exclude what the command-time gate already surfaced would
    # warn about the same four refusals a second time.
    chk.ok(event_total(port, uid, "unit_warning", "Probe Ingot")
           - before == 1,
           "the four that did not fit are warned about ONCE, not again "
           "when the eight that did arrive",
           str([e.get("text") for e in event_log(port)[mark:]
                if e.get("uid") == uid]))
    chk.ok(terminal_orders(port, uid) == [],
           "no terminal order is left in the store",
           str(terminal_orders(port, uid)))
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
    before = event_total(port, uid, "unit_warning", "Probe Ingot")
    chk.ok(wait_settled(port, uid, oid, 90.0),
           "the order terminalises and is pruned")
    chk.ok(len(ingots(port, uid)) == 1,
           "nothing moved — no item half-moved")
    # The refusal's own structured cause is what the message quotes, so
    # this is the arrival gate reporting itself rather than a generic
    # "something went wrong".
    refusals = [e for e in event_log(port)
                if e.get("category") == "unit_warning" and e.get("uid") == uid
                and "receiver_full" in e.get("text", "")]
    chk.ok(bool(refusals),
           "the arrival refusal surfaces its structured reason",
           str([e.get("text") for e in event_log(port)[-6:]]))
    chk.ok(event_total(port, uid, "unit_warning", "Probe Ingot") - before == 1,
           "warned exactly ONCE (counting coalesced rows' own count)")
    chk.ok(current_action(port, uid) != "transfer_order",
           "the carrier is released rather than stuck on a dead order",
           current_action(port, uid))
    chk.ok(terminal_orders(port, uid) == [],
           "no terminal order is left in the store",
           str(terminal_orders(port, uid)))
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
    before = event_total(port, uid, "unit_warning", "instance_missing")
    chk.ok(wait_settled(port, uid, oid, 90.0),
           "the order terminalises and is pruned")
    chk.ok(event_total(port, uid, "unit_warning", "instance_missing")
           - before == 1,
           "the missing instance is surfaced player-visibly with its "
           "structured cause, exactly once")
    chk.ok(len(stored(port, bid)) == 1,
           "its sibling still lands — one refusal neither rolls back nor "
           "blocks another item")
    chk.ok(terminal_orders(port, uid) == [],
           "no terminal order is left in the store",
           str(terminal_orders(port, uid)))
    send(port, f"unit.destroy({uid}); return 'ok'")


def phase_vanished_counterpart(port: int, chk: Checks) -> None:
    """6. The destination is demolished mid-walk.

    #1247 retired this QUIETLY, on the reasoning that a demolished
    destination is attrition rather than something to interrupt the
    player over. #1253 prunes the terminal order, which removes the only
    place that reason could still have been read afterwards -- so the
    order would otherwise vanish with the cargo still aboard and nothing
    anywhere saying why. It is surfaced like every other failure now.
    """
    print("\n--- 6. a counterpart that vanished is surfaced and pruned ---")
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
    before = event_total(port, uid, "unit_warning", "no longer exists")
    send(port, f"building.destroy({bid}); return 'ok'")
    chk.ok(wait_settled(port, uid, oid, 60.0),
           "the order retires and is pruned")
    chk.ok(event_total(port, uid, "unit_warning", "no longer exists")
           - before == 1,
           "the player is told the destination is gone — exactly once, and "
           "not silently, because the pruned order is no longer readable")
    chk.ok(len(ingots(port, uid)) == 1, "the carrier keeps its cargo")
    chk.ok(current_action(port, uid) != "transfer_order",
           "the carrier is released", current_action(port, uid))
    chk.ok(terminal_orders(port, uid) == [],
           "no terminal order is left in the store",
           str(terminal_orders(port, uid)))
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
    # Specifically THIS warning: unit_ai.lua's own stuck-walk watchdog
    # also files a unit_warning while the carrier bumps against the
    # ocean, so "some warning appeared" would pass without the order ever
    # having reported anything. The hold is alive, so the message names
    # it (requirement 2's "naming the unit and the target").
    needle = "couldn't reach the transfer destination (Probe Small Hold)"
    before = event_total(port, uid, "unit_warning", needle)
    started = time.time()
    oid_raw = command_order(port, uid, order_request(port, uid, bid, 1))
    try:
        oid = int(float(oid_raw))
    except (TypeError, ValueError):
        chk.ok(False, "the order was accepted", oid_raw)
        return
    settled = wait_settled(port, uid, oid, 180.0)
    elapsed = time.time() - started
    chk.ok(settled, f"the order gives up and is pruned (after {elapsed:.0f} s)")
    chk.ok(elapsed > 30.0,
           "it persisted rather than giving up immediately", f"{elapsed:.0f} s")
    chk.ok(event_total(port, uid, "unit_warning", needle) - before == 1,
           "the player is warned ONCE, by unit and target name, that the "
           "transfer destination could not be reached",
           str([e.get("text") for e in event_log(port)[-8:]]))
    chk.ok(len(ingots(port, uid)) == 1, "the cargo stayed put")
    chk.ok(terminal_orders(port, uid) == [],
           "no terminal order is left in the store — never retried forever, "
           "never silently dropped", str(terminal_orders(port, uid)))
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
    settled = wait_settled(port, uid, oid, 400.0)
    elapsed = time.time() - started
    if chk.ok(settled, f"the long haul completes (after {elapsed:.0f} s)"):
        chk.ok(len(stored(port, bid)) == 1,
               "and commits on arrival", str(stored(port, bid)))
    chk.ok(elapsed > 60.0,
           "the trip really did outlast the 60 s stall budget — a total-trip "
           "budget would have abandoned it", f"{elapsed:.0f} s")
    send(port, f"unit.destroy({uid}); return 'ok'")


def phase_cancel(port: int, chk: Checks) -> None:
    """10. The player calls the haul off mid-walk (#1253 requirement 1).

    Driven through `unitAi.cancelTransferOrder`, which is exactly what
    the unit context menu's "Cancel transfer" row invokes -- the menu's
    own gate and this gesture's sequencing are pinned headlessly in
    `Test.Headless.UI.TransferContextMenu`, and what only a real engine
    can show is that the carrier really does stop walking with nothing
    moved.
    """
    print("\n--- 10. a mid-walk cancel releases the carrier ---")
    lane = LANES[1]
    bid = spawn_hold(port, "probe_wide_hold", CANCEL_HOLD_X, lane)
    if not chk.ok(bid is not None, "3x1 probe hold spawned"):
        return
    uid = spawn_carrier(port, CANCEL_HOLD_X + 14, lane)
    give(port, uid, "probe_ingot", 2)
    oid_raw = command_order(port, uid, order_request(port, uid, bid, 2))
    try:
        oid = int(float(oid_raw))
    except (TypeError, ValueError):
        chk.ok(False, "the order was accepted", oid_raw)
        return
    chk.ok(wait_state(port, uid, oid, "in_transit", 20.0) is not None,
           "the carrier sets off")
    # Let it get genuinely under way, so "released" means something.
    time.sleep(3.0)
    start = unit_tile(port, uid)
    chk.ok(any(o.get("id") == oid for o in orders(port, uid)),
           "the order is live when the player cancels it")

    before = event_total(port, uid, "unit_event", "was cancelled")
    res = send(port, "return tostring(require('scripts.unit_ai')"
                     f".cancelTransferOrder({uid}))")
    chk.ok(res == "true", "the cancel gesture reports success", res)
    chk.ok(orders(port, uid) == [],
           "the order is gone from the store immediately — cancelled, "
           "surfaced and pruned in one gesture", str(orders(port, uid)))
    chk.ok(event_total(port, uid, "unit_event", "was cancelled") - before == 1,
           "a player-visible line is filed against the carrier, exactly once")
    chk.ok(len(ingots(port, uid)) == 2, "NOTHING moved")
    chk.ok(not stored(port, bid), "and nothing reached the hold")

    # Released: the transfer lock is gone, so the ambient tick takes the
    # unit back, and it is not still marching at the hold.
    time.sleep(6.0)
    chk.ok(current_action(port, uid) != "transfer_order",
           "the carrier is released from the transfer job",
           current_action(port, uid))
    end = unit_tile(port, uid)
    if start and end and start[0] is not None and end[0] is not None:
        # It may wander (radius 5), but it must not have carried on
        # closing the ~11 remaining tiles to the hold.
        chk.ok(abs(end[0] - CANCEL_HOLD_X) > 5.0,
               "it stopped walking at the hold rather than finishing the trip",
               f"{start} -> {end}, hold at x={CANCEL_HOLD_X}")

    # A second cancel is inert, not an error: the order is already gone.
    again = send(port, "return tostring(require('scripts.unit_ai')"
                       f".cancelTransferOrder({uid}))")
    chk.ok(again == "false", "a second cancel is inert", again)
    send(port, f"unit.destroy({uid}); return 'ok'")


def phase_carrier_dies(port: int, chk: Checks) -> None:
    """11. The carrier DIES mid-walk (#1253, review round 1).

    Distinct from the destroyed carrier phase 9 folds in, and the reason
    it needs its own check: `unit.kill` leaves the instance in place, so
    every reference the order carries still resolves and the integrity
    sweep stays perfectly quiet about it -- while `scripts/unit_ai.lua`
    short-circuits a `dead` pose before any action scores, so the
    executor can never reach the terminal transition that prunes it.
    Nothing would have noticed. It is also the only one of the two whose
    aftermath is directly observable: the corpse still resolves a store,
    so `unit.getTransferOrders` can be asked.
    """
    print("\n--- 11. a carrier that DIES mid-walk leaves no order ---")
    lane = LANES[1]
    bid = spawn_hold(port, "probe_wide_hold", KILL_HOLD_X, lane)
    if not chk.ok(bid is not None, "3x1 probe hold spawned"):
        return
    uid = spawn_carrier(port, KILL_HOLD_X + 14, lane)
    give(port, uid, "probe_ingot", 1)
    oid_raw = command_order(port, uid, order_request(port, uid, bid, 1))
    try:
        oid = int(float(oid_raw))
    except (TypeError, ValueError):
        chk.ok(False, "the order was accepted", oid_raw)
        return
    chk.ok(wait_state(port, uid, oid, "in_transit", 20.0) is not None,
           "the carrier sets off")
    send(port, f"unit.kill({uid}); return 'ok'")
    # unit.getPose is what scripts/unit_ai.lua's own dead short-circuit
    # reads, so this asserts the carrier is dead by the exact measure
    # that stops it ever ticking again.
    def pose() -> str:
        return send(port, f"return tostring(unit.getPose({uid}))").strip('"')
    poll_until(15.0, lambda: pose() == "dead" or None)
    chk.ok(pose() == "dead", "the carrier is dead", pose())
    chk.ok(wait_settled(port, uid, oid, 20.0),
           "its order is retired rather than left pending forever")
    # The corpse is still a live instance -- which is exactly why nothing
    # downstream would have flagged the abandoned order.
    chk.ok(send(port, f"return tostring(unit.exists({uid}))") == "true",
           "…and the corpse itself is still there, so this was never a "
           "consequence of the unit disappearing")
    chk.ok(orders(port, uid) == [], "no order at all remains",
           str(orders(port, uid)))
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


def log_lines(path: str, needle: str) -> list[str]:
    try:
        with open(path, "r", encoding="utf-8", errors="replace") as f:
            return [ln.rstrip("\n") for ln in f if needle in ln]
    except OSError:
        return []


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

        # An ORPHAN candidate rides along in the same save (#1253): a
        # second carrier with its own in-flight order, destroyed before
        # the save is taken. Nothing ever ticks that order again, so
        # without the engine-side cleanup it would be captured here,
        # restored below, and report its dead carrier to the integrity
        # sweep on every save and load for the rest of the session.
        ghost = spawn_carrier(port, gx + 11, gy)
        give(port, ghost, "probe_ingot", 1)
        ghost_oid = command_order(port, ghost, order_request(port, ghost, bid, 1))
        chk.ok(ghost_oid not in ("false", "nil"),
               "the doomed carrier's order was accepted", ghost_oid)
        chk.ok(wait_state(port, ghost, int(float(ghost_oid)), "in_transit",
                          30.0) is not None,
               "…and is in flight when its carrier dies")
        send(port, f"unit.destroy({ghost}); return 'ok'")

        saved = send(port, f"return engine.saveWorld('probe_world', '{SLOT}')")
        chk.ok(saved.strip() == "true", "engine.saveWorld accepted", saved)
        save_file = os.path.join(root, "saves", SLOT, "world.synworld")
        chk.ok(poll_until(30.0, lambda: os.path.exists(save_file) or None)
               is not None, "the save file appeared")
        chk.ok(not log_lines(log_b, "integrity diagnostic"),
               "the SAVE reported no integrity diagnostic — the destroyed "
               "carrier's order never reached it",
               str(log_lines(log_b, "integrity diagnostic")[:3]))
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
        # Requirement 5's "the integrity sweep stays quiet", end to end
        # and in a fresh process: World.Load.Stage logs one line per
        # dangling transfer-order reference, and the orphaned order's
        # dead carrier, endpoints and items would be several.
        chk.ok(not log_lines(log_c, "transfer-order integrity diagnostic"),
               "the LOAD reported no dangling transfer-order reference",
               str(log_lines(log_c, "transfer-order integrity diagnostic")[:3]))
        send(port, "engine.setPaused(false); return 'ok'")
        chk.ok(wait_settled(port, uid, oid, 180.0),
               "the carrier resumes, the order completes and is pruned")
        chk.ok(len(stored(port, bid)) == 1, "the item is in the hold")
        chk.ok(orders(port, uid) == [],
               "and the carrier is left with no order at all",
               str(orders(port, uid)))

        # A save taken AFTER that terminal outcome carries only live
        # orders — there are none left, and the second save writing
        # cleanly with no diagnostic is what says so at the boundary
        # rather than only in the store.
        saved2 = send(port, f"return engine.saveWorld('probe_world', '{SLOT}')")
        chk.ok(saved2.strip() == "true", "a post-completion save succeeds",
               saved2)
        time.sleep(2.0)
        chk.ok(not log_lines(log_c, "integrity diagnostic"),
               "…with no integrity diagnostic of any kind",
               str(log_lines(log_c, "integrity diagnostic")[:3]))
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

    all_phases = set(range(1, 12))
    wanted = {int(p) for p in args.only.split(",") if p.strip()} or all_phases
    # A selection that names no real phase must FAIL, not report a clean
    # run of nothing: an exit-0 probe that executed zero checks is
    # indistinguishable from one that passed.
    if not wanted & all_phases:
        print(f"no such phase(s): {sorted(wanted)} (valid: 1-11)")
        return 2
    tmp = tempfile.mkdtemp(prefix="synarchy_transfer_order_probe_")
    chk = Checks()
    proc = None
    try:
        if wanted & (set(range(1, 9)) | {10, 11}):
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
                (10, phase_cancel),
                (11, phase_carrier_dies),
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
    try:
        sys.exit(main())
    except FixtureNotRegistered as exc:
        print(f"\n{exc}")
        sys.exit(1)
