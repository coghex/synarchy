#!/usr/bin/env python3
"""Expedition retrieval-and-return end-to-end probe (#920).

`docs/expedition_gameplay_loop.md` step 6: "Prove that the player can
order pickup, see whether an item fits, identify its carrier, return
home, and deposit or consume it. Do not add a generalized caravan
interface unless direct RTS retrieval proves inadequate."

Every piece of that chain already existed. What had never been
exercised is the chain as a PLAYER-DRIVEN SEQUENCE AT DISTANCE, against
an item lying in a real ruin rather than in the colony — and running it
end to end is what surfaced the two stall bugs this probe now guards
(see "What this caught" below).

Deliberately no new one-click "retrieve" verb and no caravan/logistics
interface: the sequence here is exactly the direct-RTS one a player
already has — order pickup (`unitAi.commandPickup`, what
`scripts/init_context_menu.lua`'s "Pick up" entry dispatches), order the
carrier home (`unitAi.commandMove`, the "Move here" entry), then store
it adjacent (`unit.depositToCargo(uid, bid, defName, instanceId)`).

That verb is a LAX AI verb with no adjacency gate of its own, so this
probe applies the Chebyshev-<=1 rule itself rather than booting the UI,
which needs a GPU. It used to be the exact call
`scripts/unit_info_v2_context_menu.lua`'s "Store in <cargo>" entry made;
#1249 retired that entry in favour of a queued order-at-a-distance
("Store 1" / "Store all" targeting the open container window), so the
call below is no longer any player gesture's own. It is kept
deliberately: this probe's subject is the direct-RTS RETRIEVAL loop
(#920), and the adjacent deposit is how it banks the recovered item
without depending on the transfer-order executor's own timing.

Checks, in one uninterrupted session against a REAL generated world:

  1. CAPACITY IS LEGIBLE BEFORE THE TRIP. An order that cannot fit is
     refused at command time — no `pickupOrder`, no travel, and a
     player-visible warning naming that carrier and that item. Shedding
     one ballast item makes the identical order succeed, so the gate is
     the exact `getCarryingWeight + <ground row>.weight > capacity`
     formula (both sides live: worn/accessory mass on the carrier, fill
     and nested contents on the instance) and not a coarse guess. Since
     #1666 the order reads that row through
     `item.getGroundForUnit(uid, gid)` — the CARRIER'S own page — rather
     than through the active-page `item.listGround()`; here the carrier
     is on the active page, so both name the same instance. The
     pre-existing ARRIVAL-time check is untouched and still runs.
  2. TRAVEL + PICKUP + CARRIER IDENTITY. The carrier walks to the ruin
     over many ticks (no teleport, monotonically closing), picks the
     item up through the real `pickup_ground` AI action, and the pickup
     lands a player-facing event naming both the item and its carrier.
  3. SURVIVAL INTERRUPTION MID-RETURN. With the return order pending,
     the carrier is made hungry; it really eats (`eat_from_inventory` /
     `eating` activity), still holds the recovered instance throughout,
     and then resumes closing on home — "orders are requests, not
     puppet strings", and the request survives.
  4. SAVE MID-JOURNEY, RESTART THE PROCESS, LOAD. Saved on the inbound
     leg; a FRESH engine process loads it and the same instance id is
     still on the same carrier, the return intent (`commandedTask`) is
     still pending, the carrier finishes the walk on its own, and the
     item is deposited into colony storage with its identity and
     properties intact.
  5. USABLE AS A LOCALLY PRODUCED ITEM. The recovered radio is
     withdrawn from storage by a DIFFERENT colonist (targeted by
     instance id) and drives an existing, provenance-blind consumer:
     `notify_allies`' radio branch, which keys purely on "an inventory
     item whose defName is radio". Without a radio that colonist never
     broadcasts; holding the recovered one it does, and a third colonist
     learns the water source from the broadcast.

What this caught (both fixed alongside this probe):
  * `pickup_timeout` was a TOTAL-TRIP budget, so an ordered pickup
    self-destructed after 30 game-seconds of travel however well it was
    going — capping ordered retrieval at roughly 21 tiles and making
    requirement 1's "at a remote location" impossible. Observed: a
    carrier walked 26 tiles, reached 1.7 tiles from the item, and was
    told "Couldn't reach item to pick up".
  * `maintainTask`'s TASK_TIMEOUT_SEC was the same shape for move
    orders, dropping a still-progressing return leg at ~42 tiles (and
    sooner once a survival interruption ate into the budget).
  Both are now STALL timers that reset on a new closest approach, and
  (#1291) charge only time the unit was free to pursue the order —
  the interruption in stage 3 costs it nothing however long it lasts.

Ground-item ids are ZERO-based (`src/Item/Ground.hs` `gisNextId = 0`),
unlike every other allocator here — this probe compares them
numerically and never tests them for truthiness.

Runs against a throwaway isolated resource root, so it never touches
the developer's real `saves/`.

Usage:
  python3 tools/expedition_retrieval_probe.py
  python3 tools/expedition_retrieval_probe.py --seed 42 --size 64 --port 9920

Exit code 0 = all checks passed.
"""
from __future__ import annotations

import argparse
import glob
import math
import os
import shutil
import sys
import tempfile
import time

from probelib import (boot, quit_engine, send, send_json, poll_until,
                      capture_request_id, wait_save_complete,
                      wait_load_published)

REPO = os.path.dirname(os.path.dirname(os.path.abspath(__file__)))

PAGE = "expedition"
SLOT = "expedition_retrieval_probe"
LOG_A = "/tmp/expedition_retrieval_probe_a.log"
LOG_B = "/tmp/expedition_retrieval_probe_b.log"

# The retrieval target. The radio is the only shipped item with a
# provenance-blind consumer that needs no water source to drive
# (check 5), which is what makes it the target here.
#
# It is STAGED by this probe (see stage_target), not scavenged from the
# ruin's own contents. #921 removed ruin_small's two fixed `kind: item`
# entries — a ruin now guarantees no specific item, only weighted
# loot-table rolls, and `radio` is deliberately not in ruin_common
# (it is spawn-only starting equipment). Nothing about #920 is relaxed
# by staging it: every check below still runs against a real ground
# instance lying in a real placed ruin, ~HOME_MIN_DIST tiles from home.
TARGET_DEF = "radio"

# Home sits this far from the ruin anchor — comfortably beyond any
# sightline that could reveal the ruin (#1230 replaced the def's 6-tile
# discovery halo with a night-aware sight radius of at most 12 tiles),
# and (at the acolyte's ~0.7 tiles/s
# comfort speed) well past the ~21 tiles the old total-trip
# pickup_timeout allowed, so a regression to that shape fails check 2.
HOME_MIN_DIST = 26
HOME_MAX_DIST = 30
CHUNK = 16

# A single position sample may not jump further than this. A sample is a
# couple of seconds of walking at the acolyte's ~0.7 tiles/s comfort
# speed; 4.0 leaves generous headroom for a slow console round trip while
# still ruling out a teleport.
MAX_STEP_TILES = 4.0

# Ballast for the over-capacity case. Deliberately NOT a Materials-
# category item: `store_materials` fires on any Materials in inventory
# (utility 3.0 * fill^3, and the colony's cargo is right there), which
# would both move the carrier and shed the ballast mid-check.
BALLAST_DEF = "pick_steel"


class Checks:
    def __init__(self) -> None:
        self.failed = 0

    def ok(self, cond: bool, label: str) -> bool:
        print(f"  [{'PASS' if cond else 'FAIL'}] {label}", flush=True)
        if not cond:
            self.failed += 1
        return bool(cond)


# --------------------------------------------------------------------------
# Boot / bootstrap
# --------------------------------------------------------------------------
def make_isolated_root(base: str) -> str:
    """A throwaway resource root: the real read-only content families
    symlinked, plus its OWN empty saves/ (mirrors
    tools/persistence_contract_probe.py's helper)."""
    root = os.path.join(base, "root")
    os.makedirs(root, exist_ok=True)
    for family in ("scripts", "assets", "data", "config"):
        target = os.path.join(root, family)
        if not os.path.exists(target):
            os.symlink(os.path.join(REPO, family), target)
    os.makedirs(os.path.join(root, "saves"), exist_ok=True)
    return root


def bootstrap(port: int) -> None:
    """Register the defs + AI scripts the GUI loading screen would.
    Headless has no loading screen, so both engines do it by hand."""
    loaders = [
        ("data/substances/*.yaml", "engine.loadSubstanceYaml"),
        ("data/items/*.yaml", "engine.loadItemYaml"),
        ("data/equipment/*.yaml", "engine.loadEquipmentYaml"),
        ("data/materials/*.yaml", "engine.loadMaterialYaml"),
        ("data/units/*.yaml", "engine.loadUnitYaml"),
        ("data/buildings/*.yaml", "engine.loadBuildingYaml"),
        ("data/loot_tables/*.yaml", "engine.loadLootTableYaml"),
    ]
    for pattern, fn in loaders:
        for path in sorted(glob.glob(os.path.join(REPO, pattern))):
            send(port, f"{fn}('{os.path.relpath(path, REPO)}'); return 'ok'")
    send(port, "engine.loadLocationYaml('data/locations/ruin_small.yaml'); return 'ok'")
    for script, z in (("scripts/unit_stats.lua", 0.1),
                      ("scripts/unit_resources.lua", 0.2),
                      ("scripts/unit_ai.lua", 0.1)):
        send(port, f"engine.loadScript('{script}', {z}); return 'ok'")


def boot_probe(root: str, port: int, log: str, label: str):
    return boot(port, log=log, label=label, ready_timeout=240,
                args=["--resource-root", root])


# --------------------------------------------------------------------------
# World / geometry helpers
# --------------------------------------------------------------------------
def surface_z(port: int, gx: int, gy: int):
    """world.getSurfaceAt returns several values; the console tab-joins
    them, so wrap the call in parens to keep only the first."""
    raw = send(port, f"return (world.getSurfaceAt({gx},{gy}))")
    try:
        return float(raw)
    except (TypeError, ValueError):
        return None


def load_region(port: int, cx: int, cy: int, pad: int = 3) -> None:
    send(port, f"return world.loadChunksInRegion({cx-pad},{cy-pad},{cx+pad},{cy+pad})")
    send(port, "return world.waitForChunks(120)", timeout=130)


def ground_items(port: int) -> list:
    data = send_json(port, "return item.listGround()")
    return data if isinstance(data, list) else []


def ring_candidates(port: int, gx: int, gy: int) -> list:
    """Dry, resolvable tiles on a ring HOME_MIN_DIST..HOME_MAX_DIST from
    (gx, gy). Scanned server-side — one console round trip, not one per
    tile."""
    lua = (f"local out={{}}; for r={HOME_MIN_DIST},{HOME_MAX_DIST} do "
           f"for a=0,35 do local ang=a*math.pi/18; "
           f"local x=math.floor({gx}+r*math.cos(ang)); "
           f"local y=math.floor({gy}+r*math.sin(ang)); "
           f"local sz=(world.getSurfaceAt(x,y)); local f=world.getFluidAt(x,y); "
           f"if sz and not f then out[#out+1]={{x=x,y=y,z=sz}} end "
           f"end end; return out")
    data = send_json(port, lua, timeout=45)
    if not isinstance(data, list):
        return []
    seen, out = set(), []
    for c in data:
        key = (c["x"], c["y"])
        if key not in seen:
            seen.add(key)
            out.append(c)
    return out


def corridor_roughness(port: int, x0: int, y0: int, x1: int, y1: int,
                       samples: int = 24):
    """Largest surface-Z step between consecutive samples along the
    straight line from (x0,y0) to (x1,y1), or None if any sample is
    unresolved or wet. Placement legality says nothing about whether a
    carrier can WALK the route (tools/location_embark_probe.py learned
    the same lesson about cliffs), and a route that a path can't follow
    would fail the trip for reasons that have nothing to do with the
    retrieval chain under test."""
    lua = (f"local worst=0; local prev=nil; "
           f"for i=0,{samples} do local t=i/{samples}; "
           f"local x=math.floor({x0}+({x1}-{x0})*t); "
           f"local y=math.floor({y0}+({y1}-{y0})*t); "
           f"local sz=(world.getSurfaceAt(x,y)); local f=world.getFluidAt(x,y); "
           f"if not sz or f then return -1 end; "
           f"if prev then local d=math.abs(sz-prev); if d>worst then worst=d end end; "
           f"prev=sz end; return worst")
    raw = send(port, lua, timeout=30)
    try:
        v = float(raw)
    except (TypeError, ValueError):
        return None
    return None if v < 0 else v


def unit_pos(port: int, uid: int):
    r = send_json(port, f"local i=unit.getInfo({uid}); "
                        f"return i and {{x=i.gridX,y=i.gridY}} or nil")
    if isinstance(r, dict) and "x" in r:
        return float(r["x"]), float(r["y"])
    return None


def current_action(port: int, uid: int) -> str:
    return send(port, f"local s=require('scripts.unit_ai').getState({uid}); "
                      f"return s and s.currentAction or 'nil'")


def inventory(port: int, uid: int) -> list:
    """A unit's loose inventory with every stable per-instance property.

    `quality`/`condition` are surfaced only for defs that declare a spec
    for them (a radio declares neither), so comparing just those two
    would be a vacuous nil == nil test — hence the wider set. `temp` is
    deliberately excluded: it is present only while an item is off
    ambient, so it can legitimately appear or vanish on its own."""
    lua = (f"local out={{}}; for _,it in ipairs(unit.getInventory({uid}) or {{}}) do "
           f"out[#out+1]={{defName=it.defName,instanceId=it.instanceId,"
           f"displayName=it.displayName,weight=it.weight,fill=it.currentFill,"
           f"sharpness=it.sharpness,broken=it.broken,contentsKey=it.contentsKey,"
           f"quality=it.quality,condition=it.condition}} end; "
           f"return out")
    data = send_json(port, lua)
    return data if isinstance(data, list) else []


def properties(item: dict) -> dict:
    """The instance's identity-independent properties, for round-trip
    comparison."""
    return {k: v for k, v in (item or {}).items() if k != "instanceId"}


def find_instance(items: list, inst_id):
    for it in items:
        if it.get("instanceId") == inst_id:
            return it
    return None


def dist(a, b) -> float:
    return math.hypot(a[0] - b[0], a[1] - b[1])


def sample_walk(port: int, uid: int, seconds: float, interval: float = 1.0) -> list:
    """Positions sampled while a unit walks. Returned so the caller can
    assert the shape of the journey, not just its endpoint."""
    out = []
    deadline = time.time() + seconds
    while time.time() < deadline:
        p = unit_pos(port, uid)
        if p:
            out.append(p)
        time.sleep(interval)
    return out


def assert_real_travel(chk: Checks, samples: list, goal, label: str,
                       min_samples: int, min_closed: float) -> None:
    """The journey happened over many ticks and closed on its
    destination — not a teleport, not a stationary unit."""
    steps = [dist(samples[i], samples[i + 1]) for i in range(len(samples) - 1)]
    biggest = max(steps) if steps else 0.0
    # Both bounds matter: a motionless unit is not "not a teleport", it is
    # a stalled one, and reporting that as a pass would be a lie.
    chk.ok(len(samples) >= min_samples and 0.05 < biggest <= MAX_STEP_TILES,
           f"{label} is real multi-tick travel — moving, and not a teleport "
           f"({len(samples)} samples, largest single step {biggest:.2f} tiles)")
    if not samples:
        chk.ok(False, f"{label} produced no position samples")
        return
    chk.ok(dist(samples[-1], goal) < dist(samples[0], goal) - min_closed,
           f"{label} closed on its destination "
           f"({dist(samples[0], goal):.1f} -> {dist(samples[-1], goal):.1f} tiles)")


def spawn_unit(port: int, gx: float, gy: float, clear_water: bool = True) -> int:
    raw = send(port, f"return unit.spawn('acolyte', {gx}, {gy})")
    try:
        uid = int(float(raw))
    except (TypeError, ValueError):
        return -1
    if uid < 0:
        return -1
    # Settle onto the ground, then (usually) retire the standing
    # find_water goal so its search can't out-compete what's under test.
    time.sleep(2.0)
    if clear_water:
        poll_until(10.0, lambda: send(
            port, f"local ai=require('scripts.unit_ai'); "
                  f"local s=ai.getState({uid}); if not s then return false end; "
                  f"ai.markGoalAccomplished(s,'find_water'); return true") == "true")
    return uid


def event_log(port: int) -> list:
    data = send_json(port, "return engine.getEventLog()")
    return data if isinstance(data, list) else []


def check_ai_tick_clean(chk: Checks, log_path: str, label: str) -> None:
    """No `Lua error in update()` in the engine log.

    A raise out of the unit_ai update tick kills EVERY unit's AI for that
    tick, not just the action that raised — so it fails silently as
    "nothing moved" rather than as an error anyone is looking at. Two
    such raises (unbound `findTechnomule`/`groundCountOf` in
    unit_ai_deliver.lua, left over from the #538 module split) fired the
    moment this probe's colony building appeared with unmet material
    need, and stopped the whole expedition dead. Cheap to assert, so
    assert it."""
    try:
        with open(log_path) as fh:
            bad = [ln.strip() for ln in fh if "Lua error in update()" in ln]
    except OSError as exc:
        chk.ok(False, f"{label}: could not read the engine log ({exc})")
        return
    uniq = sorted(set(bad))
    chk.ok(not uniq,
           f"{label}: the unit_ai update tick raised nothing "
           f"({len(bad)} error line(s){': ' + uniq[0] if uniq else ''})")


# --------------------------------------------------------------------------
# Site selection
# --------------------------------------------------------------------------
def stage_target(port: int, gx: int, gy: int):
    """Put the retrieval target on the ground at a chosen ruin's anchor.

    #921: ruin contents are weighted loot-table draws now, so no ruin
    guarantees a radio — and this probe needs a DETERMINISTIC target
    (check 5 drives notify_allies' radio branch, which keys on defName).
    Staging it here keeps that determinism without teaching the ruin to
    guarantee anything. The ruin's chunk is already loaded by the
    caller, so the instance lands in the room like any other ground
    item; this probe reads it back through the same item.listGround()
    surface it always did, and the pickup order reads it through
    item.getGroundForUnit on the carrier's own page (#1666) — the same
    page, for a carrier standing on the active one.

    Returns the staged ground instance, or None if it never appeared."""
    send(port, f"item.spawnGround('{TARGET_DEF}', {gx}, {gy}, nil, '{PAGE}'); "
               f"return 'ok'")
    return poll_until(15.0, lambda: next(
        (g for g in ground_items(port)
         if g.get("defName") == TARGET_DEF
         and abs(g["x"] - gx) <= 3 and abs(g["y"] - gy) <= 3), None))


def pick_site(chk: Checks, port: int):
    """Choose a real placed ruin, stage the retrieval target in it, and
    find a colony home tile ~HOME_MIN_DIST..HOME_MAX_DIST away across
    walkable ground.

    Returns (ruin, target_item, home_xy) or None."""
    ruins = poll_until(30.0, lambda: [
        e for e in (send_json(port, f"return world.listPlacedLocations('{PAGE}')") or [])
        if isinstance(e, dict) and e.get("id") == "ruin_small"])
    if not ruins:
        chk.ok(False, "world places at least one ruin_small")
        return None
    print(f"  {len(ruins)} ruin_small placed", flush=True)

    for ruin in ruins:
        gx, gy = int(ruin["gx"]), int(ruin["gy"])
        load_region(port, int(ruin["cx"]), int(ruin["cy"]))
        rz = surface_z(port, gx, gy)
        best = None
        for c in ring_candidates(port, gx, gy):
            if rz is not None and abs(c["z"] - rz) > 2:
                continue
            rough = corridor_roughness(port, c["x"], c["y"], gx, gy)
            if rough is None or rough > 2.0:
                continue
            if best is None or rough < best[0]:
                best = (rough, c)
            if rough == 0:
                break
        if not best:
            print(f"  ruin ({gx},{gy}): no walkable home candidate, trying next",
                  flush=True)
            continue
        rough, c = best
        # Staged only once this ruin is actually the site — a ruin
        # rejected for its terrain must not be left holding a stray
        # radio for a colonist to wander into and confuse check 5.
        target = stage_target(port, gx, gy)
        if not target:
            print(f"  ruin ({gx},{gy}): staged {TARGET_DEF} never appeared, "
                  f"trying next", flush=True)
            continue
        print(f"  site: ruin ({gx},{gy}) z={rz}, staged {TARGET_DEF} "
              f"gid={target['id']} at ({target['x']:.0f},{target['y']:.0f}), "
              f"home ({c['x']},{c['y']}) z={c['z']} at "
              f"{dist((c['x'], c['y']), (gx, gy)):.1f} tiles, "
              f"corridor roughness {rough}", flush=True)
        return ruin, target, (int(c["x"]), int(c["y"]))
    chk.ok(False, "a ruin with a walkable colony site at "
                  f"{HOME_MIN_DIST}..{HOME_MAX_DIST} tiles exists")
    return None


def build_colony(chk: Checks, port: int, home) -> int:
    """A cargo_hold_S at `home` — the colony's existing storage. Returns
    its bid, or -1."""
    hx, hy = home
    raw = send(port, f"return building.spawn('cargo_hold_S', {hx}, {hy})")
    try:
        bid = int(float(raw))
    except (TypeError, ValueError):
        chk.ok(False, f"cargo_hold_S spawned at home (got {raw!r})")
        return -1
    cap = poll_until(30.0, lambda: (
        lambda v: v if (v or 0) > 0 else None)(
            _as_float(send(port, f"return building.getStorageCapacity({bid})"))))
    if not chk.ok(bool(cap), f"colony cargo_hold_S spawned at ({hx},{hy}) "
                             f"with storage capacity {cap}"):
        return -1
    # Finish it. A cargo_hold_S spawns Constructing (build_work 240) and an
    # unfinished building is a CONSTRUCTION SITE: build_nearby and
    # deliver_to_build_site (utility 6.0, with a lock-in) will pull the
    # carrier off its expedition to go and finish the colony's store —
    # correct behaviour, and a confound here. The colony's storage is
    # meant to be a building that already exists.
    required = _as_float(send(port, f"return building.getBuildRequired({bid})")) or 240.0
    send(port, f"building.addBuildProgress({bid}, {required + 1.0}); return 'ok'")
    built = poll_until(20.0, lambda: send(
        port, f"return building.getActivity({bid})") == "built")
    if not chk.ok(bool(built),
                  f"the colony's storage is a FINISHED building, not a construction "
                  f"site the carrier would be pulled away to work on "
                  f"(activity {send(port, f'return building.getActivity({bid})')!r})"):
        return -1
    return bid


def _as_float(raw):
    try:
        return float(raw)
    except (TypeError, ValueError):
        return None


def adjacent_tile(port: int, bid: int):
    """A tile that satisfies the storage menu's own adjacency rule
    (Chebyshev <= 1 from the building's footprint)."""
    info = send_json(port, f"return building.getInfo({bid})")
    if not isinstance(info, dict):
        return None
    bx, by = int(info.get("gridX", 0)), int(info.get("gridY", 0))
    tw, th = int(info.get("tileW", 1) or 1), int(info.get("tileH", 1) or 1)
    return (bx + tw, by + th - 1), (bx, by, tw, th)


def is_adjacent(pos, foot) -> bool:
    bx, by, tw, th = foot
    ux, uy = int(math.floor(pos[0])), int(math.floor(pos[1]))
    dx = bx - ux if ux < bx else (ux - (bx + tw - 1) if ux >= bx + tw else 0)
    dy = by - uy if uy < by else (uy - (by + th - 1) if uy >= by + th else 0)
    return max(dx, dy) <= 1


# --------------------------------------------------------------------------
# Check 1 — capacity legible before the trip
# --------------------------------------------------------------------------
def check_capacity_gate(chk: Checks, port: int, carrier: int, target, ruin_xy):
    gid = int(target["id"])
    weight = float(target["weight"])
    cap = _as_float(send(port, f"return unit.getStat({carrier},'carrying_capacity')"))
    carried = _as_float(send(port, f"return unit.getCarryingWeight({carrier})"))
    print(f"  carrier capacity {cap:.1f} kg, carrying {carried:.1f} kg, "
          f"target {weight:.2f} kg", flush=True)

    # The accepted case needs real headroom first. A weak strength roll
    # can spawn an acolyte with almost none, so shed personal tools — the
    # same thing a player does before an expedition.
    for tool in ("pick_steel", "shovel_steel", "axe_steel"):
        carried = _as_float(send(port, f"return unit.getCarryingWeight({carrier})")) or 0
        if carried + weight <= cap:
            break
        send(port, f"unit.removeItem({carrier}, '{tool}'); return 'ok'")
    carried = _as_float(send(port, f"return unit.getCarryingWeight({carrier})")) or 0
    if not chk.ok(carried + weight <= cap,
                  f"precondition: carrier has room for the target "
                  f"({carried:.1f} + {weight:.2f} <= {cap:.1f} kg)"):
        return False

    # --- over capacity: refuse before any travel -----------------------
    ballast = 0
    while ballast < 24:
        c = _as_float(send(port, f"return unit.getCarryingWeight({carrier})")) or 0
        if c + weight > cap:
            break
        send(port, f"unit.addItem({carrier}, '{BALLAST_DEF}', 1); return 'ok'")
        ballast += 1
    over_carried = _as_float(send(port, f"return unit.getCarryingWeight({carrier})")) or 0
    chk.ok(over_carried + weight > cap,
           f"ballast ({ballast}x {BALLAST_DEF}) puts the carrier over capacity "
           f"for the target ({over_carried:.1f} + {weight:.2f} > {cap:.1f} kg)")

    before_events = len(event_log(port))
    accepted = send(port, f"return require('scripts.unit_ai')"
                          f".commandPickup({carrier}, {gid})")
    chk.ok(accepted.strip() == "false",
           f"over-capacity commandPickup is REFUSED (returned {accepted!r})")
    chk.ok(send(port, f"local s=require('scripts.unit_ai').getState({carrier}); "
                      f"return s.pickupOrder and 'set' or 'none'") == "none",
           "refused order leaves no pickupOrder to act on")

    new_events = event_log(port)[before_events:]
    label = send(port, f"local d; for _,x in ipairs(item.listDefs() or {{}}) do "
                       f"if x.name=='{TARGET_DEF}' then d=x end end; "
                       f"return d and d.displayName or '{TARGET_DEF}'")
    name = send(port, f"local i=unit.getInfo({carrier}); return i and i.name or ''")
    # `name` must be non-empty or the substring test below is vacuous.
    hits = [e for e in new_events
            if e.get("category") == "unit_warning" and e.get("uid") == carrier
            and label in (e.get("text") or "")
            and name and name in (e.get("text") or "")]
    chk.ok(bool(hits),
           f"refusal emits a player-visible warning naming the carrier "
           f"({name!r}) and the item ({label!r}): "
           f"{hits[0]['text'] if hits else new_events!r}")

    # ... and the trip never starts. The signal is the AI's own chosen
    # action, not displacement: an idle colonist wanders, and over any
    # window short enough to be practical, ambient wander and a purposeful
    # walk are indistinguishable by distance alone. `pickup_ground` never
    # being selected is exact — with no pickupOrder there is nothing for
    # the AI to act on — and the item still lying in the ruin is the
    # outcome the player actually cares about.
    seen: list = []
    for _ in range(10):
        time.sleep(1.0)
        a = current_action(port, carrier)
        if not seen or seen[-1] != a:
            seen.append(a)
    chk.ok("pickup_ground" not in seen,
           f"refused order never enters the pickup_ground action "
           f"(actions seen: {' -> '.join(seen)})")
    chk.ok(send(port, f"local s=require('scripts.unit_ai').getState({carrier}); "
                      f"return s.pickupOrder and 'set' or 'none'") == "none",
           "and no pickupOrder appeared during the dwell either")
    chk.ok(any(g.get("id") == gid for g in ground_items(port)),
           f"and the item is still lying in the ruin, unretrieved "
           f"(carrier {dist(unit_pos(port, carrier), ruin_xy):.1f} tiles away)")

    # --- boundary: shed the ballast and the SAME order is accepted -----
    for _ in range(ballast):
        send(port, f"unit.removeItem({carrier}, '{BALLAST_DEF}'); return 'ok'")
    under = _as_float(send(port, f"return unit.getCarryingWeight({carrier})")) or 0
    accepted = send(port, f"return require('scripts.unit_ai')"
                          f".commandPickup({carrier}, {gid})")
    chk.ok(under + weight <= cap and accepted.strip() == "true",
           f"the identical order is ACCEPTED once it fits "
           f"({under:.1f} + {weight:.2f} <= {cap:.1f} kg, returned {accepted!r}) "
           f"— the gate is the live carrying-weight/instance-weight sum, "
           f"not a coarse guess")
    return True


# --------------------------------------------------------------------------
# Check 2 — travel, pickup, carrier identity
# --------------------------------------------------------------------------
def check_outbound(chk: Checks, port: int, carrier: int, target):
    gid = int(target["id"])
    goal = (float(target["x"]), float(target["y"]))
    start = unit_pos(port, carrier)
    # An acolyte spawns with a radio of its own (data/units/acolyte.yaml),
    # so "the carrier now holds a radio" proves nothing — only a radio
    # instance it did NOT have before the order does.
    already = {it["instanceId"] for it in inventory(port, carrier)
               if it.get("defName") == TARGET_DEF}
    print(f"  outbound: {dist(start, goal):.1f} tiles to the item; carrier "
          f"already holds {TARGET_DEF} instance(s) {sorted(already)}", flush=True)

    samples = [start]
    saw_pickup_action = False
    picked = None
    deadline = time.time() + 240.0
    while time.time() < deadline:
        if current_action(port, carrier) == "pickup_ground":
            saw_pickup_action = True
        picked = find_instance_by_def(inventory(port, carrier), TARGET_DEF,
                                      exclude=already)
        if picked:
            break
        p = unit_pos(port, carrier)
        if p:
            samples.append(p)
        time.sleep(1.0)

    chk.ok(saw_pickup_action,
           "the carrier acts on the order through the real pickup_ground AI action")
    if not chk.ok(picked is not None,
                  f"the carrier reaches the ruin and picks the {TARGET_DEF} lying "
                  f"in it up "
                  f"({len(samples)} position samples over "
                  f"{dist(start, goal):.1f} tiles)"):
        return None
    chk.ok(not any(g.get("id") == gid for g in ground_items(port)),
           f"the ruin's ground item (gid {gid}) is gone from the world — it MOVED "
           f"into the carrier, it was not copied")

    assert_real_travel(chk, samples, goal, "the outbound leg",
                       min_samples=8, min_closed=5.0)

    label = send(port, f"local d; for _,x in ipairs(item.listDefs() or {{}}) do "
                       f"if x.name=='{TARGET_DEF}' then d=x end end; "
                       f"return d and d.displayName or '{TARGET_DEF}'")
    name = send(port, f"local i=unit.getInfo({carrier}); return i and i.name or ''")
    # Category, not wording, separates this from the refusal warning
    # check 1 emits: a completed pickup is a unit_event.
    hits = [e for e in event_log(port)
            if e.get("category") == "unit_event" and e.get("uid") == carrier
            and label in (e.get("text") or "")
            and name and name in (e.get("text") or "")]
    chk.ok(bool(hits),
           f"the pickup is reported on a player-facing surface identifying the "
           f"item and its carrier: {hits[-1]['text'] if hits else '(no event)'}")
    print(f"  recovered instance id {picked['instanceId']}: {properties(picked)}",
          flush=True)
    return picked


def find_instance_by_def(items: list, def_name: str, exclude: set):
    for it in items:
        if it.get("defName") == def_name and it.get("instanceId") not in exclude:
            return it
    return None


# --------------------------------------------------------------------------
# Check 3 — survival interruption on the return leg
# --------------------------------------------------------------------------
def provision(port: int, carrier: int) -> None:
    """Top the carrier back up for the return leg.

    The probe deliberately retires the `find_water` goal so the water
    search can't compete with the behaviour under test — which also means
    the carrier will never go and refill. A ~28-tile round trip drains
    the canteen it spawned with, and hydration attrition then collapses
    it (observed: pose=collapsed at 41% hydration) and eventually kills
    it (observed: pose=dead), which has nothing to do with retrieval but
    fails every downstream check. A real session would send it to water;
    the probe hands it a full canteen instead."""
    send(port, f"unit.modifyItemFill({carrier}, 'canteen_steel_2l', 2.0); return 'ok'")
    # The ENGINE's max_hydration is authoritative here. unit_stats.get's
    # derived value disagreed with it (53.4 vs 38.7 on the same unit), and
    # provisioning against the wrong maximum is what let the carrier keep
    # sliding toward the 5% death_threshold in unit_resource_config.
    maxh = _as_float(send(port, f"return unit.getStat({carrier},'max_hydration')"))
    if maxh:
        send(port, f"unit.setStat({carrier}, 'hydration', {maxh:.3f}); return 'ok'")


def alive(port: int, carrier: int) -> str:
    return send(port, f"return unit.getPose({carrier})")


def hydration_fraction(port: int, carrier: int):
    """hydration / max_hydration, both from the engine. Below 0.05 the
    resource tick kills the unit outright (unit_resource_config's
    death_threshold); below 0.20 it collapses. A probe that retires the
    find_water goal has to watch this itself."""
    cur = _as_float(send(port, f"return unit.getStat({carrier},'hydration')"))
    mx = _as_float(send(port, f"return unit.getStat({carrier},'max_hydration')"))
    if not cur or not mx or mx <= 0:
        return None
    return cur / mx


def death_report(port: int, carrier: int) -> str:
    """Why a carrier is down, from the player-facing streams.

    A dead or collapsed carrier fails every downstream check at once, and
    the pose alone doesn't say why. Survival deaths land in the event log
    as survival_critical; non-combat harm (falls, hazards, wound deaths)
    drains from injury.*; fights drain from combat.*. Reported together so
    one failure line names the cause instead of prompting a re-run."""
    # A SURVIVAL death (dehydration/starvation) reports through the
    # EVENT LOG as survival_critical -- verified live: forcing hydration
    # to 1% emits "X is dehydrated" then "X died of dehydration", both
    # log=true even headless. It does NOT appear in injury.* or combat.*,
    # so those two are reported alongside rather than instead: looking
    # only at them once made a perfectly well-reported death read as
    # silent.
    events = [f"{e.get('category')}: {e.get('text')}" for e in event_log(port)
              if e.get("uid") == carrier
              and e.get("category") in ("survival_critical", "survival_warning",
                                        "unit_warning")]
    inj = send(port, "local t=injury.drainEvents() or {}; return #t")
    cmb = send(port, "local t=combat.drainEvents() or {}; return #t")
    return (f"last events {events[-3:]}; injury events drained={inj}, "
            f"combat events drained={cmb}")


def check_interrupted_return(chk: Checks, port: int, carrier: int, home,
                             instance_id):
    hx, hy = home
    provision(port, carrier)
    pose = alive(port, carrier)
    hyd = hydration_fraction(port, carrier)
    if not chk.ok(pose not in ("dead", "collapsed") and (hyd or 0) > 0.5,
                  f"the carrier is on its feet and watered for the return leg "
                  f"(pose {pose!r}, hydration {hyd if hyd is None else round(hyd, 3)}; "
                  f"{death_report(port, carrier)})"):
        return False
    send(port, f"require('scripts.unit_ai').commandMove({carrier},{hx},{hy}); "
               f"return 'ok'")
    started = unit_pos(port, carrier)
    chk.ok(poll_until(20.0, lambda: current_action(port, carrier) == "follow_command")
           is not None,
           "the return order is accepted and the carrier starts home")

    # Let it get properly under way before interrupting.
    poll_until(30.0, lambda: (lambda p: p and dist(p, started) > 3.0)(
        unit_pos(port, carrier)))

    # A real survival need: empty the STOMACH meter so eat_from_inventory
    # outranks the pending order. Its utility is (1 - hungerFrac) *
    # eat_weight, and it only fires below eat_max_fraction, so it is
    # always >= 7.5 against follow_command's 7.0 — "a hungry unit
    # interrupts orders to eat" is the documented #306 shape.
    #
    # Hunger rather than thirst on purpose: hydration feeds the
    # consciousness model, so a thirst deep enough to outrank the order
    # can knock the carrier out before it ever drinks, and a collapsed
    # unit runs no AI at all (observed: pose=collapsed, no drink, the
    # return stalled). The stomach meter has no such path — and eating
    # refills it, which cleanly ends the interruption.
    #
    # Make sure there is food to eat: the carrier spawns with rations,
    # but a 27-tile outbound walk can consume them.
    send(port, f"unit.addItem({carrier}, 'rations', 1); return 'ok'")
    has_food = send(
        port, f"local n=0; for _,it in ipairs(unit.getInventory({carrier}) or {{}}) do "
              f"if it.defName=='rations' then n=n+1 end end; return n")
    chk.ok((_as_float(has_food) or 0) > 0,
           f"precondition: the carrier has food to eat ({has_food} rations)")
    maxhun = _as_float(send(port, f"return unit.getStat({carrier},'max_hunger')"))
    if not chk.ok(bool(maxhun), f"the carrier reports a max_hunger ({maxhun})"):
        return False
    send(port, f"unit.setStat({carrier}, 'hunger', {maxhun * 0.10:.3f}); return 'ok'")
    at_interrupt = unit_pos(port, carrier)

    ate = poll_until(45.0, lambda: (
        current_action(port, carrier) == "eat_from_inventory"
        or send(port, f"return unit.getActivity({carrier})") == "eating"))
    chk.ok(bool(ate),
           "a real survival need (eating) preempts the pending return order")
    chk.ok(find_instance(inventory(port, carrier), instance_id) is not None,
           "the recovered item stays carried across the interruption")
    chk.ok(send(port, f"local s=require('scripts.unit_ai').getState({carrier}); "
                      f"return s.commandedTask and 'pending' or 'dropped'") == "pending",
           "the return order is still pending (a request, not a cancellation)")

    # Drain the injury/combat streams WHILE waiting, not after. A wound
    # death reports through injury.drainEvents (falls, hazards, wound
    # deaths), NOT the event log, and these are DRAINED streams -- one
    # sample after the fact sees nothing, and in engine B they are empty
    # anyway because player events do not survive save/load.
    drained: list = []

    def _resumed():
        for stream in ("injury", "combat"):
            got = send_json(port,
                            f"local t={stream}.drainEvents() or {{}}; local o={{}}; "
                            f"for _,e in ipairs(t) do o[#o+1]=tostring(e.kind or '?')"
                            f"..'/'..tostring(e.cause or e.severity or e.text or '') "
                            f"end; return o")
            if isinstance(got, list) and got:
                drained.extend(f"{stream}:{x}" for x in got)
        return (current_action(port, carrier) == "follow_command"
                and (lambda p: p and dist(p, home) < dist(at_interrupt, home) - 2.0)(
                    unit_pos(port, carrier)))

    resumed = poll_until(120.0, _resumed)
    # Diagnostics in the message: a stalled resume is almost always the
    # carrier being in some OTHER state, and "distance unchanged" alone
    # doesn't say which.
    pose = send(port, f"return unit.getPose({carrier})")
    hun = send(port, f"return unit.getStat({carrier}, 'hunger')")
    chk.ok(bool(resumed),
           f"the carrier resumes the return leg after eating "
           f"({dist(at_interrupt, home):.1f} -> "
           f"{dist(unit_pos(port, carrier), home):.1f} tiles from home; "
           f"action={current_action(port, carrier)}, pose={pose}, "
           f"hunger={hun}/{maxhun:.1f}; streams={drained[-8:]})")

    # The inbound leg gets the same scrutiny as the outbound one: a real
    # walk home, sampled over many ticks, not a snap-back. Bounded so the
    # save below still lands with most of the journey unwalked — the
    # restart has to have something left to finish. The distance bar is
    # modest on purpose: a carrier that has just walked 27 tiles and
    # stopped to eat resumes at well under comfort speed (0.1-0.35
    # tiles/s observed across runs, depending on how tired it is), so
    # this window is a tile or two, not a dozen. What it establishes is
    # DIRECTION — that the carrier is closing on home again rather than
    # standing still or drifting; check 4b's full post-restart leg is
    # where the substantial closure gets asserted.
    assert_real_travel(chk, sample_walk(port, carrier, 20.0, interval=0.8), home,
                       "the inbound leg", min_samples=8, min_closed=1.0)
    chk.ok(find_instance(inventory(port, carrier), instance_id) is not None,
           "and the recovered item is still carried once the walk resumes")
    return True


# --------------------------------------------------------------------------
# Check 5 — usable as a locally produced item
# --------------------------------------------------------------------------
def notify_phase(port: int, uid: int) -> str:
    return send(port, f"local s=require('scripts.unit_ai').getState({uid}); "
                      f"return (s and s.notifyPhase) or 'none'")


def water_source_count(port: int, uid: int) -> int:
    raw = send(port, f"local s=require('scripts.unit_ai').getState({uid}); "
                     f"return (s and s.knownWaterSources and #s.knownWaterSources) or 0")
    try:
        return int(float(raw))
    except (TypeError, ValueError):
        return 0


def check_usable(chk: Checks, port: int, bid: int, home, instance_id, recovered,
                 carrier: int):
    """Withdraw the exact stored instance to a DIFFERENT colonist and
    drive an existing consumer with it. notify_allies' radio branch keys
    purely on 'an inventory item whose defName is radio' — it cannot
    tell a recovered instance from a locally produced one, which is the
    point."""
    hx, hy = home
    operator = spawn_unit(port, hx + 2, hy + 1)
    # The recipient sits well out of walk-notify range for the observation
    # window below: the no-radio branch walks to uninformed allies in
    # person, and at ~0.7 tiles/s it must not be able to reach this one
    # and inform it the slow way while we're proving the radio is what
    # made the difference.
    recipient = -1
    for ox, oy in ((20, 0), (0, 20), (-20, 0), (0, -20), (14, 14)):
        recipient = spawn_unit(port, hx + ox, hy + oy, clear_water=False)
        if recipient > 0:
            break
    if not chk.ok(operator > 0 and recipient > 0,
                  "two more colonists spawn at the colony for the consumer check"):
        return
    # The recipient must be the ONLY uninformed acolyte, or walk-notify
    # has a nearer target and this stops being a test of the radio: the
    # carrier is standing right next to the operator with no water source
    # of its own, so hand it one.
    send(port, f"local ai=require('scripts.unit_ai'); local s=ai.getState({carrier}); "
               f"if s then s.knownWaterSources={{{{x={hx+6},y={hy+6}}}}} end; return 'ok'")
    chk.ok(poll_until(15.0, lambda: water_source_count(port, recipient) == 0
                      and send(port, f"local s=require('scripts.unit_ai')"
                                     f".getState({recipient}); "
                                     f"return s and 'ready' or 'none'") == "ready")
           is not None,
           "the recipient has live AI state and is the only uninformed colonist")
    chk.ok(find_instance_by_def(inventory(port, recipient), "radio", set()) is not None,
           "the recipient carries a radio, so a broadcast can reach it")
    # The operator must NOT already have a radio of its own, or the
    # recovered one proves nothing.
    send(port, f"unit.removeItem({operator}, 'radio'); return 'ok'")
    chk.ok(find_instance_by_def(inventory(port, operator), "radio", set()) is None,
           "the operator carries no radio of its own")

    send(port, f"local ai=require('scripts.unit_ai'); local s=ai.getState({operator}); "
               f"s.knownWaterSources={{{{x={hx+6},y={hy+6}}}}}; "
               f"s.notifyPhase=nil; ai.setGoal(s,'notify_allies'); return 'ok'")
    broadcast_without = poll_until(
        12.0, lambda: notify_phase(port, operator) == "broadcasting")
    chk.ok(broadcast_without is None,
           "without a radio the operator never reaches the broadcast branch")

    chk.ok(water_source_count(port, recipient) == 0,
           "precondition: the recipient is still uninformed when the radio arrives "
           "(walk-notify never got there)")

    ok = send(port, f"return unit.withdrawFromCargo({operator}, {bid}, "
                    f"'{TARGET_DEF}', {instance_id})")
    chk.ok(ok.strip() == "true",
           f"a different colonist withdraws the EXACT recovered instance "
           f"({instance_id}) from colony storage (returned {ok!r})")
    held = find_instance(inventory(port, operator), instance_id)
    chk.ok(held is not None and properties(held) == properties(recovered),
           f"the withdrawn item is the same instance with the same properties "
           f"({properties(held)})")

    send(port, f"local ai=require('scripts.unit_ai'); local s=ai.getState({operator}); "
               f"s.notifyPhase=nil; ai.setGoal(s,'notify_allies'); return 'ok'")
    # Which BRANCH ran is proven by range, not by catching the
    # `broadcasting` phase: notify_broadcast_seconds is 1.0, so that phase
    # is a one-second window a console round trip can miss. The radio
    # branch reaches every radio-bearer at unlimited range; the walk
    # branch has to stand next to them. So: the recipient gets informed
    # while the operator is still tens of tiles away.
    informed = poll_until(60.0, lambda: water_source_count(port, recipient) > 0)
    op_pos, rc_pos = unit_pos(port, operator), unit_pos(port, recipient)
    gap = dist(op_pos, rc_pos) if (op_pos and rc_pos) else -1.0
    chk.ok(bool(informed),
           f"holding the recovered radio, the operator informs another colonist — "
           f"the consumer cannot tell it from a locally produced one "
           f"(operator action={current_action(port, operator)}, "
           f"notifyPhase={notify_phase(port, operator)}, "
           f"hasRadio={find_instance_by_def(inventory(port, operator), 'radio', set()) is not None}, "
           f"recipient pose={send(port, f'return unit.getPose({recipient})')})")
    chk.ok(bool(informed) and gap > 5.0,
           f"and it did so over the RADIO, not by walking there: the operator was "
           f"{gap:.1f} tiles from the recipient, far outside walk-notify's "
           f"stand-next-to-them transfer")


# --------------------------------------------------------------------------
# main
# --------------------------------------------------------------------------
def main() -> int:
    ap = argparse.ArgumentParser(description=__doc__)
    ap.add_argument("--seed", type=int, default=42)
    ap.add_argument("--size", type=int, default=64)
    ap.add_argument("--plates", type=int, default=3)
    ap.add_argument("--port", type=int, default=9920)
    ap.add_argument("--keep-root", action="store_true",
                    help="don't delete the throwaway resource root on exit")
    args = ap.parse_args()

    chk = Checks()
    base = tempfile.mkdtemp(prefix="synarchy_expedition_")
    root = make_isolated_root(base)
    print(f"isolated resource root: {root}", flush=True)

    carrier = -1
    bid = -1
    home = None
    recovered = None
    try:
        # ================= engine A: build and run the expedition ======
        print("\n--- engine A: real world, ruin, colony, expedition ---", flush=True)
        proc = boot_probe(root, args.port, LOG_A, "engine A")
        try:
            bootstrap(args.port)
            send(args.port,
                 f"world.init('{PAGE}', {args.seed}, {args.size}, {args.plates}, "
                 f"'Expedition Test World'); return 'ok'")
            send(args.port, "return world.waitForInit(400)", timeout=420)
            send(args.port, f"world.show('{PAGE}'); return 'ok'")

            site = pick_site(chk, args.port)
            if not site:
                return 2
            ruin, target, home = site
            ruin_xy = (float(ruin["gx"]), float(ruin["gy"]))

            bid = build_colony(chk, args.port, home)
            if bid < 0:
                return 2
            spot, foot = adjacent_tile(args.port, bid)
            carrier = spawn_unit(args.port, spot[0] + 0.5, spot[1] + 0.5)
            if not chk.ok(carrier > 0, f"a carrier spawns at the colony {spot}"):
                return 2

            print("\n[1] capacity is legible BEFORE the trip", flush=True)
            if not check_capacity_gate(chk, args.port, carrier, target, ruin_xy):
                return 2

            print("\n[2] travel, pickup, and carrier identity", flush=True)
            recovered = check_outbound(chk, args.port, carrier, target)
            if recovered is None:
                return 2
            instance_id = recovered["instanceId"]

            print("\n[3] a survival need interrupts the return leg", flush=True)
            check_interrupted_return(chk, args.port, carrier, spot, instance_id)

            print("\n[4a] save mid-journey, on the inbound leg", flush=True)
            here = unit_pos(args.port, carrier)
            remaining = dist(here, spot)
            chk.ok(remaining > 8.0,
                   f"the save happens with the journey unfinished "
                   f"({remaining:.1f} tiles still to walk)")
            chk.ok(find_instance(inventory(args.port, carrier), instance_id) is not None,
                   "the recovered instance is on the carrier at save time")
            saved = send(args.port,
                         f"return engine.saveWorld('{PAGE}', '{SLOT}')")
            chk.ok(saved.strip() == "true", f"engine.saveWorld accepted ({saved!r})")
            rid = capture_request_id(args.port, "return engine.getSaveStatus()")
            done, status = wait_save_complete(args.port, rid)
            chk.ok(done, f"save {rid} reached SaveCaptureComplete ({status})")
            check_ai_tick_clean(chk, LOG_A, "engine A")
        finally:
            quit_engine(args.port, proc)

        # ================= engine B: fresh process, load, finish =======
        print("\n--- engine B: fresh process loads the save and finishes ---",
              flush=True)
        proc = boot_probe(root, args.port, LOG_B, "engine B")
        try:
            bootstrap(args.port)
            send(args.port, f"engine.loadSave('{SLOT}'); return 'queued'")
            published, status = wait_load_published(args.port, 180)
            if not chk.ok(published, f"the save loads and publishes ({status})"):
                return 2
            send(args.port, f"world.show('{PAGE}'); return 'ok'")
            # Same reason as the outbound provisioning: the carrier still
            # has the whole inbound leg to walk and no way to seek water.
            provision(args.port, carrier)
            pose = alive(args.port, carrier)
            chk.ok(pose not in ("dead", "collapsed"),
                   f"the carrier survived the restart on its feet (pose {pose!r}; "
                   f"{death_report(args.port, carrier)})")
            # Engine A explicitly loaded the chunks the expedition walks
            # through; engine B has to as well. After a load, chunks queue
            # progressively around the restored camera, and a carrier
            # whose route is not resident cannot path along it — it just
            # shuffles in place with its order still pending.
            spot, foot = adjacent_tile(args.port, bid)
            here = unit_pos(args.port, carrier) or spot
            cxs = sorted({int(math.floor(here[0] / CHUNK)), int(math.floor(spot[0] / CHUNK))})
            cys = sorted({int(math.floor(here[1] / CHUNK)), int(math.floor(spot[1] / CHUNK))})
            send(args.port, f"return world.loadChunksInRegion("
                            f"{cxs[0]-1},{cys[0]-1},{cxs[-1]+1},{cys[-1]+1})")
            send(args.port, "return world.waitForChunks(120)", timeout=130)
            send(args.port, "engine.setPaused(false); return 'ok'")

            print("\n[4b] the expedition survives the restart and completes",
                  flush=True)
            held = find_instance(inventory(args.port, carrier), instance_id)
            chk.ok(held is not None,
                   f"after the restart the same instance ({instance_id}) is still "
                   f"on the same carrier ({carrier}): {held}")
            chk.ok(held is not None and properties(held) == properties(recovered),
                   f"the recovered item's properties survived the round trip "
                   f"({properties(held)})")
            chk.ok(send(args.port,
                        f"local s=require('scripts.unit_ai').getState({carrier}); "
                        f"return s and s.commandedTask and 'pending' or 'dropped'")
                   == "pending",
                   "the return intent is still pending after the load")

            b_samples: list = []
            b_actions: list = []

            def _approach():
                p = unit_pos(args.port, carrier)
                if p:
                    b_samples.append(p)
                a = current_action(args.port, carrier)
                if not b_actions or b_actions[-1] != a:
                    b_actions.append(a)
                return bool(p) and is_adjacent(p, foot)

            arrived = poll_until(240.0, _approach, interval=1.0)
            chk.ok(bool(arrived),
                   f"the carrier finishes the return on its own and arrives "
                   f"adjacent to colony storage (at {unit_pos(args.port, carrier)}, "
                   f"footprint {foot}; actions: {' -> '.join(b_actions[:12])})")
            assert_real_travel(chk, b_samples, spot,
                               "the post-restart return leg",
                               min_samples=5, min_closed=3.0)

            # A lax AI verb (D-7): the engine API has no adjacency gate
            # of its own, so the adjacency is asserted HERE as part of
            # the deposit — otherwise a carrier that never arrived could
            # still "deposit" from across the map and the check would
            # pass vacuously. Since #1249 no player MENU makes this call
            # (see the module docstring); the rule this reproduces is
            # the probe's own, not a menu's.
            at_deposit = unit_pos(args.port, carrier)
            adj = bool(at_deposit) and is_adjacent(at_deposit, foot)
            ok = send(args.port, f"return unit.depositToCargo({carrier}, {bid}, "
                                 f"'{TARGET_DEF}', {instance_id})")
            chk.ok(adj and ok.strip() == "true",
                   f"the carrier deposits it into existing colony storage from an "
                   f"adjacent tile (adjacent={adj} at {at_deposit}, returned {ok!r})")
            stored = send_json(args.port, f"return building.getStorage({bid})")
            stored = stored if isinstance(stored, list) else []
            match = find_instance(stored, instance_id)
            chk.ok(match is not None,
                   f"the exact instance is in colony storage: {match}")
            chk.ok(find_instance(inventory(args.port, carrier), instance_id) is None,
                   "and is no longer on the carrier")

            print("\n[5] the recovered item is usable as a local one", flush=True)
            check_usable(chk, args.port, bid, spot, instance_id, recovered, carrier)
            check_ai_tick_clean(chk, LOG_B, "engine B")
        finally:
            quit_engine(args.port, proc)
    finally:
        if args.keep_root:
            print(f"kept resource root: {base}", flush=True)
        else:
            shutil.rmtree(base, ignore_errors=True)

    print(f"\n--- {'PASS' if chk.failed == 0 else 'FAIL'}: "
          f"expedition retrieval and return end to end "
          f"({chk.failed} failing check(s)) ---")
    return 0 if chk.failed == 0 else 1


if __name__ == "__main__":
    sys.exit(main())
