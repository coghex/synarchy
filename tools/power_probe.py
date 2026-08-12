#!/usr/bin/env python3
"""Power-node placement + network probe (#358/#360) — the build-tool-
driven path, not just the raw Lua API.

#358 ships the power-node registry (Power.Types), the power.* Lua verbs
(isPlaceable / placeNode / getNode / getNodeForBuilding / listNodes), and
the ACTUAL player-facing placement path: scripts/build_tool.lua's
buildTool.commitPlacement routes a solar_panel / high_voltage_battery
placement through power.placeNode against the currently-selected unit
(consuming the item). Ordinary non-power buildings are handled by the
normal build-tool paths (starting-building spawn or construction
designation), not by this power placement helper. #360 adds wire
connectivity + the energy balance tick (Power.Network): this probe wires
the placed nodes together with scripts/wire.lua's M.place (the same verb
the chop/construct-style wire designation job calls), confirms
power.listNetworks/getNetworkForNode report the connected component, and
fast-forwards the world clock to show a wired battery's storedWh actually
rise under real daylight generation. It then proves everything (nodes AND
their charge) through a save -> quit -> fresh-restart -> load round-trip
(the gold-standard save check, mirroring multiworld_save_probe.py) since a
power node is only "network-attachment-ready" if it actually reconnects to
its building after a reload.

Brownout-under-load isn't probed here: #361 (the generic requires_power
consumer) doesn't exist yet, so there's no real drain to attach to a
network. That side of the balance math (charge, hold, brownout under a
synthetic drain) is covered by the pure hspec suite
(Test.Headless.Power.Network), which needs no real consumer to exercise it.

What it does:
  1. Boots a headless engine, loads defs, builds a flat arena, spawns a
     technomule (its starting kit carries the new items — #358).
  2. power.isPlaceable: true for the two power items, false for an
     ordinary building (furnace).
  3. buildTool.commitPlacement with NO unit selected refuses a power-item
     placement (no building appears, inventory untouched).
  4. With the technomule selected, commitPlacement places a solar panel
     and a battery — each consumes exactly one matching item and
     registers a node reporting the right role + parameters
     (power.getNode / getNodeForBuilding).
  5. Exhausting the mule's remaining solar panels makes the next
     commitPlacement refuse (inventory unaffected).
  6. Wire connects the panel + battery: power.listNetworks/
     getNetworkForNode report them on ONE network; the unwired second
     panel reports no network at all.
  7. Fast-forwarding the world clock (world.setTimeScale) over real
     daylight hours shows the wired battery's storedWh actually rise.
  7b. Demolishing a node's host retires the node (#1206): building.destroy
     on the UNWIRED second panel (chosen so no network membership or
     charge is disturbed), waited out until the queued BuildingDestroy
     genuinely completed, leaves getNodeForBuilding nil and drops the row
     from listNodes while the battery's node is untouched.
  8. Save -> quit -> fresh restart -> reload defs -> load: every surviving
     building, its power node, AND the battery's charged storedWh survive,
     reconnected by BuildingId — while the demolished panel's building AND
     node are both absent, and its retired node id was neither restored
     nor handed to anything else. The save is taken AFTER 7b, so this is
     the fresh-process half of #1206 requirement 2: cleanup happened in
     the live destruction transaction, not at load time.
  9. Longitude-local generation (#794), on a SEPARATE small real world
     (worldSize=8) rather than the arena above (whose synthetic
     wgpWorldSize=100000 makes any two in-arena tiles' longitudes
     practically identical): two solar panels at the meridian and the
     EXACT antipodal point of the cylinder (u=0 vs u=64) each land on
     their own isolated network, and each network's generationW tracks
     ITS OWN tile's world.getSunAngleAt reading — and the two panels'
     generationW differ by a wide margin, not one shared global value.
     Runs after the arena's save (step 8) completes, so this second page
     never rides along in that save file.
 10. Wire topology survives real chunk EVICTION (#1207), on a THIRD page
     — a worldSize=64 real world, because the arena cannot prove this:
     an arena's chunks are all reconstructed at load behind a 100-chunk
     cache, so its wire chunk is never demonstrably unloaded. A source +
     storage pair joined by a five-tile wire run inside ONE chunk is
     built, then the camera is driven into fresh territory until
     world.getChunkInfo reports that chunk genuinely `loaded=false`, and:
     both membership queries (getNetworkForNode AND listNetworks) still
     report the network; the battery's storedWh keeps rising under
     unpaused game time while detached; freezing the clock and walking
     the camera back reloads the chunk with membership and stored energy
     unchanged (no discontinuity); clearing ONE middle wire piece splits
     the run into two networks and it STAYS split across another
     eviction and reload (a stale historical WeSetStructure must never
     resurrect connectivity); and structure.clearAll leaves no topology
     derivable at all, evicted or resident.
 11. That same page is then saved with the camera parked far away and
     reloaded in engine B (a FRESH process that never held the page):
     the wire chunk comes up `loaded=false` and the network is still
     reported without the camera ever visiting it or the chunk being
     explicitly loaded.

Usage: python3 tools/power_probe.py [--port 9358]
Exit 0 = every check passed.
"""
from __future__ import annotations

import argparse
import glob
import json
import math
import os
import shutil
import socket
import subprocess
import sys
import tempfile
import time
import uuid
from pathlib import Path
from probelib import quit_engine, boot, send, wait_load_published

SAVE_PREFIX = "power_probe_"  # save dirs this probe owns (cleanup scoped to it)
REPO = Path(__file__).resolve().parent.parent


def make_isolated_root(base: str) -> str:
    """A throwaway resource root: real scripts/assets/data/config
    (symlinked -- read-only content, safe to share) plus its OWN empty
    saves/ directory, so this probe never touches a real player's saves
    (round-6 review, issue #767 requirement 15's cross-referenced-probe
    isolation gap)."""
    root = os.path.join(base, "root")
    os.makedirs(root, exist_ok=True)
    for family in ("scripts", "assets", "data", "config"):
        target = os.path.join(root, family)
        if not os.path.exists(target):
            os.symlink(os.path.join(REPO, family), target)
    os.makedirs(os.path.join(root, "saves"), exist_ok=True)
    return root


def jget(port: int, lua: str, timeout: float = 10.0):
    raw = send(port, lua, timeout)
    try:
        return json.loads(raw)
    except json.JSONDecodeError:
        return raw.strip('"')


def as_int(s) -> int | None:
    try:
        return int(float(s))
    except (TypeError, ValueError):
        return None


def bootstrap_defs(port: int) -> None:
    """Load the defs needed to spawn AND to re-resolve saved buildings on
    load (fromBuildingSnapshot drops any entity whose def isn't
    registered — the load side needs these too)."""
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


def wait_active(port: int, page: str, secs: float = 10.0) -> bool:
    deadline = time.time() + secs
    while time.time() < deadline:
        if send(port, "return world.getActiveWorldId()").strip('"') == page:
            return True
        time.sleep(0.2)
    return False


def count_item(port: int, uid: int, name: str) -> int:
    return as_int(send(port,
        f"local c=0; for _,it in ipairs(unit.getInventory({uid}) or {{}}) do "
        f"if it.defName=='{name}' then c=c+1 end end; return c")) or 0


def check(passed: bool, ok: bool, label: str, detail: str = "") -> bool:
    print(f"  [{'PASS' if ok else 'FAIL'}] {label}" + (f": {detail}" if detail else ""))
    return passed and ok


# ── #1207 eviction fixture ──────────────────────────────────────────────
# A worldSize=64 REAL world (the arena cannot show eviction — see the
# module docstring). Seed 42's chunk (2,-1) is pre-verified completely dry
# and flat at z=30, so the whole network — both nodes AND every wire tile
# — lives inside that ONE chunk and a single eviction detaches all of it.
# Topology is 2D (Power.Network.wireComponents is 4-adjacency on (gx,gy)
# and a node's tile is its building anchor), so the z-level is incidental;
# what matters is that placement succeeds and the tiles share a chunk.
EV_PAGE = "evict_check"
EV_SEED, EV_SIZE, EV_PLATES = 42, 64, 4
EV_CHUNK = (2, -1)
EV_PANEL_TILE = (36, -8)
EV_BATT_TILE = (40, -8)
EV_WIRE_RUN = [(36, -7), (37, -7), (38, -7), (39, -7), (40, -7)]
EV_CUT_TILE = (38, -7)          # middle piece: clearing it splits the run
EV_HOME_TILE = (38, -8)         # camera target that reloads EV_CHUNK

# Chunks far enough from EV_CHUNK to fill the 200-chunk cache without
# ever competing with it for eviction: 225 chunks at |cy| >= 10, all
# inside this world's valid u/v diamond so none of them aliases another.
EV_FILL_REGION = (-7, 10, 7, 24)

# Camera stops that force eviction. Each is 3 chunks further east than
# the last, so every stop needs NEW chunks generated — eviction only runs
# on a tick that actually generates a batch, so re-parking on already-
# loaded ground would be a silent no-op. Every stop is also much closer
# to EV_CHUNK than any EV_FILL_REGION chunk is, which is what puts
# EV_CHUNK in the evicted set under the existing distance policy.
EV_CAMERA_STOPS = [(cx * 16 + 8, -8) for cx in (10, 13, 16, 19, 22, 25)]


def chunk_loaded(port: int, coord: tuple[int, int]) -> bool | None:
    info = jget(port, f"return world.getChunkInfo({coord[0]}, {coord[1]})")
    if not isinstance(info, dict):
        return None
    return bool(info.get("loaded"))


def force_eviction(port: int, coord: tuple[int, int], stop_from: int = 0,
                    secs: float = 25.0) -> int | None:
    """Drive the camera through fresh territory until @coord is GENUINELY
    unloaded. Returns the index of the next unused camera stop, or None if
    the chunk never evicted. Never asserts on its own — the caller reports
    the outcome as a check so a policy change reads as a probe failure
    rather than a hang."""
    for idx in range(stop_from, len(EV_CAMERA_STOPS)):
        gx, gy = EV_CAMERA_STOPS[idx]
        send(port, f"camera.goToTile({gx}, {gy}); return 'ok'")
        deadline = time.time() + secs
        while time.time() < deadline:
            if chunk_loaded(port, coord) is False:
                return idx + 1
            time.sleep(0.25)
    return None


def reload_chunk(port: int, coord: tuple[int, int], tile: tuple[int, int],
                  secs: float = 30.0) -> bool:
    """Walk the camera back onto @tile and wait for @coord to come back."""
    send(port, f"camera.goToTile({tile[0]}, {tile[1]}); return 'ok'")
    deadline = time.time() + secs
    while time.time() < deadline:
        if chunk_loaded(port, coord) is True:
            return True
        time.sleep(0.25)
    return False


def net_members(port: int, node_id: int) -> list[int] | None:
    """The sorted node-id set of the network @node_id sits on, or None when
    it is on no network at all."""
    net = jget(port, f"return power.getNetworkForNode({node_id})")
    if not isinstance(net, dict):
        return None
    return sorted(net.get("nodeIds", []))


def listed_members(port: int) -> list[list[int]]:
    """Every network listNetworks reports, as sorted node-id sets — the
    second membership surface requirement 1 names, checked alongside
    getNetworkForNode so the two cannot disagree."""
    nets = jget(port, "return power.listNetworks()")
    if not isinstance(nets, list):
        return []
    return sorted(sorted(n.get("nodeIds", [])) for n in nets
                   if isinstance(n, dict))


def stored_wh(port: int, bid: int) -> float | None:
    node = jget(port, f"return power.getNodeForBuilding({bid})")
    if not isinstance(node, dict):
        return None
    value = node.get("storedWh")
    return float(value) if isinstance(value, (int, float)) else None


def main() -> int:
    ap = argparse.ArgumentParser(description=__doc__,
                                  formatter_class=argparse.RawDescriptionHelpFormatter)
    ap.add_argument("--port", type=int, default=9358)
    args = ap.parse_args()
    port = args.port

    tmpdir = tempfile.mkdtemp(prefix="power_probe_")
    try:
        root = make_isolated_root(tmpdir)
        return _run(port, root)
    finally:
        shutil.rmtree(tmpdir, ignore_errors=True)


def _run(port: int, root: str) -> int:
    passed = True

    save_name = f"{SAVE_PREFIX}{uuid.uuid4().hex[:12]}"
    save_dir = os.path.join(root, "saves", save_name)
    if os.path.exists(save_dir):
        sys.exit(f"refusing to run: {save_dir} already exists")

    # A SECOND save for the #1207 fresh-process check (step 11): it has to
    # be taken with the camera far from the wire chunk, which the arena
    # save above deliberately isn't, and it must not disturb any of that
    # save's existing assertions.
    ev_save_name = f"{SAVE_PREFIX}evict_{uuid.uuid4().hex[:12]}"
    ev_save_dir = os.path.join(root, "saves", ev_save_name)
    if os.path.exists(ev_save_dir):
        sys.exit(f"refusing to run: {ev_save_dir} already exists")

    logA = "/tmp/power_probe_A.log"
    logB = "/tmp/power_probe_B.log"
    procA = procB = None
    try:
        # ── Engine A: place nodes through the build tool, save ──────────
        procA = boot(port, log=logA, label="engine A", args=["--resource-root", root])
        bootstrap_defs(port)
        send(port, "world.initArena('power_probe'); return 'ok'")
        send(port, "world.show('power_probe'); return 'ok'")
        if not wait_active(port, "power_probe"):
            sys.exit("FAIL: power_probe arena never became active")

        uid = as_int(send(port, "return unit.spawn('technomule', 5, 5)"))
        if uid is None:
            sys.exit("FAIL: technomule spawn rejected")
        # starting_inventory materializes asynchronously on the unit
        # thread — wait for it rather than racing the first query.
        for _ in range(50):
            if count_item(port, uid, "solar_panel") >= 2:
                break
            time.sleep(0.1)

        passed = check(passed, count_item(port, uid, "solar_panel") == 2,
                        "mule starting kit carries 2 solar_panel")
        passed = check(passed, count_item(port, uid, "high_voltage_battery") == 2,
                        "mule starting kit carries 2 high_voltage_battery")

        # --- 2. power.isPlaceable ---
        passed = check(passed,
            send(port, "return power.isPlaceable('solar_panel')") == "true",
            "isPlaceable(solar_panel)")
        passed = check(passed,
            send(port, "return power.isPlaceable('high_voltage_battery')") == "true",
            "isPlaceable(high_voltage_battery)")
        passed = check(passed,
            send(port, "return power.isPlaceable('furnace')") == "false",
            "isPlaceable(furnace) is false (ordinary building)")

        # --- 3. commitPlacement with NO unit selected ---
        send(port, "unit.deselectAll(); return 'ok'")
        r = jget(port,
            "local id,err = require('scripts.build_tool').commitPlacement("
            "'solar_panel', 7, 5); return {ok = (id ~= nil), err = err}")
        passed = check(passed, isinstance(r, dict) and r.get("ok") is False,
                       "commitPlacement(solar_panel) refused with no selection", r)
        passed = check(passed, count_item(port, uid, "solar_panel") == 2,
                       "refused placement left the mule's inventory untouched")
        passed = check(passed,
            send(port, "return building.getInfo(1) and 'yes' or 'no'") == "no",
            "no building appeared at (7,5) after the refusal")
        # --- 4. Select the mule; place a source + a storage node ---
        send(port, f"unit.select({uid}); return 'ok'")
        panel_bid = as_int(send(port,
            "return require('scripts.build_tool').commitPlacement("
            "'solar_panel', 7, 5)"))
        passed = check(passed, panel_bid is not None,
                       "commitPlacement(solar_panel) with mule selected", panel_bid)
        passed = check(passed, count_item(port, uid, "solar_panel") == 1,
                       "one solar_panel consumed from the mule")
        node = jget(port, f"return power.getNodeForBuilding({panel_bid})")
        passed = check(passed,
            isinstance(node, dict) and node.get("role") == "source"
            and node.get("peakWatts") == 400 and node.get("capacityWh") == 0,
            "solar panel node reports role=source, 400 W, 0 Wh", node)

        batt_bid = as_int(send(port,
            "return require('scripts.build_tool').commitPlacement("
            "'high_voltage_battery', 8, 5)"))
        passed = check(passed, batt_bid is not None,
                       "commitPlacement(high_voltage_battery) with mule selected",
                       batt_bid)
        passed = check(passed, count_item(port, uid, "high_voltage_battery") == 1,
                       "one high_voltage_battery consumed from the mule")
        node = jget(port, f"return power.getNodeForBuilding({batt_bid})")
        passed = check(passed,
            isinstance(node, dict) and node.get("role") == "storage"
            and node.get("capacityWh") == 5000 and node.get("peakWatts") == 0,
            "battery node reports role=storage, 5000 Wh, 0 W", node)

        # --- 5. Exhausting inventory refuses further placement ---
        second_panel_bid = as_int(send(port,
            "return require('scripts.build_tool').commitPlacement("
            "'solar_panel', 10, 5)"))
        passed = check(passed, second_panel_bid is not None,
                       "second solar_panel placement (last one in the mule)",
                       second_panel_bid)
        passed = check(passed, count_item(port, uid, "solar_panel") == 0,
                       "mule now carries 0 solar_panel")
        r = jget(port,
            "local id,err = require('scripts.build_tool').commitPlacement("
            "'solar_panel', 11, 5); return {ok = (id ~= nil), err = err}")
        passed = check(passed, isinstance(r, dict) and r.get("ok") is False,
                       "third solar_panel placement refused (mule is empty)", r)

        expected_nodes = 3  # 2 solar panels + 1 battery
        node_count = as_int(send(port, "local ns=power.listNodes(); return #ns"))
        passed = check(passed, node_count == expected_nodes,
                       f"listNodes reports {expected_nodes} nodes", node_count)

        # --- 6. Wire connectivity (#360): join the panel (7,5) and the
        # battery (8,5) with a two-tile wire run just south of them, and
        # confirm both land on ONE network. M.place is the same verb the
        # wire designation job calls (scripts/unit_ai_construct.lua) — calling it
        # directly here skips the job/AI machinery, matching how other
        # probes call a tool module's placement function directly.
        for gx, gy in [(7, 6), (8, 6)]:
            send(port, f"require('scripts.wire').place({gx}, {gy}); return 'ok'")

        panel_node = jget(port, f"return power.getNodeForBuilding({panel_bid})")
        batt_node = jget(port, f"return power.getNodeForBuilding({batt_bid})")
        panel_net = jget(port, f"return power.getNetworkForNode({panel_node['id']})")
        batt_net = jget(port, f"return power.getNetworkForNode({batt_node['id']})")
        passed = check(passed,
            isinstance(panel_net, dict) and isinstance(batt_net, dict)
            and sorted(panel_net.get("nodeIds", [])) == sorted(batt_net.get("nodeIds", []))
            and len(panel_net.get("nodeIds", [])) == 2,
            "solar panel + battery share one network after wiring",
            {"panel_net": panel_net, "battery_net": batt_net})

        second_panel_node = jget(port,
            f"return power.getNodeForBuilding({second_panel_bid})")
        lone_net = jget(port,
            f"return power.getNetworkForNode({second_panel_node['id']})")
        passed = check(passed, lone_net is None,
                       "the unwired second solar panel has no network", lone_net)

        # --- 7. Charging over a simulated few hours of daylight ---
        stored_before = jget(port,
            f"return power.getNodeForBuilding({batt_bid}).storedWh")
        send(port, "world.setTimeScale('power_probe', 120); return 'ok'")
        time.sleep(2.5)
        send(port, "world.setTimeScale('power_probe', 0); return 'ok'")
        stored_after = jget(port, f"return power.getNodeForBuilding({batt_bid}).storedWh")
        passed = check(passed,
            isinstance(stored_before, (int, float))
            and isinstance(stored_after, (int, float))
            and stored_after > stored_before,
            "battery storedWh rose over simulated daylight "
            f"({stored_before} -> {stored_after})")

        # --- 7b. Demolition retires the node (#1206), LIVE. Runs after
        # every connectivity/charging assertion above and BEFORE the save
        # below, so the same save file carries both halves: the surviving
        # nodes (step 8) and this retired one, which the fresh process
        # must not restore. The host is the UNWIRED second panel — it is
        # on no network, so retiring it cannot perturb the wired pair's
        # membership or the battery's charge.
        battery_node_before = jget(port,
            f"return power.getNodeForBuilding({batt_bid})")
        demolished_node = jget(port,
            f"return power.getNodeForBuilding({second_panel_bid})")
        demolished_nid = (as_int(demolished_node.get("id"))
                          if isinstance(demolished_node, dict) else None)
        passed = check(passed, demolished_nid is not None,
                       "the panel about to be demolished has a node",
                       demolished_node)
        send(port, f"building.destroy({second_panel_bid}); return 'ok'")
        # building.destroy only ENQUEUES; the drain runs on the unit
        # thread. Wait for the instance to genuinely be gone rather than
        # racing it — the save below must be taken after the transaction
        # completed, or it would prove nothing about requirement 2.
        for _ in range(100):
            if send(port, f"return building.getInfo({second_panel_bid}) "
                          "and 'yes' or 'no'") == "no":
                break
            time.sleep(0.1)
        gone = send(port, f"return building.getInfo({second_panel_bid}) "
                          "and 'yes' or 'no'")
        passed = check(passed, gone == "no",
                       "BuildingDestroy completed for the second solar panel",
                       gone)
        passed = check(passed,
            jget(port, f"return power.getNodeForBuilding({second_panel_bid})")
                is None,
            "getNodeForBuilding reports nil for the demolished panel")
        listed_ids = jget(port,
            "local out={}; for _,n in ipairs(power.listNodes()) do "
            "out[#out+1]=n.id end; return out")
        passed = check(passed,
            isinstance(listed_ids, list) and demolished_nid not in listed_ids,
            "listNodes no longer contains the demolished panel's node",
            listed_ids)
        battery_node_after = jget(port,
            f"return power.getNodeForBuilding({batt_bid})")
        passed = check(passed,
            isinstance(battery_node_after, dict)
            and battery_node_after == battery_node_before,
            "the untouched battery node is unchanged by the demolition",
            {"before": battery_node_before, "after": battery_node_after})
        surviving_nodes = 2  # the wired panel + the battery
        passed = check(passed,
            as_int(send(port, "local ns=power.listNodes(); return #ns"))
                == surviving_nodes,
            f"listNodes reports {surviving_nodes} nodes after the demolition")

        # --- 8. Save -> quit -> fresh restart -> load ---
        saved = send(port, f"return engine.saveWorld('power_probe', '{save_name}')")
        passed = check(passed, saved.strip() == "true",
                       "engine.saveWorld returned true", saved)
        save_file = os.path.join(save_dir, "world.synworld")
        for _ in range(100):
            if os.path.exists(save_file):
                break
            time.sleep(0.1)
        passed = check(passed, os.path.exists(save_file),
                       f"save file appeared at {save_file}")

        # --- 9. Per-source longitude-local generation at REAL,
        # meaningfully-separated positions (#794). The arena above (#358/
        # #360's existing home) carries a synthetic wgpWorldSize of 100000
        # chunks (World.Thread.Command.Init) so its whole loaded footprint
        # spans a tiny fraction of that circumference — nowhere near enough
        # separation for a comparison against world.getSunAngleAt to
        # actually distinguish per-source local phasing from the pre-#794
        # bug (one shared global sun angle applied to every source). A
        # dedicated small REAL world (worldSize=8, circumference 128 tiles)
        # gives two ordinary tiles a genuinely different longitude: (10,10)
        # sits at the meridian (u=0) while (64,0) is EXACTLY the antipodal
        # point (u=64, half the circumference) — the "opposite sides of the
        # cylinder" case the issue calls out by name. Seed 42 + these exact
        # coordinates are pre-verified flat, dry, loaded land. This runs on
        # the SAME engine A instance, AFTER the arena's save already
        # completed above, so this second page never rides along in it.
        send(port, "world.init('longitude_check', 42, 8, 3); return 'ok'")
        send(port, "return world.waitForInit(120)", timeout=125)
        send(port, "world.show('longitude_check'); return 'ok'")
        if not wait_active(port, "longitude_check"):
            sys.exit("FAIL: longitude_check world never became active")
        # (64,0) is far from the default spawn/camera area and its chunk
        # won't load on demand without a camera nearby — load both target
        # chunks explicitly before placing anything there.
        #
        # #1175: (64,0) is a u-ALIAS at this world size — u = 64 is exactly
        # the wrap period, so the engine records the panel (and the wire
        # beside it) at the canonical twin (0,64)/(1,64). That is the
        # contract, not a surprise: every placement verb resolves to the
        # frame chunks are stored under, and the coords it reports back are
        # canonical. This phase deliberately keeps asking in the ALIAS
        # frame, because the point of the comparison is longitude, and an
        # alias names the same physical longitude — world.getSunAngleAt(64,0)
        # and the panel's own local intensity have to agree precisely
        # BECAUSE they are one place. If a future change made the alias
        # resolve somewhere else, these checks would catch it.
        send(port, "return world.loadChunksInRegion(-1, -1, 5, 1)")
        send(port, "return world.waitForChunks(60)", timeout=65)

        lc_uid = as_int(send(port, "return unit.spawn('technomule', 10, 10)"))
        if lc_uid is None:
            sys.exit("FAIL: longitude_check technomule spawn rejected")
        for _ in range(50):
            if count_item(port, lc_uid, "solar_panel") >= 2:
                break
            time.sleep(0.1)
        send(port, f"unit.select({lc_uid}); return 'ok'")

        meridian_bid = as_int(send(port,
            "return require('scripts.build_tool').commitPlacement("
            "'solar_panel', 10, 10)"))
        antipodal_bid = as_int(send(port,
            "return require('scripts.build_tool').commitPlacement("
            "'solar_panel', 64, 0)"))
        passed = check(passed, meridian_bid is not None and antipodal_bid is not None,
                       "both longitude-check solar panels placed",
                       {"meridian": meridian_bid, "antipodal": antipodal_bid})

        # Wire each panel into its OWN isolated network.
        send(port, "require('scripts.wire').place(11, 10); return 'ok'")
        send(port, "require('scripts.wire').place(65, 0); return 'ok'")

        send(port, "world.setTimeScale('longitude_check', 0); return 'ok'")
        send(port, "world.setTime('longitude_check', 9, 37); return 'ok'")
        time.sleep(0.3)

        generation = {}
        for label, node_bid, gx, gy in [
            ("meridian panel", meridian_bid, 10, 10),
            ("antipodal panel", antipodal_bid, 64, 0),
        ]:
            node = jget(port, f"return power.getNodeForBuilding({node_bid})")
            net = (jget(port, f"return power.getNetworkForNode({node['id']})")
                   if isinstance(node, dict) else None)
            sun_angle = jget(port, f"return world.getSunAngleAt({gx}, {gy})")
            expected_gen = (400.0 * max(0.0, -math.cos(2 * math.pi * float(sun_angle)))
                            if isinstance(sun_angle, (int, float)) else None)
            generation[label] = net.get("generationW") if isinstance(net, dict) else None
            passed = check(passed,
                isinstance(net, dict) and expected_gen is not None
                and abs(net.get("generationW", -1.0) - expected_gen) < 0.5,
                f"{label} ({gx},{gy}) network generationW tracks its own "
                "world.getSunAngleAt-derived local intensity",
                {"net": net, "sunAngle": sun_angle, "expected": expected_gen})

        # The bug this guards against: one shared global sun angle applied
        # to every source regardless of position would report the SAME
        # generationW for both panels. At u=0 vs u=64 (half the
        # circumference apart, the maximum possible separation) the two
        # panels' true local intensities are as far apart as they can get
        # (full peak vs. fully clamped to 0 at this clock time), so this
        # catches the regression with a wide margin.
        passed = check(passed,
            all(isinstance(v, (int, float)) for v in generation.values())
            and abs(generation["meridian panel"] - generation["antipodal panel"]) > 50.0,
            "meridian and antipodal panels report meaningfully DIFFERENT "
            "generationW, not one shared global value", generation)

        # --- 10. Wire topology is residency-INDEPENDENT (#1207) ---------
        # A third page, because neither earlier one can show this: the
        # arena's chunks are all rebuilt at load behind a 100-chunk cache,
        # and longitude_check (worldSize 8, 64 chunk slots) can never
        # exceed the 200-chunk cache eviction needs at all.
        send(port, f"world.init('{EV_PAGE}', {EV_SEED}, {EV_SIZE}, "
                   f"{EV_PLATES}); return 'ok'")
        send(port, "return world.waitForInit(300)", timeout=310)
        send(port, f"world.show('{EV_PAGE}'); return 'ok'")
        if not wait_active(port, EV_PAGE):
            sys.exit(f"FAIL: {EV_PAGE} world never became active")
        # Clock frozen at midday for the whole phase: the panel generates
        # (so the detached-charging check has something to measure) while
        # every "unchanged across the reload" comparison stays exact,
        # because a zero time scale makes the power tick a no-op.
        send(port, f"world.setTimeScale('{EV_PAGE}', 0); return 'ok'")
        send(port, f"world.setTime('{EV_PAGE}', 12, 0); return 'ok'")

        ev_uid = as_int(send(port,
            f"return unit.spawn('technomule', {EV_PANEL_TILE[0]}, "
            f"{EV_PANEL_TILE[1] - 1})"))
        if ev_uid is None:
            sys.exit(f"FAIL: {EV_PAGE} technomule spawn rejected")
        for _ in range(50):
            if count_item(port, ev_uid, "solar_panel") >= 1:
                break
            time.sleep(0.1)
        send(port, f"unit.select({ev_uid}); return 'ok'")

        ev_panel_bid = as_int(send(port,
            "return require('scripts.build_tool').commitPlacement("
            f"'solar_panel', {EV_PANEL_TILE[0]}, {EV_PANEL_TILE[1]})"))
        ev_batt_bid = as_int(send(port,
            "return require('scripts.build_tool').commitPlacement("
            f"'high_voltage_battery', {EV_BATT_TILE[0]}, {EV_BATT_TILE[1]})"))
        passed = check(passed,
            ev_panel_bid is not None and ev_batt_bid is not None,
            "eviction-page source + storage placed",
            {"panel": ev_panel_bid, "battery": ev_batt_bid})
        if ev_panel_bid is None or ev_batt_bid is None:
            sys.exit("FAIL: cannot continue the #1207 phase without both nodes")

        for gx, gy in EV_WIRE_RUN:
            send(port, f"require('scripts.wire').place({gx}, {gy}); return 'ok'")
        ev_panel_node = jget(port, f"return power.getNodeForBuilding({ev_panel_bid})")
        ev_batt_node = jget(port, f"return power.getNodeForBuilding({ev_batt_bid})")
        if not (isinstance(ev_panel_node, dict) and isinstance(ev_batt_node, dict)):
            sys.exit(f"FAIL: eviction-page nodes never registered: "
                     f"{ev_panel_node} / {ev_batt_node}")
        ev_panel_nid = as_int(ev_panel_node.get("id"))
        ev_batt_nid = as_int(ev_batt_node.get("id"))
        if ev_panel_nid is None or ev_batt_nid is None:
            sys.exit(f"FAIL: eviction-page node ids unreadable: "
                     f"{ev_panel_node} / {ev_batt_node}")
        joined = sorted([ev_panel_nid, ev_batt_nid])
        loaded_members = net_members(port, ev_panel_nid)
        passed = check(passed,
            chunk_loaded(port, EV_CHUNK) is True
            and loaded_members == joined
            and net_members(port, ev_batt_nid) == joined
            and listed_members(port) == [joined],
            "baseline: both nodes share ONE network while the wire chunk is loaded",
            {"members": loaded_members, "listed": listed_members(port)})

        # Fill the cache with chunks that are FAR from the wire chunk, then
        # drive the camera east through fresh ground until the wire chunk
        # itself is genuinely gone from wtdChunks.
        send(port, f"return world.loadChunksInRegion({EV_FILL_REGION[0]}, "
                   f"{EV_FILL_REGION[1]}, {EV_FILL_REGION[2]}, "
                   f"{EV_FILL_REGION[3]})", timeout=30)
        send(port, "return world.waitForChunks(300)", timeout=310)
        next_stop = force_eviction(port, EV_CHUNK)
        passed = check(passed, next_stop is not None,
                       "wire chunk genuinely EVICTED (getChunkInfo loaded=false)",
                       chunk_loaded(port, EV_CHUNK))
        if next_stop is None:
            sys.exit("FAIL: never forced a real eviction — the rest of the "
                     "#1207 phase would assert nothing")

        # Requirement 1: both membership surfaces still report the network.
        passed = check(passed,
            net_members(port, ev_panel_nid) == joined
            and net_members(port, ev_batt_nid) == joined,
            "getNetworkForNode reports the SAME network with the wire chunk evicted",
            {"panel": net_members(port, ev_panel_nid),
             "battery": net_members(port, ev_batt_nid)})
        passed = check(passed, listed_members(port) == [joined],
                       "listNetworks reports the same single network while evicted",
                       listed_members(port))

        # Requirement 1 (second half): stored charge keeps evolving with
        # UNPAUSED game time while detached — the pre-fix freeze. The
        # explicit unpause matters: step 8's save transaction leaves the
        # engine paused (pause is a one-way ratchet per save attempt), and
        # tickWorldTime forces effScale to 0 while paused, so without this
        # the whole window would measure a stopped clock rather than a
        # detached network.
        was_paused = send(port, "return engine.isPaused()").strip()
        send(port, "engine.setPaused(false); return 'ok'")
        detached_before = stored_wh(port, ev_batt_bid)
        send(port, f"world.setTimeScale('{EV_PAGE}', 120); return 'ok'")
        time.sleep(2.5)
        send(port, f"world.setTimeScale('{EV_PAGE}', 0); return 'ok'")
        # Pause before the reload comparisons below, so membership and
        # stored energy are compared with no tick timing ambiguity at all.
        send(port, "engine.setPaused(true); return 'ok'")
        time.sleep(0.5)
        detached_after = stored_wh(port, ev_batt_bid)
        detached_net = jget(port, f"return power.getNetworkForNode({ev_panel_nid})")
        passed = check(passed,
            detached_before is not None and detached_after is not None
            and detached_after > detached_before,
            "battery storedWh kept RISING while its wire chunk was evicted "
            f"({detached_before} -> {detached_after})",
            {"pausedOnEntry": was_paused,
             "generationW": (detached_net.get("generationW")
                             if isinstance(detached_net, dict) else None)})

        # Requirement 2: reloading introduces no discontinuity. The clock
        # is already frozen (timeScale 0) so the comparison carries no tick
        # timing ambiguity.
        frozen_members = net_members(port, ev_panel_nid)
        frozen_listed = listed_members(port)
        frozen_stored = stored_wh(port, ev_batt_bid)
        passed = check(passed, reload_chunk(port, EV_CHUNK, EV_HOME_TILE),
                       "wire chunk reloaded after walking the camera back",
                       chunk_loaded(port, EV_CHUNK))
        passed = check(passed,
            net_members(port, ev_panel_nid) == frozen_members
            and listed_members(port) == frozen_listed,
            "membership unchanged across the reload (no discontinuity)",
            {"before": frozen_members, "after": net_members(port, ev_panel_nid)})
        reloaded_stored = stored_wh(port, ev_batt_bid)
        passed = check(passed,
            frozen_stored is not None and reloaded_stored is not None
            and abs(reloaded_stored - frozen_stored) < 1e-3,
            "stored energy unchanged across the reload "
            f"({frozen_stored} -> {reloaded_stored})")

        # Requirement 3: a cleared piece STAYS cleared. Cutting the middle
        # of the run splits it, and no eviction/reload cycle may let the
        # earlier WeSetStructure resurrect the connection.
        send(port, f"return structure.clear({EV_CUT_TILE[0]}, "
                   f"{EV_CUT_TILE[1]}, 'wire')")
        time.sleep(1.0)
        passed = check(passed,
            net_members(port, ev_panel_nid) == [ev_panel_nid]
            and net_members(port, ev_batt_nid) == [ev_batt_nid],
            "clearing one middle wire piece splits the network in two",
            {"panel": net_members(port, ev_panel_nid),
             "battery": net_members(port, ev_batt_nid)})
        next_stop = force_eviction(port, EV_CHUNK, stop_from=next_stop)
        passed = check(passed, next_stop is not None,
                       "wire chunk evicted again after the clear",
                       chunk_loaded(port, EV_CHUNK))
        passed = check(passed,
            net_members(port, ev_panel_nid) == [ev_panel_nid]
            and net_members(port, ev_batt_nid) == [ev_batt_nid],
            "the cleared piece stays cleared while evicted — no stale "
            "set-structure edit resurrects the link",
            {"panel": net_members(port, ev_panel_nid),
             "battery": net_members(port, ev_batt_nid)})
        if next_stop is None:
            sys.exit("FAIL: could not re-evict the wire chunk after the clear")
        passed = check(passed, reload_chunk(port, EV_CHUNK, EV_HOME_TILE),
                       "wire chunk reloaded after the clear")
        passed = check(passed,
            net_members(port, ev_panel_nid) == [ev_panel_nid]
            and net_members(port, ev_batt_nid) == [ev_batt_nid],
            "still split after the evict -> reload round trip",
            {"panel": net_members(port, ev_panel_nid),
             "battery": net_members(port, ev_batt_nid)})

        # --- 11. Fresh-process load with the wire chunk still evicted ---
        # Re-join the run, park the camera far away so the SAVED camera
        # leaves the wire chunk out of the load's own chunk fill, and save.
        send(port, f"require('scripts.wire').place({EV_CUT_TILE[0]}, "
                   f"{EV_CUT_TILE[1]}); return 'ok'")
        time.sleep(1.0)
        passed = check(passed, net_members(port, ev_panel_nid) == joined,
                       "re-placing the cut piece rejoins the network",
                       net_members(port, ev_panel_nid))
        next_stop = force_eviction(port, EV_CHUNK, stop_from=next_stop)
        passed = check(passed, next_stop is not None,
                       "wire chunk evicted with the camera parked for the save",
                       chunk_loaded(port, EV_CHUNK))
        ev_saved_members = net_members(port, ev_panel_nid)
        ev_saved_stored = stored_wh(port, ev_batt_bid)
        ev_saved = send(port,
            f"return engine.saveWorld('{EV_PAGE}', '{ev_save_name}')")
        passed = check(passed, ev_saved.strip() == "true",
                       "engine.saveWorld returned true for the eviction page",
                       ev_saved)
        ev_save_file = os.path.join(ev_save_dir, "world.synworld")
        for _ in range(150):
            if os.path.exists(ev_save_file):
                break
            time.sleep(0.1)
        passed = check(passed, os.path.exists(ev_save_file),
                       f"eviction-page save file appeared at {ev_save_file}")

        # structure.clearAll wipes every loaded overlay AND strips the
        # structure edits from the log, so it must leave nothing derivable
        # on EITHER side of residency. Runs after the save above, whose
        # file is already on disk with the network intact.
        send(port, "return structure.clearAll()")
        time.sleep(1.0)
        passed = check(passed,
            net_members(port, ev_panel_nid) is None
            and net_members(port, ev_batt_nid) is None
            and listed_members(port) == [],
            "structure.clearAll removes the page's wire topology entirely "
            "(wire chunk still evicted)",
            {"panel": net_members(port, ev_panel_nid),
             "listed": listed_members(port)})
        passed = check(passed, reload_chunk(port, EV_CHUNK, EV_HOME_TILE),
                       "wire chunk reloaded after clearAll")
        passed = check(passed,
            net_members(port, ev_panel_nid) is None
            and listed_members(port) == [],
            "still no topology after reloading the cleared chunk — "
            "connectivity is not derivable from stripped history",
            {"panel": net_members(port, ev_panel_nid),
             "listed": listed_members(port)})

        quit_engine(port, procA)
        procA = None

        procB = boot(port, log=logB, label="engine B", args=["--resource-root", root])
        bootstrap_defs(port)
        pre = send(port, "return world.getActiveWorldId()")
        loaded = send(port, f"return engine.loadSave('{save_name}')")
        passed = check(passed, loaded.strip() == "true",
                       "engine.loadSave returned true (pre-load active: "
                       f"{pre})", loaded)
        # Issue #763: the saved page ("power_probe", its own id verbatim --
        # no more main_world remap) doesn't exist live until published.
        published, load_status = wait_load_published(port, 180)
        passed = check(passed, published,
                       "load transaction published", load_status)
        send(port, "return world.waitForInit(180)", timeout=190)
        time.sleep(2)
        send(port, "world.show('power_probe'); return 'ok'")
        wait_active(port, "power_probe")

        for bid, want_def, want_role, want_peak, want_cap in [
            (panel_bid, "solar_panel", "source", 400, 0),
            (batt_bid, "high_voltage_battery", "storage", 0, 5000),
        ]:
            info = jget(port, f"return building.getInfo({bid})")
            passed = check(passed,
                isinstance(info, dict) and info.get("defName") == want_def,
                f"building #{bid} ({want_def}) survived the reload", info)
            node = jget(port, f"return power.getNodeForBuilding({bid})")
            passed = check(passed,
                isinstance(node, dict) and node.get("role") == want_role
                and node.get("peakWatts") == want_peak
                and node.get("capacityWh") == want_cap,
                f"building #{bid}'s power node survived with role/params intact",
                node)
            if bid == batt_bid:
                stored_reloaded = node.get("storedWh") if isinstance(node, dict) else None
                passed = check(passed,
                    isinstance(stored_reloaded, (int, float))
                    and abs(stored_reloaded - stored_after) < 1e-3,
                    "battery's charged storedWh survived the reload "
                    f"({stored_after} -> {stored_reloaded})")

        # #1206 requirement 2, in a process that never saw the demolition:
        # the retired node must not come back, its building must not come
        # back, and no id may have been renumbered or reused to make the
        # count come out right.
        passed = check(passed,
            jget(port, f"return building.getInfo({second_panel_bid})") is None,
            "the demolished panel's BUILDING is absent after a fresh-process load")
        passed = check(passed,
            jget(port, f"return power.getNodeForBuilding({second_panel_bid})")
                is None,
            "the demolished panel's NODE is absent after a fresh-process load")
        reloaded_ids = jget(port,
            "local out={}; for _,n in ipairs(power.listNodes()) do "
            "out[#out+1]=n.id end; return out")
        passed = check(passed,
            isinstance(reloaded_ids, list) and demolished_nid not in reloaded_ids,
            "the retired node id was neither restored nor reused after the load",
            reloaded_ids)

        node_count_after = as_int(send(port, "local ns=power.listNodes(); return #ns"))
        passed = check(passed, node_count_after == surviving_nodes,
                       f"listNodes reports {surviving_nodes} nodes after reload",
                       node_count_after)

        # --- 11 (cont.). #1207 requirement 5, in a process that has never
        # held this page: load the eviction save and query topology BEFORE
        # any camera movement or explicit chunk load. World.Load.Stage
        # fills chunks around the SAVED camera, which was parked far east,
        # so the wire chunk must come up unloaded.
        ev_loaded = send(port, f"return engine.loadSave('{ev_save_name}')")
        passed = check(passed, ev_loaded.strip() == "true",
                       "engine.loadSave returned true for the eviction save",
                       ev_loaded)
        ev_published, ev_status = wait_load_published(port, 300)
        passed = check(passed, ev_published,
                       "eviction-save load transaction published", ev_status)
        # Activating the page is not a camera move and loads no chunk; the
        # power queries all read the ACTIVE page.
        send(port, f"world.show('{EV_PAGE}'); return 'ok'")
        if not wait_active(port, EV_PAGE):
            sys.exit(f"FAIL: {EV_PAGE} never became active after the load")
        ev_still_evicted = chunk_loaded(port, EV_CHUNK)
        passed = check(passed, ev_still_evicted is False,
                       "the wire chunk is NOT loaded in the fresh process",
                       ev_still_evicted)
        # Re-resolve the node ids through their BUILDINGS rather than
        # reusing engine A's, so this asserts topology and not id stability
        # (which the reload checks above already own).
        fresh_panel_node = jget(port,
            f"return power.getNodeForBuilding({ev_panel_bid})")
        fresh_batt_node = jget(port,
            f"return power.getNodeForBuilding({ev_batt_bid})")
        fresh_panel_nid = (as_int(fresh_panel_node.get("id"))
                           if isinstance(fresh_panel_node, dict) else None)
        fresh_batt_nid = (as_int(fresh_batt_node.get("id"))
                          if isinstance(fresh_batt_node, dict) else None)
        fresh_joined = (sorted([fresh_panel_nid, fresh_batt_nid])
                        if fresh_panel_nid is not None
                        and fresh_batt_nid is not None else None)
        fresh_members = (net_members(port, fresh_panel_nid)
                         if fresh_panel_nid is not None else None)
        passed = check(passed,
            fresh_joined is not None
            and fresh_members == fresh_joined
            and net_members(port, fresh_batt_nid) == fresh_joined
            and listed_members(port) == [fresh_joined]
            and len(ev_saved_members or []) == len(fresh_joined),
            "fresh-process load reports the SAME network without the camera "
            "ever visiting the wire's chunk",
            {"saved": ev_saved_members, "fresh": fresh_members,
             "listed": listed_members(port)})
        fresh_stored = stored_wh(port, ev_batt_bid)
        passed = check(passed,
            ev_saved_stored is not None and fresh_stored is not None
            and abs(fresh_stored - ev_saved_stored) < 1e-3,
            "the battery's charge survived the round trip "
            f"({ev_saved_stored} -> {fresh_stored})")

        print("\n" + ("ALL POWER CHECKS PASSED" if passed else "SOME FAILED"))
        return 0 if passed else 1
    finally:
        if procA is not None:
            quit_engine(port, procA)
        if procB is not None:
            quit_engine(port, procB)
        for path in (save_dir, ev_save_dir):
            if os.path.exists(path):
                shutil.rmtree(path, ignore_errors=True)


if __name__ == "__main__":
    sys.exit(main())
