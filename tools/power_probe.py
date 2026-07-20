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
  8. Save -> quit -> fresh restart -> reload defs -> load: every placed
     building, its power node, AND the battery's charged storedWh survive,
     reconnected by BuildingId.
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
import time
import uuid
from probelib import quit_engine, boot, send, wait_load_published

SAVE_PREFIX = "power_probe_"  # save dirs this probe owns (cleanup scoped to it)


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


def main() -> int:
    ap = argparse.ArgumentParser(description=__doc__,
                                  formatter_class=argparse.RawDescriptionHelpFormatter)
    ap.add_argument("--port", type=int, default=9358)
    args = ap.parse_args()
    port = args.port
    passed = True

    save_name = f"{SAVE_PREFIX}{uuid.uuid4().hex[:12]}"
    save_dir = os.path.join("saves", save_name)
    if os.path.exists(save_dir):
        sys.exit(f"refusing to run: {save_dir} already exists")

    logA = "/tmp/power_probe_A.log"
    logB = "/tmp/power_probe_B.log"
    procA = procB = None
    try:
        # ── Engine A: place nodes through the build tool, save ──────────
        procA = boot(port, log=logA, label="engine A")
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

        quit_engine(port, procA)
        procA = None

        procB = boot(port, log=logB, label="engine B")
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
            (second_panel_bid, "solar_panel", "source", 400, 0),
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

        node_count_after = as_int(send(port, "local ns=power.listNodes(); return #ns"))
        passed = check(passed, node_count_after == expected_nodes,
                       f"listNodes still reports {expected_nodes} nodes after reload",
                       node_count_after)

        print("\n" + ("ALL POWER CHECKS PASSED" if passed else "SOME FAILED"))
        return 0 if passed else 1
    finally:
        if procA is not None:
            quit_engine(port, procA)
        if procB is not None:
            quit_engine(port, procB)
        if os.path.exists(save_dir):
            shutil.rmtree(save_dir, ignore_errors=True)


if __name__ == "__main__":
    sys.exit(main())
