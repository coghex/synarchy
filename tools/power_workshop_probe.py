#!/usr/bin/env python3
"""Powered-workshop consumer probe (#361) — the generic power-drain
consumer on top of #358 (nodes) / #359 (wire) / #360 (network balance).

#361 gives power a purpose: a workshop building def can set a
`power_drain` (watts) and becomes a CONSUMER — not a Power.Types node
(Power.Network derives its tile + drain fresh from BuildingManager every
call, see Power.Network.consumersOn) — whose drain folds into whatever
network its tile is wired to. A building is a consumer iff its
power_drain is > 0; no shipped building sets one today (workbench/furnace
stay power-free — assigning power_drain to real content, incl. any new
art it wants, is a follow-up, not this probe's job). `power.
isBuildingPowered(bid)` answers the gating question; `Engine.Scripting.
Lua.API.Craft.validateStation` refuses craft.executeAt on an unpowered
station, and the craft_job AI's "working" phase pours no progress while
browned out (idle, not failed) — resuming automatically once the network
has generation/charge again.

This probe registers its OWN throwaway building + recipe defs (mirroring
how tools/craft_bill_probe.py injects a temp recipe YAML) rather than
flipping a shipped building's power_drain — so it exercises the #361
mechanism in complete isolation from every other probe's fixtures.

What it does (all against a single flat arena):
  1. Boots headless, loads real defs + a synthetic power_drain=150W
     "forge" workshop + a tiny probe recipe for it, builds the workshop
     through the normal materials + build-progress machinery.
  2. Unwired: power.isBuildingPowered is false; craft.executeAt refuses
     with a "no power" reason.
  3. Wires a solar panel + battery to the workshop (2 wire tiles, the
     panel bridges them exactly like the #360 connectivity tests) at
     midnight: still unpowered (0 generation, 0 stored) — wiring alone
     isn't enough. power.listNetworks() reports drainW == the workshop's
     power_drain and consumerIds includes it.
  4. Flips to noon (panel's peak output alone covers the drain): now
     powered, and craft.executeAt succeeds.
  5. AI end-to-end at midnight: craft_job claims + fetches + walks up to
     the built station but pours NO bill progress while browned out (idle,
     not failed, not released); flipping to noon lets it complete.
  6. A short real fast-forward at noon shows the battery's storedWh
     actually RISE (generation > drain); the same fast-forward at midnight
     shows it FALL (drain with no generation) — the day/night balance the
     issue's "Done when" calls for, with a real consumer's drain now
     folded in throughout (not the synthetic map #360's own tests use).

Usage: python3 tools/power_workshop_probe.py [--port 9361]
Exit 0 = every check passed.
"""
from __future__ import annotations

import argparse
import glob
import json
import os
import socket
import subprocess
import sys
import time
from probelib import boot, send

SPROOT = "/tmp"
TEST_RECIPE_YAML = f"{SPROOT}/power_workshop_probe_recipes.yaml"
TEST_BUILDING_YAML = f"{SPROOT}/power_workshop_probe_buildings.yaml"
PROBE_RECIPE = "power_workshop_probe_forge"
PROBE_BUILDING = "power_workshop_probe_bench"
PROBE_DRAIN_W = 150.0

TEST_RECIPES = f"""\
recipes:
  - id: {PROBE_RECIPE}
    station: forge
    inputs:
      - item: steel_bar
    work: 1
    outputs:
      - item: steel_hardware
        count: 1
"""

# A throwaway workshop def: no materials to deliver (build_work alone
# gates it Built), one "forge" operation, and the power_drain (#361)
# under test — reusing an existing texture path since art isn't the
# point here (see the probe's docstring: assigning power_drain to real
# content is a follow-up, not this fixture).
TEST_BUILDINGS = f"""\
buildings:
  - name: "{PROBE_BUILDING}"
    display_name: "Probe Bench"
    category: "Production"
    description: "Throwaway #361 test fixture — not shipped content."
    sprite: "assets/textures/buildings/workbench/default.png"
    tile_size: {{ x: 1, y: 1 }}
    placement: "flat_ground"
    race: "acolyte_cult"
    build_work: 60.0
    operations:
      - forge
    power_drain: {PROBE_DRAIN_W}
"""

PAGE = "power_workshop_probe"


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


def as_float(s) -> float | None:
    try:
        return float(s)
    except (TypeError, ValueError):
        return None


def shutdown(proc: subprocess.Popen, port: int) -> None:
    try:
        send(port, "engine.quit()", timeout=2)
    except OSError:
        pass
    for _ in range(50):
        if proc.poll() is not None:
            break
        time.sleep(0.1)
    if proc.poll() is None:
        proc.kill()
        proc.wait(timeout=5)


def bootstrap_defs(port: int) -> None:
    for pattern, fn in [
        ("data/substances/*.yaml", "engine.loadSubstanceYaml"),
        ("data/items/*.yaml",      "engine.loadItemYaml"),
        ("data/equipment/*.yaml",  "engine.loadEquipmentYaml"),
        ("data/materials/*.yaml",  "engine.loadMaterialYaml"),
        ("data/units/*.yaml",      "engine.loadUnitYaml"),
        ("data/recipes/*.yaml",    "engine.loadRecipeYaml"),
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


# The stash lives on the module table (NOT _G): debug-console lines
# don't share a global env, but the loaded module table persists.
def ai_off(port: int) -> None:
    send(port,
         "local ai = require('scripts.unit_ai'); "
         "if not ai.__probe_orig_update then "
         "ai.__probe_orig_update = ai.update end; "
         "ai.update = function() end; return 'ai-off'")


def ai_on(port: int) -> None:
    send(port,
         "local ai = require('scripts.unit_ai'); "
         "if ai.__probe_orig_update then "
         "ai.update = ai.__probe_orig_update end; "
         "return 'ai-on'")


def spawn_acolyte(port: int, x: float, y: float) -> int:
    uid = as_int(send(port, f"return unit.spawn('acolyte', {x}, {y})"))
    if uid is None:
        sys.exit("unit.spawn failed")
    time.sleep(0.5)
    # Retire the spawn-seeded find_water goal: the arena has no water,
    # and a scouting acolyte walks off-course instead of crafting.
    send(port,
         f"local ai = require('scripts.unit_ai'); "
         f"local s = ai.getState({uid}); "
         f"ai.markGoalAccomplished(s, 'find_water'); return 'ok'")
    return uid


def spawn_station(port: int, uid: int, def_name: str, gx: int, gy: int,
                   materials: dict[str, int], progress: int = 500) -> int:
    """building.spawn + deliver build materials through the real
    machinery, then addBuildProgress to Built."""
    raw = send(port, f"return building.spawn('{def_name}', {gx}, {gy})")
    bid = as_int(raw)
    if bid is None:
        sys.exit(f"building.spawn('{def_name}') failed: {raw}")
    for _ in range(50):
        if send(port, f"return building.getInfo({bid}) and 'yes' or 'no'"
                ).strip('"') == "yes":
            break
        time.sleep(0.1)
    else:
        sys.exit(f"{def_name} instance never appeared")
    for item, count in materials.items():
        send(port,
             f"for i=1,{count} do unit.addItem({uid},'{item}'); "
             f"unit.transferItemToBuilding({uid},{bid},'{item}') end; "
             f"return 'ok'")
    if send(port, f"return building.areMaterialsSatisfied({bid}) "
                  f"and 'yes' or 'no'").strip('"') != "yes":
        sys.exit(f"{def_name} materials not satisfied after delivery")
    send(port, f"building.addBuildProgress({bid}, {progress}); return 'ok'")
    act = send(port, f"return building.getActivity({bid})").strip('"')
    if act != "built":
        sys.exit(f"{def_name} never reached built (activity={act})")
    return bid


def add_bill(port: int, bid: int, recipe: str, count: int | None = None):
    """→ (billId or None, err)."""
    arg = f", {count}" if count is not None else ""
    raw = send(port,
               f"local id,err = craft.addBill({bid}, '{recipe}'{arg}); "
               f"return id and ('ID:'..id) or ('ERR:'..tostring(err))"
               ).strip('"')
    if raw.startswith("ID:"):
        return int(float(raw[3:])), ""
    return None, raw


def ground_count_near(port: int, name: str, gx: int, gy: int, radius: int) -> int:
    return as_int(send(port,
        f"local n=0; for _,g in ipairs(item.listGround() or {{}}) do "
        f"if g.defName=='{name}' and math.abs(g.x-{gx})<={radius} "
        f"and math.abs(g.y-{gy})<={radius} then n=n+1 end end; return n")) or 0


def check(passed: bool, ok: bool, label: str, detail="") -> bool:
    print(f"  [{'PASS' if ok else 'FAIL'}] {label}" + (f": {detail}" if detail else ""))
    return passed and ok


def poll(port: int, seconds: float, fn, interval: float = 1.0) -> bool:
    """Poll fn until true, defensively unpausing each pass (mirrors
    craft_bill_probe.py's poll — a stuck-walk notification could
    otherwise auto-pause the sim under us)."""
    deadline = time.time() + seconds
    while time.time() < deadline:
        send(port, "engine.setPaused(false); return 'ok'")
        if fn():
            return True
        time.sleep(interval)
    return False


def network_for(port: int, bid: int):
    """The single network containing consumer building bid, or None."""
    nets = jget(port, "return power.listNetworks()")
    if not isinstance(nets, list):
        return None
    for net in nets:
        if isinstance(net, dict) and bid in (net.get("consumerIds") or []):
            return net
    return None


def main() -> int:
    ap = argparse.ArgumentParser(description=__doc__,
                                  formatter_class=argparse.RawDescriptionHelpFormatter)
    ap.add_argument("--port", type=int, default=9361)
    args = ap.parse_args()
    port = args.port
    passed = True

    log = f"{SPROOT}/power_workshop_probe_engine.log"
    proc = boot(port, log)
    try:
        bootstrap_defs(port)
        with open(TEST_RECIPE_YAML, "w") as f:
            f.write(TEST_RECIPES)
        n = as_int(send(port, f"return engine.loadRecipeYaml('{TEST_RECIPE_YAML}')"))
        passed = check(passed, n == 1, "probe recipe loaded", f"count={n}")
        with open(TEST_BUILDING_YAML, "w") as f:
            f.write(TEST_BUILDINGS)
        n = as_int(send(port, f"return engine.loadBuildingYaml('{TEST_BUILDING_YAML}')"))
        passed = check(passed, n == 1, "probe building loaded", f"count={n}")

        send(port, f"world.initArena('{PAGE}'); return 'ok'")
        send(port, f"world.show('{PAGE}'); return 'ok'")
        if not wait_active(port, PAGE):
            sys.exit(f"FAIL: {PAGE} arena never became active")

        # Freeze the clock so every "instant" assertion below is
        # deterministic — only the explicit fast-forward phase at the
        # end unpauses it.
        send(port, f"world.setTimeScale('{PAGE}', 0); return 'ok'")
        send(port, f"world.setTime('{PAGE}', 0, 0); return 'ok'")

        ai_off(port)
        uid = spawn_acolyte(port, 6, 3)
        bid_w = spawn_station(port, uid, PROBE_BUILDING, 6, 2, {})

        # --- 1. Never wired: unpowered, executeAt refuses ---
        powered = send(port, f"return power.isBuildingPowered({bid_w})").strip('"')
        passed = check(passed, powered == "false",
                       "power_drain workshop starts unpowered (never wired)",
                       powered)

        send(port, f"unit.addItem({uid}, 'steel_bar'); return 'ok'")
        r = jget(port,
            f"local ok,err = craft.executeAt({uid}, '{PROBE_RECIPE}', {bid_w}); "
            f"return {{ok = ok, err = err}}")
        passed = check(passed,
            isinstance(r, dict) and r.get("ok") is False
            and "no power" in str(r.get("err", "")),
            "craft.executeAt refuses with a no-power reason (unwired)", r)

        # --- 2. Wire a panel + battery to the workshop, still midnight ---
        send(port, f"unit.addItem({uid}, 'solar_panel'); "
                   f"unit.addItem({uid}, 'high_voltage_battery'); return 'ok'")
        panel_bid = as_int(send(port,
            f"local nid, bid = power.placeNode({uid}, 'solar_panel', 8, 2); return bid"))
        batt_bid = as_int(send(port,
            f"local nid, bid = power.placeNode({uid}, 'high_voltage_battery', 10, 2); return bid"))
        passed = check(passed, panel_bid is not None and batt_bid is not None,
                       "solar panel + battery placed", (panel_bid, batt_bid))

        panel_node = jget(port, f"return power.getNodeForBuilding({panel_bid})")
        batt_node = jget(port, f"return power.getNodeForBuilding({batt_bid})")
        passed = check(passed,
            isinstance(panel_node, dict) and panel_node.get("role") == "source"
            and panel_node.get("peakWatts") == 400,
            "panel node role=source, 400W", panel_node)
        passed = check(passed,
            isinstance(batt_node, dict) and batt_node.get("role") == "storage"
            and batt_node.get("capacityWh") == 5000,
            "battery node role=storage, 5000Wh", batt_node)

        # Two wire stubs; the panel at (8,2) is orthogonally adjacent to
        # BOTH (7,2) and (9,2), bridging them (+ the workshop at (6,2)
        # and the battery at (10,2)) into one network — same bridging
        # mechanic as the #360 connectivity tests, just with a consumer
        # building riding along instead of only nodes.
        send(port, "require('scripts.wire').place(7, 2); return 'ok'")
        send(port, "require('scripts.wire').place(9, 2); return 'ok'")

        # wire.place queues a world command the world thread applies on
        # its own next iteration — poll rather than querying the very
        # instant after both sends (the same class of async landing as
        # world.setTime below).
        poll(port, 5,
             lambda: len((network_for(port, bid_w) or {}).get("nodeIds", [])) == 2,
             interval=0.2)
        net = network_for(port, bid_w)
        passed = check(passed,
            isinstance(net, dict)
            and sorted(net.get("nodeIds", [])) == sorted(
                [panel_node["id"], batt_node["id"]])
            and net.get("drainW") == PROBE_DRAIN_W and net.get("generationW") == 0,
            f"wired at midnight: one network, drainW={PROBE_DRAIN_W}W, generationW=0",
            net)

        powered = send(port, f"return power.isBuildingPowered({bid_w})").strip('"')
        passed = check(passed, powered == "false",
                       "still unpowered at midnight (wired but no charge/generation)",
                       powered)
        r = jget(port,
            f"local ok,err = craft.executeAt({uid}, '{PROBE_RECIPE}', {bid_w}); "
            f"return {{ok = ok, err = err}}")
        passed = check(passed,
            isinstance(r, dict) and r.get("ok") is False
            and "no power" in str(r.get("err", "")),
            "craft.executeAt still refuses (wired, unpowered)", r)

        # --- 3. Flip to noon: panel's peak output alone covers the drain ---
        send(port, f"world.setTime('{PAGE}', 12, 0); return 'ok'")
        noon_ready = poll(port, 10,
            lambda: (as_float(send(port,
                f"local n = power.listNetworks()[1]; return n and n.generationW or 0"))
                or 0) > 300,
            interval=0.5)
        passed = check(passed, noon_ready, "time flip to noon landed (generationW > 300)")

        powered = send(port, f"return power.isBuildingPowered({bid_w})").strip('"')
        passed = check(passed, powered == "true",
                       "powered at noon (400W generation > 150W drain)", powered)

        before_bars = ground_count_near(port, "steel_hardware", 6, 2, 3)
        r = jget(port,
            f"local ok,err = craft.executeAt({uid}, '{PROBE_RECIPE}', {bid_w}); "
            f"return {{ok = ok, err = err}}")
        passed = check(passed, isinstance(r, dict) and r.get("ok") is True,
                       "craft.executeAt succeeds once powered", r)

        # --- 4. AI end-to-end: stall at midnight, resume at noon ---
        send(port, f"world.setTime('{PAGE}', 0, 0); return 'ok'")
        send(port, f"unit.addItem({uid}, 'steel_bar'); return 'ok'")
        bill_id, msg = add_bill(port, bid_w, PROBE_RECIPE, 1)
        passed = check(passed, bill_id is not None, "AI bill queued", msg)
        ai_on(port)
        # Re-retire find_water: the first live AI tick reseeds spawn
        # goals, overwriting the earlier retirement.
        time.sleep(1.0)
        send(port,
             f"local ai = require('scripts.unit_ai'); "
             f"local s = ai.getState({uid}); "
             f"ai.markGoalAccomplished(s, 'find_water'); return 'ok'")

        claimed = poll(port, 20, lambda: jget(
            port, f"local b = craft.getBill({bill_id}); "
                  f"return b and b.claimant or -1") == uid)
        passed = check(passed, claimed, "AI claims the bill")

        # Give the AI a few seconds standing at the (browned-out) station
        # — progress must NOT move.
        time.sleep(5)
        stalled = jget(port, f"local b = craft.getBill({bill_id}); "
                              f"return b and b.progress or -1")
        passed = check(passed, stalled == 0,
                       "bill claimed but progress stays 0 while browned out",
                       stalled)

        send(port, f"world.setTime('{PAGE}', 12, 0); return 'ok'")
        done = poll(port, 60, lambda: send(
            port, f"return craft.getBill({bill_id}) and 'y' or 'n'"
            ).strip('"') == "n")
        passed = check(passed, done, "bill completes once the network is powered")
        after_bars = ground_count_near(port, "steel_hardware", 6, 2, 3)
        passed = check(passed, after_bars > before_bars,
                       "fresh output appeared at the station",
                       f"{before_bars} -> {after_bars}")

        # --- 5. Day/night balance: consumer drain actually moves storedWh ---
        ai_off(port)
        stored0 = as_float(jget(port, f"return power.getNodeForBuilding({batt_bid}).storedWh"))
        send(port, f"world.setTime('{PAGE}', 12, 0); return 'ok'")
        send(port, f"world.setTimeScale('{PAGE}', 60); return 'ok'")
        time.sleep(5)
        send(port, f"world.setTimeScale('{PAGE}', 0); return 'ok'")
        stored1 = as_float(jget(port, f"return power.getNodeForBuilding({batt_bid}).storedWh"))
        passed = check(passed,
            stored0 is not None and stored1 is not None and stored1 > stored0,
            "battery storedWh rises over simulated daylight (generation > drain)",
            f"{stored0} -> {stored1}")

        send(port, f"world.setTime('{PAGE}', 0, 0); return 'ok'")
        send(port, f"world.setTimeScale('{PAGE}', 60); return 'ok'")
        time.sleep(5)
        send(port, f"world.setTimeScale('{PAGE}', 0); return 'ok'")
        stored2 = as_float(jget(port, f"return power.getNodeForBuilding({batt_bid}).storedWh"))
        passed = check(passed,
            stored1 is not None and stored2 is not None and stored2 < stored1,
            "battery storedWh falls over simulated night (drain, no generation)",
            f"{stored1} -> {stored2}")

        print("\n" + ("ALL POWER WORKSHOP CHECKS PASSED" if passed else "SOME FAILED"))
        return 0 if passed else 1
    finally:
        shutdown(proc, port)


if __name__ == "__main__":
    sys.exit(main())
