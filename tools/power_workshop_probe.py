#!/usr/bin/env python3
"""Job-dependent craft-recipe power draw probe (#590) — supersedes the
original #361 building-level consumer model for CRAFTING purposes.

#361 gave power its first consumer: a workshop building def could set a
flat `power_drain` (watts) and draw it constantly whenever Built,
regardless of whether anyone was actually working it. #590 replaces that
for craft/repair stations with a JOB-dependent load: the recipe itself
carries an optional `power_draw` (watts, default 0 — most recipes are
unaffected). A recipe with power_draw > 0 only demands power while its
craft bill is CLAIMED AND ACTIVELY BEING WORKED (Craft.Bills.cbWorking —
Power.Network.activeCraftConsumersOn folds every such bill's draw into
the network's live drain); merely claiming a bill (still fetching
materials or walking to the station) draws nothing yet, an idle Built
station with no bill draws nothing at all even at full generation, and —
the flip side — a bill PAUSED mid-cycle keeps drawing for as long as its
existing holder keeps working it (Craft.Bills.claimAvailable lets that
holder finish the cycle; only fresh claims are blocked). `power.
isStationPoweredForRecipe(bid, recipeId)` is the job-aware gating query
(a zero-power recipe is always true, any station, wired or not);
`Engine.Scripting.Lua.API.Craft.validateStation` refuses craft.executeAt/
repair.repairAt on a station that can't satisfy a positive-power
recipe's demand, and the craft_job AI's "working" phase pours no
progress while browned out (idle, not failed) — resuming automatically
once the network can cover it. `power.isBuildingPowered(bid)` (the old
#361 query) still exists for a hypothetical future ALWAYS-ON non-crafting
device via bdPowerDrain — no shipped or crafting building sets that
field any more, so it's trivially true for every craft station.

This probe registers its OWN throwaway building + recipe defs (mirroring
how tools/craft_bill_probe.py injects a temp recipe YAML) rather than
flipping a shipped building/recipe — so it exercises the #590 mechanism
in complete isolation from every other probe's fixtures. The workshop
def carries NO power_drain (per #590, a craft station's load lives on
the recipe); the probe recipe carries `power_draw: 150`.

What it does (all against a single flat arena):
  1. Boots headless, loads real defs + a synthetic "forge" workshop (no
     power_drain) + a tiny power_draw=150W probe recipe for it, builds
     the workshop through the normal materials + build-progress
     machinery. power.isBuildingPowered is trivially true throughout
     (the building itself never draws) — only the recipe gate matters.
  2. Unwired: power.isStationPoweredForRecipe is false; craft.executeAt
     refuses with a "no power" reason.
  3. Wires a solar panel + battery to the workshop (2 wire tiles, the
     panel bridges them exactly like the #360 connectivity tests) at
     midnight, with NO bill claimed yet: still unpowered (0 generation,
     0 stored), and power.listNetworks() reports drainW == 0 — wired
     alone isn't a draw.
  4. Flips to noon (panel's peak output alone would cover the recipe's
     draw): power.isStationPoweredForRecipe flips true and
     craft.executeAt succeeds — but power.listNetworks() STILL reports
     drainW == 0, because no bill is claimed: full generation, idle
     station, zero demand (the #590 "not merely because it exists"
     requirement).
  5. Manually drives a bill through claim -> working -> pause -> release
     (bypassing the AI) to isolate each transition: drainW stays 0 on
     claim alone, jumps to 150W once marked working, STAYS 150W while
     paused (an existing holder keeps working through a pause), and
     drops to 0 once un-marked working or released — proving drain
     tracks cbWorking specifically, not the claim or pause flags.
  6. AI end-to-end at midnight: craft_job claims, fetches, and walks up
     to the built station (drainW stays 0 through fetch/walking), then
     marks itself working (drainW reads 150W) but pours NO bill progress
     while browned out (idle, not failed, not released); flipping to
     noon lets it complete, after which drainW returns to 0 (the bill is
     done and gone).
  7. A short real fast-forward with a bill held claimed AND marked
     working (deterministic, AI off) shows the battery's storedWh
     actually RISE at noon (generation 400W > drain 150W) and FALL at
     midnight (drain with no generation) — the day/night balance the
     issue's "Done when" calls for, driven by a real ACTIVE job's draw,
     not a synthetic map entry.

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
from probelib import clear_find_water, quit_engine, boot, send

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
    power_draw: {PROBE_DRAIN_W}
"""

# A throwaway workshop def: no materials to deliver (build_work alone
# gates it Built), one "forge" operation. Deliberately NO power_drain
# (#590) — a craft station's electrical load lives on the recipe now,
# not the building; see the probe's docstring.
TEST_BUILDINGS = f"""\
buildings:
  - name: "{PROBE_BUILDING}"
    display_name: "Probe Bench"
    category: "Production"
    description: "Throwaway #590 test fixture — not shipped content."
    sprite: "assets/textures/buildings/workbench/default.png"
    tile_size: {{ x: 1, y: 1 }}
    placement: "flat_ground"
    race: "acolyte_cult"
    build_work: 60.0
    operations:
      - forge
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
    clear_find_water(port, uid)
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


def first_network(port: int):
    """This probe only ever wires up one network — its (only) entry
    from power.listNetworks(), or None before anything's wired. Keying
    off array position rather than a building id matters here: a node
    (panel/battery) never appears in consumerIds, and an idle station
    with no claimed bill doesn't either (#590) — so a lookup keyed on
    "which network mentions bid X" would miss exactly the idle case
    this probe is built to exercise."""
    nets = jget(port, "return power.listNetworks()")
    if isinstance(nets, list) and nets and isinstance(nets[0], dict):
        return nets[0]
    return None


def drain_of(port: int) -> float | None:
    net = first_network(port)
    return net.get("drainW") if isinstance(net, dict) else None


def powered_for_recipe(port: int, bid: int, recipe: str) -> str:
    return send(port,
        f"return power.isStationPoweredForRecipe({bid}, '{recipe}')"
        ).strip('"')


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
        recipe = jget(port, f"return craft.get('{PROBE_RECIPE}')")
        passed = check(passed,
            isinstance(recipe, dict) and recipe.get("powerDraw") == PROBE_DRAIN_W,
            "craft.get exposes powerDraw alongside the other recipe fields",
            recipe)
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

        # --- 0. The building itself carries no power_drain (#590): the
        # OLD #361 query is trivially true regardless of wiring — only
        # the recipe-aware gate below is meaningful for this station.
        always = send(port, f"return power.isBuildingPowered({bid_w})").strip('"')
        passed = check(passed, always == "true",
                       "isBuildingPowered is trivially true (no power_drain set)",
                       always)

        # --- 1. Never wired: unpowered, executeAt refuses ---
        powered = powered_for_recipe(port, bid_w, PROBE_RECIPE)
        passed = check(passed, powered == "false",
                       "isStationPoweredForRecipe false (never wired)", powered)

        send(port, f"unit.addItem({uid}, 'steel_bar'); return 'ok'")
        r = jget(port,
            f"local ok,err = craft.executeAt({uid}, '{PROBE_RECIPE}', {bid_w}); "
            f"return {{ok = ok, err = err}}")
        passed = check(passed,
            isinstance(r, dict) and r.get("ok") is False
            and "no power" in str(r.get("err", "")),
            "craft.executeAt refuses with a no-power reason (unwired)", r)

        # --- 2. Wire a panel + battery to the workshop, still midnight,
        # no bill claimed yet: wiring alone must not draw ---
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
        # mechanic as the #360 connectivity tests, just with a station
        # riding along instead of only nodes.
        send(port, "require('scripts.wire').place(7, 2); return 'ok'")
        send(port, "require('scripts.wire').place(9, 2); return 'ok'")

        # wire.place queues a world command the world thread applies on
        # its own next iteration — poll rather than querying the very
        # instant after both sends (the same class of async landing as
        # world.setTime below). The workshop itself only appears in
        # consumerIds once isStationPoweredForRecipe (or an active bill)
        # asks about it, so poll on the two NODES landing on one network.
        poll(port, 5,
             lambda: len((first_network(port) or {}).get("nodeIds", [])) == 2,
             interval=0.2)
        net = first_network(port)
        passed = check(passed,
            isinstance(net, dict)
            and sorted(net.get("nodeIds", [])) == sorted(
                [panel_node["id"], batt_node["id"]])
            and net.get("generationW") == 0,
            "wired at midnight: one network, generationW=0", net)
        passed = check(passed,
            drain_of(port) == 0,
            "drainW == 0 while wired but no bill is claimed (idle station)",
            drain_of(port))

        powered = powered_for_recipe(port, bid_w, PROBE_RECIPE)
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

        # --- 3. Flip to noon: panel's peak output alone covers the
        # recipe's draw, but with no bill claimed the network must
        # still show zero drain — full generation, idle station ---
        send(port, f"world.setTime('{PAGE}', 12, 0); return 'ok'")
        noon_ready = poll(port, 10,
            lambda: (as_float(send(port,
                f"local n = power.listNetworks()[1]; return n and n.generationW or 0"))
                or 0) > 300,
            interval=0.5)
        passed = check(passed, noon_ready, "time flip to noon landed (generationW > 300)")

        powered = powered_for_recipe(port, bid_w, PROBE_RECIPE)
        passed = check(passed, powered == "true",
                       "powered at noon (400W generation > 150W draw)", powered)
        passed = check(passed,
            drain_of(port) == 0,
            "drainW still 0 at noon with no bill claimed (#590: not merely "
            "because it exists or is powered)", drain_of(port))

        before_bars = ground_count_near(port, "steel_hardware", 6, 2, 3)
        r = jget(port,
            f"local ok,err = craft.executeAt({uid}, '{PROBE_RECIPE}', {bid_w}); "
            f"return {{ok = ok, err = err}}")
        passed = check(passed, isinstance(r, dict) and r.get("ok") is True,
                       "craft.executeAt succeeds once powered", r)

        # --- 4. Manually drive a bill through claim -> working -> pause
        # -> release (no AI): drain must track cbWorking exactly, NOT
        # the claim or pause flags on their own (#590 fix) ---
        bill_id, msg = add_bill(port, bid_w, PROBE_RECIPE, 1)
        passed = check(passed, bill_id is not None, "manual bill queued", msg)
        claimed = send(port,
            f"return craft.claimBill({bill_id}, {uid}, 60)").strip('"')
        passed = check(passed, claimed == "true", "manual claim succeeds", claimed)
        passed = check(passed,
            drain_of(port) == 0,
            "drainW stays 0 on claim alone (not yet marked working — "
            "mirrors the AI's fetch/walking phases)", drain_of(port))

        send(port, f"craft.setBillWorking({bill_id}, true); return 'ok'")
        passed = check(passed,
            drain_of(port) == PROBE_DRAIN_W,
            "drainW == 150W once the bill is marked working", drain_of(port))

        # Pausing a bill an existing holder is mid-cycle on must NOT cut
        # its power: Craft.Bills.claimAvailable lets that holder keep
        # working to the end of the cycle, so activeCraftConsumersOn
        # must keep drawing for it too (only cbWorking gates the drain,
        # never cbPaused).
        send(port, f"craft.setBillPaused({bill_id}, true); return 'ok'")
        passed = check(passed,
            drain_of(port) == PROBE_DRAIN_W,
            "drainW stays 150W while paused — existing holder keeps "
            "working through the pause", drain_of(port))
        send(port, f"craft.setBillPaused({bill_id}, false); return 'ok'")

        send(port, f"craft.setBillWorking({bill_id}, false); return 'ok'")
        passed = check(passed,
            drain_of(port) == 0,
            "drainW back to 0 once un-marked working", drain_of(port))

        # releaseBill clears cbWorking on its own too, independent of an
        # explicit setBillWorking(false) — re-mark working then release
        # to prove the engine-side reset, not just the Lua-side one.
        send(port, f"craft.setBillWorking({bill_id}, true); return 'ok'")
        send(port, f"craft.releaseBill({bill_id}); return 'ok'")
        passed = check(passed,
            drain_of(port) == 0,
            "drainW back to 0 the instant a working bill is released "
            "(releaseBill itself clears cbWorking)", drain_of(port))
        send(port, f"craft.cancelBill({bill_id}); return 'ok'")

        # --- 5. AI end-to-end: fetch/walking draws nothing, working
        # shows drain, stall at midnight, resume at noon, drain returns
        # to 0 once the bill is gone ---
        send(port, f"world.setTime('{PAGE}', 0, 0); return 'ok'")
        send(port, f"unit.addItem({uid}, 'steel_bar'); return 'ok'")
        bill_id, msg = add_bill(port, bid_w, PROBE_RECIPE, 1)
        passed = check(passed, bill_id is not None, "AI bill queued", msg)
        ai_on(port)
        # Re-retire find_water: the first live AI tick reseeds spawn
        # goals, overwriting the earlier retirement.
        time.sleep(1.0)
        clear_find_water(port, uid)

        claimed = poll(port, 20, lambda: jget(
            port, f"local b = craft.getBill({bill_id}); "
                  f"return b and b.claimant or -1") == uid)
        passed = check(passed, claimed, "AI claims the bill")

        # The station is a couple of tiles away, so fetch/walking is
        # brief but real — poll for the AI to actually reach "working"
        # (marked via craft.setBillWorking) rather than asserting drain
        # the instant it claims, which would still be mid-walk.
        working = poll(port, 20, lambda: jget(
            port, f"local b = craft.getBill({bill_id}); "
                  f"return b and b.working or false") is True)
        passed = check(passed, working, "AI reaches the working phase")
        passed = check(passed,
            drain_of(port) == PROBE_DRAIN_W,
            "drainW == 150W once the AI is actively working the bill",
            drain_of(port))

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
        passed = check(passed,
            drain_of(port) == 0,
            "drainW back to 0 once the bill is done and gone", drain_of(port))

        # --- 6. Day/night balance under a real ACTIVE job's drain ---
        # (deterministic: AI off, bill held claimed AND marked working
        # by hand for the whole fast-forward so the drain is continuous,
        # not dependent on the AI actually finishing crafts inside the
        # window).
        ai_off(port)
        bill_id, msg = add_bill(port, bid_w, PROBE_RECIPE)  # repeat forever
        passed = check(passed, bill_id is not None, "day/night bill queued", msg)
        send(port, f"craft.claimBill({bill_id}, {uid}, 3600); "
                   f"craft.setBillWorking({bill_id}, true); return 'ok'")

        stored0 = as_float(jget(port, f"return power.getNodeForBuilding({batt_bid}).storedWh"))
        send(port, f"world.setTime('{PAGE}', 12, 0); return 'ok'")
        send(port, f"world.setTimeScale('{PAGE}', 60); return 'ok'")
        time.sleep(5)
        send(port, f"world.setTimeScale('{PAGE}', 0); return 'ok'")
        stored1 = as_float(jget(port, f"return power.getNodeForBuilding({batt_bid}).storedWh"))
        passed = check(passed,
            stored0 is not None and stored1 is not None and stored1 > stored0,
            "battery storedWh rises over simulated daylight (generation > active drain)",
            f"{stored0} -> {stored1}")

        send(port, f"world.setTime('{PAGE}', 0, 0); return 'ok'")
        send(port, f"world.setTimeScale('{PAGE}', 60); return 'ok'")
        time.sleep(5)
        send(port, f"world.setTimeScale('{PAGE}', 0); return 'ok'")
        stored2 = as_float(jget(port, f"return power.getNodeForBuilding({batt_bid}).storedWh"))
        passed = check(passed,
            stored1 is not None and stored2 is not None and stored2 < stored1,
            "battery storedWh falls over simulated night (active drain, no generation)",
            f"{stored1} -> {stored2}")
        send(port, f"craft.releaseBill({bill_id}); craft.cancelBill({bill_id}); return 'ok'")

        print("\n" + ("ALL POWER WORKSHOP CHECKS PASSED" if passed else "SOME FAILED"))
        return 0 if passed else 1
    finally:
        quit_engine(port, proc)


if __name__ == "__main__":
    sys.exit(main())
