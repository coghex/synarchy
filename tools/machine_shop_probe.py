#!/usr/bin/env python3
"""Electric furnace + machine shop shipped-content probe (#591).

#590 gave craft recipes a job-dependent `power_draw`; #358-#360 gave the
grid solar panels, batteries, wire, and network balance;
`tools/power_workshop_probe.py` proves that MECHANISM end-to-end against
a throwaway synthetic building + recipes, deliberately isolated from
every other probe's fixtures. This probe instead exercises the first
REAL shipped content built on top of it:

  - `furnace` (data/buildings/furnace.yaml) gains one powered smelt
    recipe, `smelt_steel_electric` (data/recipes/smelting.yaml) —
    same ore input and bar yield as the existing coal-fired
    `smelt_steel_*`/`smelt_bronze_*` recipes at the same `smelt`
    station, just no `fuel:` line and a `power_draw` instead. There is
    only ever ONE furnace building; this is additive, not a second
    "electric furnace" building.
  - `machine_shop` (data/buildings/machine_shop.yaml) is the genuinely
    new building, with a dedicated `"machine"` station operation (kept
    separate from workbench's existing, currently recipe-less
    `"assemble"` op so workbench's role stays untouched). Its two
    recipes (data/recipes/machining.yaml) — `machine_wiring`
    (bronze_bar -> wiring) and `machine_electric_motor` (steel_bar +
    wiring -> electric_motor) — both carry `power_draw`.

What it does (all against a single flat arena, two independent
station+panel+battery groups far enough apart that they never bridge
into one network):

  1. Boots headless, loads every real def (including the three files
     above), builds a real `furnace` and a real `machine_shop` through
     the normal materials + build-progress machinery, and confirms
     both are `Built` and (#591 req. 8) `machine_shop` shows up in
     `building.listDefs()` with the right category/build-work, its
     built instance reporting the `"machine"` operation.
  2. Confirms the three new/changed recipes load with `power_draw > 0`
     and (for the smelt variant) no `fuel` line.
  3. Regression check: with NEITHER station wired, the existing
     coal-fired `smelt_steel_lignite` recipe still succeeds at the
     unwired furnace — proving #591 is additive, not a change to
     furnace's existing behavior.
  4. Still unwired: `craft.executeAt` refuses "no power" for
     `smelt_steel_electric` at the furnace and for both `machine_wiring`
     and `machine_electric_motor` at the machine shop.
  5. Wires each station to its own solar panel + battery (2 wire tiles
     each, mirroring the #360 connectivity tests' bridging layout), at
     midnight: still unpowered, `drainW == 0` on both networks.
  6. Flips to noon (panel peak alone covers every recipe's draw):
     `craft.executeAt` now succeeds for `smelt_steel_electric`,
     `machine_wiring`, and (feeding it the wiring it needs directly,
     to isolate the recipe/power gate from ground-item pickup
     logistics already covered elsewhere) `machine_electric_motor` in
     turn, with fresh output appearing at each station each time.
  7. A manually-driven bill (claim -> mark working -> add progress ->
     complete; AI neutralised throughout for determinism) on
     `machine_wiring` shows progress frozen at midnight and completing
     once flipped to noon — the stall/resume loop #591's acceptance
     criteria call for, against real content instead of a fixture.

Usage: python3 tools/machine_shop_probe.py [--port 9391]
Exit 0 = every check passed.
"""
from __future__ import annotations

import argparse
import glob
import sys

from probelib import boot, quit_engine, send, send_json, init_arena, poll_until

PAGE = "machine_shop_probe"

# Furnace group.
FURNACE_XY = (6, 2)
FURNACE_UNIT_XY = (6, 3)
# Machine shop group — far enough away (28 tiles) that its wire run
# never bridges with the furnace group's.
SHOP_XY = (6, 30)
SHOP_UNIT_XY = (6, 31)


def as_int(s) -> int | None:
    try:
        return int(float(s))
    except (TypeError, ValueError):
        return None


def check(passed: bool, ok: bool, label: str, detail="") -> bool:
    print(f"  [{'PASS' if ok else 'FAIL'}] {label}" + (f": {detail}" if detail else ""))
    return passed and ok


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


# unit_ai's update() ticks automatically from engine boot (no opt-in
# load needed) and would otherwise autonomously claim/work the bills
# this probe drives by hand — neutralise it, mirroring
# power_workshop_probe.py / craft_bill_probe.py.
def ai_off(port: int) -> None:
    send(port,
         "local ai = require('scripts.unit_ai'); "
         "if not ai.__probe_orig_update then "
         "ai.__probe_orig_update = ai.update end; "
         "ai.update = function() end; return 'ai-off'")


def spawn_unit(port: int, x: float, y: float) -> int:
    """Raw unit.spawn — no clear_find_water (that needs a real AI
    tick to seed state, and ai_off() disables ticking before any unit
    exists here, mirroring craft_probe.py's plain unit.spawn: nothing
    autonomous ever runs, so there's no wandering goal to retire)."""
    uid = as_int(send(port, f"return unit.spawn('acolyte', {x}, {y})"))
    if uid is None:
        sys.exit("unit.spawn failed")
    return uid


def spawn_station(port: int, uid: int, def_name: str, gx: int, gy: int,
                   materials: dict[str, int], progress: int = 1000) -> int:
    """building.spawn + deliver build materials through the real
    machinery, then addBuildProgress to Built."""
    raw = send(port, f"return building.spawn('{def_name}', {gx}, {gy})")
    bid = as_int(raw)
    if bid is None:
        sys.exit(f"building.spawn('{def_name}') failed: {raw}")
    if not poll_until(5, lambda: send(
            port, f"return building.getInfo({bid}) and 'yes' or 'no'") == "yes"):
        sys.exit(f"{def_name} instance never appeared")
    for item, count in materials.items():
        send(port,
             f"for i=1,{count} do unit.addItem({uid},'{item}'); "
             f"unit.transferItemToBuilding({uid},{bid},'{item}') end; "
             f"return 'ok'")
    if send(port, f"return building.areMaterialsSatisfied({bid}) "
                  f"and 'yes' or 'no'") != "yes":
        sys.exit(f"{def_name} materials not satisfied after delivery")
    send(port, f"building.addBuildProgress({bid}, {progress}); return 'ok'")
    act = send(port, f"return building.getActivity({bid})")
    if act != "built":
        sys.exit(f"{def_name} never reached built (activity={act})")
    return bid


def wire_up(port: int, uid: int, gx: int, gy: int) -> tuple[int, int]:
    """Place a solar panel + battery beside a station at (gx,gy) and
    bridge station-panel-battery with two wire tiles, mirroring the
    #360 connectivity tests' layout. Returns (panel_bid, batt_bid)."""
    send(port, f"unit.addItem({uid}, 'solar_panel'); "
               f"unit.addItem({uid}, 'high_voltage_battery'); return 'ok'")
    panel_bid = as_int(send(port,
        f"local nid, bid = power.placeNode({uid}, 'solar_panel', {gx + 2}, {gy}); return bid"))
    batt_bid = as_int(send(port,
        f"local nid, bid = power.placeNode({uid}, 'high_voltage_battery', {gx + 4}, {gy}); return bid"))
    if panel_bid is None or batt_bid is None:
        sys.exit(f"power.placeNode failed near ({gx},{gy}): panel={panel_bid} batt={batt_bid}")
    send(port, f"require('scripts.wire').place({gx + 1}, {gy}); return 'ok'")
    send(port, f"require('scripts.wire').place({gx + 3}, {gy}); return 'ok'")
    return panel_bid, batt_bid


def network_for_building(port: int, bid: int):
    node = send_json(port, f"return power.getNodeForBuilding({bid})")
    if not isinstance(node, dict) or "id" not in node:
        return None
    return send_json(port, f"return power.getNetworkForNode({node['id']})")


def drain_of(port: int, panel_bid: int) -> float | None:
    net = network_for_building(port, panel_bid)
    return net.get("drainW") if isinstance(net, dict) else None


def inventory_count(port: int, uid: int, name: str) -> int:
    """craft.executeAt's fresh outputs land in the CRAFTER's own
    inventory (it returns their instance ids so the caller decides
    what to do with them — the craft_job AI drops them to the
    station's ground, but a bare executeAt call like this probe's
    doesn't), so "did output appear" is an inventory check, not a
    ground-item scan."""
    return as_int(send(port,
        f"local n=0; for _,it in ipairs(unit.getInventory({uid}) or {{}}) do "
        f"if it.defName=='{name}' then n=n+1 end end; return n")) or 0


def exec_recipe(port: int, uid: int, recipe: str, bid: int):
    # craft.executeAt's second return value is an ERROR STRING on
    # refusal but the fresh output INSTANCE IDS (a list) on success —
    # "res" names it generically rather than implying it's always an
    # error.
    return send_json(port,
        f"local ok,res = craft.executeAt({uid}, '{recipe}', {bid}); "
        f"return {{ok = ok, res = res}}")


def main() -> int:
    ap = argparse.ArgumentParser(description=__doc__,
                                  formatter_class=argparse.RawDescriptionHelpFormatter)
    ap.add_argument("--port", type=int, default=9391)
    args = ap.parse_args()
    port = args.port
    passed = True

    proc = boot(port, f"/tmp/machine_shop_probe_{port}.log")
    try:
        bootstrap_defs(port)
        init_arena(port, PAGE)
        send(port, f"world.setTimeScale('{PAGE}', 0); return 'ok'", expect_result=False)
        send(port, f"world.setTime('{PAGE}', 0, 0); return 'ok'", expect_result=False)
        ai_off(port)

        # --- 1. Real defs load with the right shape ---
        smelt_e = send_json(port, "return craft.get('smelt_steel_electric')")
        passed = check(passed,
            isinstance(smelt_e, dict) and smelt_e.get("powerDraw", 0) > 0
            and not smelt_e.get("fuel"),
            "smelt_steel_electric: power_draw>0, no fuel line", smelt_e)

        wiring_r = send_json(port, "return craft.get('machine_wiring')")
        passed = check(passed,
            isinstance(wiring_r, dict) and wiring_r.get("powerDraw", 0) > 0
            and wiring_r.get("station") == "machine",
            "machine_wiring: power_draw>0, station=machine", wiring_r)

        motor_r = send_json(port, "return craft.get('machine_electric_motor')")
        passed = check(passed,
            isinstance(motor_r, dict) and motor_r.get("powerDraw", 0) > 0
            and motor_r.get("station") == "machine",
            "machine_electric_motor: power_draw>0, station=machine", motor_r)

        coal_r = send_json(port, "return craft.get('smelt_steel_lignite')")
        passed = check(passed,
            isinstance(coal_r, dict) and coal_r.get("powerDraw", 0) == 0
            and isinstance(coal_r.get("fuel"), dict),
            "existing smelt_steel_lignite unaffected (power_draw=0, still has fuel)",
            coal_r)

        # --- 2. Build both stations for real ---
        fx, fy = FURNACE_XY
        ux, uy = FURNACE_UNIT_XY
        uid_f = spawn_unit(port, ux, uy)
        bid_furnace = spawn_station(port, uid_f, "furnace", fx, fy,
                                     {"granite_chunk": 6, "steel_bar": 2})

        sx, sy = SHOP_XY
        sux, suy = SHOP_UNIT_XY
        uid_s = spawn_unit(port, sux, suy)
        bid_shop = spawn_station(port, uid_s, "machine_shop", sx, sy,
                                  {"steel_bar": 8, "steel_hardware": 6,
                                   "steel_plate": 4, "electric_motor": 1})

        defs = send_json(port, "return building.listDefs()")
        shop_def = next((d for d in defs if isinstance(d, dict)
                          and d.get("name") == "machine_shop"), None) \
            if isinstance(defs, list) else None
        passed = check(passed,
            isinstance(shop_def, dict) and shop_def.get("category") == "Production"
            and shop_def.get("buildWork") == 200.0,
            "machine_shop registered in building.listDefs() (req. 8)", shop_def)

        ops = send_json(port, f"return building.getOperations({bid_shop})")
        passed = check(passed, ops == ["machine"],
                       "built machine_shop instance offers the 'machine' operation", ops)

        # --- 3. Regression: existing coal smelting still works UNWIRED ---
        send(port, f"unit.addItem({uid_f}, 'iron_ore_chunk'); "
                   f"unit.addItem({uid_f}, 'lignite_chunk'); "
                   f"unit.addItem({uid_f}, 'lignite_chunk'); "
                   f"unit.addItem({uid_f}, 'lignite_chunk'); return 'ok'")
        before_coal = inventory_count(port, uid_f, "steel_bar")
        r = exec_recipe(port, uid_f, "smelt_steel_lignite", bid_furnace)
        passed = check(passed, isinstance(r, dict) and r.get("ok") is True,
                       "existing coal-fired smelt still succeeds, furnace fully unwired", r)
        after_coal = inventory_count(port, uid_f, "steel_bar")
        passed = check(passed, after_coal > before_coal,
                       "coal-smelted steel_bar appeared", f"{before_coal} -> {after_coal}")

        # --- 4. New powered recipes refuse unwired ---
        send(port, f"unit.addItem({uid_f}, 'iron_ore_chunk'); return 'ok'")
        r = exec_recipe(port, uid_f, "smelt_steel_electric", bid_furnace)
        passed = check(passed,
            isinstance(r, dict) and r.get("ok") is False and "no power" in str(r.get("res", "")),
            "smelt_steel_electric refuses (furnace unwired)", r)

        send(port, f"unit.addItem({uid_s}, 'bronze_bar'); return 'ok'")
        r = exec_recipe(port, uid_s, "machine_wiring", bid_shop)
        passed = check(passed,
            isinstance(r, dict) and r.get("ok") is False and "no power" in str(r.get("res", "")),
            "machine_wiring refuses (machine_shop unwired)", r)

        send(port, f"unit.addItem({uid_s}, 'steel_bar'); "
                   f"for i=1,4 do unit.addItem({uid_s}, 'wiring') end; return 'ok'")
        r = exec_recipe(port, uid_s, "machine_electric_motor", bid_shop)
        passed = check(passed,
            isinstance(r, dict) and r.get("ok") is False and "no power" in str(r.get("res", "")),
            "machine_electric_motor refuses (machine_shop unwired)", r)

        # --- 5. Wire each station to its own panel + battery, midnight ---
        panel_f, batt_f = wire_up(port, uid_f, fx, fy)
        panel_s, batt_s = wire_up(port, uid_s, sx, sy)
        wired_f = poll_until(5, lambda: len(
            (network_for_building(port, panel_f) or {}).get("nodeIds", [])) == 2)
        wired_s = poll_until(5, lambda: len(
            (network_for_building(port, panel_s) or {}).get("nodeIds", [])) == 2)
        passed = check(passed, bool(wired_f) and bool(wired_s),
                       "both groups wired into their own 2-node network")
        passed = check(passed,
            drain_of(port, panel_f) == 0 and drain_of(port, panel_s) == 0,
            "drainW == 0 on both networks at midnight (no bill claimed)")

        # --- 6. Noon: all three new recipes succeed ---
        send(port, f"world.setTime('{PAGE}', 12, 0); return 'ok'")
        noon = poll_until(10, lambda: (network_for_building(port, panel_f) or {})
                           .get("generationW", 0) > 300)
        passed = check(passed, bool(noon), "time flip to noon landed (generationW > 300)")

        before = inventory_count(port, uid_f, "steel_bar")
        r = exec_recipe(port, uid_f, "smelt_steel_electric", bid_furnace)
        passed = check(passed, isinstance(r, dict) and r.get("ok") is True,
                       "smelt_steel_electric succeeds once furnace is powered", r)
        after = inventory_count(port, uid_f, "steel_bar")
        passed = check(passed, after > before, "electric-smelted steel_bar appeared",
                       f"{before} -> {after}")

        before = inventory_count(port, uid_s, "wiring")
        r = exec_recipe(port, uid_s, "machine_wiring", bid_shop)
        passed = check(passed, isinstance(r, dict) and r.get("ok") is True,
                       "machine_wiring succeeds once machine_shop is powered", r)
        after = inventory_count(port, uid_s, "wiring")
        passed = check(passed, after > before, "spooled wiring appeared",
                       f"{before} -> {after}")

        before = inventory_count(port, uid_s, "electric_motor")
        r = exec_recipe(port, uid_s, "machine_electric_motor", bid_shop)
        passed = check(passed, isinstance(r, dict) and r.get("ok") is True,
                       "machine_electric_motor succeeds once machine_shop is powered", r)
        after = inventory_count(port, uid_s, "electric_motor")
        passed = check(passed, after > before, "assembled electric_motor appeared",
                       f"{before} -> {after}")

        # --- 7. Manually-driven bill: stall at midnight, complete at noon.
        # craft.addBillProgress is a plain pour primitive with NO power
        # gate of its own (Craft.Bills.addBillProgress) — the gate lives
        # in the craft_job AI's own discipline of checking
        # power.isStationPoweredForRecipe(bid, recipe, billId) before
        # ever calling addBillProgress (scripts/unit_ai_craft.lua, "working"
        # phase). This phase mirrors that exact AI decision by hand
        # (AI still neutralised) rather than asserting a gate that
        # doesn't exist on the primitive itself. ---
        send(port, f"world.setTime('{PAGE}', 0, 0); return 'ok'")
        send(port, f"unit.addItem({uid_s}, 'bronze_bar'); return 'ok'")
        bill_raw = send(port,
            f"local id,err = craft.addBill({bid_shop}, 'machine_wiring', 1); "
            f"return id and ('ID:'..id) or ('ERR:'..tostring(err))")
        bill_id = int(bill_raw[3:]) if bill_raw.startswith("ID:") else None
        passed = check(passed, bill_id is not None, "manual bill queued", bill_raw)

        claimed = send(port, f"return craft.claimBill({bill_id}, {uid_s}, 60)")
        passed = check(passed, claimed == "true", "manual claim succeeds", claimed)
        send(port, f"craft.setBillWorking({bill_id}, true); return 'ok'")
        passed = check(passed,
            drain_of(port, panel_s) == 120.0,
            "drainW reads machine_wiring's 120W once marked working", drain_of(port, panel_s))

        send(port, f"world.setTime('{PAGE}', 0, 0); return 'ok'")
        gate_midnight = send(port,
            f"return power.isStationPoweredForRecipe({bid_shop}, 'machine_wiring', {bill_id})")
        passed = check(passed, gate_midnight == "false",
                       "isStationPoweredForRecipe false at midnight (the AI's own pour gate)",
                       gate_midnight)
        stalled = send_json(port, f"local b = craft.getBill({bill_id}); return b and b.progress or -1")
        passed = check(passed, stalled == 0,
                       "progress stays 0 -- the (mirrored) AI gate blocks the pour while browned out",
                       stalled)

        send(port, f"world.setTime('{PAGE}', 12, 0); return 'ok'")
        gate_noon = poll_until(10, lambda: send(port,
            f"return power.isStationPoweredForRecipe({bid_shop}, 'machine_wiring', {bill_id})") == "true")
        passed = check(passed, bool(gate_noon), "isStationPoweredForRecipe true at noon")
        send(port, f"craft.addBillProgress({bill_id}, 1.0); return 'ok'")
        r = send_json(port,
            f"local ok,err = craft.executeAt({uid_s}, 'machine_wiring', {bid_shop}, {bill_id}); "
            f"return {{ok = ok, err = err}}")
        passed = check(passed, isinstance(r, dict) and r.get("ok") is True,
                       "craft.executeAt fires the cycle once progress hits 1.0 while powered", r)
        remaining = send(port, f"return craft.completeBillCycle({bill_id})")
        passed = check(passed, remaining == "0",
                       "completeBillCycle finishes the one-shot bill (remaining=0)", remaining)

        print("\n" + ("ALL MACHINE SHOP CHECKS PASSED" if passed else "SOME FAILED"))
        return 0 if passed else 1
    finally:
        quit_engine(port, proc)


if __name__ == "__main__":
    sys.exit(main())
