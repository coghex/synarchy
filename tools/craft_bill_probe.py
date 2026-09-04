#!/usr/bin/env python3
"""Craft bill + craft AI probe (#329) — the bill backend and the
craft_job worker loop, end to end.

Boots a headless engine on a flat arena, loads defs + recipes +
buildings, builds a furnace through the real delivery + build-progress
machinery, then checks:

  1. Bill backend (craft.* verbs, AI neutralised):
     - addBill validation: unknown recipe, repair-tagged recipe, a
       station that doesn't offer the recipe's operation, unknown
       building — all refused with a reason; a valid bill returns an
       id and the expected shape (pending, no claimant, progress 0).
     - getBills lists it (globally and per station).
     - Claims: claim wins, a rival's claim against a fresh holder is
       refused, the holder can refresh, an expired claim is taken
       over, release hands the bill back with progress kept.
     - Progress clamps to [0,1]; completeBillCycle counts down and
       removes the bill at 0; cancelBill removes outright.
  2. Craft AI (unit_ai craft_job, AI live): with steel bars lying near
     the built furnace and a 2-count bill queued, an acolyte claims
     the bill (observable via getBill().claimant), sources the bars
     from the ground, works the station, and produces — the FRESH
     output instances (ids returned by craft.executeAt) are laid down
     at the station as ground items while a same-def item the crafter
     already carried stays carried, the bill is removed when its count
     runs out, and the crafter earns trade-skill XP ("smithing" for
     untagged recipes).

     The carried-item half of that needs two fixture preconditions
     established first (#1772), because without them there is no run in
     which the assertion can both be reached AND pass. The fixture chunk
     weighs 10 kg against an acolyte's ~16 kg carrying_capacity on top
     of a ~12 kg loadout, so (a) it leaves the crafter overweight and
     unit_ai_fetch's loadFeasible (#1326) refuses every input fetch,
     and (b) at that fill store_materials outranks wander and hauls the
     overload into the furnace's own storage. On master those were the
     SAME event: the craft could only proceed BECAUSE the auto-haul
     took the fixture item away. So the phase now grants the crafter
     headroom to carry the chunk and its input together, suspends
     store_materials by pulling storage targets out of scan range, and
     asserts both preconditions — plus the pre-suppression evidence
     that the auto-haul really does outrank wander at the fixture's own
     load — before restoring everything it changed. Nothing in
     production moves: the #1326 gate and the auto-store policy are
     untouched, the fixture just stops violating one and racing the
     other. The identity contract itself — a craft never consumes or
     replaces a same-def instance the crafter already held — is pinned
     without any AI timing at all by
     Test.Headless.Craft.OutputIdentity.
  3. Knowledge gate: a knowledge-gated bill draws no worker while the
     acolyte doesn't know the theory, and is worked once
     unit.setKnowledge grants it.
  4. Cargo rung: with the only steel bar deposited in a built cargo
     hold (no loose/mule stock), the crafter withdraws it from storage
     (the last rung of the sourcing ladder) and works the bill to
     completion, emptying the store.
  5. Pause boundary (#796), AI live: pausing a multi-cycle bill while
     it's actively worked lets exactly the in-flight cycle finish, then
     clears the claim so the bill sits idle (no second cycle) until
     unpaused; pausing a bill still fetching/walking (never reached
     "working") aborts and releases it outright, with the sourced
     material neither lost nor duplicated. Both resume production once
     unpaused.
  6. Until-stock bills (#795), AI live: craft.addBill(bid, recipe, nil,
     target) persists a real stock-target order (mode/target/output
     shape on getBill). At zero stock, the AI crafts up to (never
     fewer/more than) the target, correctly accounting for a
     multi-item-per-cycle recipe, then goes idle (claim released,
     condition-satisfied) with no further production. Removing output
     below the target makes the SAME bill eligible again and it
     replenishes; a NEW bill queued while stock already covers its
     target is never claimed at all. Two bills racing toward the same
     target on two different crafters settle to a BOUNDED overshoot
     (never unbounded runaway production) and both end up idle. A
     white-box check drives craftUtility/craftExecute directly to
     reproduce the exact scan-then-claim race deterministically: stock
     rising to target IN BETWEEN the scan that picked a candidate and
     the later claim attempt must still refuse the claim.

  7. Dead-claimant reconciliation (#1680), AI off, real power grid: a
     PAUSED bill for a positive-power recipe, claimed and marked
     working, keeps its station drawing the recipe's wattage; destroying
     the claimant then drops the claim and the working flag on their own
     -- nothing releases the bill, and while paused no live rival can
     take it over either -- and the network's drain returns to zero and
     STAYS there across an observation window, with the bill's progress
     and remaining count preserved for a later crafter.

     Paused deliberately: an UNPAUSED stale bill can be repaired
     incidentally by an ordinary dead-claimant takeover
     (Craft.Bills.claimAvailable), which would let this phase pass
     without the reconciliation existing at all.

     Not covered here: a real save/quit/restart/load round-trip for an
     until-stock bill — this probe's fixture is an ARENA world, which
     per CLAUDE.md's #365 note hangs the world thread on load, so
     arenas can't be a save-test page. The new persisted fields
     (cbMode/cbTarget/cbOutputItem) round-trip through the exact same
     generic Serialize derivation every other CraftBill field already
     relies on, proven directly by the pure hspec roundtrip test in
     Test.Headless.Craft.Bills.

Usage: python3 tools/craft_bill_probe.py [--port 9319]
"""
import argparse, glob, json, socket, subprocess, sys, time
from probelib import clear_find_water, quit_engine, boot, send, send_json

SPROOT = "/tmp"
# Owner tag for the identity window's temporary carrying-capacity
# modifier, so unit.removeModifier can take back exactly it.
CAPACITY_MOD_SOURCE = "craft_bill_probe_identity_window"
TEST_YAML = f"{SPROOT}/craft_bill_probe_recipes.yaml"

# Probe recipes: tiny work values so a cycle completes in seconds
# (shipped smelts are work 30 ≈ a minute at skill 0). Inputs/outputs
# use shipped item defs.
TEST_RECIPES = """\
recipes:
  - id: bill_probe_smelt
    station: smelt
    inputs:
      - item: steel_bar
    work: 2
    outputs:
      - item: granite_chunk
        count: 2
  - id: bill_probe_gated
    station: smelt
    knowledge: metallurgy
    inputs:
      - item: steel_bar
    work: 1
    outputs:
      - item: granite_chunk
  - id: bill_probe_until
    station: smelt
    inputs:
      - item: steel_bar
    work: 1
    outputs:
      - item: bronze_bar
        count: 2
  - id: bill_probe_powered
    station: smelt
    inputs:
      - item: steel_bar
    work: 30
    outputs:
      - item: granite_chunk
    power_draw: 150
"""

# The #1680 phase's recipe wattage, mirrored from TEST_RECIPES above so
# the drain assertions name one number.
POWERED_DRAW_W = 150.0


def bootstrap(port):
    """Load defs + the flat arena (the loading screen doesn't run
    headless). unit_ai stays live — it's half of what's under test —
    but is toggled off around the backend phase (see ai_off/ai_on)."""
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
    send(port,
         "return require('scripts.movement_arena').buildCourse('flat').name")
    for _ in range(60):
        raw = send(port, "return world.getActiveWorldId()").strip().strip('"')
        if raw and raw not in ("null", "nil"):
            break
        time.sleep(0.5)
    else:
        sys.exit("arena page never became the active world")
    send(port, "return world.loadChunksInRegion(-1, -1, 2, 2)")
    send(port, "return world.waitForChunks(60)", timeout=65.0)


# The stash lives on the module table (NOT _G): debug-console lines
# don't share a global env, but the loaded module table persists.
def ai_off(port):
    send(port,
         "local ai = require('scripts.unit_ai'); "
         "if not ai.__probe_orig_update then "
         "ai.__probe_orig_update = ai.update end; "
         "ai.update = function() end; return 'ai-off'")


def ai_on(port):
    send(port,
         "local ai = require('scripts.unit_ai'); "
         "if ai.__probe_orig_update then "
         "ai.update = ai.__probe_orig_update end; "
         "return 'ai-on'")


def spawn_acolyte(port, x, y):
    uid = int(float(send(port, f"return unit.spawn('acolyte', {x}, {y})")))
    if uid < 0:
        sys.exit("unit.spawn failed")
    time.sleep(0.5)
    # Retire the spawn-seeded find_water goal: the arena has no water,
    # and a scouting acolyte walks off-course instead of crafting.
    clear_find_water(port, uid)
    return uid


def spawn_station(port, uid, def_name, gx, gy, materials, progress=500):
    """building.spawn + deliver build materials through the real
    machinery, then addBuildProgress to Built."""
    raw = send(port, f"return building.spawn('{def_name}', {gx}, {gy})")
    try:
        bid = int(float(raw))
    except ValueError:
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


def inv_instance_ids(port, uid, name):
    """Instance ids of all top-level inventory items with defName."""
    raw = send(port,
        f"local out={{}}; for _,it in ipairs(unit.getInventory({uid}) "
        f"or {{}}) do if it.defName=='{name}' then "
        f"out[#out+1]=it.instanceId end end; return out")
    try:
        ids = json.loads(raw)
        return ids if isinstance(ids, list) else []
    except json.JSONDecodeError:
        return []


def account_instance(port, iid):
    """Where instance `iid` actually is: a unit's inventory, the ground,
    or a building's storage. #1772's fixture failed with a bare
    `kept=[]`, which reads as "destroyed" while the instance was in fact
    sitting in the furnace's storage -- so a failure here names the
    holder instead of leaving the next reader to rebuild the trace."""
    found = send_json(port,
        f"local want={iid}; local out={{}}; "
        f"for _,u in ipairs(unit.getAllIds() or {{}}) do "
        f"for _,it in ipairs(unit.getInventory(u) or {{}}) do "
        f"if it.instanceId==want then out[#out+1]='unit:'..u end end end; "
        f"for _,g in ipairs(item.listGround() or {{}}) do "
        f"if g.instanceId==want then out[#out+1]='ground' end end; "
        f"for _,b in ipairs(building.getActiveIds() or {{}}) do "
        f"for _,it in ipairs(building.getStorage(b) or {{}}) do "
        f"if it.instanceId==want then out[#out+1]='building:'..b end end end; "
        f"return out")
    if isinstance(found, list) and found:
        return ",".join(str(x) for x in found)
    return "unaccounted"


def acolyte_tunable(port, name):
    """One live acolyte tunable, so the window below compares against
    and restores what it actually found rather than a literal copy that
    would go stale if a default moved."""
    return float(send(port,
        f"return require('scripts.unit_ai_tunables').acolyte.{name}"))


def unit_load(port, uid):
    """(carrying weight, effective carrying_capacity) for `uid` -- the
    two numbers unit_ai_fetch's loadFeasible (#1326) compares."""
    raw = send(port,
        f"return string.format('%.4f,%.4f', "
        f"unit.getCarryingWeight({uid}) or -1, "
        f"unit.getStat({uid}, 'carrying_capacity') or -1)").strip('"')
    carried, cap = raw.split(",")
    return float(carried), float(cap)


def set_store_scan_range(port, value):
    """store_materials' scan range on the LIVE acolyte tunables --
    unit_ai.tickOne re-reads config[defName] every tick, so this takes
    effect on the next decision."""
    send(port, "require('scripts.unit_ai_tunables').acolyte"
               f".store_scan_range = {value}; return 'ok'")


def store_materials_utility(port, uid):
    """storeMaterialsUtility for `uid` under the live tunables, against a
    throwaway state table so the unit's real AI state is untouched.
    Returns the string 'ineligible' for -math.huge (JSON has no
    infinity), else the numeric utility as text."""
    return send(port,
        "local lg=require('scripts.unit_ai_logistics'); "
        "local p=require('scripts.unit_ai_tunables').acolyte; "
        f"local u=lg.storeMaterialsUtility({uid}, {{}}, p); "
        "return (u == -math.huge) and 'ineligible' or tostring(u)"
        ).strip('"')


def add_bill(port, bid, recipe, count=None):
    """→ (billId or None, err)."""
    arg = f", {count}" if count is not None else ""
    raw = send(port,
               f"local id,err = craft.addBill({bid}, '{recipe}'{arg}); "
               f"return id and ('ID:'..id) or ('ERR:'..tostring(err))"
               ).strip('"')
    if raw.startswith("ID:"):
        return int(float(raw[3:])), ""
    return None, raw


def add_until_bill(port, bid, recipe, target):
    """→ (billId or None, err) for an until-stock bill (#795)."""
    raw = send(port,
               f"local id,err = craft.addBill({bid}, '{recipe}', nil, {target}); "
               f"return id and ('ID:'..id) or ('ERR:'..tostring(err))"
               ).strip('"')
    if raw.startswith("ID:"):
        return int(float(raw[3:])), ""
    return None, raw


def ground_count_near(port, name, gx, gy, radius):
    return int(float(send(port,
        f"local n=0; for _,g in ipairs(item.listGround() or {{}}) do "
        f"if g.defName=='{name}' and math.abs(g.x-{gx})<={radius} "
        f"and math.abs(g.y-{gy})<={radius} then n=n+1 end end; return n")))


def ground_stock(port, name):
    """Global ground count of `name`, no range limit -- the until-stock
    target's authoritative scope (mirrors groundStockTally/
    groundStockCountOf)."""
    return int(float(send(port,
        f"local n=0; for _,g in ipairs(item.listGround() or {{}}) do "
        f"if g.defName=='{name}' then n=n+1 end end; return n")))


def first_network(port):
    """The (only) network this probe ever wires up -- power.listNetworks()
    on the active page, or None before anything is wired. Keyed off
    array position, not a building id: an idle station with no actively
    worked bill never appears in consumerIds at all (#590), which is
    exactly the state the #1680 phase ends in."""
    nets = send_json(port, "return power.listNetworks()")
    if isinstance(nets, list) and nets and isinstance(nets[0], dict):
        return nets[0]
    return None


def drain_of(port):
    net = first_network(port)
    return net.get("drainW") if isinstance(net, dict) else None


def unit_exists(port, uid):
    return send(port, f"return unit.exists({uid})").strip('"') == "true"


def check(passed, ok, label, detail=""):
    print(f"  [{'PASS' if ok else 'FAIL'}] {label}"
          + (f": {detail}" if detail else ""))
    return passed and ok


def poll(port, seconds, fn, interval=1.0):
    """Poll fn until true, defensively unpausing each pass: notification
    categories can be user-configured to auto-pause (config/
    notifications.local.yaml), and e.g. a stuck-walk unit_warning
    mid-fetch would otherwise freeze the whole sim under the probe."""
    deadline = time.time() + seconds
    while time.time() < deadline:
        send(port, "engine.setPaused(false); return 'ok'")
        if fn():
            return True
        time.sleep(interval)
    return False


def main():
    ap = argparse.ArgumentParser()
    ap.add_argument("--port", type=int, default=9319)
    args = ap.parse_args()
    port = args.port
    passed = True

    proc = boot(port, f"{SPROOT}/craft_bill_probe_engine.log")
    try:
        bootstrap(port)
        with open(TEST_YAML, "w") as f:
            f.write(TEST_RECIPES)
        n = int(float(send(port, f"return engine.loadRecipeYaml('{TEST_YAML}')")))
        passed = check(passed, n == 4, "probe recipes loaded", f"count={n}")

        # Backend phase runs with the AI off so no acolyte races the
        # scripted claim/progress calls.
        ai_off(port)
        uid = spawn_acolyte(port, 2, 2)
        bid_f = spawn_station(port, uid, "furnace", 6, 2,
                              {"granite_chunk": 6, "steel_bar": 2})

        # --- 1. Bill backend ---
        b, msg = add_bill(port, bid_f, "no_such_recipe")
        passed = check(passed, b is None and "unknown recipe" in msg,
                       "addBill(unknown recipe) refused", msg)
        b, msg = add_bill(port, bid_f, "repair_condition")
        passed = check(passed, b is None and "repair recipe" in msg,
                       "addBill(repair recipe) refused", msg)
        b, msg = add_bill(port, bid_f, "forge_steel_dagger")
        passed = check(passed, b is None and "does not offer" in msg,
                       "addBill(forge recipe at furnace) refused", msg)
        b, msg = add_bill(port, 99999, "bill_probe_smelt")
        passed = check(passed, b is None and "no such building" in msg,
                       "addBill(unknown building) refused", msg)

        bill1, msg = add_bill(port, bid_f, "bill_probe_smelt", 3)
        passed = check(passed, bill1 is not None and bill1 >= 1,
                       "addBill ok returns an id", msg or f"id={bill1}")
        shape = send_json(port, f"return craft.getBill({uid}, {bill1})")
        ok = (isinstance(shape, dict) and shape.get("station") == bid_f
              and shape.get("recipe") == "bill_probe_smelt"
              and shape.get("remaining") == 3
              and shape.get("progress") == 0
              and "claimant" not in shape)
        passed = check(passed, ok, "getBill shape (pending, no claimant)",
                       shape)
        allb = send_json(port, "return craft.getBills()")
        perb = send_json(port, f"return craft.getBills({bid_f})")
        ok = (isinstance(allb, list) and isinstance(perb, list)
              and any(x.get("id") == bill1 for x in allb)
              and any(x.get("id") == bill1 for x in perb))
        passed = check(passed, ok, "getBills lists it (global + station)")

        uid2 = spawn_acolyte(port, 12, 12)
        ok = send(port, f"return craft.claimBill({bill1}, {uid}, 30) "
                        f"and 'y' or 'n'").strip('"') == "y"
        passed = check(passed, ok, "claimBill wins on a pending bill")
        ok = send(port, f"return craft.claimBill({bill1}, {uid2}, 30) "
                        f"and 'y' or 'n'").strip('"') == "n"
        passed = check(passed, ok, "rival claim against a fresh holder refused")
        claimant = send_json(port, f"return craft.getBill({uid}, {bill1})").get("claimant")
        passed = check(passed, claimant == uid, "claimant observable via getBill",
                       f"claimant={claimant}")
        ok = send(port, f"return craft.claimBill({bill1}, {uid}, 30) "
                        f"and 'y' or 'n'").strip('"') == "y"
        passed = check(passed, ok, "holder refresh succeeds")
        time.sleep(0.4)
        ok = send(port, f"return craft.claimBill({bill1}, {uid2}, 0.1) "
                        f"and 'y' or 'n'").strip('"') == "y"
        passed = check(passed, ok, "expired claim taken over (timeout 0.1s)")

        prog = send_json(port, f"return craft.addBillProgress({uid}, {bill1}, 0.6)")
        passed = check(passed, abs(float(prog) - 0.6) < 1e-6,
                       "addBillProgress 0.6", prog)
        prog = send_json(port, f"return craft.addBillProgress({uid}, {bill1}, 0.6)")
        passed = check(passed, abs(float(prog) - 1.0) < 1e-6,
                       "progress clamps at 1.0", prog)
        ok = send(port, f"return craft.releaseBill({uid}, {bill1}) and 'y' or 'n'"
                  ).strip('"') == "y"
        after = send_json(port, f"return craft.getBill({uid}, {bill1})")
        ok = (ok and isinstance(after, dict) and "claimant" not in after
              and abs(after.get("progress", 0) - 1.0) < 1e-6)
        passed = check(passed, ok, "release keeps progress, clears claimant",
                       after)

        rem = send_json(port, f"return craft.completeBillCycle({uid}, {bill1})")
        passed = check(passed, rem == 2, "completeBillCycle → remaining 2", rem)
        after = send_json(port, f"return craft.getBill({uid}, {bill1})")
        ok = (isinstance(after, dict) and after.get("remaining") == 2
              and after.get("progress") == 0)
        passed = check(passed, ok, "cycle reset progress + decremented", after)
        send_json(port, f"return craft.completeBillCycle({uid}, {bill1})")
        rem = send_json(port, f"return craft.completeBillCycle({uid}, {bill1})")
        gone = send(port, f"return craft.getBill({uid}, {bill1}) and 'y' or 'n'"
                    ).strip('"')
        passed = check(passed, rem == 0 and gone == "n",
                       "count exhausted removes the bill",
                       f"rem={rem} exists={gone}")

        bill2, _ = add_bill(port, bid_f, "bill_probe_smelt")   # repeat mode
        shape = send_json(port, f"return craft.getBill({uid}, {bill2})")
        passed = check(passed,
                       isinstance(shape, dict) and shape.get("remaining") == -1,
                       "count omitted → repeat forever (-1)", shape)
        ok = send(port, f"return craft.cancelBill({bill2}) and 'y' or 'n'"
                  ).strip('"') == "y"
        gone = send(port, f"return craft.getBill({uid}, {bill2}) and 'y' or 'n'"
                    ).strip('"')
        passed = check(passed, ok and gone == "n", "cancelBill removes")

        # uid2 was only a rival claimant; remove it so the AI phase has
        # exactly one worker.
        send(port, f"unit.destroy({uid2}); return 'ok'")

        # --- 2. Craft AI end to end ---
        # 3 bars on the ground near the furnace: 2 cycles × 1 input,
        # plus one spare for the knowledge-gate phase below.
        for i in range(3):
            send(port, f"item.spawnGround('steel_bar', {7.5 + 0.3*i}, 2.5); "
                       f"return 'ok'")
        # Output-identity fixture: the crafter carries a granite_chunk
        # of its own (same def as the bill's OUTPUT). The deposit must
        # drop the freshly crafted instances (dropItemById on the ids
        # executeAt returns), never this carried one.
        send(port, f"unit.addItem({uid}, 'granite_chunk'); return 'ok'")
        keep_ids = inv_instance_ids(port, uid, "granite_chunk")
        passed = check(passed, len(keep_ids) == 1,
                       "identity fixture: crafter carries one granite chunk",
                       keep_ids)
        # Two things about that one 10 kg chunk have to be established
        # before the AI runs, or this phase cannot test what it claims
        # to (#1772).
        #
        # (a) It puts the crafter OVER capacity. A spawned acolyte
        #     carries ~12 kg of loadout against a body-derived
        #     carrying_capacity of ~16 kg, so the fixture chunk lands it
        #     ~35 % overweight -- and unit_ai_fetch's loadFeasible
        #     (#1326) then refuses every input fetch, leaving the bill
        #     silently unclaimed. The phase only ever got moving on
        #     master because store_materials hauled the overload away,
        #     which is the very deposit that ate the fixture instance.
        #     So the craft "working" and the item "vanishing" were the
        #     SAME event: there was no run in which the assertion could
        #     both be reached and pass.
        #
        # (b) store_materials is a legitimate rival for it either way.
        #     granite_chunk is category Materials
        #     (data/items/granite_chunk.yaml) and the furnace declares
        #     100 kg of ordinary storage (data/buildings/furnace.yaml),
        #     so the crafter ends the bill standing adjacent to a valid
        #     target, and the action deliberately deposits EVERY carried
        #     Materials item (scripts/unit_ai_logistics.lua). That is
        #     intended hauling, not a bug -- it just must not be racing
        #     the assertion.
        #
        # Evidence for (b) first: with the chunk carried, a storage
        # target really does resolve, so the suspension below is doing
        # real work rather than disabling something already inert.
        #
        # ELIGIBILITY only -- deliberately not "and it outranks wander".
        # The utility is base * fill^3, and fill is carried/capacity
        # against a carrying_capacity rolled per spawn from body
        # composition (~16-39 kg observed across runs). So how far the
        # action clears the wander floor is spawn-roll dependent, and
        # asserting a margin here would just be a NEW flaky check in a
        # probe this change exists to make deterministic. The margin is
        # reported for the record; only eligibility is required.
        wander = acolyte_tunable(port, "base_wander_utility")
        crowded_util = store_materials_utility(port, uid)
        passed = check(passed, crowded_util != "ineligible",
                       "the auto-haul that took the chunk is eligible at "
                       "the fixture's own load",
                       f"store={crowded_util} wander={wander}")

        # (a): give the crafter headroom, so the craft can run with the
        # chunk still carried instead of needing it hauled away first.
        #
        # A MODIFIER, not unit.setStat: carrying_capacity is body-derived
        # (Unit.Thread.Command.Body recomputes it from lean mass and
        # strength), so a written base is liable to be recomputed away,
        # while a modifier is applied on TOP of whatever the base
        # currently is. +80 kg clears the whole cycle -- the ~12 kg
        # loadout, the 10 kg chunk, a 0.5 kg steel_bar input and the two
        # 10 kg fresh outputs held between executeAt and the drop --
        # from any spawn roll, and unit.removeModifier takes it back off
        # by source at the end of the window. The #1326 gate itself is
        # untouched; the fixture just stops violating it.
        _, cap_before = unit_load(port, uid)
        send(port, f"unit.addModifier({uid}, 'carrying_capacity', 80.0, "
                   f"'{CAPACITY_MOD_SOURCE}'); return 'ok'")
        carried, cap = unit_load(port, uid)
        # Prove the boost LANDED. Without this the check below would
        # pass on any spawn that happened to roll enough capacity on its
        # own, which is exactly the luck this fixture exists to remove.
        passed = check(passed, cap >= cap_before + 79.0,
                       "precondition: the identity window's capacity "
                       "headroom actually applied",
                       f"cap {cap_before} -> {cap}")
        bar_w = float(send(port,
            "for _,d in ipairs(item.listDefs() or {}) do "
            "if d.name=='steel_bar' then return d.weight or -1 end end; "
            "return -1"))
        passed = check(passed, bar_w > 0 and carried + bar_w <= cap,
                       "precondition: the crafter can carry the fixture "
                       "chunk AND the recipe's input (loadFeasible, #1326)",
                       f"carried={carried} + bar={bar_w} <= cap={cap}")

        # (b): suspend the rival for the length of the window, and assert
        # that precondition rather than assume it.
        #
        # NEGATIVE, not zero: findStorageTarget seeds bestD = maxRange
        # and accepts `d <= bestD`, so a range of 0 still matches a
        # target the unit is standing exactly on -- and the crafter is
        # allowed to stand ON a 1x1 station's footprint (executeAt gates
        # at Chebyshev <= 1). A negative range no distance can satisfy
        # closes that, rather than relying on the AI happening to pick a
        # perimeter tile.
        shipped_store_range = acolyte_tunable(port, "store_scan_range")
        set_store_scan_range(port, -1.0)
        supp_util = store_materials_utility(port, uid)
        passed = check(passed, supp_util == "ineligible",
                       "precondition: store_materials suspended for the "
                       "identity window (no storage target in range)",
                       f"utility={supp_util}")
        xp0 = float(send(port, f"return unit.getSkill({uid}, 'smithing') or 0"))
        bill3, msg = add_bill(port, bid_f, "bill_probe_smelt", 2)
        passed = check(passed, bill3 is not None, "AI bill queued", msg)
        ai_on(port)
        # Retire the find_water spawn goal AGAIN now that the AI is
        # live: the first real tick seeds spawn goals, overwriting a
        # retirement issued while the AI was off — and the water-scout
        # goal floor (~3.0) outranks the craft entry utility.
        time.sleep(1.5)
        clear_find_water(port, uid)

        claimed = poll(port, 30, lambda: send_json(
            port, f"return craft.getBill({uid}, {bill3})") in ("nil", "", None, "null")
            or send_json(port,
                         f"local b = craft.getBill({uid}, {bill3}); "
                         f"return b and b.claimant or -1") == uid)
        passed = check(passed, claimed, "AI claims the bill (or already done)")

        done = poll(port, 150, lambda: send(
            port, f"return craft.getBill({uid}, {bill3}) and 'y' or 'n'"
            ).strip('"') == "n")
        passed = check(passed, done, "2-count bill worked to completion")

        outs = ground_count_near(port, "granite_chunk", 6, 2, 3)
        passed = check(passed, outs >= 4,
                       "outputs laid down at the station (≥4 granite chunks)",
                       f"found={outs}")
        kept = inv_instance_ids(port, uid, "granite_chunk")
        held_by = account_instance(port, keep_ids[0]) if keep_ids else "n/a"
        passed = check(passed, kept == keep_ids,
                       "carried same-def item kept; only fresh outputs dropped",
                       f"kept={kept} expected={keep_ids} held_by={held_by}")
        # Window closed: hand back both fixture adjustments so every
        # later phase runs default behaviour.
        set_store_scan_range(port, shipped_store_range)
        send(port, f"unit.removeModifier({uid}, '{CAPACITY_MOD_SOURCE}'); "
                   f"return 'ok'")
        xp1 = float(send(port, f"return unit.getSkill({uid}, 'smithing') or 0"))
        passed = check(passed, xp1 > xp0,
                       "crafter earned smithing XP", f"{xp0} → {xp1}")

        # --- 3. Knowledge gate ---
        bill4, msg = add_bill(port, bid_f, "bill_probe_gated", 1)
        passed = check(passed, bill4 is not None, "gated bill queued", msg)
        poll(port, 8, lambda: False)   # let the AI tick, unpaused
        untouched = send(port,
            f"local b = craft.getBill({uid}, {bill4}); "
            f"return (b and not b.claimant) and 'y' or 'n'").strip('"') == "y"
        passed = check(passed, untouched,
                       "unknowing acolyte leaves the gated bill alone")
        send(port, f"unit.setKnowledge({uid}, 'metallurgy', 50); return 'ok'")
        done = poll(port, 90, lambda: send(
            port, f"return craft.getBill({uid}, {bill4}) and 'y' or 'n'"
            ).strip('"') == "n")
        passed = check(passed, done, "granted knowledge unlocks the bill")

        # --- 4. Cargo rung of the sourcing ladder ---
        # Every loose bar is consumed by now; stock the ONLY remaining
        # steel_bar inside a built cargo hold. The crafter must source
        # it from storage (inventory → ground → mule → cargo) to work
        # the bill. AI off for the scripted build + stocking so the
        # delivery AI doesn't race the fixture setup.
        ai_off(port)
        bid_c = spawn_station(port, uid, "cargo_hold_S", 2, 6,
                              {"steel_plate": 10, "steel_bar": 24,
                               "steel_hardware": 10, "electric_motor": 2,
                               "processing_unit": 2}, progress=5000)
        send(port, f"unit.addItem({uid}, 'steel_bar'); "
                   f"unit.depositToCargo({uid}, {bid_c}, 'steel_bar'); "
                   f"return 'ok'")
        stored = int(float(send(port,
            f"local n=0; for _,it in ipairs(building.getStorage({bid_c}) "
            f"or {{}}) do if it.defName=='steel_bar' then n=n+1 end end; "
            f"return n")))
        loose = ground_count_near(port, "steel_bar", 5, 4, 40)
        passed = check(passed, stored == 1 and loose == 0,
                       "fixture: the only bar lives in cargo storage",
                       f"stored={stored} loose={loose}")
        bill5, msg = add_bill(port, bid_f, "bill_probe_smelt", 1)
        passed = check(passed, bill5 is not None, "cargo-sourced bill queued",
                       msg)
        ai_on(port)
        done = poll(port, 150, lambda: send(
            port, f"return craft.getBill({uid}, {bill5}) and 'y' or 'n'"
            ).strip('"') == "n")
        emptied = int(float(send(port,
            f"local n=0; for _,it in ipairs(building.getStorage({bid_c}) "
            f"or {{}}) do if it.defName=='steel_bar' then n=n+1 end end; "
            f"return n")))
        passed = check(passed, done and emptied == 0,
                       "bill sourced from cargo storage worked to completion",
                       f"done={done} left_in_store={emptied}")

        # --- 5. Pause boundary through the real craft AI (#796) ---
        # 5a. Pause a multi-cycle bill while it's actively being worked:
        # exactly the in-flight cycle finishes, then the bill sits idle
        # (claim cleared, no second cycle) until unpaused.
        for i in range(3):
            send(port, f"item.spawnGround('steel_bar', {7.5 + 0.3*i}, 3.5); "
                       f"return 'ok'")
        uid3 = spawn_acolyte(port, 3, 3)
        bill6, msg = add_bill(port, bid_f, "bill_probe_smelt", 3)
        passed = check(passed, bill6 is not None,
                       "#796 pause-during-work bill queued (3-count)", msg)

        reached_working = poll(port, 60, lambda: send(port,
            f"local b=craft.getBill({uid}, {bill6}); "
            f"return (b and b.working) and 'y' or 'n'").strip('"') == "y")
        passed = check(passed, reached_working,
                       "AI reaches the working phase on the new bill")

        send(port, f"craft.setBillPaused({bill6}, true); return 'ok'")
        cycle_done = poll(port, 30, lambda: send(port,
            f"local b=craft.getBill({uid}, {bill6}); "
            f"return (b and b.remaining==2 and not b.claimant) and 'y' or 'n'"
            ).strip('"') == "y")
        passed = check(passed, cycle_done,
                       "exactly the in-flight cycle finishes; claim clears")

        # Observation window, engine kept unpaused: no second cycle
        # should start while still paused.
        poll(port, 8, lambda: False)
        still_idle = send(port,
            f"local b=craft.getBill({uid}, {bill6}); "
            f"return (b and b.remaining==2 and not b.claimant "
            f"and not b.working) and 'y' or 'n'").strip('"') == "y"
        passed = check(passed, still_idle,
                       "no second cycle begins during the observation window")

        send(port, f"craft.setBillPaused({bill6}, false); return 'ok'")
        finished = poll(port, 90, lambda: send(
            port, f"return craft.getBill({uid}, {bill6}) and 'y' or 'n'"
            ).strip('"') == "n")
        passed = check(passed, finished,
                       "unpausing lets a fresh claim finish the remaining cycles")

        # 5b. Pause BEFORE the claimant ever reaches "working" (still
        # fetching/walking): the job must abort and release outright —
        # no craft executes, and the fetched material is neither lost
        # nor duplicated. The material sits a SHORT walk from the
        # crafter (not the station) — far enough to reliably observe
        # "claimed but not working" via polling, but short enough to
        # resolve within a sane timeout: unit_ai.lua's dispatcher only
        # re-invokes an action's execute function (where the pause
        # check lives) once the unit's activity drops back to "idle"
        # (arrival) or the chosen action changes — never mid-walk,
        # so it doesn't clobber in-flight pathing every tick. A pause
        # noticed mid-walk therefore takes effect at the NEXT arrival,
        # not necessarily the instant it's set; a long walk (e.g. to a
        # station tens of tiles off) would blow past any reasonable
        # test timeout despite the fix working correctly, so this
        # fixture deliberately stays close instead of stress-testing
        # that unrelated, pre-existing dispatch behavior.
        send(port, f"unit.destroy({uid}); unit.destroy({uid3}); return 'ok'")
        uid4 = spawn_acolyte(port, 2, 2)
        send(port, "item.spawnGround('steel_bar', 2, 9); return 'ok'")
        outs_before = ground_count_near(port, "granite_chunk", 6, 2, 3)
        bill7, msg = add_bill(port, bid_f, "bill_probe_smelt", 1)
        passed = check(passed, bill7 is not None,
                       "#796 pause-during-fetch/walk bill queued", msg)

        claimed_not_working = poll(port, 30, lambda: send(port,
            f"local b=craft.getBill({uid4}, {bill7}); "
            f"return (b and b.claimant=={uid4} and not b.working) "
            f"and 'y' or 'n'").strip('"') == "y")
        passed = check(passed, claimed_not_working,
                       "AI claims bill7 while still fetching/walking")

        send(port, f"craft.setBillPaused({bill7}, true); return 'ok'")
        released = poll(port, 30, lambda: send(port,
            f"local b=craft.getBill({uid4}, {bill7}); "
            f"return (b and not b.claimant and not b.working) "
            f"and 'y' or 'n'").strip('"') == "y")
        passed = check(passed, released,
                       "pausing before 'working' aborts + releases the claim")

        outs_after = ground_count_near(port, "granite_chunk", 6, 2, 3)
        passed = check(passed, outs_after == outs_before,
                       "no craft executed for bill7 (no new outputs)",
                       f"{outs_before} -> {outs_after}")
        ground_left = ground_count_near(port, "steel_bar", 2, 9, 5)
        inv_left = len(inv_instance_ids(port, uid4, "steel_bar"))
        passed = check(passed, ground_left + inv_left == 1,
                       "the fetched steel_bar is neither lost nor duplicated",
                       f"ground={ground_left} inv={inv_left}")

        send(port, f"craft.setBillPaused({bill7}, false); return 'ok'")
        finished7 = poll(port, 90, lambda: send(
            port, f"return craft.getBill({uid4}, {bill7}) and 'y' or 'n'"
            ).strip('"') == "n")
        passed = check(passed, finished7,
                       "unpausing lets bill7 finish once reclaimed")

        # --- 6. Until-stock bills (#795) ---
        # 6a. Backend shape + AI-driven completion at zero stock,
        # correctly accounting for a multi-item-per-cycle recipe: target
        # 5 at 2 bronze_bar/cycle needs ceil(5/2)=3 cycles -- stops at
        # 6, never fewer (short of target) or more (a 4th cycle).
        ai_off(port)
        base_bronze = ground_stock(port, "bronze_bar")
        passed = check(passed, base_bronze == 0,
                       "fixture: no bronze_bar on the ground yet",
                       f"found={base_bronze}")
        for i in range(4):
            send(port, f"item.spawnGround('steel_bar', {7.5 + 0.3*i}, 4.5); "
                       f"return 'ok'")
        bill_u1, msg = add_until_bill(port, bid_f, "bill_probe_until", 5)
        passed = check(passed, bill_u1 is not None,
                       "#795 until-stock bill queued", msg)
        shape = send_json(port, f"return craft.getBill({uid4}, {bill_u1})")
        ok = (isinstance(shape, dict) and shape.get("mode") == "until"
              and shape.get("target") == 5
              and shape.get("outputItem") == "bronze_bar"
              and shape.get("remaining") == -1
              and "claimant" not in shape)
        passed = check(passed, ok,
                       "until-stock bill shape (mode/target/outputItem)", shape)

        uid5 = spawn_acolyte(port, 3, 4)
        ai_on(port)
        time.sleep(1.5)
        clear_find_water(port, uid5)
        reached = poll(port, 90, lambda: ground_stock(port, "bronze_bar") >= 5)
        passed = check(passed, reached, "AI crafts up to the target (>=5 bronze_bar)")
        stock_at_target = ground_stock(port, "bronze_bar")
        passed = check(passed, stock_at_target == 6,
                       "exactly 3 cycles run (2/cycle) -- stops at 6, not fewer/more",
                       f"stock={stock_at_target}")
        idled = poll(port, 20, lambda: send_json(
            port, f"return craft.getBill({uid5}, {bill_u1})").get("claimant") is None)
        passed = check(passed, idled,
                       "bill goes idle (claim released) once condition-satisfied")
        poll(port, 8, lambda: False)   # observation window, no player action
        stock_after_wait = ground_stock(port, "bronze_bar")
        passed = check(passed, stock_after_wait == stock_at_target,
                       "no further crafting while condition-satisfied",
                       f"{stock_at_target} -> {stock_after_wait}")

        # 6b. Consuming output below target makes the SAME bill eligible
        # again and it replenishes (one more cycle to cover the deficit).
        removed = int(float(send(port,
            "local n=0; for _,g in ipairs(item.listGround() or {}) do "
            "if g.defName=='bronze_bar' and n<3 then "
            "item.removeGround(g.id); n=n+1 end end; return n")))
        passed = check(passed, removed == 3, "removed 3 bronze_bar (stock -> 3)",
                       f"removed={removed}")
        stock_after_removal = ground_stock(port, "bronze_bar")
        passed = check(passed, stock_after_removal == 3,
                       "stock now below target (3 < 5)",
                       f"stock={stock_after_removal}")
        replenished = poll(port, 90, lambda: ground_stock(port, "bronze_bar") >= 5)
        passed = check(passed, replenished,
                       "the same bill automatically resumes and replenishes")

        # A NEW bill queued while stock already covers its target is
        # never claimed at all -- "added output while pending" case.
        stock_now = ground_stock(port, "bronze_bar")
        bill_u4, msg = add_until_bill(port, bid_f, "bill_probe_until",
                                      max(1, stock_now - 1))
        passed = check(passed, bill_u4 is not None,
                       "already-satisfied until-bill queued", msg)
        poll(port, 8, lambda: False)   # let the AI tick, unpaused
        untouched = send_json(port, f"return craft.getBill({uid5}, {bill_u4})")
        passed = check(passed,
                       isinstance(untouched, dict)
                       and untouched.get("claimant") is None,
                       "a bill already at/above target is never claimed",
                       untouched)
        send(port, f"craft.cancelBill({bill_u4}); return 'ok'")

        # 6c. Two bills, same output, same target, worked by two
        # DIFFERENT crafters -- bounded overshoot, never unbounded
        # runaway production (#795).
        ai_off(port)
        for i in range(6):
            send(port, f"item.spawnGround('steel_bar', {7.5 + 0.3*i}, 6.5); "
                       f"return 'ok'")
        base3 = ground_stock(port, "bronze_bar")
        target3 = base3 + 3
        bill_u2, msg = add_until_bill(port, bid_f, "bill_probe_until", target3)
        passed = check(passed, bill_u2 is not None, "racing bill A queued", msg)
        bill_u3, msg = add_until_bill(port, bid_f, "bill_probe_until", target3)
        passed = check(passed, bill_u3 is not None, "racing bill B queued", msg)
        uid6 = spawn_acolyte(port, 3, 6)
        uid7 = spawn_acolyte(port, 4, 6)
        ai_on(port)
        time.sleep(1.5)
        clear_find_water(port, uid6)
        clear_find_water(port, uid7)

        def both_idle():
            bA = send_json(port, f"return craft.getBill({uid6}, {bill_u2})")
            bB = send_json(port, f"return craft.getBill({uid6}, {bill_u3})")
            return (isinstance(bA, dict) and bA.get("claimant") is None
                    and isinstance(bB, dict) and bB.get("claimant") is None)

        settled = poll(port, 120, both_idle)
        passed = check(passed, settled,
                       "both racing until-bills settle to idle (no perpetual claim)")
        final_stock = ground_stock(port, "bronze_bar")
        # Each bill can overshoot the shared target by at most one
        # in-flight cycle (2 bronze_bar) before it notices the OTHER
        # bill already pushed stock to target -- bounded, never
        # unbounded, duplicate production.
        overshoot_bound = target3 + 2 * 2
        passed = check(passed, target3 <= final_stock <= overshoot_bound,
                       f"bounded overshoot, not unbounded ({target3} <= "
                       f"stock <= {overshoot_bound})",
                       f"stock={final_stock}")
        poll(port, 8, lambda: False)
        passed = check(passed, ground_stock(port, "bronze_bar") == final_stock,
                       "production has genuinely stopped (stable across a wait)")

        # 6d. White-box claim-boundary race (review round 1): stock can
        # rise BETWEEN the tick that scans/picks a candidate
        # (craftUtility) and the later tick that actually claims it
        # (craftExecute) -- e.g. another crafter's cycle lands in that
        # window. Drive the two AI entry points directly (module state
        # stashed on the loaded module table, same idiom as ai_off/
        # ai_on -- plain locals don't survive across debug-console
        # lines) so the race is reproduced deterministically rather
        # than relying on real AI tick timing.
        ai_off(port)
        uid8 = spawn_acolyte(port, 3, 8)
        send(port, "item.spawnGround('steel_bar', 3.3, 8); return 'ok'")
        base4 = ground_stock(port, "bronze_bar")
        target4 = base4 + 2
        bill_u5, msg = add_until_bill(port, bid_f, "bill_probe_until", target4)
        passed = check(passed, bill_u5 is not None,
                       "claim-boundary-race bill queued", msg)

        scanned = send(port,
            "local ai=require('scripts.unit_ai_craft'); ai.__probe_s={}; "
            "local p=require('scripts.unit_ai_tunables').acolyte; "
            f"ai.craftUtility({uid8}, ai.__probe_s, p); "
            "return ai.__probe_s.craftCandidate and 'has-candidate' "
            "or 'no-candidate'").strip('"')
        passed = check(passed, scanned == "has-candidate",
                       "scan (craftUtility) picks the not-yet-satisfied bill",
                       scanned)

        # Stock rises to the target IN BETWEEN the scan and the claim --
        # exactly the window the review flagged.
        send(port, "item.spawnGround('bronze_bar', 6.2, 2.2); "
                   "item.spawnGround('bronze_bar', 6.3, 2.3); return 'ok'")
        passed = check(passed, ground_stock(port, "bronze_bar") == target4,
                       "stock now meets the target, after the scan",
                       f"stock={ground_stock(port, 'bronze_bar')}")

        claimed = send(port,
            "local ai=require('scripts.unit_ai_craft'); "
            "local p=require('scripts.unit_ai_tunables').acolyte; "
            f"ai.craftExecute({uid8}, ai.__probe_s, p); "
            "return ai.__probe_s.craftJob and 'claimed' or 'not-claimed'"
            ).strip('"')
        passed = check(passed, claimed == "not-claimed",
                       "claim (craftExecute) refuses the now-stale candidate",
                       claimed)
        after = send_json(port, f"return craft.getBill({uid8}, {bill_u5})")
        passed = check(passed,
                       isinstance(after, dict) and after.get("claimant") is None,
                       "the bill itself was never actually claimed engine-side",
                       after)
        send(port, f"craft.cancelBill({bill_u5}); return 'ok'")

        # --- 7. Dead-claimant reconciliation (#1680) ---
        #
        # Nothing in the engine or the Lua AI used to reconcile a bill
        # against its claimant's liveness: every clearing path
        # (releaseBill / completeBillCycle) runs inside the claimant's
        # OWN tick, so a destroyed crafter left the bill claimed and
        # cbWorking = True forever and the grid kept billing the station
        # the recipe's full wattage.
        ai_off(port)

        # A real grid: one wire tile bridging the already-built furnace
        # at (6,2) to a solar panel at (8,2), so power.listNetworks()
        # reports a network the furnace's craft draw attaches to. The
        # panel's own generation is irrelevant here -- drainW is the
        # consumer sum, independent of whether the network can meet it.
        # A FRESH carrier for the panel: the acolyte the earlier phases
        # used has been loaded and unloaded repeatedly, and placeNode
        # refuses outright if the item never made it into the inventory.
        uid_pw = spawn_acolyte(port, 5, 3)
        send(port, f"unit.addItem({uid_pw}, 'solar_panel'); return 'ok'")
        carried = int(float(send(port,
            f"local n=0; for _,it in ipairs(unit.getInventory({uid_pw}) or {{}}) "
            f"do if it.defName=='solar_panel' then n=n+1 end end; return n")))
        passed = check(passed, carried == 1,
                       "fixture: the placer carries the solar panel",
                       f"carried={carried}")
        # placeNode returns (nodeId, buildingId) on success and
        # (nil, reason) on refusal -- key the check off the NODE id, not
        # the second value, which is the reason string on failure.
        panel_node = send(port,
            f"local nid, b = power.placeNode({uid_pw}, 'solar_panel', 8, 2); "
            f"return nid and ('ID:'..nid) or ('ERR:'..tostring(b))").strip('"')
        passed = check(passed, panel_node.startswith("ID:"),
                       "solar panel placed for the reconciliation phase",
                       panel_node)
        send(port, "require('scripts.wire').place(7, 2); return 'ok'")
        # wire.place queues a world command applied on the world thread's
        # own next iteration -- poll rather than reading the instant
        # after the send.
        wired = poll(port, 10,
                     lambda: len((first_network(port) or {}).get("nodeIds", []))
                             == 1,
                     interval=0.2)
        passed = check(passed, wired, "panel + wire settle into one network",
                       first_network(port))
        passed = check(passed, drain_of(port) == 0,
                       "drainW starts at 0 (no bill worked yet)",
                       drain_of(port))

        uid_dead = spawn_acolyte(port, 5, 2)
        bill_pw, msg = add_bill(port, bid_f, "bill_probe_powered", 3)
        passed = check(passed, bill_pw is not None,
                       "powered bill queued at the furnace", msg)
        claimed = send(port,
            f"return craft.claimBill({bill_pw}, {uid_dead}, 600)").strip('"')
        passed = check(passed, claimed == "true",
                       "the soon-to-die crafter claims it", claimed)
        send(port, f"craft.setBillWorking({uid_dead}, {bill_pw}, true); "
                   f"return 'ok'")
        send(port, f"craft.addBillProgress({uid_dead}, {bill_pw}, 0.4); "
                   f"return 'ok'")
        # PAUSED on purpose: while paused, claimAvailable refuses even a
        # dead-claimant takeover, so nothing but the #1680 sweep can
        # repair this bill. An unpaused one could be cleared by an
        # ordinary rival claim and pass vacuously.
        send(port, f"craft.setBillPaused({bill_pw}, true); return 'ok'")
        passed = check(passed, drain_of(port) == POWERED_DRAW_W,
                       f"drainW == {POWERED_DRAW_W}W while the (paused) "
                       f"claimant is working", drain_of(port))

        send(port, f"unit.destroy({uid_dead}); return 'ok'")
        gone = poll(port, 20, lambda: not unit_exists(port, uid_dead),
                    interval=0.2)
        passed = check(passed, gone,
                       "the claimant is actually gone from the unit registry")

        def reconciled():
            b = send_json(port, f"return craft.getBill({uid_pw}, {bill_pw})")
            return (isinstance(b, dict) and b.get("claimant") is None
                    and b.get("working") is False and drain_of(port) == 0)

        passed = check(passed, poll(port, 30, reconciled, interval=0.5),
                       "the orphaned bill is disowned and the station's "
                       "draw returns to 0",
                       send_json(port, f"return craft.getBill({uid_pw}, {bill_pw})"))

        # ... and STAYS there: nothing re-claims it, and no drain creeps
        # back over an observation window.
        drains = []
        for _ in range(6):
            time.sleep(0.5)
            drains.append(drain_of(port))
        passed = check(passed, all(d == 0 for d in drains),
                       "drain stays at 0 across the observation window",
                       drains)
        after = send_json(port, f"return craft.getBill({uid_pw}, {bill_pw})")
        passed = check(passed,
                       isinstance(after, dict) and after.get("claimant") is None
                       and after.get("working") is False,
                       "the bill is still unclaimed and not working", after)
        # Ownership only: the queued cycle survives for a later crafter,
        # and clearing the claim never granted one -- the bill is still
        # paused, and still refuses a fresh claim.
        passed = check(passed,
                       isinstance(after, dict) and after.get("paused") is True
                       and after.get("remaining") == 3
                       and abs(float(after.get("progress", -1)) - 0.4) < 1e-6,
                       "progress/remaining/pause survive the reconciliation",
                       after)
        refused = send(port,
            f"return craft.claimBill({bill_pw}, {uid_pw}, 600)").strip('"')
        passed = check(passed, refused == "false",
                       "the disowned PAUSED bill still refuses a fresh claim "
                       "(#796 unchanged)", refused)
        send(port, f"craft.cancelBill({bill_pw}); return 'ok'")

        print("\n" + ("ALL CRAFT BILL CHECKS PASSED" if passed
                      else "SOME FAILED"))
        return 0 if passed else 1
    finally:
        quit_engine(port, proc)


if __name__ == "__main__":
    sys.exit(main())
