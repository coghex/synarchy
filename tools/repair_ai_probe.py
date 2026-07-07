#!/usr/bin/env python3
"""Repair AI probe (#302) — the AI/designation layer on top of repair.repairAt
(#301) and unit.repairItem (#300).

Boots a headless engine on a flat arena, loads defs + recipes + buildings,
and drives the repair_job action in scripts/unit_ai.lua (which stays LIVE —
it IS the machinery under test, unlike movement_probe which neutralises it):

  1. own_inventory : an acolyte carries a BROKEN weapon (condition 0) and a
     merely-degraded armor piece. The AI must claim the higher-severity
     broken item first, repair it at a furnace (consuming lignite_chunk),
     THEN move on to the armor — proving the tiered severity ordering
     (repairSeverity: broken > quadratic ramp) actually drives claim order.
  2. equipped_ground : an EQUIPPED weapon (in the weapon slot, not
     inventory) has low sharpness; the whetstone it needs is a GROUND item
     near the acolyte. Exercises equipment.getLoadout scanning +
     fetchWantsFromGround for the consumable.
  3. mule_spare_gear : the degraded item AND its repair consumable both
     sit on the technomule, not the acting acolyte. Exercises the
     fetch_item phase (transferItemToUnit off the mule) + fetchWantsFromMule
     for the consumable + the post-repair return-to-mule handoff.
  4. dead_claimant_release : the first claimant is destroyed mid-fetch
     (before it reaches the mule); a second acolyte must pick the same
     item back up and finish the job — proving the repairClaims stale-claim
     self-heal (mirrors chopClaims/constructClaims).
  5. abort_returns_item : the item is fetched off the mule, then its
     station is destroyed mid-job — the abort must return the fetched
     item to the mule (abortRepairJob), not leak it into the worker's
     own inventory (regression for a review finding on the fetch_item ->
     mid-job-failure path).
  6. own_item_collision : the acolyte ALSO carries its own healthy
     axe_steel (acolyte.yaml's default loadout) while fetching a
     DIFFERENT, degraded axe_steel off the mule — proving the fetch and
     the return-to-mule both target the flagged instanceId, not just
     defName (regression for a review finding: a defName-only transfer
     could pop the worker's own item instead of the one actually fetched).
  7. role_weight : scripts/unit_roles.lua's weight() gives the "smith" role
     (#265, previously dormant) its first real ON_ROLE boost on repair_job,
     and now correctly damps a smith's OTHER routine work — a pure Lua
     check, no world/units needed.
  8. player_priority : the #303 UI hook — unitAi.setRepairPriority flags a
     LOWER-severity item (mildly degraded armor) ahead of a HIGHER-severity
     one (a broken weapon) on the same acolyte, proving scanHeldItems'
     priority-first comparison overrides its normal severity ordering (the
     inverse of phase 1). Also checks isRepairPriority's before/after state
     and that the flag self-clears once the prioritized item is actually
     repaired.
  9. priority_gating : #303 review — an item above BOTH thresholds can't be
     offered/shown as "priority" even if flagged at the backend, since the
     AI would never actually act on it. Pure Lua checks against synthetic
     item tables (no world/units needed, mirrors phase 7).

Test fixtures deliberately use condition/sharpness = 5 (not 20-40) for the
"degraded but not broken" cases: repair_job's utility (base 1.2 * severity)
must clear ambient wander's utility (up to ~0.8 at full stamina, scripts/
unit_ai.lua wanderUtility) for the AI to reliably prioritize it over idling
— severity=(1-5/50)^2=0.81 gives a comfortable margin (0.97 > 0.8).

Usage: python3 tools/repair_ai_probe.py [--port 9382] [--phase all]
"""
from __future__ import annotations

import argparse
import glob
import json
import socket
import subprocess
import sys
import time
from probelib import clear_find_water, quit_engine, boot, send

LOG = "/tmp/repair_ai_probe_engine.log"


def jget(port: int, lua: str, timeout: float = 10.0):
    raw = send(port, lua, timeout)
    try:
        return json.loads(raw)
    except json.JSONDecodeError:
        return raw.strip('"')


def bootstrap(port: int) -> None:
    """Load defs + the flat arena. unit_ai is auto-loaded at boot and IS
    the machinery under test, so it stays live (construction_probe's
    convention, NOT movement_probe's neutralise-the-AI one)."""
    loaders = [
        ("data/substances/*.yaml", "engine.loadSubstanceYaml"),
        ("data/items/*.yaml",      "engine.loadItemYaml"),
        ("data/equipment/*.yaml",  "engine.loadEquipmentYaml"),
        ("data/materials/*.yaml",  "engine.loadMaterialYaml"),
        ("data/units/*.yaml",      "engine.loadUnitYaml"),
        ("data/recipes/*.yaml",    "engine.loadRecipeYaml"),
        ("data/buildings/*.yaml",  "engine.loadBuildingYaml"),
    ]
    for pattern, fn in loaders:
        for path in sorted(glob.glob(pattern)):
            send(port, f"{fn}('{path}'); return 'ok'")
    send(port,
         "return require('scripts.movement_arena').buildCourse('flat').name")
    if not poll_until(port, 30, lambda: wid(port)):
        sys.exit("arena page never became the active world")
    # The flat arena PRE-builds exactly a 5x5 chunk block (chunks -2..2,
    # chunkSize=16 -> tiles -32..47); requesting anything wider falls
    # through to real chunk generation, which has no plate data on an
    # arena page and crashes the world thread. Stay inside that block.
    send(port, "return world.loadChunksInRegion(-2, -2, 2, 2)")
    send(port, "return world.waitForChunks(60)", timeout=65.0)


def wid(port: int) -> str | None:
    raw = send(port, "return world.getActiveWorldId()").strip().strip('"')
    return raw if raw and raw not in ("null", "nil") else None


def poll_until(port: int, seconds: float, fn):
    deadline = time.time() + seconds
    while time.time() < deadline:
        # Defensive: ANY unit_warning (not just repair's "No X available"
        # — e.g. the pre-existing stuck-walk watchdog's "Stuck — can't
        # reach destination") pauses the engine per this repo's
        # config/notifications.yaml. A human would notice and dismiss it;
        # a headless poll loop would otherwise hang for its entire
        # remaining budget with gameTime frozen. Unconditionally
        # unpausing each cycle is a no-op when already running.
        send(port, "engine.setPaused(false); return 'ok'")
        v = fn()
        if v:
            return v
        time.sleep(0.3)
    return None


CHECKS: list[tuple[str, bool]] = []


def check(label: str, ok: bool) -> None:
    CHECKS.append((label, bool(ok)))
    print(f"  [{'PASS' if ok else 'FAIL'}] {label}")


def spawn_acolyte(port: int, x: float, y: float) -> int:
    uid = send(port, f"return unit.spawn('acolyte', {x}, {y})")
    try:
        n = int(float(uid))
    except ValueError:
        sys.exit(f"unit.spawn failed: {uid!r}")
    # Shed the spawn-seeded tools so they don't collide with our
    # deliberately-placed test items / over-fill capacity (mirrors
    # construction_probe.spawn_acolyte). unit.spawn's default loadout is
    # queued to the unit thread and can still be settling in — a
    # removeItem issued before it lands is a silent no-op, leaving the
    # heavy default tools in place to push the acolyte over carrying
    # capacity later (a fetch then silently no-ops and the AI reports a
    # false "nothing available" instead of actually fetching). Retry
    # until none of them remain.
    TOOL_DEFS = {"pick_steel", "shovel_steel", "axe_steel", "rations"}

    def stripped():
        inv = jget(port, f"return unit.getInventory({n})")
        if inv is None:
            return False
        present = {it["defName"] for it in inv} & TOOL_DEFS
        for def_name in present:
            send(port, f"unit.removeItem({n}, '{def_name}'); return 'ok'")
        return not present
    if not poll_until(port, 20, stripped):
        sys.exit(f"unit {n} still carries default tools after stripping")
    # Retire the spawn-seeded find_water goal — the arena has no water, so
    # the goal never completes and its search utility can edge out a
    # repair job a few tiles away (same fix construction_probe applies).
    if not clear_find_water(port, n, seconds=20):
        sys.exit(f"unit {n} never got AI state")
    return n


def spawn_mule(port: int, x: float, y: float) -> int:
    uid = send(port, f"return unit.spawn('technomule', {x}, {y})")
    try:
        n = int(float(uid))
    except ValueError:
        sys.exit(f"technomule spawn failed: {uid!r}")
    # unit.spawn is queued to the unit thread — its default cargo (and
    # the unit itself, for unit.getInfo/addItem purposes) isn't reliably
    # queryable until that lands (mirrors spawn_acolyte's settle wait).
    if not poll_until(port, 20, lambda: send(
            port, f"return unit.getInfo({n}) and 'yes' or 'no'"
            ).strip('"') == "yes"):
        sys.exit(f"technomule {n} never became queryable")
    return n


def destroy_unit(port: int, uid: int) -> None:
    send(port, f"unit.destroy({uid}); return 'ok'")


def spawn_station(port: int, uid: int, def_name: str, gx: int, gy: int,
                   materials: dict[str, int]) -> int:
    """Spawn def_name at (gx, gy), deliver build materials from uid
    through the real machinery, and build it fully. Returns the built
    building id."""
    raw = send(port, f"return building.spawn('{def_name}', {gx}, {gy})")
    try:
        bid = int(float(raw))
    except ValueError:
        sys.exit(f"building.spawn('{def_name}') failed: {raw}")
    if not poll_until(port, 20, lambda: send(
            port, f"return building.getInfo({bid}) and 'yes' or 'no'"
            ).strip('"') == "yes"):
        sys.exit(f"{def_name} instance never appeared")
    for item_name, count in materials.items():
        send(port,
             f"for i=1,{count} do unit.addItem({uid},'{item_name}'); "
             f"unit.transferItemToBuilding({uid},{bid},'{item_name}') end; "
             f"return 'ok'")
    sat = send(port, f"return building.areMaterialsSatisfied({bid}) "
                     f"and 'yes' or 'no'").strip('"')
    if sat != "yes":
        sys.exit(f"{def_name} materials not satisfied after delivery")
    send(port, f"building.addBuildProgress({bid}, 100000); return 'ok'")
    act = send(port, f"return building.getActivity({bid})").strip('"')
    if act != "built":
        sys.exit(f"{def_name} never reached built (got {act})")
    return bid


def build_station(port: int, def_name: str, gx: int, gy: int,
                   materials: dict[str, int]) -> int:
    """spawn_station needs a real unit to draw materials from — use a
    throwaway builder so the test acolyte's inventory stays clean."""
    builder = spawn_acolyte(port, gx - 1.5, gy - 1.5)
    bid = spawn_station(port, builder, def_name, gx, gy, materials)
    destroy_unit(port, builder)
    return bid


def force_item_state(port: int, uid: int, def_name: str,
                      cond: float, sharp: float) -> int:
    """Add def_name to uid's inventory and force its condition/sharpness to
    exact values via the double-repairItem trick (delta is ADDITIVE and
    clamped, not a set — floor both axes at 0 first, then apply a precise
    positive delta from that known floor). Returns the new instanceId."""
    send(port, f"unit.addItem({uid}, '{def_name}'); return 'ok'")
    # unit.spawn's default loadout seeding can still be settling on a
    # just-created unit (e.g. spawn_mule has no internal wait, unlike
    # spawn_acolyte's goal-retirement poll) — retry until addItem's
    # effect is actually queryable instead of assuming it landed already.
    ids = poll_until(port, 20, lambda: jget(port,
        f"local out={{}}; for _,it in ipairs(unit.getInventory({uid}) or {{}}) do "
        f"if it.defName=='{def_name}' then out[#out+1]=it.instanceId end end; "
        f"return #out > 0 and out or false"))
    if not ids:
        sys.exit(f"unit {uid} never received {def_name}")
    iid = int(ids[-1])
    send(port, f"unit.repairItem({uid}, {iid}, -1000, -1000); return 'ok'")
    send(port, f"unit.repairItem({uid}, {iid}, {cond}, {sharp}); return 'ok'")
    return iid


def item_state(port: int, uid: int, iid: int):
    """{cond, sharp, loc} for instance iid wherever it sits on uid right
    now (inventory, equipped loadout, or worn accessory) — or None."""
    return jget(port,
        f"for _,it in ipairs(unit.getInventory({uid}) or {{}}) do "
        f"  if it.instanceId=={iid} then "
        f"    return {{cond=it.condition,sharp=it.sharpness,loc='inv'}} end end; "
        f"for slot,it in pairs(equipment.getLoadout({uid}) or {{}}) do "
        f"  if it.instanceId=={iid} then "
        f"    return {{cond=it.condition,sharp=it.sharpness,loc='equip'}} end end; "
        f"for _,it in ipairs(equipment.getAccessories({uid}) or {{}}) do "
        f"  if it.instanceId=={iid} then "
        f"    return {{cond=it.condition,sharp=it.sharpness,loc='acc'}} end end; "
        f"return nil")


def find_state_anywhere(port: int, uids: list[int], iid: int):
    """item_state, tried across several candidate holders (the item may
    have moved between them — own gear vs. mule)."""
    for uid in uids:
        st = item_state(port, uid, iid)
        if st is not None:
            return st
    return None


def count_item(port: int, uid: int, name: str) -> int:
    return int(float(send(port,
        f"local c=0; for _,it in ipairs(unit.getInventory({uid}) or {{}}) do "
        f"if it.defName=='{name}' then c=c+1 end end; return c")))


def count_ground(port: int, name: str) -> int:
    return int(float(send(port,
        f"local c=0; for _,g in ipairs(item.listGround() or {{}}) do "
        f"if g.defName=='{name}' then c=c+1 end end; return c")))


def has_repair_job(port: int, uid: int) -> bool:
    return send(port,
        f"local ai=require('scripts.unit_ai'); local s=ai.getState({uid}); "
        f"return (s ~= nil and s.repairJob ~= nil)") == "true"


# --- phases -------------------------------------------------------------


def phase_own_inventory(port: int) -> None:
    print("\n[phase 1] OWN gear: broken weapon claimed before "
          "merely-degraded armor")
    build_station(port, "furnace", 3, 2, {"granite_chunk": 6, "steel_bar": 2})
    uid = spawn_acolyte(port, 4.5, 3.5)
    # lignite_chunk is GROUND, fetched during the locked fetch_consumable
    # phase (mirrors phase 2's ground whetstone) rather than pre-loaded
    # into inventory: lignite_chunk is category "Materials", and an idle
    # Materials item sitting in inventory BEFORE a job claims/locks it is
    # fair game for the unrelated store_materials action (base utility
    # 3.0, scales with carry fill) to auto-deposit into the very furnace
    # this test just built (storage_capacity 100) — a real but narrow
    # interaction between two independently-correct actions, not a
    # repair_job bug (once claimed, repair_job's 6.0 lock always beats
    # store_materials' 3.0 ceiling). Ground-sourcing sidesteps it, same
    # as real gameplay's typical "fetch specifically to repair" flow.
    send(port, "item.spawnGround('lignite_chunk', 6.5, 2.5); "
               "item.spawnGround('lignite_chunk', 6.5, 3.5); return 'ok'")
    skill_before = jget(port, f"return unit.getSkill({uid}, 'smithing')")
    axe = force_item_state(port, uid, "axe_steel", cond=0.0, sharp=100.0)
    gam = force_item_state(port, uid, "wool_gambeson", cond=5.0, sharp=100.0)

    axe_done = poll_until(port, 120,
        lambda: (item_state(port, uid, axe) or {}).get("cond") == 100)
    check("broken weapon (condition 0) repaired first", axe_done is not None)

    gam_mid = item_state(port, uid, gam)
    check("armor untouched while the higher-severity weapon job was active",
          gam_mid is not None and gam_mid["cond"] == 5)

    gam_done = poll_until(port, 120,
        lambda: (item_state(port, uid, gam) or {}).get("cond") == 100)
    check("lower-severity armor repaired afterward", gam_done is not None)

    check("both ground lignite_chunk fetched and consumed",
          count_ground(port, "lignite_chunk") == 0
          and count_item(port, uid, "lignite_chunk") == 0)
    skill_after = jget(port, f"return unit.getSkill({uid}, 'smithing')")
    # acolyte.yaml rolls a baseline smithing skill (base 20, range 15) —
    # it's never nil, so "work-XP granted" means the two successful
    # repairs measurably RAISED it, not that it went from nil to a value.
    check("smithing work-XP granted (#265 smith role's first work action)",
          skill_before is not None and skill_after is not None
          and skill_after > skill_before)
    destroy_unit(port, uid)


def phase_equipped_ground(port: int) -> None:
    print("\n[phase 2] EQUIPPED weapon (weapon slot) + GROUND consumable "
          "(fetchWantsFromGround)")
    build_station(port, "workbench", 9, 2,
                  {"wood_log": 4, "steel_hardware": 4, "steel_bar": 2})
    uid = spawn_acolyte(port, 10.5, 3.5)
    # Ground whetstone BEFORE the weapon is degraded (see phase 1's note
    # on the claim-vs-setup race and its pause-on-warning consequence).
    send(port, "item.spawnGround('whetstone', 13.5, 3.5); return 'ok'")
    ground_before = count_ground(port, "whetstone")
    check("ground whetstone present before repair", ground_before >= 1)

    axe = force_item_state(port, uid, "axe_steel", cond=100.0, sharp=5.0)
    equipped = send(port,
        f"return equipment.equip({uid}, 'right_hand', 'axe_steel', {axe})") == "true"
    check("weapon equipped into right_hand slot", equipped)
    before = item_state(port, uid, axe)
    check("axe sits in equipment loadout, not inventory",
          before is not None and before["loc"] == "equip")

    done = poll_until(port, 120,
        lambda: (item_state(port, uid, axe) or {}).get("sharp") == 100)
    check("equipped weapon sharpened to 100 via ground-sourced whetstone",
          done is not None)
    check("ground whetstone consumed", count_ground(port, "whetstone")
          == ground_before - 1)
    after = item_state(port, uid, axe)
    check("axe still equipped (not left loose in inventory) after repair",
          after is not None and after["loc"] == "equip")
    destroy_unit(port, uid)


def phase_mule_spare_gear(port: int) -> None:
    print("\n[phase 3] MULE-held spare gear: fetch_item + "
          "fetchWantsFromMule + return-to-mule")
    build_station(port, "furnace", 15, 2, {"granite_chunk": 6, "steel_bar": 2})
    mule = spawn_mule(port, 17.5, 3.5)
    axe = force_item_state(port, mule, "axe_steel", cond=5.0, sharp=100.0)
    send(port, f"unit.addItem({mule}, 'lignite_chunk'); return 'ok'")
    uid = spawn_acolyte(port, 16.5, 3.5)
    # Fetching BOTH the mule-held item and its repair consumable adds
    # their combined weight on top of the acolyte's own equipped gear —
    # a rolled-low carrying_capacity can genuinely not fit axe_steel
    # (2kg) + lignite_chunk (5kg), which repairUtility now correctly
    # refuses to claim (see #302's capacity-feasibility gate). Boost
    # strength so this scenario's "should succeed" path isn't at the
    # mercy of the capacity roll.
    send(port, f"unit.setStat({uid}, 'strength', 3.0); return 'ok'")

    claimed = poll_until(port, 30, lambda: has_repair_job(port, uid))
    check("acolyte claimed the mule-held item", claimed is not None)

    done = poll_until(port, 180,
        lambda: (find_state_anywhere(port, [uid, mule], axe) or {}).get("cond") == 100)
    check("mule-sourced weapon repaired to full condition", done is not None)

    returned = poll_until(port, 30, lambda: count_item(port, mule, "axe_steel") == 1)
    check("repaired weapon returned to the mule", returned is not None)
    check("acolyte no longer holds the (returned) weapon",
          count_item(port, uid, "axe_steel") == 0)
    lignite_gone = poll_until(port, 10, lambda: (
        count_item(port, mule, "lignite_chunk") == 0
        and count_item(port, uid, "lignite_chunk") == 0))
    check("lignite_chunk fetched from the mule and consumed",
          lignite_gone is not None)
    destroy_unit(port, uid)
    destroy_unit(port, mule)


def phase_dead_claimant_release(port: int) -> None:
    print("\n[phase 4] dead claimant releases a mule-held claim; a second "
          "acolyte finishes it")
    # Own row (y=-6/-4.5), away from phases 1-3, so nothing collides. The
    # arena's pre-built block only spans chunks -2..2 (tiles -32..47), so
    # "far" here means the opposite edge of that block, not truly distant.
    build_station(port, "furnace", 20, -6, {"granite_chunk": 6, "steel_bar": 2})
    mule = spawn_mule(port, 22.5, -4.5)
    axe = force_item_state(port, mule, "axe_steel", cond=5.0, sharp=100.0)
    send(port, f"unit.addItem({mule}, 'lignite_chunk'); return 'ok'")

    # Spawned far from the mule so it's still walking (fetch_item phase,
    # not yet arrived) when destroyed — the item never actually leaves
    # the mule.
    a = spawn_acolyte(port, -25.5, -4.5)
    claimed = poll_until(port, 30, lambda: has_repair_job(port, a))
    check("first acolyte claimed the mule-held item", claimed is not None)
    destroy_unit(port, a)
    check("item still on the mule after the claimant died",
          count_item(port, mule, "axe_steel") == 1)

    b = spawn_acolyte(port, 21.5, -4.5)
    send(port, f"unit.setStat({b}, 'strength', 3.0); return 'ok'")  # see phase 3's note
    done = poll_until(port, 180,
        lambda: (find_state_anywhere(port, [b, mule], axe) or {}).get("cond") == 100)
    check("second acolyte picked up the released claim and finished the repair",
          done is not None)
    destroy_unit(port, b)
    destroy_unit(port, mule)


def phase_abort_returns_item(port: int) -> None:
    print("\n[phase 5] a job aborted AFTER fetch_item returns the fetched "
          "item to the mule (regression: it used to leak into the "
          "worker's own inventory)")
    bid = build_station(port, "furnace", 35, 2, {"granite_chunk": 6, "steel_bar": 2})
    mule = spawn_mule(port, 37.5, 3.5)
    axe = force_item_state(port, mule, "axe_steel", cond=5.0, sharp=100.0)
    send(port, f"unit.addItem({mule}, 'lignite_chunk'); return 'ok'")
    uid = spawn_acolyte(port, 36.5, 3.5)
    send(port, f"unit.setStat({uid}, 'strength', 3.0); return 'ok'")  # see phase 3's note

    claimed = poll_until(port, 30, lambda: has_repair_job(port, uid))
    check("acolyte claimed the mule-held item", claimed is not None)

    fetched = poll_until(port, 30, lambda: count_item(port, uid, "axe_steel") == 1)
    check("item fetched off the mule into the acolyte's own inventory",
          fetched is not None)

    # Wait for the "walking" phase to actually CACHE job.bid to this
    # specific station before destroying it — earlier phases' furnaces
    # (#1 etc.) persist in this shared arena, so destroying ours before
    # job.bid is cached would just send the acolyte on a long walk to one
    # of those instead of aborting (job.bid pins the abort to THIS
    # building regardless of what else exists elsewhere).
    bid_cached = poll_until(port, 30, lambda: jget(port,
        f"local ai=require('scripts.unit_ai'); local st=ai.getState({uid}); "
        f"return st and st.repairJob and st.repairJob.bid") == bid)
    check("acolyte's job cached this station before it's destroyed",
          bid_cached is not None)

    # Destroy that cached station WHILE the item is sitting in the
    # acolyte's inventory (mid-job, past fetch_item) — forces the
    # "walking" phase's missing-building abort. Since the station is now
    # genuinely gone, repairUtility's own reachability gate also stops the
    # acolyte from re-claiming this axe once the job releases (there's no
    # other repair_condition station within its scan of the mule/axe).
    send(port, f"building.destroy({bid}); return 'ok'")

    returned = poll_until(port, 30, lambda: count_item(port, mule, "axe_steel") == 1)
    check("aborted job returns the fetched item to the mule (not leaked)",
          returned is not None)
    # Destroy the acolyte the INSTANT the return is observed: a farther
    # repair_condition station (phase 1's furnace) still exists elsewhere
    # in this shared arena, so the still-degraded axe would otherwise be
    # a valid (if distant) candidate again on the very next thought tick —
    # this check only cares that THIS abort didn't leak the item, not
    # whether a later, unrelated claim eventually re-fetches it.
    destroy_unit(port, uid)
    check("acolyte no longer holds the item after the abort",
          count_item(port, mule, "axe_steel") == 1)
    destroy_unit(port, mule)


def phase_own_item_collision(port: int) -> None:
    print("\n[phase 6] the worker's OWN same-defName item is never confused "
          "with the flagged mule instance (instanceId-targeted transfer)")
    build_station(port, "furnace", 42, 2, {"granite_chunk": 6, "steel_bar": 2})
    mule = spawn_mule(port, 44.5, 3.5)
    mule_axe = force_item_state(port, mule, "axe_steel", cond=5.0, sharp=100.0)
    send(port, f"unit.addItem({mule}, 'lignite_chunk'); return 'ok'")

    uid = spawn_acolyte(port, 43.5, 3.5)
    send(port, f"unit.setStat({uid}, 'strength', 3.0); return 'ok'")  # see phase 3's note
    # spawn_acolyte strips the default axe_steel; add a fresh, HEALTHY one
    # back — the exact collision a real acolyte hits (acolyte.yaml starts
    # with its own axe_steel). A defName-only transfer could pop THIS one
    # instead of the flagged (degraded) mule instance.
    own_axe = force_item_state(port, uid, "axe_steel", cond=100.0, sharp=100.0)

    claimed = poll_until(port, 30, lambda: has_repair_job(port, uid))
    check("acolyte claimed the mule-held (degraded) item, not its own",
          claimed is not None)

    done = poll_until(port, 180,
        lambda: (find_state_anywhere(port, [uid, mule], mule_axe) or {}).get("cond") == 100)
    check("the FLAGGED mule instance reaches full condition", done is not None)

    poll_until(port, 30, lambda: count_item(port, mule, "axe_steel") == 1)
    own_state = item_state(port, uid, own_axe)
    check("acolyte's OWN axe is untouched (same instance, still healthy)",
          own_state is not None and own_state["cond"] == 100)
    mule_state = item_state(port, mule, mule_axe)
    check("the repaired FLAGGED instance is the one back on the mule "
          "(not the acolyte's own)",
          mule_state is not None and mule_state["cond"] == 100)
    check("acolyte carries exactly its own one axe_steel afterward",
          count_item(port, uid, "axe_steel") == 1)
    destroy_unit(port, uid)
    destroy_unit(port, mule)


def phase_role_weight(port: int) -> None:
    print("\n[phase 7] role_weight: smith's (#265) first real ON_ROLE "
          "effect on repair_job")
    family = jget(port,
        "return require('scripts.unit_roles').ACTION_FAMILY.repair_job")
    check("repair_job mapped to the 'craft' family", family == "craft")

    def weight(role: str, action: str) -> float:
        return jget(port,
            f"local m = require('scripts.unit_roles'); "
            f"return m.weight({{role='{role}'}}, '{action}')")

    check("smith gets ON_ROLE (1.4) on repair_job", weight("smith", "repair_job") == 1.4)
    check("miner gets OFF_ROLE (0.7) on repair_job", weight("miner", "repair_job") == 0.7)
    check("laborer stays neutral (1.0) on repair_job", weight("laborer", "repair_job") == 1.0)
    # Now that "craft" has an action, a smith's OTHER routine work is
    # correctly damped too (M.weight's familyHasActions gate) — this was
    # a no-op (1.0) before #302 gave the smith family any actions at all.
    check("smith is now OFF_ROLE (0.7) on dig_designation (craft family "
          "now has actions)", weight("smith", "dig_designation") == 0.7)
    check("miner stays ON_ROLE (1.4) on its own dig_designation",
          weight("miner", "dig_designation") == 1.4)


def phase_player_priority(port: int) -> None:
    print("\n[phase 8] player-set repair priority (#303 UI) beats a "
          "higher-severity, unflagged candidate")
    build_station(port, "furnace", 3, -12, {"granite_chunk": 6, "steel_bar": 2})
    uid = spawn_acolyte(port, 4.5, -11.5)
    send(port, "item.spawnGround('lignite_chunk', 6.5, -12.5); "
               "item.spawnGround('lignite_chunk', 6.5, -11.5); return 'ok'")
    # Broken weapon (severity band, the higher of the two — see
    # repairSeverity) vs. a mildly degraded armor piece (quadratic-ramp
    # severity, lower) that the player flags as priority. Phase 1 proves
    # the unflagged ordering picks the broken weapon first; this phase
    # proves flagging the armor inverts that.
    axe = force_item_state(port, uid, "axe_steel", cond=0.0, sharp=100.0)
    gam = force_item_state(port, uid, "wool_gambeson", cond=5.0, sharp=100.0)

    check("item starts unflagged", send(port,
        f"local ai=require('scripts.unit_ai'); "
        f"return ai.isRepairPriority({gam})") == "false")

    flagged = send(port,
        f"local ai=require('scripts.unit_ai'); "
        f"ai.setRepairPriority({gam}, true); "
        f"return ai.isRepairPriority({gam})") == "true"
    check("unitAi.setRepairPriority flags the armor instance", flagged)

    gam_done = poll_until(port, 120,
        lambda: (item_state(port, uid, gam) or {}).get("cond") == 100)
    check("player-prioritized armor (lower severity) repaired FIRST",
          gam_done is not None)

    axe_mid = item_state(port, uid, axe)
    check("higher-severity weapon untouched while the priority job ran",
          axe_mid is not None and axe_mid["cond"] == 0)

    check("priority flag self-clears once the item is actually repaired",
          send(port, f"local ai=require('scripts.unit_ai'); "
                     f"return ai.isRepairPriority({gam})") == "false")

    axe_done = poll_until(port, 120,
        lambda: (item_state(port, uid, axe) or {}).get("cond") == 100)
    check("un-prioritized weapon repaired afterward", axe_done is not None)
    destroy_unit(port, uid)


def phase_priority_gating(port: int) -> None:
    print("\n[phase 9] priority menu/status is gated on the item actually "
          "needing repair (#303 review: an above-threshold item flagged "
          "'priority' would otherwise sit forever with no effect)")
    # Pure Lua checks against synthetic item tables — no world/units
    # needed (mirrors phase 7's role_weight). repairStatus reads only
    # the fields it's handed (instanceId/condition/sharpness), so a
    # fabricated table exercises the same code path a real item would.
    healthy = "{instanceId=999901, condition=90, sharpness=100}"
    degraded = "{instanceId=999902, condition=5, sharpness=100}"

    check("itemNeedsRepair is false for a healthy item (90% > threshold)",
          jget(port, f"local ai=require('scripts.unit_ai'); "
                     f"return ai.itemNeedsRepair({healthy})") is False)
    check("itemNeedsRepair is true for a degraded item (5% < threshold)",
          jget(port, f"local ai=require('scripts.unit_ai'); "
                     f"return ai.itemNeedsRepair({degraded})") is True)

    # Flag the HEALTHY instance directly at the backend (simulating any
    # path that could set the flag without going through the gated
    # menu) and confirm the UI layer refuses to offer/show it anyway.
    send(port, "local ai=require('scripts.unit_ai'); "
               "ai.setRepairPriority(999901, true); return 'ok'")
    check("menuItem offers nothing for a flagged-but-healthy item",
          jget(port, f"local rs=require('scripts.ui.repair_status'); "
                     f"return rs.menuItem({healthy}) ~= nil") is False)
    check("suffix shows nothing for a flagged-but-healthy item",
          jget(port, f"local rs=require('scripts.ui.repair_status'); "
                     f"return rs.suffix({healthy})") == "")
    check("hintLine shows nothing for a flagged-but-healthy item",
          jget(port, f"local rs=require('scripts.ui.repair_status'); "
                     f"return rs.hintLine({healthy})") is None)

    # The degraded instance, still unflagged, DOES get offered.
    check("menuItem offers 'Prioritize Repair' for a degraded item",
          jget(port, f"local rs=require('scripts.ui.repair_status'); "
                     f"local m = rs.menuItem({degraded}); "
                     f"return m ~= nil and m.label") == "Prioritize Repair")


PHASES = {
    "own_inventory": phase_own_inventory,
    "equipped_ground": phase_equipped_ground,
    "mule_spare_gear": phase_mule_spare_gear,
    "dead_claimant_release": phase_dead_claimant_release,
    "abort_returns_item": phase_abort_returns_item,
    "own_item_collision": phase_own_item_collision,
    "role_weight": phase_role_weight,
    "player_priority": phase_player_priority,
    "priority_gating": phase_priority_gating,
}


def main() -> int:
    ap = argparse.ArgumentParser(description=__doc__,
        formatter_class=argparse.RawDescriptionHelpFormatter)
    ap.add_argument("--port", type=int, default=9382)
    ap.add_argument("--phase", default="all", choices=["all"] + list(PHASES))
    args = ap.parse_args()

    proc = boot(args.port, log=LOG)
    try:
        bootstrap(args.port)
        if not wid(args.port):
            print("FAIL: no active world after arena build", file=sys.stderr)
            return 2
        todo = PHASES.values() if args.phase == "all" else [PHASES[args.phase]]
        for phase in todo:
            # Defensive: a "No <consumable> available" failure emits a
            # unit_warning event, and this repo's config/notifications.yaml
            # has pause:true for that category — an unexpected failure in
            # one phase would otherwise freeze gameTime for every phase
            # after it. Each phase starts from a known unpaused state.
            send(args.port, "engine.setPaused(false); return 'ok'")
            phase(args.port)
    finally:
        quit_engine(args.port, proc)
        try:
            proc.wait(timeout=10)
        except subprocess.TimeoutExpired:
            proc.kill()

    failed = [label for label, ok in CHECKS if not ok]
    print(f"\n{len(CHECKS) - len(failed)}/{len(CHECKS)} checks passed"
          + (f"; FAILED: {failed}" if failed else ""))
    return 1 if failed else 0


if __name__ == "__main__":
    sys.exit(main())
