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
     repaired. The ordering is judged from ONE observed timeline
     (observe_repair_ordering, #1767): completing the flagged job is what
     frees the AI onto the other item, so "the other item is still
     untouched" -- read after the fact -- is a property CORRECT follow-on
     work destroys.
  9. priority_gating : #303 review — an item above BOTH thresholds can't be
     offered/shown as "priority" even if flagged at the backend, since the
     AI would never actually act on it. Pure Lua checks against synthetic
     item tables (no world/units needed, mirrors phase 7).
 10. ground_target : #1737 — the DEGRADED TARGET ITSELF lies on the ground
     (phase 2's only ground item is the CONSUMABLE, for an equipped
     target). Exercises the ladder's middle rung end to end against a real
     world: the row's new instanceId/sharpness/kind, the claim carrying
     fromGround, the instance-preserving pickup, the repair, and the
     return to the ground when the job ends. Supporting evidence only —
     the primary gate is the deterministic `repair ground target` hspec
     describe, since repair_ai is manual-only/flaky.

Test fixtures deliberately use condition/sharpness = 5 (not 20-40) for the
"degraded but not broken" cases: repair_job's utility (base 1.2 * severity)
must clear ambient wander's utility (up to ~0.8 at full stamina, scripts/
unit_ai_needs.lua wanderUtility) for the AI to reliably prioritize it over idling
— severity=(1-5/50)^2=0.81 gives a comfortable margin (0.97 > 0.8).

Usage: python3 tools/repair_ai_probe.py [--port 9382] [--phase all]
"""
from __future__ import annotations

import argparse
import glob
import socket
import subprocess
import sys
import time
from probelib import clear_find_water, quit_engine, boot, send, send_json

LOG = "/tmp/repair_ai_probe_engine.log"


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
        # config/notifications.local.yaml. A human would notice and dismiss it;
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
        inv = send_json(port, f"return unit.getInventory({n})")
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


def add_item_instance(port: int, uid: int, def_name: str) -> int:
    """Add def_name to uid's inventory and return the new instanceId,
    leaving its condition/sharpness alone (a fresh item is HEALTHY, so it
    is not a repair candidate yet). Extracted from force_item_state so a
    phase can open its own candidate window deliberately."""
    send(port, f"unit.addItem({uid}, '{def_name}'); return 'ok'")
    # unit.spawn's default loadout seeding can still be settling on a
    # just-created unit (e.g. spawn_mule has no internal wait, unlike
    # spawn_acolyte's goal-retirement poll) — retry until addItem's
    # effect is actually queryable instead of assuming it landed already.
    ids = poll_until(port, 20, lambda: send_json(port,
        f"local out={{}}; for _,it in ipairs(unit.getInventory({uid}) or {{}}) do "
        f"if it.defName=='{def_name}' then out[#out+1]=it.instanceId end end; "
        f"return #out > 0 and out or false"))
    if not ids:
        sys.exit(f"unit {uid} never received {def_name}")
    return int(ids[-1])


def _force_state_lua(uid: int, iid: int, cond: float, sharp: float) -> str:
    """The double-repairItem trick for ONE instance: the delta is ADDITIVE
    and clamped, not a set, so floor both axes at 0 first and then apply a
    precise positive delta from that known floor."""
    return (f"unit.repairItem({uid}, {iid}, -1000, -1000); "
            f"unit.repairItem({uid}, {iid}, {cond}, {sharp}); ")


def force_item_states(port: int, uid: int, states) -> None:
    """Force several already-held instances to exact condition/sharpness in
    ONE console round trip, so they all become repair candidates at the
    same instant instead of several round trips (and several live AI ticks)
    apart. `states` is an iterable of (instanceId, cond, sharp)."""
    lua = "".join(_force_state_lua(uid, iid, cond, sharp)
                  for iid, cond, sharp in states)
    send(port, lua + "return 'ok'")


def force_item_state(port: int, uid: int, def_name: str,
                      cond: float, sharp: float) -> int:
    """Add def_name to uid's inventory and force its condition/sharpness to
    exact values. Returns the new instanceId."""
    iid = add_item_instance(port, uid, def_name)
    send(port, f"unit.repairItem({uid}, {iid}, -1000, -1000); return 'ok'")
    send(port, f"unit.repairItem({uid}, {iid}, {cond}, {sharp}); return 'ok'")
    return iid


def item_state(port: int, uid: int, iid: int):
    """{cond, sharp, loc} for instance iid wherever it sits on uid right
    now (inventory, equipped loadout, or worn accessory) — or None."""
    return send_json(port,
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


def repair_job_of(port: int, uid: int):
    """{instanceId, fromGround, onMule, phase} for uid's live repair job,
    or None. #1737's fromGround is the durable provenance that decides
    whether the target owes a drop on the ground or a hand-back to a
    mule, so a phase asserting the ground rung has to read it rather
    than infer it from where the item happens to be."""
    return send_json(port,
        f"local ai=require('scripts.unit_ai'); local s=ai.getState({uid}); "
        f"local j = s and s.repairJob; if not j then return nil end; "
        f"return {{instanceId=j.instanceId, fromGround=(j.fromGround==true), "
        f"        onMule=(j.onMule==true), phase=tostring(s.repairPhase)}}")


def repair_ordering_sample(port: int, uid: int, first: int, second: int):
    """Both repair candidates' claims AND conditions in ONE console round
    trip, so an ordering verdict rests on a single instant rather than on
    two reads separated by live AI work.

    ``claim`` is unitAi.getRepairClaimant's uid (-1 for unclaimed),
    ``job`` the instanceId of the unit's live repairJob (-1 for none) and
    ``cond`` the instance's condition wherever it currently sits (-1 if it
    is not on this unit at all) -- the same inventory/loadout/accessory
    walk item_state does, inlined so the whole snapshot is one request.
    ``action`` is what the AI is actually doing, so a run in which it never
    claims EITHER candidate (the #724 timing flake this probe is
    manual-only for) reads as such instead of looking like an ordering
    defect.
    """
    return send_json(port,
        f"local ai=require('scripts.unit_ai'); local s=ai.getState({uid}); "
        f"local j = s and s.repairJob; "
        f"local function cond(iid) "
        f"  for _,it in ipairs(unit.getInventory({uid}) or {{}}) do "
        f"    if it.instanceId==iid then return it.condition end end; "
        f"  for _,it in pairs(equipment.getLoadout({uid}) or {{}}) do "
        f"    if it.instanceId==iid then return it.condition end end; "
        f"  for _,it in ipairs(equipment.getAccessories({uid}) or {{}}) do "
        f"    if it.instanceId==iid then return it.condition end end; "
        f"  return -1 end; "
        f"return {{job=(j and j.instanceId or -1), "
        f"  action=tostring(s and s.currentAction), "
        f"  phase=tostring(s and s.repairPhase), "
        f"  firstClaim=(ai.getRepairClaimant({first}) or -1), "
        f"  secondClaim=(ai.getRepairClaimant({second}) or -1), "
        f"  firstCond=cond({first}), secondCond=cond({second})}}")


def _ordering_side(sample, iid: int, claim_key: str, cond_key: str,
                   seen_degraded: bool):
    """(engaged?, restored?, description) for one candidate in one snapshot.

    Engagement is POSITIVE evidence that the AI has acted on THIS
    instance: it holds the repair claim, it is the live repairJob's
    target, or it has been RESTORED — seen below full condition earlier in
    this run and at full condition now. Nothing here is inferred from the
    other candidate's state, and a candidate that has simply never been
    degraded is not "restored": full condition only counts as evidence
    once a repair could have produced it.
    """
    claim = int(float(sample.get(claim_key, -1)))
    cond = float(sample.get(cond_key, -1))
    job = int(float(sample.get("job", -1)))
    restored = seen_degraded and cond >= 100
    engaged = claim >= 0 or job == iid or restored
    return engaged, restored, (f"claimed_by={'-' if claim < 0 else claim}, "
                               f"is_job={'yes' if job == iid else 'no'}, "
                               f"cond={cond:g}")


def observe_repair_ordering(port: int, uid: int, first: int, second: int,
                            first_label: str, second_label: str,
                            seconds: float, open_window=None):
    """Classify which of two repair candidates the AI worked on FIRST by
    watching ONE timeline, and keep watching until `first` is restored.

    `open_window` is called ONCE, after the baseline snapshot and before
    the first polling cycle, and is what turns the two instances into
    repair candidates. Sampling therefore starts strictly before the AI
    can have engaged either of them: a console round trip is ~0.3s and the
    AI claims a job within that, so a caller that made the candidates
    first and only then began observing would already have missed the
    claim.

    Two independent reads cannot decide this ordering. Completing the
    flagged job is precisely what releases the AI onto the other item — on
    a successful repair.repairAt, scripts/unit_ai_repair.lua clears the
    priority flag and frees the job in the same tick — and one successful
    repair visit restores an item atomically to full condition
    (src/Engine/Scripting/Lua/API/Repair.hs). So "the other item is still
    untouched", sampled a poll interval later, is a property that CORRECT
    follow-on work destroys; a run whose ordering was right fails it.

    The verdict is drawn from the FIRST ordering event this loop sees — a
    claim, a live repairJob target, or a restoration — and it is always
    positive evidence about one instance, never the absence of evidence
    about the other:

      "first"        -- `first` was engaged before `second`: the ordering
                        this phase requires.
      "second"       -- `second` was engaged first: the inversion this
                        phase exists to reject.
      "inconclusive" -- the first ordering event was missed (the AI was
                        already engaged when the window opened, both
                        candidates first appear engaged in the same
                        snapshot, or nothing was ever observed). Never
                        read as either ordering.

    Returns {verdict, reason, events, first_repaired, second_repaired}.
    """
    events: list[str] = []
    verdict = None
    reason = ""
    started = time.time()
    degraded = {"first": False, "second": False}
    last_key = [None]

    def record(sample):
        for side, cond_key in (("first", "firstCond"), ("second", "secondCond")):
            cond = float(sample.get(cond_key, -1))
            if 0 <= cond < 100:
                degraded[side] = True
        fe, fr, fd = _ordering_side(sample, first, "firstClaim", "firstCond",
                                    degraded["first"])
        se, sr, sd = _ordering_side(sample, second, "secondClaim", "secondCond",
                                    degraded["second"])
        # Dedupe on the ORDERING-relevant state only, so the log is one
        # line per transition; the AI's action/phase rides along as
        # context without turning every wander step into a new entry.
        key = f"{first_label}({fd})  {second_label}({sd})"
        if not events or last_key[0] != key:
            last_key[0] = key
            events.append(f"t={time.time() - started:5.1f}s  {key}  "
                          f"ai(action={sample.get('action')}, "
                          f"repair_phase={sample.get('phase')})")
        return (fe, fr), (se, sr)

    # Admissible precondition (#1736's shape): the window has to OPEN with
    # neither candidate engaged, or the first ordering event already
    # happened off-camera and nothing observed later can name it.
    base = repair_ordering_sample(port, uid, first, second)
    if base is None:
        return {"verdict": "inconclusive", "events": events,
                "reason": "the unit reported no AI state when the ordering "
                          "window opened",
                "first_repaired": False, "second_repaired": False}
    (base_first, _), (base_second, _) = record(base)
    if base_first or base_second:
        engaged = ", ".join(
            label for label, hit in ((first_label, base_first),
                                     (second_label, base_second)) if hit)
        verdict = "inconclusive"
        reason = (f"the AI was already engaged with {engaged} when the "
                  f"ordering window opened, so the first ordering event "
                  f"was missed")

    if open_window is not None:
        open_window()
        started = time.time()
    first_repaired = second_repaired = False
    deadline = started + seconds
    while not first_repaired and time.time() < deadline:
        # Same defensive unpause poll_until does: any unit_warning pauses
        # this repo's engine, which would otherwise freeze the timeline.
        send(port, "engine.setPaused(false); return 'ok'")
        sample = repair_ordering_sample(port, uid, first, second)
        if sample is not None:
            (hit_first, res_first), (hit_second, res_second) = record(sample)
            if verdict is None and (hit_first or hit_second):
                if hit_first and hit_second:
                    verdict = "inconclusive"
                    reason = (f"{first_label} and {second_label} both first "
                              f"appear engaged in the same snapshot, so "
                              f"neither can be named first")
                elif hit_first:
                    verdict = "first"
                    reason = (f"{first_label} was engaged first, with "
                              f"{second_label} not yet claimed, targeted or "
                              f"restored")
                else:
                    verdict = "second"
                    reason = (f"{second_label} was engaged first, with "
                              f"{first_label} not yet claimed, targeted or "
                              f"restored")
            first_repaired = first_repaired or res_first
            second_repaired = second_repaired or res_second
        time.sleep(0.3)

    if verdict is None:
        verdict = "inconclusive"
        reason = (f"no ordering event was observed within {seconds:g}s -- "
                  f"neither candidate was ever seen claimed, targeted or "
                  f"restored")
    return {"verdict": verdict, "reason": reason, "events": events,
            "first_repaired": first_repaired,
            "second_repaired": second_repaired}


def ground_state(port: int, iid: int):
    """{gid, cond, sharp, qual, kind} for the ground row carrying item
    INSTANCE iid, or None. Keyed on the instance rather than the gid
    because a returned target is re-spawned at the worker's own tile and
    so gets a fresh gid -- the instance id is the only stable identity
    across the take-and-return round trip."""
    return send_json(port,
        f"for _,g in ipairs(item.listGround() or {{}}) do "
        f"  if g.instanceId=={iid} then "
        f"    return {{gid=g.id, cond=g.condition, sharp=g.sharpness, "
        f"             qual=g.quality, kind=g.kind, x=g.x, y=g.y}} end end; "
        f"return nil")


def held_row(port: int, uid: int, iid: int):
    """{cond, sharp, qual} for instance iid in uid's own INVENTORY (the
    only place a picked-up ground target can be), or None."""
    return send_json(port,
        f"for _,it in ipairs(unit.getInventory({uid}) or {{}}) do "
        f"  if it.instanceId=={iid} then "
        f"    return {{cond=it.condition, sharp=it.sharpness, "
        f"             qual=it.quality}} end end; return nil")


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
    skill_before = send_json(port, f"return unit.getSkill({uid}, 'smithing')")
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
    skill_after = send_json(port, f"return unit.getSkill({uid}, 'smithing')")
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
    bid_cached = poll_until(port, 30, lambda: send_json(port,
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
    family = send_json(port,
        "return require('scripts.unit_roles').ACTION_FAMILY.repair_job")
    check("repair_job mapped to the 'craft' family", family == "craft")

    def weight(role: str, action: str) -> float:
        return send_json(port,
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
    # Both items go in HEALTHY first, so neither is a repair candidate yet
    # and the AI cannot claim either (#1736's admissible precondition).
    # Degrading a broken axe into the inventory several console round trips
    # before the flag lands gives the live AI time to claim it, and once
    # s.repairJob is set repairUtility returns the in-progress lock and
    # never re-scans — the flag then arrives too late to invert anything
    # and the phase proves nothing (observed on master, #1767).
    axe = add_item_instance(port, uid, "axe_steel")
    gam = add_item_instance(port, uid, "wool_gambeson")

    check("item starts unflagged", send(port,
        f"local ai=require('scripts.unit_ai'); "
        f"return ai.isRepairPriority({gam})") == "false")

    flagged = send(port,
        f"local ai=require('scripts.unit_ai'); "
        f"ai.setRepairPriority({gam}, true); "
        f"return ai.isRepairPriority({gam})") == "true"
    check("unitAi.setRepairPriority flags the armor instance", flagged)

    # ONE timeline, not two reads (#1767). The old oracle waited for the
    # armor to hit 100 and then required the axe to still be at exactly 0
    # -- but the armor reaching 100 is the very event that clears the
    # priority flag and frees the job, so the AI is then correctly free to
    # start on the axe, and a single repair visit restores it atomically.
    # The ordering is now decided by the FIRST claim/target/restoration
    # event observed on either instance, which correct follow-on work
    # cannot invalidate.
    #
    # The observer opens the candidate window itself: it baselines both
    # instances while they are still healthy (so neither CAN be engaged),
    # then degrades both in ONE round trip. The two candidates therefore
    # appear at the same instant with the flag already in place, and
    # sampling is already running when the AI makes its first pick.
    ordering = observe_repair_ordering(
        port, uid, gam, axe, "armor", "weapon", seconds=120,
        open_window=lambda: force_item_states(
            port, uid, [(axe, 0.0, 100.0), (gam, 5.0, 100.0)]))
    check("player-prioritized armor (lower severity) repaired",
          ordering["first_repaired"])

    armor_first = ordering["verdict"] == "first"
    if not armor_first:
        print(f"      ordering verdict: {ordering['verdict']} -- "
              f"{ordering['reason']}")
        print("      observations the ordering was judged on:")
        for line in ordering["events"]:
            print(f"        {line}")
    check("the flagged armor was claimed/repaired before any work on the "
          "higher-severity weapon", armor_first)

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
          send_json(port, f"local ai=require('scripts.unit_ai'); "
                          f"return ai.itemNeedsRepair({healthy})") is False)
    check("itemNeedsRepair is true for a degraded item (5% < threshold)",
          send_json(port, f"local ai=require('scripts.unit_ai'); "
                          f"return ai.itemNeedsRepair({degraded})") is True)

    # Flag the HEALTHY instance directly at the backend (simulating any
    # path that could set the flag without going through the gated
    # menu) and confirm the UI layer refuses to offer/show it anyway.
    send(port, "local ai=require('scripts.unit_ai'); "
               "ai.setRepairPriority(999901, true); return 'ok'")
    check("menuItem offers nothing for a flagged-but-healthy item",
          send_json(port, f"local rs=require('scripts.ui.repair_status'); "
                          f"return rs.menuItem({healthy}) ~= nil") is False)
    # `send`, not `send_json`: the assertion is that the suffix is the
    # EMPTY STRING, and send_json maps an empty transport result to None
    # (probelib.py). `send` returns the bare text, quotes already
    # stripped, so "" stays "".
    check("suffix shows nothing for a flagged-but-healthy item",
          send(port, f"local rs=require('scripts.ui.repair_status'); "
                     f"return rs.suffix({healthy})") == "")
    check("hintLine shows nothing for a flagged-but-healthy item",
          send_json(port, f"local rs=require('scripts.ui.repair_status'); "
                          f"return rs.hintLine({healthy})") is None)

    # The degraded instance, still unflagged, DOES get offered.
    check("menuItem offers 'Prioritize Repair' for a degraded item",
          send_json(port, f"local rs=require('scripts.ui.repair_status'); "
                          f"local m = rs.menuItem({degraded}); "
                          f"return m ~= nil and m.label") == "Prioritize Repair")


def phase_ground_target(port: int) -> None:
    print("\n[phase 10] GROUND-held TARGET (#1737): the ladder's middle "
          "rung — claim, instance-preserving pickup, repair, return")
    # Own row, clear of every other phase's furniture.
    build_station(port, "furnace", 3, -18, {"granite_chunk": 6, "steel_bar": 2})
    uid = spawn_acolyte(port, 4.5, -17.5)
    # The consumable is ground-sourced for phase 1's reason (an idle
    # Materials item in inventory is fair game for store_materials).
    send(port, "item.spawnGround('lignite_chunk', 6.5, -17.5); "
               "item.spawnGround('lignite_chunk', 6.5, -16.5); return 'ok'")
    # The TARGET itself on the ground. condition=5 is an explicit salvage
    # base, and the penalty roll only ever subtracts, so the spawned
    # instance is always under repair_condition_threshold.
    gid = int(float(send(port,
        "return item.spawnGround('axe_steel', 7.5, -18.5, "
        "{ condition = 5 })")))
    # Single-valued on purpose: getGroundForUnit returns (entry,
    # pageResolved), and the console serializes a multi-return as
    # tab-separated values, which is not JSON.
    row = send_json(port,
        f"local r = item.getGroundForUnit({uid}, {gid}); return r")
    check("ground row carries instanceId, sharpness and kind (#1737)",
          isinstance(row, dict) and row.get("instanceId") is not None
          and row.get("sharpness") is not None and row.get("kind") == "weapon")
    if not isinstance(row, dict) or row.get("instanceId") is None:
        destroy_unit(port, uid)
        return
    iid = int(row["instanceId"])
    sharp0, qual0 = row["sharpness"], row["quality"]
    check("the ground target is degraded below the repair threshold",
          row["condition"] < 50)

    job = poll_until(port, 90, lambda: repair_job_of(port, uid))
    check("acolyte claimed the GROUND instance, not a held or mule one",
          job is not None and job.get("instanceId") == iid
          and job.get("fromGround") is True and job.get("onMule") is False)

    held = poll_until(port, 180, lambda: held_row(port, uid, iid))
    check("the EXACT instance was taken into inventory (item.pickupGround "
          "preserves it; nothing was re-created)", held is not None)
    check("its quality and sharpness survived the pickup untouched",
          held is not None and held["qual"] == qual0
          and held["sharp"] == sharp0)
    check("it is no longer lying on the ground while carried",
          ground_state(port, iid) is None)

    # The job ENDS by returning the target to the ground, so the repaired
    # state is observed wherever it has got to by then.
    repaired = poll_until(port, 240, lambda:
        ((held_row(port, uid, iid) or {}).get("cond") == 100)
        or ((ground_state(port, iid) or {}).get("cond") == 100))
    check("the ground-sourced weapon was repaired to full condition",
          repaired is not None)

    back = poll_until(port, 90, lambda: ground_state(port, iid))
    check("the target was RETURNED to the ground when the job ended "
          "(#1737 requirement 8), never left in the worker's inventory",
          back is not None)
    check("it went back at full condition and unchanged quality",
          back is not None and back["cond"] == 100 and back["qual"] == qual0)
    check("the worker is not still holding it", held_row(port, uid, iid) is None)
    destroy_unit(port, uid)


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
    "ground_target": phase_ground_target,
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
            # unit_warning event, and this repo's config/notifications.local.yaml
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
