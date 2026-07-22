#!/usr/bin/env python3
"""Mental-effectiveness probe (#353) — the shared combat/craft
mental-effectiveness plumbing, end to end.

``Test.Headless.Combat.MentalEffectiveness`` (hspec) is the deterministic
authority for the mentalEffectiveness / hitChance / defenderDodgeChance /
applyMentalQuality formulas against synthetic UnitInstance fixtures. This
probe drives a REAL headless engine to prove the Haskell<->Lua plumbing on
top of that math:

  1. ``unit.getMentalEffectiveness(uid)`` returns the documented values
     for pinned concentration/mental_state combinations, read off a REAL
     UnitInstance (not a synthetic hspec fixture).
  2. Craft-bill progress: the REAL scripts/unit_ai_craft.lua progress-
     pour code path (driven white-box via craftUtility/craftExecute,
     exactly like tools/craft_bill_probe.py's own technique — not a
     reimplementation of its formula in this probe) accrues progress,
     over a known real-time gap, proportional to effectiveness.
  3. Craft completion applies the #353 quality delta on top of the
     existing #343 skill x knowledge base quality via a real
     ``craft.executeAt`` call, clamped, matching the exact formula the
     hspec suite pins.
  4. Combat event damage energy (``combat.drainEvents()``'s "raw" hit
     payload) is not shifted by mental effectiveness: attacker/defender
     stats are pinned so hits reliably land (rawHit pinned past the 0.95
     cap) at every effectiveness value under test, rather than comparing
     hit RATES (which the issue explicitly forbids) — the deterministic
     proof that damage energy itself can't be reached by effectiveness
     lives in the hspec suite (computeAttackerSkill/computeDefenderEvasion
     invariance to concentration/mental_state); this is an end-to-end
     sanity check on top of it.

``scripts/brain.lua``'s ``brain.tick``/``scripts/mental_state.lua``'s
``mental.tick`` are neutralised for the probe's duration so a pinned
concentration/mental_state value can't be silently overwritten by the
live homeostasis tick between checks — the same "AI off" idiom
tools/craft_bill_probe.py uses for unit_ai.update.

Usage: python3 tools/mental_efficiency_probe.py [--port 9353]
"""
import argparse
import glob
import json
import sys
import time

from probelib import boot, quit_engine, send, spawn_acolyte, poll_until

SPROOT = "/tmp"
TEST_YAML = f"{SPROOT}/mental_efficiency_probe_recipes.yaml"

# Tiny work + a skill tag so the base quality (#343) is deterministic
# (craftQuality(skill, Nothing) = skill, unclamped at 50). The output is
# a weapon (steel_dagger) rather than a raw material: item.getInventory
# only surfaces a "quality" field when the item def declares a quality
# spec (Engine.Scripting.Lua.API.Units.Inventory) — a plain material
# like granite_chunk has none, so the #353 delta would be unobservable
# through unit.getInventory even though it was correctly applied.
TEST_RECIPES = """\
recipes:
  - id: mental_probe_forge
    station: forge
    inputs:
      - item: steel_bar
    work: 4
    skill: smithing
    outputs:
      - item: steel_dagger
"""


def jget(port, lua, timeout=10.0):
    raw = send(port, lua, timeout)
    try:
        return json.loads(raw)
    except json.JSONDecodeError:
        return raw.strip('"')


def check(passed, ok, label, detail=""):
    print(f"  [{'PASS' if ok else 'FAIL'}] {label}"
          + (f": {detail}" if detail else ""))
    return passed and ok


def near(a, b, tol=0.01):
    return abs(a - b) < tol


def bootstrap(port):
    """Load defs + a flat arena (the loading screen doesn't run headless).
    Mirrors tools/craft_bill_probe.py's proven bootstrap exactly."""
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
    send(port, "return world.loadChunksInRegion(-2, -2, 2, 2)")
    send(port, "return world.waitForChunks(60)", timeout=65.0)


def mental_off(port):
    """Neutralise brain.tick/mental.tick so a pinned concentration/
    mental_state stat can't be overwritten by the live homeostasis tick
    (if it happens to be running) between checks."""
    send(port,
         "local b = require('scripts.brain'); "
         "if not b.__probe_orig_tick then b.__probe_orig_tick = b.tick end; "
         "b.tick = function() end; return 'ok'")
    send(port,
         "local m = require('scripts.mental_state'); "
         "if not m.__probe_orig_tick then m.__probe_orig_tick = m.tick end; "
         "m.tick = function() end; return 'ok'")


def ai_off(port):
    """scripts/unit_ai.lua is auto-loaded headless and live-ticking —
    it would otherwise autonomously work the probe's own craft bills
    and drive spawned units around (wander, retreat, ...), racing every
    manually-driven check below. Mirrors tools/craft_bill_probe.py's
    identical idiom."""
    send(port,
         "local ai = require('scripts.unit_ai'); "
         "if not ai.__probe_orig_update then "
         "ai.__probe_orig_update = ai.update end; "
         "ai.update = function() end; return 'ok'")


def combat_log_off(port):
    """scripts/combat_log.lua is auto-loaded headless and drains
    combat.drainEvents() every tick (0.1s) to feed the combat-log UI —
    a second consumer racing this probe's own drain calls for the same
    events. Neutralise its update so this probe is the only reader."""
    send(port,
         "local cl = require('scripts.combat_log'); "
         "if not cl.__probe_orig_update then cl.__probe_orig_update = cl.update end; "
         "cl.update = function() end; return 'ok'")


def resources_off(port):
    """scripts/unit_resources.lua is auto-loaded headless and live-
    ticking: it periodically re-derives body-composition stats
    (strength, max_stamina, ...) from lean_mass/fat_mass via
    unit.recomputeBody-style logic, silently overwriting a directly
    pinned unit.setStat('strength', ...)/('stamina', ...) on the very
    next physiology tick. Neutralise it so a pinned attacker stat stays
    pinned for the combat damage-energy sample."""
    send(port,
         "local ur = require('scripts.unit_resources'); "
         "if not ur.__probe_orig_update then ur.__probe_orig_update = ur.update end; "
         "ur.update = function() end; return 'ok'")


def pin(port, uid, concentration, euphoric):
    """Pin concentration + mental_state directly (0 = STABLE, 3 = EUPHORIC
    in scripts/mental_state.lua) — never treating the world seed as a
    combat RNG seed, per the issue's explicit requirement."""
    ms = 3 if euphoric else 0
    send(port,
         f"unit.setStat({uid}, 'concentration', {concentration}); "
         f"unit.setStat({uid}, 'mental_state', {ms}); return 'ok'")


def get_effectiveness(port, uid):
    return float(send(port, f"return unit.getMentalEffectiveness({uid})"))


def spawn_station(port, uid, def_name, gx, gy, materials, progress=500):
    """building.spawn + deliver build materials through the real
    machinery, then addBuildProgress to Built. Mirrors
    tools/craft_bill_probe.py's spawn_station."""
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


def add_bill(port, bid, recipe, count=None):
    """-> (billId or None, err)."""
    arg = f", {count}" if count is not None else ""
    raw = send(port,
               f"local id,err = craft.addBill({bid}, '{recipe}'{arg}); "
               f"return id and ('ID:'..id) or ('ERR:'..tostring(err))"
               ).strip('"')
    if raw.startswith("ID:"):
        return int(float(raw[3:])), ""
    return None, raw


def _craft_ai_tick(port, uid):
    """One real scripts/unit_ai_craft.lua craftExecute(uid, state, params)
    invocation against this probe's own per-unit state table (module-
    scoped so it survives across separate debug-console lines, the same
    reason tools/craft_bill_probe.py's white-box checks stash theirs on
    ai.__probe_s rather than a plain Lua local)."""
    send(port,
         f"local ai = require('scripts.unit_ai_craft'); "
         f"local p = require('scripts.unit_ai_tunables').acolyte; "
         f"ai.craftExecute({uid}, ai.__probe_states[{uid}], p); return 'ok'")


def craft_progress_via_ai(port, uid, sleep_s):
    """Drive the REAL scripts/unit_ai_craft.lua progress-pour code path
    (white-box, mirroring tools/craft_bill_probe.py's own
    craftUtility/craftExecute technique) with a KNOWN elapsed real-time
    gap, and return (billId, progressBefore, progressAfter). Exercising
    the genuine module — rather than reimplementing its formula in this
    probe — is what actually catches a regression in
    unit_ai_craft.lua's own mental-effectiveness term (an omission,
    wrong placement, or a stale multiplier).

    `uid` must already be adjacent to the intended station's footprint
    and carrying enough of the recipe's inputs, and that station must
    have exactly one pending, unclaimed bill (whichever recipe) for
    craftUtility's scan to find unambiguously.

    Call sequence, each its own real craftExecute invocation:
      1. craftUtility — scans, sets state.craftCandidate.
      2. craftExecute — claims the bill; phase -> "fetch"; returns.
      3. craftExecute — fetch (inputs already held -> instant) falls
         through to walking (already adjacent -> instant); phase ->
         "working", lastCraftAt = engine.gameTime() now; returns.
      4. (sleep `sleep_s` real seconds)
      5. craftExecute — phase "working": elapsed = gameTime() - the
         lastCraftAt stamped in step 3, i.e. the real gap this
         function actually slept — pours real progress via
         craft.addBillProgress, scaled by params.craft_rate, skill,
         AND unit.getMentalEffectiveness(uid), exactly as shipped.
    """
    send(port,
         f"local ai = require('scripts.unit_ai_craft'); "
         f"ai.__probe_states = ai.__probe_states or {{}}; "
         f"ai.__probe_states[{uid}] = {{}}; return 'ok'")
    send(port,
         f"local ai = require('scripts.unit_ai_craft'); "
         f"local p = require('scripts.unit_ai_tunables').acolyte; "
         f"ai.craftUtility({uid}, ai.__probe_states[{uid}], p); return 'ok'")
    _craft_ai_tick(port, uid)   # claim -> phase "fetch"
    _craft_ai_tick(port, uid)   # fetch + walk (both instant) -> phase "working"
    bill_id_raw = send(port,
        f"local ai = require('scripts.unit_ai_craft'); "
        f"local j = ai.__probe_states[{uid}].craftJob; "
        f"return j and j.billId or 'nil'").strip('"')
    if bill_id_raw in ("nil", ""):
        return None, None, None
    bill_id = int(float(bill_id_raw))
    before = (jget(port, f"return craft.getBill({bill_id})") or {}).get("progress")
    time.sleep(sleep_s)
    _craft_ai_tick(port, uid)   # phase "working" -> real progress pour
    after = (jget(port, f"return craft.getBill({bill_id})") or {}).get("progress")
    return bill_id, before, after


def craft_execute(port, uid, recipe, bid):
    """-> (ok, [instanceIds] or errText)."""
    raw = send(port,
        f"local ok,res = craft.executeAt({uid}, '{recipe}', {bid}); "
        f"if ok then return 'OK:' .. table.concat(res, ',') "
        f"else return 'ERR:' .. tostring(res) end").strip('"')
    if raw.startswith("OK:"):
        ids = [int(x) for x in raw[3:].split(",") if x]
        return True, ids
    return False, raw[4:]


def item_quality(port, uid, iid):
    raw = send(port,
        f"for _,it in ipairs(unit.getInventory({uid}) or {{}}) do "
        f"if it.instanceId == {iid} then return it.quality end end; "
        f"return -1")
    return float(raw)


def combat_damage_sample(port, conc, euphoric, want, retries):
    """Fire fresh attacker/defender swings (a fresh pair per attempt, so
    wound accumulation on one side can't drift the comparison) until
    `want` LANDED-hit "raw" damage-energy values are collected.
    Attacker/defender stats are pinned so rawHit sits well past the 0.95
    outer clamp at EVERY valid effectiveness (0.75..1.10) — hits should
    land almost every attempt regardless of which effectiveness is under
    test. Unarmed (no weapon, no natural weapon) restricts every landed
    hit to the SAME "blunt" mechanism (the only kind with nonzero
    effectiveness in Combat.Resolution.Strike.pickPartKind's fallback),
    and the "raw" field (Combat.Resolution.Damage's driver energy/
    momentum) is a function of the ATTACKER's own swing alone — strength/
    mode/skill/stamina/pain plus fixed body_mass/height/arm geometry —
    never of which body part the RNG-driven part/kind picker actually
    landed on. Pinning body_mass/height/stamina (species templates roll
    these per spawn, and a fresh unit's rolled stamina otherwise scales
    the swing's work via staminaFrac) makes "raw" reproducible across
    independently-random swings, unlike "eff" (post target-resistance/
    toughness, and so dominated by which body part got hit)."""
    vals = []
    attempts = 0
    while len(vals) < want and attempts < retries:
        attempts += 1
        atk = int(float(send(port, f"return unit.spawn('acolyte', 2, 2)")))
        tgt = int(float(send(port, f"return unit.spawn('acolyte', 4, 2)")))
        # An acolyte spawns with a steel dagger EQUIPPED by default —
        # strip it so mEquipped is Nothing and Combat.Resolution.Strike.
        # pickPartKind falls back to the "unarmed" natural-weapon
        # profile (stab/slash effectiveness 0, blunt 1.0), which is what
        # actually pins every landed hit to the single "blunt" mechanism
        # this function's docstring relies on — a dagger's real stab/
        # slash mix would otherwise pick a different weapon FACET
        # (Combat.Resolution.Strike.resolveStrike takes kind as an
        # input) on every swing, varying "raw" for reasons unrelated to
        # mental effectiveness.
        send(port, f"unit.dropEquipmentToGround({atk}, 'right_hand'); "
                   f"unit.dropEquipmentToGround({atk}, 'left_hand'); return 'ok'")
        send(port,
             f"unit.setStat({atk}, 'dexterity', 20); "
             f"unit.setStat({atk}, 'perception', 20); "
             f"unit.setStat({atk}, 'strength', 20); "
             f"unit.setStat({atk}, 'body_mass', 70); "
             f"unit.setStat({atk}, 'height', 1.8); "
             f"unit.setStat({atk}, 'stamina', 10000); "
             f"unit.setSkill({atk}, 'unarmed', 100); "
             f"unit.setStat({tgt}, 'agility', 0.01); "
             f"unit.setStat({tgt}, 'reflexes', 0.01); "
             f"unit.setStat({tgt}, 'dexterity', 0.01); "
             f"unit.setStat({tgt}, 'perception', 0.01); "
             f"unit.setStat({tgt}, 'body_mass', 70); "
             f"unit.setSkill({tgt}, 'balance', 0); "
             f"unit.setSkill({tgt}, 'dodge', 0); "
             f"return 'ok'")
        pin(port, atk, conc, euphoric)
        pin(port, tgt, conc, euphoric)
        send(port, "combat.drainEvents(); return 'ok'")  # clear stale events
        send(port, f"combat.attack({atk}, {tgt}, 'quick'); return 'ok'")
        evs = poll_until(3.0, lambda: jget(port, "return combat.drainEvents()") or None)
        for e in (evs or []):
            if e.get("kind") == "hit":
                vals.append(float(e["payload"]["raw"]))
                break
        send(port, f"unit.destroy({atk}); unit.destroy({tgt}); return 'ok'")
    return vals


def main():
    ap = argparse.ArgumentParser()
    ap.add_argument("--port", type=int, default=9353)
    args = ap.parse_args()
    port = args.port
    passed = True

    proc = boot(port, f"{SPROOT}/mental_efficiency_probe_engine.log")
    try:
        bootstrap(port)
        mental_off(port)
        combat_log_off(port)
        ai_off(port)
        resources_off(port)

        # --- 1. The exposed effectiveness value (real UnitInstance) ---
        uid = spawn_acolyte(port, 2, 2, clear_water=False)
        mental_off(port)  # re-neutralise in case the spawn re-required the modules

        pin(port, uid, 1.0, False)
        eff = get_effectiveness(port, uid)
        passed = check(passed, near(eff, 1.00), "neutral concentration -> 1.00", f"eff={eff}")

        pin(port, uid, 0.5, False)
        eff = get_effectiveness(port, uid)
        passed = check(passed, near(eff, 0.875), "concentration 0.5 -> 0.875", f"eff={eff}")

        pin(port, uid, 0.0, False)
        eff = get_effectiveness(port, uid)
        passed = check(passed, near(eff, 0.75), "zero concentration -> 0.75", f"eff={eff}")

        pin(port, uid, 1.0, True)
        eff = get_effectiveness(port, uid)
        passed = check(passed, near(eff, 1.10), "full concentration + euphoria -> 1.10", f"eff={eff}")

        pin(port, uid, 0.0, True)
        eff = get_effectiveness(port, uid)
        passed = check(passed, near(eff, 0.75 * 1.10),
                       "zero concentration + euphoria -> 0.75 x 1.10", f"eff={eff}")

        # --- setup for 2/3: a built furnace + the probe recipe ---
        with open(TEST_YAML, "w") as f:
            f.write(TEST_RECIPES)
        n = int(float(send(port, f"return engine.loadRecipeYaml('{TEST_YAML}')")))
        passed = check(passed, n == 1, "probe recipe loaded", f"count={n}")

        crafter = spawn_acolyte(port, 6, 2, clear_water=False)
        mental_off(port)
        bid = spawn_station(port, crafter, "workbench", 8, 2,
                            {"wood_log": 4, "steel_hardware": 4, "steel_bar": 2})
        # craft.executeAt (used directly below, bypassing the AI's own
        # walk-to-station phase) refuses a crafter that isn't adjacent
        # to the station's footprint.
        send(port, f"unit.setPos({crafter}, 8, 2); return 'ok'")
        send(port, f"unit.setSkill({crafter}, 'smithing', 50); return 'ok'")

        # --- 2. Craft-bill progress scales by the real effectiveness,
        #        driven through the ACTUAL scripts/unit_ai_craft.lua
        #        progress-pour code path (craftUtility/craftExecute),
        #        not a reimplementation of its formula in this probe —
        #        see craft_progress_via_ai's docstring. One bill at a
        #        time so craftUtility's scan is unambiguous.
        CRAFT_SLEEP_S = 2.0

        send(port, f"unit.addItem({crafter}, 'steel_bar'); return 'ok'")
        bill_lo, msg_lo = add_bill(port, bid, "mental_probe_forge", 5)
        passed = check(passed, bill_lo is not None, "distracted probe bill added", msg_lo)
        pin(port, crafter, 0.0, False)   # effectiveness 0.75
        eff_lo = get_effectiveness(port, crafter)
        got_lo, before_lo, after_lo = craft_progress_via_ai(port, crafter, CRAFT_SLEEP_S)
        passed = check(passed, got_lo == bill_lo,
                       "the real AI claimed + worked the distracted probe bill",
                       f"claimed={got_lo} want={bill_lo}")
        send(port, f"craft.cancelBill({bill_lo}); return 'ok'")

        send(port, f"unit.addItem({crafter}, 'steel_bar'); return 'ok'")
        bill_hi, msg_hi = add_bill(port, bid, "mental_probe_forge", 5)
        passed = check(passed, bill_hi is not None, "focused probe bill added", msg_hi)
        pin(port, crafter, 1.0, False)   # effectiveness 1.00
        eff_hi = get_effectiveness(port, crafter)
        got_hi, before_hi, after_hi = craft_progress_via_ai(port, crafter, CRAFT_SLEEP_S)
        passed = check(passed, got_hi == bill_hi,
                       "the real AI claimed + worked the focused probe bill",
                       f"claimed={got_hi} want={bill_hi}")
        send(port, f"craft.cancelBill({bill_hi}); return 'ok'")

        delta_lo = (after_lo - before_lo) if None not in (before_lo, after_lo) else None
        delta_hi = (after_hi - before_hi) if None not in (before_hi, after_hi) else None
        passed = check(passed, near(eff_lo, 0.75) and near(eff_hi, 1.00),
                       "distinct pinned effectiveness values read back correctly",
                       f"eff_lo={eff_lo} eff_hi={eff_hi}")
        passed = check(passed,
                       delta_lo is not None and delta_hi is not None
                       and delta_lo > 0 and delta_hi > 0,
                       "the real progress-pour tick accrued progress for both crafters",
                       f"delta_lo={delta_lo} delta_hi={delta_hi}")
        passed = check(passed,
                       bool(delta_lo) and bool(delta_hi)
                       and near(delta_lo / delta_hi, 0.75, tol=0.1),
                       "real AI-driven progress-pour rate scales by the effectiveness "
                       "ratio (0.75)",
                       f"ratio={(delta_lo / delta_hi) if delta_hi else None}")

        # --- 3. Completion quality adjustment + clamping ---
        send(port, f"unit.addItem({crafter}, 'steel_bar'); return 'ok'")
        pin(port, crafter, 0.0, False)   # effectiveness 0.75 -> delta clamps at -10
        ok, res = craft_execute(port, crafter, "mental_probe_forge", bid)
        passed = check(passed, ok, "distracted (eff 0.75) craft executes", res)
        if ok:
            q_lo = item_quality(port, crafter, res[0])
            passed = check(passed, near(q_lo, 40.0, tol=1.0),
                           "distracted output quality = base 50 - 10 (clamped) = 40",
                           f"quality={q_lo}")

        send(port, f"unit.addItem({crafter}, 'steel_bar'); return 'ok'")
        pin(port, crafter, 1.0, True)    # effectiveness 1.10 -> delta clamps at +10
        ok, res = craft_execute(port, crafter, "mental_probe_forge", bid)
        passed = check(passed, ok, "euphoric (eff 1.10) craft executes", res)
        if ok:
            q_hi = item_quality(port, crafter, res[0])
            passed = check(passed, near(q_hi, 60.0, tol=1.0),
                           "euphoric output quality = base 50 + 10 (clamped) = 60",
                           f"quality={q_hi}")

        # --- 4. Combat damage energy unchanged across effectiveness ---
        lo_vals = combat_damage_sample(port, 0.0, False, want=6, retries=20)
        hi_vals = combat_damage_sample(port, 1.0, True, want=6, retries=20)
        passed = check(passed, len(lo_vals) >= 4 and len(hi_vals) >= 4,
                       "enough landed hits sampled at both effectiveness levels",
                       f"lo={len(lo_vals)} hi={len(hi_vals)}")
        if lo_vals and hi_vals:
            mean_lo = sum(lo_vals) / len(lo_vals)
            mean_hi = sum(hi_vals) / len(hi_vals)
            # "raw" (pre-target-resistance driver energy/momentum) is a
            # function of the ATTACKER's own pinned stats alone, never of
            # which body part the RNG-driven picker landed on — so unlike
            # "eff" it should be reproducible near-exactly across
            # independently-random swings. A tight tolerance here is
            # therefore meaningful, not just generous slack; the
            # deterministic proof that damage energy can't be reached by
            # effectiveness at all remains the hspec suite's
            # computeAttackerSkill/computeDefenderEvasion invariance
            # check — this is an end-to-end plumbing sanity check on top
            # of it.
            ratio = mean_hi / mean_lo if mean_lo else float("inf")
            passed = check(passed, 0.95 < ratio < 1.0526,
                           "mean landed-hit raw damage energy is not shifted by effectiveness",
                           f"mean(eff=0.75)={mean_lo:.3f} mean(eff=1.10)={mean_hi:.3f} ratio={ratio:.3f}")

        print("\n" + ("ALL MENTAL EFFECTIVENESS CHECKS PASSED" if passed
                      else "SOME FAILED"))
        return 0 if passed else 1
    finally:
        quit_engine(port, proc)


if __name__ == "__main__":
    sys.exit(main())
