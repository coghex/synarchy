#!/usr/bin/env python3
"""Stop-before-waste meal policy probe (#1219, SURV-7).

`unit.feed` consumes DISCRETE food whole and clamps the credit at
`max_hunger`, binning the overflow by design. So the marginal ration of
a meal used to be mostly thrown away: `eatExecute` fed to 99 % fullness,
and a hungry acolyte opened ~3 indivisible 250 kcal rations against a
body-mass-scaled stomach. The policy (project-owner decision SURV-7) is
to stop before waste — keep feeding to full from BULK food, but do not
open another discrete item once the stomach can no longer hold most of
it.

Everything here runs on a flat `world.initArena` page with the AI
DISPATCH LOOP deliberately not loaded: `scripts/unit_ai.lua` is never
loaded and nothing but this probe ever decides to eat. Each scenario
sets the stomach level and runs one real meal through the exported
`unitAiNeeds.eatExecute` with the real `unit_ai_tunables` params, in ONE
console line, so the engine's own metabolism cannot drain hunger between
the setup and the meal. That makes one meal one synchronous production
call with exact, race-free counts.

`max_hunger` is `body_mass * 10` (Unit.Thread.Command.Body), continuously
re-derived, and body mass is ROLLED per spawn — observed spawns range
over 43-105 kg, i.e. 437-1043 kcal of stomach. An unpinned run would test
whichever stomach it drew, and a small acolyte has no room to eat two
rations before the marginal one at all. `max_hunger` itself cannot be
held (the body refresh overwrites it within a tick), so each unit's
`body_mass` is pinned to 71.3 kg instead, which re-derives to exactly the
issue's canonical 713 kcal default acolyte. Body size is not what is
under test. Every threshold is still derived from the unit's OWN reported
`max_hunger`, and the expected feed count is recomputed by
`expected_feeds` below — an independent restatement of the documented
rule, so an edit to either side fails.

Six checks:

  1. WITHHOLD   — a hungry acolyte carrying 3 rations, with less than
                  half a ration of room left after the second, stops
                  with the third UNOPENED. The withheld item costs
                  exactly zero `unit.feed` calls (so no eating animation:
                  `UnitEat` is queued only on a successful feed) and zero
                  `salts.mealSalt` calls, while every item that WAS eaten
                  still got its salt.
  2. BULK       — the same setup plus a part-full quinoa sack finishes
                  the meal from the sack: two rations eaten, the third
                  still withheld, the sack drawn down, the 99 % meal
                  target reached. The sack deliberately offers FEWER
                  calories than a ration, so largest-available selection
                  picks rations first and the sack can only be reached
                  by continuing PAST a withheld ration.
  3. FIRST ITEM — the threshold governs the MARGINAL item only. A unit
                  whose stomach is already too full to hold half a
                  ration still opens exactly one, then stops.
  4. STARVING   — Requirement 4: a near-starving unit carrying only
                  rations still eats them, because genuine room passes
                  the threshold.
  5. TEN BOUND  — the meal's 10-successful-feed bound survives, and bulk
                  food is never withheld: 12 nearly-empty sacks yield
                  exactly 10 feeds and 2 leftover sacks.
  6. ENTRY      — eating/foraging ENTRY conditions are untouched: the
                  eat gate still scores (1 - hungerFrac) * eat_weight and
                  the forage gate still sees a carried ration (so a unit
                  holding food does not forage), because both ask the
                  unfiltered "is there anything edible?" query.

Exit 0 = every check passed.

Usage: python3 tools/meal_waste_probe.py [--port 9192]

This probe implements the shared `probe-result/v1` contract: `--describe`
prints its ordered stable checks without booting an engine, and a harnessed
run writes structured events while a standalone run keeps its human-readable
per-check output.
"""
from __future__ import annotations

import argparse
import glob
import sys

import probe_protocol
from probelib import boot, init_arena, quit_engine, send, send_json

LOG = "/tmp/meal_waste_engine.log"
LOG_NAME = "meal_waste_engine.log"
PROBE_KEY = "meal_waste"

CHECKS = [
    ("withholds_marginal_ration",
     "a marginal third ration stays unopened with no feed or salt side effects"),
    ("bulk_finishes_meal",
     "bulk food finishes the meal past a withheld discrete ration"),
    ("first_item_exempt",
     "the first item is exempt before later marginal items are withheld"),
    ("starving_eats_rations",
     "a near-starving unit still eats the available rations"),
    ("feed_bound_preserved",
     "the ten-feed bound survives and bulk food is never withheld"),
    ("entry_gates_unchanged",
     "eat and forage entry conditions remain unchanged"),
]

DESCRIPTOR = probe_protocol.build_descriptor(PROBE_KEY, CHECKS)

# This probe's console default. Its one-line Lua batches drive whole
# meal cycles, so they get longer than probelib.send_json's 10 s -- the
# same 20 s the local jget this file used to define had.
QUERY_TIMEOUT = 20.0

RATION = "rations"
SACK = "quinoa_sack"
RATION_KCAL = 250.0        # data/items/rations.yaml
# The issue's default acolyte: max_hunger = body_mass * 10, so a 71.3 kg
# body is a 713 kcal stomach. Pinned per unit so the scenarios below are
# exact rather than hostage to the spawn's body-mass roll.
BODY_MASS_KG = 71.3
STOMACH_KCAL = BODY_MASS_KG * 10.0
# Metabolism keeps draining hunger while a scenario runs (~1.4 kcal/s),
# so post-meal levels are compared with a slack of a few kcal.
HUNGER_SLACK = 5.0
SACK_KCAL_PER_KG = 3680.0  # data/items/quinoa_sack.yaml

# The policy under test, restated independently of the Lua.
THRESHOLD = 0.5            # scripts/unit_ai_tunables eat_discrete_min_room_fraction
MEAL_TARGET = 0.99         # eatExecute's fullness break
MEAL_BOUND = 10            # eatExecute's feed bound

# Room left in the stomach after the second ration of checks 1 and 2.
# Below half a ration, so the third is "mostly wasted" and withheld.
MARGIN = 60.0
# Room at the START of check 3's meal — already under half a ration, so
# only the first-item exemption can open anything at all.
TIGHT_START_ROOM = 100.0
# Available calories in check 2's sack: MORE than the leftover deficit
# (so it can finish the meal) but LESS than a ration (so rations are
# still picked first and the sack is only reached past a withheld one).
SACK_KCAL = 150.0
# Check 5: one feed drains a sack this small and drops it, so 12 sacks
# are 12 candidate feeds against a 10-feed bound.
TINY_SACK_KG = 0.005


def expected_feeds(max_hunger: float, start: float, rations: int) -> int:
    """Feeds a ration-only meal should perform, from the documented rule.

    Independent restatement of `eatExecute` + the #1219 threshold: feed
    while the stomach is under the target and rations remain, but stop
    opening a ration once — after the first one of this meal — the
    remaining room is under THRESHOLD of a ration.
    """
    hunger, fed = start, 0
    while fed < MEAL_BOUND and rations > 0:
        if hunger >= max_hunger * MEAL_TARGET:
            break
        if fed > 0 and (max_hunger - hunger) < THRESHOLD * RATION_KCAL:
            break
        hunger = min(max_hunger, hunger + RATION_KCAL)
        rations -= 1
        fed += 1
    return fed


def bootstrap(port: int) -> None:
    for pattern, fn in [
        ("data/substances/*.yaml", "engine.loadSubstanceYaml"),
        ("data/items/*.yaml", "engine.loadItemYaml"),
        ("data/equipment/*.yaml", "engine.loadEquipmentYaml"),
        ("data/materials/*.yaml", "engine.loadMaterialYaml"),
        ("data/units/*.yaml", "engine.loadUnitYaml"),
    ]:
        for path in sorted(glob.glob(pattern)):
            send(port, f"{fn}('{path}'); return 'ok'")


def spawn(port: int, x: float, y: float) -> int:
    """A bare acolyte with every food item stripped.

    No find_water clear and no AI stack: nothing ticks these units, so
    the only thing that ever eats is this probe's own eatExecute call.
    """
    raw = send(port, f"return unit.spawn('acolyte', {x}, {y})")
    try:
        uid = int(float(raw))
    except (TypeError, ValueError):
        sys.exit(f"unit.spawn failed: {raw!r}")
    if uid < 0:
        sys.exit(f"unit.spawn returned {uid}")
    send(port,
         f"local u={uid}; for _=1,30 do local inv=unit.getInventory(u) or {{}}; "
         f"local f=nil; for _,it in ipairs(inv) do if it.food then "
         f"f=it.defName; break end end; if not f then break end; "
         f"unit.removeItem(u,f) end; "
         f"unit.setStat(u,'body_mass',{BODY_MASS_KG}); return 'ok'")
    got = float(send(port, f"return unit.getStat({uid},'max_hunger')"))
    if abs(got - STOMACH_KCAL) > 1.0:
        sys.exit(f"pinning body_mass failed: unit {uid} reports "
                 f"max_hunger={got}, wanted {STOMACH_KCAL}")
    return uid


def meal(port: int, uid: int, give: str, set_hunger: str):
    """Stock the unit, set its stomach, and run ONE instrumented meal.

    All three happen in a single console line so the engine's metabolism
    cannot drain hunger in between. `give` and `set_hunger` are Lua
    fragments; `mh` (the unit's own max_hunger) is in scope for both.

    `unit.feed` and `salts.mealSalt` are wrapped for the duration of the
    call, so a withheld item's zero side effects are measured rather than
    inferred, then restored.
    """
    lua = (
        f"local uid={uid}; "
        "local mh=unit.getStat(uid,'max_hunger'); "
        f"{give} "
        f"{set_hunger} "
        "local start=unit.getStat(uid,'hunger'); "
        "local needs=require('scripts.unit_ai_needs'); "
        "local params=require('scripts.unit_ai_tunables').acolyte; "
        "local salts=require('scripts.salts'); "
        "local calls,fed,salted=0,0,0; "
        "local realFeed,realSalt=unit.feed,salts.mealSalt; "
        "unit.feed=function(u,n) calls=calls+1; local r=realFeed(u,n); "
        "if r then fed=fed+1 end; return r end; "
        "salts.mealSalt=function(u) salted=salted+1; return realSalt(u) end; "
        "local ok,err=pcall(needs.eatExecute, uid, {}, params); "
        "unit.feed=realFeed; salts.mealSalt=realSalt; "
        "local rations,sacks,fill=0,0,0; "
        "for _,it in ipairs(unit.getInventory(uid) or {}) do "
        f"if it.defName=='{RATION}' then rations=rations+1 end; "
        f"if it.defName=='{SACK}' then sacks=sacks+1; "
        "fill=fill+(it.currentFill or 0) end end; "
        "return {ok=ok, err=tostring(err), calls=calls, fed=fed, "
        "salted=salted, rations=rations, sacks=sacks, fill=fill, "
        "maxHunger=mh, startHunger=start, "
        "hunger=unit.getStat(uid,'hunger')}"
    )
    report = send_json(port, lua, timeout=25.0)
    if not isinstance(report, dict):
        sys.exit(f"meal report was not a table: {report!r}")
    if not report.get("ok"):
        sys.exit(f"eatExecute raised: {report.get('err')}")
    return report


def check(rep: probe_protocol.Reporter, check_id: str, passed: bool,
          label: str, detail: str, report=None) -> bool:
    event_detail = {"summary": detail}
    if report is not None:
        event_detail["report"] = report
    rep.check(check_id, passed, f"{label}: {detail}", event_detail)
    if not passed and report is not None:
        rep.note(f"         raw: {report}")
    return passed


def main() -> int:
    ap = argparse.ArgumentParser(description=__doc__)
    ap.add_argument("--port", type=int, default=9192)
    ap.add_argument("--describe", action="store_true",
                    help="print the probe-result/v1 check declaration and "
                         "exit without booting an engine")
    args = ap.parse_args()
    if args.describe:
        print(DESCRIPTOR.to_json())
        return 0
    rep = probe_protocol.reporter_from_env(DESCRIPTOR)
    try:
        return _run(args.port, rep)
    finally:
        rep.close()


def _run(port: int, rep: probe_protocol.Reporter) -> int:
    ok = True

    half_ration = THRESHOLD * RATION_KCAL
    if MARGIN >= half_ration or TIGHT_START_ROOM >= half_ration:
        sys.exit("scenario constants are not marginal — fix MARGIN")
    if not (MARGIN < SACK_KCAL < RATION_KCAL):
        sys.exit("sack must hold more than the deficit, less than a ration")

    three_rations = f"for _=1,3 do unit.addItem(uid,'{RATION}') end;"

    proc = boot(port, log=rep.engine_log_path(LOG_NAME, LOG),
                args=rep.engine_args())
    try:
        bootstrap(port)
        init_arena(port)
        rep.note(f"\nration={RATION_KCAL:.0f} kcal; a discrete item opens only "
                 f"while room >= {half_ration:.0f} kcal\n")

        # --- 1. Withhold the mostly-wasted third ration ---
        # Start so that exactly MARGIN kcal of room remain after two
        # rations: an EMPTY stomach would still have room for a third,
        # and opening it there would be correct (check 4 proves it).
        uid1 = spawn(port, 0, 0)
        r1 = meal(port, uid1, three_rations,
                  f"unit.setStat(uid,'hunger', mh - 2*{RATION_KCAL} - {MARGIN});")
        want1 = expected_feeds(r1["maxHunger"], r1["startHunger"], 3)
        ok1 = (want1 == 2 and r1["calls"] == 2 and r1["fed"] == 2
               and r1["salted"] == 2 and r1["rations"] == 1
               and abs(r1["hunger"] - (r1["maxHunger"] - MARGIN))
                   < HUNGER_SLACK)
        ok &= check(rep, "withholds_marginal_ration", ok1,
                    "3 rations: the marginal one stays unopened",
                    f"feed calls={r1['calls']} (fed={r1['fed']}, rule says "
                    f"{want1}) salt={r1['salted']} "
                    f"rations_left={r1['rations']} "
                    f"hunger={r1['hunger']:.1f}/{r1['maxHunger']:.1f} "
                    f"(room {r1['maxHunger'] - r1['hunger']:.0f} < "
                    f"{half_ration:.0f})", r1)

        # --- 2. Bulk food still finishes the meal ---
        uid2 = spawn(port, 2, 0)
        sack_fill = SACK_KCAL / SACK_KCAL_PER_KG
        r2 = meal(port, uid2,
                  f"{three_rations} unit.addItem(uid,'{SACK}',{sack_fill});",
                  f"unit.setStat(uid,'hunger', mh - 2*{RATION_KCAL} - {MARGIN});")
        drawn = (sack_fill - r2["fill"]) * SACK_KCAL_PER_KG
        ok2 = (r2["fed"] == 3 and r2["salted"] == 3 and r2["rations"] == 1
               and r2["sacks"] == 1 and drawn > 0
               and r2["hunger"] >= r2["maxHunger"] * MEAL_TARGET)
        ok &= check(rep, "bulk_finishes_meal", ok2,
                    "a part-full sack finishes the withheld meal",
                    f"fed={r2['fed']} salt={r2['salted']} "
                    f"rations_left={r2['rations']} sacks={r2['sacks']} "
                    f"sack_drawn={drawn:.1f} kcal "
                    f"hunger={r2['hunger']:.1f} (target "
                    f"{r2['maxHunger'] * MEAL_TARGET:.1f})", r2)

        # --- 3. The first item of a meal is always opened ---
        uid3 = spawn(port, 4, 0)
        r3 = meal(port, uid3, three_rations,
                  f"unit.setStat(uid,'hunger', mh - {TIGHT_START_ROOM});")
        want3 = expected_feeds(r3["maxHunger"], r3["startHunger"], 3)
        ok3 = (want3 == 1 and r3["fed"] == 1 and r3["salted"] == 1
               and r3["rations"] == 2
               and r3["hunger"] >= r3["maxHunger"] * MEAL_TARGET)
        ok &= check(rep, "first_item_exempt", ok3,
                    "first item exempt, marginal items withheld after",
                    f"start room={TIGHT_START_ROOM:.0f} (< {half_ration:.0f}) "
                    f"fed={r3['fed']} (rule says {want3}) "
                    f"rations_left={r3['rations']} "
                    f"hunger={r3['hunger']:.1f}", r3)

        # --- 4. A starving unit with only rations still eats ---
        uid4 = spawn(port, 6, 0)
        r4 = meal(port, uid4, three_rations,
                  "unit.setStat(uid,'hunger',0); "
                  "unit.setStat(uid,'calories',"
                  "(unit.getStat(uid,'max_calories') or 0)*0.05);")
        want4 = expected_feeds(r4["maxHunger"], r4["startHunger"], 3)
        ok4 = (want4 >= 2 and r4["fed"] == want4
               and r4["rations"] == 3 - want4 and r4["salted"] == want4)
        ok &= check(rep, "starving_eats_rations", ok4,
                    "near-starving unit still eats its rations",
                    f"fed={r4['fed']} (rule says {want4}) "
                    f"rations_left={r4['rations']} "
                    f"hunger={r4['hunger']:.1f}/{r4['maxHunger']:.1f}", r4)

        # --- 5. The 10-feed bound survives; bulk is never withheld ---
        uid5 = spawn(port, 8, 0)
        r5 = meal(port, uid5,
                  f"for _=1,12 do unit.addItem(uid,'{SACK}',{TINY_SACK_KG}) end;",
                  "unit.setStat(uid,'hunger',0);")
        ok5 = (r5["fed"] == MEAL_BOUND and r5["salted"] == MEAL_BOUND
               and r5["sacks"] == 2)
        ok &= check(rep, "feed_bound_preserved", ok5,
                    "10-feed meal bound intact, bulk never withheld",
                    f"fed={r5['fed']} salt={r5['salted']} "
                    f"sacks_left={r5['sacks']} (of 12)", r5)

        # --- 6. Entry conditions untouched ---
        # One line again: the gates read live hunger, so a drain between
        # the setStat and the read would move eatUtility off its formula.
        uid6 = spawn(port, 10, 0)
        frac = 0.2
        gates = send_json(port,
            f"local uid={uid6}; "
            "local mh=unit.getStat(uid,'max_hunger'); "
            f"unit.addItem(uid,'{RATION}'); "
            f"unit.setStat(uid,'hunger', mh*{frac}); "
            "local needs=require('scripts.unit_ai_needs'); "
            "local params=require('scripts.unit_ai_tunables').acolyte; "
            "local h=unit.getStat(uid,'hunger'); "
            "local e=needs.eatUtility(uid,{},params); "
            "local f=needs.forageUtility(uid,{},params); "
            "return {eat=e, forageBlocked=(f==-math.huge), "
            "expected=(1-h/mh)*params.eat_weight}",
            timeout=QUERY_TIMEOUT)
        ok6 = (isinstance(gates, dict)
               and gates.get("forageBlocked") is True
               and isinstance(gates.get("eat"), (int, float))
               and abs(gates["eat"] - gates["expected"]) < 1e-6)
        ok &= check(rep, "entry_gates_unchanged", ok6,
                    "eat/forage entry gates unchanged",
                    f"eatUtility={gates.get('eat')} "
                    f"(formula {gates.get('expected')}) "
                    f"forage_blocked_by_carried_food="
                    f"{gates.get('forageBlocked')}", gates)

        rep.note("\n" + ("ALL MEAL-WASTE CHECKS PASSED" if ok else "SOME FAILED"))
        return 0 if ok else 1
    finally:
        quit_engine(port, proc)


if __name__ == "__main__":
    sys.exit(main())
