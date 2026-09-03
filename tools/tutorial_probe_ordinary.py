#!/usr/bin/env python3
"""The ordinary first-session progression of `tools/tutorial_probe.py`
(#2145): an empty session carried through the shipped first_session
branch to a fresh-process reload.

Stages 1-7 run on the first engine (`run_session`) and stage 8 on the
second (`run_reload`). What this owner proves, in order:

  * only the root objective is revealed in an empty session;
  * placing an acolyte_portal completes the portal objective and reveals
    "Secure water source";
  * one acolyte DISCOVERING generated water through its own FOV scan and
    SHARING it over the radio completes the water objective, with the
    recipient immobilized and blind so the share cannot be a second
    discovery;
  * water alone checks the water subobjective WITHOUT completing the
    composite, and a ration on the same acolyte then completes it;
  * stripping the supplies unchecks both live subobjectives and leaves
    the completed composite LATCHED;
  * a save/load round trip in a fresh process preserves every completed
    full objective, brings the HUD back collapsed, and recomputes the
    live checks from the LOADED world's inventory.

It imports the sticky owner not at all, boots no engine, and touches no
save slot: the facade owns all three.
"""
from __future__ import annotations

import os
import sys

sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
from probelib import poll_until, send
from tutorial_probe_contracts import (EXPEDITION_WATER_L, OBJ_EXPEDITION,
                                      OBJ_PORTAL, OBJ_WATER, SLOT, SUB_FOOD,
                                      SUB_WATER, Checks, hud_open, progress,
                                      settle)
from tutorial_probe_harness import (carried, find_fixture_site, grant_ai_state,
                                    give_ration, known_water_count,
                                    pin_daylight, place_portal_without_roster,
                                    refill_canteen, sees_water,
                                    spawn_player_acolyte, strip_supplies,
                                    wait_for_spawn_kit, water_sources)
from tutorial_probe_setup import (load_through_barrier,
                                  prepare_generated_session,
                                  prepare_reload_session,
                                  save_through_barrier, set_paused)


# --------------------------------------------------------------------------
# Phases
# --------------------------------------------------------------------------
def phase_baseline(port: int, checks: Checks) -> None:
    p = progress(port)
    checks.check("a fresh session has nothing completed", p.completed == [], str(p))
    checks.check("only the root objective is revealed at the start",
                 p.row_ids == [OBJ_PORTAL], str(p.row_ids))
    row = p.row(OBJ_PORTAL) or {}
    checks.check("the root objective reads as incomplete and active",
                 row.get("completed") is False and row.get("active") is True,
                 str(row))


def phase_portal(port: int, checks: Checks, gx: int, gy: int) -> int:
    # The roster is suppressed (see `place_portal_without_roster` and the
    # facade docstring): the probe supplies its own party so it controls
    # the supply state.
    bid = place_portal_without_roster(port, gx, gy)

    p = settle(port, lambda s: s.is_completed(OBJ_PORTAL))
    checks.check("placing an acolyte portal completes the portal objective",
                 p.is_completed(OBJ_PORTAL), str(p))
    checks.check("completing the portal objective reveals 'Secure water source'",
                 OBJ_WATER in p.row_ids, str(p.row_ids))
    checks.check("the water objective is revealed but not yet completed",
                 p.is_completed(OBJ_WATER) is False, str(p))
    checks.check("the expedition objective stays hidden behind the water objective",
                 OBJ_EXPEDITION not in p.row_ids, str(p.row_ids))
    return bid


def phase_party(port: int, checks: Checks, gx: int, gy: int) -> tuple[int, int]:
    """Two player acolytes at the camp, stripped of the spawn kit.

    Spawned while PAUSED, so scripts/unit_ai.lua has not created their AI
    state yet and the evaluator cannot see them at all — which is what
    lets the kit be shed before it is ever evaluated. One short unpaused
    window then grants the AI state the evaluator enumerates through.
    """
    finder = spawn_player_acolyte(port, gx, gy)
    mate = spawn_player_acolyte(port, gx + 1, gy)
    for uid in (finder, mate):
        wait_for_spawn_kit(port, uid)
        strip_supplies(port, uid)
        litres, rations = carried(port, uid)
        checks.check(f"acolyte {uid} sheds its spawn kit (0 L water, 0 rations)",
                     litres == 0.0 and rations == 0,
                     f"{litres} L, {rations} rations")

    grant_ai_state(port, [finder, mate],
                   "the spawned acolytes never received AI state")

    checks.check("neither stripped acolyte knows a water source yet",
                 known_water_count(port, finder) == 0
                 and known_water_count(port, mate) == 0,
                 f"{known_water_count(port, finder)} / "
                 f"{known_water_count(port, mate)}")
    p = progress(port)
    checks.check("a stripped party leaves the water objective incomplete",
                 p.is_completed(OBJ_WATER) is False, str(p))
    checks.check("a stripped party leaves the expedition objective incomplete",
                 p.is_completed(OBJ_EXPEDITION) is False, str(p))
    return finder, mate


def phase_discover_and_share(port: int, checks: Checks, finder: int, mate: int,
                             sx: int, sy: int) -> None:
    """The finder walks onto the shore, its FOV scan registers the water,
    and the radio broadcast hands it to the second acolyte.

    The recipient is FROZEN for the whole unpaused window. Without that
    it keeps its own active `find_water` goal and searches — over a
    120-second wait it can wander onto water and register a source by
    itself, which would satisfy "the recipient knows a source" even if
    the broadcast were completely broken. Freezing pins the published
    position `unitVisibleTiles` reads (`unit.setPos` is what moves it),
    so a recipient parked in the water-free camp cannot see, and
    therefore cannot scan, any water at all. `notify_allies`' radio
    branch writes straight into the recipient's AI state and applies no
    range or movement requirement, so the freeze costs the share leg
    nothing.
    """
    # #1230: unit.getVisibleTiles is night-aware, so both the finder's
    # "sees the pond it is standing on" and the recipient's "sees
    # nothing" now depend on the world clock. Pin it to noon — the
    # widest radius — so this phase asserts the same thing at every hour
    # a run can reach, and so a PASS on "the recipient is blind" is
    # earned by its position rather than by darkness.
    pin_daylight(port)
    blind_before = not sees_water(port, mate)
    checks.check("the recipient starts with no water anywhere in its own "
                 "field of view", blind_before)

    send(port, f"unit.setFrozen({mate}, true); return 'ok'", timeout=15.0)
    send(port, f"unit.setPos({finder}, {sx}, {sy}); return 'ok'", timeout=15.0)
    set_paused(port, False)
    found = poll_until(60.0, lambda: known_water_count(port, finder) > 0)
    shared = poll_until(120.0, lambda: known_water_count(port, mate) > 0)
    set_paused(port, True)
    # Read the recipient's view BEFORE unfreezing, so this reports the
    # pinned position it actually held for the whole window.
    blind_after = not sees_water(port, mate)
    send(port, f"unit.setFrozen({mate}, false); return 'ok'", timeout=15.0)

    checks.check("the acolyte on the shore DISCOVERS the generated water source",
                 found is not None,
                 f"knownWaterSources={known_water_count(port, finder)}")
    checks.check("the recipient never saw water itself while immobilized "
                 "(so any source it holds was TOLD to it)", blind_after)
    checks.check("the discovery is SHARED with the second acolyte over the radio",
                 shared is not None,
                 f"knownWaterSources={known_water_count(port, mate)}")
    finder_src, mate_src = water_sources(port, finder), water_sources(port, mate)
    checks.check("the shared source is the finder's own tile, not a second "
                 "discovery", bool(mate_src) and mate_src <= finder_src,
                 f"recipient {sorted(mate_src)} not a subset of finder "
                 f"{sorted(finder_src)}")

    p = settle(port, lambda s: s.is_completed(OBJ_WATER))
    checks.check("discovering and sharing water completes the water-source "
                 "objective", p.is_completed(OBJ_WATER), str(p))
    checks.check("completing the water objective reveals 'Prepare an expedition'",
                 OBJ_EXPEDITION in p.row_ids, str(p.row_ids))
    checks.check("the composite reveals both of its live subobjectives",
                 SUB_WATER in p.row_ids and SUB_FOOD in p.row_ids, str(p.row_ids))

    # The finder drinks and refills at the lake during that unpaused
    # window, so re-shed both acolytes before the stepwise restore. This
    # is also the first live-reversal check: a subobjective that went
    # true from real drinking must go false again when the water is gone.
    for uid in (finder, mate):
        strip_supplies(port, uid)
    p = settle(port, lambda s: not s.is_checked(SUB_WATER))
    checks.check("with no supplies carried, the water subobjective is unchecked",
                 p.is_checked(SUB_WATER) is False, str(p))
    checks.check("with no supplies carried, the food subobjective is unchecked",
                 p.is_checked(SUB_FOOD) is False, str(p))
    checks.check("an unsupplied party leaves the expedition objective incomplete",
                 p.is_completed(OBJ_EXPEDITION) is False, str(p))


def phase_supplies(port: int, checks: Checks, finder: int) -> None:
    """Restore the shed kit one item at a time and watch each answer."""
    refill_canteen(port, finder)
    litres, _ = carried(port, finder)
    checks.check(f"the acolyte now carries at least {EXPEDITION_WATER_L} L "
                 f"of water", litres >= EXPEDITION_WATER_L, f"{litres} L")

    p = settle(port, lambda s: s.is_checked(SUB_WATER))
    checks.check("carrying enough water checks the live water subobjective",
                 p.is_checked(SUB_WATER), str(p))
    checks.check("water alone does NOT check the food subobjective",
                 p.is_checked(SUB_FOOD) is False, str(p))
    checks.check("water alone does NOT complete 'Prepare an expedition'",
                 p.is_completed(OBJ_EXPEDITION) is False, str(p))

    give_ration(port, finder, f"acolyte {finder} never received its ration")

    p = settle(port, lambda s: s.is_checked(SUB_FOOD))
    checks.check("water AND a ration on the same acolyte checks the food "
                 "subobjective", p.is_checked(SUB_FOOD), str(p))
    checks.check("both subobjectives satisfied completes 'Prepare an expedition'",
                 p.is_completed(OBJ_EXPEDITION), str(p))
    # A fully-satisfied composite leaves the default active view (#958's
    # hide rule), taking its subobjective rows with it. Pinned here
    # because it is the ONE place the panel's rows and the durable state
    # legitimately disagree: the completion is permanent, the rows are not.
    checks.check("the satisfied composite drops out of the active view",
                 (p.row(OBJ_EXPEDITION) or {}).get("active") is False, str(p))
    checks.check("its subobjective rows go with it",
                 SUB_WATER not in p.row_ids and SUB_FOOD not in p.row_ids,
                 str(p.row_ids))


def phase_latch(port: int, checks: Checks, finder: int) -> None:
    """Removing the supplies afterwards must not untick the completion."""
    strip_supplies(port, finder)
    litres, rations = carried(port, finder)
    checks.check("the supplies are gone again",
                 litres == 0.0 and rations == 0, f"{litres} L, {rations} rations")

    p = settle(port, lambda s: not s.is_checked(SUB_WATER))
    checks.check("removing the water unchecks the live water subobjective",
                 p.is_checked(SUB_WATER) is False, str(p))
    checks.check("removing the ration unchecks the live food subobjective",
                 p.is_checked(SUB_FOOD) is False, str(p))
    checks.check("the completed 'Prepare an expedition' objective stays completed",
                 p.is_completed(OBJ_EXPEDITION), str(p))
    checks.check("the earlier full objectives stay completed too",
                 p.is_completed(OBJ_PORTAL) and p.is_completed(OBJ_WATER), str(p))


def phase_save(port: int, checks: Checks) -> list[str]:
    before = progress(port).completed
    save_through_barrier(port, checks, SLOT,
                         "the save completes through the real save barrier")
    return before


def phase_reload(port: int, checks: Checks, expected: list[str]) -> None:
    if not load_through_barrier(
            port, checks, SLOT,
            "the save loads and publishes in a fresh process"):
        return

    p = settle(port, lambda s: s.is_completed(OBJ_EXPEDITION), seconds=30.0)
    checks.check("every completed full objective survives the round trip",
                 sorted(p.completed) == sorted(expected),
                 f"{sorted(p.completed)} != {sorted(expected)}")
    open_state = hud_open(port)
    checks.check("the HUD comes back collapsed after a load",
                 open_state == "false", open_state)
    checks.check("the live subobjectives recompute from the LOADED world "
                 "(no supplies were saved, so both read unchecked)",
                 p.is_checked(SUB_WATER) is False
                 and p.is_checked(SUB_FOOD) is False, str(p))
    checks.check("the completed composite is active again with its unchecked rows",
                 (p.row(OBJ_EXPEDITION) or {}).get("active") is True
                 and SUB_WATER in p.row_ids and SUB_FOOD in p.row_ids, str(p))


# --------------------------------------------------------------------------
# The two legs, sequenced by the facade
# --------------------------------------------------------------------------
def run_session(port: int, checks: Checks, seed: int, size: int) -> list[str]:
    """Stages 1-7 on a freshly booted engine: build the ordinary session
    and save it. Returns the completed-objective set the reload leg must
    find again."""
    prepare_generated_session(port, seed, size)

    (wx, wy), (sx, sy), (cx, cy) = find_fixture_site(port)
    print(f"   generated water at ({wx},{wy}); shore ({sx},{sy}); "
          f"camp ({cx},{cy})")

    print("== 1. a fresh session shows only the first objective ==")
    phase_baseline(port, checks)

    print("== 2. place the acolyte portal ==")
    phase_portal(port, checks, cx, cy)

    print("== 3. a two-acolyte party, stripped of its spawn kit ==")
    finder, mate = phase_party(port, checks, cx, cy)

    print("== 4. discover real water, then share it over the radio ==")
    phase_discover_and_share(port, checks, finder, mate, sx, sy)

    print("== 5. restore the supplies one step at a time ==")
    phase_supplies(port, checks, finder)

    print("== 6. removing the supplies must not untick the completion ==")
    phase_latch(port, checks, finder)

    print("== 7. save ==")
    return phase_save(port, checks)


def run_reload(port: int, checks: Checks, expected: list[str]) -> None:
    """Stage 8 on the second engine: the fresh-process round trip."""
    prepare_reload_session(port)
    print("== 8. the round trip ==")
    phase_reload(port, checks, expected)
