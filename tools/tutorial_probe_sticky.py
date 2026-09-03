#!/usr/bin/env python3
"""The pre-latched sticky-presentation flow of `tools/tutorial_probe.py`
(#2145): a branch that latches BEFORE it is ever revealed, is shown
once, retires, and comes back only under the ordinary rule.

Stages 9-14 run on the third engine (`run_session`) and stage 15 on the
fourth (`run_reload`).

A separate boot and a separate acolyte on purpose: by the time the
ordinary flow reaches "Prepare an expedition", "Secure water source" has
already completed, so the composite is always revealed while it is still
incomplete there -- exactly the case the module's OWN hide-on-completion
rule already covered. Reproducing the bug needs the opposite order: the
unstripped spawn kit satisfies both prepare subobjectives (and so latches
the composite) before secure_water_source ever completes, since nothing
about carrying supplies has anything to do with discovering water.

What this owner proves, in order (#996, #1941, #2056):

  * the unstripped spawn kit latches the composite before it is revealed,
    and the checklist does not show it;
  * placing the portal leaves it hidden behind "Secure water source";
  * discovering real water reveals the already-completed branch rather
    than latching and hiding it in the same instant;
  * a REAL gameplay HUD over a COLLAPSED panel presents nothing, and an
    OPEN panel on a GPU-less engine still presents nothing -- the
    negative half of #2056, which is this probe's own to own;
  * an explicit `acknowledgePresented` retires the branch;
  * removing the supplies brings the RETIRED branch back under the
    ordinary live-check rule, and resupplying hides it again with no
    second presentation;
  * a fresh-process reload of that finished, retired session does not
    return any already-retired ancestor to the active checklist.

The POSITIVE presentation proof -- that the rows really do reach a
rendered frame -- is not here and must not move here: it belongs to
tools/tutorial_hud_probe.py, which runs `--offscreen` with a live
renderer and measures pixels.

The four HUD helpers below are this owner's alone; `hud_open` is the one
tutorial_hud reader both scenarios share and lives in
`tutorial_probe_contracts`. This module imports the ordinary owner not at
all, boots no engine, and touches no save slot.
"""
from __future__ import annotations

import os
import sys
import time

sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
from probelib import poll_until, send
from tutorial_probe_contracts import (OBJ_EXPEDITION, OBJ_PORTAL, OBJ_WATER,
                                      STICKY_SLOT, SUB_FOOD, SUB_WATER, Checks,
                                      ProbeError, hud_open, progress, settle)
from tutorial_probe_harness import (face_toward, find_fixture_site,
                                    give_ration, grant_ai_state,
                                    known_water_count, pin_daylight,
                                    place_portal_without_roster,
                                    refill_canteen, set_frozen, sight_failure,
                                    sight_snapshot, spawn_player_acolyte,
                                    strip_supplies, wait_for_spawn_kit,
                                    wait_for_teleport)
from tutorial_probe_setup import (load_through_barrier,
                                  prepare_generated_session,
                                  prepare_reload_session, save_through_barrier,
                                  set_paused)

#: How long the pre-latched leg waits for the acolyte's own FOV scan
#: to register the water. Named so the failure message cannot drift
#: away from the budget it reports on.
REVEAL_DISCOVERY_SECONDS = 60.0


# --------------------------------------------------------------------------
# The real gameplay HUD, this owner's alone
# --------------------------------------------------------------------------
def show_gameplay_hud(port: int) -> None:
    """Boot the REAL scripts/hud.lua and show it, so the checklist has a
    visible page to be presented on.

    #1941's acknowledgement is gated on `hud.visible` -- the flag
    scripts/hud.lua's own show() sets, and the one
    scripts/tutorial_hud.lua mirrors into the page visibility it paints
    by -- so a run that never boots the gameplay HUD can never present
    anything, which is exactly why every leg above this one leaves the
    branch sticky. Nothing here is stubbed: the module, its UI build and
    its show path are the shipped ones, reached the same way ui_manager
    reaches them. Its font and box textures are nil headless (there is
    no GPU font atlas), which leaves the rows unlabelled -- the viewport
    hit boxes that presentation is actually measured by are laid out
    either way.
    """
    got = send(port,
               "local hud = require('scripts.hud'); "
               "hud.init(nil, nil, 1280, 720); hud.createUI(); hud.show(); "
               "local th = package.loaded['scripts.tutorial_hud']; "
               "if th then th.reflow(1280, 720) end; "
               "return tostring(hud.visible)", timeout=60.0)
    if got != "true":
        raise ProbeError(f"scripts/hud.lua would not come up visible: {got!r}")


def open_checklist(port: int) -> list[str]:
    """Open the panel and report the rows THAT BUILD laid out, in ONE
    console chunk.

    Since #2056 the retirement can no longer overtake a second
    round-trip on this engine -- headless draws no frame, so nothing is
    acknowledged until this probe says so explicitly. The single chunk
    is kept anyway: it is the build itself talking, which is what the
    #996 assertion below wants to hear, and it stays correct on an
    engine that does render.
    """
    raw = send(port,
               "local th = package.loaded['scripts.tutorial_hud']; "
               "if not th then return '' end; "
               "th.setOpen(true); "
               "return table.concat(th.dump().rowIds, ',')", timeout=15.0)
    return [r for r in raw.split(",") if r]


def checklist_rows(port: int) -> list[str]:
    """The ids the tutorial panel has actually LAID OUT -- what the
    player is looking at, as opposed to what the model reports active.
    Empty while the panel is collapsed, by construction."""
    raw = send(port,
               "local th = package.loaded['scripts.tutorial_hud']; "
               "if not th then return '' end; "
               "return table.concat(th.dump().rowIds, ',')", timeout=15.0)
    return [r for r in raw.split(",") if r]


def hud_visible(port: int) -> str:
    return send(port,
                "local th = package.loaded['scripts.tutorial_hud']; "
                "if not th then return 'absent' end; "
                "return tostring(th.dump().hudVisible)", timeout=15.0)


# --------------------------------------------------------------------------
# Phases
# --------------------------------------------------------------------------
def phase_pre_latched_baseline(port: int, checks: Checks,
                               gx: int, gy: int) -> int:
    """One UNSTRIPPED acolyte at the camp, and the latch its spawn kit
    produces before anything has been revealed. Returns its uid."""
    uid = spawn_player_acolyte(port, gx + 2, gy)
    wait_for_spawn_kit(port, uid)
    grant_ai_state(port, [uid], "the spawned acolyte never received AI state")

    # tutorial_eval's own tick is what checks the subobjectives and
    # latches the composite -- it is not pause-gated, but it still needs
    # at least one tick after the AI state just materialized, so this
    # settles rather than reading a single snapshot.
    p = settle(port, lambda s: s.is_completed(OBJ_EXPEDITION))
    checks.check("the unstripped acolyte's spawn kit checks both prepare "
                 "subobjectives immediately", p.is_checked(SUB_WATER)
                 and p.is_checked(SUB_FOOD), str(p))
    checks.check("the composite latches before it is ever revealed",
                 p.is_completed(OBJ_EXPEDITION), str(p))
    checks.check("the composite is not yet observable -- neither the portal nor "
                 "the water objective has completed",
                 OBJ_EXPEDITION not in p.row_ids, str(p.row_ids))
    return uid


def phase_pre_latched_portal(port: int, checks: Checks, gx: int, gy: int) -> int:
    # No roster this time either: the probe's own acolyte is the only one
    # whose supplies matter, and a spawned roster would be provisioned too.
    bid = place_portal_without_roster(port, gx, gy)

    p = settle(port, lambda s: s.is_completed(OBJ_PORTAL))
    checks.check("placing the portal completes the portal objective",
                 p.is_completed(OBJ_PORTAL), str(p))
    checks.check("the already-latched composite still stays hidden behind "
                 "'Secure water source', which has not completed yet",
                 OBJ_EXPEDITION not in p.row_ids
                 and p.is_completed(OBJ_WATER) is False, str(p))
    return bid


def phase_pre_latched_reveal(port: int, checks: Checks, uid: int,
                             sx: int, sy: int, wx: int, wy: int) -> None:
    """The acolyte discovers real water -- secure_water_source completes,
    and the already-latched prepare branch is revealed for the FIRST
    time. The checklist must stay non-empty (#996's whole point).

    Sight here is SET UP, not hoped for (#1771). This leg drives the
    same night-aware FOV scan the main discovery path does, so it earns
    the same determinism the main path already establishes: the clock is
    pinned and its application waited for, the queued teleport is waited
    for, the finder is turned toward the water it is meant to find, and
    only then does the phase assert — positively, before unpausing —
    that the engine really does report THAT tile (not incidental water)
    in the unit's field of view.

    Both ways this can fail carry the state that classifies them, and
    neither spends the whole discovery budget to say so: a
    precondition that does not hold aborts immediately with the same
    snapshot a post-poll timeout would produce.
    """
    pin_daylight(port)
    send(port, f"unit.setPos({uid}, {sx}, {sy}); return 'ok'", timeout=15.0)
    landed = wait_for_teleport(port, uid, sx, sy)
    if landed:
        # Freeze first, then face — and stay frozen for the whole
        # discovery window. The freeze is what makes the facing stick
        # (see `face_toward`) AND what pins the position the FOV query
        # reads, so the field of view asserted below is the SAME field
        # of view the AI scans until the poll ends, rather than drifting
        # with a wander this leg has no reason to exercise. It is the
        # same instrument the main discovery path already uses on its
        # recipient, for the same reason: sight that a phase depends on
        # should be established, not hoped for.
        set_frozen(port, uid, True)
        face_toward(port, uid, sx, sy, wx, wy)
    before = sight_snapshot(port, uid, "before the discovery window",
                            (sx, sy), (wx, wy))
    if not landed:
        raise ProbeError(sight_failure(
            f"acolyte {uid} never landed on the shore tile ({sx},{sy})",
            before))
    if not checks.check("the acolyte can see the target water tile before the "
                        "discovery window opens", before.sees_target is True,
                        before.classify()):
        set_frozen(port, uid, False, required=False)
        raise ProbeError(sight_failure(
            f"acolyte {uid} cannot see the water ({wx},{wy}) it is expected "
            f"to discover", before))

    set_paused(port, False)
    found = poll_until(REVEAL_DISCOVERY_SECONDS,
                       lambda: known_water_count(port, uid) > 0)
    set_paused(port, True)
    # Snapshot BEFORE unfreezing, so a failure reports the pinned state
    # the unit actually held for the whole window rather than whatever
    # the sim publishes the instant the pin comes off.
    after = (None if found is not None
             else sight_snapshot(port, uid, "at the discovery timeout",
                                 (sx, sy), (wx, wy)))
    set_frozen(port, uid, False, required=False)
    if after is not None:
        raise ProbeError(sight_failure(
            f"acolyte {uid} never discovered the generated water "
            f"({wx},{wy}) within {REVEAL_DISCOVERY_SECONDS:.0f} s",
            before, after))

    p = settle(port, lambda s: s.is_completed(OBJ_WATER))
    checks.check("discovering water completes 'Secure water source'",
                 p.is_completed(OBJ_WATER), str(p))
    # place_portal/secure_water still leave the default checklist view
    # (they were revealed while still incomplete, so the ordinary
    # hide-on-completion rule applies to them unchanged); only the
    # already-latched prepare branch carries the #996 suppression -- and
    # only until it has been presented, which the next phase does.
    checks.check("the already-latched prepare branch is observable in authored "
                 "preorder, not an empty checklist (#996)",
                 p.active_row_ids == [OBJ_EXPEDITION, SUB_WATER, SUB_FOOD],
                 str(p.active_row_ids))
    checks.check("place_portal and secure_water are retained as completed "
                 "history, not re-shown in the active view",
                 OBJ_PORTAL in p.row_ids and OBJ_WATER in p.row_ids
                 and OBJ_PORTAL not in p.active_row_ids
                 and OBJ_WATER not in p.active_row_ids, str(p))
    row = p.row(OBJ_EXPEDITION) or {}
    checks.check("the composite renders active, with its normal completed marker",
                 row.get("active") is True and row.get("completed") is True,
                 str(row))
    checks.check("both prepare subobjectives render active and checked",
                 p.is_checked(SUB_WATER) and p.is_checked(SUB_FOOD)
                 and (p.row(SUB_WATER) or {}).get("active") is True
                 and (p.row(SUB_FOOD) or {}).get("active") is True, str(p))


def phase_pre_latched_presentation(port: int, checks: Checks, uid: int) -> None:
    """#1941: the #996 suppression is a LOAN, and this is where it is
    repaid -- as far as a GPU-LESS probe honestly can.

    #2056 split this phase in two, because this probe boots
    `--headless` (probelib.boot's default) and
    src/Engine/Loop/Headless.hs draws no frame at all. Acknowledgement
    is now gated on a completed RENDERER snapshot having held the rows,
    so on this engine it can never fire on its own -- and must not, or
    the gate would be certifying a presentation nobody could have seen.

    What this phase owns is therefore the MODEL INTEGRATION: the real
    hud, the real panel, the real viewport, the real retirement rule,
    and -- the strongest thing headless can say -- the proof that
    nothing retires while no frame is drawn. The presentation PROOF
    itself, that the rows really reach a frame, belongs to
    tools/tutorial_hud_probe.py, which runs `--offscreen` with a live
    renderer and measures pixels.

    The transition between the two halves is made EXPLICITLY, by
    calling #958's own acknowledgePresented with the ids the panel laid
    out. That is a test-only stand-in for the renderer, spelled out
    rather than waited for, so this phase's later checks (retirement,
    durable latches, the reversal leg that follows) still run against
    the shipped rule.
    """
    show_gameplay_hud(port)
    visible = hud_visible(port)
    checks.check("the tutorial checklist page is painted once the gameplay HUD "
                 "is showing", visible == "true", visible)

    # A visible HUD is not enough on its own: a COLLAPSED panel lays out
    # no rows, so ticking against it presents nothing. Give the update
    # tick a real window to get this wrong in.
    time.sleep(2.0)
    p = progress(port)
    checks.check("a collapsed panel presents nothing, however long the HUD is "
                 "visible -- the branch is still waiting",
                 OBJ_EXPEDITION in p.active_row_ids, str(p.active_row_ids))
    checks.check("and it really is collapsed", checklist_rows(port) == [],
                 str(checklist_rows(port)))

    shown = open_checklist(port)
    checks.check("opening the panel renders the whole already-latched branch "
                 "(the #996 guarantee, unchanged)",
                 OBJ_EXPEDITION in shown and SUB_WATER in shown
                 and SUB_FOOD in shown, str(shown))

    # #2056's headless half, and the reason this probe can no longer
    # simply wait: an open panel on a visible HUD is NOT presentation on
    # an engine that draws no frame. Give the update tick a long, honest
    # window to acknowledge anyway -- it must not.
    time.sleep(3.0)
    p = progress(port)
    checks.check("a GPU-less engine never fabricates presentation: the branch "
                 "is still waiting after seconds of ticks on a visible, OPEN "
                 "panel, because no frame has been drawn (#2056)",
                 OBJ_EXPEDITION in p.active_row_ids, str(p.active_row_ids))
    checks.check("and the rows are still laid out, waiting to be seen",
                 OBJ_EXPEDITION in checklist_rows(port), str(checklist_rows(port)))
    presented = send(port,
                     "local th = package.loaded['scripts.tutorial_hud']; "
                     "if not th then return 'no-module' end; "
                     "return tostring(th.isPresented())", timeout=15.0)
    checks.check("and the surface says so itself -- its viewport has not been "
                 "presented", presented.strip().strip('"') == "false", presented)

    # The explicit model transition. This stands in for the renderer
    # this engine does not have; tools/tutorial_hud_probe.py is where
    # the real thing is proven.
    acked = send(port,
                 "local tp = require('scripts.tutorial_progress'); "
                 "local th = package.loaded['scripts.tutorial_hud']; "
                 "if not th then return 'no-module' end; "
                 "local ids = th.dump().rowIds; "
                 "if #ids == 0 then return 'no-rows' end; "
                 "tp.acknowledgePresented(ids); "
                 "return table.concat(ids, ',')", timeout=15.0)
    checks.check("the test-only presentation stand-in reports the laid-out "
                 "branch to #958",
                 OBJ_EXPEDITION in acked and SUB_WATER in acked
                 and SUB_FOOD in acked, acked)

    p = settle(port, lambda s: OBJ_EXPEDITION not in s.active_row_ids,
               seconds=20.0)
    checks.check("having been reported presented, the branch retires from the "
                 "active checklist", OBJ_EXPEDITION not in p.active_row_ids,
                 str(p.active_row_ids))
    checks.check("its subobjective rows retire with it",
                 SUB_WATER not in p.active_row_ids
                 and SUB_FOOD not in p.active_row_ids, str(p.active_row_ids))
    checks.check("the checklist reaches its EMPTY completed state -- the "
                 "shipped session's terminal branch no longer pins it open",
                 p.active_row_ids == [], str(p.active_row_ids))
    checks.check("the durable completions are untouched by the retirement",
                 p.is_completed(OBJ_EXPEDITION) and p.is_completed(OBJ_PORTAL)
                 and p.is_completed(OBJ_WATER), str(p))
    checks.check("retirement is a display transition, not a supply change -- "
                 "the acolyte is still provisioned",
                 p.is_checked(SUB_WATER) and p.is_checked(SUB_FOOD), str(p))

    # poll_until answers on TRUTHINESS, and "empty" is the state being
    # waited for, so the sentinel is a marker rather than the list.
    settled = poll_until(10.0,
                         lambda: "empty" if checklist_rows(port) == [] else None)
    checks.check("the open panel itself ends up empty, not merely the model",
                 settled == "empty", str(checklist_rows(port)))


def phase_pre_latched_reversal(port: int, checks: Checks, uid: int) -> None:
    """Removing the supplies afterwards must still uncheck the live
    subobjectives, bring the branch back, and never touch the durable
    completion.

    Since #1941 this is the ORDINARY rule doing the work, not a
    suppression: the composite retired above, and it returns for exactly
    the reason any completed composite returns -- a live subobjective
    came back off. That is requirement 3, proven on the branch that
    started out sticky.
    """
    strip_supplies(port, uid)
    p = settle(port, lambda s: not s.is_checked(SUB_WATER))
    checks.check("removing the water unchecks the live water subobjective",
                 p.is_checked(SUB_WATER) is False, str(p))
    checks.check("removing the ration unchecks the live food subobjective",
                 p.is_checked(SUB_FOOD) is False, str(p))
    checks.check("the RETIRED branch returns to the active checklist under the "
                 "ordinary hide rule, showing the unchecked rows",
                 OBJ_EXPEDITION in p.active_row_ids
                 and SUB_WATER in p.active_row_ids
                 and SUB_FOOD in p.active_row_ids, str(p.active_row_ids))
    checks.check("the composite's durable completion is untouched",
                 p.is_completed(OBJ_EXPEDITION), str(p))


def phase_pre_latched_resupply(port: int, checks: Checks, uid: int) -> list[str]:
    """Put the supplies back so the branch is FINISHED again, then save.

    The state that goes to disk matters: a save taken with the
    subobjectives unchecked would come back with the composite
    legitimately active under the ordinary rule, and could not tell
    #1941's load reconstruction from #996's old permanent one. Saving a
    finished, retired branch is what makes the reload leg conclusive.
    """
    refill_canteen(port, uid)
    give_ration(port, uid, f"acolyte {uid} never got its ration back")

    p = settle(port, lambda s: s.active_row_ids == [], seconds=20.0)
    checks.check("re-satisfying the retired branch empties the checklist again, "
                 "with no second presentation needed",
                 p.active_row_ids == [], str(p.active_row_ids))

    before = p.completed
    save_through_barrier(port, checks, STICKY_SLOT,
                         "the retired-branch session saves through the real "
                         "save barrier")
    return before


def phase_pre_latched_reload(port: int, checks: Checks,
                             expected: list[str]) -> None:
    """#1941 requirement 4, in a FRESH PROCESS: a save whose tutorial was
    already finished must not put the ancestors the player watched
    retire back on the checklist.

    Presentation is deliberately never persisted, so the load has no
    history to restore -- it RECONSTRUCTS one, treating every id the
    restored durable set already makes structurally reveal-eligible as
    previously presented. The evaluator then re-checks both
    subobjectives against the same loaded world the save was taken from,
    which is precisely the tick that used to resurrect all five rows.
    """
    if not load_through_barrier(
            port, checks, STICKY_SLOT,
            "the retired-branch save loads and publishes in a fresh process"):
        return

    p = settle(port, lambda s: s.is_checked(SUB_WATER) and s.is_checked(SUB_FOOD),
               seconds=45.0)
    checks.check("every completed full objective survives the round trip",
                 sorted(p.completed) == sorted(expected),
                 f"{sorted(p.completed)} != {sorted(expected)}")
    checks.check("the acolyte's supplies came back, so the evaluator re-checks "
                 "both subobjectives",
                 p.is_checked(SUB_WATER) and p.is_checked(SUB_FOOD), str(p))
    open_state = hud_open(port)
    checks.check("the HUD comes back collapsed after a load", open_state == "false",
                 open_state)
    checks.check("the checklist stays EMPTY -- no already-retired ancestor is "
                 "returned to the active view (#1941)",
                 p.active_row_ids == [], str(p.active_row_ids))

    # Not a single-frame answer: hold it across further evaluation ticks,
    # since the defect this replaces was a tick recomputing the rows back.
    time.sleep(2.0)
    p = progress(port)
    checks.check("and it stays empty across further evaluation ticks",
                 p.active_row_ids == [], str(p.active_row_ids))
    checks.check("with every durable latch still intact underneath",
                 sorted(p.completed) == sorted(expected),
                 f"{sorted(p.completed)} != {sorted(expected)}")


# --------------------------------------------------------------------------
# The two legs, sequenced by the facade
# --------------------------------------------------------------------------
def run_session(port: int, checks: Checks, seed: int, size: int) -> list[str]:
    """Stages 9-14 on a freshly booted engine: latch the branch before it
    is revealed, present it, retire it, reverse it, resupply and save.
    Returns the completed-objective set the reload leg must find again."""
    prepare_generated_session(port, seed, size)

    (wx, wy), (sx, sy), (cx, cy) = find_fixture_site(port)

    print("== 9. an unstripped acolyte latches the composite before "
          "it is ever revealed ==")
    uid = phase_pre_latched_baseline(port, checks, cx, cy)

    print("== 10. placing the portal keeps the branch hidden behind "
          "'Secure water source' ==")
    phase_pre_latched_portal(port, checks, cx, cy)

    print("== 11. discovering water reveals the branch already "
          "complete ==")
    phase_pre_latched_reveal(port, checks, uid, sx, sy, wx, wy)

    print("== 12. presenting the branch on the real HUD retires it "
          "(#1941) ==")
    phase_pre_latched_presentation(port, checks, uid)

    print("== 13. removing supplies brings the RETIRED branch back "
          "under the ordinary hide rule ==")
    phase_pre_latched_reversal(port, checks, uid)

    print("== 14. re-supply, then save the finished, retired session ==")
    return phase_pre_latched_resupply(port, checks, uid)


def run_reload(port: int, checks: Checks, expected: list[str]) -> None:
    """Stage 15 on the fourth engine: the retired branch does not come
    back."""
    prepare_reload_session(port)
    print("== 15. an already-retired branch does not come back ==")
    phase_pre_latched_reload(port, checks, expected)
