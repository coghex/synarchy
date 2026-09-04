#!/usr/bin/env python3
"""Session (b): local placement, the real portal roster, and discovery
driven by a real move order (#2164).

The part of the embark flow no other probe exercises. It reloads the
same portal-free `SAVE_BASE` session (a) used, places the canonical
local start, waits for the portal's own roster, then selects one
acolyte with a REAL `input.click` and orders it with a REAL right-click
— never `unitAi.commandMove` or `unit.setPos` — so the discovery it
observes arises from actual sight (#1770). It then walks the unit back
out of sight and in again to prove no duplicate event, photographs the
lifecycle icons against session (a)'s own baselines, and publishes
`SAVE_LOCAL`.

Its return value is that save's durability: session (c) reads the slot
from a fresh process, so the facade suppresses it when this returns
false (#1746).
"""
from __future__ import annotations

import os
import time

from probelib import clear_find_water, poll_until, send
from location_content_probe import load_defs
from offscreen_probe import (arm_portal_placement, building_count, center_on,
                             click_at_seed, find_widget, placement_mode,
                             png_differs, png_stats, screenshot, set_zoom,
                             zoom_fade_end)
from portal_ghost_probe import in_world_view
from .constants import FIXTURE_PAGE, PORTAL, SAVE_BASE, SAVE_LOCAL
from .invocation import SessionContext, check, save_and_wait
from .support import (MAX_SIGHT_TILES, discovery_events, ensure_armed,
                      ensure_mobile, find_safe_local_start,
                      list_locations_sorted, match_by_anchor, nearby_seeds,
                      order_move_to, require_selection, sees_location,
                      select_unit_via_click, unit_ids_by_def, unit_info,
                      wait_for_hud_settle)


def session_local_and_discovery(ctx: SessionContext):
    port, shots = ctx.port, ctx.shots
    w, h = ctx.w, ctx.h
    target, control = ctx.target, ctx.control
    cx0, cy0 = ctx.centre
    load_defs(port)
    send(port, f"require('scripts.main_menu').loadAndShowSave('{SAVE_BASE}'); return 'ok'")
    reached = poll_until(90.0, lambda: in_world_view(port))
    if not check("session b: reached world_view after loading the fixture save",
                 bool(reached)):
        return None
    wait_for_hud_settle(port)
    # A loaded save always resumes paused (the persistence contract's
    # load-time policy) — nothing simulates (building appear->built
    # transition, the roster spawn timer, unit AI) until unpaused.
    send(port, "engine.setPaused(false); return 'ok'")

    locs = list_locations_sorted(port)
    t = match_by_anchor(locs, target["gx"], target["gy"])
    c = match_by_anchor(locs, control["gx"], control["gy"])
    if not check("session b: target + control ruins re-derived after load",
                 bool(t) and bool(c)):
        return None
    # #1230: reveal is sight-based against the instance's own bounds.
    # Whether a unit can see the ruin is asked of the ENGINE
    # (`sees_location`); MAX_SIGHT_TILES is used only to pick a
    # walk-away DESTINATION comfortably past any possible sightline,
    # which is a distance this probe chooses rather than a boundary it
    # asserts.
    margin = MAX_SIGHT_TILES
    bounds = t["bounds"]

    # -- step 13: a valid, non-remote position outside every location's
    # bounds places instantly — no warning. --
    arm_portal_placement(port)
    local_hit = find_safe_local_start(port, nearby_seeds(t["gx"], t["gy"]), cx0, cy0)
    if not check("found a valid, non-remote, terrain-safe local-start position",
                 bool(local_hit)):
        return None
    lseed_gx, lseed_gy, lgx, lgy, _ldist, _lthr = local_hit
    print(f"  local portal position ({lgx},{lgy})")
    ensure_armed(port)
    before = building_count(port, PORTAL)
    check("click lands on the local position",
          click_at_seed(port, lseed_gx, lseed_gy, lgx, lgy, cx0, cy0))
    time.sleep(0.4)
    check("no remote-settlement modal appears for a local placement",
          not find_widget(port, "Establish Here"))
    check("exactly one portal placed locally, no confirmation needed",
          building_count(port, PORTAL) == before + 1,
          f"before={before} after={building_count(port, PORTAL)}")
    check("placement mode exited after the local placement",
          placement_mode(port) == "off")

    # -- step 14: allow the normal portal roster to start spawning. --
    before_ids = unit_ids_by_def(port, "acolyte")
    roster_uid = poll_until(
        45.0, lambda: next(iter(unit_ids_by_def(port, "acolyte") - before_ids), None))
    if not check("at least one roster acolyte spawned from the new portal",
                 roster_uid is not None):
        return None
    roster_uid = int(roster_uid)

    # -- steps 15-16: the spawned acolyte starts out of sight of the
    # target ruin, which is still undiscovered. --
    info = unit_info(port, roster_uid)
    if not check("roster acolyte position query answers", bool(info)):
        return None
    ux, uy = info["gridX"], info["gridY"]
    check("the spawned acolyte starts out of sight of the target ruin",
          not sees_location(port, roster_uid, bounds),
          f"unit at ({ux:.1f},{uy:.1f}), ruin bounds {bounds}")
    pre_move = match_by_anchor(list_locations_sorted(port), t["gx"], t["gy"]) or {}
    check("target ruin is still undiscovered before the unit approaches",
          not pre_move.get("discovered"))

    # A fresh roster acolyte carries the standing find_water goal every
    # unit.spawn'd unit gets; clear it so it can't out-compete the
    # order below (see probelib.clear_find_water's own gotcha note).
    clear_find_water(port, roster_uid)

    # The roster's own spawn-time formation walk (a short in-place
    # shuffle away from the portal, see building_spawn.lua) can still
    # collapse a unit on the terrain-safe site found above (a lesser,
    # recoverable risk than the lethal fall find_safe_local_start now
    # screens out) — wait it out before issuing the real order, exactly
    # as a real player would wait for their colonist to get back up.
    if not check("roster acolyte is alive and mobile before ordering it",
                  ensure_mobile(port, roster_uid),
                  f"info={unit_info(port, roster_uid)}"):
        return None

    # -- step 15 (cont'd): select + order via REAL input, not
    # unitAi.commandMove/unit.setPos. --
    outcome = select_unit_via_click(port, roster_uid, w, h)
    if not check("select the roster acolyte via a real click",
                 outcome.ok, outcome.describe(roster_uid)):
        # A blocking precondition, in the same form as this phase's
        # three above it (#1770). The move orders below carry no uid —
        # init_mouse.lua's right-click handler steers whatever is
        # selected — while every visibility poll here is pinned to
        # `roster_uid`, so with the selection unproven the orders and
        # the assertions can address different units. Ending here also
        # spends none of the 180s of poll budget watching a unit that
        # was never ordered, and returning None suppresses session (c)
        # exactly as a failed save does: no move, discovery,
        # visibility, icon or save work happens after this point.
        return None
    if not require_selection(port, roster_uid, "the approach move order"):
        return None
    move_resolved = order_move_to(port, t["gx"], t["gy"], cx0, cy0)
    check("real right-click move order resolves the target ruin's anchor tile",
          move_resolved == (t["gx"], t["gy"]), f"got {move_resolved}")

    # -- step 17: coming within sight discovers it exactly
    # once, attributed to this unit, with matching anchor coords. --
    discovered_flag = poll_until(90.0, lambda: next(
        (e.get("discovered") for e in list_locations_sorted(port)
         if e.get("gx") == t["gx"] and e.get("gy") == t["gy"]), False))
    check("approaching the target ruin (real move order) flips it to discovered",
          bool(discovered_flag))
    roster_ids = unit_ids_by_def(port, "acolyte") - before_ids
    evs = discovery_events(port)
    check("exactly one location_discovery event after the first approach",
          len(evs) == 1, f"got {evs}")
    if evs:
        # Attributed to SOME acolyte from this portal's roster, not
        # necessarily `roster_uid` itself: the other roster members are
        # left to their own ambient wander (unordered) rather than
        # frozen, so on a close local-start site it's the real,
        # unscripted AI — not this probe — that occasionally lets a
        # different roster acolyte see the ruin first. What must
        # stay true is that it's a real portal-spawned unit, not a
        # stray from elsewhere.
        check("discovery event attributed to a roster acolyte from this portal",
              evs[0].get("uid") in roster_ids, f"uid={evs[0].get('uid')}, roster={roster_ids}")
        check("discovery event carries the target ruin's anchor coords",
              evs[0].get("coords") == {"x": t["gx"], "y": t["gy"]},
              f"coords={evs[0].get('coords')}")

    # `location_discovery` is a popup-notification category
    # (data/notification_categories.yaml: popup: true) — the real
    # engine reaction to the event above spawns a screen-centred
    # notification card (scripts/popup.lua) that stays up until
    # dismissed. Left alone, it does two things a real player wouldn't
    # want either: sits in the middle of the icon-checkpoint screenshots
    # below, and — being a real pointer-blocking clickable card exactly
    # where this probe's move-order clicks land (screen centre) —
    # silently swallows every subsequent input.click before it ever
    # reaches scripts/init_mouse.lua's world-click routing, so step 19's
    # "walk out of sight" order below would otherwise never take effect.
    # Dismissing it directly (like `clear_find_water`, not a player
    # action under test) is the equivalent of a real player clicking
    # its OK button before continuing.
    send(port, "return require('scripts.popup').dismissAll()")

    # -- step 18: the un-approached control ruin stays undiscovered. --
    control_now = match_by_anchor(list_locations_sorted(port), c["gx"], c["gy"])
    check("the un-approached control ruin stays undiscovered",
          bool(control_now) and not control_now.get("discovered"))

    # -- checkpoint: one type icon + one unknown marker, using the
    # IDENTICAL camera centre + zoom formula session (a) used for its
    # pre-portal `icon_unknown_target.png`/`icon_unknown_control.png`
    # shots (same tempdir, same file names) — a terrain-stable baseline
    # per ruin, so the comparisons below isolate the ICON change alone
    # rather than "these are two different ruins on different terrain".
    # Zoom is saved/restored around this so step 19's move-order clicks
    # below resolve hover/click tiles under the same conditions as
    # ordinary gameplay rather than the coarse icon-inspection zoom
    # level. --
    play_zoom = send(port, "return camera.getZoom()")
    fade_end = zoom_fade_end(port)
    full_zoom = fade_end * 1.5
    shot_hidden_target = os.path.join(shots, "icon_unknown_target.png")
    center_on(port, t["gx"], t["gy"])
    set_zoom(port, full_zoom)
    time.sleep(0.3)
    shot_discovered = os.path.join(shots, "icon_discovered.png")
    if check("discovered-icon screenshot answers",
             screenshot(port, shot_discovered)):
        st = png_stats(shot_discovered)
        check("discovered-icon frame is not blank",
              bool(st) and st[2] >= 3, f"distinct colors: {st and st[2]}")
        check("target ruin's icon visibly changed from its own pre-portal "
              "unknown-marker baseline (same camera centre/zoom) once seen",
              png_differs(shot_hidden_target, shot_discovered, min_fraction=0.0002))
    center_on(port, c["gx"], c["gy"])
    set_zoom(port, full_zoom)
    time.sleep(0.3)
    shot_control_hidden = os.path.join(shots, "icon_control_still_hidden.png")
    if check("control (still-undiscovered)-icon screenshot answers",
             screenshot(port, shot_control_hidden)):
        check("discovered vs. still-undiscovered icon screenshots visibly differ",
              png_differs(shot_discovered, shot_control_hidden, min_fraction=0.0002))
    set_zoom(port, float(play_zoom))
    time.sleep(0.3)

    # -- step 19: move out of sight, then back in — no duplicate
    # discovery event. Exits along the +x edge, far enough that an
    # acolyte's ~6-tile sight radius certainly clears the ruin, rather
    # than all the way back to the portal, to keep the round trip short.
    # `sees_location` below is what actually PROVES it left view, so
    # this distance only has to be plausible, not conservative — padding
    # it out to the widest theoretical sightline just lengthens the
    # return leg past the poll budget. --
    outside_x = bounds["max_x"] + margin + 2
    outside_y = t["gy"]
    if not require_selection(port, roster_uid,
                             "the walk-out-of-sight move order"):
        return None
    resolved_out = order_move_to(port, outside_x, outside_y, cx0, cy0)
    check("real move order to walk out of sight resolves a tile",
          resolved_out is not None)
    left_margin = poll_until(
        90.0, lambda: not sees_location(port, roster_uid, bounds))
    check("the unit actually walks out of sight of the ruin", bool(left_margin))

    if not require_selection(port, roster_uid, "the return move order"):
        return None
    resolved_back = order_move_to(port, t["gx"], t["gy"], cx0, cy0)
    check("real move order back into the ruin resolves the anchor tile",
          resolved_back == (t["gx"], t["gy"]), f"got {resolved_back}")
    reentered = poll_until(
        90.0, lambda: sees_location(port, roster_uid, bounds))
    check("the unit comes back within sight of the ruin", bool(reentered))
    time.sleep(1.0)
    evs_again = discovery_events(port)
    check("leaving and returning emits no duplicate discovery event",
          len(evs_again) == 1, f"got {evs_again}")

    # -- step 20 (prep): save this session's world for the reload check.
    # Session (c) loads this slot from a FRESH process, so it may only
    # start once THIS request has reached its terminal phase (#1746);
    # the result is returned so the caller can suppress it otherwise. --
    return save_and_wait(port, FIXTURE_PAGE, SAVE_LOCAL, "session b")
