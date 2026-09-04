#!/usr/bin/env python3
"""Session (a): the icon state before any portal exists, ghost validity,
and the remote-settlement modal (#2164).

The first of the three offscreen sessions, and the only one that never
spawns a unit. It loads `SAVE_BASE`, photographs both ruins' shared
unknown markers, proves an ordinary position's neutral ghost and an
overlapping one's rejection, then drives the remote modal through both
of its branches — cancel, then confirm.

It deliberately never saves: the world it ends with holds a remote
portal, and session (b) needs the SAME portal-free fixture, so this
session's world is discarded with its engine.
"""
from __future__ import annotations

import os
import time

from probelib import poll_until, send
from location_content_probe import load_defs
from offscreen_probe import (arm_portal_placement, building_count,
                             can_place_at, center_on, click_at_seed,
                             click_widget, find_buildable, find_widget,
                             goto_and_resolve, placement_mode, png_differs,
                             png_stats, screenshot, set_zoom, zoom_fade_end)
from portal_ghost_probe import center_on_tile, in_world_view
from .constants import PORTAL, SAVE_BASE
from .invocation import SessionContext, check
from .support import (ensure_armed, list_locations_sorted, match_by_anchor,
                      nearby_seeds, remote_seeds, wait_for_hud_settle)


def session_ghost_and_remote(ctx: SessionContext) -> None:
    port, shots = ctx.port, ctx.shots
    target, control = ctx.target, ctx.control
    cx0, cy0 = ctx.centre
    load_defs(port)
    send(port, f"require('scripts.main_menu').loadAndShowSave('{SAVE_BASE}'); return 'ok'")
    reached = poll_until(90.0, lambda: in_world_view(port))
    if not check("session a: reached world_view after loading the fixture save",
                 bool(reached)):
        return
    wait_for_hud_settle(port)

    locs = list_locations_sorted(port)
    if not check("session a: fixture still lists both ruins after load",
                 len(locs) >= 2, f"found {len(locs)}"):
        return
    t = match_by_anchor(locs, target["gx"], target["gy"]) or locs[0]
    c = match_by_anchor(locs, control["gx"], control["gy"]) or locs[1]

    # -- steps 2-3: zoom to icon-visible level BEFORE any portal exists;
    # both ruins show the SHARED unknown marker (#1230). --
    fade_end = zoom_fade_end(port)
    full_zoom = fade_end * 1.5
    center_on(port, t["gx"], t["gy"])
    set_zoom(port, full_zoom)
    time.sleep(0.3)
    check("target ruin starts unknown", not t.get("discovered"))
    check("target ruin's lifecycle really is 'unknown', so the marker "
          "below is the shared unknown icon and not a later state",
          t.get("lifecycle") == "unknown", f"lifecycle={t.get('lifecycle')!r}")
    shot_hidden_target = os.path.join(shots, "icon_unknown_target.png")
    if check("unknown-marker (target) screenshot answers",
             screenshot(port, shot_hidden_target)):
        st = png_stats(shot_hidden_target)
        check("unknown-marker (target) frame is not blank",
              bool(st) and st[2] >= 3, f"distinct colors: {st and st[2]}")
    center_on(port, c["gx"], c["gy"])
    set_zoom(port, full_zoom)
    time.sleep(0.3)
    check("control ruin starts unknown", not c.get("discovered"))
    shot_hidden_control = os.path.join(shots, "icon_unknown_control.png")
    if check("unknown-marker (control) screenshot answers",
             screenshot(port, shot_hidden_control)):
        st = png_stats(shot_hidden_control)
        check("unknown-marker (control) frame is not blank",
              bool(st) and st[2] >= 3, f"distinct colors: {st and st[2]}")

    # -- step 4: arm portal placement. --
    arm_portal_placement(port)
    check("placement armed", placement_mode(port) == "placement")

    # -- step 5: an ordinary valid (non-overlapping) position renders a
    # neutral ghost — proven via canPlaceAt, the oracle that drives the
    # tint (portal_ghost_probe already proves the exact RGB direction). --
    ordinary_hit = find_buildable(port, PORTAL, nearby_seeds(t["gx"], t["gy"]),
                                   want_remote=False, screen_x=cx0, screen_y=cy0)
    shot_valid = None
    if check("found an ordinary valid buildable position", bool(ordinary_hit)):
        oseed_gx, oseed_gy, ogx, ogy, _odist, _othr = ordinary_hit
        hover_r = goto_and_resolve(port, oseed_gx, oseed_gy, cx0, cy0)
        check("camera resolves the ordinary position", hover_r == (ogx, ogy),
              f"got {hover_r}")
        send(port, f"return input.moveMouse({cx0}, {cy0})")
        time.sleep(0.3)
        valid_ok, _ = can_place_at(port, PORTAL, ogx, ogy)
        check(f"ordinary position ({ogx},{ogy}) reports canPlaceAt=true "
              f"(neutral ghost)", valid_ok)
        shot_valid = os.path.join(shots, "ghost_valid.png")
        check("valid-ghost screenshot answers", screenshot(port, shot_valid))

    # -- steps 6-7: a footprint overlapping the target ruin's bounds
    # renders a red (invalid) ghost, and clicking it spawns nothing
    # while placement stays armed. --
    inside = center_on_tile(port, t["gx"], t["gy"], cx0, cy0)
    if check("camera converges on the target ruin's anchor tile",
             inside == (t["gx"], t["gy"]), f"got {inside}, target ({t['gx']},{t['gy']})"):
        send(port, f"return input.moveMouse({cx0}, {cy0})")
        time.sleep(0.3)
        invalid_ok, reason = can_place_at(port, PORTAL, t["gx"], t["gy"])
        check("overlapping position reports canPlaceAt=false (red ghost)",
              not invalid_ok, f"reason={reason}")
        shot_invalid = os.path.join(shots, "ghost_invalid.png")
        if check("invalid-ghost screenshot answers", screenshot(port, shot_invalid)) \
                and shot_valid:
            check("valid vs. invalid ghost screenshots visibly differ",
                  png_differs(shot_valid, shot_invalid, min_fraction=0.0002))
        ensure_armed(port)
        before = building_count(port, PORTAL)
        send(port, f"return input.click({cx0}, {cy0})")
        time.sleep(0.4)
        check("clicking the overlapping position spawns no portal",
              building_count(port, PORTAL) == before,
              f"before={before} after={building_count(port, PORTAL)}")
        check("placement remains armed after the rejected click",
              placement_mode(port) == "placement")

    # -- steps 8-11: a valid, otherwise-remote position stays a neutral
    # ghost; clicking presents the remote-settlement modal; Choose
    # Another Site cancels cleanly (armed, nothing spawned); re-opening
    # and confirming Establish Here places exactly one portal remotely. --
    cx_avg = sum(e["gx"] for e in locs) / len(locs)
    cy_avg = sum(e["gy"] for e in locs) / len(locs)
    remote_hit = find_buildable(port, PORTAL, remote_seeds(cx_avg, cy_avg),
                                 want_remote=True, screen_x=cx0, screen_y=cy0)
    if not check("found a valid remote buildable position", bool(remote_hit)):
        return
    rseed_gx, rseed_gy, rgx, rgy, rdist, rthr = remote_hit
    print(f"  remote position ({rgx},{rgy}) distance={rdist} threshold={rthr}")
    hover_r2 = goto_and_resolve(port, rseed_gx, rseed_gy, cx0, cy0)
    check("camera resolves the remote position", hover_r2 == (rgx, rgy),
          f"got {hover_r2}")
    send(port, f"return input.moveMouse({cx0}, {cy0})")
    time.sleep(0.3)
    remote_valid, _ = can_place_at(port, PORTAL, rgx, rgy)
    check("remote position still reports canPlaceAt=true (neutral ghost, "
          "never red)", remote_valid)
    shot_remote_ghost = os.path.join(shots, "ghost_remote.png")
    check("remote-ghost screenshot answers", screenshot(port, shot_remote_ghost))

    ensure_armed(port)
    before = building_count(port, PORTAL)
    check("click lands on the remote position",
          click_at_seed(port, rseed_gx, rseed_gy, rgx, rgy, cx0, cy0))
    time.sleep(0.3)
    modal_up = poll_until(5.0, lambda: find_widget(port, "Establish Here"))
    check("clicking the remote position presents the remote-settlement modal",
          bool(modal_up))
    shot_modal = os.path.join(shots, "remote_modal.png")
    check("remote-modal screenshot answers", screenshot(port, shot_modal))
    check("no portal spawned while the modal is open",
          building_count(port, PORTAL) == before)

    check("click 'Choose Another Site'", click_widget(port, "Choose Another Site"))
    time.sleep(0.3)
    check("modal closed after Choose Another Site",
          not find_widget(port, "Establish Here"))
    check("still no portal spawned after cancel",
          building_count(port, PORTAL) == before)
    check("placement remains armed after cancel",
          placement_mode(port) == "placement")

    click_at_seed(port, rseed_gx, rseed_gy, rgx, rgy, cx0, cy0)
    modal_up2 = poll_until(5.0, lambda: find_widget(port, "Establish Here"))
    check("re-clicking the remote position re-presents the modal", bool(modal_up2))
    check("click 'Establish Here'", click_widget(port, "Establish Here"))
    time.sleep(0.5)
    check("exactly one portal placed after confirming remotely",
          building_count(port, PORTAL) == before + 1,
          f"before={before} after={building_count(port, PORTAL)}")
    check("placement mode exited after the confirmed remote placement",
          placement_mode(port) == "off")
    # Deliberately not saved: this session's world (one remote portal)
    # is discarded on quit, so session (b) reloads SAVE_BASE clean.
