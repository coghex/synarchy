#!/usr/bin/env python3
"""Embark-to-discovery end-to-end GPU probe (#782) — the final #159
locations-arc integration gate.

Boots the real offscreen graphical profile (GPU on, window off) and
drives the whole embark flow through the SAME player-facing paths a
real session uses: zoom-map icon inspection, build-tool ghost
validity, the remote-settlement confirmation modal, and — the part no
other probe exercises — discovery triggered by a REAL unit movement
ORDER issued through real input (click-select + right-click move),
never by a direct `unitAi.commandMove`/`unit.setPos` debug-console
call. Debug-console queries are used only to locate generated
locations, inspect authoritative state, pick test coordinates, and
verify results (`world.listPlacedLocations`, `building.canPlaceAt`,
`building.remoteCheck`, `unit.hitTestAt`, `unit.getInfo`,
`engine.getEventLog`) — every player-visible action (portal placement,
modal interaction, zoom-map viewing, unit selection, unit orders) goes
through `input.*`/`ui.dumpWidgets`.

This is deliberately NOT a re-derivation of ground already covered by
landed GPU probes:
  * tools/portal_ghost_probe.py already proves the exact white-vs-red
    ghost RGB tint direction and that an invalid click spawns nothing
    — this probe reuses `canPlaceAt` (the oracle that DRIVES that
    tint) plus screenshot-difference checks rather than re-deriving
    the pixel math.
  * tools/offscreen_probe.py's phases 6-7 already cover the remote
    modal's Choose-Another-Site/Establish-Here branches and paired
    discovery-state icons via a directly-spawned unit.
  * The pure "Location map icons" Hspec group proves the exact
    wrap/seam/upright icon geometry.

What's unique here is the single uninterrupted player-facing session:
inspecting BOTH ruins' hidden icons before any portal exists, the
overlap-rejection ghost, the remote-vs-local placement branches (in
two isolated sessions — the starting portal is unique, so a session
that already placed one remotely can't also exercise the canonical
local-start branch), the real portal roster spawn, discovery driven by
a real move ORDER (not a teleport), event-log assertions (exactly one
`location_discovery` event, none on re-entry), and persistence through
a real save -> quit -> fresh restart -> load.

Two phases:
  1. `--headless` (no GPU): generate a real world containing at least
     two `ruin_small` locations (retrying alternate seeds if the
     default seed doesn't place enough — changing world-generation
     location density is out of scope, so this is handled by seed
     selection, never by generation changes) and save it — the fixture
     both GPU sessions below load via the production Load Game path
     (`scripts.main_menu.loadAndShowSave`), so worldgen is paid once.
  2. `--offscreen` (GPU on, window off), THREE sessions against that
     one fixture:
       a. ghost validity + remote-modal cancel/confirm (never saved
          back over the fixture, so session (b) starts from the same
          clean, portal-free world);
       b. canonical local placement, the real portal roster, real
          click-select + right-click move-order discovery, the
          re-entry no-duplicate-event check, then a save under a new
          name;
       c. fresh restart -> load that save -> verify the discovered/
          undiscovered icon state and location count survived intact.

Needs a GPU (Vulkan device) — manual-only, never CI-gated, same as
tools/offscreen_probe.py / tools/portal_ghost_probe.py.

Usage:
  python3 tools/location_embark_probe.py
  python3 tools/location_embark_probe.py --seed 42 --size 64 --port 9420
"""
from __future__ import annotations

import argparse
import json
import os
import re
import sys
import tempfile
import time

sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
from probelib import boot, poll_until, quit_engine, send, send_json, clear_find_water
from location_content_probe import load_defs, gen_world, placed_ready
from offscreen_probe import (
    screenshot, png_stats, png_differs, widgets, find_widget, click_widget,
    can_place_at, remote_check, load_region_around, goto_and_resolve,
    find_buildable, arm_portal_placement, placement_mode, building_count,
    click_at_seed, zoom_fade_end, set_zoom, center_on,
)
from portal_ghost_probe import center_on_tile, in_world_view

LOG_PREP = "/tmp/location_embark_prep_engine.log"
LOG_S1 = "/tmp/location_embark_session1_engine.log"
LOG_S2 = "/tmp/location_embark_session2_engine.log"
LOG_S3 = "/tmp/location_embark_session3_engine.log"

PORTAL = "acolyte_portal"
RUIN_LABEL = "Small Ruin"  # data/locations/ruin_small.yaml `label`
SAVE_BASE = "location_embark_base"     # portal-free fixture, loaded by (a) and (b)
SAVE_LOCAL = "location_embark_local"   # (b)'s own save, reloaded by (c)

failures: list[str] = []
_current_log: list[str | None] = [None]


def set_log(path: str) -> None:
    _current_log[0] = path


def _tail(path: str, n: int = 15) -> str:
    try:
        with open(path) as f:
            return "".join(f.readlines()[-n:])
    except OSError:
        return "(log unavailable)"


def check(name: str, ok: bool, detail: str = "") -> bool:
    print(f"  [{'PASS' if ok else 'FAIL'}] {name}"
          + (f" — {detail}" if detail and not ok else ""))
    if not ok:
        failures.append(name if not detail else f"{name} — {detail}")
        if _current_log[0]:
            print(f"    recent engine log ({_current_log[0]}):")
            for line in _tail(_current_log[0]).splitlines():
                print(f"      {line}")
    return ok


# --------------------------------------------------------------------------
# Location/discovery helpers
# --------------------------------------------------------------------------
def list_locations_sorted(port: int, page: str = "main_world") -> list[dict]:
    raw = send(port, f"return world.listPlacedLocations('{page}')")
    try:
        data = json.loads(raw) if raw and raw not in ("nil", "null", "{}", "[]") else []
    except json.JSONDecodeError:
        data = []
    data = data if isinstance(data, list) else []
    # Deterministic ordering independent of Lua table-serialization
    # order, so target/control identity stays stable across sessions.
    return sorted(data, key=lambda e: (e.get("cx", 0), e.get("cy", 0)))


def match_by_anchor(locs: list[dict], gx: int, gy: int) -> dict | None:
    return next((e for e in locs if e.get("gx") == gx and e.get("gy") == gy), None)


def expand(bounds: dict, margin: int) -> dict:
    return {"min_x": bounds["min_x"] - margin, "min_y": bounds["min_y"] - margin,
            "max_x": bounds["max_x"] + margin, "max_y": bounds["max_y"] + margin}


def point_in_expanded(x: float, y: float, bounds: dict, margin: int) -> bool:
    eb = expand(bounds, margin)
    return eb["min_x"] <= x <= eb["max_x"] and eb["min_y"] <= y <= eb["max_y"]


def event_log(port: int) -> list[dict]:
    raw = send(port, "return engine.getEventLog()").strip()
    if not raw or raw in ("nil", "null", "{}", "[]"):
        return []
    try:
        data = json.loads(raw)
    except json.JSONDecodeError:
        return []
    return data if isinstance(data, list) else []


def discovery_events(port: int, label: str = RUIN_LABEL) -> list[dict]:
    text = f"Discovered: {label}"
    return [e for e in event_log(port)
            if e.get("category") == "location_discovery" and e.get("text") == text]


def nearby_seeds(gx: int, gy: int) -> list[tuple[int, int]]:
    """Offsets well clear of a 5x5 ruin's bounds + 6-tile discovery
    margin, but comfortably inside the 128-tile remote-portal
    threshold — reused for both an 'ordinary valid' probe position
    (session 1) and the canonical local-start position (session 2).

    Capped at 35 tiles (rather than the 90 an earlier revision used):
    session 2's local-start candidate is also the position a real
    roster acolyte must actually WALK from, ordered via
    `unitAi.commandMove`'s single-command budget
    (`unit_ai_core.lua`'s `TASK_TIMEOUT_SEC = 60.0`, arrival tolerance
    `TASK_ARRIVAL_TILES = 0.6`) — measured "ordered" travel speed is
    ~0.85 tiles/s, so a ~59-tile candidate (the old 50-90 range could
    produce one) leaves the unit still short of the target's discovery
    margin when the commanded task times out and control reverts to
    ambient wander, which never resumes the approach. 35 tiles leaves
    ample margin under the 51-tile ceiling for wander interruptions/
    terrain detours while staying outside the ruin's own bounds+margin."""
    return [
        (gx + dx, gy + dy)
        for dx, dy in ((20, 0), (-20, 0), (0, 20), (0, -20),
                       (28, 10), (-28, -10), (10, 28), (-10, -28),
                       (35, 0), (-35, 0), (0, 35), (0, -35))
    ]


def remote_seeds(cx: float, cy: float) -> list[tuple[int, int]]:
    """Offsets from the centroid of every placed ruin, at several
    radii in 8 compass directions — a genuinely remote (>128 tiles from
    the NEAREST location, not just this one) candidate depends on the
    real, seed-specific layout, so this casts a wide net rather than
    assuming fixed absolute coordinates."""
    dirs = ((1, 0), (-1, 0), (0, 1), (0, -1), (1, 1), (-1, -1), (1, -1), (-1, 1))
    return [(int(cx + d * dx), int(cy + d * dy))
            for d in (250, 400, 600, 800) for dx, dy in dirs]


# --------------------------------------------------------------------------
# Unit selection / real move-order helpers
# --------------------------------------------------------------------------
def unit_ids_by_def(port: int, def_name: str) -> set[int]:
    raw = send(port, "return unit.list()")
    return {int(m.group(1)) for m in
            re.finditer(r"id=(\d+)\s+" + re.escape(def_name) + r"\b", raw)}


def unit_info(port: int, uid: int) -> dict:
    r = send_json(port, f"return unit.getInfo({uid})")
    return r if isinstance(r, dict) else {}


def unit_is_dead(info: dict) -> bool:
    """True once a unit's own anim shows it's gone for good ('injured_death',
    'dead-idle', ...) — no amount of waiting recovers this, unlike an
    ordinary collapse (see `unit_incapacitated`)."""
    anim = (info.get("currentAnim") or "").lower()
    return "death" in anim or "dead" in anim


def unit_incapacitated(info: dict) -> bool:
    """True while a real move order would be dropped or ignored: this
    seed's terrain near the local-start position includes a real drop,
    and a roster acolyte walking it can fall and collapse — the SAME
    risk a real player's colonist runs. Checked so the probe can wait
    out a collapse and retry, rather than treating an occasional real
    fall as a probe failure. Death (see `unit_is_dead`) also counts here
    since it too means a command won't be acted on, but it's never worth
    waiting out."""
    anim = (info.get("currentAnim") or "").lower()
    return (bool(info.get("knockedDown")) or "collapse" in anim
            or "injured" in anim or unit_is_dead(info))


def ensure_mobile(port: int, uid: int, timeout: float = 60.0) -> bool:
    """Wait out a collapse (see `unit_incapacitated`) before issuing a
    new order — a real player would simply wait for their colonist to
    get back up too. Bails immediately (no point waiting out the full
    timeout) if the unit is dead outright."""
    if unit_is_dead(unit_info(port, uid)):
        return False
    return bool(poll_until(timeout, lambda: (
        lambda info: bool(info) and not unit_incapacitated(info))(unit_info(port, uid))))


def terrain_delta_around(port: int, gx: int, gy: int, radius: int = 3) -> float | None:
    """Max - min surfaceZ in a (2*radius+1)^2 box centred on (gx, gy), or
    None if nothing resolved (e.g. an ungenerated region). Building
    placement legality (`canPlaceAt`) says nothing about whether the
    ground AROUND a candidate site is safe for a freshly spawned roster's
    short in-place formation walk — this seed's terrain includes real
    cliffs, and an early local-start candidate right at the edge of one
    killed its entire roster (a lethal fall, not the recoverable collapse
    `unit_incapacitated` already anticipated) before any move order could
    land. Used to filter local-start candidates in
    `find_safe_local_start` below."""
    result = send_json(
        port,
        f"local minZ,maxZ=nil,nil; "
        f"for dx=-{radius},{radius} do for dy=-{radius},{radius} do "
        f"local sz=(world.getSurfaceAt({gx}+dx,{gy}+dy)); "
        f"if sz then if not minZ or sz<minZ then minZ=sz end; "
        f"if not maxZ or sz>maxZ then maxZ=sz end end end end; "
        f"return (minZ and maxZ) and (maxZ-minZ) or nil")
    return result if isinstance(result, (int, float)) else None


def find_safe_local_start(port: int, seeds, screen_x: int, screen_y: int,
                           max_delta: float = 15.0, radius: int = 3):
    """Like `find_buildable(..., want_remote=False)`, but also rejects a
    candidate whose surrounding terrain is too steep for a freshly
    spawned roster's formation walk to survive (`terrain_delta_around`) —
    only the LOCAL-start search needs this: it's the only candidate
    search in this probe whose result a real roster actually walks
    around on foot; the ghost/remote checks in session (a) never spawn
    a unit."""
    for seed in seeds:
        hit = find_buildable(port, PORTAL, [seed], want_remote=False,
                              screen_x=screen_x, screen_y=screen_y)
        if not hit:
            continue
        _, _, gx, gy, _, _ = hit
        delta = terrain_delta_around(port, gx, gy, radius)
        if delta is not None and delta <= max_delta:
            return hit
    return None


def _hit_test_in_rect_has(port: int, uid: int, x1: int, y1: int,
                           x2: int, y2: int) -> bool:
    raw = send(port, f"return unit.hitTestInRect({x1}, {y1}, {x2}, {y2})")
    ids = re.findall(r"\d+", raw)
    return str(uid) in ids


def locate_unit_pixel(port: int, uid: int, w: int, h: int,
                       max_steps: int = 14) -> tuple[int, int] | None:
    """Bisect `unit.hitTestInRect` down to the small screen-pixel box
    containing `uid`'s sprite quad, returning its centre pixel (or None
    if the unit isn't found on screen at all).

    A single `unit.hitTestAt(cx0, cy0)` at the raw screen centre — the
    approach a first attempt here took, mirroring how every other real
    click in this probe locates its target via `world.pickTile` — looks
    for a hit at ONE fixed pixel; it only lands on a moving/off-centre
    unit's actual (much smaller) sprite quad by coincidence.
    `camera.goToTile`/`world.pickTile` converge screen centre onto a
    whole TILE, not onto a specific unit's own fractional sub-tile
    position within it, so requiring an exact-centre hit is what made
    `unit.hitTestAt` alone look broken. `unit.hitTestInRect`, queried
    over successively smaller rects, reliably narrows onto the unit's
    real screen position instead (verified against a real `input.click`
    at the converged point actually landing in `unit.getSelected()`)."""
    x1, y1, x2, y2 = 0, 0, w, h
    if not _hit_test_in_rect_has(port, uid, x1, y1, x2, y2):
        return None
    for _ in range(max_steps):
        if x2 - x1 <= 2 and y2 - y1 <= 2:
            break
        mx, my = (x1 + x2) // 2, (y1 + y2) // 2
        quadrants = [(x1, y1, mx, my), (mx, y1, x2, my),
                     (x1, my, mx, y2), (mx, my, x2, y2)]
        for qx1, qy1, qx2, qy2 in quadrants:
            if _hit_test_in_rect_has(port, uid, qx1, qy1, qx2, qy2):
                x1, y1, x2, y2 = qx1, qy1, qx2, qy2
                break
        else:
            # The unit moved out from under the shrinking search box
            # between hitTestInRect calls (it's a live, simulating
            # unit) — the last confirmed box is still a usable, if
            # coarser, click target.
            break
    return (x1 + x2) // 2, (y1 + y2) // 2


def select_unit_via_click(port: int, uid: int, w: int, h: int,
                           attempts: int = 5) -> bool:
    """Select the roster acolyte via a REAL `input.click`, located by
    `locate_unit_pixel` and confirmed via `unit.getSelected()` — the
    same player-facing left-click path `scripts/init_mouse.lua` routes
    to `unit.select` internally.

    Retries the WHOLE bisection from a fresh full-screen box on a
    failed attempt: `uid` is a live, ambient-wandering roster acolyte,
    so a single bisection run can lose track of it mid-search (drifting
    out from under the shrinking box between successive
    `hitTestInRect` round trips) and converge on a stale pixel a real
    click then misses — the same risk `ensure_mobile` above already
    accepts and retries around for the unit's spawn-formation walk."""
    for _ in range(attempts):
        pixel = locate_unit_pixel(port, uid, w, h)
        if not pixel:
            continue
        px, py = pixel
        send(port, f"return input.moveMouse({px}, {py})")
        send(port, f"return input.click({px}, {py}, 'left')")
        time.sleep(0.2)
        selected = send(port, "return unit.getSelected()")
        if str(uid) in re.findall(r"\d+", selected):
            return True
    return False


def wait_for_hud_settle(port: int, seconds: float = 3.0) -> None:
    """`ui_manager.currentMenu` (what `in_world_view` polls) flips to
    'world_view' one engine tick BEFORE `scripts/loading_screen.lua`'s
    own finalization runs `hud.show` -> `resetMainWorldToolIfDirty` ->
    `selectDefaultTool` on a LATER tick of its update() loop. That
    reset routes through the exact same `onChange` -> `world.
    setToolMode` -> `buildTool.onToolMode('tool_default')` ->
    `buildTool.exitPlacement()` chain a real toolbar click uses — so
    arming placement before this one-time handoff settles gets silently
    reset the instant it lands, indistinguishable from a real player
    toggling the toolbar. A real player can't race this (nothing is
    clickable before the HUD finishes showing), so this is a probe-side
    sequencing fix, not a gameplay defect. A fixed settle window is
    cheap (once per session, not per action) and avoids depending on
    the loading screen's own internal tick cadence."""
    time.sleep(seconds)


def ensure_armed(port: int) -> None:
    """Defensive re-arm right before a click that depends on placement
    mode: cheap when already armed, and a safety net beyond the
    one-time settle in `wait_for_hud_settle` for the same underlying
    race."""
    if placement_mode(port) != "placement":
        arm_portal_placement(port)
        time.sleep(0.2)


def order_move_to(port: int, target_gx: int, target_gy: int, cx0: int, cy0: int):
    """Right-click a real player move-order at whatever tile the camera
    +cursor actually resolve for (target_gx, target_gy) — the SAME
    `world.pickTile`-driven resolution scripts/init_mouse.lua's real
    right-click handler uses, routing through `unitAi.commandMove` for
    every currently-selected unit. Returns the resolved tile, or None.

    Uses `center_on_tile`'s ITERATIVE convergence (not the single-shot
    `goto_and_resolve` most hover checks use elsewhere in this probe) —
    a single-shot resolution can land several tiles off the requested
    target (elevation-dependent isometric projection), which is fine
    for "any valid tile will do" checks but not here, where the
    destination must land inside a specific ~17-tile discovery-margin
    box."""
    resolved = center_on_tile(port, target_gx, target_gy, cx0, cy0)
    if not resolved:
        return None
    send(port, f"return input.moveMouse({cx0}, {cy0})")
    send(port, f"return input.click({cx0}, {cy0}, 'right')")
    time.sleep(0.3)
    return resolved


# --------------------------------------------------------------------------
# Phase 1: headless fixture prep
# --------------------------------------------------------------------------
def prepare_fixture(port: int, seeds: list[int], size: int, min_ruins: int = 2,
                     page: str = "ew"):
    """Try each seed in turn until one places >= min_ruins ruin_small
    locations, then save it as SAVE_BASE. Returns (seed, ruins) or
    (None, []) if every candidate seed falls short — a fail-fast
    diagnostic, never a silent generation-density change (out of
    scope per the issue)."""
    for candidate in seeds:
        set_log(LOG_PREP)
        proc = boot(port, log=LOG_PREP, label=f"prep engine (seed {candidate})")
        try:
            load_defs(port)
            gen_world(port, page, candidate, size)
            ruins = [e for e in placed_ready(port)
                     if e.get("id") == "ruin_small" and "bounds" in e]
            print(f"  seed {candidate}: {len(ruins)} ruin_small placed")
            if len(ruins) >= min_ruins:
                send(port, f"engine.saveWorld('{page}', '{SAVE_BASE}'); return 'saved'")
                time.sleep(1.0)
                return candidate, ruins
        finally:
            quit_engine(port, proc)
    return None, []


# --------------------------------------------------------------------------
# Session (a): zoom-map icons before any portal, ghost validity,
# remote-modal cancel/confirm.
# --------------------------------------------------------------------------
def session_ghost_and_remote(port: int, w: int, h: int, shots: str,
                              target: dict, control: dict) -> None:
    cx0, cy0 = w // 2, h // 2
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
    # both ruins show their undiscovered icon. --
    fade_end = zoom_fade_end(port)
    full_zoom = fade_end * 1.5
    center_on(port, t["gx"], t["gy"])
    set_zoom(port, full_zoom)
    time.sleep(0.3)
    check("target ruin starts undiscovered", not t.get("discovered"))
    shot_hidden_target = os.path.join(shots, "icon_hidden_target.png")
    if check("hidden-icon (target) screenshot answers",
             screenshot(port, shot_hidden_target)):
        st = png_stats(shot_hidden_target)
        check("hidden-icon (target) frame is not blank",
              bool(st) and st[2] >= 3, f"distinct colors: {st and st[2]}")
    center_on(port, c["gx"], c["gy"])
    set_zoom(port, full_zoom)
    time.sleep(0.3)
    check("control ruin starts undiscovered", not c.get("discovered"))
    shot_hidden_control = os.path.join(shots, "icon_hidden_control.png")
    if check("hidden-icon (control) screenshot answers",
             screenshot(port, shot_hidden_control)):
        st = png_stats(shot_hidden_control)
        check("hidden-icon (control) frame is not blank",
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


# --------------------------------------------------------------------------
# Session (b): canonical local placement, real portal roster, real
# click-select + right-click move-order discovery, save.
# --------------------------------------------------------------------------
def session_local_and_discovery(port: int, w: int, h: int, shots: str,
                                 target: dict, control: dict):
    cx0, cy0 = w // 2, h // 2
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
    margin = t.get("discovery_margin", 6)
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

    # -- steps 15-16: the spawned acolyte starts outside the target
    # ruin's discovery margin, which is still undiscovered. --
    info = unit_info(port, roster_uid)
    if not check("roster acolyte position query answers", bool(info)):
        return None
    ux, uy = info["gridX"], info["gridY"]
    check("the spawned acolyte starts outside the target ruin's discovery margin",
          not point_in_expanded(ux, uy, bounds, margin),
          f"unit at ({ux:.1f},{uy:.1f}), expanded bounds {expand(bounds, margin)}")
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
    check("select the roster acolyte via a real click",
          select_unit_via_click(port, roster_uid, w, h))
    move_resolved = order_move_to(port, t["gx"], t["gy"], cx0, cy0)
    check("real right-click move order resolves the target ruin's anchor tile",
          move_resolved == (t["gx"], t["gy"]), f"got {move_resolved}")

    # -- step 17: entering the expanded margin discovers it exactly
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
        # different roster acolyte reach the margin first. What must
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
    # "exit the margin" order below would otherwise never take effect.
    # Dismissing it directly (like `clear_find_water`, not a player
    # action under test) is the equivalent of a real player clicking
    # its OK button before continuing.
    send(port, "return require('scripts.popup').dismissAll()")

    # -- step 18: the un-approached control ruin stays undiscovered. --
    control_now = match_by_anchor(list_locations_sorted(port), c["gx"], c["gy"])
    check("the un-approached control ruin stays undiscovered",
          bool(control_now) and not control_now.get("discovered"))

    # -- checkpoint: one discovered + one undiscovered icon, using the
    # IDENTICAL camera centre + zoom formula session (a) used for its
    # pre-portal `icon_hidden_target.png`/`icon_hidden_control.png`
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
    shot_hidden_target = os.path.join(shots, "icon_hidden_target.png")
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
              "hidden-icon baseline (same camera centre/zoom) once discovered",
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

    # -- step 19: move out past the margin, then back in — no duplicate
    # discovery event. Exits along the +x edge, just past the margin,
    # rather than all the way back to the portal, to keep the round
    # trip short. --
    outside_x = bounds["max_x"] + margin + 8
    outside_y = t["gy"]
    resolved_out = order_move_to(port, outside_x, outside_y, cx0, cy0)
    check("real move order to exit the margin resolves a tile",
          resolved_out is not None)
    left_margin = poll_until(60.0, lambda: not point_in_expanded(
        unit_info(port, roster_uid).get("gridX", 0.0),
        unit_info(port, roster_uid).get("gridY", 0.0), bounds, margin))
    check("the unit actually leaves the discovery margin", bool(left_margin))

    resolved_back = order_move_to(port, t["gx"], t["gy"], cx0, cy0)
    check("real move order back into the ruin resolves the anchor tile",
          resolved_back == (t["gx"], t["gy"]), f"got {resolved_back}")
    reentered = poll_until(60.0, lambda: point_in_expanded(
        unit_info(port, roster_uid).get("gridX", -1e9),
        unit_info(port, roster_uid).get("gridY", -1e9), bounds, margin))
    check("the unit re-enters the discovery margin", bool(reentered))
    time.sleep(1.0)
    evs_again = discovery_events(port)
    check("leaving and returning emits no duplicate discovery event",
          len(evs_again) == 1, f"got {evs_again}")

    # -- step 20 (prep): save this session's world for the reload check. --
    check("save this session's world", "true" in send(
        port, f"engine.saveWorld('main_world', '{SAVE_LOCAL}'); "
              f"return 'true'").lower())
    time.sleep(0.5)
    return True


# --------------------------------------------------------------------------
# Session (c): fresh restart -> load -> verify persistence.
# --------------------------------------------------------------------------
def session_reload_check(port: int, w: int, h: int, shots: str,
                          target: dict, control: dict, expected_total: int) -> None:
    cx0, cy0 = w // 2, h // 2
    load_defs(port)
    send(port, f"require('scripts.main_menu').loadAndShowSave('{SAVE_LOCAL}'); return 'ok'")
    reached = poll_until(90.0, lambda: in_world_view(port))
    if not check("session c: reached world_view after loading the saved session",
                 bool(reached)):
        return

    locs = list_locations_sorted(port)
    check("no locations were duplicated by the save -> quit -> restart -> "
          "load round-trip", len(locs) == expected_total,
          f"expected {expected_total}, got {len(locs)}")
    t = match_by_anchor(locs, target["gx"], target["gy"])
    c = match_by_anchor(locs, control["gx"], control["gy"])
    check("target ruin still known after reload", bool(t))
    check("control ruin still known after reload", bool(c))
    if t:
        check("target ruin remains discovered after save -> quit -> restart -> load",
              bool(t.get("discovered")))
    if c:
        check("control ruin remains undiscovered after save -> quit -> "
              "restart -> load", not c.get("discovered"))

    # -- step 21 checkpoint: restored icons after save/load, checked
    # against the SAME terrain-stable per-ruin baselines session (a)
    # captured pre-portal (identical camera centre/zoom, same tempdir) —
    # not just against each other. --
    fade_end = zoom_fade_end(port)
    full_zoom = fade_end * 1.5
    shot_hidden_target = os.path.join(shots, "icon_hidden_target.png")
    shot_hidden_control = os.path.join(shots, "icon_hidden_control.png")
    center_on(port, target["gx"], target["gy"])
    set_zoom(port, full_zoom)
    time.sleep(0.3)
    shot_t = os.path.join(shots, "icon_reloaded_target.png")
    if check("reloaded target-icon screenshot answers", screenshot(port, shot_t)):
        st = png_stats(shot_t)
        check("reloaded target-icon frame is not blank",
              bool(st) and st[2] >= 3, f"distinct colors: {st and st[2]}")
        check("reloaded target ruin's icon still differs from its own "
              "pre-portal hidden-icon baseline (discovered state survived "
              "save -> quit -> restart -> load)",
              png_differs(shot_hidden_target, shot_t, min_fraction=0.0002))
    center_on(port, control["gx"], control["gy"])
    set_zoom(port, full_zoom)
    time.sleep(0.3)
    shot_c = os.path.join(shots, "icon_reloaded_control.png")
    if check("reloaded control-icon screenshot answers", screenshot(port, shot_c)):
        check("the reloaded discovered and undiscovered icons render differently",
              png_differs(shot_t, shot_c, min_fraction=0.0002))
        check("reloaded control ruin's icon still matches its own pre-portal "
              "hidden-icon baseline (undiscovered state survived "
              "save -> quit -> restart -> load)",
              not png_differs(shot_hidden_control, shot_c, min_fraction=0.0002))


def main() -> int:
    ap = argparse.ArgumentParser(description=__doc__)
    ap.add_argument("--seed", type=int, default=42)
    ap.add_argument("--alt-seeds", default="7,99,123,2026",
                    help="comma-separated fallback seeds tried in order if "
                         "--seed doesn't place >= 2 ruin_small locations")
    ap.add_argument("--size", type=int, default=64)
    ap.add_argument("--port", type=int, default=9420)
    ap.add_argument("--win-size", default="1280x720")
    args = ap.parse_args()

    w, h = (int(v) for v in args.win_size.lower().split("x"))
    shots = tempfile.mkdtemp(prefix="location_embark_probe_")
    seeds = [args.seed] + [int(s) for s in args.alt_seeds.split(",") if s.strip()]

    print(f"== phase 0: headless fixture prep (size {args.size}) ==")
    used_seed, ruins = prepare_fixture(args.port, seeds, args.size)
    if not check("a candidate seed placed at least two ruin_small locations",
                  len(ruins) >= 2,
                  f"tried seeds {seeds}, best count {len(ruins)}"):
        return report(shots)
    print(f"  fixture ready: seed={used_seed}, {len(ruins)} ruin(s), "
          f"saved as '{SAVE_BASE}'")

    ruins_sorted = sorted(ruins, key=lambda e: (e["cx"], e["cy"]))
    target, control = ruins_sorted[0], ruins_sorted[1]
    expected_total = len(ruins)

    print("== session (a): zoom-map icons, ghost validity, remote-modal flow ==")
    set_log(LOG_S1)
    proc1 = boot(args.port, mode=("--offscreen",), args=["--size", args.win_size],
                 log=LOG_S1, label="offscreen engine (session a)")
    try:
        session_ghost_and_remote(args.port, w, h, shots, target, control)
    finally:
        quit_engine(args.port, proc1)

    print("== session (b): local placement, roster, real-order discovery, save ==")
    set_log(LOG_S2)
    proc2 = boot(args.port, mode=("--offscreen",), args=["--size", args.win_size],
                 log=LOG_S2, label="offscreen engine (session b)")
    try:
        session_local_and_discovery(args.port, w, h, shots, target, control)
    finally:
        quit_engine(args.port, proc2)

    print("== session (c): fresh restart -> load -> verify persistence ==")
    set_log(LOG_S3)
    proc3 = boot(args.port, mode=("--offscreen",), args=["--size", args.win_size],
                 log=LOG_S3, label="offscreen engine (session c)")
    try:
        session_reload_check(args.port, w, h, shots, target, control, expected_total)
    finally:
        quit_engine(args.port, proc3)

    return report(shots)


def report(shots: str) -> int:
    print(f"\nscreenshots kept in {shots}")
    print("-" * 56)
    if failures:
        for f in failures:
            print(f"FAIL: {f}", file=sys.stderr)
        print(f"location_embark_probe: {len(failures)} check(s) FAILED")
        return 1
    print("location_embark_probe: all checks passed")
    return 0


if __name__ == "__main__":
    sys.exit(main())
