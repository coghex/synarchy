#!/usr/bin/env python3
"""The engine reads and real-input gestures more than one session needs
(#2164).

Split out of `tools/location_embark_probe.py` unchanged. Two families
that share one module because both are "how a session talks to the
running engine", and every one of them is used by at least two of the
three sessions or by a session and the fixture:

  * locations, sight, event log, screenshot luminance and the
    deterministic coordinate searches the ghost/local/remote candidates
    come from;
  * unit lookup, the collapse/death predicates a real order has to wait
    out, the `hitTestInRect` bisection that finds a live unit's pixel,
    and the real click-select / right-click move-order gestures
    themselves (#1770).

A helper only one session uses stays with that session. Nothing here
boots an engine, opens a port, or records a failure of its own —
`check` is the facade's ledger, reached through `.invocation`.
"""
from __future__ import annotations

import json
import re
import time
from typing import NamedTuple

from probelib import poll_until, send, send_json
from offscreen_probe import (arm_portal_placement, find_buildable,
                             goto_and_resolve, placement_mode)
from portal_ghost_probe import center_on_tile, in_world_view
from .constants import FIXTURE_PAGE, PORTAL, RUIN_LABEL
from .invocation import check


# --------------------------------------------------------------------------
# Location/discovery helpers
# --------------------------------------------------------------------------
def list_locations_sorted(port: int, page: str = FIXTURE_PAGE) -> list[dict]:
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


#: Comfortably past the widest sightline this probe can produce
#: (#1230): a unit sees at most perception * awareRangeTiles (6.0)
#: tiles, the page-local night factor only shrinks that, and no shipped
#: unit carries a perception above 2.0. Used ONLY to pick a walk-away
#: DESTINATION far enough that the ruin certainly leaves view — whether
#: a unit can actually see it is asked of the engine (`sees_location`),
#: never approximated by a box.
MAX_SIGHT_TILES = 12


#: Half-width (logical pixels) of the square crop centred on the screen
#: that the icon-state checks below measure. The camera is centred on
#: the ruin's anchor and the icon draws at
#: World.Render.Zoom.Icons.locationIconTargetPixels (32 LOGICAL px), so a
#: crop this size contains the whole marker plus a little surrounding
#: terrain. Measuring a crop rather than the full frame matters: a 32px
#: icon is a vanishing fraction of a 1280x720 frame, and a full-frame
#: mean would drown the darkening under unchanged terrain.
ICON_CROP_HALF_PX = 40


def centre_luma(path: str, w: int, h: int) -> float | None:
    """Mean luminance of a square crop centred on the frame, over the
    OPAQUE pixels only. Returns None if unreadable or empty.

    Alpha-weighted rather than flat: the icon is a silhouette on a
    transparent background composited over terrain, and #1230 darkens
    RGB while leaving alpha exactly as supplied, so averaging the
    composited colour is what actually moves when the tint applies."""
    try:
        from PIL import Image
    except ImportError:
        return None
    try:
        with Image.open(path) as im:
            im = im.convert("RGB")
            # Screenshots come back at FRAMEBUFFER resolution, which on a
            # HiDPI display is a multiple of the logical size; scale the
            # crop by the real ratio rather than assuming 1:1.
            scale = im.width / float(w) if w else 1.0
            half = max(4, int(ICON_CROP_HALF_PX * scale))
            cx, cy = im.width // 2, im.height // 2
            box = (max(0, cx - half), max(0, cy - half),
                   min(im.width, cx + half), min(im.height, cy + half))
            crop = im.crop(box).convert("L")
            hist = crop.histogram()
            total = sum(hist)
            if not total:
                return None
            return sum(i * n for i, n in enumerate(hist)) / float(total)
    except Exception:
        return None


def sees_location(port: int, uid: int, bounds: dict) -> bool:
    """Whether unit `uid` can currently SEE any tile of `bounds` — the
    engine's own answer, not an approximation.

    This is literally the predicate `Location.Discovery.sightContactsWhere`
    tests (#1230): the unit's visible-tile set (`unit.getVisibleTiles`,
    the same night-aware radius/cone/occlusion calculation location
    reveal runs) intersected with the location's stored inclusive
    bounds. Asking the engine beats any radius box this probe could
    compute: a box has to be conservative enough to cover the widest
    possible perception, which then reports "in sight" for units that
    demonstrably are not — cone and terrain having ruled it out."""
    got = send(port,
               f"for _, t in ipairs(unit.getVisibleTiles({uid}) or {{}}) do "
               f"if t.x >= {bounds['min_x']} and t.x <= {bounds['max_x']} "
               f"and t.y >= {bounds['min_y']} and t.y <= {bounds['max_y']} "
               f"then return 'yes' end end; return 'no'", timeout=20.0)
    return (got or "").strip().strip('"') == "yes"


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
    terrain detours while staying out of sight of the ruin."""
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


def live_selection(port: int) -> tuple[set[int], str]:
    """The engine's CURRENT unit selection as a set of uids, beside the
    raw text `unit.getSelected()` answered with.

    The raw text is carried alongside deliberately: it is what the
    failure diagnostics below quote, and quoting the parsed set instead
    would hide the difference between an empty selection, a malformed
    reply and a console error."""
    raw = (send(port, "return unit.getSelected()") or "").strip()
    return {int(n) for n in re.findall(r"\d+", raw)}, raw


class SelectionOutcome(NamedTuple):
    """What a `select_unit_via_click` run actually observed, not just
    whether it worked.

    `observed` is the LAST `unit.getSelected()` reply the run saw, and
    `None` means no selection query was ever performed — the state a
    zero-attempt (or never-located) run ends in, which is a different
    fact from "the engine answered with an empty selection" and is
    reported as such (#1770)."""
    ok: bool
    observed: str | None
    clicks: int

    def describe(self, uid: int) -> str:
        if self.observed is None:
            return (f"requested uid {uid}; no selection query performed "
                    f"({self.clicks} click(s) issued)")
        return (f"requested uid {uid}; unit.getSelected() last returned "
                f"{self.observed!r} after {self.clicks} click(s)")


def select_unit_via_click(port: int, uid: int, w: int, h: int,
                           attempts: int = 5) -> SelectionOutcome:
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
    accepts and retries around for the unit's spawn-formation walk.

    Success is the selection being EXACTLY `uid`, the same predicate
    `require_selection` re-checks before every order below, so the two
    cannot drift: a plain left click on a unit routes to `unit.select`,
    which REPLACES the selection (`scripts/init_mouse_entity.lua:116-142`
    — only the Shift branch merges), so one uid is the real outcome of
    the gesture under test rather than a stricter reading of it.

    Returns a `SelectionOutcome` rather than a bare bool: the caller
    treats a failure as a blocking precondition and has to be able to
    report what the selection query answered with (#1770)."""
    observed: str | None = None
    clicks = 0
    for _ in range(attempts):
        pixel = locate_unit_pixel(port, uid, w, h)
        if not pixel:
            continue
        px, py = pixel
        send(port, f"return input.moveMouse({px}, {py})")
        send(port, f"return input.click({px}, {py}, 'left')")
        clicks += 1
        time.sleep(0.2)
        ids, observed = live_selection(port)
        if ids == {uid}:
            return SelectionOutcome(True, observed, clicks)
    return SelectionOutcome(False, observed, clicks)


def require_selection(port: int, uid: int, what: str) -> bool:
    """Gate a real right-click move order on the live selection being
    EXACTLY `uid`.

    `order_move_to` below takes no uid at all:
    `scripts/init_mouse_entity.lua`'s right-click handler reads the
    selection at dispatch time and orders EVERY selected unit, while this
    phase's visibility polls are pinned to one. Asserting the identity
    immediately before each order is what makes "the orders and the
    assertions concern the same unit" a checked fact for the whole
    phase rather than an assumption a stray click, a deselect or a
    death could quietly break (#1770)."""
    ids, raw = live_selection(port)
    return check(f"the live selection is exactly uid {uid} before {what}",
                 ids == {uid},
                 f"unit.getSelected() returned {raw!r}")


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
    destination must land inside a specific sight-radius
    box."""
    resolved = center_on_tile(port, target_gx, target_gy, cx0, cy0)
    if not resolved:
        return None
    send(port, f"return input.moveMouse({cx0}, {cy0})")
    send(port, f"return input.click({cx0}, {cy0}, 'right')")
    time.sleep(0.3)
    return resolved
