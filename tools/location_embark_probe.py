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

Every artifact this probe creates — the four engine logs, the two save
slots, and the screenshots — lives under ONE directory this invocation
owns (#1569), and the run deletes that directory again whether it
passes or fails. The engines boot with that directory's own resource
root, so the save slots below are unreachable from an ordinary
`cabal run` and the developer's live `saves/` is neither written nor
rotated. `--keep-artifacts` is the explicit opt-in that retains the
directory instead, for diagnosing a failure.

Usage:
  python3 tools/location_embark_probe.py
  python3 tools/location_embark_probe.py --seed 42 --size 64 --port 9420
  python3 tools/location_embark_probe.py --keep-artifacts
"""
from __future__ import annotations

import argparse
import json
import os
import re
import shutil
import stat
import sys
import tempfile
import time
import traceback
from typing import NamedTuple

sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
from probelib import (boot, capture_request_id, clear_find_water, poll_until,
                      quit_engine, send, send_json, wait_save_complete)
from location_content_probe import load_defs, gen_world, placed_ready
from offscreen_probe import (
    screenshot, png_stats, png_differs, widgets, find_widget, click_widget,
    can_place_at, remote_check, load_region_around, goto_and_resolve,
    find_buildable, arm_portal_placement, placement_mode, building_count,
    click_at_seed, zoom_fade_end, set_zoom, center_on,
)
from portal_ghost_probe import center_on_tile, in_world_view
from probe_runner_diagnostics import FailureEmitter   # noqa: E402 - durable failure records (#1982)

REPO = os.path.dirname(os.path.dirname(os.path.abspath(__file__)))

PORTAL = "acolyte_portal"
RUIN_LABEL = "Small Ruin"  # data/locations/ruin_small.yaml `label`
# The world page phase 0 generates the fixture on. A load keeps every
# saved page's OWN id (#763: no main_world remap), so every session below
# must address this same page -- both are named from this one constant so
# they cannot drift apart again.
FIXTURE_PAGE = "ew"

#: The two save slots, named inside THIS invocation's own resource root
#: (below) rather than the developer's live one. Uniqueness is a
#: property of the complete path, not of the key: two invocations own
#: two different roots, so two runs can hold these same two keys without
#: touching each other's files — and a developer slot that happens to
#: share a name is in a third root entirely, never opened.
SAVE_BASE = "location_embark_base"     # portal-free fixture, loaded by (a) and (b)
SAVE_LOCAL = "location_embark_local"   # (b)'s own save, reloaded by (c)

failures: list[str] = []
_current_log: list[str | None] = [None]

#: #1982 — this run's durable failure records. Built at import, so the
#: offset every record carries is measured from the probe's own start;
#: emitted by `report` below instead of the unflushed `FAIL:` print that
#: the runner's block-buffered pipe used to strand above its 25-line
#: tail.
FAILURE = FailureEmitter("location_embark_probe")


def set_log(path: str | None) -> None:
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
# Invocation-owned artifacts and isolation (#1569)
# --------------------------------------------------------------------------
def _make_owner_writable(top: str) -> None:
    """Add owner write (and directory search) permission throughout a
    freshly copied tree.

    `shutil.copytree` reproduces the SOURCE's mode bits, so a checkout
    whose `config/` is read-only — a CI cache restored read-only, a
    read-only mount, an archive unpacked without write bits — yields a
    private `config/` this run cannot use and cannot delete: a directory
    needs owner write+search before any of its entries can be unlinked,
    so `release_artifacts` would report residue and leave the whole tree
    behind on a run that did nothing wrong. The copy is THIS
    invocation's, so it is made writable regardless of what the source
    happened to be; the source itself is never touched.
    """
    for path, dirs, files in os.walk(top):
        for name in [None, *dirs, *files]:
            target = path if name is None else os.path.join(path, name)
            try:
                mode = os.lstat(target).st_mode
                if stat.S_ISLNK(mode):
                    continue
                extra = stat.S_IRWXU if stat.S_ISDIR(mode) else stat.S_IRUSR | stat.S_IWUSR
                os.chmod(target, stat.S_IMODE(mode) | extra)
            except OSError:
                # Best effort: a mode this process cannot change is
                # reported by the cleanup that actually trips over it,
                # with the path it failed on, rather than here.
                pass


class RunArtifacts:
    """Every file one invocation of this probe creates, under a single
    directory that invocation owns.

    `base` comes from `tempfile.mkdtemp`, so it is this process's alone
    and disjoint from every other invocation's — which is what makes the
    logical names inside it (`engine_prep.log`, `location_embark_base`,
    `icon_discovered.png`) safe to keep fixed. Two concurrent runs on
    different `--port` values write two different trees; a developer
    save slot of the same name lives in the checkout's root and is never
    opened at all.
    """

    def __init__(self, base: str) -> None:
        self.base = base
        self.root = os.path.join(base, "root")
        self.logs = os.path.join(base, "logs")
        self.shots = os.path.join(base, "screenshots")

    def build(self) -> None:
        """Materialise the throwaway resource root and the two artifact
        directories beside it.

        The read-only content families are symlinked; `config/` is
        COPIED without the developer's `*.local.yaml` overrides, so a
        personal setting can neither be changed by this run nor decide
        what it observes; `saves/` starts empty and belongs to this run.
        `app/App/ResourceRoot.hs` chdirs each engine into `root`, so
        every relative write the sessions below make — the two save
        slots above especially — lands inside this tree.
        """
        os.makedirs(self.root, exist_ok=True)
        for family in ("scripts", "assets", "data"):
            target = os.path.join(self.root, family)
            if not os.path.exists(target):
                os.symlink(os.path.join(REPO, family), target)
        config_dst = os.path.join(self.root, "config")
        if not os.path.exists(config_dst):
            shutil.copytree(os.path.join(REPO, "config"), config_dst,
                            ignore=shutil.ignore_patterns("*.local.yaml"))
            _make_owner_writable(config_dst)
        os.makedirs(os.path.join(self.root, "saves"), exist_ok=True)
        os.makedirs(self.logs, exist_ok=True)
        os.makedirs(self.shots, exist_ok=True)

    def log(self, name: str) -> str:
        return os.path.join(self.logs, f"{name}.log")

    def boot_args(self, extra: list[str] | None = None) -> list[str]:
        """Engine CLI args pinning the boot to THIS run's root. Every
        boot the probe makes — including each phase-0 seed retry — goes
        through here, so none of them can fall back to the cwd."""
        return [*(extra or []), "--resource-root", self.root]


def release_artifacts(art: RunArtifacts, keep: bool) -> None:
    """Retire this invocation's artifact directory, once every engine it
    booted has been through `quit_engine`.

    Without `--keep-artifacts` the whole tree goes away and anything
    that SURVIVES is recorded as a failing check: a green result sitting
    beside leftover saves is exactly the outcome this isolation exists
    to prevent, so it must not be reported as a pass. That residue
    report is not the diagnostic opt-in — it names what is left over
    precisely because the run did not intend to leave it.

    Only ever removes the directory this process made with
    `tempfile.mkdtemp`; `rmtree` unlinks the symlinked content families
    rather than recursing into them, so the real `scripts/`, `assets/`
    and `data/` are never followed.
    """
    if keep:
        # Each line names what this run ACTUALLY produced. A run that
        # failed at phase 0 holds no save slot and no screenshot, and
        # saying otherwise would send the reader looking for files the
        # failure is the reason they do not have.
        saves = os.path.join(art.root, "saves")
        print(f"\nretained this run's artifacts (--keep-artifacts): {art.base}")
        for label, path in (("engine logs", art.logs),
                            ("screenshots", art.shots),
                            ("saves", saves)):
            try:
                held = sorted(os.listdir(path))
            except OSError:
                held = []
            print(f"  {label:14} {path}"
                  + (f" ({', '.join(held)})" if held else " (empty)"))
        print(f"  {'resource root':14} {art.root}")
        return
    try:
        shutil.rmtree(art.base)
    except OSError as exc:
        failures.append(f"could not remove this run's artifact directory "
                        f"{art.base}: {exc}")
        return
    if os.path.exists(art.base):
        failures.append(f"this run's artifact directory survived removal: "
                        f"{art.base}")


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


# --------------------------------------------------------------------------
# Durable saves (#1746)
# --------------------------------------------------------------------------
def save_and_wait(port: int, page: str, slot: str, label: str) -> bool:
    """`engine.saveWorld`, then tie completion to THIS request's own id.

    `engine.saveWorld` only ACCEPTS synchronously
    (src/Engine/Scripting/Lua/API/Save.hs): it returns false on a
    validation failure — with the reason going to the engine log, not
    to the console — and true once the command is queued, while the
    encode and the disk write run afterwards behind the save barrier.
    So the API's own Boolean is the only acceptance signal, and
    `SaveCaptureComplete` (or the terminal `SaveFailed`) for THIS
    request id is the only durability signal; a fixed sleep proves
    neither, and a status left behind by an earlier save answers for
    the wrong request.

    Returns True only when this slot is on disk. Every reader of the
    slot — a later session, a fresh process — must start only after
    that, so a caller gates its dependent work on the result. A false
    return still permits the caller's `finally` shutdown: quitting the
    engine that failed to save is cleanup, not a dependent read.
    """
    accepted = send(port, f"return engine.saveWorld('{page}', '{slot}')").strip()
    if not check(f"{label}: engine.saveWorld('{page}', '{slot}') accepted",
                 accepted.lower() == "true",
                 f"returned {accepted!r}; the validation reason is logged in "
                 f"{_current_log[0]}"):
        return False
    request_id = capture_request_id(port, "return engine.getSaveStatus()")
    if not check(f"{label}: engine.getSaveStatus() reports a request id for "
                 f"'{slot}'",
                 request_id is not None,
                 f"no request id was ever observed for "
                 f"engine.saveWorld('{page}', '{slot}'); see {_current_log[0]}"):
        return False
    ok, status = wait_save_complete(port, request_id)
    if not check(f"{label}: save of '{slot}' (request {request_id}) reaches "
                 f"SaveCaptureComplete",
                 ok,
                 f"engine.saveWorld('{page}', '{slot}') request {request_id} "
                 f"ended at {status}"):
        return False
    print(f"    saved '{slot}' (request {request_id}, phase "
          f"{status.get('phase')})")
    return True


# --------------------------------------------------------------------------
# Phase 1: headless fixture prep
# --------------------------------------------------------------------------
def prepare_fixture(port: int, seeds: list[int], size: int,
                     art: RunArtifacts, min_ruins: int = 2,
                     page: str = FIXTURE_PAGE):
    """Try each seed in turn until one places >= min_ruins ruin_small
    locations, then save it as SAVE_BASE and wait for that save's own
    request to become durable.

    Returns `(seed, ruins)` when the fixture is on disk, `(None, [])`
    if every candidate seed falls short — a fail-fast diagnostic, never
    a silent generation-density change (out of scope per the issue) —
    and `(None, ruins)` when a seed qualified but its save was refused
    or never completed. Those last two are distinct on purpose: the
    caller may only report "no seed qualified" for the first, and every
    session that would LOAD this slot is suppressed for both (#1746).
    A qualifying seed whose save fails is not retried on the next seed;
    the failure is the save, not the world.

    Every retry boots into `art.root`, so the seed that eventually wins
    writes its fixture save there and the ones that don't write nothing
    the developer's root can see."""
    for candidate in seeds:
        set_log(art.log("engine_prep"))
        proc = boot(port, log=art.log("engine_prep"), args=art.boot_args(),
                    label=f"prep engine (seed {candidate})")
        try:
            load_defs(port)
            gen_world(port, page, candidate, size)
            ruins = [e for e in placed_ready(port)
                     if e.get("id") == "ruin_small" and "bounds" in e]
            print(f"  seed {candidate}: {len(ruins)} ruin_small placed")
            if len(ruins) >= min_ruins:
                if not save_and_wait(port, page, SAVE_BASE, "phase 0"):
                    return None, ruins
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
    shot_hidden_target = os.path.join(shots, "icon_unknown_target.png")
    shot_hidden_control = os.path.join(shots, "icon_unknown_control.png")
    center_on(port, target["gx"], target["gy"])
    set_zoom(port, full_zoom)
    time.sleep(0.3)
    shot_t = os.path.join(shots, "icon_reloaded_target.png")
    if check("reloaded target-icon screenshot answers", screenshot(port, shot_t)):
        st = png_stats(shot_t)
        check("reloaded target-icon frame is not blank",
              bool(st) and st[2] >= 3, f"distinct colors: {st and st[2]}")
        check("reloaded target ruin's icon still differs from its own "
              "pre-portal unknown-marker baseline (discovered state survived "
              "save -> quit -> restart -> load)",
              png_differs(shot_hidden_target, shot_t, min_fraction=0.0002))
    center_on(port, control["gx"], control["gy"])
    set_zoom(port, full_zoom)
    time.sleep(0.3)
    shot_c = os.path.join(shots, "icon_reloaded_control.png")
    if check("reloaded control-icon screenshot answers", screenshot(port, shot_c)):
        check("the reloaded type icon and unknown marker render differently",
              png_differs(shot_t, shot_c, min_fraction=0.0002))
        check("reloaded control ruin's icon still matches its own pre-portal "
              "unknown-marker baseline (unknown state survived "
              "save -> quit -> restart -> load)",
              not png_differs(shot_hidden_control, shot_c, min_fraction=0.0002))

    # -- steps 22a-22d (#1230): the three icon appearances, on the GPU,
    # at ONE camera centre and ONE zoom, so the only thing varying
    # between the frames is the location's own lifecycle.
    #
    # This runs LAST, in the reload session, for two reasons. It drives
    # the control ruin forward through discovered/cleared/depleted and
    # lifecycle promotion is one-way, so it must come after every check
    # that requires the control to still be unknown — which is all of
    # session (b) and everything above. And it reuses the camera/zoom
    # sequence this session has just proven works (the reloaded-icon
    # shots above), rather than the post-save tail of session (b),
    # where the same sequence photographed a static frame.
    #
    # What this can and cannot prove: data/locations/ruin_small.yaml is
    # the only shipped location definition, so both ruins are instances
    # of it and "every definition shares ONE unknown marker" is not
    # demonstrable from a real world — that claim belongs to the pure
    # "Location map icons" Hspec group, which registers synthetic
    # definitions. What IS demonstrable here is that the unknown marker
    # and the ruin type icon render DIFFERENTLY at one camera centre and
    # zoom, and that the spent states render that type icon strictly
    # DARKER.
    inst_id = (c or {}).get("instance_id")
    if not check("reloaded control ruin exposes its instance id for the "
                 "icon-state sweep", inst_id is not None):
        return

    def shoot_state(tag: str) -> str | None:
        center_on(port, control["gx"], control["gy"])
        set_zoom(port, full_zoom)
        time.sleep(0.4)
        path = os.path.join(shots, f"icon_state_{tag}.png")
        return path if screenshot(port, path) else None

    def force_lifecycle(state: str) -> bool:
        got = send(port,
                   f"return tostring(world.setLocationLifecycle("
                   f"{int(inst_id)}, '{state}'))")
        return "true" in (got or "").lower()

    # `shot_c` above is this same camera centre and zoom, taken while
    # the control was still unknown — reuse it rather than re-shooting,
    # so the unknown frame is provably the one the persistence check
    # just validated.
    shot_unknown = shot_c
    lit_path = None
    if check("control ruin promotes to discovered", force_lifecycle("discovered")):
        lit_path = shoot_state("discovered")
        if check("discovered-state screenshot answers", bool(lit_path)):
            check("the unknown marker and the ruin type icon render "
                  "DIFFERENTLY at the same camera centre and zoom — the "
                  "map does not show the ruin's type until a unit has "
                  "seen it",
                  png_differs(shot_unknown, lit_path, min_fraction=0.0001))

    lit = centre_luma(lit_path, w, h) if lit_path else None
    spent_shots: dict[str, str] = {}
    for spent in ("cleared", "depleted"):
        if not check(f"control ruin promotes to {spent}", force_lifecycle(spent)):
            continue
        shot = shoot_state(spent)
        if not check(f"{spent}-state screenshot answers", bool(shot)):
            continue
        spent_shots[spent] = shot
        dark = centre_luma(shot, w, h)
        if check(f"{spent}-state luminance measurable and comparable",
                 lit is not None and dark is not None,
                 f"discovered={lit} {spent}={dark}"):
            check(f"the {spent} icon renders strictly DARKER than the "
                  f"discovered one at the same camera centre and zoom",
                  dark < lit,
                  f"discovered luma={lit:.3f} {spent} luma={dark:.3f}")
    # Both spent states share one bitmap and one tint, so they must be
    # indistinguishable from each other — the thing that would break if
    # either grew its own texture.
    if len(spent_shots) == 2:
        check("cleared and depleted render identically — one type icon, "
              "one tint, no second authored bitmap",
              not png_differs(spent_shots["cleared"], spent_shots["depleted"],
                              min_fraction=0.0001))


def run_probe(args, w: int, h: int, art: RunArtifacts) -> None:
    """Phase 0 plus the three offscreen sessions, all inside the caller's
    cleanup guard. Every `boot` here goes through `art.boot_args`, and
    every session shuts its engine down in a `finally` before control
    can return to that guard."""
    art.build()
    # Named while the run is live so its logs can be tailed, and named
    # honestly: without the opt-in this path is gone by the time the
    # summary prints, and only the summary is allowed to point at
    # artifacts that are still there.
    print(f"isolated resource root: {art.root}"
          + ("" if args.keep_artifacts
             else " (removed on exit; pass --keep-artifacts to retain it)"))
    seeds = [args.seed] + [int(s) for s in args.alt_seeds.split(",") if s.strip()]

    print(f"== phase 0: headless fixture prep (size {args.size}) ==")
    used_seed, ruins = prepare_fixture(args.port, seeds, args.size, art)
    if not check("a candidate seed placed at least two ruin_small locations",
                  len(ruins) >= 2,
                  f"tried seeds {seeds}, best count {len(ruins)}"):
        return
    if used_seed is None:
        # A seed qualified but its save was refused or never completed;
        # save_and_wait already recorded which step failed and why.
        # Sessions (a) and (b) both LOAD this slot, and (c) loads what
        # (b) would have saved, so none of them may run (#1746).
        print("  sessions (a), (b) and (c) skipped: the fixture save never "
              f"reached SaveCaptureComplete, so '{SAVE_BASE}' is not durable")
        return
    print(f"  fixture ready: seed={used_seed}, {len(ruins)} ruin(s), "
          f"saved as '{SAVE_BASE}'")

    ruins_sorted = sorted(ruins, key=lambda e: (e["cx"], e["cy"]))
    target, control = ruins_sorted[0], ruins_sorted[1]
    expected_total = len(ruins)
    shots = art.shots
    win = art.boot_args(["--size", args.win_size])

    print("== session (a): zoom-map icons, ghost validity, remote-modal flow ==")
    set_log(art.log("engine_session_a"))
    proc1 = boot(args.port, mode=("--offscreen",), args=win,
                 log=art.log("engine_session_a"),
                 label="offscreen engine (session a)")
    try:
        session_ghost_and_remote(args.port, w, h, shots, target, control)
    finally:
        quit_engine(args.port, proc1)

    print("== session (b): local placement, roster, real-order discovery, save ==")
    set_log(art.log("engine_session_b"))
    proc2 = boot(args.port, mode=("--offscreen",), args=win,
                 log=art.log("engine_session_b"),
                 label="offscreen engine (session b)")
    try:
        saved_local = session_local_and_discovery(args.port, w, h, shots,
                                                  target, control)
    finally:
        quit_engine(args.port, proc2)

    if not saved_local:
        # Session (c) is the fresh-process half of the save -> quit ->
        # restart -> load proof, and it reads SAVE_LOCAL. Without a
        # completed save of that slot the load has nothing durable to
        # find, and its failure would be attributed to the load rather
        # than to the save that never finished (#1746).
        print("== session (c) skipped: session (b) published no durable "
              f"'{SAVE_LOCAL}' ==")
        return

    print("== session (c): fresh restart -> load -> verify persistence ==")
    set_log(art.log("engine_session_c"))
    proc3 = boot(args.port, mode=("--offscreen",), args=win,
                 log=art.log("engine_session_c"),
                 label="offscreen engine (session c)")
    try:
        session_reload_check(args.port, w, h, shots, target, control, expected_total)
    finally:
        quit_engine(args.port, proc3)


def main() -> int:
    ap = argparse.ArgumentParser(description=__doc__)
    ap.add_argument("--seed", type=int, default=42)
    ap.add_argument("--alt-seeds", default="7,99,123,2026",
                    help="comma-separated fallback seeds tried in order if "
                         "--seed doesn't place >= 2 ruin_small locations")
    ap.add_argument("--size", type=int, default=64)
    ap.add_argument("--port", type=int, default=9420)
    ap.add_argument("--win-size", default="1280x720")
    ap.add_argument("--keep-artifacts", action="store_true",
                    help="keep this run's artifact directory (engine logs, "
                         "isolated resource root with its saves, and "
                         "screenshots) instead of deleting it, and name it "
                         "in the summary — for diagnosing a failure")
    args = ap.parse_args()

    w, h = (int(v) for v in args.win_size.lower().split("x"))
    # The guard starts HERE, one statement after the directory exists,
    # so nothing between this point and the report below — building the
    # isolated root, a phase-0 seed that never boots an engine, an early
    # return, a dead engine, an unexpected exception — can leave the
    # tree behind.
    art = RunArtifacts(tempfile.mkdtemp(prefix="synarchy_location_embark_"))
    try:
        run_probe(args, w, h, art)
    except KeyboardInterrupt:
        release_artifacts(art, args.keep_artifacts)
        raise
    except SystemExit as exc:
        # `probelib.boot` aborts the run this way when an engine dies
        # before READY or never prints it. Recording it as a failing
        # check rather than letting it exit keeps the artifact release
        # below on the path, and names the abort in the summary.
        failures.append(f"the run aborted before finishing: {exc}")
    except Exception as exc:  # noqa: BLE001 - reported, then re-summarised
        failures.append(f"unexpected {type(exc).__name__} during the run: {exc}")
        traceback.print_exc()
    return report(art, args.keep_artifacts)


def report(art: RunArtifacts, keep: bool) -> int:
    if failures:
        # BEFORE release_artifacts below, which removes the tree the
        # engine log lives inside. Requirement 4 is met by retaining a
        # BOUNDED excerpt of that log in the capture, never by keeping the
        # tree, so requirement 5's unconditional cleanup is untouched.
        FAILURE.context_log(_current_log[0])
    release_artifacts(art, keep)
    print("-" * 56)
    if failures:
        # Durable records rather than an unflushed stderr print (#1982):
        # the runner reads these back from the COMPLETE capture, so a
        # failed check survives however much output followed it. Emitted
        # after release_artifacts because that call can record a leftover
        # of its own, which belongs in the same block.
        FAILURE.report(failures)
        FAILURE.context("artifact root",
                        f"{art.base} ({'retained' if keep else 'removed'})")
        print(f"location_embark_probe: {len(failures)} check(s) FAILED")
        if not keep:
            print("  (re-run with --keep-artifacts to retain this run's "
                  "engine logs, saves and screenshots)")
        return 1
    print("location_embark_probe: all checks passed")
    return 0


if __name__ == "__main__":
    sys.exit(main())
