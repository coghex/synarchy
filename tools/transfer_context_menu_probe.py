#!/usr/bin/env python3
"""Offscreen probe for the "Transfer" context-menu entry (#1014 phase B1,
widened to faction-based endpoint eligibility by #1085 phase A2).

Drives the REAL production path — a real right-click on a real
Vulkan-rendered building/unit, a real menu row located by its VISIBLE
LABEL through `ui.dumpWidgets()` (never a hardcoded screen coordinate),
a real click on that row — and checks the resulting transfer session
through `scripts.transfer_session`. `--offscreen` (GPU on, window off)
is required: the real HUD/context-menu flow never boots headless (it
gates on `fontsReady`, a GPU font atlas), and `--headless` refuses
`input.*` injection outright.

Registers a throwaway 1x1 building fixture (`build_work: 0.0`, positive
`storage_capacity`) rather than spawning the shipped `cargo_hold_S`:
that def's real `build_work` (240s, worker-driven) means
`building.spawn` alone would leave it stuck in the "appearing" activity
forever with no construct_job AI running to finish it — `build_work:
0.0` takes the OTHER branch of `Building.Types.currentActivity` (no
`state_animations` block here either, so its computed `appearDuration`
is 0), which reports Built the instant it's spawned. Mirrors
`tools/construction_blueprint_footprint_probe.py`'s own throwaway-def
technique.

Verifies, in order:
  1. a selected acolyte + a right-click on the built storage building
     -> a "Transfer" row is visible on the resulting context menu
     (located via `ui.dumpWidgets()`, alongside the pre-existing
     "Contents" row -- #1014 requirement 7's regression check).
     "Contents" is then ACTIVATED and the container window it opens is
     asked which endpoint it opened for: #1234 generalized that window
     from a bare building id to an endpoint identity (kind + id), and
     only activating the row proves the route survived the signature
     change;
  2. that right-click's `debug.drainActionOutcomes()` record shows
     `handler = "context_menu_building"`, never `"move_order"` --
     requirement 7's "the existing right-click move-order path is not
     triggered" checked directly against the real click-routing oracle,
     not inferred;
  3. activating "Transfer" creates a session
     (`scripts.transfer_session.get()`) naming the exact NAMED endpoint
     identities #1085 records (source/destination tables, no operation
     field);
  4. the same three checks against a right-click on the technomule
     (located precisely via `unit.hitTestInRect` bisection, since a
     unit's live position isn't tile-aligned the way `camera.goToTile`
     is) -- alongside the pre-existing "Info"/"Attack" rows;
  5. #1085 section 9's deliberate widening: a right-click on a SECOND
     player acolyte also offers "Transfer" -- the transfer_receiver data
     marker is gone and faction eligibility replaced it, so the row is
     no longer technomule-only;
  6. the two exclusions that must survive that widening: a right-click
     on the SELECTED acolyte itself offers no "Transfer" (self-transfer),
     and neither does a right-click on a wildlife unit
     (not player-commandable).

Determinism (#1014 review): every unit is spawned several tiles away
from the storage building's own tile and from each other, because
`scripts/init_context_menu.lua`'s `tryBuildingMenu` is tried before
`tryUnitMenu` -- a unit standing on the building's footprint would open
the building menu instead of the unit menu.

Manual-only (needs-gpu) unless promoted through `tools/ci_probes.py`
per CLAUDE.md; the CI-blocking gate for this feature is
`cabal test synarchy-test-headless --test-options='--match "Transfer
context menu"'` (`test-headless/Test/Headless/UI/TransferContextMenu.hs`).

Usage: python3 tools/transfer_context_menu_probe.py
       [--port 9425] [--size 1024x768]
"""
from __future__ import annotations

import argparse
import os
import re
import sys
import tempfile
import time

sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
from probelib import boot, poll_until, quit_engine, send, send_json

SPROOT = tempfile.gettempdir()
TEST_BUILDING_YAML = os.path.join(SPROOT, "transfer_context_menu_probe_buildings.yaml")
DEF_CARGO = "probe_transfer_cargo_hold"

TEST_BUILDINGS = f"""\
buildings:
  - name: "{DEF_CARGO}"
    display_name: "Probe Transfer Cargo Hold"
    category: "Test"
    description: "Throwaway #1014 test fixture — not shipped content."
    sprite: "assets/textures/buildings/cargo_hold_S/default.png"
    tile_size: {{ x: 1, y: 1 }}
    placement: "flat_ground"
    race: "acolyte_cult"
    build_work: 0.0
    storage_capacity: 200.0
"""

failures = 0


def check(name: str, ok: bool, detail: str = "") -> bool:
    global failures
    print(f"  [{'PASS' if ok else 'FAIL'}] {name}"
          + (f" — {detail}" if detail and not ok else ""))
    failures += not ok
    return ok


# --------------------------------------------------------------------------
# UI helpers (F3/#645 widget oracle, extended by #1014 for context-menu rows)
# --------------------------------------------------------------------------
def widgets(port: int):
    got = send_json(port, "return ui.dumpWidgets()", timeout=10.0)
    return got if isinstance(got, list) else []


def find_widget(port: int, label: str):
    for w in widgets(port):
        if (w.get("label") or "").strip().lower() == label.lower():
            return w
    return None


def click_widget_center(port: int, w: dict) -> None:
    b = w.get("bounds") or {}
    x = int(b.get("x", 0) + b.get("w", 0) / 2)
    y = int(b.get("y", 0) + b.get("h", 0) / 2)
    send(port, f"return input.moveMouse({x}, {y})")
    send(port, f"return input.click({x}, {y})")


def drain_outcomes(port: int):
    got = send_json(port, "return debug.drainActionOutcomes()", timeout=10.0)
    return got if isinstance(got, list) else []


# --------------------------------------------------------------------------
# World-tile helpers
# --------------------------------------------------------------------------
def tile_surface(port: int, x: int, y: int):
    """(surfaceZ, isDry) for one tile, or None if the chunk isn't loaded."""
    raw = send(port, f"return world.getSurfaceAt({x}, {y})")
    parts = raw.split()
    if len(parts) < 3:
        return None
    try:
        z = int(float(parts[0]))
    except ValueError:
        return None
    return z, parts[2] in ("null", "nil")


def _candidate_grid(step: int, extent: int):
    pts = [(dx, dy) for dx in range(-extent, extent + 1, step)
                     for dy in range(-extent, extent + 1, step)]
    pts.sort(key=lambda p: p[0] * p[0] + p[1] * p[1])
    return pts


# How far from the origin 'allocate_dry_anchors' will look for land,
# nearest-to-origin first. 60 tiles is bounded by two things: the
# ±4-chunk region main() loads (chunks are 16 tiles, so tiles -64..79
# are resolvable) and world.getAreaFluid's documented max radius of 64.
# The 4-tile step inside '_candidate_grid' is what keeps each unit's
# tile clear of the building's own footprint -- the #1014 determinism
# requirement above.
SEARCH_RADIUS = 60


CHUNK_TILES = 16


def _search_centres(rings: int = 2, spacing: int = 2 * SEARCH_RADIUS):
    """Origin first, then square rings outward.

    A default world is 128 CHUNKS across (`scripts/world_manager.lua`),
    i.e. ~2048 tiles, so the radius-60 box around the origin is a ~3%
    sliver of it -- "all ocean here" says nothing about the world. Two
    runs while building #1085's scenarios came up with all 14641 tiles
    of that box fluid, which is the probe failing to look rather than
    the world lacking land."""
    pts = [(0, 0)]
    for r in range(1, rings + 1):
        d = r * spacing
        pts += [(d, 0), (-d, 0), (0, d), (0, -d),
                (d, d), (-d, -d), (d, -d), (-d, d)]
    return pts


def allocate_dry_anchors(port: int, n: int, min_sep: int = 12,
                          radius: int = SEARCH_RADIUS):
    """Take `n` DISTINCT dry tiles, each at least `min_sep` tiles from
    every tile already taken, out of ONE shared candidate list.

    Deliberately not n disjoint slices of that list (#1085): slicing
    gives each fixture only 1/n of the pool. Every fixture draws from
    all of it and the separation filter is what keeps them apart, which
    also satisfies #1014's determinism requirement that no unit stands
    on the building's footprint (`tryBuildingMenu` is tried before
    `tryUnitMenu`). `min_sep` is deliberately much wider than one
    footprint: every fixture WANDERS, and neighbours that drift into
    each other end up with overlapping sprites, which is what made a
    4-tile spacing route right-clicks to the wrong unit.

    Searches outward from the origin (see `_search_centres`), loading
    each centre's chunks first, and uses ONE bulk `world.getAreaFluid`
    call per centre to eliminate wet tiles -- a per-tile scan of this
    many candidates would be thousands of round trips."""
    span = radius // CHUNK_TILES + 1
    offsets = _candidate_grid(4, radius)
    for ox, oy in _search_centres():
        ccx, ccy = ox // CHUNK_TILES, oy // CHUNK_TILES
        send(port, f"return world.loadChunksInRegion({ccx - span}, "
                   f"{ccy - span}, {ccx + span}, {ccy + span})")
        send(port, "return world.waitForChunks(90)", timeout=95.0)
        wet = set()
        got = send_json(port,
                        f"return world.getAreaFluid({ox}, {oy}, {radius})",
                        timeout=30.0)
        if isinstance(got, list):
            for cell in got:
                if isinstance(cell, dict) and "x" in cell and "y" in cell:
                    wet.add((int(cell["x"]), int(cell["y"])))
        picked: list = []
        for dx, dy in offsets:
            gx, gy = ox + dx, oy + dy
            if (gx, gy) in wet:
                continue
            if any(max(abs(gx - tx), abs(gy - ty)) < min_sep
                   for tx, ty in picked):
                continue
            # Confirm against the tile itself: getAreaFluid says nothing
            # about a tile whose chunk never loaded.
            info = tile_surface(port, gx, gy)
            if info is None or not info[1]:
                continue
            picked.append((gx, gy))
            if len(picked) == n:
                return picked
        print(f"  (no usable land around {(ox, oy)}: {len(wet)} of "
              f"{len(offsets)} candidate tiles are fluid, "
              f"{len(picked)} dry site(s) found)")
    return None


def center_on_tile(port: int, target_gx: int, target_gy: int,
                    screen_x: int, screen_y: int, tries: int = 8):
    """camera.goToTile centers the CAMERA on (gx, gy), but the tile that
    actually resolves under a given screen pixel is off by a few tiles
    in practice (isometric projection + that tile's own terrain
    height) -- iteratively correct the target by the observed
    world.pickTile error until it converges (or gives up after
    `tries`). Returns the tile actually resolved at (screen_x,
    screen_y), matching the technique validated in
    tools/portal_ghost_probe.py's center_on_tile /
    tools/location_embark_probe.py's center_on_tile-driven flow."""
    gx, gy = target_gx, target_gy
    resolved = None
    for _ in range(tries):
        send(port, f"camera.goToTile({gx}, {gy}); return 'ok'")
        # POLL rather than assume one fixed sleep is enough: the camera
        # needs a frame or two to settle and the tile under the cursor
        # has to be rendered before world.pickTile answers at all. A
        # single 0.4 s wait returned nil for every try on one run
        # (observed live), which failed the scenario for a reason that
        # had nothing to do with the behaviour under test.
        picked = poll_until(5.0, lambda: send_json(
            port, f"return {{world.pickTile({screen_x}, {screen_y})}}") or None)
        if not picked:
            continue
        resolved = (picked[0], picked[1])
        if resolved == (target_gx, target_gy):
            break
        gx += target_gx - resolved[0]
        gy += target_gy - resolved[1]
    return resolved


def set_paused(port: int, on: bool) -> None:
    send(port, f"engine.setPaused({'true' if on else 'false'}); return 'ok'")
    time.sleep(0.3)


def centre_and_freeze(port: int, gx: int, gy: int, sx: int, sy: int):
    """Centre the camera on a tile, then freeze the simulation.

    Both halves are needed and they conflict: `world.pickTile` answers
    nil while the simulation is PAUSED (so the camera convergence loop
    must run unpaused), but units WANDER while it runs -- over the
    several minutes these scenarios take, fixtures drift into each other
    and a unit standing under a neighbour's sprite simply cannot be
    clicked, which `locate_unit_pixel`'s hitTestAt confirmation then
    (correctly) reports as "could not locate". Unpausing only for the
    convergence loop gives both: a few seconds of drift instead of
    minutes, and everything held still for the locate-and-click that
    follows."""
    set_paused(port, False)
    resolved = center_on_tile(port, gx, gy, sx, sy)
    set_paused(port, True)
    return resolved


def _hit_test_in_rect_has(port: int, uid: int, x1: int, y1: int,
                           x2: int, y2: int) -> bool:
    raw = send(port, f"return unit.hitTestInRect({x1}, {y1}, {x2}, {y2})")
    ids = re.findall(r"\d+", raw)
    return str(uid) in ids


def _hit_test_at_is(port: int, uid: int, x: int, y: int) -> bool:
    """Does a click at this pixel resolve to exactly `uid`? This is the
    SAME single-target hit test the right-click router uses, so it is
    the only question that predicts where the menu will actually go."""
    raw = send(port, f"return unit.hitTestAt({x}, {y})").strip().strip('"')
    if raw in ("", "nil", "null"):
        return False
    try:
        return int(float(raw)) == uid
    except ValueError:
        return False


def locate_unit_pixel(port: int, uid: int, w: int, h: int,
                       max_steps: int = 14):
    """Bisect unit.hitTestInRect down to the small screen-pixel box
    containing `uid`'s sprite quad, then CONFIRM the resulting pixel
    against unit.hitTestAt -- the same technique validated in
    tools/location_embark_probe.py's locate_unit_pixel, plus the
    confirmation pass #1085 needed. A single unit.hitTestAt(cx0, cy0) at
    the tile-converged screen centre only lands on a unit's own (much
    smaller, sub-tile) sprite quad by coincidence, but the bisection
    alone is not enough either: hitTestInRect answers "is uid ANYWHERE
    in this rect", so with several units milling around, the box's
    centre pixel can sit on a neighbour's overlapping sprite and the
    right-click then routes somewhere else entirely (observed live --
    a self-transfer scenario that quietly hit the wrong acolyte)."""
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
            break
    cx, cy = (x1 + x2) // 2, (y1 + y2) // 2
    if _hit_test_at_is(port, uid, cx, cy):
        return cx, cy
    for r in range(1, 7):
        d = r * 3
        for dx, dy in ((0, -d), (0, d), (-d, 0), (d, 0),
                       (-d, -d), (d, -d), (-d, d), (d, d)):
            if _hit_test_at_is(port, uid, cx + dx, cy + dy):
                return cx + dx, cy + dy
    return None


# --------------------------------------------------------------------------
# Scenario helpers
# --------------------------------------------------------------------------
def right_click_and_check_routing(port: int, x: int, y: int,
                                   expect_handler: str, scenario: str) -> None:
    drain_outcomes(port)  # clear anything queued before this click
    send(port, f"return input.moveMouse({x}, {y})")
    send(port, f"return input.click({x}, {y}, 'right')")
    time.sleep(0.4)
    outcomes = drain_outcomes(port)
    handlers = [o.get("handler") for o in outcomes if isinstance(o, dict)]
    check(f"{scenario}: right-click routed through {expect_handler!r}",
          expect_handler in handlers, f"got handlers={handlers!r}")
    check(f"{scenario}: right-click did NOT fall through to a move order",
          "move_order" not in handlers, f"got handlers={handlers!r}")


def get_session(port: int):
    return send_json(port, "return require('scripts.transfer_session').get()")


def clear_session(port: int) -> None:
    send(port, "require('scripts.transfer_session').clear(); return 'ok'")


def close_menu(port: int) -> None:
    """Dismiss any open context menu, so the next scenario's
    `find_widget` can never match a row left over from the previous
    one (only the scenarios that ACTIVATE 'Transfer' close it by
    clicking)."""
    send(port, "require('scripts.ui.context_menu').hide(); return 'ok'")
    time.sleep(0.2)


def locate_building_pixel(port: int, bid: int, cx: int, cy: int,
                           step: int = 6, rings: int = 8):
    """The tile CENTRE pixel is not guaranteed to land on a building's
    own clickable quad (sprites are anchored to the diamond bottom and
    the tile's terrain height shifts them), so search outward from it
    until `building.hitTestAt` actually reports `bid` -- the same
    "locate the real target, never trust a computed coordinate" rule
    `locate_unit_pixel` follows for units."""
    def hits(x: int, y: int) -> bool:
        raw = send(port, f"return building.hitTestAt({x}, {y})")
        return raw.strip().strip('"') not in ("", "nil", "null") \
            and int(float(raw)) == bid

    if hits(cx, cy):
        return cx, cy
    for r in range(1, rings + 1):
        d = r * step
        for dx, dy in ((0, -d), (0, d), (-d, 0), (d, 0),
                       (-d, -d), (d, -d), (-d, d), (d, d)):
            if hits(cx + dx, cy + dy):
                return cx + dx, cy + dy
    return None


def right_click_unit(port: int, uid: int, fb_w: int, fb_h: int,
                      scenario: str):
    """Centre on the unit's LIVE tile (never its spawn tile -- wildlife
    and acolytes both wander, and by the last scenario a squirrel can be
    tens of tiles from where it was spawned), bisect to its own sprite
    pixel, and right-click it. Returns True when the unit menu really
    opened."""
    info = send_json(port, f"return unit.getInfo({uid})")
    if not check(f"{scenario}: unit still exists", isinstance(info, dict),
                 f"got {info!r}"):
        return False
    gx, gy = int(info.get("gridX", 0)), int(info.get("gridY", 0))
    cx0, cy0 = fb_w // 2, fb_h // 2
    resolved = centre_and_freeze(port, gx, gy, cx0, cy0)
    near = resolved is not None \
        and max(abs(resolved[0] - gx), abs(resolved[1] - gy)) <= 2
    check(f"{scenario}: camera centres near the unit's live tile", near,
          f"got {resolved!r} for live tile {(gx, gy)!r}")
    time.sleep(0.3)
    pixel = locate_unit_pixel(port, uid, fb_w, fb_h)
    if not check(f"{scenario}: located the unit's own screen pixel",
                 pixel is not None):
        return False
    px, py = pixel
    right_click_and_check_routing(port, px, py, "context_menu_unit", scenario)
    return True


def main() -> int:
    ap = argparse.ArgumentParser(
        description=__doc__, formatter_class=argparse.RawDescriptionHelpFormatter)
    ap.add_argument("--port", type=int, default=9425)
    ap.add_argument("--size", default="1024x768")
    args = ap.parse_args()
    port = args.port

    print(f"== offscreen boot (port {port}, {args.size}) ==")
    proc = boot(port, mode=("--offscreen",), args=["--size", args.size],
                label="offscreen engine")

    # -- Real UI flow to the in-game HUD (same path as
    # tools/construction_blueprint_footprint_probe.py / tools/offscreen_probe.py).
    menu_up = poll_until(60.0, lambda: find_widget(port, "Create World"))
    check("loading screen -> main menu", bool(menu_up))
    check("click 'Create World'", bool(find_widget(port, "Create World")))
    click_widget_center(port, find_widget(port, "Create World"))
    create_up = poll_until(20.0, lambda: find_widget(port, "Generate World"))
    check("create-world screen reached", bool(create_up))
    click_widget_center(port, find_widget(port, "Generate World"))

    def world_done():
        got = send(port, "local p = world.getInitProgress(); return p", timeout=5.0)
        return got.strip() == "3"

    print("  (generating world, ~1-2 min)")
    check("worldgen completes (phase 3)", bool(poll_until(300.0, world_done, interval=2.0)))
    cont = poll_until(60.0, lambda: find_widget(port, "Continue"))
    check("post-generation Continue button appears", bool(cont))
    click_widget_center(port, find_widget(port, "Continue"))
    hud_up = poll_until(60.0, lambda: not find_widget(port, "Continue"))
    check("in-game HUD reached", bool(hud_up))
    time.sleep(2.0)  # let the first in-game frames render

    fb_w, fb_h = (int(v) for v in args.size.split("x"))

    # -- Register the throwaway instantly-built storage fixture.
    with open(TEST_BUILDING_YAML, "w") as f:
        f.write(TEST_BUILDINGS)
    n = send(port, f"return engine.loadBuildingYaml('{TEST_BUILDING_YAML}')")
    check("probe building def loaded", float(n) == 1.0, f"got {n!r}")

    print("  (scanning terrain outward from the origin for dry anchor sites)")
    sites = allocate_dry_anchors(port, 5)
    if not check("found five separated dry sites for the fixtures",
                 sites is not None):
        quit_engine(port, proc)
        return 1
    (bax, bay), (max_, may_), (aax, aay), (a2x, a2y), (wax, way) = sites
    print(f"  (fixture sites: building={(bax, bay)} mule={(max_, may_)} "
          f"acolyte={(aax, aay)} acolyte2={(a2x, a2y)} "
          f"wildlife={(wax, way)})")

    # -- Spawn source unit, destinations.
    uid_raw = send(port, f"return unit.spawn('acolyte', {aax}, {aay}, nil, 'player')")
    uid = int(float(uid_raw))
    bid_raw = send(port, f"return building.spawn('{DEF_CARGO}', {bax}, {bay})")
    if not check("storage building spawned", bid_raw not in ("", "nil", "null"),
                 f"got {bid_raw!r}"):
        quit_engine(port, proc)
        return 1
    bid = int(float(bid_raw))
    mule_raw = send(port, f"return unit.spawn('technomule', {max_}, {may_}, nil, 'player')")
    mule_uid = int(float(mule_raw))
    # A2's widening + its two exclusions need a second player-commandable
    # unit that is NOT the technomule, and one that is not commandable at
    # all (unit.spawn defaults to the WILDLIFE faction when no tag is
    # passed -- see Unit.Faction.defaultSpawnFaction).
    a2_raw = send(port, f"return unit.spawn('acolyte', {a2x}, {a2y}, nil, 'player')")
    acolyte2_uid = int(float(a2_raw))
    wild_raw = send(port, f"return unit.spawn('red_squirrel', {wax}, {way})")
    wild_uid = int(float(wild_raw))
    wild_faction = send(port, f"return unit.getFaction({wild_uid})").strip('"')
    check("wildlife fixture is genuinely not player-commandable",
          wild_faction == "wildlife", f"got {wild_faction!r}")

    built = poll_until(10.0, lambda: send(
        port, f"return building.getActivity({bid})").strip('"') == "built")
    check("storage building reaches Built activity", bool(built))
    eligible = send_json(
        port,
        f"return unit.transferEndpointInfo({{ kind = 'building', id = {bid} }})")
    check("building reports transfer-eligible via the endpoint query",
          isinstance(eligible, dict) and eligible.get("eligible") is True,
          f"got {eligible!r}")
    check("endpoint query reports the building's capacity and contents",
          isinstance(eligible, dict) and eligible.get("capacity") == 200.0
          and eligible.get("storedWeight") == 0.0,
          f"got {eligible!r}")

    # Freeze between scenarios so no fixture wanders while the probe
    # works; `centre_and_freeze` lifts it only for each camera move.
    # `unit.setFrozen` is deliberately NOT used: it only stops the
    # render-side update, so a "frozen" unit keeps walking.
    set_paused(port, True)

    send(port, f"return unit.select({uid})")
    selected = send(port, "return unit.getSelected()")
    check("acolyte selected", str(uid) in selected, f"got {selected!r}")

    # ------------------------------------------------------------------
    # Scenario 1: right-click the built storage building.
    # ------------------------------------------------------------------
    print("== building receiver ==")
    cx0, cy0 = fb_w // 2, fb_h // 2
    resolved_b = centre_and_freeze(port, bax, bay, cx0, cy0)
    check("camera resolves the building's own tile", resolved_b == (bax, bay),
          f"got {resolved_b!r}")
    bpixel = locate_building_pixel(port, bid, cx0, cy0)
    if not check("located the storage building's own screen pixel",
                 bpixel is not None):
        quit_engine(port, proc)
        return 1
    bpx, bpy = bpixel

    right_click_and_check_routing(port, bpx, bpy, "context_menu_building",
                                  "building")

    contents_row = find_widget(port, "Contents")
    if check("building menu: 'Contents' still appears (requirement 7 regression)",
             bool(contents_row)):
        # #1234 generalized the container window to an endpoint identity
        # (kind + id). Merely seeing the row does not prove the route
        # into the window survived that signature change, so ACTIVATE it
        # and read back which endpoint the manager actually opened for.
        click_widget_center(port, contents_row)
        time.sleep(0.5)
        opened = send(port, "return require('scripts.cargo_inventory_panel')"
                            ".isOpen()").strip()
        check("activating 'Contents' opens the container window",
              opened == "true", f"got {opened!r}")
        target = send_json(port, "local s = require("
                                 "'scripts.cargo_inventory_panel').state;"
                                 " return {kind = s.kind, id = s.id}")
        check("the container window opened on THIS building endpoint",
              isinstance(target, dict) and target.get("kind") == "building"
              and target.get("id") == bid, f"got {target!r}")
        send(port, "require('scripts.cargo_inventory_panel').closeIfOpen();"
                   " return 'ok'")
        time.sleep(0.3)
        # Re-open the menu the rest of this scenario reads from: the
        # click above consumed it.
        right_click_and_check_routing(port, bpx, bpy, "context_menu_building",
                                      "building (menu reopened)")
    transfer_row_b = find_widget(port, "Transfer")
    if check("building menu: 'Transfer' is visible", bool(transfer_row_b)):
        check("building menu: 'Transfer' is enabled",
              transfer_row_b.get("enabled") is True)
        click_widget_center(port, transfer_row_b)
        time.sleep(0.3)
        session_b = get_session(port)
        ok_b = isinstance(session_b, dict)
        check("activating 'Transfer' created a session", ok_b, f"got {session_b!r}")
        if ok_b:
            src_b = session_b.get("source") or {}
            dst_b = session_b.get("destination") or {}
            check("session source == the selected acolyte, named",
                  src_b.get("kind") == "unit" and src_b.get("id") == uid,
                  f"got {src_b!r}")
            check("session destination == the storage building, named",
                  dst_b.get("kind") == "building" and dst_b.get("id") == bid,
                  f"got {dst_b!r}")
            contract_b = session_b.get("contract") or {}
            check("session contract.state == 'queued'",
                  contract_b.get("state") == "queued", f"got {contract_b!r}")
            check("session carries NO operation field (#1085 removed it)",
                  "operation" not in contract_b, f"got {contract_b!r}")
    clear_session(port)
    close_menu(port)

    # ------------------------------------------------------------------
    # Scenario 2: right-click the technomule.
    # ------------------------------------------------------------------
    print("== unit destination (technomule) ==")
    if not right_click_unit(port, mule_uid, fb_w, fb_h, "technomule"):
        quit_engine(port, proc)
        return 1

    info_row = find_widget(port, "Info")
    check("unit menu: 'Info' still appears (requirement 7 regression)", bool(info_row))
    transfer_row_u = find_widget(port, "Transfer")
    if check("unit menu: 'Transfer' is visible on the technomule", bool(transfer_row_u)):
        check("unit menu: 'Transfer' is enabled",
              transfer_row_u.get("enabled") is True)
        click_widget_center(port, transfer_row_u)
        time.sleep(0.3)
        session_u = get_session(port)
        ok_u = isinstance(session_u, dict)
        check("activating 'Transfer' created a session", ok_u, f"got {session_u!r}")
        if ok_u:
            src_u = session_u.get("source") or {}
            dst_u = session_u.get("destination") or {}
            check("session source == the selected acolyte, named",
                  src_u.get("kind") == "unit" and src_u.get("id") == uid,
                  f"got {src_u!r}")
            check("session destination == the technomule, named",
                  dst_u.get("kind") == "unit" and dst_u.get("id") == mule_uid,
                  f"got {dst_u!r}")
            contract_u = session_u.get("contract") or {}
            check("session contract.state == 'queued'",
                  contract_u.get("state") == "queued", f"got {contract_u!r}")
            check("session carries NO operation field (#1085 removed it)",
                  "operation" not in contract_u, f"got {contract_u!r}")
    clear_session(port)
    close_menu(port)

    # ------------------------------------------------------------------
    # Scenario 3: #1085 section 9's widening -- a SECOND player acolyte.
    # ------------------------------------------------------------------
    print("== unit destination (a second player acolyte, A2 widening) ==")
    if not right_click_unit(port, acolyte2_uid, fb_w, fb_h, "acolyte"):
        quit_engine(port, proc)
        return 1
    check("acolyte menu: 'Info' appears", bool(find_widget(port, "Info")))
    transfer_row_a = find_widget(port, "Transfer")
    if check("acolyte menu: 'Transfer' is visible (no longer technomule-only)",
             bool(transfer_row_a)):
        click_widget_center(port, transfer_row_a)
        time.sleep(0.3)
        session_a = get_session(port)
        ok_a = isinstance(session_a, dict)
        check("activating 'Transfer' on an acolyte created a session", ok_a,
              f"got {session_a!r}")
        if ok_a:
            dst_a = session_a.get("destination") or {}
            check("session destination == the second acolyte, named",
                  dst_a.get("kind") == "unit" and dst_a.get("id") == acolyte2_uid,
                  f"got {dst_a!r}")
    clear_session(port)
    close_menu(port)

    # ------------------------------------------------------------------
    # Scenario 4: the two exclusions that must survive the widening.
    # ------------------------------------------------------------------
    print("== exclusions (self-transfer, non-commandable) ==")
    if right_click_unit(port, uid, fb_w, fb_h, "self"):
        check("self menu: 'Info' appears", bool(find_widget(port, "Info")))
        check("self menu: 'Transfer' is ABSENT (no self-transfer)",
              find_widget(port, "Transfer") is None)
        self_session = get_session(port)
        check("no session was created for a self-transfer",
              self_session in (None, "null"), f"got {self_session!r}")
    close_menu(port)

    if right_click_unit(port, wild_uid, fb_w, fb_h, "wildlife"):
        check("wildlife menu: 'Info' appears", bool(find_widget(port, "Info")))
        check("wildlife menu: 'Transfer' is ABSENT (not player-commandable)",
              find_widget(port, "Transfer") is None)
    close_menu(port)

    quit_engine(port, proc)
    if failures:
        print(f"\ntransfer_context_menu_probe: {failures} check(s) FAILED")
        return 1
    print("\ntransfer_context_menu_probe: all checks passed")
    return 0


if __name__ == "__main__":
    sys.exit(main())
