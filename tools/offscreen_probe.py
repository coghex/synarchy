#!/usr/bin/env python3
"""Offscreen render mode probe — the #650 gate.

Boots the engine with --offscreen (full Vulkan render, NO window, no
GLFW, no swapchain) and asserts the mode end to end:

1. Boot + READY, and the real UI flow runs (the loading screen
   completes and the main menu's widgets appear) — which GPU-less
   --headless never does.
2. debug.captureScreenshot returns a valid, NON-BLANK PNG at the
   requested --size.
3. F2 input injection drives the UI windowless: clicking the main
   menu's "Create World" button — located via the F3 ui.dumpWidgets
   oracle, not hardcoded coordinates — lands on the Create World
   screen, and the next screenshot differs from the menu.
4. Parallel instances: a second offscreen engine boots on another port
   WHILE the first is still running; both answer console queries and
   capture screenshots without interfering.
5. (unless --skip-worldgen) End-to-end to the in-game HUD: click
   "Generate World", wait for generation to finish, and assert a
   third, again-different screenshot plus a working world query.
6. (unless --skip-worldgen) Remote portal warning (#779), against the
   SAME generated world from phase 5, located dynamically via
   building.canPlaceAt/building.remoteCheck (never hardcoded world
   coordinates — worldgen is unseeded here, same "oracle, not guessed
   coordinates" rule the issue asks the UI half of this probe to
   follow too): a valid remote position renders a normal (white, not
   red) ghost; clicking it presents the remote-settlement modal
   (located via ui.dumpWidgets, not hardcoded coordinates) without
   spawning anything; a second click while it's open cannot stack a
   second modal; "Choose Another Site" closes it, spawns nothing, and
   leaves placement armed; clicking a nearby valid position (close to
   a real placed location) commits instantly with no modal; re-opening
   and clicking "Establish Here" revalidates and spawns exactly one
   portal, exiting placement.
7. (unless --skip-worldgen) Location discovery-state map icons (#781),
   against the same generated world, located via
   world.listPlacedLocations() (never a hardcoded seed/coordinate): a
   placed ruin shows its undiscovered icon before any player unit
   approaches; loading its chunks (structure physically visible) alone
   does not change the icon; spawning a player-faction unit at the ruin
   flips ONLY that ruin's icon to discovered (a second, un-approached
   ruin's stays undiscovered); the icon stays legible at a second map
   zoom level; rotating the camera keeps the pipeline rendering (the
   icon's screen-upright invariant across all 4 facings is proven
   exactly, at the math level, by the pure Hspec group "Location map
   icons" — this phase only proves the GPU path renders SOMETHING that
   updates on rotation); and the discovered state survives a real
   save -> quit -> fresh restart -> load.

Needs a GPU (Vulkan device) — manual-only, never CI-gated.

Usage: python3 tools/offscreen_probe.py [--port 9418] [--size 1280x720]
       [--skip-worldgen]
"""
from __future__ import annotations

import argparse
import json
import os
import sys
import tempfile
import time

sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
from probelib import boot, poll_until, quit_engine, send, send_json

failures = 0


def check(name: str, ok: bool, detail: str = "") -> bool:
    global failures
    print(f"  [{'PASS' if ok else 'FAIL'}] {name}"
          + (f" — {detail}" if detail and not ok else ""))
    failures += not ok
    return ok


# --------------------------------------------------------------------------
# PNG helpers (PIL)
# --------------------------------------------------------------------------
def png_stats(path: str):
    """(width, height, #distinct colors) or None if unreadable."""
    try:
        from PIL import Image
        with Image.open(path) as im:
            im = im.convert("RGBA")
            colors = im.getcolors(maxcolors=1 << 20)
            return im.width, im.height, (len(colors) if colors else (1 << 20))
    except Exception:
        return None


def png_differs(path_a: str, path_b: str, min_fraction: float = 0.001) -> bool:
    """True when at least min_fraction of pixels differ. (Deliberately
    no getbbox() short-circuit: on RGBA, Pillow 10+ defaults it to
    alpha_only=True, and two fully-opaque frames always bbox to None.)"""
    from PIL import Image, ImageChops
    with Image.open(path_a) as a, Image.open(path_b) as b:
        if a.size != b.size:
            return True
        diff = ImageChops.difference(a.convert("RGBA"), b.convert("RGBA"))
        changed = sum(diff.convert("L").histogram()[1:])
        return changed >= min_fraction * a.width * a.height


# --------------------------------------------------------------------------
# UI helpers (F2 inject + F3 widget oracle)
# --------------------------------------------------------------------------
def widgets(port: int):
    got = send_json(port, "return ui.dumpWidgets()", timeout=10.0)
    return got if isinstance(got, list) else []


def find_widget(port: int, label: str):
    for w in widgets(port):
        if (w.get("label") or "").strip().lower() == label.lower():
            return w
    return None


def click_widget(port: int, label: str) -> bool:
    w = find_widget(port, label)
    if not w:
        return False
    b = w.get("bounds") or {}
    x = int(b.get("x", 0) + b.get("w", 0) / 2)
    y = int(b.get("y", 0) + b.get("h", 0) / 2)
    send(port, f"return input.click({x}, {y})", timeout=10.0)
    return True


def screenshot(port: int, path: str) -> bool:
    got = send_json(port, f"return debug.captureScreenshot('{path}')",
                    timeout=30.0)
    return isinstance(got, dict) and got.get("path") == path


# --------------------------------------------------------------------------
# Remote portal warning (#779) helpers
# --------------------------------------------------------------------------
PORTAL = "acolyte_portal"


def can_place_at(port: int, def_name: str, gx: int, gy: int) -> tuple[bool, str | None]:
    r = send(port,
              f"local v,r = building.canPlaceAt('{def_name}', {gx}, {gy}); "
              f"return tostring(v) .. '|' .. tostring(r)").strip('"')
    valid_s, _, reason_s = r.partition("|")
    return valid_s == "true", (None if reason_s in ("nil", "") else reason_s)


def remote_check(port: int, def_name: str, gx: int, gy: int):
    """(remote: bool, distance: int|None, thresholdTiles: int)."""
    r = send(port,
              f"local rem,d,t = building.remoteCheck('{def_name}', {gx}, {gy}); "
              f"return tostring(rem) .. '|' .. tostring(d) .. '|' .. tostring(t)"
              ).strip('"')
    rem_s, _, rest = r.partition("|")
    dist_s, _, thr_s = rest.partition("|")
    distance = None if dist_s == "nil" else int(float(dist_s))
    threshold = int(float(thr_s)) if thr_s not in ("", "nil") else None
    return rem_s == "true", distance, threshold


def load_region_around(port: int, gx: int, gy: int, radius_chunks: int = 2) -> None:
    cx, cy = gx // 16, gy // 16
    send(port, f"return world.loadChunksInRegion("
               f"{cx - radius_chunks},{cy - radius_chunks},"
               f"{cx + radius_chunks},{cy + radius_chunks})")
    send(port, "return world.waitForChunks(30)", timeout=35)


def goto_and_resolve(port: int, gx: int, gy: int, screen_x: int, screen_y: int):
    """Points the camera at (gx, gy) and returns whatever tile ACTUALLY
    resolves under (screen_x, screen_y) via world.pickTile. The
    isometric projection plus that tile's own terrain height mean the
    screen-centre tile is essentially never exactly (gx, gy) (verified
    empirically in tools/portal_ghost_probe.py's center_on_tile) — worse,
    iteratively CORRECTING toward an exact match assumes a roughly
    linear pixel-to-tile shift, which rough/varied terrain around an
    arbitrary guessed coordinate can violate badly enough to diverge.
    So every caller here works with the RESOLVED tile instead of
    assuming convergence onto a specific guess. Navigating to the SAME
    (gx, gy) again is deterministic (fixed terrain + fixed camera
    target), so re-issuing this exact call later reliably reproduces
    the same resolved tile — that's what re-positions the camera before
    each click below, rather than a fragile iterative correction."""
    send(port, f"camera.goToTile({gx}, {gy}); return 'ok'")
    time.sleep(0.4)
    picked = send_json(port, f"return {{world.pickTile({screen_x}, {screen_y})}}")
    return (picked[0], picked[1]) if picked else None


def find_buildable(port: int, def_name: str, seed_targets, want_remote: bool,
                    screen_x: int, screen_y: int):
    """For each seed (gx, gy), resolve the tile ACTUALLY under the
    screen centre (goto_and_resolve) and classify THAT tile with the
    engine's own oracle (building.canPlaceAt / building.remoteCheck)
    rather than the seed coordinates themselves — an oracle-driven
    search, matching how ruin/location probes (e.g.
    tools/portal_ghost_probe.py) locate real worldgen features, robust
    to the seed itself landing on unbuildable or off-target terrain.
    Returns (seed_gx, seed_gy, resolved_gx, resolved_gy, distance,
    thresholdTiles) — re-navigating with the SEED coordinates
    (goto_and_resolve) later reliably reproduces the resolved tile."""
    for gx, gy in seed_targets:
        resolved = goto_and_resolve(port, gx, gy, screen_x, screen_y)
        if not resolved:
            continue
        rgx, rgy = resolved
        load_region_around(port, rgx, rgy)
        valid, _ = can_place_at(port, def_name, rgx, rgy)
        if not valid:
            continue
        remote, distance, threshold = remote_check(port, def_name, rgx, rgy)
        if remote == want_remote:
            return gx, gy, rgx, rgy, distance, threshold
    return None


def arm_portal_placement(port: int) -> None:
    send(port,
         "require('scripts.build_tool').enterPlacement("
         "{kind='building', def='" + PORTAL + "', isStarting=true}); "
         "return 'ok'")


def placement_mode(port: int) -> str:
    return send(port,
                "return require('scripts.build_tool').state.mode").strip('"')


def building_count(port: int, def_name: str) -> int:
    r = send(port, "return building.list()")
    return r.count(def_name)


def click_at_seed(port: int, seed_gx: int, seed_gy: int, resolved_gx: int,
                   resolved_gy: int, screen_x: int, screen_y: int) -> bool:
    """Re-navigates to the seed tile (deterministically reproducing the
    same resolved tile — see goto_and_resolve) and clicks screen centre.
    Returns whether the re-resolved tile still matches the one the
    caller already classified; the click always fires regardless (a
    mismatch is a probe-environment anomaly worth surfacing via the
    caller's own check, not a reason to skip exercising the feature)."""
    resolved = goto_and_resolve(port, seed_gx, seed_gy, screen_x, screen_y)
    send(port, f"return input.moveMouse({screen_x}, {screen_y})")
    send(port, f"return input.click({screen_x}, {screen_y})")
    return resolved == (resolved_gx, resolved_gy)


def remote_warning_phase(port: int, cx0: int, cy0: int, shots: str) -> None:
    print("== remote portal warning (#779) ==")

    located = send_json(port, "return world.listPlacedLocations()", timeout=10.0)
    located = located if isinstance(located, list) else []

    # -- find a valid, remote position: seed points scattered across the
    # world, resolved + oracle-classified rather than assumed (see
    # find_buildable/goto_and_resolve).
    remote_seeds = [
        (400, 400), (900, 300), (300, 900), (1400, 1400), (1700, 500),
        (500, 1700), (100, 1200), (1200, 100), (1900, 900), (900, 1900),
    ]
    remote_hit = find_buildable(port, PORTAL, remote_seeds, want_remote=True,
                                 screen_x=cx0, screen_y=cy0)
    if not check("found a valid remote buildable position", bool(remote_hit)):
        return
    rseed_gx, rseed_gy, rgx, rgy, rdist, rthr = remote_hit
    print(f"  remote position ({rgx},{rgy}) distance={rdist} threshold={rthr}")

    # -- find a valid, NON-remote position near an actual placed
    # location (needs at least one placed location to be meaningful).
    nearby_hit = None
    if located:
        loc = located[0]
        lgx, lgy = loc.get("gx", 0), loc.get("gy", 0)
        nearby_seeds = [
            (lgx + dx, lgy + dy)
            for dx, dy in ((40, 0), (-40, 0), (0, 40), (0, -40),
                           (60, 20), (-60, -20), (20, 60), (-20, -60),
                           (90, 0), (-90, 0), (0, 90), (0, -90))
        ]
        nearby_hit = find_buildable(port, PORTAL, nearby_seeds, want_remote=False,
                                     screen_x=cx0, screen_y=cy0)
    check("world has at least one placed location", bool(located))
    if not check("found a valid nearby (non-remote) buildable position",
                 bool(nearby_hit)):
        nearby_hit = None
    if nearby_hit:
        nseed_gx, nseed_gy, ngx, ngy, ndist, nthr = nearby_hit
        print(f"  nearby position ({ngx},{ngy}) distance={ndist} threshold={nthr}")

    before = building_count(port, PORTAL)

    # -- valid remote position renders a normal (white) ghost: hover it
    # while armed and confirm canPlaceAt (which drives the ghost tint)
    # still reports valid — remote is never coloured invalid/red.
    arm_portal_placement(port)
    hover_resolved = goto_and_resolve(port, rseed_gx, rseed_gy, cx0, cy0)
    check("camera resolves the remote position", hover_resolved == (rgx, rgy),
          f"got {hover_resolved}")
    send(port, f"return input.moveMouse({cx0}, {cy0})")
    time.sleep(0.3)
    valid_now, _ = can_place_at(port, PORTAL, rgx, rgy)
    check("remote position still reports canPlaceAt=true (white ghost, "
          "never red)", valid_now)
    shot_ghost = os.path.join(shots, "remote_ghost.png")
    check("remote-ghost screenshot answers", screenshot(port, shot_ghost))

    # -- clicking it presents the modal without spawning anything.
    check("click lands on the remote position",
          click_at_seed(port, rseed_gx, rseed_gy, rgx, rgy, cx0, cy0))
    time.sleep(0.3)
    modal_up = poll_until(5.0, lambda: find_widget(port, "Establish Here"))
    check("clicking the remote position presents the modal", bool(modal_up))
    check("no portal spawned while the modal is open",
          building_count(port, PORTAL) == before)
    shot_modal = os.path.join(shots, "remote_modal.png")
    check("modal screenshot answers", screenshot(port, shot_modal))

    # -- a second click while it's open cannot stack a second modal
    # (world clicks don't even pass through the modal boundary, so this
    # click is expected to land on the modal itself, not the world).
    send(port, f"return input.click({cx0}, {cy0})")
    time.sleep(0.3)
    establish_widgets = [w for w in widgets(port)
                         if (w.get("label") or "").strip().lower()
                         == "establish here"]
    check("repeated clicks do not stack a second modal",
          len(establish_widgets) == 1, f"found {len(establish_widgets)}")

    # -- "Choose Another Site" closes it, spawns nothing, placement
    # stays armed.
    check("click 'Choose Another Site'",
          click_widget(port, "Choose Another Site"))
    time.sleep(0.3)
    check("modal closed after Choose Another Site",
          not find_widget(port, "Establish Here"))
    check("still no portal spawned after cancel",
          building_count(port, PORTAL) == before)
    check("placement remains armed after cancel",
          placement_mode(port) == "placement")
    shot_cancelled = os.path.join(shots, "remote_cancelled.png")
    if check("post-cancel screenshot answers",
             screenshot(port, shot_cancelled)):
        check("post-cancel frame differs from the open-modal frame "
              "(modal gone, placement context unchanged underneath)",
              png_differs(shot_modal, shot_cancelled))

    # -- re-open, then "Establish Here" revalidates and spawns exactly
    # one portal, exiting placement.
    click_at_seed(port, rseed_gx, rseed_gy, rgx, rgy, cx0, cy0)
    modal_up2 = poll_until(5.0, lambda: find_widget(port, "Establish Here"))
    check("re-clicking the remote position re-presents the modal",
          bool(modal_up2))
    check("click 'Establish Here'", click_widget(port, "Establish Here"))
    time.sleep(0.5)
    check("exactly one portal placed after confirming",
          building_count(port, PORTAL) == before + 1,
          f"before={before} after={building_count(port, PORTAL)}")
    check("modal closed after confirming", not find_widget(port, "Establish Here"))
    check("placement mode exited after a confirmed remote placement",
          placement_mode(port) == "off")

    # -- a nearby valid position commits instantly, no modal.
    if nearby_hit:
        before2 = building_count(port, PORTAL)
        arm_portal_placement(port)
        click_at_seed(port, nseed_gx, nseed_gy, ngx, ngy, cx0, cy0)
        time.sleep(0.5)
        check("clicking a nearby valid position never presents the modal",
              not find_widget(port, "Establish Here"))
        check("clicking a nearby valid position commits instantly "
              "(single click, no confirmation)",
              building_count(port, PORTAL) == before2 + 1,
              f"before={before2} after={building_count(port, PORTAL)}")


# --------------------------------------------------------------------------
# Location discovery-state map icons (#781) helpers
# --------------------------------------------------------------------------
def list_locations(port: int, page: str = "main_world") -> list[dict]:
    got = send_json(port, f"return world.listPlacedLocations('{page}')", timeout=10.0)
    return got if isinstance(got, list) else []


def zoom_fade_end(port: int) -> float:
    r = send(port, "return camera.getZoomFadeEnd()")
    return float(r)


def set_zoom(port: int, zoom: float) -> None:
    send(port, f"camera.setZoom({zoom}); return 'ok'")


def center_on(port: int, gx: int, gy: int) -> None:
    send(port, f"camera.goToTile({gx}, {gy}); return 'ok'")
    time.sleep(0.3)


def spawn_player_unit(port: int, gx: int, gy: int, page: str = "main_world") -> int:
    r = send(port,
              f"return unit.spawn('acolyte', {gx}, {gy}, nil, 'player', '{page}')")
    try:
        return int(float(r.strip('"')))
    except ValueError:
        return -1


def location_map_icons_phase(port: int, w: int, h: int, shots: str):
    """The #781 gate: paired discovery-state zoom-map icons, verified
    through screenshots + the world.listPlacedLocations() oracle against
    THIS run's real (unseeded) worldgen — never a hardcoded seed, map
    coordinate, or click position. The precise wrap/seam/duplicate-icon
    geometry is exhaustively covered by the pure, GPU-free Hspec group
    'Location map icons' (Test.Headless.Location.MapIcons) — this phase
    proves the full GPU render pipeline actually surfaces that same
    behaviour on screen, not a second derivation of the wrap math."""
    print("== location map icons (#781) ==")
    cx0, cy0 = w // 2, h // 2

    locations = list_locations(port)
    if not check("world has at least two placed locations "
                 "(need one to approach, one to leave alone)",
                 len(locations) >= 2, f"found {len(locations)}"):
        return None
    target, control = locations[0], locations[1]
    tgx, tgy = target["gx"], target["gy"]
    ccx, ccy = control["gx"], control["gy"]

    fade_end = zoom_fade_end(port)
    full_zoom = fade_end * 1.5

    # -- full map visibility, centred on the target ruin, BEFORE any
    # player unit has approached: undiscovered icon. camera.goToTile
    # itself resets zoom (its "zoomSafe" branch, Engine.Scripting.Lua.
    # API.Camera.cameraGotoTileFn), so it must run BEFORE setZoom, never
    # after, or the map-visibility zoom gets clobbered back to 0.5.
    center_on(port, tgx, tgy)
    set_zoom(port, full_zoom)
    time.sleep(0.3)
    check("target ruin starts undiscovered per world.listPlacedLocations()",
          not list_locations(port)[0].get("discovered")
          if list_locations(port) else False)
    shot_undiscovered = os.path.join(shots, "icon_undiscovered.png")
    check("undiscovered-icon screenshot answers",
          screenshot(port, shot_undiscovered))

    # -- the terrain/structure being physically visible (chunks around
    # the ruin loaded) must not, by itself, change the icon: load the
    # region and re-shoot before any unit is near it. This is checked at
    # the STATE level (world.listPlacedLocations' discovered flag), not
    # by pixel-diffing against 'shot_undiscovered' — loading a 2-chunk
    # radius legitimately repaints most of the frame with newly-visible
    # terrain, which would swamp any icon-sized pixel delta and make a
    # full-frame diff meaningless here. 'shot_loaded' below instead
    # becomes the TERRAIN-STABLE baseline the discovery comparison uses.
    load_region_around(port, tgx, tgy, radius_chunks=2)
    time.sleep(0.3)
    still_undiscovered = next(
        (loc for loc in list_locations(port)
         if loc.get("gx") == tgx and loc.get("gy") == tgy), None)
    check("loading the ruin's chunks (structure visible) does not "
          "discover it", bool(still_undiscovered)
          and not still_undiscovered.get("discovered"))
    shot_loaded = os.path.join(shots, "icon_loaded_not_discovered.png")
    check("post-load screenshot answers", screenshot(port, shot_loaded))

    # -- approaching the target ruin (a player-faction unit inside its
    # discovery margin) flips ONLY that ruin to discovered.
    player_uid = spawn_player_unit(port, tgx, tgy)
    check("player unit spawned at the target ruin", player_uid >= 0)
    target_discovered = poll_until(10.0, lambda: next(
        (loc.get("discovered") for loc in list_locations(port)
         if loc.get("gx") == tgx and loc.get("gy") == tgy), False))
    check("approaching the target ruin flips it to discovered",
          bool(target_discovered))
    control_still_hidden = next(
        (loc for loc in list_locations(port)
         if loc.get("gx") == ccx and loc.get("gy") == ccy), None)
    check("the un-approached control ruin stays undiscovered",
          bool(control_still_hidden)
          and not control_still_hidden.get("discovered"))

    time.sleep(0.3)
    shot_discovered = os.path.join(shots, "icon_discovered.png")
    if check("discovered-icon screenshot answers",
             screenshot(port, shot_discovered)):
        # Compared against 'shot_loaded' (same loaded terrain, taken
        # right before this unit spawned) rather than 'shot_undiscovered'
        # — see the note above on why a terrain-stable baseline is the
        # only pixel-diff pair that isolates the discovery-driven change.
        check("the icon frame visibly changes once the ruin is discovered",
              png_differs(shot_loaded, shot_discovered, min_fraction=0.001))

    # -- readable at a second, different map zoom level.
    set_zoom(port, full_zoom * 1.6)
    time.sleep(0.3)
    shot_zoom2 = os.path.join(shots, "icon_zoom2.png")
    if check("second-zoom-level screenshot answers",
             screenshot(port, shot_zoom2)):
        st = png_stats(shot_zoom2)
        check("second-zoom-level frame is not blank",
              bool(st) and st[2] >= 3, f"distinct colors: {st and st[2]}")
    # -- rotating the map moves the icon with its location (the frame
    # changes) while the render pipeline keeps working; upright-ness
    # itself is proven exactly by the Hspec group's axis-aligned-square
    # assertion across all four facings, not re-derived from pixels here.
    # (goToTile-before-setZoom again — see the note above.)
    center_on(port, tgx, tgy)
    set_zoom(port, full_zoom)
    time.sleep(0.3)
    shot_pre_rotate = os.path.join(shots, "icon_pre_rotate.png")
    screenshot(port, shot_pre_rotate)
    send(port, "camera.rotateCW(); return 'ok'")
    time.sleep(0.5)
    shot_rotated = os.path.join(shots, "icon_rotated.png")
    if check("post-rotation screenshot answers", screenshot(port, shot_rotated)):
        check("the frame changes after rotating the camera",
              png_differs(shot_pre_rotate, shot_rotated))
    send(port, "camera.rotateCCW(); return 'ok'")
    time.sleep(0.3)

    # -- save (quit -> fresh restart -> load happens back in main(), the
    # same shape every other save-persistence check in this probe suite
    # uses — see 'location_map_icons_reload_check').
    check("save the world", "true" in send(
        port, "engine.saveWorld('main_world', 'offscreen_icon_test'); "
              "return 'true'").lower())
    time.sleep(0.5)
    return (tgx, tgy)


def location_map_icons_reload_check(port: int, tgx: int, tgy: int) -> None:
    """Continuation of 'location_map_icons_phase' after a fresh restart +
    load (run against a NEW engine instance, mirroring how the rest of
    this probe's own quit/restart pattern works elsewhere)."""
    send(port, "engine.loadSave('offscreen_icon_test'); return 'queued'")
    # world.waitForInit blocks until the load's world-thread work is done
    # (the same barrier tools/multiworld_save_probe.py uses after a load —
    # engine.loadSave itself only QUEUES the load; getInitProgress tracks
    # fresh generation, not a load, so it never reaches phase 3 here).
    send(port, "return world.waitForInit(120)", timeout=125.0)
    send(port, "world.show('main_world'); return 'ok'")
    time.sleep(0.5)
    reloaded = next(
        (loc for loc in list_locations(port)
         if loc.get("gx") == tgx and loc.get("gy") == tgy), None)
    check("the discovered icon state survives save -> quit -> restart -> load",
          bool(reloaded) and bool(reloaded.get("discovered")))


def main() -> int:
    ap = argparse.ArgumentParser(description=__doc__)
    ap.add_argument("--port", type=int, default=9418)
    ap.add_argument("--size", default="1280x720")
    ap.add_argument("--skip-worldgen", action="store_true",
                    help="skip the Generate World -> HUD phase (~1 min)")
    args = ap.parse_args()

    w, h = (int(v) for v in args.size.lower().split("x"))
    shots = tempfile.mkdtemp(prefix="offscreen_probe_")

    print(f"== offscreen boot (port {args.port}, {args.size}) ==")
    proc = boot(args.port, mode=("--offscreen",),
                args=["--size", args.size], label="offscreen engine")

    # -- 1. real UI flow: the loading screen finishes and the menu's
    # widgets exist. GPU-less --headless never gets here, so this is
    # the "full UI stack actually booted" assertion.
    menu_up = poll_until(60.0, lambda: find_widget(args.port, "Create World"))
    check("loading screen -> main menu (Create World widget visible)",
          bool(menu_up))

    # -- 2. non-blank screenshot at the requested size.
    shot_menu = os.path.join(shots, "menu.png")
    check("captureScreenshot answers", screenshot(args.port, shot_menu))
    st = png_stats(shot_menu)
    check(f"menu PNG valid at {w}x{h}", bool(st) and st[0] == w and st[1] == h,
          f"got {st}")
    check("menu PNG is not blank (>= 3 distinct colors)",
          bool(st) and st[2] >= 3, f"distinct colors: {st and st[2]}")

    # -- 3. windowless input injection: click Create World via the F3
    # oracle's bounds and require both the widget change and a visibly
    # different frame.
    check("click 'Create World' (bounds from ui.dumpWidgets)",
          click_widget(args.port, "Create World"))
    create_up = poll_until(20.0, lambda: find_widget(args.port, "Generate World"))
    check("create-world screen reached (Generate World widget visible)",
          bool(create_up))
    shot_create = os.path.join(shots, "create.png")
    if check("second screenshot answers", screenshot(args.port, shot_create)):
        check("create-world frame differs from menu frame",
              png_differs(shot_menu, shot_create))

    # -- 4. parallel instances: a second engine while the first runs.
    port2 = args.port + 1
    print(f"== parallel second instance (port {port2}) ==")
    proc2 = boot(port2, mode=("--offscreen",),
                 args=["--size", args.size], label="second offscreen engine")
    menu2 = poll_until(60.0, lambda: find_widget(port2, "Create World"))
    check("second instance reaches its own menu", bool(menu2))
    shot2 = os.path.join(shots, "second.png")
    check("second instance captures its own screenshot",
          screenshot(port2, shot2) and bool(png_stats(shot2)))
    check("first instance still answering alongside the second",
          bool(find_widget(args.port, "Generate World")))
    quit_engine(port2, proc2)

    # -- 5. through real worldgen to the in-game HUD.
    if not args.skip_worldgen:
        print("== Generate World -> in-game HUD (takes ~1 min) ==")
        check("click 'Generate World'", click_widget(args.port, "Generate World"))

        def world_done():
            got = send(args.port, "local p = world.getInitProgress(); return p",
                       timeout=5.0)
            return got.strip() == "3"  # phase 3 = done

        check("worldgen completes (phase 3)", bool(poll_until(300.0, world_done,
                                                              interval=2.0)))
        # Generation done -> the screen offers Regenerate/Continue;
        # Continue is the click that actually enters the game.
        cont = poll_until(60.0, lambda: find_widget(args.port, "Continue"))
        check("post-generation Continue button appears", bool(cont))
        check("click 'Continue'", click_widget(args.port, "Continue"))
        hud_up = poll_until(60.0, lambda: not find_widget(args.port, "Continue"))
        check("create-world screen dismissed (in-game view)", bool(hud_up))
        time.sleep(3.0)  # let the first world frames render
        shot_hud = os.path.join(shots, "hud.png")
        if check("in-game screenshot answers",
                 screenshot(args.port, shot_hud)):
            check("in-game frame differs from create-world frame",
                  png_differs(shot_create, shot_hud))
        got = send_json(args.port, "return world.getChunkInfo(0, 0)", timeout=10.0)
        check("world query answers in-game", isinstance(got, dict))

        # -- 6. remote portal warning (#779), against this same world.
        remote_warning_phase(args.port, w // 2, h // 2, shots)

        # -- 7. location discovery-state map icons (#781), against this
        # same world.
        icon_target = location_map_icons_phase(args.port, w, h, shots)
    else:
        icon_target = None

    quit_engine(args.port, proc)

    # -- 7 (cont'd): fresh restart -> load, proving the discovered icon
    # state (not the one-off in-process view) actually round-trips.
    if icon_target:
        print("== fresh restart -> load (icon persistence, #781) ==")
        proc3 = boot(args.port, mode=("--offscreen",),
                     args=["--size", args.size],
                     label="offscreen engine (icon reload)")
        location_map_icons_reload_check(args.port, *icon_target)
        quit_engine(args.port, proc3)

    print(f"\nscreenshots kept in {shots}")
    if failures:
        print(f"offscreen_probe: {failures} check(s) FAILED")
        return 1
    print("offscreen_probe: all checks passed")
    return 0


if __name__ == "__main__":
    sys.exit(main())
