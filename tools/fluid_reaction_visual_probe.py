#!/usr/bin/env python3
"""Offscreen evidence that a solidification reaches BOTH live
presentations without a page reload (#2485, requirement 14; FR-4 of epic
#2480).

The contact itself is visually silent by the owner's decision — no
side-deco marker, no effect — so the stone IS the presentation, and there
are two of them:

* the DETAILED tile render, which rebuilds its quads from
  ``wsTilesRef`` and therefore only needs the quad-cache invalidation
  every terrain edit already does; and
* the ZOOM map, which does not. Its renderer samples a precomputed atlas
  texture whose terrain pixels are produced once, at page
  initialization. Clearing the quad caches cannot change a single pixel
  of it — #2485 regenerates the affected chunk's block and republishes
  the atlas, and this probe is the evidence that the republication
  actually reaches the screen.

Every capture is taken in ONE process at a FIXED camera, zoom and sun
angle, with the engine paused around each one, and each is paired with an
immediate re-capture of the same state. That control is what makes the
before/after difference mean something: a frame that is byte-identical to
its own re-capture but differs from the pre-reaction frame changed
because of the reaction, not because the renderer is noisy.

"Without reloading" is asserted, not assumed: the page's generated id,
its active-world id and ``engine.getLoadStatus()`` are all read either
side and must show that no load transaction ran at all.

Needs a real Vulkan device (``--offscreen``); classified ``needs-gpu`` in
``tools/ci_probes.py``.

Usage:
  python3 tools/fluid_reaction_visual_probe.py [--port 9283] [--shots DIR]
"""
from __future__ import annotations

import argparse
import glob
import os
import sys
import tempfile
import time
from pathlib import Path

from probelib import (boot, quit_engine, poll_until, send, send_json,
                      viewport, win_to_fb)
from offscreen_probe import (find_widget, png_differs, png_region_changed_pixels,
                             png_stats, screenshot)

REPO = Path(__file__).resolve().parent.parent

SEED = 771
WORLD_SIZE = 8
PLATE_COUNT = 3
# The page the real gameplay path creates. `scripts/world_view.lua`'s
# createWorld makes "main_world" from `worldView.worldParams`, and a page
# this probe had made itself would be replaced by it the moment the world
# view opened.
PAGE = "main_world"
PRODUCTS = ("basalt", "obsidian")
# The zoom pass colours a tile from the material's palette entry, which
# `World.ZoomMap.ColorPalette.buildColorPalette` derives from the
# material's own `zoom:` chunk texture — so the probe derives its
# expectation from the same file rather than from a hardcoded colour.
PRODUCT_ZOOM_TEXTURE = {
    "basalt":   "assets/textures/world/zoommap/basalt_chunk.png",
    "obsidian": "assets/textures/world/zoommap/obsidian_chunk.png",
}
FRAME = (1024, 768)
# Detailed tiles are drawn below World.Grid.zoomFadeStart (1.2) and the
# zoom map is fully opaque at or above zoomFadeEnd (1.6).
DETAIL_ZOOM = 0.5
MAP_ZOOM = 2.0
REACTION_TIMEOUT = 90.0
# One chunk's zoom quad is a small part of a whole-world map. Generous
# enough that the camera's zoom step does not have to be pinned to the
# pixel, tight enough that a repaint of the map fails it.
MAP_REGION_FRACTION = 0.05


class Checks:
    def __init__(self) -> None:
        self.failed = 0

    def ok(self, cond: bool, label: str) -> None:
        print(f"  [{'PASS' if cond else 'FAIL'}] {label}")
        if not cond:
            self.failed += 1


def as_int(s: str):
    try:
        return int(float(s))
    except (TypeError, ValueError):
        return None


def bootstrap_materials(port: int) -> None:
    for path in sorted(glob.glob("data/materials/*.yaml")):
        send(port, f"engine.loadMaterialYaml('{path}'); return 'ok'")


# Every world read here is PAGE-SCOPED. The offscreen session has a UI
# in front of it and more than one page can become the active world; a
# bare active-world read would then answer about a different world than
# the one the reaction is happening in.
def terrain_at(port: int, gx: int, gy: int):
    return as_int(send(
        port,
        f"local s, t = world.getTerrainAt({gx}, {gy}, '{PAGE}'); return t"))


def surface_at(port: int, gx: int, gy: int):
    return as_int(send(
        port,
        f"local s, t = world.getTerrainAt({gx}, {gy}, '{PAGE}'); return s"))


def material_at(port: int, gx: int, gy: int) -> str:
    raw = send(port,
               f"local i, n = world.getMaterialAt({gx}, {gy}, '{PAGE}'); "
               f"return n")
    return raw.strip().strip('"')


def active_id(port: int) -> str:
    return send(port, "return world.getActiveWorldId()").strip().strip('"')


def set_paused(port: int, on: bool) -> None:
    send(port, f"engine.setPaused({'true' if on else 'false'}); return 'ok'")
    time.sleep(0.4)


def freeze_frame(port: int) -> None:
    """Everything that would move a pixel on its own. The sun angle is
    pinned because the day/night cycle relights the whole terrain, and
    the time scale because the calendar drives it."""
    send(port, f"world.setTimeScale('{PAGE}', 0); return 'ok'")
    send(port, f"world.setSunAngle('{PAGE}', 0.5); return 'ok'")


def capture_pair(port: int, chk: Checks, shots: str, name: str, box=None):
    """One frame plus its own immediate re-capture, and how much the two
    differ.

    That difference is the NOISE FLOOR of this view in this process — the
    engine keeps rendering while paused and the scene is not bit-frozen —
    and it is what makes the before/after difference mean something: a
    change larger than the floor is a change in the scene, not in the
    renderer. Returned rather than asserted to be zero, which it is not.
    """
    a = os.path.join(shots, f"{name}.png")
    b = os.path.join(shots, f"{name}_control.png")
    chk.ok(screenshot(port, a), f"captured {name}.png")
    time.sleep(0.6)
    chk.ok(screenshot(port, b), f"captured {name}_control.png")
    if box is None:
        noise = 0 if not png_differs(a, b, min_fraction=0.0) else \
            whole_frame_changed(a, b)
    else:
        noise = png_region_changed_pixels(a, b, box) or 0
    print(f"  [note] {name} noise floor: {noise} px"
          + ("" if box is None else f" inside {box}"))
    return a, noise


def mean_colour(path: str, box) -> tuple[float, float, float] | None:
    """Mean RGB inside box=(x, y, w, h), ignoring fully transparent
    pixels."""
    from PIL import Image
    x, y, w, h = box
    try:
        with Image.open(path) as im:
            px = list(im.convert("RGBA").crop((x, y, x + w, y + h)).getdata())
    except Exception:
        return None
    opaque = [p for p in px if p[3] > 0]
    if not opaque:
        return None
    n = len(opaque)
    return (sum(p[0] for p in opaque) / n,
            sum(p[1] for p in opaque) / n,
            sum(p[2] for p in opaque) / n)


def texture_mean_colour(path: str):
    """The mean opaque colour of a material's zoom chunk texture — the
    same image the engine's palette entry for that material is built
    from."""
    from PIL import Image
    with Image.open(path) as im:
        px = list(im.convert("RGBA").getdata())
    opaque = [p for p in px if p[3] > 0]
    if not opaque:
        return None
    n = len(opaque)
    return (sum(p[0] for p in opaque) / n,
            sum(p[1] for p in opaque) / n,
            sum(p[2] for p in opaque) / n)


def png_diff_bbox(path_a: str, path_b: str):
    """Bounding box (x, y, w, h) of every pixel that differs between two
    frames, or None when nothing differs.

    The zoom map draws the world through its own atlas layout rather than
    the tile hit-test's transform, so ``world.pickTile`` does not say
    where a tile's zoom pixels are. What the map DOES give is a change
    confined to the affected chunk's own quad, and that is what this
    measures: where the difference is, and how big it is.
    """
    from PIL import Image, ImageChops
    with Image.open(path_a) as a, Image.open(path_b) as b:
        if a.size != b.size:
            return None
        diff = ImageChops.difference(a.convert("RGB"), b.convert("RGB"))
        return diff.convert("L").point(lambda v: 255 if v else 0).getbbox()


def colour_distance(a, b) -> float:
    return sum((x - y) ** 2 for x, y in zip(a, b)) ** 0.5


def whole_frame_changed(path_a: str, path_b: str) -> int:
    from PIL import Image, ImageChops
    with Image.open(path_a) as a, Image.open(path_b) as b:
        diff = ImageChops.difference(a.convert("RGBA"), b.convert("RGBA"))
        return sum(diff.convert("L").histogram()[1:])


def reach_world_view(chk: Checks, port: int) -> bool:
    """Straight to the gameplay surface with a SMALL pinned world.

    The debug console sandboxes ``_G``, but ``package.loaded`` holds every
    game module. ``uiManager.showMenu('world_view')`` is the one entry a
    new game, a loaded save and the test arena all converge on, and the
    world it opens is the one ``scripts/world_view.lua`` builds from
    ``worldView.worldParams`` — so pinning those parameters is how this
    probe gets a small, deterministic world through the REAL creation
    path. Calling ``world.init`` directly instead does not work: the world
    view creates ``main_world`` on top of it and renders that.
    """
    menu = poll_until(120.0, lambda: find_widget(port, "Create World"),
                      interval=1.0)
    chk.ok(bool(menu), "the loading screen reached the main menu")
    bootstrap_materials(port)
    send(port,
         f"local wv = package.loaded['scripts.world_view']; "
         f"wv.worldParams = {{ seed = {SEED}, worldSize = {WORLD_SIZE}, "
         f"plateCount = {PLATE_COUNT} }}; return 'ok'")
    send(port, "package.loaded['scripts.ui_manager'].showMenu('world_view'); "
               "return 'ok'")
    active = poll_until(300.0, lambda: active_id(port) == PAGE, interval=2.0)
    chk.ok(bool(active),
           f"'{PAGE}' is the active world (got {active_id(port)!r})")
    if not active:
        return False
    send(port, "return world.waitForInit(180)", timeout=190)
    time.sleep(3.0)
    resolved = as_int(send(port, f"return world.getSeed('{PAGE}')"))
    chk.ok(resolved == SEED,
           f"the world that came up carries the pinned seed {SEED} "
           f"(got {resolved!r}) -- otherwise this probe is grading a "
           f"different world from the one it pinned")
    return True


def pick_tile(port: int, px: int, py: int):
    """``world.pickTile`` answers with TWO values, not a table, so the
    console call packs them into one."""
    got = send_json(port,
                    f"local x, y = world.pickTile({px}, {py}); "
                    f"if x == nil then return nil end; return {{x = x, y = y}}",
                    timeout=10.0)
    if isinstance(got, dict):
        return as_int(str(got.get("x"))), as_int(str(got.get("y")))
    return None, None


def find_contact_pair(port: int, around):
    """A dry, above-sea-level pair of horizontally adjacent land tiles,
    with the one-z STEP between them BUILT rather than searched for.

    The step is what makes the contact happen at all rather than happen
    to. ``world.setFluidTile`` gives every cell the same single level of
    depth, so a FLAT pair leaves both surfaces equal and the lateral
    phase — which only moves fluid down a surface gradient — plans
    nothing; the two sit side by side indefinitely unless the surrounding
    terrain happens to drain one of them. With the lava side one z higher
    its surface stands above the water's and gravity carries it down into
    it, which is the contact.

    A generated page is not guaranteed to contain a suitable natural
    step anywhere near the camera, so one ordinary ``world.addTile`` —
    the same player edit the debug terrain tool makes — supplies it. That
    edit lands BEFORE any capture, so nothing it changes is attributed to
    the reaction.

    Dry, so each fluid edit genuinely places the fluid it names rather
    than replacing a cell the page already had; above sea level, so the
    page's own ocean is not what is reacting.

    The search runs OUTWARD from the tile the camera is actually looking
    at, so the contact lands in frame rather than at a fixed world
    coordinate the camera may not be over.
    """
    cx, cy = around
    for radius in range(0, 10):
        for dy in range(-radius, radius + 1):
            for dx in range(-radius, radius + 1):
                if max(abs(dx), abs(dy)) != radius:
                    continue
                gx, gy = cx + dx, cy + dy
                ta = terrain_at(port, gx, gy)
                tb = terrain_at(port, gx + 1, gy)
                if ta is None or tb is None or ta != tb or ta <= 0:
                    continue
                if surface_at(port, gx, gy) != ta:
                    continue
                if surface_at(port, gx + 1, gy) != tb:
                    continue
                return (gx, gy), (gx + 1, gy), ta
    return None, None, None


def raise_lava_side(chk: Checks, port: int, tile, baseline: int):
    """One ordinary add-tile, so the lava sits a z above the water."""
    send(port, f"world.addTile('{PAGE}', {tile[0]}, {tile[1]}, 'loam'); "
               f"return 'ok'")
    deadline = time.time() + 20.0
    top = baseline
    while time.time() < deadline:
        top = terrain_at(port, *tile)
        if top is not None and top > baseline:
            break
        time.sleep(0.2)
    chk.ok(top == baseline + 1,
           f"built the one-z step the contact needs: {tile} rose from "
           f"{baseline} to {top}")
    return top


def screen_box_for(port: int, tile, vp: dict, step: int = 4, reach: int = 160):
    """The on-screen box of one tile, in FRAMEBUFFER pixels (what the
    captures are in; ``world.pickTile`` speaks window coordinates and the
    two differ on a HiDPI display).

    Seeded by a coarse scan outward from the screen centre rather than
    by the centre pixel alone: the target is chosen from what the camera
    is already looking at, but it is the first FLAT DRY pair found
    searching outward, which can be a tile or two off centre. Once any
    pixel of the tile is found the box is grown from it along both axes,
    still through the engine's own mapping rather than a re-derived
    isometric transform.

    ``None`` when nothing within reach of the centre resolves to the
    tile, which the caller reports rather than grading a region that is
    not there.
    """
    cx, cy = int(vp["win_w"] // 2), int(vp["win_h"] // 2)
    seed = None
    for radius in range(0, reach + 1, 8):
        for dy in range(-radius, radius + 1, 8):
            for dx in range(-radius, radius + 1, 8):
                if radius and max(abs(dx), abs(dy)) != radius:
                    continue
                if pick_tile(port, cx + dx, cy + dy) == tile:
                    seed = (cx + dx, cy + dy)
                    break
            if seed:
                break
        if seed:
            break
    if seed is None:
        return None

    sx, sy = seed

    def edge(dx: int, dy: int) -> int:
        far = 0
        for d in range(step, reach + 1, step):
            if pick_tile(port, sx + dx * d, sy + dy * d) != tile:
                break
            far = d
        return far

    left, right = edge(-1, 0), edge(1, 0)
    up, down = edge(0, -1), edge(0, 1)
    x0, y0 = win_to_fb(vp, sx - left - step, sy - up - step)
    x1, y1 = win_to_fb(vp, sx + right + step, sy + down + step)
    return (max(0, x0), max(0, y0), max(1, x1 - x0), max(1, y1 - y0))


def fluid_at(port: int, gx: int, gy: int) -> str:
    raw = send(port,
               f"local t = world.getFluidAt({gx}, {gy}, '{PAGE}'); "
               f"return t or 'dry'")
    return raw.strip().strip('"')


def react(chk: Checks, port: int, lava_tile, water_tile, baseline):
    """Run the real contact with the sim RUNNING.

    The engine-level pause gates the whole fluid tick
    (``Sim.Thread.simTick`` reads ``enginePausedRef``), so it has to come
    off for the reaction and is asserted rather than assumed: a paused
    run would sit here for the whole budget and report "no stone" about a
    sim that never ran.
    """
    running = send(port, "return engine.isPaused()").strip()
    chk.ok(running == "false",
           f"the simulation is running for the contact "
           f"(engine.isPaused() = {running!r})")
    send(port, f"world.setFluidTile('{PAGE}', {water_tile[0]}, "
               f"{water_tile[1]}, 'ocean'); return 'ok'")
    send(port, f"world.setFluidTile('{PAGE}', {lava_tile[0]}, "
               f"{lava_tile[1]}, 'lava'); return 'ok'")
    time.sleep(1.0)
    # The OCEAN side is the durable half of the fixture: the lava is
    # consumed by the very contact this probe is waiting for, and on a
    # fast reaction it is already gone a second later — so asserting it
    # is still there would fail exactly when the probe has succeeded.
    chk.ok(fluid_at(port, *water_tile) == "ocean",
           f"the water edit landed (got {fluid_at(port, *water_tile)!r})")
    chk.ok(fluid_at(port, *lava_tile) == "lava"
           or (terrain_at(port, *lava_tile) or 0) > baseline,
           "the lava edit landed, or its contact has already resolved")
    deadline = time.time() + REACTION_TIMEOUT
    top = None
    while time.time() < deadline:
        top = terrain_at(port, *lava_tile)
        if top is not None and top > baseline:
            break
        time.sleep(0.3)
    chk.ok(top is not None and top > baseline,
           f"the contact solidified: the lava column's terrain top rose from "
           f"{baseline} to {top} (lava cell now "
           f"{fluid_at(port, *lava_tile)!r}, water cell "
           f"{fluid_at(port, *water_tile)!r})")
    return top


def main() -> int:
    ap = argparse.ArgumentParser()
    ap.add_argument("--port", type=int, default=9283)
    ap.add_argument("--shots", default=None,
                    help="keep the captures in this directory")
    args = ap.parse_args()
    port = args.port
    chk = Checks()

    with tempfile.TemporaryDirectory(prefix="fluid_reaction_visual_") as base:
        shots = args.shots or os.path.join(base, "shots")
        os.makedirs(shots, exist_ok=True)
        log = os.path.join(base, "engine.log")
        proc = boot(port, log=log,
                    args=["--offscreen", "--size", f"{FRAME[0]}x{FRAME[1]}"],
                    ready_timeout=240)
        try:
            if not reach_world_view(chk, port):
                return 1
            freeze_frame(port)

            # The identity the "no reload" assertions are made against.
            generated_before = send(port, f"return world.getIdentity('{PAGE}')")
            load_before = send(port, "return engine.getLoadStatus()")

            # Look at the middle of the frame and react on what is
            # there: a fixed world coordinate is not necessarily anywhere
            # the camera can see.
            vp = viewport(port, fallback=FRAME)
            send(port, f"camera.setZoom({DETAIL_ZOOM}); return 'ok'")
            time.sleep(1.0)
            centre = pick_tile(port, int(vp["win_w"] // 2), int(vp["win_h"] // 2))
            chk.ok(centre[0] is not None,
                   f"the camera is looking at a real tile (got {centre})")
            if centre[0] is None:
                return 1
            lava_tile, water_tile, baseline = find_contact_pair(port, centre)
            chk.ok(lava_tile is not None,
                   f"found a flat dry land pair in frame (lava {lava_tile}, "
                   f"water {water_tile}, terrain top {baseline})")
            if lava_tile is None:
                return 1
            baseline = raise_lava_side(chk, port, lava_tile, baseline)
            before_material = material_at(port, *lava_tile)
            chk.ok(before_material not in PRODUCTS,
                   f"the target column is not already a reaction product "
                   f"(got {before_material!r})")

            # The camera is deliberately NOT re-pointed. The target was
            # chosen from what this camera already frames, and
            # `pin_camera_to_tile` pins the z-slice to the tile's own z,
            # which re-offsets the whole view and moved the target seven
            # tiles off centre when it was tried here. Leaving the camera
            # exactly where the before-capture will be taken is also what
            # makes before and after comparable at all.
            box = screen_box_for(port, lava_tile, vp)
            chk.ok(box is not None,
                   f"the target tile {lava_tile} is on screen at {box} "
                   f"(centre pixel resolves to "
                   f"{pick_tile(port, int(vp['win_w'] // 2), int(vp['win_h'] // 2))})")
            if box is None:
                return 1

            # -- before: detailed tiles, then the zoom map.
            set_paused(port, True)
            detail_before, detail_noise = capture_pair(
                port, chk, shots, "detail_before", box)
            stats = png_stats(detail_before)
            chk.ok(stats is not None and stats[2] > 16,
                   f"the detailed frame is a real rendered scene, not a blank "
                   f"or near-uniform image (got {stats})")
            send(port, f"camera.setZoom({MAP_ZOOM}); return 'ok'")
            time.sleep(1.0)
            map_before, map_noise = capture_pair(port, chk, shots, "map_before")
            chk.ok(png_differs(detail_before, map_before),
                   "the zoom-map frame is a different view from the detailed "
                   "one, so the map captures are really of the map")

            # -- the reaction itself, with the sim running.
            send(port, f"camera.setZoom({DETAIL_ZOOM}); return 'ok'")
            time.sleep(0.5)
            set_paused(port, False)
            print(f"  [note] active world before the reaction: "
                  f"{active_id(port)!r}")
            top = react(chk, port, lava_tile, water_tile, baseline)
            print(f"  [note] active world after the reaction: "
                  f"{active_id(port)!r}")
            set_paused(port, True)
            if top is None:
                return 1
            material = material_at(port, *lava_tile)
            chk.ok(material in PRODUCTS,
                   f"the new top is an authored reaction product "
                   f"(got {material!r})")
            # Give the world thread's republished atlas a few frames to
            # be uploaded and rebaked before the map capture.
            time.sleep(3.0)

            # -- after: the same two views, same camera, same process.
            detail_after, detail_noise_after = capture_pair(
                port, chk, shots, "detail_after", box)
            changed = png_region_changed_pixels(detail_before, detail_after, box)
            floor = max(detail_noise, detail_noise_after)
            chk.ok(changed is not None and changed > floor,
                   f"the detailed tile render changed INSIDE the solidified "
                   f"tile's own screen box {box}: {changed} px against a "
                   f"{floor} px noise floor")
            send(port, f"camera.setZoom({MAP_ZOOM}); return 'ok'")
            time.sleep(1.0)
            map_after, map_noise_after = capture_pair(
                port, chk, shots, "map_after")
            map_changed = whole_frame_changed(map_before, map_after)
            map_floor = max(map_noise, map_noise_after)
            chk.ok(map_changed > map_floor,
                   f"the ZOOM MAP changed: {map_changed} px against a "
                   f"{map_floor} px noise floor — which only a regenerated "
                   f"terrain pixel block and a republished atlas can do, "
                   f"since its renderer never reads the edited chunk")

            # …and the change is CONFINED to the affected chunk's own
            # quad and reads as the product the reaction actually chose.
            # A whole-frame delta alone would also pass for a change
            # anywhere else in the map, or for one the wrong colour.
            bbox = png_diff_bbox(map_before, map_after)
            chk.ok(bbox is not None, f"the zoom change has a locatable "
                                     f"region (bbox {bbox})")
            if bbox is not None:
                x0, y0, x1, y1 = bbox
                region = (x0, y0, x1 - x0, y1 - y0)
                area = (x1 - x0) * (y1 - y0)
                frame_area = FRAME[0] * FRAME[1]
                chk.ok(area <= frame_area * MAP_REGION_FRACTION,
                       f"…and it is confined to {region}, "
                       f"{100.0 * area / frame_area:.2f}% of the frame — one "
                       f"chunk's quad, not a repaint of the map")
                chose = texture_mean_colour(PRODUCT_ZOOM_TEXTURE[material])
                other = texture_mean_colour(PRODUCT_ZOOM_TEXTURE[
                    "obsidian" if material == "basalt" else "basalt"])
                was = mean_colour(map_before, region)
                now = mean_colour(map_after, region)
                if None in (chose, other, was, now):
                    chk.ok(False, "could not sample the zoom region's colour")
                else:
                    chk.ok(colour_distance(now, chose)
                           < colour_distance(was, chose),
                           f"that region moved TOWARD the chosen product's "
                           f"own zoom colour ({material}): "
                           f"{colour_distance(was, chose):.1f} -> "
                           f"{colour_distance(now, chose):.1f}")
                    chk.ok(colour_distance(now, chose)
                           < colour_distance(now, other),
                           f"…and now reads closer to {material} than to "
                           f"the other product "
                           f"({colour_distance(now, chose):.1f} vs "
                           f"{colour_distance(now, other):.1f})")

            # -- and none of it was a reload.
            generated_after = send(port, f"return world.getIdentity('{PAGE}')")
            chk.ok(generated_after == generated_before,
                   "the page's identity is unchanged, so the session was "
                   "never replaced")
            chk.ok(send(port, "return engine.getLoadStatus()") == load_before,
                   "no load transaction ran at any point")
            still = active_id(port)
            chk.ok(still == PAGE,
                   f"the same page is still active (got {still!r})")
            print(f"\nEvidence written to {shots}:")
            for name in sorted(os.listdir(shots)):
                print(f"  {name}")
        finally:
            quit_engine(port, proc)

    print()
    if chk.failed:
        print(f"FAILED: {chk.failed} check(s)")
        return 1
    print("PASSED: the stone appears in both live presentations, no reload")
    return 0


if __name__ == "__main__":
    sys.exit(main())
