#!/usr/bin/env python3
"""Committed building blueprint footprint — visual gate (#807).

The automated CI gate for #807 is the pure headless spec
(``Test.Headless.Construct.Footprint``, run via ``cabal test
synarchy-test-headless --test-options='--match "Construction blueprint
footprint"'``): it proves ``World.Construct.Types.
constructDesignationFootprint`` — the exact pure function the render
pass calls — expands one committed ``CtBuilding`` designation into its
def's full footprint while the designation map still holds exactly one
entry. That's a data-flow proof, not a pixel one.

This script is the manual, GPU-bound visual half: it boots the engine
with ``--offscreen`` (real Vulkan render, real UI flow, no window) so
the actual committed-blueprint quads described in the issue
(``World.Render.CursorQuads``) draw to a real framebuffer, then proves
via screenshot diffing that a multi-tile building's ghost visibly
covers MORE than one tile, while ``construction.getDesignationCount``
proves the underlying job stayed a single entry throughout — i.e. the
render expanded, the data model didn't (#807 requirement 2).

Two gotchas this script works around, discovered by hand against a live
offscreen engine before this was scripted (neither is specific to #807 —
any offscreen visual probe of world content hits them):
  1. The camera lands zoomed OUT past World.Grid's zoomFadeEnd (1.6)
     after "Continue" — the chunk-mosaic zoom-map overview
     (World.ZoomMap), which does not draw per-tile cursor/designation
     quads at all (World.Render's tileAlpha <= 0.001 skip). Fixed by
     forcing camera.setZoom(0.5) (same value camera.goToTile's
     "zoomSafe" branch already uses for ordinary gameplay).
  2. camera.goToTile sets the z-slice to the target tile's elevation +
     World.Render.surfaceHeadroom (25) — appropriate for normal play
     (headroom above terrain), but a flat designation site 25 z-levels
     below the slice can render far enough off-plane to be
     effectively invisible. Fixed by re-pinning camera.setZSlice to the
     anchor's OWN surface elevation right after goToTile.

Registers its own throwaway 2x3 (+ 1x1 size-control) building YAML
fixture — mirroring how tools/power_workshop_probe.py and
tools/craft_bill_probe.py inject temp fixtures — rather than adding a
multi-tile building to shipped gameplay content (out of scope per the
issue). Needs a real GPU (Vulkan device) — manual-only, never CI-gated
(see tools/ci_probes.py --status; #807's CI-blocking gate is the pure
spec above).

Usage: python3 tools/construction_blueprint_footprint_probe.py
       [--port 9420] [--size 1024x768]
"""
from __future__ import annotations

import argparse
import os
import sys
import tempfile
import time

sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
from probelib import boot, poll_until, quit_engine, send, send_json

SPROOT = "/tmp"
TEST_BUILDING_YAML = f"{SPROOT}/construction_blueprint_footprint_probe_buildings.yaml"
DEF_2X3 = "probe_footprint_building_2x3"
DEF_1X1 = "probe_footprint_building_1x1"
SPRITE = "assets/textures/buildings/workbench/default.png"

TEST_BUILDINGS = f"""\
buildings:
  - name: "{DEF_2X3}"
    display_name: "Probe Footprint 2x3"
    category: "Test"
    description: "Throwaway #807 test fixture — not shipped content."
    sprite: "{SPRITE}"
    tile_size: {{ x: 2, y: 3 }}
    placement: "flat_ground"
    race: "acolyte_cult"
    build_work: 0.0
  - name: "{DEF_1X1}"
    display_name: "Probe Footprint 1x1 control"
    category: "Test"
    description: "Throwaway #807 test fixture — not shipped content."
    sprite: "{SPRITE}"
    tile_size: {{ x: 1, y: 1 }}
    placement: "flat_ground"
    race: "acolyte_cult"
    build_work: 0.0
"""

# Candidate footprint-anchor offsets from world origin, tried nearest-
# first until one is fully flat + dry (real worldgen has no guaranteed-
# clear spawn like the arena courses do, so a handful of fixed spots
# isn't reliable across different seeds). Kept within a modest radius
# of origin: camera.goToTile clamps its target near the world edge (the
# "glacier rim" fence, #297/#298) on anything but a huge world, and the
# default Create World size (128 tiles) clamps a ~60-tile-out target
# back to nearly the origin — silently pointing the camera at the WRONG
# spot instead of the anchor just designated. The grid is split into two
# DISJOINT interleaved halves so the 1x1 control and the 2x3 target can
# never land on the same tile — construction.designate keys a
# CtBuilding purely by its anchor, so two fixtures on one tile would
# silently overwrite the first instead of adding a second job.
def _candidate_grid(step: int, extent: int):
    pts = [(dx, dy) for dx in range(-extent, extent + 1, step)
                     for dy in range(-extent, extent + 1, step)]
    pts.sort(key=lambda p: p[0] * p[0] + p[1] * p[1])
    return pts


_ALL_CANDIDATES = _candidate_grid(5, 25)
CANDIDATES_2X3 = _ALL_CANDIDATES[0::2]
CANDIDATES_1X1 = _ALL_CANDIDATES[1::2]

failures = 0


def check(name: str, ok: bool, detail: str = "") -> bool:
    global failures
    print(f"  [{'PASS' if ok else 'FAIL'}] {name}"
          + (f" — {detail}" if detail and not ok else ""))
    failures += not ok
    return ok


# --------------------------------------------------------------------------
# PNG helpers (PIL) — diff PIXEL COUNT, not just a boolean (offscreen_probe's
# png_differs collapses to bool; the footprint-size claim needs the
# magnitude so a 6-tile footprint can be told apart from a 1-tile one).
# --------------------------------------------------------------------------
def png_stats(path: str):
    try:
        from PIL import Image
        with Image.open(path) as im:
            im = im.convert("RGBA")
            colors = im.getcolors(maxcolors=1 << 20)
            return im.width, im.height, (len(colors) if colors else (1 << 20))
    except Exception:
        return None


def png_diff_count(path_a: str, path_b: str) -> int:
    """# of pixels that differ between two same-size screenshots.

    Deliberately no getbbox() short-circuit: on RGBA, Pillow 10+ defaults
    it to alpha_only=True, and two fully-opaque frames always bbox to
    None (the same gotcha offscreen_probe.py's png_differs works around).
    """
    from PIL import Image, ImageChops
    with Image.open(path_a) as a, Image.open(path_b) as b:
        if a.size != b.size:
            return max(a.size[0] * a.size[1], b.size[0] * b.size[1])
        diff = ImageChops.difference(a.convert("RGBA"), b.convert("RGBA"))
        return sum(diff.convert("L").histogram()[1:])


# --------------------------------------------------------------------------
# UI helpers (F3 widget oracle) — same pattern as offscreen_probe.py.
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


def wid(port: int):
    raw = send(port, "return world.getActiveWorldId()")
    raw = raw.strip().strip('"')
    return raw if raw and raw not in ("null", "nil") else None


def designation_count(port: int, pageid: str) -> int | None:
    raw = send(port, f"return construction.getDesignationCount('{pageid}')")
    try:
        return int(float(raw))
    except (TypeError, ValueError):
        return None


def designation_at(port: int, pageid: str, x: int, y: int):
    return send_json(
        port, f"return construction.getDesignationAt('{pageid}', {x}, {y})")


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


def find_dry_anchor(port: int, w: int, h: int, candidates):
    """First candidate whose full w x h footprint is entirely dry
    (real worldgen has no guaranteed-clear spawn like the arena courses
    do, and a random seed can put the origin in open ocean). Doesn't
    require FLAT terrain: the fix under test stores the designation's
    z once at the anchor and renders the whole footprint on that single
    plane regardless of the real terrain underneath (the issue's
    "single Z-plane, no per-tile column lookups" requirement) — uneven
    ground is a fine visual candidate for it, unlike open water, where
    the ghost would render half-submerged and be hard to see reliably
    in a screenshot diff. Caller is expected to have already loaded a
    region wide enough to cover every candidate (getSurfaceAt reports
    None on an unloaded chunk, which this treats as a non-match rather
    than a reason to load on demand — candidate lists span dozens of
    tiles, so a per-candidate load/wait round trip would make the
    search slow)."""
    for cx, cy in candidates:
        tiles = [(cx + i, cy + j) for i in range(w) for j in range(h)]
        infos = [tile_surface(port, x, y) for x, y in tiles]
        if any(info is None for info in infos):
            continue
        if all(dry for _, dry in infos):
            anchor_z = infos[0][0]
            return cx, cy, anchor_z
    return None


def frame_on(port: int, ax: int, ay: int, z: int) -> None:
    """Center + zoom + z-slice the camera on one designation site, and
    make sure its chunks are loaded — see the gotchas in the module
    docstring (zoom past zoomFadeEnd; z-slice offset by surfaceHeadroom
    AND continuously re-derived by camZTracking, which must be turned
    off first or it re-applies that offset on the very next frame)."""
    send(port, f"camera.goToTile({ax}, {ay}); return 'ok'")
    send(port, "camera.setZoom(0.5); return 'ok'")
    send(port, "camera.setZTracking(false); return 'ok'")
    send(port, f"camera.setZSlice({z}); return 'ok'")
    send(port, f"return world.loadChunksInRegion({(ax - 3) // 16}, {(ay - 3) // 16}, "
               f"{(ax + 4) // 16}, {(ay + 4) // 16})")
    send(port, "return world.waitForChunks(30)", timeout=35.0)
    time.sleep(1.0)


def main() -> int:
    ap = argparse.ArgumentParser(
        description=__doc__, formatter_class=argparse.RawDescriptionHelpFormatter)
    ap.add_argument("--port", type=int, default=9420)
    ap.add_argument("--size", default="1024x768")
    args = ap.parse_args()
    port = args.port
    shots = tempfile.mkdtemp(prefix="construction_blueprint_footprint_probe_")

    print(f"== offscreen boot (port {port}, {args.size}) ==")
    proc = boot(port, mode=("--offscreen",), args=["--size", args.size],
                label="offscreen engine")

    # -- Real UI flow to the in-game HUD (same path as tools/offscreen_probe.py):
    # this is what actually wires up construction.setDesignateTexture (hud.lua),
    # which a raw debug-console world.initArena bypass would skip.
    menu_up = poll_until(60.0, lambda: find_widget(port, "Create World"))
    check("loading screen -> main menu", bool(menu_up))
    check("click 'Create World'", click_widget(port, "Create World"))
    create_up = poll_until(20.0, lambda: find_widget(port, "Generate World"))
    check("create-world screen reached", bool(create_up))
    check("click 'Generate World'", click_widget(port, "Generate World"))

    def world_done():
        got = send(port, "local p = world.getInitProgress(); return p", timeout=5.0)
        return got.strip() == "3"

    print("  (generating world, ~1 min)")
    check("worldgen completes (phase 3)", bool(poll_until(300.0, world_done, interval=2.0)))
    cont = poll_until(60.0, lambda: find_widget(port, "Continue"))
    check("post-generation Continue button appears", bool(cont))
    check("click 'Continue'", click_widget(port, "Continue"))
    hud_up = poll_until(60.0, lambda: not find_widget(port, "Continue"))
    check("in-game HUD reached", bool(hud_up))
    time.sleep(2.0)  # let the first in-game frames render

    pageid = wid(port)
    if not check("active world id resolves", bool(pageid)):
        quit_engine(port, proc)
        return 1

    # -- Register the throwaway 2x3 + 1x1-control building defs.
    with open(TEST_BUILDING_YAML, "w") as f:
        f.write(TEST_BUILDINGS)
    n = send(port, f"return engine.loadBuildingYaml('{TEST_BUILDING_YAML}')")
    check("probe building defs loaded", float(n) == 2.0, f"got {n!r}")

    # Load wide enough to cover every candidate in one shot (both lists
    # span roughly -25..25 tiles -> chunks -2..2) rather than paying a
    # load/wait round trip per candidate tried below.
    print("  (scanning nearby terrain for a dry anchor site)")
    n_queued = send(port, "return world.loadChunksInRegion(-2, -2, 2, 2)")
    n_remaining = send(port, "return world.waitForChunks(60)", timeout=65.0)
    print(f"  (chunks queued={n_queued!r}, remaining after wait={n_remaining!r})")

    anchor_1x1 = find_dry_anchor(port, 1, 1, CANDIDATES_1X1)
    anchor_2x3 = find_dry_anchor(port, 2, 3, CANDIDATES_2X3)
    if anchor_1x1 is None or anchor_2x3 is None:
        sample = CANDIDATES_1X1[0]
        print(f"  (debug) sample tile_surface{sample}: "
              f"{tile_surface(port, *sample)!r}")
        print(f"  (debug) got.getSurfaceAt raw: "
              f"{send(port, f'return world.getSurfaceAt({sample[0]}, {sample[1]})')!r}")
    if not check("found a dry site for the 1x1 control", anchor_1x1 is not None):
        quit_engine(port, proc)
        return 1
    if not check("found a dry site for the 2x3 blueprint", anchor_2x3 is not None):
        quit_engine(port, proc)
        return 1
    ax1, ay1, z1 = anchor_1x1
    ax2, ay2, z2 = anchor_2x3

    count0 = designation_count(port, pageid)
    check("no designations before the probe starts", count0 == 0, f"got {count0}")

    # -- 1x1 size control, framed on its own camera position: commits
    # exactly like build_tool.lua's real commit call
    # (construction.designate(pageId, gx, gy, gx, gy, 'building', defName)
    # — Construct.hs only ever reads the FIRST coordinate pair for a
    # CtBuilding target).
    frame_on(port, ax1, ay1, z1)
    shot_before_1x1 = os.path.join(shots, "before_1x1.png")
    check("1x1 baseline screenshot answers", screenshot(port, shot_before_1x1))
    send(port, f"construction.designate('{pageid}', {ax1}, {ay1}, {ax1}, {ay1}, "
               f"'building', '{DEF_1X1}'); return 'ok'")
    time.sleep(0.5)
    count1 = designation_count(port, pageid)
    check("1x1 control designated as one job", count1 == 1, f"got {count1}")
    shot_after_1x1 = os.path.join(shots, "after_1x1.png")
    check("post-1x1 screenshot answers", screenshot(port, shot_after_1x1))

    # -- 2x3 target, framed on ITS OWN camera position (never overlapping
    # the 1x1 control's tiles — see CANDIDATES_1X1's offset).
    frame_on(port, ax2, ay2, z2)
    shot_before_2x3 = os.path.join(shots, "before_2x3.png")
    check("2x3 baseline screenshot answers", screenshot(port, shot_before_2x3))
    send(port, f"construction.designate('{pageid}', {ax2}, {ay2}, {ax2}, {ay2}, "
               f"'building', '{DEF_2X3}'); return 'ok'")
    time.sleep(0.5)
    count2 = designation_count(port, pageid)
    check("2x3 blueprint STILL only adds one job (#807 req 2)", count2 == 2,
          f"got {count2} (expected 2 total: the 1x1 control + this one job, "
          f"never 7)")
    shot_after_2x3 = os.path.join(shots, "after_2x3.png")
    check("post-2x3 screenshot answers", screenshot(port, shot_after_2x3))

    # -- The designation MAP holds only the anchor (#807 req 2): an
    # off-anchor footprint tile (well inside the 2x3 rectangle) reports
    # no designation of its own, even though the render pass (checked
    # below via the screenshot diff) draws a ghost over it too.
    interior = designation_at(port, pageid, ax2 + 1, ay2 + 1)
    check("an off-anchor footprint tile has no designation entry of its own",
          interior is None, f"got {interior!r}")
    anchor_entry = designation_at(port, pageid, ax2, ay2)
    check("the anchor tile itself IS the (only) designation entry",
          isinstance(anchor_entry, dict), f"got {anchor_entry!r}")

    # -- The visual claim: the 2x3 blueprint must change visibly MORE
    # screen area than the 1x1 control did — proof the render pass
    # actually draws all 6 footprint tiles, not just the anchor (which
    # would make the two diffs the same size, footprint size be damned).
    st_before = png_stats(shot_before_1x1)
    check("screenshots are valid non-trivial PNGs",
          bool(st_before) and st_before[2] >= 3, f"got {st_before}")
    diff_1x1 = png_diff_count(shot_before_1x1, shot_after_1x1)
    diff_2x3 = png_diff_count(shot_before_2x3, shot_after_2x3)
    check("1x1 control ghost is visible at all", diff_1x1 > 0, f"diff={diff_1x1}")
    check("2x3 blueprint ghost is visible at all", diff_2x3 > 0, f"diff={diff_2x3}")
    check("2x3 blueprint visibly covers MORE area than the 1x1 control "
          "(>= 3x its pixel diff -- the full footprint, not just the anchor)",
          diff_2x3 >= diff_1x1 * 3,
          f"1x1 diff={diff_1x1}, 2x3 diff={diff_2x3}")

    quit_engine(port, proc)
    print(f"\nscreenshots kept in {shots}")
    if failures:
        print(f"construction_blueprint_footprint_probe: {failures} check(s) FAILED")
        return 1
    print("construction_blueprint_footprint_probe: all checks passed")
    return 0


if __name__ == "__main__":
    sys.exit(main())
