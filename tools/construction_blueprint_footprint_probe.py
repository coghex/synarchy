#!/usr/bin/env python3
"""Committed building blueprint ghost — visual gate (#807, re-pointed by #1845).

#807 made a committed ``CtBuilding`` designation draw one generic
category marker on every tile of its def's footprint, and this probe
graded that by proving a 2x3 blueprint changed at least three times the
screen area a 1x1 one did. DTV-10 reverses that decision on purpose: a
planned building is now ONE sprite of the building's OWN art, sized from
its own texture and dropped by its own ``sprite_anchor``, at the same
60 % the staked pre-delivery instance draws with.

So the graded claim moved with it. The area RATIO is gone — it was
satisfied by six copies of one marker and would be satisfied again by
any six-tile smear — and what replaces it is a direct comparison against
the thing the ghost is a ghost OF:

  * the designated ghost's changed-pixel BOUNDING BOX equals the box the
    same building occupies once it is really staked there, which is the
    "same dimensions and anchor offset as that building's placed sprite"
    acceptance criterion;
  * staking the building over its own live designation changes almost
    nothing on screen, which is the "no jump at staking" one; and
  * a control def whose sprite is a different SIZE draws a
    correspondingly different box, so the geometry demonstrably follows
    the definition rather than a fixed 96x64 marker.

The fixtures had to move too. Both of #807's used the same 96x96
``workbench/default.png``, so an implementation that still drew one
fixed-size quad would pass every equality above; the 2x3 fixture now
carries a ``tile_bottom`` sprite anchor (a real, observable vertical
drop) and the 1x1 control a 32x32 sprite, and the 2x3 declares
``build_work`` and a material bill so that a staked instance of it is
the PRE-DELIVERY ghost the designation must be indistinguishable from.

The automated CI gate remains the pure headless spec
(``Test.Headless.Construct.Footprint`` and ``Test.Headless.Building.Ghost``,
via ``cabal test synarchy-test-headless --test-options='--match
"building ghost"'``). This script is the manual, GPU-bound visual half:
it boots with ``--offscreen`` (real Vulkan render, real UI flow, no
window) so the actual quads draw to a real framebuffer, while
``construction.getDesignationCount`` proves the underlying job stayed a
single anchor-only entry throughout — the one half of #807 that #1845
deliberately keeps.

The fixture world (#1587)
-------------------------
This probe reaches its world the way a player does — "Create World",
then "Generate World" — because that flow is what brings up the real
in-game HUD, camera and world the ghosts are graded against, which a raw
debug-console ``world.initArena`` bypass would skip. Left alone, that
screen rolls a FRESH random seed every run
(``scripts/create_world/settings_tab.lua`` replaces an empty pending
seed with ``randbox.newHexSeed()``), so the terrain this probe had to
find a building site on was a different world each time — and a world
whose origin sits in open ocean has no site at all, which used to abort
the run before a single designation existed.

So the seed is PINNED here, through the create-world screen's own seed
control (``randbox.setValue``) rather than by changing any default:
``DEFAULT_SEED_HEX`` names a world verified to carry dry ground for both
anchors inside the candidate grid below, and ``--seed`` overrides it.
``world.getSeed`` then confirms the generated world really is that one,
and every run prints a one-line ``FIXTURE`` identity (seed, world size,
plate count, both chosen anchors) so two runs can be compared for more
than "both exited 0".

Failure protocol (#1587 requirement 4)
--------------------------------------
Exit **2** means the fixture never came up — no site, no world, no HUD,
no registered defs — so NOTHING was graded; the cause is named on
stderr. Exit **1** means the footprint WAS graded and came up short.
Exit **0** is a pass. ``--force-no-site`` reproduces the exit-2 path end
to end without editing this file, and ``--self-test`` asserts the whole
mapping with no engine at all.

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
       [--port 9420] [--size 1024x768] [--seed 0000002A]
       python3 tools/construction_blueprint_footprint_probe.py --force-no-site
       python3 tools/construction_blueprint_footprint_probe.py --self-test
"""
from __future__ import annotations

import argparse
import contextlib
import io
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
# Two DIFFERENT canvases on purpose (#1845): a ghost that still drew one
# fixed-size marker would pass every geometry comparison below if both
# fixtures shared a sprite, as #807's two did.
SPRITE_BIG = "assets/textures/buildings/workbench/default.png"      # 96x96
SPRITE_SMALL = "assets/textures/buildings/dungeon_1/post.png"       # 32x32

# Exit codes. Keeping the two failure modes DISTINCT is #1587
# requirement 4 (same convention as tools/combat_anim_probe.py): 1 means
# the footprint was graded and came up short, 2 means it was never
# graded at all because the fixture never came up.
FAIL_EXIT = 1
SETUP_EXIT = 2

# The pinned fixture world (#1587). Written the way the create-world
# seed field spells one — 8 uppercase hex digits, which
# scripts/create_world/generation.lua reads with tonumber(seed, 16).
# Verified to carry dry ground for BOTH anchors inside the candidate
# grid below at the create-world screen's own default size (128 chunks)
# and plate count (10); see the FIXTURE line every run prints.
DEFAULT_SEED_HEX = "0000002A"

# Tolerances for the camera-landing proof below. LANDING_EPS is far
# above float32 noise at these magnitudes (~1e-4 around 800 world units)
# and far below one tile, so it can only be tripped by a real clamp.
BASIS_EPS = 1e-3
LANDING_EPS = 0.05

# How far the designated ghost's diff box may sit from the staked
# building's (#1845). They are the SAME rectangle by construction, so the
# only slack this needs to absorb is the outermost row or two of nearly
# transparent sprite edge, which can blend to the background under one
# opacity and not the other. Two pixels is far below the drop
# `sprite_anchor: tile_bottom` applies (16 px on the standard tile) and
# far below the difference between the two fixture canvases, so a wrong
# anchor or a wrong size cannot hide inside it.
GHOST_BOX_TOL = 2

# The 1x1 control's sprite is a third of the 2x3's on each axis, so its
# box must come back visibly smaller. Stated as a loose bound (half),
# not the exact 3x, because the two boxes are of DIFFERENT art and this
# is a "the geometry follows the definition" claim, not a ratio claim of
# the kind #807's area check turned out to be.
CONTROL_BOX_MAX_FRACTION = 0.5

# The 2x3 target declares `sprite_anchor: tile_bottom` and a material
# bill it will never be given, so a staked instance of it renders as the
# PRE-DELIVERY ghost — the exact state the committed designation must be
# indistinguishable from. The 1x1 control keeps the default anchor and a
# third of the canvas, so its box is a genuinely different shape.
TEST_BUILDINGS = f"""\
buildings:
  - name: "{DEF_2X3}"
    display_name: "Probe Footprint 2x3"
    category: "Test"
    description: "Throwaway #1845 test fixture — not shipped content."
    sprite: "{SPRITE_BIG}"
    visual_class: "freestanding_installation"
    tile_size: {{ x: 2, y: 3 }}
    placement: "flat_ground"
    race: "acolyte_cult"
    sprite_anchor: "tile_bottom"
    build_work: 240.0
    materials:
      steel_plate: 10
  - name: "{DEF_1X1}"
    display_name: "Probe Footprint 1x1 control"
    category: "Test"
    description: "Throwaway #1845 test fixture — not shipped content."
    sprite: "{SPRITE_SMALL}"
    visual_class: "freestanding_installation"
    tile_size: {{ x: 1, y: 1 }}
    placement: "flat_ground"
    race: "acolyte_cult"
    build_work: 0.0
"""

# Candidate footprint-anchor offsets from world origin, tried nearest-
# first until one is fully dry. The world is pinned (DEFAULT_SEED_HEX),
# so this search is deterministic — it resolves to the same two anchors
# on every run, which is what the FIXTURE line publishes. The grid stays
# modest because the whole candidate span has to be chunk-loaded up
# front, and it is split into two DISJOINT interleaved halves so the 1x1
# control and the 2x3 target can never land on the same tile —
# construction.designate keys a CtBuilding purely by its anchor, so a
# second fixture on one tile would be REFUSED outright (#1595) and this
# probe would silently be testing one blueprint instead of two.
#
# Staying inside camera.goToTile's glacier fence (#297/#298) is NOT left
# to the radius: worldSize is counted in CHUNKS, so the fence on a
# 128-chunk world sits hundreds of tiles out and this grid is nowhere
# near it — but a smaller world, or a different grid, could reach it.
# unclamped_landing_cause() below proves per anchor, against the live
# engine, that goToTile lands exactly where it was asked to.
CANDIDATE_STEP = 5
CANDIDATE_EXTENT = 25


def _candidate_grid(step: int, extent: int):
    pts = [(dx, dy) for dx in range(-extent, extent + 1, step)
                     for dy in range(-extent, extent + 1, step)]
    pts.sort(key=lambda p: p[0] * p[0] + p[1] * p[1])
    return pts


_ALL_CANDIDATES = _candidate_grid(CANDIDATE_STEP, CANDIDATE_EXTENT)
CANDIDATES_2X3 = _ALL_CANDIDATES[0::2]
CANDIDATES_1X1 = _ALL_CANDIDATES[1::2]

failures = 0


def check(name: str, ok: bool, detail: str = "") -> bool:
    """One GRADED assertion — a failure here means exit FAIL_EXIT."""
    global failures
    print(f"  [{'PASS' if ok else 'FAIL'}] {name}"
          + (f" — {detail}" if detail and not ok else ""))
    failures += not ok
    return ok


# --------------------------------------------------------------------------
# Setup failures (#1587 requirement 4) — never a footprint verdict.
# --------------------------------------------------------------------------
class SetupFailure(RuntimeError):
    """The fixture could not be established — nothing was ever graded."""


def require_setup(name: str, ok: bool, cause: str) -> None:
    """One SETUP precondition. Prints like a check, raises on failure."""
    print(f"  [{'OK  ' if ok else 'SETUP'}] {name}")
    if not ok:
        raise SetupFailure(cause)


def report_setup_failure(cause: str) -> int:
    """Print a named fixture cause and yield the SETUP exit code.

    `main` routes every `SetupFailure` through here, so the mapping this
    returns is the one a live run uses and `--self-test` can assert.
    """
    print(f"FAIL (setup): {cause}", file=sys.stderr)
    print(f"\nconstruction_blueprint_footprint_probe: SETUP FAILURE — {cause}"
          f"\n  (the blueprint footprint was never graded; exit {SETUP_EXIT})")
    return SETUP_EXIT


def verdict_exit(graded_failures: int) -> int:
    """The graded verdict, reached only once the fixture stood up."""
    return FAIL_EXIT if graded_failures else 0


def site_cause(anchor_1x1, anchor_2x3, seed_hex: str,
               step: int = CANDIDATE_STEP,
               extent: int = CANDIDATE_EXTENT) -> str | None:
    """Why the render site could not be obtained, or None if it was."""
    missing = []
    if anchor_1x1 is None:
        missing.append("the 1x1 control")
    if anchor_2x3 is None:
        missing.append("the 2x3 blueprint")
    if not missing:
        return None
    return (f"no dry building site for {' and '.join(missing)} on the fixture "
            f"world (seed 0x{seed_hex}): every candidate anchor within "
            f"+/-{extent} tiles of the origin (step {step}) is wet or "
            f"unloaded. No designation was ever committed and no screenshot "
            f"was ever compared, so this run graded no footprint at all — "
            f"re-pin DEFAULT_SEED_HEX to a world with dry ground near the "
            f"origin rather than reading this as a rendering regression.")


def basis_cause(origin, unit_x, unit_y) -> str | None:
    """Whether goToTile still spans two independent tile axes.

    The glacier fence (Engine.Loop.Camera.applyLimits) clamps ONE screen
    axis, so a fixture world whose interior is too small for the two
    candidate offsets would collapse this basis and make the landing
    check below pass vacuously.
    """
    cross = ((unit_x[0] - origin[0]) * (unit_y[1] - origin[1])
             - (unit_x[1] - origin[1]) * (unit_y[0] - origin[0]))
    if abs(cross) > BASIS_EPS:
        return None
    return (f"camera.goToTile does not span two independent tile axes on this "
            f"world (origin={origin}, +1 gx={unit_x}, +1 gy={unit_y}): the "
            f"glacier fence is clamping every teleport, so no anchor can be "
            f"framed at the tile it was designated on.")


def unclamped_landing_cause(label: str, ax: int, ay: int,
                            origin, unit_x, unit_y, landed) -> str | None:
    """Whether goToTile(ax, ay) landed on the tile it was asked for.

    gridToWorld is affine in (gx, gy), so three measured teleports —
    (0,0), (1,0), (0,1) — reconstruct where an UNCLAMPED teleport to any
    tile would land, without this probe restating the engine's own tile
    metrics. A landing that differs is the glacier clamp pulling the
    camera somewhere else, which would frame the wrong spot (#1587
    requirement 3).
    """
    if landed is None:
        return (f"camera.getPosition() did not report where the {label} anchor "
                f"({ax},{ay}) teleport landed, so the framing could not be "
                f"verified.")
    want_x = origin[0] + ax * (unit_x[0] - origin[0]) + ay * (unit_y[0] - origin[0])
    want_y = origin[1] + ax * (unit_x[1] - origin[1]) + ay * (unit_y[1] - origin[1])
    if (abs(landed[0] - want_x) <= LANDING_EPS
            and abs(landed[1] - want_y) <= LANDING_EPS):
        return None
    return (f"camera.goToTile clamped the {label} anchor ({ax},{ay}): the "
            f"camera landed at ({landed[0]:.3f}, {landed[1]:.3f}) instead of "
            f"({want_x:.3f}, {want_y:.3f}), so the screenshots would frame a "
            f"different place than the tile just designated (the #297/#298 "
            f"glacier fence). Move the candidate grid closer to the origin, "
            f"or use a larger world.")


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


def png_diff_bbox(path_a: str, path_b: str):
    """Bounding box (x0, y0, x1, y1) of the pixels that differ, or None.

    #1845 grades SHAPE, not just magnitude: the box a designated ghost
    changes has to be the box the staked building occupies. A pixel
    COUNT cannot tell a correctly sized sprite from a same-area smear of
    tiles, which is exactly the difference this slice is about.

    Deliberately not getbbox() on the RGBA difference: on Pillow 10+
    that defaults to alpha_only=True, and two fully-opaque frames always
    bbox to None (the same gotcha png_diff_count works around).
    """
    from PIL import Image, ImageChops
    with Image.open(path_a) as a, Image.open(path_b) as b:
        if a.size != b.size:
            return None
        diff = ImageChops.difference(a.convert("RGBA"), b.convert("RGBA"))
        return diff.convert("L").getbbox()


def bbox_size(box):
    """(w, h) of a diff bounding box, or (0, 0) when nothing changed."""
    if not box:
        return (0, 0)
    return (box[2] - box[0], box[3] - box[1])


def boxes_agree(box_a, box_b, tol: int) -> bool:
    """Same rectangle to within `tol` pixels on every edge."""
    if not box_a or not box_b:
        return False
    return all(abs(u - v) <= tol for u, v in zip(box_a, box_b))


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


def placeable_at(port: int, def_name: str, x: int, y: int) -> bool:
    """Would `building.spawn(def_name, x, y)` be accepted right now?

    The engine's own answer, not a restatement of its rules — the same
    call the build tool makes every ghost frame.
    """
    raw = send(port, f"return building.canPlaceAt('{def_name}', {x}, {y})")
    return raw.split()[0].strip().lower() == "true" if raw.split() else False


def find_dry_anchor(port: int, w: int, h: int, candidates,
                    def_name: str | None = None):
    """First candidate whose full w x h footprint is entirely dry.

    Open water is excluded because a ghost there renders half-submerged
    and is hard to see reliably in a screenshot diff. Caller is expected
    to have already loaded a region wide enough to cover every candidate
    (getSurfaceAt reports None on an unloaded chunk, which this treats as
    a non-match rather than a reason to load on demand — candidate lists
    span dozens of tiles, so a per-candidate load/wait round trip would
    make the search slow).

    With `def_name`, the site must ALSO be one the engine would really
    accept a spawn of that definition on (#1845). A designation is
    admitted on ground `Building.Placement.canPlaceAt` would refuse — it
    marks its anchor and nothing else — so without this the 2x3 target
    could be planned on a slope, the stake would come back "ground is
    uneven", and the two comparisons that stake exists to support would
    both pass by comparing a screenshot to itself.

    An empty candidate list therefore yields None, which is what
    ``--force-no-site`` uses to drive the real setup-failure path
    without editing this file.
    """
    for cx, cy in candidates:
        tiles = [(cx + i, cy + j) for i in range(w) for j in range(h)]
        infos = [tile_surface(port, x, y) for x, y in tiles]
        if any(info is None for info in infos):
            continue
        if not all(dry for _, dry in infos):
            continue
        if def_name is not None and not placeable_at(port, def_name, cx, cy):
            continue
        anchor_z = infos[0][0]
        return cx, cy, anchor_z
    return None


def cam_position(port: int):
    """The camera's world-space position, or None if it didn't answer."""
    raw = send(port, "return camera.getPosition()")
    parts = raw.split()
    if len(parts) < 2:
        return None
    try:
        return float(parts[0]), float(parts[1])
    except ValueError:
        return None


def goto_and_read(port: int, gx: int, gy: int):
    """camera.goToTile(gx, gy), then where the camera actually landed."""
    send(port, f"camera.goToTile({gx}, {gy}); return 'ok'")
    return cam_position(port)


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


def pin_seed(port: int, seed_hex: str) -> tuple[str, str]:
    """Type ``seed_hex`` into the create-world screen's own seed control.

    Goes through ``randbox.setValue``, which is the same mutation the
    dice button and a player's keystrokes end at — so the create-world
    screen's defaults, and how generation.lua resolves them, are left
    exactly as shipped (#1587 out-of-scope). Returns what the control
    reads back and what the menu's pending params now carry.
    """
    shown = send(
        port,
        "local st=require('scripts.create_world.settings_tab'); "
        "local rb=require('scripts.ui.randbox'); "
        "if not st.seedRandBoxId then return 'NO-SEED-CONTROL' end; "
        f"rb.setValue(st.seedRandBoxId, '{seed_hex}'); "
        "return tostring(rb.getValue(st.seedRandBoxId))",
        timeout=10.0).strip()
    pending = send(
        port,
        "local m=require('scripts.create_world_menu'); "
        "return tostring(m.pending.seed)", timeout=10.0).strip()
    return shown, pending


def pending_gen_params(port: int) -> tuple[str, str]:
    """(worldSize, plateCount) as the create-world menu finally resolved
    them — read AFTER generation.start, which writes every tab's widget
    values back into `pending`."""
    raw = send(
        port,
        "local m=require('scripts.create_world_menu'); "
        "return tostring(m.pending.worldSize)..' '..tostring(m.pending.plateCount)",
        timeout=10.0).strip()
    parts = raw.split()
    if len(parts) < 2:
        return "?", "?"
    return parts[0], parts[1]


def main() -> int:
    ap = argparse.ArgumentParser(
        description=__doc__, formatter_class=argparse.RawDescriptionHelpFormatter)
    ap.add_argument("--port", type=int, default=9420)
    ap.add_argument("--size", default="1024x768")
    ap.add_argument("--seed", default=DEFAULT_SEED_HEX,
                    help="hex world seed to pin the fixture world to "
                         f"(default {DEFAULT_SEED_HEX})")
    ap.add_argument("--force-no-site", action="store_true",
                    help="search NO candidate anchors, to exercise the "
                         "setup-failure path end to end without editing "
                         "this file")
    ap.add_argument("--self-test", action="store_true",
                    help="assert the failure-classification and exit-code "
                         "mapping with no engine at all")
    args = ap.parse_args()
    if args.self_test:
        return self_test()

    seed_hex = args.seed.strip().upper()
    try:
        int(seed_hex, 16)
    except ValueError:
        ap.error(f"--seed must be hexadecimal (got {args.seed!r})")

    port = args.port
    shots = tempfile.mkdtemp(prefix="construction_blueprint_footprint_probe_")

    print(f"== offscreen boot (port {port}, {args.size}) ==")
    proc = boot(port, mode=("--offscreen",), args=["--size", args.size],
                label="offscreen engine")
    # Registered for teardown before ANY fallible work below (#1323): an
    # unexpected socket/parsing/image exception used to skip every
    # quit_engine call and strand this engine holding its port.
    try:
        return _run(port, shots, seed_hex, args.force_no_site)
    except SetupFailure as failure:
        return report_setup_failure(str(failure))
    finally:
        quit_engine(port, proc)


def _run(port: int, shots: str, seed_hex: str, force_no_site: bool) -> int:
    # -- Real UI flow to the in-game HUD (same path as tools/offscreen_probe.py):
    # this is what actually brings up the gameplay camera and HUD the
    # ghosts are graded against, which a raw debug-console
    # world.initArena bypass would skip.
    menu_up = poll_until(60.0, lambda: find_widget(port, "Create World"))
    require_setup("loading screen -> main menu", bool(menu_up),
                  "the main menu never appeared, so the real UI flow to the "
                  "HUD never started.")
    require_setup("click 'Create World'", click_widget(port, "Create World"),
                  "the 'Create World' button could not be clicked.")
    create_up = poll_until(20.0, lambda: find_widget(port, "Generate World"))
    require_setup("create-world screen reached", bool(create_up),
                  "the create-world screen never appeared.")

    # -- Pin the fixture world (#1587) BEFORE generating: left alone this
    # screen rolls a fresh random seed each run, and a world whose origin
    # is open ocean has no building site to render on at all.
    shown, pending = pin_seed(port, seed_hex)
    require_setup(f"seed control pinned to 0x{seed_hex}",
                  shown == seed_hex and pending == seed_hex,
                  f"the create-world seed could not be pinned to "
                  f"0x{seed_hex}: the seed control reads {shown!r} and the "
                  f"menu's pending params carry {pending!r}. Without a pinned "
                  f"seed the fixture world is random, so nothing graded here "
                  f"would be reproducible.")

    require_setup("click 'Generate World'", click_widget(port, "Generate World"),
                  "the 'Generate World' button could not be clicked.")

    def world_done():
        got = send(port, "local p = world.getInitProgress(); return p", timeout=5.0)
        return got.strip() == "3"

    print("  (generating world, ~1 min)")
    require_setup("worldgen completes (phase 3)",
                  bool(poll_until(300.0, world_done, interval=2.0)),
                  "world generation never reached phase 3.")
    cont = poll_until(60.0, lambda: find_widget(port, "Continue"))
    require_setup("post-generation Continue button appears", bool(cont),
                  "the post-generation 'Continue' button never appeared.")
    require_setup("click 'Continue'", click_widget(port, "Continue"),
                  "the 'Continue' button could not be clicked.")
    hud_up = poll_until(60.0, lambda: not find_widget(port, "Continue"))
    require_setup("in-game HUD reached", bool(hud_up),
                  "the in-game HUD never came up, so nothing was ever "
                  "framed or graded.")
    time.sleep(2.0)  # let the first in-game frames render

    pageid = wid(port)
    require_setup("active world id resolves", bool(pageid),
                  "world.getActiveWorldId() named no page, so there is no "
                  "world to designate on.")

    # The engine's OWN reading of which world this is — proof the pinned
    # seed survived generation.lua's tonumber(seed, 16), not just that
    # the text box accepted it.
    live_seed = send(port, f"return world.getSeed('{pageid}')").strip()
    want_seed = str(int(seed_hex, 16))
    require_setup(f"generated world carries seed {want_seed}",
                  live_seed == want_seed,
                  f"the generated world reports seed {live_seed!r}, not the "
                  f"pinned {want_seed} (0x{seed_hex}). The fixture world is "
                  f"not the verified one, so its terrain is unknown.")
    size_txt, plates_txt = pending_gen_params(port)

    # -- Register the throwaway 2x3 + 1x1-control building defs.
    with open(TEST_BUILDING_YAML, "w") as f:
        f.write(TEST_BUILDINGS)
    n = send(port, f"return engine.loadBuildingYaml('{TEST_BUILDING_YAML}')")
    try:
        loaded = float(n)
    except (TypeError, ValueError):
        loaded = -1.0
    require_setup("probe building defs loaded", loaded == 2.0,
                  f"engine.loadBuildingYaml registered {n!r} of the probe's 2 "
                  f"throwaway building defs from {TEST_BUILDING_YAML}. "
                  f"Nothing downstream of a fixture that never registered can "
                  f"be graded.")

    # Load wide enough to cover every candidate in one shot (both lists
    # span roughly -25..25 tiles -> chunks -2..2) rather than paying a
    # load/wait round trip per candidate tried below.
    print("  (scanning nearby terrain for a dry anchor site)")
    n_queued = send(port, "return world.loadChunksInRegion(-2, -2, 2, 2)")
    n_remaining = send(port, "return world.waitForChunks(60)", timeout=65.0)
    print(f"  (chunks queued={n_queued!r}, remaining after wait={n_remaining!r})")

    cands_1x1 = [] if force_no_site else CANDIDATES_1X1
    cands_2x3 = [] if force_no_site else CANDIDATES_2X3
    if force_no_site:
        print("  (--force-no-site: searching no candidate anchors at all)")
    anchor_1x1 = find_dry_anchor(port, 1, 1, cands_1x1)
    # The 2x3 target is STAKED later, so its site has to be one the
    # engine would really build on — see find_dry_anchor.
    anchor_2x3 = find_dry_anchor(port, 2, 3, cands_2x3, DEF_2X3)
    cause = site_cause(anchor_1x1, anchor_2x3, seed_hex)
    require_setup("found dry sites for both the control and the blueprint",
                  cause is None, cause or "")
    ax1, ay1, z1 = anchor_1x1
    ax2, ay2, z2 = anchor_2x3

    # -- The fixture's identity, on one line, so two runs can be compared
    # for more than "both exited 0" (#1587 acceptance).
    print(f"FIXTURE seed=0x{seed_hex} worldSize={size_txt} "
          f"plateCount={plates_txt} anchor1x1={ax1},{ay1}@{z1} "
          f"anchor2x3={ax2},{ay2}@{z2}")

    # -- Requirement 3: both anchors must be inside the region
    # camera.goToTile can frame WITHOUT clamping, or the screenshots
    # below would show somewhere other than the tile just designated.
    # Measured against the live engine rather than restated from its
    # constants — see unclamped_landing_cause.
    origin = goto_and_read(port, 0, 0)
    unit_x = goto_and_read(port, 1, 0)
    unit_y = goto_and_read(port, 0, 1)
    require_setup("camera.goToTile answers with a position",
                  None not in (origin, unit_x, unit_y),
                  f"camera.getPosition() did not report a teleport landing "
                  f"(origin={origin!r}, +1 gx={unit_x!r}, +1 gy={unit_y!r}), "
                  f"so anchor framing could not be verified.")
    cause = basis_cause(origin, unit_x, unit_y)
    require_setup("camera.goToTile spans both tile axes unclamped",
                  cause is None, cause or "")
    for label, ax, ay in (("1x1 control", ax1, ay1), ("2x3 blueprint", ax2, ay2)):
        cause = unclamped_landing_cause(label, ax, ay, origin, unit_x, unit_y,
                                        goto_and_read(port, ax, ay))
        require_setup(f"{label} anchor frames unclamped", cause is None,
                      cause or "")

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

    # -- The designation MAP holds only the anchor (#807 req 2, the half
    # #1845 keeps): an off-anchor footprint tile reports no designation
    # of its own, and the ONE quad the render pass draws is anchored on
    # the entry that does exist.
    interior = designation_at(port, pageid, ax2 + 1, ay2 + 1)
    check("an off-anchor footprint tile has no designation entry of its own",
          interior is None, f"got {interior!r}")
    anchor_entry = designation_at(port, pageid, ax2, ay2)
    check("the anchor tile itself IS the (only) designation entry",
          isinstance(anchor_entry, dict), f"got {anchor_entry!r}")

    st_before = png_stats(shot_before_1x1)
    check("screenshots are valid non-trivial PNGs",
          bool(st_before) and st_before[2] >= 3, f"got {st_before}")
    diff_1x1 = png_diff_count(shot_before_1x1, shot_after_1x1)
    diff_2x3 = png_diff_count(shot_before_2x3, shot_after_2x3)
    check("1x1 control ghost is visible at all", diff_1x1 > 0, f"diff={diff_1x1}")
    check("2x3 blueprint ghost is visible at all", diff_2x3 > 0, f"diff={diff_2x3}")

    box_1x1 = png_diff_bbox(shot_before_1x1, shot_after_1x1)
    box_2x3 = png_diff_bbox(shot_before_2x3, shot_after_2x3)
    w1, h1 = bbox_size(box_1x1)
    w2, h2 = bbox_size(box_2x3)
    print(f"GHOST BOXES 1x1={box_1x1} ({w1}x{h1})  2x3={box_2x3} ({w2}x{h2})")

    # -- STAKE the 2x3 building over its own live designation. The
    # designation ghost and the staked pre-delivery ghost are the same
    # 60 % picture of the same definition at the same anchor (#1845
    # requirement 3), so this must change essentially nothing on screen
    # — and the render pass must not stack a second 60 % quad on the
    # first while both records exist.
    staked = send_json(port, f"return building.spawn('{DEF_2X3}', {ax2}, {ay2})",
                       timeout=10.0)
    # A refused spawn means NOTHING was staked, and both comparisons below
    # would then compare a screenshot to itself and pass. That is a
    # fixture failure, not a rendering regression, so it exits SETUP.
    require_setup("the 2x3 fixture stakes at its own designation anchor",
                  isinstance(staked, (int, float)) and staked,
                  f"building.spawn('{DEF_2X3}', {ax2}, {ay2}) was refused "
                  f"({staked!r}), so no staked building exists to compare the "
                  f"designated ghost against and this run graded neither the "
                  f"box equality nor the no-jump claim.")
    time.sleep(0.5)
    live = send_json(port, f"return building.getInfo({int(staked)})", timeout=10.0)
    require_setup("the staked building is really on the page",
                  isinstance(live, dict),
                  f"building.getInfo({int(staked)}) answered {live!r} after a "
                  f"spawn that reported success, so there is no instance for "
                  f"the ghost comparison to be about.")
    shot_staked = os.path.join(shots, "after_stake_2x3.png")
    check("post-stake screenshot answers", screenshot(port, shot_staked))
    box_staked = png_diff_bbox(shot_before_2x3, shot_staked)
    ws_, hs_ = bbox_size(box_staked)
    print(f"STAKED BOX {box_staked} ({ws_}x{hs_})")
    # …and the staked building really is DRAWN, so the box equality below
    # is a comparison of two visible things.
    check("the staked building is visible at all", ws_ > 0 and hs_ > 0,
          f"staked box={box_staked}")

    # THE claim this probe exists for since #1845: the planned ghost
    # occupies exactly the rectangle the real building occupies. #807's
    # marker could not have satisfied this — six 96x64 diamonds tile a
    # different shape than one 96x96 sprite dropped by tile_bottom.
    check("the designated ghost's box IS the staked building's box "
          "(same dimensions and anchor offset as the placed sprite)",
          boxes_agree(box_2x3, box_staked, GHOST_BOX_TOL),
          f"designated={box_2x3}, staked={box_staked} "
          f"(tolerance {GHOST_BOX_TOL}px per edge)")

    # …and the transition between them is invisible: staking changes far
    # less than designating did. Not "zero": the two records coexist for
    # a frame and the engine re-sorts, so a handful of edge pixels may
    # move.
    diff_stake_step = png_diff_count(shot_after_2x3, shot_staked)
    check("staking causes no visible jump (changes far less than the "
          "designation itself did)",
          diff_stake_step * 10 < diff_2x3,
          f"designate diff={diff_2x3}, stake-step diff={diff_stake_step}")

    # -- The size control: a def with a different sprite draws a
    # different box. This is what makes the equality above a statement
    # about the DEFINITION rather than about one fixed quad size that
    # every ghost happens to share.
    check("the 1x1 control's ghost is visibly a different, smaller shape "
          "(its sprite is a third of the 2x3's on each axis)",
          w1 > 0 and h1 > 0
          and w1 <= w2 * CONTROL_BOX_MAX_FRACTION
          and h1 <= h2 * CONTROL_BOX_MAX_FRACTION,
          f"1x1 box={w1}x{h1}, 2x3 box={w2}x{h2}")

    print(f"\nscreenshots kept in {shots}")
    if failures:
        print(f"construction_blueprint_footprint_probe: {failures} check(s) "
              f"FAILED — the footprint was graded and came up short "
              f"(exit {FAIL_EXIT})")
        return verdict_exit(failures)
    print("construction_blueprint_footprint_probe: all checks passed")
    return verdict_exit(failures)


# --------------------------------------------------------------------------
# Self-test (#1587 requirement 4) — no engine, no GPU, no world.
# --------------------------------------------------------------------------
def self_test() -> int:
    """Prove the two failure modes stay distinguishable, repeatably.

    A live run shows the fixture holds; it cannot show what happens when
    it does not. These cases drive the same classification functions and
    the same exit-code mapping a live run uses, with synthetic readings.
    """
    problems: list[str] = []

    def expect(label: str, got, want) -> None:
        if got != want:
            problems.append(f"{label}: expected {want!r}, got {got!r}")

    def expect_named(label: str, cause: str | None, needle: str) -> None:
        if cause is None or needle not in cause:
            problems.append(
                f"{label}: expected a cause naming {needle!r}, got {cause!r}")

    # --- the site search: both anchors present is the only clean case --
    expect("both anchors found", site_cause((0, 0, 5), (5, 5, 6), "0000002A"),
           None)
    expect_named("no control site", site_cause(None, (5, 5, 6), "0000002A"),
                 "the 1x1 control")
    expect_named("no blueprint site", site_cause((0, 0, 5), None, "0000002A"),
                 "the 2x3 blueprint")
    expect_named("neither site", site_cause(None, None, "0000002A"),
                 "the 1x1 control and the 2x3 blueprint")
    expect_named("the site cause names the fixture world",
                 site_cause(None, None, "DEADBEEF"), "0xDEADBEEF")
    expect_named("the site cause says nothing was graded",
                 site_cause(None, None, "0000002A"), "graded no footprint")
    # An empty candidate list is exactly what --force-no-site passes, so
    # the forced path lands on this same cause rather than a private one.
    expect("an empty candidate list finds nothing",
           find_dry_anchor(0, 1, 1, []), None)

    # --- the camera-landing proof --------------------------------------
    # A square isometric basis, the shape gridToWorld actually produces:
    # +1 gx moves (+w, +h), +1 gy moves (-w, +h).
    o, ux, uy = (0.0, 0.0), (32.0, 16.0), (-32.0, 16.0)
    expect("a two-axis basis is accepted", basis_cause(o, ux, uy), None)
    expect_named("a v-axis pinned to centre is refused",
                 basis_cause(o, (32.0, 0.0), (-32.0, 0.0)), "two independent")
    expect_named("a collapsed basis is refused",
                 basis_cause(o, (32.0, 16.0), (32.0, 16.0)), "two independent")
    expect("an unclamped landing is accepted",
           unclamped_landing_cause("1x1 control", 5, -10, o, ux, uy,
                                   (5 * 32.0 - -10 * 32.0, 5 * 16.0 + -10 * 16.0)),
           None)
    expect("float32 noise under the tolerance is accepted",
           unclamped_landing_cause("1x1 control", 5, -10, o, ux, uy,
                                   (480.0 + 0.01, -80.0 - 0.01)),
           None)
    expect_named("a clamped landing is refused",
                 unclamped_landing_cause("2x3 blueprint", 0, 25, o, ux, uy,
                                         (-800.0, 100.0)),
                 "clamped the 2x3 blueprint anchor (0,25)")
    expect_named("a clamped landing names where it wanted to land",
                 unclamped_landing_cause("2x3 blueprint", 0, 25, o, ux, uy,
                                         (-800.0, 100.0)),
                 "(-800.000, 400.000)")
    expect_named("an unreadable landing is refused",
                 unclamped_landing_cause("1x1 control", 0, 0, o, ux, uy, None),
                 "did not report")

    # --- the #1845 geometry comparators --------------------------------
    # The graded claim is now a SHAPE comparison, so the comparators it
    # rests on get the same treatment as the camera proof above: driven
    # directly, with synthetic boxes, including the cases that must FAIL.
    expect("an identical box agrees",
           boxes_agree((10, 20, 110, 140), (10, 20, 110, 140), GHOST_BOX_TOL),
           True)
    expect("a box off by the tolerance still agrees",
           boxes_agree((10, 20, 110, 140),
                       (10 + GHOST_BOX_TOL, 20, 110, 140 - GHOST_BOX_TOL),
                       GHOST_BOX_TOL),
           True)
    expect("a box shifted by a tile_bottom drop does NOT agree",
           boxes_agree((10, 20, 110, 140), (10, 36, 110, 156), GHOST_BOX_TOL),
           False)
    expect("a box of a different size does NOT agree",
           boxes_agree((10, 20, 110, 140), (10, 20, 210, 140), GHOST_BOX_TOL),
           False)
    expect("a missing box never agrees",
           boxes_agree(None, (10, 20, 110, 140), GHOST_BOX_TOL), False)
    expect("an invisible ghost never agrees with a visible one",
           boxes_agree((10, 20, 110, 140), None, GHOST_BOX_TOL), False)
    expect("bbox_size measures a box", bbox_size((10, 20, 110, 140)),
           (100, 120))
    expect("bbox_size reads an absent box as nothing",
           bbox_size(None), (0, 0))

    # --- the exit-code mapping itself ---------------------------------
    # This is the live run's own mapping, called directly. Its diagnostic
    # is captured rather than printed so the self-test's output stays
    # readable — and then asserted, since a cause nobody can read would
    # leave the two failure modes indistinguishable in practice.
    out, err = io.StringIO(), io.StringIO()
    with contextlib.redirect_stdout(out), contextlib.redirect_stderr(err):
        setup_code = report_setup_failure("a synthetic fixture cause")
    expect("a fixture cause exits SETUP", setup_code, SETUP_EXIT)
    expect_named("the fixture diagnostic names its cause",
                 out.getvalue() + err.getvalue(), "a synthetic fixture cause")
    expect_named("the fixture diagnostic says nothing was graded",
                 out.getvalue() + err.getvalue(), "never graded")
    expect("SETUP is 2", SETUP_EXIT, 2)
    expect("a graded miss exits FAIL", verdict_exit(1), FAIL_EXIT)
    expect("several graded misses still exit FAIL", verdict_exit(4), FAIL_EXIT)
    expect("FAIL is 1", FAIL_EXIT, 1)
    expect("a clean grading passes", verdict_exit(0), 0)
    expect("the two failure modes differ", SETUP_EXIT == FAIL_EXIT, False)

    # --- require_setup routes a failed precondition to SetupFailure ----
    with contextlib.redirect_stdout(io.StringIO()):
        require_setup("a satisfied precondition", True, "unused")
        try:
            require_setup("a violated precondition", False, "a named cause")
        except SetupFailure as raised:
            expect("require_setup carries the cause", str(raised), "a named cause")
        else:
            problems.append("require_setup did not raise on a false precondition")

    # --- the candidate grid stays two disjoint halves ------------------
    expect("the candidate halves are disjoint",
           set(CANDIDATES_1X1) & set(CANDIDATES_2X3), set())
    expect("every candidate is covered",
           sorted(CANDIDATES_1X1 + CANDIDATES_2X3), sorted(_ALL_CANDIDATES))

    print(f"\n--- self-test ---\n  classification + exit-code cases: "
          f"{'FAILED' if problems else 'all passed'}")
    for problem in problems:
        print(f"  [FAIL] {problem}")
    return FAIL_EXIT if problems else 0


if __name__ == "__main__":
    sys.exit(main())
