#!/usr/bin/env python3
"""Structure construction frames — the #2488 PIXEL gate.

The hspec group "structure construction frames" proves everything a quad
can say: which frame an appearance selects, that the quad carries the
target's facemap handle, and that it carries the lifecycle render flag.
It cannot prove what that flag DOES, because the flag is read in the
fragment shader (`Engine.Graphics.Vulkan.ShaderCode`, `color.a *
faceAlpha` vs `color.a`) and a headless run has no fragments.

So this probe boots --offscreen (real Vulkan, no window), registers a
FIXTURE pack whose art is built to make the shader's decision legible in
the frame buffer, and reads the resulting pixels.

The fixture art
---------------
Three horizontal bands, each 96x64 pixels' worth of one tile:

  facemap:  band A = pure GREEN, opaque   -> a painted top-light mask
            band B = pure BLACK, opaque   -> RGB sums to ~0
            band C =  transparent          -> OUTSIDE the silhouette
  frame:    band A = pure RED,   opaque
            band B = pure GREEN, opaque
            band C = pure BLUE,  opaque
            band D = white at alpha 0      -> a transparent frame pixel
  static:   band A = pure RED,   opaque
            band C = pure CYAN,  opaque    -> the unflagged control

How a claim is attributed: difference AND position
-------------------------------------------------
The captures share ONE camera — pinned and zoomed before the first of
them and never touched again — and the charged unit is spawned before it
too, so it stands in all of them and cancels out. Paused, at a fixed sun
angle, two such frames differ only where something was added between
them:

  baseline -> site        = the construction site, and its side effects
  site     -> with static = the static control

That NARROWS the search but is not on its own proof of provenance:
`construction.addJobProgress` also stamps the D-18 corner slope into
`ctSlopes`, which re-meshes the tile, so terrain pixels legitimately land
in the first set. So every colour claim is also PLACED. Bands A (red) and
B (green) sit inside the facemap's silhouette and are therefore drawn
whatever the lifecycle flag does; their measured screen spans and their
known canvas columns give the quad's scale and origin, and every other
band's columns follow. Nothing reads the projection — the ruler is
measured off the frame under test.

Each claim is also gated on its own difference set being non-empty
first, so "no cyan" has to mean "clipped" and not "nothing was placed".

Five claims:

  1. band C's BLUE is in what the SITE drew, in band C's own columns.
     Without the lifecycle flag the reused facemap's zero alpha would
     multiply it away, so its presence is the flag working.
  2. no CYAN is in what the STATIC CONTROL drew — the same art, same
     facemap, one tile over, unflagged. That is the existing alpha
     behaviour, unchanged.
  3. band D's columns stay unpainted: the texture's own alpha still
     decides, flag or no flag.
  4. bands A, B and C come back at the SAME brightness. A is lit through
     a painted top mask, B through a zero-RGB one, C through no mask at
     all — so equality is the shader's zero-sum top-light fall-through
     surviving the alpha change, and the RGB path being untouched.
  5. at a zoom inside the world's fade band, where `tileAlpha` is about
     a half, band C comes back DIMMER — against its own baseline at the
     same zoom, with the designation popped. Scene opacity travels on
     the quad's tint and the flag does not touch it.

Needs a GPU (Vulkan device) — manual-only, never CI-gated.

Usage: python3 tools/structure_construction_probe.py [--port 9519]
       [--size 1280x720] [--out DIR] [--keep-open]
"""
from __future__ import annotations

import argparse
import os
import shutil
import struct
import sys
import time
import zlib

sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
from probelib import (boot, pin_camera_to_tile, poll_until, quit_engine, send,
                      send_json, set_paused, viewport)

failures = 0

PACK = "probe_construction"

# Relative to the resource root, because a declared construction frame
# path that escapes it is refused at registration — which is the point of
# that rule and not a limitation to work around.
FIXTURE_DIR = ".probe_construction_fixture"
STATIC_PATH = f"{FIXTURE_DIR}/static.png"
FACE_PATH = f"{FIXTURE_DIR}/face.png"
FRAME_PATHS = [f"{FIXTURE_DIR}/frame_{i}.png" for i in range(3)]

TILE_W, TILE_H = 96, 64

# The four bands, as (x0, x1) columns of the 96x64 canvas. Rows 8..55 in
# every band, which keeps them inside the tile diamond's vertical span.
BAND_A = (6, 28)
BAND_B = (28, 50)
BAND_C = (50, 72)
BAND_D = (72, 92)
BAND_Y = (8, 56)


def check(ok: bool, label: str, detail: str = "") -> bool:
    global failures
    if ok:
        print(f"  PASS  {label}")
    else:
        failures += 1
        print(f"  FAIL  {label}{(' — ' + detail) if detail else ''}")
    return ok


# --------------------------------------------------------------------------
# Fixture art
# --------------------------------------------------------------------------
def write_png(path: str, pixels) -> None:
    """A 96x64 RGBA PNG from a (x, y) -> (r, g, b, a) function.

    Hand-rolled rather than Pillow's writer: the probe already depends on
    Pillow to READ the capture, but the fixture art is the thing under
    test and building it out of raw bytes keeps it obvious.
    """
    raw = bytearray()
    for y in range(TILE_H):
        raw.append(0)                      # filter type 0 (None)
        for x in range(TILE_W):
            raw.extend(bytes(pixels(x, y)))
    def chunk(tag: bytes, data: bytes) -> bytes:
        return (struct.pack(">I", len(data)) + tag + data
                + struct.pack(">I", zlib.crc32(tag + data) & 0xFFFFFFFF))
    png = (b"\x89PNG\r\n\x1a\n"
           + chunk(b"IHDR", struct.pack(">IIBBBBB", TILE_W, TILE_H, 8, 6, 0, 0, 0))
           + chunk(b"IDAT", zlib.compress(bytes(raw), 9))
           + chunk(b"IEND", b""))
    with open(path, "wb") as fh:
        fh.write(png)


def in_band(x: int, y: int, band) -> bool:
    return band[0] <= x < band[1] and BAND_Y[0] <= y < BAND_Y[1]


def write_fixture_art(root: str) -> None:
    os.makedirs(root, exist_ok=True)

    def facemap(x, y):
        if in_band(x, y, BAND_A):
            return (0, 255, 0, 255)        # painted top-light mask
        if in_band(x, y, BAND_B):
            return (0, 0, 0, 255)          # RGB sums to ~0
        return (0, 0, 0, 0)                # outside the silhouette

    def frame(x, y):
        if in_band(x, y, BAND_A):
            return (255, 0, 0, 255)
        if in_band(x, y, BAND_B):
            return (0, 255, 0, 255)
        if in_band(x, y, BAND_C):
            return (0, 0, 255, 255)
        if in_band(x, y, BAND_D):
            return (255, 255, 255, 0)      # transparent frame pixel
        return (0, 0, 0, 0)

    def static(x, y):
        if in_band(x, y, BAND_A):
            return (255, 0, 0, 255)
        if in_band(x, y, BAND_C):
            return (0, 255, 255, 255)      # the unflagged control
        return (0, 0, 0, 0)

    write_png(os.path.join(root, "face.png"), facemap)
    write_png(os.path.join(root, "static.png"), static)
    # Three identical frames, so a progress of 0.5 lands on the MIDDLE
    # one and the sequence is a real sequence rather than a single image
    # the index can never get wrong.
    for i in range(3):
        write_png(os.path.join(root, f"frame_{i}.png"), frame)


# --------------------------------------------------------------------------
# Pixel oracles
# --------------------------------------------------------------------------
def load_rgb(path: str):
    return load_rgb_sized(path)[0]


def load_rgb_sized(path: str):
    """The capture's pixels and its WIDTH.

    The width comes from the file rather than the window: on a HiDPI
    display the framebuffer is larger than the window, and every located
    assertion maps a flat pixel index back to (x, y) through it.
    """
    from PIL import Image
    with Image.open(path) as im:
        w = im.width
        raw = im.convert("RGB").tobytes()
    return [tuple(raw[i:i + 3]) for i in range(0, len(raw), 3)], w


def changed(before, after, width: int):
    """`[(x, y, pixel)]` for every position `after` differs from `before`.

    Two frames captured with the SAME camera, paused, at a fixed sun
    angle, with the same units standing in both, differ only where
    something was added between them. So a difference set narrows the
    search — but it is NOT on its own proof that a colour came from the
    fixture: `construction.addJobProgress` also stamps the D-18 corner
    slope into `ctSlopes`, which re-meshes the tile, so terrain pixels
    are legitimately in the baseline->site set too.

    That is why every colour claim below is ALSO placed: the bands are
    located from the frame's own red band and the rest are asserted
    inside the columns that band implies. Difference plus position is
    what makes "this is the fixture's blue" a statement about the
    fixture.
    """
    if len(before) != len(after):
        return []
    return [(i % width, i // width, b)
            for i, (a, b) in enumerate(zip(before, after)) if a != b]


def dominant(pixels, channel: int, floor_: int = 90, others: int = 40):
    """Pixels where one channel is strong and the other two are near zero.

    Brightness scales all three channels by one factor, so a pure primary
    stays pure whatever the sun does — which is what makes a channel a
    usable label for "which band drew this pixel".
    """
    got = []
    for x, y, px in pixels:
        rest = [v for i, v in enumerate(px) if i != channel]
        if px[channel] >= floor_ and all(v <= others for v in rest):
            got.append((x, y, px))
    return got


def cyanish(pixels, floor_: int = 90, red_max: int = 40, tol: int = 12):
    return [(x, y, px) for x, y, px in pixels
            if px[0] <= red_max and px[1] >= floor_ and px[2] >= floor_
            and abs(px[1] - px[2]) <= tol]


def whitish(pixels, floor_: int = 200, tol: int = 12):
    return [(x, y, px) for x, y, px in pixels
            if min(px) >= floor_ and max(px) - min(px) <= tol]


def bbox(pixels):
    """(x0, y0, x1, y1) of a located pixel set, or None."""
    if not pixels:
        return None
    xs = [x for x, _, _ in pixels]
    ys = [y for _, y, _ in pixels]
    return (min(xs), min(ys), max(xs), max(ys))


def peak(pixels, channel: int):
    return max(px[channel] for _, _, px in pixels)


def band_columns(red_box, green_box, canvas_lo: int, canvas_hi: int):
    """Predict a canvas column range's SCREEN columns from two known bands.

    Band A (red) and band B (green) occupy fixed, adjacent canvas columns
    and are both drawn whatever the lifecycle flag does — they are inside
    the facemap's silhouette. Two known canvas spans and their two
    measured screen spans give the quad's scale and origin, and every
    other band follows. Nothing here reads the projection: it is measured
    off the very frame under test.
    """
    lo_canvas, hi_canvas = BAND_A[0], BAND_B[1]
    lo_screen, hi_screen = red_box[0], green_box[2] + 1
    scale = (hi_screen - lo_screen) / float(hi_canvas - lo_canvas)
    return (lo_screen + (canvas_lo - lo_canvas) * scale,
            lo_screen + (canvas_hi - lo_canvas) * scale)


def within(pixels, lo: float, hi: float, slack: float = 2.0) -> bool:
    """Is every one of these pixels inside a screen column range?"""
    return all(lo - slack <= x <= hi + slack for x, _, _ in pixels)


# --------------------------------------------------------------------------
# Engine plumbing
# --------------------------------------------------------------------------
def active_page(port: int):
    got = send_json(port, "local id = world.getActiveWorldId();"
                          " return id and {page = tostring(id)} or nil")
    if not isinstance(got, dict):
        return None
    page = str(got.get("page", "")).strip()
    return page if page and page != "nil" else None


def wait_content_loaded(port: int, seconds: float = 90.0) -> bool:
    probe = ("return {units = #unit.listDefs(),"
             " buildings = #building.listDefs()}")

    def ready():
        got = send_json(port, probe)
        return (isinstance(got, dict) and got.get("units", 0) > 0
                and got.get("buildings", 0) > 0)
    return bool(poll_until(seconds, ready))


def register_fixture_pack(port: int) -> str:
    """Register the fixture pack through the production verb.

    Deliberately NOT through scripts/structures.lua: that module's own
    pack-loading path is covered by the hspec group, and reaching it here
    would mean re-registering after the boot loader already claimed its
    pack name. What this probe needs is a pack whose ART is built to make
    the shader's decision visible, which is the payload, not the loader.
    """
    frames = ", ".join(
        f"{{texture='{p}', texHandle=engine.loadTexture('{p}')}}"
        for p in FRAME_PATHS)
    lua = (
        f"local hs = engine.loadTexture('{STATIC_PATH}');"
        f" local hf = engine.loadTexture('{FACE_PATH}');"
        f" local ok = structure.registerPackArt{{ pack='{PACK}',"
        "   kinds={{kind='floor', buildable=true, build_work=1.0,"
        "           materials={}}},"
        f"   art={{{{kind='floor', texture='{STATIC_PATH}', texHandle=hs,"
        f"           facemap='{FACE_PATH}', faceHandle=hf}}}},"
        f"   construction={{{{kind='floor', texture='{STATIC_PATH}',"
        f"                    texHandle=hs, frames={{{frames}}}}}}} }};"
        " return tostring(ok)")
    return (send(port, lua, timeout=30.0) or "").strip()


def capture(port: int, path: str, settle: float = 1.5) -> bool:
    """One capture, with no way to mistake a stale file for a fresh one.

    The path is REMOVED first and the engine's own reply is checked, so a
    screenshot that never happened cannot be read back as the frame under
    test — which is exactly how a pixel gate turns into a pass that
    compared nothing.
    """
    try:
        os.remove(path)
    except FileNotFoundError:
        pass
    time.sleep(settle)
    reply = (send(port, f"return tostring(debug.captureScreenshot('{path}'))",
                  timeout=30.0) or "").strip()
    if reply.lower() in ("false", "nil", ""):
        return False
    return os.path.exists(path) and os.path.getsize(path) > 4096


def main() -> int:
    ap = argparse.ArgumentParser()
    ap.add_argument("--port", type=int, default=9519)
    ap.add_argument("--size", default="1280x720")
    ap.add_argument("--out", default="/tmp/structure_construction")
    ap.add_argument("--keep-open", action="store_true")
    args = ap.parse_args()

    os.makedirs(args.out, exist_ok=True)
    write_fixture_art(FIXTURE_DIR)

    proc = boot(args.port, args=["--size", args.size], label="offscreen",
                mode=("--offscreen",))
    try:
        print("phase 1: content load")
        if not check(wait_content_loaded(args.port),
                     "unit/building defs populated"):
            return 1

        print("phase 2: gameplay view on the flat arena")
        send(args.port, "package.loaded['scripts.ui_manager'].onOpenArena();"
                        " return 'ok'", timeout=20.0)
        page = poll_until(90.0, lambda: active_page(args.port))
        if not check(bool(page), "a real arena page is active"):
            return 1
        vp = viewport(args.port, fallback=(1280, 720))
        cx = int(vp.get("win_w", 1280)) // 2
        cy = int(vp.get("win_h", 720)) // 2
        pick = (f"local gx, gy = world.pickTile({cx}, {cy});"
                " return gx and {gx = gx, gy = gy} or nil")
        if not check(bool(poll_until(90.0,
                                     lambda: send_json(args.port, pick))),
                     "the arena renders and picks"):
            return 1

        print("phase 3: freeze the frame")
        set_paused(args.port, True)
        send(args.port, "world.setTimeScale(0); world.setSunAngle(0.5);"
                        " return 'ok'")
        anchor = poll_until(30.0, lambda: send_json(args.port, pick))
        if not isinstance(anchor, dict):
            check(False, "picked an anchor tile")
            return 1
        ax, ay = int(anchor["gx"]), int(anchor["gy"])
        print(f"        anchor tile ({ax}, {ay}) on page '{page}'")

        print("phase 4: register the fixture pack, then FIX the camera")
        if not check(register_fixture_pack(args.port) == "true",
                     "structure.registerPackArt accepted the fixture pack"):
            return 1

        site_x, site_y = ax, ay
        ctrl_x, ctrl_y = ax + 2, ay
        # The camera is pinned and zoomed BEFORE the baseline and never
        # touched again, so all three captures share one framing. That is
        # what makes a pixel difference between them attributable to what
        # was added between them, and nothing else — a baseline framed
        # differently would let terrain or HUD pixels move into and out of
        # the colour predicates on their own.
        floor_z = send_json(args.port,
                            f"return (world.getTerrainAt({site_x}, {site_y}))")
        pin_camera_to_tile(args.port, site_x + 1, site_y,
                           (int(floor_z) + 1) if isinstance(floor_z, int) else 26)
        send(args.port, "camera.setZoom(0.25); return 'ok'")

        # The builder is spawned BEFORE the baseline, deliberately.
        # `payMaterials` charges a UNIT's inventory, and the fixture's
        # cost is an authored EMPTY bill — a real, valid paid state — so
        # any unit will do and none has to be carrying anything; what the
        # call is for is the receipt, which is what makes the designation
        # PAID and therefore drawn at all. Spawning it here rather than
        # later means it stands in EVERY capture and cancels out of every
        # difference set, wherever it happens to be — which is a stronger
        # guarantee than putting it off-camera and hoping.
        #
        # Any unit on the active page will do, so an EXISTING one is
        # preferred: `unit.spawn` answers -1 whenever the registry or the
        # spawn tile is not ready, and depending on it made this step the
        # probe's flakiest. Spawning stays as the fallback, still polled.
        # The def name comes from the engine's OWN list rather than a
        # hardcoded 'acolyte': `unit.spawn` answers -1 for a def the
        # registry has not loaded yet, and the content load finishes one
        # family at a time, so naming a specific unit made this the
        # probe's flakiest step for no benefit — any unit can be charged
        # an empty bill. The spawn is on the SITE tile, which the
        # designation just proved is resident.
        def try_spawn():
            raw = send(args.port,
                       "local d = unit.listDefs();"
                       " if not d or #d == 0 then return -1 end;"
                       f" return unit.spawn(d[1], {site_x}, {site_y})")
            try:
                uid = int(float((raw or "").strip()))
            except (TypeError, ValueError):
                return None
            return uid if uid > 0 else None

        found = poll_until(20.0, lambda: send_json(
            args.port, "local ids = unit.getAllIds();"
                       " return ids and ids[1] and {uid = ids[1]} or nil"))
        builder = (int(found["uid"]) if isinstance(found, dict)
                   else poll_until(60.0, try_spawn))
        if not check(bool(builder),
                     "a unit exists to charge the (empty) bill to",
                     "unit.spawn never returned a real uid"):
            return 1

        base_path = os.path.join(args.out, "0_baseline.png")
        if not check(capture(args.port, base_path), "captured the baseline"):
            return 1
        base, width = load_rgb_sized(base_path)

        print("phase 5: a PAID designation with declared frames")
        ok = send(args.port,
                  f"return tostring(construction.designate('{page}',"
                  f" {site_x}, {site_y}, {site_x}, {site_y}, 'structure',"
                  f" '{PACK}', 'floor'))", timeout=20.0)
        check((ok or "").strip() == "true", "the designation was accepted")
        job = poll_until(30.0, lambda: send_json(
            args.port, f"return construction.getDesignationAt('{page}',"
                       f" {site_x}, {site_y})"))
        if not check(isinstance(job, dict) and "attempt" in job,
                     "the designation landed", f"got {job!r}"):
            return 1
        attempt = int(job["attempt"])

        paid = send(args.port,
                    f"return tostring(construction.payMaterials('{page}',"
                    f" {site_x}, {site_y}, {attempt}, {builder}))",
                    timeout=20.0)
        check((paid or "").strip() == "true", "the designation is PAID")
        # Halfway: the middle frame of three.
        send(args.port, f"construction.addJobProgress('{page}', {site_x},"
                        f" {site_y}, 0.5, {attempt}); return 'ok'")
        got = poll_until(30.0, lambda: (send_json(
            args.port, f"return construction.getDesignationAt('{page}',"
                       f" {site_x}, {site_y})") or {}).get("paid") is True)
        check(bool(got), "the paid state is readable back")

        site_path = os.path.join(args.out, "1_construction.png")
        if not check(capture(args.port, site_path),
                     "captured the construction site"):
            return 1
        site = load_rgb(site_path)
        site_px = changed(base, site, width)
        if not check(len(site_px) > 200,
                     "the construction site changed the frame at all",
                     f"{len(site_px)} px differ from the baseline"):
            return 1

        print("phase 6: the unflagged STATIC control, one tile over")
        z = send_json(args.port,
                      f"return (world.getTerrainAt({ctrl_x}, {ctrl_y}))")
        placed = send(args.port,
                      f"local hs = engine.loadTexture('{STATIC_PATH}');"
                      f" local hf = engine.loadTexture('{FACE_PATH}');"
                      f" return tostring(structure.place({ctrl_x}, {ctrl_y},"
                      f" 'floor', hs, hf,"
                      f" {int(z) + 1 if isinstance(z, int) else 1},"
                      f" '{STATIC_PATH}', '{FACE_PATH}'))", timeout=20.0)
        check((placed or "").strip() == "true", "the static control placed")
        ctrl_path = os.path.join(args.out, "2_with_static.png")
        if not check(capture(args.port, ctrl_path),
                     "captured the static control"):
            return 1
        with_ctrl = load_rgb(ctrl_path)
        ctrl_px = changed(site, with_ctrl, width)
        if not check(len(ctrl_px) > 200,
                     "the static control changed the frame at all — so its "
                     "absence of cyan below is clipping, not an unplaced piece",
                     f"{len(ctrl_px)} px differ from the previous capture"):
            return 1

        print("phase 7: locate the fixture's bands, then read them")
        # Bands A (red) and B (green) are INSIDE the facemap silhouette,
        # so they are drawn whatever the lifecycle flag does — which is
        # what makes them a usable ruler. Everything else is asserted
        # against the columns they imply, so a terrain pixel the D-18
        # slope stamp put into the difference set cannot satisfy a claim
        # by being the right colour somewhere else on screen.
        reds = dominant(site_px, 0)
        greens = dominant(site_px, 1)
        if not check(len(reds) > 100 and len(greens) > 100,
                     "the two in-silhouette bands located the sprite",
                     f"red={len(reds)} green={len(greens)}"):
            return 1
        red_box, green_box = bbox(reds), bbox(greens)
        if not check(red_box[2] < green_box[0],
                     "band A sits left of band B, as the fixture draws them",
                     f"red={red_box} green={green_box}"):
            return 1
        c_lo, c_hi = band_columns(red_box, green_box, *BAND_C)
        d_lo, d_hi = band_columns(red_box, green_box, *BAND_D)
        print(f"        band C predicted at screen columns "
              f"{c_lo:.1f}..{c_hi:.1f}, band D at {d_lo:.1f}..{d_hi:.1f}")

        blues = dominant(site_px, 2)
        check(len(blues) > 100 and within(blues, c_lo, c_hi),
              "a frame pixel OUTSIDE the reused facemap's silhouette is "
              "visible, in band C's own columns",
              f"{len(blues)} blue px, bbox {bbox(blues)}")

        cyans = cyanish(ctrl_px)
        check(not cyans,
              "the UNFLAGGED static piece's out-of-silhouette pixels are "
              "still clipped",
              f"{len(cyans)} cyan px of {len(ctrl_px)} the control drew, "
              f"bbox {bbox(cyans)}")

        whites = whitish(site_px) + whitish(ctrl_px)
        check(not [p for p in whites if within([p], d_lo, d_hi)],
              "a fully transparent frame pixel paints nothing, flag or no "
              "flag",
              f"{len(whites)} white px, bbox {bbox(whites)}")

        if check(bool(blues), "all three lit bands reached the frame buffer",
                 f"red={len(reds)} green={len(greens)} blue={len(blues)}"):
            peaks = (peak(reds, 0), peak(greens, 1), peak(blues, 2))
            check(max(peaks) - min(peaks) <= 6,
                  "a painted top mask, a zero-RGB mask and NO mask all light "
                  "the frame identically",
                  f"peaks red/green/blue = {peaks}")

        print("phase 8: scene opacity still multiplies the flagged quad")
        # `tileAlpha` is the zoom fade (World.Render: 1.0 below
        # zoomFadeStart, ramping to 0 at zoomFadeEnd) and the
        # construction pass draws at `opaqueTint tileAlpha`, so a zoom
        # inside that band is a scene opacity below 1 with nothing else
        # changed. Its own baseline is the SAME zoom with the designation
        # cancelled, so the comparison is like for like.
        fade_lo = send_json(args.port, "return camera.getZoomFadeStart()")
        fade_hi = send_json(args.port, "return camera.getZoomFadeEnd()")
        mid = (float(fade_lo) + float(fade_hi)) / 2.0 \
            if isinstance(fade_lo, (int, float)) \
            and isinstance(fade_hi, (int, float)) else 1.4
        send(args.port, f"camera.setZoom({mid}); return 'ok'")
        faded_path = os.path.join(args.out, "3_faded.png")
        if not check(capture(args.port, faded_path),
                     f"captured at zoom {mid} (scene opacity ~0.5)"):
            return 1
        faded = load_rgb(faded_path)
        # The ATOMIC pop, not the queued cancel: `cancelDesignation` is
        # fire-and-forget on the world thread, and this frame is paused.
        # `cancelDesignationForRefund` removes the designation and
        # returns it in one synchronous step, so the very next capture
        # cannot still be showing it.
        popped = send(args.port,
                      f"local j = construction.cancelDesignationForRefund("
                      f"'{page}', {site_x}, {site_y});"
                      " return tostring(j ~= nil)", timeout=20.0)
        gone = poll_until(30.0, lambda: send_json(
            args.port, f"return construction.getDesignationAt('{page}',"
                       f" {site_x}, {site_y})") is None)
        if not check((popped or "").strip() == "true" and bool(gone),
                     "the designation was cancelled",
                     f"cancelDesignationForRefund -> {popped!r}"):
            return 1
        faded_base_path = os.path.join(args.out, "4_faded_baseline.png")
        if not check(capture(args.port, faded_base_path),
                     "captured the faded baseline"):
            return 1
        faded_px = changed(load_rgb(faded_base_path), faded, width)
        check(len(faded_px) > 50,
              "the site is still drawn at a scene opacity below 1",
              f"{len(faded_px)} px differ once the designation goes")
        faded_reds = dominant(faded_px, 0)
        faded_greens = dominant(faded_px, 1)
        faded_blues = dominant(faded_px, 2)
        if check(bool(faded_reds) and bool(faded_greens) and bool(faded_blues),
                 "the same three bands are still identifiable at the faded "
                 "zoom",
                 f"red={len(faded_reds)} green={len(faded_greens)} "
                 f"blue={len(faded_blues)}"):
            f_lo, f_hi = band_columns(bbox(faded_reds), bbox(faded_greens),
                                      *BAND_C)
            check(within(faded_blues, f_lo, f_hi),
                  "…still in band C's own columns, re-measured at this zoom",
                  f"blue bbox {bbox(faded_blues)} vs {f_lo:.1f}..{f_hi:.1f}")
            # tileAlpha halves at the fade band's midpoint, and the quad's
            # tint carries it, so the band blends with the ground behind
            # instead of painting at full strength. A flag that made the
            # texture's alpha authoritative for the TINT as well — rather
            # than only for the face map — would leave this unchanged.
            full, half = peak(blues, 2), peak(faded_blues, 2)
            check(half <= full * 0.75,
                  "…and the tint's alpha still multiplies the flagged quad: "
                  "band C is dimmer at half scene opacity",
                  f"peak blue {full} at full opacity vs {half} at ~0.5")
    finally:
        if not args.keep_open:
            quit_engine(args.port, proc)
        shutil.rmtree(FIXTURE_DIR, ignore_errors=True)

    print()
    for name in ("0_baseline.png", "1_construction.png", "2_with_static.png",
                 "3_faded.png", "4_faded_baseline.png"):
        print(f"  {os.path.join(args.out, name)}")
    print()
    if failures:
        print(f"structure_construction_probe: {failures} failure(s)")
        return 1
    print("structure_construction_probe: all checks passed.")
    return 0


if __name__ == "__main__":
    sys.exit(main())
