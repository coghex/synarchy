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

Four claims, one capture each:

  1. band C's BLUE reaches the frame buffer. Without the lifecycle flag
     the reused facemap's zero alpha would multiply it away, so its
     presence is the flag working.
  2. the SAME piece's art drawn as an ordinary STATIC structure — the
     control, one tile over, same facemap, unflagged — shows no CYAN.
     That is the existing alpha behaviour, unchanged.
  3. band D paints nothing: the texture's own alpha still decides, flag
     or no flag, so a transparent frame pixel stays transparent.
  4. bands A, B and C come back at the SAME brightness. A is lit through
     a painted top mask, B through a zero-RGB one, C through no mask at
     all — so equality is the shader's zero-sum top-light fall-through
     surviving the alpha change, and the RGB path being untouched.

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
    from PIL import Image
    with Image.open(path) as im:
        raw = im.convert("RGB").tobytes()
    return [tuple(raw[i:i + 3]) for i in range(0, len(raw), 3)]


def dominant(pixels, channel: int, floor_: int = 90, others: int = 40):
    """Pixels where one channel is strong and the other two are near zero.

    Brightness scales all three channels by one factor, so a pure primary
    stays pure whatever the sun does — which is what makes a channel a
    usable label for "which band drew this pixel".
    """
    got = []
    for px in pixels:
        rest = [v for i, v in enumerate(px) if i != channel]
        if px[channel] >= floor_ and all(v <= others for v in rest):
            got.append(px[channel])
    return got


def cyanish(pixels, floor_: int = 90, red_max: int = 40, tol: int = 12):
    return [px for px in pixels
            if px[0] <= red_max and px[1] >= floor_ and px[2] >= floor_
            and abs(px[1] - px[2]) <= tol]


def whitish(pixels, floor_: int = 200, tol: int = 12):
    return [px for px in pixels
            if min(px) >= floor_ and max(px) - min(px) <= tol]


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
    time.sleep(settle)
    send(port, f"return debug.captureScreenshot('{path}')", timeout=30.0)
    return os.path.exists(path)


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

        print("phase 4: register the fixture pack")
        if not check(register_fixture_pack(args.port) == "true",
                     "structure.registerPackArt accepted the fixture pack"):
            return 1

        # A baseline BEFORE anything of ours is on screen. The arena's own
        # ground scatter is re-randomised per process, so a colour that
        # happens to occur naturally must be discounted rather than
        # assumed absent.
        base_path = os.path.join(args.out, "0_baseline.png")
        if not check(capture(args.port, base_path), "captured the baseline"):
            return 1
        base = load_rgb(base_path)

        print("phase 5: a PAID designation with declared frames")
        site_x, site_y = ax, ay
        ctrl_x, ctrl_y = ax + 2, ay
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

        # payMaterials charges a UNIT's inventory. The fixture's cost is
        # an authored EMPTY bill — a real, valid paid state — so any unit
        # will do and none has to be carrying anything; what the call is
        # here for is the receipt, which is what makes the designation
        # PAID and therefore drawn by the construction pass at all.
        raw = send(args.port,
                   f"return unit.spawn('acolyte', {site_x + 4}, {site_y + 4})")
        try:
            builder = int(float((raw or "").strip()))
        except (TypeError, ValueError):
            builder = -1
        if not check(builder > 0,
                     "a unit exists to charge the (empty) bill to",
                     f"unit.spawn returned {raw!r}"):
            return 1
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

        print("phase 6: the unflagged STATIC control, one tile over")
        z = send_json(args.port,
                      f"return (world.getTerrainAt({ctrl_x}, {ctrl_y}))")
        placed = send(args.port,
                      f"local hs = engine.loadTexture('{STATIC_PATH}');"
                      f" local hf = engine.loadTexture('{FACE_PATH}');"
                      f" return tostring(structure.place({ctrl_x}, {ctrl_y},"
                      f" 'floor', hs, hf, {int(z) + 1 if isinstance(z, int) else 1},"
                      f" '{STATIC_PATH}', '{FACE_PATH}'))", timeout=20.0)
        check((placed or "").strip() == "true", "the static control placed")

        print("phase 7: capture and read the pixels")
        floor_z = send_json(args.port,
                            f"return structure.floorZAt({ctrl_x}, {ctrl_y})")
        pin_camera_to_tile(args.port, ax + 1, ay,
                           int(floor_z) if isinstance(floor_z, int) else 26)
        send(args.port, "camera.setZoom(0.25); return 'ok'")
        shot_path = os.path.join(args.out, "1_construction.png")
        if not check(capture(args.port, shot_path), "captured the scene"):
            return 1
        shot = load_rgb(shot_path)

        blues = dominant(shot, 2)
        base_blues = dominant(base, 2)
        check(len(blues) > 200 and len(blues) > len(base_blues) + 200,
              "a frame pixel OUTSIDE the reused facemap's silhouette is "
              "visible",
              f"{len(blues)} blue-dominant px (baseline {len(base_blues)})")

        cyans = cyanish(shot)
        base_cyans = cyanish(base)
        check(len(cyans) <= len(base_cyans) + 20,
              "the UNFLAGGED static piece's out-of-silhouette pixels are "
              "still clipped",
              f"{len(cyans)} cyan px (baseline {len(base_cyans)})")

        whites = whitish(shot)
        base_whites = whitish(base)
        check(len(whites) <= len(base_whites) + 20,
              "a fully transparent frame pixel paints nothing, flag or no "
              "flag",
              f"{len(whites)} white px (baseline {len(base_whites)})")

        reds = dominant(shot, 0)
        greens = dominant(shot, 1)
        if check(bool(reds) and bool(greens) and bool(blues),
                 "all three lit bands reached the frame buffer",
                 f"red={len(reds)} green={len(greens)} blue={len(blues)}"):
            peak = (max(reds), max(greens), max(blues))
            check(max(peak) - min(peak) <= 6,
                  "a painted top mask, a zero-RGB mask and NO mask all light "
                  "the frame identically",
                  f"peaks red/green/blue = {peak}")
    finally:
        if not args.keep_open:
            quit_engine(args.port, proc)
        shutil.rmtree(FIXTURE_DIR, ignore_errors=True)

    print()
    print(f"  {os.path.join(args.out, '1_construction.png')}")
    print()
    if failures:
        print(f"structure_construction_probe: {failures} failure(s)")
        return 1
    print("structure_construction_probe: all checks passed.")
    return 0


if __name__ == "__main__":
    sys.exit(main())
