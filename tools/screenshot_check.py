#!/usr/bin/env python3
"""GUI screenshot-verb check (#643) — HUMAN-RUN, needs a windowed instance.

debug.captureScreenshot copies the swapchain image, so unlike every
*_probe.py it cannot run under the GPU-less --headless mode (where the
verb deliberately errors). This check therefore ATTACHES to an
already-running GRAPHICAL instance instead of booting its own engine —
launch the game normally (its debug console listens on port 8008), then:

  python3 tools/screenshot_check.py            # attach to port 8008
  python3 tools/screenshot_check.py --port 9008

It asserts, against the live instance:
  1. debug.captureScreenshot(path) returns {path, width, height},
  2. the file is a valid PNG whose IHDR dimensions match the reply,
  3. the pixels (unfiltered per the PNG spec, RGB only — the capture
     path forces alpha opaque) aren't one uniform color: an all-black
     or solid-color capture is the classic wrong-layout/wrong-image
     symptom,
  4. an unwritable path comes back as {error=...}, not a crash, and the
     instance still answers afterwards.

Colors/orientation still deserve one human eyeball against the screen
(the pure swizzle/row-order contract is pinned by hspec in
Test.Headless.Graphics.Screenshot).

Exit code 0 = all checks passed.
"""
from __future__ import annotations

import argparse
import os
import struct
import sys
import tempfile
import time
import zlib

from probelib import send_json


def read_png_ihdr(path: str) -> tuple[int, int]:
    """Return (width, height) from the PNG's IHDR, validating the magic."""
    with open(path, "rb") as f:
        magic = f.read(8)
        if magic != b"\x89PNG\r\n\x1a\n":
            raise AssertionError(f"{path}: not a PNG (bad magic {magic!r})")
        length = struct.unpack(">I", f.read(4))[0]
        ctype = f.read(4)
        if ctype != b"IHDR" or length < 13:
            raise AssertionError(f"{path}: first chunk is {ctype!r}, not IHDR")
        w, h = struct.unpack(">II", f.read(8))
        return w, h


def png_is_uniform_color(path: str) -> bool:
    """True if every pixel has the same RGB value (alpha ignored — the
    capture path forces it to 255, so an opaque all-black frame must
    still be caught). Unfilters the scanlines per the PNG spec; the
    engine's encoder always writes 8-bit RGBA (color type 6), which is
    asserted rather than assumed."""
    idat = b""
    w = h = bit_depth = color_type = None
    with open(path, "rb") as f:
        f.read(8)
        while True:
            hdr = f.read(8)
            if len(hdr) < 8:
                break
            length, ctype = struct.unpack(">I", hdr[:4])[0], hdr[4:]
            data = f.read(length)
            f.read(4)  # crc
            if ctype == b"IHDR":
                w, h, bit_depth, color_type = struct.unpack(">IIBB", data[:10])
            elif ctype == b"IDAT":
                idat += data
            elif ctype == b"IEND":
                break
    if (bit_depth, color_type) != (8, 6):
        raise AssertionError(
            f"{path}: expected 8-bit RGBA (bit depth 8, color type 6), "
            f"got bit depth {bit_depth}, color type {color_type}")

    raw = zlib.decompress(idat)
    bpp = 4
    stride = w * bpp
    prior = bytearray(stride)
    first_rgb = None
    for y in range(h):
        base = y * (stride + 1)
        filt = raw[base]
        line = bytearray(raw[base + 1:base + 1 + stride])
        if filt == 1:    # Sub
            for x in range(bpp, stride):
                line[x] = (line[x] + line[x - bpp]) & 0xFF
        elif filt == 2:  # Up
            for x in range(stride):
                line[x] = (line[x] + prior[x]) & 0xFF
        elif filt == 3:  # Average
            for x in range(stride):
                left = line[x - bpp] if x >= bpp else 0
                line[x] = (line[x] + (left + prior[x]) // 2) & 0xFF
        elif filt == 4:  # Paeth
            for x in range(stride):
                a = line[x - bpp] if x >= bpp else 0
                b = prior[x]
                c = prior[x - bpp] if x >= bpp else 0
                p = a + b - c
                pa, pb, pc = abs(p - a), abs(p - b), abs(p - c)
                if pa <= pb and pa <= pc:
                    pr = a
                elif pb <= pc:
                    pr = b
                else:
                    pr = c
                line[x] = (line[x] + pr) & 0xFF
        elif filt != 0:
            raise AssertionError(f"{path}: unknown PNG filter {filt}")
        for x in range(0, stride, bpp):
            rgb = (line[x], line[x + 1], line[x + 2])
            if first_rgb is None:
                first_rgb = rgb
            elif rgb != first_rgb:
                return False  # second color found — not uniform
        prior = line
    return True


def main() -> int:
    ap = argparse.ArgumentParser(description=__doc__)
    ap.add_argument("--port", type=int, default=8008,
                    help="debug-console port of the RUNNING graphical "
                         "instance (default 8008)")
    args = ap.parse_args()
    port = args.port

    failures = []

    def check(name: str, ok: bool, detail: str = "") -> None:
        print(f"  [{'ok' if ok else 'FAIL'}] {name}" + (f" — {detail}" if detail else ""))
        if not ok:
            failures.append(name)

    out_path = os.path.join(
        tempfile.gettempdir(), f"screenshot_check_{int(time.time())}.png")

    print(f"screenshot_check: attaching to port {port}")
    reply = send_json(port, f'return debug.captureScreenshot("{out_path}")',
                      timeout=15.0)
    if not isinstance(reply, dict):
        print(f"  [FAIL] no JSON reply from debug.captureScreenshot: {reply!r}")
        print("  (is a GRAPHICAL instance running on this port? headless "
              "instances refuse this verb by design)")
        return 1
    if "error" in reply:
        print(f"  [FAIL] capture refused: {reply['error']}")
        return 1

    check("reply has path/width/height",
          all(k in reply for k in ("path", "width", "height")), str(reply))
    check("file exists", os.path.isfile(out_path), out_path)

    w, h = read_png_ihdr(out_path)
    check("PNG magic + IHDR parse", True, f"{w}x{h}")
    check("IHDR dims match reply",
          (w, h) == (reply.get("width"), reply.get("height")),
          f"png={w}x{h} reply={reply.get('width')}x{reply.get('height')}")
    check("pixels not one uniform color", not png_is_uniform_color(out_path))

    bad = send_json(
        port,
        'return debug.captureScreenshot("/nonexistent-dir-643/x.png")',
        timeout=15.0)
    check("unwritable path returns error table",
          isinstance(bad, dict) and "error" in bad, str(bad))

    alive = send_json(port, "return 1 + 1", timeout=5.0)
    check("instance still responsive afterwards", alive == 2, str(alive))

    print(f"\ncapture written to {out_path} — eyeball it against the screen "
          "(colors, orientation)")
    if failures:
        print(f"screenshot_check: FAILED ({len(failures)}): {', '.join(failures)}")
        return 1
    print("screenshot_check: all checks passed")
    return 0


if __name__ == "__main__":
    sys.exit(main())
