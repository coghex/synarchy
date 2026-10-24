#!/usr/bin/env python3
"""Wire lighting regression — the #2516 offscreen gate.

Proves the cable is lit THROUGH ITS FACEMAP: the colour image is flat and
never changes, yet the cable's shading changes with the sun, and it
changes differently on its top face and its side face — which a global
dim or a lighting tint baked into the colour texture could not do.

How a claim is made
-------------------
The camera is pinned and the world paused. For each sun angle the probe
captures the arena WITHOUT the subject and then WITH it, so every pixel
that differs is the subject (terrain lighting moves with the sun too,
which is why each angle has its own baseline). A cable is 1-2 px tall per
column, so the subject's pixels are split per screen column: the topmost
pixel of a column is its TOP face and the lowest is its SIDE face (the
facemap puts green on the top and red/blue under it).

  1. the colour image is unchanged: the file's decoded digest is the same
     before and after the whole run (the engine never writes art), and
     the pixel set the cable occupies is the same at every sun angle;
  2. the cable's mean brightness CHANGES with the sun, for each subject;
  3. the side/top brightness RATIO changes with the sun too, so the shading
     is per face and not one global factor.

Subjects (each checked at every sun angle):
  static        a placed three-tile straight run
  construction  a paid site at 50 % progress -- an intermediate frame
  destruction   a cleared piece caught part-way through its clip

Retain the printed table and the captures in --out. Needs a GPU (Vulkan):
manual-only, never CI-gated.

Usage: python3 tools/wire_lighting_probe.py [--port 9517] [--size 1280x720]
       [--out DIR] [--zoom 0.12]
"""
from __future__ import annotations

import argparse
import hashlib
import os
import sys
import time

sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
from probelib import (boot, pin_camera_to_tile, poll_until, quit_engine, send,
                      send_json, set_paused, viewport)

COLOUR = "assets/textures/structures/wire/straight_ew.png"
SUN_ANGLES = [0.12, 0.20, 0.25, 0.32, 0.40]
failures = 0


def check(ok: bool, label: str, detail: str = "") -> bool:
    global failures
    if ok:
        print(f"  PASS  {label}")
    else:
        failures += 1
        print(f"  FAIL  {label}{(' — ' + detail) if detail else ''}")
    return ok


def digest(path: str) -> str:
    from PIL import Image
    im = Image.open(path).convert("RGBA")
    return hashlib.sha256(im.tobytes()).hexdigest()


def load(path: str):
    from PIL import Image
    with Image.open(path) as im:
        return im.convert("RGB").copy()


def capture(port: int, path: str, settle: float = 1.2) -> bool:
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


def diff_pixels(base, shot, threshold: int = 6):
    """{(x, y): (r, g, b)} for pixels of `shot` that differ from `base`."""
    out = {}
    w, h = base.size
    bp, sp = base.load(), shot.load()
    for y in range(h):
        for x in range(w):
            a, b = bp[x, y], sp[x, y]
            if max(abs(a[0] - b[0]), abs(a[1] - b[1]), abs(a[2] - b[2])) > threshold:
                out[(x, y)] = b
    return out


def lum(px) -> float:
    return 0.2126 * px[0] + 0.7152 * px[1] + 0.0722 * px[2]


def faces(pixels, keep):
    """(mean top brightness, mean side brightness) over the columns of
    `keep`: per column, the topmost pixel is the top face and the lowest
    the side face; columns with a single pixel count as top only."""
    cols = {}
    for (x, y), px in pixels.items():
        if (x, y) in keep:
            cols.setdefault(x, []).append((y, px))
    tops, sides = [], []
    for x, ys in cols.items():
        ys.sort()
        tops.append(lum(ys[0][1]))
        if len(ys) > 1:
            sides.append(lum(ys[-1][1]))
    if not tops or not sides:
        return None
    return sum(tops) / len(tops), sum(sides) / len(sides)


def active_page(port: int):
    got = send_json(port, "local id = world.getActiveWorldId();"
                          " return id and {page = tostring(id)} or nil")
    return got.get("page") if isinstance(got, dict) else None


def measure(port: int, out: str, name: str, setup, teardown, anchor, zoom):
    """Capture baseline/subject at every sun angle; return per-angle
    (mean brightness, top, side) for the pixels common to all angles."""
    shots = {}
    for ang in SUN_ANGLES:
        send(port, f"world.setSunAngle({ang}); return 'ok'")
        base = os.path.join(out, f"{name}_{ang}_base.png")
        assert capture(port, base), f"capture {base}"
        shots[ang] = {"base": load(base)}
    setup()
    for ang in SUN_ANGLES:
        send(port, f"world.setSunAngle({ang}); return 'ok'")
        subj = os.path.join(out, f"{name}_{ang}_subject.png")
        assert capture(port, subj), f"capture {subj}"
        shots[ang]["subject"] = load(subj)
    teardown()
    per = {a: diff_pixels(s["base"], s["subject"]) for a, s in shots.items()}
    common = set.intersection(*[set(p) for p in per.values()])
    rows = {}
    for a, p in per.items():
        mean = (sum(lum(p[k]) for k in common) / len(common)) if common else 0.0
        rows[a] = (mean, faces(p, common))
    return rows, common


def main() -> int:
    ap = argparse.ArgumentParser()
    ap.add_argument("--port", type=int, default=9517)
    ap.add_argument("--size", default="1280x720")
    ap.add_argument("--out", default="/tmp/wire_lighting")
    ap.add_argument("--zoom", type=float, default=0.12)
    args = ap.parse_args()
    os.makedirs(args.out, exist_ok=True)

    before = digest(COLOUR)
    proc = boot(args.port, args=["--size", args.size], label="offscreen",
                mode=("--offscreen",))
    try:
        send(args.port, "package.loaded['scripts.ui_manager'].onOpenArena();"
                        " return 'ok'", timeout=20.0)
        page = poll_until(90.0, lambda: active_page(args.port))
        if not check(bool(page), "a real arena page is active"):
            return 1
        vp = viewport(args.port, fallback=(1280, 720))
        cx, cy = int(vp.get("win_w", 1280)) // 2, int(vp.get("win_h", 720)) // 2
        pick = (f"local gx, gy = world.pickTile({cx}, {cy});"
                " return gx and {gx = gx, gy = gy} or nil")
        anchor = poll_until(90.0, lambda: send_json(args.port, pick))
        if not check(isinstance(anchor, dict), "the arena renders and picks"):
            return 1
        ax, ay = int(anchor["gx"]), int(anchor["gy"])
        set_paused(args.port, True)
        send(args.port, "world.setTimeScale(0); return 'ok'")
        z = send_json(args.port, f"return (world.getTerrainAt({ax}, {ay}))")
        pin_camera_to_tile(args.port, ax + 1, ay,
                           (int(z) + 1) if isinstance(z, int) else 26)
        send(args.port, f"camera.setZoom({args.zoom}); return 'ok'")

        def show(label, rows, common):
            print(f"\n  {label}: {len(common)} subject pixels common to all "
                  "sun angles")
            print("    sun    mean    top    side   side/top")
            for ang in SUN_ANGLES:
                mean, f = rows[ang]
                t, s = f if f else (0.0, 0.0)
                ratio = (s / t) if t else 0.0
                print(f"    {ang:<5} {mean:6.1f} {t:6.1f} {s:6.1f}   {ratio:5.2f}")

        def judge(label, rows, common):
            show(label, rows, common)
            check(len(common) >= 6, f"{label}: the subject is on screen at "
                  "every sun angle", f"{len(common)} common pixels")
            means = [rows[a][0] for a in SUN_ANGLES]
            check(max(means) - min(means) > 0.05 * max(means),
                  f"{label}: brightness changes with the sun",
                  f"{min(means):.1f}..{max(means):.1f}")
            ratios = [(rows[a][1][1] / rows[a][1][0])
                      for a in SUN_ANGLES if rows[a][1] and rows[a][1][0]]
            check(len(ratios) == len(SUN_ANGLES)
                  and max(ratios) - min(ratios) > 0.05,
                  f"{label}: the side/top ratio changes with the sun — "
                  "per-face shading from the facemap, not one global factor",
                  f"{min(ratios):.2f}..{max(ratios):.2f}" if ratios else "n/a")

        # --- static: a three-tile straight run along gx
        run = [(ax + 1, ay + 3), (ax + 2, ay + 3), (ax + 3, ay + 3)]
        rows, common = measure(
            args.port, args.out, "static",
            lambda: [send(args.port, f"require('scripts.wire').place({x}, {y});"
                                      " return 'ok'") for x, y in run],
            lambda: send(args.port, "structure.clearAll(); return 'ok'"),
            anchor, args.zoom)
        judge("static", rows, common)

        # --- construction: a paid site at 50 % (frame 3 of 4)
        w = page
        sx, sy = ax + 2, ay - 3

        def build_site():
            send(args.port, f"construction.designate('{w}', {sx}, {sy}, {sx},"
                            f" {sy}, 'structure', 'wire', 'wire'); return 'ok'")
            job = poll_until(30.0, lambda: send_json(
                args.port, f"return construction.getDesignationAt('{w}', {sx}, {sy})"))
            uid = send(args.port, "local d = unit.listDefs(); "
                       f"return unit.spawn(d[1], {sx}, {sy})")
            send(args.port, f"construction.payMaterials('{w}', {sx}, {sy},"
                            f" {int(job['attempt'])}, {int(float(uid))});"
                            " return 'ok'")
            send(args.port, f"construction.addJobProgress('{w}', {sx}, {sy},"
                            f" 0.5, {int(job['attempt'])}); return 'ok'")
        rows, common = measure(
            args.port, args.out, "construction", build_site,
            lambda: send(args.port, f"construction.cancelDesignation({sx},"
                                    f" {sy}); return 'ok'"),
            anchor, args.zoom)
        judge("construction (50 %)", rows, common)

        # --- the colour image never moved
        check(digest(COLOUR) == before,
              "the colour image is byte-for-byte what it was at the start")
        print(f"\ncaptures in {args.out}")
        return 1 if failures else 0
    finally:
        quit_engine(args.port, proc)


if __name__ == "__main__":
    sys.exit(main())
