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

    quit_engine(args.port, proc)

    print(f"\nscreenshots kept in {shots}")
    if failures:
        print(f"offscreen_probe: {failures} check(s) FAILED")
        return 1
    print("offscreen_probe: all checks passed")
    return 0


if __name__ == "__main__":
    sys.exit(main())
