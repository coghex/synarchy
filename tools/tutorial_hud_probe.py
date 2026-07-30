#!/usr/bin/env python3
"""Offscreen visual probe for the tutorial checklist HUD (#960).

The GPU-backed complement to the pure "Tutorial HUD" hspec group
(test-headless/Test/Headless/UI/TutorialHud.hs): the headless gate proves
the model, geometry and input policies, this one proves the surface is
actually DRAWN over a real world and that real mouse input behaves the
way those policies promise.

Boots `--offscreen` (GPU on, window off), walks the real menu flow to an
in-game HUD, then:

  1. Collapsed at entry: the panel is closed and only its toggle exists.
  2. Open/close by REAL clicks on the toggle, located through the
     module's own requirement-7 dump — never a hardcoded coordinate.
  3. Transparent overlay: opening the list changes only a small
     fraction of the pixels inside the list's own rect, and that rect
     still shows many distinct colors afterwards. An opaque background
     panel would flip both.
  4. Long-list scrolling: a 40-objective tree is injected through
     #958's own `tutorialProgress.setTree`, the list overflows its
     viewport, and a real wheel event over a row scrolls it (and
     visibly repaints).
  5. Terrain input outside controls: with the list open, no modal
     boundary exists, its rows block no pointers and register no
     callbacks, and a real click landing ON a row is routed by the
     engine down the GAMEPLAY chain — the same handler a click on bare
     terrain gets — while a click on the toggle is routed to this
     module's own callback. The oracle is the engine's own F4
     action-outcome record (`debug.drainActionOutcomes`), which reports
     the route it actually took.

Needs a GPU (Vulkan device) — manual-only, never CI-gated, same as
tools/offscreen_probe.py.

Usage:
  python3 tools/tutorial_hud_probe.py
  python3 tools/tutorial_hud_probe.py --port 9421 --size 1280x720
"""
from __future__ import annotations

import argparse
import os
import sys
import tempfile
import time

from probelib import boot, poll_until, quit_engine, send, send_json
from offscreen_probe import (screenshot, png_stats, png_differs,
                             find_widget, click_widget, center_on)

LOG = "/tmp/tutorial_hud_engine.log"
TOGGLE_CALLBACK = "onTutorialHudToggle"
# A tree big enough to overflow the viewport at any supported size.
PROBE_ROWS = 40

failures: list[str] = []


def check(name: str, ok: bool, detail: str = "") -> bool:
    print(f"  [{'PASS' if ok else 'FAIL'}] {name}"
          + (f" — {detail}" if detail and not ok else ""))
    if not ok:
        failures.append(name if not detail else f"{name} — {detail}")
    return ok


# --------------------------------------------------------------------------
# Module introspection (requirement 7) — the ONLY source of coordinates.
# --------------------------------------------------------------------------
def dump(port: int) -> dict:
    got = send_json(port, "return require('scripts.tutorial_hud').dump()",
                    timeout=10.0)
    return got if isinstance(got, dict) else {}


def poll_dump(port: int, pred, seconds: float = 10.0) -> dict | None:
    """Poll the module's dump until `pred(dump)` holds (one call per tick)."""
    def once():
        d = dump(port)
        return d if d and pred(d) else None
    return poll_until(seconds, once)


def center_of(rect: dict) -> tuple[int, int]:
    return (int(rect["x"] + rect["w"] / 2), int(rect["y"] + rect["h"] / 2))


def click_toggle(port: int, d: dict) -> None:
    x, y = center_of(d["toggle"])
    send(port, f"return input.click({x}, {y})", timeout=10.0)


# --------------------------------------------------------------------------
# Input routing oracle: the engine's own F4 action-outcome record says
# which route a real click actually took.
# --------------------------------------------------------------------------
def drain(port: int) -> list[dict]:
    got = send_json(port, "return debug.drainActionOutcomes()", timeout=10.0)
    return got if isinstance(got, list) else []


def click_and_route(port: int, x: int, y: int) -> dict:
    """Click at (x, y) and return its single action-outcome record."""
    drain(port)
    send(port, f"return input.click({x}, {y})", timeout=10.0)
    recs = poll_until(5.0, lambda: drain(port) or None) or []
    clicks = [r for r in recs if r.get("kind") == "input.click"]
    return clicks[0] if clicks else {}


def list_rect(d: dict) -> dict:
    """The open list's own viewport, from the module's reported bounds."""
    return {"x": int(d["panelX"]), "y": int(d["listTop"]),
            "w": int(d["panelW"]),
            "h": int(max(1, d["listBottom"] - d["listTop"]))}


# --------------------------------------------------------------------------
# Pixel helpers (transparency)
# --------------------------------------------------------------------------
def changed_fraction(path_a: str, path_b: str, rect: dict) -> float:
    """Fraction of pixels inside `rect` that differ between two frames."""
    from PIL import Image, ImageChops
    with Image.open(path_a) as a, Image.open(path_b) as b:
        box = (rect["x"], rect["y"], rect["x"] + rect["w"], rect["y"] + rect["h"])
        ca = a.convert("RGBA").crop(box)
        cb = b.convert("RGBA").crop(box)
        diff = ImageChops.difference(ca, cb)
        changed = sum(diff.convert("L").histogram()[1:])
        total = max(1, ca.width * ca.height)
        return changed / total


def distinct_colors_in(path: str, rect: dict) -> int:
    from PIL import Image
    with Image.open(path) as im:
        box = (rect["x"], rect["y"], rect["x"] + rect["w"], rect["y"] + rect["h"])
        crop = im.convert("RGBA").crop(box)
        colors = crop.getcolors(maxcolors=1 << 20)
        return len(colors) if colors else (1 << 20)


# --------------------------------------------------------------------------
# Phases
# --------------------------------------------------------------------------
def reach_hud(port: int, shots: str) -> bool:
    """Real menu flow: loading screen -> Create World -> Generate -> HUD."""
    if not check("loading screen reaches the main menu",
                 bool(poll_until(90.0, lambda: find_widget(port, "Create World")))):
        return False
    check("click 'Create World'", click_widget(port, "Create World"))
    if not check("create-world screen reached",
                 bool(poll_until(20.0, lambda: find_widget(port, "Generate World")))):
        return False
    check("click 'Generate World'", click_widget(port, "Generate World"))

    def done():
        # getInitProgress pushes FOUR values (phase, current, total,
        # label); bind the first so the console reports just the phase.
        return send(port, "local p = world.getInitProgress(); return p",
                    timeout=5.0).strip() == "3"

    if not check("worldgen completes (phase 3)",
                 bool(poll_until(300.0, done, interval=2.0))):
        return False
    if not check("post-generation 'Continue' appears",
                 bool(poll_until(60.0, lambda: find_widget(port, "Continue")))):
        return False
    check("click 'Continue'", click_widget(port, "Continue"))
    check("create-world screen dismissed (in-game view)",
          bool(poll_until(60.0, lambda: not find_widget(port, "Continue"))))
    # goToTile resets the camera into the zoomed-in band (its zoomSafe
    # branch), which is where terrain click-through is meaningful.
    center_on(port, 0, 0)
    time.sleep(3.0)
    return True


def collapsed_phase(port: int, w: int, h: int, shots: str) -> dict:
    d = dump(port)
    check("the checklist module is live in a real session", bool(d))
    check("panel starts COLLAPSED at session entry", d.get("open") is False)
    check("no rows rendered while collapsed", len(d.get("rows") or []) == 0)
    check("the toggle exists anyway (persistent HUD button)",
          bool(d.get("toggle", {}).get("handle")))
    # This module is loadScript'd long before ui_manager runs hud.init,
    # so its FIRST build has neither font nor box textures. By the time
    # the HUD is up it must have rebuilt itself with both, without
    # needing the tutorial model to change.
    check("the live surface has picked up hud's font + box textures",
          d.get("assetsReady") is True, str(d.get("assetsReady")))
    check("the toggle is drawn, not an invisible hit box",
          bool(d.get("toggle", {}).get("label")), str(d.get("toggle")))
    t = d.get("toggle", {})
    check("toggle is fully in frame",
          bool(t) and t["x"] >= 0 and t["y"] >= 0
          and t["x"] + t["w"] <= w and t["y"] + t["h"] <= h,
          str(t))
    check("toggle is right-anchored (past the screen midpoint)",
          bool(t) and t["x"] > w / 2, str(t))
    return d


def open_phase(port: int, closed_shot: str, shots: str) -> dict:
    d = dump(port)
    click_toggle(port, d)
    opened = poll_dump(port, lambda x: x.get("open") is True)
    check("a real click on the toggle OPENS the list", bool(opened))
    d = dump(port)
    check("open list renders the active objective rows",
          len(d.get("rows") or []) > 0, str(d.get("rowIds")))
    check("the toggle rect did not move when the list opened",
          bool(opened) and d["toggle"] == opened["toggle"],
          f"{opened and opened.get('toggle')} vs {d.get('toggle')}")
    shot_open = os.path.join(shots, "open.png")
    check("open-panel screenshot answers", screenshot(port, shot_open))
    check("the open panel visibly repaints the frame",
          png_differs(closed_shot, shot_open))
    return d


def transparency_phase(port: int, closed_shot: str, d: dict, shots: str) -> None:
    shot_open = os.path.join(shots, "open.png")
    rect = list_rect(d)
    frac = changed_fraction(closed_shot, shot_open, rect)
    # Text glyphs over terrain touch a modest slice of the rect; an
    # opaque background would repaint essentially all of it.
    check("opening the list changes only part of its own rect "
          "(no background over terrain)",
          0.0 < frac < 0.5, f"changed fraction {frac:.3f}")
    colors = distinct_colors_in(shot_open, rect)
    check("terrain is still visible under the open list "
          "(many distinct colors in its rect)",
          colors >= 16, f"distinct colors {colors}")


def inject_wide_tree(port: int) -> None:
    """Replace the session tree through #958's own injection point."""
    send(port,
         "local tp = require('scripts.tutorial_progress'); local subs = {}; "
         f"for i = 1, {PROBE_ROWS} do subs[i] = "
         "{ id = string.format('probe_sub_%03d', i), kind = 'subobjective', "
         "label = 'Probe objective ' .. i, tooltip = 'probe tooltip ' .. i, "
         "evaluator = 'probe_eval', order = i, children = {}, "
         "subobjectives = {} } end; "
         "tp.setTree({ id = 'first_session', root = { id = 'probe_root', "
         "kind = 'composite', label = 'Probe root', tooltip = 'probe root', "
         "evaluator = 'probe_eval', order = 1, children = {}, "
         "subobjectives = subs } }); "
         "require('scripts.tutorial_hud').rebuild(); return 'ok'",
         timeout=15.0)


def scroll_phase(port: int, shots: str) -> dict:
    inject_wide_tree(port)
    d = dump(port)
    check(f"a {PROBE_ROWS + 1}-row tree overflows the viewport",
          (d.get("scrollRange") or 0) > 0,
          f"range {d.get('scrollRange')} capacity {d.get('capacity')}")
    check("the visible window is exactly the viewport's capacity",
          len(d.get("rows") or []) == d.get("capacity"))
    shot_top = os.path.join(shots, "list_top.png")
    check("long-list screenshot answers", screenshot(port, shot_top))

    row = (d.get("rows") or [{}])[0]
    rx, ry = center_of(row)
    send(port, f"return input.moveMouse({rx}, {ry})", timeout=10.0)
    send(port, "return input.scroll(0, -2)", timeout=10.0)
    scrolled = poll_dump(port, lambda x: (x.get("scrollOffset") or 0) > 0)
    check("a real wheel event over a row scrolls the list", bool(scrolled))
    after = dump(port)
    check("scrolling shifted the visible window's first row",
          bool(after.get("rowIds")) and after["rowIds"][0] != d["rowIds"][0],
          f"{d.get('rowIds', [None])[0]} -> {after.get('rowIds', [None])[0]}")
    shot_scrolled = os.path.join(shots, "list_scrolled.png")
    if check("scrolled screenshot answers", screenshot(port, shot_scrolled)):
        check("the scrolled list visibly repaints", png_differs(shot_top, shot_scrolled))
    return after


def passthrough_phase(port: int, d: dict, w: int, h: int) -> None:
    blocked = send(port, "return UI.isInputBlocked()", timeout=10.0).strip()
    check("the open checklist creates no modal boundary", blocked == "false", blocked)

    row = (d.get("rows") or [{}])[0]
    handle = row.get("handle")
    policy = send_json(
        port,
        f"return {{ blocks = UI.isPointerBlocking({handle}), "
        f"captures = UI.isScrollCapturing({handle}), "
        f"onClick = UI.getElementOnClick({handle}) }}",
        timeout=10.0) or {}
    check("rows block no pointers", policy.get("blocks") is False, str(policy))
    check("rows register no activation callback",
          policy.get("onClick") in (None, False, ""), str(policy))
    check("rows do capture the wheel", policy.get("captures") is True, str(policy))

    rx, ry = center_of(row)
    # Control: bare terrain at the SAME height, left of the panel and
    # clear of both left-hand toolbar clusters.
    control = click_and_route(port, w // 4, ry)
    check("a control click on bare terrain routes to the gameplay chain",
          bool(control) and control.get("handler")
          and control.get("handler") != TOGGLE_CALLBACK, str(control))

    over_row = click_and_route(port, rx, ry)
    check("a real click landing ON a checklist row is recorded at that row",
          bool(over_row)
          and abs((over_row.get("where") or {}).get("x", -1) - rx) <= 1
          and abs((over_row.get("where") or {}).get("y", -1) - ry) <= 1,
          str(over_row))
    check("that click takes the SAME gameplay route as bare terrain "
          "(the row consumed nothing)",
          bool(over_row) and over_row.get("handler") == control.get("handler"),
          f"{over_row.get('handler')!r} vs control {control.get('handler')!r}")


def reclose_phase(port: int, closed_shot: str, shots: str) -> None:
    d = dump(port)
    tx, ty = center_of(d["toggle"])
    rec = click_and_route(port, tx, ty)
    # The counterpart to the pass-through check: the toggle IS a real
    # control, so the identical input verb routes to this module.
    check("a click on the toggle routes to the checklist's own callback",
          rec.get("handler") == TOGGLE_CALLBACK and rec.get("outcome") == "accepted",
          str(rec))
    closed = poll_dump(port, lambda x: x.get("open") is False)
    check("a second click on the toggle CLOSES the list again", bool(closed))
    after = dump(port)
    check("no rows remain after closing", len(after.get("rows") or []) == 0)
    shot_reclosed = os.path.join(shots, "reclosed.png")
    if check("re-closed screenshot answers", screenshot(port, shot_reclosed)):
        check("the closed frame differs from the open one",
              png_differs(os.path.join(shots, "list_scrolled.png"), shot_reclosed))


def main() -> int:
    ap = argparse.ArgumentParser(description=__doc__)
    ap.add_argument("--port", type=int, default=9421)
    ap.add_argument("--size", default="1280x720")
    args = ap.parse_args()

    w, h = (int(v) for v in args.size.lower().split("x"))
    shots = tempfile.mkdtemp(prefix="tutorial_hud_probe_")
    print(f"== offscreen boot (port {args.port}, {args.size}) ==")
    print(f"   screenshots: {shots}")
    proc = boot(args.port, mode=("--offscreen",),
                args=["--size", args.size], log=LOG,
                label="offscreen engine")
    try:
        print("== real menu flow -> in-game HUD (takes ~1-2 min) ==")
        if not reach_hud(args.port, shots):
            print("\nFAILED to reach the in-game HUD — aborting")
            return 1

        print("== 1. collapsed at session entry ==")
        collapsed_phase(args.port, w, h, shots)
        shot_closed = os.path.join(shots, "closed.png")
        if check("closed-panel screenshot answers", screenshot(args.port, shot_closed)):
            st = png_stats(shot_closed)
            check(f"closed PNG valid at {w}x{h}",
                  bool(st) and st[0] == w and st[1] == h, str(st))

        print("== 2. open by a real click on the toggle ==")
        opened = open_phase(args.port, shot_closed, shots)

        print("== 3. transparent overlay over terrain ==")
        transparency_phase(args.port, shot_closed, opened, shots)

        print("== 4. long-list scrolling ==")
        scrolled = scroll_phase(args.port, shots)

        print("== 5. terrain input outside the controls ==")
        passthrough_phase(args.port, scrolled, w, h)

        print("== 6. close again ==")
        reclose_phase(args.port, shot_closed, shots)
    finally:
        quit_engine(args.port, proc)

    print()
    if failures:
        print(f"FAILED ({len(failures)}):")
        for f in failures:
            print(f"  - {f}")
        return 1
    print("ALL CHECKS PASSED")
    return 0


if __name__ == "__main__":
    sys.exit(main())
