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
  6. (#996/#1941/#2056) A branch that latches — and whose subobjectives
     check — before it is ever revealed still renders, in authored
     order with its normal completed marker, instead of an empty
     checklist. Same `setTree` injection point as check 4, shaped like
     the real first_session tree so a composite branch exists to latch
     early. Since #2056 this check also carries the arc's only PIXEL
     proof of presentation: the sticky composite's own ink is
     attributed in a real frame — its text element is hidden and
     re-shown and the columns that change ARE its marker and label, the
     #1581 oracle's technique — before the branch retires and the panel
     reaches its empty completed state. The captures and the boundary
     crossing happen inside one Lua chunk, so no update tick can
     interleave and the measured frames provably precede the empty
     state. `tools/tutorial_probe.py` owns the GPU-less half (nothing
     retires while no frame is drawn) and cannot own this one.
  7. (#1419) The toggle caption's RENDERED GLYPH PIXELS fall inside the
     toggle box and inside the framebuffer, measured separately for the
     collapsed "> Objectives" and the open "v Objectives". Nothing in
     the module is trusted for the measurement — the caption element is
     hidden and re-shown, and the pixels that change ARE the glyphs.
  8. (#1581) The same rendered-glyph measurement applied to the
     OBJECTIVE ROWS, over the shipped `data/tutorials/first_session.yaml`
     tree restored from the live engine registry — so the strings
     measured are the authored ones ("Place portal", "Secure water
     source", "Prepare an expedition", "Prepare water", "Prepare food")
     rather than a synthetic stand-in. Every one of the five shipped
     rows is measured, covering all four depths the tree uses, and each
     row's ink must fall inside both the checklist panel and the
     framebuffer. This oracle found the shipped tree overrunning the
     panel on every row below the root, which is why
     scripts/tutorial_hud.lua now fits a row's string to the budget its
     indent leaves; the assertions stay because that fit is the ONLY
     thing bounding a row (rows are still never clipped by the renderer,
     and `fitToggle` covers the caption alone). Since #1941 the five
     rows are reached in TWO stable captures rather than one transient
     sticky one — see `restore_shipped_tree`.
  9. (#1941) A finished checklist stays finished across a REAL save and
     load in this same GPU session: the emptied panel comes back
     collapsed, and reopening it does not repopulate with the ancestors
     the player already watched retire, across the evaluation tick that
     re-checks both subobjectives against the loaded world.

Needs a GPU (Vulkan device) — manual-only, never CI-gated, same as
tools/offscreen_probe.py.

The row bound proves containment for the LAYOUT ACTUALLY EXERCISED. A
run at one size says nothing about a different one: `panelW` is capped
by the framebuffer (scripts/tutorial_hud.lua) while the row font grows
with UI scale, and `_differing_columns` can only see pixels the
framebuffer kept, so ink discarded outside it is unobservable here. Run
the sizes you care about.

Usage:
  python3 tools/tutorial_hud_probe.py
  python3 tools/tutorial_hud_probe.py --port 9421 --size 1280x720
"""
from __future__ import annotations

import argparse
import os
import shutil
import sys
import tempfile
import time

from probelib import (boot, capture_request_id, poll_until, quit_engine,
                      send, send_json, wait_load_published,
                      wait_save_complete)
from offscreen_probe import (screenshot, png_stats, png_differs,
                             find_widget, click_widget, center_on)

LOG = "/tmp/tutorial_hud_engine.log"
#: This probe's own save slot, for the #1941 reload check.
SLOT = "tutorial_hud_probe_slot"
TOGGLE_CALLBACK = "onTutorialHudToggle"
# A tree big enough to overflow the viewport at any supported size.
PROBE_ROWS = 40

# data/tutorials/first_session.yaml, in its authored pre-order: the
# shipped ids, their authored labels, and the depth each sits at. This
# is the EXPECTATION checked against what the engine registry hands
# back, never a substitute for it -- the strings the oracle measures
# always arrive through `engine.getTutorialTree()`.
SHIPPED_ROWS = [
    ("first_session_place_portal",       ("Place portal",         0)),
    ("first_session_secure_water",       ("Secure water source",  1)),
    ("first_session_prepare_expedition", ("Prepare an expedition", 2)),
    ("first_session_prepare_water",      ("Prepare water",        3)),
    ("first_session_prepare_food",       ("Prepare food",         3)),
]

failures: list[str] = []


def remove_probe_slot() -> None:
    """Delete only this probe's own save slot, under the repo's saves/."""
    saves = os.path.join(
        os.path.dirname(os.path.dirname(os.path.abspath(__file__))), "saves")
    target = os.path.join(saves, SLOT)
    if (os.path.basename(target) == SLOT
            and os.path.dirname(target) == saves
            and os.path.isdir(target)
            and not os.path.islink(target)):
        shutil.rmtree(target)


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


# --------------------------------------------------------------------------
# Rendered-caption oracle (#1419)
#
# An advance-width query would NOT do: engine.getTextWidth sums glyph
# advances (Engine.Graphics.Font.Util.calculateTextWidthScaled) while the
# renderer places quads by bearing and bitmap size
# (Engine.Graphics.Font.Draw.layoutTextUI), so only the frame itself says
# where the ink landed. The caption element is hidden and re-shown around
# a capture, and the columns that change are the glyphs — which isolates
# them from the 9-slice box behind them without assuming anything about
# either one's color.
# --------------------------------------------------------------------------
def _differing_columns(path_a: str, path_b: str, rect: dict) -> set[int]:
    """Absolute x columns inside `rect` that differ between two frames."""
    from PIL import Image, ImageChops
    box = (rect["x"], rect["y"], rect["x"] + rect["w"], rect["y"] + rect["h"])
    with Image.open(path_a) as a, Image.open(path_b) as b:
        diff = ImageChops.difference(a.convert("RGBA").crop(box),
                                     b.convert("RGBA").crop(box)).convert("L")
        cols = set()
        px = diff.load()
        for x in range(diff.width):
            for y in range(diff.height):
                if px[x, y] != 0:
                    cols.add(rect["x"] + x)
                    break
    return cols


def _caption_band(t: dict, w: int, h: int) -> dict:
    """The toggle's own row of the frame, extended to the RIGHT EDGE so
    ink painting past the box is measured rather than cropped away."""
    pad = 24
    x0 = max(0, int(t["x"]) - pad)
    y0 = max(0, int(t["y"]) - pad)
    y1 = min(h, int(t["y"]) + int(t["h"]) + pad)
    return {"x": x0, "y": y0, "w": max(1, w - x0), "h": max(1, y1 - y0)}


def caption_bounds_phase(port: int, w: int, h: int, shots: str,
                         tag: str) -> None:
    """Measure the live caption's glyph columns and bound them."""
    for attempt in range(3):
        before = dump(port)
        t = before.get("toggle") or {}
        label = t.get("label")
        if not check(f"[{tag}] the caption element exists to measure",
                     bool(label), str(t)):
            return
        band = _caption_band(t, w, h)
        with_a = os.path.join(shots, f"caption_{tag}_with_{attempt}.png")
        without = os.path.join(shots, f"caption_{tag}_without_{attempt}.png")
        with_b = os.path.join(shots, f"caption_{tag}_again_{attempt}.png")
        shot_a = screenshot(port, with_a)
        send(port, f"UI.setVisible({label}, false); return 'ok'", timeout=10.0)
        time.sleep(0.4)
        shot_b = screenshot(port, without)
        # Re-shown before anything can return, so a failed capture or a
        # drifting frame never leaves the surface without its caption.
        send(port, f"UI.setVisible({label}, true); return 'ok'", timeout=10.0)
        time.sleep(0.4)
        shot_c = screenshot(port, with_b)
        if not (shot_a and shot_b and shot_c):
            if attempt < 2:
                continue
            check(f"[{tag}] caption screenshots answer", False,
                  f"with={shot_a} without={shot_b} again={shot_c}")
            return
        after = dump(port)
        # A rebuild mid-measurement would have replaced the very element
        # being hidden, so the three frames would not be comparable.
        rebuilt = (after.get("rebuildCount") != before.get("rebuildCount")
                   or (after.get("toggle") or {}).get("label") != label)
        # Anything OTHER than the caption that moved between the first
        # and last capture is background drift; retry rather than fold
        # it into the measurement.
        drift = _differing_columns(with_a, with_b, band)
        if rebuilt or drift:
            if attempt < 2:
                print(f"       [{tag}] retrying: rebuilt={rebuilt}, "
                      f"{len(drift)} column(s) drifted between the two "
                      "captioned frames")
                continue
            check(f"[{tag}] the frame held still long enough to measure "
                  "the caption",
                  False, f"rebuilt={rebuilt} drifting columns={len(drift)}")
            return
        cols = _differing_columns(with_a, without, band)
        tx, tw = int(t["x"]), int(t["w"])
        if not check(f"[{tag}] the caption renders at all "
                     "(a non-empty glyph set)", bool(cols),
                     f"no pixels changed when the caption was hidden, "
                     f"band {band}"):
            return
        lo, hi = min(cols), max(cols)
        print(f"       [{tag}] caption {t.get('caption')!r} glyph columns "
              f"x={lo} -> x={hi}; toggle box x={tx} w={tw} "
              f"(exclusive right edge {tx + tw}); framebuffer width {w}")
        # Half-open, exactly like the in-frame checks above: the box owns
        # columns [t.x, t.x + t.w) and the frame owns [0, w).
        check(f"[{tag}] every caption glyph column starts at or after the "
              "toggle box's left edge", lo >= tx,
              f"leftmost glyph x={lo} < box x={tx}")
        check(f"[{tag}] every caption glyph column falls inside the "
              "toggle box", hi < tx + tw,
              f"rightmost glyph x={hi} >= box right edge {tx + tw}")
        check(f"[{tag}] every caption glyph column falls inside the "
              "framebuffer", hi < w, f"rightmost glyph x={hi} >= width {w}")
        return


# --------------------------------------------------------------------------
# Rendered-row oracle (#1581)
#
# The caption oracle above bounds the toggle caption and nothing else --
# #1419 declared row rendering out of scope, so this is the half it left.
# Objective rows are placed by scripts/tutorial_hud.lua with no width
# budget, no truncation, no wrapping and no clip, anchored at
# `panelX + indent` inside a RIGHT-ANCHORED panel, so a row wider than
# its remaining budget paints toward and past the right framebuffer edge
# with nothing in the module to stop it.
#
# Measurement is the caption's technique, per row: hide that one row's
# text element, capture, re-show, and the columns that changed ARE its
# glyphs. An advance-width query would not do -- engine.getTextWidth
# sums advances while the renderer places quads by bearing and bitmap
# size, so only the frame says where the ink landed.
# --------------------------------------------------------------------------
def _row_band(row: dict, w: int, h: int) -> dict:
    """The row's own slice of the frame, at FULL framebuffer width so ink
    painting past either edge of the panel is measured rather than
    cropped away, and padded vertically by a whole row so an ascender or
    descender leaving the row rect is still seen. Only this row's text is
    hidden for the capture, so a neighbour's glyphs inside the padding
    cannot be mistaken for this row's."""
    pad = max(1, int(row.get("h") or 1))
    y0 = max(0, int(row["y"]) - pad)
    y1 = min(h, int(row["y"]) + int(row["h"]) + pad)
    return {"x": 0, "y": y0, "w": max(1, w), "h": max(1, y1 - y0)}


def _row_label(row: dict) -> str:
    """How a failing row is NAMED: its shipped id and authored label."""
    return f"row {row.get('id')!r} ({row.get('label')!r})"


def measure_row_bounds(port: int, w: int, h: int, shots: str, tag: str,
                       index: int) -> None:
    """Bound one rendered row's glyph columns, by its position in the
    live dump. Re-reads the dump on every attempt so a rebuild between
    attempts is picked up rather than measured against a dead handle."""
    for attempt in range(3):
        before = dump(port)
        rows = before.get("rows") or []
        if index >= len(rows):
            check(f"[{tag}] row {index} is still rendered to measure", False,
                  f"only {len(rows)} row(s) in the dump")
            return
        row = rows[index]
        handle = row.get("textHandle")
        # A row with no live text element has no ink to bound, and the
        # module would have to be TRUSTED to say so -- fail instead.
        if not check(f"[{tag}] {_row_label(row)} has a live text element "
                     "to measure", bool(handle), str(row)):
            return
        band = _row_band(row, w, h)
        stem = f"row_{tag}_{index}_{attempt}"
        with_a = os.path.join(shots, f"{stem}_with.png")
        without = os.path.join(shots, f"{stem}_without.png")
        with_b = os.path.join(shots, f"{stem}_again.png")
        shot_a = screenshot(port, with_a)
        send(port, f"UI.setVisible({handle}, false); return 'ok'", timeout=10.0)
        time.sleep(0.4)
        shot_b = screenshot(port, without)
        # Re-shown before anything can return, so a failed capture or a
        # drifting frame never leaves the list missing a row.
        send(port, f"UI.setVisible({handle}, true); return 'ok'", timeout=10.0)
        time.sleep(0.4)
        shot_c = screenshot(port, with_b)
        if not (shot_a and shot_b and shot_c):
            if attempt < 2:
                continue
            check(f"[{tag}] {_row_label(row)} screenshots answer", False,
                  f"with={shot_a} without={shot_b} again={shot_c}")
            return
        after = dump(port)
        after_rows = after.get("rows") or []
        # A rebuild mid-measurement would have replaced the very element
        # being hidden, so the three frames would not be comparable.
        rebuilt = (after.get("rebuildCount") != before.get("rebuildCount")
                   or index >= len(after_rows)
                   or after_rows[index].get("textHandle") != handle
                   or after_rows[index].get("id") != row.get("id"))
        # Anything OTHER than this row that moved between the first and
        # last capture is background drift; retry rather than fold it in.
        drift = _differing_columns(with_a, with_b, band)
        if rebuilt or drift:
            if attempt < 2:
                print(f"       [{tag}] retrying {_row_label(row)}: "
                      f"rebuilt={rebuilt}, {len(drift)} column(s) drifted "
                      "between the two rendered frames")
                continue
            check(f"[{tag}] the frame held still long enough to measure "
                  f"{_row_label(row)}", False,
                  f"rebuilt={rebuilt} drifting columns={len(drift)}")
            return
        cols = _differing_columns(with_a, without, band)
        px, pw = int(before["panelX"]), int(before["panelW"])
        if not check(f"[{tag}] {_row_label(row)} renders at all "
                     "(a non-empty glyph set)", bool(cols),
                     f"no pixels changed when its text was hidden, "
                     f"band {band}"):
            return
        lo, hi = min(cols), max(cols)
        print(f"       [{tag}] depth {row.get('depth')} {_row_label(row)} "
              f"glyph columns x={lo} -> x={hi}; panel x={px} w={pw} "
              f"(exclusive right edge {px + pw}); framebuffer width {w}")
        # Half-open, exactly like the caption checks: the panel owns
        # columns [panelX, panelX + panelW) and the frame owns [0, w).
        check(f"[{tag}] every glyph column of {_row_label(row)} starts at "
              "or after the checklist panel's left edge", lo >= px,
              f"leftmost glyph x={lo} < panel x={px}")
        check(f"[{tag}] every glyph column of {_row_label(row)} falls "
              "inside the checklist panel", hi < px + pw,
              f"rightmost glyph x={hi} >= panel right edge {px + pw}")
        check(f"[{tag}] every glyph column of {_row_label(row)} falls "
              "inside the framebuffer", hi < w,
              f"rightmost glyph x={hi} >= width {w}")
        return


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


def inject_shipped_shape(port: int) -> None:
    """Replace the session tree with the shipped first_session SHAPE
    (place_portal -> secure_water -> prepare_expedition{water, food}),
    through #958's own injection point -- same technique as
    inject_wide_tree, but shaped like the real tree instead of a flat
    list, so a composite branch exists to latch early."""
    send(port,
         "local tp = require('scripts.tutorial_progress'); "
         "local function node(id, kind, order, children, subs) "
         "return { id = id, kind = kind, label = id .. ' label', "
         "tooltip = id .. ' tooltip', evaluator = id .. '_eval', "
         "order = order, children = children or {}, "
         "subobjectives = subs or {} } end; "
         "local water = node('prepare_water', 'subobjective', 1); "
         "local food  = node('prepare_food', 'subobjective', 2); "
         "local exp   = node('prepare_expedition', 'composite', 1, {}, "
         "{water, food}); "
         "local sec   = node('secure_water', 'full', 1, {exp}); "
         "local root  = node('place_portal', 'full', 1, {sec}); "
         "tp.reset(); tp.setTree({ id = 'first_session', root = root }); "
         "return 'ok'", timeout=15.0)


def open_and_capture_build(port: int, with_path: str, without_path: str,
                           again_path: str, warm_path: str) -> dict:
    """Open the panel and, WITHOUT ever yielding the Lua thread, capture
    the sticky rows in a real rendered frame.

    ONE console chunk, for two independent reasons:

      * scripts/tutorial_hud.lua's update tick runs on this same thread,
        so while this chunk is running nothing can acknowledge or retire
        anything. The frames captured below are therefore guaranteed to
        predate the empty completed state, which is exactly what
        requirement 5 asks to be shown.
      * the rows and the frames have to be the SAME observation. A
        second round-trip would report a state the captures might not
        share.

    The captures are ordered by #2056's own boundary rather than by
    hope. `debug.captureScreenshot` blocks until a frame's fence
    signals, but the request is dequeued AFTER that frame's UI snapshot
    was taken, so the very first capture after a UI mutation can still
    show the frame before it. The loop therefore captures throwaway
    frames until `UI.isPresented` says a completed snapshot really held
    the current viewport, and only then captures the frame that is
    measured. The same handshake is re-run after the row's text element
    is hidden, so `without` is provably a frame that lacks exactly that
    ink rather than one that had not caught up yet.

    Returns the row list, the measured row's text handle, and the two
    boundary observations the caller asserts on.
    """
    lua = (
        "local th = require('scripts.tutorial_hud'); "
        "th.setOpen(true); "
        "local rows = {}; "
        "local handle = nil; "
        "local d0 = th.dump(); "
        "for i, r in ipairs(d0.rows) do "
        "  rows[i] = { id = r.id, marker = r.marker, depth = r.depth, "
        "              x = r.x, y = r.y, w = r.w, h = r.h }; "
        "  if i == 1 then handle = r.textHandle end end; "
        # Wait for the OPEN panel to be provably on a rendered frame.
        "local function settleFrames(pred) "
        "  for _ = 1, 8 do "
        "    if pred() then return true end; "
        "    debug.captureScreenshot('" + warm_path + "') end; "
        "  return pred() end; "
        "local shownPresented = settleFrames(function() "
        "  return th.isPresented() end); "
        "local a = debug.captureScreenshot('" + with_path + "'); "
        # Hide exactly the sticky row's own text, re-settle, capture.
        "local hidPresented = false; "
        "local b, c = nil, nil; "
        "if handle ~= nil then "
        "  UI.setVisible(handle, false); "
        "  local t = UI.armPresentation(); "
        "  hidPresented = settleFrames(function() "
        "    return UI.isPresented(t) end); "
        "  b = debug.captureScreenshot('" + without_path + "'); "
        "  UI.setVisible(handle, true); "
        "  local t2 = UI.armPresentation(); "
        "  settleFrames(function() return UI.isPresented(t2) end); "
        "  c = debug.captureScreenshot('" + again_path + "') end; "
        "local d = th.dump(); "
        "return { rows = rows, handle = handle, "
        "         panelX = d0.panelX, panelW = d0.panelW, "
        "         shownPresented = shownPresented, "
        "         hidPresented = hidPresented, "
        "         stillOpen = d.open, rowCount = #d.rows, "
        "         rebuildCount = d.rebuildCount, "
        "         shots = (a ~= nil and a.path ~= nil) "
        "                 and (b ~= nil and b.path ~= nil) "
        "                 and (c ~= nil and c.path ~= nil) }")
    got = send_json(port, lua, timeout=180.0)
    return got if isinstance(got, dict) else {}


def already_latched_phase(port: int, w: int, h: int, shots: str) -> None:
    """#996 then #1941, in the order a player meets them — and since
    #2056, with the presentation actually PROVEN rather than assumed.

    #996: latch the composite and check both of its subobjectives BEFORE
    that branch is ever revealed (the shipped acolyte spawn kit does
    exactly this before secure_water_source ever completes), then reveal
    it by completing its ancestors. The real, GPU-rendered checklist
    must show the branch -- not an empty panel.

    #1941: that suppression is a LOAN. This surface, on a visible page
    with the panel open, is what reports the presentation; the update
    tick that follows spends the suppression, and the ordinary #958 hide
    rule then empties the checklist. The reveal happens with the panel
    CLOSED on purpose -- a collapsed panel lays out no rows, so nothing
    can be presented before the open below.

    #2056: the old version of this phase only asserted that the
    `already_latched.png` screenshot REQUEST answered, so an empty-panel
    capture passed it. This one attributes pixels to the sticky rows the
    same way the #1581 row oracle does: capture the open panel, hide the
    sticky composite's own text element, capture again, and the columns
    that changed ARE its marker and label ink. Both captures happen
    inside one Lua chunk that also crosses #2056's boundary explicitly,
    so they provably precede the empty completed state rather than
    racing it.
    """
    inject_shipped_shape(port)
    send(port,
         "local tp = require('scripts.tutorial_progress'); "
         "require('scripts.tutorial_hud').setOpen(false); "
         "tp.setSubobjectiveChecked('prepare_water', true); "
         "tp.setSubobjectiveChecked('prepare_food', true); "
         "tp.completeObjective('prepare_expedition'); "
         "tp.completeObjective('place_portal'); "
         "tp.completeObjective('secure_water'); "
         "return 'ok'", timeout=15.0)
    branch = ["prepare_expedition", "prepare_water", "prepare_food"]
    d = poll_dump(port, lambda x: x.get("activeIds") == branch) or dump(port)
    check("the model holds the already-latched branch active while the "
          "panel is still collapsed", d.get("activeIds") == branch,
          str(d.get("activeIds")))
    check("and a collapsed panel has presented nothing",
          (d.get("rows") or []) == [], str(d.get("rowIds")))

    with_path = os.path.join(shots, "already_latched.png")
    without_path = os.path.join(shots, "already_latched_row_hidden.png")
    again_path = os.path.join(shots, "already_latched_again.png")
    warm_path = os.path.join(shots, "already_latched_warm.png")
    built = open_and_capture_build(port, with_path, without_path,
                                   again_path, warm_path)
    rows = built.get("rows") or []
    check("the already-latched prepare branch renders in authored order, "
          "instead of an empty checklist (#996)",
          [r.get("id") for r in rows] == branch,
          str([r.get("id") for r in rows]))
    check("the composite renders its normal completed marker, and both "
          "subobjectives render checked",
          [r.get("marker") for r in rows] == ["[x]", "(x)", "(x)"],
          str(rows))
    # The whole capture ran without a single update tick, so this is
    # the state the frames below were taken in.
    check("the panel was still OPEN and still holding all three rows "
          "throughout the capture",
          built.get("stillOpen") is True and built.get("rowCount") == 3,
          str(built))

    # #2056's positive half: the boundary really was crossed, twice.
    check("a completed renderer snapshot held the open panel before the "
          "measured frame was captured (#2056)",
          built.get("shownPresented") is True, str(built.get("shownPresented")))
    check("and again after the sticky row's text was hidden, so the two "
          "frames differ by that ink and nothing else",
          built.get("hidPresented") is True, str(built.get("hidPresented")))
    if not check("all three measurement screenshots answered",
                 built.get("shots") is True, str(built.get("shots"))):
        return

    # Requirement 5: attribute the captured pixels to the STICKY row.
    handle = built.get("handle")
    if not check("the sticky composite has a live text element to "
                 "attribute pixels to", bool(handle), str(built)):
        return
    # The row rect and the panel bounds come from the CAPTURE ITSELF,
    # never from a later round-trip: the branch retires within a tick of
    # the chunk returning, so by the time a second dump answers the rows
    # are gone -- which is the retirement working, not a missing row.
    if not check("the sticky composite's geometry came out of the "
                 "capture", isinstance(rows[0].get("y"), (int, float))
                 and isinstance(built.get("panelX"), (int, float)),
                 str(built)):
        return
    band = _row_band(rows[0], w, h)
    drift = _differing_columns(with_path, again_path, band)
    check("the frame held still across the measurement -- nothing but "
          "the hidden row differs between the two rendered captures",
          not drift, f"{len(drift)} column(s) drifted")
    cols = _differing_columns(with_path, without_path, band)
    px, pw = int(built["panelX"]), int(built["panelW"])
    check("the already-latched checklist screenshot contains the STICKY "
          "row's own ink -- its marker and label, not merely a non-empty "
          "panel rect (#2056 requirement 5)", bool(cols),
          f"no pixels changed when {branch[0]!r}'s text was hidden, "
          f"band {band}")
    if cols:
        lo, hi = min(cols), max(cols)
        print(f"       sticky row {branch[0]!r} glyph columns "
              f"x={lo} -> x={hi}; panel x={px} w={pw}")
        check("and that ink falls inside the checklist panel",
              lo >= px and hi < px + pw,
              f"glyph columns {lo}..{hi} vs panel [{px}, {px + pw})")

    # #1941: having been shown -- and now provably shown -- it retires.
    # Because this is the shipped session's TERMINAL branch, the
    # checklist reaches the empty completed state it could never reach
    # while the suppression was permanent. The panel stays OPEN
    # throughout: this is an emptied list, not a closed one.
    retired = poll_dump(port,
                        lambda x: x.get("open") is True
                        and (x.get("rows") or []) == []
                        and (x.get("activeIds") or []) == [])
    check("having been presented, the branch retires and the still-OPEN "
          "checklist reaches its empty completed state (#1941)",
          bool(retired), str(dump(port)))
    shot = os.path.join(shots, "retired_empty.png")
    check("the retired, empty checklist screenshot answers",
          screenshot(port, shot))
    # The empty state is a DIFFERENT frame from the one that carried the
    # rows -- the pixels the sticky row occupied are gone.
    if os.path.exists(shot) and os.path.exists(with_path):
        gone = _differing_columns(with_path, shot, band)
        check("and the sticky row's band really did change between the "
              "presented frame and the empty one", bool(gone),
              "the two frames are identical inside the row band")


def restore_shipped_tree(port: int, completed: list[str]) -> str:
    """Put the LIVE ENGINE REGISTRY tree back in front of the HUD with
    exactly `completed` latched, and open the panel.

    The labels measured have to be the authored ones, so the tree comes
    from `engine.getTutorialTree()` -- #957 writes that registry once at
    boot from data/tutorials/first_session.yaml and nothing in this
    probe touches it, so it survives the synthetic injections above.
    Copying the strings into Python or Lua would measure this file's
    idea of the labels instead of the shipped file's.

    The durable set is what selects which rows are on screen, and every
    state used below is STABLE under the ordinary #958 rule -- no
    subobjective is ever checked here, so a composite whose reveal is
    reached is never hideable and never leaves mid-measurement. That
    matters since #1941: the old fixture re-adopted the tree to latch
    all five rows sticky at once, and a suppression is now spent by the
    very act of showing it, so a measurement lasting several seconds and
    several screenshots can no longer be built on one. Two stable
    captures replace one transient one; between them they still measure
    every shipped row at every authored depth."""
    ids = ", ".join(f"'{cid}'" for cid in completed)
    got = send(port,
               "local tp = require('scripts.tutorial_progress'); "
               "local hud = require('scripts.tutorial_hud'); "
               "local tree = engine.getTutorialTree(); "
               "if tree == nil then return 'no-tree' end; "
               "tp.reset(); tp.setTree(tree); "
               f"for _, id in ipairs({{{ids}}}) do tp.completeObjective(id) end; "
               "hud.setOpen(true); hud.rebuild(); "
               "return tree.id or 'unnamed'",
               timeout=15.0).strip()
    return got


def measure_shipped_stage(port: int, w: int, h: int, shots: str, tag: str,
                          completed: list[str], want: list[int]) -> list[int]:
    """One stable capture of the shipped tree: latch `completed`, then
    bound the glyphs of the rows at SHIPPED_ROWS indices `want`."""
    tree_id = restore_shipped_tree(port, completed)
    if not check(f"[{tag}] the live engine registry still holds the shipped "
                 "first_session tree", tree_id == "first_session", tree_id):
        return []
    expected_ids = [SHIPPED_ROWS[i][0] for i in want]
    d = poll_dump(port, lambda x: x.get("rowIds") == expected_ids) or dump(port)
    if not check(f"[{tag}] exactly the expected shipped rows render, in "
                 "authored order", d.get("rowIds") == expected_ids,
                 f"{d.get('rowIds')} != {expected_ids} "
                 f"(capacity {d.get('capacity')}, "
                 f"scrollOffset {d.get('scrollOffset')})"):
        return []
    rows = d.get("rows") or []
    # The labels are the AUTHORED strings, not this file's copy of them:
    # they arrived through the registry, and the expectation is what is
    # checked against them. Depth is asserted too because indent eats
    # the row's horizontal budget, so a wrong depth would measure a
    # different layout than the shipped one.
    rendered = [(r.get("id"), r.get("label"), r.get("depth")) for r in rows]
    expected = [(SHIPPED_ROWS[i][0], SHIPPED_ROWS[i][1][0], SHIPPED_ROWS[i][1][1])
                for i in want]
    check(f"[{tag}] the rendered rows carry the authored labels and depths",
          rendered == expected, f"{rendered} != {expected}")
    for i in range(len(rows)):
        measure_row_bounds(port, w, h, shots, tag, i)
    shot = os.path.join(shots, f"shipped_rows_{tag}.png")
    check(f"[{tag}] the shipped-row screenshot answers",
          screenshot(port, shot))
    # The depths ACTUALLY rendered and measured, not the ones this file
    # expected: the coverage claim below has to be evidence.
    return [r.get("depth") for r in rows]


def shipped_rows_phase(port: int, w: int, h: int, shots: str) -> None:
    """#1581: bound the RENDERED GLYPHS of the shipped tree's rows.

    Every shipped row and every authored depth is still measured; they
    are reached in two stable stages rather than one (see
    restore_shipped_tree). Stage `ancestors` latches only the root, so
    the root stays active (its child is not complete) beside the child
    it reveals; stage `branch` latches the whole chain with no
    subobjective checked, so the terminal composite is not hideable and
    displays both of its subobjectives.
    """
    covered = measure_shipped_stage(port, w, h, shots, "ancestors",
                                    [SHIPPED_ROWS[0][0]], [0, 1])
    covered += measure_shipped_stage(port, w, h, shots, "branch",
                                     [SHIPPED_ROWS[0][0], SHIPPED_ROWS[1][0],
                                      SHIPPED_ROWS[2][0]], [2, 3, 4])
    check("the two stages between them measure every depth the shipped "
          "tree authors", sorted(set(covered)) == [0, 1, 2, 3],
          str(sorted(set(covered))))


def spawn_provisioned_acolyte(port: int) -> int:
    """Spawn one PLAYER-faction acolyte at the camera, with its shipped
    spawn kit.

    scripts/tutorial_eval.lua only counts `player` acolytes, and the kit
    (a full canteen and two rations, data/units/acolyte.yaml) is exactly
    what checks both prepare subobjectives -- which is what lets the
    reload check below observe an EMPTY checklist rather than a
    composite that is legitimately active because its live checks are
    off."""
    raw = send(port,
               "local gx, gy = camera.getPosition(); "
               "return tostring(unit.spawn('acolyte', math.floor(gx), "
               "math.floor(gy), nil, 'player'))", timeout=30.0).strip()
    try:
        return int(float(raw))
    except (ValueError, TypeError):
        return -1


def retired_reload_phase(port: int, shots: str) -> None:
    """#1941 requirement 4, through a REAL save and load in this GPU
    session: a checklist the player finished stays finished.

    Presentation is deliberately never persisted, so the load has no
    history to restore -- it RECONSTRUCTS one, treating every id the
    restored durable set already makes structurally reveal-eligible as
    previously presented. Without that rule this is exactly where the
    five rows came back: the load rebuilt the history against a fully
    completed set and judged every id sticky at once.
    """
    uid = spawn_provisioned_acolyte(port)
    if not check("a provisioned player acolyte spawns to satisfy the "
                 "prepare subobjectives", uid > 0, str(uid)):
        return
    # The shipped tree, fully latched, with the evaluator free to check
    # both subobjectives off the acolyte's own kit.
    tree_id = restore_shipped_tree(port, [rid for rid, _ in SHIPPED_ROWS[:3]])
    if not check("the shipped tree is back in front of the HUD for the "
                 "round trip", tree_id == "first_session", tree_id):
        return
    empty = poll_dump(port,
                      lambda x: (x.get("activeIds") or []) == [], seconds=30.0)
    if not check("with the acolyte provisioned, the checklist reaches its "
                 "EMPTY completed state before the save",
                 bool(empty), str(dump(port))):
        return

    page = send(port, "return tostring(world.getActiveWorldId())",
                timeout=15.0).strip().strip('"')
    accepted = send(port, f"return engine.saveWorld('{page}', '{SLOT}')",
                    timeout=60.0).strip()
    if not check("the finished session saves through the real save barrier",
                 accepted == "true", accepted):
        return
    req = capture_request_id(port, "return engine.getSaveStatus()")
    ok, status = wait_save_complete(port, req) if req is not None else (False, None)
    if not check("the save completes", ok, str(status)):
        return

    accepted = send(port, f"return engine.loadSave('{SLOT}')", timeout=60.0).strip()
    if not check("the save is accepted for loading", accepted == "true", accepted):
        return
    req = capture_request_id(port, "return engine.getLoadStatus()")
    published, status = wait_load_published(port, request_id=req)
    if not check("the save loads and publishes", published, str(status)):
        return

    d = poll_dump(port, lambda x: x.get("open") is False, seconds=30.0) or dump(port)
    check("the panel comes back COLLAPSED after a load (presentation state "
          "is never persisted)", d.get("open") is False, str(d.get("open")))
    # Open it and watch: the evaluation tick re-checks both subobjectives
    # against the same loaded world the save was taken from, which is the
    # exact tick that used to recompute five rows back onto the list.
    send(port, "require('scripts.tutorial_hud').setOpen(true); return 'ok'",
         timeout=15.0)
    still_empty = poll_dump(port,
                            lambda x: (x.get("activeIds") or []) == []
                            and (x.get("rows") or []) == [], seconds=30.0)
    check("the reopened checklist does NOT repopulate -- no already-retired "
          "ancestor returns to the active view (#1941)",
          bool(still_empty), str(dump(port)))
    time.sleep(2.0)
    after = dump(port)
    check("and it stays empty across further evaluation ticks",
          (after.get("activeIds") or []) == [], str(after.get("activeIds")))
    shot = os.path.join(shots, "reloaded_empty.png")
    check("the reloaded, empty checklist screenshot answers",
          screenshot(port, shot))


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

        print("== 1b. the collapsed caption's rendered glyph bounds (#1419) ==")
        caption_bounds_phase(args.port, w, h, shots, "collapsed")

        print("== 2. open by a real click on the toggle ==")
        opened = open_phase(args.port, shot_closed, shots)

        print("== 3. transparent overlay over terrain ==")
        transparency_phase(args.port, shot_closed, opened, shots)

        print("== 3b. the open caption's rendered glyph bounds (#1419) ==")
        caption_bounds_phase(args.port, w, h, shots, "open")

        print("== 4. long-list scrolling ==")
        scrolled = scroll_phase(args.port, shots)

        print("== 5. terrain input outside the controls ==")
        passthrough_phase(args.port, scrolled, w, h)

        print("== 6. close again ==")
        reclose_phase(args.port, shot_closed, shots)

        print("== 7. a branch already latched before its first reveal "
              "renders instead of an empty checklist (#996) ==")
        already_latched_phase(args.port, w, h, shots)

        print("== 8. the SHIPPED rows' rendered glyph bounds (#1581) ==")
        shipped_rows_phase(args.port, w, h, shots)

        print("== 9. a finished checklist stays finished across a real "
              "save/load (#1941) ==")
        retired_reload_phase(args.port, shots)
    finally:
        quit_engine(args.port, proc)
        remove_probe_slot()

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
