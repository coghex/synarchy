#!/usr/bin/env python3
"""F4 Layer A check (#646) — HUMAN-RUN, needs a windowed instance.

Layer A's routing outcomes are produced by the real GLFW-backed input
thread (`Engine.Input.Thread`) and its Lua-side tool/selection chain
(`scripts/init_mouse.lua`) — neither runs under `--headless` (no window,
no input thread), so this ATTACHES to an already-running GRAPHICAL
instance instead of booting its own, exactly like
`tools/input_check.py`/`tools/screenshot_check.py`. Launch the game
normally (console on port 8008), then:

  python3 tools/action_outcome_layer_a_check.py             # port 8008
  python3 tools/action_outcome_layer_a_check.py --port 9008

Reuses `scripts/input_check_fixture.lua` (#644) — a button at known
framebuffer coordinates — rather than building a second fixture, and
checks, end to end through the real input pipeline:

  1. a UI-consumed click (on the fixture button) drains EXACTLY ONE
     debug.drainActionOutcomes() record, with outcome "accepted" and
     handler == "onInputCheckClick" — the fixture's actual registered
     callback, not just any non-empty handler — AND a `where` matching
     the click's own coordinates (review round 9: the widget route
     used to hard-code `where` to nil regardless of where the click
     actually landed);
  1b. a RIGHT-click on that same left-click-only button (it never
      registers a right-click handler) also drains handler ==
      "onInputCheckClick" with a matching `where`  — proves the
      no-right-click-handler route preserves both the actual
      consumer's identity and its coordinates rather than a generic
      placeholder;
  2. a click on empty framebuffer space (well clear of any widget)
     drains EXACTLY ONE "deadclick" record, also with a matching
     `where`. The check forces this
     deterministically by calling
     `require("scripts.ui_manager").showMenu("main")` first — a
     gameplay-active click on empty ground is a legitimate "noop"
     deselect instead (see scripts/init_mouse.lua), so the main menu is
     the one state where a truly empty click is guaranteed to be a
     genuine deadclick. This SWITCHES THE RUNNING INSTANCE TO THE MAIN
     MENU — run it when that's fine to do, not mid-session.
  3. (best-effort, only if a gameplay world is ALREADY active on attach)
     a no-selection right-click at the framebuffer's extreme corner —
     the off-world tile-menu-miss deadclick route (#646 review round
     6). This one can't be forced deterministically the way #2 can (it
     needs camera framing that actually puts empty space at that
     corner, which this script doesn't control), so a miss here is
     reported as informational, not a failure.

Exit code 0 = all REQUIRED checks (1, 1b, 2) passed.
"""
from __future__ import annotations

import argparse
import sys

from probelib import send, send_json

PORT = 8008
failures: list[str] = []


def check(name: str, ok: bool, detail: str = "") -> None:
    print(f"  [{'ok' if ok else 'FAIL'}] {name}" + (f" — {detail}" if detail else ""))
    if not ok:
        failures.append(name)


def info(name: str, detail: str = "") -> None:
    print(f"  [info] {name}" + (f" — {detail}" if detail else ""))


def lua(code: str, timeout: float = 10.0):
    return send_json(PORT, code, timeout=timeout)


def drain():
    d = lua("return debug.drainActionOutcomes()")
    return d if isinstance(d, list) else []


def main() -> int:
    global PORT
    ap = argparse.ArgumentParser(description=__doc__)
    ap.add_argument("--port", type=int, default=8008,
                     help="debug-console port of the RUNNING graphical "
                          "instance (default 8008)")
    args = ap.parse_args()
    PORT = args.port

    print(f"action_outcome_layer_a_check: attaching to port {PORT}")

    sizes = lua("return {engine.getFramebufferSize()}")
    if not isinstance(sizes, list):
        print(f"  [FAIL] size query failed: {sizes!r} "
              "(is a GRAPHICAL instance running on this port?)")
        return 1
    fb_w, fb_h = sizes

    win_sizes = lua("return {engine.getWindowSize()}")
    win_w, win_h = win_sizes if isinstance(win_sizes, list) else (fb_w, fb_h)
    scale_x, scale_y = win_w / fb_w, win_h / fb_h

    def where_matches(rec: dict, fx: float, fy: float, tol: float = 2.0) -> bool:
        """`where` is recorded in WINDOW-space coordinates (review round
        9), while every click this script issues is specified in
        FRAMEBUFFER pixels (input.click's own contract) — convert
        before comparing, since the two spaces differ under HiDPI."""
        w = rec.get("where")
        if not isinstance(w, dict):
            return False
        ex, ey = fx * scale_x, fy * scale_y
        return (abs(w.get("x", float("inf")) - ex) <= tol
                and abs(w.get("y", float("inf")) - ey) <= tol)

    send(PORT,
         'if not package.loaded["scripts.input_check_fixture"] then '
         'engine.loadScript("scripts/input_check_fixture.lua") end',
         expect_result=False)
    geom = lua('return require("scripts.input_check_fixture").setup()')
    if not (isinstance(geom, dict) and "btnX" in geom):
        print(f"  [FAIL] fixture setup failed: {geom!r}")
        return 1
    bx, by = geom["btnX"], geom["btnY"]

    drain()  # clear anything already buffered before this run

    # 1. UI-consumed click -> exactly one "accepted" record naming the
    # fixture's actual registered callback (input_check_fixture.lua wires
    # the button's onClick to "onInputCheckClick") — not just ANY handler,
    # which would still pass if the wrong consumer recorded the click.
    lua(f"return input.click({bx}, {by})")
    recs = drain()
    check("widget click drains EXACTLY ONE record", len(recs) == 1, str(recs))
    widget_rec = recs[0] if len(recs) == 1 else next(iter(recs), {})
    check('that record is accepted with handler == "onInputCheckClick"',
          bool(widget_rec.get("kind") == "input.click"
               and widget_rec.get("outcome") == "accepted"
               and widget_rec.get("handler") == "onInputCheckClick"),
          str(recs))
    check("that record's where matches the actual click position "
          "(review round 9 — the widget route used to hard-code nil)",
          where_matches(widget_rec, bx, by), str(recs))

    # 1b. A right-click on that SAME left-click-only fixture button (it
    # never registers UI.setOnRightClick) takes the no-right-click-
    # handler route — Engine.Input.Thread must still record the exact
    # consuming widget's callback, not a generic placeholder (review
    # round 8: it previously recorded a fixed "ui_widget_no_
    # rightclick_handler" string regardless of which control ate the
    # click). Deterministic — this fixture's shape doesn't depend on
    # camera/world state.
    lua(f'return input.click({bx}, {by}, "right")')
    recs_rc = drain()
    check("right-click on the left-click-only button drains EXACTLY ONE record",
          len(recs_rc) == 1, str(recs_rc))
    rc_rec = recs_rc[0] if len(recs_rc) == 1 else next(iter(recs_rc), {})
    check('that record is accepted with handler == "onInputCheckClick" '
          "(the control's own identity, not a generic placeholder)",
          bool(rc_rec.get("kind") == "input.click"
               and rc_rec.get("outcome") == "accepted"
               and rc_rec.get("handler") == "onInputCheckClick"),
          str(recs_rc))
    check("that record's where matches the actual click position",
          where_matches(rc_rec, bx, by), str(recs_rc))

    # 3 (best-effort, gameplay must already be active on attach — this
    # script doesn't create a world). The off-world tile-menu-miss
    # deadclick route (scripts/init_mouse.lua's no-selection right-click
    # branch, review round 6): deselect everything, then a right-click
    # at the extreme corner MIGHT land off-world depending on camera
    # framing this script doesn't control. A confirmed deadclick is a
    # pass; anything else (the corner happened to show world) is
    # informational, not a failure — check #2 below is what forces a
    # deterministic deadclick.
    gameplay_active = lua(
        'return require("scripts.ui_manager").isGameplayInputActive()')
    if gameplay_active is True:
        lua("unit.deselectAll(); return true")
        drain()
        cx, cy = fb_w * 0.02, fb_h * 0.02
        lua(f'return input.click({cx}, {cy}, "right")')
        recs3 = drain()
        tile_menu_rec = next((r for r in recs3 if r.get("kind") == "input.click"), {})
        if tile_menu_rec.get("outcome") == "deadclick":
            check("off-world no-selection right-click drains a deadclick "
                  "(#646 review round 6 route)", True, str(recs3))
            check("that record's where matches the actual click position",
                  where_matches(tile_menu_rec, cx, cy), str(recs3))
        else:
            info("off-world no-selection right-click landed on world "
                 "geometry at this camera framing — route not exercised "
                 "this run (not a failure; see the deterministic check below)",
                 str(recs3))
    else:
        info("no gameplay world active on attach — off-world tile-menu-miss "
             "route not exercised this run (not a failure)")

    # 2. Force main-menu state so an empty-space click is unambiguously a
    # deadclick (gameplay-active would legitimately read "noop" instead —
    # see scripts/init_mouse.lua's deselect branch), then click well clear
    # of the fixture (top-right corner) and require exactly one record.
    lua('require("scripts.ui_manager").showMenu("main"); return true')
    drain()  # the menu transition itself is not part of what we're testing
    ex, ey = fb_w * 0.95, fb_h * 0.05
    lua(f"return input.click({ex}, {ey})")
    recs2 = drain()
    check("empty-space click at the main menu drains EXACTLY ONE record",
          len(recs2) == 1, str(recs2))
    deadclick_rec = recs2[0] if len(recs2) == 1 else next(iter(recs2), {})
    check("that record is a deadclick",
          deadclick_rec.get("kind") == "input.click"
          and deadclick_rec.get("outcome") == "deadclick",
          str(recs2))
    check("that record's where matches the actual click position "
          "(the game-chain route, scripts/init_mouse.lua)",
          where_matches(deadclick_rec, ex, ey), str(recs2))

    print(f"\n{'ALL LAYER A CHECKS PASSED' if not failures else f'{len(failures)} FAILURE(S)'}")
    return 1 if failures else 0


if __name__ == "__main__":
    sys.exit(main())
