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
     debug.drainActionOutcomes() record, with outcome "accepted" and a
     handler naming the button's callback;
  2. a click on empty framebuffer space (well clear of any widget)
     drains EXACTLY ONE "deadclick" record. The check forces this
     deterministically by calling
     `require("scripts.ui_manager").showMenu("main")` first — a
     gameplay-active click on empty ground is a legitimate "noop"
     deselect instead (see scripts/init_mouse.lua), so the main menu is
     the one state where a truly empty click is guaranteed to be a
     genuine deadclick. This SWITCHES THE RUNNING INSTANCE TO THE MAIN
     MENU — run it when that's fine to do, not mid-session.

Exit code 0 = all checks passed.
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

    # 1. UI-consumed click -> exactly one "accepted" record with a handler.
    lua(f"return input.click({bx}, {by})")
    recs = drain()
    check("widget click drains EXACTLY ONE record", len(recs) == 1, str(recs))
    widget_rec = recs[0] if len(recs) == 1 else next(iter(recs), {})
    check("that record is accepted with a handler naming the callback",
          bool(widget_rec.get("outcome") == "accepted" and widget_rec.get("handler")),
          str(recs))

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

    print(f"\n{'ALL LAYER A CHECKS PASSED' if not failures else f'{len(failures)} FAILURE(S)'}")
    return 1 if failures else 0


if __name__ == "__main__":
    sys.exit(main())
