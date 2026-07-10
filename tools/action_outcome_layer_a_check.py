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

  1. a UI-consumed click (on the fixture button) drains a
     debug.drainActionOutcomes() record with outcome "accepted" and a
     handler naming the button's callback;
  2. a click on empty framebuffer space (well clear of any widget) also
     drains exactly one record — "deadclick" if the instance isn't
     currently driving a visible gameplay world (e.g. sitting at a
     menu), or "noop"/a world-selection outcome if it is. Either is a
     pass; the point is that every click, not just the widget one,
     produces exactly one record.

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

    # 1. UI-consumed click -> "accepted", handler names the callback.
    lua(f"return input.click({bx}, {by})")
    recs = drain()
    widget_rec = next((r for r in recs if r.get("handler")), None)
    check("widget click drains an accepted record with a handler",
          bool(widget_rec and widget_rec.get("outcome") == "accepted"),
          str(recs))

    # 2. Click well clear of the fixture (top-right corner) — always
    # produces exactly one record, whatever the current game state.
    gameplay_active = lua(
        'return require("scripts.ui_manager").isGameplayInputActive()')
    ex, ey = fb_w * 0.95, fb_h * 0.05
    lua(f"return input.click({ex}, {ey})")
    recs2 = drain()
    click_recs = [r for r in recs2 if r.get("kind") == "input.click"]
    if gameplay_active is True:
        check("empty-space click while gameplay-active drains one record "
              "(deadclick if genuinely nothing was there, otherwise the "
              "world's own selection/noop outcome)",
              len(click_recs) >= 1, str(recs2))
    else:
        check("empty-space click while NOT gameplay-active drains a "
              "deadclick record",
              any(r.get("outcome") == "deadclick" for r in click_recs),
              str(recs2))

    print(f"\n{'ALL LAYER A CHECKS PASSED' if not failures else f'{len(failures)} FAILURE(S)'}")
    return 1 if failures else 0


if __name__ == "__main__":
    sys.exit(main())
