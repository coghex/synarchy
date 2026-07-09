#!/usr/bin/env python3
"""Input-injection check (#644) — HUMAN-RUN, needs a windowed instance.

The input.* verbs push synthetic events into the real input pipeline,
which only runs on a graphical instance (the GPU-less --headless mode
has no input thread and the verbs refuse there by design). Like
tools/screenshot_check.py, this ATTACHES to an already-running
GRAPHICAL instance instead of booting its own engine — launch the game
normally (console on port 8008), then:

  python3 tools/input_check.py             # attach to port 8008
  python3 tools/input_check.py --port 9008

It loads scripts/input_check_fixture.lua (a page of UI elements at
known framebuffer coordinates that records every input broadcast) and
asserts, end to end through the real pipeline:

  1. coordinate contract: input.moveMouse takes FRAMEBUFFER pixels
     (the debug.captureScreenshot space, #643) and lands at the exact
     window-space position engine.getMousePosition reports — the
     retina/DPI conversion, checked at whatever DPI the display has;
  2. a click at a framebuffer pixel activates the element drawn there
     (fixture button's onClick fires exactly once per click);
  3. a mods click really holds the modifier: the fixture samples
     engine.isKeyDown("Shift") inside its click callback;
  4. key/keyDown/keyUp: broadcast routing, held state visible to
     engine.isKeyDown between down and up, released after;
  5. text entry: with the fixture's text element focused, input.type
     fills it (char events through the focus path), input.key
     "Backspace" edits it, "Enter" submits;
  6. scroll routing: over the clickable element → UI scroll; over
     empty space → game scroll;
  7. drag: mouseDown + moveMouse + mouseUp — button reads held
     in between, and the release pairs with the press on the same
     "game" route (no UI desync).

Run it from any screen (main menu is fine); the fixture cleans itself
up afterwards. Exit code 0 = all checks passed.
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


def expect_ok(name: str, reply) -> None:
    check(name, isinstance(reply, dict) and reply.get("ok") is True, str(reply))


def state():
    return lua('return require("scripts.input_check_fixture").getState()')


def main() -> int:
    global PORT
    ap = argparse.ArgumentParser(description=__doc__)
    ap.add_argument("--port", type=int, default=8008,
                    help="debug-console port of the RUNNING graphical "
                         "instance (default 8008)")
    args = ap.parse_args()
    PORT = args.port

    print(f"input_check: attaching to port {PORT}")

    sizes = lua("return {engine.getFramebufferSize()}")
    wsizes = lua("return {engine.getWindowSize()}")
    if not (isinstance(sizes, list) and isinstance(wsizes, list)):
        print(f"  [FAIL] size queries failed: fb={sizes!r} win={wsizes!r} "
              "(is a GRAPHICAL instance running on this port?)")
        return 1
    fb_w, fb_h = sizes
    win_w, win_h = wsizes
    print(f"  framebuffer {fb_w}x{fb_h}, window {win_w}x{win_h} "
          f"(scale {fb_w / win_w:.2f}x)")

    # Load once per engine lifetime: a second loadScript would create a
    # second broadcast-receiving instance sharing the same singleton
    # state table, double-counting every event.
    send(PORT,
         'if not package.loaded["scripts.input_check_fixture"] then '
         'engine.loadScript("scripts/input_check_fixture.lua") end',
         expect_result=False)
    geom = lua('return require("scripts.input_check_fixture").setup()')
    if not (isinstance(geom, dict) and "btnX" in geom):
        print(f"  [FAIL] fixture setup failed: {geom!r}")
        return 1
    bx, by = geom["btnX"], geom["btnY"]
    tx, ty = geom["txtX"], geom["txtY"]

    try:
        # 1. coordinate contract: fb pixels in, window coords observed
        probe_x, probe_y = fb_w * 0.75, fb_h * 0.25
        expect_ok("moveMouse acks", lua(f"return input.moveMouse({probe_x}, {probe_y})"))
        pos = lua("return {engine.getMousePosition()}")
        want = (probe_x * win_w / fb_w, probe_y * win_h / fb_h)
        ok = (isinstance(pos, list)
              and abs(pos[0] - want[0]) < 0.51 and abs(pos[1] - want[1]) < 0.51)
        check("fb→window conversion lands exactly", ok,
              f"got {pos}, want ~{want}")

        # engine-side hit-test agrees the button is at its fb coords
        hit = lua(f"return UI.findElementAt({bx}, {by})")
        check("engine hit-test sees fixture at its fb coords",
              hit == geom["btnHandle"], f"findElementAt={hit!r}")

        # 2. click activates the element drawn at that pixel
        expect_ok("click acks", lua(f"return input.click({bx}, {by})"))
        st = state()
        check("click at fb pixel fires the element's onClick",
              st["clicks"] == 1, f"clicks={st['clicks']}")
        check("plain click holds no shift", st["shiftAtClick"] is False,
              str(st["shiftAtClick"]))

        # 3. modifier click
        expect_ok("shift-click acks",
                  lua(f'return input.click({bx}, {by}, "left", {{"shift"}})'))
        st = state()
        check("shift-click observed shift held in the callback",
              st["clicks"] == 2 and st["shiftAtClick"] is True,
              f"clicks={st['clicks']} shift={st['shiftAtClick']}")
        check("shift released after the click",
              lua('return engine.isKeyDown("Shift")') is False)

        # 4. key hold: down → visible to pollers + broadcast; up → released
        expect_ok("keyDown acks", lua('return input.keyDown("W")'))
        check("held key visible to engine.isKeyDown",
              lua('return engine.isKeyDown("W")') is True)
        expect_ok("keyUp acks", lua('return input.keyUp("W")'))
        check("released key no longer down",
              lua('return engine.isKeyDown("W")') is False)
        st = state()
        check("key broadcasts routed (down and up)",
              "W" in st["keysDown"] and "W" in st["keysUp"],
              f"downs={st['keysDown']} ups={st['keysUp']}")
        bad = lua('return input.key("NotAKey")')
        check("unknown key name rejected",
              isinstance(bad, dict) and "error" in bad, str(bad))

        # 5. text entry through the focus path
        check("text element takes focus",
              lua('return require("scripts.input_check_fixture").focusText()') is True)
        expect_ok("type acks", lua('return input.type("Hi world")'))
        got = lua('return require("scripts.input_check_fixture").getText()')
        check("typed text landed in the focused field", got == "Hi world",
              f"text={got!r}")
        expect_ok("backspace acks", lua('return input.key("Backspace")'))
        got = lua('return require("scripts.input_check_fixture").getText()')
        check("Backspace edits the field", got == "Hi worl", f"text={got!r}")
        expect_ok("enter acks", lua('return input.key("Enter")'))
        st = state()
        check("Enter submits the field", st["submits"] == 1,
              f"submits={st['submits']}")
        lua('return require("scripts.input_check_fixture").unfocusText()')

        # 6. scroll routing: over the clickable → UI; over empty → game
        expect_ok("moveMouse to button acks",
                  lua(f"return input.moveMouse({bx}, {by})"))
        expect_ok("scroll acks", lua("return input.scroll(0, -2)"))
        st = state()
        check("scroll over the element routes to UI scroll",
              st["uiScrolls"] >= 1, f"uiScrolls={st['uiScrolls']}")
        empty_x, empty_y = bx, by - 150  # above the fixture, left edge
        expect_ok("moveMouse to empty space acks",
                  lua(f"return input.moveMouse({empty_x}, {empty_y})"))
        expect_ok("scroll on empty acks", lua("return input.scroll(0, -2)"))
        st = state()
        check("scroll over empty space routes to game scroll",
              st["gameScrolls"] >= 1, f"gameScrolls={st['gameScrolls']}")

        # 7. drag with route pairing (empty space → 'game' both ways)
        ax, ay = empty_x, empty_y
        dx, dy = empty_x + 60, empty_y + 40
        expect_ok("mouseDown acks", lua(f"return input.mouseDown({ax}, {ay})"))
        check("button reads held mid-drag",
              lua("return engine.isMouseButtonDown(1)") is True)
        expect_ok("drag move acks", lua(f"return input.moveMouse({dx}, {dy})"))
        expect_ok("mouseUp acks", lua(f"return input.mouseUp({dx}, {dy})"))
        check("button released after mouseUp",
              lua("return engine.isMouseButtonDown(1)") is False)
        st = state()
        downs = [m for m in st["mouseDowns"] if m["button"] == 1]
        ups = [m for m in st["mouseUps"] if m["button"] == 1]
        check("drag press+release both broadcast",
              len(downs) >= 1 and len(ups) >= 1,
              f"downs={len(downs)} ups={len(ups)}")
        check("release pairs with the press on the 'game' route",
              bool(ups) and ups[-1]["route"] == "game",
              f"route={ups[-1]['route'] if ups else None}")
    finally:
        lua('return require("scripts.input_check_fixture").teardown()')

    alive = lua("return 1 + 1")
    check("instance still responsive afterwards", alive == 2, str(alive))

    if failures:
        print(f"input_check: FAILED ({len(failures)}): {', '.join(failures)}")
        return 1
    print("input_check: all checks passed")
    return 0


if __name__ == "__main__":
    sys.exit(main())
