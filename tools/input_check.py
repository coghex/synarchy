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
     engine.isKeyDown("Shift") inside its click callback (#697 — the
     release is fenced behind those callbacks; #727 made the click's
     own ack synchronously resolve that fence too, so the released
     check below normally sees it on the FIRST poll — it still polls
     briefly rather than asserting that timing exactly);
  4. key/keyDown/keyUp: broadcast routing, held state visible to
     engine.isKeyDown between down and up, released after;
  5. text entry: with the fixture's text element focused, input.type
     fills it (char events through the focus path), input.key
     "Backspace" edits it, "Enter" submits;
  6. scroll routing: over the clickable element → UI scroll; over
     empty space → game scroll;
  7. drag: mouseDown + moveMouse + mouseUp — button reads held
     in between, and the release pairs with the press on the same
     "game" route (no UI desync);
  8. split-hold modifier OWNERSHIP (#1927): a modifier-bearing split
     hold whose up half names NO modifier list still cleans up its own
     modifier, a split hold nested inside an independent
     input.keyDown("Shift") leaves that outer hold held, and the
     release verbs refuse a mods argument outright (naming the down
     half) rather than accepting one that cannot control the release.

Run it from any screen (main menu is fine); the fixture cleans itself
up afterwards. Exit code 0 = all checks passed.

The end-to-end run above stays human-run. The one thing that does NOT
need a window is the harness's own missed-click handling (#2052):

  python3 tools/input_check.py --selftest   # or --self-test

replays a missed click offline — no engine, no window, no network — and
proves the dependent checks report a diagnostic instead of aborting the
process, so the primary failure stays visible and the later sections and
the summary still run.
"""
from __future__ import annotations

import argparse
import sys
import time

from probelib import send, send_json


PORT = 8008
failures: list[str] = []
#: Every check name reached, in order — the record that proves a run got
#: past a failure instead of aborting on it (#2052's self-test asserts on
#: it; the live run only ever appends).
checks_run: list[str] = []


def check(name: str, ok: bool, detail: str = "") -> None:
    checks_run.append(name)
    print(f"  [{'ok' if ok else 'FAIL'}] {name}" + (f" — {detail}" if detail else ""))
    if not ok:
        failures.append(name)


def has_click_sample(name: str, st) -> bool:
    """Whether the fixture's click callback populated ``shiftAtClick``.

    The fixture initializes it to nil (input_check_fixture.lua:26) and
    only assigns it inside ``onInputCheckClick``, and a nil Lua field is
    OMITTED from the serialized state — so after a missed click the key
    simply is not there. Indexing it directly used to raise a KeyError
    that aborted the whole process, burying the primary click failure and
    skipping every later section plus the summary (#2052).

    Returns True when the dependent check can be evaluated; otherwise
    reports it as failed, naming the absent field and the precondition
    that never ran, and lets the caller move on to the next section.
    """
    if isinstance(st, dict) and "shiftAtClick" in st:
        return True
    check(name, False,
          "not evaluated: fixture state carries no shiftAtClick — the "
          "click callback never ran, so the click above missed")
    return False


def lua(code: str, timeout: float = 10.0):
    return send_json(PORT, code, timeout=timeout)


def expect_ok(name: str, reply) -> None:
    check(name, isinstance(reply, dict) and reply.get("ok") is True, str(reply))


def state():
    return lua('return require("scripts.input_check_fixture").getState()')


def poll_until(pred, timeout: float = 1.0, interval: float = 0.02) -> bool:
    """Poll pred until true or timeout — for effects that land a tick
    later by design (the #697 deferred modifier release)."""
    deadline = time.monotonic() + timeout
    while time.monotonic() < deadline:
        if pred():
            return True
        time.sleep(interval)
    return pred()


def main() -> int:
    global PORT
    ap = argparse.ArgumentParser(description=__doc__)
    ap.add_argument("--port", type=int, default=8008,
                    help="debug-console port of the RUNNING graphical "
                         "instance (default 8008)")
    ap.add_argument("--selftest", "--self-test", action="store_true",
                    help="offline self-test of this harness's missed-click "
                         "handling (#2052) — no engine, window or network")
    args = ap.parse_args()
    # Selected BEFORE the port is used for anything: the self-test must
    # reach no engine, fixture, poll, window or socket.
    if args.selftest:
        return selftest()
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
    # state table, double-counting every event. The tick rate argument
    # is REQUIRED — without it loadScript silently no-ops and the
    # fixture never registers for broadcasts (require() alone doesn't).
    # 0.0 is the EVENT-ONLY interval (#1695): the fixture is reached by
    # broadcast and never put on the update timer, which is exactly what
    # this check wants. It is not a "no rate" placeholder — a negative,
    # NaN, infinite or sub-millisecond rate would be REFUSED and load
    # nothing at all.
    send(PORT,
         'if not package.loaded["scripts.input_check_fixture"] then '
         'engine.loadScript("scripts/input_check_fixture.lua", 0.0) end',
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
        if has_click_sample("plain click holds no shift", st):
            check("plain click holds no shift", st["shiftAtClick"] is False,
                  str(st["shiftAtClick"]))

        # 3. modifier click
        expect_ok("shift-click acks",
                  lua(f'return input.click({bx}, {by}, "left", {{"shift"}})'))
        st = state()
        if has_click_sample("shift-click observed shift held in the callback",
                            st):
            check("shift-click observed shift held in the callback",
                  st["clicks"] == 2 and st["shiftAtClick"] is True,
                  f"clicks={st['clicks']} shift={st['shiftAtClick']}")
        # The release rides the fence behind the click's callbacks
        # (#697); since #727 the click's own ack resolves that fence
        # synchronously, so this normally reads released immediately —
        # poll_until tolerates it landing a beat later regardless.
        check("shift released after the click",
              poll_until(lambda:
                  lua('return engine.isKeyDown("Shift")') is False))

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

        # 8. #1927 split-hold modifier ownership. The two live
        # reproductions from docs/project_review_693-682.md PRR-1, run
        # here against the real pipeline: before #1927 the first block
        # left Shift stuck held, and the second released Shift out from
        # under the outer hold.
        expect_ok("modifier-bearing keyDown acks",
                  lua('return input.keyDown("W", {"shift"})'))
        check("split hold publishes its modifier between the halves",
              lua('return engine.isKeyDown("Shift")') is True)
        expect_ok("moveMouse mid-hold acks",
                  lua(f"return input.moveMouse({dx}, {dy})"))
        # The up half names NO modifier list — the ownership record,
        # not this call's arguments, decides what gets released.
        expect_ok("keyUp acks with no mods argument",
                  lua('return input.keyUp("W")'))
        check("split hold released its own modifier without a repeated list",
              poll_until(lambda:
                  lua('return engine.isKeyDown("Shift")') is False))
        check("split hold released its primary key",
              lua('return engine.isKeyDown("W")') is False)

        expect_ok("independent shift hold acks",
                  lua('return input.keyDown("Shift")'))
        expect_ok("nested modifier-bearing mouseDown acks",
                  lua(f'return input.mouseDown({ax}, {ay}, "left", {{"shift"}})'))
        expect_ok("nested mouseUp acks",
                  lua(f'return input.mouseUp({dx}, {dy})'))
        check("independent modifier hold survives the nested split hold",
              lua('return engine.isKeyDown("Shift")') is True)
        expect_ok("independent shift release acks",
                  lua('return input.keyUp("Shift")'))
        check("independent modifier hold ends on its own release",
              poll_until(lambda:
                  lua('return engine.isKeyDown("Shift")') is False))

        bad_key_up = lua('return input.keyUp("W", {"shift"})')
        check("keyUp refuses a mods argument, naming keyDown",
              isinstance(bad_key_up, dict)
              and "input.keyDown" in str(bad_key_up.get("error", "")),
              str(bad_key_up))
        bad_mouse_up = lua(f'return input.mouseUp({dx}, {dy}, "left", {{"shift"}})')
        check("mouseUp refuses a mods argument, naming mouseDown",
              isinstance(bad_mouse_up, dict)
              and "input.mouseDown" in str(bad_mouse_up.get("error", "")),
              str(bad_mouse_up))
        check("a refused release verb changed no held state",
              lua('return engine.isKeyDown("Shift")') is False
              and lua("return engine.isMouseButtonDown(1)") is False)
    finally:
        lua('return require("scripts.input_check_fixture").teardown()')

    alive = lua("return 1 + 1")
    check("instance still responsive afterwards", alive == 2, str(alive))

    return summarize()


def summarize() -> int:
    """Final summary + exit status: 0 only when every check passed."""
    if failures:
        print(f"input_check: FAILED ({len(failures)}): {', '.join(failures)}")
        return 1
    print("input_check: all checks passed")
    return 0


def selftest() -> int:
    """Offline proof (#2052) that a missed click no longer aborts the run.

    Replays the sections-2-and-3 sequence a MISSED click really produces
    — the callback never fired, so ``clicks`` is still 0 and the nil
    ``shiftAtClick`` is absent from the serialized state — through the
    very same ``check``/``has_click_sample``/``summarize`` path the live
    run uses, then asserts the properties the KeyError used to destroy.

    Touches no engine, window, socket or fixture. Exit 0 = the harness
    behaves; 1 = this hardening regressed.
    """
    global failures, checks_run

    print("input_check: offline self-test (no engine, window or network)")
    missed = {                     # exactly what getState() returns after a miss
        "clicks": 0,
        "keysDown": [], "keysUp": [], "chars": "", "submits": 0,
        "uiScrolls": 0, "gameScrolls": 0,
        "mouseDowns": [], "mouseUps": [],
    }
    primary = "click at fb pixel fires the element's onClick"
    plain = "plain click holds no shift"
    shifted = "shift-click observed shift held in the callback"
    sentinel = "instance still responsive afterwards"

    check(primary, missed["clicks"] == 1, f"clicks={missed['clicks']}")
    if has_click_sample(plain, missed):
        check(plain, missed["shiftAtClick"] is False,
              str(missed["shiftAtClick"]))
    if has_click_sample(shifted, missed):
        check(shifted,
              missed["clicks"] == 2 and missed["shiftAtClick"] is True,
              f"clicks={missed['clicks']} shift={missed['shiftAtClick']}")
    # Stands in for sections 3-8 and the post-teardown responsiveness
    # check: the run must still get here.
    check(sentinel, True)

    ran, failed = list(checks_run), list(failures)
    status = summarize()           # prints the failure summary it asserts on
    failures, checks_run = [], []

    problems: list[str] = []

    def want(ok: bool, why: str) -> None:
        print(f"  [{'ok' if ok else 'FAIL'}] {why}")
        if not ok:
            problems.append(why)

    want(primary in failed and plain in failed
         and failed.index(primary) < failed.index(plain),
         "the missed click is reported before the dependent diagnostic")
    want(plain in failed and shifted in failed,
         "neither dependent check is reported as a pass")
    want(ran.count(plain) == 1 and ran.count(shifted) == 1,
         "each dependent check reports exactly once")
    want(sentinel in ran and ran.index(sentinel) > ran.index(shifted),
         "a later check still executes after the missing field")
    want(status != 0, "the run's exit status stays non-zero")

    # The guard must not weaken the assertions when the field IS there.
    populated = dict(missed, clicks=1, shiftAtClick=False)
    want(has_click_sample(plain, populated) is True,
         "a populated shiftAtClick evaluates the real assertion")
    failures, checks_run = [], []

    if problems:
        print(f"input_check --selftest: FAILED ({len(problems)}): "
              f"{', '.join(problems)}")
        return 1
    print("input_check --selftest: all checks passed")
    return 0


if __name__ == "__main__":
    sys.exit(main())
