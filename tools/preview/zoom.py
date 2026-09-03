"""The `zoom` family: centered bounded zoom (#1907) across every main
preview display kind, driven through the REAL wheel pipeline.

One scenario, `check_zoom` — phase `11.`, one boot per display kind
(bare list, focused item, unit enlarged, building, flora item,
structure item). `docs/engine_contracts.md` names this phase number as
the zoom gate, so it is preserved verbatim.

A library, not a probe: registered nowhere, runnable only through the
facade's inventory (`python3 tools/preview_probe.py --only zoom`).
"""
from __future__ import annotations

import time
from probelib import quit_engine, send, poll_until

from .harness import (boot_preview, check, click_element, dump, first_item,
                      poll_state, window_size)

# ---------------------------------------------------------------------
# Centered bounded zoom (#1907)
# ---------------------------------------------------------------------
#
# Everything below drives the REAL wheel pipeline: input.moveMouse puts
# the cursor over the dump-reported zoom REGION and input.scroll
# delivers a real GLFW-shaped wheel event, so the engine's own
# routeScroll decides which surface owns it. No screen coordinate is
# ever written here — every one is read back from previewManager.dump().
#
# 1.25 is scripts/ui/preview_zoom.lua's per-notch factor, so ten whole
# notches already overshoot the 8x range; the saturating deltas below
# are deliberately larger still, and prove magnitude is honoured rather
# than reduced to a sign.
ZOOM_SATURATING_DELTA = 24


def zoom_of(port: int) -> dict:
    """previewManager.dump().zoom — the multiplier, its two limits, the
    region the wheel is captured over, the surface's element handle, and
    the selected sprite's ACTUAL rendered bounds."""
    return dump(port).get("zoom") or {}


def _center(rect: dict) -> tuple[int, int]:
    return (int(rect.get("x", 0) + rect.get("width", 0) / 2),
            int(rect.get("y", 0) + rect.get("height", 0) / 2))


def scroll_over_zoom_region(port: int, dy: float, shift: bool = False) -> None:
    """One real wheel event at the center of the reported zoom region.

    `shift` holds a real LeftShift across the event (input.keyDown ->
    input.scroll -> input.keyUp), which is how the engine's own
    shiftHeld is computed (Engine.Input.Thread.Scroll reads
    inpKeyStates) — the only way to exercise Requirement 6's plain/Shift
    parity through the pipeline rather than around it."""
    region = zoom_of(port).get("region") or {}
    cx, cy = _center(region)
    send(port, f"return input.moveMouse({cx}, {cy})", timeout=10.0)
    if shift:
        send(port, 'return input.keyDown("LeftShift")', timeout=10.0)
    send(port, f"return input.scroll(0, {dy})", timeout=10.0)
    if shift:
        send(port, 'return input.keyUp("LeftShift")', timeout=10.0)


def scroll_at(port: int, x: int, y: int, dy: float) -> None:
    send(port, f"return input.moveMouse({x}, {y})", timeout=10.0)
    send(port, f"return input.scroll(0, {dy})", timeout=10.0)


def check_zoom_geometry(label: str, z: dict) -> bool:
    """Requirements 2-4 against what is REALLY on screen: the sprite's
    engine-reported bounds sit wholly inside the zoom region and stay
    centered on it, with no non-finite, negative or inverted extent."""
    sprite = z.get("sprite") or {}
    region = z.get("region") or {}
    for key in ("x", "y", "w", "h"):
        if not isinstance(sprite.get(key), (int, float)):
            return check(f"{label}: sprite bounds reported", False, str(sprite))
    for key in ("x", "y", "width", "height"):
        if not isinstance(region.get(key), (int, float)):
            return check(f"{label}: zoom region reported", False, str(region))
    tol = 1.0
    ok_positive = sprite["w"] > 0 and sprite["h"] > 0
    ok_contained = (sprite["x"] >= region["x"] - tol
                    and sprite["y"] >= region["y"] - tol
                    and sprite["x"] + sprite["w"]
                        <= region["x"] + region["width"] + tol
                    and sprite["y"] + sprite["h"]
                        <= region["y"] + region["height"] + tol)
    ok_centered = (abs((sprite["x"] + sprite["w"] / 2)
                       - (region["x"] + region["width"] / 2)) <= tol
                   and abs((sprite["y"] + sprite["h"] / 2)
                           - (region["y"] + region["height"] / 2)) <= tol)
    return check(f"{label}: centered and wholly inside its preview region",
                 ok_positive and ok_contained and ok_centered,
                 f"sprite={sprite} region={region}")


def check_zoom_pane(port: int, label: str, settle=None) -> bool:
    """The wheel contract for ONE preview pane, whichever mode backs it.

    `settle` is an optional callable run after each input, for the modes
    whose geometry is applied by previewManager.update on the next tick.
    """
    def state() -> dict:
        if settle:
            settle()
        return zoom_of(port)

    start = state()
    results = [
        check(f"{label}: a new session starts at multiplier 1",
              start.get("multiplier") == 1, start.get("multiplier")),
        check(f"{label}: the preview region owns a zoom surface",
              start.get("surface") is not None, start.get("surface")),
        check(f"{label}: limits reported as 1/8 and 1",
              start.get("min") == 0.125 and start.get("max") == 1,
              f"min={start.get('min')} max={start.get('max')}"),
        check_zoom_geometry(f"{label} at 1", start),
    ]
    fitted = dict(start.get("sprite") or {})

    # Graded shrinking: each event must move the multiplier DOWN, and a
    # bigger delta must move it further than a smaller one.
    scroll_over_zoom_region(port, 0.5)
    small = state().get("multiplier")
    scroll_over_zoom_region(port, -0.5)
    scroll_over_zoom_region(port, 2)
    large = state().get("multiplier")
    results.append(check(f"{label}: wheel magnitude is meaningful, not just "
                         "its sign",
                         isinstance(small, (int, float))
                         and isinstance(large, (int, float))
                         and small < 1 and large < small,
                         f"dy=0.5 -> {small}, dy=2 -> {large}"))

    # Saturate at the floor.
    scroll_over_zoom_region(port, ZOOM_SATURATING_DELTA)
    low = state()
    results.append(check(f"{label}: clamps exactly at the 1/8 floor",
                         low.get("multiplier") == 0.125,
                         low.get("multiplier")))
    low_sprite = low.get("sprite") or {}
    if fitted.get("w") and low_sprite.get("w"):
        results.append(check(
            f"{label}: both rendered dimensions are one eighth of the "
            "fitted ones",
            abs(low_sprite["w"] - fitted["w"] / 8) <= 1.0
            and abs(low_sprite["h"] - fitted["h"] / 8) <= 1.0,
            f"fitted={fitted['w']}x{fitted['h']} "
            f"floor={low_sprite['w']}x{low_sprite['h']}"))
    results.append(check_zoom_geometry(f"{label} at 1/8", low))

    # Further input at the floor is consumed and changes nothing.
    scroll_over_zoom_region(port, ZOOM_SATURATING_DELTA)
    results.append(check(f"{label}: further input at the floor changes nothing",
                         state().get("multiplier") == 0.125,
                         state().get("multiplier")))

    # Plain and Shift-modified wheel behave identically.
    scroll_over_zoom_region(port, -1)
    plain = state().get("multiplier")
    scroll_over_zoom_region(port, 1)
    scroll_over_zoom_region(port, -1, shift=True)
    shifted = state().get("multiplier")
    results.append(check(f"{label}: plain and Shift wheel behave identically",
                         plain == shifted, f"plain={plain} shift={shifted}"))

    # Back to the ceiling: exactly the fitted size, never past it.
    scroll_over_zoom_region(port, -ZOOM_SATURATING_DELTA)
    high = state()
    results.append(check(f"{label}: clamps exactly at the 1 ceiling",
                         high.get("multiplier") == 1, high.get("multiplier")))
    high_sprite = high.get("sprite") or {}
    if fitted.get("w") and high_sprite.get("w"):
        results.append(check(
            f"{label}: and back to exactly the fitted size, never larger",
            abs(high_sprite["w"] - fitted["w"]) <= 1.0
            and abs(high_sprite["h"] - fitted["h"]) <= 1.0,
            f"fitted={fitted['w']}x{fitted['h']} "
            f"back={high_sprite['w']}x{high_sprite['h']}"))
    results.append(check_zoom_geometry(f"{label} back at 1", high))
    scroll_over_zoom_region(port, ZOOM_SATURATING_DELTA)
    scroll_over_zoom_region(port, -ZOOM_SATURATING_DELTA)
    results.append(check(f"{label}: further input at the ceiling changes nothing",
                         state().get("multiplier") == 1,
                         state().get("multiplier")))
    return all(results)


def check_zoom_list_ownership(port: int, label: str) -> bool:
    """Requirement 7, over the REAL routing: a wheel over a located list
    row moves the list and never the zoom, and a wheel over the pane
    moves the zoom and never the list — including once the zoom has
    saturated, where a fall-through would show up as a moving list."""
    d = dump(port)
    rows = d.get("rows") or []
    if not rows:
        return check(f"{label}: list-versus-pane input ownership", False,
                     "no visible rows to aim at")
    bounds = rows[0].get("bounds") or {}
    row_x = int(bounds.get("x", 0) + bounds.get("w", 0) / 2)
    row_y = int(bounds.get("y", 0) + bounds.get("h", 0) / 2)

    before_zoom = zoom_of(port).get("multiplier")
    scroll_at(port, row_x, row_y, -3)
    after = dump(port)
    ok_list_owns = check(f"{label}: a wheel over the list never changes zoom",
                         (after.get("zoom") or {}).get("multiplier")
                         == before_zoom,
                         f"before={before_zoom} "
                         f"after={(after.get('zoom') or {}).get('multiplier')}")

    offset_before_pane = dump(port).get("scrollOffset")
    scroll_over_zoom_region(port, 2)
    mid = dump(port)
    ok_pane_owns = check(f"{label}: a wheel over the pane never moves the list",
                         mid.get("scrollOffset") == offset_before_pane
                         and (mid.get("zoom") or {}).get("multiplier")
                             != before_zoom,
                         f"offset {offset_before_pane} -> "
                         f"{mid.get('scrollOffset')}, zoom -> "
                         f"{(mid.get('zoom') or {}).get('multiplier')}")

    # At the floor the surplus must be consumed here, not leak downward.
    scroll_over_zoom_region(port, ZOOM_SATURATING_DELTA)
    offset_at_limit = dump(port).get("scrollOffset")
    for _ in range(3):
        scroll_over_zoom_region(port, 4)
    saturated = dump(port)
    ok_no_leak = check(f"{label}: input at the zoom limit does not fall "
                       "through to the list",
                       saturated.get("scrollOffset") == offset_at_limit
                       and (saturated.get("zoom") or {}).get("multiplier")
                           == 0.125,
                       f"offset {offset_at_limit} -> "
                       f"{saturated.get('scrollOffset')}")

    # Leave the session at the fitted size for whatever runs next.
    scroll_over_zoom_region(port, -ZOOM_SATURATING_DELTA)
    return all([ok_list_owns, ok_pane_owns, ok_no_leak])


def check_zoom_resize_preserved(port: int, label: str, settle=None) -> bool:
    """Requirement 9's last rung: a framebuffer resize preserves the
    multiplier while recomputing the fitted size from the NEW region."""
    scroll_over_zoom_region(port, 2)
    if settle:
        settle()
    before = zoom_of(port)
    held = before.get("multiplier")
    win_w, win_h = window_size(port)
    send(port, f"return engine.setResolution({win_w + 180}, {win_h + 140})",
         timeout=10.0)
    after = poll_until(
        10.0, lambda: ((zoom_of(port).get("region") or {}).get("width")
                       != (before.get("region") or {}).get("width"))
        and zoom_of(port)) or zoom_of(port)
    if settle:
        settle()
        after = zoom_of(port)
    ok_region = check(f"{label}: the zoom region really was recomputed",
                      (after.get("region") or {}).get("width")
                      != (before.get("region") or {}).get("width"),
                      f"{(before.get('region') or {}).get('width')} -> "
                      f"{(after.get('region') or {}).get('width')}")
    ok_held = check(f"{label}: the multiplier survives the resize",
                    after.get("multiplier") == held,
                    f"before={held} after={after.get('multiplier')}")
    ok_geometry = check_zoom_geometry(f"{label} after resize", after)
    scroll_over_zoom_region(port, -ZOOM_SATURATING_DELTA)
    return all([ok_region, ok_held, ok_geometry])


def _zoom_settle():
    """Unit and building geometry is applied by previewManager.update on
    the engine's own tick, so give it one before reading back."""
    time.sleep(0.2)


def check_zoom_object_identity(port: int, label: str, expect_reset: bool,
                               settle=None) -> bool:
    """Requirement 9. Click a DIFFERENT list row (located from the dump,
    never a hardcoded coordinate) at a non-default multiplier and see
    whether the zoom resets.

    Which answer is correct is the whole point: in a BARE simple
    category each texture IS the preview object, so it resets; in
    units/<name>, buildings/<name>, flora/<name> and structures/<name>
    the row selects another view of the SAME object, so it is
    preserved."""
    rows = dump(port).get("rows") or []
    selected = (dump(port).get("selected") or {}).get("label")
    target = next((r for r in rows if r.get("label") != selected), None)
    if target is None:
        return check(f"{label}: object-identity zoom rule", False,
                     "no second visible row to select")

    scroll_over_zoom_region(port, 3)
    if settle:
        settle()
    held = zoom_of(port).get("multiplier")
    if held == 1:
        return check(f"{label}: object-identity zoom rule", False,
                     "the wheel did not move the multiplier off 1")

    click_element(port, target.get("bounds") or {})
    after = poll_until(10.0, lambda: (
        (dump(port).get("selected") or {}).get("label") == target.get("label")
        and dump(port))) or dump(port)
    if settle:
        settle()
        after = dump(port)
    ok_selected = check(f"{label}: the row selection really changed",
                        (after.get("selected") or {}).get("label")
                        == target.get("label"),
                        after.get("selected"))
    got = (after.get("zoom") or {}).get("multiplier")
    if expect_reset:
        ok_rule = check(f"{label}: a different preview OBJECT resets the "
                        "multiplier to 1",
                        got == 1, f"held={held} after={got}")
    else:
        ok_rule = check(f"{label}: another view of the SAME preview object "
                        "preserves the multiplier",
                        got == held, f"held={held} after={got}")
    scroll_over_zoom_region(port, -ZOOM_SATURATING_DELTA)
    return ok_selected and ok_rule


def check_zoom(port: int) -> bool:
    """11. Centered bounded zoom (#1907) across every main preview
    display, driven through real input.moveMouse/input.scroll against
    dump-reported bounds."""
    print("11. centered bounded zoom: every main preview display")
    results = []

    # (target, label, list-backed, resets on a row click, needs a tick)
    targets = [
        ("icons", "bare list", True, True, False),
        ("icons/skill/climbing.png", "focused item", False, None, False),
        ("units/acolyte", "unit enlarged", True, False, True),
        ("buildings/acolyte_portal", "building", True, False, True),
        (f"flora/{first_item('flora')}", "flora item", True, False, False),
        ("structures/wire", "structure item", True, False, False),
    ]

    for target, label, list_backed, resets, ticks in targets:
        proc = boot_preview(port, f"11. zoom {target}", target,
                            f"preview engine (zoom {target})")
        try:
            settle = _zoom_settle if ticks else None
            poll_state(port, "ready", seconds=20.0)
            if settle:
                settle()
            results.append(check_zoom_pane(port, label, settle=settle))
            if list_backed:
                results.append(check_zoom_list_ownership(port, label))
                results.append(check_zoom_object_identity(
                    port, label, expect_reset=bool(resets), settle=settle))
            results.append(check_zoom_resize_preserved(port, label,
                                                       settle=settle))
        finally:
            quit_engine(port, proc)

    return all(results)
