#!/usr/bin/env python3
"""--preview real-boot browser probe (#886 Phase 2, #887 Phase 3, #888 Phase 4).

Needs a GPU and a real GLFW surface/swapchain. The probe sets
SYNARCHY_PREVIEW_HIDDEN=1, so those windows stay hidden and non-activating
while preserving the windowed resize path an offscreen boot would bypass.
It is manual-only, never CI-gated (see tools/preview_cli_probe.py for the
no-GPU CLI-contract checks this probe used to also carry, split out in #886
so a classifier/path-containment regression fails PRs directly instead of
waiting for a manual dev-machine run).

Every check boots its own engine, so the whole run creates (and closes) a
hidden window per target — around twenty-two of them, a few minutes end to
end. Each of those boots writes its OWN log (#1763): the path is printed
when it is allocated and again in a closing summary that maps every log
back to the phase and target that wrote it, so a failing boot's output is
still there afterwards.

Checks:
  1. Boot profile + preview target over the debug console
     (engine.getBootProfile/getPreviewTarget), plus simple-category list
     mode (--preview icons): the texture filter is
     forced to nearest regardless of the persisted video config; the
     discovered entry list (require("scripts.preview_manager").dump())
     matches a filesystem-derived expectation computed independently in
     this probe; the first entry is auto-selected and its texture
     resolves to "ready"; clicking a DIFFERENT row (located from the
     dump's per-row interactive bounds — never hardcoded coordinates,
     the offscreen_probe.py convention) changes the selection; a real
     tapped Up/Down key changes the selection exactly once, while a held
     arrow pauses briefly and then repeats quickly, minimally scrolling the
     selected row into view until its matching key-up; scrolling
     over the list (input.moveMouse + input.scroll, located the same
     way) changes the reported scroll offset; a framebuffer GROW
     (engine.setResolution) reflows the panel bounds while preserving
     the current selection and scroll offset, and at every size the
     visible row count actually fits the reported panel height (no
     overflow); a subsequent SHRINK well below the list's natural row
     budget reduces the visible row count and still fits.
  2. Focused item mode (--preview icons/<item>): texture filter forced
     to nearest; no list (dump().rows is absent/empty) while the
     requested texture resolves; a resize reflows the panel bounds.
  3. Unit animation viewer (--preview units/acolyte, #887): the
     animation list matches a filesystem-derived expectation exactly and
     in order; the default selection is idle/south; the known YAML
     fps/loop values for the selected clip are reported; the frame index
     advances over wall time; clicking a DIFFERENT list row (located
     from the dump, never hardcoded coordinates) changes the selected
     animation and restarts the clip; real Up/Down pairs select animations
     and real Left/Right pairs wrap across the rendered direction cells;
     clicking a mirrored direction cell
     (located the same way) enlarges it and reports its real source
     direction; a resize preserves the animation, direction, and scroll
     offset; a clip whose AUTHORED loop is false still reports
     loop=false yet replays continuously past its own cycle (#1833);
     only the requested unit's textures load. Since #1260
     acolyte is the atlas pilot, so the viewer must additionally report
     EVERY animation as atlas-backed, sample the compiled atlases named
     by the checked-in index with that index's own cell geometry and
     real per-direction frame counts, and open not one source frame.
  4. A promoted declaration (--preview units/tiller, #887/#1257/#1261):
     a tree that carried an inventory-only `asset_units:` declaration
     until #1261 promoted it to a real `units:` entry browses with the
     DECLARED fps=8 / loop=true / flip=true on every animation, all
     eight direction cells populated by that declared mirroring, and —
     since #1261 retired per-frame unit-animation loading — every
     animation atlas-backed like any other unit's.
  4b. The rest of the shipped roster (--preview units/<name> for every
     remaining declared unit, #1261): one boot each, asserting the
     animation list still equals what is on disk, every animation
     reports atlas storage naming its own compiled atlas from the
     index, and nothing outside that unit's own directory loaded. The
     roster is read from data/units/ rather than written here, so a
     tree added without this probe noticing fails rather than passes.
  5. Buildings viewer (--preview buildings/acolyte_portal, #888): the
     entry list — animation subdirectories AND loose statics together —
     matches a filesystem+YAML-derived expectation exactly and in order,
     each row carrying its own static/animation identity; the default
     selection is the DIRECTORY holding the state_animations.built
     animation's declared frames (`idle`, never the YAML's own
     `portal-idle` name); its effective fps/loop come from that YAML
     entry; the frame index advances over wall time; a resize preserves
     the selection and scroll offset; clicking a STATIC row (located
     from the dump, never hardcoded coordinates) selects it and exposes
     NO playback; an animation entry whose AUTHORED loop is false still
     reports loop=false yet replays continuously past its own cycle
     (#1833); only this building's textures load.
  6. Buildings with no built state (--preview buildings/cargo_hold_S,
     #888): the default falls back to the YAML's own sprite
     (`default.png`), and the `demolish/` folder the YAML never mentions
     is still recognized as an animation by the numbered-frame
     convention, reporting the documented fps=8 / loop=false defaults
     (NOT the units viewer's loop=true).
  7. Buildings with no YAML at all (--preview buildings/dungeon_1,
     #888): every entry is a static — including the `damaged/` subtree,
     which surfaces as ordinary item-relative statics rather than one
     animation — and the default falls all the way through to the first
     entry.
  8. Flat grouped items (--preview flora/<name>, --preview
     structures/wire, #888): both dispatch into #886's SIMPLE-category
     browser rooted at the item's own folder (mode == "list") rather
     than a bespoke viewer, with the item folder's own textures listed
     in order and the first auto-selected.
  9. Canonical dispatch sweep (#888 / epic #427 acceptance): every
     canonical category — icons, items, ui, world, units, flora,
     buildings, structures — boots to its documented mode, and the
     Phase 1 (#632) "placeholder" mode is gone from every one of them.
 10. Trimmed loading (Requirement 5): engine.getLoadedTexturePaths() —
     the engine's OWN authoritative record of every texture ever loaded
     this session (Engine.Asset's apAssetPaths, populated by
     engine.loadTexture's Haskell handler itself, not any Lua caller's
     self-reported bookkeeping) — contains ONLY paths under the browsed
     category's root plus the documented chrome allowlist (list mode
     only), with no extras and nothing missing; the normal ~25-script
     gameplay set never loaded (the `ui` global, wired only outside the
     preview boot profile, stays nil).

Usage:
  python3 tools/preview_probe.py [--port 9150]

Exit 0 = all checks passed.
"""
from __future__ import annotations

import argparse
import json
import os
import re
import sys
import time
from probelib import boot, quit_engine, send, send_json, poll_until

LOG_DIR = "/tmp"
LOG_PREFIX = "preview_probe_engine"


class BootLogs:
    """One retained engine log per boot, plus the phase->path map (#1763).

    `probelib.boot` opens its log truncating, so two boots pointed at one
    path leave only the last boot's output. This probe boots far more
    than once per phase — `check_units_roster` and
    `check_canonical_dispatch_sweep` both loop, so a full run launches
    one engine per shipped unit and one per swept category on top of the
    fixed phases — and the same TARGET recurs across phases (`icons`,
    `units/acolyte` and `structures/wire` are each browsed more than
    once). A target-derived name therefore cannot be unique; every
    allocation carries its own ordinal as well.

    The mapping is printed the moment a path is handed out, not only in
    the closing summary: `boot` calls `sys.exit` when an engine dies
    before READY, and a failing run is exactly the run whose log a
    reader needs to find.
    """

    def __init__(self, directory: str = LOG_DIR, prefix: str = LOG_PREFIX):
        self._directory = directory
        self._prefix = prefix
        self._allocated: list[tuple[str, str]] = []

    def allocate(self, phase: str) -> str:
        """Reserve (and announce) this boot's own log path."""
        ordinal = len(self._allocated) + 1
        slug = re.sub(r"[^a-z0-9]+", "-", phase.lower()).strip("-") or "boot"
        path = os.path.join(self._directory,
                            f"{self._prefix}_{ordinal:02d}_{slug}.log")
        self._allocated.append((phase, path))
        print(f"  engine log [{ordinal:02d}] {phase}: {path}")
        return path

    def report(self) -> None:
        """Name every log this run wrote, against the phase that wrote it."""
        if not self._allocated:
            print("\nno engine was booted, so this run wrote no engine logs")
            return
        print(f"\nengine logs from this run ({len(self._allocated)} boot"
              f"{'' if len(self._allocated) == 1 else 's'}):")
        for ordinal, (phase, path) in enumerate(self._allocated, start=1):
            print(f"  {ordinal:02d}. {phase}: {path}")


LOGS = BootLogs()

# Every texture scripts.ui.list's list.init() (highlight.png) and its
# scrollbar.init() (arrow buttons + track + the 9-slice scrolltab set,
# scripts/ui/scrollbar.lua + scripts/ui/box_textures.lua) load THE
# MOMENT any list-mode browser is built, regardless of whether that
# particular list ever needs to scroll — the ONE allowed exception to
# "textures within the requested category" (Requirement 5). List mode
# only; focused/item mode never calls assetBrowser.init() at all.
CHROME_TEXTURE_PATHS = frozenset({
    "assets/textures/ui/highlight.png",
    "assets/textures/ui/scrollup.png",
    "assets/textures/ui/scrolldown.png",
    "assets/textures/ui/scrollbar.png",
    "assets/textures/ui/scrollbartop.png",
    "assets/textures/ui/scrollbarbottom.png",
    "assets/textures/ui/scrolltab/scrolltab.png",
    "assets/textures/ui/scrolltab/scrolltabn.png",
    "assets/textures/ui/scrolltab/scrolltabs.png",
    "assets/textures/ui/scrolltab/scrolltabe.png",
    "assets/textures/ui/scrolltab/scrolltabw.png",
    "assets/textures/ui/scrolltab/scrolltabne.png",
    "assets/textures/ui/scrolltab/scrolltabnw.png",
    "assets/textures/ui/scrolltab/scrolltabse.png",
    "assets/textures/ui/scrolltab/scrolltabsw.png",
})


def check(name: str, ok: bool, detail: str = "") -> bool:
    print(f"  [{'PASS' if ok else 'FAIL'}] {name}"
          + (f"  ({detail})" if detail else ""))
    return ok


def dump(port: int):
    got = send_json(port, 'return require("scripts.preview_manager").dump()')
    return got if isinstance(got, dict) else {}


def press_preview_key(port: int, key: str, changed, seconds: float = 10.0) \
        -> tuple[dict, dict]:
    """Tap one real key-down/key-up pair and return two stable observations.

    Release is enqueued immediately after the press, before any debug dump:
    waiting to observe the intermediate state would itself hold the key past
    the repeat delay and turn a tap oracle into an accidental long press.
    """
    quoted = json.dumps(key)
    # One console request is essential: two TCP round trips can themselves
    # exceed the 200 ms repeat delay and no longer describe a tap.
    send(port, f"local d=input.keyDown({quoted}); "
               f"local u=input.keyUp({quoted}); return d and u", timeout=10.0)
    reached = poll_until(seconds, lambda: (
        (lambda state: state if changed(state) else None)(dump(port))))
    reached = reached or dump(port)
    time.sleep(0.15)
    return reached, dump(port)


def hold_preview_key(port: int, key: str, changed, seconds: float = 10.0) \
        -> tuple[dict, dict]:
    """Hold through ``changed``, release, then return held/released states."""
    quoted = json.dumps(key)
    send(port, f"return input.keyDown({quoted})", timeout=10.0)
    try:
        held = poll_until(seconds, lambda: (
            (lambda state: state if changed(state) else None)(dump(port))))
        held = held or dump(port)
    finally:
        send(port, f"return input.keyUp({quoted})", timeout=10.0)
    time.sleep(0.15)
    return held, dump(port)


def window_size(port: int) -> tuple[int, int]:
    """The current WINDOW dimensions (engine.getVideoConfig's vcWidth/
    vcHeight) — the coordinate space engine.setResolution actually
    writes.

    Resize checks below MUST resize relative to these, never to
    previewManager's reported panelBounds: the panel is derived from the
    FRAMEBUFFER, which on a HiDPI display is 2x the window, and is
    further reduced by the browser's margins and list column. Feeding a
    panel height back into setResolution therefore asks for a window far
    larger than intended, so a "shrink" could silently grow the
    framebuffer and leave the visible row count unchanged."""
    got = send_json(port, "local w, h = engine.getVideoConfig(); return {w = w, h = h}")
    if isinstance(got, dict) and got.get("w") and got.get("h"):
        return int(got["w"]), int(got["h"])
    return 800, 600


def framebuffer_size(port: int) -> tuple[int, int]:
    """The current FRAMEBUFFER dimensions — what the browser's layout is
    actually derived from, and (on a HiDPI display) a whole-number
    multiple of the window size window_size() reports."""
    got = send_json(port, "local w, h = engine.getFramebufferSize(); return {w = w, h = h}")
    if isinstance(got, dict) and got.get("w") and got.get("h"):
        return int(got["w"]), int(got["h"])
    return 800, 600


def poll_state(port: int, want: str, seconds: float = 10.0, interval: float = 0.2) -> dict:
    """Poll previewManager.dump() until .state == want (texture upload is
    async — onAssetLoaded lands a tick or two after the request)."""
    deadline = time.monotonic() + seconds
    d = dump(port)
    while d.get("state") != want and time.monotonic() < deadline:
        time.sleep(interval)
        d = dump(port)
    return d


def check_trimmed_loading(port: int, category_root_prefix: str, allow_chrome: bool) -> bool:
    """Requirement 5, verified against the engine's OWN authoritative
    texture-load record (engine.getLoadedTexturePaths — Engine.Asset's
    apAssetPaths, populated by engine.loadTexture's Haskell handler
    itself) rather than any Lua caller's self-reported bookkeeping: every
    loaded texture this whole session is EITHER under the browsed
    category's root OR (list mode only) one of the documented chrome
    assets — no extras, nothing unaccounted for (#886 round-2 review)."""
    loaded = send_json(port, "return engine.getLoadedTexturePaths()")
    loaded = loaded if isinstance(loaded, list) else []
    allowed_chrome = CHROME_TEXTURE_PATHS if allow_chrome else frozenset()
    unaccounted = [p for p in loaded
                   if not p.startswith(category_root_prefix) and p not in allowed_chrome]
    return check("every engine-loaded texture is under the browsed "
                "category's root or a documented chrome asset",
                not unaccounted,
                f"loaded={loaded} unaccounted={unaccounted}")


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


def check_no_gameplay_scripts_loaded(port: int) -> bool:
    """The normal ~25-script gameplay/menu set (init_loader.lua's
    non-preview branch) never loads in preview mode — the `ui` global it
    wires (require("scripts.ui.registry")) is the cheapest sentinel:
    nil here means that whole branch never ran."""
    result = send(port, "return ui == nil")
    return check("normal gameplay script set never loaded (ui global is nil)",
                 result == "true", result)


def expected_entries_at(root: str) -> list[str]:
    """Independent, filesystem-derived expectation — mirrors
    Engine.Preview.Discovery.discoverEntries's contract (recursive,
    .png only, "/"-joined, sorted) without importing any Haskell/Lua
    code, so this actually cross-checks the real discovery behavior
    rather than restating it. Takes an arbitrary root, because #888
    routes a flora/structures ITEM folder through the very same
    discovery the simple categories use."""
    labels = []
    for dirpath, _dirs, files in os.walk(root):
        rel = os.path.relpath(dirpath, root)
        for f in files:
            if f.lower().endswith(".png"):
                label = f if rel == "." else f"{rel.replace(os.sep, '/')}/{f}"
                labels.append(label)
    return sorted(labels)


def expected_entries(category: str) -> list[str]:
    return expected_entries_at(os.path.join("assets", "textures", category))


def check_simple_list_mode(port: int) -> bool:
    print("1. boot profile/target + simple-category list mode (--preview icons)")
    proc = boot(port, log=LOGS.allocate("1. icons list"),
                mode=("--preview", "icons"),
                label="preview engine (icons list)")
    try:
        expected = expected_entries("icons")

        profile = send(port, "return engine.getBootProfile()")
        ok_profile = check("boot profile == preview", profile == "preview", profile)
        target = send_json(port, "return engine.getPreviewTarget()")
        ok_target = check("preview target == icons (bare category)",
                          isinstance(target, dict)
                          and target.get("category") == "icons"
                          and target.get("item") is None,
                          target)

        d = poll_state(port, "ready")

        # Requirement 3: nearest-neighbour is forced for the preview
        # session (previewManager.init), NOT merely assumed from the
        # default video config, which a user's persisted
        # config/video.local.yaml can override to "linear" (#886
        # round-1 review).
        texture_filter = send(port, "return select(10, engine.getVideoConfig())")
        ok_filter = check("texture filter forced to nearest",
                          texture_filter == "nearest", texture_filter)

        ok_mode = check("mode == list", d.get("mode") == "list", d.get("mode"))
        rows = d.get("rows") or []
        entry_count = d.get("entryCount")
        ok_count = check("entryCount matches filesystem-derived expectation",
                         entry_count == len(expected),
                         f"entryCount={entry_count} expected={len(expected)}")

        # The FULL ordered entry list, not just the count (#886 round-4
        # review) — a count-only check can't catch an omission or
        # substitution anywhere past the visible/selected rows.
        dumped_entries = [e.get("label") for e in (d.get("entries") or [])]
        ok_entries = check("full discovered entry list matches the "
                          "filesystem-derived expectation exactly, in order",
                          dumped_entries == expected,
                          f"dumped={dumped_entries[:5]}...({len(dumped_entries)}) "
                          f"expected={expected[:5]}...({len(expected)})"
                          if dumped_entries != expected else "")

        selected = d.get("selected") or {}
        ok_first = check("first entry auto-selected",
                         selected.get("label") == (expected[0] if expected else None),
                         selected)
        ok_ready = check("selection resolved to ready", d.get("state") == "ready", d.get("state"))

        # #2026: drive the engine's public injected-input path, including
        # matching key-up events. Tap once to prove immediate single-step
        # behavior, then HOLD one key through the preview-local delay/cadence
        # until it has crossed the initial viewport. That simultaneously
        # proves fast repeat, release, and minimum scroll-into-view. Finish
        # with one precise Up/Down pair at the scrolled position.
        ok_keys = True
        keyboard_end = d
        visible = len(rows)
        if len(expected) < 2 or visible < 1 or len(expected) <= visible:
            ok_keys = check("Up/Down keyboard navigation has a scrollable fixture",
                            False,
                            f"entries={len(expected)} visible={visible}")
        else:
            down_state, released_state = press_preview_key(
                port, "Down",
                lambda state: (state.get("selected") or {}).get("label")
                    == expected[1])
            ok_one_step = check(
                "Down key-down selects one adjacent row and key-up adds no step",
                (down_state.get("selected") or {}).get("label") == expected[1]
                and (released_state.get("selected") or {}).get("label")
                    == expected[1],
                f"down={down_state.get('selected')} "
                f"up={released_state.get('selected')}")

            # Put the selection two rows before the viewport edge with a real
            # click. The hold's immediate step reaches the last visible row;
            # only a DELAYED repeat can cross the edge and scroll. This stays
            # fast even on a Retina framebuffer that fits 65 rows at once.
            repeat_start_index = visible - 2
            setup_row = next(
                (row for row in rows
                 if row.get("label") == expected[repeat_start_index]), None)
            if setup_row:
                b = setup_row.get("bounds") or {}
                x = int(b.get("x", 0) + b.get("w", 0) / 2)
                y = int(b.get("y", 0) + b.get("h", 0) / 2)
                send(port, f"return input.click({x}, {y})", timeout=10.0)
            setup_state = poll_until(
                5.0, lambda: (lambda state: state
                    if (state.get("selected") or {}).get("label")
                        == expected[repeat_start_index] else None)(dump(port)))

            # `target_index` is zero-based and deliberately names the first
            # entry beyond the original visible row budget. poll_until's
            # interval is slower than the 40 ms repeat cadence, so accept any
            # observed index at or beyond it and derive the exact expected
            # minimum offset from the index where key-up actually lands.
            target_index = visible
            index_by_label = {label: index for index, label in enumerate(expected)}
            hold_started = time.monotonic()
            _held, keyboard_end = hold_preview_key(
                port, "Down",
                lambda state: index_by_label.get(
                    (state.get("selected") or {}).get("label"), -1)
                    >= target_index)
            hold_elapsed = time.monotonic() - hold_started
            selected_index = index_by_label.get(
                (keyboard_end.get("selected") or {}).get("label"), -1)
            expected_offset = max(0, selected_index - visible + 1)
            visible_rows = keyboard_end.get("rows") or []
            time.sleep(0.15)
            stopped_state = dump(port)
            ok_min_scroll = check(
                "held Down repeats quickly, stops on key-up, and scrolls only enough to expose its row",
                setup_state is not None
                and selected_index >= target_index
                and hold_elapsed < 5.0
                and keyboard_end.get("scrollOffset") == expected_offset
                and [r.get("label") for r in visible_rows]
                    == expected[expected_offset:expected_offset + visible]
                and any(r.get("label") == expected[selected_index]
                        and r.get("value") is True for r in visible_rows),
                f"elapsed={hold_elapsed:.3f}s selected={keyboard_end.get('selected')} "
                f"offset={keyboard_end.get('scrollOffset')} "
                f"rows={[r.get('label') for r in visible_rows]}") \
                and check(
                    "held Down remains stopped after release",
                    (stopped_state.get("selected") or {}).get("label")
                        == expected[selected_index]
                    and stopped_state.get("scrollOffset") == expected_offset,
                    f"released={keyboard_end.get('selected')} "
                    f"later={stopped_state.get('selected')}")

            _up_held, after_up = press_preview_key(
                port, "Up",
                lambda state: (state.get("selected") or {}).get("label")
                    == expected[selected_index - 1])
            _down_held, keyboard_end = press_preview_key(
                port, "Down",
                lambda state: (state.get("selected") or {}).get("label")
                    == expected[selected_index])
            ok_both = check(
                "Up and Down each traverse one row through the same selection path",
                (after_up.get("selected") or {}).get("label")
                    == expected[selected_index - 1]
                and (keyboard_end.get("selected") or {}).get("label")
                    == expected[selected_index],
                f"up={after_up.get('selected')} "
                f"down={keyboard_end.get('selected')}")
            ok_keys = all([ok_one_step, ok_min_scroll, ok_both])

        # Continue the older pointer checks from the live keyboard state.
        rows = keyboard_end.get("rows") or rows
        selected = keyboard_end.get("selected") or selected

        # Pick a different visible row than the current selection to click.
        other = next((r for r in rows if r.get("label") != selected.get("label")), None)
        if other is None:
            ok_click = check("selection changes on click", False,
                             "no second visible row to click")
        else:
            b = other.get("bounds") or {}
            x = int(b.get("x", 0) + b.get("w", 0) / 2)
            y = int(b.get("y", 0) + b.get("h", 0) / 2)
            send(port, f"return input.click({x}, {y})", timeout=10.0)
            d2 = poll_state(port, "ready")
            new_selected = d2.get("selected") or {}
            ok_click = check("selection changes on click (via row bounds, not hardcoded coords)",
                             new_selected.get("label") == other.get("label"),
                             new_selected)

        # Scroll: only meaningful if there are more entries than fit
        # visible at once (maxVisible=16, scripts/ui/asset_browser.lua).
        ok_scroll = True
        if len(expected) > 16 and rows:
            b0 = rows[0].get("bounds") or {}
            cx = int(b0.get("x", 0) + b0.get("w", 0) / 2)
            cy = int(b0.get("y", 0) + b0.get("h", 0) / 2)
            before = dump(port).get("scrollOffset")
            send(port, f"return input.moveMouse({cx}, {cy})", timeout=10.0)
            max_offset = max(0, len(expected) - len(rows))
            dy = 3 if before >= max_offset else -3
            send(port, f"return input.scroll(0, {dy})", timeout=10.0)
            after = dump(port).get("scrollOffset")
            ok_scroll = check("scroll offset changes on wheel input",
                             after != before, f"before={before} after={after}")

        # Resize (#886 round-1 review): the preview window is resizable
        # (App.Preview reuses the normal window config) — a framebuffer
        # resize must reflow the panel/sprite bounds AND preserve the
        # current selection + scroll offset rather than silently
        # resetting them (previewManager.onFramebufferResize).
        before_resize = dump(port)
        prev_bounds = before_resize.get("panelBounds") or {}
        prev_selected = before_resize.get("selected") or {}
        prev_scroll = before_resize.get("scrollOffset")
        # Resize relative to the WINDOW, never to panelBounds — see
        # window_size()'s docstring for why the latter is a unit error.
        win_w, win_h = window_size(port)
        send(port, f"return engine.setResolution({win_w + 200}, {win_h + 150})",
             timeout=10.0)
        after_resize = poll_until(
            10.0, lambda: (dump(port).get("panelBounds") or {}) != prev_bounds
                and dump(port))
        after_resize = after_resize or dump(port)
        ok_resize_bounds = check("panel bounds reflow on resize",
                                 after_resize.get("panelBounds") != prev_bounds,
                                 after_resize.get("panelBounds"))
        ok_resize_selection = check("selection preserved across resize",
                                    (after_resize.get("selected") or {}).get("label")
                                    == prev_selected.get("label"),
                                    after_resize.get("selected"))
        ok_resize_scroll = check("scroll offset preserved across resize",
                                 after_resize.get("scrollOffset") == prev_scroll,
                                 after_resize.get("scrollOffset"))

        # The visible row count must actually fit the reported panel
        # height at every size (#886 round-3 review: the browser
        # previously hardcoded a fixed 16-row list regardless of
        # params.height). itemHeight mirrors
        # scripts/ui/asset_browser.lua's own scaling exactly.
        ui_scale_raw = send(port, "return engine.getUIScale()")
        try:
            item_height = 32.0 * float(ui_scale_raw)
        except (TypeError, ValueError):
            item_height = 32.0

        def assert_rows_fit(label: str, state: dict) -> bool:
            b = state.get("panelBounds") or {}
            rows = state.get("rows") or []
            h = b.get("height", 0)
            return check(f"visible rows fit within the panel height, no overflow ({label})",
                        len(rows) * item_height <= h + 1,
                        f"rows={len(rows)} itemHeight={item_height} panelHeight={h}")

        ok_grow_fit = assert_rows_fit("after grow", after_resize)

        # Shrink (#886 round-3 review): well below the list's natural
        # row budget (icons has 67 entries, comfortably more than fit at
        # any of these sizes) — reproduces the exact regression the
        # review reported (an 800x600->800x400 shrink leaving a
        # 512px-tall list inside a 320px-tall panel).
        rows_before_shrink = len(after_resize.get("rows") or [])
        prev_h = (after_resize.get("panelBounds") or {}).get("height")
        # Self-calibrating rather than a guessed fraction: solve for the
        # WINDOW height that leaves room for about half the rows
        # currently shown. A fixed fraction can't work across displays —
        # the browser's row budget is floor(panelHeight / itemHeight),
        # where panelHeight comes from the FRAMEBUFFER (2x the window on
        # HiDPI) and itemHeight scales with the user's UI scale, so on a
        # retina screen at a small UI scale even a halved window can
        # still fit all 67 icons and "shrink" nothing at all.
        cur_win_w, cur_win_h = window_size(port)
        _, cur_fb_h = framebuffer_size(port)
        fb_ratio = (cur_fb_h / cur_win_h) if cur_win_h else 1.0
        target_rows = max(4, rows_before_shrink // 2)
        target_win_h = max(200, int((target_rows * item_height + 80) / fb_ratio))
        send(port, f"return engine.setResolution({cur_win_w}, {target_win_h})",
             timeout=10.0)
        after_shrink = poll_until(
            10.0, lambda: (dump(port).get("panelBounds") or {}).get("height") != prev_h
                and dump(port))
        after_shrink = after_shrink or dump(port)
        # Report a failed resize as itself rather than as a confusing
        # row-count mismatch — the two have very different causes.
        shrunk_h = (after_shrink.get("panelBounds") or {}).get("height")
        ok_shrank = check("the shrink actually changed the panel height",
                          shrunk_h is not None and prev_h is not None
                          and shrunk_h < prev_h,
                          f"before={prev_h} after={shrunk_h}")
        ok_shrink_rows = check("visible row count decreases on shrink",
                              len(after_shrink.get("rows") or []) < rows_before_shrink,
                              f"before={rows_before_shrink} "
                              f"after={len(after_shrink.get('rows') or [])}")
        ok_shrink_fit = assert_rows_fit("after shrink", after_shrink)

        # Trimmed loading (Requirement 5) — engine-authoritative (#886
        # round-2 review): every texture engine.getLoadedTexturePaths()
        # reports resolves under the browsed category's root or is a
        # documented chrome asset, and the normal gameplay script set
        # never loaded.
        root_prefix = os.path.join("assets", "textures", "icons") + os.sep
        ok_trimmed = check_trimmed_loading(port, root_prefix, allow_chrome=True)
        ok_no_gameplay = check_no_gameplay_scripts_loaded(port)

        return all([ok_profile, ok_target, ok_filter, ok_mode, ok_count,
                    ok_entries, ok_first, ok_ready, ok_keys,
                    ok_click, ok_scroll, ok_resize_bounds, ok_resize_selection,
                    ok_resize_scroll, ok_grow_fit, ok_shrank, ok_shrink_rows,
                    ok_shrink_fit, ok_trimmed, ok_no_gameplay])
    finally:
        quit_engine(port, proc)


def check_focused_item_mode(port: int) -> bool:
    print("2. focused item mode (--preview icons/skill/climbing.png): no list")
    target = "icons/skill/climbing.png"
    proc = boot(port, log=LOGS.allocate(f"2. {target}"),
                mode=("--preview", target),
                label="preview engine (icons item)")
    try:
        d = poll_state(port, "ready")
        texture_filter = send(port, "return select(10, engine.getVideoConfig())")
        ok_filter = check("texture filter forced to nearest",
                          texture_filter == "nearest", texture_filter)

        ok_mode = check("mode == item", d.get("mode") == "item", d.get("mode"))
        ok_no_list = check("no list (rows absent)", not d.get("rows"), d.get("rows"))
        selected = d.get("selected") or {}
        ok_selected = check("resolved texture == the requested item",
                            selected.get("label") == "skill/climbing.png",
                            selected)
        ok_ready = check("resolved to ready", d.get("state") == "ready", d.get("state"))

        # Trimmed loading (Requirement 5) — engine-authoritative (#886
        # round-2 review): focused mode never calls assetBrowser.init(),
        # so no list chrome is allowed at all — every engine-loaded
        # texture must be under the browsed category's root, and the
        # normal gameplay script set never loaded.
        root_prefix = os.path.join("assets", "textures", "icons") + os.sep
        ok_trimmed = check_trimmed_loading(port, root_prefix, allow_chrome=False)
        ok_no_gameplay = check_no_gameplay_scripts_loaded(port)

        # Resize (#886 round-1 review): focused mode has no list to
        # preserve, but the panel/sprite still must reflow, not overflow
        # or go stale (previewManager.onFramebufferResize).
        prev_bounds = d.get("panelBounds") or {}
        # Window units, not panel bounds — see window_size()'s docstring.
        win_w, win_h = window_size(port)
        send(port, f"return engine.setResolution({win_w + 200}, {win_h + 150})",
             timeout=10.0)
        after_resize = poll_until(
            10.0, lambda: (dump(port).get("panelBounds") or {}) != prev_bounds
                and dump(port)) or dump(port)
        ok_resize = check("panel bounds reflow on resize",
                          after_resize.get("panelBounds") != prev_bounds,
                          after_resize.get("panelBounds"))

        return (ok_filter and ok_mode and ok_no_list and ok_selected
                and ok_ready and ok_trimmed and ok_no_gameplay and ok_resize)
    finally:
        quit_engine(port, proc)


GAME_DIRECTION_ORDER = ["south", "south-west", "west", "north-west",
                        "north", "north-east", "east", "south-east"]

# The five stored directions a bilaterally-symmetric animation ships;
# the other three are mirrored at draw time.
MIRROR_SOURCE = {"south-west": "south-east", "west": "east",
                 "north-west": "north-east"}


# Every spelling Engine.Preview.Unit.parseDirectionDirName accepts, so
# this expectation stays a faithful independent implementation of the
# documented rule rather than a stricter one that would false-fail on a
# short-form direction folder.
DIRECTION_SPELLINGS = frozenset(
    [d.lower() for d in GAME_DIRECTION_ORDER]
    + ["s", "sw", "w", "nw", "n", "ne", "e", "se"])


def expected_unit_animations(unit: str) -> list[str]:
    """Independent, FILESYSTEM-derived expectation for the animation
    list: direct children of animations/ that hold at least one
    recognized, non-symlinked direction folder with at least one
    non-symlinked .png, case-sensitive lexicographic — computed without
    importing any Haskell/Lua code.

    #1261 moved the viewer's own authority to the unit YAML and its
    compiled index, which makes this a genuinely independent oracle
    rather than a restatement: the two must agree exactly, because
    `tools/pack_atlas.py --validate-only --strict` fails on any
    animation PNG no declaration owns, and the engine refuses a unit
    whose index does not cover its declarations. A folder that appears
    here and NOT in the dump is an undeclared one — which is the
    excluded case #1261 specifies, and a real finding for a tracked
    tree."""
    root = os.path.join("assets", "textures", "units", unit, "animations")
    out = []
    for name in os.listdir(root):
        animdir = os.path.join(root, name)
        if not os.path.isdir(animdir) or os.path.islink(animdir):
            continue
        has_frames = False
        for d in os.listdir(animdir):
            if d.lower() not in DIRECTION_SPELLINGS:
                continue
            ddir = os.path.join(animdir, d)
            if not os.path.isdir(ddir) or os.path.islink(ddir):
                continue
            if any(f.lower().endswith(".png")
                   and not os.path.islink(os.path.join(ddir, f))
                   for f in os.listdir(ddir)):
                has_frames = True
                break
        if has_frames:
            out.append(name)
    return sorted(out)


def compiled_index(unit: str):
    """The unit's generated atlas index, or None when it ships none.

    Read straight off disk with the stdlib json module: the probe's whole
    job here (#1260, D-9) is to prove the RUNNING viewer is sampling the
    artifacts that are actually checked in, so its expectation has to
    come from those artifacts rather than from anything the engine
    reports about itself."""
    path = os.path.join("assets", "textures", "units", unit, "atlas", "index.json")
    if not os.path.exists(path):
        return None
    with open(path) as fh:
        return json.load(fh)


def check_atlas_backed(port: int, unit: str, d: dict) -> bool:
    """#1260: the units viewer renders a MIGRATED unit through the
    production atlas metadata, not a preview-only per-frame decoder.

    Everything below is cross-checked against the compiled index on
    disk. A viewer that quietly fell back to the source frames beside a
    broken atlas would still pass every #887 check — it would list the
    same animations, play at the same rate, and mirror the same cells —
    so the storage mode has to be observed directly."""
    index = compiled_index(unit)
    if index is None:
        return check(f"{unit} ships a compiled atlas index", False,
                     "no assets/textures/units/%s/atlas/index.json" % unit)
    by_name = {a["name"]: a for a in index.get("animations", [])}

    # 1. EVERY animation selected the atlas — not just the one playing.
    entries = d.get("entries") or []
    legacy = [e.get("label") for e in entries if e.get("storage") != "atlas"]
    wrong_path = [e.get("label") for e in entries
                  if e.get("atlas") != (by_name.get(e.get("label")) or {}).get("atlas_path")]
    ok_all = check("every animation reports atlas storage, each naming its "
                   "own compiled atlas from the index",
                   entries and not legacy and not wrong_path,
                   f"count={len(entries)} legacy={legacy[:5]} "
                   f"mismatched={wrong_path[:5]}")
    ok_count = check("the viewer lists every animation the index compiled",
                     len(entries) == len(by_name),
                     f"dumped={len(entries)} index={len(by_name)}")

    # 2. The PLAYING clip samples that atlas, with the index's own cell
    #    geometry — a whole-image sample would report no cell at all.
    pb = d.get("playback") or {}
    want = by_name.get(pb.get("animation")) or {}
    cell = pb.get("cell") or {}
    ok_playing = check("the playing clip samples its compiled atlas with the "
                       "index's own cell size",
                       pb.get("storage") == "atlas"
                       and pb.get("atlas") == want.get("atlas_path")
                       and pb.get("texturePath") == want.get("atlas_path")
                       and cell.get("width") == want.get("cell_width")
                       and cell.get("height") == want.get("cell_height"),
                       f"playback=({pb.get('storage')}, {pb.get('texturePath')}, "
                       f"{cell}) index=({want.get('atlas_path')}, "
                       f"{want.get('cell_width')}x{want.get('cell_height')})")

    # 3. Each visible direction cell samples a SUB-RECT of that one
    #    image, and reports the index's REAL frame count for its own
    #    direction — never the padded column count (D-5).
    rows = {r["direction"]: r for r in want.get("directions", [])}
    dirs = pb.get("directions") or []
    bad = []
    for c in dirs:
        uv = c.get("uv") or {}
        src = rows.get(c.get("source"))
        if (c.get("texturePath") != want.get("atlas_path")
                or not uv
                or not (0.0 <= uv.get("u0", -1) < uv.get("u1", -1) <= 1.0)
                or not (0.0 <= uv.get("v0", -1) < uv.get("v1", -1) <= 1.0)
                or src is None
                or c.get("frameCount") != src.get("frame_count")):
            bad.append(c.get("direction"))
    ok_cells = check("every direction cell samples a sub-rect of that one "
                     "atlas, with the index's REAL per-direction frame count",
                     dirs and not bad,
                     f"cells={len(dirs)} bad={bad}")

    # 4. And nothing opened a source frame. This is the reduction the
    #    pilot claims, measured against the engine's own load record.
    loaded = send_json(port, "return engine.getLoadedTexturePaths()")
    loaded = loaded if isinstance(loaded, list) else []
    src_prefix = os.path.join("assets", "textures", "units", unit,
                              "animations") + os.sep
    from_source = [p for p in loaded if p.startswith(src_prefix)]
    atlas_prefix = os.path.join("assets", "textures", "units", unit,
                                "atlas") + os.sep
    from_atlas = [p for p in loaded if p.startswith(atlas_prefix)]
    ok_no_source = check("no source animation frame is loaded at all — only "
                         "compiled atlases",
                         not from_source and from_atlas,
                         f"source={from_source[:3]}({len(from_source)}) "
                         f"atlas={len(from_atlas)}")

    return all([ok_all, ok_count, ok_playing, ok_cells, ok_no_source])


def expected_yaml_meta(unit: str, animation: str):
    """(fps, loop) as declared in data/units/<unit>.yaml, or None when
    the file or the entry is absent. Parsed with a deliberately dumb
    line scanner rather than PyYAML (not a probe dependency) — the unit
    files are uniformly two-space-indented, so the animation's own
    fps:/loop: lines are the first ones after its key at a deeper
    indent."""
    path = os.path.join("data", "units", unit + ".yaml")
    if not os.path.exists(path):
        return None
    key = animation + ":"
    fps = loop = None
    indent = None
    with open(path) as fh:
        for line in fh:
            stripped = line.strip()
            cur = len(line) - len(line.lstrip())
            if indent is None:
                if stripped == key:
                    indent = cur
                continue
            if stripped and cur <= indent:
                break
            if stripped.startswith("fps:"):
                fps = float(stripped.split(":", 1)[1].strip())
            elif stripped.startswith("loop:"):
                loop = stripped.split(":", 1)[1].strip() == "true"
    if indent is None:
        return None
    # The YAML's own per-field defaults when the entry omits them.
    return (8.0 if fps is None else fps, True if loop is None else loop)


def click_element(port: int, bounds: dict) -> None:
    """Click the centre of a dump-reported interactive rect — the
    offscreen_probe.py convention: coordinates ALWAYS come from the
    dump, never from a hardcoded layout guess."""
    x = int(bounds.get("x", 0) + bounds.get("w", bounds.get("width", 0)) / 2)
    y = int(bounds.get("y", 0) + bounds.get("h", bounds.get("height", 0)) / 2)
    send(port, f"return input.click({x}, {y})", timeout=10.0)


def poll_unit_ready(port: int, seconds: float = 15.0) -> dict:
    """Poll until the unit viewer has a playing animation (its textures
    upload asynchronously, so the first dumps carry no playback yet)."""
    got = poll_until(seconds, lambda: (
        (lambda d: d if (d.get("playback") or {}).get("ready") else None)(dump(port))))
    return got or dump(port)


def check_forced_replay(port: int, what: str, selected_at: float,
                       frame_count, fps) -> bool:
    """#1833: preview playback REPLAYS every clip, whatever its authored
    `loop` says — the source value stays truthful in the dump, but the
    frame index wraps past the end instead of holding the last frame.

    Deliberately latency-independent, because the tracked `loop: false`
    cycles are short next to a TCP dump round trip (acolyte's
    attack_quick_RH_dagger is 5 frames at 12 fps = 0.417 s, cargo_hold_S's
    demolish 4 at 8 fps = 0.5 s). So this never tries to catch two samples
    inside one cycle, and never demands a strictly decreasing consecutive
    pair. Instead it waits until the clip is provably PAST its own end —
    `selected_at` is taken only after a dump already reported the new
    selection, and both viewers set their clock synchronously inside
    setAnimation/setEntry, so the real clock origin is at or BEFORE it —
    and then requires one sample below the final frame. Under the
    pre-#1833 hold-at-end policy that index is pinned at frameCount-1
    forever once the clip ends, so a single such sample is proof. The
    index must also keep CHANGING, so a clip frozen on some other frame
    cannot pass either.

    fps is read from the dump and guarded: a non-positive effective fps
    has no cycle period at all (frameIndexAt clamps rate to 0 and stays
    on frame 0), so such a fixture is reported unusable rather than
    divided by.
    """
    if not isinstance(frame_count, int) or frame_count < 2 \
            or not isinstance(fps, (int, float)) or fps <= 0:
        return check(f"{what}: replays continuously past its cycle", False,
                     f"unusable fixture: frameCount={frame_count!r} fps={fps!r}")
    cycle = frame_count / float(fps)
    while time.monotonic() - selected_at <= cycle:
        time.sleep(0.05)
    seen: list[int] = []
    deadline = time.time() + max(8.0, cycle * 6)
    while time.time() < deadline:
        idx = (dump(port).get("playback") or {}).get("frameIndex")
        if isinstance(idx, int):
            seen.append(idx)
        if any(i < frame_count - 1 for i in seen) and len(set(seen)) > 1:
            break
        time.sleep(0.05)
    ok_wrap = check(f"{what}: a sample taken past the {cycle:.3f} s cycle "
                    "reports a frame below the last one (impossible while "
                    "holding the end)",
                    any(i < frame_count - 1 for i in seen),
                    f"frameCount={frame_count} fps={fps} samples={seen}")
    ok_moving = check(f"{what}: the index keeps advancing past the cycle "
                      "boundary rather than settling on one frame",
                      len(set(seen)) > 1, f"samples={seen}")
    return ok_wrap and ok_moving


def check_units_mode(port: int) -> bool:
    print("3. unit animation viewer (--preview units/acolyte)")
    unit = "acolyte"
    proc = boot(port, log=LOGS.allocate(f"3. units/{unit}"),
                mode=("--preview", f"units/{unit}"),
                label="preview engine (units)")
    try:
        expected = expected_unit_animations(unit)
        d = poll_unit_ready(port)

        ok_mode = check("mode == unit", d.get("mode") == "unit", d.get("mode"))
        ok_filter = check("texture filter forced to nearest",
                          send(port, "return select(10, engine.getVideoConfig())")
                          == "nearest")

        # Requirement 1: the FULL ordered animation list, cross-checked
        # against the filesystem — a count-only check can't catch an
        # omission or substitution past the visible rows.
        listed = [e.get("label") for e in (d.get("entries") or [])]
        ok_entries = check("animation list matches the filesystem-derived "
                           "expectation exactly, in order",
                           listed == expected,
                           f"dumped={listed[:5]}...({len(listed)}) "
                           f"expected={expected[:5]}...({len(expected)})"
                           if listed != expected else "")

        # Requirement 2: idle (or the first animation), direction south.
        pb = d.get("playback") or {}
        ok_default = check("default selection is idle / south",
                           d.get("defaultAnim") == "idle"
                           and pb.get("animation") == "idle"
                           and pb.get("direction") == "south",
                           f"defaultAnim={d.get('defaultAnim')} playback={pb}")

        # Requirement 5: the clip's effective fps/loop come from the
        # unit's own YAML when it declares them.
        want_meta = expected_yaml_meta(unit, "idle")
        ok_meta = check("effective fps/loop match data/units/acolyte.yaml",
                        want_meta is not None
                        and abs((pb.get("fps") or 0) - want_meta[0]) < 1e-6
                        and pb.get("loop") == want_meta[1],
                        f"dump=({pb.get('fps')}, {pb.get('loop')}) yaml={want_meta}")

        # Requirement 3/4: every direction cell present, in game order,
        # with the three western ones reporting their real mirror source.
        dirs = pb.get("directions") or []
        ok_dirs = check("all eight direction cells, in the game's own order",
                        [c.get("direction") for c in dirs] == GAME_DIRECTION_ORDER,
                        [c.get("direction") for c in dirs])
        mirrored = {c["direction"]: c.get("source")
                    for c in dirs if c.get("mirrored")}
        ok_mirror = check("mirrored cells report their real source direction",
                          mirrored == MIRROR_SOURCE, mirrored)

        # #2026: use actual engine input pairs for both navigation axes.
        # Animation navigation goes through the browser's ordinary select
        # callback (including async texture readiness); direction navigation
        # walks the view's rendered cells, so mirrored/wrapped entries are
        # covered without reselecting or restarting the animation.
        idle_index = listed.index("idle") if "idle" in listed else -1
        if idle_index < 0 or len(listed) < 2:
            ok_key_animation = check("Up/Down animation navigation has a fixture",
                                     False, f"animations={listed}")
        else:
            step = 1 if idle_index + 1 < len(listed) else -1
            key = "Down" if step == 1 else "Up"
            restore_key = "Up" if step == 1 else "Down"
            adjacent = listed[idle_index + step]
            held, released = press_preview_key(
                port, key,
                lambda state: (state.get("selected") or {}).get("label")
                    == adjacent
                    and (state.get("playback") or {}).get("animation")
                    == adjacent
                    and (state.get("playback") or {}).get("ready") is True,
                seconds=15.0)
            _restore_held, restored = press_preview_key(
                port, restore_key,
                lambda state: (state.get("selected") or {}).get("label") == "idle"
                    and (state.get("playback") or {}).get("animation") == "idle"
                    and (state.get("playback") or {}).get("ready") is True,
                seconds=15.0)
            ok_key_animation = check(
                "Up/Down key pairs select adjacent animations exactly once",
                (held.get("selected") or {}).get("label") == adjacent
                and (released.get("selected") or {}).get("label") == adjacent
                and (restored.get("selected") or {}).get("label") == "idle",
                f"held={held.get('selected')} released={released.get('selected')} "
                f"restored={restored.get('selected')}")

        restored = poll_unit_ready(port)
        restored_pb = restored.get("playback") or {}
        restored_dirs = restored_pb.get("directions") or []
        initial_zoom = (restored.get("zoom") or {}).get("multiplier")
        if not restored_dirs or restored_pb.get("direction") != "south":
            ok_key_direction = check("Left/Right direction navigation has a fixture",
                                     False, restored_pb)
        else:
            left_want = restored_dirs[-1]
            left_held, left_released = press_preview_key(
                port, "Left",
                lambda state: (state.get("playback") or {}).get("direction")
                    == left_want.get("direction"))
            _right_held, right_released = press_preview_key(
                port, "Right",
                lambda state: (state.get("playback") or {}).get("direction")
                    == "south")
            left_pb = left_held.get("playback") or {}
            left_up_pb = left_released.get("playback") or {}
            right_pb = right_released.get("playback") or {}
            ok_key_direction = check(
                "Left/Right key pairs wrap rendered directions without changing animation or zoom",
                left_pb.get("direction") == left_want.get("direction")
                and left_pb.get("sourceDirection") == left_want.get("source")
                and left_up_pb.get("direction") == left_want.get("direction")
                and right_pb.get("direction") == "south"
                and left_pb.get("animation") == "idle"
                and right_pb.get("animation") == "idle"
                and (left_held.get("zoom") or {}).get("multiplier") == initial_zoom
                and (right_released.get("zoom") or {}).get("multiplier") == initial_zoom,
                f"left=({left_pb.get('direction')}, {left_pb.get('sourceDirection')}) "
                f"right={right_pb.get('direction')} zoom={initial_zoom}")

        # Requirement 9: the frame index advances over WALL time. idle
        # runs at the YAML's fps, so a second is many frames — poll for
        # any change rather than asserting a specific index.
        # NB the 1-tuple: poll_until returns on a TRUTHY value, and a
        # frame index of 0 is falsy — returning the bare index would
        # silently poll past a real advance back to frame zero.
        before = ((dump(port).get("playback") or {}).get("frameIndex"))
        after = poll_until(6.0, lambda: (
            (lambda i: (i,) if i != before else None)(
                (dump(port).get("playback") or {}).get("frameIndex"))))
        ok_advance = check("frame index advances over wall time",
                           after is not None and after[0] != before,
                           f"before={before} after={after}")

        # Requirement 3: clicking a direction cell enlarges it — pick a
        # MIRRORED one so source-direction reporting is exercised through
        # a real click, not just the initial dump. Done BEFORE the
        # animation switch below, deliberately: `idle` is a flip:true
        # clip, whereas the first visible row (attack_heavy_RH_dagger)
        # declares flip:false and so has no mirrored cell at all.
        target = next((c for c in dirs if c.get("mirrored")), None)
        if target is None:
            ok_cell = check("clicking a direction cell enlarges it", False,
                            "no mirrored direction cell to click")
        else:
            click_element(port, target.get("bounds") or {})
            after_click = poll_until(6.0, lambda: (
                (lambda p: p if p.get("direction") == target["direction"] else None)(
                    (dump(port).get("playback") or {}))))
            after_click = after_click or (dump(port).get("playback") or {})
            ok_cell = check("clicking a mirrored direction cell enlarges it and "
                            "reports its source direction",
                            after_click.get("direction") == target["direction"]
                            and after_click.get("mirrored") is True
                            and after_click.get("sourceDirection")
                                == MIRROR_SOURCE.get(target["direction"]),
                            after_click)

        # Resize: animation, direction, and scroll offset all survive
        # (the #887 amendment's reflow contract). Run right after the
        # direction click so the direction under test is the MIRRORED
        # one just selected, not merely the south default.
        pre = dump(port)
        pre_pb = pre.get("playback") or {}
        pre_bounds = pre.get("panelBounds") or {}
        pre_scroll = pre.get("scrollOffset")
        # Window units, not panel bounds — see window_size()'s docstring.
        win_w, win_h = window_size(port)
        send(port, f"return engine.setResolution({win_w + 200}, {win_h + 150})",
             timeout=10.0)
        post = poll_until(10.0, lambda: (
            (lambda s: s if (s.get("panelBounds") or {}) != pre_bounds else None)(
                dump(port)))) or dump(port)
        post_pb = post.get("playback") or {}
        ok_resize = check("animation, direction, and scroll offset all survive a resize",
                          post_pb.get("animation") == pre_pb.get("animation")
                          and post_pb.get("direction") == pre_pb.get("direction")
                          and post.get("scrollOffset") == pre_scroll,
                          f"before=({pre_pb.get('animation')}, "
                          f"{pre_pb.get('direction')}, {pre_scroll}) "
                          f"after=({post_pb.get('animation')}, "
                          f"{post_pb.get('direction')}, {post.get('scrollOffset')})")

        # Requirement 3: selecting a different animation (row located
        # from the dump, never a hardcoded coordinate) switches the clip
        # — and the NEW clip's effective fps/loop come from its own YAML
        # entry, a second, independent metadata data point.
        rows = post.get("rows") or []
        other = next((r for r in rows
                      if r.get("label") not in (None, post_pb.get("animation"))), None)
        if other is None:
            ok_select = check("selecting another animation changes the clip",
                              False, "no second visible row to click")
        else:
            click_element(port, other.get("bounds") or {})
            d2 = poll_unit_ready(port)
            d2_pb = d2.get("playback") or {}
            want2 = expected_yaml_meta(unit, other["label"])
            ok_select = check("selecting another animation (via row bounds, not "
                              "hardcoded coords) switches the clip and its metadata",
                              (d2.get("selected") or {}).get("label") == other["label"]
                              and d2_pb.get("animation") == other["label"]
                              and want2 is not None
                              and abs((d2_pb.get("fps") or 0) - want2[0]) < 1e-6
                              and d2_pb.get("loop") == want2[1],
                              f"clicked={other['label']} "
                              f"selected={(d2.get('selected') or {}).get('label')} "
                              f"playback=({d2_pb.get('animation')}, "
                              f"{d2_pb.get('fps')}, {d2_pb.get('loop')}) yaml={want2}")

        # #1833: forced continuous replay. Pick a row whose SOURCE
        # `loop` is false (acolyte has 26 of them) — under the
        # pre-#1833 hold-at-end policy such a clip freezes on its last
        # frame within half a second of selection, which is exactly the
        # usability gap this viewer existed to hit.
        d3 = poll_unit_ready(port)
        loops = {e.get("label"): e.get("loop") for e in (d3.get("entries") or [])}
        nonloop = next((r for r in (d3.get("rows") or [])
                        if loops.get(r.get("label")) is False), None)
        if nonloop is None:
            ok_replay = check("a source loop:false animation replays "
                              "continuously", False,
                              "no visible loop:false row to click")
        else:
            click_element(port, nonloop.get("bounds") or {})
            got = poll_until(10.0, lambda: (
                (lambda s: s if (s.get("playback") or {}).get("animation")
                    == nonloop["label"]
                    and (s.get("playback") or {}).get("ready") else None)(
                        dump(port))))
            selected_at = time.monotonic()
            pb3 = (got or dump(port)).get("playback") or {}
            # Requirement 6: the dump still reports the AUTHORED value.
            # Without this the replay check below would also pass on an
            # implementation that simply forced every clip to loop=true.
            ok_truthful = check("the replay fixture still reports its authored "
                                "loop=false (or this check proves nothing)",
                                pb3.get("animation") == nonloop["label"]
                                and pb3.get("loop") is False,
                                f"animation={pb3.get('animation')} "
                                f"loop={pb3.get('loop')}")
            ok_replay = check_forced_replay(
                port, f"units/{unit} {nonloop['label']} (authored loop=false)",
                selected_at, pb3.get("frameCount"), pb3.get("fps")) \
                and ok_truthful

        # #1260: the atlas contract, checked against the compiled index
        # on disk. Deliberately LAST, after the animation switch and the
        # resize above, so it observes a viewer that has already
        # reselected and rebuilt rather than only its first frame.
        ok_atlas = check_atlas_backed(port, unit, poll_unit_ready(port))

        # Requirement 6: only THIS unit's textures (plus list chrome).
        root_prefix = os.path.join("assets", "textures", "units", unit) + os.sep
        ok_trimmed = check_trimmed_loading(port, root_prefix, allow_chrome=True)
        ok_no_gameplay = check_no_gameplay_scripts_loaded(port)

        return all([ok_mode, ok_filter, ok_entries, ok_default, ok_meta,
                    ok_dirs, ok_mirror, ok_key_animation, ok_key_direction,
                    ok_advance, ok_select, ok_cell,
                    ok_resize, ok_replay, ok_atlas, ok_trimmed,
                    ok_no_gameplay])
    finally:
        quit_engine(port, proc)


def check_units_promoted(port: int) -> bool:
    """A tree #1261 (TEX-6) promoted out of the inventory-only form.

    `tiller` used to be this probe's "no data/units YAML at all"
    fixture. #1257 gave it a declaration under the top-level
    `asset_units:` key; #1261 promoted that to a real `units:` entry,
    because with per-frame unit-animation loading retired an animation
    renders only through the compiled atlas its declaration drives.

    The visible behaviour asserted here has not changed across either
    step — the declaration states `flip: true` over the canonical five,
    which is exactly what the pre-#1257 inference produced from that
    layout — so both were a change of SOURCE, not of result.
    """
    print("4. promoted declaration (--preview units/tiller): declared metadata")
    unit = "tiller"
    proc = boot(port, log=LOGS.allocate(f"4. units/{unit}"),
                mode=("--preview", f"units/{unit}"),
                label="preview engine (units, promoted)")
    try:
        yaml_path = os.path.join("data", "units", unit + ".yaml")
        declaration = ""
        if os.path.exists(yaml_path):
            with open(yaml_path) as fh:
                declaration = fh.read()
        # Pin the fixture's premise: the file exists AND declares the
        # gameplay key rather than the inventory-only one. Without both,
        # the metadata assertions below could pass against an
        # `asset_units:` entry and prove nothing about the promotion.
        ok_declared = check(
            "the fixture really is declared under units: (or this "
            "check proves nothing)",
            any(line.rstrip() == "units:"
                for line in declaration.splitlines())
            and not any(line.rstrip() == "asset_units:"
                        for line in declaration.splitlines()),
            yaml_path)

        d = poll_unit_ready(port)
        expected = expected_unit_animations(unit)
        listed = [e.get("label") for e in (d.get("entries") or [])]
        ok_entries = check("animation list matches the filesystem-derived "
                           "expectation exactly, in order",
                           listed == expected, f"dumped={listed} expected={expected}")

        ok_meta = check("every animation reports the DECLARED fps=8 / "
                        "loop=true / flip=true",
                        all(abs((e.get("fps") or 0) - 8.0) < 1e-6
                            and e.get("loop") is True
                            and e.get("flip") is True
                            for e in (d.get("entries") or [])),
                        [(e.get("label"), e.get("fps"), e.get("loop"),
                          e.get("flip")) for e in (d.get("entries") or [])])

        # The declared five-direction mirroring populates all eight
        # cells, exactly as the pre-#1257 inference did.
        pb = d.get("playback") or {}
        dirs = [c.get("direction") for c in (pb.get("directions") or [])]
        ok_dirs = check("declared mirroring populates all eight cells",
                        dirs == GAME_DIRECTION_ORDER, dirs)
        # `source`, not `sourceDirection`: a per-direction CELL reports
        # its own authored source under `source`, while `sourceDirection`
        # is the top-level playback field naming the ENLARGED cell's.
        # This check read the top-level name off each cell and so
        # compared three nils to three direction names — it could only
        # ever fail (found by #1260, which first ran the probe against a
        # tree where the units phase above passes).
        ok_mirrored = check("W/SW/NW are the mirrored cells, sourced from "
                            "their eastern counterparts",
                            [c.get("source")
                             for c in (pb.get("directions") or [])
                             if c.get("mirrored")]
                            == ["south-east", "east", "north-east"],
                            [(c.get("direction"), c.get("mirrored"),
                              c.get("source"))
                             for c in (pb.get("directions") or [])])

        # #1261: a promoted tree is atlas-backed like any other. Before
        # it, tiller was the probe's canonical LEGACY unit.
        ok_atlas = check_atlas_backed(port, unit, d)

        root_prefix = os.path.join("assets", "textures", "units", unit) + os.sep
        ok_trimmed = check_trimmed_loading(port, root_prefix, allow_chrome=True)
        ok_no_gameplay = check_no_gameplay_scripts_loaded(port)

        return all([ok_declared, ok_entries, ok_meta, ok_dirs, ok_mirrored,
                    ok_atlas, ok_trimmed, ok_no_gameplay])
    finally:
        quit_engine(port, proc)


def shipped_units() -> list[str]:
    """Every declared unit, from data/units/ rather than from a list
    written here — a tree added without this probe noticing is the
    failure mode a hardcoded roster would hide."""
    return sorted(f[:-len(".yaml")]
                  for f in os.listdir(os.path.join("data", "units"))
                  if f.endswith(".yaml"))


def check_units_roster(port: int) -> bool:
    """#1261 requirement 2: EVERY shipped unit tree browses, and does so
    through the production atlas/index path.

    Phases 3 and 4 cover acolyte and tiller in depth. This one is
    breadth: one boot per remaining tree, asserting the three things
    that can only be observed against real compiled artifacts — the
    animation list still equals what is on disk, every animation
    reports atlas storage naming its own compiled atlas, and nothing
    outside this unit's own directory was loaded.
    """
    covered = {"acolyte", "tiller"}
    units = [u for u in shipped_units() if u not in covered]
    print(f"4b. the rest of the shipped roster ({', '.join(units)}): "
          f"atlas-backed browsing")
    ok = True
    for unit in units:
        proc = boot(port, log=LOGS.allocate(f"4b. units/{unit}"),
                    mode=("--preview", f"units/{unit}"),
                    label=f"preview engine (units/{unit})")
        try:
            d = poll_unit_ready(port)
            expected = expected_unit_animations(unit)
            listed = [e.get("label") for e in (d.get("entries") or [])]
            ok_entries = check(f"{unit}: animation list matches the "
                               f"filesystem-derived expectation, in order",
                               listed == expected,
                               f"dumped={listed} expected={expected}")
            ok_atlas = check_atlas_backed(port, unit, d)
            root_prefix = os.path.join(
                "assets", "textures", "units", unit) + os.sep
            ok_trimmed = check_trimmed_loading(port, root_prefix,
                                               allow_chrome=True)
            ok = all([ok, ok_entries, ok_atlas, ok_trimmed])
        finally:
            quit_engine(port, proc)
    return ok


def is_frame_name(f: str) -> bool:
    """The checked-in numbered-frame convention, mirroring
    Engine.Preview.Building.isFrameFileName: frame_000.png, frame1.png,
    frame-3.png — but never floor.png or wall_ne.png."""
    stem = os.path.splitext(f)[0].lower()
    if not stem.startswith("frame"):
        return False
    rest = stem[len("frame"):].lstrip("-_")
    return bool(rest) and rest.isascii() and rest.isdigit()


def building_yaml(name: str) -> dict:
    """{'sprite': str|None, 'built': str|None,
        'anims': {name: {'fps': float, 'loop': bool, 'frames': [paths]}}}
    from data/buildings/<name>.yaml, or all-empty when the file is
    absent (dungeon_1 has none).

    Parsed with a deliberately dumb indent scanner rather than PyYAML
    (not a probe dependency), the same way expected_yaml_meta above
    reads a unit file. The per-animation defaults restated here (fps 8,
    loop FALSE) are BuildingYamlAnim's own — note loop differs from the
    units schema's default of true."""
    out: dict = {"sprite": None, "built": None, "anims": {}}
    path = os.path.join("data", "buildings", name + ".yaml")
    if not os.path.exists(path):
        return out
    section = None            # None | "state_animations" | "animations"
    section_indent = -1
    anim_indent = None
    cur = None
    with open(path) as fh:
        for raw in fh:
            stripped = raw.strip()
            if not stripped or stripped.startswith("#"):
                continue
            indent = len(raw) - len(raw.lstrip())
            if section is not None and indent <= section_indent:
                section, anim_indent, cur = None, None, None
            if section is None:
                if stripped.startswith("sprite:"):
                    out["sprite"] = stripped.split(":", 1)[1].strip().strip('"')
                elif stripped == "state_animations:":
                    section, section_indent = "state_animations", indent
                elif stripped == "animations:":
                    section, section_indent = "animations", indent
                continue
            if section == "state_animations":
                if stripped.startswith("built:"):
                    out["built"] = stripped.split(":", 1)[1].strip().strip('"')
                continue
            # section == "animations": a key at the block's own child
            # indent starts a new animation; anything deeper belongs to it.
            if stripped.endswith(":") and (anim_indent is None
                                           or indent == anim_indent):
                anim_indent = indent
                cur = stripped[:-1].strip()
                out["anims"][cur] = {"fps": 8.0, "loop": False, "frames": []}
            elif cur is None:
                continue
            elif stripped.startswith("fps:"):
                out["anims"][cur]["fps"] = float(stripped.split(":", 1)[1].strip())
            elif stripped.startswith("loop:"):
                out["anims"][cur]["loop"] = (
                    stripped.split(":", 1)[1].strip() == "true")
            elif stripped.startswith("- "):
                out["anims"][cur]["frames"].append(stripped[2:].strip().strip('"'))
    return out


def expected_building_entries(name: str) -> list[tuple[str, bool]]:
    """Independent, filesystem+YAML-derived expectation for a building's
    entry list — mirrors Engine.Preview.Building.discoverBuildingEntries
    without importing any Haskell/Lua code: a directory whose textures a
    YAML animation actually DECLARES, or whose textures all follow the
    numbered-frame convention, is ONE animation entry labeled by its
    item-relative path; every other directory is descended into, so its
    textures surface as ordinary statics. Returns (label, animated)
    pairs in the engine's own label order."""
    root = os.path.join("assets", "textures", "buildings", name)
    meta = building_yaml(name)
    anim_dirs = {
        os.path.dirname(p).replace(os.sep, "/")
        for a in meta["anims"].values() for p in a["frames"]
    }
    out: list[tuple[str, bool]] = []

    def walk(d: str) -> None:
        for entry in sorted(os.listdir(d)):
            full = os.path.join(d, entry)
            if os.path.islink(full):
                continue
            label = os.path.relpath(full, root).replace(os.sep, "/")
            if os.path.isdir(full):
                pngs = [f for f in os.listdir(full)
                        if f.lower().endswith(".png")
                        and not os.path.islink(os.path.join(full, f))]
                if pngs and (full.replace(os.sep, "/") in anim_dirs
                             or all(is_frame_name(f) for f in pngs)):
                    out.append((label, True))
                else:
                    walk(full)
            elif entry.lower().endswith(".png"):
                out.append((label, False))

    walk(root)
    return sorted(out)


def dumped_building_entries(d: dict) -> list[tuple[str, bool]]:
    return [(e.get("label"), e.get("animated") is True)
            for e in (d.get("entries") or [])]


def built_default_label(name: str) -> str | None:
    """The label the state_animations.built animation must resolve to:
    the DIRECTORY its declared frames live in — deliberately computed
    from the frame paths, never from the animation's own YAML name,
    because acolyte_portal's differ (portal-idle vs idle/)."""
    meta = building_yaml(name)
    built = meta["built"]
    frames = meta["anims"].get(built, {}).get("frames") if built else None
    if not frames:
        return None
    return os.path.basename(os.path.dirname(frames[0]))


def check_buildings_mode(port: int) -> bool:
    print("5. buildings viewer (--preview buildings/acolyte_portal)")
    name = "acolyte_portal"
    proc = boot(port, log=LOGS.allocate(f"5. buildings/{name}"),
                mode=("--preview", f"buildings/{name}"),
                label="preview engine (buildings)")
    try:
        meta = building_yaml(name)
        expected = expected_building_entries(name)
        want_default = built_default_label(name)
        d = poll_state(port, "ready")

        ok_mode = check("mode == building (no placeholder remains)",
                        d.get("mode") == "building", d.get("mode"))

        # Requirement 1: the FULL ordered entry list — animation
        # subdirectories AND loose statics together — cross-checked
        # against the filesystem+YAML.
        ok_entries = check("entry list (labels + static/animation identity) "
                           "matches the filesystem-derived expectation "
                           "exactly, in order",
                           dumped_building_entries(d) == expected,
                           f"dumped={dumped_building_entries(d)} "
                           f"expected={expected}")

        ok_default = check("default selection is the state_animations.built "
                           "animation's DIRECTORY, not its YAML name",
                           want_default is not None
                           and d.get("defaultEntry") == want_default
                           and (d.get("selected") or {}).get("label") == want_default,
                           f"defaultEntry={d.get('defaultEntry')} "
                           f"selected={d.get('selected')} want={want_default} "
                           f"(yaml animation name={meta['built']!r})")

        # Requirement 1: playback metadata comes from that YAML entry.
        pb = d.get("playback") or {}
        want_meta = meta["anims"].get(meta["built"] or "", None)
        ok_meta = check("effective fps/loop match data/buildings/"
                        f"{name}.yaml",
                        want_meta is not None
                        and abs((pb.get("fps") or 0) - want_meta["fps"]) < 1e-6
                        and pb.get("loop") == want_meta["loop"],
                        f"dump=({pb.get('fps')}, {pb.get('loop')}) yaml={want_meta}")

        # The frame index advances over WALL time. NB the 1-tuple:
        # poll_until returns on a TRUTHY value and frame 0 is falsy.
        before = ((dump(port).get("playback") or {}).get("frameIndex"))
        after = poll_until(6.0, lambda: (
            (lambda i: (i,) if i != before else None)(
                (dump(port).get("playback") or {}).get("frameIndex"))))
        ok_advance = check("frame index advances over wall time",
                           after is not None and after[0] != before,
                           f"before={before} after={after}")

        # Resize: the selected entry and scroll offset both survive.
        pre = dump(port)
        pre_bounds = pre.get("panelBounds") or {}
        pre_selected = (pre.get("selected") or {}).get("label")
        pre_scroll = pre.get("scrollOffset")
        win_w, win_h = window_size(port)
        send(port, f"return engine.setResolution({win_w + 200}, {win_h + 150})",
             timeout=10.0)
        post = poll_until(10.0, lambda: (
            (lambda s: s if (s.get("panelBounds") or {}) != pre_bounds else None)(
                dump(port)))) or dump(port)
        ok_resize = check("panel bounds reflow while the selection and scroll "
                          "offset survive a resize",
                          (post.get("panelBounds") or {}) != pre_bounds
                          and (post.get("selected") or {}).get("label") == pre_selected
                          and post.get("scrollOffset") == pre_scroll,
                          f"before=({pre_selected}, {pre_scroll}) "
                          f"after=({(post.get('selected') or {}).get('label')}, "
                          f"{post.get('scrollOffset')})")

        # Requirement 1: selecting a STATIC row (located from the dump,
        # never a hardcoded coordinate) switches to it — and a static
        # selection exposes NO playback at all, which is exactly what
        # distinguishes it from an animation entry.
        statics = {label for label, animated in expected if not animated}
        row = next((r for r in (post.get("rows") or [])
                    if r.get("label") in statics), None)
        if row is None:
            ok_static = check("clicking a static row selects it with no playback",
                              False, "no visible static row to click")
        else:
            click_element(port, row.get("bounds") or {})
            after_click = poll_until(10.0, lambda: (
                (lambda s: s if (s.get("selected") or {}).get("label")
                    == row["label"] and s.get("state") == "ready" else None)(
                        dump(port)))) or dump(port)
            ok_static = check("clicking a static row (via row bounds, not "
                              "hardcoded coords) selects it, resolves, and "
                              "exposes no playback",
                              (after_click.get("selected") or {}).get("label")
                              == row["label"]
                              and after_click.get("state") == "ready"
                              and after_click.get("playback") is None,
                              f"selected={after_click.get('selected')} "
                              f"state={after_click.get('state')} "
                              f"playback={after_click.get('playback')}")

        # #1833: forced continuous replay, on the buildings half. The
        # portal's `appear` is 16 frames at 8 fps — a 2.0 s cycle, the
        # widest tracked `loop: false` margin — and buildings DEFAULT to
        # loop=false, so this is the half where holding the end bit
        # hardest. Placed after the behavioural checks above and reading
        # its rows from a FRESH dump, because those may have scrolled or
        # reselected — and because it deliberately ends on an animation
        # rather than the static selection ok_static needs.
        cur = dump(port)
        anim_loops = {e.get("label"): e.get("loop")
                      for e in (cur.get("entries") or [])
                      if e.get("animated") is True}
        nonloop = next((r for r in (cur.get("rows") or [])
                        if anim_loops.get(r.get("label")) is False), None)
        if nonloop is None:
            ok_replay = check("a loop:false animation entry replays "
                              "continuously", False,
                              "no visible loop:false animation row to click")
        else:
            click_element(port, nonloop.get("bounds") or {})
            got = poll_until(10.0, lambda: (
                (lambda s: s if (s.get("playback") or {}).get("entry")
                    == nonloop["label"] and s.get("state") == "ready" else None)(
                        dump(port))))
            selected_at = time.monotonic()
            pb2 = (got or dump(port)).get("playback") or {}
            # Requirement 6: the dump still reports the AUTHORED value —
            # without this the replay check would also pass on an
            # implementation that forced every entry to loop=true.
            ok_truthful = check("the replay fixture still reports its authored "
                                "loop=false (or this check proves nothing)",
                                pb2.get("entry") == nonloop["label"]
                                and pb2.get("loop") is False,
                                f"entry={pb2.get('entry')} loop={pb2.get('loop')}")
            ok_replay = check_forced_replay(
                port, f"buildings/{name} {nonloop['label']} (authored loop=false)",
                selected_at, pb2.get("frameCount"), pb2.get("fps")) \
                and ok_truthful

        # Requirement 1: only THIS building's textures (plus list chrome).
        root_prefix = os.path.join("assets", "textures", "buildings", name) + os.sep
        ok_trimmed = check_trimmed_loading(port, root_prefix, allow_chrome=True)
        ok_no_gameplay = check_no_gameplay_scripts_loaded(port)

        return all([ok_mode, ok_entries, ok_default, ok_meta, ok_advance,
                    ok_resize, ok_replay, ok_static, ok_trimmed,
                    ok_no_gameplay])
    finally:
        quit_engine(port, proc)


def check_buildings_without_built(port: int) -> bool:
    print("6. building with no state_animations.built "
          "(--preview buildings/cargo_hold_S): sprite fallback + "
          "convention-recognized animation")
    name = "cargo_hold_S"
    proc = boot(port, log=LOGS.allocate(f"6. buildings/{name}"),
                mode=("--preview", f"buildings/{name}"),
                label="preview engine (buildings, no built state)")
    try:
        meta = building_yaml(name)
        expected = expected_building_entries(name)
        ok_fixture = check("the fixture really declares no built state (or "
                           "this check proves nothing)",
                           meta["built"] is None and meta["sprite"] is not None,
                           f"built={meta['built']} sprite={meta['sprite']}")
        d = poll_state(port, "ready")
        ok_entries = check("entry list matches the filesystem-derived "
                           "expectation exactly, in order",
                           dumped_building_entries(d) == expected,
                           f"dumped={dumped_building_entries(d)} "
                           f"expected={expected}")
        ok_default = check("default selection falls back to the YAML's own "
                           "sprite",
                           d.get("defaultEntry")
                           == os.path.basename(meta["sprite"] or ""),
                           f"defaultEntry={d.get('defaultEntry')} "
                           f"sprite={meta['sprite']}")

        # demolish/ is a real numbered-frame folder the YAML never
        # mentions: recognized by convention, with the documented
        # building defaults (fps 8, loop FALSE — not the units viewer's
        # loop=true).
        by_label = {e.get("label"): e for e in (d.get("entries") or [])}
        demolish = by_label.get("demolish")
        ok_convention = check("a YAML-less numbered-frame directory is still "
                              "an animation, with fps=8 / loop=false",
                              demolish is not None
                              and demolish.get("animated") is True
                              and abs((demolish.get("fps") or 0) - 8.0) < 1e-6
                              and demolish.get("loop") is False,
                              demolish)

        root_prefix = os.path.join("assets", "textures", "buildings", name) + os.sep
        ok_trimmed = check_trimmed_loading(port, root_prefix, allow_chrome=True)
        return all([ok_fixture, ok_entries, ok_default, ok_convention, ok_trimmed])
    finally:
        quit_engine(port, proc)


def check_buildings_without_yaml(port: int) -> bool:
    print("7. building with NO data/buildings YAML (--preview buildings/dungeon_1): "
          "first-entry default, nested statics")
    name = "dungeon_1"
    proc = boot(port, log=LOGS.allocate(f"7. buildings/{name}"),
                mode=("--preview", f"buildings/{name}"),
                label="preview engine (buildings, no yaml)")
    try:
        ok_fixture = check("the fixture really has no building YAML (or this "
                           "check proves nothing)",
                           not os.path.exists(os.path.join("data", "buildings",
                                                           name + ".yaml")))
        expected = expected_building_entries(name)
        d = poll_state(port, "ready")
        ok_entries = check("entry list matches the filesystem-derived "
                           "expectation exactly, in order",
                           dumped_building_entries(d) == expected,
                           f"dumped={dumped_building_entries(d)} "
                           f"expected={expected}")
        # damaged/ holds piece sprites, not frames: it must surface as
        # item-relative statics, never as one animation entry.
        labels = [label for label, _ in dumped_building_entries(d)]
        ok_nested = check("the damaged/ subtree surfaces as ordinary "
                          "item-relative statics, not one animation",
                          "damaged" not in labels
                          and any(l.startswith("damaged/") for l in labels)
                          and all(not animated
                                  for _, animated in dumped_building_entries(d)),
                          labels)
        ok_default = check("default falls all the way through to the first entry",
                           d.get("defaultEntry") == (expected[0][0] if expected else "")
                           and (d.get("selected") or {}).get("label")
                               == (expected[0][0] if expected else None),
                           f"defaultEntry={d.get('defaultEntry')} "
                           f"selected={d.get('selected')}")
        ok_no_playback = check("a static selection exposes no playback",
                               d.get("playback") is None, d.get("playback"))
        root_prefix = os.path.join("assets", "textures", "buildings", name) + os.sep
        ok_trimmed = check_trimmed_loading(port, root_prefix, allow_chrome=True)
        return all([ok_fixture, ok_entries, ok_nested, ok_default,
                    ok_no_playback, ok_trimmed])
    finally:
        quit_engine(port, proc)


def first_item(category: str) -> str:
    """The first real item directory of a grouped category — derived,
    not hardcoded, so renaming an asset folder can't silently turn this
    into a pre-boot rejection check."""
    root = os.path.join("assets", "textures", category)
    return sorted(d for d in os.listdir(root)
                  if os.path.isdir(os.path.join(root, d))
                  and not os.path.islink(os.path.join(root, d)))[0]


def check_flat_grouped_item(port: int, category: str, item: str) -> bool:
    """#888 Requirement 2: flora and structures item folders are flat
    sets of static PNGs, so they are ROUTED into #886's simple-category
    browser rooted at the item's own folder rather than given viewers of
    their own. This is a dispatch-level check by design — the browsing
    behavior itself is already gated by check 1."""
    proc = boot(port, log=LOGS.allocate(f"8. {category}/{item}"),
                mode=("--preview", f"{category}/{item}"),
                label=f"preview engine ({category}/{item})")
    try:
        root = os.path.join("assets", "textures", category, item)
        expected = expected_entries_at(root)
        d = poll_state(port, "ready")
        ok_mode = check(f"{category}/{item}: mode == list (the shared simple "
                        "browser, rooted at the item folder)",
                        d.get("mode") == "list", d.get("mode"))
        listed = [e.get("label") for e in (d.get("entries") or [])]
        ok_entries = check(f"{category}/{item}: the item folder's own textures, "
                           "in order",
                           listed == expected,
                           f"dumped={listed} expected={expected}")
        ok_first = check(f"{category}/{item}: first entry auto-selected and "
                         "resolved",
                         (d.get("selected") or {}).get("label")
                         == (expected[0] if expected else None)
                         and d.get("state") == "ready",
                         f"selected={d.get('selected')} state={d.get('state')}")
        ok_trimmed = check_trimmed_loading(port, root + os.sep, allow_chrome=True)
        ok_no_gameplay = check_no_gameplay_scripts_loaded(port)
        return all([ok_mode, ok_entries, ok_first, ok_trimmed, ok_no_gameplay])
    finally:
        quit_engine(port, proc)


def check_flat_grouped_dispatch(port: int) -> bool:
    print("8. flora/structures items reuse the simple-category browser")
    return all([check_flat_grouped_item(port, "flora", first_item("flora")),
                check_flat_grouped_item(port, "structures", "wire")])


def check_canonical_dispatch_sweep(port: int) -> bool:
    """The epic (#427) acceptance sweep: EVERY canonical category
    dispatches to its documented behavior, and the Phase 1 (#632)
    placeholder mode no longer exists anywhere."""
    print("9. canonical dispatch sweep: every category, no placeholder left")
    targets = [
        ("icons", "list"), ("items", "list"), ("ui", "list"), ("world", "list"),
        ("units/acolyte", "unit"),
        (f"flora/{first_item('flora')}", "list"),
        ("buildings/workbench", "building"),
        ("structures/wire", "list"),
    ]
    results = []
    for target, want_mode in targets:
        proc = boot(port, log=LOGS.allocate(f"9. sweep {target}"),
                    mode=("--preview", target),
                    label=f"preview engine (sweep {target})")
        try:
            d = poll_until(20.0, lambda: (
                (lambda s: s if s.get("mode") else None)(dump(port)))) or dump(port)
            mode = d.get("mode")
            results.append(check(f"--preview {target} dispatches to mode="
                                 f"{want_mode}",
                                 mode == want_mode and mode != "placeholder",
                                 f"mode={mode}"))
        finally:
            quit_engine(port, proc)
    return all(results)


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
        proc = boot(port, log=LOGS.allocate(f"11. zoom {target}"),
                    mode=("--preview", target),
                    label=f"preview engine (zoom {target})")
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


def main() -> int:
    ap = argparse.ArgumentParser(description=__doc__)
    ap.add_argument("--port", type=int, default=9150)
    args = ap.parse_args()

    # Keep the real GLFW/Vulkan/swapchain path (including live window
    # resizing) without repeatedly taking keyboard focus from the developer.
    # The variable is inherited only by this probe's engine subprocesses.
    os.environ["SYNARCHY_PREVIEW_HIDDEN"] = "1"

    # The summary runs in a `finally` because the interesting runs are
    # the ones that do not reach the end: `probelib.boot` raises
    # SystemExit when an engine dies before READY, and the log named
    # here is what says why.
    try:
        results = [
            check_simple_list_mode(args.port),
            check_focused_item_mode(args.port),
            check_units_mode(args.port),
            check_units_promoted(args.port),
            check_units_roster(args.port),
            check_buildings_mode(args.port),
            check_buildings_without_built(args.port),
            check_buildings_without_yaml(args.port),
            check_flat_grouped_dispatch(args.port),
            check_canonical_dispatch_sweep(args.port),
            check_zoom(args.port),
        ]
    finally:
        LOGS.report()

    passed = all(results)
    print(f"\n  {'PASS' if passed else 'FAIL'}: --preview real-boot browser"
          + ("" if passed else " — see failures above"))
    return 0 if passed else 1


if __name__ == "__main__":
    sys.exit(main())
