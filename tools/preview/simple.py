"""The `simple` family: bare simple-category list mode and focused-item
mode (#886, real keyboard input since #2026).

Two scenarios, one fresh hidden boot each:

  * `check_simple_list_mode` — phase `1.`, `--preview icons`.
  * `check_focused_item_mode` — phase `2.`, `--preview icons/<item>`.

A library, not a probe: registered nowhere, runnable only through the
facade's inventory (`python3 tools/preview_probe.py --only simple`).
"""
from __future__ import annotations

import os
import time
from probelib import quit_engine, send, send_json, poll_until

from .harness import (boot_preview, check, check_no_gameplay_scripts_loaded,
                      check_trimmed_loading, dump, expected_entries,
                      framebuffer_size, hold_preview_key, poll_state,
                      press_preview_key, window_size)

def check_simple_list_mode(port: int) -> bool:
    print("1. boot profile/target + simple-category list mode (--preview icons)")
    proc = boot_preview(port, "1. icons list", "icons",
                        "preview engine (icons list)")
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
    proc = boot_preview(port, f"2. {target}", target,
                        "preview engine (icons item)")
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
