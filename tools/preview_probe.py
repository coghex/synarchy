#!/usr/bin/env python3
"""--preview real-boot browser probe (#632 Phase 1, #886 Phase 2).

Needs a GPU (a real GLFW window — --preview has no offscreen variant) —
manual-only, never CI-gated (see tools/preview_cli_probe.py for the
no-GPU CLI-contract checks this probe used to also carry, split out in
#886 so a classifier/path-containment regression fails PRs directly
instead of waiting for a manual dev-machine run).

Checks:
  1. Boot profile + preview target over the debug console
     (engine.getBootProfile/getPreviewTarget), grouped+item form
     ("units/acolyte") — the Phase 1 (#632) placeholder-label boot path,
     unaffected by #886.
  2. Simple-category list mode (--preview icons): the texture filter is
     forced to nearest regardless of the persisted video config; the
     discovered entry list (require("scripts.preview_manager").dump())
     matches a filesystem-derived expectation computed independently in
     this probe; the first entry is auto-selected and its texture
     resolves to "ready"; clicking a DIFFERENT row (located from the
     dump's per-row interactive bounds — never hardcoded coordinates,
     the offscreen_probe.py convention) changes the selection; scrolling
     over the list (input.moveMouse + input.scroll, located the same
     way) changes the reported scroll offset; a framebuffer GROW
     (engine.setResolution) reflows the panel bounds while preserving
     the current selection and scroll offset, and at every size the
     visible row count actually fits the reported panel height (no
     overflow); a subsequent SHRINK well below the list's natural row
     budget reduces the visible row count and still fits.
  3. Focused item mode (--preview icons/<item>): texture filter forced
     to nearest; no list (dump().rows is absent/empty) while the
     requested texture resolves; a resize reflows the panel bounds.
  4. Trimmed loading (Requirement 5): engine.getLoadedTexturePaths() —
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
import os
import sys
import time
from probelib import boot, quit_engine, send, send_json, poll_until

LOG = "/tmp/preview_probe_engine.log"

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


def check_no_gameplay_scripts_loaded(port: int) -> bool:
    """The normal ~25-script gameplay/menu set (init_loader.lua's
    non-preview branch) never loads in preview mode — the `ui` global it
    wires (require("scripts.ui.registry")) is the cheapest sentinel:
    nil here means that whole branch never ran."""
    result = send(port, "return ui == nil")
    return check("normal gameplay script set never loaded (ui global is nil)",
                 result == "true", result)


def expected_entries(category: str) -> list[str]:
    """Independent, filesystem-derived expectation — mirrors
    Engine.Preview.Discovery.discoverEntries's contract (recursive,
    .png only, "/"-joined, sorted) without importing any Haskell/Lua
    code, so this actually cross-checks the real discovery behavior
    rather than restating it."""
    root = os.path.join("assets", "textures", category)
    labels = []
    for dirpath, _dirs, files in os.walk(root):
        rel = os.path.relpath(dirpath, root)
        for f in files:
            if f.lower().endswith(".png"):
                label = f if rel == "." else f"{rel.replace(os.sep, '/')}/{f}"
                labels.append(label)
    return sorted(labels)


def check_grouped_real_boot(port: int) -> bool:
    print("1. grouped+item real boot: boot profile + preview target (Phase 1 placeholder)")
    proc = boot(port, log=LOG, mode=("--preview", "units/acolyte"),
                label="preview engine (grouped)")
    try:
        profile = send(port, "return engine.getBootProfile()")
        ok1 = check("boot profile == preview", profile == "preview", profile)

        target = send_json(port, "return engine.getPreviewTarget()")
        ok2 = check(
            "preview target == units/acolyte",
            isinstance(target, dict)
            and target.get("category") == "units"
            and target.get("item") == "acolyte",
            target)
        return ok1 and ok2
    finally:
        quit_engine(port, proc)


def check_simple_list_mode(port: int) -> bool:
    print("2. simple-category list mode (--preview icons)")
    proc = boot(port, log=LOG, mode=("--preview", "icons"),
                label="preview engine (icons list)")
    try:
        expected = expected_entries("icons")
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

        selected = d.get("selected") or {}
        ok_first = check("first entry auto-selected",
                         selected.get("label") == (expected[0] if expected else None),
                         selected)
        ok_ready = check("selection resolved to ready", d.get("state") == "ready", d.get("state"))

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
            send(port, "return input.scroll(0, -3)", timeout=10.0)
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
        new_w = int(prev_bounds.get("width", 400)) + 300
        new_h = int(prev_bounds.get("height", 300)) + 200
        send(port, f"return engine.setResolution({new_w}, {new_h})", timeout=10.0)
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
        shrink_w = int(prev_bounds.get("width", 400))
        shrink_h = max(200, int((prev_h or 400) * 0.5))
        send(port, f"return engine.setResolution({shrink_w}, {shrink_h})", timeout=10.0)
        after_shrink = poll_until(
            10.0, lambda: (dump(port).get("panelBounds") or {}).get("height") != prev_h
                and dump(port))
        after_shrink = after_shrink or dump(port)
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

        return all([ok_filter, ok_mode, ok_count, ok_first, ok_ready, ok_click,
                    ok_scroll, ok_resize_bounds, ok_resize_selection,
                    ok_resize_scroll, ok_grow_fit, ok_shrink_rows, ok_shrink_fit,
                    ok_trimmed, ok_no_gameplay])
    finally:
        quit_engine(port, proc)


def check_focused_item_mode(port: int) -> bool:
    print("3. focused item mode (--preview icons/skill/climbing.png): no list")
    target = "icons/skill/climbing.png"
    proc = boot(port, log=LOG, mode=("--preview", target),
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
        new_w = int(prev_bounds.get("width", 400)) + 300
        new_h = int(prev_bounds.get("height", 300)) + 200
        send(port, f"return engine.setResolution({new_w}, {new_h})", timeout=10.0)
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


def main() -> int:
    ap = argparse.ArgumentParser(description=__doc__)
    ap.add_argument("--port", type=int, default=9150)
    args = ap.parse_args()

    results = [
        check_grouped_real_boot(args.port),
        check_simple_list_mode(args.port),
        check_focused_item_mode(args.port),
    ]

    passed = all(results)
    print(f"\n  {'PASS' if passed else 'FAIL'}: --preview real-boot browser"
          + ("" if passed else " — see failures above"))
    return 0 if passed else 1


if __name__ == "__main__":
    sys.exit(main())
