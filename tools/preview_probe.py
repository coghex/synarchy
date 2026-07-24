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
     way) changes the reported scroll offset; a framebuffer resize
     (engine.setResolution) reflows the panel bounds while preserving
     the current selection and scroll offset.
  3. Focused item mode (--preview icons/<item>): texture filter forced
     to nearest; no list (dump().rows is absent/empty) while the
     requested texture resolves; a resize reflows the panel bounds.
  4. Trimmed loading (Requirement 5): the engine's OWN authoritative
     texture-upload count (blood.gpuStats().texSize) matches EXACTLY
     the browsed category's own requested textures plus the documented
     chrome allowlist (list mode only) — not just previewManager's
     self-reported bookkeeping — and every self-reported path resolves
     under the browsed category's own root.

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

        # Trimmed loading (Requirement 5): cross-check against the
        # engine's OWN authoritative texture-upload count
        # (blood.gpuStats already exposes texSize, the live
        # textureSizeRef entry count — an engine-side ground truth, not
        # self-reported by previewManager) so an unrelated/hidden
        # texture load anywhere in the engine shows up as a count
        # mismatch here, not just a gap in previewManager's own
        # bookkeeping (#886 round-1 review).
        dfinal = dump(port)
        loaded = dfinal.get("loadedPaths") or []
        root_prefix = os.path.join("assets", "textures", "icons") + os.sep
        ok_paths = check("every requested texture path stays under the "
                        "browsed category's root",
                        all(p.startswith(root_prefix) for p in loaded),
                        loaded)

        gpu = send_json(port, "return blood.gpuStats()")
        tex_size = gpu.get("texSize") if isinstance(gpu, dict) else None
        # List mode always loads scripts.ui.list's 6 chrome textures
        # (highlight.png + 5 scrollbar assets) the moment ANY list is
        # built — list.init()/scrollbar.init() are unconditional,
        # regardless of whether this particular list needs to scroll —
        # so the engine-wide total must be EXACTLY chrome +
        # previewManager's own tracked requests, no slack for anything
        # else (a gameplay catalog, a stray world/HUD texture, ...) to
        # have snuck in.
        CHROME_TEXTURE_COUNT = 6
        expected_tex_size = CHROME_TEXTURE_COUNT + len(loaded)
        ok_trimmed = check("engine-wide loaded-texture count is EXACTLY "
                          "chrome + the browsed category's own requests "
                          "(no unrelated/hidden texture load)",
                          tex_size == expected_tex_size,
                          f"texSize={tex_size} expected={expected_tex_size} "
                          f"(chrome={CHROME_TEXTURE_COUNT} loaded={len(loaded)})")

        return all([ok_filter, ok_mode, ok_count, ok_first, ok_ready, ok_click,
                    ok_scroll, ok_resize_bounds, ok_resize_selection,
                    ok_resize_scroll, ok_paths, ok_trimmed])
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

        # Trimmed loading: focused mode never calls assetBrowser.init(),
        # so no list chrome loads at all — the engine-wide texture count
        # must be EXACTLY the one requested texture (see the list-mode
        # check above for why this is the engine's own ground truth,
        # not previewManager's self-reported loadedPaths).
        loaded = d.get("loadedPaths") or []
        gpu = send_json(port, "return blood.gpuStats()")
        tex_size = gpu.get("texSize") if isinstance(gpu, dict) else None
        ok_trimmed = check("engine-wide loaded-texture count == exactly "
                          "the one requested texture (no list chrome, "
                          "no unrelated/hidden texture load)",
                          tex_size == len(loaded),
                          f"texSize={tex_size} loaded={loaded}")

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
                and ok_ready and ok_trimmed and ok_resize)
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
