#!/usr/bin/env python3
"""--preview real-boot browser probe (#632 Phase 1, #886 Phase 2, #887 Phase 3).

Needs a GPU (a real GLFW window — --preview has no offscreen variant) —
manual-only, never CI-gated (see tools/preview_cli_probe.py for the
no-GPU CLI-contract checks this probe used to also carry, split out in
#886 so a classifier/path-containment regression fails PRs directly
instead of waiting for a manual dev-machine run).

Checks:
  1. Boot profile + preview target over the debug console
     (engine.getBootProfile/getPreviewTarget) on a still-placeholder
     grouped category ("flora/example") — the Phase 1 (#632)
     placeholder-label boot path, which #888 will replace.
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
  4. Unit animation viewer (--preview units/acolyte, #887): the
     animation list matches a filesystem-derived expectation exactly and
     in order; the default selection is idle/south; the known YAML
     fps/loop values for the selected clip are reported; the frame index
     advances over wall time; clicking a DIFFERENT list row (located
     from the dump, never hardcoded coordinates) changes the selected
     animation and restarts the clip; clicking a mirrored direction cell
     (located the same way) enlarges it and reports its real source
     direction; a resize preserves the animation, direction, and scroll
     offset; only the requested unit's textures load.
  5. Missing-YAML defaults (--preview units/tiller, #887): a unit with
     no data/units/<name>.yaml at all still browses, with fps=8 /
     loop=true on every animation and all eight direction cells
     populated by the inferred five-direction mirroring.
  6. Trimmed loading (Requirement 5): engine.getLoadedTexturePaths() —
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
    # A still-placeholder grouped category (#888 replaces it) — units is
    # the real viewer as of #887 and is covered by check 4 below.
    print("1. grouped+item real boot: boot profile + preview target (Phase 1 placeholder)")
    proc = boot(port, log=LOG, mode=("--preview", "flora/example"),
                label="preview engine (grouped placeholder)")
    try:
        profile = send(port, "return engine.getBootProfile()")
        ok1 = check("boot profile == preview", profile == "preview", profile)

        target = send_json(port, "return engine.getPreviewTarget()")
        ok2 = check(
            "preview target == flora/example",
            isinstance(target, dict)
            and target.get("category") == "flora"
            and target.get("item") == "example",
            target)

        d = dump(port)
        ok3 = check("mode == placeholder (grouped categories #888 has yet to land)",
                    d.get("mode") == "placeholder", d.get("mode"))
        return ok1 and ok2 and ok3
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

        return all([ok_filter, ok_mode, ok_count, ok_entries, ok_first, ok_ready,
                    ok_click, ok_scroll, ok_resize_bounds, ok_resize_selection,
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
    """Independent, filesystem-derived expectation for the animation
    list — mirrors Engine.Preview.Unit.discoverUnitAnimations's contract
    (direct children of animations/ that hold at least one recognized,
    non-symlinked direction folder with at least one non-symlinked .png,
    case-sensitive lexicographic) without importing any Haskell/Lua
    code."""
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


def check_units_mode(port: int) -> bool:
    print("4. unit animation viewer (--preview units/acolyte)")
    unit = "acolyte"
    proc = boot(port, log=LOG, mode=("--preview", f"units/{unit}"),
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
        send(port, "return engine.setResolution({}, {})".format(
            int(pre_bounds.get("width", 400)) + 260,
            int(pre_bounds.get("height", 300)) + 180), timeout=10.0)
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

        # Requirement 6: only THIS unit's textures (plus list chrome).
        root_prefix = os.path.join("assets", "textures", "units", unit) + os.sep
        ok_trimmed = check_trimmed_loading(port, root_prefix, allow_chrome=True)
        ok_no_gameplay = check_no_gameplay_scripts_loaded(port)

        return all([ok_mode, ok_filter, ok_entries, ok_default, ok_meta,
                    ok_dirs, ok_mirror, ok_advance, ok_select, ok_cell,
                    ok_resize, ok_trimmed, ok_no_gameplay])
    finally:
        quit_engine(port, proc)


def check_units_without_yaml(port: int) -> bool:
    print("5. unit with NO data/units YAML (--preview units/tiller): documented defaults")
    unit = "tiller"
    proc = boot(port, log=LOG, mode=("--preview", f"units/{unit}"),
                label="preview engine (units, no yaml)")
    try:
        ok_no_yaml = check("the fixture really has no unit YAML (or this "
                           "check proves nothing)",
                           not os.path.exists(os.path.join("data", "units",
                                                           unit + ".yaml")))
        d = poll_unit_ready(port)
        expected = expected_unit_animations(unit)
        listed = [e.get("label") for e in (d.get("entries") or [])]
        ok_entries = check("animation list matches the filesystem-derived "
                           "expectation exactly, in order",
                           listed == expected, f"dumped={listed} expected={expected}")

        ok_defaults = check("every animation falls back to fps=8 / loop=true",
                            all(abs((e.get("fps") or 0) - 8.0) < 1e-6
                                and e.get("loop") is True
                                for e in (d.get("entries") or [])),
                            [(e.get("label"), e.get("fps"), e.get("loop"))
                             for e in (d.get("entries") or [])])

        # The inferred five-direction mirroring still populates all
        # eight cells for an asset-only unit.
        pb = d.get("playback") or {}
        dirs = [c.get("direction") for c in (pb.get("directions") or [])]
        ok_dirs = check("inferred mirroring still populates all eight cells",
                        dirs == GAME_DIRECTION_ORDER, dirs)

        root_prefix = os.path.join("assets", "textures", "units", unit) + os.sep
        ok_trimmed = check_trimmed_loading(port, root_prefix, allow_chrome=True)
        ok_no_gameplay = check_no_gameplay_scripts_loaded(port)

        return all([ok_no_yaml, ok_entries, ok_defaults, ok_dirs,
                    ok_trimmed, ok_no_gameplay])
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
        check_units_mode(args.port),
        check_units_without_yaml(args.port),
    ]

    passed = all(results)
    print(f"\n  {'PASS' if passed else 'FAIL'}: --preview real-boot browser"
          + ("" if passed else " — see failures above"))
    return 0 if passed else 1


if __name__ == "__main__":
    sys.exit(main())
