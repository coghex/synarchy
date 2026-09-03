#!/usr/bin/env python3
"""--preview real-boot browser probe (#886 Phase 2, #887 Phase 3, #888 Phase 4,
#1907 zoom; split into family owners by #2089).

Needs a GPU and a real GLFW surface/swapchain. The probe sets
SYNARCHY_PREVIEW_HIDDEN=1, so those windows stay hidden and non-activating
while preserving the windowed resize path an offscreen boot would bypass.
It is manual-only, never CI-gated (see tools/preview_cli_probe.py for the
no-GPU CLI-contract checks this probe used to also carry, split out in #886
so a classifier/path-containment regression fails PRs directly instead of
waiting for a manual dev-machine run).

Every check boots its own engine, so the whole run creates (and closes) a
hidden window per target — one per fixed phase, one per shipped unit, one
per swept category and one per zoomed display kind, a few minutes end to
end. Each of those boots writes its OWN log (#1763): the path is printed
when it is allocated and again in a closing summary that maps every log
back to the phase and target that wrote it, so a failing boot's output is
still there afterwards.

This file is the facade: CLI parsing, the `FAMILIES` inventory, family
dispatch, the closing log summary and the one aggregate exit. Every
scenario body lives in an owner under `tools/preview/` (see that
package's docstring), and the shared mechanics — engine launch and
per-boot logs, polling and dumps, input and geometry helpers, the
filesystem/YAML/atlas expectations, the texture-loading check — live in
`tools/preview/harness.py` and nowhere else. The families:

  simple     phases 1-2    bare simple-category list, focused item
  units      phases 3-4b   acolyte viewer, promoted tiller, the roster
  buildings  phases 5-7    built-state, no-built-state, YAML-free
  dispatch   phases 8-9    grouped flora/structure items, category sweep
  zoom       phase 11      all six display kinds

`--only <family>` runs just that family's boots; with no selector the
run is every scenario exactly once, in the inventory's order.

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
 10. Trimmed loading (Requirement 5, a shared per-phase helper rather
     than a boot of its own): engine.getLoadedTexturePaths() —
     the engine's OWN authoritative record of every texture ever loaded
     this session (Engine.Asset's apAssetPaths, populated by
     engine.loadTexture's Haskell handler itself, not any Lua caller's
     self-reported bookkeeping) — contains ONLY paths under the browsed
     category's root plus the documented chrome allowlist (list mode
     only), with no extras and nothing missing; the normal ~25-script
     gameplay set never loaded (the `ui` global, wired only outside the
     preview boot profile, stays nil).
 11. Centered bounded zoom (#1907), one boot per display kind (bare
     list, focused item, unit enlarged, building, flora item, structure
     item), driving the REAL wheel pipeline with input.moveMouse +
     input.scroll over the dump-reported zoom REGION rather than any
     hardcoded coordinate: a session starts at multiplier 1, delta
     MAGNITUDE is honoured rather than reduced to a sign, both limits
     clamp exactly at 1/8 and 1 with further input consumed and stable,
     the rendered dimensions are one eighth of the fitted ones at the
     floor and exactly the fitted ones back at the ceiling, the sprite
     stays centered and wholly inside its region throughout, plain and
     a really-held-Shift wheel behave identically, a wheel over a
     located list row moves the list and never the zoom while a wheel
     over the pane moves the zoom and never the list (including once
     saturated), the object-identity rule resets on a different
     BARE-category texture and preserves for a unit animation /
     building entry / flora piece, and a resize preserves the
     multiplier while recomputing the region.

Usage:
  python3 tools/preview_probe.py [--port 9150]
      [--only {simple,units,buildings,dispatch,zoom}]

Exit 0 = all checks passed.
"""
from __future__ import annotations

import argparse
import os
import sys
from types import ModuleType
from typing import NamedTuple

from preview import buildings, dispatch, harness, simple, units, zoom


class Family(NamedTuple):
    """One independently runnable family: its `--only` name, the module
    that owns its scenario bodies, and those scenarios' attribute names
    in the order the family runs them."""
    name: str
    module: ModuleType
    scenarios: tuple[str, ...]


#: THE inventory (#2089 requirement 5): every scenario belongs to exactly
#: one family and appears exactly once, and the aggregate run is this
#: tuple flattened in order — the same order the pre-split `main` ran.
#: Both the default run and `--only` read from here and nowhere else.
#: Scenarios are named, not bound, so each is resolved on its owner at
#: run time (which is also what lets a GPU-free test intercept one).
FAMILIES: tuple[Family, ...] = (
    Family("simple", simple,
           ("check_simple_list_mode", "check_focused_item_mode")),
    Family("units", units,
           ("check_units_mode", "check_units_promoted", "check_units_roster")),
    Family("buildings", buildings,
           ("check_buildings_mode", "check_buildings_without_built",
            "check_buildings_without_yaml")),
    Family("dispatch", dispatch,
           ("check_flat_grouped_dispatch", "check_canonical_dispatch_sweep")),
    Family("zoom", zoom,
           ("check_zoom",)),
)

FAMILY_NAMES: tuple[str, ...] = tuple(family.name for family in FAMILIES)


def selected_families(only: str | None) -> tuple[Family, ...]:
    """The families a run executes: all of them, in inventory order, or
    exactly the one `--only` named. An unknown name never reaches here —
    argparse's `choices` rejects it before a log or an engine exists."""
    if only is None:
        return FAMILIES
    chosen = tuple(family for family in FAMILIES if family.name == only)
    if not chosen:
        raise ValueError(f"unknown preview family {only!r}; "
                         f"choose from {', '.join(FAMILY_NAMES)}")
    return chosen


def run_family(family: Family, port: int) -> list[bool]:
    """Run one family's scenarios in order, each against a fresh boot of
    its own, collecting every result rather than stopping at the first
    failure (an engine that dies before READY still exits the probe)."""
    return [getattr(family.module, scenario)(port)
            for scenario in family.scenarios]


def main(argv: list[str] | None = None) -> int:
    ap = argparse.ArgumentParser(description=__doc__)
    ap.add_argument("--port", type=int, default=9150)
    ap.add_argument("--only", choices=FAMILY_NAMES, default=None,
                    help="run only this family's scenarios (default: every "
                         "family, once, in order)")
    args = ap.parse_args(argv)

    # Keep the real GLFW/Vulkan/swapchain path (including live window
    # resizing) without repeatedly taking keyboard focus from the developer.
    # The variable is inherited only by this probe's engine subprocesses.
    os.environ["SYNARCHY_PREVIEW_HIDDEN"] = "1"

    # The summary runs in a `finally` because the interesting runs are
    # the ones that do not reach the end: `probelib.boot` raises
    # SystemExit when an engine dies before READY, and the log named
    # here is what says why.
    results: list[bool] = []
    try:
        for family in selected_families(args.only):
            results.extend(run_family(family, args.port))
    finally:
        harness.LOGS.report()

    passed = all(results)
    scope = "" if args.only is None else f" [--only {args.only}]"
    print(f"\n  {'PASS' if passed else 'FAIL'}: --preview real-boot browser"
          f"{scope}" + ("" if passed else " — see failures above"))
    return 0 if passed else 1


if __name__ == "__main__":
    sys.exit(main())
