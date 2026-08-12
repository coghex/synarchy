#!/usr/bin/env python3
"""Offscreen probe for the shared item-list widget (#1088, epic #1013
phase C0).

Drives the REAL production path for all three migrated hosts — real
clicks on real Vulkan-rendered tab boxes and rows, every target located
through `ui.dumpWidgets()` (never a hardcoded screen coordinate), plus a
real framebuffer resize — and reads the result back through the widget's
own dump records. `--offscreen` (GPU on, window off) is required: the
real HUD/panel flow never boots headless (it gates on `fontsReady`, a
GPU font atlas), and `--headless` refuses `input.*` injection outright.

The cargo panel is reached by a real right-click on the building when
`building.hitTestAt` can locate its screen pixel, and otherwise through
the panel's own public entry point — that route into the panel is
tools/transfer_context_menu_probe.py's gate, whereas everything #1088
changed lives INSIDE the panel and is checked through the real UI either
way. (On the runs used to build this probe the runtime-registered
throwaway building def never resolved through `building.hitTestAt` at
any pixel, which is a pre-existing offscreen hit-test condition unrelated
to this widget.)

The widget's rows are reported by `scripts/ui/item_list.lua`'s own
`dump()`, aggregated by `scripts/ui/registry.lua` — which matters for
the item-contents panel specifically: those rows deliberately register
NO click callback, and registry.lua's generic fallback pass only reports
elements that HAVE one. Without the widget's own dump those rows would
be invisible to this probe and "rows and counts" unverifiable there.

Registers a throwaway 1x1 storage fixture (`build_work: 0.0`, positive
`storage_capacity`) rather than spawning the shipped `cargo_hold_S`:
that def's real `build_work` (240s, worker-driven) would leave a
`building.spawn`ed instance stuck "appearing" forever with no
construct_job AI running. Mirrors tools/transfer_context_menu_probe.py's
own throwaway-def technique.

Since #1234 the cargo panel is an endpoint-kind agnostic CONTAINER
WINDOW, so this probe is also the gate on a building endpoint rendering
exactly as it did before that generalization, and on a unit endpoint
reaching the same window through the same manager.

Verifies, in order:

  1. cargo Contents panel (a BUILDING endpoint): its title and
     capacity/stored-weight header name the real building; the widget
     renders one row per STACK (not per item) with the right counts and
     categories; its tab strip is one shrink-to-fit row of `All` +
     first-appearance categories, entirely inside the panel; clicking a
     category tab filters the rows to it; a framebuffer resize keeps the
     panel open on the SAME endpoint identity and the SAME selected
     category; a rebuild leaks no duplicate rows.
  2. cargo rows route a real right-click to the representative instance:
     the "Withdraw" menu that appears names the row the probe clicked.
  3. a UNIT endpoint (#1234) opens through that same manager: its title
     names the unit, its header reports `transferEndpointInfo`'s own
     capacity and stored weight (which counts equipment and accessories,
     so it is deliberately NOT the rows' summed weight), its rows are
     that call's loose inventory, its tab strip behaves like the
     building's, a live inventory change refreshes it without reopening,
     a resize preserves the endpoint identity AND the selected tab, unit
     rows expose no row action, and a wildlife unit — not
     player-commandable — cannot open the window at all.
  4. first-aid-kit Contents panel: the Haskell-side pre-grouped rows
     appear unchanged (never re-split by the finer stack key), the rows
     expose NO right-click action at all, and an emptied container
     renders its "(empty)" state.
  5. unit inventory section: rows and counts, a wrapped/centred tab
     strip inside the section rect, tab selection filtering, and a real
     right-click reaching the representative instance's Equip/Contents
     menu.

Manual-only (needs-gpu) unless promoted through `tools/ci_probes.py` per
CLAUDE.md; the CI-blocking gates for this feature are
`cabal test synarchy-test-headless --test-options='--match "Item list
widget"'` plus the two migrated-host describes in
`test-headless/Test/Headless/UI/ResponsiveGameplay.hs`.

Usage: python3 tools/item_list_widget_probe.py
       [--port 9428] [--size 1280x900]
"""
from __future__ import annotations

import argparse
import os
import sys
import tempfile
import time

sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
from probelib import boot, poll_until, quit_engine, send, send_json

SPROOT = tempfile.gettempdir()
TEST_BUILDING_YAML = os.path.join(SPROOT, "item_list_widget_probe_buildings.yaml")
DEF_CARGO = "probe_item_list_cargo"

TEST_BUILDINGS = f"""\
buildings:
  - name: "{DEF_CARGO}"
    display_name: "Probe Item List Cargo"
    category: "Test"
    description: "Throwaway #1088 test fixture — not shipped content."
    sprite: "assets/textures/buildings/cargo_hold_S/default.png"
    tile_size: {{ x: 1, y: 1 }}
    placement: "flat_ground"
    race: "acolyte_cult"
    build_work: 0.0
    storage_capacity: 400.0
"""

# Deliberately spans several categories so the tab strip has something
# to compute, and repeats defs so grouping has something to merge.
# (defName, copies)
CARGO_STOCK = [
    ("steel_bar", 4),
    ("wood_log", 3),
    ("bandage", 2),
    ("quinoa_sack", 1),
]

# Debug-console expressions naming each host's live widget instance.
CARGO_LIST_ID = "require('scripts.cargo_inventory_panel').state.listId"
ITEM_CONTENTS_LIST_ID = "require('scripts.item_contents_panel').state.listId"
UNIT_INV_LIST_ID = "require('scripts.unit_info_v2').invListId"

failures = 0


def check(name: str, ok: bool, detail: str = "") -> bool:
    global failures
    print(f"  [{'PASS' if ok else 'FAIL'}] {name}"
          + (f" — {detail}" if detail and not ok else ""))
    failures += not ok
    return ok


# --------------------------------------------------------------------------
# Widget oracle (F3/#645), never a hardcoded coordinate
# --------------------------------------------------------------------------
def widgets(port: int):
    got = send_json(port, "return ui.dumpWidgets()", timeout=15.0)
    return got if isinstance(got, list) else []


def find_widget(port: int, label: str):
    for w in widgets(port):
        if (w.get("label") or "").strip().lower() == label.lower():
            return w
    return None


def item_rows(port: int, list_id_lua: str):
    """The rendered rows of ONE item-list instance, straight from the
    widget's own dump().

    Scoped by instance on purpose: the unit-info inventory section stays
    on screen behind a floating Contents popup, so an unscoped "every
    item_list row" read would mix two hosts' rows together."""
    raw = send(port, f"local id = {list_id_lua}; return tostring(id)").strip().strip('"')
    if raw in ("", "nil", "null"):
        return []
    prefix = f"item_list:{raw.split('.')[0]}:"
    return [w for w in widgets(port)
            if w.get("type") == "item_list"
            and (w.get("id") or "").startswith(prefix)]


def tab_boxes(port: int, list_id_lua: str):
    """The rendered tab boxes belonging to ONE item-list instance.

    Bounds come from the real `ui.dumpWidgets()` oracle (never a
    computed coordinate); WHICH records belong to this host is resolved
    by intersecting their element handles with the widget's own
    `getTabs()` — tabbar.dump() reports a tab's visible LABEL as its
    `name`, so the engine-side element name is not available there."""
    own = send_json(port, "local il = require('scripts.ui.item_list');"
                          " local out = {};"
                          f" for i, t in ipairs(il.getTabs({list_id_lua})) do"
                          " out[i] = {key = t.key, handle = t.boxId} end;"
                          " return out")
    if not isinstance(own, list) or not own:
        return []
    keys = {e["handle"]: e["key"] for e in own
            if isinstance(e, dict) and "handle" in e}
    out = []
    for w in widgets(port):
        if w.get("type") == "tabbar" and w.get("handle") in keys:
            w = dict(w)
            w["key"] = keys[w["handle"]]
            out.append(w)
    # Same order the strip itself was built in.
    order = [e["handle"] for e in own if isinstance(e, dict)]
    out.sort(key=lambda w: order.index(w["handle"]))
    return out


def panel_chrome(port: int) -> dict:
    """The container window's own host-owned chrome (#1234).

    Read through scripts/ui/label's real accessors on the live element
    ids the panel is holding, so this reports what is actually rendered
    rather than what the probe expected to be rendered."""
    got = send_json(port, "local s = require('scripts.cargo_inventory_panel')"
                          ".state; local l = require('scripts.ui.label');"
                          " return {title = s.titleId and l.getText(s.titleId),"
                          " subtitle = s.subtitleId and l.getText(s.subtitleId)}")
    return got if isinstance(got, dict) else {}


def click_widget_center(port: int, w: dict) -> None:
    b = w.get("bounds") or {}
    x = int(b.get("x", 0) + b.get("w", 0) / 2)
    y = int(b.get("y", 0) + b.get("h", 0) / 2)
    send(port, f"return input.moveMouse({x}, {y})")
    send(port, f"return input.click({x}, {y})")


def right_click_widget_center(port: int, w: dict) -> None:
    b = w.get("bounds") or {}
    x = int(b.get("x", 0) + b.get("w", 0) / 2)
    y = int(b.get("y", 0) + b.get("h", 0) / 2)
    send(port, f"return input.moveMouse({x}, {y})")
    send(port, f"return input.click({x}, {y}, 'right')")


def close_menu(port: int) -> None:
    send(port, "require('scripts.ui.context_menu').hide(); return 'ok'")
    time.sleep(0.2)


# --------------------------------------------------------------------------
# World-tile helpers (same technique as tools/transfer_context_menu_probe.py)
# --------------------------------------------------------------------------
CHUNK_TILES = 16
SEARCH_RADIUS = 60


def tile_surface(port: int, x: int, y: int):
    raw = send(port, f"return world.getSurfaceAt({x}, {y})")
    parts = raw.split()
    if len(parts) < 3:
        return None
    try:
        z = int(float(parts[0]))
    except ValueError:
        return None
    return z, parts[2] in ("null", "nil")


def _candidate_grid(step: int, extent: int):
    pts = [(dx, dy) for dx in range(-extent, extent + 1, step)
                     for dy in range(-extent, extent + 1, step)]
    pts.sort(key=lambda p: p[0] * p[0] + p[1] * p[1])
    return pts


def _search_centres(rings: int = 2, spacing: int = 2 * SEARCH_RADIUS):
    pts = [(0, 0)]
    for r in range(1, rings + 1):
        d = r * spacing
        pts += [(d, 0), (-d, 0), (0, d), (0, -d),
                (d, d), (-d, -d), (d, -d), (-d, d)]
    return pts


def allocate_dry_anchors(port: int, n: int, min_sep: int = 12,
                          radius: int = SEARCH_RADIUS):
    span = radius // CHUNK_TILES + 1
    offsets = _candidate_grid(4, radius)
    for ox, oy in _search_centres():
        ccx, ccy = ox // CHUNK_TILES, oy // CHUNK_TILES
        send(port, f"return world.loadChunksInRegion({ccx - span}, "
                   f"{ccy - span}, {ccx + span}, {ccy + span})")
        send(port, "return world.waitForChunks(90)", timeout=95.0)
        wet = set()
        got = send_json(port,
                        f"return world.getAreaFluid({ox}, {oy}, {radius})",
                        timeout=30.0)
        if isinstance(got, list):
            for cell in got:
                if isinstance(cell, dict) and "x" in cell and "y" in cell:
                    wet.add((int(cell["x"]), int(cell["y"])))
        picked: list = []
        for dx, dy in offsets:
            gx, gy = ox + dx, oy + dy
            if (gx, gy) in wet:
                continue
            if any(max(abs(gx - tx), abs(gy - ty)) < min_sep
                   for tx, ty in picked):
                continue
            info = tile_surface(port, gx, gy)
            if info is None or not info[1]:
                continue
            picked.append((gx, gy))
            if len(picked) == n:
                return picked
    return None


def set_paused(port: int, on: bool) -> None:
    send(port, f"engine.setPaused({'true' if on else 'false'}); return 'ok'")
    time.sleep(0.3)


def locate_building_pixel(port: int, bid: int, fb_w: int, fb_h: int,
                           step: int = 4):
    """Find a screen pixel that really resolves to `bid`.

    Scans engine-side in ONE debug-console statement rather than a
    per-pixel round trip, and asks `building.hitTestAt` — the SAME
    single-target hit test the right-click router uses, so it is the
    only question that predicts where the menu will actually go. A
    computed tile-centre pixel is not good enough: sprites are anchored
    to the diamond bottom and the tile's terrain height shifts them."""
    # hitTestBuildingAt normalizes the pixel by the WINDOW size, so scan
    # that space rather than the framebuffer's (they can differ).
    win = send_json(port, "local w, h = engine.getWindowSize(); return {w=w, h=h}")
    sw = int((win or {}).get("w") or fb_w) if isinstance(win, dict) else fb_w
    sh = int((win or {}).get("h") or fb_h) if isinstance(win, dict) else fb_h
    raw = send(port,
               f"local b = {bid};"
               f" for y = 0, {sh}, {step} do"
               f"  for x = 0, {sw}, {step} do"
               "   local h = building.hitTestAt(x, y);"
               "   if h and h == b then return x .. ',' .. y end"
               "  end"
               " end; return 'none'", timeout=90.0).strip().strip('"')
    if "," not in raw:
        print(f"  (diag: hit-test scan over {sw}x{sh} returned {raw!r})")
        return None
    sx, sy = raw.split(",")
    return int(sx), int(sy)


# --------------------------------------------------------------------------
# Scenarios
# --------------------------------------------------------------------------
def check_no_duplicate_rows(port: int, scenario: str, rows: list) -> None:
    """A rebuild that failed to tear its previous elements down would
    leave two live rows carrying the same widget id."""
    ids = [r.get("id") for r in rows]
    check(f"{scenario}: no duplicate row records after rebuilds",
          len(ids) == len(set(ids)), f"got {ids!r}")


def cargo_scenario(port: int, bid: int, bpixel, fb_w: int, fb_h: int) -> None:
    print("== cargo Contents panel ==")
    if bpixel:
        bpx, bpy = bpixel
        send(port, f"return input.moveMouse({bpx}, {bpy})")
        send(port, f"return input.click({bpx}, {bpy}, 'right')")
        time.sleep(0.4)
        contents = find_widget(port, "Contents")
        if check("cargo context menu offers 'Contents'", bool(contents)):
            click_widget_center(port, contents)
            time.sleep(0.5)
    else:
        # The context-menu route to this panel is
        # tools/transfer_context_menu_probe.py's own gate; everything
        # #1088 changed lives INSIDE the panel, so open it through its
        # real public entry point and keep every check below real.
        print("  (building pixel not located — opening the panel through "
              "its public entry point instead)")
        send(port, "require('scripts.cargo_inventory_panel')"
                   f".openFor('building', {bid}, 200, 200); return 'ok'")
        time.sleep(0.5)

    opened = send(port, "return require('scripts.cargo_inventory_panel')"
                        ".isOpen()").strip()
    if not check("cargo Contents panel opened", opened == "true", f"got {opened!r}"):
        return

    # -- #1234: the window records the BUILDING endpoint's identity, and
    #    its chrome names the real building and its real storage load.
    ident = send_json(port, "local s = require('scripts.cargo_inventory_panel')"
                            ".state; return {kind = s.kind, id = s.id}")
    check("the window records a 'building' endpoint identity",
          isinstance(ident, dict) and ident.get("kind") == "building"
          and ident.get("id") == bid, f"got {ident!r}")
    chrome = panel_chrome(port)
    binfo = send_json(port, f"return building.getInfo({bid})")
    want_name = (binfo or {}).get("displayName") if isinstance(binfo, dict) else None
    check("the cargo window's title names the building",
          bool(want_name) and chrome.get("title") == want_name,
          f"got {chrome.get('title')!r} want {want_name!r}")
    cap = send(port, f"return building.getStorageCapacity({bid})").strip()
    used = send(port, f"return building.getStorageWeight({bid})").strip()
    want_sub = "Storage: %.2f / %.2f kg" % (float(used), float(cap))
    check("the cargo window's header reports the building's real "
          "stored weight and capacity",
          chrome.get("subtitle") == want_sub,
          f"got {chrome.get('subtitle')!r} want {want_sub!r}")

    rows = item_rows(port, CARGO_LIST_ID)
    check("cargo rows are one per STACK, not one per item",
          len(rows) == len(CARGO_STOCK),
          f"got {len(rows)} rows for {len(CARGO_STOCK)} stacks: "
          f"{[r.get('defName') for r in rows]!r}")
    by_def = {r.get("defName"): r for r in rows}
    for defname, copies in CARGO_STOCK:
        row = by_def.get(defname)
        if check(f"cargo row for {defname} exists", bool(row)):
            check(f"cargo row {defname} reports count {copies}",
                  row.get("count") == copies, f"got {row.get('count')!r}")
    check_no_duplicate_rows(port, "cargo", rows)
    check("every cargo row carries a right-click action",
          all(r.get("rightClick") is True for r in rows),
          f"got {[r.get('rightClick') for r in rows]!r}")

    # -- Tab strip: one row, inside the panel, All first.
    tabs = tab_boxes(port, CARGO_LIST_ID)
    if check("cargo tab strip rendered", len(tabs) >= 2, f"got {len(tabs)}"):
        ys = {int((t.get("bounds") or {}).get("y", 0)) for t in tabs}
        check("cargo tabs occupy exactly ONE row (shrink-to-fit, never wrap)",
              len(ys) == 1, f"got row tops {sorted(ys)!r}")
        panel_right = send_json(
            port, "local p = require('scripts.ui.panel');"
                  " local s = require('scripts.cargo_inventory_panel').state;"
                  " local x, y = p.getPosition(s.panelId);"
                  " local w, h = p.getSize(s.panelId); return {x=x, w=w}")
        if isinstance(panel_right, dict):
            left = panel_right.get("x", 0)
            right = left + panel_right.get("w", 0)
            inside = all(
                (t.get("bounds") or {}).get("x", 0) >= left
                and ((t.get("bounds") or {}).get("x", 0)
                     + (t.get("bounds") or {}).get("w", 0)) <= right + 1
                for t in tabs)
            check("every cargo tab stays inside the panel's own width", inside,
                  f"panel x={left} right={right}, tabs="
                  f"{[(t.get('bounds') or {}).get('x') for t in tabs]!r}")
        check("cargo tab strip starts with 'All'",
              (tabs[0].get("label") or "").startswith("All"),
              f"got {tabs[0].get('label')!r}")

        # -- Selecting a category filters the rows to it.
        target = next((t for t in tabs
                       if not (t.get("label") or "").startswith("All")), None)
        if check("a non-All category tab exists", bool(target)):
            label = (target.get("label") or "").split(" (")[0]
            click_widget_center(port, target)
            time.sleep(0.5)
            filtered = item_rows(port, CARGO_LIST_ID)
            check(f"selecting '{label}' filters the rows to that category",
                  bool(filtered) and all(r.get("category") == label
                                         for r in filtered),
                  f"got {[(r.get('defName'), r.get('category')) for r in filtered]!r}")
            check_no_duplicate_rows(port, "cargo (after tab click)", filtered)
            active = send(port, "return require('scripts.cargo_inventory_panel')"
                                ".state.activeTab").strip().strip('"')
            check("the panel records the clicked category as its selection",
                  active == label, f"got {active!r}")

            # -- A resize keeps the panel open on the same target AND tab.
            send(port, f"return engine.setWindowSize({fb_w - 160}, {fb_h - 120})")
            time.sleep(1.5)
            still_open = send(port, "return require('scripts.cargo_inventory_panel')"
                                    ".isOpen()").strip()
            check("a resize keeps the cargo panel open", still_open == "true",
                  f"got {still_open!r}")
            after = send_json(port, "local s = require("
                                    "'scripts.cargo_inventory_panel').state;"
                                    " return {kind = s.kind, id = s.id,"
                                    " tab = s.activeTab}")
            check("a resize preserves the panel's endpoint identity AND "
                  "selected tab",
                  isinstance(after, dict) and after.get("kind") == "building"
                  and after.get("id") == bid
                  and after.get("tab") == label, f"got {after!r}")
            resized_rows = item_rows(port, CARGO_LIST_ID)
            check_no_duplicate_rows(port, "cargo (after resize)", resized_rows)
            send(port, f"return engine.setWindowSize({fb_w}, {fb_h})")
            time.sleep(1.5)

            # Back to All so the right-click scenario sees every row.
            all_tab = next((t for t in tab_boxes(port, CARGO_LIST_ID)
                            if (t.get("label") or "").startswith("All")), None)
            if all_tab:
                click_widget_center(port, all_tab)
                time.sleep(0.5)

    # -- A real right-click reaches the representative instance.
    rows = item_rows(port, CARGO_LIST_ID)
    target_row = next((r for r in rows if r.get("defName") == "steel_bar"), None)
    if check("cargo steel_bar row still rendered for the right-click check",
             bool(target_row)):
        right_click_widget_center(port, target_row)
        time.sleep(0.4)
        labels = [w.get("label") for w in widgets(port)]
        withdraw = [l for l in labels if l and l.startswith("Withdraw")]
        check("right-clicking a cargo row opens its Withdraw menu",
              bool(withdraw), f"menu labels: {labels!r}")
        close_menu(port)

    send(port, "require('scripts.cargo_inventory_panel').closeIfOpen();"
               " return 'ok'")
    time.sleep(0.3)


def unit_endpoint_scenario(port: int, uid: int, wild_uid: int,
                            fb_w: int, fb_h: int) -> None:
    """#1234: the SAME container window, opened on a unit endpoint.

    Every expected value is read from the engine's own
    `unit.transferEndpointInfo` rather than restated here, so this
    checks that the window renders that endpoint — not that it renders
    numbers the probe happened to pick."""
    print("== unit endpoint (same container window) ==")
    ep = send_json(port, "return unit.transferEndpointInfo("
                         f"{{kind = 'unit', id = {uid}}})")
    if not check("engine reports the acolyte as an eligible endpoint",
                 isinstance(ep, dict) and ep.get("eligible") is True,
                 f"got {ep!r}"):
        return
    contents = ep.get("contents")
    if not check("the endpoint carries loose inventory to render",
                 isinstance(contents, list) and contents,
                 f"got {contents!r}"):
        return

    accepted = send(port, "return require('scripts.cargo_inventory_panel')"
                          f".openFor('unit', {uid}, 240, 240)").strip()
    check("the manager accepts a unit endpoint", accepted == "true",
          f"got {accepted!r}")
    time.sleep(0.5)
    opened = send(port, "return require('scripts.cargo_inventory_panel')"
                        ".isOpen()").strip()
    if not check("the container window opened on the unit", opened == "true",
                 f"got {opened!r}"):
        return

    ident = send_json(port, "local s = require('scripts.cargo_inventory_panel')"
                            ".state; return {kind = s.kind, id = s.id}")
    check("the window records a 'unit' endpoint identity",
          isinstance(ident, dict) and ident.get("kind") == "unit"
          and ident.get("id") == uid, f"got {ident!r}")

    # -- Title and header come from the endpoint, not from a building.
    chrome = panel_chrome(port)
    info = send_json(port, f"return unit.getInfo({uid})")
    want_title = None
    if isinstance(info, dict):
        want_title = (info.get("name") or info.get("displayName")
                      or info.get("defName"))
    want_title = want_title or ep.get("displayName")
    check("the unit window's title names the unit",
          chrome.get("title") == want_title,
          f"got {chrome.get('title')!r} want {want_title!r}")
    want_sub = "Carrying: %.2f / %.2f kg" % (float(ep.get("storedWeight") or 0),
                                             float(ep.get("capacity") or 0))
    check("the unit window's header reports transferEndpointInfo's own "
          "capacity and stored weight",
          chrome.get("subtitle") == want_sub,
          f"got {chrome.get('subtitle')!r} want {want_sub!r}")
    # storedWeight counts equipment and accessories too, so it must not
    # be mistakable for the rendered rows' summed weight.
    loose = sum(float(c.get("weight") or 0) for c in contents
                if isinstance(c, dict))
    check("stored weight is the recursive load, not the loose rows' sum",
          abs(float(ep.get("storedWeight") or 0) - loose) > 1e-6
          or not loose,
          f"storedWeight={ep.get('storedWeight')!r} loose sum={loose!r}")

    # -- Rows are that call's loose inventory, one per stack.
    rows = item_rows(port, CARGO_LIST_ID)
    want_defs = {c.get("defName") for c in contents if isinstance(c, dict)}
    got_defs = {r.get("defName") for r in rows}
    check("the unit window renders its loose inventory, one row per stack",
          got_defs == want_defs,
          f"got {sorted(d for d in got_defs if d)!r} "
          f"want {sorted(d for d in want_defs if d)!r}")
    check_no_duplicate_rows(port, "unit endpoint", rows)
    check("unit rows expose NO row action in this slice",
          all(r.get("rightClick") is False for r in rows),
          f"got {[r.get('rightClick') for r in rows]!r}")

    # -- Tab strip, and selecting a category.
    tabs = tab_boxes(port, CARGO_LIST_ID)
    label = None
    if check("the unit window renders a tab strip", len(tabs) >= 2,
             f"got {len(tabs)}"):
        check("the unit window's tab strip starts with 'All'",
              (tabs[0].get("label") or "").startswith("All"),
              f"got {tabs[0].get('label')!r}")
        target = next((t for t in tabs
                       if not (t.get("label") or "").startswith("All")), None)
        if check("a non-All category tab exists on the unit window",
                 bool(target)):
            label = (target.get("label") or "").split(" (")[0]
            click_widget_center(port, target)
            time.sleep(0.5)
            filtered = item_rows(port, CARGO_LIST_ID)
            check(f"selecting '{label}' filters the unit rows to it",
                  bool(filtered) and all(r.get("category") == label
                                         for r in filtered),
                  f"got {[(r.get('defName'), r.get('category')) for r in filtered]!r}")

            # -- A resize preserves the endpoint identity AND the tab.
            send(port, f"return engine.setWindowSize({fb_w - 160}, {fb_h - 120})")
            time.sleep(1.5)
            check("a resize keeps the unit window open",
                  send(port, "return require('scripts.cargo_inventory_panel')"
                             ".isOpen()").strip() == "true")
            after = send_json(port, "local s = require("
                                    "'scripts.cargo_inventory_panel').state;"
                                    " return {kind = s.kind, id = s.id,"
                                    " tab = s.activeTab}")
            check("a resize preserves the unit endpoint identity AND tab",
                  isinstance(after, dict) and after.get("kind") == "unit"
                  and after.get("id") == uid and after.get("tab") == label,
                  f"got {after!r}")
            send(port, f"return engine.setWindowSize({fb_w}, {fb_h})")
            time.sleep(1.5)
            all_tab = next((t for t in tab_boxes(port, CARGO_LIST_ID)
                            if (t.get("label") or "").startswith("All")), None)
            if all_tab:
                click_widget_center(port, all_tab)
                time.sleep(0.5)

    # -- Contents are read LIVE: an inventory change reaches the open
    #    window through its own per-tick refresh, with no reopen.
    before = {r.get("defName") for r in item_rows(port, CARGO_LIST_ID)}
    send(port, f"return unit.addItem({uid}, 'quinoa_sack')")
    live = poll_until(6.0, lambda: "quinoa_sack" in
                      {r.get("defName") for r in item_rows(port, CARGO_LIST_ID)},
                      interval=0.4)
    check("a live inventory change refreshes the open unit window",
          bool(live), f"rows before={sorted(d for d in before if d)!r}")
    after_rows = item_rows(port, CARGO_LIST_ID)
    check_no_duplicate_rows(port, "unit endpoint (after live change)", after_rows)
    refreshed = panel_chrome(port)
    ep2 = send_json(port, "return unit.transferEndpointInfo("
                          f"{{kind = 'unit', id = {uid}}})")
    if isinstance(ep2, dict):
        want_sub2 = "Carrying: %.2f / %.2f kg" % (
            float(ep2.get("storedWeight") or 0), float(ep2.get("capacity") or 0))
        check("the header follows the live stored weight too",
              refreshed.get("subtitle") == want_sub2,
              f"got {refreshed.get('subtitle')!r} want {want_sub2!r}")

    send(port, "require('scripts.cargo_inventory_panel').closeIfOpen();"
               " return 'ok'")
    time.sleep(0.3)

    # -- A unit that is not player-commandable is not an endpoint.
    faction = send(port, f"return unit.getFaction({wild_uid})").strip().strip('"')
    check("wildlife fixture is genuinely not player-commandable",
          faction == "wildlife", f"got {faction!r}")
    wild_ep = send_json(port, "return unit.transferEndpointInfo("
                              f"{{kind = 'unit', id = {wild_uid}}})")
    check("engine reports the wildlife unit as ineligible",
          not isinstance(wild_ep, dict) or wild_ep.get("eligible") is not True,
          f"got {wild_ep!r}")
    refused = send(port, "return require('scripts.cargo_inventory_panel')"
                         f".openFor('unit', {wild_uid}, 240, 240)").strip()
    check("the manager refuses a non-commandable unit", refused == "false",
          f"got {refused!r}")
    check("a refused open leaves the window closed",
          send(port, "return require('scripts.cargo_inventory_panel')"
                     ".isOpen()").strip() == "false")
    state = send_json(port, "local s = require('scripts.cargo_inventory_panel')"
                            ".state; return {panel = s.panelId ~= nil,"
                            " list = s.listId ~= nil}")
    check("a refused open creates no panel or list state",
          isinstance(state, dict) and state.get("panel") is False
          and state.get("list") is False, f"got {state!r}")

    # An unknown kind is refused the same way.
    unknown = send(port, "return require('scripts.cargo_inventory_panel')"
                         ".openFor('item_container', 1, 240, 240)").strip()
    check("the manager refuses an unknown endpoint kind", unknown == "false",
          f"got {unknown!r}")


def item_contents_scenario(port: int, mule_uid: int, uid: int) -> None:
    print("== first-aid-kit Contents panel ==")
    # The technomule's own starting inventory carries a PRE-STOCKED kit
    # (data/units/technomule.yaml -> starting_inventory); unit.addItem
    # mints an empty one, which is the empty-state fixture below.
    engine_rows = send_json(
        port, f"return unit.getItemContents({mule_uid}, 'first_aid_kit')")
    if not check("engine reports the kit's pre-grouped contents",
                 isinstance(engine_rows, list) and engine_rows,
                 f"got {engine_rows!r}"):
        return
    expected = {r.get("defName"): r.get("count") for r in engine_rows
                if isinstance(r, dict)}

    send(port, "require('scripts.item_contents_panel').openFor("
               f"{mule_uid}, 'first_aid_kit', 400, 300); return 'ok'")
    time.sleep(0.6)
    opened = send(port, "return require('scripts.item_contents_panel')"
                        ".isOpen()").strip()
    if not check("item-contents panel opened", opened == "true", f"got {opened!r}"):
        return

    rows = item_rows(port, ITEM_CONTENTS_LIST_ID)
    check("pre-grouped rows appear unchanged — never re-split by the "
          "finer stack key",
          len(rows) == len(expected),
          f"got {len(rows)} widget rows vs {len(expected)} engine rows: "
          f"{[r.get('defName') for r in rows]!r} vs {sorted(expected)!r}")
    for r in rows:
        d = r.get("defName")
        if d in expected:
            check(f"item-contents row {d} keeps the engine's own count",
                  r.get("count") == expected[d],
                  f"got {r.get('count')!r} vs {expected[d]!r}")
    check_no_duplicate_rows(port, "item contents", rows)
    check("item-contents rows expose NO right-click action",
          all(r.get("rightClick") is False for r in rows),
          f"got {[r.get('rightClick') for r in rows]!r}")
    check("item-contents rows are NOT reported as enabled click targets",
          all(r.get("enabled") is not True or r.get("rightClick") is False
              for r in rows))
    check("item-contents panel renders NO tab strip",
          not tab_boxes(port, ITEM_CONTENTS_LIST_ID))

    # -- The empty state, on the acolyte's own freshly-minted (empty) kit.
    send(port, "require('scripts.item_contents_panel').closeIfOpen(); return 'ok'")
    time.sleep(0.3)
    send(port, "require('scripts.item_contents_panel').openFor("
               f"{uid}, 'first_aid_kit', 400, 300); return 'ok'")
    time.sleep(0.6)
    check("an empty container still opens its panel",
          send(port, "return require('scripts.item_contents_panel')"
                     ".isOpen()").strip() == "true")
    check("an empty container renders no rows (the '(empty)' state)",
          not item_rows(port, ITEM_CONTENTS_LIST_ID))
    send(port, "require('scripts.item_contents_panel').closeIfOpen(); return 'ok'")


def unit_inventory_scenario(port: int, uid: int) -> None:
    print("== unit inventory section ==")
    send(port, f"return unit.select({uid})")
    time.sleep(0.8)
    rows = item_rows(port, UNIT_INV_LIST_ID)
    if not check("unit inventory renders rows", bool(rows)):
        return
    check_no_duplicate_rows(port, "unit inventory", rows)
    check("every unit-inventory row carries a right-click action",
          all(r.get("rightClick") is True for r in rows),
          f"got {[r.get('rightClick') for r in rows]!r}")

    tabs = tab_boxes(port, UNIT_INV_LIST_ID)
    if check("unit inventory tab strip rendered", len(tabs) >= 2,
             f"got {len(tabs)}"):
        check("unit inventory tab strip starts with 'All'",
              (tabs[0].get("label") or "").startswith("All"),
              f"got {tabs[0].get('label')!r}")
        rect = send_json(port, "local u = require('scripts.unit_info_v2');"
                               " local r = u.invRect;"
                               " return r and {x=r.x, y=r.y, w=r.w, h=r.h} or nil")
        if isinstance(rect, dict):
            left, right = rect["x"], rect["x"] + rect["w"]
            inside = all(
                (t.get("bounds") or {}).get("x", 0) >= left
                and ((t.get("bounds") or {}).get("x", 0)
                     + (t.get("bounds") or {}).get("w", 0)) <= right + 1
                for t in tabs)
            check("every unit-inventory tab stays inside the section rect",
                  inside, f"rect x={left}..{right}, tabs="
                          f"{[(t.get('bounds') or {}).get('x') for t in tabs]!r}")
            # Wrapped rows are CENTRED: each row's left margin matches
            # its right margin. With one row this still holds.
            by_row: dict[int, list] = {}
            for t in tabs:
                by_row.setdefault(int((t.get("bounds") or {}).get("y", 0)),
                                  []).append(t)
            centred = True
            for _, row_tabs in by_row.items():
                lo = min((t.get("bounds") or {}).get("x", 0) for t in row_tabs)
                hi = max((t.get("bounds") or {}).get("x", 0)
                         + (t.get("bounds") or {}).get("w", 0) for t in row_tabs)
                if abs((lo - left) - (right - hi)) > 2:
                    centred = False
            check("each unit-inventory tab row is centred in the section",
                  centred, f"rows={ {k: len(v) for k, v in by_row.items()} !r}")

        target = next((t for t in tabs
                       if not (t.get("label") or "").startswith("All")), None)
        if check("a non-All unit-inventory category tab exists", bool(target)):
            label = (target.get("label") or "").split(" (")[0]
            click_widget_center(port, target)
            time.sleep(0.8)
            filtered = item_rows(port, UNIT_INV_LIST_ID)
            check(f"selecting '{label}' filters the unit inventory to it",
                  bool(filtered) and all(r.get("category") == label
                                         for r in filtered),
                  f"got {[(r.get('defName'), r.get('category')) for r in filtered]!r}")
            active = send(port, "return require('scripts.unit_info_v2')"
                                ".activeInvTab").strip().strip('"')
            check("the section records the clicked category as its selection",
                  active == label, f"got {active!r}")
            all_tab = next(
                (t for t in tab_boxes(port, UNIT_INV_LIST_ID)
                 if (t.get("label") or "").startswith("All")), None)
            if all_tab:
                click_widget_center(port, all_tab)
                time.sleep(0.8)

    rows = item_rows(port, UNIT_INV_LIST_ID)
    if rows:
        right_click_widget_center(port, rows[0])
        time.sleep(0.4)
        labels = [w.get("label") for w in widgets(port) if w.get("label")]
        check("right-clicking a unit-inventory row opens its item menu",
              any(l in ("Equip", "Unequip", "Contents", "Drop")
                  or l.startswith("Store") for l in labels),
              f"menu labels: {labels!r}")
        close_menu(port)


def main() -> int:
    ap = argparse.ArgumentParser()
    ap.add_argument("--port", type=int, default=9428)
    ap.add_argument("--size", default="1280x900")
    args = ap.parse_args()
    port = args.port

    print(f"booting offscreen engine on port {port} ({args.size}) ...")
    proc = boot(port, args=["--size", args.size],
                mode=("--offscreen",), ready_timeout=180.0)

    menu_up = poll_until(60.0, lambda: find_widget(port, "Create World"))
    check("loading screen -> main menu", bool(menu_up))
    if not menu_up:
        quit_engine(port, proc)
        return 1
    click_widget_center(port, find_widget(port, "Create World"))
    create_up = poll_until(20.0, lambda: find_widget(port, "Generate World"))
    check("create-world screen reached", bool(create_up))
    click_widget_center(port, find_widget(port, "Generate World"))

    def world_done():
        return send(port, "local p = world.getInitProgress(); return p",
                    timeout=5.0).strip() == "3"

    print("  (generating world, ~1-2 min)")
    check("worldgen completes (phase 3)",
          bool(poll_until(300.0, world_done, interval=2.0)))
    cont = poll_until(60.0, lambda: find_widget(port, "Continue"))
    check("post-generation Continue button appears", bool(cont))
    click_widget_center(port, find_widget(port, "Continue"))
    hud_up = poll_until(60.0, lambda: not find_widget(port, "Continue"))
    check("in-game HUD reached", bool(hud_up))
    time.sleep(2.0)

    fb_w, fb_h = (int(v) for v in args.size.split("x"))

    with open(TEST_BUILDING_YAML, "w") as f:
        f.write(TEST_BUILDINGS)
    n = send(port, f"return engine.loadBuildingYaml('{TEST_BUILDING_YAML}')")
    check("probe building def loaded", float(n) == 1.0, f"got {n!r}")

    print("  (scanning terrain outward from the origin for dry anchor sites)")
    sites = allocate_dry_anchors(port, 4)
    if not check("found four separated dry sites for the fixtures",
                 sites is not None):
        quit_engine(port, proc)
        return 1
    (bax, bay), (aax, aay), (max_, may_), (wax, way) = sites
    print(f"  (fixture sites: building={(bax, bay)} acolyte={(aax, aay)} "
          f"technomule={(max_, may_)} wildlife={(wax, way)})")

    uid = int(float(send(port,
        f"return unit.spawn('acolyte', {aax}, {aay}, nil, 'player')")))
    mule_uid = int(float(send(port,
        f"return unit.spawn('technomule', {max_}, {may_}, nil, 'player')")))
    # unit.spawn defaults to the WILDLIFE faction when no tag is given —
    # the #1234 ineligible-endpoint fixture.
    wild_uid = int(float(send(port,
        f"return unit.spawn('red_squirrel', {wax}, {way})")))
    bid_raw = send(port, f"return building.spawn('{DEF_CARGO}', {bax}, {bay})")
    if not check("storage building spawned",
                 bid_raw.strip() not in ("", "nil", "null"), f"got {bid_raw!r}"):
        quit_engine(port, proc)
        return 1
    bid = int(float(bid_raw))
    check("storage building reaches Built activity",
          bool(poll_until(10.0, lambda: send(
              port, f"return building.getActivity({bid})").strip('"') == "built")))

    # -- Stock the cargo through the real deposit verb, and give the
    #    acolyte its own multi-category inventory plus a first-aid kit.
    for defname, copies in CARGO_STOCK:
        send(port, f"for i = 1, {copies} do unit.addItem({uid}, '{defname}');"
                   f" unit.depositToCargo({uid}, {bid}, '{defname}')"
                   " end; return 'ok'", timeout=20.0)
    stored = send_json(port, f"return building.getStorage({bid})")
    check("cargo stocked through the real deposit verb",
          isinstance(stored, list)
          and len(stored) == sum(c for _, c in CARGO_STOCK),
          f"got {len(stored) if isinstance(stored, list) else stored!r}")

    for defname in ("steel_bar", "steel_bar", "bandage", "first_aid_kit"):
        send(port, f"return unit.addItem({uid}, '{defname}')")
    inv = send_json(port, f"return unit.getInventory({uid})")
    check("acolyte carries a multi-category inventory",
          isinstance(inv, list) and len(inv) >= 4,
          f"got {len(inv) if isinstance(inv, list) else inv!r}")

    # Bring the cargo on screen AND onto the camera's own z-slice:
    # Building.HitTest only considers buildings at or below the slice
    # (matching the render cull), so a building standing above it is
    # unclickable no matter where the camera points.
    binfo = send_json(port, f"return building.getInfo({bid})")
    gz = int((binfo or {}).get("gridZ", 0)) if isinstance(binfo, dict) else 0
    send(port, f"camera.goToTile({bax}, {bay}); camera.setZSlice({gz});"
               " return 'ok'")
    time.sleep(1.5)
    set_paused(port, True)
    bpixel = locate_building_pixel(port, bid, fb_w, fb_h)
    if not bpixel:
        print(f"  (diag: building gridZ={gz} camera="
              f"{send(port, 'return camera.getPosition()').strip()!r} "
              f"zslice={send(port, 'return camera.getZSlice()').strip()!r} "
              f"zoom={send(port, 'return camera.getZoom()').strip()!r})")
    cargo_scenario(port, bid, bpixel, fb_w, fb_h)

    unit_endpoint_scenario(port, uid, wild_uid, fb_w, fb_h)
    unit_inventory_scenario(port, uid)
    item_contents_scenario(port, mule_uid, uid)

    quit_engine(port, proc)
    if failures:
        print(f"\nitem_list_widget_probe: {failures} check(s) FAILED")
        return 1
    print("\nitem_list_widget_probe: all checks passed")
    return 0


if __name__ == "__main__":
    sys.exit(main())
