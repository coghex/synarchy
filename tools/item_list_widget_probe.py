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

The cargo panel is reached by a REAL right-click on the building, routed
through `building.hitTestAt` exactly as `scripts/init_context_menu.lua`
does. Localizing that building is a required observation: if the pixel
cannot be found the probe FAILS, and only then falls back to the panel's
public entry point so the remaining #1088 checks (which all live INSIDE
the panel) still report something useful. Before #1286 that fallback was
silent — the context-menu check sat inside `if bpixel:`, so a failed
localization removed it and the run still passed.

Localization itself is #1286's fix, in `probelib.focus_and_locate`: a bare
`camera.goToTile` leaves z-tracking ON, which pins the z-slice 25 levels
above the surface, and both the render and every hit test then offset the
target by `(gridZ - zSlice) * tileSideHeight` — far enough at
`goToTile`'s own zoom to push it off the bottom of the viewport. The
slice has to be re-pinned to the target's own z AFTER every `goToTile`.

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

Since #1238 that window owns an ordered STACK of nesting levels, so this
is also the rendered gate on pushing, replacing, dismissing and
restoring them, and on only the deepest one being interactive. Its own
throwaway item fixture (`probe_deep_kit`) is a container whose default
contents are 15 distinct defs plus a real `first_aid_kit`, which is what
gives a nested level both more rows than its cap and a container row of
its own.

Since #1237 a BUILDING endpoint renders the player's REMEMBERED contents
(`building.getContainerKnowledge`), so this is also the rendered gate on
all three knowledge states, on the "as of…" age, and on the rule that
opening the window reveals nothing. The never-inspected fixture is a
WORKER-BUILT storage def left at zero progress: A3 seeds a container as
known-empty at its first transition to Built, so an instant-built one
cannot supply that state, and calling a knowledge verb to manufacture it
would make the probe assert its own writes.

Verifies, in order:

  1. cargo Contents panel (a BUILDING endpoint): its title and
     capacity/stored-weight header name the real building; the widget
     renders one row per STACK (not per item) with the right counts and
     categories; its tab strip is one shrink-to-fit row of `All` +
     first-appearance categories, entirely inside the panel; clicking a
     category tab filters the rows to it; a framebuffer resize keeps the
     panel open on the SAME endpoint identity and the SAME selected
     category; a rebuild leaks no duplicate rows.
  2. cargo rows route a real right-click to the representative instance,
     and the menu that appears is #1249's queued gesture: "Retrieve 1" /
     "Retrieve all" on a merged row, firing one queues a REAL durable
     transfer order (read back through `unit.getTransferOrders`) whose
     items are the row's own instance ids, and neither retired label
     ("Withdraw ...", "Store in ...") appears anywhere.
  3. a UNIT endpoint (#1234) opens through that same manager: its title
     names the unit, its header reports `transferEndpointInfo`'s own
     capacity and stored weight (which counts equipment and accessories,
     so it is deliberately NOT the rows' summed weight), its rows are
     that call's loose inventory, its tab strip behaves like the
     building's, a live inventory change refreshes it without reopening,
     a resize preserves the endpoint identity AND the selected tab, unit
     rows expose no row action, and a wildlife unit — not
     player-commandable — cannot open the window at all.
  4. last-known contents (#1237): a never-inspected container renders as
     unknown — not as an empty one — with its capacity still shown, an
     unknown stored weight and no age; opening it reveals nothing; a
     known-empty one renders "(empty)" with an age; a completed deposit
     refreshes an ALREADY-OPEN window to "known" with the moved item and
     a fresh age; and that age advances across two increasing
     `engine.gameTime()` readings taken against the same unchanged
     `revealedAt`.
  5. first-aid-kit Contents panel: the Haskell-side pre-grouped rows
     appear unchanged (never re-split by the finer stack key), a plain
     row offers no menu at all (the level is render-only), and an
     emptied container renders its "(empty)" state.
  8. the nesting stack (#1238): a container row's real right-click ->
     "Contents" gesture pushes a level addressed by the exact instance
     clicked; the parent stays PAINTED but out of input scope, with a
     right-click on one of its rows opening nothing; a level inside that
     one pushes a third whose path extends its parent's; opening another
     container at the same level replaces it and discards every deeper
     one; Escape closes exactly one level per press, deepest first,
     restoring the newly deepest level each time; the mouse WHEEL
     scrolls whichever level is deepest, and a real framebuffer resize
     preserves the whole nesting path AND every level's own offset; a
     building-side level renders the engine's REMEMBERED contents with
     the PARENT's own age, while the unit-info gesture opens a LIVE
     level at the base.
  6. unit inventory section: rows and counts, a wrapped/centred tab
     strip inside the section rect, tab selection filtering, and a real
     right-click reaching the representative instance's Equip/Contents
     menu.
  9. the escort session (#1250): a Mode A session opens TWO flanking
     panels as ONE non-modal stack level, both clamped inside the
     framebuffer and neither overlapping the other, source on the left
     and destination on the right; the camera snaps onto the pair (the
     one gesture in this file that moves it at all); a source-pane row
     offers Store and only Store while a destination-pane row offers
     Retrieve and only Retrieve; firing Store all moves the items for
     real and refreshes BOTH panes' headers within the gesture, leaving
     the session open; a real resize keeps the PAIR sized to the
     framebuffer it is drawn on, in frame and flanking rather than
     stacked; and one dismissal closes both panels and ends the
     session.
 10. the unit-to-unit escort's two-sided hold (#1251): a target taken
     out of the medic squad first (`treat_ally` at 8.0 is a band that
     legitimately preempts the 7.5 hold, so it must not be what decides)
     and put under a real move order — so it is genuinely in motion,
     under a named lock that outranks every routine-work one — is
     preempted by the session on its very next AI tick, stops where it
     stood rather than where it was sent, and does not move again for
     the WHOLE of the source's approach; both units then report their
     two roles in the one session; the pair renders over two UNIT
     endpoints and commits real instances in both directions through the
     real row menus; and one dismissal releases BOTH, each proved by the
     real AI — the hold stops winning, and a fresh move order is taken
     up — rather than by a cleared table.
  7. tracked temperature (#1268): both raw-item hosts present a row's
     summary in the row text AND in a tooltip line, derived from the
     same string; a group holding two tracked values and one ambient
     member reports all three rather than the representative's; an
     EQUIPPED row presents one too (the path `equipment.getLoadout` had
     no `temp` field for at all); cooling inside one displayed degree
     rebuilds nothing while crossing the boundary refreshes the row; and
     a deposited exact instance carries its temperature into the
     container window on both endpoint kinds.

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
import math
import os
import sys
import tempfile
import time

sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
from probelib import (boot, camera_state, centred_within, clear_find_water,
                      focus_and_locate, locate_building_pixel, poll_until,
                      quit_engine, send, send_json, set_paused,
                      targeting_report, viewport, win_to_fb)

SPROOT = tempfile.gettempdir()
TEST_BUILDING_YAML = os.path.join(SPROOT, "item_list_widget_probe_buildings.yaml")
DEF_CARGO = "probe_item_list_cargo"
DEF_EMPTY = "probe_item_list_empty"
DEF_UNSEEN = "probe_item_list_unseen"

# DEF_UNSEEN is deliberately WORKER-BUILT (`build_work > 0`). A
# `building.spawn`ed instance of it is created at zero progress and never
# reaches Built, so A3's `SeedAtBuildCompletion` trigger never fires and
# its knowledge record genuinely does not exist — the never-inspected
# state, obtained without calling a single knowledge-mutating verb. Its
# capacity is still def-declared and therefore still LIVE, which is what
# requirement 1 needs a fixture for.
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
  - name: "{DEF_EMPTY}"
    display_name: "Probe Item List Empty Cargo"
    category: "Test"
    description: "Throwaway #1237 known-empty fixture — not shipped content."
    sprite: "assets/textures/buildings/cargo_hold_S/default.png"
    tile_size: {{ x: 1, y: 1 }}
    placement: "flat_ground"
    race: "acolyte_cult"
    build_work: 0.0
    storage_capacity: 250.0
  - name: "{DEF_UNSEEN}"
    display_name: "Probe Item List Unseen Cargo"
    category: "Test"
    description: "Throwaway #1237 never-inspected fixture — not shipped content."
    sprite: "assets/textures/buildings/cargo_hold_S/default.png"
    tile_size: {{ x: 1, y: 1 }}
    placement: "flat_ground"
    race: "acolyte_cult"
    build_work: 240.0
    storage_capacity: 300.0
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

# #1238 fixtures. A throwaway item-container whose default contents are
# 15 DISTINCT defs plus a real `first_aid_kit` — so a level rendering it
# has more rows than its own 12-row cap (something to scroll) AND a
# container row of its own (somewhere to descend). Registered through
# the real item-YAML loader, the same throwaway-def technique the
# building fixtures above use.
TEST_ITEM_YAML = os.path.join(SPROOT, "item_list_widget_probe_items.yaml")
DEF_DEEP_KIT = "probe_deep_kit"
DEEP_KIT_CONTENTS = [
    "bandage", "gauze", "elastic_wrap", "tweezers", "scissors",
    "steel_bar", "wood_log", "quinoa_sack", "wiring", "whetstone",
    "tomato", "wheat_grain", "granite_chunk", "steel_plate",
    "steel_hardware",
]
# Since #1418 EVERY creation path materializes a container def's
# declared `contents:` -- `unit.addItem` included -- so the deep kit
# would arrive stocked either way. It still comes in through a spawned
# carrier's starting inventory, deliberately: that is the path the
# nesting fixture was BUILT on, and re-routing it would be changing the
# fixture rather than following the behaviour change.
#
# What the change does take away is the free empty container the
# item-contents scenario used for its "(empty)" render state. That state
# is real UI behaviour and keeps its coverage, so the probe now AUTHORS
# an empty container: a throwaway def with no `contents:` key at all
# decodes to an empty list and materializes empty, however it is created.
DEF_EMPTY_BOX = "probe_empty_box"
TEST_EMPTY_BOX = f"""\
  - name: "{DEF_EMPTY_BOX}"
    display_name: "Probe Empty Box"
    sprite: "assets/textures/items/medical/first_aid_kit.png"
    weight: 0.5
    bulk: 4.0
    kind: container
    category: Medical
"""

TEST_ITEMS = "items:\n" + f"""\
  - name: "{DEF_DEEP_KIT}"
    display_name: "Probe Deep Kit"
    sprite: "assets/textures/items/medical/first_aid_kit.png"
    weight: 0.5
    bulk: 4.0
    kind: container
    category: Medical
    contents:
""" + "".join(f"      - {{ item: {d}, count: 1 }}\n"
              for d in DEEP_KIT_CONTENTS) + \
    "      - { item: first_aid_kit, count: 1 }\n" + TEST_EMPTY_BOX

TEST_UNIT_YAML = os.path.join(SPROOT, "item_list_widget_probe_units.yaml")
DEF_CARRIER = "probe_kit_carrier"
TEST_UNITS = f"""\
units:
  - name: {DEF_CARRIER}
    display_name: "Probe Kit Carrier"
    sprite: "assets/textures/units/tiller/animations/idle/south/frame_000.png"
    starting_inventory:
      - {{ item: "{DEF_DEEP_KIT}", count: 1 }}
"""

# Enough DISTINCT defs in the cargo that the base level (10-row cap) has
# somewhere to scroll to as well. Deposited BEFORE the two containers, so
# both of those stay inside the first rendered rows: a level renders its
# rows in the remembered list's own order, and `unit.depositToCargo`
# PREPENDS (`biStorage = item : biStorage`), so the newest deposit is the
# first row.
CARGO_BULK_STOCK = [
    "bronze_bar", "granite_chunk", "wiring", "whetstone", "tomato",
    "wheat_grain", "steel_plate", "steel_hardware", "rations", "radio",
    "solar_panel",
]

# Debug-console expressions naming each host's live widget instance.
# Since #1238 the container window owns a STACK of levels, and the
# item-contents popup is one of them rather than a second panel:
# `getLevel(i)` names one (default the deepest), and every read below
# goes through it because a level may not exist.
LEVEL = "require('scripts.cargo_inventory_panel').getLevel"
BASE_LEVEL = f"({LEVEL}(1) or {{src={{}}}})"
DEEP_LEVEL = f"({LEVEL}() or {{src={{}}}})"
CARGO_LIST_ID = f"{BASE_LEVEL}.listId"
ITEM_CONTENTS_LIST_ID = f"{DEEP_LEVEL}.listId"
UNIT_INV_LIST_ID = "require('scripts.unit_info_v2').invListId"

failures = 0


def probe_result() -> int:
    """The run's exit status: non-zero whenever ANY check failed.

    A SETUP failure is one of those (#1911): a scenario that cannot
    establish its fixture reports it and the run is red, rather than the
    probe grading the fixture anyway and exiting green."""
    if failures:
        print(f"\nitem_list_widget_probe: {failures} check(s) FAILED")
        return 1
    print("\nitem_list_widget_probe: all checks passed")
    return 0


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
                          ".getLevel(1) or {}; local l = require('scripts.ui.label');"
                          " return {title = s.titleId and l.getText(s.titleId),"
                          " subtitle = s.subtitleId and l.getText(s.subtitleId)}")
    return got if isinstance(got, dict) else {}


def chrome_text(port: int, name: str):
    """One of the window's own labels, located by its engine element name
    through the `ui.dumpWidgets()` oracle rather than by reading the
    panel's bookkeeping.

    Returns None when the label does not exist, which is itself the
    assertion for the two labels #1237 renders CONDITIONALLY:
    `cargo_inv_age` (absent for a live endpoint and for a never-inspected
    container) and `cargo_inv_empty` (the item-list widget's own
    empty-state line, absent whenever there are rows)."""
    for w in widgets(port):
        if w.get("type") == "label" and w.get("name") == name:
            return w.get("label")
    return None


def format_age(elapsed: float) -> str:
    """Mirror of `formatAge` in scripts/cargo_inventory_panel.lua.

    Deliberately restated here rather than read out of the panel: the
    checks below assert that the RENDERED label equals what the engine's
    own `revealedAt` and `engine.gameTime()` say it must be — the same
    technique the header checks already use for
    "Storage: %.2f / %.2f kg". A wording or bucketing change in the Lua
    therefore fails this gate instead of slipping past it."""
    s = int(math.floor(elapsed))
    if s < 0:
        s = 0
    if s < 5:
        return "just now"
    if s < 60:
        return f"{s}s ago"
    if s < 3600:
        m, rs = s // 60, s % 60
        if m < 10 and rs > 0:
            return f"{m}m {rs}s ago"
        return f"{m}m ago"
    if s < 86400:
        hr, m = s // 3600, (s % 3600) // 60
        if m > 0:
            return f"{hr}h {m}m ago"
        return f"{hr}h ago"
    d, hr = s // 86400, (s % 86400) // 3600
    if hr > 0:
        return f"{d}d {hr}h ago"
    return f"{d}d ago"


def knowledge(port: int, bid: int) -> dict:
    got = send_json(port, f"return building.getContainerKnowledge({bid})")
    return got if isinstance(got, dict) else {}


def game_time(port: int) -> float:
    raw = send(port, "return engine.gameTime()").strip()
    try:
        return float(raw)
    except ValueError:
        return float("nan")


def expected_age(port: int, bid: int):
    """The "as of…" line the open window MUST be showing, derived from
    the engine's own knowledge record and game clock — never a wall
    clock. Read while the simulation is stopped so the two round trips
    below describe one instant."""
    k = knowledge(port, bid)
    revealed = k.get("revealedAt")
    if not isinstance(revealed, (int, float)):
        return None, k
    return "as of " + format_age(game_time(port) - float(revealed)), k


def orders_of(port: int, uid: int):
    """Every durable transfer order a unit carries, as comparable
    strings: `"<src kind>:<id>><dst kind>:<id>[<instance ids>]"`.

    Read through `unit.getTransferOrders`, which is #1246's own live
    surface onto the per-page store -- so a gesture that merely ran a
    callback without queueing anything reports nothing here."""
    raw = send(port, "local out = {};"
                     f" for _, o in ipairs(unit.getTransferOrders({uid}) or {{}}) do"
                     "   local ids = {};"
                     "   for j, e in ipairs(o.entries or {}) do"
                     "     ids[j] = e.instanceId end;"
                     "   out[#out + 1] = o.source.kind .. ':' .. o.source.id"
                     "     .. '>' .. o.destination.kind .. ':'"
                     "     .. o.destination.id .. '['"
                     "     .. table.concat(ids, ',') .. ']';"
                     " end;"
                     " return table.concat(out, ';')").strip('"')
    return [chunk for chunk in raw.split(";") if chunk]


def open_window_on(port: int, bid: int) -> bool:
    accepted = send(port, "return require('scripts.cargo_inventory_panel')"
                          f".openFor('building', {bid}, 240, 240)").strip()
    time.sleep(0.5)
    return accepted == "true"


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


def focus_building(port: int, bid: int, gx: int, gy: int, vp: dict):
    """Put the built storage fixture on a targetable screen pixel.

    `camera.goToTile` alone leaves z-tracking ON, which pins the z-slice
    25 levels above the surface and pushes the tile it just "went to"
    clean off the bottom of the viewport — #1286. `focus_and_locate`
    pins the slice to the building's OWN `gridZ` after `goToTile`, which
    is what both the render and `Building.HitTest` measure their vertical
    offset from. Returns the window-space pixel, or None."""
    binfo = send_json(port, f"return building.getInfo({bid})")
    gz = int((binfo or {}).get("gridZ", 0)) if isinstance(binfo, dict) else 0
    # Freeze BEFORE targeting: the fixtures stand on a mountainous
    # shoreline and an acolyte left walking on it collapses from
    # accumulated falls (#1286).
    set_paused(port, True)
    pixel = focus_and_locate(port, gx, gy, gz, vp,
                             lambda: locate_building_pixel(port, bid, vp))
    cam = camera_state(port)
    check("camera preconditions settled for the hit test "
          "(z-tracking off, slice == the building's gridZ, tile zoom band)",
          cam.get("zTracking") is False and cam.get("zSlice") == gz
          and isinstance(cam.get("zoom"), (int, float))
          and cam.get("zoom") < 1.2,
          f"got {cam!r} for gridZ {gz}")
    if pixel is None:
        print(targeting_report(port, vp, "building", bid, site=(gx, gy)))
    else:
        check("the camera centres on the building "
              "(its hit-test pixel is near the screen centre)",
              centred_within(vp, pixel),
              f"got {pixel!r} for a {vp['win_w']}x{vp['win_h']} window")
    return pixel


# --------------------------------------------------------------------------
# Scenarios
# --------------------------------------------------------------------------
def check_no_duplicate_rows(port: int, scenario: str, rows: list) -> None:
    """A rebuild that failed to tear its previous elements down would
    leave two live rows carrying the same widget id."""
    ids = [r.get("id") for r in rows]
    check(f"{scenario}: no duplicate row records after rebuilds",
          len(ids) == len(set(ids)), f"got {ids!r}")


def cargo_scenario(port: int, bid: int, bpixel, vp: dict,
                    uid: int) -> None:
    print("== cargo Contents panel ==")
    # The `Contents` context-menu route is a REQUIRED observation, not an
    # optional one (#1286 requirement 4). It used to sit inside `if
    # bpixel:`, so a failed localization silently REMOVED the check and
    # the run still printed "all checks passed" — the one shape that lets
    # a future regression re-enter the fallback path unnoticed. Failing
    # the localization here is what makes the fallback diagnostic-only.
    if not check("located the storage building's own screen pixel "
                 "(the real context-menu route)", bool(bpixel)):
        # Everything #1088 changed lives INSIDE the panel, so still open
        # it — through its real public entry point — and keep every check
        # below real. The run has already been marked failed above.
        print("  (building pixel not located — opening the panel through "
              "its public entry point instead; this run is a FAILURE, the "
              "fallback only keeps the remaining panel checks meaningful)")
        send(port, "require('scripts.cargo_inventory_panel')"
                   f".openFor('building', {bid}, 200, 200); return 'ok'")
        time.sleep(0.5)
    else:
        bpx, bpy = win_to_fb(vp, *bpixel)
        send(port, f"return input.moveMouse({bpx}, {bpy})")
        send(port, f"return input.click({bpx}, {bpy}, 'right')")
        time.sleep(0.4)
        contents = find_widget(port, "Contents")
        if check("cargo context menu offers 'Contents'", bool(contents)):
            click_widget_center(port, contents)
            time.sleep(0.5)

    opened = send(port, "return require('scripts.cargo_inventory_panel')"
                        ".isOpen()").strip()
    if not check("cargo Contents panel opened", opened == "true", f"got {opened!r}"):
        return

    # -- #1234: the window records the BUILDING endpoint's identity, and
    #    its chrome names the real building and its real storage load.
    ident = send_json(port, "local s = require('scripts.cargo_inventory_panel')"
                            ".getLevel(1) or {src={}};"
                            " return {kind = s.src.endpointKind, id = s.src.id}")
    check("the window records a 'building' endpoint identity",
          isinstance(ident, dict) and ident.get("kind") == "building"
          and ident.get("id") == bid, f"got {ident!r}")
    chrome = panel_chrome(port)
    binfo = send_json(port, f"return building.getInfo({bid})")
    want_name = (binfo or {}).get("displayName") if isinstance(binfo, dict) else None
    check("the cargo window's title names the building",
          bool(want_name) and chrome.get("title") == want_name,
          f"got {chrome.get('title')!r} want {want_name!r}")
    # Since #1237 the header reports the REMEMBERED weight, not the live
    # one. Every stack here was deposited through the real verb, which
    # reveals on commit, so the two agree — and that agreement is worth
    # asserting separately: it is what makes "renders the snapshot"
    # compatible with "a freshly-used container looks current".
    k = knowledge(port, bid)
    cap = send(port, f"return building.getStorageCapacity({bid})").strip()
    used = send(port, f"return building.getStorageWeight({bid})").strip()
    check("a container stocked through the real deposit verb has a "
          "remembered record matching its live storage",
          k.get("state") == "known"
          and abs(float(k.get("storedWeight") or 0) - float(used)) < 0.01
          and abs(float(k.get("capacity") or 0) - float(cap)) < 0.01,
          f"knowledge {k!r} vs live {used}/{cap}")
    want_sub = "Storage: %.2f / %.2f kg" % (float(k.get("storedWeight") or 0),
                                            float(k.get("capacity") or 0))
    check("the cargo window's header reports the building's remembered "
          "stored weight and its live capacity",
          chrome.get("subtitle") == want_sub,
          f"got {chrome.get('subtitle')!r} want {want_sub!r}")
    want_age, _ = expected_age(port, bid)
    check("a known container shows an 'as of…' age derived from its "
          "revealedAt and the game clock",
          want_age is not None
          and chrome_text(port, "cargo_inv_age") == want_age,
          f"got {chrome_text(port, 'cargo_inv_age')!r} want {want_age!r}")

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
                  " local s = require('scripts.cargo_inventory_panel').getLevel(1) or {};"
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
            active = send(port, "return (require('scripts.cargo_inventory_panel')"
                                ".getLevel(1) or {}).activeTab").strip().strip('"')
            check("the panel records the clicked category as its selection",
                  active == label, f"got {active!r}")

            # -- A resize keeps the panel open on the same target AND tab.
            send(port, "return engine.setWindowSize("
                       f"{vp['fb_w'] - 160}, {vp['fb_h'] - 120})")
            time.sleep(1.5)
            still_open = send(port, "return require('scripts.cargo_inventory_panel')"
                                    ".isOpen()").strip()
            check("a resize keeps the cargo panel open", still_open == "true",
                  f"got {still_open!r}")
            after = send_json(port, "local s = require("
                                    "'scripts.cargo_inventory_panel')"
                                    ".getLevel(1) or {src={}};"
                                    " return {kind = s.src.endpointKind,"
                                    " id = s.src.id, tab = s.activeTab}")
            check("a resize preserves the panel's endpoint identity AND "
                  "selected tab",
                  isinstance(after, dict) and after.get("kind") == "building"
                  and after.get("id") == bid
                  and after.get("tab") == label, f"got {after!r}")
            resized_rows = item_rows(port, CARGO_LIST_ID)
            check_no_duplicate_rows(port, "cargo (after resize)", resized_rows)
            send(port, "return engine.setWindowSize("
                       f"{vp['fb_w']}, {vp['fb_h']})")
            time.sleep(1.5)

            # Back to All so the right-click scenario sees every row.
            all_tab = next((t for t in tab_boxes(port, CARGO_LIST_ID)
                            if (t.get("label") or "").startswith("All")), None)
            if all_tab:
                click_widget_center(port, all_tab)
                time.sleep(0.5)

    # -- A real right-click reaches the representative instance, and the
    #    menu it opens is #1249's queued Retrieve gesture. The acolyte is
    #    selected first because Retrieve resolves its executor through the
    #    shared selection rule and is OMITTED (never disabled) with no
    #    eligible unit -- which is itself checked below.
    rows = item_rows(port, CARGO_LIST_ID)
    target_row = next((r for r in rows if r.get("defName") == "steel_bar"), None)
    if check("cargo steel_bar row still rendered for the right-click check",
             bool(target_row)):
        # With NOTHING selected there is no retriever, so the gesture is
        # absent rather than greyed out -- the disabled placeholder this
        # issue retired is exactly what must not come back.
        send(port, "return unit.deselectAll()")
        time.sleep(0.4)
        right_click_widget_center(port, target_row)
        time.sleep(0.4)
        labels = [w.get("label") for w in widgets(port) if w.get("label")]
        check("with no unit selected a cargo row offers NO transfer entry "
              "-- omitted, never a disabled row",
              not any(l.startswith(("Retrieve", "Withdraw")) for l in labels),
              f"menu labels: {labels!r}")
        close_menu(port)

        send(port, f"return unit.select({uid})")
        time.sleep(0.4)
        rows = item_rows(port, CARGO_LIST_ID)
        target_row = next((r for r in rows
                           if r.get("defName") == "steel_bar"), None)
    if check("cargo steel_bar row still rendered with a unit selected",
             bool(target_row)):
        ids = target_row.get("instanceIds") or []
        right_click_widget_center(port, target_row)
        time.sleep(0.4)
        labels = [w.get("label") for w in widgets(port) if w.get("label")]
        check("right-clicking a cargo row opens its Retrieve menu",
              "Retrieve 1" in labels, f"menu labels: {labels!r}")
        # The merged steel_bar row stands for several instances, so it
        # must offer the batch entry too -- and the ids it names are the
        # row's own, which is what the widget now reports.
        check("a MERGED cargo row offers Retrieve all beside Retrieve 1",
              ("Retrieve all" in labels) == (len(ids) > 1),
              f"menu labels: {labels!r} for instanceIds {ids!r}")
        check("neither retired player path survives in the cargo row menu",
              not any(l.startswith("Withdraw") or l.startswith("Store in ")
                      for l in labels),
              f"menu labels: {labels!r}")
        entry = find_widget(port, "Retrieve all") or find_widget(port, "Retrieve 1")
        expected = ids if (len(ids) > 1 and find_widget(port, "Retrieve all")) \
            else ids[:1]
        if check("a Retrieve entry is clickable", bool(entry)):
            before = orders_of(port, uid)
            click_widget_center(port, entry)
            time.sleep(0.6)
            after = orders_of(port, uid)
            new = [o for o in after if o not in before]
            # The whole promotion: the acolyte is nowhere near the cargo
            # and the gesture still succeeded, because it queued an order
            # for the unit job to walk rather than moving an item now.
            if check("firing Retrieve queues exactly one durable transfer "
                     "order at a distance", len(new) == 1,
                     f"before={before!r} after={after!r}"):
                check("the queued order runs FROM the cargo TO the "
                      "selected unit, naming the row's own instance ids",
                      new[0] == f"building:{bid}>unit:{uid}"
                                f"[{','.join(str(i) for i in expected)}]",
                      f"got {new[0]!r} for instanceIds {ids!r}")
        else:
            close_menu(port)

    send(port, "require('scripts.cargo_inventory_panel').closeIfOpen();"
               " return 'ok'")
    time.sleep(0.3)


def unit_endpoint_scenario(port: int, uid: int, wild_uid: int,
                            vp: dict) -> None:
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
                            ".getLevel(1) or {src={}};"
                            " return {kind = s.src.endpointKind, id = s.src.id}")
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
    # #1237 requirement 6: a unit knows its own contents, so there is no
    # staleness to date and no "as of…" line to draw.
    check("a unit endpoint renders NO age indicator",
          chrome_text(port, "cargo_inv_age") is None,
          f"got {chrome_text(port, 'cargo_inv_age')!r}")

    # -- Rows are that call's loose inventory, one per stack.
    rows = item_rows(port, CARGO_LIST_ID)
    want_defs = {c.get("defName") for c in contents if isinstance(c, dict)}
    got_defs = {r.get("defName") for r in rows}
    check("the unit window renders its loose inventory, one row per stack",
          got_defs == want_defs,
          f"got {sorted(d for d in got_defs if d)!r} "
          f"want {sorted(d for d in want_defs if d)!r}")
    check_no_duplicate_rows(port, "unit endpoint", rows)
    # #1238: a unit endpoint's rows route a right-click now, because a
    # container row has to be able to open its contents as the next
    # level. What they still expose is NO TRANSFER action: a plain row
    # opens no menu at all, and a container row's menu is exactly
    # "Contents".
    check("unit rows route a right-click (the inspection route)",
          all(r.get("rightClick") is True for r in rows),
          f"got {[r.get('rightClick') for r in rows]!r}")
    plain_row = next((r for r in rows
                      if r.get("defName") not in (None, "first_aid_kit")), None)
    if check("a plain (non-container) unit row is located", bool(plain_row)):
        right_click_widget_center(port, plain_row)
        time.sleep(0.4)
        labels = [w.get("label") for w in widgets(port)]
        check("a plain unit row opens NO menu — no Contents, and no "
              "transfer action either",
              not any(l == "Contents"
                      or (l or "").startswith(("Withdraw", "Retrieve"))
                      for l in labels),
              f"menu labels: {labels!r}")
        close_menu(port)
    kit_row = next((r for r in rows
                    if r.get("defName") == "first_aid_kit"), None)
    if check("the acolyte's carried kit is rendered as a unit row",
             bool(kit_row)):
        right_click_widget_center(port, kit_row)
        time.sleep(0.4)
        labels = [w.get("label") for w in widgets(port) if w.get("label")]
        check("a CONTAINER unit row offers exactly the inspection entry",
              "Contents" in labels
              and not any(l.startswith(("Withdraw", "Retrieve"))
                          for l in labels),
              f"menu labels: {labels!r}")
        close_menu(port)

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
            send(port, "return engine.setWindowSize("
                       f"{vp['fb_w'] - 160}, {vp['fb_h'] - 120})")
            time.sleep(1.5)
            check("a resize keeps the unit window open",
                  send(port, "return require('scripts.cargo_inventory_panel')"
                             ".isOpen()").strip() == "true")
            after = send_json(port, "local s = require("
                                    "'scripts.cargo_inventory_panel')"
                                    ".getLevel(1) or {src={}};"
                                    " return {kind = s.src.endpointKind,"
                                    " id = s.src.id, tab = s.activeTab}")
            check("a resize preserves the unit endpoint identity AND tab",
                  isinstance(after, dict) and after.get("kind") == "unit"
                  and after.get("id") == uid and after.get("tab") == label,
                  f"got {after!r}")
            send(port, "return engine.setWindowSize("
                       f"{vp['fb_w']}, {vp['fb_h']})")
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
                            ".getLevel(1) or {};"
                            " return {panel = s.panelId ~= nil,"
                            " list = s.listId ~= nil}")
    check("a refused open creates no panel or list state",
          isinstance(state, dict) and state.get("panel") is False
          and state.get("list") is False, f"got {state!r}")

    # An unknown kind is refused the same way.
    unknown = send(port, "return require('scripts.cargo_inventory_panel')"
                         ".openFor('item_container', 1, 240, 240)").strip()
    check("the manager refuses an unknown endpoint kind", unknown == "false",
          f"got {unknown!r}")


def knowledge_scenario(port: int, unseen_bid: int, empty_bid: int,
                        uid: int) -> None:
    """#1237: the three knowledge states, the age, and the no-reveal rule,
    all read off the REAL rendered window.

    Runs with the simulation stopped except for the one deliberate
    interval that advances the game clock, so every "engine says X, the
    label says Y" pair describes a single instant."""
    print("== last-known container contents (#1237) ==")
    set_paused(port, True)

    # -- Never inspected. The fixture is worker-built and left at zero
    #    progress, so nothing has ever seeded or revealed it.
    k = knowledge(port, unseen_bid)
    if not check("the engine reports the worker-built fixture as genuinely "
                 "never-inspected", k.get("state") == "unknown", f"got {k!r}"):
        return
    cap = k.get("capacity")
    check("its capacity is still live and positive even with no record",
          isinstance(cap, (int, float)) and cap > 0, f"got {cap!r}")

    if not check("the container window opens on a never-inspected container",
                 open_window_on(port, unseen_bid)):
        return
    chrome = panel_chrome(port)
    want_sub = "Storage: unknown / %.2f kg" % float(cap)
    check("its header reads the stored weight as UNKNOWN while still "
          "showing the live capacity",
          chrome.get("subtitle") == want_sub,
          f"got {chrome.get('subtitle')!r} want {want_sub!r}")
    check("a never-inspected container renders NO rows",
          not item_rows(port, CARGO_LIST_ID))
    check("it renders an explicit never-inspected line, not an empty list",
          chrome_text(port, "cargo_inv_empty")
              == "Contents unknown (never inspected)",
          f"got {chrome_text(port, 'cargo_inv_empty')!r}")
    check("a never-inspected container shows no age at all",
          chrome_text(port, "cargo_inv_age") is None,
          f"got {chrome_text(port, 'cargo_inv_age')!r}")

    # -- Requirement 4: opening reveals nothing. Tick it for a while,
    #    including a real tab interaction, then re-ask the engine.
    time.sleep(1.5)
    for t in tab_boxes(port, CARGO_LIST_ID):
        click_widget_center(port, t)
        time.sleep(0.3)
    after = knowledge(port, unseen_bid)
    check("opening (and interacting with) the window reveals NOTHING — the "
          "container is still never-inspected",
          after.get("state") == "unknown" and after.get("revealedAt") is None,
          f"got {after!r}")
    send(port, "require('scripts.cargo_inventory_panel').closeIfOpen();"
               " return 'ok'")
    time.sleep(0.3)

    # -- Known-EMPTY: a second instant-built fixture, seeded at Built and
    #    deliberately never stocked.
    k = knowledge(port, empty_bid)
    if not check("the engine reports the unstocked instant-built fixture as "
                 "known-empty", k.get("state") == "empty", f"got {k!r}"):
        return
    if not check("the container window opens on a known-empty container",
                 open_window_on(port, empty_bid)):
        return
    chrome = panel_chrome(port)
    want_sub = "Storage: %.2f / %.2f kg" % (float(k.get("storedWeight") or 0),
                                            float(k.get("capacity") or 0))
    check("a known-empty container reports a real zero stored weight, not "
          "an unknown one",
          chrome.get("subtitle") == want_sub,
          f"got {chrome.get('subtitle')!r} want {want_sub!r}")
    check("it renders the known-empty line, distinct from the unknown one",
          chrome_text(port, "cargo_inv_empty") == "(empty)",
          f"got {chrome_text(port, 'cargo_inv_empty')!r}")
    want_age, _ = expected_age(port, empty_bid)
    check("a known-empty container shows an age derived from its own "
          "revealedAt and the game clock",
          want_age is not None
          and chrome_text(port, "cargo_inv_age") == want_age,
          f"got {chrome_text(port, 'cargo_inv_age')!r} want {want_age!r}")

    # -- Requirement 5: a completed movement refreshes an OPEN window.
    before_revealed = (knowledge(port, empty_bid) or {}).get("revealedAt")
    set_paused(port, False)
    send(port, f"return unit.addItem({uid}, 'quinoa_sack')")
    send(port, f"return unit.depositToCargo({uid}, {empty_bid},"
               " 'quinoa_sack')", timeout=20.0)
    moved = poll_until(8.0, lambda: "quinoa_sack" in
                       {r.get("defName")
                        for r in item_rows(port, CARGO_LIST_ID)},
                       interval=0.4)
    set_paused(port, True)
    time.sleep(0.5)
    check("a completed deposit refreshes the ALREADY-OPEN window to the "
          "moved item, with no reopen", bool(moved),
          f"rows: {[r.get('defName') for r in item_rows(port, CARGO_LIST_ID)]!r}")
    k = knowledge(port, empty_bid)
    check("the engine's record is now 'known' with a newer revealedAt",
          k.get("state") == "known"
          and isinstance(k.get("revealedAt"), (int, float))
          and (before_revealed is None
               or k["revealedAt"] > before_revealed),
          f"got {k!r} (was revealed at {before_revealed!r})")
    chrome = panel_chrome(port)
    want_sub = "Storage: %.2f / %.2f kg" % (float(k.get("storedWeight") or 0),
                                            float(k.get("capacity") or 0))
    check("the header follows the refreshed remembered weight",
          chrome.get("subtitle") == want_sub,
          f"got {chrome.get('subtitle')!r} want {want_sub!r}")
    check("the known-empty line is gone now that there are remembered rows",
          chrome_text(port, "cargo_inv_empty") is None,
          f"got {chrome_text(port, 'cargo_inv_empty')!r}")
    want_age, _ = expected_age(port, empty_bid)
    check("and the age is the fresh one",
          want_age is not None
          and chrome_text(port, "cargo_inv_age") == want_age,
          f"got {chrome_text(port, 'cargo_inv_age')!r} want {want_age!r}")

    # -- Requirement 3 read literally: the SAME fixed revealedAt observed
    #    at two increasing game-clock readings. engine.gameTime() only
    #    advances while unpaused, so the interval is a real unpause; the
    #    fixed revealedAt is re-asserted afterwards so a stray reveal
    #    during it fails the check rather than faking a pass.
    t0 = game_time(port)
    age0 = chrome_text(port, "cargo_inv_age")
    revealed0 = k.get("revealedAt")
    set_paused(port, False)
    time.sleep(12.0)
    set_paused(port, True)
    time.sleep(0.5)
    t1 = game_time(port)
    k1 = knowledge(port, empty_bid)
    age1 = chrome_text(port, "cargo_inv_age")
    check("the game clock advanced across the unpaused interval", t1 > t0,
          f"got {t0!r} -> {t1!r}")
    check("the observation itself did not move (same revealedAt)",
          k1.get("revealedAt") == revealed0,
          f"got {k1.get('revealedAt')!r} want {revealed0!r}")
    want0 = ("as of " + format_age(t0 - float(revealed0))
             if isinstance(revealed0, (int, float)) else None)
    want1 = ("as of " + format_age(t1 - float(revealed0))
             if isinstance(revealed0, (int, float)) else None)
    check("the displayed age matches the derivation at BOTH readings",
          age0 == want0 and age1 == want1,
          f"got {age0!r}/{age1!r} want {want0!r}/{want1!r}")
    check("and it visibly ADVANCED as game time passed",
          age1 != age0, f"still {age0!r} after {t1 - t0:.1f} game seconds")

    send(port, "require('scripts.cargo_inventory_panel').closeIfOpen();"
               " return 'ok'")
    time.sleep(0.3)


def item_contents_scenario(port: int, mule_uid: int, uid: int) -> None:
    print("== first-aid-kit Contents panel ==")
    # The technomule's own starting inventory carries a PRE-STOCKED kit
    # (data/units/technomule.yaml -> starting_inventory). Since #1418 so
    # does every other creation path, so the empty-state case below uses
    # a def that AUTHORS no contents rather than a creation path that
    # used to skip them.
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
    # #1238: this level ROUTES right-clicks now -- a container row has to
    # be able to open the next level. What it still offers is nothing
    # else: no transfer action anywhere, and a plain (non-container) row
    # opens no menu at all.
    check("item-contents rows route a right-click (the inspection route)",
          all(r.get("rightClick") is True for r in rows),
          f"got {[r.get('rightClick') for r in rows]!r}")
    plain = next((r for r in rows if r.get("defName") == "bandage"), None)
    if check("a plain (non-container) item-contents row is located",
             bool(plain)):
        right_click_widget_center(port, plain)
        time.sleep(0.4)
        labels = [w.get("label") for w in widgets(port)]
        check("a plain row on this render-only level opens NO menu -- "
              "no Contents, and no transfer action either",
              not any(l in ("Contents",)
                      or (l or "").startswith(("Withdraw", "Retrieve"))
                      for l in labels),
              f"menu labels: {labels!r}")
        close_menu(port)
    check("item-contents panel renders NO tab strip",
          not tab_boxes(port, ITEM_CONTENTS_LIST_ID))

    # -- The empty state, on a container that is GENUINELY empty: a def
    #    authoring no `contents:` key decodes to an empty list, so it
    #    materializes empty however it was created. Since #1418 that is
    #    the only way to get one -- a first-aid kit now arrives stocked
    #    from `unit.addItem` too -- and this state is real UI behaviour,
    #    so it keeps its coverage rather than going away with the
    #    creation-path quirk that used to supply it.
    send(port, "require('scripts.item_contents_panel').closeIfOpen(); return 'ok'")
    time.sleep(0.3)
    send(port, f"return unit.addItem({uid}, '{DEF_EMPTY_BOX}')")
    # Row COUNT over the console, not the serialized table: an empty Lua
    # table has no array/object distinction to preserve, so `#r` is what
    # separates "container with no contents" (0) from "no such container"
    # (-1).
    engine_empty = send(
        port, f"local r=unit.getItemContents({uid}, '{DEF_EMPTY_BOX}'); "
              "if not r then return -1 end; return #r").strip().strip('"')
    check("the fixture container really is empty at the engine level",
          engine_empty in ("0", "0.0"), f"row count {engine_empty!r}")
    send(port, "require('scripts.item_contents_panel').openFor("
               f"{uid}, '{DEF_EMPTY_BOX}', 400, 300); return 'ok'")
    time.sleep(0.6)
    check("an empty container still opens its panel",
          send(port, "return require('scripts.item_contents_panel')"
                     ".isOpen()").strip() == "true")
    check("an empty container renders no rows (the '(empty)' state)",
          not item_rows(port, ITEM_CONTENTS_LIST_ID))
    send(port, "require('scripts.item_contents_panel').closeIfOpen(); return 'ok'")
    send(port, f"return unit.removeItem({uid}, '{DEF_EMPTY_BOX}')")


def unit_inv_row(port: int, def_name: str, category: str):
    """One unit-inventory row, filtering to its category first when the
    All view is too short to render it.

    The unit-info inventory is a fixed-height HUD section: the widget
    renders only the rows that fit, and `itemList.dump()` reports only
    rendered rows, so a row past the fold is simply absent. Selecting
    its category is the same gesture a player has, and the widget's own
    `fitVertical` guarantees at least one row is always reachable — so
    with one row per fixture category this resolves whatever the
    section's capacity turns out to be. An empty category (the item is
    not carried, so there is nothing to filter to) looks in the current
    view only."""
    def find():
        return next((r for r in item_rows(port, UNIT_INV_LIST_ID)
                     if r.get("defName") == def_name), None)
    row = find()
    if row or not category:
        return row
    tab = next((t for t in tab_boxes(port, UNIT_INV_LIST_ID)
                if (t.get("label") or "").split(" (")[0] == category), None)
    if not tab:
        return None
    click_widget_center(port, tab)
    time.sleep(0.8)
    return find()


def temperature_scenario(port: int, uid: int, empty_bid: int) -> None:
    """#1268: both raw-item hosts present a row's TRACKED temperature.

    Every seeded value goes in through `unit.setItemTemp` (which reaches
    inventory, equipment and accessories alike) and every rendered value
    comes back out through the widget's own dump — `text`/`rawText` for
    what the row label says, `tooltipHint` for the tooltip line. Neither
    surface has any other read path: no Lua API reports a rendered label
    or an element's tooltip content.

    The SIMULATION IS PAUSED throughout. Item cooling runs on the game
    clock and freezes with the pause flag (World/Thread/ItemTemp.hs), so
    an unpaused run could cross a displayed degree boundary between
    seeding a value and asserting on it.

    The exact seeds are chosen to make the summary unfakeable: the two
    hot logs straddle 42 from BOTH sides (41.6 and 42.3), so a
    presentation reading only the representative would still say "42°C"
    — and the third log is left at ambient, which only a whole-group
    summary can report."""
    print("== tracked temperature presentation (#1268) ==")
    set_paused(port, True)

    # -- Strip the acolyte first. The unit-info inventory is a
    #    fixed-height HUD section and the widget renders only the rows
    #    that FIT: at the default 1280x900 a spawned acolyte's own kit
    #    fills it, and the section was measured rendering one row of
    #    nine. Discarding that kit leaves one row per fixture category,
    #    which — with `unit_inv_row`'s tab fallback below — is what makes
    #    each fixture row reachable whatever the section's capacity is.
    #    The first-aid kit stays: item_contents_scenario still needs it.
    for slot in (send_json(port, "local out = {};"
                                 " for s in pairs(equipment.getLoadout("
                                 f"{uid}) or {{}}) do out[#out+1] = s end;"
                                 " return out") or []):
        send(port, f"return equipment.unequip({uid}, '{slot}')")
    worn = send(port, f"local a = equipment.getAccessories({uid});"
                      " return a and #a or 0").strip()
    for i in range(int(float(worn or 0)), 0, -1):
        send(port, f"return equipment.unequipAccessory({uid}, {i})")
    # Discarded rather than deposited: unequipping the whole kit could
    # otherwise overrun the cargo's declared capacity, and a refused
    # deposit would silently leave the row it was meant to remove.
    for it in (send_json(port, f"return unit.getInventory({uid})") or []):
        if it.get("defName") != "first_aid_kit":
            send(port, f"return unit.removeItem({uid}, '{it['defName']}')")

    # -- Seed a three-member group: two tracked, one ambient. A fresh
    #    def, so nothing already in the acolyte's inventory merges in.
    for _ in range(3):
        send(port, f"return unit.addItem({uid}, 'wood_log')")
    logs = [it for it in (send_json(port, f"return unit.getInventory({uid})")
                          or []) if it.get("defName") == "wood_log"]
    if not check("three wood logs are carried for the temperature fixture",
                 len(logs) == 3, f"got {len(logs)}"):
        set_paused(port, False)
        return
    # The third stays ambient — only a whole-group summary can report it.
    hot_a, hot_b = (int(logs[i]["instanceId"]) for i in (0, 1))
    send(port, f"return unit.setItemTemp({uid}, {hot_a}, 41.6)")
    send(port, f"return unit.setItemTemp({uid}, {hot_b}, 42.3)")

    # -- An EQUIPPED row too: before #1268 `equipment.getLoadout`'s slot
    #    table carried no `temp` field at all, so this is the one path a
    #    Lua-only change could not have reached.
    send(port, f"return unit.addItem({uid}, 'pick_steel')")
    pick = send(port, f"local inv = unit.getInventory({uid});"
                      " for _, it in ipairs(inv) do"
                      " if it.defName == 'pick_steel' then"
                      " return it.instanceId end end; return nil").strip()
    if not check("a pick was added to equip", pick not in ("", "nil", "null"),
                 f"got {pick!r}"):
        set_paused(port, False)
        return
    pick_id = int(float(pick))
    equipped = send(port, f"return equipment.equip({uid}, 'right_hand',"
                          f" 'pick_steel', {pick_id})").strip()
    check("the pick equips into the acolyte's right hand", equipped == "true",
          f"got {equipped!r}")
    send(port, f"return unit.setItemTemp({uid}, {pick_id}, 88.4)")
    slot_temp = send(port, f"local lo = equipment.getLoadout({uid});"
                           " local s = lo and lo['right_hand'];"
                           " return s and tostring(s.temp) or 'nil'"
                           ).strip().strip('"')
    check("equipment.getLoadout exposes the equipped item's tracked temp",
          slot_temp not in ("", "nil", "null"), f"got {slot_temp!r}")

    # -- Unit inventory: the rows the acolyte's own panel renders.
    send(port, f"return unit.select({uid})")
    time.sleep(0.9)
    def inv_category(def_name: str) -> str:
        return send(port, f"local inv = unit.getInventory({uid});"
                          " for _, it in ipairs(inv) do"
                          f" if it.defName == '{def_name}' then"
                          " return it.category end end; return ''"
                          ).strip().strip('"')

    log_cat = inv_category("wood_log")
    kit_cat = inv_category("first_aid_kit")
    pick_cat = send(port, f"local lo = equipment.getLoadout({uid});"
                          " local s = lo and lo['right_hand'];"
                          " return s and s.category or ''"
                          ).strip().strip('"')

    log_row = unit_inv_row(port, "wood_log", log_cat)
    if check("the unit inventory renders a wood-log row", bool(log_row),
             f"category {log_cat!r}, rendered "
             f"{[r.get('defName') for r in item_rows(port, UNIT_INV_LIST_ID)]!r}"):
        check("the three logs stay ONE group despite differing temperatures",
              log_row.get("count") == 3, f"got {log_row.get('count')!r}")
        check("the log row summarizes EVERY member, not the representative",
              log_row.get("tempSummary") == "ambient + 42°C",
              f"got {log_row.get('tempSummary')!r}")
        check("the log row's text carries that summary",
              "ambient + 42°C" in (log_row.get("rawText") or ""),
              f"got {log_row.get('rawText')!r}")
        check("the log row's tooltip carries it as a labeled line",
              (log_row.get("tooltipHint") or "")
                  .endswith("temperature: ambient + 42°C"),
              f"got {log_row.get('tooltipHint')!r}")

    kit_row = unit_inv_row(port, "first_aid_kit", kit_cat)
    if check("the unit inventory renders an untracked row too",
             bool(kit_row), f"category {kit_cat!r}"):
        check("an item at ambient reads 'ambient' rather than blank",
              kit_row.get("tempSummary") == "ambient",
              f"got {kit_row.get('tempSummary')!r}")

    # -- The EQUIPPED row, and the stability contract driven on it. Its
    #    category tab stays selected across the three reads below, so the
    #    handle comparison is between two renders of the same row.
    pick_row = unit_inv_row(port, "pick_steel", pick_cat)
    if not check("the unit inventory renders the EQUIPPED pick row",
                 bool(pick_row), f"category {pick_cat!r}, rendered "
                 f"{[r.get('defName') for r in item_rows(port, UNIT_INV_LIST_ID)]!r}"):
        set_paused(port, False)
        return
    check("the equipped row presents its own tracked temperature",
          pick_row.get("tempSummary") == "88°C",
          f"got {pick_row.get('tempSummary')!r}")
    check("the equipped row's text and tooltip agree on it",
          "88°C" in (pick_row.get("rawText") or "")
          and (pick_row.get("tooltipHint") or "")
                  .endswith("temperature: 88°C"),
          f"text={pick_row.get('rawText')!r} "
          f"tip={pick_row.get('tooltipHint')!r}")

    # -- Cooling that stays inside one displayed degree must not rebuild
    #    the row; crossing the boundary must. Compared by element handle,
    #    which the widget only replaces on a real rebuild.
    send(port, f"return unit.setItemTemp({uid}, {pick_id}, 88.1)")
    time.sleep(0.9)
    within = unit_inv_row(port, "pick_steel", pick_cat)
    check("a raw change inside one displayed degree rebuilds nothing",
          bool(within) and pick_row.get("handle") == within.get("handle")
          and within.get("tempSummary") == "88°C",
          f"{pick_row.get('handle')!r} -> "
          f"{within and within.get('handle')!r}, "
          f"{within and within.get('tempSummary')!r}")
    send(port, f"return unit.setItemTemp({uid}, {pick_id}, 70.0)")
    time.sleep(0.9)
    crossed = unit_inv_row(port, "pick_steel", pick_cat)
    check("crossing a displayed degree DOES refresh the row",
          bool(crossed) and crossed.get("tempSummary") == "70°C"
          and "70°C" in (crossed.get("rawText") or ""),
          f"got {crossed and crossed.get('tempSummary')!r}")

    # -- The container window, on BOTH endpoint kinds. The building one
    #    is reached by depositing an EXACT seeded instance, so the row it
    #    renders is provably the one that was heated. It goes into the
    #    hitherto-EMPTY fixture rather than the main cargo, which already
    #    holds ambient logs the hot one would merge with — the two
    #    endpoints must land on DIFFERENT summaries here, or one of them
    #    could pass on the other's string.
    stored = send(port, f"return unit.depositToCargo({uid}, {empty_bid},"
                        f" 'wood_log', {hot_b})").strip()
    check("the hot log deposits into the empty cargo as an exact instance",
          stored == "true", f"got {stored!r}")
    for kind, ident, want in (("building", empty_bid, "42°C"),
                              ("unit", uid, "ambient + 42°C")):
        opened = send(port, "return require('scripts.cargo_inventory_panel')"
                            f".openFor('{kind}', {ident}, 240, 240)").strip()
        if not check(f"the container window opens on the {kind} endpoint",
                     opened == "true", f"got {opened!r}"):
            continue
        time.sleep(0.6)
        row = next((r for r in item_rows(port, CARGO_LIST_ID)
                    if r.get("defName") == "wood_log"), None)
        if not check(f"the {kind} window renders a wood-log row", bool(row)):
            continue
        check(f"the {kind} window's row presents the same summary",
              row.get("tempSummary") == want,
              f"got {row.get('tempSummary')!r} want {want!r}")
        check(f"the {kind} window's row TEXT carries it",
              want in (row.get("rawText") or ""),
              f"got {row.get('rawText')!r}")
        # #1268 gave this window a row tooltip it did not have before,
        # bounded to the row's display text plus the temperature line.
        check(f"the {kind} window's row gained a bounded temperature tooltip",
              row.get("tooltipHint") == f"temperature: {want}"
              and want not in (row.get("tooltipText") or ""),
              f"hint={row.get('tooltipHint')!r} "
              f"text={row.get('tooltipText')!r}")
    send(port, "require('scripts.cargo_inventory_panel').closeIfOpen();"
               " return 'ok'")
    set_paused(port, False)


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
        # #1249: "Store" names the OPEN CONTAINER WINDOW's endpoint, and
        # no window is open here -- so the player has named no target and
        # the entry is absent. This is the negative half of
        # store_gesture_scenario below.
        check("with no container window open a unit-inventory row offers "
              "no Store entry at all",
              not any(l.startswith("Store") for l in labels),
              f"menu labels: {labels!r}")
        close_menu(port)


def store_gesture_scenario(port: int, uid: int, bid: int) -> None:
    """#1249: "Store" on a unit-inventory row, against the REAL rendered
    menu of the REAL open container window.

    The acolyte stands nowhere near the cargo hold, which is the whole
    point: the retired path enumerated ADJACENT cargos and deposited on
    the spot, so this gesture could not have existed before. What makes
    it work is that the open window names the target and the queued
    order (#1246) carries the walk."""
    print("== #1249 Store gesture (unit inventory -> open window) ==")
    send(port, f"return unit.select({uid})")
    send(port, "require('scripts.cargo_inventory_panel')"
               f".openFor('building', {bid}, 200, 200); return 'ok'")
    time.sleep(0.8)
    if not check("the container window is open on the cargo endpoint for "
                 "the Store gesture",
                 send(port, "return require('scripts.cargo_inventory_panel')"
                            ".isOpen()").strip() == "true"):
        return

    row = unit_inv_row(port, "steel_bar", "Materials")
    if not check("a merged unit-inventory row is located for Store",
                 bool(row)):
        send(port, "require('scripts.cargo_inventory_panel').closeIfOpen();"
                   " return 'ok'")
        return
    ids = row.get("instanceIds") or []
    right_click_widget_center(port, row)
    time.sleep(0.4)
    labels = [w.get("label") for w in widgets(port) if w.get("label")]
    check("with a container window open, a unit-inventory row offers Store 1",
          "Store 1" in labels, f"menu labels: {labels!r}")
    check("a MERGED unit-inventory row offers Store all beside Store 1",
          ("Store all" in labels) == (len(ids) > 1),
          f"menu labels: {labels!r} for instanceIds {ids!r}")
    check("the retired adjacent-cargo entry is gone from the row menu",
          not any(l.startswith("Store in ") for l in labels),
          f"menu labels: {labels!r}")

    entry = find_widget(port, "Store all") or find_widget(port, "Store 1")
    expected = ids if find_widget(port, "Store all") else ids[:1]
    if check("a Store entry is clickable", bool(entry)):
        before = orders_of(port, uid)
        cam_before = send(port, "local x, y = camera.getPosition();"
                                " return string.format('%.3f,%.3f,%.3f',"
                                " x or 0, y or 0, camera.getZoom())")
        click_widget_center(port, entry)
        time.sleep(0.6)
        after = orders_of(port, uid)
        new = [o for o in after if o not in before]
        if check("firing Store queues exactly one durable transfer order "
                 "with no adjacency", len(new) == 1,
                 f"before={before!r} after={after!r}"):
            check("the queued order runs FROM the unit TO the open "
                  "window's endpoint, naming every id the row stands for",
                  new[0] == f"unit:{uid}>building:{bid}"
                            f"[{','.join(str(i) for i in expected)}]",
                  f"got {new[0]!r} for instanceIds {ids!r}")
        cam_after = send(port, "local x, y = camera.getPosition();"
                               " return string.format('%.3f,%.3f,%.3f',"
                               " x or 0, y or 0, camera.getZoom())")
        check("the Store gesture moved the camera not at all (D-4)",
              cam_after == cam_before,
              f"before={cam_before!r} after={cam_after!r}")
    else:
        close_menu(port)

    send(port, "require('scripts.cargo_inventory_panel').closeIfOpen();"
               " return 'ok'")
    time.sleep(0.3)



# --------------------------------------------------------------------------
# #1250: the Mode A escort session's rendered pair
# --------------------------------------------------------------------------

def pane_list_id(pane_key: str) -> str:
    """A debug-console expression naming one escort pane's widget
    instance — the same shape `level_list_id` gives a single-pane
    level, addressed through the manager's own pane accessor rather
    than by indexing `panes` positionally."""
    return ("(function() local c = require('scripts.cargo_inventory_panel');"
            f" local p = c.getPane(c.getLevel(1), '{pane_key}');"
            " return p and p.listId end)()")


def escort_session_scenario(port: int, bid: int, bax: int, bay: int,
                            vp: dict) -> None:
    """#1250: the rendered escort pair.

    Its own acolyte, spawned ON the cargo's anchor tile so the contract's
    footprint rule already reports it in reach — the WALK is the headless
    gate's business (and the AI's), while what needs a GPU here is the
    pair of real panels and the real row menus on them. Everything below
    is located through the widget oracle and the manager's dump; not one
    screen coordinate is written down.

    Deliberately last of the transfer scenarios: it opens a window at the
    base level, which replaces whatever else was open, and it leaves the
    stack empty again on the way out."""
    print("== #1250 escort session (the rendered flanking pair) ==")
    send(port, "require('scripts.cargo_inventory_panel').closeIfOpen();"
               " return 'ok'")
    esc_raw = send(port, f"return unit.spawn('acolyte', {bax}, {bay},"
                         " nil, 'player')")
    if not check("escort acolyte spawned on the cargo's own tile",
                 esc_raw.strip() not in ("", "nil", "null"), f"got {esc_raw!r}"):
        return
    esc_uid = int(float(esc_raw))
    # A freshly spawned acolyte carries the standing `find_water` goal,
    # whose search utility competes with the escort hold for this very
    # unit. Retiring it is what makes the arrival below a measurement of
    # the ESCORT rather than a race against thirst on whatever terrain
    # this run's world happened to generate.
    check("the escort's standing find_water goal is retired",
          clear_find_water(port, esc_uid))
    # ...and so is the one drive that outranks the hold outright. This
    # world reliably contains a bleeding ally by the time the transfer
    # scenarios run, `treat_ally` scans 60 tiles for one, and it scores
    # 8.0 against the escort's 7.5 — so whether this escort arrives at
    # all came down to which acolyte the medic squad ranked best that
    # run. Observed failing here on a run where the identical check had
    # passed twice; see `retire_medic_drive`, which #1251's own scenario
    # needs for exactly the same reason.
    check("the escort is out of the medic squad, so the arrival below "
          "measures the hold and not the medic ranking",
          retire_medic_drive(port, esc_uid))
    # Three MORE rations on top of the spawn loadout's own, so the row
    # is unambiguously merged and "Store all" has more than one instance
    # to name.
    send(port, f"for i = 1, 3 do unit.addItem({esc_uid}, 'rations') end;"
               " return 'ok'", timeout=20.0)

    # Pan the camera well away from the pair FIRST. The probe's earlier
    # scenarios already centred it on this very cargo, so a snap onto the
    # pair would otherwise be a no-op and unobservable — this is what
    # makes the D-4 check a real one rather than a tautology.
    send(port, f"camera.goToTile({bax + 60}, {bay + 60}); return 'ok'")
    time.sleep(0.5)
    cam_before = send(port, "local x, y = camera.getPosition();"
                            " return string.format('%.3f,%.3f', x or 0, y or 0)")
    made = send(port, "return tostring(require('scripts.transfer_session')"
                      f".create({esc_uid}, 'building', {bid}) ~= nil)")
    if not check("a Mode A session is created on the cargo endpoint",
                 made.strip().strip('"') == "true", f"got {made!r}"):
        return

    opened = poll_until(45.0, lambda: send(
        port, "local s = require('scripts.transfer_session').get();"
              " return s and s.phase or 'none'").strip('"') == "open",
        interval=0.5)
    if not check("the REAL unit AI holds the escort and opens the pair on "
                 "arrival", bool(opened),
                 # WHICH action owns the escort, and whether the session
                 # is even still there, are the two things that tell a
                 # timeout apart from a hold that lost to a higher band
                 # (this check has been seen to lose to `treat_ally`) or
                 # from panels that refused to open. Reported rather than
                 # left to a second run to guess at.
                 "escort running "
                 f"{ai_action(port, esc_uid)!r}, session phase "
                 + send(port,
                        "local s = require('scripts.transfer_session').get();"
                        " return s and s.phase or 'none'")):
        send(port, "require('scripts.transfer_session').clear(); return 'ok'")
        return
    time.sleep(0.6)

    d = stack_dump(port)
    lv = (d.get("levels") or [{}])[0]
    check("the pair is ONE stack level of the escort kind (D-9's stated "
          "exception), non-modal at the base",
          d.get("depth") == 1 and lv.get("kind") == "escort"
          and lv.get("paneCount") == 2 and lv.get("modal") is False,
          f"got {d!r}")
    panes = lv.get("panes") or []
    if check("both panes report rendered geometry", len(panes) == 2
             and all(isinstance(p.get("width"), (int, float)) for p in panes),
             f"got {panes!r}"):
        a, b = panes[0], panes[1]
        fb_w, fb_h = vp["fb_w"], vp["fb_h"]

        def in_frame(p):
            return (p["x"] >= 0 and p["y"] >= 0
                    and p["x"] + p["width"] <= fb_w
                    and p["y"] + p["height"] <= fb_h)

        check("both panels are clamped inside the framebuffer",
              in_frame(a) and in_frame(b),
              f"framebuffer {fb_w}x{fb_h}, panes {a!r} {b!r}")
        overlap = not (a["x"] + a["width"] <= b["x"]
                       or b["x"] + b["width"] <= a["x"]
                       or a["y"] + a["height"] <= b["y"]
                       or b["y"] + b["height"] <= a["y"])
        check("the two panels flank rather than overlap", not overlap,
              f"panes {a!r} {b!r}")
        check("the source unit is on the left and the destination on the "
              "right, reading from -> to",
              a.get("paneKey") == "source"
              and b.get("paneKey") == "destination" and a["x"] < b["x"],
              f"panes {a!r} {b!r}")

    cam_after = send(port, "local x, y = camera.getPosition();"
                           " return string.format('%.3f,%.3f', x or 0, y or 0)")
    check("opening the session snapped the camera onto the pair (D-4 — "
          "Mode A is the ONE gesture that moves it)",
          cam_after != cam_before,
          f"before={cam_before!r} after={cam_after!r}")

    # -- the real rendered row menus, one per pane, in both directions.
    src_rows = item_rows(port, pane_list_id("source"))
    dst_rows = item_rows(port, pane_list_id("destination"))
    check("both panes render their endpoint's rows",
          len(src_rows) > 0 and len(dst_rows) > 0,
          f"source={len(src_rows)} destination={len(dst_rows)}")

    def row_named(rows, def_name):
        for w in rows:
            if (w.get("defName") or "") == def_name:
                return w
        return None

    ration = row_named(src_rows, "rations")
    if check("the merged rations row is located on the source pane",
             bool(ration), f"rows {[w.get('defName') for w in src_rows]!r}"):
        right_click_widget_center(port, ration)
        time.sleep(0.4)
        labels = [w.get("label") for w in widgets(port) if w.get("label")]
        check("a source-pane row offers Store 1 and Store all",
              "Store 1" in labels and "Store all" in labels,
              f"menu labels: {labels!r}")
        check("a source-pane row offers no Retrieve — direction comes from "
              "WHICH pane was clicked",
              not any((l or "").startswith("Retrieve") for l in labels),
              f"menu labels: {labels!r}")

        before_panes = {p.get("paneKey"): (p.get("subtitle"), p.get("rowCount"))
                        for p in panes}
        entry = find_widget(port, "Store all")
        if check("the Store all entry is clickable", bool(entry)):
            click_widget_center(port, entry)
            time.sleep(0.8)
            d2 = stack_dump(port)
            lv2 = (d2.get("levels") or [{}])[0]
            after_panes = {p.get("paneKey"): (p.get("subtitle"), p.get("rowCount"))
                           for p in (lv2.get("panes") or [])}
            check("committing refreshes BOTH panes in the same gesture — "
                  "each header's own stored weight moved",
                  after_panes.get("source") != before_panes.get("source")
                  and after_panes.get("destination")
                      != before_panes.get("destination"),
                  f"before={before_panes!r} after={after_panes!r}")
            check("the session stays open and repeatable after a commit",
                  d2.get("depth") == 1 and lv2.get("kind") == "escort"
                  and send(port,
                           "local s = require('scripts.transfer_session').get();"
                           " return s and s.phase or 'none'").strip('"')
                      == "open",
                  f"got {d2!r}")
            check("the rations really left the acolyte's own pane",
                  not any((w.get("defName") or "") == "rations"
                          for w in item_rows(port, pane_list_id("source"))),
                  "the source pane still lists a rations row")
        else:
            close_menu(port)

    # -- a real resize, rendered (#1250 review round 1).
    #
    #    Two 440-wide panels need 904 px at 1x, so on a framebuffer
    #    narrower than that the PAIR has to be fitted to width — each
    #    panel clamped on its own would land them on top of each other.
    #    What is asserted here is the invariant that holds at EVERY
    #    width: the pair's combined width fits the framebuffer it is
    #    drawn on, and the two still flank rather than stack.
    #
    #    Deliberately NOT asserted as "narrower than 440": `--offscreen`
    #    renders to a fixed-size target, so `engine.setWindowSize` moves
    #    the window without necessarily moving the FRAMEBUFFER this
    #    geometry is measured against, and demanding a shrink would then
    #    be demanding one that was never required. The envelope's formal
    #    minimum (800x600 @ 1x, where the fit genuinely fires and each
    #    pane really does come out under 440) is pinned deterministically
    #    by hspec instead — Test.Headless.UI.TransferSession drives the
    #    real framebuffer ref down to it.
    send(port, "return engine.setWindowSize(800, 600)")
    time.sleep(1.5)
    tight = stack_dump(port)
    tlv = (tight.get("levels") or [{}])[0]
    tpanes = tlv.get("panes") or []
    if check("the session survives a real resize",
             tight.get("depth") == 1 and tlv.get("kind") == "escort"
             and len(tpanes) == 2, f"got {tight!r}"):
        tvp = viewport(port, fallback=(800, 600))
        a, b = tpanes[0], tpanes[1]

        def fits(p):
            return (p["x"] >= 0 and p["y"] >= 0
                    and p["x"] + p["width"] <= tvp["fb_w"]
                    and p["y"] + p["height"] <= tvp["fb_h"])

        overlap = not (a["x"] + a["width"] <= b["x"]
                       or b["x"] + b["width"] <= a["x"]
                       or a["y"] + a["height"] <= b["y"]
                       or b["y"] + b["height"] <= a["y"])
        check("after the resize both panels still fit the framebuffer and "
              "still flank, source left",
              fits(a) and fits(b) and not overlap and a["x"] < b["x"]
              and a.get("paneKey") == "source",
              f"framebuffer {tvp['fb_w']}x{tvp['fb_h']}, panes {a!r} {b!r}")
        check("the PAIR is sized to the framebuffer it is drawn on, not "
              "each panel independently",
              a["width"] + b["width"] <= tvp["fb_w"],
              f"framebuffer width {tvp['fb_w']}, "
              f"panes {a['width']} + {b['width']}")

    dst_rows = item_rows(port, pane_list_id("destination"))
    target = dst_rows[0] if dst_rows else None
    if check("a destination-pane row is located for the reverse direction",
             bool(target)):
        right_click_widget_center(port, target)
        time.sleep(0.4)
        labels = [w.get("label") for w in widgets(port) if w.get("label")]
        check("a destination-pane row offers Retrieve, never Store",
              any((l or "").startswith("Retrieve") for l in labels)
              and not any((l or "").startswith("Store") for l in labels),
              f"menu labels: {labels!r}")
        close_menu(port)

    # -- coupled close: one dismissal takes both panels AND the session.
    send(port, "require('scripts.cargo_inventory_panel').popLevel(); return 'ok'")
    time.sleep(0.5)
    closed = stack_dump(port)
    check("closing the window closes BOTH panels and ends the session "
          "(requirement 7)",
          closed.get("depth") == 0
          and send(port, "return tostring(require('scripts.transfer_session')"
                         ".get())").strip('"') == "nil",
          f"got {closed!r}")

    # -- #1254 (UIT-5B): an ABNORMAL close leaves a usable window stack.
    #
    # The one thing this needs a GPU for. Every failure TRIGGER, and the
    # release of every hold, is pinned deterministically by hspec
    # (Test.Headless.UI.TransferSession, `session failure handling`);
    # what no fixture that renders nothing can state is that the real
    # panels' ELEMENTS went with the session, and that the next window to
    # open on the same page draws and hit-tests normally rather than over
    # two orphaned panels.
    #
    # A FRESH escort, spawned on a surface-checked tile just OUTSIDE the
    # cargo's footprint — adjacent, so the contract's Chebyshev-1
    # rect-to-rect rule already reports it in reach and the session opens
    # on the first AI tick with no walk at all — and destroyed again at
    # the end.
    #
    # Both halves of that are deliberate. Fresh, because this probe's
    # world reliably contains bleeding acolytes (see
    # `retire_medic_drive`) and one that has been standing in it for a
    # few minutes can be at a third of its blood; since #1254 an
    # UNCONSCIOUS held unit correctly ends its own session, so a second
    # session on a worn-down unit would measure the world's attrition
    # instead of the teardown. And OUTSIDE the footprint, because
    # spawning on the anchor puts a unit inside the building, where the
    # drop to the ground can wound it — the same hazard, arriving faster.
    #
    # `unit.destroy` rather than `unit.kill`: a death is a world EVENT (a
    # corpse, witnesses, the mood and combat reactions they provoke) and
    # the very next scenario measures AI priority on a pair nearby, so a
    # kill here would make that measurement depend on this one. WHICH
    # detector fires is hspec's business — `session failure handling`
    # covers death, unconsciousness, faction loss and demolition
    # separately; what needs a GPU is only that the panels really left the
    # screen and that the stack still works.
    ab_uid, ab_raw = None, ""
    for dx, dy in ((-1, 0), (2, 0), (0, -1), (0, 2), (-1, -1), (2, 2)):
        tx, ty = bax + dx, bay + dy
        surface = tile_surface(port, tx, ty)
        if surface is None or not surface[1]:
            continue
        ab_raw = send(port, f"return unit.spawn('acolyte', {tx}, {ty},"
                            " nil, 'player')")
        if ab_raw.strip() not in ("", "nil", "null"):
            ab_uid = int(float(ab_raw))
            break
    if not check("a fresh acolyte is spawned beside the cargo for the "
                 "abnormal-close phase",
                 ab_uid is not None, f"got {ab_raw!r}"):
        return
    check("its standing find_water goal is retired", clear_find_water(port, ab_uid))
    check("it is out of the medic squad, so nothing outranks the hold",
          retire_medic_drive(port, ab_uid))
    reopened = send(port, "return tostring(require('scripts.transfer_session')"
                          f".create({ab_uid}, 'building', {bid}) ~= nil)")
    if check("a second Mode A session opens on the same endpoint",
             reopened.strip().strip('"') == "true", f"got {reopened!r}"):
        again = poll_until(45.0, lambda: send(
            port, "local s = require('scripts.transfer_session').get();"
                  " return s and s.phase or 'none'").strip('"') == "open",
            interval=0.5)
        if check("it reaches the open state with both panes drawn",
                 again and (stack_dump(port).get("levels")
                            or [{}])[0].get("paneCount") == 2,
                 "session "
                 + send(port,
                        "local s = require('scripts.transfer_session').get();"
                        " return (s and s.phase or 'gone') .. ' pose='"
                        f" .. tostring(unit.getPose({ab_uid}))")):
            send(port, f"return unit.destroy({ab_uid})")
            time.sleep(1.5)
            abnormal = stack_dump(port)
            alive = send(port, f"return tostring(unit.exists({ab_uid}))")
            check("losing the held escort ends the session and takes both "
                  "panels with it (#1254 requirements 1 and 5)",
                  alive.strip().strip('"') == "false"
                  and abnormal.get("depth") == 0
                  and send(port, "return tostring(require('scripts"
                                 ".transfer_session').get())").strip('"')
                      == "nil",
                  f"exists {alive!r}, stack {abnormal!r}")
            # Nothing orphaned: the panels' own widgets are gone from the
            # live UI, not merely forgotten by the manager.
            panes_left = [w for w in widgets(port)
                          if (w.get("name") or "").startswith("cargo_inv")]
            check("no escort panel widget is left on screen",
                  not panes_left, f"still present: {panes_left!r}")
            # ...and the stack accepts the next open, rendering rows.
            if check("the next container window opens normally after the "
                     "abnormal close", open_window_on(port, bid)):
                rows = item_rows(port, level_list_id(1))
                nxt = stack_dump(port)
                check("that window is an ordinary endpoint level with real "
                      "rows",
                      nxt.get("depth") == 1
                      and (nxt.get("levels") or [{}])[0].get("kind")
                          == "endpoint"
                      and len(rows) > 0,
                      f"stack {nxt!r}, {len(rows)} row(s)")
            send(port, "require('scripts.cargo_inventory_panel')"
                       ".closeIfOpen(); return 'ok'")
    send(port, f"return unit.destroy({ab_uid})")


# --------------------------------------------------------------------------
# #1251: the unit-to-unit escort's two-sided hold
# --------------------------------------------------------------------------

def unit_pos(port: int, uid: int):
    """`(gridX, gridY)` for `uid`, or None if it stopped resolving."""
    info = send_json(port, f"return unit.getInfo({uid})")
    if not isinstance(info, dict):
        return None
    x, y = info.get("gridX"), info.get("gridY")
    if not isinstance(x, (int, float)) or not isinstance(y, (int, float)):
        return None
    return (float(x), float(y))


def tile_gap(a, b) -> float:
    """Euclidean tile distance, or -1 when either end is unknown — which
    never satisfies a `> n` motion test, so an unresolvable unit reads as
    "did not move" rather than as movement."""
    if a is None or b is None:
        return -1.0
    return math.hypot(b[0] - a[0], b[1] - a[1])


def ai_action(port: int, uid: int) -> str:
    """`aiState[uid].currentAction` — which action last WON for this
    unit, which is how a preemption is observed rather than inferred."""
    return send(port, "local s = require('scripts.unit_ai').getState("
                      f"{uid}); return s and s.currentAction or 'nil'"
                ).strip('"')


def retire_medic_drive(port: int, uid: int) -> bool:
    """Take `uid` out of the medic squad for the duration of this
    measurement, and answer whether it took.

    `treat_ally` scores 8.0 — deliberately ABOVE the 7.5 escort hold,
    because #1250 records treatment as one of the bands that still
    preempts it. Its patient scan reaches 60 tiles, so ONE ally bleeding
    anywhere in this probe's world makes an acolyte's own AI walk away
    from the hold, correctly, and the scenario below would then be
    measuring that documented exception instead of the hold. (This is
    not hypothetical: the first run of this gate failed with the target
    sitting in `treat_ally`.)

    So the competing drive is retired first, exactly as the escort
    scenario retires `find_water` for the same reason. `medicCapability`
    gates on the `bleed_control` knowledge, and an already-locked claim
    outranks even that, so both go — in ONE console statement, which
    runs on the Lua thread and therefore cannot be interleaved with a
    tick that re-claims. What is left competing with the hold is the
    routine-work band it is supposed to beat."""
    send(port, f"unit.setKnowledge({uid}, 'bleed_control', 0);"
               f" local s = require('scripts.unit_ai').getState({uid});"
               " if s then s.treatClaim = nil; s.treatPending = nil end;"
               " return 'ok'")
    try:
        return float(send(port,
                          f"return unit.getKnowledge({uid}, 'bleed_control')"
                          ).strip().strip('"')) == 0.0
    except (TypeError, ValueError):
        return False


def chebyshev(a, b) -> float:
    """The contract's OWN distance measure between two 1x1 endpoints —
    `withinReach` is Chebyshev <= 1 — or -1 when either end is unknown.
    Used here instead of a Euclidean gap because "the escort still has
    somewhere to walk" is exactly "the pair is NOT yet in reach", and
    only this measure answers that question."""
    if a is None or b is None:
        return -1.0
    return max(abs(b[0] - a[0]), abs(b[1] - a[1]))


class EscortSeparation:
    """Everything the escort staging loop below observed, retained per
    ATTEMPT rather than only for the last one (#1911).

    A single pair of variables used to hold the destination and the
    paused snapshot, overwritten on every retry — so a run that
    exhausted all four attempts reported one bearing and one gap and
    lost the three tries before it. That is exactly the evidence needed
    to tell "this world never had room" from "one bearing kept pointing
    back at the escort", and it was unavailable without rerunning a
    fifteen-minute GPU probe. The retained coordinated run of
    2026-08-26 is the case in point: it recorded a maximum-axis gap of
    exactly `1.0` and nothing about the three attempts that preceded it.

    Recorded per attempt: where the target was actually sent, both
    units' positions at the paused instant whenever a snapshot was taken
    at all, and the Chebyshev gap those imply — the contract's OWN
    measure, so `separated` answers the same question `withinReach`
    does."""

    def __init__(self) -> None:
        self.attempts: list[dict] = []

    def record(self, sent_to, src, dst) -> None:
        """One attempt. `sent_to` is None when the target's AI never took
        a move order up, in which case no snapshot was taken either."""
        self.attempts.append({"sent_to": sent_to, "src": src, "dst": dst,
                              "gap": chebyshev(src, dst)})

    @property
    def last(self) -> dict:
        return self.attempts[-1] if self.attempts else {}

    @property
    def ordered(self) -> bool:
        """Did the target end up under a real player move order?"""
        return self.last.get("sent_to") is not None

    @property
    def separated(self) -> bool:
        """Is the pair OUTSIDE the transfer contract's own reach rule —
        Chebyshev > 1 — at the paused instant a session would be created?"""
        return self.ordered and self.last.get("gap", -1.0) > 1.0

    @property
    def sent_to(self):
        return self.last.get("sent_to")

    @property
    def at_create(self):
        return self.last.get("dst")

    def detail(self) -> str:
        """Every attempt, in order, so a setup failure is attributable
        from the run's own output instead of from a rerun."""
        if not self.attempts:
            return "no attempt was made"
        return "; ".join(
            f"#{n} sent to {a['sent_to']!r}, escort at {a['src']!r}, "
            f"target at {a['dst']!r}, Chebyshev {a['gap']:.3f}"
            for n, a in enumerate(self.attempts, 1))


def stage_escort_separation(port: int, src_uid: int, dst_uid: int,
                            attempts: int = 4,
                            settle: float = 1.5) -> EscortSeparation:
    """Order the target away until the pair is outside the transfer
    contract's reach with the simulation stopped, and answer everything
    observed on the way.

    Both acolytes keep ticking, so a separation observed a moment ago
    can be gone a couple of console round trips later — which is how two
    earlier versions of this check failed (once at 0.84 tiles apart,
    once at 0.58, each having been clear moments before).
    `engine.setPaused` is the only thing that really holds a unit still
    (`unit.setFrozen` is a render pin, CLAUDE.md) and positions must be
    re-read AFTER pausing; the order points away, so a gap that has not
    opened yet opens by waiting rather than by trying something else.
    Nothing new is tried when it does not open: a third staging
    heuristic is exactly what the two documented failures above warn
    against, so an exhausted loop is reported rather than worked around.

    Hands back with the simulation stopped at the instant of its last
    snapshot, whether or not that snapshot separated: the caller either
    creates the session right there, or gives the pause to
    `end_escort_setup` along with everything else it restores. Only the
    world BETWEEN attempts is resumed, and only because the next attempt
    needs somewhere to walk."""
    staging = EscortSeparation()
    for _ in range(attempts):
        if staging.attempts:
            set_paused(port, False)
        sent_to = order_target_away(port, dst_uid, src_uid)
        if sent_to is None:
            staging.record(None, None, None)
            break
        time.sleep(settle)
        set_paused(port, True)
        at_create = unit_pos(port, dst_uid)
        src_at_create = unit_pos(port, src_uid)
        staging.record(sent_to, src_at_create, at_create)
        if staging.separated:
            break
    return staging


def end_escort_setup(port: int) -> None:
    """Leave the engine as a completed run leaves it: running, with no
    session behind it.

    Every early return from the escort scenario goes through this one
    helper. The setup-failure path needs it as much as the later ones
    do — the staging loop pauses in order to measure, so a scenario
    returning from there without it would hand whatever runs next a
    stopped simulation."""
    set_paused(port, False)
    send(port, "require('scripts.transfer_session').clear(); return 'ok'")


def spawn_pair_apart(port: int, ax: int, ay: int):
    """Put the escort a few tiles from its target, and answer
    `(src_uid, dst_uid)`.

    Reachability is established by WALKING, never assumed. The escort is
    spawned on the anchor and sent to a candidate tile; only a tile it
    actually reaches is used, and the TARGET is then spawned on the
    anchor the escort has just left — so the escort's own approach is
    the path it has already walked, in reverse.

    Both weaker versions of this failed on real terrain. Walking the
    TARGET clear put the measurement behind a second pathfinding
    problem: once it could not get two tiles clear in any of twelve
    directions, once it could not leave its spawn tile at all while the
    escort beside it wandered four tiles. Spawning it on a
    surface-checked tile instead removed that, but a tile with a surface
    is not necessarily a tile the escort can REACH, and the pair then
    never opened inside a minute across three tiles. Walking the leg
    that the scenario depends on is the only version that proves it.

    Chunks around the anchor are loaded first: the camera has been
    elsewhere for this whole probe, and pathing across terrain that was
    never generated is its own way to produce a unit that does not
    move."""
    ai = "require('scripts.unit_ai')"
    ccx, ccy = ax // CHUNK_TILES, ay // CHUNK_TILES
    send(port, f"return world.loadChunksInRegion({ccx - 1}, {ccy - 1},"
               f" {ccx + 1}, {ccy + 1})")
    send(port, "return world.waitForChunks(60)", timeout=65.0)
    src_raw = send(port, f"return unit.spawn('acolyte', {ax}, {ay},"
                         " nil, 'player')")
    if src_raw.strip() in ("", "nil", "null"):
        return None, None
    src_uid = int(float(src_raw))
    # Its own standing goals must not fight the scouting walk below, for
    # the same reason they must not fight the measurement afterwards.
    clear_find_water(port, src_uid)
    retire_medic_drive(port, src_uid)
    for dx, dy in ((2, 0), (0, 2), (-2, 0), (0, -2), (2, 2), (-2, -2),
                   (3, 0), (0, 3), (-3, 0), (0, -3)):
        tx, ty = ax + dx, ay + dy
        surface = tile_surface(port, tx, ty)
        if surface is None or not surface[1]:
            continue
        send(port, f"unit.stop({src_uid});"
                   f" {ai}.commandMove({src_uid}, {tx}, {ty}); return 'ok'")
        if poll_until(20.0,
                      lambda: tile_gap(unit_pos(port, src_uid),
                                       (float(tx), float(ty))) < 0.9,
                      interval=0.5):
            dst_raw = send(port, f"return unit.spawn('acolyte', {ax}, {ay},"
                                 " nil, 'player')")
            if dst_raw.strip() in ("", "nil", "null"):
                return src_uid, None
            # Leave the escort idle where it stands, so nothing it was
            # told to do earlier is still in flight when the session is
            # created.
            send(port, f"unit.stop({src_uid}); return 'ok'")
            return src_uid, int(float(dst_raw))
    return src_uid, None


def order_target_away(port: int, dst_uid: int, src_uid: int):
    """Put the target under a real player move order pointing AWAY from
    the escort; answer where it was sent, or None if the AI never took
    one up.

    A commanded move rather than the ambient wander: `follow_command`
    sits at 7.0, above every routine-work lock, so the escort hold
    preempting it is strictly harder than preempting the wander tick,
    and its destination is chosen here rather than rolled, so "it never
    got there" is a fact the caller can check.

    AWAY is load-bearing. An earlier version aimed at fixed offsets from
    the anchor, and since the escort had walked out to a tile of its
    own, the first of those pointed straight at it: the target dutifully
    closed the gap to 0.58 tiles and the pair was in reach before the
    session was ever created. Perpendicular fallbacks are offered next
    because they at least do not close it.

    Taking the order UP is what this waits for — `follow_command`
    becoming the unit's current action, which is the AI selecting it and
    issuing the walk. How far that walk then gets is generated terrain's
    business, and it is not what the hold is measured against: the
    caller checks separately that the target does not move once held,
    and that it never reached where it was sent."""
    ai = "require('scripts.unit_ai')"
    here = unit_pos(port, dst_uid)
    src = unit_pos(port, src_uid)
    if here is None or src is None:
        return None
    vx, vy = here[0] - src[0], here[1] - src[1]
    norm = math.hypot(vx, vy)
    if norm < 0.01:
        vx, vy, norm = 1.0, 0.0, 1.0
    ux, uy = vx / norm, vy / norm
    bearings = ((ux, uy), (-uy, ux), (uy, -ux))
    for reach in (6, 4, 3):
        for bx, by in bearings:
            tx = int(round(here[0] + bx * reach))
            ty = int(round(here[1] + by * reach))
            surface = tile_surface(port, tx, ty)
            if surface is None or not surface[1]:
                continue
            send(port, f"unit.stop({dst_uid});"
                       f" {ai}.commandMove({dst_uid}, {tx}, {ty});"
                       " return 'ok'")
            if poll_until(8.0,
                          lambda: ai_action(port, dst_uid) == "follow_command",
                          interval=0.4):
                return (float(tx), float(ty))
    return None


def accepts_movement(port: int, uid: int, ax: int, ay: int,
                     window: float = 8.0) -> bool:
    """Does `uid` take an ordinary move order again? Answered by
    watching the real AI either SELECT the order or act on it.

    Selecting counts, and that is the point rather than a concession.
    The hold is a utility, so "released" means neither side of it wins
    any more and the ordinary ladder decides again — and `follow_command`
    becoming this unit's current action IS the ladder taking the
    player's order. Whether the unit then covers ground is generated
    terrain's business: a unit that cannot path anywhere from a ledge
    does not move under ANY order, which looks identical to one still
    pinned and is not. Displacement is still accepted first, because
    when it happens it says the same thing more loudly.

    `unit.stop` FIRST. A unit released from the hold still carries the
    move order the hold preempted, and `follow_command` resumes it — so
    the action does not SWITCH when a new order arrives, and unit_ai
    re-executes a running action only on a switch or when the unit is
    idle. The new destination would otherwise sit unapplied behind a
    walk to the old one — the same reason `transfer_session.close` stops
    rather than merely releasing.

    Several destinations, for the reason `order_target_away` offers
    several: one reachable tile is the whole answer."""
    ai = "require('scripts.unit_ai')"
    for dx, dy in ((0, 0), (2, 0), (0, 2), (-2, 0), (0, -2),
                   (4, 0), (0, 4), (-4, 0), (0, -4)):
        tx, ty = ax + dx, ay + dy
        surface = tile_surface(port, tx, ty)
        if surface is None or not surface[1]:
            continue
        before = unit_pos(port, uid)
        if before is None:
            return False
        send(port, f"unit.stop({uid}); {ai}.commandMove({uid}, {tx}, {ty});"
                   " return 'ok'")
        if poll_until(window,
                      lambda: tile_gap(before, unit_pos(port, uid)) > 0.5
                              or ai_action(port, uid) == "follow_command",
                      interval=0.4):
            return True
    return False


def unit_escort_session_scenario(port: int, aax: int, aay: int) -> None:
    """#1251 (UIT-4): a session whose DESTINATION is a unit holds both
    ends, and both are released together.

    The behavioural half needs a real engine, which is why it lives in a
    probe rather than beside the deterministic hspec cases: the target
    has to be in observable motion BEFORE the session exists, since an
    initially idle unit standing still afterwards proves nothing. So it
    is put under a real move order first, and what is checked is that
    the hold takes that order over on its next tick, that the target's
    position then does not change for the WHOLE of the source's
    approach, and that both units can be steered again once the window
    closes.

    The rendered half is the pair itself: two live panes over two unit
    endpoints, a real row menu committing in each direction, and one
    dismissal closing both.

    Deliberately after the building escort, whose stack this takes over
    and leaves empty again.

    This function is the SETUP half only, and each of its three `setup:`
    preconditions is terminal: a fixture it cannot establish ends the
    scenario here (#1911). `escort_hold_measurement` — everything that
    grades the hold, the approach and the rendered pair — is reached
    only once all three hold, so no check downstream of them can be
    graded against a pair the probe has already reported invalid."""
    print("== #1251 unit-to-unit escort (the two-sided hold) ==")
    send(port, "require('scripts.cargo_inventory_panel').closeIfOpen();"
               " return 'ok'")

    src_uid, dst_uid = spawn_pair_apart(port, aax, aay)
    if not check("setup: the escort stands a few tiles from its target, over "
                 "ground it has WALKED — so its approach below is a leg this "
                 "world is known to admit",
                 src_uid is not None and dst_uid is not None,
                 f"escort={src_uid!r} target={dst_uid!r}"):
        end_escort_setup(port)
        return
    for uid in (src_uid, dst_uid):
        check(f"unit {uid}'s standing find_water goal is retired",
              clear_find_water(port, uid))
        # After clear_find_water, which is what waits for the AI state to
        # exist at all.
        check(f"unit {uid} is out of the medic squad for this measurement "
              "— treat_ally (8.0) is a band that legitimately preempts "
              "the 7.5 hold, so it must not be what decides",
              retire_medic_drive(port, uid))
    # One stack per side, so each pane has a row and each direction has
    # something of its own to move.
    send(port, f"for i = 1, 3 do unit.addItem({src_uid}, 'rations') end;"
               f" for i = 1, 2 do unit.addItem({dst_uid}, 'bandage') end;"
               " return 'ok'", timeout=20.0)

    # Order the target away, let it open the gap, and COMMIT to the
    # measurement with the simulation stopped.
    staging = stage_escort_separation(port, src_uid, dst_uid)
    if not check("setup: the target is under a real player move order "
                 "before the session exists — its own AI has SELECTED "
                 "follow_command (7.0, above every routine-work lock), so "
                 "an idle unit standing still afterwards is not what gets "
                 "measured",
                 staging.ordered,
                 f"running {ai_action(port, dst_uid)!r} at "
                 f"{unit_pos(port, dst_uid)!r}; {staging.detail()}"):
        end_escort_setup(port)
        return

    # WHICH action is pending matters as much as that one is: the claim
    # below is that the hold preempts a player move order, so the thing
    # preempted has to be named, not assumed from the fact that something
    # moved. (The first run of this gate found the target walking under
    # `treat_ally` instead — see `retire_medic_drive`.) Read off the AI
    # state, so pausing does not disturb it.
    walking_under = ai_action(port, dst_uid)
    check("the pending action really is the move order, not some other "
          "action that happens to walk",
          walking_under == "follow_command", f"got {walking_under!r}")

    # TERMINAL, because every check past it is a claim about an approach
    # (#1911). A pair already within Chebyshev 1 makes "the pair opens"
    # pass with no approach at all and "the target did not move for the
    # whole of the approach" measure a walk that never happened — so a
    # staging loop that ran out of attempts fails the scenario at SETUP
    # rather than grading five checks against a fixture the probe itself
    # has just reported invalid.
    if not check("setup: the escort has a real approach to make — the pair "
                 "is outside the contract's own reach rule at the moment "
                 "the session is created, so 'held still through the "
                 "approach' is a claim about an approach that happened",
                 staging.separated, staging.detail()):
        end_escort_setup(port)
        return
    escort_hold_measurement(port, src_uid, dst_uid, staging, aax, aay)


def escort_hold_measurement(port: int, src_uid: int, dst_uid: int,
                            staging: EscortSeparation,
                            aax: int, aay: int) -> None:
    """The half that only means anything once the pair really is staged
    apart, in its own function so that it CANNOT run against a fixture
    the gate above rejected (#1911 requirement 4).

    Enters with the simulation stopped at the instant `staging`
    measured, and hands back with it running."""
    sent_to, at_create = staging.sent_to, staging.at_create
    # Created while STOPPED, so nothing can drift between the gate above
    # and the session it is a precondition for. The target's move order
    # is still pending and un-preempted at this instant; the very next
    # tick after the sim resumes is what has to take it over.
    made = send(port, "return tostring(require('scripts.transfer_session')"
                      f".create({src_uid}, 'unit', {dst_uid}) ~= nil)")
    set_paused(port, False)
    if not check("a unit-to-unit Mode A session is created",
                 made.strip().strip('"') == "true", f"got {made!r}"):
        end_escort_setup(port)
        return
    # `escort_hold`, not `escort_transfer`: the TARGET side is its own
    # registered action so that every commandable species has it, while
    # the source side stays the per-species capability the source gate
    # asks about (#1251 review round 1).
    took = poll_until(20.0,
                      lambda: ai_action(port, dst_uid) == "escort_hold",
                      interval=0.3)
    if not check("session creation PREEMPTS the target's move order — the "
                 "hold wins on its very next tick, mid-walk", bool(took),
                 # A hold that never wins and a session that quietly
                 # ENDED look identical from currentAction alone: with no
                 # session, `roleOf` answers nil, the hold scores -inf
                 # and the target goes straight back to its move order.
                 # The escort retires a session whose destination stops
                 # resolving, so the phase and both roles are what tell
                 # those apart.
                 f"currentAction={ai_action(port, dst_uid)!r}, session "
                 + send(port,
                        "local s = require('scripts.transfer_session');"
                        " local a = s.get();"
                        " return (a and a.phase or 'gone') .. ' src='"
                        f" .. tostring(s.roleOf({src_uid})) .. ' dst='"
                        f" .. tostring(s.roleOf({dst_uid}))")):
        end_escort_setup(port)
        return
    held = unit_pos(port, dst_uid)
    # Between the create call and the tick that acts on it the target is
    # still walking, so a tile of drift is the hold arriving on the next
    # tick rather than the hold failing. What must NOT have happened is
    # it carrying on to where it was sent.
    check("the target stopped where it stood, short of where it was sent",
          tile_gap(at_create, held) < 2.0 and tile_gap(held, sent_to) > 1.0,
          f"created at {at_create!r}, held at {held!r}, sent to {sent_to!r}")

    opened = poll_until(60.0, lambda: send(
        port, "local s = require('scripts.transfer_session').get();"
              " return s and s.phase or 'none'").strip('"') == "open",
        interval=0.5)
    if not check("the source walks over to the held target and the pair "
                 "opens", bool(opened)):
        end_escort_setup(port)
        return
    settled = unit_pos(port, dst_uid)
    check("the target's position did not move for the WHOLE of the "
          "source's approach — the walk had a fixed destination",
          tile_gap(held, settled) < 0.01,
          f"held at {held!r}, at arrival {settled!r}")
    roles = send_json(port, "local s = require('scripts.transfer_session');"
                            f" return {{ src = s.roleOf({src_uid}),"
                            f" dst = s.roleOf({dst_uid}) }}")
    check("both units are held, on their two sides of the one session",
          isinstance(roles, dict) and roles.get("src") == "source"
          and roles.get("dst") == "target", f"got {roles!r}")
    time.sleep(0.6)

    d = stack_dump(port)
    lv = (d.get("levels") or [{}])[0]
    check("the unit-to-unit pair is ONE escort level with two panes",
          d.get("depth") == 1 and lv.get("kind") == "escort"
          and lv.get("paneCount") == 2, f"got {d!r}")
    src_rows = item_rows(port, pane_list_id("source"))
    dst_rows = item_rows(port, pane_list_id("destination"))
    check("both panes render their own UNIT endpoint's rows",
          any((w.get("defName") or "") == "rations" for w in src_rows)
          and any((w.get("defName") or "") == "bandage" for w in dst_rows),
          f"source={[w.get('defName') for w in src_rows]!r} "
          f"destination={[w.get('defName') for w in dst_rows]!r}")

    def row_named(rows, def_name):
        for w in rows:
            if (w.get("defName") or "") == def_name:
                return w
        return None

    def carries(uid: int, def_name: str) -> bool:
        inv = send_json(port, f"return unit.getInventory({uid})")
        return isinstance(inv, list) and any(
            isinstance(it, dict) and it.get("defName") == def_name
            for it in inv)

    # -- both directions, through the real rendered row menus. Which pane
    #    was clicked is what picks the direction, so this is the same
    #    builder answering twice rather than two gestures.
    ration = row_named(src_rows, "rations")
    if check("the escort's own rations row is located on the source pane",
             bool(ration)):
        right_click_widget_center(port, ration)
        time.sleep(0.4)
        entry = find_widget(port, "Store all")
        if check("a source-pane row offers Store all against a UNIT "
                 "destination", bool(entry)):
            click_widget_center(port, entry)
            time.sleep(0.8)
            check("Store committed into the held TARGET unit",
                  carries(dst_uid, "rations") and not carries(src_uid, "rations"),
                  "the rations did not move between the two units")
        else:
            close_menu(port)

    bandage = row_named(item_rows(port, pane_list_id("destination")), "bandage")
    if check("the target's own bandage row is located on the destination "
             "pane", bool(bandage)):
        right_click_widget_center(port, bandage)
        time.sleep(0.4)
        entry = find_widget(port, "Retrieve all")
        if check("a destination-pane row offers Retrieve all off a UNIT "
                 "endpoint", bool(entry)):
            click_widget_center(port, entry)
            time.sleep(0.8)
            check("Retrieve committed back into the escort",
                  carries(src_uid, "bandage") and not carries(dst_uid, "bandage"),
                  "the bandages did not move back")
        else:
            close_menu(port)

    # -- coupled close: one dismissal takes both panels, the session AND
    #    both holds (requirement 2).
    send(port, "require('scripts.cargo_inventory_panel').popLevel();"
               " return 'ok'")
    time.sleep(0.5)
    closed = stack_dump(port)
    released = send_json(port, "local s = require('scripts.transfer_session');"
                               " return { session = tostring(s.get()),"
                               f" src = s.holdsUnit({src_uid}),"
                               f" dst = s.holdsUnit({dst_uid}) }}")
    check("one dismissal closes both panels, ends the session and releases "
          "BOTH units",
          closed.get("depth") == 0 and isinstance(released, dict)
          and released.get("session") == "nil"
          and released.get("src") is False and released.get("dst") is False,
          f"stack={closed!r} session={released!r}")
    # Release means the real AI is deciding again — a cleared Lua table
    # would not prove that, since the hold is a utility rather than a
    # flag. Two observations per unit: the hold has stopped winning, and
    # an ordinary move order is taken up.
    for role, uid in (("escort", src_uid), ("target", dst_uid)):
        loose = poll_until(
            10.0,
            lambda: ai_action(port, uid) not in ("escort_transfer",
                                                 "escort_hold"),
            interval=0.4)
        check(f"the released {role} is no longer being decided by the hold "
              "— the ordinary action ladder has taken it back",
              bool(loose), f"still running {ai_action(port, uid)!r}")
        check(f"the released {role} accepts ordinary AI movement again",
              accepts_movement(port, uid, aax, aay),
              f"at {unit_pos(port, uid)!r}, running "
              f"{ai_action(port, uid)!r}, under none of the move orders "
              "offered")


# --------------------------------------------------------------------------
# #1238: the nesting stack
# --------------------------------------------------------------------------

def stack_dump(port: int) -> dict:
    got = send_json(port, "return require('scripts.cargo_inventory_panel').dump()")
    return got if isinstance(got, dict) else {}


def level_list_id(index: int) -> str:
    """A debug-console expression naming level `index`'s widget instance."""
    return (f"(require('scripts.cargo_inventory_panel').getLevel({index})"
            " or {}).listId")


def scroll_to_row(port: int, list_id_lua: str, def_name: str):
    """Scroll a level's list until `def_name` is among the RENDERED rows,
    and answer that row.

    Which rows a level shows first is the grouped order -- a hashmap
    enumeration on the engine side -- so a probe must never assume a
    given row is on screen. Scrolls through the widget's own offset one
    page at a time and gives up at the end of the list.

    Deliberately looks at the CURRENT view first and only then walks from
    the top: a level the probe has already scrolled on purpose must not
    have that offset thrown away just because something else wanted to
    find a row on it."""
    il = "require('scripts.ui.item_list')"
    row = next((r for r in item_rows(port, list_id_lua)
                if r.get("defName") == def_name), None)
    if row:
        return row
    max_off = int(float(send(port, f"return {il}.maxScrollOffset({list_id_lua})")))
    step = max(1, int(float(send(port, f"return {il}.rowCapacity({list_id_lua})"))))
    offset = 0
    while True:
        send(port, f"return {il}.setScrollOffset({list_id_lua}, {offset})")
        time.sleep(0.3)
        row = next((r for r in item_rows(port, list_id_lua)
                    if r.get("defName") == def_name), None)
        if row or offset >= max_off:
            return row
        offset = min(max_off, offset + step)


def open_contents_on(port: int, list_id_lua: str, def_name: str) -> bool:
    """Push a nested level the way a PLAYER does: locate the row through
    `ui.dumpWidgets()`, right-click it, then locate and click the
    "Contents" entry of the real context menu. No coordinate is ever
    hardcoded, and no window API is called."""
    row = scroll_to_row(port, list_id_lua, def_name)
    if not row:
        return False
    right_click_widget_center(port, row)
    time.sleep(0.4)
    entry = find_widget(port, "Contents")
    if not entry:
        close_menu(port)
        return False
    click_widget_center(port, entry)
    time.sleep(0.6)
    return True


def nesting_stack_scenario(port: int, bid: int, kit_iid: int,
                           mule_uid: int, vp: dict) -> None:
    """#1238: one window manager, an ordered stack of levels, only the
    deepest interactive.

    Every push here goes through the real gesture — a right-click on a
    real rendered row, then a real click on the real context-menu entry
    — so what is verified is the route a player has, not an API the
    window happens to expose."""
    print("== nesting stack (#1238) ==")
    send(port, "require('scripts.cargo_inventory_panel').closeIfOpen();"
               " return 'ok'")
    time.sleep(0.3)
    if not check("container window opens on the stocked cargo",
                 open_window_on(port, bid)):
        return

    base = stack_dump(port)
    base_levels = base.get("levels") or []
    if not check("the open window is a stack of exactly one level",
                 base.get("depth") == 1 and len(base_levels) == 1,
                 f"got {base!r}"):
        return
    check("the base level is NOT modal and the game is not input-blocked",
          base_levels[0].get("modal") is False
          and base.get("inputBlocked") is False,
          f"got modal={base_levels[0].get('modal')!r} "
          f"blocked={base.get('inputBlocked')!r}")

    # -- Push a level through the real row gesture.
    if not check("a container row in the cargo opens a nested level",
                 open_contents_on(port, CARGO_LIST_ID, DEF_DEEP_KIT)):
        return
    close_menu(port)
    d2 = stack_dump(port)
    lv = d2.get("levels") or []
    if not check("the stack is now two levels deep",
                 d2.get("depth") == 2 and len(lv) == 2, f"got {d2!r}"):
        return
    check("the nested level is a building-side REMEMBERED level, "
          "addressed by the exact instance the player clicked",
          lv[1].get("kind") == "buildingItem"
          and lv[1].get("bid") == bid
          and lv[1].get("path") == [kit_iid],
          f"got kind={lv[1].get('kind')!r} bid={lv[1].get('bid')!r} "
          f"path={lv[1].get('path')!r} (want [{kit_iid}])")
    check("the nested level carries the PARENT's own observation age",
          lv[1].get("ageText") == lv[0].get("ageText")
          and bool(lv[1].get("ageText")),
          f"got {lv[1].get('ageText')!r} vs parent {lv[0].get('ageText')!r}")

    # The rows are the engine's own remembered answer, not a restatement.
    remembered = send_json(
        port, f"return building.getRememberedItemContents({bid}, {{{kit_iid}}})")
    want = [r.get("defName") for r in (remembered or {}).get("items", [])
            if isinstance(r, dict)]
    got_rows = item_rows(port, level_list_id(2))
    got = [r.get("defName") for r in got_rows]
    # The level shows its row cap's worth of the engine's own list, IN
    # THAT ORDER -- the widget never re-sorts a pre-grouped answer.
    check("the nested level renders the engine's REMEMBERED contents, in "
          "the engine's own order",
          bool(want) and bool(got) and got == want[:len(got)],
          f"got {got!r} want a prefix of {want!r}")

    # -- The parent stays PAINTED but INERT.
    parent_rows = item_rows(port, CARGO_LIST_ID)
    check("the parent level stays painted behind the nested one",
          bool(parent_rows), "parent rows vanished")
    check("the parent level is out of input scope and the game is blocked",
          lv[0].get("pageInScope") is False
          and lv[1].get("pageInScope") is True
          and d2.get("inputBlocked") is True,
          f"got base={lv[0].get('pageInScope')!r} "
          f"deep={lv[1].get('pageInScope')!r} "
          f"blocked={d2.get('inputBlocked')!r}")
    # Whichever row the parent is actually RENDERING (it is scrolled and
    # capped, so naming a def would be naming a row that may not be on
    # screen); what matters is that it is one of the parent's own.
    inert_target = parent_rows[0] if parent_rows else None
    if check("a parent row is still located for the inert-click check",
             bool(inert_target)):
        right_click_widget_center(port, inert_target)
        time.sleep(0.4)
        check("a right-click on a shallower level's row opens nothing",
              not [w for w in widgets(port)
                   if (w.get("label") or "").startswith(("Withdraw",
                                                          "Retrieve"))],
              "a transfer menu appeared behind the modal boundary")
        close_menu(port)

    # -- A third level, and same-level REPLACEMENT.
    if check("a container row INSIDE the nested level opens a third",
             open_contents_on(port, level_list_id(2), "first_aid_kit")):
        lv3 = stack_dump(port).get("levels") or []
        check("the third level's path EXTENDS the second's by exactly one "
              "instance id -- the nesting path, not a fresh address",
              len(lv3) == 3
              and (lv3[2].get("path") or [])[:1] == [kit_iid]
              and len(lv3[2].get("path") or []) == 2,
              f"got {[l.get('path') for l in lv3]!r}")

    # -- An EXTERNAL request starts over at the base, discarding every
    #    deeper level. This is the replacement route a player actually
    #    has: a shallower level's own rows are behind the modal boundary
    #    (proved inert above), so the level-N-replaces-level-N case is
    #    exercised through the manager's own API by the CI-blocking
    #    `--match "container window stack"` group instead.
    before = stack_dump(port).get("depth")
    check("the external request runs against a MULTI-level stack",
          before == 3, f"got depth {before!r}")
    open_window_on(port, bid)
    after = stack_dump(port)
    check("an external request targets the BASE level and discards every "
          "deeper level",
          after.get("depth") == 1
          and (after.get("levels") or [{}])[0].get("kind") == "endpoint",
          f"got {after.get('depth')!r} / "
          f"{[l.get('kind') for l in (after.get('levels') or [])]!r}")

    # -- Escape closes one level per press, deepest first.
    #
    #    Only the DEPTH is measured through the real key. A gameplay
    #    Escape ALSO reaches uiManager.onUIEscape (a separate broadcast
    #    from init_keys' dismiss cascade -- Engine.Input.Thread.Keyboard
    #    queues LuaUIEscape for every Escape in the ordinary game-input
    #    path), and in `world_view` that TOGGLES the pause menu. That is
    #    pre-existing behaviour shared by every panel in the cascade and
    #    is not this window's; but the pause menu is a LayerModal page,
    #    so while it is up it owns the input boundary and any scope
    #    reading would be about IT. The interactivity claim is therefore
    #    measured separately below, through the same popLevel() the key
    #    routes to.
    open_contents_on(port, CARGO_LIST_ID, DEF_DEEP_KIT)
    open_contents_on(port, level_list_id(2), "first_aid_kit")
    close_menu(port)
    start = stack_dump(port).get("depth")
    depths = [start]
    for _ in range(start or 0):
        send(port, "return input.key('Escape')")
        time.sleep(0.4)
        depths.append(stack_dump(port).get("depth"))
    check("a real Escape closes exactly ONE level per press, deepest first",
          depths == [3, 2, 1, 0], f"got {depths!r}")
    # Leave no pause menu behind for the blocks below.
    send(port, "require('scripts.pause_menu').hide(); return 'ok'")
    time.sleep(0.4)
    check("the pause menu the Escape cascade toggled is dismissed, and no "
          "modal boundary survives an emptied stack",
          send(port, "return UI.isInputBlocked()").strip() == "false",
          f"got {send(port, 'return UI.isInputBlocked()').strip()!r}")

    # -- Dismissal restores the newly deepest level each time.
    open_window_on(port, bid)
    open_contents_on(port, CARGO_LIST_ID, DEF_DEEP_KIT)
    open_contents_on(port, level_list_id(2), "first_aid_kit")
    close_menu(port)
    steps = []
    for _ in range(3):
        popped = send(port, "return require('scripts.cargo_inventory_panel')"
                            ".popLevel()").strip()
        time.sleep(0.3)
        d = stack_dump(port)
        deepest = ((d.get("levels") or [])[-1:] or [{}])[0]
        steps.append({
            "popped": popped,
            "depth": d.get("depth"),
            "scope": deepest.get("pageInScope") if d.get("depth") else None,
            "blocked": d.get("inputBlocked"),
        })
    check("each dismissal restores the newly deepest level's interactivity",
          [st["scope"] for st in steps[:2]] == [True, True],
          f"got {steps!r}")
    check("the modal boundary lifts once only the base level is left",
          steps[1]["blocked"] is False and steps[2]["blocked"] is False,
          f"got {[st['blocked'] for st in steps]!r}")
    check("every dismissal reports that it closed a level",
          [st["popped"] for st in steps] == ["true", "true", "true"],
          f"got {[st['popped'] for st in steps]!r}")

    # -- Per-level scroll survives a real resize, with the nesting path.
    #
    #    Both offsets are moved by the REAL wheel, each while its level
    #    IS the deepest one -- which is the only way a level can be
    #    scrolled at all, and therefore the only way a shallower level
    #    can be carrying an offset when a resize arrives.
    send(port, "require('scripts.cargo_inventory_panel').closeIfOpen();"
               " return 'ok'")
    time.sleep(0.3)
    open_window_on(port, bid)
    il = "require('scripts.ui.item_list')"
    max1 = float(send(port, f"return {il}.maxScrollOffset({level_list_id(1)})"))
    wheel_over_deepest(port)
    pushed = open_contents_on(port, CARGO_LIST_ID, DEF_DEEP_KIT)
    max2 = float(send(port, f"return {il}.maxScrollOffset({level_list_id(2)})"))
    if check("both levels have more rows than they can show at once",
             pushed and max1 >= 2 and max2 >= 2,
             f"pushed={pushed!r} max offsets {max1!r} / {max2!r} "
             f"depth={stack_dump(port).get('depth')!r}"):
        for _ in range(3):
            wheel_over_deepest(port)
        before = stack_dump(port)
        offsets = [l.get("scroll") for l in (before.get("levels") or [])]
        check("the wheel moved BOTH levels to distinct nonzero offsets",
              len(offsets) == 2 and all(isinstance(o, int) and o > 0
                                        for o in offsets)
              and offsets[0] != offsets[1],
              f"got {offsets!r}")

        send(port, "return engine.setWindowSize("
                   f"{vp['fb_w'] - 140}, {vp['fb_h'] - 100})")
        time.sleep(1.8)
        after = stack_dump(port)
        blv = before.get("levels") or []
        alv = after.get("levels") or []
        check("a resize preserves the whole nesting path",
              after.get("depth") == before.get("depth") == 2
              and [l.get("path") for l in alv] == [l.get("path") for l in blv]
              and [l.get("kind") for l in alv] == [l.get("kind") for l in blv],
              f"got {[(l.get('kind'), l.get('path')) for l in alv]!r} "
              f"vs {[(l.get('kind'), l.get('path')) for l in blv]!r}")
        check("a resize preserves EVERY level's own scroll offset",
              [l.get("scroll") for l in alv] == offsets,
              f"got {[l.get('scroll') for l in alv]!r} vs {offsets!r}")
        send(port, "return engine.setWindowSize("
                   f"{vp['fb_w']}, {vp['fb_h']})")
        time.sleep(1.5)

    # -- A UNIT-carried level renders LIVE contents, from the unit-info
    #    gesture's own entry point.
    send(port, "require('scripts.cargo_inventory_panel').closeIfOpen();"
               " return 'ok'")
    time.sleep(0.3)
    live_before = send_json(
        port, f"return unit.getItemContents({mule_uid}, 'first_aid_kit')")
    send(port, "require('scripts.item_contents_panel').openFor("
               f"{mule_uid}, 'first_aid_kit', 420, 320); return 'ok'")
    time.sleep(0.6)
    d = stack_dump(port)
    lvs = d.get("levels") or []
    check("the unit-info Contents gesture opens at the BASE level, live",
          d.get("depth") == 1 and len(lvs) == 1
          and lvs[0].get("kind") == "unitItem"
          and lvs[0].get("uid") == mule_uid
          and lvs[0].get("ageText") is None,
          f"got {lvs!r}")
    rows = item_rows(port, level_list_id(1))
    check("its rows are the engine's LIVE answer for that container",
          isinstance(live_before, list)
          and len(rows) == len(live_before),
          f"got {len(rows)} rows vs {len(live_before) if isinstance(live_before, list) else live_before!r}")
    send(port, "require('scripts.cargo_inventory_panel').closeIfOpen();"
               " return 'ok'")
    time.sleep(0.3)


def wheel_over_deepest(port: int) -> None:
    """One wheel notch over the DEEPEST level's own panel, located from
    the window's own dump rather than a computed coordinate."""
    d = stack_dump(port)
    levels = d.get("levels") or []
    if not levels:
        return
    bounds = send_json(
        port, "local p = require('scripts.ui.panel');"
              " local l = require('scripts.cargo_inventory_panel').getLevel();"
              " if not l or not l.panelId then return nil end;"
              " local x, y = p.getPosition(l.panelId);"
              " local w, h = p.getSize(l.panelId);"
              " return {x=x, y=y, w=w, h=h}")
    if not isinstance(bounds, dict):
        return
    cx = int(bounds.get("x", 0) + bounds.get("w", 0) / 2)
    cy = int(bounds.get("y", 0) + bounds.get("h", 0) / 2)
    send(port, f"return input.moveMouse({cx}, {cy})")
    send(port, "return input.scroll(0, -1)")
    time.sleep(0.25)

def main() -> int:
    ap = argparse.ArgumentParser()
    ap.add_argument("--port", type=int, default=9428)
    ap.add_argument("--size", default="1280x900")
    args = ap.parse_args()
    port = args.port

    print(f"booting offscreen engine on port {port} ({args.size}) ...")
    proc = boot(port, args=["--size", args.size],
                mode=("--offscreen",), ready_timeout=180.0)
    # Registered for teardown before ANY fallible work below (#1323): an
    # unexpected socket/parsing/widget exception used to skip every
    # quit_engine call and strand this engine holding its port.
    try:
        return _run(port, args)
    finally:
        quit_engine(port, proc)


def _run(port: int, args) -> int:
    menu_up = poll_until(60.0, lambda: find_widget(port, "Create World"))
    check("loading screen -> main menu", bool(menu_up))
    if not menu_up:
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

    # Both screen spaces, read off the ENGINE rather than the `--size`
    # string it was asked for: every hit test normalizes by the window
    # while `input.*` speaks framebuffer (#1286).
    vp = viewport(port, fallback=tuple(int(v) for v in args.size.split("x")))
    check("engine reports a usable window and framebuffer extent",
          vp["win_w"] > 0 and vp["win_h"] > 0 and vp["fb_w"] > 0
          and vp["fb_h"] > 0, f"got {vp!r}")
    print(f"  (window {vp['win_w']}x{vp['win_h']}, "
          f"framebuffer {vp['fb_w']}x{vp['fb_h']})")

    with open(TEST_BUILDING_YAML, "w") as f:
        f.write(TEST_BUILDINGS)
    n = send(port, f"return engine.loadBuildingYaml('{TEST_BUILDING_YAML}')")
    check("probe building defs loaded", float(n) == 3.0, f"got {n!r}")

    with open(TEST_ITEM_YAML, "w") as f:
        f.write(TEST_ITEMS)
    ni = send(port, f"return engine.loadItemYaml('{TEST_ITEM_YAML}')")
    check("probe deep-kit item def loaded", float(ni) >= 1.0, f"got {ni!r}")
    with open(TEST_UNIT_YAML, "w") as f:
        f.write(TEST_UNITS)
    nu = send(port, f"return engine.loadUnitYaml('{TEST_UNIT_YAML}')")
    check("probe kit-carrier unit def loaded", float(nu) >= 1.0, f"got {nu!r}")

    print("  (scanning terrain outward from the origin for dry anchor sites)")
    sites = allocate_dry_anchors(port, 7)
    if not check("found seven separated dry sites for the fixtures",
                 sites is not None):
        return 1
    ((bax, bay), (aax, aay), (max_, may_), (wax, way),
     (eax, eay), (uax, uay), (cax, cay)) = sites
    print(f"  (fixture sites: building={(bax, bay)} acolyte={(aax, aay)} "
          f"technomule={(max_, may_)} wildlife={(wax, way)} "
          f"empty-cargo={(eax, eay)} unseen-cargo={(uax, uay)} "
          f"kit-carrier={(cax, cay)})")

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
        return 1
    bid = int(float(bid_raw))
    check("storage building reaches Built activity",
          bool(poll_until(10.0, lambda: send(
              port, f"return building.getActivity({bid})").strip('"') == "built")))

    # -- #1237 fixtures. The instant-built one seeds known-empty at Built
    #    and is deliberately never stocked; the worker-built one never
    #    reaches Built at all, so it is never seeded and stays genuinely
    #    never-inspected.
    empty_bid = int(float(send(
        port, f"return building.spawn('{DEF_EMPTY}', {eax}, {eay})")))
    check("known-empty fixture reaches Built activity",
          bool(poll_until(10.0, lambda: send(
              port,
              f"return building.getActivity({empty_bid})").strip('"')
                  == "built")))
    unseen_bid = int(float(send(
        port, f"return building.spawn('{DEF_UNSEEN}', {uax}, {uay})")))
    check("never-inspected fixture stays UNBUILT (worker-built, zero "
          "progress, no construct_job AI running)",
          send(port, f"return building.getActivity({unseen_bid})").strip('"')
              != "built")

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
    # (matching the render cull), and offsets the quad by the difference,
    # so a building standing anywhere but ON the slice is drawn away from
    # the tile the camera converged on — unclickable no matter where the
    # camera points. The bare `camera.setZSlice` this used to do could not
    # achieve that: `goToTile` re-enables z-tracking, which rewrote the
    # slice back to `surface + 25` on the very next frame (#1286).
    bpixel = focus_building(port, bid, bax, bay, vp)
    cargo_scenario(port, bid, bpixel, vp, uid)

    knowledge_scenario(port, unseen_bid, empty_bid, uid)
    unit_endpoint_scenario(port, uid, wild_uid, vp)
    unit_inventory_scenario(port, uid)
    store_gesture_scenario(port, uid, bid)
    # After unit_inventory_scenario and knowledge_scenario: this one
    # strips the acolyte down and stocks the known-empty fixture, both
    # of which those two assert on first. Before item_contents_scenario,
    # whose first-aid kit it deliberately leaves carried.
    temperature_scenario(port, uid, empty_bid)
    item_contents_scenario(port, mule_uid, uid)

    # -- #1238 stock: the nesting fixtures go in LAST so nothing above
    #    (which asserts exact cargo row counts and inventory shapes) sees
    #    them. Each deposit is a real completed movement, so it also
    #    refreshes the container's knowledge record -- which is what
    #    gives the nested levels something remembered to render.
    #
    #    The two CONTAINERS go in first and the bulk after, so both stay
    #    inside the base level's first rendered rows while the level as a
    #    whole still has more rows than its cap.
    carrier_uid = int(float(send(
        port, f"return unit.spawn('{DEF_CARRIER}', {cax}, {cay}, nil, 'player')")))
    for defname in CARGO_BULK_STOCK:
        send(port, f"unit.addItem({uid}, '{defname}');"
                   f" unit.depositToCargo({uid}, {bid}, '{defname}');"
                   " return 'ok'", timeout=20.0)
    send(port, f"unit.depositToCargo({carrier_uid}, {bid}, '{DEF_DEEP_KIT}');"
               f" unit.addItem({uid}, 'first_aid_kit');"
               f" unit.depositToCargo({uid}, {bid}, 'first_aid_kit');"
               " return 'ok'", timeout=20.0)
    known = send_json(port, f"return building.getContainerKnowledge({bid})")
    kit_iid = None
    for row in (known or {}).get("items", []):
        if isinstance(row, dict) and row.get("defName") == DEF_DEEP_KIT:
            kit_iid = row.get("instanceId")
    stocked = send_json(
        port, f"return building.getRememberedItemContents({bid}, {{{kit_iid}}})"
        ) if isinstance(kit_iid, int) else None
    nested_rows = len((stocked or {}).get("items", []))
    if check("the deep kit is remembered in the cargo, STOCKED with more "
             "rows than a level can show at once, with an instance id",
             isinstance(kit_iid, int) and kit_iid > 0 and nested_rows > 12,
             f"got id={kit_iid!r} nested rows={nested_rows!r}"):
        nesting_stack_scenario(port, bid, int(kit_iid), mule_uid, vp)

    # -- #1250 LAST: it spawns its own escort, opens a window at the base
    #    level (replacing anything above), commits real items into the
    #    cargo and leaves the stack empty, so nothing that asserts exact
    #    cargo contents or row counts may run after it.
    escort_session_scenario(port, bid, bax, bay, vp)
    # -- #1251 after it, and last of all: it spawns two more acolytes,
    #    walks one of them, and takes over the base level the escort
    #    scenario just emptied.
    unit_escort_session_scenario(port, aax, aay)

    return probe_result()


if __name__ == "__main__":
    sys.exit(main())
