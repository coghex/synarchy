#!/usr/bin/env python3
"""The widget oracle and the real-input gestures for
`tools/item_list_widget_probe.py` (#2046).

Every target this probe touches is located through `ui.dumpWidgets()`
and every interaction is a real injected click, right-click or scroll —
never a hardcoded screen coordinate and never a window API substituted
for a player gesture. Those two rules are this module's whole subject,
so it owns the reads (`widgets`, `find_widget`, `item_rows`,
`tab_boxes`, `panel_chrome`, `chrome_text`), the engine-side oracles the
rendered text is graded against (`format_age`, `knowledge`, `game_time`,
`expected_age`, `orders_of`) and the gestures themselves
(`click_widget_center`, `right_click_widget_center`, `close_menu`).

`stack_dump` and `level_list_id` live here rather than with the nesting
scenarios because both escort scenarios and the nesting scenario call
them. A helper with consumers in more than one scenario module is shared
support; putting it in one of them would force the other to import a
sibling's implementation detail.

Imports no sibling module: it reads the engine and reports what is
rendered, and grades nothing itself.
"""
from __future__ import annotations

import math
import os
import sys
import time

sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
from probelib import send, send_json


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
    """Mirror of `formatAge` in scripts/cargo_inventory_endpoints.lua.

    (It lived in scripts/cargo_inventory_panel.lua until #2155 split the
    endpoint owner out; the manager still re-exports it.)

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
# The container window's own stack (#1238), read by escort AND nesting
# --------------------------------------------------------------------------
def stack_dump(port: int) -> dict:
    got = send_json(port, "return require('scripts.cargo_inventory_panel').dump()")
    return got if isinstance(got, dict) else {}


def level_list_id(index: int) -> str:
    """A debug-console expression naming level `index`'s widget instance."""
    return (f"(require('scripts.cargo_inventory_panel').getLevel({index})"
            " or {}).listId")
