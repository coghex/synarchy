#!/usr/bin/env python3
"""The unit-inventory scenarios of `tools/item_list_widget_probe.py`
(#2046): the inventory section of the unit-info panel, the tracked
temperature both raw-item hosts present, and the Store gesture a row
offers.

  * `unit_inventory_scenario` — #1088's rendered contract on the
    unit-info host: rows and counts, a wrapped tab strip inside the
    section rect, tab filtering, and a row's real Equip/Contents menu.
  * `temperature_scenario` — #1268's tracked temperature in the row text
    and its tooltip, a group reporting all three of its values, an
    EQUIPPED row, the rebuild boundary, and a deposited instance
    carrying its temperature into the container window.
  * `store_gesture_scenario` — #1249's "Store" on a unit-inventory row,
    queued as a real durable transfer order.

Scenario BODIES only, plus `unit_inv_row`, which is theirs alone: the
temperature and Store scenarios are its only two callers.

`temperature_scenario` runs AFTER `unit_inventory_scenario` and
`knowledge_scenario` and BEFORE `item_contents_scenario` — it strips the
acolyte down and stocks the known-empty fixture, and leaves the
first-aid kit carried on purpose. That ordering is the facade's to
enforce and is documented at the call site.
"""
from __future__ import annotations

import os
import sys
import time

sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
from item_list_widget_probe_checks import check, check_no_duplicate_rows
from item_list_widget_probe_fixtures import CARGO_LIST_ID, UNIT_INV_LIST_ID
from item_list_widget_probe_oracle import (click_widget_center, close_menu,
                                           find_widget, item_rows, orders_of,
                                           right_click_widget_center,
                                           tab_boxes, widgets)
from probelib import send, send_json, set_paused

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
