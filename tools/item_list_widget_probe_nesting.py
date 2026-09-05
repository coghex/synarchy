#!/usr/bin/env python3
"""The nesting-stack scenario of `tools/item_list_widget_probe.py`
(#2046): #1238's ordered stack of container-window levels.

`nesting_stack_scenario` is the rendered gate on pushing, replacing,
dismissing and restoring levels, on only the deepest one being
interactive, on a level's own scroll offset surviving a real framebuffer
resize, and on a building-side level rendering the engine's REMEMBERED
contents with the PARENT's own age while the unit-info gesture opens a
LIVE level at the base.

`scroll_to_row`, `open_contents_on` and `wheel_over_deepest` are its own
helpers — no other scenario module calls them. The stack READS
(`stack_dump`, `level_list_id`) are not: both escort scenarios use them
too, so they live in the shared oracle module instead.
"""
from __future__ import annotations

import os
import sys
import time

sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
from item_list_widget_probe_checks import check
from item_list_widget_probe_fixtures import CARGO_LIST_ID, DEF_DEEP_KIT
from item_list_widget_probe_oracle import (click_widget_center, close_menu,
                                           find_widget, item_rows,
                                           level_list_id, open_window_on,
                                           right_click_widget_center,
                                           stack_dump, widgets)
from probelib import send, send_json


# --------------------------------------------------------------------------
# #1238: the nesting stack
# --------------------------------------------------------------------------
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
