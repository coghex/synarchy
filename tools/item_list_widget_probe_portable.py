#!/usr/bin/env python3
"""The PORTABLE container window level (#2527, epic #1231 PLC-17) for
`tools/item_list_widget_probe.py`.

The rendered gate on a crate that stands on the FLOOR having a window at
all: the ground-item right-click menu offering "Contents" for an item
whose definition declares `storage:` and for nothing else, the level
that entry opens with no unit and no building involved, its four
knowledge states drawn as four different screens, a descent into a
remembered nested container, and the rule that none of it ever writes
knowledge or reads the live crate.

Everything here goes through real input on real Vulkan-rendered pixels:
the crate is localized with `item.hitTestAt` (the very hit test
`scripts/init_context_menu.lua` routes a right-click through), the menu
is opened by a real right-click, and its entry is clicked by its own
rendered label. The two knowledge verbs the states need are called
directly — they are PLC-7's, they have their own gate, and the window
deliberately never calls them, so a probe that made the window
manufacture its own states would be asserting the opposite of the
contract.

One screenshot is captured with the level open on a known-contents
crate: this is the visual evidence #2527's acceptance asks for, and the
path is reported so the run's caller can attach it.
"""
from __future__ import annotations

import os
import sys
import time

sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
from item_list_widget_probe_checks import check
from item_list_widget_probe_fixtures import DEF_GROUND_CRATE
from item_list_widget_probe_oracle import (click_widget_center, close_menu,
                                           find_widget, stack_dump)
from probelib import send, send_json, set_paused


def _menu_offers(port: int, label: str) -> bool:
    """Is `label` a RENDERED entry of the open context menu?

    Read off the live widget tree (`ui.dumpWidgets`, through
    `find_widget`) rather than the table the host handed
    `context_menu.show`: what is under test is what the player can
    actually click, and a probe that read the host's own table would
    pass on an entry that never reached the screen."""
    return bool(find_widget(port, label))


def _right_click(port: int, pixel) -> None:
    x, y = pixel
    send(port, f"return input.moveMouse({x}, {y})")
    send(port, f"return input.click({x}, {y}, 'right')")
    time.sleep(0.4)


def _base_level(port: int) -> dict:
    levels = stack_dump(port).get("levels") or []
    return levels[0] if levels else {}


def _level(port: int, index: int) -> dict:
    levels = stack_dump(port).get("levels") or []
    return levels[index - 1] if len(levels) >= index else {}


def _close_stack(port: int) -> None:
    send(port, "require('scripts.cargo_inventory_panel').closeIfOpen();"
               " return 'ok'")
    time.sleep(0.2)


def _open_contents_from_ground(port: int, pixel) -> bool:
    """The whole player gesture: right-click the crate, find the
    rendered "Contents" entry, click it."""
    _right_click(port, pixel)
    entry = find_widget(port, "Contents")
    if not entry:
        close_menu(port)
        return False
    click_widget_center(port, entry)
    time.sleep(0.4)
    return True


def portable_scenario(port: int, crate_gid: int, crate_pixel,
                      bar_pixel, shot_path: str):
    """#2527: the ground crate's own window, end to end.

    `crate_pixel` and `bar_pixel` are window-space points the two ground
    items really hit-test to; a None for either means localization
    failed, which is itself a failed check rather than a skipped one."""
    print("-- portable container level (#2527)")
    try:
        _portable_checks(port, crate_gid, crate_pixel, bar_pixel, shot_path)
    finally:
        # Hand the simulation BACK RUNNING, however this scenario ended.
        # Localizing the crate froze it (`focus_ground_item`, the #1286
        # discipline every hit test here needs), and the escort
        # scenarios that follow spawn units and then wait for the REAL
        # AI to give them state — which a paused world never does, so a
        # scenario that left the freeze in place would fail the two
        # behind it rather than itself.
        set_paused(port, False)


def _portable_checks(port: int, crate_gid: int, crate_pixel,
                     bar_pixel, shot_path: str):
    if not check("the ground crate was localized on a real screen pixel",
                 crate_pixel is not None):
        return
    crate_iid = send(port, "for _, g in ipairs(item.listGround() or {}) do"
                           f"  if g.id == {crate_gid} then"
                           "    return g.instanceId end end;"
                           " return -1").strip()
    crate_iid = int(float(crate_iid))
    check("the crate's ground row carries its own instance id and the "
          "definition's storage declaration",
          crate_iid > 0
          and send(port, "for _, g in ipairs(item.listGround() or {}) do"
                         f"  if g.id == {crate_gid} then"
                         "    return tostring(g.hasStorage) end end;"
                         " return 'missing'").strip().strip('"') == "true",
          f"got instanceId={crate_iid!r}")

    # -- 1. The entry is offered for a storage-declaring item, and for
    #       no other ground item. Both menus are opened by a REAL
    #       right-click on a REAL rendered pixel.
    _right_click(port, crate_pixel)
    offers_contents = _menu_offers(port, "Contents")
    opened_at_all = _menu_offers(port, "Info")
    check("a storage-declaring ground item's right-click menu renders a "
          "Contents entry", offers_contents and opened_at_all,
          f"Contents={offers_contents!r} Info={opened_at_all!r}")
    close_menu(port)
    if bar_pixel is not None:
        _right_click(port, bar_pixel)
        bar_contents = _menu_offers(port, "Contents")
        bar_info = _menu_offers(port, "Info")
        check("an ordinary ground item's menu opens and offers NO "
              "Contents entry — a container kind or a fluid capacity is "
              "not a storage declaration",
              bar_info and not bar_contents,
              f"Contents={bar_contents!r} Info={bar_info!r}")
        close_menu(port)
    else:
        check("the ordinary ground item was localized for the negative "
              "case", False)

    # -- 2. Never-inspected. The level opens, names no unit and no
    #       building, and says the contents are unknown rather than
    #       drawing an empty list.
    check("the ground Contents entry opens a level",
          _open_contents_from_ground(port, crate_pixel))
    lvl = _base_level(port)
    check("the level is a portableItem addressed by the crate's own "
          "INSTANCE id, with no unit and no building in its identity",
          lvl.get("kind") == "portableItem"
          and lvl.get("instanceId") == crate_iid
          and lvl.get("uid") is None and lvl.get("bid") is None,
          f"got {lvl!r}")
    check("never-inspected renders as unknown, with the LIVE capacity, "
          "no age and an empty text that is not '(empty)'",
          lvl.get("knowledgeState") == "unknown"
          and "unknown" in str(lvl.get("subtitle"))
          and "60.00" in str(lvl.get("subtitle"))
          and lvl.get("ageText") is None
          and lvl.get("emptyText") == "Contents unknown (never inspected)"
          and lvl.get("rowCount") == 0,
          f"got {lvl!r}")
    _close_stack(port)

    # -- 3. Weight-only: hefted, never opened. PLC-7's own verb takes
    #       the observation; the window never would.
    send(port, f"return item.observeContainerWeight({crate_iid})")
    time.sleep(0.5)
    check("the weight-only state opens", _open_contents_from_ground(port, crate_pixel))
    lvl = _base_level(port)
    check("weight-only renders the remembered whole mass with the "
          "contents still unknown, and an age from the WEIGHING",
          lvl.get("knowledgeState") == "weight-only"
          and "unknown" not in str(lvl.get("subtitle"))
          and lvl.get("ageText") is not None
          and lvl.get("emptyText") == "Contents unknown (never opened)"
          and lvl.get("rowCount") == 0,
          f"got {lvl!r}")
    _close_stack(port)

    # -- 4. Known contents, and the descent into a remembered nested
    #       container.
    send(port, f"return item.observeContainerContents({crate_iid})")
    time.sleep(0.5)
    check("the known-contents state opens",
          _open_contents_from_ground(port, crate_pixel))
    lvl = _base_level(port)
    check("known-contents renders the remembered rows with a contents "
          "age and no empty text at all",
          lvl.get("knowledgeState") == "known"
          and lvl.get("rowCount", 0) >= 2
          and lvl.get("ageText") is not None
          and lvl.get("emptyText") is None,
          f"got {lvl!r}")

    # The visual evidence, taken with the level open and populated.
    shot = send_json(port, f"return debug.captureScreenshot('{shot_path}')",
                     timeout=30.0)
    check("a screenshot of the open portable level was captured",
          bool(shot) and os.path.isfile(shot_path), f"wrote {shot_path}")
    print(f"  (portable level screenshot: {shot_path})")

    # -- 5. A container ROW opens a deeper level that keeps the ROOT
    #       crate's identity and merely extends the path: a nested crate
    #       has no record of its own.
    rows = send_json(port, "local il = require('scripts.ui.item_list');"
                           " local l = require('scripts.cargo_inventory_panel')"
                           "   .getLevel(1);"
                           " local out = {};"
                           " for i, r in ipairs(il.getRows(l.listId)) do"
                           "   out[i] = (r.item or {}).defName end;"
                           " return out")
    kit_row = None
    if isinstance(rows, list):
        for i, name in enumerate(rows, start=1):
            if name == "first_aid_kit":
                kit_row = i
    if check("the base level renders the nested container row",
             kit_row is not None, f"got {rows!r}"):
        opened = send(port, "local il = require('scripts.ui.item_list');"
                            " local cip = require('scripts.cargo_inventory_panel');"
                            " local cm = require('scripts.ui.context_menu');"
                            " local l = cip.getLevel(1);"
                            f" local row = il.getRows(l.listId)[{kit_row}];"
                            " local got, orig = nil, cm.show;"
                            " cm.show = function(items) got = items end;"
                            " il.handleCallback('onItemListRightClick', row.hitId);"
                            " cm.show = orig;"
                            " for _, e in ipairs(got or {}) do"
                            "   if e.label == 'Contents' then e.callback();"
                            "     return 'true' end end;"
                            " return 'false'").strip().strip('"')
        time.sleep(0.4)
        deep = _level(port, 2)
        check("a container row pushes a nested portable level that keeps "
              "the ROOT crate's instance id and extends the path",
              opened == "true" and deep.get("kind") == "portableItem"
              and deep.get("instanceId") == crate_iid
              and len(deep.get("path") or []) == 1,
              f"opened={opened!r} got {deep!r}")
        check("a portable row offers ONLY the inspection entry — no "
              "transfer and no Retrieve gesture on a render-only level",
              _portable_row_labels(port, 1, kit_row) == ["Contents"],
              f"got {_portable_row_labels(port, 1, kit_row)!r}")

    # -- 6. Never a live read. Emptying the real crate on the floor
    #       changes nothing the level renders.
    before = _level(port, 1).get("rowCount")
    send(port, f"return item.removeGround({crate_gid})")
    time.sleep(0.6)
    check("emptying the real crate off the floor leaves the remembered "
          "level rendering exactly what it rendered before",
          _level(port, 1).get("rowCount") == before,
          f"before={before!r} after={_level(port, 1).get('rowCount')!r}")
    _close_stack(port)


def _portable_row_labels(port: int, level: int, row_index: int) -> list[str]:
    got = send_json(port, "local il = require('scripts.ui.item_list');"
                          " local cip = require('scripts.cargo_inventory_panel');"
                          " local cm = require('scripts.ui.context_menu');"
                          f" local l = cip.getLevel({level});"
                          " if not l then return {} end;"
                          f" local row = il.getRows(l.listId)[{row_index}];"
                          " if not row then return {} end;"
                          " local got, orig = nil, cm.show;"
                          " cm.show = function(items) got = items end;"
                          " il.handleCallback('onItemListRightClick', row.hitId);"
                          " cm.show = orig;"
                          " local out = {};"
                          " for i, e in ipairs(got or {}) do out[i] = e.label end;"
                          " return out")
    return [str(x) for x in got] if isinstance(got, list) else []
