#!/usr/bin/env python3
"""The endpoint scenarios of `tools/item_list_widget_probe.py` (#2046):
the container window over a BUILDING and over a UNIT, what the player is
allowed to know about a container's contents, and one item's own
contents.

  * `cargo_scenario` — #1088's rendered contract on a building endpoint,
    plus #1249's queued Retrieve gestures on its rows.
  * `unit_endpoint_scenario` — #1234's generalization: the same window,
    the same manager, a unit's own `transferEndpointInfo`, and a
    wildlife unit that cannot open it at all.
  * `knowledge_scenario` — #1237's three remembered states and the
    "as of…" age, graded against the engine's own `revealedAt`.
  * `item_contents_scenario` — the pre-grouped rows of one carried
    container, its render-only rows, and its "(empty)" state.

Scenario BODIES only. Everything these four share with any other
scenario module — the widget oracle, the gestures, the duplicate-row
check, the fixture ids and the level-addressing expressions — comes from
the support modules, never from a sibling scenario module.
"""
from __future__ import annotations

import os
import sys
import time

sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
from item_list_widget_probe_checks import check, check_no_duplicate_rows
from item_list_widget_probe_fixtures import (CARGO_LIST_ID, CARGO_STOCK,
                                             DEF_EMPTY_BOX,
                                             ITEM_CONTENTS_LIST_ID)
from item_list_widget_probe_oracle import (chrome_text, click_widget_center,
                                           close_menu, expected_age,
                                           find_widget, format_age, game_time,
                                           item_rows, knowledge,
                                           open_window_on, orders_of,
                                           panel_chrome,
                                           right_click_widget_center,
                                           tab_boxes, widgets)
from probelib import poll_until, send, send_json, set_paused, win_to_fb

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
