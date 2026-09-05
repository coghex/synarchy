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
`building.spawn`ed instance stuck "constructing" forever with no
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

Since #2046 the scenario bodies live in four owner-scoped modules and
their shared support in four more. This file is the sole executable
entry point and the only registered probe: the eight below are
libraries, none of them named `*_probe.py`, and none boots an engine,
generates a world, opens a port or spawns a subprocess.

  `..._endpoints`   cargo, unit endpoint, last-known knowledge, item
                    contents
  `..._inventory`   unit inventory, tracked temperature, Store gesture
  `..._escort`      both escort sessions, their movement staging and the
                    two-sided hold
  `..._nesting`     the container-window stack, row scrolling, Contents
                    gestures, deepest-level wheel routing

  `..._fixtures`    the authored defs, the stock lists, the staged
                    `Fixtures` state
  `..._oracle`      `ui.dumpWidgets()` reads and the real-input gestures
  `..._terrain`     dry-anchor allocation and the #1286 camera focus
  `..._checks`      `check`, `probe_result` and the failure accumulator

A helper with consumers in more than one scenario module belongs in
support, never in one of them: that is why `stack_dump`/`level_list_id`
(escort and nesting) and `check_no_duplicate_rows` (endpoints and
inventory) sit in `..._oracle` and `..._checks` rather than beside their
first caller.

`_run` below is the orchestration authority: one short sequence naming
every scenario in the order it runs and every mutation of the shared
fixture state between them.

The failure accumulator is `item_list_widget_probe_checks.failures`, a
module-level int rebound by `check`. Read it through the module, never
with `from item_list_widget_probe_checks import failures` — that binds a
stale 0 and turns every failed check into a green run.

Usage: python3 tools/item_list_widget_probe.py
       [--port 9428] [--size 1280x900]
"""
from __future__ import annotations

import argparse
import os
import sys
import time

sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
from item_list_widget_probe_checks import check, probe_result
from item_list_widget_probe_endpoints import (cargo_scenario,
                                              item_contents_scenario,
                                              knowledge_scenario,
                                              unit_endpoint_scenario)
from item_list_widget_probe_escort import (escort_session_scenario,
                                           unit_escort_session_scenario)
from item_list_widget_probe_fixtures import (CARGO_BULK_STOCK, CARGO_STOCK,
                                             DEF_CARGO, DEF_CARRIER,
                                             DEF_DEEP_KIT, DEF_EMPTY,
                                             DEF_UNSEEN, Fixtures,
                                             stage_fixture_defs)
from item_list_widget_probe_inventory import (store_gesture_scenario,
                                              temperature_scenario,
                                              unit_inventory_scenario)
from item_list_widget_probe_nesting import nesting_stack_scenario
from item_list_widget_probe_oracle import click_widget_center, find_widget
from item_list_widget_probe_terrain import allocate_dry_anchors, focus_building
from probelib import boot, poll_until, quit_engine, send, send_json, viewport


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


def bootstrap_world(port: int, args):
    """Menu -> generated world -> in-game HUD, and the two screen spaces
    every later hit test is measured in.

    The one world this run generates. No scenario module boots an engine
    or generates a world; this is the only place either happens.
    Answers the viewport, or None when the main menu never appeared."""
    menu_up = poll_until(60.0, lambda: find_widget(port, "Create World"))
    check("loading screen -> main menu", bool(menu_up))
    if not menu_up:
        return None
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
    return vp


def stage_fixtures(port: int, vp: dict):
    """Register the authored defs, find seven separated dry sites, spawn
    every fixture and stock the two the early scenarios read.

    The shared state the scenarios below then mutate in order. Answers
    the staged `Fixtures`, or None when the storage building — which
    every endpoint check needs — could not be spawned."""
    stage_fixture_defs(port)

    print("  (scanning terrain outward from the origin for dry anchor sites)")
    sites = allocate_dry_anchors(port, 7)
    if not check("found seven separated dry sites for the fixtures",
                 sites is not None):
        return None
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
        return None
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
    return Fixtures(bid=bid, empty_bid=empty_bid, unseen_bid=unseen_bid,
                    uid=uid, mule_uid=mule_uid, wild_uid=wild_uid,
                    building_site=(bax, bay), acolyte_site=(aax, aay),
                    carrier_site=(cax, cay), bpixel=bpixel)


def stage_nesting_stock(port: int, fx: Fixtures):
    """#1238 stock, deposited only AFTER every exact cargo row count and
    inventory shape above has been asserted.

    The two CONTAINERS go in first and the bulk after, so both stay
    inside the base level's first rendered rows while the level as a
    whole still has more rows than its cap. Each deposit is a real
    completed movement, so it also refreshes the container's knowledge
    record — which is what gives the nested levels something remembered
    to render. Answers the deep kit's instance id, or None when the
    fixture did not come out stocked deeply enough to nest."""
    cax, cay = fx.carrier_site
    carrier_uid = int(float(send(
        port, f"return unit.spawn('{DEF_CARRIER}', {cax}, {cay}, nil, 'player')")))
    for defname in CARGO_BULK_STOCK:
        send(port, f"unit.addItem({fx.uid}, '{defname}');"
                   f" unit.depositToCargo({fx.uid}, {fx.bid}, '{defname}');"
                   " return 'ok'", timeout=20.0)
    send(port, f"unit.depositToCargo({carrier_uid}, {fx.bid}, '{DEF_DEEP_KIT}');"
               f" unit.addItem({fx.uid}, 'first_aid_kit');"
               f" unit.depositToCargo({fx.uid}, {fx.bid}, 'first_aid_kit');"
               " return 'ok'", timeout=20.0)
    known = send_json(port,
                      f"return building.getContainerKnowledge({fx.bid})")
    kit_iid = None
    for row in (known or {}).get("items", []):
        if isinstance(row, dict) and row.get("defName") == DEF_DEEP_KIT:
            kit_iid = row.get("instanceId")
    stocked = send_json(
        port, f"return building.getRememberedItemContents({fx.bid}, {{{kit_iid}}})"
        ) if isinstance(kit_iid, int) else None
    nested_rows = len((stocked or {}).get("items", []))
    if not check("the deep kit is remembered in the cargo, STOCKED with more "
                 "rows than a level can show at once, with an instance id",
                 isinstance(kit_iid, int) and kit_iid > 0 and nested_rows > 12,
                 f"got id={kit_iid!r} nested rows={nested_rows!r}"):
        return None
    return int(kit_iid)


def _run(port: int, args) -> int:
    """The ordered sequence, and the authority on it.

    Ten scenarios in one load-bearing order, over one engine, one world
    and one shared fixture graph. Every ordering constraint between them
    is stated at the call site it constrains; none of them is enforced
    anywhere else, so a scenario moved here without its comment is a
    silent change to what the checks below it mean."""
    vp = bootstrap_world(port, args)
    if vp is None:
        return 1
    fx = stage_fixtures(port, vp)
    if fx is None:
        return 1

    cargo_scenario(port, fx.bid, fx.bpixel, vp, fx.uid)
    knowledge_scenario(port, fx.unseen_bid, fx.empty_bid, fx.uid)
    unit_endpoint_scenario(port, fx.uid, fx.wild_uid, vp)
    unit_inventory_scenario(port, fx.uid)
    store_gesture_scenario(port, fx.uid, fx.bid)
    # After unit_inventory_scenario and knowledge_scenario: this one
    # strips the acolyte down and stocks the known-empty fixture, both
    # of which those two assert on first. Before item_contents_scenario,
    # whose first-aid kit it deliberately leaves carried.
    temperature_scenario(port, fx.uid, fx.empty_bid)
    item_contents_scenario(port, fx.mule_uid, fx.uid)

    # -- #1238 stock: the nesting fixtures go in LAST so nothing above
    #    (which asserts exact cargo row counts and inventory shapes) sees
    #    them.
    kit_iid = stage_nesting_stock(port, fx)
    if kit_iid is not None:
        nesting_stack_scenario(port, fx.bid, kit_iid, fx.mule_uid, vp)

    # -- #1250 LAST: it spawns its own escort, opens a window at the base
    #    level (replacing anything above), commits real items into the
    #    cargo and leaves the stack empty, so nothing that asserts exact
    #    cargo contents or row counts may run after it.
    bax, bay = fx.building_site
    escort_session_scenario(port, fx.bid, bax, bay, vp)
    # -- #1251 after it, and last of all: it spawns two more acolytes,
    #    walks one of them, and takes over the base level the escort
    #    scenario just emptied.
    aax, aay = fx.acolyte_site
    unit_escort_session_scenario(port, aax, aay)

    return probe_result()


if __name__ == "__main__":
    sys.exit(main())
