#!/usr/bin/env python3
"""Widget, targeting, contract-reader and fixture helpers for the transfer
GESTURE stages (`tools/transfer_gesture_stages.py`), which
`tools/transfer_context_menu_probe.py` runs.

Library code, not a probe: no CLI, no engine boot, and not registered. It
is what remained of the retired end-to-end transfer probe's helper layer
(#1255, #2048) once that probe was replaced by focused coverage - see
`docs/archive/transfer_end_to_end_probe_coverage_map.md` for which checks moved
and why. Four concerns:

  * the WIDGET oracle - `ui.dumpWidgets()` and the item-list widget's own
    `dump()`, plus the real `input.*` gestures aimed at what they locate.
    Never a hardcoded coordinate;
  * WORLD-ENTITY targeting - `building.hitTestAt` for a building and
    `unit.hitTestInRect` bisection confirmed by `unit.hitTestAt` for a
    unit, and the z-slice-aware camera placement (#1286) that has to
    precede either;
  * CONTRACT readers - the endpoint projection, `checkTransfer`'s
    structured result, durable orders, the event log and the live Mode A
    session, each read as the shipped contract reports it;
  * FIXTURE support - the throwaway item defs, the level-shelf site
    search, spawning, staging, row location, window opening, and the unit
    control (`calm`) the gestures depend on.
"""
from __future__ import annotations

import os
import re
import sys
import time

sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
from probelib import (camera_state, centred_within, clear_find_water,
                      focus_and_locate, locate_building_pixel, poll_until,
                      send, send_json, set_paused, targeting_report,
                      win_to_fb)
from transfer_probe_checks import Checks

CHUNK_TILES = 16
SEARCH_RADIUS = 60

# --------------------------------------------------------------------------
# Throwaway content
# --------------------------------------------------------------------------
# ONE item def per directed LEG. A merged row's singular gesture names the
# row's representative, which is its FIRST member - so a source holding two
# instances of the same def would let "Store 1" move an instance this probe
# did not choose, and the leg would still pass. With one def per leg the
# source holds exactly one instance of it, the row is single-membered, and
# the entry the gesture creates can be compared against an EXACT instance
# id. (It also exercises #1249's rule that a single-instance row shows the
# singular entry alone.)
#
# 0.25 kg is deliberately a BINARY fraction: capacity is a 32-bit Float and
# the gate is `load + weight <= capacity` summed one accepted item at a
# time, so a decimal-friendly fixture lands a few ulps over. `bulk` is
# REQUIRED by `Engine.Asset.YamlItems` and one bad definition rejects the
# whole FILE.
LEG_DEFS_B = [f"probe_ut_b{i}" for i in range(1, 7)]
LEG_DEFS_A = [f"probe_ut_a{i}" for i in range(1, 7)]
ALL_ITEM_DEFS = LEG_DEFS_B + LEG_DEFS_A

ITEMS_YAML = "items:\n" + "".join(f"""\
  - name: "{d}"
    display_name: "Probe UT {d}"
    sprite: "assets/textures/items/material/bar_steel.png"
    weight: 0.25
    bulk: 0.25
    category: Materials
""" for d in ALL_ITEM_DEFS)

# Debug-console expressions naming the live widget instances this reads.
# The container window owns a STACK of levels (#1238), so every read goes
# through the manager's own accessor because a level may not exist.
WINDOW = "require('scripts.cargo_inventory_panel')"
BASE_LEVEL = f"({WINDOW}.getLevel(1) or {{src={{}}}})"
CARGO_LIST_ID = f"{BASE_LEVEL}.listId"
UNIT_INV_LIST_ID = "require('scripts.unit_info_v2').invListId"


def pane_list_id(pane_key: str) -> str:
    """A debug-console expression naming one escort pane's list instance,
    addressed through the manager's own pane accessor rather than by
    indexing `panes` positionally."""
    return ("(function() local c = require('scripts.cargo_inventory_panel');"
            f" local p = c.getPane(c.getLevel(1), '{pane_key}');"
            " return p and p.listId end)()")


def load_item_defs(chk: Checks, port: int, base: str) -> bool:
    """Register the leg item defs. Written OUTSIDE the checkout (into this
    run's temp dir) so nothing under `data/` is ever touched, and loaded by
    absolute path. A rejected file is reported as a plain 0, so an exact
    count is what tells "registered" from "silently registered nothing"."""
    items = os.path.join(base, "transfer_gesture_items.yaml")
    with open(items, "w", encoding="utf-8") as f:
        f.write(ITEMS_YAML)
    ni = send(port, f"return engine.loadItemYaml('{items}')").strip()
    return chk.ok(_num(ni) == float(len(ALL_ITEM_DEFS)),
                  f"the {len(ALL_ITEM_DEFS)} throwaway leg item defs "
                  f"registered", f"engine.loadItemYaml returned {ni!r}")


# --------------------------------------------------------------------------
# Widget oracle (never a hardcoded coordinate)
# --------------------------------------------------------------------------
def widgets(port: int):
    got = send_json(port, "return ui.dumpWidgets()", timeout=20.0)
    return got if isinstance(got, list) else []


def find_widget(port: int, label: str):
    for w in widgets(port):
        if (w.get("label") or "").strip().lower() == label.lower():
            return w
    return None


def menu_labels(port: int) -> list:
    return [w.get("label") for w in widgets(port) if w.get("label")]


def resolve_list_id(port: int, list_id_lua: str):
    raw = send(port, f"local id = {list_id_lua}; return tostring(id)"
               ).strip().strip('"')
    return None if raw in ("", "nil", "null") else raw


def item_rows(port: int, list_id_lua: str):
    """The rendered rows of ONE item-list instance, straight from the
    widget's own dump(). Scoped by instance because several hosts are on
    screen at once and an unscoped read would mix their rows."""
    raw = resolve_list_id(port, list_id_lua)
    if raw is None:
        return []
    prefix = f"item_list:{raw.split('.')[0]}:"
    return [w for w in widgets(port)
            if w.get("type") == "item_list"
            and (w.get("id") or "").startswith(prefix)]


def row_named(rows, def_name):
    for w in rows:
        if (w.get("defName") or "") == def_name:
            return w
    return None


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


def close_window(port: int) -> None:
    send(port, f"{WINDOW}.closeIfOpen(); return 'ok'")
    time.sleep(0.3)


# --------------------------------------------------------------------------
# World-entity targeting (the production hit tests, never a coordinate)
# --------------------------------------------------------------------------
def _hit_rect_has(port: int, uid: int, x1: int, y1: int, x2: int,
                  y2: int) -> bool:
    raw = send(port, f"return unit.hitTestInRect({x1}, {y1}, {x2}, {y2})")
    return str(uid) in re.findall(r"\d+", raw)


def _hit_at_is(port: int, uid: int, x: int, y: int) -> bool:
    """Does a right-click at this pixel really reach `uid`?

    Two questions, because the ROUTER asks two:
    `scripts/init_context_menu.lua` tries `tryBuildingMenu` BEFORE
    `tryUnitMenu`, so a pixel where a building also hit-tests opens the
    BUILDING's menu however unambiguously the unit answers. Confirming
    only `unit.hitTestAt` there produced a session on a cargo hold when
    the probe had asked for one on the technomule standing beside it —
    caught by the identity check, but only after the wrong session
    existed."""
    raw = send(port, f"return unit.hitTestAt({x}, {y})").strip().strip('"')
    if raw in ("", "nil", "null"):
        return False
    try:
        if int(float(raw)) != uid:
            return False
    except ValueError:
        return False
    b = send(port, f"return building.hitTestAt({x}, {y})").strip().strip('"')
    return b in ("", "nil", "null")


def locate_unit_pixel(port: int, uid: int, vp: dict, max_steps: int = 14):
    """Bisect `unit.hitTestInRect` down to the small WINDOW-space box
    holding `uid`'s sprite quad, then CONFIRM with `unit.hitTestAt` —
    the technique `tools/transfer_context_menu_probe.py` validated.
    The bisection alone is not enough: hitTestInRect answers "is uid
    ANYWHERE in this rect", so with several units milling around the
    box's centre pixel can sit on a neighbour's overlapping sprite."""
    x1, y1, x2, y2 = 0, 0, vp["win_w"], vp["win_h"]
    if not _hit_rect_has(port, uid, x1, y1, x2, y2):
        return None
    for _ in range(max_steps):
        if x2 - x1 <= 2 and y2 - y1 <= 2:
            break
        mx, my = (x1 + x2) // 2, (y1 + y2) // 2
        for qx1, qy1, qx2, qy2 in ((x1, y1, mx, my), (mx, y1, x2, my),
                                   (x1, my, mx, y2), (mx, my, x2, y2)):
            if _hit_rect_has(port, uid, qx1, qy1, qx2, qy2):
                x1, y1, x2, y2 = qx1, qy1, qx2, qy2
                break
        else:
            break
    cx, cy = (x1 + x2) // 2, (y1 + y2) // 2
    if _hit_at_is(port, uid, cx, cy):
        return cx, cy
    for r in range(1, 7):
        d = r * 3
        for dx, dy in ((0, -d), (0, d), (-d, 0), (d, 0),
                       (-d, -d), (d, -d), (-d, d), (d, d)):
            if _hit_at_is(port, uid, cx + dx, cy + dy):
                return cx + dx, cy + dy
    return None


def focus_entity(chk: Checks, port: int, kind: str, tid: int, vp: dict,
                 label: str):
    """Put a world entity on a targetable WINDOW-space pixel and answer
    it, or None. `camera.goToTile` alone leaves z-tracking ON, which
    pins the slice 25 levels above the surface and pushes the target
    clean off the viewport (#1286) — `focus_and_locate` re-pins the
    slice to the target's OWN gridZ after every goToTile."""
    info = send_json(port, f"return {kind}.getInfo({tid})")
    if not isinstance(info, dict):
        chk.ok(False, f"{label}: the {kind} still resolves", f"got {info!r}")
        return None
    gx, gy = int(info.get("gridX", 0)), int(info.get("gridY", 0))
    gz = int(info.get("gridZ", 0))
    locate = ((lambda: locate_building_pixel(port, tid, vp)) if kind == "building"
              else (lambda: locate_unit_pixel(port, tid, vp)))
    pixel = focus_and_locate(port, gx, gy, gz, vp, locate)
    cam = camera_state(port)
    chk.ok(cam.get("zTracking") is False and cam.get("zSlice") == gz
           and isinstance(cam.get("zoom"), (int, float))
           and cam.get("zoom") < 1.2,
           f"{label}: camera preconditions settled (z-tracking off, slice == "
           f"the {kind}'s gridZ, tile zoom band)", f"got {cam!r} for gridZ {gz}")
    if not chk.ok(pixel is not None,
                  f"{label}: located the {kind}'s own screen pixel through "
                  f"the production hit test"):
        print(targeting_report(port, vp, kind, tid, site=(gx, gy)))
        return None
    chk.ok(centred_within(vp, pixel),
           f"{label}: the camera centres on the {kind}",
           f"got {pixel!r} for a {vp['win_w']}x{vp['win_h']} window")
    return pixel


def right_click_pixel(port: int, pixel, vp: dict) -> None:
    """Right-click a WINDOW-space pixel. `input.*` takes FRAMEBUFFER
    pixels and converts to window space itself, so the located pixel has
    to cross back the other way first (#1286)."""
    fx, fy = win_to_fb(vp, pixel[0], pixel[1])
    send(port, f"return input.moveMouse({fx}, {fy})")
    send(port, f"return input.click({fx}, {fy}, 'right')")
    time.sleep(0.5)


# --------------------------------------------------------------------------
# Contract readers
# --------------------------------------------------------------------------
def ep_lua(ep) -> str:
    """A Lua endpoint literal from a `(kind, id)` PAIR.

    Unpacked rather than indexed so passing a bare uid — which every
    endpoint in this probe also has sitting in a local beside it — fails
    here, naming the argument, instead of surfacing several frames later
    as an opaque TypeError inside a stage."""
    kind, ident = ep
    return "{ kind = '%s', id = %d }" % (kind, int(ident))


def ep_info(port: int, ep) -> dict:
    got = send_json(port, f"return unit.transferEndpointInfo({ep_lua(ep)})")
    return got if isinstance(got, dict) else {}


def ep_ids(port: int, ep, def_name: str) -> list:
    """The exact instance ids of `def_name` an endpoint holds, as the
    transfer contract's own projection reports them."""
    return sorted(int(it["instanceId"])
                  for it in (ep_info(port, ep).get("contents") or [])
                  if it.get("defName") == def_name
                  and isinstance(it.get("instanceId"), (int, float)))


def ep_rect(port: int, ep):
    info = ep_info(port, ep)
    try:
        return (int(info["gridX"]), int(info["gridY"]),
                int(info.get("tileW", 1)), int(info.get("tileH", 1)))
    except (KeyError, TypeError, ValueError):
        return None


def footprint_gap(port: int, a, b):
    """The contract's OWN measure between two endpoints — Chebyshev
    between occupied RECTANGLES — or None when either end is unknown.
    `withinReach` is <= 1, so this is what "they are NOT adjacent" means."""
    ra, rb = ep_rect(port, a), ep_rect(port, b)
    if ra is None or rb is None:
        return None
    ax, ay, aw, ah = ra
    bx, by, bw, bh = rb
    dx = max(bx - (ax + aw - 1), ax - (bx + bw - 1), 0)
    dy = max(by - (ay + ah - 1), ay - (by + bh - 1), 0)
    return max(dx, dy)


def request_lua(src, dst, def_name: str, ids) -> str:
    """The contract's own request table for these exact instances."""
    items = ", ".join("{ instanceId = %d, defName = '%s' }" % (int(i), def_name)
                      for i in ids)
    return ("{ source = %s, destination = %s, items = { %s } }"
            % (ep_lua(src), ep_lua(dst), items))


def check_transfer(port: int, src, dst, def_name: str, ids):
    """`unit.checkTransfer` on that request — the STRUCTURED result:
    `{ accepted, completion, outcomes }`, one outcome per requested item
    in request order.

    This is the contract's own answer rather than a consequence of it.
    Bracketing a gesture with it is what distinguishes "the policy
    accepted this exact instance and then moved it" from "the item ended
    up somewhere plausible": a mis-encoded or refused outcome cannot
    produce an accepted check beforehand AND a refusal afterwards.
    Read-only by construction — `checkTransfer` validates and mutates
    nothing, which is exactly why it can be asked twice around a real
    commit."""
    got = send_json(port, "return unit.checkTransfer(%s)"
                    % request_lua(src, dst, def_name, ids))
    return got if isinstance(got, dict) else None


def outcome_states(result) -> list:
    """`state[:reason[/cause]]` per outcome, in request order."""
    out = []
    for o in (result or {}).get("outcomes") or []:
        if not isinstance(o, dict):
            continue
        text = str(o.get("state"))
        if o.get("reason"):
            text += ":" + str(o["reason"])
        if o.get("cause"):
            text += "/" + str(o["cause"])
        out.append(text)
    return out


def outcome_ids(result) -> list:
    return [o.get("instanceId") for o in (result or {}).get("outcomes") or []
            if isinstance(o, dict)]


def assert_structured_move(chk: Checks, port: int, label: str, src, dst,
                           def_name: str, ids, before,
                           deferred_reach: bool) -> None:
    """The two halves of the structured-result assertion for one leg.

    The BEFORE half differs by mode, and the difference is the arc's own
    reach split rather than a convenience: `checkTransfer` and
    `commitTransfer` still REQUIRE adjacency, and only
    `createTransferOrder` defers it (`ReachPolicy`).

      * Mode A commits on the spot, with the escort standing there, so
        the contract must ACCEPT: completion "all", one outcome per
        requested instance in request order, no failures.
      * Mode B fires from across the map, so the contract must refuse —
        and refuse for RANGE alone. `out_of_range` on the exact
        requested ids, with the request itself structurally accepted, is
        the sharpest statement of Mode B's whole promotion: the policy
        would not move these items now, and the ORDER may still be
        created.

    The AFTER half is shared and is what proves the move: the identical
    request, asked once the carrier is adjacent, must fail on IDENTITY
    (`instance_missing`) rather than on range. End-state ownership
    cannot tell those two apart; this can."""
    ids = [int(i) for i in ids]
    before_states = outcome_states(before)
    if deferred_reach:
        chk.ok(isinstance(before, dict) and outcome_ids(before) == ids
               and before_states
               and all(st == "failed:out_of_range" for st in before_states),
               f"{label}: before the gesture the contract refuses this exact "
               f"request for RANGE alone (`out_of_range` on every requested "
               f"instance) — which is precisely what Mode B's order defers",
               f"got {before!r}")
    else:
        chk.ok(isinstance(before, dict) and before.get("accepted") is True
               and before.get("completion") == "all"
               and outcome_ids(before) == ids
               and all(not st.startswith("failed") for st in before_states),
               f"{label}: the contract ACCEPTS the exact request before the "
               f"gesture — completion 'all', one outcome per requested "
               f"instance in request order, none of them a failure",
               f"got {before!r}")
    after = check_transfer(port, src, dst, def_name, ids)
    after_states = outcome_states(after)
    chk.ok(isinstance(after, dict) and outcome_ids(after) == ids
           and after_states
           and all(st == "failed:instance_missing" for st in after_states),
           f"{label}: and afterwards it refuses the identical request on "
           f"IDENTITY, not on range — `instance_missing` per instance is the "
           f"contract saying those exact items left the source",
           f"got {after!r}")


def orders(port: int, uid: int) -> list:
    data = send_json(port, f"return unit.getTransferOrders({uid})")
    return data if isinstance(data, list) else []


def entry_states(order: dict) -> list:
    """One stored order's per-item states, in entry order, each as
    `state[:reason[/cause]]` — the durable structured record #1246 keeps
    for a queued transfer."""
    out = []
    for e in order.get("entries") or []:
        if not isinstance(e, dict):
            continue
        text = str(e.get("state"))
        if e.get("reason"):
            text += ":" + str(e["reason"])
        if e.get("cause"):
            text += "/" + str(e["cause"])
        out.append(text)
    return out


def order_identity(order: dict) -> dict:
    """Everything durable about one order: its id, both endpoint
    identities and the exact instance ids it names, in entry order."""
    return {
        "id": order.get("id"),
        "source": f"{(order.get('source') or {}).get('kind')}"
                  f":{(order.get('source') or {}).get('id')}",
        "destination": f"{(order.get('destination') or {}).get('kind')}"
                       f":{(order.get('destination') or {}).get('id')}",
        "items": [e.get("instanceId") for e in (order.get("entries") or [])],
    }


def event_log(port: int) -> list:
    data = send_json(port, "return engine.getEventLog()", timeout=20.0)
    return data if isinstance(data, list) else []


def event_total(port: int, uid: int, category: str, needle: str) -> int:
    """How many times a matching event has been emitted for `uid`. The
    log COALESCES identical consecutive entries into one row carrying a
    `count`, so counting rows reports one for two emissions."""
    total = 0
    for e in event_log(port):
        if (e.get("category") == category and e.get("uid") == uid
                and needle in (e.get("text") or "")):
            total += int(e.get("count", 1) or 1)
    return total


def session(port: int):
    return send_json(port, "return require('scripts.transfer_session').get()")


def session_phase(port: int) -> str:
    return send(port, "local s = require('scripts.transfer_session').get();"
                      " return s and s.phase or 'none'").strip().strip('"')


def ai_action(port: int, uid: int) -> str:
    return send(port, "local s = require('scripts.unit_ai').getState("
                      f"{uid}); return s and s.currentAction or 'nil'"
                ).strip().strip('"')


def retire_medic_drive(port: int, uid: int) -> bool:
    """Take `uid` out of the medic squad. `treat_ally` scores 8.0,
    deliberately ABOVE the 7.5 escort hold, and its patient scan reaches
    60 tiles — so ONE bleeding ally anywhere in this world makes an
    acolyte walk away from a hold, correctly, and every escort
    measurement below would then be measuring that documented exception.
    One console statement, so a tick cannot interleave and re-claim."""
    send(port, f"unit.setKnowledge({uid}, 'bleed_control', 0);"
               f" local s = require('scripts.unit_ai').getState({uid});"
               " if s then s.treatClaim = nil; s.treatPending = nil end;"
               " return 'ok'")
    try:
        return float(send(port, f"return unit.getKnowledge({uid},"
                                " 'bleed_control')").strip().strip('"')) == 0.0
    except (TypeError, ValueError):
        return False


def sustain(port: int, uid: int) -> None:
    """Top one unit's survival needs back up. Apply while PAUSED.

    A gate that walks the same acolyte through a dozen legs is also
    running the #306 survival ladder on it, and that ladder deliberately
    outranks Mode A's 7.5 hold: an escort that gets hungry enough stops
    escorting and eats (observed as `eat_from_inventory` mid-approach),
    and one that collapses ends the session outright through the
    incapacitated-source rule (observed as a session going
    `approaching` -> gone with no stale reason ever sampled). Neither is
    a transfer-system fact, so pinning the physiological precondition is
    what makes these stages measure the transfer system — the same
    technique `docs/engine_contracts.md`'s expedition gate uses in
    reverse when it SEEDS a deficit on purpose.

    Every need is topped up to its OWN maximum rather than to a
    constant: body mass varies several-fold across acolytes, so an
    absolute value would mean something different per unit.

    All four are RESOURCES, not deficits — `sleep_pressure` included,
    which is why it is set to its maximum and not to zero. `go_to_sleep`
    scores on `1 - sleep_pressure / max_sleep_pressure` and disables
    itself outright below `sleep_min_deficit`, so a full meter is what
    takes the sleep drive out of the measurement; a zeroed one is the
    sleepiest a unit can be. Its maximum lives on `scripts.unit_stats`
    rather than on the engine stat table, which is why it is read
    separately."""
    send(port, "local u = %d;"
               " for _, pair in ipairs({ {'hunger','max_hunger'},"
               " {'hydration','max_hydration'},"
               " {'calories','max_calories'} }) do"
               "  local m = unit.getStat(u, pair[2]);"
               "  if m then unit.setStat(u, pair[1], m) end end;"
               " local us = require('scripts.unit_stats');"
               " local msp = us.get(u, 'max_sleep_pressure');"
               " if msp then unit.setStat(u, 'sleep_pressure', msp) end;"
               " return 'ok'" % uid)


def calm(port: int, uid: int) -> None:
    """Retire every standing drive that outscores or races the gestures
    this probe measures, for one unit."""
    clear_find_water(port, uid)
    retire_medic_drive(port, uid)
    sustain(port, uid)


def _num(raw, default: float = -1.0) -> float:
    try:
        return float(raw)
    except (TypeError, ValueError):
        return default


# --------------------------------------------------------------------------
# Fixture siting
# --------------------------------------------------------------------------
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


def _search_centres(rings: int = 2, spacing: int = 2 * SEARCH_RADIUS):
    pts = [(0, 0)]
    for r in range(1, rings + 1):
        d = r * spacing
        pts += [(d, 0), (-d, 0), (0, d), (0, -d),
                (d, d), (-d, -d), (d, -d), (-d, d)]
    return pts


# The whole site search runs ENGINE-SIDE in one debug-console statement.
# A per-tile round trip over a 48-tile radius is thousands of them, and
# the corridor sampling multiplies that again; done in Lua with a
# memoized surface lookup it is one call.
#
# Elevation is the whole reason the corridor half exists. Every walk in
# this scenario is a REAL walk, and on the ridged terrain a fixed seed
# happens to produce, two dry tiles at the same height can still be
# separated by a climb: an acolyte plays `climb` / `climb_pullup` engine
# animations, during which its AI does not re-decide at all, so a leg
# only a dozen tiles long can outlast any sane budget — and accumulated
# falls are how the other offscreen probes lose units mid-run. A flat
# shelf removes both.
#
# Hubs are tried in distance order rather than the first dry tile being
# taken as the hub: seed 42's origin sits on a ridge between z 9 and z
# 45, so the nearest dry tile is a hub with no shelf around it at all.
_ANCHOR_SCAN_LUA = (
    "local ox, oy, R, ST, N, SEP, BAND, HUBS, CLUS ="
    " %d, %d, %d, %d, %d, %d, %d, %d, %d;"
    " local memo = {};"
    " local function surf(x, y)"
    "  local k = x * 100000 + y; local v = memo[k];"
    "  if v ~= nil then return v ~= false and v or nil end;"
    "  local z, _t, ft = world.getSurfaceAt(x, y);"
    "  if z == nil or ft ~= nil then memo[k] = false; return nil end;"
    "  memo[k] = z; return z end;"
    " local cand = {};"
    " for dx = -R, R, ST do for dy = -R, R, ST do"
    "  local z = surf(ox + dx, oy + dy);"
    "  if z then cand[#cand + 1] ="
    "   { x = ox + dx, y = oy + dy, z = z, d = dx * dx + dy * dy } end"
    " end end;"
    " table.sort(cand, function(a, b) return a.d < b.d end);"
    " local function corridor(a, b, z0)"
    "  for i = 1, 6 do local t = i / 7;"
    "   local x = math.floor(a.x + (b.x - a.x) * t + 0.5);"
    "   local y = math.floor(a.y + (b.y - a.y) * t + 0.5);"
    "   local z = surf(x, y);"
    "   if not z or math.abs(z - z0) > BAND then return false end"
    "  end; return true end;"
    " for h = 1, math.min(#cand, HUBS) do"
    "  local hub = cand[h]; local picked = { hub };"
    "  for i = 1, #cand do local c = cand[i];"
    "   if c ~= hub and math.abs(c.z - hub.z) <= BAND"
    "    and math.max(math.abs(c.x - hub.x), math.abs(c.y - hub.y)) <= CLUS"
    "   then"
    "    local ok = true;"
    "    for _, p in ipairs(picked) do"
    "     if math.max(math.abs(c.x - p.x), math.abs(c.y - p.y)) < SEP then"
    "      ok = false; break end end;"
    "    if ok and corridor(hub, c, hub.z) then picked[#picked + 1] = c end;"
    "    if #picked >= N then break end"
    "   end end;"
    "  if #picked >= N then local out = {};"
    "   for i, p in ipairs(picked) do out[i] = p.x .. ',' .. p.y end;"
    "   return table.concat(out, ';') end"
    " end; return 'none'")


def allocate_flat_anchors(port: int, n: int, min_sep: int = 6,
                          radius: int = 48, bands=(0, 1), hubs: int = 40,
                          cluster: int = 16):
    """`n` separated dry tiles on ONE LEVEL shelf, with a level corridor
    from each to the hub, searched outward from the origin.

    `bands` is tried in order and 0 — every site and every sampled
    corridor tile at the SAME z — is what this really wants. A drop of
    more than one z level is a FALL, every fall is a knockdown
    (`Unit.Thread.Movement.Timers`: the unit enters the Collapsed pose
    with a self-timed get-up), and an incapacitated endpoint ends a Mode
    A session by rule. That is transient enough to be invisible to any
    poll and long-walk-biased enough to look like an intermittent Mode A
    bug: it cost two full runs before the pose was caught in the act. A
    ±1 band is offered as a fallback so a world with no perfectly level
    shelf still runs rather than failing setup outright.

    `cluster` is what keeps the shelf a NEIGHBOURHOOD rather than a
    scatter: without it the corridor rule alone is happy to place the
    hub at one end of a long flat plain and the rest of the fixtures 40
    tiles away at the other, and every leg then becomes a 40-tile haul.

    `min_sep` keeps every fixture out of every other one's footprint —
    `scripts/init_context_menu.lua` tries the BUILDING menu before the
    unit menu, so a unit standing on a hold's tile would open the wrong
    one — and it is what makes the "no adjacency was required" half of
    Mode B a real observation: an endpoint pair this far apart is
    nowhere near the contract's Chebyshev <= 1 reach when the gesture
    fires."""
    span = radius // CHUNK_TILES + 1
    for band in bands:
        for ox, oy in _search_centres(rings=2, spacing=2 * radius):
            ccx, ccy = ox // CHUNK_TILES, oy // CHUNK_TILES
            send(port, f"return world.loadChunksInRegion({ccx - span}, "
                       f"{ccy - span}, {ccx + span}, {ccy + span})")
            send(port, "return world.waitForChunks(120)", timeout=125.0)
            raw = send(port,
                       _ANCHOR_SCAN_LUA % (ox, oy, radius, 4, n, min_sep,
                                           band, hubs, cluster),
                       timeout=180.0).strip().strip('"')
            if raw and raw != "none" and "," in raw:
                out = []
                for pair in raw.split(";"):
                    gx, gy = pair.split(",")
                    out.append((int(gx), int(gy)))
                if len(out) == n:
                    print(f"  (shelf found at z-band {band})", flush=True)
                    return out
    return None


def spawn_hold(chk: Checks, port: int, def_name: str, gx: int, gy: int,
               label: str, want_built: bool = True):
    raw = send(port, f"return building.spawn('{def_name}', {gx}, {gy})")
    try:
        bid = int(float(raw))
    except (TypeError, ValueError):
        chk.ok(False, f"{label}: building.spawn accepted", f"got {raw!r}")
        return None
    built = poll_until(15.0, lambda: send(
        port, f"return building.getActivity({bid})").strip().strip('"') == "built")
    if want_built:
        if not chk.ok(bool(built), f"{label}: reaches Built activity"):
            return None
    else:
        chk.ok(built is None,
               f"{label}: stays UNBUILT (worker-built, zero progress, no "
               f"construct_job AI running), so nothing has ever seeded it")
    return bid


def spawn_unit(chk: Checks, port: int, def_name: str, gx: int, gy: int,
               label: str, faction: str | None = "player",
               quiet: bool = False):
    """`unit.spawn` defaults to the WILDLIFE faction when no tag is
    given, so `faction=None` is how this probe asks for one."""
    tag = f", nil, '{faction}'" if faction else ""
    raw = send(port, f"return unit.spawn('{def_name}', {gx}, {gy}{tag})")
    try:
        uid = int(float(raw))
    except (TypeError, ValueError):
        chk.ok(False, f"{label}: unit.spawn accepted", f"got {raw!r}")
        return None
    if faction == "player" and not quiet:
        calm(port, uid)
    return uid


def add_one(port: int, uid: int, def_name: str) -> None:
    send(port, f"return unit.addItem({uid}, '{def_name}')")


def stage_into_hold(port: int, stager: int, bid: int, def_name: str) -> None:
    """Put one instance of `def_name` into a building's loose storage.

    Staging only, and deliberately through the LAX AI verb (D-7): what
    this probe measures is the PLAYER paths, and using one of them to
    set up the other's precondition would make a leg assert its own
    fixture. `depositToCargo` also refreshes container knowledge, which
    is what the modeB stage's own reveal check then reads."""
    add_one(port, stager, def_name)
    send(port, f"return unit.depositToCargo({stager}, {bid}, '{def_name}')",
         timeout=20.0)


# --------------------------------------------------------------------------
# Locating a rendered row
# --------------------------------------------------------------------------
def tab_boxes(port: int, list_id_lua: str):
    """The rendered tab boxes belonging to ONE item-list instance.

    Bounds come from the real widget oracle; WHICH records belong to this
    host is resolved by intersecting their element handles with the
    widget's own `getTabs()` (the tab strip's dump reports a tab's
    visible LABEL as its name, so the engine-side element name is not
    available there)."""
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
    order = [e["handle"] for e in own if isinstance(e, dict)]
    out.sort(key=lambda w: order.index(w["handle"]))
    return out


def find_row(port: int, list_id_lua: str, def_name: str,
             category: str = "Materials"):
    """One rendered row, reached the way a player reaches one.

    A list renders only the rows that FIT and `dump()` reports only
    rendered rows, so a row past the fold is simply absent. Two
    escalations, both gestures the player has: select the row's own
    category tab, then scroll. Neither is the gesture under test — the
    right-click and the click on the located menu entry are — so this is
    navigation, not measurement."""
    def look():
        return row_named(item_rows(port, list_id_lua), def_name)

    row = look()
    if row:
        return row
    tab = next((t for t in tab_boxes(port, list_id_lua)
                if (t.get("label") or "").split(" (")[0] == category), None)
    if tab:
        click_widget_center(port, tab)
        time.sleep(0.7)
        row = look()
        if row:
            return row
    max_off = int(_num(send(port, "return require('scripts.ui.item_list')"
                                  f".maxScrollOffset({list_id_lua})"), 0))
    for off in range(0, max_off + 1):
        send(port, "require('scripts.ui.item_list').setScrollOffset("
                   f"{list_id_lua}, {off}); return 'ok'")
        time.sleep(0.35)
        row = look()
        if row:
            return row
    return None


def open_window(chk: Checks, port: int, ep, label: str) -> bool:
    """Open the container window on one endpoint through the same entry
    point the "Contents" context-menu row calls."""
    accepted = send(port, f"return {WINDOW}.openFor('{ep[0]}', {ep[1]},"
                          " 240, 240)").strip()
    time.sleep(0.7)
    is_open = send(port, f"return {WINDOW}.isOpen()").strip() == "true"
    return chk.ok(accepted == "true" and is_open,
                  f"{label}: the container window opens on {ep[0]} #{ep[1]}",
                  f"openFor returned {accepted!r}, isOpen={is_open}")


# --------------------------------------------------------------------------
# Fixture staging and unit control, shared by every leg of both modes
# --------------------------------------------------------------------------
# `seed_source` and `retire_leg_item` are the two halves of the same
# rule: each leg mints its OWN throwaway def, so every leg starts from
# the same load however many earlier ones have completed. `send_home`
# is what makes a leg's gesture fire from where the mode says it can —
# a carrier left standing at the endpoint it just served would make
# Mode B's "no adjacency was required" a fact about the fixture rather
# than about the mode.
def seed_source(port: int, src, def_name: str, stager: int) -> None:
    if src[0] == "unit":
        add_one(port, src[1], def_name)
    else:
        stage_into_hold(port, stager, src[1], def_name)


def retire_leg_item(port: int, dst, def_name: str) -> None:
    """Take a finished leg's item back out of a UNIT destination.

    Twelve legs deliver into three endpoints, and the acolyte is the
    destination of four of them. Left in place the deliveries ACCUMULATE,
    so a later leg's headroom depends on how many earlier Mode B orders
    have already completed — which is a timing question, not a contract
    one. That is not hypothetical: an unretired run refused
    `modeA storage->acolyte` with the contract's own `receiver_full`, and
    the two `Retrieve 1` gestures into the same full acolyte then queued
    no order at all, so the run failed while the contract was behaving
    exactly as specified.

    Retiring makes every leg start from the same load. Each leg mints its
    OWN throwaway def, so removing BY DEF NAME cannot touch another leg's
    item or the fixtures. Building destinations are deliberately left
    alone: those hold 200 kg against twelve sub-kilogram items, and the
    lax cargo verbs are the AI's path, not something a gate for the
    strict policy should be routing its own cleanup through.
    """
    kind, ident = dst
    if kind != "unit":
        return
    send(port, "return unit.removeItem(%d, '%s')" % (int(ident), def_name))


def adjacent_dry_tiles(port: int, gx: int, gy: int) -> list:
    """Every dry tile beside `(gx, gy)`.

    Beside, never ON. The contract's reach is Chebyshev <= 1, so an
    adjacent tile is already in range of a 1x1 endpoint and nothing is
    gained by standing on its footprint — while a unit spawned inside a
    building's own tile is measurably destroyed by it: the first version
    of this probe put its escort there and found the acolyte carrying
    nine wounds and playing `injured_death` a minute later, which ends
    the session through the incapacitated-source rule and reads exactly
    like a Mode A bug."""
    out = []
    for dx, dy in ((1, 0), (-1, 0), (0, 1), (0, -1),
                   (1, 1), (-1, -1), (1, -1), (-1, 1)):
        info = tile_surface(port, gx + dx, gy + dy)
        if info is not None and info[1]:
            out.append((gx + dx, gy + dy))
    return out


# --------------------------------------------------------------------------
# The escort-session lifecycle: this module's PUBLIC interface
# --------------------------------------------------------------------------
# `knowledge`, `batch` and `save` each open a real escort session too,
# and they open it through exactly these entry points. There is one
# implementation of "create a session, run the world until the escort
# arrives and the pair opens, and tear the whole thing down again",
# because the three ways an open can fail are only distinguishable when
# every caller samples the same phases on the way.
def stack_dump(port: int) -> dict:
    got = send_json(port, f"return {WINDOW}.dump()")
    return got if isinstance(got, dict) else {}


def session_names(port: int, src_uid: int, dst_ep):
    """`(does the live session name exactly these endpoints, the session)`."""
    s = session(port)
    return (isinstance(s, dict)
            and (s.get("source") or {}).get("id") == src_uid
            and (s.get("destination") or {}).get("kind") == dst_ep[0]
            and (s.get("destination") or {}).get("id") == dst_ep[1]), s


def create_session(chk: Checks, port: int, src_uid: int, dst_ep, vp: dict,
                   label: str, via_menu: bool) -> bool:
    """Create a Mode A session — through the REAL right-click "Transfer"
    row when `via_menu`, otherwise through the same
    `transfer_session.create` that row's callback calls.

    The menu route is a REQUIRED observation with a fallback, never a
    silent one: a row that cannot be located, and a row that produced a
    session on some OTHER endpoint, are both recorded as failures here
    and the session is then built directly so the legs below still
    report. A silent fallback would delete these checks instead of
    failing them."""
    send(port, "unit.deselectAll(); return 'ok'")
    send(port, f"return unit.select({src_uid})")
    time.sleep(0.5)
    if via_menu:
        kind = "building" if dst_ep[0] == "building" else "unit"
        pixel = focus_entity(chk, port, kind, dst_ep[1], vp, label)
        if pixel is None:
            via_menu = False   # focus_entity recorded the localization failure
    if via_menu:
        right_click_pixel(port, pixel, vp)
        labels = menu_labels(port)
        entry = find_widget(port, "Transfer")
        if chk.ok(bool(entry),
                  f"{label}: the right-click menu offers 'Transfer'",
                  f"menu labels: {labels!r}"):
            click_widget_center(port, entry)
            time.sleep(0.8)
            named, live = session_names(port, src_uid, dst_ep)
            if chk.ok(named,
                      f"{label}: activating the rendered 'Transfer' row "
                      f"created the session on the exact endpoint identity "
                      f"{dst_ep[0]}:{dst_ep[1]}", f"got {live!r}"):
                return True
            send(port, "require('scripts.transfer_session').clear();"
                       " return 'ok'")
        else:
            close_menu(port)
    made = send(port, "return tostring(require('scripts.transfer_session')"
                      f".create({src_uid}, '{dst_ep[0]}', {dst_ep[1]})"
                      " ~= nil)").strip().strip('"')
    if not chk.ok(made == "true", f"{label}: a Mode A session is created",
                  f"got {made!r}"):
        return False
    named, live = session_names(port, src_uid, dst_ep)
    return chk.ok(named,
                  f"{label}: the session names the exact endpoint identities",
                  f"got {live!r}")


def await_session_open(chk: Checks, port: int, src_uid: int, label: str,
                       seconds: float = 150.0) -> bool:
    """Run the world until the escort arrives and the pair opens.

    Every distinct phase is recorded on the way, because the three ways
    this can fail are not distinguishable from the final state alone:
    stuck on `approaching` is an AI that never won, a phase that reached
    `open` and then vanished is a TEARDOWN (a zoom-band change and a HUD
    hide both clear a session), and never seeing a phase at all is a
    session that was refused."""
    seen: list = []
    stale: list = []
    poses: list = []

    def look():
        # `staleReason` is PUBLIC precisely so a gate can ask the same
        # question the session's own tick asks; sampled alongside the
        # phase, it is what tells "the AI never won" apart from "an
        # endpoint stopped qualifying and the session was torn down".
        got = send(port, "local ts = require('scripts.transfer_session');"
                         " local s = ts.get();"
                         " return (s and s.phase or 'none') .. '/' .."
                         " tostring(s and ts.staleReason() or 'n/a') .. '/'"
                         f" .. tostring(unit.getPose({src_uid}))"
                   ).strip().strip('"')
        phase, _, rest = got.partition("/")
        reason, _, pose = rest.partition("/")
        if pose and pose not in poses:
            poses.append(pose)
        if not seen or seen[-1] != phase:
            seen.append(phase)
        if reason not in ("nil", "n/a", "") and reason not in stale:
            stale.append(reason)
        return phase == "open"

    set_paused(port, False)
    opened = poll_until(seconds, look, interval=0.5)
    set_paused(port, True)
    time.sleep(0.6)
    return chk.ok(bool(opened),
                  f"{label}: the REAL unit AI walks the escort and opens the "
                  f"pair on arrival",
                  f"phases seen {seen!r}, stale reasons seen {stale!r}, "
                  f"escort poses seen {poses!r} (a Collapsed one is a FALL "
                  f"knockdown, which ends the session by rule), escort "
                  f"running {ai_action(port, src_uid)!r}, stack depth "
                  f"{stack_dump(port).get('depth')!r}, camera "
                  f"{camera_state(port)!r}")


def close_session(chk: Checks, port: int, held: list, label: str) -> None:
    """One dismissal takes both panes AND the session AND every hold."""
    send(port, f"{WINDOW}.popLevel(); return 'ok'")
    time.sleep(0.8)
    chk.ok(session(port) is None,
           f"{label}: closing the level ends the session (one coupled teardown)")
    for uid in held:
        got = send(port, "return tostring(require('scripts.transfer_session')"
                         f".holdsUnit({uid}))").strip().strip('"')
        chk.ok(got == "false", f"{label}: unit {uid} is released by the "
                               f"session ending", f"holdsUnit = {got!r}")


