#!/usr/bin/env python3
"""End-to-end integration gate for the unified transfer system (#1255,
epic #1013 slice UIT-6 — the arc's final child).

Every earlier slice of the arc has its own focused gate. This one proves
they hold together: ONE fixed-seed session in which an exact item
instance moves in BOTH directions between all three endpoint CLASSES
(acolyte, technomule, built storage — two of which share the contract's
`unit` endpoint KIND), reaching the same commit policy through BOTH
player modes, plus the batch, knowledge, widget and persistence
behaviours the arc's design decisions promise.

Shaped after `tools/expedition_loop_probe.py` (#923): independently
reported STAGES so a failure names which part broke, a FRESH process
that re-checks every durable identity, one deterministic `FINGERPRINT`
line so two consecutive runs can be diffed for identity AND result, and
operational failures recorded against the stage they interrupted rather
than allowed to traceback past a PASS summary.

`--offscreen` (GPU on, window off) is required and is what the bare
invocation boots: Mode A's flanking panes, Mode B's row menus and the
container window are real Vulkan-rendered UI, and the real HUD flow
never boots headless (it gates on `fontsReady`, a GPU font atlas) while
`--headless` refuses `input.*` injection outright. `--offscreen` is an
ENGINE boot mode, not a probe argument — nothing has to be passed on the
command line.

TARGETING. Two different oracles, never a hardcoded coordinate:

  * rendered UI controls (menu rows, tab boxes, item rows) are located
    through `ui.dumpWidgets()` and the item-list widget's own `dump()`;
  * WORLD ENTITIES are located and CONFIRMED through the production hit
    tests — `building.hitTestAt` for a building, `unit.hitTestInRect`
    bisection plus `unit.hitTestAt` for a unit — exactly as
    `tools/transfer_context_menu_probe.py` does, after
    `probelib.focus_and_locate` pins the camera's z-slice to the
    target's own `gridZ` (#1286).

Every item-moving gesture below is a REAL right-click on a REAL rendered
row followed by a REAL click on the located menu entry. The OPENING
routes are driven through the real right-click menu wherever the shipped
game has one — "Contents" on a storage building for the container
window, "Transfer" on a building and on a unit for a Mode A session —
and the repeats then call the same entry point that menu row's callback
calls, because what this gate measures is the transfer, not the menu
(which `tools/transfer_context_menu_probe.py` owns). A UNIT endpoint's
container window has NO world right-click route at all
(`scripts/init_context_menu.lua`'s unit menu offers Info / Attack /
Transfer / Cancel transfer and no "Contents"), so the window manager's
own entry point IS the route for one; asserting a menu row that does not
exist would be asserting a feature this arc did not ship.

ENVIRONMENT, and why each choice is load-bearing rather than incidental.
Every one of them was a live failure first:

  * the fixtures are sited on ONE LEVEL SHELF with a level corridor
    between them (`allocate_flat_anchors`), and LEVEL is meant
    literally. Seed 42's origin sits on a ridge between z 9 and z 45,
    and a carrier crossing it plays `climb` / `climb_pullup` engine
    animations during which its AI does not re-decide at all, so a
    twelve-tile leg outlives any sane budget. Worse, a drop of more than
    one z level is a FALL, and every fall is a KNOCKDOWN
    (`Unit.Thread.Movement.Timers`: Collapsed pose plus a self-timed
    get-up) — and an incapacitated endpoint ENDS a Mode A session by
    rule. That is transient enough to be invisible to any poll and
    biased toward long approaches, so it presents as an intermittent
    Mode A bug; it cost two full runs before the pose was caught in the
    act;
  * no unit is ever spawned ON a building's own tile. One that is
    accumulates wounds and is playing `injured_death` a minute later,
    which ends a Mode A session through the incapacitated-source rule
    and reads exactly like a Mode A bug;
  * the camera is settled into the tile zoom band during setup, before
    any session exists. A Mode A arrival snaps the camera, and crossing
    a zoom band is one of `view_teardown`'s triggers — it CLEARS the
    session, so one created on the post-worldgen zoomed-out view tears
    itself down the instant it opens;
  * the never-inspected fixture is retired the moment its observations
    are made. It is the world's only construction site, and
    `build_nearby` scans thirty tiles for one;
  * each Mode B carrier is sent home before its own leg, because the
    previous leg left it standing AT the endpoint it served — and the
    "no adjacency was required" half of a leg has to be a fact about the
    mode rather than about where the unit happened to be.

ISOLATION. The whole run lives on a throwaway resource root: `scripts`,
`assets` and `data` symlinked (read-only content), `config/` COPIED (the
real UI writes settings, and #1266 says a test never touches the
developer's `config/*.local.yaml`), and its own empty `saves/`. Both
processes — the first engine and the fresh-reload one — share that ONE
root and nothing else, so no save and no local configuration can leak
between two consecutive runs.

Stages, in order:

  setup       a fixed-seed world through the real create-world screen,
              the probe's own throwaway defs, and the three endpoint
              classes stocked.
  knowledge   D-2: a never-inspected container reads unknown with its
              capacity still shown and reveals nothing on being opened;
              proximity alone never reveals; contents go GENUINELY stale
              (a wildlife withdrawal mutates storage without revealing,
              because `revealContainerForUnit` is player-gated); and a
              Mode A open is what refreshes them.
  modeB       all six directed legs through the real queued gestures
              (#1249's Store/Retrieve), each committing on arrival.
  modeA       the same six directed legs through three real escort
              sessions (#1250/#1251), committing on the spot.
  batch       D-1: twelve into room for eight stores eight, reports the
              remainder, and no single item half-moves.
  widget      requirement 1d: every container view encountered in this
              run was rendered by the ONE item-list widget, asserted
              from the rendered dumps collected as each view opened.
  save        D-3: a Mode B order left IN FLIGHT and a Mode A session
              left OPEN on a DIFFERENT pair, captured into one save.
  load        a FRESH PROCESS: the order survives with its exact
              identity and a non-terminal state and then completes
              exactly once, while the session is gone and both units it
              held are free.

Known-flaky neighbours: `tools/expedition_retrieval_probe.py` and
`tools/repair_ai_probe.py` are the arc-adjacent AI probes with recorded
intermittent failures; this one is deliberately built to avoid their
failure mode by keeping the simulation PAUSED except across the walks it
actually measures.

Manual-only (needs-gpu) per `tools/ci_probes.py`; the CI-blocking gates
for this feature are the hspec groups `--match "Unit transfer"`,
`"Transfer context menu"`, `"Container knowledge"`, `"Item list widget"`,
`"persistence contract"` and `"save components"`.

Usage: python3 tools/unified_transfer_probe.py
       [--port 9432] [--size 1280x900] [--seed 42] [--world-size 64]
       [--keep-root]

Exit 0 = every check passed.
"""
from __future__ import annotations

import argparse
import json
import os
import re
import shutil
import sys
import tempfile
import time
import traceback

sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
from probelib import (boot, camera_state, capture_request_id, centred_within,
                      clear_find_water, focus_and_locate,
                      locate_building_pixel, pin_camera_to_tile, poll_until,
                      quit_engine, send, send_json, set_paused,
                      targeting_report, viewport, wait_load_published,
                      wait_save_complete, win_to_fb)

REPO = os.path.dirname(os.path.dirname(os.path.abspath(__file__)))

LOG_A = "/tmp/synarchy_unified_transfer_a.log"
LOG_B = "/tmp/synarchy_unified_transfer_b.log"

SLOT = "probe_unified_transfer"

STAGES = ["setup", "knowledge", "modeB", "modeA", "batch", "widget",
          "save", "load"]

# --------------------------------------------------------------------------
# Throwaway content
# --------------------------------------------------------------------------
# ONE def per LEG. A merged row's singular gesture names the row's
# representative, which is its FIRST member — so a source holding two
# instances of the same def would let "Store 1" move an instance this
# probe did not choose, and the leg would still pass. With one def per
# leg the source holds exactly one instance of it, the row is
# single-membered, and the entry the gesture creates can be compared
# against an EXACT instance id. (It also exercises #1249's rule that a
# single-instance row shows the singular entry alone.)
#
# 0.25 kg is deliberately a BINARY fraction, and the partial hold's
# 2.0 kg capacity is another: capacity is a 32-bit Float and the gate is
# `load + weight <= capacity` summed one accepted item at a time, so a
# decimal-friendly fixture lands a few ulps over and fits seven — an
# arithmetic artefact that reads exactly like a partial-batch bug. Eight
# of 0.25 sum to precisely 2.0.
#
# `bulk` is REQUIRED by `Engine.Asset.YamlItems` and one bad definition
# rejects the whole FILE; it is consumed by nothing here (the transfer
# capacity gate is weight-only).
LEG_DEFS_B = [f"probe_ut_b{i}" for i in range(1, 7)]
LEG_DEFS_A = [f"probe_ut_a{i}" for i in range(1, 7)]
DEF_BATCH = "probe_ut_batch"
DEF_STALE = "probe_ut_stale_item"
DEF_SAVE = "probe_ut_save_item"
ALL_ITEM_DEFS = LEG_DEFS_B + LEG_DEFS_A + [DEF_BATCH, DEF_STALE, DEF_SAVE]

ITEMS_YAML = "items:\n" + "".join(f"""\
  - name: "{d}"
    display_name: "Probe UT {d}"
    sprite: "assets/textures/items/material/bar_steel.png"
    weight: 0.25
    bulk: 0.25
    category: Materials
""" for d in ALL_ITEM_DEFS)

DEF_HOLD = "probe_ut_hold"
DEF_PARTIAL = "probe_ut_partial"
DEF_STALE_HOLD = "probe_ut_stale_hold"
DEF_UNSEEN = "probe_ut_unseen"

# `build_work: 0.0` takes the other branch of `Building.Types.currentActivity`
# (and with no `state_animations` block the computed appear duration is 0),
# so a `building.spawn`ed instance reports Built immediately — the shipped
# `cargo_hold_S`'s real 240 s of worker-driven work would leave one stuck
# "appearing" forever with no construct_job AI running.
#
# DEF_UNSEEN is the exception, and deliberately so: worker-built and left
# at zero progress, it never reaches Built, so A3's seed-at-completion
# trigger never fires and its knowledge record genuinely does not exist.
# That is the never-inspected state obtained without calling a single
# knowledge-mutating verb — manufacturing it with one would make this
# probe assert its own writes.
BUILDINGS_YAML = "".join([
    "buildings:\n",
    f"""\
  - name: "{DEF_HOLD}"
    display_name: "Probe UT Hold"
    category: "Test"
    description: "Throwaway #1255 test fixture - not shipped content."
    sprite: "assets/textures/buildings/cargo_hold_S/default.png"
    tile_size: {{ x: 1, y: 1 }}
    placement: "flat_ground"
    race: "acolyte_cult"
    build_work: 0.0
    storage_capacity: 400.0
  - name: "{DEF_PARTIAL}"
    display_name: "Probe UT Partial Hold"
    category: "Test"
    description: "Throwaway #1255 test fixture - not shipped content."
    sprite: "assets/textures/buildings/cargo_hold_S/default.png"
    tile_size: {{ x: 1, y: 1 }}
    placement: "flat_ground"
    race: "acolyte_cult"
    build_work: 0.0
    storage_capacity: 2.0
  - name: "{DEF_STALE_HOLD}"
    display_name: "Probe UT Stale Hold"
    category: "Test"
    description: "Throwaway #1255 test fixture - not shipped content."
    sprite: "assets/textures/buildings/cargo_hold_S/default.png"
    tile_size: {{ x: 1, y: 1 }}
    placement: "flat_ground"
    race: "acolyte_cult"
    build_work: 0.0
    storage_capacity: 200.0
  - name: "{DEF_UNSEEN}"
    display_name: "Probe UT Unseen Hold"
    category: "Test"
    description: "Throwaway #1255 test fixture - not shipped content."
    sprite: "assets/textures/buildings/cargo_hold_S/default.png"
    tile_size: {{ x: 1, y: 1 }}
    placement: "flat_ground"
    race: "acolyte_cult"
    build_work: 240.0
    storage_capacity: 300.0
"""])

CHUNK_TILES = 16
SEARCH_RADIUS = 60

# Debug-console expressions naming the live widget instances this probe
# reads. The container window owns a STACK of levels (#1238), so every
# read goes through the manager's own accessor because a level may not
# exist.
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


class SetupError(RuntimeError):
    """The scenario could not reach the state it tests."""


# --------------------------------------------------------------------------
# Stage-aware check recorder
# --------------------------------------------------------------------------
class Checks:
    """Every check is attributed to one of the eight STAGES, so a failure
    says WHICH part of the system broke instead of only that it did."""

    def __init__(self) -> None:
        self.failed = 0
        self.stage = STAGES[0]
        self.by_stage: dict[str, list[int]] = {s: [0, 0] for s in STAGES}
        self.reached: list[str] = []

    def enter(self, stage: str, title: str) -> None:
        assert stage in STAGES, stage
        self.stage = stage
        if stage not in self.reached:
            self.reached.append(stage)
        print(f"\n=== [{stage}] {title} ===", flush=True)

    def ok(self, cond: bool, label: str, detail: str = "") -> bool:
        cond = bool(cond)
        # Recording against a stage counts as reaching it, so an
        # operational failure raised before its own enter() still reports
        # as FAIL rather than as NOT REACHED.
        if self.stage not in self.reached:
            self.reached.append(self.stage)
        slot = self.by_stage[self.stage]
        slot[0 if cond else 1] += 1
        print(f"  [{'PASS' if cond else 'FAIL'}][{self.stage}] {label}"
              + (f"  ({detail})" if detail and not cond else ""), flush=True)
        if not cond:
            self.failed += 1
        return cond

    def outcomes(self) -> dict[str, str]:
        """Per-stage pass/fail, for the run fingerprint. Deliberately
        outcomes only and no measurements: two runs of the same seed must
        agree on WHAT happened, while a sampled weight or a wall time is
        a measurement and will differ."""
        out = {}
        for stage in STAGES:
            passed, failed = self.by_stage[stage]
            out[stage] = ("not-reached" if stage not in self.reached
                          else ("fail" if failed else "pass"))
        return out

    def report(self) -> None:
        print("\n--- stage summary ---", flush=True)
        broken, failing = [], []
        for stage in STAGES:
            passed, failed = self.by_stage[stage]
            if failed:
                status = f"FAIL ({failed} of {passed + failed} checks)"
                broken.append(stage)
                failing.append(stage)
            elif stage not in self.reached:
                status = "NOT REACHED"
                broken.append(stage)
            else:
                status = f"pass ({passed} checks)"
            print(f"  {stage:<9} {status}", flush=True)
        if broken:
            culprit = failing[0] if failing else broken[0]
            print(f"\n--- FAIL: the unified transfer system broke at stage "
                  f"'{culprit}' (stages affected: {', '.join(broken)}) ---",
                  flush=True)
        else:
            print("\n--- PASS: both modes, all three endpoint classes, both "
                  "directions, batches, knowledge, one widget and "
                  "persistence hold together ---", flush=True)


# --------------------------------------------------------------------------
# Boot / isolation
# --------------------------------------------------------------------------
def make_isolated_root(base: str) -> str:
    """A throwaway resource root: the read-only content families
    symlinked, `config/` COPIED (the real UI flow writes settings and
    must never touch the developer's own), and its OWN empty saves/."""
    root = os.path.join(base, "root")
    os.makedirs(root, exist_ok=True)
    for family in ("scripts", "assets", "data"):
        target = os.path.join(root, family)
        if not os.path.exists(target):
            os.symlink(os.path.join(REPO, family), target)
    config_dst = os.path.join(root, "config")
    if not os.path.exists(config_dst):
        shutil.copytree(os.path.join(REPO, "config"), config_dst)
    os.makedirs(os.path.join(root, "saves"), exist_ok=True)
    return root


def boot_offscreen(root: str, port: int, size: str, log: str, label: str):
    return boot(port, log=log, label=label, ready_timeout=240,
                mode=("--offscreen",),
                args=["--size", size, "--resource-root", root])


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


def warning_texts(port: int, uid: int) -> list:
    """Every `unit_warning` text currently in the log for `uid`, one entry
    per coalesced row repeated by its own `count` — so a re-emission is a
    new element rather than an invisible counter bump."""
    out = []
    for e in event_log(port):
        if e.get("category") == "unit_warning" and e.get("uid") == uid:
            out.extend([e.get("text") or ""] * int(e.get("count", 1) or 1))
    return out


def knowledge(port: int, bid: int) -> dict:
    got = send_json(port, f"return building.getContainerKnowledge({bid})")
    return got if isinstance(got, dict) else {}


def live_storage_ids(port: int, bid: int, def_name: str) -> list:
    got = send_json(port, f"return building.getStorage({bid})")
    if not isinstance(got, list):
        return []
    return sorted(int(it["instanceId"]) for it in got
                  if isinstance(it, dict) and it.get("defName") == def_name
                  and isinstance(it.get("instanceId"), (int, float)))


def remembered_ids(port: int, bid: int, def_name: str) -> list:
    return sorted(int(it["instanceId"])
                  for it in (knowledge(port, bid).get("items") or [])
                  if isinstance(it, dict) and it.get("defName") == def_name
                  and isinstance(it.get("instanceId"), (int, float)))


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


# --------------------------------------------------------------------------
# Requirement 1d: one widget renders every container view
# --------------------------------------------------------------------------
class ViewLedger:
    """Evidence, collected as each container view is opened, that the ONE
    item-list widget rendered it.

    Collected LIVE rather than replayed at the end: a list instance only
    exists while its view is open, so the rendered dump is the only
    moment the question can be asked at all. Two independent facts per
    view - the list id names a live instance in the widget's OWN
    registry (`scripts/ui/item_list.getModel`, which answers nil for
    an id it does not own), and every row the widget oracle reports
    for it carries `type = "item_list"` - because a view with no
    rows can only supply the first, and a view whose rows came from
    somewhere else would still have a registered instance."""

    def __init__(self) -> None:
        self.views: dict[str, dict] = {}

    def record(self, port: int, label: str, list_id_lua: str) -> dict:
        list_id = resolve_list_id(port, list_id_lua)
        registered = False
        if list_id is not None:
            # getModel, not getTabs: getTabs answers `{}` for an id it
            # does not own, and an empty table is TRUTHY in Lua, so it
            # cannot tell a live instance from a missing one.
            got = send(port, "local il = require('scripts.ui.item_list');"
                             f" return tostring(il.getModel({list_id_lua})"
                             " ~= nil)").strip().strip('"')
            registered = got == "true"
        rows = item_rows(port, list_id_lua) if list_id is not None else []
        entry = {
            "listId": list_id,
            "registered": registered,
            "rows": len(rows),
            "allItemList": all(r.get("type") == "item_list" for r in rows),
        }
        self.views[label] = entry
        return entry


# --------------------------------------------------------------------------
# The real create-world screen, pinned to a fixed seed
# --------------------------------------------------------------------------
def reach_main_menu(chk: Checks, port: int) -> bool:
    return chk.ok(bool(poll_until(90.0, lambda: find_widget(port, "Create World"))),
                  "the loading screen reaches the main menu")


def create_world(chk: Checks, port: int, seed: int, world_size: int,
                 plates: int) -> bool:
    """Generate the session's world through the REAL create-world screen,
    with the generation parameters pinned.

    The seed and size are written into `createWorldMenu.pending`, which
    is exactly what `create_world/generation.lua` reads: the settings
    tab's own randbox and dropdown write there through their `onChange`,
    and nothing between here and Generate overwrites either field (only
    the advanced/general/timeline tabs' `getWidgetValues` feed back into
    `pending`, and none of them owns the seed or the size). Without this
    the seed is a fresh random roll per run and no fingerprint could be
    compared across two of them."""
    click_widget_center(port, find_widget(port, "Create World"))
    if not chk.ok(bool(poll_until(30.0,
                                  lambda: find_widget(port, "Generate World"))),
                  "the create-world screen is reached"):
        return False
    send(port, "local m = require('scripts.create_world_menu');"
               f" m.pending.seed = string.format('%08X', {seed});"
               f" m.pending.worldSize = '{world_size}';"
               f" m.pending.plateCount = '{plates}';"
               " return 'ok'")
    pinned = send_json(port, "local m = require('scripts.create_world_menu');"
                             " return {seed = m.pending.seed,"
                             " size = m.pending.worldSize}")
    if not chk.ok(isinstance(pinned, dict)
                  and pinned.get("seed") == "%08X" % seed
                  and pinned.get("size") == str(world_size),
                  f"the create-world screen is pinned to seed 0x{seed:08X} at "
                  f"world size {world_size}", f"got {pinned!r}"):
        return False
    click_widget_center(port, find_widget(port, "Generate World"))
    print("  (generating world, ~1-2 min)", flush=True)
    # `world.getInitProgress()` returns FOUR values (phase, chunks done,
    # chunks total, label); binding one local takes the phase alone,
    # which is what this is asking about. Returning the call directly
    # would send back the whole tuple and never compare equal.
    done = poll_until(420.0,
                      lambda: send(port, "local p = world.getInitProgress();"
                                         " return p", timeout=5.0).strip() == "3",
                      interval=2.0)
    if not chk.ok(bool(done), "worldgen completes (phase 3)"):
        return False
    if not chk.ok(bool(poll_until(90.0, lambda: find_widget(port, "Continue"))),
                  "the post-generation Continue button appears"):
        return False
    click_widget_center(port, find_widget(port, "Continue"))
    if not chk.ok(bool(poll_until(90.0,
                                  lambda: not find_widget(port, "Continue"))),
                  "the in-game HUD is reached"):
        return False
    time.sleep(2.0)
    got = send(port, "return tostring(world.getSeed())").strip().strip('"')
    chk.ok(got == str(seed),
           f"the generated world really carries the pinned seed {seed}",
           f"world.getSeed() = {got!r}")
    return True


def load_fixtures(chk: Checks, port: int, base: str) -> bool:
    """Register the probe's own throwaway defs. Written OUTSIDE the
    checkout (into this run's temp dir) so nothing under `data/` is ever
    touched, and loaded by absolute path."""
    items = os.path.join(base, "unified_transfer_items.yaml")
    builds = os.path.join(base, "unified_transfer_buildings.yaml")
    with open(items, "w", encoding="utf-8") as f:
        f.write(ITEMS_YAML)
    with open(builds, "w", encoding="utf-8") as f:
        f.write(BUILDINGS_YAML)
    ni = send(port, f"return engine.loadItemYaml('{items}')").strip()
    nb = send(port, f"return engine.loadBuildingYaml('{builds}')").strip()
    # A rejected file is reported as a plain 0 (the loader hands back an
    # empty list after a parse failure), so an exact count is what tells
    # "registered" from "silently registered nothing".
    okay = chk.ok(_num(ni) == float(len(ALL_ITEM_DEFS)),
                  f"the probe's {len(ALL_ITEM_DEFS)} throwaway item defs "
                  f"registered", f"engine.loadItemYaml returned {ni!r}")
    okay = chk.ok(_num(nb) == 4.0,
                  "the probe's four throwaway building defs registered",
                  f"engine.loadBuildingYaml returned {nb!r}") and okay
    return okay


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


def open_window_by_right_click(chk: Checks, port: int, ep, vp: dict,
                               label: str) -> bool:
    """The REAL route: right-click the world entity (located and
    confirmed by the production hit test) and activate the rendered
    "Contents" row located through `ui.dumpWidgets()`."""
    kind = "building" if ep[0] == "building" else "unit"
    pixel = focus_entity(chk, port, kind, ep[1], vp, label)
    if pixel is None:
        return False
    right_click_pixel(port, pixel, vp)
    labels = menu_labels(port)
    entry = find_widget(port, "Contents")
    if not chk.ok(bool(entry),
                  f"{label}: the right-click menu offers 'Contents'",
                  f"menu labels: {labels!r}"):
        close_menu(port)
        return False
    click_widget_center(port, entry)
    time.sleep(0.8)
    lvl = send_json(port, f"return {WINDOW}.getLevel(1)")
    src = (lvl or {}).get("src") if isinstance(lvl, dict) else None
    return chk.ok(isinstance(src, dict) and src.get("endpointKind") == ep[0]
                  and src.get("id") == ep[1],
                  f"{label}: the activated row opened the window on the exact "
                  f"endpoint identity {ep[0]} #{ep[1]}", f"got {src!r}")


# --------------------------------------------------------------------------
# One directed leg, through Mode B's queued gesture
# --------------------------------------------------------------------------
def seed_source(port: int, src, def_name: str, stager: int) -> None:
    if src[0] == "unit":
        add_one(port, src[1], def_name)
    else:
        stage_into_hold(port, stager, src[1], def_name)


def send_home(port: int, uid: int, home, away_from, gap: int = 3,
              seconds: float = 120.0) -> None:
    """Walk `uid` back to its own anchor until it is clear of
    `away_from`.

    Every leg leaves its carrier standing AT the endpoint it just
    served, so without this the next leg would fire its gesture from
    adjacent — and the "Mode B needed no adjacency" half of the leg
    would be asserting something that happened to be false rather than
    something the mode guarantees. Sending it home also gives each leg a
    real trip to make, which is what the arrival commit is."""
    if home is None:
        return
    send(port, f"unit.stop({uid}); require('scripts.unit_ai').commandMove("
               f"{uid}, {home[0]}, {home[1]}); return 'ok'")
    set_paused(port, False)
    poll_until(seconds,
               lambda: (footprint_gap(port, ("unit", uid), away_from) or 0)
               >= gap, interval=0.5)
    # Leave it IDLE where it stands, so nothing it was told to do
    # earlier is still in flight when the next gesture is made — the
    # same precondition `tools/item_list_widget_probe.py`'s
    # `spawn_pair_apart` establishes before creating a session, and for
    # the same reason: an action that is deliberately not forceExecute
    # runs only on a switch or on an idle unit, so a carrier caught
    # mid-path is a different starting state from the one a player ever
    # produces.
    send(port, f"unit.stop({uid}); return 'ok'")
    set_paused(port, True)
    time.sleep(0.4)


def mode_b_leg(chk: Checks, port: int, ledger: ViewLedger, fp: dict, key: str,
               src, dst, def_name: str, verb: str, stager: int,
               home=None) -> bool:
    """One directed leg through #1249's queued gesture.

    Store fires from the SOURCE unit's own info row into the open
    window's active endpoint; Retrieve fires from a window row into the
    unit the shared selection rule resolves. Which unit walks is the
    other one each time, and it is the order — not the click — that
    moves anything."""
    label = f"modeB {key}"
    display = f"Probe UT {def_name}"
    # The acting unit, the endpoint the window opens on, and the row's
    # host are all decided by the VERB, never by comparing an executor's
    # uid to an endpoint id: unit ids and building ids are separate
    # spaces, so `acolyte 1` and `hold 1` compare equal.
    executor = src[1] if verb == "Store" else dst[1]
    window_ep = dst if verb == "Store" else src
    rows_id = UNIT_INV_LIST_ID if verb == "Store" else CARGO_LIST_ID

    set_paused(port, True)
    # The carrier is about to make two real trips; a starving one makes
    # neither.
    calm(port, executor)
    # `window_ep` is also the endpoint the carrier is NOT, so it is what
    # it has to start away from.
    send_home(port, executor, home, window_ep)
    seed_source(port, src, def_name, stager)
    ids = ep_ids(port, src, def_name)
    if not chk.ok(len(ids) == 1,
                  f"{label}: the source holds exactly one {def_name}, so the "
                  f"row is single-membered and its gesture names an EXACT "
                  f"instance", f"got {ids!r}"):
        return False
    iid = ids[0]
    if not chk.ok(iid not in ep_ids(port, dst, def_name),
                  f"{label}: the destination does not already hold instance "
                  f"{iid}"):
        return False

    # The acting unit IS the selection: Store reads the row off the
    # selected unit's own info panel, and Retrieve resolves its
    # executor out of the selection.
    send(port, "unit.deselectAll(); return 'ok'")
    send(port, f"return unit.select({executor})")
    time.sleep(0.7)
    if not open_window(chk, port, window_ep, label):
        return False
    ledger.record(port, f"container window on a {window_ep[0]} endpoint",
                  CARGO_LIST_ID)
    if verb == "Store":
        ledger.record(port, "unit-info inventory section", UNIT_INV_LIST_ID)

    gap = footprint_gap(port, src, dst)
    chk.ok(gap is not None and gap > 1,
           f"{label}: the two endpoints are OUT of the contract's reach when "
           f"the gesture fires (footprint Chebyshev {gap}) — Mode B requires "
           f"no adjacency, which is the whole promotion", f"gap={gap!r}")

    row = find_row(port, rows_id, def_name)
    if not chk.ok(bool(row),
                  f"{label}: the {def_name} row is located on the rendered "
                  f"{'unit-info inventory' if verb == 'Store' else 'container window'}"):
        close_window(port)
        return False
    chk.ok(row.get("instanceIds") == [iid],
           f"{label}: the rendered row stands for exactly instance {iid}",
           f"got {row.get('instanceIds')!r}")

    before_ids = {o.get("id") for o in orders(port, executor)}
    before_ev = event_total(port, executor, "unit_event", display)
    before_check = check_transfer(port, src, dst, def_name, [iid])
    right_click_widget_center(port, row)
    time.sleep(0.5)
    labels = menu_labels(port)
    chk.ok(f"{verb} 1" in labels,
           f"{label}: the rendered row menu offers '{verb} 1'",
           f"menu labels: {labels!r}")
    chk.ok(f"{verb} all" not in labels,
           f"{label}: a single-instance row shows the singular entry alone",
           f"menu labels: {labels!r}")
    entry = find_widget(port, f"{verb} 1")
    if not chk.ok(bool(entry), f"{label}: the '{verb} 1' entry is clickable"):
        close_menu(port)
        close_window(port)
        return False
    click_widget_center(port, entry)
    time.sleep(0.9)

    new = [o for o in orders(port, executor) if o.get("id") not in before_ids]
    if not chk.ok(len(new) == 1,
                  f"{label}: firing '{verb} 1' queues exactly one durable "
                  f"transfer order", f"got {[order_identity(o) for o in new]!r}"):
        close_window(port)
        return False
    ident = order_identity(new[0])
    fp.setdefault("orders", {})[f"modeB:{key}"] = ident
    chk.ok(ident["source"] == f"{src[0]}:{src[1]}"
           and ident["destination"] == f"{dst[0]}:{dst[1]}"
           and ident["items"] == [iid],
           f"{label}: the queued order runs from {src[0]}:{src[1]} to "
           f"{dst[0]}:{dst[1]} naming exactly instance {iid}",
           f"got {ident!r}")
    chk.ok(iid in ep_ids(port, src, def_name),
           f"{label}: nothing moved at click time — the source still owns "
           f"instance {iid} while the carrier has yet to walk")
    close_window(port)

    # -- the walk, and the commit that arrival IS.
    # The order's own ENTRY states are the structured record of a Mode B
    # commit, and #1253 PRUNES a terminal order on the tick that ends it
    # — so the last states observed while it still existed are the only
    # place that record can be read, and the poll captures them as it
    # goes rather than looking after the fact.
    seen_states: list = []

    def landed_and_recorded():
        for o in orders(port, executor):
            if o.get("id") == ident["id"]:
                states = entry_states(o)
                if states and (not seen_states or seen_states[-1] != states):
                    seen_states.append(states)
        return iid in ep_ids(port, dst, def_name)

    set_paused(port, False)
    landed = poll_until(300.0, landed_and_recorded, interval=1.0)
    set_paused(port, True)
    time.sleep(0.4)
    if not chk.ok(bool(landed),
                  f"{label}: the carrier walks the order and commits on "
                  f"arrival — instance {iid} reaches {dst[0]}:{dst[1]}",
                  f"carrier {executor} running {ai_action(port, executor)!r} "
                  f"at footprint Chebyshev "
                  f"{footprint_gap(port, ('unit', executor), dst)!r} from the "
                  f"destination, orders "
                  f"{[order_identity(o) for o in orders(port, executor)]!r}"):
        return False
    chk.ok(iid not in ep_ids(port, src, def_name),
           f"{label}: and left the source — the instance is in exactly one "
           f"endpoint, never both")
    chk.ok(not any(o.get("id") == ident["id"]
                   for o in orders(port, executor)),
           f"{label}: the completed order was pruned on the tick that ended it")
    chk.ok(event_total(port, executor, "unit_event", display) - before_ev == 1,
           f"{label}: the completion reached the player exactly once",
           f"delta {event_total(port, executor, 'unit_event', display) - before_ev}")
    chk.ok(bool(seen_states) and all(st == ["queued"] or st == ["in_transit"]
                                     or st == ["ready_to_commit"]
                                     or st == ["completed"]
                                     for st in seen_states)
           and seen_states[0] in (["queued"], ["in_transit"]),
           f"{label}: the order's own entry ran the lifecycle and never "
           f"reported a failure state — observed {seen_states!r}",
           f"got {seen_states!r}")
    assert_structured_move(chk, port, label, src, dst, def_name, [iid],
                           before_check, deferred_reach=True)
    fp.setdefault("legs", {})[f"modeB:{key}"] = iid
    return True


# --------------------------------------------------------------------------
# Mode A: a real escort session, and the legs committed inside it
# --------------------------------------------------------------------------
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


def mode_a_leg(chk: Checks, port: int, fp: dict, key: str, src, dst,
               def_name: str, verb: str, pane: str, iid: int,
               held_uid: int) -> bool:
    """One directed leg inside an open session: a real right-click on the
    real rendered pane row, then the located entry. The direction comes
    from WHICH pane was clicked, so a swapped pair is unrepresentable."""
    label = f"modeA {key}"
    display = f"Probe UT {def_name}"
    other = "Retrieve" if verb == "Store" else "Store"
    rows_id = pane_list_id(pane)
    row = find_row(port, rows_id, def_name)
    if not chk.ok(bool(row),
                  f"{label}: the {def_name} row is located on the rendered "
                  f"{pane} pane"):
        return False
    chk.ok(row.get("instanceIds") == [iid],
           f"{label}: the rendered row stands for exactly instance {iid}",
           f"got {row.get('instanceIds')!r}")

    before_ev = event_total(port, held_uid, "unit_event", display)
    before_check = check_transfer(port, src, dst, def_name, [iid])
    right_click_widget_center(port, row)
    time.sleep(0.5)
    labels = menu_labels(port)
    chk.ok(f"{verb} 1" in labels,
           f"{label}: the {pane} pane's row menu offers '{verb} 1'",
           f"menu labels: {labels!r}")
    chk.ok(not any((l or "").startswith(other) for l in labels),
           f"{label}: and offers no '{other}' — direction comes from WHICH "
           f"pane was clicked", f"menu labels: {labels!r}")
    entry = find_widget(port, f"{verb} 1")
    if not chk.ok(bool(entry), f"{label}: the '{verb} 1' entry is clickable"):
        close_menu(port)
        return False
    click_widget_center(port, entry)
    time.sleep(1.0)

    chk.ok(iid in ep_ids(port, dst, def_name),
           f"{label}: the gesture commits ON THE SPOT — instance {iid} is at "
           f"{dst[0]}:{dst[1]} immediately, with no order and no walk",
           f"destination now holds {ep_ids(port, dst, def_name)!r}")
    chk.ok(iid not in ep_ids(port, src, def_name),
           f"{label}: and left {src[0]}:{src[1]} — the instance is in exactly "
           f"one endpoint")
    chk.ok(event_total(port, held_uid, "unit_event", display) - before_ev == 1,
           f"{label}: the commit reached the player exactly once")
    chk.ok(session_phase(port) == "open",
           f"{label}: the session stays open and repeatable after a commit",
           f"phase {session_phase(port)!r}")
    assert_structured_move(chk, port, label, src, dst, def_name, [iid],
                           before_check, deferred_reach=False)
    fp.setdefault("legs", {})[f"modeA:{key}"] = iid
    return True


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


def accepts_movement(port: int, uid: int, window: float = 10.0) -> bool:
    """Does `uid` take an ordinary move order again?

    Answered by watching the real AI either SELECT the order or act on
    it. Selecting counts, and that is the point: the hold is a utility,
    so "released" means the ordinary ladder decides again, and
    `follow_command` becoming this unit's current action IS the ladder
    taking the player's order. Whether the unit then covers ground is
    generated terrain's business. `unit.stop` first, because a released
    unit still carries whatever the hold preempted and the action would
    not SWITCH when the new order arrives."""
    info = send_json(port, f"return unit.getInfo({uid})")
    if not isinstance(info, dict):
        return False
    ax, ay = int(info.get("gridX", 0)), int(info.get("gridY", 0))
    before = (float(info.get("gridX", 0)), float(info.get("gridY", 0)))
    ai = "require('scripts.unit_ai')"
    for dx, dy in ((2, 0), (0, 2), (-2, 0), (0, -2), (4, 0), (0, 4)):
        tx, ty = ax + dx, ay + dy
        surface = tile_surface(port, tx, ty)
        if surface is None or not surface[1]:
            continue
        send(port, f"unit.stop({uid}); {ai}.commandMove({uid}, {tx}, {ty});"
                   " return 'ok'")

        def moved_or_took():
            got = send_json(port, f"return unit.getInfo({uid})")
            if isinstance(got, dict):
                now = (float(got.get("gridX", 0)), float(got.get("gridY", 0)))
                if abs(now[0] - before[0]) + abs(now[1] - before[1]) > 0.5:
                    return True
            return ai_action(port, uid) == "follow_command"

        if poll_until(window, moved_or_took, interval=0.4):
            return True
    return False


def park_beside(port: int, uid: int, ep, seconds: float = 45.0) -> bool:
    """Walk `uid` to within the contract's own reach of `ep` and answer
    whether it got there.

    Spawn position is not enough on its own: a fixture spawned beside a
    container is free to wander off during the console round trips that
    follow, and the proximity case below has to be measured on a unit
    that really is standing there."""
    rect = ep_rect(port, ep)
    if rect is None:
        return False
    spot = adjacent_dry(port, rect[0], rect[1])
    if spot is None:
        return False
    send(port, f"unit.stop({uid}); require('scripts.unit_ai').commandMove("
               f"{uid}, {spot[0]}, {spot[1]}); return 'ok'")
    set_paused(port, False)
    got = poll_until(seconds,
                     lambda: (footprint_gap(port, ("unit", uid), ep) or 9) <= 1,
                     interval=0.5)
    set_paused(port, True)
    time.sleep(0.4)
    return got is not None


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


def adjacent_dry(port: int, gx: int, gy: int):
    """The first dry tile beside `(gx, gy)`, or None."""
    tiles = adjacent_dry_tiles(port, gx, gy)
    return tiles[0] if tiles else None


# --------------------------------------------------------------------------
# Stages
# --------------------------------------------------------------------------
def stage_knowledge(chk: Checks, port: int, ledger: ViewLedger, ids: dict,
                    fp: dict, vp: dict) -> None:
    """D-2: contents are genuinely stale, and only an INTERACTION
    refreshes them."""
    unseen, stale = ids["unseen"], ids["stale"]
    set_paused(port, True)

    # -- never inspected.
    k = knowledge(port, unseen)
    if not chk.ok(k.get("state") == "unknown",
                  "a container nothing has ever interacted with reads as "
                  "genuinely never-inspected", f"got {k!r}"):
        return
    cap = k.get("capacity")
    chk.ok(isinstance(cap, (int, float)) and cap > 0,
           "its capacity is still LIVE and positive even with no record at all",
           f"got {cap!r}")
    if open_window(chk, port, ("building", unseen), "knowledge"):
        ledger.record(port, "never-inspected container window", CARGO_LIST_ID)
        sub = send_json(port, "local s = require('scripts.cargo_inventory_panel')"
                              ".getLevel(1) or {}; local l ="
                              " require('scripts.ui.label');"
                              " return {subtitle = s.subtitleId and"
                              " l.getText(s.subtitleId)}")
        want = "Storage: unknown / %.2f kg" % float(cap)
        chk.ok(isinstance(sub, dict) and sub.get("subtitle") == want,
               "its header reads the stored weight as UNKNOWN while still "
               "showing the live capacity",
               f"got {(sub or {}).get('subtitle')!r} want {want!r}")
        chk.ok(not item_rows(port, CARGO_LIST_ID),
               "a never-inspected container renders no rows — it is not an "
               "empty one")
        time.sleep(1.5)
        for t in tab_boxes(port, CARGO_LIST_ID):
            click_widget_center(port, t)
            time.sleep(0.3)
        after = knowledge(port, unseen)
        chk.ok(after.get("state") == "unknown"
               and after.get("revealedAt") is None,
               "opening the window and interacting with it reveals NOTHING",
               f"got {after!r}")
        close_window(port)
    # Every observation this fixture exists for has now been made, and it
    # is the world's only construction site: `build_nearby` scans 30
    # tiles for an Appearing building with work left, so leaving it
    # standing would pull acolytes away from the very walks the rest of
    # this run measures — and would eventually finish it, which SEEDS a
    # knowledge record and destroys the state it was built to supply.
    send(port, f"return building.destroy({unseen})")
    time.sleep(0.5)
    chk.ok(send(port, f"return building.getActivity({unseen})"
                ).strip().strip('"') in ("", "nil", "null"),
           "the never-inspected fixture is retired once it has been observed, "
           "so no construction site competes with the escorts below",
           f"activity {send(port, f'return building.getActivity({unseen})')!r}")

    # -- a known container, and what does NOT refresh it.
    k = knowledge(port, stale)
    r0 = k.get("revealedAt")
    stocked = remembered_ids(port, stale, DEF_STALE)
    if not chk.ok(k.get("state") == "known" and isinstance(r0, (int, float))
                  and len(stocked) == 2,
                  "the stocked container is remembered with both instances and "
                  "an observation time", f"got {k!r}"):
        return

    watcher = ids["watcher"]
    parked = park_beside(port, watcher, ("building", stale))
    gap0 = footprint_gap(port, ("unit", watcher), ("building", stale))
    if chk.ok(bool(parked) and gap0 is not None and gap0 <= 1,
              f"a player unit is standing right beside it (footprint "
              f"Chebyshev {gap0})", f"gap={gap0!r}"):
        set_paused(port, False)
        time.sleep(8.0)
        set_paused(port, True)
        time.sleep(0.4)
        gap1 = footprint_gap(port, ("unit", watcher), ("building", stale))
        chk.ok(gap1 is not None and gap1 <= 3,
               f"and stayed in its immediate vicinity for the whole interval "
               f"(footprint Chebyshev {gap0} -> {gap1})", f"gap={gap1!r}")
        chk.ok(knowledge(port, stale).get("revealedAt") == r0
               and remembered_ids(port, stale, DEF_STALE) == stocked,
               "PROXIMITY ALONE never reveals: eight seconds of a player unit "
               "standing beside it changes neither the observation time nor "
               "the remembered contents",
               f"got {knowledge(port, stale)!r}")

    # -- and how the record goes genuinely stale. A non-player unit's
    #    withdrawal mutates storage without revealing, because every
    #    unit-driven reveal is gated on isPlayerCommandable — so this is
    #    a real divergence rather than one manufactured by a test verb.
    took = send(port, f"return unit.withdrawFromCargo({ids['wildlife']},"
                      f" {stale}, '{DEF_STALE}')").strip().strip('"')
    chk.ok(took == "true", "a non-player unit withdraws one instance",
           f"got {took!r}")
    live = live_storage_ids(port, stale, DEF_STALE)
    remembered = remembered_ids(port, stale, DEF_STALE)
    chk.ok(len(live) == 1 and remembered == stocked
           and knowledge(port, stale).get("revealedAt") == r0,
           "the player's picture is now GENUINELY STALE: storage holds one "
           "instance while the record still remembers both, unchanged",
           f"live={live!r} remembered={remembered!r}")

    # -- the Mode A open is the interaction that refreshes it.
    reveal_uid = ids["reveal"]
    calm(port, reveal_uid)
    escort_info = send_json(port, f"return unit.getInfo({reveal_uid})")
    anim = (escort_info or {}).get("currentAnim") if isinstance(
        escort_info, dict) else None
    if not chk.ok(isinstance(escort_info, dict)
                  and not str(anim or "").startswith("injured_"),
                  "the escort is alive and unhurt before the session that "
                  "measures the reveal — an incapacitated source ends a "
                  "session by rule, which would read as a reveal bug",
                  f"got {escort_info!r}"):
        return
    if not create_session(chk, port, reveal_uid, ("building", stale), vp,
                          "knowledge Mode A reveal", via_menu=False):
        return
    if not await_session_open(chk, port, reveal_uid, "knowledge Mode A reveal",
                              seconds=60.0):
        send(port, "require('scripts.transfer_session').clear(); return 'ok'")
        return
    k2 = knowledge(port, stale)
    chk.ok(isinstance(k2.get("revealedAt"), (int, float))
           and k2["revealedAt"] > float(r0),
           "a Mode A session OPENING on the container is what refreshes it — "
           "a fresh observation time", f"got {k2!r} (was {r0!r})")
    chk.ok(remembered_ids(port, stale, DEF_STALE) == live,
           "and the refreshed record now agrees with live storage, the "
           "withdrawn instance gone",
           f"remembered={remembered_ids(port, stale, DEF_STALE)!r} live={live!r}")
    fp["knowledge"] = {"stocked": stocked, "afterWithdraw": live}
    close_session(chk, port, [reveal_uid], "knowledge Mode A reveal")


def stage_mode_b(chk: Checks, port: int, ledger: ViewLedger, ids: dict,
                 fp: dict, vp: dict) -> None:
    """All six directed legs through #1249's queued gestures."""
    acolyte, mule, hold = ids["acolyte"], ids["technomule"], ids["hold"]
    U_A, U_M, B_H = ("unit", acolyte), ("unit", mule), ("building", hold)
    # Targeting is measured against where things ARE, so the world stops
    # while the camera is placed and the hit test is asked.
    set_paused(port, True)

    # The REAL right-click route, once: locate and confirm the building
    # through the production hit test, then activate the rendered
    # "Contents" row. Every later open calls the same entry point that
    # row's callback calls.
    #
    # Deliberately building-only. A UNIT endpoint's container window has
    # no world right-click route at all — `scripts/init_context_menu.lua`'s
    # unit menu offers Info / Attack / Transfer / Cancel transfer and no
    # "Contents" — so the window manager's own entry point IS the route
    # for one, and asserting a menu row that does not exist would be
    # asserting a feature this arc did not ship.
    open_window_by_right_click(chk, port, B_H, vp, "modeB building route")
    close_window(port)

    before = knowledge(port, hold).get("revealedAt")
    legs = [
        ("acolyte->storage", U_A, B_H, LEG_DEFS_B[0], "Store"),
        ("storage->acolyte", B_H, U_A, LEG_DEFS_B[1], "Retrieve"),
        ("technomule->storage", U_M, B_H, LEG_DEFS_B[2], "Store"),
        ("storage->technomule", B_H, U_M, LEG_DEFS_B[3], "Retrieve"),
        ("acolyte->technomule", U_A, U_M, LEG_DEFS_B[4], "Store"),
        ("technomule->acolyte", U_M, U_A, LEG_DEFS_B[5], "Retrieve"),
    ]
    homes = ids.get("homes") or {}
    for key, src, dst, def_name, verb in legs:
        executor = src[1] if verb == "Store" else dst[1]
        mode_b_leg(chk, port, ledger, fp, key, src, dst, def_name, verb,
                   acolyte, home=homes.get(executor))
    after = knowledge(port, hold).get("revealedAt")
    chk.ok(isinstance(after, (int, float)) and isinstance(before, (int, float))
           and after > before,
           "a COMPLETED movement refreshes the container's record (D-2's "
           "other reveal trigger)", f"{before!r} -> {after!r}")


def stage_mode_a(chk: Checks, port: int, ledger: ViewLedger, ids: dict,
                 fp: dict, vp: dict) -> None:
    """The same six directed legs through three real escort sessions."""
    acolyte, mule, hold = ids["acolyte"], ids["technomule"], ids["hold"]
    U_A, U_M, B_H = ("unit", acolyte), ("unit", mule), ("building", hold)

    sessions = [
        # (key, escort uid, destination endpoint, via the real menu?,
        #  walk away first?, legs)
        ("acolyte<->storage", acolyte, B_H, True, True,
         [("acolyte->storage", U_A, B_H, LEG_DEFS_A[0], "Store", "source"),
          ("storage->acolyte", B_H, U_A, LEG_DEFS_A[1], "Retrieve",
           "destination")]),
        ("technomule<->storage", mule, B_H, False, False,
         [("technomule->storage", U_M, B_H, LEG_DEFS_A[2], "Store", "source"),
          ("storage->technomule", B_H, U_M, LEG_DEFS_A[3], "Retrieve",
           "destination")]),
        ("acolyte<->technomule", acolyte, U_M, True, False,
         [("acolyte->technomule", U_A, U_M, LEG_DEFS_A[4], "Store", "source"),
          ("technomule->acolyte", U_M, U_A, LEG_DEFS_A[5], "Retrieve",
           "destination")]),
    ]
    for key, escort, dst_ep, via_menu, walk_away, legs in sessions:
        label = f"modeA {key}"
        set_paused(port, True)
        calm(port, escort)
        if dst_ep[0] == "unit":
            calm(port, dst_ep[1])
            # Get the destination unit off the storage building's own
            # tiles first. The right-click router tries the BUILDING menu
            # before the unit menu, so a unit standing on a hold hands
            # every right-click to the hold — and the previous session
            # left it standing on exactly that.
            send_home(port, dst_ep[1], (ids.get("homes") or {}).get(dst_ep[1]),
                      B_H, gap=3)
        seeded = {}
        for leg_key, src, dst, def_name, verb, pane in legs:
            seed_source(port, src, def_name, acolyte)
            got = ep_ids(port, src, def_name)
            if not chk.ok(len(got) == 1,
                          f"{label}: {src[0]}:{src[1]} holds exactly one "
                          f"{def_name} for the {leg_key} leg", f"got {got!r}"):
                seeded = {}
                break
            seeded[leg_key] = got[0]
        if not seeded:
            continue

        if walk_away:
            # Walk FIRST is the whole of Mode A, so at least one session
            # has to start out of reach and get there under its own AI.
            send_home(port, escort, (ids.get("homes") or {}).get(escort),
                      dst_ep, gap=2, seconds=90.0)
            gap = footprint_gap(port, ("unit", escort), dst_ep)
            chk.ok(gap is not None and gap > 1,
                   f"{label}: the escort starts OUT of the contract's reach "
                   f"(footprint Chebyshev {gap}), so the session has a real "
                   f"walk to make", f"gap={gap!r}")

        if not create_session(chk, port, escort, dst_ep, vp, label, via_menu):
            send(port, "require('scripts.transfer_session').clear(); return 'ok'")
            continue
        if not await_session_open(chk, port, escort, label):
            send(port, "require('scripts.transfer_session').clear(); return 'ok'")
            continue

        d = stack_dump(port)
        lv = (d.get("levels") or [{}])[0]
        chk.ok(d.get("depth") == 1 and lv.get("kind") == "escort"
               and lv.get("paneCount") == 2,
               f"{label}: the pair is ONE stack level of the escort kind "
               f"owning two panes", f"got {d!r}")
        ledger.record(port, "escort source pane", pane_list_id("source"))
        ledger.record(port, "escort destination pane",
                      pane_list_id("destination"))

        held = [escort]
        if dst_ep[0] == "unit":
            held.append(dst_ep[1])
            roles = {uid: send(port, "return tostring(require("
                                     "'scripts.transfer_session')"
                                     f".roleOf({uid}))").strip().strip('"')
                     for uid in held}
            chk.ok(roles.get(escort) == "source"
                   and roles.get(dst_ep[1]) == "target",
                   f"{label}: unit-to-unit holds BOTH ends — the escort as "
                   f"'source', the destination as 'target'", f"got {roles!r}")

        for leg_key, src, dst, def_name, verb, pane in legs:
            mode_a_leg(chk, port, fp, leg_key, src, dst, def_name, verb, pane,
                       seeded[leg_key], escort)
        close_session(chk, port, held, label)


def stage_batch(chk: Checks, port: int, ids: dict, fp: dict, vp: dict) -> None:
    """D-1: twelve into room for eight stores eight and reports the rest,
    and no single item ever half-moves."""
    acolyte, partial = ids["acolyte"], ids["partial"]
    U_A, B_P = ("unit", acolyte), ("building", partial)
    label = "batch"
    set_paused(port, True)
    calm(port, acolyte)
    before_ids = set(ep_ids(port, U_A, DEF_BATCH))
    for _ in range(12):
        add_one(port, acolyte, DEF_BATCH)
    minted = [i for i in ep_ids(port, U_A, DEF_BATCH)
              if i not in before_ids]
    if not chk.ok(len(minted) == 12 and not before_ids,
                  f"{label}: the acolyte carries exactly twelve {DEF_BATCH} "
                  f"instances and nothing else of that def", f"got {minted!r}"):
        return
    if not chk.ok(not ep_ids(port, B_P, DEF_BATCH),
                  f"{label}: the small hold starts empty of them"):
        return

    if not create_session(chk, port, acolyte, B_P, vp, label, via_menu=False):
        return
    if not await_session_open(chk, port, acolyte, label):
        send(port, "require('scripts.transfer_session').clear(); return 'ok'")
        return

    row = find_row(port, pane_list_id("source"), DEF_BATCH)
    if not chk.ok(bool(row), f"{label}: the merged twelve-instance row is "
                             f"located on the source pane"):
        close_session(chk, port, [acolyte], label)
        return
    chk.ok(sorted(row.get("instanceIds") or []) == sorted(minted),
           f"{label}: the merged row stands for all twelve exact instances",
           f"got {row.get('instanceIds')!r}")
    before_warns = warning_texts(port, acolyte)
    before_check = check_transfer(port, U_A, B_P, DEF_BATCH, minted)
    right_click_widget_center(port, row)
    time.sleep(0.5)
    labels = menu_labels(port)
    chk.ok("Store all" in labels and "Store 1" in labels,
           f"{label}: a merged row offers Store all beside Store 1",
           f"menu labels: {labels!r}")
    entry = find_widget(port, "Store all")
    if not chk.ok(bool(entry), f"{label}: the 'Store all' entry is clickable"):
        close_menu(port)
        close_session(chk, port, [acolyte], label)
        return
    click_widget_center(port, entry)
    time.sleep(1.5)

    stored = ep_ids(port, B_P, DEF_BATCH)
    kept = ep_ids(port, U_A, DEF_BATCH)
    chk.ok(len(stored) == 8,
           f"{label}: eight of the twelve fit and were stored",
           f"got {len(stored)}: {stored!r}")
    chk.ok(len(kept) == 4,
           f"{label}: the four that did not fit are still on the acolyte",
           f"got {len(kept)}: {kept!r}")
    chk.ok(sorted(stored + kept) == sorted(minted)
           and not set(stored) & set(kept),
           f"{label}: every one of the twelve is in EXACTLY ONE endpoint — "
           f"nothing half-moved, nothing was lost and nothing was duplicated",
           f"stored={stored!r} kept={kept!r} minted={sorted(minted)!r}")
    # The remainder is reported by COUNT and by the contract's OWN reason,
    # so read the warning TEXT rather than counting rows that merely
    # mention the verb: "couldn't Store 4 x <item> -- receiver_full" is
    # the whole claim, and a message that reported some other count, or
    # invented a reason, would satisfy a substring count.
    new_warns = [w for w in warning_texts(port, acolyte)
                 if w not in before_warns]
    want = "couldn't Store %d x %s -- receiver_full" % (
        len(minted) - 8, f"Probe UT {DEF_BATCH}")
    chk.ok(len(new_warns) == 1 and want in new_warns[0],
           f"{label}: the remainder is REPORTED exactly once, naming the four "
           f"that did not fit and the contract's own 'receiver_full' reason",
           f"got {new_warns!r}, wanted a single warning containing {want!r}")
    # ...and the contract itself agrees, per instance: the eight that
    # moved are no longer the acolyte's to move, while the four that
    # stayed still are.
    # `completion` is DERIVED from the outcomes here rather than
    # hardcoded, so this asserts the contract is internally consistent
    # about its own summary as well as splitting where D-1 says.
    states = outcome_states(before_check)
    fails = sum(1 for st in states if st.startswith("failed"))
    want_completion = ("all" if fails == 0
                       else "none" if fails == len(states) else "partial")
    chk.ok(isinstance(before_check, dict)
           and before_check.get("accepted") is True
           and outcome_ids(before_check) == [int(i) for i in minted]
           and fails == len(minted) - 8
           and before_check.get("completion") == want_completion,
           f"{label}: the contract's own structured answer already splits "
           f"twelve into eight and four before anything moves, with a "
           f"`completion` that agrees with its own outcomes",
           f"got {before_check!r} (states {states!r})")
    after_stored = check_transfer(port, U_A, B_P, DEF_BATCH, stored)
    after_kept = check_transfer(port, U_A, B_P, DEF_BATCH, kept)
    chk.ok(isinstance(after_stored, dict)
           and outcome_ids(after_stored) == [int(i) for i in stored]
           and all(st == "failed:instance_missing"
                   for st in outcome_states(after_stored)),
           f"{label}: afterwards the contract refuses the eight that moved on "
           f"IDENTITY — they are no longer the acolyte's to offer",
           f"got {after_stored!r}")
    chk.ok(isinstance(after_kept, dict)
           and outcome_ids(after_kept) == [int(i) for i in kept],
           f"{label}: and still names the four that stayed, so the partial "
           f"batch split exactly where the report said it did",
           f"got {after_kept!r}")
    fp["batch"] = {"minted": sorted(minted), "stored": stored, "kept": kept}
    close_session(chk, port, [acolyte], label)


def stage_widget(chk: Checks, ledger: ViewLedger, fp: dict) -> None:
    """Requirement 1d: ONE widget rendered every container view this run
    encountered, asserted from the dumps collected as each one opened."""
    expected = [
        "container window on a building endpoint",
        "container window on a unit endpoint",
        "unit-info inventory section",
        "never-inspected container window",
        "escort source pane",
        "escort destination pane",
    ]
    missing = [name for name in expected if name not in ledger.views]
    chk.ok(not missing,
           "every container view this scenario can produce was actually "
           "opened and recorded", f"never opened: {missing!r}")
    for name, ev in sorted(ledger.views.items()):
        chk.ok(ev["registered"],
               f"the {name} is rendered by the ONE item-list widget (its list "
               f"id names a live instance of it)", f"got {ev!r}")
        chk.ok(ev["allItemList"],
               f"every row the {name} rendered came back through that same "
               f"widget's dump ({ev['rows']} rows)", f"got {ev!r}")
    ids = [ev["listId"] for ev in ledger.views.values() if ev["listId"]]
    chk.ok(len(ids) == len(set(ids)),
           "each view was its own INSTANCE of that widget rather than one "
           "shared list re-pointed at a second endpoint", f"list ids {ids!r}")
    fp["views"] = sorted(ledger.views)


def stage_save(chk: Checks, port: int, ids: dict, fp: dict, vp: dict):
    """D-3: one save taken with a Mode B order IN FLIGHT and a Mode A
    session OPEN on a DIFFERENT pair.

    Order matters and is not incidental: the Mode B gesture needs the
    container window, and the escort pair IS the base level — opening a
    container at that level would replace it and end the session. So the
    order is queued first, while paused, and the session is created
    afterwards. The only unpaused window between them is the one the
    escort needs to arrive, which is why its partner is spawned already
    beside it.

    Returns the state the fresh process must re-check, or None."""
    acolyte, mule, stale = ids["acolyte"], ids["technomule"], ids["stale"]
    U_A, B_T = ("unit", acolyte), ("building", stale)
    label = "save"
    set_paused(port, True)
    calm(port, acolyte)

    # Put a real TRIP between the carrier and its destination first. The
    # session below needs the world running for the few seconds its
    # escort takes to arrive, and the carrier's order is walking during
    # exactly those seconds — from the far end of the fixture shelf it
    # cannot arrive within them, which is what leaves the order genuinely
    # in flight rather than racing the save.
    rect = ep_rect(port, B_T)
    if rect is not None:
        far = max(ids["sites"],
                  key=lambda p: max(abs(p[0] - rect[0]), abs(p[1] - rect[1])))
        send(port, f"unit.stop({acolyte}); require('scripts.unit_ai')"
                   f".commandMove({acolyte}, {far[0]}, {far[1]}); return 'ok'")
        set_paused(port, False)
        poll_until(120.0,
                   lambda: (footprint_gap(port, U_A, B_T) or 0) >= 10,
                   interval=0.5)
        set_paused(port, True)
        time.sleep(0.4)
    chk.ok((footprint_gap(port, U_A, B_T) or 0) >= 4,
           f"{label}: the carrier starts its order well away from the "
           f"destination (footprint Chebyshev "
           f"{footprint_gap(port, U_A, B_T)!r})")

    # -- the Mode B order, queued through the real gesture and left in flight.
    if not mode_b_queue_only(chk, port, U_A, B_T, DEF_SAVE, acolyte, label):
        return None
    live = [o for o in orders(port, acolyte) if not o.get("terminal")]
    if not chk.ok(len(live) == 1, f"{label}: the acolyte carries exactly one "
                                  f"live order", f"got {live!r}"):
        return None
    ident = order_identity(live[0])
    iid = ident["items"][0] if ident["items"] else None

    # -- the Mode A session, on a pair that shares nothing with that order.
    info = send_json(port, f"return unit.getInfo({mule})")
    if not isinstance(info, dict):
        chk.ok(False, f"{label}: the technomule still resolves")
        return None
    beside = adjacent_dry(port, int(info.get("gridX", 0)),
                          int(info.get("gridY", 0)))
    if not chk.ok(beside is not None,
                  f"{label}: a dry tile beside the technomule for its escort "
                  f"partner"):
        return None
    partner = spawn_unit(chk, port, "acolyte", beside[0], beside[1],
                         f"{label} escort partner")
    if partner is None:
        return None
    ids["partner"] = partner
    calm(port, mule)
    calm(port, partner)
    if not create_session(chk, port, mule, ("unit", partner), vp, label,
                          via_menu=False):
        return None
    if not await_session_open(chk, port, mule, label, seconds=90.0):
        send(port, "require('scripts.transfer_session').clear(); return 'ok'")
        return None
    sess = session(port)

    # -- and the order must still be going somewhere.
    live = [o for o in orders(port, acolyte) if not o.get("terminal")]
    if not chk.ok(len(live) == 1 and order_identity(live[0])["id"]
                  == ident["id"],
                  f"{label}: the Mode B order is still IN FLIGHT when the save "
                  f"is taken — non-terminal, nothing committed",
                  f"got {[order_identity(o) for o in orders(port, acolyte)]!r}"):
        return None
    states = [e.get("state") for e in (live[0].get("entries") or [])]
    chk.ok(all(s not in ("completed", "failed") for s in states),
           f"{label}: none of its entries has reached a terminal state",
           f"got {states!r}")
    chk.ok(iid is not None and iid in ep_ids(port, U_A, DEF_SAVE),
           f"{label}: its instance is still on the carrier")

    held = [mule, partner]
    page = send(port, "return world.getActiveWorldId()").strip().strip('"')
    rid_pre = send(port, f"return engine.saveWorld('{page}', '{SLOT}')"
                   ).strip().strip('"')
    chk.ok(rid_pre == "true", f"{label}: engine.saveWorld accepted the request",
           f"got {rid_pre!r}")
    rid = capture_request_id(port, "return engine.getSaveStatus()")
    okay, status = wait_save_complete(port, rid, seconds=180.0) if rid else (False, None)
    if not chk.ok(bool(okay), f"{label}: the save completes and is durable",
                  f"status {status!r}"):
        return None
    fp["persisted"] = {"order": ident, "held": sorted(held),
                       "sessionId": (sess or {}).get("id")}
    chk.ok(True, f"{label}: captured order #{ident['id']} in flight and a Mode "
                 f"A session holding {sorted(held)}")
    return {"order": ident, "instance": iid, "held": sorted(held),
            "carrier": acolyte, "destination": B_T, "page": page}


def mode_b_queue_only(chk: Checks, port: int, src, dst, def_name: str,
                      stager: int, label: str) -> bool:
    """Fire one real Store gesture and leave the order queued — the
    save stage's own leg, which deliberately never runs to completion."""
    seed_source(port, src, def_name, stager)
    ids = ep_ids(port, src, def_name)
    if not chk.ok(len(ids) == 1,
                  f"{label}: the carrier holds exactly one {def_name}",
                  f"got {ids!r}"):
        return False
    iid = ids[0]
    send(port, "unit.deselectAll(); return 'ok'")
    send(port, f"return unit.select({src[1]})")
    time.sleep(0.7)
    if not open_window(chk, port, dst, label):
        return False
    gap = footprint_gap(port, src, dst)
    chk.ok(gap is not None and gap > 1,
           f"{label}: the carrier is far from the destination, so the order "
           f"has a real trip left to make (footprint Chebyshev {gap})",
           f"gap={gap!r}")
    row = find_row(port, UNIT_INV_LIST_ID, def_name)
    if not chk.ok(bool(row), f"{label}: the {def_name} row is located"):
        close_window(port)
        return False
    right_click_widget_center(port, row)
    time.sleep(0.5)
    entry = find_widget(port, "Store 1")
    if not chk.ok(bool(entry), f"{label}: the 'Store 1' entry is clickable",
                  f"menu labels: {menu_labels(port)!r}"):
        close_menu(port)
        close_window(port)
        return False
    click_widget_center(port, entry)
    time.sleep(0.9)
    close_window(port)
    got = [o for o in orders(port, src[1])
           if [e.get("instanceId") for e in (o.get("entries") or [])] == [iid]]
    return chk.ok(len(got) == 1,
                  f"{label}: the gesture queued one durable order naming "
                  f"instance {iid}",
                  f"got {[order_identity(o) for o in orders(port, src[1])]!r}")


def stage_load(chk: Checks, port: int, fp: dict, base: str, state: dict,
               args) -> None:
    """A FRESH PROCESS: the Mode B order survives with its exact identity
    and completes; the Mode A session does not survive at all."""
    label = "load"
    if not chk.ok(bool(poll_until(120.0,
                                  lambda: find_widget(port, "Create World"))),
                  f"{label}: the fresh process reaches the main menu"):
        return
    # The save names the probe's own defs, so they have to exist again
    # before the load validates its content.
    if not load_fixtures(chk, port, base):
        return
    accepted = send(port, "require('scripts.main_menu').loadAndShowSave("
                          f"'{SLOT}'); return 'ok'", timeout=30.0)
    rid = capture_request_id(port, "return engine.getLoadStatus()")
    published, status = wait_load_published(port, seconds=300.0, request_id=rid)
    if not chk.ok(bool(published),
                  f"{label}: the save loads and publishes in the fresh process",
                  f"accepted={accepted!r} status={status!r}"):
        return
    ready = poll_until(90.0,
                       lambda: send(port, "return world.getActiveWorldId()"
                                    ).strip().strip('"') == state["page"],
                       interval=0.5)
    chk.ok(bool(ready), f"{label}: the loaded page is the one that was saved",
           f"got {send(port, 'return world.getActiveWorldId()')!r}")
    # Chunks queue progressively after publication.
    time.sleep(15.0)
    set_paused(port, True)

    carrier = state["carrier"]
    want = state["order"]
    got = [order_identity(o) for o in orders(port, carrier)]
    chk.ok(got == [want],
           f"{label}: the Mode B order survives with its EXACT identity — same "
           f"order id, same endpoint identities, same instance ids",
           f"got {got!r} want {[want]!r}")
    live = [o for o in orders(port, carrier) if not o.get("terminal")]
    chk.ok(len(live) == 1,
           f"{label}: and is still non-terminal, with work left to do",
           f"got {[order_identity(o) for o in orders(port, carrier)]!r}")
    chk.ok(state["instance"] in ep_ids(port, ("unit", carrier), DEF_SAVE),
           f"{label}: the carrier still holds the exact instance the order "
           f"names")

    # Ask whether the module is even loaded first: "no session" is only
    # a statement about the load if the module that would hold one is
    # live in this process. It is (the offscreen HUD boots the whole
    # script stack), and asserting it keeps the check from passing
    # vacuously in some future profile where it is not.
    loaded = send(port, "return tostring(package.loaded['scripts."
                        "transfer_session'] ~= nil)").strip().strip('"')
    chk.ok(loaded == "true",
           f"{label}: the session module is live in the fresh process, so the "
           f"next check is about the LOAD and not about an unloaded module",
           f"package.loaded = {loaded!r}")
    chk.ok(session(port) is None,
           f"{label}: the Mode A session did NOT survive — a session is "
           f"transient by design and a load must never restore one pointing "
           f"at endpoints the replacement session may not have",
           f"got {session(port)!r}")
    chk.ok(stack_dump(port).get("depth") in (0, None),
           f"{label}: and its panes are gone with it",
           f"got {stack_dump(port)!r}")
    for uid in state["held"]:
        exists = send(port, f"return tostring(unit.exists({uid}))"
                      ).strip().strip('"')
        if not chk.ok(exists == "true",
                      f"{label}: formerly held unit {uid} came back",
                      f"unit.exists = {exists!r}"):
            continue
        holds = send(port, "return tostring(require('scripts.transfer_session')"
                           f".holdsUnit({uid}))").strip().strip('"')
        chk.ok(holds == "false",
               f"{label}: formerly held unit {uid} is not held by anything")

    set_paused(port, False)
    for uid in state["held"]:
        chk.ok(accepts_movement(port, uid),
               f"{label}: formerly held unit {uid} takes an ordinary player "
               f"order again — the hold really is released, not merely absent "
               f"from a table")

    before_ev = event_total(port, carrier, "unit_event",
                            f"Probe UT {DEF_SAVE}")
    landed = poll_until(300.0,
                        lambda: state["instance"]
                        in ep_ids(port, state["destination"], DEF_SAVE),
                        interval=1.0)
    set_paused(port, True)
    time.sleep(0.5)
    if not chk.ok(bool(landed),
                  f"{label}: the resumed carrier walks the surviving order and "
                  f"commits it",
                  f"carrier running {ai_action(port, carrier)!r}, orders "
                  f"{[order_identity(o) for o in orders(port, carrier)]!r}"):
        return
    chk.ok(state["instance"] not in ep_ids(port, ("unit", carrier), DEF_SAVE),
           f"{label}: and the instance left the carrier")
    chk.ok(not orders(port, carrier),
           f"{label}: the completed order was pruned",
           f"got {[order_identity(o) for o in orders(port, carrier)]!r}")
    chk.ok(event_total(port, carrier, "unit_event", f"Probe UT {DEF_SAVE}")
           - before_ev == 1,
           f"{label}: it completed EXACTLY ONCE across the process boundary")


def stage_setup(chk: Checks, port: int, fp: dict, base: str, args):
    """A fixed-seed world, the probe's own defs, and the three endpoint
    classes stocked. Returns the fixture ids, or None."""
    if not reach_main_menu(chk, port):
        return None, None
    if not create_world(chk, port, args.seed, args.world_size, args.plates):
        return None, None
    vp = viewport(port, fallback=tuple(int(v) for v in args.size.split("x")))
    chk.ok(vp["win_w"] > 0 and vp["win_h"] > 0 and vp["fb_w"] > 0
           and vp["fb_h"] > 0,
           "the engine reports a usable window and framebuffer extent",
           f"got {vp!r}")
    print(f"  (window {vp['win_w']}x{vp['win_h']}, "
          f"framebuffer {vp['fb_w']}x{vp['fb_h']})", flush=True)
    if not load_fixtures(chk, port, base):
        return None, vp

    print("  (scanning terrain outward from the origin for dry anchor sites)",
          flush=True)
    sites = allocate_flat_anchors(port, 7)
    if not chk.ok(sites is not None,
                  "seven separated dry sites are found for the fixtures"):
        return None, vp
    (hold_xy, partial_xy, stale_xy, unseen_xy,
     acolyte_xy, mule_xy, wild_xy) = sites
    print(f"  (fixture sites: hold={hold_xy} partial={partial_xy} "
          f"stale={stale_xy} unseen={unseen_xy} acolyte={acolyte_xy} "
          f"technomule={mule_xy} wildlife={wild_xy})", flush=True)

    ids = {"sites": list(sites)}
    ids["hold"] = spawn_hold(chk, port, DEF_HOLD, *hold_xy, "storage hold")
    ids["partial"] = spawn_hold(chk, port, DEF_PARTIAL, *partial_xy,
                                "small hold (room for exactly eight)")
    ids["stale"] = spawn_hold(chk, port, DEF_STALE_HOLD, *stale_xy,
                              "knowledge hold")
    ids["unseen"] = spawn_hold(chk, port, DEF_UNSEEN, *unseen_xy,
                               "never-inspected hold", want_built=False)
    ids["acolyte"] = spawn_unit(chk, port, "acolyte", *acolyte_xy, "acolyte")
    ids["technomule"] = spawn_unit(chk, port, "technomule", *mule_xy,
                                   "technomule")
    # `unit.spawn` defaults to the WILDLIFE faction when no tag is given.
    ids["wildlife"] = spawn_unit(chk, port, "red_squirrel", *wild_xy,
                                 "wildlife", faction=None, quiet=True)
    if any(v is None for v in ids.values()):
        chk.ok(False, "every fixture spawned", f"got {ids!r}")
        return None, vp

    # The knowledge stage's two extra units, both spawned in place rather
    # than walked there: what it measures is the reveal rule, not
    # pathfinding, and a unit already standing beside the container makes
    # both the proximity case and the Mode A arrival immediate.
    beside = adjacent_dry_tiles(port, *stale_xy)
    if not chk.ok(len(beside) >= 2,
                  "two dry tiles beside the knowledge hold, so neither of its "
                  "two units has to stand on the building itself",
                  f"got {beside!r}"):
        return None, vp
    ids["watcher"] = spawn_unit(chk, port, "acolyte", beside[0][0], beside[0][1],
                                "knowledge watcher")
    ids["reveal"] = spawn_unit(chk, port, "acolyte", beside[1][0], beside[1][1],
                               "knowledge escort")
    if ids["watcher"] is None or ids["reveal"] is None:
        return None, vp

    # Settle the camera into the TILE zoom band NOW, while nothing is
    # open. A Mode A session's arrival snap calls `camera.goToTile`, and
    # crossing a zoom band is one of `view_teardown`'s triggers — it
    # clears the session outright — so a session created while the
    # camera is still on the post-worldgen zoomed-out view tears itself
    # down the instant it opens. Pinning here once makes every later
    # snap a within-band move.
    hz = send_json(port, f"return building.getInfo({ids['hold']})")
    pin_camera_to_tile(port, hold_xy[0], hold_xy[1],
                       int((hz or {}).get("gridZ", 0)))
    cam = camera_state(port)
    chk.ok(isinstance(cam.get("zoom"), (int, float)) and cam["zoom"] < 1.2,
           "the camera is settled in the zoomed-in tile band before any "
           "session exists, so no arrival snap crosses a band and tears one "
           "down", f"got {cam!r}")

    ids["homes"] = {ids["acolyte"]: acolyte_xy, ids["technomule"]: mule_xy}

    for key, ep in (("acolyte", ("unit", ids["acolyte"])),
                    ("technomule", ("unit", ids["technomule"])),
                    ("storage", ("building", ids["hold"]))):
        info = ep_info(port, ep)
        chk.ok(info.get("eligible") is True,
               f"the {key} endpoint is eligible to the transfer contract",
               f"got {info!r}")
    chk.ok(ep_info(port, ("unit", ids["wildlife"])).get("eligible") is False,
           "and the wildlife unit is NOT — it is not player-commandable, which "
           "is what makes its withdrawal below a non-revealing one")

    # Two instances into the knowledge hold, through the lax AI verb, so
    # it starts KNOWN with a real observation time.
    for _ in range(2):
        stage_into_hold(port, ids["reveal"], ids["stale"], DEF_STALE)
    chk.ok(len(remembered_ids(port, ids["stale"], DEF_STALE)) == 2,
           "the knowledge hold is stocked and remembered")

    set_paused(port, True)
    fp["endpoints"] = {k: v for k, v in sorted(ids.items())
                       if isinstance(v, int)}
    return ids, vp


def main() -> int:
    ap = argparse.ArgumentParser(description="Unified transfer system gate")
    ap.add_argument("--port", type=int, default=9432)
    ap.add_argument("--size", default="1280x900")
    ap.add_argument("--seed", type=int, default=42)
    ap.add_argument("--world-size", type=int, default=64)
    ap.add_argument("--plates", type=int, default=3)
    ap.add_argument("--keep-root", action="store_true",
                    help="don't delete the throwaway resource root on exit")
    args = ap.parse_args()

    chk = Checks()
    ledger = ViewLedger()
    base = tempfile.mkdtemp(prefix="synarchy_unified_transfer_")
    root = make_isolated_root(base)
    print(f"isolated resource root: {root}", flush=True)
    fp: dict = {"seed": args.seed, "worldSize": args.world_size,
                "plates": args.plates}
    port = args.port
    state = None

    try:
        # ============ engine A: the whole scenario, then a save =========
        proc = boot_offscreen(root, port, args.size, LOG_A, "engine A")
        try:
            chk.enter("setup", "a fixed-seed world and the three endpoint "
                               "classes")
            ids, vp = stage_setup(chk, port, fp, base, args)
            if ids is None:
                raise SetupError("the scenario's fixtures could not be built")

            chk.enter("knowledge", "D-2: contents are genuinely stale, and "
                                   "only an interaction refreshes them")
            stage_knowledge(chk, port, ledger, ids, fp, vp)

            chk.enter("modeB", "all six directed legs through the queued "
                               "gestures")
            stage_mode_b(chk, port, ledger, ids, fp, vp)

            chk.enter("modeA", "the same six legs through three real escort "
                               "sessions")
            stage_mode_a(chk, port, ledger, ids, fp, vp)

            chk.enter("batch", "D-1: twelve into room for eight")
            stage_batch(chk, port, ids, fp, vp)

            chk.enter("widget", "one widget rendered every container view")
            stage_widget(chk, ledger, fp)

            chk.enter("save", "a Mode B order in flight and a Mode A session "
                              "open, in one save")
            state = stage_save(chk, port, ids, fp, vp)
        finally:
            quit_engine(port, proc)

        # ============ engine B: a genuinely fresh process ===============
        chk.enter("load", "a fresh process re-checks every durable identity")
        if state is None:
            chk.ok(False, "load: the save stage produced nothing to reload")
        else:
            proc = boot_offscreen(root, port, args.size, LOG_B, "engine B")
            try:
                stage_load(chk, port, fp, base, state, args)
            finally:
                quit_engine(port, proc)
    except SetupError as exc:
        chk.ok(False, f"the scenario could not reach the state it tests: {exc}")
    except SystemExit as exc:
        # `probelib.boot` reports an engine that died before READY, or
        # never printed it, by calling sys.exit() — and SystemExit derives
        # from BaseException, not Exception, so the clause below does not
        # see it. Left uncaught it would unwind straight through the
        # finally, which prints the stage summary first: a stage entered
        # but not yet asserted in would be reported as passing on the way
        # out.
        chk.ok(False, f"the engine could not be started, or died, during stage "
                      f"'{chk.stage}' (SystemExit: {exc.code})")
    except Exception as exc:  # noqa: BLE001
        # An operational failure — a dead engine, a socket timeout, a
        # malformed console response — is a real probe failure and must
        # name its stage like any other. Left to propagate it would exit
        # non-zero with a traceback but NO recorded failing check, and the
        # summary below would then print PASS over the top of it.
        # KeyboardInterrupt is deliberately still allowed to propagate.
        chk.ok(False, f"unexpected {type(exc).__name__} while running stage "
                      f"'{chk.stage}': {exc}")
        traceback.print_exc()
    finally:
        if args.keep_root:
            print(f"kept resource root: {base}", flush=True)
        else:
            shutil.rmtree(base, ignore_errors=True)
        fp["stages"] = chk.outcomes()
        print(f"\nFINGERPRINT {json.dumps(fp, sort_keys=True)}", flush=True)
        chk.report()

    return 0 if chk.failed == 0 else 1


if __name__ == "__main__":
    sys.exit(main())
