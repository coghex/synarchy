#!/usr/bin/env python3
"""Headless power-grid wire probe (#359).

Verifies the wire structure piece end-to-end on a flat arena world,
WITHOUT a GPU or a human watching:

  1. connections   : scripts.wire's autotile shape derivation — placing a
                      line + a plus of wire tiles picks the right
                      connection variant (isolated/end/straight/corner/
                      tee/cross) purely from 4-neighbour adjacency, and a
                      later placement re-caps already-placed neighbours.
  2. path_builder  : the build_tool.lua PATH placement UX itself (not
                      just the lower-level construction.* calls it
                      drives) — a real two-click drag through
                      buildTool.handleMouseDown (pixel coords resolved
                      via world.pickTile, same as a player click) snaps
                      to a straight line via buildTool.snapWirePath and
                      designates ONLY that line, not the drag's filled
                      bounding rectangle; a pending anchor cancels on
                      right-click without designating anything.
  3. build_ai      : the construct_job AI end-to-end for kind "wire" —
                      designate, an acolyte carrying `wiring` claims,
                      builds, and places a real wire piece; the
                      designation clears and the piece lands in the
                      PERSISTED per-chunk overlay (structure.loadedCount).

Usage:
  python3 tools/wire_probe.py [--port 9359] [--phase all]

Exit code 0 = all checks passed.
"""
from __future__ import annotations

import argparse
import glob
import json
import socket
import subprocess
import sys
import time
from probelib import clear_find_water, quit_engine, boot, send, send_json

LOG = "/tmp/wire_probe_engine.log"


def bootstrap(port: int) -> None:
    """Load defs + the flat arena (the loading screen doesn't run headless).
    unit_ai is auto-loaded at boot and IS the machinery under test, so it
    stays live (matches construction_probe.py)."""
    loaders = [
        ("data/substances/*.yaml", "engine.loadSubstanceYaml"),
        ("data/items/*.yaml",      "engine.loadItemYaml"),
        ("data/equipment/*.yaml",  "engine.loadEquipmentYaml"),
        ("data/materials/*.yaml",  "engine.loadMaterialYaml"),
        ("data/units/*.yaml",      "engine.loadUnitYaml"),
        ("data/buildings/*.yaml",  "engine.loadBuildingYaml"),
    ]
    for pattern, fn in loaders:
        for path in sorted(glob.glob(pattern)):
            send(port, f"{fn}('{path}'); return 'ok'")
    send(port,
         "return require('scripts.movement_arena').buildCourse('flat').name")
    if not poll_until(port, 30, lambda: wid(port)):
        sys.exit("arena page never became the active world")
    send(port, "return world.loadChunksInRegion(-1, -1, 2, 2)")
    send(port, "return world.waitForChunks(60)", timeout=65.0)
    # A survival_critical notification (data/notification_categories.yaml
    # `pause: true`) can auto-pause the engine, which freezes unit_ai's
    # update() (it early-returns while paused per scripts/pause.lua) —
    # silently stalling every phase below. Defensively resume; harmless
    # no-op if nothing paused it.
    send(port, "engine.setPaused(false); return 'ok'")


def wid(port: int) -> str | None:
    raw = send(port, "return world.getActiveWorldId()")
    raw = raw.strip().strip('"')
    return raw if raw and raw not in ("null", "nil") else None


def designation_at(port: int, x: int, y: int):
    return send_json(
        port,
        f"return construction.getDesignationAt(world.getActiveWorldId(), {x}, {y})")


def wire_at(port: int, x: int, y: int):
    return send_json(port, f"return structure.getAt({x}, {y}, 'wire')")


def wire_shape(port: int, x: int, y: int) -> str | None:
    """The connection-variant NAME a placed wire tile resolved to, read
    back from its texture path (data/structure_packs/wire.yaml maps
    shape name -> assets/textures/structures/wire/<shape>.png 1:1)."""
    info = wire_at(port, x, y)
    if not info or "tex" not in info:
        return None
    path = info["tex"]
    return path.rsplit("/", 1)[-1].removesuffix(".png")


def pick_tile(port: int, sx: int, sy: int) -> tuple[int, int]:
    """world.pickTile(screenX, screenY) -> (gx, gy) — the SAME hit test
    buildTool.handleMouseDown runs on a real click. Returns whichever
    tile is actually under that pixel rather than assuming a hand-derived
    projection, so the path-builder phase stays correct even if camera/
    projection constants ever change."""
    raw = send(port, f"return world.pickTile({sx}, {sy})")
    parts = raw.split()
    return int(float(parts[0])), int(float(parts[1]))


def spawn_acolyte(port: int, x: float, y: float) -> int:
    uid = send(port, f"return unit.spawn('acolyte', {x}, {y})")
    try:
        n = int(float(uid))
    except ValueError:
        sys.exit(f"unit.spawn failed: {uid!r}")
    for it in ("pick_steel", "shovel_steel", "axe_steel",
               "rations", "rations"):
        send(port, f"unit.removeItem({n}, '{it}'); return 'ok'")
    if not clear_find_water(port, n):
        sys.exit(f"unit {n} never got AI state")
    return n


def destroy_unit(port: int, uid: int) -> None:
    send(port, f"unit.destroy({uid}); return 'ok'")


def poll_until(port: int, seconds: float, fn):
    """Poll fn until truthy, defensively unpausing each pass: notification
    categories can auto-pause (config/notifications.yaml unit_warning /
    survival_critical), which would otherwise freeze unit_ai's update()
    (early-returns while paused) and stall the whole probe (matches
    tools/craft_bill_probe.py's `poll`)."""
    deadline = time.time() + seconds
    while time.time() < deadline:
        send(port, "engine.setPaused(false); return 'ok'")
        v = fn()
        if v:
            return v
        time.sleep(0.3)
    return None


CHECKS: list[tuple[str, bool]] = []


def check(label: str, ok: bool) -> None:
    CHECKS.append((label, bool(ok)))
    print(f"  [{'PASS' if ok else 'FAIL'}] {label}")


# --- phases ------------------------------------------------------------


def phase_connections(port: int) -> None:
    """Direct scripts.wire placement (no AI): a 3-tile line, a lone tile,
    and a plus/cross, checking the autotile shape at each step. All
    coordinates stay within the arena's loaded -1,-1,2,2 chunk region
    (tiles roughly -16..47) — placing on an unloaded chunk is a silent
    engine no-op (structure.place), which would misread as a shape bug."""
    print("\n[phase 1] connection-aware autotile shapes")

    # A lone tile: no neighbours -> isolated.
    send(port, "require('scripts.wire').place(10, 10); return 'ok'")
    check("lone tile is 'isolated'", wire_shape(port, 10, 10) == "isolated")

    # A 3-tile east-west line: (5,20)-(6,20)-(7,20).
    send(port, "require('scripts.wire').place(5, 20); return 'ok'")
    check("first tile of a line starts as isolated (no neighbour yet)",
          wire_shape(port, 5, 20) == "isolated")
    send(port, "require('scripts.wire').place(6, 20); return 'ok'")
    check("placing an east neighbour re-caps the first tile to end_e",
          wire_shape(port, 5, 20) == "end_e"
          and wire_shape(port, 6, 20) == "end_w")
    send(port, "require('scripts.wire').place(7, 20); return 'ok'")
    check("middle tile of a 3-run reads straight_ew",
          wire_shape(port, 6, 20) == "straight_ew")
    check("far (east) end of the run reads end_w",
          wire_shape(port, 7, 20) == "end_w")
    check("near (west) end is untouched by the far placement (end_e)",
          wire_shape(port, 5, 20) == "end_e")

    # A plus: centre (20,20) with all 4 neighbours -> cross, with each
    # neighbour reading as a single-connection end back toward centre.
    cx, cy = 20, 20
    for dx, dy in ((0, -1), (1, 0), (0, 1), (-1, 0)):
        send(port, f"require('scripts.wire').place({cx+dx}, {cy+dy}); return 'ok'")
    send(port, f"require('scripts.wire').place({cx}, {cy}); return 'ok'")
    check("plus centre reads cross", wire_shape(port, cx, cy) == "cross")
    check("plus north arm re-capped to end_s", wire_shape(port, cx, cy - 1) == "end_s")
    check("plus east arm re-capped to end_w", wire_shape(port, cx + 1, cy) == "end_w")
    check("plus south arm re-capped to end_n", wire_shape(port, cx, cy + 1) == "end_n")
    check("plus west arm re-capped to end_e", wire_shape(port, cx - 1, cy) == "end_e")

    # A corner: east neighbour + south neighbour placed first, corner tile
    # (their shared NW corner) placed last -> corner_se (connects south+east).
    send(port, "require('scripts.wire').place(15, 14); return 'ok'")   # E of corner
    send(port, "require('scripts.wire').place(14, 15); return 'ok'")   # S of corner
    send(port, "require('scripts.wire').place(14, 14); return 'ok'")   # corner tile
    check("corner tile reads corner_se", wire_shape(port, 14, 14) == "corner_se")


def phase_path_builder(port: int) -> None:
    """Drive the REAL build_tool.lua two-click path placement through
    buildTool.handleMouseDown — pixel clicks resolved via world.pickTile,
    exactly like a player's mouse input — rather than only exercising the
    lower-level construction.* Lua API. HUD/toolbar never initialise
    headless (no GPU), so buildTool.hud (normally set by hud.lua after
    building the toolbar) is stubbed with just the one field
    handleMouseDown/enterPlacement actually read: worldId."""
    print("\n[phase 2] build_tool.lua wire PATH placement (real click path)")
    w = wid(port)
    send(port, "camera.setPosition(0, 0); return 'ok'")
    send(port, "local bt = require('scripts.build_tool'); "
               f"bt.hud = {{ worldId = '{w}' }}; return 'ok'")
    send(port, "local bt = require('scripts.build_tool'); "
               "bt.enterPlacement({kind='structure', pack='wire', "
               "piece='wire', edge=nil, displayName='Wire'}); return 'ok'")

    # Click 1: anchor. Discover the actual tile under the pixel via the
    # SAME world.pickTile the click handler uses, rather than assuming a
    # hand-derived screen->tile projection.
    ax, ay = pick_tile(port, 960, 540)
    clicked = send(port, "local bt = require('scripts.build_tool'); "
                          "return bt.handleMouseDown(1, 960, 540)")
    check("first click consumed and anchors the path", clicked == "true")
    anchor = send_json(port, "local bt = require('scripts.build_tool'); "
                              "return bt.state.anchor")
    check("anchor matches the clicked tile", anchor == [ax, ay])

    # Click 2: a DIAGONAL drag (not axis-aligned) — the path tool must
    # snap this to a straight line, unlike the filled-rectangle commit
    # every other structure piece uses.
    hx, hy = pick_tile(port, 1056, 668)
    committed = send(port, "local bt = require('scripts.build_tool'); "
                            "return bt.handleMouseDown(1, 1056, 668)")
    check("second click consumed and commits the path", committed == "true")
    check("anchor cleared after commit",
          send(port, "local bt = require('scripts.build_tool'); "
                     "return bt.state.anchor == nil") == "true")

    # snapWirePath returns two values (multi-return), which the console
    # prints tab-separated like pickTile — not JSON.
    snap_raw = send(port,
        f"local bt = require('scripts.build_tool'); "
        f"return bt.snapWirePath({ax}, {ay}, {hx}, {hy})")
    ex, ey = (int(float(v)) for v in snap_raw.split())
    on_line = set()
    if ex == ax:   # vertical snap
        on_line = {(ax, y) for y in range(min(ay, ey), max(ay, ey) + 1)}
    else:          # horizontal snap
        on_line = {(x, ay) for x in range(min(ax, ex), max(ax, ex) + 1)}

    bbox = {(x, y)
            for x in range(min(ax, hx), max(ax, hx) + 1)
            for y in range(min(ay, hy), max(ay, hy) + 1)}
    on_line_designated = all(
        (designation_at(port, x, y) or {}).get("pack") == "wire"
        for (x, y) in on_line)
    off_line_clear = all(
        designation_at(port, x, y) is None
        for (x, y) in bbox - on_line)
    check("every tile ON the snapped line got designated", on_line_designated)
    check("the drag was a diagonal (line is a proper subset of its bbox)",
          len(on_line) < len(bbox))
    check("tiles OFF the line (inside the drag's bbox) were NOT designated",
          off_line_clear)

    # Clean up this phase's designations so they don't leak into others.
    for (x, y) in on_line:
        send(port, f"construction.cancelDesignation({x}, {y}); return 'ok'")

    # Right-click with a pending anchor cancels it — no designation at all.
    nx, ny = pick_tile(port, 1200, 700)
    send(port, f"local bt = require('scripts.build_tool'); "
               f"bt.handleMouseDown(1, 1200, 700); return 'ok'")
    send(port, "local bt = require('scripts.build_tool'); "
               "bt.handleMouseDown(2, 1200, 700); return 'ok'")
    check("right-click cancels a pending anchor without designating",
          send(port, "local bt = require('scripts.build_tool'); "
                     "return bt.state.anchor == nil") == "true"
          and designation_at(port, nx, ny) is None)

    send(port, "local bt = require('scripts.build_tool'); "
               "bt.exitPlacement(); return 'ok'")


def phase_build_ai(port: int) -> None:
    """One wire designation, built by an acolyte carrying `wiring`."""
    print("\n[phase 3] wire structure job via construct_job AI")
    w = wid(port)
    send(port, f"construction.designate('{w}', 8, 8, 8, 8, "
               "'structure', 'wire', 'wire'); return 'ok'")
    time.sleep(0.5)
    check("wire tile designated", designation_at(port, 8, 8) is not None)

    uid = spawn_acolyte(port, 5.5, 8.5)
    send(port, f"unit.addItem({uid}, 'wiring', 0); return 'ok'")

    claimed = poll_until(port, 20, lambda: (
        (designation_at(port, 8, 8) or {}).get("status") == "claimed"))
    check("designation became 'claimed'", claimed is not None)

    before = send(port, "return structure.loadedCount()")
    done = poll_until(port, 60, lambda: (
        send(port, "return structure.hasAt(8, 8, 'wire')") == "true"
        and designation_at(port, 8, 8) is None))
    check("wire placed and designation cleared", done is not None)
    check("placed piece is in the PERSISTED per-chunk overlay",
          send(port, "return structure.loadedCount()") != before)
    check("built wire tile renders as an isolated node (no wired neighbours)",
          wire_shape(port, 8, 8) == "isolated")
    check("build consumed the carried wiring",
          send(port,
               "for _, it in ipairs(unit.getInventory(" + str(uid) + ") or {}) do "
               "if it.defName == 'wiring' then return false end end; "
               "return true") == "true")
    destroy_unit(port, uid)


PHASES = {
    "connections": phase_connections,
    "path_builder": phase_path_builder,
    "build_ai": phase_build_ai,
}


def main() -> int:
    ap = argparse.ArgumentParser(description=__doc__)
    ap.add_argument("--port", type=int, default=9359)
    ap.add_argument("--phase", default="all", choices=["all"] + list(PHASES))
    args = ap.parse_args()

    proc = boot(args.port, log=LOG)
    try:
        bootstrap(args.port)
        if not wid(args.port):
            print("FAIL: no active world after arena build", file=sys.stderr)
            return 2
        todo = PHASES.values() if args.phase == "all" else [PHASES[args.phase]]
        for phase in todo:
            phase(args.port)
    finally:
        quit_engine(args.port, proc)
        try:
            proc.wait(timeout=10)
        except subprocess.TimeoutExpired:
            proc.kill()

    failed = [label for label, ok in CHECKS if not ok]
    print(f"\n{len(CHECKS) - len(failed)}/{len(CHECKS)} checks passed"
          + (f"; FAILED: {failed}" if failed else ""))
    return 1 if failed else 0


if __name__ == "__main__":
    sys.exit(main())
