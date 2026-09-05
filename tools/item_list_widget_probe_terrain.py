#!/usr/bin/env python3
"""Terrain, camera and dry-anchor helpers for
`tools/item_list_widget_probe.py` (#2046).

Two jobs, both about placing a fixture somewhere the rendered checks can
reach it.

`allocate_dry_anchors` scans generated terrain outward from the origin
for separated dry sites, so the run's seven fixtures never land in water
or on top of each other. `focus_building` is #1286's fix: a bare
`camera.goToTile` leaves z-tracking ON, which pins the z-slice 25 levels
above the surface and pushes the tile it just went to off the bottom of
the viewport, so the slice has to be re-pinned to the target's own
`gridZ` AFTER every `goToTile`.

`tile_surface` is shared support rather than the escort module's own:
the escort scenarios use it to choose walkable ground, and
`allocate_dry_anchors` uses it to reject wet sites.
"""
from __future__ import annotations

import os
import sys

sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
from item_list_widget_probe_checks import check
from probelib import (camera_state, centred_within, focus_and_locate,
                      locate_building_pixel, send, send_json, set_paused,
                      targeting_report)

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
