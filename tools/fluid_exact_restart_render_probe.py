#!/usr/bin/env python3
"""Manual GPU extension of ``fluid_exact_restart``: the reloaded PARTIAL
cell renders with the level mask matching its exact level (#2535, DFL-6
of epic #2514).

``tools/fluid_exact_restart_probe.py`` proves a simulation-made partial
cell keeps its exact units and level across a fresh-process save/load,
but only through queries -- and a query value, or a mask index a
renderer reports about itself, does not prove what reaches the screen
(the root ``CLAUDE.md`` rule that headless success is not visual
correctness). This probe consumes that SAME saved scenario and grades
the reloaded cell's pixels:

  1. Engine A (headless): the restart probe's own steps 1-4 -- a real
     generated page, one ``world.setFluidTile``, the flow turning the
     edited cell partial, a paused and settled expectation, a completed
     save.
  2. Engine B (``--offscreen``): load that save, enter the world view
     the way a menu load does, confirm the reloaded neighbourhood still
     matches the expectation exactly, then freeze the frame (paused,
     time scale 0, pinned sun) and pin the camera on the cell.
  3. Capture the RELOADED frame and an immediate re-capture, whose
     difference is this view's noise floor.
  4. For each level L in 1..8, author the SAME cell at the same whole z
     with top level L through ``debug.setFluidSurface`` (the exact
     setter; no integer query is used as one) and capture a reference.
     Then restore the reloaded value and capture once more.
  5. Grade inside the cell's own screen box -- the union of every pixel
     the eight references disagree on, which is exactly where that
     cell's level geometry is drawn. The reloaded frame must be closest
     to the reference for its OWN level, within the noise floor, and
     strictly farther from every other level; the restored frame must
     match the reloaded one the same way.

Everything is retained under ``--out``: every frame, a crop of the cell
box from each, and ``manifest.json`` naming the page, the cell, its
exact units, level, integer ceiling, render height and matched mask, the
box, the noise floor and every per-level difference -- the evidence an
owner attaches to a PR.

Manual-only (``needs-gpu`` in ``tools/ci_probes.py``); never a CI gate.

Usage:
  python3 tools/fluid_exact_restart_render_probe.py [--port 9286] \\
      [--out /tmp/fluid-exact-restart-render]
"""
from __future__ import annotations

import argparse
import json
import os
import sys
import tempfile
import time
from pathlib import Path

from probelib import boot, poll_until, quit_engine, send
from offscreen_probe import find_widget, png_region_changed_pixels, screenshot
from fluid_exact_restart_probe import (
    PAGE, Checks, area_fluid, build_partial_save, ceil_div8, level_of,
    load_saved, make_isolated_root, verify_reload)

FRAME = (1024, 768)
DETAIL_ZOOM = 0.5


def set_view(port: int, tile, zoom: float, slice_z: int) -> None:
    """Point at the tile, set the zoom, and pin the z-slice, in that
    order (``tools/fluid_reaction_visual_probe.py``'s ``set_view``: every
    camera move re-enables z tracking, so the slice is pinned last)."""
    send(port, f"camera.goToTile({tile[0]}, {tile[1]}); "
               f"camera.setZoom({zoom}); camera.setZTracking(false); "
               f"camera.setZSlice({slice_z}); return 'ok'")
    time.sleep(1.0)


def freeze_frame(port: int) -> None:
    """Everything that would move a pixel on its own: the simulation,
    the calendar and the sun."""
    send(port, "engine.setPaused(true); return 'ok'")
    send(port, f"world.setTimeScale('{PAGE}', 0); return 'ok'")
    send(port, f"world.setSunAngle('{PAGE}', 0.5); return 'ok'")
    time.sleep(0.5)


def enter_world_view(chk: Checks, port: int) -> bool:
    """The gameplay surface for the loaded page, reached the way the
    menu's own load does: ``uiManager.onSaveLoaded`` has already bound
    the loaded page as the current world, so marking the view as
    loaded-from-save and showing it renders that page rather than
    creating a new one."""
    send(port, "local wv = package.loaded['scripts.world_view']; "
               "wv.loadedFromSave = true; "
               "package.loaded['scripts.ui_manager'].showMenu('world_view'); "
               "local h = package.loaded['scripts.hud']; "
               "if h and h.hide then h.hide() end; return 'ok'")
    time.sleep(3.0)
    active = send(port, "return world.getActiveWorldId()").strip().strip('"')
    return chk.ok(active == PAGE,
                  f"the world view renders the reloaded page (active {active!r})")


def author(chk: Checks, port: int, tile, kind: str, units: int) -> bool:
    """Set the cell to exact ``units`` and wait until it reads back."""
    ok = send(port, f"return debug.setFluidSurface('{PAGE}', {tile[0]}, "
                    f"{tile[1]}, '{kind}', {units})").strip() == "true"
    if not ok:
        return chk.ok(False, f"debug.setFluidSurface accepted {units} units")
    landed = poll_until(20.0, lambda: (area_fluid(port, tile[0], tile[1], 0)
                                       or {}).get(tuple(tile), {})
                        .get("surfaceUnits") == units, interval=0.25)
    if not landed:
        return chk.ok(False, f"the cell reads back {units} units after authoring")
    time.sleep(1.5)  # the chunk's quads rebuild on the next frames
    return True


def diff_box(path_a: str, path_b: str):
    """(x, y, w, h) of every pixel differing between two frames, or None."""
    from PIL import Image, ImageChops
    with Image.open(path_a) as a, Image.open(path_b) as b:
        if a.size != b.size:
            return None
        diff = ImageChops.difference(a.convert("RGB"), b.convert("RGB"))
        bbox = diff.convert("L").point(lambda v: 255 if v else 0).getbbox()
    if bbox is None:
        return None
    left, top, right, bottom = bbox
    return (left, top, right - left, bottom - top)


def union(a, b):
    if a is None:
        return b
    if b is None:
        return a
    x0, y0 = min(a[0], b[0]), min(a[1], b[1])
    x1 = max(a[0] + a[2], b[0] + b[2])
    y1 = max(a[1] + a[3], b[1] + b[3])
    return (x0, y0, x1 - x0, y1 - y0)


def crop(path: str, box, out: str, pad: int = 12, scale: int = 4) -> None:
    from PIL import Image
    x, y, w, h = box
    with Image.open(path) as im:
        region = im.crop((max(0, x - pad), max(0, y - pad),
                          min(im.width, x + w + pad), min(im.height, y + h + pad)))
        region.resize((region.width * scale, region.height * scale),
                      Image.NEAREST).save(out)


def main() -> int:
    ap = argparse.ArgumentParser()
    ap.add_argument("--port", type=int, default=9286)
    ap.add_argument("--out", default=None,
                    help="evidence directory (kept); default: a new temp dir")
    args = ap.parse_args()
    port = args.port
    out = Path(args.out or tempfile.mkdtemp(prefix="fluid_exact_restart_render_"))
    out.mkdir(parents=True, exist_ok=True)
    print(f"evidence directory: {out}")

    chk = Checks()
    with tempfile.TemporaryDirectory(prefix="fluid_exact_restart_render_root_") as base:
        root = make_isolated_root(base)

        print("== engine A (headless): generate, flow a partial cell, save ==")
        proc = boot(port, log=str(out / "engine_a.log"),
                    args=["--resource-root", root], ready_timeout=180)
        try:
            scenario = build_partial_save(chk, port)
        finally:
            quit_engine(port, proc)
        if scenario is None:
            print("\nFAILED: no saved partial cell to render.")
            return 1

        print("== engine B (offscreen): load, render, grade ==")
        proc = boot(port, log=str(out / "engine_b.log"), mode=("--offscreen",),
                    args=["--size", f"{FRAME[0]}x{FRAME[1]}",
                          "--resource-root", root], ready_timeout=240)
        try:
            menu = poll_until(120.0, lambda: find_widget(port, "Create World"),
                              interval=1.0)
            chk.ok(bool(menu), "the loading screen reached the main menu")
            if not (load_saved(chk, port) and enter_world_view(chk, port)):
                return 1
            freeze_frame(port)
            verify_reload(chk, port, scenario)

            tile = tuple(scenario["tile"])
            cell = scenario["cell"]
            units, level = cell["surfaceUnits"], cell["level"]
            ceiling = ceil_div8(units)
            set_view(port, tile, DETAIL_ZOOM, ceiling + 1)

            reloaded = str(out / "reloaded.png")
            control = str(out / "reloaded_control.png")
            chk.ok(screenshot(port, reloaded), "captured reloaded.png")
            time.sleep(0.6)
            chk.ok(screenshot(port, control), "captured reloaded_control.png")

            refs = {}
            base_units = (ceiling - 1) * 8
            for lvl in range(1, 9):
                if not author(chk, port, tile, cell["type"], base_units + lvl):
                    return 1
                refs[lvl] = str(out / f"level_{lvl}.png")
                chk.ok(screenshot(port, refs[lvl]), f"captured level_{lvl}.png")
            if not author(chk, port, tile, cell["type"], units):
                return 1
            restored = str(out / "restored.png")
            chk.ok(screenshot(port, restored), "captured restored.png")
        finally:
            quit_engine(port, proc)

    box = None
    for a in range(1, 9):
        for b in range(a + 1, 9):
            box = union(box, diff_box(refs[a], refs[b]))
    if not chk.ok(box is not None,
                  "the eight authored levels render differently somewhere "
                  "(the cell's own screen box)"):
        return 1
    noise = png_region_changed_pixels(reloaded, control, box) or 0
    diffs = {lvl: png_region_changed_pixels(reloaded, refs[lvl], box)
             for lvl in refs}
    restored_diffs = {lvl: png_region_changed_pixels(restored, refs[lvl], box)
                      for lvl in refs}
    matched = min(diffs, key=lambda k: (diffs[k] is None, diffs[k] or 0))
    print(f"  [note] cell box {box}, noise floor {noise} px, "
          f"per-level differences {diffs}")
    chk.ok(matched == level,
           f"the reloaded cell renders closest to authored level {level} "
           f"(matched level {matched})")
    chk.ok(diffs[level] is not None and diffs[level] <= noise,
           f"the reloaded cell's box matches its own level within the noise "
           f"floor ({diffs[level]} <= {noise} px)")
    chk.ok(all(d is not None and d > diffs[level]
               for k, d in diffs.items() if k != level),
           "every other level renders measurably differently in that box")
    chk.ok(min(restored_diffs, key=lambda k: restored_diffs[k] or 0) == level
           and (restored_diffs[level] or 0) <= noise,
           f"re-authoring the reloaded value renders the same mask again "
           f"({restored_diffs})")

    crop(reloaded, box, str(out / "crop_reloaded.png"))
    for lvl, path in refs.items():
        crop(path, box, str(out / f"crop_level_{lvl}.png"))
    manifest = {
        "page": PAGE,
        "tile": list(tile),
        "type": cell["type"],
        "surfaceUnits": units,
        "fluidLevel": level,
        "fluidSurf": ceiling,
        "renderSurfaceZ": units / 8,
        "terrainZ": cell["terrainZ"],
        "identities": {"ceiling": ceiling == ceil_div8(units),
                       "level": level == level_of(units)},
        "matchedMaskLevel": matched,
        "cellBox": list(box),
        "noiseFloorPx": noise,
        "perLevelDiffPx": diffs,
        "restoredPerLevelDiffPx": restored_diffs,
        "frames": {"reloaded": "reloaded.png", "control": "reloaded_control.png",
                   "restored": "restored.png",
                   "levels": {lvl: f"level_{lvl}.png" for lvl in refs}},
        "scenario": scenario,
        "failedChecks": chk.failed,
    }
    (out / "manifest.json").write_text(json.dumps(manifest, indent=2) + "\n")
    print(f"evidence: {out / 'manifest.json'}")

    print()
    if chk.failed:
        print(f"FAILED: {chk.failed} check(s)")
        return 1
    print(f"PASSED: the reloaded partial cell renders with level mask {level}")
    return 0


if __name__ == "__main__":
    sys.exit(main())
