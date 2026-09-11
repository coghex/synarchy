#!/usr/bin/env python3
"""Capture the DFL-1 flat-step fluid scene offscreen, for owner signoff (#2517).

Requirement 7 of #2517 asks the owner to accept the flat-step presentation
from a before/after scene showing a river or lake beside one-z and multi-z
drops, in-chunk and at a loaded chunk seam. Generated terrain does not
reliably contain that arrangement, so this scripts it: an arena page, a
flat plateau carved down in controlled whole-z steps, and freshwater laid
on the plateau. Boot and console use follow `tools/offscreen_probe.py`
(`probelib.boot` with `mode=("--offscreen",)`, `debug.captureScreenshot`).

Run it once per side, against each side's own engine binary::

    python3 tools/flat_fluid_scene.py --engine "$(cabal list-bin exe:synarchy)" \
        --out /tmp/flat_fluid_branch.png --port 9481

The two images are EVIDENCE FOR A HUMAN DECISION, not a byte comparison:
the arena re-randomises per-tile ground scatter on every chunk re-mesh, so
two runs of the SAME build already disagree on thousands of pixels. What
the owner is asked to judge is the fluid presentation — whether river and
lake tops read as flat whole-z steps and whether every drop, one z
included, shows a vertical edge.

Needs a GPU (a real Vulkan device). Manual-only, never CI-gated, and not
a probe: it asserts nothing and returns no verdict.
"""
from __future__ import annotations

import argparse
import os
import sys
import time

sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))

import probelib  # noqa: E402
from probelib import send, send_json  # noqa: E402

# The scene, as a west-to-east depth profile in whole z below the plateau.
#
# Index 0 is the anchor column; the run is 20 columns wide and is placed so
# that index 12 lands on a CHUNK BOUNDARY (chunks are 16 tiles). Reading
# east:
#
#   0-1    the lake plateau, level
#   2      a ONE-z step down            <- the case #2517 adds
#   3      a two-z step down
#   4      a three-z step down
#   5-11   level again
#   12     a ONE-z step down, exactly at the chunk seam
#   13-19  level
#
# Five bands carry fluid; one more band south of them is carved to the same
# depth but left DRY, so every column also shows a wet-to-dry one-z drop
# (a water surface sits one z above its own terrain). North of the fluid is
# uncarved ground, which rises away from the water and must show no side
# face at all.
DEPTH_PROFILE = [0, 0, -1, -3, -6, -6, -6, -6, -6, -6, -6, -6,
                 -7, -7, -7, -7, -7, -7, -7, -7]
SEAM_INDEX = 12          # this column starts a new chunk
FLUID_BANDS = (-2, 2)    # inclusive band range carrying fluid
DRY_SHELF_BAND = 3       # carved with them, deliberately left dry


def wait_for_defs(port: int, seconds: float = 60.0) -> bool:
    """READY precedes the startup loader; definitions fill ~10-20 s later."""
    deadline = time.time() + seconds
    while time.time() < deadline:
        units = send_json(port, "return #unit.listDefs()")
        if isinstance(units, (int, float)) and units > 0:
            return True
        time.sleep(0.5)
    return False


def open_arena(port: int) -> None:
    """Reach the gameplay view.

    `world.initArena` alone leaves the main menu up and renders nothing;
    the debug console sandboxes `_G`, but `package.loaded` holds every
    game module, so the UI's own arena entry point is reachable.
    """
    send(port, "package.loaded['scripts.ui_manager'].onOpenArena()",
         expect_result=False)
    send(port, "return world.waitForInit(120)", timeout=130.0)


def freeze(port: int) -> None:
    """Pause BEFORE placing, so nothing drifts between edit and capture."""
    send(port, "engine.setPaused(true)", expect_result=False)
    send(port, "world.setTimeScale(0)", expect_result=False)
    send(port, "world.setSunAngle(0.5)", expect_result=False)
    time.sleep(0.5)


def build_scene(port: int, page: str, ax: int, ay: int) -> dict:
    """Carve the profile and lay the freshwater on top of it.

    Sent as ONE Lua chunk rather than a few hundred console round trips:
    the edits then land as one batch, so the terrain re-meshes once and
    the frame is stable by the time the capture asks for it. The debug
    console evaluates ONE LINE per request — a chunk split across lines
    comes back as the continuation prompt `>` — so the statements below
    are semicolon-separated on a single line.
    """
    depths = ",".join(str(d) for d in DEPTH_PROFILE)
    b0, b1 = FLUID_BANDS
    parts = [
        f"local page,ax,ay='{page}',{ax},{ay}",
        f"local depth={{{depths}}}",
        f"local b0,b1,dry={b0},{b1},{DRY_SHELF_BAND}",
        "local base=world.getSurfaceAt(ax,ay)",
        "if not base then return nil end",
        # Carve the fluid bands and the dry shelf to the profile depth.
        "for band=b0,dry do for i=1,#depth do "
        "for z=base+depth[i]+1,base do "
        "world.setCell(page,ax+i-1,ay+band,z,'air') end end end",
        # Lake over the level plateau, river from the first step east.
        "for i=1,2 do for band=b0,b1 do "
        "world.setFluidTile(page,ax+i-1,ay+band,'water') end end",
        "for i=3,#depth do for band=b0,b1 do "
        "world.setFluidTile(page,ax+i-1,ay+band,'river') end end",
        "return {base=base,columns=#depth}",
    ]
    got = send_json(port, " ".join(parts), timeout=60.0)
    if not isinstance(got, dict):
        raise SystemExit(f"scene build gave no result: {got!r}")
    time.sleep(3.0)
    return got


def report_profile(port: int, ax: int, ay: int) -> None:
    """Read the built scene back off the engine, so the capture is not the
    only evidence that the steps are actually there."""
    parts = [
        f"local ax,ay={ax},{ay}",
        "local out={}",
        f"for i=1,{len(DEPTH_PROFILE)} do "
        "local s,t,ft,fs=world.getSurfaceAt(ax+i-1,ay) "
        "out[i]={terrain=t,fluid=fs,kind=ft} end",
        "return out",
    ]
    got = send_json(port, " ".join(parts), timeout=30.0)
    if not isinstance(got, list):
        print(f"profile read-back unavailable: {got!r}", file=sys.stderr)
        return
    print("column  terrainZ  fluidZ  kind")
    for i, row in enumerate(got):
        if not isinstance(row, dict):
            continue
        mark = "  <- chunk seam" if i == SEAM_INDEX else ""
        print(f"  {i:>2}      {row.get('terrain')!s:>6}  "
              f"{row.get('fluid')!s:>6}  {row.get('kind')!s:<6}{mark}")


def main() -> int:
    ap = argparse.ArgumentParser(description=__doc__)
    ap.add_argument("--port", type=int, default=9481)
    ap.add_argument("--size", default="1280x720")
    ap.add_argument("--out", required=True, help="screenshot path (PNG)")
    ap.add_argument("--engine", help="engine binary; else cabal run")
    ap.add_argument("--log", help="engine log path")
    ap.add_argument("--zoom", type=float, default=0.5,
                    help="camera zoom; quantised by the engine")
    args = ap.parse_args()

    if args.engine:
        os.environ["SYNARCHY_PROBE_ENGINE_EXE"] = args.engine

    out = os.path.abspath(args.out)
    proc = probelib.boot(args.port, log=args.log, mode=("--offscreen",),
                         args=["--size", args.size], label="flat-fluid scene")
    try:
        if not wait_for_defs(args.port):
            raise SystemExit("definitions never loaded")
        open_arena(args.port)
        freeze(args.port)

        # Anchor on whatever tile the engine reports at screen centre —
        # the arena's z-slice shifts the view, so fixed world coordinates
        # land off-screen. pickTile returns five values, so wrap it.
        pick = send_json(args.port,
                         "local gx,gy,z,page=world.pickTile(640,360) "
                         "return {x=gx,y=gy,z=z,page=page}")
        if not isinstance(pick, dict) or pick.get("x") is None:
            raise SystemExit(f"world.pickTile gave no tile: {pick!r}")
        ax, ay = int(pick["x"]), int(pick["y"])
        page = pick.get("page") or "arena"
        # Straddle a chunk boundary: chunks are 16 tiles, and the river
        # lane runs 15+ tiles east, so start it just short of one.
        # Put DEPTH_PROFILE's seam step exactly on a chunk boundary.
        ax = (ax // 16) * 16 + 16 - SEAM_INDEX
        print(f"anchor tile ({ax}, {ay}) on page {page!r}")

        info = build_scene(args.port, page, ax, ay)
        base = int(info.get("base", 0))
        print(f"plateau terrain z={base}, {info.get('columns')} columns, "
              f"seam at column {SEAM_INDEX}")
        report_profile(args.port, ax, ay)

        # `camera.goToTile` alone leaves z-tracking on, which pushes the
        # target ~25 levels off the bottom of the viewport (#1286). Pin the
        # slice to the LAKE's water surface (one z above the plateau
        # terrain), so the whole descent sits inside the rendered window.
        send(args.port, f"camera.setZoom({args.zoom})", expect_result=False)
        if not probelib.pin_camera_to_tile(args.port, ax + SEAM_INDEX - 4,
                                           ay, base + 1):
            print("warning: camera z-slice pin did not hold", file=sys.stderr)
        time.sleep(2.0)

        got = send_json(args.port, f"return debug.captureScreenshot('{out}')",
                        timeout=30.0)
        if not (isinstance(got, dict) and got.get("path") == out):
            raise SystemExit(f"captureScreenshot failed: {got!r}")
        print(f"wrote {out}")
        return 0
    finally:
        probelib.quit_engine(args.port, proc)


if __name__ == "__main__":
    raise SystemExit(main())
