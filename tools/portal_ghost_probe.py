#!/usr/bin/env python3
"""Offscreen portal-ghost visual probe (#778).

GPU-backed screenshot verification that the starting-portal build-tool
ghost visibly reflects `building.canPlaceAt`'s location-exclusion
verdict — the visual complement to the engine-logic coverage in
`tools/portal_location_probe.py` and the pure "Portal location
exclusion" Hspec group (test-headless/Test/Headless/Building/Placement.hs).

Two phases:

  1. `--headless` (no GPU): generate a real world, find a real
     `ruin_small` from the world-gen overlay, load its chunk so it
     stamps + spawns content, and save it — the fixture the GPU phase
     loads.
  2. `--offscreen` (GPU on, window off): boot fresh, load defs, load the
     save via `scripts.main_menu.loadAndShowSave` — the SAME production
     path the "Load Game" menu button uses — to reach the real in-game
     HUD/world_view, then:

     a. Direct ghost-tint verification (position-controlled, so it
        isolates the tint from mouse/terrain variability):
        `building.setGhost` at the SAME screen position with
        valid=true then valid=false, and confirm the invalid
        screenshot's ghost pixels shift red relative to the valid one
        — matches Building.Render.ghostTint's Vec4 1 1 1 0.6 (neutral)
        vs Vec4 1 0.4 0.4 0.6 (red-dominant).
     b. End-to-end pipeline: hover the REAL mouse (input.moveMouse)
        over the ruin's anchor (inside its bounds) vs. an adjacent
        tile (outside its bounds), driving buildTool.update's REAL
        building.canPlaceAt call, and confirm the two resulting
        screenshots differ.
     c. Click the invalid (inside-bounds) position and confirm no
        acolyte_portal was placed.

Needs a GPU (Vulkan device) — manual-only, never CI-gated, same as
tools/offscreen_probe.py.

Usage:
  python3 tools/portal_ghost_probe.py
  python3 tools/portal_ghost_probe.py --seed 42 --size 48 --port 9419
"""
from __future__ import annotations

import argparse
import os
import tempfile
import time

from probelib import (boot, capture_request_id, poll_until, quit_engine, send,
                      send_json, wait_load_published)
from location_content_probe import (load_defs, gen_world, placed_ready,
                                    wait_floor, make_isolated_root,
                                    remove_isolated_root, save_and_wait)
from offscreen_probe import screenshot, png_stats, png_differs
from run_probes import FailureEmitter   # durable failure records (#1982)

LOG_PREP = "/tmp/portal_ghost_prep_engine.log"
LOG_GPU = "/tmp/portal_ghost_gpu_engine.log"
#: #1982 — this run's durable failure records, built at import so the
#: offset each carries is measured from the probe's own start.
FAILURE = FailureEmitter("portal_ghost_probe")
#: Base of the per-invocation save slot. The run's own random token is
#: appended so two runs cannot collide and no slot a normal `cabal run`
#: could reach is ever created (#1620 requirement 5).
SAVE_BASE = "portal_ghost_probe"
PORTAL = "acolyte_portal"
#: Prefix of this invocation's throwaway resource root. The random
#: suffix mkdtemp appends to it is also the per-invocation save-slot
#: token, so the root and the slots it holds share one identity.
ROOT_PREFIX = "portal_ghost_probe_root_"

failures: list[str] = []


def check(name: str, ok: bool, detail: str = "") -> bool:
    print(f"  [{'PASS' if ok else 'FAIL'}] {name}"
          + (f" — {detail}" if detail and not ok else ""))
    if not ok:
        failures.append(name if not detail else f"{name} — {detail}")
    return ok


# --------------------------------------------------------------------------
# Phase 1: headless prep — a real ruin_small, saved to disk.
# --------------------------------------------------------------------------
def prepare_save(seed: int, size: int, port: int, root: str,
                 slot: str) -> dict:
    """Build the fixture the GPU phase reads, in THIS invocation's own
    resource root, and return only once the save reached its OWN
    request's terminal successful status. The GPU process below is a
    DIFFERENT process reading this slot; before #1620 a one-second sleep
    stood in for that, so a save still writing (or refused outright) left
    the reader loading whatever generation of a fixed slot happened to be
    on disk."""
    proc = boot(port, log=LOG_PREP, args=["--resource-root", root])
    try:
        load_defs(port)
        gen_world(port, "pw", seed, size)
        located = placed_ready(port)
        ruins = [e for e in located if e.get("id") == "ruin_small" and "bounds" in e]
        if not ruins:
            return {}
        ruin = ruins[0]
        cx, cy = ruin["cx"], ruin["cy"]
        send(port, f"return world.loadChunksInRegion({cx-1},{cy-1},{cx+1},{cy+1})")
        send(port, "return world.waitForChunks(60)", timeout=65)
        wait_floor(port, ruin["gx"], ruin["gy"])
        save_failures: list[str] = []
        if not save_and_wait(port, "pw", slot, save_failures, log=LOG_PREP):
            check("phase 1 fixture save reached its own terminal success",
                  False, "; ".join(save_failures))
            return {}
        return ruin
    finally:
        quit_engine(port, proc)


# --------------------------------------------------------------------------
# Phase 2 helpers
# --------------------------------------------------------------------------
def in_world_view(port: int) -> bool:
    r = send(port, "return require('scripts.ui_manager').currentMenu")
    return r.strip('"') == "world_view"


def arm_portal_placement(port: int) -> None:
    send(port,
         "require('scripts.build_tool').enterPlacement("
         "{kind='building', def='" + PORTAL + "', isStarting=true}); "
         "return 'ok'")


def hover(port: int, x: int, y: int) -> None:
    send(port, f"return input.moveMouse({x}, {y})")
    # buildTool.update() runs on the next engine tick; give it a couple
    # of frames before the ghost/render state settles.
    time.sleep(0.3)


def set_ghost(port: int, gx: int, gy: int, valid: bool) -> None:
    send(port, f"return building.setGhost('{PORTAL}', {gx}, {gy}, "
               f"{'true' if valid else 'false'})")
    time.sleep(0.2)


def building_count(port: int, def_name: str) -> int:
    r = send(port, "return building.list()")
    return r.count(def_name)


def center_on_tile(port: int, target_gx: int, target_gy: int,
                    screen_x: int, screen_y: int, tries: int = 6):
    """camera.goToTile centers the CAMERA on (gx, gy), but the tile that
    actually ends up under a given screen pixel depends on the isometric
    projection AND that tile's own terrain height, so it's off by a few
    tiles in practice (verified empirically — never a fixed constant).
    Iteratively correct the goToTile target by the observed
    world.pickTile(screen_x, screen_y) error until it converges on the
    exact target tile (or gives up after `tries`). Returns the tile
    actually resolved at (screen_x, screen_y) — use THIS, not
    (target_gx, target_gy), for anything keyed off "what's under the
    mouse" (setGhost/canPlaceAt/click all key off the SAME pickTile
    call under the hood, so matching it exactly is what matters, not
    matching the original target)."""
    gx, gy = target_gx, target_gy
    resolved = None
    for _ in range(tries):
        send(port, f"camera.goToTile({gx}, {gy}); return 'ok'")
        time.sleep(0.4)
        picked = send_json(port, f"return {{world.pickTile({screen_x}, {screen_y})}}")
        if not picked:
            continue
        resolved = (picked[0], picked[1])
        if resolved == (target_gx, target_gy):
            break
        gx += target_gx - resolved[0]
        gy += target_gy - resolved[1]
    return resolved


def region_avg_rgb(path: str, mask):
    from PIL import Image, ImageStat
    with Image.open(path) as im:
        stat = ImageStat.Stat(im.convert("RGB"), mask=mask)
        return tuple(stat.mean)


def diff_mask(path_a: str, path_b: str, threshold: int = 0):
    """Grayscale mask of pixels that changed between the two screenshots
    (255 = changed) — the region the two ghost states actually occupy.
    Threshold 0 (any difference counts): the ghost is a small, mostly
    dark sprite, so a magnitude threshold tuned for a bright object
    would miss its (real, but low-magnitude on dark pixels) tint shift
    — verified against a live run, where the true per-channel delta
    was ~2-8 levels, comfortably under any threshold above 1."""
    from PIL import Image, ImageChops
    with Image.open(path_a) as a, Image.open(path_b) as b:
        diff = ImageChops.difference(a.convert("RGB"), b.convert("RGB"))
        return diff.convert("L").point(lambda p: 255 if p > threshold else 0)


def main() -> int:
    ap = argparse.ArgumentParser(description=__doc__)
    ap.add_argument("--seed", type=int, default=42)
    ap.add_argument("--size", type=int, default=48)
    ap.add_argument("--port", type=int, default=9419)
    ap.add_argument("--win-size", default="1024x576")
    args = ap.parse_args()

    # Screenshots are kept for inspection (they are not save artifacts);
    # the resource root holding this run's save IS one, so it lives in
    # its own tree and is removed unconditionally below (#1620
    # requirement 6). BOTH engines of this invocation — the headless prep
    # and the offscreen reader — get that same root.
    shots = tempfile.mkdtemp(prefix="portal_ghost_probe_")
    base = tempfile.mkdtemp(prefix=ROOT_PREFIX)
    # The WHOLE random suffix, not the text after the last underscore:
    # mkdtemp's alphabet includes '_', so splitting on it can throw most
    # of the entropy away (and can leave nothing at all).
    token = os.path.basename(base)[len(ROOT_PREFIX):]
    try:
        rc = run(args, make_isolated_root(base), f"{SAVE_BASE}_{token}", shots)
    finally:
        # Reported, never swallowed, and reported even when `run` is
        # leaving by an exception (boot() exits the process on a dead
        # engine) — a root that survived is exactly the artifact #1620
        # requirement 6 forbids.
        leftover = remove_isolated_root(base)
        if leftover:
            FAILURE.check(leftover)
    return 1 if leftover else rc


def run(args, root: str, slot: str, shots: str) -> int:
    win_w, win_h = (int(v) for v in args.win_size.lower().split("x"))
    cx0, cy0 = win_w // 2, win_h // 2

    print(f"== phase 1: headless prep (seed {args.seed}, size {args.size}) ==")
    ruin = prepare_save(args.seed, args.size, args.port, root, slot)
    if not ruin:
        if failures:
            # The save was refused or never completed; prepare_save already
            # recorded it, and the GPU reader deliberately never boots.
            return report(shots)
        return report_prep_setup_failure()
    gx, gy = ruin["gx"], ruin["gy"]
    bounds = ruin["bounds"]
    print(f"  ruin at ({gx},{gy}), bounds {bounds}, saved as '{slot}'")

    print(f"== phase 2: offscreen GPU visual check (port {args.port}) ==")
    proc = boot(args.port, mode=("--offscreen",),
                args=["--size", args.win_size, "--resource-root", root],
                log=LOG_GPU, label="offscreen engine")
    try:
        load_defs(args.port)
        # The PRODUCTION load path is kept exactly as the "Load Game"
        # menu button uses it. It calls engine.loadSave internally and
        # returns nothing, so acceptance is read back off
        # mainMenu.lastError, and completion is tied to THAT load's own
        # request id — the loading screen's own poll correlates with no
        # particular transaction (#1620 requirement 4).
        err = send(args.port,
                   "local mm = require('scripts.main_menu'); "
                   f"mm.loadAndShowSave('{slot}'); "
                   "return mm.lastError or 'accepted'").strip().strip('"')
        if not check("main_menu.loadAndShowSave accepted the load",
                     err == "accepted", err):
            quit_engine(args.port, proc)
            return report(shots)
        load_id = capture_request_id(args.port, "return engine.getLoadStatus()")
        if not check("engine.getLoadStatus() reports this load's request id",
                     load_id is not None):
            quit_engine(args.port, proc)
            return report(shots)
        published, load_status = wait_load_published(args.port, 120.0,
                                                     request_id=load_id)
        if not check(f"load request {load_id} reached LoadPublished",
                     published, f"{load_status}"):
            quit_engine(args.port, proc)
            return report(shots)
        reached = poll_until(90.0, lambda: in_world_view(args.port))
        if not check("reached world_view after loading the save", bool(reached)):
            quit_engine(args.port, proc)
            return report(shots)

        # Re-derive the ruin's bounds on the loaded page — issue #763
        # replaced the old "every load lands under main_world" remap
        # with saved-page-id-preserving replacement, so the loaded page
        # is still 'pw' (the id it was saved under above), not
        # 'main_world'. Query the live active page rather than
        # hardcoding either id, so this stays correct regardless: only
        # its CONTENT (the ruin overlay entry), not its page id, is what
        # the save/load round-trip must preserve unchanged.
        active_page = send(args.port, "return world.getActiveWorldId()").strip().strip('"')
        located = send_json(args.port,
            f"return world.listPlacedLocations('{active_page}')")
        located = located if isinstance(located, list) else []
        ruins2 = [e for e in located if e.get("id") == "ruin_small" and "bounds" in e]
        if not check("ruin_small still known from the overlay after load",
                     bool(ruins2)):
            quit_engine(args.port, proc)
            return report(shots)
        ruin2 = min(ruins2, key=lambda e: (e["gx"] - gx) ** 2 + (e["gy"] - gy) ** 2)
        gx, gy = ruin2["gx"], ruin2["gy"]
        bounds = ruin2["bounds"]

        cx_chunk, cy_chunk = ruin2["cx"], ruin2["cy"]
        send(args.port,
             f"return world.loadChunksInRegion({cx_chunk-1},{cy_chunk-1},"
             f"{cx_chunk+1},{cy_chunk+1})")
        send(args.port, "return world.waitForChunks(60)", timeout=65)

        # Converge the camera so the ruin's anchor (deep inside bounds)
        # sits exactly under the mouse at screen centre — camera.goToTile
        # centers the CAMERA, not necessarily the tile under a given
        # pixel (isometric projection + per-tile terrain height shift
        # it), so this is resolved empirically rather than assumed.
        inside = center_on_tile(args.port, gx, gy, cx0, cy0)
        if not check(f"camera converges on the ruin anchor ({gx},{gy}) at "
                     f"screen centre", inside == (gx, gy), f"got {inside}"):
            quit_engine(args.port, proc)
            return report(shots)
        igx, igy = inside

        # -- (a) direct ghost-tint verification, position held fixed at
        #    the now-confirmed on-screen inside-bounds tile. --
        set_ghost(args.port, igx, igy, True)
        shot_valid_tint = os.path.join(shots, "tint_valid.png")
        check("valid-tint screenshot answers",
              screenshot(args.port, shot_valid_tint))
        set_ghost(args.port, igx, igy, False)
        shot_invalid_tint = os.path.join(shots, "tint_invalid.png")
        check("invalid-tint screenshot answers",
              screenshot(args.port, shot_invalid_tint))

        if png_stats(shot_valid_tint) and png_stats(shot_invalid_tint):
            mask = diff_mask(shot_valid_tint, shot_invalid_tint)
            nonzero = mask.getbbox()
            if check("the two tint states render a visible ghost difference",
                     bool(nonzero)):
                vr, vg, vb = region_avg_rgb(shot_valid_tint, mask)
                ir, ig, ib = region_avg_rgb(shot_invalid_tint, mask)
                check("valid ghost tint is neutral (R≈G≈B in the "
                      "changed region)",
                      abs(vr - vg) < 20 and abs(vr - vb) < 20,
                      f"valid RGB=({vr:.1f},{vg:.1f},{vb:.1f})")
                check("invalid ghost tint is red-dominant vs. the valid tint "
                      "(G/R and B/R ratios drop)",
                      (ig / max(ir, 1)) < (vg / max(vr, 1)) - 0.05
                      and (ib / max(ir, 1)) < (vb / max(vr, 1)) - 0.05,
                      f"valid RGB=({vr:.1f},{vg:.1f},{vb:.1f}) "
                      f"invalid RGB=({ir:.1f},{ig:.1f},{ib:.1f})")
        send(args.port, "return building.clearGhost()")

        # -- (b) end-to-end pipeline: real mouse -> buildTool.update ->
        #    canPlaceAt -> setGhost, at the confirmed inside-bounds tile. --
        arm_portal_placement(args.port)
        hover(args.port, cx0, cy0)
        shot_inside = os.path.join(shots, "pipeline_inside.png")
        check("pipeline: inside-bounds screenshot answers",
              screenshot(args.port, shot_inside))

        # -- (c) clicking the invalid (inside-bounds) position must not
        #    place the portal — same tile, same mouse position, no camera
        #    move in between. --
        before = building_count(args.port, PORTAL)
        send(args.port, f"return input.click({cx0}, {cy0})")
        time.sleep(0.5)
        after = building_count(args.port, PORTAL)
        check("clicking the invalid (inside-bounds) position does not "
              "place the portal",
              after == before, f"count before={before} after={after}")

        # -- (b, cont'd) an adjacent OUTSIDE-bounds tile, again confirmed
        #    under the mouse rather than assumed. --
        outside_target = (bounds["max_x"] + 6, gy)
        outside = center_on_tile(args.port, outside_target[0], outside_target[1],
                                  cx0, cy0)
        outside_ok = outside is not None and not (
            bounds["min_x"] <= outside[0] <= bounds["max_x"]
            and bounds["min_y"] <= outside[1] <= bounds["max_y"])
        if check("camera converges on a genuinely outside-bounds tile",
                 outside_ok, f"got {outside}, bounds {bounds}"):
            hover(args.port, cx0, cy0)
            shot_outside = os.path.join(shots, "pipeline_outside.png")
            if check("pipeline: outside-bounds screenshot answers",
                     screenshot(args.port, shot_outside)):
                check("moving the ghost across the location boundary "
                      "changes the displayed state",
                      png_differs(shot_inside, shot_outside,
                                  min_fraction=0.0002))
    finally:
        quit_engine(args.port, proc)

    return report(shots)


def report_prep_setup_failure() -> int:
    """The one terminal exit that does NOT go through `report` below.

    `prepare_save` returning no ruin without having recorded a failure of
    its own means the fixture never materialised — a SETUP failure in
    #1575's vocabulary ("try another seed"), not a product one. It used to
    print straight to stderr and return, which is precisely the #1982 loss
    the rest of this probe was repaired for: the runner merges stderr into
    a block-buffered stdout pipe, so this line overtook the buffered prep
    output and landed above the retained `--tail`, and `report` -- the only
    thing emitting durable records -- was never reached.

    Only the prep log is named: phase 2 has not booted, so `LOG_GPU` names
    either nothing or a previous run's file.
    """
    FAILURE.setup("phase 1 (headless prep): no ruin_small with resolvable "
                  "bounds placed")
    FAILURE.context_log(LOG_PREP, label="prep engine log")
    return 1


def report(shots: str) -> int:
    print(f"\nscreenshots kept in {shots}")
    print("-" * 56)
    if failures:
        # Durable records rather than the unflushed stderr print this was
        # (#1982): `run_probes.py` merges this probe's stderr into a
        # block-buffered stdout pipe and prints only its last 25 lines, so
        # a printed `FAIL:` overtook the buffered checks and landed above
        # the retained tail. These are read back from the COMPLETE
        # capture. Both engines of this invocation are named: a ghost that
        # never rendered and one that rendered wrongly are the same check
        # and different logs. The screenshots this probe deliberately
        # KEEPS are named here too — its artifact policy is unchanged.
        FAILURE.report(failures)
        FAILURE.context_log(LOG_PREP, label="prep engine log")
        FAILURE.context_log(LOG_GPU, label="offscreen engine log")
        FAILURE.context("screenshots", shots)
        return 1
    print("ALL CHECKS PASSED")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
