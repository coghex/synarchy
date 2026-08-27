#!/usr/bin/env python3
"""Structure wall rotation — the #1712 visual gate.

A wall's stored slot is its PHYSICAL world edge and never moves, but the
screen position that edge occupies does. This probe boots the engine
--offscreen (full Vulkan render, NO window), stamps ONE explicit scene,
and captures it at all four camera facings so the wall art, the terrain
interleave and the billboard lift can be eyeballed at every orientation.

The scene is deliberate, not incidental:

  * a room whose four sides each carry a DIFFERENT authored wall slot,
    with a corner post at every vertex — symmetric geometry would look
    right under a wrong rotation,
  * a HIGH terrain rim on two sides, so the #415/#417 per-strip terrain
    interleave has something to resolve against at every facing, and
  * flora BILLBOARDS immediately outside all four walls, so the #418
    lift has something to lift at every facing.

What it asserts automatically is the pipeline, not the pixels: four
non-blank captures at the requested size, each DIFFERENT from the other
three (a facing that failed to re-render, or art that did not rotate,
collapses that), the room still readable back off the engine afterwards,
and a clean shutdown on success AND on failure. Judging that the art
reads correctly is the human's job — the four PNGs are the deliverable,
and their paths are printed at the end. The exact rotation arithmetic is
proven separately and exhaustively by the pure hspec groups
"World.Render.StructureRotation" and "World.Render.FrontWallLift".

Needs a GPU (Vulkan device) — manual-only, never CI-gated.

Usage: python3 tools/structure_rotation_probe.py [--port 9518]
       [--size 1280x720] [--out DIR] [--keep-open]
"""
from __future__ import annotations

import argparse
import hashlib
import os
import sys
import time

sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
from probelib import (boot, camera_state, pin_camera_to_tile, poll_until,
                      quit_engine, send, send_json, set_paused, viewport)

failures = 0

FACINGS = ["facesouth", "facewest", "facenorth", "faceeast"]


def check(ok: bool, label: str, detail: str = "") -> bool:
    global failures
    if ok:
        print(f"  PASS  {label}")
    else:
        failures += 1
        print(f"  FAIL  {label}{(' — ' + detail) if detail else ''}")
    return ok


def structures_call(port: int, lua: str, timeout: float = 10.0):
    """Reach scripts/structures.lua through ``package.loaded``.

    The debug console sandboxes ``_G``, so there is no bare ``structures``
    global; ``package.loaded`` is reachable and holds every game module
    the boot loader required.
    """
    return send(port, "local S = package.loaded['scripts.structures'];"
                      f" if not S then return 'no-structures' end; {lua}",
                timeout=timeout)


def active_page(port: int) -> str | None:
    """The active world page id, or None while there is not one yet.

    NEVER `tostring(world.getActiveWorldId())`: that turns "no world" into
    the truthy string "nil", which a poll accepts — and every later
    world-scoped command then targets a page that does not exist and is
    silently dropped, leaving a scene that still screenshots fine.
    """
    got = send_json(port, "local id = world.getActiveWorldId();"
                          " return id and {page = tostring(id)} or nil")
    if not isinstance(got, dict):
        return None
    page = str(got.get("page", "")).strip()
    return page if page and page != "nil" else None


def wait_content_loaded(port: int, seconds: float = 90.0) -> bool:
    """READY precedes the startup loader: unit/building defs are EMPTY for
    the first ~10-20 s while the menu phase loads content. Opening the
    arena before they fill silently produces an empty world."""
    return poll_until(seconds, lambda: send_json(
        port, "return {units = #unit.listDefs(),"
              " buildings = #building.listDefs()}") not in (None, {})
        and (lambda g: g.get("units", 0) > 0 and g.get("buildings", 0) > 0)(
            send_json(port, "return {units = #unit.listDefs(),"
                            " buildings = #building.listDefs()}")))


def stamp_scene(port: int, ax: int, ay: int, page: str) -> None:
    """A 5x5 room with one authored wall slot per side, a post at every
    corner, a 3-high terrain rim on two sides, and flora just outside all
    four walls."""
    lo, hi = 0, 4
    lua = [
        "local S = package.loaded['scripts.structures']",
        "if not S then return 'no-structures' end",
        f"local ax, ay = {ax}, {ay}",
        # Floor first: posts are gated on one, and walls take its z.
        f"for dx = {lo}, {hi} do for dy = {lo}, {hi} do"
        "   S.floor(ax + dx, ay + dy) end end",
        # One authored slot per side, each on that side's OUTWARD edge.
        f"for d = {lo}, {hi} do",
        f"  S.wall(ax + d, ay + {lo}, 'ne')",   # low gy  -> NE edge
        f"  S.wall(ax + d, ay + {hi}, 'sw')",   # high gy -> SW edge
        f"  S.wall(ax + {lo}, ay + d, 'nw')",   # low gx  -> NW edge
        f"  S.wall(ax + {hi}, ay + d, 'se')",   # high gx -> SE edge
        "end",
        # A post at each of the room's four corner tiles' outward vertex.
        f"S.post(ax + {lo}, ay + {lo}, 'n')",
        f"S.post(ax + {hi}, ay + {lo}, 'e')",
        f"S.post(ax + {hi}, ay + {hi}, 's')",
        f"S.post(ax + {lo}, ay + {hi}, 'w')",
        "return 'ok'",
    ]
    send(port, " ".join(lua), timeout=30.0)

    # Terrain rim: two tiles clear of the room on the -gy and +gx sides,
    # so a rim sits in front of the room at two of the four facings and
    # behind it at the other two. Two levels puts its top ONE above the
    # room's floor — high enough to occlude a front wall's far strips
    # (#415's dug-room rim) and still inside the camera's z-slice. The
    # arena's own material (loam, id 56) by NUMBER: world.addTile takes a
    # registry name or an id, and the arena registers no name a probe can
    # rely on.
    send(port, "local page = '%s' "
               "for d = -2, 6 do "
               "  for h = 1, 2 do "
               "    world.addTile(page, %d + d, %d - 2, 56) "
               "    world.addTile(page, %d + 6, %d + d, 56) "
               "  end "
               "end return 'ok'" % (page, ax, ay, ax, ay), timeout=30.0)

    # Flora billboards immediately outside all four walls. Species ids are
    # allocated in content-load order, so a small spread guarantees at
    # least one tall tree whichever file loaded first.
    send(port, "local page = '%s' local n = 0 "
               "for d = 0, 4 do "
               "  local fid = 1 + (d %% 6) "
               "  local z = (world.getTerrainAt(%d + d, %d - 1) or 0) "
               "  world.setVegAt(page, %d + d, %d - 1, z, fid) "
               "  z = (world.getTerrainAt(%d + d, %d + 5) or 0) "
               "  world.setVegAt(page, %d + d, %d + 5, z, fid) "
               "  z = (world.getTerrainAt(%d - 1, %d + d) or 0) "
               "  world.setVegAt(page, %d - 1, %d + d, z, fid) "
               "  z = (world.getTerrainAt(%d + 5, %d + d) or 0) "
               "  world.setVegAt(page, %d + 5, %d + d, z, fid) "
               "  n = n + 4 "
               "end return n" % ((page,) + (ax, ay) * 8), timeout=30.0)


def capture(port: int, path: str, settle: float = 1.2) -> bytes:
    """Let the render thread produce fresh frames, then capture. The
    offscreen loop paces on a fixed ~60 fps sleep, so a short settle is
    several frames — a capture taken the instant a rotation is queued can
    still show the previous facing."""
    time.sleep(settle)
    send(port, f"return debug.captureScreenshot('{path}')", timeout=30.0)
    if not os.path.exists(path):
        return b""
    with open(path, "rb") as fh:
        return fh.read()


def main() -> int:
    ap = argparse.ArgumentParser()
    ap.add_argument("--port", type=int, default=9518)
    ap.add_argument("--size", default="1280x720")
    ap.add_argument("--out", default="/tmp/structure_rotation")
    ap.add_argument("--keep-open", action="store_true",
                    help="leave the engine running after the captures")
    args = ap.parse_args()

    os.makedirs(args.out, exist_ok=True)
    proc = boot(args.port, args=["--size", args.size], label="offscreen",
                mode=("--offscreen",))
    shots: dict[str, bytes] = {}
    try:
        print("phase 1: content load")
        check(wait_content_loaded(args.port), "unit/building defs populated")

        print("phase 2: gameplay view on the flat arena")
        send(args.port, "package.loaded['scripts.ui_manager'].onOpenArena();"
                        " return 'ok'", timeout=20.0)
        page = poll_until(90.0, lambda: active_page(args.port))
        if not check(bool(page), "a real arena page is active",
                     f"world.getActiveWorldId() never named one (got {page!r})"):
            return 1
        vp = viewport(args.port, fallback=(1280, 720))
        cx = int(vp.get("win_w", 1280)) // 2
        cy = int(vp.get("win_h", 720)) // 2
        # world.pickTile returns TWO numbers (gx, gy), not a table.
        pick = (f"local gx, gy = world.pickTile({cx}, {cy});"
                " return gx and {gx = gx, gy = gy} or nil")
        check(poll_until(90.0, lambda: send_json(args.port, pick)),
              "the arena renders and picks")

        print("phase 3: freeze the frame, then stamp the scene")
        set_paused(args.port, True)
        send(args.port, "world.setTimeScale(0); world.setSunAngle(0.5);"
                        " return 'ok'")
        anchor = poll_until(30.0, lambda: send_json(args.port, pick))
        if not isinstance(anchor, dict):
            check(False, "picked an anchor tile", "world.pickTile returned nil")
            return 1
        ax, ay = int(anchor["gx"]) - 2, int(anchor["gy"]) - 2
        print(f"        anchor tile ({ax}, {ay}) on page '{page}'")
        stamp_scene(args.port, ax, ay, page)
        # Terrain and vegetation edits are QUEUED world commands, so the
        # read-backs below have to wait for the world thread to apply them.
        poll_until(30.0, lambda: (send_json(
            args.port, "return {z = (world.getTerrainAt(%d, %d))}" % (ax + 2, ay - 2))
            or {}).get("z", 0) >= 2)
        rim = send_json(args.port,
                        "local a = (world.getTerrainAt(%d, %d));"
                        " local b = (world.getTerrainAt(%d, %d));"
                        " return {a = a, b = b}" % (ax + 2, ay - 2, ax + 6, ay + 2))
        check(isinstance(rim, dict)
              and (rim.get("a") or 0) >= 2 and (rim.get("b") or 0) >= 2,
              "the terrain rim rose on both sides of the room",
              f"world.getTerrainAt on the two rim tiles = {rim!r}")
        veg = send_json(args.port,
                        "local n = 0;"
                        " for d = 0, 4 do"
                        "   if (world.getVegAt(%d + d, %d - 1) or 0) > 0 then n = n + 1 end;"
                        "   if (world.getVegAt(%d + d, %d + 5) or 0) > 0 then n = n + 1 end;"
                        "   if (world.getVegAt(%d - 1, %d + d) or 0) > 0 then n = n + 1 end;"
                        "   if (world.getVegAt(%d + 5, %d + d) or 0) > 0 then n = n + 1 end;"
                        " end return n" % ((ax, ay) * 4))
        check(isinstance(veg, int) and veg == 20,
              "a billboard stands outside all four walls",
              f"{veg!r} of 20 flora tiles took")
        placed = send_json(args.port, "return structure.count()")
        check(isinstance(placed, int) and placed >= 45,
              "the room stamped (floors + 4 wall sides + posts)",
              f"structure.count() = {placed!r}")
        # Every side must really carry its own authored slot.
        for slot, gx, gy in (("wall_ne", ax + 2, ay), ("wall_sw", ax + 2, ay + 4),
                             ("wall_nw", ax, ay + 2), ("wall_se", ax + 4, ay + 2)):
            got = send(args.port, f"return tostring(structure.hasAt({gx}, {gy},"
                                  f" '{slot}'))")
            check((got or "").strip() == "true", f"{slot} present at ({gx},{gy})")

        print("phase 4: capture all four facings")
        # The room's own floor z — world.getTerrainAt returns several
        # values, structure.floorZAt exactly one.
        got_z = send_json(args.port,
                          f"return structure.floorZAt({ax + 2}, {ay + 2})")
        # One level ABOVE the floor, so the rim's top is inside the slice
        # and can occlude rather than being culled away above it.
        z = (int(got_z) + 1) if isinstance(got_z, int) else 26
        pin_camera_to_tile(args.port, ax + 2, ay + 2, z)
        # Zoom is quantised and INVERTED: 0.25 is the closest step, which
        # is what makes the wall art legible in the capture.
        send(args.port, "camera.setZoom(0.25); return 'ok'")
        for i, name in enumerate(FACINGS):
            if i > 0:
                # goToTile re-arms z-tracking, so the pin has to follow
                # every rotation or the room slides off the viewport.
                send(args.port, "camera.rotateCW(); return 'ok'")
                pin_camera_to_tile(args.port, ax + 2, ay + 2, z)
                send(args.port, "camera.setZoom(0.25); return 'ok'")
            facing = send(args.port, "return tostring(camera.getFacing())")
            path = os.path.join(args.out, f"wall_{name}.png")
            shots[name] = capture(args.port, path)
            check(len(shots[name]) > 4096,
                  f"{name}: captured a non-trivial PNG (facing={(facing or '').strip()})",
                  f"{len(shots[name])} bytes")

        print("phase 5: the four frames differ")
        digests = {n: hashlib.sha256(b).hexdigest() for n, b in shots.items() if b}
        check(len(set(digests.values())) == len(digests),
              "every facing rendered a distinct frame",
              f"digests: { {n: d[:8] for n, d in digests.items()} }")

        cam = camera_state(args.port)
        check(cam.get("zTracking") is False,
              "the camera stayed pinned across all four rotations",
              f"camera: {cam}")
    finally:
        if not args.keep_open:
            quit_engine(args.port, proc)

    print()
    for name in FACINGS:
        print(f"  {os.path.join(args.out, f'wall_{name}.png')}")
    print()
    if failures:
        print(f"structure_rotation_probe: {failures} failure(s)")
        return 1
    print("structure_rotation_probe: all checks passed — eyeball the four "
          "PNGs above for the art, the terrain interleave and the billboards.")
    return 0


if __name__ == "__main__":
    sys.exit(main())
