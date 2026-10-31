#!/usr/bin/env python3
"""Wire facing capture set — the #2901 evidence.

Boots --offscreen (real Vulkan, no window), opens the flat arena, freezes
the clock and, for each of three scenes (a straight run, a corner and a
tee), captures the placed wire at all four camera facings. Wire art is
chosen from the SCREEN connections (Structure.Facing.screenWireShape), so
every scene must read as one continuous cable at every facing.

Besides the pictures, each capture records what the renderer actually
draws, read from the engine rather than restated: the stored (world)
shape of every tile, and `structure.drawnFacemap` — the facemap the render
pass lights it with, which names the DRAWN variant.

Every capture is saved whole and as a nearest-neighbour 4x crop around the
subject, under --out, with `captures.json` listing each file and what it
shows. Needs a GPU: manual only, never CI-gated.

Usage: python3 tools/wire_facing_capture.py --out <dir> [--port 9518]
       [--size 1280x720]
"""
from __future__ import annotations

import argparse
import json
import os
import sys

sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
from probelib import (boot, pin_camera_to_tile, poll_until, quit_engine, send,
                      send_json, set_paused, viewport)
from wire_gameplay_capture import (active_page, capture, crop4x, locate, shape,
                                   wait_content_loaded)

FACINGS = ["south", "west", "north", "east"]   # camera.rotateCW order

# Scene name -> tile offsets from the anchor, and what the scene is for.
SCENES = {
    "straight": ([(0, 0), (1, 0), (2, 0)],
                 "a three-tile straight run (end, straight, end)"),
    "corner": ([(0, 0), (1, 0), (0, 1)],
               "an L: a corner tile with an arm east and an arm south"),
    "tee": ([(0, 0), (-1, 0), (1, 0), (0, 1)],
            "a tee: a centre tile joined west, east and south"),
}


def drawn_face(port: int, x: int, y: int):
    """The facemap the render pass lights the tile's wire with, as a bare
    variant name: it identifies the variant DRAWN at the current facing."""
    got = send(port, f"return structure.drawnFacemap({x}, {y}, 'wire')")
    got = (got or "").strip().strip('"')
    if not got or got == "nil":
        return None
    return got.rsplit("/", 1)[-1].removesuffix(".png")


def main() -> int:
    ap = argparse.ArgumentParser()
    ap.add_argument("--port", type=int, default=9518)
    ap.add_argument("--size", default="1280x720")
    ap.add_argument("--out", required=True)
    args = ap.parse_args()
    os.makedirs(args.out, exist_ok=True)
    log: list[dict] = []

    proc = boot(args.port, args=["--size", args.size], label="offscreen",
                mode=("--offscreen",))
    try:
        if not wait_content_loaded(args.port):
            sys.exit("content never loaded")
        send(args.port, "package.loaded['scripts.ui_manager'].onOpenArena();"
                        " return 'ok'", timeout=20.0)
        page = poll_until(90.0, lambda: active_page(args.port))
        vp = viewport(args.port, fallback=(1280, 720))
        cx, cy = int(vp.get("win_w", 1280)) // 2, int(vp.get("win_h", 720)) // 2
        pick = (f"local gx, gy = world.pickTile({cx}, {cy});"
                " return gx and {gx = gx, gy = gy} or nil")
        anchor = poll_until(90.0, lambda: send_json(args.port, pick))
        ax, ay = int(anchor["gx"]), int(anchor["gy"])
        set_paused(args.port, True)
        send(args.port, "world.setTimeScale(0); return 'ok'")
        z = send_json(args.port, f"return (world.getTerrainAt({ax}, {ay}))")
        zz = (int(z) + 1) if isinstance(z, int) else 26
        send(args.port, f"world.setTime('{page}', 12, 0); return 'ok'")

        def home():
            pin_camera_to_tile(args.port, ax, ay, zz)

        # The camera starts at FaceSouth; turn it back after each scene.
        for name, (offsets, what) in SCENES.items():
            send(args.port, "structure.clearAll(); return 'ok'")
            tiles = [(ax + dx, ay + dy) for dx, dy in offsets]
            for tx, ty in tiles:
                send(args.port, f"require('scripts.wire').place({tx}, {ty});"
                                " return 'ok'")
            stored = {f"{tx},{ty}": shape(args.port, tx, ty) for tx, ty in tiles}
            print(f"{name}: stored world shapes {stored}")
            for fi, fname in enumerate(FACINGS):
                if fi:
                    send(args.port, "camera.rotateCW(); return 'ok'")
                home()
                drawn = {f"{tx},{ty}": drawn_face(args.port, tx, ty)
                         for tx, ty in tiles}
                path = os.path.join(args.out, f"{name}_{fname}.png")
                if not capture(args.port, path):
                    sys.exit(f"capture failed: {name} {fname}")
                from PIL import Image
                # Framed on the cable itself at EACH facing: the run's
                # screen position moves with the camera.
                centre = locate(Image.open(path).convert("RGB"), (cx, cy))
                crop = crop4x(path, centre, half=110, fixed_centre=True)
                log.append({"scene": name, "facing": fname,
                            "file": os.path.basename(path),
                            "crop": os.path.basename(crop),
                            "stored_shapes": stored, "drawn_facemaps": drawn,
                            "shows": f"{what}, camera facing {fname}"})
                print(f"  captured {name}_{fname}: drawn {drawn}")
            send(args.port, "camera.rotateCW(); return 'ok'")   # back to south
            home()
        with open(os.path.join(args.out, "captures.json"), "w") as fh:
            json.dump({"anchor": [ax, ay], "captures": log}, fh, indent=2)
        print(f"wrote {len(log)} captures to {args.out}")
        return 0
    finally:
        quit_engine(args.port, proc)


if __name__ == "__main__":
    sys.exit(main())
