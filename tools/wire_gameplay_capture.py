#!/usr/bin/env python3
"""Wire gameplay-scale capture set — the #2516 checkpoint-3 evidence.

Boots --offscreen (real Vulkan, no window), opens the flat arena, freezes
the clock and captures flat Wire at the camera's gameplay zoom:

  facings   a two-tile run at all four camera facings x three sun hours
  neighbour a lone wire tile, a second tile PAID and half built next to it,
            then that tile completing and re-capping its neighbour's shape
  teardown  one tile of a three-tile run cleared through structure.clear,
            caught at successive points of its destruction clip

Every capture is saved whole and as a nearest-neighbour 4x crop around the
subject (the cable is only 2 px wide at gameplay scale), under --out, with
`captures.json` listing each file and what it shows. Needs a GPU: manual
only, never CI-gated.

Usage: python3 tools/wire_gameplay_capture.py --out docs/pr-proofs/wire-gameplay
       [--port 9518] [--size 1280x720]
"""
from __future__ import annotations

import argparse
import json
import os
import sys
import time

sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
from probelib import (boot, pin_camera_to_tile, poll_until, quit_engine, send,
                      send_json, set_paused, viewport)

HOURS = [7, 12, 17]   # world clock hours; the engine maps them to the sun
# Real seconds the clock runs between destruction captures (cumulative
# game time lands inside each of the five frames of a 0.5 s clip).
CLIP_STEPS = [0.0, 0.13, 0.1, 0.1, 0.1]
FACINGS = ["south", "west", "north", "east"]   # camera.rotateCW order


def capture(port: int, path: str, settle: float = 1.2) -> bool:
    try:
        os.remove(path)
    except FileNotFoundError:
        pass
    time.sleep(settle)
    reply = (send(port, f"return tostring(debug.captureScreenshot('{path}'))",
                  timeout=30.0) or "").strip()
    return reply.lower() not in ("false", "nil", "") \
        and os.path.exists(path) and os.path.getsize(path) > 4096


def locate(im, fallback: tuple[int, int]) -> tuple[int, int]:
    """Centroid of the cable-coloured pixels outside the HUD strip, or the
    fallback when none are found: the subject moves with the facing."""
    px = im.load()
    xs, ys = [], []
    for y in range(im.height):
        for x in range(120, im.width - 200):
            r, g, b = px[x, y]
            if r > 8 and r > 1.9 * g and r > 5.0 * b:
                xs.append(x)
                ys.append(y)
    if not xs:
        return fallback
    return (sum(xs) // len(xs), sum(ys) // len(ys))


def crop4x(path: str, centre: tuple[int, int], half: int = 70,
           fixed_centre: bool = False) -> str:
    """A 4x nearest-neighbour crop around the subject, saved next to
    `path`. The subject is LOCATED in the frame (the cable's own colour),
    so every facing is framed on it."""
    from PIL import Image
    im = Image.open(path).convert("RGB")
    cx, cy = centre if fixed_centre else locate(im, centre)
    box = (max(0, cx - half), max(0, cy - half // 2),
           min(im.width, cx + half), min(im.height, cy + half // 2))
    out = path.replace(".png", "_x4.png")
    c = im.crop(box)
    c.resize((c.width * 4, c.height * 4), Image.NEAREST).save(out)
    return out


def active_page(port: int):
    got = send_json(port, "local id = world.getActiveWorldId();"
                          " return id and {page = tostring(id)} or nil")
    return got.get("page") if isinstance(got, dict) else None


def wait_content_loaded(port: int) -> bool:
    probe = ("return {units = #unit.listDefs(),"
             " buildings = #building.listDefs()}")
    return bool(poll_until(90.0, lambda: (lambda g: isinstance(g, dict)
                and g.get("units", 0) > 0 and g.get("buildings", 0) > 0)(
                    send_json(port, probe))))


def shape(port: int, x: int, y: int):
    got = send_json(port, f"return structure.getAt({x}, {y}, 'wire')")
    if isinstance(got, dict) and "tex" in got:
        return got["tex"].rsplit("/", 1)[-1].removesuffix(".png")
    return None


def stocked_payer(port: int, x: int, y: int) -> int:
    """A unit that really carries wiring: any unit, given three `wiring`
    items (as tools/wire_probe.py does). Payment is checked by the callers,
    because an unpaid designation draws the designation GHOST, not a
    construction frame, and would pass every brightness assertion without
    exercising one."""
    uid = -1
    for _ in range(60):
        uid = int(float(send(port, "local d = unit.listDefs();"
                                   " if not d or #d == 0 then return -1 end;"
                                   f" return unit.spawn(d[1], {x}, {y})") or -1))
        if uid > 0:
            break
        time.sleep(0.5)
    if uid <= 0:
        sys.exit("unit.spawn never returned a unit")
    for _ in range(3):
        send(port, f"unit.addItem({uid}, 'wiring', 0); return 'ok'")
    for _ in range(60):
        have = send(port, f"local c = 0; for _, it in ipairs(unit.getInventory({uid}) or {{}}) do "
                          "if it.defName == 'wiring' then c = c + 1 end end; return c")
        if int(float(have or 0)) >= 3:
            return uid
        time.sleep(0.1)
    sys.exit("the unit never received its wiring")


def pay_and_progress(port: int, page: str, x: int, y: int, uid: int,
                     progress: float = 0.5) -> None:
    job = poll_until(30.0, lambda: send_json(
        port, f"return construction.getDesignationAt('{page}', {x}, {y})"))
    paid = send(port, f"return tostring(construction.payMaterials('{page}',"
                      f" {x}, {y}, {int(job['attempt'])}, {uid}))").strip('"')
    if paid != "true":
        sys.exit(f"payMaterials refused for ({x},{y}): {paid!r}")
    send(port, f"construction.addJobProgress('{page}', {x}, {y}, {progress},"
               f" {int(job['attempt'])}); return 'ok'")
    after = poll_until(30.0, lambda: (lambda d: d if isinstance(d, dict)
        and d.get("paid") is True and abs(float(d.get("progress", 0)) - progress) < 1e-6
        else None)(send_json(
            port, f"return construction.getDesignationAt('{page}', {x}, {y})")))
    if not after:
        sys.exit(f"the site at ({x},{y}) is not paid at progress {progress}")


def main() -> int:
    ap = argparse.ArgumentParser()
    ap.add_argument("--port", type=int, default=9518)
    ap.add_argument("--size", default="1280x720")
    ap.add_argument("--out", required=True)
    args = ap.parse_args()
    os.makedirs(args.out, exist_ok=True)
    log: list[dict] = []

    fixed: dict[str, tuple[int, int]] = {}

    def shot(name: str, what: str, centre, group: str | None = None) -> None:
        path = os.path.join(args.out, name + ".png")
        if not capture(args.port, path):
            sys.exit(f"capture failed: {name}")
        from PIL import Image
        if group is not None:
            # One framing per sequence, located on its FIRST frame, so
            # the frames can be compared by eye.
            if group not in fixed:
                fixed[group] = locate(Image.open(path).convert("RGB"), centre)
            centre = fixed[group]
        log.append({"file": name + ".png", "crop": os.path.basename(
            crop4x(path, centre, fixed_centre=group is not None)),
            "shows": what})
        print(f"  captured {name}")

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

        def home():
            pin_camera_to_tile(args.port, ax, ay, zz)

        def hour(h: int):
            send(args.port, f"world.setTime('{page}', {h}, 0); return 'ok'")

        home()
        zoom = send(args.port, "return camera.getZoom()").strip()
        print(f"gameplay zoom {zoom}; anchor tile ({ax}, {ay}); page {page}")

        # --- facings x suns: a two-tile run
        for tx, ty in ((ax, ay), (ax + 1, ay)):
            send(args.port, f"require('scripts.wire').place({tx}, {ty});"
                            " return 'ok'")
        run = [shape(args.port, ax, ay), shape(args.port, ax + 1, ay)]
        print("two-tile run:", run)
        for fi, fname in enumerate(FACINGS):
            if fi:
                send(args.port, "camera.rotateCW(); return 'ok'")
            home()
            for h in HOURS:
                hour(h)
                shot(f"facing_{fname}_hour{h:02d}",
                     f"two-tile run {run}, camera facing {fname}, sun hour {h}",
                     (cx, cy))
        send(args.port, "camera.rotateCW(); return 'ok'")   # back to south
        home()
        hour(12)
        send(args.port, "structure.clearAll(); return 'ok'")

        # --- neighbour shape change through construction
        # One framing for the three frames: the anchor tile sits at the
        # screen centre and the run extends to its south-east.
        fixed["neighbour"] = (cx + 25, cy + 50)
        bx, by = ax + 1, ay
        send(args.port, f"require('scripts.wire').place({ax}, {ay});"
                        " return 'ok'")
        shot("neighbour_0_lone_tile",
             f"a lone wire tile ({shape(args.port, ax, ay)})", (cx, cy), "neighbour")
        send(args.port, f"construction.designate('{page}', {bx}, {by}, {bx},"
                        f" {by}, 'structure', 'wire', 'wire'); return 'ok'")
        uid = stocked_payer(args.port, ax, ay - 3)
        pay_and_progress(args.port, page, bx, by, uid)
        # The payer has done its job; leaving it standing would put a unit
        # (and its selection ring) in every later frame.
        send(args.port, f"unit.destroy({uid}); return 'ok'")
        shot("neighbour_1_neighbour_half_built",
             "the next tile paid and half built: its construction frame is "
             "drawn, the first tile is still a lone tile", (cx, cy), "neighbour")
        # POP the paid job (what completion does) rather than cancel it:
        # a cancel refunds the wiring as a ground item, which would show as
        # a marker on the finished tile.
        send(args.port, f"construction.cancelDesignationForRefund('{page}',"
                        f" {bx}, {by}); return 'ok'")
        send(args.port, f"require('scripts.wire').place({bx}, {by});"
                        " return 'ok'")
        done = [shape(args.port, ax, ay), shape(args.port, bx, by)]
        shot("neighbour_2_completed",
             f"construction completed: the first tile re-capped to {done[0]},"
             f" the new tile is {done[1]}", (cx, cy), "neighbour")

        # --- destruction through structure.clear
        send(args.port, "structure.clearAll(); return 'ok'")
        # A fresh row: the neighbour scene's paid site stamped its D-18
        # corner slope into the terrain there (existing behaviour of
        # `construction.addJobProgress`), which would darken this scene.
        ty = ay + 4
        pin_camera_to_tile(args.port, ax, ty, zz)
        for dx in (0, 1, 2):
            send(args.port, f"require('scripts.wire').place({ax + dx}, {ty});"
                            " return 'ok'")
        shot("teardown_0_before_clear",
             "a three-tile straight run before the middle tile is cleared",
             (cx, cy), "teardown")
        # The destruction clip runs on `engine.gameTime()` (pause freezes
        # it, `world.setTimeScale` does not touch it): unpause for a short
        # real-time step, pause, read the clock, capture. The frame each
        # capture holds is floor(elapsed * fps), fps 10, five frames.
        t_clear = float(send(args.port, "return engine.gameTime()"))
        send(args.port, f"structure.clear({ax + 1}, {ty}, 'wire'); return 'ok'")
        for i, dt in enumerate(CLIP_STEPS, 1):
            # Run the clock for `dt` REAL seconds inside ONE console call,
            # so console latency does not decide how far the clip got.
            send(args.port,
                 "engine.setPaused(false); local t = engine.realTime();"
                 f" while engine.realTime() - t < {dt} do end;"
                 " engine.setPaused(true); return 'ok'", timeout=30.0)
            elapsed = float(send(args.port, "return engine.gameTime()")) - t_clear
            frame = min(4, int(elapsed * 10)) if elapsed < 0.5 else None
            shot(f"teardown_{i}_clip",
                 f"the cleared middle tile, {elapsed:.2f} game s into its "
                 "0.5 s destruction clip "
                 + (f"(frame {frame + 1} of 5)" if frame is not None
                    else "(expired)"), (cx, cy), "teardown")
        with open(os.path.join(args.out, "captures.json"), "w") as fh:
            json.dump({"zoom": zoom, "anchor": [ax, ay], "run": run,
                       "completed": done, "captures": log}, fh, indent=2)
        print(f"wrote {len(log)} captures to {args.out}")
        return 0
    finally:
        quit_engine(args.port, proc)


if __name__ == "__main__":
    sys.exit(main())
