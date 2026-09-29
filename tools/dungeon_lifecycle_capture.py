#!/usr/bin/env python3
"""Capture the approved Dungeon damage variants in a real offscreen arena.

Run from the repository root. The two rows show default/weathered/broken/
ruined floors, then the same floors with south-corner posts. Four facings
exercise the actual static facemap alpha and lighting. Needs Vulkan.
"""
import argparse
import json
from pathlib import Path

from probelib import (boot, pin_camera_to_tile, poll_until, quit_engine,
                      send, send_json, set_paused)
from structure_rotation_probe import active_page, capture, wait_content_loaded


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--port", type=int, default=9526)
    parser.add_argument("--out", type=Path, default=Path("/tmp/dungeon-lifecycle-capture"))
    args = parser.parse_args()
    out = args.out.resolve()
    out.mkdir(parents=True, exist_ok=True)
    proc = boot(args.port, args=["--size", "1280x720"], mode=("--offscreen",),
                label="Dungeon art capture")
    try:
        assert wait_content_loaded(args.port), "content did not load"
        send(args.port, "package.loaded['scripts.ui_manager'].onOpenArena(); return 'ok'")
        page = poll_until(90, lambda: active_page(args.port))
        assert page, "arena did not open"
        point = poll_until(90, lambda: send_json(args.port,
            "local x,y=world.pickTile(640,360); return x and {x=x,y=y} or nil"))
        assert point, "arena not pickable"
        x, y = int(point["x"]), int(point["y"])
        set_paused(args.port, True)
        send(args.port, "world.setTimeScale(0); world.setSunAngle(0.25); return 'ok'")
        records = []
        for index, variant in enumerate((None, "weathered", "broken", "ruined")):
            v = "nil" if variant is None else json.dumps(variant)
            for row in range(2):
                gx, gy = x + index * 2 - 3, y + row * 3 - 1
                lua = ("local S=package.loaded['scripts.structures']; "
                       f"assert(S.floor({gx},{gy},nil,nil,{v})); ")
                if row:
                    lua += f"assert(S.post({gx},{gy},'s',nil,nil,{v})); "
                send(args.port, lua + "return 'ok'")
                for kind, slot in (("floor", "floor"), ("post", "post_s")) if row else (("floor", "floor"),):
                    placed = send_json(args.port, f"return structure.getAt({gx},{gy},'{slot}')")
                    prefix = "assets/textures/buildings/dungeon_1/"
                    expected = prefix + (variant + "/" if variant else "") + kind + ".png"
                    assert placed and placed["tex"] == expected, placed
                    if variant:
                        assert placed["face"] == f"assets/textures/facemap/dungeon_1/{variant}_{kind}.png", placed
                    records.append({"x": gx, "y": gy, "slot": slot, **placed})
        z = records[0]["z"]
        shots = []
        for index, facing in enumerate(("facesouth", "facewest", "facenorth", "faceeast")):
            if index:
                send(args.port, "camera.rotateCW(); return 'ok'")
            assert pin_camera_to_tile(args.port, x, y, z)
            send(args.port, "camera.setZoom(0.32); return 'ok'")
            data = capture(args.port, str(out / (facing + ".png")))
            assert len(data) > 10000, "empty screenshot"
            shots.append(data)
        assert len(set(shots)) == 4, "camera did not rotate"
        (out / "placed.json").write_text(json.dumps(records, indent=2) + "\n")
        print(f"PASS: 12 placed pieces use the selected textures and masks; four captures in {out}")
    finally:
        quit_engine(args.port, proc)


if __name__ == "__main__":
    main()
