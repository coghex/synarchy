#!/usr/bin/env python3
"""Capture #2533's generated river bends, using an explicit engine binary.

Run once per revision with --engine /absolute/binary --out /absolute/directory.
Uses only the existing loopback console client, an isolated resource root,
and offscreen rendering. No edits or runtime fluid activation are performed.
The seed and camera recipe are identical on both revisions; screenshots
support visual review, not pixel-equality assertions.
"""
import argparse
import hashlib
import json
import os
from pathlib import Path
import time

from flat_fluid_scene import wait_for_defs, open_arena
from fluid_levels_render_capture import make_root, lua
from probelib import boot, quit_engine, pin_camera_to_tile, poll_until


def main():
    ap = argparse.ArgumentParser(description=__doc__)
    ap.add_argument('--engine', type=Path, required=True)
    ap.add_argument('--out', type=Path, required=True)
    ap.add_argument('--port', type=int, default=9534)
    args = ap.parse_args()
    source = Path(__file__).resolve().parent.parent
    out = args.out.resolve()
    out.mkdir(parents=True, exist_ok=False)
    root = make_root(source, out)
    binary = args.engine.resolve()
    os.environ['SYNARCHY_PROBE_ENGINE_EXE'] = str(binary)
    port = args.port
    proc = boot(port, log=str(out / 'engine.log'), mode=('--offscreen',),
                args=['--size', '1280x720', '--resource-root', str(root)])
    try:
        assert wait_for_defs(port, 120), 'definitions did not load'
        open_arena(port)
        assert lua(port, "engine.setPaused(true); return world.init('rivers',42,64,3)") is True
        assert poll_until(180, lambda: lua(port, "return world.getSeed('rivers') == 42") is True)
        assert poll_until(180, lambda: 'done' in str(lua(port, 'return world.waitForInit(10)')))
        lua(port, "world.hide('test_arena'); world.show('rivers'); world.setTimeScale('rivers',0); "
            "world.setTime('rivers',3,0); package.loaded['scripts.world_view'].sendTexturesToWorld('rivers'); "
            "package.loaded['scripts.hud'].hide(); return true")
        frames = []
        # The first bend contains the largest required surface adjustments
        # in seed 42. The second contains the prior water-above-bank witness.
        for name, x, y, z in [('steep-bend', -104, -210, 25),
                              ('bank-reach', -118, 36, 98),
                              ('gentle-reach', -158, -111, 21)]:
            cx, cy = x // 16, y // 16
            lua(port, f"world.loadChunksInRegion({cx-2},{cy-2},{cx+2},{cy+2},'rivers'); return true")
            assert poll_until(90, lambda: str(lua(port, "return world.waitForChunks(10,'rivers')")).strip() == '0')
            assert poll_until(30, lambda: lua(port,
                f"return world.getTerrainAt({x},{y},'rivers') ~= nil") is True)
            samples = lua(port, f"local _,t=world.getTerrainAt({x},{y},'rivers'); "
                f"local f,s=world.getFluidAt({x},{y},'rivers'); return {{terrain=t,fluid=f,surface=s}}")
            for turn in range(4):
                assert pin_camera_to_tile(port, x, y, z)
                lua(port, 'camera.setZoom(1.2); return true')
                time.sleep(1)
                image = out / f'{name}-{turn}.png'
                result = lua(port, f'return debug.captureScreenshot({json.dumps(str(image))})')
                assert result.get('path') == str(image), result
                frames.append(dict(image=image.name, x=x, y=y, z=z, samples=samples,
                                   facing=lua(port, 'return camera.getFacing()')))
                print(image, flush=True)
                lua(port, 'camera.rotateCW(); return true')
        (out / 'manifest.json').write_text(json.dumps(dict(
            engine=str(binary), engine_sha256=hashlib.sha256(binary.read_bytes()).hexdigest(),
            seed=42, world_size=64, plates=3, frames=frames), indent=2) + '\n')
    finally:
        quit_engine(port, proc)


if __name__ == '__main__':
    main()
