#!/usr/bin/env python3
"""Capture production slope orientation without editing flags between views.

Manual offscreen visual gate: dry hill, isolated slope, all terrain masks and
all vegetation masks. Read back unchanged fixture state after every rotation.
"""
import argparse
import hashlib
import json
import os
from pathlib import Path
import time

from flat_fluid_scene import wait_for_defs
from fluid_levels_render_capture import make_root, lua
from probe_engine import prepare_executable
from probelib import boot, quit_engine, poll_until, pin_camera_to_tile

ROOT = Path(__file__).resolve().parent.parent
PAGE = 'test_arena'


def recipe():
    terrain, tiles = {}, {}
    scenes = []

    def scene(name, cx, cy, zoom, z=0):
        for x in range(cx-8, cx+9):
            for y in range(cy-7, cy+8):
                terrain[x,y] = -8
        scenes.append(dict(name=name, x=cx, y=cy, z=z, zoom=zoom))

    def tile(x,y,z,bits=0,veg=0):
        terrain[x,y] = z
        tiles[x,y] = dict(x=x,y=y,z=z,bits=bits,veg=veg)

    scene('isolated', -24, 8, .35, -2)
    tile(-24,8,-2,2)
    tile(-21,8,-3)
    scene('hill', -24, -24, .65)
    for i,z in enumerate([-2,-2,-3,-3,-4,-4,-5,-5]):
        for y in [-27,-26,-22,-21]:
            tile(-28+i,y,z,2 if i in [1,3,5] else 0)
    for name,cx,cy,veg in [('masks',8,8,0),('vegetation',8,-24,1)]:
        scene(name,cx,cy,.75)
        for bits in range(16):
            tile(cx-5+3*(bits%4),cy-4+3*(bits//4),-2,bits,veg)
    return dict(terrain=[dict(x=x,y=y,z=z) for (x,y),z in sorted(terrain.items())],
                tiles=list(tiles.values()),scenes=scenes)


def main():
    ap=argparse.ArgumentParser(description=__doc__)
    ap.add_argument('--engine',type=Path)
    ap.add_argument('--out',type=Path,required=True)
    ap.add_argument('--port',type=int,default=9550)
    a=ap.parse_args()
    out=a.out.resolve();out.mkdir(parents=True,exist_ok=False)
    engine=a.engine.resolve(strict=True) if a.engine else Path(prepare_executable(announce=print))
    engine_hash=hashlib.sha256(engine.read_bytes()).hexdigest()
    os.environ['SYNARCHY_PROBE_ENGINE_EXE']=str(engine)
    root=make_root(ROOT,out)
    spec=recipe();(out/'recipe.json').write_text(json.dumps(spec,indent=2)+'\n')
    owned=[];ready=False;frames=[]
    try:
        proc=boot(a.port,log=str(out/'engine.log'),mode=('--offscreen',),
                  args=['--size','1280x900','--resource-root',str(root)],
                  on_launch=owned.append,ready_timeout=60)
        ready=True;assert wait_for_defs(a.port,60)
        assert lua(a.port,"engine.setPaused(true); package.loaded['scripts.ui_manager'].onOpenArena(); return true") is True
        assert 'done' in str(lua(a.port,'return world.waitForInit(30)')).lower()
        lua(a.port,f"world.setTimeScale('{PAGE}',0); world.setTime('{PAGE}',3,0); package.loaded['scripts.hud'].hide(); return true")
        for start in range(0,len(spec['terrain']),60):
            rows=','.join('{%d,%d,%d}'%(r['x'],r['y'],r['z']) for r in spec['terrain'][start:start+60])
            assert lua(a.port,f"for _,r in ipairs({{{rows}}}) do for z=r[3]+1,0 do assert(world.setCell('{PAGE}',r[1],r[2],z,'air')) end; assert(world.setCell('{PAGE}',r[1],r[2],r[3],r[3]>-8 and 'granite' or 'basalt')) end; return true") is True
        # Author once, after terrain. There are NO writes to slope or vegetation
        # inside the capture loop: camera changes must not alter world state.
        for r in spec['tiles']:
            lua(a.port,f"assert(world.setSlope('{PAGE}',{r['x']},{r['y']},{r['z']},{r['bits']})); assert(world.setVegAt('{PAGE}',{r['x']},{r['y']},{r['z']},{r['veg']})); return true")
        def observe():
            rows=','.join('{%d,%d}'%(r['x'],r['y']) for r in spec['tiles'])
            return lua(a.port,f"local out={{}}; for _,r in ipairs({{{rows}}}) do local _,z=world.getTerrainAt(r[1],r[2]); local f=world.getFluidAt(r[1],r[2]); out[#out+1]={{x=r[1],y=r[2],z=z,bits=world.getSlopeAt(r[1],r[2]),veg=world.getVegAt(r[1],r[2]),wet=f~=nil}} end; return out")
        expected=[dict(r,wet=False) for r in spec['tiles']]
        assert poll_until(30,lambda:observe()==expected),observe()
        for _ in range(4):
            if lua(a.port,'return camera.getFacing()')==0:break
            lua(a.port,'camera.rotateCW(); return true')
        for scene in spec['scenes']:
            for _ in range(4):
                facing=int(lua(a.port,'return camera.getFacing()'))
                assert pin_camera_to_tile(a.port,scene['x'],scene['y'],scene['z'])
                lua(a.port,f"camera.setZoom({scene['zoom']}); package.loaded['scripts.popup'].dismissAll(); return true")
                time.sleep(.6)
                observed=observe();assert observed==expected,observed
                path=out/f"{scene['name']}-{facing}.png"
                result=lua(a.port,f'return debug.captureScreenshot({json.dumps(str(path))})')
                assert isinstance(result,dict) and result.get('path')==str(path),result
                frames.append(dict(scene=scene['name'],facing=facing,path=path.name,state_unchanged=True))
                print(path.name,flush=True)
                lua(a.port,'camera.rotateCW(); return true')
        assert observe()==expected
        (out/'manifest.json').write_text(json.dumps(dict(complete=True,engine_sha256=engine_hash,
            frames=frames,observations=expected,flags_authored_once=True),indent=2)+'\n')
    finally:
        if ready:quit_engine(a.port,proc)
        elif owned and owned[0].poll() is None:
            owned[0].terminate();owned[0].wait(timeout=10)


if __name__=='__main__':main()
