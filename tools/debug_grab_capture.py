#!/usr/bin/env python3
"""Retain #2489's real-input offscreen acceptance evidence.

Build first with cabal build all, then run with --output <empty directory>.
Only fixture setup uses the console's world/camera/item setters. Every captured
relocation goes through the F8 button and injected physical mouse events.
The private resource root, engine log and save are retained beside the images.
"""
from __future__ import annotations

import argparse
import json
import os
from pathlib import Path
import shutil
import subprocess
import time

from probelib import (boot, capture_request_id, poll_until, quit_engine, send,
                      send_json, wait_load_published, wait_save_complete)


class Capture:
    def __init__(self, port, output):
        self.port, self.output = port, output
        self.evidence = []

    def call(self, lua):
        return send_json(self.port, lua)

    def command(self, lua):
        result = send(self.port, lua + "; return true")
        assert result == "true", result

    def row(self, gid):
        row = next(r for r in self.call("return item.listGround()") if r["id"] == gid)
        row["temperature"] = self.call(f"return item.getGroundTemp({gid})")
        return row

    def frame(self, name, gid):
        time.sleep(.15)
        state = self.call("return {page=world.getActiveWorldId(),"
                          "window={engine.getWindowSize()},"
                          "framebuffer={engine.getFramebufferSize()},"
                          "scale=engine.getUIScale(),mouse={engine.getMousePosition()},"
                          "quads=item.debugQuads(),armed=require('scripts.debug').armedGrab}")
        state.update(name=name, item=self.row(gid))
        state["surface"] = self.call(
            f"local _,z=world.getTerrainAt(math.floor({state['item']['x']}),"
            f"math.floor({state['item']['y']})); return z")
        shot = self.call(f"return debug.captureScreenshot({json.dumps(str(self.output / (name + '.png')))})")
        assert "error" not in shot, shot
        self.evidence.append(state)
        self.flush()
        print(name, state["item"]["x"], state["item"]["y"], flush=True)
        return state["item"]

    def flush(self):
        (self.output / "evidence.json").write_text(json.dumps(self.evidence, indent=2) + "\n")

    def arm(self):
        if not self.call("return require('scripts.debug').visible"):
            self.command("input.key('F8')")
        widgets = self.call("return ui.dumpWidgets()")
        button = next(w for w in widgets if w.get("name") == "grab_button" and w["visible"])
        self.evidence.append({"grabButton": button})
        if not self.call("return require('scripts.debug').armedGrab"):
            b = button["bounds"]
            self.command(f"input.click({b['x'] + b['w']/2},{b['y'] + b['h']/2})")
        assert self.call("return require('scripts.debug').armedGrab") is True, self.call(
            "local d=require('scripts.debug'); return {visible=d.visible,rects=d.clickableRects,"
            "view=d.inGameplayView(),menu=require('scripts.ui_manager').currentMenu,"
            "hud=require('scripts.hud').currentView,blocked=UI.isInputBlocked()}")

    def sprite(self, gid):
        q = self.call("return item.debugQuads()")
        ww, wh = self.call("return {engine.getWindowSize()}")
        for s in q["sample"]:
            x = (((s["x0"] + s["x1"]) / 2 - q["camX"]) / (q["zoom"] * ww / wh) + 1) * ww / 2
            y = (((s["y0"] + s["y1"]) / 2 - q["camY"]) / q["zoom"] + 1) * wh / 2
            if self.call(f"return item.hitTestAt({x},{y})") == gid:
                return x, y
        raise AssertionError(f"no visible hit-tested sprite for {gid}: {q}")

    def spawn(self, name, x, y):
        return self.call(f"return item.spawnGround('{name}',{x},{y},{{quality=73,condition=68}})")

    @staticmethod
    def same_instance(before, after):
        assert {k: v for k, v in before.items() if k not in ("x", "y")} == {
            k: v for k, v in after.items() if k not in ("x", "y")}

    def drag(self, name, gid, delta=(90, 40), invalid=False):
        self.arm()
        x, y = self.sprite(gid)
        before = self.frame(name + "-before", gid)
        self.command(f"input.mouseDown({x},{y})")
        self.command(f"input.moveMouse({x + delta[0]},{y + delta[1]})")
        mid = self.frame(name + "-mid", gid)
        assert (before["x"], before["y"]) != (mid["x"], mid["y"])
        self.same_instance(before, mid)
        if invalid:
            # Inject an off-window/off-world pointer, then re-enter on release.
            pick = self.call("return {world.pickPos(1000000,1000000)}")
            assert not pick, pick
            self.command("input.moveMouse(1000000,1000000)")
            held = self.frame(name + "-invalid", gid)
            assert held == mid
        self.command(f"input.mouseUp({x + delta[0] + 25},{y + delta[1] + 10})")
        after = self.frame(name + "-after", gid)
        self.same_instance(before, after)
        assert (mid["x"], mid["y"]) != (after["x"], after["y"])
        assert self.call("return require('scripts.debug').armedGrab") is True

    def run(self):
        assert poll_until(180, lambda: self.call("return require('scripts.startup_loader').isDone()") is True)
        assert poll_until(60, lambda: send(self.port, "return require('scripts.ui_manager').currentMenu") == "main")
        self.command("require('scripts.ui_manager').showMenu('test_arena')")
        assert poll_until(30, lambda: send(self.port, "return require('scripts.ui_manager').currentMenu") == "test_arena_view")
        assert poll_until(30, lambda: self.call("return world.getActiveWorldId()") == "test_arena"
                         and self.call("return (world.getTerrainAt(0,0))") == 0)
        self.command("camera.goToTile(45,0); camera.setZTracking(false); camera.setZSlice(0); camera.setZoom(.3)")
        assert poll_until(30, lambda: self.call("return require('scripts.debug').inGameplayView()") is True)
        lantern = self.spawn("lantern", 45.5, .5)
        axe = self.spawn("axe_steel", 44.5, 1.5)
        self.drag("arena-lantern", lantern, invalid=True)
        self.command("camera.setZoom(.4)")
        self.drag("arena-axe", axe)

        self.command("require('scripts.ui_manager').showMenu('main'); world.destroyAll()")
        self.command("local v=require('scripts.world_view'); v.ensureStructuralTextures(); "
                     "require('scripts.world_manager').createWorld({worldId='grab_world',seed=1,"
                     "worldSize=8,plateCount=3,structural=v.structuralTextures})")
        assert poll_until(180, lambda: "done" in send(self.port, "return world.waitForInit(1)"))
        self.command("require('scripts.ui_manager').showMenu('world_view'); world.loadChunksInRegion('grab_world',-2,-2,2,2)")
        assert poll_until(60, lambda: self.call("return (world.getTerrainAt(-24,-12))") is not None)
        self.command("camera.goToTile(-23,-12); camera.setZTracking(false); camera.setZSlice(5); camera.setZoom(.35); "
                     "engine.setUIScale(.75); require('scripts.debug').onFramebufferResize(1280,720)")
        lantern = self.spawn("lantern", -24.5, -12.5)
        axe = self.spawn("axe_steel", -21.5, -12.5)
        self.command(f"engine.setPaused(true); item.setGroundTemp({lantern},123); item.setGroundTemp({axe},87)")
        self.arm()
        x, y = self.sprite(lantern)
        before = self.frame("world-lantern-before", lantern)
        self.command(f"input.mouseDown({x},{y}); input.moveMouse(650,340)")
        high = self.frame("world-lantern-mid-higher", lantern)
        assert self.evidence[-1]["surface"] > self.evidence[-2]["surface"]
        self.same_instance(before, high)
        self.command("input.mouseUp(490,460)")
        low = self.frame("world-lantern-after-lower", lantern)
        assert self.evidence[-1]["surface"] < self.evidence[-2]["surface"]
        self.same_instance(before, low)
        self.command("camera.setZoom(.4)")
        self.drag("world-axe", axe, delta=(0, 100))
        saved = {gid: self.row(gid) for gid in (lantern, axe)}
        assert send(self.port, "return engine.saveWorld('grab_world','grab_acceptance')") == "true"
        save_id = capture_request_id(self.port, "return engine.getSaveStatus()")
        ok, status = wait_save_complete(self.port, save_id)
        assert ok, status
        self.evidence.append({"save": status, "items": saved})
        # Load while holding an actual capture: publication must clear its owner.
        x, y = self.sprite(axe)
        self.command(f"input.mouseDown({x},{y})")
        assert send(self.port, "return engine.loadSave('grab_acceptance')") == "true"
        load_id = capture_request_id(self.port, "return engine.getLoadStatus()")
        ok, status = wait_load_published(self.port, request_id=load_id)
        assert ok, status
        assert poll_until(30, lambda: self.call("return require('scripts.debug').armedGrab") is None)
        self.command(f"input.moveMouse({x+100},{y+50}); input.mouseUp({x+100},{y+50})")
        for gid, row in saved.items():
            assert self.row(gid) == row, (row, self.row(gid))
        self.evidence.append({"load": status, "items": [self.row(gid) for gid in saved], "grabCleared": True})
        self.frame("world-after-load", lantern)


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--port", type=int, default=9551)
    parser.add_argument("--output", type=Path, required=True)
    args = parser.parse_args()
    output = args.output.resolve()
    output.mkdir(parents=True, exist_ok=False)
    repo = Path(__file__).resolve().parent.parent
    resource = output / "run-root"
    resource.mkdir()
    for name in ("assets", "scripts", "data"):
        (resource / name).symlink_to(repo / name, target_is_directory=True)
    shutil.copytree(repo / "config", resource / "config", ignore=shutil.ignore_patterns("*.local.yaml"))
    if "SYNARCHY_PROBE_ENGINE_EXE" not in os.environ:
        exe = subprocess.check_output(["cabal", "list-bin", "exe:synarchy"], cwd=repo, text=True).strip()
        os.environ["SYNARCHY_PROBE_ENGINE_EXE"] = exe
    proc = boot(args.port, log=str(resource / "engine.log"), mode=("--offscreen",),
                args=["--size", "1280x720", "--resource-root", str(resource)])
    capture = Capture(args.port, output)
    try:
        capture.run()
    finally:
        capture.flush()
        quit_engine(args.port, proc)


if __name__ == "__main__":
    main()
