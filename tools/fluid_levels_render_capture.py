#!/usr/bin/env python3
"""Manual GPU capture of #2529's exact fluid geometry; never a CI probe.

Create a paused exact scene with the implementation, retaining a normal save.
Pass that identical --fixture directory to the base binary for comparison.
Only debug.setFluidSurface authors exact values; normal save snapshot replay
restores them. Existing integer Lua queries are never used as exact setters.

  python3 tools/fluid_levels_render_capture.py --port 9429 --size 1280x720 \
      --out /tmp/issue-2529-captures
  python3 tools/fluid_levels_render_capture.py --engine /path/to/base/synarchy \
      --source-root /path/to/base --fixture /tmp/issue-2529-captures/fixture \
      --out /tmp/issue-2529-base

All outputs, config and saves live under --out. Engines always use --offscreen;
teardown owns only the launched process. Screenshots support owner judgment,
not byte-equality assertions.
"""
from __future__ import annotations

import argparse
import hashlib
import json
import os
from pathlib import Path
import shutil
import subprocess
import sys
import time

from flat_fluid_scene import wait_for_defs, open_arena
from probelib import (boot, quit_engine, send_json, poll_until,
                      wait_load_published, wait_save_complete, pin_camera_to_tile)
from probe_engine import prepare_executable

REPO = Path(__file__).resolve().parent.parent
PAGE = "test_arena"
SLOT = "fluid_levels_review"
ICE_PAGE = "fluid_levels_ice"
KINDS = ("river", "lake", "lava", "ocean")


def lua(port: int, code: str):
    result = send_json(port, code, timeout=60)
    if ((isinstance(result, dict) and result.get("error"))
            or (isinstance(result, str) and "error:" in result.lower())):
        raise RuntimeError(result)
    return result


def recipe() -> dict:
    """Explicit signed eighth planes; all terrain edits remain whole-z."""
    terrain = {}
    cells = {}
    scenes = []
    for row, kind in enumerate(KINDS):
        y = -24 + row * 12
        # Adjacent levels show the fine staircase; separated levels expose
        # each slab beside dry ground. The x=16 boundary lies in the strip.
        for x in range(7, 26):
            for yy in range(y - 1, y + 5):
                terrain[x, yy] = -4
        for level in range(1, 9):
            x = 8 + (level - 1) * 2
            cells[x, y] = (kind, -32 + level)
            cells[12 + level - 1, y + 3] = (kind, -32 + level)
        scenes.append(dict(name=kind + "-levels", x=16, y=y + 1, z=-2, zoom=0.5))
    # Stacks, partial neighbours, enclosed cell and multi-z drops, with
    # independent left/right endpoints. Keep this separate from shallow
    # rows so the owner can inspect it at the same scale.
    for x in range(-24, -3):
        for y in range(-8, 9):
            terrain[x, y] = -8
    for i, kind in enumerate(KINDS):
        x, y = -22 + i * 5, -5
        cells[x, y] = (kind, -5)  # full stack plus a 3/8 cap
        cells[x, y + 1] = (kind, -43)
        cells[x + 1, y] = (kind, -29)
        for dx in range(3):
            for dy in range(3):
                cells[x + dx, y + 6 + dy] = (kind, -21)
    scenes.append(dict(name="stacks-and-enclosures", x=-13, y=0, z=0, zoom=0.85))
    for scene in scenes:
        scene["page"] = PAGE
    return dict(
        terrain=[dict(x=x, y=y, z=z) for (x, y), z in sorted(terrain.items())],
        fluids=[dict(x=x, y=y, kind=k, exact=u) for (x, y), (k, u) in sorted(cells.items())],
        scenes=scenes,
    )


def build_scene(port: int, spec: dict) -> None:
    assert lua(port, "return engine.setPaused(true)") is True
    lua(port, f"world.setTimeScale('{PAGE}',0); return true")
    # Terrain commands are queued before fluid snapshots. The arena starts
    # at zero and has solid columns down through -15.
    for start in range(0, len(spec["terrain"]), 80):
        batch = spec["terrain"][start:start + 80]
        rows = ",".join("{%d,%d,%d}" % (r["x"], r["y"], r["z"]) for r in batch)
        assert lua(port, f"local rows={{{rows}}}; for _,r in ipairs(rows) do "
                   f"for z=r[3]+1,0 do assert(world.setCell('{PAGE}',r[1],r[2],z,'air')) end; "
                   f"assert(world.setCell('{PAGE}',r[1],r[2],r[3],'basalt')) end; return true") is True
    for start in range(0, len(spec["fluids"]), 80):
        batch = spec["fluids"][start:start + 80]
        rows = ",".join("{%d,%d,'%s',%d}" % (r["x"], r["y"], r["kind"], r["exact"]) for r in batch)
        assert lua(port, f"local rows={{{rows}}}; for _,r in ipairs(rows) do "
                   f"assert(debug.setFluidSurface('{PAGE}',r[1],r[2],r[3],r[4])) end; return true") is True
    last = spec["fluids"][-1]
    expected = -(-last["exact"] // 8)
    assert poll_until(30, lambda: lua(port,
        f"local _,s=world.getFluidAt({last['x']},{last['y']}); return s") == expected)


def add_ice_scene(port: int, spec: dict) -> None:
    """Use ordinary generated ice; no ice setter or generation-policy change."""
    assert lua(port, f"return world.init('{ICE_PAGE}',42,64,8)") is True
    # Keep each console wait bounded so a caller can report progress.
    deadline = time.monotonic() + 180
    while time.monotonic() < deadline:
        result = lua(port, "return world.waitForInit(30)")
        if isinstance(result, str) and "done" in result:
            break
    else:
        raise RuntimeError("ice world initialization timed out")
    assert lua(port, f"world.hide('{PAGE}'); world.show('{ICE_PAGE}'); "
               f"package.loaded['scripts.world_view'].sendTexturesToWorld('{ICE_PAGE}'); "
               "engine.setPaused(true); camera.setZoom(0.5); return true") is True
    spec["ice_generation"] = dict(seed=42, world_size=64, plate_count=8)
    spec["ice_samples"] = []
    for name, x, y, mode, kind in (
        ("dry-drape-ice", 180, 210, "drape", None),
        ("dry-basin-ice", 205, 205, "basin", None),
        ("covered-partial-lake", 218, 218, "basin", "lake"),
        ("covered-partial-ocean", 224, 224, "drape", "ocean"),
    ):
        assert pin_camera_to_tile(port, x, y, 31)
        def sample():
            return lua(port, f"local z,m=world.getIceAt({x},{y},'{ICE_PAGE}'); "
                f"local _,t=world.getTerrainAt({x},{y},'{ICE_PAGE}'); "
                f"local f=world.getFluidAt({x},{y},'{ICE_PAGE}'); "
                "return {ice=z,mode=m,terrain=t,fluid=f}")
        assert poll_until(45, lambda: isinstance((r := sample()), dict) and r.get("ice") is not None)
        before = sample()
        assert before["mode"] == mode, (name, before)
        assert before.get("fluid") is None, ("expected dry ice fixture", before)
        if kind:
            # The ice was generated independently. The exact edit preserves
            # it, just as the normal replay does on both captured revisions.
            exact = before["terrain"] * 8 + 3
            assert lua(port, f"return debug.setFluidSurface('{ICE_PAGE}',{x},{y},'{kind}',{exact})") is True
            assert poll_until(30, lambda: sample().get("fluid") == kind)
            after = sample()
            assert (after["ice"], after["mode"]) == (before["ice"], before["mode"])
        else:
            exact = None
        spec["ice_samples"].append(dict(name=name, x=x, y=y, before=before, exact=exact, kind=kind))
        spec["scenes"].append(dict(name=name, x=x, y=y, z=before["ice"] + 2,
                                    zoom=0.5, page=ICE_PAGE))


def make_root(source: Path, out: Path) -> Path:
    root = out / "resource-root"
    root.mkdir()
    for name in ("assets", "scripts", "data"):
        (root / name).symlink_to(source / name, target_is_directory=True)
    shutil.copytree(source / "config", root / "config",
                    ignore=shutil.ignore_patterns("*.local.yaml"))
    return root


def capture(port: int, out: Path, spec: dict) -> list:
    frames = []
    assert lua(port, "package.loaded['scripts.ui_manager'].showMenu('test_arena_view'); package.loaded['scripts.hud'].hide(); return true") is True
    assert lua(port, "return engine.setPaused(true)") is True
    lua(port, f"world.setTimeScale('{PAGE}',0); return true")
    for scene in spec["scenes"]:
        page = scene["page"]
        assert lua(port, f"world.hide('{PAGE}'); world.hide('{ICE_PAGE}'); "
                   f"world.show('{page}'); world.setTimeScale('{page}',0); "
                   f"package.loaded['scripts.world_view'].sendTexturesToWorld('{page}'); return true") is True
        for turn in range(4):
            if turn:
                lua(port, "camera.rotateCW(); return true")
            facing = lua(port, "return camera.getFacing()")
            assert pin_camera_to_tile(port, scene["x"], scene["y"], scene["z"])
            assert lua(port, f"camera.setZoom({scene['zoom']}); return true") is True
            if page == ICE_PAGE:
                expected = next(r for r in spec["ice_samples"] if r["name"] == scene["name"])
                def ice_matches():
                    actual = lua(port, f"local z,m=world.getIceAt({scene['x']},{scene['y']},'{page}'); return {{z=z,mode=m}}")
                    return (isinstance(actual, dict) and actual.get("z") == expected["before"]["ice"]
                            and actual.get("mode") == expected["before"]["mode"])
                assert poll_until(45, ice_matches), f"ice changed or failed to load: {scene['name']}"
            for light, angle in (("day", 0.125), ("night", 0.75)):
                lua(port, f"world.setTime('{page}',{int(angle * 24)},0); return true")
                time.sleep(0.6)
                name = f"{scene['name']}-{facing}-{light}.png"
                path = out / name
                result = lua(port, f"return debug.captureScreenshot({json.dumps(str(path))})")
                if not isinstance(result, dict) or result.get("path") != str(path):
                    raise RuntimeError(f"capture failed: {result!r}")
                frame = dict(scene=scene, facing=facing, light=light, sun_angle=angle,
                             path=name, camera=lua(port, "local x,y= camera.getPosition(); "
                             "return {x=x,y=y,z=camera.getZSlice(),zoom=camera.getZoom()}"))
                frames.append(frame)
                print(name, flush=True)
        lua(port, "camera.rotateCW(); return true")  # return to initial facing
    return frames


def verify_exact_fixture(summary: dict, spec: dict) -> None:
    """Cross-check every authored arena plane against the real save decoder."""
    arena = next(p for p in summary["pages"] if p["pageId"] == PAGE)
    cells = sorted(spec["fluids"], key=lambda r: (r["x"], r["y"]))
    rendered = ";".join(f"{r['x']},{r['y']},{r['kind']},{r['exact']}" for r in cells)
    digest = 14695981039346656037
    for byte in rendered.encode():
        digest = ((digest ^ byte) * 1099511628211) & ((1 << 64) - 1)
    actual = arena["fluidSnapshots"]
    assert actual["count"] == len(cells), actual
    assert actual["digest"] == f"{digest:016x}", actual


def main() -> int:
    ap = argparse.ArgumentParser(description=__doc__)
    ap.add_argument("--port", type=int, default=9429)
    ap.add_argument("--size", default="1280x720")
    ap.add_argument("--out", type=Path, required=True)
    ap.add_argument("--engine", type=Path)
    ap.add_argument("--codec", type=Path)
    ap.add_argument("--source-root", type=Path, default=REPO)
    ap.add_argument("--fixture", type=Path)
    ap.add_argument("--scenes", nargs="+", help="capture selected recipe scene names")
    args = ap.parse_args()
    out = args.out.resolve()
    out.mkdir(parents=True, exist_ok=True)
    source = args.source_root.resolve()
    root = make_root(source, out)
    if args.engine:
        os.environ["SYNARCHY_PROBE_ENGINE_EXE"] = str(args.engine.resolve())
    binary = Path(prepare_executable(REPO, timeout=1800, announce=print))
    os.environ["SYNARCHY_PROBE_ENGINE_EXE"] = str(binary)
    if args.codec:
        os.environ["SYNARCHY_SAVE_CODEC_EXE"] = str(args.codec.resolve())
    codec = Path(prepare_executable(REPO, timeout=1800, announce=print,
                target="exe:synarchy-save-codec", env_var="SYNARCHY_SAVE_CODEC_EXE"))
    spec = recipe()
    proc = boot(args.port, log=str(out / "engine.log"), mode=("--offscreen",),
                args=["--size", args.size, "--resource-root", str(root)])
    try:
        assert wait_for_defs(args.port, 120), "definitions did not load"
        if args.fixture:
            spec = json.loads((args.fixture.parent / "recipe.json").read_text())
            shutil.copytree(args.fixture, root / "saves" / SLOT)
            assert lua(args.port, f"return engine.loadSave('{SLOT}')") is True
            published, status = wait_load_published(args.port)
            assert published, f"fixture load failed: {status}"
        else:
            open_arena(args.port)
            build_scene(args.port, spec)
            add_ice_scene(args.port, spec)
            lua(args.port, f"engine.saveWorld('{PAGE}','{SLOT}'); return true")
            status = lua(args.port, "return engine.getSaveStatus()")
            assert isinstance(status, dict), "save request was not accepted"
            saved, status = wait_save_complete(args.port, status["id"])
            assert saved, f"fixture save failed: {status}"
        (out / "recipe.json").write_text(json.dumps(spec, indent=2) + "\n")
        shutil.copytree(root / "saves" / SLOT, out / "fixture")
        subprocess.run([str(codec), "summary", "--fixture",
            str(out / "fixture" / "world.synworld"), "--output", str(out / "fixture-summary.json")], check=True)
        summary = json.loads((out / "fixture-summary.json").read_text())
        verify_exact_fixture(summary, spec)
        capture_spec = dict(spec)
        if args.scenes:
            known = {scene["name"] for scene in spec["scenes"]}
            if set(args.scenes) - known:
                raise ValueError(f"unknown scenes: {set(args.scenes) - known}")
            capture_spec["scenes"] = [scene for scene in spec["scenes"] if scene["name"] in args.scenes]
        frames = capture(args.port, out, capture_spec)
        binary = Path(os.environ["SYNARCHY_PROBE_ENGINE_EXE"])
        manifest = dict(source_revision=subprocess.check_output(
            ["git", "-C", str(source), "rev-parse", "HEAD"], text=True).strip(),
            source_dirty=bool(subprocess.check_output(["git", "-C", str(source), "status", "--porcelain"])),
            binary=str(binary), binary_sha256=hashlib.sha256(binary.read_bytes()).hexdigest(),
            argv=sys.argv, resource_root=str(root), fixture_path="fixture",
            fixture_authoring="debug.setFluidSurface -> WeSetFluidSnapshot -> normal session save/load",
            fixture_sha256=hashlib.sha256((out / "fixture" / "world.synworld").read_bytes()).hexdigest(),
            codec=str(codec), codec_sha256=hashlib.sha256(codec.read_bytes()).hexdigest(),
            source_diff_sha256=hashlib.sha256(subprocess.check_output(
                ["git", "-C", str(source), "diff", "HEAD"])).hexdigest(),
            driver_sha256=hashlib.sha256(Path(__file__).read_bytes()).hexdigest(),
            size=args.size, frames=frames)
        (out / "manifest.json").write_text(json.dumps(manifest, indent=2) + "\n")
        return 0
    finally:
        quit_engine(args.port, proc)


if __name__ == "__main__":
    raise SystemExit(main())
