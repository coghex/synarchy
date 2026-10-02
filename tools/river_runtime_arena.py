#!/usr/bin/env python3
"""Author and observe a controlled river laboratory over the existing socket API.

This is a manual characterization, not a green regression gate, and it is not
registered with CI or the probe runner. It injects a FINITE initial water
charge; it does not implement an upstream river source. Socket observations are
whole-z ceilings and wet/dry only, taken after wall-clock intervals, so a run
reproduces the recipe and the observation procedure, never an exact tick
count or the historical samples. For exact, fixed-step solver quantities use
the hydraulic harness (`cabal run -v0 exe:river-runtime-harness`, #2719).

All config, saves, logs and results live in a fresh output directory: the
engine boots with that directory's own resource root (a copy of the tracked
config, without per-machine *.local.yaml), so its saves land there too. The
console port is session-owned: by default a free loopback port the OS assigns,
never 8008, and the driver stops only the engine it launched. The recipe is
ordinary JSON, independent of the parallel arena-scenario implementation.
"""
from __future__ import annotations

import argparse
import hashlib
import json
import os
from pathlib import Path
import shutil
import socket
import subprocess
import time

from probelib import GUI_PORT, boot, send_json, poll_until

REPO = Path(__file__).resolve().parent.parent
PAGE = "river_runtime_lab"
CLASSIFICATION = "characterization, not behaviour approval"


def session_port() -> int:
    """A free loopback port for this session: the OS's choice, never 8008."""
    while True:
        with socket.socket(socket.AF_INET, socket.SOCK_STREAM) as probe:
            probe.bind(("127.0.0.1", 0))
            port = probe.getsockname()[1]
        if port != GUI_PORT:
            return port


def stop_owned(proc: subprocess.Popen, timeout: float = 15.0) -> None:
    """Stop the engine this driver launched, through its process handle only.

    Never through the console port: once the child has exited, another
    engine may have bound the same port, and an ``engine.quit()`` sent there
    would stop that one instead. A child that has already exited is reaped
    and nothing else is touched.
    """
    if proc.poll() is not None:
        return
    proc.terminate()
    try:
        proc.wait(timeout=timeout)
    except subprocess.TimeoutExpired:
        proc.kill()
        proc.wait(timeout=10)


def recipe() -> dict:
    terrain = {}
    water = {}
    pairs = []
    for name, source, target, target_z, units in (
        ("raised-sill", (7, -12), (8, -12), -3, 24),
        ("downhill-control", (7, -8), (8, -8), -5, 24),
        ("one-level-interior", (7, -4), (8, -4), -4, 8),
        ("one-level-seam", (15, -4), (16, -4), -4, 8),
    ):
        terrain[source] = -4
        terrain[target] = target_z
        water[source] = -32 + units
        pairs.append(dict(name=name, source=list(source), target=list(target)))
    # A dammed main channel and an initially closed side diversion. Walls are
    # the arena's untouched z=0 terrain; every authored surface stays below it.
    for x in range(-20, 29):
        terrain[x, 8] = -4
        if x < 0:
            water[x, 8] = -16
    terrain[0, 8] = 0  # dam
    for y in range(9, 21):
        terrain[-4, y] = -5
    terrain[-4, 9] = 0  # diversion gate
    for x in range(-7, 0):
        for y in range(20, 24):
            terrain[x, y] = -6
    return dict(
        page=PAGE,
        terrain=[dict(x=x, y=y, z=z) for (x, y), z in sorted(terrain.items())],
        water=[dict(x=x, y=y, exact=u) for (x, y), u in sorted(water.items())],
        pairs=pairs,
        dam=dict(x=0, y=8, closed_z=0, open_z=-4),
        diversion=dict(x=-4, y=9, closed_z=0, open_z=-5),
        limitations=["finite initial water, no continuous supply",
                     "all 25 arena chunks are resident",
                     "socket surface observations are rounded ceilings",
                     "wall-clock observation; no exact tick-count claim"],
    )


def main() -> int:
    parser = argparse.ArgumentParser(
        description=__doc__, formatter_class=argparse.RawDescriptionHelpFormatter)
    parser.add_argument("--engine", type=Path, required=True,
                        help="the synarchy executable to characterize")
    parser.add_argument("--out", type=Path, required=True,
                        help="a NEW directory for the recipe, observations, "
                             "transcript, manifest, engine log and resource root")
    parser.add_argument("--port", type=int, default=None,
                        help="console port (default: a free loopback port; "
                             f"{GUI_PORT} is refused)")
    args = parser.parse_args()
    if args.port is None:
        args.port = session_port()
    if args.port == GUI_PORT:
        parser.error(f"port {GUI_PORT} belongs to the owner's GUI; omit --port")
    out = args.out.resolve()
    out.mkdir(parents=True, exist_ok=False)
    root = out / "resource-root"
    root.mkdir()
    for name in ("scripts", "assets", "data"):
        (root / name).symlink_to(REPO / name, target_is_directory=True)
    shutil.copytree(REPO / "config", root / "config",
                    ignore=shutil.ignore_patterns("*.local.yaml"))
    engine = args.engine.resolve(strict=True)
    os.environ["SYNARCHY_PROBE_ENGINE_EXE"] = str(engine)
    spec = recipe()
    (out / "recipe.json").write_text(json.dumps(spec, indent=2) + "\n")
    observations = []
    transcript = out / "socket.jsonl"

    def lua(code):
        result = send_json(args.port, code, timeout=45)
        with transcript.open("a") as stream:
            stream.write(json.dumps(dict(code=code, result=result)) + "\n")
        if isinstance(result, dict) and result.get("error"):
            raise RuntimeError(result)
        return result

    def carve(rows):
        for start in range(0, len(rows), 40):
            data = ",".join("{%d,%d,%d}" % (r["x"], r["y"], r["z"])
                            for r in rows[start:start+40])
            result = lua(f"local rows={{{data}}}; for _,r in ipairs(rows) do "
                         f"for z=r[3]+1,0 do assert(world.setCell('{PAGE}',r[1],r[2],z,'air')) end; "
                         f"assert(world.setCell('{PAGE}',r[1],r[2],r[3],'basalt')) end; return true")
            if result is not True:
                raise RuntimeError(("carve rejected", result))

    def terrain_matches(rows):
        data = ",".join("{%d,%d,%d}" % (r["x"], r["y"], r["z"]) for r in rows)
        return lua(f"local rows={{{data}}}; for _,r in ipairs(rows) do "
                   "local _,z=world.getTerrainAt(r[1],r[2]); "
                   "if z~=r[3] then return false end end; return true") is True

    def observe(label):
        points = sorted({(r["x"], r["y"]) for r in spec["terrain"]})
        data = ",".join("{%d,%d}" % p for p in points)
        cells = lua(f"local points={{{data}}}; local out={{}}; for _,p in ipairs(points) do "
                    "local _,z=world.getTerrainAt(p[1],p[2]); local k,s=world.getFluidAt(p[1],p[2]); "
                    "out[#out+1]={x=p[1],y=p[2],terrain=z,wet=k~=nil,kind=k,ceiling=s} end; return out")
        if not isinstance(cells, list) or len(cells) != len(points):
            raise RuntimeError(("incomplete observation", cells))
        observations.append(dict(label=label, cells=cells, monotonic=time.monotonic()))
        (out / "observations.json").write_text(json.dumps(observations, indent=2) + "\n")
        print(label, flush=True)

    def run_interval(label):
        if lua("return engine.setPaused(false)") is not True:
            raise RuntimeError("unpause rejected")
        time.sleep(3)
        if lua("return engine.setPaused(true)") is not True:
            raise RuntimeError("pause rejected")
        time.sleep(0.3)  # let already-queued world writebacks land
        observe(label)

    proc = None

    def own(launched):
        nonlocal proc
        proc = launched

    try:
        # Own the engine from the instant it exists, so an interrupt
        # during boot cannot strand it (#1682).
        boot(args.port, log=str(out / "engine.log"),
             args=["--resource-root", str(root)], ready_timeout=60,
             on_launch=own)
        lua("engine.setPaused(true); return true")
        # Content setup only; no gameplay script or scenario-system dependency.
        for family, loader in (("substances", "loadSubstanceYaml"),
                               ("materials", "loadMaterialYaml")):
            for path in sorted((REPO / "data" / family).glob("*.yaml")):
                lua(f"engine.{loader}({json.dumps(str(path.relative_to(REPO)))}); return true")
        lua(f"world.initArena('{PAGE}'); return true")
        result = lua("return world.waitForInit(30)")
        if not isinstance(result, str) or "done" not in result.lower():
            raise RuntimeError(("arena failed", result))
        lua(f"world.show('{PAGE}'); return true")
        if not poll_until(15, lambda: lua("return world.getTerrainAt(0,0)") is not None):
            raise RuntimeError("arena never became queryable")
        carve(spec["terrain"])
        if not poll_until(30, lambda: terrain_matches(spec["terrain"])):
            raise RuntimeError("terrain did not match recipe")
        for row in spec["water"]:
            if lua(f"return debug.setFluidSurface('{PAGE}',{row['x']},{row['y']},'river',{row['exact']})") is not True:
                raise RuntimeError("fluid edit rejected")
        if not poll_until(20, lambda: lua("local k,s=world.getFluidAt(7,-12); return k=='river' and s==-1") is True):
            raise RuntimeError("water not applied")
        observe("authored-paused")
        run_interval("dam-closed-diversion-closed")
        gate = spec["diversion"]
        change = [dict(x=gate["x"], y=gate["y"], z=gate["open_z"])]
        carve(change)
        if not poll_until(20, lambda: terrain_matches(change)):
            raise RuntimeError("diversion edit not applied")
        run_interval("dam-closed-diversion-open")
        gate = spec["dam"]
        change = [dict(x=gate["x"], y=gate["y"], z=gate["open_z"])]
        carve(change)
        if not poll_until(20, lambda: terrain_matches(change)):
            raise RuntimeError("dam edit not applied")
        run_interval("dam-open-diversion-open")
        manifest = dict(source_commit=subprocess.check_output(
            ["git", "-C", str(REPO), "rev-parse", "HEAD"], text=True).strip(),
            engine=str(engine), engine_sha256=hashlib.sha256(engine.read_bytes()).hexdigest(),
            driver="tools/river_runtime_arena.py",
            driver_sha256=hashlib.sha256(Path(__file__).read_bytes()).hexdigest(),
            recipe_sha256=hashlib.sha256((out / "recipe.json").read_bytes()).hexdigest(),
            classification=CLASSIFICATION,
            resource_root="isolated copy under the output directory",
            limitations=spec["limitations"], completed=True)
        (out / "manifest.json").write_text(json.dumps(manifest, indent=2) + "\n")
        return 0
    finally:
        if proc is not None:
            stop_owned(proc)


if __name__ == "__main__":
    raise SystemExit(main())
