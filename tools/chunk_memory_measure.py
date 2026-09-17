#!/usr/bin/env python3
"""Manual-only CRS-2 experiment. Offscreen needs a GPU; never CI eligible.

One invocation owns one fresh process. Matrix orchestration and verdicts live
in docs/chunk_memory_measurement.md. No build, RTS tuning or gameplay policy is
changed by this tool. Supply a committed production binary explicitly.
"""
from __future__ import annotations

import argparse
import gzip
import hashlib
import json
import os
from pathlib import Path
import platform
import shutil
import socket
import subprocess
import sys
import tempfile
import threading
import time

from probelib import send

ROOT = Path(__file__).resolve().parent.parent
GIB = 1024 ** 3


def rss_bytes(value, source, system=sys.platform):
    """ps RSS is KiB on both hosts; getrusage's ru_maxrss has different units."""
    if source == "ps-rss-kib":
        return int(value) * 1024
    if source == "ru_maxrss":
        return int(value) * (1 if system == "darwin" else 1024)
    raise ValueError(f"unknown RSS source: {source}")


def canonical(x, y, size):
    u = x - y
    wrapped = (u + size // 2) % size - size // 2
    shift = (wrapped - u) // 2
    return x + shift, y - shift


def traversal(size):
    """Serpentine 3x3 footprints whose union has >=1000 canonical chunks."""
    for i, x in enumerate(range(-15, 16, 2)):
        ys = list(range(-15, 16, 2))
        for y in ys if i % 2 == 0 else reversed(ys):
            coords = sorted({canonical(a, b, size)
                             for a in range(x - 1, x + 2)
                             for b in range(y - 1, y + 2)})
            yield x, y, coords


def require_samples(samples, phases):
    for phase in phases:
        if not any(s['phase'] == phase and s['rssBytes'] > 0 for s in samples):
            raise RuntimeError(f"missing RSS samples for {phase}")


def require_region(result, coords):
    if not isinstance(result, list) or len(result) != len(coords):
        raise RuntimeError("incomplete terrain verification response")
    if not all(isinstance(v, dict) and v.get('loaded') is True for v in result):
        raise RuntimeError("requested terrain was not resident")


def require_json(raw):
    try:
        return json.loads(raw)
    except (ValueError, TypeError) as e:
        raise RuntimeError(f"console did not return JSON: {raw[:200]!r}") from e


class Sampler:
    def __init__(self, pid, interval):
        self.pid, self.interval = pid, interval
        self.phase = 'boot'
        self.samples, self.errors = [], []
        self.stop = threading.Event()
        self.thread = threading.Thread(target=self.run, daemon=True)

    def run(self):
        while not self.stop.is_set():
            started = time.monotonic()
            try:
                out = subprocess.run(['ps', '-o', 'rss=', '-p', str(self.pid)],
                                     capture_output=True, text=True, timeout=5)
                if out.returncode == 0 and out.stdout.strip():
                    self.samples.append({'timeSeconds': started, 'phase': self.phase,
                                         'rssBytes': rss_bytes(out.stdout, 'ps-rss-kib')})
            except (OSError, ValueError, subprocess.TimeoutExpired) as e:
                self.errors.append(str(e))
            self.stop.wait(max(0, self.interval - (time.monotonic() - started)))


def isolated_root(parent):
    path = Path(tempfile.mkdtemp(prefix='resource-root-', dir=parent))
    for name in ('assets', 'data', 'scripts'):
        (path / name).symlink_to(ROOT / name, target_is_directory=True)
    shutil.copytree(ROOT / 'config', path / 'config',
                    ignore=shutil.ignore_patterns('*.local.yaml', 'shell_history.txt'))
    return path


def run(args):
    output = Path(args.output).resolve()
    output.mkdir(parents=True, exist_ok=False)
    binary = Path(args.binary).resolve(strict=True)
    with socket.socket() as sock:
        sock.bind(('127.0.0.1', 0))
        port = sock.getsockname()[1]
    if port == 8008:
        raise RuntimeError('refusing reserved GUI port')
    resource = isolated_root(output)
    rts = [] if args.rts == 'production' else ['+RTS', '-A8M', '-RTS']
    command = [str(binary), '--' + args.mode, '--port', str(port),
               '--resource-root', str(resource)]
    if args.mode == 'offscreen':
        command += ['--size', '1280x720']
    command += rts
    record = {'schema': 1, 'status': 'incomplete', 'argv': command,
              'revision': subprocess.check_output(['git', 'rev-parse', 'HEAD'], cwd=ROOT, text=True).strip(),
              'binarySha256': hashlib.sha256(binary.read_bytes()).hexdigest(),
              'buildProfile': 'production: -O2 -optc-O3',
              'bakedRts': '-N -A128M', 'rtsOverride': rts, 'mode': args.mode,
              'worldSize': args.size, 'seed': 42, 'plates': 3,
              'platform': platform.platform(), 'logicalCpus': os.cpu_count(),
              'samplingIntervalSeconds': args.interval,
              'counter': 'ps -o rss: KiB converted to bytes; sampled, not exact peaks',
              'forcedGC': False, 'manualOnly': True, 'commands': [], 'observations': []}
    if sys.platform == 'darwin':
        record['hardware'] = subprocess.check_output(
            ['sysctl', 'hw.model', 'hw.memsize', 'hw.physicalcpu', 'hw.logicalcpu',
             'machdep.cpu.brand_string'], text=True).strip()
    logpath = output / 'engine.log'
    proc = sampler = None

    def query(lua, timeout=30):
        start = time.monotonic()
        raw = send(port, lua, timeout=timeout, idle=.025)
        record['commands'].append({'timeSeconds': start, 'elapsedSeconds': time.monotonic()-start,
                                   'lua': lua, 'raw': raw})
        if lua == 'return engine.getLoadStatus()' and raw.startswith('REJECTED: a load transaction replaced the session'):
            return None
        return require_json(raw)

    def poll(lua, accept, seconds=180):
        deadline = time.monotonic() + seconds
        last = None
        while time.monotonic() < deadline:
            if proc.poll() is not None:
                raise RuntimeError(f'engine exited during {lua}: {proc.returncode}')
            last = query(lua)
            if accept(last):
                return last
            time.sleep(.2)
        raise RuntimeError(f'timeout: {lua}: {last!r}')

    def observation(label):
        # The first read schedules a nonblocking simulation-owner sample.
        query('return world.getChunkMemory()')
        value = poll('return world.getChunkMemory()',
                     lambda v: isinstance(v, dict) and v.get('simulation', {}).get('available')
                     and v['simulation']['ageSeconds'] < 2, seconds=30)
        record['observations'].append({'label': label, 'phase': sampler.phase, 'value': value})
        return value

    def region(coords):
        calls = ','.join(f'world.getChunkInfo({x},{y})' for x, y in coords)
        values = query('return {' + calls + '}')
        require_region(values, coords)

    try:
        with logpath.open('w') as log:
            proc = subprocess.Popen(command, cwd=ROOT, stdout=log, stderr=subprocess.STDOUT)
            sampler = Sampler(proc.pid, args.interval)
            sampler.thread.start()
            deadline = time.monotonic() + 120
            while f'READY port={port}' not in logpath.read_text(errors='replace'):
                if proc.poll() is not None or time.monotonic() >= deadline:
                    raise RuntimeError('engine did not reach READY')
                time.sleep(.1)
            if args.mode == 'offscreen':
                poll('return require("scripts.startup_loader").isDone()', lambda v: v is True)
                query('require("scripts.ui_manager").ensureWorldView(); return true')
                poll('local w=require("scripts.world_view"); return w.texturesLoadedCount >= w.texturesNeeded',
                     lambda v: v is True)
            query('world.resetChunkMemoryWindow(); return true')
            sampler.phase = 'generation'
            if args.mode == 'offscreen':
                accepted = query('local v=require("scripts.world_view"); '
                                 'local w=require("scripts.world_manager"); '
                                 f'local id=w.createWorld({{worldId="measurement",seed=42,worldSize={args.size},'
                                 'plateCount=3,structural=v.structuralTextures}); '
                                 'if id then w.showWorld(id) end; return id == "measurement"')
                if accepted is not True:
                    raise RuntimeError('ordinary world creation refused')
            else:
                query(f'world.init("measurement",42,{args.size},3); world.show("measurement"); return true')
            poll('return world.getInitProgress()',
                 lambda v: isinstance(v, dict) and v.get('phase') == 3, seconds=1200)
            poll('return world.getActiveWorldId() == "measurement"', lambda v: v is True)
            if args.mode == 'offscreen':
                query('require("scripts.ui_manager").showMenu("world_view"); return true')
                poll('return require("scripts.ui_manager").isGameplayView()', lambda v: v is True)
            observation('generation-complete')
            sampler.phase = 'traversal'
            visited = set()
            for i, (x, y, coords) in enumerate(traversal(args.size)):
                query(f'camera.goToTile({x*16+8},{y*16+8}); '
                      f'world.loadChunksInRegion({x-1},{y-1},{x+1},{y+1},"measurement"); return true')
                remaining = query('return world.waitForChunks(120,"measurement")', timeout=130)
                if remaining != 0:
                    raise RuntimeError(f'chunk work incomplete: {remaining}')
                region(coords)
                visited.update(coords)
                if i % 8 == 0:
                    observation(f'traversal-{i}')
                if i % 32 == 0:
                    print(f'traversal {i}/256; {len(visited)} unique verified chunks', flush=True)
            if len(visited) < 1000:
                raise RuntimeError('traversal did not visit 1000 chunks')
            record['verifiedDistinctChunks'] = len(visited)
            observation('traversal-complete')
            if args.mode == 'offscreen':
                sampler.phase = 'gameplay'
                query('camera.goToTile(8,8); world.loadChunksInRegion(-5,-5,4,4,"measurement"); return true')
                if query('return world.waitForChunks(120,"measurement")', timeout=130) != 0:
                    raise RuntimeError('colony base did not load')
                # Find real dry ground; species/catalogue come from ordinary startup.
                sites = query('local s={}; for x=-20,20 do for y=-20,20 do '
                              'local z=world.getTerrainAt(x,y); '
                              'if z and z==world.getTerrainAt(x+1,y) and z==world.getTerrainAt(x,y+1) '
                              'and not world.getFluidAt(x,y) then s[#s+1]={x=x,y=y,z=z}; '
                              'if #s==5 then return s end end end end; return s')
                if not isinstance(sites, list) or len(sites) != 5:
                    raise RuntimeError('no five dry unit sites')
                ids = []
                for site in sites:
                    uid = query(f'return unit.spawn("acolyte",{site["x"]},{site["y"]},{site["z"]},"player")')
                    if not isinstance(uid, int) or uid <= 0:
                        raise RuntimeError('unit spawn failed')
                    ids.append(uid)
                record['colony'] = {'unitIds': ids, 'requestedBaseChunks': 100, 'durationSeconds': 30}
                query('engine.setPaused(false); return true')
                for i in range(10):
                    time.sleep(3)
                    observation(f'gameplay-{i}')
                surviving = query('return unit.getAllIds()')
                if not isinstance(surviving, list) or not set(ids).issubset(surviving):
                    raise RuntimeError('five-unit scenario did not remain populated')
                if sys.platform == 'darwin':
                    result = subprocess.run(['vmmap', '-summary', str(proc.pid)],
                                            capture_output=True, text=True, timeout=30)
                    record['vmmapReturnCode'] = result.returncode
                    (output / 'vmmap-summary.txt').write_text(result.stdout + result.stderr)
            sampler.phase = 'save'
            if query('return engine.saveWorld("measurement","chunk-memory")', timeout=180) is not True:
                raise RuntimeError('save request refused')
            saved = poll('return engine.getSaveStatus()', lambda v: isinstance(v, dict)
                         and v.get('phase') in ('SaveCaptureComplete','SaveFailed'), seconds=180)
            if saved['phase'] != 'SaveCaptureComplete':
                raise RuntimeError(f'save failed: {saved}')
            sampler.phase = 'loading'
            if query('return engine.loadSave("chunk-memory")') is not True:
                raise RuntimeError('load request refused')
            loaded = poll('return engine.getLoadStatus()', lambda v: isinstance(v, dict)
                          and v.get('phase') in ('LoadPublished','LoadFailed','LoadReconciliationFailed'), seconds=1200)
            if loaded['phase'] != 'LoadPublished':
                raise RuntimeError(f'load failed: {loaded}')
            if query('return world.waitForChunks(120,"measurement")', timeout=130) != 0:
                raise RuntimeError('load warmup incomplete')
            observation('load-complete')
            time.sleep(2)
            observation('load-settled')
            require_samples(sampler.samples, ['generation','traversal','loading'] +
                            (['gameplay'] if args.mode == 'offscreen' else []))
            sampler.phase = 'shutdown'
            query('engine.quit(); return true')
            proc.wait(timeout=30)
            if proc.returncode != 0:
                raise RuntimeError(f'unclean exit: {proc.returncode}')
            record['status'] = 'complete'
    except BaseException as e:
        record['failure'] = f'{type(e).__name__}: {e}'
        raise
    finally:
        if proc is not None and proc.poll() is None:
            proc.terminate()
            try:
                proc.wait(timeout=10)
            except subprocess.TimeoutExpired:
                proc.kill()
                proc.wait(timeout=10)
        if sampler is not None:
            sampler.stop.set()
            sampler.thread.join(timeout=6)
            record['samples'] = sampler.samples
            record['samplerErrors'] = sampler.errors
            record['phasePeaksBytes'] = {p: max(s['rssBytes'] for s in sampler.samples if s['phase']==p)
                                         for p in sorted({s['phase'] for s in sampler.samples})}
        record['exitCode'] = None if proc is None else proc.returncode
        (output / 'measurement.json').write_text(json.dumps(record, indent=2) + '\n')
        if logpath.exists():
            with gzip.open(output / 'engine.log.gz', 'wb') as dst:
                dst.write(logpath.read_bytes())
            logpath.unlink()
        shutil.rmtree(resource)


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('--binary', required=True)
    parser.add_argument('--output', required=True, help='new directory; existing results are never overwritten')
    parser.add_argument('--size', type=int, choices=[64,256], required=True)
    parser.add_argument('--rts', choices=['production','small-nursery'], required=True)
    parser.add_argument('--mode', choices=['headless','offscreen'], default='headless')
    parser.add_argument('--interval', type=float, default=.25)
    args = parser.parse_args()
    if not 0.05 <= args.interval <= 5:
        parser.error('sample interval must be between 0.05 and 5 seconds')
    run(args)


if __name__ == '__main__':
    main()
