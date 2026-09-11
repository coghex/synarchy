#!/usr/bin/env python3
"""Exercise sample+synth audio through a real headless boot and null callback.

Uses an isolated resource root, generated decoder fixtures, an empty arena and
the public Lua API. It never initializes a physical output device. --describe
emits probe-result/v1 checks without building or launching anything.
"""
from __future__ import annotations

import argparse
from pathlib import Path
import shutil
import subprocess
import tempfile
import time

import yaml
import probe_protocol
from probelib import boot, send, send_json, init_arena

ROOT = Path(__file__).resolve().parents[1]
CHECKS = [
    ('null_boot', 'real boot uses the pinned null backend and advancing callbacks'),
    ('mixed_sources', 'resident sample and synth loops coexist through the worker'),
    ('spatial_listener', 'headless camera/page publication admits spatial World audio'),
    ('player_pause', 'player pause drops new freeze voices while UI continues'),
    ('resume', 'player resume admits World voices again'),
    ('volumes', 'live and saved volumes remain separate and persist in the isolated root'),
    ('unknown_sound', 'unknown sounds produce bounded observable worker diagnostics'),
    ('session_reset', 'destroy-all advances the epoch and clears voices and logical loops'),
    ('shutdown', 'the real process exits cleanly after joining audio'),
]
DESCRIPTOR = probe_protocol.build_descriptor('audio_null', CHECKS)


def fixture_root(destination: Path) -> None:
    """Read-only shared resources, private config/catalog/audio fixture files."""
    (destination / 'scripts').symlink_to(ROOT / 'scripts', target_is_directory=True)
    for family in ('data', 'assets'):
        target = destination / family
        target.mkdir()
        for source in (ROOT / family).iterdir():
            if source.name != 'audio':
                (target / source.name).symlink_to(source, target_is_directory=source.is_dir())
    shutil.copytree(ROOT / 'config', destination / 'config',
                    ignore=shutil.ignore_patterns('*.local.yaml'))
    shutil.copytree(ROOT / 'data/audio', destination / 'data/audio')
    if (ROOT / 'assets/audio').is_dir():
        shutil.copytree(ROOT / 'assets/audio', destination / 'assets/audio')
    else:
        (destination / 'assets/audio').mkdir()
    shutil.copyfile(ROOT / 'test-headless/data/audio/tone.wav', destination / 'assets/audio/probe.wav')
    path = destination / 'data/audio/sounds.yaml'
    catalog = yaml.safe_load(path.read_text())
    catalog['sounds'].extend([
        {'id': 'probe_sample', 'type': 'ui_effect',
         'source': {'sample': {'path': 'assets/audio/probe.wav'}},
         'policy': {'loop': {'allowed': True}}},
        {'id': 'probe_world', 'type': 'world_effect',
         'source': {'synth': {'instrument': 'menu_selected', 'gate_ms': 10000,
            'timbre': {'generator': {'waveform': 'sine', 'frequency_hz': 440},
                       'envelope': {'attack_ms': 2, 'decay_ms': 10, 'sustain_level': 0.3},
                       'filter': {'mode': 'bypass'}, 'gain_db': -12}}},
         'policy': {'loop': {'allowed': True}}},
    ])
    path.write_text(yaml.safe_dump(catalog, sort_keys=False))


def run(port: int, rep: probe_protocol.Reporter) -> int:
    owned: list[subprocess.Popen] = []
    ready = False
    stopped = False
    labels = dict(CHECKS)

    def check(key: str, condition: bool, detail=None) -> None:
        if not rep.check(key, bool(condition), labels[key], detail):
            raise AssertionError(labels[key])

    def status():
        value = send_json(port, 'return audio.getStatus()', idle=0.03)
        if not isinstance(value, dict):
            raise RuntimeError(f'audio.getStatus returned {value!r}')
        if not isinstance(value.get('drops'), dict):
            value['drops'] = {}
        return value

    def await_status(predicate, seconds=5):
        deadline = time.monotonic() + seconds
        latest = status()
        while not predicate(latest) and time.monotonic() < deadline:
            time.sleep(0.02)
            latest = status()
        return latest

    def lua(command):
        return send_json(port, command, idle=0.03)

    temporary = tempfile.TemporaryDirectory(prefix='synarchy-audio-null-')
    try:
        root = Path(temporary.name)
        fixture_root(root)
        proc = boot(port, log=rep.engine_log_path('audio_null_engine.log',
                    f'/tmp/synarchy-audio-null-{port}.log'),
                    args=[*rep.engine_args(), '--resource-root', str(root)],
                    ready_timeout=45, on_launch=owned.append)
        ready = True
        initial = await_status(lambda s: s.get('native', {}).get('callbacks', 0) > 1)
        native = initial.get('native', {})
        # ma_backend_null is 14 in the checksum-pinned miniaudio 0.11.25.
        check('null_boot', initial.get('lifecycle') == 'running_null'
              and native.get('sink') == 'null' and native.get('backend') == 14
              and native.get('callbacks', 0) > 1 and native.get('renderedFrames', 0) > 0,
              {'status': initial})

        init_arena(port, 'audio_probe', timeout=30)
        lua("camera.goToTile(0,0); return true")
        options = "{pageId='audio_probe',position={x=0,y=0,z=0}}"
        enqueued = lua("return {audio.startLoop('sample','probe_sample'),"
                       f"audio.startLoop('world','probe_world',{options})}}")
        playing = await_status(lambda s: s['native']['activeLoops'] == 2)
        native = playing['native']
        check('mixed_sources', enqueued == [True, True] and native['samples'] == 1
              and native['activeLoops'] == 2 and native['peakVoices'] >= 2
              and native['mixPeak'] > 0 and native['nonfiniteSamples'] == 0,
              {'status': playing})
        check('spatial_listener', not playing['drops'].get('NoListener', 0)
              and not playing['drops'].get('WrongPage', 0) and native['activeLoops'] == 2)

        before = native['accepted']
        accepted_pause = lua('return engine.setPaused(true)')
        lua(f"return audio.play('probe_world',{options})")
        lua("return audio.play('menu_selected')")
        paused = await_status(lambda s: s['drops'].get('PausedWorld', 0) >= 1
                              and s['native']['accepted'] > before)
        check('player_pause', accepted_pause is True and paused['drops'].get('PausedWorld', 0) == 1
              and paused['native']['accepted'] == before + 1 and paused['native']['activeLoops'] == 2,
              {'status': paused})
        accepted_resume = lua('return engine.setPaused(false)')
        lua(f"return audio.play('probe_world',{options})")
        resumed = await_status(lambda s: s['native']['accepted'] == before + 2)
        check('resume', accepted_resume is True and resumed['native']['accepted'] == before + 2)

        original = lua('return audio.getSavedVolumes()')
        values = {'master': 31, 'world': 43, 'ui': 57}
        lua('return audio.setVolumes({master=31,world=43,ui=57})')
        preview = status()['volumes']
        persisted_before = lua('return audio.getSavedVolumes()')
        saved = lua('return audio.saveVolumes({master=31,world=43,ui=57})')
        persisted = lua('return audio.getSavedVolumes()')
        check('volumes', preview == values and persisted_before == original and saved is True
              and persisted == values and (root / 'config/audio.local.yaml').is_file())

        lua("return audio.play('missing_probe_sound')")
        missing = await_status(lambda s: s['drops'].get('UnknownSound', 0) == 1)
        check('unknown_sound', missing['drops'].get('UnknownSound', 0) == 1
              and missing['diagnosticWarnings'] >= 1 and len(missing['lastError']) <= 512)

        epoch = missing['epoch']
        lua('world.destroyAll(); return true')
        reset = await_status(lambda s: s['epoch'] == epoch + 1 and s['native']['activeVoices'] == 0)
        lua("return audio.stopLoop('sample')")
        retired = await_status(lambda s: s['drops'].get('MissingLoop', 0) >= 1)
        check('session_reset', reset['epoch'] == epoch + 1 and reset['native']['activeLoops'] == 0
              and reset['native']['activeVoices'] == 0 and reset['volumes'] == values
              and retired['drops'].get('MissingLoop', 0) >= 1
              and reset['snapshotSequence'] > initial['snapshotSequence'], {'status': reset})

        send(port, 'engine.quit()', expect_result=False, idle=0.03)
        result = proc.wait(timeout=15)
        stopped = True
        check('shutdown', result == 0, {'exitCode': result})
        rep.note('PASS: headless audio reached sample+synth PCM and joined the forced-null callback')
        return 0
    except AssertionError:
        return 1
    except (OSError, RuntimeError, subprocess.TimeoutExpired) as error:
        rep.abort(str(error))
        return 2
    finally:
        for proc in owned:
            if proc.poll() is None:
                # A failed bind may belong to someone else. Only talk to the
                # console after this process's own READY marker was observed.
                if ready and not stopped:
                    try:
                        send(port, 'engine.quit()', expect_result=False, timeout=2, idle=0.03)
                        proc.wait(timeout=10)
                    except (OSError, subprocess.TimeoutExpired):
                        pass
                if proc.poll() is None:
                    proc.terminate()
                    try:
                        proc.wait(timeout=5)
                    except subprocess.TimeoutExpired:
                        proc.kill()
                        proc.wait(timeout=5)
        temporary.cleanup()


def main() -> int:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('--port', type=int, default=9187)
    parser.add_argument('--describe', action='store_true')
    args = parser.parse_args()
    if args.describe:
        print(DESCRIPTOR.to_json())
        return 0
    rep = probe_protocol.reporter_from_env(DESCRIPTOR)
    try:
        return run(args.port, rep)
    finally:
        rep.close()


if __name__ == '__main__':
    raise SystemExit(main())
