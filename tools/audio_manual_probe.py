#!/usr/bin/env python3
"""Audio listening/device and rendered Settings probe (manual-only, needs GPU).

--interactive opens a visible game and physical output; requires a terminal.
--offscreen-check verifies the same playback/UI sequence using forced-null output,
without opening a window. Neither mode writes the owner's config. --describe
only prints the protocol descriptor. Physical listening/recovery verdicts are
reported separately from the four automated checks.
"""
from __future__ import annotations

import argparse
import json
from pathlib import Path
import subprocess
import sys
import tempfile
import time

from audio_null_probe import fixture_root
import probe_protocol
from probelib import boot, init_arena, send, send_json

CHECKS = [
    ('device', 'negotiated output and callback progress match the requested mode'),
    ('settings', 'real input reaches rendered Audio settings and its three sliders'),
    ('playback', 'sample/synth/spatial/loop/pause sequence reaches the running mixer'),
    ('shutdown', 'the owned graphical/offscreen process exits cleanly'),
]
DESCRIPTOR = probe_protocol.build_descriptor('audio_manual', CHECKS)


def run(port: int, interactive: bool, rep: probe_protocol.Reporter) -> int:
    owned: list[subprocess.Popen] = []
    ready = False
    labels = dict(CHECKS)
    evidence = {'mode': 'interactive' if interactive else 'offscreen', 'checks': {}, 'facts': {}}
    evidence_path = Path(rep.engine_log_path('audio_manual_evidence.json',
                         f'/tmp/synarchy-audio-manual-{port}.json'))
    temporary = tempfile.TemporaryDirectory(prefix='synarchy-audio-manual-')

    def lua(command):
        return send_json(port, command, idle=0.05)

    def status():
        value = lua('return audio.getStatus()')
        if not isinstance(value, dict):
            raise RuntimeError(f'invalid audio status: {value!r}')
        return value

    def accepted(command):
        if lua(command) is not True:
            raise RuntimeError(f'audio command was not accepted: {command}')

    def wait_for(action, predicate, timeout=30):
        deadline = time.monotonic() + timeout
        while True:
            value = action()
            if predicate(value):
                return value
            if time.monotonic() >= deadline:
                raise RuntimeError(f'timed out waiting for probe state: {value!r}')
            time.sleep(0.05)

    def check(key, condition, detail=None):
        evidence['checks'][key] = {'passed': bool(condition), 'detail': detail}
        if not rep.check(key, bool(condition), labels[key], detail):
            raise AssertionError(labels[key])

    def widget(label):
        values = lua('return ui.dumpWidgets()')
        return next((w for w in values if w.get('label', '').strip() == label), None) if isinstance(values, list) else None

    def click(label):
        target = wait_for(lambda: widget(label), lambda value: value is not None)
        bounds = target['bounds']
        # dumpWidgets and input.click both use framebuffer coordinates.
        x, y = bounds['x'] + bounds['w'] / 2, bounds['y'] + bounds['h'] / 2
        lua(f'return input.click({x},{y})')

    def facts(tag):
        snapshot = status()
        native = snapshot.get('native') or {}
        rate = native.get('sampleRate', 0)
        details = {key: native.get(key) for key in (
            'sink', 'backend', 'deviceName', 'sampleRate', 'periodFrames', 'ringFill',
            'ringMin', 'ringMax', 'callbacks', 'underruns', 'transitions')}
        details['lifecycle'] = snapshot['lifecycle']
        details['ringFillMs'] = 1000 * native.get('ringFill', 0) / rate if rate else None
        evidence['facts'][tag] = details
        rep.info(f'{tag}: {json.dumps(details, sort_keys=True)}', details)
        return snapshot

    try:
        root = Path(temporary.name)
        fixture_root(root)
        mode = () if interactive else ('--offscreen', '--size', '1280x720')
        proc = boot(port, mode=mode, ready_timeout=60, on_launch=owned.append,
                    log=rep.engine_log_path('audio_manual_engine.log', f'/tmp/synarchy-audio-manual-{port}.log'),
                    args=[*rep.engine_args(), '--resource-root', str(root)])
        ready = True
        initial = wait_for(status, lambda s: (s.get('native') or {}).get('callbacks', 0) > 1)
        native = initial['native']
        check('device', native['sampleRate'] > 0 and native['periodFrames'] > 0
              and (interactive or (initial['lifecycle'] == 'running_null'
                                   and native['sink'] == 'null' and native['backend'] == 14)))
        facts('Negotiated output; ring fill excludes device buffering')
        # Start quietly, with only the isolated process's live volumes changed.
        lua('return audio.setVolumes({master=50,world=70,ui=70})')
        click('Settings')
        click('Audio')
        wait_for(lambda: lua('return ui.dumpWidgets()'),
                 lambda ws: isinstance(ws, list) and all(
                     any(w.get('name') == f'audio_{key}' for w in ws)
                     for key in ('master', 'world', 'ui')))
        capture = str(Path(rep.engine_log_path('audio_settings.png',
                      f'/tmp/synarchy-audio-settings-{port}.png')).resolve())
        shot = send_json(port, f'return debug.captureScreenshot({json.dumps(capture)})', timeout=30)
        check('settings', isinstance(shot, dict) and shot.get('path') == capture
              and Path(capture).is_file(), {'screenshot': capture})
        click('Back')
        lua('return audio.setVolumes({master=50,world=70,ui=70})')
        playback_start = status()['native']
        before = playback_start['accepted']
        for sound in ('menu_selected', 'menu_back'):
            rep.note(f'Playing {sound}')
            accepted(f"return audio.play('{sound}')")
            time.sleep(0.8)
        rep.note('Sample loop, then stop fade')
        accepted("return audio.startLoop('sample','probe_sample')")
        time.sleep(1)
        accepted("return audio.stopLoop('sample')")
        init_arena(port, 'audio_probe', timeout=30)
        lua('camera.goToTile(0,0); camera.setZoom(0.25); return true')
        wait_for(lambda: lua('return camera.getFacing()'), lambda facing: facing == 0)
        options = "{pageId='audio_probe',position={x=-8,y=8,z=0}}"
        accepted(f"return audio.startLoop('spatial','probe_world',{options})")
        wait_for(status, lambda s: s['native']['activeLoops'] == 1)
        for label, x, y in [('left', -8, 8), ('centre', 0, 0), ('right', 8, -8)]:
            rep.note(f'Spatial synth: {label}')
            accepted("return audio.updateLoop('spatial',"
                f"{{pageId='audio_probe',position={{x={x},y={y},z=0}}}})")
            time.sleep(1)
        rep.note('Player pause: World freezes while the menu cue continues')
        accepted('return engine.setPaused(true)')
        accepted("return audio.play('menu_selected')")
        time.sleep(1)
        accepted('return engine.setPaused(false)')
        time.sleep(1)
        accepted("return audio.stopLoop('spatial')")
        ended = wait_for(status, lambda s: s['native']['activeVoices'] == 0)
        check('playback', ended['native']['accepted'] >= before + 5
              and ended['native']['nonfiniteSamples'] == 0 and ended['native']['mixPeak'] > 0
              and ended['transport']['invalidDrops'] == 0 and not ended['drops'],
              {'status': ended})
        counts = {'beforePlayback': playback_start['underruns'],
                  'duringPlayback': ended['native']['underruns'] - playback_start['underruns']}
        evidence['underruns'] = counts
        rep.info(f'Underruns: {json.dumps(counts)}', counts)
        if interactive:
            heard = input('Were the cues clear, the pan left/centre/right, and pause/resume smooth? [y/n] ').strip().lower()
            rep.info(f'Owner listening verdict: {heard or "no answer"}')
            evidence['ownerListening'] = heard
            evidence['physicalListeningAccepted'] = initial['lifecycle'] == 'running_real' and heard == 'y'
            if initial['lifecycle'] != 'running_real' or heard != 'y':
                rep.warn('Physical listening acceptance remains incomplete.')
            rep.note('Optional recovery check: switch/disconnect/reconnect the output device, then enter y; enter s to skip.')
            baseline = facts('Before manual device change')
            recovery = input('Device change completed? [y/s] ').strip().lower()
            if recovery == 'y':
                time.sleep(2)
                after = facts('After manual device change')
                lua("return audio.play('menu_selected')")
                evidence['recovery'] = {'before': baseline, 'after': after,
                    'ownerHeardCue': input('Was the cue audible after recovery? [y/n] ').strip().lower()}
                rep.info('Recovery evidence', evidence['recovery'])
            else:
                rep.skip('Owner device recovery was not exercised.')
        else:
            rep.note('Offscreen verification only; no physical listening or device-recovery verdict.')
        facts('Before clean shutdown')
        send(port, 'engine.quit()', expect_result=False, idle=0.03)
        check('shutdown', proc.wait(timeout=15) == 0)
        return 0
    except AssertionError:
        return 1
    except (OSError, RuntimeError, EOFError, subprocess.TimeoutExpired) as error:
        rep.abort(str(error))
        return 2
    finally:
        for proc in owned:
            if proc.poll() is None and ready:
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
        evidence_path.parent.mkdir(parents=True, exist_ok=True)
        evidence_path.write_text(json.dumps(evidence, indent=2) + '\n')
        rep.note(f'Evidence: {evidence_path}')


def main() -> int:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('--port', type=int, default=9188)
    parser.add_argument('--describe', action='store_true')
    mode = parser.add_mutually_exclusive_group()
    mode.add_argument('--interactive', action='store_true')
    mode.add_argument('--offscreen-check', action='store_true')
    args = parser.parse_args()
    if args.describe:
        print(DESCRIPTOR.to_json())
        return 0
    if not (args.interactive or args.offscreen_check):
        parser.error('choose --interactive (visible window and speakers) or --offscreen-check (null output)')
    if args.interactive and not sys.stdin.isatty():
        parser.error('--interactive requires a terminal for listening verdicts')
    rep = probe_protocol.reporter_from_env(DESCRIPTOR)
    try:
        return run(args.port, args.interactive, rep)
    finally:
        rep.close()


if __name__ == '__main__':
    raise SystemExit(main())
