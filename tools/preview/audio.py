"""Audio authoring through the real preview UI, using forced-null output."""
from __future__ import annotations

import json
from pathlib import Path
import shutil
import tempfile
import time

from probelib import poll_until, quit_engine, send, send_json
from preview.harness import boot_preview, check, click_element, dump, poll_state


def _status(port):
    return send_json(port, 'return audio.getStatus()') or {}


def _await(port, predicate):
    value = poll_until(10, lambda: (lambda s: s if predicate(s) else None)(_status(port)))
    if not value:
        raise RuntimeError(f'audio status timed out: {_status(port)}')
    return value


def _click(port, name):
    click_element(port, dump(port)['audio']['buttons'][f'preview_audio_{name}']['bounds'])
    time.sleep(0.12)


def _capture(port, stem):
    path = f'/tmp/synarchy-preview-audio-{port}-{stem}.png'
    result = send_json(port, f'return debug.captureScreenshot({json.dumps(path)})', timeout=30)
    if not isinstance(result, dict) or not Path(path).is_file():
        raise RuntimeError(f'screenshot failed: {result}')
    print(f'  screenshot: {path}')


def check_audio_synth(port):
    proc = boot_preview(port, 'audio synth', 'audio', 'audio preview')
    results = []
    try:
        state = poll_state(port, 'ready')
        panel = state['audio']
        status = _await(port, lambda s: s.get('native', {}).get('callbacks', 0) > 0)
        results.append(check('audio preview uses a null callback and exposes both shipped synth cues',
            state['mode'] == 'audio' and panel['open'] and status['native']['sink'] == 'null'
            and {'menu_back', 'menu_selected'} <= {r['label'] for r in panel['rows']}))
        for label in ('menu_back', 'menu_selected'):
            row = next(r for r in dump(port)['audio']['rows'] if r['label'] == label)
            before = _status(port)['native']['accepted']
            click_element(port, row['bounds'])
            after = _await(port, lambda s: s.get('native', {}).get('accepted', 0) > before)
            results.append(check(f'clicking {label} reaches the engine mixer',
                dump(port)['audio']['selected'] == row['id'] and after['native']['peakVoices'] == 1))
        _capture(port, 'synth')
        _click(port, 'stop')
        _await(port, lambda s: s['native']['activeVoices'] == 0)
        revision = _status(port)['previewRevision']
        _click(port, 'reload')
        _await(port, lambda s: s['previewRevision'] > revision)
        results.append(check('Reload completes without autoplay', _status(port)['native']['accepted'] == 0))
        results.append(check('audio preview loads no textures or gameplay UI',
            not send_json(port, 'return engine.getLoadedTexturePaths()')
            and send_json(port, 'return ui == nil') is True))
    finally:
        quit_engine(port, proc)
    return all(results)


def check_audio_file(port):
    results = []
    with tempfile.TemporaryDirectory(prefix='synarchy-preview-audio-') as temporary:
        file = Path(temporary) / 'audition 雪.wav'
        shutil.copyfile('test-headless/data/audio/tone.wav', file)
        proc = boot_preview(port, 'external audio file', str(file), 'audio file preview')
        try:
            state = poll_state(port, 'ready')
            started = _await(port, lambda s: s.get('native', {}).get('accepted', 0) > 0)
            results.append(check('explicit file with spaces and Unicode autoplays through the decoder',
                state['audio']['category'] == 'files' and started['native']['decodedFrames'] >= 12000))
            _capture(port, 'file')
            # The readback may be a macOS /var -> /private/var canonical path.
            selected = dump(port)['audio']['selected']
            epoch = started['epoch']
            send(port, 'engine.setResolution(800, 600)')
            time.sleep(0.4)
            state = dump(port)
            results.append(check('resize preserves the selected file without replay',
                state['audio']['selected'] == selected and _status(port)['epoch'] == epoch))
            shutil.copyfile('test-headless/data/audio/stereo.wav', file)
            _click(port, 'reload')
            reloaded = _await(port, lambda s: s['previewRevision'] > started['previewRevision'])
            results.append(check('Reload replaces edited PCM without accumulating resident samples',
                reloaded['native']['samples'] == started['native']['samples']
                and reloaded['native']['decodedFrames'] == started['native']['decodedFrames'] - 12000 + 480))
            _click(port, 'play')
            _await(port, lambda s: s['native']['accepted'] > 0)
            _click(port, 'stop')
            _await(port, lambda s: s['native']['activeVoices'] == 0)
        finally:
            quit_engine(port, proc)
    return all(results)


def check_audio_footer(port):
    proc = boot_preview(port, 'visual audio footer', 'icons', 'visual preview audio')
    results = []
    try:
        state = poll_state(port, 'ready')
        previous = state.get('selected')
        panel = state['audio']
        results.append(check('visual preview starts with a bottom-left Audio control',
            not panel['open'] and panel['footer']['bounds']['x'] == 12
            and panel['footer']['bounds']['width'] > 0))
        _capture(port, 'footer')
        click_element(port, panel['footer']['bounds'])
        opened = poll_until(5, lambda: dump(port).get('audio', {}).get('open'))
        results.append(check('real footer click opens Audio', bool(opened)))
        click_element(port, dump(port)['audio']['footer']['bounds'])
        closed = poll_until(5, lambda: not dump(port).get('audio', {}).get('open'))
        results.append(check('second footer click restores visual preview selection',
            bool(closed) and dump(port).get('selected') == previous))
    finally:
        quit_engine(port, proc)
    return all(results)


def check_audio_external_reload(port):
    """#2611: a reload another preview caller starts must refresh the pane.

    The pane's own Reload control is deliberately never clicked here.
    `audio.previewReload()` arrives through the debug console instead, which
    is the exact path that used to leave stale rows behind — preview IDs are
    reassigned positionally on every load, so an unreconciled row dispatches a
    reused ID to a different sound.
    """
    results = []
    root = Path('assets/audio')
    owned = not root.exists()
    kept, dropped = root / 'kept.wav', root / 'dropped.wav'
    # Never overwrite authored audio: this scenario deletes what it writes.
    for path in (kept, dropped):
        if path.exists():
            raise RuntimeError(f'{path} already exists; refusing to overwrite it')
    root.mkdir(parents=True, exist_ok=True)
    try:
        shutil.copyfile('test-headless/data/audio/tone.wav', kept)
        shutil.copyfile('test-headless/data/audio/stereo.wav', dropped)
        proc = boot_preview(port, 'external audio reload', 'audio',
                            'audio external reload')
        try:
            poll_state(port, 'ready')
            _click(port, 'files')
            panel = dump(port)['audio']
            labels = [row['label'] for row in panel['rows']]
            results.append(check('both discovered files are listed before the external reload',
                {'kept.wav', 'dropped.wav'} <= set(labels), f'rows={labels}'))
            # Select the entry that will NOT survive, so the fallback is exercised.
            row = next(r for r in panel['rows'] if r['label'] == 'dropped.wav')
            click_element(port, row['bounds'])
            time.sleep(0.12)
            before = _status(port)
            dropped.unlink()
            results.append(check('an external caller accepts the reload request',
                send_json(port, 'return audio.previewReload()') is True))
            after = _await(port, lambda s: s['previewRevision'] > before['previewRevision'])
            settled = after['previewRevision']
            # A stale pane never reaches the settled revision. Report what it
            # is still displaying instead of timing out into a traceback.
            panel = poll_until(10, lambda: (lambda p: p if p.get('revision') == settled
                else None)(dump(port).get('audio') or {})) or dump(port).get('audio') or {}
            rows = panel.get('rows') or []
            labels = [row['label'] for row in rows]
            results.append(check('the pane reconciles without a second Reload click',
                labels == ['kept.wav'],
                f'rows={labels} revision={panel.get("revision")} settled={settled}'))
            entries = {entry['id']: entry['label'] for entry in after['previewEntries']}
            results.append(check('every displayed row names the sound its current ID plays',
                bool(rows) and all(entries.get(row['id']) == row['label'] for row in rows),
                f'rows={[(row["id"], row["label"]) for row in rows]} catalog={entries}'))
            results.append(check('selection falls back to a surviving entry in the category',
                entries.get(panel.get('selected')) == 'kept.wav',
                f'selected={panel.get("selected")}'))
            accepted = _status(port)['native']['accepted']
            if rows:
                click_element(port, rows[0]['bounds'])
            played = rows and poll_until(10, lambda: (lambda s: s
                if s['native']['accepted'] > accepted else None)(_status(port)))
            results.append(check('a reconciled row click reaches the engine mixer',
                bool(played) and played['native']['peakVoices'] == 1,
                f'accepted={_status(port)["native"]["accepted"]} before={accepted}'))
        finally:
            quit_engine(port, proc)
    finally:
        for path in (kept, dropped):
            if path.is_file():
                path.unlink()
        if owned and root.is_dir():
            root.rmdir()
    return all(results)
