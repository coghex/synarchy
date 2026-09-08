# Approved first menu sounds

Status: owner-approved listening reference; runtime integration is future work.
Parent authority: [audio system design](../../audio_system_design.md), D-7,
D-26, D-34, and AUD-5. This reference preserves two content choices without
changing the foundation's scope, readiness state, or twelve delivery slices.

## Owner decisions and acceptance

Approved on 2026-09-07 (America/Los_Angeles) by Vincent Coghlan in foreground
session `20260908T000349Z-audio-first-player-facing-sound-pale-69a90f`.
The owner's verdict after hearing the first pair was:

> yea those sound good to me, exactly as i imagined

- **MC-1 — Source:** short UI cues use the planned synth. The owner prefers
  a uniform but natural character and sounds that mix well together.
- **MC-2 — Selected:** clicking a button that changes to another menu plays
  a solid, fairly deep click. This means menu-button activation, not selecting
  a unit or item.
- **MC-3 — Rejected / Back:** the Back button plays the second cue: an even
  deeper click with a deep wood-block character. Here, “rejected” names the
  Back cue; no general error, disabled-button, or rejected-order hook has been
  agreed.
- **MC-4 — Initial boundary:** accepted-order sounds are excluded. Unit
  selection will have a separate sound later. Other controls and keyboard
  shortcuts have not been given a sound contract by this session.

The owner approved preserving these exact settings, previews, reproduction
script, and signoff, and publishing the reference through a standalone PR.
This archive documents the approved sounds; production IDs and runtime wiring
remain for the later integration task.

## Listen

- [Selection, then Back, repeated three times — WAV](selected-then-back.wav)
- [The same audition — MP3](selected-then-back.mp3)
- Individual cues: [selected](selected.wav), [Back](back.wav)

The WAV files contain 48,000 Hz, stereo, signed 16-bit PCM with identical left
and right channels. The combined audition lasts 4.912 seconds: 250 ms of leading
silence, then three pairs with 550 ms after selection and 850 ms after Back.
The MP3 is the original listening derivative and is retained as presented.
These are offline references for later synthesized playback, not production
sample assets.

## Exact approved recipe

[render.py](render.py) is the unchanged audition generator, and
[patches.json](patches.json) is its parameter export. Both cues use one seeded
xorshift32 white-noise generator, then a sample-counted linear ADSR, then one
fixed band-pass filter, then gain. They use no samples, layered oscillators,
pitch glide, or effects beyond the planned synth surface.

| Parameter | Selected | Rejected / Back |
| --- | ---: | ---: |
| Band-pass centre, Hz | 620 | 310 |
| Resonance Q | 2.8 | 4.2 |
| Attack, ms | 0.2 | 0.25 |
| Decay, ms | 5 | 7 |
| Sustain level | 0 | 0 |
| Gate, ms | 6 | 8 |
| Release, ms | 55 | 85 |
| Noise seed | 1831565813 | 1831565813 |
| Gain, dB | +12 | +15.5 |
| Rendered duration, ms | 61 | 93 |
| Measured peak, dBFS before PCM quantization | -8.108 | -10.362 |

The filter uses the constant-0-dB-peak band-pass coefficients in the
[RBJ Audio EQ Cookbook](https://www.w3.org/TR/audio-eq-cookbook/), implemented
in transposed direct form II. The generator specifies noise conversion,
frame rounding, gain, and PCM quantization exactly.

The noise excitation decays to zero before the gate expires. The remaining
rendered interval lets the resonant filter tail decay; the long release value
does not mean a sustained noise burst. Preserve that tail when comparing a
future native implementation with the reference.

This approval establishes the heard sound, not bit parity with an unimplemented
runtime. The Python audition uses fixed seeds and Python floating point;
the planned native voice-sequence seed derivation, float32 processing, voice
retirement, and common mixer gain staging can affect the result. Future
integration should compare its output with these references and retain the
approved character. Single-cue checks do not establish actual mixer headroom
under concurrency or command-to-speaker latency.

## Reproduce and verify without overwriting the references

From the repository root, with Python 3 and its standard library:

```bash
python3 - <<'PY'
import hashlib
import json
import shutil
import subprocess
import sys
import tempfile
from pathlib import Path

reference = Path('docs/audio/menu_cues').resolve()
expected = json.loads((reference / 'approved_sha256.json').read_text())
for name, digest in expected.items():
    actual = hashlib.sha256((reference / name).read_bytes()).hexdigest()
    assert actual == digest, f'Archived file changed: {name}'

with tempfile.TemporaryDirectory(prefix='synarchy-menu-cues-check-') as folder:
    output = Path(folder)
    shutil.copyfile(reference / 'render.py', output / 'render.py')
    subprocess.run([sys.executable, str(output / 'render.py')], check=True,
                   stdout=subprocess.DEVNULL)
    for name in ('patches.json', 'selected.wav', 'back.wav',
                 'selected-then-back.wav'):
        actual = hashlib.sha256((output / name).read_bytes()).hexdigest()
        assert actual == expected[name], f'Reproduction differs: {name}'

print('Approved archive and regenerated WAVs/parameters match.')
PY
```

The renderer also checks finite samples, peak headroom below 0.95, tails below
one 16-bit PCM step, deterministic repetition, and WAV channels, sample width,
sample rate, and frame count. The verification command was run successfully on
macOS for this local handoff. Cross-platform bit identity has not been measured.

For reference, the listening MP3 was encoded with:

```bash
ffmpeg -v error -i selected-then-back.wav -ac 1 -codec:a libmp3lame \
  -b:a 48k selected-then-back.mp3
```

MP3 byte reproduction is not required: the original derivative is hash-checked,
and the WAVs are the reproducible sound reference. All audio here was generated
mathematically for this session; no third-party recording was used. The archived
generator and parameters preserve its provenance.
