#!/usr/bin/env python3
"""Reproduce synthetic decoder fixtures; no recordings or external assets.

WAV contains exactly 11025 mono signed-16 PCM samples of a 440 Hz sine at
44100 Hz. FLAC and MP3 encode that same signal. Lossless outputs are stable;
MP3 bytes may vary with the recorded FFmpeg/libmp3lame version.
"""
from __future__ import annotations

import argparse
import hashlib
import json
import math
from pathlib import Path
import shutil
import struct
import subprocess
import wave


def generate(destination: Path) -> None:
    encoder = shutil.which('ffmpeg')
    if encoder is None:
        raise SystemExit('ffmpeg is required to regenerate FLAC and MP3 fixtures')
    destination.mkdir(parents=True, exist_ok=True)
    pcm = b''.join(struct.pack('<h', round(0.2 * 32767 * math.sin(2 * math.pi * 440 * i / 44100)))
                   for i in range(11025))
    with wave.open(str(destination / 'tone.wav'), 'wb') as output:
        output.setparams((1, 2, 44100, 11025, 'NONE', 'not compressed'))
        output.writeframes(pcm)
    commands = []
    for extension, options in [('flac', ['-c:a', 'flac']),
                               ('mp3', ['-c:a', 'libmp3lame', '-b:a', '128k'])]:
        args = [encoder, '-hide_banner', '-loglevel', 'error', '-y',
                '-i', str(destination / 'tone.wav'), '-map_metadata', '-1',
                '-fflags', '+bitexact', '-flags:a', '+bitexact', *options,
                str(destination / ('tone.' + extension))]
        subprocess.run(args, check=True)
        commands.append(args[1:])
    # Separate channels at the mix rate exercise stereo preservation without
    # conflating it with the mono/rate converter tested by tone.*.
    with wave.open(str(destination / 'stereo.wav'), 'wb') as output:
        output.setparams((2, 2, 48000, 480, 'NONE', 'not compressed'))
        output.writeframes(b''.join(struct.pack('<hh', i * 10, -i * 10) for i in range(480)))
    (destination / 'corrupt.wav').write_bytes(b'RIFF\x00\x00\x00\x00truncated fixture')
    manifest = {
        'generator': 'tools/generate_audio_fixtures.py',
        'source': 'Mathematical signals generated for Synarchy tests; project MIT license.',
        'encoder': subprocess.check_output([encoder, '-version'], text=True).splitlines()[0],
        'commands': commands,
        'files': {p.name: {'sha256': hashlib.sha256(p.read_bytes()).hexdigest(),
                           'bytes': p.stat().st_size}
                  for p in sorted(destination.iterdir()) if p.suffix in ('.wav', '.flac', '.mp3')},
    }
    (destination / 'manifest.json').write_text(json.dumps(manifest, indent=2) + '\n')


if __name__ == '__main__':
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('destination', type=Path)
    generate(parser.parse_args().destination)
