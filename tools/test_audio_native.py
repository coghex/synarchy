#!/usr/bin/env python3
"""Compile and exercise Synarchy's native audio boundary with forced-null output.

The optional build directory retains binaries/logs for investigation. Vendor
objects are reused only when their content, compiler, and flags match. Every
Synarchy source is rebuilt and the actual behavior test is always executed.
"""
from __future__ import annotations

import argparse
import hashlib
import json
import os
from pathlib import Path
import platform
import shutil
import subprocess
import tempfile

ROOT = Path(__file__).resolve().parents[1]
DEFINES = ['-DMA_NO_ENGINE', '-DMA_NO_RESOURCE_MANAGER', '-DMA_NO_NODE_GRAPH']


def run(build: Path, sanitize: bool) -> None:
    build.mkdir(parents=True, exist_ok=True)
    compiler = shutil.which(os.environ.get('CC', 'cc'))
    if compiler is None:
        raise SystemExit('C compiler not found')
    vendor = ROOT / 'cbits/vendor/miniaudio'
    provenance = json.loads((vendor / 'provenance.json').read_text())
    for name, info in provenance['files'].items():
        if hashlib.sha256((vendor / name).read_bytes()).hexdigest() != info['sha256']:
            raise SystemExit(f'Vendored source differs from its pinned checksum: {name}')
    common = ['-std=c11', '-O1', '-g', *DEFINES, '-I' + str(vendor),
              '-I' + str(ROOT / 'cbits/audio')]
    if sanitize:
        common += ['-fsanitize=address,undefined', '-fno-omit-frame-pointer']
    version = subprocess.check_output([compiler, '--version'])
    signature = hashlib.sha256(version + json.dumps(common).encode()
        + (vendor / 'miniaudio.c').read_bytes()
        + (vendor / 'miniaudio.h').read_bytes()).hexdigest()
    vendor_object = build / ('miniaudio-' + signature[:16] + '.o')
    log = build / 'commands.jsonl'
    with log.open('w') as record:
        def execute(args: list[str]) -> None:
            record.write(json.dumps(args) + '\n')
            record.flush()
            subprocess.run(args, check=True, cwd=ROOT)

        if not vendor_object.exists():
            pending = vendor_object.with_suffix('.pending.o')
            execute([compiler, *common, '-c', str(vendor / 'miniaudio.c'), '-o', str(pending)])
            pending.replace(vendor_object)
        sources = sorted((ROOT / 'cbits/audio').glob('*.c'))
        sources.extend(sorted((ROOT / 'test-headless/cbits').glob('audio_*_test.c')))
        objects = [str(vendor_object)]
        for source in sources:
            obj = build / (source.stem + '.o')
            execute([compiler, *common, '-DSYN_AUDIO_TEST', '-Wall', '-Wextra', '-Werror', '-c',
                     str(source), '-o', str(obj)])
            objects.append(str(obj))
        libraries = ['-lpthread', '-lm']
        if platform.system() == 'Darwin':
            for framework in ('CoreFoundation', 'CoreAudio', 'AudioToolbox'):
                libraries += ['-framework', framework]
        else:
            libraries += ['-ldl']
        binary = build / 'audio-native-test'
        execute([compiler, *common, *objects, *libraries, '-o', str(binary)])
        execute([str(binary)])


def main() -> None:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('--build-dir', type=Path)
    parser.add_argument('--sanitize', action='store_true')
    args = parser.parse_args()
    if args.build_dir:
        run(args.build_dir.resolve(), args.sanitize)
    else:
        with tempfile.TemporaryDirectory(prefix='synarchy-audio-native-') as tmp:
            run(Path(tmp), args.sanitize)


if __name__ == '__main__':
    main()
