#!/usr/bin/env python3
"""Exercise the real Setup hook against isolated header/object timelines."""
from pathlib import Path
import os
import shutil
import subprocess
import tempfile

ROOT = Path(__file__).resolve().parents[1]


def main() -> None:
    with tempfile.TemporaryDirectory(prefix='synarchy-audio-build-deps-') as scratch:
        work = Path(scratch)
        source = work / 'Main.hs'
        source.write_text('import BuildSupport.AudioDependencies\nimport System.Environment\n'
                          'main = getArgs >>= mapM_ invalidateAudioDependencies\n')
        binary = work / 'check'
        subprocess.run(['ghc', '-v0', '-i' + str(ROOT), '-outputdir', str(work / 'compiled'),
                        str(source), '-o', str(binary)], check=True)
        for changed in ('none', 'private', 'public', 'vendor'):
            tree = work / changed
            def write(name: str, newer: bool = False) -> Path:
                path = tree / name
                path.parent.mkdir(parents=True, exist_ok=True)
                path.write_text('fixture\n')
                os.utime(path, (300 if newer else 100, 300 if newer else 100))
                return path
            write('cbits/audio/syn_audio.h', changed == 'public')
            write('cbits/audio/syn_audio_internal.h', changed == 'private')
            write('cbits/vendor/miniaudio/miniaudio.h', changed == 'vendor')
            write('cbits/audio/worker.c')
            write('src/Engine/Audio/Native.hsc')
            write('src/Engine/Audio/Native/CatalogPOD.hsc')
            objects = [write('output/cbits/audio/worker.' + suffix)
                       for suffix in ('o', 'dyn_o', 'p_o', 'p_dyn_o')]
            vendor = write('output/cbits/vendor/miniaudio/miniaudio.o')
            bindings = [write('output/Engine/Audio/' + name)
                        for name in ('Native.hs', 'Native/CatalogPOD.hs')]
            unrelated = write('output/cbits/lua_debug.o')
            for path in objects + [vendor, unrelated] + bindings:
                os.utime(path, (200, 200))
            subprocess.run([str(binary), 'output'], cwd=tree, check=True)
            assert all(p.exists() == (changed == 'none') for p in objects), changed
            assert vendor.exists() == (changed != 'vendor'), changed
            assert all(p.exists() == (changed != 'public') for p in bindings), changed
            assert unrelated.exists(), changed
            # Missing outputs and missing source families remain safe on repeat.
            shutil.rmtree(tree / 'src')
            subprocess.run([str(binary), 'output'], cwd=tree, check=True)
    print('PASS: actual Setup hook invalidates audio header dependents in every build way, preserving unrelated outputs')


if __name__ == '__main__':
    main()
