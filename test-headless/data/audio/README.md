# Audio decoder fixtures

These signals were generated for Synarchy under the project's MIT license.
They contain no recorded performance or third-party sound assets.

| File | Signal and purpose |
| --- | --- |
| `tone.wav` | 11025 mono signed-16 samples at 44100 Hz: 440 Hz sine, amplitude 0.2. Exact duration 0.25 seconds; mono/rate conversion and lossless reference. |
| `tone.flac` | Lossless encoding of `tone.wav`; FLAC decoding and PCM agreement. |
| `tone.mp3` | 128 kbit/s libmp3lame encoding of `tone.wav`; lossy decoding and MP3 loop refusal. |
| `stereo.wav` | 480 stereo signed-16 frames at 48000 Hz, opposite linear ramps; channel preservation without resampling. |
| `corrupt.wav` | Deliberately truncated RIFF bytes; failure isolation. |

Regenerate in a new directory, leaving tracked inputs available for comparison:

```bash
python3 tools/generate_audio_fixtures.py /tmp/synarchy-audio-fixtures
```

Python 3 and FFmpeg with FLAC/libmp3lame encoders are required only to regenerate
fixtures. The checked-in `manifest.json` records encoder version, exact commands,
file sizes and SHA-256 hashes. Initial generation used FFmpeg 8.0. The generator
records the version actually used on each run; do not hand-edit codec output.

WAV and FLAC tests assert tight waveform/channel/rate expectations. MP3 is lossy,
has encoder delay/padding, and may change bytes across encoder builds. Its tests
check tolerant duration, frequency and energy rather than sample or byte identity.
`python3 tools/test_audio_native.py --sanitize` exercises all three decoders using
forced-null devices and verifies the pinned miniaudio source hashes.

These files are test inputs only. They must not be used as placeholder game audio.
