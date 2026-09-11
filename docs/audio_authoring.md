# Authoring sounds

Audio definitions live in three startup-loaded catalogs under `data/audio/`.
The game resolves types and instruments before uploading sounds, then keeps the
catalog fixed for that game process. Restart the game after changing definitions
or samples; the audio preview player can reload them in place.
The [runtime guide](audio_runtime.md) covers devices, configuration and diagnostics;
the [design](audio_system_design.md) records the decisions behind this contract.


## Audition in preview

Run these from the task worktree (quote file paths containing spaces):

```sh
cabal run exe:synarchy -- --preview audio
cabal run exe:synarchy -- --preview audio/files
cabal run exe:synarchy -- --preview "/path/to/bear.wav"
```

Every visual preview also has an **Audio** button at the bottom left. **Synth**
contains the authored synth sounds, initially `menu_selected` and `menu_back`.
**Files** lists WAV, FLAC and MP3 files under `assets/audio`, authored sample
paths, and the explicitly selected CLI file. Relative CLI paths belong to the
calling directory, even with `--resource-root`. The CLI file may be outside the
project; it is read without copying or changing it. Game sample definitions
still require paths contained within `assets/audio`.

Click a row to hear it, or use **Play** / Space to repeat the selection.
Up/Down selects and plays adjacent rows. **Stop** clears the current audition.
An explicit CLI file plays once on opening; categories wait for a selection.
Only one audition plays at a time. **Reload** stops playback, rereads the catalogs
and samples, and preserves the selection when its source still exists. It frees
the old decoded samples before allocating replacements. Edit a synth definition
or overwrite your WAV, then click Reload and Play to compare the result.

Auditions retain the authored synth timbre, seed, gain and gate. They use the UI
bus without spatial attenuation, player-pause freezing, looping or cooldown.
Master and UI controls adjust the session only; use Settings to save volumes.
Click Audio again or press Escape to return to a visual preview; Escape closes
an audio-only preview. Invalid samples remain listed as unavailable while valid
sounds stay playable. The usual decoding budgets apply; discovery is capped at
256 local files and 256 synth sounds, plus one explicit CLI file, and skips
hidden entries and child symlinks. No bear sounds are shipped by this player.

Hidden preview probes force null output. Validate the player with
`--match "Audio.Preview"` and `python3 tools/preview_probe.py --only audio`;
listening acceptance still requires the owner's audible preview.


## Files and inheritance

| File | Root list | Entry fields |
| --- | --- | --- |
| `sound_types.yaml` | `sound_types` | `id`, optional `policy` |
| `instruments.yaml` | `instruments` | `id`, optional `timbre` |
| `sounds.yaml` | `sounds` | `id`, `type`, `source`, optional `policy` |

Each file also requires `schema_version: 1`. IDs match
`[a-z][a-z0-9_]{0,63}`. IDs in different families are independent. Every duplicate
definition of an ID is disabled, together with sounds depending on it; unrelated
valid entries survive. Unknown fields, explicit `null`, wrong types and non-finite
numbers are errors. Catalog diagnostics identify the file, entry and dependency.

A type's policy extends compiled defaults. A sound's policy recursively overrides
its type: changing `concurrency.max_instances` preserves the inherited cooldown.
An instrument's timbre extends compiled defaults; a synth sound's `timbre` patch
overrides that instrument. Explicit `source.synth.gate_ms` takes final precedence
over the resulting `default_gate_ms`. Omit a field to inherit it.

## A complete synth example

These three documents can be used together. The resulting `ui_ping` is a short
sine tone on the Interface bus. The shipped menu cues use the same schema with
their approved noise/filter recipes in `data/audio/instruments.yaml`.

`sound_types.yaml`:

```yaml
schema_version: 1
sound_types:
  - id: ui_effect
    policy:
      bus: ui
      spatial: {mode: non_spatial}
      player_pause: continue
      concurrency: {max_instances: 4, overflow: steal_oldest, cooldown_ms: 100}
```

`instruments.yaml`:

```yaml
schema_version: 1
instruments:
  - id: soft_ping
    timbre:
      generator: {waveform: sine, frequency_hz: 660}
      envelope: {attack_ms: 2, decay_ms: 20, sustain_level: 0.3, release_ms: 35}
      gain_db: -12
      default_gate_ms: 45
```

`sounds.yaml`:

```yaml
schema_version: 1
sounds:
  - id: ui_ping
    type: ui_effect
    source: {synth: {instrument: soft_ping}}
```

Play it with `audio.play("ui_ping")`. Its boolean result reports enqueue success;
the worker can still reject an accepted request for cooldown or voice pressure.
Inspect `audio.getStatus()` for the actual playback counters and drop reasons.

## Samples

Each sound has exactly one source branch: `sample` or `synth`. A sample branch
contains only `path`, for example
`source: {sample: {path: assets/audio/units/bear/selection_01.wav}}` once that
recording exists and has owner approval. This illustrative path is not shipped
content. Supply the real recording and its provenance before adding a definition.

Paths must be normalized, relative POSIX paths under `assets/audio/`, with no
`..`, backslashes or NUL. The resolved symlink target must remain under that root.
WAV, FLAC and MP3 are supported; extension matching is case-insensitive. MP3 is
one-shot only. Looping WAV/FLAC repeats the entire decoded file; there are no
custom loop points, streaming sources or crossfades.

Decode happens at startup into resident stereo float PCM at the configured mix
rate. Multiple paths resolving to the same file share its PCM. Paths are loaded
in sorted order, so aggregate-budget admission is reproducible. Defaults allow
16 MiB encoded per asset, 15 seconds decoded per asset, 120 seconds in aggregate,
and 64 MiB decoded PCM in aggregate. Reaching any limit disables the affected
sounds and leaves valid siblings available. Decode reads only up to the budget
plus a detection frame; it does not trust a file's claimed duration.

Record production files, original source/license, modifications and the owner's
accepted filenames in `assets/audio/ATTRIBUTION.md` when the first library assets
are added. Mathematical decoder fixtures have separate
[provenance](../test-headless/data/audio/README.md).

## Policy reference

All numeric ranges below include their endpoints unless stated otherwise.

| Field within `policy` | Default | Accepted values |
| --- | --- | --- |
| `bus` | `world` | `world`, `ui` |
| `gain_db` | 0 | -96…24 |
| `priority` | 50 | integer 0…100; higher wins |
| `player_pause` | `freeze` | `freeze`, `continue` |
| `spatial.mode` | `world` | `world`, `non_spatial` |
| `spatial.min_distance_tiles` | 1 | 0…1024 |
| `spatial.max_distance_tiles` | 40 | 0…4096, strictly greater than minimum |
| `spatial.rolloff` | `linear` | `linear` only |
| `spatial.vertical_scale` | 1 | 0…8 |
| `concurrency.max_instances` | 8 | integer 1…runtime `max_voices` |
| `concurrency.overflow` | `steal_oldest` | `drop_new`, `steal_oldest` |
| `concurrency.cooldown_ms` | 0 | 0…60000 |
| `loop.allowed` | false | boolean |
| `loop.stop_fade_ms` | 20 | 0…5000 |

UI sounds require both `non_spatial` and `continue`. World sounds may be spatial
or non-spatial; both follow the World volume and zoom mute. Spatial sounds need
a valid listener and a position on its page. The camera determines pan, distance
and zoom range. Distance uses tiles, with Z multiplied by `vertical_scale`.

Cooldown is per sound ID, shared by all callers. A successful start begins it;
a rejected start does not. At the per-sound limit, `drop_new` refuses the request
and `steal_oldest` replaces that sound's oldest voice. At global capacity the
lowest-priority voice is considered first. A lower-priority incoming sound drops;
equal or higher priority can steal. Ties use oldest start frame, then lowest slot.
Voices fading after a stop still occupy capacity until they retire.

## Instrument reference

| Field within `timbre` | Default | Accepted values |
| --- | --- | --- |
| `generator.waveform` | `sine` | `sine`, `saw`, `square`, `triangle`, `white_noise` |
| `generator.frequency_hz` | 440 | 20…min(20000, 0.45 × mix rate) |
| `generator.start_phase` | `reset` | `reset`, `random` |
| `generator.noise_seed` | 1831565813 | integer 0…4294967295 |
| `envelope.attack_ms` | 2 | 0…30000 |
| `envelope.decay_ms` | 20 | 0…30000 |
| `envelope.sustain_level` | 0.5 | 0…1 |
| `envelope.release_ms` | 35 | 0…30000 |
| `filter.mode` | `bypass` | `bypass`, `low_pass`, `high_pass`, `band_pass` |
| `filter.cutoff_hz` | 1000 | 20…0.45 × mix rate |
| `filter.resonance_q` | 0.707 | 0.1…20 |
| `gain_db` | 0 | -96…24 |
| `default_gate_ms` | 45 | 0…60000 |

The gate starts release from the envelope level reached at that moment, even
during attack or decay. A synth loop sustains until stopped. Noise and random
phase use audio-owned deterministic state; playback consumes no gameplay random
draws. This synth supports one oscillator, envelope and static filter per voice.

## Runtime calls

```lua
audio.play("ui_ping", {gainDb = -3, pitchSemitones = 2})
audio.startLoop("workshop_hum", "authored_loop", {
    pageId = "colony", position = {x = 10, y = 20, z = 0}
})
audio.updateLoop("workshop_hum", {gainDb = -6})
audio.stopLoop("workshop_hum")
```

`authored_loop` must be a real sound whose policy permits looping. Logical loop
IDs are nonempty strings of at most 128 characters, without NUL. Starting the
same loop ID with the same sound updates position/gain without restarting it;
another sound conflicts. Stop releases the logical ID immediately while the old
voice fades. Reusing that ID creates a new lifetime.

Play/start accept optional `gainDb` (-48…12), `pitchSemitones` (-24…24), and paired
`pageId` plus `position = {x, y, z}` with finite coordinates. Update accepts gain
and the paired position fields, but no pitch change. Callers cannot supply paths,
native handles, bus, priority, filters, envelope or concurrency policy.

Explicit player pause freezes selected World source states exactly and drops
new freeze-policy triggers. UI/continue sounds run. Internal save/notification
pauses do not freeze audio. Session replacement clears voices, queued session
work, loop IDs, cooldown and listener state; player volumes remain process scoped.

## Verify a change

Use the relevant `Audio.Catalog`, `Audio.Upload`, `Audio.Native` or `Audio.Runtime`
Hspec group, then audition the exact sound through the game. Headless tests prove
decoding and behavior; they cannot establish whether a recording sounds right.
The current shipped catalog contains the two menu cues. Unit response-set
authoring is being developed in the [bear pilot](unit_selection_audio_design.md).
