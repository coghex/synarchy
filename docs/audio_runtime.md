# Audio runtime and diagnostics

The audio worker owns the native core, catalog, decoded samples and logical loop
bindings. Gameplay and UI submit semantic requests through `AudioCapability`.
Its two fields alias the abstract transport and a copied status reference;
native handles never enter `EngineEnv`, Lua or saves.

The C device callback only reads interleaved stereo PCM from the SPSC ring,
zero-fills shortages and updates lock-free counters. Mixing, decoding, allocation,
logging, device recovery and Haskell calls never run on that callback. The worker
invokes bounded native command/refill operations and waits between useful work.
It starts before producers and joins after them, before native storage is freed.

See [authoring](audio_authoring.md) for content and Lua calls, the
[design](audio_system_design.md) for rationale, and the capability/persistence
inventories for the classified ownership boundary.

## Output modes

| Game mode | Audio output |
| --- | --- |
| Graphical | Default physical backend, with null fallback and real-device retries |
| `--headless` | Context restricted to `ma_backend_null`; no physical backend probes or retries |
| `--offscreen` | The same forced-null policy |
| `--preview` | Real output with null fallback; `SYNARCHY_PREVIEW_HIDDEN` forces null |
| `--dump`, `--language-report` | No audio worker/native core |

All initialized engine environments have a safe transport/status projection.
Modes without a worker report `disabled`; invalid startup also leaves audio
disabled without stopping the game. Settings remain readable and writable.

A real-device initialization **or start** failure falls back to null. Native
notifications only set an atomic flag. The next worker service joins and
reinitializes the device outside the callback, clearing the ring so old queued
PCM cannot burst on recovery. Null output continues voice clocks silently.
Retries start at one second and double to a 30-second cap by default. Failure
of both outputs reports `disabled` and retains retry intent while the core is
alive. Failure to construct the core at all disables audio for that process. Preview
can retry construction with Reload; every attempt joins the previous native
callback before rebuilding its disposable authoring catalog.

Stopping is idempotent. An intentional teardown's stopped notification is cleared
after callback join, so it cannot bypass the retry delay. A session reset is safe
even while the previous device is unavailable.

## Runtime tuning

`config/audio_runtime.yaml` is a complete, strict `schema_version: 1` document
under `audio_runtime`. Unknown fields, missing fields, non-finite numbers, or an
invalid relationship reject the **whole** file and use compiled defaults, with
a boot warning. Player Settings never writes this file.

| Group/key | Default | Constraint |
| --- | --- | --- |
| `mix.sample_rate_hz` | 48000 | integer 22050…96000 |
| `mix.render_chunk_frames` | 256 | power of two, 64…1024 |
| `mix.target_fill_frames` | 1024 | 128…1048576; chunk multiple, at least two chunks |
| `mix.ring_capacity_frames` | 4096 | 256…2097152; chunk multiple, target plus at least two chunks |
| `mix.max_voices` | 128 | integer 16…1024 |
| `mix.command_batch_limit` | 256 | integer 16…1024 |
| `mix.worker_idle_wait_us` | 1000 | integer 250…5000 |
| `transport.event_capacity` | 2048 | integer 128…65536 |
| `transport.control_backlog_warn` | 64 | integer 1…65536 |
| `device.period_frames` | 256 | 0 (backend choice), or integer 64…1024 |
| `device.periods` | 2 | 0 (backend choice), or integer 2…4 |
| `device.retry_initial_ms` / `retry_max_ms` | 1000 / 30000 | integers 1…300000; initial ≤ maximum |
| `smoothing.bus_gain_ms` / `instance_gain_ms` | 20 / 10 | 0.001…10000 |
| `smoothing.pause_out_ms` / `pause_in_ms` | 5 / 10 | 0.001…10000 |
| `spatial.close_range_scale` / `far_range_scale` | 0.85 / 1.5 | 0.001…100; close ≤ far |
| `spatial.close_gain_db` / `far_gain_db` | 1.5 / -2 | -96…24 |
| `limiter.knee` | 0.95 | ≥0.5 and <1 |
| `assets.max_encoded_mib` | 16 | integer 1…1024 |
| `assets.max_asset_seconds` | 15 | 0.001…3600 |
| `assets.max_total_seconds` | 120 | 0.001…86400 |
| `assets.max_decoded_mib` | 64 | integer 1…4096 |
| `player_curve.exponent` | 2 | 1…4 |
| `telemetry.publish_hz` | 10 | 0.1…100 |
| `telemetry.underruns_per_minute_warn` | 3 | integer 1…1000000 |
| `telemetry.service_budget_fraction_warn` | 0.5 | 0.001…100 |
| `telemetry.rate_limit_seconds` | 10 | 0.001…3600 |

At the defaults, a render chunk represents 5.33 ms and target ring fill represents
21.33 ms. Ring capacity is an upper bound of 85.33 ms, not the desired latency.
The negotiated backend period and device buffering add their own latency.
Inspect runtime facts instead of treating requested period values as guarantees.
Normal refill produces at most the missing target frames per call; device recovery
may also prefill a restarted device before that pass.

## Player volumes

`config/audio_default.yaml` supplies defaults. `config/audio.local.yaml` is a
gitignored sparse overlay with only changed `master`, `world` and `ui` values.
Each value is an integer 0…100. Invalid local keys fall back independently;
a bad World setting does not discard a valid Master value. Saving all defaults
removes the local file through the durable config-write helper.

Amplitude is `(value / 100)^exponent`: 0 is exact mute, 100 is unity, and the
default midpoint is amplitude 0.25. Master multiplies World and Interface.
Saved values are applied before starting the device. Changes ramp smoothly;
muting a bus does not freeze its source clocks.

Settings keeps current, pending and saved audio values independently. Sliders
preview immediately; Apply keeps that live choice without disk writes. Save
updates the saved baseline only after a successful durable write. Back restores
saved values. Defaults previews tracked defaults until Save. Resize preserves
unapplied slider values.

## Status and warnings

`audio.getStatus()` returns copied data. Lifecycle is `starting`, `running_real`,
`running_null`, `degraded_null`, `disabled` or `stopped`. `health.degraded` is the
independent pressure indicator; pressure does not shut down a healthy device.

| Status family | Meaning |
| --- | --- |
| `snapshotSequence`, `publishedNs` | Monotonic publication sequence and process monotonic timestamp in nanoseconds; no wall-clock meaning |
| `volumes`, `epoch`, `sessionResets` | Current player values and transient audio-session boundary |
| `catalog` | Valid/disabled authored type, instrument and sound entry counts; asset counts use distinct authored paths, while native sample count uses shared decoded files |
| `catalogSounds`, `catalogWarnings` | Final playable sounds and boot diagnostic count; a wholly unreadable file has a warning but no knowable entry count |
| `transport` | Lifetime enqueue/refusal/coalescing/stale counts and current/peak event/control queue depths |
| `drops` | Worker refusals by reason, including unknown ID, missing listener, wrong page, missing/conflicting loop, cooldown, per-sound capacity, global priority, pause and stale session |
| `perSoundSteals`, `globalSteals` | Successful replacements attributed by allocation policy |
| `health` | Recent underruns, service frames, lifetime budget violations and current pressure flags |
| `diagnosticWarnings`, `lastError` | Emitted runtime warnings and one bounded last diagnostic, not an accumulating history |
| `native` | Sink, backend, device name, negotiated sample rate/period, ring fill/min/max, active/peak voices and loops, accepted/dropped/stolen counts, rendered/callback frames, callback/underrun counts, decoded frames/bytes, limiter/non-finite counters, mix peak, transitions and latest service duration |

Ordinary status publication is limited to `publish_hz`, including the disabled
worker. Startup, device transitions and shutdown publish immediately. Volumes in
the Lua query are read live so dragging a slider need not wait for publication.
Queue depths exclude the bounded batch already removed for worker processing.
The worker forces each published snapshot and its sequence before completing the
write. Transport and health counters are strict too: leaving Settings and the
preview pane closed does not retain an accumulating history of old telemetry.

Health warns for three underruns in the trailing minute, control depth above 64,
or service time over half the audio represented by the frames that call rendered.
The underrun window uses 61 one-second buckets, conservatively expiring an
observation at most one second late. A zero-frame call creates no budget violation;
the next rendered batch can clear that condition. Recovery clears current flags
while lifetime counters remain.

Catalog causes warn at boot. Runtime warnings use `(reason, sound/loop ID)` keys:
first occurrence immediately, then at most once per configured interval with a
suppressed count. The cache retains at most 256 recently used keys. Error text is
bounded to 512 characters. `ENGINE_DEBUG=Audio` enables the normal Audio category;
callback and native notifications never log directly.

## Focused verification

```bash
cabal build all
cabal test synarchy-test-headless --test-options='--match Audio.'
python3 tools/test_audio_native.py --sanitize
python3 tools/test_audio_build_dependencies.py
python3 tools/audio_null_probe.py --port 9187
python3 tools/audio_manual_probe.py --offscreen-check --port 9188
```

The null probe boots the actual executable with a private resource root and
generated sample/synth definitions. It asserts the pinned null backend, actual
callback/render progress, mixed loops, spatial listener publication, pause/resume,
volume persistence, worker diagnostics, session reset and clean process exit.
It neither writes the developer's settings nor opens speakers or a window.

The manual probe's `--offscreen-check` mode additionally drives the rendered
Settings → Audio tab through real input and retains a screenshot. It exercises
menu synths, a mathematical sample loop, left/centre/right World loop updates,
pause/resume and clean shutdown through forced-null output. It requires a GPU
and is manual-only. Its four automated checks do not establish listening quality.

For owner listening, run `python3 tools/audio_manual_probe.py --interactive
--port 9188` in a terminal. This explicitly opens a game window and physical
audio output in an isolated resource root. Start with comfortable speaker/headphone
volume; the probe uses Master 50%, World/Interface 70% locally. It prints actual
backend, device name, rate, period and ring fill in milliseconds, then asks for
the listening verdict and an optional manual output-device change. Recovery
snapshots and the subsequent heard-cue verdict remain separate evidence. A null
fallback or skipped device change cannot establish physical-device acceptance.
The engine process and private resources are cleaned up on completion/interruption.
The probe does not change the operating system's output selection itself.

Native tests inject initialization/start/allocation failures only in their test
build. The production API has no failure-injection control. Address/undefined
sanitizers cover the ring, decoders, DSP, voice policy and recovery paths.

The private native header is shared by several C objects. Cabal's ordinary C
freshness check does not follow header dependencies; `BuildSupport.AudioDependencies`
invalidates affected objects and generated Haskell bindings when those headers
change. Preserve the hook and its test when changing the native build layout.

Physical-device listening remains an owner activity. Headless success establishes
behavior and numerical output; it cannot establish device quality or taste.
The normal repository launch restrictions still apply to graphical game launches.

## Preview authoring boundary

`App.Preview` starts audio before input/Lua and joins it after those producers.
It still starts no world, unit, sim or combat worker. `PreviewAudioConfig` is
worker-local immutable input naming at most one explicit local audio file.
The normal catalog path resolver stays contained within `assets/audio`;
`Engine.Audio.Preview.Catalog` supplies the exception for that exact selected
file only. No Lua API accepts file paths.

`audio.previewPlay(id)`, `audio.previewStop()` and `audio.previewReload()` return
false outside `BootPreview`. Play accepts only playable IDs from the copied
`audio.getStatus().previewEntries`. Replacement resets voices and stale commands
atomically before enqueueing the audition. A rejected play leaves the epoch
unchanged. Reload is retained across later session resets, destroys the old core
before allocating the new one, and preserves live volumes. `previewRevision`
advances when a preview load attempt completes (including initialization failure).
The pane remembers the revision whose entries it consumed and reconciles its whole
entry model, selection and visible rows against any later revision, whichever
preview caller requested the reload. Preview IDs are reassigned positionally on
every load, so an unreconciled model can name a different sound: the pane refuses
playback from one, reconciling instead of dispatching when a click or key arrives
after the revision advanced. Normal status publications cannot masquerade as
completed reloads. These metadata, UI state, and control requests are transient
and excluded from saves.
