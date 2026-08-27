# Audio system design

This document is the sole product and implementation source of truth for
Synarchy's first audio runtime. It combines the settled product rationale with
implementable component boundaries, module placement, native/Haskell contracts,
schemas, lifecycle integration, failure behavior, verification, and
dependency-ordered delivery slices. Product and architecture decisions are
recorded as D-1 through D-26; the component-level decisions that refine them
continue as D-27 through D-49. The concrete documentation deliverables and
their slice ownership are kept in the
[documentation roadmap](#documentation-roadmap) below.

Design state: `ready for issue processing`

Status legend: `[ ]` unprocessed · `[#N]` linked to issue N · `[no-issue]`
reviewed and deliberately not tracked separately · `[deferred]` blocked on a
concrete precondition

## Processing status

- [ ] EPIC. Add Synarchy's sampled and synthesized audio foundation
- [ ] AUD-1. Establish the native audio core and device boundary
- [ ] AUD-2. Add the audio transport capability and worker lifecycle
- [ ] AUD-3. Define audio runtime, player, and content configuration
- [ ] AUD-4. Decode and mix resident sampled one-shots
- [ ] AUD-5. Render minimalist synthesized voices
- [ ] AUD-6. Expose semantic one-shot triggers and audio status
- [ ] AUD-7. Anchor world audio to the camera and zoom model
- [ ] AUD-8. Add logical loops and deterministic voice policy
- [ ] AUD-9. Integrate player pause and session replacement
- [ ] AUD-10. Add the Settings Audio tab and player volume persistence
- [ ] AUD-11. Harden device recovery and audio health diagnostics
- [ ] AUD-12. Complete deterministic integration coverage and operator docs

## Epic contract

- **Goal:** Synarchy can trigger small resident sampled sounds and minimalist
  synthesized sounds through a responsive, spatially aware runtime whose native
  real-time path is isolated from Haskell scheduling and allocation.
- **Done when:** a graphical run can load and play WAV, FLAC, and non-looping
  MP3 assets; trigger all five selected synth generators and all three filters;
  mix simultaneous sample and synth voices; spatialize world sounds around the
  camera; control logical loops; preserve selected world voices across player
  pause; silence and restore World naturally across the zoom-map transition;
  apply Master, World, and UI Settings volumes; exercise the same mixer through
  a speaker-silent null device in headless/offscreen; isolate malformed content;
  report pressure and device failures; and shut down without callback or native
  lifetime races.
- **Users and operators:** players hear world and UI feedback; content authors
  configure semantic sounds without changing gameplay code; engine maintainers
  own the C/Haskell boundary and can diagnose health without reproducing a
  physical-device failure.
- **Arc label:** None proposed

## Current state and evidence

Synarchy has no audio implementation today. `synarchy.cabal` compiles only
`cbits/font_stb.c` and `cbits/lua_debug.c`; its runtime resources include image,
font, data, script, and enumerated config files but no audio formats. There is no
`Engine.Audio` module family, audio content catalog, player audio config, or
audio worker.

The existing integration points constrain the component shape:

- `src/Engine/Core/Workers.hs` is the sole worker registry and shutdown-order
  definition. Every boot module constructs `EngineWorkers` by record syntax, so
  a new `ewAudio` field intentionally makes `-Wmissing-fields` find any forgotten
  boot topology.
- `app/App/Graphical.hs`, `App/Offscreen.hs`, `App/Headless.hs`, `App/Dump.hs`,
  and `App/Preview.hs` currently start their workers directly. Graphical and
  offscreen have a renderer/camera; headless has neither; dump and preview are
  intentionally trimmed profiles.
- `src/Engine/Core/State.hs` has no suitable existing audio field. Section
  6.4(c) of `docs/engineenv_capability_inventory.md` requires an approved ninth
  capability to update the vocabulary, audit constant and self-tests, empty
  temporary ceiling, persistence and capability inventory rows, Cabal module
  lists, a real narrowed consumer, and projection-aliasing hspec coverage in one
  lockstep change. The user has explicitly approved that addition.
- `src/Engine/Loop.hs` integrates the live camera on the main render thread,
  while `World.Grid.zoomFadeStart`/`zoomFadeEnd` are already the renderer's
  transition authority. `Engine.Loop` is the right publisher of a coalesced
  listener snapshot; the audio worker must not read render or world state
  directly.
- `src/Engine/Scripting/Lua/API/Core.hs:setPausedFn` is the player-intent pause
  boundary. Save/load and notification pauses write the engine pause state by
  other paths, which gives audio a precise place to distinguish a player pause
  from an internal pause.
- `scripts/settings_menu.lua` composes General, Graphics, Input, and
  Notifications tabs. `scripts/settings/data.lua` already demonstrates why a
  setting family with a different engine-side home needs its own
  current/pending/saved state rather than being folded into video state.
- `Engine.Save.Config` is the preferred player-config precedent: compiled safe
  fallback, tracked template, sparse gitignored local overrides, per-key lenient
  reads, clamped writes, and removal of a local file when it matches the tracked
  default.
- `Engine.Scripting.Lua.API` gives each Lua global table a registration module;
  `Engine.Scripting.Lua.API.Shell` separately copies those tables into the
  in-game shell sandbox. A new `audio` table has to join both lists.
- `~/work/idou` validates the useful seed—a Haskell owner filling a native SPSC
  ring, health counters, resident decoding, and deterministic synth pieces—but
  its Haskell device callback and per-sample Haskell mixer are deliberately not
  copied.

A fresh tracker search on 2026-08-12 found no open or closed issue whose title
mentions audio, sound, SFX, music, or miniaudio, so this umbrella does not
silently duplicate an existing arc.

## Desired experience

Gameplay and UI code trigger a semantic ID, not an asset path or oscillator.
The call returns after a non-blocking enqueue; no gameplay producer waits for a
decoder, device, ring, mixer, or callback. Sample and synth definitions behave
the same at the call site.

World sounds remain anchored to their page positions while the camera pans or
rotates. Close zoom is slightly louder and narrower; farther detailed zoom is
slightly softer and hears a wider radius. Crossing the existing renderer fade
band drives only World to silence, exactly at `zoomFadeEnd`; UI remains audible.
World voices keep advancing while map-muted and become audible at their natural
later position when the player zooms back in.

An explicit player pause freezes only `player_pause: freeze` World voices. Their
sample cursor, oscillator phase, noise state, envelope, filter state, gate, and
loop position resume exactly. UI and future pause-continuing ambience remain
audible. Internal save/load/notification pauses do not freeze audio clocks. A
load publication clears the old session's voices rather than persisting them.

The Settings Audio tab contains Master, World, and UI sliders from 0 through
100. Dragging previews immediately. Apply changes the live session, Save writes
sparse player overrides, Back restores the saved values, and Defaults previews
the tracked defaults. Zero is a hard mute; authored and diagnostic gain remains
signed dB.

Missing files, bad definitions, rejected triggers, voice pressure, an unavailable
device, or an audio-worker failure never terminates the engine. The affected
sound becomes silent, valid siblings keep playing, and the engine receives a
bounded, rate-limited warning plus a queryable status snapshot.

## Scope

### In scope

- A vendored, pinned miniaudio low-level integration compiled as C.
- One native fixed-format stereo mixer, one PCM SPSC ring, one C callback, and
  real or null output selection.
- Eager WAV, FLAC, and MP3 decoding to shared resident float PCM; MP3 is
  one-shot only.
- Sample one-shots and full-file WAV/FLAC loops.
- Sine, band-limited saw/square/triangle, deterministic white noise, ADSR, and
  one optional LP/HP/BP filter per synth voice.
- Master/World/UI routing, gain smoothing, a transparent-under-threshold master
  safety stage, voice/concurrency/cooldown policy, and telemetry.
- Page-scoped world positions, camera-relative stereo panning, distance and
  vertical attenuation, camera re-basing, zoom range/gain, and map mute.
- Logical loop start/update/stop without exposing native handles.
- Player-pause freeze/resume and load-publication reset.
- YAML-authored sound types, instruments, and sounds; tracked runtime tuning;
  sparse player volume persistence.
- Haskell and Lua semantic APIs, the ninth capability, worker topology,
  deterministic offline/null tests, and a manual real-device probe.

### Out of scope

- Songs, music sequencing, tempo, musical transport, MIDI, note routing, and a
  music bus.
- AAC, Vorbis, Opus, streaming decode, long tracks, and compressed-format loop
  seeking.
- Weighted variants, randomized choice/ranges, multi-source sounds, layered
  oscillators, pink/colored noise, LFOs, filter envelopes, modulation, pitch
  glide, and automation.
- HRTF, occlusion, reverberation, Doppler, physical acoustics, multi-listener,
  surround output, and user-selectable output devices.
- Sample loop regions/crossfades; V1 loops the full decoded WAV/FLAC asset and
  content authors provide a seamless boundary when one is needed.
- Persistence of active voices or loops in save data. Gameplay reissues any
  future ambience after load.
- The production player-facing sound library. This arc supplies deterministic
  fixtures and enough semantic definitions to prove the foundation.

## Design

### Component and data flow

```text
Lua / world / unit / input / main-render producers
                     │ semantic IDs + logical LoopIds + listener snapshots
                     ▼
       AudioCapability { AudioTransport, AudioStatusRef }
                     │ bounded triggers + ordered controls + coalesced state
                     ▼
              Engine.Audio.Thread (only owner)
          catalog lookup · page gate · LoopId map · batching
                     │ compact POD commands and numeric handles
                     ▼
            cbits/audio/syn_audio_* (native C core)
   resident PCM · voices · oscillator/ADSR/filter · buses · spatial mix
                     │ one producer, float32 stereo frames
                     ▼
                miniaudio ma_pcm_rb (SPSC)
                     │ one consumer; copy/zero-fill only
                     ▼
          miniaudio C device callback ──► real or null device
```

The native callback never enters Haskell. Haskell never owns, mutates, or sees
native voice, sample-buffer, ring, device, or DSP pointers. Native code never
parses YAML, resolves `WorldPageId`, reads engine records, logs, allocates from a
gameplay call, or invokes a Haskell callback.

### Ownership map

| Component | Owns | May observe | Must not own |
|---|---|---|---|
| Gameplay/Lua producers | semantic request arguments | enqueue result | catalog entries, native handles, voices |
| `AudioTransport` | cross-thread mailboxes, sequence/session stamps | pressure counters | native core or content registry |
| Haskell audio worker | resolved catalog, logical/native ID maps, warning limiter, worker loop | transport, logger, status | per-sample DSP or device callback |
| Native core | decoded PCM, numeric definitions, voices, DSP, buses, ring, device/context | POD commands | YAML, engine page/camera types, Lua |
| Device callback | ring read cursor and atomic counters | requested frame count | allocation, locks, logs, commands, DSP |
| `AudioStatusRef` | bounded public snapshot/counters | multi-thread queue-drop updates | native pointers or unbounded error history |

### Proposed source layout

```text
src/Engine/Audio/Types/Base.hs          IDs, enums, FFI-independent values
src/Engine/Audio/Types.hs               page-scoped trigger/listener types
src/Engine/Audio/Transport.hs           abstract multi-producer mailbox
src/Engine/Audio/Status.hs              bounded shared health snapshot
src/Engine/Audio/Config/Runtime.hs      config/audio_runtime.yaml
src/Engine/Audio/Config/Player.hs       default + sparse local volumes
src/Engine/Audio/Catalog/Types.hs       raw and resolved content records
src/Engine/Audio/Catalog/Yaml.hs        per-entry YAML parsing
src/Engine/Audio/Catalog/Resolve.hs     inheritance/dependency isolation
src/Engine/Audio/Native.hs              opaque FFI and Storable POD layouts
src/Engine/Audio/Thread.hs              lifecycle facade
src/Engine/Audio/Thread/Boot.hs         catalog/native initialization
src/Engine/Audio/Thread/Dispatch.hs     command resolution and batching
src/Engine/Audio/Thread/Listener.hs     page/wrap/facing relative transforms
src/Engine/Audio/API.hs                 narrow Haskell producer primitives
src/Engine/Core/Capability/Audio.hs     two-field ninth capability projection
src/Engine/Scripting/Lua/API/Audio.hs   Lua stack marshalling
src/Engine/Scripting/Lua/API/Register/Audio.hs

cbits/vendor/miniaudio/miniaudio.h
cbits/vendor/miniaudio/miniaudio.c
cbits/vendor/miniaudio/LICENSE
cbits/audio/syn_audio.h                 the only Haskell-visible C header
cbits/audio/syn_audio_internal.h
cbits/audio/syn_audio_core.c            lifetime, commands, buses, voice pool
cbits/audio/syn_audio_device.c          real/null context, callback, recovery
cbits/audio/syn_audio_decode.c          eager bounded decode and dedup helpers
cbits/audio/syn_audio_dsp.c             oscillator, ADSR, filter, protection
cbits/audio/syn_audio_ring.c            ma_pcm_rb wrapper and counters
```

`Base.hs` follows the repository's no-local-dependency Base convention.
Implementation may make a further cohesive split when a module becomes large;
the names above define ownership, not a demand to force unrelated code into one
file.

### Native dependency and miniaudio surface

V1 vendors miniaudio 0.11.25's exact `miniaudio.c`/`miniaudio.h` pair and its
license, records the upstream tag and file checksums, and compiles the `.c` file
directly. This follows upstream's stated transition path toward 0.12 and avoids
an implicit system-library version. Miniaudio does not promise ABI compatibility
between releases, so upgrades are explicit reviewed source changes.

Synarchy uses only these low-level families:

- `ma_context`/`ma_device` and device notifications;
- `ma_pcm_rb` as the SPSC final-output ring;
- `ma_decoder` and miniaudio's converter/resampler path for eager WAV/FLAC/MP3
  normalization; and
- result/string helpers needed for diagnostics.

It does not use `ma_engine`, `ma_sound`, the resource manager, node graph,
high-level spatializer, or high-level effects. Synarchy's small fixed mixer must
remain visible and testable in `syn_audio_*` rather than split between two voice
allocators and policy systems.

### Mix format, buffering, and latency

The logical mix is interleaved IEEE float32 stereo at one configured sample
rate. All assets are converted to it at startup. The device is requested in the
same client format; miniaudio may convert from that client format to the
backend's native format. The native core reports both requested and negotiated
device facts.

The selected starting values are fixed by D-49:

| Parameter | Selected default | Valid range / invariant |
|---|---:|---|
| sample rate | 48,000 Hz | 22,050-96,000 Hz |
| render chunk | 256 frames (5.33 ms) | power of two, 64-1,024 |
| target ring fill | 1,024 frames (21.33 ms) | chunk multiple, at least 2 chunks |
| ring capacity | 4,096 frames (85.33 ms) | chunk multiple, at least target + 2 chunks |
| device period hint | 256 frames | 0 for backend default or 64-1,024 |
| device periods hint | 2 | 0 for backend default or 2-4 |
| worker idle wait | 1 ms | 0.25-5 ms |
| native command batch | 256 | 16-1,024 |
| event mailbox capacity | 2,048 | 128-65,536 |
| maximum voices | 128 | 16-1,024, hard-capped by allocation sanity |

`target_fill_frames / sample_rate` is the queueing portion of response latency;
the negotiated backend/device buffer is additional. The initial acceptance goal
is under 50 ms command-to-speaker on ordinary local hardware, without making a
hard promise about arbitrary drivers. The worker refills only to the target,
not the full ring, and always services commands before each refill pass.

The C callback copies as many requested frames as are readable, zero-fills the
remainder, atomically increments underrun and callback counters, and returns. It
allocates nothing, takes no mutex/STM lock, performs no logging, invokes no FFI
callback, touches no voice state, and never starts/stops/uninitializes a device.
Because it always writes every output byte and the master stage guarantees
finite `[-1,1]` samples, the device may enable miniaudio's
`noPreSilencedOutputBuffer` and `noClip` flags.

### C ABI

`cbits/audio/syn_audio.h` is a small statically linked ABI, not a mirror of
miniaudio structures. Every cross-boundary struct uses fixed-width C types,
contains no compiler `bool`, pointer-sized integer, flexible array, or Haskell
pointer, and begins with `abi_version`/`struct_size` where it may evolve.
Compile-time C size/alignment assertions and Haskell `Storable` tests must agree.

The public families are:

```c
typedef struct syn_audio_core syn_audio_core;

syn_audio_result syn_audio_create(
    const syn_audio_config*, syn_audio_sink_kind, syn_audio_core**);
syn_audio_result syn_audio_start(syn_audio_core*);
void             syn_audio_stop(syn_audio_core*);
void             syn_audio_destroy(syn_audio_core*);

syn_audio_result syn_audio_load_sample(
    syn_audio_core*, const char* utf8_path,
    const syn_audio_decode_limits*, uint32_t* sample_handle);
syn_audio_result syn_audio_add_instrument(
    syn_audio_core*, const syn_audio_instrument_desc*, uint32_t* handle);
syn_audio_result syn_audio_add_sound(
    syn_audio_core*, const syn_audio_sound_desc*, uint32_t* handle);

syn_audio_result syn_audio_service(
    syn_audio_core*, const syn_audio_command*, uint32_t command_count,
    syn_audio_command_result*, syn_audio_status*);
syn_audio_result syn_audio_render_offline(
    syn_audio_core*, float* interleaved_stereo, uint32_t frames,
    syn_audio_status*);
const char*      syn_audio_last_error(const syn_audio_core*);
```

`syn_audio_create` allocates the fixed voice/mix/ring storage and initializes a
stopped real or null device. Catalog upload is legal only before
`syn_audio_start`; this makes every allocation and decode an initialization
operation. `syn_audio_service` applies a bounded POD command batch, returns a
result per command (accepted/dropped/missing plus any evicted loop key), fills
the ring to target, handles non-callback device recovery, and snapshots native
status. `syn_audio_render_offline` bypasses the ring/device for exact DSP tests
but uses the identical command, voice, bus, and rendering functions.

All functions return a closed `syn_audio_result`; the last-error buffer is
owned by the core, bounded, and copied by the Haskell worker before the next C
call. No native error long-jumps, terminates the process, prints directly, or
crosses into Haskell as an asynchronous exception.

`syn_audio_command` is one fixed-size tagged POD record. Its payload can carry a
numeric sound handle, native `uint64_t` loop key, relative xyz, instance gain
dB, pitch semitones, listener affine rebase, bus targets, pause transition, or
session reset. Logical text IDs and YAML enums never cross the ABI.

### Haskell transport and worker

`AudioCapability` contains exactly:

```haskell
data AudioCapability = AudioCapability
  { acTransport ∷ AudioTransport
  , acStatusRef ∷ AudioStatusRef
  }
```

Both fields alias the containers on `EngineEnv`; the projection allocates
nothing. Native runtime state remains a local owned by the audio thread.

`AudioTransport` is one abstract transport field with several internal lanes,
not several new `EngineEnv` fields:

- a bounded `TBQueue` for one-shot triggers; enqueue uses `tryWriteTBQueue`, so
  high-rate gameplay cannot block and overflow is counted;
- an ordered low-rate control queue for loop start/update/stop, player pause,
  session reset, and shutdown wakeups; these correctness-critical controls are
  not silently discarded;
- latest-wins stamped slots for listener snapshots and three live volume
  values, preventing 60 Hz camera/slider publication from building a backlog;
  and
- one STM sequence allocator and session epoch internal to the transport so the
  worker can merge observations deterministically and reject stale-session
  work.

Only the abstract enqueue/read functions are exported; consumers cannot inspect
or mutate its lanes. The control queue is unbounded only for low-rate trusted
engine controls, and status warns if it exceeds 64 entries. Loop updates are
coalesced by `LoopId` within each worker batch. A producer that tries to use
loop controls as a per-tick sample stream is an API misuse and becomes visible
through backlog/coalescing counters rather than blocking a gameplay thread.

The worker cycle is:

1. take the latest listener/volume slots and drain at most the configured
   discrete batch in global stamp order;
2. resolve `SoundId`, pause/page/session policy, logical `LoopId`, and trigger
   overrides in Haskell;
3. convert accepted work to POD commands and call one `syn_audio_service`;
4. reconcile per-command results and loop evictions, merge native/Haskell
   counters, and publish status no more than ten times per second; and
5. wait for transport activity or the configured 1 ms refill deadline.

One cycle never drains without a bound before rendering. Catalog interpretation,
path checking, dependency warnings, and decoding happen before the device
starts, never in this loop.

### Worker boot and shutdown topology

`startAudioThread` takes `CoreCapability`, `AudioCapability`, and an explicit
sink policy; it does not take unrestricted `EngineEnv`. It absorbs audio-only
initialization errors. If a C core cannot be built, it publishes `AudioDisabled`,
logs the cause, and either runs a lightweight mailbox-draining degraded worker
or returns `Nothing`; boot continues either way.

Boot modes are fixed as follows:

| Mode | Audio worker | Sink | Catalog/mixer exercised |
|---|---|---|---|
| Graphical | yes, before input/Lua/world producers | real, fallback null | yes |
| Headless | yes, before Lua/world producers | forced null | yes |
| Offscreen | yes, before input/Lua/world producers | forced null | yes |
| Dump | no | none | no |
| Preview | no | none | no |

Every successful boot stores the returned `Maybe ThreadState` in `ewAudio`.
Audio is last in `postRenderWorkers`: combat/sim stop before render teardown as
today; unit/world/input/Lua producers then stop; audio drains their final
commands and stops last while the logger is still alive. The worker's `finally`
path calls `syn_audio_stop`/`syn_audio_destroy`; the C stop joins the callback
before freeing ring, device, catalog, or PCM. Repeated shutdown remains safe
through `ThreadState` and idempotent native stop/destroy guards.

### Authored content files

All three files have `schema_version: 1`, reject unknown top-level and entry
keys, and parse each list entry independently after the YAML document itself is
decoded. An invalid entry disables itself; a duplicate ID disables every entry
with that ID so file order never selects a winner. A syntactically invalid whole
file contributes an empty family and one clear warning, but the engine still
boots.

`data/audio/sound_types.yaml`:

```yaml
schema_version: 1
sound_types:
  - id: world_effect
    policy:
      bus: world
      spatial:
        mode: world
        min_distance_tiles: 1.0
        max_distance_tiles: 40.0
        rolloff: linear
        vertical_scale: 1.0
      gain_db: 0.0
      priority: 50
      concurrency:
        max_instances: 8
        overflow: steal_oldest
        cooldown_ms: 0
      loop:
        allowed: false
        stop_fade_ms: 20
      player_pause: freeze

  - id: ui_effect
    policy:
      bus: ui
      spatial: { mode: non_spatial }
      gain_db: 0.0
      priority: 80
      concurrency: { max_instances: 4, overflow: steal_oldest,
                     cooldown_ms: 0 }
      loop: { allowed: false, stop_fade_ms: 10 }
      player_pause: continue
```

`data/audio/instruments.yaml`:

```yaml
schema_version: 1
instruments:
  - id: short_square
    timbre:
      generator:
        waveform: square
        frequency_hz: 440.0
        start_phase: reset
        noise_seed: 1831565813
      envelope:
        attack_ms: 2.0
        decay_ms: 20.0
        sustain_level: 0.5
        release_ms: 35.0
      filter:
        mode: high_pass
        cutoff_hz: 180.0
        resonance_q: 0.707
      gain_db: -4.5
      default_gate_ms: 45.0
```

`data/audio/sounds.yaml`:

```yaml
schema_version: 1
sounds:
  - id: ui_confirm
    type: ui_effect
    source:
      synth:
        instrument: short_square
        gate_ms: 45.0
        timbre:
          generator: { frequency_hz: 880.0 }

  - id: pick_impact
    type: world_effect
    policy:
      gain_db: -2.0
      concurrency: { max_instances: 6 }
    source:
      sample:
        path: assets/audio/tools/pick_impact.wav
```

The `policy` block on a concrete sound is a recursive partial override of its
type's resolved policy. The `timbre` and `gate_ms` under `source.synth` are
recursive partial overrides of the named instrument. A mapping replaces a leaf
only when the leaf is present; absent leaves inherit. Lists do not merge (there
are no list-valued V1 leaves). Explicit YAML `null` is invalid rather than a
delete operation. Resolution order is compiled safe fallback, sound type,
instrument for synth-only fields, concrete sound policy/timbre/source, then the
closed runtime `gain_db` and `pitch_semitones` overrides.

Generator fields are a stable superset. `frequency_hz`/`start_phase` do not
affect white noise; `noise_seed` does not affect periodic waveforms. Keeping the
unused values legal lets a concrete sound override only `waveform` without
needing deletion semantics for fields inherited from another generator kind.
`start_phase` is `reset` or deterministic `random`; random phase and per-voice
white-noise streams derive from the authored seed, resolved `SoundId`, and
monotonic native voice sequence, so the same command sequence renders
bit-identically offline while concurrent voices do not all share one stream.

### Catalog validation and isolation

Identifiers match `[a-z][a-z0-9_]{0,63}`. `LoopId` is runtime text, non-empty
UTF-8, at most 128 code points. Asset paths must be relative, normalized,
contain no `..`, remain beneath `assets/audio/`, and end case-insensitively in
`.wav`, `.flac`, or `.mp3`.

The resolved range contract is:

| Value | Range / rule |
|---|---|
| authored gain | -96 to +24 dB |
| trigger gain | -48 to +12 dB |
| trigger pitch | -24 to +24 semitones |
| min distance | 0 to 1,024 tiles |
| max distance | greater than min, at most 4,096 tiles |
| vertical scale | 0 to 8 |
| priority | integer 0-100 |
| max instances | 1 through runtime `max_voices` |
| cooldown | 0-60,000 ms |
| stop fade | 0-5,000 ms |
| generator frequency | 20 Hz through min(20 kHz, 0.45 × mix rate) |
| gate | 0-60,000 ms; ignored by a running logical loop |
| attack/decay/release | each 0-30,000 ms |
| sustain | 0-1 |
| filter cutoff | 20 Hz through 0.45 × mix rate |
| resonance Q | 0.1-20 |

Only `linear` attenuation exists in V1. A one-value enum is intentional: the
key and unit are stable, unknown future-looking values are rejected, and adding
another curve later does not reinterpret existing data. UI-bus definitions must
be `non_spatial` and `player_pause: continue`; `freeze` is legal only on World.
World may be non-spatial for global world feedback.

Dependency resolution is deterministic and accumulates causes:

- an invalid `SoundTypeId` disables every sound naming it;
- an invalid `SynthInstrumentId` disables only synth sounds naming it;
- an invalid/missing/oversized asset disables every sound using that path;
- an invalid concrete sound disables only itself;
- a sound with zero or two source branches is invalid;
- an MP3 sound whose resolved loop policy allows loops is invalid; and
- valid siblings remain registered even when another dependency closure fails.

Warnings name the definition ID, originating file/list index, dependency chain,
and terminal cause. Registration and numeric handle assignment sort IDs/paths,
so failure order and test results never depend on `HashMap` or YAML ordering.

### Resident asset budgets

Decoding is eager, deduplicated by normalized path, bounded while reading, and
normalizes once to mix-rate float32 stereo. D-49 selects these defaults:

| Budget | Selected default |
|---|---:|
| encoded file | 16 MiB per asset |
| decoded duration | 15 seconds per asset |
| aggregate decoded duration | 120 seconds |
| aggregate decoded PCM | 64 MiB |

The decoder receives the remaining aggregate frame/byte allowance and stops at
limit + 1 rather than allocating an entire overlong file before rejecting it.
Budget accounting uses final stereo frames/bytes, not compressed size. Paths
are decoded in sorted order; an asset that would exceed the remaining aggregate
budget is disabled with both its own cost and remaining budget in the warning.

WAV and FLAC may be played once or loop the full decoded buffer. MP3 may be
played once only because reliable seamless loop points and seek behavior are
outside V1. Mono and multichannel inputs are converted to stereo through
miniaudio's channel converter; unusual rates are resampled at startup.

### Runtime tuning and player configuration

`config/audio_runtime.yaml` is a strict tracked engine file. It is never written
by Settings and never overlaid by a player-local file. A missing, malformed, or
internally inconsistent file warns and falls back atomically to compiled safe
defaults; mixing values from a partly invalid latency profile could violate ring
invariants. Unknown keys are errors.

The D-47/D-49 selected starting shape is:

```yaml
schema_version: 1
audio_runtime:
  mix:
    sample_rate_hz: 48000
    render_chunk_frames: 256
    target_fill_frames: 1024
    ring_capacity_frames: 4096
    max_voices: 128
    command_batch_limit: 256
    worker_idle_wait_us: 1000
  transport:
    event_capacity: 2048
    control_backlog_warn: 64
  device:
    period_frames: 256
    periods: 2
    retry_initial_ms: 1000
    retry_max_ms: 30000
  smoothing:
    bus_gain_ms: 20.0
    instance_gain_ms: 10.0
    pause_out_ms: 5.0
    pause_in_ms: 10.0
  spatial:
    close_range_scale: 0.85
    far_range_scale: 1.50
    close_gain_db: 1.5
    far_gain_db: -2.0
  limiter:
    knee: 0.95
  assets:
    max_encoded_mib: 16
    max_asset_seconds: 15.0
    max_total_seconds: 120.0
    max_decoded_mib: 64
  player_curve:
    exponent: 2.0
  telemetry:
    publish_hz: 10
    underruns_per_minute_warn: 3
    service_budget_fraction_warn: 0.50
    rate_limit_seconds: 10
```

Cross-field validation requires the buffer/chunk invariants in the latency table,
positive finite smoothing/spatial values, `close_range_scale ≤ far_range_scale`,
limiter knee in `[0.5, 1)`, player exponent in `[1,4]`, and positive budgets.
Retry uses 1, 2, 4, 8, 16, then 30-second delays and stays capped.

Player files use the existing sparse-overlay convention:

```yaml
# config/audio_default.yaml (tracked)
audio:
  master_volume: 100
  world_volume: 100
  ui_volume: 100
```

`config/audio.local.yaml` is gitignored and contains only values different from
that tracked template. Each key independently accepts an integer 0-100; a bad
key falls through to the layer beneath it while valid siblings remain effective.
Writes clamp, serialize only differences, and remove the local file when all
three values match the template.

The D-47 player curve is:

```text
v = 0                  → hard mute (linear gain 0, dB = -∞)
v = 1..100, p = 2.0    → linear gain = (v / 100)^p
                         effective dB = 20 log10(linear gain)
```

It maps 100 to 0 dB, 50 to approximately -12.04 dB, 10 to -40 dB, and 1
to -80 dB. The runtime exponent is engine tuning, not a player preference.
Status and programmer APIs report effective dB/linear gain, while Settings and
the player file remain 0-100.

### Native voices, buses, and DSP

The native core preallocates the voice pool and three interleaved chunk buffers:
World accumulation, UI accumulation, and final output. Master is a final gain,
not a source bus. Each source routes to exactly one of World or UI; there is no
way to bypass Master.

For every chunk the core:

1. clears World and UI accumulation buffers;
2. renders each active voice into its bus with source, envelope, optional
   filter, instance gain, spatial gain, and pan;
3. applies smoothed player bus gain and zoom World gain while summing buses;
4. applies smoothed Master gain;
5. replaces any non-finite sample with zero and counts it; and
6. leaves samples through `±knee` unchanged, then applies a continuous
   saturating soft knee approaching `±1` above the threshold.

The safety stage is not a loudness compressor and is not expected to engage in
normal authored mixes. It exists to prevent wrap/clipping and expose bad gain
staging through `limited_samples`/peak counters. Authored positive dB is legal,
so tests must prove the output remains finite and bounded under worst-case voice
concurrency.

Sample voices use a float frame cursor and linear interpolation so the closed
trigger pitch override can change playback rate. Every asset is already at mix
rate, so pitch ratio `2^(semitones/12)` is the only runtime resampling factor.
One-shots retire at end of PCM; loops wrap the whole WAV/FLAC buffer.

Synth processing order is generator → amplitude ADSR → optional filter → timbre
gain → common voice path. Exact algorithms are:

- sine is `sin(2πphase)`;
- saw and square use one-sample PolyBLEP discontinuity correction;
- triangle integrates the PolyBLEP square with leakage and clamps its state;
- white noise is xorshift32, with zero seed remapped to a fixed nonzero constant;
- ADSR uses sample-counted linear segments; release ramps from the current
  level to zero over exactly `release_ms` rather than subtracting a slope based
  on level 1;
- a one-shot enters release when its gate expires; a loop ignores gate, holds
  sustain, and uses its loop stop fade when stopped;
- filters use RBJ-cookbook biquad coefficients and transposed direct form II,
  with separate state per stereo channel; LP/HP/BP coefficients are fixed for
  the voice because V1 has no filter automation; and
- any DSP state whose absolute value falls below `1e-20` is snapped to zero to
  prevent denormal slowdowns without injecting a DC offset.

Bus target changes use a one-pole finite ramp over `bus_gain_ms`; per-loop gain
uses `instance_gain_ms`. Zero/mute is reached exactly at ramp completion. Pausing
a freeze voice does not advance source/DSP state: the mixer ramps its remembered
last output to zero over `pause_out_ms`, holds silence and exact source state,
then ramps newly resumed output in over `pause_in_ms`. This avoids a discontinuity
without consuming the sample/envelope/noise state the user asked to preserve.

### Spatial frame and camera re-basing

A listener snapshot contains active `WorldPageId`, camera page-space x/y,
z-slice, facing, zoom, and page wrap width/height. The main render loop publishes
one after camera integration every unlocked graphical/offscreen tick. Publication
is latest-wins; an unchanged snapshot is not sent to C. Headless tests may inject
a snapshot through the Haskell transport fixture, but no public Lua verb can
spoof the listener.

On a spatial trigger, the Haskell worker:

1. requires a current listener and exact page match;
2. computes shortest wrapped page-space dx/dy from listener to source;
3. rotates dx/dy into the camera-facing screen frame;
4. computes dz from source z to camera z-slice; and
5. sends only this relative xyz vector to C.

When the listener moves or rotates on the same page, Haskell derives an affine
relative-frame rebase from the old and new snapshots. C applies that matrix and
translation to every active World voice's stored relative position before the
next render. This keeps one-shots anchored while the camera moves without C
learning `WorldPageId`, world wrapping, camera records, or isometric topology.
A page change, missing page, or discontinuous teleport resets World voices
rather than applying a huge ambiguous transform; UI voices continue.

Spatial gain uses:

```text
d = sqrt(dx² + dy² + (vertical_scale × dz)²)
attenuation = 1                                  when d ≤ min_distance
attenuation = 1 - (d-min)/(scaled_max-min)       between min and scaled_max
attenuation = 0                                  when d ≥ scaled_max
```

`scaled_max = max_distance × zoom_range_scale`. Pan is the camera-screen
horizontal direction `clamp(dx / max(sqrt(dx²+dy²), ε), -1, 1)` mapped to
constant-power stereo gains `sqrt((1-pan)/2)` and `sqrt((1+pan)/2)`. A source
above or below the listener is centered; z changes distance only.

Within detailed zoom, a normalized detail factor interpolates
`close_range_scale/gain_db` at the camera zoom floor to
`far_range_scale/gain_db` at `zoomFadeStart`. Across the renderer's fade band,
World additionally multiplies by `1 - smoothstep(zoomFadeStart, zoomFadeEnd,
zoom)`, reaching exact zero at and beyond `zoomFadeEnd`. Native smoothing makes
rapid wheel motion click-safe. UI never receives either zoom factor.

### Voice allocation, concurrency, and loops

The D-48 policy gives each resolved sound priority 0-100,
`max_instances`, `cooldown_ms`, and per-sound overflow policy
`drop_new|steal_oldest`.

- Cooldown begins when a voice is accepted and uses the monotonic output-frame
  clock. It is global per `SoundId`, not per source position.
- Releasing/fading/frozen voices count as active until retired.
- At per-sound capacity, `drop_new` rejects; `steal_oldest` retires the oldest
  voice of that same sound before accepting the new one.
- At global capacity, the allocator chooses the oldest voice among the lowest
  priority. The incoming voice may steal it only when incoming priority is at
  least the victim priority; otherwise the incoming voice is dropped.
- One-shots and loops share the same pool and may steal each other. Authors
  protect important UI/loop sounds through higher priority, not an invisible
  native class.
- All choices are deterministic: priority, then start-frame, then voice-slot
  index. Every drop/steal reason is counted.

The public `LoopId` is text chosen by Haskell/Lua gameplay. The Haskell worker
maps it to a monotonic native `uint64_t` key; neither representation is persisted
or exposed as a voice handle.

- First `startLoop(loopId, soundId, ...)` validates that the resolved sound
  permits loops, allocates a native key, and is subject to cooldown/budgets.
- Repeating start with the same `LoopId` and same `SoundId` is idempotent and
  acts like update; it neither retriggers nor allocates a second voice.
- Repeating start with the same `LoopId` but a different `SoundId` is rejected
  as an identity conflict.
- Update changes only optional position and instance gain; it cannot change
  pitch, routing, source, timbre, or policy.
- Stop removes the logical mapping immediately and applies the sound's authored
  `stop_fade_ms`. Reusing the text ID may start a new native loop while the old
  one finishes its inaccessible fade.
- A native allocation result identifies an evicted loop key. Haskell removes
  its reverse mapping; later updates/stops of a missing ID are harmless,
  rate-limited warnings.
- MP3 never passes loop validation.

### Pause, zoom, and session lifecycle

`engine.setPaused` enqueues `AudioSetPlayerPaused` only after the pause change is
accepted. No audio code watches `enginePausedRef`, so save/load and notification
pauses do not masquerade as player intent.

On pause, existing `freeze` voices preserve exact state after the configured
output ramp; new `freeze` one-shots/loop starts are dropped and counted;
`continue` voices and commands remain active. Resume fades retained voices in
and advances from the preserved cursor/phase. World map mute is orthogonal:
voices of either pause policy advance while zoom gain is zero.

The transport stamps every request with a runtime-only session epoch. Load
publication atomically advances the epoch and enqueues `AudioResetSession`; the
worker clears all voices, loop maps, per-sound cooldown history, ring contents,
and stale older-epoch commands. This is not a save-barrier owner and adds no
audio field to `SaveData`. A normal save does nothing to audio.

### Haskell and Lua API

The narrow Haskell producer surface is asynchronous and semantic:

```haskell
playSound       ∷ AudioCapability → SoundId → TriggerContext → IO EnqueueResult
startSoundLoop  ∷ AudioCapability → LoopId → SoundId
                → TriggerContext → IO EnqueueResult
updateSoundLoop ∷ AudioCapability → LoopId → LoopUpdate → IO EnqueueResult
stopSoundLoop   ∷ AudioCapability → LoopId → IO EnqueueResult
readAudioStatus ∷ AudioCapability → IO AudioStatusSnapshot
```

`TriggerContext` contains optional page/position plus optional finite
`gain_db`/`pitch_semitones`; spatial resolved sounds require page and position,
while non-spatial sounds require neither. `LoopUpdate` exposes only optional
page/position and gain. `EnqueueResult` distinguishes enqueued, bounded-trigger
queue full, and structurally invalid immediate arguments; it cannot promise the
worker later finds a valid `SoundId` or available voice.

Lua receives a separate global table:

```lua
audio.play("pick_impact", {
    pageId = pageId,
    position = { x = 12, y = 8, z = 3 },
    gainDb = -1.5,
    pitchSemitones = 0,
})

audio.startLoop("forge_bell", "forge_hum", opts)
audio.updateLoop("forge_bell", { position = pos, gainDb = -3 })
audio.stopLoop("forge_bell")
local status = audio.getStatus()
```

The four trigger verbs return a boolean enqueue result. Bad Lua types/ranges
return `false` and log through the existing guarded Lua-function boundary rather
than throwing through the Lua thread. The `audio` table is copied into
`shellSandbox` for diagnostics. No API accepts paths, bus names, ADSR/filter
parameters, priority, concurrency, pause policy, source kind, or native handles.

Settings uses these engine-owned config verbs on the same table:

```lua
audio.getSavedVolumes()       -- effective tracked + local values on disk
audio.getDefaultVolumes()     -- tracked template without local overrides
audio.setVolumes(table)       -- live preview only
audio.saveVolumes(table)      -- sparse persistence + live apply
```

Each returns/accepts `{master=0..100, world=0..100, ui=0..100}`. Config I/O is
performed outside the callback and errors return `false` while preserving the
last live values.

### Settings integration

Audio is inserted after Graphics: General, Graphics, Audio, Input,
Notifications. `scripts/settings/audio_tab.lua` creates three existing slider
widgets and value labels; it needs no new textures.

`scripts/settings/data.lua` adds a separate `currentAudio`, `pendingAudio`, and
`savedAudio` family. Slider `onChange` updates pending and immediately calls
`audio.setVolumes`. Settings page rebuild/resize snapshots those values rather
than rereading native status and losing unapplied state.

- Apply copies pending to current and reapplies live, but does not write.
- Save applies, calls `audio.saveVolumes`, and on success refreshes saved.
- Back reloads `getSavedVolumes`, applies them live, and resets all three tables.
- Defaults loads `getDefaultVolumes`, updates current/pending, and previews live;
  it becomes persistent only if the player presses Save.

The audio worker loads effective player volumes before starting its device, so
the first audible frame uses the saved values. Settings APIs operate safely
when audio is disabled: persistence still succeeds, status stays disabled, and
the saved choice applies on a future successful boot.

### Failure isolation and health

Graphical first attempts the platform's normal backend. Initial failure creates
a null device so the same callback/ring/voice clocks continue silently, marks
`AudioDegradedNull`, and retries the real backend with capped exponential delay.
A device stop/reroute/interruption notification only sets native atomic flags;
the Haskell-owned service call performs stop/uninit/reinit outside the callback.
On any real↔null switch the ring resets so old queued sound cannot burst later;
voices continue on wall-clock output through the null sink.

Headless/offscreen explicitly initialize a context restricted to
`ma_backend_null` and never probe or retry a physical backend. Dump/preview have
no audio core. Failure even to create a null/native core produces
`AudioDisabled`; enqueue remains safe and bounded.

`AudioStatusSnapshot` is bounded and contains:

- lifecycle (`starting`, `running_real`, `running_null`, `degraded_null`,
  `disabled`, `stopped`), sink/backend/device name, requested rate, negotiated
  period, and last transition/error;
- current/min/max ring fill, rendered/callback frames, total/recent underruns,
  service-call duration and budget violations;
- active/peak voices and loops, accepted, cooldown/per-sound/global/paused/page/
  unknown/queue drops, steals, and missing-loop controls;
- current/peak trigger and control backlog, coalesced listener/volume/loop
  updates, and stale-session discards;
- valid/disabled type/instrument/sound/asset counts and decoded frames/bytes;
  and
- mix peak, limited/non-finite samples, last publish time, and a monotonic
  snapshot sequence.

The snapshot holds one bounded error text, not a growing list. The engine logger
gains an appended `CatAudio` category and `ENGINE_DEBUG=Audio` parsing. Catalog
causes warn once at boot. Runtime diagnostics key by `(reason, SoundId/LoopId)`,
emit immediately, then at most once per configured ten seconds with a suppressed
count. Native callback/device notifications never log directly.

Health thresholds do not shut audio down. Three underruns in a rolling minute,
control backlog above 64, or native service time above half the audio represented
by the frames it rendered changes status to degraded and warns. Recovery clears
the recent condition but preserves lifetime counters.

## Decisions

### D-1. Production mixing and DSP are native C responsibilities

Synarchy will not copy Idou's per-sample Haskell production renderer. The
performance- and real-time-sensitive mixer, synth stepping, spatial gain/pan,
bus accumulation, master output processing, resident sample cursors, and device
callback may live in C. Haskell remains the orchestration and engine-integration
layer. The all-Haskell implementation was valuable as a learning/reference
system but is rejected as the production boundary.

### D-2. A Haskell audio worker remains the sole control owner

Moving rendering into C does not make gameplay threads native-audio clients.
One Haskell worker owns lifecycle and command ordering and invokes the native
core in coarse operations. This preserves Synarchy's queue-based thread model,
keeps native handles out of gameplay code, and gives one place to enforce page,
priority, and content policies.

### D-3. The device boundary is one native SPSC PCM output ring

The producer/native mixer writes a completed interleaved PCM mix to one
single-producer/single-consumer ring; the device callback consumes it. Source
types do not each own a device ring, and the output ring is not itself the
multi-input abstraction.

### D-4. V1 supports small resident samples and a minimalist synth

Short assets in the selected sample formats are fully decoded and resident.
Artificial sounds come from a small synth descended conceptually from Idou, not
from a general instrument or patch system. Both source types mix concurrently
through the same voice, spatial, bus, and master-output policies.

### D-5. The song system is outside this arc

Idou's song facade, adaptive timeline, music transport, MIDI support, tempo/bar
scheduling, arrangement, and patch authoring are not prerequisites or hidden
follow-ons of this audio foundation. A future music design may build on the
foundation but must define its own product contract.

### D-6. The camera is the world-audio listener

Spatial world sounds are heard relative to the current camera on the active
world page. UI and other declared non-spatial sounds bypass this listener.

### D-7. The minimal synth includes five generators, ADSR, and one filter

V1 supports sine, band-limited saw, square, triangle, and deterministic white
noise. Each voice has one ADSR amplitude envelope and one bypassable filter
selectable as low-pass, high-pass, or band-pass. Pink/colored noise, pitch
glide, multiple oscillators, filter-envelope modulation, LFOs, and other
modulation remain outside this arc. All parameters in the selected surface are
authored data rather than compiled per-sound constants.

### D-8. Sound types, synth instruments, and sounds are data-driven

Gameplay triggers a validated `SoundId`; it does not provide an asset path or
construct a native synth patch. Tracked YAML defines reusable `SoundType`
policy, reusable `SynthInstrument` timbre, and concrete sample-or-synth sounds.
The three registries have distinct responsibilities under one deterministic
override model; exact YAML key spelling remains component-design work. None of
these authoring concepts is hard-coded into gameplay modules.

### D-9. V1 has Master, World, and UI volume control

Every audible source routes to either the `World` or `UI` child bus and then to
`Master`. A new Audio tab in Settings exposes sliders for all three gains. The
values participate in Settings' pending/apply/save/back/default lifecycle and
persist as player preferences; engine mixer budgets and sound content are not
stored in the same local override.

### D-10. Concrete sounds may override all authored parent fields

`SoundType` owns reusable routing/playback policy and `SynthInstrument` owns
reusable timbre/DSP policy. A concrete `SoundDefinition` may override any
authorable field from either referenced definition, using explicit nested
blocks and deterministic precedence. Runtime trigger calls do not share that
broad authority: they provide the semantic sound ID and runtime context, plus
only deliberately exposed instance gain or pitch when needed.

### D-11. Each V1 SoundId has exactly one source

A concrete sound selects either one supported sample asset or one synth
instrument with its pitch/gate values. V1 has no weighted source list, random
variant selection, or random pitch/gain range. The semantic ID boundary leaves
those features possible later without changing gameplay call sites.

### D-12. Player pause freezes world-event voices, not the whole mixer

An explicit player pause freezes voices whose sound-type policy is `freeze`,
including their sample cursor or synth oscillator, envelope, and filter state.
UI remains audible. Future ambience may select `continue` and remain audible
while paused even when it uses spatial/world routing, so pause policy is
orthogonal to bus choice. Save/load and other short engine-internal pauses do
not freeze audio. This rejects both a global audio pause, which would silence
UI and ambience, and an unconditional World-bus pause, which would couple two
independent policies.

### D-13. Detailed-view zoom changes range and gain; the zoom map is silent

Close zoom uses a slightly louder World trim and a narrower audible radius.
Zooming out within the detailed view gently lowers World gain while expanding
the radius. Across Synarchy's existing zoom-map fade band the target World gain
drops sharply to zero at `zoomFadeEnd`; once the HUD is `zoomed_out`, no world
sound is audible. Native gain changes are smoothed even when the target changes
sharply, preventing clicks. UI gain is independent of camera zoom.

### D-14. Audio volume sliders live-preview

Dragging Master, World, or UI immediately previews the pending value. Apply
accepts the preview for the session, Save persists sparse player overrides,
Back restores the saved values, and Defaults previews the tracked defaults.
This follows the settings menu's existing preview-and-revert pattern and lets
the player judge an audio setting while changing it.

### D-15. Paused voices retain state; new paused-world triggers are dropped

At an explicit player pause, every existing `freeze` voice retains its sample
cursor or synth oscillator, envelope, and filter state and resumes from that
point. This is native voice-state retention, not sound restart, and is part of
V1 rather than a deferred enhancement. A freeze-policy trigger received after
the pause boundary is dropped and counted instead of being queued or created
frozen. The completed-mix ring may still play its short already-rendered tail;
the maximum response delay is the configured target-fill latency, after which
the retained voice state no longer advances until resume.

### D-16. Zoom-map muting does not stop voice clocks

World voices continue advancing while the zoom map drives their bus to zero.
Zooming back into the detailed view smoothly raises the gain of any voice still
active at its current playback point. Finished one-shots stay finished; loops
return at their naturally advanced cursor. Zoom therefore changes audibility,
not world-audio time.

### D-17. Players use 0-100; authors and programmers use signed decibels

Settings stores and displays integer percentages. Zero is a hard mute and 100
is 0 dB/unity; intermediate values follow a documented perceptual curve whose
tuning belongs in tracked runtime configuration. YAML definitions and
programmer-facing gain APIs use signed decibels with explicit `_db` names.
Native DSP uses linear amplitude after boundary conversion, and the master
protection stage remains authoritative when positive gains combine.

### D-18. Graphical uses real output; headless and offscreen use null output

Graphical mode opens the normal playback device. Headless and offscreen start
the functional audio runtime against miniaudio's null destination, exercising
the worker, native mixer, ring, callback, and health reporting without emitting
speaker output. Dump and preview start no audio runtime; preview can gain real
or null output later if an audio-preview product is designed. Direct offline
rendering remains available for deterministic tests that should not depend on
callback timing.

### D-19. Invalid sound data disables only the affected dependency closure

A missing, malformed, unsupported, oversized, or unresolved sound definition
never crashes the game and never interrupts correctly defined sounds. Startup
keeps every valid type, instrument, asset, and sound, disables only definitions
that depend on an invalid node, and reports each cause through engine warnings
and audio status. Triggering a rejected or unknown ID is a silent no-op with
telemetry and rate-limited diagnostics. CI catalog validation remains strict so
repository-owned content cannot treat runtime tolerance as permission to ship
broken definitions.

### D-20. V1 sample assets are eagerly decoded and shared

During audio startup, every unique sample asset referenced by the valid catalog
is decoded once into the fixed resident PCM representation. Multiple sounds
referencing the same path share that buffer. Decode failure invalidates only
dependent sounds. Per-asset duration and aggregate memory budgets apply to the
decoded representation, not the encoded file size. Playback never performs
first-use decoding on a gameplay or callback path.

### D-21. Sample loading supports multiple encoded formats but not MIDI

The source schema and loader are format-neutral: a sample definition names one
supported encoded audio asset which is normalized to the same resident PCM
form. Synarchy will preserve Idou's generic miniaudio-decoder boundary rather
than expose a WAV-specific API. MIDI remains excluded with the song system.
The exact codec set is fixed by D-22.

### D-22. V1 sample formats are WAV, FLAC, and non-looping MP3

V1 accepts miniaudio's dependency-free built-in decoder set: WAV, FLAC, and
MP3. WAV and FLAC definitions may be one-shot or looping. MP3 definitions are
non-looping because encoder delay/padding makes it a poor precision-loop source;
a looping MP3 is invalid sound data and disables only that definition under
D-19. AAC/M4A, Vorbis, Opus, and other formats requiring a custom decoder are
deferred until a concrete asset justifies the additional dependency. All three
accepted formats eagerly normalize to the same shared resident PCM form, so no
codec logic enters mixing or the callback.

### D-23. V1 includes logically identified controllable loops

WAV and FLAC sounds may be controllable loops. Haskell callers assign a logical
`LoopId` and can start, update, or stop it; starting an active identity updates
that loop rather than creating another voice. Updates may move the spatial
position and change instance gain. Stop follows a fade duration authored in the
resolved sound policy, preserving click-free release without giving callers
arbitrary envelope control. Native voice handles remain private to the audio
worker. MP3 cannot loop under D-22.

### D-24. Runtime trigger overrides are deliberately closed

A one-shot trigger carries `SoundId`, page/position for a spatial sound, and
optional signed `gain_db` and `pitch_semitones`. A loop start additionally
carries `LoopId`; loop updates carry identity plus optional position and
instance `gain_db`; stop carries only identity and uses the authored fade.
Callers cannot override routing, attenuation, ADSR, filter, loop policy,
priority, concurrency, pause behavior, asset, or instrument. This retains
useful per-event variation without turning gameplay code into an unvalidated
sound-authoring layer.

### D-25. Audio may add a ninth `audio-transport` EngineEnv capability

The repository maintainer explicitly approves the narrow new capability
required by `docs/engineenv_capability_inventory.md` section 6.4(c).
`AudioCapability` contains only the multi-producer command queue and shared
status snapshot, with a total `EngineEnv` projection. The audio worker owns the
device, decoded assets, native core, output ring, mixer, DSP, and voices
privately. This decision does not approve unrelated audio state on `EngineEnv`,
a generic capability, or another permanent full-access importer; implementation
must make the complete synchronized inventory/audit/test changes mandated by
section 6.4(c).

### D-26. The foundation uses deterministic fixtures; production SFX come later

This arc creates or checks in tiny deterministic fixtures sufficient to prove
WAV, FLAC, MP3, looping, spatial, bus, and failure behavior, plus a few named
reference sounds for manual development. The maintainer may supply a small
number of representative development files, but they are not assumed to be the
game's production library and are checked in only with suitable provenance and
redistribution rights. Selecting, authoring, licensing, and reviewing the full
player-facing sound library is a later content effort and does not block the
audio foundation.

### D-27. Use a Synarchy-owned low-level miniaudio core

Vendor and pin miniaudio 0.11.25, compile its `.c` file, and use device,
decoder/converter, and PCM-ring primitives only. Do not adopt `ma_engine`, the
resource manager, node graph, high-level spatializer, or high-level effects.
This preserves one allocator/policy authority and a deterministic offline path.

### D-28. The logical output is one float32 stereo mix

Every resident asset and generated voice enters one interleaved float32 stereo
mix at the configured engine rate. Backend conversion is a device concern. The
format/channel count is an invariant, not authored data.

### D-29. The device callback is C-only and ring-only

The callback may copy from the SPSC ring, zero-fill, and update atomics. It may
not allocate, lock, log, mix, handle commands, call Haskell, or manage device
lifecycle.

### D-30. Cross-boundary work uses a versioned POD batch ABI

Haskell uploads validated numeric definitions before start and submits bounded
fixed-layout command batches during service. No Haskell/native callback,
logical text ID, YAML value, miniaudio struct, or native pointer crosses the
boundary.

### D-31. Authored inheritance is recursive, explicit, and strict

Types own policy, instruments own timbre/default gate, and sounds own one source
plus partial `policy`/`timbre` overrides. Missing leaves inherit, explicit null
and unknown keys fail, duplicate IDs have no winner, and only runtime gain/pitch
may override the resolved definition.

### D-32. Catalog failure isolation follows dependency closures

Malformed definitions and assets disable only themselves and definitions that
reference them. Registration order is sorted and deterministic. A whole-file
syntax failure empties that family but does not abort the engine.

### D-33. Decode is eager, bounded, shared, and startup-only

Each normalized path is decoded at most once to mix-format PCM before device
start. Limits apply while reading and measure decoded duration/bytes. No decoder
or allocation runs on trigger or callback paths.

### D-34. The native synth uses fixed deterministic algorithms

Periodic generators use sine/PolyBLEP/integrated triangle, noise uses seeded
xorshift32, ADSR is sample-counted linear, filters are fixed RBJ TDF2 biquads,
and denormals snap to zero. V1 adds no hidden modulation or automation.

### D-35. Master protection is transparent below an authored threshold

Bus and Master gains smooth, non-finite output becomes zero and is counted, and
only samples beyond a configured knee enter a continuous saturating curve. The
stage is safety/telemetry, not a creative compressor.

### D-36. Haskell publishes relative-frame rebases, not engine state

Haskell resolves page, wrapping, camera facing, and listener movement. C stores
only listener-relative vectors and applies affine rebase commands to anchored
World voices; it never learns Synarchy page/camera/topology types.

### D-37. World zoom changes range and one independently smoothed bus gain

Detailed zoom interpolates authored max-distance scale and a modest dB trim;
the renderer fade band multiplies World to exact zero at `zoomFadeEnd`. Voice
clocks continue and UI is untouched.

### D-38. Logical loops bind one immutable SoundId per lifetime

Repeated same-ID/same-sound starts update idempotently; a different sound is a
conflict. Stop applies authored fade and releases the logical ID immediately.
Native loop keys are monotonic worker-private values, never caller handles.

### D-39. The shared transport is one abstract multi-lane container

Bounded one-shots cannot block; low-rate correctness controls retain ordering;
camera/volume state is latest-wins; sequence/session stamps let the worker merge
and discard deterministically. This remains one capability field rather than
loose shared audio state.

### D-40. Audio starts before producers and stops after them

Graphical uses real-with-null-fallback, headless/offscreen force null, and
dump/preview start none. `ewAudio` is last in post-render teardown so final
producer commands drain before callback/native destruction.

### D-41. Audio failure never advances engine lifecycle to shutdown

Initialization, device, decode, worker, and malformed-content failures publish
disabled/degraded status and warnings. They do not write `CleaningUp` or throw
through boot/gameplay threads.

### D-42. Only accepted `engine.setPaused` transitions control audio pause

This boundary represents explicit player intent. Freeze voices preserve exact
source/DSP state with output-only ramps; new freeze triggers drop; internal
pause causes do not affect audio clocks.

### D-43. Load replacement is an epoch reset, not persisted audio state

Load publication advances a transport epoch and clears voices, ring, loop maps,
cooldowns, and stale commands. Audio is neither a save component nor a save
barrier owner.

### D-44. Public APIs expose semantic IDs and a closed override surface

Haskell/Lua can play, start/update/stop logical loops, and read status. Settings
can read/apply/save three volumes. Paths, native handles, routing, content policy,
and DSP parameters remain unavailable to triggers.

### D-45. Audio Settings owns a separate live/pending/saved family

Three existing sliders live-preview through the worker. Apply is session-only,
Save persists sparse overrides, Back restores disk, and Defaults previews the
tracked template. UI rebuilds preserve unapplied values.

### D-46. The foundation proves formats with generated deterministic fixtures

Tests use a mathematically generated source signal and checked/generated WAV,
FLAC, and MP3 derivatives with a script and provenance metadata. MP3 checks use
tolerances; lossless/offline DSP uses exact or tight numeric assertions. A
production sound library remains later work.

### D-47. Player volume uses a squared-amplitude curve

For values 1-100, linear gain is `(value / 100)^2`; 0 is a hard mute. This
makes 100 = 0 dB, 50 approximately -12.04 dB, 10 = -40 dB, and 1 = -80 dB.
The exponent remains a tracked runtime tuning value with 2.0 as the selected
default; Settings and player config remain integer 0-100. This resolves Q-20.

### D-48. Voice pressure uses deterministic authored priority

The shared pool contains 128 voices by default. Per-sound capacity selects
`drop_new` or `steal_oldest`; at global capacity an incoming voice may steal the
oldest member of the lowest-priority group only when its priority is at least
the victim's. Loops have no hidden protection and use authored priority. This
resolves Q-21.

### D-49. V1 starts at the selected latency and resident budgets

The defaults are 48 kHz, 256-frame render chunks, 1,024-frame target fill, a
4,096-frame ring, 128 voices, 16 MiB encoded per asset, 15 decoded seconds per
asset, and 120 seconds/64 MiB aggregate decoded PCM. They remain tracked runtime
tunables subject to the specified range and cross-field validation. This
resolves Q-22.

## Open questions

### Q-1. What is the exact minimal synth surface?

Resolved by D-7.

### Q-2. Are synth and sample sounds named definitions or raw trigger parameters?

Resolved by D-8.

### Q-3. Which fixed buses exist in v1?

Resolved by D-9.

### Q-4. What happens to world audio while simulation is paused?

Resolved by D-12.

### Q-5. Does camera zoom change listening range?

Resolved by D-13.

### Q-6. Which boot modes start real, null, or no audio?

Resolved by D-18.

### Q-7. Where do initial production audio assets come from?

Resolved by D-26.

### Q-8. How broad is definition inheritance and overriding?

Resolved by D-10.

### Q-9. Does one SoundId select one source or a weighted variant set?

Resolved by D-11.

### Q-10. Do Audio sliders live-preview pending values?

Resolved by D-14.

### Q-11. What happens to new world-event triggers received during player pause?

Resolved by D-15.

### Q-12. Do muted world voices continue advancing on the zoom map?

Resolved by D-16.

### Q-13. How should 0-100 volume values map to native gain?

Resolved by D-17.

### Q-14. Is catalog validation atomic or per-definition tolerant at runtime?

Resolved by D-19.

### Q-15. Are all V1 sample assets decoded during audio startup?

Resolved by D-20.

### Q-16. Which encoded sample formats are supported in V1?

Resolved by D-22.

### Q-17. Does V1 include controllable looping voices?

Resolved by D-23.

### Q-18. Which values may a trigger override at runtime?

Resolved by D-24.

### Q-19. May audio add Synarchy's ninth EngineEnv capability?

Resolved by D-25.

### Q-20. Is the proposed squared-amplitude player volume curve correct?

Resolved by D-47.

### Q-21. Is the proposed deterministic voice-stealing rule correct?

Resolved by D-48.

### Q-22. Are the proposed latency and resident-budget defaults acceptable?

Resolved by D-49.

## Documentation roadmap

This roadmap keeps documentation work attached to the slice that makes each
contract true. These documents are implementation deliverables, not
prerequisites for issue processing. Until a delivery slice lands, this design
remains the authoritative description of the corresponding unimplemented
contract. Documentation must land in the same PR as the behavior or fixture it
first describes, and AUD-12 performs the final consistency pass across the
completed system.

### New documentation

| Path | Purpose and required contents | Delivery ownership |
| --- | --- | --- |
| `docs/audio_authoring.md` | Canonical content-author guide for `sound_types.yaml`, `instruments.yaml`, and `sounds.yaml`: schema versions, exact fields and ranges, inheritance and override precedence, semantic IDs, sample versus synth definitions, buses, spatial and pause policies, one-shots and loops, permitted runtime overrides, validation isolation, codec and asset limits, and copyable examples. | AUD-12 creates and verifies it from the contracts delivered by AUD-3 through AUD-8. |
| `docs/audio_runtime.md` | Maintainer/operator guide for `config/audio_runtime.yaml`, real/null/none boot modes, worker and callback ownership, latency and resident budgets, status fields and thresholds, bounded warnings, device fallback/retry, `CatAudio` diagnostics, headless behavior, manual probing, troubleshooting, and clean shutdown. It links back to this design for rationale instead of repeating the full architecture. | AUD-12 creates and verifies it from AUD-2, AUD-3, and AUD-11. |
| `test/fixtures/audio/README.md` | Provenance for every deterministic WAV, FLAC, and MP3 fixture: source signal, purpose of each derivative, generator script and command, tool/codec versions, redistribution terms, hashes or reproducibility check, and the reason MP3 assertions use tolerant acoustic checks rather than byte/sample equality. | AUD-4 creates it with the first generated fixtures; AUD-12 verifies it and the generation path. |
| `assets/audio/ATTRIBUTION.md` | Per-asset production/development-library provenance: repository path, title or description, creator/source, license, source URL when applicable, modifications, and redistribution notes. Generated test fixtures stay in their colocated fixture README. | The first later change that adds non-generated redistributable audio creates it and records every added asset. This is future sound-library work and does not block the foundation epic. |

`docs/audio_authoring.md` and `docs/audio_runtime.md` deliberately serve
different audiences. The authoring guide is the data contract for people
creating sounds; the runtime guide is the operational contract for people
building, testing, and diagnosing the engine. Neither becomes a second design
record: behavioral rationale and deferred alternatives remain in this document.

### Existing documentation to update

| Path | Required update | Delivery ownership |
| --- | --- | --- |
| `docs/engineenv_capability_inventory.md` | Add the approved ninth `audio-transport` capability, its two-field projection, thread/lifecycle ownership, vocabulary, permanent-import boundary impact, and synchronized audit expectations. | AUD-2, in the same PR as the capability and audit changes. |
| `docs/persistence_state_inventory.md` | Record audio transport/runtime state as non-persistent and document the separate player audio-settings family and load/session-reset boundary. | AUD-2 records the ownership boundary; AUD-10 updates it if the landed settings representation requires a more specific inventory entry. |
| `tools/README.md` | Document the headless null probe and manual graphical probe, including invocation, observable facts, speaker-safety expectations, CI eligibility/manual-only classification, and troubleshooting use. | AUD-11 adds the manual probe entry; AUD-12 adds the integrated null probe entry and performs the final inventory check. |
| `docs/player_manual.md` | Explain the Audio settings tab, Master/World/UI sliders, 0-100 user scale, live preview, Apply/Save/Back/Defaults behavior, and why map zoom silences only World audio. Avoid exposing programmer-facing dB values as player controls. | AUD-10, with AUD-12 checking the final wording against behavior. |
| `CLAUDE.md` | Add the audio worker to the threading model and record the relevant focused tests/probes, null-headless rule, configuration/data locations, and any new maintenance convention future agents must preserve. | AUD-2 adds thread ownership; AUD-12 completes testing and maintenance guidance. |
| `README.md` | Add only a short audio/resource or setup note if the finished foundation changes how a developer runs the game or obtains required assets. Do not duplicate either audio guide. | AUD-12 decides from the landed developer experience; no change is required when setup remains unchanged. |

### Documentation completion gate

AUD-12 may mark this roadmap complete only when:

- every path and command in the guides exists and works from the repository
  root;
- authoring examples are accepted by the real parsers and match the shipped
  default schemas;
- status names, thresholds, backend modes, and recovery behavior match the live
  runtime rather than an earlier slice proposal;
- the fixture-generation procedure reproduces legally redistributable inputs
  for all three supported formats, with any lossy-tool variation called out;
- the headless instructions force and assert null output without probing a
  physical device;
- the capability, persistence, player, probe, and agent-maintenance documents
  agree on ownership and lifecycle; and
- the production sound library remains explicitly outside this epic, while its
  future attribution requirement is not lost.

## Verification strategy

Verification separates deterministic rendering, engine integration, null-device
behavior, and physical-device behavior.

- Native/FFI layout tests pin every POD size/alignment/tag and ABI version.
- Offline C-core tests render exact chunks without a device and cover ring
  wrap/partial availability, zero-fill, finite/bounded output, bus summing,
  smoothing, safety-stage counters, voice retirement, and deterministic victim
  selection.
- Decoder fixtures originate from one generated signal. WAV/FLAC assert channel,
  rate, duration, and lossless/tight sample expectations; MP3 asserts tolerant
  duration/frequency/energy and rejection as a loop. Mono/stereo/rate conversion,
  dedup, per/aggregate budgets, missing/corrupt files, and limit+1 early aborts
  are covered.
- Synth tests cover frequency, band-limited discontinuities, triangle bounds,
  seeded-noise reproducibility/independence, every ADSR stage including release
  from a partial level, LP/HP/BP response, bypass, denormal cleanup, gate, pitch,
  and coexistence with sample voices.
- Catalog hspec tests cover schema versions, unknown keys, duplicate IDs, source
  union, inheritance precedence, dependency closures, enum/range/path rules,
  MP3 loop prohibition, stable ordering, and survival of valid siblings.
- Transport tests cover non-blocking bounded enqueue, ordered controls,
  latest-wins listener/volumes, sequence merge, loop-update coalescing, session
  stamps, pressure counters, and bounded status/error text.
- Capability tests prove the projection aliases exactly transport/status; the
  capability and persistence audits prove the ninth vocabulary, empty ceiling,
  and absence of native state/full-access growth.
- Boot tests cover real/null/none mode selection, worker construction in all
  five boot modules, failure-to-disabled behavior, producer-before-audio
  shutdown ordering, partial-boot cleanup, idempotent stop, and callback join
  before free.
- Spatial tests cover page mismatch/no listener, wrapped shortest displacement,
  every camera facing, affine movement/rotation rebase, vertical scaling,
  constant-power pan, distance endpoints, teleports/page reset, close/far zoom,
  smooth render-band mute, exact map zero, advancing muted clocks, and UI
  independence.
- Loop tests cover idempotent start, SoundId conflict, update surface, full-file
  wrap, stop fade, ID reuse during old fade, cooldown/concurrency/global stealing,
  evicted-key reconciliation, and missing-ID warnings.
- Pause/load tests cover exact sample/oscillator/noise/envelope/filter retention,
  output ramps without state advance, continue voices, dropped new freeze
  triggers, internal-pause independence, map-mute independence, epoch reset,
  stale-command discard, and no save-state changes.
- Settings headless Lua tests cover tab order, all three sliders, live preview,
  rebuild preservation, Apply/Save/Back/Defaults, per-key lenient sparse config,
  local-file removal, curve endpoints, and safe persistence while disabled.
- A headless probe forces miniaudio null, triggers sample and synth sounds through
  the real worker/ring/callback, observes advancing callback/render counters, and
  asserts the backend is null. It opens no speakers.
- A manual-only graphical probe reports negotiated backend/rate/period and ring
  latency, plays reference sample/synth/spatial/loop cases, exercises recovery,
  and verifies clean shutdown. It is not a default CI gate.

Iteration uses the narrowest affected hspec describe or native test wrapper.
The whole headless suite and `make ci` remain CI/full-gate authority per
repository policy; audio work does not introduce worldgen or save-version
baseline changes.

## Delivery plan

### AUD-1. Establish the native audio core and device boundary

- **Outcome:** Vendored miniaudio and a C-only core can render silence offline,
  fill/read one SPSC ring through a C callback, and start/stop real or forced-null
  devices without Haskell callbacks or lifetime races.
- **Scope:** pinned source/license/checksums, Cabal C integration, opaque ABI,
  fixed mix/ring storage, offline render entrypoint, real/null context selection,
  callback zero-fill/counters, FFI layouts, idempotent lifetime.
- **Phase:** 1 — native substrate
- **Depends on:** `none`
- **Ordering:** critical path
- **Relevant decisions:** D-27 through D-30, D-40, D-41, D-49
- **Acceptance signals:** C/Haskell layout tests; offline silent frames; ring
  wrap/underrun tests; forced-null callback counters advance; callback is wholly
  C-owned; stop joins before memory free.
- **Out of scope:** catalogs, voices, decode, synth, engine capability, Lua/UI.
- **Open questions:** None

### AUD-2. Add the audio transport capability and worker lifecycle

- **Outcome:** Every boot mode explicitly owns an `ewAudio` slot; graphical,
  headless, and offscreen run a narrowed Haskell worker over real/null policy,
  while dump/preview run none, and audio failure degrades without engine exit.
- **Scope:** abstract transport/status, two-field ninth capability, EngineEnv
  construction, capability/persistence inventories, audit constant/self-tests,
  empty ceiling, projection test, worker/boot/shutdown wiring, partial-boot paths.
- **Phase:** 2 — engine ownership
- **Depends on:** AUD-1
- **Ordering:** critical path
- **Relevant decisions:** D-39 through D-41
- **Acceptance signals:** focused capability/audit self-tests; all boot modules
  compile under missing-fields; headless null advances status; dump/preview none;
  forced init failure leaves engine controllable; teardown order is producer then
  audio.
- **Out of scope:** catalog content, audible voices, Lua triggers, settings.
- **Open questions:** None

### AUD-3. Define audio runtime, player, and content configuration

- **Outcome:** Strict runtime tuning, lenient sparse player volumes, and
  independently validated sound-type/instrument/sound catalogs resolve to a
  deterministic dependency-isolated registry without playing audio yet.
- **Scope:** YAML types/parsers/resolution, exact schemas/ranges/path rules,
  default/override configs and gitignore/Cabal resources, runtime invariant
  fallback, sorted registration plan, repository-empty/minimal initial catalog.
- **Phase:** 2 — data contracts
- **Depends on:** `none`
- **Ordering:** can land first
- **Relevant decisions:** D-31 through D-33, D-45, D-47, D-49
- **Acceptance signals:** focused config/catalog hspec; invalid closure isolation;
  sparse write/removal; unknown-key and duplicate-ID rejection; deterministic
  resolved registry snapshots.
- **Out of scope:** native decode/registration, UI widgets, production SFX.
- **Open questions:** None

### AUD-4. Decode and mix resident sampled one-shots

- **Outcome:** Valid WAV/FLAC/MP3 definitions eagerly become shared resident
  sample handles and native one-shot voices mix through World/UI/Master offline
  and through the worker; invalid/oversized assets disable only dependents.
- **Scope:** bounded decoder, conversion/dedup/budgets, generated format fixtures,
  sample cursor/pitch, voice pool, bus accumulators/gain smoothing/master safety,
  per-sound/global allocation primitives and status counters.
- **Phase:** 3 — first audible source
- **Depends on:** AUD-1, AUD-3
- **Ordering:** critical path
- **Relevant decisions:** D-28, D-32, D-33, D-35, D-46, D-48, D-49
- **Acceptance signals:** all three format fixtures decode/play; PCM is shared;
  budgets abort early; concurrent sample mix is finite/bounded; deterministic
  drops/steals and retirement; valid siblings survive broken assets.
- **Out of scope:** synth, public Lua API, spatial camera, logical loops.
- **Open questions:** None

### AUD-5. Render minimalist synthesized voices

- **Outcome:** Resolved synth sounds render sine/saw/square/triangle/white-noise
  voices with ADSR and bypass/LP/HP/BP in the same pool and buses as samples.
- **Scope:** native oscillator/noise/envelope/filter state, gate/pitch/timbre gain,
  deterministic seeds/phases, denormal handling, instrument registration and
  sound-timbre overrides, sample+synth coexistence.
- **Phase:** 3 — second audible source
- **Depends on:** AUD-1, AUD-3, AUD-4
- **Ordering:** critical path
- **Relevant decisions:** D-31, D-34, D-35
- **Acceptance signals:** deterministic offline spectral/envelope/filter tests;
  all generator/filter modes; partial-level release; multiple mixed voices;
  finite protected output.
- **Out of scope:** modulation, multiple oscillators, Lua triggers, spatial/loop.
- **Open questions:** None

### AUD-6. Expose semantic one-shot triggers and audio status

- **Outcome:** Haskell and Lua callers can non-blockingly play valid sample or
  synth `SoundId`s, use only gain/pitch/position overrides, and inspect bounded
  status without paths or native handles.
- **Scope:** `Engine.Audio.API`, worker dispatch/catalog upload, command batching
  and results, `audio.play`/`audio.getStatus`, registration and shell sandbox,
  argument marshalling, unknown/queue/policy diagnostics.
- **Phase:** 4 — engine-facing API
- **Depends on:** AUD-2, AUD-3, AUD-4, AUD-5
- **Ordering:** critical path
- **Relevant decisions:** D-30, D-39, D-44
- **Acceptance signals:** headless Lua/Haskell triggers reach native frames;
  queue full never blocks; forbidden overrides are impossible/rejected; bad IDs
  warn rate-limited; status remains bounded and truthful.
- **Out of scope:** camera publication, loops, pause, settings widgets.
- **Open questions:** None

### AUD-7. Anchor world audio to the camera and zoom model

- **Outcome:** Page-scoped world one-shots remain anchored across camera motion,
  pan/attenuate predictably, hear farther/softer when zoomed out, and become
  smoothly silent on the zoom map without affecting UI or voice clocks.
- **Scope:** listener snapshots, wrap/facing conversion, native relative rebase,
  page/no-listener gates, spatial gains/pan/vertical scale, teleport/page reset,
  render-loop publication, zoom range/trim/fade.
- **Phase:** 5 — world integration
- **Depends on:** AUD-6
- **Ordering:** critical path
- **Relevant decisions:** D-36, D-37
- **Acceptance signals:** focused spatial/zoom hspec and offline mix assertions;
  all facings/wrap seams; anchored camera motion; exact map zero; natural later
  resume; UI invariant.
- **Out of scope:** occlusion/acoustics, loops, player pause.
- **Open questions:** None

### AUD-8. Add logical loops and deterministic voice policy

- **Outcome:** Haskell/Lua can start, update, and stop full-file WAV/FLAC or
  sustained synth loops by logical ID; concurrency/cooldown/priority behavior is
  deterministic and observable under pressure.
- **Scope:** logical/native loop maps, API verbs/results, idempotence/conflicts,
  moving position/gain, stop fade/ID reuse, MP3 prohibition, loop eviction
  reconciliation, final allocator/cooldown policy and counters.
- **Phase:** 5 — controllable sources
- **Depends on:** AUD-6, AUD-7
- **Ordering:** critical path
- **Relevant decisions:** D-38, D-44, D-48
- **Acceptance signals:** loop contract tests; moving spatial loop; synth sustain;
  sample wrap; stop fade; conflict/reuse; deterministic steals/drops; no native
  handle in public API.
- **Out of scope:** loop regions/crossfades, persistence, music transport.
- **Open questions:** None

### AUD-9. Integrate player pause and session replacement

- **Outcome:** Explicit player pause freezes/resumes selected World one-shots and
  loops exactly while UI/continue voices run, and load publication atomically
  retires the old audio session without affecting save data.
- **Scope:** accepted `setPausedFn` publication, native freeze/output ramps,
  paused-trigger drop, transport epochs, load-publish reset/stale discard, ring/
  loop/cooldown clear, internal-pause independence.
- **Phase:** 6 — lifecycle semantics
- **Depends on:** AUD-7, AUD-8
- **Ordering:** critical path
- **Relevant decisions:** D-42, D-43
- **Acceptance signals:** exact cursor/phase/noise/envelope/filter retention;
  continue/UI audible; new freeze drop; save/notification no freeze; load epoch
  clears old voices and cannot accept stale queued work; no save-version change.
- **Out of scope:** persisted ambience or loop restoration.
- **Open questions:** None

### AUD-10. Add the Settings Audio tab and player volume persistence

- **Outcome:** Master/World/UI 0-100 sliders live-preview and follow Apply, Save,
  Back, Defaults, resize, and sparse-local persistence contracts even when the
  audio device is disabled.
- **Scope:** config Lua verbs, separate settings data family, Audio tab/order,
  three sliders/value labels, snapshot/restore, curve conversion/status, Cabal
  config resources and gitignore.
- **Phase:** 6 — player control
- **Depends on:** AUD-2, AUD-3, AUD-6
- **Ordering:** not on the critical path
- **Relevant decisions:** D-45, D-47
- **Acceptance signals:** focused headless settings/config tests; immediate mix
  target changes; exact 0/100 and midpoint curve; Apply vs Save; Back/default;
  UI rebuild preservation; local file removal; safe disabled persistence.
- **Out of scope:** output-device selection, ambient/music slider.
- **Open questions:** None

### AUD-11. Harden device recovery and audio health diagnostics

- **Outcome:** Graphical audio falls back to continuously advancing null output
  and retries safely after init/device failure; operators can diagnose latency,
  backlog, voice, content, and mixer pressure without callback logging.
- **Scope:** device notifications/atomic flags, real↔null transition and ring
  reset, retry schedule, `CatAudio`/debug parsing, rolling thresholds, bounded
  rate limiting/status, graphical manual probe, null-only assertion.
- **Phase:** 7 — operational hardening
- **Depends on:** AUD-2, AUD-4, AUD-5, AUD-6
- **Ordering:** not on the critical path
- **Relevant decisions:** D-40, D-41, D-49
- **Acceptance signals:** injected failure/recovery tests; no stale burst; voices
  advance null; headless never probes physical; threshold degrade/recover;
  bounded diagnostics; manual probe documents negotiated facts and clean stop.
- **Out of scope:** player-selectable devices, engine shutdown on audio failure.
- **Open questions:** None

### AUD-12. Complete deterministic integration coverage and operator docs

- **Outcome:** One deterministic end-to-end null path proves both source families,
  buses, spatial/zoom, loops, pause, settings, and shutdown; maintainers have
  authoring, config, status, fixture-provenance, and manual-probe documentation.
- **Scope:** focused headless null probe and CI eligibility decision, cross-source
  integration specs, generated fixture script/provenance, content-author schema
  reference, runtime/status troubleshooting, tools/Cabal inventory updates.
- **Phase:** 8 — completion gate
- **Depends on:** AUD-4, AUD-5, AUD-7, AUD-8, AUD-9, AUD-10, AUD-11
- **Ordering:** critical path
- **Relevant decisions:** D-31 through D-49
- **Acceptance signals:** null backend is asserted; all major counters/behaviors
  advance in one run; no speaker access; docs match schemas/status; fixtures are
  reproducible and licensed; relevant focused tests/probe pass.
- **Out of scope:** production SFX library and any deferred codec/music feature.
- **Open questions:** None
