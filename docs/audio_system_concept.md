# Audio system concept

Synarchy needs a small, dependable audio foundation that can play authored
one-shot sounds and generate simple artificial tones without tying audio
correctness to the render loop or the simulation tick. This document captures
the product and architecture concept only. Component boundaries, one-PR
delivery slices, and tracker processing belong in the later
`docs/audio_system_design.md`.

During concept exploration this document is authoritative. Once the component
design is approved, the design document becomes the implementation source of
truth and this document remains the rationale and concept record.

Design state: `exploring`

Concept status: `settled; component design pending`

Status legend: `[ ]` unprocessed · `[#N]` linked to issue N · `[no-issue]`
reviewed and deliberately not tracked separately · `[deferred]` blocked on a
concrete precondition

## Processing status

- [ ] EPIC. Add Synarchy's sampled and synthesized audio foundation

No child slices are defined here. The separate component design will add the
dependency-ordered processing ledger after the concept is settled.

## Epic contract

- **Goal:** Synarchy can trigger small resident sampled sounds and minimalist
  synthesized sounds through a responsive, spatially aware audio runtime whose
  real-time path is isolated from ordinary Haskell scheduling and allocation.
- **Done when:** a graphical run can load and play each selected sample format,
  trigger a supported synthesized sound, control a logical loop, spatialize a
  world sound relative to the camera, play a non-spatial sound, and mix
  concurrent voices without blocking gameplay threads; headless/offscreen
  exercise the same runtime through null output; malformed content disables
  only dependent sounds; Master/World/UI controls follow the Settings
  lifecycle; health signals expose failures and pressure; and shutdown or an
  unavailable device degrades safely to silence.
- **Users and operators:** players hear game and UI feedback; content authors
  select or define sounds; engine maintainers own the native/Haskell boundary
  and diagnose audio health.
- **Arc label:** None proposed

## Current state and evidence

Synarchy has no audio subsystem today: there are no production audio modules,
audio dependencies, audio configuration, or sound assets under `src/`,
`synarchy.cabal`, `config/`, or `assets/`. The current worker registry contains
combat, simulation, unit, world, input, and Lua workers but no audio worker
(`src/Engine/Core/Workers.hs`). Runtime resources already resolve under one
validated resource root containing `scripts/`, `assets/`, `data/`, and
`config/`, so a future `assets/audio/` family fits beneath the existing root
without inventing a second path model (`app/App/ResourceRoot.hs`).

`EngineEnv` is a shared cross-thread record with a closed capability inventory.
Its post-flip procedure says subsystem-owned runtime state should remain with
the subsystem; genuinely shared state that fits none of the eight existing
capabilities requires explicit approval for a ninth capability
(`docs/engineenv_capability_inventory.md` section 6.4). Audio's renderer,
voices, decoded buffers, native device, and DSP state therefore must not be
added as loose `EngineEnv` fields. Only a deliberately small command/status
transport may need a shared audio capability.

Pause and zoom already have authoritative engine boundaries that audio can
observe without inventing parallel UI state. `enginePausedRef` is the engine's
pause source of truth, while `scripts/pause.lua` distinguishes ordinary player
pause transitions from engine-imposed save/load and notification pauses. The
world renderer fades between detailed tiles and the zoom map over
`World.Grid.zoomFadeStart` 1.2 through `zoomFadeEnd` 1.6; the HUD classifies
those bands as `zoomed_in`, transitional `none`, and `zoomed_out`
(`src/World/Render.hs`, `src/World/Render/Zoom/Quads.hs`, and
`scripts/hud.lua`). Audio can reuse the camera zoom and these boundaries, but
must still smooth native gain changes to avoid discontinuities.

The local reference project `~/work/idou` demonstrates the architectural seed:

- `src/Audio/Thread.hs` has one Haskell audio owner, command/event queues,
  target-fill rendering, health counters, and a miniaudio playback device.
- `src/Audio/Thread/Render.hs` converges synthesized voices and decoded clips
  into one stereo PCM mix before writing a single-producer/single-consumer ring.
- `cbits/miniaudio_rb_helpers.c` wraps miniaudio's lock-free ring buffer.
- `cbits/miniaudio_decode_helpers.c` calls the format-agnostic
  `ma_decoder_init_file` and normalizes decoded assets to floating-point PCM;
  despite WAV-named tests, Idou's useful loading boundary is not WAV-specific.
- `src/Audio/Oscillator.hs` and `src/Audio/Envelope.hs` implement band-limited
  basic waveforms, seeded noise, and ADSR envelopes; the larger patch graph,
  filters, modulation matrix, MIDI, and song runtime show where a deliberately
  minimal Synarchy synth must stop.

The reference also exposes boundaries Synarchy should improve. Its device
callback enters Haskell through a wrapper, its mixer performs per-sample DSP in
Haskell, and its loop drains an unbounded command queue completely before
rendering. Those choices were useful for understanding the system, but they are
not the selected production boundary for Synarchy.

As of 2026-08-12, upstream miniaudio 0.11.25 provides the relevant low-level
device, decoding, and lock-free SPSC PCM-ring primitives, plus native null
output. It is designed to be compiled directly into the application. The local
Idou copy is 0.11.24. Upstream is preparing a future 0.12 split implementation,
so a new integration should compile the supplied `miniaudio.c` and include
`miniaudio.h`, rather than establish a new dependency on the legacy
single-header implementation pattern.

The current upstream decoder API has built-in WAV, FLAC, and MP3 support and a
custom-decoder vtable for other codecs; the official examples demonstrate
Vorbis and Opus adapters. AAC is not a built-in format, so supporting AAC would
mean selecting, integrating, licensing, and testing another decoder rather than
merely allowing a file extension
([miniaudio decoding manual](https://miniaud.io/docs/manual/index.html#Decoding),
[custom-decoder example](https://miniaud.io/docs/examples/custom_decoder.html)).

No open or closed GitHub issue with an audio, SFX, music, or miniaudio premise
overlaps this arc. Broad searches for the word "sound" return unrelated prose
matches, not an audio-system tracker.

## Desired experience

Gameplay and Lua code identify a sound semantically and enqueue a request; they
never touch a device, PCM buffer, native voice, or DSP object. Enqueueing does
not block on decoding or playback.

A sound request can produce either:

- a short authored sample in a supported file format, decoded and held in
  memory; or
- a minimalist synthesized voice suitable for beeps, pulses, alarms, impacts,
  noise bursts, and other artificial feedback.

World sounds name their source world page and position. Sounds from the active
page are placed relative to the camera listener; sounds from inactive pages are
inaudible. UI and other explicitly non-spatial sounds bypass world positioning.
Multiple sample and synth voices can coexist in one output mix.

One-shots use fire-and-forget `SoundId` triggers. Controllable WAV/FLAC loops
use a Haskell-owned logical `LoopId`: callers can start a named sound under that
identity, update its position and instance gain, and stop it through the
sound-definition's authored fade. Starting an already-active ID updates the
existing loop rather than creating an immortal duplicate. Neither one-shots nor
loops expose native voice handles.

An explicit player pause freezes ordinary world-event voices and their sample,
oscillator, envelope, and filter state; resume continues from that retained
state rather than restarting the sound. UI remains audible. Pause behavior is a
sound-type policy rather than an unavoidable consequence of selecting the World
bus, so future ambience can be declared pause-continuing without adding an
Ambience bus to V1. Engine-internal save/load pauses do not freeze audio. New
freeze-policy triggers received during player pause are dropped rather than
queued for a stale burst on resume.

Within the detailed world view, zoom is part of the listener model: close zoom
is slightly louder with a narrower audible radius, while zooming out gently
reduces World gain and expands the audible radius. Across the existing zoom-map
fade band the World bus falls sharply but smoothly to silence, reaching exactly
zero at the fully zoomed-out map boundary. The zoom map never presents world
sounds, regardless of how wide the distance calculation would otherwise be.
The close/far gain trims, distance multipliers, and smoothing time are tracked
runtime tuning; the final zero boundary comes from the renderer's canonical
`zoomFadeEnd` rather than a separately configurable duplicate.

World voices keep advancing while the zoom map mutes them. If the player
returns before a voice finishes, its gain rises smoothly from its current
playback point; a one-shot that finished while inaudible does not reappear, and
a loop returns at the position it would naturally have reached.

Players control `Master`, `World`, and `UI` volume with three sliders in a new
Audio tab in Settings. `World` and `UI` are the two source-routing buses;
`Master` is their parent and controls the completed mix. The settings must join
the menu's existing pending/apply/save/back/default lifecycle rather than form a
separate write-through UI. Dragging a slider immediately previews its pending
value; Back and Defaults remain audible operations because they restore or load
values into that same live preview.

Player-facing volume is always an integer 0-100: 0 is hard mute and 100 maps to
0 dB (unity). Values between them follow the configured perceptual curve.
Programmer- and author-facing gains are signed decibels, use explicit `_db`
field names, and may attenuate or boost. Native mixing converts the resolved dB
values to linear amplitude; positive authored gain remains subject to the
master protection stage.

Audio output is optional infrastructure, not a condition for the game to run.
An unavailable device produces a clear diagnostic and a queryable degraded
state, while gameplay continues silently. Normal shutdown stops producers and
the audio owner before freeing native buffers or device state.

Graphical mode uses a real output device. Headless and offscreen modes run the
same worker, mixer, ring, callback, and status path against miniaudio's null
destination, allowing functional audio tests without touching the user's
speakers. Dump and preview start no audio runtime; a future audio-preview
feature may deliberately change preview policy. Deterministic DSP tests may
still render buffers directly without starting even a null device.

One bad sound never disables a valid sound or crashes the game. Startup builds
the valid dependency closure: a missing/invalid sample disables only sounds
that reference it; a bad type or instrument disables only dependent sounds.
Each rejection sends a warning carrying the definition ID, source path, and
cause to the engine logger/status surface. Later attempts to trigger a rejected
or unknown ID are silent no-ops with counters and rate-limited diagnostics,
rather than a warning flood. Correct definitions continue to play normally.

## Scope

### In scope

- One Haskell audio-control worker and a small Haskell-facing command/status
  API.
- A native C audio core called in coarse render/control operations across the
  FFI, not once per sample.
- A fully native miniaudio device callback.
- A native SPSC stereo PCM output ring between the producer/mixer and device
  callback.
- Loading small supported sample files completely into resident, normalized PCM
  buffers.
- Built-in decoding of WAV and FLAC one-shots/loops plus non-looping MP3
  one-shots, all normalized through the same resident-sample path.
- Concurrent one-shot and looping sample voices, subject to explicit budgets.
- Logical loop start/update/stop control for WAV/FLAC sources, with authored
  stop fades and no gameplay-visible native handles.
- A minimalist synthesized-source family with sine, band-limited saw, square,
  triangle, deterministic white noise, ADSR, and one optional low-pass,
  high-pass, or band-pass filter.
- Mixing sample and synth sources into the same output.
- Basic world spatialization relative to the camera plus an explicitly
  non-spatial path.
- A fixed `Master`/`World`/`UI` bus model, output protection, and health
  telemetry.
- A new Audio settings tab with Master, World, and UI volume sliders.
- Tracked, validated YAML registries for sound types, synth instruments, and
  concrete sounds, plus a separate layered player-volume configuration.
- Safe startup, degraded-silent operation, shutdown, and an offline/null test
  path.
- Real output in graphical mode, a speaker-safe null destination in headless
  and offscreen modes, and no audio runtime in dump or preview.
- Logical sound identities and validation rather than gameplay-owned asset
  paths or native handles.

### Out of scope

- Idou's song, timeline, tempo, bar, arrangement, or adaptive-music interfaces.
- MIDI file playback, MIDI controllers, musical-performance instruments, and
  patch graphs. The data concept named `SynthInstrument` is only a reusable
  synth timbre, not a MIDI or performance system.
- AAC, M4A/MP4, Vorbis, Opus, and other custom-decoder formats. They remain
  compatible with the generic source boundary but require a later codec and
  dependency decision.
- Long-form music playback or streamed audio assets.
- A dedicated ambience scheduler, ambience content library, or separate
  Ambience bus. The pause policy is intentionally compatible with that future
  work, but does not pull it into V1.
- General-purpose modular synthesis, arbitrary node graphs, modulation
  matrices, LFO routing, multi-layer patches, or live patch editing.
- Capture, microphones, voice chat, recording, or multiple output devices.
- HRTF, surround output, Doppler, propagation simulation, occlusion, room
  acoustics, and reverb.
- Persisting live voices, playback cursors, ring contents, or native audio
  objects in saves.
- Selecting or producing the game's full sound-effects library. Infrastructure
  can be verified with repository-owned test fixtures before production sound
  content exists.

## Conceptual architecture

```text
Lua / world / unit / combat / UI producers
                    |
             semantic commands
                    v
          shared Haskell command queue
                    |
                    v
       Audio.Thread (sole control owner)
       - resolves logical sound definitions
       - gates active world pages
       - publishes camera-listener changes
       - batches native control operations
       - reads native health/status
                    |
              coarse-grained FFI
                    v
            native C audio core
       - resident decoded sample assets and cursors
       - synth and envelope state
       - voice allocation and budgets
       - spatial gain/pan and bus accumulation
       - master protection and PCM rendering
                    |
             SPSC stereo PCM ring
                    v
       native miniaudio device callback
                    |
                 device
```

### Ownership and concurrency

The Haskell audio worker is the only ordinary caller allowed to mutate the
native audio core. Gameplay producers share only a multi-producer command
transport. The native device callback is the sole consumer of the PCM ring and
does not inspect voice, asset, catalog, listener, or Haskell state.

The callback's complete job is to acquire PCM frames, copy or commit them to the
device buffer, fill any shortage with silence, and update native atomic health
counters. It never calls Haskell, allocates, logs, decodes, acquires a general
lock, changes device lifecycle, or runs gameplay/DSP decisions.

The producer renders ahead to a target fill rather than to ring capacity. A
command flood must not postpone replenishment indefinitely: the design must
bound command work per render cycle or otherwise make buffer service the
higher-priority obligation. Cross-FFI rendering occurs by chunk so no
per-sample FFI overhead exists.

Because the ring contains the completed interleaved mix, a control change
cannot remove World samples already queued without also discarding queued UI
samples. Pause, zoom, and live-volume changes therefore take audible effect
after at most the configured target-fill latency, with native gain ramps used
where appropriate. On player pause the core stops advancing freeze-policy
voices at the next render boundary; the already-rendered tail drains, and
resume continues exactly from the retained cursor/oscillator/envelope/filter
state. The component design must budget this latency rather than add bus rings
or rewind already-rendered audio merely to claim instantaneous pause.

### Native/Haskell boundary

Production mixer and DSP execution lives in C. Haskell owns the engine-facing
types, logical identities, command routing, lifecycle orchestration, catalog
interpretation, page gating, listener publication, status presentation, and
tests of those policies. Pure Haskell reference functions remain acceptable
when they materially improve validation, but the output device never depends on
per-sample Haskell execution.

The preferred starting proposal is a small Synarchy-owned C core using
miniaudio's low-level device, decoder, conversion, and ring primitives. Adopting
miniaudio's entire high-level engine or node graph is not assumed by this
concept; the component design must compare that option against keeping
Synarchy's fixed mixer policy explicit.

### Source definitions

Both source families share one logical `SoundId` namespace so callers do not
care whether a sound is sampled or synthesized. Sound types, synth instruments,
and concrete sounds are all authored data rather than Haskell constructors,
Lua tables, or native constants.

The selected authoring/configuration layout is:

```text
data/audio/sound_types.yaml   reusable routing and playback-policy defaults
data/audio/instruments.yaml   reusable synth generator, ADSR, and filter timbres
data/audio/sounds.yaml        concrete SoundIds and their sample/synth sources
assets/audio/**               resident assets in the selected sample formats

config/audio_runtime.yaml     tracked engine/mixer tuning, not player preference
config/audio_default.yaml     tracked player-volume defaults
config/audio.local.yaml       sparse, gitignored player-volume overrides
```

This preserves Synarchy's existing resource split: game content lives under
`data/` and `assets/`; developer/runtime tuning can use a tracked standalone
config like `config/pathing.yaml`; and Settings-owned player state follows the
existing tracked-default plus sparse-local-override convention used by save and
video settings. Keeping runtime tuning out of `audio.local.yaml` prevents a
player's saved volumes from silently pinning old voice, ring, or asset budgets
after the shipped defaults change.

The three authored concepts have deliberately different jobs:

- A `SoundType` supplies reusable policy defaults such as `world` versus `ui`
  routing, spatial mode and attenuation, base gain in decibels, priority,
  per-sound concurrency, overflow/voice-stealing policy, cooldown, loop policy,
  and player-pause behavior (`freeze` or `continue`).
- A `SynthInstrument` supplies a reusable artificial timbre: generator kind,
  generator defaults, ADSR values, optional filter mode/cutoff/resonance, and
  timbre gain in decibels. It contains no MIDI mapping, sequencer, or song
  behavior.
- A `SoundDefinition` is the concrete ID gameplay triggers. It names a
  `SoundType` and selects exactly one source family: a supported sample asset,
  or a `SynthInstrument` plus the pitch/gate values needed for that sound.

An illustrative shape follows. The concept names, responsibilities, and
precedence are settled; exact key spelling and parser representation remain for
the component design.

```yaml
sound_types:
  - id: world_effect
    bus: world
    spatial: { mode: world, min_distance_tiles: 1, max_distance_tiles: 40,
               rolloff: linear, vertical_scale: 1 }
    playback: { gain_db: 0, priority: 50, max_instances: 8,
                overflow: steal_oldest, cooldown_ms: 0, loop: false,
                player_pause: freeze }

instruments:
  - id: short_square
    generator: { kind: square, frequency_hz: 440, phase: reset }
    envelope: { attack_ms: 2, decay_ms: 20, sustain_level: 0.5,
                release_ms: 35 }
    filter: { type: high_pass, cutoff_hz: 180, resonance_q: 0.707 }
    gain_db: -4.5

sounds:
  - id: ui_confirm
    type: ui_effect
    source:
      synth: { instrument: short_square, frequency_hz: 880, gate_ms: 45 }

  - id: pick_impact
    type: world_effect
    source:
      sample: { path: assets/audio/tools/pick_impact.wav }
```

The catalog-level source sum is therefore:

```text
SoundDefinition =
  { SoundId, SoundTypeId,
    Source = Sample { supported encoded asset }
           | Synth  { SynthInstrumentId, pitch, gate },
    optional authored overrides }
```

Precedence is shipped engine fallbacks, then `SoundType` defaults and (for
synths) `SynthInstrument` defaults, then concrete-sound overrides, then a
closed trigger-time override set. A concrete sound may override any authorable
field from its type or instrument. The schema keeps those overrides in explicit
nested policy/source blocks so same-named values do not have ambiguous
ownership. A one-shot trigger contains `SoundId`, page/position when spatial,
optional signed `gain_db`, and optional `pitch_semitones`. A loop command adds
its logical `LoopId`; an update may change position and instance `gain_db`, and
stop uses the definition's authored fade. Callers cannot override routing,
spatial curves, ADSR, filter, loop policy, priority, concurrency, pause policy,
asset, or instrument identity. This makes the selected surface fully
data-driven without moving sound design back into gameplay code.

V1 resolves each `SoundId` to exactly one source. The source is either one
supported encoded sample asset or one synth instrument plus its concrete
pitch/gate settings. Weighted sample choices, random pitch/gain ranges, and
other variant-set forms are deferred; adding one later does not change gameplay
callers because they still trigger the same semantic `SoundId`.

Native code receives compact validated definitions and numeric handles; it
does not parse Synarchy YAML or know logical path conventions. Every reference,
enum, range, source union, and asset must be validated before the audio worker
starts. Unknown fields should be rejected so a misspelled sound-design setting
cannot silently use a fallback.

Every unique sample asset is decoded outside the device callback during audio
startup, converted once to the core's fixed floating-point mix format, and
retained for reuse by every sound that references it. Resident budgets are
measured against decoded frames/bytes, not compressed file size: a tiny MP3 can
still expand into an impermissibly long buffer. "Small" needs enforced
per-asset duration and aggregate decoded-memory budgets in the component design
so accidentally placing a long track on the resident path disables that sound
with a clear warning rather than consuming unbounded memory.

### Minimal synth direction

The minimal synth is one oscillator or noise generator feeding one optional
filter, an amplitude envelope, gain, and the same spatial/bus path as a sample
voice. Its v1 surface is:

- sine, band-limited saw, square, and triangle waveforms;
- deterministic white noise;
- frequency in hertz, start-phase policy, gain, and a finite gate/lifetime;
- one ADSR amplitude envelope;
- one bypassable filter selectable as low-pass, high-pass, or band-pass, with
  cutoff and resonance; and
- multiple independent synth voices under the shared voice budget.

The selected generator, all ADSR values, pitch/gate, filter mode, cutoff,
resonance, and gains are authored data. Pink or colored noise, pitch glide,
multi-oscillator layering, filter-envelope modulation, LFOs, and parameter
automation are deferred. MIDI note identity and performance-instrument
semantics are out of scope even if a convenience function later converts a
note number to hertz.

### Spatial model

The camera is the listener for world sounds. Haskell owns the conversion from a
page-scoped Synarchy world coordinate into a listener-local position, including
camera facing and world topology; the C core receives a simple relative vector
and does not know `WorldPageId`, isometric projection, wrapping rules, or engine
camera records.

The first spatializer is speaker panning plus distance and vertical attenuation,
not physical acoustics. Rotation with camera facing must keep a sound's apparent
screen direction stable. Camera zoom scales both the listener's audible radius
and a modest World gain trim while the detailed view remains visible. The
existing render fade band then drives the World bus sharply but click-safely to
zero at `zoomFadeEnd`; the fully zoomed-out map is world-silent. A request
marked non-spatial plays independently of camera position unless its routing
policy still selects the World bus.

### Time and lifecycle proposals

The audio core maintains a monotonic output-frame clock for rendering, voice
lifetime, diagnostics, and future scheduling. This is not a song transport and
does not expose tempo or musical units.

Audio normally remains wall-clock based rather than running at world time
scale: accelerating simulation must not pitch or accelerate samples and synth
voices. An explicit player pause is the exception: voices whose sound-type
policy is `freeze` stop advancing and resume from the same point, while UI and
future pause-continuing ambience remain on the output-frame clock. A save/load
or other engine-internal pause does not alter audio clocks. High-rate gameplay
producers need coalescing, cooldown, priority, and voice-budget policies so
accelerated simulation cannot turn every internal event into an unbounded
audible event. A new freeze-policy trigger received during player pause is
dropped and counted, while `continue` triggers remain admissible.

The runtime is rebuilt, not persisted, across process boot and save-load
publication. After a load, any continuing ambience or loops must be reissued
from current gameplay state; stale pre-load voices must not refer to discarded
world pages.

The engine integration is an explicitly maintainer-approved ninth narrow
`audio-transport` capability. Its `AudioCapability` contains only the
multi-producer command queue and shared status snapshot, plus a named audio
worker slot in `EngineWorkers`. The native runtime itself remains private to
`Audio.Thread`. Adding the capability must follow every synchronized inventory,
audit, self-test, module, and projection-test change required by
`docs/engineenv_capability_inventory.md` section 6.4(c); this approval does not
authorize loose audio fields or a new full-`EngineEnv` importer.

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

## Verification strategy

Verification should separate deterministic rendering from physical-device
behavior:

- Native unit/offline tests render known sample and synth definitions into
  buffers and check duration, envelope behavior, frequency, channel balance,
  determinism, finiteness, peak bounds, and voice retirement.
- Haskell tests exercise logical-ID validation, command ordering, page gating,
  listener conversion, voice/concurrency policy, degraded status, and shutdown
  without opening a physical device.
- Trigger-contract tests prove only position, signed instance gain, and pitch
  may vary at runtime; attempts to smuggle catalog policy or native handles
  across the API are structurally impossible or rejected.
- Catalog tests reject duplicate or unresolved type/instrument/sound IDs,
  malformed source unions, unknown fields, invalid enum/range combinations, and
  sample assets that violate the resident-audio budget. They also prove invalid
  dependency closures are isolated, every valid sibling remains playable, and
  warnings/status identify each rejected ID and cause. Precedence tests prove
  exactly which type, instrument, sound, and trigger values win.
- Decoder fixtures cover every selected encoded format, input sample rates,
  mono/stereo conversion, eager decode, shared-path deduplication, and budgets
  measured after conversion to resident PCM.
- Ring tests stress wraparound, partial availability, underrun zero-fill,
  producer/consumer ownership, and cleanup order.
- Mix tests prove sample and synth voices coexist and that spatial/non-spatial
  sources route through the intended buses. Filter tests cover bypass,
  low-pass, high-pass, and band-pass responses without relying on a device.
- Pause tests prove an explicit player pause freezes only `freeze` voices,
  pause-continuing UI/future ambience advances, engine-internal pauses leave
  audio clocks alone, resume continues from the exact retained sample/synth
  state, new freeze-policy triggers are dropped, and audible response stays
  within the configured target-fill latency.
- Zoom tests exercise close/far radius and gain scaling, smooth traversal of
  the render fade band, exact World silence at and beyond `zoomFadeEnd`, and
  complete independence of the UI bus. They also prove voice clocks keep
  advancing while muted and surviving voices fade back naturally on zoom-in.
- Settings tests cover all three 0-100 sliders, pending/apply/save/back/default
  behavior, sparse `audio.local.yaml` persistence, and the engine's normalized
  gain values, including live preview and restoration on Back. Gain tests cover
  0 as hard mute, 100 as 0 dB, the configured perceptual curve, signed authored
  dB values, and finite linear-amplitude conversion.
- Loop tests prove logical-ID start/update/stop behavior, idempotent repeated
  starts, moving position/gain updates, authored stop fades, pause retention,
  zoom-map clock advancement, voice retirement, and rejection of looping MP3.
- Capability tests and repository audits prove the new `audio-transport`
  projection aliases only the shared command/status containers, while native
  runtime state remains absent from `EngineEnv` and full-access imports do not
  grow.
- Invalid, oversized, unsupported, and missing sample assets are disabled with
  explicit warnings outside the callback; unrelated sample and synth sounds
  remain playable.
- A focused manual probe boots a real device, reports negotiated format and
  latency/health counters, triggers reference sounds, and verifies clean device
  shutdown. It is not a default headless-suite gate.
- A headless probe selects the null backend, exercises sample and synth playback
  through the real worker/ring/callback path, observes advancing counters, and
  verifies that no physical output backend was opened. Offscreen UI coverage
  uses the same null policy.
- The design must define acceptable underrun, render-time, ring-fill, command
  backlog, active-voice, stolen/dropped-voice, and device-failure telemetry
  before implementation is declared complete.

## Component-design handoff

`docs/audio_system_design.md` will take the settled decisions and proposals from
this concept, define the exact C ABI and ownership boundaries, choose the native
miniaudio surface, specify Haskell types and capability integration, settle
content/config formats, and produce one-PR-sized dependency-ordered delivery
slices. No concept-level product question remains open. The component design
still needs to decide and specify:

- the exact C ABI and whether the fixed native core uses only miniaudio's
  low-level primitives or selected higher-level helpers;
- mix format, sample rate, render chunk, target-fill/ring capacity, latency
  budget, device negotiation, null selection, and retry behavior;
- oscillator/filter algorithms, coefficient updates, denormal handling, gain
  smoothing, mix accumulation, and master protection;
- exact YAML keys, defaults, units, validation ranges, dependency-closure
  diagnostics, decoded duration/memory budgets, and the 0-100 perceptual curve;
- voice allocation, priority/concurrency/stealing semantics, logical `LoopId`
  lifecycle, command batching/backpressure, and health thresholds;
- concrete Haskell/Lua APIs, the approved capability/inventory integration,
  worker startup/shutdown ordering, settings persistence, and save-load reset;
  and
- deterministic fixtures, focused tests/probes, documentation, and
  dependency-ordered one-PR delivery slices.

Until that document is created, the processing ledger intentionally contains
only the epic.
