# Foreground audio implementation

This is the local working record for foreground session
`20260910T142328Z-audio-unit-selection-cue-8bd04e`, on branch
`foreground/bear-selection-audio`, based on
`5b677136fdacb75a604da796e066c50710c128b7`. No tracker artifact or PR has been
created at the start of this record. On 2026-09-11 the owner chose the completed
audio foundation and preview player as the first standalone PR boundary; bear
sounds and selection responses follow separately (selection design D-6).

The [foundation design](audio_system_design.md) and
[selection pilot design](unit_selection_audio_design.md) own the contracts.
This record does not disposition their issue-processing checklists.

## Implemented locally

- Pinned miniaudio 0.11.25, exact upstream files, license and SHA-256 provenance.
- C-owned stopped/real/null device lifecycle and SPSC ring. The callback only
  copies ring PCM, zero-fills shortages and updates lock-free counters.
- Fixed-width, versioned catalog/command/status ABI with compiler size checks
  and Haskell layout/round-trip tests.
- Bounded eager WAV/FLAC/MP3 decoding, stereo/rate conversion, sharing,
  startup-only catalog upload, symlink containment and aggregate budgets.
- Sample interpolation and full-buffer loops; deterministic native generators,
  ADSR, fixed RBJ filters, separate World/UI accumulation, gain smoothing and
  Master soft-knee protection.
- Native cooldown, per-sound/global voice policy, loop identity/eviction,
  spatial pan/attenuation/rebase, zoom gain, exact source-state freeze/resume,
  and joined-callback session/ring reset.
- Wrapped voices choose the nearest periodic image after listener movement,
  including camera motion across a source's antipode; a PCM test proves the
  corresponding left/right channel change without restarting the source.
- Strict runtime tuning, lenient sparse player-volume persistence through the
  durable config writer, strict authored definitions with dependency isolation.
- Authored menu_selected/menu_back synth definitions using the approved menu
  reference parameters, plus scoped forward/back menu activation hooks.
- Ordered bounded transport with retained controls and stamped latest listener/volume slots;
  loop-update coalescing preserves start/stop barriers and epochs reject stale work.
- Worker-local runtime reconciles native loop rejection and eviction before later controls.
- Ninth AudioCapability and both shared fields are inventoried; the capability audit
  passes with 93 fields, 15 records and no new full-access importer. Its 237 self-test
  groups pass (539 assertions).
- Optional audio worker and real/null/none boot wiring, callback-safe shutdown and
  partial-boot cleanup; semantic Lua API, shell namespace, camera publication, accepted
  player pause, and load/exit session resets.
- Audio Settings tab with three live sliders and separate current/pending/saved state,
  including resize, Apply/Save/Back/Defaults and failed-save behavior.
- Failure injection covers device init/start fallback, null-only recovery, retry
  backoff and session reset while both output attempts fail.
- Bounded health telemetry and per-reason/ID diagnostics, monotonic status snapshots,
  rolling underrun buckets, actual rendered-frame service budgets, queue/loop peaks,
  distinct per-sound/global drops and steals, and catalog admission counts.
- Mathematical decoder fixtures and their reproducible generator. These are
  test signals, not proposed bear sound assets.

## Evidence and corrective experiments

`python3 tools/test_audio_native.py --build-dir /tmp/synarchy-audio-native-foreground --sanitize`
passes generator/pitch/band-limiting, ADSR, filter response, noise independence,
pause retention, limiter/mute, loop/capacity/eviction, spatial/rebase/zoom,
all three sample formats, decode limits, stereo conversion, SPSC wrap and null
callback lifecycle checks under address/undefined-behavior sanitizers.

Production builds and targeted `Audio.` Hspec tests have passed. The first
cross-language menu render exposed stale C objects from an incremental build:
`syn_audio_device.o` and `syn_audio_ring.o` still used the previous private
struct layout, corrupting a new mix-buffer pointer. The crash was reproducible
with `--match 'renders the shipped menu synth'`; the macOS report points to
`syn_audio_mix` attempting to zero address `0x100000001`.

Rebuilding only those four static/dynamic objects removed the crash and yielded
20 passing Hspec examples (`/tmp/synarchy-audio-20260910T084055.log`). Cabal
3.16's [C-source freshness check](https://github.com/haskell/cabal/blob/Cabal-v3.16.1.0/Cabal/src/Distribution/Simple/GHC/Build/Utils.hs)
compares the C source and object timestamps. `BuildSupport.AudioDependencies`
now invalidates the affected native objects and generated Haskell bindings when
their shared headers change. The real hook's isolated timeline tests pass via
`python3 tools/test_audio_build_dependencies.py`; a subsequent private-header
edit also rebuilt the affected objects through ordinary Cabal.

The latest focused audio run passes **63 examples** with no failures, including the
runtime, optional worker, capability projection, Lua boundary, Settings and health
checks (`/tmp/synarchy-audio-20260910T100241.log`). It also validates the authoring
guide's actual YAML examples, pause admission and load/exit reset integration.
The native sanitizer suite passes decoder/DSP/policy/ring tests, injected device
and allocation failures, zero envelope stages, independent deterministic random
phase and denormal cleanup.

An additional regression run found a stale frozen log-category transcription:
the new CatAudio constructor needed its explicit ENGINE_LOG_AUDIO entry. After
that correction, the menu-responsive, pause, logging and namespace selection
passes **203 examples**, no failures (`/tmp/synarchy-audio-20260910T101229.log`).
The preceding run's worker lifecycle and shutdown cases also passed.

The canonical concurrent probe run
`python3 tools/run_probes.py --only audio_null,debug_console_boot --exact --jobs 2 --retries 0`
passes both probes (7.8 s wall time). The nine-check null probe exercises actual
sample/synth playback through the worker/ring/callback, listener publication,
pause/resume, volume persistence, diagnostics, whole-session reset and shutdown.
The console probe proves partial-boot cleanup now joins the audio worker too.

`python3 tools/audio_manual_probe.py --offscreen-check --port 9188` passes four
checks: negotiated forced-null output, rendered Settings → Audio through real
input, sample/synth/spatial/loop/pause playback and clean exit. The actual PNG
was inspected: all three labels/sliders and action buttons are visible and
aligned at 1280×720. Retained evidence: `/tmp/synarchy-audio-settings-9188.png`,
`/tmp/synarchy-audio-manual-9188.json`, `/tmp/synarchy-audio-manual-9188.log`.
That run recorded **seven underruns before the playback sequence, zero during
it**. Startup underruns remain a measured limitation for physical acceptance;
this is not a claim of interruption-free physical output. The explicit
`--interactive` mode retains owner listening and optional device-change verdicts
separately; it has not been launched.

The high-scale Settings test caught a half-pixel slider-knob overflow caused by
rounding the knob and cap in opposite directions. Rounding the cap up and knob
down keeps every part of the slider inside its track; all four Settings tests pass.
Intentional device teardown now clears its stopped notification after callback
join, preventing that notification from bypassing a failed recovery's backoff.

The macOS hsc2hs helper initially inherited an unused Vulkan dylib dependency.
Darwin's `hsc2hs-options: --lflag=-Wl,-dead_strip_dylibs` removes that unused
dependency while preserving ordinary Unicode preprocessing. Cross-compiling
hsc2hs via assembly was rejected because it corrupted Unicode source text.

Config-write audit plus its self-tests pass with six classified writers.
Source-distribution audit self-tests pass 66 assertions including native/build/
fixture/documentation families. Capability and persistence inventories, both
module-budget guards, Unicode operators, enum wire order and CI parity pass.
The probe registry's persistence-sweep self-test passes 85 assertions.
Full source-distribution validation passes for the staged foundation: 8,058
tracked entries and all 6,677 required resources across 32 families, with legacy
and local config exclusions preserved. Final production `cabal build all
synarchy-test-headless` passes (`/tmp/synarchy-audio-20260910T102104.log`).
The manual probe's missing/contradictory mode and non-terminal audible-mode
checks all refuse before the boot function can run. The primary checkout is clean.

## Remaining work

- Retain the final content and owner-acceptance evidence before publication.
- Physical-device listening/recovery acceptance and evaluation of observed startup
  underruns. No visible launch or physical output has occurred in this session.
- After preview-player acceptance, try engine-synth bear variants; owner-authored
  WAV files are the chosen fallback. Exact selection-response semantics, the
  response-set extension, actual selection integration and listening acceptance
  remain. No production bear audio has been sourced or generated yet.

All physical-device listening and visible game acceptance remain unperformed.
Headless/native checks exercise null output only. No complete CI suite has run.

## Preview-first amendment and implementation

On 2026-09-10 the owner moved preview audio ahead of bear sound development.
The player now supports `--preview audio`, `audio/synth`, `audio/files` and an
explicit local WAV/FLAC/MP3 path. Every visual preview has a bottom-left Audio
control. Synth uses the actual two shipped engine cues; Files uses the same
decoder/mixer. Play replaces the prior audition; Stop clears it; Reload rebuilds
content in place after joining/freeing the old native core. Live Master/UI
volume controls do not write settings. The earlier no-audio-in-preview design
is explicitly amended in the design/runtime/engine contracts.

The first complete audio run passed 74 examples, including exact first-play PCM
parity with both in-game cues, external Unicode/space paths, corrupt-file
isolation, edited-file reload without PCM accumulation, retained Reload controls,
preview-only Lua guards, and the real shipped pane's UI logic.
`/tmp/synarchy-audio-20260910T170904.log`. Existing preview regression tests passed
203 examples: `/tmp/synarchy-audio-20260910T171358.log`.

The real hidden-preview probe passes all 11 checks across three boots:
`python3 tools/preview_probe.py --only audio --port 9194`.
It exercised real input clicks on both synth cues, external-file autoplay,
resizing without replay, edited PCM reload, Stop, texture-free audio boot and
the Audio footer's round trip to the visual browser. Screenshots were inspected;
button widths now use the real font metrics and the footer is legible.
Retained evidence: `/tmp/synarchy-preview-audio-9194.log`,
`/tmp/synarchy-preview-audio-9194-synth.png`,
`/tmp/synarchy-preview-audio-9194-file.png`,
`/tmp/synarchy-preview-audio-9194-footer.png`.
These boots use null output and establish UI behavior, not listening quality.
The first real boot caught omitted required page arguments in UI constructors;
the pure pane harness now checks that API boundary explicitly.

Final preview validation passes: 75 Audio examples (including disabled-retry
snapshot ordering), 203 existing Preview examples, and 49 App.Cli examples.
Logs: `/tmp/synarchy-audio-20260910T172014.log`,
`/tmp/synarchy-audio-20260910T171358.log`,
`/tmp/synarchy-preview-app-cli-hspec.log`.
The canonical `audio_null,preview_cli` probe selection passes 2/2
(`/tmp/synarchy-audio-preview-cli-gates.log`). Source-distribution validation
passes with 8,065 tracked entries and 6,678 required resources across 32
families (`/tmp/synarchy-preview-sdist.log`). Capability/persistence inventories,
config writes, Unicode operators, module budgets, CI parity and the preview
probe dispatcher self-test pass. Primary checkout is clean; all changes remain
local in the implementation worktree. No PR, visible window or physical audio
was launched. Next is the owner's listening pass in the completed audio player;
bear synthesis and selection integration follow afterward.

## PR boundary accepted, 2026-09-11

The owner said the preview looked good and was ready for a PR. The delivery
scope is now the implemented audio foundation, menu cues, Settings and preview
player. Bear sound content and selection-response behavior are follow-up work.
Physical-device listening and recovery remain unverified; the PR must retain
that distinction from the passing offline/null and rendered-UI evidence.
Portable preview screenshots and validation context are retained in
[the preview evidence](audio/preview_player/README.md).
