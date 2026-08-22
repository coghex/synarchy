# Coordinated test validity findings — 2026-08-21

This report records six concerns from an approved coordinated-test assessment covering manual-probe validity and settings discoverability. Each entry preserves focused evidence for later one-at-a-time disposition with `process-report`.

Status legend: `[ ]` unprocessed · `[#N]` filed as issue N · `[no-issue]`
reviewed and deliberately never to be filed · `[deferred]` blocked on a
concrete precondition

## Methodology

The source was approved assessment
`20260821T153024Z-ui-settings-discoverability-ui-tutorial--c60de1`, which
correlated seven observations from five manual probes and one model-driven
playtest at commit
`495fb21ea77bd3d37b50b3ec1c4beada9a4a3e9c`.

The assessment inspected the complete source reports and coordinator logs,
retained engine logs, the playtest turn trace and representative frames,
tutorial HUD screenshots, owning implementation and probe code, and
tested-to-current diffs. The implicated paths were rechecked at
`9a0dbe2bb5e060483eaec4347cd0a016e92156b5`; none changed after the assessed
commit.

No scenario was rerun while preparing this report. The settings observation
remains based on one model-driven session, and the tutorial observation used
synthetic labels rather than the shipped authored labels. No implementation,
test, tracker item, or remote state was changed.

## Status

- [ ] CTV-1. Save-pause resume oracle reads a nonexistent page before loading
- [ ] CTV-2. Location-stamp fixture bypasses its shipped flat-ground precondition
- [ ] CTV-3. Tooltip timing controls may be too easy to miss below the Graphics fold
- [ ] CTV-4. Tutorial HUD probe does not establish objective-row legibility
- [ ] CTV-5. Blood save-load oracle compares unrelated engine-wide texture totals
- [ ] CTV-6. Construction footprint probe can abort before rendering a blueprint

---

## Manual probe semantics

### CTV-1. Save-pause resume oracle reads a nonexistent page before loading

The save-pause probe claims to establish that a resumed loaded session uses the
normal time scale. It actually unpauses the live `pausetest` page and then asks
for the time scale of absent page `main_world` before initiating the load. The
API returns its normal default of `1.0` for an absent page, making this
assertion incapable of detecting an incorrect resumed speed.

The run’s other save/load pause and frozen-clock assertions passed. This is a
coverage defect in one oracle, not evidence of a product pause regression.

**Evidence:**

- `tools/save_pause_probe.py:163-170` — unpauses the current session, then
  queries `world.getTimeScale("main_world")` before the load request.
- `tools/save_pause_probe.py:172-198` — the actual load and publication wait
  occur only after the purported resumed-loaded-session assertion.
- `src/Engine/Scripting/Lua/API/World/Clock.hs:199-217` — returns `1.0` when
  the requested page is not registered.
- The retained engine log identifies the generated and saved page as
  `pausetest`; no `main_world` page exists at the queried boundary.

**Handoff context:**

- **Current behavior:** The resume-speed check passes from the missing-page
  fallback regardless of the live or subsequently loaded page’s real speed.
- **Expected direction:** The probe should load and publish the saved session,
  resolve its active page, unpause that page, and assert its actual time scale.
- **Scope and constraints:** Preserve the existing save/load pause,
  frozen-clock, publication, and stray-time-scale race checks. Do not change
  the API’s documented absent-page fallback merely to strengthen this probe.
- **Remaining uncertainty:** None material about the false-green path.

### CTV-2. Location-stamp fixture bypasses its shipped flat-ground precondition

The location-stamp idempotency probe creates a synthetic five-by-five room with
no placement constraints. Shipped ruin content requires flat ground, but the
probe allows its room onto arbitrary terrain and treats expected structure
counts plus the stamp flag as proof of successful materialization.

The retained run produced the expected floors, walls, posts, and persistence
results while logging 37 rejected footprint-leveling operations. Its
idempotency evidence remains useful, but its setup and oracle cannot support
the broader claim that the full footprint materialized correctly.

**Evidence:**

- `tools/location_stamp_idempotent_probe.py:64-78` — writes the synthetic
  `stamp_probe_room` definition with `anchor: []`.
- `data/locations/ruin_small.yaml:19-30` — the shipped room definition requires
  `anchor: [flat]` for its five-by-five footprint.
- `tools/location_stamp_idempotent_probe.py:180-201` — treats structure counts
  and the stamp flag as successful initial materialization without checking
  terrain edits across the footprint.
- The retained engine log records 17 rejected below-floor cell writes and 20
  rejected out-of-range slope writes during that materialization.

**Handoff context:**

- **Current behavior:** The probe can pass its initial room and persistence
  checks despite rejected terrain-leveling edits that shipped placement rules
  are meant to avoid.
- **Expected direction:** The fixture should satisfy the intended placement
  constraints, or the probe should explicitly verify every terrain result
  needed by its materialization claim.
- **Scope and constraints:** Retain the real chunk-load, clear-one-floor,
  save/restart/load, stamp-flag, and never-before-loaded-location idempotency
  coverage. Do not weaken the existing structure-count assertions.
- **Remaining uncertainty:** The run does not establish a defect in shipped
  flat-anchored locations.

---

## Settings playtest evidence

### CTV-3. Tooltip timing controls may be too easy to miss below the Graphics fold

> **Captured note:** Tooltip timing controls are easy to miss below the
> Graphics fold, and eight turns were too tight to finish the multi-step
> settings goal.

**Verification:** Partially verified — the controls are objectively below the
initial Graphics viewport, and one naive-player session searched all three
other tabs before finding them. One incomplete model-driven session is not
enough to establish a repeatable player-facing discoverability defect.

At 1920×1080, the initial Graphics view ended at `Texture Filter`. The player
searched Notifications, General, and Input, returned to Graphics, attempted one
wheel action that caused no visible movement, and finally exposed `Tooltip
Delay` and `Hint Delay` by dragging the scrollbar. That consumed the eighth and
final turn, so the player never changed a value, applied it, or returned to the
title screen.

The approved assessment classified the turn-budget exhaustion as downstream of
this search path rather than an independent harness concern.

**Evidence:**

- `scripts/settings/graphics_tab.lua:676-779` — creates Tooltip Delay and Hint
  Delay as rows 10 and 11 after the other Graphics settings.
- `scripts/settings_menu.lua:85,634-641` — opens on Graphics and creates the
  tab’s complete row list inside the shared scroll infrastructure.
- `scripts/settings_menu.lua:790-858` — only the current visible-row window is
  shown; deeper rows require a scrollbar offset change.
- Approved assessment
  `20260821T153024Z-ui-settings-discoverability-ui-tutorial--c60de1` preserves
  the eight-turn trace and the initial/final offscreen frames.

**Handoff context:**

- **Current behavior:** Tooltip timing is categorized under Graphics and is
  outside the initial viewport. One naive player interpreted it as a likely
  Notifications, General, or Input setting and exhausted the run budget just
  as the controls became visible.
- **Expected behavior:** A new player given the tooltip-speed goal should be
  able to discover the controls, adjust them, apply the change, and return
  without external guidance.
- **Scope and constraints:** Preserve settings scrolling, responsive layouts,
  the distinction between tooltip dwell and hint delay, live preview, and
  Apply/Save semantics. Do not file a product issue from this single session
  without corroboration.
- **Remaining uncertainty:** A focused repeat with at least 12 turns is needed
  to determine whether independent naive-player sessions show the same search
  pattern and whether the full settings workflow succeeds once the controls
  are found.

---

## UI and graphics probe validity

### CTV-4. Tutorial HUD probe does not establish objective-row legibility

The tutorial HUD probe rigorously measures the rendered glyph bounds of the
open/closed toggle caption, but it applies no equivalent horizontal-bound
oracle to the objective rows. Its long-list phase checks scrolling and frame
changes only.

Retained screenshots visibly clip synthetic objective labels at the right
framebuffer edge. The shipped-shape phase also substitutes labels derived from
internal IDs, so those images do not establish whether the current shipped
tutorial labels clip. The probe therefore leaves horizontal row legibility
unproved.

**Evidence:**

- `tools/tutorial_hud_probe.py:252-360` — hides and restores the toggle caption
  to isolate its rendered glyph columns and assert both box and framebuffer
  bounds.
- `tools/tutorial_hud_probe.py:363-404` — injects a long synthetic tree and
  verifies overflow, wheel scrolling, row-window movement, and screenshot
  differences without measuring row glyph bounds.
- `tools/tutorial_hud_probe.py:444-464` — constructs a shipped-shaped tree but
  assigns each node `id .. ' label'` rather than using the authored shipped
  label.
- Retained `list_scrolled.png` and `already_latched.png` show synthetic text
  reaching or crossing the right framebuffer edge.

**Handoff context:**

- **Current behavior:** The probe can pass while objective-row text is
  horizontally clipped, provided the toggle caption and vertical behavior
  remain correct.
- **Expected direction:** The visual gate should establish that rendered row
  glyphs remain within the checklist and framebuffer, including a phase that
  uses the shipped tutorial tree and authored labels.
- **Scope and constraints:** Preserve caption measurement, transparent overlay
  behavior, wheel routing, passthrough input, vertical scrolling, authored row
  order, and already-latched branch coverage.
- **Remaining uncertainty:** The retained synthetic frames suggest a product
  risk but do not prove that current shipped tutorial content is clipped.

### CTV-5. Blood save-load oracle compares unrelated engine-wide texture totals

The blood GPU lifecycle probe expects engine-wide bindless and texture-map
totals after a save-load replacement to equal the totals captured before the
load. A real loaded world creates its own preview, zoom atlas, and world
textures, so the two sides are not comparable even when all old blood textures
were reclaimed.

Four comparable teardown and replacement paths each reclaimed exactly four
blood resources. Only save-load failed, with global totals increasing by 40
while the replacement page’s blood-handle map was empty. The failure therefore
does not establish a blood texture leak.

**Evidence:**

- `tools/blood_gpu_lifecycle_probe.py:147-162` — waits for global bindless and
  texture-map totals to equal the pre-path baseline.
- `tools/blood_gpu_lifecycle_probe.py:177-199` — prepares and loads a real saved
  world for the save-load replacement path.
- The retained engine log shows the loaded world creating a new preview
  texture, uploading a zoom atlas, and sending its world texture set.
- The same run reported zero blood handles on the replacement page, while the
  other four lifecycle paths returned their comparable global counters exactly
  to baseline.

**Handoff context:**

- **Current behavior:** Legitimate replacement-world allocations make the
  save-load path fail an engine-wide equality oracle even when no replacement
  blood handles survive.
- **Expected direction:** The save-load path should verify disposal through
  blood-owned handles or compare global counters only after accounting for the
  replacement world’s independently owned resources.
- **Scope and constraints:** Preserve the strong exact-counter assertions on
  teardown paths whose non-blood resource sets are comparable. Retain the
  recreate-and-render coverage and do not convert disposal into a check of the
  new page’s empty map alone.
- **Remaining uncertainty:** The evidence rules out this failed assertion as
  proof of a leak; it does not independently prove disposal of every old
  blood-owned GPU handle.

### CTV-6. Construction footprint probe can abort before rendering a blueprint

The construction-blueprint footprint probe searches a fixed grid within 25
tiles of world origin for dry sites. Its seed-0 run generated ocean across that
search region, found no dry one-by-one control site, and aborted before creating
any designation or screenshot.

The result is a deterministic fixture failure, not evidence about construction
blueprint rendering. A visual gate that can stop before producing a frame
cannot reliably protect its stated footprint contract.

**Evidence:**

- `tools/construction_blueprint_footprint_probe.py:88-110` — defines two fixed,
  interleaved candidate sets within ±25 tiles of origin.
- `tools/construction_blueprint_footprint_probe.py:218-242` — treats unloaded or
  wet tiles as non-matches and returns no anchor when the candidate list is
  exhausted.
- `tools/construction_blueprint_footprint_probe.py:316-335` — loads only chunks
  `-2..2`, scans the fixed candidates, and returns immediately if either dry
  site is absent.
- The retained run reported an ocean sample, emitted no designation checks, and
  left its screenshot directory empty.

**Handoff context:**

- **Current behavior:** A valid world seed can block the probe during terrain
  setup, before either the one-by-one control or two-by-three footprint is
  rendered.
- **Expected direction:** The probe should acquire a deterministic renderable
  site compatible with camera clamping and distinguish setup failure from a
  footprint-render failure.
- **Scope and constraints:** Preserve the real UI flow that wires designation
  textures, disjoint control and target anchors, single-Z-plane footprint
  contract, pixel-difference comparison, and guaranteed engine teardown. Do
  not treat a missing dry site as a rendering failure.
- **Remaining uncertainty:** The preferred fixture may be a guaranteed arena
  course, a controlled terrain edit, or a bounded deterministic search that
  remains compatible with the probe’s camera constraints.
