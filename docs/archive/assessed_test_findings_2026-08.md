# Assessed coordinated-test findings — harness boundaries and HUD rendering

This report records four current concerns selected from an approved coordinated-test assessment: three weaknesses in probe setup or infrastructure and one player-visible HUD rendering defect.

Status legend: `[ ]` unprocessed · `[#N]` filed as issue N · `[no-issue]`
reviewed and deliberately never to be filed · `[deferred]` blocked on a
concrete precondition

## Methodology

The source was approved assessment
`20260828T134204Z-ai-repair-page-aware-sourcing-ui-contain-f50c42`, which
correlated fifteen observations from thirteen coordinated test and playtest
runs at assessed commit
`5d9ff469d242145c5887e75964d3497e5eea86c2`.

The assessment read the complete source reports and primary logs, inspected
the named retained artifacts, traced the implicated behavior from each tested
revision to the assessed commit, and reviewed tracker coverage. For this
report, the four concerns assigned to the durable report lane were rechecked
against current master at
`0e871280a3d5fb3f12c2089825128efcb88d9f8f`. None of their relevant
implementation or probe paths changed after the assessment.

No scenario was rerun while drafting this report. The retained reports,
engine logs, screenshots, and assessment remain the observational evidence.
The exact renderer cause of the HUD defect and the effect on pointer hit
targets remain unknown. Tracker deduplication and final disposition are
intentionally left to `process-report`.

The assessment's other eight findings are not copied into this report:
three were already assigned to existing tracker work, two were fixed or
superseded, two require focused future tests before classification, and one
concerned disposable one-use apparatus whose durable diagnostic need is
already represented elsewhere. No implementation, test, tracker item, or
remote state was changed while drafting this report.

## Status

- [x] AT-1. Lunge cancellation fixtures spawn on unloaded terrain — [#1909]
- [x] AT-2. Item-list probe continues after a failed reach precondition — [#1911]
- [x] AT-3. Direct probe boots count cold compilation as engine readiness — [#1913]
- [x] AT-4. HUD bitmap glyphs collapse at east/west camera facings — [no-issue]

---

## Scenario preconditions

### [#1909] AT-1. Lunge cancellation fixtures spawn on unloaded terrain

The lunge probe stages its timeout and never-lift cases outside the flat
arena's loaded chunk footprint. Those cases therefore depend on the engine's
unloaded-terrain `Z=0` fallback instead of exercising a reachable arena
fixture. The lunge state assertions passed, but the real-launch setup for
those cancellation paths is weaker than the probe claims.

**Evidence:**

- `tools/lunge_probe.py:512-530` — assigns the timeout fixture to row 60
  and the unlifted fixture to row 90.
- `src/World/Generate/Arena.hs:56-58` — defines the arena as a radius-two,
  5×5 chunk footprint.
- `src/World/Generate/Arena.hs:130-132` — generates only chunk coordinates
  from `-arenaRadius` through `arenaRadius` on each axis.
- `.git/codex-test/reports/20260826T134337Z-probe-lunge-849527.test-result.md:56-63`
  — records missing-chunk warnings and `defaulting Z=0` for both units in
  each affected fixture while all ten declared lunge checks passed.

**Handoff context:**

- **Current behavior:** The timeout and never-lift cases can pass while their
  units stand over unloaded coordinates supplied with fallback elevation.
- **Expected direction:** Every real-launch fixture should establish loaded,
  reachable terrain before grading lunge behavior, or explicitly provision
  the additional chunks it needs.
- **Scope and constraints:** Preserve fresh units per case, bounded lunge
  re-staging, the real timeout and never-air state paths, stored launch
  identity, and the existing declared-check ledger. This is a probe-fixture
  concern, not a request to change lunge production behavior.
- **Remaining uncertainty:** The retained run does not show that fallback
  terrain changed the state-machine result; it shows that those two cases do
  not establish the representative physical setup they claim.

### [#1911] AT-2. Item-list probe continues after a failed reach precondition

The unit-to-unit escort branch requires the source and target to be outside
the transfer contract's reach before creating the session. After four live
attempts, it records that precondition with a non-terminal check and proceeds
into session creation even when the pair is already in reach. Dependent
approach and hold failures can therefore be reported from an invalid fixture.

**Evidence:**

- `tools/item_list_widget_probe.py:323-328` — `check` records a failure and
  returns a Boolean but does not stop the scenario.
- `tools/item_list_widget_probe.py:2400-2411` — retries the live move-order
  fixture four times and stops retrying only when the paused Chebyshev gap
  exceeds 1.
- `tools/item_list_widget_probe.py:2433-2444` — records the greater-than-1
  reach condition without consuming the returned Boolean, then immediately
  calls `transfer_session.create`.
- `.git/codex-test/reports/20260826T141456Z-probe-item-list-widget-a3577b.test-result.md:57-65`
  — records an exact gap of 1.0 and confirms that the probe continued into
  the dependent session path.

**Handoff context:**

- **Current behavior:** A failed load-bearing distance condition increments
  the failure count but does not prevent the probe from grading the approach
  and two-sided hold that depend on it.
- **Expected direction:** The branch should establish a deterministic
  out-of-reach pair or terminate the scenario as a setup failure before
  creating the session.
- **Scope and constraints:** Preserve the real player move order, paused
  position measurement, `follow_command` arbitration, unit-to-unit Mode A
  session, two-sided hold, and rendered paired panels. Retain attempted
  destinations and both paused endpoint positions when setup fails.
- **Remaining uncertainty:** The invalid precondition weakens the later
  session-disappearance observation but does not establish that it caused the
  session to close.

## Probe launch lifecycle

### [#1913] AT-3. Direct probe boots count cold compilation as engine readiness

A probe invoked directly without `SYNARCHY_PROBE_ENGINE_EXE` launches
`cabal run` and starts the fixed 180-second `READY` deadline immediately.
Cold compilation is therefore timed as though an engine process were already
starting. Two fresh-worktree runs expired while compilation was still active,
producing empty engine logs and no product coverage; one also required cleanup
of surviving compiler descendants.

**Evidence:**

- `tools/probelib.py:42` — defines the shared engine-readiness timeout as
  180 seconds.
- `tools/probelib.py:201-237` — resolves the launch command, starts the
  process, begins the `READY` deadline, and kills only the immediate process
  when the deadline expires.
- `tools/probe_engine.py:17-25` — documents that aggregate runs receive one
  resolved executable while direct invocation deliberately retains the
  historical `cabal run` fallback.
- `tools/probe_engine.py:42-45` and `tools/probe_engine.py:100-112` — select
  `cabal run -v0 exe:synarchy --` whenever no runner-supplied executable is
  present.
- `tools/run_probes.py:1480-1499` — the aggregate runner already separates
  its lock-coordinated freshness build from every probe process.
- `.git/codex-test/reports/20260827T120147Z-probe-meal-waste-54f407.test-result.md:49-62`
  — records the 180.4-second failure, empty engine log, and still-running
  Cabal/GHC descendants.
- `.git/codex-test/reports/20260827T120245Z-probe-construction-blueprint-footprint-70c09e.test-result.md:45-58`
  — records another exact readiness timeout and an executable completed
  roughly 25 seconds after the deadline.

**Handoff context:**

- **Current behavior:** A supported direct probe command can report “engine
  never printed READY” before an engine executable exists, lose build
  diagnostics to an empty engine log, and potentially leave compiler
  descendants behind.
- **Expected direction:** Direct invocation should distinguish executable
  preparation from engine readiness and clean up its complete owned process
  tree on setup failure.
- **Scope and constraints:** Preserve the ability to run an individual probe
  without a manual prior build, the aggregate runner's single locked
  preflight, its resolved-executable handoff, explicit probe readiness
  timeouts, and diagnostics for genuine engine startup failure.
- **Remaining uncertainty:** Host contention affected the measured build
  durations, but the timer structurally includes compilation regardless of
  machine load.

## Rendered UI isolation

### [no-issue] AT-4. HUD bitmap glyphs collapse at east/west camera facings

> **Disposition:** No issue — the finding's own retained captures refute it. In
> all four of `wall_face{south,north,west,east}.png` and the `toolbar_face*.png`
> crops, the hamburger and all six toolbar glyphs render full-size and legible;
> each control's 60×60 box has identical glyph-ink pixel counts and a 60-pixel
> horizontal extent at every facing, and every pixel differing between any two
> facings falls in the outermost 6-pixel rounded-corner margin where the rotated
> world shows through — zero differing interior pixels. The four full frames are
> mutually distinct, so the rotation did occur. The code agrees: the bindless UI
> vertex shader applies only `ubo.uiProj` (framebuffer-size only), `rotateCW`
> leaves `camRotation` at 0, and `src/UI/` contains no reference to camera
> facing at all.

A real offscreen Vulkan session captured the same HUD at all four world-camera
facings. The hamburger and six left-toolbar bitmap glyphs were legible facing
south and north but became blank or narrow vertical slivers facing west and
east. Button backgrounds and screen-space text remained normally rendered,
showing a camera-facing coupling specific to the bitmap sprite path.

**Evidence:**

- `scripts/hud.lua:102-132` — loads the map and tool controls from bitmap
  textures under `assets/textures/ui/hud/`.
- `scripts/hud.lua:494-540` — builds the left tool stack as a
  `toolbarTool` toggle using those textures.
- `scripts/ui/toggle.lua:109-121` and `scripts/ui/toggle.lua:250-300` —
  create each control as a square `UI.newSprite` on its UI page.
- `src/Engine/Scene/Batch/Sprite.hs:23-47` — keeps UI-layer sprites visible
  independently of world culling and assigns generic sprites no directional
  face map.
- `src/Engine/Graphics/Vulkan/ShaderCode.hs:281-312` — defines the bindless
  UI vertex path using `ubo.uiProj`; it contains no intended world-camera
  facing transform.
- `tools/structure_rotation_probe.py:256-286` — rotates one pinned session
  through all four facings, waits for fresh frames, and verifies that each
  capture is nontrivial and distinct.
- `.git/codex-test/reports/20260828T132335Z-probe-structure-rotation-42f06e.test-result.md:50-58`
  — records the direction-specific glyph collapse and identifies the retained
  full-frame and toolbar-crop artifacts.
- `tools/ci_probes.py:354-360` — classifies the only four-facing rendered
  capture as manual-only and scoped to structure art rather than HUD bitmap
  invariance.

**Handoff context:**

- **Current behavior:** Rotating the world camera east or west visibly
  distorts screen-space toolbar artwork while surrounding UI remains stable.
- **Expected direction:** Screen-space bitmap geometry, UVs, legibility, and
  interactive bounds should remain invariant under world-camera rotation.
- **Scope and constraints:** Investigate the UI sprite geometry, texture/UV,
  batching, and pipeline boundary without changing world rotation,
  directional world sprites, face-map lighting, or day/night behavior. Check
  the hamburger, every toolbar glyph, representative bitmap UI outside the
  toolbar, and pointer hit targets at all four facings.
- **Remaining uncertainty:** The retained run does not identify the responsible
  renderer layer, whether the clickable rectangles also collapse, or whether
  bitmap UI surfaces beyond the left toolbar are affected.
