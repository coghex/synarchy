# Project Review Findings: PRs #262–#248

These entries record focused evidence from the senior review of the next twelve merged PRs in merge order — #262, #259, #258, #257, #256, #255, #252, #253, #251, #250, #244, and #248 — for later one-at-a-time disposition. The first-parent window contains exactly those twelve PR merges and no unrelated direct commits; #251 is represented by its squash commit.

Status legend: `[ ]` unprocessed · `[#N]` filed · `[no-issue]` closed without an issue · `[deferred]` blocked on a concrete precondition

PR #262's cursor teardown, #257's arena editor teardown, #256's placement teardown, and #253's picker teardown now live in the shared view-transition registry; #259's slider/toggle/randbox hover overlays still resolve through their clickable parents; #251's debug claim independently requires the zoomed-in gameplay view; #250's rectangle hit-test uses the same active-page unit filter as point selection; #244's expanded `ToolMode` remains append-only and its load-reset state machine is covered by focused Lua checks; and #248's drag state is canceled by the central zoom/HUD/menu sweeps. Focused checks passed for the audit suite (35/35), toolbar default reset, deferred load reset, and all 37 enum append-only guards. Synthetic checks also reproduced a content-changing dump that `world_check` reports as `PASS`, a fully contained deep lava cell classified as `FLOATING_LAVA`, and zoom-map-equivalent mouse routing that still calls `unit.hitTestAt` and selects its result. No full headless suite, graphical/offscreen session, real world-check seed run, baseline capture, behavior probe, or `make ci` was run. Three non-duplicate concerns remain.

## Status

- [ ] PRR-1. Zoom-map clicks still act on invisible world entities
- [ ] PRR-2. Deterministic baseline hashes are recorded but never enforced
- [ ] PRR-3. FLOATING_LAVA measures depth rather than floating containment

## 1. Zoom-map gameplay routing

### PRR-1. Zoom-map clicks still act on invisible world entities

> **Captured note:** Gate entity selection, context menus, drag selection, and move orders on the zoomed-in world view as well as menu/modal ownership. A click intended for the zoom map or fade band must not hit-test or command world-view entities that are not being rendered there.

**Verification:** Verified structurally and with a focused Lua routing harness. PR #258 correctly made menu, pause, and modal state inert, but its `isGameplayInputActive()` predicate deliberately remains true in both the zoom map and fade band. The ordinary left/right game-click chain uses that predicate without the extra `hud.currentView == "zoomed_in"` check already applied to debug actions, so it continues to hit-test units/items/buildings, arm box selection, open entity/tile menus, and issue move orders after those render passes have faded completely out. A stubbed zoom-map-equivalent call (`gameplayActive=true`, debug view false) invoked `unit.hitTestAt` once and selected the returned unit.

**Evidence:**

- Issue #154 / PR #258 scoped its new input gate to hidden worlds, menus, pause, and non-gameplay overlays. That closed the reported fallback-world mutation, but did not define the separate zoom-band boundary even though the same handler owns both contexts.
- `scripts/ui_manager.lua:74-124` defines `isGameplayView` only from the current top-level menu and pause state, then adds modal exclusivity in `isGameplayInputActive`; neither predicate inspects `hud.currentView` or camera zoom.
- `scripts/init_mouse.lua:175-188` explicitly documents that `gameplayActive` remains true on the zoom map and fade band, and therefore adds `debugOverlay.inGameplayView()` for armed debug placement. The comment also states that normal selection below is intentionally outside that extra guard.
- `scripts/init_mouse.lua:300-365` consequently arms box selection and calls `unit.hitTestAt`, `item.hitTestAt`, and `building.hitTestAt` for every gameplay-active left click. A hit immediately selects that entity and clears the other domains, with no zoom-band check.
- `scripts/init_mouse.lua:410-483` applies the same menu/modal-only gate to right clicks, then attempts building/unit/item/tile context menus or sends every selected unit a move order to `world.pickTile(x, y)`. These actions also have no zoom-band guard.
- `scripts/hud.lua:1001-1021` independently treats the same left/right press as zoom-map input, selecting or clearing a chunk. Thus a zoom-map click can both perform the intended chunk action and mutate an invisible entity/action domain through the game handler.
- `src/World/Render.hs:47-60,98-119,152-168` computes `tileAlpha` from the fade band and emits no world cursor, ground-item, unit, or building quads once it reaches `0.001`. This is an actual visibility boundary, not merely smaller sprites beneath the zoom map.
- `src/Unit/HitTest.hs:45-90`, `src/Building/HitTest.hs:40-85`, and `src/World/Render/GroundItemQuads.hs:298-345` project candidates using the live camera and z-depth but never apply `tileAlpha`, `zoomFadeStart`/`zoomFadeEnd`, or `hud.currentView`. They can therefore return a target whose corresponding render pass emitted no quad.
- `scripts/debug.lua:288-318` provides the local precedent: its parallel hit-test combines menu/pause ownership with `hud.currentView == "zoomed_in"` specifically to reject the zoom map and fade band.
- A focused stub harness loaded the real `scripts/init_mouse.lua` with `isGameplayInputActive()` returning true (the zoom-map contract), `debugOverlay.inGameplayView()` false, and a unit hit-test returning id 42. One left press called `unit.hitTestAt` and selected id 42, confirming the routing does not have another implicit view guard.
- Tracker and pending-report searches for zoom-map entity clicks, invisible unit/building/item selection, and zoom-map move orders found no owner. Open #1230 concerns location discovery icons, not input routing.

**Handoff context:**

- **Current behavior:** In a top-level world/test-arena view with no modal, `gameplayActive` stays true at every camera zoom. On the fully zoomed-out map, invisible unit/item/building hit boxes can consume a left or right click; an empty right click can open a tile menu or command selected units using the zoom-map camera projection. Drag release similarly calls `unit.hitTestInRect` after arming in the same unguarded fallback path.
- **Expected behavior:** Zoom-map clicks perform only zoom-map interactions, fade-band clicks do not act on either hidden interaction plane, and entity selection/context/move/box-select paths are reachable only while their world-view quads are meaningfully interactive. Menu/modal ownership remains enforced independently.
- **Scope and constraints:** Surfaced from PR #258 / issue #154, with #251's debug-view predicate as the established sibling contract. Preserve chunk selection (#813), zoom-map camera controls, modal input ownership, debug-layer pass-through behavior, and the existing active-page filters inside each hit-test. Keep click and drag press/release classification coherent when a view transition occurs mid-gesture.
- **Remaining uncertainty:** No offscreen pixel-driven reproduction was run, so how easy it is to land on a particular invisible sprite depends on current camera position, zoom, and entity geometry. The render/input predicate mismatch and action routing are direct; an offscreen regression should place a known entity under its zoom-map-projected coordinate and prove left, right, and drag routes remain inert while chunk selection still works.

## 2. Deterministic world-baseline identity

### PRR-2. Deterministic baseline hashes are recorded but never enforced

> **Captured note:** Compare each deterministic current dump with the canonical content hash stored in its tracked baseline. Aggregate tile, elevation, fluid, and issue counts are useful diagnostics, but they must not allow content-different deterministic output to pass without the required rebaseline.

**Verification:** Reproduced deterministically. Baseline capture stores the SHA-256 hash of every canonical dump, and `world_check` computes current hashes, but the check uses them only to ask whether the current runs agree with each other. It never compares a current hash with any stored baseline hash. A synthetic dump with one additional tile field had a different canonical hash while preserving every audited aggregate; `check_seed` returned `PASS` with no failures.

**Evidence:**

- Issue #22 / PR #252 made deterministic **audit summaries** exact, but both the issue and implementation treated summary equality as the baseline contract. They did not test whether the deterministic world content itself still matched the tracked baseline.
- `tools/world_determinism.py:33-61` defines `hash_dump` over the full canonicalized tile array, normalizing only tile/key order. A hash mismatch therefore means world dump content changed, not harmless JSON ordering.
- `tools/world_baseline.py:110-130` writes `determinism.hashes` alongside aggregate stats and representative audit output. Current tracked baselines contain three identical hashes for deterministic seeds.
- `tools/world_check.py:220-242` loads the baseline and computes current hashes, but feeds only `len(set(hashes))` and the stored `deterministic` boolean to `check_determinism_status`. It never reads `baseline["determinism"]["hashes"]`.
- `tools/world_check.py:244-304` then compares only tile count, four elevation aggregates, tolerant fluid-count envelopes, and issue-category counts. Material swaps, biome/vegetation/ore changes, relocated anomalies, or any other content change that preserves those aggregates can pass.
- A focused synthetic check captured a clean 3×3 dump with hash `bc5697c…`, added an otherwise ignored `reviewProbe` field to one tile (current hash `dd98509…`), and ran the real `check_seed` with the changed dump and original baseline. It reported `PASS` and `failures: []` despite `hash changed: True`.
- `tools/test_audit.py:573-680` tests exact category counts and current-run determinism status only. All 35 groups pass while the synthetic hash mismatch passes, so current coverage pins the weaker contract rather than detecting the omission.
- `CLAUDE.md:63-71` states that baselines are tracked, worldgen output is bit-identical across supported platforms, and a worldgen-output PR that skips its tier-3 rebaseline fails CI. That promised gate is not true for content changes invisible to the aggregate checks.
- Tracker and pending-report searches for stored baseline hashes, canonical dump identity, and content-different `world_check` passes found no owner.

**Handoff context:**

- **Current behavior:** A current seed can be deterministic across its own one or more runs and differ from the checked-in deterministic baseline while `world_check` still passes, provided its coarse audited aggregates and category counts match. The stored content hashes are dead validation data.
- **Expected behavior:** For a baseline marked deterministic, every accepted current canonical dump matches the baseline's canonical content identity; a mismatch fails with an actionable rebaseline message. Current-run hash agreement continues to diagnose determinism separately. Historical racy baselines retain their envelope policy rather than being forced through a single representative hash.
- **Scope and constraints:** Surfaced from PR #252 / issue #22. Preserve order-independent canonicalization, the BUG/QUALITY diagnostics, racy-seed envelopes, improvement reporting where intentionally meaningful, platform-independent output, and the documented tier-3 regeneration workflow. A focused unit test should prove both a semantically identical reordered dump and a content-different aggregate-preserving dump.
- **Remaining uncertainty:** The right compatibility behavior for a deterministic baseline file captured before the hash field existed, or containing multiple distinct historical hashes despite its flag, needs an explicit decision. Current checked-in deterministic baselines already carry one repeated content hash, so that migration question does not weaken the reproduced false pass.

## 3. Lava containment classification

### PRR-3. FLOATING_LAVA measures depth rather than floating containment

> **Captured note:** Classify lava as floating/perched from its relationship to its pool boundary and surrounding terrain, not from fluid-column depth alone. Deep contained pools and small genuine spills must not share one count whose large threshold hides the distinction.

**Verification:** Verified structurally and with a synthetic contained pool. PR #255 proved seed 1337's 301 deep lava cells were fully contained, but changed only the permitted count. The classifier still reports every non-ocean fluid column deeper than 15 as floating without inspecting a single neighbour. A deep lava cell surrounded cardinally by dry terrain at the lava surface was reported as `FLOATING_LAVA: 1`; conversely, a real shallow perched/spilling configuration below that depth or a small deep spill under the threshold receives no blocking containment signal.

**Evidence:**

- Issue #20 asked to reduce **or correctly classify** floating-lava artifacts and explicitly preferred a generation fix over raising a threshold. PR #255's investigation established that all 301 seed-1337 examples were deep but contained and that zero bordering dry tiles sat below the lava surface.
- The merged PR did not encode that successful containment test. It raised `FLOATING_LAVA` from 100 to 450, retaining all 301 known false positives and treating their quantity as the regression signal.
- `tools/world_audit.py:203-226` names `check_floating_fluid`, but its complete predicate is `fluidSurf - terrainZ > FLOATING_FLUID_DEPTH`. It reads no neighbouring cell, connected pool, rim elevation, spill path, or surface consistency.
- `tools/world_audit.py:993-1001` says thresholds are approximately 1.5× the worst **current** value across the 21-seed baseline set. `:1049-1058` still fixes `FLOATING_LAVA` at 450 from historical seed-1337 count 301.
- Current tracked baselines no longer support that calibration: only seed 12321 (11) and seed 250 (2) contain `FLOATING_LAVA`; seed 1337 contains zero. The threshold is now about 41× the current observed maximum while its comment still describes superseded output.
- A focused five-cell synthetic grid used one lava center at `terrainZ=-20`, `fluidSurf=0`, with all four dry cardinal rim cells at terrain 0. The pool cannot spill across that represented boundary, yet the real audit returned `{'FLOATING_LAVA': 1, 'ISOLATED_FLUID': 1}` and described only `depth=20`.
- `tools/test_audit.py:129-149` asserts that a uniform deep lava grid produces `FLOATING_LAVA`; it has no contained-versus-breached comparison. The full 35-group audit self-test passes while pinning depth as the intended classifier.
- PR #255's threshold matters particularly outside an exact deterministic-baseline match (new/stress seeds and racy mode). Even for a deterministic baseline, the content-identity finding above shows aggregate category equality can miss one false positive being replaced by one genuine defect at another location.
- Tracker and pending-report searches for lava containment, deep contained pools, perched lava, and the `FLOATING_LAVA` threshold found no owner.

**Handoff context:**

- **Current behavior:** `FLOATING_LAVA` counts deep lava volume, not floating or spill geometry. Known legitimate deep pools consume threshold headroom, while a smaller genuine rim breach can remain below the quality cap or preserve a baseline's aggregate count.
- **Expected behavior:** The audit distinguishes legitimate depth from unsupported/perched/spilling lava using a documented local or connected-pool invariant. Contained deep pools do not count as floating; real breaches produce a focused category whose threshold reflects acceptable occurrence, ideally zero when the predicate is unambiguous.
- **Scope and constraints:** Surfaced from PR #255 / issue #20. This is an audit-contract correction, not authorization to alter lava generation or baselines. Preserve ocean exclusion, useful depth telemetry under a truthful metric name if desired, deterministic audit output, new-seed stress reporting, and separate river/lake policy where deep basins or underground fluid are legitimate for different reasons.
- **Remaining uncertainty:** A five-cell fixture proves the current classifier ignores containment but does not define the production pool-connectivity algorithm. The processor should inspect lava-pool generation and decide how region edges, underground lava, multiple surface levels, and incomplete dump windows affect a safe containment test before setting BUG versus QUALITY severity.
