# Project Review Findings: PRs #1210–#1183

These entries record focused evidence from the senior review of merged PRs #1210 through #1183 for later one-at-a-time disposition.

Status legend: `[ ]` unprocessed · `[#N]` filed · `[no-issue]` closed without an issue · `[deferred]` blocked on a concrete precondition

## Status

- [ ] PRR-1. Early debug-console quit is lost during startup
- [ ] PRR-2. Name plate misses seam-alias locations
- [ ] PRR-3. Explicit page selection bypasses active-page recurrence
- [ ] PRR-4. Responsive menu tests inherit the saved UI scale
- [ ] PRR-5. Hydrology source comments contradict the authoritative pipeline map

## 1. Engine lifecycle

### PRR-1. Early debug-console quit is lost during startup

> **Captured note:** Do not overwrite an early debug-console quit during startup. `READY` is emitted before the remaining workers and startup handshake finish. An immediate, acknowledged `engine.quit()` sets `CleaningUp`, but `runStartupHandshake` later unconditionally writes `EngineRunning`. The official probe hangs, and a proper response-reading client reproduced it 3/3 times.

**Verification:** Verified — a quit accepted immediately after the listener's readiness marker can be overwritten by the later startup handshake, leaving the headless process running after it acknowledged shutdown.

**Evidence:**

- `src/Engine/Scripting/Lua/DebugServer.hs:193` — the listener prints and flushes `READY` as soon as its socket is bound.
- `app/App/Headless.hs:41` — headless starts four more workers after `startLuaThread` has already exposed the debug listener.
- `src/Engine/Scripting/Lua/Thread/Console.hs:48` — the built-in quit path writes `CleaningUp` directly and returns `shutting down` to the client.
- `src/Engine/Loop/Mode.hs:127` — after a 100 ms settle, the startup handshake unconditionally writes `EngineRunning` without checking whether another thread changed the lifecycle.
- `tools/debug_console_boot_probe.py:210` — the CI-eligible successful-bind check sends `engine.quit()` immediately after observing `READY` and then waits for exit; it timed out at current `master`.

**Handoff context:**

- **Current behavior:** A client can receive both `READY` and the `shutting down` acknowledgement, yet the process remains alive because the startup transition wins the race.
- **Expected behavior:** Once an accepted quit changes the lifecycle to cleanup, startup must not return it to the running state, and the successful-bind probe must exit cleanly.
- **Scope and constraints:** Surfaced while reviewing PR #1198 / issue #1190. Preserve the readiness contract, the built-in acknowledgement behavior, and the shared startup semantics of all loop modes.
- **Remaining uncertainty:** Reproduced three times in headless mode; offscreen shares the relevant listener and loop machinery but was not separately reproduced at capture time.

## 2. Wrapped-world and page boundaries

### PRR-2. Name plate misses seam-alias locations

> **Captured note:** Use seam-aware location containment in the etymology name plate. `scripts/name_plate.lua` uses raw rectangle comparisons. Since #1193 canonicalizes selected tiles, selecting the wrapped image of a seam-crossing discovered location can make its etymology action disappear. The engine's authoritative `boundsContainsPoint` already handles this topology.

**Verification:** Verified — the name plate applies raw Cartesian containment to a selected tile that the pick path now reports in a canonical alias frame, while the location subsystem defines containment over all cylindrical seam aliases.

**Evidence:**

- `scripts/name_plate.lua:74` — `locationRowAt` accepts a location only when `gx` and `gy` fall inside the stored bounds through four raw comparisons.
- `src/World/Render/HitTest.hs:121` — the pick path canonicalizes both the selected tile and fractional hover position before reporting them.
- `src/Location/Bounds.hs:96` — location bounds have explicit cylindrical seam aliases, and `boundsContainsPoint` tests all of them.
- `test-headless/Test/Headless/Location/Bounds.hs:205` — the focused fixture proves a point rejected by raw containment is inside the same physical bounds through its seam alias.
- `test-headless/Test/Headless/UI/ResponsiveGameplay.hs:3115` — the name-plate entry-point test covers only ordinary bounds around `(5,5)`, so it cannot fail on the wrapped case.

**Handoff context:**

- **Current behavior:** A discovered location can be physically selected through a seam alias while its name and etymology action are absent from the name plate.
- **Expected behavior:** A discovered location containing the selected physical tile remains reachable from the name plate regardless of which valid seam alias names that tile.
- **Scope and constraints:** Surfaced in PR #1194 / issue #1104 through its interaction with PR #1193 / issue #1175. Undiscovered locations must remain excluded, and the fix must retain the repository's canonical cylindrical-world coordinate contract.
- **Remaining uncertainty:** The failure is established by the current coordinate and containment contracts; its frequency in generated worlds depends on how often a placed location's selectable footprint lands at the seam.

### PRR-3. Explicit page selection bypasses active-page recurrence

> **Captured note:** Keep etymology recurrence anchored to the active world page. `world.getEtymology(..., pageId)` resolves both the target and recurrence candidates from the requested page. Supplying an inactive page therefore includes its discovered locations, directly violating #1104's active-page-only recurrence contract. The required inactive-page regression was never implemented.

**Verification:** Verified — the optional page argument selects an arbitrary live page, and the recurrence set is then built from that page rather than from the active page promised by the module contract.

**Evidence:**

- `src/Engine/Scripting/Lua/API/WorldQuery/Etymology.hs:18` — the module contract says every inactive page is absent from recurrence by construction.
- `src/Engine/Scripting/Lua/API/WorldQuery/Etymology.hs:215` — `world.getEtymology` publicly accepts an optional `pageId` and passes it into resolution.
- `src/Engine/Scripting/Lua/API/WorldQuery/Etymology.hs:300` — `resolveEtymology` derives the identity, location instances, target, and eligible recurrence entries from the state returned for that argument.
- `src/Engine/Scripting/Lua/API/WorldQuery/Etymology.hs:400` — `worldStateFor` looks up an explicitly named page directly; only an omitted argument resolves the active page.
- `test-headless/Test/Headless/Language/Etymology.hs:570` — recurrence tests exercise pure eligible sets but never create active and inactive pages to test the public query boundary.

**Handoff context:**

- **Current behavior:** Callers can query an inactive page and receive recurrence links computed from that inactive page's discovered locations.
- **Expected behavior:** Recurrence never exposes entities from inactive pages, including when a caller supplies a page identifier to the query surface.
- **Scope and constraints:** Surfaced while reviewing PR #1194 / issue #1104. The query remains read-only; discovered-only location eligibility, inspected-river scoping, and morpheme-identity matching remain separate contracts.
- **Remaining uncertainty:** No production Lua caller currently passes the optional page argument; the violation is exposed through the registered public API and its untested contract.

## 3. Test reliability

### PRR-4. Responsive menu tests inherit the saved UI scale

> **Captured note:** Make ResponsiveMenus tests independent of the user's saved UI scale. With the legitimate local setting `ui_scale: 1.5`, `UI.ResponsiveMenus` reports 88 examples and two failures. #1188's new identity test assumes three rows are simultaneously visible, although the responsive browser correctly shows two plus a scrollbar. An older scroll test has the same unpinned-scale defect.

**Verification:** Verified — the headless engine loads the developer's runtime video configuration, the UI modules read that scale, and the affected specs assert geometry for an implicit 1.0 scale without setting it.

**Evidence:**

- `src/Engine/Core/Init.hs:179` — engine initialization prefers `config/video.local.yaml` over the tracked default and stores the resulting video configuration in the test environment.
- `scripts/ui/scale.lua:4` — menu geometry reads the engine's current UI scale rather than a test-local default.
- `test-headless/Test/Headless/UI/ResponsiveMenus.hs:772` — the save-identity regression creates a 1280×720 browser and requires all three supplied saves to appear in the visible-row dump without pinning a scale.
- `scripts/save_browser.lua:248` — the browser intentionally derives the visible-row count from scaled row and chrome sizes, so two visible rows plus scrolling is valid at 1.5x.
- `test-headless/Test/Headless/UI/ResponsiveMenus.hs:1183` — the create-world scroll assertion likewise assumes a particular scale without setting one.

**Handoff context:**

- **Current behavior:** `cabal test synarchy-test-headless --test-options='--match "UI.ResponsiveMenus"'` produced 88 examples and two failures under a valid local 1.5x scale; the #1188 test saw two visible rows instead of three, and the older scroll-reveal geometry assertion also failed.
- **Expected behavior:** Focused headless UI tests produce the same verdict regardless of the developer's saved runtime UI scale, while tests that intentionally exercise a scale state it explicitly.
- **Scope and constraints:** Surfaced through PR #1188 / issue #1107. This finding concerns regression-gate hermeticity; the save browser correctly remained responsive and scrollable in the observed 1.5x layout.
- **Remaining uncertainty:** Other headless UI suites may also inherit local video settings; only `UI.ResponsiveMenus` was investigated for this finding.

## 4. Documentation consistency

### PRR-5. Hydrology source comments contradict the authoritative pipeline map

> **Captured note:** Align remaining hydrology source comments with the authoritative pipeline map. The new authoritative document correctly says `computeOceanMap` is tectonic-plate-seeded and `composeFluidMap` reads global surface-fluid tables. Three current source comments still claim world-edge seeding or water-table-driven composition.

**Verification:** Verified — current implementation and the authoritative document agree with each other, while three source comments describe different seeding or data ownership.

**Evidence:**

- `docs/hydrology_pipeline.md:147` — the authoritative pipeline says `composeFluidMap` reads global lake, river, ocean, and lava tables, not the water table.
- `docs/hydrology_pipeline.md:191` — the ocean ownership map says the coarse `computeOceanMap` flood is seeded from non-land tectonic plates, while only the tile-resolution flood is world-edge-seeded.
- `src/World/Fluid/Ocean.hs:58` — the implementation locates one below-sea seed near each non-land plate center, with a bounded outward search.
- `src/World/Geology/Timeline.hs:157` — the timeline comment still calls the coarse ocean map world-edge-reachable.
- `src/World/Generate/Chunk/Fluid.hs:65` — the composition comment still calls the coarse flood world-edge-seeded.
- `src/World/Fluid/Ocean.hs:41` — the ocean comment still calls `composeFluidMap` water-table-driven even though surface composition does not read the water table.

**Handoff context:**

- **Current behavior:** Readers following source-local comments receive the wrong ocean-seeding model and the wrong dependency direction between surface fluid and the subsurface water table.
- **Expected behavior:** Source comments and `docs/hydrology_pipeline.md` describe the same current stage ownership, flood seeds, and surface/subsurface dependency direction.
- **Scope and constraints:** Surfaced while reviewing PR #1201 / issue #1109. This is a documentation consistency finding; no world-generation output or runtime behavior change was observed or implied.
- **Remaining uncertainty:** None at capture time.
