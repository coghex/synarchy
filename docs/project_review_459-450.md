# Project Review Findings: PRs #459–#450

These entries record focused evidence from the senior review of the next twelve merged PRs in merge order — #459, #460, #458, #457, #456, #455, #454, #449, #453, #452, #451, and #450 — for later one-at-a-time disposition. The first-parent window contains exactly those twelve PR merges and no direct commits.

Status legend: `[ ]` unprocessed · `[#N]` filed · `[no-issue]` closed without an issue · `[deferred]` blocked on a concrete precondition

The construction-job flow, ruins and structure variants, script teardown, legacy fullscreen fallback, tracked world baselines, sorted layer-batch merge, and vertex `Storable` work retain their intended core behavior in current source and focused coverage. Later changes repaired the durable construction-payment failure from #459 and the strict-pattern UBO sizing hazard noted around #450; #449 also repaired #452's initially invalid workflow YAML inside this review window. The focused `Graphics.VideoConfig` suite passed 4/4, `Render.PanMargin` passed 6/6, `Scene.BatchMerge` passed 12/12, `World.FloraGrowth` passed 25/25, and `World.DesignationSeam` passed 33/33. No graphical session, malformed-content load, arena save probe, network fault injection, full suite, world check, or `make ci` was run. Six non-duplicate concerns remain; each preserves its uncertainty for the processor to settle before drafting an issue.

## Status

- [x] PRR-1. Foraging discovery remains planar at the cylindrical seam — [#1707]
- [x] PRR-2. Flora YAML can opt into zero-time repeat harvesting — [#1711]
- [x] PRR-3. Food YAML does not enforce one positive nutrition mode — [#1716]
- [x] PRR-4. Fresh and loaded arenas use different base vegetation seeds — [#1718]
- [x] PRR-5. The quad cache can pair one camera stamp with another camera's geometry — [#1720]
- [ ] PRR-6. Optional ntfy requests have retries but no repository-defined time bound

## 1. Seam-aware foraging discovery

### [#1707] PRR-1. Foraging discovery remains planar at the cylindrical seam

> **Captured note:** Make forage lookup, harvestability search, and distance ordering use the same cylindrical coordinate contract as the harvest action. The action now canonicalizes wrap aliases, but discovery still scans raw chunk keys and compares raw planar coordinates, so a hungry unit near a world seam can fail to see physically adjacent food across that seam.

**Verification:** Partially verified. The current query implementation proves that its scan region and distance metric are planar, while the action path proves that wrapped aliases are valid inputs. Existing seam tests pass but do not exercise the forage query. No synthetic engine page was created to demonstrate the missed candidate end to end, and the exact player-visible frequency depends on unit placement and the generated seam geography.

**Evidence:**

- PR #460 / issue #94 introduced `world.findHarvestableFlora` as the search primitive used by foraging AI. Its contract is nearest harvestable flora within a radius; neither artifact records an exception at the cylindrical wrap seam.
- `src/Engine/Scripting/Lua/API/Forage/Query.hs:195-234` accepts raw global `gx`/`gy`, converts the raw bounding square to chunk coordinates, and calls `lookupChunk (ChunkCoord cx cy)` for each key. It neither canonicalizes the requested keys nor expands the region through the seam-aware chunk helpers.
- The same function reconstructs each stored tile's canonical global coordinates and ranks it with raw Euclidean subtraction at `src/Engine/Scripting/Lua/API/Forage/Query.hs:245-251`. Crop candidates repeat the planar comparison at `:263-287`, so even candidates reached by some other route would be ordered or rejected by the wrong seam distance.
- `src/Engine/Scripting/Lua/API/Forage/Lookup.hs:18-34`, used by `getFloraAt` and `getFloraGrowthAt`, likewise applies `globalToChunk` and a raw chunk lookup without canonicalizing an alias.
- In contrast, `src/Engine/Scripting/Lua/API/Forage/Harvest.hs:57-72` obtains the page's wrap width and applies `canonicalTile` before locating the target. This later #1175 change makes the execution verb accept coordinates that the discovery and inspection verbs can miss.
- `scripts/unit_ai_needs.lua:263-288` calls `world.findHarvestableFlora(floor(info.gridX), floor(info.gridY), radius)` and trusts its returned distance and coordinates. Its preceding ground-food fallback at `:249-260` also compares raw `dx`/`dy`, leaving both rungs of the emergency food search planar.
- `World.Generate.Coordinates` already exposes `localizeTileToAnchor`, `seamTileDist2`, and `chunkInSeamRegion`. Current designation-seam coverage passed 33/33, but repository searches found no forage-query case in that suite and no all-state tracker or pending-report duplicate for seam-aware foraging.

**Handoff context:**

- **Current behavior:** A flora instance is stored under a canonical chunk key. A unit close to the opposite alias asks the forage query to scan a nearby raw chunk rectangle; the wrapped neighbor key may not be looked up, and any reached candidate is filtered with raw planar distance. The harvest action itself would accept the alias if the caller already knew the target.
- **Expected behavior:** Forage lookup and nearest-candidate search use the page's cylindrical topology for region membership, target lookup, distance cutoff, and ordering. A unit sees the same physically adjacent flora regardless of which valid wrap alias represents its position.
- **Scope and constraints:** Surfaced from PR #460 / issue #94. Preserve growth-phase and regrowth eligibility, crop/ground-food priority, deterministic tie-breaking, returned canonical coordinates, page-local wrap width, and the later #1175 action canonicalization. Prefer the established coordinate helpers over a forage-private modulo formula. Add a production-path test with search origin and target on opposite seam aliases, including nearest ordering when a planar candidate also exists.
- **Remaining uncertainty:** Static inspection establishes the coordinate mismatch but not its current play frequency. The processor should verify one real page fixture before choosing whether this is one shared food-search issue or should keep loose-item ground search outside the initial scope.

## 2. Flora harvest schema validity

### [#1711] PRR-2. Flora YAML can opt into zero-time repeat harvesting

> **Captured note:** Validate harvest regrowth as a finite, strictly positive duration when a flora entry is repeat-harvestable. The loader currently accepts zero and negative values, and the harvest verb records that value unchanged after awarding yield, leaving the same instance immediately eligible for another direct harvest.

**Verification:** Partially verified. The parser-to-runtime path proves that a non-positive value reaches the harvest-state map and that a second direct call passes the `live <= 0` guard. All shipped flora use positive regrowth values, so this is a latent content-authoring defect rather than a currently demonstrated exploit. It remains possible that zero was intended to mean one-shot removal, but the implementation does not give it that meaning.

**Evidence:**

- PR #460 / issue #94 added authored harvest yields and `regrowth_time` as the basis for repeatable wild foraging. The linked contract describes depleted flora becoming available again after time; it does not define zero or negative timers.
- `src/Engine/Asset/YamlFlora.hs:62-101` decodes `regrowth_time` as a required `Float` but performs no finite or positive-domain validation. Yield counts are also decoded without domain checks, though this finding is limited to the timer behavior with the clearest repeated-reward consequence.
- `src/Engine/Scripting/Lua/API/YamlTextures.hs:323-344` copies the decoded duration directly into `FloraHarvest`; there is no later catalogue validation before gameplay can use the definition.
- `src/Engine/Scripting/Lua/API/Forage/Harvest.hs:133-138` awards the configured yields when the live timer is non-positive and reinserts `fhRegrowth` unchanged. With zero or a negative duration, another direct `world.harvestFlora` call sees the same non-positive live value and can award the yield again.
- `src/World/Flora/Harvest.hs:30-42` removes non-positive timers on the next positive growth tick. That cleanup does not protect repeated calls made before the tick, and removal makes the instance bare-harvestable again rather than implementing a permanent one-shot state.
- `World.FloraGrowth` passed 25/25 and pins ordinary growth and `harvestOpen` behavior, but it does not load an invalid authored duration or call the production harvest verb twice. All shipped `data/flora/*.yaml` regrowth values are positive; all-state tracker and pending-report searches found no matching schema-boundary concern.

**Handoff context:**

- **Current behavior:** Zero, negative, non-finite, and ordinary positive durations share the same accepted YAML shape. A non-positive entry can yield repeatedly through direct action calls, and the next growth tick does not turn it into an explicit one-shot harvest.
- **Expected behavior:** A repeat-harvest definition has a finite, strictly positive regrowth duration and fails loading with a useful content error otherwise. If one-shot harvests are desired, they use an explicit lifecycle/removal contract rather than overloading a non-positive timer.
- **Scope and constraints:** Surfaced from PR #460 / issue #94. Preserve the existing positive-duration regrowth behavior, lifecycle/season gates, tagged-harvest selection, deterministic yield rolling, and valid shipped data. Validate at the authored-data or catalogue boundary rather than relying on every action caller to clamp the value. Add a malformed-fixture test and retain a positive-duration execution test.
- **Remaining uncertainty:** No shipped datum triggers this today. The processor should confirm whether designers need zero as a one-shot shorthand; if so, specify and test the resulting instance-removal and save/load semantics instead of merely permitting zero.

## 3. Food nutrition schema validity

### [#1716] PRR-3. Food YAML does not enforce one positive nutrition mode

> **Captured note:** Require each food definition to select exactly one valid nutrition mode: positive discrete calories, or positive calories-per-kilogram on a compatible bulk/container item. The current defaults admit neither mode and also admit both; direct feeding then consumes a zero-calorie item as success, can reduce hunger for a negative value, or silently chooses the bulk branch when both are present.

**Verification:** Partially verified. The decoder and feeding code establish the branch and mutation behavior without malformed live data. Every shipped food currently chooses a valid positive mode, and auto-eat filters out non-positive nutrition before calling `unit.feed`; the malformed cases therefore require future content or another direct API caller. Whether hybrid foods should ever carry both values is a design choice, but the runtime currently does not combine them.

**Evidence:**

- PR #458 / issue #93 introduced discrete food, bulk food, digestion, and automatic eating. `Item.Types` documents these as two shapes: `ifCalories > 0` for discrete food and `ifCaloriesPerKg > 0` for bulk food.
- `src/Engine/Asset/YamlItems.hs:54-68` decodes both `food.nutrition.calories` and `calories_per_kg` as optional numbers defaulting to zero. It validates neither finiteness/positivity nor mutual exclusion.
- `src/Engine/Scripting/Lua/API/Items/Defs.hs:100-113` installs both numbers unchanged in `ItemFood`; no catalogue-stage invariant repairs the permissive decoder.
- `src/Engine/Scripting/Lua/API/Units/Survival.hs:91-157` selects bulk behavior only when `ifCaloriesPerKg > 0`. Otherwise it removes one discrete item and applies `min maxH (cur + ifCalories food)` without a lower clamp. Zero therefore consumes an item and returns `Just 0`; a negative value consumes an item and can reduce the hunger value while still returning a successful result.
- If both fields are positive, that same branch order silently chooses calories-per-kilogram and ignores the discrete value. A positive per-kilogram definition on an item without compatible fill/container state is accepted but contributes no usable bulk quantity.
- `scripts/unit_ai_needs.lua:141-158` ignores candidates whose computed nutrition is non-positive, so malformed zero/negative food becomes invisible to the AI while remaining consumable through the public verb. Lua also treats numeric zero as truthy, which makes `Just 0` a success-shaped API result rather than a conventional false failure.
- Shipped tomatoes, wheat, quinoa, berries, greens, and rations all select a positive mode. All-state tracker and pending-report searches found no issue covering the food-definition invariant.

**Handoff context:**

- **Current behavior:** A `food` block with both nutrition fields omitted loads with two zeroes; direct feeding can remove the item for no benefit. Negative discrete calories can reduce hunger, both-positive data silently means bulk-only, and bulk nutrition is not coupled to an item shape that can actually provide mass.
- **Expected behavior:** Catalogue loading rejects non-finite, non-positive, ambiguous, or structurally incompatible nutrition definitions with the source item identified. Exactly one supported nutrition mode reaches feeding code.
- **Scope and constraints:** Surfaced from PR #458 / issue #93. Preserve valid discrete and bulk consumption, partial bulk servings, stomach capacity, digestion timing, inventory mutation atomicity, AI selection order, and current valid content. Confirm the authoritative container/fill predicate before coupling it to per-kilogram food. Test malformed YAML/catalogue fixtures plus one valid case for each mode.
- **Remaining uncertainty:** A future design might intentionally allow both values as alternative presentations or a zero-calorie consumable. If either is wanted, its selection and success semantics need to be explicit; the current priority branch and default-zero behavior are not enough to distinguish intent from typo.

## 4. Arena base-world persistence

### [#1718] PRR-4. Fresh and loaded arenas use different base vegetation seeds

> **Captured note:** Give a debug arena one stable base-world identity across creation and load. Fresh arenas generate their grass variants from `newStdGen`, but their synthetic persisted parameters record seed zero and the load path rebuilds from `mkStdGen 0`; because the save stores edits rather than the complete base tile grid, untouched vegetation can change after a save/load round trip.

**Verification:** Verified from the construction, snapshot, and load paths. The two generators are not defined to use the same seed, and the vegetation IDs are random outputs not represented by the edit overlay. No arena probe compared untouched vegetation before and after a round trip, so the report does not claim a particular visible cell changes on every possible run; a chance-equal pattern is possible.

**Evidence:**

- PR #454 / issue #365 refactored arena pages through the flat-world builder so they could participate in save/load, and its merged description says an arena is rebuilt identically on load before edits are replayed.
- `src/World/Thread/Command/Init.hs:406-447` obtains `newStdGen`, passes it to `generateArenaChunks`, and then records synthetic `WorldGenParams` with `wgpSeed = 0` for the page.
- `src/World/Load/Stage.hs:291-305` recognizes the arena generator kind and reconstructs its base chunks with `generateArenaChunks (mkStdGen 0)` before applying the saved overlay.
- `src/World/Generate/Arena.hs:49-115` consumes the supplied generator with `randomR (0, 3)` to choose grass vegetation IDs. The generated base therefore contains random data whose identity depends on the unpersisted fresh generator.
- `src/World/Save/Snapshot.hs:118-150` persists generation parameters, the edit map, entities, activities, and related page state, but not the complete `wsTilesRef` base grid. Untouched base vegetation is consequently reconstructed rather than restored.
- `tools/multiworld_save_probe.py --arena` verifies that a deliberately edited arena tile's z value survives. It does not snapshot an untouched base vegetation pattern, so replaying the edit overlay can pass while the surrounding base changes.
- All-state tracker and pending-report searches for arena grass/vegetation identity across save/load found no duplicate concern.

**Handoff context:**

- **Current behavior:** A newly created arena uses process-provided randomness for its grass variants, while a loaded arena uses deterministic seed zero. Explicit edits survive replay, but untouched random base details are free to change at the first load.
- **Expected behavior:** Fresh creation and reconstruction derive the arena base from one persisted identity, or the random base state itself is saved. A save/load round trip preserves untouched tiles as well as overlays.
- **Scope and constraints:** Surfaced from PR #454 / issue #365. Preserve the 5×5 flat arena topology, arena generator-kind recognition, transactional load behavior, overlay replay, page isolation, and the existing ordinary-world seed contract. The smallest repair may be to choose and record the deterministic arena seed at creation rather than expand the save payload. Extend the arena probe or a focused persistence test to compare an untouched vegetation vector across save/load.
- **Remaining uncertainty:** The arena is a debug/testing world and its grass variation may have been considered cosmetic. The processor should confirm whether exact base identity is part of the promised arena persistence contract; the PR's “identically” language and shared save system currently suggest that it is.

## 5. Camera snapshot consistency

### [#1720] PRR-5. The quad cache can pair one camera stamp with another camera's geometry

> **Captured note:** Build and stamp a cached world-quad pass from one immutable camera snapshot. The world render loop decides to rebuild from `currentSnap`, but the quad builder rereads the live camera reference and uses that second value for facing, zoom, z slice, position, and bounds; a concurrent camera update can therefore produce geometry for camera B stored under camera A's cache stamp.

**Verification:** Partially verified. The two independent STM reads and the mixed use of their values are explicit in current source, and the capability inventory confirms that the camera reference is shared and written outside the world thread. No scheduler-controlled or graphical reproduction forced an update between those reads. A later world tick should usually detect the changed camera, so the visible symptom may be a transient edge pop or redundant rebuild rather than a durable stale cache.

**Evidence:**

- PR #453 / issue #447 added pan margins and background quad-cache rebuilds. Its stated contract is that camera snapshot values used for cache invalidation and coverage agree while a parallel rebuild is in flight.
- `src/World/Render.hs:42-68` reads the camera and constructs `currentSnap`; `:79-94` passes that snapshot to `renderWorldQuads` and then stores the returned quads as `WorldQuadCache curGen currentSnap sorted`.
- `src/World/Render/Quads.hs:44-58` accepts the supplied snapshot but separately reads `rvCameraRef`. It takes framebuffer dimensions and pan margins from the snapshot while taking facing, zoom, z slice, and position from that second live camera.
- The builder uses the second camera in its visibility and coordinate work through `src/World/Render/Quads.hs:57-89` and `:122-127`, including `computeViewBounds camera`. The resulting quad set can therefore describe a different view than the snapshot attached to it.
- `docs/engineenv_capability_inventory.md:442` classifies `cameraRef` as a multi-thread shared live container read by the world and render paths and written from the main-render and Lua paths. An intervening update is permitted by the architecture, not excluded by thread ownership.
- The focused `Render.PanMargin` suite passed 6/6, but those pure cases supply a coherent snapshot/camera pair and cannot expose a second live read racing the stamp. All-state tracker and pending-report searches found no duplicate for mismatched camera cache identity.

**Handoff context:**

- **Current behavior:** Rebuild necessity is assessed from camera A. The expensive quad walk may then use camera B but publish its result tagged as A. Subsequent invalidation can repair the mismatch, but the cache handoff itself does not satisfy its own identity contract.
- **Expected behavior:** Every camera-derived value used to build a cache entry comes from the same immutable value used as that entry's stamp. The function boundary should make a mixed pair difficult or impossible to construct.
- **Scope and constraints:** Surfaced from PR #453 / issue #447. Preserve asynchronous rebuilding, generation checks, pan-margin reuse, layer sorting, z/facing filtering, and the cheap main-thread handoff. Prefer passing the captured camera or a complete render snapshot through the pure builder rather than broadening STM synchronization. Add a test whose live reference changes after snapshot capture and assert that stamp and geometry remain coherent.
- **Remaining uncertainty:** The actual visual severity has not been measured, and the ordinary next-tick invalidation may reduce it to a single handoff. The processor should reproduce or instrument the race before assigning gameplay severity, while treating the cache-identity violation itself as established.

## 6. Notification workflow time bounds

### PRR-6. Optional ntfy requests have retries but no repository-defined time bound

> **Captured note:** Put an explicit short deadline around each optional ntfy delivery, and preferably the notification job as a whole. The workflow retries errors and tolerates a completed failure, but `continue-on-error` cannot release a runner when a connected endpoint stops producing a response and `curl` has no connect or total timeout.

**Verification:** Partially verified. The checked-in workflow has no `timeout-minutes`, `curl --connect-timeout`, or `curl --max-time`, so the repository supplies no short bound. No deliberately stalled endpoint was used, and GitHub-hosted networking or the platform's broad default job timeout may terminate some hangs eventually. The concern is therefore boundedness and resource isolation, not evidence that ntfy has already stalled a real run.

**Evidence:**

- PR #452 introduced `.github/workflows/ntfy-notify.yml` as a best-effort repository-event notification. Its test plan left the first live event unchecked, and the design goal was that notification failures not affect the primary workflow.
- `.github/workflows/ntfy-notify.yml:5-46` defines no job-level `timeout-minutes`. Each of its four delivery branches runs `curl` with `--retry 3 --retry-delay 2 --retry-all-errors` but without connection or total-transfer limits.
- `continue-on-error: true` handles a command that eventually exits nonzero; it does not interrupt a command still waiting on a socket. The retry count likewise bounds attempts only after `curl` recognizes a failure.
- Later commit `73f5a546` was described as adding notification “timeout” resilience but added retries and `continue-on-error`, not an elapsed-time deadline. The initially invalid YAML was separately repaired by #449 within this same twelve-merge window and is not reported as a current problem.
- All-state tracker and pending-report searches for ntfy/curl timeout handling found no duplicate concern.

**Handoff context:**

- **Current behavior:** Ordinary connection errors are retried and an eventual failure is non-blocking, but a request that remains connected without completing can occupy the notification job until an external platform/network limit intervenes.
- **Expected behavior:** Optional notification delivery has an explicit, short, repository-owned maximum duration for connection and transfer, with a job timeout as a final guard. Failure still remains independent of the triggering repository event.
- **Scope and constraints:** Surfaced from PR #452. Preserve event filters, message formatting, secret handling, retries for brief transient failures, and best-effort semantics. Avoid exposing the ntfy URL or message body in diagnostics. A workflow-lint check plus a local/fault-injected stalled HTTP endpoint is sufficient validation; no real notification is required.
- **Remaining uncertainty:** Hosted-runner sockets may already fail quickly in the common outage modes, and this workflow may run rarely enough that the broad platform timeout is accepted operationally. The processor should weigh that runner-cost risk against the maintenance cost of explicit bounds before drafting.
