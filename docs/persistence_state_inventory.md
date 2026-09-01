# Persistence State Inventory

**Status:** Authoritative, Phase 1 of the save-overhaul epic (issue
#756). Written 2026-07-12, against `master@21db64b3`.

This is the field-by-field classification the
[persistence contract](persistence_contract.md) requires. Read the
contract first — it defines the five classifications, the "root state
owner" scope, and the queue/transport rules referenced below.

**Columns:** *Owner* is `file:line` of the record/definition at the time
of writing (line numbers drift; the field name is the stable key).
*Scope* is `global` (one instance for the whole engine) or `per-page`
(one instance per `WorldState`/`WorldPageSave`). *Restoration
dependency* is what must already be loaded/resolved before this item can
be restored. *Validation* is what "restored correctly" means beyond
type-correct deserialization, where that's non-trivial. *Test oracle* is
the probe/test expected to catch a regression here; `none yet` is
recorded honestly rather than inventing coverage that doesn't exist.

Every root-owner field name in this document is wrapped in backticks in
its own table cell, under a `### OwnerName` heading naming the exact
record (or, for Lua, `### Lua persistence registry`) that owns it —
`tools/persistence_inventory_audit.py` matches classifications by BOTH
the backtick-quoted name AND its owner heading, so a name shared by two
different owners (say, a field on one record and an unrelated Lua
module) can't cross-satisfy each other's requirement. **Do not
reformat a field name out of backticks, and do not remove or rename a
`### OwnerName` heading** when editing this file; the audit will report
the affected fields as unclassified.

---

## 1. `EngineEnv` (`src/Engine/Core/State.hs:64`) — global

### EngineEnv

| Field | Scope | Classification | Restoration dependency | Validation | Test oracle |
|---|---|---|---|---|---|
| `engineConfig` | global | Exclude | — | boot flag, not session state | none yet |
| `engineStateRef` | global | Rebuild | see §2 (`EngineState` fields classified individually) | the IORef itself is always freshly allocated at boot; it is never the pointer that's restored, only the value it comes to hold, which is why the interesting classification decisions live on `EngineState`'s own fields (§2), not here | none yet |
| `videoConfigRef` | global | Exclude | — | local runtime config (`config/video.local.yaml`, #638/#786) | `tools/config_state_probe.py` |
| `windowSizeRef` | global | Exclude | — | OS/window-owned | none yet |
| `windowPosRef` | global | Exclude | — | OS/window-owned | none yet |
| `windowStateRef` | global | Exclude | — | OS/window-owned | none yet |
| `framebufferSizeRef` | global | Exclude | — | OS/window-owned | none yet |
| `framebufferMinimizeGenRef` | global | Exclude | — | OS/window-owned (a live minimize counter, meaningless across processes) | none yet |
| `fpsRef` | global | Exclude | — | display setting (`video.local.yaml`) | `tools/config_state_probe.py` |
| `brightnessRef` | global | Exclude | — | display setting (`video.local.yaml`) | `tools/config_state_probe.py` |
| `pixelSnapRef` | global | Exclude | — | display setting (`video.local.yaml`) | `tools/config_state_probe.py` |
| `textureFilterRef` | global | Exclude | — | display setting (`video.local.yaml`) | `tools/config_state_probe.py` |
| `inputQueue` | global | Exclude | — | transport queue; see contract §3 | none yet |
| `inputBarrierNextRef` | global | Exclude | — | session-only automation token allocator | none yet |
| `inputBarrierRef` | global | Exclude | — | session-only automation token allocator | none yet |
| `loggerRef` | global | Exclude | — | logging sink, rebuilt at boot | none yet |
| `luaToEngineQueue` | global | Exclude | — | transport queue; see contract §3 | none yet |
| `luaQueue` | global | Exclude | — | transport queue; see contract §3 | none yet |
| `lifecycleRef` | global | Exclude | — | rebuilt at boot (`EngineStarting`) | none yet |
| `assetPoolRef` | global | Rebuild | textures on disk | GPU asset pool rebuilt from content | none yet |
| `textureNameRegistryRef` | global | Rebuild | textures on disk | name→handle registry rebuilt at boot | none yet |
| `nextObjectIdRef` | global | Exclude | — | scene-object id allocator; scene itself is session-only | none yet |
| `nextItemInstanceIdRef` | global | Persist exactly | — | restored by plain ASSIGNMENT from `sdNextItemInstanceId` (#67, reshaped by #763's session-replacement load — `World/Load/Publish.hs` documents why the discarded session's value is deliberately not max'd in); collision-freedom is enforced at SAVE time instead, by `World.Save.Snapshot`'s `itemAllocatorErrors` | `tools/item_instance_probe.py` |
| `fontCacheRef` | global | Rebuild | font assets on disk | rebuilt at boot | none yet |
| `inputStateRef` | global | Exclude | — | live device state (keys/buttons down) | none yet |
| `keyBindingsRef` | global | Exclude | — | local runtime config (`config/keybinds.local.yaml`, #638/#786) | `tools/config_state_probe.py` |
| `currentKeyDownRef` | global | Exclude | — | transient dispatch flag, `Nothing` outside a key-down broadcast | none yet |
| `textBuffersRef` | global | Exclude | — | transient scene-object text cache (`engine.spawnText`/`setText`/`getText`), NOT a UI text-input buffer; entries are created and removed with their scene nodes (#1961) and the scene graph is itself session-only | `Test.Headless.Lua.SceneText` |
| `cameraRef` | global | Exclude | active page's `WorldCamera` + `wpsCameraZoom`/`wpsCameraFacing` | session-only render camera; see §8 for the per-page source of truth | none yet |
| `uiCameraRef` | global | Exclude | window size | derived from window size at boot | none yet |
| `uiManagerRef` | global | Exclude | — | entire UI tree rebuilt by Lua on load | none yet |
| `focusManagerRef` | global | Exclude | — | rebuilt as UI rebuilds | none yet |
| `worldManagerRef` | global | Rebuild | `SaveData.sdWorlds`, `sdActivePage`, `sdVisiblePages` | the container itself is rebuilt; each contained `WorldState` is restored per §3/§4 | `tools/multiworld_save_probe.py` |
| `hudActivePageRef` | global | Exclude | `wmVisible` | reset from the active/visible page set post-load | none yet |
| `loadStatusRef` | global | Exclude | — | Runtime-only whole-session load transaction diagnostics (#763), the load-side counterpart to `saveBarrierRef`; never serialized. Also carries #1181's test-only `StageGate` — likewise runtime-only coordination, and inert unless a test arms it. | `Test.Headless.Load.Status`, `tools/transactional_load_probe.py` |
| `pendingLoadRef` | global | Exclude | — | Runtime-only single-slot staged-session handoff (#763) between `WorldLoadTransaction` (staging) and `WorldLoadPublish` (the atomic swap); never serialized, and always cleared before/after use. | `tools/transactional_load_probe.py` |
| `inputThreadActiveRef` | global | Exclude | — | Runtime-only boot-mode flag (#763 round 4): True once `Engine.Input.Thread.startInputThread` actually launched (never happens under `App.Headless`); consulted to decide whether `SaveInput` belongs in a save/load transaction's owner set, never serialized. | `tools/transactional_load_probe.py`, `tools/save_barrier_probe.py` |
| `worldQueue` | global | Exclude | — | transport queue; see contract §3 | none yet |
| `sunAngleRef` | global | Rebuild | visible head page's world time | `SolarBase`, derived via `worldTimeToSunAngle` (#1869 added the `world.setSunAngle` override flag beside the angle; both are within-session render state) | none yet |
| `worldPreviewRef` | global | Exclude | — | pending GPU upload payload | none yet |
| `worldPreviewGenerationRef` | global | Exclude | — | runtime-only monotonic generation token used to suppress stale world-preview upload announcements; never serialized | `Test.Headless.Lua.PreviewGeneration` |
| `zoomAtlasDataRef` | global | Exclude | — | pending GPU upload payload | none yet |
| `screenshotRequestQueue` | global | Exclude | — | debug-only transport queue; see contract §3 | none yet |
| `worldQuadsRef` | global | Rebuild | loaded chunk data | render cache | none yet |
| `sceneStatsRef` | global | Exclude | — | transient scene-assembly telemetry (#1921): per-category scanned/emitted/elapsed measurements for the last completed `updateWorldTiles` pass, cleared by world teardown and rebuilt by the next pass | none yet |
| `textureSystemRef` | global | Exclude | — | GPU bindless texture system | none yet |
| `samplerCacheRef` | global | Exclude | — | GPU sampler cache | none yet |
| `textureSizeRef` | global | Exclude | — | GPU texture dimension cache | none yet |
| `maxImageDimensionRef` | global | Exclude | — | The device's real `maxImageDimension2D` (#2020), queried at Vulkan init; a property of the machine, never of the session | hspec `--match "map image plan"` |
| `bloodDisposeQueue` | global | Exclude | — | transient cross-thread GPU-dispose transport queue (#788); see contract §3 | `tools/blood_gpu_lifecycle_probe.py` |
| `defaultFaceMapSlotRef` | global | Exclude | — | GPU texture slot | none yet |
| `floraCatalogRef` | global | Rebuild | `data/*.yaml` flora content | reloaded fresh from YAML at boot; species referenced by numeric id from world state (see §9 re: its unused `Serialize` instance) | `tools/flora_growth_probe.py` |
| `materialRegistryRef` | global | Rebuild | built-in material table | boot-time, not YAML-driven | none yet |
| `unitManagerRef` | global | Rebuild | see §5 (`UnitManager` fields classified individually) | the IORef itself is always freshly allocated at boot; the interesting classification decisions live on `UnitManager`'s own fields (§5) | none yet |
| `unitQueue` | global | Exclude | — | transport queue; see contract §3 | none yet |
| `utsRef` | global | Rebuild | `wpsUnits`/`wpsUnitSimStates` after load | the IORef itself is always freshly allocated at boot and repopulated from the restored `UnitInstance`/`UnitSimState` snapshot, not itself directly serialized; the classification decision for the state it holds lives on `UnitThreadState.utsSimStates` (§5), which IS persisted | `tools/movement_probe.py` (post-load steering sanity) |
| `statRNGRef` | global | Exclude | — | explicitly non-deterministic, not save-seeded (contract §1) | none yet |
| `buildingManagerRef` | global | Rebuild | see §5 (`BuildingManager` fields classified individually) | the IORef itself is always freshly allocated at boot; the interesting classification decisions live on `BuildingManager`'s own fields (§5) | none yet |
| `texPaletteRef` | global | Persist exactly | — | `sdTexPalette` | `tools/persistence_contract_probe.py` (see §12) |
| `texPaletteHandlesRef` | global | Exclude | `texPaletteRef` | runtime GPU translation table rebuilt from `texPaletteRef` | none yet |
| `structureWallCatalogRef` | global | Rebuild | `data/structure_packs/*.yaml` wall art | directional wall sprites/cap facemaps per pack variant (#1712), re-registered from the pack YAML by `scripts/structures.lua` at every boot; keyed by texture PATH so it stays valid across the palette replacement a load performs | none yet |
| `structureArtCatalogRef` | global | Rebuild | `data/structure_packs/*.yaml` per-kind art + `build:` blocks | per-kind texture/facemap pairs for UNPLACED structure pieces (#1842) plus which kinds carry complete build metadata, re-registered from the pack YAML by `scripts/structures.lua` and `scripts/wire.lua` at every boot; keyed by pack NAME and holding texture PATHS, so — like `structureWallCatalogRef` — it stays valid across the palette replacement a load performs, and registering it interns nothing into `texPaletteRef` | none yet |
| `buildingQueue` | global | Exclude | — | transport queue; see contract §3 | none yet |
| `combatQueue` | global | Exclude | — | transport queue; see contract §3 | none yet |
| `combatEventsRef` | global | Exclude | — | explicitly not-persisted event stream to Lua | none yet |
| `injuryEventsRef` | global | Exclude | — | explicitly not-persisted event stream to Lua | `tools/injury_log_probe.py` (stream behavior, not persistence) |
| `thoughtEventsRef` | global | Exclude | — | explicitly not-persisted event stream to Lua | `tools/thought_probe.py` (stream behavior, not persistence) |
| `actionOutcomeRef` | global | Exclude | — | explicitly not-persisted event stream to Lua | `tools/action_outcome_probe.py` (stream behavior, not persistence) |
| `buildingGhostRef` | global | Exclude | — | placement-preview UI state | none yet |
| `worldGenConfigRef` | global | Rebuild | `config/world_gen_default.yaml` | global worldgen tunables, distinct from a specific world's `wpsGenParams` | none yet |
| `pathingConfigRef` | global | Rebuild | `config/pathing.yaml` | global pathing tunables | none yet |
| `simQueue` | global | Exclude | — | transport queue; see contract §3 | none yet |
| `enginePausedRef` | global | Persist exactly | — | `sdEnginePaused`; authoritative over any Lua-side copy (see §7 `pause` module) | `tools/save_pause_probe.py` |
| `playerIntentGenRef` | global | Exclude | — | #913: runtime-only monotonic counter of PLAYER pause/time-scale intents (an `MVar` so it doubles as the mutex serializing them against the autosave restore), bumped by `engine.setPaused`/`world.setTimeScale` and by nothing the engine itself writes. An autosave compares a snapshot of it to decide whether restoring its own pre-request pause/time scale would overwrite a choice the player made during the save. Meaningless across sessions (only differences within one request window matter), so persisting it would be recording a number nothing could ever read. | `tools/autosave_probe.py` |
| `enginePauseGenRef` | global | Exclude | — | #1730: runtime-only monotonic counter of pause assertions made by an engine source INDEPENDENT of any running save (a `pause: true` notification category, an `engine.loadSave` acceptance), read and written only inside the `playerIntentGenRef` mutex above. A pause epoch records no owner and a second pause on an already-paused session is a complete no-op, so an autosave compares a snapshot of this counter to tell "somebody else still wants the game paused" from "only I paused it". Meaningless across sessions (only differences within one request window matter), so persisting it would be recording a number nothing could ever read. | `test-headless/Test/Headless/World/PauseSpeed.hs` |
| `saveBarrierRef` | global | Exclude | — | Runtime-only coordinated-save request/owner acknowledgement diagnostics; never serialized. | save-barrier hspec + headless probe |
| `gameTimeRef` | global | Persist exactly | — | `sdGameTime` | `tools/save_pause_probe.py` |
| `lastSaveTimeRef` | global | Exclude | — | wall-clock bookkeeping, session-only | none yet |
| `itemManagerRef` | global | Rebuild | the `data/items/` tree, recursively (#1232) | see §9 | none yet |
| `equipmentClassManagerRef` | global | Rebuild | `data/*.yaml` equipment content | see §9 | none yet |
| `substanceManagerRef` | global | Rebuild | `data/*.yaml` substance content | see §9 | none yet |
| `infectionManagerRef` | global | Rebuild | `data/*.yaml` infection content | see §9 | `tools/infection_probe.py` |
| `recipeManagerRef` | global | Rebuild | `data/recipes/*.yaml` | see §9 | `tools/craft_probe.py` |
| `locationDefsRef` | global | Rebuild | `data/*.yaml` location content | see §9 | `tools/location_content_probe.py` |
| `lootTableRegistryRef` | global | Rebuild | `data/*.yaml` loot content | see §9 | none yet |
| `tutorialRegistryRef` | global | Rebuild | `data/tutorials/*.yaml` | see §9 | hspec `--match "Tutorial definitions"` |
| `eventStoreRef` | global | Exclude | — | player-event ring buffer plus its session mutation-sequence counter (#1714), explicitly session-only | `tools/injury_log_probe.py` (stream behavior) |
| `notificationCfgRef` | global | Exclude | — | local runtime config (`config/notifications.local.yaml`, #638/#786) | `tools/config_state_probe.py` |
| `notificationOrder` | global | Exclude | — | local runtime config, derived ordering | `tools/config_state_probe.py` |
| `popupQueueRef` | global | Exclude | — | transient popup event queue | none yet |

`engineStateRef` and `worldManagerRef`/`unitManagerRef`/
`buildingManagerRef` are pointer fields whose *pointed-to* records are
classified in their own sections (§2, §3/§4, §5) rather than here.

## 2. `EngineState` (`src/Engine/Core/State.hs:329`) — global, main-thread-private

### EngineState

| Field | Scope | Classification | Restoration dependency | Validation | Test oracle |
|---|---|---|---|---|---|
| `timingState` | global | Exclude | — | frame counters/timing, reset at boot | none yet |
| `graphicsState` | global | Exclude | — | entire Vulkan device/swapchain/pipeline/buffer state; rebuilt at boot. Individual GPU handles are not separately inventoried — they are not gameplay state and none are ever save-file candidates. | none yet |
| `sceneManager` | global | Exclude | — | scene graph, rebuilt by UI/world render setup each session | none yet |

## 3. `WorldManager` / `WorldState` (`src/World/State/Types.hs`)

### WorldManager

`WorldManager` (`:261`) — global:

| Field | Classification | Restoration dependency | Validation | Test oracle |
|---|---|---|---|---|
| `wmWorlds` | Rebuild | `SaveData.sdWorlds` | publication REPLACES the whole set (`World.Load.Publish.publishStagedSession`), never merges — see the resolved-divergence note below | `tools/multiworld_save_probe.py`, `tools/transactional_load_probe.py` |
| `wmVisible` | Rebuild | `SaveData.sdVisiblePages` | none beyond type-correctness | `tools/multiworld_save_probe.py` |
| `wmSelectionGen` | Exclude | none — never written to or read from a save | monotonic within a session: `World.Load.Publish.publishStagedSession` seeds the replacement manager from the outgoing counter rather than from 0, so a page-selection generation is never reissued and a placement binding captured before a load can never read fresh after it | hspec `--match "Build placement page binding"` |
| `wmProjectedWorlds` | Exclude | none — never written to or read from a save | re-synchronised to `wmWorlds`' ids by `settleSelectionProjection` whenever the world queue drains, alongside `wmProjectedVisible`; the two are always projected and settled together | hspec `--match "Build placement page binding"` |
| `wmProjectedVisible` | Exclude | none — never written to or read from a save | re-synchronised to `wmVisible` by `settleSelectionProjection` whenever the world queue drains; while it is ahead it is ahead in the safe direction (an over-predicted change reads as stale) | hspec `--match "Build placement page binding"` |
| `wmProjectedGen` | Exclude | none — never written to or read from a save | re-synchronised to `wmSelectionGen` by `settleSelectionProjection` whenever the world queue drains, so a request whose predicted effect never materialised cannot leave the two permanently apart | hspec `--match "Build placement page binding"` |
| `wmSelectionPending` | Exclude | none — never written to or read from a save | clamped at zero on completion and reset outright by `World.Load.Publish.publishStagedSession`, which is what covers the one path that discards queued commands (`World.Thread.processAuthorizedSave`) | hspec `--match "Build placement page binding"` |

**Resolved**: v82-era behavior diverged from the contract's target here —
the pre-#763 `handleWorldLoadSaveCommand`
(`src/World/Thread/Command/Save/LoadWorld.hs`, #191/#218, deleted)
deliberately kept any live page outside the set the load "owned"
(restored pages + their saved original ids + a prior load's pages)
rather than dropping it, a merge rather than the whole-session
replacement contract §1 requires. Issue #763 (save-overhaul C2)
implemented the target: `World.Load.Publish.publishStagedSession`
registers exactly the staged session's own pages under
`worldManagerRef`, so a page that isn't part of the save being loaded
does not survive publication. See `persistence_contract.md`'s "Resolved
divergence: loading used to merge, not replace" for the full writeup.

### WorldState

`WorldState` (`:43`) — per-page, one instance per live world:

| Field | Classification | Restoration dependency | Validation | Test oracle |
|---|---|---|---|---|
| `wsTilesRef` | Rebuild | `wpsGenParams` + `wpsEdits` | chunk regen followed by edit replay must reproduce the pre-save surface exactly for every edited tile | `tools/world_check.py` (determinism), `tools/multiworld_save_probe.py` |
| `wsCameraRef` | Persist exactly | — | `wpsCameraX`/`wpsCameraY` | `tools/persistence_contract_probe.py` (see §12) |
| `wsTexturesRef` | Exclude | — | runtime GPU handles | none yet |
| `wsGenParamsRef` | Persist exactly | — | `wpsGenParams`; deliberately meaningful seed data (contract §1) | `tools/multiworld_save_probe.py` |
| `wsTimeRef` | Persist exactly | — | `wpsTimeHour`/`wpsTimeMinute` | `tools/save_pause_probe.py` |
| `wsDateRef` | Persist exactly | — | `wpsDateYear`/`wpsDateMonth`/`wpsDateDay` | `tools/flora_growth_probe.py` |
| `wsTimeScaleRef` | Exclude | — | **Implemented by #757/#758**: `World.Save.Snapshot.PageSnapshot` has no time-scale field at all; the temporary adapter fabricates `wpsTimeScale = 1` for every page, unconditionally. | `tools/save_pause_probe.py` |
| `wsResumeScaleRef` | Exclude | — | #1599: the speed this page's clock is to be reinstated at when the CURRENT pause epoch ends (`World.Pause`), `Nothing` outside one. Runtime-only and deliberately unpersistable: a load comes up paused with every page's clock at the default 1.0 (`wsTimeScaleRef` above), and `World.Load.Publish` starts the published session's own epoch from that, so a saved epoch could only ever restore a speed the load policy has already refused to honour. | `tools/save_pause_probe.py`, `Test.Headless.World.PauseSpeed` |
| `wsZoomCacheRef` | Rebuild | loaded chunk data | render cache | none yet |
| `wsQuadCacheRef` | Rebuild | loaded chunk data | render cache | none yet |
| `wsQuadCacheGenRef` | Rebuild | — | cache generation counter | none yet |
| `wsZoomQuadCacheRef` | Rebuild | loaded chunk data | render cache | none yet |
| `wsBgQuadCacheRef` | Rebuild | loaded chunk data | render cache | none yet |
| `wsBakedZoomRef` | Rebuild | loaded chunk data | render cache | none yet |
| `wsBakedBgRef` | Rebuild | loaded chunk data | render cache | none yet |
| `wsInitQueueRef` | Reset to default | — | page load-progress queue, always starts fresh | none yet |
| `wsInitQueueLock` | Exclude | — | #2001: the mutex held while this page's init queue and its `wsLoadPhaseRef` are changed together, so the two `IORef`s move as one step with respect to the other writing thread. A lock is not state a save could hold — it has no value, only an occupancy that is meaningless outside the process — and a fresh or loaded page gets a fresh one. Nothing derives from it; it is never read as gameplay state | hspec `--match "canonical chunk identity"` |
| `wsChunkResidencyRef` | Exclude | — | #2001: this page's chunk-residency owner — for every canonical `World.Chunk.Residency.ChunkKey`, whether it is absent, requested, in flight or resident, plus the page's own generation epoch. Runtime bookkeeping that MIRRORS `wsTilesRef`, which is itself rebuilt (chunks regenerate and edits replay), so there is nothing here a save could restore that the rebuild does not already re-establish: a fresh or loaded page starts with an empty owner, and `World.Load.Stage` re-admits its restored centre (or its whole arena set) and re-registers its initial queue as it rebuilds them. The epoch is deliberately unrestorable — it exists to tell one generation of a page id from the next WITHIN a process, and a saved value would name a generation that no longer exists. Nothing derives from it; it is never read as gameplay state | hspec `--match "canonical chunk identity"`, `--match "init-queue chunks land under canonical keys"` |
| `wsMapModeRef` | Persist exactly | — | `wpsMapMode` (contract §1: visible world/page state) | `tools/persistence_contract_probe.py` (see §12) |
| `wsCursorRef` | Exclude | — | transient hover/cursor state (contract §5 exclusion list) | none yet |
| `wsToolModeRef` | **Reset to default** | — | `DefaultTool`, per #103; matches existing runtime behavior already, this just formalizes it as the contract classification | none yet |
| `wsCursorSnapshotRef` | Exclude | — | transient | none yet |
| `wsLoadPhaseRef` | Reset to default | — | page load-phase tracker, always starts at the initial phase | none yet |
| `wsZoomAtlasRef` | Exclude | — | GPU atlas handle | none yet |
| `wsEditsRef` | Persist exactly | — | `wpsEdits`; core gameplay data (player terrain edits) | `tools/multiworld_save_probe.py`, `tools/world_check.py` |
| `wsChunkEditGenRef` | Exclude | — | per-chunk live-edit generation, the causal fence the world thread rejects a pre-edit sim fluid writeback on (#1596). Transient cross-thread ordering state, meaningful only against the sim's in-flight `scsEditGen` for the same session: a fresh or loaded page starts empty (every chunk at generation 0) and the sim's own `SimState` is likewise rebuilt from 0, so the two sides agree without either being saved. Nothing derives from it; it is never read as gameplay state | hspec `--match "fluid writeback staleness"`, `--match "persistence contract"` |
| `wsOreSurveyRef` | Rebuild | loaded chunk/ore data | zoom-map survey memo, derived | none yet |
| `wsMineDesignationsRef` | Persist exactly | — | `wpsMineDesignations` | `tools/persistence_contract_probe.py` (see §12) |
| `wsGroundItemsRef` | Persist exactly | — | `wpsGroundItems` | `tools/item_instance_probe.py` |
| `wsSpoilRef` | Persist exactly | — | `wpsSpoilPiles` | `tools/persistence_contract_probe.py` (see §12) |
| `wsStructureStageRef` | Exclude | — | explicitly never saved; in-progress structure placement must finish or be abandoned by the snapshot boundary (contract §3) | `tools/location_stamp_idempotent_probe.py` (idempotency, not this state directly) |
| `wsConstructDesignationsRef` | Persist exactly | — | `wpsConstructDesignations` | `tools/construction_probe.py` |
| `wsConstructAttemptRef` | Persist exactly | — | `wpsConstructNextAttempt` (#1844: this page's construction ATTEMPT allocator — the id the next designation admitted here will take). Durable because a designation created after a load must not be able to collide with one the save already holds, and because every delayed lifecycle operation names the attempt it observed. It only ever ADVANCES: a cancellation, a completion, a self-clearing load and a whole-page sweep all leave it where it is, so a retired id can never come to name a live job while delayed work for the first may still be in flight. The load boundary additionally raises it past every id its own designations carry, so a truncated or hand-edited cursor cannot reissue one. | `Test.Headless.Construct.AttemptIdentity` (`--match "construct attempt identity"`), `tools/construction_probe.py` |
| `wsFloraHarvestsRef` | Persist exactly | — | `wpsFloraHarvests` (#1854 re-keyed this off the tile onto the harvested plant's own `FloraInstanceId`, so one berry bush's timer no longer depletes every harvestable co-tenant beside it) | `tools/flora_growth_probe.py`, `tools/foraging_probe.py`, hspec `--match "flora instance identity"` |
| `wsChopDesignationsRef` | Persist exactly | — | `wpsChopDesignations` (#1854 re-keyed this onto `FloraInstanceId` too; the record carries the designated plant's own canonical tile, and `World.Flora.Designation` is the ONE operation that moves this map and the loaded `fiChopDesignated` mirror together) | `tools/chop_probe.py`, hspec `--match "flora instance identity"` |
| `wsPendingChopMigrationRef` | Persist exactly | — | `wpsPendingChopMigration` (#1854: pre-identity TILE-keyed chop designations read out of a save whose chunk was not resident, so no instance could be resolved yet. Persisted rather than dropped so a second save/load cannot silently discard a designation the player made; `World.Flora.Designation` drains an entry the moment its chunk is admitted, resolving it to the single plant the old wood-tagged harvest would have felled, and discarding it with a diagnostic if the resolved tile holds none. DEFERRED, never authoritative: no designation, marker, claim or harvest query may be answered from it) | hspec `--match "flora instance persistence"` |
| `wsPendingFloraHarvestsRef` | Persist exactly | — | `wpsPendingFloraHarvests` (#1854: pre-identity TILE-keyed regrowth timers on exactly the same deferred-never-authoritative terms. Drained by expanding one legacy tile timer onto EVERY harvestable instance on that tile with the same remaining time — the observable behaviour the tile-keyed map used to produce — leaving decorative co-tenants untouched) | hspec `--match "flora instance persistence"` |
| `wsPlantedFloraCursorRef` | Persist exactly | — | `wpsPlantedFloraCursor` (#1854: this page's planted-flora id allocator, carried by `world-edits` v2 beside the `WePlaceFlora` edits whose ids it accounts for. Kept strictly above every planted `FloraInstanceId` the page has issued — `validateWorldEdits` enforces that at the component boundary — so planting after a load can never reissue a live id. GENERATED ids come from a disjoint namespace this allocator does not own) | hspec `--match "flora instance identity"` / `"flora instance persistence"` |
| `wsCraftBillsRef` | Persist exactly | — | `wpsCraftBills` | `tools/craft_bill_probe.py` |
| `wsTransferOrdersRef` | Persist exactly | — | `wpsTransferOrders` (#1246, epic #1013 slice UIT-2A: this page's queue of durable transfer orders — each order's stable per-page id, its acting unit, its endpoint pair and every requested item's own lifecycle state, plus the page-local id allocator, which rides INSIDE the record so a load cannot mint a colliding id). Restored VERBATIM like `wpsCraftBills`, never pruned AT THIS BOUNDARY (#1253 prunes a terminal order LIVE, on the tick that surfaced its outcome, so the store holds only live work by the time a save is taken — the codec itself still restores whatever it was given): a dangling carrier/endpoint/item is a tolerated, non-blocking diagnostic (`World.Save.Integrity.sessionIntegrityWarnings`, logged at both boundaries), while a reference resolving on a DIFFERENT page is a hard error. | `Test.Headless.World.TransferOrders` + `Test.Headless.World.Save.Contract` (`--match "persistence contract"`), `Test.Headless.World.Save.Integrity` |
| `wsPowerNodesRef` | Persist exactly | — | `wpsPowerNodes` | `tools/power_probe.py` |
| `wsPendingContainerSeedsRef` | Exclude | — | #1087: the containers THIS SESSION placed that have not yet reached `Built` — the watch list the building drain re-checks each tick so an instant-built (`bdBuildWork == 0`) storage building seeds known-empty at its real transition rather than at placement. Session-local BY DESIGN: a restored page must start with it empty, which is exactly what stops a loaded already-built container from masquerading as a new construction event. Emptied by demolition/clear-all. | `Test.Headless.Building.Knowledge` (`--match "Container knowledge"`) |
| `wsContainerKnowledgeRef` | Persist exactly | — | `wpsContainerKnowledge` (#1087: the player's last-known view of each container's contents — page-scoped and PLAYER-GLOBAL, never per-unit; its remembered `ItemInstance`s are historical observations, deliberately excluded from `allItemInstanceIds` and live `item_instance` resolution) | `Test.Headless.Building.Knowledge` (`--match "Container knowledge"`) |
| `wsTillDesignationsRef` | Persist exactly | — | `wpsTillDesignations` | `tools/till_probe.py` |
| `wsCropPlotsRef` | Persist exactly | — | `wpsCropPlots` | `tools/crop_probe.py` |
| `wsPlantDesignationsRef` | Persist exactly | — | `wpsPlantDesignations` (#1858: the serialized shape and the `Persist exactly` classification are unchanged — the qualification is on the RESTORE, see the wire row below) | `tools/plant_probe.py`, `Test.Headless.World.CropPlant` (`--match "crop plant invalidation"`) |
| `wsBloodStoreRef` | Exclude | — | blood is transient BY DESIGN, an epic-wide (#603) deliberate contract — see docs/blood_decals.md's "Transience" section and closed issue #884 | `tools/blood_decal_probe.py` (behavior, not persistence) |
| `wsBloodTextureHandlesRef` | Exclude | — | GPU handle cache | none yet |
| `wsIdentityRef` | Persist exactly | — | `wpsIdentity` (#707 player-facing world identity; its optional `wiLanguage` language provenance rides along, #1092, as does its optional `wiEtymology` name-expression source, #1104) | `tools/multiworld_save_probe.py` |

`CursorSnapshot` and `LoadPhase` (`:274`, `:285`) are the types behind
`wsCursorSnapshotRef`/`wsLoadPhaseRef` above, not separate root owners.

## 4. `World.Save.Types` (`src/World/Save/Types.hs`) — the tagged save envelope (#759, save-overhaul B1)

This section was originally the ground truth of what the pre-#759 flat
`[header][SaveData]` format wrote to disk. #759 replaced that framing
with a tagged, checksummed component container
(`World.Save.Envelope`/`.Codec`/`.Types`) — `SaveData`/`WorldPageSave`/
`SaveMetadata` are unchanged Haskell shapes. Under #759 (B1) `SaveData`
rode as a single transitional `"session"` component; #760 (B2, see §10)
retired that component and split gameplay state into independently
versioned components, so `SaveData`/`WorldPageSave` are now only a
transitional IN-MEMORY bridge into the world-thread load path
(`snapshotToSaveData`), NOT any wire contract. `SaveMetadata` still
rides, standalone, as the `"metadata"` component. `SaveHeader`
below describes the envelope's fixed 16-byte framing header, not a raw
`[header][SaveData]` pair. The two rows marked **(new-format target
differs)** are the only classifications in this document that diverge
from what the field's CURRENT code does (a #756-era note, unrelated to
the #759 framing change).

Fields with no non-trivial restoration dependency or validation rule
beyond type-correct deserialization say so plainly (contract §2:
validation is only interesting "where that's non-trivial") rather than
inventing one.

### SaveHeader

`SaveHeader` (`src/World/Save/Types.hs`) — global. Describes the tagged
envelope's fixed 16-byte header; the real codec
(`World.Save.Envelope.Codec`) manipulates these three values as raw
big-endian scalars under its own explicit byte-layout control, not this
record — it exists here purely so this contract's audit
(`tools/persistence_inventory_audit.py`) keeps a stable root-owner
record to classify:

| Field | Classification | Restoration dependency | Validation | Test oracle |
|---|---|---|---|---|
| `shMagic` | Persist exactly | — (read first, before anything else) | must equal the envelope magic (`0x53595241`, same "SYRA" value the pre-#759 format used) or the file is rejected as not a save at all | `save envelope` (`Test.Headless.World.Save.Envelope`) |
| `shEnvelopeVersion` | Persist exactly | — (read second) | must equal `World.Save.Envelope.currentEnvelopeVersion` or load fails clearly, naming the unsupported version (contract §5) — independent of the "session" component's own `currentSaveVersion` | `save envelope` (`Test.Headless.World.Save.Envelope`) |
| `shManifestLength` | Persist exactly | — (read third) | bounded by the documented `elMaxManifestBytes` limit and the actual remaining file length before the manifest is decoded | `save envelope` (`Test.Headless.World.Save.Envelope`) |

### SaveMetadata

`SaveMetadata` (`:297`) — global:

| Field | Classification | Restoration dependency | Validation | Test oracle |
|---|---|---|---|---|
| `smName` | Persist exactly | — | must satisfy `sanitizeSaveName` (it's the save-slot identity) | `tools/multiworld_save_probe.py` |
| `smSeed` | Persist exactly | — | none beyond type-correctness (listing metadata only; the authoritative seed for a page is its own `wpsGenParams`) | `tools/multiworld_save_probe.py` |
| `smWorldSize` | Persist exactly | — | none beyond type-correctness (listing metadata only) | `tools/multiworld_save_probe.py` |
| `smPlateCount` | Persist exactly | — | none beyond type-correctness (listing metadata only) | `tools/multiworld_save_probe.py` |
| `smTimestamp` | Persist exactly | — | none beyond type-correctness (display only) | `tools/persistence_contract_probe.py` (well-formed timestamp string; see §12) |
| `smWorldName` | Persist exactly | the active page's `wpsIdentity` at save time | mirrors that page's identity; `Nothing` for an unnamed world | `tools/multiworld_save_probe.py` |
| `smWorldGloss` | Persist exactly | `smWorldName` | must be `Nothing` whenever `smWorldName` is `Nothing` (a gloss cannot exist without a display name) | `tools/multiworld_save_probe.py` |
| `smAutosave` | Persist exactly | the save REQUEST (`srmAutosave`), not any gameplay state | `True` only for a generation the interval autosave scheduler requested; a `"metadata"` v1 payload has no such field and migrates to `False` (legacy saves are manual saves) via `World.Save.Compat.MetadataV1` | `tools/autosave_probe.py` |

### WorldPageSave

`WorldPageSave` (`:325`) — per-page. Every field below whose
restoration dependency isn't otherwise noted needs only its own page's
prior fields (no cross-page ordering requirement):

| Field | Classification | Restoration dependency | Validation | Test oracle |
|---|---|---|---|---|
| `wpsPageId` | Persist as identity/reference | — | restored verbatim, unchanged (#763: no remap, no collision rename — `assignRestoreIds`/`RestoreIds.hs` are gone, since replacement never collides with anything) | `tools/multiworld_save_probe.py`, `tools/transactional_load_probe.py` |
| `wpsGenParams` | Persist exactly | — | none beyond type-correctness (chunk regen is not re-validated against it at load) | `tools/multiworld_save_probe.py` |
| `wpsCameraX` | Persist exactly | — | none beyond type-correctness | `tools/persistence_contract_probe.py` (see §12) |
| `wpsCameraY` | Persist exactly | — | none beyond type-correctness | `tools/persistence_contract_probe.py` (see §12) |
| `wpsCameraZoom` | Persist exactly | — | none beyond type-correctness | `tools/persistence_contract_probe.py` (see §12) |
| `wpsCameraFacing` | Persist exactly | — | none beyond type-correctness | `tools/persistence_contract_probe.py` (see §12) |
| `wpsTimeHour` | Persist exactly | — | none beyond type-correctness (not range-checked against 0-23 at load) | `tools/save_pause_probe.py` |
| `wpsTimeMinute` | Persist exactly | — | none beyond type-correctness (not range-checked against 0-59 at load) | `tools/save_pause_probe.py` |
| `wpsDateYear` | Persist exactly | — | none beyond type-correctness | `tools/flora_growth_probe.py` |
| `wpsDateMonth` | Persist exactly | — | none beyond type-correctness (not range-checked against the world's calendar at load) | `tools/flora_growth_probe.py` |
| `wpsDateDay` | Persist exactly | — | none beyond type-correctness (not range-checked against the world's calendar at load) | `tools/flora_growth_probe.py` |
| `wpsTimeScale` | Exclude (legacy field fabricated by the adapter) | n/a | n/a | **Implemented by #757, cleanly modeled by #758** (contract §1, "the pre-save speed is not persisted"): the field is still WRITTEN — v88's positional format can't drop it — but `World.Save.Snapshot.Adapter.snapshotToSaveData` always fabricates `1`, never a captured value (#757 already hardcoded this at the old inline call site; #758 gives it an explicit, tested home). `tools/save_pause_probe.py` was updated by #757 to match. |
| `wpsMapMode` | Persist exactly | — | none beyond type-correctness | `tools/persistence_contract_probe.py` (see §12) |
| `wpsToolMode` | Reset to default (legacy field fabricated by the adapter) | n/a (reset, not restored) | always `DefaultTool` regardless of the stored value | v82 writes the field, but load already ignores it and resets to `DefaultTool` per #103 — a currently-dead field. **#758**: `World.Save.Snapshot.PageSnapshot` has no tool-mode field at all; the adapter fabricates `DefaultTool` for every page, so the "no captured value, only a fixed default" contract is now explicit at the type level, not just at load time. |
| `wpsEdits` | Persist exactly | `wpsGenParams` (edits replay onto regenerated terrain) | replayed edits must reproduce the pre-save surface exactly for every edited tile | `tools/multiworld_save_probe.py`, `tools/world_check.py` (determinism) |
| `wpsMineDesignations` | Persist exactly | referenced tile coordinates must be within the page | a claimant referencing a unit that failed to restore is not currently detected/cleared | none yet |
| `wpsConstructDesignations` | Persist exactly | referenced tile coordinates must be within the page | same claimant caveat as `wpsMineDesignations` | `tools/construction_probe.py` |
| `wpsConstructNextAttempt` | Persist exactly | `wpsConstructDesignations` (the allocator is raised past every id its own designations carry as the slice is applied) | must sit strictly above every attempt id the page's designations hold — enforced at the load boundary rather than trusted | `Test.Headless.Construct.AttemptIdentity` (`--match "construct attempt identity"`) |
| `wpsGroundItems` | Persist exactly | — | instance ids must be below `sdNextItemInstanceId` — validated at SAVE time over the whole session (`World.Save.Snapshot`'s `itemAllocatorErrors`), which is what the plain-assignment restore relies on | `tools/item_instance_probe.py` |
| `wpsSpoilPiles` | Persist exactly | referenced tile coordinates must be within the page | none beyond type-correctness | `tools/persistence_contract_probe.py` (see §12) |
| `wpsBuildings` | Persist exactly | `bmDefs` must already have every referenced building def (contract §4: missing def fails load) | referenced def names must resolve | `tools/multiworld_save_probe.py` |
| `wpsUnits` | Persist exactly | `umDefs` must already have every referenced unit def (contract §4: missing def fails load) | referenced def names must resolve | `tools/multiworld_save_probe.py` |
| `wpsUnitSimStates` | Persist exactly | `wpsUnits` (sim state is keyed by `UnitId`, restored after unit instances) | every sim-state key should correspond to a restored unit id (an orphaned key is not currently detected) | `tools/movement_probe.py` (post-load) |
| `wpsFloraHarvests` | Persist exactly | the plants the ids name are regenerated with the chunk (flora placement is deterministic), so nothing has to be restored first | #1854: keyed by `FloraInstanceId`, not by tile. A timer whose plant no longer exists is tolerated and unreachable rather than a failure — the same class as a dangling craft bill; removing a plant clears its entry at the point of removal (`World.Flora.Designation.replaceChunkForgettingFlora`), so one surviving means the plant outlived the save | `tools/flora_growth_probe.py`, hspec `--match "flora instance identity"` |
| `wpsChopDesignations` | Persist exactly | as `wpsFloraHarvests` | #1854: keyed by `FloraInstanceId`; the record's own `chGX`/`chGY` are canonical tile coords and must be within the page. Same claimant caveat as `wpsMineDesignations` | `tools/chop_probe.py`, hspec `--match "flora instance identity"` |
| `wpsPendingChopMigration` | Persist exactly | referenced tile coordinates must be within the page | #1854: DEFERRED legacy migration state, never a second authority — see `wsPendingChopMigrationRef` above | hspec `--match "flora instance persistence"` |
| `wpsPendingFloraHarvests` | Persist exactly | referenced tile coordinates must be within the page | #1854: as `wpsPendingChopMigration` | hspec `--match "flora instance persistence"` |
| `wpsPlantedFloraCursor` | Persist exactly | — | #1854: must be strictly above every planted `FloraInstanceId` in this page's `wpsEdits`, checked by `World.Save.Component.Page.validateWorldEdits` at the component boundary (the same shape as `world-activity`'s ground-item allocator clause). A `world-edits` v1 save records no ids at all, so `applyWorldEdits` assigns them deterministically — ascending canonical chunk coordinate, then each chunk's own oldest-first log order — and initializes this cursor above all of them | hspec `--match "flora instance persistence"` |
| `wpsCraftBills` | Persist exactly | referenced station building must already be restored (`wpsBuildings`) | `cbStation`/`cbClaimant` absent from the whole session are tolerated (#758); either resolving on a DIFFERENT page than the bill is hard-rejected by `World.Save.Integrity.sessionIntegrityErrors` (#764) | `tools/craft_bill_probe.py`, `Test.Headless.World.Save.Integrity` |
| `wpsPowerNodes` | Persist exactly | referenced host building must already be restored (`wpsBuildings`) | `pnBuilding` absent from the whole session is tolerated (#758); resolving on a DIFFERENT page than the node is hard-rejected by `World.Save.Integrity.sessionIntegrityErrors` (#764) | `tools/power_probe.py`, `Test.Headless.World.Save.Integrity` |
| `wpsContainerKnowledge` | Persist exactly | referenced container building must already be restored (`wpsBuildings`) | a record whose `BuildingId` is absent from the page is a tolerated, non-blocking diagnostic SCRUBBED at load (`World.Load.Stage`), never a failure — unlike a bill/node, a memory of a vanished container has no player-facing surface that could ever clear it. Remembered item DEF NAMES follow the ordinary missing-item-definition contract (`missingItemDefReferences`); remembered INSTANCE IDS are exempt from the allocator/duplicate/live-reference checks by design (#1087) | `Test.Headless.Building.Knowledge` (`--match "Container knowledge"`) |
| `wpsTransferOrders` | Persist exactly | — | #1246: this page's durable transfer orders, carried by the OPTIONAL `transfer-orders` component. Restored straight into `wsTransferOrdersRef`, verbatim — a dangling carrier/endpoint/item is logged and kept, never pruned at load and never a load failure. Since #1253 a terminal order is pruned LIVE before the save, so one appearing here at all means a save landed inside that sub-tick window. Appended for save v93 (the transitional in-memory bridge marker; the on-disk component carries its own v1). | `Test.Headless.World.TransferOrders` (`--match "persistence contract"`) |
| `wpsTillDesignations` | Persist exactly | referenced tile coordinates must be within the page | same claimant caveat as `wpsMineDesignations` | `tools/till_probe.py` |
| `wpsCropPlots` | Persist exactly | referenced tile coordinates must be within the page | none beyond type-correctness | `tools/crop_probe.py` |
| `wpsPlantDesignations` | Persist exactly | referenced tile coordinates must be within the page | same claimant caveat as `wpsMineDesignations`. #1858 adds one post-restore reconciliation, the `wpsContainerKnowledge` pattern applied to terrain rather than buildings: tilled soil is a CONTINUOUS validity requirement, so a restored record whose tile is RESIDENT and no longer tilled soil is SCRUBBED — at `World.Load.Stage` for the terrain that load reconstructs, and again as each further chunk publishes (`World.Plant.Validate`). Never a load failure, and never inferred from silence: a record whose chunk is not resident is UNKNOWN and is retained until its terrain resolves | `tools/plant_probe.py`, `Test.Headless.World.CropPlant` (`--match "crop plant invalidation"`) |
| `wpsIdentity` | Persist exactly | — | none beyond type-correctness | `tools/multiworld_save_probe.py` |

### SaveData

`SaveData` (`:438`) — global. `sdMetadata`/`sdGameTime`/`sdEnginePaused`/
`sdTexPalette`/`sdNextItemInstanceId` are restored first and unconditionally
(step 0 of `handleWorldLoadSaveCommand`), before any page — every other
field either has no cross-field dependency or is noted below:

| Field | Classification | Restoration dependency | Validation | Test oracle |
|---|---|---|---|---|
| `sdMetadata` | Persist exactly | — | none beyond type-correctness | `tools/multiworld_save_probe.py` |
| `sdGameTime` | Persist exactly | — | none beyond type-correctness | `tools/save_pause_probe.py` |
| `sdEnginePaused` | Persist exactly | — | authoritative over any Lua-side copy (contract, §7 `pause` module) | `tools/save_pause_probe.py` |
| `sdTexPalette` | Persist exactly | must restore before any page replays a `WeSetStructure` edit (palette-id → path resolution) | none beyond type-correctness | `tools/persistence_contract_probe.py` (see §12) |
| `sdNextItemInstanceId` | Persist exactly | — | ASSIGNED onto the allocator on load, never max'd against the replaced session (#67/#763); post-load item creation still can't collide with a loaded id, because the snapshot validator refuses to save a session holding one at or above this value | `tools/item_instance_probe.py` |
| `sdActivePage` | Persist as identity/reference | must name a page present in `sdWorlds` (falls back to the first page if not, per `activeWorldPage`) | resolves to a real restored page | `tools/multiworld_save_probe.py` |
| `sdVisiblePages` | Persist as identity/reference | pages must exist post-restore | none beyond type-correctness | `tools/multiworld_save_probe.py` |
| `sdWorlds` | Persist exactly (container) | — | each entry independently follows `WorldPageSave`'s rules above | `tools/multiworld_save_probe.py` |

## 5. Gameplay managers

`UnitManager`, `BuildingManager` and `UnitThreadState` ARE in
`ROOT_RECORDS` (#1703): each is reached from `EngineEnv` through a bare
IORef pointer — `unitManagerRef`/`buildingManagerRef`/`utsRef`, the
fields that hang directly off `EngineEnv`, are classified in §1 as
`Rebuild` pointers that delegate the real decisions onto these records'
own fields — so the audit scans all three directly and a field added to
any of them fails until it carries a classification row here. Before
#1703 only the pointers were scanned, which left the delegation
landing in unenforced territory.

Their own `### ` owner headings below are therefore load-bearing, not
merely navigational: `parse_classified_names` scopes classification per
owner heading, so a row moved out from under one of these headings stops
counting.

### UnitManager

`UnitManager` (`src/Unit/Types/Manager.hs:37`) — global:

| Field | Classification | Restoration dependency | Validation | Test oracle |
|---|---|---|---|---|
| `umDefs` | Rebuild | `data/units/*.yaml` | none beyond type-correctness | see §9 |
| `umInstances` | Persist exactly | via `UnitSnapshot`/`wpsUnits`, needs `umDefs` resolved first | missing species def fails load (contract §4) | `tools/multiworld_save_probe.py` |
| `umSelected` | Exclude | — | selections are cleared on load (contract §1) | none yet |
| `umNextId` | Persist exactly | `usnNextId` | must exceed every restored `UnitId` so post-load spawns can't collide | `tools/item_instance_probe.py`-style id-collision reasoning (no dedicated probe) |

### BuildingManager

`BuildingManager` (`src/Building/Types.hs:231`) — global:

| Field | Classification | Restoration dependency | Validation | Test oracle |
|---|---|---|---|---|
| `bmDefs` | Rebuild | `data/buildings/*.yaml` | none beyond type-correctness | see §9 |
| `bmInstances` | Persist exactly | via `BuildingSnapshot`/`wpsBuildings`, needs `bmDefs` resolved first | missing building def fails load (contract §4) | `tools/multiworld_save_probe.py` |
| `bmNextId` | Persist exactly | — | must exceed every restored `BuildingId` so post-load spawns can't collide | `tools/persistence_contract_probe.py` (see §12) |
| `bmSelected` | Exclude | — | selections are cleared on load (contract §1) | none yet |

### UnitThreadState

`UnitThreadState` (`src/Unit/Sim/Types.hs:231`) — global. The unit
thread's own simulation store, reached from `EngineEnv` via `utsRef`
(§1). The POINTER stays `Rebuild` — the IORef and its container are
freshly allocated at boot and repopulated on load — while the state it
holds is persisted, which is the distinction this heading exists to
record:

| Field | Classification | Restoration dependency | Validation | Test oracle |
|---|---|---|---|---|
| `utsSimStates` | Persist exactly | via `UnitSimDTO`/the `unit-sim` component (§10) and `wpsUnitSimStates`, needs the restored `UnitInstance`s (`umInstances`) resolved first — a sim state keyed by a `UnitId` no unit carries has nothing to steer | per-unit pos/pose/target/path/deadlines restore as saved; in-flight move targets keep the hazard policy their bytes were written under (see `UnitSimDTO` in §10) | `tools/movement_probe.py` (post-load steering sanity); `tools/persistence_contract_probe.py` (see §12) |

### UnitInstance (reset-on-load fields)

`UnitInstance` fields explicitly dropped by `fromUnitSnapshot`
(`src/World/Save/Types.hs:756`) — per-unit, reset rather than persisted.
Not in `ROOT_RECORDS` (these are individual fields WITHIN
`UnitInstance`, which is itself reached only via `umInstances` above,
already covered — the audit scans the three manager records themselves,
not the records they point at):

| Field | Owner | Scope | Classification | Restoration dependency | Validation | Test oracle |
|---|---|---|---|---|---|---|
| `uiLastAttackerUid` | `src/World/Save/Types.hs:756` | per-unit | Reset to default | — | always `Nothing` post-load | none yet |
| `uiLastAttackerAt` | `src/World/Save/Types.hs:756` | per-unit | Reset to default | — | always `Nothing` post-load | none yet |
| `uiAnimOverride` | `src/World/Save/Types.hs:756` | per-unit | Reset to default | — | always cleared post-load | `tools/combat_anim_probe.py` (behavior, not persistence) |
| `uiFrozen` | `src/World/Save/Types.hs:756` | per-unit | Reset to default | — | always `False` post-load | none yet |
| `uiForceLoop` | `src/World/Save/Types.hs:756` | per-unit | Reset to default | — | always `False` post-load | none yet |
| `uiClimbDest` | `src/World/Save/Types.hs:756` | per-unit | Reset to default | — | always `Nothing` post-load | `tools/movement_probe.py` (behavior, not persistence) |
| `uiTrailState` | `src/World/Save/Types.hs:910` | per-unit | Reset to default | — | always `Nothing` post-load (ongoing-bleeding emitter accumulator — the #882 trail gates AND the #883 pool cluster anchor/layer count, which share one record and are transient together) | `tools/bleeding_trail_probe.py` (behavior, not persistence); hspec `--match "a NON-EMPTY pool cluster resets on load"` |

Other gameplay managers (item defs, ground items, and every per-page
designation/job manager) are already classified in §3/§4 by their
`ws*Ref`/`wps*` field — they are per-`WorldState` root owners, not
separate global managers, so they are not repeated here:

`ItemManager.imDefs` → see §9. `GroundItems` (`gisNextId`, `gisItems`) →
`wsGroundItemsRef`/`wpsGroundItems` in §3/§4.
`MineDesignations`/`ConstructDesignations`/`ChopDesignations`/
`TillDesignations`/`PlantDesignations` → their respective `ws*Ref`/
`wps*` rows in §3/§4. `CraftBills`/`CraftBill` (`cbClaimant`, `cbWorking`,
`cbPaused`, progress — durable facts, never queue-resident, see
contract §3) → `wsCraftBillsRef`/`wpsCraftBills`. `RecipeManager.rmDefs`
→ see §9. `PowerNodes`/`PowerNode` → `wsPowerNodesRef`/`wpsPowerNodes`.

`BloodStore` (`src/Blood/Types.hs:444`, `bstPool`/`bstDecals`) → see
`wsBloodStoreRef` in §3 (Exclude).

## 6. Worker-thread-owned state (not reachable from `EngineEnv`/`WorldState`)

These are **not** root state owners under the contract §2 definition
(nothing reaches them from `EngineEnv`/`WorldState`), but are inventoried
per issue requirement 3 ("worker-thread-owned simulation state") for
completeness. The audit does not scan these files; a change here does
not require an inventory update to pass CI, though it should still get
one for documentation's sake.

### Worker-thread-owned state

| Item | Owner | Scope | Classification | Restoration dependency | Validation | Test oracle |
|---|---|---|---|---|---|---|
| `World.Thread`'s `lastTimeRef` | `src/World/Thread.hs:34` | global (one dt clock for the whole world thread, not per-page) | Exclude | — | dt clock, reset at boot | none yet |
| `Unit.Thread`'s `lastTimeRef` | `src/Unit/Thread.hs:37` | global | Exclude | — | dt clock, reset at boot | none yet |
| `Combat.Thread`'s local `tick` counter | `src/Combat/Thread.hs:51` | global | Exclude | — | resets to 0 every restart, gates wound-tick rate only (contract §1: thread scheduling not persisted) | none yet |
| `Sim.Thread`'s `simStateRef` (`SimState`) | `src/Sim/Thread.hs:62`, `src/Sim/State/Types.hs` | global — ONE `SimState` for the whole thread, containing a per-page `ssWorlds :: HashMap WorldPageId SimWorldState` map | Rebuild | loaded chunk tile/fluid data (`wsTilesRef`) | fresh `SimChunkState` derives from chunk tile/fluid data as each chunk reactivates post-load; settled results already live in `wsTilesRef`/`wsEditsRef`, this is pure active-simulation scratch space | `tools/world_check.py` (fluid settle behavior) |
| `SimWorldState`/`SimChunkState` entries within `simStateRef` | `src/Sim/State/Types.hs` | per-page (`SimWorldState`) / per-chunk (`SimChunkState`, nested within a page's entry) | Rebuild | same as `simStateRef` | same reasoning, at finer granularity — a page/chunk not currently active simply has no entry, rebuilt on activation | `tools/world_check.py` |
| `Lua.Thread`'s `lbsLuaState` (the Lua VM) | `src/Engine/Scripting/Lua/Thread.hs`, `src/Engine/Scripting/Lua/Types.hs:35` | global (one Lua VM for the whole engine) | Rebuild | boot-time `loadScript` sequence, then §7's `saveModules.prepareLoad`/`applyAll` | the VM CONTAINER itself is rebuilt fresh by re-running `loadScript` at boot — this single "Rebuild" classification is for that container, not a blanket claim about everything inside it. The specific durable SLICES of its global tables are separately classified, one label each: §7's three `saveModules`-registered persistent components are `Persist exactly` in their own right, its one `registerResetHook`-registered module is `Reset to default`; everything else in the VM's global state is `Exclude` by omission (never touched by save/load). | `tools/lua_orphan_prune_probe.py` |
| `Lua.Thread`'s `lbsScripts` (registered scripts + tick schedule) | `src/Engine/Scripting/Lua/Types.hs:22` | global | Exclude | — | rebuilt by the boot-time `loadScript` sequence | none yet |
| `Lua.Thread`'s `lbsNextScriptId` | `src/Engine/Scripting/Lua/Types.hs` | global | Exclude | — | rebuilt at boot | none yet |
| `Engine.Input.Thread` | `src/Engine/Input/Thread.hs` | global | Exclude | — | no persistent thread-local state at all — local IORefs are recreated per-event inside handler scope, so "Exclude" here documents the absence rather than a specific field | none yet |
| `Lua.Thread`'s debug-console `debugQueue` (`TQueue DebugCommand`, `dcCommand`/`dcResponse`) | `src/Engine/Scripting/Lua/DebugServer.hs:24`, `src/Engine/Scripting/Lua/Thread.hs:107` | global (one debug TCP server for the whole engine) | Exclude | — | queued-but-unprocessed debug-console commands (and their response `MVar`s) at a snapshot boundary are cancelled, not persisted or replayed — an open debug-shell session has no gameplay meaning (contract §1/§5: exclude open debug-shell contents); a client mid-command at that instant simply never gets a response, same as if the engine had been killed | none yet |

## 7. Lua persistence registry (`scripts/lib/save_modules.lua`)

Since issue #761 (save-overhaul B3) this is a versioned, scoped,
fail-fast COMPONENT registry, not an opaque `name -> blob` map: each
persistent module declares a schema version, required/optional status,
dependencies, and explicit snapshot/decode/validate/apply functions
(`World.Save.Component.Types.ComponentCodec`'s contract, mirrored in
Lua). Every registered persistent component rides as its own
dynamically-added envelope component (`"lua.<module>"`), independent of
any other Lua-owned or Haskell-owned component's checksum/version.
Exactly three modules call `saveMods.register(...)`; each is a root
state owner under the contract §2 definition, and the audit scans
`scripts/` for these call sites directly and checks each registered name
against the classifications below.

A module with no durable state calls `saveMods.registerResetHook(id,
resetFn)` instead (`unit_resources` below) — this is NOT a save
component (no version, no envelope entry, not scanned by the audit),
just a callback run on every load. `pause` no longer registers at all
(neither `register` nor `registerResetHook`) — see its row below.

**Exit to Menu is a SECOND session-replacement path, and neither
mechanism above sees it** (#1610). It destroys every world and resets the
Haskell entity managers without loading anything, so `applyAll` never
runs, no reset hook fires, and no `onSaveLoaded` broadcast is emitted.
Session-scoped Lua state is cleared there through its own registry,
`scripts/lib/session_teardown.lua` — deliberately independent of this
one: a separate id namespace, its own invocation path
(`pauseMenu.onExitToMenu` and nowhere else), and no interaction with
`registry` / `resetHooks` / `applyAll` / `onSaveLoaded`. That
independence is what lets the durable `unit_ai` and `building_spawn`
COMPONENTS opt in — `registerResetHook` refuses an id a save component
already owns — without touching their registration, payload version, or
load rollback semantics. Five modules register today: `unit_ai`,
`building_spawn`, `build_tool`, `mine_tool`, `transfer_session`. The
boundary runs BEFORE `world.destroyAll` so every callback still sees a
live session, each callback is independently `pcall`ed, and it adds
nothing at all to the load path. It is NOT scanned by
`tools/persistence_inventory_audit.py`: it registers no save component
and clears only state already classified below (or in §8), so a
registration there is a lifecycle detail of an existing classification,
never a new root state owner.

### Lua persistence registry

| Module | Owner | Scope | Classification | Restoration dependency | Validation | Test oracle |
|---|---|---|---|---|---|---|
| `unit_ai` | `scripts/unit_ai.lua` | global (per-id state keyed inside the component payload) | Persist exactly (versioned component, v7) | live unit ids must already be restored (`umInstances`) | component-local: payload must be a table keyed by positive-integer unit ids mapping to state tables. Applied PER ENTITY since #900: each row is resolved against the restored session's own unit set (`saveModules.applyEntityRows`, fed by the entity context `prepareLoad` stashes for `applyAll`), a row whose unit is absent is dropped with a diagnostic rather than applied, and `aiState` ends up holding exactly the applicable rows — never merged into live state, so a reused session-global id cannot inherit the previous session's row. The `_preLoadState` snapshot/restore dance and its off-page-preservation branch are retired; `onSaveLoaded` keeps only the orphan prune and the nested-reference scrub. Since #1589 that scrub (in `scripts/unit_ai_reconcile.lua`) covers EVERY family `unit_ai_ref_schema.lua`'s `REF_SCHEMA` declares — the flat unit/building fields, the treat/delivery claims, `craftJob`, `repairJob`, `pickupOrder`, a ground `forageTarget`, and the `forageLoot`/`harvestLoot` collections (`repairJob` declaring three edges since #1737: its `instanceId`, its optional station `bid`, and the optional page-local `groundGid` a ground-sourced target is still lying at) — because the reference walk, the wire wrap/unwrap, the tag validator and the scrub are all derived from that one table, so a declared family cannot be silently skipped by one of them. `knownLocations` is the one deliberate exception, keeping the specialized page-qualified scrub and separate count `unit_ai_locations.lua` owns. Per-page ids (`craft_bill`, `ground_item`) are resolved against the OWNING unit's page, using the `LoadReconcileContext` the engine appends to the `onSaveLoaded` broadcast (`World.Save.Payload` → `World.Load.Stage`/`Publish` → `Engine.Scripting.Lua.Thread.Dispatch`), never a live active-page query; a missing or malformed context fails the reconciliation visibly rather than falling back to one. A dangling reference remains a tolerated non-blocking diagnostic at both boundaries — clearing it here is what that tolerance is promised on SESSION LIFECYCLE (#1610): the durable payload is unchanged, but the LIVE table is also emptied in place on Exit to Menu, through `scripts/lib/session_teardown.lua`'s `unit_ai` registration (made in `scripts/unit_ai_save.lua` beside the component itself). That path loads nothing, so no row is restored after it and nothing is cleared twice for one load; `unitAi.update` additionally holds off until the next session activates, because `UnitClearAll` is still queued when `onExitToMenu` returns and `ensureState` would otherwise rebuild the rows from the destroyed session's units | `tools/lua_orphan_prune_probe.py`, `Test.Headless.Lua.SaveModules` |
| `building_spawn` | `scripts/building_spawn.lua` | global (per-id state keyed inside the component payload) | Persist exactly (versioned component, v3) | live building ids must already be restored (`bmInstances`) | same per-entity application as `unit_ai`, resolved against the restored building set; NOTE the roster-countdown itself is NOT here — it lives on `BuildingInstance` and is covered under `wpsBuildings` in §4. SESSION LIFECYCLE (#1610): same as `unit_ai`'s — the live `state` table is emptied in place on Exit to Menu via a `session_teardown` registration made beside the component in `buildingSpawn.init`, and `buildingSpawn.update` holds off through the `BuildingClearAll` drain window so `ensureState` cannot rebuild the destroyed session's rows | `tools/lua_orphan_prune_probe.py`, `Test.Headless.Lua.SaveModules` |
| `tutorial_progress` | `scripts/tutorial_progress.lua` | global (whole save session — not per page, portal, or unit; keys on authored YAML objective ids, not runtime entity ids) | Persist exactly (versioned component, v1) | none — `deps = {}`; authored tutorial ids resolve against the boot-loaded tutorial registry (#957), not against any restored entity set | component-local `validate()`: the payload is a table whose ONLY field is `completed`, a dense array of unique non-empty objective id strings — a non-table payload, an unknown field, a hole/non-integer key, a non-string or empty id, or a repeated id is a hard load failure, as is an unsupported schema version (`inputVersions = {1}`). Registered `required = false` with a `default()` of `{ completed = {} }`, so a supported save written before this component existed loads with FRESH tutorial progress rather than failing. Restoration: `apply()` replaces the durable completed set wholesale, clears the live subobjective checks (never persisted — recomputed each session), then `reconcile()` drops any completed id the currently loaded tree no longer defines as a full objective, as a tolerated non-blocking diagnostic (persistence contract's dangling-reference rule), never a load failure | `Test.Headless.Lua.TutorialProgress` (`--match "Tutorial progress"`) |
| `unit_resources` | `scripts/unit_resources.lua` | global (per-id cache) | Reset to default | — | reset hook (`registerResetHook`, not a save component); `alerts.resetOnLoad()` must clear the per-unit alert-debounce cache on every load, including a load with no data for this module at all — deliberately never persisted so a reused unit id (post `umNextId` rewind) can't inherit stale suppression state | none yet |
| `unit_ai_claims` | `scripts/unit_ai_claims.lua` (registered from `scripts/unit_ai_save.lua`; owners live in `unit_ai_dig`/`_chop`/`_construct`/`_farm`/`_repair` and `unit_ai_encounter`) | global, session-wide transient coordination | Reset to default | — | reset hook (`registerResetHook`, not a save component, and deliberately not under the persistent component's `unit_ai` id). `claimsLib.resetAll()` empties all TEN registries in place on every successful load: five coordinate work claims, `repairClaims`, `repairPriority`, and #916's three same-tick encounter overlays (`localEpisodeActive`, `localEpisodeDisengaged`, `localParticipation`). None lives in `aiState`. The work tables must not survive rewound entity ids/clocks; the encounter overlays must not override the newly loaded encounter's persisted episode/occupant state when page and instance ids are reused. Content-data caches remain deliberately excluded. Session teardown reaches the same registered family through the existing unit-AI lifecycle. | `Test.Headless.Lua.UnitAiLoadReset` (`--match "unit AI load reset"`), `tools/construction_probe.py` |
| `transfer_session` | `scripts/transfer_session.lua` | global (at most ONE live Mode A escort session for the whole process) | Reset to default | — | reset hook (`registerResetHook`, not a save component): a load REPLACES the session, so the panels and the uids `M.active` names describe a session that no longer exists. The hook runs the SAME coupled teardown every other path runs, with `unitsAreStale = true` — the one disposition that does NOT stop the units it recorded, because after a load those uids name whatever the restore put on them. The DURABLE half of a player transfer is elsewhere and unaffected: #1246's `transfer-orders` component in §4/§6. SESSION LIFECYCLE (#1610): Exit to Menu clears it too — previously the hand-listed `pcall` #1014 added to `pauseMenu.onExitToMenu`, now a `scripts/lib/session_teardown.lua` registration, made at module scope because the `pcall` it replaces `require`d this module unconditionally — a caller holding it loaded but never initialized still had its session cleared, and still must. That one is the ORDINARY teardown (no `unitsAreStale`): it runs before `world.destroyAll`, so the units it holds still exist and must actually be stopped — the ordering `docs/engine_contracts.md` §Player transfers requires | `Test.Headless.Lua.SessionTeardown` (`--match "session teardown clears Lua entity state"`) |
| `pause` (`paused` field) | `scripts/pause.lua` | global | Exclude | — | no registration of any kind (requirement 5) — `pause.paused` is an in-memory transition-detection hint only, never read for real logic; `enginePausedRef`/`sdEnginePaused` is authoritative (see §1) | `tools/save_pause_probe.py` |
| `pause` (`prevTimeScale` field) | `scripts/pause.lua` | global | Exclude | — | since #1599 a diagnostic MIRROR only: the authoritative resume speed is the engine-side pause epoch (`wsResumeScaleRef` in §3), which is the only place that can observe a pause imposed without running Lua. `pause.onSaveLoaded` still resets it to `1.0` regardless of what an older save contained — a loaded session resumes at default speed (contract §1, "the pre-save speed is not persisted") | `tools/save_pause_probe.py`, `tools/transactional_load_probe.py` |

## 8. Camera / world-view / UI / tool / selection state

None of these are in `ROOT_RECORDS` (they're either not reachable from
`EngineEnv`/`WorldState` at all — the two Lua-side rows — or they
duplicate a field already classified by its own owner elsewhere, cross-
referenced below rather than re-audited here).

### Camera / world-view / UI / tool / selection state

| Item | Owner | Scope | Classification | Restoration dependency | Validation | Test oracle |
|---|---|---|---|---|---|---|
| `WorldCamera` (`wcX`, `wcY`) | `src/World/Render/Camera/Types.hs:34` | per-page | Persist exactly | — | none beyond type-correctness | source of `wpsCameraX`/`wpsCameraY`, see §3/§4; none yet |
| `Camera2D` (`camPosition`, `camVelocity`, `camZoom`, `camZoomVelocity`, `camRotation`, `camDragging`, `camDragOrigin`, `camZSlice`, `camZTracking`) | `src/Engine/Graphics/Camera.hs:39` | global | Exclude | the active page's `WorldCamera` + `wpsCameraZoom`/`wpsCameraFacing` | session-only render camera; re-synced on load, not itself a source of truth | none yet |
| `Camera2D`'s `camFacing` | `src/Engine/Graphics/Camera.hs:39` | global | Persist as identity/reference | `wpsCameraFacing` | mirrors `wpsCameraFacing`, the true source of truth | none yet |
| `UICamera` (`uiCamWidth`, `uiCamHeight`) | `src/Engine/Graphics/Camera.hs:66` | global | Exclude | window size | derived from window size at boot | none yet |
| `CursorState` | `src/World/Cursor/Types.hs:9` | per-page | Exclude | — | already covered as `wsCursorRef` in §3 (contract §1 exclusion list: hover state) | none yet |
| `UIPageManager` | `src/UI/Types.hs:321` | global | Exclude | — | already covered as `uiManagerRef` in §1 | none yet |
| `FocusManager` | `src/UI/ShellFocus.hs:52` | global | Exclude | — | already covered as `focusManagerRef` in §1 | none yet |
| `ToolMode` (per-page) | `src/World/Tool/Types.hs:12` | per-page | Reset to default | — | already covered as `wsToolModeRef`/`wpsToolMode` in §3/§4 | none yet |
| `UnitManager.umSelected` | `src/Unit/Types.hs:626` | global | Exclude | — | already covered in §5 | none yet |
| `BuildingManager.bmSelected` | `src/Building/Types.hs:183` | global | Exclude | — | already covered in §5 | none yet |
| `dragSelect` state (`state`, `startX/Y`, `currX/Y`, `page`) | `scripts/unit_drag_select.lua:20` | global (Lua module singleton) | Exclude | — | transient UI gesture FSM, not registered with `saveModules` — a drag in progress at save time is simply abandoned | none yet |
| Tool-script anchor/preview state (`mine_tool.lua`, `build_tool.lua`, `chop_tool.lua`, `till_tool.lua`, `plant_tool.lua`) | `scripts/*_tool.lua` | per-page (mirrors engine-side anchor fields) | Exclude | — | transient designation-in-progress UI state, not registered with `saveModules`. SESSION LIFECYCLE (#1610): `build_tool`'s armed placement (#82) and `mine_tool`'s pending anchor (#102) are cleared on Exit to Menu through `scripts/lib/session_teardown.lua` — previously two hand-listed `pcall`s in `pauseMenu.onExitToMenu`, now registrations. Both run BEFORE `world.destroyAll`, which is what lets `mine_tool.cancel` reach the engine-side anchor of a world that still exists. The other three tools hold no cross-session state and register nothing | none yet |
| `debugOverlay` module state (per-mode `entries`/`listVisible`/`buttonId`, each mode's `armedField` selection, `modeOrder`, hit-test layout state) | `scripts/debug.lua:43-73` | global (Lua module singleton) | Exclude | — | transient debug-overlay UI state (open panel, armed spawn/edit selection, mode list) — not registered with `saveModules`; explicitly required Exclude by contract §5 ("debug overlays"). Already reset to fresh per-mode defaults at `require`/reload time (`scripts/debug.lua:69-73`), independent of save/load. | none yet |

## 9. Content-definition registries (current content, not persisted state)

All Rebuild — loaded fresh from YAML (or built in) at boot, referenced
by id/name from persisted instances rather than embedded. A missing
definition a persisted instance refers to fails loading per contract §4.

### Content-definition registries

| Registry | Owner | Scope | Classification | Restoration dependency (Loader) | Validation | Test oracle |
|---|---|---|---|---|---|---|
| `UnitDef` (via `UnitManager.umDefs`) | `src/Unit/Types.hs:357` | global | Rebuild | `src/Engine/Asset/YamlUnits.hs` | none beyond type-correctness | `tools/role_probe.py` and others (content behavior, not persistence) |
| `BuildingDef` (via `BuildingManager.bmDefs`) | `src/Building/Types.hs:47` | global | Rebuild | `src/Engine/Asset/YamlBuildings.hs` | none beyond type-correctness | `tools/construction_probe.py` |
| `ItemDef` / `ItemManager.imDefs` | `src/Item/Types.hs:136`, `:372` | global | Rebuild | `src/Engine/Asset/YamlItems.hs` | none beyond type-correctness | `tools/item_instance_probe.py` |
| `EquipmentClassManager.ecmDefs` | `src/Equipment/Types.hs:43` | global | Rebuild | `src/Engine/Asset/YamlEquipment.hs` | none beyond type-correctness | `tools/repair_item_probe.py` |
| `SubstanceManager.sbmDefs` | `src/Substance/Types.hs:43` | global | Rebuild | `src/Engine/Asset/YamlSubstance.hs` | none beyond type-correctness | none yet |
| `InfectionManager.infmDefs` | `src/Infection/Types.hs:46` | global | Rebuild | `src/Engine/Asset/YamlInfection.hs` | none beyond type-correctness | `tools/infection_probe.py` |
| `RecipeManager.rmDefs` | `src/Craft/Types.hs:119` | global | Rebuild | `src/Engine/Asset/YamlRecipes.hs` | none beyond type-correctness | `tools/craft_probe.py` |
| `LocationRegistry.lrDefs` | `src/Location/Types.hs:68` | global | Rebuild | `src/Engine/Asset/YamlLocations.hs` | none beyond type-correctness | `tools/location_content_probe.py` |
| `LootTableRegistry.ltrDefs` | `src/LootTable/Types.hs:34` | global | Rebuild | `src/Engine/Asset/YamlLootTables.hs` | none beyond type-correctness | none yet |
| `TutorialRegistry.trTree` | `src/Tutorial/Types.hs:139` | global | Rebuild | `src/Engine/Asset/YamlTutorials.hs` | whole-directory validation at load (#957): the directory must exist and hold exactly one tree, and that tree must have unique ids, required presentation/evaluator fields, known kinds, resolvable references, valid kind/relationship combinations, exactly one root, no orphans, no cycles, and one parent per objective. `loadTutorialDir` writes this slot exactly once per call — the validated tree, or `Nothing` on any failure — so it is never partial and never dependent on directory read order. | hspec `--match "Tutorial definitions"` |
| `MaterialRegistry` | `src/World/Material.hs:233` | global | Rebuild | built-in, boot-time (fixed 256-slot table) | #763 (whole-session load transaction, round 3): every `MaterialId` in the edit log, spoil piles, and worldgen tectonic plates is checked against `isKnownMaterial` (id 0/air excluded — deliberately unregistered by design, legitimately persisted by the locations carving primitive) before publication; a load referencing an id this build never registered is rejected. Every id 0-255 is a structurally valid vector slot regardless (`getMaterialProps` can never fail), so this is the only way to detect a genuinely removed material. | `tools/world_check.py`, `tools/transactional_load_probe.py` |
| `FloraCatalog` (`fcSpecies`, `fcWorldGen`, `fcNextId`) | `src/World/Flora/Types.hs:244` | global | Rebuild | `src/Engine/Asset/YamlFlora.hs` | this type derives `Serialize`/`Generic` unlike its sibling content registries, but nothing in `SaveData` embeds it; species are referenced by numeric id from world state instead. #763 (round 5): every `FloraId` in the edit log (`WePlaceFlora`) and crop plots (`cpSpecies`) is checked against `lookupSpecies` before publication; a load referencing an unresolved species is rejected. | `tools/flora_growth_probe.py`, `tools/transactional_load_probe.py` |

---

## 10. `World.Save.Component.*` (`src/World/Save/Component*`) — the B2 save-component wire contract (#760)

#760 (save-overhaul B2) replaced #759's single transitional `"session"`
component (which wrapped the whole positional `SaveData`) with a set of
independently versioned, Haskell-owned persistence components riding
inside the same B1 envelope. `SaveData`/`WorldPageSave` are no longer
any wire contract — they survive only as a transitional IN-MEMORY bridge
into the still-unchanged world-thread load path (`snapshotToSaveData`),
which is why they remain classified under §4 above. The canonical
in-memory form is `World.Save.Snapshot.SessionSnapshot`; each component
below converts to/from a slice of it, is version-dispatched on decode,
self-validates, declares its dependencies, and names its owner. The
authoritative registry is `World.Save.Component.saveComponentRegistry`
(plus the envelope-owned `metadata` component); each `ComponentId` is
defined in `World.Save.Component.Types`.

Every component below is `Persist exactly` — the whole point of the
split is that each owns a distinct slice of persisted gameplay state.
The audit (`tools/persistence_inventory_audit.py`) cross-checks BOTH
directions: every persistent component here must have a registered
`ComponentId`, and every registered `ComponentId` must have a
persistent-classified row here — so a new component owner cannot land
without a classification decision, and a persistent owner cannot be
documented without actually being wired into the registry (contract §2,
requirement 5). An owner classified rebuilt/reset/excluded needs no
registration.

### Save components

| Component DTO | ComponentId | Classification | Owner / boundary reason |
|---|---|---|---|
| `SaveMetadata` (metadata component) | `metadata` | Persist exactly | Envelope — listing metadata, readable without decoding gameplay |
| `CoreSessionDTO` | `core-session` | Persist exactly | the session — game time, active/visible page refs, live camera, GLOBAL item/building/unit allocators |
| `TexPaletteDTO` | `texture-palette` | Persist exactly | renderer structure/edit layer — palette ids can't be rebuilt from content defs |
| `WorldPagesDTO` | `world-pages` (v9) | Persist exactly | the world page — page-set authority: identity, generation parameters, dates/clocks, map mode, and per-page camera. Generation parameters contain #911's placed-location instance table and page-local allocator, #1102's `GeoFeatureId`-keyed river-name table, and the terrain-generation inputs. A location stores its resolved bounds, display name/gloss, optional #1104 etymology source, lifecycle, content-spawn flag, and — since #916 — its optional encounter: the one-time count roll, roster-complete/death-only/cleared flags, activation/current-episode/once-per-episode feedback state, and every occupant's same-page unit id, distinct home tile, and engaged/returning state. Since #917 it also stores its GUARANTEED SIGNIFICANT-item obligations (per slot: the authored item def name, the spawned item's physical `iiInstanceId` once bound, and its own `taken` latch) plus the one instance-level clearance-notice latch that #917 generalized OUT of the encounter, so a location authoring significant items and no encounter has one too. `leCleared` is now encounter completion ALONE; the cleared LIFECYCLE is the conjunction of the conditions a location actually authors, and a location authoring neither never clears. v9 freezes the pre-#917 v8 shape as `PageCoreDTOv8`/`WorldGenParamsDTOv7`/`LocationInstancesDTOv5`/`LocationInstanceDTOv5`/`LocationEncounterDTOv1`; `migrateWorldPagesV8` preserves every stored value, lifts the encounter's clearance-notice flag onto the instance so a deferred notice is neither replayed nor lost, and adds NO significant obligations — reading them off today's YAML would owe a materialized world an item it never spawned. v8 freezes the pre-encounter v7 shape as `PageCoreDTOv7`/`WorldGenParamsDTOv6`/`LocationInstancesDTOv4`/`LocationInstanceDTOv4`; `migrateWorldPagesV7` preserves the materialized locations and adds no encounter. The v1 reconstruction path likewise strips any encounter AND any significant obligation supplied by today's definitions. Earlier migrations remain version-specific: v6 drops #1230's obsolete discovery margin; v5 predates etymology sources; v4 predates river names; v3 predates location glosses; v2 predates language provenance; v1 predates location instances and resolves its per-chunk flags through `resolveLegacyLocations`. Historical absent fields stay absent rather than being inferred from current content or regenerated data. |
| `WorldEditsDTO` | `world-edits` | Persist exactly | world edit layer — per-page terrain + structure edit log |
| `WorldActivityDTO` | `world-activity` (v3) | Persist exactly | mutable-world-activity layer — designations, flora harvests, crop plots, ground items, spoil. v2 (#1175) is a SEMANTIC bump with a byte-identical layout: it promises every designation key is CANONICAL (u-wrapped into the frame chunks are stored under — see `World.Render.HitTest`'s tile-coordinate contract), so one physical tile has exactly one key. v1 made no such promise, and `applyWorldActivity` re-keys a v1 payload on load using the page's own world size (`csDeps = [world-pages]`), collapsing two aliases of one tile to a single entry and re-saving as the current version. Away from the seam, and in arena / non-wrapping worlds, the repair is the identity. v3 (#1233) is the first SHAPE change: its ground items carry the physical values below, and v1/v2 — byte-identical to each other — decode through the one frozen `WorldActivityDTOv2` layout via `migrateWorldActivityV2`. Since #1233 every persisted `ItemInstanceDTO` in this component additionally carries the item's own EXTERNAL bulk (litres) and its OPTIONAL internal storage capacities, both materialized ONCE from the definition at creation exactly as `itdWeight` already is — so editing a definition's bulk never re-values an item an existing save already holds. Both are `Maybe`, and a pre-#1233 payload decodes with both ABSENT through the whole recursive contents tree: never a fabricated `0` (which the item loader itself rejects as invalid bulk) and never re-derived from today's definition, which would be exactly the retroactive reinterpretation #1233 requirement 6 forbids. Nothing in that slice CONSUMES either value (it is data only), so representing the absence is what keeps a reader from silently falling back to a since-edited definition; PLC-3B, the first slice that enforces a capacity, decides what an absent value means under enforcement. |
| `BuildingsDTO` | `buildings` (v2) | Persist exactly | `BuildingManager` — per-page building snapshot. v2 (#1233): its delivered-materials and loose-storage items carry the physical values below; v1 decodes through the frozen `BuildingInstanceDTOv1` tree via `migrateBuildingsDTOv1`. Since #1233 every persisted `ItemInstanceDTO` in this component additionally carries the item's own EXTERNAL bulk (litres) and its OPTIONAL internal storage capacities, both materialized ONCE from the definition at creation exactly as `itdWeight` already is — so editing a definition's bulk never re-values an item an existing save already holds. Both are `Maybe`, and a pre-#1233 payload decodes with both ABSENT through the whole recursive contents tree: never a fabricated `0` (which the item loader itself rejects as invalid bulk) and never re-derived from today's definition, which would be exactly the retroactive reinterpretation #1233 requirement 6 forbids. Nothing in that slice CONSUMES either value (it is data only), so representing the absence is what keeps a reader from silently falling back to a since-edited definition; PLC-3B, the first slice that enforces a capacity, decides what an absent value means under enforcement. |
| `UnitsDTO` | `units` (v2) | Persist exactly | `UnitManager` — per-page unit snapshot (stats/skills/equipment/inventory/wounds). v2 (#1233): its inventory, equipment and accessory items carry the physical values below; v1 decodes through the frozen `UnitInstanceDTOv1` tree via `migrateUnitsDTOv1`. Since #1233 every persisted `ItemInstanceDTO` in this component additionally carries the item's own EXTERNAL bulk (litres) and its OPTIONAL internal storage capacities, both materialized ONCE from the definition at creation exactly as `itdWeight` already is — so editing a definition's bulk never re-values an item an existing save already holds. Both are `Maybe`, and a pre-#1233 payload decodes with both ABSENT through the whole recursive contents tree: never a fabricated `0` (which the item loader itself rejects as invalid bulk) and never re-derived from today's definition, which would be exactly the retroactive reinterpretation #1233 requirement 6 forbids. Nothing in that slice CONSUMES either value (it is data only), so representing the absence is what keeps a reader from silently falling back to a since-edited definition; PLC-3B, the first slice that enforces a capacity, decides what an absent value means under enforcement. |
| `UnitSimDTO` | `unit-sim` (v3) | Persist exactly | `UnitThreadState` — per-page unit sim state (paths/targets/poses/deadlines). v3 (#1217): a move target additionally carries the REQUEST's damaging-drop policy (`MoveHazardPolicy`, an append-only enum reused as a leaf), so an ambient wander that may not route over a fall stays that way across a save. v1 and v2 — whose sim states are byte-identical to each other, differing only in v1's untyped map keys — decode through the frozen `UnitSimStateDTOv1`/`MoveTargetDTOv1` pair via `migrateUnitSimDTOv1`/`migrateUnitSimDTOv2`, and every in-flight target in them lands on `FallPermitted`: the behavior those bytes were written under, never inferred from the target's speed, its destination, or the unit's species. |
| `CraftBillsDTO` | `craft-bills` | Persist exactly | `Craft.Bills` — per-page craft-bill queue |
| `PowerNodesDTO` | `power-nodes` | Persist exactly | `Power.Types` — per-page power-node registry + stored charge |
| `ContainerKnowledgeDTO` | `container-knowledge` (v2, **OPTIONAL**) | Persist exactly | `Building.Knowledge` — the player's last-known contents per container, page-scoped and player-global (#1087). The FIRST optional component, joined by #1246's `transfer-orders` (next row) — the two of them are the whole optional set of this static Haskell registry. Every compatibility baseline predates it, and an ABSENT payload means "no container has ever been inspected" (`blankPageSnapshot`'s empty default), never known-empty and never inferred from live storage. A PRESENT payload that is malformed or at an unsupported version still fails the load. Its remembered `ItemInstanceDTO`s are historical observations — excluded from `allItemInstanceIds`, the allocator bound, the duplicate-live-id check, and live `item_instance` reference resolution; their def names remain ordinary content references. v2 (#1233): a remembered item carries the physical values below; v1 decodes through the frozen `ContainerRecordDTOv1` tree via `migrateContainerKnowledgeDTOv1` — "optional" governs ABSENCE, not the migration of a payload that IS present. Since #1233 every persisted `ItemInstanceDTO` in this component additionally carries the item's own EXTERNAL bulk (litres) and its OPTIONAL internal storage capacities, both materialized ONCE from the definition at creation exactly as `itdWeight` already is — so editing a definition's bulk never re-values an item an existing save already holds. Both are `Maybe`, and a pre-#1233 payload decodes with both ABSENT through the whole recursive contents tree: never a fabricated `0` (which the item loader itself rejects as invalid bulk) and never re-derived from today's definition, which would be exactly the retroactive reinterpretation #1233 requirement 6 forbids. Nothing in that slice CONSUMES either value (it is data only), so representing the absence is what keeps a reader from silently falling back to a since-edited definition; PLC-3B, the first slice that enforces a capacity, decides what an absent value means under enforcement. |
| `TransferOrdersDTO` | `transfer-orders` (v1, **OPTIONAL**) | Persist exactly | `Unit.Transfer.Orders` — the per-page queue of durable transfer orders (#1246, epic #1013 slice UIT-2A): each order's stable id, its acting unit, its endpoint pair and every requested item's own six-state lifecycle, in request order, plus the page-local id allocator. The SECOND optional component in this static Haskell registry (`lua.tutorial_progress` is separately optional on the Lua side): every tracked compatibility baseline predates it, and an ABSENT payload means "no order is queued" — true rather than invented, because before this slice there was nowhere for an order to be stored at all. A PRESENT payload that is malformed or at an unsupported version still fails the load. `Unit.Transfer`'s live policy vocabulary is mirrored by frozen DTOs with explicit field-by-field conversion (`TransferStateDTO`/`TransferReasonDTO`/`TransferEndpointDTO`), so growing that vocabulary is a visible wire event rather than a silent reinterpretation. |

This table is the STATIC Haskell-owned component set only. Since issue
#761 (save-overhaul B3) there is no `"lua-state"` transitional entry
here at all: each module registered with
`scripts/lib/save_modules.lua` is its OWN dynamically-added envelope
component (`"lua.<module>"`, disjoint reserved namespace —
`World.Save.Component.Types.luaComponentPrefix`), gathered from the
live Lua registry at save/load time rather than declared statically in
Haskell. See §7 for the Lua-owned component list.

---

## 11. Typed persistent references (`World.Save.Reference` / `World.Save.Integrity`) — the C3 reference graph (#764)

#764 (save-overhaul C3) gives every durable cross-component reference a
declared kind and scope rather than leaving it an untyped raw id. Two
distinct vocabularies exist, cross-checked by
`tools/persistence_inventory_audit.py` the same way §1-§7's root-owner
fields and Lua save modules already are (contract §7, requirement 15):
a newly introduced reference-bearing field or kind with no row below
fails the audit.

**Haskell DTO fields** typed with a `World.Save.Reference` wrapper
(`SamePageRef`/`CrossPageRef`) declare their scope at the TYPE level —
see `World.Save.Reference`'s module haddock for why this needs no new
wire bytes beyond the wrapped id. Classified `Persist as identity/
reference` uniformly: each is a stable id resolved against the current
session at load time, never an embedded copy.

### Typed persistent references

| Field | DTO / component | Scope | Classification | Validated by | Migration |
|---|---|---|---|---|---|
| `bilStation` | `CraftBillDTO` (`craft-bills`) | Same page as the bill | Persist as identity/reference | `World.Save.Integrity.sessionIntegrityErrors` (wrong-page hard-fails; absent-everywhere tolerated, #758) | v1→v2, `migrateCraftBillDTOv1` |
| `bilClaimant` | `CraftBillDTO` (`craft-bills`) | Same page as the bill (optional) | Persist as identity/reference | `World.Save.Integrity.sessionIntegrityErrors` (wrong-page hard-fails; absent tolerated) | v1→v2, `migrateCraftBillDTOv1` |
| `nodBuilding` | `PowerNodeDTO` (`power-nodes`) | Same page as the node | Persist as identity/reference | `World.Save.Integrity.sessionIntegrityErrors` (wrong-page hard-fails; absent tolerated) | v1→v2, `migratePowerNodeDTOv1` |
| `psSim` (map KEY, not a field value) | `PageSimDTO` (`unit-sim`) | Same page as the sim-state slice | Persist as identity/reference | `World.Save.Snapshot`'s `OrphanedUnitSimState` check (pre-existing #758 whole-session invariant — a sim owner with no matching unit on the same page hard-fails; this component was already validated, only the wire TYPE was untyped before round-3 review, #764) | v1→v2→v3, `migratePageSimDTOv1`/`migrateUnitSimDTOv1` then `migratePageSimDTOv2`/`migrateUnitSimDTOv2` |
| `ps2Sim` (map KEY, not a field value) | `PageSimDTOv2` (`unit-sim`, FROZEN v2) | Same page as the sim-state slice | Persist as identity/reference | decode-only: the same keys `psSim` above documents, read off pre-#1217 bytes and carried across verbatim by `migratePageSimDTOv2` (#1217) — validation happens once, on the migrated current shape | terminal (a frozen shape is never written) |
| `trdUnit` | `TransferOrderDTO` (`transfer-orders`) | Same page as the order | Persist as identity/reference | `World.Save.Integrity.sessionIntegrityErrors` (wrong-page hard-fails); absent-everywhere is a tolerated, REPORTED diagnostic via `sessionIntegrityWarnings` | none (v1) |
| `trdSource` / `trdDestination` (the `SamePageRef` inside each `TransferEndpointDTO` arm) | `TransferOrderDTO` (`transfer-orders`) | Same page as the order | Persist as identity/reference | same as `trdUnit` — an order can only be carried out within one page, so a cross-page endpoint is never legitimate | none (v1) |
| `qtdInstance` | `QueuedTransferDTO` (`transfer-orders`) | Same page as the order | Persist as identity/reference | same as `trdUnit`; a value ≤ 0 can never name a live instance and simply reports as dangling rather than being coerced into a large `Word64` | none (v1) |
| `leodUnitId` | `LocationEncounterOccupantDTO` (`world-pages`) | Same page as the placed location encounter | Persist as identity/reference | `World.Save.Integrity.sessionIntegrityErrors` (wrong-page hard-fails); absent-everywhere is tolerated and reported by `sessionIntegrityWarnings`, with roster membership retained so it cannot falsely clear | none (world-pages v8; v7 decodes with no encounter) |
| `lsidInstanceId` | `LocationSignificantItemDTO` (`world-pages`) | Same page as the placed location that owes the item — and, while the obligation is UNTAKEN, on that page's GROUND specifically | Persist as identity/reference | `World.Save.Integrity.significantProvenanceErrors` hard-fails an untaken obligation whose item resolves on another page or in an inventory/storage (it cannot be held without having been picked up, which is the one thing that latches `taken`), and hard-fails one physical id owed by two obligations; `significantDanglingWarnings` reports absent-everywhere and tolerates it, leaving the obligation untaken so a missing item can never be mistaken for a satisfied condition. Once `lsidTaken` is true no rule applies at all — post-pickup loss, consumption and destruction are explicitly allowed. `Location.Instance.locationSignificantItemErrors` additionally rejects a duplicated slot, an obligation marked taken that names no item (the one shape the provenance rules above cannot see, having no id to resolve), and same-page duplicate ownership, at component decode. The clearance predicate itself also refuses to count a taken-but-unbound obligation, so such a payload cannot clear a location even if it reached a live table. Absent until the content spawn binds one, which is what keeps an unspawned obligation incomplete — and while it is absent the sibling `lsidItemDefName` is validated against the live item registry by `World.Save.Types.missingSignificantItemReferences`, so a save owing an item this build no longer defines is refused rather than published into a location that could never spawn it. A BOUND obligation is exempt: nothing re-spawns a filled slot. | none (world-pages v9; v8 and older decode with NO obligations — `fromLocationInstanceDTOv5` never infers them from today's YAML) |

**Lua reference kinds** are the controlled `kind` vocabulary a
registered Lua save component's `references()` hook reports
(`scripts/unit_ai_save_refs.lua`, `scripts/building_spawn.lua` — see
#761's `references` spec field). Cross-validated by
`Engine.Scripting.Lua.API.Save.Integrity`'s `knownEntitiesFromSaveData` /
`World.Save.Integrity.luaReferenceErrors`: `unit`/`building`/
`item_instance` resolve session-wide (matching each component's own
declared `scope = "global"` — these three are GLOBAL allocators, one
counter for the whole session), while `craft_bill`/`ground_item`
resolve against the OWNING unit's page specifically (each `references()`
edge carries an `owner` — the unit id it came from — since `BillId`/
ground-item ids are PER-PAGE allocators, and a session-wide match would
let a reference meant for one page's missing bill falsely resolve
against an unrelated same-numbered bill elsewhere). `location_instance`
(#915, per-unit location memory) is per-page too, but resolves against
the page the EDGE ITSELF declares (`page` on the reference edge, and
`page` inside the persisted `{__ref="location_instance", id, page, x, y}`
entry) rather than the owning unit's: a placed location's durable
identity is `(WorldPageId, LocationInstanceId)` (#911) and the page is
part of what the unit remembers, not an incidental fact about where the
remembering unit currently stands. Either way, a
reference that doesn't resolve is a non-blocking diagnostic (dangling
target, or one at/above its kind's allocator), never a load failure —
the #761-established tolerated-dangling-reference contract. Classified
`Persist as identity/reference` uniformly, same reasoning as the
Haskell table above. The persisted FIELDS themselves (not just this
diagnostic surface) are typed too — see "Typed persistent references
and the shared integrity graph" in `persistence_contract.md`'s §9 for
the `{__ref=kind, id=N}` wire shape and the v1→v2 migration.

### Lua reference kinds

| Kind | Reported by | Resolution scope | Haskell-owned target | Classification |
|---|---|---|---|---|
| `unit` | `unit_ai_save_refs.lua`, `building_spawn.lua` | session-wide (global allocator) | `units` component (`UnitId`) | Persist as identity/reference |
| `building` | `unit_ai_save_refs.lua`, `building_spawn.lua` | session-wide (global allocator) | `buildings` component (`BuildingId`) | Persist as identity/reference |
| `craft_bill` | `unit_ai_save_refs.lua` | owning unit's page (per-page allocator) | `craft-bills` component (`BillId`) | Persist as identity/reference |
| `item_instance` | `unit_ai_save_refs.lua` | session-wide (global allocator) | carried inventory, owned by the `units` component's own snapshot | Persist as identity/reference |
| `ground_item` | `unit_ai_save_refs.lua` | owning unit's page (per-page allocator) | `world-activity` component (ground items) | Persist as identity/reference |
| `location_instance` | `unit_ai_save_refs.lua` | the edge's OWN declared world page (per-page allocator) | `world-pages` component (`LocationInstanceId`, on the page's gen params) | Persist as identity/reference |

---

## 12. Test coverage map (#767, save-overhaul D1)

The machine-readable map from every persistent §10/§7 owner to its
contract coverage, at COMPONENT granularity (requirement 3's own
"Owning component" column) rather than per-field — a single whole-
session structural comparison discharges every field a component owns
in one assertion, so per-field rows would only restate the same
pointer over and over. `tools/persistence_inventory_audit.py` enforces
that every §10 component classified `Persist exactly` and every §7 Lua
module classified `Persist exactly` has a row here (see "The
persistence-inventory audit" in `persistence_contract.md`).

"Canonical inspection path" is one of:
- **`SessionSnapshot` field(s) (`World.Save.Snapshot`)** — the Haskell
  components. `World.Save.Snapshot.SessionSnapshot`/`PageSnapshot`
  derive `Eq`, so structural equality of two assembled snapshots IS the
  canonical, order-independent (every collection is `HashMap`-keyed)
  comparison — no separate JSON/dump schema needed for these.
  `tools/persistence_snapshot.py`'s `compare_session_files` decodes N
  save files through the real `World.Save.Envelope.decodeSessionEnvelope`
  and asserts every decoded `SessionSnapshot` is pairwise `≡`, GPU-less
  and engine-less (a `cabal repl` subprocess against raw files).
- **`lua.<module>` canonical component payload bytes** — the Lua
  components. `scripts/lib/data_codec.lua`'s canonical (sorted-key)
  encoding means identical logical state always re-encodes to identical
  bytes, so byte-equality of the raw `lua.unit_ai`/`lua.building_spawn`
  envelope payloads across independently-produced saves is exactly as
  strong a structural proof as `SessionSnapshot`'s derived `Eq` — no
  separate decode step needed either.

"Round-trip assertion" for every row below is the SAME structural
comparison (either the pure in-process hspec round trip — encode then
decode then `≡`, see `Test.Headless.World.Save.Contract`'s
"persistence contract" describe group — or the real fresh-process
comparison across independently-produced save generations, see
`tools/persistence_contract_probe.py`/`tools/persistence_contract_sweep.py`)
unless noted otherwise. "Reset/rebuild assertion" is left "—" for a
component with nothing reset/rebuilt at its OWN boundary (the reset/
rebuild fields living beside these components — tool mode, time scale,
selections, UI-transient state — are §1/§3/§8 rows, not components, and
are separately asserted by the reset-policy checks the same probe/test
group runs, per contract requirement 6).

### Test coverage map

| Component | Canonical inspection path | Round-trip assertion | Reset/rebuild assertion | Focused test |
|---|---|---|---|---|
| `metadata` | `SaveMetadata` fields via `dump_canonical_summary`/`engine.listSaves()` (request-adjacent, excluded from the strict `SessionSnapshot` comparison per contract requirement 5 — seed/worldSize/plateCount are re-verified via the authoritative `pgsGenParams`/`wgpSeed` instead) | `tools/multiworld_save_probe.py`, `tools/persistence_contract_probe.py` (`smTimestamp` is a well-formed timestamp string) | — | `tools/save_compat_migration_probe.py` |
| `core-session` | `SessionSnapshot`'s global fields (`snapGameTime`/`snapNextItemId`/`snapNextBuildingId`/`snapNextUnitId`/`snapActivePage`/`snapVisiblePages`/`snapLiveCamera`) | `tools/persistence_contract_probe.py`, `Test.Headless.World.Save.Contract` | `sgLiveCamera`'s owner-page reset on an empty visible set is asserted structurally (type has no stale-owner state to reset) | `tools/multiworld_save_probe.py` |
| `texture-palette` | `SessionSnapshot.snapTexPalette` (`TexPalette`) | `tools/persistence_contract_probe.py`, `Test.Headless.World.Save.Contract` | — | `Test.Headless.World.Save.Components` |
| `world-pages` | `PageSnapshot`'s identity/gen-params/dates/clocks/map-mode/per-page-camera fields. Gen params include `wgpLocationInstances` (allocator, stored geometry/name/provenance/lifecycle/content state, #916's optional encounter roll, exact roster, occupant homes, activation/current-episode feedback and guard-policy state, and #917's significant-item obligations plus the instance-level clearance-notice latch) and #1102's page-local river-name table. | `tools/persistence_contract_probe.py`, `Test.Headless.World.Save.Contract` (whose rich fixture carries obligations in three states — taken, spawned-and-untaken, and unspawned — so a round trip that collapsed the list or lost the optional physical id cannot pass); #916's populated encounter round-trip and v7/v1 absence migrations, and #917's v8→v9 migration, are pinned by `Test.Headless.World.Save.Compat` and `Test.Headless.Location.Instance`; the compound predicate itself by `Test.Headless.Location.SignificantContents` and `Test.Headless.World.LocationDiscovery`, and exact same-page occupant and significant-item resolution by `Test.Headless.World.Save.Integrity`. `pgsGenParams`' full fidelity (including `GeoTimeline`) is additionally cross-checked by chunk-regen determinism. | `wsToolModeRef`/`wsTimeScaleRef` reset-to-default is asserted live (`engine.getToolMode()`/`world.getTimeScale()` post-load) | `tools/world_check.py`, `tools/multiworld_save_probe.py`, `tools/location_content_probe.py`, `tools/river_naming_probe.py` |
| `world-edits` | `PageSnapshot.pgsEdits` | `tools/persistence_contract_probe.py`, `Test.Headless.World.Save.Contract` | — | `tools/world_check.py` (determinism), `tools/location_overlay_probe.py` |
| `world-activity` | `PageSnapshot`'s designation/ground-item/spoil-pile fields (`pgsMineDesignations`/`pgsConstructDesignations`/`pgsChopDesignations`/`pgsTillDesignations`/`pgsCropPlots`/`pgsPlantDesignations`/`pgsGroundItems`/`pgsSpoilPiles`) | `tools/persistence_contract_probe.py`, `Test.Headless.World.Save.Contract` | — | `tools/chop_probe.py`, `tools/till_probe.py`, `tools/crop_probe.py`, `tools/plant_probe.py`, `tools/item_instance_probe.py` |
| `buildings` | `PageSnapshot.pgsBuildings` | `tools/persistence_contract_probe.py`, `Test.Headless.World.Save.Contract` | `bmSelected` reset is asserted live (`building.getSelected()` post-load) | `tools/multiworld_save_probe.py`, `tools/construction_probe.py` |
| `units` | `PageSnapshot.pgsUnits` | `tools/persistence_contract_probe.py`, `Test.Headless.World.Save.Contract` | `umSelected`/reset-on-load `UnitInstance` fields (`uiLastAttackerUid`/`uiAnimOverride`/etc.) asserted live | `tools/multiworld_save_probe.py` |
| `unit-sim` | `PageSnapshot.pgsUnitSimStates` | `tools/persistence_contract_probe.py`, `Test.Headless.World.Save.Contract` | — | `tools/movement_probe.py` |
| `craft-bills` | `PageSnapshot.pgsCraftBills` | `tools/persistence_contract_probe.py`, `Test.Headless.World.Save.Contract` | — | `tools/craft_bill_probe.py` |
| `power-nodes` | `PageSnapshot.pgsPowerNodes` | `tools/persistence_contract_probe.py`, `Test.Headless.World.Save.Contract` | — | `tools/power_probe.py` |
| `container-knowledge` | `PageSnapshot.pgsContainerKnowledge` | `tools/persistence_contract_probe.py`, `Test.Headless.World.Save.Contract` (a POPULATED record rides in the representative session), `Test.Headless.Building.Knowledge` (all three states through the real codec) | a record whose building is absent from the restored page is SCRUBBED with a diagnostic at load (`World.Load.Stage`), and a save predating the component decodes to an empty map (every container never-inspected) — both asserted by `Test.Headless.Building.Knowledge`, the second also against the real tracked pre-#1087 fixture by `Test.Headless.World.Save.Compat` | `Test.Headless.Building.Knowledge` (`--match "Container knowledge"`) |
| `transfer-orders` | `PageSnapshot.pgsTransferOrders` | `tools/persistence_contract_probe.py`, `Test.Headless.World.Save.Contract` (a POPULATED store rides in the representative session, covering all six `TransferState` shapes, request order, and a building-to-building order whose acting unit is neither endpoint), `Test.Headless.World.TransferOrders` (the same through the LIVE `wsTransferOrdersRef` — real `WorldSave` capture, real `stageSession` restore) | a save predating the component decodes to the empty store (no order queued, allocator back at 1), and a present-but-malformed payload still fails the load — both asserted by `Test.Headless.World.Save.Components`. A dangling carrier/endpoint/item is REPORTED (`sessionIntegrityWarnings` at save, `World.Load.Stage` at load) and the order RETAINED, never pruned by the codec — #1253's pruning is a LIVE gameplay transition (the executor, or `Unit.Transfer.Live` when the carrier dies), which is what normally leaves nothing dangling to report. | `Test.Headless.World.Save.Components` / `Test.Headless.World.Save.Integrity` / `Test.Headless.World.TransferOrders` |
| `lua.unit_ai` | `lua.unit_ai` canonical component payload bytes | `tools/persistence_contract_probe.py` (byte-equality across independently-produced saves); `Test.Headless.Lua.SaveModules` (encode/decode/apply contract) | per-entity apply (absent-owner rows dropped with a diagnostic, present rows applied exactly) asserted against the real registration by `tools/lua_orphan_prune_probe.py`, and against the generic mechanism by `Test.Headless.Lua.SaveModules`' "per-entity component application" cases | `tools/lua_orphan_prune_probe.py`, `Test.Headless.Lua.SaveModules` |
| `lua.building_spawn` | `lua.building_spawn` canonical component payload bytes | same as `lua.unit_ai` | same as `lua.unit_ai` | `tools/lua_orphan_prune_probe.py`, `Test.Headless.Lua.SaveModules` |
| `lua.tutorial_progress` | `lua.tutorial_progress` canonical component payload bytes — a single `completed` array of authored objective ids, emitted in sorted order so the payload is byte-stable for one progress state | `Test.Headless.Lua.TutorialProgress` (`snapshotAll` → `prepareLoad` → `applyAll` through the real registry: the completed set survives, and a subobjective checked before the save comes back UNCHECKED — live state is recomputed, never restored) | `apply()` clears the live subobjective checks on every load, and a save carrying no `lua.tutorial_progress` component at all applies `default()`'s fresh progress — both asserted by `Test.Headless.Lua.TutorialProgress` | `Test.Headless.Lua.TutorialProgress` (`--match "Tutorial progress"`) |

Every entry not covered by `SessionSnapshot`'s own comparison (visual-
asset fallback policy, the reset-only fields listed in §1/§3/§8, and
the assembled failure contract) is covered directly by
`tools/persistence_contract_probe.py`/`_sweep.py` and
`Test.Headless.World.Save.Contract`'s own "persistence contract"
describe group — see `docs/persistence_contract.md`'s updated §6/§10
for the consolidated final test matrix.

---

## Summary: what's actually new here vs. what v82 already does

Every classification above matches v82's current behavior **except**
four, none of them implemented by this issue (#756):

1. `wsTimeScaleRef` / `wpsTimeScale` — v82 persisted it; target was
   Exclude (contract §1, "the pre-save speed is not persisted").
   **Implemented by #757/#758**: see the row above and CLAUDE.md's
   Save/Load architecture notes
   (`World.Save.Snapshot.SessionSnapshot`).
2. `pause.lua`'s `prevTimeScale` — v82 persisted and restored it;
   target was Exclude (same rule). **Implemented by #757.**
3. `wsToolModeRef` / `wpsToolMode` — v82 writes the field, but the field
   is already dead at load (overridden to `DefaultTool` by #103); this
   was a reclassification of already-dead behavior, not a functional
   change at the time. **#758** additionally removed the field from
   `World.Save.Snapshot.PageSnapshot` entirely, so the adapter's
   `DefaultTool` fabrication is now the only source of the legacy
   field's value.
4. `wmWorlds`/`wmVisible` (i.e. the load path as a whole) — v82's
   `handleWorldLoadSaveCommand` deliberately MERGED a loaded save into
   whatever was already live, preserving unrelated pages (#191/#218);
   target was whole-session replacement (contract §1). This was the
   largest divergence of the four — a load-path behavior, not a single
   field. **Implemented by #763** (save-overhaul C2): the whole
   incrementally-mutating load path (`LoadWorld.hs`/`LoadPage.hs`/
   `RestoreIds.hs`) was replaced by a staged transaction
   (`World.Load.Stage` + `World.Load.Publish`) that publishes exactly
   the saved session's own pages — see `persistence_contract.md`'s
   "Resolved divergence: loading used to merge, not replace" for the
   full writeup and why #191 made the old choice deliberately.

Everything else documents v82's existing, unchanged behavior under the
new taxonomy.
