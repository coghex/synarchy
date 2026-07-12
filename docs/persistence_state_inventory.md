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
its own table cell — `tools/persistence_inventory_audit.py` greps for
exactly that pattern, so **do not reformat a field name out of
backticks** when editing this file; the audit will report it as
unclassified.

---

## 1. `EngineEnv` (`src/Engine/Core/State.hs:64`) — global

| Field | Scope | Classification | Restoration dependency | Validation | Test oracle |
|---|---|---|---|---|---|
| `engineConfig` | global | Exclude | — | boot flag, not session state | none yet |
| `engineStateRef` | global | — | see §2 | pointer to `EngineState`, classified field-by-field there, not itself a persistence decision | — |
| `videoConfigRef` | global | Exclude | — | local runtime config (`config/video.yaml`, #638) | `tools/config_state_probe.py` |
| `windowSizeRef` | global | Exclude | — | OS/window-owned | none yet |
| `windowStateRef` | global | Exclude | — | OS/window-owned | none yet |
| `framebufferSizeRef` | global | Exclude | — | OS/window-owned | none yet |
| `fpsRef` | global | Exclude | — | display setting (`video.yaml`) | `tools/config_state_probe.py` |
| `brightnessRef` | global | Exclude | — | display setting (`video.yaml`) | `tools/config_state_probe.py` |
| `pixelSnapRef` | global | Exclude | — | display setting (`video.yaml`) | `tools/config_state_probe.py` |
| `textureFilterRef` | global | Exclude | — | display setting (`video.yaml`) | `tools/config_state_probe.py` |
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
| `nextItemInstanceIdRef` | global | Persist exactly | — | must restore as `max(loaded, current)`, never lower (#67) — the existing `sdNextItemInstanceId` rule | `tools/item_instance_probe.py` |
| `fontCacheRef` | global | Rebuild | font assets on disk | rebuilt at boot | none yet |
| `inputStateRef` | global | Exclude | — | live device state (keys/buttons down) | none yet |
| `keyBindingsRef` | global | Exclude | — | local runtime config (`config/keybinds.yaml`, #638) | `tools/config_state_probe.py` |
| `currentKeyDownRef` | global | Exclude | — | transient dispatch flag, `Nothing` outside a key-down broadcast | none yet |
| `textBuffersRef` | global | Exclude | — | transient UI text-input buffers | none yet |
| `cameraRef` | global | Exclude | active page's `WorldCamera` + `wpsCameraZoom`/`wpsCameraFacing` | session-only render camera; see §8 for the per-page source of truth | none yet |
| `uiCameraRef` | global | Exclude | window size | derived from window size at boot | none yet |
| `uiManagerRef` | global | Exclude | — | entire UI tree rebuilt by Lua on load | none yet |
| `focusManagerRef` | global | Exclude | — | rebuilt as UI rebuilds | none yet |
| `worldManagerRef` | global | Rebuild | `SaveData.sdWorlds`, `sdActivePage`, `sdVisiblePages` | the container itself is rebuilt; each contained `WorldState` is restored per §3/§4 | `tools/multiworld_save_probe.py` |
| `hudActivePageRef` | global | Exclude | `wmVisible` | reset from the active/visible page set post-load | none yet |
| `loadProvenanceRef` | global | Reset to default | — | reset to empty at boot; session-scoped collision-remap bookkeeping, never itself written to the save file | `tools/multiworld_save_probe.py` (collision-remap paths) |
| `worldQueue` | global | Exclude | — | transport queue; see contract §3 | none yet |
| `sunAngleRef` | global | Rebuild | active page's world time | derived via `worldTimeToSunAngle` | none yet |
| `worldPreviewRef` | global | Exclude | — | pending GPU upload payload | none yet |
| `zoomAtlasDataRef` | global | Exclude | — | pending GPU upload payload | none yet |
| `screenshotRequestQueue` | global | Exclude | — | debug-only transport queue; see contract §3 | none yet |
| `worldQuadsRef` | global | Rebuild | loaded chunk data | render cache | none yet |
| `textureSystemRef` | global | Exclude | — | GPU bindless texture system | none yet |
| `samplerCacheRef` | global | Exclude | — | GPU sampler cache | none yet |
| `textureSizeRef` | global | Exclude | — | GPU texture dimension cache | none yet |
| `defaultFaceMapSlotRef` | global | Exclude | — | GPU texture slot | none yet |
| `floraCatalogRef` | global | Rebuild | `data/*.yaml` flora content | reloaded fresh from YAML at boot; species referenced by numeric id from world state (see §9 re: its unused `Serialize` instance) | `tools/flora_growth_probe.py` |
| `materialRegistryRef` | global | Rebuild | built-in material table | boot-time, not YAML-driven | none yet |
| `unitManagerRef` | global | — | see §5 (`UnitManager` fields classified individually) | — | — |
| `unitQueue` | global | Exclude | — | transport queue; see contract §3 | none yet |
| `utsRef` | global | Rebuild | `wpsUnits`/`wpsUnitSimStates` after load | sim-side per-unit pos/pose/target/path rebuilt from the restored `UnitInstance`/`UnitSimState` snapshot, not itself directly serialized | `tools/movement_probe.py` (post-load steering sanity) |
| `statRNGRef` | global | Exclude | — | explicitly non-deterministic, not save-seeded (contract §1) | none yet |
| `buildingManagerRef` | global | — | see §5 (`BuildingManager` fields classified individually) | — | — |
| `texPaletteRef` | global | Persist exactly | — | `sdTexPalette` | none yet |
| `texPaletteHandlesRef` | global | Exclude | `texPaletteRef` | runtime GPU translation table rebuilt from `texPaletteRef` | none yet |
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
| `gameTimeRef` | global | Persist exactly | — | `sdGameTime` | `tools/save_pause_probe.py` |
| `lastSaveTimeRef` | global | Exclude | — | wall-clock bookkeeping, session-only | none yet |
| `itemManagerRef` | global | Rebuild | `data/items/*.yaml` | see §9 | none yet |
| `equipmentClassManagerRef` | global | Rebuild | `data/*.yaml` equipment content | see §9 | none yet |
| `substanceManagerRef` | global | Rebuild | `data/*.yaml` substance content | see §9 | none yet |
| `infectionManagerRef` | global | Rebuild | `data/*.yaml` infection content | see §9 | `tools/infection_probe.py` |
| `recipeManagerRef` | global | Rebuild | `data/recipes/*.yaml` | see §9 | `tools/craft_probe.py` |
| `locationDefsRef` | global | Rebuild | `data/*.yaml` location content | see §9 | `tools/location_content_probe.py` |
| `lootTableRegistryRef` | global | Rebuild | `data/*.yaml` loot content | see §9 | none yet |
| `eventStoreRef` | global | Exclude | — | player-event ring buffer, explicitly session-only | `tools/injury_log_probe.py` (stream behavior) |
| `notificationCfgRef` | global | Exclude | — | local runtime config (`config/notifications.yaml`, #638) | `tools/config_state_probe.py` |
| `notificationOrder` | global | Exclude | — | local runtime config, derived ordering | `tools/config_state_probe.py` |
| `popupQueueRef` | global | Exclude | — | transient popup event queue | none yet |

`engineStateRef` and `worldManagerRef`/`unitManagerRef`/
`buildingManagerRef` are pointer fields whose *pointed-to* records are
classified in their own sections (§2, §3/§4, §5) rather than here.

## 2. `EngineState` (`src/Engine/Core/State.hs:329`) — global, main-thread-private

| Field | Scope | Classification | Restoration dependency | Validation | Test oracle |
|---|---|---|---|---|---|
| `timingState` | global | Exclude | — | frame counters/timing, reset at boot | none yet |
| `graphicsState` | global | Exclude | — | entire Vulkan device/swapchain/pipeline/buffer state; rebuilt at boot. Individual GPU handles are not separately inventoried — they are not gameplay state and none are ever save-file candidates. | none yet |
| `assetConfig` | global | Exclude | — | rebuilt at boot | none yet |
| `sceneManager` | global | Exclude | — | scene graph, rebuilt by UI/world render setup each session | none yet |

## 3. `WorldManager` / `WorldState` (`src/World/State/Types.hs`)

`WorldManager` (`:261`) — global:

| Field | Classification | Restoration dependency | Test oracle |
|---|---|---|---|
| `wmWorlds` | Rebuild | `SaveData.sdWorlds` | `tools/multiworld_save_probe.py` — **but see below**: today's rebuild MERGES restored pages into whatever's live, it does not replace the session (contract §1 divergence) |
| `wmVisible` | Rebuild | `SaveData.sdVisiblePages` | `tools/multiworld_save_probe.py` |

**Current v82 behavior diverges from the contract's target here**:
`handleWorldLoadSaveCommand` (`src/World/Thread/Command/Save/LoadWorld.hs`,
#191/#218) deliberately keeps any live page outside the set the load
"owns" (restored pages + their saved original ids + a prior load's
pages) rather than dropping it — a merge, not the whole-session
replacement contract §1 requires. This PR does not change that; see
`persistence_contract.md`'s "Divergence: current loading merges, it does
not replace" for the full writeup and the responsible future child.

`WorldState` (`:43`) — per-page, one instance per live world:

| Field | Classification | Restoration dependency | Validation | Test oracle |
|---|---|---|---|---|
| `wsTilesRef` | Rebuild | `wpsGenParams` + `wpsEdits` | chunk regen followed by edit replay must reproduce the pre-save surface exactly for every edited tile | `tools/world_check.py` (determinism), `tools/multiworld_save_probe.py` |
| `wsCameraRef` | Persist exactly | — | `wpsCameraX`/`wpsCameraY` | none yet |
| `wsTexturesRef` | Exclude | — | runtime GPU handles | none yet |
| `wsGenParamsRef` | Persist exactly | — | `wpsGenParams`; deliberately meaningful seed data (contract §1) | `tools/multiworld_save_probe.py` |
| `wsTimeRef` | Persist exactly | — | `wpsTimeHour`/`wpsTimeMinute` | `tools/save_pause_probe.py` |
| `wsDateRef` | Persist exactly | — | `wpsDateYear`/`wpsDateMonth`/`wpsDateDay` | `tools/flora_growth_probe.py` |
| `wsTimeScaleRef` | **Exclude (new format)** | — | v82 currently persists via `wpsTimeScale`; contract §1/§5 retarget this to Exclude — the pre-save speed is not part of the contract. Not a runtime change in this issue. | `tools/save_pause_probe.py` (must be updated by whichever child implements this) |
| `wsZoomCacheRef` | Rebuild | loaded chunk data | render cache | none yet |
| `wsQuadCacheRef` | Rebuild | loaded chunk data | render cache | none yet |
| `wsQuadCacheGenRef` | Rebuild | — | cache generation counter | none yet |
| `wsZoomQuadCacheRef` | Rebuild | loaded chunk data | render cache | none yet |
| `wsBgQuadCacheRef` | Rebuild | loaded chunk data | render cache | none yet |
| `wsBakedZoomRef` | Rebuild | loaded chunk data | render cache | none yet |
| `wsBakedBgRef` | Rebuild | loaded chunk data | render cache | none yet |
| `wsInitQueueRef` | Reset to default | — | page load-progress queue, always starts fresh | none yet |
| `wsMapModeRef` | Persist exactly | — | `wpsMapMode` (contract §1: visible world/page state) | none yet |
| `wsCursorRef` | Exclude | — | transient hover/cursor state (contract §5 exclusion list) | none yet |
| `wsToolModeRef` | **Reset to default** | — | `DefaultTool`, per #103; matches existing runtime behavior already, this just formalizes it as the contract classification | none yet |
| `wsCursorSnapshotRef` | Exclude | — | transient | none yet |
| `wsLoadPhaseRef` | Reset to default | — | page load-phase tracker, always starts at the initial phase | none yet |
| `wsZoomAtlasRef` | Exclude | — | GPU atlas handle | none yet |
| `wsEditsRef` | Persist exactly | — | `wpsEdits`; core gameplay data (player terrain edits) | `tools/multiworld_save_probe.py`, `tools/world_check.py` |
| `wsOreSurveyRef` | Rebuild | loaded chunk/ore data | zoom-map survey memo, derived | none yet |
| `wsMineDesignationsRef` | Persist exactly | — | `wpsMineDesignations` | none yet |
| `wsGroundItemsRef` | Persist exactly | — | `wpsGroundItems` | `tools/item_instance_probe.py` |
| `wsSpoilRef` | Persist exactly | — | `wpsSpoilPiles` | none yet |
| `wsStructureStageRef` | Exclude | — | explicitly never saved; in-progress structure placement must finish or be abandoned by the snapshot boundary (contract §3) | `tools/location_stamp_idempotent_probe.py` (idempotency, not this state directly) |
| `wsConstructDesignationsRef` | Persist exactly | — | `wpsConstructDesignations` | `tools/construction_probe.py` |
| `wsFloraHarvestsRef` | Persist exactly | — | `wpsFloraHarvests` | `tools/flora_growth_probe.py`, `tools/foraging_probe.py` |
| `wsChopDesignationsRef` | Persist exactly | — | `wpsChopDesignations` | `tools/chop_probe.py` |
| `wsCraftBillsRef` | Persist exactly | — | `wpsCraftBills` | `tools/craft_bill_probe.py` |
| `wsPowerNodesRef` | Persist exactly | — | `wpsPowerNodes` | `tools/power_probe.py` |
| `wsTillDesignationsRef` | Persist exactly | — | `wpsTillDesignations` | `tools/till_probe.py` |
| `wsCropPlotsRef` | Persist exactly | — | `wpsCropPlots` | `tools/crop_probe.py` |
| `wsPlantDesignationsRef` | Persist exactly | — | `wpsPlantDesignations` | `tools/plant_probe.py` |
| `wsBloodStoreRef` | Exclude | — | explicitly never saved (#604, decal/model debug feature) | `tools/blood_decal_probe.py` (behavior, not persistence) |
| `wsBloodTextureHandlesRef` | Exclude | — | GPU handle cache | none yet |
| `wsIdentityRef` | Persist exactly | — | `wpsIdentity` (#707 player-facing world identity) | `tools/multiworld_save_probe.py` |

`CursorSnapshot` and `LoadPhase` (`:274`, `:285`) are the types behind
`wsCursorSnapshotRef`/`wsLoadPhaseRef` above, not separate root owners.

## 4. `World.Save.Types` (`src/World/Save/Types.hs`) — the current envelope (v82)

This section is the ground truth of what v82 actually writes to disk
today. Per the issue-review correction, it is recorded as-is (not an
idealized target) — the two rows marked **(new-format target differs)**
are the only classifications in this document that diverge from what
v82's field currently does.

Fields with no non-trivial restoration dependency or validation rule
beyond type-correct deserialization say so plainly (contract §2:
validation is only interesting "where that's non-trivial") rather than
inventing one.

`SaveHeader` (`:291`) — global:

| Field | Classification | Restoration dependency | Validation | Test oracle |
|---|---|---|---|---|
| `shMagic` | Persist exactly | — (read first, before anything else) | must equal `saveMagic` (`0x53595241`) or the file is rejected as not a save at all | none yet |
| `shVersion` | Persist exactly | — (read second) | must equal `currentSaveVersion` or load fails clearly with "expected vN, got vM" (contract §5) | format-mismatch error path (manual) |

`SaveMetadata` (`:297`) — global:

| Field | Classification | Restoration dependency | Validation | Test oracle |
|---|---|---|---|---|
| `smName` | Persist exactly | — | must satisfy `sanitizeSaveName` (it's the save-slot identity) | `tools/multiworld_save_probe.py` |
| `smSeed` | Persist exactly | — | none beyond type-correctness (listing metadata only; the authoritative seed for a page is its own `wpsGenParams`) | `tools/multiworld_save_probe.py` |
| `smWorldSize` | Persist exactly | — | none beyond type-correctness (listing metadata only) | `tools/multiworld_save_probe.py` |
| `smPlateCount` | Persist exactly | — | none beyond type-correctness (listing metadata only) | `tools/multiworld_save_probe.py` |
| `smTimestamp` | Persist exactly | — | none beyond type-correctness (display only) | none yet |
| `smWorldName` | Persist exactly | the active page's `wpsIdentity` at save time | mirrors that page's identity; `Nothing` for an unnamed world | `tools/multiworld_save_probe.py` |
| `smWorldGloss` | Persist exactly | `smWorldName` | must be `Nothing` whenever `smWorldName` is `Nothing` (a gloss cannot exist without a display name) | `tools/multiworld_save_probe.py` |

`WorldPageSave` (`:325`) — per-page. Every field below whose
restoration dependency isn't otherwise noted needs only its own page's
prior fields (no cross-page ordering requirement):

| Field | Classification | Restoration dependency | Validation | Test oracle |
|---|---|---|---|---|
| `wpsPageId` | Persist as identity/reference | live page ids already in the session (`assignRestoreIds`) | restore-target id must be unique within the session after collision-renaming | `tools/multiworld_save_probe.py` |
| `wpsGenParams` | Persist exactly | — | none beyond type-correctness (chunk regen is not re-validated against it at load) | `tools/multiworld_save_probe.py` |
| `wpsCameraX` | Persist exactly | — | none beyond type-correctness | none yet |
| `wpsCameraY` | Persist exactly | — | none beyond type-correctness | none yet |
| `wpsCameraZoom` | Persist exactly | — | none beyond type-correctness | none yet |
| `wpsCameraFacing` | Persist exactly | — | none beyond type-correctness | none yet |
| `wpsTimeHour` | Persist exactly | — | none beyond type-correctness (not range-checked against 0-23 at load) | `tools/save_pause_probe.py` |
| `wpsTimeMinute` | Persist exactly | — | none beyond type-correctness (not range-checked against 0-59 at load) | `tools/save_pause_probe.py` |
| `wpsDateYear` | Persist exactly | — | none beyond type-correctness | `tools/flora_growth_probe.py` |
| `wpsDateMonth` | Persist exactly | — | none beyond type-correctness (not range-checked against the world's calendar at load) | `tools/flora_growth_probe.py` |
| `wpsDateDay` | Persist exactly | — | none beyond type-correctness (not range-checked against the world's calendar at load) | `tools/flora_growth_probe.py` |
| `wpsTimeScale` | **Exclude (new-format target differs)** | n/a (excluded) | n/a | v82 persists it; the contract retargets it to Exclude (contract §1, "the pre-save speed is not persisted") — a future runtime child's change, not this issue's. `tools/save_pause_probe.py` currently depends on the v82 behavior and must be updated alongside that change. |
| `wpsMapMode` | Persist exactly | — | none beyond type-correctness | none yet |
| `wpsToolMode` | **Reset to default (new-format target differs)** | n/a (reset, not restored) | always `DefaultTool` regardless of the stored value | v82 writes the field, but load already ignores it and resets to `DefaultTool` per #103 — a currently-dead field being formally reclassified, not a behavior change; a future format could drop it entirely. None yet as a dedicated test. |
| `wpsEdits` | Persist exactly | `wpsGenParams` (edits replay onto regenerated terrain) | replayed edits must reproduce the pre-save surface exactly for every edited tile | `tools/multiworld_save_probe.py`, `tools/world_check.py` (determinism) |
| `wpsMineDesignations` | Persist exactly | referenced tile coordinates must be within the page | a claimant referencing a unit that failed to restore is not currently detected/cleared | none yet |
| `wpsConstructDesignations` | Persist exactly | referenced tile coordinates must be within the page | same claimant caveat as `wpsMineDesignations` | `tools/construction_probe.py` |
| `wpsGroundItems` | Persist exactly | — | instance ids must be below `sdNextItemInstanceId` (enforced by the max'd-never-lowered restore rule on the allocator, not by validating each item) | `tools/item_instance_probe.py` |
| `wpsSpoilPiles` | Persist exactly | referenced tile coordinates must be within the page | none beyond type-correctness | none yet |
| `wpsBuildings` | Persist exactly | `bmDefs` must already have every referenced building def (contract §4: missing def fails load) | referenced def names must resolve | `tools/multiworld_save_probe.py` |
| `wpsUnits` | Persist exactly | `umDefs` must already have every referenced unit def (contract §4: missing def fails load) | referenced def names must resolve | `tools/multiworld_save_probe.py` |
| `wpsUnitSimStates` | Persist exactly | `wpsUnits` (sim state is keyed by `UnitId`, restored after unit instances) | every sim-state key should correspond to a restored unit id (an orphaned key is not currently detected) | `tools/movement_probe.py` (post-load) |
| `wpsFloraHarvests` | Persist exactly | referenced tile coordinates must be within the page | none beyond type-correctness | `tools/flora_growth_probe.py` |
| `wpsChopDesignations` | Persist exactly | referenced tile coordinates must be within the page | same claimant caveat as `wpsMineDesignations` | `tools/chop_probe.py` |
| `wpsCraftBills` | Persist exactly | referenced station building must already be restored (`wpsBuildings`) | `cbClaimant`, if any, should reference a restored unit (not currently re-validated) | `tools/craft_bill_probe.py` |
| `wpsPowerNodes` | Persist exactly | referenced host building must already be restored (`wpsBuildings`) | none beyond type-correctness | `tools/power_probe.py` |
| `wpsTillDesignations` | Persist exactly | referenced tile coordinates must be within the page | same claimant caveat as `wpsMineDesignations` | `tools/till_probe.py` |
| `wpsCropPlots` | Persist exactly | referenced tile coordinates must be within the page | none beyond type-correctness | `tools/crop_probe.py` |
| `wpsPlantDesignations` | Persist exactly | referenced tile coordinates must be within the page | same claimant caveat as `wpsMineDesignations` | `tools/plant_probe.py` |
| `wpsIdentity` | Persist exactly | — | none beyond type-correctness | `tools/multiworld_save_probe.py` |

`SaveData` (`:438`) — global. `sdMetadata`/`sdGameTime`/`sdEnginePaused`/
`sdTexPalette`/`sdNextItemInstanceId` are restored first and unconditionally
(step 0 of `handleWorldLoadSaveCommand`), before any page — every other
field either has no cross-field dependency or is noted below:

| Field | Classification | Restoration dependency | Validation | Test oracle |
|---|---|---|---|---|
| `sdMetadata` | Persist exactly | — | none beyond type-correctness | `tools/multiworld_save_probe.py` |
| `sdGameTime` | Persist exactly | — | none beyond type-correctness | `tools/save_pause_probe.py` |
| `sdEnginePaused` | Persist exactly | — | authoritative over any Lua-side copy (contract, §7 `pause` module) | `tools/save_pause_probe.py` |
| `sdLuaModules` | Persist exactly (opaque, Lua-owned) | restored engine-side (live unit/building ids) BEFORE Lua's `deserializeAll` reconciles per-id state against them | see §7 for per-module rules | see §7 |
| `sdTexPalette` | Persist exactly | must restore before any page replays a `WeSetStructure` edit (palette-id → path resolution) | none beyond type-correctness | none yet |
| `sdNextItemInstanceId` | Persist exactly | — | restored as `max(current, saved)`, never lowered (#67), so post-load item creation can't collide with a loaded id | `tools/item_instance_probe.py` |
| `sdActivePage` | Persist as identity/reference | must name a page present in `sdWorlds` (falls back to the first page if not, per `activeWorldPage`) | resolves to a real restored page | `tools/multiworld_save_probe.py` |
| `sdVisiblePages` | Persist as identity/reference | pages must exist post-restore | none beyond type-correctness | `tools/multiworld_save_probe.py` |
| `sdWorlds` | Persist exactly (container) | — | each entry independently follows `WorldPageSave`'s rules above | `tools/multiworld_save_probe.py` |

## 5. Gameplay managers

`UnitManager` (`src/Unit/Types.hs:623`) — global:

| Field | Classification | Restoration dependency | Test oracle |
|---|---|---|---|
| `umDefs` | Rebuild | `data/units/*.yaml` | see §9 |
| `umInstances` | Persist exactly | via `UnitSnapshot`/`wpsUnits`, needs `umDefs` resolved first (missing species def fails load — contract §4) | `tools/multiworld_save_probe.py` |
| `umSelected` | Exclude | — | selections are cleared on load (contract §1) | none yet |
| `umNextId` | Persist exactly | `usnNextId` | `tools/item_instance_probe.py`-style id-collision reasoning (no dedicated probe) |

`BuildingManager` (`src/Building/Types.hs:179`) — global:

| Field | Classification | Restoration dependency | Test oracle |
|---|---|---|---|
| `bmDefs` | Rebuild | `data/buildings/*.yaml` | see §9 |
| `bmInstances` | Persist exactly | via `BuildingSnapshot`/`wpsBuildings`, needs `bmDefs` resolved first | `tools/multiworld_save_probe.py` |
| `bmNextId` | Persist exactly | — | none yet |
| `bmSelected` | Exclude | — | selections are cleared on load (contract §1) | none yet |

`UnitInstance` fields explicitly dropped by `fromUnitSnapshot`
(`src/World/Save/Types.hs:756`) — per-unit, reset rather than persisted:

| Field | Classification | Test oracle |
|---|---|---|
| `uiLastAttackerUid` | Reset to default | none yet |
| `uiLastAttackerAt` | Reset to default | none yet |
| `uiAnimOverride` | Reset to default | `tools/combat_anim_probe.py` (behavior, not persistence) |
| `uiFrozen` | Reset to default | none yet |
| `uiForceLoop` | Reset to default | none yet |
| `uiClimbDest` | Reset to default | `tools/movement_probe.py` (behavior, not persistence) |

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

| Item | Owner | Classification | Test oracle |
|---|---|---|---|
| `World.Thread`'s `lastTimeRef` | `src/World/Thread.hs:34` | Exclude | dt clock, reset at boot | none yet |
| `Unit.Thread`'s `lastTimeRef` | `src/Unit/Thread.hs:37` | Exclude | dt clock, reset at boot | none yet |
| `Combat.Thread`'s local `tick` counter | `src/Combat/Thread.hs:51` | Exclude | resets to 0 every restart, gates wound-tick rate only (contract §1: thread scheduling not persisted) | none yet |
| `Sim.Thread`'s `simStateRef` (`SimState`/`SimWorldState`/`SimChunkState`) | `src/Sim/Thread.hs:62`, `src/Sim/State/Types.hs` | Rebuild | fresh `SimChunkState` derives from chunk tile/fluid data as each chunk reactivates post-load; settled results already live in `wsTilesRef`/`wsEditsRef`, this is pure active-simulation scratch space | `tools/world_check.py` (fluid settle behavior) |
| `Lua.Thread`'s `lbsLuaState` (the Lua VM) | `src/Engine/Scripting/Lua/Thread.hs`, `src/Engine/Scripting/Lua/Types.hs:35` | Rebuild + Persist (mixed) | the VM itself is rebuilt fresh by re-running `loadScript` at boot; the specific durable slices of its global tables are what §7's `saveModules` registry persists — everything else in the VM is Exclude by omission | `tools/lua_orphan_prune_probe.py` |
| `Lua.Thread`'s `lbsScripts` (registered scripts + tick schedule) | `src/Engine/Scripting/Lua/Types.hs:22` | Exclude | rebuilt by the boot-time `loadScript` sequence | none yet |
| `Lua.Thread`'s `lbsNextScriptId` | `src/Engine/Scripting/Lua/Types.hs` | Exclude | rebuilt at boot | none yet |
| `Engine.Input.Thread` | `src/Engine/Input/Thread.hs` | — | no persistent thread-local state; local IORefs are recreated per-event inside handler scope | — |

## 7. Lua persistence registry (`scripts/lib/save_modules.lua`)

Exactly four modules call `saveMods.register(...)`. Each is a root
state owner under the contract §2 definition; the audit greps
`scripts/` for these call sites directly.

| Module | Owner | Classification | Restoration dependency | Test oracle |
|---|---|---|---|---|
| `unit_ai` | `scripts/unit_ai.lua:335` | Persist exactly (opaque blob) | live unit ids must already be restored (`umInstances`) so the pre-load-snapshot/restore dance (`unitAi._preLoadState`, #195/#191) can reconcile off-page units | `tools/lua_orphan_prune_probe.py` |
| `unit_resources` | `scripts/unit_resources.lua:68` | Reset to default | — | no serializer; `alerts.resetOnLoad()` clears the per-unit alert-debounce cache on every load, including a load with no blob at all — deliberately never persisted so a reused unit id (post `umNextId` rewind) can't inherit stale suppression state | none yet |
| `building_spawn` | `scripts/building_spawn.lua:274` | Persist exactly (opaque blob) | live building ids must already be restored (`bmInstances`); NOTE the roster-countdown itself is NOT here — it lives on `BuildingInstance` and is covered under `wpsBuildings` in §4 | none yet |
| `pause` (`paused` field) | `scripts/pause.lua:127` | Exclude (already dead) | — | the blob's `paused` value is read but ignored at load; `enginePausedRef`/`sdEnginePaused` is authoritative (see §1) | `tools/save_pause_probe.py` |
| `pause` (`prevTimeScale` field) | `scripts/pause.lua:127` | **Exclude (new-format target differs)** | — | currently persisted and restored; contract §1 ("the pre-save speed is not persisted") retargets this to Exclude. Not a runtime change in this issue — flagged for whichever child implements it (same child as `wpsTimeScale`, see §4). | `tools/save_pause_probe.py` (must be updated alongside) |

## 8. Camera / world-view / UI / tool / selection state

| Item | Owner | Scope | Classification | Test oracle |
|---|---|---|---|---|
| `WorldCamera` (`wcX`, `wcY`) | `src/World/Render/Camera/Types.hs:34` | per-page | Persist exactly | source of `wpsCameraX`/`wpsCameraY`, see §3/§4; none yet |
| `Camera2D` (`camPosition`, `camVelocity`, `camZoom`, `camZoomVelocity`, `camRotation`, `camDragging`, `camDragOrigin`, `camZSlice`, `camZTracking`) | `src/Engine/Graphics/Camera.hs:39` | global | Exclude | session-only render camera; re-synced from the active page's `WorldCamera` + `wpsCameraZoom`/`wpsCameraFacing` on load, not itself a source of truth |
| `Camera2D`'s `camFacing` | `src/Engine/Graphics/Camera.hs:39` | global | Persist as identity/reference | mirrors `wpsCameraFacing`, the true source of truth |
| `UICamera` (`uiCamWidth`, `uiCamHeight`) | `src/Engine/Graphics/Camera.hs:66` | global | Exclude | derived from window size |
| `CursorState` | `src/World/Cursor/Types.hs:9` | per-page | Exclude | already covered as `wsCursorRef` in §3 (contract §1 exclusion list: hover state) |
| `UIPageManager` | `src/UI/Types.hs:321` | global | Exclude | already covered as `uiManagerRef` in §1 |
| `FocusManager` | `src/UI/Focus.hs:34` | global | Exclude | already covered as `focusManagerRef` in §1 |
| `ToolMode` (per-page) | `src/World/Tool/Types.hs:12` | per-page | Reset to default | already covered as `wsToolModeRef`/`wpsToolMode` in §3/§4 |
| `UnitManager.umSelected` | `src/Unit/Types.hs:626` | global | Exclude | already covered in §5 |
| `BuildingManager.bmSelected` | `src/Building/Types.hs:183` | global | Exclude | already covered in §5 |
| `dragSelect` state (`state`, `startX/Y`, `currX/Y`, `page`) | `scripts/unit_drag_select.lua:20` | global (Lua module singleton) | Exclude | transient UI gesture FSM, not registered with `saveModules` — a drag in progress at save time is simply abandoned |
| Tool-script anchor/preview state (`mine_tool.lua`, `build_tool.lua`, `chop_tool.lua`, `till_tool.lua`, `plant_tool.lua`) | `scripts/*_tool.lua` | per-page (mirrors engine-side anchor fields) | Exclude | transient designation-in-progress UI state, not registered with `saveModules` |

## 9. Content-definition registries (current content, not persisted state)

All Rebuild — loaded fresh from YAML (or built in) at boot, referenced
by id/name from persisted instances rather than embedded. A missing
definition a persisted instance refers to fails loading per contract §4.

| Registry | Owner | Loader | Test oracle |
|---|---|---|---|
| `UnitDef` (via `UnitManager.umDefs`) | `src/Unit/Types.hs:357` | `src/Engine/Asset/YamlUnits.hs` | `tools/role_probe.py` and others (content behavior, not persistence) |
| `BuildingDef` (via `BuildingManager.bmDefs`) | `src/Building/Types.hs:47` | `src/Engine/Asset/YamlBuildings.hs` | `tools/construction_probe.py` |
| `ItemDef` / `ItemManager.imDefs` | `src/Item/Types.hs:136`, `:372` | `src/Engine/Asset/YamlItems.hs` | `tools/item_instance_probe.py` |
| `EquipmentClassManager.ecmDefs` | `src/Equipment/Types.hs:43` | `src/Engine/Asset/YamlEquipment.hs` | `tools/repair_item_probe.py` |
| `SubstanceManager.sbmDefs` | `src/Substance/Types.hs:43` | `src/Engine/Asset/YamlSubstance.hs` | none yet |
| `InfectionManager.infmDefs` | `src/Infection/Types.hs:46` | `src/Engine/Asset/YamlInfection.hs` | `tools/infection_probe.py` |
| `RecipeManager.rmDefs` | `src/Craft/Types.hs:119` | `src/Engine/Asset/YamlRecipes.hs` | `tools/craft_probe.py` |
| `LocationRegistry.lrDefs` | `src/Location/Types.hs:68` | `src/Engine/Asset/YamlLocations.hs` | `tools/location_content_probe.py` |
| `LootTableRegistry.ltrDefs` | `src/LootTable/Types.hs:34` | `src/Engine/Asset/YamlLootTables.hs` | none yet |
| `MaterialRegistry` | `src/World/Material.hs:233` | built-in, boot-time (fixed 256-slot table) | `tools/world_check.py` |
| `FloraCatalog` (`fcSpecies`, `fcWorldGen`, `fcNextId`) | `src/World/Flora/Types.hs:244` | `src/Engine/Asset/YamlFlora.hs` | `tools/flora_growth_probe.py` — note: this type derives `Serialize`/`Generic` unlike its sibling content registries, but nothing in `SaveData` embeds it; species are referenced by numeric id from world state instead. Flagged here, not changed — no code changes in this issue. |

---

## Summary: what's actually new here vs. what v82 already does

Every classification above matches v82's current behavior **except**
four, none of them implemented by this issue:

1. `wsTimeScaleRef` / `wpsTimeScale` — v82 persists it; target is
   Exclude (contract §1, "the pre-save speed is not persisted").
2. `pause.lua`'s `prevTimeScale` — v82 persists and restores it; target
   is Exclude (same rule).
3. `wsToolModeRef` / `wpsToolMode` — v82 writes the field, but the field
   is already dead at load (overridden to `DefaultTool` by #103); this
   is a reclassification of already-dead behavior, not a functional
   change.
4. `wmWorlds`/`wmVisible` (i.e. the load path as a whole) — v82's
   `handleWorldLoadSaveCommand` deliberately MERGES a loaded save into
   whatever's already live, preserving unrelated pages (#191/#218);
   target is whole-session replacement (contract §1). This is the
   largest divergence of the four — a load-path behavior, not a single
   field — see `persistence_contract.md`'s "Divergence: current loading
   merges, it does not replace" for the full writeup, why #191 made this
   choice deliberately, and the future child (A2) responsible for
   reconciling it with the new contract.

Everything else documents v82's existing, unchanged behavior under the
new taxonomy.
