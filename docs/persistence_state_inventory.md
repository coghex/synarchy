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
| `windowStateRef` | global | Exclude | — | OS/window-owned | none yet |
| `framebufferSizeRef` | global | Exclude | — | OS/window-owned | none yet |
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
| `nextItemInstanceIdRef` | global | Persist exactly | — | must restore as `max(loaded, current)`, never lower (#67) — the existing `sdNextItemInstanceId` rule | `tools/item_instance_probe.py` |
| `fontCacheRef` | global | Rebuild | font assets on disk | rebuilt at boot | none yet |
| `inputStateRef` | global | Exclude | — | live device state (keys/buttons down) | none yet |
| `keyBindingsRef` | global | Exclude | — | local runtime config (`config/keybinds.local.yaml`, #638/#786) | `tools/config_state_probe.py` |
| `currentKeyDownRef` | global | Exclude | — | transient dispatch flag, `Nothing` outside a key-down broadcast | none yet |
| `textBuffersRef` | global | Exclude | — | transient UI text-input buffers | none yet |
| `cameraRef` | global | Exclude | active page's `WorldCamera` + `wpsCameraZoom`/`wpsCameraFacing` | session-only render camera; see §8 for the per-page source of truth | none yet |
| `uiCameraRef` | global | Exclude | window size | derived from window size at boot | none yet |
| `uiManagerRef` | global | Exclude | — | entire UI tree rebuilt by Lua on load | none yet |
| `focusManagerRef` | global | Exclude | — | rebuilt as UI rebuilds | none yet |
| `worldManagerRef` | global | Rebuild | `SaveData.sdWorlds`, `sdActivePage`, `sdVisiblePages` | the container itself is rebuilt; each contained `WorldState` is restored per §3/§4 | `tools/multiworld_save_probe.py` |
| `hudActivePageRef` | global | Exclude | `wmVisible` | reset from the active/visible page set post-load | none yet |
| `loadStatusRef` | global | Exclude | — | Runtime-only whole-session load transaction diagnostics (#763), the load-side counterpart to `saveBarrierRef`; never serialized. | `Test.Headless.Load.Status`, `tools/transactional_load_probe.py` |
| `pendingLoadRef` | global | Exclude | — | Runtime-only single-slot staged-session handoff (#763) between `WorldLoadTransaction` (staging) and `WorldLoadPublish` (the atomic swap); never serialized, and always cleared before/after use. | `tools/transactional_load_probe.py` |
| `inputThreadActiveRef` | global | Exclude | — | Runtime-only boot-mode flag (#763 round 4): True once `Engine.Input.Thread.startInputThread` actually launched (never happens under `App.Headless`); consulted to decide whether `SaveInput` belongs in a save/load transaction's owner set, never serialized. | `tools/transactional_load_probe.py`, `tools/save_barrier_probe.py` |
| `worldQueue` | global | Exclude | — | transport queue; see contract §3 | none yet |
| `sunAngleRef` | global | Rebuild | active page's world time | derived via `worldTimeToSunAngle` | none yet |
| `worldPreviewRef` | global | Exclude | — | pending GPU upload payload | none yet |
| `zoomAtlasDataRef` | global | Exclude | — | pending GPU upload payload | none yet |
| `screenshotRequestQueue` | global | Exclude | — | debug-only transport queue; see contract §3 | none yet |
| `worldQuadsRef` | global | Rebuild | loaded chunk data | render cache | none yet |
| `textureSystemRef` | global | Exclude | — | GPU bindless texture system | none yet |
| `samplerCacheRef` | global | Exclude | — | GPU sampler cache | none yet |
| `textureSizeRef` | global | Exclude | — | GPU texture dimension cache | none yet |
| `bloodDisposeQueue` | global | Exclude | — | transient cross-thread GPU-dispose transport queue (#788); see contract §3 | `tools/blood_gpu_lifecycle_probe.py` |
| `defaultFaceMapSlotRef` | global | Exclude | — | GPU texture slot | none yet |
| `floraCatalogRef` | global | Rebuild | `data/*.yaml` flora content | reloaded fresh from YAML at boot; species referenced by numeric id from world state (see §9 re: its unused `Serialize` instance) | `tools/flora_growth_probe.py` |
| `materialRegistryRef` | global | Rebuild | built-in material table | boot-time, not YAML-driven | none yet |
| `unitManagerRef` | global | Rebuild | see §5 (`UnitManager` fields classified individually) | the IORef itself is always freshly allocated at boot; the interesting classification decisions live on `UnitManager`'s own fields (§5) | none yet |
| `unitQueue` | global | Exclude | — | transport queue; see contract §3 | none yet |
| `utsRef` | global | Rebuild | `wpsUnits`/`wpsUnitSimStates` after load | sim-side per-unit pos/pose/target/path rebuilt from the restored `UnitInstance`/`UnitSimState` snapshot, not itself directly serialized | `tools/movement_probe.py` (post-load steering sanity) |
| `statRNGRef` | global | Exclude | — | explicitly non-deterministic, not save-seeded (contract §1) | none yet |
| `buildingManagerRef` | global | Rebuild | see §5 (`BuildingManager` fields classified individually) | the IORef itself is always freshly allocated at boot; the interesting classification decisions live on `BuildingManager`'s own fields (§5) | none yet |
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
| `saveBarrierRef` | global | Exclude | — | Runtime-only coordinated-save request/owner acknowledgement diagnostics; never serialized. | save-barrier hspec + headless probe |
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
| `assetConfig` | global | Exclude | — | rebuilt at boot | none yet |
| `sceneManager` | global | Exclude | — | scene graph, rebuilt by UI/world render setup each session | none yet |

## 3. `WorldManager` / `WorldState` (`src/World/State/Types.hs`)

### WorldManager

`WorldManager` (`:261`) — global:

| Field | Classification | Restoration dependency | Validation | Test oracle |
|---|---|---|---|---|
| `wmWorlds` | Rebuild | `SaveData.sdWorlds` | publication REPLACES the whole set (`World.Load.Publish.publishStagedSession`), never merges — see the resolved-divergence note below | `tools/multiworld_save_probe.py`, `tools/transactional_load_probe.py` |
| `wmVisible` | Rebuild | `SaveData.sdVisiblePages` | none beyond type-correctness | `tools/multiworld_save_probe.py` |

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
| `wsCameraRef` | Persist exactly | — | `wpsCameraX`/`wpsCameraY` | none yet |
| `wsTexturesRef` | Exclude | — | runtime GPU handles | none yet |
| `wsGenParamsRef` | Persist exactly | — | `wpsGenParams`; deliberately meaningful seed data (contract §1) | `tools/multiworld_save_probe.py` |
| `wsTimeRef` | Persist exactly | — | `wpsTimeHour`/`wpsTimeMinute` | `tools/save_pause_probe.py` |
| `wsDateRef` | Persist exactly | — | `wpsDateYear`/`wpsDateMonth`/`wpsDateDay` | `tools/flora_growth_probe.py` |
| `wsTimeScaleRef` | Exclude | — | **Implemented by #757/#758**: `World.Save.Snapshot.PageSnapshot` has no time-scale field at all; the temporary adapter fabricates `wpsTimeScale = 1` for every page, unconditionally. | `tools/save_pause_probe.py` |
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
| `smTimestamp` | Persist exactly | — | none beyond type-correctness (display only) | none yet |
| `smWorldName` | Persist exactly | the active page's `wpsIdentity` at save time | mirrors that page's identity; `Nothing` for an unnamed world | `tools/multiworld_save_probe.py` |
| `smWorldGloss` | Persist exactly | `smWorldName` | must be `Nothing` whenever `smWorldName` is `Nothing` (a gloss cannot exist without a display name) | `tools/multiworld_save_probe.py` |

### WorldPageSave

`WorldPageSave` (`:325`) — per-page. Every field below whose
restoration dependency isn't otherwise noted needs only its own page's
prior fields (no cross-page ordering requirement):

| Field | Classification | Restoration dependency | Validation | Test oracle |
|---|---|---|---|---|
| `wpsPageId` | Persist as identity/reference | — | restored verbatim, unchanged (#763: no remap, no collision rename — `assignRestoreIds`/`RestoreIds.hs` are gone, since replacement never collides with anything) | `tools/multiworld_save_probe.py`, `tools/transactional_load_probe.py` |
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
| `wpsTimeScale` | Exclude (legacy field fabricated by the adapter) | n/a | n/a | **Implemented by #757, cleanly modeled by #758** (contract §1, "the pre-save speed is not persisted"): the field is still WRITTEN — v88's positional format can't drop it — but `World.Save.Snapshot.Adapter.snapshotToSaveData` always fabricates `1`, never a captured value (#757 already hardcoded this at the old inline call site; #758 gives it an explicit, tested home). `tools/save_pause_probe.py` was updated by #757 to match. |
| `wpsMapMode` | Persist exactly | — | none beyond type-correctness | none yet |
| `wpsToolMode` | Reset to default (legacy field fabricated by the adapter) | n/a (reset, not restored) | always `DefaultTool` regardless of the stored value | v82 writes the field, but load already ignores it and resets to `DefaultTool` per #103 — a currently-dead field. **#758**: `World.Save.Snapshot.PageSnapshot` has no tool-mode field at all; the adapter fabricates `DefaultTool` for every page, so the "no captured value, only a fixed default" contract is now explicit at the type level, not just at load time. |
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
| `sdTexPalette` | Persist exactly | must restore before any page replays a `WeSetStructure` edit (palette-id → path resolution) | none beyond type-correctness | none yet |
| `sdNextItemInstanceId` | Persist exactly | — | restored as `max(current, saved)`, never lowered (#67), so post-load item creation can't collide with a loaded id | `tools/item_instance_probe.py` |
| `sdActivePage` | Persist as identity/reference | must name a page present in `sdWorlds` (falls back to the first page if not, per `activeWorldPage`) | resolves to a real restored page | `tools/multiworld_save_probe.py` |
| `sdVisiblePages` | Persist as identity/reference | pages must exist post-restore | none beyond type-correctness | `tools/multiworld_save_probe.py` |
| `sdWorlds` | Persist exactly (container) | — | each entry independently follows `WorldPageSave`'s rules above | `tools/multiworld_save_probe.py` |

## 5. Gameplay managers

`UnitManager`/`BuildingManager` are not in `ROOT_RECORDS` (they aren't
fields of `EngineEnv`/`WorldState` themselves — `unitManagerRef`/
`buildingManagerRef`, the fields that ARE, are classified in §1 as
pointers into here), so the audit doesn't scan them; they're inventoried
per issue requirement 3 for completeness and get owner headings for
navigability, same as everything else.

### UnitManager

`UnitManager` (`src/Unit/Types.hs:623`) — global:

| Field | Classification | Restoration dependency | Validation | Test oracle |
|---|---|---|---|---|
| `umDefs` | Rebuild | `data/units/*.yaml` | none beyond type-correctness | see §9 |
| `umInstances` | Persist exactly | via `UnitSnapshot`/`wpsUnits`, needs `umDefs` resolved first | missing species def fails load (contract §4) | `tools/multiworld_save_probe.py` |
| `umSelected` | Exclude | — | selections are cleared on load (contract §1) | none yet |
| `umNextId` | Persist exactly | `usnNextId` | must exceed every restored `UnitId` so post-load spawns can't collide | `tools/item_instance_probe.py`-style id-collision reasoning (no dedicated probe) |

### BuildingManager

`BuildingManager` (`src/Building/Types.hs:179`) — global:

| Field | Classification | Restoration dependency | Validation | Test oracle |
|---|---|---|---|---|
| `bmDefs` | Rebuild | `data/buildings/*.yaml` | none beyond type-correctness | see §9 |
| `bmInstances` | Persist exactly | via `BuildingSnapshot`/`wpsBuildings`, needs `bmDefs` resolved first | missing building def fails load (contract §4) | `tools/multiworld_save_probe.py` |
| `bmNextId` | Persist exactly | — | must exceed every restored `BuildingId` so post-load spawns can't collide | none yet |
| `bmSelected` | Exclude | — | selections are cleared on load (contract §1) | none yet |

### UnitInstance (reset-on-load fields)

`UnitInstance` fields explicitly dropped by `fromUnitSnapshot`
(`src/World/Save/Types.hs:756`) — per-unit, reset rather than persisted.
Not in `ROOT_RECORDS` either (these are individual fields WITHIN
`UnitInstance`, which is itself reached only via `umInstances` above,
already covered):

| Field | Owner | Scope | Classification | Restoration dependency | Validation | Test oracle |
|---|---|---|---|---|---|---|
| `uiLastAttackerUid` | `src/World/Save/Types.hs:756` | per-unit | Reset to default | — | always `Nothing` post-load | none yet |
| `uiLastAttackerAt` | `src/World/Save/Types.hs:756` | per-unit | Reset to default | — | always `Nothing` post-load | none yet |
| `uiAnimOverride` | `src/World/Save/Types.hs:756` | per-unit | Reset to default | — | always cleared post-load | `tools/combat_anim_probe.py` (behavior, not persistence) |
| `uiFrozen` | `src/World/Save/Types.hs:756` | per-unit | Reset to default | — | always `False` post-load | none yet |
| `uiForceLoop` | `src/World/Save/Types.hs:756` | per-unit | Reset to default | — | always `False` post-load | none yet |
| `uiClimbDest` | `src/World/Save/Types.hs:756` | per-unit | Reset to default | — | always `Nothing` post-load | `tools/movement_probe.py` (behavior, not persistence) |

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
| `Lua.Thread`'s `lbsLuaState` (the Lua VM) | `src/Engine/Scripting/Lua/Thread.hs`, `src/Engine/Scripting/Lua/Types.hs:35` | global (one Lua VM for the whole engine) | Rebuild | boot-time `loadScript` sequence, then §7's `saveModules.prepareLoad`/`applyAll` | the VM CONTAINER itself is rebuilt fresh by re-running `loadScript` at boot — this single "Rebuild" classification is for that container, not a blanket claim about everything inside it. The specific durable SLICES of its global tables are separately classified, one label each: §7's two `saveModules`-registered persistent components are `Persist exactly` in their own right, its one `registerResetHook`-registered module is `Reset to default`; everything else in the VM's global state is `Exclude` by omission (never touched by save/load). | `tools/lua_orphan_prune_probe.py` |
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
Exactly two modules call `saveMods.register(...)`; each is a root state
owner under the contract §2 definition, and the audit scans `scripts/`
for these call sites directly and checks each registered name against
the classifications below.

A module with no durable state calls `saveMods.registerResetHook(id,
resetFn)` instead (`unit_resources` below) — this is NOT a save
component (no version, no envelope entry, not scanned by the audit),
just a callback run on every load. `pause` no longer registers at all
(neither `register` nor `registerResetHook`) — see its row below.

### Lua persistence registry

| Module | Owner | Scope | Classification | Restoration dependency | Validation | Test oracle |
|---|---|---|---|---|---|---|
| `unit_ai` | `scripts/unit_ai.lua` | global (per-id state keyed inside the component payload) | Persist exactly (versioned component, v1) | live unit ids must already be restored (`umInstances`) | component-local: payload must be a table keyed by positive-integer unit ids mapping to state tables; the pre-load-snapshot/restore dance (`unitAi._preLoadState`, #195/#191) reconciles per-id state against the survivor set — since #763 (C2) a load replaces the complete session, so `onSaveLoaded`'s survivor lists always name every live unit and the dance's off-page-preservation branch is dead code in normal operation, kept only as a defensive no-op | `tools/lua_orphan_prune_probe.py`, `Test.Headless.Lua.SaveModules` |
| `building_spawn` | `scripts/building_spawn.lua` | global (per-id state keyed inside the component payload) | Persist exactly (versioned component, v1) | live building ids must already be restored (`bmInstances`) | same reconcile requirement as `unit_ai` (temporary pre-C2 compatibility adapter); NOTE the roster-countdown itself is NOT here — it lives on `BuildingInstance` and is covered under `wpsBuildings` in §4 | `tools/lua_orphan_prune_probe.py`, `Test.Headless.Lua.SaveModules` |
| `unit_resources` | `scripts/unit_resources.lua` | global (per-id cache) | Reset to default | — | reset hook (`registerResetHook`, not a save component); `alerts.resetOnLoad()` must clear the per-unit alert-debounce cache on every load, including a load with no data for this module at all — deliberately never persisted so a reused unit id (post `umNextId` rewind) can't inherit stale suppression state | none yet |
| `pause` (`paused` field) | `scripts/pause.lua` | global | Exclude | — | no registration of any kind (requirement 5) — `pause.paused` is an in-memory transition-detection hint only, never read for real logic; `enginePausedRef`/`sdEnginePaused` is authoritative (see §1) | `tools/save_pause_probe.py` |
| `pause` (`prevTimeScale` field) | `scripts/pause.lua` | global | Exclude | — | `pause.onSaveLoaded` always resets it to `1.0` regardless of what an older save contained — a loaded session resumes at default speed (contract §1, "the pre-save speed is not persisted") | `tools/save_pause_probe.py` |

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
| `FocusManager` | `src/UI/Focus.hs:34` | global | Exclude | — | already covered as `focusManagerRef` in §1 | none yet |
| `ToolMode` (per-page) | `src/World/Tool/Types.hs:12` | per-page | Reset to default | — | already covered as `wsToolModeRef`/`wpsToolMode` in §3/§4 | none yet |
| `UnitManager.umSelected` | `src/Unit/Types.hs:626` | global | Exclude | — | already covered in §5 | none yet |
| `BuildingManager.bmSelected` | `src/Building/Types.hs:183` | global | Exclude | — | already covered in §5 | none yet |
| `dragSelect` state (`state`, `startX/Y`, `currX/Y`, `page`) | `scripts/unit_drag_select.lua:20` | global (Lua module singleton) | Exclude | — | transient UI gesture FSM, not registered with `saveModules` — a drag in progress at save time is simply abandoned | none yet |
| Tool-script anchor/preview state (`mine_tool.lua`, `build_tool.lua`, `chop_tool.lua`, `till_tool.lua`, `plant_tool.lua`) | `scripts/*_tool.lua` | per-page (mirrors engine-side anchor fields) | Exclude | — | transient designation-in-progress UI state, not registered with `saveModules` | none yet |
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
| `WorldPagesDTO` | `world-pages` | Persist exactly | the world page — page-set authority: identity, gen params, dates/clocks, map mode, per-page camera |
| `WorldEditsDTO` | `world-edits` | Persist exactly | world edit layer — per-page terrain + structure edit log |
| `WorldActivityDTO` | `world-activity` | Persist exactly | mutable-world-activity layer — designations, flora harvests, crop plots, ground items, spoil |
| `BuildingsDTO` | `buildings` | Persist exactly | `BuildingManager` — per-page building snapshot |
| `UnitsDTO` | `units` | Persist exactly | `UnitManager` — per-page unit snapshot (stats/skills/equipment/inventory/wounds) |
| `UnitSimDTO` | `unit-sim` | Persist exactly | `UnitThreadState` — per-page unit sim state (paths/targets/poses/deadlines) |
| `CraftBillsDTO` | `craft-bills` | Persist exactly | `Craft.Bills` — per-page craft-bill queue |
| `PowerNodesDTO` | `power-nodes` | Persist exactly | `Power.Types` — per-page power-node registry + stored charge |

This table is the STATIC Haskell-owned component set only. Since issue
#761 (save-overhaul B3) there is no `"lua-state"` transitional entry
here at all: each module registered with
`scripts/lib/save_modules.lua` is its OWN dynamically-added envelope
component (`"lua.<module>"`, disjoint reserved namespace —
`World.Save.Component.Types.luaComponentPrefix`), gathered from the
live Lua registry at save/load time rather than declared statically in
Haskell. See §7 for the Lua-owned component list.

---

## Summary: what's actually new here vs. what v82 already does

Every classification above matches v82's current behavior **except**
four, none of them implemented by this issue (#756):

1. `wsTimeScaleRef` / `wpsTimeScale` — v82 persisted it; target was
   Exclude (contract §1, "the pre-save speed is not persisted").
   **Implemented by #757/#758**: see the row above and CLAUDE.md's
   "Session snapshot (#758, save-overhaul A3)" note.
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
