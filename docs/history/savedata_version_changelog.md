# SaveData / WorldPageSave version changelog (superseded — context only)

This is the per-version changelog that used to live as a comment attached to
`currentSaveVersion` in `src/World/Save/Types.hs` (moved out by issue #984).
It tracked bumps of that one `Int` — which reached 91 — recording whatever
changed `SaveData`/`WorldPageSave`'s serialized shape or otherwise forced a
whole-file save-version rejection.

The record is sparse, not a complete v1-v91 ledger. There is no v1 entry —
the comment starts at v2 — and these version numbers between v2 and v91
were never documented with their own header either: v14, v16-v21, v28-v32,
v38-v47, v51, v53, v56. (v38 is a special case: its header text was lost
before this move, but an orphaned fragment of its body survives and is
preserved verbatim below, unattributed, where the source had it.) This
file preserves exactly what the source comment recorded, without inventing
entries to fill the gaps.

The scheme this changelog describes no longer governs save compatibility.
The persistence overhaul (#756-#768) replaced whole-file version rejection
with per-component versioning — each envelope component carries its own
`ccVersion`/`cdVersion` and migrates from frozen DTOs (see
`World/Save/Envelope.hs` and `docs/persistence_contract.md`). v90 below is
the bump that made that switch. `currentSaveVersion` survives only as the
schema version of the transitional in-memory `SaveData`/`WorldPageSave` load
bridge — see its Haddock in `World/Save/Types.hs` for what it still means.

Kept here for provenance and git-archaeology, not as a live compatibility
contract.

The source comment was never a single ordered list: the file historically
attached its oldest entries (v2-v27) as a leading Haddock block above the
`currentSaveVersion` declaration, then grew newer ones (v33 onward, plus a
few gaps for undocumented bumps) as a trailing right-hand-side comment that
got a new entry prepended each time. The two sections below mirror that same
split rather than force one unified order.

One correction from issue #984's review is applied here. The two versions
and their correct issues are: **v61** is `UnitSimState`'s vestigial
`usFallImpact` removal, issue **#386** (commit `97beb0f37`); **v62** is the
`rpMeanderSeed`/`rscMeanderSeed` removal, issue **#385** (commit
`9dc960c17`). The source comment had these merged and misattributed: commit
`9dc960c17` overwrote the trailing block's `v61` header with a `v62` header
but left v61's original body text underneath it untouched, producing one
entry labeled `v62` whose body actually described v61's change (and so
carried v61's #386, mislabeled as v62's issue). v62's own accurate text —
correctly citing #385 — had, separately, been appended to the leading block
instead of the trailing one (see that section's last entry, below). Both
versions are restored here as distinct, correctly attributed entries in the
trailing block, in their proper chronological position; nothing else was
reordered or reworded beyond that.

## Leading block (v2 – v27)

- **v2** — introduced the version header + `sdGameTime` + `sdEnginePaused`.
- **v3** (Phase 2) — added the per-chunk edit log so player tile changes
  survive eviction + save/load.
- **v4** (Phase 3) — added the building snapshot.
- **v5** (Phase 4) — added units + sim state.
- **v6** (Phase 5) — adds Lua module blobs (`aiState`, `building_spawn`
  state, pause state).
- **v7** — drops `sdClimate` + `sdRiverFlow`. Both fields round-tripped
  empty HashMaps every save (no producer code ever wrote to the backing
  IORefs); live climate state lives inside `sdGenParams`.
- **v8** (Phase 2 equipment) — adds `uisEquipped` to `UnitInstanceSnapshot`:
  a slot id → `ItemInstance` map persisting equipped gear.
- **v9** — adds `iiQuality` + `iiCondition` to `ItemInstance` (per-item
  rolled state). Positional `Serialize` means any field added to
  `ItemInstance` bumps the format; can't be reverse-compatible without a
  migration shim.
- **v10** — adds `uisAccessories` to `UnitInstanceSnapshot` — items worn off
  the silhouette (robes, goggles, rings…). Empty list is legal for units
  without any accessory slots.
- **v11** — adds `bisBuildProgress` to `BuildingInstanceSnapshot` —
  accumulated worker-seconds toward `bdBuildWork`. Required so a save
  mid-construction restores at the same progress fraction (otherwise the
  building would either reset to 0 or skip to Built depending on the new
  `currentActivity` branch).
- **v12** — adds `bisMaterialsDelivered` to `BuildingInstanceSnapshot` — the
  full `ItemInstance` list for each material type consumed into the build.
  Required so an electric motor that went in at 100% condition comes back
  out at its then-current condition on a future deconstruction recovery.
- **v13** — adds `bisStorage` to `BuildingInstanceSnapshot` — cargo
  currently deposited in the building (separate from delivered build
  materials). Same `ItemInstance` preservation rule so deposited items
  retain their quality/condition through save/load and through
  deposit→withdraw round-trips.
- **v15** — adds `lcWaterTableMap` to `LoadedChunk` for the water-table
  river rework (Phase A). Chunks are transient so the field itself doesn't
  round-trip via saves, but other shape/field deletions on `RiverSegment`
  in Phase B will. Bumping now so any pre-rework saves are clearly rejected
  before Phase B lands.
- **v22** — removes the `EruptionEvent` constructor + `LavaFlow` record
  from `GeoEvent` / `Timeline.Types` as part of the lava-v1 phase-3
  cleanup. Lava placement is now driven by the pure-function Magma system;
  per-period eruption rolls are gone. Constructor tag shift makes the
  schema incompatible with v21.
- **v23** — adds `gtWorldLavaPools` (WorldLakes-shaped lava pool table) to
  `GeoTimeline` — surface lava placed with pooling semantics instead of the
  per-tile film. Positional Generic `Serialize` gains a trailing field,
  incompatible with v22.
- **v24** — adds `wgpLavaPoolDepth` / `wgpLavaPoolRadius` (volcanism config
  levers) to `WorldGenParams`' manual `Serialize`.
- **v25** — adds `gtCoastal` (global coastal-erosion table) to
  `GeoTimeline` — coastal erosion moved from the per-chunk windowed pass to
  a world-init global pass on the stitched terrain (cross-window coastline
  divergence / seam-cliff fix).
- **v26** — adds `gtSeabed` (global seabed table) to `GeoTimeline` —
  ocean-floor relief (depth-from-shore ramp + noise replacing the flat
  seaLevel−1 basin carve) + seabed materials + bedrock outcrops.
- **v27** — adds `gtWorldOcean` (tile-resolution edge-connected ocean
  bitmask) to `GeoTimeline` — `composeFluidMap` ORs it into the
  chunk-level ocean test so sub-sea tiles the coarse chunk-flood missed
  render ocean (sea-stops-at-chunk-boundary fix).
- **v62** — removes the dead `rpMeanderSeed` (`RiverParams`) and
  `rscMeanderSeed` (`RiverSegmentCarve`) fields. Both rode along in the
  serialized `wgpGeoTimeline` (`GeoTimeline` → `gtPeriods` → `gpEvents` →
  `GeoEvent`, e.g. `HydroEvent (RiverFeature _)`) but were never read — the
  meander noise they seeded was discarded by the river carve. Positional
  Generic `Serialize` drops the trailing field, incompatible with v61
  (#385). *(This entry was appended here, out of chronological order, by
  the same commit that bumped the version to 62 — see the note above; kept
  in its original position rather than moved down to sit next to v61.)*

## Trailing block (v33 – v91, newest first)

- **v91** (#761, save-overhaul B3) — `sdLuaModules` removed — Lua-owned
  state no longer rides through `SaveData` at all; each registered Lua
  module is its own dynamically-added envelope component
  (`World.Save.Component.Types`'s `luaComponentPrefix`), gathered/applied
  by `Engine.Scripting.Lua.API.Save` directly, never part of this
  transitional load-bridge shape.
- **v90** (#759, save-overhaul B1) — no layout change to
  `SaveData`/`WorldPageSave` themselves — this bump marked the transition
  to the tagged, checksummed envelope format (`World.Save.Envelope`).
  `SaveData` now rides as the "session" component's payload inside that
  envelope rather than being the whole file; `currentSaveVersion` is that
  component's own schema version (unchanged meaning — still bump it
  whenever this record's or `WorldPageSave`'s layout changes), kept
  deliberately separate from the envelope's own framing version
  (`World.Save.Envelope.currentEnvelopeVersion`).
- **v89** — `WorldGenParams` gains trailing `wgpLocationDiscovered` (#780)
  — a per-chunk one-time discovery flag (player-faction unit has entered a
  placed location's discovery-margin halo), independent of
  `wgpLocationStamped`/`wgpLocationContentsSpawned`.
- **v88** — `CraftBill` gains `cbMode`/`cbTarget`/`cbOutputItem` (#795) —
  the persisted until-stock craft-bill mode
  (`Craft.Bills.addUntilStockBill`), appended after `cbWorking`.
- **v87** — `ConstructDesignation` gains `cdMaterialsPaid` (#799) — a
  durable payment marker so a structure designation's material cost, once
  taken from a claimant's inventory, is never charged a second time to a
  replacement worker after the original claimant dies or the world is
  reloaded.
- **v86** — worldgen-output change (#812) — final-age mountain soil shed
  is now redistributed to the adjacent lower/gentler receiving terrain
  instead of simply deleted (`World.Geology.Erosion.Math` `shedCredit`),
  closing out #225's redistribution requirement that PR #279 left undone.
- **v85** — `WorldEdit` gains trailing simulation-fluid snapshot
  constructors, so a coordinated save can replay pre-boundary World → Sim
  → World fluid writebacks while the loaded game remains paused.
- **v84** — worldgen-output change (#811) — Tiny(32)/Small(64) worlds now
  get the inland-origin river-source extension (issue #221) instead of the
  old `worldSize>=128` gate, guarded by a new caldera/supervolcano hazard
  check so the extension can't breach a caldera and flood lava through the
  newly-carved valley.
- **v83** — worldgen-output change, not a layout change (#785). Final
  regional climate is now rebuilt from the completed timeline's own
  evolved CO2/solar forcing instead of hardcoded baseline forcing
  (1.0/0.0/1.0) with only the `csGlobalCO2`/`csGlobalTemp`/`csSolarConst`
  summary fields patched from the timeline afterward — so a save's stored
  regional climate grid differs from pre-#785 saves for the same seed.
- **v82** — player-facing world identity (#707). `WorldPageSave` gains a
  trailing `wpsIdentity` (`Maybe WorldIdentity`: display name + optional
  gloss) and `SaveMetadata` gains trailing `smWorldName` / `smWorldGloss`
  (the ACTIVE page's identity, for save listing). Both records are
  positional Generic `Serialize`, so the appended fields shift the layout.
- **v81** — `Pose` (`Unit.Sim.Types`) gains a new trailing `Sleeping`
  constructor (#612) for the circadian sleep-goal AI. Appended at the end,
  per `Pose`'s own append-only policy.
- **v80** — `CraftBill` (`Craft.Bills`) gains a new trailing `cbWorking`
  field (#590) — is the claimant CURRENTLY pouring work into this cycle
  (vs. still fetching/walking), which `Power.Network.activeCraftConsumersOn`
  now keys its live demand off instead of "claimed and not paused".
  Appended at the end, per this record's own append-only policy.
- **v79** — `WorldEdit` gains a new trailing `WePlaceFlora` constructor
  (#336) — the farm AI's row-crop planting completion
  (`world.plantRowCropAt`). Order-preserving (appended at the end), but
  per this module's own policy any new `WorldEdit` variant bumps the
  version.
- **v78** — `WorldPageSave` gains trailing `wpsPlantDesignations` (#335) —
  plant designations (tile → surface z + chosen crop species). Like the
  other designation layers, restored straight into
  `wsPlantDesignationsRef`; markers re-render from the stored z. `ToolMode`
  also gains `PlantTool` (appended, tag 7).
- **v77** — `WorldPageSave` gains trailing `wpsCropPlots` (#334) — planted
  groundcover-crop tiles (species + planted day + health). Like the
  designation layers, restored straight into `wsCropPlotsRef`; the render
  pass derives the current growth texture from it with no chunk loading
  needed.
- **v76** — `WorldPageSave` gains trailing `wpsTillDesignations` (#333) —
  till designations (tile → surface z), same shape as
  `wpsChopDesignations`. `ToolMode` also gains `TillTool` (appended, tag
  6).
- **v75** — `PowerNode` (nested in `WorldPageSave`'s `wpsPowerNodes`) gains
  trailing `pnStoredWh` (#360) — a storage node's current charge, the one
  piece of the network energy balance that must survive save/load
  (connectivity + generation/drain are recomputed fresh from wire + node
  positions).
- **v74** — `CraftBill` (nested in `WorldPageSave`'s `wpsCraftBills`) gains
  trailing `cbSeq` and `cbPaused` (#330's manual-reorder + pause bill
  controls).
- **v73** — `WorldPageSave` gains trailing `wpsPowerNodes` (#358) — the
  per-world power-node registry (placed solar-panel/battery source +
  storage nodes; role + peak watts / capacity Wh; `Power.Types`).
- **v72** — `WorldPageSave` gains trailing `wpsCraftBills` (#329) — the
  per-world craft-bill queue (station orders + claim/progress state;
  `Craft.Bills`).
- **v71** — `UnitSimState` gains `usMoveGrade` (uphill slope
  speed/stamina, #375) — positional Generic `Serialize`, so the appended
  field shifts the record layout.
- **v70** — `WorldGenParams` gains trailing `wgpLocationStamped` (#424) —
  a dedicated one-time geometry-stamp flag per chunk, replacing the
  `structure.hasAt(gx,gy,"floor")` inference the lazy location stamper
  used to decide "already materialized". That check was fooled by a
  player clearing the anchor floor tile: the location was still stamped,
  but the guard saw "no floor" and re-ran the builder, clobbering edits.
  The new flag is independent of structure edits, like the existing
  `wgpLocationContentsSpawned` (#90).
- **v69** — coastline variety (#220) — worldgen OUTPUT change, no schema
  change. The serialized `CoastalTable` (`GeoTimeline` → `gtCoastal`) now
  reaches 28 tiles inland and encodes steepness-driven coast profiles;
  base terrain near convergent land-ocean margins keeps its mountains
  (continentalShelf modulation). Old saves would replay stale coastlines
  against new chunk regen.
- **v68** — `ItemInstance` gains trailing `iiTemp` (`Maybe Float`, °C;
  `Nothing` = at ambient) — item temperature + cooling toward the tile's
  ambient (#344). Rides in every serialized item (ground items, unit
  inventory/equipment/accessories snapshot, building storage/materials).
- **v67** — `WorldPageSave` gains trailing `wpsChopDesignations` (#97) —
  chop designations (tile → surface z). `ToolMode` also gains `ChopTool`
  (appended, tag 5).
- **v66** — `WorldPageSave` gains trailing `wpsFloraHarvests` (#94) —
  harvested flora tiles with live regrowth timers.
- **v65** — two-layer food model (#93) — the persisted "hunger" `uiStats`
  entry is redefined from energy store to STOMACH meter (`max_hunger`
  halves to `bm*10`) and a new "calories"/"max_calories" store takes over
  catabolism/thermo/heal gating. `ItemContainer` gains `icDefaultFill` and
  `ItemFood` gains `ifCaloriesPerKg` (both Generic `Serialize`,
  positional).
- **v64** — `WorldGenParams` gains trailing `wgpLocationContentsSpawned`
  (one-time content-spawn flag per chunk, independent of the
  structure-geometry idempotency check; #90).
- **v63** — `WorldGenParams` gains trailing `wgpLocationOverlay` (sparse
  chunk→location-id map placed at world init; serialized so a loaded world
  keeps its layout without recomputation; #89).
- **v62** — drop dead `rpMeanderSeed`/`rscMeanderSeed` from the serialized
  `GeoTimeline` (write-only scalar, never read; #385). See the fuller
  rationale under the leading block, above — both entries describe the
  same bump.
- **v61** — `UnitSimState` drops vestigial `usFallImpact` (write-only
  scalar, never read; #386). Falls route through `usPendingFallDrop` +
  `Unit.Fall`.
- **v60** — `WorldPageSave` gains `wpsConstructDesignations` (construction
  designation layer, #95).
- **v59** — `SaveData` restructured — per-world state moved off
  `SaveData` into a new `WorldPageSave` record; `SaveData` now holds
  globals + `sdActivePage` + `sdVisiblePages` + `sdWorlds`
  (`[WorldPageSave]`). Foundation for one-save-all-pages (#215, epic
  #214).
- **v58** — steep faces shed soil to bare rock in last-age erosion (#225).
- **v57** — per-unit name (`uisName`) (#264).
- **v55** — despike spike-only convergence pass (#254) — base terrain
  output shifts on regen, so reject older saves.
- **v54** — structure edits (`WeSetStructure`/`WeClearStructure`) +
  `sdTexPalette`.
- **v52** — `UnitSimState` gains `usJumpApex` (leap arc state).
- **v50** — `UnitInstance` gains `uiImmuneResponse` + `uiImmunities`
  (immunity system).
- **v49** — `Wound` gains `woundInfectionType` (data-driven infections).
- **v48** — `WorldEdit` gains `WeSetCell` (3D set-cell edit for
  locations).
- *(unattributed fragment, preserved verbatim from the source: "per-instance
  edge keenness, split from `iiCondition` for weapon degradation." This
  continuation lost its own entry header at some point before this move —
  out of scope for issue #984, which corrects only the v61/v62 mix-up
  above.)*
- **v37** — `WorldGenParams` gains trailing `wgpTimelineParams`
  (player-configurable timeline depth: eon/era/period/epoch/age counts).
- **v36** — `ItemInstance` gains trailing `iiWeight` (per-instance rolled
  weights — raw gems vary per find).
- **v35** — `mdChunkProgress`.
- **v34** — `sdSpoilPiles` + `WeAddTile`.
- **v33** — `smPercent`.
