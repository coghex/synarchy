{-# LANGUAGE Strict, UnicodeSyntax, DeriveGeneric, DeriveAnyClass #-}
module World.Save.Types
    ( SaveData(..)
    , WorldPageSave(..)
    , activeWorldPage
    , SaveMetadata(..)
    , SaveHeader(..)
    , saveMagic
    , currentSaveVersion
    , BuildingSnapshot(..)
    , BuildingInstanceSnapshot(..)
    , toBuildingSnapshot
    , fromBuildingSnapshot
    , UnitSnapshot(..)
    , UnitInstanceSnapshot(..)
    , toUnitSnapshot
    , fromUnitSnapshot
    ) where

import UPrelude
import Data.Serialize (Serialize)
import GHC.Generics (Generic)
import qualified Data.HashMap.Strict as HM
import Structure.Palette (TexPalette)
import World.Generate.Types (WorldGenParams(..))
import World.Page.Types (WorldPageId(..))
import World.Render.Zoom.Types (ZoomMapMode(..))
import World.Tool.Types (ToolMode(..))
import World.Edit.Types (WorldEdits)
import World.Mine.Types (MineDesignations)
import World.Construct.Types (ConstructDesignations)
import Craft.Bills (CraftBills)
import Power.Types (PowerNodes)
import World.Chop.Types (ChopDesignations)
import World.Spoil.Types (SpoilPiles)
import World.Flora.Harvest (FloraHarvests)
import Item.Ground (GroundItems)
import Engine.Graphics.Camera (CameraFacing(..))
import Building.Types (BuildingId(..), BuildingInstance(..), BuildingDef(..)
                      , BuildingManager(..), buildingsOnPage)
import Unit.Types (UnitId(..), UnitInstance(..), UnitDef(..), UnitManager(..)
                  , StatModifier(..), Wound(..), Scar(..), unitsOnPage)
import Unit.Direction (Direction(..))
import Unit.Sim.Types (UnitSimState(..))
import Item.Types (ItemInstance(..))

-- | 4-byte magic prefix on every save file. Spells "SYRA"
--   (Synarchy) in little-endian. Detects "this isn't a save file"
--   before we even try to decode the version field.
saveMagic ∷ Word32
saveMagic = 0x53595241

-- | Current save schema version. Bumped on every schema change.
--   v2 introduced the version header + sdGameTime + sdEnginePaused.
--   v3 (Phase 2) added the per-chunk edit log so player tile changes
--   survive eviction + save/load.
--   v4 (Phase 3) added the building snapshot.
--   v5 (Phase 4) added units + sim state.
--   v6 (Phase 5) adds Lua module blobs (aiState, building_spawn state,
--       pause state).
--   v7 drops sdClimate + sdRiverFlow. Both fields round-tripped empty
--       HashMaps every save (no producer code ever wrote to the backing
--       IORefs); live climate state lives inside sdGenParams.
--   v8 (Phase 2 equipment) adds uisEquipped to UnitInstanceSnapshot:
--       a slot id → ItemInstance map persisting equipped gear.
--   v9 adds iiQuality + iiCondition to ItemInstance (per-item rolled
--       state). Positional Serialize means any field added to
--       ItemInstance bumps the format; can't be reverse-compatible
--       without a migration shim.
--   v10 adds uisAccessories to UnitInstanceSnapshot — items worn off
--       the silhouette (robes, goggles, rings…). Empty list is legal
--       for units without any accessory slots.
--   v11 adds bisBuildProgress to BuildingInstanceSnapshot — accumulated
--       worker-seconds toward bdBuildWork. Required so a save mid-
--       construction restores at the same progress fraction (otherwise
--       the building would either reset to 0 or skip to Built depending
--       on the new currentActivity branch).
--   v12 adds bisMaterialsDelivered to BuildingInstanceSnapshot — the
--       full ItemInstance list for each material type consumed into
--       the build. Required so an electric motor that went in at
--       100% condition comes back out at its then-current condition
--       on a future deconstruction recovery.
--   v13 adds bisStorage to BuildingInstanceSnapshot — cargo currently
--       deposited in the building (separate from delivered build
--       materials). Same ItemInstance preservation rule so deposited
--       items retain their quality/condition through save/load and
--       through deposit→withdraw round-trips.
--   v15 adds lcWaterTableMap to LoadedChunk for the water-table river
--       rework (Phase A). Chunks are transient so the field itself
--       doesn't round-trip via saves, but other shape/field deletions
--       on RiverSegment in Phase B will. Bumping now so any pre-rework
--       saves are clearly rejected before Phase B lands.
--   v22 removes the EruptionEvent constructor + LavaFlow record from
--       GeoEvent / Timeline.Types as part of the lava-v1 phase-3
--       cleanup. Lava placement is now driven by the pure-function
--       Magma system; per-period eruption rolls are gone. Constructor
--       tag shift makes the schema incompatible with v21.
--   v23 adds 'gtWorldLavaPools' (WorldLakes-shaped lava pool table)
--       to GeoTimeline — surface lava placed with pooling semantics
--       instead of the per-tile film. Positional Generic Serialize
--       gains a trailing field, incompatible with v22.
--   v24 adds 'wgpLavaPoolDepth' / 'wgpLavaPoolRadius' (volcanism
--       config levers) to WorldGenParams' manual Serialize.
--   v25 adds 'gtCoastal' (global coastal-erosion table) to
--       GeoTimeline — coastal erosion moved from the per-chunk
--       windowed pass to a world-init global pass on the stitched
--       terrain (cross-window coastline divergence / seam-cliff fix).
--   v26 adds 'gtSeabed' (global seabed table) to GeoTimeline —
--       ocean-floor relief (depth-from-shore ramp + noise replacing
--       the flat seaLevel−1 basin carve) + seabed materials + bedrock
--       outcrops.
--   v27 adds 'gtWorldOcean' (tile-resolution edge-connected ocean
--       bitmask) to GeoTimeline — composeFluidMap ORs it into the
--       chunk-level ocean test so sub-sea tiles the coarse chunk-flood
--       missed render ocean (sea-stops-at-chunk-boundary fix).
--   v62 removes the dead 'rpMeanderSeed' (RiverParams) and
--       'rscMeanderSeed' (RiverSegmentCarve) fields. Both rode along in
--       the serialized 'wgpGeoTimeline' (GeoTimeline → gtPeriods →
--       gpEvents → GeoEvent, e.g. HydroEvent (RiverFeature _)) but were
--       never read — the meander noise they seeded was discarded by the
--       river carve. Positional Generic Serialize drops the trailing
--       field, incompatible with v61 (#385).
currentSaveVersion ∷ Int
currentSaveVersion = 74  -- v74: CraftBill (nested in WorldPageSave's
                         --      wpsCraftBills) gains trailing 'cbSeq'
                         --      and 'cbPaused' (#330's manual-reorder
                         --      + pause bill controls).
                         -- v73: WorldPageSave gains trailing
                         --      'wpsPowerNodes' (#358) — the per-world
                         --      power-node registry (placed solar-panel/
                         --      battery source + storage nodes; role +
                         --      peak watts / capacity Wh; Power.Types).
                         -- v72: WorldPageSave gains trailing
                         --      'wpsCraftBills' (#329) — the per-world
                         --      craft-bill queue (station orders +
                         --      claim/progress state; Craft.Bills).
                         -- v71: UnitSimState gains usMoveGrade (uphill
                         --      slope speed/stamina, #375) — positional
                         --      Generic Serialize, so the appended field
                         --      shifts the record layout.
                         -- v70: WorldGenParams gains trailing
                         --      'wgpLocationStamped' (#424) — a dedicated
                         --      one-time geometry-stamp flag per chunk,
                         --      replacing the structure.hasAt(gx,gy,"floor")
                         --      inference the lazy location stamper used to
                         --      decide "already materialized". That check
                         --      was fooled by a player clearing the anchor
                         --      floor tile: the location was still stamped,
                         --      but the guard saw "no floor" and re-ran the
                         --      builder, clobbering edits. The new flag is
                         --      independent of structure edits, like the
                         --      existing wgpLocationContentsSpawned (#90).
                         -- v69: coastline variety (#220) — worldgen
                         --      OUTPUT change, no schema change. The
                         --      serialized CoastalTable (GeoTimeline →
                         --      gtCoastal) now reaches 28 tiles inland
                         --      and encodes steepness-driven coast
                         --      profiles; base terrain near convergent
                         --      land-ocean margins keeps its mountains
                         --      (continentalShelf modulation). Old
                         --      saves would replay stale coastlines
                         --      against new chunk regen.
                         -- v68: ItemInstance gains trailing 'iiTemp'
                         --      (Maybe Float, °C; Nothing = at ambient)
                         --      — item temperature + cooling toward the
                         --      tile's ambient (#344). Rides in every
                         --      serialized item (ground items, unit
                         --      inventory/equipment/accessories snapshot,
                         --      building storage/materials).
                         -- v67: WorldPageSave gains trailing
                         --      'wpsChopDesignations' (#97) — chop
                         --      designations (tile → surface z). ToolMode
                         --      also gains ChopTool (appended, tag 5).
                         -- v66: WorldPageSave gains trailing
                         --      'wpsFloraHarvests' (#94) — harvested
                         --      flora tiles with live regrowth timers.
                         -- v65: two-layer food model (#93) — the
                         --      persisted "hunger" uiStats entry is
                         --      redefined from energy store to STOMACH
                         --      meter (max_hunger halves to bm*10) and a
                         --      new "calories"/"max_calories" store takes
                         --      over catabolism/thermo/heal gating.
                         --      ItemContainer gains icDefaultFill and
                         --      ItemFood gains ifCaloriesPerKg (both
                         --      Generic Serialize, positional).
                         -- v64: WorldGenParams gains trailing
                         --      'wgpLocationContentsSpawned' (one-time
                         --      content-spawn flag per chunk, independent
                         --      of the structure-geometry idempotency
                         --      check; #90).
                         -- v63: WorldGenParams gains trailing 'wgpLocationOverlay'
                         --      (sparse chunk→location-id map placed at world
                         --      init; serialized so a loaded world keeps its
                         --      layout without recomputation; #89).
                         -- v62: drop dead rpMeanderSeed/rscMeanderSeed from the serialized GeoTimeline
                         --      (write-only scalar, never read; #386). Falls
                         --      route through usPendingFallDrop + Unit.Fall.
                         -- v60: WorldPageSave gains wpsConstructDesignations
                         --      (construction designation layer, #95).
                         -- v59: SaveData restructured — per-world state moved off
                         --      SaveData into a new WorldPageSave record;
                         --      SaveData now holds globals + sdActivePage +
                         --      sdVisiblePages + sdWorlds ([WorldPageSave]).
                         --      Foundation for one-save-all-pages (#215, epic #214).
                         -- v58: steep faces shed soil to bare rock in last-age erosion (#225)
                         -- v57: per-unit name (uisName) (#264)
                         -- v55: despike spike-only convergence pass (#254) — base
                         --      terrain output shifts on regen, so reject older saves
                         -- v54: structure edits (WeSetStructure/WeClearStructure) + sdTexPalette
                         -- v52: UnitSimState gains usJumpApex (leap arc state)
                         -- v50: UnitInstance gains uiImmuneResponse + uiImmunities (immunity system)
                         -- v49: Wound gains woundInfectionType (data-driven infections)
                         -- v48: WorldEdit gains WeSetCell (3D set-cell edit for locations)
                         -- (per-instance edge keenness, split from
                         -- iiCondition for weapon degradation).
                         -- (v37: WorldGenParams gains trailing
                         -- 'wgpTimelineParams' (player-configurable
                         -- timeline depth: eon/era/period/epoch/age counts).
                         -- (v36: ItemInstance gains trailing
                         -- 'iiWeight' (per-instance rolled weights —
                         -- raw gems vary per find).
                         -- (v35: mdChunkProgress; v34: sdSpoilPiles
                         -- + WeAddTile; v33: smPercent.)

-- | File prefix: magic + version. Decoded before the SaveData body.
--   Old (v1) saves have no header — magic check fails, loader rejects
--   with "Save file invalid or incompatible".
data SaveHeader = SaveHeader
    { shMagic   ∷ !Word32
    , shVersion ∷ !Int
    } deriving (Show, Eq, Serialize, Generic)

-- | Human-readable metadata for save listing
data SaveMetadata = SaveMetadata
    { smName       ∷ !Text
    , smSeed       ∷ !Word64
    , smWorldSize  ∷ !Int
    , smPlateCount ∷ !Int
    , smTimestamp  ∷ !Text        -- ^ ISO 8601 string
    } deriving (Show, Eq, Serialize, Generic)

-- | Per-world-page save payload. Everything scoped to a single world
--   page — terrain gen params, camera, clock, edits, and that page's
--   buildings/units/sim-states — lives here. A 'SaveData' carries a list
--   of these ('sdWorlds') plus the genuinely global fields.
--
--   Splitting per-world state into its own record is what lets every
--   live world page be persisted in one save (epic #214): the save
--   command snapshots every page in 'wmWorlds' into one 'WorldPageSave'
--   each. Like 'SaveData', this is encoded positionally by cereal's
--   Generic instance, so any layout change bumps 'currentSaveVersion'.
data WorldPageSave = WorldPageSave
    { wpsPageId       ∷ !WorldPageId
        -- ^ The id this page had at save time. On load the active page
        --   restores under @main_world@ (see 'sdActivePage'); additional
        --   pages restore under this id.
    , wpsGenParams    ∷ !WorldGenParams
    , wpsCameraX      ∷ !Float
    , wpsCameraY      ∷ !Float
    , wpsCameraZoom   ∷ !Float
    , wpsCameraFacing ∷ !CameraFacing
    , wpsTimeHour     ∷ !Int
    , wpsTimeMinute   ∷ !Int
    , wpsDateYear     ∷ !Int
    , wpsDateMonth    ∷ !Int
    , wpsDateDay      ∷ !Int
    , wpsTimeScale    ∷ !Float
    , wpsMapMode      ∷ !ZoomMapMode
    , wpsToolMode     ∷ !ToolMode
    , wpsEdits        ∷ !WorldEdits
        -- ^ Per-chunk edit log. Restored before chunk regeneration on
        --   load; chunks then replay their edits to recover the player's
        --   modifications. Edits survive eviction during a play session.
    , wpsMineDesignations ∷ !MineDesignations
        -- ^ Mine designations incl. mid-dig corner progress. Restored
        --   straight into wsMineDesignationsRef; markers re-render from
        --   the stored z, so no chunk loading is required first.
    , wpsConstructDesignations ∷ !ConstructDesignations
        -- ^ Construction designations (#95): build target + status +
        --   progress per tile. Like mine designations, ghosts re-render
        --   from the stored z, so restoration needs no chunk loading.
    , wpsGroundItems  ∷ !GroundItems
        -- ^ Items lying in the world. Full ItemInstances + float
        --   positions; resting height derives from terrain at render,
        --   so restoration needs no chunk loading either.
    , wpsSpoilPiles   ∷ !SpoilPiles
        -- ^ Spoil mounds (vertex-keyed partial fills — see
        --   World.Spoil.Types). Fills are relative to each tile's terrain
        --   surface; promoted cells live in wpsEdits as WeAddTile, so
        --   restoration is order-independent.
    , wpsBuildings    ∷ !BuildingSnapshot
        -- ^ This page's placed buildings. Restored AFTER edits +
        --   center-chunk regen so buildings landing on player-edited
        --   terrain end up at the right z (their saved biGridZ already
        --   reflects the post-edit terrain at place time, but the chunk
        --   needs to have replayed its edits first for downstream queries
        --   to agree).
    , wpsUnits        ∷ !UnitSnapshot
        -- ^ This page's live unit instances + their stats/skills/
        --   modifiers/inventory. Restored alongside the sim states below.
    , wpsUnitSimStates ∷ !(HM.HashMap UnitId UnitSimState)
        -- ^ Per-unit sim state (position, pose, activity, target, path,
        --   *Until timers) for this page's units. Restored into utsRef on
        --   EngineEnv.
    , wpsFloraHarvests ∷ !FloraHarvests
        -- ^ Harvested flora tiles (#94): tile → regrowth game-seconds
        --   remaining. Like designations, restored straight into
        --   wsFloraHarvestsRef — render and queries need no chunk
        --   loading first. Appended for save v66 (positional Generic
        --   Serialize — field order is load-bearing).
    , wpsChopDesignations ∷ !ChopDesignations
        -- ^ Chop designations (#97): tile → surface z. Like the other
        --   designation layers, restored straight into
        --   wsChopDesignationsRef; markers re-render from the stored z.
        --   Appended for save v67.
    , wpsCraftBills ∷ !CraftBills
        -- ^ Craft-bill queue (#329): standing per-station craft orders
        --   incl. claim + cycle progress. Station references are
        --   BuildingIds, which restore verbatim (see wpsBuildings), so
        --   bills reconnect to their stations; restore prunes bills
        --   whose station was orphaned. Appended for save v72.
    , wpsPowerNodes ∷ !PowerNodes
        -- ^ Power-node registry (#358): placed solar-panel/battery
        --   source + storage nodes (role + parameters). Like craft
        --   bills, references a BuildingId that restores verbatim (see
        --   wpsBuildings); restore prunes nodes whose building was
        --   orphaned. Appended for save v73.
    } deriving (Show, Serialize, Generic)

-- | Everything needed to reconstruct the saved game. Per-world state is
--   carried in 'sdWorlds' (one 'WorldPageSave' per saved page — every
--   live page, not just the active one); the remaining fields are
--   genuinely global, shared by every page or describing the save as
--   a whole.
--
--   Schema is versioned via the file header — see saveMagic /
--   currentSaveVersion / SaveHeader above. Bump currentSaveVersion
--   whenever this record's layout (or 'WorldPageSave''s) changes
--   (cereal's Generic encoding is positional, so reordering or inserting
--   a field is breaking).
data SaveData = SaveData
    { sdMetadata   ∷ !SaveMetadata
        -- ^ Save-listing metadata (name/seed/size/plates/timestamp),
        --   describing the primary (active) world.
    -- Global fields (one per save, shared across all pages):
    , sdGameTime     ∷ !Double   -- ^ gameTimeRef value (game-clock seconds).
    , sdEnginePaused ∷ !Bool     -- ^ enginePausedRef value. Auto-pause-on-save
                                  --   means this is always True for v2+ saves.
    , sdLuaModules   ∷ !(HM.HashMap Text Text)
        -- ^ Per-Lua-module opaque blobs. Each registered module
        --   serializes its state to a string via the Lua
        --   `saveModules.serializeAll()` registry; engine treats them
        --   as opaque text. On load, Lua restores via
        --   `saveModules.deserializeAll(blobs)` BEFORE the engine-side
        --   restore happens, so AI memory + spawn-sequencer state
        --   line up with the units/buildings the engine then writes.
    , sdTexPalette ∷ !TexPalette
        -- ^ Texture path↔id palette. Structure edits in each page's edits
        --   store palette ids; this resolves them to paths → runtime
        --   handles on load. Stable ids → no per-object remap.
    , sdNextItemInstanceId ∷ !Word64
        -- ^ Snapshot of 'nextItemInstanceIdRef' at save time. Restored
        --   (max'd, never lowered) on load so post-load item creation
        --   continues above every saved 'iiInstanceId' and can't reuse an
        --   id still held by a loaded item.
    -- Multi-page fields (#215 / epic #214):
    , sdActivePage   ∷ !WorldPageId
        -- ^ The primary/active page at save time. Restores under id
        --   @main_world@ (the documented convention that existing Lua /
        --   headless code assumes) regardless of its original id.
    , sdVisiblePages ∷ ![WorldPageId]
        -- ^ Pages that were visible (wmVisible) at save time, so the
        --   loaded game comes up showing what the player last saw.
    , sdWorlds       ∷ ![WorldPageSave]
        -- ^ Every saved world page, one entry per live page in
        --   wmWorlds at save time.
    } deriving (Show, Serialize, Generic)

-- | The primary/active world page in a save — the one that restores as
--   @main_world@. Falls back to the first page if 'sdActivePage' names no
--   page in 'sdWorlds' (defensive; every real save records a valid active
--   id). 'Nothing' only for a malformed empty-world save.
activeWorldPage ∷ SaveData → Maybe WorldPageSave
activeWorldPage sd =
    case filter ((≡ sdActivePage sd) . wpsPageId) (sdWorlds sd) of
        (w:_) → Just w
        []    → case sdWorlds sd of
                  (w:_) → Just w
                  []    → Nothing

-- | Persistable snapshot of `BuildingManager`. Drops `bmDefs`
--   (regenerated from YAML at boot) and `bmSelected` (transient UI
--   state, reset on load).
data BuildingSnapshot = BuildingSnapshot
    { bsnInstances ∷ !(HM.HashMap BuildingId BuildingInstanceSnapshot)
    , bsnNextId    ∷ !Word32
    } deriving (Show, Serialize, Generic)

-- | Persistable snapshot of `BuildingInstance`. Drops `biTexture`
--   (runtime asset handle, meaningless across sessions — re-resolved
--   from the def at load time via `biDefName`).
data BuildingInstanceSnapshot = BuildingInstanceSnapshot
    { bisDefName    ∷ !Text
    , bisAnchorX    ∷ !Int
    , bisAnchorY    ∷ !Int
    , bisGridZ      ∷ !Int
    , bisSpawnedAt  ∷ !Double
    , bisTileW      ∷ !Int
    , bisTileH      ∷ !Int
    , bisSpawnRemaining ∷ !Int
    , bisBuildProgress ∷ !Float
    , bisMaterialsDelivered ∷ !(HM.HashMap Text [ItemInstance])
    , bisStorage           ∷ ![ItemInstance]
    } deriving (Show, Serialize, Generic)

-- | Build a snapshot from a live BuildingManager, restricted to the world
--   being saved (@page@). The manager is global across worlds, so an
--   unfiltered snapshot would serialize other worlds' buildings and stamp
--   them onto the load target (#76). Strips `bmDefs` (regenerated from
--   YAML on next boot) and `bmSelected` (resets to Nothing).
toBuildingSnapshot ∷ WorldPageId → BuildingManager → BuildingSnapshot
toBuildingSnapshot page bm = BuildingSnapshot
    { bsnInstances = HM.map toBuildingInstanceSnapshot
                            (buildingsOnPage page (bmInstances bm))
    , bsnNextId    = bmNextId bm
    }

toBuildingInstanceSnapshot ∷ BuildingInstance → BuildingInstanceSnapshot
toBuildingInstanceSnapshot bi = BuildingInstanceSnapshot
    { bisDefName       = biDefName bi
    , bisAnchorX       = biAnchorX bi
    , bisAnchorY       = biAnchorY bi
    , bisGridZ         = biGridZ bi
    , bisSpawnedAt     = biSpawnedAt bi
    , bisTileW         = biTileW bi
    , bisTileH         = biTileH bi
    , bisSpawnRemaining = biSpawnRemaining bi
    , bisBuildProgress = biBuildProgress bi
    , bisMaterialsDelivered = biMaterialsDelivered bi
    , bisStorage           = biStorage bi
    }

-- | Restore a BuildingManager from a snapshot. `defs` come from the
--   already-loaded BuildingManager (registered from YAML at boot);
--   we use them to re-resolve each instance's `biTexture` from its
--   `bisDefName`. Instances whose def is no longer registered (e.g.
--   the player removed the YAML between sessions) are dropped with a
--   warning written to the IO log by the caller.
--
--   Returns (manager, [orphan BuildingId]) so the caller can log
--   the dropped entries.
--   @page@ is the world the buildings load into (the load target); every
--   restored building is stamped with it so the runtime world scoping
--   holds after a load (#76).
fromBuildingSnapshot ∷ WorldPageId → HM.HashMap Text BuildingDef
                     → BuildingSnapshot
                     → (BuildingManager, [BuildingId])
fromBuildingSnapshot page defs snap =
    let pairs = HM.toList (bsnInstances snap)
        resolved = [ (bid, fromBuildingInstanceSnapshot page d snap')
                   | (bid, snap') ← pairs
                   , Just d ← [HM.lookup (bisDefName snap') defs]
                   ]
        orphans  = [ bid
                   | (bid, snap') ← pairs
                   , not (HM.member (bisDefName snap') defs)
                   ]
        bm = BuildingManager
                { bmDefs      = defs
                , bmInstances = HM.fromList resolved
                , bmNextId    = bsnNextId snap
                , bmSelected  = Nothing
                }
    in (bm, orphans)

fromBuildingInstanceSnapshot ∷ WorldPageId → BuildingDef
                             → BuildingInstanceSnapshot → BuildingInstance
fromBuildingInstanceSnapshot page def s = BuildingInstance
    { biDefName        = bisDefName s
    , biPage           = page             -- runtime world scoping (#76)
    , biTexture        = bdTexture def    -- re-resolved
    , biAnchorX        = bisAnchorX s
    , biAnchorY        = bisAnchorY s
    , biGridZ          = bisGridZ s
    , biSpawnedAt      = bisSpawnedAt s
    , biTileW          = bisTileW s
    , biTileH          = bisTileH s
    , biSpawnRemaining = bisSpawnRemaining s
    , biBuildProgress  = bisBuildProgress s
    , biMaterialsDelivered = bisMaterialsDelivered s
    , biStorage            = bisStorage s
    }

-- Unit snapshots ---------------------------------------------------

-- | Persistable snapshot of `UnitManager`. Drops `umDefs` (regenerated
--   from YAML at boot) and `umSelected` (transient UI state).
data UnitSnapshot = UnitSnapshot
    { usnInstances ∷ !(HM.HashMap UnitId UnitInstanceSnapshot)
    , usnNextId    ∷ !Word32
    } deriving (Show, Serialize, Generic)

-- | Persistable snapshot of `UnitInstance`. Drops `uiTexture` +
--   `uiDirSprites` (runtime asset handles, re-resolved from the def at
--   load via `uisDefName`). Everything else round-trips faithfully —
--   stats, modifiers, skills, inventory, the current animation frame
--   the unit was on, and pose/activity strings.
data UnitInstanceSnapshot = UnitInstanceSnapshot
    { uisDefName     ∷ !Text
    , uisBaseWidth   ∷ !Float
    , uisGridX       ∷ !Float
    , uisGridY       ∷ !Float
    , uisGridZ       ∷ !Int
    , uisFacing      ∷ !Direction
    , uisCurrentAnim ∷ !Text
    , uisAnimStart   ∷ !Double
    , uisAnimReverse ∷ !Bool
    , uisActivity    ∷ !Text
    , uisPose        ∷ !Text
    , uisAnimStride  ∷ !Int
    , uisStats       ∷ !(HM.HashMap Text Float)
    , uisModifiers   ∷ !(HM.HashMap Text [StatModifier])
    , uisSkills      ∷ !(HM.HashMap Text Float)
    , uisKnowledge   ∷ !(HM.HashMap Text Float)
    , uisInventory   ∷ ![ItemInstance]
    , uisEquipped    ∷ !(HM.HashMap Text ItemInstance)
      -- ^ v8: slot id → equipped item. Empty map is legal (no gear).
      --   Serialize roundtrip is positional, not name-keyed, so any
      --   future addition must go after this field; bump again if so.
    , uisAccessories ∷ ![ItemInstance]
      -- ^ v10: items worn off the silhouette (robes, goggles, rings…).
      --   Order preserved.
    , uisFactionId   ∷ !Text
      -- ^ v16: spawn-time-only faction tag (no def-level default).
      --   Used by the combat layer for hostile/friendly checks.
      --   "player" / "wildlife" / future custom tags.
    , uisWounds      ∷ ![Wound]
      -- ^ v16: per-unit wound list. Roundtrips faithfully. Generic
      --   Serialize over the Wound record below; fields are
      --   positional, so appending a field to Wound also bumps v.
    , uisScars       ∷ ![Scar]
      -- ^ v45: permanent scar records left by healed severe wounds.
    , uisImmuneResponse ∷ !Float
      -- ^ v50: systemic immune-response level (the active infection fight).
    , uisImmunities   ∷ !(HM.HashMap Text Float)
      -- ^ v50: acquired per-type immunity (decays very slowly).
    , uisBlood       ∷ !Float
      -- ^ v16: current blood volume in litres. Spawn-time seeded
      --   from body_mass; ticked down by Combat.Wounds bleeding.
    , uisName        ∷ !Text
      -- ^ v57: persistent per-unit display name (#264). "" for unnamed
      --   units. Appended last — Serialize roundtrip is positional.
    } deriving (Show, Serialize, Generic)

-- | Build a snapshot from a live UnitManager, restricted to the world
--   being saved (@page@). The manager is global across worlds, so an
--   unfiltered snapshot would serialize other worlds' units and stamp
--   them onto the load target (#78).
toUnitSnapshot ∷ WorldPageId → UnitManager → UnitSnapshot
toUnitSnapshot page um = UnitSnapshot
    { usnInstances = HM.map toUnitInstanceSnapshot
                            (unitsOnPage page (umInstances um))
    , usnNextId    = umNextId um
    }

toUnitInstanceSnapshot ∷ UnitInstance → UnitInstanceSnapshot
toUnitInstanceSnapshot ui = UnitInstanceSnapshot
    { uisDefName     = uiDefName ui
    , uisBaseWidth   = uiBaseWidth ui
    , uisGridX       = uiGridX ui
    , uisGridY       = uiGridY ui
    , uisGridZ       = uiGridZ ui
    , uisFacing      = uiFacing ui
    , uisCurrentAnim = uiCurrentAnim ui
    , uisAnimStart   = uiAnimStart ui
    , uisAnimReverse = uiAnimReverse ui
    , uisActivity    = uiActivity ui
    , uisPose        = uiPose ui
    , uisAnimStride  = uiAnimStride ui
    , uisStats       = uiStats ui
    , uisModifiers   = uiModifiers ui
    , uisSkills      = uiSkills ui
    , uisKnowledge   = uiKnowledge ui
    , uisInventory   = uiInventory ui
    , uisEquipped    = uiEquipment ui
    , uisAccessories = uiAccessories ui
    , uisFactionId   = uiFactionId ui
    , uisWounds      = uiWounds ui
    , uisScars       = uiScars ui
    , uisImmuneResponse = uiImmuneResponse ui
    , uisImmunities  = uiImmunities ui
    , uisBlood       = uiBlood ui
    , uisName        = uiName ui
    }

-- | Restore a UnitManager from a snapshot. Like buildings: instances
--   whose def is no longer registered get dropped with the orphan
--   list returned for caller logging. `umSelected` resets to empty.
--   @page@ is the world the units are loaded into (always the load
--   target, "main_world"); every restored unit is stamped with it so the
--   runtime-only world scoping holds after a load (#78).
fromUnitSnapshot ∷ WorldPageId → HM.HashMap Text UnitDef → UnitSnapshot
                 → (UnitManager, [UnitId])
fromUnitSnapshot page defs snap =
    let pairs = HM.toList (usnInstances snap)
        resolved = [ (uid, fromUnitInstanceSnapshot page d s)
                   | (uid, s) ← pairs
                   , Just d ← [HM.lookup (uisDefName s) defs]
                   ]
        orphans  = [ uid
                   | (uid, s) ← pairs
                   , not (HM.member (uisDefName s) defs)
                   ]
        um = UnitManager
                { umDefs      = defs
                , umInstances = HM.fromList resolved
                , umSelected  = mempty
                , umNextId    = usnNextId snap
                }
    in (um, orphans)

fromUnitInstanceSnapshot ∷ WorldPageId → UnitDef → UnitInstanceSnapshot
                         → UnitInstance
fromUnitInstanceSnapshot page def s = UnitInstance
    { uiDefName     = uisDefName s
    , uiName        = uisName s
    , uiPage        = page                -- runtime world scoping (#78)
    , uiTexture     = udTexture def       -- re-resolved
    , uiDirSprites  = udDirSprites def    -- re-resolved
    , uiBaseWidth   = uisBaseWidth s
    , uiGridX       = uisGridX s
    , uiGridY       = uisGridY s
    , uiGridZ       = uisGridZ s
    -- uiRealZ is render-only; restore it from the integer Z so loaded
    -- units stand at the right visual height. Active climbs at save
    -- time would lose their interpolation progress, but combat/climb
    -- state is transient by design.
    , uiRealZ       = fromIntegral (uisGridZ s)
    , uiFacing      = uisFacing s
    , uiCurrentAnim = uisCurrentAnim s
    , uiAnimStart   = uisAnimStart s
    , uiAnimReverse = uisAnimReverse s
    , uiActivity    = uisActivity s
    , uiPose        = uisPose s
    , uiAnimStride  = uisAnimStride s
    , uiStats       = uisStats s
    , uiModifiers   = uisModifiers s
    , uiSkills      = uisSkills s
    , uiKnowledge   = uisKnowledge s
    , uiInventory   = uisInventory s
    , uiEquipment   = uisEquipped s
    , uiAccessories = uisAccessories s
    , uiFactionId   = uisFactionId s
    , uiWounds      = uisWounds s
    , uiScars       = uisScars s
    , uiImmuneResponse = uisImmuneResponse s
    , uiImmunities  = uisImmunities s
    , uiBlood       = uisBlood s
    -- Runtime-only combat memory — reset on load. A bear that was
    -- in the middle of a fight gets a clean slate on reload; the
    -- next incoming hit will re-trigger retaliation.
    , uiLastAttackerUid = Nothing
    , uiLastAttackerAt  = 0
    -- Runtime-only animation override — Lua re-sets if needed.
    , uiAnimOverride = ""
    -- Runtime-only debug flags — always False on load.
    , uiFrozen      = False
    , uiForceLoop   = False
    , uiClimbDest   = Nothing   -- runtime-only; not persisted
    }
