{-# LANGUAGE Strict, UnicodeSyntax, DeriveGeneric, DeriveAnyClass #-}
module World.Save.Types
    ( SaveData(..)
    , WorldPageSave(..)
    , activeWorldPage
    , SaveMetadata(..)
    , SaveHeader(..)
    , saveMagic
    , currentSaveVersion
    , checkWorldCount
    , BuildingSnapshot(..)
    , BuildingInstanceSnapshot(..)
    , toBuildingSnapshot
    , fromBuildingSnapshot
    , UnitSnapshot(..)
    , UnitInstanceSnapshot(..)
    , toUnitSnapshot
    , fromUnitSnapshot
    , MissingDefRef(..)
    , renderMissingDefRef
    , missingDefReferences
    , MissingItemDefRef(..)
    , renderMissingItemDefRef
    , missingItemDefReferences
    , MissingRecipeRef(..)
    , renderMissingRecipeRef
    , missingRecipeReferences
    , MissingBillOutputItemRef(..)
    , renderMissingBillOutputItemRef
    , missingBillOutputItemReferences
    , MissingConstructDefRef(..)
    , renderMissingConstructDefRef
    , missingConstructDefReferences
    , MissingMaterialRef(..)
    , renderMissingMaterialRef
    , missingMaterialReferences
    , MissingFloraRef(..)
    , renderMissingFloraRef
    , missingFloraReferences
    , MissingLocationRef(..)
    , renderMissingLocationRef
    , missingLocationOverlayReferences
    , MissingInfectionRef(..)
    , renderMissingInfectionRef
    , missingInfectionReferences
    ) where

import UPrelude
import Data.Serialize (Serialize)
import GHC.Generics (Generic)
import qualified Data.HashMap.Strict as HM
import qualified Data.HashSet as HS
import qualified Data.Text as T
import Structure.Palette (TexPalette)
import World.Generate.Types (WorldGenParams(..))
import World.Page.Types (WorldPageId(..), WorldIdentity(..))
import World.Render.Zoom.Types (ZoomMapMode(..))
import World.Tool.Types (ToolMode(..))
import World.Edit.Types (WorldEdits, WorldEdit(..))
import World.Mine.Types (MineDesignations)
import World.Construct.Types
    (ConstructDesignations, ConstructDesignation(..), ConstructTarget(..))
import Craft.Bills (CraftBills(..), CraftBill(..), BillId(..))
import Power.Types (PowerNodes)
import World.Chop.Types (ChopDesignations)
import World.Till.Types (TillDesignations)
import World.Plant.Types (PlantDesignations)
import World.Spoil.Types (SpoilPiles, SpoilPile(..))
import World.Material (MaterialId(..), MaterialRegistry, isKnownMaterial)
import World.Plate.Types (TectonicPlate(..))
import World.Flora.Harvest (FloraHarvests)
import World.Flora.CropPlot (CropPlots, CropPlot(..))
import World.Flora.Types (FloraId(..), FloraCatalog, lookupSpecies)
import World.Chunk.Types (ChunkCoord(..))
import Infection.Types (InfectionManager, lookupInfection)
import Item.Ground (GroundItems(..), GroundItem(..))
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
currentSaveVersion = 91  -- v91 (#761, save-overhaul B3): 'sdLuaModules'
                         -- removed — Lua-owned state no longer rides
                         -- through 'SaveData' at all; each registered Lua
                         -- module is its own dynamically-added envelope
                         -- component ("World.Save.Component.Types"'s
                         -- 'luaComponentPrefix'), gathered/applied by
                         -- "Engine.Scripting.Lua.API.Save" directly, never
                         -- part of this transitional load-bridge shape.
                         --
                         -- v90 (#759, save-overhaul B1): no layout change
                         -- to SaveData/WorldPageSave themselves — this
                         -- bump marked the transition to the tagged,
                         -- checksummed envelope format
                         -- ("World.Save.Envelope"). 'SaveData' now rides
                         -- as the "session" component's payload inside
                         -- that envelope rather than being the whole
                         -- file; 'currentSaveVersion' is that
                         -- component's own schema version (unchanged
                         -- meaning — still bump it whenever this
                         -- record's or 'WorldPageSave''s layout
                         -- changes), kept deliberately separate from
                         -- the envelope's own framing version
                         -- ('World.Save.Envelope.currentEnvelopeVersion').
                         -- v89: WorldGenParams gains trailing
                         -- 'wgpLocationDiscovered' (#780) — a per-chunk
                         -- one-time discovery flag (player-faction unit
                         -- has entered a placed location's discovery-
                         -- margin halo), independent of
                         -- wgpLocationStamped/wgpLocationContentsSpawned.
                         -- v88: CraftBill gains cbMode/cbTarget/cbOutputItem
                         -- (#795) — the persisted until-stock craft-bill
                         -- mode (Craft.Bills.addUntilStockBill), appended
                         -- after cbWorking.
                         -- v87: ConstructDesignation gains cdMaterialsPaid
                         -- (#799) — a durable payment marker so a structure
                         -- designation's material cost, once taken from a
                         -- claimant's inventory, is never charged a second
                         -- time to a replacement worker after the original
                         -- claimant dies or the world is reloaded.
                         -- v86: worldgen-output change (#812) — final-age
                         -- mountain soil shed is now redistributed to the
                         -- adjacent lower/gentler receiving terrain instead
                         -- of simply deleted (World.Geology.Erosion.Math
                         -- shedCredit), closing out #225's redistribution
                         -- requirement that PR #279 left undone.
                         -- v85: WorldEdit gains trailing simulation-fluid
                         -- snapshot constructors, so a coordinated save can
                         -- replay pre-boundary World → Sim → World fluid
                         -- writebacks while the loaded game remains paused.
                         -- v84: worldgen-output change (#811) — Tiny(32)/
                         --      Small(64) worlds now get the inland-origin
                         --      river-source extension (issue #221) instead
                         --      of the old worldSize>=128 gate, guarded by
                         --      a new caldera/supervolcano hazard check so
                         --      the extension can't breach a caldera and
                         --      flood lava through the newly-carved valley.
                         -- v83: worldgen-output change, not a layout
                         --      change (#785). Final regional climate
                         --      is now rebuilt from the completed
                         --      timeline's own evolved CO2/solar
                         --      forcing instead of hardcoded baseline
                         --      forcing (1.0/0.0/1.0) with only the
                         --      csGlobalCO2/csGlobalTemp/csSolarConst
                         --      summary fields patched from the
                         --      timeline afterward — so a save's
                         --      stored regional climate grid differs
                         --      from pre-#785 saves for the same seed.
                         -- v82: player-facing world identity (#707).
                         --      WorldPageSave gains a trailing
                         --      'wpsIdentity' (Maybe WorldIdentity:
                         --      display name + optional gloss) and
                         --      SaveMetadata gains trailing
                         --      'smWorldName' / 'smWorldGloss' (the
                         --      ACTIVE page's identity, for save
                         --      listing). Both records are positional
                         --      Generic Serialize, so the appended
                         --      fields shift the layout.
                         -- v81: Pose (Unit.Sim.Types) gains a new
                         --      trailing 'Sleeping' constructor (#612)
                         --      for the circadian sleep-goal AI.
                         --      Appended at the end, per Pose's own
                         --      append-only policy.
                         -- v80: CraftBill (Craft.Bills) gains a new
                         --      trailing 'cbWorking' field (#590) — is
                         --      the claimant CURRENTLY pouring work
                         --      into this cycle (vs. still fetching/
                         --      walking), which Power.Network.
                         --      activeCraftConsumersOn now keys its
                         --      live demand off instead of "claimed and
                         --      not paused". Appended at the end, per
                         --      this record's own append-only policy.
                         -- v79: WorldEdit gains a new trailing
                         --      'WePlaceFlora' constructor (#336) — the
                         --      farm AI's row-crop planting completion
                         --      (world.plantRowCropAt). Order-preserving
                         --      (appended at the end), but per this
                         --      module's own policy any new WorldEdit
                         --      variant bumps the version.
                         -- v78: WorldPageSave gains trailing
                         --      'wpsPlantDesignations' (#335) — plant
                         --      designations (tile → surface z + chosen
                         --      crop species). Like the other
                         --      designation layers, restored straight
                         --      into wsPlantDesignationsRef; markers
                         --      re-render from the stored z. ToolMode
                         --      also gains PlantTool (appended, tag 7).
                         -- v77: WorldPageSave gains trailing
                         --      'wpsCropPlots' (#334) — planted
                         --      groundcover-crop tiles (species +
                         --      planted day + health). Like the
                         --      designation layers, restored straight
                         --      into wsCropPlotsRef; the render pass
                         --      derives the current growth texture
                         --      from it with no chunk loading needed.
                         -- v76: WorldPageSave gains trailing
                         --      'wpsTillDesignations' (#333) — till
                         --      designations (tile → surface z), same
                         --      shape as wpsChopDesignations. ToolMode
                         --      also gains TillTool (appended, tag 6).
                         -- v75: PowerNode (nested in WorldPageSave's
                         --      wpsPowerNodes) gains trailing
                         --      'pnStoredWh' (#360) — a storage node's
                         --      current charge, the one piece of the
                         --      network energy balance that must
                         --      survive save/load (connectivity +
                         --      generation/drain are recomputed fresh
                         --      from wire + node positions).
                         -- v74: CraftBill (nested in WorldPageSave's
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

-- | The shape of the tagged save envelope's fixed 16-byte header
--   (issue #759, save-overhaul B1): magic, the envelope FRAMING
--   version (separate from any carried component's own schema
--   version — see 'World.Save.Envelope.currentEnvelopeVersion'), and
--   the manifest's length in bytes. Retained here — rather than in
--   "World.Save.Envelope.Types" — purely so the persistence-inventory
--   audit (`tools/persistence_inventory_audit.py`) keeps a stable
--   root-owner record to classify; the real codec
--   ("World.Save.Envelope.Codec") manipulates these three values as
--   raw scalars under its own explicit byte-layout control, not this
--   record, and never constructs one. A pre-#759 flat file (v89 and
--   earlier) has no manifest at all — its first 16 bytes decode as a
--   header whose 'shEnvelopeVersion' can never coincide with a real
--   envelope version, so it is rejected the same way any other
--   version mismatch is, with no heuristic positional decoding.
data SaveHeader = SaveHeader
    { shMagic           ∷ !Word32
    , shEnvelopeVersion ∷ !Word32
    , shManifestLength  ∷ !Word64
    } deriving (Show, Eq, Generic)

-- | Human-readable metadata for save listing
data SaveMetadata = SaveMetadata
    { smName       ∷ !Text
        -- ^ The save-slot/file identity (validated by sanitizeSaveName)
        --   — NOT the world's player-facing name, see smWorldName.
    , smSeed       ∷ !Word64
    , smWorldSize  ∷ !Int
    , smPlateCount ∷ !Int
    , smTimestamp  ∷ !Text        -- ^ ISO 8601 string
    , smWorldName  ∷ !(Maybe Text)
        -- ^ v82 (#707): the ACTIVE page's player-facing display name at
        --   save time (its 'wpsIdentity'), so save listings can show it
        --   without decoding sdWorlds. Nothing for an unnamed world.
    , smWorldGloss ∷ !(Maybe Text)
        -- ^ v82 (#707): that identity's optional English gloss. Always
        --   Nothing when smWorldName is Nothing (a gloss cannot exist
        --   without a display name).
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
        --   source + storage nodes (role + parameters), plus each
        --   storage node's current charge (pnStoredWh, #360). Like
        --   craft bills, references a BuildingId that restores
        --   verbatim (see wpsBuildings); restore prunes nodes whose
        --   building was orphaned. Appended for save v73, pnStoredWh
        --   for v75.
    , wpsTillDesignations ∷ !TillDesignations
        -- ^ Till designations (#333): tile → surface z. Like the other
        --   designation layers, restored straight into
        --   wsTillDesignationsRef; markers re-render from the stored z.
        --   Appended for save v76.
    , wpsCropPlots ∷ !CropPlots
        -- ^ Planted groundcover-crop tiles (#334): tile → (species,
        --   planted day, health). Restored straight into
        --   wsCropPlotsRef; render/harvest derive growth state from it
        --   with no chunk loading needed. Appended for save v77.
    , wpsPlantDesignations ∷ !PlantDesignations
        -- ^ Plant designations (#335): tile → (surface z, chosen crop).
        --   Like the other designation layers, restored straight into
        --   wsPlantDesignationsRef; markers re-render from the stored
        --   z. Appended for save v78.
    , wpsIdentity ∷ !(Maybe WorldIdentity)
        -- ^ Player-facing identity (#707): display name + optional
        --   gloss. Lives HERE — on the page's saved state — rather than
        --   deriving from any id, because load remaps ids (the active
        --   page → main_world, collisions → "<id>#N") while the
        --   identity must follow the page itself. Restored straight
        --   into wsIdentityRef. Appended for save v82.
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
        -- Lua-owned state (issue #761, save-overhaul B3): no longer a
        -- field here at all. Each registered Lua module is its own
        -- dynamically-added envelope component (@"lua.<module>"@),
        -- prepared and applied directly by
        -- "Engine.Scripting.Lua.API.Save" — decode/validate BEFORE the
        -- engine-side restore is queued, apply still ahead of it, same
        -- ordering as before, just no longer routed through this record.
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

-- | Reject a decoded save with NO world pages (corrupt / truncated). Any
--   non-empty 'sdWorlds' is accepted: the save command snapshots every live
--   page (#216) and the load handler restores all of them (#217/#218), so a
--   multi-page save is fully supported. Only the empty case is rejected, so
--   the rest of the loader can assume at least one page.
--
--   Applied by 'World.Save.Serialize.loadWorld' AND, for issue #762's
--   storage-transaction candidate re-read (requirement 3), by
--   'World.Save.Storage' before either a fresh publish or a
--   previous-generation load fallback trusts a decoded generation. NOT
--   applied by 'World.Save.Serialize.listSaves': listing decodes just the
--   \"metadata\" component (issue #759 requirement 4), which never carries
--   'sdWorlds' at all.
checkWorldCount ∷ SaveData → Either Text SaveData
checkWorldCount sd = case sdWorlds sd of
    [] → Left "Save contains no world pages (corrupt or truncated file)"
    _  → Right sd

-- | Persistable snapshot of `BuildingManager`. Drops `bmDefs`
--   (regenerated from YAML at boot) and `bmSelected` (transient UI
--   state, reset on load).
data BuildingSnapshot = BuildingSnapshot
    { bsnInstances ∷ !(HM.HashMap BuildingId BuildingInstanceSnapshot)
    , bsnNextId    ∷ !Word32
    } deriving (Show, Eq, Serialize, Generic)

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
    } deriving (Show, Eq, Serialize, Generic)

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
    } deriving (Show, Eq, Serialize, Generic)

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
    } deriving (Show, Eq, Serialize, Generic)

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

-- Missing-definition validation (#760 requirement 9) -----------------

-- | A saved building/unit instance whose content-definition reference
--   ('bisDefName'/'uisDefName') does NOT resolve against the currently-
--   registered definitions. Per #760 requirement 9 a missing gameplay
--   definition is a LOAD-VALIDATION FAILURE (the complete load is
--   rejected), not the silent per-entity pruning
--   'fromBuildingSnapshot'/'fromUnitSnapshot' fall back to — those still
--   return orphans as defense-in-depth, but the load boundary rejects a
--   save carrying any such reference before publishing any live state, so
--   that pruning path is unreachable in normal play. (Missing VISUAL
--   assets remain a soft #756 fallback — this is only about definitions.)
data MissingDefRef = MissingDefRef
    { mdrKind    ∷ !Text          -- ^ @"building"@ or @"unit"@
    , mdrPage    ∷ !WorldPageId
    , mdrEntity  ∷ !Word32        -- ^ the 'BuildingId'/'UnitId' raw value
    , mdrDefName ∷ !Text          -- ^ the unresolved definition name
    } deriving (Show, Eq)

renderMissingDefRef ∷ MissingDefRef → Text
renderMissingDefRef r =
    mdrKind r <> " #" <> T.pack (show (mdrEntity r)) <> " on page '"
        <> unWorldPageId (mdrPage r) <> "' references unknown definition '"
        <> mdrDefName r <> "'"
  where unWorldPageId (WorldPageId t) = t

-- | Every saved building/unit whose definition name is absent from the
--   registered-definition key sets, across all saved pages. Empty ⇒ every
--   content reference resolves and the load may proceed. Pure: the def
--   key-sets come from the live managers at the load boundary
--   (@'Building.Types.bmDefs'@/@'Unit.Types.umDefs'@), the per-page
--   snapshots from the decoded save.
missingDefReferences
    ∷ HS.HashSet Text                               -- ^ registered building def names
    → HS.HashSet Text                               -- ^ registered unit def names
    → [(WorldPageId, BuildingSnapshot, UnitSnapshot)]
    → [MissingDefRef]
missingDefReferences buildingDefs unitDefs pages = concatMap pageRefs pages
  where
    pageRefs (pid, bs, us) =
        [ MissingDefRef "building" pid (unBuildingId bid) (bisDefName b)
        | (bid, b) ← HM.toList (bsnInstances bs)
        , not (HS.member (bisDefName b) buildingDefs) ]
        ⧺
        [ MissingDefRef "unit" pid (unUnitId uid) (uisDefName u)
        | (uid, u) ← HM.toList (usnInstances us)
        , not (HS.member (uisDefName u) unitDefs) ]

-- Item / recipe / construct-target def-name validation (#760 round 8) -

-- | A saved 'ItemInstance' (anywhere in the save — building storage/
--   materials-delivered, unit inventory/equipped/accessories, ground
--   items) whose 'iiDefName' does not resolve against the currently-
--   registered item definitions. Checked recursively through
--   'iiContents' (a first-aid kit's own nested items) — same
--   load-validation contract as 'MissingDefRef' (requirement 9): the
--   complete load is rejected before any live state publishes, not a
--   silent per-item drop.
data MissingItemDefRef = MissingItemDefRef
    { midrSource  ∷ !Text          -- ^ e.g. "building storage", "unit inventory"
    , midrPage    ∷ !WorldPageId
    , midrItemId  ∷ !Word64        -- ^ the item's own 'iiInstanceId'
    , midrDefName ∷ !Text          -- ^ the unresolved item definition name
    } deriving (Show, Eq)

renderMissingItemDefRef ∷ MissingItemDefRef → Text
renderMissingItemDefRef r =
    midrSource r <> " item #" <> T.pack (show (midrItemId r)) <> " on page '"
        <> unWorldPageId (midrPage r) <> "' references unknown item \
           \definition '" <> midrDefName r <> "'"
  where unWorldPageId (WorldPageId t) = t

-- | One 'ItemInstance' plus every item nested (recursively) in its
--   'iiContents' (mirrors 'World.Save.Snapshot.flattenItemInstanceIds').
flattenItemInstances ∷ ItemInstance → [ItemInstance]
flattenItemInstances i = i : concatMap flattenItemInstances (iiContents i)

-- | Every saved item instance — across ground items, unit inventory/
--   equipped/accessories, and building storage/materials-delivered,
--   recursively through nested contents — whose def name is absent
--   from the registered item-definition key set. Empty ⇒ every item
--   reference resolves and the load may proceed.
missingItemDefReferences
    ∷ HS.HashSet Text                     -- ^ registered item def names
    → [(WorldPageId, WorldPageSave)]
    → [MissingItemDefRef]
missingItemDefReferences itemDefs pages = concatMap pageRefs pages
  where
    pageRefs (pid, w) = concat
        [ concatMap (buildingRefs pid) (HM.elems (bsnInstances (wpsBuildings w)))
        , concatMap (unitRefs pid) (HM.elems (usnInstances (wpsUnits w)))
        , concatMap (groundRefs pid) (HM.elems (gisItems (wpsGroundItems w)))
        ]
    buildingRefs pid b = concat
        [ concatMap (itemRefs pid "building storage") (bisStorage b)
        , concatMap (itemRefs pid "building materials delivered")
              (concat (HM.elems (bisMaterialsDelivered b)))
        ]
    unitRefs pid u = concat
        [ concatMap (itemRefs pid "unit inventory") (uisInventory u)
        , concatMap (itemRefs pid "unit equipped") (HM.elems (uisEquipped u))
        , concatMap (itemRefs pid "unit accessories") (uisAccessories u)
        ]
    groundRefs pid gi = itemRefs pid "ground item" (giInst gi)
    itemRefs pid src inst =
        [ MissingItemDefRef src pid (iiInstanceId i) (iiDefName i)
        | i ← flattenItemInstances inst
        , not (HS.member (iiDefName i) itemDefs) ]

-- | A saved craft bill whose 'cbRecipe' does not resolve against the
--   currently-registered recipe catalogue. Same load-validation
--   contract as 'MissingDefRef'.
data MissingRecipeRef = MissingRecipeRef
    { mrrPage   ∷ !WorldPageId
    , mrrBillId ∷ !Word32
    , mrrRecipe ∷ !Text
    } deriving (Show, Eq)

renderMissingRecipeRef ∷ MissingRecipeRef → Text
renderMissingRecipeRef r =
    "craft bill #" <> T.pack (show (mrrBillId r)) <> " on page '"
        <> unWorldPageId (mrrPage r) <> "' references unknown recipe '"
        <> mrrRecipe r <> "'"
  where unWorldPageId (WorldPageId t) = t

-- | Every saved craft bill, across all pages, whose recipe id is absent
--   from the registered recipe key set. Empty ⇒ every recipe reference
--   resolves and the load may proceed.
missingRecipeReferences
    ∷ HS.HashSet Text                     -- ^ registered recipe ids
    → [(WorldPageId, WorldPageSave)]
    → [MissingRecipeRef]
missingRecipeReferences recipeDefs pages =
    [ MissingRecipeRef pid (unBillId (cbId b)) (cbRecipe b)
    | (pid, w) ← pages
    , b ← HM.elems (cbsBills (wpsCraftBills w))
    , not (HS.member (cbRecipe b) recipeDefs) ]

-- | A saved craft bill's 'cbOutputItem' (#795 — the item-definition name an
--   UntilStock bill's stock target counts against, captured at add time)
--   that does not resolve against the currently-registered item
--   definitions. Same load-validation contract as 'MissingRecipeRef'/
--   'MissingItemDefRef': the complete load is rejected before any live
--   state publishes. 'cbOutputItem' is empty for FixedCount/RepeatForever
--   bills (never set), so only a non-empty value is checked here.
data MissingBillOutputItemRef = MissingBillOutputItemRef
    { mbirPage    ∷ !WorldPageId
    , mbirBillId  ∷ !Word32
    , mbirDefName ∷ !Text
    } deriving (Show, Eq)

renderMissingBillOutputItemRef ∷ MissingBillOutputItemRef → Text
renderMissingBillOutputItemRef r =
    "craft bill #" <> T.pack (show (mbirBillId r)) <> " on page '"
        <> unWorldPageId (mbirPage r) <> "' references unknown output item \
           \definition '" <> mbirDefName r <> "'"
  where unWorldPageId (WorldPageId t) = t

-- | Every saved craft bill, across all pages, whose non-empty
--   'cbOutputItem' is absent from the registered item-definition key set.
--   Empty ⇒ every bill output-item reference resolves (or has none) and
--   the load may proceed.
missingBillOutputItemReferences
    ∷ HS.HashSet Text                     -- ^ registered item def names
    → [(WorldPageId, WorldPageSave)]
    → [MissingBillOutputItemRef]
missingBillOutputItemReferences itemDefs pages =
    [ MissingBillOutputItemRef pid (unBillId (cbId b)) (cbOutputItem b)
    | (pid, w) ← pages
    , b ← HM.elems (cbsBills (wpsCraftBills w))
    , not (T.null (cbOutputItem b))
    , not (HS.member (cbOutputItem b) itemDefs) ]

-- | A saved construct designation whose target names a building
--   definition ('World.Construct.Types.CtBuilding') that does not
--   resolve against the currently-registered building definitions.
--   Same load-validation contract as 'MissingDefRef' — a 'CtStructure'
--   target references a structure-pack piece, not a building def, and
--   is out of scope here.
data MissingConstructDefRef = MissingConstructDefRef
    { mcdPage    ∷ !WorldPageId
    , mcdTile    ∷ !(Int, Int)
    , mcdDefName ∷ !Text
    } deriving (Show, Eq)

renderMissingConstructDefRef ∷ MissingConstructDefRef → Text
renderMissingConstructDefRef r =
    "construct designation at " <> T.pack (show (mcdTile r)) <> " on page '"
        <> unWorldPageId (mcdPage r) <> "' references unknown building \
           \definition '" <> mcdDefName r <> "'"
  where unWorldPageId (WorldPageId t) = t

-- | Every saved construct designation, across all pages, whose
--   'CtBuilding' target names a building definition absent from the
--   registered building key set. Empty ⇒ every construct-target
--   reference resolves and the load may proceed.
missingConstructDefReferences
    ∷ HS.HashSet Text                     -- ^ registered building def names
    → [(WorldPageId, WorldPageSave)]
    → [MissingConstructDefRef]
missingConstructDefReferences buildingDefs pages =
    [ MissingConstructDefRef pid tile defName
    | (pid, w) ← pages
    , (tile, cd) ← HM.toList (wpsConstructDesignations w)
    , CtBuilding defName ← [cdTarget cd]
    , not (HS.member defName buildingDefs) ]

-- Material-id validation (issue #763 round-3 review) -----------------

-- | A saved 'MaterialId' — from the edit log ('WeAddTile'/'WeSetCell'),
--   a spoil pile, or a worldgen tectonic plate's base material — that
--   this build's 'World.Material.MaterialRegistry' never registered.
--   Unlike every other 'Missing*Ref' above, 'MaterialId' is a numeric
--   'Word8' index that is ALWAYS a structurally valid slot (see
--   'World.Material.isKnownMaterial''s haddock), so this can only mean
--   the material genuinely existed in the save's origin build's YAML
--   data and was later removed from this one — same load-validation
--   contract as every other missing reference (the issue's own
--   acceptance criteria names "material" explicitly alongside
--   unit/item/building/recipe).
data MissingMaterialRef = MissingMaterialRef
    { mmrSource ∷ !Text          -- ^ e.g. "edit log", "spoil pile", "tectonic plate"
    , mmrPage   ∷ !WorldPageId
    , mmrCoord  ∷ !(Int, Int)
    , mmrMatId  ∷ !Word8
    } deriving (Show, Eq)

renderMissingMaterialRef ∷ MissingMaterialRef → Text
renderMissingMaterialRef r =
    mmrSource r <> " at " <> T.pack (show (mmrCoord r)) <> " on page '"
        <> unWorldPageId (mmrPage r) <> "' references unknown material id "
        <> T.pack (show (mmrMatId r))
  where unWorldPageId (WorldPageId t) = t

-- | Every saved material reference, across all pages, that does not
--   resolve against the currently-registered material set. Covers the
--   edit log, spoil piles, AND each page's worldgen plate data
--   ('wgpPlates' — round 4 review: a plate's base material is
--   persisted just like any other 'MaterialId' and staging would
--   otherwise silently render it with 'defaultMaterialProps' instead
--   of rejecting the load). Empty ⇒ every reference resolves and the
--   load may proceed.
missingMaterialReferences
    ∷ MaterialRegistry
    → [(WorldPageId, WorldPageSave)]
    → [MissingMaterialRef]
missingMaterialReferences registry pages = concatMap pageRefs pages
  where
    pageRefs (pid, w) =
        [ MissingMaterialRef "edit log" pid (gx, gy) (unMaterialId mat)
        | edits ← HM.elems (wpsEdits w)
        , edit ← edits
        , (gx, gy, mat) ← editMaterialRef edit
        , not (isKnownMaterial registry mat) ]
        ⧺
        [ MissingMaterialRef "spoil pile" pid coord (unMaterialId (spMat sp))
        | (coord, sp) ← HM.toList (wpsSpoilPiles w)
        , not (isKnownMaterial registry (spMat sp)) ]
        ⧺
        [ MissingMaterialRef "tectonic plate" pid
              (plateCenterX plate, plateCenterY plate)
              (unMaterialId (plateMaterial plate))
        | plate ← wgpPlates (wpsGenParams w)
        , not (isKnownMaterial registry (plateMaterial plate)) ]
    editMaterialRef (WeAddTile gx gy mat)   = [(gx, gy, mat)]
    editMaterialRef (WeSetCell gx gy _z mat) = [(gx, gy, mat)]
    editMaterialRef _                        = []

-- Flora-id validation (issue #763 round-5 review) --------------------

-- | A saved 'FloraId' — from the edit log ('WePlaceFlora') or a crop
--   plot's species — that this build's 'World.Flora.Types.FloraCatalog'
--   does not resolve. Unlike 'MaterialId', a 'FloraId' genuinely CAN be
--   invalid ('lookupSpecies' returns 'Maybe', no always-total vector
--   slot to fall back on), so this is a direct existence check, no
--   special-cased sentinel id needed. Flora species drive crop/foraging
--   gameplay (growth stage, harvest yield, ...), so an unresolved one
--   is exactly the same class of load-blocking problem as a missing
--   unit/item/building/recipe/material definition.
data MissingFloraRef = MissingFloraRef
    { mfrSource ∷ !Text          -- ^ e.g. "edit log", "crop plot"
    , mfrPage   ∷ !WorldPageId
    , mfrCoord  ∷ !(Int, Int)
    , mfrFloraId ∷ !Word16
    } deriving (Show, Eq)

renderMissingFloraRef ∷ MissingFloraRef → Text
renderMissingFloraRef r =
    mfrSource r <> " at " <> T.pack (show (mfrCoord r)) <> " on page '"
        <> unWorldPageId (mfrPage r) <> "' references unknown flora id "
        <> T.pack (show (mfrFloraId r))
  where unWorldPageId (WorldPageId t) = t

-- | Every saved flora-species reference, across all pages, that does
--   not resolve against the currently-registered flora catalog. Covers
--   the edit log ('WePlaceFlora') and crop plots ('cpSpecies'). Empty
--   ⇒ every reference resolves and the load may proceed.
missingFloraReferences
    ∷ FloraCatalog
    → [(WorldPageId, WorldPageSave)]
    → [MissingFloraRef]
missingFloraReferences catalog pages = concatMap pageRefs pages
  where
    pageRefs (pid, w) =
        [ MissingFloraRef "edit log" pid (gx, gy) (unFloraId fid)
        | edits ← HM.elems (wpsEdits w)
        , edit ← edits
        , (gx, gy, fid) ← editFloraRef edit
        , unresolved fid ]
        ⧺
        [ MissingFloraRef "crop plot" pid coord (unFloraId (cpSpecies cp))
        | (coord, cp) ← HM.toList (wpsCropPlots w)
        , unresolved (cpSpecies cp) ]
    editFloraRef (WePlaceFlora gx gy fid _day _grow) = [(gx, gy, fid)]
    editFloraRef _                                    = []
    unresolved fid = maybe True (const False) (lookupSpecies fid catalog)

-- Location-overlay-id validation (issue #763 round-7 review) ---------

-- | A saved location-overlay entry ('WorldGenParams.wgpLocationOverlay',
--   one per stamped chunk) whose location id does not resolve against
--   the currently-registered 'Location.Types.LocationRegistry'. Unlike
--   'MaterialId'/'FloraId', this is a plain Text key — same shape as
--   'MissingDefRef' — but it lives on 'WorldGenParams' rather than
--   inside 'WorldPageSave' proper, which is why it needed its own
--   validation function even though the check itself is a direct
--   'HS.HashSet' membership test. A missing location definition would
--   otherwise silently skip location discovery/placement-bounds
--   checks for that chunk after publication instead of rejecting the
--   load.
data MissingLocationRef = MissingLocationRef
    { mlrPage  ∷ !WorldPageId
    , mlrCoord ∷ !(Int, Int)
    , mlrLocId ∷ !Text
    } deriving (Show, Eq)

renderMissingLocationRef ∷ MissingLocationRef → Text
renderMissingLocationRef r =
    "location overlay chunk " <> T.pack (show (mlrCoord r)) <> " on page '"
        <> unWorldPageId (mlrPage r) <> "' references unknown location id '"
        <> mlrLocId r <> "'"
  where unWorldPageId (WorldPageId t) = t

-- | Every saved location-overlay reference, across all pages, that does
--   not resolve against the currently-registered location definitions.
--   Empty ⇒ every reference resolves and the load may proceed.
missingLocationOverlayReferences
    ∷ HS.HashSet Text                     -- ^ registered location def ids
    → [(WorldPageId, WorldPageSave)]
    → [MissingLocationRef]
missingLocationOverlayReferences locationDefs pages = concatMap pageRefs pages
  where
    pageRefs (pid, w) =
        [ MissingLocationRef pid (cx, cy) locId
        | (ChunkCoord cx cy, locId) ← HM.toList (wgpLocationOverlay (wpsGenParams w))
        , not (HS.member locId locationDefs) ]

-- Infection-definition validation (issue #763 round-8 review) --------

-- | A saved 'Wound' whose 'woundInfectionType' does not resolve against
--   the currently-registered 'Infection.Types.InfectionManager'. Empty
--   string is the documented "no infection" sentinel (every wound
--   starts this way; see 'Combat.Resolution'/'Combat.Wounds.Sever') and
--   is deliberately excluded, mirroring 'World.Material.isKnownMaterial'\'s
--   own air (id 0) exclusion — same shape of problem, a real sentinel
--   value that must never be treated as "missing". A genuinely
--   unresolved infection type is a required-content gap of the same
--   kind as a missing unit/item/building/recipe/material/location
--   definition: 'Engine.Scripting.Lua.API.Units.Combat.lookupInfection'
--   drives the actual gameplay treatment/progression path off it, so
--   loading with a fallback here would silently change the wound's
--   behavior after publication rather than rejecting the load.
data MissingInfectionRef = MissingInfectionRef
    { mirPage    ∷ !WorldPageId
    , mirUnitId  ∷ !Word32
    , mirWoundPart ∷ !Text
    , mirInfType ∷ !Text
    } deriving (Show, Eq)

renderMissingInfectionRef ∷ MissingInfectionRef → Text
renderMissingInfectionRef r =
    "unit #" <> T.pack (show (mirUnitId r)) <> " wound (" <> mirWoundPart r
        <> ") on page '" <> unWorldPageId (mirPage r)
        <> "' references unknown infection id '" <> mirInfType r <> "'"
  where unWorldPageId (WorldPageId t) = t

-- | Every saved wound-infection reference, across all pages, that does
--   not resolve against the currently-registered infection catalogue.
--   Empty ⇒ every reference resolves and the load may proceed.
missingInfectionReferences
    ∷ InfectionManager
    → [(WorldPageId, WorldPageSave)]
    → [MissingInfectionRef]
missingInfectionReferences infMgr pages = concatMap pageRefs pages
  where
    pageRefs (pid, w) =
        [ MissingInfectionRef pid (unUnitId uid) (woundPart wd) infType
        | (uid, u) ← HM.toList (usnInstances (wpsUnits w))
        , wd ← uisWounds u
        , let infType = woundInfectionType wd
        , not (T.null infType)
        , not (isJust (lookupInfection infType infMgr)) ]
    isJust (Just _) = True
    isJust Nothing  = False
