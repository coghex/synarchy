{-# LANGUAGE Strict, UnicodeSyntax, DeriveGeneric, DeriveAnyClass, DerivingStrategies #-}
-- | Page-scoped world components (issue #760, save-overhaul B2). Each
--   carries a slice PER world page, keyed by 'WorldPageId', and every
--   one is validated against the authoritative page set the
--   @"world-pages"@ component establishes (requirement 8):
--
--   - @"world-pages"@ (required, page-set authority) — per page: identity,
--     generation params, dates/clocks, map mode, and the page's OWN
--     remembered camera position. Owner: the world page. Boundary reason:
--     this is the spine every other page-scoped component's page set is
--     checked against; the world-generation seed lives in its gen params
--     (requirement 10). No dependencies — it is the root of the page
--     dependency graph.
--   - @"world-edits"@ (required) — per page: the terrain + structure edit
--     log. Owner: the world edit layer. Boundary reason: player terrain/
--     structure modifications are a distinct, replay-on-load concern.
--   - @"world-activity"@ (required) — per page: designations (mine/
--     construct/chop/till/plant), flora harvests, crop plots, ground
--     items, and spoil piles. Owner: the mutable-world-activity layer.
--     Boundary reason: requirement 2 bullet 4's "designations, jobs,
--     progress, flora, crops, ground items, spoil" — the transient-ish
--     but persisted world activity, grouped away from the terrain spine
--     and the entity managers.
--
--   Every slice list is encoded in canonical (page-id ascending) order
--   so identical input produces identical bytes (requirement 10).
--
--   Requirement 4 — the on-disk contract is FROZEN, distinct from every
--   mutable runtime record. NONE of the evolving live gameplay records
--   is embedded directly here; each is mirrored by a component-owned
--   DTO with an explicit, reviewable field-by-field conversion
--   ('to…'/'from…'), exactly the discipline "World.Save.Component.Entities"
--   applies to the unit-sim / craft-bill / power-node records:
--
--   - 'WorldGenParams'      → 'WorldGenParamsDTO'
--   - 'WorldIdentity'       → 'WorldIdentityDTO'
--   - 'WorldEdit'           → 'WorldEditDTO' (its own frozen tag order,
--                             decoupled from the live sum's constructor
--                             order, so REORDERING the live type can no
--                             longer silently corrupt v1 bytes)
--   - 'MineDesignation'     → 'MineDesignationDTO'
--   - 'ConstructDesignation'→ 'ConstructDesignationDTO'
--                             ('StructurePieceDTO'/'ConstructTargetDTO')
--   - 'ChopDesignation'     → 'ChopDesignationDTO'
--   - 'TillDesignation'     → 'TillDesignationDTO'
--   - 'PlantDesignation'    → 'PlantDesignationDTO'
--   - 'CropPlot'            → 'CropPlotDTO'
--   - 'GroundItem'/'GroundItems' → 'GroundItemDTO'/'GroundItemsDTO'
--   - 'SpoilPile'           → 'SpoilPileDTO'
--
--   A field/constructor added, dropped, or reordered on any of those
--   live records surfaces here as a compile error in its @from…@
--   conversion, never as silent byte drift in a shipped v1 save. As in
--   Entities, genuine LEAF content references are reused as-is rather
--   than mirrored, because they are themselves append-only, save-version-
--   governed content, and mirroring them would be both absurd and no
--   safer than reusing them: the payload-free append-only enums
--   ('ZoomMapMode', 'ConstructStatus'), the durable coordinate/id/content
--   references ('ChunkCoord', 'FluidType', 'MaterialId', 'FloraId',
--   'ItemInstance'), a bare 'Float' regrowth timer ('FloraHarvests', which
--   has no record at all to freeze), and — inside 'WorldGenParamsDTO' —
--   the deeply nested worldgen CONFIG sub-records ('CalendarConfig',
--   'SunConfig', 'MoonConfig', 'GeoTimeline', 'OceanMap', 'OceanDistMap',
--   'ClimateParams', 'ClimateState', 'OreLevers', 'TimelineParams',
--   'LocationOverlay', 'TectonicPlate'), which are the worldgen layer's
--   own independently save-governed content (the same boundary Entities
--   draws at the already-frozen 'BuildingInstanceSnapshot'/
--   'UnitInstanceSnapshot' persistence types). The DTO field order is
--   chosen so the derived cereal layout is byte-identical to the previous
--   direct embedding — the frozen tracked fixture stays valid.
module World.Save.Component.Page
    ( worldPagesCodec
    , worldEditsCodec
    , worldActivityCodec
    , PageCoreDTO(..)
    , WorldPagesDTO(..)
    , PageEditsDTO(..)
    , WorldEditsDTO(..)
    , PageActivityDTO(..)
    , WorldActivityDTO(..)
      -- * Frozen leaf DTOs (requirement 4)
    , WorldGenParamsDTO(..)
    , WorldIdentityDTO(..)
    , WorldEditDTO(..)
    , MineDesignationDTO(..)
    , StructurePieceDTO(..)
    , ConstructTargetDTO(..)
    , ConstructDesignationDTO(..)
    , ChopDesignationDTO(..)
    , TillDesignationDTO(..)
    , PlantDesignationDTO(..)
    , CropPlotDTO(..)
    , GroundItemDTO(..)
    , GroundItemsDTO(..)
    , SpoilPileDTO(..)
    , toWorldGenParamsDTO
    , fromWorldGenParamsDTO
    , basePageSnapshots
    , applyWorldEdits
    , applyWorldActivity
    ) where

import UPrelude
import qualified Data.HashMap.Strict as HM
import qualified Data.HashSet as HS
import qualified Data.List as L
import qualified Data.Text as T
import Data.Serialize (Serialize)
import GHC.Generics (Generic)
import Craft.Bills (emptyCraftBills)
import Power.Types (emptyPowerNodes)
import World.Generate.Types (WorldGenParams(..), withVolcanoCtx)
import World.Magma.Types (emptyVolcanoCtx)
import World.Plate.Types (TectonicPlate)
import World.Time.Types (CalendarConfig, SunConfig, MoonConfig)
import World.Geology.Timeline.Types (GeoTimeline, TimelineParams)
import World.Ocean.Types (OceanMap, OceanDistMap)
import World.Weather.Types (ClimateParams, ClimateState)
import World.Geology.Ore.Types (OreLevers)
import Location.Overlay.Types (LocationOverlay)
import World.Chunk.Types (ChunkCoord)
import World.Page.Types (WorldPageId, WorldIdentity(..))
import World.Render.Zoom.Types (ZoomMapMode)
import World.Edit.Types (WorldEdit(..), WorldEdits, emptyWorldEdits)
import World.Fluid.Types (FluidType)
import World.Material.Id (MaterialId)
import World.Flora.Types (FloraId)
import World.Mine.Types (MineDesignation(..), MineDesignations)
import World.Construct.Types
    ( ConstructDesignation(..), ConstructTarget(..), StructurePiece(..)
    , ConstructStatus, ConstructDesignations )
import World.Chop.Types (ChopDesignation(..), ChopDesignations)
import World.Till.Types (TillDesignation(..), TillDesignations)
import World.Plant.Types (PlantDesignation(..), PlantDesignations)
import World.Spoil.Types (SpoilPile(..), SpoilPiles, emptySpoilPiles)
import World.Flora.Harvest (FloraHarvests, emptyFloraHarvests)
import World.Flora.CropPlot (CropPlot(..), CropPlots, emptyCropPlots)
import Item.Ground (GroundItem(..), GroundItems(..), emptyGroundItems)
import Item.Types (ItemInstance)
import World.Save.Types
    ( BuildingSnapshot(..), UnitSnapshot(..) )
import World.Save.Snapshot (SessionSnapshot(..), PageSnapshot(..))
import World.Save.Component.Types

-- Canonical (page-id ascending) ordered list of a snapshot's pages.
orderedPages ∷ SessionSnapshot → [PageSnapshot]
orderedPages = L.sortOn pgsPageId . HM.elems . snapPages

tshow ∷ Show a ⇒ a → Text
tshow = T.pack . show

-- Frozen leaf DTOs (requirement 4) -----------------------------------

-- | Frozen mirror of 'WorldGenParams' (a mutable runtime record that
--   gains fields as worldgen features land — the #89/#90/#424/#780
--   location flags are the recent examples). Field order matches
--   'WorldGenParams''s MANUAL 'Serialize' instance exactly (every field
--   except the transient @wgpVolcanoCtx@, which that instance also
--   skips and rebuilds on load), so the derived cereal layout here is
--   byte-identical to embedding the record directly. Nested worldgen
--   config sub-records are reused as leaves (see the module haddock).
data WorldGenParamsDTO = WorldGenParamsDTO
    { gpSeed                   ∷ !Word64
    , gpWorldSize              ∷ !Int
    , gpPlateCount             ∷ !Int
    , gpPlates                 ∷ ![TectonicPlate]
    , gpCalender               ∷ !CalendarConfig
    , gpSunConfig              ∷ !SunConfig
    , gpMoonConfig             ∷ !MoonConfig
    , gpGeoTimeline            ∷ !GeoTimeline
    , gpOceanMap               ∷ !OceanMap
    , gpOceanDist              ∷ !OceanDistMap
    , gpClimateParams          ∷ !ClimateParams
    , gpClimateState           ∷ !ClimateState
    , gpErosionIntensity       ∷ !Float
    , gpVolcanicActivity       ∷ !Float
    , gpLavaPoolDepth          ∷ !Int
    , gpLavaPoolRadius         ∷ !Int
    , gpWaterfallQuantum       ∷ !Int
    , gpOreLevers              ∷ !OreLevers
    , gpTimelineParams         ∷ !TimelineParams
    , gpLocationOverlay        ∷ !LocationOverlay
    , gpLocationContentsSpawned ∷ !(HS.HashSet ChunkCoord)
    , gpLocationStamped        ∷ !(HS.HashSet ChunkCoord)
    , gpLocationDiscovered     ∷ !(HS.HashSet ChunkCoord)
    } deriving (Show, Eq, Generic, Serialize)

toWorldGenParamsDTO ∷ WorldGenParams → WorldGenParamsDTO
toWorldGenParamsDTO p = WorldGenParamsDTO
    { gpSeed                    = wgpSeed p
    , gpWorldSize               = wgpWorldSize p
    , gpPlateCount              = wgpPlateCount p
    , gpPlates                  = wgpPlates p
    , gpCalender                = wgpCalender p
    , gpSunConfig               = wgpSunConfig p
    , gpMoonConfig              = wgpMoonConfig p
    , gpGeoTimeline             = wgpGeoTimeline p
    , gpOceanMap                = wgpOceanMap p
    , gpOceanDist               = wgpOceanDist p
    , gpClimateParams           = wgpClimateParams p
    , gpClimateState            = wgpClimateState p
    , gpErosionIntensity        = wgpErosionIntensity p
    , gpVolcanicActivity        = wgpVolcanicActivity p
    , gpLavaPoolDepth           = wgpLavaPoolDepth p
    , gpLavaPoolRadius          = wgpLavaPoolRadius p
    , gpWaterfallQuantum        = wgpWaterfallQuantum p
    , gpOreLevers               = wgpOreLevers p
    , gpTimelineParams          = wgpTimelineParams p
    , gpLocationOverlay         = wgpLocationOverlay p
    , gpLocationContentsSpawned = wgpLocationContentsSpawned p
    , gpLocationStamped         = wgpLocationStamped p
    , gpLocationDiscovered      = wgpLocationDiscovered p
    }

-- | Rebuild the live record from the DTO, restoring the transient
--   @wgpVolcanoCtx@ via 'withVolcanoCtx' exactly the way the manual
--   'Serialize' instance's @get@ does (from seed / world-size / plates /
--   timeline). Adding a field to 'WorldGenParams' breaks THIS
--   construction — the conscious reconciliation requirement 4 asks for.
fromWorldGenParamsDTO ∷ WorldGenParamsDTO → WorldGenParams
fromWorldGenParamsDTO d = withVolcanoCtx WorldGenParams
    { wgpSeed                    = gpSeed d
    , wgpWorldSize               = gpWorldSize d
    , wgpPlateCount              = gpPlateCount d
    , wgpPlates                  = gpPlates d
    , wgpCalender                = gpCalender d
    , wgpSunConfig               = gpSunConfig d
    , wgpMoonConfig              = gpMoonConfig d
    , wgpGeoTimeline             = gpGeoTimeline d
    , wgpOceanMap                = gpOceanMap d
    , wgpOceanDist               = gpOceanDist d
    , wgpClimateParams           = gpClimateParams d
    , wgpClimateState            = gpClimateState d
    , wgpErosionIntensity        = gpErosionIntensity d
    , wgpVolcanicActivity        = gpVolcanicActivity d
    , wgpLavaPoolDepth           = gpLavaPoolDepth d
    , wgpLavaPoolRadius          = gpLavaPoolRadius d
    , wgpWaterfallQuantum        = gpWaterfallQuantum d
    , wgpOreLevers               = gpOreLevers d
    , wgpTimelineParams          = gpTimelineParams d
    , wgpLocationOverlay         = gpLocationOverlay d
    , wgpLocationContentsSpawned = gpLocationContentsSpawned d
    , wgpLocationStamped         = gpLocationStamped d
    , wgpLocationDiscovered      = gpLocationDiscovered d
    , wgpVolcanoCtx              = emptyVolcanoCtx
    }

-- | Frozen mirror of 'WorldIdentity'.
data WorldIdentityDTO = WorldIdentityDTO
    { widName  ∷ !Text
    , widGloss ∷ !(Maybe Text)
    } deriving (Show, Eq, Generic, Serialize)

toWorldIdentityDTO ∷ WorldIdentity → WorldIdentityDTO
toWorldIdentityDTO i = WorldIdentityDTO (wiName i) (wiGloss i)

fromWorldIdentityDTO ∷ WorldIdentityDTO → WorldIdentity
fromWorldIdentityDTO d = WorldIdentity (widName d) (widGloss d)

-- | Frozen mirror of 'WorldEdit'. Its OWN constructor order is the wire
--   contract, decoupled from the live sum's — so reordering the live
--   constructors (which the live type's own append-only comment warns
--   against) can no longer silently corrupt a shipped v1 save. Adding a
--   live constructor makes 'toWorldEditDTO' non-exhaustive (a compile
--   warning under @-Werror@), forcing a conscious DTO extension. Leaf
--   payload references ('FluidType'/'MaterialId'/'FloraId') are reused.
data WorldEditDTO
    = WeDeleteTileD !Int !Int
    | WeSetFluidTileD !Int !Int !FluidType
    | WeAddTileD !Int !Int !MaterialId
    | WeSetSlopeD !Int !Int !Int !Word8
    | WeSetCellD !Int !Int !Int !MaterialId
    | WeSetStructureD !Int !Int !Word8 !Int !Int !Int
    | WeClearStructureD !Int !Int !Word8
    | WeSetVegD !Int !Int !Int !Word8
    | WePlaceFloraD !Int !Int !FloraId !Int !Float
    | WeSetFluidSnapshotD !Int !Int !FluidType !Int
    | WeClearFluidSnapshotD !Int !Int
    deriving (Show, Eq, Generic, Serialize)

toWorldEditDTO ∷ WorldEdit → WorldEditDTO
toWorldEditDTO (WeDeleteTile a b)              = WeDeleteTileD a b
toWorldEditDTO (WeSetFluidTile a b f)          = WeSetFluidTileD a b f
toWorldEditDTO (WeAddTile a b m)               = WeAddTileD a b m
toWorldEditDTO (WeSetSlope a b c w)            = WeSetSlopeD a b c w
toWorldEditDTO (WeSetCell a b c m)             = WeSetCellD a b c m
toWorldEditDTO (WeSetStructure a b w c d e)    = WeSetStructureD a b w c d e
toWorldEditDTO (WeClearStructure a b w)        = WeClearStructureD a b w
toWorldEditDTO (WeSetVeg a b c w)              = WeSetVegD a b c w
toWorldEditDTO (WePlaceFlora a b fl d fx)      = WePlaceFloraD a b fl d fx
toWorldEditDTO (WeSetFluidSnapshot a b f z)    = WeSetFluidSnapshotD a b f z
toWorldEditDTO (WeClearFluidSnapshot a b)      = WeClearFluidSnapshotD a b

fromWorldEditDTO ∷ WorldEditDTO → WorldEdit
fromWorldEditDTO (WeDeleteTileD a b)           = WeDeleteTile a b
fromWorldEditDTO (WeSetFluidTileD a b f)       = WeSetFluidTile a b f
fromWorldEditDTO (WeAddTileD a b m)            = WeAddTile a b m
fromWorldEditDTO (WeSetSlopeD a b c w)         = WeSetSlope a b c w
fromWorldEditDTO (WeSetCellD a b c m)          = WeSetCell a b c m
fromWorldEditDTO (WeSetStructureD a b w c d e) = WeSetStructure a b w c d e
fromWorldEditDTO (WeClearStructureD a b w)     = WeClearStructure a b w
fromWorldEditDTO (WeSetVegD a b c w)           = WeSetVeg a b c w
fromWorldEditDTO (WePlaceFloraD a b fl d fx)   = WePlaceFlora a b fl d fx
fromWorldEditDTO (WeSetFluidSnapshotD a b f z) = WeSetFluidSnapshot a b f z
fromWorldEditDTO (WeClearFluidSnapshotD a b)   = WeClearFluidSnapshot a b

-- | Frozen mirror of 'MineDesignation'.
data MineDesignationDTO = MineDesignationDTO
    { mdiZ             ∷ !Int
    , mdiCorners       ∷ !(Float, Float, Float, Float)
    , mdiChunkProgress ∷ !Float
    } deriving (Show, Eq, Generic, Serialize)

toMineDesignationDTO ∷ MineDesignation → MineDesignationDTO
toMineDesignationDTO m =
    MineDesignationDTO (mdZ m) (mdCorners m) (mdChunkProgress m)

fromMineDesignationDTO ∷ MineDesignationDTO → MineDesignation
fromMineDesignationDTO d =
    MineDesignation (mdiZ d) (mdiCorners d) (mdiChunkProgress d)

-- | Frozen mirror of 'StructurePiece'.
data StructurePieceDTO = StructurePieceDTO
    { spiPack ∷ !Text
    , spiKind ∷ !Text
    , spiEdge ∷ !(Maybe Text)
    } deriving (Show, Eq, Generic, Serialize)

toStructurePieceDTO ∷ StructurePiece → StructurePieceDTO
toStructurePieceDTO s = StructurePieceDTO (spPack s) (spKind s) (spEdge s)

fromStructurePieceDTO ∷ StructurePieceDTO → StructurePiece
fromStructurePieceDTO d = StructurePiece (spiPack d) (spiKind d) (spiEdge d)

-- | Frozen mirror of 'ConstructTarget'.
data ConstructTargetDTO
    = CtStructureD !StructurePieceDTO
    | CtBuildingD !Text
    deriving (Show, Eq, Generic, Serialize)

toConstructTargetDTO ∷ ConstructTarget → ConstructTargetDTO
toConstructTargetDTO (CtStructure p) = CtStructureD (toStructurePieceDTO p)
toConstructTargetDTO (CtBuilding n)  = CtBuildingD n

fromConstructTargetDTO ∷ ConstructTargetDTO → ConstructTarget
fromConstructTargetDTO (CtStructureD p) = CtStructure (fromStructurePieceDTO p)
fromConstructTargetDTO (CtBuildingD n)  = CtBuilding n

-- | Frozen mirror of 'ConstructDesignation'. 'ConstructStatus' is a
--   payload-free append-only enum, reused as a leaf (see the module
--   haddock) exactly like 'Pose'/'Direction' in Entities.
data ConstructDesignationDTO = ConstructDesignationDTO
    { cdiZ             ∷ !Int
    , cdiTarget        ∷ !ConstructTargetDTO
    , cdiStatus        ∷ !ConstructStatus
    , cdiProgress      ∷ !Float
    , cdiMaterialsPaid ∷ !Bool
    } deriving (Show, Eq, Generic, Serialize)

toConstructDesignationDTO ∷ ConstructDesignation → ConstructDesignationDTO
toConstructDesignationDTO c = ConstructDesignationDTO
    { cdiZ             = cdZ c
    , cdiTarget        = toConstructTargetDTO (cdTarget c)
    , cdiStatus        = cdStatus c
    , cdiProgress      = cdProgress c
    , cdiMaterialsPaid = cdMaterialsPaid c
    }

fromConstructDesignationDTO ∷ ConstructDesignationDTO → ConstructDesignation
fromConstructDesignationDTO d = ConstructDesignation
    { cdZ             = cdiZ d
    , cdTarget        = fromConstructTargetDTO (cdiTarget d)
    , cdStatus        = cdiStatus d
    , cdProgress      = cdiProgress d
    , cdMaterialsPaid = cdiMaterialsPaid d
    }

-- | Frozen mirror of 'ChopDesignation'.
newtype ChopDesignationDTO = ChopDesignationDTO { chiZ ∷ Int }
    deriving stock (Generic)
    deriving newtype (Show, Eq)
    deriving anyclass (Serialize)

toChopDesignationDTO ∷ ChopDesignation → ChopDesignationDTO
toChopDesignationDTO = ChopDesignationDTO . chZ

fromChopDesignationDTO ∷ ChopDesignationDTO → ChopDesignation
fromChopDesignationDTO = ChopDesignation . chiZ

-- | Frozen mirror of 'TillDesignation'.
newtype TillDesignationDTO = TillDesignationDTO { tliZ ∷ Int }
    deriving stock (Generic)
    deriving newtype (Show, Eq)
    deriving anyclass (Serialize)

toTillDesignationDTO ∷ TillDesignation → TillDesignationDTO
toTillDesignationDTO = TillDesignationDTO . tlZ

fromTillDesignationDTO ∷ TillDesignationDTO → TillDesignation
fromTillDesignationDTO = TillDesignation . tliZ

-- | Frozen mirror of 'PlantDesignation'.
data PlantDesignationDTO = PlantDesignationDTO
    { ptiZ    ∷ !Int
    , ptiCrop ∷ !FloraId
    } deriving (Show, Eq, Generic, Serialize)

toPlantDesignationDTO ∷ PlantDesignation → PlantDesignationDTO
toPlantDesignationDTO p = PlantDesignationDTO (ptZ p) (ptCrop p)

fromPlantDesignationDTO ∷ PlantDesignationDTO → PlantDesignation
fromPlantDesignationDTO d = PlantDesignation (ptiZ d) (ptiCrop d)

-- | Frozen mirror of 'CropPlot'.
data CropPlotDTO = CropPlotDTO
    { cpiSpecies    ∷ !FloraId
    , cpiPlantedDay ∷ !Int
    , cpiHealth     ∷ !Float
    } deriving (Show, Eq, Generic, Serialize)

toCropPlotDTO ∷ CropPlot → CropPlotDTO
toCropPlotDTO c = CropPlotDTO (cpSpecies c) (cpPlantedDay c) (cpHealth c)

fromCropPlotDTO ∷ CropPlotDTO → CropPlot
fromCropPlotDTO d = CropPlot (cpiSpecies d) (cpiPlantedDay d) (cpiHealth d)

-- | Frozen mirror of 'GroundItem'. 'ItemInstance' is reused as a leaf
--   content reference (the same one 'BuildingInstanceSnapshot'/
--   'UnitInstanceSnapshot' already carry).
data GroundItemDTO = GroundItemDTO
    { giiInst ∷ !ItemInstance
    , giiX    ∷ !Float
    , giiY    ∷ !Float
    } deriving (Show, Eq, Generic, Serialize)

toGroundItemDTO ∷ GroundItem → GroundItemDTO
toGroundItemDTO g = GroundItemDTO (giInst g) (giX g) (giY g)

fromGroundItemDTO ∷ GroundItemDTO → GroundItem
fromGroundItemDTO d = GroundItem (giiInst d) (giiX d) (giiY d)

-- | Frozen mirror of the 'GroundItems' registry (its id counter + map).
data GroundItemsDTO = GroundItemsDTO
    { gisiNextId ∷ !Int
    , gisiItems  ∷ !(HM.HashMap Int GroundItemDTO)
    } deriving (Show, Eq, Generic, Serialize)

toGroundItemsDTO ∷ GroundItems → GroundItemsDTO
toGroundItemsDTO g =
    GroundItemsDTO (gisNextId g) (HM.map toGroundItemDTO (gisItems g))

fromGroundItemsDTO ∷ GroundItemsDTO → GroundItems
fromGroundItemsDTO d =
    GroundItems (gisiNextId d) (HM.map fromGroundItemDTO (gisiItems d))

-- | Frozen mirror of 'SpoilPile'.
data SpoilPileDTO = SpoilPileDTO
    { spiMat  ∷ !MaterialId
    , spiFill ∷ !(Float, Float, Float, Float)
    } deriving (Show, Eq, Generic, Serialize)

toSpoilPileDTO ∷ SpoilPile → SpoilPileDTO
toSpoilPileDTO s = SpoilPileDTO (spMat s) (spFill s)

fromSpoilPileDTO ∷ SpoilPileDTO → SpoilPile
fromSpoilPileDTO d = SpoilPile (spiMat d) (spiFill d)

-- Tile-keyed map conversions (each value goes through its own DTO;
-- keys are plain coordinate leaves).
toMineDTO ∷ MineDesignations → HM.HashMap (Int, Int) MineDesignationDTO
toMineDTO = HM.map toMineDesignationDTO
fromMineDTO ∷ HM.HashMap (Int, Int) MineDesignationDTO → MineDesignations
fromMineDTO = HM.map fromMineDesignationDTO

toConstructDTO ∷ ConstructDesignations
               → HM.HashMap (Int, Int) ConstructDesignationDTO
toConstructDTO = HM.map toConstructDesignationDTO
fromConstructDTO ∷ HM.HashMap (Int, Int) ConstructDesignationDTO
                 → ConstructDesignations
fromConstructDTO = HM.map fromConstructDesignationDTO

toChopDTO ∷ ChopDesignations → HM.HashMap (Int, Int) ChopDesignationDTO
toChopDTO = HM.map toChopDesignationDTO
fromChopDTO ∷ HM.HashMap (Int, Int) ChopDesignationDTO → ChopDesignations
fromChopDTO = HM.map fromChopDesignationDTO

toTillDTO ∷ TillDesignations → HM.HashMap (Int, Int) TillDesignationDTO
toTillDTO = HM.map toTillDesignationDTO
fromTillDTO ∷ HM.HashMap (Int, Int) TillDesignationDTO → TillDesignations
fromTillDTO = HM.map fromTillDesignationDTO

toPlantDTO ∷ PlantDesignations → HM.HashMap (Int, Int) PlantDesignationDTO
toPlantDTO = HM.map toPlantDesignationDTO
fromPlantDTO ∷ HM.HashMap (Int, Int) PlantDesignationDTO → PlantDesignations
fromPlantDTO = HM.map fromPlantDesignationDTO

toCropDTO ∷ CropPlots → HM.HashMap (Int, Int) CropPlotDTO
toCropDTO = HM.map toCropPlotDTO
fromCropDTO ∷ HM.HashMap (Int, Int) CropPlotDTO → CropPlots
fromCropDTO = HM.map fromCropPlotDTO

toSpoilDTO ∷ SpoilPiles → HM.HashMap (Int, Int) SpoilPileDTO
toSpoilDTO = HM.map toSpoilPileDTO
fromSpoilDTO ∷ HM.HashMap (Int, Int) SpoilPileDTO → SpoilPiles
fromSpoilDTO = HM.map fromSpoilPileDTO

toEditsDTO ∷ WorldEdits → HM.HashMap ChunkCoord [WorldEditDTO]
toEditsDTO = HM.map (map toWorldEditDTO)
fromEditsDTO ∷ HM.HashMap ChunkCoord [WorldEditDTO] → WorldEdits
fromEditsDTO = HM.map (map fromWorldEditDTO)

-- world-pages -------------------------------------------------------

-- | One page's identity / clock / camera core. All evolving records are
--   frozen DTOs; 'ZoomMapMode' is a payload-free append-only leaf enum.
data PageCoreDTO = PageCoreDTO
    { pcPageId      ∷ !WorldPageId
    , pcGenParams   ∷ !WorldGenParamsDTO
    , pcCameraX     ∷ !Float
    , pcCameraY     ∷ !Float
    , pcTimeHour    ∷ !Int
    , pcTimeMinute  ∷ !Int
    , pcDateYear    ∷ !Int
    , pcDateMonth   ∷ !Int
    , pcDateDay     ∷ !Int
    , pcMapMode     ∷ !ZoomMapMode
    , pcIdentity    ∷ !(Maybe WorldIdentityDTO)
    } deriving (Show, Generic, Serialize)

newtype WorldPagesDTO = WorldPagesDTO { wpdPages ∷ [PageCoreDTO] }
    deriving stock (Generic)
    deriving newtype (Show, Serialize)

worldPagesCodec ∷ ComponentCodec WorldPagesDTO
worldPagesCodec = serializeCodec
    worldPagesComponentId 1 True []
    encodePages (\_ d → Right d) validatePages
  where
    encodePages snap = WorldPagesDTO (map toPageCore (orderedPages snap))
    toPageCore p = PageCoreDTO
        { pcPageId     = pgsPageId p
        , pcGenParams  = toWorldGenParamsDTO (pgsGenParams p)
        , pcCameraX    = pgsCameraX p
        , pcCameraY    = pgsCameraY p
        , pcTimeHour   = pgsTimeHour p
        , pcTimeMinute = pgsTimeMinute p
        , pcDateYear   = pgsDateYear p
        , pcDateMonth  = pgsDateMonth p
        , pcDateDay    = pgsDateDay p
        , pcMapMode    = pgsMapMode p
        , pcIdentity   = toWorldIdentityDTO <$> pgsIdentity p
        }
    -- Component-local invariant (requirement 3): the page-set authority
    -- must not itself carry a duplicate or empty page set.
    validatePages (WorldPagesDTO ps)
        | null ps = [ ComponentError worldPagesComponentId 1 ValidatePhase
                        "no world pages in save" ]
        | otherwise =
            [ ComponentError worldPagesComponentId 1 ValidatePhase
                ("duplicate page id " <> tshow pid)
            | (pid, n) ← HM.toList
                          (HM.fromListWith (+) [ (pcPageId p, 1 ∷ Int) | p ← ps ])
            , n > 1 ]

-- | Turn the decoded page cores into the base 'PageSnapshot' map every
--   other page-scoped component then writes onto (assembly). All entity/
--   activity/edit fields start empty and are overwritten by their own
--   REQUIRED components; a valid save leaves none of these placeholders.
basePageSnapshots ∷ WorldPagesDTO → HM.HashMap WorldPageId PageSnapshot
basePageSnapshots (WorldPagesDTO ps) =
    HM.fromList [ (pcPageId p, toBase p) | p ← ps ]
  where
    toBase p = PageSnapshot
        { pgsPageId       = pcPageId p
        , pgsGenParams    = fromWorldGenParamsDTO (pcGenParams p)
        , pgsCameraX      = pcCameraX p
        , pgsCameraY      = pcCameraY p
        , pgsTimeHour     = pcTimeHour p
        , pgsTimeMinute   = pcTimeMinute p
        , pgsDateYear     = pcDateYear p
        , pgsDateMonth    = pcDateMonth p
        , pgsDateDay      = pcDateDay p
        , pgsMapMode      = pcMapMode p
        , pgsIdentity     = fromWorldIdentityDTO <$> pcIdentity p
        , pgsEdits        = emptyWorldEdits
        , pgsMineDesignations      = HM.empty
        , pgsConstructDesignations = HM.empty
        , pgsGroundItems  = emptyGroundItems
        , pgsSpoilPiles   = emptySpoilPiles
        , pgsBuildings    = BuildingSnapshot { bsnInstances = HM.empty, bsnNextId = 0 }
        , pgsUnits        = UnitSnapshot { usnInstances = HM.empty, usnNextId = 0 }
        , pgsUnitSimStates = HM.empty
        , pgsFloraHarvests = emptyFloraHarvests
        , pgsChopDesignations = HM.empty
        , pgsCraftBills   = emptyCraftBills
        , pgsPowerNodes   = emptyPowerNodes
        , pgsTillDesignations = HM.empty
        , pgsCropPlots    = emptyCropPlots
        , pgsPlantDesignations = HM.empty
        }

-- world-edits -------------------------------------------------------

data PageEditsDTO = PageEditsDTO
    { pedPageId ∷ !WorldPageId
    , pedEdits  ∷ !(HM.HashMap ChunkCoord [WorldEditDTO])
    } deriving (Show, Generic, Serialize)

newtype WorldEditsDTO = WorldEditsDTO { wedPages ∷ [PageEditsDTO] }
    deriving stock (Generic)
    deriving newtype (Show, Serialize)

worldEditsCodec ∷ ComponentCodec WorldEditsDTO
worldEditsCodec = serializeCodec
    worldEditsComponentId 1 True [worldPagesComponentId]
    (\snap → WorldEditsDTO
        [ PageEditsDTO (pgsPageId p) (toEditsDTO (pgsEdits p))
        | p ← orderedPages snap ])
    (\_ d → Right d) (const [])

applyWorldEdits
    ∷ Word32 → WorldEditsDTO → HM.HashMap WorldPageId PageSnapshot
    → Either [ComponentError] (HM.HashMap WorldPageId PageSnapshot)
applyWorldEdits ver (WorldEditsDTO slices) =
    applyPageSlices worldEditsComponentId ver pedPageId
        (\s p → p { pgsEdits = fromEditsDTO (pedEdits s) }) slices

-- world-activity ----------------------------------------------------

data PageActivityDTO = PageActivityDTO
    { padPageId        ∷ !WorldPageId
    , padMine          ∷ !(HM.HashMap (Int, Int) MineDesignationDTO)
    , padConstruct     ∷ !(HM.HashMap (Int, Int) ConstructDesignationDTO)
    , padChop          ∷ !(HM.HashMap (Int, Int) ChopDesignationDTO)
    , padTill          ∷ !(HM.HashMap (Int, Int) TillDesignationDTO)
    , padPlant         ∷ !(HM.HashMap (Int, Int) PlantDesignationDTO)
    , padFloraHarvests ∷ !FloraHarvests
    , padCropPlots     ∷ !(HM.HashMap (Int, Int) CropPlotDTO)
    , padGroundItems   ∷ !GroundItemsDTO
    , padSpoilPiles    ∷ !(HM.HashMap (Int, Int) SpoilPileDTO)
    } deriving (Show, Generic, Serialize)

newtype WorldActivityDTO = WorldActivityDTO { wadPages ∷ [PageActivityDTO] }
    deriving stock (Generic)
    deriving newtype (Show, Serialize)

worldActivityCodec ∷ ComponentCodec WorldActivityDTO
worldActivityCodec = serializeCodec
    worldActivityComponentId 1 True [worldPagesComponentId]
    (\snap → WorldActivityDTO (map toActivity (orderedPages snap)))
    (\_ d → Right d) (const [])
  where
    toActivity p = PageActivityDTO
        { padPageId        = pgsPageId p
        , padMine          = toMineDTO (pgsMineDesignations p)
        , padConstruct     = toConstructDTO (pgsConstructDesignations p)
        , padChop          = toChopDTO (pgsChopDesignations p)
        , padTill          = toTillDTO (pgsTillDesignations p)
        , padPlant         = toPlantDTO (pgsPlantDesignations p)
        , padFloraHarvests = pgsFloraHarvests p
        , padCropPlots     = toCropDTO (pgsCropPlots p)
        , padGroundItems   = toGroundItemsDTO (pgsGroundItems p)
        , padSpoilPiles    = toSpoilDTO (pgsSpoilPiles p)
        }

applyWorldActivity
    ∷ Word32 → WorldActivityDTO → HM.HashMap WorldPageId PageSnapshot
    → Either [ComponentError] (HM.HashMap WorldPageId PageSnapshot)
applyWorldActivity ver (WorldActivityDTO slices) =
    applyPageSlices worldActivityComponentId ver padPageId writeActivity slices
  where
    writeActivity s p = p
        { pgsMineDesignations      = fromMineDTO (padMine s)
        , pgsConstructDesignations = fromConstructDTO (padConstruct s)
        , pgsChopDesignations      = fromChopDTO (padChop s)
        , pgsTillDesignations      = fromTillDTO (padTill s)
        , pgsPlantDesignations     = fromPlantDTO (padPlant s)
        , pgsFloraHarvests         = padFloraHarvests s
        , pgsCropPlots             = fromCropDTO (padCropPlots s)
        , pgsGroundItems           = fromGroundItemsDTO (padGroundItems s)
        , pgsSpoilPiles            = fromSpoilDTO (padSpoilPiles s)
        }
