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
--   - 'WorldGenParams'      → 'WorldGenParamsDTO' (with its nested live
--                             config/state records frozen recursively —
--                             see "World.Save.Component.WorldGen")
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
--   - 'GroundItem'/'GroundItems' → 'GroundItemDTO'/'GroundItemsDTO', its
--                             nested 'ItemInstance' frozen recursively via
--                             'ItemInstanceDTO'
--   - 'SpoilPile'           → 'SpoilPileDTO'
--
--   'WorldGenParamsDTO' and its full nested worldgen config/state tree
--   live in "World.Save.Component.WorldGen" (imported + re-exported here);
--   'ItemInstanceDTO' is defined below beside 'GroundItemDTO'.
--
--   A field/constructor added, dropped, or reordered on any of those
--   live records surfaces here (or in "…WorldGen") as a compile error in
--   its @from…@ conversion, never as silent byte drift in a shipped v1
--   save. Per the component frozen-DTO boundary rule (stated in
--   "World.Save.Component.Types"), genuine LEAF references are reused
--   as-is rather than mirrored: the payload-free append-only enums
--   ('ZoomMapMode', 'ConstructStatus'), and the durable coordinate/id/
--   content references ('ChunkCoord', 'FluidType', 'MaterialId',
--   'FloraId'), plus a bare 'Float' regrowth timer ('FloraHarvests',
--   which has no record at all to freeze). The DTO field order is chosen
--   so the derived cereal layout is byte-identical to the previous direct
--   embedding — the frozen tracked fixture stays valid.
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
    , ItemInstanceDTO(..)
    , GroundItemDTO(..)
    , GroundItemsDTO(..)
    , SpoilPileDTO(..)
    , toWorldGenParamsDTO
    , fromWorldGenParamsDTO
    , toItemInstanceDTO
    , fromItemInstanceDTO
    , toGroundItemDTO
    , fromGroundItemDTO
    , basePageSnapshots
    , applyWorldEdits
    , applyWorldActivity
    , validatePages
    , validateWorldActivity
    ) where

import UPrelude
import qualified Data.HashMap.Strict as HM
import qualified Data.List as L
import qualified Data.Text as T
import Data.Serialize (Serialize)
import GHC.Generics (Generic)
import Craft.Bills (emptyCraftBills)
import Power.Types (emptyPowerNodes)
import World.Save.Component.WorldGen
    (WorldGenParamsDTO(..), toWorldGenParamsDTO, fromWorldGenParamsDTO)
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
import Item.Types (ItemInstance(..))
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

-- | Frozen mirror of 'ItemInstance' (a mutable runtime record whose
--   fields — fill / quality / condition / sharpness / temperature — are
--   live gameplay state, appended to across saves v36/v42/v56/v68). Per
--   the component frozen-DTO boundary rule ("World.Save.Component.Types"),
--   this live record is frozen with an explicit field-by-field conversion
--   rather than embedded — and it is frozen RECURSIVELY: 'iiContents' is
--   itself a @['ItemInstance']@ (a first-aid kit holds items, a kit can
--   hold a kit), so 'itdContents' recurses through 'ItemInstanceDTO' too;
--   a shallow wrapper re-embedding the live nested list would still drift.
--   Every other field is a leaf scalar/'Maybe' scalar. Field order mirrors
--   'ItemInstance''s positional 'Generic Serialize' layout exactly, so the
--   bytes are unchanged from embedding the live type directly.
data ItemInstanceDTO = ItemInstanceDTO
    { itdDefName     ∷ !Text
    , itdCurrentFill ∷ !Float
    , itdQuality     ∷ !Float
    , itdCondition   ∷ !Float
    , itdWeight      ∷ !Float
    , itdSharpness   ∷ !Float
    , itdContents    ∷ ![ItemInstanceDTO]
    , itdInstanceId  ∷ !Word64
    , itdTemp        ∷ !(Maybe Float)
    } deriving (Show, Eq, Generic, Serialize)

toItemInstanceDTO ∷ ItemInstance → ItemInstanceDTO
toItemInstanceDTO i = ItemInstanceDTO
    { itdDefName     = iiDefName i
    , itdCurrentFill = iiCurrentFill i
    , itdQuality     = iiQuality i
    , itdCondition   = iiCondition i
    , itdWeight      = iiWeight i
    , itdSharpness   = iiSharpness i
    , itdContents    = map toItemInstanceDTO (iiContents i)
    , itdInstanceId  = iiInstanceId i
    , itdTemp        = iiTemp i
    }

fromItemInstanceDTO ∷ ItemInstanceDTO → ItemInstance
fromItemInstanceDTO d = ItemInstance
    { iiDefName     = itdDefName d
    , iiCurrentFill = itdCurrentFill d
    , iiQuality     = itdQuality d
    , iiCondition   = itdCondition d
    , iiWeight      = itdWeight d
    , iiSharpness   = itdSharpness d
    , iiContents    = map fromItemInstanceDTO (itdContents d)
    , iiInstanceId  = itdInstanceId d
    , iiTemp        = itdTemp d
    }

-- | Frozen mirror of 'GroundItem'. Its 'ItemInstance' recurses through
--   'ItemInstanceDTO' above.
data GroundItemDTO = GroundItemDTO
    { giiInst ∷ !ItemInstanceDTO
    , giiX    ∷ !Float
    , giiY    ∷ !Float
    } deriving (Show, Eq, Generic, Serialize)

toGroundItemDTO ∷ GroundItem → GroundItemDTO
toGroundItemDTO g = GroundItemDTO (toItemInstanceDTO (giInst g)) (giX g) (giY g)

fromGroundItemDTO ∷ GroundItemDTO → GroundItem
fromGroundItemDTO d = GroundItem (fromItemInstanceDTO (giiInst d)) (giiX d) (giiY d)

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

-- | Component-local invariant (requirement 3): the page-set authority
--   must not itself carry a duplicate or empty page set. Hoisted to top
--   level (round-14 review) so "World.Save.Compat.SessionV90"'s B1
--   migration path can run the SAME validator a modern envelope's
--   decode always does, rather than skip it entirely.
validatePages ∷ WorldPagesDTO → [ComponentError]
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

-- | Component-local invariant (#760 round 8, mirrors
--   @worldPagesCodec@'s @validatePages@ precedent above): every ground
--   item's own id must sit below that page's ground-items allocator
--   ('gisiNextId') — 'Item.Ground.GroundItems' ids are allocated
--   per-page (see 'Item.Ground.emptyGroundItems'). A literal duplicate
--   key within one page's @gisiItems@ map is structurally impossible
--   once decoded (a 'HashMap' cannot carry two entries under the same
--   key), so there is nothing further to check there.
validateWorldActivity ∷ WorldActivityDTO → [ComponentError]
validateWorldActivity (WorldActivityDTO slices) =
    [ ComponentError worldActivityComponentId 1 ValidatePhase
        ("page '" <> tshow (padPageId s) <> "': ground item #"
         <> tshow gid <> " is not below the page's ground-item \
            \allocator (" <> tshow (gisiNextId (padGroundItems s)) <> ")")
    | s   ← slices
    , gid ← HM.keys (gisiItems (padGroundItems s))
    , gid ≥ gisiNextId (padGroundItems s)
    ]

worldActivityCodec ∷ ComponentCodec WorldActivityDTO
worldActivityCodec = serializeCodec
    worldActivityComponentId 1 True [worldPagesComponentId]
    (\snap → WorldActivityDTO (map toActivity (orderedPages snap)))
    (\_ d → Right d) validateWorldActivity
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
