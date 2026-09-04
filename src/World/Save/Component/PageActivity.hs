{-# LANGUAGE Strict, DeriveGeneric, DeriveAnyClass, DerivingStrategies #-}
-- | The @"world-activity"@ owner (issue #760, save-overhaul B2; split
--   out of "World.Save.Component.Page" by #2135).
--
--   @"world-activity"@ (required) — per page: designations (mine/
--   construct/chop/till/plant), flora harvests, crop plots, ground
--   items, and spoil piles. Owner: the mutable-world-activity layer.
--   Boundary reason: requirement 2 bullet 4's "designations, jobs,
--   progress, flora, crops, ground items, spoil" — the transient-ish
--   but persisted world activity, grouped away from the terrain spine
--   and the entity managers. Its slice list is encoded in the canonical
--   (page-id ascending) order
--   'World.Save.Component.PageCore.orderedPages' establishes, so
--   identical input produces identical bytes (requirement 10).
--
--   The only cross-owner import is that ordering helper. This owner
--   never imports "World.Save.Component.PageEdits" — its sibling — nor
--   the "World.Save.Component.Page" façade; the shared apply
--   scaffolding both siblings run their slices through
--   ('World.Save.Component.Types.applyPageSlices') already lives one
--   level down, in the module both already depend on.
--
--   Requirement 4 — the on-disk contract is FROZEN, distinct from every
--   mutable runtime record; see "World.Save.Component.Page" for the
--   page-scoped statement of that rule. The live records this owner
--   mirrors are:
--
--   - 'MineDesignation'     → 'MineDesignationDTO'
--   - 'ConstructDesignation'→ 'ConstructDesignationDTO'
--                             ('StructurePieceDTO'/'ConstructTargetDTO')
--   - 'ChopDesignation'     → 'ChopDesignationDTO'
--   - 'TillDesignation'     → 'TillDesignationDTO'
--   - 'PlantDesignation'    → 'PlantDesignationDTO'
--   - 'CropPlot'            → 'CropPlotDTO'
--   - 'GroundItem'/'GroundItems' → 'GroundItemDTO'/'GroundItemsDTO', its
--                             nested 'ItemInstance' frozen recursively via
--                             'ItemInstanceDTO' (whose optional storage
--                             capacities are frozen as 'ItemStorageDTO';
--                             the pre-#1233 tree stays as
--                             'ItemInstanceDTOv1'/'GroundItemDTOv1'/
--                             'GroundItemsDTOv1', reached by
--                             @world-activity@ v1+v2 through the frozen
--                             'PageActivityDTOv2'/'WorldActivityDTOv2'
--                             slice, and by three other components plus
--                             the v90 legacy tree)
--   - 'SpoilPile'           → 'SpoilPileDTO'
--
--   Per the frozen-DTO boundary rule (stated in
--   "World.Save.Component.Types"), genuine LEAF references are reused
--   as-is rather than mirrored: the payload-free append-only enum
--   'ConstructStatus', and the durable coordinate/id/content references
--   ('MaterialId', 'FloraId', the authored 'World.Flora.Reference.FloraRef',
--   and — since #1854 — the opaque 'FloraInstanceId' the Chop/harvest
--   maps are keyed by). A regrowth timer is a bare 'Float' with no
--   record at all to freeze, but its MAP is not a leaf: #1854 re-keyed
--   the live 'World.Flora.Harvest.FloraHarvests' alias, so every frozen
--   slice names 'FloraHarvestsDTOv1' — the tile-keyed shape those
--   payloads were written with — explicitly.
module World.Save.Component.PageActivity
    ( -- * Frozen designation DTOs
      MineDesignationDTO(..)
    , StructurePieceDTO(..)
    , ConstructTargetDTO(..)
    , ConstructDesignationDTO(..)
    , ConstructDesignationDTOv1(..)
    , toConstructDTO
    , fromConstructDTO
    , migrateConstructDesignations
    , ChopDesignationDTO(..)
    , ChopDesignationDTOv1(..)
    , FloraHarvestsDTOv1
    , TillDesignationDTO(..)
    , PlantDesignationDTO(..)
    , PlantDesignationDTOv1(..)
    , CropPlotDTO(..)
    , CropPlotDTOv1(..)
      -- * Frozen item / ground-item / spoil DTOs
    , ItemStorageDTO(..)
    , ItemInstanceDTO(..)
    , ItemInstanceDTOv1(..)
    , GroundItemDTO(..)
    , GroundItemDTOv1(..)
    , GroundItemsDTO(..)
    , GroundItemsDTOv1(..)
    , SpoilPileDTO(..)
    , toItemInstanceDTO
    , fromItemInstanceDTO
    , toItemInstanceDTOv1
    , migrateItemInstanceDTOv1
    , toGroundItemDTO
    , fromGroundItemDTO
    , toGroundItemsDTO
    , toGroundItemsDTOv1
      -- * The @"world-activity"@ wire shapes
    , PageActivityDTO(..)
    , WorldActivityDTO(..)
    , PageActivityDTOv2(..)
    , WorldActivityDTOv2(..)
    , PageActivityDTOv3(..)
    , WorldActivityDTOv3(..)
    , PageActivityDTOv4(..)
    , WorldActivityDTOv4(..)
    , PageActivityDTOv5(..)
    , WorldActivityDTOv5(..)
      -- * The component
    , worldActivityCodec
    , validateWorldActivity
    , applyWorldActivity
    , migrateWorldActivityV2
    ) where

import UPrelude
import qualified Data.HashMap.Strict as HM
import qualified Data.List as L
import Data.Serialize (Serialize)
import GHC.Generics (Generic)
import World.Generate.Types (WorldGenParams(..))
import World.Generate.Coordinates (canonicalTile)
import World.Page.Types (WorldPageId)
import World.Material.Id (MaterialId)
import World.Flora.Types (FloraId)
import World.Flora.Reference (FloraRef(..))
import World.Mine.Types (MineDesignation(..), MineDesignations)
import World.Construct.Types
    ( ConstructDesignation(..), ConstructTarget(..), StructurePiece(..)
    , ConstructStatus, ConstructDesignations )
import World.Chop.Types (ChopDesignation(..), ChopDesignations)
import World.Construct.Attempt
    ( ConstructAttemptId, advanceConstructAttemptsPast
    , firstConstructAttemptId, takeConstructAttempts )
import World.Construct.Receipt (ConstructPayment(..))
import World.Flora.Identity (FloraInstanceId)
import World.Till.Types (TillDesignation(..), TillDesignations)
import World.Plant.Types
    ( PlantDesignationOf(..), SavedPlantDesignation
    , SavedPlantDesignations )
import World.Spoil.Types (SpoilPile(..), SpoilPiles)
import World.Flora.CropPlot
    ( CropPlotOf(..), SavedCropPlot, SavedCropPlots )
import Item.Ground (GroundItem(..), GroundItems(..), emptyGroundItems)
import Item.Types (ItemInstance(..), ItemStorage(..))
import World.Save.Snapshot (PageSnapshot(..))
import World.Save.Component.PageCore (orderedPages)
import World.Save.Component.Types

-- | Frozen mirror of 'MineDesignation'.
--
--   __Non-finite legacy progress is repaired on the way in, not
--   refused (#2338).__ This DTO round-trips 'mdiCorners' and
--   'mdiChunkProgress' verbatim, and before @world.digTile@ and the
--   @WorldDigTile@ handler judged their arguments a NaN or infinite
--   value could reach them and be written here. Such a save is still
--   perfectly readable, so the wire shape deliberately does NOT reject
--   it — decoding stays total and the component version is unchanged.
--   @World.Load.Stage.repairStagedMineDesignations@ is the single place
--   the policy is applied: at staging, every corner of an offending
--   designation is reset to 1.0 (undug) and its chunk progress to 0,
--   'mdiZ' and the tile key are kept, one warning names the page and
--   the tile, and the load proceeds. A designation whose numbers are
--   all finite is untouched.
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

-- | The FROZEN pre-#1844 designation mirror — @world-activity@ v1, v2,
--   v3 AND v4 all encode this five-field shape.
--
--   Kept verbatim rather than extended in place (requirement 22): its
--   bytes are what every shipped save holds, and the sixth field the
--   live encoding appends must not change how those are read.
data ConstructDesignationDTOv1 = ConstructDesignationDTOv1
    { cdi1Z             ∷ !Int
    , cdi1Target        ∷ !ConstructTargetDTO
    , cdi1Status        ∷ !ConstructStatus
    , cdi1Progress      ∷ !Float
    , cdi1MaterialsPaid ∷ !Bool
    } deriving (Show, Eq, Generic, Serialize)

-- | Frozen mirror of 'ConstructDesignation'. 'ConstructStatus' is a
--   payload-free append-only enum, reused as a leaf (see the module
--   haddock) exactly like 'Pose'/'Direction' in Entities;
--   'ConstructPayment' is the same arrangement, and 'ConstructAttemptId'
--   is a newtype over 'Word64'.
data ConstructDesignationDTO = ConstructDesignationDTO
    { cdiZ        ∷ !Int
    , cdiTarget   ∷ !ConstructTargetDTO
    , cdiStatus   ∷ !ConstructStatus
    , cdiProgress ∷ !Float
    , cdiAttempt  ∷ !ConstructAttemptId
    , cdiPayment  ∷ !ConstructPayment
    } deriving (Show, Eq, Generic, Serialize)

toConstructDesignationDTO ∷ ConstructDesignation → ConstructDesignationDTO
toConstructDesignationDTO c = ConstructDesignationDTO
    { cdiZ        = cdZ c
    , cdiTarget   = toConstructTargetDTO (cdTarget c)
    , cdiStatus   = cdStatus c
    , cdiProgress = cdProgress c
    , cdiAttempt  = cdAttempt c
    , cdiPayment  = cdPayment c
    }

fromConstructDesignationDTO ∷ ConstructDesignationDTO → ConstructDesignation
fromConstructDesignationDTO d = ConstructDesignation
    { cdZ        = cdiZ d
    , cdTarget   = fromConstructTargetDTO (cdiTarget d)
    , cdStatus   = cdiStatus d
    , cdProgress = cdiProgress d
    , cdAttempt  = cdiAttempt d
    , cdPayment  = cdiPayment d
    }

-- | Legacy designations → current shape, for ONE page (requirement 12).
--
--   A pre-#1844 payload records no attempt identity at all, so one is
--   assigned to every entry here, and the rules are chosen so the result
--   is the same on every machine and every run:
--
--     * Ids go out in ASCENDING TILE-KEY order, starting at
--       'firstConstructAttemptId'. Hash order would make a migrated save
--       differ run to run, and re-saving it would then produce different
--       bytes for the same input.
--     * The page's allocator lands one past the highest id issued, so no
--       later designation can collide with a migrated one.
--     * @cdMaterialsPaid = False@ becomes 'CpUnpaid' — nothing was
--       removed, so there is nothing to record. @True@ becomes
--       'CpLegacyPaid': materials really did leave someone's inventory,
--       but WHICH is unrecoverable from the payload, so the receipt is
--       reconstructed from the currently registered build metadata
--       during load STAGING — or the load is rejected there, because
--       inventing a refund and losing the materials are both wrong.
migrateConstructDesignations
    ∷ HM.HashMap (Int, Int) ConstructDesignationDTOv1
    → (HM.HashMap (Int, Int) ConstructDesignationDTO, ConstructAttemptId)
migrateConstructDesignations legacy =
    let entries = L.sortOn fst (HM.toList legacy)
        (ids, next) = takeConstructAttempts (length entries)
                                            firstConstructAttemptId
    in ( HM.fromList
            [ (k, ConstructDesignationDTO
                    { cdiZ        = cdi1Z d
                    , cdiTarget   = cdi1Target d
                    , cdiStatus   = cdi1Status d
                    , cdiProgress = cdi1Progress d
                    , cdiAttempt  = aid
                    , cdiPayment  = if cdi1MaterialsPaid d
                                      then CpLegacyPaid else CpUnpaid
                    })
            | ((k, d), aid) ← zip entries ids ]
       , next )

-- | The FROZEN pre-#1854 chop designation: a bare surface z, because
--   the map that held it was keyed by TILE. Referenced by the frozen
--   @world-activity@ v1/v2/v3 slices and by
--   "World.Save.Compat.SessionV90"'s v90 page save, and still the wire
--   shape of the PENDING legacy entries v4 carries (they are exactly
--   these entries, waiting for an instance to attach to). Never
--   edited.
newtype ChopDesignationDTOv1 = ChopDesignationDTOv1 { chiZ ∷ Int }
    deriving stock (Generic)
    deriving newtype (Show, Eq)
    deriving anyclass (Serialize)

-- | Frozen mirror of the CURRENT 'ChopDesignation' (#1854): the same
--   surface z, plus the canonical tile the designated plant stands on.
--   The map is keyed by 'FloraInstanceId' now, so the tile has to
--   travel with the record — a marker and a nearest-designation scan
--   have nowhere else to read it from.
data ChopDesignationDTO = ChopDesignationDTO
    { chiZ2  ∷ !Int
    , chiGX2 ∷ !Int
    , chiGY2 ∷ !Int
    } deriving (Show, Eq, Generic, Serialize)

toChopDesignationDTO ∷ ChopDesignation → ChopDesignationDTO
toChopDesignationDTO cd = ChopDesignationDTO (chZ cd) (chGX cd) (chGY cd)

fromChopDesignationDTO ∷ ChopDesignationDTO → ChopDesignation
fromChopDesignationDTO d = ChopDesignation (chiZ2 d) (chiGX2 d) (chiGY2 d)

-- | A pre-#1854 tile-keyed designation, kept in its own tile-keyed
--   PENDING map until the chunk that resolves it arrives.
toPendingChopDesignationDTO ∷ ChopDesignation → ChopDesignationDTOv1
toPendingChopDesignationDTO = ChopDesignationDTOv1 . chZ

-- | The reverse. The tile comes from the map KEY, so it is written back
--   into the record here — a pending entry is a real 'ChopDesignation'
--   that simply has no instance yet.
fromPendingChopDesignationDTO
    ∷ (Int, Int) → ChopDesignationDTOv1 → ChopDesignation
fromPendingChopDesignationDTO (gx, gy) d = ChopDesignation (chiZ d) gx gy

-- | The FROZEN pre-#1854 regrowth-timer map: TILE → remaining
--   game-seconds. Spelled out here rather than named through the live
--   'World.Flora.Harvest.FloraHarvests' alias, which #1854 re-pointed at
--   'FloraInstanceId': a frozen DTO that referred through the live alias
--   would have silently rewritten every shipped v1/v2/v3 (and v90)
--   payload's decoding the moment that alias changed. It is also the
--   wire shape of v4's own PENDING legacy timers.
type FloraHarvestsDTOv1 = HM.HashMap (Int, Int) Float

-- | Frozen mirror of 'TillDesignation'.
newtype TillDesignationDTO = TillDesignationDTO { tliZ ∷ Int }
    deriving stock (Generic)
    deriving newtype (Show, Eq)
    deriving anyclass (Serialize)

toTillDesignationDTO ∷ TillDesignation → TillDesignationDTO
toTillDesignationDTO = TillDesignationDTO . tlZ

fromTillDesignationDTO ∷ TillDesignationDTO → TillDesignation
fromTillDesignationDTO = TillDesignation . tliZ

-- | Frozen mirror of 'World.Plant.Types.SavedPlantDesignation' — the
--   CURRENT (@world-activity@ v6) shape, whose crop is the durable
--   'FloraRef' #2243 persists rather than the runtime ordinal.
data PlantDesignationDTO = PlantDesignationDTO
    { ptiZ    ∷ !Int
    , ptiCrop ∷ !FloraRef
    } deriving (Show, Eq, Generic, Serialize)

-- | The FROZEN pre-#2243 shape (@world-activity@ v1–v5, and the v90
--   legacy tree), whose crop is a runtime 'FloraId'. Retyping the field
--   in place would have re-laid out every shipped activity payload —
--   a single-constructor record emits no tag, so nothing would have
--   moved visibly and everything would have decoded to garbage.
data PlantDesignationDTOv1 = PlantDesignationDTOv1
    { pti1Z    ∷ !Int
    , pti1Crop ∷ !FloraId
    } deriving (Show, Eq, Generic, Serialize)

toPlantDesignationDTO ∷ SavedPlantDesignation → PlantDesignationDTO
toPlantDesignationDTO p = PlantDesignationDTO (ptZ p) (ptCrop p)

fromPlantDesignationDTO ∷ PlantDesignationDTO → SavedPlantDesignation
fromPlantDesignationDTO d = PlantDesignation (ptiZ d) (ptiCrop d)

-- | v1 → current (#2243): the ordinal becomes a legacy reference, on
--   exactly the terms 'migrateWorldEditDTOv2' spells out.
migratePlantDesignationDTOv1 ∷ PlantDesignationDTOv1 → PlantDesignationDTO
migratePlantDesignationDTOv1 d =
    PlantDesignationDTO (pti1Z d) (FloraByLegacyId (pti1Crop d))

-- | Frozen mirror of 'World.Flora.CropPlot.SavedCropPlot' — the CURRENT
--   (@world-activity@ v6) shape. See 'PlantDesignationDTO' above.
data CropPlotDTO = CropPlotDTO
    { cpiSpecies    ∷ !FloraRef
    , cpiPlantedDay ∷ !Int
    , cpiHealth     ∷ !Float
    } deriving (Show, Eq, Generic, Serialize)

-- | The FROZEN pre-#2243 crop plot (@world-activity@ v1–v5, and the v90
--   legacy tree).
data CropPlotDTOv1 = CropPlotDTOv1
    { cpi1Species    ∷ !FloraId
    , cpi1PlantedDay ∷ !Int
    , cpi1Health     ∷ !Float
    } deriving (Show, Eq, Generic, Serialize)

toCropPlotDTO ∷ SavedCropPlot → CropPlotDTO
toCropPlotDTO c = CropPlotDTO (cpSpecies c) (cpPlantedDay c) (cpHealth c)

fromCropPlotDTO ∷ CropPlotDTO → SavedCropPlot
fromCropPlotDTO d = CropPlot (cpiSpecies d) (cpiPlantedDay d) (cpiHealth d)

-- | v1 → current (#2243).
migrateCropPlotDTOv1 ∷ CropPlotDTOv1 → CropPlotDTO
migrateCropPlotDTOv1 d = CropPlotDTO
    (FloraByLegacyId (cpi1Species d)) (cpi1PlantedDay d) (cpi1Health d)

-- | Frozen mirror of 'Item.Types.ItemStorage' — a portable item's
--   INTERNAL weight + bulk capacities (#1233). Frozen rather than reused
--   as a leaf under boundary-rule clause (2): it is an ordinary live
--   gameplay record that could plausibly gain fields (accepted kinds,
--   openings, rigidity) for reasons that have nothing to do with save
--   compatibility, and it rides inside the recursive item tree where such
--   drift would silently re-layout every item-bearing component's bytes.
data ItemStorageDTO = ItemStorageDTO
    { isdWeightCapacity ∷ !Float
    , isdBulkCapacity   ∷ !Float
    } deriving (Show, Eq, Generic, Serialize)

toItemStorageDTO ∷ ItemStorage → ItemStorageDTO
toItemStorageDTO s =
    ItemStorageDTO (isWeightCapacity s) (isBulkCapacity s)

fromItemStorageDTO ∷ ItemStorageDTO → ItemStorage
fromItemStorageDTO d =
    ItemStorage (isdWeightCapacity d) (isdBulkCapacity d)

-- | Frozen mirror of 'ItemInstance' (a mutable runtime record whose
--   fields — fill / quality / condition / sharpness / temperature — are
--   live gameplay state, appended to across saves v36/v42/v56/v68 and
--   #1233's physical values). Per the component frozen-DTO boundary rule
--   ("World.Save.Component.Types"),
--   this live record is frozen with an explicit field-by-field conversion
--   rather than embedded — and it is frozen RECURSIVELY: 'iiContents' is
--   itself a @['ItemInstance']@ (a first-aid kit holds items, a kit can
--   hold a kit), so 'itdContents' recurses through 'ItemInstanceDTO' too;
--   a shallow wrapper re-embedding the live nested list would still drift.
--   Every other field is a leaf scalar/'Maybe' scalar, except #1233's
--   optional storage capacities (frozen as 'ItemStorageDTO'). Field order
--   mirrors 'ItemInstance''s positional 'Generic Serialize' layout
--   exactly.
--
--   __This is the CURRENT (#1233) shape.__ The pre-#1233 one is frozen
--   verbatim as 'ItemInstanceDTOv1' below and is what every historical
--   item-bearing payload still decodes through; this type was NOT edited
--   in place, because four independently-versioned components
--   (@world-activity@, @buildings@, @units@, @container-knowledge@) plus
--   the v90 legacy tree all carry it, and editing it would have silently
--   re-laid-out every one of their shipped payloads.
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
    , itdBulk        ∷ !(Maybe Float)
    , itdStorage     ∷ !(Maybe ItemStorageDTO)
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
    , itdBulk        = iiBulk i
    , itdStorage     = toItemStorageDTO <$> iiStorage i
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
    , iiBulk        = itdBulk d
    , iiStorage     = fromItemStorageDTO <$> itdStorage d
    }

-- | The FROZEN pre-#1233 item shape, preserved verbatim for decode-only
--   backward compatibility: every field the current DTO has except the
--   physical values (#1233's external bulk + internal storage
--   capacities). Recursive through ITSELF, so a historical nested
--   @iiContents@ tree decodes at the old layout all the way down.
--
--   Reached by @world-activity@ v1/v2, @buildings@ v1, @units@ v1,
--   @container-knowledge@ v1, and "World.Save.Compat.SessionV90"'s v90
--   tree. Never edited; a further item schema change freezes the CURRENT
--   shape as 'ItemInstanceDTOv2' rather than touching this one
--   (frozen-DTO boundary rule).
data ItemInstanceDTOv1 = ItemInstanceDTOv1
    { itd1DefName     ∷ !Text
    , itd1CurrentFill ∷ !Float
    , itd1Quality     ∷ !Float
    , itd1Condition   ∷ !Float
    , itd1Weight      ∷ !Float
    , itd1Sharpness   ∷ !Float
    , itd1Contents    ∷ ![ItemInstanceDTOv1]
    , itd1InstanceId  ∷ !Word64
    , itd1Temp        ∷ !(Maybe Float)
    } deriving (Show, Eq, Generic, Serialize)

-- | Encoder for the frozen shape — the round-trip partner a frozen-DTO
--   fixture and a migration test are built with (the same reason
--   'toWorldIdentityDTOv2' exists).
toItemInstanceDTOv1 ∷ ItemInstance → ItemInstanceDTOv1
toItemInstanceDTOv1 i = ItemInstanceDTOv1
    { itd1DefName     = iiDefName i
    , itd1CurrentFill = iiCurrentFill i
    , itd1Quality     = iiQuality i
    , itd1Condition   = iiCondition i
    , itd1Weight      = iiWeight i
    , itd1Sharpness   = iiSharpness i
    , itd1Contents    = map toItemInstanceDTOv1 (iiContents i)
    , itd1InstanceId  = iiInstanceId i
    , itd1Temp        = iiTemp i
    }

-- | __The #1233 absence policy, stated once and applied everywhere.__
--
--   A pre-#1233 item decodes with its physical values ABSENT
--   ('Nothing'), recursively through its whole contents tree. That is a
--   deliberate choice between three options, and the other two are both
--   wrong:
--
--     * Fabricating @Just 0@ would write an INVALID bulk (the loader
--       rejects a zero) into the session and then to disk, where nothing
--       downstream could tell it apart from an authored value.
--     * Re-deriving it from the item's CURRENT definition would be
--       exactly the retroactive reinterpretation #1233 requirement 6
--       forbids: which build first loaded the save would decide what a
--       long-materialized crate is worth, and two players on different
--       content versions would get different answers for the same bytes.
--
--   Representing the absence instead keeps it honest AND non-silent —
--   every reader has to destructure a 'Maybe', so nothing can quietly
--   fall back to a definition that has since been edited. Nothing in
--   this slice consumes either value (#1233 requirement 8 is data only),
--   so PLC-4 (the epic's former PLC-3B) — the first slice that
--   ENFORCES a capacity — is what
--   decides how an absent value behaves under enforcement. A migrated
--   item that is re-saved keeps the absence verbatim: it is a fact about
--   that item's history, not a placeholder waiting to be filled.
migrateItemInstanceDTOv1 ∷ ItemInstanceDTOv1 → ItemInstanceDTO
migrateItemInstanceDTOv1 d = ItemInstanceDTO
    { itdDefName     = itd1DefName d
    , itdCurrentFill = itd1CurrentFill d
    , itdQuality     = itd1Quality d
    , itdCondition   = itd1Condition d
    , itdWeight      = itd1Weight d
    , itdSharpness   = itd1Sharpness d
    , itdContents    = map migrateItemInstanceDTOv1 (itd1Contents d)
    , itdInstanceId  = itd1InstanceId d
    , itdTemp        = itd1Temp d
    , itdBulk        = Nothing
    , itdStorage     = Nothing
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

-- | The FROZEN pre-#1233 ground item (#1233): identical but for the item
--   shape it carries.
data GroundItemDTOv1 = GroundItemDTOv1
    { gii1Inst ∷ !ItemInstanceDTOv1
    , gii1X    ∷ !Float
    , gii1Y    ∷ !Float
    } deriving (Show, Eq, Generic, Serialize)

toGroundItemDTOv1 ∷ GroundItem → GroundItemDTOv1
toGroundItemDTOv1 g =
    GroundItemDTOv1 (toItemInstanceDTOv1 (giInst g)) (giX g) (giY g)

migrateGroundItemDTOv1 ∷ GroundItemDTOv1 → GroundItemDTO
migrateGroundItemDTOv1 d = GroundItemDTO
    (migrateItemInstanceDTOv1 (gii1Inst d)) (gii1X d) (gii1Y d)

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

-- | The FROZEN pre-#1233 ground-items registry (#1233).
data GroundItemsDTOv1 = GroundItemsDTOv1
    { gisi1NextId ∷ !Int
    , gisi1Items  ∷ !(HM.HashMap Int GroundItemDTOv1)
    } deriving (Show, Eq, Generic, Serialize)

toGroundItemsDTOv1 ∷ GroundItems → GroundItemsDTOv1
toGroundItemsDTOv1 g =
    GroundItemsDTOv1 (gisNextId g) (HM.map toGroundItemDTOv1 (gisItems g))

migrateGroundItemsDTOv1 ∷ GroundItemsDTOv1 → GroundItemsDTO
migrateGroundItemsDTOv1 d =
    GroundItemsDTO (gisi1NextId d) (HM.map migrateGroundItemDTOv1 (gisi1Items d))

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

toChopDTO ∷ ChopDesignations → HM.HashMap FloraInstanceId ChopDesignationDTO
toChopDTO = HM.map toChopDesignationDTO
fromChopDTO ∷ HM.HashMap FloraInstanceId ChopDesignationDTO → ChopDesignations
fromChopDTO = HM.map fromChopDesignationDTO

toTillDTO ∷ TillDesignations → HM.HashMap (Int, Int) TillDesignationDTO
toTillDTO = HM.map toTillDesignationDTO
fromTillDTO ∷ HM.HashMap (Int, Int) TillDesignationDTO → TillDesignations
fromTillDTO = HM.map fromTillDesignationDTO

toPlantDTO ∷ SavedPlantDesignations
           → HM.HashMap (Int, Int) PlantDesignationDTO
toPlantDTO = HM.map toPlantDesignationDTO
fromPlantDTO ∷ HM.HashMap (Int, Int) PlantDesignationDTO
             → SavedPlantDesignations
fromPlantDTO = HM.map fromPlantDesignationDTO

toCropDTO ∷ SavedCropPlots → HM.HashMap (Int, Int) CropPlotDTO
toCropDTO = HM.map toCropPlotDTO
fromCropDTO ∷ HM.HashMap (Int, Int) CropPlotDTO → SavedCropPlots
fromCropDTO = HM.map fromCropPlotDTO

toSpoilDTO ∷ SpoilPiles → HM.HashMap (Int, Int) SpoilPileDTO
toSpoilDTO = HM.map toSpoilPileDTO
fromSpoilDTO ∷ HM.HashMap (Int, Int) SpoilPileDTO → SpoilPiles
fromSpoilDTO = HM.map fromSpoilPileDTO

-- world-activity ----------------------------------------------------

data PageActivityDTO = PageActivityDTO
    { padPageId        ∷ !WorldPageId
    , padMine          ∷ !(HM.HashMap (Int, Int) MineDesignationDTO)
    , padConstruct     ∷ !(HM.HashMap (Int, Int) ConstructDesignationDTO)
      -- ^ #1844: each entry now carries its own attempt identity and its
      --   payment record, so the five-field shape every earlier version
      --   wrote is frozen as 'ConstructDesignationDTOv1'.
    , padChop          ∷ !(HM.HashMap FloraInstanceId ChopDesignationDTO)
      -- ^ #1854: keyed by the designated PLANT, not by its tile.
    , padTill          ∷ !(HM.HashMap (Int, Int) TillDesignationDTO)
    , padPlant         ∷ !(HM.HashMap (Int, Int) PlantDesignationDTO)
    , padFloraHarvests ∷ !(HM.HashMap FloraInstanceId Float)
      -- ^ #1854: likewise keyed by the harvested plant. Spelled out
      --   rather than named through 'World.Flora.Harvest.FloraHarvests'
      --   for symmetry with the frozen slices below, which must NOT
      --   follow that alias anywhere.
    , padCropPlots     ∷ !(HM.HashMap (Int, Int) CropPlotDTO)
      -- ^ Crop plots stay TILE-keyed and are untouched by #1854: a plot
      --   is one-per-tile by construction and never coexists with wild
      --   'FloraInstance's on the same tile (tilled soil excludes
      --   natural flora placement), so it has no co-tenancy to
      --   disambiguate.
    , padGroundItems   ∷ !GroundItemsDTO
    , padSpoilPiles    ∷ !(HM.HashMap (Int, Int) SpoilPileDTO)
    , padPendingChop   ∷ !(HM.HashMap (Int, Int) ChopDesignationDTOv1)
      -- ^ #1854: pre-identity designations whose chunk was not resident
      --   when the save was read, so no instance could be resolved.
      --   Carried (rather than dropped) so a second save/load cannot
      --   silently discard a designation the player made. Never
      --   authoritative — see "World.Chop.Types".
    , padPendingHarvests ∷ !FloraHarvestsDTOv1
      -- ^ #1854: pre-identity regrowth timers on the same terms.
    , padConstructNextAttempt ∷ !ConstructAttemptId
      -- ^ #1844: the page's OWN construct-attempt allocator. Persisted
      --   beside the designations it hands ids to, because an attempt id
      --   is only durable if the cursor that will not reissue it is
      --   durable too.
    } deriving (Show, Generic, Serialize)

newtype WorldActivityDTO = WorldActivityDTO { wadPages ∷ [PageActivityDTO] }
    deriving stock (Generic)
    deriving newtype (Show, Serialize)

-- | The FROZEN pre-#2243 activity slice (@world-activity@ v5): the v5
--   shape verbatim — everything the current slice carries, but with the
--   crop plots and plant designations still naming their species by
--   runtime ordinal.
data PageActivityDTOv5 = PageActivityDTOv5
    { pad5PageId        ∷ !WorldPageId
    , pad5Mine          ∷ !(HM.HashMap (Int, Int) MineDesignationDTO)
    , pad5Construct     ∷ !(HM.HashMap (Int, Int) ConstructDesignationDTO)
    , pad5Chop          ∷ !(HM.HashMap FloraInstanceId ChopDesignationDTO)
    , pad5Till          ∷ !(HM.HashMap (Int, Int) TillDesignationDTO)
    , pad5Plant         ∷ !(HM.HashMap (Int, Int) PlantDesignationDTOv1)
    , pad5FloraHarvests ∷ !(HM.HashMap FloraInstanceId Float)
    , pad5CropPlots     ∷ !(HM.HashMap (Int, Int) CropPlotDTOv1)
    , pad5GroundItems   ∷ !GroundItemsDTO
    , pad5SpoilPiles    ∷ !(HM.HashMap (Int, Int) SpoilPileDTO)
    , pad5PendingChop   ∷ !(HM.HashMap (Int, Int) ChopDesignationDTOv1)
    , pad5PendingHarvests ∷ !FloraHarvestsDTOv1
    , pad5ConstructNextAttempt ∷ !ConstructAttemptId
    } deriving (Show, Generic, Serialize)

newtype WorldActivityDTOv5 =
    WorldActivityDTOv5 { wad5Pages ∷ [PageActivityDTOv5] }
    deriving stock (Generic)
    deriving newtype (Show, Serialize)

-- | The FROZEN pre-#1844 activity slice (@world-activity@ v4): #1854's
--   instance-keyed Chop/harvest maps and pending-migration maps, but
--   with construct designations still at their five-field
--   'ConstructDesignationDTOv1' shape and no attempt allocator.
data PageActivityDTOv4 = PageActivityDTOv4
    { pad4PageId          ∷ !WorldPageId
    , pad4Mine            ∷ !(HM.HashMap (Int, Int) MineDesignationDTO)
    , pad4Construct       ∷ !(HM.HashMap (Int, Int) ConstructDesignationDTOv1)
    , pad4Chop            ∷ !(HM.HashMap FloraInstanceId ChopDesignationDTO)
    , pad4Till            ∷ !(HM.HashMap (Int, Int) TillDesignationDTO)
    , pad4Plant           ∷ !(HM.HashMap (Int, Int) PlantDesignationDTOv1)
    , pad4FloraHarvests   ∷ !(HM.HashMap FloraInstanceId Float)
    , pad4CropPlots       ∷ !(HM.HashMap (Int, Int) CropPlotDTOv1)
    , pad4GroundItems     ∷ !GroundItemsDTO
    , pad4SpoilPiles      ∷ !(HM.HashMap (Int, Int) SpoilPileDTO)
    , pad4PendingChop     ∷ !(HM.HashMap (Int, Int) ChopDesignationDTOv1)
    , pad4PendingHarvests ∷ !FloraHarvestsDTOv1
    } deriving (Show, Generic, Serialize)

newtype WorldActivityDTOv4 =
    WorldActivityDTOv4 { wad4Pages ∷ [PageActivityDTOv4] }
    deriving stock (Generic)
    deriving newtype (Show, Serialize)

-- | The FROZEN pre-#1854 activity slice (@world-activity@ v3): the same
--   fields the current slice carries, but with Chop designations and
--   regrowth timers still keyed by TILE and with no pending-migration
--   maps. Its harvest field names 'FloraHarvestsDTOv1' explicitly
--   rather than following the live 'FloraHarvests' alias #1854
--   re-pointed.
data PageActivityDTOv3 = PageActivityDTOv3
    { pad3PageId        ∷ !WorldPageId
    , pad3Mine          ∷ !(HM.HashMap (Int, Int) MineDesignationDTO)
    , pad3Construct     ∷ !(HM.HashMap (Int, Int) ConstructDesignationDTOv1)
    , pad3Chop          ∷ !(HM.HashMap (Int, Int) ChopDesignationDTOv1)
    , pad3Till          ∷ !(HM.HashMap (Int, Int) TillDesignationDTO)
    , pad3Plant         ∷ !(HM.HashMap (Int, Int) PlantDesignationDTOv1)
    , pad3FloraHarvests ∷ !FloraHarvestsDTOv1
    , pad3CropPlots     ∷ !(HM.HashMap (Int, Int) CropPlotDTOv1)
    , pad3GroundItems   ∷ !GroundItemsDTO
    , pad3SpoilPiles    ∷ !(HM.HashMap (Int, Int) SpoilPileDTO)
    } deriving (Show, Generic, Serialize)

newtype WorldActivityDTOv3 =
    WorldActivityDTOv3 { wad3Pages ∷ [PageActivityDTOv3] }
    deriving stock (Generic)
    deriving newtype (Show, Serialize)

-- | The FROZEN pre-#1233 activity slice (@world-activity@ v1 AND v2 —
--   #1175's bump was semantic, so both encoded versions share this one
--   byte layout). Identical to the current slice except for the ground
--   items it carries, which is the whole reason it exists: #1233 appended
--   physical values to the recursive item tree, and the shipped v1/v2
--   payloads must keep decoding at their original layout.
data PageActivityDTOv2 = PageActivityDTOv2
    { pad2PageId        ∷ !WorldPageId
    , pad2Mine          ∷ !(HM.HashMap (Int, Int) MineDesignationDTO)
    , pad2Construct     ∷ !(HM.HashMap (Int, Int) ConstructDesignationDTOv1)
    , pad2Chop          ∷ !(HM.HashMap (Int, Int) ChopDesignationDTOv1)
    , pad2Till          ∷ !(HM.HashMap (Int, Int) TillDesignationDTO)
    , pad2Plant         ∷ !(HM.HashMap (Int, Int) PlantDesignationDTOv1)
    , pad2FloraHarvests ∷ !FloraHarvestsDTOv1
    , pad2CropPlots     ∷ !(HM.HashMap (Int, Int) CropPlotDTOv1)
    , pad2GroundItems   ∷ !GroundItemsDTOv1
    , pad2SpoilPiles    ∷ !(HM.HashMap (Int, Int) SpoilPileDTO)
    } deriving (Show, Generic, Serialize)

newtype WorldActivityDTOv2 =
    WorldActivityDTOv2 { wad2Pages ∷ [PageActivityDTOv2] }
    deriving stock (Generic)
    deriving newtype (Show, Serialize)

migratePageActivityV2 ∷ PageActivityDTOv2 → PageActivityDTOv3
migratePageActivityV2 s = PageActivityDTOv3
    { pad3PageId        = pad2PageId s
    , pad3Mine          = pad2Mine s
    , pad3Construct     = pad2Construct s
    , pad3Chop          = pad2Chop s
    , pad3Till          = pad2Till s
    , pad3Plant         = pad2Plant s
    , pad3FloraHarvests = pad2FloraHarvests s
    , pad3CropPlots     = pad2CropPlots s
    , pad3GroundItems   = migrateGroundItemsDTOv1 (pad2GroundItems s)
    , pad3SpoilPiles    = pad2SpoilPiles s
    }

-- | v3 → v4 (#1854). The two re-keyed maps CANNOT be translated here:
--   resolving a tile to the plant that stands on it needs that chunk's
--   flora, which a pure component migration has no access to. Both
--   therefore land in the PENDING maps verbatim, and
--   "World.Flora.Designation" drains each entry the moment its chunk is
--   admitted to residency — expanding one legacy tile timer onto every
--   harvestable co-tenant (the observable behaviour the tile-keyed map
--   had), and resolving one legacy designation to the single plant the
--   old wood-tagged harvest would have felled. The live maps therefore
--   start EMPTY, which is honest: nothing is known per-instance yet.
--
--   Every other field crosses unchanged, crop plots included — they are
--   tile-keyed by construction and #1854 does not touch them.
migratePageActivityV3 ∷ PageActivityDTOv3 → PageActivityDTOv4
migratePageActivityV3 s = PageActivityDTOv4
    { pad4PageId          = pad3PageId s
    , pad4Mine            = pad3Mine s
    , pad4Construct       = pad3Construct s
    , pad4Chop            = HM.empty
    , pad4Till            = pad3Till s
    , pad4Plant           = pad3Plant s
    , pad4FloraHarvests   = HM.empty
    , pad4CropPlots       = pad3CropPlots s
    , pad4GroundItems     = pad3GroundItems s
    , pad4SpoilPiles      = pad3SpoilPiles s
    , pad4PendingChop     = pad3Chop s
    , pad4PendingHarvests = pad3FloraHarvests s
    }

-- | v4 → v5 (#1844). Every field crosses unchanged except the construct
--   designations, which gain an attempt identity and a payment record
--   apiece — see 'migrateConstructDesignations' for why the assignment
--   is ordered rather than taken from hashmap traversal, and why a
--   legacy @paid@ flag becomes 'CpLegacyPaid' rather than a receipt
--   invented here.
migratePageActivityV4 ∷ PageActivityDTOv4 → PageActivityDTOv5
migratePageActivityV4 s =
    let (construct, next) = migrateConstructDesignations (pad4Construct s)
    in PageActivityDTOv5
        { pad5PageId               = pad4PageId s
        , pad5Mine                 = pad4Mine s
        , pad5Construct            = construct
        , pad5Chop                 = pad4Chop s
        , pad5Till                 = pad4Till s
        , pad5Plant                = pad4Plant s
        , pad5FloraHarvests        = pad4FloraHarvests s
        , pad5CropPlots            = pad4CropPlots s
        , pad5GroundItems          = pad4GroundItems s
        , pad5SpoilPiles           = pad4SpoilPiles s
        , pad5PendingChop          = pad4PendingChop s
        , pad5PendingHarvests      = pad4PendingHarvests s
        , pad5ConstructNextAttempt = next
        }

-- | v5 → v6 (#2243). Every field crosses unchanged except the crop
--   plots and plant designations, whose species ordinals become legacy
--   references — see 'migrateWorldEditDTOv2' for why a pure migration
--   cannot resolve one to a name here, and where it is resolved instead.
migratePageActivityV5 ∷ PageActivityDTOv5 → PageActivityDTO
migratePageActivityV5 s = PageActivityDTO
    { padPageId               = pad5PageId s
    , padMine                 = pad5Mine s
    , padConstruct            = pad5Construct s
    , padChop                 = pad5Chop s
    , padTill                 = pad5Till s
    , padPlant                = HM.map migratePlantDesignationDTOv1
                                      (pad5Plant s)
    , padFloraHarvests        = pad5FloraHarvests s
    , padCropPlots            = HM.map migrateCropPlotDTOv1 (pad5CropPlots s)
    , padGroundItems          = pad5GroundItems s
    , padSpoilPiles           = pad5SpoilPiles s
    , padPendingChop          = pad5PendingChop s
    , padPendingHarvests      = pad5PendingHarvests s
    , padConstructNextAttempt = pad5ConstructNextAttempt s
    }

-- | v1/v2 → v3: every designation map crosses unchanged and each ground
--   item's physical values decode absent (see 'migrateItemInstanceDTOv1'
--   for why). #1175's v1 canonical-frame repair is deliberately NOT done
--   here — 'applyWorldActivity' re-keys for EVERY accepted version, so
--   the repair stays in one place rather than being split between a
--   migration and an apply step.
migrateWorldActivityV2 ∷ WorldActivityDTOv2 → WorldActivityDTO
migrateWorldActivityV2 (WorldActivityDTOv2 slices) =
    WorldActivityDTO
        (map (migratePageActivityV5 . migratePageActivityV4
                                    . migratePageActivityV3
                                    . migratePageActivityV2)
             slices)

-- | v3 → v6: see 'migratePageActivityV3' onward.
migrateWorldActivityV3 ∷ WorldActivityDTOv3 → WorldActivityDTO
migrateWorldActivityV3 (WorldActivityDTOv3 slices) =
    WorldActivityDTO (map (migratePageActivityV5 . migratePageActivityV4
                                                 . migratePageActivityV3)
                          slices)

-- | v4 → v6: see 'migratePageActivityV4' and 'migratePageActivityV5'.
migrateWorldActivityV4 ∷ WorldActivityDTOv4 → WorldActivityDTO
migrateWorldActivityV4 (WorldActivityDTOv4 slices) =
    WorldActivityDTO (map (migratePageActivityV5 . migratePageActivityV4)
                          slices)

-- | v5 → v6: see 'migratePageActivityV5'.
migrateWorldActivityV5 ∷ WorldActivityDTOv5 → WorldActivityDTO
migrateWorldActivityV5 (WorldActivityDTOv5 slices) =
    WorldActivityDTO (map migratePageActivityV5 slices)

-- | Component-local invariant (#760, mirrors
--   @worldPagesCodec@'s @validatePages@ precedent above): every ground
--   item's own id must sit below that page's ground-items allocator
--   ('gisiNextId') — 'Item.Ground.GroundItems' ids are allocated
--   per-page (see 'Item.Ground.emptyGroundItems'). A literal duplicate
--   key within one page's @gisiItems@ map is structurally impossible
--   once decoded (a 'HashMap' cannot carry two entries under the same
--   key), so there is nothing further to check there.
--
--   #1667: the page's OWN allocator floor is a separate clause, so it
--   is checked even when @gisiItems@ is empty — an empty map used to
--   certify any cursor, including a NEGATIVE one ('gisiNextId' being an
--   unrestricted wire 'Int'), which 'Item.Ground.spawnGroundItem' would
--   then hand out verbatim. Ground items are the engine's one ZERO-based
--   allocator (@docs\/persistence_contract.md@), so the floor here is 0,
--   not the 1 every other allocator uses; it is read from
--   'emptyGroundItems' itself rather than restated, so the two cannot
--   drift. 'World.Save.Component.Transfer.validateTransferOrders' is the
--   precedent this generalizes.
validateWorldActivity ∷ WorldActivityDTO → [ComponentError]
validateWorldActivity (WorldActivityDTO slices) = concat
    [ [ ComponentError worldActivityComponentId 3 ValidatePhase
          ("page '" <> tshow (padPageId s) <> "': ground-item allocator \
             \is " <> tshow (gisiNextId (padGroundItems s)) <> ", below \
             \the first valid ground-item id (" <> tshow firstGroundItemId
           <> ")")
      | s ← slices
      , gisiNextId (padGroundItems s) < firstGroundItemId
      ]
    , [ ComponentError worldActivityComponentId 3 ValidatePhase
          ("page '" <> tshow (padPageId s) <> "': ground item #"
           <> tshow gid <> " is not below the page's ground-item \
              \allocator (" <> tshow (gisiNextId (padGroundItems s)) <> ")")
      | s   ← slices
      , gid ← HM.keys (gisiItems (padGroundItems s))
      , gid ≥ gisiNextId (padGroundItems s)
      ]
    ]
  where
    firstGroundItemId = gisNextId emptyGroundItems

-- | v2 (#1175) is a SEMANTIC bump, not a shape change: the wire layout
--   is byte-identical to v1, but a v2 payload promises every designation
--   key is CANONICAL (u-wrapped into the frame chunks are stored under —
--   see "World.Render.HitTest"'s frame contract), so one physical tile
--   has exactly one key. v1 made no such promise: it recorded whatever
--   raw coord the pick reported, which at the seam could be an alias.
--
--   v1 therefore stays explicitly accepted through its own frozen entry
--   rather than having its meaning quietly rewritten underneath it, and
--   'applyWorldActivity' canonicalises a v1 payload's keys on the way
--   into the session (it re-saves as the current version, so the repair
--   is durable).
--
--   v3 (#1233) is the first SHAPE change: ground items carry the physical
--   values #1233 added to the recursive item tree. v1 and v2 are
--   byte-identical to each other (#1175 changed only what their keys
--   PROMISE), so both decode through the one frozen 'WorldActivityDTOv2'
--   layout and both take 'applyWorldActivity''s re-keying — a v2 payload
--   is already canonical, so for it that step is the identity.
--   v4 (#1854) is the second SHAPE change: Chop designations and
--   regrowth timers are keyed by 'FloraInstanceId' instead of by tile,
--   and the slice gained the two tile-keyed PENDING maps that hold a
--   legacy entry until its chunk arrives. v3's own layout is frozen as
--   'PageActivityDTOv3'; v1 and v2 keep sharing 'PageActivityDTOv2' and
--   now reach v4 through v3, so there is exactly one translation of the
--   old tile keys rather than two copies of it.
--   v5 (#1844) is the third: every construct designation carries its own
--   attempt identity and payment record, and the page carries the
--   allocator that issued them. v4's own layout is frozen as
--   'PageActivityDTOv4', and every older version reaches v5 through it,
--   so the legacy assignment lives in exactly one place.
worldActivityCodec ∷ ComponentCodec WorldActivityDTO
worldActivityCodec = componentCodec ComponentSpec
    { csComponent     = worldActivityComponentId
    , csVersion       = 6
    , csRequired      = True
    , csDeps          = [worldPagesComponentId]
    , csEncode        = \snap →
        WorldActivityDTO (map toActivity (orderedPages snap))
    , csDecode        = id
    , csOlderVersions = [ atVersion 5 migrateWorldActivityV5
                        , atVersion 4 migrateWorldActivityV4
                        , atVersion 3 migrateWorldActivityV3
                        , atVersion 2 migrateWorldActivityV2
                        , atVersion 1 migrateWorldActivityV2 ]
    , csValidate      = validateWorldActivity
    }
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
        , padPendingChop   =
            HM.map toPendingChopDesignationDTO (pgsPendingChopMigration p)
        , padPendingHarvests = pgsPendingFloraHarvests p
        , padConstructNextAttempt = pgsConstructNextAttempt p
        }

-- | #1175: a v1 slice's designation keys carry no canonical-frame
--   promise, so each is re-keyed into the stored frame here — using the
--   PAGE's own world size, which @csDeps = [worldPagesComponentId]@
--   guarantees is already in place. Two v1 aliases of one physical tile
--   collapse to a single key — the last in ascending ORIGINAL-key order
--   wins. Which of the two survives is arbitrary (they describe the same
--   tile), but it must not be HASH order, or the repair and the v2 bytes
--   it re-saves as would vary run to run.
--
--   Applied for every accepted version: on a v2 payload it is the
--   identity by construction, which is exactly the invariant worth
--   re-establishing at the boundary rather than trusting.
applyWorldActivity
    ∷ Word32 → WorldActivityDTO → HM.HashMap WorldPageId PageSnapshot
    → Either [ComponentError] (HM.HashMap WorldPageId PageSnapshot)
applyWorldActivity ver (WorldActivityDTO slices) =
    applyPageSlices worldActivityComponentId ver padPageId writeActivity slices
  where
    canonDesignationTile ws cd =
        let (cgx, cgy) = canonicalTile ws (chGX cd) (chGY cd)
        in cd { chGX = cgx, chGY = cgy }
    writeActivity s p =
        let ws = wgpWorldSize (pgsGenParams p)
            canon ∷ HM.HashMap (Int, Int) v → HM.HashMap (Int, Int) v
            canon m = HM.fromList
                [ (canonicalTile ws gx gy, v)
                | ((gx, gy), v) ← L.sortOn fst (HM.toList m) ]
        in p
            { pgsMineDesignations      = canon (fromMineDTO (padMine s))
            , pgsConstructDesignations = canon (fromConstructDTO (padConstruct s))
            -- The allocator is raised past every id the page actually
            -- carries, so a payload whose cursor sits below one of its
            -- own designations (a hand-edited or truncated save) cannot
            -- reissue that id. Identity for a well-formed payload.
            , pgsConstructNextAttempt =
                advanceConstructAttemptsPast
                    (map cdiAttempt (HM.elems (padConstruct s)))
                    (padConstructNextAttempt s)
              -- #1854: keyed by instance identity, so there is no key to
              -- canonicalise — but the TILE each designation carries is
              -- still a coordinate, and #1175's frame promise has to hold
              -- for it exactly as it did for the old key.
            , pgsChopDesignations      =
                HM.map (canonDesignationTile ws) (fromChopDTO (padChop s))
            , pgsTillDesignations      = canon (fromTillDTO (padTill s))
            , pgsPlantDesignations     = canon (fromPlantDTO (padPlant s))
            , pgsFloraHarvests         = padFloraHarvests s
            , pgsCropPlots             = fromCropDTO (padCropPlots s)
            , pgsGroundItems           = fromGroundItemsDTO (padGroundItems s)
            , pgsSpoilPiles            = fromSpoilDTO (padSpoilPiles s)
              -- The two PENDING maps are the last tile-keyed shapes, so
              -- they take the same v1 canonical-frame repair every other
              -- tile key does — a pre-#1175 alias here would otherwise
              -- never match the chunk that could resolve it.
            , pgsPendingChopMigration  = HM.fromList
                [ let tile = canonicalTile ws gx gy
                  in (tile, fromPendingChopDesignationDTO tile cd)
                | ((gx, gy), cd) ← L.sortOn fst
                                     (HM.toList (padPendingChop s)) ]
            , pgsPendingFloraHarvests  = canon (padPendingHarvests s)
            }
