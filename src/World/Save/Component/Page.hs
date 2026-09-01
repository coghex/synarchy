{-# LANGUAGE Strict, DeriveGeneric, DeriveAnyClass, DerivingStrategies #-}
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
--                             see "World.Save.Component.WorldGen"; the
--                             pre-#1104 shape stays as
--                             'WorldGenParamsDTOv4', the pre-#1102 one as
--                             'WorldGenParamsDTOv3', the pre-#1101 one as
--                             'WorldGenParamsDTOv2' and the pre-#911 one
--                             as 'WorldGenParamsDTOv1')
--   - 'WorldIdentity'       → 'WorldIdentityDTO' (its optional
--                             'LanguageProvenance' frozen as
--                             'LanguageProvenanceDTO' and its optional
--                             #1104 etymology source as
--                             'EtymologySourceDTO'; the pre-#1104 shape
--                             stays as 'WorldIdentityDTOv2' and the
--                             pre-#1092 one as 'WorldIdentityDTOv1')
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
--   'FloraId', and — since #1854 — the opaque 'FloraInstanceId' the
--   Chop/harvest maps are keyed by). A regrowth timer is a bare 'Float'
--   with no record at all to freeze, but its MAP is not a leaf: #1854
--   re-keyed the live 'FloraHarvests' alias, so every frozen slice names
--   'FloraHarvestsDTOv1' — the tile-keyed shape those payloads were
--   written with — explicitly. The DTO field order is chosen
--   so the derived cereal layout is byte-identical to the previous direct
--   embedding — the frozen tracked fixture stays valid.
module World.Save.Component.Page
    ( worldPagesCodec
    , worldEditsCodec
    , worldActivityCodec
    , PageCoreDTO(..)
    , WorldPagesDTO(..)
    , PageCoreDTOv1(..)
    , WorldPagesDTOv1(..)
    , PageCoreDTOv2(..)
    , WorldPagesDTOv2(..)
    , PageCoreDTOv3(..)
    , WorldPagesDTOv3(..)
    , PageCoreDTOv4(..)
    , WorldPagesDTOv4(..)
    , PageCoreDTOv5(..)
    , WorldPagesDTOv5(..)
    , PageCoreDTOv6(..)
    , WorldPagesDTOv6(..)
    , PageCoreDTOv7(..)
    , PageCoreDTOv8(..)
    , WorldPagesDTOv7(..)
    , WorldPagesDTOv8(..)
    , PageEditsDTOv1(..)
    , WorldEditsDTOv1(..)
    , WorldPages(..)
    , migrateWorldEditsV1
    , migrateWorldPagesV1
    , migrateWorldPagesV2
    , migrateWorldPagesV3
    , migrateWorldPagesV4
    , migrateWorldPagesV5
    , migrateWorldPagesV6
    , migrateWorldPagesV7
    , migrateWorldPagesV8
    , PageEditsDTO(..)
    , WorldEditsDTO(..)
    , PageActivityDTO(..)
    , WorldActivityDTO(..)
    , PageActivityDTOv2(..)
    , PageActivityDTOv3(..)
    , WorldActivityDTOv3(..)
    , PageActivityDTOv4(..)
    , WorldActivityDTOv4(..)
    , WorldActivityDTOv2(..)
    , migrateWorldActivityV2
      -- * Frozen leaf DTOs (requirement 4)
    , WorldGenParamsDTO(..)
    , WorldGenParamsDTOv1(..)
    , WorldGenParamsDTOv2(..)
    , WorldGenParamsDTOv3(..)
    , WorldGenParamsDTOv4(..)
    , WorldGenParamsDTOv5(..)
    , WorldGenParamsDTOv6(..)
    , WorldGenParamsDTOv7(..)
    , RiverNameDTO(..)
    , RiverNamesDTO(..)
    , EtymologySourceDTO(..)
    , WorldIdentityDTO(..)
    , WorldIdentityDTOv1(..)
    , WorldIdentityDTOv2(..)
    , LanguageProvenanceDTO(..)
    , toWorldIdentityDTOv2
    , WorldEditDTO(..)
    , WorldEditDTOv1(..)
    , MineDesignationDTO(..)
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
    , CropPlotDTO(..)
    , ItemStorageDTO(..)
    , ItemInstanceDTO(..)
    , ItemInstanceDTOv1(..)
    , GroundItemDTO(..)
    , GroundItemDTOv1(..)
    , GroundItemsDTO(..)
    , GroundItemsDTOv1(..)
    , SpoilPileDTO(..)
    , toWorldGenParamsDTO
    , fromWorldGenParamsDTO
    , fromWorldGenParamsDTOv1
    , toWorldGenParamsDTOv1
    , fromWorldGenParamsDTOv2
    , toWorldGenParamsDTOv2
    , fromWorldGenParamsDTOv3
    , toWorldGenParamsDTOv3
    , fromWorldGenParamsDTOv4
    , toWorldGenParamsDTOv4
    , fromWorldGenParamsDTOv5
    , toWorldGenParamsDTOv5
    , fromWorldGenParamsDTOv6
    , toWorldGenParamsDTOv6
    , fromWorldGenParamsDTOv7
    , toWorldGenParamsDTOv7
    , toEtymologySourceDTO
    , fromEtymologySourceDTO
    , toItemInstanceDTO
    , fromItemInstanceDTO
    , toItemInstanceDTOv1
    , migrateItemInstanceDTOv1
    , toGroundItemDTO
    , fromGroundItemDTO
    , toGroundItemsDTO
    , toGroundItemsDTOv1
    , basePageSnapshots
    , blankPageSnapshot
    , applyWorldEdits
    , applyWorldActivity
    , validatePages
    , validateWorldActivity
    ) where

import UPrelude
import qualified Data.HashMap.Strict as HM
import qualified Data.List as L
import Data.Serialize (Serialize)
import GHC.Generics (Generic)
import Craft.Bills (emptyCraftBills)
import Unit.Transfer.Orders (emptyTransferOrders)
import Building.Knowledge (emptyContainerKnowledge)
import Power.Types (emptyPowerNodes)
import World.Save.Component.WorldGen
    ( WorldGenParamsDTO(..), toWorldGenParamsDTO, fromWorldGenParamsDTO
    , WorldGenParamsDTOv1(..), fromWorldGenParamsDTOv1
    , toWorldGenParamsDTOv1
    , WorldGenParamsDTOv2(..), fromWorldGenParamsDTOv2
    , toWorldGenParamsDTOv2
    , WorldGenParamsDTOv3(..), fromWorldGenParamsDTOv3
    , toWorldGenParamsDTOv3
    , WorldGenParamsDTOv4(..), fromWorldGenParamsDTOv4
    , toWorldGenParamsDTOv4
    , WorldGenParamsDTOv5(..), fromWorldGenParamsDTOv5
    , toWorldGenParamsDTOv5
    , WorldGenParamsDTOv6(..), fromWorldGenParamsDTOv6
    , toWorldGenParamsDTOv6
    , WorldGenParamsDTOv7(..), fromWorldGenParamsDTOv7
    , toWorldGenParamsDTOv7
    , EtymologySourceDTO(..)
    , toEtymologySourceDTO, fromEtymologySourceDTO
    , RiverNameDTO(..), RiverNamesDTO(..) )
import Location.Instance
    ( locationInstanceAllocatorErrors, locationInstanceBoundsErrors
    , locationSignificantItemErrors )
import World.Generate.Types (WorldGenParams(..))
import World.Generate.Coordinates (canonicalTile)
import World.Chunk.Types (ChunkCoord(..), wrapChunkCoordU)
import World.Page.Types (WorldPageId, WorldIdentity(..))
import Language.Generated.Types
    ( LanguageProvenance(..), LangSeed(..), GeneratorVersion(..) )
import World.Render.Zoom.Types (ZoomMapMode(..))
import World.Edit.Types (WorldEdit(..), WorldEdits, emptyWorldEdits)
import World.Fluid.Types (FluidType)
import World.Material.Id (MaterialId)
import World.Flora.Types (FloraId)
import World.Mine.Types (MineDesignation(..), MineDesignations)
import World.Construct.Types
    ( ConstructDesignation(..), ConstructTarget(..), StructurePiece(..)
    , ConstructStatus, ConstructDesignations )
import World.Chop.Types (ChopDesignation(..), ChopDesignations)
import World.Construct.Attempt
    ( ConstructAttemptId, advanceConstructAttemptsPast
    , firstConstructAttemptId, takeConstructAttempts )
import World.Construct.Receipt (ConstructPayment(..))
import World.Flora.Identity
    ( FloraInstanceId, floraInstanceIdToLua
    , isFloraInstanceIdNone, isPlantedFloraInstanceId
    , firstPlantedFloraCursor, nextPlantedFloraCursor
    , plantedFloraCursorAbove )
import World.Till.Types (TillDesignation(..), TillDesignations)
import World.Plant.Types (PlantDesignation(..), PlantDesignations)
import World.Spoil.Types (SpoilPile(..), SpoilPiles, emptySpoilPiles)
import World.Flora.Harvest (emptyFloraHarvests,
                            emptyPendingFloraHarvests)
import World.Flora.CropPlot (CropPlot(..), CropPlots, emptyCropPlots)
import Item.Ground (GroundItem(..), GroundItems(..), emptyGroundItems)
import Item.Types (ItemInstance(..), ItemStorage(..))
import World.Save.Types
    ( BuildingSnapshot(..), UnitSnapshot(..) )
import World.Save.Snapshot (SessionSnapshot(..), PageSnapshot(..))
import World.Save.Component.Types

-- Canonical (page-id ascending) ordered list of a snapshot's pages.
orderedPages ∷ SessionSnapshot → [PageSnapshot]
orderedPages = L.sortOn pgsPageId . HM.elems . snapPages

-- Frozen leaf DTOs (requirement 4) -----------------------------------

-- | Frozen mirror of 'WorldIdentity' — the CURRENT (world-pages v9)
--   shape: the optional language provenance #1092 added, plus the
--   optional etymology source #1104 added. #1230 took the component to
--   v7 and #916 took it to v8 without touching the identity, so both
--   frozen page cores embed this same type.
--
--   The two are independently optional, exactly as they are on the live
--   record: provenance says WHICH language named the world, the source
--   says WHAT expression it rendered. A world can have the first without
--   the second (a caller that recorded a language but no expression),
--   never the second without the first — the source carries its own
--   provenance, so the pair can never disagree about the language.
data WorldIdentityDTO = WorldIdentityDTO
    { widName      ∷ !Text
    , widGloss     ∷ !(Maybe Text)
    , widLanguage  ∷ !(Maybe LanguageProvenanceDTO)
    , widEtymology ∷ !(Maybe EtymologySourceDTO)
    } deriving (Show, Eq, Generic, Serialize)

-- | Frozen mirror of 'LanguageProvenance' (#1092). Seed and version
--   live in ONE optional DTO, never as two independently-optional
--   fields — a decode can then never produce a seed without a version
--   (or the reverse), which would be an unreconstructible profile.
--   The primitives are the wire contract; the live newtypes are
--   reapplied on the way back in.
data LanguageProvenanceDTO = LanguageProvenanceDTO
    { lpdSeed    ∷ !Word64
    , lpdVersion ∷ !Int
    } deriving (Show, Eq, Generic, Serialize)

toLanguageProvenanceDTO ∷ LanguageProvenance → LanguageProvenanceDTO
toLanguageProvenanceDTO p = LanguageProvenanceDTO
    { lpdSeed    = langSeedWord (lpSeed p)
    , lpdVersion = generatorVersionInt (lpVersion p)
    }

fromLanguageProvenanceDTO ∷ LanguageProvenanceDTO → LanguageProvenance
fromLanguageProvenanceDTO d = LanguageProvenance
    { lpSeed    = LangSeed (lpdSeed d)
    , lpVersion = GeneratorVersion (lpdVersion d)
    }

toWorldIdentityDTO ∷ WorldIdentity → WorldIdentityDTO
toWorldIdentityDTO i = WorldIdentityDTO (wiName i) (wiGloss i)
    (toLanguageProvenanceDTO <$> wiLanguage i)
    (toEtymologySourceDTO <$> wiEtymology i)

fromWorldIdentityDTO ∷ WorldIdentityDTO → WorldIdentity
fromWorldIdentityDTO d = WorldIdentity (widName d) (widGloss d)
    (fromLanguageProvenanceDTO <$> widLanguage d)
    (fromEtymologySourceDTO <$> widEtymology d)

-- | The FROZEN pre-#1104 identity shape (@world-pages@ v3 through v5),
--   preserved verbatim for decode-only backward compatibility: name,
--   gloss, and #1092's language provenance, with no etymology source.
--   Never edited; a further identity schema change freezes the CURRENT
--   shape as 'WorldIdentityDTOv3' rather than touching this one
--   (frozen-DTO boundary rule).
data WorldIdentityDTOv2 = WorldIdentityDTOv2
    { wid2Name     ∷ !Text
    , wid2Gloss    ∷ !(Maybe Text)
    , wid2Language ∷ !(Maybe LanguageProvenanceDTO)
    } deriving (Show, Eq, Generic, Serialize)

-- | Encoder for the frozen shape — the round-trip partner a frozen-DTO
--   fixture is built with (the same reason 'toWorldGenParamsDTOv3'
--   exists). Kept exported with no consumer yet (#1119) because that
--   fixture seam is the whole point of a frozen shape's encoder, and
--   'toItemInstanceDTOv1' cites it as the precedent for its own.
toWorldIdentityDTOv2 ∷ WorldIdentity → WorldIdentityDTOv2
toWorldIdentityDTOv2 i = WorldIdentityDTOv2 (wiName i) (wiGloss i)
    (toLanguageProvenanceDTO <$> wiLanguage i)

-- | A pre-#1104 identity keeps its name, gloss, and language EXACTLY
--   and decodes with NO etymology source — the same honest absence
--   @fromWorldIdentityDTOv1@ produces for provenance. A world named
--   before the expression was recorded genuinely has none to recover,
--   and deriving one from the name would fabricate a meaning.
fromWorldIdentityDTOv2 ∷ WorldIdentityDTOv2 → WorldIdentity
fromWorldIdentityDTOv2 d = WorldIdentity (wid2Name d) (wid2Gloss d)
    (fromLanguageProvenanceDTO <$> wid2Language d) Nothing

-- | The FROZEN pre-#1092 identity shape, preserved verbatim for
--   decode-only backward compatibility: name and gloss, no language.
--   Referenced by the frozen 'PageCoreDTOv1'/'PageCoreDTOv2' page cores
--   and by "World.Save.Compat.SessionV90"'s v90 page save. Never
--   edited; a further identity schema change freezes the CURRENT
--   shape as 'WorldIdentityDTOv2' rather than touching either of them
--   (frozen-DTO boundary rule).
data WorldIdentityDTOv1 = WorldIdentityDTOv1
    { wid1Name  ∷ !Text
    , wid1Gloss ∷ !(Maybe Text)
    } deriving (Show, Eq, Generic, Serialize)

-- | Historical identities decode with provenance ABSENT — never
--   inferred (#1092 requirement 3, following #915's precedent). A
--   world named before provenance was recorded genuinely has no
--   recoverable language, and guessing one would attach a false
--   etymology to a real world. Name and gloss carry across exactly.
fromWorldIdentityDTOv1 ∷ WorldIdentityDTOv1 → WorldIdentity
fromWorldIdentityDTOv1 d =
    WorldIdentity (wid1Name d) (wid1Gloss d) Nothing Nothing

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
      -- #1854: appended at the END, never grown in place. This sum is
      -- positionally serialized, so a sixth field on @WePlaceFloraD@
      -- would have reinterpreted tag 8 in every shipped v1 log; a new
      -- trailing constructor leaves all eleven older tags meaning exactly
      -- what they meant (tools/enum_append_only_audit.py).
    | WePlaceFloraWithIdD !Int !Int !FloraId !Int !Float !FloraInstanceId
    deriving (Show, Eq, Generic, Serialize)

-- | The FROZEN pre-#1854 edit sum, preserved verbatim for decode-only
--   backward compatibility (@world-edits@ v1, and the v90 compatibility
--   tree). #1854 appended a 'FloraInstanceId' to @WePlaceFloraD@'s
--   PAYLOAD — which moves no constructor tag but changes that
--   constructor's bytes, so a shipped v1 log must decode through this
--   copy of the old shape rather than the live one.
--
--   Its constructor ORDER is a wire contract of its own and is never
--   touched; a new live constructor is added to 'WorldEditDTO' above,
--   which makes 'migrateWorldEditDTOv1' non-exhaustive in the safe
--   direction (a compile error here, never silent drift on disk).
data WorldEditDTOv1
    = WeDeleteTileDv1 !Int !Int
    | WeSetFluidTileDv1 !Int !Int !FluidType
    | WeAddTileDv1 !Int !Int !MaterialId
    | WeSetSlopeDv1 !Int !Int !Int !Word8
    | WeSetCellDv1 !Int !Int !Int !MaterialId
    | WeSetStructureDv1 !Int !Int !Word8 !Int !Int !Int
    | WeClearStructureDv1 !Int !Int !Word8
    | WeSetVegDv1 !Int !Int !Int !Word8
    | WePlaceFloraDv1 !Int !Int !FloraId !Int !Float
    | WeSetFluidSnapshotDv1 !Int !Int !FluidType !Int
    | WeClearFluidSnapshotDv1 !Int !Int
    deriving (Show, Eq, Generic, Serialize)

-- | v1 → current. Every constructor crosses unchanged except the
--   planted-flora one, which crosses into the id-LESS @WePlaceFloraD@
--   it has always been: a v1 log records no ids, and there is nothing in
--   its bytes to recover one from. 'applyWorldEdits' rewrites each into
--   the identity-bearing constructor once the page's own world size is
--   in hand (see its note) — the same "repair lives in ONE place, at
--   apply time" rule #1175's canonical-frame repair follows.
migrateWorldEditDTOv1 ∷ WorldEditDTOv1 → WorldEditDTO
migrateWorldEditDTOv1 e = case e of
    WeDeleteTileDv1 a b            → WeDeleteTileD a b
    WeSetFluidTileDv1 a b f        → WeSetFluidTileD a b f
    WeAddTileDv1 a b m             → WeAddTileD a b m
    WeSetSlopeDv1 a b c w          → WeSetSlopeD a b c w
    WeSetCellDv1 a b c m           → WeSetCellD a b c m
    WeSetStructureDv1 a b w c d f  → WeSetStructureD a b w c d f
    WeClearStructureDv1 a b w      → WeClearStructureD a b w
    WeSetVegDv1 a b c w            → WeSetVegD a b c w
    WePlaceFloraDv1 a b fl d fx    → WePlaceFloraD a b fl d fx
    WeSetFluidSnapshotDv1 a b f z  → WeSetFluidSnapshotD a b f z
    WeClearFluidSnapshotDv1 a b    → WeClearFluidSnapshotD a b

toWorldEditDTO ∷ WorldEdit → WorldEditDTO
toWorldEditDTO (WeDeleteTile a b)              = WeDeleteTileD a b
toWorldEditDTO (WeSetFluidTile a b f)          = WeSetFluidTileD a b f
toWorldEditDTO (WeAddTile a b m)               = WeAddTileD a b m
toWorldEditDTO (WeSetSlope a b c w)            = WeSetSlopeD a b c w
toWorldEditDTO (WeSetCell a b c m)             = WeSetCellD a b c m
toWorldEditDTO (WeSetStructure a b w c d e)    = WeSetStructureD a b w c d e
toWorldEditDTO (WeClearStructure a b w)        = WeClearStructureD a b w
toWorldEditDTO (WeSetVeg a b c w)              = WeSetVegD a b c w
toWorldEditDTO (WePlaceFlora a b fl d fx)     = WePlaceFloraD a b fl d fx
toWorldEditDTO (WePlaceFloraWithId a b fl d fx i) =
    WePlaceFloraWithIdD a b fl d fx i
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
fromWorldEditDTO (WePlaceFloraD a b fl d fx)  = WePlaceFlora a b fl d fx
fromWorldEditDTO (WePlaceFloraWithIdD a b fl d fx i) =
    WePlaceFloraWithId a b fl d fx i
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
--   This is the CURRENT (v9) wire shape — see 'PageCoreDTOv8' for the
--   frozen pre-#917 one, 'PageCoreDTOv7' for the
--   frozen pre-#916 one, 'PageCoreDTOv6' for the
--   frozen pre-#1230 one, 'PageCoreDTOv5' for the
--   pre-#1104 one, 'PageCoreDTOv4' for the pre-#1102 one,
--   'PageCoreDTOv3' for the pre-#1101 one, 'PageCoreDTOv2' for the
--   pre-#1092 one, and 'PageCoreDTOv1' for the pre-#911 one.
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

-- | The FROZEN v1 wire shape, preserved verbatim for decode-only
--   backward compatibility: identical to 'PageCoreDTO' except that its
--   gen params are 'WorldGenParamsDTOv1' (three chunk-keyed location
--   sets, no instance table — #911 replaced two of them) and its
--   identity is the pre-#1092 'WorldIdentityDTOv1'. Never edited; a
--   further schema change adds a newer type instead (frozen-DTO
--   boundary rule). "World.Save.Compat.SessionV90"'s B1 path builds
--   these too, since v90 bytes carry exactly the v1 gen params.
data PageCoreDTOv1 = PageCoreDTOv1
    { pc1PageId      ∷ !WorldPageId
    , pc1GenParams   ∷ !WorldGenParamsDTOv1
    , pc1CameraX     ∷ !Float
    , pc1CameraY     ∷ !Float
    , pc1TimeHour    ∷ !Int
    , pc1TimeMinute  ∷ !Int
    , pc1DateYear    ∷ !Int
    , pc1DateMonth   ∷ !Int
    , pc1DateDay     ∷ !Int
    , pc1MapMode     ∷ !ZoomMapMode
    , pc1Identity    ∷ !(Maybe WorldIdentityDTOv1)
    } deriving (Show, Generic, Serialize)

newtype WorldPagesDTOv1 = WorldPagesDTOv1 { wpd1Pages ∷ [PageCoreDTOv1] }
    deriving stock (Generic)
    deriving newtype (Show, Serialize)

-- | The FROZEN v2 wire shape (#911 through #1092), preserved verbatim
--   for decode-only backward compatibility: the #911 gen params, but
--   the pre-#1092 identity with no language provenance. Never edited.
--
--   Its gen params are 'WorldGenParamsDTOv2', the frozen pre-#1101
--   shape — #1101 was the "later gen-params change" this comment used
--   to anticipate, so the field was repointed off the current type onto
--   the frozen copy exactly as described, leaving these bytes unchanged.
data PageCoreDTOv2 = PageCoreDTOv2
    { pc2PageId      ∷ !WorldPageId
    , pc2GenParams   ∷ !WorldGenParamsDTOv2
    , pc2CameraX     ∷ !Float
    , pc2CameraY     ∷ !Float
    , pc2TimeHour    ∷ !Int
    , pc2TimeMinute  ∷ !Int
    , pc2DateYear    ∷ !Int
    , pc2DateMonth   ∷ !Int
    , pc2DateDay     ∷ !Int
    , pc2MapMode     ∷ !ZoomMapMode
    , pc2Identity    ∷ !(Maybe WorldIdentityDTOv1)
    } deriving (Show, Generic, Serialize)

newtype WorldPagesDTOv2 = WorldPagesDTOv2 { wpd2Pages ∷ [PageCoreDTOv2] }
    deriving stock (Generic)
    deriving newtype (Show, Serialize)

-- | The FROZEN v3 wire shape (#1092 through #1101), preserved verbatim
--   for decode-only backward compatibility: #1092's identity (name,
--   gloss, optional language provenance) over the frozen pre-#1101 gen
--   params, whose location instances carry no gloss of their own. Never
--   edited; a further schema change adds a newer type instead
--   (frozen-DTO boundary rule).
data PageCoreDTOv3 = PageCoreDTOv3
    { pc3PageId      ∷ !WorldPageId
    , pc3GenParams   ∷ !WorldGenParamsDTOv2
    , pc3CameraX     ∷ !Float
    , pc3CameraY     ∷ !Float
    , pc3TimeHour    ∷ !Int
    , pc3TimeMinute  ∷ !Int
    , pc3DateYear    ∷ !Int
    , pc3DateMonth   ∷ !Int
    , pc3DateDay     ∷ !Int
    , pc3MapMode     ∷ !ZoomMapMode
    , pc3Identity    ∷ !(Maybe WorldIdentityDTOv2)
    } deriving (Show, Generic, Serialize)

newtype WorldPagesDTOv3 = WorldPagesDTOv3 { wpd3Pages ∷ [PageCoreDTOv3] }
    deriving stock (Generic)
    deriving newtype (Show, Serialize)

-- | The FROZEN v4 wire shape (#1101 through #1102), preserved verbatim
--   for decode-only backward compatibility: #1092's identity over
--   #1101's gen params, whose location instances carry a gloss but
--   whose page carries no river-name table. Never edited; a further
--   schema change adds a newer type instead (frozen-DTO boundary rule).
data PageCoreDTOv4 = PageCoreDTOv4
    { pc4PageId      ∷ !WorldPageId
    , pc4GenParams   ∷ !WorldGenParamsDTOv3
    , pc4CameraX     ∷ !Float
    , pc4CameraY     ∷ !Float
    , pc4TimeHour    ∷ !Int
    , pc4TimeMinute  ∷ !Int
    , pc4DateYear    ∷ !Int
    , pc4DateMonth   ∷ !Int
    , pc4DateDay     ∷ !Int
    , pc4MapMode     ∷ !ZoomMapMode
    , pc4Identity    ∷ !(Maybe WorldIdentityDTOv2)
    } deriving (Show, Generic, Serialize)

newtype WorldPagesDTOv4 = WorldPagesDTOv4 { wpd4Pages ∷ [PageCoreDTOv4] }
    deriving stock (Generic)
    deriving newtype (Show, Serialize)

-- | The FROZEN v5 wire shape (#1102 through #1104), preserved verbatim
--   for decode-only backward compatibility: #1092's identity over
--   #1102's gen params, neither of which carries an etymology source.
--   Never edited; a further schema change adds a newer type instead
--   (frozen-DTO boundary rule).
data PageCoreDTOv5 = PageCoreDTOv5
    { pc5PageId      ∷ !WorldPageId
    , pc5GenParams   ∷ !WorldGenParamsDTOv4
    , pc5CameraX     ∷ !Float
    , pc5CameraY     ∷ !Float
    , pc5TimeHour    ∷ !Int
    , pc5TimeMinute  ∷ !Int
    , pc5DateYear    ∷ !Int
    , pc5DateMonth   ∷ !Int
    , pc5DateDay     ∷ !Int
    , pc5MapMode     ∷ !ZoomMapMode
    , pc5Identity    ∷ !(Maybe WorldIdentityDTOv2)
    } deriving (Show, Generic, Serialize)

newtype WorldPagesDTOv5 = WorldPagesDTOv5 { wpd5Pages ∷ [PageCoreDTOv5] }
    deriving stock (Generic)
    deriving newtype (Show, Serialize)

-- | The FROZEN v6 wire shape (#1104 through #1230), preserved verbatim
--   for decode-only backward compatibility: #1104's identity (carrying
--   its own etymology source) over #1104's gen params, whose location
--   instances still carry the @discovery_margin@ #1230 removed. Never edited; a further schema
--   change adds a newer type instead (frozen-DTO boundary rule).
data PageCoreDTOv6 = PageCoreDTOv6
    { pc6PageId      ∷ !WorldPageId
    , pc6GenParams   ∷ !WorldGenParamsDTOv5
    , pc6CameraX     ∷ !Float
    , pc6CameraY     ∷ !Float
    , pc6TimeHour    ∷ !Int
    , pc6TimeMinute  ∷ !Int
    , pc6DateYear    ∷ !Int
    , pc6DateMonth   ∷ !Int
    , pc6DateDay     ∷ !Int
    , pc6MapMode     ∷ !ZoomMapMode
    , pc6Identity    ∷ !(Maybe WorldIdentityDTO)
      -- ^ the CURRENT identity shape, not 'WorldIdentityDTOv2': #1104
      --   put an etymology source on the page identity in v6 and #1230
      --   changed nothing about it.
    } deriving (Show, Generic, Serialize)

newtype WorldPagesDTOv6 = WorldPagesDTOv6 { wpd6Pages ∷ [PageCoreDTOv6] }
    deriving stock (Generic)
    deriving newtype (Show, Serialize)

-- | The FROZEN v7 wire shape (#1230 through #916): the current page
--   identity over the frozen pre-encounter worldgen/location DTO.
data PageCoreDTOv7 = PageCoreDTOv7
    { pc7PageId      ∷ !WorldPageId
    , pc7GenParams   ∷ !WorldGenParamsDTOv6
    , pc7CameraX     ∷ !Float
    , pc7CameraY     ∷ !Float
    , pc7TimeHour    ∷ !Int
    , pc7TimeMinute  ∷ !Int
    , pc7DateYear    ∷ !Int
    , pc7DateMonth   ∷ !Int
    , pc7DateDay     ∷ !Int
    , pc7MapMode     ∷ !ZoomMapMode
    , pc7Identity    ∷ !(Maybe WorldIdentityDTO)
    } deriving (Show, Generic, Serialize)

newtype WorldPagesDTOv7 = WorldPagesDTOv7 { wpd7Pages ∷ [PageCoreDTOv7] }
    deriving stock (Generic)
    deriving newtype (Show, Serialize)

-- | The FROZEN v8 wire shape (#916 through #917): the current page
--   identity over the frozen pre-significant-contents worldgen/location
--   DTO ('WorldGenParamsDTOv7'), whose instances carry #916's encounter
--   — with its clearance-notice flag still nested inside it — and no
--   significant-item obligations.
data PageCoreDTOv8 = PageCoreDTOv8
    { pc8PageId      ∷ !WorldPageId
    , pc8GenParams   ∷ !WorldGenParamsDTOv7
    , pc8CameraX     ∷ !Float
    , pc8CameraY     ∷ !Float
    , pc8TimeHour    ∷ !Int
    , pc8TimeMinute  ∷ !Int
    , pc8DateYear    ∷ !Int
    , pc8DateMonth   ∷ !Int
    , pc8DateDay     ∷ !Int
    , pc8MapMode     ∷ !ZoomMapMode
    , pc8Identity    ∷ !(Maybe WorldIdentityDTO)
    } deriving (Show, Generic, Serialize)

newtype WorldPagesDTOv8 = WorldPagesDTOv8 { wpd8Pages ∷ [PageCoreDTOv8] }
    deriving stock (Generic)
    deriving newtype (Show, Serialize)

-- | The canonical decoded value of the @world-pages@ component, kept
--   separate from either wire DTO ("World.Save.Component.Types": the
--   canonical type a codec decodes INTO is the migration target). It is
--   the base 'PageSnapshot' map every other page-scoped component then
--   writes onto, plus the page ids in encoded order — the map alone
--   cannot answer the duplicate-page-id invariant, since a 'HM.HashMap'
--   silently collapses a duplicate key.
data WorldPages = WorldPages
    { wpPageIds ∷ ![WorldPageId]
    , wpBase    ∷ !(HM.HashMap WorldPageId PageSnapshot)
    } deriving (Show)

-- | Encoding always writes the current v9 shape; v8 payloads decode
--   through their own frozen DTO via 'migrateWorldPagesV8' (#917), v7
--   via 'migrateWorldPagesV7' (#916), v6
--   via 'migrateWorldPagesV6' (#1230), v5
--   via 'migrateWorldPagesV5' (#1104), v4
--   via 'migrateWorldPagesV4' (#1102), v3 via 'migrateWorldPagesV3'
--   (#1101), v2 via 'migrateWorldPagesV2'
--   (#1092), and v1 via 'migrateWorldPagesV1' (#911). Issue #1093: this
--   used to be a hand-rolled 'ComponentCodec' because the shared helper
--   had no real multi-version dispatch — 'componentCodec' now expresses
--   it, with each accepted version declared exactly once.
worldPagesCodec ∷ ComponentCodec WorldPages
worldPagesCodec = componentCodec ComponentSpec
    { csComponent     = worldPagesComponentId
    , csVersion       = 9
    , csRequired      = True
    , csDeps          = []
    , csEncode        = \snap →
        WorldPagesDTO (map toPageCore (orderedPages snap))
    , csDecode        = basePageSnapshots
    , csOlderVersions = [ atVersion 8 migrateWorldPagesV8
                        , atVersion 7 migrateWorldPagesV7
                        , atVersion 6 migrateWorldPagesV6
                        , atVersion 5 migrateWorldPagesV5
                        , atVersion 4 migrateWorldPagesV4
                        , atVersion 3 migrateWorldPagesV3
                        , atVersion 2 migrateWorldPagesV2
                        , atVersion 1 migrateWorldPagesV1 ]
    , csValidate      = validatePages
    }
  where
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
--   level so "World.Save.Compat.SessionV90"'s B1
--   migration path can run the SAME validator a modern envelope's
--   decode always does, rather than skip it entirely.
validatePages ∷ WorldPages → [ComponentError]
validatePages wp
    | null (wpPageIds wp) = [err "no world pages in save"]
    | otherwise =
        [ err ("duplicate page id " <> tshow pid)
        | (pid, n) ← HM.toList
                      (HM.fromListWith (+) [ (p, 1 ∷ Int) | p ← wpPageIds wp ])
        , n > 1 ]
        -- #911: the page-local location-instance allocator, mirroring
        -- @world-activity@'s own ground-item allocator check. #1668
        -- adds the table's GEOMETRY beside its ids: the save decode
        -- path is the one place an 'Location.Bounds.AbsBounds' is built
        -- from unrestricted wire 'Int's rather than downstream of the
        -- YAML loader's inverted-bounds gate, so an inverted stored
        -- footprint is rejected HERE -- in ValidatePhase, after every
        -- accepted version has migrated into this one canonical value
        -- -- rather than being published as spatial authority.
        ⧺ [ err ("page '" <> tshow (pgsPageId p) <> "': " <> msg)
          | p   ← HM.elems (wpBase wp)
          , let lis = wgpLocationInstances (pgsGenParams p)
          , msg ← locationInstanceAllocatorErrors lis
                    ⧺ locationInstanceBoundsErrors lis
                    ⧺ locationSignificantItemErrors lis
          ]
  where err = ComponentError worldPagesComponentId 9 ValidatePhase

-- | Turn the decoded current v9 page cores into the base 'PageSnapshot' map every
--   other page-scoped component then writes onto (assembly). All entity/
--   activity/edit fields start empty and are overwritten by their own
--   REQUIRED components; a valid save leaves none of these placeholders.
basePageSnapshots ∷ WorldPagesDTO → WorldPages
basePageSnapshots (WorldPagesDTO ps) = WorldPages
    { wpPageIds = map pcPageId ps
    , wpBase    = HM.fromList [ (pcPageId p, toBase p) | p ← ps ]
    }
  where
    toBase p = (blankPageSnapshot (pcPageId p)
                    (fromWorldGenParamsDTO (pcGenParams p)))
        { pgsCameraX    = pcCameraX p
        , pgsCameraY    = pcCameraY p
        , pgsTimeHour   = pcTimeHour p
        , pgsTimeMinute = pcTimeMinute p
        , pgsDateYear   = pcDateYear p
        , pgsDateMonth  = pcDateMonth p
        , pgsDateDay    = pcDateDay p
        , pgsMapMode    = pcMapMode p
        , pgsIdentity   = fromWorldIdentityDTO <$> pcIdentity p
        }

-- | The v8→v9 migration (#917): every historical placed location keeps
--   its exact stored identity, geometry, name, lifecycle, content flag
--   and encounter — including whether that encounter had already been
--   completed and whether its clearance notice had been spent, which is
--   lifted out of the encounter onto the instance where the
--   generalized latch now lives
--   ('World.Save.Component.WorldGen.fromLocationInstanceDTOv5'). So a
--   ruin defeated before it was ever seen still announces itself once
--   on sight, and one that already announced never announces again.
--
--   It gains NO significant-item obligations. Reading them off today's
--   YAML would hand a previously materialized world an item it never
--   spawned and nobody could take, permanently blocking a clearance the
--   pre-#917 build had already granted on the encounter alone — the
--   same reason 'migrateWorldPagesV7' refuses to roll an encounter, and
--   the same reason the v1 reconstruction discards both.
migrateWorldPagesV8 ∷ WorldPagesDTOv8 → WorldPages
migrateWorldPagesV8 (WorldPagesDTOv8 ps) = WorldPages
    { wpPageIds = map pc8PageId ps
    , wpBase    = HM.fromList [ (pc8PageId p, toBase p) | p ← ps ]
    }
  where
    toBase p = (blankPageSnapshot (pc8PageId p)
                    (fromWorldGenParamsDTOv7 (pc8GenParams p)))
        { pgsCameraX    = pc8CameraX p
        , pgsCameraY    = pc8CameraY p
        , pgsTimeHour   = pc8TimeHour p
        , pgsTimeMinute = pc8TimeMinute p
        , pgsDateYear   = pc8DateYear p
        , pgsDateMonth  = pc8DateMonth p
        , pgsDateDay    = pc8DateDay p
        , pgsMapMode    = pc8MapMode p
        , pgsIdentity   = fromWorldIdentityDTO <$> pc8Identity p
        }

-- | The v7→v8 migration (#916): every historical placed location keeps
--   its exact stored identity, geometry, name, lifecycle, and content flag,
--   and gains no encounter. Rolling an encounter while loading would let the
--   current content build reinterpret a previously materialized world.
migrateWorldPagesV7 ∷ WorldPagesDTOv7 → WorldPages
migrateWorldPagesV7 (WorldPagesDTOv7 ps) = WorldPages
    { wpPageIds = map pc7PageId ps
    , wpBase    = HM.fromList [ (pc7PageId p, toBase p) | p ← ps ]
    }
  where
    toBase p = (blankPageSnapshot (pc7PageId p)
                    (fromWorldGenParamsDTOv6 (pc7GenParams p)))
        { pgsCameraX    = pc7CameraX p
        , pgsCameraY    = pc7CameraY p
        , pgsTimeHour   = pc7TimeHour p
        , pgsTimeMinute = pc7TimeMinute p
        , pgsDateYear   = pc7DateYear p
        , pgsDateMonth  = pc7DateMonth p
        , pgsDateDay    = pc7DateDay p
        , pgsMapMode    = pc7MapMode p
        , pgsIdentity   = fromWorldIdentityDTO <$> pc7Identity p
        }

-- | The v6 migration (#1230): decode the frozen v6 page cores into the
--   same base 'PageSnapshot' map. The ONLY difference is each location
--   instance's stored @discovery_margin@, which is DROPPED — the live
--   'Location.Instance.LocationInstance' has no such field any more,
--   because reveal became sight-based against the instance's own
--   bounds. Everything else about every instance rides across
--   untouched: its allocator, id, definition id, chunk, anchor, bounds,
--   display name, gloss, etymology source, lifecycle and
--   contents-spawned flag — as do the page's own identity, river names,
--   clocks, camera and map mode. A ruin a pre-#1230 save had already
--   discovered therefore stays discovered, and one it had not is
--   rediscovered by sight rather than by walking into a halo.
migrateWorldPagesV6 ∷ WorldPagesDTOv6 → WorldPages
migrateWorldPagesV6 (WorldPagesDTOv6 ps) = WorldPages
    { wpPageIds = map pc6PageId ps
    , wpBase    = HM.fromList [ (pc6PageId p, toBase p) | p ← ps ]
    }
  where
    toBase p = (blankPageSnapshot (pc6PageId p)
                    (fromWorldGenParamsDTOv5 (pc6GenParams p)))
        { pgsCameraX    = pc6CameraX p
        , pgsCameraY    = pc6CameraY p
        , pgsTimeHour   = pc6TimeHour p
        , pgsTimeMinute = pc6TimeMinute p
        , pgsDateYear   = pc6DateYear p
        , pgsDateMonth  = pc6DateMonth p
        , pgsDateDay    = pc6DateDay p
        , pgsMapMode    = pc6MapMode p
        , pgsIdentity   = fromWorldIdentityDTO <$> pc6Identity p
        }

-- | The v5 migration (#1104): decode the frozen v5 page cores into the
--   same base 'PageSnapshot' map. The ONLY difference is the optional
--   etymology source, which comes back ABSENT on all three of the
--   things that can carry one — the page's own identity, each location
--   instance, and each river name. A save written before #1104 recorded
--   no expressions, and one is never reconstructed after the fact from
--   a stored name, gloss, entity type, id, or content definition
--   (#1104 requirement 1). Those names and glosses themselves, the
--   page's language provenance, the location instances with their
--   lifecycles, the river-name table with its ids, clocks, camera, and
--   map mode all ride across untouched — so a pre-#1104 save keeps
--   every name it had and simply reports its etymology as unavailable.
migrateWorldPagesV5 ∷ WorldPagesDTOv5 → WorldPages
migrateWorldPagesV5 (WorldPagesDTOv5 ps) = WorldPages
    { wpPageIds = map pc5PageId ps
    , wpBase    = HM.fromList [ (pc5PageId p, toBase p) | p ← ps ]
    }
  where
    toBase p = (blankPageSnapshot (pc5PageId p)
                    (fromWorldGenParamsDTOv4 (pc5GenParams p)))
        { pgsCameraX    = pc5CameraX p
        , pgsCameraY    = pc5CameraY p
        , pgsTimeHour   = pc5TimeHour p
        , pgsTimeMinute = pc5TimeMinute p
        , pgsDateYear   = pc5DateYear p
        , pgsDateMonth  = pc5DateMonth p
        , pgsDateDay    = pc5DateDay p
        , pgsMapMode    = pc5MapMode p
        , pgsIdentity   = fromWorldIdentityDTOv2 <$> pc5Identity p
        }

-- | The v4 migration (#1102): decode the frozen v4 page cores into the
--   same base 'PageSnapshot' map. The ONLY difference is the per-page
--   river-name table, which comes back EMPTY
--   ('World.Save.Component.WorldGen.fromWorldGenParamsDTOv3'): a save
--   written before #1102 named no rivers, and a name is never inferred
--   after the fact for a page whose language it was not rendered from
--   (#1102 requirements 5 and 6). Its rivers still carry ids, which are
--   derived from the timeline the page already stores, so the identity
--   half of the feature works on a pre-#1102 save with no migration at
--   all. Everything else — identity with its provenance, location
--   instances with their stored names and glosses, clocks, camera, map
--   mode — rides across untouched.
migrateWorldPagesV4 ∷ WorldPagesDTOv4 → WorldPages
migrateWorldPagesV4 (WorldPagesDTOv4 ps) = WorldPages
    { wpPageIds = map pc4PageId ps
    , wpBase    = HM.fromList [ (pc4PageId p, toBase p) | p ← ps ]
    }
  where
    toBase p = (blankPageSnapshot (pc4PageId p)
                    (fromWorldGenParamsDTOv3 (pc4GenParams p)))
        { pgsCameraX    = pc4CameraX p
        , pgsCameraY    = pc4CameraY p
        , pgsTimeHour   = pc4TimeHour p
        , pgsTimeMinute = pc4TimeMinute p
        , pgsDateYear   = pc4DateYear p
        , pgsDateMonth  = pc4DateMonth p
        , pgsDateDay    = pc4DateDay p
        , pgsMapMode    = pc4MapMode p
        , pgsIdentity   = fromWorldIdentityDTOv2 <$> pc4Identity p
        }

-- | The v3 migration (#1101): decode the frozen v3 page cores into the
--   same base 'PageSnapshot' map. The ONLY difference is the per-page
--   LOCATION instances, whose stored display names carry across
--   EXACTLY — a location named before this landing keeps that name
--   forever (#1101 requirements 4 and 7), and is not renamed into the
--   world's language on upgrade — while each gains no gloss
--   ('World.Save.Component.WorldGen.fromLocationInstanceDTOv1'). The
--   page's own identity, provenance included, rides across untouched:
--   #1101 changed no world-identity field.
migrateWorldPagesV3 ∷ WorldPagesDTOv3 → WorldPages
migrateWorldPagesV3 (WorldPagesDTOv3 ps) = WorldPages
    { wpPageIds = map pc3PageId ps
    , wpBase    = HM.fromList [ (pc3PageId p, toBase p) | p ← ps ]
    }
  where
    toBase p = (blankPageSnapshot (pc3PageId p)
                    (fromWorldGenParamsDTOv2 (pc3GenParams p)))
        { pgsCameraX    = pc3CameraX p
        , pgsCameraY    = pc3CameraY p
        , pgsTimeHour   = pc3TimeHour p
        , pgsTimeMinute = pc3TimeMinute p
        , pgsDateYear   = pc3DateYear p
        , pgsDateMonth  = pc3DateMonth p
        , pgsDateDay    = pc3DateDay p
        , pgsMapMode    = pc3MapMode p
        , pgsIdentity   = fromWorldIdentityDTOv2 <$> pc3Identity p
        }

-- | The v2 migration (#1092): decode the frozen v2 page cores into
--   the same base 'PageSnapshot' map. The identity difference is the
--   headline — every v2 page's name and gloss carry across byte-exact
--   while its language provenance decodes ABSENT
--   (@fromWorldIdentityDTOv1@), never inferred from the world seed or
--   the name text. Its gen params are the frozen pre-#1101 shape, so
--   its location instances likewise keep their stored names and gain
--   no gloss. Clocks, camera, and map mode ride across untouched.
migrateWorldPagesV2 ∷ WorldPagesDTOv2 → WorldPages
migrateWorldPagesV2 (WorldPagesDTOv2 ps) = WorldPages
    { wpPageIds = map pc2PageId ps
    , wpBase    = HM.fromList [ (pc2PageId p, toBase p) | p ← ps ]
    }
  where
    toBase p = (blankPageSnapshot (pc2PageId p)
                    (fromWorldGenParamsDTOv2 (pc2GenParams p)))
        { pgsCameraX    = pc2CameraX p
        , pgsCameraY    = pc2CameraY p
        , pgsTimeHour   = pc2TimeHour p
        , pgsTimeMinute = pc2TimeMinute p
        , pgsDateYear   = pc2DateYear p
        , pgsDateMonth  = pc2DateMonth p
        , pgsDateDay    = pc2DateDay p
        , pgsMapMode    = pc2MapMode p
        , pgsIdentity   = fromWorldIdentityDTOv1 <$> pc2Identity p
        }

-- | The v1 migration (#911): decode the frozen v1 page cores into the
--   same base 'PageSnapshot' map, with each page's gen params rebuilt by
--   'fromWorldGenParamsDTOv1' — which leaves the instance table empty and
--   the page's old per-chunk discovered / contents-spawned sets PENDING
--   on it. Turning those into instances needs each definition's
--   bounds / label — since #1230 there is no margin to resolve, reveal
--   being sight against those bounds — and no component decoder has the
--   location registry, so the load path resolves them
--   ('Location.Instance.resolveLegacyLocationInstances') at its
--   content-validation stage before publication.
--   @wgpLocationStamped@ rides across untouched — it stays a chunk
--   property (#424). Its identity decodes with #1092's language
--   provenance absent as well: a pre-#911 save predates provenance
--   entirely.
migrateWorldPagesV1 ∷ WorldPagesDTOv1 → WorldPages
migrateWorldPagesV1 (WorldPagesDTOv1 ps) = WorldPages
    { wpPageIds = map pc1PageId ps
    , wpBase    = HM.fromList [ (pc1PageId p, toBase p) | p ← ps ]
    }
  where
    toBase p = (blankPageSnapshot (pc1PageId p)
                    (fromWorldGenParamsDTOv1 (pc1GenParams p)))
        { pgsCameraX    = pc1CameraX p
        , pgsCameraY    = pc1CameraY p
        , pgsTimeHour   = pc1TimeHour p
        , pgsTimeMinute = pc1TimeMinute p
        , pgsDateYear   = pc1DateYear p
        , pgsDateMonth  = pc1DateMonth p
        , pgsDateDay    = pc1DateDay p
        , pgsMapMode    = pc1MapMode p
        , pgsIdentity   = fromWorldIdentityDTOv1 <$> pc1Identity p
        }

-- | The zeroed base 'PageSnapshot' the v6, v5, v4, v3, v2, and v1 paths above
--   all build on, so they can never drift in which placeholder fields
--   they leave for the other components to fill. Each caller record-updates
--   the page-core scalars it decoded; everything left here is a
--   placeholder a REQUIRED component overwrites during assembly.
blankPageSnapshot ∷ WorldPageId → WorldGenParams → PageSnapshot
blankPageSnapshot pid params =
    PageSnapshot
        { pgsPageId       = pid
        , pgsGenParams    = params
        , pgsCameraX      = 0
        , pgsCameraY      = 0
        , pgsTimeHour     = 0
        , pgsTimeMinute   = 0
        , pgsDateYear     = 0
        , pgsDateMonth    = 0
        , pgsDateDay      = 0
        , pgsMapMode      = ZMDefault
        , pgsIdentity     = Nothing
        , pgsEdits        = emptyWorldEdits
        , pgsMineDesignations      = HM.empty
        , pgsConstructDesignations = HM.empty
        , pgsConstructNextAttempt  = firstConstructAttemptId
        , pgsGroundItems  = emptyGroundItems
        , pgsSpoilPiles   = emptySpoilPiles
        , pgsBuildings    = BuildingSnapshot { bsnInstances = HM.empty, bsnNextId = 0 }
        , pgsUnits        = UnitSnapshot { usnInstances = HM.empty, usnNextId = 0 }
        , pgsUnitSimStates = HM.empty
        , pgsFloraHarvests = emptyFloraHarvests
        , pgsChopDesignations = HM.empty
        , pgsPendingChopMigration = HM.empty
        , pgsPendingFloraHarvests = emptyPendingFloraHarvests
        , pgsPlantedFloraCursor = firstPlantedFloraCursor
        , pgsCraftBills   = emptyCraftBills
        , pgsPowerNodes   = emptyPowerNodes
          -- #1087: the FIRST of the two defaults genuinely reached in a
          -- successful load. @"container-knowledge"@ is the first
          -- OPTIONAL gameplay component, so a save written before it
          -- existed carries no such payload at all and every page keeps
          -- this empty map — which is exactly right: every container in
          -- a pre-#1087 session is never-inspected, never known-empty,
          -- and never inferred from its live contents.
        , pgsContainerKnowledge = emptyContainerKnowledge
          -- #1246: the SECOND default genuinely reached in a successful
          -- load, for the same reason. @"transfer-orders"@ is the second
          -- OPTIONAL gameplay component, so a save written before it
          -- existed carries no such payload and every page keeps this
          -- empty queue — which is exactly right: no order could have
          -- been queued in a session that had nowhere to store one, and
          -- the allocator starts where a fresh page's does.
        , pgsTransferOrders = emptyTransferOrders
        , pgsTillDesignations = HM.empty
        , pgsCropPlots    = emptyCropPlots
        , pgsPlantDesignations = HM.empty
        }

-- world-edits -------------------------------------------------------

data PageEditsDTO = PageEditsDTO
    { pedPageId ∷ !WorldPageId
    , pedEdits  ∷ !(HM.HashMap ChunkCoord [WorldEditDTO])
    , pedPlantedFloraCursor ∷ !Word64
      -- ^ #1854: this page's planted-flora id allocator cursor, saved
      --   beside the very edits whose ids it accounts for so the two
      --   can never be restored out of step.
    } deriving (Show, Generic, Serialize)

-- | The FROZEN @world-edits@ v1 page slice: the page id and its edit
--   log, with no allocator cursor and with every entry in the frozen
--   'WorldEditDTOv1' shape.
data PageEditsDTOv1 = PageEditsDTOv1
    { ped1PageId ∷ !WorldPageId
    , ped1Edits  ∷ !(HM.HashMap ChunkCoord [WorldEditDTOv1])
    } deriving (Show, Generic, Serialize)

newtype WorldEditsDTO = WorldEditsDTO { wedPages ∷ [PageEditsDTO] }
    deriving stock (Generic)
    deriving newtype (Show, Serialize)

-- | The FROZEN @world-edits@ v1 component payload.
newtype WorldEditsDTOv1 = WorldEditsDTOv1 { wed1Pages ∷ [PageEditsDTOv1] }
    deriving stock (Generic)
    deriving newtype (Show, Serialize)

-- | v1 → v2 (#1854): every edit crosses through
--   'migrateWorldEditDTOv1', and the cursor starts at the fresh-page
--   floor. Both the ids and the real cursor are established by
--   'applyWorldEdits', which is the only place that knows the page's
--   world size — see its note.
migrateWorldEditsV1 ∷ WorldEditsDTOv1 → WorldEditsDTO
migrateWorldEditsV1 (WorldEditsDTOv1 slices) = WorldEditsDTO
    [ PageEditsDTO
        { pedPageId = ped1PageId s
        , pedEdits  = HM.map (map migrateWorldEditDTOv1) (ped1Edits s)
        , pedPlantedFloraCursor = firstPlantedFloraCursor
        }
    | s ← slices ]

-- | Component-local invariant (#1854 requirement 5): a page's
--   planted-flora allocator cursor must be strictly above every planted
--   'FloraInstanceId' its own edit log carries, or planting after a load
--   would reissue an id that is already standing in the world. Modelled
--   on 'validateWorldActivity''s ground-item allocator clause.
--
--   GENERATED ids are deliberately not checked against it: they come
--   from a disjoint namespace ("World.Flora.Identity") that this
--   allocator does not own and can never collide with.
validateWorldEdits ∷ WorldEditsDTO → [ComponentError]
validateWorldEdits (WorldEditsDTO slices) =
    [ ComponentError worldEditsComponentId 2 ValidatePhase
        ("page '" <> tshow (pedPageId s) <> "': planted flora id "
         <> tshow (floraInstanceIdToLua iid) <> " is not below the page's \
            \planted-flora allocator ("
         <> tshow (pedPlantedFloraCursor s) <> ")")
    | s     ← slices
    , edits ← HM.elems (pedEdits s)
    , WePlaceFloraWithIdD _ _ _ _ _ iid ← edits
    , isPlantedFloraInstanceId iid
    , plantedFloraCursorAbove [iid] > pedPlantedFloraCursor s
    ]

worldEditsCodec ∷ ComponentCodec WorldEditsDTO
worldEditsCodec = componentCodec ComponentSpec
    { csComponent     = worldEditsComponentId
    , csVersion       = 2
    , csRequired      = True
    , csDeps          = [worldPagesComponentId]
    , csEncode        = \snap → WorldEditsDTO
        [ PageEditsDTO (pgsPageId p) (toEditsDTO (pgsEdits p))
                       (pgsPlantedFloraCursor p)
        | p ← orderedPages snap ]
    , csDecode        = id
    , csOlderVersions = [ atVersion 1 migrateWorldEditsV1 ]
    , csValidate      = validateWorldEdits
    }

-- | #1854: a v1 slice records no planted-flora ids, and the LEGACY
--   'WePlaceFlora' constructor that carries none is decode-only, so the
--   rewrite into the identity-bearing form happens HERE — the one place
--   that has both the edit log and the page's own world size, exactly as
--   'applyWorldActivity' owns #1175's canonical-frame repair for every
--   accepted version rather than splitting it between a migration and an
--   apply step.
--
--   It runs for EVERY accepted version, which is what makes it total: a
--   v2 slice's own ids and cursor are authoritative and nothing moves
--   (the rewrite touches only id-less entries, and the cursor only
--   grows), while a v1 slice gets both. Assignment is deterministic and
--   repeatable — chunk keys visited in ascending CANONICAL
--   chunk-coordinate order, never 'Data.HashMap.Strict' iteration order,
--   and each chunk's log in its own stored oldest-first order — so
--   migrating the same save twice produces the same ids.
applyWorldEdits
    ∷ Word32 → WorldEditsDTO → HM.HashMap WorldPageId PageSnapshot
    → Either [ComponentError] (HM.HashMap WorldPageId PageSnapshot)
applyWorldEdits ver (WorldEditsDTO slices) =
    applyPageSlices worldEditsComponentId ver pedPageId writeEdits slices
  where
    writeEdits s p =
        let decoded   = fromEditsDTO (pedEdits s)
            worldSize = wgpWorldSize (pgsGenParams p)
            -- A v1 payload has no cursor field at all; its migration
            -- supplies the fresh-page floor, which is where assignment
            -- must start.
            seed = if ver ≥ 2 then pedPlantedFloraCursor s
                              else firstPlantedFloraCursor
            (edits, cursor) = assignPlantedIds worldSize seed decoded
        in p { pgsEdits = edits, pgsPlantedFloraCursor = cursor }

-- | Rewrite every id-less 'WePlaceFlora' into 'WePlaceFloraWithId',
--   allocating from @seed@ upward, and return the log beside a cursor
--   that sits strictly above every planted id it now carries.
assignPlantedIds ∷ Int → Word64 → WorldEdits → (WorldEdits, Word64)
assignPlantedIds worldSize seed edits =
    let ordered = L.sortOn (chunkOrderKey worldSize . fst) (HM.toList edits)
        (rebuilt, cursor) = L.foldl' perChunk ([], max firstPlantedFloraCursor seed)
                                     ordered
        allocated = [ iid | (_, es) ← rebuilt, WePlaceFloraWithId _ _ _ _ _ iid ← es ]
    in ( HM.fromList (reverse rebuilt)
       , max cursor (plantedFloraCursorAbove allocated) )
  where
    perChunk (acc, cursor) (coord, es) =
        let (es', cursor') = L.foldl' perEdit ([], cursor) es
        in ((coord, reverse es') : acc, cursor')
    perEdit (acc, cursor) e = case e of
        WePlaceFlora gx gy fid day w →
            let (iid, cursor') = nextPlantedFloraCursor cursor
            in (WePlaceFloraWithId gx gy fid day w iid : acc, cursor')
        WePlaceFloraWithId _ _ _ _ _ iid
            | isFloraInstanceIdNone iid →
                let (iid', cursor') = nextPlantedFloraCursor cursor
                in (reId e iid' : acc, cursor')
        _ → (e : acc, cursor)
    reId (WePlaceFloraWithId gx gy fid day w _) iid =
        WePlaceFloraWithId gx gy fid day w iid
    reId e _ = e
    -- Canonical coordinate FIRST, then the raw key as a tie-break. The
    -- canonical key alone is not a total order over this map: near the
    -- seam two distinct alias keys canonicalize to the same chunk (which
    -- is exactly the case 'World.Edit.Types.canonicalizeWorldEdits'
    -- exists to merge), and 'L.sortOn' is stable — so their relative
    -- order would have fallen back to 'HM.toList''s, making the ids this
    -- assignment hands out depend on hashmap traversal. Ordering the
    -- alias before its canonical twin also matches canonicalizeWorldEdits'
    -- own merge order, so the two agree about which entry came first.
    chunkOrderKey ws coord =
        let ChunkCoord cx cy = wrapChunkCoordU ws coord
            ChunkCoord rx ry = coord
        in ((cx, cy), (cx, cy) ≡ (rx, ry), (rx, ry))

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
    , pad4Plant           ∷ !(HM.HashMap (Int, Int) PlantDesignationDTO)
    , pad4FloraHarvests   ∷ !(HM.HashMap FloraInstanceId Float)
    , pad4CropPlots       ∷ !(HM.HashMap (Int, Int) CropPlotDTO)
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
    , pad3Plant         ∷ !(HM.HashMap (Int, Int) PlantDesignationDTO)
    , pad3FloraHarvests ∷ !FloraHarvestsDTOv1
    , pad3CropPlots     ∷ !(HM.HashMap (Int, Int) CropPlotDTO)
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
    , pad2Plant         ∷ !(HM.HashMap (Int, Int) PlantDesignationDTO)
    , pad2FloraHarvests ∷ !FloraHarvestsDTOv1
    , pad2CropPlots     ∷ !(HM.HashMap (Int, Int) CropPlotDTO)
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
migratePageActivityV4 ∷ PageActivityDTOv4 → PageActivityDTO
migratePageActivityV4 s =
    let (construct, next) = migrateConstructDesignations (pad4Construct s)
    in PageActivityDTO
        { padPageId               = pad4PageId s
        , padMine                 = pad4Mine s
        , padConstruct            = construct
        , padChop                 = pad4Chop s
        , padTill                 = pad4Till s
        , padPlant                = pad4Plant s
        , padFloraHarvests        = pad4FloraHarvests s
        , padCropPlots            = pad4CropPlots s
        , padGroundItems          = pad4GroundItems s
        , padSpoilPiles           = pad4SpoilPiles s
        , padPendingChop          = pad4PendingChop s
        , padPendingHarvests      = pad4PendingHarvests s
        , padConstructNextAttempt = next
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
        (map (migratePageActivityV4 . migratePageActivityV3
                                    . migratePageActivityV2)
             slices)

-- | v3 → v5: see 'migratePageActivityV3' and 'migratePageActivityV4'.
migrateWorldActivityV3 ∷ WorldActivityDTOv3 → WorldActivityDTO
migrateWorldActivityV3 (WorldActivityDTOv3 slices) =
    WorldActivityDTO (map (migratePageActivityV4 . migratePageActivityV3)
                          slices)

-- | v4 → v5: see 'migratePageActivityV4'.
migrateWorldActivityV4 ∷ WorldActivityDTOv4 → WorldActivityDTO
migrateWorldActivityV4 (WorldActivityDTOv4 slices) =
    WorldActivityDTO (map migratePageActivityV4 slices)

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
    , csVersion       = 5
    , csRequired      = True
    , csDeps          = [worldPagesComponentId]
    , csEncode        = \snap →
        WorldActivityDTO (map toActivity (orderedPages snap))
    , csDecode        = id
    , csOlderVersions = [ atVersion 4 migrateWorldActivityV4
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
