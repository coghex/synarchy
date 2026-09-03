{-# LANGUAGE Strict, DeriveGeneric, DeriveAnyClass, DerivingStrategies #-}
-- | The per-page ENTITY INSTANCE snapshots: the @"buildings"@ and
--   @"units"@ save components (issue #760, save-overhaul B2; extracted
--   from "World.Save.Component.Entities" by #2150).
--
--   - @"buildings"@ (required) — per page: the building instances +
--     their delivered materials / storage / build progress. Owner:
--     'Building.Types.BuildingManager'. Depends on @"world-pages"@ +
--     @"core-session"@ (the global 'BuildingId' allocator).
--   - @"units"@ (required) — per page: the unit instances (stats, skills,
--     modifiers, equipment, inventory, wounds, scars, immunity, blood).
--     Owner: 'Unit.Types.UnitManager'. Depends on @"world-pages"@ +
--     @"core-session"@ (the global 'UnitId' allocator).
--
--   What lives here is the WHOLE contract of those two components: the
--   current DTOs, the frozen historical ones, the conversions between
--   them and the live snapshots, the v1 migrations, the two codecs and
--   the two assembly functions. Nothing else in the entity family reads
--   any of it, which is why it is a module and not a section — the
--   simulation ("World.Save.Component.EntitySimulation") and attached
--   system ("World.Save.Component.EntitySystems") owners share only the
--   component machinery and "World.Save.PageOrder"'s page ordering.
--
--   Requirement 4 — the on-disk contract is FROZEN, distinct from every
--   mutable runtime record. EVERY evolving live gameplay record reachable
--   from a component DTO is mirrored by a component-owned DTO with an
--   explicit, reviewable field-by-field conversion ('to…'/'from…'); none
--   is embedded directly. The per-page ENTITY snapshots are frozen: the
--   @"buildings"@/@"units"@ components carry 'BuildingInstanceDTO' /
--   'UnitInstanceDTO', NOT the "World.Save.Types" positional
--   'BuildingInstanceSnapshot'/'UnitInstanceSnapshot'. Those snapshots
--   themselves directly carry mutable 'Item.Types.ItemInstance' values
--   (materials delivered / storage / inventory / equipped / accessories)
--   and, on units, the live 'StatModifier'/'Wound'/'Scar' records — so a
--   v1 @"buildings"@/@"units"@ payload could still drift from an
--   unrelated change to any of those without the component's OWN version
--   dispatch noticing. The transitive freeze closes that: the
--   'Item.Types.ItemInstance' fields reuse
--   "World.Save.Component.Page"'s shared 'ItemInstanceDTO';
--   'StatModifier'/'Wound'/'Scar' get 'StatModifierDTO'/'WoundDTO'/
--   'ScarDTO' here. Leaf enums ('Unit.Sim.Types.Pose' and friends) and
--   durable id newtypes ('BuildingId'/'UnitId') are reused as-is —
--   append-only content references with no independent mutable identity
--   (boundary rule leaf clause (a), see "World.Save.Component.Types").
--   The frozen DTOs mirror the originals' exact field order and leaf
--   types, so the derived cereal layout was byte-identical to the earlier
--   direct embedding and the components stayed at schema v1 through that
--   freeze (verified by the frozen tracked fixture in
--   "Test.Headless.World.Save.Components"). Both have since moved to
--   v2 for a real SHAPE change: #1233 appended the recursive item tree's
--   physical values, so the pre-#1233 trees are frozen as
--   'BuildingInstanceDTOv1'/'UnitInstanceDTOv1' and the shared item DTO
--   carries them (see "World.Save.Component.Page").
--
--   The @"buildings"@/@"units"@ components carry ONLY the per-page
--   instance maps — NOT the per-page @bsnNextId@/@usnNextId@ counters.
--   The building- and unit-id allocators are global, owned once by the
--   @"core-session"@ component (requirement 9: "allocator components
--   remain above all governed ids"), so a page slice must not carry a
--   duplicate copy. On assembly the per-page 'BuildingSnapshot'/
--   'UnitSnapshot' is reconstructed with @bsnNextId@/@usnNextId@ filled
--   from that single global allocator.
module World.Save.Component.EntitySnapshots
    ( buildingsCodec
    , PageBuildingsDTO(..)
    , BuildingsDTO(..)
    , BuildingInstanceDTO(..)
    , fromBuildingInstanceDTO
    , BuildingInstanceDTOv1(..)
    , PageBuildingsDTOv1(..)
    , BuildingsDTOv1(..)
    , toBuildingInstanceDTOv1
    , migrateBuildingsDTOv1
    , unitsCodec
    , PageUnitsDTO(..)
    , UnitsDTO(..)
    , UnitInstanceDTO(..)
    , StatModifierDTO(..)
    , WoundDTO(..)
    , ScarDTO(..)
    , toUnitInstanceDTO
    , fromUnitInstanceDTO
    , UnitInstanceDTOv1(..)
    , PageUnitsDTOv1(..)
    , UnitsDTOv1(..)
    , toUnitInstanceDTOv1
    , migrateUnitsDTOv1
    , applyBuildings
    , applyUnits
    ) where

import UPrelude
import qualified Data.HashMap.Strict as HM
import Data.Serialize (Serialize)
import GHC.Generics (Generic)
import World.Page.Types (WorldPageId)
import Building.Types (BuildingId)
import Unit.Types (UnitId, StatModifier(..), Wound(..), Scar(..))
import Unit.Sim.Types (Direction)
import World.Save.Types
    ( BuildingSnapshot(..), BuildingInstanceSnapshot(..)
    , UnitSnapshot(..), UnitInstanceSnapshot(..) )
import World.Save.Component.Page
    ( ItemInstanceDTO(..), toItemInstanceDTO, fromItemInstanceDTO
    , ItemInstanceDTOv1, toItemInstanceDTOv1, migrateItemInstanceDTOv1 )
import World.Save.Snapshot (PageSnapshot(..))
import World.Save.Component.Types
import World.Save.PageOrder (orderedPages)

-- buildings ---------------------------------------------------------

-- | Frozen mirror of 'BuildingInstanceSnapshot'. That
--   positional "World.Save.Types" snapshot directly carries mutable
--   'ItemInstance' values ('bisMaterialsDelivered'/'bisStorage'), so
--   embedding it would let an unrelated 'ItemInstance' change drift a v1
--   @"buildings"@ payload without the component's own version dispatch
--   noticing — the exact gap the frozen-DTO boundary rule
--   ("World.Save.Component.Types") forbids. The two item fields reuse the
--   shared 'ItemInstanceDTO' (frozen recursively there); every other
--   field is a leaf scalar/'Text'. Field order mirrors
--   'BuildingInstanceSnapshot' exactly, so the derived cereal bytes were
--   unchanged from the earlier direct embedding. This is the CURRENT (v2)
--   shape; the pre-#1233 one is 'BuildingInstanceDTOv1' below.
data BuildingInstanceDTO = BuildingInstanceDTO
    { bidDefName            ∷ !Text
    , bidAnchorX            ∷ !Int
    , bidAnchorY            ∷ !Int
    , bidGridZ              ∷ !Int
    , bidSpawnedAt          ∷ !Double
    , bidTileW              ∷ !Int
    , bidTileH              ∷ !Int
    , bidSpawnRemaining     ∷ !Int
    , bidBuildProgress      ∷ !Float
    , bidMaterialsDelivered ∷ !(HM.HashMap Text [ItemInstanceDTO])
    , bidStorage            ∷ ![ItemInstanceDTO]
    } deriving (Show, Eq, Generic, Serialize)

toBuildingInstanceDTO ∷ BuildingInstanceSnapshot → BuildingInstanceDTO
toBuildingInstanceDTO b = BuildingInstanceDTO
    { bidDefName            = bisDefName b
    , bidAnchorX            = bisAnchorX b
    , bidAnchorY            = bisAnchorY b
    , bidGridZ              = bisGridZ b
    , bidSpawnedAt          = bisSpawnedAt b
    , bidTileW              = bisTileW b
    , bidTileH              = bisTileH b
    , bidSpawnRemaining     = bisSpawnRemaining b
    , bidBuildProgress      = bisBuildProgress b
    , bidMaterialsDelivered =
        HM.map (map toItemInstanceDTO) (bisMaterialsDelivered b)
    , bidStorage            = map toItemInstanceDTO (bisStorage b)
    }

fromBuildingInstanceDTO ∷ BuildingInstanceDTO → BuildingInstanceSnapshot
fromBuildingInstanceDTO d = BuildingInstanceSnapshot
    { bisDefName            = bidDefName d
    , bisAnchorX            = bidAnchorX d
    , bisAnchorY            = bidAnchorY d
    , bisGridZ              = bidGridZ d
    , bisSpawnedAt          = bidSpawnedAt d
    , bisTileW              = bidTileW d
    , bisTileH              = bidTileH d
    , bisSpawnRemaining     = bidSpawnRemaining d
    , bisBuildProgress      = bidBuildProgress d
    , bisMaterialsDelivered =
        HM.map (map fromItemInstanceDTO) (bidMaterialsDelivered d)
    , bisStorage            = map fromItemInstanceDTO (bidStorage d)
    }

-- | The FROZEN v1 building instance (#1233), preserved verbatim for
--   decode-only backward compatibility: identical to the current DTO but
--   for the item shape its two item fields carry
--   ('ItemInstanceDTOv1' — the pre-#1233 recursive tree). Never edited; a
--   further building schema change freezes the CURRENT shape as
--   'BuildingInstanceDTOv2' rather than touching this one.
data BuildingInstanceDTOv1 = BuildingInstanceDTOv1
    { bid1DefName            ∷ !Text
    , bid1AnchorX            ∷ !Int
    , bid1AnchorY            ∷ !Int
    , bid1GridZ              ∷ !Int
    , bid1SpawnedAt          ∷ !Double
    , bid1TileW              ∷ !Int
    , bid1TileH              ∷ !Int
    , bid1SpawnRemaining     ∷ !Int
    , bid1BuildProgress      ∷ !Float
    , bid1MaterialsDelivered ∷ !(HM.HashMap Text [ItemInstanceDTOv1])
    , bid1Storage            ∷ ![ItemInstanceDTOv1]
    } deriving (Show, Eq, Generic, Serialize)

-- | Encoder for the frozen shape — the round-trip partner a v1 fixture
--   and a migration test are built with.
toBuildingInstanceDTOv1 ∷ BuildingInstanceSnapshot → BuildingInstanceDTOv1
toBuildingInstanceDTOv1 b = BuildingInstanceDTOv1
    { bid1DefName            = bisDefName b
    , bid1AnchorX            = bisAnchorX b
    , bid1AnchorY            = bisAnchorY b
    , bid1GridZ              = bisGridZ b
    , bid1SpawnedAt          = bisSpawnedAt b
    , bid1TileW              = bisTileW b
    , bid1TileH              = bisTileH b
    , bid1SpawnRemaining     = bisSpawnRemaining b
    , bid1BuildProgress      = bisBuildProgress b
    , bid1MaterialsDelivered =
        HM.map (map toItemInstanceDTOv1) (bisMaterialsDelivered b)
    , bid1Storage            = map toItemInstanceDTOv1 (bisStorage b)
    }

-- | v1 → v2: every non-item field crosses unchanged; both item
--   collections migrate through 'migrateItemInstanceDTOv1', so a stored
--   crate and a delivered plank alike decode with their physical values
--   ABSENT (that function documents why absence, not a fabricated zero
--   or a re-derivation from the current definition).
migrateBuildingInstanceDTOv1 ∷ BuildingInstanceDTOv1 → BuildingInstanceDTO
migrateBuildingInstanceDTOv1 d = BuildingInstanceDTO
    { bidDefName            = bid1DefName d
    , bidAnchorX            = bid1AnchorX d
    , bidAnchorY            = bid1AnchorY d
    , bidGridZ              = bid1GridZ d
    , bidSpawnedAt          = bid1SpawnedAt d
    , bidTileW              = bid1TileW d
    , bidTileH              = bid1TileH d
    , bidSpawnRemaining     = bid1SpawnRemaining d
    , bidBuildProgress      = bid1BuildProgress d
    , bidMaterialsDelivered =
        HM.map (map migrateItemInstanceDTOv1) (bid1MaterialsDelivered d)
    , bidStorage            = map migrateItemInstanceDTOv1 (bid1Storage d)
    }

-- | Per-page building slice. Carries ONLY the instance map — the
--   building-id allocator (@bsnNextId@) is deliberately absent, since it
--   is a global counter owned once by @"core-session"@ (requirement 9).
--   Each instance is the frozen 'BuildingInstanceDTO', not the live-item-
--   carrying "World.Save.Types" snapshot (see 'BuildingInstanceDTO').
data PageBuildingsDTO = PageBuildingsDTO
    { pbPageId    ∷ !WorldPageId
    , pbInstances ∷ !(HM.HashMap BuildingId BuildingInstanceDTO)
    } deriving (Show, Eq, Generic, Serialize)

newtype BuildingsDTO = BuildingsDTO { bdPages ∷ [PageBuildingsDTO] }
    deriving stock (Generic)
    deriving newtype (Show, Eq, Serialize)

-- | The FROZEN v1 page slice (#1233), carrying the frozen v1 instances.
data PageBuildingsDTOv1 = PageBuildingsDTOv1
    { pb1PageId    ∷ !WorldPageId
    , pb1Instances ∷ !(HM.HashMap BuildingId BuildingInstanceDTOv1)
    } deriving (Show, Eq, Generic, Serialize)

newtype BuildingsDTOv1 = BuildingsDTOv1 { bd1Pages ∷ [PageBuildingsDTOv1] }
    deriving stock (Generic)
    deriving newtype (Show, Eq, Serialize)

migrateBuildingsDTOv1 ∷ BuildingsDTOv1 → BuildingsDTO
migrateBuildingsDTOv1 (BuildingsDTOv1 slices) = BuildingsDTO
    [ PageBuildingsDTO (pb1PageId s)
          (HM.map migrateBuildingInstanceDTOv1 (pb1Instances s))
    | s ← slices ]

-- Depends on @"core-session"@ too: assembly refills each page's
-- @bsnNextId@ from the GLOBAL building-id allocator that @"core-session"@
-- installs, so it must fold first (requirement 9).
--
-- v2 (#1233): a building's delivered materials and loose storage carry
-- the physical values #1233 appended to the recursive item tree, so the
-- shape changed and v1 decodes through its own frozen tree.
buildingsCodec ∷ ComponentCodec BuildingsDTO
buildingsCodec = componentCodec ComponentSpec
    { csComponent     = buildingsComponentId
    , csVersion       = 2
    , csRequired      = True
    , csDeps          = [worldPagesComponentId, coreSessionComponentId]
    , csEncode        = \snap → BuildingsDTO
        [ PageBuildingsDTO (pgsPageId p)
              (HM.map toBuildingInstanceDTO (bsnInstances (pgsBuildings p)))
        | p ← orderedPages snap ]
    , csDecode        = id
    , csOlderVersions = [ atVersion 1 migrateBuildingsDTOv1 ]
    , csValidate      = const []
    }

-- | Reconstruct each page's 'BuildingSnapshot' from its instance slice,
--   filling @bsnNextId@ from the ONE global building-id allocator
--   (@snapNextBuildingId@, threaded in from @"core-session"@) rather than
--   from a per-page copy the wire no longer carries. @ver@ is the
--   component's real encoded version (for accurate page-mismatch errors,
--   requirement 6); @nextId@ is the global building-id allocator.
applyBuildings
    ∷ Word32 → Word32 → BuildingsDTO → HM.HashMap WorldPageId PageSnapshot
    → Either [ComponentError] (HM.HashMap WorldPageId PageSnapshot)
applyBuildings ver nextId (BuildingsDTO slices) =
    applyPageSlices buildingsComponentId ver pbPageId
        (\s p → p { pgsBuildings = BuildingSnapshot
                        { bsnInstances =
                            HM.map fromBuildingInstanceDTO (pbInstances s)
                        , bsnNextId = nextId } })
        slices

-- units -------------------------------------------------------------

-- | Frozen mirror of 'StatModifier' (a live "Unit.Types" record mutated
--   in place by the stat system; its own append-only comment warns fields
--   go at the end). Every field is a leaf scalar/'Text'/'Maybe'.
data StatModifierDTO = StatModifierDTO
    { smdDelta   ∷ !Float
    , smdSource  ∷ !Text
    , smdExpiry  ∷ !(Maybe Double)
    , smdPercent ∷ !Float
    } deriving (Show, Eq, Generic, Serialize)

toStatModifierDTO ∷ StatModifier → StatModifierDTO
toStatModifierDTO m = StatModifierDTO
    { smdDelta = smDelta m, smdSource = smSource m
    , smdExpiry = smExpiry m, smdPercent = smPercent m }

fromStatModifierDTO ∷ StatModifierDTO → StatModifier
fromStatModifierDTO d = StatModifier
    { smDelta = smdDelta d, smSource = smdSource d
    , smExpiry = smdExpiry d, smPercent = smdPercent d }

-- | Frozen mirror of 'Wound' (a live "Unit.Types" record the combat/wound
--   tick mutates and grows fields on across saves). Every field is a leaf
--   scalar/'Text'/'Bool'; field order mirrors 'Wound' exactly.
data WoundDTO = WoundDTO
    { wdPart          ∷ !Text
    , wdKind          ∷ !Text
    , wdSeverity      ∷ !Float
    , wdAt            ∷ !Double
    , wdBandage       ∷ !Float
    , wdClot          ∷ !Float
    , wdHeal          ∷ !Float
    , wdDressing      ∷ !Text
    , wdInfection     ∷ !Float
    , wdClean         ∷ !Bool
    , wdInfectionType ∷ !Text
    , wdNecrosis      ∷ !Float
    } deriving (Show, Eq, Generic, Serialize)

toWoundDTO ∷ Wound → WoundDTO
toWoundDTO w = WoundDTO
    { wdPart          = woundPart w
    , wdKind          = woundKind w
    , wdSeverity      = woundSeverity w
    , wdAt            = woundAt w
    , wdBandage       = woundBandage w
    , wdClot          = woundClot w
    , wdHeal          = woundHeal w
    , wdDressing      = woundDressing w
    , wdInfection     = woundInfection w
    , wdClean         = woundClean w
    , wdInfectionType = woundInfectionType w
    , wdNecrosis      = woundNecrosis w
    }

fromWoundDTO ∷ WoundDTO → Wound
fromWoundDTO d = Wound
    { woundPart          = wdPart d
    , woundKind          = wdKind d
    , woundSeverity      = wdSeverity d
    , woundAt            = wdAt d
    , woundBandage       = wdBandage d
    , woundClot          = wdClot d
    , woundHeal          = wdHeal d
    , woundDressing      = wdDressing d
    , woundInfection     = wdInfection d
    , woundClean         = wdClean d
    , woundInfectionType = wdInfectionType d
    , woundNecrosis      = wdNecrosis d
    }

-- | Frozen mirror of 'Scar' (a live "Unit.Types" record). Leaf fields.
data ScarDTO = ScarDTO
    { scdPart     ∷ !Text
    , scdKind     ∷ !Text
    , scdSeverity ∷ !Float
    , scdAt       ∷ !Double
    } deriving (Show, Eq, Generic, Serialize)

toScarDTO ∷ Scar → ScarDTO
toScarDTO s = ScarDTO
    { scdPart = scarPart s, scdKind = scarKind s
    , scdSeverity = scarSeverity s, scdAt = scarAt s }

fromScarDTO ∷ ScarDTO → Scar
fromScarDTO d = Scar
    { scarPart = scdPart d, scarKind = scdKind d
    , scarSeverity = scdSeverity d, scarAt = scdAt d }

-- | Frozen mirror of 'UnitInstanceSnapshot'. Like
--   'BuildingInstanceDTO', that positional "World.Save.Types" snapshot
--   directly carries mutable 'ItemInstance' values
--   ('uisInventory'/'uisEquipped'/'uisAccessories') AND the live
--   'StatModifier'/'Wound'/'Scar' records ('uisModifiers'/'uisWounds'/
--   'uisScars'), any of which could drift a v1 @"units"@ payload without
--   the component's own version dispatch noticing. Each is frozen: items
--   via the shared 'ItemInstanceDTO', the three unit records via
--   'StatModifierDTO'/'WoundDTO'/'ScarDTO' above. 'Direction' is an
--   append-only leaf enum, reused as-is. Field order + leaf types mirror
--   'UnitInstanceSnapshot' exactly, so the derived cereal bytes were
--   unchanged from the earlier direct embedding. This is the CURRENT (v2)
--   shape; the pre-#1233 one is 'UnitInstanceDTOv1' below.
data UnitInstanceDTO = UnitInstanceDTO
    { uidDefName        ∷ !Text
    , uidBaseWidth      ∷ !Float
    , uidGridX          ∷ !Float
    , uidGridY          ∷ !Float
    , uidGridZ          ∷ !Int
    , uidFacing         ∷ !Direction
    , uidCurrentAnim    ∷ !Text
    , uidAnimStart      ∷ !Double
    , uidAnimReverse    ∷ !Bool
    , uidActivity       ∷ !Text
    , uidPose           ∷ !Text
    , uidAnimStride     ∷ !Int
    , uidStats          ∷ !(HM.HashMap Text Float)
    , uidModifiers      ∷ !(HM.HashMap Text [StatModifierDTO])
    , uidSkills         ∷ !(HM.HashMap Text Float)
    , uidKnowledge      ∷ !(HM.HashMap Text Float)
    , uidInventory      ∷ ![ItemInstanceDTO]
    , uidEquipped       ∷ !(HM.HashMap Text ItemInstanceDTO)
    , uidAccessories    ∷ ![ItemInstanceDTO]
    , uidFactionId      ∷ !Text
    , uidWounds         ∷ ![WoundDTO]
    , uidScars          ∷ ![ScarDTO]
    , uidImmuneResponse ∷ !Float
    , uidImmunities     ∷ !(HM.HashMap Text Float)
    , uidBlood          ∷ !Float
    , uidName           ∷ !Text
    } deriving (Show, Eq, Generic, Serialize)

toUnitInstanceDTO ∷ UnitInstanceSnapshot → UnitInstanceDTO
toUnitInstanceDTO u = UnitInstanceDTO
    { uidDefName        = uisDefName u
    , uidBaseWidth      = uisBaseWidth u
    , uidGridX          = uisGridX u
    , uidGridY          = uisGridY u
    , uidGridZ          = uisGridZ u
    , uidFacing         = uisFacing u
    , uidCurrentAnim    = uisCurrentAnim u
    , uidAnimStart      = uisAnimStart u
    , uidAnimReverse    = uisAnimReverse u
    , uidActivity       = uisActivity u
    , uidPose           = uisPose u
    , uidAnimStride     = uisAnimStride u
    , uidStats          = uisStats u
    , uidModifiers      = HM.map (map toStatModifierDTO) (uisModifiers u)
    , uidSkills         = uisSkills u
    , uidKnowledge      = uisKnowledge u
    , uidInventory      = map toItemInstanceDTO (uisInventory u)
    , uidEquipped       = HM.map toItemInstanceDTO (uisEquipped u)
    , uidAccessories    = map toItemInstanceDTO (uisAccessories u)
    , uidFactionId      = uisFactionId u
    , uidWounds         = map toWoundDTO (uisWounds u)
    , uidScars          = map toScarDTO (uisScars u)
    , uidImmuneResponse = uisImmuneResponse u
    , uidImmunities     = uisImmunities u
    , uidBlood          = uisBlood u
    , uidName           = uisName u
    }

fromUnitInstanceDTO ∷ UnitInstanceDTO → UnitInstanceSnapshot
fromUnitInstanceDTO d = UnitInstanceSnapshot
    { uisDefName        = uidDefName d
    , uisBaseWidth      = uidBaseWidth d
    , uisGridX          = uidGridX d
    , uisGridY          = uidGridY d
    , uisGridZ          = uidGridZ d
    , uisFacing         = uidFacing d
    , uisCurrentAnim    = uidCurrentAnim d
    , uisAnimStart      = uidAnimStart d
    , uisAnimReverse    = uidAnimReverse d
    , uisActivity       = uidActivity d
    , uisPose           = uidPose d
    , uisAnimStride     = uidAnimStride d
    , uisStats          = uidStats d
    , uisModifiers      = HM.map (map fromStatModifierDTO) (uidModifiers d)
    , uisSkills         = uidSkills d
    , uisKnowledge      = uidKnowledge d
    , uisInventory      = map fromItemInstanceDTO (uidInventory d)
    , uisEquipped       = HM.map fromItemInstanceDTO (uidEquipped d)
    , uisAccessories    = map fromItemInstanceDTO (uidAccessories d)
    , uisFactionId      = uidFactionId d
    , uisWounds         = map fromWoundDTO (uidWounds d)
    , uisScars          = map fromScarDTO (uidScars d)
    , uisImmuneResponse = uidImmuneResponse d
    , uisImmunities     = uidImmunities d
    , uisBlood          = uidBlood d
    , uisName           = uidName d
    }

-- | The FROZEN pre-#1233 unit instance, preserved verbatim for
--   decode-only backward compatibility: identical to the current DTO but
--   for the item shape its three item fields carry
--   ('ItemInstanceDTOv1'). Never edited; a further unit schema change
--   freezes the CURRENT shape as 'UnitInstanceDTOv2' rather than touching
--   this one.
data UnitInstanceDTOv1 = UnitInstanceDTOv1
    { uid1DefName        ∷ !Text
    , uid1BaseWidth      ∷ !Float
    , uid1GridX          ∷ !Float
    , uid1GridY          ∷ !Float
    , uid1GridZ          ∷ !Int
    , uid1Facing         ∷ !Direction
    , uid1CurrentAnim    ∷ !Text
    , uid1AnimStart      ∷ !Double
    , uid1AnimReverse    ∷ !Bool
    , uid1Activity       ∷ !Text
    , uid1Pose           ∷ !Text
    , uid1AnimStride     ∷ !Int
    , uid1Stats          ∷ !(HM.HashMap Text Float)
    , uid1Modifiers      ∷ !(HM.HashMap Text [StatModifierDTO])
    , uid1Skills         ∷ !(HM.HashMap Text Float)
    , uid1Knowledge      ∷ !(HM.HashMap Text Float)
    , uid1Inventory      ∷ ![ItemInstanceDTOv1]
    , uid1Equipped       ∷ !(HM.HashMap Text ItemInstanceDTOv1)
    , uid1Accessories    ∷ ![ItemInstanceDTOv1]
    , uid1FactionId      ∷ !Text
    , uid1Wounds         ∷ ![WoundDTO]
    , uid1Scars          ∷ ![ScarDTO]
    , uid1ImmuneResponse ∷ !Float
    , uid1Immunities     ∷ !(HM.HashMap Text Float)
    , uid1Blood          ∷ !Float
    , uid1Name           ∷ !Text
    } deriving (Show, Eq, Generic, Serialize)

-- | Encoder for the frozen shape — the round-trip partner a v1 fixture
--   and a migration test are built with.
toUnitInstanceDTOv1 ∷ UnitInstanceSnapshot → UnitInstanceDTOv1
toUnitInstanceDTOv1 u = UnitInstanceDTOv1
    { uid1DefName        = uisDefName u
    , uid1BaseWidth      = uisBaseWidth u
    , uid1GridX          = uisGridX u
    , uid1GridY          = uisGridY u
    , uid1GridZ          = uisGridZ u
    , uid1Facing         = uisFacing u
    , uid1CurrentAnim    = uisCurrentAnim u
    , uid1AnimStart      = uisAnimStart u
    , uid1AnimReverse    = uisAnimReverse u
    , uid1Activity       = uisActivity u
    , uid1Pose           = uisPose u
    , uid1AnimStride     = uisAnimStride u
    , uid1Stats          = uisStats u
    , uid1Modifiers      = HM.map (map toStatModifierDTO) (uisModifiers u)
    , uid1Skills         = uisSkills u
    , uid1Knowledge      = uisKnowledge u
    , uid1Inventory      = map toItemInstanceDTOv1 (uisInventory u)
    , uid1Equipped       = HM.map toItemInstanceDTOv1 (uisEquipped u)
    , uid1Accessories    = map toItemInstanceDTOv1 (uisAccessories u)
    , uid1FactionId      = uisFactionId u
    , uid1Wounds         = map toWoundDTO (uisWounds u)
    , uid1Scars          = map toScarDTO (uisScars u)
    , uid1ImmuneResponse = uisImmuneResponse u
    , uid1Immunities     = uisImmunities u
    , uid1Blood          = uisBlood u
    , uid1Name           = uisName u
    }

-- | v1 → v2: every non-item field crosses unchanged; the inventory,
--   equipment and accessory items each migrate through
--   'migrateItemInstanceDTOv1' (physical values decode absent).
migrateUnitInstanceDTOv1 ∷ UnitInstanceDTOv1 → UnitInstanceDTO
migrateUnitInstanceDTOv1 d = UnitInstanceDTO
    { uidDefName        = uid1DefName d
    , uidBaseWidth      = uid1BaseWidth d
    , uidGridX          = uid1GridX d
    , uidGridY          = uid1GridY d
    , uidGridZ          = uid1GridZ d
    , uidFacing         = uid1Facing d
    , uidCurrentAnim    = uid1CurrentAnim d
    , uidAnimStart      = uid1AnimStart d
    , uidAnimReverse    = uid1AnimReverse d
    , uidActivity       = uid1Activity d
    , uidPose           = uid1Pose d
    , uidAnimStride     = uid1AnimStride d
    , uidStats          = uid1Stats d
    , uidModifiers      = uid1Modifiers d
    , uidSkills         = uid1Skills d
    , uidKnowledge      = uid1Knowledge d
    , uidInventory      = map migrateItemInstanceDTOv1 (uid1Inventory d)
    , uidEquipped       = HM.map migrateItemInstanceDTOv1 (uid1Equipped d)
    , uidAccessories    = map migrateItemInstanceDTOv1 (uid1Accessories d)
    , uidFactionId      = uid1FactionId d
    , uidWounds         = uid1Wounds d
    , uidScars          = uid1Scars d
    , uidImmuneResponse = uid1ImmuneResponse d
    , uidImmunities     = uid1Immunities d
    , uidBlood          = uid1Blood d
    , uidName           = uid1Name d
    }

-- | Per-page unit slice. Carries ONLY the instance map — the unit-id
--   allocator (@usnNextId@) is absent for the same global-allocator
--   reason as @bsnNextId@ above. Each instance is the frozen
--   'UnitInstanceDTO', not the live-record-carrying "World.Save.Types"
--   snapshot (see 'UnitInstanceDTO').
data PageUnitsDTO = PageUnitsDTO
    { puPageId    ∷ !WorldPageId
    , puInstances ∷ !(HM.HashMap UnitId UnitInstanceDTO)
    } deriving (Show, Eq, Generic, Serialize)

newtype UnitsDTO = UnitsDTO { udPages ∷ [PageUnitsDTO] }
    deriving stock (Generic)
    deriving newtype (Show, Eq, Serialize)

-- | The FROZEN v1 page slice (#1233), carrying the frozen v1 instances.
data PageUnitsDTOv1 = PageUnitsDTOv1
    { pu1PageId    ∷ !WorldPageId
    , pu1Instances ∷ !(HM.HashMap UnitId UnitInstanceDTOv1)
    } deriving (Show, Eq, Generic, Serialize)

newtype UnitsDTOv1 = UnitsDTOv1 { ud1Pages ∷ [PageUnitsDTOv1] }
    deriving stock (Generic)
    deriving newtype (Show, Eq, Serialize)

migrateUnitsDTOv1 ∷ UnitsDTOv1 → UnitsDTO
migrateUnitsDTOv1 (UnitsDTOv1 slices) = UnitsDTO
    [ PageUnitsDTO (pu1PageId s)
          (HM.map migrateUnitInstanceDTOv1 (pu1Instances s))
    | s ← slices ]

-- Depends on @"core-session"@ too, for the global unit-id allocator
-- (@usnNextId@), same reasoning as @"buildings"@ above.
--
-- v2 (#1233): a unit's inventory, equipment and accessories carry the
-- physical values #1233 appended to the recursive item tree, so the shape
-- changed and v1 decodes through its own frozen tree.
unitsCodec ∷ ComponentCodec UnitsDTO
unitsCodec = componentCodec ComponentSpec
    { csComponent     = unitsComponentId
    , csVersion       = 2
    , csRequired      = True
    , csDeps          = [worldPagesComponentId, coreSessionComponentId]
    , csEncode        = \snap → UnitsDTO
        [ PageUnitsDTO (pgsPageId p)
              (HM.map toUnitInstanceDTO (usnInstances (pgsUnits p)))
        | p ← orderedPages snap ]
    , csDecode        = id
    , csOlderVersions = [ atVersion 1 migrateUnitsDTOv1 ]
    , csValidate      = const []
    }

applyUnits
    ∷ Word32 → Word32 → UnitsDTO → HM.HashMap WorldPageId PageSnapshot
    → Either [ComponentError] (HM.HashMap WorldPageId PageSnapshot)
applyUnits ver nextId (UnitsDTO slices) =
    applyPageSlices unitsComponentId ver puPageId
        (\s p → p { pgsUnits = UnitSnapshot
                        { usnInstances =
                            HM.map fromUnitInstanceDTO (puInstances s)
                        , usnNextId = nextId } })
        slices
