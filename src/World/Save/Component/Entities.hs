{-# LANGUAGE Strict, UnicodeSyntax, DeriveGeneric, DeriveAnyClass, DerivingStrategies #-}
-- | Entity + entity-adjacent page-scoped components (issue #760,
--   save-overhaul B2). All page-scoped, all validated against the
--   @"world-pages"@ authority (requirement 8):
--
--   - @"buildings"@ (required) — per page: the building instances +
--     their delivered materials / storage / build progress. Owner:
--     'Building.Types.BuildingManager'. Depends on @"world-pages"@.
--   - @"units"@ (required) — per page: the unit instances (stats, skills,
--     modifiers, equipment, inventory, wounds, scars, immunity, blood).
--     Owner: 'Unit.Types.UnitManager'. Depends on @"world-pages"@.
--   - @"unit-sim"@ (required) — per page: per-unit simulation state
--     (position, pose, activity, target, path, gameplay deadlines).
--     Owner: 'Unit.Sim.Types.UnitThreadState'. Depends on @"world-pages"@
--     AND @"units"@ — a sim state must have a matching unit
--     (the orphan check runs at whole-session assembly).
--   - @"craft-bills"@ (required) — per page: the craft-bill queue.
--     Owner: 'Craft.Bills'. Depends on @"world-pages"@ + @"buildings"@
--     (bills reference stations by 'BuildingId'; a demolished station's
--     lingering bill is tolerated gameplay behaviour, so the dependency
--     is for ordering, not a hard orphan reject — see the
--     "World.Save.Snapshot" haddock).
--   - @"power-nodes"@ (required) — per page: the power-node registry
--     (source/storage nodes + a storage node's stored charge). Owner:
--     'Power.Types'. Depends on @"world-pages"@ + @"buildings"@, same
--     tolerated-dangling-reference reasoning as craft bills.
--
--   Requirement 4 — the on-disk contract is FROZEN, distinct from every
--   mutable runtime record. EVERY evolving live gameplay record reachable
--   from a component DTO is mirrored by a component-owned DTO with an
--   explicit, reviewable field-by-field conversion ('to…'/'from…'); none
--   is embedded directly. A field added, dropped, or reordered on any
--   live record changes only its conversion function — surfacing as a
--   compile error to reconcile — never as silent byte drift in a shipped
--   v1 component:
--
--   - The runtime STATE records
--     ('UnitSimState', 'CraftBill'/'CraftBills', 'PowerNode'/'PowerNodes')
--     have mirror DTOs 'UnitSimStateDTO', 'CraftBillDTO'/'BillQueueDTO',
--     'PowerNodeDTO'/'NodeRegistryDTO'.
--   - The per-page ENTITY snapshots are frozen too (review round 6): the
--     @"buildings"@/@"units"@ components carry 'BuildingInstanceDTO' /
--     'UnitInstanceDTO', NOT the "World.Save.Types" positional
--     'BuildingInstanceSnapshot'/'UnitInstanceSnapshot'. Those snapshots
--     themselves directly carry mutable 'ItemInstance' values (materials
--     delivered / storage / inventory / equipped / accessories) and, on
--     units, the live 'StatModifier'/'Wound'/'Scar' records — so a v1
--     @"buildings"@/@"units"@ payload could still drift from an unrelated
--     change to any of those without the component's OWN version dispatch
--     noticing. The transitive freeze closes that: the 'ItemInstance'
--     fields reuse "World.Save.Component.Page"'s shared 'ItemInstanceDTO';
--     'StatModifier'/'Wound'/'Scar' get 'StatModifierDTO'/'WoundDTO'/
--     'ScarDTO' here. Leaf enums ('Pose'/'UnitActivity'/'Direction'/
--     'BillMode'/'PowerRole') and durable id newtypes ('BillId'/
--     'PowerNodeId'/'BuildingId'/'UnitId') are reused as-is — append-only
--     content references with no independent mutable identity (boundary
--     rule leaf clause (a), see "World.Save.Component.Types"). The frozen
--     DTOs mirror the originals' exact field order and leaf types, so the
--     derived cereal layout is byte-identical to the earlier direct
--     embedding and the components stay schema v1 (verified by the frozen
--     tracked fixture in "Test.Headless.World.Save.Components").
--   - The @"buildings"@/@"units"@ components carry ONLY the per-page
--     instance maps — NOT the per-page @bsnNextId@/@usnNextId@ counters.
--     The building- and unit-id allocators are global, owned once by the
--     @"core-session"@ component (requirement 9: "allocator components
--     remain above all governed ids"), so a page slice must not carry a
--     duplicate copy. On assembly the per-page 'BuildingSnapshot'/
--     'UnitSnapshot' is reconstructed with @bsnNextId@/@usnNextId@ filled
--     from that single global allocator.
module World.Save.Component.Entities
    ( buildingsCodec
    , unitsCodec
    , unitSimCodec
    , craftBillsCodec
    , powerNodesCodec
    , PageBuildingsDTO(..)
    , BuildingsDTO(..)
    , BuildingInstanceDTO(..)
    , toBuildingInstanceDTO
    , fromBuildingInstanceDTO
    , PageUnitsDTO(..)
    , UnitsDTO(..)
    , UnitInstanceDTO(..)
    , StatModifierDTO(..)
    , WoundDTO(..)
    , ScarDTO(..)
    , toUnitInstanceDTO
    , fromUnitInstanceDTO
    , PageSimDTO(..)
    , UnitSimDTO(..)
    , PageSimDTOv1(..)
    , UnitSimDTOv1(..)
    , migratePageSimDTOv1
    , migrateUnitSimDTOv1
    , UnitSimStateDTO(..)
    , MoveTargetDTO(..)
    , PageCraftBillsDTO(..)
    , CraftBillsDTO(..)
    , CraftBillDTO(..)
    , BillQueueDTO(..)
    , CraftBillDTOv1(..)
    , BillQueueDTOv1(..)
    , PageCraftBillsDTOv1(..)
    , CraftBillsDTOv1(..)
    , migrateCraftBillDTOv1
    , migrateCraftBillsDTOv1
    , PagePowerNodesDTO(..)
    , PowerNodesDTO(..)
    , PowerNodeDTO(..)
    , NodeRegistryDTO(..)
    , PowerNodeDTOv1(..)
    , NodeRegistryDTOv1(..)
    , PagePowerNodesDTOv1(..)
    , PowerNodesDTOv1(..)
    , migratePowerNodeDTOv1
    , migratePowerNodesDTOv1
    , toUnitSimStateDTO
    , fromUnitSimStateDTO
    , toCraftBillDTO
    , fromCraftBillDTO
    , toBillQueueDTO
    , fromBillQueueDTO
    , toPowerNodeDTO
    , fromPowerNodeDTO
    , toNodeRegistryDTO
    , fromNodeRegistryDTO
    , applyBuildings
    , applyUnits
    , applyUnitSim
    , applyCraftBills
    , applyPowerNodes
    ) where

import UPrelude
import qualified Data.HashMap.Strict as HM
import qualified Data.List as L
import qualified Data.Text as T
import Data.Serialize (Serialize)
import GHC.Generics (Generic)
import World.Page.Types (WorldPageId)
import Building.Types (BuildingId)
import Craft.Bills
    ( CraftBills(..), CraftBill(..), BillId(..), BillMode )
import Power.Types
    ( PowerNodes(..), PowerNode(..), PowerNodeId(..), PowerRole )
import Unit.Types (UnitId, StatModifier(..), Wound(..), Scar(..))
import Unit.Sim.Types
    ( UnitSimState(..), MoveTarget(..), Pose, UnitActivity, Direction )
import World.Save.Types
    ( BuildingSnapshot(..), BuildingInstanceSnapshot(..)
    , UnitSnapshot(..), UnitInstanceSnapshot(..) )
import World.Save.Component.Page
    ( ItemInstanceDTO(..), toItemInstanceDTO, fromItemInstanceDTO )
import World.Save.Snapshot (SessionSnapshot(..), PageSnapshot(..))
import World.Save.Component.Types
import World.Save.Reference (SamePageRef(..))
import qualified Data.Serialize as S

orderedPages ∷ SessionSnapshot → [PageSnapshot]
orderedPages = L.sortOn pgsPageId . HM.elems . snapPages

tshow ∷ Show a ⇒ a → Text
tshow = T.pack . show

-- buildings ---------------------------------------------------------

-- | Frozen mirror of 'BuildingInstanceSnapshot' (review round 6). That
--   positional "World.Save.Types" snapshot directly carries mutable
--   'ItemInstance' values ('bisMaterialsDelivered'/'bisStorage'), so
--   embedding it would let an unrelated 'ItemInstance' change drift a v1
--   @"buildings"@ payload without the component's own version dispatch
--   noticing — the exact gap the frozen-DTO boundary rule
--   ("World.Save.Component.Types") forbids. The two item fields reuse the
--   shared 'ItemInstanceDTO' (frozen recursively there); every other
--   field is a leaf scalar/'Text'. Field order mirrors
--   'BuildingInstanceSnapshot' exactly, so the derived cereal bytes are
--   unchanged from the earlier direct embedding (component stays v1).
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

-- Depends on @"core-session"@ too: assembly refills each page's
-- @bsnNextId@ from the GLOBAL building-id allocator that @"core-session"@
-- installs, so it must fold first (requirement 9).
buildingsCodec ∷ ComponentCodec BuildingsDTO
buildingsCodec = serializeCodec
    buildingsComponentId 1 True [worldPagesComponentId, coreSessionComponentId]
    (\snap → BuildingsDTO
        [ PageBuildingsDTO (pgsPageId p)
              (HM.map toBuildingInstanceDTO (bsnInstances (pgsBuildings p)))
        | p ← orderedPages snap ])
    (\_ d → Right d) (const [])

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

-- | Frozen mirror of 'UnitInstanceSnapshot' (review round 6). Like
--   'BuildingInstanceDTO', that positional "World.Save.Types" snapshot
--   directly carries mutable 'ItemInstance' values
--   ('uisInventory'/'uisEquipped'/'uisAccessories') AND the live
--   'StatModifier'/'Wound'/'Scar' records ('uisModifiers'/'uisWounds'/
--   'uisScars'), any of which could drift a v1 @"units"@ payload without
--   the component's own version dispatch noticing. Each is frozen: items
--   via the shared 'ItemInstanceDTO', the three unit records via
--   'StatModifierDTO'/'WoundDTO'/'ScarDTO' above. 'Direction' is an
--   append-only leaf enum, reused as-is. Field order + leaf types mirror
--   'UnitInstanceSnapshot' exactly, so the derived cereal bytes are
--   unchanged from the earlier direct embedding (component stays v1).
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

-- Depends on @"core-session"@ too, for the global unit-id allocator
-- (@usnNextId@), same reasoning as @"buildings"@ above.
unitsCodec ∷ ComponentCodec UnitsDTO
unitsCodec = serializeCodec
    unitsComponentId 1 True [worldPagesComponentId, coreSessionComponentId]
    (\snap → UnitsDTO
        [ PageUnitsDTO (pgsPageId p)
              (HM.map toUnitInstanceDTO (usnInstances (pgsUnits p)))
        | p ← orderedPages snap ])
    (\_ d → Right d) (const [])

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

-- unit-sim ----------------------------------------------------------

-- | Frozen mirror of 'MoveTarget' (a mutable runtime record).
data MoveTargetDTO = MoveTargetDTO
    { mvtX     ∷ !Float
    , mvtY     ∷ !Float
    , mvtSpeed ∷ !Float
    } deriving (Show, Eq, Generic, Serialize)

-- | Frozen mirror of 'UnitSimState' (a mutable runtime record that gains
--   fields as movement/climb/fall features land). Field-for-field with
--   the live record's CURRENT shape; leaf enums are reused (see the
--   module haddock). Conversion is explicit — a change to 'UnitSimState'
--   surfaces here as a type error to reconcile, never as silent byte
--   drift in a shipped v1 save.
data UnitSimStateDTO = UnitSimStateDTO
    { simRealX            ∷ !Float
    , simRealY            ∷ !Float
    , simGridZ            ∷ !Int
    , simRealZ            ∷ !Float
    , simTarget           ∷ !(Maybe MoveTargetDTO)
    , simPose             ∷ !Pose
    , simState            ∷ !UnitActivity
    , simFacing           ∷ !Direction
    , simLocalPath        ∷ ![(Float, Float)]
    , simDrinkUntil       ∷ !(Maybe Double)
    , simEatUntil         ∷ !(Maybe Double)
    , simPickupUntil      ∷ !(Maybe Double)
    , simTransitionUntil  ∷ !(Maybe Double)
    , simTransitionStride ∷ !Int
    , simPostTransition   ∷ ![Pose]
    , simClimbFromTile    ∷ !(Maybe (Float, Float, Int))
    , simClimbToTile      ∷ !(Maybe (Float, Float, Int))
    , simClimbStartTime   ∷ !(Maybe Double)
    , simClimbSlipAt      ∷ !(Maybe Double)
    , simFallFromTile     ∷ !(Maybe (Float, Float, Int))
    , simFallToTile       ∷ !(Maybe (Float, Float, Int))
    , simPendingClimbXP   ∷ !Float
    , simGetUpAt          ∷ !(Maybe Double)
    , simPendingFallDrop  ∷ !(Maybe Int)
    , simJumpApex         ∷ !(Maybe Float)
    , simMoveGrade        ∷ !Float
    } deriving (Show, Eq, Generic, Serialize)

toMoveTargetDTO ∷ MoveTarget → MoveTargetDTO
toMoveTargetDTO m = MoveTargetDTO
    { mvtX = mtTargetX m, mvtY = mtTargetY m, mvtSpeed = mtSpeed m }

fromMoveTargetDTO ∷ MoveTargetDTO → MoveTarget
fromMoveTargetDTO d = MoveTarget
    { mtTargetX = mvtX d, mtTargetY = mvtY d, mtSpeed = mvtSpeed d }

toUnitSimStateDTO ∷ UnitSimState → UnitSimStateDTO
toUnitSimStateDTO s = UnitSimStateDTO
    { simRealX            = usRealX s
    , simRealY            = usRealY s
    , simGridZ            = usGridZ s
    , simRealZ            = usRealZ s
    , simTarget           = toMoveTargetDTO <$> usTarget s
    , simPose             = usPose s
    , simState            = usState s
    , simFacing           = usFacing s
    , simLocalPath        = usLocalPath s
    , simDrinkUntil       = usDrinkUntil s
    , simEatUntil         = usEatUntil s
    , simPickupUntil      = usPickupUntil s
    , simTransitionUntil  = usTransitionUntil s
    , simTransitionStride = usTransitionStride s
    , simPostTransition   = usPostTransition s
    , simClimbFromTile    = usClimbFromTile s
    , simClimbToTile      = usClimbToTile s
    , simClimbStartTime   = usClimbStartTime s
    , simClimbSlipAt      = usClimbSlipAt s
    , simFallFromTile     = usFallFromTile s
    , simFallToTile       = usFallToTile s
    , simPendingClimbXP   = usPendingClimbXP s
    , simGetUpAt          = usGetUpAt s
    , simPendingFallDrop  = usPendingFallDrop s
    , simJumpApex         = usJumpApex s
    , simMoveGrade        = usMoveGrade s
    }

fromUnitSimStateDTO ∷ UnitSimStateDTO → UnitSimState
fromUnitSimStateDTO d = UnitSimState
    { usRealX            = simRealX d
    , usRealY            = simRealY d
    , usGridZ            = simGridZ d
    , usRealZ            = simRealZ d
    , usTarget           = fromMoveTargetDTO <$> simTarget d
    , usPose             = simPose d
    , usState            = simState d
    , usFacing           = simFacing d
    , usLocalPath        = simLocalPath d
    , usDrinkUntil       = simDrinkUntil d
    , usEatUntil         = simEatUntil d
    , usPickupUntil      = simPickupUntil d
    , usTransitionUntil  = simTransitionUntil d
    , usTransitionStride = simTransitionStride d
    , usPostTransition   = simPostTransition d
    , usClimbFromTile    = simClimbFromTile d
    , usClimbToTile      = simClimbToTile d
    , usClimbStartTime   = simClimbStartTime d
    , usClimbSlipAt      = simClimbSlipAt d
    , usFallFromTile     = simFallFromTile d
    , usFallToTile       = simFallToTile d
    , usPendingClimbXP   = simPendingClimbXP d
    , usGetUpAt          = simGetUpAt d
    , usPendingFallDrop  = simPendingFallDrop d
    , usJumpApex         = simJumpApex d
    , usMoveGrade        = simMoveGrade d
    }

-- | Issue #764 (save-overhaul C3) round-3 review: @psSim@'s map KEY is a
--   unit-simulation state's OWNING unit — a durable cross-component
--   reference (this component's own dependency on @"units"@ exists
--   precisely because of it) exactly like a craft bill's station or a
--   power node's host building, just carried as a 'HM.HashMap' key
--   rather than a field value. Typed the same way
--   ("World.Save.Reference"'s 'SamePageRef', which derives 'Hashable'
--   for exactly this use) rather than a bare 'UnitId' — a sim-state
--   entry is always expected on the SAME page as the page slice
--   carrying it (the live 'pgsUnitSimStates' this mirrors is itself
--   page-scoped; "World.Save.Snapshot"'s @OrphanedUnitSimState@ check
--   already enforces the SAME-page relationship this type now
--   documents). Bumped this component to v2; v1 decodes via
--   'migrateUnitSimDTOv1' below.
data PageSimDTO = PageSimDTO
    { psPageId ∷ !WorldPageId
    , psSim    ∷ !(HM.HashMap (SamePageRef UnitId) UnitSimStateDTO)
    } deriving (Show, Eq, Generic, Serialize)

newtype UnitSimDTO = UnitSimDTO { usdPages ∷ [PageSimDTO] }
    deriving stock (Generic)
    deriving newtype (Show, Eq, Serialize)

-- | The FROZEN v1 shape, preserved verbatim for decode-only backward
--   compatibility — @psSim@ here is keyed by the original bare
--   'UnitId', exactly as it shipped. Never edited; a further schema
--   change adds a v3 type instead (frozen-DTO boundary rule).
data PageSimDTOv1 = PageSimDTOv1
    { ps1PageId ∷ !WorldPageId
    , ps1Sim    ∷ !(HM.HashMap UnitId UnitSimStateDTO)
    } deriving (Show, Eq, Generic, Serialize)

newtype UnitSimDTOv1 = UnitSimDTOv1 { usd1Pages ∷ [PageSimDTOv1] }
    deriving stock (Generic)
    deriving newtype (Show, Eq, Serialize)

-- | Translate an unambiguous v1 page slice into v2: every v1 sim-state
--   map key has always meant "this unit, on THIS page" (the live
--   'pgsUnitSimStates' this mirrors is itself page-scoped — see
--   'PageSimDTO' haddock above), so wrapping every key in 'SamePageRef'
--   is total and never ambiguous (requirement 14).
migratePageSimDTOv1 ∷ PageSimDTOv1 → PageSimDTO
migratePageSimDTOv1 d = PageSimDTO
    { psPageId = ps1PageId d
    , psSim    = HM.mapKeys SamePageRef (ps1Sim d)
    }

migrateUnitSimDTOv1 ∷ UnitSimDTOv1 → UnitSimDTO
migrateUnitSimDTOv1 (UnitSimDTOv1 ps) = UnitSimDTO (map migratePageSimDTOv1 ps)

-- | Issue #764 (save-overhaul C3) round-3 review: hand-rolled
--   'ComponentCodec' (mirrors 'craftBillsCodec'/'powerNodesCodec' —
--   'serializeCodec' has no real multi-version dispatch) now that this
--   component needs v1→v2 migration too.
unitSimCodec ∷ ComponentCodec UnitSimDTO
unitSimCodec = ComponentCodec
    { ccId        = unitSimComponentId
    , ccVersion   = 2
    , ccInputVers = [1, 2]
    , ccRequired  = True
    , ccDeps      = [worldPagesComponentId, unitsComponentId]
    , ccEncode    = \snap → S.encode (UnitSimDTO
        [ PageSimDTO (pgsPageId p)
            (HM.mapKeys SamePageRef (HM.map toUnitSimStateDTO (pgsUnitSimStates p)))
        | p ← orderedPages snap ])
    , ccDecode    = \v bytes → case v of
        2 → case S.decode bytes of
              Left err → Left (ComponentError unitSimComponentId v
                                 DecodePhase ("malformed payload: " <> T.pack err))
              Right d  → Right d
        1 → case S.decode bytes of
              Left err → Left (ComponentError unitSimComponentId v
                                 DecodePhase ("malformed payload: " <> T.pack err))
              Right (d ∷ UnitSimDTOv1) → Right (migrateUnitSimDTOv1 d)
        _ → Left (ComponentError unitSimComponentId v DecodePhase
                    "unsupported schema version (reader supports v1, v2)")
    , ccValidate  = const []
    }

applyUnitSim
    ∷ Word32 → UnitSimDTO → HM.HashMap WorldPageId PageSnapshot
    → Either [ComponentError] (HM.HashMap WorldPageId PageSnapshot)
applyUnitSim ver (UnitSimDTO slices) =
    applyPageSlices unitSimComponentId ver psPageId
        (\s p → p { pgsUnitSimStates =
            HM.mapKeys unSamePageRef (HM.map fromUnitSimStateDTO (psSim s)) })
        slices

-- craft-bills -------------------------------------------------------

-- | Frozen mirror of 'CraftBill' (a mutable runtime record appended to
--   across #329/#590/#795). Reuses the stable 'BillId'/'BuildingId'/
--   'UnitId'/'BillMode' leaf types. Issue #764 (save-overhaul C3):
--   'bilStation'/'bilClaimant' are typed as same-page persistent
--   references ("World.Save.Reference"'s 'SamePageRef') rather than
--   bare ids — a bill's station/claimant are always expected on the
--   SAME page as the bill itself, a fact that used to live only in a
--   comment (see "World.Save.Integrity"'s wrong-page check, which reads
--   this declaration). Bumped this component to v2; v1 decodes via
--   'migrateCraftBillDTOv1' below (requirement 12/14).
data CraftBillDTO = CraftBillDTO
    { bilId         ∷ !BillId
    , bilStation    ∷ !(SamePageRef BuildingId)
    , bilRecipe     ∷ !Text
    , bilRemaining  ∷ !Int
    , bilClaimant   ∷ !(Maybe (SamePageRef UnitId))
    , bilClaimedAt  ∷ !Double
    , bilProgress   ∷ !Float
    , bilSeq        ∷ !Int
    , bilPaused     ∷ !Bool
    , bilWorking    ∷ !Bool
    , bilMode       ∷ !BillMode
    , bilTarget     ∷ !Int
    , bilOutputItem ∷ !Text
    } deriving (Show, Eq, Generic, Serialize)

-- | The FROZEN v1 shape (issue #760), preserved verbatim for decode-only
--   backward compatibility — 'bilStation'/'bilClaimant' here are the
--   original bare ids, exactly as they shipped. Never edited; a further
--   schema change adds a v3 type instead (frozen-DTO boundary rule, see
--   "World.Save.Component.Types").
data CraftBillDTOv1 = CraftBillDTOv1
    { bil1Id         ∷ !BillId
    , bil1Station    ∷ !BuildingId
    , bil1Recipe     ∷ !Text
    , bil1Remaining  ∷ !Int
    , bil1Claimant   ∷ !(Maybe UnitId)
    , bil1ClaimedAt  ∷ !Double
    , bil1Progress   ∷ !Float
    , bil1Seq        ∷ !Int
    , bil1Paused     ∷ !Bool
    , bil1Working    ∷ !Bool
    , bil1Mode       ∷ !BillMode
    , bil1Target     ∷ !Int
    , bil1OutputItem ∷ !Text
    } deriving (Show, Eq, Generic, Serialize)

-- | Frozen mirror of the 'CraftBills' queue (bills + its embedded id
--   counter).
data BillQueueDTO = BillQueueDTO
    { bqBills  ∷ !(HM.HashMap BillId CraftBillDTO)
    , bqNextId ∷ !Word32
    } deriving (Show, Eq, Generic, Serialize)

data BillQueueDTOv1 = BillQueueDTOv1
    { bq1Bills  ∷ !(HM.HashMap BillId CraftBillDTOv1)
    , bq1NextId ∷ !Word32
    } deriving (Show, Eq, Generic, Serialize)

-- | Translate an unambiguous v1 bill into v2: 'bil1Station' and
--   'bil1Claimant' are already known, by construction, to be same-page
--   references (v1 never carried anything else — every bill's station/
--   claimant has always been resolved against its OWN page, see
--   "World.Load.Stage"), so this is a total, never-ambiguous wrap
--   (requirement 14: "translate only when kind and page can be
--   determined unambiguously from the old component's owning context" —
--   satisfied here since the OLD component's very shape only ever meant
--   one thing).
migrateCraftBillDTOv1 ∷ CraftBillDTOv1 → CraftBillDTO
migrateCraftBillDTOv1 d = CraftBillDTO
    { bilId         = bil1Id d
    , bilStation    = SamePageRef (bil1Station d)
    , bilRecipe     = bil1Recipe d
    , bilRemaining  = bil1Remaining d
    , bilClaimant   = SamePageRef <$> bil1Claimant d
    , bilClaimedAt  = bil1ClaimedAt d
    , bilProgress   = bil1Progress d
    , bilSeq        = bil1Seq d
    , bilPaused     = bil1Paused d
    , bilWorking    = bil1Working d
    , bilMode       = bil1Mode d
    , bilTarget     = bil1Target d
    , bilOutputItem = bil1OutputItem d
    }

toCraftBillDTO ∷ CraftBill → CraftBillDTO
toCraftBillDTO b = CraftBillDTO
    { bilId         = cbId b
    , bilStation    = SamePageRef (cbStation b)
    , bilRecipe     = cbRecipe b
    , bilRemaining  = cbRemaining b
    , bilClaimant   = SamePageRef <$> cbClaimant b
    , bilClaimedAt  = cbClaimedAt b
    , bilProgress   = cbProgress b
    , bilSeq        = cbSeq b
    , bilPaused     = cbPaused b
    , bilWorking    = cbWorking b
    , bilMode       = cbMode b
    , bilTarget     = cbTarget b
    , bilOutputItem = cbOutputItem b
    }

fromCraftBillDTO ∷ CraftBillDTO → CraftBill
fromCraftBillDTO d = CraftBill
    { cbId         = bilId d
    , cbStation    = unSamePageRef (bilStation d)
    , cbRecipe     = bilRecipe d
    , cbRemaining  = bilRemaining d
    , cbClaimant   = unSamePageRef <$> bilClaimant d
    , cbClaimedAt  = bilClaimedAt d
    , cbProgress   = bilProgress d
    , cbSeq        = bilSeq d
    , cbPaused     = bilPaused d
    , cbWorking    = bilWorking d
    , cbMode       = bilMode d
    , cbTarget     = bilTarget d
    , cbOutputItem = bilOutputItem d
    }

toBillQueueDTO ∷ CraftBills → BillQueueDTO
toBillQueueDTO q = BillQueueDTO
    { bqBills = HM.map toCraftBillDTO (cbsBills q), bqNextId = cbsNextId q }

fromBillQueueDTO ∷ BillQueueDTO → CraftBills
fromBillQueueDTO d = CraftBills
    { cbsBills = HM.map fromCraftBillDTO (bqBills d), cbsNextId = bqNextId d }

data PageCraftBillsDTO = PageCraftBillsDTO
    { pcbPageId ∷ !WorldPageId
    , pcbBills  ∷ !BillQueueDTO
    } deriving (Show, Generic, Serialize)

data PageCraftBillsDTOv1 = PageCraftBillsDTOv1
    { pcb1PageId ∷ !WorldPageId
    , pcb1Bills  ∷ !BillQueueDTOv1
    } deriving (Show, Generic, Serialize)

newtype CraftBillsDTO = CraftBillsDTO { cbdPages ∷ [PageCraftBillsDTO] }
    deriving stock (Generic)
    deriving newtype (Show, Serialize)

newtype CraftBillsDTOv1 = CraftBillsDTOv1 { cbd1Pages ∷ [PageCraftBillsDTOv1] }
    deriving stock (Generic)
    deriving newtype (Show, Serialize)

migrateCraftBillsDTOv1 ∷ CraftBillsDTOv1 → CraftBillsDTO
migrateCraftBillsDTOv1 (CraftBillsDTOv1 ps) = CraftBillsDTO
    [ PageCraftBillsDTO (pcb1PageId p)
        (BillQueueDTO (HM.map migrateCraftBillDTOv1 (bq1Bills (pcb1Bills p)))
                      (bq1NextId (pcb1Bills p)))
    | p ← ps ]

-- | Component-local invariant (#760 round 8, mirrors
--   "World.Save.Component.Page"'s @worldPagesCodec@ @validatePages@
--   precedent): every bill's own id must sit below that PAGE's queue
--   allocator ('bqNextId') — 'BillId' is allocated per-page (see
--   'Craft.Bills.emptyCraftBills'), unlike the item/building/unit
--   allocators, which are global. A literal duplicate key within one
--   page's @bqBills@ map is structurally impossible once decoded (a
--   'HashMap' cannot carry two entries under the same key), so there is
--   nothing further to check there.
--
--   Round 9 adds the companion check the allocator check alone misses:
--   the map KEY and the DTO's own embedded 'bilId' are two independent
--   copies of the same identity (mirrored from live 'CraftBill'/
--   'CraftBills', which always keeps them in sync by construction — but
--   a decoded-from-disk envelope has no such guarantee). A hand-crafted
--   or corrupted envelope could carry @bqBills = {#1 -> bill{bilId=#2}}@
--   and the allocator check alone would accept it (both #1 and #2 sit
--   below the allocator), yet runtime APIs (which key off BOTH the
--   registry's map key via 'Craft.Bills.claimBill'/'releaseBill' AND the
--   bill's own 'cbId' field) would then disagree about which bill this
--   is. Reject any entry where the two disagree.
--
--   Deliberately OUT OF SCOPE here (this component-local validator sees
--   only ONE component's own DTO, never the assembled cross-component
--   picture): a dangling 'cbStation'/'cbClaimant' reference (a station
--   'BuildingId'/claimant 'UnitId' absent from the WHOLE session, not
--   just this page) is NOT hard-validated here, and never rejects the
--   load on that ground alone. This is not an oversight — it would
--   contradict an existing, deliberate design decision from #758:
--   "World.Save.Snapshot" (~line 199-207) documents that a demolished
--   station leaving its bills "lingering, visible + cancellable" is
--   tolerated gameplay behaviour, not corruption, and hard-failing on
--   it would reject otherwise-valid saves. "World.Load.Stage" restores
--   bills/nodes VERBATIM (never prunes them, issue #763 round 9) —
--   exactly the "do not drop the source record" contract requirement 11
--   asks for. Issue #764 (save-overhaul C3) adds the cross-component
--   check this component-local validator structurally cannot:
--   "World.Save.Integrity".'World.Save.Integrity.sessionIntegrityErrors'
--   runs once the WHOLE session is assembled and DOES hard-reject a
--   station/claimant that resolves on a DIFFERENT page than the bill
--   itself (a genuine wrong-page violation, never legitimate) while
--   still tolerating one absent from the entire session.
validateCraftBills ∷ CraftBillsDTO → [ComponentError]
validateCraftBills (CraftBillsDTO slices) = concat
    [ [ ComponentError craftBillsComponentId 2 ValidatePhase
          ("page '" <> tshow (pcbPageId s) <> "': bill #"
           <> tshow (unBillId bid) <> " is not below the page's bill \
              \allocator (" <> tshow (bqNextId (pcbBills s)) <> ")")
      | s   ← slices
      , bid ← HM.keys (bqBills (pcbBills s))
      , unBillId bid ≥ bqNextId (pcbBills s)
      ]
    , [ ComponentError craftBillsComponentId 2 ValidatePhase
          ("page '" <> tshow (pcbPageId s) <> "': bill map key #"
           <> tshow (unBillId k) <> " holds a bill whose own id is #"
           <> tshow (unBillId (bilId v)))
      | s      ← slices
      , (k, v) ← HM.toList (bqBills (pcbBills s))
      , k ≠ bilId v
      ]
    ]

-- | Issue #764 (save-overhaul C3): the current schema is v2
--   (typed 'SamePageRef' station/claimant, see 'CraftBillDTO'). A
--   hand-rolled 'ComponentCodec' rather than 'serializeCodec' — that
--   helper only ever accepted its own current version, with no real
--   multi-version dispatch wired up despite the seam being documented
--   ("World.Save.Component.Types"'s @ccInputVers@ haddock); this is the
--   first component to actually need it. v1 decodes via
--   'migrateCraftBillsDTOv1'; encoding always writes the current v2
--   shape.
craftBillsCodec ∷ ComponentCodec CraftBillsDTO
craftBillsCodec = ComponentCodec
    { ccId        = craftBillsComponentId
    , ccVersion   = 2
    , ccInputVers = [1, 2]
    , ccRequired  = True
    , ccDeps      = [worldPagesComponentId, buildingsComponentId]
    , ccEncode    = \snap → S.encode (CraftBillsDTO
        [ PageCraftBillsDTO (pgsPageId p) (toBillQueueDTO (pgsCraftBills p))
        | p ← orderedPages snap ])
    , ccDecode    = \v bytes → case v of
        2 → case S.decode bytes of
              Left err → Left (ComponentError craftBillsComponentId v
                                 DecodePhase ("malformed payload: " <> T.pack err))
              Right d  → Right d
        1 → case S.decode bytes of
              Left err → Left (ComponentError craftBillsComponentId v
                                 DecodePhase ("malformed payload: " <> T.pack err))
              Right (d ∷ CraftBillsDTOv1) → Right (migrateCraftBillsDTOv1 d)
        _ → Left (ComponentError craftBillsComponentId v DecodePhase
                    "unsupported schema version (reader supports v1, v2)")
    , ccValidate  = validateCraftBills
    }

applyCraftBills
    ∷ Word32 → CraftBillsDTO → HM.HashMap WorldPageId PageSnapshot
    → Either [ComponentError] (HM.HashMap WorldPageId PageSnapshot)
applyCraftBills ver (CraftBillsDTO slices) =
    applyPageSlices craftBillsComponentId ver pcbPageId
        (\s p → p { pgsCraftBills = fromBillQueueDTO (pcbBills s) }) slices

-- power-nodes -------------------------------------------------------

-- | Frozen mirror of 'PowerNode' (a mutable runtime record appended to
--   across #358/#360). Reuses the stable 'PowerNodeId'/'BuildingId'/
--   'PowerRole' leaf types. Issue #764 (save-overhaul C3): 'nodBuilding'
--   is typed as a same-page persistent reference ("World.Save.Reference"'s
--   'SamePageRef') rather than a bare id — a node's host building is
--   always expected on the SAME page as the node itself (see
--   'CraftBillDTO''s identical reasoning). Bumped this component to v2;
--   v1 decodes via 'migratePowerNodeDTOv1' below.
data PowerNodeDTO = PowerNodeDTO
    { nodId         ∷ !PowerNodeId
    , nodBuilding   ∷ !(SamePageRef BuildingId)
    , nodRole       ∷ !PowerRole
    , nodPeakWatts  ∷ !Float
    , nodCapacityWh ∷ !Float
    , nodStoredWh   ∷ !Float
    } deriving (Show, Eq, Generic, Serialize)

-- | The FROZEN v1 shape (issue #760), preserved verbatim for decode-only
--   backward compatibility.
data PowerNodeDTOv1 = PowerNodeDTOv1
    { nod1Id         ∷ !PowerNodeId
    , nod1Building   ∷ !BuildingId
    , nod1Role       ∷ !PowerRole
    , nod1PeakWatts  ∷ !Float
    , nod1CapacityWh ∷ !Float
    , nod1StoredWh   ∷ !Float
    } deriving (Show, Eq, Generic, Serialize)

-- | Frozen mirror of the 'PowerNodes' registry (nodes + its embedded id
--   counter).
data NodeRegistryDTO = NodeRegistryDTO
    { regNodes  ∷ !(HM.HashMap PowerNodeId PowerNodeDTO)
    , regNextId ∷ !Word32
    } deriving (Show, Eq, Generic, Serialize)

data NodeRegistryDTOv1 = NodeRegistryDTOv1
    { reg1Nodes  ∷ !(HM.HashMap PowerNodeId PowerNodeDTOv1)
    , reg1NextId ∷ !Word32
    } deriving (Show, Eq, Generic, Serialize)

-- | Unambiguous v1→v2 translation, same reasoning as
--   'migrateCraftBillDTOv1': v1's 'nod1Building' has always meant "the
--   host building on THIS node's own page" (see "World.Load.Stage"), so
--   wrapping it in 'SamePageRef' never guesses (requirement 14).
migratePowerNodeDTOv1 ∷ PowerNodeDTOv1 → PowerNodeDTO
migratePowerNodeDTOv1 d = PowerNodeDTO
    { nodId         = nod1Id d
    , nodBuilding   = SamePageRef (nod1Building d)
    , nodRole       = nod1Role d
    , nodPeakWatts  = nod1PeakWatts d
    , nodCapacityWh = nod1CapacityWh d
    , nodStoredWh   = nod1StoredWh d
    }

toPowerNodeDTO ∷ PowerNode → PowerNodeDTO
toPowerNodeDTO n = PowerNodeDTO
    { nodId         = pnId n
    , nodBuilding   = SamePageRef (pnBuilding n)
    , nodRole       = pnRole n
    , nodPeakWatts  = pnPeakWatts n
    , nodCapacityWh = pnCapacityWh n
    , nodStoredWh   = pnStoredWh n
    }

fromPowerNodeDTO ∷ PowerNodeDTO → PowerNode
fromPowerNodeDTO d = PowerNode
    { pnId         = nodId d
    , pnBuilding   = unSamePageRef (nodBuilding d)
    , pnRole       = nodRole d
    , pnPeakWatts  = nodPeakWatts d
    , pnCapacityWh = nodCapacityWh d
    , pnStoredWh   = nodStoredWh d
    }

toNodeRegistryDTO ∷ PowerNodes → NodeRegistryDTO
toNodeRegistryDTO r = NodeRegistryDTO
    { regNodes = HM.map toPowerNodeDTO (pnsNodes r), regNextId = pnsNextId r }

fromNodeRegistryDTO ∷ NodeRegistryDTO → PowerNodes
fromNodeRegistryDTO d = PowerNodes
    { pnsNodes = HM.map fromPowerNodeDTO (regNodes d), pnsNextId = regNextId d }

migrateNodeRegistryDTOv1 ∷ NodeRegistryDTOv1 → NodeRegistryDTO
migrateNodeRegistryDTOv1 d = NodeRegistryDTO
    { regNodes = HM.map migratePowerNodeDTOv1 (reg1Nodes d)
    , regNextId = reg1NextId d }

data PagePowerNodesDTO = PagePowerNodesDTO
    { ppnPageId ∷ !WorldPageId
    , ppnNodes  ∷ !NodeRegistryDTO
    } deriving (Show, Generic, Serialize)

data PagePowerNodesDTOv1 = PagePowerNodesDTOv1
    { ppn1PageId ∷ !WorldPageId
    , ppn1Nodes  ∷ !NodeRegistryDTOv1
    } deriving (Show, Generic, Serialize)

newtype PowerNodesDTO = PowerNodesDTO { pndPages ∷ [PagePowerNodesDTO] }
    deriving stock (Generic)
    deriving newtype (Show, Serialize)

newtype PowerNodesDTOv1 = PowerNodesDTOv1 { pnd1Pages ∷ [PagePowerNodesDTOv1] }
    deriving stock (Generic)
    deriving newtype (Show, Serialize)

migratePowerNodesDTOv1 ∷ PowerNodesDTOv1 → PowerNodesDTO
migratePowerNodesDTOv1 (PowerNodesDTOv1 ps) = PowerNodesDTO
    [ PagePowerNodesDTO (ppn1PageId p) (migrateNodeRegistryDTOv1 (ppn1Nodes p))
    | p ← ps ]

-- | Component-local invariant (#760 round 8), same shape as
--   'validateCraftBills': every node's own id must sit below that
--   page's node-registry allocator ('regNextId') — 'PowerNodeId' is
--   allocated per-page (see 'Power.Types.emptyPowerNodes'). A literal
--   duplicate key within one page's @regNodes@ map is structurally
--   impossible once decoded, same reasoning as bills.
--
--   Round 9 adds the same key/value identity check 'validateCraftBills'
--   gained: the map key and the DTO's own embedded 'nodId' must agree —
--   a decoded envelope with @regNodes = {#1 -> node{nodId=#2}}@ would
--   otherwise pass the allocator check yet leave runtime APIs (which key
--   off both identities) disagreeing about which node this is.
--
--   Deliberately OUT OF SCOPE here (same component-local-vs-cross-
--   component reasoning as 'validateCraftBills' above): a dangling
--   'pnBuilding' reference (a host building absent from the WHOLE
--   session) is NOT hard-validated here. See "World.Save.Snapshot"
--   (~line 199-207) and "World.Save.Integrity"'s
--   'World.Save.Integrity.sessionIntegrityErrors' (issue #764) — the
--   latter DOES hard-reject a host building that resolves on a
--   DIFFERENT page than the node itself, once the whole session is
--   assembled and cross-component checking is actually possible.
validatePowerNodes ∷ PowerNodesDTO → [ComponentError]
validatePowerNodes (PowerNodesDTO slices) = concat
    [ [ ComponentError powerNodesComponentId 2 ValidatePhase
          ("page '" <> tshow (ppnPageId s) <> "': power node #"
           <> tshow (unPowerNodeId nid) <> " is not below the page's node \
              \allocator (" <> tshow (regNextId (ppnNodes s)) <> ")")
      | s   ← slices
      , nid ← HM.keys (regNodes (ppnNodes s))
      , unPowerNodeId nid ≥ regNextId (ppnNodes s)
      ]
    , [ ComponentError powerNodesComponentId 2 ValidatePhase
          ("page '" <> tshow (ppnPageId s) <> "': power node map key #"
           <> tshow (unPowerNodeId k) <> " holds a node whose own id is #"
           <> tshow (unPowerNodeId (nodId v)))
      | s      ← slices
      , (k, v) ← HM.toList (regNodes (ppnNodes s))
      , k ≠ nodId v
      ]
    ]

-- | Same reasoning as 'craftBillsCodec': hand-rolled for real
--   multi-version decode. Current schema is v2 (typed 'SamePageRef'
--   host building, see 'PowerNodeDTO'); v1 decodes via
--   'migratePowerNodesDTOv1'.
powerNodesCodec ∷ ComponentCodec PowerNodesDTO
powerNodesCodec = ComponentCodec
    { ccId        = powerNodesComponentId
    , ccVersion   = 2
    , ccInputVers = [1, 2]
    , ccRequired  = True
    , ccDeps      = [worldPagesComponentId, buildingsComponentId]
    , ccEncode    = \snap → S.encode (PowerNodesDTO
        [ PagePowerNodesDTO (pgsPageId p) (toNodeRegistryDTO (pgsPowerNodes p))
        | p ← orderedPages snap ])
    , ccDecode    = \v bytes → case v of
        2 → case S.decode bytes of
              Left err → Left (ComponentError powerNodesComponentId v
                                 DecodePhase ("malformed payload: " <> T.pack err))
              Right d  → Right d
        1 → case S.decode bytes of
              Left err → Left (ComponentError powerNodesComponentId v
                                 DecodePhase ("malformed payload: " <> T.pack err))
              Right (d ∷ PowerNodesDTOv1) → Right (migratePowerNodesDTOv1 d)
        _ → Left (ComponentError powerNodesComponentId v DecodePhase
                    "unsupported schema version (reader supports v1, v2)")
    , ccValidate  = validatePowerNodes
    }

applyPowerNodes
    ∷ Word32 → PowerNodesDTO → HM.HashMap WorldPageId PageSnapshot
    → Either [ComponentError] (HM.HashMap WorldPageId PageSnapshot)
applyPowerNodes ver (PowerNodesDTO slices) =
    applyPageSlices powerNodesComponentId ver ppnPageId
        (\s p → p { pgsPowerNodes = fromNodeRegistryDTO (ppnNodes s) }) slices
