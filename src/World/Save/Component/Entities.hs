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
    , UnitSimStateDTO(..)
    , MoveTargetDTO(..)
    , PageCraftBillsDTO(..)
    , CraftBillsDTO(..)
    , CraftBillDTO(..)
    , BillQueueDTO(..)
    , PagePowerNodesDTO(..)
    , PowerNodesDTO(..)
    , PowerNodeDTO(..)
    , NodeRegistryDTO(..)
    , toUnitSimStateDTO
    , fromUnitSimStateDTO
    , toBillQueueDTO
    , fromBillQueueDTO
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

data PageSimDTO = PageSimDTO
    { psPageId ∷ !WorldPageId
    , psSim    ∷ !(HM.HashMap UnitId UnitSimStateDTO)
    } deriving (Show, Eq, Generic, Serialize)

newtype UnitSimDTO = UnitSimDTO { usdPages ∷ [PageSimDTO] }
    deriving stock (Generic)
    deriving newtype (Show, Eq, Serialize)

unitSimCodec ∷ ComponentCodec UnitSimDTO
unitSimCodec = serializeCodec
    unitSimComponentId 1 True [worldPagesComponentId, unitsComponentId]
    (\snap → UnitSimDTO
        [ PageSimDTO (pgsPageId p) (HM.map toUnitSimStateDTO (pgsUnitSimStates p))
        | p ← orderedPages snap ])
    (\_ d → Right d) (const [])

applyUnitSim
    ∷ Word32 → UnitSimDTO → HM.HashMap WorldPageId PageSnapshot
    → Either [ComponentError] (HM.HashMap WorldPageId PageSnapshot)
applyUnitSim ver (UnitSimDTO slices) =
    applyPageSlices unitSimComponentId ver psPageId
        (\s p → p { pgsUnitSimStates = HM.map fromUnitSimStateDTO (psSim s) })
        slices

-- craft-bills -------------------------------------------------------

-- | Frozen mirror of 'CraftBill' (a mutable runtime record appended to
--   across #329/#590/#795). Reuses the stable 'BillId'/'BuildingId'/
--   'UnitId'/'BillMode' leaf types.
data CraftBillDTO = CraftBillDTO
    { bilId         ∷ !BillId
    , bilStation    ∷ !BuildingId
    , bilRecipe     ∷ !Text
    , bilRemaining  ∷ !Int
    , bilClaimant   ∷ !(Maybe UnitId)
    , bilClaimedAt  ∷ !Double
    , bilProgress   ∷ !Float
    , bilSeq        ∷ !Int
    , bilPaused     ∷ !Bool
    , bilWorking    ∷ !Bool
    , bilMode       ∷ !BillMode
    , bilTarget     ∷ !Int
    , bilOutputItem ∷ !Text
    } deriving (Show, Eq, Generic, Serialize)

-- | Frozen mirror of the 'CraftBills' queue (bills + its embedded id
--   counter).
data BillQueueDTO = BillQueueDTO
    { bqBills  ∷ !(HM.HashMap BillId CraftBillDTO)
    , bqNextId ∷ !Word32
    } deriving (Show, Eq, Generic, Serialize)

toCraftBillDTO ∷ CraftBill → CraftBillDTO
toCraftBillDTO b = CraftBillDTO
    { bilId         = cbId b
    , bilStation    = cbStation b
    , bilRecipe     = cbRecipe b
    , bilRemaining  = cbRemaining b
    , bilClaimant   = cbClaimant b
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
    , cbStation    = bilStation d
    , cbRecipe     = bilRecipe d
    , cbRemaining  = bilRemaining d
    , cbClaimant   = bilClaimant d
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

newtype CraftBillsDTO = CraftBillsDTO { cbdPages ∷ [PageCraftBillsDTO] }
    deriving stock (Generic)
    deriving newtype (Show, Serialize)

-- | Component-local invariant (#760 round 8, mirrors
--   "World.Save.Component.Page"'s @worldPagesCodec@ @validatePages@
--   precedent): every bill's own id must sit below that PAGE's queue
--   allocator ('bqNextId') — 'BillId' is allocated per-page (see
--   'Craft.Bills.emptyCraftBills'), unlike the item/building/unit
--   allocators, which are global. A literal duplicate key within one
--   page's @bqBills@ map is structurally impossible once decoded (a
--   'HashMap' cannot carry two entries under the same key), so there is
--   nothing further to check there.
validateCraftBills ∷ CraftBillsDTO → [ComponentError]
validateCraftBills (CraftBillsDTO slices) =
    [ ComponentError craftBillsComponentId 1 ValidatePhase
        ("page '" <> tshow (pcbPageId s) <> "': bill #"
         <> tshow (unBillId bid) <> " is not below the page's bill \
            \allocator (" <> tshow (bqNextId (pcbBills s)) <> ")")
    | s   ← slices
    , bid ← HM.keys (bqBills (pcbBills s))
    , unBillId bid ≥ bqNextId (pcbBills s)
    ]

craftBillsCodec ∷ ComponentCodec CraftBillsDTO
craftBillsCodec = serializeCodec
    craftBillsComponentId 1 True [worldPagesComponentId, buildingsComponentId]
    (\snap → CraftBillsDTO
        [ PageCraftBillsDTO (pgsPageId p) (toBillQueueDTO (pgsCraftBills p))
        | p ← orderedPages snap ])
    (\_ d → Right d) validateCraftBills

applyCraftBills
    ∷ Word32 → CraftBillsDTO → HM.HashMap WorldPageId PageSnapshot
    → Either [ComponentError] (HM.HashMap WorldPageId PageSnapshot)
applyCraftBills ver (CraftBillsDTO slices) =
    applyPageSlices craftBillsComponentId ver pcbPageId
        (\s p → p { pgsCraftBills = fromBillQueueDTO (pcbBills s) }) slices

-- power-nodes -------------------------------------------------------

-- | Frozen mirror of 'PowerNode' (a mutable runtime record appended to
--   across #358/#360). Reuses the stable 'PowerNodeId'/'BuildingId'/
--   'PowerRole' leaf types.
data PowerNodeDTO = PowerNodeDTO
    { nodId         ∷ !PowerNodeId
    , nodBuilding   ∷ !BuildingId
    , nodRole       ∷ !PowerRole
    , nodPeakWatts  ∷ !Float
    , nodCapacityWh ∷ !Float
    , nodStoredWh   ∷ !Float
    } deriving (Show, Eq, Generic, Serialize)

-- | Frozen mirror of the 'PowerNodes' registry (nodes + its embedded id
--   counter).
data NodeRegistryDTO = NodeRegistryDTO
    { regNodes  ∷ !(HM.HashMap PowerNodeId PowerNodeDTO)
    , regNextId ∷ !Word32
    } deriving (Show, Eq, Generic, Serialize)

toPowerNodeDTO ∷ PowerNode → PowerNodeDTO
toPowerNodeDTO n = PowerNodeDTO
    { nodId         = pnId n
    , nodBuilding   = pnBuilding n
    , nodRole       = pnRole n
    , nodPeakWatts  = pnPeakWatts n
    , nodCapacityWh = pnCapacityWh n
    , nodStoredWh   = pnStoredWh n
    }

fromPowerNodeDTO ∷ PowerNodeDTO → PowerNode
fromPowerNodeDTO d = PowerNode
    { pnId         = nodId d
    , pnBuilding   = nodBuilding d
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

data PagePowerNodesDTO = PagePowerNodesDTO
    { ppnPageId ∷ !WorldPageId
    , ppnNodes  ∷ !NodeRegistryDTO
    } deriving (Show, Generic, Serialize)

newtype PowerNodesDTO = PowerNodesDTO { pndPages ∷ [PagePowerNodesDTO] }
    deriving stock (Generic)
    deriving newtype (Show, Serialize)

-- | Component-local invariant (#760 round 8), same shape as
--   'validateCraftBills': every node's own id must sit below that
--   page's node-registry allocator ('regNextId') — 'PowerNodeId' is
--   allocated per-page (see 'Power.Types.emptyPowerNodes'). A literal
--   duplicate key within one page's @regNodes@ map is structurally
--   impossible once decoded, same reasoning as bills.
validatePowerNodes ∷ PowerNodesDTO → [ComponentError]
validatePowerNodes (PowerNodesDTO slices) =
    [ ComponentError powerNodesComponentId 1 ValidatePhase
        ("page '" <> tshow (ppnPageId s) <> "': power node #"
         <> tshow (unPowerNodeId nid) <> " is not below the page's node \
            \allocator (" <> tshow (regNextId (ppnNodes s)) <> ")")
    | s   ← slices
    , nid ← HM.keys (regNodes (ppnNodes s))
    , unPowerNodeId nid ≥ regNextId (ppnNodes s)
    ]

powerNodesCodec ∷ ComponentCodec PowerNodesDTO
powerNodesCodec = serializeCodec
    powerNodesComponentId 1 True [worldPagesComponentId, buildingsComponentId]
    (\snap → PowerNodesDTO
        [ PagePowerNodesDTO (pgsPageId p) (toNodeRegistryDTO (pgsPowerNodes p))
        | p ← orderedPages snap ])
    (\_ d → Right d) validatePowerNodes

applyPowerNodes
    ∷ Word32 → PowerNodesDTO → HM.HashMap WorldPageId PageSnapshot
    → Either [ComponentError] (HM.HashMap WorldPageId PageSnapshot)
applyPowerNodes ver (PowerNodesDTO slices) =
    applyPageSlices powerNodesComponentId ver ppnPageId
        (\s p → p { pgsPowerNodes = fromNodeRegistryDTO (ppnNodes s) }) slices
