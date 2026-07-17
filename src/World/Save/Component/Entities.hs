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
--   mutable runtime record:
--
--   - The three actively-evolving runtime STATE records
--     ('UnitSimState', 'CraftBill'/'CraftBills', 'PowerNode'/'PowerNodes')
--     are NOT embedded directly. Each has a component-owned mirror DTO
--     ('UnitSimStateDTO', 'CraftBillDTO'/'BillQueueDTO',
--     'PowerNodeDTO'/'NodeRegistryDTO') with an explicit, reviewable
--     field-by-field conversion ('to…'/'from…'). A field added, dropped,
--     or reordered on any of those live records changes only its
--     conversion function — never a shipped v1 component's bytes. Leaf
--     enums ('Pose'/'UnitActivity'/'Direction'/'BillMode'/'PowerRole')
--     and durable id newtypes ('BillId'/'PowerNodeId'/'BuildingId'/
--     'UnitId') are reused as-is: they are append-only, save-version-
--     governed content references, exactly as the legacy
--     'BuildingInstanceSnapshot'/'UnitInstanceSnapshot' DTOs already
--     reuse them.
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
    , PageUnitsDTO(..)
    , UnitsDTO(..)
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
import Data.Serialize (Serialize)
import GHC.Generics (Generic)
import World.Page.Types (WorldPageId)
import Building.Types (BuildingId)
import Craft.Bills
    ( CraftBills(..), CraftBill(..), BillId, BillMode )
import Power.Types
    ( PowerNodes(..), PowerNode(..), PowerNodeId, PowerRole )
import Unit.Types (UnitId)
import Unit.Sim.Types
    ( UnitSimState(..), MoveTarget(..), Pose, UnitActivity, Direction )
import World.Save.Types
    ( BuildingSnapshot(..), BuildingInstanceSnapshot
    , UnitSnapshot(..), UnitInstanceSnapshot )
import World.Save.Snapshot (SessionSnapshot(..), PageSnapshot(..))
import World.Save.Component.Types

orderedPages ∷ SessionSnapshot → [PageSnapshot]
orderedPages = L.sortOn pgsPageId . HM.elems . snapPages

-- buildings ---------------------------------------------------------

-- | Per-page building slice. Carries ONLY the instance map — the
--   building-id allocator (@bsnNextId@) is deliberately absent, since it
--   is a global counter owned once by @"core-session"@ (requirement 9).
data PageBuildingsDTO = PageBuildingsDTO
    { pbPageId    ∷ !WorldPageId
    , pbInstances ∷ !(HM.HashMap BuildingId BuildingInstanceSnapshot)
    } deriving (Show, Eq, Generic, Serialize)

newtype BuildingsDTO = BuildingsDTO { bdPages ∷ [PageBuildingsDTO] }
    deriving stock (Generic)
    deriving newtype (Show, Eq, Serialize)

buildingsCodec ∷ ComponentCodec BuildingsDTO
buildingsCodec = serializeCodec
    buildingsComponentId 1 True [worldPagesComponentId]
    (\snap → BuildingsDTO
        [ PageBuildingsDTO (pgsPageId p) (bsnInstances (pgsBuildings p))
        | p ← orderedPages snap ])
    (\_ d → Right d) (const [])

-- | Reconstruct each page's 'BuildingSnapshot' from its instance slice,
--   filling @bsnNextId@ from the ONE global building-id allocator
--   (@snapNextBuildingId@, threaded in from @"core-session"@) rather than
--   from a per-page copy the wire no longer carries.
applyBuildings
    ∷ Word32 → BuildingsDTO → HM.HashMap WorldPageId PageSnapshot
    → Either [ComponentError] (HM.HashMap WorldPageId PageSnapshot)
applyBuildings nextId (BuildingsDTO slices) =
    applyPageSlices buildingsComponentId pbPageId
        (\s p → p { pgsBuildings = BuildingSnapshot
                        { bsnInstances = pbInstances s, bsnNextId = nextId } })
        slices

-- units -------------------------------------------------------------

-- | Per-page unit slice. Carries ONLY the instance map — the unit-id
--   allocator (@usnNextId@) is absent for the same global-allocator
--   reason as @bsnNextId@ above.
data PageUnitsDTO = PageUnitsDTO
    { puPageId    ∷ !WorldPageId
    , puInstances ∷ !(HM.HashMap UnitId UnitInstanceSnapshot)
    } deriving (Show, Eq, Generic, Serialize)

newtype UnitsDTO = UnitsDTO { udPages ∷ [PageUnitsDTO] }
    deriving stock (Generic)
    deriving newtype (Show, Eq, Serialize)

unitsCodec ∷ ComponentCodec UnitsDTO
unitsCodec = serializeCodec
    unitsComponentId 1 True [worldPagesComponentId]
    (\snap → UnitsDTO
        [ PageUnitsDTO (pgsPageId p) (usnInstances (pgsUnits p))
        | p ← orderedPages snap ])
    (\_ d → Right d) (const [])

applyUnits
    ∷ Word32 → UnitsDTO → HM.HashMap WorldPageId PageSnapshot
    → Either [ComponentError] (HM.HashMap WorldPageId PageSnapshot)
applyUnits nextId (UnitsDTO slices) =
    applyPageSlices unitsComponentId puPageId
        (\s p → p { pgsUnits = UnitSnapshot
                        { usnInstances = puInstances s, usnNextId = nextId } })
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
    ∷ UnitSimDTO → HM.HashMap WorldPageId PageSnapshot
    → Either [ComponentError] (HM.HashMap WorldPageId PageSnapshot)
applyUnitSim (UnitSimDTO slices) =
    applyPageSlices unitSimComponentId psPageId
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

craftBillsCodec ∷ ComponentCodec CraftBillsDTO
craftBillsCodec = serializeCodec
    craftBillsComponentId 1 True [worldPagesComponentId, buildingsComponentId]
    (\snap → CraftBillsDTO
        [ PageCraftBillsDTO (pgsPageId p) (toBillQueueDTO (pgsCraftBills p))
        | p ← orderedPages snap ])
    (\_ d → Right d) (const [])

applyCraftBills
    ∷ CraftBillsDTO → HM.HashMap WorldPageId PageSnapshot
    → Either [ComponentError] (HM.HashMap WorldPageId PageSnapshot)
applyCraftBills (CraftBillsDTO slices) =
    applyPageSlices craftBillsComponentId pcbPageId
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

powerNodesCodec ∷ ComponentCodec PowerNodesDTO
powerNodesCodec = serializeCodec
    powerNodesComponentId 1 True [worldPagesComponentId, buildingsComponentId]
    (\snap → PowerNodesDTO
        [ PagePowerNodesDTO (pgsPageId p) (toNodeRegistryDTO (pgsPowerNodes p))
        | p ← orderedPages snap ])
    (\_ d → Right d) (const [])

applyPowerNodes
    ∷ PowerNodesDTO → HM.HashMap WorldPageId PageSnapshot
    → Either [ComponentError] (HM.HashMap WorldPageId PageSnapshot)
applyPowerNodes (PowerNodesDTO slices) =
    applyPageSlices powerNodesComponentId ppnPageId
        (\s p → p { pgsPowerNodes = fromNodeRegistryDTO (ppnNodes s) }) slices
