{-# LANGUAGE Strict, UnicodeSyntax, DeriveGeneric, DeriveAnyClass #-}
-- | Entity + entity-adjacent page-scoped components (issue #760,
--   save-overhaul B2). All page-scoped, all validated against the
--   @"world-pages"@ authority (requirement 8):
--
--   - @"buildings"@ (required) — per page: the building snapshot
--     (instances + their delivered materials / storage / build progress).
--     Owner: 'Building.Types.BuildingManager'. Depends on @"world-pages"@.
--   - @"units"@ (required) — per page: the unit snapshot (stats, skills,
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
--   These reuse the already-frozen 'BuildingSnapshot'/'UnitSnapshot'/
--   'UnitSimState'/'CraftBills'/'PowerNodes' shapes as their payloads —
--   the component's OWN frozen DTO is the per-page-list wrapper, distinct
--   from every mutable runtime manager (requirement 4).
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
    , PageCraftBillsDTO(..)
    , CraftBillsDTO(..)
    , PagePowerNodesDTO(..)
    , PowerNodesDTO(..)
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
import Craft.Bills (CraftBills)
import Power.Types (PowerNodes)
import Unit.Types (UnitId)
import Unit.Sim.Types (UnitSimState)
import World.Save.Types (BuildingSnapshot(..), UnitSnapshot(..))
import World.Save.Snapshot (SessionSnapshot(..), PageSnapshot(..))
import World.Save.Component.Types

orderedPages ∷ SessionSnapshot → [PageSnapshot]
orderedPages = L.sortOn pgsPageId . HM.elems . snapPages

-- buildings ---------------------------------------------------------

data PageBuildingsDTO = PageBuildingsDTO
    { pbPageId    ∷ !WorldPageId
    , pbBuildings ∷ !BuildingSnapshot
    } deriving (Show, Eq, Generic, Serialize)

newtype BuildingsDTO = BuildingsDTO { bdPages ∷ [PageBuildingsDTO] }
    deriving (Show, Eq, Generic, Serialize)

buildingsCodec ∷ ComponentCodec BuildingsDTO
buildingsCodec = serializeCodec
    buildingsComponentId 1 True [worldPagesComponentId]
    (\snap → BuildingsDTO
        [ PageBuildingsDTO (pgsPageId p) (pgsBuildings p) | p ← orderedPages snap ])
    (\_ d → Right d) (const [])

applyBuildings
    ∷ BuildingsDTO → HM.HashMap WorldPageId PageSnapshot
    → Either [ComponentError] (HM.HashMap WorldPageId PageSnapshot)
applyBuildings (BuildingsDTO slices) =
    applyPageSlices buildingsComponentId pbPageId
        (\s p → p { pgsBuildings = pbBuildings s }) slices

-- units -------------------------------------------------------------

data PageUnitsDTO = PageUnitsDTO
    { puPageId ∷ !WorldPageId
    , puUnits  ∷ !UnitSnapshot
    } deriving (Show, Eq, Generic, Serialize)

newtype UnitsDTO = UnitsDTO { udPages ∷ [PageUnitsDTO] }
    deriving (Show, Eq, Generic, Serialize)

unitsCodec ∷ ComponentCodec UnitsDTO
unitsCodec = serializeCodec
    unitsComponentId 1 True [worldPagesComponentId]
    (\snap → UnitsDTO
        [ PageUnitsDTO (pgsPageId p) (pgsUnits p) | p ← orderedPages snap ])
    (\_ d → Right d) (const [])

applyUnits
    ∷ UnitsDTO → HM.HashMap WorldPageId PageSnapshot
    → Either [ComponentError] (HM.HashMap WorldPageId PageSnapshot)
applyUnits (UnitsDTO slices) =
    applyPageSlices unitsComponentId puPageId
        (\s p → p { pgsUnits = puUnits s }) slices

-- unit-sim ----------------------------------------------------------

data PageSimDTO = PageSimDTO
    { psPageId ∷ !WorldPageId
    , psSim    ∷ !(HM.HashMap UnitId UnitSimState)
    } deriving (Show, Eq, Generic, Serialize)

newtype UnitSimDTO = UnitSimDTO { usdPages ∷ [PageSimDTO] }
    deriving (Show, Eq, Generic, Serialize)

unitSimCodec ∷ ComponentCodec UnitSimDTO
unitSimCodec = serializeCodec
    unitSimComponentId 1 True [worldPagesComponentId, unitsComponentId]
    (\snap → UnitSimDTO
        [ PageSimDTO (pgsPageId p) (pgsUnitSimStates p) | p ← orderedPages snap ])
    (\_ d → Right d) (const [])

applyUnitSim
    ∷ UnitSimDTO → HM.HashMap WorldPageId PageSnapshot
    → Either [ComponentError] (HM.HashMap WorldPageId PageSnapshot)
applyUnitSim (UnitSimDTO slices) =
    applyPageSlices unitSimComponentId psPageId
        (\s p → p { pgsUnitSimStates = psSim s }) slices

-- craft-bills -------------------------------------------------------

data PageCraftBillsDTO = PageCraftBillsDTO
    { pcbPageId ∷ !WorldPageId
    , pcbBills  ∷ !CraftBills
    } deriving (Show, Generic, Serialize)

newtype CraftBillsDTO = CraftBillsDTO { cbdPages ∷ [PageCraftBillsDTO] }
    deriving (Show, Generic, Serialize)

craftBillsCodec ∷ ComponentCodec CraftBillsDTO
craftBillsCodec = serializeCodec
    craftBillsComponentId 1 True [worldPagesComponentId, buildingsComponentId]
    (\snap → CraftBillsDTO
        [ PageCraftBillsDTO (pgsPageId p) (pgsCraftBills p) | p ← orderedPages snap ])
    (\_ d → Right d) (const [])

applyCraftBills
    ∷ CraftBillsDTO → HM.HashMap WorldPageId PageSnapshot
    → Either [ComponentError] (HM.HashMap WorldPageId PageSnapshot)
applyCraftBills (CraftBillsDTO slices) =
    applyPageSlices craftBillsComponentId pcbPageId
        (\s p → p { pgsCraftBills = pcbBills s }) slices

-- power-nodes -------------------------------------------------------

data PagePowerNodesDTO = PagePowerNodesDTO
    { ppnPageId ∷ !WorldPageId
    , ppnNodes  ∷ !PowerNodes
    } deriving (Show, Generic, Serialize)

newtype PowerNodesDTO = PowerNodesDTO { pndPages ∷ [PagePowerNodesDTO] }
    deriving (Show, Generic, Serialize)

powerNodesCodec ∷ ComponentCodec PowerNodesDTO
powerNodesCodec = serializeCodec
    powerNodesComponentId 1 True [worldPagesComponentId, buildingsComponentId]
    (\snap → PowerNodesDTO
        [ PagePowerNodesDTO (pgsPageId p) (pgsPowerNodes p) | p ← orderedPages snap ])
    (\_ d → Right d) (const [])

applyPowerNodes
    ∷ PowerNodesDTO → HM.HashMap WorldPageId PageSnapshot
    → Either [ComponentError] (HM.HashMap WorldPageId PageSnapshot)
applyPowerNodes (PowerNodesDTO slices) =
    applyPageSlices powerNodesComponentId ppnPageId
        (\s p → p { pgsPowerNodes = ppnNodes s }) slices
