{-# LANGUAGE Strict #-}
-- | The definition catalog the scenario decoder validates against, built
--   from the LIVE registries (#2713, SCN-13 of epic #2698).
--
--   One function, because the decoder and the construction adapters must
--   read the same definitions: a catalog assembled anywhere else (the
--   test fixtures used to) could drift from what the adapters later
--   construct from. Read-only: this module writes no registry.
module Scenario.Load.Catalog
    ( liveScenarioCatalog
    ) where

import UPrelude
import Data.IORef (readIORef)
import qualified Data.HashMap.Strict as HM
import qualified Data.HashSet as HS
import Building.Types (BuildingManager(..))
import Engine.Core.Capability.Building (BuildingCapability(..), toBuildingCapability)
import Engine.Core.Capability.ContentRegistries
    (ContentRegistriesCapability(..), toContentRegistriesCapability)
import Engine.Core.Capability.ContentRegistriesView
    (ContentRegistriesViewCapability(..), toContentRegistriesViewCapability)
import Engine.Core.Capability.RenderHandoff (RenderHandoffCapability(..), toRenderHandoffCapability)
import Engine.Core.Capability.UnitCombat (UnitCombatCapability(..), toUnitCombatCapability)
import Engine.Core.Capability.WorldSim (WorldSimCapability(..), toWorldSimCapability)
import Engine.Core.ReadOnlyRef (readReadOnlyRef)
import Engine.Core.State (EngineEnv)
import Infection.Types (InfectionManager(..))
import Scenario.Items.Build (itemCatalogOf)
import Scenario.Placement.Catalog (withPlacementCatalog)
import Scenario.Types (ScenarioCatalog(..), emptyScenarioCatalog)
import Scenario.Units.Catalog (unitCatalogOf)
import Unit.Types (UnitDef(..), UnitManager(..))
import World.Flora.Types (FloraCatalog(..), FloraSpecies(..))
import World.Material (getMaterialProps, MaterialProps(..))
import World.Material.Id (MaterialId(..))

-- | The catalog of the definitions loaded right now.
liveScenarioCatalog ∷ EngineEnv → IO ScenarioCatalog
liveScenarioCatalog env = do
    let regs = toContentRegistriesCapability env
        view = toContentRegistriesViewCapability env
    um ← readIORef (ucUnitManagerRef (toUnitCombatCapability env))
    bm ← readIORef (bcBuildingManagerRef (toBuildingCapability env))
    reg ← readIORef (crPhysiologyRegistryRef regs)
    ecm ← readReadOnlyRef (crvEquipmentClassManagerRef view)
    items ← readReadOnlyRef (crvItemManagerRef view)
    locs ← readIORef (crLocationDefsRef regs)
    profiles ← readIORef (crLootProfileRegistryRef regs)
    art ← readIORef (rhStructureArtCatalogRef (toRenderHandoffCapability env))
    InfectionManager inf ← readIORef (crInfectionManagerRef regs)
    materials ← readIORef (wsMaterialRegistryRef (toWorldSimCapability env))
    flora ← readIORef (wsFloraCatalogRef (toWorldSimCapability env))
    pure $ withPlacementCatalog (bmDefs bm) locs art profiles emptyScenarioCatalog
        { catUnits = unitCatalogOf reg ecm (umDefs um)
        , catItems = itemCatalogOf items
        , catInfections = HS.fromList (HM.keys inf)
          -- the materials the registry holds (the placeholder every unregistered
          -- id reads as is not one), and the flora species by name: the same
          -- lookups the terrain and flora adapters resolve with
        , catMaterials = HS.fromList
            [ mpName props
            | i ← [1 ∷ Int .. 255]
            , let props = getMaterialProps materials (MaterialId (fromIntegral i))
            , mpName props ≢ "unknown" ]
        , catFlora = HS.fromList (map fsName (HM.elems (fcSpecies flora)))
        , catKnowledge = HS.fromList
            [ k | d ← HM.elems (umDefs um), k ← HM.keys (udKnowledgeTemplates d) ] }
