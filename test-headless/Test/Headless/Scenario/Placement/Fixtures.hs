{-# LANGUAGE OverloadedStrings #-}
-- | The engine fixture the "Scenario.Placement" gate (#2712, SCN-12 of
--   epic #2698) runs against. Nothing fakes a layer: the definitions are
--   the SHIPPED ones loaded through the real YAML verbs into a headless
--   engine with a running world thread (items, equipment, factions,
--   physiology, infections, units, buildings, loot tables and profiles,
--   the @ruin_small@ location), the structure pack is the shipped
--   @dungeon_1@ pack registered by the REAL @scripts/structures.lua@
--   (variants and their facemaps included), the page is a real finite
--   arena created through the world thread's own init command, and the
--   catalog the decoder validates against is built by
--   "Scenario.Placement.Catalog" from the live registries.
module Test.Headless.Scenario.Placement.Fixtures
    ( Fx(..)
    , withFx
    , evalDebug
    , scenarioCatalog
    , decodeDocument
    , newArena
    , newArenaAs
    , freshPageName
    , locationRegistry
    , bmDefsOf
    , itemsOf
    , addLocationDef
    , cryptDef
    , lootTables
    , quietDef
    ) where

import UPrelude
import Data.IORef (newIORef, readIORef, writeIORef, modifyIORef')
import Data.List (sort)
import qualified Data.ByteString.Char8 as BC
import qualified Data.HashMap.Strict as HM
import qualified Data.HashSet as HS
import qualified Data.Text as T
import GHC.Clock (getMonotonicTimeNSec)
import System.FilePath ((</>))
import Building.Types (BuildingDef, BuildingManager(..))
import Engine.Asset.Discovery (walkFilesWithExtension)
import Engine.Asset.YamlMaterials (loadPopulatedMaterialRegistry)
import Engine.Core.Capability.ContentRegistries
    (ContentRegistriesCapability(..), toContentRegistriesCapability)
import Engine.Core.Capability.ContentRegistriesView
    (ContentRegistriesViewCapability(..), toContentRegistriesViewCapability)
import Engine.Core.ReadOnlyRef (readReadOnlyRef)
import Engine.Core.State (EngineEnv(..))
import Engine.Core.Thread (ThreadControl(..))
import Engine.Scripting.Lua.API (registerLuaAPI)
import Engine.Scripting.Lua.Thread (createLuaBackendState)
import Engine.Scripting.Lua.Thread.Console (executeDebugLua)
import Engine.Scripting.Lua.Types (LuaBackendState(..))
import Infection.Types (InfectionManager(..))
import Item.Types (ItemManager)
import Location.Bounds (RelBounds(..))
import Location.Types
    ( LocationContent(..), LocationDef(..), LocationRegistry, registerLocation )
import LootProfile.Types (LootProfileRegistry)
import LootTable.Types (LootTableRegistry)
import Scenario.Items.Build (itemCatalogOf)
import Scenario.Placement.Catalog (withPlacementCatalog)
import Scenario.Schema (decodeScenarioBytes)
import Scenario.Types
import Scenario.Units.Catalog (unitCatalogOf)
import Test.Headless.Harness (sendWorldCommand, waitForWorldInit, withHeadlessEngine)
import Test.Headless.Harness.Isolation (withIsolatedResourceRoot)
import Unit.Types (UnitDef(..), UnitManager(..))
import World.Arena.Bounds (mkArenaBounds)
import World.Command.Types (WorldCommand(..))
import World.Page.Types (WorldPageId(..))
import World.State.Types (WorldState)

-- | The engine and the Lua state that loaded the definitions.
data Fx = Fx { fxEnv ∷ EngineEnv, fxLua ∷ LuaBackendState }

newBareLuaBackend ∷ EngineEnv → IO LuaBackendState
newBareLuaBackend env = do
    ls ← createLuaBackendState (luaToEngineQueue env) (luaQueue env)
                               (assetPoolRef env) (nextObjectIdRef env)
                               (inputStateRef env) (loggerRef env)
    stateRef ← newIORef ThreadRunning
    registerLuaAPI (lbsLuaState ls) env ls stateRef
    pure ls

evalDebug ∷ LuaBackendState → Text → IO Text
evalDebug ls src = T.dropAround (≡ '"') <$> executeDebugLua (lbsLuaState ls) src

-- | Boot the engine with its world thread, load the shipped definitions
--   and register the shipped structure pack through the real Lua loader.
withFx ∷ (Fx → IO ()) → IO ()
withFx act = withIsolatedResourceRoot $ withHeadlessEngine $ \env → do
    ls ← newBareLuaBackend env
    loadShipped ls
    registerPack ls
    installLua ls
    logger ← readIORef (loggerRef env)
    loadPopulatedMaterialRegistry logger "data/materials" ≫= writeIORef (materialRegistryRef env)
    act (Fx env ls)

-- | Every shipped definition family, in the order the engine's own boot
--   loads them (locations LAST: their content ids resolve against the
--   others).
loadShipped ∷ LuaBackendState → IO ()
loadShipped ls = do
    items ← sort ⊚ walkFilesWithExtension "data/items" ".yaml"
    forM_ items $ \f → load "loadItemYaml" ("data/items" </> f)
    load "loadEquipmentYaml" "data/equipment/humanoid.yaml"
    load "loadFactionYaml" "data/factions/base.yaml"
    load "loadPhysiologyYaml" "data/physiology/systems.yaml"
    load "loadInfectionYaml" "data/infections/bacteria.yaml"
    units ← sort ⊚ walkFilesWithExtension "data/units" ".yaml"
    forM_ units $ \f → load "loadUnitYaml" ("data/units" </> f)
    buildings ← sort ⊚ walkFilesWithExtension "data/buildings" ".yaml"
    forM_ buildings $ \f → load "loadBuildingYaml" ("data/buildings" </> f)
    load "loadLootTableYaml" "data/loot_tables/ruin_common.yaml"
    load "loadLootProfileYaml" "data/loot_profiles/ruin_industrial_salvage.yaml"
    load "loadLocationYaml" "data/locations/ruin_small.yaml"
  where
    load verb path = void $ evalDebug ls $ T.concat
        [ "engine.", verb, "('", T.pack path, "'); return 'loaded'" ]

-- | The shipped @dungeon_1@ pack, registered by the REAL placer module:
--   its default art, its wall caps and its @damaged@, @weathered@,
--   @broken@ and @ruined@ variants with their facemap overrides.
registerPack ∷ LuaBackendState → IO ()
registerPack ls = do
    r ← evalDebug ls "local s = require('scripts.structures'); s.registerPackArt(); return 'ok'"
    unless (r ≡ "ok") $ fail ("the pack did not register: " <> T.unpack r)

-- | The unit modules the readiness hook and the unit loader reach, loaded
--   before anything is materialized (the AI module FIRST, so the resource
--   modules' satellites see the real singleton).
installLua ∷ LuaBackendState → IO ()
installLua ls = void $ evalDebug ls $ T.intercalate " "
    [ "require('scripts.unit_ai');"
    , "_G.__res = require('scripts.unit_resources'); __res.init(0);"
    , "_G.__starv = require('scripts.starvation');"
    , "return 'ok'" ]

-- * Registries

itemsOf ∷ EngineEnv → IO ItemManager
itemsOf env = readReadOnlyRef (crvItemManagerRef (toContentRegistriesViewCapability env))

locationRegistry ∷ EngineEnv → IO LocationRegistry
locationRegistry env = readIORef (crLocationDefsRef (toContentRegistriesCapability env))

lootTables ∷ EngineEnv → IO LootTableRegistry
lootTables env = readIORef (crLootTableRegistryRef (toContentRegistriesCapability env))

lootProfiles ∷ EngineEnv → IO LootProfileRegistry
lootProfiles env = readIORef (crLootProfileRegistryRef (toContentRegistriesCapability env))

bmDefsOf ∷ EngineEnv → IO (HM.HashMap Text BuildingDef)
bmDefsOf env = bmDefs ⊚ readIORef (buildingManagerRef env)

-- | The catalog the decoder validates against, built from the LIVE
--   registries.
scenarioCatalog ∷ EngineEnv → IO ScenarioCatalog
scenarioCatalog env = do
    um ← readIORef (unitManagerRef env)
    bm ← readIORef (buildingManagerRef env)
    reg ← readIORef (crPhysiologyRegistryRef (toContentRegistriesCapability env))
    ecm ← readReadOnlyRef (crvEquipmentClassManagerRef (toContentRegistriesViewCapability env))
    items ← itemsOf env
    locs ← locationRegistry env
    profiles ← lootProfiles env
    art ← readIORef (structureArtCatalogRef env)
    InfectionManager inf ← readIORef (crInfectionManagerRef (toContentRegistriesCapability env))
    pure $ withPlacementCatalog (bmDefs bm) locs art profiles emptyScenarioCatalog
        { catUnits = unitCatalogOf reg ecm (umDefs um)
        , catItems = itemCatalogOf items
        , catInfections = HS.fromList (HM.keys inf)
        , catKnowledge = HS.fromList
            [ k | d ← HM.elems (umDefs um), k ← HM.keys (udKnowledgeTemplates d) ] }

-- | A document through the real decoder.
decodeDocument ∷ EngineEnv → String → IO (Scenario, [ScenarioDiagnostic])
decodeDocument env yaml = do
    cat ← scenarioCatalog env
    case decodeScenarioBytes cat (BC.pack yaml) of
        ScenarioLoaded s ds → pure (s, ds)
        ScenarioFailed f → fail ("scenario failed: " ⧺ show f)

-- * Pages

freshPageName ∷ Text → IO WorldPageId
freshPageName t = do
    n ← getMonotonicTimeNSec
    pure (WorldPageId (t <> "_" <> T.pack (show n)))

-- | A real finite arena page, created through the world thread's own init
--   command.
newArena ∷ EngineEnv → Text → Int → Int → IO (WorldPageId, WorldState)
newArena env name w h = freshPageName name ≫= \pid → newArenaAs env pid w h

-- | 'newArena' under a stated page id.
newArenaAs ∷ EngineEnv → WorldPageId → Int → Int → IO (WorldPageId, WorldState)
newArenaAs env pid w h = do
    ab ← either (fail ∘ show) pure (mkArenaBounds w h)
    sendWorldCommand env (WorldInitArena pid (Just ab))
    ws ← waitForWorldInit env pid 60
    pure (pid, ws)

-- * Fixture location definitions

-- | Register a location definition beside the shipped ones.
addLocationDef ∷ EngineEnv → LocationDef → IO ()
addLocationDef env def =
    modifyIORef' (crLocationDefsRef (toContentRegistriesCapability env)) (registerLocation def)

-- | A 2x2 crypt derived from the shipped @ruin_small@: a ranged
--   encounter, one guaranteed significant item and two pending container
--   shells of the shipped @field_toolbox@ drawing from the shipped
--   @ruin_industrial_salvage@ profile — every owed slot family at once,
--   from production items and profiles.
cryptDef ∷ LocationDef → LocationDef
cryptDef ruin = ruin
    { ldId = "crypt_fx", ldLabel = "Crypt", ldBounds = RelBounds 0 0 1 1
    , ldContents = take 1 (filter ((≡ "unit") ∘ lconKind) (ldContents ruin))
        ⧺ take 1 (filter lconSignificant (ldContents ruin))
        ⧺ [ shell, shell ]
        ⧺ [ fixedBars, (head' (filter ((≡ "loot_table") ∘ lconKind) (ldContents ruin))) { lconRolls = 3 } ] }
  where
    base = case ldContents ruin of
        c : _ → c
        [] → error "ruin_small has no contents"
    head' (x : _) = x
    head' [] = error "ruin_small has no loot table entry"
    fixedBars = base { lconKind = "item", lconId = "steel_bar", lconCount = 2, lconPosition = Nothing
                     , lconFaction = Nothing, lconRolls = 1, lconCountRange = Nothing
                     , lconClearance = Nothing, lconSignificant = False, lconProfile = Nothing }
    shell = base { lconKind = "container", lconId = "field_toolbox"
                 , lconCount = 1, lconPosition = Nothing, lconFaction = Nothing
                 , lconRolls = 1, lconCountRange = Nothing, lconClearance = Nothing
                 , lconSignificant = False, lconProfile = Just "ruin_industrial_salvage" }

-- | A ruin with no encounter: nothing in it joins by @encounter@.
quietDef ∷ LocationDef → LocationDef
quietDef ruin = ruin
    { ldId = "quiet_fx", ldLabel = "Quiet"
    , ldContents = filter ((≢ "unit") ∘ lconKind) (ldContents ruin) }
