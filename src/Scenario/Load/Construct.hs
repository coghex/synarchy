{-# LANGUAGE Strict #-}
-- | Install a decoded scenario's content into the published arena page
--   (#2713, SCN-13 of epic #2698), through the construction adapters
--   SCN-09 to SCN-12 built and the runtime's own owners.
--
--   The order is fixed by the dependencies, and is the one
--   "Scenario.Placement" documents:
--
--   1. terrain, fluids and flora (the ground everything else stands on);
--   2. buildings, judged against the REAL terrain just installed;
--   3. locations validated, and the units that would join a rejected one
--      excluded before they are built;
--   4. units, then ground items (the omitted default content of the
--      accepted locations among them);
--   5. locations resolved against what actually installed, and the
--      structures and locations committed on the world thread in one
--      request.
--
--   Nothing here writes a registry or a manager directly: every write goes
--   through an adapter or an owner function. A REJECTED entry is a warning
--   and part of the retained subset (D-14, D-47). A runtime failure to
--   install an entry the adapters ACCEPTED is different: the page is then
--   not the scenario, so it is reported as a failure and the caller
--   recovers.
module Scenario.Load.Construct
    ( ConstructEnv(..)
    , Constructed(..)
    , constructScenario
    ) where

import UPrelude
import Data.IORef (atomicModifyIORef', readIORef)
import qualified Data.HashMap.Strict as HM
import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import Building.Placement (lookupSurfaceZ)
import Building.Types
import Engine.Core.Capability.Building (BuildingCapability(..), toBuildingCapability)
import Engine.Core.Capability.ContentRegistries
    (ContentRegistriesCapability(..), toContentRegistriesCapability)
import Engine.Core.Capability.ContentRegistriesView
    (ContentRegistriesViewCapability(..), toContentRegistriesViewCapability)
import Engine.Core.Capability.UnitCombat (UnitCombatCapability(..), toUnitCombatCapability)
import Engine.Core.Capability.WorldSim
    (WorldSimCapability(..), toWorldSimCapability, withPageLifecycle)
import Engine.Core.Log (LoggerState)
import Engine.Core.ReadOnlyRef (readReadOnlyRef)
import Engine.Core.State (EngineEnv, freshItemInstanceId)
import Engine.Scripting.Lua.Types (LuaBackendState(..))
import Gameplay.Tags.Live (commitItemTreeAssignments)
import Item.Types (ItemInstance(..))
import qualified Item.Ground as G
import Location.Instance
import Scenario.Items.Build (ItemBuild(..), ItemBuildEnv(..), itemRefTargets, itemTagTargets)
import Scenario.Items.Owners (buildGroundItems, installGroundItems)
import Scenario.Load.Types (ScenarioLoadPoint(..))
import Scenario.Placement.Building
import Scenario.Placement.Install (requestPlacementInstall)
import Scenario.Placement.Location
import Scenario.Placement.Types
import Scenario.Types
import Scenario.Units.Build
import Scenario.Units.Install
import Scenario.Units.Ready (luaUnitReadiness)
import Scenario.World.Capture (placementTiles)
import Scenario.World.Install (requestWorldInstall)
import Scenario.World.Types
import Unit.Types (UnitManager(..))
import World.Arena.Bounds (ArenaBounds)
import World.Chunk.Admit (pageIncarnation)
import World.Generate.Types (WorldGenParams(..))
import World.Page.Types (WorldPageId)
import World.State.Types

-- | What the construction reads: the engine, the Lua state the units'
--   paused-readiness hook runs in (which is why this must be called on the
--   thread that owns it), and the seam the suite holds the run at.
data ConstructEnv = ConstructEnv
    { ceEnv    ∷ EngineEnv
    , ceLua    ∷ LuaBackendState
    , ceLogger ∷ LoggerState
    , cePage   ∷ !WorldPageId
    , ceBounds ∷ !(Maybe ArenaBounds)
    , ceSeam   ∷ ScenarioLoadPoint → IO ()
    }

data Constructed = Constructed
    { ccDiagnostics ∷ ![ScenarioDiagnostic]
      -- ^ every rejection, in the order the phases found them
    , ccFailures    ∷ ![Text]
      -- ^ accepted content that could not be installed; empty when the
      --   page is the scenario
    }

-- | Install @sc@. Runs on the Lua thread.
constructScenario ∷ ConstructEnv → Scenario → IO Constructed
constructScenario ce sc = do
    let env = ceEnv ce
        pid = cePage ce
        wsc = toWorldSimCapability env
        regs = toContentRegistriesCapability env
        view = toContentRegistriesViewCapability env
        unitRef = ucUnitManagerRef (toUnitCombatCapability env)
        buildingRef = bcBuildingManagerRef (toBuildingCapability env)
    wm0 ← readIORef (wsWorldManagerRef wsc)
    case lookup pid (wmWorlds wm0) of
        Nothing → pure (Constructed [] ["the published arena page '" <> tshow pid <> "' is gone"])
        Just ws → do
            epoch ← pageIncarnation ws
            -- 1. terrain, fluids and flora
            ceSeam ce PointTerrain
            worldOutcome ← requestWorldInstall wsc pid epoch ScenarioWorldRequest
                { swrTerrain = indexed "terrain" (scTerrain sc)
                , swrFluids = indexed "fluids" (scFluids sc)
                , swrFlora = indexed "flora" (scFlora sc) }
            (worldDiags, worldFails) ← pure $ case worldOutcome of
                WorldInstalled r → (wirDiagnostics r, [])
                WorldInstallNotAdmitted → ([], ["the arena page no longer admits the terrain install"])
                WorldInstallNotArena → ([], ["the page is not an arena"])
            -- the page as it now is
            items ← readReadOnlyRef (crvItemManagerRef view)
            now ← readIORef (wsGameTimeRef wsc)
            defsB ← bmDefs <$> readIORef buildingRef
            locs ← readIORef (crLocationDefsRef regs)
            loot ← readIORef (crLootTableRegistryRef regs)
            params ← readIORef (wsGenParamsRef ws)
            let seed = maybe 0 wgpSeed params
                worldSize = maybe 0 wgpWorldSize params
                itemEnv = ItemBuildEnv items (freshItemInstanceId env)
                unitTiles = [ (floor (ueX e + 0.5), floor (ueY e + 0.5)) | e ← scUnits sc ]
            td ← readIORef (wsTilesRef ws)
            edits ← readIORef (wsEditsRef ws)
            let tiles = placementTiles (ceBounds ce) seed edits td
                            (buildingFootprintTiles defsB (scBuildings sc) ⧺ unitTiles)
            bm0 ← readIORef buildingRef
            let lis0 = maybe emptyLocationInstances wgpLocationInstances params
                pageB = bm0 { bmInstances = HM.filter ((≡ pid) ∘ biPage) (bmInstances bm0) }
            -- 2. buildings
            bbs ← buildBuildings BuildingBuildEnv
                      { bbeDefs = defsB, bbeItems = itemEnv, bbeBounds = ceBounds ce
                      , bbeWorldSize = worldSize, bbeTiles = tiles, bbeExisting = pageB
                      , bbeLocations = lis0, bbeNow = now, bbePage = pid }
                (indexed "buildings" (scBuildings sc))
            let brefs = BuildingInstallRefs
                    { birBuildings = buildingRef, birUnits = unitRef
                    , birWorld = wsWorldManagerRef wsc
                    , birLifecycle = withPageLifecycle wsc
                    , birWorldSize = worldSize }
            bcs ← installBuildingBuilds brefs pid epoch bbs
            -- 3. locations judged, their would-be occupants decided
            let lenv = LocationBuildEnv
                    { lbeDefs = locs, lbeBounds = ceBounds ce, lbeSeed = seed, lbeNamer = Nothing }
                lv = validateLocations lenv (indexed "locations" (scLocations sc))
                (liveUnits, unitCascade) =
                    rejectUnitsOfRejectedLocations (lvRejected lv) (indexed "units" (scUnits sc))
            -- 4. units
            ceSeam ce PointUnits
            um ← readIORef unitRef
            reg ← readIORef (crPhysiologyRegistryRef regs)
            ecm ← readReadOnlyRef (crvEquipmentClassManagerRef view)
            inf ← readIORef (crInfectionManagerRef regs)
            let uenv = UnitBuildEnv
                    { ubeDefs = umDefs um, ubeItems = itemEnv, ubeEquipment = ecm
                    , ubePhysiology = reg, ubeInfections = inf, ubeNow = now
                    , ubePage = pid
                    , ubeSurfaceZ = \x y →
                        lookupSurfaceZ worldSize tiles (floor (x + 0.5), floor (y + 0.5)) }
            ubs ← forM liveUnits $ \(p, e) → (,) e <$> buildUnit uenv p e
            let urefs = UnitInstallRefs
                    { uirUnits = unitRef, uirSims = ucUtsRef (toUnitCombatCapability env)
                    , uirBuildings = buildingRef, uirWorld = wsWorldManagerRef wsc
                    , uirLifecycle = withPageLifecycle wsc
                    , uirReady = luaUnitReadiness (lbsLuaState (ceLua ce)) }
            ucs ← installUnitBuilds urefs pid epoch (map snd ubs)
            -- ground items, with the default content the accepted
            -- locations omit
            let (acceptedLocs, defaultGround) =
                    expandLocationDefaults locs loot (scGroundItems sc) (lvAccepted lv)
            (placements, gbuild) ← buildGroundItems itemEnv (scGroundItems sc ⧺ defaultGround)
            let (store, _) = installGroundItems [ p | Just p ← placements ] G.emptyGroundItems
            -- a materialization establishes the page's baseline: its ground is replaced
            atomicModifyIORef' (wsGroundItemsRef ws) (const (store, ()))
            _ ← commitItemTreeAssignments unitRef buildingRef (wsWorldManagerRef wsc)
                    (itemTagTargets gbuild) (itemRefTargets gbuild)
            -- 5. locations resolved, and everything committed on the world thread
            let joins =
                    [ UnitJoin loc uid (ueX e, ueY e)
                    | ((e, _), UnitInstalled uid _) ← zip ubs ucs
                    , Just loc ← [ueEncounter e] ]
                bindings = Map.fromList $
                    [ (r, ItemBinding i (iiDefName n) (iiInstanceId n ≡ iiInstanceId root))
                    | (placement, tree) ← zip placements (ibTrees gbuild)
                    , Just _ ← [placement], Just root ← [tree], n ← flatNodes root
                    , (i, r) ← ibRefs gbuild, i ≡ iiInstanceId n ]
                    ⧺ [ (r, ItemBinding i (iiDefName n) False)
                      | BuildingBuild (Just b) _ ← bbs
                      , BuildingInstalled _ ref ← bcs, ref ≡ bbRef b
                      , Just root ← ibTrees (bbItems b), n ← flatNodes root
                      , (i, r) ← ibRefs (bbItems b), i ≡ iiInstanceId n ]
                (installs, resolveDiags) = resolveLocationInstalls locs joins bindings acceptedLocs
            ceSeam ce PointPlacement
            placed ← requestPlacementInstall wsc pid epoch ScenarioPlacementRequest
                { sprStructures = Just (indexed "structures" (scStructures sc))
                , sprLocations = Just installs }
            let (placeDiags, placeFails) = case placed of
                    PlacementInstalled r → (pirDiagnostics r, [])
                    PlacementNotAdmitted → ([], ["the arena page no longer admits the placement install"])
                    PlacementNotArena → ([], ["the page is not an arena"])
                unitFails = [ "unit '" <> ref <> "' could not be installed: " <> tshow why
                            | UnitNotInstalled ref why ← ucs ]
                buildingFails = [ "building '" <> ref <> "' could not be installed: " <> tshow why
                                | BuildingNotInstalled ref why ← bcs ]
            pure Constructed
                { ccDiagnostics = worldDiags ⧺ concatMap bbuDiagnostics bbs ⧺ lvDiagnostics lv
                    ⧺ unitCascade ⧺ unitDiagnostics (map snd ubs) ucs
                    ⧺ ibDiagnostics gbuild ⧺ resolveDiags ⧺ placeDiags
                , ccFailures = worldFails ⧺ unitFails ⧺ buildingFails ⧺ placeFails }
  where
    flatNodes i = i : concatMap flatNodes (iiContents i)
    indexed ∷ Text → [α] → [(Text, α)]
    indexed key xs = [ (key <> "[" <> T.pack (show i) <> "]", x) | (i, x) ← zip [0 ∷ Int ..] xs ]
    -- a rejected unit's completion already carries its build diagnostics
    -- (and the lethality rejection after them); every other unit's are the
    -- build's own
    unitDiagnostics builds comps = concat
        [ case c of
            UnitRejected ds → ds
            _ → ubDiagnostics b
        | (b, c) ← zip builds comps ]
