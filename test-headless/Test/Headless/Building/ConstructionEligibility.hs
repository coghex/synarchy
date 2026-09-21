{-# LANGUAGE OverloadedStrings #-}
-- | The engine-owned half of the "able-bodied builder census" gate
--   (#2641): the same contribution rule asserted on @biBuildProgress@
--   as the REGISTERED building API leaves it, not on a Lua double.
--
--   "Test.Headless.Lua.BuilderEligibility" covers the census in a bare
--   Lua VM, which is fast and complete about the shapes but doubles
--   @building.addBuildProgress@. Everything below that seam is real
--   here: a booted headless engine, the registered
--   'Engine.Scripting.Lua.API.Buildings.Progress' verbs, an engine-owned
--   'Building.Types.BuildingInstance' that is @constructing@ because
--   @biBuildProgress < bdBuildWork@, engine-owned
--   'Unit.Types.UnitInstance's that @unit.getAllIds@ \/ @unit.getInfo@
--   \/ @unit.getPose@ answer from, the shipped
--   @scripts\/building_spawn.lua@ tick, and the shipped
--   @scripts\/unit_ai_core.lua@ census it calls. The only thing the
--   fixture writes directly is the scene: which units exist, where they
--   stand, and what pose they are in.
--
--   So an example's progress reading is what the engine committed
--   through the real API, and a pose change is a real change to the
--   field @unit.getPose@ reads (@uiPose@, which
--   'Unit.Thread.Command.Pose' leaves on a killed instance — the
--   retention that makes a corpse enumerable at all).
--
--   No worker threads run, so nothing else advances construction
--   between ticks; @workerRate@ is @n * n@ for one to three workers, so
--   at @dt = 0.1@ one healthy worker earns 0.1 per tick and three earn
--   0.9.
--
--   Run just this gate: @cabal test synarchy-test-headless
--   --test-options='--match "able-bodied builder census"'@.
module Test.Headless.Building.ConstructionEligibility (spec) where

import UPrelude
import Test.Hspec
import Test.Headless.Harness.Isolation (withIsolatedResourceRoot)
import Test.Headless.Harness.Log (initializeEngineHeadlessQuiet)
import Data.IORef (atomicModifyIORef', newIORef, readIORef, writeIORef)
import qualified Data.HashMap.Strict as HM
import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as VU

import Building.Schema
import Building.Types
    ( BuildingActivity(..), BuildingDef(..), BuildingId(..)
    , BuildingInstance(..), BuildingManager(..), currentActivity
    , emptyBuildingManager )
import Engine.Asset.Handle (TextureHandle(..))
import Engine.Core.Init (EngineInitResult(..))
import Engine.Core.State (EngineEnv(..))
import Engine.Core.Thread (ThreadControl(..))
import Engine.Scripting.Lua.API (registerLuaAPI)
import Engine.Scripting.Lua.Thread (createLuaBackendState)
import Engine.Scripting.Lua.Thread.Console (executeDebugLua)
import Engine.Scripting.Lua.Types (LuaBackendState(..))
import Structure.Types (emptyChunkStructures)
import Unit.Direction (Direction(..))
import Unit.Faction (Faction(..))
import Unit.Types
    ( UnitDef(..), UnitId(..), UnitInstance(..), UnitManager(..)
    , defaultNaturalResistance, emptyUnitManager )
import World.Chunk.Types
    (ChunkCoord(..), ColumnTiles(..), LoadedChunk(..))
import World.Flora.Types (emptyFloraChunkData)
import World.Fluid.Types (emptyIceMap)
import World.Generate.Types (WorldGenParams(..), defaultWorldGenParams)
import World.Page.Types (WorldPageId(..))
import World.State.Types
    ( WorldManager(..), WorldState(..), emptyWorldManager, emptyWorldState )
import World.Tile.Types (WorldTileData(..))

-- * Fixture identity

fixturePage ∷ WorldPageId
fixturePage = WorldPageId "builder_eligibility"

-- | The site. 1x1 at (11,10), so Chebyshev 1 is the eight tiles around
--   it and the three workers below stand on three different ones.
siteBid ∷ BuildingId
siteBid = BuildingId 1

siteDefName ∷ Text
siteDefName = "eligibility_workshop"

-- | Far more work than any example earns, so the site never completes
--   mid-case and flips itself to @built@ — which would stop the tick
--   for a reason that has nothing to do with the census.
siteWork ∷ Float
siteWork = 1000

-- | The three workers, and where they stand. All three are adjacent;
--   'farUid' is not, and is the control that the footprint gate is
--   still doing its own job.
workerA, workerB, workerC, farUid ∷ UnitId
workerA = UnitId 1
workerB = UnitId 2
workerC = UnitId 3
farUid  = UnitId 4

unitTiles ∷ [(UnitId, (Float, Float))]
unitTiles =
    [ (workerA, (10, 10))
    , (workerB, (10, 11))
    , (workerC, (12, 10))
    , (farUid,  (20, 20))
    ]

-- | One healthy worker, ten ticks of 0.1: @R(1) * dt@ ten times.
oneWorkerTen, threeWorkersTen ∷ Float
oneWorkerTen    = 1.0
threeWorkersTen = 9.0

-- * Terrain

flatChunk ∷ ChunkCoord → LoadedChunk
flatChunk coord =
    let area = 16 * 16
        col  = ColumnTiles
            { ctStartZ = 0, ctMats = VU.singleton 1
            , ctSlopes = VU.singleton 0, ctVeg = VU.singleton 0 }
    in LoadedChunk
        { lcCoord             = coord
        , lcTiles             = V.replicate area col
        , lcSurfaceMap        = VU.replicate area 0
        , lcTerrainSurfaceMap = VU.replicate area 0
        , lcFluidMap          = V.replicate area Nothing
        , lcIceMap            = emptyIceMap
        , lcFlora             = emptyFloraChunkData
        , lcSideDeco          = VU.replicate area 0
        , lcWaterTableMap     = VU.replicate area 0
        , lcMagma             = Nothing
        , lcStructures        = emptyChunkStructures
        }

fixtureTiles ∷ WorldTileData
fixtureTiles = WorldTileData
    { wtdChunks    = HM.fromList
        [ (c, flatChunk c) | c ← [ChunkCoord 0 0, ChunkCoord 1 0
                                 , ChunkCoord 0 1, ChunkCoord 1 1] ]
    , wtdMaxChunks = 4
    }

-- * Definitions

-- | @bdBuildWork > 0@ is the worker-driven arm of
--   'Building.Types.currentActivity', and @bdMaterials@ is empty so
--   @building.areMaterialsSatisfied@ is trivially true and the tick's
--   materials gate is never what an example is measuring.
siteDef ∷ BuildingDef
siteDef = BuildingDef
    { bdName            = siteDefName
    , bdDisplayName     = siteDefName
    , bdCategory        = "Test"
    , bdDescription     = ""
    , bdTextures        = legacyAssets (TextureHandle 0)
    , bdIconTexture     = TextureHandle 0
    , bdTileW           = 1
    , bdTileH           = 1
    , bdPlacement       = "flat_ground"
    , bdIsStarting      = False
    , bdRace            = "acolyte"
    , bdSpriteAnchor    = "diamond_bottom"
    , bdBuildWork       = siteWork
    , bdMaterials       = HM.empty
    , bdStorageCapacity = 0
    , bdOperations      = []
    , bdAnimations      = HM.empty
    , bdRoleAnims       = Map.empty
    , bdVisualClass     = FreestandingInstallation
    , bdPowerDrain      = 0
    , bdPowerNode       = Nothing
    }

siteInstance ∷ BuildingInstance
siteInstance = BuildingInstance
    { biDefName            = siteDefName
    , biPage               = fixturePage
    , biTexture            = TextureHandle 0
    , biAnchorX            = 11
    , biAnchorY            = 10
    , biGridZ              = 0
    , biSpawnedAt          = 0
    , biTileW              = 1
    , biTileH              = 1
    , biSpawnRemaining     = 0
    , biBuildProgress      = 0
    , biMaterialsDelivered = HM.empty
    , biStorage            = []
    }

acolyteDef ∷ UnitDef
acolyteDef = UnitDef
    { udName = "acolyte", udNamePool = Nothing
    , udDisplayName = Just "Acolyte"
    , udTexture = TextureHandle 0, udPortrait = Nothing
    , udDirSprites = Map.empty
    , udBaseWidth = 0, udMaxSpeed = 1.0, udRunThreshold = 0.6
    , udAnimations = HM.empty, udStateAnims = HM.empty, udEagerStats = False
    , udStatTemplates = HM.empty, udBodyTemplates = HM.empty
    , udSkillTemplates = HM.empty, udKnowledgeTemplates = HM.empty
    , udStartingInventory = []
    , udEquipmentClass = Nothing, udStartingEquipment = HM.empty
    , udStartingAccessories = []
    , udBodyParts = []
    , udNaturalResistance = defaultNaturalResistance
    , udNaturalWeapon = Nothing, udModifiers = []
    , udFactionTags = []
    }

mkUnit ∷ (Float, Float) → UnitInstance
mkUnit (gx, gy) = UnitInstance
    { uiDefName = "acolyte", uiName = "", uiPage = fixturePage
    , uiTexture = TextureHandle 0, uiDirSprites = Map.empty
    , uiBaseWidth = 0, uiGridX = gx, uiGridY = gy, uiGridZ = 0
    , uiRealZ = 0, uiFacing = DirS
    , uiCurrentAnim = "", uiAnimStart = 0, uiAnimReverse = False
    , uiActivity = "idle", uiPose = "standing", uiAnimStride = 1
    , uiStats = HM.singleton "carrying_capacity" 100
    , uiModifiers = HM.empty, uiSkills = HM.empty
    , uiKnowledge = HM.empty, uiInventory = [], uiEquipment = HM.empty
    , uiAccessories = [], uiFactionId = FactionPlayer, uiWounds = []
    , uiScars = [], uiImmuneResponse = 0, uiImmunities = HM.empty
    , uiBlood = 5.0, uiLastAttackerUid = Nothing, uiLastAttackerAt = 0
    , uiAnimOverride = "", uiFrozen = False, uiForceLoop = False
    , uiClimbDest = Nothing, uiTrailState = Nothing
    }

-- * Scene

-- | One visible page holding the untouched site and the four units, all
--   standing. Every example starts here, so nothing inherits a pose or a
--   progress reading from the previous one.
resetScene ∷ EngineEnv → IO ()
resetScene env = do
    ws ← emptyWorldState
    writeIORef (wsTilesRef ws) fixtureTiles
    writeIORef (wsGenParamsRef ws) (Just defaultWorldGenParams { wgpWorldSize = 8 })
    writeIORef (worldManagerRef env) emptyWorldManager
        { wmWorlds = [(fixturePage, ws)], wmVisible = [fixturePage] }
    writeIORef (buildingManagerRef env) emptyBuildingManager
        { bmDefs      = HM.singleton siteDefName siteDef
        , bmInstances = HM.singleton siteBid siteInstance
        , bmNextId    = 2 }
    writeIORef (unitManagerRef env) emptyUnitManager
        { umDefs      = HM.singleton "acolyte" acolyteDef
        , umInstances = HM.fromList [ (uid, mkUnit at) | (uid, at) ← unitTiles ]
        , umNextId    = 5 }
    writeIORef (gameTimeRef env) 0
    writeIORef (enginePausedRef env) False

-- | Set one unit's @uiPose@ — the very field @unit.getPose@ reads.
setPose ∷ EngineEnv → UnitId → Text → IO ()
setPose env uid pose =
    atomicModifyIORef' (unitManagerRef env) $ \um →
        (um { umInstances = HM.adjust (\u → u { uiPose = pose })
                                      uid (umInstances um) }, ())

-- | The engine-owned progress the registered API committed.
progress ∷ EngineEnv → IO Float
progress env = do
    bm ← readIORef (buildingManagerRef env)
    pure $ maybe (-1) biBuildProgress (HM.lookup siteBid (bmInstances bm))

-- | What the engine says the site is doing. Every example asserts this
--   is still @Constructing@, so "no progress" can never be "the tick
--   stopped for some other reason".
activity ∷ EngineEnv → IO (Maybe BuildingActivity)
activity env = do
    bm ← readIORef (buildingManagerRef env)
    pure $ do
        inst ← HM.lookup siteBid (bmInstances bm)
        def  ← HM.lookup (biDefName inst) (bmDefs bm)
        pure (currentActivity 0 inst def)

-- * Lua plumbing

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

-- | Establish the @scripts.unit_ai@ singleton exactly as
--   @scripts/unit_ai.lua@ does — the empty table first, then the REAL
--   @scripts.unit_ai_core@ extending it — so the census the construction
--   tick calls is the shipped one and @core.aiState@ is the table it
--   reads. The full @scripts/unit_ai.lua@ is deliberately not booted:
--   this gate is about the census, and a live AI would also be
--   RESELECTING actions between ticks.
installAi ∷ LuaBackendState → IO Text
installAi ls = evalDebug ls $ T.intercalate " "
    [ "package.loaded['scripts.unit_ai'] ="
    , "  package.loaded['scripts.unit_ai'] or {};"
    , "_G.__core = require('scripts.unit_ai_core');"
    , "return 'ai'" ]

-- | Clear the AI rows and the spawn sequencer's per-building state, both
--   of which survive a re-@require@ by design.
resetLua ∷ LuaBackendState → IO Text
resetLua ls = evalDebug ls $ T.intercalate " "
    [ "for k in pairs(_G.__core.aiState) do _G.__core.aiState[k] = nil end;"
    , "local BS = require('scripts.building_spawn');"
    , "for k in pairs(BS.state) do BS.state[k] = nil end;"
    , "return 'reset'" ]

-- | Give a unit the cached @build_nearby@ action aimed at the site — the
--   state a worker that has arrived and started is carrying.
assign ∷ LuaBackendState → UnitId → IO Text
assign ls uid = evalDebug ls $ T.concat
    [ "_G.__core.aiState[", tshow (unUnitId uid), "] ="
    , "  { currentAction = 'build_nearby', buildTarget = "
    , tshow (unBuildingId siteBid), " };"
    , "return 'assigned'" ]

-- | The REAL suspension a collapsed or dead unit's tick takes
--   (@scripts/unit_ai.lua@'s pose return), folded to
--   @currentAction|buildTarget@ so the caller can assert what it did NOT
--   clear.
suspend ∷ LuaBackendState → UnitId → IO Text
suspend ls uid = evalDebug ls $ T.concat
    [ "_G.__core.suspendOrders(", tshow (unUnitId uid), ");"
    , "local s = _G.__core.aiState[", tshow (unUnitId uid), "];"
    , "return tostring(s.currentAction) .. '|' .. tostring(s.buildTarget)" ]

-- | @n@ ticks of the shipped construction update at @dt = 0.1@.
tick ∷ LuaBackendState → Int → IO Text
tick ls n = evalDebug ls $ T.concat
    [ "local BS = require('scripts.building_spawn');"
    , "for _ = 1, ", tshow n, " do BS.update(0.1) end;"
    , "return 'ticked'" ]

-- | Float progress compared with a tolerance: the deltas accumulate as
--   'Float' inside the engine, so an exact equality would be asserting
--   IEEE rounding rather than the census.
shouldBeNear ∷ Float → Float → Expectation
shouldBeNear got want
    | abs (got - want) < 1.0e-3 = pure ()
    | otherwise = expectationFailure $
        "expected progress " ++ show want ++ ", got " ++ show got

-- * Spec

spec ∷ Spec
spec = describe "able-bodied builder census" $
       describe "engine-owned progress (the registered building API)" $
       aroundAll setup $ do

    it "is a live constructing site with a worker the census can see" $
        \(env, ls) → do
            resetScene env
            _ ← resetLua ls
            _ ← assign ls workerA
            activity env `shouldReturn` Just Constructing
            progress env `shouldReturn` 0
            _ ← tick ls 10
            p ← progress env
            p `shouldBeNear` oneWorkerTen
            activity env `shouldReturn` Just Constructing

    it "commits nothing more once the only worker dies, and keeps what \
       \the engine already recorded" $ \(env, ls) → do
        resetScene env
        _ ← resetLua ls
        _ ← assign ls workerA
        _ ← tick ls 10
        earned ← progress env
        earned `shouldBeNear` oneWorkerTen
        setPose env workerA "dead"
        -- The suspension preserves the cached action and target: that is
        -- what makes the corpse enumerable as a builder at all, and what
        -- this gate is proving the census now looks past.
        suspend ls workerA `shouldReturn` "build_nearby|1"
        _ ← tick ls 100
        after' ← progress env
        after' `shouldBeNear` earned
        activity env `shouldReturn` Just Constructing

    it "commits nothing more once the only worker collapses" $ \(env, ls) → do
        resetScene env
        _ ← resetLua ls
        _ ← assign ls workerA
        _ ← tick ls 10
        earned ← progress env
        setPose env workerA "collapsed"
        suspend ls workerA `shouldReturn` "build_nearby|1"
        _ ← tick ls 100
        after' ← progress env
        after' `shouldBeNear` earned
        activity env `shouldReturn` Just Constructing

    it "resumes on revival with no re-selection, charging only the ticks \
       \after the unit stood up" $ \(env, ls) → do
        resetScene env
        _ ← resetLua ls
        _ ← assign ls workerA
        setPose env workerA "collapsed"
        suspend ls workerA `shouldReturn` "build_nearby|1"
        _ ← tick ls 50
        down ← progress env
        down `shouldBeNear` 0
        -- Nothing reassigns the action: the row is still the one the
        -- worker was knocked down holding.
        setPose env workerA "standing"
        _ ← tick ls 10
        up ← progress env
        up `shouldBeNear` oneWorkerTen

    it "rates a mixed team by its healthy count alone" $ \(env, ls) → do
        resetScene env
        _ ← resetLua ls
        forM_ [workerA, workerB, workerC] (void ∘ assign ls)
        setPose env workerB "dead"
        setPose env workerC "collapsed"
        _ ← suspend ls workerB
        _ ← suspend ls workerC
        -- Pre-fix this read three builders and paid R(3) = 9x, nine
        -- times what the one living worker is owed.
        _ ← tick ls 10
        mixed ← progress env
        mixed `shouldBeNear` oneWorkerTen
        -- And the healthy count is still what scales the rate: stand the
        -- other two up and the next ten ticks pay R(3).
        setPose env workerB "standing"
        setPose env workerC "standing"
        _ ← tick ls 10
        whole ← progress env
        whole `shouldBeNear` (oneWorkerTen + threeWorkersTen)

    it "leaves the footprint gate alone: a healthy but distant worker \
       \still contributes nothing" $ \(env, ls) → do
        resetScene env
        _ ← resetLua ls
        _ ← assign ls workerA
        _ ← assign ls farUid
        _ ← tick ls 10
        p ← progress env
        p `shouldBeNear` oneWorkerTen

  where
    -- Isolation wraps the boot (#1357): engine init is itself a config
    -- writer, and scripts/ is symlinked into the isolated root, so the
    -- real shipped Lua still loads.
    setup act = withIsolatedResourceRoot $ do
        EngineInitResult env ← initializeEngineHeadlessQuiet
        ls ← newBareLuaBackend env
        resetScene env
        _ ← installAi ls
        act (env, ls)
