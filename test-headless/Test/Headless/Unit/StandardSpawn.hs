{-# LANGUAGE Strict #-}
{-# LANGUAGE OverloadedStrings #-}
-- | "Standard spawn profile" (#2756): a test can spawn a unit whose
--   every stat, skill, knowledge value and body input is its
--   definition's base\/mean, with no draw from the gameplay RNG.
--
--   Everything runs on SHIPPED content loaded through the production
--   Lua loaders — the faction catalogue, every item under @data/items@,
--   the humanoid equipment class and @data/units/acolyte.yaml@ — and
--   through the REAL spawn handler, so the starting kit, the capacity
--   shed and the body authorities are the ones gameplay uses. The
--   portal half drives the shipped @scripts/building_spawn.lua@; the
--   one stub is @scripts.unit_ai@, pre-seeded so its singleton never
--   boots (the same seam "Portal spawn page binding" uses).
--
--   The engine runs NO worker threads, so nothing but the spawn under
--   test can touch the shared stat RNG between the two reads that
--   bracket it.
--
--   Run just this gate: @cabal test synarchy-test-headless
--   --test-options='--match "Standard spawn profile"'@.
module Test.Headless.Unit.StandardSpawn (spec) where

import UPrelude
import Test.Hspec
import Test.Headless.Harness.Isolation (withIsolatedResourceRoot)
import Data.IORef (atomicModifyIORef', newIORef, readIORef, writeIORef)
import Data.List (sort)
import qualified Data.HashMap.Strict as HM
import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as VU
import System.FilePath ((</>))
import System.Random (StdGen, mkStdGen)

import Building.Schema
import Building.Types
    ( BuildingDef(..), BuildingId(..), BuildingInstance(..)
    , BuildingManager(..), emptyBuildingManager )
import Engine.Asset.Discovery (walkFilesWithExtension)
import Engine.Asset.Handle (TextureHandle(..))
import Engine.Core.Capability.UnitCombat
    (UnitCombatCapability(..), toUnitCombatCapability)
import Engine.Core.Init (EngineInitResult(..))
import Engine.Core.State (EngineEnv(..))
import Engine.Core.Thread (ThreadControl(..))
import qualified Engine.Core.Queue as Q
import Engine.Scripting.Lua.API (registerLuaAPI)
import Engine.Scripting.Lua.Thread (createLuaBackendState)
import Engine.Scripting.Lua.Thread.Console (executeDebugLua)
import Engine.Scripting.Lua.Types (LuaBackendState(..))
import Item.Types (ItemInstance(..))
import Structure.Types (emptyChunkStructures)
import Test.Headless.Harness.Log (initializeEngineHeadlessQuiet)
import Unit.Command.Types (UnitCommand(..), SpawnProfile(..))
import Unit.Faction (Faction(..))
import Unit.Thread.Command (processAllUnitCommands)
import Unit.Thread.Command.Body (bloodSeedFromStats, seedBodyComposition)
import Unit.Thread.Command.Spawn
    (handleUnitSpawnCommand, rollTemplates, standardTemplates)
import Unit.Types
    ( UnitDef(..), UnitId(..), UnitInstance(..), UnitManager(..)
    , bloodMassRatio )
import World.Chunk.Admit (pageIncarnation)
import World.Chunk.Types (ChunkCoord(..), ColumnTiles(..), LoadedChunk(..))
import World.Flora.Types (emptyFloraChunkData)
import World.Fluid.Types (emptyIceMap)
import World.Generate.Types (WorldGenParams(..), defaultWorldGenParams)
import World.Page.Types (WorldPageId(..))
import World.State.Types
    (WorldManager(..), WorldState(..), emptyWorldManager, emptyWorldState)
import World.Tile.Types (WorldTileData(..))

-- * Fixture

page ∷ WorldPageId
page = WorldPageId "standard_spawn"

terrainZ ∷ Int
terrainZ = 4

acolyteName, portalDefName ∷ Text
acolyteName   = "acolyte"
portalDefName = "acolyte_portal"

portalBid ∷ BuildingId
portalBid = BuildingId 1

flatChunk ∷ LoadedChunk
flatChunk =
    let area = 16 * 16
        col  = ColumnTiles
            { ctStartZ = terrainZ
            , ctMats   = VU.singleton 1
            , ctSlopes = VU.singleton 0
            , ctVeg    = VU.singleton 0 }
    in LoadedChunk
        { lcCoord             = ChunkCoord 0 0
        , lcTiles             = V.replicate area col
        , lcSurfaceMap        = VU.replicate area terrainZ
        , lcTerrainSurfaceMap = VU.replicate area terrainZ
        , lcFluidMap          = V.replicate area Nothing
        , lcIceMap            = emptyIceMap
        , lcFlora             = emptyFloraChunkData
        , lcSideDeco          = VU.replicate area 0
        , lcWaterTableMap     = VU.replicate area 0
        , lcMagma             = Nothing
        , lcStructures        = emptyChunkStructures }

-- | A built portal (no build work, no appear animation) standing on the
--   page, keyed by the shipped def name so @building_spawn.lua@'s REAL
--   roster config drives it. The first roster entry is an acolyte.
portalDef ∷ BuildingDef
portalDef = BuildingDef
    { bdName = portalDefName, bdDisplayName = portalDefName
    , bdCategory = "Test", bdDescription = ""
    , bdTextures = legacyAssets (TextureHandle 0)
    , bdIconTexture = TextureHandle 0
    , bdTileW = 1, bdTileH = 1, bdPlacement = "flat_ground"
    , bdIsStarting = True, bdRace = "acolyte"
    , bdSpriteAnchor = "diamond_bottom", bdBuildWork = 0
    , bdMaterials = HM.empty, bdStorageCapacity = 0, bdOperations = []
    , bdAnimations = HM.empty, bdRoleAnims = Map.empty
    , bdVisualClass = FreestandingInstallation
    , bdPowerDrain = 0, bdPowerNode = Nothing }

portalInstance ∷ BuildingInstance
portalInstance = BuildingInstance
    { biDefName = portalDefName, biPage = page
    , biTexture = TextureHandle 0, biAnchorX = 2, biAnchorY = 3
    , biGridZ = terrainZ, biSpawnedAt = 0, biTileW = 1, biTileH = 1
    , biSpawnRemaining = 6, biBuildProgress = 0
    , biMaterialsDelivered = HM.empty, biStorage = [] }

-- | One live, visible page holding the portal; no units; every queue
--   drained. The unit DEFINITIONS the loaders registered are kept.
resetScene ∷ EngineEnv → IO WorldState
resetScene env = do
    ws ← emptyWorldState
    writeIORef (wsTilesRef ws) WorldTileData
        { wtdChunks = HM.singleton (ChunkCoord 0 0) flatChunk
        , wtdMaxChunks = 1 }
    writeIORef (wsGenParamsRef ws)
        (Just defaultWorldGenParams { wgpWorldSize = 8 })
    writeIORef (worldManagerRef env) emptyWorldManager
        { wmWorlds = [(page, ws)], wmVisible = [page] }
    writeIORef (buildingManagerRef env) emptyBuildingManager
        { bmDefs = HM.singleton portalDefName portalDef
        , bmInstances = HM.singleton portalBid portalInstance
        , bmNextId = 2 }
    atomicModifyIORef' (unitManagerRef env) $ \um →
        (um { umInstances = HM.empty }, ())
    writeIORef (gameTimeRef env) 0
    writeIORef (enginePausedRef env) False
    _ ← drainUnitQueue env
    pure ws

drainUnitQueue ∷ EngineEnv → IO [UnitCommand]
drainUnitQueue env = go []
  where
    go acc = Q.tryReadQueue (unitQueue env) ≫= \case
        Nothing  → pure (reverse acc)
        Just cmd → go (cmd : acc)

-- * Content, through the production loaders

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

loadWith ∷ LuaBackendState → Text → FilePath → IO ()
loadWith ls verb path = void $ evalDebug ls $ T.concat
    [ "engine.", verb, "('", T.pack path, "'); return 'loaded'" ]

-- | The shipped content a spawned acolyte depends on, in the startup
--   loader's order (factions before units, #2506).
loadShippedContent ∷ LuaBackendState → IO ()
loadShippedContent ls = do
    items ← sort ⊚ walkFilesWithExtension "data/items" ".yaml"
    forM_ items $ loadWith ls "loadItemYaml" ∘ ("data/items" </>)
    loadWith ls "loadEquipmentYaml" "data/equipment/humanoid.yaml"
    loadWith ls "loadFactionYaml" "data/factions/base.yaml"
    loadWith ls "loadUnitYaml" "data/units/acolyte.yaml"

-- | The AI stub (only its walk-out verb is reached) and a Lua RNG spy:
--   @math.random@ and @math.randomseed@ count their calls.
installLuaStubs ∷ LuaBackendState → IO Text
installLuaStubs ls = evalDebug ls $ T.intercalate " "
    [ "package.loaded['scripts.unit_ai'] = {"
    , "  commandMove = function() end };"
    , "_G.RNG = { random = 0, seed = 0 };"
    , "local realRandom, realSeed = math.random, math.randomseed;"
    , "math.random = function(...) RNG.random = RNG.random + 1;"
    , "  return realRandom(...) end;"
    , "math.randomseed = function(...) RNG.seed = RNG.seed + 1;"
    , "  return realSeed(...) end;"
    , "return 'stubbed'" ]

resetLua ∷ LuaBackendState → IO Text
resetLua ls = evalDebug ls $ T.intercalate " "
    [ "local BS = require('scripts.building_spawn');"
    , "for k in pairs(BS.state) do BS.state[k] = nil end;"
    , "BS.standardProfile = false;"
    , "RNG.random, RNG.seed = 0, 0;"
    , "return 'reset'" ]

-- * Readers

acolyteDef ∷ EngineEnv → IO UnitDef
acolyteDef env = do
    um ← readIORef (unitManagerRef env)
    maybe (fail "the shipped acolyte definition did not register") pure
          (HM.lookup acolyteName (umDefs um))

unitOf ∷ EngineEnv → UnitId → IO UnitInstance
unitOf env uid = do
    um ← readIORef (unitManagerRef env)
    maybe (fail ("no unit " <> show uid)) pure (HM.lookup uid (umInstances um))

statRNG ∷ EngineEnv → IO String
statRNG env = show ⊚ readIORef (statRNGRef env)

-- | Spawn through the REAL handler on the fixture page.
spawnWith ∷ EngineEnv → WorldState → Word32 → SpawnProfile → IO UnitInstance
spawnWith env ws raw profile = do
    epoch ← pageIncarnation ws
    handleUnitSpawnCommand env (ucUtsRef (toUnitCombatCapability env))
        (UnitId raw) acolyteName 2.5 3.5 terrainZ FactionPlayer page epoch
        profile
    unitOf env (UnitId raw)

stat ∷ UnitInstance → Text → Float
stat u k = HM.lookupDefault (-1) k (uiStats u)

approx ∷ Float → Float → Bool
approx a b = abs (a - b) ≤ 1.0e-3 * max 1 (abs b)

-- | Everything that should be identical between two standard units:
--   every rolled or derived value, and every kit instance's rolled
--   quality and weight (instance ids differ by construction).
fingerprint ∷ UnitInstance
            → ( [(Text, Float)], [(Text, Float)], [(Text, Float)], Text
              , [(Text, Float, Float)] )
fingerprint u =
    ( sort (HM.toList (uiStats u)), sort (HM.toList (uiSkills u))
    , sort (HM.toList (uiKnowledge u)), uiName u
    , sort [ (iiDefName i, iiQuality i, iiWeight i) | i ← kit ] )
  where kit = uiInventory u ⧺ HM.elems (uiEquipment u) ⧺ uiAccessories u

-- * Spec

spec ∷ Spec
spec = describe "Standard spawn profile (#2756)" $ aroundAll setup $ do
    directSpec
    rolledSpec
    portalSpec
    luaApiSpec
  where
    setup act = withIsolatedResourceRoot $ do
        EngineInitResult env ← initializeEngineHeadlessQuiet
        ls ← newBareLuaBackend env
        loadShippedContent ls
        _ ← installLuaStubs ls
        act (env, ls)

directSpec ∷ SpecWith (EngineEnv, LuaBackendState)
directSpec = describe "a standard acolyte, spawned directly" $ do

    it "takes every stat template's base, and its body inputs' means, \
       \at the input boundary" $ \(env, _) → do
        ws ← resetScene env
        def ← acolyteDef env
        u ← spawnWith env ws 101 SpawnStandard
        -- Non-body stats keep their base untouched; strength's base is
        -- promoted into strength_base before the body scales it.
        forM_ (HM.toList (udStatTemplates def)) $ \(k, (b, _)) →
            if k ≡ "strength"
                then (k, stat u "strength_base") `shouldBe` (k, b)
                else (k, stat u k) `shouldBe` (k, b)
        -- The body inputs: height stays a live stat (the loader files it
        -- beside the stat templates, so the loop above already pinned
        -- it); bulk and bodyfat are consumed by the body authority
        -- (never live stats), so they are read back through what it
        -- derived from them.
        let bodyMean k = maybe (-1) fst
                (HM.lookup k (HM.union (udBodyTemplates def)
                                       (udStatTemplates def)))
            h = bodyMean "height"
        stat u "height" `shouldBe` h
        HM.member "bulk" (uiStats u) `shouldBe` False
        HM.member "bodyfat" (uiStats u) `shouldBe` False
        stat u "frame_mass" `shouldSatisfy`
            approx (22 * h * h * bodyMean "bulk")
        (stat u "fat_mass" / stat u "body_mass") `shouldSatisfy`
            approx (bodyMean "bodyfat")
        -- The shipped means, pinned so a data change is noticed here.
        (h, bodyMean "bulk", bodyMean "bodyfat") `shouldBe` (1.8, 1.0, 0.2)

    it "derives everything else from the body authority, exactly as a \
       \rolled unit with those inputs would" $ \(env, _) → do
        ws ← resetScene env
        def ← acolyteDef env
        u ← spawnWith env ws 102 SpawnStandard
        uiStats u `shouldBe` seedBodyComposition
            (HM.union (standardTemplates (udStatTemplates def))
                      (standardTemplates (udBodyTemplates def)))
        let bm = stat u "body_mass"
        stat u "strength" `shouldSatisfy` approx 1.0
        stat u "max_hydration" `shouldSatisfy` approx (bm * 0.6)
        stat u "max_hunger" `shouldSatisfy` approx (bm * 10)
        stat u "max_calories" `shouldSatisfy` approx (bm * 20)
        uiBlood u `shouldBe` bloodSeedFromStats (uiStats u)
        uiBlood u `shouldSatisfy` approx (bm * bloodMassRatio)

    it "carries the capacity the formula gives at those inputs (~23.9 kg)" $
        \(env, _) → do
            ws ← resetScene env
            u ← spawnWith env ws 103 SpawnStandard
            let lm  = stat u "lean_mass"
                str = stat u "strength"
            stat u "carrying_capacity" `shouldSatisfy`
                approx (3.2 * ((lm * str) ** 0.6))
            stat u "carrying_capacity" `shouldSatisfy` approx 23.887

    it "takes every skill and knowledge template's base" $ \(env, _) → do
        ws ← resetScene env
        def ← acolyteDef env
        u ← spawnWith env ws 104 SpawnStandard
        HM.null (udSkillTemplates def) `shouldBe` False
        HM.null (udKnowledgeTemplates def) `shouldBe` False
        uiSkills u `shouldBe` HM.map fst (udSkillTemplates def)
        uiKnowledge u `shouldBe` HM.map fst (udKnowledgeTemplates def)

    it "spawns with the real starting kit, nothing shed" $ \(env, _) → do
        ws ← resetScene env
        def ← acolyteDef env
        u ← spawnWith env ws 105 SpawnStandard
        map iiDefName (uiInventory u) `shouldBe`
            [ n | (n, _, _) ← udStartingInventory def ]
        sort (HM.toList (HM.map iiDefName (uiEquipment u))) `shouldBe`
            sort (HM.toList (udStartingEquipment def))
        map iiDefName (uiAccessories u) `shouldBe` udStartingAccessories def
        -- the canteen still takes its authored fill, and the
        -- personal name is still drawn from the pool.
        map iiCurrentFill (take 1 (uiInventory u)) `shouldBe` [2.0]
        uiName u `shouldSatisfy` (not ∘ T.null)

    it "consumes nothing from the gameplay stat RNG, name and kit \
       \included" $ \(env, _) → do
        ws ← resetScene env
        writeIORef (statRNGRef env) (mkStdGen 91)
        before ← statRNG env
        _ ← spawnWith env ws 106 SpawnStandard
        statRNG env `shouldReturn` before

    it "is the same unit every time: two standard spawns agree on every \
       \value and every kit roll" $ \(env, _) → do
        ws ← resetScene env
        a ← spawnWith env ws 107 SpawnStandard
        writeIORef (statRNGRef env) (mkStdGen 5)
        b ← spawnWith env ws 107 SpawnStandard
        fingerprint b `shouldBe` fingerprint a

rolledSpec ∷ SpecWith (EngineEnv, LuaBackendState)
rolledSpec = describe "an ordinary spawn" $ do

    it "still rolls, drawing exactly the templates' rolls from a \
       \controlled generator" $ \(env, _) → do
        ws ← resetScene env
        def ← acolyteDef env
        let g0 = mkStdGen 2024 ∷ StdGen
            (rolled, g1) = rollTemplates (udStatTemplates def) g0
            (rolledB, g2) = rollTemplates (udBodyTemplates def) g1
            (skills, g3) = rollTemplates (udSkillTemplates def) g2
            (knowledge, _) = rollTemplates (udKnowledgeTemplates def) g3
        writeIORef (statRNGRef env) g0
        u ← spawnWith env ws 201 SpawnRolled
        uiStats u `shouldBe` seedBodyComposition (HM.union rolled rolledB)
        uiSkills u `shouldBe` skills
        uiKnowledge u `shouldBe` knowledge
        -- and that roll is not the standard unit: under this seed the
        -- skills leave their bases.
        uiSkills u `shouldNotBe` HM.map fst (udSkillTemplates def)
        after ← statRNG env
        after `shouldNotBe` show g0

portalSpec ∷ SpecWith (EngineEnv, LuaBackendState)
portalSpec = describe "the portal roster" $ do

    it "rolls by default: the queued spawn carries the rolled profile" $
        \(env, ls) → do
            _ ← resetScene env
            _ ← resetLua ls
            _ ← evalDebug ls
                "require('scripts.building_spawn').update(0.016); return 'ok'"
            cmds ← drainUnitQueue env
            [ p | UnitSpawn _ n _ _ _ _ _ _ p ← cmds, n ≡ acolyteName ]
                `shouldBe` [SpawnRolled]

    it "delivers a standard acolyte once a test switches the standard \
       \roster on, with no Haskell or Lua RNG draw and no reseed" $
        \(env, ls) → do
            _ ← resetScene env
            _ ← resetLua ls
            writeIORef (statRNGRef env) (mkStdGen 77)
            before ← statRNG env
            _ ← evalDebug ls $ T.intercalate " "
                [ "local BS = require('scripts.building_spawn');"
                , "BS.setTestStandardProfile(true);"
                , "BS.update(0.016); return 'ok'" ]
            cmds ← drainUnitQueue env
            [ p | UnitSpawn _ n _ _ _ _ _ _ p ← cmds, n ≡ acolyteName ]
                `shouldBe` [SpawnStandard]
            -- Commit it through the real dispatcher.
            forM_ cmds $ Q.writeQueue (unitQueue env)
            _ ← processAllUnitCommands env (ucUtsRef (toUnitCombatCapability env))
            um ← readIORef (unitManagerRef env)
            def ← acolyteDef env
            case HM.elems (umInstances um) of
                [u] → do
                    uiFactionId u `shouldBe` FactionPlayer
                    uiSkills u `shouldBe` HM.map fst (udSkillTemplates def)
                    stat u "carrying_capacity" `shouldSatisfy` approx 23.887
                other → expectationFailure
                    ("expected one portal acolyte, got " <> show (length other))
            statRNG env `shouldReturn` before
            evalDebug ls "return tostring(RNG.random) .. '|' .. tostring(RNG.seed)"
                `shouldReturn` "0|0"

    it "switches back off on session teardown, a save load and shutdown, \
       \so it cannot leak into the next session" $ \(_, ls) → do
        _ ← resetLua ls
        let probe hook = evalDebug ls $ T.intercalate " "
                [ "local BS = require('scripts.building_spawn');"
                , "BS.setTestStandardProfile(true);", hook
                , "return tostring(BS.standardProfile)" ]
        probe "BS.onSaveLoaded({}, {});" `shouldReturn` "false"
        probe "BS.shutdown();" `shouldReturn` "false"
        probe (T.intercalate " "
            [ "BS.init();"
            , "local ST = require('scripts.lib.session_teardown');"
            , "ST.runAll(); ST.beginSession();" ]) `shouldReturn` "false"
        -- and only an explicit true turns it on.
        evalDebug ls
            "local BS = require('scripts.building_spawn'); \
            \BS.setTestStandardProfile('yes'); \
            \return tostring(BS.standardProfile)"
            `shouldReturn` "false"

luaApiSpec ∷ SpecWith (EngineEnv, LuaBackendState)
luaApiSpec = describe "unit.spawn's profile argument" $ do

    it "queues the standard profile only for an explicit \"standard\"" $
        \(env, ls) → do
            _ ← resetScene env
            _ ← evalDebug ls $ T.concat
                [ "unit.spawn('acolyte', 2.5, 3.5, nil, 'player', '"
                , unWorldPageId page, "', nil, 'standard');"
                , "unit.spawn('acolyte', 2.5, 3.5, nil, 'player', '"
                , unWorldPageId page, "', nil, 'rolled');"
                , "unit.spawn('acolyte', 2.5, 3.5, nil, 'player', '"
                , unWorldPageId page, "');"
                , "return 'ok'" ]
            cmds ← drainUnitQueue env
            [ p | UnitSpawn _ _ _ _ _ _ _ _ p ← cmds ]
                `shouldBe` [SpawnStandard, SpawnRolled, SpawnRolled]

    it "refuses an unknown profile before allocating an id or queueing \
       \anything" $ \(env, ls) → do
        _ ← resetScene env
        idBefore ← umNextId ⊚ readIORef (unitManagerRef env)
        r ← evalDebug ls $ T.concat
            [ "return tostring(unit.spawn('acolyte', 2.5, 3.5, nil, 'player', '"
            , unWorldPageId page, "', nil, 'strong') == -1)" ]
        r `shouldBe` "true"
        n ← evalDebug ls $ T.concat
            [ "return tostring(unit.spawn('acolyte', 2.5, 3.5, nil, 'player', '"
            , unWorldPageId page, "', nil, 1) == -1)" ]
        n `shouldBe` "true"
        length ⊚ drainUnitQueue env `shouldReturn` 0
        (umNextId ⊚ readIORef (unitManagerRef env)) `shouldReturn` idBefore
