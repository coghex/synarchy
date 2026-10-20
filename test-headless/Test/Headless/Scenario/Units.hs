{-# LANGUAGE OverloadedStrings #-}
-- | The "Scenario.Units" gate (#2710, epic #2698 SCN-10): scenario unit
--   construction and capture against the real engine.
--
--   Nothing here fakes a layer. Definitions are the SHIPPED acolyte (and
--   a lazy, bodyless definition derived from it and registered in the
--   unit manager), loaded through the real YAML verbs into a headless
--   engine; documents go through the real "Scenario.Schema" decoder with
--   the catalog "Scenario.Units.Catalog" builds from the live
--   definitions; construction is "Scenario.Units.Build" with the real
--   body, physiology, item and modifier authorities; installation is
--   "Scenario.Units.Install" against the engine's own manager refs and
--   page-lifecycle lock; the paused hooks are the production Lua modules.
--
--   The fresh-process comparison re-runs THIS test binary twice as child
--   processes (see 'freshProcessSpec'); the canonical outputs of both
--   runs are retained under the system temporary directory.
--
--   Run just this gate: @cabal test synarchy-test-headless
--   --test-options='--match "Scenario.Units"'@.
module Test.Headless.Scenario.Units (spec) where

import UPrelude
import Test.Hspec
import Control.Exception (bracket_)
import Data.IORef (atomicModifyIORef', modifyIORef', newIORef, readIORef, writeIORef)
import Data.List (sort, sortOn)
import qualified Data.Aeson as A
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Lazy as BL
import qualified Data.HashMap.Strict as HM
import qualified Data.HashSet as HS
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import GHC.Float (castFloatToWord32)
import Item.Ground (GroundItems(..))
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as VU
import qualified Data.Yaml as Y
import System.Directory (createDirectoryIfMissing, getTemporaryDirectory, doesFileExist)
import System.Environment (getEnvironment, getExecutablePath, lookupEnv)
import System.Exit (ExitCode(..))
import System.FilePath ((</>))
import System.Process (CreateProcess(..), proc, readCreateProcessWithExitCode)
import System.Random (StdGen, mkStdGen, randomR)

import Combat.Resolution.Common (painFor)
import Engine.Asset.Discovery (walkFilesWithExtension)
import Engine.Core.Capability.ContentRegistries
    (ContentRegistriesCapability(..), toContentRegistriesCapability)
import Engine.Core.Capability.ContentRegistriesView
    (ContentRegistriesViewCapability(..), toContentRegistriesViewCapability)
import Engine.Core.Capability.UnitCombat (UnitCombatCapability(..), toUnitCombatCapability)
import Engine.Core.Capability.WorldSim (toWorldSimCapability, withPageLifecycle)
import Engine.Core.Init (EngineInitResult(..))
import Engine.Core.ReadOnlyRef (readReadOnlyRef)
import Engine.Core.State (EngineEnv(..), freshItemInstanceId)
import Engine.Core.Thread (ThreadControl(..))
import Engine.Scripting.Lua.API (registerLuaAPI)
import Engine.Scripting.Lua.Thread (createLuaBackendState)
import Engine.Scripting.Lua.Thread.Console (executeDebugLua)
import Engine.Scripting.Lua.Types (LuaBackendState(..))
import Gameplay.Tags.Index (categoryTagsOf, emptyCategoryIndex, nullCategoryIndex)
import Gameplay.Tags.Tag (mkItemTarget)
import Gameplay.Tags.Types (GameplayTag, mkGameplayTag)
import Infection.Types (InfectionManager(..))
import Item.Types (ItemInstance(..), ItemManager, itemTotalWeight)
import Scenario.Items.Build (ItemBuildEnv(..), itemCatalogOf)
import Scenario.Items.Capture (CaptureCtx(..))
import Scenario.Items.Refs (itemRefsToAscList, lookupItemRef, nullItemRefs)
import Scenario.Schema (decodeScenarioBytes)
import Scenario.Types
import Scenario.Units.Build
import Scenario.Units.Capture
import Scenario.Units.Catalog
import Scenario.Units.Defaults (rollTemplatesFor)
import Scenario.Units.Encode (unitEntryValue)
import Scenario.Units.Install
import Scenario.Units.Ready (luaUnitReadiness)
import Structure.Types (emptyChunkStructures)
import Test.Headless.Harness.Isolation (withIsolatedResourceRoot)
import Test.Headless.Harness.Log (initializeEngineHeadlessQuiet)
import Unit.Blood (bloodViewOf)
import Unit.Sim.Types (Pose(..), UnitSimState(..), UnitThreadState(..))
import Unit.Stats (effectiveStat)
import Unit.Thread.Command.Body (injurySpeedMult)
import Combat.Wounds (tickAllWounds)
import Unit.Anim (poseTag)
import Unit.Command.Types (SpawnProfile(..), UnitCommand(..))
import qualified Engine.Core.Queue as Q
import Unit.Thread.Command (processAllUnitCommands)
import Unit.Thread.Command.Spawn (handleUnitSpawnCommand)
import Unit.Thread.Command.Spawn (spawnModifierMap)
import World.Save.Types (SaveMetadata, UnitSnapshot(..), toUnitSnapshot, fromUnitSnapshot)
import World.Save.Snapshot (SessionGlobals(..), SessionSnapshot(..), PageSnapshot(..), LiveCameraSnapshot(..), captureSessionSnapshot)
import World.Save.Snapshot.Adapter (SaveRequestMeta(..), snapshotSaveMetadata)
import World.Save.Envelope (decodeSessionEnvelope, encodeSessionSnapshot)
import World.Save.Component.EntitySnapshots
    (UnitsDTO(..), PageUnitsDTO(..), UnitsDTOv5(..), PageUnitsDTOv5(..), unitsCodec)
import World.Save.Component.Types (ComponentCodec(..))
import World.Save.Integrity (IntegrityError(..), sessionIntegrityErrors, sessionIntegrityWarnings)
import Test.Headless.World.Save.Components.Fixture (minimalGlobals, minimalPage, minimalUnitInstance, minimalSimState)
import qualified Data.Serialize as S
import Unit.Faction (Faction(..))
import Unit.Faction.Membership (resolveSpawnFaction)
import Unit.Types
import World.Chunk.Admit (pageIncarnation)
import World.Chunk.Types (ChunkCoord(..), ColumnTiles(..), LoadedChunk(..))
import World.Flora.Types (emptyFloraChunkData)
import World.Fluid.Types (emptyIceMap)
import World.Generate.Types (WorldGenParams(..), defaultWorldGenParams)
import World.Page.Types (WorldPageId(..))
import World.State.Types (WorldManager(..), WorldState(..), emptyWorldManager, emptyWorldState)
import World.Tile.Types (WorldTileData(..))

-- * Scene

pageP ∷ WorldPageId
pageP = WorldPageId "scenario_units"

terrainZ ∷ Int
terrainZ = 4

-- | The scenario start time: every wound age and modifier remaining
--   time below is exact against it.
startTime ∷ Double
startTime = 4096

lazyName ∷ Text
lazyName = "scenario_lazy"

flatChunk ∷ LoadedChunk
flatChunk =
    let area = 16 * 16
        col = ColumnTiles { ctStartZ = terrainZ, ctMats = VU.singleton 1
                          , ctSlopes = VU.singleton 0, ctVeg = VU.singleton 0 }
    in LoadedChunk
        { lcCoord = ChunkCoord 0 0, lcTiles = V.replicate area col
        , lcSurfaceMap = VU.replicate area terrainZ
        , lcTerrainSurfaceMap = VU.replicate area terrainZ
        , lcFluidMap = V.replicate area Nothing, lcIceMap = emptyIceMap
        , lcFlora = emptyFloraChunkData, lcSideDeco = VU.replicate area 0
        , lcWaterTableMap = VU.replicate area 0, lcMagma = Nothing
        , lcStructures = emptyChunkStructures }

-- | One flat page, no units, no items, game time 'startTime'. Returns
--   the page so a test can read its incarnation.
resetScene ∷ EngineEnv → IO WorldState
resetScene env = do
    ws ← emptyWorldState
    writeIORef (wsTilesRef ws) WorldTileData
        { wtdChunks = HM.singleton (ChunkCoord 0 0) flatChunk, wtdMaxChunks = 1, wtdBounds = Nothing }
    writeIORef (wsGenParamsRef ws) (Just defaultWorldGenParams { wgpWorldSize = 8 })
    writeIORef (worldManagerRef env) emptyWorldManager
        { wmWorlds = [(pageP, ws)], wmVisible = [pageP] }
    atomicModifyIORef' (unitManagerRef env) $ \um →
        (um { umInstances = HM.empty, umTags = emptyCategoryIndex, umScenarioRefs = Map.empty
            , umSelected = HS.empty }, ())
    atomicModifyIORef' (ucUtsRef (toUnitCombatCapability env)) $ \uts →
        (uts { utsSimStates = HM.empty }, ())
    writeIORef (gameTimeRef env) startTime
    writeIORef (enginePausedRef env) True
    pure ws

-- * Engine fixture

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

loadShipped ∷ LuaBackendState → IO ()
loadShipped ls = do
    items ← sort ⊚ walkFilesWithExtension "data/items" ".yaml"
    forM_ items $ \f → load "loadItemYaml" ("data/items" </> f)
    load "loadEquipmentYaml" "data/equipment/humanoid.yaml"
    load "loadFactionYaml" "data/factions/base.yaml"
    load "loadPhysiologyYaml" "data/physiology/systems.yaml"
    load "loadInfectionYaml" "data/infections/bacteria.yaml"
    load "loadUnitYaml" "data/units/acolyte.yaml"
  where
    load verb path = void $ evalDebug ls $ T.concat
        [ "engine.", verb, "('", T.pack path, "'); return 'loaded'" ]

-- | Count every gameplay draw from Lua's stream, so a test can prove
--   construction and capture make none; the AI module is required FIRST
--   so the resource modules' satellites see the real singleton.
installLua ∷ LuaBackendState → IO ()
installLua ls = void $ evalDebug ls $ T.intercalate " "
    [ "_G.RNG = { random = 0, seed = 0 };"
    , "local realRandom, realSeed = math.random, math.randomseed;"
    , "math.random = function(...) RNG.random = RNG.random + 1; return realRandom(...) end;"
    , "math.randomseed = function(...) RNG.seed = RNG.seed + 1; return realSeed(...) end;"
    , "require('scripts.unit_ai');"
    , "_G.__res = require('scripts.unit_resources'); __res.init(0);"
    , "_G.__starv = require('scripts.starvation');"
    , "return 'ok'" ]

-- | The lazy, bodyless definition: the acolyte's templates without its
--   body or physiology, rolled on demand.
lazyDef ∷ UnitDef → UnitDef
lazyDef d = d
    { udName = lazyName, udNamePool = Nothing, udEagerStats = False
    , udStatTemplates = HM.fromList
        [ ("strength", (3.0, 2.0)), ("agility", (1.0, 0.5)), ("endurance", (1.0, 1.0))
        , ("carrying_capacity", (20, 10)) ]
    , udBodyTemplates = HM.empty
    , udSkillTemplates = HM.fromList [("unarmed", (10, 6))]
    , udKnowledgeTemplates = HM.empty
    , udStartingInventory = [], udStartingEquipment = HM.empty, udStartingAccessories = []
    , udEquipmentClass = Nothing, udModifiers = []
    , udPhysiology = noUnitPhysiology }

setup ∷ (Fx → IO ()) → IO ()
setup act = withIsolatedResourceRoot $ do
    EngineInitResult env ← initializeEngineHeadlessQuiet
    ls ← newBareLuaBackend env
    loadShipped ls
    installLua ls
    acolyte ← defNamed env "acolyte"
    modifyIORef' (unitManagerRef env) $ \um →
        um { umDefs = HM.insert lazyName (lazyDef acolyte) (umDefs um) }
    act (Fx env ls)

defNamed ∷ EngineEnv → Text → IO UnitDef
defNamed env n = do
    um ← readIORef (unitManagerRef env)
    maybe (fail ("definition " <> T.unpack n <> " is not registered")) pure (HM.lookup n (umDefs um))

-- | Run with one definition replaced, restoring it after.
withDefChange ∷ EngineEnv → Text → (UnitDef → UnitDef) → IO α → IO α
withDefChange env n f act = do
    original ← defNamed env n
    let set d = modifyIORef' (unitManagerRef env) $ \um → um { umDefs = HM.insert n d (umDefs um) }
    bracket_ (set (f original)) (set original) act

-- * Registries

itemsOf ∷ EngineEnv → IO ItemManager
itemsOf env = readReadOnlyRef (crvItemManagerRef (toContentRegistriesViewCapability env))

catalogOf ∷ EngineEnv → IO ScenarioCatalog
catalogOf env = do
    um ← readIORef (unitManagerRef env)
    reg ← readIORef (crPhysiologyRegistryRef (toContentRegistriesCapability env))
    ecm ← readReadOnlyRef (crvEquipmentClassManagerRef (toContentRegistriesViewCapability env))
    items ← itemsOf env
    InfectionManager inf ← readIORef (crInfectionManagerRef (toContentRegistriesCapability env))
    pure emptyScenarioCatalog
        { catUnits = unitCatalogOf reg ecm (umDefs um)
        , catItems = itemCatalogOf items
        , catInfections = HS.fromList (HM.keys inf)
        , catKnowledge = HS.fromList
            [ k | d ← HM.elems (umDefs um), k ← HM.keys (udKnowledgeTemplates d) ] }

-- | A units document through the real decoder.
decodeUnits ∷ EngineEnv → String → IO ([UnitEntry], [ScenarioDiagnostic])
decodeUnits env yaml = do
    cat ← catalogOf env
    case decodeScenarioBytes cat (BC.pack ("version: 1\nunits:\n" ⧺ yaml)) of
        ScenarioLoaded s ds → pure (scUnits s, ds)
        ScenarioFailed f → fail ("scenario failed: " ⧺ show f)

cleanUnits ∷ EngineEnv → String → IO [UnitEntry]
cleanUnits env yaml = do
    (us, ds) ← decodeUnits env yaml
    ds `shouldBe` []
    pure us

buildEnvOf ∷ EngineEnv → IO UnitBuildEnv
buildEnvOf env = do
    um ← readIORef (unitManagerRef env)
    items ← itemsOf env
    reg ← readIORef (crPhysiologyRegistryRef (toContentRegistriesCapability env))
    ecm ← readReadOnlyRef (crvEquipmentClassManagerRef (toContentRegistriesViewCapability env))
    inf ← readIORef (crInfectionManagerRef (toContentRegistriesCapability env))
    now ← readIORef (gameTimeRef env)
    pure UnitBuildEnv
        { ubeDefs = umDefs um
        , ubeItems = ItemBuildEnv items (freshItemInstanceId env)
        , ubeEquipment = ecm, ubePhysiology = reg, ubeInfections = inf
        , ubeNow = now, ubePage = pageP
        , ubeSurfaceZ = \_ _ → Just terrainZ }

installRefsOf ∷ Fx → UnitInstallRefs
installRefsOf fx = UnitInstallRefs
    { uirUnits = unitManagerRef env
    , uirSims = ucUtsRef (toUnitCombatCapability env)
    , uirBuildings = buildingManagerRef env
    , uirWorld = worldManagerRef env
    , uirLifecycle = withPageLifecycle (toWorldSimCapability env)
    , uirReady = luaUnitReadiness (lbsLuaState (fxLua fx)) }
  where env = fxEnv fx

-- | Build every entry (document paths @units[i]@) and install the
--   accepted ones on the current incarnation of 'pageP'.
construct ∷ Fx → WorldState → [UnitEntry] → IO ([UnitCompletion], [ScenarioDiagnostic])
construct fx ws entries = do
    let env = fxEnv fx
    benv ← buildEnvOf env
    builds ← forM (zip [0 ∷ Int ..] entries) $ \(i, e) →
        buildUnit benv ("units[" <> tshow i <> "]") e
    epoch ← pageIncarnation ws
    cs ← installUnitBuilds (installRefsOf fx) pageP epoch builds
    pure (cs, concatMap ubDiagnostics builds)

-- | Construct a document that must produce no diagnostic and install
--   every unit.
constructClean ∷ Fx → WorldState → String → IO [UnitId]
constructClean fx ws yaml = do
    us ← cleanUnits (fxEnv fx) yaml
    (cs, ds) ← construct fx ws us
    ds `shouldBe` []
    forM cs $ \case
        UnitInstalled uid _ → pure uid
        other → fail ("not installed: " ⧺ show other)

unitOf ∷ EngineEnv → UnitId → IO UnitInstance
unitOf env uid = do
    um ← readIORef (unitManagerRef env)
    maybe (fail ("no unit " ⧺ show uid)) pure (HM.lookup uid (umInstances um))

simOf ∷ EngineEnv → UnitId → IO UnitSimState
simOf env uid = do
    uts ← readIORef (ucUtsRef (toUnitCombatCapability env))
    maybe (fail ("no sim state " ⧺ show uid)) pure (HM.lookup uid (utsSimStates uts))

-- | Capture every unit on the page from one read of the managers.
captureAll ∷ EngineEnv → IO UnitsCapture
captureAll env = do
    um ← readIORef (unitManagerRef env)
    wm ← readIORef (worldManagerRef env)
    items ← itemsOf env
    reg ← readIORef (crPhysiologyRegistryRef (toContentRegistriesCapability env))
    now ← readIORef (gameTimeRef env)
    let itemCtx = CaptureCtx
            { ccRef = \i → lookupItemRef i (wmItemScenarioRefs wm)
            , ccTags = \i → maybe Set.empty (\t → categoryTagsOf t (wmItemTags wm)) (mkItemTarget i) }
        ctx = UnitCaptureCtx
            { uccDefs = umDefs um, uccItems = items, uccPhysiology = reg, uccNow = now
            , uccUnitRef = \u → Map.lookup u (umScenarioRefs um)
            , uccUnitTags = \u → categoryTagsOf u (umTags um)
            , uccItemCtx = itemCtx
            , uccOtherTrees = [], uccPreNamed = const Nothing, uccEncounterOf = const Nothing }
    pure (captureUnits ctx [] (sortOn fst (HM.toList (umInstances um))))

-- | The captured entries as one YAML document the decoder reads.
captureDocument ∷ UnitsCapture → BC.ByteString
captureDocument c = Y.encode (A.object [ ("version", A.toJSON (1 ∷ Int))
                                       , ("units", A.toJSON (map unitEntryValue (ucapEntries c))) ])

statRNG ∷ EngineEnv → IO String
statRNG env = show ⊚ readIORef (statRNGRef env)

luaDraws ∷ Fx → IO Text
luaDraws fx = evalDebug (fxLua fx) "return tostring(RNG.random) .. ',' .. tostring(RNG.seed)"

tag ∷ Text → GameplayTag
tag t = fromMaybe (error "empty tag") (mkGameplayTag t)

stat ∷ UnitInstance → Text → Float
stat u k = HM.lookupDefault (-1) k (uiStats u)

approx ∷ Float → Float → Bool
approx a b = abs (a - b) ≤ 1.0e-4 * max 1 (abs b)

allItems ∷ UnitInstance → [ItemInstance]
allItems u = concatMap flat (uiInventory u ⧺ HM.elems (uiEquipment u) ⧺ uiAccessories u)
  where flat i = i : concatMap flat (iiContents i)

-- * Spec

spec ∷ Spec
spec = describe "Scenario.Units" $ do
    freshProcessSpec
    seamStaticSpec
    aroundAll setup $ do
        defaultsSpec
        overrideSpec
        bodyResourceSpec
        healthSpec
        loadoutSpec
        roundTripSpec
        rejectionSpec
        lifecycleSpec
        persistenceSpec

-- ** Omitted values

defaultsSpec ∷ SpecWith Fx
defaultsSpec = describe "omitted values" $ do

    it "an eager, body-bearing unit is completely initialized before completion, drawing no gameplay RNG" $ \fx → do
        let env = fxEnv fx
        ws ← resetScene env
        before ← statRNG env
        draws0 ← luaDraws fx
        [uid] ← constructClean fx ws "  - {id: scout, definition: acolyte, x: 2.5, y: 3.5}\n"
        u ← unitOf env uid
        def ← defNamed env "acolyte"
        -- every stat template, the body composition and its derived values
        forM_ (HM.keys (udStatTemplates def) ⧺ bodyMassStats ⧺ bodyDerivedStats ⧺ ["strength_base"]) $ \k →
            (k, HM.member k (uiStats u)) `shouldBe` (k, True)
        -- the spawn-only seed inputs are consumed, as at spawn
        HM.member "bulk" (uiStats u) `shouldBe` False
        HM.keys (uiSkills u) `shouldMatchList` HM.keys (udSkillTemplates def)
        HM.keys (uiKnowledge u) `shouldMatchList` HM.keys (udKnowledgeTemplates def)
        uiName u `shouldSatisfy` (not ∘ T.null)
        -- every Simulated pool is seeded, at its maximum
        stat u "hydration" `shouldSatisfy` approx (stat u "max_hydration")
        stat u "calories" `shouldSatisfy` approx (stat u "max_calories")
        uiBlood u `shouldSatisfy` approx (stat u "body_mass" * bloodMassRatio)
        -- the definition's whole kit, nothing shed
        map iiDefName (uiInventory u) `shouldBe` [ n | (n, _, _) ← udStartingInventory def ]
        sort (HM.keys (uiEquipment u)) `shouldBe` sort (HM.keys (udStartingEquipment def))
        map iiDefName (uiAccessories u) `shouldBe` udStartingAccessories def
        -- the matching simulation state
        ss ← simOf env uid
        (usRealX ss, usRealY ss, usGridZ ss, usPose ss) `shouldBe` (2.5, 3.5, terrainZ, Standing)
        after ← statRNG env
        after `shouldBe` before
        luaDraws fx `shouldReturn` draws0

    it "a lazy, bodyless unit resolves every template up front, so a later read draws nothing" $ \fx → do
        let env = fxEnv fx
        ws ← resetScene env
        [uid] ← constructClean fx ws ("  - {id: grazer, definition: " ⧺ T.unpack lazyName ⧺ ", x: 1, y: 1}\n")
        u ← unitOf env uid
        -- every template is resolved (the readiness pass also writes the
        -- injury failure meters, which are not scenario values)
        HM.keys (templateStats lazyStatNames u) `shouldMatchList` lazyStatNames
        uiBlood u `shouldBe` 0
        HM.member "body_mass" (uiStats u) `shouldBe` False
        before ← statRNG env
        v ← evalDebug (fxLua fx) ("return unit.getStat(" <> tshow (unUnitId uid) <> ", 'agility')")
        (realToFrac (read (T.unpack v) ∷ Double) ∷ Float) `shouldBe` stat u "agility"
        statRNG env `shouldReturn` before
        u' ← unitOf env uid
        uiStats u' `shouldBe` uiStats u

    it "repeats the same values for the same identity and varies them between identities" $ \fx → do
        let env = fxEnv fx
            doc = "  - {id: a, definition: acolyte, x: 1, y: 1}\n  - {id: b, definition: acolyte, x: 2, y: 2}\n"
        ws ← resetScene env
        [a1, b1] ← constructClean fx ws doc
        ua1 ← unitOf env a1
        ub1 ← unitOf env b1
        -- perturb everything that must not matter, then build again
        replicateM_ 97 (atomicModifyIORef' (statRNGRef env) (\g → let (_, g') = randomR (0 ∷ Int, 9) g in (g', ())))
        replicateM_ 13 (void (freshItemInstanceId env))
        ws2 ← resetScene env
        modifyIORef' (unitManagerRef env) (\um → um { umNextId = umNextId um + 41 })
        [bId, aId] ← constructClean fx ws2
            "  - {id: b, tags: [other], definition: acolyte, x: 2, y: 2}\n  - {id: a, tags: [edited], definition: acolyte, x: 1, y: 1}\n"
        ua2 ← unitOf env aId
        ub2 ← unitOf env bId
        aId `shouldNotBe` a1
        let visible u = (uiStats u, uiSkills u, uiKnowledge u, uiName u, uiBlood u
                        , map (\i → (iiDefName i, iiQuality i, iiWeight i)) (allItems u))
        visible ua2 `shouldBe` visible ua1
        visible ub2 `shouldBe` visible ub1
        uiStats ua1 `shouldNotBe` uiStats ub1

-- ** Overrides

overrideSpec ∷ SpecWith Fx
overrideSpec = describe "explicit values" $ do

    it "an explicit strength_base survives a changed definition default and its derived strength survives the paused hooks and the first tick" $ \fx → do
        let env = fxEnv fx
            ls = fxLua fx
        ws ← resetScene env
        [uid] ← withDefChange env "acolyte"
                    (\d → d { udStatTemplates = HM.insert "strength" (5.0, 0) (udStatTemplates d) }) $
                    constructClean fx ws "  - {id: s, definition: acolyte, x: 1, y: 1, stats: {strength_base: 1.2}}\n"
        u ← unitOf env uid
        stat u "strength_base" `shouldBe` 1.2
        let lm = stat u "lean_mass"
            h = stat u "height"
            derived = 1.2 * ((lm / (8.8 * h * h)) ** 0.7)
        stat u "strength" `shouldSatisfy` approx derived
        let uidT = tshow (unUnitId uid)
        _ ← evalDebug ls ("unit.recomputeBody(" <> uidT <> "); __res.onSaveLoaded({" <> uidT <> "}); return 'ok'")
        _ ← evalDebug ls ("__starv.refreshStrength(" <> uidT <> "); return 'ok'")
        u' ← unitOf env uid
        stat u' "strength_base" `shouldBe` 1.2
        stat u' "strength" `shouldSatisfy` approx derived

    it "a bodyless unit's explicit strength stays exactly as stated whatever the template becomes" $ \fx → do
        let env = fxEnv fx
        ws ← resetScene env
        [uid] ← withDefChange env lazyName
                    (\d → d { udStatTemplates = HM.insert "strength" (40, 0) (udStatTemplates d) }) $
                    constructClean fx ws ("  - {id: g, definition: " ⧺ T.unpack lazyName ⧺ ", x: 1, y: 1, stats: {strength: 12}}\n")
        u ← unitOf env uid
        stat u "strength" `shouldBe` 12

    it "a stat the definition gained after the file was written takes its fallback; stated ones are kept" $ \fx → do
        let env = fxEnv fx
            doc = "  - {id: g, definition: " ⧺ T.unpack lazyName ⧺ ", x: 1, y: 1, stats: {agility: 0.75}}\n"
        ws ← resetScene env
        [u1] ← constructClean fx ws doc
        base ← unitOf env u1
        ws2 ← resetScene env
        [u2] ← withDefChange env lazyName
                   (\d → d { udStatTemplates = HM.insert "focus" (2.0, 1.0) (udStatTemplates d) }) $
                   constructClean fx ws2 doc
        grown ← unitOf env u2
        stat grown "agility" `shouldBe` 0.75
        stat grown "strength" `shouldBe` stat base "strength"
        HM.member "focus" (uiStats grown) `shouldBe` True

    it "explicit zero values and explicit empty collections are authoritative" $ \fx → do
        let env = fxEnv fx
        ws ← resetScene env
        [uid] ← constructClean fx ws $ unlines
            [ "  - id: bare"
            , "    definition: acolyte"
            , "    x: 1"
            , "    y: 1"
            , "    name: \"\""
            , "    stats: {reflexes: 0, toughness: 0, exhaustion: 0}"
            , "    skills: {dagger: 0}"
            , "    knowledge: {}"
            , "    wounds: []"
            , "    scars: []"
            , "    immunities: {}"
            , "    immune_response: 0"
            , "    inventory: []"
            , "    equipment: {}"
            , "    accessories: []" ]
        u ← unitOf env uid
        (stat u "reflexes", stat u "toughness", stat u "exhaustion") `shouldBe` (0, 0, 0)
        HM.lookup "dagger" (uiSkills u) `shouldBe` Just 0
        uiKnowledge u `shouldBe` HM.empty
        uiName u `shouldBe` ""
        (uiInventory u, HM.null (uiEquipment u), uiAccessories u, uiWounds u) `shouldBe` ([], True, [], [])
        -- an empty kit has no accessory buffs to reconstruct
        def ← defNamed env "acolyte"
        items ← itemsOf env
        uiModifiers u `shouldBe` spawnModifierMap items def []

-- ** Body and resources

bodyResourceSpec ∷ SpecWith Fx
bodyResourceSpec = describe "body composition and resources" $ do

    it "keeps an explicit current body, a depleted pool and reduced blood through the paused hooks and the first tick" $ \fx → do
        let env = fxEnv fx
            ls = fxLua fx
        ws ← resetScene env
        [uid] ← constructClean fx ws $ unlines
            [ "  - id: thin"
            , "    definition: acolyte"
            , "    x: 1"
            , "    y: 1"
            , "    stats: {height: 1.8, frame_mass: 71.28, body_mass: 60, lean_mass: 14.8, fat_mass: 3.2,"
            , "            strength_base: 1.0, hydration: 4.0, calories: 600}"
            , "    blood: 3.2" ]
        u ← unitOf env uid
        -- no seed margin was applied: a lean mass below the spawn margin
        -- (1.07 × 0.20 × frame_mass ≈ 15.25) is kept exactly
        (stat u "lean_mass", stat u "fat_mass", stat u "body_mass") `shouldBe` (14.8, 3.2, 60)
        stat u "max_hydration" `shouldSatisfy` approx (60 * 0.6)
        stat u "hydration" `shouldBe` 4.0
        stat u "calories" `shouldBe` 600
        uiBlood u `shouldBe` 3.2
        -- the omitted pools take their seeds from the FINAL maxima
        stat u "hunger" `shouldSatisfy` approx (stat u "max_hunger")
        let uidT = tshow (unUnitId uid)
        _ ← evalDebug ls ("unit.recomputeBody(" <> uidT <> "); __res.onSaveLoaded({" <> uidT <> "}); return 'ok'")
        u1 ← unitOf env uid
        uiStats u1 `shouldBe` uiStats u
        uiBlood u1 `shouldBe` uiBlood u
        -- one real resource pass: ordinary change, never a refill
        writeIORef (enginePausedRef env) False
        r ← evalDebug ls "__res.update(0.5); return 'ok'"
        r `shouldBe` "ok"
        u2 ← unitOf env uid
        -- hydration drains; digestion may move calories, but nothing
        -- refills either pool to its maximum
        stat u2 "hydration" `shouldSatisfy` (≤ 4.0)
        stat u2 "calories" `shouldSatisfy` (< 0.6 * stat u2 "max_calories")
        writeIORef (enginePausedRef env) True

    it "takes the spawn seed path from stated seed inputs alone" $ \fx → do
        let env = fxEnv fx
        ws ← resetScene env
        [uid] ← constructClean fx ws
            "  - {id: seeded, definition: acolyte, x: 1, y: 1, stats: {height: 2.0, bulk: 1.0, bodyfat: 0.2}}\n"
        u ← unitOf env uid
        stat u "frame_mass" `shouldSatisfy` approx (22 * 2.0 * 2.0 * 1.0)
        HM.member "bulk" (uiStats u) `shouldBe` False

-- ** Health

healthSpec ∷ SpecWith Fx
healthSpec = describe "health" $ do

    it "restores injured, treated, healing and infected wounds with their derived effects" $ \fx → do
        let env = fxEnv fx
        ws ← resetScene env
        [uid] ← constructClean fx ws $ unlines
            [ "  - id: hurt"
            , "    definition: acolyte"
            , "    x: 1"
            , "    y: 1"
            , "    wounds:"
            , "      - {part: l_leg, kind: slash, severity: 0.6, age: 64, bandage: 0.25, clot: 0.5,"
            , "         heal: 0.5, dressing: bandage, infection: 0.3, clean: true, infection_type: staph, necrosis: 0.1}"
            , "      - {part: torso, kind: blunt, severity: 0.2}"
            , "    scars: [{part: head, kind: slash, severity: 0.8, age: 1024}]"
            , "    immune_response: 0.4"
            , "    immunities: {staph: 0.5}" ]
        u ← unitOf env uid
        def ← defNamed env "acolyte"
        case uiWounds u of
            [w1, w2] → do
                (woundPart w1, woundSeverity w1, woundAt w1, woundBandage w1, woundClot w1)
                    `shouldBe` ("l_leg", 0.6, startTime - 64, 0.25, 0.5)
                (woundHeal w1, woundDressing w1, woundInfection w1, woundClean w1
                    , woundInfectionType w1, woundNecrosis w1)
                    `shouldBe` (0.5, "bandage", 0.3, True, "staph", 0.1)
                woundEffSeverity w1 `shouldSatisfy` approx 0.3
                -- an omitted optional field takes the fresh-wound default
                (woundAt w2, woundBandage w2, woundHeal w2) `shouldBe` (startTime, 1, 0)
            other → expectationFailure ("wounds: " ⧺ show other)
        map scarAt (uiScars u) `shouldBe` [startTime - 1024]
        (uiImmuneResponse u, uiImmunities u) `shouldBe` (0.4, HM.singleton "staph" 0.5)
        -- the derived injury results come from the existing authorities
        let view = bloodViewOf (uiStats u) (uiBlood u) 0
        injurySpeedMult view (udBodyParts def) u `shouldSatisfy` (< 1)
        painFor u `shouldSatisfy` (> 0)

    it "a healed unit (wounds: []) has no injury effect" $ \fx → do
        let env = fxEnv fx
        ws ← resetScene env
        [uid] ← constructClean fx ws "  - {id: well, definition: acolyte, x: 1, y: 1, wounds: []}\n"
        u ← unitOf env uid
        def ← defNamed env "acolyte"
        injurySpeedMult (bloodViewOf (uiStats u) (uiBlood u) 0) (udBodyParts def) u `shouldBe` 1
        painFor u `shouldBe` 0

    describe "the paused baseline is what the ordinary path reaches at its first pass" $ do
        let cases =
                [ ("a concussion knocks the unit out", "{part: head, kind: concussion, severity: 0.4}", "", "collapsed")
                , ("a shattered leg leaves a lucid unit crawling", "{part: l_leg, kind: fracture, severity: 0.9}", "", "crawling")
                , ("a bleeding unit below the unconscious threshold collapses", "{part: torso, kind: slash, severity: 0.3}", ", blood: 0.5", "collapsed")
                , ("a starving unit already has its discounted strength", "{part: torso, kind: blunt, severity: 0.1}", ", stats: {calories: 0, hunger: 0}", "standing")
                , ("a lightly hurt unit stands", "{part: torso, kind: blunt, severity: 0.1}", "", "standing") ]
        forM_ cases $ \(name, wound, extra, wantPose) →
            it name $ \fx → do
                let env = fxEnv fx
                ws ← resetScene env
                [uid] ← constructClean fx ws $
                    "  - {id: hurt, definition: acolyte, x: 1, y: 1, wounds: [" ⧺ wound ⧺ "]" ⧺ extra ⧺ "}\n"
                a ← unitOf env uid
                sa ← simOf env uid
                -- complete before any pass: instance and simulation agree
                (uiPose a, poseText (usPose sa)) `shouldBe` (wantPose, wantPose)
                -- zero-time reconciliation leaves the baseline alone
                _ ← evalDebug (fxLua fx) ("__starv.refreshStrength(" <> tshow (unUnitId uid) <> "); return 'ok'")
                a' ← unitOf env uid
                stat a' "strength" `shouldBe` stat a "strength"
                -- the ordinary path: the same unit state spawned normally,
                -- then one real wound tick and one real resource pass
                b ← ordinaryTwin fx ws a
                sb ← simOf env (fst b)
                poseText (usPose sb) `shouldBe` wantPose
                (stat a "strength" / stat a "strength_body")
                    `shouldSatisfy` approx (stat (snd b) "strength" / stat (snd b) "strength_body")

    it "over generated specs, readiness is a pure evaluation: an accepted unit keeps every authored value \
       \bit for bit and gets the ordinary path's verdict; a rejected one leaves no trace" $ \fx → do
        let env = fxEnv fx
            pick xs g = let (i, g') = randomR (0, length xs - 1) g in (xs !! i, g')
            statPool =
                [ ("stamina", ["0", "0.5", "3"]), ("endurance", ["0.2", "1"])
                , ("hydration", ["0", "1", "20"]), ("calories", ["0", "100"]), ("hunger", ["0"])
                , ("exhaustion", ["0", "5"]), ("sleep_pressure", ["0", "5"]) ]
            woundPool =
                [ "", "{part: head, kind: concussion, severity: 0.4}"
                , "{part: l_leg, kind: fracture, severity: 0.9}"
                , "{part: torso, kind: slash, severity: 0.3}"
                , "{part: heart, kind: stab, severity: 0.8, heal: -0.5}"
                , "{part: torso, kind: blunt, severity: 0.1}"
                , "{part: l_hand, kind: fracture, severity: 0.9}" ]
            bodyPool =
                [ "", "frame_mass: 70, body_mass: 60, lean_mass: 10, fat_mass: 10"
                , "frame_mass: 70, body_mass: 60, lean_mass: 14.3, fat_mass: 3"
                , "frame_mass: 70, body_mass: 60, lean_mass: 20, fat_mass: 8"
                , "frame_mass: 70, body_mass: 60, lean_mass: 20, fat_mass: 0" ]
            bloodPool = ["", ", blood: 0.5", ", blood: 1.5"]
            loadoutPool =
                [ "", ", equipment: {left_hand: {id: gen-dagger, tags: [blade], definition: steel_dagger}}" ]
            specOf seed =
                let g0 = mkStdGen seed
                    (stats, g1) = foldl' (\(acc, g) (k, vs) →
                                     let (keep, g') = randomR (0 ∷ Int, 2) g
                                         (v, g'') = pick vs g'
                                     in (if keep ≡ 0 then acc ⧺ [k ⧺ ": " ⧺ v] else acc, g''))
                                 ([], g0) statPool
                    (w, g2) = pick woundPool g1
                    (b, g3) = pick bodyPool g2
                    (bl, g4) = pick bloodPool g3
                    (lo, g5) = pick loadoutPool g4
                    (cold, _) = randomR (0 ∷ Int, 3) g5
                    statText = intercalate' ", " (filter (not ∘ null) (b : stats))
                in ( "  - {id: gen, tags: [gen-unit], definition: acolyte, x: 1, y: 1, stats: {" ⧺ statText ⧺ "}"
                     ⧺ ", wounds: [" ⧺ w ⧺ "]" ⧺ bl ⧺ lo ⧺ "}\n"
                   , cold ≡ 0 )
            setCold c = void $ evalDebug (fxLua fx) $
                "require('scripts.thermo').debugAmbient = " <> (if c then "-10" else "nil") <> "; return 'ok'"
        outcomes ← forM [1 .. 160 ∷ Int] $ \seed → do
            ws ← resetScene env
            let (doc, cold) = specOf seed
            setCold cold
            (us, _) ← decodeUnits env doc
            benv ← buildEnvOf env
            builds ← forM us (buildUnit benv "units[0]")
            r ← case [ (ub, b) | ub ← builds, Just b ← [ubUnit ub] ] of
                [] → pure "native-rejected"
                ((ub, b) : _) → do
                    epoch ← pageIncarnation ws
                    [c] ← installUnitBuilds (installRefsOf fx) pageP epoch [ub]
                    verdict ← case c of
                        UnitInstalled uid _ → do
                            u ← unitOf env uid
                            -- pure: every authored value is bit-identical
                            (seed, authoredState (buAuthoredStats b) u)
                                `shouldBe` (seed, authoredState (buAuthoredStats b) (buInstance b))
                            pure (Just (uiPose u, strengthRatio u))
                        UnitRejected _ → do
                            noTrace env seed ws
                            pure Nothing
                        other → fail ("seed " ⧺ show seed ⧺ ": " ⧺ show other)
                    -- the ordinary verdict; a cold arena's frostbite is applied by the
                    -- ordinary pass but, being no authored state, never by readiness,
                    -- so the twin is only compared where it applies none
                    unless cold $ do
                        ordinary ← ordinaryZeroTime fx ws (buInstance b)
                        (seed, verdict) `shouldBe`
                            (seed, (\(p, st, _, _) → (p, strengthRatioOf st)) ⊚ ordinary)
                    pure (maybe "rejected" fst verdict)
            setCold False
            pure (r, cold)
        let os = map fst outcomes
        -- the generated specs really reach every outcome, cold ones included
        nub' os `shouldSatisfy` (\xs → all (`elem` xs) ["rejected", "standing", "collapsed", "crawling"])
        length [ () | (o, True) ← outcomes, o ≢ "native-rejected" ] `shouldSatisfy` (> 10)

    describe "readiness performs nothing (round-4 regressions)" $ do
        it "a disabled hand holding a tagged, referenced dagger, with no stamina, is rejected without dropping it" $ \fx → do
            let env = fxEnv fx
                doc st = "  - {id: maimed, tags: [m-unit], definition: acolyte, x: 1, y: 1, stats: {stamina: " ⧺ st
                         ⧺ "}, wounds: [{part: l_hand, kind: fracture, severity: 0.9}], \
                           \equipment: {left_hand: {id: m-dagger, tags: [blade], definition: steel_dagger}}}\n"
            ws ← resetScene env
            us ← cleanUnits env (doc "0")
            (cs, _) ← construct fx ws us
            [ map sdEffect ds | UnitRejected ds ← cs ] `shouldBe` [[EntryRejected]]
            noTrace env 0 ws
            -- the same entry, alive, installs: nothing of the first attempt remains
            us2 ← cleanUnits env (doc "3")
            (cs2, _) ← construct fx ws us2
            [ () | UnitInstalled _ _ ← cs2 ] `shouldBe` [()]
            um ← readIORef (unitManagerRef env)
            -- and even a lucid unit keeps its dagger until the first real pass
            [ HM.member "left_hand" (uiEquipment u) | u ← HM.elems (umInstances um) ] `shouldBe` [True]
            ground ← readIORef (wsGroundItemsRef ws)
            HM.size (gisItems ground) `shouldBe` 0
        it "an explicit body and wounds: [] are kept exactly in a cold arena, starving" $ \fx → do
            let env = fxEnv fx
            ws ← resetScene env
            _ ← evalDebug (fxLua fx) "require('scripts.thermo').debugAmbient = -10; return 'ok'"
            us ← cleanUnits env "  - {id: frozen, definition: acolyte, x: 1, y: 1, stats: {frame_mass: 70, \
                                \body_mass: 60, lean_mass: 20, fat_mass: 1.5, calories: 0}, wounds: []}\n"
            benv ← buildEnvOf env
            [ub] ← forM us (buildUnit benv "units[0]")
            Just b ← pure (ubUnit ub)
            epoch ← pageIncarnation ws
            [c] ← installUnitBuilds (installRefsOf fx) pageP epoch [ub]
            _ ← evalDebug (fxLua fx) "require('scripts.thermo').debugAmbient = nil; return 'ok'"
            [UnitInstalled uid _] ← pure [c]
            u ← unitOf env uid
            authoredState (buAuthoredStats b) u `shouldBe` authoredState (buAuthoredStats b) (buInstance b)
            (stat u "fat_mass", stat u "body_mass", uiWounds u) `shouldBe` (1.5, 60, [])
        it "a fat mass below its floor rejects the four masses, and the unit takes a seeded body (manager ruling)" $ \fx → do
            let env = fxEnv fx
            ws ← resetScene env
            us ← cleanUnits env "  - {id: lean, definition: acolyte, x: 1, y: 1, stats: {frame_mass: 70, \
                                \body_mass: 60, lean_mass: 20, fat_mass: 0}}\n"
            (cs, ds) ← construct fx ws us
            [ () | UnitInstalled _ _ ← cs ] `shouldBe` [()]
            sort [ sdPath d | d ← ds, sdEffect d ≡ FieldRejected ] `shouldBe`
                sort [ "units[0].stats." <> k | k ← ["frame_mass", "body_mass", "lean_mass", "fat_mass"] ]

    describe "a baseline the resource pass would collapse or kill at once" $ do
        it "low stamina is collapsed before completion" $ \fx → do
            let env = fxEnv fx
            ws ← resetScene env
            [uid] ← constructClean fx ws "  - {id: tired, definition: acolyte, x: 1, y: 1, stats: {endurance: 1, stamina: 0.5}}\n"
            u ← unitOf env uid
            ss ← simOf env uid
            (uiPose u, usPose ss) `shouldBe` ("collapsed", Collapsed)
        forM_ [ ("no stamina is rejected", "stats: {stamina: 0}")
              , ("lean mass below the muscle floor is rejected"
                , "stats: {frame_mass: 70, body_mass: 60, lean_mass: 10, fat_mass: 10}") ] $ \(name, extra) →
            it name $ \fx → do
                let env = fxEnv fx
                ws ← resetScene env
                us ← cleanUnits env $ "  - {id: doomed, tags: [d-unit], definition: acolyte, x: 1, y: 1, "
                    ⧺ extra ⧺ ", inventory: [{id: d-kit, tags: [d-item], definition: first_aid_kit}]}\n"
                (cs, _) ← construct fx ws us
                [ map sdEffect ds | UnitRejected ds ← cs ] `shouldBe` [[EntryRejected]]
                um ← readIORef (unitManagerRef env)
                (HM.size (umInstances um), Map.size (umScenarioRefs um), nullCategoryIndex (umTags um))
                    `shouldBe` (0, 0, True)
                wm ← readIORef (worldManagerRef env)
                (nullCategoryIndex (wmItemTags wm), nullItemRefs (wmItemScenarioRefs wm)) `shouldBe` (True, True)

    it "a starving unit's strength survives capture and reconstruction while paused" $ \fx → do
        let env = fxEnv fx
        ws ← resetScene env
        [uid] ← constructClean fx ws "  - {id: thin, definition: acolyte, x: 1, y: 1, stats: {calories: 0}}\n"
        a ← unitOf env uid
        stat a "strength" `shouldSatisfy` approx (0.5 * stat a "strength_body")
        c ← captureAll env
        (us', _) ← decodeUnitsBytes env (captureDocument c)
        ws2 ← resetScene env
        (cs, ds) ← construct fx ws2 us'
        ds `shouldBe` []
        [UnitInstalled uid2 _] ← pure cs
        b ← unitOf env uid2
        (stat b "strength", stat b "strength_body") `shouldBe` (stat a "strength", stat a "strength_body")

    it "rejects a unit whose stated health already meets a death condition, with what it owns" $ \fx → do
        let env = fxEnv fx
        ws ← resetScene env
        us ← cleanUnits env $ unlines
            [ "  - id: gone"
            , "    definition: acolyte"
            , "    x: 1"
            , "    y: 1"
            , "    wounds: [{part: heart, kind: stab, severity: 1.0}]"
            , "    inventory: [{id: c1, definition: canteen_steel_2l}]"
            , "  - {id: bled, definition: acolyte, x: 1, y: 1, blood: 0}" ]
        before ← readIORef (nextItemInstanceIdRef env)
        (cs, ds) ← construct fx ws us
        um ← readIORef (unitManagerRef env)
        HM.size (umInstances um) `shouldBe` 0
        length [ () | UnitRejected _ ← cs ] `shouldBe` 2
        [ sdPath d | d ← ds, sdEffect d ≡ CascadeRejected ] `shouldBe` ["units[0].inventory[0]"]
        [ sdPath d | d ← ds, sdEffect d ≡ EntryRejected ] `shouldMatchList` ["units[0]", "units[1]"]
        -- rejected before anything was minted
        readIORef (nextItemInstanceIdRef env) `shouldReturn` before

-- ** Loadout and modifiers

loadoutSpec ∷ SpecWith Fx
loadoutSpec = describe "loadout and modifiers" $ do

    it "applies the definition's and every worn accessory's modifiers once, and reconstructs them once" $ \fx → do
        let env = fxEnv fx
        ws ← resetScene env
        [uid] ← constructClean fx ws $ unlines
            [ "  - id: geared"
            , "    definition: acolyte"
            , "    x: 1"
            , "    y: 1"
            , "    accessories: [{id: g1, definition: technogoggles, condition: 50}]"
            , "    modifiers: {perception: [{source: tonic, delta: 0.25, remaining: 128}]}" ]
        u ← unitOf env uid
        def ← defNamed env "acolyte"
        items ← itemsOf env
        let expected = HM.insertWith (flip (⧺)) "perception"
                           [StatModifier 0.25 "tonic" (Just (startTime + 128)) 0]
                           (spawnModifierMap items def (uiAccessories u))
        uiModifiers u `shouldBe` expected
        c ← captureAll env
        case ucapEntries c of
            [e] → ueModifiers e `shouldBe` Authored (HM.singleton "perception" [ModifierSpec "tonic" 0.25 0 (Just 128)])
            es → expectationFailure ("captured " ⧺ show (length es))
        -- reconstruct from the capture: the same modifiers, not doubled
        (us', _) ← decodeUnitsBytes env (captureDocument c)
        ws2 ← resetScene env
        (cs, ds) ← construct fx ws2 us'
        ds `shouldBe` []
        [UnitInstalled uid2 _] ← pure cs
        u2 ← unitOf env uid2
        uiModifiers u2 `shouldBe` uiModifiers u
        effectiveStat startTime (stat u2 "perception") (HM.lookupDefault [] "perception" (uiModifiers u2))
            `shouldSatisfy` approx (effectiveStat startTime (stat u "perception")
                                        (HM.lookupDefault [] "perception" (uiModifiers u)))

    it "keeps current nested contents and item tags, and a stated empty or overloaded loadout is neither replenished nor shed" $ \fx → do
        let env = fxEnv fx
            ls = fxLua fx
        ws ← resetScene env
        let plates = concat [ "{definition: granite_chunk}, " | _ ← [1 ∷ Int .. 8] ]
        [heavy, empty] ← constructClean fx ws $ unlines
            [ "  - id: mule"
            , "    definition: acolyte"
            , "    x: 1"
            , "    y: 1"
            , "    inventory: [{id: kit-1, tags: [medical], definition: first_aid_kit, contents: [{id: b1, tags: [spare], definition: bandage}]}, "
              ⧺ plates ⧺ "{definition: granite_chunk}]"
            , "  - {id: none, definition: acolyte, x: 2, y: 1, inventory: [], equipment: {}, accessories: []}" ]
        u ← unitOf env heavy
        e ← unitOf env empty
        length (uiInventory u) `shouldBe` 10
        (uiInventory e, HM.null (uiEquipment e), uiAccessories e) `shouldBe` ([], True, [])
        items ← itemsOf env
        let carried = sum (map (itemTotalWeight items) (uiInventory u ⧺ HM.elems (uiEquipment u) ⧺ uiAccessories u))
            cap = effectiveStat startTime (stat u "carrying_capacity")
                                (HM.lookupDefault [] "carrying_capacity" (uiModifiers u))
        carried `shouldSatisfy` (> cap)
        wm ← readIORef (worldManagerRef env)
        case uiInventory u of
            kit : _ → do
                map iiDefName (iiContents kit) `shouldBe` ["bandage"]
                let tagsOf i = maybe Set.empty (\t → categoryTagsOf t (wmItemTags wm)) (mkItemTarget i)
                tagsOf (iiInstanceId kit) `shouldBe` Set.singleton (tag "medical")
                map (tagsOf ∘ iiInstanceId) (iiContents kit) `shouldBe` [Set.singleton (tag "spare")]
            [] → expectationFailure "no inventory"
        -- ordinary gameplay still refuses the next insertion: the strict
        -- transfer policy judges the same effective capacity
        [giver] ← constructClean fx ws "  - {id: giver, definition: acolyte, x: 1, y: 1, inventory: [{definition: bandage}], equipment: {}, accessories: []}\n"
        g ← unitOf env giver
        -- scenario units take their definition's default tags and no
        -- controller (owner decision, docs/scenario_format.md); the
        -- player-transfer policy needs commandable endpoints, so both
        -- are handed to the local player here, isolating the capacity rule
        acolyteTags ← udFactionTags ⊚ defNamed env "acolyte"
        modifyIORef' (unitManagerRef env) $ \um → um
            { umInstances = foldl' (\m i → HM.adjust (\x → x { uiFaction = resolveSpawnFaction acolyteTags (Just FactionPlayer) }) i m)
                                   (umInstances um) [giver, heavy] }
        let iid = maybe 0 iiInstanceId (listToMaybe (uiInventory g))
        r ← evalDebug ls $ T.concat
            [ "local r = unit.checkTransfer({ source = { kind = 'unit', id = ", tshow (unUnitId giver)
            , " }, destination = { kind = 'unit', id = ", tshow (unUnitId heavy)
            , " }, items = { { instanceId = ", tshow iid, ", defName = 'bandage' } } });"
            , "return r.outcomes[1].reason" ]
        r `shouldBe` "receiver_full"

    it "rejects an item whose kind the slot does not accept, with its contents" $ \fx → do
        let env = fxEnv fx
        ws ← resetScene env
        us ← cleanUnits env "  - {id: odd, definition: acolyte, x: 1, y: 1, equipment: {boots: {definition: steel_dagger}}}\n"
        (cs, ds) ← construct fx ws us
        [UnitInstalled uid _] ← pure cs
        u ← unitOf env uid
        HM.member "boots" (uiEquipment u) `shouldBe` False
        map (\d → (sdPath d, sdEffect d)) ds `shouldBe` [("units[0].equipment.boots", FieldRejected)]

-- ** Round trip

roundTripSpec ∷ SpecWith Fx
roundTripSpec = describe "capture" $ do

    it "construct → change → capture → YAML → reconstruct is equivalent, under new runtime ids" $ \fx → do
        let env = fxEnv fx
        ws ← resetScene env
        [uid, _] ← constructClean fx ws $ unlines
            [ "  - id: lead"
            , "    tags: [squad]"
            , "    definition: acolyte"
            , "    x: 1.25"
            , "    y: 2.75"
            , "    facing: north-east"
            , "    wounds: [{part: l_leg, kind: slash, severity: 0.5, age: 32}]"
            , "  - {id: grazer, definition: " ⧺ T.unpack lazyName ⧺ ", x: 3, y: 3}" ]
        before ← statRNG env
        -- change supported state, as gameplay would
        modifyIORef' (unitManagerRef env) $ \um → um
            { umInstances = HM.adjust (\u → u { uiStats = HM.insert "hydration" 7.5 (uiStats u)
                                               , uiBlood = uiBlood u - 0.5
                                               , uiWounds = []
                                               , uiInventory = drop 1 (uiInventory u) }) uid (umInstances um) }
        c1 ← captureAll env
        statRNG env `shouldReturn` before
        map ueId (ucapEntries c1) `shouldBe` [ExplicitId "grazer", ExplicitId "lead"]
        let lead = [ e | e ← ucapEntries c1, ueId e ≡ ExplicitId "lead" ]
        map ueWounds lead `shouldBe` [Authored []]
        map (HM.lookup "hydration" ∘ ueStats) lead `shouldBe` [Just 7.5]
        map ueTags lead `shouldBe` [[tag "squad"]]
        (us', ds') ← decodeUnitsBytes env (captureDocument c1)
        ds' `shouldBe` []
        us' `shouldBe` ucapEntries c1
        ws2 ← resetScene env
        modifyIORef' (unitManagerRef env) (\um → um { umNextId = umNextId um + 100 })
        replicateM_ 31 (void (freshItemInstanceId env))
        (cs, ds) ← construct fx ws2 us'
        ds `shouldBe` []
        length [ () | UnitInstalled _ _ ← cs ] `shouldBe` 2
        c2 ← captureAll env
        ucapEntries c2 `shouldBe` ucapEntries c1

    it "captures a lazy unit's unresolved stats through the scenario fallback, leaving the live unit and gameplay RNG alone" $ \fx → do
        let env = fxEnv fx
        ws ← resetScene env
        [uid] ← constructClean fx ws ("  - {id: lz, definition: " ⧺ T.unpack lazyName ⧺ ", x: 1, y: 1}\n")
        -- as an ordinary lazy spawn leaves it: nothing resolved yet
        modifyIORef' (unitManagerRef env) $ \um → um
            { umInstances = HM.adjust (\u → u { uiStats = HM.empty }) uid (umInstances um) }
        before ← statRNG env
        c ← captureAll env
        statRNG env `shouldReturn` before
        u ← unitOf env uid
        uiStats u `shouldBe` HM.empty
        def ← defNamed env lazyName
        map ueStats (ucapEntries c) `shouldBe`
            [rollTemplatesFor' "lz" (udStatTemplates def)]
        -- (the readiness pass's injury meters are not captured)
        -- and the document builds the same values back
        (us', _) ← decodeUnitsBytes env (captureDocument c)
        ws2 ← resetScene env
        (cs, ds) ← construct fx ws2 us'
        ds `shouldBe` []
        [UnitInstalled uid2 _] ← pure cs
        u2 ← unitOf env uid2
        templateStats lazyStatNames u2 `shouldBe` rollTemplatesFor' "lz" (udStatTemplates def)

    it "names units structurally, whatever order the manager holds them in" $ \fx → do
        let env = fxEnv fx
        ws ← resetScene env
        _ ← constructClean fx ws "  - {definition: acolyte, x: 1, y: 1}\n  - {definition: acolyte, x: 2, y: 2}\n"
        -- forget the references: these are units a scenario did not name
        modifyIORef' (unitManagerRef env) (\um → um { umScenarioRefs = Map.empty })
        c1 ← captureAll env
        um ← readIORef (unitManagerRef env)
        -- the same units under swapped ids
        let swapped = HM.fromList (zip (sort (HM.keys (umInstances um))) (reverse (sortOn fst (HM.toList (umInstances um)))))
        writeIORef (unitManagerRef env) um { umInstances = HM.map snd swapped }
        c2 ← captureAll env
        map (\e → (ueId e, ueX e)) (ucapEntries c2) `shouldBe` map (\e → (ueId e, ueX e)) (ucapEntries c1)

    it "unit and item capture names are unique, persistable, never a document id, keep a free authored \
       \spelling, and do not depend on unit ids or manager order — over adversarial ids" $ \fx → do
        let env = fxEnv fx
        ws ← resetScene env
        [tmplId] ← constructClean fx ws
            "  - {id: tmpl, definition: acolyte, x: 1, y: 1, inventory: [{definition: bandage}], equipment: {}, accessories: []}\n"
        tmpl ← unitOf env tmplId
        um0 ← readIORef (unitManagerRef env)
        items ← itemsOf env
        reg ← readIORef (crPhysiologyRegistryRef (toContentRegistriesCapability env))
        bandage ← maybe (fail "no item") pure (listToMaybe (uiInventory tmpl))
        let longA = T.replicate 512 "a"
            unitExplicit = [ "scout", "units.0", "units.0-2", "units.1", "x", "a.b", "hold", longA ]
            unitAuto = [ "units[0]", "units[1]", "units[2]" ]
            itemPool = [ "scout", "units.0", "units.1", "units.0.inventory.0", "scout.inventory.0", "x-2", "y" ]
            docPool = [ "hold", "x", "units.0", "ground.0" ]
        forM_ [1 .. 300 ∷ Int] $ \seed → do
            let g0 = mkStdGen seed
                (n, g1) = randomR (2 ∷ Int, 7) g0
                (unitRefs, g2) = shuffleWith g1 (map Just (unitExplicit ⧺ unitAuto) ⧺ replicate 4 Nothing)
                (itemRefs, g3) = shuffleWith g2 (map Just itemPool ⧺ replicate 4 Nothing)
                (docIds, g4) = subsetOf docPool g3
                (xs, _) = foldl' (\(acc, g) _ → let (v, g') = randomR (0 ∷ Int, 3) g in (acc ⧺ [fromIntegral v], g'))
                                 ([], g4) [1 .. n]
                units = [ ( unitRef, itemRef
                          , tmpl { uiGridX = x, uiInventory = [bandage { iiInstanceId = fromIntegral (1000 + k) }] } )
                        | (k, unitRef, itemRef, x) ← zip4' [0 ..] (take n unitRefs) (take n (itemRefs ⧺ repeat Nothing)) xs ]
                itemRefMap = Map.fromList [ (fromIntegral (1000 + k), r) | (k, (_, Just r, _)) ← zip [0 ∷ Int ..] units ]
                run ids = captureUnits UnitCaptureCtx
                    { uccDefs = umDefs um0, uccItems = items, uccPhysiology = reg, uccNow = startTime
                    , uccUnitRef = \u → join (lookup u [ (i, r) | (i, (r, _, _)) ← zip ids units ])
                    , uccUnitTags = const Set.empty
                    , uccItemCtx = CaptureCtx { ccRef = (`Map.lookup` itemRefMap), ccTags = const Set.empty }
                    , uccOtherTrees = [], uccPreNamed = const Nothing, uccEncounterOf = const Nothing }
                    docIds [ (i, u) | (i, (_, _, u)) ← zip ids units ]
                idsA = map UnitId [1 .. fromIntegral n]
                (idsB, _) = shuffleWith (mkStdGen (seed * 7)) (map UnitId [50 .. 49 + fromIntegral n])
                cA = run idsA
                cB = run (reverse idsB)
                namesOf c = [ scenarioIdText (ueId e) | e ← ucapEntries c ]
                          ⧺ [ scenarioIdText (ieId i) | e ← ucapEntries c, i ← concatMap flatEntry (authoredOr [] (ueInventory e)) ]
                allNames = namesOf cA
            length (nub' allNames) `shouldBe` length allNames
            filter (`elem` docIds) allNames `shouldBe` []
            map (\t → (T.length t ≤ 512, T.all (\c → c ≢ '[' ∧ c ≢ ']' ∧ c ≢ ' ') t)) allNames
                `shouldSatisfy` all (≡ (True, True))
            -- an authored explicit unit reference is kept unless a document entry holds it
            forM_ (zip idsA units) $ \(uid, (r, _, _)) → case r of
                Just ref | not (T.any (≡ '[') ref), ref `notElem` docIds →
                    lookup uid (ucapRefs cA) `shouldBe` Just ref
                _ → pure ()
            -- the same arena under other unit ids and another order captures identically
            ucapEntries cB `shouldBe` ucapEntries cA

    it "every captured value survives YAML exactly — over random live states" $ \fx → do
        let env = fxEnv fx
        ws ← resetScene env
        [uid] ← constructClean fx ws "  - {id: r, definition: acolyte, x: 1, y: 1}\n"
        base ← unitOf env uid
        let parts = ["l_leg", "torso", "head", "r_arm"]
            kinds = ["slash", "stab", "fracture", "internal"]
            awkward = [0, 1 / 3, 0.1, 1.0e-7, 123.456, 2 / 7, 0.999999]
            awkward01 = [0, 1 / 3, 0.1, 1.0e-7, 2 / 7, 0.999999, 1]
        forM_ [1 .. 120 ∷ Int] $ \seed → do
            let g0 = mkStdGen seed
                pick xs g = let (i, g') = randomR (0, length xs - 1) g in (xs !! i, g')
                unit01 g = let (v, g') = randomR (0, 1 ∷ Float) g in (v, g')
                (a1, g1) = pick awkward g0
                (h, g2a) = unit01 g1
                (b1, g2) = pick awkward01 g2a
                (part, g3) = pick parts g2
                (kind, g4) = pick kinds g3
                (sev, g5) = randomR (0, 1.6 ∷ Float) g4
                (age, g6) = randomR (0, 5000 ∷ Int) g5
                (heal, g7) = randomR (-0.5, 1 ∷ Float) g6
                (blood, g8) = randomR (0.6, 5 ∷ Float) g7
                (rem', g9) = randomR (1, 900 ∷ Int) g8
                (pct, _) = randomR (-0.5, 0.5 ∷ Float) g9
                w = Wound { woundPart = part, woundKind = kind, woundSeverity = sev
                          , woundAt = startTime - fromIntegral age, woundBandage = h, woundClot = b1
                          , woundHeal = heal, woundDressing = "bandage", woundInfection = h / 3
                          , woundClean = even seed, woundInfectionType = if even seed then "staph" else ""
                          , woundNecrosis = 0 }
                u = base { uiStats = HM.insert "hydration" (a1 * 10) (HM.insert "strength_base" (h + 0.5) (uiStats base))
                         , uiWounds = [w], uiBlood = blood, uiImmuneResponse = h
                         , uiImmunities = HM.singleton "staph" b1
                         , uiModifiers = HM.insert "agility"
                             [StatModifier a1 "tonic" (Just (startTime + fromIntegral rem')) pct]
                             (uiModifiers base) }
            modifyIORef' (unitManagerRef env) $ \um → um { umInstances = HM.insert uid u (umInstances um) }
            c ← captureAll env
            (us', ds) ← decodeUnitsBytes env (captureDocument c)
            ds `shouldBe` []
            us' `shouldBe` ucapEntries c

-- ** Rejections

rejectionSpec ∷ SpecWith Fx
rejectionSpec = describe "diagnostics" $ do

    it "reports invalid fields, definitions, body parts, slots and references and keeps the unrelated units" $ \fx → do
        let env = fxEnv fx
        ws ← resetScene env
        (us, ds) ← decodeUnits env $ unlines
            [ "  - {id: ok, definition: acolyte, x: 1, y: 1}"
            , "  - {id: ghost, definition: no_such_unit, x: 1, y: 1}"
            , "  - {id: p, definition: acolyte, x: 1, y: 1, stats: {strength: 3, carrying_capacity: 9, wings: 2},"
            , "     wounds: [{part: tail, kind: slash, severity: 0.2}], equipment: {jetpack: {definition: bandage}}}"
            , "  - {id: e, definition: acolyte, x: 1, y: 1, encounter: nowhere}" ]
        map ueId us `shouldBe` [ExplicitId "ok", ExplicitId "p"]
        map (\d → (sdPath d, sdEffect d)) ds `shouldMatchList`
            [ ("units[1].definition", EntryRejected)
            , ("units[2].stats.strength", FieldRejected)
            , ("units[2].stats.carrying_capacity", FieldRejected)
            , ("units[2].stats.wings", FieldRejected)
            , ("units[2].wounds[0].part", EntryRejected)
            , ("units[2].equipment.jetpack", FieldRejected)
            , ("units[3].encounter", EntryRejected) ]
        (cs, ds2) ← construct fx ws us
        ds2 `shouldBe` []
        length [ () | UnitInstalled _ _ ← cs ] `shouldBe` 2

    it "rejects a partial or incoherent body as a group, never by order" $ \fx → do
        let env = fxEnv fx
        ws ← resetScene env
        us ← cleanUnits env $ unlines
            [ "  - {id: half, definition: acolyte, x: 1, y: 1, stats: {body_mass: 60, lean_mass: 20}}"
            , "  - {id: both, definition: acolyte, x: 1, y: 1, stats: {frame_mass: 70, body_mass: 60, lean_mass: 20,"
            , "     fat_mass: 8, bulk: 1.2}}"
            , "  - {id: over, definition: acolyte, x: 1, y: 1, stats: {hydration: 9999}}" ]
        (cs, ds) ← construct fx ws us
        length [ () | UnitInstalled _ _ ← cs ] `shouldBe` 3
        map (\d → (sdPath d, sdEffect d)) ds `shouldMatchList`
            [ ("units[0].stats.body_mass", FieldRejected)
            , ("units[0].stats.lean_mass", FieldRejected)
            , ("units[1].stats.bulk", FieldRejected)
            , ("units[2].stats.hydration", FieldRejected) ]
        um ← readIORef (unitManagerRef env)
        let byRef r = [ u | (i, u) ← HM.toList (umInstances um), Map.lookup i (umScenarioRefs um) ≡ Just r ]
        -- the partial body took the seed path; the coherent one is exact;
        -- the overfull pool was seeded to its maximum
        map (\u → stat u "body_mass" ≢ 60) (byRef "half") `shouldBe` [True]
        map (\u → stat u "lean_mass") (byRef "both") `shouldBe` [20]
        map (\u → approx (stat u "hydration") (stat u "max_hydration")) (byRef "over") `shouldBe` [True]

-- ** Lifecycle

lifecycleSpec ∷ SpecWith Fx
lifecycleSpec = describe "lifecycle" $ do

    it "a stale page admission leaves no unit, sim state, reference, tag or item behind" $ \fx → do
        let env = fxEnv fx
        ws ← resetScene env
        us ← cleanUnits env "  - {id: late, tags: [late-unit], definition: acolyte, x: 1, y: 1, inventory: [{id: c9, tags: [late-item], definition: bandage}]}\n"
        benv ← buildEnvOf env
        [entry0] ← pure us
        b ← buildUnit benv "units[0]" entry0
        epoch ← pageIncarnation ws
        -- the page is re-initialized under the same name
        ws2 ← emptyWorldState
        writeIORef (wsTilesRef ws2) =≪ readIORef (wsTilesRef ws)
        modifyIORef' (worldManagerRef env) (\wm → wm { wmWorlds = [(pageP, ws2)] })
        epoch2 ← pageIncarnation ws2
        epoch2 `shouldNotBe` epoch
        cs ← installUnitBuilds (installRefsOf fx) pageP epoch [b]
        cs `shouldBe` [UnitNotInstalled "late" PageNotAdmitting]
        um ← readIORef (unitManagerRef env)
        (HM.size (umInstances um), Map.size (umScenarioRefs um)) `shouldBe` (0, 0)
        uts ← readIORef (ucUtsRef (toUnitCombatCapability env))
        HM.size (utsSimStates uts) `shouldBe` 0
        wm ← readIORef (worldManagerRef env)
        (nullCategoryIndex (wmItemTags wm), nullItemRefs (wmItemScenarioRefs wm)) `shouldBe` (True, True)

    describe "every failed commit leaves nothing behind, and a retry succeeds" $ do
        let doc = "  - {id: carrier, tags: [c-unit], definition: acolyte, x: 1, y: 1, \
                  \inventory: [{id: kit-9, tags: [c-item], definition: first_aid_kit}]}\n"
            nothingLeft fx = do
                let env = fxEnv fx
                um ← readIORef (unitManagerRef env)
                (HM.size (umInstances um), Map.size (umScenarioRefs um), nullCategoryIndex (umTags um))
                    `shouldBe` (0, 0, True)
                uts ← readIORef (ucUtsRef (toUnitCombatCapability env))
                HM.size (utsSimStates uts) `shouldBe` 0
                wm ← readIORef (worldManagerRef env)
                (nullCategoryIndex (wmItemTags wm), nullItemRefs (wmItemScenarioRefs wm)) `shouldBe` (True, True)
            failingWith ready fx = do
                let env = fxEnv fx
                ws ← resetScene env
                us ← cleanUnits env doc
                benv ← buildEnvOf env
                [b] ← forM us (buildUnit benv "units[0]")
                epoch ← pageIncarnation ws
                cs ← installUnitBuilds (installRefsOf fx) { uirReady = ready } pageP epoch [b]
                nothingLeft fx
                -- the same entry, freshly built, installs cleanly afterwards
                (cs2, _) ← construct fx ws us
                [ () | UnitInstalled _ _ ← cs2 ] `shouldBe` [()]
                pure cs
        it "a failing readiness hook" $ \fx → do
            cs ← failingWith (\_ _ → pure (Left "injected failure")) fx
            cs `shouldBe` [UnitNotInstalled "carrier" (ReadinessFailed "injected failure")]
        it "an unknown readiness pose" $ \fx → do
            cs ← failingWith (\_ _ → pure (Right (ReadyPose (Just "flying")))) fx
            cs `shouldBe` [UnitNotInstalled "carrier" (ReadinessFailed "unknown readiness pose 'flying'")]
        it "a gameplay-lethal answer is a rejection" $ \fx → do
            cs ← failingWith (\_ _ → pure (Right (ReadyLethal "a ruptured heart"))) fx
            [ map sdEffect ds | UnitRejected ds ← cs ] `shouldBe` [[EntryRejected]]
        it "an item attachment the session refuses" $ \fx → do
            let env = fxEnv fx
            ws ← resetScene env
            -- another live item already holds the reference kit-9
            _ ← constructClean fx ws "  - {id: holder, definition: acolyte, x: 2, y: 2, \
                                     \inventory: [{id: kit-9, definition: bandage}]}\n"
            um0 ← readIORef (unitManagerRef env)
            us ← cleanUnits env doc
            (cs, _) ← construct fx ws us
            [ () | UnitNotInstalled "carrier" (ItemsRefused _) ← cs ] `shouldBe` [()]
            um ← readIORef (unitManagerRef env)
            HM.keys (umInstances um) `shouldBe` HM.keys (umInstances um0)
            Map.elems (umScenarioRefs um) `shouldBe` ["holder"]
            wm ← readIORef (worldManagerRef env)
            map snd (itemRefsToAscList (wmItemScenarioRefs wm)) `shouldBe` ["kit-9"]

    it "rejects a worsening vital wound the injury pass would kill at once (gameplay severity)" $ \fx → do
        let env = fxEnv fx
        ws ← resetScene env
        -- inflicted 0.8, but healing backwards: the gameplay view reads
        -- 0.8 × (1 − (−0.5)) = 1.2 on the heart
        us ← cleanUnits env "  - {id: dying, definition: acolyte, x: 1, y: 1, \
                            \wounds: [{part: heart, kind: stab, severity: 0.8, heal: -0.5}], \
                            \inventory: [{id: k2, tags: [t2], definition: bandage}]}\n"
        (cs, _) ← construct fx ws us
        [ [ (sdPath d, sdEffect d) | d ← ds ] | UnitRejected ds ← cs ] `shouldBe` [[("units[0]", EntryRejected)]]
        um ← readIORef (unitManagerRef env)
        HM.size (umInstances um) `shouldBe` 0
        wm ← readIORef (worldManagerRef env)
        (nullCategoryIndex (wmItemTags wm), nullItemRefs (wmItemScenarioRefs wm)) `shouldBe` (True, True)
        -- the same wound healing forwards is sub-lethal and installs
        us2 ← cleanUnits env "  - {id: mending, definition: acolyte, x: 1, y: 1, \
                             \wounds: [{part: heart, kind: stab, severity: 0.8, heal: 0.5}]}\n"
        (cs2, _) ← construct fx ws us2
        [ () | UnitInstalled _ _ ← cs2 ] `shouldBe` [()]

    it "a second unit claiming a held reference is not installed" $ \fx → do
        let env = fxEnv fx
        ws ← resetScene env
        us ← cleanUnits env "  - {id: twin, definition: acolyte, x: 1, y: 1}\n"
        _ ← construct fx ws us
        (cs, _) ← construct fx ws us
        cs `shouldBe` [UnitNotInstalled "twin" (ReferenceHeld "twin")]
        um ← readIORef (unitManagerRef env)
        HM.size (umInstances um) `shouldBe` 1

    it "a destroyed unit takes its reference and tags with it" $ \fx → do
        let env = fxEnv fx
        ws ← resetScene env
        [uid] ← constructClean fx ws "  - {id: doomed, tags: [t], definition: acolyte, x: 1, y: 1}\n"
        _ ← evalDebug (fxLua fx) ("unit.destroy(" <> tshow (unUnitId uid) <> "); return 'ok'")
        drainUnitQueue env
        um ← readIORef (unitManagerRef env)
        (HM.member uid (umInstances um), Map.member uid (umScenarioRefs um)) `shouldBe` (False, False)
        nullCategoryIndex (umTags um) `shouldBe` True

-- ** Persistence

persistenceSpec ∷ SpecWith Fx
persistenceSpec = describe "ordinary-save identity (D-52)" $ do

    it "a save → load through the real codec keeps each unit's reference, and a capture after it names the unit by it" $ \fx → do
        let env = fxEnv fx
        ws ← resetScene env
        [kept, auto] ← constructClean fx ws
            "  - {id: kept, definition: acolyte, x: 1, y: 1}\n  - {definition: acolyte, x: 2, y: 2}\n"
        um ← readIORef (unitManagerRef env)
        uts ← readIORef (ucUtsRef (toUnitCombatCapability env))
        let unitSnap = toUnitSnapshot pageP um
        usnScenarioRefs unitSnap `shouldBe` Map.fromList [(kept, "kept"), (auto, "units[1]")]
        usnScenarioRefs (toUnitSnapshot (WorldPageId "elsewhere") um) `shouldBe` Map.empty
        let page = (minimalPage pageP)
                { pgsUnits = unitSnap
                , pgsUnitSimStates = HM.filterWithKey (\k _ → HM.member k (usnInstances unitSnap))
                                                      (utsSimStates uts) }
        nextItem ← readIORef (nextItemInstanceIdRef env)
        snap ← either (fail ∘ show) pure $
            captureSessionSnapshot (globalsOn pageP [pageP] nextItem) [page]
        back ← either (fail ∘ show) pure $ fmap (\(_, s, _, _) → s) $
            decodeSessionEnvelope HS.empty HS.empty (encodeSessionSnapshot (saveMeta snap) snap [])
        backPage ← maybe (fail "page lost") pure (HM.lookup pageP (snapPages back))
        usnScenarioRefs (pgsUnits backPage) `shouldBe` usnScenarioRefs unitSnap
        -- restore onto a fresh manager, then capture: the same names
        items ← itemsOf env
        inf ← readIORef (crInfectionManagerRef (toContentRegistriesCapability env))
        ecm ← readReadOnlyRef (crvEquipmentClassManagerRef (toContentRegistriesViewCapability env))
        let (restored, _, _, _, _) = fromUnitSnapshot pageP (umDefs um) inf ecm items (pgsUnits backPage)
        umScenarioRefs restored `shouldBe` umScenarioRefs um
        before ← captureAll env
        writeIORef (unitManagerRef env) restored
        after ← captureAll env
        map ueId (ucapEntries after) `shouldBe` map ueId (ucapEntries before)
        map ueId (ucapEntries after) `shouldBe` [ExplicitId "kept", ExplicitId "units.1"]

    it "a restore keeps only the references of the units it produced" $ \fx → do
        let env = fxEnv fx
        ws ← resetScene env
        [uid] ← constructClean fx ws "  - {id: kept, definition: acolyte, x: 1, y: 1}\n"
        um ← readIORef (unitManagerRef env)
        items ← itemsOf env
        inf ← readIORef (crInfectionManagerRef (toContentRegistriesCapability env))
        ecm ← readReadOnlyRef (crvEquipmentClassManagerRef (toContentRegistriesViewCapability env))
        let snap = (toUnitSnapshot pageP um)
                { usnScenarioRefs = Map.fromList [(uid, "kept"), (UnitId 9999, "ghost")] }
            (restored, _, _, _, _) = fromUnitSnapshot pageP (umDefs um) inf ecm items snap
        umScenarioRefs restored `shouldBe` Map.singleton uid "kept"

    it "units v5 payloads decode with no references, and v6 round-trips them" $ \_ → do
        let v5 = S.encode (UnitsDTOv5 [PageUnitsDTOv5 pageP HM.empty []])
        fmap (\(UnitsDTO ss) → map puScenarioRefs ss) (ccDecode unitsCodec 5 v5) `shouldBe` Right [[]]
        let v6 = UnitsDTO [PageUnitsDTO pageP HM.empty [] [(UnitId 3, "scout"), (UnitId 7, "units[1]")]]
        ccDecode unitsCodec 6 (S.encode v6) `shouldBe` Right v6
        ccValidate unitsCodec v6 `shouldBe` []

    it "the units wire refuses malformed reference lists" $ \_ → do
        let errs refs = ccValidate unitsCodec (UnitsDTO [PageUnitsDTO pageP HM.empty [] refs])
        errs [(UnitId 1, "")] `shouldSatisfy` (not ∘ null)
        errs [(UnitId 1, T.replicate 513 "a")] `shouldSatisfy` (not ∘ null)
        errs [(UnitId 1, "a b")] `shouldSatisfy` (not ∘ null)
        errs [(UnitId 1, "a"), (UnitId 1, "b")] `shouldSatisfy` (not ∘ null)
        errs [(UnitId 1, "a"), (UnitId 2, "a")] `shouldSatisfy` (not ∘ null)
        errs [(UnitId 2, "a"), (UnitId 1, "b")] `shouldSatisfy` (not ∘ null)
        errs [(UnitId 1, "a"), (UnitId 2, "b")] `shouldBe` []

    it "integrity refuses a reference two pages' units hold and warns about one naming no unit" $ \_ → do
        let pA = WorldPageId "a"
            pB = WorldPageId "b"
            unitOn i = UnitSnapshot (HM.singleton (UnitId i) (minimalUnitInstance [])) 10 mempty mempty
            withRef i r p = (minimalPage p)
                { pgsUnits = (unitOn i) { usnScenarioRefs = Map.singleton (UnitId i) r }
                , pgsUnitSimStates = HM.singleton (UnitId i) minimalSimState }
            globals = globalsOn pA [pA, pB] 1
        dup ← either (fail ∘ show) pure (captureSessionSnapshot globals [withRef 1 "same" pA, withRef 2 "same" pB])
        [ ieCode e | e ← sessionIntegrityErrors dup ] `shouldSatisfy` elem "duplicate-scenario-reference"
        -- and the load assembly refuses such a save outright
        case decodeSessionEnvelope HS.empty HS.empty (encodeSessionSnapshot (saveMeta dup) dup []) of
            Left _ → pure ()
            Right _ → expectationFailure "a save whose reference two units hold loaded"
        let dangling = (minimalPage pA)
                { pgsUnits = (unitOn 1) { usnScenarioRefs = Map.singleton (UnitId 5) "ghost" }
                , pgsUnitSimStates = HM.singleton (UnitId 1) minimalSimState }
        snap ← either (fail ∘ show) pure (captureSessionSnapshot (globalsOn pA [pA] 1) [dangling])
        [ iePath e | e ← sessionIntegrityWarnings snap, ieCode e ≡ "dangling-reference" ]
            `shouldSatisfy` elem "units[page=a].scenario-refs[unit=5]"

-- * The readiness seam

-- | The pass modules a scenario unit's paused readiness runs
--   (@unit_resources.reconcileUnit@ with the mind trio off), and the one
--   delegate the seam calls for alerts. The mind trio (@brain@,
--   @thoughts@, @mental_state@) is not run by readiness, so it is out of
--   this check's scope.
readinessModules ∷ [FilePath]
readinessModules =
    [ "unit_resources", "cardio", "thermo", "salts", "physiology"
    , "unit_resource_upkeep", "unit_resource_energy", "unit_resource_tick"
    , "unit_resource_carry", "unit_resource_regrowth", "unit_resource_injury"
    , "unit_resource_failure", "starvation", "injuries", "unit_stats"
    , "unit_resource_params", "unit_resource_config", "lib/stat_base" ]

-- | Engine verbs those modules may call directly: reads, the two commit
--   previews, and diagnostics.
readOnlyVerbs ∷ [Text]
readOnlyVerbs =
    [ "unit.getStat", "unit.getWounds", "unit.getInfo", "unit.getPose"
    , "unit.getPhysioMode", "unit.getActivity", "unit.readPhysio", "unit.getMaxSpeed"
    , "unit.readPhysioBase", "unit.readBloodView", "unit.getScars", "unit.getPhysioDefault"
    , "unit.getModifiers", "unit.getMaxStamina", "unit.getInsulation", "unit.getCurrentAnim"
    , "unit.getCarryingWeight", "unit.getBlood", "unit.getAllStats", "unit.getAllIds"
    , "unit.previewStamina", "unit.previewPhysio", "equipment.getLoadout"
    , "world.getFluidAt", "world.getClimateAt", "world.getAmbientAt"
    , "engine.logWarn", "engine.logDebug", "engine.logError", "engine.logInfo"
    , "engine.getPhysiologyRegistry", "engine.gameTime" ]

-- | The mutating verbs: callable ONLY from @scripts/unit_effects.lua@.
mutatingVerbs ∷ [Text]
mutatingVerbs =
    [ "unit.setStat", "unit.recomputeBody", "unit.commitStamina", "unit.commitPhysio"
    , "unit.frostbite", "unit.recoverStance", "unit.dropEquipmentToGround"
    , "unit.applyCombatEngagement", "unit.setPhysioMode", "unit.clearPhysioMode"
    , "unit.kill", "unit.collapse", "unit.crawl", "unit.revive"
    , "engine.emitEventForUnit", "engine.emitEvent" ]

-- | Every engine-namespace call in a Lua source, comments removed.
engineCalls ∷ Text → [Text]
engineCalls src =
    [ ns <> "." <> verb
    | line ← T.lines src
    , let code = fst (T.breakOn "--" line)
    , (ns, rest) ← namespaced code
    , let verb = T.takeWhile (\c → c ≡ '_' ∨ c `elem` (['a' .. 'z'] ⧺ ['A' .. 'Z'] ⧺ ['0' .. '9'])) rest
    , not (T.null verb)
    , T.isPrefixOf "(" (T.drop (T.length verb) rest) ]
  where
    namespaced code = concat
        [ [ (ns, T.drop (T.length ns + 1) piece)
          | piece ← T.tails code
          , T.isPrefixOf (ns <> ".") piece
          , let before = T.dropEnd (T.length piece) code
          , T.null before ∨ not (isIdent (T.last before)) ]
        | ns ← ["unit", "engine", "equipment", "world", "item", "building"] ]
    isIdent c = c ≡ '_' ∨ c ≡ '.' ∨ c `elem` (['a' .. 'z'] ⧺ ['A' .. 'Z'] ⧺ ['0' .. '9'])

-- | The write barriers the pass modules install over the GLOBAL
--   @unit.setStat@ (regrowth's and carry's remainder forgets): they wrap the
--   verb rather than bypass it, and the seam's production @setStat@ calls the
--   global, so it goes through them exactly as before; a recorded write
--   reaches neither.
writeBarriers ∷ [(FilePath, Text)]
writeBarriers = [("unit_resource_regrowth", "local previous = unit.setStat")]

-- | A line that binds an engine namespace, or one of its verbs, to a name
--   (@local u = unit@, @local set = unit.setStat@) rather than calling it.
aliasing ∷ Text → Bool
aliasing l = case T.breakOn "=" l of
    (_, rhs) | not (T.null rhs) →
        let r = T.strip (T.drop 1 rhs)
        in not ("=" `T.isPrefixOf` T.drop 1 rhs)
           ∧ (r `elem` ["unit", "engine"]
              ∨ any (\ns → (ns <> ".") `T.isPrefixOf` r ∧ T.all isVerbChar (T.drop (T.length ns + 1) r))
                    ["unit", "engine"])
    _ → False
  where isVerbChar c = c ≡ '_' ∨ c `elem` (['a' .. 'z'] ⧺ ['A' .. 'Z'] ⧺ ['0' .. '9'])

seamStaticSpec ∷ Spec
seamStaticSpec = describe "the readiness seam (static)" $ do
    it "no pass module calls a mutating engine verb except through scripts/unit_effects.lua, \
       \and every verb they call is classified" $ do
        found ← forM readinessModules $ \m → do
            src ← TE.decodeUtf8 ⊚ BC.readFile ("scripts/" ⧺ m ⧺ ".lua")
            pure (m, engineCalls src)
        [ (m, v) | (m, vs) ← found, v ← vs, v `elem` mutatingVerbs ] `shouldBe` []
        [ (m, v) | (m, vs) ← found, v ← vs, v `notElem` readOnlyVerbs ] `shouldBe` []
        -- and no module captures a namespace or a verb in a local, which
        -- would escape the check
        aliases ← forM readinessModules $ \m → do
            src ← TE.decodeUtf8 ⊚ BC.readFile ("scripts/" ⧺ m ⧺ ".lua")
            pure [ (m, l) | l ← map (T.strip ∘ fst ∘ T.breakOn "--") (T.lines src)
                          , aliasing l, (m, l) `notElem` writeBarriers ]
        concat aliases `shouldBe` []
        -- the alert delegate is reached only through the seam
        direct ← forM readinessModules $ \m → do
            src ← TE.decodeUtf8 ⊚ BC.readFile ("scripts/" ⧺ m ⧺ ".lua")
            pure [ (m, l) | l ← map (fst ∘ T.breakOn "--") (T.lines src)
                          , any (`T.isInfixOf` l) ["alerts.emitDeathAlert(", "alerts.checkSurvivalAlerts("] ]
        concat direct `shouldBe` []
    it "the seam's production effects are exactly the verbs it stands for" $ do
        src ← TE.decodeUtf8 ⊚ BC.readFile "scripts/unit_effects.lua"
        let calls = engineCalls src
        forM_ [ v | v ← mutatingVerbs, v ≢ "engine.emitEvent" ] $ \v →
            (v, v `elem` calls) `shouldBe` (v, True)

-- * The fresh-process comparison

-- | The environment variable that turns a run of this binary into a
--   child: @<output file>|<variant>@.
childVar ∷ String
childVar = "SYNARCHY_SCENARIO_UNITS_CHILD"

-- | The document both children construct. Variant @perturbed@ edits
--   every tag, lists the units in another order, and advances the
--   gameplay stat RNG and both allocators before construction.
freshDoc ∷ Bool → String
freshDoc perturbed = unlines (if perturbed then reverse rows else rows)
  where
    t a b = if perturbed then b else a
    rows =
        [ "  - {id: alpha, tags: [" ⧺ t "red" "blue" ⧺ "], definition: acolyte, x: 1, y: 1, inventory: [{id: k1, tags: [" ⧺ t "a" "b" ⧺ "], definition: first_aid_kit}]}"
        , "  - {id: beta, definition: acolyte, x: 2, y: 2, stats: {strength_base: 1.3}}"
        , "  - {id: gamma, definition: " ⧺ T.unpack lazyName ⧺ ", x: 3, y: 3}" ]

-- | One child run: construct 'freshDoc' in this fresh process and write
--   the canonical capture (tags removed: the perturbed child edits
--   them) plus whether the gameplay streams moved.
childRun ∷ FilePath → Bool → Fx → IO ()
childRun out perturbed fx = do
    let env = fxEnv fx
    ws ← resetScene env
    when perturbed $ do
        replicateM_ 211 (atomicModifyIORef' (statRNGRef env) (\g → let (_, g') = randomR (0 ∷ Int, 9) g in (g', ())))
        replicateM_ 57 (void (freshItemInstanceId env))
        modifyIORef' (unitManagerRef env) (\um → um { umNextId = umNextId um + 23 })
    before ← statRNG env
    draws ← luaDraws fx
    _ ← constructClean fx ws (freshDoc perturbed)
    c ← captureAll env
    after ← statRNG env
    draws' ← luaDraws fx
    um ← readIORef (unitManagerRef env)
    let untag e = e { ueTags = [], ueInventory = fmap (map untagItem) (ueInventory e)
                    , ueEquipment = fmap (HM.map untagItem) (ueEquipment e)
                    , ueAccessories = fmap (map untagItem) (ueAccessories e) }
        untagItem i = i { ieTags = [], ieContents = fmap (map untagItem) (ieContents i) }
        canonical = A.object
            [ ("units", A.toJSON (map (unitEntryValue ∘ untag) (ucapEntries c)))
            , ("gameplay_rng_unchanged", A.toJSON (before ≡ after))
            , ("lua_draws_unchanged", A.toJSON (draws ≡ draws'))
            , ("unit_ids", A.toJSON (sort (map unUnitId (HM.keys (umInstances um))))) ]
    BL.writeFile out (A.encode canonical)

freshProcessSpec ∷ Spec
freshProcessSpec = describe "fresh process" $ do
    it "child entry (a no-op outside a child run)" $ do
        mChild ← lookupEnv childVar
        case fmap (break (≡ '|')) mChild of
            Just (out, '|' : variant) → setup (childRun out (variant ≡ "perturbed"))
            _ → pure ()
    it "two fresh processes construct the same scenario-visible values" $ do
        mChild ← lookupEnv childVar
        when (isNothing mChild) $ do
            exe ← getExecutablePath
            tmp ← getTemporaryDirectory
            let dir = tmp </> "synarchy-scenario-units-fresh"
            createDirectoryIfMissing True dir
            outs ← forM ["plain", "perturbed"] $ \variant → do
                let out = dir </> (variant ⧺ ".json")
                parentEnv ← getEnvironment
                (code, so, se) ← readCreateProcessWithExitCode
                    (proc exe ["--match", "Scenario.Units/fresh process/child entry"])
                        { env = Just ((childVar, out ⧺ "|" ⧺ variant)
                                      : filter ((≢ childVar) ∘ fst) parentEnv) } ""
                when (code ≢ ExitSuccess) $
                    expectationFailure ("child " ⧺ variant ⧺ " failed: " ⧺ so ⧺ se)
                present ← doesFileExist out
                present `shouldBe` True
                BL.readFile out
            case map A.decode outs ∷ [Maybe A.Value] of
                [Just (A.Object a), Just (A.Object b)] → do
                    let field k o = lookupKey k o
                    field "units" a `shouldBe` field "units" b
                    field "gameplay_rng_unchanged" a `shouldBe` Just (A.Bool True)
                    field "gameplay_rng_unchanged" b `shouldBe` Just (A.Bool True)
                    field "lua_draws_unchanged" b `shouldBe` Just (A.Bool True)
                    -- the runtime ids really differ
                    field "unit_ids" a `shouldNotBe` field "unit_ids" b
                other → expectationFailure ("unreadable child output: " ⧺ show other ⧺ " in " ⧺ dir)
  where
    lookupKey k o = Y.parseMaybe (A..: k) o ∷ Maybe A.Value

-- * Helpers

rollTemplatesFor' ∷ Text → HM.HashMap Text (Float, Float) → HM.HashMap Text Float
rollTemplatesFor' ref = rollTemplatesFor ref "stat"

shuffleWith ∷ StdGen → [α] → ([α], StdGen)
shuffleWith g0 xs0 = go g0 xs0 []
  where
    go g [] acc = (acc, g)
    go g xs acc = let (i, g') = randomR (0, length xs - 1) g
                      (a, b) = splitAt i xs
                  in case b of
                      (y : ys) → go g' (a ⧺ ys) (y : acc)
                      [] → (acc, g')

subsetOf ∷ [α] → StdGen → ([α], StdGen)
subsetOf xs g0 = foldl' (\(acc, g) x → let (keep, g') = randomR (False, True) g
                                       in (if keep then acc ⧺ [x] else acc, g')) ([], g0) xs

zip4' ∷ [a] → [b] → [c] → [d] → [(a, b, c, d)]
zip4' (a : as) (b : bs) (c : cs) (d : ds) = (a, b, c, d) : zip4' as bs cs ds
zip4' _ _ _ _ = []

nub' ∷ Ord α ⇒ [α] → [α]
nub' = Set.toList ∘ Set.fromList

flatEntry ∷ ItemEntry → [ItemEntry]
flatEntry e = e : concatMap flatEntry (authoredOr [] (ieContents e))

-- | Session globals for pages of this file's own, with the item
--   allocator above every item a test minted.
globalsOn ∷ WorldPageId → [WorldPageId] → Word64 → SessionGlobals
globalsOn active visible nextItem = minimalGlobals
    { sgActivePage = active, sgVisiblePages = visible
    , sgNextItemId = max 1 nextItem, sgNextUnitId = 100000
    , sgLiveCamera = (sgLiveCamera minimalGlobals) { lcsOwnerPage = Just active } }

saveMeta ∷ SessionSnapshot → SaveMetadata
saveMeta = snapshotSaveMetadata
    (SaveRequestMeta { srmSlotName = "slot", srmTimestamp = "ts", srmAutosave = False })

decodeUnitsBytes ∷ EngineEnv → BC.ByteString → IO ([UnitEntry], [ScenarioDiagnostic])
decodeUnitsBytes env bytes = do
    cat ← catalogOf env
    case decodeScenarioBytes cat bytes of
        ScenarioLoaded s ds → pure (scUnits s, ds)
        ScenarioFailed f → fail ("scenario failed: " ⧺ show f ⧺ "\n" ⧺ BC.unpack bytes)

poseText ∷ Pose → Text
poseText = poseTag

-- | The ordinary path for a unit in @a@'s state: an acolyte spawned
--   through the real spawn handler, given @a@'s stats, wounds and blood,
--   then one real wound tick and one real resource pass (engine running),
--   with every queued pose command applied. Answers the twin's id and
--   final instance.
ordinaryTwin ∷ Fx → WorldState → UnitInstance → IO (UnitId, UnitInstance)
ordinaryTwin fx ws a = do
    let env = fxEnv fx
    -- a fresh id from the allocator, as unit.spawn takes one: ids are
    -- never reused within a session, so no per-unit Lua cache of an
    -- earlier unit can be inherited
    twin ← atomicModifyIORef' (unitManagerRef env) $ \m → let (i, m') = nextUnitId m in (m', i)
    epoch ← pageIncarnation ws
    acolyteTags ← udFactionTags ⊚ defNamed env "acolyte"
    handleUnitSpawnCommand env (ucUtsRef (toUnitCombatCapability env)) twin "acolyte"
        (uiGridX a + 2) (uiGridY a) terrainZ (resolveSpawnFaction acolyteTags Nothing) pageP epoch
        SpawnStandard
    modifyIORef' (unitManagerRef env) $ \m → m
        { umInstances = HM.adjust (\b → b { uiStats = uiStats a, uiWounds = uiWounds a
                                           , uiBlood = uiBlood a, uiModifiers = uiModifiers a })
                                  twin (umInstances m) }
    writeIORef (enginePausedRef env) False
    tickAllWounds env 0.1
    drainUnitQueue env
    r ← evalDebug (fxLua fx) "__res.update(0.1); return 'ok'"
    drainUnitQueue env
    writeIORef (enginePausedRef env) True
    r `shouldBe` "ok"
    b ← unitOf env twin
    pure (twin, b)

-- | The scenario-visible state the comparison holds two units to: pose,
--   stats, blood and wounds.
canonicalOf ∷ EngineEnv → UnitId → IO (Text, [(Text, Float)], Float, [Wound])
canonicalOf env uid = do
    u ← unitOf env uid
    pure (uiPose u, sort (HM.toList (uiStats u)), uiBlood u, uiWounds u)

-- | The ordinary path at zero elapsed time: an acolyte spawned through the
--   real spawn handler, given @b@'s native state (stats, wounds, blood,
--   modifiers, skills and knowledge; not its items, whose ids are the
--   scenario unit's), then the wound tick and the per-unit reconciliation
--   (@unit_resources.reconcileUnit@, production effects) at dt 0, with
--   every queued command applied and the pose mirror published after each.
--   'Nothing' when that kills it.
ordinaryZeroTime ∷ Fx → WorldState → UnitInstance
                 → IO (Maybe (Text, [(Text, Float)], Float, [Wound]))
ordinaryZeroTime fx ws b = do
    let env = fxEnv fx
    -- a fresh id from the allocator, as unit.spawn takes one: ids are
    -- never reused within a session, so no per-unit Lua cache of an
    -- earlier unit can be inherited
    twin ← atomicModifyIORef' (unitManagerRef env) $ \m → let (i, m') = nextUnitId m in (m', i)
    epoch ← pageIncarnation ws
    acolyteTags ← udFactionTags ⊚ defNamed env "acolyte"
    handleUnitSpawnCommand env (ucUtsRef (toUnitCombatCapability env)) twin "acolyte"
        (uiGridX b + 3) (uiGridY b) terrainZ (resolveSpawnFaction acolyteTags Nothing) pageP epoch
        SpawnStandard
    modifyIORef' (unitManagerRef env) $ \m → m
        { umInstances = HM.adjust (\t → t { uiStats = uiStats b, uiWounds = uiWounds b
                                           , uiBlood = uiBlood b, uiModifiers = uiModifiers b
                                           , uiSkills = uiSkills b, uiKnowledge = uiKnowledge b
                                           , uiScars = uiScars b, uiImmuneResponse = uiImmuneResponse b
                                           , uiImmunities = uiImmunities b })
                                  twin (umInstances m) }
    tickAllWounds env 0
    killed1 ← applyQueued env twin
    r ← evalDebug (fxLua fx) $ "local id = " <> tshow (unUnitId twin)
        <> "; __res.reconcileUnit(id, unit.getInfo(id), 0, { mind = false }); return 'ok'"
    r `shouldBe` "ok"
    killed2 ← applyQueued env twin
    -- A queued kill IS the death: the pass can queue a later resource's
    -- collapse behind an earlier one's kill (pools iterate in table
    -- order), and the collapse handler would repose the corpse.
    out ← if killed1 ∨ killed2 then pure Nothing else Just ⊚ canonicalOf env twin
    _ ← evalDebug (fxLua fx) ("unit.destroy(" <> tshow (unUnitId twin) <> "); return 'ok'")
    drainUnitQueue env
    pure out
  where
    -- the unit thread applies queued commands, then publishes the pose;
    -- answers whether a kill for @who@ was among them
    applyQueued env who = do
        cmds ← takeAll env []
        forM_ cmds (Q.writeQueue (unitQueue env))
        drainUnitQueue env
        uts ← readIORef (ucUtsRef (toUnitCombatCapability env))
        modifyIORef' (unitManagerRef env) $ \m → m
            { umInstances = HM.mapWithKey (\i u → maybe u (\ss → u { uiPose = poseTag (usPose ss) })
                                                       (HM.lookup i (utsSimStates uts)))
                                          (umInstances m) }
        pure (any (\case { UnitKill u → u ≡ who; _ → False }) cmds)
    takeAll env acc = Q.tryReadQueue (unitQueue env) ≫= \case
        Nothing → pure (reverse acc)
        Just c → takeAll env (c : acc)

-- | The authored state a pure readiness must leave alone: every authorable
--   stat (as bit patterns, absent ones included), and the health, skills,
--   knowledge, modifiers and loadout. The pose is the one thing it decides.
authoredState ∷ [Text] → UnitInstance
              → ([(Text, Maybe Word32)], [Wound], [Scar], Word32, Word32, [(Text, Word32)]
                , [(Text, Word32)], [(Text, Word32)], Text, [ItemInstance], [(Text, ItemInstance)]
                , [ItemInstance])
authoredState keys u =
    ( [ (k, castFloatToWord32 ⊚ HM.lookup k (uiStats u)) | k ← keys ]
    , uiWounds u, uiScars u, castFloatToWord32 (uiBlood u), castFloatToWord32 (uiImmuneResponse u)
    , bits (uiImmunities u), bits (uiSkills u), bits (uiKnowledge u)
    , tshow (sortOn fst (HM.toList (uiModifiers u)))
    , uiInventory u, sortOn fst (HM.toList (uiEquipment u)), uiAccessories u )
  where bits m = sortOn fst [ (k, castFloatToWord32 v) | (k, v) ← HM.toList m ]

strengthRatio ∷ UnitInstance → Maybe Float
strengthRatio u = strengthRatioOf (sort (HM.toList (uiStats u)))

strengthRatioOf ∷ [(Text, Float)] → Maybe Float
strengthRatioOf st = do
    s ← lookup "strength" st
    b ← lookup "strength_body" st
    pure (fromIntegral (round (s / b * 1.0e4 ∷ Float) ∷ Int) / 1.0e4)

-- | A rejected entry leaves nothing: no unit, reference or tag, no item tag
--   or reference, and nothing on the page's ground.
noTrace ∷ EngineEnv → Int → WorldState → IO ()
noTrace env seed ws = do
    um ← readIORef (unitManagerRef env)
    (seed, HM.size (umInstances um), Map.size (umScenarioRefs um), nullCategoryIndex (umTags um))
        `shouldBe` (seed, 0, 0, True)
    wm ← readIORef (worldManagerRef env)
    (seed, nullCategoryIndex (wmItemTags wm), nullItemRefs (wmItemScenarioRefs wm))
        `shouldBe` (seed, True, True)
    ground ← readIORef (wsGroundItemsRef ws)
    (seed, HM.size (gisItems ground)) `shouldBe` (seed, 0)

lazyStatNames ∷ [Text]
lazyStatNames = ["strength", "agility", "endurance", "carrying_capacity"]

templateStats ∷ [Text] → UnitInstance → HM.HashMap Text Float
templateStats ks u = HM.filterWithKey (\k _ → k `elem` ks) (uiStats u)

intercalate' ∷ String → [String] → String
intercalate' _ [] = ""
intercalate' _ [x] = x
intercalate' sep (x : xs) = x ⧺ sep ⧺ intercalate' sep xs

-- | Run every queued unit command on this thread, as the unit thread
--   would.
drainUnitQueue ∷ EngineEnv → IO ()
drainUnitQueue env = void (processAllUnitCommands env (ucUtsRef (toUnitCombatCapability env)))
