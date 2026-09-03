-- | Production flora-content coverage. Unlike the pure growth fixtures, this
--   loads the shipped YAML so an approved texture family cannot remain
--   unregistered without a focused test noticing.
--
--   Since #2241 it also owns the ATOMICITY of flora's duplicate-name
--   refusal, which needs the real @engine.loadFloraYaml@ binding rather
--   than the pure decoder: what has to be shown is that a refused file
--   leaves no trace in any of the four things registration touches — the
--   catalog, the id allocator, the texture-name registry and the asset
--   load queue — and three of those four are engine state the decoder
--   never reaches.
module Test.Headless.Asset.FloraContent (spec) where

import UPrelude
import Test.Hspec
import qualified Data.HashMap.Strict as HM
import Data.List (nub, sort)
import qualified Data.Text as T
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as VU
import Data.IORef (newIORef, readIORef)
import System.Directory
    ( createDirectoryIfMissing, doesFileExist, getTemporaryDirectory
    , listDirectory, removeDirectoryRecursive )
import Control.Exception (finally)
import Engine.Asset.Handle (TextureHandle(..))
import Engine.Asset.TextureNameRegistry (lookupTextureName)
import Engine.Core.Capability.RenderView
    (RenderViewCapability(..), toRenderViewCapability)
import Engine.Core.Init (EngineInitResult(..))
import Engine.Core.Queue (QueueStats(..), queueStats)
import Engine.Core.State
    ( EngineEnv, floraCatalogRef, loggerRef, luaToEngineQueue, luaQueue
    , assetPoolRef, nextObjectIdRef, inputStateRef )
import Engine.Core.Thread (ThreadControl(..))
import Engine.Scripting.Lua.API (registerLuaAPI)
import Engine.Scripting.Lua.Thread (createLuaBackendState)
import Engine.Scripting.Lua.Thread.Console (executeDebugLua)
import Engine.Scripting.Lua.Types (LuaBackendState(..))
import Test.Headless.Harness.Isolation
    (isInsideIsolatedResourceRoot, withIsolatedResourceRoot)
import Test.Headless.Harness.Log (initializeEngineHeadlessQuiet)
import Engine.Asset.YamlFlora
import Engine.Asset.YamlItems
    ( ItemYamlDef(..), ItemYamlFood(..), ItemYamlWeight(..), loadItemYaml )
import Engine.Asset.YamlMaterials (loadPopulatedMaterialRegistry)
import Engine.Core.Log
    (LoggerState, LogBackend(..), LogConfig(..), defaultLogConfig, initLogger)
import World.Chunk.Types (ChunkCoord(..), chunkSize)
import World.Flora.Growth (instanceLifespan)
import World.Flora.Placement (computeChunkFlora, speciesFitnessDetail)
import World.Flora.Render (resolveFloraTexture)
import World.Flora.Identity (floraInstanceIdNone)
import World.Flora.Types
    ( AnnualCycleKey(..), AnnualStage(..), FloraCatalog(..)
    , FloraChunkData(..)
    , FloraId(..), FloraInstance(..), FloraSpecies(..), FloraWorldGen(..)
    , LifePhase(..), LifecycleType(..), emptyFloraCatalog, findSpeciesByName
    , insertSpecies, insertWorldGen, newFloraSpecies )
import World.Fluid.Types (FluidCell(..), FluidType(..))
import World.Material
    (MaterialId(..), MaterialRegistry, materialIdByName)
import World.Weather.Types
    ( ClimateCoord(..), ClimateGrid(..), ClimateState(..), RegionClimate(..)
    , SeasonalClimate(..), climateRegionCount, defaultRegionClimate
    , initClimateState )

spec ∷ Spec
spec = do
    describe "saguaro flora content" $ do
        it "registers the approved texture family as decorative desert flora" $ do
            logger ← silentLogger
            defs ← loadFloraYaml logger "data/flora/saguaro.yaml"
            registry ← loadPopulatedMaterialRegistry logger "data/materials"
            case defs of
                [def] → assertSaguaro registry def
                _ → expectationFailure $
                    "expected exactly one saguaro definition, got "
                        ⧺ show (length defs)

    describe "tomato crop content" $ do
        it "ships dedicated art without changing the crop or item contract" $ do
            logger ← silentLogger
            floraDefs ← loadFloraYaml logger "data/flora/crops.yaml"
            itemDefs ← loadItemYaml logger "data/items/tomato.yaml"
            case ( filter ((≡ "tomato_plant") ∘ fydName) floraDefs
                 , filter ((≡ "tomato") ∘ iydName) itemDefs ) of
                ([floraDef], [itemDef]) → do
                    assertTomatoFlora floraDef
                    assertTomatoItem itemDef
                    assertTomatoTextures floraDef
                (floraMatches, itemMatches) → expectationFailure $
                    "expected one tomato crop and item definition, got "
                        ⧺ show (length floraMatches, length itemMatches)

    describe "cattail flora content" $ do
        it "loads the exact decorative wetland contract and all textures" $
            withCattail assertCattail

        it "places only on the exposed version of an otherwise identical tile" $
            withCattail assertCattailPlacement

        it "resolves adult, juvenile, seasonal, and dead textures exactly" $
            withCattail (\_ def → assertCattailTextures def)

    describe "duplicate authored names (#2241 requirement 4)" $ do

        it "boots inside the scratch resource root, never the checkout \
           \(#1357)" $ withFloraEngine $ \_ →
            isInsideIsolatedResourceRoot `shouldReturn` True

        it "refuses a whole file whose second definition collides, \
           \leaving no catalog insert, no fcNextId advance, no texture \
           \registration and no queued load" $ withFloraEngine $ \eng → do
            -- The colliding definition is SECOND and the unique one
            -- FIRST, which is the shape the atomicity claim is about:
            -- registerFloraSpecies allocates an id and queues textures
            -- well before its catalog insert, so a refusal decided when
            -- the collision is REACHED would already have registered the
            -- definition ahead of it.
            _ ← loadFlora eng "data/flora/saguaro.yaml"
            before ← snapshotFlora eng
            withFloraFixture "collide" mixedDuplicateYaml $ \path → do
                (count, parsed, refusal) ← loadFloraOutcome eng path
                (count, parsed) `shouldBe` ("0", "true")
                refusal `shouldBe` "saguaro"
                after ← snapshotFlora eng
                after `shouldBe` before
                -- and specifically: the unique definition that sat ahead
                -- of the collision registered nothing at all.
                cat ← readIORef (floraCatalogRef (feEnv eng))
                findSpeciesByName "probe_2241_unique" cat `shouldBe` Nothing
                nameRegistered eng "flora_base_probe_2241_unique"
                    `shouldReturn` False

        it "refuses a file that duplicates a name WITHIN itself, with \
           \nothing already in the catalog to collide with" $
            withFloraEngine $ \eng → do
            before ← snapshotFlora eng
            withFloraFixture "selfdup" selfDuplicateYaml $ \path → do
                (count, parsed, refusal) ← loadFloraOutcome eng path
                (count, parsed) `shouldBe` ("0", "true")
                refusal `shouldBe` "probe_2241_twice"
                snapshotFlora eng `shouldReturn` before

        it "accepts the same file once its collision is gone, so the \
           \refusal is about the duplicate and not about the fixture" $
            withFloraEngine $ \eng → do
            withFloraFixture "unique" uniqueFloraYaml $ \path → do
                (count, parsed, refusal) ← loadFloraOutcome eng path
                parsed `shouldBe` "true"
                refusal `shouldBe` "nil"
                count `shouldNotBe` "0"
                cat ← readIORef (floraCatalogRef (feEnv eng))
                (fsName ∘ snd <$> findSpeciesByName "probe_2241_unique" cat)
                    `shouldBe` Just "probe_2241_unique"

        it "flora.register stays NONFATAL on a collision: nil, a warning, \
           \and no mutation" $ withFloraEngine $ \eng → do
            first ← evalLua eng
                "return tostring(flora.register('probe_2241_runtime', 0))"
            first `shouldNotBe` "nil"
            before ← snapshotFlora eng
            again ← evalLua eng
                "return tostring(flora.register('probe_2241_runtime', 0))"
            again `shouldBe` "nil"
            snapshotFlora eng `shouldReturn` before

-- * The private flora engine

-- | A throwaway headless engine with the real Lua API registered.
--
--   PRIVATE per example: @engine.loadFloraYaml@ mutates shared engine
--   state (catalog, asset pool, texture-name registry) in ways no
--   @finally@ can undo — the same reasoning
--   "Test.Headless.Asset.TextureFallback" records.
--
--   ISOLATED because the boot itself writes @config\/@ (#1357): engine
--   initialization migrates legacy config and materializes an absent
--   @config\/notifications.local.yaml@. The wrap goes AROUND the boot,
--   never inside it, and the scratch root symlinks @data\/@, so the
--   shipped YAML these examples load resolves unchanged.
data FloraEngine = FloraEngine
    { feEnv ∷ EngineEnv
    , feLua ∷ LuaBackendState
    }

withFloraEngine ∷ (FloraEngine → Expectation) → Expectation
withFloraEngine action = withIsolatedResourceRoot $ do
    EngineInitResult env ← initializeEngineHeadlessQuiet
    ls ← createLuaBackendState (luaToEngineQueue env) (luaQueue env)
                               (assetPoolRef env) (nextObjectIdRef env)
                               (inputStateRef env) (loggerRef env)
    stateRef ← newIORef ThreadRunning
    registerLuaAPI (lbsLuaState ls) env ls stateRef
    action (FloraEngine env ls)

evalLua ∷ FloraEngine → Text → IO Text
evalLua eng src =
    T.strip ∘ T.filter (≢ '"') <$> executeDebugLua (lbsLuaState (feLua eng)) src

loadFlora ∷ FloraEngine → Text → IO Text
loadFlora eng path =
    evalLua eng ("return string.format('%d', engine.loadFloraYaml('"
                 <> path <> "'))")

-- | The three values the binding answers with when the caller opts in:
--   count, parse outcome, and #2241's refusal detail (@nil@ when the
--   file was not refused).
loadFloraOutcome ∷ FloraEngine → Text → IO (Text, Text, Text)
loadFloraOutcome eng path = do
    out ← evalLua eng
        ("local n, parsed, refusal = engine.loadFloraYaml('" <> path
         <> "', true); return string.format('%d|%s|%s', n, \
            \tostring(parsed), tostring(refusal))")
    case T.splitOn "|" out of
        [n, parsed, refusal] → pure (n, parsed, refusal)
        _                    → pure (out, out, out)

-- | Everything a flora registration touches, captured together: a
--   refusal must move none of it.
data FloraSnapshot = FloraSnapshot
    { fsnNextId   ∷ Word16
    , fsnSpecies  ∷ [Text]
    , fsnWorldGen ∷ Int
    , fsnEnqueued ∷ Word64  -- ^ cumulative asset-queue writes
    } deriving (Show, Eq)

snapshotFlora ∷ FloraEngine → IO FloraSnapshot
snapshotFlora eng = do
    cat ← readIORef (floraCatalogRef (feEnv eng))
    stats ← queueStats (fst (lbsMsgQueues (feLua eng)))
    pure FloraSnapshot
        { fsnNextId   = fcNextId cat
        , fsnSpecies  = sort [ fsName sp | sp ← HM.elems (fcSpecies cat) ]
        , fsnWorldGen = HM.size (fcWorldGen cat)
        , fsnEnqueued = qsEnqueued stats
        }

nameRegistered ∷ FloraEngine → Text → IO Bool
nameRegistered eng name = do
    reg ← readIORef (rvTextureNameRegistryRef
                        (toRenderViewCapability (feEnv eng)))
    pure (isJust (lookupTextureName name reg))

withFloraFixture ∷ String → String → (Text → Expectation) → Expectation
withFloraFixture label body action = do
    tmp ← getTemporaryDirectory
    let dir = tmp ⊘ ("synarchy-2241-" ⧺ label)
        path = dir ⊘ "probe.yaml"
    createDirectoryIfMissing True dir
    writeFile path body
    action (T.pack path) `finally` removeDirectoryRecursive dir

-- | One unique definition FIRST, then one whose name is already in the
--   catalog.
mixedDuplicateYaml ∷ String
mixedDuplicateYaml = floraFile ["probe_2241_unique", "saguaro"]

-- | Two definitions in one file sharing a name nothing else has.
selfDuplicateYaml ∷ String
selfDuplicateYaml = floraFile ["probe_2241_twice", "probe_2241_twice"]

-- | 'mixedDuplicateYaml' with the collision removed.
uniqueFloraYaml ∷ String
uniqueFloraYaml = floraFile ["probe_2241_unique"]

floraFile ∷ [String] → String
floraFile names = unlines ("flora:" : concatMap floraEntry names)

floraEntry ∷ String → [String]
floraEntry name =
    [ "  - name: " ⧺ name
    , "    type: groundcover"
    , "    texDir: \"assets/textures/flora\""
    , "    worldGen:"
    , "      category: groundcover"
    , "      minTemp: 0"
    , "      maxTemp: 40"
    , "      idealTemp: 20"
    , "      minPrecip: 0"
    , "      maxPrecip: 1"
    , "      idealPrecip: 0.5"
    ]

withCattail
    ∷ (MaterialRegistry → FloraYamlDef → Expectation)
    → Expectation
withCattail action = do
    logger ← silentLogger
    defs ← loadFloraYaml logger "data/flora/wetlands.yaml"
    registry ← loadPopulatedMaterialRegistry logger "data/materials"
    case defs of
        [def] → action registry def
        _ → expectationFailure $
            "expected exactly one common cattail definition, got "
            ⧺ show (length defs)

silentLogger ∷ IO LoggerState
silentLogger = initLogger defaultLogConfig
    { lcBackend = LogToCallback (\_ → pure ()) }

assertSaguaro ∷ MaterialRegistry → FloraYamlDef → Expectation
assertSaguaro registry def = do
    fydName def `shouldBe` "saguaro"
    fydType def `shouldBe` "cactus"
    fydTexDir def `shouldBe` "assets/textures/flora/saguaro"
    fydLifecycle def `shouldBe` "perennial"
    fydMinLife def `shouldBe` Just 18000
    fydMaxLife def `shouldBe` Just 54000
    fydDeathChance def `shouldBe` Just 0.01
    fydHarvest def `shouldBe` Nothing

    map (\p → (fypTag p, fypTexture p, fypAge p)) (fydPhases def)
        `shouldBe`
            [ ("sprout", "sprout.png", 0)
            , ("matured", "matured.png", 1800)
            , ("dead", "dead.png", 54000)
            ]

    map (\c → (fycsTag c, fycsStartDay c, fycsTexture c))
            (fydAnnualCycle def)
        `shouldBe`
            [ ("dormant", 0, "matured.png")
            , ("budding", 90, "matured.png")
            , ("flowering", 120, "matured_flowering.png")
            , ("fruiting", 150, "matured_fruiting.png")
            , ("senescing", 240, "matured.png")
            ]

    map (\o → (fycoPhase o, fycoCycle o, fycoTexture o))
            (fydCycleOverrides def)
        `shouldBe`
            [ (phase, cycle, texture)
            | phase ← ["sprout", "dead"]
            , cycle ← ["dormant", "budding", "flowering", "fruiting", "senescing"]
            , let texture = if phase ≡ "sprout" then "sprout.png" else "dead.png"
            ]

    let wg = fydWorldGen def
    fywCategory wg `shouldBe` "cactus"
    (fywMinTemp wg, fywIdealTemp wg, fywMaxTemp wg)
        `shouldBe` (18, 30, 45)
    (fywMinPrecip wg, fywIdealPrecip wg, fywMaxPrecip wg)
        `shouldBe` (0.02, 0.22, 0.30)
    (fywMinAlt wg, fywIdealAlt wg, fywMaxAlt wg)
        `shouldBe` (Just (-20), Just 120, Just 500)
    (fywMinHumidity wg, fywIdealHumidity wg, fywMaxHumidity wg)
        `shouldBe` (Just 0.02, Just 0.18, Just 0.55)
    fywMaxSlope wg `shouldBe` Just 3
    fywDensity wg `shouldBe` Just 0.08
    fywFootprint wg `shouldBe` Just 14
    fywSoils wg `shouldBe`
        ["sandy_loam", "sandy_clay_loam", "loamy_sand"]

    let resolvedSoils = map (materialIdByName registry) (fywSoils wg)
    resolvedSoils `shouldSatisfy` all isJust
    case [unMaterialId mid | Just mid ← resolvedSoils] of
        [sandyLoam, sandyClayLoam, loamySand] → do
            let runtimeWG = toRuntimeWorldGen wg
                    [sandyLoam, sandyClayLoam, loamySand]
                score soil precip = fst $ speciesFitnessDetail runtimeWG
                    soil 0 30 precip 0.18 120
                primaryScore = score sandyLoam 0.22
                clayScore = score sandyClayLoam 0.27
                sandScore = score loamySand 0.18
            clayScore `shouldSatisfy` (> 0)
            sandScore `shouldSatisfy` (> 0)
            primaryScore `shouldSatisfy` (> clayScore)
            primaryScore `shouldSatisfy` (> sandScore)
        _ → expectationFailure "expected all three saguaro soils to resolve"

toRuntimeWorldGen ∷ FloraYamlWorldGen → [Word8] → FloraWorldGen
toRuntimeWorldGen wg soilIds =
    let minAlt = fromMaybe (-100) (fywMinAlt wg)
        maxAlt = fromMaybe 800 (fywMaxAlt wg)
        minHumidity = fromMaybe 0 (fywMinHumidity wg)
        maxHumidity = fromMaybe 1 (fywMaxHumidity wg)
    in FloraWorldGen
        { fwCategory = fywCategory wg
        , fwMinTemp = fywMinTemp wg
        , fwMaxTemp = fywMaxTemp wg
        , fwIdealTemp = fywIdealTemp wg
        , fwMinPrecip = fywMinPrecip wg
        , fwMaxPrecip = fywMaxPrecip wg
        , fwIdealPrecip = fywIdealPrecip wg
        , fwMinAlt = minAlt
        , fwMaxAlt = maxAlt
        , fwIdealAlt = fromMaybe ((minAlt + maxAlt) `div` 2) (fywIdealAlt wg)
        , fwMinHumidity = minHumidity
        , fwMaxHumidity = maxHumidity
        , fwIdealHumidity = fromMaybe ((minHumidity + maxHumidity) / 2)
                              (fywIdealHumidity wg)
        , fwMaxSlope = maybe 15 fromIntegral (fywMaxSlope wg)
        , fwDensity = fromMaybe 0.1 (fywDensity wg)
        , fwSoils = soilIds
        , fwFootprint = fromMaybe 0 (fywFootprint wg)
        }

assertTomatoFlora ∷ FloraYamlDef → Expectation
assertTomatoFlora def = do
    fydName def `shouldBe` "tomato_plant"
    fydType def `shouldBe` "row_crop"
    fydTexDir def `shouldBe` "assets/textures/flora/tomato_plant"
    fydLifecycle def `shouldBe` "annual"
    (fydMinLife def, fydMaxLife def, fydDeathChance def)
        `shouldBe` (Nothing, Nothing, Nothing)

    map (\p → (fypTag p, fypTexture p, fypAge p)) (fydPhases def)
        `shouldBe`
            [ ("sprout", "sprout.png", 0)
            , ("matured", "matured.png", 60)
            , ("dead", "dead.png", 360)
            ]
    map (\c → (fycsTag c, fycsStartDay c, fycsTexture c))
            (fydAnnualCycle def)
        `shouldBe`
            [ ("dormant", 0, "matured_dormant.png")
            , ("budding", 30, "matured_budding.png")
            , ("flowering", 60, "matured_flowering.png")
            , ("fruiting", 90, "matured_fruiting.png")
            , ("senescing", 240, "matured_senescing.png")
            ]
    map (\o → (fycoPhase o, fycoCycle o, fycoTexture o))
            (fydCycleOverrides def)
        `shouldBe`
            [ ("sprout", "dormant", "sprout_dormant.png")
            , ("sprout", "budding", "sprout_budding.png")
            , ("sprout", "senescing", "sprout_senescing.png")
            , ("dead", "dormant", "dead.png")
            , ("dead", "budding", "dead.png")
            , ("dead", "flowering", "dead.png")
            , ("dead", "fruiting", "dead.png")
            , ("dead", "senescing", "dead.png")
            ]

    case fydHarvest def of
        Just harvest → do
            fyhTags harvest `shouldBe` ["fruit"]
            map (\y → (fyyId y, fyyMin y, fyyMax y)) (fyhYield harvest)
                `shouldBe` [("tomato", 2, 4)]
            fyhRegrowthTime harvest `shouldBe` 43200
            fyhHarvestedTexture harvest `shouldBe` Just "matured_senescing.png"
        Nothing → expectationFailure "tomato_plant lost its harvest contract"

    let wg = fydWorldGen def
    fywCategory wg `shouldBe` "row_crop"
    (fywMinTemp wg, fywIdealTemp wg, fywMaxTemp wg)
        `shouldBe` (10, 22, 32)
    (fywMinPrecip wg, fywIdealPrecip wg, fywMaxPrecip wg)
        `shouldBe` (0.3, 0.6, 0.9)
    (fywMinAlt wg, fywIdealAlt wg, fywMaxAlt wg)
        `shouldBe` (Just (-50), Just 100, Just 400)
    (fywMinHumidity wg, fywIdealHumidity wg, fywMaxHumidity wg)
        `shouldBe` (Just 0.3, Just 0.6, Just 0.9)
    fywMaxSlope wg `shouldBe` Just 2
    fywDensity wg `shouldBe` Just 0
    fywFootprint wg `shouldBe` Just 6
    fywSoils wg `shouldBe` ["loam", "sandy_loam", "silt_loam", "clay_loam"]

assertTomatoItem ∷ ItemYamlDef → Expectation
assertTomatoItem def = do
    iydName def `shouldBe` "tomato"
    iydDisplayName def `shouldBe` "Tomato"
    iydSprite def `shouldBe` "assets/textures/items/supply/tomato.png"
    iydWeight def `shouldBe` WeightFixed 0.12
    iydBulk def `shouldBe` 0.2
    iydKind def `shouldBe` "misc"
    iydCategory def `shouldBe` "Supplies"
    iydFood def `shouldBe` Just (ItemYamlFood 35 0)

assertTomatoTextures ∷ FloraYamlDef → Expectation
assertTomatoTextures def = do
    let root = T.unpack (fydTexDir def)
        expected = sort
            [ "dead.png"
            , "matured.png"
            , "matured_budding.png"
            , "matured_dormant.png"
            , "matured_flowering.png"
            , "matured_fruiting.png"
            , "matured_senescing.png"
            , "sprout.png"
            , "sprout_budding.png"
            , "sprout_dormant.png"
            , "sprout_senescing.png"
            ]
        declared = sort ∘ nub $
            map (T.unpack ∘ fypTexture) (fydPhases def)
            ⧺ map (T.unpack ∘ fycsTexture) (fydAnnualCycle def)
            ⧺ map (T.unpack ∘ fycoTexture) (fydCycleOverrides def)
            ⧺ maybe [] (maybe [] (pure ∘ T.unpack) ∘ fyhHarvestedTexture)
                (fydHarvest def)
    actual ← sort ⊚ listDirectory root
    actual `shouldBe` expected
    declared `shouldBe` expected
    mapM (doesFileExist ∘ (root ⊘)) declared
        `shouldReturn` replicate (length declared) True
    doesFileExist "assets/textures/items/supply/tomato.png"
        `shouldReturn` True

assertCattail ∷ MaterialRegistry → FloraYamlDef → Expectation
assertCattail registry def = do
    fydName def `shouldBe` "common_cattail"
    fydType def `shouldBe` "perennial_wetland_herb"
    fydTexDir def `shouldBe` "assets/textures/flora/common_cattail"
    fydLifecycle def `shouldBe` "perennial"
    fydMinLife def `shouldBe` Just 1800
    fydMaxLife def `shouldBe` Just 7200
    fydDeathChance def `shouldBe` Just 0.05
    fydHarvest def `shouldBe` Nothing

    map (\p → (fypTag p, fypTexture p, fypAge p)) (fydPhases def)
        `shouldBe`
            [ ("sprout", "sprout.png", 0)
            , ("matured", "matured.png", 180)
            , ("dead", "dead.png", 7200)
            ]

    map (\c → (fycsTag c, fycsStartDay c, fycsTexture c))
            (fydAnnualCycle def)
        `shouldBe`
            [ ("dormant", 0, "matured_dormant.png")
            , ("budding", 60, "matured.png")
            , ("flowering", 120, "matured_flowering.png")
            , ("fruiting", 180, "matured_fruiting.png")
            , ("senescing", 260, "matured_senescing.png")
            ]

    map (\o → (fycoPhase o, fycoCycle o, fycoTexture o))
            (fydCycleOverrides def)
        `shouldBe`
            [ ("sprout", "dormant", "sprout_dormant.png")
            , ("sprout", "budding", "sprout_budding.png")
            , ("sprout", "flowering", "sprout_budding.png")
            , ("sprout", "fruiting", "sprout_budding.png")
            , ("sprout", "senescing", "sprout_senescing.png")
            ]
            ⧺ [ ("dead", cycle, "dead.png")
              | cycle ← ["dormant", "budding", "flowering", "fruiting"
                        , "senescing"] ]

    let wg = fydWorldGen def
    fywCategory wg `shouldBe` "wildflower"
    (fywMinTemp wg, fywIdealTemp wg, fywMaxTemp wg)
        `shouldBe` (-5, 14, 30)
    (fywMinPrecip wg, fywIdealPrecip wg, fywMaxPrecip wg)
        `shouldBe` (0.5, 0.8, 1.0)
    (fywMinAlt wg, fywIdealAlt wg, fywMaxAlt wg)
        `shouldBe` (Just (-30), Just 30, Just 350)
    (fywMinHumidity wg, fywIdealHumidity wg, fywMaxHumidity wg)
        `shouldBe` (Just 0.7, Just 0.9, Just 1.0)
    fywMaxSlope wg `shouldBe` Just 1
    fywDensity wg `shouldBe` Just 0.18
    fywFootprint wg `shouldBe` Just 6
    fywSoils wg `shouldBe` cattailSoils

    let declaredTextures = sort ∘ nub $
            map fypTexture (fydPhases def)
            ⧺ map fycsTexture (fydAnnualCycle def)
            ⧺ map fycoTexture (fydCycleOverrides def)
    declaredTextures `shouldBe` sort cattailTextures
    forM_ cattailTextures $ \texture →
        doesFileExist (T.unpack (fydTexDir def) ⊘ T.unpack texture)
            `shouldReturn` True

    let resolvedSoils = map (materialIdByName registry) cattailSoils
    resolvedSoils `shouldSatisfy` all isJust
    case traverse (materialIdByName registry) cattailSoils of
        Just soilIds@(firstSoil:_) → do
            let runtimeWG = toRuntimeWorldGen wg (map unMaterialId soilIds)
                idealScore soil = fst $ speciesFitnessDetail runtimeWG
                    (unMaterialId soil) 0 14 0.8 0.9 30
            map idealScore soilIds `shouldSatisfy` all (> 0)
            case materialIdByName registry "loam" of
                Just loam → fst (speciesFitnessDetail runtimeWG
                    (unMaterialId loam) 0 14 0.8 0.9 30) `shouldBe` 0
                Nothing → expectationFailure "expected ordinary loam to resolve"
            fst (speciesFitnessDetail runtimeWG
                (unMaterialId firstSoil) 0 14 0.8 0.6 30) `shouldBe` 0
        Just [] → expectationFailure "expected at least one cattail soil"
        Nothing → expectationFailure "expected all six cattail soils to resolve"

assertCattailPlacement ∷ MaterialRegistry → FloraYamlDef → Expectation
assertCattailPlacement registry def =
    case traverse (materialIdByName registry) cattailSoils of
        Just (targetSoil:_) → do
            let worldSize = 64
                area = chunkSize * chunkSize
                target = 0
                surfZ = 30
                fid = FloraId 1
                wg = toRuntimeWorldGen (fydWorldGen def)
                    [ unMaterialId soil
                    | soilName ← cattailSoils
                    , Just soil ← [materialIdByName registry soilName] ]
                species = (newFloraSpecies "common_cattail" (TextureHandle 0))
                    { fsLifecycle = Perennial 1800 7200 0.05 }
                catalog = insertWorldGen fid wg $
                    insertSpecies fid species emptyFloraCatalog
                surfaceMap = VU.replicate area minBound VU.// [(target, surfZ)]
                surfaceMats = VU.replicate area (unMaterialId targetSoil)
                surfaceSlopes = VU.replicate area 0
                exposedFluid = V.replicate area Nothing
                standingFluid = exposedFluid V.//
                    [(target, Just (FluidCell Lake surfZ))]
                climate = cattailClimate worldSize
                -- Seed 10, not the original 7: #2241 re-salted the
                -- placement roll off the species' authored NAME instead
                -- of its position in worldGenSpecies, so every seed's
                -- roll moved once. The contrast this example asserts —
                -- the SAME tile places when exposed and never when
                -- submerged — is unchanged; only the seed at which the
                -- exposed half rolls in had to be re-picked.
                place fluid = fcdInstances $ computeChunkFlora
                    "test-page" 10 worldSize (ChunkCoord 0 0)
                    surfaceMap surfaceMats surfaceSlopes fluid climate catalog
                exposed = place exposedFluid
                submerged = place standingFluid
            length exposed `shouldSatisfy` (`elem` [2, 3])
            map fiSpecies exposed `shouldSatisfy` all (≡ fid)
            submerged `shouldBe` []
        Just [] → expectationFailure "expected at least one cattail soil"
        Nothing → expectationFailure "expected all six cattail soils to resolve"

assertCattailTextures ∷ FloraYamlDef → Expectation
assertCattailTextures def = do
    let handles = HM.fromList $
            zip cattailTextures (map TextureHandle [1..])
        handleFor texture = fromMaybe (TextureHandle 999) $
            HM.lookup texture handles
        phaseRows =
            [ (tag, LifePhase tag (fypAge phase) (handleFor (fypTexture phase)))
            | phase ← fydPhases def
            , Just tag ← [parsePhaseTag (fypTag phase)] ]
        cycleRows =
            [ AnnualStage tag (fycsStartDay stage)
                (handleFor (fycsTexture stage))
            | stage ← fydAnnualCycle def
            , Just tag ← [parseCycleTag (fycsTag stage)] ]
        overrideRows =
            [ ( AnnualCycleKey phaseTag cycleTag
              , handleFor (fycoTexture override) )
            | override ← fydCycleOverrides def
            , Just phaseTag ← [parsePhaseTag (fycoPhase override)]
            , Just cycleTag ← [parseCycleTag (fycoCycle override)] ]
        fid = FloraId 1
        species = FloraSpecies
            { fsName = fydName def
            , fsBaseTexture = handleFor "matured.png"
            , fsLifecycle = Perennial 1800 7200 0.05
            , fsPhases = HM.fromList phaseRows
            , fsAnnualCycle = cycleRows
            , fsCycleOverrides = HM.fromList overrideRows
            , fsHarvest = Nothing
            }
        catalog = insertSpecies fid species emptyFloraCatalog
        mkInstance age health = FloraInstance
            { fiSpecies = fid
            , fiTileX = 0
            , fiTileY = 0
            , fiOffU = 0
            , fiOffV = 0
            , fiZ = 30
            , fiAge = age
            , fiHealth = health
            , fiVariant = 0
            , fiBaseWidth = 6
            , fiInstanceId = floraInstanceIdNone
            , fiChopDesignated = False
            }
        resolve day inst = resolveFloraTexture catalog 360 day inst
        adult = mkInstance 300 1
        juvenile = mkInstance 0 0

    length phaseRows `shouldBe` length (fydPhases def)
    length cycleRows `shouldBe` length (fydAnnualCycle def)
    length overrideRows `shouldBe` length (fydCycleOverrides def)
    resolve 0 adult `shouldBe` handleFor "matured_dormant.png"
    resolve 60 adult `shouldBe` handleFor "matured.png"
    resolve 120 adult `shouldBe` handleFor "matured_flowering.png"
    resolve 180 adult `shouldBe` handleFor "matured_fruiting.png"
    resolve 260 adult `shouldBe` handleFor "matured_senescing.png"
    resolve 0 juvenile `shouldBe` handleFor "sprout_dormant.png"
    resolve 60 juvenile `shouldBe` handleFor "sprout_budding.png"
    resolve 120 juvenile `shouldBe` handleFor "sprout_budding.png"
    resolve 180 juvenile `shouldBe` handleFor "sprout_budding.png"
    resolve 260 juvenile `shouldBe` handleFor "sprout_senescing.png"
    case instanceLifespan species (mkInstance 0 1) of
        Just lifespan →
            resolve 0 (mkInstance (lifespan + 1) 1)
                `shouldBe` handleFor "dead.png"
        Nothing → expectationFailure "expected cattail to be mortal"

cattailClimate ∷ Int → ClimateState
cattailClimate worldSize =
    let regionCount = climateRegionCount worldSize
        ideal = defaultRegionClimate
            { rcAirTemp = SeasonalClimate 14 14
            , rcPrecipitation = SeasonalClimate 0.8 0.8
            , rcHumidity = 0.9
            }
        regions = HM.fromList
            [ (ClimateCoord x y, ideal)
            | x ← [0 .. regionCount - 1]
            , y ← [0 .. regionCount - 1] ]
    in (initClimateState worldSize)
        { csClimate = ClimateGrid regions regionCount }

cattailSoils ∷ [Text]
cattailSoils =
    ["peat", "mucky_peat", "muck", "silt", "silty_clay", "silty_clay_loam"]

cattailTextures ∷ [Text]
cattailTextures =
    [ "sprout.png"
    , "sprout_dormant.png"
    , "sprout_budding.png"
    , "sprout_senescing.png"
    , "matured.png"
    , "matured_dormant.png"
    , "matured_flowering.png"
    , "matured_fruiting.png"
    , "matured_senescing.png"
    , "dead.png"
    ]
