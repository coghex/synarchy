{-# LANGUAGE Strict #-}
-- | The @move_cost@ authoring domain (#1734): a material's
--   surface-traversal multiplier must reach a 'MaterialRegistry' finite
--   and strictly positive, or the planner and the mover disagree about
--   the same ground.
--
--   Coverage here is deliberately anchored at the LOADER boundary the
--   existing pathing suites never cross: they build synthetic registries
--   with 'registerMaterial' and so cannot see what @Data.Yaml@ actually
--   hands the engine. Each invalid class therefore goes through the real
--   decoder, and the emitted diagnostic is asserted, not just the value.
module Test.Headless.Asset.MaterialMoveCost (spec) where

import UPrelude
import Test.Hspec
import Data.IORef (IORef, newIORef, readIORef, modifyIORef')
import Data.List (isInfixOf)
import qualified Data.Text as T
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as VU
import qualified Data.HashMap.Strict as HM
import System.FilePath ((</>))
import Engine.Core.Log
    ( initLogger, defaultLogConfig, LogConfig(..), LogBackend(..)
    , LogCategory(..), LogLevel(..), LogEntry(..), LoggerState
    )
import Engine.Asset.YamlMaterials
    ( MaterialDef(..), defaultMoveCost, validMoveCost, loadMaterialYaml
    , materialPropsFromDef, loadPopulatedMaterialRegistry )
import World.Chunk.Types (ChunkCoord(..), LoadedChunk(..), ColumnTiles(..), chunkSize)
import World.Tile.Types (WorldTileData(..))
import World.Fluid.Types (emptyIceMap)
import World.Flora.Types (emptyFloraChunkData)
import Structure.Types (emptyChunkStructures)
import World.Material
    (MaterialRegistry, MaterialId(..), MaterialProps(..), getMaterialProps)
import Unit.Pathing.Cost (defaultPathingConfig, materialFactor)
import Unit.Thread.Movement.PathAdvance (rawStepLength)
import Test.Headless.Harness.Isolation (withExclusiveTempDirectory)

-- | A commanded speed and tick length that are themselves unremarkable:
--   both finite and strictly positive, so a zero step length can only
--   come from the material factor (the mover multiplies by BOTH of these
--   independently of any material — see 'rawStepLength').
probeSpeed ∷ Float
probeSpeed = 2.0

probeDt ∷ Double
probeDt = 1 / 60

spec ∷ Spec
spec = do

    describe "the domain itself" $ do
        it "rejects every value that makes the planner and the mover disagree" $ do
            validMoveCost (1 / 0)    `shouldBe` False   -- +Infinity
            validMoveCost (-1 / 0)   `shouldBe` False   -- -Infinity
            validMoveCost (0 / 0)    `shouldBe` False   -- NaN
            validMoveCost 0          `shouldBe` False
            validMoveCost (-2.5)     `shouldBe` False
            validMoveCost (-1.0e-6)  `shouldBe` False

        it "accepts every finite positive multiplier, shipped range and beyond" $ do
            validMoveCost defaultMoveCost `shouldBe` True
            -- The shipped corpus spans 1.1-1.8; the existing AStar suite
            -- deliberately exercises a valid 12.0, and a sub-1.0 value is
            -- authorable even though nothing ships one.
            mapM_ (\c → validMoveCost c `shouldBe` True)
                  [1.1, 1.5, 1.8, 12.0, 0.5, 1.0e-6, 3.4e38]

        it "the omitted-field default is itself inside the domain" $
            (defaultMoveCost, validMoveCost defaultMoveCost) `shouldBe` (1.0, True)

    describe "an out-of-domain value decoded from real YAML" $ do
        -- Data.Yaml decodes an oversized scalar straight to an infinity
        -- for a Float field, so the rendered value in each diagnostic
        -- below is also the proof that the decode did what the issue says.
        let cases =
                [ ("plus infinity",   "1e999",  "Infinity")
                , ("minus infinity",  "-1e999", "-Infinity")
                , ("zero",            "0",      "0.0")
                , ("finite negative", "-2.5",   "-2.5")
                ]

        mapM_ (\(label, authored, rendered) →
            it ("substitutes the default for " ⧺ label
                ⧺ ", naming the file, id and value") $
                withMaterialFiles [("bad.yaml", materialsYaml [oneMaterial 7 "silt" authored])] $ \dir → do
                    (logger, entriesRef) ← callbackLogger
                    defs ← loadMaterialYaml logger (dir </> "bad.yaml")
                    map mdMoveCost defs `shouldBe` [defaultMoveCost]
                    warns ← moveCostWarnings entriesRef
                    case warns of
                        [msg] → do
                            let s = T.unpack msg
                            (dir </> "bad.yaml") `shouldSatisfy` (`isInfixOf` s)
                            "material id 7"      `shouldSatisfy` (`isInfixOf` s)
                            "(silt)"             `shouldSatisfy` (`isInfixOf` s)
                            "move_cost"          `shouldSatisfy` (`isInfixOf` s)
                            rendered             `shouldSatisfy` (`isInfixOf` s)
                        other → expectationFailure $
                            "expected exactly one move_cost warning, got "
                                ⧺ show (length other)) cases

        -- NaN completes the domain, but it is unreachable through this
        -- decoder rather than merely unshipped: `Data.Yaml` parses plain
        -- scalars into `Scientific`, which cannot represent NaN, so the
        -- YAML 1.1 `.nan` token stays a STRING and fails the Float field
        -- outright. That is a type error one layer above the domain, and
        -- `loadYamlList` already rejects the file for it — which is why
        -- the domain's NaN half is asserted on `validMoveCost` above.
        it "a .nan scalar is refused by the decoder before the domain sees it" $
            withMaterialFiles [("nan.yaml", materialsYaml [oneMaterial 7 "silt" ".nan"])] $ \dir → do
                (logger, entriesRef) ← callbackLogger
                defs ← loadMaterialYaml logger (dir </> "nan.yaml")
                defs `shouldBe` []
                entries ← readIORef entriesRef
                let warns = [ leMessage e | e ← entries, leLevel e ≡ LevelWarn ]
                warns `shouldSatisfy` any
                    (T.isPrefixOf ("Failed to parse material YAML "
                                   <> T.pack (dir </> "nan.yaml")))

        it "is surfaced at warning severity, not debug" $
            withMaterialFiles [("bad.yaml", materialsYaml [oneMaterial 7 "silt" "1e999"])] $ \dir → do
                (logger, entriesRef) ← callbackLogger
                _ ← loadMaterialYaml logger (dir </> "bad.yaml")
                entries ← readIORef entriesRef
                let offending = [ e | e ← entries
                                , "move_cost" `T.isInfixOf` leMessage e
                                , "outside" `T.isInfixOf` leMessage e ]
                map leLevel    offending `shouldBe` [LevelWarn]
                map leCategory offending `shouldBe` [CatAsset]

    describe "an out-of-domain value costs nothing but itself" $ do
        it "a valid sibling in the same file keeps its exact authored value" $
            withMaterialFiles
                [("mixed.yaml", materialsYaml [ oneMaterial 7 "silt" "1e999"
                                              , oneMaterial 8 "sand" "1.5" ])] $ \dir → do
                (logger, entriesRef) ← callbackLogger
                defs ← loadMaterialYaml logger (dir </> "mixed.yaml")
                map (\d → (mdId d, mdName d, mdMoveCost d)) defs
                    `shouldBe` [ (7, "silt", defaultMoveCost)
                               , (8, "sand", 1.5) ]
                warns ← moveCostWarnings entriesRef
                length warns `shouldBe` 1

        it "the offending material's other fields are untouched" $
            withMaterialFiles [("bad.yaml", T.unpack (T.unlines
                [ "materials:"
                , "  - id: 7"
                , "    name: silt"
                , "    hardness: 0.25"
                , "    density: 1.8"
                , "    dig_spoil: heavy_gravel"
                , "    dig_gems: true"
                , "    move_cost: -1e999"
                , "    tile: silt_tile"
                , "    zoom: silt_zoom"
                , "    bg: silt_bg"
                ]))] $ \dir → do
                (logger, _) ← callbackLogger
                defs ← loadMaterialYaml logger (dir </> "bad.yaml")
                case defs of
                    [d] → do
                        mdMoveCost d `shouldBe` defaultMoveCost
                        mdHardness d `shouldBe` 0.25
                        mdDensity  d `shouldBe` 1.8
                        mdDigSpoil d `shouldBe` Just "heavy_gravel"
                        mdDigGems  d `shouldBe` True
                        mdTile     d `shouldBe` "silt_tile"
                    other → expectationFailure $
                        "expected one material, got " ⧺ show (length other)

    describe "accepted values pass through unchanged" $ do
        it "an omitted move_cost is exactly 1.0, with no diagnostic" $
            withMaterialFiles [("plain.yaml", T.unpack (T.unlines
                [ "materials:"
                , "  - id: 9"
                , "    name: granite"
                , "    tile: t"
                , "    zoom: z"
                , "    bg: b"
                ]))] $ \dir → do
                (logger, entriesRef) ← callbackLogger
                defs ← loadMaterialYaml logger (dir </> "plain.yaml")
                map mdMoveCost defs `shouldBe` [1.0]
                moveCostWarnings entriesRef ⌦ (`shouldBe` [])

        it "finite positive custom values survive verbatim, 12.0 included" $
            withMaterialFiles
                [("custom.yaml", materialsYaml
                    [ oneMaterial 10 "a" "1.1"
                    , oneMaterial 11 "b" "1.8"
                    , oneMaterial 12 "c" "12.0"
                    , oneMaterial 13 "d" "0.5" ])] $ \dir → do
                (logger, entriesRef) ← callbackLogger
                defs ← loadMaterialYaml logger (dir </> "custom.yaml")
                map mdMoveCost defs `shouldBe` [1.1, 1.8, 12.0, 0.5]
                moveCostWarnings entriesRef ⌦ (`shouldBe` [])

        it "every shipped material loads in-domain and unwarned" $ do
            (logger, entriesRef) ← callbackLogger
            reg ← loadPopulatedMaterialRegistry logger "data/materials"
            moveCostWarnings entriesRef ⌦ (`shouldBe` [])
            -- Every slot, registered or not: an unregistered one holds
            -- `defaultMaterialProps`, whose 1.0 is in-domain by the same
            -- rule, so nothing here needs to know the shipped id set.
            let bad = [ (i, mpMoveCost (props reg i))
                      | i ← [0 .. 255]
                      , not (validMoveCost (mpMoveCost (props reg i))) ]
            bad `shouldBe` []

    describe "both registration paths consume the validated value" $ do
        -- Normalization lives in `loadMaterialYaml`, the one decode
        -- boundary both paths cross, and `materialPropsFromDef` is the
        -- one conversion both perform — so neither can construct props
        -- from an un-normalized def without bypassing the loader itself.
        it "loadPopulatedMaterialRegistry registers the substituted default" $
            withMaterialFiles
                [("bad.yaml", materialsYaml [ oneMaterial 7 "silt" "1e999"
                                            , oneMaterial 8 "sand" "1.5" ])] $ \dir → do
                (logger, _) ← callbackLogger
                reg ← loadPopulatedMaterialRegistry logger dir
                mpMoveCost (props reg 7) `shouldBe` defaultMoveCost
                mpMoveCost (props reg 8) `shouldBe` 1.5

        it "materialPropsFromDef — the Lua path's conversion — carries it too" $
            withMaterialFiles
                [("bad.yaml", materialsYaml [ oneMaterial 7 "silt" "-1e999"
                                            , oneMaterial 8 "sand" "1.5" ])] $ \dir → do
                (logger, _) ← callbackLogger
                defs ← loadMaterialYaml logger (dir </> "bad.yaml")
                map (mpMoveCost ∘ materialPropsFromDef) defs
                    `shouldBe` [defaultMoveCost, 1.5]

    describe "the runtime invariant the domain buys" $ do
        let invalidFixture = materialsYaml
                [ oneMaterial 1 "inf"      "1e999"
                , oneMaterial 2 "neginf"   "-1e999"
                , oneMaterial 3 "zero"     "0"
                , oneMaterial 4 "negative" "-2.5"
                , oneMaterial 5 "soft"     "1.8"
                , oneMaterial 6 "costly"   "12.0"
                ]

        it "materialFactor is finite and positive for every registered material" $
            withMaterialFiles [("m.yaml", invalidFixture)] $ \dir → do
                (logger, _) ← callbackLogger
                reg ← loadPopulatedMaterialRegistry logger dir
                let factors = [ materialFactor reg (worldOfMaterial i) 0 0
                              | i ← [1 .. 6] ]
                factors `shouldSatisfy` all (\f → not (isNaN f) ∧ not (isInfinite f) ∧ f > 0)
                -- The rescued classes land on firm ground, not on the
                -- `max 0.1` floor that used to make them 10x FASTER
                -- than bare rock and cheap enough for A* to prefer.
                take 4 factors `shouldBe` replicate 4 1.0
                drop 4 factors `shouldBe` [1.8, 12.0]

        it "a unit's step length stays positive on every registered material" $
            withMaterialFiles [("m.yaml", invalidFixture)] $ \dir → do
                (logger, _) ← callbackLogger
                reg ← loadPopulatedMaterialRegistry logger dir
                let steps = [ rawStepLength defaultPathingConfig probeSpeed 0
                                  (materialFactor reg (worldOfMaterial i) 0 0)
                                  probeDt
                            | i ← [1 .. 6] ]
                steps `shouldSatisfy` all (\s → not (isNaN s) ∧ not (isInfinite s) ∧ s > 0)
                -- The freeze this issue is named for: before the domain,
                -- material 1 (+Infinity) made this exactly 0 forever.
                case steps of
                    (onInfinity : _) →
                        onInfinity `shouldBe` probeSpeed * realToFrac probeDt
                    [] → expectationFailure "no step lengths computed"

        it "shipped materials keep a positive step length too" $ do
            (logger, _) ← callbackLogger
            reg ← loadPopulatedMaterialRegistry logger "data/materials"
            let steps = [ rawStepLength defaultPathingConfig probeSpeed 0
                              (materialFactor reg (worldOfMaterial i) 0 0)
                              probeDt
                        | i ← [0 .. 255] ]
            steps `shouldSatisfy` all (\s → not (isNaN s) ∧ not (isInfinite s) ∧ s > 0)

props ∷ MaterialRegistry → Word8 → MaterialProps
props reg i = getMaterialProps reg (MaterialId (fromIntegral i))

-- | A one-chunk world whose every column is a single surface tile made
--   of @mid@ — enough for 'materialFactor', which reads the top of the
--   column at the queried tile.
worldOfMaterial ∷ Word8 → WorldTileData
worldOfMaterial mid =
    let area  = chunkSize * chunkSize
        terrV = VU.replicate area 5
        tiles = V.replicate area (ColumnTiles
                    { ctStartZ = 5
                    , ctMats   = VU.singleton mid
                    , ctSlopes = VU.singleton 0
                    , ctVeg    = VU.singleton 0 })
        lc = LoadedChunk
            { lcCoord             = ChunkCoord 0 0
            , lcTiles             = tiles
            , lcSurfaceMap        = terrV
            , lcTerrainSurfaceMap = terrV
            , lcFluidMap          = V.replicate area Nothing
            , lcIceMap            = emptyIceMap
            , lcFlora             = emptyFloraChunkData
            , lcSideDeco          = VU.empty
            , lcWaterTableMap     = VU.empty
            , lcMagma             = Nothing
            , lcStructures        = emptyChunkStructures
            }
    in WorldTileData { wtdChunks = HM.singleton (ChunkCoord 0 0) lc
                     , wtdMaxChunks = 1 }

-- | One material entry, with only the mandatory fields plus the
--   @move_cost@ under test.
oneMaterial ∷ Int → String → String → String
oneMaterial mid name authored = unlines
    [ "  - id: " ⧺ show mid
    , "    name: " ⧺ name
    , "    move_cost: " ⧺ authored
    , "    tile: " ⧺ name ⧺ "_tile"
    , "    zoom: " ⧺ name ⧺ "_zoom"
    , "    bg: " ⧺ name ⧺ "_bg"
    ]

materialsYaml ∷ [String] → String
materialsYaml entries = "materials:\n" ⧺ concat entries

-- | The @move_cost@ domain diagnostics only, so an unrelated asset log
--   line can't satisfy or break a count.
moveCostWarnings ∷ IORef [LogEntry] → IO [Text]
moveCostWarnings entriesRef = do
    entries ← readIORef entriesRef
    pure [ leMessage e
         | e ← reverse entries
         , leLevel e ≡ LevelWarn
         , "Invalid move_cost in material YAML" `T.isPrefixOf` leMessage e ]

callbackLogger ∷ IO (LoggerState, IORef [LogEntry])
callbackLogger = do
    entriesRef ← newIORef []
    logger ← initLogger defaultLogConfig
        { lcBackend = LogToCallback (\e → modifyIORef' entriesRef (e :))
        , lcDebugCategories = [CatAsset]
        }
    pure (logger, entriesRef)

withMaterialFiles ∷ [(FilePath, String)] → (FilePath → IO a) → IO a
withMaterialFiles files action =
    withExclusiveTempDirectory "synarchy-move-cost-spec" $ \dir → do
        mapM_ (\(name, contents) → writeFile (dir </> name) contents) files
        action dir
