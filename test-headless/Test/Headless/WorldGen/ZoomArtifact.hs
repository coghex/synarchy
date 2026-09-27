{-# LANGUAGE Strict #-}
module Test.Headless.WorldGen.ZoomArtifact (spec, worldSpec) where

import UPrelude
import Control.DeepSeq (force)
import Control.Exception (evaluate)
import qualified Data.ByteString as BS
import Data.Either (isLeft, isRight)
import Data.IORef (readIORef)
import qualified Data.Text as T
import qualified Data.Vector as V
import System.Directory
    ( copyFile, createDirectory, listDirectory, removeDirectoryRecursive )
import Test.Hspec
import Engine.Core.Log (LogConfig(..), LoggerState, defaultLogConfig, initLogger)
import Test.Headless.Harness.Log (quietLogBackend)
import World.Material (emptyMaterialRegistry)
import Test.Headless.Harness (sharedWorld, getWorldGenParams)
import Engine.Core.State (EngineEnv, loggerRef, materialRegistryRef)
import World.Types
import World.ZoomMap.Artifact
import World.ZoomMap.Cache (buildZoomCacheWithPixels)
import World.ZoomMap.ColorPalette (buildColorPalette)
import World.Geology.Timeline.Stitch
    (buildTimelineStageCache, finishBorderedCache)
import World.Material
    ( MaterialId(..), MaterialProps(..), getMaterialProps, matGranite
    , registerMaterial )
import Test.Headless.Harness.Isolation (withExclusiveTempDirectory)

spec ∷ Spec
spec = describe "exact zoom reconstruction artifact" $ do
    it "round-trips entries and ordered RGBA blocks exactly" $ do
        let encoded = encodeZoomArtifact fixtureKey fixtureEntries fixturePixels
        case encoded ≫= decodeZoomArtifact fixtureKey of
          Left reason → expectationFailure (T.unpack reason)
          Right artifact → do
            zaEntries artifact `shouldBe` fixtureEntries
            zaPixels artifact `shouldBe` fixturePixels
            zaBytes artifact `shouldSatisfy` (> 0)

    it "rejects stale keys, truncation, corruption, and oversized counts" $ do
        let bytes = either (error . T.unpack) id $
                encodeZoomArtifact fixtureKey fixtureEntries fixturePixels
        decodeZoomArtifact fixtureKey
            { zakParamsDigest = BS.replicate 32 9 } bytes
            `shouldSatisfy` isLeft
        decodeZoomArtifact fixtureKey
            { zakProducerDigest = BS.replicate 32 9 } bytes
            `shouldSatisfy` isLeft
        decodeZoomArtifact fixtureKey
            { zakRegistryDigest = BS.replicate 32 9 } bytes
            `shouldSatisfy` isLeft
        decodeZoomArtifact fixtureKey (BS.take (BS.length bytes - 1) bytes)
            `shouldSatisfy` isLeft
        let corrupt = BS.init bytes <> BS.singleton (BS.last bytes + 1)
        decodeZoomArtifact fixtureKey corrupt `shouldSatisfy` isLeft
        -- Count is the first word after magic/schema/semantic.
        let hugeCount = BS.take 16 bytes <> BS.replicate 4 255 <> BS.drop 20 bytes
            hugeKey = fixtureKey { zakEntryCount = fromIntegral (maxBound ∷ Word32) }
        decodeZoomArtifact hugeKey hugeCount `shouldSatisfy` isLeft

    it "rejects over-cap artifacts before inspecting their vectors" $ do
        let tooLarge = fixtureKey { zakEntryCount = 32768 }
        encodeZoomArtifact tooLarge V.empty V.empty `shouldBe`
            Left "zoom artifact exceeds the 64 MiB limit"

    it "atomically replaces one artifact and treats storage failures as misses" $
      withTempRoot $ \root → do
        let path = root ⊘ "cache" ⊘ "zoom" ⊘ "current.zarf"
        firstWrite ← publishZoomArtifactAt path fixtureKey
            fixtureEntries fixturePixels
        firstWrite `shouldSatisfy` isRight
        let replacement = V.map (BS.map (+ 1)) fixturePixels
        secondWrite ← publishZoomArtifactAt path fixtureKey
            fixtureEntries replacement
        secondWrite `shouldSatisfy` isRight
        loaded ← loadZoomArtifactAt path fixtureKey
        zaPixels ⊚ loaded `shouldBe` Right replacement
        names ← listDirectory (root ⊘ "cache" ⊘ "zoom")
        names `shouldBe` ["current.zarf"]

        removeDirectoryRecursive (root ⊘ "cache")
        BS.writeFile (root ⊘ "cache") "not a directory"
        failed ← publishZoomArtifactAt path fixtureKey fixtureEntries fixturePixels
        failed `shouldSatisfy` isLeft
        loadZoomArtifactAt path fixtureKey ≫= (`shouldSatisfy` isLeft)

    paletteTextureSpec

-- | #2692: the key must depend on every texture the palette samples,
-- not only files under the four fixed resource roots.  Each fixture is
-- an isolated temp tree whose YAML names a PNG outside those roots by
-- absolute path; the PNGs are copies of tracked zoom textures.
paletteTextureSpec ∷ Spec
paletteTextureSpec = describe "palette textures outside the resource roots" $ do
    it "rekeys when only a material zoom texture's bytes change" $
      withPaletteFixture $ \fixture → do
        copyFile sandstonePng (pfTexture fixture)
        writeMaterialYaml fixture (pfTexture fixture)
        keyA ← fixtureKeyFor fixture
        copyFile shalePng (pfTexture fixture)
        keyB ← fixtureKeyFor fixture
        assertStaleArtifactRejected fixture keyA keyB

    it "rekeys when only a vegetation variant texture's bytes change" $
      withPaletteFixture $ \fixture → do
        writeVegetationYaml fixture (pfTexture fixture)
        copyFile sandstonePng (pfTexture fixture)
        keyA ← fixtureKeyFor fixture
        copyFile shalePng (pfTexture fixture)
        keyB ← fixtureKeyFor fixture
        assertStaleArtifactRejected fixture keyA keyB

    it "keys identical content identically (two successful builds)" $
      withPaletteFixture $ \fixture → do
        copyFile sandstonePng (pfTexture fixture)
        writeMaterialYaml fixture (pfTexture fixture)
        writeVegetationYaml fixture (pfTexture fixture)
        keyA ← fixtureKeyFor fixture
        keyB ← fixtureKeyFor fixture
        keyA `shouldSatisfy` isRight
        keyB `shouldSatisfy` isRight
        keyA `shouldBe` keyB

    it "refuses a key when a sampled texture is missing" $
      withPaletteFixture $ \fixture → do
        writeMaterialYaml fixture (pfTexture fixture)
        fixtureKeyFor fixture ≫= expectNamedFailure fixture "material"
        writeVegetationYaml fixture (pfTexture fixture)
        writeMaterialYaml fixture sandstonePng
        fixtureKeyFor fixture ≫= expectNamedFailure fixture "vegetation"

    it "refuses a key when a sampled texture is unreadable or undecodable" $
      withPaletteFixture $ \fixture → do
        -- A directory at the path cannot be read as a file by anyone,
        -- including root, unlike a permission-stripped file.
        createDirectory (pfTexture fixture)
        writeMaterialYaml fixture (pfTexture fixture)
        fixtureKeyFor fixture ≫= expectNamedFailure fixture "material"
        removeDirectoryRecursive (pfTexture fixture)
        BS.writeFile (pfTexture fixture) "not a png"
        fixtureKeyFor fixture ≫= expectNamedFailure fixture "material"

data PaletteFixture = PaletteFixture
    { pfRoot    ∷ FilePath
    , pfMatDir  ∷ FilePath
    , pfVegDir  ∷ FilePath
    , pfTexture ∷ FilePath
    , pfLogger  ∷ LoggerState
    }

withPaletteFixture ∷ (PaletteFixture → IO a) → IO a
withPaletteFixture action = withTempRoot $ \root → do
    let matDir = root ⊘ "materials"
        vegDir = root ⊘ "vegetation"
        textureDir = root ⊘ "custom"
    mapM_ createDirectory [matDir, vegDir, textureDir]
    logger ← initLogger defaultLogConfig { lcBackend = quietLogBackend }
    action PaletteFixture
        { pfRoot = root, pfMatDir = matDir, pfVegDir = vegDir
        , pfTexture = textureDir ⊘ "palette.png", pfLogger = logger }

sandstonePng, shalePng ∷ FilePath
sandstonePng = "assets/textures/world/zoommap/sandstone_chunk.png"
shalePng = "assets/textures/world/zoommap/shale_chunk.png"

writeMaterialYaml ∷ PaletteFixture → FilePath → IO ()
writeMaterialYaml fixture zoomPath =
    writeFile (pfMatDir fixture ⊘ "custom.yaml") $ unlines
        [ "materials:"
        , "  - id: 200"
        , "    name: custom_rock"
        , "    tile: " <> show sandstonePng
        , "    zoom: " <> show zoomPath
        , "    bg: " <> show sandstonePng
        ]

writeVegetationYaml ∷ PaletteFixture → FilePath → IO ()
writeVegetationYaml fixture variantPath =
    writeFile (pfVegDir fixture ⊘ "custom.yaml") $ unlines
        [ "vegetation:"
        , "  - id_start: 200"
        , "    name: custom_moss"
        , "    variants:"
        , "      - " <> show variantPath
        ]

fixtureKeyFor ∷ PaletteFixture → IO (Either Text ZoomArtifactKey)
fixtureKeyFor fixture = do
    palette ← buildColorPalette (pfLogger fixture)
        (pfMatDir fixture) (pfVegDir fixture)
    buildZoomArtifactKey fixtureParams emptyMaterialRegistry palette
  where
    fixtureParams = defaultWorldGenParams { wgpWorldSize = 16 }

-- | Both builds succeed, differ, and an artifact published under the
-- original texture's key is a miss under the edited texture's key.
assertStaleArtifactRejected
    ∷ PaletteFixture → Either Text ZoomArtifactKey
    → Either Text ZoomArtifactKey → IO ()
assertStaleArtifactRejected fixture keyA keyB =
    case (keyA, keyB) of
      (Right before, Right after) → do
        after `shouldNotBe` before
        let path = pfRoot fixture ⊘ "cache" ⊘ "zoom" ⊘ "current.zarf"
            count = zakEntryCount before
            entries = V.replicate count (V.head fixtureEntries)
            pixels = V.replicate count (V.head fixturePixels)
        publishZoomArtifactAt path before entries pixels
            ≫= (`shouldSatisfy` isRight)
        loadZoomArtifactAt path before ≫= (`shouldSatisfy` isRight)
        loadZoomArtifactAt path after ≫= (`shouldSatisfy` isLeft)
      _ → expectationFailure $
          "expected two successful keys, got " <> show (keyA, keyB)

expectNamedFailure
    ∷ PaletteFixture → Text → Either Text ZoomArtifactKey → IO ()
expectNamedFailure fixture kind result = case result of
    Right _ → expectationFailure "expected key construction to fail"
    Left reason → do
        T.unpack reason `shouldContain` T.unpack kind
        T.unpack reason `shouldContain` pfTexture fixture

-- | The optimization's load-bearing equality: fresh init supplies a bordered
-- terrain cache while save load reconstructs from scratch.  Both paths must
-- produce the same ordered entries and pixel blocks, and the real pair must
-- survive the exact storage codec.  This deliberately uses the suite's
-- canonical shared world instead of adding another world generation.
worldSpec ∷ SpecWith EngineEnv
worldSpec = describe "fresh/cache and load/scratch zoom identity" $
    it "matches exactly and survives publish then load (seed 42 w64 plates 3)" $
      \env → do
        ws ← sharedWorld env 42 64 3
        mParams ← getWorldGenParams ws
        params ← case mParams of
            Nothing → expectationFailure "shared world has no generation params"
                >> error "unreachable"
            Just value → pure value
        registry ← readIORef (materialRegistryRef env)
        logger ← readIORef (loggerRef env)
        palette ← buildColorPalette logger "data/materials" "data/vegetation"
        let timeline = wgpGeoTimeline params
            stageCache = buildTimelineStageCache
                (wgpSeed params) (wgpPlates params) (wgpWorldSize params)
                registry timeline
            borderedCache = finishBorderedCache (gtCoastal timeline) stageCache
            cached = buildZoomCacheWithPixels params registry palette
                         (Just borderedCache)
            scratch = buildZoomCacheWithPixels params registry palette Nothing
        cached' ← evaluate (force cached)
        scratch' ← evaluate (force scratch)
        cached' `shouldBe` scratch'

        keyResult ← buildZoomArtifactKey params registry palette
        key ← case keyResult of
            Left reason → expectationFailure (T.unpack reason) >> error "unreachable"
            Right value → pure value
        withTempRoot $ \root → do
            let path = root ⊘ "cache" ⊘ "zoom" ⊘ "current.zarf"
            published ← uncurry (publishZoomArtifactAt path key) cached'
            published `shouldSatisfy` isRight
            loaded ← loadZoomArtifactAt path key
            (zaEntries ⊚ loaded, zaPixels ⊚ loaded)
                `shouldBe` (Right (fst scratch'), Right (snd scratch'))

            let granite = getMaterialProps registry matGranite
                overriddenRegistry = registerMaterial (unMaterialId matGranite)
                    (granite { mpHardness = mpHardness granite + 0.25 }) registry
            overriddenKeyResult ← buildZoomArtifactKey params overriddenRegistry
                palette
            overriddenKey ← case overriddenKeyResult of
                Left reason → expectationFailure (T.unpack reason)
                    >> error "unreachable"
                Right value → pure value
            overriddenKey `shouldNotBe` key
            loadZoomArtifactAt path overriddenKey
                ≫= (`shouldSatisfy` isLeft)

fixtureKey ∷ ZoomArtifactKey
fixtureKey = ZoomArtifactKey
    { zakProducerDigest = BS.replicate 32 0
    , zakParamsDigest = BS.replicate 32 1
    , zakResourcesDigest = BS.replicate 32 2
    , zakRegistryDigest = BS.replicate 32 3
    , zakEntryCount = 2
    }

fixtureEntries ∷ V.Vector ZoomChunkEntry
fixtureEntries = V.fromList
    [ ZoomChunkEntry (-1) 2 (-16) 32 7 123 True False 4 True
    , ZoomChunkEntry 3 (-4) 48 (-64) 9 (-55) False True 2 False
    ]

fixturePixels ∷ V.Vector BS.ByteString
fixturePixels = V.fromList
    [ BS.replicate blockBytes 17, BS.pack (take blockBytes (cycle [0 .. 255])) ]
  where
    blockBytes = zoomTileSize * zoomTileSize * 4

withTempRoot ∷ (FilePath → IO a) → IO a
withTempRoot = withExclusiveTempDirectory "synarchy-zoom-artifact-spec"
