{-# LANGUAGE Strict #-}
module Test.Headless.WorldGen.ZoomArtifact (spec, worldSpec) where

import UPrelude
import Control.DeepSeq (force)
import Control.Exception (evaluate, finally)
import qualified Data.ByteString as BS
import Data.Either (isLeft, isRight)
import Data.IORef (readIORef)
import qualified Data.Text as T
import qualified Data.Vector as V
import System.Directory
    ( createDirectory, doesDirectoryExist, doesFileExist
    , getTemporaryDirectory, listDirectory, removeDirectoryRecursive
    , removeFile )
import System.IO (hClose, openBinaryTempFile)
import Test.Hspec
import Test.Headless.Harness (sharedWorld, getWorldGenParams)
import Engine.Core.State (EngineEnv, loggerRef, materialRegistryRef)
import World.Types
import World.ZoomMap.Artifact
import World.ZoomMap.Cache (buildZoomCacheWithPixels)
import World.ZoomMap.ColorPalette (buildColorPalette)
import World.Geology.Timeline.Stitch
    (buildTimelineStageCache, finishBorderedCache)

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

        keyResult ← buildZoomArtifactKey params
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

fixtureKey ∷ ZoomArtifactKey
fixtureKey = ZoomArtifactKey
    { zakProducerDigest = BS.replicate 32 0
    , zakParamsDigest = BS.replicate 32 1
    , zakResourcesDigest = BS.replicate 32 2
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
withTempRoot action = do
    tmp ← getTemporaryDirectory
    (root, handle) ← openBinaryTempFile tmp "synarchy-zoom-artifact-spec"
    hClose handle
    removeFile root
    createDirectory root
    let reset = do
            directory ← doesDirectoryExist root
            when directory $ removeDirectoryRecursive root
            file ← doesFileExist root
            when file $ removeFile root
    action root `finally` reset
