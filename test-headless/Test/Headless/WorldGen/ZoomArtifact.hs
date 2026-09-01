{-# LANGUAGE Strict #-}
module Test.Headless.WorldGen.ZoomArtifact (spec) where

import UPrelude
import Control.Exception (finally)
import qualified Data.ByteString as BS
import Data.Either (isLeft, isRight)
import qualified Data.Text as T
import qualified Data.Vector as V
import System.Directory
    ( createDirectoryIfMissing, doesDirectoryExist, doesFileExist
    , getCurrentDirectory, getTemporaryDirectory, listDirectory
    , removeDirectoryRecursive, removeFile, setCurrentDirectory )
import Test.Hspec
import World.ZoomMap.Artifact
import World.ZoomMap.Types (ZoomChunkEntry(..), zoomTileSize)

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
        decodeZoomArtifact fixtureKey (BS.take (BS.length bytes - 1) bytes)
            `shouldSatisfy` isLeft
        let corrupt = BS.init bytes <> BS.singleton (BS.last bytes + 1)
        decodeZoomArtifact fixtureKey corrupt `shouldSatisfy` isLeft
        -- Count is the first word after magic/schema/semantic.
        let hugeCount = BS.take 16 bytes <> BS.replicate 4 255 <> BS.drop 20 bytes
            hugeKey = fixtureKey { zakEntryCount = fromIntegral (maxBound ∷ Word32) }
        decodeZoomArtifact hugeKey hugeCount `shouldSatisfy` isLeft

    it "atomically replaces one artifact and treats storage failures as misses" $
      withTempRoot $ \root → do
        firstWrite ← publishZoomArtifact fixtureKey fixtureEntries fixturePixels
        firstWrite `shouldSatisfy` isRight
        let replacement = V.map (BS.map (+ 1)) fixturePixels
        secondWrite ← publishZoomArtifact fixtureKey fixtureEntries replacement
        secondWrite `shouldSatisfy` isRight
        loaded ← loadZoomArtifact fixtureKey
        zaPixels ⊚ loaded `shouldBe` Right replacement
        names ← listDirectory (root ⊘ "cache" ⊘ "zoom")
        names `shouldBe` ["current.zarf"]

        removeDirectoryRecursive (root ⊘ "cache")
        BS.writeFile (root ⊘ "cache") "not a directory"
        failed ← publishZoomArtifact fixtureKey fixtureEntries fixturePixels
        failed `shouldSatisfy` isLeft
        loadZoomArtifact fixtureKey ≫= (`shouldSatisfy` isLeft)

fixtureKey ∷ ZoomArtifactKey
fixtureKey = ZoomArtifactKey
    { zakParamsDigest = BS.replicate 32 1
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
    previous ← getCurrentDirectory
    tmp ← getTemporaryDirectory
    let root = tmp ⊘ "synarchy-zoom-artifact-spec"
        reset = do
            directory ← doesDirectoryExist root
            when directory $ removeDirectoryRecursive root
            file ← doesFileExist root
            when file $ removeFile root
    reset
    createDirectoryIfMissing True root
    (setCurrentDirectory root >> action root)
        `finally` (setCurrentDirectory previous >> reset)
