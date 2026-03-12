module Test.Headless.WorldGen.Geology (spec) where

import UPrelude
import Test.Hspec
import qualified Data.HashMap.Strict as HM
import qualified Data.Vector.Unboxed as VU
import Test.Headless.Harness
import World.Types

spec ∷ Spec
spec = aroundAll withHeadlessEngine $ do

    describe "Geological features" $ do

        it "generates a geological timeline with periods" $ \env → do
            sendWorldCommand env (WorldInit (WorldPageId "geo") 42 64 5)
            ws ← waitForWorldInit env (WorldPageId "geo") 180
            mParams ← getWorldGenParams ws
            case mParams of
                Just params → do
                    let timeline = wgpGeoTimeline params
                    length (gtPeriods timeline) `shouldSatisfy` (> 0)
                Nothing → expectationFailure "params should exist"

        it "chunk surface maps have valid dimensions" $ \env → do
            sendWorldCommand env (WorldInit (WorldPageId "surf") 42 64 3)
            ws ← waitForWorldInit env (WorldPageId "surf") 120
            tiles ← getWorldTileData ws
            let chunks = HM.elems (wtdChunks tiles)
            forM_ chunks $ \lc → do
                VU.length (lcSurfaceMap lc) `shouldBe` (chunkSize * chunkSize)

        it "surface heights are within reasonable range" $ \env → do
            sendWorldCommand env (WorldInit (WorldPageId "heights") 42 64 3)
            ws ← waitForWorldInit env (WorldPageId "heights") 120
            tiles ← getWorldTileData ws
            let chunks = HM.elems (wtdChunks tiles)
            forM_ chunks $ \lc →
                forM_ (VU.toList (lcSurfaceMap lc)) $ \h → do
                    h `shouldSatisfy` (≥ (-256))
                    h `shouldSatisfy` (≤ 512)
