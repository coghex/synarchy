module Test.Headless.WorldGen.Geology (spec) where

-- | Geology smoke tests — read-only, all against the shared canonical
--   world (42/64/3). The timeline-periods check used to generate its
--   own plates-5 world; any world has periods, so it shares too.

import UPrelude
import Test.Hspec
import qualified Data.HashMap.Strict as HM
import qualified Data.Vector.Unboxed as VU
import Engine.Core.State (EngineEnv)
import Test.Headless.Harness
import World.Types

spec ∷ SpecWith EngineEnv
spec = do

    describe "Geological features" $ do

        it "generates a geological timeline with periods" $ \env → do
            ws ← sharedWorld env 42 64 3
            mParams ← getWorldGenParams ws
            case mParams of
                Just params → do
                    let timeline = wgpGeoTimeline params
                    length (gtPeriods timeline) `shouldSatisfy` (> 0)
                Nothing → expectationFailure "params should exist"

        it "chunk surface maps have valid dimensions" $ \env → do
            ws ← sharedWorld env 42 64 3
            tiles ← getWorldTileData ws
            let chunks = HM.elems (wtdChunks tiles)
            forM_ chunks $ \lc → do
                VU.length (lcSurfaceMap lc) `shouldBe` (chunkSize * chunkSize)

        it "surface heights are within reasonable range" $ \env → do
            ws ← sharedWorld env 42 64 3
            tiles ← getWorldTileData ws
            let chunks = HM.elems (wtdChunks tiles)
            forM_ chunks $ \lc →
                forM_ (VU.toList (lcSurfaceMap lc)) $ \h → do
                    h `shouldSatisfy` (≥ (-256))
                    h `shouldSatisfy` (≤ 512)
