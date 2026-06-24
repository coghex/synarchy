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
import World.Base (GeoCoord(..))
import World.Geology.Types (GeoModification(..))
import World.Hydrology.Glacier.Carving (applyGlacierMoraine)
import World.Hydrology.Types (GlacierMoraineParams(..))
import World.Material (matMoraine, unMaterialId)
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

    describe "Glacier moraines" $ do
        let moraine = GlacierMoraineParams
                { gmpCenter          = GeoCoord 0 0
                , gmpFlowDir         = 0.0
                , gmpLength          = 20
                , gmpWidth           = 10
                , gmpFootElev        = 100
                , gmpDepositHeight   = 8
                , gmpRidgeHalfLength = 4
                }
            apply = applyGlacierMoraine moraine 64

        it "deposits moraine at the former terminus" $ \_env → do
            let mod' = apply 20 0 100
            gmElevDelta mod' `shouldSatisfy` (> 0)
            gmMaterialOverride mod' `shouldBe` Just (unMaterialId matMoraine)
            gmIntrusionDepth mod' `shouldBe` gmElevDelta mod'

        it "does not deposit outside the moraine ridge" $ \_env → do
            let mod' = apply 20 20 100
            gmElevDelta mod' `shouldBe` 0
            gmMaterialOverride mod' `shouldBe` Nothing
            gmIntrusionDepth mod' `shouldBe` 0

        it "does not compound over already-high terrain" $ \_env → do
            let mod' = apply 20 0 120
            gmElevDelta mod' `shouldBe` 0
            gmMaterialOverride mod' `shouldBe` Nothing
            gmIntrusionDepth mod' `shouldBe` 0
