{-# LANGUAGE Strict, UnicodeSyntax #-}
-- | Pure tests for Unit.Pathing.Cost.
--   We don't need the engine; we construct a synthetic WorldTileData
--   with handcrafted terrain and fluid maps and exercise stepCost
--   against known scenarios.
module Test.Headless.Unit.Pathing.Cost (spec) where

import UPrelude
import Test.Hspec
import qualified Data.HashMap.Strict as HM
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as VU
import World.Chunk.Types (ChunkCoord(..), LoadedChunk(..), chunkSize, columnIndex)
import World.Tile.Types (WorldTileData(..))
import World.Fluid.Types (FluidCell(..), FluidType(..), emptyIceMap)
import World.Flora.Types (emptyFloraChunkData)
import Unit.Pathing.Cost

-- | Build a single chunk at the origin with uniform terrain z and
--   no fluid. Useful for "open flat terrain" tests.
flatChunk ∷ Int → LoadedChunk
flatChunk terrZ =
    let area = chunkSize * chunkSize
        terrV = VU.replicate area terrZ
        surfV = VU.replicate area terrZ
        fluidV = V.replicate area Nothing
    in LoadedChunk
        { lcCoord             = ChunkCoord 0 0
        , lcTiles             = V.empty
        , lcSurfaceMap        = surfV
        , lcTerrainSurfaceMap = terrV
        , lcFluidMap          = fluidV
        , lcIceMap            = emptyIceMap
        , lcFlora             = emptyFloraChunkData
        , lcSideDeco          = VU.empty
        , lcWaterTableMap     = VU.empty
        , lcMagma             = Nothing
        }

-- | Build a single chunk with per-tile terrain/fluid set by a function.
--   Local coordinates (lx, ly) in [0..15].
customChunk ∷ ((Int, Int) → (Int, Maybe FluidType)) → LoadedChunk
customChunk f =
    let area = chunkSize * chunkSize
        terrV = VU.generate area $ \i →
            let (lx, ly) = (i `mod` chunkSize, i `div` chunkSize)
            in fst (f (lx, ly))
        surfV = terrV
        fluidV = V.generate area $ \i →
            let (lx, ly) = (i `mod` chunkSize, i `div` chunkSize)
            in case snd (f (lx, ly)) of
                Just ft → Just (FluidCell ft (fst (f (lx, ly))))
                Nothing → Nothing
    in LoadedChunk
        { lcCoord             = ChunkCoord 0 0
        , lcTiles             = V.empty
        , lcSurfaceMap        = surfV
        , lcTerrainSurfaceMap = terrV
        , lcFluidMap          = fluidV
        , lcIceMap            = emptyIceMap
        , lcFlora             = emptyFloraChunkData
        , lcSideDeco          = VU.empty
        , lcWaterTableMap     = VU.empty
        , lcMagma             = Nothing
        }

worldWith ∷ LoadedChunk → WorldTileData
worldWith lc = WorldTileData
    { wtdChunks    = HM.singleton (ChunkCoord 0 0) lc
    , wtdMaxChunks = 1
    }

-- Tolerant float comparison for tests
approxEq ∷ Float → Float → Bool
approxEq a b = abs (a - b) < 0.001

spec ∷ Spec
spec = do
    describe "Unit.Pathing.Cost.stepCost" $ do

        describe "flat terrain" $ do
            let wtd = worldWith (flatChunk 5)

            it "cardinal step on flat terrain costs 1.0" $
                case stepCost wtd (3, 3) (4, 3) of
                    Just c  → c `shouldSatisfy` approxEq 1.0
                    Nothing → expectationFailure "expected Just"

            it "diagonal step on flat terrain costs ≈ sqrt(2)" $
                case stepCost wtd (3, 3) (4, 4) of
                    Just c  → c `shouldSatisfy` approxEq (sqrt 2)
                    Nothing → expectationFailure "expected Just"

            it "two-tile horizontal step costs 2.0" $
                case stepCost wtd (3, 3) (5, 3) of
                    Just c  → c `shouldSatisfy` approxEq 2.0
                    Nothing → expectationFailure "expected Just"

        describe "climb" $ do
            -- Half the chunk at z=5, half at z=6. A west-side tile to
            -- an east-side tile crosses a 1-z step up.
            let wtd = worldWith $ customChunk $ \(lx, _) →
                    let z = if lx < 8 then 5 else 6
                    in (z, Nothing)

            it "1-z climb adds climbFactor on top of horizontal cost" $
                case stepCost wtd (7, 5) (8, 5) of
                    Just c  → c `shouldSatisfy` approxEq (1.0 + climbFactor)
                    Nothing → expectationFailure "expected Just"

            it "stepping down 1-z adds the fall cost (small but nonzero)" $
                case stepCost wtd (8, 5) (7, 5) of
                    Just c  →
                        -- horizontalDist + fallFactor^1 = 1.0 + fallFactor
                        c `shouldSatisfy` approxEq (1.0 + fallFactor)
                    Nothing → expectationFailure "expected Just"

        describe "fall scaling" $ do
            it "a 3-z drop is much more expensive than a 1-z drop" $ do
                let wtd1 = worldWith $ customChunk $ \(lx, _) →
                        let z = if lx < 8 then 5 else 4
                        in (z, Nothing)
                    wtd3 = worldWith $ customChunk $ \(lx, _) →
                        let z = if lx < 8 then 5 else 2
                        in (z, Nothing)
                let c1 = stepCost wtd1 (7, 5) (8, 5)
                    c3 = stepCost wtd3 (7, 5) (8, 5)
                case (c1, c3) of
                    (Just v1, Just v3) → do
                        -- Exponential: cost ratio should be much greater than 3
                        (v3 / v1) `shouldSatisfy` (> 3.0)
                    _ → expectationFailure "expected Just from both"

        describe "fluid impassability" $ do
            let wtd = worldWith $ customChunk $ \(lx, _) →
                    if lx ≡ 5 then (0, Just Ocean)
                    else if lx ≡ 6 then (10, Just Lava)
                    else (5, Nothing)

            it "step into ocean is Nothing" $
                stepCost wtd (4, 3) (5, 3) `shouldBe` Nothing

            it "step into lava is Nothing" $
                stepCost wtd (7, 3) (6, 3) `shouldBe` Nothing

        describe "fluid penalties" $ do
            let wtd = worldWith $ customChunk $ \(lx, _) →
                    if lx ≡ 5 then (5, Just River)
                    else if lx ≡ 6 then (5, Just Lake)
                    else (5, Nothing)

            it "step into river adds riverPenalty" $
                case stepCost wtd (4, 3) (5, 3) of
                    Just c  → c `shouldSatisfy` approxEq (1.0 + riverPenalty)
                    Nothing → expectationFailure "expected Just"

            it "step into lake adds lakePenalty" $
                case stepCost wtd (7, 3) (6, 3) of
                    Just c  → c `shouldSatisfy` approxEq (1.0 + lakePenalty)
                    Nothing → expectationFailure "expected Just"

        describe "unloaded chunks" $ do
            it "step into a tile in an unloaded chunk is Nothing" $ do
                let wtd = worldWith (flatChunk 5)
                -- (50, 50) is in chunk (3, 3), not loaded
                stepCost wtd (15, 15) (50, 50) `shouldBe` Nothing

        describe "tunable thresholds are sane" $ do
            it "replanCostThreshold sits between flat and 1-z climb" $ do
                replanCostThreshold `shouldSatisfy` (> sqrt 2)
                replanCostThreshold `shouldSatisfy` (< 1.0 + climbFactor)
