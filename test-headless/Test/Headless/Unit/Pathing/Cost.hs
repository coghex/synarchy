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
import World.Chunk.Types (ChunkCoord(..), ColumnTiles(..), LoadedChunk(..), chunkSize)
import World.Tile.Types (WorldTileData(..))
import World.Fluid.Types (FluidCell(..), FluidType(..), emptyIceMap)
import World.Flora.Types (emptyFloraChunkData)
import Structure.Types (emptyChunkStructures)
import World.Material (MaterialRegistry, emptyMaterialRegistry)
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
        , lcStructures        = emptyChunkStructures
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
        , lcStructures        = emptyChunkStructures
        }

-- | Build a single chunk with per-tile (terrainZ, surface slope bits).
--   Unlike the fixtures above this fills lcTiles — `slopeGrade` reads
--   slope bits out of the column data (`ctSlopes`), not the surface map.
slopedChunk ∷ ((Int, Int) → (Int, Word8)) → LoadedChunk
slopedChunk f =
    let area = chunkSize * chunkSize
        terrV = VU.generate area $ \i →
            let (lx, ly) = (i `mod` chunkSize, i `div` chunkSize)
            in fst (f (lx, ly))
        colV = V.generate area $ \i →
            let (lx, ly) = (i `mod` chunkSize, i `div` chunkSize)
                (z, bits) = f (lx, ly)
            in ColumnTiles { ctStartZ = z
                           , ctMats   = VU.singleton 1
                           , ctSlopes = VU.singleton bits
                           , ctVeg    = VU.singleton 0
                           }
    in LoadedChunk
        { lcCoord             = ChunkCoord 0 0
        , lcTiles             = colV
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

worldWith ∷ LoadedChunk → WorldTileData
worldWith lc = WorldTileData
    { wtdChunks    = HM.singleton (ChunkCoord 0 0) lc
    , wtdMaxChunks = 1
    }

-- Tolerant float comparison for tests
approxEq ∷ Float → Float → Bool
approxEq a b = abs (a - b) < 0.001

-- The pathing tunables now live in a config record; the tests exercise
-- the default profile (the historical hard-coded values).
pc ∷ PathingConfig
pc = defaultPathingConfig

-- These fixtures carry no per-column material (lcTiles = V.empty), so the
-- #312 material factor is 1.0 — an empty registry keeps every cost
-- assertion below identical to the pre-#312 behaviour. The material
-- factor itself is covered in "Test.Headless.Unit.Pathing.AStar".
reg ∷ MaterialRegistry
reg = emptyMaterialRegistry

spec ∷ Spec
spec = do
    describe "Unit.Pathing.Cost.stepCost" $ do

        describe "flat terrain" $ do
            let wtd = worldWith (flatChunk 5)

            it "cardinal step on flat terrain costs 1.0" $
                case stepCost pc reg wtd (3, 3) (4, 3) of
                    Just c  → c `shouldSatisfy` approxEq 1.0
                    Nothing → expectationFailure "expected Just"

            it "diagonal step on flat terrain costs ≈ sqrt(2)" $
                case stepCost pc reg wtd (3, 3) (4, 4) of
                    Just c  → c `shouldSatisfy` approxEq (sqrt 2)
                    Nothing → expectationFailure "expected Just"

            it "two-tile horizontal step costs 2.0" $
                case stepCost pc reg wtd (3, 3) (5, 3) of
                    Just c  → c `shouldSatisfy` approxEq 2.0
                    Nothing → expectationFailure "expected Just"

        describe "climb" $ do
            -- Half the chunk at z=5, half at z=6. A west-side tile to
            -- an east-side tile crosses a 1-z step up.
            let wtd = worldWith $ customChunk $ \(lx, _) →
                    let z = if lx < 8 then 5 else 6
                    in (z, Nothing)

            it "1-z climb adds the climb factor on top of horizontal cost" $
                case stepCost pc reg wtd (7, 5) (8, 5) of
                    Just c  → c `shouldSatisfy` approxEq (1.0 + pcClimbFactor pc)
                    Nothing → expectationFailure "expected Just"

            it "stepping down 1-z is a free walk-off (no fall cost)" $
                case stepCost pc reg wtd (8, 5) (7, 5) of
                    Just c  →
                        -- A single step down is below fallTriggerDrop (2),
                        -- so it costs nothing beyond the horizontal
                        -- distance — units walk off a 1-z lip, they don't
                        -- "fall". Real (costed) falls start at a 2-z drop.
                        c `shouldSatisfy` approxEq 1.0
                    Nothing → expectationFailure "expected Just"

        describe "fall scaling" $ do
            it "a 3-z drop is much more expensive than a 1-z drop" $ do
                let wtd1 = worldWith $ customChunk $ \(lx, _) →
                        let z = if lx < 8 then 5 else 4
                        in (z, Nothing)
                    wtd3 = worldWith $ customChunk $ \(lx, _) →
                        let z = if lx < 8 then 5 else 2
                        in (z, Nothing)
                let c1 = stepCost pc reg wtd1 (7, 5) (8, 5)
                    c3 = stepCost pc reg wtd3 (7, 5) (8, 5)
                case (c1, c3) of
                    (Just v1, Just v3) → do
                        -- Exponential: cost ratio should be much greater than 3
                        (v3 / v1) `shouldSatisfy` (> 3.0)
                    _ → expectationFailure "expected Just from both"

        describe "deep-drop and deep-climb finiteness (#815)" $ do
            it "a 56-z drop with the default fall_factor stays finite (5^56 overflows Float)" $ do
                let wtd = worldWith $ customChunk $ \(lx, _) →
                        let z = if lx < 8 then 60 else 4
                        in (z, Nothing)
                case stepCost pc reg wtd (7, 5) (8, 5) of
                    Just c  → do
                        isInfinite c `shouldBe` False
                        isNaN c `shouldBe` False
                        c `shouldSatisfy` (≥ 0)
                    Nothing → expectationFailure "expected Just"

            it "an overflowing climb factor still yields a finite, non-negative cost" $ do
                let wtd = worldWith $ customChunk $ \(lx, _) →
                        let z = if lx < 8 then 4 else 60
                        in (z, Nothing)
                    pcHuge = pc { pcClimbFactor = 1 / 0 }
                case stepCost pcHuge reg wtd (7, 5) (8, 5) of
                    Just c  → do
                        isInfinite c `shouldBe` False
                        isNaN c `shouldBe` False
                        c `shouldSatisfy` (≥ 0)
                    Nothing → expectationFailure "expected Just"

        describe "fluid impassability" $ do
            let wtd = worldWith $ customChunk $ \(lx, _) →
                    if lx ≡ 5 then (0, Just Ocean)
                    else if lx ≡ 6 then (10, Just Lava)
                    else (5, Nothing)

            it "step into ocean is Nothing" $
                stepCost pc reg wtd (4, 3) (5, 3) `shouldBe` Nothing

            it "step into lava is Nothing" $
                stepCost pc reg wtd (7, 3) (6, 3) `shouldBe` Nothing

        describe "fluid penalties" $ do
            let wtd = worldWith $ customChunk $ \(lx, _) →
                    if lx ≡ 5 then (5, Just River)
                    else if lx ≡ 6 then (5, Just Lake)
                    else (5, Nothing)

            it "step into river adds the river penalty" $
                case stepCost pc reg wtd (4, 3) (5, 3) of
                    Just c  → c `shouldSatisfy` approxEq (1.0 + pcRiverPenalty pc)
                    Nothing → expectationFailure "expected Just"

            it "step into lake adds the lake penalty" $
                case stepCost pc reg wtd (7, 3) (6, 3) of
                    Just c  → c `shouldSatisfy` approxEq (1.0 + pcLakePenalty pc)
                    Nothing → expectationFailure "expected Just"

        describe "unloaded chunks" $ do
            it "step into a tile in an unloaded chunk is Nothing" $ do
                let wtd = worldWith (flatChunk 5)
                -- (50, 50) is in chunk (3, 3), not loaded
                stepCost pc reg wtd (15, 15) (50, 50) `shouldBe` Nothing

        describe "tunable thresholds are sane" $ do
            it "replan cost threshold sits between flat and 1-z climb" $ do
                pcReplanCostThreshold pc `shouldSatisfy` (> sqrt 2)
                pcReplanCostThreshold pc `shouldSatisfy` (< 1.0 + pcClimbFactor pc)

    describe "Unit.Pathing.Cost.slopeGrade (#375)" $ do
        -- One ramp tile at (8, 5): a W-facing slope (bit 3) at its
        -- surface z — the west neighbour is 1 z below, so downhill
        -- points west and heading east is straight uphill.
        let rampBitsW = 8 ∷ Word8
            wtd = worldWith $ slopedChunk $ \(lx, ly) →
                if (lx, ly) ≡ (8, 5) then (6, rampBitsW) else (5, 0)

        it "heading straight up the fall line reads +1" $
            slopeGrade wtd 8 5 6 (1, 0) `shouldSatisfy` approxEq 1.0

        it "heading straight down the fall line reads -1" $
            slopeGrade wtd 8 5 6 (-1, 0) `shouldSatisfy` approxEq (-1.0)

        it "heading across the slope reads 0" $
            slopeGrade wtd 8 5 6 (0, 1) `shouldSatisfy` approxEq 0.0

        it "a diagonal ascent reads the partial grade (≈ 0.707)" $ do
            let d = 1 / sqrt 2
            slopeGrade wtd 8 5 6 (d, -d) `shouldSatisfy` approxEq d

        it "flat tiles read 0 whatever the heading" $
            slopeGrade wtd 4 4 5 (1, 0) `shouldSatisfy` approxEq 0.0

        it "missing slope data (unloaded chunk) reads 0" $
            slopeGrade wtd 50 50 5 (1, 0) `shouldSatisfy` approxEq 0.0

        it "a degenerate crest (opposing bits) reads 0" $ do
            -- E|W both set: the downhill vectors cancel.
            let wtd' = worldWith $ slopedChunk $ \(lx, ly) →
                    if (lx, ly) ≡ (8, 5) then (6, 2 ⌄ 8) else (5, 0)
            slopeGrade wtd' 8 5 6 (1, 0) `shouldSatisfy` approxEq 0.0

    describe "Unit.Pathing.Cost.slopeSpeedFactor (#375)" $ do
        it "full uphill grade halves speed at the default penalty" $
            slopeSpeedFactor pc 1.0
                `shouldSatisfy` approxEq (1 - pcUphillSpeedPenalty pc)

        it "full downhill grade gives the mild bonus" $
            slopeSpeedFactor pc (-1.0)
                `shouldSatisfy` approxEq (1 + pcDownhillSpeedBonus pc)

        it "flat is neutral" $
            slopeSpeedFactor pc 0 `shouldSatisfy` approxEq 1.0

        it "partial grade scales proportionally" $
            slopeSpeedFactor pc 0.5
                `shouldSatisfy` approxEq (1 - 0.5 * pcUphillSpeedPenalty pc)

        it "is floored at 0.1 even for an extreme (unclamped) penalty" $
            slopeSpeedFactor pc { pcUphillSpeedPenalty = 5.0 } 1.0
                `shouldSatisfy` approxEq 0.1
