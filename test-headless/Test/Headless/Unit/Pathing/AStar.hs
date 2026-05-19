{-# LANGUAGE Strict, UnicodeSyntax #-}
-- | Tests for Unit.Pathing.AStar.
--   Uses synthetic WorldTileData fixtures: a single chunk at the
--   origin with handcrafted per-tile terrain and fluid.
module Test.Headless.Unit.Pathing.AStar (spec) where

import UPrelude
import Test.Hspec
import qualified Data.HashMap.Strict as HM
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as VU
import World.Chunk.Types (ChunkCoord(..), LoadedChunk(..), chunkSize)
import World.Tile.Types (WorldTileData(..))
import World.Fluid.Types (FluidCell(..), FluidType(..), emptyIceMap)
import World.Flora.Types (emptyFloraChunkData)
import Unit.Pathing.AStar

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
        }

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
        }

worldWith ∷ LoadedChunk → WorldTileData
worldWith lc = WorldTileData
    { wtdChunks    = HM.singleton (ChunkCoord 0 0) lc
    , wtdMaxChunks = 1
    }

spec ∷ Spec
spec = do
    describe "Unit.Pathing.AStar.localAStar" $ do

        describe "open flat terrain" $ do
            let wtd = worldWith (flatChunk 5)

            it "src == dst → empty path" $
                localAStar wtd (3, 3) (3, 3) 16 `shouldBe` []

            it "one cardinal step" $
                localAStar wtd (3, 3) (4, 3) 16 `shouldBe` [(4, 3)]

            it "corner-to-corner of chunk uses 15 diagonal steps" $ do
                let path = localAStar wtd (0, 0) (15, 15) 32
                length path `shouldBe` 15
                last path `shouldBe` (15, 15)

        describe "obstacle avoidance" $ do
            -- Lava wall at lx=8 ly ∈ [1..14]; openings at ly=0 and ly=15.
            let wtd = worldWith $ customChunk $ \(lx, ly) →
                    if lx ≡ 8 ∧ ly > 0 ∧ ly < 15
                       then (10, Just Lava)
                       else (5, Nothing)
                onLava (x, y) = x ≡ 8 ∧ y > 0 ∧ y < 15

            it "routes around the lava wall" $ do
                let path = localAStar wtd (5, 5) (12, 5) 32
                any onLava path `shouldBe` False
                last path `shouldBe` (12, 5)

        describe "unreachable destination" $ do
            -- Full vertical wall at lx=8 cuts the chunk in half.
            let wtd = worldWith $ customChunk $ \(lx, _) →
                    if lx ≡ 8 then (10, Just Lava) else (5, Nothing)

            it "returns a partial path that ends at the wall" $ do
                let path = localAStar wtd (5, 5) (12, 5) 16
                case path of
                    [] → expectationFailure "expected partial progress toward wall"
                    _  → let (lx, _) = last path
                         in lx `shouldBe` 7

        describe "radius bound" $ do
            let wtd = worldWith (flatChunk 5)

            it "tiny radius yields a path that stops at the bound" $ do
                let path = localAStar wtd (0, 0) (15, 15) 3
                length path `shouldBe` 3
                last path `shouldBe` (3, 3)

        describe "high-cost climb" $ do
            -- Cheap detour around a high-cost ridge. The ridge is at
            -- lx=8, all ly. Stepping onto the ridge costs ~1+climbFactor;
            -- stepping past it via flat tiles around the corner is
            -- much cheaper if any flat passage exists.
            --
            -- Here we leave a flat passage at ly=0 only; everywhere
            -- else lx=8 is z=15 (10 z above z=5 surroundings).
            let wtd = worldWith $ customChunk $ \(lx, ly) →
                    if lx ≡ 8 ∧ ly /= 0
                       then (15, Nothing)
                       else (5, Nothing)

            it "prefers the flat passage at ly=0 over climbing the ridge" $ do
                let path = localAStar wtd (5, 5) (12, 5) 32
                -- Path must include at least one tile at ly=0.
                any (\(_, y) → y ≡ 0) path `shouldBe` True
                last path `shouldBe` (12, 5)
