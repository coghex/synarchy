{-# LANGUAGE Strict, UnicodeSyntax #-}
-- | Pure tests for 'World.Slope.slopeBit' (the per-side slope decision)
--   and 'World.Slope.wrapChunkCoordU' (the seam wrap used by the
--   cross-chunk slope recompute).
--
--   Regression under test (issue #222): a water tile at the top of a
--   waterfall sits beside an open-air drop of MORE than one z-level, so
--   the old @diff ≡ 1@ rule never sloped it and the surface ended flat.
--   The fix lets a WET tile slope toward any present neighbour that is
--   one or more levels lower (the exposed-air edge). Dry land keeps the
--   strict single-step terrace rule.
--
--   An ABSENT neighbour (unloaded chunk / world edge) arrives as the
--   'minBound' sentinel and must never count as a drop; the cross-chunk
--   recompute path re-runs the border strip on load AND eviction so the
--   slope tracks the loaded set, and it wraps the lookup coord so a
--   cross-seam neighbour resolves. 'wrapChunkCoordU' is that wrap.
--
--   'slopeBit' is pure: we pass the my/neighbour surface z's directly and
--   a tiny home-chunk fluid map to control whether the neighbour cell is
--   wet. The 'neighborLookup' argument is unused by 'slopeBit' itself
--   (the caller resolves elevations first), so it is stubbed to Nothing.
module Test.Headless.World.Render.SlopeBit (spec) where

import UPrelude
import Test.Hspec
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as VU
import World.Chunk.Types (ChunkCoord(..), chunkSize, columnIndex)
import World.Fluid.Types (FluidCell(..), FluidType(..))
import World.Slope (slopeBit, wrapChunkCoordU)

-- | One chunk's fluid map: all-empty, with the listed cells set wet.
fluidMapWith ∷ [((Int, Int), FluidCell)] → V.Vector (Maybe FluidCell)
fluidMapWith cells =
    V.replicate (chunkSize * chunkSize) Nothing
      V.// [ (columnIndex x y, Just fc) | ((x, y), fc) ← cells ]

home ∷ ChunkCoord
home = ChunkCoord 0 0

noNeighbors ∷ ChunkCoord → Maybe (VU.Vector Int)
noNeighbors _ = Nothing

-- | Evaluate one side. @myZ@/@neighZ@ are surface z's (use 'minBound' for
--   @neighZ@ to model an absent neighbour); the neighbour cell @(nlx,nly)@
--   is read from @fluidMap@ to decide if it is wet.
sbit ∷ Bool → Int → Int → (Int, Int) → V.Vector (Maybe FluidCell) → Bool
sbit myHasFluid myZ neighZ (nlx, nly) fluidMap =
    slopeBit myHasFluid myZ neighZ nlx nly home fluidMap noNeighbors

dryMap ∷ V.Vector (Maybe FluidCell)
dryMap = fluidMapWith []

-- | A neighbour cell at (6,5) that is itself wet.
wetNeighborMap ∷ V.Vector (Maybe FluidCell)
wetNeighborMap = fluidMapWith [((6, 5), FluidCell Lake 9)]

nbr ∷ (Int, Int)
nbr = (6, 5)

spec ∷ Spec
spec = do
  describe "wet tile (issue #222 waterfall lip)" $ do
    it "slopes toward a present neighbour a big drop below" $
      sbit True 10 7 nbr dryMap `shouldBe` True
    it "slopes toward a neighbour exactly one lower (existing bed rule)" $
      sbit True 10 9 nbr dryMap `shouldBe` True
    it "does NOT slope toward an absent (minBound) neighbour" $
      sbit True 10 minBound nbr dryMap `shouldBe` False
    it "stays flat when the neighbour is equal height" $
      sbit True 10 10 nbr dryMap `shouldBe` False
    it "stays flat when the neighbour is higher" $
      sbit True 10 12 nbr dryMap `shouldBe` False

  describe "dry land keeps the strict terrace rule" $ do
    it "slopes toward a neighbour exactly one lower" $
      sbit False 10 9 nbr dryMap `shouldBe` True
    it "does NOT slope toward a multi-level drop" $
      sbit False 10 7 nbr dryMap `shouldBe` False
    it "does NOT dip into a one-lower WET neighbour (bank rule)" $
      sbit False 10 9 nbr wetNeighborMap `shouldBe` False

  describe "wrapChunkCoordU (cross-seam neighbour resolution)" $ do
    it "leaves an interior coord unchanged" $
      wrapChunkCoordU 64 (ChunkCoord 2 3) `shouldBe` ChunkCoord 2 3
    it "folds a coord that has crossed the u-seam back into range" $
      -- u = cx-cy = 32 is one period (w=64) past the far edge; it wraps
      -- to u = -32, i.e. the chunk stored on the opposite side.
      wrapChunkCoordU 64 (ChunkCoord 16 (-16)) `shouldBe` ChunkCoord (-16) 16
    it "is the identity for a non-wrapping (zero-size) world" $
      wrapChunkCoordU 0 (ChunkCoord 5 7) `shouldBe` ChunkCoord 5 7
