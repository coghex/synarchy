{-# LANGUAGE Strict, UnicodeSyntax #-}
-- | Pure tests for 'World.Slope.slopeBit' — the per-side slope decision.
--
--   Regression under test (issue #222): a water tile at the top of a
--   waterfall sits beside an open-air drop of MORE than one z-level, so
--   the old @diff ≡ 1@ rule never sloped it and the surface ended flat.
--   The fix lets a WET tile slope toward an IN-CHUNK neighbour that is
--   one or more levels lower (the exposed-air edge). Dry land keeps the
--   strict single-step terrace rule.
--
--   The exposed-air rule is restricted to IN-CHUNK neighbours on purpose:
--   a cross-chunk neighbour can be unloaded / evicted / seam-wrapped, so
--   keying the new slope on it would make the surface depend on chunk
--   load order. Cross-chunk drops therefore keep the old single-step
--   ('diff ≡ 1') behaviour. These tests pin both halves: in-chunk drops
--   slope; cross-chunk big drops do NOT (no new load-order/seam coupling).
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
import World.Slope (slopeBit)

-- | One chunk's fluid map: all-empty, with the listed cells set wet.
fluidMapWith ∷ [((Int, Int), FluidCell)] → V.Vector (Maybe FluidCell)
fluidMapWith cells =
    V.replicate (chunkSize * chunkSize) Nothing
      V.// [ (columnIndex x y, Just fc) | ((x, y), fc) ← cells ]

home ∷ ChunkCoord
home = ChunkCoord 0 0

noNeighbors ∷ ChunkCoord → Maybe (VU.Vector Int)
noNeighbors _ = Nothing

-- | Evaluate one side. @myZ@/@neighZ@ are surface z's; the neighbour
--   cell @(nlx,nly)@ is read from @fluidMap@ to decide if it is wet.
--   In-range @(nlx,nly)@ is an in-chunk neighbour; out-of-range is a
--   cross-chunk neighbour.
sbit ∷ Bool → Int → Int → (Int, Int) → V.Vector (Maybe FluidCell) → Bool
sbit myHasFluid myZ neighZ (nlx, nly) fluidMap =
    slopeBit myHasFluid myZ neighZ nlx nly home fluidMap noNeighbors

dryMap ∷ V.Vector (Maybe FluidCell)
dryMap = fluidMapWith []

-- | A neighbour cell at the in-chunk (6,5) that is itself wet.
wetNeighborMap ∷ V.Vector (Maybe FluidCell)
wetNeighborMap = fluidMapWith [((6, 5), FluidCell Lake 9)]

inChunk ∷ (Int, Int)
inChunk = (6, 5)             -- local coords in [0, chunkSize)

crossChunk ∷ (Int, Int)
crossChunk = (chunkSize, 5)  -- east of the chunk's east edge

spec ∷ Spec
spec = do
  describe "wet tile, in-chunk drop (issue #222 waterfall lip)" $ do
    it "slopes toward a dry neighbour a big drop below" $
      sbit True 10 7 inChunk dryMap `shouldBe` True
    it "slopes toward a neighbour exactly one lower (existing bed rule)" $
      sbit True 10 9 inChunk dryMap `shouldBe` True
    it "stays flat when the neighbour is equal height" $
      sbit True 10 10 inChunk dryMap `shouldBe` False
    it "stays flat when the neighbour is higher" $
      sbit True 10 12 inChunk dryMap `shouldBe` False

  describe "wet tile, cross-chunk neighbour (no new load-order coupling)" $ do
    it "does NOT take the new exposed-air slope toward a big drop" $
      sbit True 10 7 crossChunk dryMap `shouldBe` False
    it "still slopes toward a one-lower neighbour (existing bed rule)" $
      sbit True 10 9 crossChunk dryMap `shouldBe` True
    it "ignores the unloaded (minBound) sentinel" $
      sbit True 10 minBound crossChunk dryMap `shouldBe` False

  describe "dry land keeps the strict terrace rule" $ do
    it "slopes toward a neighbour exactly one lower" $
      sbit False 10 9 inChunk dryMap `shouldBe` True
    it "does NOT slope toward a multi-level drop" $
      sbit False 10 7 inChunk dryMap `shouldBe` False
    it "does NOT dip into a one-lower WET neighbour (bank rule)" $
      sbit False 10 9 inChunk wetNeighborMap `shouldBe` False
