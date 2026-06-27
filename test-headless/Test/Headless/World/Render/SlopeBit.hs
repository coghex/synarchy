{-# LANGUAGE Strict, UnicodeSyntax #-}
-- | Pure tests for 'World.Slope.slopeBit' — the per-side slope decision.
--
--   Regression under test (issue #222): a water tile at the top of a
--   waterfall sits beside an open-air drop of MORE than one z-level, so
--   the old @diff ≡ 1@ rule never sloped it and the surface ended flat.
--   The fix lets a WET tile slope toward any LOADED neighbour that is one
--   or more levels lower (the exposed-air edge), while DRY land keeps the
--   strict single-step terrace rule. The unloaded-neighbour sentinel
--   ('minBound', from 'neighborElev') must never count as a drop.
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
sbit ∷ Bool → Int → Int → (Int, Int) → V.Vector (Maybe FluidCell) → Bool
sbit myHasFluid myZ neighZ (nlx, nly) fluidMap =
    slopeBit myHasFluid myZ neighZ nlx nly home fluidMap noNeighbors

dryMap ∷ V.Vector (Maybe FluidCell)
dryMap = fluidMapWith []

-- | A neighbour cell at (6,5) that is itself wet.
wetNeighborMap ∷ V.Vector (Maybe FluidCell)
wetNeighborMap = fluidMapWith [((6, 5), FluidCell Lake 9)]

spec ∷ Spec
spec = do
  describe "wet tile (issue #222 waterfall lip)" $ do
    it "slopes toward a loaded dry neighbour a big drop below" $
      sbit True 10 7 (6, 5) dryMap `shouldBe` True
    it "slopes toward a neighbour exactly one lower (existing bed rule)" $
      sbit True 10 9 (6, 5) dryMap `shouldBe` True
    it "does NOT slope toward an unloaded (minBound) neighbour" $
      sbit True 10 minBound (6, 5) dryMap `shouldBe` False
    it "stays flat when the neighbour is equal height" $
      sbit True 10 10 (6, 5) dryMap `shouldBe` False
    it "stays flat when the neighbour is higher" $
      sbit True 10 12 (6, 5) dryMap `shouldBe` False

  describe "dry land keeps the strict terrace rule" $ do
    it "slopes toward a neighbour exactly one lower" $
      sbit False 10 9 (6, 5) dryMap `shouldBe` True
    it "does NOT slope toward a multi-level drop" $
      sbit False 10 7 (6, 5) dryMap `shouldBe` False
    it "does NOT dip into a one-lower WET neighbour (bank rule)" $
      sbit False 10 9 (6, 5) wetNeighborMap `shouldBe` False
