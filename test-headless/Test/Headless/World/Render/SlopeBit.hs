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
import Data.List (sort)
import World.Chunk.Types (ChunkCoord(..), chunkSize, columnIndex)
import World.Fluid.Types (FluidCell(..), FluidType(..))
import World.Tile.Types (WorldTileData, emptyWorldTileData, insertChunk)
import World.Generate.Arena (generateFlatChunk)
import World.Slope (slopeBit, wrapChunkCoordU, slopeRecomputeAffected)

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

-- | A WorldTileData holding (flat) chunks at exactly the given coords;
--   'slopeRecomputeAffected' only inspects which coords are present.
tileWith ∷ [ChunkCoord] → WorldTileData
tileWith = foldr (insertChunk . generateFlatChunk) emptyWorldTileData

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

  -- The set the slope recompute rewrites — and which the dig-slope restore
  -- (applyDigSlopesTd) must cover exactly, or border dig masks are lost.
  describe "slopeRecomputeAffected (recompute = dig-restore set)" $ do
    it "includes a loaded chunk and its loaded neighbours" $
      sort (slopeRecomputeAffected 0 [ChunkCoord 0 0] (tileWith [ChunkCoord 0 0, ChunkCoord 1 0]))
        `shouldBe` sort [ChunkCoord 0 0, ChunkCoord 1 0]
    it "includes the loaded neighbour of an EVICTED (absent) chunk, not the chunk itself" $
      -- changed = an evicted coord (not in the tile data); its loaded
      -- neighbour must still be re-sloped (and re-dig-masked).
      slopeRecomputeAffected 0 [ChunkCoord 5 5] (tileWith [ChunkCoord 4 5])
        `shouldBe` [ChunkCoord 4 5]
    it "resolves a wrapped cross-SEAM neighbour" $
      -- east neighbour of the seam chunk (31,0) is raw (32,0), which wraps
      -- to (0,32) under w=64; the affected set must find it by wrap.
      sort (slopeRecomputeAffected 64 [ChunkCoord 31 0] (tileWith [ChunkCoord 31 0, ChunkCoord 0 32]))
        `shouldBe` sort [ChunkCoord 31 0, ChunkCoord 0 32]
