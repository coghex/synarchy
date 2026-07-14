{-# LANGUAGE Strict, UnicodeSyntax #-}
-- | Pure tests for 'World.Render.WaterSlope.waterSlopeAt' — the slope
--   helper feeding rendered River/Lake quads (issue #816).
--
--   Regression under test: PR #232 relaxed only 'World.Slope.slopeBit'
--   (the TERRAIN slope path, covered by SlopeBit.hs). The freshwater
--   RENDER path, 'waterSlopeAt', still gated on an exact one-level drop
--   in all four branches, so a waterfall lip beside a multi-level drop
--   rendered flat. The fix relaxes each branch from @≡ mySurf - 1@ to
--   @< mySurf@ (one or more levels lower), leaving the wet/dry branch
--   split, equal/higher/unavailable handling, and the all-four-lower
--   flatten (@raw ≡ 15 → 0@) untouched.
module Test.Headless.World.Render.WaterSlope (spec) where

import UPrelude
import Test.Hspec
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as VU
import World.Chunk.Types (ChunkCoord(..), chunkSize, columnIndex)
import World.Fluid.Types (FluidCell(..), FluidType(..))
import World.Render.WaterSlope (waterSlopeAt)

-- | The tile under test sits away from every chunk edge so all four
--   cardinal neighbours are in-chunk by default.
home ∷ ChunkCoord
home = ChunkCoord 0 0

tx, ty, mySurf ∷ Int
tx = 8
ty = 8
mySurf = 10

-- | A flat, dry terrain map at the home tile's own surface height
--   (equal-height baseline), with the listed cells overridden.
terrMapWith ∷ [((Int, Int), Int)] → VU.Vector Int
terrMapWith overrides =
    VU.replicate (chunkSize * chunkSize) mySurf
      VU.// [ (columnIndex x y, z) | ((x, y), z) ← overrides ]

-- | An all-dry fluid map, with the listed cells set wet.
fluidMapWith ∷ [((Int, Int), FluidCell)] → V.Vector (Maybe FluidCell)
fluidMapWith cells =
    V.replicate (chunkSize * chunkSize) Nothing
      V.// [ (columnIndex x y, Just fc) | ((x, y), fc) ← cells ]

flatTerr ∷ VU.Vector Int
flatTerr = terrMapWith []

dryMap ∷ V.Vector (Maybe FluidCell)
dryMap = fluidMapWith []

noChunkLookup ∷ ChunkCoord → Maybe (V.Vector (Maybe FluidCell))
noChunkLookup _ = Nothing

noTerrLookup ∷ ChunkCoord → Maybe (VU.Vector Int)
noTerrLookup _ = Nothing

-- | Evaluate the tile at (tx,ty) against the given in-chunk maps, with
--   no cross-chunk neighbours available.
slopeAt ∷ V.Vector (Maybe FluidCell) → VU.Vector Int → Word8
slopeAt fluidMap terrMap =
    waterSlopeAt fluidMap terrMap home noChunkLookup noTerrLookup tx ty mySurf

-- East neighbour cell coords (grid E: lx+1, ly)
east ∷ (Int, Int)
east = (tx + 1, ty)

west, north, south ∷ (Int, Int)
west = (tx - 1, ty)
north = (tx, ty - 1)
south = (tx, ty + 1)

spec ∷ Spec
spec = do
  describe "dry neighbour" $ do
    it "slopes toward a neighbour exactly one level lower" $
      slopeAt dryMap (terrMapWith [(east, mySurf - 1)]) `shouldBe` 6
    it "slopes toward a multi-level drop (waterfall lip, issue #816)" $
      slopeAt dryMap (terrMapWith [(east, mySurf - 6)]) `shouldBe` 6
    it "stays flat toward an equal-height neighbour" $
      slopeAt dryMap flatTerr `shouldBe` 0
    it "stays flat toward a higher neighbour" $
      slopeAt dryMap (terrMapWith [(east, mySurf + 2)]) `shouldBe` 0

  describe "wet neighbour" $ do
    it "slopes toward a wet neighbour exactly one level lower" $
      slopeAt (fluidMapWith [(east, FluidCell River (mySurf - 1))]) flatTerr
        `shouldBe` 6
    it "slopes toward a wet neighbour a multi-level drop below" $
      slopeAt (fluidMapWith [(east, FluidCell Lake (mySurf - 5))]) flatTerr
        `shouldBe` 6
    it "stays flat toward an equal-height wet neighbour" $
      slopeAt (fluidMapWith [(east, FluidCell River mySurf)]) flatTerr
        `shouldBe` 0
    it "stays flat toward a higher wet neighbour" $
      slopeAt (fluidMapWith [(east, FluidCell River (mySurf + 3))]) flatTerr
        `shouldBe` 0

  describe "unavailable neighbour" $
    it "does not slope toward an unloaded cross-chunk neighbour" $
      -- tx=0 pushes the west lookup off-chunk; with no chunkLookup entry
      -- for the neighbouring chunk it must resolve to no slope.
      waterSlopeAt dryMap flatTerr home noChunkLookup noTerrLookup 0 ty mySurf
        `shouldBe` 0

  describe "fully enclosed water (all four neighbours lower)" $
    it "flattens instead of sloping in all four directions at once" $
      slopeAt dryMap (terrMapWith
        [ (east, mySurf - 1), (west, mySurf - 2)
        , (north, mySurf - 3), (south, mySurf - 1)
        ]) `shouldBe` 0

  describe "grid-to-render slope-direction mapping" $ do
    it "north neighbour maps to bits 1+2 (3)" $
      slopeAt dryMap (terrMapWith [(north, mySurf - 1)]) `shouldBe` 3
    it "east neighbour maps to bits 2+4 (6)" $
      slopeAt dryMap (terrMapWith [(east, mySurf - 1)]) `shouldBe` 6
    it "south neighbour maps to bits 4+8 (12)" $
      slopeAt dryMap (terrMapWith [(south, mySurf - 1)]) `shouldBe` 12
    it "west neighbour maps to bits 1+8 (9)" $
      slopeAt dryMap (terrMapWith [(west, mySurf - 1)]) `shouldBe` 9

  describe "loaded cross-chunk neighbour" $ do
    it "slopes toward a loaded cross-chunk multi-level drop, matching in-chunk" $
      -- tx=0 pushes the west lookup to chunk (-1,0), local x = chunkSize-1
      let neighborCoord = ChunkCoord (-1) 0
          neighborTerr = terrMapWith [((chunkSize - 1, ty), mySurf - 4)]
          chunkLookup c = if c ≡ neighborCoord then Just dryMap else Nothing
          terrLookup c = if c ≡ neighborCoord then Just neighborTerr else Nothing
      in waterSlopeAt dryMap flatTerr home chunkLookup terrLookup 0 ty mySurf
           `shouldBe` 9
    it "slopes toward a loaded cross-chunk wet multi-level drop" $
      let neighborCoord = ChunkCoord (-1) 0
          neighborFluid = fluidMapWith [((chunkSize - 1, ty), FluidCell River (mySurf - 3))]
          chunkLookup c = if c ≡ neighborCoord then Just neighborFluid else Nothing
          terrLookup c = if c ≡ neighborCoord then Just flatTerr else Nothing
      in waterSlopeAt dryMap flatTerr home chunkLookup terrLookup 0 ty mySurf
           `shouldBe` 9

  -- Real seed-42 tile (-55,-29), pulled from a live `--dump=terrain,fluid
  -- --seed 42 --worldSize 32` run: a river surface at z=38 borders a DRY
  -- north neighbour at z=32 (a six-level drop — the same magnitude as the
  -- issue's own (-117,22) counterexample) and a wet east neighbour at
  -- z=33 (five-level drop), with south (z=39) and west (z=43) both
  -- higher. The exact-one-level rule left this flat; the fix slopes it
  -- toward both the dry north lip and the wet east drop.
  describe "issue #816 regression (real seed-42 river lip, tile -55,-29)" $
    it "slopes north (dry, 6 levels) + east (wet, 5 levels), not south/west (higher)" $
      let mySurf816 = 38
          terr816 = terrMapWith [ (north, 32), (south, 39) ]
          fluid816 = fluidMapWith
            [ (east, FluidCell River 33), (west, FluidCell River 43) ]
      in waterSlopeAt fluid816 terr816 home noChunkLookup noTerrLookup tx ty mySurf816
           `shouldBe` 7 -- bits 1+2 (N) | 2+4 (E)

  -- The issue's own named counterexample at (-117, 22): river surface
  -- 176, dry east neighbour 170 (a six-level drop), with north, south,
  -- and west unchanged (equal height, no slope). A deterministic
  -- fixture on the exact stated numbers, since the original dump's
  -- seed/worldSize/plate parameters that produced this specific tile
  -- were not recorded in the issue and could not be reproduced (see
  -- the -55,-29 case above, pulled from a real seed-42 dump, for a
  -- live-generated confirmation of the same bug pattern).
  describe "issue #816 named counterexample ((-117,22): river 176, east dry 170)" $
    it "slopes eastward only, not north/south/west (unchanged, equal height)" $
      let mySurf816b = 176
          -- terrMapWith's default fill is the module-level `mySurf`
          -- (10), so north/south/west must be pinned to mySurf816b
          -- explicitly rather than left at the default.
          terr816b = terrMapWith
            [ (north, 176), (south, 176), (west, 176), (east, 170) ]
      in waterSlopeAt dryMap terr816b home noChunkLookup noTerrLookup tx ty mySurf816b
           `shouldBe` 6 -- bits 2+4 (E) only
