{-# LANGUAGE Strict #-}
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
--   wet ('sbit' resolves that cell exactly as the production caller does
--   for an in-chunk neighbour).
--
--   Issue #1685: the bank rule used to resolve the neighbour's wetness
--   INSIDE 'slopeBit', reading only the tile's own chunk, so a wet cell
--   one coordinate across a loaded chunk boundary read as dry and a soft
--   dry bank exactly one level above it still got a slope bit. The wet
--   flag is now resolved by 'World.Slope.Compute.neighborHasFluidAt' at
--   the call site — the same seam-aware lookup the hard-rock jagged path
--   already used. A pure 'slopeBit' example cannot exercise that, so the
--   cross-chunk cases below drive the PRODUCTION path
--   ('recomputeNeighborSlopes' over a real 'WorldTileData') and read the
--   resulting 'ctSlopes' bit back out.
module Test.Headless.World.Render.SlopeBit (spec) where

import UPrelude
import Test.Hspec
import qualified Data.HashMap.Strict as HM
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as VU
import Data.List (sort)
import World.Chunk.Types (ChunkCoord(..), ColumnTiles(..), LoadedChunk(..)
                         , chunkSize, columnIndex)
import World.Fluid.Types (FluidCell(..), FluidType(..))
import World.Material (MaterialId(..), MaterialProps(..), MaterialRegistry
                      , defaultMaterialProps, emptyMaterialRegistry
                      , matLoam, registerMaterial)
import World.Tile.Types (WorldTileData(..), emptyWorldTileData, insertChunk
                        , lookupChunk)
import World.Generate.Arena (generateFlatChunk)
import World.Slope (slopeBit, wrapChunkCoordU, slopeRecomputeAffected
                   , recomputeNeighborSlopes)

-- | One chunk's fluid map: all-empty, with the listed cells set wet.
fluidMapWith ∷ [((Int, Int), FluidCell)] → V.Vector (Maybe FluidCell)
fluidMapWith cells =
    V.replicate (chunkSize * chunkSize) Nothing
      V.// [ (columnIndex x y, Just fc) | ((x, y), fc) ← cells ]

home ∷ ChunkCoord
home = ChunkCoord 0 0

-- | Evaluate one side. @myZ@/@neighZ@ are surface z's (use 'minBound' for
--   @neighZ@ to model an absent neighbour); the neighbour cell @(nlx,nly)@
--   is read from @fluidMap@ to decide if it is wet — the same in-chunk
--   read 'World.Slope.Compute.neighborHasFluidAt' performs at the
--   production call site.
sbit ∷ Bool → Int → Int → (Int, Int) → V.Vector (Maybe FluidCell) → Bool
sbit myHasFluid myZ neighZ (nlx, nly) fluidMap =
    slopeBit myHasFluid myZ neighZ (isWetCell fluidMap nlx nly)

isWetCell ∷ V.Vector (Maybe FluidCell) → Int → Int → Bool
isWetCell fluidMap nlx nly = case fluidMap V.! columnIndex nlx nly of
    Just _  → True
    Nothing → False

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

-- * Cross-chunk fixtures (issue #1685)
--
--   These drive the PRODUCTION integration — 'recomputeNeighborSlopes',
--   which builds the wrapping terrain AND fluid callbacks and calls
--   'computeChunkSlopesCols' over the border strip — rather than calling
--   'slopeBit' with test-local fluid data, because the defect was partly
--   in WHICH callback the call site handed down.

-- | Seed for the recompute. Only 'applyRoughness' and the hard-rock
--   jagged path consume it, and 'softRegistry' below keeps both out of
--   the picture, so every expectation here is seed-independent.
fixtureSeed ∷ Word64
fixtureSeed = 4242

-- | Loam (what 'generateFlatChunk' lays down) registered SOFT: hardness
--   0.25 is below 'slopeHardnessThreshold' (0.7) so the soft terrace path
--   runs, and below the 0.3 roughness floor so 'applyRoughness' is the
--   identity. The stored bitmask is therefore exactly the raw slope.
softRegistry ∷ MaterialRegistry
softRegistry =
    registerMaterial (unMaterialId matLoam)
        defaultMaterialProps { mpHardness = 0.25 } emptyMaterialRegistry

-- | A uniform chunk: every column flat-topped at @z@ in loam, with the
--   listed local cells holding a lake cell.
flatChunkAt ∷ ChunkCoord → Int → [(Int, Int)] → LoadedChunk
flatChunkAt coord z wets =
    let base = generateFlatChunk coord
        area = chunkSize * chunkSize
        col  = (V.head (lcTiles base)) { ctStartZ = z }
    in base { lcTiles             = V.replicate area col
            , lcSurfaceMap        = VU.replicate area z
            , lcTerrainSurfaceMap = VU.replicate area z
            , lcFluidMap          = fluidMapWith
                  [ ((x, y), FluidCell Lake z) | (x, y) ← wets ]
            }

-- | The stored slope bitmask of local column @(lx,ly)@ in chunk @coord@.
--   'Nothing' means the chunk is not loaded at all.
slopeAt ∷ WorldTileData → ChunkCoord → (Int, Int) → Maybe Word8
slopeAt wtd coord (lx, ly) =
    (\lc → VU.head (ctSlopes (lcTiles lc V.! columnIndex lx ly)))
        <$> lookupChunk coord wtd

resloped ∷ Int → [ChunkCoord] → WorldTileData → WorldTileData
resloped worldSize changed =
    recomputeNeighborSlopes fixtureSeed worldSize softRegistry changed

evictChunk ∷ ChunkCoord → WorldTileData → WorldTileData
evictChunk coord wtd = wtd { wtdChunks = HM.delete coord (wtdChunks wtd) }

-- | East-facing slope bit (bit 1 of the N/E/S/W mask).
bitEast ∷ Word8
bitEast = 2

-- | The east-border column of a chunk, and its 4-neighbour to the east —
--   local (0, eastRow) of the NEXT chunk over.
eastBorder ∷ (Int, Int)
eastBorder = (chunkSize - 1, eastRow)

eastRow ∷ Int
eastRow = 8

-- | Home at z, its east neighbour chunk one level lower (and optionally
--   wet at the cell facing 'eastBorder'), recomputed the way a neighbour
--   load does it. 'nbrCoord' is the key the neighbour is STORED under;
--   for the seam cases that is the wrapped coord, which the production
--   lookup must find from the raw cross-boundary coord.
bankFixture ∷ Int → ChunkCoord → ChunkCoord → Bool → WorldTileData
bankFixture worldSize homeCoord nbrCoord nbrWet =
    let wets = [ (0, eastRow) | nbrWet ]
        wtd  = insertChunk (flatChunkAt nbrCoord 9 wets)
             $ insertChunk (flatChunkAt homeCoord 10 [])
               emptyWorldTileData
    in resloped worldSize [nbrCoord] wtd

seamHome ∷ ChunkCoord
seamHome = ChunkCoord 31 0

-- | 'wrapChunkCoordU 64' folds seamHome's raw east neighbour (32,0) to
--   this coord — the only key the neighbour chunk is stored under.
seamNbrWrapped ∷ ChunkCoord
seamNbrWrapped = ChunkCoord 0 32

seamWorldSize ∷ Int
seamWorldSize = 64

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

  -- Issue #1685: the dry bank rule now sees a wet neighbour that lives in
  -- the ADJACENT chunk, which is what 'slopeBit' could never do while it
  -- resolved fluid from the home chunk's own map alone. Driven through
  -- 'recomputeNeighborSlopes' so the production terrain AND fluid
  -- callbacks are the ones under test.
  describe "dry bank rule across a chunk seam (issue #1685)" $ do
    it "refuses the terrace bit for a wet neighbour in the next chunk" $
      slopeAt (bankFixture 0 home (ChunkCoord 1 0) True) home eastBorder
        `shouldBe` Just 0
    it "still takes the terrace bit for a DRY neighbour in the next chunk" $
      -- The control that keeps the fix from being a blanket suppression
      -- of every boundary slope: an ordinary one-lower dry neighbour
      -- across the same boundary still slopes.
      slopeAt (bankFixture 0 home (ChunkCoord 1 0) False) home eastBorder
        `shouldBe` Just bitEast
    it "takes no bit at all toward a chunk that is not loaded" $
      -- A missing FLUID lookup reads as dry, but the missing TERRAIN
      -- neighbour is still the 'minBound' sentinel, so no bit is set —
      -- the two halves of the absent-neighbour rule are distinct.
      slopeAt (resloped 0 [home]
                 (insertChunk (flatChunkAt home 10 []) emptyWorldTileData))
              home eastBorder
        `shouldBe` Just 0

  -- The neighbour is stored ONLY under its wrapped coord, so these pass
  -- only if the production lookups wrap the raw cross-boundary coord
  -- (World.Slope.Recompute) before consulting the loaded set.
  describe "dry bank rule across the wrapped U seam (issue #1685)" $ do
    it "refuses the terrace bit for a wet neighbour across the seam" $
      slopeAt (bankFixture seamWorldSize seamHome seamNbrWrapped True)
              seamHome eastBorder
        `shouldBe` Just 0
    it "still takes the terrace bit for a DRY neighbour across the seam" $
      -- Also proves the seam lookup RESOLVES: a wrapped chunk the lookup
      -- failed to find would read as absent and score 0 either way.
      slopeAt (bankFixture seamWorldSize seamHome seamNbrWrapped False)
              seamHome eastBorder
        `shouldBe` Just bitEast

  -- The recompute re-runs the border strip on load AND eviction, so the
  -- stored bit tracks the currently loaded set rather than the order the
  -- chunks arrived in.
  describe "seam bank rule follows the loaded set, not load order" $ do
    it "clears, restores and re-clears the bit as the neighbour changes" $ do
      let nbrCoord = ChunkCoord 1 0
          alone    = resloped 0 [home]
                       (insertChunk (flatChunkAt home 10 []) emptyWorldTileData)
          withDry  = resloped 0 [nbrCoord]
                       (insertChunk (flatChunkAt nbrCoord 9 []) alone)
          evicted  = resloped 0 [nbrCoord] (evictChunk nbrCoord withDry)
          withWet  = resloped 0 [nbrCoord]
                       (insertChunk (flatChunkAt nbrCoord 9 [(0, eastRow)]) evicted)
      map (\wtd → slopeAt wtd home eastBorder)
          [alone, withDry, evicted, withWet]
        `shouldBe` [Just 0, Just bitEast, Just 0, Just 0]
    it "gives the same bit whichever chunk was inserted first" $ do
      let nbrCoord  = ChunkCoord 1 0
          homeFirst = insertChunk (flatChunkAt nbrCoord 9 [(0, eastRow)])
                        (insertChunk (flatChunkAt home 10 []) emptyWorldTileData)
          nbrFirst  = insertChunk (flatChunkAt home 10 [])
                        (insertChunk (flatChunkAt nbrCoord 9 [(0, eastRow)])
                          emptyWorldTileData)
      map (\wtd → slopeAt wtd home eastBorder)
          [resloped 0 [nbrCoord] homeFirst, resloped 0 [home] nbrFirst]
        `shouldBe` [Just 0, Just 0]

