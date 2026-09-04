{-# LANGUAGE Strict #-}

-- | Shared low-level primitives for the river-identification pipeline
--   ("World.Fluid.River.Identify" and its siblings): D4 direction codes,
--   the world-grid stepping function, the descending-sort helper
--   used by both the flow/spillway pass and the component-culling pass,
--   and the per-tile spillway-ownership relation those two passes share.
module World.Fluid.River.Identify.Common
    ( dirNorth, dirEast, dirSouth, dirWest, dirNone
    , stepDir
    , sortDescOn
    , SpillwayOwners(..)
    , spillwayOwnersAt
    , spillwayOwnerCount
    , isSpillwayOwner
    ) where

import UPrelude
import qualified Data.Vector.Unboxed as VU

-- * Direction codes
--
-- We use 'Word8' for D4 codes packed into one array per world tile.

dirNorth, dirEast, dirSouth, dirWest, dirNone ∷ Word8
dirNorth = 0
dirEast  = 1
dirSouth = 2
dirWest  = 3
dirNone  = 4

-- | Apply a D4 direction to a tile index. The world wraps along the
--   isometric u-axis (gx − gy), which the gx-grid here can't honour
--   exactly without a full coordinate rework. East/west steps wrap as
--   a plain x-axis torus: stepping east from the rightmost column
--   lands at the leftmost column of the same row.
--
--   NOTE on what this wrap actually does: the grid's x-edge columns
--   sit at the diamond world's glacier corners (the torus edge pairs
--   tiles whose v differs by ~worldTiles, so at least one side is
--   glacier/void in every pairing). The torus edges therefore connect
--   real terrain to 'minBound' walls in practice and are no-ops for
--   flow. Seam continuity does NOT come from this wrap — it comes
--   from the stitched grid double-covering the seam region (every
--   near-seam tile appears at both its canonical position and its
--   u-alias, each with a full in-grid neighbourhood). The lake
--   identifier ('World.Fluid.Lake.Identify') treats the x-edges as
--   plain walls for the same reason — the two conventions are
--   equivalent on real terrain.
--
--   N/S steps return 'Nothing' at the world's v-axis boundary
--   (where 'isBeyondGlacier' would have cut terrain anyway).
{-# INLINE stepDir #-}
stepDir ∷ Int → Int → Word8 → Maybe Int
stepDir worldTiles idx d
    | d ≡ dirNorth = let by = idx `div` worldTiles
                     in if by > 0 then Just (idx - worldTiles) else Nothing
    | d ≡ dirSouth = let by = idx `div` worldTiles
                     in if by < worldTiles - 1 then Just (idx + worldTiles)
                                              else Nothing
    | d ≡ dirEast  = let bx = idx `mod` worldTiles
                     in Just (if bx < worldTiles - 1
                              then idx + 1
                              else idx + 1 - worldTiles)
    | d ≡ dirWest  = let bx = idx `mod` worldTiles
                     in Just (if bx > 0
                              then idx - 1
                              else idx - 1 + worldTiles)
    | otherwise    = Nothing

-- | Insertion sort, descending by a projected key. Insertion sort is
--   fine for these — n is bounded by world area (spillway count /
--   raw river count). Shared by the spillway-injection order
--   (key = @(z,_,_)@) and the river-component keep order
--   (key = @counts VU.! i@).
sortDescOn ∷ Ord b ⇒ (a → b) → [a] → [a]
sortDescOn key = foldr insertDesc []
  where
    insertDesc x [] = [x]
    insertDesc x (y:rest)
        | key x ≥ key y = x : y : rest
        | otherwise     = y : insertDesc x rest

-- * Spillway ownership

-- | Which lakes spill through a tile (#2323).
--
--   A spillway tile is the lowest above-sea external neighbour of some
--   basin, and nothing stops two ADJACENT basins from independently
--   picking the same tile. The relation is therefore one-to-many, and
--   collapsing it to a per-tile scalar silently discarded every
--   contributor but the last one written — which misrouted the
--   discarded lake's outflow straight back into its own basin and
--   misreported the resulting river's source.
--
--   Stored compressed-sparse-row: 'spoOffsets' has length
--   @nTiles + 1@, and tile @t@'s contributors are the slice
--   @spoLakes[spoOffsets ! t .. spoOffsets ! (t+1) - 1]@. That slice is
--   always in ascending 'World.Fluid.Lake.Types.LakeId' order, so no
--   consumer ever sees a traversal-order-dependent owner. Most tiles
--   own an empty slice.
data SpillwayOwners = SpillwayOwners
    { spoOffsets ∷ !(VU.Vector Int)
      -- ^ Per-tile start offsets into 'spoLakes'; length @nTiles + 1@.
    , spoLakes   ∷ !(VU.Vector Int)
      -- ^ Contributor lake ids, grouped by tile, ascending within
      --   each tile's group.
    } deriving (Show, Eq)

-- | The complete contributor set of one tile, ascending. Empty when
--   the tile is no lake's spillway.
{-# INLINE spillwayOwnersAt #-}
spillwayOwnersAt ∷ SpillwayOwners → Int → VU.Vector Int
spillwayOwnersAt (SpillwayOwners offs lids) idx =
    let lo = offs VU.! idx
        hi = offs VU.! (idx + 1)
    in VU.slice lo (hi - lo) lids

-- | How many lakes spill through this tile. @0@ for an ordinary tile,
--   @1@ for a unique-owner spillway, @≥ 2@ for a shared one.
{-# INLINE spillwayOwnerCount #-}
spillwayOwnerCount ∷ SpillwayOwners → Int → Int
spillwayOwnerCount (SpillwayOwners offs _) idx =
    (offs VU.! (idx + 1)) - (offs VU.! idx)

-- | Does lake @lid@ spill through tile @idx@? Hot path — the empty
--   case (nearly every tile) costs two vector reads and a comparison,
--   and the occupied case scans a slice of one or two elements.
{-# INLINE isSpillwayOwner #-}
isSpillwayOwner ∷ SpillwayOwners → Int → Int → Bool
isSpillwayOwner (SpillwayOwners offs lids) idx lid =
    let lo = offs VU.! idx
        hi = offs VU.! (idx + 1)
        scan i | i ≥ hi              = False
               | lids VU.! i ≡ lid   = True
               | otherwise           = scan (i + 1)
    in lo < hi ∧ scan lo
