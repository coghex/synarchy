{-# LANGUAGE Strict #-}
-- | The zoom cache's presentation-only ocean boundary fill, split out
--   of "World.ZoomMap.Cache.BuildPixels" so it can be pinned on a
--   constructed chunk (issue #2316).
--
--   The fill exists because 'World.Generate.Chunk.Fluid.composeFluidMap'
--   gates ocean on @terrain ≤ seaLevel ∧ (chunkIsOceanic ∨ worldOceanBit)@:
--   a sub-sea tile in a chunk that neither the coarse chunk flood nor
--   the tile-resolution mask claims composes DRY, and composition is
--   per-chunk so it cannot see the ocean one tile away in the chunk
--   next door either. Closing those one-tile shoreline gaps is what
--   this pass is for, so the composed mask does NOT make it redundant
--   and it is kept rather than deleted (#2316 requirement 6).
--
--   What it is not is a flood. It is exactly ONE cardinal dilation of
--   the composed ocean mask: every neighbour test reads the immutable
--   composed map, never the map being produced, so a synthesized cell
--   can never seed another one and the result does not depend on the
--   order tiles are visited in.
module World.ZoomMap.Cache.OceanFill
    ( extendOceanBoundary
    ) where

import UPrelude
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as VU
import World.Fluid.Internal (FluidMap)
import World.Types

-- | One cardinal dilation of a chunk's composed ocean mask.
--
--   A tile is promoted to @'FluidCell' 'Ocean' seaLevel@ when all of:
--
--     * it composed dry,
--     * its surface z is real and at most @seaLevel + 2@, and
--     * a cardinal neighbour is OCEAN in the composed map — River,
--       Lake and Lava neighbours seed nothing, matching the
--       cross-chunk probe, which has always tested only for ocean.
--
--   Neighbours outside the chunk are delegated to the caller's probe,
--   which is given the out-of-range local coordinate (one axis at
--   @-1@ or @chunkSize@) and answers for the adjacent chunk.
--
--   Every lookup is against @composed@, so this is a pure function of
--   the composed map, the elevations and the probe: promoted tiles are
--   invisible to the promotion test, and scanning the chunk in any
--   order yields the same vector.
extendOceanBoundary
    ∷ (Int → Int → Bool)  -- ^ Neighbour-chunk ocean probe.
    → VU.Vector Int       -- ^ Per-tile surface z ('minBound' = no tile).
    → FluidMap            -- ^ Composed fluid map for the chunk.
    → FluidMap
extendOceanBoundary neighborHasOcean elevs composed = V.imap extend composed
  where
    extend idx cell
      | isJust cell                       = cell
      | admits idx ∧ adjacentToOcean idx  = Just (FluidCell Ocean seaLevel)
      | otherwise                         = cell

    -- The admission bound is unchanged: tiles up to two above sea
    -- level are eligible, even though only those at or below it paint
    -- as ocean (see 'World.ZoomMap.Cache.Pixels.generateChunkPixels').
    admits idx =
        let z = elevs VU.! idx
        in z ≤ seaLevel + 2 ∧ z > minBound

    adjacentToOcean idx =
        let lx = idx `mod` chunkSize
            ly = idx `div` chunkSize
        in oceanAt lx (ly - 1) ∨ oceanAt lx (ly + 1)
           ∨ oceanAt (lx - 1) ly ∨ oceanAt (lx + 1) ly

    oceanAt x y
      | x ≥ 0 ∧ x < chunkSize ∧ y ≥ 0 ∧ y < chunkSize =
          case composed V.! (y * chunkSize + x) of
              Just (FluidCell Ocean _) → True
              _                        → False
      | otherwise = neighborHasOcean x y
