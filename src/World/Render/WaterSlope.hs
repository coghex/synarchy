{-# LANGUAGE Strict #-}
module World.Render.WaterSlope
    ( waterSlopeAt
    ) where

import UPrelude
import qualified Data.Vector.Unboxed as VU
import qualified Data.Vector as V
import World.Types

-- * Water Slope Helpers

-- | Compute a slope ID (Word8) for a water tile by checking if
--   adjacent water tiles have lower surfaces.
--
--   Grid-space directions map to isometric pixel-space as:
--     Grid N (ly-1) → pixel right  (East,  bit 2)
--     Grid E (lx+1) → pixel bottom (South, bit 4)
--     Grid S (ly+1) → pixel left   (West,  bit 8)
--     Grid W (lx-1) → pixel top    (North, bit 1)
--
--   Supports cross-chunk lookups via the chunkLookup function. A step
--   off this chunk is built in the home chunk's RAW frame, so at the
--   cylindrical U seam it names an alias of the coord the neighbour is
--   stored under; the caller must resolve it through
--   'World.Render.ChunkLookup' (#1135). With that in place, a False from
--   a cross-chunk branch means the neighbour genuinely isn't loaded —
--   before it, a loaded neighbour across the seam silently read as
--   absent and the slope bit was never set there.
waterSlopeAt ∷ V.Vector (Maybe FluidCell) → VU.Vector Int → ChunkCoord
             → (ChunkCoord → Maybe (V.Vector (Maybe FluidCell)))
             → (ChunkCoord → Maybe (VU.Vector Int))
             → Int → Int → Int → Word8
waterSlopeAt fluidMap terrSurfMap coord chunkLookup terrLookup lx ly mySurf =
    let checkNeighbor nx ny
            | nx ≥ 0 ∧ nx < chunkSize ∧ ny ≥ 0 ∧ ny < chunkSize =
                case fluidMap V.! (ny * chunkSize + nx) of
                    Just fc → fcSurface fc < mySurf
                    -- Dry neighbor: slope toward it if terrain is one or
                    -- more levels below the water surface (river bank /
                    -- waterfall lip).
                    Nothing → terrSurfMap VU.! (ny * chunkSize + nx) < mySurf
            | otherwise =
                -- Cross-chunk lookup
                let ChunkCoord cx cy = coord
                    (cx', lx') = if nx < 0 then (cx - 1, nx + chunkSize)
                                 else if nx ≥ chunkSize then (cx + 1, nx - chunkSize)
                                 else (cx, nx)
                    (cy', ly') = if ny < 0 then (cy - 1, ny + chunkSize)
                                 else if ny ≥ chunkSize then (cy + 1, ny - chunkSize)
                                 else (cy, ny)
                in case chunkLookup (ChunkCoord cx' cy') of
                    Nothing → False
                    Just neighborFM →
                        case neighborFM V.! (ly' * chunkSize + lx') of
                            Just fc → fcSurface fc < mySurf
                            Nothing → case terrLookup (ChunkCoord cx' cy') of
                                Nothing → False
                                Just nTerrMap →
                                    nTerrMap VU.! (ly' * chunkSize + lx') < mySurf
        -- Grid XY → UV/screen mapping. Each grid step is diagonal
        -- in UV space (u=x-y, v=x+y):
        --   Grid N (y-1) → u+, v- → pixel NE → bits 1+2 = 3
        --   Grid E (x+1) → u+, v+ → pixel SE → bits 2+4 = 6
        --   Grid S (y+1) → u-, v+ → pixel SW → bits 4+8 = 12
        --   Grid W (x-1) → u-, v- → pixel NW → bits 1+8 = 9
        gridN = checkNeighbor lx (ly - 1)
        gridE = checkNeighbor (lx + 1) ly
        gridS = checkNeighbor lx (ly + 1)
        gridW = checkNeighbor (lx - 1) ly
        raw = (if gridN then 3  else 0)   -- grid N → pixel NE (bits 1+2)
          ⌄ (if gridE then 6  else 0)   -- grid E → pixel SE (bits 2+4)
          ⌄ (if gridS then 12 else 0)   -- grid S → pixel SW (bits 4+8)
          ⌄ (if gridW then 9  else 0)   -- grid W → pixel NW (bits 1+8)
          ∷ Word8
        -- Flatten every combination whose corner bits union to 15 —
        -- which is NOT only "all four neighbours lower". Each grid
        -- direction above contributes TWO of the four corner bits, so
        -- seven of the sixteen lower-neighbour combinations reach 15:
        -- both OPPOSITE pairs (N+S = 3⌄12, E+W = 6⌄9), all four
        -- THREE-neighbour sets (N+E+S, N+E+W, N+S+W, E+S+W), and
        -- N+E+S+W. All seven would slope every direction at once and
        -- render as a nonsensical pyramid, so all seven flatten; the
        -- encoding cannot tell a three-sided lip from a four-sided one
        -- anyway.
        --
        -- The terrain path applies the same NUMERIC rule (raw ≡ 15 → 0
        -- at 'World.Slope.Compute', src/World/Slope/Compute.hs:147 for
        -- wet tiles and :153 for soft dry terrain). The topologies each
        -- catches differ: that path sets ONE bit per lower cardinal
        -- neighbour, so 15 is reachable there only with all four lower.
    in if raw ≡ 15 then 0 else raw
