{-# LANGUAGE Strict, UnicodeSyntax #-}
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
--   Supports cross-chunk lookups via the chunkLookup function.
waterSlopeAt ∷ V.Vector (Maybe FluidCell) → VU.Vector Int → ChunkCoord
             → (ChunkCoord → Maybe (V.Vector (Maybe FluidCell)))
             → (ChunkCoord → Maybe (VU.Vector Int))
             → Int → Int → Int → Word8
waterSlopeAt fluidMap terrSurfMap coord chunkLookup terrLookup lx ly mySurf =
    let checkNeighbor nx ny
            | nx ≥ 0 ∧ nx < chunkSize ∧ ny ≥ 0 ∧ ny < chunkSize =
                case fluidMap V.! (ny * chunkSize + nx) of
                    Just fc → fcSurface fc ≡ mySurf - 1
                    -- Dry neighbor: slope toward it if terrain is
                    -- exactly 1 below water surface (river bank).
                    Nothing → terrSurfMap VU.! (ny * chunkSize + nx) ≡ mySurf - 1
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
                            Just fc → fcSurface fc ≡ mySurf - 1
                            Nothing → case terrLookup (ChunkCoord cx' cy') of
                                Nothing → False
                                Just nTerrMap →
                                    nTerrMap VU.! (ly' * chunkSize + lx') ≡ mySurf - 1
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
          .|. (if gridE then 6  else 0)   -- grid E → pixel SE (bits 2+4)
          .|. (if gridS then 12 else 0)   -- grid S → pixel SW (bits 4+8)
          .|. (if gridW then 9  else 0)   -- grid W → pixel NW (bits 1+8)
          ∷ Word8
    in if raw ≡ 15 then 0 else raw
