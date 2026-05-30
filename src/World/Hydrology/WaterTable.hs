{-# LANGUAGE Strict, UnicodeSyntax #-}

-- | Per-chunk water-table computation — Phase 2 simplified.
--
--   The water table is the elevation of the saturation horizon: below
--   it the ground is saturated (groundwater), above it dry. Surface
--   lakes are now placed by the global 'WorldLakes' table identified
--   at world init (see "World.Fluid.Lake.Identify"); this module is
--   responsible for the SUBSURFACE water table only — what a player
--   sees when they dig.
--
--   The model is just: wt[t] = terrain[t] − depthFromClimate(t).
--   Wet climates push the saturated horizon up close to the surface;
--   arid climates push it far below. No priority flood, no per-tile
--   spillway propagation — the global table handles surface placement,
--   and 'World.Generate.Chunk' bumps the wt up to the lake surface
--   under lake beds after the fluid map is known, so a dig through a
--   lake floor still exposes water.
module World.Hydrology.WaterTable
    ( computeWaterTable
    , depthFromClimate
    , isSubsurfaceWet
    , waterTableAtTile
    ) where

import UPrelude
import qualified Data.Vector.Unboxed as VU
import World.Chunk.Types (LoadedChunk(..), chunkSize)
import World.Plate (wrapGlobalU)
import World.Types (ChunkCoord(..))
import World.Weather.Lookup (LocalClimate(..), lookupLocalClimate)
import World.Weather.Types (ClimateState)

-- | Vertical distance below terrain that the water table sits at, in
--   undisturbed land. Climate-only — wet climates shallow, arid deep.
depthFromClimate ∷ LocalClimate → Int
depthFromClimate c =
    let p = lcPrecip c   -- 0..1
        t = lcTemp c     -- °C
        baseDepth
          | p > 0.40   = 2
          | p > 0.20   = 5
          | p > 0.10   = 15
          | otherwise = 40
        aridPenalty
          | t > 25.0 ∧ p < 0.15 = 40
          | t > 20.0 ∧ p < 0.25 = 15
          | otherwise           = 0
    in baseDepth + aridPenalty

-- | Compute the water-table z-value for each tile in a chunk's
--   interior.
--
--   Output: 'VU.Vector Int' of length @chunkSize * chunkSize@, indexed
--   @ly * chunkSize + lx@.
--
--   This is the subsurface baseline. Surface water (lakes) is placed
--   separately by 'World.Generate.Chunk.composeFluidMap' from the
--   global 'WorldLakes' table; the caller in 'generateChunk' patches
--   the result of this function so under-lake tiles have @wt ≥
--   lake.surface@ before storing on 'LoadedChunk'.
computeWaterTable
    ∷ ClimateState
    → Int                  -- ^ worldSize
    → ChunkCoord
    → VU.Vector Int        -- ^ chunk interior terrain (chunkSize²)
    → VU.Vector Int
computeWaterTable climate worldSize coord interiorTerrain =
    let ChunkCoord cx cy = coord
        toGlobal lx ly =
            let gx = cx * chunkSize + lx
                gy = cy * chunkSize + ly
            in wrapGlobalU worldSize gx gy
    in VU.generate (chunkSize * chunkSize) $ \i →
        let lx = i `mod` chunkSize
            ly = i `div` chunkSize
            t  = interiorTerrain VU.! i
        in if t ≡ minBound
           then minBound
           else
             let (gx, gy) = toGlobal lx ly
                 c        = lookupLocalClimate climate worldSize gx gy
             in t - depthFromClimate c

-- * Subsurface query

-- | Read the water-table elevation at a chunk-local (lx, ly) tile.
--   The water table is the z-value below which the column is saturated
--   with groundwater. @z ≤ waterTableAtTile lc lx ly@ means a buried
--   tile at z is wet; revealing it (by digging) should expose water.
waterTableAtTile ∷ LoadedChunk → Int → Int → Int
waterTableAtTile lc lx ly = lcWaterTableMap lc VU.! (ly * chunkSize + lx)

-- | Is the buried position @(lx, ly, z)@ wet? Below the water table,
--   the ground is saturated; removing a tile here should expose water.
isSubsurfaceWet ∷ LoadedChunk → Int → Int → Int → Bool
isSubsurfaceWet lc lx ly z = z ≤ waterTableAtTile lc lx ly
