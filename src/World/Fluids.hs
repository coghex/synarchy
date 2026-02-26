{-# LANGUAGE Strict, UnicodeSyntax #-}
module World.Fluids
    ( -- * Types
      FluidType(..)
    , FluidCell(..)
    , OceanMap
      -- * Region
    , RegionCoord(..)
    , Region(..)
    , chunkToRegion
      -- * Ocean flood fill
    , computeOceanMap
      -- * Chunk-level fluid
    , computeChunkFluid
    , computeChunkLava
    , computeChunkLakes
    , computeChunkRivers
    , unionFluidMap
    , fixupSegmentContinuity
      -- * Query
    , isOceanChunk
    , hasAnyLavaQuick
    , hasAnyOceanFluid
    ) where

-- Re-export from submodules
import World.Fluid.Types (FluidType(..), FluidCell(..))
import World.Fluid.Internal (unionFluidMap)
import World.Fluid.Ocean (computeOceanMap, isOceanChunk, hasAnyOceanFluid
                         , computeChunkFluid)
import World.Fluid.River (computeChunkRivers, fixupSegmentContinuity)
import World.Fluid.Lake (computeChunkLakes)
import World.Fluid.Lava (computeChunkLava, hasAnyLavaQuick)

-- These are re-exported from World.Base / World.Types but kept in the
-- export list for backwards compatibility with existing call sites.
import World.Types (OceanMap, RegionCoord(..), Region(..), ChunkCoord(..)
                   , chunkToRegion)
