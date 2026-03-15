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
    , computeChunkIce
    , computeChunkLava
    , computeChunkLakes
    , computeChunkRivers
    , unionFluidMap
    , equilibrateFluidMap
    , fillCoastalGaps
    , fixupSegmentContinuity
    , sealCrossChunkRivers
      -- * Query
    , isOceanChunk
    , hasAnyLavaQuick
    , hasAnyRiverQuick
    , hasAnyLakeQuick
    , hasAnyOceanFluid
    ) where

-- Re-export from submodules
import World.Fluid.Types (FluidType(..), FluidCell(..))
import World.Fluid.Internal (unionFluidMap, equilibrateFluidMap
                            , fillCoastalGaps)
import World.Fluid.Ocean (computeOceanMap, isOceanChunk, hasAnyOceanFluid
                         , computeChunkFluid)
import World.Fluid.River (computeChunkRivers, fixupSegmentContinuity
                         , hasAnyRiverQuick, sealCrossChunkRivers)
import World.Fluid.Ice (computeChunkIce)
import World.Fluid.Lake (computeChunkLakes, hasAnyLakeQuick)
import World.Fluid.Lava (computeChunkLava, hasAnyLavaQuick)

-- These are re-exported from World.Base / World.Types but kept in the
-- export list for backwards compatibility with existing call sites.
import World.Types (OceanMap, RegionCoord(..), Region(..), ChunkCoord(..)
                   , chunkToRegion)
