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
      -- * Query
    , isOceanChunk
    , hasAnyOceanFluid
    ) where

import World.Fluid.Types (FluidType(..), FluidCell(..))
import World.Fluid.Ocean (computeOceanMap, isOceanChunk, hasAnyOceanFluid)
import World.Types (OceanMap, RegionCoord(..), Region(..), chunkToRegion)
