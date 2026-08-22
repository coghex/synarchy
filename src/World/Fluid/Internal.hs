{-# LANGUAGE Strict #-}
module World.Fluid.Internal
    ( FluidMap
    , wrapChunkCoordU
    , floorDiv'
    ) where

import UPrelude
import qualified Data.Vector as V
import World.Fluid.Types (FluidCell(..))
import World.Chunk.Types (wrapChunkCoordU)

type FluidMap = V.Vector (Maybe FluidCell)

-- * Chunk coord wrapping
--
-- 'wrapChunkCoordU' is the canonical seam wrap, defined in
-- "World.Chunk.Types" and re-exported here so the fluid / ocean / seabed /
-- lake / magma / zoommap paths share one source of truth with the slope /
-- chunk-loading path (see "World.Slope"). See issue #316.

-- * Misc helpers

floorDiv' ∷ Int → Int → Int
floorDiv' a b = floor (fromIntegral a / fromIntegral b ∷ Double)
