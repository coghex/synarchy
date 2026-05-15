{-# LANGUAGE Strict, UnicodeSyntax #-}
module Sim.River.Project
    ( projectRiverSimple
    ) where

import UPrelude
import qualified Data.Vector.Unboxed as VU
import World.Fluid.Internal (FluidMap)
import World.River.Types (RiverMask)

-- | No-op: river fluid placement is disabled.
--   Returns the existing fluid map unchanged.
projectRiverSimple ∷ RiverMask
                   → VU.Vector Int
                   → FluidMap
                   → FluidMap
projectRiverSimple _mask _terrainMap existing = existing
