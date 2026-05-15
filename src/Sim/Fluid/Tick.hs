{-# LANGUAGE Strict, UnicodeSyntax #-}
module Sim.Fluid.Tick
    ( simulateFluidTick
    ) where

import UPrelude
import Sim.State.Types (SimState(..))

-- | Fluid simulation is disabled. Rivers are placed statically
--   by the generation pipeline. This function is a no-op skeleton
--   kept for future overflow/interaction sim.
simulateFluidTick ∷ SimState → SimState
simulateFluidTick = id
