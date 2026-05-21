{-# LANGUAGE Strict, UnicodeSyntax #-}
module Item.Roll
    ( rollItemSpec
    ) where

import UPrelude
import Data.IORef (IORef, atomicModifyIORef')
import System.Random (StdGen)
import Unit.Stats (rollStat)

-- | Sample a value from an item def's (min, max) roll spec. Returns
--   100.0 when the spec is Nothing — items that don't declare a spec
--   spawn at full quality / condition.
--
--   Shape: normal distribution centred at (min+max)/2 with sigma
--   (max-min)/4, clamped to [min, max]. Same distribution shape as
--   `Unit.Stats.rollStat` (which clamps to base ± range/2 = the
--   midpoint ± half-range).
rollItemSpec ∷ Maybe (Float, Float) → IORef StdGen → IO Float
rollItemSpec Nothing _ = return 100.0
rollItemSpec (Just (mn, mx)) rngRef =
    atomicModifyIORef' rngRef $ \g →
        let base    = (mn + mx) / 2
            range   = mx - mn
            (v, g') = rollStat base range g
        in (g', v)
