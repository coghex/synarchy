{-# LANGUAGE Strict, UnicodeSyntax #-}
module Item.Roll
    ( rollItemSpec
    , rollItemWeight
    ) where

import UPrelude
import Data.IORef (IORef, atomicModifyIORef')
import System.Random (StdGen)
import Item.Types (ItemDef(..))
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

-- | This instance's empty weight: truncated normal around the def's
--   (mean, range) spec when one exists (raw gems vary per find),
--   else exactly idWeight. Floor at 0.001 kg — a zero-weight item
--   breaks nothing but reads as a bug in the inventory UI.
rollItemWeight ∷ ItemDef → IORef StdGen → IO Float
rollItemWeight def rngRef = case idWeightSpec def of
    Nothing → return (idWeight def)
    Just (mean, range) →
        atomicModifyIORef' rngRef $ \g →
            let (v, g') = rollStat mean range g
            in (g', max 0.001 v)
