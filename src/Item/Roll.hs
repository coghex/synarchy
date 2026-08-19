{-# LANGUAGE Strict #-}
module Item.Roll
    ( rollItemSpec
    , rollItemWeight
      -- * Ground salvage (#1421)
    , groundQualityFallbackRange
    , groundConditionBaseRange
    , groundConditionPenaltyRange
    , salvageCondition
    , rollGroundQuality
    , rollGroundCondition
    ) where

import UPrelude
import Data.IORef (IORef, atomicModifyIORef')
import System.Random (StdGen, randomR)
import Item.Types (ItemDef(..))
import Unit.Stats (rollStat)

-- | Sample a value from an item def's (min, max) quality roll spec.
--   Returns 100.0 when the spec is Nothing — items that don't declare a
--   spec spawn at full quality.
--
--   Condition no longer has a spec of its own (#1421): it is runtime
--   wear state, so every freshly made item starts at 100 and only the
--   ground-salvage path below starts one lower.
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

-- * Ground salvage (#1421)
--
-- @item.spawnGround@ is the SALVAGE path: an item found lying in the
-- world is pre-owned, not new. That is a property of how the item came
-- to exist, so these three ranges are fixed here for that one creation
-- site and are deliberately not authored per definition.

-- | Quality a ground-spawned item lands in when its definition declares
--   no @quality:@ spec of its own — loot and location content used to
--   spawn uniformly perfect.
groundQualityFallbackRange ∷ (Float, Float)
groundQualityFallbackRange = (20, 80)

-- | The condition a ground-spawned item STARTED at, before the world
--   wore it. Drawn uniformly.
groundConditionBaseRange ∷ (Float, Float)
groundConditionBaseRange = (80, 100)

-- | The wear the world put on it since. Drawn uniformly, independently
--   of the base, and never suppressed by an explicit caller condition.
groundConditionPenaltyRange ∷ (Float, Float)
groundConditionPenaltyRange = (0, 20)

-- | Combine the salvage path's TWO independent draws: the condition the
--   item started at, minus the wear it has taken, clamped to [0, 100].
--
--   Kept pure and separate from the draws so the arithmetic can be
--   checked against known draws rather than against the resulting
--   range. That distinction is load-bearing: @rand(80,100) −
--   rand(0,20)@ is the difference of two uniform draws of equal width,
--   so it is TRIANGULAR — peaking at 80, thinning toward 60 and 100 —
--   while a single @rand(60,100)@ has the identical bounds AND the
--   identical mean of 80 and is flat. Neither a range check nor an
--   average can tell them apart.
salvageCondition ∷ Float → Float → Float
salvageCondition base penalty = max 0 (min 100 (base - penalty))

-- | Draw uniformly from a (min, max) range.
rollUniformRange ∷ (Float, Float) → IORef StdGen → IO Float
rollUniformRange (mn, mx) rngRef =
    atomicModifyIORef' rngRef $ \g →
        let (v, g') = randomR (mn, mx) g in (g', v)

-- | Ground-spawn quality: an explicit caller prop REPLACES the roll —
--   a caller naming a quality is naming the item's workmanship
--   outright — else the definition's own spec, else
--   'groundQualityFallbackRange'. Both rolled forms keep
--   'rollItemSpec'\'s truncated-normal semantics.
--
--   The explicit case consumes NO draw: the stat RNG is shared with
--   other gameplay consumers, so silently spending and discarding one
--   would shift every later roll.
rollGroundQuality ∷ ItemDef → Maybe Float → IORef StdGen → IO Float
rollGroundQuality _   (Just q) _      = return q
rollGroundQuality def Nothing  rngRef =
    rollItemSpec (Just (fromMaybe groundQualityFallbackRange
                                  (idQualitySpec def)))
                 rngRef

-- | Ground-spawn condition: @base − penalty@, clamped. @base@ is the
--   caller's explicit @condition@ prop when one is given and a
--   'groundConditionBaseRange' draw otherwise — but the penalty is
--   drawn either way, because a caller naming a condition is naming
--   what the item STARTED as, not what it is now. There is no way to
--   request or guarantee pristine condition by bypassing the penalty.
rollGroundCondition ∷ Maybe Float → IORef StdGen → IO Float
rollGroundCondition mBase rngRef = do
    base ← case mBase of
        Just b  → return b
        Nothing → rollUniformRange groundConditionBaseRange rngRef
    penalty ← rollUniformRange groundConditionPenaltyRange rngRef
    return (salvageCondition base penalty)
