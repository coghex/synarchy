{-# LANGUAGE Strict #-}
-- | Unit-scale physics constants, neutral between how a unit MOVES and
--   what that movement COSTS it.
--
--   Both halves must agree: the descent "Unit.Thread.Movement.Fall"
--   integrates and the impact energy "Unit.Fall" turns into injuries are
--   two readings of the same fall, so they read one definition of how
--   tall a z-level is and how hard gravity pulls. Before #1146 that one
--   definition lived in the injury model, which made every motion module
--   import "Unit.Fall" just to learn what gravity is; the values are
--   unchanged, only their home is.
--
--   No local dependencies — this is a Base-style leaf module, so it can
--   sit under both the motion and the injury trees without a cycle.
module Unit.Physics
    ( metresPerZ
    , gravity
    ) where

import UPrelude

-- | Metres of real height per world z-level. A z-step is roughly a
--   floor/step — human-scale — so a 2-z drop ≈ 3 m. Drives the whole
--   fall energy calc; raise it to make every fall more dangerous.
metresPerZ ∷ Float
metresPerZ = 1.5

-- | Standard gravity (m/s²).
gravity ∷ Float
gravity = 9.81
