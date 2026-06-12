{-# LANGUAGE Strict, UnicodeSyntax #-}
-- | Pure stat-rolling primitives.
--
-- 'rollStat' draws from a truncated normal distribution with mean
-- = @base@ and sigma = @range@ / 4, rejecting samples outside the
-- window [base - range/2, base + range/2]. A final clamp keeps
-- results at or above zero — strength below zero is meaningless
-- (\"too weak to breathe\"); the upper bound is intentionally soft
-- so creatures like dragons (base ≫ 100) work the same way.
--
-- Range = 0 returns @base@ unchanged (no roll, no RNG consumption).
module Unit.Stats
    ( rollStat
    , boxMuller
    , effectiveStat
    , applySkillXP
    ) where

import UPrelude
import System.Random (StdGen, randomR)
import Unit.Types (StatModifier(..))

-- | One standard-normal sample via Box-Muller. Returns (z, newGen).
--   Only consumes two uniform draws; the second normal sample of the
--   Box-Muller pair is discarded for simplicity.
boxMuller ∷ StdGen → (Float, StdGen)
boxMuller g0 =
    let (u1, g1) = randomR (0 ∷ Float, 1) g0
        (u2, g2) = randomR (0 ∷ Float, 1) g1
        -- Guard against u1 = 0 producing -Infinity from log.
        r        = sqrt (-2 * log (max u1 1e-9))
        z        = r * cos (2 * pi * u2)
    in (z, g2)

-- | Roll one stat. With range = 0 returns base exactly (no RNG draw).
--   Otherwise: truncated normal (sigma = range/4) on
--   [base - range/2, base + range/2]. Values outside the window are
--   clamped to the nearest bound (very rare with sigma = range/4 —
--   ~5% of raw draws). Final clamp to >= 0.
-- | Apply XP to a skill and return the new skill level.
--
--   Formula: @newLevel = level + xp / max (level * level, 1e-4)@.
--
--   This makes growth scale quadratically with the current level:
--   gaining 0.5 XP at level 1 jumps by 0.5; at level 2 by 0.125; at
--   level 3 by ~0.056. The @max ... 1e-4@ guard prevents division by
--   zero if a skill is somehow at 0 (won't happen via normal roll,
--   but possible after @setSkill 0@) — at level 0 the gain rockets
--   to xp*10000, which is fine because real callers don't pump XP
--   into a zeroed skill.
applySkillXP ∷ Float → Float → Float
applySkillXP level xp = level + xp / max (level * level) 1e-4

-- | Effective stat value at time @now@ =
--   @(base + Σ active deltas) × (1 + Σ active percents)@, clamped at
--   zero. A modifier is active when it has no expiry, OR its expiry
--   is in the future (strictly: @now < expiry@, so the modifier ends
--   exactly at its expiry time).
--
--   Pure — caller supplies @now@ so the function is testable without IO.
effectiveStat ∷ Double → Float → [StatModifier] → Float
effectiveStat now base mods =
    let active  = filter isActive mods
        delta   = sum (smDelta   <$> active)
        percent = sum (smPercent <$> active)
    in max 0 ((base + delta) * (1 + percent))
  where
    isActive m = case smExpiry m of
        Nothing → True
        Just t  → now < t

{-# NOINLINE rollStat #-}
rollStat ∷ Float → Float → StdGen → (Float, StdGen)
rollStat base range g
    | range ≤ 0 = (max 0 base, g)
    | otherwise =
        case boxMuller g of
            (z, g') →
                ( max 0
                    (max (base - range / 2)
                        (min (base + range / 2)
                            (base + z * (range / 4))))
                , g'
                )
