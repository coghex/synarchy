{-# LANGUAGE Strict #-}
-- | The accepted domain of a world time scale, and the representation
--   guards the world clock needs to stay total (#2280).
--
--   @world.setTimeScale(pageId, scale)@ takes game-minutes per
--   real-second: @0@ pauses the page clock and a positive value advances
--   it. The value the Lua boundary accepts is stored as a 'Float'
--   (@WorldSetTimeScale@, @wsTimeScaleRef@, @wsResumeScaleRef@) and is
--   later multiplied by the elapsed seconds of a tick and floored into
--   an 'Int' day count by 'World.Time.Types.advanceWorldClock'. NaN, an
--   infinity, a negative value, or a finite value large enough to floor
--   outside 'Int' all corrupt or pin that clock.
--
--   This module is the ONE definition of what is accepted, shared by the
--   Lua boundary (which refuses at the door, before any world command is
--   queued) and by 'World.Time.Types.advanceWorldClock' (which refuses
--   again, so no other producer can corrupt the clock by going around
--   Lua). Neither side may hard-code a bound of its own.
--
--   __The ceiling is derived, never chosen.__ It is a
--   representation-safety bound, not a gameplay speed limit: every
--   shipped caller sits many orders of magnitude below it (the largest
--   is @50000@ in @tools\/farm_ai_probe.py@ and @tools\/crop_probe.py@).
module World.Time.Scale
    ( -- * The accepted domain
      TimeScaleRefusal(..)
    , classifyTimeScale
    , acceptedTimeScale
    , describeTimeScaleRefusal
    , maxTimeScale
    , maxClockDayCount
      -- * The clock's own constants
    , clockMinutesPerDay
    , clockMaxInDayMinute
    , clockMaxElapsedStep
    , acceptedElapsed
      -- * Representation guards
    , floorToInt
    , addChecked
    ) where

import UPrelude
import Engine.Core.Clock (maxElapsedStep)

-- * The clock's own constants

-- | Minutes in a clock day, exactly as
--   'World.Time.Types.advanceWorldClock' divides by it. Deliberately not
--   derived from 'World.Time.Types.CalendarConfig': the time-of-day
--   arithmetic is fixed at 24×60 regardless of the calendar's month and
--   year lengths, and a ceiling that disagreed with the arithmetic it
--   guards would not be a bound at all.
clockMinutesPerDay ∷ Float
clockMinutesPerDay = 1440

-- | The last minute a tick can START from — 23:59, i.e. one minute
--   before the next midnight. This is the worst case for the ceiling
--   below: any later start is a different day.
clockMaxInDayMinute ∷ Float
clockMaxInDayMinute = clockMinutesPerDay - 1

-- | 'Engine.Core.Clock.maxElapsedStep' in the clock's own storage type.
--   0.25 is exactly representable in both 'Double' and 'Float', so this
--   narrowing is exact — which is what lets the ceiling below be derived
--   rather than approximated.
clockMaxElapsedStep ∷ Float
clockMaxElapsedStep = realToFrac maxElapsedStep

-- * The accepted domain

-- | The largest day count one tick is allowed to produce.
--
--   NOT @maxBound ∷ Int@, and deliberately not compared against
--   @fromIntegral (maxBound ∷ Int) ∷ Float@: 2^63−1 is not representable
--   in 'Float' and rounds UP to 2^63, so a scale passing such a
--   comparison could still floor past 'Int'. @maxBound \`div\` 2 + 1@ is
--   the next power of two down (2^62 on a 64-bit 'Int'), which is both
--   exactly representable in 'Float' and provably ≤ @maxBound@.
maxClockDayCount ∷ Int
maxClockDayCount = maxBound `div` 2 + 1

-- | The largest stored scale the boundary accepts.
--
--   Derived from the constants above by inverting the clock's own
--   arithmetic: a worst-case normal tick starts at 'clockMaxInDayMinute'
--   and runs for a full 'clockMaxElapsedStep', so the day count it
--   produces is
--
--   > (clockMaxInDayMinute + scale * clockMaxElapsedStep) / clockMinutesPerDay
--
--   and this is the @scale@ at which that equals 'maxClockDayCount'.
--   Every intermediate is exact in 'Float' (1440 = 45·2^5 and 0.25 =
--   2^-2 are exact, and subtracting 1439 falls below the ulp at that
--   magnitude), so the accepted maximum floors to exactly
--   'maxClockDayCount' rather than to something one ulp past it.
--
--   'World.Time.Types.advanceWorldClock' still guards each 'floor'
--   independently via 'floorToInt': the ceiling is the contract, that
--   guard is what makes the function total regardless.
maxTimeScale ∷ Float
maxTimeScale =
    (fromIntegral maxClockDayCount * clockMinutesPerDay - clockMaxInDayMinute)
        / clockMaxElapsedStep

-- | Why a time scale was refused. Each maps to one diagnostic in
--   'describeTimeScaleRefusal'.
data TimeScaleRefusal
  = ScaleNotANumber     -- ^ @NaN@: floors unpredictably and pins the clock.
  | ScaleInfinite       -- ^ Either infinity, including one a finite Lua
                        --   number narrowed to on the way into 'Float'.
  | ScaleNegative       -- ^ Runs the time of day backwards while the
                        --   date cannot roll back with it.
  | ScaleAboveCeiling   -- ^ Finite, but past 'maxTimeScale'.
  deriving (Eq, Show)

-- | The whole policy, in one total function over 'Float'.
--
--   Both signed zeros are accepted and normalised to a positive zero:
--   @-0.0 ≡ 0@ holds, and the stored scale is a paused clock either way.
classifyTimeScale ∷ Float → Either TimeScaleRefusal Float
classifyTimeScale s
  | isNaN s          = Left ScaleNotANumber
  | isInfinite s     = Left ScaleInfinite
  | s ≡ 0            = Right 0
  | s < 0            = Left ScaleNegative
  | s > maxTimeScale = Left ScaleAboveCeiling
  | otherwise        = Right s

-- | 'classifyTimeScale' as a predicate, for the clock's own guard.
acceptedTimeScale ∷ Float → Bool
acceptedTimeScale s = case classifyTimeScale s of
    Left _  → False
    Right _ → True

-- | The refusal diagnostic. Always names the rejected value or its
--   numeric category, because it is the only signal a refused call
--   produces — the verb returns @false@ plus this text and never raises.
describeTimeScaleRefusal ∷ TimeScaleRefusal → Float → Text
describeTimeScaleRefusal refusal scale = reason <> " " <> policy
  where
    policy = "The time scale must be a finite number in [0, "
             <> tshow maxTimeScale <> "] game-minutes per real-second; \
             \the existing time scale is left unchanged."
    reason = case refusal of
      ScaleNotANumber → prefix
          <> " is not a number; a NaN scale floors unpredictably and \
             \freezes the world clock."
      ScaleInfinite → prefix
          <> " is infinite; an infinite scale pins the world clock, and \
             \a finite Lua number this large becomes infinite in the \
             \clock's Float storage."
      ScaleNegative → prefix
          <> " is negative; a negative scale runs the time of day \
             \backwards while the calendar date cannot follow it."
      ScaleAboveCeiling → prefix
          <> " is above the largest scale whose worst-case tick still \
             \produces a representable day count."
    prefix = "time scale " <> tshow scale

-- | The elapsed-seconds domain 'World.Time.Types.advanceWorldClock'
--   accepts.
--
--   Finite and non-negative — NOT capped at 'clockMaxElapsedStep'. The
--   cap belongs to the producer ('Engine.Core.Clock.sanitiseElapsed'
--   applies it before the world tick ever calls the clock); enforcing it
--   here would reject the multi-day carries the clock's own contract
--   pins. Over-cap elapsed values stay safe through 'floorToInt', which
--   guards the quantities each 'floor' actually receives — strictly what
--   representability needs, and strictly more than a @dt@ cap could
--   give, since a modest scale over a long @dt@ is perfectly
--   representable while the ceiling scale over an over-cap @dt@ is not.
acceptedElapsed ∷ Float → Bool
acceptedElapsed dt = not (isNaN dt) ∧ not (isInfinite dt) ∧ dt ≥ 0

-- * Representation guards

-- | @-2^63@ on a 64-bit 'Int'. A power of two, so this narrowing is
--   exact — which is why the guard below is stated against 'minBound'
--   and its negation rather than against @maxBound@, whose 'Float'
--   image rounds the wrong way.
intFloorLowerInclusive ∷ Float
intFloorLowerInclusive = fromIntegral (minBound ∷ Int)

-- | @2^63@ on a 64-bit 'Int', exclusive: the largest 'Float' below it is
--   @2^63 - 2^39@, whose floor is comfortably inside 'Int'.
intFloorUpperExclusive ∷ Float
intFloorUpperExclusive = negate intFloorLowerInclusive

-- | @floor@ into 'Int', but only where the result is representable.
--   'Nothing' for NaN, either infinity, and any finite value whose floor
--   would fall outside @[minBound, maxBound]@ — the partial
--   @floor ∷ Float → Int@ is never evaluated in those cases.
floorToInt ∷ Float → Maybe Int
floorToInt x
  | isNaN x ∨ isInfinite x    = Nothing
  | x < intFloorLowerInclusive = Nothing
  | x ≥ intFloorUpperExclusive = Nothing
  | otherwise                  = Just (floor x)

-- | Addition that reports 'Int' overflow instead of wrapping into it.
addChecked ∷ Int → Int → Maybe Int
addChecked a b
  | b ≥ 0 ∧ a > maxBound - b = Nothing
  | b < 0 ∧ a < minBound - b = Nothing
  | otherwise                = Just (a + b)
