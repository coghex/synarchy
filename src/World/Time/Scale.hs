{-# LANGUAGE Strict, DeriveGeneric, DeriveAnyClass, DerivingStrategies #-}
-- | The accepted domain of a world time scale, and the representation
--   guards the world clock needs to stay total (#2280).
--
--   @world.setTimeScale(pageId, scale)@ takes game-minutes per
--   real-second: @0@ pauses the page clock and a positive value advances
--   it. The value the Lua boundary accepts is stored as a 'Float'
--   (@WorldSetTimeScale@, @wsTimeScaleRef@, @wsResumeScaleRef@) and is
--   later multiplied by the elapsed seconds of a tick and split into an
--   'Int' minute count by 'World.Time.Types.advanceWorldClock'. NaN, an
--   infinity, a negative value, or a finite value large enough to make
--   that split inexact all corrupt or pin that clock.
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
    , worstCaseMinuteTotal
    , worstCaseDayCount
      -- * The clock's own constants
    , clockMinutesPerDayInt
    , clockMinutesPerDay
    , clockMaxInDayMinute
    , clockMaxElapsedStep
    , clockMinutesPerDayD
    , acceptedElapsed
      -- * Retained sub-minute progress (#2471)
    , ClockRemainder
    , clockRemainderMinutes
    , zeroClockRemainder
    , maxClockRemainder
    , mkClockRemainder
    , repairClockRemainder
    , clockTickErrorBound
      -- * Representation guards
    , floorToInt
    , floorToIntD
    , floorToIntExact
    , doubleExactIntegerBound
    , addChecked
    , mulCheckedNonNeg
    , nextDownFloat
    , nextDownDouble
    ) where

import UPrelude
import Control.DeepSeq (NFData)
import GHC.Generics (Generic)
import Engine.Core.Clock (maxElapsedStep)

-- * The clock's own constants

-- | Minutes in a clock day, at the type
--   'World.Time.Types.advanceWorldClock' actually divides by since #2471:
--   the day/minute split is EXACT 'Int' arithmetic, so this is where the
--   value is stated and the floating copies below are derived from it.
--
--   Deliberately not derived from 'World.Time.Types.CalendarConfig': the
--   time-of-day arithmetic is fixed at 24×60 regardless of the
--   calendar's month and year lengths, and a ceiling that disagreed with
--   the arithmetic it guards would not be a bound at all.
clockMinutesPerDayInt ∷ Int
clockMinutesPerDayInt = 1440

-- | 'clockMinutesPerDayInt' in the stored time scale's type. 1440 is
--   exactly representable in 'Float', so the derivation is exact and the
--   two cannot drift.
clockMinutesPerDay ∷ Float
clockMinutesPerDay = fromIntegral clockMinutesPerDayInt

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

-- | 'clockMinutesPerDay' in the calendar accumulator's own type (#2471).
--   Widened from the 'Float' constant rather than restated, so the two
--   can never drift; 1440 is exactly representable in both, so the
--   widening is exact.
clockMinutesPerDayD ∷ Double
clockMinutesPerDayD = realToFrac clockMinutesPerDay

-- * Retained sub-minute progress (#2471)

-- | Game-minutes a page's calendar has already elapsed but has not yet
--   turned into a whole stored minute, ALWAYS in @[0, 1)@.
--
--   Before #2471 there was no such value: every tick added
--   @timeScale × dt@ game-minutes to an integer total and floored the
--   result straight back, so at the shipped default scale — one
--   game-minute per real second, against an elapsed step capped at
--   'clockMaxElapsedStep' — every admitted tick added at most 0.25
--   minutes and the floor discarded all of it. The calendar never moved.
--
--   __Why 'Double' and not 'Float'.__ The two inputs are 'Float's, and
--   their product is EXACT in 'Double': a 'Float' significand is 24 bits,
--   so a product needs at most 48, comfortably inside 'Double''s 53. The
--   only error a tick can introduce is therefore the rounding of the one
--   sum this value takes part in — itself plus the tick's leftover
--   FRACTION, both below one minute — which 'clockTickErrorBound'
--   bounds. The tick's whole minutes never enter that sum: they are
--   split off into exact 'Int' arithmetic first, which is what keeps the
--   bound independent of the time scale and of session length alike.
--
--   Constructed only by 'mkClockRemainder' / 'repairClockRemainder' and
--   by 'World.Time.Types.advanceWorldClock', which is what holds the
--   range invariant at every ingress.
newtype ClockRemainder = ClockRemainder Double
    deriving stock (Generic)
    deriving newtype (Show, Eq, Ord, NFData)

-- | The retained progress in game-minutes, in @[0, 1)@.
clockRemainderMinutes ∷ ClockRemainder → Double
clockRemainderMinutes (ClockRemainder r) = r

-- | No retained progress: what a fresh page, a @world.setTime@ and every
--   pre-#2471 save all start from.
zeroClockRemainder ∷ ClockRemainder
zeroClockRemainder = ClockRemainder 0

-- | The largest retained progress that exists: the 'Double' immediately
--   below one whole minute. FOUND from the representation rather than
--   written as a decimal, so it cannot drift from the range
--   'mkClockRemainder' actually accepts.
maxClockRemainder ∷ ClockRemainder
maxClockRemainder = ClockRemainder (nextDownDouble 1)

-- | The domain: finite, non-negative, and strictly below one minute.
--   'Nothing' for everything else — NaN, either infinity, a negative
--   value, and one minute or more (which is a whole minute the clock
--   should already have stored).
mkClockRemainder ∷ Double → Maybe ClockRemainder
mkClockRemainder r
  | isNaN r ∨ isInfinite r = Nothing
  | r < 0                  = Nothing
  | r ≥ 1                  = Nothing
  | otherwise              = Just (ClockRemainder r)

-- | 'mkClockRemainder' with the repair a persisted value gets: an
--   out-of-domain remainder becomes 'zeroClockRemainder' and the 'Bool'
--   says so, which is what lets a caller log the repair once. Losing at
--   most one minute of progress is never worth refusing a save over.
repairClockRemainder ∷ Double → (ClockRemainder, Bool)
repairClockRemainder r = case mkClockRemainder r of
    Just ok → (ok, False)
    Nothing → (zeroClockRemainder, True)

-- | The worst-case rounding error ONE tick's accumulation can introduce,
--   in game-minutes: half an ulp just below 2, which is the largest
--   value the FRACTIONAL accumulator can ever hold.
--
--   That ceiling of 2 is the whole point of the split arithmetic in
--   'World.Time.Types.advanceWorldClock', and it is what makes this
--   bound independent of the time scale. The tick's exact product
--   @scale × dt@ has its WHOLE-minute part taken off first and added to
--   the clock as an 'Int'; only the leftover fraction, strictly below 1,
--   is ever added to the retained remainder, itself strictly below 1. So
--   the one rounding a tick can perform happens on a sum in @[0, 2)@
--   however large the scale is — an accumulator that carried
--   @scale × dt@ whole would round at that value's own ulp, which at the
--   ceiling scale is millions of minutes.
--
--   The per-tick product is exact ('ClockRemainder' spells out why) and
--   the day/minute split is exact 'Int' arithmetic, so this rounding is
--   the entire error budget. It does not grow with session length:
--   reaching a whole minute of drift would take @1 / clockTickErrorBound@
--   ticks — over 9×10¹⁵ of them, which at 60 ticks a second is millions
--   of years of continuous play.
clockTickErrorBound ∷ Double
clockTickErrorBound = (2 - nextDownDouble 2) / 2

-- * The accepted domain

-- | The whole-minute total a worst-case NORMAL tick reaches at @scale@:
--   the last minute of a day, plus the minute its retained remainder can
--   still carry, plus a full 'clockMaxElapsedStep' at @scale@. 'Nothing'
--   when that total is not an EXACTLY split 'Int'.
--
--   This is 'World.Time.Types.advanceWorldClock''s own arithmetic, at
--   the inputs that maximise it, associated the same way: the tick's own
--   whole-minute count is taken off the exact product first, and only
--   then added to the clock. A shorter step, an earlier start or a
--   smaller retained remainder only makes the total smaller, so a scale
--   this answers 'Just' for cannot overflow on any normal tick.
--
--   __Why exactness and not merely representability (#2471).__ The
--   sub-minute remainder is only meaningful while the whole-minute split
--   of @scale × dt@ is exact — above 'doubleExactIntegerBound' a 'Double'
--   has no sub-unit precision left at all, and both the split and the
--   fraction it leaves behind become fiction. Refusing there is what
--   makes 'clockTickErrorBound' a true statement over the WHOLE accepted
--   domain rather than over the scales someone happened to test. It
--   costs nothing real: the bound still sits many orders of magnitude
--   above every shipped caller (the largest is @50000@ in
--   @tools\/farm_ai_probe.py@ and @tools\/crop_probe.py@), and a single
--   tick at the ceiling still advances the calendar by millions of
--   years.
worstCaseMinuteTotal ∷ Float → Maybe Int
worstCaseMinuteTotal scale = do
    addedWhole ← floorToIntExact
        (realToFrac scale * realToFrac clockMaxElapsedStep)
    -- The largest start: the last minute of a day, plus the one minute a
    -- remainder below 1 can carry into it.
    addChecked clockMinutesPerDayInt addedWhole

-- | The day count of 'worstCaseMinuteTotal' — the quantity the calendar
--   carry actually receives, and the one 'maxTimeScale''s documentation
--   is stated in.
worstCaseDayCount ∷ Float → Maybe Int
worstCaseDayCount scale =
    (`div` clockMinutesPerDayInt) <$> worstCaseMinuteTotal scale

-- | The largest stored scale the boundary accepts: the largest 'Float'
--   whose worst-case tick still floors to a representable 'Int' day
--   count.
--
--   FOUND, not chosen, and not left to a closed form. The algebraic
--   solution of
--
--   > scale * clockMaxElapsedStep ≡ doubleExactIntegerBound
--
--   is only a starting point: every step of it rounds, and a bound that
--   rounded UP would admit a scale that overflows — exactly the trap in
--   comparing against @fromIntegral (maxBound ∷ Int) ∷ Float@, which is
--   2^63 rather than 2^63−1. So the search walks DOWN one representable
--   'Float' at a time until 'worstCaseDayCount' — the very predicate
--   'advanceWorldClock' evaluates — actually answers 'Just'. The bound
--   therefore cannot drift from the arithmetic it guards, and it is the
--   largest safe scale rather than a convenient round number below it.
--
--   In practice the walk takes a step or two. The budget is a
--   termination guarantee, not an expectation: it bounds the search at
--   far more steps than any rounding can cost, and a downward search
--   that somehow exhausted it would refuse everything but a paused clock
--   rather than return an unproven bound.
maxTimeScale ∷ Float
maxTimeScale = search maxSearchSteps algebraicCeiling
  where
    -- Both operands are exact powers of two (0.25 exactly, and 2^53), so
    -- this quotient is exact in 'Double' AND lands on a 'Float' without
    -- rounding: the starting
    -- point is the algebraic solution ITSELF, never a value rounded
    -- below it. Walking down from there therefore reaches the largest
    -- accepted scale, and there is nothing above it to climb back to.
    algebraicCeiling ∷ Float
    algebraicCeiling =
        realToFrac (doubleExactIntegerBound / realToFrac clockMaxElapsedStep)
    search ∷ Int → Float → Float
    search budget scale
      | budget ≤ 0 ∨ scale ≤ 0 = 0
      | otherwise = case worstCaseMinuteTotal scale of
          Just _  → scale
          Nothing → search (budget - 1) (nextDownFloat scale)

-- | The search budget above. 64 is many times the handful of ulps any
--   rounding in the algebraic starting point can cost.
maxSearchSteps ∷ Int
maxSearchSteps = 64

-- | The next representable value strictly below a positive finite @x@.
--   Shared by both instantiations below so the 'Float' ceiling search
--   and the 'Double' remainder bound can never disagree about what one
--   step down means.
--
--   'decodeFloat' NORMALISES: it returns a significand in
--   @[2^(d-1), 2^d)@ for @d = floatDigits x@. Away from a binade
--   boundary, decrementing it and re-encoding at the same exponent is
--   exactly one step down. AT the bottom of a binade it is not — the
--   spacing halves below @x@, so plain @mant - 1@ steps down by a whole
--   ulp of the binade above and SKIPS the representable value in
--   between. #2471 found that the hard way: a ceiling search starting
--   at an exact power of two settled an ulp low and refused a scale the
--   clock handles perfectly well. The binade case re-encodes one
--   exponent lower instead, where the doubled significand is exact.
nextDownAt ∷ ∀ α. RealFloat α ⇒ α → α
nextDownAt x = case decodeFloat x of
    (mant, ex)
      | mant ≤ 0           → x
      | mant ≡ binadeFloor → encodeFloat (2 * mant - 1) (ex - 1)
      | otherwise          → encodeFloat (mant - 1) ex
  where
    -- The smallest NORMALISED significand of this type; a denormal's is
    -- below it, and there the uniform spacing makes @mant - 1@ right.
    binadeFloor = 2 ^ (floatDigits x - 1)

-- | 'nextDownAt' at the stored time scale's type.
nextDownFloat ∷ Float → Float
nextDownFloat = nextDownAt

-- | 'nextDownAt' at the calendar accumulator's type (#2471).
nextDownDouble ∷ Double → Double
nextDownDouble = nextDownAt


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

-- | @-2^63@ on a 64-bit 'Int', at whichever floating type is being
--   guarded. A power of two, so this narrowing is exact in both 'Float'
--   and 'Double' — which is why the guard below is stated against
--   'minBound' and its negation rather than against @maxBound@, whose
--   floating image rounds the wrong way.
intFloorLowerInclusiveAt ∷ ∀ α. RealFloat α ⇒ α
intFloorLowerInclusiveAt = fromIntegral (minBound ∷ Int)

-- | @2^63@ on a 64-bit 'Int', exclusive: the largest 'Float' below it is
--   @2^63 - 2^39@ and the largest 'Double' below it @2^63 - 2^10@, each
--   with a floor comfortably inside 'Int'.
intFloorUpperExclusiveAt ∷ ∀ α. RealFloat α ⇒ α
intFloorUpperExclusiveAt = negate intFloorLowerInclusiveAt

-- | @floor@ into 'Int', but only where the result is representable.
--   'Nothing' for NaN, either infinity, and any finite value whose floor
--   would fall outside @[minBound, maxBound]@ — the partial
--   @floor ∷ α → Int@ is never evaluated in those cases.
--
--   ONE definition for both floating types the clock uses (#2471): the
--   scale domain is judged in 'Float' and the calendar accumulates in
--   'Double', and a guard that existed only at one width would leave the
--   other's floors unprotected.
floorToIntAt ∷ ∀ α. RealFloat α ⇒ α → Maybe Int
floorToIntAt x
  | isNaN x ∨ isInfinite x       = Nothing
  | x < intFloorLowerInclusiveAt = Nothing
  | x ≥ intFloorUpperExclusiveAt = Nothing
  | otherwise                    = Just (floor x)

-- | 'floorToIntAt' at the stored time scale's type.
floorToInt ∷ Float → Maybe Int
floorToInt = floorToIntAt

-- | 'floorToIntAt' at the calendar accumulator's type (#2471).
floorToIntD ∷ Double → Maybe Int
floorToIntD = floorToIntAt

-- | @2^53@: the first 'Double' whose successor is not an integer, and
--   therefore the exclusive bound below which @floor@ and the fraction
--   it leaves behind are both EXACT. A power of two, so it is itself
--   exactly representable.
doubleExactIntegerBound ∷ Double
doubleExactIntegerBound = 2 ^^ (53 ∷ Int)

-- | 'floorToIntD', but only where the split is also exact (#2471):
--   @x@ must be below 'doubleExactIntegerBound' in magnitude, so that
--   @fromIntegral (floor x)@ reproduces the integer part without
--   rounding and @x - fromIntegral (floor x)@ is the true fraction
--   rather than a cancellation artefact.
--
--   Stricter than 'floorToIntD' and deliberately so: this is the guard
--   the retained sub-minute remainder needs, and 'maxTimeScale' is
--   derived from it, so the exactness the remainder's contract claims
--   holds at every accepted scale instead of only at small ones.
floorToIntExact ∷ Double → Maybe Int
floorToIntExact x
  | isNaN x ∨ isInfinite x        = Nothing
  | abs x ≥ doubleExactIntegerBound = Nothing
  | otherwise                     = floorToIntD x

-- | Addition that reports 'Int' overflow instead of wrapping into it.
addChecked ∷ Int → Int → Maybe Int
addChecked a b
  | b ≥ 0 ∧ a > maxBound - b = Nothing
  | b < 0 ∧ a < minBound - b = Nothing
  | otherwise                = Just (a + b)

-- | Multiplication of NON-NEGATIVE 'Int's that reports overflow instead
--   of wrapping into it. A negative operand is reported as
--   unrepresentable rather than silently handled: every caller here
--   multiplies calendar extents, which are floored at 1 or 0 first.
--
--   The calendar needs this as much as 'addChecked' does.
--   'calendarDaysPerYear' multiplies two authored 'Int' fields, and a
--   product that wraps to zero turns the carry's @divMod@ into a divide
--   by zero — which is a crash, not a clamped answer, and would break the
--   world clock's totality contract from a data file.
mulCheckedNonNeg ∷ Int → Int → Maybe Int
mulCheckedNonNeg a b
  | a < 0 ∨ b < 0        = Nothing
  | b ≡ 0                = Just 0
  | a > maxBound `div` b = Nothing
  | otherwise            = Just (a * b)
