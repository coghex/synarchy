{-# LANGUAGE Strict, DeriveGeneric, DeriveAnyClass #-}
module World.Time.Types
    ( module World.Time.Scale
    , WorldTime(..)
    , defaultWorldTime
    , PreciseWorldTime(..)
    , preciseWorldTime
    , defaultPreciseWorldTime
    , worldTimeToSunAngle
    , preciseSunAngle
    , advanceWorldClock
    , WorldDate(..)
    , worldDateAddDaysChecked
    , calendarDaysPerYearChecked
    , worldDateToDayOfYearChecked
    , defaultWorldDate
    , canonicalWorldDate
    , renderWorldDate
    , worldDateToDayOfYear
    , worldDateAddDays
    , worldAbsoluteDay
    , calendarDaysPerYear
    , CalendarConfig(..)
    , defaultCalendarConfig
    , SunConfig(..)
    , defaultSunConfig
    , MoonConfig(..)
    , defaultMoonConfig
    ) where

import UPrelude
import Control.DeepSeq (NFData)
import GHC.Generics (Generic)
import Data.Serialize (Serialize)
import World.Time.Scale

-- | Time of day in the world.
--   hour: 0-23, minute: 0-59
--   sunAngle is derived: 0.0 = midnight, 0.25 = 6am (dawn),
--                         0.5 = noon, 0.75 = 6pm (dusk)
data WorldTime = WorldTime
    { wtHour   ∷ !Int   -- ^ 0-23
    , wtMinute ∷ !Int   -- ^ 0-59
    } deriving (Show, Eq)

defaultWorldTime ∷ WorldTime
defaultWorldTime = WorldTime
    { wtHour   = 10     -- start at 10:00am (pleasant morning light)
    , wtMinute = 0
    }

-- | Convert world time to sun angle (0.0 .. 1.0)
--   Mapping: midnight (0:00) = 0.0, 6am = 0.25, noon = 0.5, 6pm = 0.75
--
--   Whole minutes only. 'preciseSunAngle' is what every LIVE page
--   uses (#2471); this remains the answer for a 'WorldTime' with no
--   retained progress beside it, and the two agree exactly there.
worldTimeToSunAngle ∷ WorldTime → Float
worldTimeToSunAngle (WorldTime h m) =
    let totalMinutes = fromIntegral h * 60.0 + fromIntegral m ∷ Float
    in totalMinutes / 1440.0   -- 1440 = 24 * 60

-- | A page's time of day as ONE value: the whole minutes every reader
--   has always seen, plus the sub-minute progress beside them (#2471).
--
--   The two are a single record rather than two refs because they are
--   published together and read together. A live page's clock is written
--   by the world thread and read by others — 'Unit.LineOfSight' on the
--   unit thread, "Engine.Scripting.Lua.API.Power" on the Lua thread —
--   and while they were separate a reader landing between the two writes
--   of a minute carry could pair the NEW minute with the OLD remainder.
--   That is a clock no tick ever produced, and it would run the sun
--   angle backwards. One 'IORef' holding both makes every published
--   state a whole one, structurally: there is no way to write half a
--   clock because there is no setter for half a clock.
data PreciseWorldTime = PreciseWorldTime
    { pwtTime      ∷ !WorldTime
    , pwtRemainder ∷ !ClockRemainder
      -- ^ Game-minutes already elapsed that 'pwtTime' cannot store,
      --   always in @[0, 1)@ — see 'World.Time.Scale.ClockRemainder'.
    } deriving (Show, Eq)

-- | A clock at a whole minute, carrying no sub-minute progress: what a
--   @world.setTime@, a fresh page and every pre-#2471 save all produce.
preciseWorldTime ∷ WorldTime → PreciseWorldTime
preciseWorldTime t = PreciseWorldTime t zeroClockRemainder

defaultPreciseWorldTime ∷ PreciseWorldTime
defaultPreciseWorldTime = preciseWorldTime defaultWorldTime

-- | The sun angle of a live page's clock: whole minutes PLUS the
--   sub-minute progress that clock is carrying (#2471).
--
--   The clock stores whole minutes, so before #2471 the angle could only
--   step once a minute — and at the default scale it never stepped at
--   all. Reading the retained remainder here makes the angle advance
--   smoothly with the tick that produced it, which is what every solar
--   consumer (rendering, line of sight, power generation) actually wants
--   from a continuously moving sun.
--
--   Contract, in the terms the acceptance states: NONDECREASING within a
--   day (the remainder only ever grows between whole-minute steps, and a
--   whole-minute step carries exactly the minute the remainder gave up),
--   and EQUAL to 'worldTimeToSunAngle' whenever the remainder is zero.
--   Midnight still wraps to 0, exactly as the whole-minute angle does.
--
--   It takes the WHOLE clock, never a minute and a remainder separately:
--   that is what stops a caller from pairing halves of two different
--   published states and getting an angle no tick ever produced.
preciseSunAngle ∷ PreciseWorldTime → Float
preciseSunAngle (PreciseWorldTime (WorldTime h m) remainder) =
    realToFrac (preciseMinutes / clockMinutesPerDayD)
  where
    preciseMinutes = fromIntegral h * 60 + fromIntegral m
                   + clockRemainderMinutes remainder ∷ Double

-- | Advance the full world clock — time of day AND calendar date (#332),
--   retaining sub-minute progress between ticks (#2471).
--
--   The predecessor wrapped at midnight without carrying the day, which
--   left the world date frozen forever (the flora annual cycle selected
--   by day-of-year could never move). This is the same minute arithmetic
--   plus the carry: the number of midnights crossed rolls the date
--   through the calendar. Returns the days rolled so the caller can
--   invalidate date-dependent caches (flora textures) only when the day
--   actually changed. A single tick can cross several midnights at high
--   time scales.
--
--   __Sub-minute progress is retained, not discarded (#2471).__ The
--   stored clock is whole minutes, and every tick used to FLOOR
--   @timeScale × dt@ straight back into it. At the shipped default scale
--   — one game-minute per real second, against an elapsed step capped at
--   'World.Time.Scale.clockMaxElapsedStep' — no admitted tick ever added
--   a whole minute, so the calendar never moved at all; at higher scales
--   the same elapsed time advanced the clock by different amounts
--   depending on how the worker happened to partition it. The remainder
--   threaded through here is that lost fraction, carried into the next
--   tick.
--
--   Equal admitted elapsed time therefore advances the calendar by the
--   same duration however it is partitioned, to within
--   'World.Time.Scale.clockTickErrorBound' per tick — and that bound is
--   independent of the time scale. The per-tick product is EXACT in
--   'Double' ('World.Time.Scale.ClockRemainder' spells out why); its
--   WHOLE-minute part is split off and carried in exact 'Int'
--   arithmetic; and only the leftover fraction, below one minute, is
--   ever added to the retained remainder, itself below one minute. So
--   the single rounding a tick performs is on a sum in @[0, 2)@ whatever
--   the scale, rather than at the ulp of a multi-day total.
--
--   That split is a correctness requirement, not an optimisation.
--   Recombining the stored minutes with the remainder before flooring
--   would round @1439 + nextDownDouble 1@ to 1440, so a page at 23:59
--   holding 'World.Time.Scale.maxClockRemainder' would cross midnight
--   and roll its date on a PAUSED tick that advanced it by nothing.
--
--   __Total over every input (#2280).__ @world.setTimeScale@ refuses a
--   scale outside 'World.Time.Scale.classifyTimeScale''s domain at the
--   Lua door, but this function enforces the same domain again so a
--   producer that goes around that door cannot corrupt the clock. It
--   also guards each 'floor' it evaluates and the calendar carry that
--   follows, because a scale inside the domain can still be handed an
--   elapsed step no normal tick would produce.
--
--   For an unacceptable scale, an unacceptable elapsed step, a whole
--   minute count this tick cannot represent as an 'Int', a STORED clock
--   whose own minute total will not fit one (nothing range-checks the
--   hour and minute a save carries — see
--   'World.Time.Scale.clockStartMinutes'), a minute total that will not
--   fit, or a calendar carry that would overflow 'wdYear', the answer is
--   the EXACT input
--   time, remainder and date with zero rolled days — never a partially
--   applied advance, and never a remainder the refused tick moved.
--   Every accepted input keeps the behaviour it already had at whole
--   minutes, and the returned clock's 'pwtTime' always satisfies
--   @0 ≤ wtHour ≤ 23@ and @0 ≤ wtMinute ≤ 59@ while its 'pwtRemainder'
--   always satisfies its own @[0, 1)@ range. The two come back as ONE
--   'PreciseWorldTime' so a caller cannot publish half an advance.
advanceWorldClock ∷ CalendarConfig → Float → Float
                  → PreciseWorldTime → WorldDate
                  → (PreciseWorldTime, WorldDate, Int)
advanceWorldClock cc timeScale dtSeconds clock date
    | not (acceptedTimeScale timeScale) = unchanged
    | not (acceptedElapsed dtSeconds)   = unchanged
    | otherwise = case floorToIntD added of
        Nothing         → unchanged
        Just addedWhole → withWholeMinutes addedWhole
  where
    PreciseWorldTime (WorldTime h m) remainder = clock
    unchanged = (clock, date, 0)

    -- Both factors are 'Float', so this product is EXACT in 'Double'
    -- (24 + 24 significand bits, against 53 available). Widening BEFORE
    -- multiplying is what makes that true: multiplying in 'Float' and
    -- widening afterwards would round first and retain the rounded value
    -- forever.
    added = realToFrac timeScale * realToFrac dtSeconds ∷ Double

    -- The whole-minute part of this tick leaves the floating world here
    -- and never comes back: everything downstream of @addedWhole@ is
    -- exact 'Int' arithmetic. Only the leftover FRACTION meets the
    -- retained remainder, so the one rounding a tick can perform happens
    -- on a sum in [0, 2) whatever the time scale — which is what
    -- 'World.Time.Scale.clockTickErrorBound' bounds.
    --
    -- The split is EXACT at every value this guard admits, and needs no
    -- cutoff of its own to be: @added@ is the exact product of two
    -- 'Float's, so it carries at most 48 significant bits, and a 48-bit
    -- value at or above 2^53 is necessarily an integer already. See
    -- 'World.Time.Scale.worstCaseMinuteTotal', which 'maxTimeScale' is
    -- derived from, for the whole argument.
    --
    -- Recombining them instead — adding the remainder back onto the
    -- whole minute count and flooring the total — is exactly what this
    -- must not do. @1439 + nextDownDouble 1@ is 1440 in 'Double', so a
    -- page sitting at 23:59 with the largest representable remainder
    -- would roll the date on a tick that advanced it by nothing at all.
    -- The stored clock's own minute total is CHECKED, not computed
    -- inline. @h@ and @m@ are whatever a save carried: nothing
    -- range-checks 'World.Save.Types.wpsTimeHour' or @wpsTimeMinute@ on
    -- the way in, so @h@ really can be 'maxBound', and a bare
    -- @h * 60 + m@ would wrap to a small negative and hand this
    -- function a clock it then "advances" — returning 23:00 and a
    -- rolled-back day for a paused tick that must return the input
    -- untouched.
    withWholeMinutes addedWhole = case clockStartMinutes h m of
        Nothing    → unchanged
        Just start → case addChecked start addedWhole of
            Nothing → unchanged
            Just minutesBeforeCarry →
                case addChecked minutesBeforeCarry minuteCarry of
                    Nothing           → unchanged
                    Just totalMinutes → roll totalMinutes
      where
        -- Exact: for @n = floor x@ the real value @x - n@ lies in [0, 1)
        -- and is a multiple of @ulp x@, so it is representable. Above
        -- 2^53 an @added@ is already an integer and this is exactly 0,
        -- which is the honest answer — such a value has no sub-minute
        -- precision left to carry.
        addedFraction = added - fromIntegral addedWhole
        -- In [0, 2): a remainder below 1 plus a fraction below 1.
        carried       = clockRemainderMinutes remainder + addedFraction
        minuteCarry   = if carried ≥ 1 then 1 else 0 ∷ Int
        -- Exact again, by the same argument, and in [0, 1). The
        -- constructor is what ESTABLISHES that range rather than
        -- assuming it: 'ClockRemainder' has no other way in, so the
        -- invariant holds by construction at this ingress as it does at
        -- the persisted one. With the split above the repair is
        -- unreachable, and the spec pins the range over pathological
        -- inputs rather than trusting the derivation.
        remainder'    = fst (repairClockRemainder
                            (carried - fromIntegral minuteCarry))

        roll totalMinutes =
            let (daysRolled, wrapped) =
                    totalMinutes `divMod` clockMinutesPerDayInt
                clock' = PreciseWorldTime
                    (WorldTime (wrapped `div` clockMinutesPerHourInt)
                               (wrapped `mod` clockMinutesPerHourInt))
                    remainder'
            in if daysRolled > 0
                then case worldDateAddDaysChecked cc daysRolled date of
                    Nothing    → unchanged
                    Just date' → (clock', date', daysRolled)
                else (clock', date, daysRolled)

-- | World date (placeholder for seasons).
--   Currently unused for sun angle calculation.
--
--   Calendar contract: the simplified world calendar gives every month
--   the same length ('ccDaysPerMonth'), so a year has
--   @ccDaysPerMonth * ccMonthsPerYear@ days. The fields below are
--   /calendar/ components, NOT a year-relative day:
--
--     * 'wdMonth' is the month-of-year (@1 .. ccMonthsPerYear@).
--     * 'wdDay'   is the day-of-/month/ (@1 .. ccDaysPerMonth@).
--
--   Those ranges are the type's contract and 'canonicalWorldDate' is
--   what holds every ingress to them (#2339); a value that reaches this
--   record from outside is put in that form before it is stored.
--
--   Anything that needs a year-relative \"ordinal day\" (e.g. flora
--   annual-cycle stage selection) must convert through
--   'worldDateToDayOfYear' — passing 'wdDay' directly aliases
--   day-of-month with day-of-year and can never reach stages past the
--   first month.
data WorldDate = WorldDate
    { wdYear  ∷ !Int
    , wdMonth ∷ !Int   -- ^ month-of-year, 1 .. ccMonthsPerYear
    , wdDay   ∷ !Int   -- ^ day-of-month, 1 .. ccDaysPerMonth
    } deriving (Show, Eq)

defaultWorldDate ∷ WorldDate
defaultWorldDate = WorldDate
    { wdYear  = 1
    , wdMonth = 1
    , wdDay   = 1
    }

-- | The one canonical form of a 'WorldDate' under a calendar (#2339):
--   @wdYear ≥ 1@, @1 ≤ wdMonth ≤ ccMonthsPerYear@ and
--   @1 ≤ wdDay ≤ ccDaysPerMonth@, each out-of-range component clamped
--   into its range.
--
--   __One definition, three callers.__ Every ingress that can put a
--   noncanonical date into a live 'World.State.Types.wsDateRef' passes
--   through this: the @world.setDate@ handler
--   ('World.Thread.Command.Time.handleWorldSetDateCommand') and load
--   staging ('World.Load.Stage'). The two ordinal converters below read
--   it too, so the bounds a stored date is repaired to and the bounds a
--   derived reading is computed from cannot drift apart — which is the
--   disagreement #2339 is about: @world.getDate@ used to report a raw
--   @month = 14@ beside a @dayOfYear@ computed for month 12.
--
--   __Why the ceilings are floored at 1.__ 'CalendarConfig' arrives from
--   world-gen data rather than a validated range
--   ('World.Generate.Config.Validate' does not bound either field), so
--   @ccMonthsPerYear@ or @ccDaysPerMonth@ can be zero or negative. Both
--   ordinal converters already answer that with @max 1@, and the
--   canonical form must agree with them: a ceiling below 1 would
--   otherwise make the clamp's own lower and upper bounds cross, and no
--   date could satisfy the form at all.
--
--   'wdYear' has no calendar-derived ceiling — a world can run for as
--   many years as an 'Int' holds — so only its floor is enforced. Years
--   below 1 predate the world epoch 'defaultWorldDate' names, which
--   'worldAbsoluteDay' already clamps away at 0.
canonicalWorldDate ∷ CalendarConfig → WorldDate → WorldDate
canonicalWorldDate cc (WorldDate year month day) = WorldDate
    { wdYear  = max 1 year
    , wdMonth = max 1 (min (max 1 (ccMonthsPerYear cc)) month)
    , wdDay   = max 1 (min (max 1 (ccDaysPerMonth cc)) day)
    }

-- | A date as the two clamp diagnostics name it: @year-month-day@.
--   One spelling so the @world.setDate@ warning
--   ('World.Thread.Command.Time.setDateClampWarning') and the load-staging
--   warning ('World.Load.Stage.stagedWorldDateWarning') read alike (#2339).
renderWorldDate ∷ WorldDate → Text
renderWorldDate (WorldDate y mo d) =
    tshow y <> "-" <> tshow mo <> "-" <> tshow d

-- | Convert a 'WorldDate' to a zero-based ordinal day-of-year, using the
--   calendar's fixed month length.
--
--   The result is the number of whole days elapsed since the first day
--   of the year, in @[0 .. daysPerYear - 1]@ where
--   @daysPerYear = ccDaysPerMonth * ccMonthsPerYear@:
--
--     * month 1, day 1   → 0   (first day of the year)
--     * month 1, day 2   → 1
--     * the last day     → daysPerYear - 1
--
--   Zero-based to match how annual-cycle stage start days are authored
--   (a stage beginning on the first day of the year uses start day 0).
--   The date is put in 'canonicalWorldDate' form first, so an
--   out-of-range 'WorldDate' can never produce a negative or
--   past-end-of-year result — and so this reading agrees by construction
--   with what the setter and load staging store (#2339). 'wdYear' is
--   ignored: the cycle repeats every year.
worldDateToDayOfYear ∷ CalendarConfig → WorldDate → Int
worldDateToDayOfYear cc date =
    let dpm = max 1 (ccDaysPerMonth cc)
        WorldDate _ m d = canonicalWorldDate cc date
    in (m - 1) * dpm + (d - 1)

-- | Days in a calendar year (fixed month length), floored at 1.
calendarDaysPerYear ∷ CalendarConfig → Int
calendarDaysPerYear cc =
    max 1 (ccDaysPerMonth cc) * max 1 (ccMonthsPerYear cc)

-- | Add whole days to a 'WorldDate', carrying through months and years
--   with the calendar's fixed month length. Negative deltas are clamped
--   to zero — the world clock never runs backwards on its own; rewinding
--   is what 'world.setDate' is for.
worldDateAddDays ∷ CalendarConfig → Int → WorldDate → WorldDate
worldDateAddDays cc delta date@(WorldDate year _ _)
    | delta ≤ 0 = date
    | otherwise =
        let dpm = max 1 (ccDaysPerMonth cc)
            total = worldDateToDayOfYear cc date + delta
            (yearsCarried, doy) = total `divMod` calendarDaysPerYear cc
        in WorldDate (year + yearsCarried)
                     (doy `div` dpm + 1)
                     (doy `mod` dpm + 1)

-- | 'worldDateAddDays', but reporting every carry it cannot represent
--   instead of wrapping through it or crashing on it (#2280).
--
--   Three distinct 'Int' hazards live on this path, and the clock's
--   totality contract needs all three closed:
--
--     * 'wdYear' is an 'Int', and a day count near the accepted scale
--       ceiling carries enough years to overflow it. The wrap would
--       silently land the world in a negative year.
--     * 'calendarDaysPerYear' MULTIPLIES two authored calendar fields.
--       'CalendarConfig' arrives from world-gen data, not from a
--       validated range, so that product can wrap — and a product that
--       wraps to zero turns the @divMod@ below into a divide by zero,
--       which is a crash rather than a wrong answer.
--     * 'worldDateToDayOfYear' multiplies the (clamped) month index by
--       the month length, which can overflow for the same reason.
--
--   'Nothing' says "this date cannot be advanced by this many days";
--   'advanceWorldClock' answers that with the unchanged clock. For every
--   input whose intermediates fit, the result is exactly
--   'worldDateAddDays'.
worldDateAddDaysChecked ∷ CalendarConfig → Int → WorldDate → Maybe WorldDate
worldDateAddDaysChecked cc delta date@(WorldDate year _ _)
    | delta ≤ 0 = Just date
    | otherwise = do
        daysPerYear ← calendarDaysPerYearChecked cc
        dayOfYear ← worldDateToDayOfYearChecked cc date
        total ← addChecked dayOfYear delta
        let dpm = max 1 (ccDaysPerMonth cc)
            (yearsCarried, doy) = total `divMod` daysPerYear
        year' ← addChecked year yearsCarried
        pure (WorldDate year' (doy `div` dpm + 1) (doy `mod` dpm + 1))

-- | 'calendarDaysPerYear' with its product checked. Never zero when it
--   answers 'Just': both factors are floored at 1 exactly as the
--   unchecked version floors them, so the quotient it feeds is safe.
calendarDaysPerYearChecked ∷ CalendarConfig → Maybe Int
calendarDaysPerYearChecked cc =
    mulCheckedNonNeg (max 1 (ccDaysPerMonth cc)) (max 1 (ccMonthsPerYear cc))

-- | 'worldDateToDayOfYear' with its product and sum checked. The same
--   'canonicalWorldDate' form, so an in-range calendar gives the
--   identical answer.
worldDateToDayOfYearChecked ∷ CalendarConfig → WorldDate → Maybe Int
worldDateToDayOfYearChecked cc date =
    let dpm = max 1 (ccDaysPerMonth cc)
        WorldDate _ m d = canonicalWorldDate cc date
    in mulCheckedNonNeg (m - 1) dpm ⌦ \whole → addChecked whole (d - 1)

-- | Whole days elapsed since the world epoch (year 1, month 1, day 1 —
--   'defaultWorldDate'), i.e. a monotonic absolute day counter. This is
--   the flora growth clock: placement ages ('fiAge') are ages at day 0,
--   so current age derives from this without any per-instance mutable
--   state (World.Flora.Growth). Clamped at 0 for degenerate dates.
worldAbsoluteDay ∷ CalendarConfig → WorldDate → Int
worldAbsoluteDay cc date@(WorldDate year _ _) =
    max 0 ((year - 1) * calendarDaysPerYear cc + worldDateToDayOfYear cc date)

data CalendarConfig = CalendarConfig
    { ccDaysPerMonth  ∷ !Int      -- ^ e.g. 30
    , ccMonthsPerYear ∷ !Int      -- ^ e.g. 12
    , ccHoursPerDay   ∷ !Int      -- ^ e.g. 24 (controls sun cycle)
    , ccMinutesPerHour ∷ !Int     -- ^ e.g. 60
    } deriving (Show, Eq, Generic, Serialize, NFData)

defaultCalendarConfig ∷ CalendarConfig
defaultCalendarConfig = CalendarConfig
    { ccDaysPerMonth   = 30
    , ccMonthsPerYear  = 12
    , ccHoursPerDay    = 24
    , ccMinutesPerHour = 60
    }

data SunConfig = SunConfig
    { scTiltAngle    ∷ !Float   -- ^ Axial tilt in radians, controls season intensity
    , scDayLength    ∷ !Float   -- ^ Base day/night ratio at equinox (0.5 = equal)
    } deriving (Show, Eq, Generic, Serialize, NFData)

defaultSunConfig ∷ SunConfig
defaultSunConfig = SunConfig
    { scTiltAngle  = 0.4      -- ~23 degrees like Earth
    , scDayLength  = 0.5
    }

data MoonConfig = MoonConfig
    { mcCycleDays    ∷ !Int     -- ^ Days per lunar cycle
    , mcPhaseOffset  ∷ !Float   -- ^ Starting phase offset (0.0-1.0)
    } deriving (Show, Eq, Generic, Serialize, NFData)

defaultMoonConfig ∷ MoonConfig
defaultMoonConfig = MoonConfig
    { mcCycleDays   = 28
    , mcPhaseOffset = 0.0
    }
