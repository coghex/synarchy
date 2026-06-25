{-# LANGUAGE Strict, UnicodeSyntax, DeriveGeneric, DeriveAnyClass #-}
module World.Time.Types
    ( WorldTime(..)
    , defaultWorldTime
    , worldTimeToSunAngle
    , advanceWorldTime
    , WorldDate(..)
    , defaultWorldDate
    , worldDateToDayOfYear
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
worldTimeToSunAngle ∷ WorldTime → Float
worldTimeToSunAngle (WorldTime h m) =
    let totalMinutes = fromIntegral h * 60.0 + fromIntegral m ∷ Float
    in totalMinutes / 1440.0   -- 1440 = 24 * 60

-- | Advance world time by a number of real seconds, scaled by a speed factor.
--   Returns the new time (wraps at 24:00).
--   timeScale: how many game-minutes pass per real-second.
advanceWorldTime ∷ Float → Float → WorldTime → WorldTime
advanceWorldTime timeScale dtSeconds (WorldTime h m) =
    let totalMinutes = fromIntegral h * 60 + fromIntegral m ∷ Float
        newTotal = totalMinutes + timeScale * dtSeconds
        -- Wrap around 1440 minutes (24 hours)
        wrapped = newTotal - 1440.0 * fromIntegral (floor (newTotal / 1440.0) ∷ Int)
        newH = floor wrapped `div` 60
        newM = floor wrapped `mod` 60
    in WorldTime (newH `mod` 24) (newM `mod` 60)

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
--   'wdMonth' and 'wdDay' are clamped into their valid ranges first, so
--   an out-of-range 'WorldDate' can never produce a negative or
--   past-end-of-year result. 'wdYear' is ignored: the cycle repeats
--   every year.
worldDateToDayOfYear ∷ CalendarConfig → WorldDate → Int
worldDateToDayOfYear cc (WorldDate _ month day) =
    let dpm = max 1 (ccDaysPerMonth cc)
        mpy = max 1 (ccMonthsPerYear cc)
        m   = max 1 (min mpy month)
        d   = max 1 (min dpm day)
    in (m - 1) * dpm + (d - 1)

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
