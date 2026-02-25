{-# LANGUAGE Strict, UnicodeSyntax, DeriveGeneric, DeriveAnyClass #-}
module World.Time.Types
    ( WorldTime(..)
    , defaultWorldTime
    , worldTimeToSunAngle
    , advanceWorldTime
    , WorldDate(..)
    , defaultWorldDate
    , CalendarConfig(..)
    , defaultCalendarConfig
    , SunConfig(..)
    , defaultSunConfig
    , MoonConfig(..)
    , defaultMoonConfig
    ) where

import UPrelude
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
data WorldDate = WorldDate
    { wdYear  ∷ !Int
    , wdMonth ∷ !Int   -- ^ 1-12
    , wdDay   ∷ !Int   -- ^ 1-31
    } deriving (Show, Eq)

defaultWorldDate ∷ WorldDate
defaultWorldDate = WorldDate
    { wdYear  = 1
    , wdMonth = 1
    , wdDay   = 1
    }

data CalendarConfig = CalendarConfig
    { ccDaysPerMonth  ∷ !Int      -- ^ e.g. 30
    , ccMonthsPerYear ∷ !Int      -- ^ e.g. 12
    , ccHoursPerDay   ∷ !Int      -- ^ e.g. 24 (controls sun cycle)
    , ccMinutesPerHour ∷ !Int     -- ^ e.g. 60
    } deriving (Show, Eq, Generic, Serialize)

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
    } deriving (Show, Eq, Generic, Serialize)

defaultSunConfig ∷ SunConfig
defaultSunConfig = SunConfig
    { scTiltAngle  = 0.4      -- ~23 degrees like Earth
    , scDayLength  = 0.5
    }

data MoonConfig = MoonConfig
    { mcCycleDays    ∷ !Int     -- ^ Days per lunar cycle
    , mcPhaseOffset  ∷ !Float   -- ^ Starting phase offset (0.0-1.0)
    } deriving (Show, Eq, Generic, Serialize)

defaultMoonConfig ∷ MoonConfig
defaultMoonConfig = MoonConfig
    { mcCycleDays   = 28
    , mcPhaseOffset = 0.0
    }
