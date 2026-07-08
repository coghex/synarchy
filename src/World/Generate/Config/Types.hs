{-# LANGUAGE Strict, UnicodeSyntax #-}
module World.Generate.Config.Types
    ( WorldGenConfig(..)
    , CalendarYaml(..)
    , SunYaml(..)
    , MoonYaml(..)
    , ClimateYaml(..)
    , ResourcesYaml(..)
    , TimelineYaml(..)
    , defaultWorldGenConfig
    , defaultResourcesYaml
    , defaultCalendarYaml
    , defaultSunYaml
    , defaultMoonYaml
    , defaultClimateYaml
    , defaultTimelineYaml
    ) where

import UPrelude
import World.Geology.Timeline.Types (TimelineParams(..), defaultTimelineParams)
import World.Time.Types
    ( CalendarConfig(..), defaultCalendarConfig
    , SunConfig(..), defaultSunConfig
    , MoonConfig(..), defaultMoonConfig
    )
import World.Weather.Types (ClimateParams(..), defaultClimateParams)

-- | YAML-friendly world generation config.
--   Fields are Maybe so that undefined values use defaults.
data WorldGenConfig = WorldGenConfig
    { wgcSeed       ∷ !(Maybe Word64)
    , wgcWorldSize  ∷ !Int
    , wgcPlateCount ∷ !Int
    , wgcCalendar   ∷ !CalendarYaml
    , wgcSun        ∷ !SunYaml
    , wgcMoon       ∷ !MoonYaml
    , wgcClimate    ∷ !ClimateYaml
    , wgcErosionIntensity ∷ !Float
    , wgcVolcanicActivity ∷ !Float
    , wgcLavaPoolDepth    ∷ !Int
      -- ^ Max lava head above a pool's landing floor (tiles).
    , wgcLavaPoolRadius   ∷ !Int
      -- ^ Max pool footprint radius (tiles); area cap = ⌈π·r²⌉.
    , wgcWaterfallQuantum ∷ !Int
      -- ^ Max water-surface drop between adjacent river tiles before a
      --   stepped gorge is carved (tiles). Lower = more terraced
      --   cascades; higher = taller single waterfalls. Exposed in the
      --   create-world advanced tab.
    , wgcResources ∷ !ResourcesYaml
      -- ^ Resource-abundance levers (ore deposition flux multipliers).
    , wgcTimeline ∷ !TimelineYaml
      -- ^ Player-configurable timeline depth (eon/era/period/epoch/age counts).
    } deriving (Show, Eq)

-- | Resource-abundance levers. Purely mechanistic multipliers on the
--   sediment flux volcanic sources shed into ore sheets — no per-world
--   minimum is enforced (ore-poor seeds are accepted by design).
data ResourcesYaml = ResourcesYaml
    { ryOreAbundance    ∷ !Float  -- ^ Global multiplier on all ore flux
    , ryIronAbundance   ∷ !Float  -- ^ Iron-specific multiplier
    , ryCopperAbundance ∷ !Float  -- ^ Copper-specific multiplier
    } deriving (Show, Eq)

data CalendarYaml = CalendarYaml
    { cyDaysPerMonth   ∷ !Int
    , cyMonthsPerYear  ∷ !Int
    , cyHoursPerDay    ∷ !Int
    , cyMinutesPerHour ∷ !Int
    } deriving (Show, Eq)

data SunYaml = SunYaml
    { syTiltAngle ∷ !Float
    , syDayLength ∷ !Float
    } deriving (Show, Eq)

data MoonYaml = MoonYaml
    { myCycleDays   ∷ !Int
    , myPhaseOffset ∷ !Float
    } deriving (Show, Eq)

data ClimateYaml = ClimateYaml
    { clIterations     ∷ !Int
    , clCoriolisScale  ∷ !Float
    , clWindDrag       ∷ !Float
    , clThermalInertia ∷ !Float
    , clOrographicScale ∷ !Float
    , clEvapScale      ∷ !Float
    , clAlbedoFeedback ∷ !Float
    , clThcThreshold   ∷ !Float
    } deriving (Show, Eq)

data TimelineYaml = TimelineYaml
    { tyEonCount   ∷ !Int
    , tyEraCount   ∷ !Int
    , tyPeriodMin  ∷ !Int
    , tyPeriodMax  ∷ !Int
    , tyEpochMin   ∷ !Int
    , tyEpochMax   ∷ !Int
    , tyAgeMin     ∷ !Int
    , tyAgeMax     ∷ !Int
    } deriving (Show, Eq)

-- Defaults

defaultWorldGenConfig ∷ WorldGenConfig
defaultWorldGenConfig = WorldGenConfig
    { wgcSeed       = Nothing
    , wgcWorldSize  = 128
    , wgcPlateCount = 10
    , wgcCalendar   = defaultCalendarYaml
    , wgcSun        = defaultSunYaml
    , wgcMoon       = defaultMoonYaml
    , wgcClimate    = defaultClimateYaml
    , wgcErosionIntensity = 0.7
    -- Volcanism levers. Defaults tuned 2026-06-06 (user request:
    -- "a little higher than it is now"): activity 1.0 → 1.25,
    -- pool depth 4 → 6, pool radius 18 → 22.
    , wgcVolcanicActivity = 1.25
    , wgcLavaPoolDepth    = 6
    , wgcLavaPoolRadius   = 22
    , wgcWaterfallQuantum = 12
    , wgcResources        = defaultResourcesYaml
    , wgcTimeline         = defaultTimelineYaml
    }

defaultResourcesYaml ∷ ResourcesYaml
defaultResourcesYaml = ResourcesYaml
    { ryOreAbundance    = 1.0
    , ryIronAbundance   = 1.0
    , ryCopperAbundance = 1.0
    }

defaultCalendarYaml ∷ CalendarYaml
defaultCalendarYaml = CalendarYaml
    { cyDaysPerMonth   = ccDaysPerMonth defaultCalendarConfig
    , cyMonthsPerYear  = ccMonthsPerYear defaultCalendarConfig
    , cyHoursPerDay    = ccHoursPerDay defaultCalendarConfig
    , cyMinutesPerHour = ccMinutesPerHour defaultCalendarConfig
    }

defaultSunYaml ∷ SunYaml
defaultSunYaml = SunYaml
    { syTiltAngle = scTiltAngle defaultSunConfig
    , syDayLength = scDayLength defaultSunConfig
    }

defaultMoonYaml ∷ MoonYaml
defaultMoonYaml = MoonYaml
    { myCycleDays   = mcCycleDays defaultMoonConfig
    , myPhaseOffset = mcPhaseOffset defaultMoonConfig
    }

defaultClimateYaml ∷ ClimateYaml
defaultClimateYaml = ClimateYaml
    { clIterations     = cpIterations defaultClimateParams
    , clCoriolisScale  = cpCoriolisScale defaultClimateParams
    , clWindDrag       = cpWindDrag defaultClimateParams
    , clThermalInertia = cpThermalInertia defaultClimateParams
    , clOrographicScale = cpOrographicScale defaultClimateParams
    , clEvapScale      = cpEvapScale defaultClimateParams
    , clAlbedoFeedback = cpAlbedoFeedback defaultClimateParams
    , clThcThreshold   = cpThcThreshold defaultClimateParams
    }

defaultTimelineYaml ∷ TimelineYaml
defaultTimelineYaml = TimelineYaml
    { tyEonCount  = tlpEonCount defaultTimelineParams
    , tyEraCount  = tlpEraCount defaultTimelineParams
    , tyPeriodMin = tlpPeriodMin defaultTimelineParams
    , tyPeriodMax = tlpPeriodMax defaultTimelineParams
    , tyEpochMin  = tlpEpochMin defaultTimelineParams
    , tyEpochMax  = tlpEpochMax defaultTimelineParams
    , tyAgeMin    = tlpAgeMin defaultTimelineParams
    , tyAgeMax    = tlpAgeMax defaultTimelineParams
    }
