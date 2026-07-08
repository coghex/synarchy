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
import qualified Data.Yaml as Yaml
import Data.Aeson ((.:), (.!=), (.:?), (.=), FromJSON(..), ToJSON(..))
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

-- FromJSON instances

-- NOTE: optional fields must use (.:?) with (.!=). With (.:), a
-- single missing key fails the WHOLE parse and loadWorldGenConfig
-- silently falls back to ALL defaults — every present setting in the
-- file gets discarded. See [[gotcha_aeson_optional_fields]].

instance FromJSON CalendarYaml where
    parseJSON (Yaml.Object v) = CalendarYaml
        <$> v .:? "days_per_month"   .!= cyDaysPerMonth defaultCalendarYaml
        <*> v .:? "months_per_year"  .!= cyMonthsPerYear defaultCalendarYaml
        <*> v .:? "hours_per_day"    .!= cyHoursPerDay defaultCalendarYaml
        <*> v .:? "minutes_per_hour" .!= cyMinutesPerHour defaultCalendarYaml
    parseJSON _ = fail "Expected an object for calendar"

instance FromJSON SunYaml where
    parseJSON (Yaml.Object v) = SunYaml
        <$> v .:? "tilt_angle" .!= syTiltAngle defaultSunYaml
        <*> v .:? "day_length" .!= syDayLength defaultSunYaml
    parseJSON _ = fail "Expected an object for sun"

instance FromJSON MoonYaml where
    parseJSON (Yaml.Object v) = MoonYaml
        <$> v .:? "cycle_days"   .!= myCycleDays defaultMoonYaml
        <*> v .:? "phase_offset" .!= myPhaseOffset defaultMoonYaml
    parseJSON _ = fail "Expected an object for moon"

instance FromJSON ClimateYaml where
    parseJSON (Yaml.Object v) = ClimateYaml
        <$> v .:? "iterations"       .!= clIterations defaultClimateYaml
        <*> v .:? "coriolis_scale"   .!= clCoriolisScale defaultClimateYaml
        <*> v .:? "wind_drag"        .!= clWindDrag defaultClimateYaml
        <*> v .:? "thermal_inertia"  .!= clThermalInertia defaultClimateYaml
        <*> v .:? "orographic_scale" .!= clOrographicScale defaultClimateYaml
        <*> v .:? "evap_scale"       .!= clEvapScale defaultClimateYaml
        <*> v .:? "albedo_feedback"  .!= clAlbedoFeedback defaultClimateYaml
        <*> v .:? "thc_threshold"    .!= clThcThreshold defaultClimateYaml
    parseJSON _ = fail "Expected an object for climate"

instance FromJSON ResourcesYaml where
    parseJSON (Yaml.Object v) = ResourcesYaml
        <$> v .:? "ore_abundance"    .!= ryOreAbundance defaultResourcesYaml
        <*> v .:? "iron_abundance"   .!= ryIronAbundance defaultResourcesYaml
        <*> v .:? "copper_abundance" .!= ryCopperAbundance defaultResourcesYaml
    parseJSON _ = fail "Expected an object for resources"

instance FromJSON TimelineYaml where
    parseJSON (Yaml.Object v) = TimelineYaml
        <$> v .:? "eon_count"   .!= tyEonCount defaultTimelineYaml
        <*> v .:? "era_count"   .!= tyEraCount defaultTimelineYaml
        <*> v .:? "period_min"  .!= tyPeriodMin defaultTimelineYaml
        <*> v .:? "period_max"  .!= tyPeriodMax defaultTimelineYaml
        <*> v .:? "epoch_min"   .!= tyEpochMin defaultTimelineYaml
        <*> v .:? "epoch_max"   .!= tyEpochMax defaultTimelineYaml
        <*> v .:? "age_min"     .!= tyAgeMin defaultTimelineYaml
        <*> v .:? "age_max"     .!= tyAgeMax defaultTimelineYaml
    parseJSON _ = fail "Expected an object for timeline"

instance FromJSON WorldGenConfig where
    parseJSON (Yaml.Object v) = do
        wgObj ← v .: "world_gen"
        WorldGenConfig
            <$> wgObj .:? "seed"
            <*> wgObj .:? "world_size"  .!= wgcWorldSize defaultWorldGenConfig
            <*> wgObj .:? "plate_count" .!= wgcPlateCount defaultWorldGenConfig
            <*> wgObj .:? "calendar"    .!= wgcCalendar defaultWorldGenConfig
            <*> wgObj .:? "sun"         .!= wgcSun defaultWorldGenConfig
            <*> wgObj .:? "moon"        .!= wgcMoon defaultWorldGenConfig
            <*> wgObj .:? "climate"     .!= wgcClimate defaultWorldGenConfig
            <*> wgObj .:? "erosion_intensity" .!= wgcErosionIntensity defaultWorldGenConfig
            <*> wgObj .:? "volcanic_activity" .!= wgcVolcanicActivity defaultWorldGenConfig
            <*> wgObj .:? "lava_pool_depth" .!= wgcLavaPoolDepth defaultWorldGenConfig
            <*> wgObj .:? "lava_pool_radius" .!= wgcLavaPoolRadius defaultWorldGenConfig
            <*> wgObj .:? "waterfall_quantum" .!= wgcWaterfallQuantum defaultWorldGenConfig
            <*> wgObj .:? "resources"   .!= wgcResources defaultWorldGenConfig
            <*> wgObj .:? "timeline"    .!= wgcTimeline defaultWorldGenConfig
    parseJSON _ = fail "Expected an object for world_gen"

-- ToJSON instances

instance ToJSON CalendarYaml where
    toJSON c = Yaml.object
        [ "days_per_month"   .= cyDaysPerMonth c
        , "months_per_year"  .= cyMonthsPerYear c
        , "hours_per_day"    .= cyHoursPerDay c
        , "minutes_per_hour" .= cyMinutesPerHour c
        ]

instance ToJSON SunYaml where
    toJSON s = Yaml.object
        [ "tilt_angle" .= syTiltAngle s
        , "day_length" .= syDayLength s
        ]

instance ToJSON MoonYaml where
    toJSON m = Yaml.object
        [ "cycle_days"   .= myCycleDays m
        , "phase_offset" .= myPhaseOffset m
        ]

instance ToJSON ClimateYaml where
    toJSON c = Yaml.object
        [ "iterations"       .= clIterations c
        , "coriolis_scale"   .= clCoriolisScale c
        , "wind_drag"        .= clWindDrag c
        , "thermal_inertia"  .= clThermalInertia c
        , "orographic_scale" .= clOrographicScale c
        , "evap_scale"       .= clEvapScale c
        , "albedo_feedback"  .= clAlbedoFeedback c
        , "thc_threshold"    .= clThcThreshold c
        ]

instance ToJSON WorldGenConfig where
    toJSON cfg = Yaml.object
        [ "world_gen" .= Yaml.object
            [ "seed"        .= wgcSeed cfg
            , "world_size"  .= wgcWorldSize cfg
            , "plate_count" .= wgcPlateCount cfg
            , "calendar"    .= wgcCalendar cfg
            , "sun"         .= wgcSun cfg
            , "moon"        .= wgcMoon cfg
            , "climate"     .= wgcClimate cfg
            , "erosion_intensity" .= wgcErosionIntensity cfg
            , "volcanic_activity" .= wgcVolcanicActivity cfg
            , "lava_pool_depth" .= wgcLavaPoolDepth cfg
            , "lava_pool_radius" .= wgcLavaPoolRadius cfg
            , "waterfall_quantum" .= wgcWaterfallQuantum cfg
            , "resources" .= wgcResources cfg
            , "timeline"  .= wgcTimeline cfg
            ]
        ]

instance ToJSON ResourcesYaml where
    toJSON r = Yaml.object
        [ "ore_abundance"    .= ryOreAbundance r
        , "iron_abundance"   .= ryIronAbundance r
        , "copper_abundance" .= ryCopperAbundance r
        ]

instance ToJSON TimelineYaml where
    toJSON t = Yaml.object
        [ "eon_count"  .= tyEonCount t
        , "era_count"  .= tyEraCount t
        , "period_min" .= tyPeriodMin t
        , "period_max" .= tyPeriodMax t
        , "epoch_min"  .= tyEpochMin t
        , "epoch_max"  .= tyEpochMax t
        , "age_min"    .= tyAgeMin t
        , "age_max"    .= tyAgeMax t
        ]
