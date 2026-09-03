{-# LANGUAGE Strict #-}
module World.Generate.Config.Types
    ( WorldGenConfig(..)
    , CalendarYaml(..)
    , SunYaml(..)
    , MoonYaml(..)
    , ClimateYaml(..)
    , ResourcesYaml(..)
    , TimelineYaml(..)
    , defaultWorldGenConfig
    , defaultTimelineYaml
      -- * The raw YAML document
    , WorldGenConfigRaw(..)
    , SunYamlRaw(..)
    , MoonYamlRaw(..)
    , ClimateYamlRaw(..)
    , ResourcesYamlRaw(..)
    , numberSourceOf
    ) where

import UPrelude
import qualified Data.Yaml as Yaml
import Data.Aeson ((.:), (.!=), (.:?), (.=), FromJSON(..), ToJSON(..))
import Engine.Core.Yaml.Scalar (NumberSource(..))
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

-- Raw YAML documents (#2288)
--
-- Every floating-point leaf decodes as a 'NumberSource' rather than a
-- 'Float', for two reasons. A scalar spelling a non-finite number
-- (@.inf@, @.nan@) is a YAML STRING, so a bare 'Float' leaf would fail
-- the whole document's parse and discard every other setting in the
-- file; and a warning about a rejected leaf has to quote the number as
-- the file spelled it, not the infinity it narrowed to.
--
-- Nothing here applies the domain. These records are the structural
-- decode only; 'World.Generate.Config.Validate.resolveWorldGenConfigRaw'
-- narrows and judges each leaf.

data SunYamlRaw = SunYamlRaw
    { sunrTiltAngle ∷ !NumberSource
    , sunrDayLength ∷ !NumberSource
    } deriving (Show, Eq)

data MoonYamlRaw = MoonYamlRaw
    { moonrCycleDays   ∷ !Int
    , moonrPhaseOffset ∷ !NumberSource
    } deriving (Show, Eq)

data ClimateYamlRaw = ClimateYamlRaw
    { climrIterations      ∷ !Int
    , climrCoriolisScale   ∷ !NumberSource
    , climrWindDrag        ∷ !NumberSource
    , climrThermalInertia  ∷ !NumberSource
    , climrOrographicScale ∷ !NumberSource
    , climrEvapScale       ∷ !NumberSource
    , climrAlbedoFeedback  ∷ !NumberSource
    , climrThcThreshold    ∷ !NumberSource
    } deriving (Show, Eq)

data ResourcesYamlRaw = ResourcesYamlRaw
    { resrOreAbundance    ∷ !NumberSource
    , resrIronAbundance   ∷ !NumberSource
    , resrCopperAbundance ∷ !NumberSource
    } deriving (Show, Eq)

data WorldGenConfigRaw = WorldGenConfigRaw
    { wcrSeed              ∷ !(Maybe Word64)
    , wcrWorldSize         ∷ !Int
    , wcrPlateCount        ∷ !Int
    , wcrCalendar          ∷ !CalendarYaml
    , wcrSun               ∷ !SunYamlRaw
    , wcrMoon              ∷ !MoonYamlRaw
    , wcrClimate           ∷ !ClimateYamlRaw
    , wcrErosionIntensity  ∷ !NumberSource
    , wcrVolcanicActivity  ∷ !NumberSource
    , wcrLavaPoolDepth     ∷ !Int
    , wcrLavaPoolRadius    ∷ !Int
    , wcrWaterfallQuantum  ∷ !Int
    , wcrResources         ∷ !ResourcesYamlRaw
    , wcrTimeline          ∷ !TimelineYaml
    } deriving (Show, Eq)

-- | A default 'Float' as the source a document would have spelled it
--   with, so an ABSENT leaf and a leaf written with the default value
--   resolve identically. Every shipped default is finite, so the
--   widening is exact.
numberSourceOf ∷ Float → NumberSource
numberSourceOf x = NumberSource (realToFrac x) (tshow x)

-- FromJSON instances

-- NOTE: optional fields must use (.:?) with (.!=). With (.:), a
-- single missing key fails the WHOLE parse and loadWorldGenConfig
-- falls back to ALL defaults — every present setting in the file gets
-- discarded. Since #2286 the loss is still total but no longer silent:
-- the loader warns once (LevelWarn / CatInit) naming the file and the
-- decoder's own error before returning the defaults. That warning is a
-- diagnostic, not a licence — a (.:) here still throws away the whole
-- authored document. See [[gotcha_aeson_optional_fields]].

instance FromJSON CalendarYaml where
    parseJSON (Yaml.Object v) = CalendarYaml
        <$> v .:? "days_per_month"   .!= cyDaysPerMonth defaultCalendarYaml
        <*> v .:? "months_per_year"  .!= cyMonthsPerYear defaultCalendarYaml
        <*> v .:? "hours_per_day"    .!= cyHoursPerDay defaultCalendarYaml
        <*> v .:? "minutes_per_hour" .!= cyMinutesPerHour defaultCalendarYaml
    parseJSON _ = fail "Expected an object for calendar"

instance FromJSON SunYamlRaw where
    parseJSON (Yaml.Object v) = SunYamlRaw
        <$> v .:? "tilt_angle" .!= numberSourceOf (syTiltAngle defaultSunYaml)
        <*> v .:? "day_length" .!= numberSourceOf (syDayLength defaultSunYaml)
    parseJSON _ = fail "Expected an object for sun"

instance FromJSON MoonYamlRaw where
    parseJSON (Yaml.Object v) = MoonYamlRaw
        <$> v .:? "cycle_days"   .!= myCycleDays defaultMoonYaml
        <*> v .:? "phase_offset"
                .!= numberSourceOf (myPhaseOffset defaultMoonYaml)
    parseJSON _ = fail "Expected an object for moon"

instance FromJSON ClimateYamlRaw where
    parseJSON (Yaml.Object v) = ClimateYamlRaw
        <$> v .:? "iterations"       .!= clIterations defaultClimateYaml
        <*> v .:? "coriolis_scale"
                .!= numberSourceOf (clCoriolisScale defaultClimateYaml)
        <*> v .:? "wind_drag"
                .!= numberSourceOf (clWindDrag defaultClimateYaml)
        <*> v .:? "thermal_inertia"
                .!= numberSourceOf (clThermalInertia defaultClimateYaml)
        <*> v .:? "orographic_scale"
                .!= numberSourceOf (clOrographicScale defaultClimateYaml)
        <*> v .:? "evap_scale"
                .!= numberSourceOf (clEvapScale defaultClimateYaml)
        <*> v .:? "albedo_feedback"
                .!= numberSourceOf (clAlbedoFeedback defaultClimateYaml)
        <*> v .:? "thc_threshold"
                .!= numberSourceOf (clThcThreshold defaultClimateYaml)
    parseJSON _ = fail "Expected an object for climate"

instance FromJSON ResourcesYamlRaw where
    parseJSON (Yaml.Object v) = ResourcesYamlRaw
        <$> v .:? "ore_abundance"
                .!= numberSourceOf (ryOreAbundance defaultResourcesYaml)
        <*> v .:? "iron_abundance"
                .!= numberSourceOf (ryIronAbundance defaultResourcesYaml)
        <*> v .:? "copper_abundance"
                .!= numberSourceOf (ryCopperAbundance defaultResourcesYaml)
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

-- | The world-generation document, structurally. There is deliberately
--   no @FromJSON WorldGenConfig@ beside this one: a second decode path
--   would be a second place a float leaf could enter unjudged.
instance FromJSON WorldGenConfigRaw where
    parseJSON (Yaml.Object v) = do
        wgObj ← v .: "world_gen"
        WorldGenConfigRaw
            <$> wgObj .:? "seed"
            <*> wgObj .:? "world_size"  .!= wgcWorldSize defaultWorldGenConfig
            <*> wgObj .:? "plate_count" .!= wgcPlateCount defaultWorldGenConfig
            <*> wgObj .:? "calendar"    .!= wgcCalendar defaultWorldGenConfig
            <*> wgObj .:? "sun"         .!= defaultSunYamlRaw
            <*> wgObj .:? "moon"        .!= defaultMoonYamlRaw
            <*> wgObj .:? "climate"     .!= defaultClimateYamlRaw
            <*> wgObj .:? "erosion_intensity"
                    .!= numberSourceOf (wgcErosionIntensity defaultWorldGenConfig)
            <*> wgObj .:? "volcanic_activity"
                    .!= numberSourceOf (wgcVolcanicActivity defaultWorldGenConfig)
            <*> wgObj .:? "lava_pool_depth" .!= wgcLavaPoolDepth defaultWorldGenConfig
            <*> wgObj .:? "lava_pool_radius" .!= wgcLavaPoolRadius defaultWorldGenConfig
            <*> wgObj .:? "waterfall_quantum" .!= wgcWaterfallQuantum defaultWorldGenConfig
            <*> wgObj .:? "resources"   .!= defaultResourcesYamlRaw
            <*> wgObj .:? "timeline"    .!= wgcTimeline defaultWorldGenConfig
    parseJSON _ = fail "Expected an object for world_gen"

-- | The raw mirrors of the shipped defaults, for an absent sub-table.

defaultSunYamlRaw ∷ SunYamlRaw
defaultSunYamlRaw = SunYamlRaw
    { sunrTiltAngle = numberSourceOf (syTiltAngle defaultSunYaml)
    , sunrDayLength = numberSourceOf (syDayLength defaultSunYaml)
    }

defaultMoonYamlRaw ∷ MoonYamlRaw
defaultMoonYamlRaw = MoonYamlRaw
    { moonrCycleDays   = myCycleDays defaultMoonYaml
    , moonrPhaseOffset = numberSourceOf (myPhaseOffset defaultMoonYaml)
    }

defaultClimateYamlRaw ∷ ClimateYamlRaw
defaultClimateYamlRaw = ClimateYamlRaw
    { climrIterations      = clIterations defaultClimateYaml
    , climrCoriolisScale   = numberSourceOf (clCoriolisScale defaultClimateYaml)
    , climrWindDrag        = numberSourceOf (clWindDrag defaultClimateYaml)
    , climrThermalInertia  = numberSourceOf (clThermalInertia defaultClimateYaml)
    , climrOrographicScale = numberSourceOf (clOrographicScale defaultClimateYaml)
    , climrEvapScale       = numberSourceOf (clEvapScale defaultClimateYaml)
    , climrAlbedoFeedback  = numberSourceOf (clAlbedoFeedback defaultClimateYaml)
    , climrThcThreshold    = numberSourceOf (clThcThreshold defaultClimateYaml)
    }

defaultResourcesYamlRaw ∷ ResourcesYamlRaw
defaultResourcesYamlRaw = ResourcesYamlRaw
    { resrOreAbundance    = numberSourceOf (ryOreAbundance defaultResourcesYaml)
    , resrIronAbundance   = numberSourceOf (ryIronAbundance defaultResourcesYaml)
    , resrCopperAbundance = numberSourceOf (ryCopperAbundance defaultResourcesYaml)
    }

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
