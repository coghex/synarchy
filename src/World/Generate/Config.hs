{-# LANGUAGE Strict, UnicodeSyntax #-}
module World.Generate.Config
    ( WorldGenConfig(..)
    , CalendarYaml(..)
    , SunYaml(..)
    , MoonYaml(..)
    , ClimateYaml(..)
    , loadWorldGenConfig
    , saveWorldGenYaml
    , defaultWorldGenConfig
    , applyConfigToParams
    ) where

import UPrelude
import qualified Data.Yaml as Yaml
import Data.Aeson ((.:), (.!=), (.:?), (.=), FromJSON(..), ToJSON(..))
import qualified Data.Aeson as Yaml (object)
import qualified Data.Text as T
import System.Directory (doesFileExist)
import World.Generate.Types (WorldGenParams(..), defaultWorldGenParams)
import World.Time.Types
    ( CalendarConfig(..), defaultCalendarConfig
    , SunConfig(..), defaultSunConfig
    , MoonConfig(..), defaultMoonConfig
    )
import World.Weather.Types
    ( ClimateParams(..), defaultClimateParams
    , initClimateState
    )

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
            ]
        ]

-- | Load world gen config from YAML, falling back to defaults on error
loadWorldGenConfig ∷ FilePath → IO WorldGenConfig
loadWorldGenConfig path = do
    exists ← doesFileExist path
    if not exists
        then return defaultWorldGenConfig
        else do
            result ← Yaml.decodeFileEither path
            case result of
                Left _  → return defaultWorldGenConfig
                Right c → return c

-- | Save world gen params as YAML (for save files)
saveWorldGenYaml ∷ FilePath → WorldGenParams → IO ()
saveWorldGenYaml path params =
    Yaml.encodeFile path (paramsToConfig params)

-- | Extract a WorldGenConfig from full WorldGenParams
paramsToConfig ∷ WorldGenParams → WorldGenConfig
paramsToConfig p = WorldGenConfig
    { wgcSeed       = Just (wgpSeed p)
    , wgcWorldSize  = wgpWorldSize p
    , wgcPlateCount = wgpPlateCount p
    , wgcCalendar   = CalendarYaml
        { cyDaysPerMonth   = ccDaysPerMonth (wgpCalender p)
        , cyMonthsPerYear  = ccMonthsPerYear (wgpCalender p)
        , cyHoursPerDay    = ccHoursPerDay (wgpCalender p)
        , cyMinutesPerHour = ccMinutesPerHour (wgpCalender p)
        }
    , wgcSun = SunYaml
        { syTiltAngle = scTiltAngle (wgpSunConfig p)
        , syDayLength = scDayLength (wgpSunConfig p)
        }
    , wgcMoon = MoonYaml
        { myCycleDays   = mcCycleDays (wgpMoonConfig p)
        , myPhaseOffset = mcPhaseOffset (wgpMoonConfig p)
        }
    , wgcClimate = ClimateYaml
        { clIterations     = cpIterations (wgpClimateParams p)
        , clCoriolisScale  = cpCoriolisScale (wgpClimateParams p)
        , clWindDrag       = cpWindDrag (wgpClimateParams p)
        , clThermalInertia = cpThermalInertia (wgpClimateParams p)
        , clOrographicScale = cpOrographicScale (wgpClimateParams p)
        , clEvapScale      = cpEvapScale (wgpClimateParams p)
        , clAlbedoFeedback = cpAlbedoFeedback (wgpClimateParams p)
        , clThcThreshold   = cpThcThreshold (wgpClimateParams p)
        }
    , wgcErosionIntensity = wgpErosionIntensity p
    , wgcVolcanicActivity = wgpVolcanicActivity p
    , wgcLavaPoolDepth    = wgpLavaPoolDepth p
    , wgcLavaPoolRadius   = wgpLavaPoolRadius p
    , wgcWaterfallQuantum = wgpWaterfallQuantum p
    }

-- | Apply a YAML config to the default WorldGenParams.
--   Only sets the configurable fields; derived fields (plates,
--   timeline, ocean map, climate state) remain at their defaults
--   and must be computed during world generation.
applyConfigToParams ∷ WorldGenConfig → WorldGenParams
applyConfigToParams cfg = defaultWorldGenParams
    { wgpSeed       = case wgcSeed cfg of
                        Just s  → s
                        Nothing → wgpSeed defaultWorldGenParams
    , wgpWorldSize  = wgcWorldSize cfg
    , wgpPlateCount = wgcPlateCount cfg
    , wgpCalender   = CalendarConfig
        { ccDaysPerMonth   = cyDaysPerMonth (wgcCalendar cfg)
        , ccMonthsPerYear  = cyMonthsPerYear (wgcCalendar cfg)
        , ccHoursPerDay    = cyHoursPerDay (wgcCalendar cfg)
        , ccMinutesPerHour = cyMinutesPerHour (wgcCalendar cfg)
        }
    , wgpSunConfig  = SunConfig
        { scTiltAngle = syTiltAngle (wgcSun cfg)
        , scDayLength = syDayLength (wgcSun cfg)
        }
    , wgpMoonConfig = MoonConfig
        { mcCycleDays   = myCycleDays (wgcMoon cfg)
        , mcPhaseOffset = myPhaseOffset (wgcMoon cfg)
        }
    , wgpClimateParams = ClimateParams
        { cpIterations      = clIterations (wgcClimate cfg)
        , cpCoriolisScale   = clCoriolisScale (wgcClimate cfg)
        , cpWindDrag        = clWindDrag (wgcClimate cfg)
        , cpThermalInertia  = clThermalInertia (wgcClimate cfg)
        , cpOrographicScale = clOrographicScale (wgcClimate cfg)
        , cpEvapScale       = clEvapScale (wgcClimate cfg)
        , cpAlbedoFeedback  = clAlbedoFeedback (wgcClimate cfg)
        , cpThcThreshold    = clThcThreshold (wgcClimate cfg)
        }
    , wgpClimateState = initClimateState (wgcWorldSize cfg)
    , wgpErosionIntensity = wgcErosionIntensity cfg
    , wgpVolcanicActivity = wgcVolcanicActivity cfg
    , wgpLavaPoolDepth    = wgcLavaPoolDepth cfg
    , wgpLavaPoolRadius   = wgcLavaPoolRadius cfg
    , wgpWaterfallQuantum = wgcWaterfallQuantum cfg
    }
