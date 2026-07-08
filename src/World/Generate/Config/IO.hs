{-# LANGUAGE Strict, UnicodeSyntax #-}
module World.Generate.Config.IO
    ( loadWorldGenConfig
    , saveWorldGenYaml
    ) where

import UPrelude
import qualified Data.Yaml as Yaml
import Data.Aeson ((.:), (.!=), (.:?), (.=), FromJSON(..), ToJSON(..))
import System.Directory (doesFileExist)
import World.Generate.Config.Normalize (normalizeWorldGenConfig)
import World.Generate.Config.Types
import World.Generate.Types (WorldGenParams(..))
import World.Geology.Ore.Types (OreLevers(..))
import World.Geology.Timeline.Types (TimelineParams(..))
import World.Time.Types (CalendarConfig(..), SunConfig(..), MoonConfig(..))
import World.Weather.Types (ClimateParams(..))

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
                Right c → return (normalizeWorldGenConfig c)

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
    , wgcResources        = ResourcesYaml
        { ryOreAbundance    = olGlobal (wgpOreLevers p)
        , ryIronAbundance   = olIron (wgpOreLevers p)
        , ryCopperAbundance = olCopper (wgpOreLevers p)
        }
    , wgcTimeline = TimelineYaml
        { tyEonCount  = tlpEonCount (wgpTimelineParams p)
        , tyEraCount  = tlpEraCount (wgpTimelineParams p)
        , tyPeriodMin = tlpPeriodMin (wgpTimelineParams p)
        , tyPeriodMax = tlpPeriodMax (wgpTimelineParams p)
        , tyEpochMin  = tlpEpochMin (wgpTimelineParams p)
        , tyEpochMax  = tlpEpochMax (wgpTimelineParams p)
        , tyAgeMin    = tlpAgeMin (wgpTimelineParams p)
        , tyAgeMax    = tlpAgeMax (wgpTimelineParams p)
        }
    }
