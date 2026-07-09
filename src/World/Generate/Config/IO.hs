{-# LANGUAGE Strict, UnicodeSyntax #-}
module World.Generate.Config.IO
    ( loadWorldGenConfig
    , saveWorldGenYaml
    ) where

import UPrelude
import qualified Data.Yaml as Yaml
import System.Directory (doesFileExist)
import World.Generate.Config.Normalize (normalizeWorldGenConfig)
import World.Generate.Config.Types
import World.Generate.Types (WorldGenParams(..))
import World.Geology.Ore.Types (OreLevers(..))
import World.Geology.Timeline.Types (TimelineParams(..))
import World.Time.Types (CalendarConfig(..), SunConfig(..), MoonConfig(..))
import World.Weather.Types (ClimateParams(..))

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
