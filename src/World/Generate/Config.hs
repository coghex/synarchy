{-# LANGUAGE Strict, UnicodeSyntax #-}
module World.Generate.Config
    ( WorldGenConfig(..)
    , CalendarYaml(..)
    , SunYaml(..)
    , MoonYaml(..)
    , ClimateYaml(..)
    , ResourcesYaml(..)
    , TimelineYaml(..)
    , defaultTimelineYaml
    , minimumWorldSize
    , normalizeWorldSize
    , normalizePlateCount
    , normalizeWorldGenInputs
    , normalizeWorldGenConfig
    , timelineParamsOf
    , loadWorldGenConfig
    , defaultWorldGenConfig
    , applyConfigToParams
    ) where

import UPrelude
import World.Generate.Config.IO (loadWorldGenConfig)
import World.Generate.Config.Normalize
    ( minimumWorldSize
    , normalizeWorldSize
    , normalizePlateCount
    , normalizeWorldGenInputs
    , normalizeWorldGenConfig
    )
import World.Generate.Config.Types
import World.Generate.Types (WorldGenParams(..), defaultWorldGenParams)
import World.Geology.Ore.Types (OreLevers(..))
import World.Geology.Timeline.Types (TimelineParams(..))
import World.Time.Types
    ( CalendarConfig(..)
    , SunConfig(..)
    , MoonConfig(..)
    )
import World.Weather.Types (ClimateParams(..), initClimateState)

-- | Apply a YAML config to the default WorldGenParams.
--   Only sets the configurable fields; derived fields (plates,
--   timeline, ocean map, climate state) remain at their defaults
--   and must be computed during world generation.
-- | Convert the YAML timeline config to the engine TimelineParams used by
--   buildTimeline. Exposed so Init can pass it before WorldGenParams is fully
--   assembled.
timelineParamsOf ∷ WorldGenConfig → TimelineParams
timelineParamsOf cfg =
    let ty = wgcTimeline cfg
        -- Defensive clamps: counts ≥ 1, and max ≥ min ≥ 1, so malformed
        -- input (e.g. min > max from a stray textbox) can't yield zero/
        -- negative counts in the hashToRangeGeo rolls.
        loHi lo hi = let lo' = max 1 lo in (lo', max lo' hi)
        (pMin, pMax) = loHi (tyPeriodMin ty) (tyPeriodMax ty)
        (eMin, eMax) = loHi (tyEpochMin ty) (tyEpochMax ty)
        (aMin, aMax) = loHi (tyAgeMin ty) (tyAgeMax ty)
    in TimelineParams
        { tlpEonCount  = max 1 (tyEonCount ty)
        , tlpEraCount  = max 1 (tyEraCount ty)
        , tlpPeriodMin = pMin
        , tlpPeriodMax = pMax
        , tlpEpochMin  = eMin
        , tlpEpochMax  = eMax
        , tlpAgeMin    = aMin
        , tlpAgeMax    = aMax
        }

applyConfigToParams ∷ WorldGenConfig → WorldGenParams
applyConfigToParams cfg0 =
    let cfg = normalizeWorldGenConfig cfg0
    in defaultWorldGenParams
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
    , wgpOreLevers        = OreLevers
        { olGlobal = ryOreAbundance (wgcResources cfg)
        , olIron   = ryIronAbundance (wgcResources cfg)
        , olCopper = ryCopperAbundance (wgcResources cfg)
        }
    , wgpTimelineParams   = timelineParamsOf cfg
    }
