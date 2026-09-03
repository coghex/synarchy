{-# LANGUAGE Strict #-}
-- | Where the world-generation float domain (#2288) meets the two
--   record shapes that carry those settings.
--
--   'World.Generate.Config.Domain' states the domain of one leaf and
--   depends on nothing. This module names the leaves: the fifteen
--   floating-point settings, once as fields of the YAML-facing
--   'WorldGenConfig' and once as fields of the generation-facing
--   'WorldGenParams', with the same field name and the same domain on
--   both sides. Every boundary drives one of these two tables, so a
--   setting cannot be validated in one shape and forgotten in the other.
--
--   The two tables are the SAME fifteen settings, and the headless
--   @world-generation setting domains@ describe pins that: same field
--   names, same domains, and defaults that agree value for value.
module World.Generate.Config.Validate
    ( -- * One leaf of a record
      FloatLeaf(..)
    , floatRejections
    , repairFloats
      -- * The world-generation config (YAML, Lua)
    , configFloatLeaves
    , worldGenConfigRejections
    , repairWorldGenConfig
      -- * The raw YAML document
    , resolveWorldGenConfigRaw
      -- * The generation parameters (saves)
    , paramsFloatLeaves
    , worldGenParamsRejections
    , repairWorldGenParams
    ) where

import UPrelude
import Engine.Core.Yaml.Scalar (NumberSource(..))
import World.Generate.Config.Domain
import World.Generate.Config.Types
import World.Generate.Types (WorldGenParams(..), defaultWorldGenParams)
import World.Geology.Ore.Types (OreLevers(..))
import World.Time.Types (SunConfig(..), MoonConfig(..))
import World.Weather.Types (ClimateParams(..))

-- | One floating-point setting, as seen on a record of type @α@: the
--   full field name a diagnostic prints, the domain it must satisfy,
--   and the pair of accessors a repair needs.
data FloatLeaf α = FloatLeaf
    { flField  ∷ Text
    , flDomain ∷ FloatDomain
    , flGet    ∷ α → Float
    , flSet    ∷ Float → α → α
    }

-- | Every leaf of a record that is outside its domain. Empty for a
--   valid record.
floatRejections ∷ [FloatLeaf α] → α → [WorldGenFieldRejection]
floatRejections leaves value =
    [ rejection
    | leaf ← leaves
    , Just rejection ← [checkWorldGenFloat (flField leaf) (flDomain leaf)
                                           (flGet leaf value)] ]

-- | Default EVERY out-of-domain leaf from @defaults@, leaving every
--   other leaf exactly as it was, and report each replacement beside the
--   rendering of the default that took its place.
--
--   This is the field-local half of the contract: one bad leaf costs
--   that leaf and nothing else. Idempotent, because a defaulted leaf is
--   in domain by construction (the shipped defaults all are, which the
--   headless describe pins).
repairFloats ∷ [FloatLeaf α] → α → α → (α, [(WorldGenFieldRejection, Text)])
repairFloats leaves defaults value0 = foldl' step (value0, []) leaves
  where
    step (value, found) leaf =
        case checkWorldGenFloat (flField leaf) (flDomain leaf) (flGet leaf value) of
            Nothing → (value, found)
            Just rejection →
                let dflt = flGet leaf defaults
                in ( flSet leaf dflt value
                   , found ⧺ [(rejection, tshow dflt)] )

-- * The world-generation config

-- | The fifteen floating-point settings of 'WorldGenConfig' — the shape
--   the YAML file decodes into and @world.setGenConfig@ writes.
configFloatLeaves ∷ [FloatLeaf WorldGenConfig]
configFloatLeaves =
    [ FloatLeaf fieldErosionIntensity
        (InRange erosionIntensityMin erosionIntensityMax)
        wgcErosionIntensity (\x c → c { wgcErosionIntensity = x })
    , FloatLeaf fieldVolcanicActivity
        (InRange volcanicActivityMin volcanicActivityMax)
        wgcVolcanicActivity (\x c → c { wgcVolcanicActivity = x })
    , FloatLeaf fieldTiltAngle AnyFinite
        (syTiltAngle . wgcSun)
        (\x c → c { wgcSun = (wgcSun c) { syTiltAngle = x } })
    , FloatLeaf fieldDayLength (InRange unitIntervalMin unitIntervalMax)
        (syDayLength . wgcSun)
        (\x c → c { wgcSun = (wgcSun c) { syDayLength = x } })
    , FloatLeaf fieldPhaseOffset (InRange unitIntervalMin unitIntervalMax)
        (myPhaseOffset . wgcMoon)
        (\x c → c { wgcMoon = (wgcMoon c) { myPhaseOffset = x } })
    , FloatLeaf fieldOreAbundance (InRange abundanceMin abundanceMax)
        (ryOreAbundance . wgcResources)
        (\x c → c { wgcResources = (wgcResources c) { ryOreAbundance = x } })
    , FloatLeaf fieldIronAbundance (InRange abundanceMin abundanceMax)
        (ryIronAbundance . wgcResources)
        (\x c → c { wgcResources = (wgcResources c) { ryIronAbundance = x } })
    , FloatLeaf fieldCopperAbundance (InRange abundanceMin abundanceMax)
        (ryCopperAbundance . wgcResources)
        (\x c → c { wgcResources = (wgcResources c) { ryCopperAbundance = x } })
    , FloatLeaf fieldCoriolisScale FiniteNonNegative
        (clCoriolisScale . wgcClimate)
        (\x c → c { wgcClimate = (wgcClimate c) { clCoriolisScale = x } })
    , FloatLeaf fieldWindDrag FiniteNonNegative
        (clWindDrag . wgcClimate)
        (\x c → c { wgcClimate = (wgcClimate c) { clWindDrag = x } })
    , FloatLeaf fieldThermalInertia (InRange unitIntervalMin unitIntervalMax)
        (clThermalInertia . wgcClimate)
        (\x c → c { wgcClimate = (wgcClimate c) { clThermalInertia = x } })
    , FloatLeaf fieldOrographicScale FiniteNonNegative
        (clOrographicScale . wgcClimate)
        (\x c → c { wgcClimate = (wgcClimate c) { clOrographicScale = x } })
    , FloatLeaf fieldEvapScale FiniteNonNegative
        (clEvapScale . wgcClimate)
        (\x c → c { wgcClimate = (wgcClimate c) { clEvapScale = x } })
    , FloatLeaf fieldAlbedoFeedback FiniteNonNegative
        (clAlbedoFeedback . wgcClimate)
        (\x c → c { wgcClimate = (wgcClimate c) { clAlbedoFeedback = x } })
    , FloatLeaf fieldThcThreshold AnyFinite
        (clThcThreshold . wgcClimate)
        (\x c → c { wgcClimate = (wgcClimate c) { clThcThreshold = x } })
    ]

worldGenConfigRejections ∷ WorldGenConfig → [WorldGenFieldRejection]
worldGenConfigRejections = floatRejections configFloatLeaves

-- | 'repairFloats' against the shipped defaults.
repairWorldGenConfig
    ∷ WorldGenConfig → (WorldGenConfig, [(WorldGenFieldRejection, Text)])
repairWorldGenConfig = repairFloats configFloatLeaves defaultWorldGenConfig

-- * The generation parameters

-- | The same fifteen settings as fields of 'WorldGenParams' — the shape
--   a save stores and generation consumes.
paramsFloatLeaves ∷ [FloatLeaf WorldGenParams]
paramsFloatLeaves =
    [ FloatLeaf fieldErosionIntensity
        (InRange erosionIntensityMin erosionIntensityMax)
        wgpErosionIntensity (\x p → p { wgpErosionIntensity = x })
    , FloatLeaf fieldVolcanicActivity
        (InRange volcanicActivityMin volcanicActivityMax)
        wgpVolcanicActivity (\x p → p { wgpVolcanicActivity = x })
    , FloatLeaf fieldTiltAngle AnyFinite
        (scTiltAngle . wgpSunConfig)
        (\x p → p { wgpSunConfig = (wgpSunConfig p) { scTiltAngle = x } })
    , FloatLeaf fieldDayLength (InRange unitIntervalMin unitIntervalMax)
        (scDayLength . wgpSunConfig)
        (\x p → p { wgpSunConfig = (wgpSunConfig p) { scDayLength = x } })
    , FloatLeaf fieldPhaseOffset (InRange unitIntervalMin unitIntervalMax)
        (mcPhaseOffset . wgpMoonConfig)
        (\x p → p { wgpMoonConfig = (wgpMoonConfig p) { mcPhaseOffset = x } })
    , FloatLeaf fieldOreAbundance (InRange abundanceMin abundanceMax)
        (olGlobal . wgpOreLevers)
        (\x p → p { wgpOreLevers = (wgpOreLevers p) { olGlobal = x } })
    , FloatLeaf fieldIronAbundance (InRange abundanceMin abundanceMax)
        (olIron . wgpOreLevers)
        (\x p → p { wgpOreLevers = (wgpOreLevers p) { olIron = x } })
    , FloatLeaf fieldCopperAbundance (InRange abundanceMin abundanceMax)
        (olCopper . wgpOreLevers)
        (\x p → p { wgpOreLevers = (wgpOreLevers p) { olCopper = x } })
    , FloatLeaf fieldCoriolisScale FiniteNonNegative
        (cpCoriolisScale . wgpClimateParams)
        (\x p → p { wgpClimateParams =
                        (wgpClimateParams p) { cpCoriolisScale = x } })
    , FloatLeaf fieldWindDrag FiniteNonNegative
        (cpWindDrag . wgpClimateParams)
        (\x p → p { wgpClimateParams =
                        (wgpClimateParams p) { cpWindDrag = x } })
    , FloatLeaf fieldThermalInertia (InRange unitIntervalMin unitIntervalMax)
        (cpThermalInertia . wgpClimateParams)
        (\x p → p { wgpClimateParams =
                        (wgpClimateParams p) { cpThermalInertia = x } })
    , FloatLeaf fieldOrographicScale FiniteNonNegative
        (cpOrographicScale . wgpClimateParams)
        (\x p → p { wgpClimateParams =
                        (wgpClimateParams p) { cpOrographicScale = x } })
    , FloatLeaf fieldEvapScale FiniteNonNegative
        (cpEvapScale . wgpClimateParams)
        (\x p → p { wgpClimateParams =
                        (wgpClimateParams p) { cpEvapScale = x } })
    , FloatLeaf fieldAlbedoFeedback FiniteNonNegative
        (cpAlbedoFeedback . wgpClimateParams)
        (\x p → p { wgpClimateParams =
                        (wgpClimateParams p) { cpAlbedoFeedback = x } })
    , FloatLeaf fieldThcThreshold AnyFinite
        (cpThcThreshold . wgpClimateParams)
        (\x p → p { wgpClimateParams =
                        (wgpClimateParams p) { cpThcThreshold = x } })
    ]

worldGenParamsRejections ∷ WorldGenParams → [WorldGenFieldRejection]
worldGenParamsRejections = floatRejections paramsFloatLeaves

-- | The save-side repair: a stored setting outside its domain is
--   replaced by the shipped default and reported, every sibling
--   setting survives, and nothing else about the params is touched.
repairWorldGenParams
    ∷ WorldGenParams → (WorldGenParams, [(WorldGenFieldRejection, Text)])
repairWorldGenParams = repairFloats paramsFloatLeaves defaultWorldGenParams

-- * The raw YAML document

-- | Apply the domain to a structurally valid document, leaf by leaf
--   (#2288), mirroring the video loader's own resolution: every
--   out-of-domain leaf takes its value from 'defaultWorldGenConfig' and
--   is reported, every other leaf survives unchanged.
--
--   Each rejection quotes the number as the FILE spelled it — @1e40@,
--   @.inf@ — rather than the 'Float' it narrowed to, which is the whole
--   reason the raw records carry a 'NumberSource'. Each is paired with
--   the rendering of the default that replaced it, for the loader's log
--   line. Pure, so a spec can pin it without a logger.
resolveWorldGenConfigRaw
    ∷ WorldGenConfigRaw → (WorldGenConfig, [(WorldGenFieldRejection, Text)])
resolveWorldGenConfigRaw raw = (config, map asSource rejections)
  where
    (config, rejections) = repairWorldGenConfig narrowed
    -- Report the number as the file spelled it, not what it narrowed to.
    asSource (r, dflt) = case lookup (wgrField r) sourceTexts of
        Just t  → (r { wgrValue = t }, dflt)
        Nothing → (r, dflt)
    sourceTexts =
        [ (fieldErosionIntensity, nsText (wcrErosionIntensity raw))
        , (fieldVolcanicActivity, nsText (wcrVolcanicActivity raw))
        , (fieldTiltAngle,        nsText (sunrTiltAngle sun))
        , (fieldDayLength,        nsText (sunrDayLength sun))
        , (fieldPhaseOffset,      nsText (moonrPhaseOffset moon))
        , (fieldOreAbundance,     nsText (resrOreAbundance res))
        , (fieldIronAbundance,    nsText (resrIronAbundance res))
        , (fieldCopperAbundance,  nsText (resrCopperAbundance res))
        , (fieldCoriolisScale,    nsText (climrCoriolisScale clim))
        , (fieldWindDrag,         nsText (climrWindDrag clim))
        , (fieldThermalInertia,   nsText (climrThermalInertia clim))
        , (fieldOrographicScale,  nsText (climrOrographicScale clim))
        , (fieldEvapScale,        nsText (climrEvapScale clim))
        , (fieldAlbedoFeedback,   nsText (climrAlbedoFeedback clim))
        , (fieldThcThreshold,     nsText (climrThcThreshold clim))
        ]
    sun  = wcrSun raw
    moon = wcrMoon raw
    clim = wcrClimate raw
    res  = wcrResources raw
    narrow = narrowWorldGenFloat . nsValue
    narrowed = WorldGenConfig
        { wgcSeed       = wcrSeed raw
        , wgcWorldSize  = wcrWorldSize raw
        , wgcPlateCount = wcrPlateCount raw
        , wgcCalendar   = wcrCalendar raw
        , wgcSun        = SunYaml (narrow (sunrTiltAngle sun))
                                  (narrow (sunrDayLength sun))
        , wgcMoon       = MoonYaml (moonrCycleDays moon)
                                   (narrow (moonrPhaseOffset moon))
        , wgcClimate    = ClimateYaml
            { clIterations      = climrIterations clim
            , clCoriolisScale   = narrow (climrCoriolisScale clim)
            , clWindDrag        = narrow (climrWindDrag clim)
            , clThermalInertia  = narrow (climrThermalInertia clim)
            , clOrographicScale = narrow (climrOrographicScale clim)
            , clEvapScale       = narrow (climrEvapScale clim)
            , clAlbedoFeedback  = narrow (climrAlbedoFeedback clim)
            , clThcThreshold    = narrow (climrThcThreshold clim)
            }
        , wgcErosionIntensity = narrow (wcrErosionIntensity raw)
        , wgcVolcanicActivity = narrow (wcrVolcanicActivity raw)
        , wgcLavaPoolDepth    = wcrLavaPoolDepth raw
        , wgcLavaPoolRadius   = wcrLavaPoolRadius raw
        , wgcWaterfallQuantum = wcrWaterfallQuantum raw
        , wgcResources  = ResourcesYaml (narrow (resrOreAbundance res))
                                        (narrow (resrIronAbundance res))
                                        (narrow (resrCopperAbundance res))
        , wgcTimeline   = wcrTimeline raw
        }
