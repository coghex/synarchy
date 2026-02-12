{-# LANGUAGE Strict, UnicodeSyntax #-}

module World.Geology.Timeline
    ( buildTimeline
    , buildPrimordialBombardment
    , buildLateBombardment
    , buildEarlyVolcanism
    , buildVolcanicEvolution
    , buildLateVolcanism
    , buildStabilization
    ) where

import UPrelude
import Data.Bits (xor)
import Data.Word (Word64)
import Data.List (foldl')
import World.Types
import World.Plate (generatePlates, TectonicPlate)
import World.Geology.Types
import World.Geology.Crater (generateCraters)
import World.Geology.Generate
    ( generateShieldVolcano
    , generateCinderCone
    , generateLavaDome
    , generateCaldera
    , generateFissure
    , generateLavaTube
    , generateSuperVolcano
    , generateHydrothermalVent
    , generateAndRegister
    , generateAndRegisterN
    )
import World.Geology.Evolution (evolveOneFeature)

-- | VolcanoEra determines where and what type of volcanoes form.
data VolcanoEra
    = VolcanoEra_Boundary    -- ^ Tied to convergent boundaries
    | VolcanoEra_Hotspot     -- ^ Random placement on land
    deriving (Show, Eq)

buildTimeline ∷ Word64 → Int → Int → GeoTimeline
buildTimeline seed worldSize plateCount =
    let plates = generatePlates seed worldSize plateCount

        initialState = TimelineBuildState
            { tbsFeatures  = []
            , tbsNextId    = 0
            , tbsPeriods   = []
            , tbsPeriodIdx = 0
            }

        s1 = buildPrimordialBombardment seed worldSize plates initialState
        s2 = buildLateBombardment seed worldSize plates s1
        s3 = buildEarlyVolcanism seed worldSize plates s2
        s4 = buildVolcanicEvolution seed worldSize plates s3
        s5 = buildLateVolcanism seed worldSize plates s4
        s6 = buildStabilization seed s5

    in GeoTimeline
        { gtSeed      = seed
        , gtWorldSize = worldSize
        , gtPeriods   = reverse (tbsPeriods s6)
        , gtFeatures  = tbsFeatures s6
        }

buildPrimordialBombardment ∷ Word64 → Int → [TectonicPlate]
                           → TimelineBuildState → TimelineBuildState
buildPrimordialBombardment seed worldSize plates tbs =
    let craterSeed = seed `xor` 0xDEADBEEF
        craters = generateCraters craterSeed worldSize plates CraterEra_Primordial
        period = GeoPeriod
            { gpName     = "Primordial Bombardment"
            , gpScale    = Eon
            , gpDuration = 100
            , gpEvents   = map CraterEvent craters
            , gpErosion  = ErosionParams 0.8 0.3 0.6 0.4 0.1 (seed + 1000)
            }
    in addPeriod period tbs

buildLateBombardment ∷ Word64 → Int → [TectonicPlate]
                     → TimelineBuildState → TimelineBuildState
buildLateBombardment seed worldSize plates tbs =
    let craterSeed = (seed `xor` 0xDEADBEEF) + 1
        craters = generateCraters craterSeed worldSize plates CraterEra_Late
        period = GeoPeriod
            { gpName     = "Late Bombardment"
            , gpScale    = Era
            , gpDuration = 50
            , gpEvents   = map CraterEvent craters
            , gpErosion  = ErosionParams 0.6 0.5 0.4 0.2 0.3 (seed + 2000)
            }
    in addPeriod period tbs

buildEarlyVolcanism ∷ Word64 → Int → [TectonicPlate]
                    → TimelineBuildState → TimelineBuildState
buildEarlyVolcanism seed worldSize plates tbs0 =
    let volcSeed = seed `xor` 0xB45A1F1C
        periodIdx = fromIntegral $ tbsPeriodIdx tbs0

        -- Shield volcanoes at hotspots
        (shields, tbs1) = generateAndRegister volcSeed worldSize plates
                              VolcanoEra_Hotspot generateShieldVolcano periodIdx tbs0

        -- Fissures at rift zones
        (fissures, tbs2) = generateAndRegister (volcSeed + 1) worldSize plates
                               VolcanoEra_Boundary generateFissure periodIdx tbs1

        -- Hydrothermal vents on ocean floor
        (vents, tbs3) = generateAndRegister (volcSeed + 2) worldSize plates
                            VolcanoEra_Boundary generateHydrothermalVent periodIdx tbs2

        events = map (\pf → VolcanicEvent (pfFeature pf)) (shields <> fissures <> vents)

        period = GeoPeriod
            { gpName     = "Early Volcanism"
            , gpScale    = Era
            , gpDuration = 70
            , gpEvents   = events
            , gpErosion  = ErosionParams 0.6 0.6 0.4 0.2 0.3 (seed + 4000)
            }
    in addPeriod period tbs3

buildVolcanicEvolution ∷ Word64 → Int → [TectonicPlate]
                       → TimelineBuildState → TimelineBuildState
buildVolcanicEvolution seed _worldSize _plates tbs0 =
    let periodIdx = tbsPeriodIdx tbs0
        evolSeed = seed `xor` 0xEF01F100

        (events, tbs1) = foldl' (evolveOneFeature evolSeed periodIdx)
                                ([], tbs0) (tbsFeatures tbs0)

        period = GeoPeriod
            { gpName     = "Volcanic Evolution"
            , gpScale    = Period
            , gpDuration = 50
            , gpEvents   = events
            , gpErosion  = ErosionParams 0.5 0.5 0.4 0.2 0.3 (seed + 5000)
            }
    in addPeriod period tbs1

buildLateVolcanism ∷ Word64 → Int → [TectonicPlate]
                   → TimelineBuildState → TimelineBuildState
buildLateVolcanism seed worldSize plates tbs0 =
    let lateSeed = seed `xor` 0x1A7EF10D
        periodIdx = tbsPeriodIdx tbs0

        -- Cinder cones (common, small)
        (cinders, tbs1) = generateAndRegister lateSeed worldSize plates
                              VolcanoEra_Boundary generateCinderCone periodIdx tbs0

        -- Lava domes at convergent boundaries
        (domes, tbs2) = generateAndRegister (lateSeed + 1) worldSize plates
                            VolcanoEra_Boundary generateLavaDome periodIdx tbs1

        -- Lava tubes (on existing volcanic terrain)
        (tubes, tbs3) = generateAndRegister (lateSeed + 2) worldSize plates
                            VolcanoEra_Hotspot generateLavaTube periodIdx tbs2

        -- Rare: one supervolcano attempt
        (supers, tbs4) = generateAndRegisterN 8 1 (lateSeed + 3) worldSize plates
                             VolcanoEra_Hotspot generateSuperVolcano periodIdx tbs3

        -- Calderas from collapsed earlier features
        (calderas, tbs5) = generateAndRegisterN 16 3 (lateSeed + 4) worldSize plates
                               VolcanoEra_Hotspot generateCaldera periodIdx tbs4

        allNew = cinders <> domes <> tubes <> supers <> calderas
        events = map (\pf → VolcanicEvent (pfFeature pf)) allNew

        period = GeoPeriod
            { gpName     = "Late Volcanism"
            , gpScale    = Period
            , gpDuration = 40
            , gpEvents   = events
            , gpErosion  = ErosionParams 0.3 0.4 0.3 0.2 0.2 (seed + 6000)
            }
    in addPeriod period tbs5

buildStabilization ∷ Word64
                   → TimelineBuildState → TimelineBuildState
buildStabilization seed tbs =
    let period = GeoPeriod
            { gpName     = "Crustal Stabilization"
            , gpScale    = Period
            , gpDuration = 80
            , gpEvents   = []
            , gpErosion  = ErosionParams 0.9 0.8 0.3 0.2 0.5 (seed + 7000)
            }
    in addPeriod period tbs
