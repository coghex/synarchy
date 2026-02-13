{-# LANGUAGE Strict, UnicodeSyntax #-}
module World.Geology.Timeline
    ( buildTimeline
    ) where

import UPrelude
import Data.Bits (xor)
import Data.Word (Word64)
import Data.List (foldl')
import qualified Data.Text as T
import World.Types
import World.Plate (generatePlates, TectonicPlate)
import World.Geology.Types
import World.Geology.Hash
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

-----------------------------------------------------------
-- Top Level
-----------------------------------------------------------

buildTimeline ∷ Word64 → Int → Int → GeoTimeline
buildTimeline seed worldSize plateCount =
    let plates = generatePlates seed worldSize plateCount
        gs0 = initGeoState seed worldSize plates

        tbs0 = TimelineBuildState
            { tbsFeatures  = []
            , tbsNextId    = 0
            , tbsPeriods   = []
            , tbsPeriodIdx = 0
            , tbsGeoState  = gs0
            }

        -- Primordial bombardment stands alone, before the eon
        s1 = buildPrimordialBombardment seed worldSize plates tbs0

        -- The single development eon
        s2 = buildEon seed worldSize plates s1

    in GeoTimeline
        { gtSeed      = seed
        , gtWorldSize = worldSize
        , gtPeriods   = reverse (tbsPeriods s2)
        , gtFeatures  = tbsFeatures s2
        }

-----------------------------------------------------------
-- Primordial Bombardment (standalone, pre-eon)
-----------------------------------------------------------

buildPrimordialBombardment ∷ Word64 → Int → [TectonicPlate]
                           → TimelineBuildState → TimelineBuildState
buildPrimordialBombardment seed worldSize plates tbs =
    let craterSeed = seed `xor` 0xDEADBEEF
        craters = generateCraters craterSeed worldSize plates CraterEra_Primordial
        gs = tbsGeoState tbs
        gs' = gs { gsDate = advanceGeoDate 500.0 (gsDate gs) }
        period = GeoPeriod
            { gpName     = "Primordial Bombardment"
            , gpScale    = Eon
            , gpDuration = 500
            , gpEvents   = map CraterEvent craters
            , gpErosion  = ErosionParams 0.8 0.3 0.6 0.4 0.1 (seed + 1000)
            }
    in addPeriod period (tbs { tbsGeoState = gs' })

-----------------------------------------------------------
-- Eon
-----------------------------------------------------------

-- | The single eon of geological development.
--   Calls buildEra recursively, min 2, max 4.
buildEon ∷ Word64 → Int → [TectonicPlate]
         → TimelineBuildState → TimelineBuildState
buildEon seed worldSize plates tbs =
    buildEraLoop seed worldSize plates 0 2 4 tbs

buildEraLoop ∷ Word64 → Int → [TectonicPlate]
             → Int → Int → Int
             → TimelineBuildState → TimelineBuildState
buildEraLoop seed worldSize plates eraIdx minEras maxEras tbs
    | eraIdx ≥ maxEras = tbs
    | otherwise =
        let eraSeed = seed `xor` (fromIntegral eraIdx * 0xA1B2C3D4)
            s1 = buildEra eraSeed worldSize plates eraIdx tbs

            -- Roll for continuation after meeting minimum
            roll = hashToFloatGeo (hashGeo eraSeed eraIdx 300)
            continue = eraIdx < (minEras - 1) ∨ roll < 0.5
        in if continue
           then buildEraLoop seed worldSize plates (eraIdx + 1) minEras maxEras s1
           else s1

-----------------------------------------------------------
-- Era
-----------------------------------------------------------

-- | An era of geological history.
--   Era-level events: flood basalts, massive impacts,
--   continental-scale changes.
--   Then recurses into periods.
buildEra ∷ Word64 → Int → [TectonicPlate] → Int
         → TimelineBuildState → TimelineBuildState
buildEra seed worldSize plates eraIdx tbs =
    let eraSeed = seed `xor` (fromIntegral eraIdx * 0xE1A2)
        gs = tbsGeoState tbs

        -- Era-level events
        -- TODO: flood basalts, massive impacts, LIPs
        -- For now, placeholder: just advance date and modify state
        eraEvents = []

        -- Era events get their own GeoPeriod
        gs' = gs { gsDate = advanceGeoDate 100.0 (gsDate gs) }
        eraPeriod = GeoPeriod
            { gpName     = "Era " <> T.pack (show eraIdx) <> " Events"
            , gpScale    = Era
            , gpDuration = 100
            , gpEvents   = eraEvents
            , gpErosion  = ErosionParams 0.7 0.5 0.5 0.3 0.2 (seed + 3000 + fromIntegral eraIdx)
            }
        s1 = addPeriod eraPeriod (tbs { tbsGeoState = gs' })

        -- Recurse into periods
        s2 = buildPeriodLoop eraSeed worldSize plates 0 2 4 s1

    in s2

-----------------------------------------------------------
-- Period
-----------------------------------------------------------

buildPeriodLoop ∷ Word64 → Int → [TectonicPlate]
                → Int → Int → Int
                → TimelineBuildState → TimelineBuildState
buildPeriodLoop seed worldSize plates periodIdx minPeriods maxPeriods tbs
    | periodIdx ≥ maxPeriods = tbs
    | otherwise =
        let periodSeed = seed `xor` (fromIntegral periodIdx * 0xB3C4D5E6)
            s1 = buildPeriod periodSeed worldSize plates periodIdx tbs

            roll = hashToFloatGeo (hashGeo periodSeed periodIdx 400)
            continue = periodIdx < (minPeriods - 1) ∨ roll < 0.5
        in if continue
           then buildPeriodLoop seed worldSize plates (periodIdx + 1) minPeriods maxPeriods s1
           else s1

-- | A period of geological history.
--   Period-level events: volcanism, tectonic changes,
--   volcanic evolution.
--   Then recurses into epochs.
buildPeriod ∷ Word64 → Int → [TectonicPlate] → Int
            → TimelineBuildState → TimelineBuildState
buildPeriod seed worldSize plates periodIdx tbs =
    let periodSeed = seed `xor` (fromIntegral periodIdx * 0xF1E2)
        gs = tbsGeoState tbs

        -- Period-level events: volcanism
        -- Which volcanic features to generate depends on the period
        -- and GeoState (e.g., high CO2 = more volcanic activity)
        s1 = applyPeriodVolcanism periodSeed worldSize plates periodIdx tbs

        -- Volcanic evolution of existing features
        s2 = applyVolcanicEvolution periodSeed s1

        -- Recurse into epochs
        s3 = buildEpochLoop periodSeed worldSize plates 0 2 6 s2

    in s3

applyPeriodVolcanism ∷ Word64 → Int → [TectonicPlate] → Int
                     → TimelineBuildState → TimelineBuildState
applyPeriodVolcanism seed worldSize plates periodIdx tbs =
    let gs = tbsGeoState tbs
        volcSeed = seed `xor` 0xB45A1F1C
        pIdx = tbsPeriodIdx tbs
        activityLevel = gsCO2 gs

        -- Shield volcanoes: rare, only when activity is high
        (shields, tbs1) = if activityLevel > 0.8
            then generateAndRegisterN 8 2 volcSeed worldSize plates
                     VolcanoEra_Hotspot generateShieldVolcano pIdx tbs
            else ([], tbs)

        -- Fissures: 1-2 per period
        (fissures, tbs2) = generateAndRegisterN 6 2 (volcSeed + 1) worldSize plates
                               VolcanoEra_Boundary generateFissure pIdx tbs1

        -- Cinder cones: 2-3 per period
        (cinders, tbs3) = generateAndRegisterN 10 3 (volcSeed + 2) worldSize plates
                              VolcanoEra_Boundary generateCinderCone pIdx tbs2

        -- Hydrothermal vents: 1-2 per period
        (vents, tbs4) = generateAndRegisterN 6 2 (volcSeed + 3) worldSize plates
                            VolcanoEra_Boundary generateHydrothermalVent pIdx tbs3

        -- Supervolcano: only attempt on the first period,
        -- guarantee at least one
        hasSuperVolcano = any isSuperVolcano (tbsFeatures tbs4)
        (supers, tbs5) = if periodIdx ≡ 0 ∨ (periodIdx ≡ 1 ∧ not hasSuperVolcano)
            then let (s, t) = generateAndRegisterN 12 1 (volcSeed + 4) worldSize plates
                                  VolcanoEra_Hotspot generateSuperVolcano pIdx tbs4
                 in if null s ∧ not hasSuperVolcano
                    -- Force one: just pick a random land position and make it work
                    then forceOneSuperVolcano (volcSeed + 5) worldSize plates pIdx t
                    else (s, t)
            else ([], tbs4)

        allNew = shields <> fissures <> cinders <> vents <> supers
        events = map (\pf → VolcanicEvent (pfFeature pf)) allNew

        gs' = (tbsGeoState tbs5)
            { gsCO2 = gsCO2 (tbsGeoState tbs5) + fromIntegral (length allNew) * 0.01
            }

        period = GeoPeriod
            { gpName     = "Volcanism " <> T.pack (show periodIdx)
            , gpScale    = Period
            , gpDuration = 50
            , gpEvents   = events
            , gpErosion  = ErosionParams 0.5 0.5 0.4 0.2 0.3 (seed + 4000)
            }
    in addPeriod period (tbs5 { tbsGeoState = gs' })

-- | Check if a feature is a supervolcano.
isSuperVolcano ∷ PersistentFeature → Bool
isSuperVolcano pf = case pfFeature pf of
    SuperVolcano _ → True
    _              → False

-- | Force-place a supervolcano by trying many positions.
forceOneSuperVolcano ∷ Word64 → Int → [TectonicPlate] → Int
                     → TimelineBuildState
                     → ([PersistentFeature], TimelineBuildState)
forceOneSuperVolcano seed worldSize plates periodIdx tbs =
    let halfTiles = (worldSize * 16) `div` 2
        go attempt
            | attempt ≥ 100 = ([], tbs)  -- give up after 100 attempts
            | otherwise =
                let h1 = hashGeo seed attempt 170
                    h2 = hashGeo seed attempt 171
                    gx = hashToRangeGeo h1 (-halfTiles) (halfTiles - 1)
                    gy = hashToRangeGeo h2 (-halfTiles) (halfTiles - 1)
                in case generateSuperVolcano seed worldSize plates gx gy of
                    Nothing → go (attempt + 1)
                    Just feature →
                        let (fid, tbs') = allocFeatureId tbs
                            pf = PersistentFeature
                                { pfId               = fid
                                , pfFeature          = feature
                                , pfActivity         = Active
                                , pfFormationPeriod   = periodIdx
                                , pfLastActivePeriod  = periodIdx
                                , pfEruptionCount     = 1
                                , pfParentId          = Nothing
                                }
                            tbs'' = registerFeature pf tbs'
                        in ([pf], tbs'')
    in go 0

-- | Evolve existing volcanic features.
applyVolcanicEvolution ∷ Word64 → TimelineBuildState → TimelineBuildState
applyVolcanicEvolution seed tbs =
    let periodIdx = tbsPeriodIdx tbs
        evolSeed = seed `xor` 0xEF01F100

        (events, tbs1) = foldl' (evolveOneFeature evolSeed periodIdx)
                                ([], tbs) (tbsFeatures tbs)

        period = GeoPeriod
            { gpName     = "Volcanic Evolution"
            , gpScale    = Period
            , gpDuration = 30
            , gpEvents   = events
            , gpErosion  = ErosionParams 0.5 0.5 0.4 0.2 0.3 (seed + 5000)
            }
    in if null events then tbs1
       else addPeriod period tbs1

-----------------------------------------------------------
-- Epoch
-----------------------------------------------------------

buildEpochLoop ∷ Word64 → Int → [TectonicPlate]
               → Int → Int → Int
               → TimelineBuildState → TimelineBuildState
buildEpochLoop seed worldSize plates epochIdx minEpochs maxEpochs tbs
    | epochIdx ≥ maxEpochs = tbs
    | otherwise =
        let epochSeed = seed `xor` (fromIntegral epochIdx * 0xC5D6E7F8)
            s1 = buildEpoch epochSeed worldSize plates epochIdx tbs

            roll = hashToFloatGeo (hashGeo epochSeed epochIdx 500)
            continue = epochIdx < (minEpochs - 1) ∨ roll < 0.5
        in if continue
           then buildEpochLoop seed worldSize plates (epochIdx + 1) minEpochs maxEpochs s1
           else s1

-- | An epoch of geological history.
--   Epoch-level events: glaciation, major river erosion,
--   sea level changes.
--   Then recurses into ages.
buildEpoch ∷ Word64 → Int → [TectonicPlate] → Int
           → TimelineBuildState → TimelineBuildState
buildEpoch seed worldSize plates epochIdx tbs =
    let epochSeed = seed `xor` (fromIntegral epochIdx * 0xA1A2)
        gs = tbsGeoState tbs

        -- Epoch-level events: glaciation if temperature is low enough
        -- TODO: glaciation events based on regional temperature
        -- TODO: river system establishment
        epochEvents = []

        gs' = gs { gsDate = advanceGeoDate 20.0 (gsDate gs) }

        -- Only emit a period if there are epoch-level events
        s1 = if null epochEvents then tbs { tbsGeoState = gs' }
             else let period = GeoPeriod
                          { gpName     = "Epoch " <> T.pack (show epochIdx)
                          , gpScale    = Epoch
                          , gpDuration = 20
                          , gpEvents   = epochEvents
                          , gpErosion  = ErosionParams 0.6 0.7 0.3 0.2 0.4 (seed + 6000)
                          }
                  in addPeriod period (tbs { tbsGeoState = gs' })

        -- Recurse into ages
        s2 = buildAgeLoop epochSeed worldSize plates 0 1 8 s1

    in s2

-----------------------------------------------------------
-- Age
-----------------------------------------------------------

buildAgeLoop ∷ Word64 → Int → [TectonicPlate]
             → Int → Int → Int
             → TimelineBuildState → TimelineBuildState
buildAgeLoop seed worldSize plates ageIdx minAges maxAges tbs
    | ageIdx ≥ maxAges = tbs
    | otherwise =
        let ageSeed = seed `xor` (fromIntegral ageIdx * 0xD7E8F9A0)
            s1 = buildAge ageSeed worldSize plates ageIdx tbs

            roll = hashToFloatGeo (hashGeo ageSeed ageIdx 600)
            continue = ageIdx < (minAges - 1) ∨ roll < 0.4
        in if continue
           then buildAgeLoop seed worldSize plates (ageIdx + 1) minAges maxAges s1
           else s1

-- | A single age — the finest granularity of the timeline.
--   Always has erosion. May have small discrete events.
--   Duration varies, which affects event probability.
buildAge ∷ Word64 → Int → [TectonicPlate] → Int
         → TimelineBuildState → TimelineBuildState
buildAge seed worldSize plates ageIdx tbs =
    let ageSeed = seed `xor` (fromIntegral ageIdx * 0xF0F1)
        gs = tbsGeoState tbs

        -- Determine age duration (millions of years)
        -- Varies from 1 to 15 MY
        durationHash = hashGeo ageSeed ageIdx 610
        duration = 1.0 + hashToFloatGeo durationHash * 14.0 ∷ Float

        -- Advance the date
        gs1 = gs { gsDate = advanceGeoDate duration (gsDate gs) }

        -- Event generation, probability scaled by duration
        -- Longer ages = more likely to have events

        -- Meteorite impacts
        -- Base rate: roughly 1 per 20 MY for the whole world
        meteoriteRoll = hashToFloatGeo (hashGeo ageSeed ageIdx 620)
        meteoriteChance = min 0.8 (duration / 20.0)
        meteorites = if meteoriteRoll < meteoriteChance
            then let craterSeed = ageSeed `xor` 0xBEEF
                     craters = generateCraters craterSeed worldSize plates CraterEra_Late
                 -- Take just 1-2 craters for a single age impact
                 in take (hashToRangeGeo (hashGeo ageSeed ageIdx 621) 1 3)
                         (map CraterEvent craters)
            else []

        -- Landslides
        -- TODO: generate based on steep terrain + precipitation
        landslideRoll = hashToFloatGeo (hashGeo ageSeed ageIdx 630)
        landslideChance = duration / 10.0
        landslides = if landslideRoll < landslideChance
            then []  -- TODO: generateLandslides
            else []

        -- Floods
        floodRoll = hashToFloatGeo (hashGeo ageSeed ageIdx 640)
        floodChance = duration / 20.0
        floods = if floodRoll < floodChance
            then []  -- TODO: generateFloods
            else []

        allEvents = meteorites <> landslides <> floods

        -- CO2 slowly decays toward baseline through weathering
        gs2 = gs1 { gsCO2 = max 0.5 (gsCO2 gs1 - duration * 0.005) }

        -- Erosion params derived from current GeoState
        erosion = erosionFromGeoState gs2 seed ageIdx

        period = GeoPeriod
            { gpName     = "Age " <> T.pack (show (tbsPeriodIdx tbs))
            , gpScale    = Age
            , gpDuration = round duration
            , gpEvents   = allEvents
            , gpErosion  = erosion
            }
    in addPeriod period (tbs { tbsGeoState = gs2 })

-- | Derive erosion parameters from current GeoState.
--   Higher CO2 = more chemical weathering.
--   This is where GeoState feeds into the per-tile application.
erosionFromGeoState ∷ GeoState → Word64 → Int → ErosionParams
erosionFromGeoState gs seed ageIdx =
    let co2 = gsCO2 gs
        -- Higher CO2 increases chemical weathering
        chemical = min 1.0 (0.2 + (co2 - 1.0) * 0.3)
    in ErosionParams
        { epIntensity = 0.5
        , epHydraulic = 0.5
        , epThermal   = 0.3
        , epWind      = 0.2
        , epChemical  = chemical
        , epSeed      = seed + fromIntegral ageIdx * 7
        }
