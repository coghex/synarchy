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
import World.Plate (generatePlates, TectonicPlate, elevationAtGlobal)
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
        currentDate = gdMillionYears (gsDate gs)
        gs' = gs { gsDate = advanceGeoDate 500.0 (gsDate gs) }
        period = GeoPeriod
            { gpName     = "Primordial Bombardment"
            , gpScale    = Eon
            , gpDuration = 500
            , gpDate     = currentDate
            , gpEvents   = map CraterEvent craters
            , gpErosion  = ErosionParams 0.8 0.3 0.6 0.4 0.1 (seed + 1000)
            }
    in addPeriod period (tbs { tbsGeoState = gs' })

-----------------------------------------------------------
-- Eon
-----------------------------------------------------------

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

            roll = hashToFloatGeo (hashGeo eraSeed eraIdx 300)
            continue = eraIdx < (minEras - 1) ∨ roll < 0.5
        in if continue
           then buildEraLoop seed worldSize plates (eraIdx + 1) minEras maxEras s1
           else s1

-----------------------------------------------------------
-- Era
-----------------------------------------------------------

buildEra ∷ Word64 → Int → [TectonicPlate] → Int
         → TimelineBuildState → TimelineBuildState
buildEra seed worldSize plates eraIdx tbs =
    let eraSeed = seed `xor` (fromIntegral eraIdx * 0xE1A2)
        gs = tbsGeoState tbs
        currentDate = gdMillionYears (gsDate gs)

        eraEvents = []

        gs' = gs { gsDate = advanceGeoDate 100.0 (gsDate gs) }
        eraPeriod = GeoPeriod
            { gpName     = "Era " <> T.pack (show eraIdx) <> " Events"
            , gpScale    = Era
            , gpDuration = 100
            , gpDate     = currentDate
            , gpEvents   = eraEvents
            , gpErosion  = ErosionParams 0.7 0.5 0.5 0.3 0.2 (seed + 3000 + fromIntegral eraIdx)
            }
        s1 = addPeriod eraPeriod (tbs { tbsGeoState = gs' })

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

buildPeriod ∷ Word64 → Int → [TectonicPlate] → Int
            → TimelineBuildState → TimelineBuildState
buildPeriod seed worldSize plates periodIdx tbs =
    let periodSeed = seed `xor` (fromIntegral periodIdx * 0xF1E2)

        s1 = applyPeriodVolcanism periodSeed worldSize plates periodIdx tbs

        s2 = applyVolcanicEvolution periodSeed worldSize plates s1

        s3 = buildEpochLoop periodSeed worldSize plates 0 2 6 s2

    in s3

applyPeriodVolcanism ∷ Word64 → Int → [TectonicPlate] → Int
                     → TimelineBuildState → TimelineBuildState
applyPeriodVolcanism seed worldSize plates periodIdx tbs =
    let gs = tbsGeoState tbs
        volcSeed = seed `xor` 0xB45A1F1C
        pIdx = tbsPeriodIdx tbs
        activityLevel = gsCO2 gs
        currentDate = gdMillionYears (gsDate gs)

        (shields, tbs1) = generateAndRegisterN 8 2 volcSeed worldSize plates
                              VolcanoEra_Hotspot generateShieldVolcano pIdx tbs

        (fissures, tbs2) = generateAndRegisterN 6 2 (volcSeed + 1) worldSize plates
                               VolcanoEra_Boundary generateFissure pIdx tbs1

        (cinders, tbs3) = generateAndRegisterN 10 3 (volcSeed + 2) worldSize plates
                              VolcanoEra_Boundary generateCinderCone pIdx tbs2

        (vents, tbs4) = generateAndRegisterN 6 2 (volcSeed + 3) worldSize plates
                            VolcanoEra_Boundary generateHydrothermalVent pIdx tbs3

        hasSuperVolcano = any isSuperVolcano (tbsFeatures tbs4)
        (supers, tbs5) = if periodIdx ≡ 0 ∨ (periodIdx ≡ 1 ∧ not hasSuperVolcano)
            then let (s, t) = generateAndRegisterN 12 1 (volcSeed + 4) worldSize plates
                                  VolcanoEra_Hotspot generateSuperVolcano pIdx tbs4
                 in if null s ∧ not hasSuperVolcano
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
            , gpDate     = currentDate
            , gpEvents   = events
            , gpErosion  = ErosionParams 0.5 0.5 0.4 0.2 0.3 (seed + 4000)
            }
    in addPeriod period (tbs5 { tbsGeoState = gs' })

isSuperVolcano ∷ PersistentFeature → Bool
isSuperVolcano pf = case pfFeature pf of
    SuperVolcano _ → True
    _              → False

forceOneSuperVolcano ∷ Word64 → Int → [TectonicPlate] → Int
                     → TimelineBuildState
                     → ([PersistentFeature], TimelineBuildState)
forceOneSuperVolcano seed worldSize plates periodIdx tbs =
    let halfTiles = (worldSize * 16) `div` 2
        go attempt
            | attempt ≥ 100 = ([], tbs)
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

applyVolcanicEvolution ∷ Word64 → Int → [TectonicPlate]
                       → TimelineBuildState → TimelineBuildState
applyVolcanicEvolution seed worldSize plates tbs =
    let periodIdx = tbsPeriodIdx tbs
        evolSeed = seed `xor` 0xEF01F100
        currentDate = gdMillionYears (gsDate (tbsGeoState tbs))

        (events, tbs1) = foldl' (evolveOneFeature evolSeed periodIdx)
                                ([], tbs) (tbsFeatures tbs)

        eruptSeed = seed `xor` 0x5E7A
        periodEruptions = catMaybes
            [ generateEruption eruptSeed worldSize periodIdx plates pf
            | pf ← tbsFeatures tbs1
            , case eruptionProfile (pfFeature pf) of
                Just ep → epTimelineScale ep ≡ Period
                Nothing → False
            ]

        allEvents = events <> periodEruptions

        period = GeoPeriod
            { gpName     = "Volcanic Evolution"
            , gpScale    = Period
            , gpDuration = 30
            , gpDate     = currentDate
            , gpEvents   = allEvents
            , gpErosion  = ErosionParams 0.5 0.5 0.4 0.2 0.3 (seed + 5000)
            }
    in if null allEvents then tbs1
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

buildEpoch ∷ Word64 → Int → [TectonicPlate] → Int
           → TimelineBuildState → TimelineBuildState
buildEpoch seed worldSize plates epochIdx tbs =
    let epochSeed = seed `xor` (fromIntegral epochIdx * 0xA1A2)
        gs = tbsGeoState tbs
        currentDate = gdMillionYears (gsDate gs)

        epochEvents = []

        gs' = gs { gsDate = advanceGeoDate 20.0 (gsDate gs) }

        s1 = if null epochEvents then tbs { tbsGeoState = gs' }
             else let period = GeoPeriod
                          { gpName     = "Epoch " <> T.pack (show epochIdx)
                          , gpScale    = Epoch
                          , gpDuration = 20
                          , gpDate     = currentDate
                          , gpEvents   = epochEvents
                          , gpErosion  = ErosionParams 0.6 0.7 0.3 0.2 0.4 (seed + 6000)
                          }
                  in addPeriod period (tbs { tbsGeoState = gs' })

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

buildAge ∷ Word64 → Int → [TectonicPlate] → Int
         → TimelineBuildState → TimelineBuildState
buildAge seed worldSize plates ageIdx tbs =
    let ageSeed = seed `xor` (fromIntegral ageIdx * 0xF0F1)
        gs = tbsGeoState tbs
        currentDate = gdMillionYears (gsDate gs)

        durationHash = hashGeo ageSeed ageIdx 610
        duration = 1.0 + hashToFloatGeo durationHash * 14.0 ∷ Float

        gs1 = gs { gsDate = advanceGeoDate duration (gsDate gs) }

        meteoriteRoll = hashToFloatGeo (hashGeo ageSeed ageIdx 620)
        meteoriteChance = min 0.8 (duration / 20.0)
        meteorites = if meteoriteRoll < meteoriteChance
            then let craterSeed = ageSeed `xor` 0xBEEF
                     craters = generateCraters craterSeed worldSize plates CraterEra_Late
                 in take (hashToRangeGeo (hashGeo ageSeed ageIdx 621) 1 3)
                         (map CraterEvent craters)
            else []

        landslideRoll = hashToFloatGeo (hashGeo ageSeed ageIdx 630)
        landslideChance = duration / 10.0
        landslides = if landslideRoll < landslideChance
            then []
            else []

        floodRoll = hashToFloatGeo (hashGeo ageSeed ageIdx 640)
        floodChance = duration / 20.0
        floods = if floodRoll < floodChance
            then []
            else []

        -- Age-level eruptions: roll for each active feature
        -- that has epTimelineScale == Age
        eruptSeed = ageSeed `xor` 0x1A7A
        eruptions = catMaybes
            [ generateEruption eruptSeed worldSize ageIdx plates pf
            | pf ← tbsFeatures tbs
            , case eruptionProfile (pfFeature pf) of
                Just ep → epTimelineScale ep ≡ Age
                Nothing → False
            ]

        allEvents = meteorites <> landslides <> floods <> eruptions

        gs2 = gs1 { gsCO2 = max 0.5 (gsCO2 gs1 - duration * 0.005) }

        erosion = erosionFromGeoState gs2 seed ageIdx

        period = GeoPeriod
            { gpName     = "Age " <> T.pack (show (tbsPeriodIdx tbs))
            , gpScale    = Age
            , gpDuration = round duration
            , gpDate     = currentDate
            , gpEvents   = allEvents
            , gpErosion  = erosion
            }
    in addPeriod period (tbs { tbsGeoState = gs2 })

erosionFromGeoState ∷ GeoState → Word64 → Int → ErosionParams
erosionFromGeoState gs seed ageIdx =
    let co2 = gsCO2 gs
        chemical = min 1.0 (0.2 + (co2 - 1.0) * 0.3)
    in ErosionParams
        { epIntensity = 0.5
        , epHydraulic = 0.5
        , epThermal   = 0.3
        , epWind      = 0.2
        , epChemical  = chemical
        , epSeed      = seed + fromIntegral ageIdx * 7
        }

-----------------------------------------------------------
-- Eruption Generation (per-feature)
-----------------------------------------------------------

-- | Roll for an eruption from a single active feature.
--   Returns an EruptionEvent if the roll succeeds.
generateEruption ∷ Word64 → Int → Int → [TectonicPlate]
                 → PersistentFeature → Maybe GeoEvent
generateEruption seed worldSize ageIdx plates pf =
    case pfActivity pf of
        Active → case eruptionProfile (pfFeature pf) of
            Nothing → Nothing
            Just profile →
                let GeoFeatureId fidInt = pfId pf
                    h1 = hashGeo seed (fidInt + ageIdx) 700
                    roll = hashToFloatGeo h1
                in if roll < epEruptChance profile
                   then Just (buildEruptionEvent seed worldSize ageIdx plates pf profile)
                   else Nothing
        _ → Nothing

buildEruptionEvent ∷ Word64 → Int → Int → [TectonicPlate]
                   → PersistentFeature → EruptionProfile → GeoEvent
buildEruptionEvent seed worldSize ageIdx plates pf profile =
    let GeoFeatureId fidInt = pfId pf
        h2 = hashGeo seed (fidInt + ageIdx) 710
        h3 = hashGeo seed (fidInt + ageIdx) 711

        radius = hashToRangeGeo h2 (epMinRadius profile) (epMaxRadius profile)
        volume = hashToRangeGeo h3 (epMinVolume profile) (epMaxVolume profile)

        (sx, sy) = featureCenter (pfFeature pf)

        -- Look up actual terrain elevation at the source
        (srcElev, _) = elevationAtGlobal seed plates worldSize sx sy

        -- Lava erupts above the current surface
        -- More eruptions = more buildup
        eruptionBoost = pfEruptionCount pf * 5
        lavaElev = srcElev + eruptionBoost + volume `div` 4

        flow = LavaFlow
            { lfSourceX   = sx
            , lfSourceY   = sy
            , lfRadius    = radius
            , lfElevation = lavaElev
            , lfVolume    = volume
            , lfMaterial  = epMaterial profile
            , lfViscosity = epViscosity profile
            }
    in EruptionEvent (pfId pf) flow

-- | Extract the center coordinates from any volcanic feature.
featureCenter ∷ VolcanicFeature → (Int, Int)
featureCenter (ShieldVolcano p)    = let GeoCoord x y = shCenter p in (x, y)
featureCenter (CinderCone p)       = let GeoCoord x y = ccCenter p in (x, y)
featureCenter (LavaDome p)         = let GeoCoord x y = ldCenter p in (x, y)
featureCenter (Caldera p)          = let GeoCoord x y = caCenter p in (x, y)
featureCenter (FissureVolcano p)   = let GeoCoord sx sy = fpStart p
                                         GeoCoord ex ey = fpEnd p
                                     in ((sx + ex) `div` 2, (sy + ey) `div` 2)
featureCenter (LavaTube p)         = let GeoCoord sx sy = ltStart p
                                         GeoCoord ex ey = ltEnd p
                                     in ((sx + ex) `div` 2, (sy + ey) `div` 2)
featureCenter (SuperVolcano p)     = let GeoCoord x y = svCenter p in (x, y)
featureCenter (HydrothermalVent p) = let GeoCoord x y = htCenter p in (x, y)
