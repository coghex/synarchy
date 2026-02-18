{-# LANGUAGE Strict, UnicodeSyntax #-}
module World.Geology.Timeline
    ( buildTimeline
    ) where

import UPrelude
import Data.Bits (xor)
import Data.Word (Word64)
import Data.List (foldl')
import qualified Data.Text as T
import World.Base (GeoCoord(..), GeoFeatureId(..))
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
import World.Hydrology.River (generateRivers, evolveRiver)
import World.Hydrology.Glacier (generateGlaciers, evolveGlacier)
import World.Hydrology.Climate (computeRegionalClimate)
import World.Hydrology.Types (HydroFeature(..), GlacierParams(..)
                             , RiverParams(..), LakeParams(..))

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

buildPrimordialBombardment seed worldSize plates tbs =
    let craterSeed = seed `xor` 0xDEADBEEF
        craters = generateCraters craterSeed worldSize plates CraterEra_Primordial
        gs = tbsGeoState tbs
        currentDate = gdMillionYears (gsDate gs)
        gs' = gs { gsDate = advanceGeoDate 500.0 (gsDate gs) }
        events = map CraterEvent craters
        period = mkGeoPeriod worldSize
            "Primordial Bombardment" Eon 500 currentDate
            events
            (ErosionParams 0.8 0.3 0.6 0.4 0.1 (seed + 1000))
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
        eraPeriod = mkGeoPeriod worldSize
            ("Era " <> T.pack (show eraIdx) <> " Events")
            Era 100 currentDate
            eraEvents
            (ErosionParams 0.7 0.5 0.5 0.3 0.2 (seed + 3000 + fromIntegral eraIdx))
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

        -- Volcanism budget per period:
        --
        -- Feature type       | attempts | max | bbox radius | cost
        -- -------------------|----------|-----|-------------|------
        -- ShieldVolcano      |   12     |  3  |   30-60     | medium
        -- Fissure            |   10     |  3  |   40-100    | medium
        -- CinderCone         |   16     |  5  |    5-15     | cheap
        -- HydrothermalVent   |   10     |  3  |     3       | cheap
        -- SuperVolcano       |   12     |  1  |  120-250    | expensive
        --
        -- Total max features per period: 15 (was 10)
        -- But most are small-radius, so bbox filter kills them
        -- for distant chunks. The expensive SuperVolcano is still
        -- capped at 1 and only generated in periods 0-1.

        (shields, tbs1) = generateAndRegisterN 12 3 volcSeed worldSize plates
                              VolcanoEra_Hotspot generateShieldVolcano pIdx tbs

        (fissures, tbs2) = generateAndRegisterN 10 3 (volcSeed + 1) worldSize plates
                               VolcanoEra_Boundary generateFissure pIdx tbs1

        (cinders, tbs3) = generateAndRegisterN 16 5 (volcSeed + 2) worldSize plates
                              VolcanoEra_Boundary generateCinderCone pIdx tbs2

        (vents, tbs4) = generateAndRegisterN 10 3 (volcSeed + 3) worldSize plates
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

        period = mkGeoPeriod worldSize
            ("Volcanism " <> T.pack (show periodIdx))
            Period 50 currentDate
            events
            (ErosionParams 0.5 0.5 0.4 0.2 0.3 (seed + 4000))
    in addPeriod period (tbs5 { tbsGeoState = gs' })

isSuperVolcano ∷ PersistentFeature → Bool
isSuperVolcano pf = case pfFeature pf of
    (VolcanicShape (SuperVolcano _)) → True
    _                                → False

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
                                , pfActivity         = FActive
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

        period = mkGeoPeriod worldSize
            "Volcanic Evolution"
            Period 30 currentDate
            allEvents
            (ErosionParams 0.5 0.5 0.4 0.2 0.3 (seed + 5000))
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
             else let period = mkGeoPeriod worldSize
                          ("Epoch " <> T.pack (show epochIdx))
                          Epoch 20 currentDate
                          epochEvents
                          (ErosionParams 0.6 0.7 0.3 0.2 0.4 (seed + 6000))
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
        meteoriteChance = min 0.85 (duration / 15.0)
        meteorites = if meteoriteRoll < meteoriteChance
            then let craterSeed = ageSeed `xor` 0xBEEF
                     craters = generateCraters craterSeed worldSize plates CraterEra_Late
                 in take (hashToRangeGeo (hashGeo ageSeed ageIdx 621) 1 5)
                         (map CraterEvent craters)
            else []

        -- Age-level eruptions
        eruptSeed = ageSeed `xor` 0x1A7A
        eruptions = catMaybes
            [ generateEruption eruptSeed worldSize ageIdx plates pf
            | pf ← tbsFeatures tbs
            , case eruptionProfile (pfFeature pf) of
                Just ep → epTimelineScale ep ≡ Age
                Nothing → False
            ]

        -----------------------------------------------------------
        -- Hydrology
        -----------------------------------------------------------
        hydroSeed = ageSeed `xor` 0xA0CA71C

        -- Generate new rivers in early ages (ageIdx < 3)
        -- No hard cap — generateRivers has its own scaleCount limit
        (newRivers, tbs_r) =
            if ageIdx < 3
            then generateRivers hydroSeed worldSize plates
                     (tbsPeriodIdx tbs) tbs
            else ([], tbs)

        -- Generate glaciers every age — generateGlaciers has its own limits
        (newGlaciers, tbs_g) =
            generateGlaciers hydroSeed worldSize plates
                gs1 (tbsPeriodIdx tbs_r) tbs_r

        -- Evolve existing hydro features PROBABILISTICALLY.
        -- Each feature has a per-age probability of evolving,
        -- instead of guaranteed evolution every age.
        -- This dramatically reduces HydroModify event count
        -- while keeping interesting evolution over long timelines.
        --
        -- Base evolution chance:
        --   Rivers:   10% per age (they're dynamic, change often)
        --   Glaciers:  5% per age (slower processes)
        --   Lakes:    0%  (passive, only change via river/glacier events)
        --
        -- Longer ages increase the chance (more time = more happens)
        durationBonus = min 0.3 (duration / 30.0)

        (hydroEvents, tbs_h) = foldl'
            (\(evts, st) pf →
                let GeoFeatureId fidInt = pfId pf
                    evolRoll = hashToFloatGeo (hashGeo hydroSeed fidInt (650 + ageIdx))
                in case pfFeature pf of
                    HydroShape (RiverFeature _)
                        | pfActivity pf ≡ FActive ∨ pfActivity pf ≡ FDormant
                        , evolRoll < 0.1 + durationBonus →
                            evolveRiver hydroSeed (tbsPeriodIdx st) (evts, st) pf
                        | otherwise → (evts, st)

                    HydroShape (GlacierFeature _)
                        | pfActivity pf ≡ FActive ∨ pfActivity pf ≡ FDormant
                        , evolRoll < 0.05 + durationBonus →
                            evolveGlacier hydroSeed (tbsPeriodIdx st) gs1 (evts, st) pf
                        | otherwise → (evts, st)

                    HydroShape (LakeFeature _) → (evts, st)
                    VolcanicShape _ → (evts, st)
            ) ([], tbs_g) (tbsFeatures tbs_g)

        -- Creation events for new features
        hydroCreationEvents = map (\pf → case pfFeature pf of
            HydroShape hf → HydroEvent hf
            _             → error "non-hydro in newRivers/newGlaciers"
            ) (newRivers <> newGlaciers)

        allEvents = meteorites <> eruptions
                 <> hydroCreationEvents <> hydroEvents

        gs2 = gs1 { gsCO2 = max 0.5 (gsCO2 gs1 - duration * 0.005) }

        erosion = erosionFromGeoState gs2 seed ageIdx

        period = mkGeoPeriod worldSize
            ("Age " <> T.pack (show (tbsPeriodIdx tbs)))
            Age (round duration) currentDate
            allEvents
            erosion

    in addPeriod period (tbs_h { tbsGeoState = gs2 })

-----------------------------------------------------------
-- Capped evolution wrappers
-----------------------------------------------------------

-- | Wrapper around evolveRiver that suppresses branching
--   when we've hit the feature cap.
evolveRiverCapped ∷ Word64 → Bool → Int
                  → ([GeoEvent], TimelineBuildState)
                  → PersistentFeature
                  → ([GeoEvent], TimelineBuildState)
evolveRiverCapped seed canBranch periodIdx (events, tbs) pf =
    if canBranch
    then evolveRiver seed periodIdx (events, tbs) pf
    else -- Run evolution but suppress branching by biasing the roll.
         -- If roll < 0.15 (branch zone), skip and treat as "continue".
         let fid = pfId pf
             GeoFeatureId fidInt = fid
             h1 = hashGeo seed fidInt 800
             roll = hashToFloatGeo h1
         in if roll < 0.15
            then (events, tbs)  -- would have branched, suppress it
            else evolveRiver seed periodIdx (events, tbs) pf

-- | Wrapper around evolveGlacier that suppresses branching
--   when we've hit the feature cap.
evolveGlacierCapped ∷ Word64 → Bool → Int → GeoState
                    → ([GeoEvent], TimelineBuildState)
                    → PersistentFeature
                    → ([GeoEvent], TimelineBuildState)
evolveGlacierCapped seed canBranch periodIdx gs (events, tbs) pf =
    if canBranch
    then evolveGlacier seed periodIdx gs (events, tbs) pf
    else let fid = pfId pf
             GeoFeatureId fidInt = fid
             h1 = hashGeo seed fidInt 900
             roll = hashToFloatGeo h1
             temp = gsCO2 gs
         in -- Cold world branch zone is roll 0.50-0.65,
            -- moderate is 0.20-0.30. Suppress those ranges.
            if (temp < 0.8 ∧ roll ≥ 0.50 ∧ roll < 0.65)
             ∨ (temp ≥ 0.8 ∧ temp ≤ 1.2 ∧ roll ≥ 0.20 ∧ roll < 0.30)
            then (events, tbs)
            else evolveGlacier seed periodIdx gs (events, tbs) pf

-----------------------------------------------------------
-- Hydro feature classification helpers
-----------------------------------------------------------

isHydroFeature ∷ FeatureShape → Bool
isHydroFeature (HydroShape _) = True
isHydroFeature _              = False

isRiverFeature ∷ FeatureShape → Bool
isRiverFeature (HydroShape (RiverFeature _)) = True
isRiverFeature _                             = False

isGlacierFeature ∷ FeatureShape → Bool
isGlacierFeature (HydroShape (GlacierFeature _)) = True
isGlacierFeature _                               = False

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
        FActive → case eruptionProfile (pfFeature pf) of
            Nothing → Nothing
            Just profile →
                let GeoFeatureId fidInt = pfId pf
                    h1 = hashGeo seed fidInt (700 + ageIdx)
                    roll = hashToFloatGeo h1
                in if roll < epEruptChance profile
                   then Just (buildEruptionEvent seed worldSize ageIdx plates pf profile)
                   else Nothing
        _ → Nothing

buildEruptionEvent ∷ Word64 → Int → Int → [TectonicPlate]
                   → PersistentFeature → EruptionProfile → GeoEvent
buildEruptionEvent seed worldSize ageIdx plates pf profile =
    let GeoFeatureId fidInt = pfId pf
        h2 = hashGeo seed fidInt (710 + ageIdx)
        h3 = hashGeo seed fidInt (711 + ageIdx)

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
featureCenter ∷ FeatureShape → (Int, Int)
featureCenter (VolcanicShape (ShieldVolcano p))
    = let GeoCoord x y = shCenter p in (x, y)
featureCenter (VolcanicShape (CinderCone p))
    = let GeoCoord x y = ccCenter p in (x, y)
featureCenter (VolcanicShape (LavaDome p))
    = let GeoCoord x y = ldCenter p in (x, y)
featureCenter (VolcanicShape (Caldera p))
    = let GeoCoord x y = caCenter p in (x, y)
featureCenter (VolcanicShape (FissureVolcano p))
    = let GeoCoord sx sy = fpStart p
          GeoCoord ex ey = fpEnd p
      in ((sx + ex) `div` 2, (sy + ey) `div` 2)
featureCenter (VolcanicShape (LavaTube p))
    = let GeoCoord sx sy = ltStart p
          GeoCoord ex ey = ltEnd p
      in ((sx + ex) `div` 2, (sy + ey) `div` 2)
featureCenter (VolcanicShape (SuperVolcano p))
    = let GeoCoord x y = svCenter p in (x, y)
featureCenter (VolcanicShape (HydrothermalVent p))
    = let GeoCoord x y = htCenter p in (x, y)
featureCenter (HydroShape (RiverFeature r))
    = let GeoCoord x y = rpSourceRegion r in (x, y)
featureCenter (HydroShape (GlacierFeature g))
    = let GeoCoord x y = glCenter g in (x, y)
featureCenter (HydroShape (LakeFeature l))
    = let GeoCoord x y = lkCenter l in (x, y)

-- | Smart constructor for GeoPeriod that pre-computes tagged events.
--   Use this instead of directly constructing GeoPeriod records.
mkGeoPeriod ∷ Int → Text → GeoScale → Int → Float → [GeoEvent] → ErosionParams → GeoPeriod
mkGeoPeriod worldSize name scale duration date events erosion =
    GeoPeriod
        { gpName         = name
        , gpScale        = scale
        , gpDuration     = duration
        , gpDate         = date
        , gpEvents       = events
        , gpErosion      = erosion
        , gpTaggedEvents = tagEventsWithBBox worldSize events
        }
