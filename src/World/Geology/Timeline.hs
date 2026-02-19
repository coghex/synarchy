{-# LANGUAGE Strict, UnicodeSyntax #-}
module World.Geology.Timeline
    ( buildTimeline
    ) where

import UPrelude
import Data.Bits (xor)
import Data.Word (Word64)
import Data.List (foldl')
import qualified Data.Text as T
import qualified Data.Vector.Unboxed as VU
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
import World.Hydrology.Glacier (generateGlaciers, evolveGlacier)
import World.Hydrology.Climate (computeRegionalClimate)
import World.Hydrology.Types (HydroFeature(..), GlacierParams(..)
                             , RiverParams(..), LakeParams(..))
import World.Hydrology.Simulation (simulateHydrology, FlowResult(..)
                                  , ElevGrid(..), buildInitialElevGrid
                                  , updateElevGrid)

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

        -- Build elevation grid ONCE from base plates
        grid0 = buildInitialElevGrid seed worldSize plates

        -- Primordial bombardment stands alone, before the eon
        (s1, grid1) = buildPrimordialBombardment seed worldSize plates tbs0 grid0

        -- The single development eon
        (s2, _grid2) = buildEon seed worldSize plates s1 grid1

    in GeoTimeline
        { gtSeed      = seed
        , gtWorldSize = worldSize
        , gtPeriods   = reverse (tbsPeriods s2)
        , gtFeatures  = tbsFeatures s2
        }

-----------------------------------------------------------
-- Primordial Bombardment (standalone, pre-eon)
-----------------------------------------------------------

buildPrimordialBombardment seed worldSize plates tbs grid =
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
        tbs' = addPeriod period (tbs { tbsGeoState = gs' })
        grid' = updateElevGrid worldSize grid period
    in (tbs', grid')

-----------------------------------------------------------
-- Eon
-----------------------------------------------------------

buildEon ∷ Word64 → Int → [TectonicPlate]
         → TimelineBuildState → ElevGrid
         → (TimelineBuildState, ElevGrid)
buildEon seed worldSize plates tbs grid =
    buildEraLoop seed worldSize plates 0 2 4 tbs grid

buildEraLoop ∷ Word64 → Int → [TectonicPlate]
             → Int → Int → Int
             → TimelineBuildState → ElevGrid
             → (TimelineBuildState, ElevGrid)
buildEraLoop seed worldSize plates eraIdx minEras maxEras tbs grid
    | eraIdx ≥ maxEras = (tbs, grid)
    | otherwise =
        let eraSeed = seed `xor` (fromIntegral eraIdx * 0xA1B2C3D4)
            (s1, grid1) = buildEra eraSeed worldSize plates eraIdx tbs grid

            roll = hashToFloatGeo (hashGeo eraSeed eraIdx 300)
            continue = eraIdx < (minEras - 1) ∨ roll < 0.5
        in if continue
           then buildEraLoop seed worldSize plates (eraIdx + 1) minEras maxEras s1 grid1
           else (s1, grid1)

-----------------------------------------------------------
-- Era
-----------------------------------------------------------

buildEra ∷ Word64 → Int → [TectonicPlate] → Int
         → TimelineBuildState → ElevGrid
         → (TimelineBuildState, ElevGrid)
buildEra seed worldSize plates eraIdx tbs grid =
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
        -- No grid update needed — eraEvents is empty
        grid1 = grid

        (s2, grid2) = buildPeriodLoop eraSeed worldSize plates 0 2 4 s1 grid1

    in (s2, grid2)

-----------------------------------------------------------
-- Period
-----------------------------------------------------------

buildPeriodLoop ∷ Word64 → Int → [TectonicPlate]
                → Int → Int → Int
                → TimelineBuildState → ElevGrid
                → (TimelineBuildState, ElevGrid)
buildPeriodLoop seed worldSize plates periodIdx minPeriods maxPeriods tbs grid
    | periodIdx ≥ maxPeriods = (tbs, grid)
    | otherwise =
        let periodSeed = seed `xor` (fromIntegral periodIdx * 0xB3C4D5E6)
            (s1, grid1) = buildPeriod periodSeed worldSize plates periodIdx tbs grid

            roll = hashToFloatGeo (hashGeo periodSeed periodIdx 400)
            continue = periodIdx < (minPeriods - 1) ∨ roll < 0.5
        in if continue
           then buildPeriodLoop seed worldSize plates (periodIdx + 1) minPeriods maxPeriods s1 grid1
           else (s1, grid1)

buildPeriod ∷ Word64 → Int → [TectonicPlate] → Int
            → TimelineBuildState → ElevGrid
            → (TimelineBuildState, ElevGrid)
buildPeriod seed worldSize plates periodIdx tbs grid =
    let periodSeed = seed `xor` (fromIntegral periodIdx * 0xF1E2)

        (s1, grid1) = applyPeriodVolcanism periodSeed worldSize plates periodIdx tbs grid

        (s2, grid2) = applyVolcanicEvolution periodSeed worldSize plates s1 grid1

        (s3, grid3) = buildEpochLoop periodSeed worldSize plates 0 2 6 s2 grid2

    in (s3, grid3)

applyPeriodVolcanism ∷ Word64 → Int → [TectonicPlate] → Int
                     → TimelineBuildState → ElevGrid
                     → (TimelineBuildState, ElevGrid)
applyPeriodVolcanism seed worldSize plates periodIdx tbs grid =
    let gs = tbsGeoState tbs
        volcSeed = seed `xor` 0xB45A1F1C
        pIdx = tbsPeriodIdx tbs
        activityLevel = gsCO2 gs
        currentDate = gdMillionYears (gsDate gs)

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
        tbs6 = addPeriod period (tbs5 { tbsGeoState = gs' })
        grid' = updateElevGrid worldSize grid period
    in (tbs6, grid')

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
                       → TimelineBuildState → ElevGrid
                       → (TimelineBuildState, ElevGrid)
applyVolcanicEvolution seed worldSize plates tbs grid =
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
    in if null allEvents then (tbs1, grid)
       else let tbs2 = addPeriod period tbs1
                grid' = updateElevGrid worldSize grid period
            in (tbs2, grid')

-----------------------------------------------------------
-- Epoch
-----------------------------------------------------------

buildEpochLoop ∷ Word64 → Int → [TectonicPlate]
               → Int → Int → Int
               → TimelineBuildState → ElevGrid
               → (TimelineBuildState, ElevGrid)
buildEpochLoop seed worldSize plates epochIdx minEpochs maxEpochs tbs grid
    | epochIdx ≥ maxEpochs = (tbs, grid)
    | otherwise =
        let epochSeed = seed `xor` (fromIntegral epochIdx * 0xC5D6E7F8)
            (s1, grid1) = buildEpoch epochSeed worldSize plates epochIdx tbs grid

            roll = hashToFloatGeo (hashGeo epochSeed epochIdx 500)
            continue = epochIdx < (minEpochs - 1) ∨ roll < 0.5
        in if continue
           then buildEpochLoop seed worldSize plates (epochIdx + 1) minEpochs maxEpochs s1 grid1
           else (s1, grid1)

buildEpoch ∷ Word64 → Int → [TectonicPlate] → Int
           → TimelineBuildState → ElevGrid
           → (TimelineBuildState, ElevGrid)
buildEpoch seed worldSize plates epochIdx tbs grid =
    let epochSeed = seed `xor` (fromIntegral epochIdx * 0xA1A2)
        gs = tbsGeoState tbs
        gs' = gs { gsDate = advanceGeoDate 20.0 (gsDate gs) }
        s1 = tbs { tbsGeoState = gs' }
        -- Grid is already up to date — just pass it through
        (s2, grid') = buildAgeLoop epochSeed worldSize plates 0 1 8 s1 grid
    in (s2, grid')

-----------------------------------------------------------
-- Age
-----------------------------------------------------------

buildAgeLoop ∷ Word64 → Int → [TectonicPlate]
             → Int → Int → Int
             → TimelineBuildState → ElevGrid
             → (TimelineBuildState, ElevGrid)
buildAgeLoop seed worldSize plates ageIdx minAges maxAges tbs grid
    | ageIdx ≥ maxAges = (tbs, grid)
    | otherwise =
        let ageSeed = seed `xor` (fromIntegral ageIdx * 0xD7E8F9A0)
            (s1, grid1) = buildAge ageSeed worldSize plates ageIdx tbs grid

            roll = hashToFloatGeo (hashGeo ageSeed ageIdx 600)
            continue = ageIdx < (minAges - 1) ∨ roll < 0.4
        in if continue
           then buildAgeLoop seed worldSize plates (ageIdx + 1)
                    minAges maxAges s1 grid1
           else (s1, grid1)

buildAge ∷ Word64 → Int → [TectonicPlate] → Int
         → TimelineBuildState → ElevGrid
         → (TimelineBuildState, ElevGrid)
buildAge seed worldSize plates ageIdx tbs elevGrid =
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
        -- Hydrology (flow simulation every age)
        -----------------------------------------------------------
        hydroSeed = ageSeed `xor` 0xA0CA71C

        flowResult = simulateHydrology hydroSeed worldSize ageIdx elevGrid

        (hydroFeatures, tbs_h) = registerFlowResults
            hydroSeed ageIdx flowResult (tbsPeriodIdx tbs) tbs

        hydroEvents = map (\pf → case pfFeature pf of
            HydroShape hf → HydroEvent hf
            _             → error "non-hydro in flow results"
            ) hydroFeatures

        -- DEBUG: trace flow results
        -- Count land cells and max accumulation for debugging
        _debugLandCount = VU.length (VU.filter id (egLand elevGrid))
        _debugGridW = egGridW elevGrid
        _debugRiverCount = length (frRivers flowResult)
        _debugLakeCount = length (frLakes flowResult)

        allEvents = meteorites <> eruptions <> hydroEvents

        gs2 = gs1 { gsCO2 = max 0.5 (gsCO2 gs1 - duration * 0.005) }
        erosion = erosionFromGeoState gs2 seed ageIdx
        period = mkGeoPeriod worldSize
            ("Age " <> T.pack (show (tbsPeriodIdx tbs))
             <> " [grid=" <> T.pack (show _debugGridW)
             <> " land=" <> T.pack (show _debugLandCount)
             <> " rivers=" <> T.pack (show _debugRiverCount)
             <> " lakes=" <> T.pack (show _debugLakeCount) <> "]")
            Age (round duration) currentDate
            allEvents erosion

        tbs_final = addPeriod period (tbs_h { tbsGeoState = gs2 })

        -- Update the elevation grid with THIS age's new period
        elevGrid' = updateElevGrid worldSize elevGrid period

    in (tbs_final, elevGrid')

-----------------------------------------------------------
-- Capped evolution wrappers
-----------------------------------------------------------

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
         in if (temp < 0.8 ∧ roll ≥ 0.50 ∧ roll < 0.65)
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

        (srcElev, _) = elevationAtGlobal seed plates worldSize sx sy

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

-----------------------------------------------------------
-- Flow Simulation Registration
-----------------------------------------------------------

registerFlowResults ∷ Word64 → Int → FlowResult → Int
                    → TimelineBuildState
                    → ([PersistentFeature], TimelineBuildState)
registerFlowResults seed ageIdx flowResult periodIdx tbs =
    let -- Remove old river/lake features entirely instead of marking extinct.
        -- They get replaced by the simulation's output each age.
        -- Glaciers and volcanic features are untouched.
        keptFeatures = filter (not . isSimulatedHydro) (tbsFeatures tbs)
        tbs0 = tbs { tbsFeatures = keptFeatures }

        -- Register new rivers
        (riverPfs, tbs1) = foldl' (\(acc, st) river →
            let (fid, st') = allocFeatureId st
                pf = PersistentFeature
                    { pfId               = fid
                    , pfFeature          = HydroShape $ RiverFeature river
                    , pfActivity         = FActive
                    , pfFormationPeriod   = periodIdx
                    , pfLastActivePeriod  = periodIdx
                    , pfEruptionCount     = 1
                    , pfParentId          = Nothing
                    }
                st'' = registerFeature pf st'
            in (pf : acc, st'')
            ) ([], tbs0) (frRivers flowResult)

        -- Register new lakes
        (lakePfs, tbs2) = foldl' (\(acc, st) lake →
            let (fid, st') = allocFeatureId st
                pf = PersistentFeature
                    { pfId               = fid
                    , pfFeature          = HydroShape $ LakeFeature lake
                    , pfActivity         = FActive
                    , pfFormationPeriod   = periodIdx
                    , pfLastActivePeriod  = periodIdx
                    , pfEruptionCount     = 0
                    , pfParentId          = Nothing
                    }
                st'' = registerFeature pf st'
            in (pf : acc, st'')
            ) ([], tbs1) (frLakes flowResult)

    in (riverPfs <> lakePfs, tbs2)

-- | Is this a river or lake from the flow simulation?
--   Glaciers are NOT simulated — they keep their own evolution.
isSimulatedHydro ∷ PersistentFeature → Bool
isSimulatedHydro pf = case pfFeature pf of
    HydroShape (RiverFeature _) → True
    HydroShape (LakeFeature _)  → True
    _                           → False
