{-# LANGUAGE Strict, UnicodeSyntax #-}
module World.Geology.Timeline
    ( buildTimeline
    ) where
import UPrelude
import Data.Bits (xor)
import Data.Word (Word64)
import Data.List (foldl', minimumBy, sortBy)
import qualified Data.Text as T
import qualified Data.Vector.Unboxed as VU
import World.Base (GeoCoord(..), GeoFeatureId(..))
import World.Types
import World.Plate (generatePlates, TectonicPlate, elevationAtGlobal, isBeyondGlacier
                   , wrapGlobalU)
import World.Generate.Timeline (applyTimelineFast)
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
import World.Hydrology.River.Meander (meanderSegments)
import World.Hydrology.River.Tributary (buildTributarySegments)

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
        -- Hydrology
        -----------------------------------------------------------

        hydroSeed = ageSeed `xor` 0xA0CA71C

        flowResult = simulateHydrology hydroSeed worldSize ageIdx elevGrid

        -- Build a partial timeline from periods so far for elevation lookups
        partialTimeline = GeoTimeline
            { gtSeed      = seed
            , gtWorldSize = worldSize
            , gtPeriods   = reverse (tbsPeriods tbs)
            , gtFeatures  = tbsFeatures tbs
            }

        (hydroFeatures, hydroEvents, tbs_h0) = reconcileHydrology
            hydroSeed ageIdx flowResult (tbsPeriodIdx tbs) worldSize
            plates partialTimeline tbs

        tbs_h = mergeConvergingRivers worldSize (tbsPeriodIdx tbs) tbs_h0
        -- Collect the final hydro events from evolved features
        -- (merging doesn't produce events, the carving comes from
        -- the updated RiverParams on the PersistentFeatures)

        allEvents = meteorites <> eruptions <> hydroEvents

        gs2 = gs1 { gsCO2 = max 0.5 (gsCO2 gs1 - duration * 0.005) }
        erosion = erosionFromGeoState gs2 seed ageIdx
        _debugLandCount = VU.length (VU.filter id (egLand elevGrid))
        _debugGridW = egGridW elevGrid
        _debugRiverCount = length (frRiverSources flowResult)
        _debugLakeCount = length (frLakes flowResult)

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

-- | Reconcile flow simulation with existing features.
--   - Existing rivers: evolve them (do NOT dry them up)
--   - Simulation rivers: create only genuinely new ones
--     (source region not near any existing river's path)
--   - Lakes: add new ones only
reconcileHydrology ∷ Word64 → Int → FlowResult → Int → Int
                   → [TectonicPlate] → GeoTimeline
                   → TimelineBuildState
                   → ([PersistentFeature], [GeoEvent], TimelineBuildState)
reconcileHydrology seed ageIdx flowResult periodIdx worldSize plates timeline tbs =
    let existingRivers = filter (isActiveRiver . pfFeature) (tbsFeatures tbs)
        existingLakes  = filter (isLakeFeature . pfFeature)  (tbsFeatures tbs)
        simSources = frRiverSources flowResult
        simLakes   = frLakes flowResult

        maxTotalRivers = 30  -- fewer, longer rivers
        currentRiverCount = length existingRivers

        -- Evolve existing rivers
        (evolvedPfs, evolveEvents, tbs1) =
            foldl' (\(pfs, evts, st) existPf →
                let (pf', evt, st') = evolveExistingRiver seed ageIdx periodIdx
                                          existPf st
                in (pf' : pfs, evt ++ evts, st')
            ) ([], [], tbs) existingRivers

        -- Create new rivers from simulation sources
        budget = maxTotalRivers - currentRiverCount

        -- Filter sources: must be far from existing rivers
        newSources = if budget ≤ 0
            then []
            else take budget $ filter (isSourceNew worldSize existingRivers) simSources

        -- Trace each source at tile level
        newRivers = catMaybes $ zipWith (\idx (gx, gy, elev, flow) →
            traceRiverFromSource seed worldSize plates timeline
                gx gy elev (ageIdx * 1000 + idx) flow
            ) [0..] newSources

        (newPfs, newEvents, tbs2) =
            foldl' (\(pfs, evts, st) river →
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
                in (pf : pfs, HydroEvent (RiverFeature river) : evts, st'')
            ) ([], [], tbs1) newRivers

        -- Lakes
        (lakePfs, lakeEvents, tbs3) = reconcileLakes seed ageIdx periodIdx
                                          existingLakes simLakes tbs2

        allNewPfs = evolvedPfs ++ newPfs ++ lakePfs
        allEvents = evolveEvents ++ newEvents ++ lakeEvents

    in (allNewPfs, allEvents, tbs3)

-- | Is a simulation source far from all existing rivers?
isSourceNew ∷ Int → [PersistentFeature] → (Int, Int, Int, Float) → Bool
isSourceNew worldSize existingRivers (sx, sy, _, _) =
    let threshold = 150  -- tiles
    in not $ any (\pf →
        let river = getRiverParamsFromPf pf
            GeoCoord ex ey = rpSourceRegion river
            dx = abs (wrappedDeltaXGeo worldSize sx ex)
            dy = abs (sy - ey)
        in dx < threshold ∧ dy < threshold
        ) existingRivers

isActiveRiver ∷ FeatureShape → Bool
isActiveRiver (HydroShape (RiverFeature _)) = True
isActiveRiver _                             = False

-- | A simulation river is "genuinely new" if its source is far from
--   the source of every existing river. Simple and fast.
isGenuinelyNew ∷ Int → [PersistentFeature] → RiverParams → Bool
isGenuinelyNew worldSize existingRivers simRiver =
    let GeoCoord sx sy = rpSourceRegion simRiver
        threshold = 120  -- tiles; ~4 grid cells at spacing=32
    in not $ any (\pf →
        let river = getRiverParamsFromPf pf
            GeoCoord ex ey = rpSourceRegion river
            dx = abs (wrappedDeltaXGeo worldSize sx ex)
            dy = abs (sy - ey)
        in dx < threshold ∧ dy < threshold
        ) existingRivers

-- | Evolve an existing river for one age.
--   Probability table:
--     20% → Meander: path segments shift laterally
--     15% → Branch: new tributary splits off
--     15% → Deepen: valley carves deeper on all segments
--     10% → Widen: channel and valley get wider
--     40% → Continue: no change
evolveExistingRiver ∷ Word64 → Int → Int
                    → PersistentFeature
                    → TimelineBuildState
                    → (PersistentFeature, [GeoEvent], TimelineBuildState)
evolveExistingRiver seed ageIdx periodIdx pf tbs =
    let fid = pfId pf
        GeoFeatureId fidInt = fid
        river = getRiverParamsFromPf pf
        h1 = hashGeo seed fidInt (900 + ageIdx)
        roll = hashToFloatGeo h1

        -- Count existing rivers to suppress branching when near cap
        currentRiverCount = length $ filter (isActiveRiver . pfFeature) (tbsFeatures tbs)
        canBranch = currentRiverCount < 25
    in
    if roll < 0.25
    -- 25%: Meander (increased from 20%)
    then let h2 = hashGeo seed fidInt (910 + ageIdx)
             meanderAmt = 0.15 + hashToFloatGeo h2 * 0.4
             newSegs = meanderSegments seed fidInt meanderAmt (rpSegments river)
             newRiver = river { rpSegments = newSegs }
             evt = HydroModify fid (RiverMeander
                     (fromIntegral (hashGeo seed fidInt (911 + ageIdx)))
                     meanderAmt)
             updatedPf = pf
                 { pfFeature          = HydroShape $ RiverFeature newRiver
                 , pfLastActivePeriod = periodIdx
                 }
             tbs' = updateFeature fid (const updatedPf) tbs
         in (updatedPf, [evt, HydroEvent (RiverFeature newRiver)], tbs')

    else if roll < 0.30 ∧ canBranch
    -- 5%: Branch (reduced from 15%, and only when under cap)
    then let numSegs = length (rpSegments river)
         in if numSegs < 3
            then evolveDeepen seed ageIdx periodIdx pf river tbs
            else
            let (childId, tbs') = allocFeatureId tbs
                h2 = hashGeo seed fidInt (920 + ageIdx)
                h3 = hashGeo seed fidInt (921 + ageIdx)
                h4 = hashGeo seed fidInt (922 + ageIdx)
                h5 = hashGeo seed fidInt (923 + ageIdx)
                h6 = hashGeo seed fidInt (924 + ageIdx)

                branchSegIdx = hashToRangeGeo h2 1 (numSegs - 1)
                branchSeg = rpSegments river !! min branchSegIdx (numSegs - 1)
                GeoCoord bx by = rsStart branchSeg
                GeoCoord bex bey = rsEnd branchSeg

                segDX = fromIntegral (bex - bx) ∷ Float
                segDY = fromIntegral (bey - by) ∷ Float
                segAngle = atan2 segDY segDX
                side = if hashToFloatGeo h3 < 0.5 then 1.0 else -1.0
                branchAngle = segAngle + side * (0.7 + hashToFloatGeo h4 * 0.7)
                branchLen = hashToRangeGeo h5 15 50
                srcX = bx - round (fromIntegral branchLen * cos branchAngle)
                srcY = by - round (fromIntegral branchLen * sin branchAngle)

                numTribSegs = hashToRangeGeo h6 2 4
                tribSegs = buildTributarySegments seed fidInt
                               srcX srcY bx by numTribSegs

                tributaryParams = RiverParams
                    { rpSourceRegion = GeoCoord srcX srcY
                    , rpMouthRegion  = GeoCoord bx by
                    , rpSegments     = tribSegs
                    , rpFlowRate     = rsFlowRate branchSeg * 0.4
                    , rpMeanderSeed  = fromIntegral (hashGeo seed fidInt (925 + ageIdx))
                    }

                childPf = PersistentFeature
                    { pfId               = childId
                    , pfFeature          = HydroShape $ RiverFeature tributaryParams
                    , pfActivity         = FActive
                    , pfFormationPeriod   = periodIdx
                    , pfLastActivePeriod  = periodIdx
                    , pfEruptionCount     = 1
                    , pfParentId          = Just fid
                    }

                evt = HydroModify fid (RiverBranch
                        (GeoCoord bx by) branchAngle branchLen childId)

                -- Increase parent flow downstream of branch
                newRiver = river
                    { rpFlowRate = rpFlowRate river + rpFlowRate tributaryParams * 0.5
                    }
                updatedPf = pf
                    { pfFeature          = HydroShape $ RiverFeature newRiver
                    , pfLastActivePeriod = periodIdx
                    , pfEruptionCount    = pfEruptionCount pf + 1
                    }

                tbs'' = registerFeature childPf tbs'
                tbs''' = updateFeature fid (const updatedPf) tbs''

            in ( updatedPf
               , [ evt
                 , HydroEvent (RiverFeature newRiver)
                 , HydroEvent (RiverFeature tributaryParams)
                 ]
               , tbs''' )

    else if roll < 0.50
    -- 15%: Deepen
    then evolveDeepen seed ageIdx periodIdx pf river tbs

    else if roll < 0.60
    -- 10%: Widen
    then let newSegs = map (\seg → seg
                 { rsWidth = min 12 (rsWidth seg + 1)
                 , rsValleyWidth = rsValleyWidth seg + 3
                 }) (rpSegments river)
             newRiver = river { rpSegments = newSegs }
             updatedPf = pf
                 { pfFeature          = HydroShape $ RiverFeature newRiver
                 , pfLastActivePeriod = periodIdx
                 }
             tbs' = updateFeature fid (const updatedPf) tbs
         in (updatedPf, [HydroEvent (RiverFeature newRiver)], tbs')

    else
    -- 40%: Continue unchanged
    (pf, [], tbs)

-- | Merge rivers that converge. After evolution, check all pairs
--   of active rivers. If river A has a segment endpoint within
--   mergeThreshold of any segment of river B, and A is shorter:
--   - Truncate A's path at the junction point
--   - Set A's mouth to the junction point
--   - Set A's parentId to B's id
--   - Increase B's downstream flow
mergeConvergingRivers ∷ Int → Int → TimelineBuildState → TimelineBuildState
mergeConvergingRivers worldSize periodIdx tbs =
    let activeRivers = filter (\pf → isActiveRiver (pfFeature pf)
                                   ∧ pfActivity pf ≡ FActive)
                              (tbsFeatures tbs)
        mergeThreshold = 24  -- tiles

        -- For each pair, check if they should merge
        go [] st = st
        go (pf : rest) st =
            let river = getRiverParamsFromPf pf
                -- Check against all other rivers (not self)
                others = filter (\other → pfId other ≠ pfId pf) rest
            in case findMergeTarget worldSize mergeThreshold pf others of
                Nothing → go rest st
                Just (targetPf, junctionCoord, segIdx) →
                    let st' = performMerge worldSize periodIdx
                                  pf targetPf junctionCoord segIdx st
                    in go rest st'

    in go activeRivers tbs

findMergeTarget ∷ Int → Int → PersistentFeature → [PersistentFeature]
               → Maybe (PersistentFeature, GeoCoord, Int)
findMergeTarget worldSize threshold pf others =
    let river = getRiverParamsFromPf pf
        -- Check the mouth of this river against segments of others
        GeoCoord mx my = rpMouthRegion river
    in case catMaybes
            [ checkRiverProximity worldSize threshold mx my other
            | other ← others ] of
        []    → Nothing
        (x:_) → Just x

checkRiverProximity ∷ Int → Int → Int → Int → PersistentFeature
                    → Maybe (PersistentFeature, GeoCoord, Int)
checkRiverProximity worldSize threshold mx my targetPf =
    let targetRiver = getRiverParamsFromPf targetPf
        segs = rpSegments targetRiver
        -- Find closest segment
        dists = zipWith (\idx seg →
            let GeoCoord sx sy = rsStart seg
                GeoCoord ex ey = rsEnd seg
                -- Distance to segment midpoint (rough but fast)
                midX = sx + wrappedDeltaXGeo worldSize ex sx `div` 2
                midY = (sy + ey) `div` 2
                dx = abs (wrappedDeltaXGeo worldSize mx midX)
                dy = abs (my - midY)
            in (dx + dy, idx, GeoCoord midX midY)
            ) [0..] segs
        close = filter (\(d, _, _) → d < threshold) dists
    in case close of
        [] → Nothing
        _  → let (_, idx, coord) = minimumBy (\(d1,_,_) (d2,_,_) → compare d1 d2) close
             in Just (targetPf, coord, idx)

performMerge ∷ Int → Int → PersistentFeature → PersistentFeature
             → GeoCoord → Int → TimelineBuildState → TimelineBuildState
performMerge worldSize periodIdx tributaryPf mainPf junctionCoord segIdx tbs =
    let tribId = pfId tributaryPf
        mainId = pfId mainPf
        tribRiver = getRiverParamsFromPf tributaryPf
        mainRiver = getRiverParamsFromPf mainPf

        -- Truncate tributary: keep segments up to where it meets the main river
        -- Add a final segment from current last endpoint to the junction
        tribSegs = rpSegments tribRiver
        lastTribSeg = if null tribSegs then Nothing else Just (last tribSegs)
        truncatedSegs = case lastTribSeg of
            Nothing → tribSegs
            Just ls → tribSegs ++
                [ RiverSegment
                    { rsStart       = rsEnd ls
                    , rsEnd         = junctionCoord
                    , rsWidth       = rsWidth ls
                    , rsValleyWidth = rsValleyWidth ls
                    , rsDepth       = rsDepth ls
                    , rsFlowRate    = rsFlowRate ls
                    , rsStartElev   = rsEndElev ls
                    , rsEndElev     = rsEndElev ls  -- approximate
                    }
                ]

        newTribRiver = tribRiver
            { rpMouthRegion = junctionCoord
            , rpSegments    = truncatedSegs
            }

        -- Increase main river's flow downstream of junction
        newMainSegs = zipWith (\idx seg →
            if idx ≥ segIdx
            then seg { rsFlowRate = rsFlowRate seg + rpFlowRate tribRiver * 0.6
                     , rsWidth = min 12 (rsWidth seg + 1)
                     }
            else seg
            ) [0..] (rpSegments mainRiver)
        newMainRiver = mainRiver
            { rpSegments = newMainSegs
            , rpFlowRate = rpFlowRate mainRiver + rpFlowRate tribRiver * 0.5
            }

        tbs' = updateFeature tribId (\p → p
            { pfFeature   = HydroShape $ RiverFeature newTribRiver
            , pfParentId  = Just mainId
            , pfLastActivePeriod = periodIdx
            }) tbs
        tbs'' = updateFeature mainId (\p → p
            { pfFeature   = HydroShape $ RiverFeature newMainRiver
            , pfLastActivePeriod = periodIdx
            }) tbs'

    in tbs''

-- | Deepen a river's channel and valley
evolveDeepen ∷ Word64 → Int → Int
             → PersistentFeature → RiverParams
             → TimelineBuildState
             → (PersistentFeature, [GeoEvent], TimelineBuildState)
evolveDeepen seed ageIdx periodIdx pf river tbs =
    let fid = pfId pf
        GeoFeatureId fidInt = fid
        h2 = hashGeo seed fidInt (930 + ageIdx)
        deepenAmt = hashToRangeGeo h2 1 3
        maxDepth = 20  -- 200m cap
        newSegs = map (\seg → seg
            { rsDepth = min maxDepth (rsDepth seg + deepenAmt)
            , rsValleyWidth = rsValleyWidth seg + 1
            }) (rpSegments river)
        newRiver = river { rpSegments = newSegs }
        updatedPf = pf
            { pfFeature          = HydroShape $ RiverFeature newRiver
            , pfLastActivePeriod = periodIdx
            , pfEruptionCount    = pfEruptionCount pf + 1
            }
        tbs' = updateFeature fid (const updatedPf) tbs
    in (updatedPf, [HydroEvent (RiverFeature newRiver)], tbs')

-- | Match simulation rivers to existing persistent rivers.
--   Returns (matched pairs, unmatched existing, unmatched simulation).
--   A match is: source regions within matchRadius tiles of each other.
matchRivers ∷ [PersistentFeature] → [RiverParams]
            → ([(PersistentFeature, RiverParams)], [PersistentFeature], [RiverParams])
matchRivers existing simulated =
    let matchRadius = 1000

        -- Try to match each existing river to the closest sim river
        go [] remainingSim matched unmatched =
            (matched, unmatched, remainingSim)
        go (ep:eps) remainingSim matched unmatched =
            case findBestMatch ep remainingSim of
                Nothing →
                    go eps remainingSim matched (ep : unmatched)
                Just (simR, restSim) →
                    go eps restSim ((ep, simR) : matched) unmatched

        findBestMatch ep sims =
            let river = getRiverParamsFromPf ep
                GeoCoord sx sy = rpSourceRegion river
                candidates = filter (\sr →
                    let GeoCoord sx2 sy2 = rpSourceRegion sr
                        dx = abs (sx - sx2)
                        dy = abs (sy - sy2)
                    in dx < matchRadius ∧ dy < matchRadius
                    ) sims
            in case candidates of
                [] → Nothing
                _  → let best = minimumBy (\a b →
                             compare (srcDist a river) (srcDist b river)) candidates
                         restSim = filter (/= best) sims
                     in Just (best, restSim)

        srcDist sr existing =
            let GeoCoord x1 y1 = rpSourceRegion sr
                GeoCoord x2 y2 = rpSourceRegion existing
            in abs (x1 - x2) + abs (y1 - y2)

    in go existing simulated [] []

getRiverParamsFromPf ∷ PersistentFeature → RiverParams
getRiverParamsFromPf pf = case pfFeature pf of
    HydroShape (RiverFeature r) → r
    _ → error "getRiverParamsFromPf: not a river"

isLakeFeature ∷ FeatureShape → Bool
isLakeFeature (HydroShape (LakeFeature _)) = True
isLakeFeature _                            = False

-- | Trace a river at tile-level resolution from a source point
--   downhill to the ocean (or glacier, or max length).
--   Uses the elevation grid for fast approximate neighbor lookups,
--   falling back to elevationAtGlobal for precision.
--
--   Step size varies: 4-8 tiles per step to keep segment count
--   manageable while maintaining path detail.
traceRiverFromSource ∷ Word64 → Int → [TectonicPlate] → GeoTimeline
                     → Int → Int → Int → Int → Float
                     → Maybe RiverParams
traceRiverFromSource seed worldSize plates timeline gx gy srcElev riverIdx flow =
    let stepSize = 6
        maxSteps = 300  -- up to 1800 tiles long
        halfTiles = (worldSize * 16) `div` 2

        -- Get elevation at a tile using plates + full timeline
        getElev x y =
            let (x', y') = wrapGlobalU worldSize x y
            in if isBeyondGlacier worldSize x' y'
               then seaLevel + 500
               else let (baseE, baseMat) = elevationAtGlobal seed plates worldSize x' y'
                        (finalE, _) = applyTimelineFast timeline worldSize x' y' (baseE, baseMat)
                    in finalE

        -- 8-directional offsets
        dirs = [ (stepSize, 0), (-stepSize, 0), (0, stepSize), (0, -stepSize)
               , (stepSize, stepSize), (stepSize, -stepSize)
               , (-stepSize, stepSize), (-stepSize, -stepSize)
               ]

        go step curX curY curElev acc
            | step ≥ maxSteps = finishPath acc
            | curElev ≤ seaLevel = finishPath ((curX, curY, curElev) : acc)
            | isBeyondGlacier worldSize curX curY = finishPath acc
            | otherwise =
                let neighbors = map (\(dx, dy) →
                        let nx = curX + dx
                            ny = curY + dy
                            (nx', ny') = wrapGlobalU worldSize nx ny
                        in if isBeyondGlacier worldSize nx' ny'
                           then (nx', ny', curElev + 1000)
                           else (nx', ny', getElev nx' ny')
                        ) dirs

                    -- Pick lowest neighbor, with slight randomness
                    -- to avoid perfectly straight paths
                    sorted = sortByElevTriple neighbors
                    (bestX, bestY, bestElev) = case sorted of
                        [] → (curX, curY, curElev)
                        [(x, y, e)] → (x, y, e)
                        ((x1, y1, e1) : (x2, y2, e2) : _) →
                            let h = hashGeo seed step
                                    (1200 + abs curX `mod` 100 + abs curY `mod` 100)
                                wobble = hashToFloatGeo h
                            in if wobble < 0.75 ∨ e2 ≥ curElev
                               then (x1, y1, e1)
                               else (x2, y2, e2)

                in if bestElev ≥ curElev
                   -- No downhill path — we're in a basin.
                   -- Try to push through: if we're above sea level,
                   -- look for the lowest neighbor even if it's uphill,
                   -- simulating the river cutting through a low ridge.
                   then let lowestNeighbor = case sorted of
                                [] → Nothing
                                ((x, y, e):_) →
                                    -- Only push through if the ridge is
                                    -- shallow (< 15 tiles above us)
                                    if e < curElev + 15
                                    then Just (x, y, e)
                                    else Nothing
                        in case lowestNeighbor of
                            Just (nx, ny, ne) →
                                go (step + 1) nx ny ne
                                   ((curX, curY, curElev) : acc)
                            Nothing → finishPath acc
                   else go (step + 1) bestX bestY bestElev
                           ((curX, curY, curElev) : acc)

        finishPath acc =
            let path = reverse acc
            in if length path < 6  -- minimum 6 waypoints
               then Nothing
               else Just (buildRiverFromPath seed riverIdx flow path)

        waypoints = go 0 gx gy srcElev []
    in waypoints

sortByElevTriple ∷ [(Int, Int, Int)] → [(Int, Int, Int)]
sortByElevTriple [] = []
sortByElevTriple [x] = [x]
sortByElevTriple xs = foldr insertSorted [] xs
  where
    insertSorted x [] = [x]
    insertSorted x@(_, _, e1) (y@(_, _, e2) : ys)
        | e1 ≤ e2   = x : y : ys
        | otherwise  = y : insertSorted x ys

-- | Convert a traced path into RiverParams.
--   Enforces monotonic descent and computes per-segment properties.
buildRiverFromPath ∷ Word64 → Int → Float → [(Int, Int, Int)] → RiverParams
buildRiverFromPath seed riverIdx baseFlow path =
    let -- Enforce monotonic descent
        monoPath = enforceMonotonicPath path
        numWP = length monoPath
        segments = zipWith (buildSegFromWaypoints seed numWP baseFlow)
                           [0..] (zip monoPath (tail monoPath))
        (srcX, srcY, _) = head monoPath
        (mouthX, mouthY, _) = last monoPath
        totalFlow = case segments of
            [] → baseFlow
            _  → rsFlowRate (last segments)
    in RiverParams
        { rpSourceRegion = GeoCoord srcX srcY
        , rpMouthRegion  = GeoCoord mouthX mouthY
        , rpSegments     = segments
        , rpFlowRate     = totalFlow
        , rpMeanderSeed  = fromIntegral (hashGeo seed riverIdx 1150)
        }

enforceMonotonicPath ∷ [(Int, Int, Int)] → [(Int, Int, Int)]
enforceMonotonicPath [] = []
enforceMonotonicPath [x] = [x]
enforceMonotonicPath ((x0, y0, e0) : rest) =
    (x0, y0, e0) : go e0 rest
  where
    go _ [] = []
    go maxE ((x, y, e) : xs) =
        let e' = min maxE e
        in (x, y, e') : go e' xs

buildSegFromWaypoints ∷ Word64 → Int → Float → Int
                      → ((Int, Int, Int), (Int, Int, Int))
                      → RiverSegment
buildSegFromWaypoints seed totalSegs baseFlow segIdx ((sx, sy, se), (ex, ey, ee)) =
    let -- Flow accumulates along the path
        t = fromIntegral (segIdx + 1) / fromIntegral totalSegs
        flow = baseFlow + t * baseFlow * 2.0  -- triples by mouth

        rawWidth = max 2 (round (flow * 6.0))
        width = min 12 rawWidth

        h1 = hashGeo seed segIdx 1161
        valleyMult = 3.0 + hashToFloatGeo h1 * 3.0
        valleyW = max (width * 3) (round (fromIntegral width * valleyMult))

        slopeDelta = abs (se - ee)
        baseDepth = max 2 (slopeDelta `div` 3 + round (flow * 2.0))
        depth = min 20 baseDepth  -- 200m cap

    in RiverSegment
        { rsStart       = GeoCoord sx sy
        , rsEnd         = GeoCoord ex ey
        , rsWidth       = width
        , rsValleyWidth = valleyW
        , rsDepth       = depth
        , rsFlowRate    = flow
        , rsStartElev   = se
        , rsEndElev     = ee
        }

-- | Evolve an existing river that still has flow in the simulation.
--   Updates: segments (from sim, for path accuracy), flow rate,
--   deepens channel slightly, increments eruption count.
--   Preserves: feature ID, formation period, parent ID.
evolveMatchedRiver ∷ Word64 → Int → Int
                   → PersistentFeature → RiverParams
                   → TimelineBuildState
                   → (PersistentFeature, [GeoEvent], TimelineBuildState)
evolveMatchedRiver seed ageIdx periodIdx existPf simRiver tbs =
    let fid = pfId existPf
        oldRiver = getRiverParamsFromPf existPf
        GeoFeatureId fidInt = fid

        -- Deepen: each age the river persists, it cuts a bit deeper
        -- Amount depends on flow rate — bigger rivers erode faster
        h1 = hashGeo seed fidInt (900 + ageIdx)
        deepenAmt = max 1 (round (rpFlowRate simRiver * 2.0))
        -- Cap deepening so rivers don't get absurdly deep over 80 ages
        maxTotalDepth = 40
        
        -- Use simulation's path (it reflects current terrain) but
        -- carry forward accumulated depth from the old river
        mergedSegs = zipWithDefault mergeSegment
            (rpSegments oldRiver) (rpSegments simRiver)

        mergeSegment oldSeg newSeg = newSeg
            { rsDepth = min maxTotalDepth
                (rsDepth oldSeg + deepenAmt)
            , rsWidth = min 12 (max (rsWidth oldSeg) (rsWidth newSeg))
            , rsValleyWidth = max (rsValleyWidth oldSeg) (rsValleyWidth newSeg)
            }

        -- If sim has more segments, take them as-is with deepening
        -- If old has more segments (river shortened), drop the extras

        newRiver = simRiver
            { rpSegments    = mergedSegs
            , rpFlowRate    = rpFlowRate simRiver  -- use current flow
            , rpMeanderSeed = rpMeanderSeed oldRiver  -- preserve for continuity
            }

        updatedPf = existPf
            { pfFeature          = HydroShape $ RiverFeature newRiver
            , pfActivity         = FActive
            , pfLastActivePeriod = periodIdx
            , pfEruptionCount    = pfEruptionCount existPf + 1
            }

        evt = HydroEvent (RiverFeature newRiver)

        tbs' = updateFeature fid (const updatedPf) tbs

    in (updatedPf, [evt], tbs')

-- | Zip two lists, using only the shorter length.
--   When simulation changes the number of segments, we take
--   the simulation's count as authoritative.
zipWithDefault ∷ (a → b → b) → [a] → [b] → [b]
zipWithDefault _ [] bs     = bs  -- new segments from sim, no old data
zipWithDefault _ _  []     = []  -- river got shorter
zipWithDefault f (a:as) (b:bs) = f a b : zipWithDefault f as bs

-- | Reconcile simulation lakes with existing lake features.
--   Much simpler than rivers: lakes that are near existing ones
--   get skipped (the existing one persists). Genuinely new lakes
--   get created. Existing lakes not near any sim lake stay
--   (they might be dammed-river lakes, not tectonic).
reconcileLakes ∷ Word64 → Int → Int
              → [PersistentFeature] → [LakeParams]
              → TimelineBuildState
              → ([PersistentFeature], [GeoEvent], TimelineBuildState)
reconcileLakes _seed _ageIdx periodIdx existingLakes simLakes tbs =
    let lakeMatchRadius = 60

        isNearExisting lk =
            let GeoCoord lx ly = lkCenter lk
            in any (\epf → case pfFeature epf of
                HydroShape (LakeFeature elk) →
                    let GeoCoord ex ey = lkCenter elk
                    in abs (lx - ex) < lakeMatchRadius
                     ∧ abs (ly - ey) < lakeMatchRadius
                _ → False
                ) existingLakes

        newLakes = filter (not . isNearExisting) simLakes

        (pfs, evts, tbs') = foldl' (\(ps, es, st) lake →
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
            in (pf : ps, HydroEvent (LakeFeature lake) : es, st'')
            ) ([], [], tbs) newLakes

    in (pfs, evts, tbs')
