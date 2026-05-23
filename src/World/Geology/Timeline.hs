{-# OPTIONS_GHC -fprof-auto #-}
{-# LANGUAGE Strict, UnicodeSyntax #-}
module World.Geology.Timeline
    ( buildTimeline
    ) where
import UPrelude
import Data.Bits (xor)
import Data.Word (Word64)
import qualified Data.Text as T
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as VU
import qualified Data.HashMap.Strict as HM
import qualified Data.HashSet as HS
import World.Base (GeoCoord(..), GeoFeatureId(..))
import World.Types
import World.Plate (generatePlates, TectonicPlate)
import World.Geology.Types
import World.Geology.Hash
import World.Geology.Crater (generateCraters)
import World.Hydrology.Types (HydroFeature(..), RiverParams(..)
                             , LakeParams(..), GlacierParams(..))
import World.Fluid.IceLevel (computeIceLevelGrid)
import World.Hydrology.Simulation (simulateHydrology, FlowResult(..)
                                  , ElevGrid(..), buildInitialElevGrid
                                  , updateElevGrid)
import World.Geology.Timeline.Helpers
    ( mkGeoPeriod, erosionFromGeoState, regionalErosionMap
    , isGlacierFeature, evolveGlacierCapped, updateLakeSpillways )
import World.Geology.Timeline.Volcanism
    ( applyPeriodVolcanism, applyVolcanicEvolution, generateEruption )
import World.Geology.Timeline.River
    ( reconcileHydrology, mergeConvergingRivers )
import World.Constants (seaLevel)
import World.Geology.Hash (hashGeo, hashToFloatGeo, scaleCount)
import World.Hydrology.Glacier (generateGlaciers)
import World.Hydrology.Glacier.Common (getGlacierParams)
import World.Weather.Types
import World.Weather.Generate (updateClimateFromGrid, oceanRegionsFromGrid)

-- * Top Level

buildTimeline ∷ Word64 → Int → Int → Float → Float → (GeoTimeline, ClimateState)
buildTimeline seed worldSize plateCount erosionIntensity volcanicActivity =
    let plates = generatePlates seed worldSize plateCount
        gs0 = initGeoState seed worldSize plates

        tbs0 = TimelineBuildState
            { tbsFeatures  = []
            , tbsNextId    = 0
            , tbsPeriods   = []
            , tbsPeriodIdx = 0
            , tbsGeoState  = gs0
            , tbsClimateState = initClimateState worldSize
            , tbsErosionIntensity = erosionIntensity
            , tbsVolcanicActivity = volcanicActivity
            , tbsCoarseOcean = HS.empty
            }

        grid0 = buildInitialElevGrid seed worldSize plates
        (s1, grid1) = buildPrimordialBombardment seed worldSize plates tbs0 grid0

        -- After bombardment: first climate snapshot from the
        -- initial plate-derived elevation grid. Cache the coarse
        -- ocean shape so per-Age climate updates can reuse it
        -- without re-deriving from the evolving grid.
        ocean1 = oceanRegionsFromGrid grid1 worldSize
        climate1 = updateClimateFromGrid worldSize ocean1 (tbsFeatures s1) (tbsClimateState s1)
        s1' = s1 { tbsClimateState = climate1, tbsCoarseOcean = ocean1 }

        (s2, finalGrid) = buildEon seed worldSize plates s1' grid1

        -- Mark the most recent Age period for soil generation.
        -- Walk from head until we find a period with gpScale = Age,
        -- so the invariant is enforced explicitly instead of
        -- relying on "head of tbsPeriods is always an Age" being
        -- true by construction (audit #24).
        markAsLastAge p = p
            { gpErosion = (gpErosion p) { epIsLastAge = True }
            , gpRegionalErosion = HM.map (\ep → ep { epIsLastAge = True })
                                         (gpRegionalErosion p)
            }
        markFirstAge [] = []
        markFirstAge (p:ps)
            | gpScale p ≡ Age = markAsLastAge p : ps
            | otherwise       = p : markFirstAge ps
        finalPeriods = markFirstAge (tbsPeriods s2)

        periods = reverse finalPeriods

        -- Pre-compute all river carve events across all periods.
        -- This is cached on GeoTimeline so applyTimeline/Fast don't
        -- recompute it per-tile (was 35% of CPU in profiling).
        riverExploded = V.concat
            [ V.filter (\(evt, _) → isRiverCarveEvtCached evt)
                       (gpExplodedEvents p)
            | p ← periods
            ]

        -- Lakes were created from the raw filledElev pre-timeline;
        -- post-process them so their lkSurface reflects post-carve
        -- spillway (audit #13).
        finalFeatures = updateLakeSpillways worldSize finalGrid (tbsFeatures s2)

        rawTimeline = GeoTimeline
            { gtSeed      = seed
            , gtWorldSize = worldSize
            , gtPeriods   = periods
            , gtFeatures  = finalFeatures
            , gtRiverExplodedEvents = riverExploded
            , gtIceLevel  = computeIceLevelGrid seed worldSize plates
                                                 (tbsClimateState s2) finalGrid
            }

    in (rawTimeline, tbsClimateState s2)

isRiverCarveEvtCached ∷ GeoEvent → Bool
isRiverCarveEvtCached (HydroEvent (RiverFeature _)) = True
isRiverCarveEvtCached (HydroEvent (GlacierFeature _)) = True
isRiverCarveEvtCached (RiverSegmentEvent _) = True
isRiverCarveEvtCached (RiverDeltaEvent _) = True
isRiverCarveEvtCached _ = False

-- * Primordial Bombardment

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
            (ErosionParams 0.8 0.3 0.6 0.4 0.1 (seed + 1000)
                           200.0 0.0 0.0 0.0 False)
            HM.empty
        tbs' = addPeriod period (tbs { tbsGeoState = gs' })
        grid' = updateElevGrid worldSize grid period
    in (tbs', grid')

-- * Eon → Era → Period → Epoch → Age loops

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

buildEra ∷ Word64 → Int → [TectonicPlate] → Int
         → TimelineBuildState → ElevGrid
         → (TimelineBuildState, ElevGrid)
buildEra seed worldSize plates eraIdx tbs grid =
    let eraSeed = seed `xor` (fromIntegral eraIdx * 0xE1A2)
        gs = tbsGeoState tbs
        currentDate = gdMillionYears (gsDate gs)
        gs' = gs { gsDate = advanceGeoDate 100.0 (gsDate gs) }

        -- === ERA-BOUNDARY OCEAN REFRESH ===
        -- Refresh the cached coarse ocean shape from the current
        -- ElevGrid. This is what per-Age climate updates use (see
        -- buildAge). Per-Era cadence keeps it stable enough that
        -- intra-Era erosion can't feed back into climate, but loose
        -- enough that long-term ocean shape changes are captured.
        coarseOcean = oceanRegionsFromGrid grid worldSize

        eraPeriod = mkGeoPeriod worldSize
            ("Era " <> T.pack (show eraIdx) <> " Events")
            Era 100 currentDate []
            (ErosionParams (tbsErosionIntensity tbs) 0.5 0.5 0.3 0.2 (seed + 3000 + fromIntegral eraIdx)
                           15.0 0.5 0.5 0.0 False)
            HM.empty
        s1 = addPeriod eraPeriod (tbs { tbsGeoState = gs'
                                       , tbsCoarseOcean = coarseOcean })
        (s2, grid2) = buildPeriodLoop eraSeed worldSize plates 0 2 4 s1 grid
    in (s2, grid2)

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
        -- Epoch is purely structural — no period is created here, so
        -- no gsDate advance. Time accumulates from the Ages below
        -- (audit #5: keep gsDate in lockstep with gpDuration sums).
        (s2, grid') = buildAgeLoop epochSeed worldSize plates 0 1 8 tbs grid
    in (s2, grid')

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
            continue = ageIdx < minAges ∨ roll < 0.4
        in if continue
           then buildAgeLoop seed worldSize plates (ageIdx + 1) minAges maxAges s1 grid1
           else (s1, grid1)

buildAge ∷ Word64 → Int → [TectonicPlate] → Int
         → TimelineBuildState → ElevGrid
         → (TimelineBuildState, ElevGrid)
buildAge seed worldSize plates ageIdx tbs0 elevGrid =
    let ageSeed = seed `xor` (fromIntegral ageIdx * 0xF0F1)
        gs = tbsGeoState tbs0
        currentDate = gdMillionYears (gsDate gs)
        durationHash = hashGeo ageSeed ageIdx 610
        duration = 1.0 + hashToFloatGeo durationHash * 14.0 ∷ Float
        -- Round once and use for BOTH the gsDate advance and the
        -- period's gpDuration so the geological clock and the
        -- erosion-time integer stay in lockstep (audit #26). Rate
        -- calculations below (meteoriteChance, weatheringRate)
        -- still use the unrounded Float for sub-Myr precision in
        -- their probability/rate inputs.
        durationI = round duration ∷ Int
        gs1 = gs { gsDate = advanceGeoDate (fromIntegral durationI) (gsDate gs) }

        -- === FRESH CLIMATE AT AGE BOUNDARY ===
        -- Recompute climate using the cached coarse ocean (refreshed
        -- at Era boundary) plus the current CO2 and persistent
        -- features. The CO2 sync absorbs any Period-tier volcanism
        -- that bumped gsCO2 without going through climate (audit #3).
        -- Holding the ocean shape stable within an Era avoids the
        -- erosion→wetter-climate→more-erosion feedback loop, while
        -- CO2-derived temperature and freshwater-moisture corridors
        -- still track per-Age evolution.
        prevClimateSynced = (tbsClimateState tbs0) { csGlobalCO2 = gsCO2 gs }
        climate = updateClimateFromGrid worldSize (tbsCoarseOcean tbs0)
                      (tbsFeatures tbs0) prevClimateSynced
        tbs = tbs0 { tbsClimateState = climate }

        meteoriteRoll = hashToFloatGeo (hashGeo ageSeed ageIdx 620)
        meteoriteChance = min 0.85 (duration / 15.0)
        meteorites = if meteoriteRoll < meteoriteChance
            then let craterSeed = ageSeed `xor` 0xBEEF
                     craters = generateCraters craterSeed worldSize plates CraterEra_Late
                 in take (hashToRangeGeo (hashGeo ageSeed ageIdx 621) 1 5)
                         (map CraterEvent craters)
            else []

        eruptSeed = ageSeed `xor` 0x1A7A
        eruptions = catMaybes
            [ generateEruption (tbsVolcanicActivity tbs) eruptSeed worldSize ageIdx plates pf
            | pf ← tbsFeatures tbs
            , case eruptionProfile (pfFeature pf) of
                Just ep → epTimelineScale ep ≡ Age
                Nothing → False
            ]

        -- === CO2 SPIKE FROM ERUPTIONS ===
        -- Each eruption bumps CO2. Super volcanoes bump it a lot.
        eruptionCO2Boost = sum
            [ case pfFeature pf of
                VolcanicShape (SuperVolcano _) → 0.15  -- massive CO2 injection
                _ → 0.02                               -- normal eruption
            | pf ← tbsFeatures tbs
            , pfActivity pf ≡ FActive
            , case eruptionProfile (pfFeature pf) of
                Just ep → epTimelineScale ep ≡ Age
                Nothing → False
            , let GeoFeatureId fidInt = pfId pf
                  h = hashGeo eruptSeed fidInt (700 + ageIdx)
                  roll = hashToFloatGeo h
                  scaledChance = maybe 0.0 (\ep → min 1.0 (epEruptChance ep * tbsVolcanicActivity tbs)) (eruptionProfile (pfFeature pf))
              in roll < scaledChance
            ]

        hydroSeed = ageSeed `xor` 0xA0CA71C
        flowResult = simulateHydrology hydroSeed worldSize ageIdx elevGrid
                                       (tbsClimateState tbs)

        (hydroFeatures, hydroEvents, tbs_h0) = reconcileHydrology
            hydroSeed ageIdx flowResult (tbsPeriodIdx tbs) worldSize
            elevGrid tbs

        tbs_h = mergeConvergingRivers worldSize (tbsPeriodIdx tbs) tbs_h0

        -- === GLACIER GENERATION + EVOLUTION ===
        glacierSeed = ageSeed `xor` 0x61AC1E5
        existingGlaciers = filter (\pf → isGlacierFeature (pfFeature pf)
                                       ∧ (pfActivity pf ≡ FActive
                                        ∨ pfActivity pf ≡ FDormant))
                                  (tbsFeatures tbs_h)
        glacierCap = scaleCount worldSize 4

        -- Generate new glaciers if below minimum population
        (newGlacierPfs, tbs_g0) =
            if length existingGlaciers < glacierCap
            then generateGlaciers glacierSeed worldSize plates gs1
                     (tbsPeriodIdx tbs) tbs_h
            else ([], tbs_h)
        newGlacierEvents = map (\pf → HydroEvent (pfToGlacierFeature pf))
                               newGlacierPfs

        -- Evolve existing glaciers (advance/retreat/surge/melt)
        branchCap = scaleCount worldSize 8
        canBranch = length existingGlaciers < branchCap
        (glacierEvolveEvents, tbs_g1) =
            foldl' (evolveGlacierCapped glacierSeed canBranch
                        (tbsPeriodIdx tbs) gs1)
                   ([], tbs_g0) existingGlaciers

        -- Re-emit carving events for all still-active glaciers
        -- This compounds carving across ages — deeper fjords each age
        activeGlacierRecarve =
            [ HydroEvent (pfToGlacierFeature pf)
            | pf ← tbsFeatures tbs_g1
            , isGlacierFeature (pfFeature pf)
            , pfActivity pf ≡ FActive
            ]

        allEvents = meteorites <> eruptions <> hydroEvents
                 <> newGlacierEvents <> glacierEvolveEvents
                 <> activeGlacierRecarve

        -- === BIDIRECTIONAL CO2 SYNC ===
        -- CO2 rises from eruptions, decays from weathering.
        -- Weathering rate depends on temperature and precipitation
        -- (warmer + wetter = faster silicate weathering = more CO2 drawdown)
        avgTemp = csGlobalTemp climate
        avgPrecip = climateAvgPrecip climate
        weatheringRate = duration * 0.005
                       * (1.0 + 0.5 * max 0.0 (avgTemp - 10.0) / 20.0)
                       * (1.0 + 0.3 * avgPrecip)
        -- Milankovitch-like CO2 oscillation for ice age cycles
        iceAgePhase = currentDate * 0.4
        co2Oscillation = 0.2 * sin (iceAgePhase * 2.0 * pi)
        newCO2 = max 0.4 (gsCO2 gs1 + eruptionCO2Boost - weatheringRate + co2Oscillation)

        gs2 = gs1 { gsCO2 = newCO2 }

        -- === CLIMATE-AWARE EROSION ===
        erosion = erosionFromGeoState (tbsErosionIntensity tbs) gs2 climate seed ageIdx False
        regErosion = regionalErosionMap (tbsErosionIntensity tbs) gs2 climate seed ageIdx False

        _debugLandCount = VU.length (VU.filter id (egLand elevGrid))
        _debugGridW = egGridW elevGrid

        period = mkGeoPeriod worldSize
            ("Age " <> T.pack (show (tbsPeriodIdx tbs))
             <> " [grid=" <> T.pack (show _debugGridW)
             <> " land=" <> T.pack (show _debugLandCount) <> "]")
            Age durationI currentDate
            allEvents erosion regErosion

        -- Update climate's CO2 to stay in sync
        climate' = climate { csGlobalCO2 = newCO2 }
        tbs_final = addPeriod period (tbs_g1 { tbsGeoState = gs2
                                              , tbsClimateState = climate' })
        elevGrid' = updateElevGrid worldSize elevGrid period

    in (tbs_final, elevGrid')

-- * Glacier helpers

pfToGlacierFeature ∷ PersistentFeature → HydroFeature
pfToGlacierFeature pf = GlacierFeature (getGlacierParams pf)

-- * Average precipitation

climateAvgPrecip ∷ ClimateState → Float
climateAvgPrecip cs =
    let regions = cgRegions (csClimate cs)
    in if HM.null regions then 0.5
       else let total = HM.foldl' (\acc rc →
                    let SeasonalClimate s w = rcPrecipitation rc
                    in acc + (s + w) / 2.0
                    ) 0.0 regions
            in total / fromIntegral (HM.size regions)
