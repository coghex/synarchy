{-# OPTIONS_GHC -fprof-auto #-}
{-# LANGUAGE Strict, UnicodeSyntax #-}
-- | Eon → Era → Period → Epoch → Age timeline loops: the recursive
--   descent that builds one 'GeoPeriod' per structural tier, bottoming
--   out at per-Age erosion/hydrology/glacier/ore events. Entry point is
--   'buildEonLoop', called once from
--   'World.Geology.Timeline.buildTimeline'.
module World.Geology.Timeline.Loop
    ( buildEonLoop
    ) where
import UPrelude
import qualified Data.Text as T
import qualified Data.HashMap.Strict as HM
import qualified Data.Vector.Unboxed as VU
import World.Types
import World.Geology.Hash
import World.Geology.Crater (generateCraters)
import World.Geology.Ore (buildOreSheets)
import World.Hydrology.Simulation
    ( simulateHydrology, ElevGrid(..), updateElevGrid )
import World.Geology.Timeline.Helpers
    ( mkGeoPeriod, erosionFromGeoState, regionalErosionMap
    , isGlacierFeature, evolveGlacierCapped )
import World.Geology.Timeline.Volcanism
    ( applyPeriodVolcanism, applyVolcanicEvolution )
import World.Geology.Timeline.River
    ( reconcileHydrology, mergeConvergingRivers )
import World.Hydrology.Glacier (generateGlaciers)
import World.Hydrology.Glacier.Common (getGlacierParams)
import World.Weather.Types
import World.Weather.Generate (updateClimateFromGrid, oceanRegionsFromGrid)

-- * Primordial Bombardment

buildPrimordialBombardment ∷ Word64 → Int → [TectonicPlate]
                           → TimelineBuildState → ElevGrid
                           → (TimelineBuildState, ElevGrid)
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

-- Timeline depth comes from the player-configured TimelineParams, carried
-- on TimelineBuildState (tbsTimelineParams) and read by each generator
-- below. Eon + Era are fixed counts; Period/Epoch/Age roll a uniform count
-- in [min,max] per parent. See the project_timeline_depth memory.

-- | Eon loop. Each eon = primordial bombardment + first-climate snapshot
--   + its eras. eonCount > 1 models cataclysmic planetary resets.
buildEonLoop ∷ Word64 → Int → [TectonicPlate]
             → TimelineBuildState → ElevGrid
             → (TimelineBuildState, ElevGrid)
buildEonLoop seed worldSize plates tbs0 grid0 = go 0 tbs0 grid0
  where
    go eonIdx tbs grid
        | eonIdx ≥ tlpEonCount (tbsTimelineParams tbs) = (tbs, grid)
        | otherwise =
            let eonSeed = seed `xor` (fromIntegral eonIdx * 0x9E3779B97F4A7C15)
                (sB, gridB) = buildPrimordialBombardment eonSeed worldSize plates tbs grid
                -- First climate snapshot from the post-bombardment grid;
                -- cache the coarse ocean so per-Age climate reuses it.
                oceanE   = oceanRegionsFromGrid gridB worldSize
                climateE = updateClimateFromGrid worldSize oceanE
                               (tbsFeatures sB) (tbsClimateState sB)
                sB' = sB { tbsClimateState = climateE, tbsCoarseOcean = oceanE }
                (sE, gridE) = buildEraGen eonSeed worldSize plates sB' gridB
            in go (eonIdx + 1) sE gridE

-- | Fixed-count era generator (tlEraCount eras per eon).
buildEraGen ∷ Word64 → Int → [TectonicPlate]
            → TimelineBuildState → ElevGrid
            → (TimelineBuildState, ElevGrid)
buildEraGen seed worldSize plates tbs0 grid0 = go 0 tbs0 grid0
  where
    go eraIdx tbs grid
        | eraIdx ≥ tlpEraCount (tbsTimelineParams tbs) = (tbs, grid)
        | otherwise =
            let eraSeed = seed `xor` (fromIntegral eraIdx * 0xA1B2C3D4)
                (s1, grid1) = buildEra eraSeed worldSize plates eraIdx tbs grid
            in go (eraIdx + 1) s1 grid1

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
        nPeriods = hashToRangeGeo (hashGeo eraSeed eraIdx 401)
                       (tlpPeriodMin (tbsTimelineParams tbs)) (tlpPeriodMax (tbsTimelineParams tbs))
        (s2, grid2) = buildPeriodGen eraSeed worldSize plates nPeriods s1 grid
    in (s2, grid2)

-- | Count-rolled period generator (nPeriods periods for this era).
buildPeriodGen ∷ Word64 → Int → [TectonicPlate] → Int
               → TimelineBuildState → ElevGrid
               → (TimelineBuildState, ElevGrid)
buildPeriodGen seed worldSize plates nPeriods tbs0 grid0 = go 0 tbs0 grid0
  where
    go periodIdx tbs grid
        | periodIdx ≥ nPeriods = (tbs, grid)
        | otherwise =
            let periodSeed = seed `xor` (fromIntegral periodIdx * 0xB3C4D5E6)
                (s1, grid1) = buildPeriod periodSeed worldSize plates periodIdx tbs grid
            in go (periodIdx + 1) s1 grid1

buildPeriod ∷ Word64 → Int → [TectonicPlate] → Int
            → TimelineBuildState → ElevGrid
            → (TimelineBuildState, ElevGrid)
buildPeriod seed worldSize plates periodIdx tbs grid =
    let periodSeed = seed `xor` (fromIntegral periodIdx * 0xF1E2)
        (s1, grid1) = applyPeriodVolcanism periodSeed worldSize plates periodIdx tbs grid
        (s2, grid2) = applyVolcanicEvolution periodSeed worldSize plates s1 grid1
        nEpochs = hashToRangeGeo (hashGeo periodSeed periodIdx 501)
                      (tlpEpochMin (tbsTimelineParams tbs)) (tlpEpochMax (tbsTimelineParams tbs))
        (s3, grid3) = buildEpochGen periodSeed worldSize plates nEpochs s2 grid2
    in (s3, grid3)

-- | Count-rolled epoch generator (nEpochs epochs for this period).
--   Epochs are structural — they create no GeoPeriod, only bound the
--   age generator below.
buildEpochGen ∷ Word64 → Int → [TectonicPlate] → Int
              → TimelineBuildState → ElevGrid
              → (TimelineBuildState, ElevGrid)
buildEpochGen seed worldSize plates nEpochs tbs0 grid0 = go 0 tbs0 grid0
  where
    go epochIdx tbs grid
        | epochIdx ≥ nEpochs = (tbs, grid)
        | otherwise =
            let epochSeed = seed `xor` (fromIntegral epochIdx * 0xC5D6E7F8)
                (s1, grid1) = buildEpoch epochSeed worldSize plates epochIdx tbs grid
            in go (epochIdx + 1) s1 grid1

buildEpoch ∷ Word64 → Int → [TectonicPlate] → Int
           → TimelineBuildState → ElevGrid
           → (TimelineBuildState, ElevGrid)
buildEpoch seed worldSize plates epochIdx tbs grid =
    let epochSeed = seed `xor` (fromIntegral epochIdx * 0xA1A2)
        -- Epoch is purely structural — no period is created here, so
        -- no gsDate advance. Time accumulates from the Ages below
        -- (audit #5: keep gsDate in lockstep with gpDuration sums).
        nAges = hashToRangeGeo (hashGeo epochSeed epochIdx 601)
                    (tlpAgeMin (tbsTimelineParams tbs)) (tlpAgeMax (tbsTimelineParams tbs))
        (s2, grid') = buildAgeGen epochSeed worldSize plates nAges tbs grid
    in (s2, grid')

-- | Count-rolled age generator (nAges ages for this epoch).
buildAgeGen ∷ Word64 → Int → [TectonicPlate] → Int
            → TimelineBuildState → ElevGrid
            → (TimelineBuildState, ElevGrid)
buildAgeGen seed worldSize plates nAges tbs0 grid0 = go 0 tbs0 grid0
  where
    go ageIdx tbs grid
        | ageIdx ≥ nAges = (tbs, grid)
        | otherwise =
            let ageSeed = seed `xor` (fromIntegral ageIdx * 0xD7E8F9A0)
                (s1, grid1) = buildAge ageSeed worldSize plates ageIdx tbs grid
            in go (ageIdx + 1) s1 grid1

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

        -- Per-age eruption events are no longer emitted (Magma overlay
        -- handles lava placement directly). The CO2 spike below still
        -- uses the same hash-based "would this volcano erupt this age?"
        -- roll, so climate dynamics keep the eruption-driven pulse
        -- without the now-obsolete GeoEvent path.
        eruptSeed = ageSeed `xor` 0x1A7A

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

        (_hydroFeatures, hydroEvents, tbs_h0) = reconcileHydrology
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

        -- === ORE DEPOSITION ===
        -- Sediment shed by this age's volcanic edifices, routed down
        -- the age's drainage field and settled into flat sheets
        -- (World.Geology.Ore). Runs on the SAME flowResult the rivers
        -- use, so deposits follow river valleys downhill. True
        -- deposition: the events raise terrain in the strata replay
        -- and later ages bury them.
        oreEvents = buildOreSheets ageSeed (tbsPeriodIdx tbs)
                                   (tbsOreLevers tbs) worldSize duration
                                   (tbsFeatures tbs_g1) elevGrid flowResult

        allEvents = meteorites <> hydroEvents
                 <> newGlacierEvents <> glacierEvolveEvents
                 <> activeGlacierRecarve <> oreEvents

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

-- * River + Glacier helpers

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
