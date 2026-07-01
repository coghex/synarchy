{-# OPTIONS_GHC -fprof-auto #-}
{-# LANGUAGE Strict, UnicodeSyntax #-}
module World.Geology.Timeline
    ( buildTimeline
    ) where
import UPrelude
import qualified Data.Text as T
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as VU
import qualified Data.HashMap.Strict as HM
import qualified Data.HashSet as HS
import World.Types
import World.Plate (generatePlates, wrapGlobalU, elevationAtGlobal)
import World.Material (MaterialRegistry, MaterialId, matGlacier)
import World.Generate.Constants (chunkBorder)
import World.Fluid.River (fixupSegmentContinuity)
import World.Generate.Timeline.Fast (applyTimelineFastFrom)
import World.Geology.Hash
import World.Geology.Crater (generateCraters)
import World.Fluid.IceLevel (computeIceLevelGrid)
import World.Fluid.Lake.Identify (identifyWorldLakes, computeRenderedOcean)
import World.Fluid.Seabed (identifySeabed)
import World.Fluid.Seabed.Types (emptySeabedTable)
import World.Fluid.OceanMask (buildWorldOceanMask, emptyWorldOceanMask)
import World.Geology.Ore (buildOreSheets, buildOreDepositTable)
import World.Geology.Ore.Types (OreLevers, emptyWorldOreDeposits)
import World.Magma.Init (buildVolcanoCtx)
import World.Magma.Pool (identifyLavaPools)
import World.Fluid.Lake.Types (emptyWorldLakes, WorldLakes(..))
import World.Fluid.River.Identify (identifyWorldRivers)
import World.Fluid.River.Types (emptyWorldRivers)
import World.Fluid.Ocean (computeOceanMap)
import World.Generate.InitTerrain
    (BorderedTerrainCache, borderedToInterior
    , computeChunkTimelinePipeline, finishBorderedPipeline)
import World.Geology.Coastal (identifyCoastalErosion)
import World.Geology.Coastal.Types (CoastalTable, emptyCoastalTable)
import World.Generate.Timeline (applyTimelineFast)
import qualified Data.Vector.Unboxed.Mutable as VUM
import Control.Parallel.Strategies (parListChunk, rdeepseq, using)
import World.Hydrology.Simulation (simulateHydrology
                                   , ElevGrid(..)
                                   , buildInitialElevGrid
                                   , updateElevGrid)
import World.Geology.Timeline.Helpers
    ( mkGeoPeriod, erosionFromGeoState, regionalErosionMap
    , isGlacierFeature, isActiveRiver, getRiverParamsFromPf
    , evolveGlacierCapped, updateLakeSpillways )
import World.Geology.Timeline.Volcanism
    ( applyPeriodVolcanism, applyVolcanicEvolution )
import World.Geology.Timeline.River
    ( reconcileHydrology, mergeConvergingRivers )
import World.Hydrology.Glacier (generateGlaciers)
import World.Hydrology.Glacier.Common (getGlacierParams)
import World.Weather.Types
import World.Weather.Generate (updateClimateFromGrid, oceanRegionsFromGrid)

-- * Top Level

buildTimeline ∷ MaterialRegistry → Word64 → Int → Int → Float → Float
              → Int    -- ^ lava pool depth lever (config)
              → Int    -- ^ lava pool radius lever (config)
              → Int    -- ^ waterfall quantum lever (config)
              → OreLevers -- ^ resource-abundance levers (config)
              → TimelineParams -- ^ timeline depth (config)
              → ( GeoTimeline, ClimateState, BorderedTerrainCache
                , OceanMap, OceanDistMap )
                -- ^ Ocean map + distances are returned (not recomputed
                --   by Init) so the seabed mask and chunk-gen ocean
                --   classification share ONE map — otherwise they
                --   disagree and the deep floor keeps clay instead of
                --   seabed muck.
buildTimeline registry seed worldSize plateCount erosionIntensity volcanicActivity
              lavaPoolDepth lavaPoolRadius waterfallQuantum oreLevers timelineParams =
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
            , tbsOreLevers = oreLevers
            , tbsTimelineParams = timelineParams
            , tbsCoarseOcean = HS.empty
            }

        grid0 = buildInitialElevGrid seed worldSize plates

        -- Eon loop (timeline depth). Each eon runs its own primordial
        -- bombardment + first-climate snapshot + eras, so eonCount > 1
        -- models a cataclysmic planetary reset (atmosphere lost +
        -- regained, re-bombarded). See buildEonLoop.
        (s2, finalGrid) = buildEonLoop seed worldSize plates tbs0 grid0

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

        -- Strip stale `HydroEvent (RiverFeature _)` from every period
        -- and emit fresh ones — one per currently-active river, using
        -- its FINAL persistent state — into the most-recent Age period.
        --
        -- This is what makes river evolution actually shape terrain.
        -- Without it, each river's only carve event is the one emitted
        -- at its birth Age; meander/deepen/widen/merge updates live in
        -- `tbsFeatures` but never reach the carve pipeline.
        --
        -- Per-Age re-emit (mirroring `activeGlacierRecarve`) was tried
        -- first but quickly blows up: event count goes O(rivers × ages)
        -- and per-tile carve-fold cost scales linearly, causing 25–50×
        -- world-gen slowdowns. Compaction keeps event count = num active
        -- rivers while still landing the final state in the carve pass.
        --
        -- As a side-benefit, merged tributaries no longer leave a
        -- phantom carve past their junction — their birth-Age event
        -- (with the full pre-merge path) is removed; only the post-merge
        -- truncated segments carve.
        periodsWithRiverRecarve =
            compactRiverEvents seed plates registry worldSize
                               (tbsFeatures s2) finalPeriods

        periods = reverse periodsWithRiverRecarve

        -- Pre-compute all river carve events across all periods.
        -- This is cached on GeoTimeline so applyTimeline/Fast don't
        -- recompute it per-tile (was 35% of CPU in profiling).
        riverExploded = V.concat
            [ V.filter (\(evt, _) → isRiverCarveEvent evt)
                       (gpExplodedEvents p)
            | p ← periods
            ]

        -- Lakes were created from the raw filledElev pre-timeline;
        -- post-process them so their lkSurface reflects post-carve
        -- spillway (audit #13).
        finalFeatures = updateLakeSpillways worldSize finalGrid (tbsFeatures s2)

        -- Pre-lake timeline: everything except gtWorldLakes. We need
        -- the timeline filled in so 'computeChunkInteriorTerrain' can
        -- pass it to 'applyTimelineChunk' below to produce the same
        -- terrain chunk-gen will produce.
        preLakeTimeline = GeoTimeline
            { gtSeed      = seed
            , gtWorldSize = worldSize
            , gtPeriods   = periods
            , gtFeatures  = finalFeatures
            , gtRiverExplodedEvents = riverExploded
            , gtIceLevel  = computeIceLevelGrid seed worldSize plates
                                                 (tbsClimateState s2) finalGrid
            , gtWorldLakes  = emptyWorldLakes
            , gtWorldRivers = emptyWorldRivers
            , gtWorldLavaPools = emptyWorldLakes
            , gtCoastal = emptyCoastalTable
            , gtSeabed = emptySeabedTable
            , gtWorldOcean = emptyWorldOceanMask
            , gtOreDeposits = emptyWorldOreDeposits
            }

        -- Ocean map: which chunks are ocean-BFS-reachable from the
        -- world edge. Used by the lake identifier below; Init.hs
        -- computes its own copy after buildTimeline — redundant work
        -- but cheap (chunk-resolution sampling).
        applyTL gx gy base =
            applyTimelineFast preLakeTimeline plates worldSize gx gy
                              registry base
        (oceanMap, oceanDist) =
            computeOceanMap seed worldSize plateCount plates applyTL

        -- Stage 1: per-chunk TIMELINE windows (plate-base →
        -- applyTimelineChunk), the expensive part, parallelized.
        -- Their per-tile values are window-position-independent, so
        -- stitching their interiors gives the unambiguous global
        -- pre-coastal terrain.
        timelineCache = buildTimelineStageCache seed plates worldSize
                                                registry preLakeTimeline

        (preCoastElev, preCoastMat) =
            stitchWorldGrids worldSize timelineCache

        -- Global coastal pass (save v25): smoothing + erosion run
        -- ONCE on the stitched grid — no window edges, so adjacent
        -- chunks can never disagree about the coastline (the old
        -- per-window pass manufactured seam cliffs up to ~18z).
        coastMouths =
            [ (mx, my)
            | p ← periods
            , HydroEvent (RiverFeature rp) ← gpEvents p
            , let GeoCoord mx my = rpMouthRegion rp
            ]
        coastalTable = identifyCoastalErosion seed worldSize plates
                                              registry coastMouths
                                              preCoastElev preCoastMat

        -- Stage 2: apply the table + despike per chunk — cheap.
        -- The result has the exact shape the old single-stage cache
        -- had; buildWorldTerrain slices interiors for the global
        -- priority flood and the zoom-cache build reads the entries
        -- via 'wgpBorderedCache'.
        borderedCache = finishBorderedCache coastalTable timelineCache

        worldTerrain = stitchWorldTerrain worldSize borderedCache

        -- TWO-STAGE HYDROLOGY (intentional, not redundant). Above, the
        -- per-age loop already ran 'simulateHydrology'/'reconcileHydrology'
        -- to carve GEOLOGICAL HISTORY into the evolving grid — migrating
        -- rivers, widening valleys, terraces (the foundation for layered
        -- valleys). Below, the global identifiers run on the now-SETTLED
        -- 'worldTerrain' as the final PLACEMENT authority and emit a
        -- compose-time channel-bed fit ('wrCarveDelta', see
        -- 'computeCarveDelta'). The two are distinct STAGES on distinct
        -- inputs: the final fit needs the finalized path/width/surface
        -- that only exist now, and the geological carve can't be dropped
        -- without losing valley evolution. They don't double-carve — the
        -- delta is a bounded top-up to channel depth on terrain the
        -- per-age erosion already shaped.
        finalLakes0 = identifyWorldLakes worldSize oceanMap worldTerrain

        -- The seabed pass (below) supersedes the flat seaLevel−1
        -- floor carve for sub-sea basins, so drop the lake table's
        -- carve deltas — seabed covers every clamped-lake tile and
        -- carves it to the ramp instead (which is still ≤ seaLevel−1,
        -- so the surface fills). Keeps the two mechanisms from
        -- double-carving.
        finalLakes = finalLakes0 { wlCarveDelta = HM.empty }

        finalRivers = identifyWorldRivers worldSize finalLakes
                                          worldTerrain
                                          (tbsClimateState s2)
                                          waterfallQuantum

        -- Global seabed pass (save v26): continental-margin relief
        -- (shelf + slope profile blended with the natural floor),
        -- materials, and bedrock outcrops — replacing the dead-flat
        -- seaLevel−1 plains.
        seabedTable = identifySeabed seed worldSize oceanDist
                                     finalLakes0 worldTerrain

        -- Tile-resolution rendered ocean (save v27): every sub-sea tile
        -- in a component that renders as ocean anywhere (edge-connected
        -- OR an enclosed inland sea with an oceanic-chunk core).
        -- Propagated to composeFluidMap so sub-sea tiles the coarse
        -- chunk-flood missed still render ocean (the
        -- sea-stops-at-a-chunk-boundary fix).
        worldOceanMask = buildWorldOceanMask worldSize
                             (computeRenderedOcean worldSize oceanMap
                                                   worldTerrain)

        -- Lava pools: same stitched terrain, lakes + rivers as water
        -- barriers. The ctx here is a throwaway built only for pool
        -- identification — Init.hs builds the persistent one for
        -- chunk gen from the same inputs (deterministic, identical).
        poolCtx = buildVolcanoCtx seed worldSize plates finalFeatures
        lavaPools = identifyLavaPools worldSize
                                      lavaPoolDepth lavaPoolRadius
                                      poolCtx finalLakes
                                      finalRivers worldTerrain

        rawTimeline = preLakeTimeline
            { gtWorldLakes  = finalLakes
            , gtWorldRivers = finalRivers
            , gtWorldLavaPools = lavaPools
            , gtCoastal = coastalTable
            , gtSeabed = seabedTable
            , gtWorldOcean = worldOceanMask
            , gtOreDeposits = buildOreDepositTable worldSize periods
            }

    in (rawTimeline, tbsClimateState s2, borderedCache, oceanMap, oceanDist)

-- | Build the per-chunk bordered-pipeline cache. Runs the full
--   plate-base → applyTimelineChunk → applyCoastalErosion → despike
--   pipeline once per chunk and stashes the bordered (elev, mat)
--   result in a HashMap indexed by chunk coord.
--
--   Per-chunk work is parallelized via @parListChunk 64 rdeepseq@
--   (forcing each chunk's vectors to NF inside its spark so they
--   land already-evaluated when the consumer reads them).
buildTimelineStageCache
    ∷ Word64
    → [TectonicPlate]
    → Int
    → MaterialRegistry
    → GeoTimeline
    → BorderedTerrainCache
buildTimelineStageCache seed plates worldSize registry timeline =
    let halfChunks = worldSize `div` 2
        allCoords  = [ ChunkCoord cx cy
                     | cx ← [-halfChunks .. halfChunks - 1]
                     , cy ← [-halfChunks .. halfChunks - 1]
                     ]
        runOne coord =
            ( coord
            , computeChunkTimelinePipeline seed plates worldSize
                                            registry timeline coord
            )
        chunkResults = map runOne allCoords
                       `using` parListChunk 64 rdeepseq
    in HM.fromList chunkResults

-- | Stage 2 over the whole cache: apply the global coastal table +
--   despike per chunk. Cheap relative to stage 1 (table lookups +
--   one despike pass) but still parallelized — w256 has 65k chunks.
finishBorderedCache
    ∷ CoastalTable
    → BorderedTerrainCache
    → BorderedTerrainCache
finishBorderedCache coastal timelineCache =
    let finished = [ (coord, finishBorderedPipeline coastal coord entry)
                   | (coord, entry) ← HM.toList timelineCache
                   ] `using` parListChunk 64 rdeepseq
    in HM.fromList finished

-- | Stitch per-chunk TIMELINE-stage entries into world-resolution
--   elevation + material grids, indexed
--   @(gy + half) * worldTiles + (gx + half)@. Beyond-glacier tiles
--   get 'minBound' / 'matGlacier'. This is the global pre-coastal
--   world that 'identifyCoastalErosion' runs against.
stitchWorldGrids
    ∷ Int                  -- ^ worldSize
    → BorderedTerrainCache
    → (VU.Vector Int, VU.Vector MaterialId)
stitchWorldGrids worldSize cache =
    let worldTiles = worldSize * chunkSize
        halfWorld  = worldTiles `div` 2
        borderSize = chunkSize + 2 * chunkBorder
        toBIdx lx ly = (ly + chunkBorder) * borderSize + (lx + chunkBorder)
        elevG = stitchWorldTerrain worldSize cache
        matG = VU.create $ do
            v ← VUM.replicate (worldTiles * worldTiles) matGlacier
            forM_ (HM.toList cache) $ \(ChunkCoord cx cy, (_, bMat)) → do
                let baseGX = cx * chunkSize
                    baseGY = cy * chunkSize
                forM_ [0 .. chunkSize * chunkSize - 1] $ \i → do
                    let lx     = i `mod` chunkSize
                        ly     = i `div` chunkSize
                        gxOff  = baseGX + lx + halfWorld
                        gyOff  = baseGY + ly + halfWorld
                        wIdx   = gyOff * worldTiles + gxOff
                    when (gxOff ≥ 0 ∧ gxOff < worldTiles
                        ∧ gyOff ≥ 0 ∧ gyOff < worldTiles) $
                        VUM.write v wIdx (bMat VU.! toBIdx lx ly)
            pure v
    in (elevG, matG)

-- | Stitch per-chunk bordered cache entries into a world-resolution
--   terrain grid indexed @(gy + half) * worldTiles + (gx + half)@.
--   Beyond-glacier tiles get 'minBound'.  Result feeds the global
--   priority flood in 'identifyWorldLakes' / 'identifyWorldRivers';
--   nothing else needs it.
stitchWorldTerrain
    ∷ Int                  -- ^ worldSize
    → BorderedTerrainCache
    → VU.Vector Int
stitchWorldTerrain worldSize cache =
    let worldTiles = worldSize * chunkSize
        halfWorld  = worldTiles `div` 2
    in VU.create $ do
        v ← VUM.replicate (worldTiles * worldTiles) minBound
        forM_ (HM.toList cache) $ \(coord@(ChunkCoord cx cy), (bElev, _)) → do
            let baseGX   = cx * chunkSize
                baseGY   = cy * chunkSize
                interior = borderedToInterior worldSize coord bElev
            forM_ [0 .. chunkSize * chunkSize - 1] $ \i → do
                let lx     = i `mod` chunkSize
                    ly     = i `div` chunkSize
                    gxOff  = baseGX + lx + halfWorld
                    gyOff  = baseGY + ly + halfWorld
                    wIdx   = gyOff * worldTiles + gxOff
                when (gxOff ≥ 0 ∧ gxOff < worldTiles
                    ∧ gyOff ≥ 0 ∧ gyOff < worldTiles) $
                    VUM.write v wIdx (interior VU.! i)
        pure v

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

-- | Compact river events so each active river contributes exactly one
--   `HydroEvent (RiverFeature _)`, carrying its final persistent state,
--   landed in the most-recent Age period. All other periods have river
--   HydroEvents stripped. See buildTimeline for the motivation.
--
--   Each segment's `rsStartElev`/`rsEndElev` is re-sampled from the
--   timeline at end-of-Eon. The original values came from the river
--   trace's elev grid at BIRTH age and never updated. Erosion in later
--   ages typically lowered terrain along and around the river path —
--   if we don't re-sample, the carve target `cf = rsEndElev - rsDepth`
--   stays at the stale (high) value while neighbor terrain has dropped,
--   so the river ends up perched on whatever survived erosion, with
--   visible water cliffs against the lower adjacent valleys.
--
--   We sample via `applyTimelineFastFrom` on a stripped-rivers view —
--   pre-river terrain at the endpoint, so the carve doesn't try to
--   target itself. Sampled values may produce non-descending segments
--   (sub-grid noise, locally rising terrain); `fixupSegmentContinuity`
--   re-threads start = prev.end and clamps `rsEndElev ≤ rsStartElev`.
--
--   Input list is newest-first (matches `tbsPeriods` convention).
compactRiverEvents
    ∷ Word64
    → [TectonicPlate]
    → MaterialRegistry
    → Int
    → [PersistentFeature]
    → [GeoPeriod]
    → [GeoPeriod]
compactRiverEvents seed plates registry ws features finalPeriods =
    let -- All periods with river HydroEvents stripped — the view we
        -- sample against. Used only for sampling; the result we emit
        -- below also starts from this list (then injects fresh re-carves
        -- into the most-recent Age period).
        strippedAll = map stripRivers finalPeriods

        sampleOne gx gy =
            let (gx', gy') = wrapGlobalU ws gx gy
                base = elevationAtGlobal seed plates ws gx' gy'
                (e, _) = applyTimelineFastFrom seed plates ws gx' gy'
                                               registry strippedAll V.empty base
            in e

        -- "Drain perched rivers": sample not just the endpoint but a
        -- small star pattern around it (center + 4 cardinal offsets at
        -- `drainRadius`), and take the MIN. Without this, segments
        -- whose endpoints happen to land on a plateau next to a lower
        -- valley keep the plateau elev as refElev — the carve target
        -- stays above the valley floor and the river ends up perched
        -- on the plateau, rendering as a tall water cliff against the
        -- valley below (seed 666 had 18-tile water cliffs from this).
        -- Sampling the local minimum lets the carve reach down into
        -- the valley so the water surface sits at the valley floor.
        -- Sampling more than ~5 tiles out picks up unrelated valleys
        -- in steep terrain (seed 666 type cases), which makes things
        -- worse not better; ≤ 3 is too local to escape narrow plateaus.
        -- Tuned visually + on the cliff fuzz.
        drainRadius ∷ Int
        drainRadius = 5

        sampleAt (GeoCoord gx gy) =
            let c = sampleOne  gx               gy
                n = sampleOne  gx              (gy - drainRadius)
                s = sampleOne  gx              (gy + drainRadius)
                e = sampleOne (gx + drainRadius) gy
                w = sampleOne (gx - drainRadius) gy
            in minimum [c, n, s, e, w]

        resampleSeg seg =
            seg { rsStartElev = sampleAt (rsStart seg)
                , rsEndElev   = sampleAt (rsEnd seg)
                }

        resampleRiver pf =
            let river = getRiverParamsFromPf pf
                resampled = V.map resampleSeg (rpSegments river)
                threaded = fixupSegmentContinuity resampled
            in river { rpSegments = threaded }

        -- Map pfId → resampled RiverParams, so tributary re-alignment
        -- below can look up the parent's *resampled* segments (we need
        -- the post-sample elevations to compute the parent's
        -- channel-floor at the junction).
        resampledById = HM.fromList
            [ (pfId pf, resampleRiver pf)
            | pf ← features
            , isActiveRiver (pfFeature pf)
            , pfActivity pf ≡ FActive
            ]

        -- Tributary re-alignment.
        --
        -- After re-sampling, a tributary's last segment ends at the
        -- sampled natural elev at its mouth coord (= junction with
        -- parent). The parent's segment, also re-sampled, has the SAME
        -- elev at that coord. But channel-floors are
        -- `lerpedElev − rsDepth`, and trib/parent have different
        -- depths, so trib's cf and parent's cf at the junction differ
        -- by `mainDepth − tribDepth` — a 4–7 tile water cliff at the
        -- confluence in the worst cases. Force trib's last segment to
        -- end at `parentCf + tribDepth` so the cf's match (clamped to
        -- ≤ rsStartElev of the last seg so we never produce uphill).
        --
        -- For a branched trib, junction is at parent's rsStart (alongT
        -- = 0). For a merged trib, junction is somewhere along the
        -- parent's segment; the alongT projection handles both cases.
        alignTribToParent tribRp parentRp =
            let tribSegs = rpSegments tribRp
                lastIdx = V.length tribSegs - 1
            in if V.null tribSegs ∨ V.null (rpSegments parentRp)
               then tribRp
               else
                 let lastSeg = tribSegs V.! lastIdx
                     junctionCoord = rsEnd lastSeg
                     parentSeg = closestParentSeg
                                     (rpSegments parentRp) junctionCoord
                     parentLerpElev = lerpSegElev parentSeg junctionCoord
                     parentCf = parentLerpElev - rsDepth parentSeg
                     desiredEnd = parentCf + rsDepth lastSeg
                     clamped = min desiredEnd (rsStartElev lastSeg)
                     newLast = lastSeg { rsEndElev = clamped }
                 in tribRp { rpSegments = tribSegs V.// [(lastIdx, newLast)] }

        finalRiverParams pf =
            let resampled = HM.lookupDefault (getRiverParamsFromPf pf)
                                             (pfId pf) resampledById
            in case pfParentId pf of
                Just parentId →
                    case HM.lookup parentId resampledById of
                        Just parentRp → alignTribToParent resampled parentRp
                        Nothing       → resampled  -- parent gone (drained, etc)
                Nothing → resampled

        freshRiverEvts =
            [ HydroEvent (RiverFeature (finalRiverParams pf))
            | pf ← features
            , isActiveRiver (pfFeature pf)
            , pfActivity pf ≡ FActive
            ]

        -- Inject the re-carves into the most-recent Age period of the
        -- already-stripped list. `injectAt` walks until the first Age
        -- and appends `freshRiverEvts` there; later periods are
        -- unchanged.
        injectAt [] = []
        injectAt (p:ps)
            | gpScale p ≡ Age =
                let evts' = gpEvents p ⧺ freshRiverEvts
                in rebuild p evts' : ps
            | otherwise = p : injectAt ps

    in injectAt strippedAll
  where
    notStaleRiver (HydroEvent (RiverFeature _)) = False
    notStaleRiver _                             = True

    rebuild p evts = mkGeoPeriod ws (gpName p) (gpScale p) (gpDuration p)
                                 (gpDate p) evts
                                 (gpErosion p) (gpRegionalErosion p)

    -- Fast path: if a period has no river HydroEvents to strip, return
    -- the original — skips bbox re-tagging in `mkGeoPeriod`.
    stripRivers p =
        let cleaned = filter notStaleRiver (gpEvents p)
        in if length cleaned ≡ length (gpEvents p)
           then p
           else rebuild p cleaned

    -- Pick the parent segment whose midpoint is closest to the
    -- junction coord — best heuristic for finding which segment the
    -- tributary actually joins.
    closestParentSeg parentSegs (GeoCoord jx jy) =
        let dist seg =
                let GeoCoord sx sy = rsStart seg
                    GeoCoord ex ey = rsEnd seg
                    mx = (sx + ex) `div` 2
                    my = (sy + ey) `div` 2
                in abs (mx - jx) + abs (my - jy)
            bestIdx = V.minIndexBy
                (\a b → compare (dist a) (dist b)) parentSegs
        in parentSegs V.! bestIdx

    -- Lerp the parent segment's reference elev at the projected
    -- position of `target`. Project `target − start` onto `end − start`,
    -- clamp t ∈ [0, 1], lerp between rsStartElev/rsEndElev. Ignores
    -- u-axis wrap — junctions are local so wrap effects are negligible.
    lerpSegElev seg (GeoCoord tx ty) =
        let GeoCoord sx sy = rsStart seg
            GeoCoord ex ey = rsEnd seg
            dx = fromIntegral (ex - sx) ∷ Float
            dy = fromIntegral (ey - sy) ∷ Float
            denom = dx * dx + dy * dy
        in if denom < 1e-6
           then rsStartElev seg
           else
             let px = fromIntegral (tx - sx) ∷ Float
                 py = fromIntegral (ty - sy) ∷ Float
                 t = max 0 (min 1 ((px * dx + py * dy) / denom))
                 startE = fromIntegral (rsStartElev seg) ∷ Float
                 endE   = fromIntegral (rsEndElev seg) ∷ Float
             in floor (startE + t * (endE - startE))

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
