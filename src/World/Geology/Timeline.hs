{-# OPTIONS_GHC -fprof-auto #-}
{-# LANGUAGE Strict, UnicodeSyntax #-}
module World.Geology.Timeline
    ( buildTimeline
    ) where
import UPrelude
import qualified Data.Vector as V
import qualified Data.HashMap.Strict as HM
import qualified Data.HashSet as HS
import World.Types
import World.Plate (generatePlates, riftFieldMemo)
import World.Material (MaterialRegistry)
import World.Fluid.IceLevel (computeIceLevelGrid)
import World.Fluid.Lake.Graben (grabenCarveIndex)
import World.Fluid.Lake.Identify (identifyWorldLakes, computeRenderedOcean)
import World.Fluid.Seabed (identifySeabed)
import World.Fluid.Seabed.Types (emptySeabedTable)
import World.Fluid.OceanMask (buildWorldOceanMask, emptyWorldOceanMask)
import World.Geology.Ore (buildOreDepositTable)
import World.Geology.Ore.Types (OreLevers, emptyWorldOreDeposits)
import World.Magma.Init (buildVolcanoCtx)
import World.Magma.Pool (identifyLavaPools)
import World.Fluid.Lake.Types (emptyWorldLakes, WorldLakes(..))
import World.Fluid.River.Identify (identifyWorldRivers)
import World.Fluid.River.Types (emptyWorldRivers)
import World.Fluid.Ocean (computeOceanMap)
import World.Generate.InitTerrain (BorderedTerrainCache)
import World.Geology.Coastal (identifyCoastalErosion)
import World.Geology.Coastal.Types (emptyCoastalTable)
import World.Generate.Timeline (applyTimelineFast)
import World.Hydrology.Simulation (buildInitialElevGrid)
import World.Geology.Timeline.Helpers (updateLakeSpillways)
import World.Geology.Timeline.Loop (buildEonLoop)
import World.Geology.Timeline.Stitch
    ( buildTimelineStageCache, finishBorderedCache
    , stitchWorldGrids, stitchWorldTerrain )
import World.Geology.Timeline.Compact (compactRiverEvents)
import World.Weather.Types (ClimateState, initClimateState)

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
        -- clamp carve deltas — seabed covers every clamped-lake tile
        -- and carves it to the ramp instead (which is still ≤
        -- seaLevel−1, so the surface fills). Keeps the two mechanisms
        -- from double-carving. In their place ride the graben deltas
        -- (#223): rift-lake bed carves for INLAND lakes only
        -- (lkSurface > seaLevel), which the seabed pass never touches.
        finalLakes = finalLakes0
            { wlCarveDelta = grabenCarveIndex seed worldSize worldTerrain
                                              riftAt finalLakes0 }

        -- Inland rift-intensity field (#223): shared by the river
        -- bed-depth model and the graben lake carve so both agree on
        -- where the world is rifting. Memoized once — cell samples
        -- are reused across every per-tile query below.
        riftAt = riftFieldMemo seed worldSize plates

        finalRivers = identifyWorldRivers seed worldSize finalLakes
                                          worldTerrain
                                          (tbsClimateState s2)
                                          waterfallQuantum
                                          riftAt

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
