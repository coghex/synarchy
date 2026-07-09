{-# LANGUAGE Strict, UnicodeSyntax #-}

-- | Global river identification, run once at world init alongside
--   'World.Fluid.Lake.Identify.identifyWorldLakes'.
--
--   Pipeline (v1):
--
--     1. Per-tile @(precip − evap)@ from the climate field. Hot, arid
--        tiles contribute negative units; wet temperate tiles positive.
--     2. D4 steepest-descent direction per tile. Spillway tiles
--        exclude their source lake's neighbors so the lake outflow
--        doesn't feed back into the lake.
--     3. Per-lake spillway identification: lowest non-in-lake terrain
--        tile cardinally adjacent to any in-lake tile. Sub-sea-level
--        spillways are dropped — the lake drains directly to ocean
--        with no river formed.
--     4. Descending-z bucket sort of all land tiles. Walk in that
--        order accumulating flow: each tile's local @(precip − evap)@
--        plus its upstream contributions, clamped non-negative,
--        propagates to its D4 downhill neighbor.  Lake tiles route
--        their accumulated flow into a per-lake accumulator instead
--        of D4 — the deepest tiles in a basin would otherwise be
--        sinks that swallowed the flow.
--     5. Lake outflow injection: spillways in descending-z order get
--        their lake's accumulated flow added, then propagate down the
--        D4 chain. Walking spillways in descending order lets cascade
--        chains (lake1 → spillway1 → lake2 → spillway2) work in a
--        single pass.
--     6. Threshold: a tile is a river-candidate iff its accumulated
--        flow exceeds 'riverThreshold'.  Below the threshold a small
--        precipitation source dries up before becoming a river — big
--        rivers survive arid stretches because they carry enough
--        flow that local evap can't drop them below.
--     7. Trace: walk D4-connected river tiles into Rivers (one River
--        per connected component). Per-tile surface z starts at the
--        tile's terrain and is clamped so no downstream step exceeds
--        'waterfallQuantum'; where terrain drops faster (cliffs) the
--        carve pass deepens the channel into a stepped gorge instead
--        of rendering a vertical water wall. Surface z is
--        monotonically non-increasing downstream.
--     8. Per-chunk bitmasks + per-tile surface z, indexed by
--        chunk coord for chunk-gen lookup.
--
--   Steps 1–5 (climate → flow field) live in
--   'World.Fluid.River.Identify.Flow'; step 7's component tracing
--   (chain extension, width expansion, component labeling/culling,
--   per-river bookkeeping) lives in
--   'World.Fluid.River.Identify.Components'; its coastal-breakthrough
--   sub-pass lives in 'World.Fluid.River.Identify.Breakthrough'; the
--   channel bed-depth model lives in
--   'World.Fluid.River.Identify.BedDepth'; and step 8's per-chunk
--   bucketing lives in 'World.Fluid.River.Identify.ChunkIndex'. This
--   module holds the entry point and the 'traceRivers' orchestrator
--   that wires them together.
module World.Fluid.River.Identify
    ( identifyWorldRivers
    , riverThreshold
      -- * Exported for tests (Test.Headless.WorldGen.WrapSeam)
    , labelRiverComponents
      -- * Exported for tests (Test.Headless.WorldGen.BedDepth)
    , computeBedDepth
    , depthFromRadius
    , maxBedDepth
    ) where

import UPrelude
import qualified Data.Vector.Unboxed as VU
import World.Chunk.Types (chunkSize)
import World.Constants (seaLevel)
import World.Fluid.Lake.Identify (computeWorldEdgeOcean)
import World.Fluid.Lake.Types (WorldLakes)
import World.Fluid.River.Identify.BedDepth
    (computeBedDepth, maxBedDepth, computeCarveDelta)
import World.Fluid.River.Identify.Breakthrough (addBreakthroughs)
import World.Fluid.River.Identify.ChunkIndex
    (buildCarveDeltaIndex, buildRiverChunkIndex)
import World.Fluid.River.Identify.Components
    ( extendRiverChains, clampCentreSurfaces, clampLateralSurfaces
    , expandWidth, depthFromRadius, cullByLength, labelRiverComponents
    , buildRivers )
import World.Fluid.River.Identify.Flow
    ( computePrecipUnits, computeEvapUnits, buildLakeIdAt
    , computeSpillways, buildIsSpillwayOf, computeDescentDirs
    , bucketSortAscending, computeFlowAccumulation )
import World.Fluid.River.Types (WorldRivers(..), emptyWorldRivers)
import World.Weather.Types (ClimateState)

-- | Minimum accumulated flow for a tile to count as a river. Below
--   threshold and the tile renders dry. v1 keeps this fairly low so
--   chains extend a meaningful distance upstream — the visual size
--   filter is the per-component length cap in
--   'World.Fluid.River.Identify.Components.targetRiverCount' below,
--   not this per-tile cutoff.
riverThreshold ∷ Int
riverThreshold = 100

-- NOTE: the maximum water-surface drop between adjacent river tiles
-- (the "waterfall quantum") is no longer a module constant — it is a
-- worldgen tunable threaded in from 'WorldGenConfig'
-- (@wgcWaterfallQuantum@, exposed in the create-world advanced tab) via
-- 'identifyWorldRivers'. Reaches with slope ≤ the quantum follow terrain
-- exactly; steeper drops (cliffs) are absorbed by carving a stepped
-- gorge so water never renders as one tall vertical sheet. Default 12.

-- * Entry point

-- | Run the full river identification pipeline.
identifyWorldRivers
    ∷ Word64               -- ^ world seed (pool/riffle bed noise, #223)
    → Int                  -- ^ worldSize (chunks per side)
    → WorldLakes
    → VU.Vector Int        -- ^ world terrain (worldTiles²)
    → ClimateState
    → Int                  -- ^ waterfall quantum (max surface drop / step)
    → (Int → Int → Float)  -- ^ rift-intensity field (#223)
    → WorldRivers
identifyWorldRivers seed worldSize lakes terrain climate waterfallQuantum0
                    riftAt =
    let -- Guard: a non-positive quantum would flatten / break the
        -- stepped-gorge clamp, so floor it at 1 regardless of config source.
        waterfallQuantum = max 1 waterfallQuantum0
        worldTiles = worldSize * chunkSize
        nTiles    = worldTiles * worldTiles

        -- Per-tile climate units.
        precipUnits = computePrecipUnits worldSize climate terrain
        evapUnits   = computeEvapUnits   worldSize climate terrain

        -- Reverse-index lakes to a tile → lakeId map (-1 if not in any).
        lakeIdAt = buildLakeIdAt worldSize lakes

        -- Per-lake spillway tile (-1 if none usable).
        spillwayOf = computeSpillways worldSize lakes terrain lakeIdAt

        -- Inverse: per-tile → lakeId of which it's the spillway (-1 if none).
        isSpillwayOf = buildIsSpillwayOf nTiles spillwayOf

        -- D4 steepest descent — spillways exclude their source lake.
        dir = computeDescentDirs worldTiles terrain lakeIdAt isSpillwayOf

        -- Sorted tile indices (ascending z, skipping minBound).
        ascOrder = bucketSortAscending terrain

        -- The flow walk.
        (flow, _lakeFlow) = computeFlowAccumulation
            worldTiles terrain lakeIdAt dir spillwayOf
            precipUnits evapUnits ascOrder

    in traceRivers seed worldSize terrain lakeIdAt isSpillwayOf spillwayOf
                   dir flow ascOrder waterfallQuantum riftAt

-- * River trace

-- | From per-tile flow, find river tiles (flow ≥ threshold), group
--   them into connected components, compute quantised surface z, and
--   build per-chunk bitmasks.
traceRivers
    ∷ Word64               -- ^ world seed (pool/riffle bed noise)
    → Int                  -- ^ worldSize
    → VU.Vector Int        -- ^ terrain
    → VU.Vector Int        -- ^ lakeIdAt
    → VU.Vector Int        -- ^ isSpillwayOf
    → VU.Vector Int        -- ^ spillwayOf
    → VU.Vector Word8      -- ^ dir
    → VU.Vector Int        -- ^ flow
    → VU.Vector Int        -- ^ ascending z order
    → Int                  -- ^ waterfall quantum (max surface drop / step)
    → (Int → Int → Float)  -- ^ rift-intensity field
    → WorldRivers
traceRivers seed worldSize terrain lakeIdAt isSpillwayOf spillwayOf dir flow
            ascOrder waterfallQuantum riftAt =
    let worldTiles = worldSize * chunkSize
        nTiles     = worldTiles * worldTiles
        half       = worldTiles `div` 2

        -- Step 1a: primary river tiles (flow crosses the per-tile threshold).
        isPrimary = VU.generate nTiles $ \i →
            let t   = terrain VU.! i
                lk  = lakeIdAt VU.! i
                fl  = flow VU.! i
            in t ≠ minBound ∧ lk < 0 ∧ t > seaLevel ∧ fl ≥ riverThreshold

        -- Step 1b: extend each chain downstream via D4 until it reaches
        -- a terminus (ocean / lake / sink). Tiles whose own flow has been
        -- pushed below threshold by arid-stretch evap still count as
        -- river — at minimum width — so the chain doesn't fragment.
        isRiverCentre = extendRiverChains worldTiles terrain lakeIdAt
                                          dir isPrimary ascOrder

        -- Step 1c-pre: waterfall clamp. Bound each downstream surface
        -- step at 'waterfallQuantum' BEFORE widening so wings inherit
        -- the clamped plane. Fixes the adjacent water-wall artifact
        -- (e.g. 79z vertical sheets at coastal scarps).
        centreSurf = clampCentreSurfaces worldTiles terrain dir
                                         isRiverCentre ascOrder waterfallQuantum

        -- Step 1c: variable width. Each centre tile widens perpendicular
        -- to its D4 dir up to its flow-derived width radius. Returns the
        -- expanded river-tile mask, per-tile width radius, and per-tile
        -- water surface z (centre tiles use the waterfall-clamped
        -- surface from 'clampCentreSurfaces'; widened tiles inherit the
        -- lowest centre's surface so the water plane stays flat across
        -- the river's cross-section).
        (isRiverTile, widthRadius, surfZ, perpDist) =
            expandWidth worldTiles terrain dir flow isRiverCentre centreSurf

        -- Step 2: label connected components — plain 4-adjacency over
        -- the widened river mask (wings included), so width wings stay
        -- in their river's component through the length cull below.
        (rawCompId, rawNComps) = labelRiverComponents
                                    worldTiles isRiverTile

        -- Step 2.5: cap the river count. Rank components by tile count
        -- (length) and keep only the top N (worldSize-scaled). Length
        -- rather than peak flow is the right signal here — peak flow
        -- picks single-tile mouth tiles, length picks visible chains.
        (isRiverTileF, compIdF, nComps) =
            cullByLength worldSize rawCompId rawNComps isRiverTile

        -- Step 3: coastal breakthrough. Stranded inland mouths within
        -- breakthrough range of an ocean tile get a Dijkstra-found
        -- path carved through the blocking terrain.  Path tiles are
        -- added to the bitmask with width 0; surface descends to
        -- @seaLevel + 1@ at the path's end.
        worldOcean = computeWorldEdgeOcean terrain worldTiles
        (isRiverTileB, compIdB, widthRadiusB, surfZB, perpDistB) =
            addBreakthroughs worldTiles isRiverTileF compIdF dir terrain
                             worldOcean widthRadius surfZ perpDist

        -- Step 3.5: lateral waterfall clamp. The chain clamp (1c-pre)
        -- bounds steps along flow edges only; meander necks and
        -- parallel reaches can still put two non-flow-linked river
        -- tiles side by side with a bigger gap. Relax to the maximal
        -- surface field with |Δ| ≤ 'waterfallQuantum' across EVERY
        -- adjacent river-tile pair (wings + breakthrough paths
        -- included). The carve pass below absorbs any lowering.
        surfZL = clampLateralSurfaces worldTiles isRiverTileB surfZB waterfallQuantum

        -- Step 4: build per-component bookkeeping (bbox, sources,
        -- sinks, peak flow). Uses post-breakthrough data so bboxes
        -- include the carved canyons.
        rivers = buildRivers worldSize terrain lakeIdAt isSpillwayOf
                             spillwayOf dir flow isRiverTileB compIdB nComps

        -- Step 5: per-chunk bitmasks + surface z + width slices.
        byChunk = buildRiverChunkIndex worldSize half isRiverTileB compIdB
                                       surfZL widthRadiusB

        -- Step 5.5 (#223): per-tile bed depth. Ordinary reaches keep
        -- the flat 'depthFromRadius' fit; confined (canyon-walled)
        -- and tectonically-rifted reaches deepen, with a thalweg
        -- cross-section and pool/riffle variation along the channel.
        bedDepth = computeBedDepth seed worldSize terrain isRiverTileB
                                   widthRadiusB perpDistB surfZL riftAt

        -- Step 6: per-tile carve delta. Includes breakthrough path
        -- tiles, which can have large delta where they cut through
        -- coastal mountains.
        carveDelta = computeCarveDelta worldTiles terrain
                                       isRiverTileB bedDepth surfZL
        carveByChunk = buildCarveDeltaIndex worldSize half isRiverTileB
                                            carveDelta

    in if nComps ≡ 0
       then emptyWorldRivers
       else WorldRivers
            { wrRivers     = rivers
            , wrByChunk    = byChunk
            , wrCarveDelta = carveByChunk
            }
