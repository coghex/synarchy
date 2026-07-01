{-# LANGUAGE Strict, UnicodeSyntax #-}

-- | World-init terrain compute, shared between 'World.Geology.Timeline'
--   (which uses it to build the world-resolution terrain that the
--   global priority flood runs against) and any future caller that
--   wants chunk-pipeline-accurate terrain without paying for all the
--   other 'generateChunk' work (materials, strata, fluid, etc.).
--
--   Lives in its own module to dodge the import cycle:
--   'World.Geology.Timeline' needs this function, but
--   'World.Generate.Chunk' transitively depends on 'World.Geology', so
--   keeping the function here lets both 'Timeline' and 'Chunk' import
--   it without going through each other.
--
--   Since save v25 the pipeline is two-stage: the TIMELINE stage
--   (plate-base → 'applyTimelineChunk') is window-position-independent
--   per tile, so 'buildTimeline' stitches one global pre-coastal grid
--   from it, runs 'identifyCoastalErosion' ONCE on that grid, and the
--   FINISH stage just applies the resulting table + despike.  The old
--   per-window 'applyCoastalErosion' computed different coastlines in
--   adjacent windows (information horizon ≈ 36 tiles vs a 14-tile
--   shared border), manufacturing seam cliffs.
module World.Generate.InitTerrain
    ( computeChunkTimelinePipeline
    , finishBorderedPipeline
    , computeChunkBorderedPipeline
    , BorderedTerrainCache
    , borderedToInterior
    ) where

import UPrelude
import Control.Monad.ST (runST)
import qualified Data.HashMap.Strict as HM
import qualified Data.Vector.Unboxed as VU
import qualified Data.Vector.Unboxed.Mutable as VUM
import World.Chunk.Types (ChunkCoord(..), chunkSize)
import World.Constants (seaLevel)
import World.Generate.Constants (chunkBorder)
import World.Generate.Coordinates (chunkToGlobal)
import World.Generate.Timeline (applyTimelineChunk, removeElevationSpikes)
import World.Geology.Coastal (applyCoastalTable)
import World.Geology.Coastal.Types (CoastalTable)
import World.Geology.Timeline.Types (GeoTimeline)
import World.Material (MaterialId, MaterialRegistry, matGlacier)
import World.Plate
    (TectonicPlate, elevationAtGlobal, isBeyondGlacier, wrapGlobalU)
import World.Scale (computeWorldScale)

-- | Per-chunk cached output of the init-time bordered pipeline
--   (plate-base → 'applyTimelineChunk' → coastal table →
--   'removeElevationSpikes'). Both 'buildWorldTerrain' and
--   'generateZoomTerrain' want this; computing it once and looking up
--   per chunk saves ~50% of the per-chunk pipeline work during init.
--
--   The pair is @(borderedElev, borderedMat)@, both length
--   @(chunkSize + 2 * chunkBorder)^2@.
type BorderedTerrainCache =
    HM.HashMap ChunkCoord (VU.Vector Int, VU.Vector MaterialId)

-- | Stage 1: plate-base → 'applyTimelineChunk' for one chunk's
--   bordered region. Per-tile output is window-position-independent
--   (verified by the BorderProbe), so 'buildTimeline' can stitch
--   these interiors into the unambiguous global pre-coastal terrain
--   that 'identifyCoastalErosion' runs against.
computeChunkTimelinePipeline
    ∷ Word64
    → [TectonicPlate]
    → Int                  -- ^ worldSize
    → MaterialRegistry
    → GeoTimeline
    → ChunkCoord
    → (VU.Vector Int, VU.Vector MaterialId)
computeChunkTimelinePipeline seed plates worldSize registry timeline coord =
    let wsc        = computeWorldScale worldSize
        borderSize = chunkSize + 2 * chunkBorder
        borderArea = borderSize * borderSize
        fromIndex idx =
            let (by, bx) = idx `divMod` borderSize
            in (bx - chunkBorder, by - chunkBorder)

        (baseElev, baseMat) = runST $ do
            elevM ← VUM.new borderArea
            matM  ← VUM.new borderArea
            forM_ [0 .. borderArea - 1] $ \idx → do
                let (lx, ly) = fromIndex idx
                    (gx, gy) = chunkToGlobal coord lx ly
                    (gx', gy') = wrapGlobalU worldSize gx gy
                if isBeyondGlacier worldSize gx' gy'
                    then do
                        VUM.write elevM idx (seaLevel + 100)
                        VUM.write matM  idx matGlacier
                    else do
                        let (elev, mat) =
                                elevationAtGlobal seed plates worldSize gx' gy'
                        VUM.write elevM idx elev
                        VUM.write matM  idx mat
            elevF ← VU.unsafeFreeze elevM
            matF  ← VU.unsafeFreeze matM
            pure (elevF, matF)

    in applyTimelineChunk timeline worldSize registry wsc coord
           (baseElev, baseMat)

-- | Stage 2: apply the global coastal table, then despike. Cheap
--   (table lookups) — the expensive smoothing/BFS work happened once
--   globally in 'identifyCoastalErosion'.
finishBorderedPipeline
    ∷ CoastalTable
    → ChunkCoord
    → (VU.Vector Int, VU.Vector MaterialId)
    → (VU.Vector Int, VU.Vector MaterialId)
finishBorderedPipeline coastal coord (timelineElev, timelineMat) =
    let borderSize = chunkSize + 2 * chunkBorder
        (postCoastElev, finalMat) =
            applyCoastalTable coastal coord (timelineElev, timelineMat)
        (finalElev, _) = removeElevationSpikes 12 4 borderSize
                                               (postCoastElev, finalMat)
    in (finalElev, finalMat)

-- | Both stages for a single chunk — for one-shot callers that don't
--   hold a timeline-stage cache. ('World.Generate.Chunk' inlines the
--   same sequence; keep them in sync.)
computeChunkBorderedPipeline
    ∷ Word64
    → [TectonicPlate]
    → Int                  -- ^ worldSize
    → MaterialRegistry
    → CoastalTable
    → GeoTimeline
    → ChunkCoord
    → (VU.Vector Int, VU.Vector MaterialId)
computeChunkBorderedPipeline seed plates worldSize registry coastal
                             timeline coord =
    finishBorderedPipeline coastal coord
        (computeChunkTimelinePipeline seed plates worldSize registry
                                      timeline coord)

-- | Slice a bordered elevation vector down to the @chunkSize^2@
--   interior used by 'buildWorldTerrain' for the global priority
--   flood. Returns @minBound@ for beyond-glacier tiles.
borderedToInterior
    ∷ Int                  -- ^ worldSize
    → ChunkCoord
    → VU.Vector Int        -- ^ bordered (chunkSize + 2*chunkBorder)^2
    → VU.Vector Int
borderedToInterior worldSize coord borderedElev =
    let borderSize = chunkSize + 2 * chunkBorder
        toIndex lx ly =
            let bx = lx + chunkBorder
                by = ly + chunkBorder
            in by * borderSize + bx
    in VU.generate (chunkSize * chunkSize) $ \i →
        let lx = i `mod` chunkSize
            ly = i `div` chunkSize
            (gx, gy) = chunkToGlobal coord lx ly
            (gx', gy') = wrapGlobalU worldSize gx gy
        in if isBeyondGlacier worldSize gx' gy'
           then minBound
           else borderedElev VU.! toIndex lx ly
