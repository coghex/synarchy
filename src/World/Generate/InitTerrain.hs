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
    , PlateBaseCache
    , buildPlateBaseCache
    ) where

import UPrelude
import Control.Monad.ST (runST)
import Control.Parallel.Strategies (parListChunk, rdeepseq, using)
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
    (TectonicPlate, elevationAtGlobal, isBeyondGlacier, wrapGlobalU
    , worldWidthTiles)
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

-- | Plate-base (elevation, material) for every WRAPPED global
--   coordinate ('wrapGlobalU' output) a chunk border can query, held
--   as a flat square indexed @(gx+halfW)*dim+(gy+halfW)@ (@dim = 2 *
--   halfW + 1@, @halfW@ the stored 'Int'). #500: every chunk's 44×44
--   bordered window ('computeChunkTimelinePipeline' below)
--   independently recomputes 'elevationAtGlobal' for its border tiles,
--   and adjacent chunks' 14-tile borders overlap heavily — each
--   physical tile lands in ~3×3 neighbouring windows. Building this
--   once and sharing it across 'buildTimelineStageCache' cuts that
--   redundancy to zero without changing any output (same deterministic
--   function, same wrapped-coordinate arguments, just memoized).
--
--   A flat array, not a 'HM.HashMap': a first cut keyed on a
--   @HashMap (Int,Int)@ turned out to cost more in hashing/boxing over
--   ~30M border-tile lookups than it saved by deduplicating
--   'elevationAtGlobal' calls. O(1) array indexing has neither cost.
type PlateBaseCache = (Int, VU.Vector (Int, MaterialId))

-- | Build the plate-base cache by direct index computation rather
--   than by replaying chunk-window loops: 'wrapGlobalU' always
--   normalizes u (= gx − gy) into @[-halfW, halfW)@, and v (= gx + gy)
--   is invariant under wrapping and excluded beyond the glacier
--   (@isBeyondGlacier@ is a strict @>@, so @[-halfW, halfW]@ inclusive
--   is kept) — so any (gx, gy) a chunk border can query, once wrapped,
--   falls inside this @dim × dim@ square with both checks passing.
--   Cells outside them (roughly half the square — u can range twice as
--   wide as its canonical window once gx and gy vary independently)
--   are unreachable by any real query, so they're filled with a cheap
--   placeholder instead of paying for 'elevationAtGlobal'.
buildPlateBaseCache ∷ Word64 → [TectonicPlate] → Int → PlateBaseCache
buildPlateBaseCache seed plates worldSize =
    let w     = worldWidthTiles worldSize
        halfW = w `div` 2
        dim   = 2 * halfW + 1
        computeCell gx gy =
            let u = gx - gy
            in if u < negate halfW ∨ u ≥ halfW
                  ∨ isBeyondGlacier worldSize gx gy
               then (0, matGlacier)
               else elevationAtGlobal seed plates worldSize gx gy
        rows = [ VU.generate dim (\j → computeCell (i - halfW) (j - halfW))
               | i ← [0 .. dim - 1]
               ]
                   `using` parListChunk 8 rdeepseq
    in (halfW, VU.concat rows)

-- | Look up a wrapped global coordinate in a 'PlateBaseCache'.
lookupPlateBase ∷ PlateBaseCache → Int → Int → (Int, MaterialId)
lookupPlateBase (halfW, vec) gx gy =
    let dim = 2 * halfW + 1
    in vec VU.! ((gx + halfW) * dim + (gy + halfW))

-- | Stage 1: plate-base → 'applyTimelineChunk' for one chunk's
--   bordered region. Per-tile output is window-position-independent
--   (verified by the BorderProbe), so 'buildTimeline' can stitch
--   these interiors into the unambiguous global pre-coastal terrain
--   that 'identifyCoastalErosion' runs against.
--
--   @mBaseCache@, when supplied, is consulted instead of calling
--   'elevationAtGlobal' directly — see 'PlateBaseCache'. One-shot
--   callers with no shared cache (e.g. 'computeChunkBorderedPipeline')
--   pass 'Nothing' and get the original per-call behaviour.
computeChunkTimelinePipeline
    ∷ Word64
    → [TectonicPlate]
    → Int                  -- ^ worldSize
    → MaterialRegistry
    → GeoTimeline
    → Maybe PlateBaseCache
    → ChunkCoord
    → (VU.Vector Int, VU.Vector MaterialId)
computeChunkTimelinePipeline seed plates worldSize registry timeline
                             mBaseCache coord =
    let wsc        = computeWorldScale worldSize
        borderSize = chunkSize + 2 * chunkBorder
        borderArea = borderSize * borderSize
        fromIndex idx =
            let (by, bx) = idx `divMod` borderSize
            in (bx - chunkBorder, by - chunkBorder)

        lookupBase gx' gy' = case mBaseCache of
            Just cache → lookupPlateBase cache gx' gy'
            Nothing    → elevationAtGlobal seed plates worldSize gx' gy'

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
                        let (elev, mat) = lookupBase gx' gy'
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
                                      timeline Nothing coord)

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
