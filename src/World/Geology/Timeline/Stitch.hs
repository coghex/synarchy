{-# OPTIONS_GHC -fprof-auto #-}
{-# LANGUAGE Strict, UnicodeSyntax #-}
-- | Per-chunk bordered-terrain cache build + stitching into
--   world-resolution grids. Runs the plate-base → applyTimelineChunk →
--   applyCoastalErosion → despike pipeline per chunk (stage 1,
--   parallelized), then stitches the results into the world-resolution
--   elevation/material grids 'World.Geology.Timeline.buildTimeline'
--   needs for its global coastal + lake/river passes.
module World.Geology.Timeline.Stitch
    ( buildTimelineStageCache
    , finishBorderedCache
    , stitchWorldGrids
    , stitchWorldTerrain
    ) where
import UPrelude
import qualified Data.Vector.Unboxed as VU
import qualified Data.Vector.Unboxed.Mutable as VUM
import qualified Data.HashMap.Strict as HM
import Control.Parallel.Strategies (parListChunk, rdeepseq, using)
import World.Types
import World.Material (MaterialRegistry, MaterialId, matGlacier)
import World.Generate.Constants (chunkBorder)
import World.Generate.InitTerrain
    (BorderedTerrainCache, borderedToInterior
    , computeChunkTimelinePipeline, finishBorderedPipeline
    , buildPlateBaseCache)
import World.Geology.Coastal.Types (CoastalTable)

-- | Build the per-chunk bordered-pipeline cache. Runs the full
--   plate-base → applyTimelineChunk → applyCoastalErosion → despike
--   pipeline once per chunk and stashes the bordered (elev, mat)
--   result in a HashMap indexed by chunk coord.
--
--   Per-chunk work is parallelized via @parListChunk 64 rdeepseq@
--   (forcing each chunk's vectors to NF inside its spark so they
--   land already-evaluated when the consumer reads them).
--
--   #500: the plate-base ('elevationAtGlobal') half of each chunk's
--   work is shared via 'buildPlateBaseCache', computed once up front
--   (its own parallel pass) rather than recomputed independently by
--   every chunk whose 14-tile border overlaps it.
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
        baseCache = buildPlateBaseCache seed plates worldSize
        runOne coord =
            ( coord
            , computeChunkTimelinePipeline seed plates worldSize
                                            registry timeline
                                            (Just baseCache) coord
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
