{-# LANGUAGE Strict #-}
-- | Build the zoom cache entries AND per-chunk pixel data in one pass,
--   split out of "World.ZoomMap.Cache" (issue #573).
--
--   Since #2298 the per-chunk work itself lives in
--   "World.ZoomMap.Cache.ChunkPass"; what remains here is the
--   WHOLE-WORLD driver: which chunks exist, in what order they are
--   emitted, and the halo every chunk's second pass reads. The map
--   pyramid runs the same two passes over a page footprint instead, so
--   there is one generator rather than two that have to agree.
module World.ZoomMap.Cache.BuildPixels
    ( buildZoomCacheWithPixels
    ) where

import UPrelude
import Control.Parallel.Strategies (parListChunk, using, rdeepseq)
import qualified Data.ByteString as BS
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Data.Vector as V
import World.Chunk.Types (ChunkCoord(..))
import World.Generate.InitTerrain (BorderedTerrainCache)
import World.Generate.Types (WorldGenParams(..))
import World.Material (MaterialRegistry)
import World.ZoomMap.Cache.ChunkPass
    (ZoomChunkPass(..), zoomChunkPass, zoomChunkPixels)
import World.ZoomMap.ColorPalette (ZoomColorPalette)
import World.ZoomMap.Types (ZoomChunkEntry)

-- * Build Zoom Cache + Per-Chunk Pixel Data

-- | Build the zoom cache and per-chunk pixel data in one pass.
--   For each chunk, computes material and vegetation at every
--   tile position (16×16) and generates RGBA pixel data using
--   the color palette.
--
--   The 'Maybe BorderedTerrainCache' is the optional init-time
--   per-chunk pipeline cache. When present (only on fresh world
--   init, where 'buildTimeline' produced it alongside the timeline),
--   'generateZoomTerrain' reads the pre-computed bordered terrain
--   instead of re-running 'applyTimelineChunk' /
--   'applyCoastalErosion' / 'removeElevationSpikes'. Loaded-save
--   paths pass 'Nothing' and pay the recompute cost.
buildZoomCacheWithPixels ∷ WorldGenParams → MaterialRegistry
                         → ZoomColorPalette
                         → Maybe BorderedTerrainCache
                         → (V.Vector ZoomChunkEntry, V.Vector BS.ByteString)
buildZoomCacheWithPixels params registry palette mBorderedCache =
    (V.fromList (map fst results), V.fromList (map snd results))
  where
    worldSize = wgpWorldSize params
    halfSize = worldSize `div` 2
    w = halfSize * 2

    allCoords = [ let wrappedU = ((u + halfSize) `mod` w + w) `mod` w - halfSize
                      ccx = (wrappedU + v) `div` 2
                      ccy = (v - wrappedU) `div` 2
                  in ChunkCoord ccx ccy
                | v ← [-halfSize .. halfSize - 1]
                , u ← [-halfSize .. halfSize - 1]
                , even (u + v)
                ]

    -- Deduplicate via Set.  Same order used for entries, pixels,
    -- and atlas packing — see comment in buildZoomCache.  'ChunkCoord'
    -- derives Ord field-wise, so this is the same ordering the
    -- @(ccx, ccy)@ tuple set produced before #2298.
    uniqueCoords = Set.toList $ Set.fromList allCoords

    chunkBatchSize = max 1 (length uniqueCoords `div` 128)

    -- Pass 1: terrain + fluid + ice for all chunks (parallelized).
    pass1Results = map (zoomChunkPass params registry mBorderedCache)
                       uniqueCoords
                     `using` parListChunk chunkBatchSize rdeepseq

    -- The halo pass 2 reads: chunk coord → pass-1 fluid map. A chunk
    -- outside the world is absent, which is what makes the latitude
    -- edge answer dry.
    fluidLookup = Map.fromList
        (zip uniqueCoords (map zcpRawFluid pass1Results))
    haloFluid coord = Map.lookup coord fluidLookup

    -- Pass 2: extend ocean at chunk boundaries using neighbor fluid
    -- data, then render.
    results =
        [ (zcpEntry pass, zoomChunkPixels palette worldSize haloFluid coord pass)
        | (coord, pass) ← zip uniqueCoords pass1Results ]
