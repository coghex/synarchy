{-# LANGUAGE Strict, UnicodeSyntax #-}

-- | Global lake identification, run once at world init.
--
--   Pipeline:
--
--     1. Caller provides a world-resolution per-tile terrain grid
--        produced by the SAME pipeline chunk-gen runs (see
--        "World.Generate.InitTerrain") so basin bitmasks match the
--        rendered terrain — no fast-vs-chunk divergence.
--     2. Tile-resolution BFS from world-boundary ocean tiles ⇒
--        'worldOcean' mask: which tiles are world-edge-connected
--        open ocean. Drives both flood seeding and basin labelling.
--        Lives in 'World.Fluid.Lake.Identify.Ocean'.
--     3. Bucket-queue priority flood. Seeds: 'worldOcean' tiles +
--        tiles adjacent to beyond-glacier sentinels. Lives in
--        'World.Fluid.Lake.Identify.Flood'.
--     4. BFS-label connected basin components (basin tile =
--        @filled > terrain@, not 'worldOcean', not unreached).
--     5. Per-component aggregate: floor, surface (= spillway, no
--        cap), area, tile bbox. Steps 4/5 live in
--        'World.Fluid.Lake.Identify.Components'.
--     6. Drop components below 'minBasinTiles' (Phase 3 will swap
--        in climate-aware filtering).
--     7. Per-chunk bitmasks: for each chunk that any kept lake
--        touches, set bits for its in-lake tiles. Steps 6/7's
--        bitmask + carve-delta build lives in
--        'World.Fluid.Lake.Identify.ChunkIndex'.
--
--   Output is a 'WorldLakes' that the chunk-gen composer reads with
--   no priority flood, BFS, or bordered-region work at runtime. The
--   per-chunk 'smoothIslandColumns' pass in 'World.Generate.Chunk.Fluid'
--   handles 1–5 z column artifacts the global flood inherently can't
--   address (noise spikes that the timeline despike misses).
--
--   WRAP-SEAM CONVENTION: the flood, the ocean BFS, and component
--   labelling all treat the grid's x-edges as plain walls (no torus
--   wrap). This is sound because the square gx/gy grid's x-edge
--   columns sit at the diamond world's glacier corners — a torus
--   edge would pair tiles whose v differs by ~worldTiles, i.e. real
--   terrain with glacier/void. Seam continuity comes instead from
--   the grid DOUBLE-COVERING the seam region: every near-seam
--   physical tile appears at both its canonical position and its
--   u-alias, each with its full physical neighbourhood in-grid, and
--   the per-chunk index stores entries under both (unwrapped) chunk
--   keys. ('World.Fluid.River.Identify.Common.stepDir' wraps E/W as an
--   x-torus instead; on real terrain the two conventions are
--   equivalent because those torus edges only reach void.)
--
--   This module holds the entry point and orchestrator that wires
--   the pipeline stages together; each stage's implementation lives
--   in its own submodule as noted above.
module World.Fluid.Lake.Identify
    ( identifyWorldLakes
    , computeWorldEdgeOcean
    , computeRenderedOcean
    ) where

import UPrelude
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as VU
import qualified Data.Vector.Unboxed.Mutable as VUM
import World.Chunk.Types (chunkSize)
import World.Ocean.Types (OceanMap)
import World.Fluid.Lake.Identify.ChunkIndex
    (buildChunkIndex, buildLakeCarveIndex)
import World.Fluid.Lake.Identify.Components
    (LakeWithId(..), buildLakes, dropOldId, labelComponents)
import World.Fluid.Lake.Identify.Flood (priorityFlood)
import World.Fluid.Lake.Identify.Ocean
    ( computeWorldEdgeOcean, computeRenderedOcean, computeRenderedOceanSeed
    , coastalChunkRadius, coastalProximity, dilateChunkSet, nearOceanMask )
import World.Fluid.Lake.Types
    ( Lake(..), WorldLakes(..), emptyWorldLakes )

-- | Phase 1/2 minimum basin size (in tiles). Phase 3 will replace
--   this with a per-basin lookup keyed by the floor tile's climate
--   (precip − evap).
--
--   Kept at 1 for now: dropping single-tile basins creates "water
--   cliff" artifacts where a 1-tile pocket adjacent to a big lake
--   was filtered out, leaving the big lake's surface looming over
--   the now-dry pocket. Phase 3 will reintroduce climate-driven
--   filtering.
minBasinTiles ∷ Int
minBasinTiles = 1

-- * Entry point

-- | Run the full lake identification pipeline. Allocates ~50 MB peak
--   for a worldSize=128 world; everything except the final
--   'WorldLakes' is dropped before returning.
--
--   The caller supplies the per-tile world terrain, expected to be
--   the same pipeline 'World.Generate.Chunk' will use at chunk-gen
--   (see 'computeChunkInteriorTerrain'). The grid is indexed
--   @gy_off * worldTiles + gx_off@ with @gx_off = gx + halfWorld@.
identifyWorldLakes
    ∷ Int                              -- ^ worldSize (chunks per side)
    → OceanMap                         -- ^ chunk-level ocean classifier
    → VU.Vector Int                    -- ^ world terrain (worldTiles^2)
    → WorldLakes
identifyWorldLakes worldSize oceanMap terrain =
    let worldTiles = worldSize * chunkSize
        -- Which tiles are world-edge connected open ocean? Computed
        -- by BFS from world-boundary cells with @terrain ≤ seaLevel@,
        -- spreading to connected sub-sea tiles. Used to (a) seed the
        -- priority flood with REAL ocean drains only — landlocked
        -- sub-sea depressions are excluded so they form inland-sea
        -- lakes instead of "ocean" with a 100z cliff to the closed
        -- basin's actual rim, and (b) exclude open-ocean tiles from
        -- basin labelling so they're not mistakenly turned into a
        -- giant 'lake' that the chunk-gen ocean classification then
        -- has to override.
        worldOcean = computeWorldEdgeOcean terrain worldTiles
        filled     = priorityFlood terrain worldOcean worldTiles
        labels     = labelComponents terrain filled worldOcean worldTiles
        nLabels    = if VU.null labels
                     then 0
                     else 1 + VU.maximum (VU.cons (-1) labels)
        -- Tiles within 'coastalProximity' of any open-ocean tile.
        -- A sub-sea basin that lives inside this mask gets its
        -- surface clamped to sea level — visually it'd be a sub-sea
        -- bay, not a perched inland lake stepped above the sea.
        -- Seed mask for "near rendered ocean": world-edge BFS ocean
        -- plus any sub-sea tile inside an oceanic chunk. The latter
        -- catches enclaves that 'computeWorldEdgeOcean' missed because
        -- they're not BFS-connected to a world-edge sub-sea tile but
        -- still render as ocean via the chunk-level check.
        renderedOceanMask = computeRenderedOceanSeed worldSize oceanMap
                                                     terrain worldOcean
        coastalMask = nearOceanMask coastalProximity worldTiles
                                    renderedOceanMask terrain
        -- Dilate the oceanic-chunk set out by 'coastalChunkRadius' so a
        -- lake whose chunk sits a couple chunks inland from any
        -- oceanic chunk still registers as coastal. Without this, the
        -- per-tile dilation alone can miss seeds whose tile-BFS path
        -- bumps into a beyond-glacier wall or a narrow land bridge.
        nearOceanicChunks = dilateChunkSet coastalChunkRadius oceanMap
        rawLakes   = buildLakes nLabels terrain filled labels worldTiles
                                coastalMask nearOceanicChunks
        kept       = V.filter
                       (\lw → lkArea (lkLake lw) ≥ minBasinTiles) rawLakes
        -- Renumber: old label → new lake id (-1 = dropped).
        idMap      = VU.create $ do
            v ← VUM.replicate nLabels (-1)
            V.iforM_ kept $ \newI lw →
                VUM.write v (lkOldId lw) newI
            pure v
        -- Strip the temporary 'lkOldId' tag (we only used it to
        -- thread the renumbering — Lake's public fields don't carry
        -- it).
        finalLakes = V.map dropOldId kept
        byChunk    = buildChunkIndex worldTiles idMap labels
        carveByChunk = buildLakeCarveIndex worldTiles terrain finalLakes
                                           byChunk
    in if V.null finalLakes
       then emptyWorldLakes
       else WorldLakes
            { wlLakes      = finalLakes
            , wlByChunk    = byChunk
            , wlCarveDelta = carveByChunk
            }
