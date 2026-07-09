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
--     3. Bucket-queue priority flood. Seeds: 'worldOcean' tiles +
--        tiles adjacent to beyond-glacier sentinels.
--     4. BFS-label connected basin components (basin tile =
--        @filled > terrain@, not 'worldOcean', not unreached).
--     5. Per-component aggregate: floor, surface (= spillway, no
--        cap), area, tile bbox.
--     6. Drop components below 'minBasinTiles' (Phase 3 will swap
--        in climate-aware filtering).
--     7. Per-chunk bitmasks: for each chunk that any kept lake
--        touches, set bits for its in-lake tiles.
--
--   Output is a 'WorldLakes' that the chunk-gen composer reads with
--   no priority flood, BFS, or bordered-region work at runtime. The
--   per-chunk 'smoothIslandColumns' pass in 'World.Generate.Chunk'
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
module World.Fluid.Lake.Identify
    ( identifyWorldLakes
    , computeWorldEdgeOcean
    , computeRenderedOcean
    ) where

import UPrelude
import qualified Data.HashMap.Strict as HM
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as MV
import qualified Data.Vector.Unboxed as VU
import qualified Data.Vector.Unboxed.Mutable as VUM
import Control.Monad.ST (runST)
import Data.STRef (newSTRef, readSTRef, writeSTRef, modifySTRef')
import World.Chunk.Types (ChunkCoord(..), chunkSize)
import World.Constants (seaLevel)
import qualified Data.HashSet as HS
import World.Fluid.Internal (wrapChunkCoordU)
import World.Ocean.Types (OceanMap)
import World.Fluid.Lake.Types
    ( Lake(..), WorldLakes(..), LakeChunkEntry(..), emptyWorldLakes )

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

-- | Tile-distance from open ocean within which a sub-sea basin gets
--   its surface clamped to 'seaLevel'. Catches a coastal basin that
--   would otherwise render as a lake perched above the adjacent sea
--   when only a thin land bar separates the two. Tuned wide enough
--   to cross the chunk-level oceanic region that surrounds the
--   tile-level world-edge ocean BFS — many "rendered as ocean" tiles
--   aren't in the world-edge BFS because the BFS only walks sub-sea
--   tiles, so a wider dilation lets the coastal-lake detection find
--   basins separated from BFS-ocean by several land tiles.
coastalProximity ∷ Int
coastalProximity = 25

-- | Chunk-level proximity used as a fallback when the tile dilation
--   can't bridge the land between a lake and ocean. A lake whose
--   chunk is within this many cardinal hops of an oceanic chunk is
--   treated as coastal.
coastalChunkRadius ∷ Int
coastalChunkRadius = 3

-- | A coastal basin is sea-level-clamped only when its FLOOR is at
--   or below @seaLevel + clampFloorTolerance@. The coastal tests
--   above are deliberately wide (chunk-dilated geography) — without
--   this gate they reach perched mountain pockets inside "coastal"
--   chunks and the clamp + carve digs them into 1-tile wells with
--   sea-level water at the bottom. Tolerance 2 keeps beach pans a
--   tile or two above sea rendering flush with the adjacent ocean.
clampFloorTolerance ∷ Int
clampFloorTolerance = 2

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

-- | Lake record extended with its raw label id (only used during
--   construction; the public 'Lake' from 'World.Fluid.Lake.Types'
--   does not carry it). We tag-and-renumber so the 'finalLakes'
--   vector is densely indexed from 0.
data LakeWithId = LakeWithId
    { lkOldId ∷ !Int
    , lkLake  ∷ !Lake
    } deriving Show

dropOldId ∷ LakeWithId → Lake
dropOldId = lkLake

-- * Step 2 — priority flood

-- | Bucket-queue priority flood over the world-tile terrain.
--   Output @filled[i]@ is the lowest elevation a drop of water at @i@
--   would have to rise to in order to escape to a seeded boundary
--   (world-edge-connected ocean or world edge). Closed depressions
--   get the rim elevation; slopes and peaks get @terrain[i]@.
--
--   Seeds: tiles flagged in 'worldOcean' (world-edge-connected open
--   ocean) plus tiles adjacent to a beyond-glacier sentinel (the
--   world boundary). Landlocked sub-sea tiles are NOT seeded — they
--   instead end up inside a basin component whose surface is the
--   real escape rim.
priorityFlood
    ∷ VU.Vector Int → VU.Vector Bool → Int → VU.Vector Int
priorityFlood terrain worldOcean worldTiles = runST $ do
    let nTiles     = worldTiles * worldTiles
        bucketBase = -10000 ∷ Int
        nBuckets   =  20000 ∷ Int
        toBucket z = max 0 (min (nBuckets - 1) (z - bucketBase))
    zM      ← VUM.replicate nTiles (maxBound ∷ Int)
    procM   ← VUM.replicate nTiles False
    buckets ← MV.replicate nBuckets ([] ∷ [Int])
    cursor  ← newSTRef 0
    let push idx z = do
            VUM.write zM idx z
            VUM.write procM idx True
            let b = toBucket z
            xs ← MV.read buckets b
            MV.write buckets b (idx : xs)
        seedTile idx = do
            let t = terrain VU.! idx
            push idx t
        -- A tile is a "boundary drain" if it has any adjacent
        -- beyond-glacier (= minBound) neighbor. Cheap edge check.
        adjacentToVoid idx =
            let bx = idx `mod` worldTiles
                by = idx `div` worldTiles
                check ok j =
                    ok ∧ terrain VU.! j ≡ minBound
            in check (bx > 0)              (idx - 1)
             ∨ check (bx < worldTiles - 1) (idx + 1)
             ∨ check (by > 0)              (idx - worldTiles)
             ∨ check (by < worldTiles - 1) (idx + worldTiles)
    -- Seed pass.
    forM_ [0 .. nTiles - 1] $ \idx → do
        let t = terrain VU.! idx
        when (t ≠ minBound) $ do
            when (worldOcean VU.! idx ∨ adjacentToVoid idx)
                 (seedTile idx)
    let tryPropagate srcZ nIdx = do
            let nT = terrain VU.! nIdx
            when (nT ≠ minBound) $ do
                done ← VUM.read procM nIdx
                when (not done) $ do
                    let nZ = max nT srcZ
                    push nIdx nZ
        drain = do
            i ← readSTRef cursor
            when (i < nBuckets) $ do
                xs ← MV.read buckets i
                case xs of
                    []           → writeSTRef cursor (i + 1) >> drain
                    (idx : rest) → do
                        MV.write buckets i rest
                        srcZ ← VUM.read zM idx
                        let bx = idx `mod` worldTiles
                            by = idx `div` worldTiles
                        when (bx > 0)              (tryPropagate srcZ (idx - 1))
                        when (bx < worldTiles - 1) (tryPropagate srcZ (idx + 1))
                        when (by > 0)              (tryPropagate srcZ (idx - worldTiles))
                        when (by < worldTiles - 1) (tryPropagate srcZ (idx + worldTiles))
                        drain
    drain
    VU.freeze zM

-- * Step 3 — connected-component labelling

-- | BFS-label every basin tile (where @filled > terrain@ AND the tile
--   isn't world-edge-connected ocean). Non-basin tiles get @-1@.
--   Components are 4-cardinally connected via basin tiles only.
--
--   Sub-sea LANDLOCKED tiles (terrain ≤ seaLevel but NOT in
--   'worldOcean') count as basin tiles — these are inland-sea floors
--   that get water at their basin's rim. World-edge ocean tiles are
--   excluded so the chunk-gen ocean classifier owns them.
labelComponents
    ∷ VU.Vector Int → VU.Vector Int → VU.Vector Bool → Int
    → VU.Vector Int
labelComponents terrain filled worldOcean worldTiles = runST $ do
    let nTiles = worldTiles * worldTiles
        isBasin i =
            let e = terrain VU.! i
                f = filled  VU.! i
            in e ≠ minBound
             ∧ not (worldOcean VU.! i)
             ∧ f ≠ maxBound
                   -- Unreached by the flood: typically tiles enclosed
                   -- by a beyond-glacier ring. Treat as not-a-basin
                   -- so they don't poison their component's surface
                   -- with 'maxBound'.
             ∧ f > e
    labels  ← VUM.replicate nTiles (-1 ∷ Int)
    nextLbl ← newSTRef 0
    forM_ [0 .. nTiles - 1] $ \start → do
        cur ← VUM.read labels start
        when (cur < 0 ∧ isBasin start) $ do
            lbl ← readSTRef nextLbl
            writeSTRef nextLbl (lbl + 1)
            VUM.write labels start lbl
            queue ← newSTRef [start]
            let loop = do
                    q ← readSTRef queue
                    case q of
                        []       → pure ()
                        (i : rs) → do
                            writeSTRef queue rs
                            let bx = i `mod` worldTiles
                                by = i `div` worldTiles
                                tryN ok nIdx
                                    | not ok = pure ()
                                    | otherwise = do
                                        nlbl ← VUM.read labels nIdx
                                        when (nlbl < 0 ∧ isBasin nIdx) $ do
                                            VUM.write labels nIdx lbl
                                            modifySTRef' queue (nIdx :)
                            tryN (bx > 0)              (i - 1)
                            tryN (bx < worldTiles - 1) (i + 1)
                            tryN (by > 0)              (i - worldTiles)
                            tryN (by < worldTiles - 1) (i + worldTiles)
                            loop
            loop
    VU.freeze labels

-- * Steps 4 + 5 — per-component aggregation

-- | Scan the label map once, summarizing each component into a 'Lake'.
buildLakes
    ∷ Int                -- ^ number of labels
    → VU.Vector Int      -- ^ terrain
    → VU.Vector Int      -- ^ filled
    → VU.Vector Int      -- ^ labels (-1 = no basin)
    → Int                -- ^ worldTiles
    → VU.Vector Bool     -- ^ coastalMask
    → OceanMap           -- ^ chunk-level oceanic set
    → V.Vector LakeWithId
buildLakes nLabels terrain filled labels worldTiles coastalMask oceanMap = runST $ do
    let half = worldTiles `div` 2
        worldSize = worldTiles `div` chunkSize
    floorsM ← VUM.replicate nLabels (maxBound ∷ Int)
    surfsM  ← VUM.replicate nLabels (minBound ∷ Int)
    areasM  ← VUM.replicate nLabels (0       ∷ Int)
    bxminM  ← VUM.replicate nLabels (maxBound ∷ Int)
    byminM  ← VUM.replicate nLabels (maxBound ∷ Int)
    bxmaxM  ← VUM.replicate nLabels (minBound ∷ Int)
    bymaxM  ← VUM.replicate nLabels (minBound ∷ Int)
    coastalM ← VUM.replicate nLabels False
    VU.iforM_ labels $ \idx lbl → when (lbl ≥ 0) $ do
        let e  = terrain VU.! idx
            f  = filled  VU.! idx
            gx = (idx `mod` worldTiles) - half
            gy = (idx `div` worldTiles) - half
            cx = gx `div` chunkSize
            cy = gy `div` chunkSize
            -- Coastal via the dilated tile mask:
            inMask = coastalMask VU.! idx
            -- Coastal via the dilated chunk set. Wrap the chunk coord
            -- through the u-axis torus so the lookup matches the
            -- canonical key 'computeOceanMap' uses (chunks at the wrap
            -- edge live at their wrapped coord).
            chunkNearOcean =
                HS.member (wrapChunkCoordU worldSize (ChunkCoord cx cy))
                          oceanMap
        VUM.modify floorsM (min e) lbl
        VUM.modify surfsM  (max f) lbl
        VUM.modify areasM  (+ 1)   lbl
        VUM.modify bxminM  (min gx) lbl
        VUM.modify byminM  (min gy) lbl
        VUM.modify bxmaxM  (max gx) lbl
        VUM.modify bymaxM  (max gy) lbl
        when (inMask ∨ chunkNearOcean) (VUM.write coastalM lbl True)
    floors ← VU.freeze floorsM
    surfs  ← VU.freeze surfsM
    areas  ← VU.freeze areasM
    bxmins ← VU.freeze bxminM
    bymins ← VU.freeze byminM
    bxmaxs ← VU.freeze bxmaxM
    bymaxs ← VU.freeze bymaxM
    coastal ← VU.freeze coastalM
    pure $ V.generate nLabels $ \i →
        let rawSurf  = surfs  VU.! i
            rawFloor = floors VU.! i
            -- Coastal basin: clamp surface to sea level so it
            -- renders flush with the adjacent open ocean instead of
            -- as a perched inland lake stepped above it. For
            -- above-sea floors the chunk-gen pipeline carves the
            -- basin tiles down to sub-sea via 'wlCarveDelta' so the
            -- water plane still finds tiles to render at @seaLevel@.
            --
            -- The clamp is gated on the basin FLOOR sitting at or
            -- barely above sea level ('clampFloorTolerance'). The
            -- coastal test alone is chunk-dilated geography — it
            -- reaches 3 chunks inland and through dry sub-sea
            -- basins, so without the floor gate a 1-tile mountain
            -- pocket at z 35 inside a "coastal" chunk would clamp
            -- 36→0 and then carve 35→−1: the infamous 1-tile wells
            -- with 30z walls and a film of sea-level water at the
            -- bottom (TERRAIN_PIT in world_audit, fixed 2026-06-05).
            -- Beach pans a tile or two above sea still clamp and
            -- render flush — the intent of dropping the original
            -- floor restriction — but perched basins keep their rim
            -- surface and render as ordinary ponds.
            adjSurf
              | coastal VU.! i
              ∧ rawSurf > seaLevel
              ∧ rawFloor ≤ seaLevel + clampFloorTolerance = seaLevel
              | otherwise                                  = rawSurf
        in LakeWithId
            { lkOldId = i
            , lkLake  = Lake
                { lkSurface  = adjSurf
                , lkFloor    = rawFloor
                , lkArea     = areas  VU.! i
                , lkBBoxMinX = bxmins VU.! i
                , lkBBoxMinY = bymins VU.! i
                , lkBBoxMaxX = bxmaxs VU.! i
                , lkBBoxMaxY = bymaxs VU.! i
                }
            }

-- * Step 2 — world-edge ocean BFS

-- | Tile-resolution BFS: which tiles are open ocean reachable from
--   the world boundary? Seeds: tiles on the world-boundary ring
--   (outermost row/col of the grid) that are at or below sea level.
--   Propagates 4-cardinally to neighbors that are also at or below
--   sea level. Beyond-glacier sentinel cells (@minBound@) are walls.
--
--   The output is consumed by 'priorityFlood' (as drain seeds) and
--   'labelComponents' (to exclude open ocean from basin labelling).
--   Landlocked sub-sea-level tiles are NOT in the result, so they
--   join an inland basin component and get a proper rim-elevation
--   surface instead of the ocean's sea level.
computeWorldEdgeOcean ∷ VU.Vector Int → Int → VU.Vector Bool
computeWorldEdgeOcean terrain worldTiles = runST $ do
    let nTiles = worldTiles * worldTiles
        isSubSea i =
            let t = terrain VU.! i
            in t ≠ minBound ∧ t ≤ seaLevel
    flag  ← VUM.replicate nTiles False
    queue ← newSTRef ([] ∷ [Int])
    -- Seed: boundary-ring tiles that are sub-sea.
    forM_ [0 .. nTiles - 1] $ \idx → do
        let bx = idx `mod` worldTiles
            by = idx `div` worldTiles
            onEdge = bx ≡ 0 ∨ bx ≡ worldTiles - 1
                   ∨ by ≡ 0 ∨ by ≡ worldTiles - 1
        when (onEdge ∧ isSubSea idx) $ do
            VUM.write flag idx True
            modifySTRef' queue (idx :)
    let bfs = do
            q ← readSTRef queue
            case q of
                []       → pure ()
                (i : rs) → do
                    writeSTRef queue rs
                    let bx = i `mod` worldTiles
                        by = i `div` worldTiles
                        tryN ok nIdx
                            | not ok = pure ()
                            | otherwise = do
                                f ← VUM.read flag nIdx
                                when (not f ∧ isSubSea nIdx) $ do
                                    VUM.write flag nIdx True
                                    modifySTRef' queue (nIdx :)
                    tryN (bx > 0)              (i - 1)
                    tryN (bx < worldTiles - 1) (i + 1)
                    tryN (by > 0)              (i - worldTiles)
                    tryN (by < worldTiles - 1) (i + worldTiles)
                    bfs
    bfs
    VU.freeze flag

-- | Tile-resolution "rendered ocean": every sub-sea tile in a
--   connected sub-sea component that renders as ocean ANYWHERE. Floods
--   (4-connected, through sub-sea) from two seed sets — world-edge
--   sub-sea tiles AND sub-sea tiles in oceanic chunks (oceanMap) — so
--   it fills not just the edge-connected open ocean but also enclosed
--   inland seas whose core the coarse chunk-flood rendered ocean.
--   'composeFluidMap' ORs this in, so a whole chunk the chunk-flood
--   couldn't reach no longer renders dry inside a sea. A truly-dry
--   inland basin (no oceanic chunk in its component) is not flooded
--   and stays a lake / dry, as before.
computeRenderedOcean ∷ Int → OceanMap → VU.Vector Int → VU.Vector Bool
computeRenderedOcean worldSize oceanMap terrain = runST $ do
    let worldTiles = worldSize * chunkSize
        nTiles = worldTiles * worldTiles
        half   = worldTiles `div` 2
        isSubSea i = let t = terrain VU.! i in t ≠ minBound ∧ t ≤ seaLevel
        chunkOf i =
            let gx = (i `mod` worldTiles) - half
                gy = (i `div` worldTiles) - half
            in wrapChunkCoordU worldSize
                   (ChunkCoord (gx `div` chunkSize) (gy `div` chunkSize))
    flag  ← VUM.replicate nTiles False
    queue ← newSTRef ([] ∷ [Int])
    forM_ [0 .. nTiles - 1] $ \idx → when (isSubSea idx) $ do
        let bx = idx `mod` worldTiles
            by = idx `div` worldTiles
            onEdge = bx ≡ 0 ∨ bx ≡ worldTiles - 1
                   ∨ by ≡ 0 ∨ by ≡ worldTiles - 1
        when (onEdge ∨ HS.member (chunkOf idx) oceanMap) $ do
            VUM.write flag idx True
            modifySTRef' queue (idx :)
    let bfs = do
            q ← readSTRef queue
            case q of
                []       → pure ()
                (i : rs) → do
                    writeSTRef queue rs
                    let bx = i `mod` worldTiles
                        by = i `div` worldTiles
                        tryN ok nIdx
                            | not ok = pure ()
                            | otherwise = do
                                f ← VUM.read flag nIdx
                                when (not f ∧ isSubSea nIdx) $ do
                                    VUM.write flag nIdx True
                                    modifySTRef' queue (nIdx :)
                    tryN (bx > 0)              (i - 1)
                    tryN (bx < worldTiles - 1) (i + 1)
                    tryN (by > 0)              (i - worldTiles)
                    tryN (by < worldTiles - 1) (i + worldTiles)
                    bfs
    bfs
    VU.freeze flag

-- * Steps 6 + 7 — filter and per-chunk bitmasks
--
--   The size filter ('minBasinTiles') runs in 'identifyWorldLakes';
--   the bitmask build is here.

-- | Dilate a set of chunks outward by @r@ cardinal hops. Used to
--   build a "near-oceanic" chunk set as a fallback for the tile-level
--   coastal dilation.
dilateChunkSet ∷ Int → HS.HashSet ChunkCoord → HS.HashSet ChunkCoord
dilateChunkSet 0 s = s
dilateChunkSet r s =
    let s' = HS.foldl' (\acc (ChunkCoord cx cy) →
                acc `HS.union` HS.fromList
                    [ ChunkCoord (cx - 1) cy
                    , ChunkCoord (cx + 1) cy
                    , ChunkCoord cx (cy - 1)
                    , ChunkCoord cx (cy + 1)
                    ])
                s s
    in dilateChunkSet (r - 1) s'

-- | Combine the world-edge ocean BFS with the chunk-level oceanic
--   classifier. A tile is included iff it's in either source — so a
--   sub-sea tile that's BFS-isolated but lives in an oceanic chunk
--   (where chunk-gen will render it as Ocean) still counts as ocean
--   for the coastal-lake detection. Catches "renders as ocean" tiles
--   that the pure tile-level BFS misses.
computeRenderedOceanSeed
    ∷ Int                  -- ^ worldSize
    → OceanMap
    → VU.Vector Int        -- ^ terrain
    → VU.Vector Bool       -- ^ worldOcean (BFS result)
    → VU.Vector Bool
computeRenderedOceanSeed worldSize oceanMap terrain worldOcean =
    let worldTiles = worldSize * chunkSize
        nTiles     = worldTiles * worldTiles
        half       = worldTiles `div` 2
    in VU.generate nTiles $ \i →
        if worldOcean VU.! i
        then True
        else
          let t = terrain VU.! i
          in if t ≡ minBound ∨ t > seaLevel
             then False
             else
               let gx = (i `mod` worldTiles) - half
                   gy = (i `div` worldTiles) - half
                   cx = gx `div` chunkSize
                   cy = gy `div` chunkSize
                   worldSize = worldTiles `div` chunkSize
               in HS.member (wrapChunkCoordU worldSize (ChunkCoord cx cy))
                            oceanMap

-- | Dilate the world-edge ocean mask outward by @maxDist@ tiles
--   (4-cardinal). Returns a mask of every land tile that's within
--   @maxDist@ steps of some open-ocean tile. Used by 'buildLakes' to
--   detect coastal basins whose surface should be clamped to
--   'seaLevel'.
nearOceanMask
    ∷ Int                  -- ^ maxDist
    → Int                  -- ^ worldTiles
    → VU.Vector Bool       -- ^ worldOcean
    → VU.Vector Int        -- ^ terrain
    → VU.Vector Bool
nearOceanMask maxDist worldTiles worldOcean terrain = runST $ do
    let nTiles = worldTiles * worldTiles
    near ← VUM.replicate nTiles False
    let seeds = [ i | i ← [0 .. nTiles - 1], worldOcean VU.! i ]
    forM_ seeds (\i → VUM.write near i True)
    frontierRef ← newSTRef seeds
    let expand 0 = pure ()
        expand d = do
            frontier ← readSTRef frontierRef
            nextRef  ← newSTRef ([] ∷ [Int])
            forM_ frontier $ \i → do
                let bx = i `mod` worldTiles
                    by = i `div` worldTiles
                    -- E/W wrap as an x-torus so coastlines at the
                    -- world edge still get dilated into. Without
                    -- this, lakes at the rightmost / leftmost columns
                    -- may not register as coastal when the wrapped
                    -- neighbour is ocean.
                    east = if bx < worldTiles - 1
                           then i + 1
                           else i + 1 - worldTiles
                    west = if bx > 0
                           then i - 1
                           else i - 1 + worldTiles
                    tryN ok nIdx = when ok $ do
                        let nT = terrain VU.! nIdx
                        cur ← VUM.read near nIdx
                        when (nT ≠ minBound ∧ not cur) $ do
                            VUM.write near nIdx True
                            modifySTRef' nextRef (nIdx :)
                tryN True                  west
                tryN True                  east
                tryN (by > 0)              (i - worldTiles)
                tryN (by < worldTiles - 1) (i + worldTiles)
            next ← readSTRef nextRef
            writeSTRef frontierRef next
            expand (d - 1)
    expand maxDist
    VU.unsafeFreeze near

-- | For each chunk holding tiles of a clamped lake (lkSurface ≤
--   seaLevel), build a per-tile carve delta vector that pulls each
--   above-sea-level lake tile down to @seaLevel − 1@. Without this,
--   a coastal basin with floor above sea level would clamp its
--   surface but render no water — composeFluidMap would see
--   @lkSurf < terrZ@ on every tile and skip them. The carve makes
--   the basin floor sub-sea so the clamped surface fills correctly.
--
--   Chunks with no clamped-lake tiles are absent from the output.
buildLakeCarveIndex
    ∷ Int                  -- ^ worldTiles
    → VU.Vector Int        -- ^ world terrain (pre-carve)
    → V.Vector Lake        -- ^ finalLakes (post-clamp surface values)
    → HM.HashMap ChunkCoord (V.Vector LakeChunkEntry)
    → HM.HashMap ChunkCoord (VU.Vector Int)
buildLakeCarveIndex worldTiles terrain finalLakes lakesByChunk =
    let half       = worldTiles `div` 2
        chunkArea  = chunkSize * chunkSize
        processChunk (ChunkCoord cx cy) entries = runST $ do
            v ← VUM.replicate chunkArea (0 ∷ Int)
            V.forM_ entries $ \entry → do
                let lid  = lceLakeId entry
                    bm   = lceBitmask entry
                    lake = finalLakes V.! lid
                when (lkSurface lake ≤ seaLevel) $
                    forM_ [0 .. chunkArea - 1] $ \li → when (bm VU.! li) $ do
                        let lx    = li `mod` chunkSize
                            ly    = li `div` chunkSize
                            gx    = cx * chunkSize + lx
                            gy    = cy * chunkSize + ly
                            gxOff = gx + half
                            gyOff = gy + half
                        when (gxOff ≥ 0 ∧ gxOff < worldTiles
                              ∧ gyOff ≥ 0 ∧ gyOff < worldTiles) $ do
                            let idx = gyOff * worldTiles + gxOff
                                t   = terrain VU.! idx
                                d   = max 0 (t - (seaLevel - 1))
                            cur ← VUM.read v li
                            when (d > cur) (VUM.write v li d)
            VU.unsafeFreeze v
        result = HM.mapWithKey processChunk lakesByChunk
        -- Drop chunks where every tile delta ended up at zero (a chunk
        -- with only non-clamped lakes).
    in HM.filter (\dv → VU.any (> 0) dv) result

-- | For every chunk that overlaps a kept lake, build a bitmask of
--   which chunk-local tiles belong to that lake. Chunks with no
--   lakes are omitted from the output map.
buildChunkIndex
    ∷ Int                -- ^ worldTiles
    → VU.Vector Int      -- ^ old label → new lake id (-1 = dropped)
    → VU.Vector Int      -- ^ labels
    → HM.HashMap ChunkCoord (V.Vector LakeChunkEntry)
buildChunkIndex worldTiles idMap labels =
    let half      = worldTiles `div` 2
        chunkArea = chunkSize * chunkSize
        nTiles    = worldTiles * worldTiles
        -- Pass 1: accumulate (chunk, lakeId) → [(lx, ly)] tile lists.
        accumKey ∷ HM.HashMap (ChunkCoord, Int) [(Int, Int)]
        accumKey = foldl' step HM.empty [0 .. nTiles - 1]
        step acc idx =
            let oldLbl = labels VU.! idx
            in if oldLbl < 0
               then acc
               else
                 let newId = idMap VU.! oldLbl
                 in if newId < 0
                    then acc
                    else
                      let gx = (idx `mod` worldTiles) - half
                          gy = (idx `div` worldTiles) - half
                          cx = gx `div` chunkSize
                          cy = gy `div` chunkSize
                          lx = ((gx `mod` chunkSize) + chunkSize) `mod` chunkSize
                          ly = ((gy `mod` chunkSize) + chunkSize) `mod` chunkSize
                      in HM.insertWith (++) (ChunkCoord cx cy, newId)
                                       [(lx, ly)] acc
        -- Pass 2: freeze each tile list into a bitmask.
        perKey ∷ HM.HashMap (ChunkCoord, Int) LakeChunkEntry
        perKey = HM.mapWithKey
            (\(_, lid) tiles →
                let bm = VU.create $ do
                        v ← VUM.replicate chunkArea False
                        forM_ tiles $ \(lx, ly) →
                            VUM.write v (ly * chunkSize + lx) True
                        pure v
                in LakeChunkEntry { lceLakeId = lid, lceBitmask = bm })
            accumKey
        -- Pass 3: regroup by chunk, dropping the redundant lake-id key.
        byChunk = HM.foldlWithKey'
            (\m (cc, _) e → HM.insertWith (++) cc [e] m)
            HM.empty perKey
    in HM.map V.fromList byChunk
