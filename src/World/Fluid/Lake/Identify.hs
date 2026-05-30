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
module World.Fluid.Lake.Identify
    ( identifyWorldLakes
    ) where

import UPrelude
import qualified Data.HashMap.Strict as HM
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as MV
import qualified Data.Vector.Unboxed as VU
import qualified Data.Vector.Unboxed.Mutable as VUM
import Control.Monad (when, forM_)
import Control.Monad.ST (runST)
import Data.STRef (newSTRef, readSTRef, writeSTRef, modifySTRef')
import World.Chunk.Types (ChunkCoord(..), chunkSize)
import World.Constants (seaLevel)
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
    → VU.Vector Int                    -- ^ world terrain (worldTiles^2)
    → WorldLakes
identifyWorldLakes worldSize terrain =
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
        rawLakes   = buildLakes nLabels terrain filled labels worldTiles
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
    in if V.null finalLakes
       then emptyWorldLakes
       else WorldLakes
            { wlLakes   = finalLakes
            , wlByChunk = byChunk
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
    → V.Vector LakeWithId
buildLakes nLabels terrain filled labels worldTiles = runST $ do
    let half = worldTiles `div` 2
    floorsM ← VUM.replicate nLabels (maxBound ∷ Int)
    surfsM  ← VUM.replicate nLabels (minBound ∷ Int)
    areasM  ← VUM.replicate nLabels (0       ∷ Int)
    bxminM  ← VUM.replicate nLabels (maxBound ∷ Int)
    byminM  ← VUM.replicate nLabels (maxBound ∷ Int)
    bxmaxM  ← VUM.replicate nLabels (minBound ∷ Int)
    bymaxM  ← VUM.replicate nLabels (minBound ∷ Int)
    VU.iforM_ labels $ \idx lbl → when (lbl ≥ 0) $ do
        let e  = terrain VU.! idx
            f  = filled  VU.! idx
            gx = (idx `mod` worldTiles) - half
            gy = (idx `div` worldTiles) - half
        VUM.modify floorsM (min e) lbl
        VUM.modify surfsM  (max f) lbl
        VUM.modify areasM  (+ 1)   lbl
        VUM.modify bxminM  (min gx) lbl
        VUM.modify byminM  (min gy) lbl
        VUM.modify bxmaxM  (max gx) lbl
        VUM.modify bymaxM  (max gy) lbl
    floors ← VU.freeze floorsM
    surfs  ← VU.freeze surfsM
    areas  ← VU.freeze areasM
    bxmins ← VU.freeze bxminM
    bymins ← VU.freeze byminM
    bxmaxs ← VU.freeze bxmaxM
    bymaxs ← VU.freeze bymaxM
    pure $ V.generate nLabels $ \i →
        LakeWithId
            { lkOldId = i
            , lkLake  = Lake
                { lkSurface  = surfs  VU.! i
                , lkFloor    = floors VU.! i
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

-- * Steps 6 + 7 — filter and per-chunk bitmasks
--
--   The size filter ('minBasinTiles') runs in 'identifyWorldLakes';
--   the bitmask build is here.

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
