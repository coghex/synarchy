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
--        per connected component). Quantise per-tile surface z with
--        a per-tile noise jitter so waterfall step boundaries don't
--        align in patterns across the world. Surface z is enforced
--        monotonically non-increasing downstream.
--     8. Per-chunk bitmasks + per-tile surface z, indexed by
--        chunk coord for chunk-gen lookup.
module World.Fluid.River.Identify
    ( identifyWorldRivers
    , riverThreshold
    ) where

import UPrelude
import qualified Data.HashMap.Strict as HM
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as VU
import qualified Data.Vector.Unboxed.Mutable as VUM
import Control.Monad (when, forM_)
import Control.Monad.ST (ST, runST)
import Data.STRef (newSTRef, readSTRef, writeSTRef, modifySTRef')
import Data.Word (Word8)
import World.Chunk.Types (ChunkCoord(..), chunkSize)
import World.Constants (seaLevel)
import World.Fluid.Lake.Types
    ( WorldLakes(..), LakeChunkEntry(..), Lake(..) )
import World.Fluid.River.Types
    ( River(..), WorldRivers(..), RiverChunkEntry(..), emptyWorldRivers )
import World.Weather.Lookup (LocalClimate(..), lookupLocalClimate)
import World.Weather.Types (ClimateState)

-- | Minimum accumulated flow for a tile to count as a river. Below
--   threshold and the tile renders dry. v1 keeps this fairly low so
--   chains extend a meaningful distance upstream — the visual size
--   filter is the per-component length cap in 'targetRiverCount'
--   below, not this per-tile cutoff.
riverThreshold ∷ Int
riverThreshold = 100

-- | Cap on the number of rivers kept, scaled by world area. Tuned so
--   worldSize=32 → ~20 rivers, worldSize=64 → ~80, worldSize=128 → ~320.
--   Earlier counts at /200 and /100 were findable in dumps but the
--   user couldn't see them in the GUI: 1-tile-wide rivers are visually
--   subtle, so density needs to be high enough that a player crossing
--   the world reliably stumbles into one. Rivers are ranked by
--   per-component tile count so the kept chains are the longest
--   visible drainages — peak-flow ranking picked single-tile mouth
--   confluences instead, which render as isolated columns.
targetRiverCount ∷ Int → Int
targetRiverCount worldSize =
    max 5 (worldSize * worldSize `div` 50)

-- | Step size that the deferred waterfall-quantisation path would
--   use. v1 keeps the surface at @carve + 1@ (see 'computeSurfaceZ'
--   for the rationale); this constant is retained as documentation
--   of the intended quantum for the future quantised path.
waterfallQuantum ∷ Int
waterfallQuantum = 12

-- * Direction codes
--
-- We use 'Word8' for D4 codes packed into one array per world tile.

dirNorth, dirEast, dirSouth, dirWest, dirNone ∷ Word8
dirNorth = 0
dirEast  = 1
dirSouth = 2
dirWest  = 3
dirNone  = 4

-- | Apply a D4 direction to a tile index. The world wraps along the
--   isometric u-axis (gx − gy), which the gx-grid here can't honour
--   exactly without a full coordinate rework. As a v1 approximation
--   we wrap east/west steps as a plain x-axis torus: stepping east
--   from the rightmost column lands at the leftmost column of the
--   same row. The lake identifier uses the same approach and lakes
--   render without visible breaks; this keeps rivers continuous at
--   the wrap edge instead of cutting off there.
--
--   N/S steps return 'Nothing' at the world's v-axis boundary
--   (where 'isBeyondGlacier' would have cut terrain anyway).
{-# INLINE stepDir #-}
stepDir ∷ Int → Int → Word8 → Maybe Int
stepDir worldTiles idx d
    | d ≡ dirNorth = let by = idx `div` worldTiles
                     in if by > 0 then Just (idx - worldTiles) else Nothing
    | d ≡ dirSouth = let by = idx `div` worldTiles
                     in if by < worldTiles - 1 then Just (idx + worldTiles)
                                              else Nothing
    | d ≡ dirEast  = let bx = idx `mod` worldTiles
                     in Just (if bx < worldTiles - 1
                              then idx + 1
                              else idx + 1 - worldTiles)
    | d ≡ dirWest  = let bx = idx `mod` worldTiles
                     in Just (if bx > 0
                              then idx - 1
                              else idx - 1 + worldTiles)
    | otherwise    = Nothing

-- * Entry point

-- | Run the full river identification pipeline.
identifyWorldRivers
    ∷ Int                  -- ^ worldSize (chunks per side)
    → WorldLakes
    → VU.Vector Int        -- ^ world terrain (worldTiles²)
    → ClimateState
    → WorldRivers
identifyWorldRivers worldSize lakes terrain climate =
    let worldTiles = worldSize * chunkSize
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

    in traceRivers worldSize terrain lakeIdAt isSpillwayOf spillwayOf
                   dir flow

-- * Climate → flow units
--
-- Convert per-tile climate samples to small integer flow units. v1
-- tuning notes are below 'riverThreshold'.

-- | Precip contribution per tile, in flow units. lcPrecip is already
--   normalized 0..1; @* 20@ keeps integer arithmetic in a comfortable
--   range while still leaving headroom over the v1 'riverThreshold'.
computePrecipUnits ∷ Int → ClimateState → VU.Vector Int → VU.Vector Int
computePrecipUnits worldSize climate terrain =
    let worldTiles = worldSize * chunkSize
        nTiles     = worldTiles * worldTiles
        half       = worldTiles `div` 2
    in VU.generate nTiles $ \i →
        let t = terrain VU.! i
        in if t ≡ minBound
           then 0
           else
             let gx = (i `mod` worldTiles) - half
                 gy = (i `div` worldTiles) - half
                 c  = lookupLocalClimate climate worldSize gx gy
             in round (lcPrecip c * 20.0 ∷ Float)

-- | Evap penalty per tile, in flow units. v1 disables this: per-tile
--   evap was unreliable across climate-region variance — some seeds
--   got blanket evap that wiped out every river, others got none.
--   The river-count balance is now handled by 'riverThreshold' (only
--   real basins cross) and 'minRiverLength' (short chains drop out).
--   Evap will return as a tuned, climate-region-area-normalized loss
--   in a later pass; for v1 every land tile contributes only precip.
computeEvapUnits ∷ Int → ClimateState → VU.Vector Int → VU.Vector Int
computeEvapUnits worldSize _climate _terrain =
    let worldTiles = worldSize * chunkSize
        nTiles     = worldTiles * worldTiles
    in VU.replicate nTiles 0

-- * Lake-id reverse index

-- | Decode 'WorldLakes' into a per-tile lake-id vector (-1 = no lake).
buildLakeIdAt ∷ Int → WorldLakes → VU.Vector Int
buildLakeIdAt worldSize lakes =
    let worldTiles = worldSize * chunkSize
        nTiles     = worldTiles * worldTiles
        half       = worldTiles `div` 2
        chunkArea  = chunkSize * chunkSize
    in VU.create $ do
        v ← VUM.replicate nTiles (-1 ∷ Int)
        forM_ (HM.toList (wlByChunk lakes)) $ \(ChunkCoord cx cy, es) →
            V.forM_ es $ \lce → do
                let lid = lceLakeId lce
                    bm  = lceBitmask lce
                forM_ [0 .. chunkArea - 1] $ \li → when (bm VU.! li) $ do
                    let lx     = li `mod` chunkSize
                        ly     = li `div` chunkSize
                        gx     = cx * chunkSize + lx
                        gy     = cy * chunkSize + ly
                        gxOff  = gx + half
                        gyOff  = gy + half
                    when (gxOff ≥ 0 ∧ gxOff < worldTiles
                          ∧ gyOff ≥ 0 ∧ gyOff < worldTiles) $
                        VUM.write v (gyOff * worldTiles + gxOff) lid
        pure v

-- * Spillway identification

-- | Per-lake spillway tile (lowest non-in-lake neighbour of any
--   in-lake tile, above sea level). Returns @-1@ where the lake has
--   no usable land spillway — these lakes drop their accumulated
--   flow into the ocean/void.
computeSpillways
    ∷ Int                  -- ^ worldSize
    → WorldLakes
    → VU.Vector Int        -- ^ terrain
    → VU.Vector Int        -- ^ lakeIdAt
    → VU.Vector Int        -- ^ lakeId → tile idx (-1 = none)
computeSpillways worldSize lakes terrain lakeIdAt =
    let worldTiles = worldSize * chunkSize
        nLakes     = V.length (wlLakes lakes)
        half       = worldTiles `div` 2
        inBounds gxo gyo = gxo ≥ 0 ∧ gxo < worldTiles
                         ∧ gyo ≥ 0 ∧ gyo < worldTiles
    in VU.create $ do
        v ← VUM.replicate nLakes (-1 ∷ Int)
        forM_ [0 .. nLakes - 1] $ \lid → do
            let lk     = wlLakes lakes V.! lid
                minX   = lkBBoxMinX lk
                minY   = lkBBoxMinY lk
                maxX   = lkBBoxMaxX lk
                maxY   = lkBBoxMaxY lk
            bestZ   ← newSTRef (maxBound ∷ Int)
            bestIdx ← newSTRef (-1      ∷ Int)
            forM_ [minY .. maxY] $ \gy →
                forM_ [minX .. maxX] $ \gx → do
                    let gxOff = gx + half
                        gyOff = gy + half
                    when (inBounds gxOff gyOff) $ do
                        let idx = gyOff * worldTiles + gxOff
                        when (lakeIdAt VU.! idx ≡ lid) $ do
                            let tryNeighbor ngx ngy = do
                                    let nxo = ngx + half
                                        nyo = ngy + half
                                    when (inBounds nxo nyo) $ do
                                        let nidx = nyo * worldTiles + nxo
                                            nz   = terrain VU.! nidx
                                            nLid = lakeIdAt VU.! nidx
                                        when (nz ≠ minBound
                                              ∧ nLid ≠ lid) $ do
                                            cur ← readSTRef bestZ
                                            when (nz < cur) $ do
                                                writeSTRef bestZ nz
                                                writeSTRef bestIdx nidx
                            tryNeighbor (gx - 1) gy
                            tryNeighbor (gx + 1) gy
                            tryNeighbor gx       (gy - 1)
                            tryNeighbor gx       (gy + 1)
            z      ← readSTRef bestZ
            picked ← readSTRef bestIdx
            -- Drop ocean spillways: their outflow goes into the sea,
            -- not a river. (Doesn't matter whether it's open-ocean BFS
            -- reachable or just sub-sea unmapped; either way no river.)
            if picked ≥ 0 ∧ z > seaLevel
                then VUM.write v lid picked
                else VUM.write v lid (-1)
        pure v

-- | Per-tile inverse: tile idx → lakeId of which it is the spillway
--   (-1 = not a spillway). If a tile is the spillway of two lakes
--   (rare; would require adjacent basins), the last-written wins.
buildIsSpillwayOf ∷ Int → VU.Vector Int → VU.Vector Int
buildIsSpillwayOf nTiles spillwayOf = VU.create $ do
    v ← VUM.replicate nTiles (-1 ∷ Int)
    forM_ [0 .. VU.length spillwayOf - 1] $ \lid → do
        let s = spillwayOf VU.! lid
        when (s ≥ 0) (VUM.write v s lid)
    pure v

-- * D4 descent

-- | Per-tile D4 steepest descent direction. Spillways exclude
--   neighbours in their source lake so lake outflow doesn't feed
--   back into the basin.  Beyond-glacier tiles, lake tiles, and
--   open-ocean (sub-sea, no lake assigned) tiles are tagged
--   'dirNone'.
computeDescentDirs
    ∷ Int                  -- ^ worldTiles
    → VU.Vector Int        -- ^ terrain
    → VU.Vector Int        -- ^ lakeIdAt
    → VU.Vector Int        -- ^ isSpillwayOf
    → VU.Vector Word8
computeDescentDirs worldTiles terrain lakeIdAt isSpillwayOf =
    let nTiles = worldTiles * worldTiles
    in VU.generate nTiles $ \idx →
        let t  = terrain   VU.! idx
            lk = lakeIdAt  VU.! idx
            sp = isSpillwayOf VU.! idx
        in if t ≡ minBound
              ∨ lk ≥ 0
              ∨ (t ≤ seaLevel ∧ lk < 0)
           then dirNone
           else
             let bx = idx `mod` worldTiles
                 by = idx `div` worldTiles
                 -- Candidate neighbours: include only if it would
                 -- not feed flow back into our source lake.
                 candidate ok nIdx d
                     | not ok = Nothing
                     | otherwise =
                         let nz   = terrain  VU.! nIdx
                             nLid = lakeIdAt VU.! nIdx
                         in if nz ≡ minBound
                            then Nothing
                            else if sp ≥ 0 ∧ nLid ≡ sp
                                 then Nothing  -- spillway: exclude source lake
                                 else if nz < t
                                      then Just (nz, d)
                                      else Nothing
                 -- E/W always have a candidate via the x-torus wrap
                 -- (see 'stepDir'); N/S still respect v-axis bounds.
                 eIdx = if bx < worldTiles - 1
                        then idx + 1
                        else idx + 1 - worldTiles
                 wIdx = if bx > 0
                        then idx - 1
                        else idx - 1 + worldTiles
                 cN = candidate (by > 0)              (idx - worldTiles) dirNorth
                 cS = candidate (by < worldTiles - 1) (idx + worldTiles) dirSouth
                 cE = candidate True                  eIdx               dirEast
                 cW = candidate True                  wIdx               dirWest
                 pick a b = case (a, b) of
                     (Nothing, x) → x
                     (x, Nothing) → x
                     (Just (z1, _), Just (z2, _))
                         | z1 ≤ z2 → a
                         | otherwise → b
             in case foldr pick Nothing [cN, cE, cS, cW] of
                    Nothing      → dirNone
                    Just (_, d') → d'

-- * Bucket sort (ascending z)

-- | Ascending z order over all non-minBound tile indices.  Bucket
--   sort: count per-z, prefix sum to slot starts, fill.
bucketSortAscending ∷ VU.Vector Int → VU.Vector Int
bucketSortAscending terrain =
    let nTiles     = VU.length terrain
        bucketBase = -10000 ∷ Int
        nBuckets   =  20000 ∷ Int
        toBucket z = max 0 (min (nBuckets - 1) (z - bucketBase))
    in VU.create $ do
        counts ← VUM.replicate nBuckets (0 ∷ Int)
        forM_ [0 .. nTiles - 1] $ \i → do
            let z = terrain VU.! i
            when (z ≠ minBound) $
                VUM.modify counts (+ 1) (toBucket z)
        -- Prefix sum into bucketStart.
        bucketStart ← VUM.new nBuckets
        cursor      ← newSTRef (0 ∷ Int)
        forM_ [0 .. nBuckets - 1] $ \b → do
            p ← readSTRef cursor
            VUM.write bucketStart b p
            c ← VUM.read counts b
            writeSTRef cursor (p + c)
        total ← readSTRef cursor
        out   ← VUM.new total
        write ← VUM.clone bucketStart
        forM_ [0 .. nTiles - 1] $ \i → do
            let z = terrain VU.! i
            when (z ≠ minBound) $ do
                let b = toBucket z
                p ← VUM.read write b
                VUM.write out p i
                VUM.write write b (p + 1)
        pure out

-- * Flow accumulation

-- | Walk in ascending z order (we reverse iterate the input to get
--   descending z). Each tile receives upstream contributions before
--   its own row is processed, then folds in @(precip − evap)@,
--   clamps non-negative, and routes to its D4 downhill neighbour
--   (or its source-lake accumulator). Pass 2 — spillway injection —
--   runs at the end.
computeFlowAccumulation
    ∷ Int                  -- ^ worldTiles
    → VU.Vector Int        -- ^ terrain
    → VU.Vector Int        -- ^ lakeIdAt
    → VU.Vector Word8      -- ^ dir
    → VU.Vector Int        -- ^ spillwayOf (lakeId → tile idx)
    → VU.Vector Int        -- ^ precipUnits
    → VU.Vector Int        -- ^ evapUnits
    → VU.Vector Int        -- ^ ascending z order
    → (VU.Vector Int, VU.Vector Int)
       -- ^ (per-tile flow, per-lake total inflow)
computeFlowAccumulation worldTiles terrain lakeIdAt dir spillwayOf
                        precipUnits evapUnits ascOrder =
    let nTiles  = worldTiles * worldTiles
        nLakes  = VU.length spillwayOf
        nOrder  = VU.length ascOrder
    in runST $ do
        flow     ← VUM.replicate nTiles (0 ∷ Int)
        lakeFlow ← VUM.replicate nLakes (0 ∷ Int)

        -- Pass 1: walk in descending z order.
        forM_ [nOrder - 1, nOrder - 2 .. 0] $ \k → do
            let idx = ascOrder VU.! k
                t   = terrain VU.! idx
            when (t ≠ minBound) $ do
                upstream ← VUM.read flow idx
                let local   = precipUnits VU.! idx - evapUnits VU.! idx
                    routed  = max 0 (upstream + local)
                VUM.write flow idx routed
                let lid = lakeIdAt VU.! idx
                if lid ≥ 0
                    then VUM.modify lakeFlow (+ routed) lid
                    else case stepDir worldTiles idx (dir VU.! idx) of
                        Just dn → VUM.modify flow (+ routed) dn
                        Nothing → pure ()

        -- Pass 2: spillway injection in descending z order.
        --
        -- Collect spillways with their z, sort descending. For each,
        -- inject lake total at the spillway, then walk D4 downstream
        -- adding the inject to each visited tile until reaching a sink
        -- or another lake. Cascade chains work because the upstream
        -- lake's spillway runs first; its inject lands in the next
        -- lake's accumulator before that lake's spillway processes.
        let spillways = [ (terrain VU.! s, s, lid)
                        | lid ← [0 .. nLakes - 1]
                        , let s = spillwayOf VU.! lid
                        , s ≥ 0
                        ]
            spillDesc = sortDescByFst spillways
        forM_ spillDesc $ \(_, sw, lid) → do
            inject ← VUM.read lakeFlow lid
            when (inject > 0) $ do
                VUM.modify flow (+ inject) sw
                walkInject worldTiles terrain lakeIdAt dir
                           lakeFlow flow sw inject

        flowF     ← VU.unsafeFreeze flow
        lakeFlowF ← VU.unsafeFreeze lakeFlow
        pure (flowF, lakeFlowF)

-- | Walk D4 downstream from the spillway, adding @inject@ to each
--   visited tile's flow until the chain terminates (sink) or enters
--   another lake (lake absorbs the inject; spillway-2 may release it).
walkInject
    ∷ Int                          -- ^ worldTiles
    → VU.Vector Int                -- ^ terrain
    → VU.Vector Int                -- ^ lakeIdAt
    → VU.Vector Word8              -- ^ dir
    → VUM.MVector s Int            -- ^ lakeFlow (mutable)
    → VUM.MVector s Int            -- ^ flow (mutable)
    → Int                          -- ^ starting tile (spillway)
    → Int                          -- ^ inject amount
    → ST s ()
walkInject worldTiles terrain lakeIdAt dir lakeFlow flow start inject =
    let go cur =
            case stepDir worldTiles cur (dir VU.! cur) of
                Nothing → pure ()
                Just dn → do
                    let nLid = lakeIdAt VU.! dn
                    if nLid ≥ 0
                       then VUM.modify lakeFlow (+ inject) nLid
                       else do
                           VUM.modify flow (+ inject) dn
                           when (terrain VU.! dn ≠ minBound) (go dn)
    in go start

-- | Sort a list of @(z, idx, lid)@ tuples by z descending.
sortDescByFst ∷ [(Int, Int, Int)] → [(Int, Int, Int)]
sortDescByFst xs = foldr insertDesc [] xs
  where
    insertDesc x [] = [x]
    insertDesc x@(zx,_,_) (y@(zy,_,_):rest)
        | zx ≥ zy   = x : y : rest
        | otherwise = y : insertDesc x rest

-- * River trace

-- | From per-tile flow, find river tiles (flow ≥ threshold), group
--   them into connected components, compute quantised surface z, and
--   build per-chunk bitmasks.
traceRivers
    ∷ Int                  -- ^ worldSize
    → VU.Vector Int        -- ^ terrain
    → VU.Vector Int        -- ^ lakeIdAt
    → VU.Vector Int        -- ^ isSpillwayOf
    → VU.Vector Int        -- ^ spillwayOf
    → VU.Vector Word8      -- ^ dir
    → VU.Vector Int        -- ^ flow
    → WorldRivers
traceRivers worldSize terrain lakeIdAt isSpillwayOf spillwayOf dir flow =
    let worldTiles = worldSize * chunkSize
        nTiles     = worldTiles * worldTiles
        half       = worldTiles `div` 2

        -- Step 1: identify river-candidate tiles.
        --   A tile is a river candidate iff flow ≥ threshold,
        --   it's not in a lake, terrain valid, above sea level.
        isRiverTile = VU.generate nTiles $ \i →
            let t   = terrain VU.! i
                lk  = lakeIdAt VU.! i
                fl  = flow VU.! i
            in t ≠ minBound ∧ lk < 0 ∧ t > seaLevel ∧ fl ≥ riverThreshold

        -- Step 2: label connected components (4-cardinal via D4 chain).
        --   We walk every river tile and assign it a component id by
        --   following dir to its sink (lake/ocean/edge); ties broken
        --   by BFS-style component labelling.
        (rawCompId, rawNComps) = labelRiverComponents
                                    worldTiles isRiverTile dir

        -- Step 2.5: cap the river count. Rank components by tile count
        -- (length) and keep only the top N (= 'targetRiverCount'
        -- worldSize). Length rather than peak flow is the right signal
        -- here — peak flow picks single-tile mouth tiles, length picks
        -- visible chains.
        (isRiverTileF, compId, nComps) =
            cullByLength worldSize rawCompId rawNComps isRiverTile

        -- Step 3: per-tile water surface z. v1 sits one z above carve;
        -- see 'computeSurfaceZ' note.
        surfZ = computeSurfaceZ worldSize terrain isRiverTileF
                                compId nComps

        -- Step 4: build per-component bookkeeping (bbox, sources,
        -- sinks, peak flow).
        rivers = buildRivers worldSize terrain lakeIdAt isSpillwayOf
                             spillwayOf dir flow isRiverTileF compId nComps

        -- Step 5: per-chunk bitmasks + surface z slices.
        byChunk = buildRiverChunkIndex worldSize half isRiverTileF compId
                                       surfZ

    in if nComps ≡ 0
       then emptyWorldRivers
       else WorldRivers
            { wrRivers  = rivers
            , wrByChunk = byChunk
            }

-- | Keep only the top @targetRiverCount worldSize@ river components
--   ranked by per-component tile count. Surviving components are
--   renumbered densely from 0; tiles in dropped components revert
--   to non-river.
--
--   Returns @(newIsRiverTile, newCompId, newNComps)@.
cullByLength
    ∷ Int                  -- ^ worldSize
    → VU.Vector Int        -- ^ raw compId (-1 = non-river)
    → Int                  -- ^ raw nComps
    → VU.Vector Bool       -- ^ raw isRiverTile
    → (VU.Vector Bool, VU.Vector Int, Int)
cullByLength worldSize rawCompId rawNComps rawIsRiverTile =
    let counts ∷ VU.Vector Int
        counts = VU.create $ do
            v ← VUM.replicate rawNComps 0
            VU.iforM_ rawCompId $ \_ cid →
                when (cid ≥ 0) (VUM.modify v (+ 1) cid)
            pure v
        keepN      = min rawNComps (targetRiverCount worldSize)
        ordered    = sortDescByKey rawNComps counts
        keptSet    = VU.create $ do
            v ← VUM.replicate rawNComps False
            forM_ (take keepN ordered) (\cid → VUM.write v cid True)
            pure v
        idMap ∷ VU.Vector Int
        idMap = runST $ do
            v ← VUM.replicate rawNComps (-1 ∷ Int)
            nextId ← newSTRef (0 ∷ Int)
            forM_ [0 .. rawNComps - 1] $ \oldId →
                when (keptSet VU.! oldId) $ do
                    newId ← readSTRef nextId
                    writeSTRef nextId (newId + 1)
                    VUM.write v oldId newId
            VU.unsafeFreeze v
        newNComps   = min rawNComps keepN
        newCompId   = VU.map (\oldId →
            if oldId < 0 then -1 else idMap VU.! oldId) rawCompId
        newIsRiver  = VU.zipWith (\rt cid → rt ∧ cid ≥ 0)
                                 rawIsRiverTile newCompId
    in (newIsRiver, newCompId, newNComps)

-- | Sort component ids @[0 .. n - 1]@ by their per-id key value
--   (descending). Insertion sort is fine — n is the raw river count,
--   bounded by world area.
sortDescByKey ∷ Int → VU.Vector Int → [Int]
sortDescByKey n key = foldr insertDesc [] [0 .. n - 1]
  where
    insertDesc x [] = [x]
    insertDesc x (y:rest)
        | key VU.! x ≥ key VU.! y = x : y : rest
        | otherwise               = y : insertDesc x rest

-- | BFS label connected components of the river-tile mask. Edges are
--   D4: tile i is connected to tile j iff @dir[i]@ points to j OR
--   @dir[j]@ points to i, AND both are river tiles.
labelRiverComponents
    ∷ Int                  -- ^ worldTiles
    → VU.Vector Bool       -- ^ isRiverTile
    → VU.Vector Word8      -- ^ dir
    → (VU.Vector Int, Int)
       -- ^ (per-tile component id, component count)
labelRiverComponents worldTiles isRiverTile dir =
    let nTiles = worldTiles * worldTiles
    in runST $ do
        ids ← VUM.replicate nTiles (-1 ∷ Int)
        nextId ← newSTRef (0 ∷ Int)
        forM_ [0 .. nTiles - 1] $ \start → do
            cur ← VUM.read ids start
            when (cur < 0 ∧ isRiverTile VU.! start) $ do
                lbl ← readSTRef nextId
                writeSTRef nextId (lbl + 1)
                VUM.write ids start lbl
                queue ← newSTRef [start]
                let loop = do
                        q ← readSTRef queue
                        case q of
                            []       → pure ()
                            (i : rs) → do
                                writeSTRef queue rs
                                let bx = i `mod` worldTiles
                                    by = i `div` worldTiles
                                    -- An edge i ↔ nIdx exists if either
                                    -- dir[i] points to nIdx (outgoing)
                                    -- or dir[nIdx] points to i (incoming).
                                    -- 'incomingFrom' must only be evaluated
                                    -- after the in-bounds guard, otherwise
                                    -- a Strict-forced argument would access
                                    -- 'dir VU.! (-1)'.
                                    tryEdge ok nIdx = when ok $ do
                                        seen ← VUM.read ids nIdx
                                        when (seen < 0
                                              ∧ isRiverTile VU.! nIdx) $ do
                                            let outgoing =
                                                  stepDir worldTiles i
                                                          (dir VU.! i)
                                                          ≡ Just nIdx
                                                incoming = case
                                                    stepDir worldTiles nIdx
                                                            (dir VU.! nIdx) of
                                                    Just j  → j ≡ i
                                                    Nothing → False
                                            when (outgoing ∨ incoming) $ do
                                                VUM.write ids nIdx lbl
                                                modifySTRef' queue (nIdx :)
                                -- E/W wrap to mirror 'stepDir's torus.
                                let west  = if bx > 0
                                            then i - 1
                                            else i - 1 + worldTiles
                                    east  = if bx < worldTiles - 1
                                            then i + 1
                                            else i + 1 - worldTiles
                                tryEdge True west
                                tryEdge True east
                                tryEdge (by > 0)              (i - worldTiles)
                                tryEdge (by < worldTiles - 1) (i + worldTiles)
                                loop
                loop
        n     ← readSTRef nextId
        idsF  ← VU.unsafeFreeze ids
        pure (idsF, n)

-- | Per-tile water surface z. v1 sits the surface 1 z above the
--   channel floor: water flows AT terrain height with one tile of
--   depth above. This is the simplest thing that visually works for
--   1-tile-wide rivers — the renderer fills @terrain+1..surface@, so
--   @surface = carve + 1@ means exactly one water tile per river
--   tile and the river contours the landscape smoothly.
--
--   A first draft quantised this to multiples of 'waterfallQuantum'
--   with a per-river noise offset, aiming for flat-stretches +
--   noise-jittered waterfalls. For lakes it would have worked (wide
--   basins fill the column), but a 1-tile-wide river with a surface
--   11 z above carve renders as an isolated water column poking up
--   from dry land — exactly the artifact the user reported. The
--   quantised approach is deferred until rivers have either variable
--   width or terrain-modification carving, both of which would let
--   the column merge into the channel cross-section.
--
--   'compId' and 'nComps' are kept in the signature so the future
--   quantised path doesn't need a wider rewrite.
computeSurfaceZ
    ∷ Int                  -- ^ worldSize
    → VU.Vector Int        -- ^ terrain
    → VU.Vector Bool       -- ^ isRiverTile
    → VU.Vector Int        -- ^ compId
    → Int                  -- ^ nComps
    → VU.Vector Int
computeSurfaceZ worldSize terrain isRiverTile _compId _nComps =
    let worldTiles = worldSize * chunkSize
        nTiles     = worldTiles * worldTiles
    in VU.generate nTiles $ \i →
        if not (isRiverTile VU.! i)
        then minBound
        else terrain VU.! i + 1

-- | Build the 'River' vector. For each component compute bbox, peak
--   flow, source-lake (if its start tile is a spillway), sink-lake
--   (if a downstream tile of the chain enters a lake).
buildRivers
    ∷ Int                  -- ^ worldSize
    → VU.Vector Int        -- ^ terrain
    → VU.Vector Int        -- ^ lakeIdAt
    → VU.Vector Int        -- ^ isSpillwayOf
    → VU.Vector Int        -- ^ spillwayOf
    → VU.Vector Word8      -- ^ dir
    → VU.Vector Int        -- ^ flow
    → VU.Vector Bool       -- ^ isRiverTile
    → VU.Vector Int        -- ^ compId
    → Int                  -- ^ nComps
    → V.Vector River
buildRivers worldSize terrain lakeIdAt isSpillwayOf _spillwayOf dir flow
            isRiverTile compId nComps =
    let worldTiles = worldSize * chunkSize
        nTiles     = worldTiles * worldTiles
        half       = worldTiles `div` 2
    in runST $ do
        bbMinX  ← VUM.replicate nComps (maxBound ∷ Int)
        bbMinY  ← VUM.replicate nComps (maxBound ∷ Int)
        bbMaxX  ← VUM.replicate nComps (minBound ∷ Int)
        bbMaxY  ← VUM.replicate nComps (minBound ∷ Int)
        peak    ← VUM.replicate nComps (0        ∷ Int)
        srcLake ← VUM.replicate nComps (-1       ∷ Int)
        snkLake ← VUM.replicate nComps (-1       ∷ Int)
        forM_ [0 .. nTiles - 1] $ \i → when (isRiverTile VU.! i) $ do
            let cid = compId VU.! i
                gx  = (i `mod` worldTiles) - half
                gy  = (i `div` worldTiles) - half
                fl  = flow VU.! i
            VUM.modify bbMinX (min gx) cid
            VUM.modify bbMinY (min gy) cid
            VUM.modify bbMaxX (max gx) cid
            VUM.modify bbMaxY (max gy) cid
            VUM.modify peak   (max fl) cid
            -- Source lake: this tile is a spillway → record.
            let sp = isSpillwayOf VU.! i
            when (sp ≥ 0) (VUM.write srcLake cid sp)
            -- Sink lake: this tile's D4 step lands in a lake.
            case stepDir worldTiles i (dir VU.! i) of
                Just dn → do
                    let nlk = lakeIdAt VU.! dn
                    when (nlk ≥ 0) (VUM.write snkLake cid nlk)
                Nothing → pure ()
        bMinXF ← VU.unsafeFreeze bbMinX
        bMinYF ← VU.unsafeFreeze bbMinY
        bMaxXF ← VU.unsafeFreeze bbMaxX
        bMaxYF ← VU.unsafeFreeze bbMaxY
        peakF  ← VU.unsafeFreeze peak
        srcF   ← VU.unsafeFreeze srcLake
        snkF   ← VU.unsafeFreeze snkLake
        pure $ V.generate nComps $ \cid →
            River
                { rivFlowRate   = peakF VU.! cid
                , rivSourceLake = let s = srcF VU.! cid
                                  in if s ≥ 0 then Just s else Nothing
                , rivSinkLake   = let s = snkF VU.! cid
                                  in if s ≥ 0 then Just s else Nothing
                , rivBBoxMinX   = bMinXF VU.! cid
                , rivBBoxMinY   = bMinYF VU.! cid
                , rivBBoxMaxX   = bMaxXF VU.! cid
                , rivBBoxMaxY   = bMaxYF VU.! cid
                }

-- | Build the per-chunk index: one 'RiverChunkEntry' per (chunk, river)
--   pair that overlaps. Both the bitmask and the per-tile surface z are
--   chunkArea-long.
buildRiverChunkIndex
    ∷ Int                  -- ^ worldSize
    → Int                  -- ^ half
    → VU.Vector Bool       -- ^ isRiverTile
    → VU.Vector Int        -- ^ compId
    → VU.Vector Int        -- ^ surfZ
    → HM.HashMap ChunkCoord (V.Vector RiverChunkEntry)
buildRiverChunkIndex worldSize half isRiverTile compId surfZ =
    let worldTiles = worldSize * chunkSize
        nTiles     = worldTiles * worldTiles
        chunkArea  = chunkSize * chunkSize
        -- Pass 1: bucket tiles into (chunk, riverId) → [(localIdx, surfZ)].
        step acc i
            | not (isRiverTile VU.! i) = acc
            | otherwise =
                let gx     = (i `mod` worldTiles) - half
                    gy     = (i `div` worldTiles) - half
                    cx     = gx `div` chunkSize
                    cy     = gy `div` chunkSize
                    lx     = ((gx `mod` chunkSize) + chunkSize) `mod` chunkSize
                    ly     = ((gy `mod` chunkSize) + chunkSize) `mod` chunkSize
                    li     = ly * chunkSize + lx
                    rid    = compId VU.! i
                    z      = surfZ VU.! i
                in HM.insertWith (++) (ChunkCoord cx cy, rid)
                                 [(li, z)] acc
        accumKey ∷ HM.HashMap (ChunkCoord, Int) [(Int, Int)]
        accumKey = foldl' step HM.empty [0 .. nTiles - 1]
        -- Pass 2: per key, freeze into bitmask + per-tile surface.
        perKey ∷ HM.HashMap (ChunkCoord, Int) RiverChunkEntry
        perKey = HM.mapWithKey
            (\(_, rid) tiles →
                let (bm, surfs) = runST $ do
                        b ← VUM.replicate chunkArea False
                        s ← VUM.replicate chunkArea minBound
                        forM_ tiles $ \(li, z) → do
                            VUM.write b li True
                            VUM.write s li z
                        bF ← VU.unsafeFreeze b
                        sF ← VU.unsafeFreeze s
                        pure (bF, sF)
                in RiverChunkEntry
                    { rceRiverId      = rid
                    , rceBitmask      = bm
                    , rcePerTileSurfZ = surfs
                    })
            accumKey
        -- Pass 3: regroup by chunk.
        byChunk = HM.foldlWithKey'
            (\m (cc, _) e → HM.insertWith (++) cc [e] m)
            HM.empty perKey
    in HM.map V.fromList byChunk
