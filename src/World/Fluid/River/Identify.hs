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
module World.Fluid.River.Identify
    ( identifyWorldRivers
    , riverThreshold
    ) where

import UPrelude
import qualified Data.HashMap.Strict as HM
import qualified Data.IntMap.Strict as IM
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as VU
import qualified Data.Vector.Unboxed.Mutable as VUM
import Control.Monad (when, forM_, foldM)
import Control.Monad.ST (ST, runST)
import Data.STRef (newSTRef, readSTRef, writeSTRef, modifySTRef')
import Data.Word (Word8)
import World.Chunk.Types (ChunkCoord(..), chunkSize)
import World.Constants (seaLevel)
import World.Fluid.Lake.Identify (computeWorldEdgeOcean)
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

-- | Reference flow level that maps to one tile of perpendicular width.
--   See 'widthRadiusFromFlow'. With @widthScale = 500@: flow 500 →
--   radius 0 (1 wide), flow 1000 → radius 1 (3 wide), flow 2000 → 2
--   (5 wide), flow 4000+ → 3 (7 wide).
widthScale ∷ Int
widthScale = 500

-- | Maximum half-width. Caps the widest river at 7 tiles end-to-end
--   (centre + 3 perpendicular on each side).
maxWidthRadius ∷ Int
maxWidthRadius = 3

-- | Per-tile width radius from accumulated flow. Logarithmic ramp so
--   small rivers stay narrow but big ones still hit the cap.
widthRadiusFromFlow ∷ Int → Int
widthRadiusFromFlow f =
    let ratio = fromIntegral (max 1 f) / fromIntegral widthScale ∷ Float
    in max 0 (min maxWidthRadius
                  (floor (logBase 2.0 (max 1.0 ratio)) ∷ Int))

-- | Maximum water-surface drop between a river tile and its
--   downstream neighbour. Reaches with slope ≤ this follow terrain
--   exactly (no extra carving, identical to unclamped behaviour);
--   steeper drops (cliffs) are absorbed by carving a stepped gorge so
--   the water never renders as one tall vertical sheet. Applied by
--   'clampCentreSurfaces'.
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
                   dir flow ascOrder

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

-- | Evap penalty per tile, in flow units. Conservative gradient: most
--   temperate tiles end up at zero, hot tiles take a small linear
--   penalty above 25 °C, arid tiles take a small linear penalty below
--   30 % humidity. Together they produce typical evap of 0–3 units in
--   land regions. v2 pairs this with the river-chain extension pass
--   below — if evap drops local flow below threshold, the chain
--   continues to its terminus at minimum width instead of fragmenting.
computeEvapUnits ∷ Int → ClimateState → VU.Vector Int → VU.Vector Int
computeEvapUnits worldSize climate terrain =
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
                 tempPart = max 0.0 ((lcTemp c - 35.0) * 0.1) ∷ Float
                 humPart  = max 0.0 ((0.2  - lcHumidity c) * 2.0) ∷ Float
             in max 0 (round (tempPart + humPart))

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
    → VU.Vector Int        -- ^ ascending z order
    → WorldRivers
traceRivers worldSize terrain lakeIdAt isSpillwayOf spillwayOf dir flow
            ascOrder =
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
                                         isRiverCentre ascOrder

        -- Step 1c: variable width. Each centre tile widens perpendicular
        -- to its D4 dir up to 'widthRadiusFromFlow'. Returns the expanded
        -- river-tile mask, per-tile width radius, and per-tile water
        -- surface z (centre tiles use the waterfall-clamped surface from
        -- 'clampCentreSurfaces'; widened tiles inherit the lowest
        -- centre's surface so the water plane stays flat across the
        -- river's cross-section).
        (isRiverTile, widthRadius, surfZ) =
            expandWidth worldTiles terrain dir flow isRiverCentre centreSurf

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
        (isRiverTileF, compIdF, nComps) =
            cullByLength worldSize rawCompId rawNComps isRiverTile

        -- Step 3: coastal breakthrough. Stranded inland mouths within
        -- 'breakthroughRange' of an ocean tile get a Dijkstra-found
        -- path carved through the blocking terrain.  Path tiles are
        -- added to the bitmask with width 0; surface descends to
        -- @seaLevel + 1@ at the path's end.
        worldOcean = computeWorldEdgeOcean terrain worldTiles
        (isRiverTileB, compIdB, widthRadiusB, surfZB) =
            addBreakthroughs worldTiles isRiverTileF compIdF dir terrain
                             worldOcean widthRadius surfZ

        -- Step 3.5: lateral waterfall clamp. The chain clamp (1c-pre)
        -- bounds steps along flow edges only; meander necks and
        -- parallel reaches can still put two non-flow-linked river
        -- tiles side by side with a bigger gap. Relax to the maximal
        -- surface field with |Δ| ≤ 'waterfallQuantum' across EVERY
        -- adjacent river-tile pair (wings + breakthrough paths
        -- included). The carve pass below absorbs any lowering.
        surfZL = clampLateralSurfaces worldTiles isRiverTileB surfZB

        -- Step 4: build per-component bookkeeping (bbox, sources,
        -- sinks, peak flow). Uses post-breakthrough data so bboxes
        -- include the carved canyons.
        rivers = buildRivers worldSize terrain lakeIdAt isSpillwayOf
                             spillwayOf dir flow isRiverTileB compIdB nComps

        -- Step 5: per-chunk bitmasks + surface z + width slices.
        byChunk = buildRiverChunkIndex worldSize half isRiverTileB compIdB
                                       surfZL widthRadiusB

        -- Step 6: per-tile carve delta. Includes breakthrough path
        -- tiles, which can have large delta where they cut through
        -- coastal mountains.
        carveDelta = computeCarveDelta worldTiles terrain
                                       isRiverTileB widthRadiusB surfZL
        carveByChunk = buildCarveDeltaIndex worldSize half isRiverTileB
                                            carveDelta

    in if nComps ≡ 0
       then emptyWorldRivers
       else WorldRivers
            { wrRivers     = rivers
            , wrByChunk    = byChunk
            , wrCarveDelta = carveByChunk
            }

-- | Extend each primary-river chain downstream along the D4 dir until
--   it reaches a terminus (lake / ocean tile / world boundary / sink).
--   Tiles below threshold are still marked river so an arid stretch
--   mid-chain doesn't fragment the river — width will end up at
--   radius 0 for those tiles because flow there is small.
--
--   Pass in descending-z order so by the time we visit a tile, any
--   upstream chain that reaches it has already marked it.
extendRiverChains
    ∷ Int                  -- ^ worldTiles
    → VU.Vector Int        -- ^ terrain
    → VU.Vector Int        -- ^ lakeIdAt
    → VU.Vector Word8      -- ^ dir
    → VU.Vector Bool       -- ^ isPrimary (flow ≥ threshold + land tile)
    → VU.Vector Int        -- ^ ascending z order
    → VU.Vector Bool
extendRiverChains worldTiles terrain lakeIdAt dir isPrimary ascOrder =
    let nTiles = worldTiles * worldTiles
        nOrd   = VU.length ascOrder
    in VU.create $ do
        isR ← VUM.replicate nTiles False
        forM_ [0 .. nTiles - 1] $ \i →
            when (isPrimary VU.! i) (VUM.write isR i True)
        forM_ [nOrd - 1, nOrd - 2 .. 0] $ \k → do
            let i = ascOrder VU.! k
            mine ← VUM.read isR i
            when mine $
                case stepDir worldTiles i (dir VU.! i) of
                    Nothing → pure ()
                    Just dn → do
                        let dnT       = terrain  VU.! dn
                            dnLake    = lakeIdAt VU.! dn ≥ 0
                            dnOcean   = dnT ≠ minBound ∧ dnT ≤ seaLevel
                                      ∧ not dnLake
                            dnBeyond  = dnT ≡ minBound
                        -- Stop the river at any terminus tile (lake,
                        -- ocean, void). Otherwise propagate the marker.
                        when (not dnLake ∧ not dnOcean ∧ not dnBeyond) $
                            VUM.write isR dn True
        pure isR

-- | Waterfall clamp: per-centre water surface, computed in ascending
--   terrain order so each tile's downstream neighbour is finalised
--   first ('computeDescentDirs' only ever picks a strictly lower
--   neighbour, so a downstream tile always sits in an earlier bucket
--   of the ascending sort):
--
--       surf[u] = min(terrain[u], surf[downstream u] + waterfallQuantum)
--
--   Monotonicity (non-increasing downstream) falls out automatically:
--   terrain[u] > terrain[dn] ≥ surf[dn], so surf[u] ≥ surf[dn], and
--   the min bounds every downstream step at 'waterfallQuantum'. Where
--   terrain drops faster than the quantum (cliffs), the clamped
--   surface dips below local terrain and 'computeCarveDelta' deepens
--   the channel — the river cuts a stepped gorge through the scarp
--   instead of rendering one tall water wall. On reaches with slope
--   ≤ quantum the min picks terrain and behaviour is unchanged.
--
--   Non-centre tiles hold minBound.
clampCentreSurfaces
    ∷ Int                  -- ^ worldTiles
    → VU.Vector Int        -- ^ terrain
    → VU.Vector Word8      -- ^ dir
    → VU.Vector Bool       -- ^ isRiverCentre (after extension)
    → VU.Vector Int        -- ^ ascending z order
    → VU.Vector Int
clampCentreSurfaces worldTiles terrain dir isRiverCentre ascOrder =
    let nTiles = worldTiles * worldTiles
        nOrd   = VU.length ascOrder
    in VU.create $ do
        surf ← VUM.replicate nTiles (minBound ∷ Int)
        forM_ [0 .. nOrd - 1] $ \k → do
            let i = ascOrder VU.! k
            when (isRiverCentre VU.! i) $ do
                let t = terrain VU.! i
                s ← case stepDir worldTiles i (dir VU.! i) of
                    Nothing → pure t
                    Just dn → do
                        dnS ← VUM.read surf dn
                        -- dn not a river centre (lake/ocean terminus):
                        -- the chain ends here at its own terrain. The
                        -- strict-descent property guarantees a centre
                        -- dn is already finalised.
                        pure $ if dnS ≡ minBound
                               then t
                               else min t (dnS + waterfallQuantum)
                VUM.write surf i s
        pure surf

-- | Lateral waterfall clamp: relax the surface field until EVERY pair
--   of 4-adjacent river tiles differs by at most 'waterfallQuantum'.
--   The chain clamp only constrains flow edges; this pass also covers
--   meander necks, parallel reaches, wing-tile adjacencies and
--   breakthrough paths. Worklist relaxation: lowering a tile can only
--   force its neighbours lower, surfaces are bounded below, so the
--   fixpoint (the q-Lipschitz lower envelope of the input surfaces
--   over the river adjacency graph) is reached in finite steps —
--   in practice one or two visits per affected tile.
clampLateralSurfaces
    ∷ Int                  -- ^ worldTiles
    → VU.Vector Bool       -- ^ isRiverTile (post-breakthrough)
    → VU.Vector Int        -- ^ surface z (post-breakthrough)
    → VU.Vector Int
clampLateralSurfaces worldTiles isRiverTile surfZ0 =
    let nTiles = VU.length surfZ0
        seeds  = [ i | i ← [0 .. nTiles - 1], isRiverTile VU.! i ]
    in VU.create $ do
        surf ← VU.thaw surfZ0
        queueRef ← newSTRef seeds
        let push i = modifySTRef' queueRef (i:)
            relaxNeighbour sI d i =
                case stepDir worldTiles i d of
                    Nothing → pure ()
                    Just n → when (isRiverTile VU.! n) $ do
                        sN ← VUM.read surf n
                        when (sN > sI + waterfallQuantum) $ do
                            VUM.write surf n (sI + waterfallQuantum)
                            push n
            loop = do
                q ← readSTRef queueRef
                case q of
                    [] → pure ()
                    (i:rest) → do
                        writeSTRef queueRef rest
                        sI ← VUM.read surf i
                        relaxNeighbour sI dirNorth i
                        relaxNeighbour sI dirEast  i
                        relaxNeighbour sI dirSouth i
                        relaxNeighbour sI dirWest  i
                        loop
        loop
        pure surf

-- | Per-tile width radius (0..maxWidthRadius), expanded perpendicular
--   to flow direction. Returns the widened river-tile mask, per-tile
--   width radius (-1 for non-river), and per-tile water surface z.
--
--   The surface z at a widened tile is inherited from the centre that
--   widened it. If multiple centres' wings overlap on a tile, the
--   minimum (= most-downstream) surface wins so the water plane
--   stays flat across the cross-section.
expandWidth
    ∷ Int                  -- ^ worldTiles
    → VU.Vector Int        -- ^ terrain
    → VU.Vector Word8      -- ^ dir
    → VU.Vector Int        -- ^ flow
    → VU.Vector Bool       -- ^ isRiverCentre (after extension)
    → VU.Vector Int        -- ^ waterfall-clamped per-centre surface z
    → (VU.Vector Bool, VU.Vector Int, VU.Vector Int)
expandWidth worldTiles terrain dir flow isRiverCentre centreSurf =
    let nTiles = worldTiles * worldTiles
        -- Don't widen into terrain that rises too far above the river's
        -- water surface. Caps the carve depth a wing tile can demand;
        -- without this, a river running past a cliff base would try to
        -- widen INTO the cliff and demand a 100+ z carve to keep the
        -- water plane flush. 'depthFromRadius r + bankSlack' gives the
        -- max pre-carve elevation a wing tile can have above the
        -- centre's surface.
        bankSlack = 3
    in runST $ do
        isR    ← VUM.replicate nTiles False
        width  ← VUM.replicate nTiles (-1   ∷ Int)
        surfZ  ← VUM.replicate nTiles minBound
        let updateTile tIdx r s = do
                VUM.write isR tIdx True
                curR ← VUM.read width tIdx
                when (r > curR) (VUM.write width tIdx r)
                curS ← VUM.read surfZ tIdx
                when (curS ≡ minBound ∨ s < curS) (VUM.write surfZ tIdx s)
            walkWing centreIdx r perpD = do
                centreS ← VUM.read surfZ centreIdx
                let maxBank = centreS + depthFromRadius r + bankSlack
                    -- Also cap downhill — a wing tile far below the
                    -- centre's surface would create a many-tile water
                    -- column trying to fill the natural depression
                    -- (e.g., a river running near a deep valley).
                    -- Limit to a modest drop so rivers don't pool
                    -- into adjacent low ground.
                    minBank = centreS - 6
                    loop k cur
                        | k > r = pure ()
                        | otherwise =
                            case stepDir worldTiles cur perpD of
                                Nothing → pure ()
                                Just nxt → do
                                    let nT = terrain VU.! nxt
                                    when (nT ≠ minBound
                                          ∧ nT ≤ maxBank
                                          ∧ nT ≥ minBank) $ do
                                        updateTile nxt r centreS
                                        loop (k + 1) nxt
                loop 1 centreIdx
        forM_ [0 .. nTiles - 1] $ \i → when (isRiverCentre VU.! i) $ do
            let surf = centreSurf VU.! i
                r    = widthRadiusFromFlow (flow VU.! i)
            updateTile i r surf
            when (r > 0) $
                forM_ (perpDirs (dir VU.! i)) (walkWing i r)
        isRf   ← VU.unsafeFreeze isR
        widthF ← VU.unsafeFreeze width
        surfZf ← VU.unsafeFreeze surfZ
        pure (isRf, widthF, surfZf)

-- | Carve depth in z for a given width radius. v2 user-approved:
--   narrow rivers (radius 0/1) carve 1 z; wider rivers carve deeper
--   so high-flow rivers cut visible canyons.
depthFromRadius ∷ Int → Int
depthFromRadius r
    | r ≥ 3     = 3
    | r ≥ 2     = 2
    | otherwise = 1

-- | Tile-radius within which an inland river-mouth will try to
--   breakthrough-carve a path to the nearest ocean. Inland rivers
--   farther from any coast stay endorheic.
breakthroughRange ∷ Int
breakthroughRange = 40

-- | Maximum cumulative carve cost (sum of uphill z plus 1 per step)
--   the breakthrough Dijkstra will accept. Beyond this the breakthrough
--   fails and the river pools at its inland terminus.
breakthroughMaxCarve ∷ Int
breakthroughMaxCarve = 50

-- | Cardinal directions perpendicular to a given D4 direction. For
--   sinks ('dirNone'), expand in all four cardinal directions so a
--   ponding terminus still picks up width.
perpDirs ∷ Word8 → [Word8]
perpDirs d
    | d ≡ dirNorth ∨ d ≡ dirSouth = [dirEast,  dirWest]
    | d ≡ dirEast  ∨ d ≡ dirWest  = [dirNorth, dirSouth]
    | otherwise                   = [dirNorth, dirSouth, dirEast, dirWest]

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

-- * Coastal breakthrough

-- | Walk every river tile with @dir = dirNone@ (a true sink, no
--   downhill neighbour) that's above sea level. These are the stranded
--   inland mouths the breakthrough pass tries to extend to ocean.
findStrandedMouths
    ∷ Int                  -- ^ nTiles
    → VU.Vector Bool       -- ^ isRiverTile
    → VU.Vector Word8      -- ^ dir
    → VU.Vector Int        -- ^ terrain
    → [Int]
findStrandedMouths nTiles isRiverTile dir terrain =
    [ i | i ← [0 .. nTiles - 1]
        , isRiverTile VU.! i
        , dir VU.! i ≡ dirNone
        , terrain VU.! i > seaLevel
        ]

-- | Dijkstra from a stranded mouth to the nearest tile in 'worldOcean'.
--   Edge cost is @max 0 (next_z − current_z) + 1@ — uphill is the bulk
--   of the cost, plus a small per-step term so flat extensions don't
--   meander.  Stops when total cost exceeds 'breakthroughMaxCarve' or
--   when distance from start exceeds 'breakthroughRange'.
--
--   Returns @Just (path, totalCost)@ on success — path is from start
--   to ocean tile inclusive.
dijkstraBreakthrough
    ∷ Int                  -- ^ worldTiles
    → Int                  -- ^ start tile idx
    → VU.Vector Int        -- ^ terrain
    → VU.Vector Bool       -- ^ worldOcean
    → Maybe ([Int], Int)
dijkstraBreakthrough worldTiles startIdx terrain worldOcean = runST $ do
    let nTiles = worldTiles * worldTiles
    bestCost ← VUM.replicate nTiles (maxBound ∷ Int)
    parent   ← VUM.replicate nTiles (-1       ∷ Int)
    dist     ← VUM.replicate nTiles (maxBound ∷ Int)
    VUM.write bestCost startIdx 0
    VUM.write dist     startIdx 0
    foundRef ← newSTRef Nothing
    let neighbours i =
            let bx = i `mod` worldTiles
                by = i `div` worldTiles
                east = if bx < worldTiles - 1
                       then i + 1
                       else i + 1 - worldTiles
                west = if bx > 0
                       then i - 1
                       else i - 1 + worldTiles
                north = if by > 0
                        then Just (i - worldTiles) else Nothing
                south = if by < worldTiles - 1
                        then Just (i + worldTiles) else Nothing
            in [Just east, Just west, north, south]
        loop pq
            | IM.null pq = pure ()
            | otherwise = do
                let ((c, (i, rest)), pqAfter) = case IM.findMin pq of
                        (ck, vs) → case vs of
                            (v:rs) → ((ck, (v, rs)), IM.delete ck pq)
                            []     → ((ck, (-1, [])), IM.delete ck pq)
                    pq1 = if null rest
                          then pqAfter
                          else IM.insert c rest pqAfter
                if i < 0 then loop pq1
                else do
                  bc ← VUM.read bestCost i
                  if c > bc
                    then loop pq1
                    else do
                      done ← readSTRef foundRef
                      case done of
                        Just _ → pure ()
                        Nothing →
                          if worldOcean VU.! i
                            then writeSTRef foundRef (Just (i, c))
                            else do
                              d ← VUM.read dist i
                              if d ≥ breakthroughRange
                                then loop pq1
                                else do
                                  pq2 ← expandNeighbours i d c pq1
                                  loop pq2
        expandNeighbours i d c pq =
            foldM (tryStep i d c) pq (neighbours i)
        tryStep i d c pq mn =
            case mn of
                Nothing → pure pq
                Just nIdx → do
                    let nT = terrain VU.! nIdx
                    if nT ≡ minBound
                       then pure pq
                       else do
                         let edgeCost = max 0 (nT - terrain VU.! i) + 1
                             newCost  = c + edgeCost
                         if newCost > breakthroughMaxCarve
                            then pure pq
                            else do
                              bcN ← VUM.read bestCost nIdx
                              if newCost < bcN
                                then do
                                    VUM.write bestCost nIdx newCost
                                    VUM.write parent   nIdx i
                                    VUM.write dist     nIdx (d + 1)
                                    pure (IM.insertWith (++) newCost
                                              [nIdx] pq)
                                else pure pq
    loop (IM.singleton 0 [startIdx])
    fr ← readSTRef foundRef
    case fr of
        Nothing → pure Nothing
        Just (endIdx, c) → do
            let rebuild cur acc = do
                    p ← VUM.read parent cur
                    if p < 0 then pure (cur : acc)
                    else rebuild p (cur : acc)
            path ← rebuild endIdx []
            pure (Just (path, c))

-- | For each stranded inland mouth, run the breakthrough Dijkstra and
--   if it returns a path within the cost cap, mark every tile on the
--   path as river with width 0 and a monotonically descending surface
--   z (last step ends at @seaLevel + 1@).  Updates the compId for
--   path tiles so they get attached to the originating river in the
--   per-chunk index.
addBreakthroughs
    ∷ Int                  -- ^ worldTiles
    → VU.Vector Bool       -- ^ isRiverTile (post-cull)
    → VU.Vector Int        -- ^ compId
    → VU.Vector Word8      -- ^ dir
    → VU.Vector Int        -- ^ terrain
    → VU.Vector Bool       -- ^ worldOcean
    → VU.Vector Int        -- ^ widthRadius
    → VU.Vector Int        -- ^ surfZ
    → (VU.Vector Bool, VU.Vector Int, VU.Vector Int, VU.Vector Int)
addBreakthroughs worldTiles isRiverTile compId dir terrain worldOcean
                 widthRadius surfZ =
    let nTiles = worldTiles * worldTiles
        mouths = findStrandedMouths nTiles isRiverTile dir terrain
    in runST $ do
        isRM   ← VU.thaw isRiverTile
        compM  ← VU.thaw compId
        widthM ← VU.thaw widthRadius
        surfM  ← VU.thaw surfZ
        forM_ mouths $ \m → do
            let mSurf = surfZ VU.! m
                mCid  = compId VU.! m
            when (mCid ≥ 0) $
                case dijkstraBreakthrough worldTiles m terrain worldOcean of
                    Nothing       → pure ()
                    Just (path, _) → do
                        -- Walk the path past the start (path[0] is the
                        -- mouth, already a river tile). Surface tracks
                        -- the natural terrain so a descending stretch
                        -- doesn't leave a many-z water column behind;
                        -- uphill stretches still descend at least 1 z
                        -- per step so the river never flows uphill.
                        -- Floor at @seaLevel + 1@ so the river hands
                        -- off cleanly to ocean at the path's end.
                        let walk prevSurf [] = pure ()
                            walk prevSurf (p : rest) = do
                                let preCarve = terrain VU.! p
                                    target =
                                        max (seaLevel + 1)
                                            (min (prevSurf - 1) preCarve)
                                VUM.write isRM   p True
                                curW ← VUM.read widthM p
                                when (curW < 0) (VUM.write widthM p 0)
                                VUM.write compM  p mCid
                                VUM.write surfM  p target
                                walk target rest
                        walk mSurf (drop 1 path)
        isRf   ← VU.unsafeFreeze isRM
        compF  ← VU.unsafeFreeze compM
        widthF ← VU.unsafeFreeze widthM
        surfF  ← VU.unsafeFreeze surfM
        pure (isRf, compF, widthF, surfF)

-- | Per-tile carve delta in z. Water surface is the centre's pre-carve
--   elevation; post-carve terrain at a river tile is
--   @min(preCarve, surface − depthFromRadius)@. The delta lowers the
--   terrain enough to ensure water visible at every river tile and
--   keep the cross-section flat under the same water plane.
--
--   For tiles whose pre-carve terrain is already below the channel
--   floor (downhill banks), delta = 0; the natural depression fills
--   with water.
computeCarveDelta
    ∷ Int                  -- ^ worldTiles
    → VU.Vector Int        -- ^ terrain
    → VU.Vector Bool       -- ^ isRiverTile
    → VU.Vector Int        -- ^ widthRadius
    → VU.Vector Int        -- ^ surfaceZ
    → VU.Vector Int
computeCarveDelta worldTiles terrain isRiverTile widthRadius surfZ =
    let nTiles = worldTiles * worldTiles
    in VU.generate nTiles $ \i →
        if not (isRiverTile VU.! i)
        then 0
        else
          let preCarve = terrain VU.! i
              surf     = surfZ   VU.! i
              r        = max 0 (widthRadius VU.! i)
              depth    = depthFromRadius r
          in max 0 (preCarve - surf + depth)

-- | Bucket the per-tile carve delta into per-chunk vectors so chunk
--   gen can look up its slice in O(1). Only chunks that touch a river
--   appear in the output map.
buildCarveDeltaIndex
    ∷ Int                  -- ^ worldSize
    → Int                  -- ^ half
    → VU.Vector Bool       -- ^ isRiverTile
    → VU.Vector Int        -- ^ per-tile carve delta
    → HM.HashMap ChunkCoord (VU.Vector Int)
buildCarveDeltaIndex worldSize half isRiverTile carveDelta =
    let worldTiles = worldSize * chunkSize
        nTiles     = worldTiles * worldTiles
        chunkArea  = chunkSize * chunkSize
        -- Pass 1: bucket non-zero deltas by chunk.
        step acc i
            | not (isRiverTile VU.! i) = acc
            | otherwise =
                let d = carveDelta VU.! i
                in if d ≤ 0 then acc
                   else
                     let gx = (i `mod` worldTiles) - half
                         gy = (i `div` worldTiles) - half
                         cx = gx `div` chunkSize
                         cy = gy `div` chunkSize
                         lx = ((gx `mod` chunkSize) + chunkSize) `mod` chunkSize
                         ly = ((gy `mod` chunkSize) + chunkSize) `mod` chunkSize
                         li = ly * chunkSize + lx
                     in HM.insertWith (++) (ChunkCoord cx cy)
                                      [(li, d)] acc
        accum ∷ HM.HashMap ChunkCoord [(Int, Int)]
        accum = foldl' step HM.empty [0 .. nTiles - 1]
        -- Pass 2: freeze each chunk's tile list into a chunkArea-long
        -- delta vector (zero for tiles without carving).
        freezeChunk tiles = VU.create $ do
            v ← VUM.replicate chunkArea (0 ∷ Int)
            forM_ tiles $ \(li, d) → VUM.write v li d
            pure v
    in HM.map freezeChunk accum

-- | Build the per-chunk index: one 'RiverChunkEntry' per (chunk, river)
--   pair that overlaps. Both the bitmask and the per-tile surface z are
--   chunkArea-long.
buildRiverChunkIndex
    ∷ Int                  -- ^ worldSize
    → Int                  -- ^ half
    → VU.Vector Bool       -- ^ isRiverTile
    → VU.Vector Int        -- ^ compId
    → VU.Vector Int        -- ^ surfZ
    → VU.Vector Int        -- ^ widthRadius
    → HM.HashMap ChunkCoord (V.Vector RiverChunkEntry)
buildRiverChunkIndex worldSize half isRiverTile compId surfZ widthRadius =
    let worldTiles = worldSize * chunkSize
        nTiles     = worldTiles * worldTiles
        chunkArea  = chunkSize * chunkSize
        -- Pass 1: bucket tiles into (chunk, riverId) → [(localIdx, surfZ, width)].
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
                    w      = max 0 (widthRadius VU.! i)
                in HM.insertWith (++) (ChunkCoord cx cy, rid)
                                 [(li, z, w)] acc
        accumKey ∷ HM.HashMap (ChunkCoord, Int) [(Int, Int, Int)]
        accumKey = foldl' step HM.empty [0 .. nTiles - 1]
        -- Pass 2: per key, freeze into bitmask + per-tile surface + width.
        perKey ∷ HM.HashMap (ChunkCoord, Int) RiverChunkEntry
        perKey = HM.mapWithKey
            (\(_, rid) tiles →
                let (bm, surfs, widths) = runST $ do
                        b ← VUM.replicate chunkArea False
                        s ← VUM.replicate chunkArea minBound
                        w ← VUM.replicate chunkArea (0 ∷ Word8)
                        forM_ tiles $ \(li, z, wr) → do
                            VUM.write b li True
                            VUM.write s li z
                            VUM.write w li (fromIntegral wr)
                        bF ← VU.unsafeFreeze b
                        sF ← VU.unsafeFreeze s
                        wF ← VU.unsafeFreeze w
                        pure (bF, sF, wF)
                in RiverChunkEntry
                    { rceRiverId       = rid
                    , rceBitmask       = bm
                    , rcePerTileSurfZ  = surfs
                    , rceWidthRadius   = widths
                    })
            accumKey
        -- Pass 3: regroup by chunk.
        byChunk = HM.foldlWithKey'
            (\m (cc, _) e → HM.insertWith (++) cc [e] m)
            HM.empty perKey
    in HM.map V.fromList byChunk
