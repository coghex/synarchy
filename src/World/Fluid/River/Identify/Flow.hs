{-# LANGUAGE Strict #-}

-- | Per-tile flow accumulation for the river-identification pipeline:
--   climate → precip/evap units, the lake-id reverse index, per-lake
--   spillway identification and its one-to-many per-tile inverse, D4
--   steepest-descent directions, and the ascending-z bucket sort +
--   flow-accumulation walk that turns those into a per-tile flow
--   field. Everything
--   'World.Fluid.River.Identify.identifyWorldRivers' needs before it
--   hands off to the trace stage. See that module's header comment
--   for the full pipeline overview.
module World.Fluid.River.Identify.Flow
    ( computePrecipUnits
    , computeEvapUnits
    , buildLakeIdAt
    , computeSpillways
    , buildSpillwayOwners
    , computeDescentDirs
    , demoteBlockedSharedSpillways
    , resolveSpillways
    , bucketSortAscending
    , computeFlowAccumulation
    ) where

import UPrelude
import qualified Data.HashMap.Strict as HM
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as VU
import qualified Data.Vector.Unboxed.Mutable as VUM
import Control.Monad.ST (ST, runST)
import Data.STRef (newSTRef, readSTRef, writeSTRef)
import World.Chunk.Types (ChunkCoord(..), chunkSize)
import World.Constants (seaLevel)
import World.Fluid.Lake.Types
    ( WorldLakes(..), LakeChunkEntry(..), Lake(..) )
import World.Fluid.River.Identify.Common
    ( dirNorth, dirEast, dirSouth, dirWest, dirNone, stepDir, sortDescOn
    , SpillwayOwners(..), spillwayOwnerCount, isSpillwayOwner )
import World.Weather.Lookup (LocalClimate(..), lookupLocalClimate)
import World.Weather.Types (ClimateState)

-- * Climate → flow units
--
-- Convert per-tile climate samples to small integer flow units. v1
-- tuning notes are below 'World.Fluid.River.Identify.riverThreshold'.

-- | Precip contribution per tile, in flow units. lcPrecip is already
--   normalized 0..1; @* 20@ keeps integer arithmetic in a comfortable
--   range while still leaving headroom over the v1 threshold.
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
--   in 'World.Fluid.River.Identify.Components' — if evap drops local
--   flow below threshold, the chain continues to its terminus at
--   minimum width instead of fragmenting.
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
                                        when (nz ≢ minBound
                                              ∧ nLid ≢ lid) $ do
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

-- | Per-tile inverse of 'computeSpillways': the COMPLETE set of lakes
--   that spill through each tile, ascending by lake id (#2323).
--
--   Two adjacent basins can independently pick the same lowest
--   external neighbour, so this relation is one-to-many. The ascending
--   order is a property of the construction (the outer loop walks lake
--   ids upward and appends), not of any later sort, so no consumer can
--   observe a traversal-order-dependent owner. See 'SpillwayOwners'.
buildSpillwayOwners ∷ Int → VU.Vector Int → SpillwayOwners
buildSpillwayOwners nTiles spillwayOf = runST $ do
    let nLakes = VU.length spillwayOf
    counts ← VUM.replicate nTiles (0 ∷ Int)
    forM_ [0 .. nLakes - 1] $ \lid → do
        let s = spillwayOf VU.! lid
        when (s ≥ 0) (VUM.modify counts (+ 1) s)
    -- Prefix sum into per-tile group starts.
    offsets ← VUM.new (nTiles + 1)
    cursor  ← newSTRef (0 ∷ Int)
    forM_ [0 .. nTiles - 1] $ \t → do
        p ← readSTRef cursor
        VUM.write offsets t p
        c ← VUM.read counts t
        writeSTRef cursor (p + c)
    total ← readSTRef cursor
    VUM.write offsets nTiles total
    -- Fill, reusing 'counts' as the per-tile write cursor.
    forM_ [0 .. nTiles - 1] $ \t → do
        p ← VUM.read offsets t
        VUM.write counts t p
    out ← VUM.new total
    forM_ [0 .. nLakes - 1] $ \lid → do
        let s = spillwayOf VU.! lid
        when (s ≥ 0) $ do
            p ← VUM.read counts s
            VUM.write out p lid
            VUM.write counts s (p + 1)
    offsetsF ← VU.unsafeFreeze offsets
    outF     ← VU.unsafeFreeze out
    pure (SpillwayOwners offsetsF outF)

-- * D4 descent

-- | Per-tile D4 steepest descent direction. A spillway excludes
--   neighbours in EVERY basin that spills through it (#2323), so no
--   contributor's outflow is routed straight back into itself.
--   Beyond-glacier tiles, lake tiles, and open-ocean (sub-sea, no lake
--   assigned) tiles are tagged 'dirNone'.
computeDescentDirs
    ∷ Int                  -- ^ worldTiles
    → VU.Vector Int        -- ^ terrain
    → VU.Vector Int        -- ^ lakeIdAt
    → SpillwayOwners       -- ^ per-tile spillway contributors
    → VU.Vector Word8
computeDescentDirs worldTiles terrain lakeIdAt owners =
    let nTiles = worldTiles * worldTiles
    in VU.generate nTiles $ \idx →
        descentDirAt worldTiles terrain lakeIdAt
                     (isSpillwayOwner owners idx) idx

-- | One tile's D4 steepest descent under an arbitrary neighbour-lake
--   exclusion. @excluded lid@ rejects a neighbour belonging to lake
--   @lid@.
--
--   'computeDescentDirs' passes the tile's own contributor set;
--   'demoteBlockedSharedSpillways' passes @const False@ to ask what
--   the descent would have been with no exclusion at all, which is how
--   it tells "the exclusion removed the last candidate" apart from
--   "this tile had no descent either way".
{-# INLINE descentDirAt #-}
descentDirAt
    ∷ Int                  -- ^ worldTiles
    → VU.Vector Int        -- ^ terrain
    → VU.Vector Int        -- ^ lakeIdAt
    → (Int → Bool)         -- ^ neighbour lake ids to exclude
    → Int                  -- ^ tile index
    → Word8
descentDirAt worldTiles terrain lakeIdAt excluded idx =
    let t  = terrain  VU.! idx
        lk = lakeIdAt VU.! idx
    in if t ≡ minBound
          ∨ lk ≥ 0
          ∨ (t ≤ seaLevel ∧ lk < 0)
       then dirNone
       else
         let bx = idx `mod` worldTiles
             by = idx `div` worldTiles
             -- Candidate neighbours: include only if it would not
             -- feed flow back into a contributing source lake.
             candidate ok nIdx d
                 | not ok = Nothing
                 | otherwise =
                     let nz   = terrain  VU.! nIdx
                         nLid = lakeIdAt VU.! nIdx
                     in if nz ≡ minBound
                        then Nothing
                        else if nLid ≥ 0 ∧ excluded nLid
                             then Nothing  -- spillway: exclude a source lake
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

-- | The no-descent fallback for a SHARED spillway (#2323, req 3).
--
--   When excluding every contributing basin leaves a shared tile with
--   no descent candidate at all, keeping the tile as anyone's spillway
--   would inject each contributor's accumulated flow at a tile that
--   cannot pass it on: 'computeFlowAccumulation' adds the inject
--   BEFORE 'walkInject' ever observes the missing direction. So every
--   contributor of such a tile is demoted to spillway @-1@ — none of
--   their flow is added there, no walk starts there, and the tile
--   contributes no river-source metadata.
--
--   Two bounds make this a fix for the collision and nothing else. It
--   applies only where the CONTRIBUTOR EXCLUSION is what removed the
--   last candidate — a shared tile with no descent for an independent
--   reason (it belongs to a third lake, or has no lower non-void
--   neighbour at all) keeps its prior behaviour, because no
--   contributor is being routed back into itself there. And it applies
--   only to tiles with two or more contributors: a unique-owner
--   spillway with no valid descent still injects, and its walk simply
--   terminates, as before.
--
--   The demotion touches injection and source metadata ONLY: the
--   tile keeps the exclusion-derived 'dirNone' 'computeDescentDirs'
--   already gave it, and no second, exclusion-free descent pass can
--   re-admit a contributing basin for it.
demoteBlockedSharedSpillways
    ∷ Int                  -- ^ worldTiles
    → VU.Vector Int        -- ^ terrain
    → VU.Vector Int        -- ^ lakeIdAt
    → SpillwayOwners       -- ^ contributors, before demotion
    → VU.Vector Word8      -- ^ dir, as derived from those contributors
    → VU.Vector Int        -- ^ spillwayOf, before demotion
    → (VU.Vector Int, SpillwayOwners)
       -- ^ (effective spillwayOf, effective contributors)
demoteBlockedSharedSpillways worldTiles terrain lakeIdAt owners dir
                             spillwayOf =
    let nTiles = VU.length (spoOffsets owners) - 1
        blocked t = spillwayOwnerCount owners t ≥ 2
                  ∧ dir VU.! t ≡ dirNone
                  ∧ descentDirAt worldTiles terrain lakeIdAt
                                 (const False) t ≢ dirNone
        spillwayOf' = VU.map (\s → if s ≥ 0 ∧ blocked s then -1 else s)
                             spillwayOf
    in (spillwayOf', buildSpillwayOwners nTiles spillwayOf')

-- | The whole spillway stage, as 'World.Fluid.River.Identify' runs it:
--   pick each lake's outlet, invert that into per-tile contributor
--   sets, derive descents that exclude every contributor, then apply
--   the shared-spillway demotion above. Returns the EFFECTIVE spillway
--   table and contributor sets — the ones downstream stages must use —
--   alongside the descent field, which is derived from the
--   pre-demotion contributors and is not recomputed.
resolveSpillways
    ∷ Int                  -- ^ worldSize
    → WorldLakes
    → VU.Vector Int        -- ^ terrain
    → VU.Vector Int        -- ^ lakeIdAt
    → (VU.Vector Int, SpillwayOwners, VU.Vector Word8)
       -- ^ (effective spillwayOf, effective owners, descent dirs)
resolveSpillways worldSize lakes terrain lakeIdAt =
    let worldTiles  = worldSize * chunkSize
        nTiles      = worldTiles * worldTiles
        spillwayOf0 = computeSpillways worldSize lakes terrain lakeIdAt
        owners0     = buildSpillwayOwners nTiles spillwayOf0
        dir         = computeDescentDirs worldTiles terrain lakeIdAt owners0
        (spillwayOf, owners) =
            demoteBlockedSharedSpillways worldTiles terrain lakeIdAt
                                         owners0 dir spillwayOf0
    in (spillwayOf, owners, dir)

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
            when (z ≢ minBound) $
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
            when (z ≢ minBound) $ do
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
            when (t ≢ minBound) $ do
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
            spillDesc = sortDescOn (\(z,_,_) → z) spillways
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
                           when (terrain VU.! dn ≢ minBound) (go dn)
    in go start
