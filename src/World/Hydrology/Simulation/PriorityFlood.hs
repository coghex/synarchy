{-# LANGUAGE Strict, UnicodeSyntax #-}
module World.Hydrology.Simulation.PriorityFlood
    ( fillDepressions
    ) where

import UPrelude
import qualified Data.Vector.Unboxed as VU
import qualified Data.Vector.Unboxed.Mutable as VUM
import Control.Monad.ST (runST)
import Data.STRef (newSTRef, readSTRef, modifySTRef')
import World.Geology.Hash (hashGeo)
import World.Hydrology.Simulation.Types (ElevGrid(..))

-- * Depression Filling (Priority-Flood with mutable binary heap)

-- | Priority-flood using a mutable binary min-heap.
--   Entries are keyed by (elevation, tieBreaker) and carry a cell
--   index. This is O(n log n) and guaranteed correct in a single pass.
--
--   Returns BOTH the filled elevation surface AND a flow-direction
--   vector. The flow direction is the drainage parent recorded
--   during the flood: when cell A is expanded to neighbor B, B
--   drains through A. This correctly handles flat areas (filled
--   depressions) where steepest-descent would fail.
fillDepressions ∷ ElevGrid → (VU.Vector Int, VU.Vector Int)
fillDepressions grid =
    let gridW = egGridW grid
        totalSamples = gridW * gridW
        elevVec = egElev grid
        landVec = egLand grid
        fromIdx idx = (idx `mod` gridW, idx `div` gridW)
        toIdx ix iy = iy * gridW + ix
        wrapIX ix = ((ix `mod` gridW) + gridW) `mod` gridW
        neighbors ∷ Int → [Int]
        neighbors idx =
            let (ix, iy) = fromIdx idx
            in [ toIdx (wrapIX (ix + dx)) ny
               | (dx, dy) ← [(-1,0),(1,0),(0,-1),(0,1)
                             ,(-1,-1),(1,-1),(-1,1),(1,1)]
               , let ny = iy + dy
               , ny ≥ 0 ∧ ny < gridW
               ]

    in runST $ do
        filled ← VUM.replicate totalSamples (maxBound ∷ Int)
        visited ← VUM.replicate totalSamples False
        -- Drainage parent from priority-flood: used as fallback for
        -- flat areas where steepest-descent can't resolve direction.
        floodParent ← VUM.replicate totalSamples (-1 ∷ Int)

        -- Seed: all non-land cells (ocean, glacier) plus Y-boundary cells.
        -- X wraps, so no X edges needed.
        -- Y-boundary cells (iy=0, iy=gridW-1) must be seeded because they
        -- may land exactly at the glacier edge and still be classified as
        -- land. Without seeding them, nearby rivers have no drainage path.
        let seeds = filter (\idx →
                let (_, iy) = fromIdx idx
                in not (landVec VU.! idx)
                 ∨ iy ≡ 0
                 ∨ iy ≡ gridW - 1
                ) [0 .. totalSamples - 1]

        -- Initialize seeds (sinks — floodParent stays −1)
        forM_ seeds $ \idx → do
            VUM.write filled idx (elevVec VU.! idx)
            VUM.write visited idx True

        -- Hash-based tie-breaking: when two cells have the same
        -- elevation, use a deterministic hash instead of index order.
        -- Without this, lower indices are processed first, creating
        -- a systematic spatial bias in flood-parent directions.
        let hashPri idx = hashGeo 7046029254386353131 idx 0

        -- Mutable binary min-heap (keyed by elevation, then hash tiebreak)
        -- Three parallel arrays: keys1 (elev), keys2 (hash), values (idx)
        heapK1 ← VUM.new totalSamples    -- elevation key
        heapK2 ← VUM.new totalSamples    -- hash tiebreak key
        heapVal ← VUM.new totalSamples   -- cell index
        heapSize ← VUM.new 1
        VUM.write heapSize 0 (0 ∷ Int)

        -- Heap operations
        let siftUp i = do
                when (i > 0) $ do
                    let parent = (i - 1) `div` 2
                    ik1 ← VUM.read heapK1 i
                    pk1 ← VUM.read heapK1 parent
                    ik2 ← VUM.read heapK2 i
                    pk2 ← VUM.read heapK2 parent
                    when (ik1 < pk1 ∨ (ik1 ≡ pk1 ∧ ik2 < pk2)) $ do
                        -- Swap entries
                        iv ← VUM.read heapVal i
                        pv ← VUM.read heapVal parent
                        VUM.write heapK1 i pk1
                        VUM.write heapK2 i pk2
                        VUM.write heapVal i pv
                        VUM.write heapK1 parent ik1
                        VUM.write heapK2 parent ik2
                        VUM.write heapVal parent iv
                        siftUp parent

            siftDown n i = do
                let left = 2 * i + 1
                    right = 2 * i + 2
                smallest0 ← pure i
                smallest1 ← if left < n
                    then do
                        lk1 ← VUM.read heapK1 left
                        sk1 ← VUM.read heapK1 smallest0
                        lk2 ← VUM.read heapK2 left
                        sk2 ← VUM.read heapK2 smallest0
                        pure (if lk1 < sk1 ∨ (lk1 ≡ sk1 ∧ lk2 < sk2)
                              then left else smallest0)
                    else pure smallest0
                smallest2 ← if right < n
                    then do
                        rk1 ← VUM.read heapK1 right
                        sk1 ← VUM.read heapK1 smallest1
                        rk2 ← VUM.read heapK2 right
                        sk2 ← VUM.read heapK2 smallest1
                        pure (if rk1 < sk1 ∨ (rk1 ≡ sk1 ∧ rk2 < sk2)
                              then right else smallest1)
                    else pure smallest1
                when (smallest2 ≠ i) $ do
                    -- Swap
                    ik1 ← VUM.read heapK1 i
                    ik2 ← VUM.read heapK2 i
                    iv  ← VUM.read heapVal i
                    sk1 ← VUM.read heapK1 smallest2
                    sk2 ← VUM.read heapK2 smallest2
                    sv  ← VUM.read heapVal smallest2
                    VUM.write heapK1 i sk1
                    VUM.write heapK2 i sk2
                    VUM.write heapVal i sv
                    VUM.write heapK1 smallest2 ik1
                    VUM.write heapK2 smallest2 ik2
                    VUM.write heapVal smallest2 iv
                    siftDown n smallest2

            heapPush elev hp idx = do
                n ← VUM.read heapSize 0
                VUM.write heapK1 n elev
                VUM.write heapK2 n hp
                VUM.write heapVal n idx
                VUM.write heapSize 0 (n + 1)
                siftUp n

            heapPop = do
                n ← VUM.read heapSize 0
                topK1 ← VUM.read heapK1 0
                topV  ← VUM.read heapVal 0
                let n' = n - 1
                VUM.write heapSize 0 n'
                when (n' > 0) $ do
                    lastK1 ← VUM.read heapK1 n'
                    lastK2 ← VUM.read heapK2 n'
                    lastV  ← VUM.read heapVal n'
                    VUM.write heapK1 0 lastK1
                    VUM.write heapK2 0 lastK2
                    VUM.write heapVal 0 lastV
                    siftDown n' 0
                pure (topK1, topV)

        -- Build initial heap from seeds
        forM_ seeds $ \idx →
            heapPush (elevVec VU.! idx) (hashPri idx) idx

        -- Priority-flood: compute filled elevation surface.
        -- Also records flood parents for flat-area fallback.
        let go = do
                n ← VUM.read heapSize 0
                when (n > 0) $ do
                    (curElev, curIdx) ← heapPop
                    let nbrs = neighbors curIdx
                    forM_ nbrs $ \nIdx → do
                        nVis ← VUM.read visited nIdx
                        when (not nVis) $ do
                            VUM.write visited nIdx True
                            let nElev = elevVec VU.! nIdx
                                nFill = max nElev curElev
                            VUM.write filled nIdx nFill
                            VUM.write floodParent nIdx curIdx
                            heapPush nFill (hashPri nIdx) nIdx
                    go

        go

        -- Safety: any unvisited cell gets its own elevation
        forM_ [0 .. totalSamples - 1] $ \idx → do
            vis ← VUM.read visited idx
            when (not vis) $
                VUM.write filled idx (elevVec VU.! idx)

        filledV ← VU.unsafeFreeze filled
        floodParentV ← VU.unsafeFreeze floodParent

        -- Step 2: Compute D8 steepest-descent flow directions on the
        -- FILLED surface. Each cell drains to the neighbor with the
        -- lowest filled elevation. For flat areas (filled depressions
        -- where all neighbors have equal elevation), fall back to the
        -- priority-flood parent which correctly resolves drainage
        -- through filled basins.
        --
        -- This eliminates the directional bias of the priority-flood
        -- parent alone, which was determined by seed processing order
        -- rather than actual terrain gradients.
        let d8FlowDir = VU.generate totalSamples $ \idx →
                if not (landVec VU.! idx)
                then -1  -- ocean/glacier → sink
                else
                    let myFill = filledV VU.! idx
                        nbrs = neighbors idx
                        -- Find neighbor with lowest filled elevation
                        findLowest [] bestIdx _bestE = bestIdx
                        findLowest (n:ns) bestIdx bestE =
                            let nE = filledV VU.! n
                            in if nE < bestE
                               then findLowest ns n nE
                               else findLowest ns bestIdx bestE
                    in case nbrs of
                        [] → -1
                        (n0:ns) →
                            let bestIdx = findLowest ns n0 (filledV VU.! n0)
                                bestE = filledV VU.! bestIdx
                            in if bestE < myFill
                               then bestIdx  -- steepest descent
                               else floodParentV VU.! idx  -- flat: use flood parent

        -- Step 3: Ocean-proximity override. For land cells near the
        -- coast, steer flow toward the nearest ocean cell to prevent
        -- rivers from running parallel to the coastline. Only
        -- overrides cells within forceRadius grid cells of ocean,
        -- and only if the current flow direction moves AWAY from
        -- the nearest ocean (dot product check).
        let forceRadius = 3 ∷ Int
            oceanDist = computeOceanDistance totalSamples gridW landVec forceRadius
            d8Biased = VU.generate totalSamples $ \idx →
                let natural = d8FlowDir VU.! idx
                in if natural < 0 ∨ not (landVec VU.! idx)
                   then natural
                   else let dist = oceanDist VU.! idx
                        in if dist > forceRadius ∨ dist ≡ 0
                           then natural  -- too far or already at coast
                           else
                            -- Find the neighbor closest to ocean
                            let nbrs = neighbors idx
                                oceanNbrs = [ n
                                    | n ← nbrs
                                    , not (landVec VU.! n)
                                    ]
                            in case oceanNbrs of
                                -- Direct ocean neighbor: force drain into it
                                (oc:_) → oc
                                -- No direct ocean neighbor but within radius:
                                -- pick the neighbor with shortest ocean distance
                                [] →
                                    let scored = [ (n, oceanDist VU.! n)
                                                 | n ← nbrs
                                                 , landVec VU.! n
                                                 , oceanDist VU.! n < dist
                                                 ]
                                    in case scored of
                                        [] → natural
                                        s0 : rest →
                                            -- Pick the one with shortest ocean dist
                                            let (bestN, _) = foldl'
                                                    (\(bn, bd) (n, d) →
                                                        if d < bd then (n, d) else (bn, bd))
                                                    s0 rest
                                            in bestN

        return (filledV, d8Biased)

-- | Compute BFS distance from each land cell to nearest ocean cell.
--   Returns maxBound for cells beyond forceRadius.
computeOceanDistance ∷ Int → Int → VU.Vector Bool → Int → VU.Vector Int
computeOceanDistance totalSamples gridW landVec maxDist = runST $ do
    distM ← VUM.replicate totalSamples (maxBound ∷ Int)
    let wrapIX ix = ((ix `mod` gridW) + gridW) `mod` gridW
        fromIdx idx = (idx `mod` gridW, idx `div` gridW)
        toIdx ix iy = iy * gridW + ix
        neighbors idx =
            let (ix, iy) = fromIdx idx
            in [ toIdx (wrapIX (ix + dx)) ny
               | (dx, dy) ← [(-1,0),(1,0),(0,-1),(0,1)
                             ,(-1,-1),(1,-1),(-1,1),(1,1)]
               , let ny = iy + dy
               , ny ≥ 0 ∧ ny < gridW
               ]

    -- Seed layer 0: every ocean cell at distance 0.
    let seeds = [ idx | idx ← [0 .. totalSamples - 1]
                      , not (landVec VU.! idx) ]
    forM_ seeds $ \idx → VUM.write distM idx 0

    -- Layered BFS: expand the current frontier into the next layer,
    -- assigning distance d to every newly-reached tile and capturing
    -- them as the next frontier. Each tile is visited at most once
    -- (the `nDist ≡ maxBound` guard fires only on first assignment),
    -- giving O(N) total work — vs. the prior O(maxDist × N) scan that
    -- re-read every tile per layer. Stops at maxDist or when the
    -- frontier empties, whichever comes first.
    let go d current
          | d > maxDist ∨ null current = return ()
          | otherwise = do
              nextRef ← newSTRef []
              forM_ current $ \idx →
                  forM_ (neighbors idx) $ \nIdx → do
                      nDist ← VUM.read distM nIdx
                      when (nDist ≡ maxBound) $ do
                          VUM.write distM nIdx d
                          modifySTRef' nextRef (nIdx :)
              nextLayer ← readSTRef nextRef
              go (d + 1) nextLayer
    go 1 seeds

    VU.unsafeFreeze distM
