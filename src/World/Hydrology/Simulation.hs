{-# LANGUAGE Strict, UnicodeSyntax #-}
module World.Hydrology.Simulation
    ( simulateHydrology
    , FlowResult(..)
    , ElevGrid(..)
    , buildInitialElevGrid
    , updateElevGrid
    , fillDepressions
    ) where

import UPrelude
import Data.List (sortBy)
import Data.Ord (comparing, Down(..))
import qualified Data.Vector.Unboxed as VU
import qualified Data.Vector.Unboxed.Mutable as VUM
import qualified Data.Vector.Algorithms.Intro as VA
import Control.Monad.ST (runST)
import Data.STRef (newSTRef, readSTRef, modifySTRef')
import World.Types
import World.Plate (elevationAtGlobal, isBeyondGlacier, wrapGlobalU, worldWidthTiles)
import World.Geology.Hash (hashGeo, hashToFloatGeo, wrappedDeltaUV)
import qualified Data.HashMap.Strict as HM
import World.Weather.Types (ClimateState(..), ClimateGrid(..)
                           , RegionClimate(..), SeasonalClimate(..))
import World.Weather.Lookup (lookupLocalClimate, LocalClimate(..))

-- * Configuration

baseSampleSpacing ∷ Int
baseSampleSpacing = 4

-- | Drainage-area target (cells of average-wetness) to qualify as a
--   river. The actual flow threshold is derived per-world from
--   climate (see `effRiverThreshold`) so river density per area
--   stays consistent across arid / balanced / wet climates rather
--   than scaling with per-cell water input (audit #15).
--
--   NB (issue #221): do NOT lower this to add inland rivers. Lowering
--   it admits many *small-catchment* (short) headwaters which, against
--   the fixed per-age river budget (`maxTotalRivers`), displace the
--   large-catchment rivers we actually want long — empirically it cut
--   median river length. Inland origins come from `walkToDivide`
--   rooting the selected (largest-catchment) rivers at their divides,
--   not from a lower qualification bar.
minRiverDrainageCells ∷ Int
minRiverDrainageCells = 16

-- | World-specific river threshold scaled by climate. Uses the same
--   per-cell water formula as `accumVec`: avg-cell water units ≈
--   avgPrecip * 10 (ignoring snowmelt for the average). For a
--   balanced world (avgPrecip ≈ 0.5) this gives 80 — the constant
--   the old `minRiverTotalFlow` was calibrated for. Floor at 16
--   matches the per-cell `max 1` floor — even extreme deserts can't
--   form rivers from <16 cells of drainage.
effRiverThreshold ∷ ClimateState → Int
effRiverThreshold climate =
    let regions = cgRegions (csClimate climate)
        avgPrecip = if HM.null regions
                    then 0.5
                    else HM.foldl' (\acc rc →
                              let SeasonalClimate s w = rcPrecipitation rc
                              in acc + (s + w) / 2.0
                              ) 0.0 regions
                         / fromIntegral (HM.size regions)
    in max minRiverDrainageCells
         $ round (fromIntegral minRiverDrainageCells * avgPrecip * 10.0 ∷ Float)

maxGridDim ∷ Int
maxGridDim = 384

minLakeDepth ∷ Int
minLakeDepth = 9

-- | Smallest worldSize for which river sources are extended upstream to
--   their catchment divide (issue #221, see `walkToDivide`). Worlds
--   below this are too small to host long rivers and pack volcanism
--   densely enough that the extension breaches calderas; they keep the
--   original (coastal) source behaviour. 128 is the smallest real
--   playable world; 32/64 are regression-gate sizes only.
minExtendWorld ∷ Int
minExtendWorld = 128

-- * Types

-- | Result of flow simulation. Instead of full RiverParams,
--   we now export river SOURCES — high-flow land cells that
--   should be traced at tile resolution.
data FlowResult = FlowResult
    { frRiverSources ∷ ![(Int, Int, Int, Float)]
      -- ^ (gx, gy, elevation, flowStrength) — sorted by flow descending
    , frLakes        ∷ ![LakeParams]
    , frFilledElev   ∷ !(VU.Vector Int)
      -- ^ Depression-filled elevation surface (same indexing as ElevGrid)
    , frFlowDir      ∷ !(VU.Vector Int)
      -- ^ Flow direction: index of downstream neighbor, −1 for ocean/sink.
      --   Following this chain is guaranteed to reach the ocean.
    } deriving (Show)

data ElevGrid = ElevGrid
    { egGridW   ∷ !Int
    , egSpacing ∷ !Int
    , egElev    ∷ !(VU.Vector Int)
    , egGX      ∷ !(VU.Vector Int)
    , egGY      ∷ !(VU.Vector Int)
    , egLand    ∷ !(VU.Vector Bool)
    } deriving (Show)

-- * Grid Construction

buildInitialElevGrid ∷ Word64 → Int → [TectonicPlate] → ElevGrid
buildInitialElevGrid seed worldSize plates =
    let totalTiles = worldSize * 16
        -- The grid lives in (u, v) space and wraps ix as a torus, so
        -- spacing must DIVIDE the world's u-period exactly: with the
        -- old floor-division spacing, gridW capped at maxGridDim left
        -- gridW·spacing < totalTiles — a never-sampled stripe at the
        -- seam (128 tiles wide at worldSize 128, 256 at 256) where no
        -- per-age rivers, lakes, or valley carving could originate,
        -- and a torus wrap stitching flow across a phantom
        -- discontinuity. Picking the smallest divisor of totalTiles
        -- ≥ ceil(totalTiles / maxGridDim) restores exact coverage
        -- (totalTiles = 16·worldSize, so divisors are dense). The
        -- [..totalTiles] bound makes the search total; spacing =
        -- totalTiles (gridW 1) is unreachable for any real world
        -- size. Regression test: Test.Headless.WorldGen.WrapSeam.
        minSpacing = max baseSampleSpacing
                         ((totalTiles + maxGridDim - 1) `div` maxGridDim)
        spacing = fromMaybe totalTiles
                      (listToMaybe [ s | s ← [minSpacing .. totalTiles]
                                       , totalTiles `mod` s ≡ 0 ])
        gridW = max 4 (totalTiles `div` spacing)
        halfGrid = gridW `div` 2
        totalSamples = gridW * gridW
        fromIdx idx = (idx `mod` gridW, idx `div` gridW)

        -- ix maps to u-axis, iy maps to v-axis
        -- u = (ix - halfGrid) * spacing
        -- v = (iy - halfGrid) * spacing
        -- gx = (u + v) / 2, gy = (v - u) / 2
        gxV = VU.generate totalSamples $ \idx →
            let (ix, iy) = fromIdx idx
                u = (ix - halfGrid) * spacing
                v = (iy - halfGrid) * spacing
            in (u + v) `div` 2

        gyV = VU.generate totalSamples $ \idx →
            let (ix, iy) = fromIdx idx
                u = (ix - halfGrid) * spacing
                v = (iy - halfGrid) * spacing
            in (v - u) `div` 2

        rawElevV = VU.generate totalSamples $ \idx →
            let gx = gxV VU.! idx
                gy = gyV VU.! idx
                (gx', gy') = wrapGlobalU worldSize gx gy
            in if isBeyondGlacier worldSize gx' gy'
               then seaLevel + 500
               else fst (elevationAtGlobal seed plates worldSize gx' gy')

        -- Add meander-inducing micro-noise to the elevation grid.
        -- On flat terrain, the D8 flow direction creates straight paths
        -- because the gradient is nearly uniform. Small coherent noise
        -- breaks the symmetry and forces flow to curve naturally,
        -- creating meandering drainage patterns. The noise amplitude
        -- scales with local flatness — steep terrain keeps its natural
        -- gradient, flat terrain gets noise to induce curvature.
        elevV = VU.imap (\idx rawE →
            if rawE ≤ seaLevel then rawE  -- don't perturb ocean/sub-sea
            else let gx = gxV VU.! idx
                     gy = gyV VU.! idx
                     -- Two octaves of coherent noise at different scales
                     n1 = meanderNoise seed worldSize gx gy 40 1300
                     n2 = meanderNoise seed worldSize gx gy 18 1301
                     noise = n1 * 0.6 + n2 * 0.4
                     -- Local slope: max elevation diff to any neighbor
                     (ix, iy) = (idx `mod` gridW, idx `div` gridW)
                     maxSlope = foldl' (\acc (dx, dy) →
                         let nx = ((ix + dx) `mod` gridW + gridW) `mod` gridW
                             ny = iy + dy
                         in if ny < 0 ∨ ny ≥ gridW then acc
                            else max acc (abs (rawE - rawElevV VU.! (ny * gridW + nx)))
                         ) 0 [(-1,0),(1,0),(0,-1),(0,1)]
                     -- Flat terrain (slope < 3) gets full noise;
                     -- steep terrain (slope > 10) gets none
                     flatness = clamp01 (1.0 - fromIntegral maxSlope / 10.0)
                     -- Amplitude: up to 1 elevation level on flat terrain
                     amplitude = 1.0 * flatness
                 in rawE + round (noise * amplitude)
            ) rawElevV

        landV = VU.generate totalSamples $ \idx →
            let gx = gxV VU.! idx
                gy = gyV VU.! idx
                (gx', gy') = wrapGlobalU worldSize gx gy
            in elevV VU.! idx > seaLevel
             ∧ not (isBeyondGlacier worldSize gx' gy')

    in ElevGrid gridW spacing elevV gxV gyV landV

-- | 2D coherent noise for meander induction. Returns [-1, 1].
--   Value noise: hash at integer lattice points, bilinear interp with
--   smoothstep. Operates in (u, v) space so the lattice can be made
--   periodic on the u-axis — the world is a cylinder along u, and the
--   raw (gx, gy) hash was discontinuous across that seam, producing
--   uncorrelated noise on physically adjacent tiles. The u-axis lattice
--   index is wrapped modulo `latU = w / wavelength`, where w is the
--   u-axis tile period. If `wavelength` doesn't divide w evenly, the
--   effective u-axis wavelength drifts by `(w / latU - wavelength)` —
--   <1% at typical world sizes, visually indistinguishable.
meanderNoise ∷ Word64 → Int → Int → Int → Int → Int → Float
meanderNoise seed worldSize gx gy wavelength prop =
    let (gx', gy') = wrapGlobalU worldSize gx gy
        w  = worldWidthTiles worldSize
        u  = gx' - gy'
        v  = gx' + gy'
        fu = fromIntegral u / fromIntegral wavelength ∷ Float
        fv = fromIntegral v / fromIntegral wavelength ∷ Float
        iu = floor fu ∷ Int
        iv = floor fv ∷ Int
        fracU = fu - fromIntegral iu
        fracV = fv - fromIntegral iv
        -- Smoothstep for C1 continuity
        su = fracU * fracU * (3.0 - 2.0 * fracU)
        sv = fracV * fracV * (3.0 - 2.0 * fracV)
        -- Periodic lattice on u-axis (v-axis doesn't wrap)
        latU = max 1 (w `div` wavelength)
        wrapU k = ((k `mod` latU) + latU) `mod` latU
        i0 = wrapU iu
        i1 = wrapU (iu + 1)
        -- Hash at four corners
        h00 = hashToFloatGeo (hashGeo seed (i0 * 7919 + iv * 6271) prop) * 2.0 - 1.0
        h10 = hashToFloatGeo (hashGeo seed (i1 * 7919 + iv * 6271) prop) * 2.0 - 1.0
        h01 = hashToFloatGeo (hashGeo seed (i0 * 7919 + (iv+1) * 6271) prop) * 2.0 - 1.0
        h11 = hashToFloatGeo (hashGeo seed (i1 * 7919 + (iv+1) * 6271) prop) * 2.0 - 1.0
        -- Bilinear interpolation
        top    = h00 * (1.0 - su) + h10 * su
        bottom = h01 * (1.0 - su) + h11 * su
    in top * (1.0 - sv) + bottom * sv

-- * Incremental Update

updateElevGrid ∷ Int → ElevGrid → GeoPeriod → ElevGrid
updateElevGrid worldSize grid period =
    let events = gpEvents period
    in if null events
       then grid
       else
       let gridW = egGridW grid
           totalSamples = gridW * gridW
           oldElev = egElev grid

           newElev = VU.generate totalSamples $ \idx →
               let gx = egGX grid VU.! idx
                   gy = egGY grid VU.! idx
                   e0 = oldElev VU.! idx
               in foldl' (\e event →
                      let mod' = applyGeoEventSimple event worldSize gx gy e
                      in e + gmElevDelta mod'
                  ) e0 events

           newLand = VU.generate totalSamples $ \idx →
               let gx = egGX grid VU.! idx
                   gy = egGY grid VU.! idx
                   (gx', gy') = wrapGlobalU worldSize gx gy
               in newElev VU.! idx > seaLevel
                ∧ not (isBeyondGlacier worldSize gx' gy')

       in grid { egElev = newElev, egLand = newLand }

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

-- * Flow Simulation

simulateHydrology ∷ Word64 → Int → Int → ElevGrid → ClimateState → FlowResult
simulateHydrology seed worldSize ageIdx grid climate =
    let gridW   = egGridW grid
        spacing = egSpacing grid
        totalSamples = gridW * gridW
        origElev = egElev grid
        landVec  = egLand grid
        gxVec    = egGX grid
        gyVec    = egGY grid

        toIdx ix iy = iy * gridW + ix
        fromIdx idx = (idx `mod` gridW, idx `div` gridW)
        wrapIX ix = ((ix `mod` gridW) + gridW) `mod` gridW

        neighborOffsets ∷ [(Int, Int)]
        neighborOffsets = [(-1,0),(1,0),(0,-1),(0,1)
                          ,(-1,-1),(1,-1),(-1,1),(1,1)]

        neighbors ∷ Int → [Int]
        neighbors idx =
            let (ix, iy) = fromIdx idx
            in [ toIdx (wrapIX (ix + dx)) ny
               | (dx, dy) ← neighborOffsets
               , let ny = iy + dy
               , ny ≥ 0 ∧ ny < gridW
               ]

        ---------------------------------------------------
        -- Step 1: Fill depressions + drainage directions
        ---------------------------------------------------
        -- fillDepressions now returns both the filled surface
        -- AND the drainage parent for each cell, recorded during
        -- the priority-flood. This correctly handles flat areas
        -- (filled depressions) where steepest-descent would fail.
        (filledElev, flowDirVec) = fillDepressions grid

        ---------------------------------------------------
        -- Step 2: Flow accumulation
        ---------------------------------------------------
        -- Sort indices by descending filled elevation using in-place
        -- vector sort. Much faster than list sort on 262K elements
        -- because it avoids list node allocation and GC pressure.
        sortedByElev ∷ VU.Vector Int
        sortedByElev = runST $ do
            mv ← VUM.generate totalSamples id
            VA.sortBy (\a b → compare (filledElev VU.! b) (filledElev VU.! a)) mv
            VU.unsafeFreeze mv

        -- Precipitation-weighted flow accumulation.
        -- Each cell contributes water based on local rainfall + snowmelt
        -- instead of a flat 1 unit. This ensures rivers form from wet
        -- regions and mountain snowmelt, not just drainage area.
        accumVec ∷ VU.Vector Int
        accumVec = runST $ do
            mv ← VUM.new totalSamples
            -- Initialize each cell's water contribution from climate
            forM_ [0 .. totalSamples - 1] $ \idx →
                if not (landVec VU.! idx)
                then VUM.write mv idx 0
                else do
                    let gx = gxVec VU.! idx
                        gy = gyVec VU.! idx
                        LocalClimate{lcTemp=temp, lcPrecip=precip
                                    , lcSnow=snowFrac} =
                            lookupLocalClimate climate worldSize gx gy
                        -- Direct rainfall (non-snow precipitation)
                        rainfall = precip * (1.0 - snowFrac)
                        -- Snowmelt: snow that thaws. Ramps from 0 at
                        -- -5°C (permanently frozen) to full melt at +5°C.
                        -- This places river sources at the snowmelt line
                        -- below glacial peaks, not on the frozen summits.
                        meltFactor = max 0.0 (min 1.0 ((temp + 5.0) / 10.0))
                        snowmelt = snowFrac * precip * meltFactor
                        -- Total water entering the system at this cell.
                        -- Scale ×10 to keep integer precision (1 old unit = 10 new).
                        -- Minimum 1 so that even dry cells contribute a trickle.
                        waterUnits = max 1 (round ((rainfall + snowmelt) * 10.0) ∷ Int)
                    VUM.write mv idx waterUnits
            -- Accumulate downstream (topological order)
            VU.forM_ sortedByElev $ \idx → do
                myAccum ← VUM.read mv idx
                let downstream = flowDirVec VU.! idx
                when (downstream ≥ 0) $
                    VUM.modify mv (+ myAccum) downstream
            VU.unsafeFreeze mv

        ---------------------------------------------------
        -- Step 3: Lakes
        ---------------------------------------------------
        -- Raw lake candidates — sorted deepest-first so large basins
        -- get priority during dedup (they "claim" more territory).
        lakes ∷ [LakeParams]
        lakes = sortBy (comparing (Down . lkDepth)) $ catMaybes
            [ let origE  = origElev VU.! idx
                  fillE  = filledElev VU.! idx
                  depth  = fillE - origE
                  gx     = gxVec VU.! idx
                  gy     = gyVec VU.! idx
                  -- Radius scales with sqrt(depth) — deep basins get
                  -- proportionally larger lakes (power-law distribution).
                  -- Cap at 50 to keep spillway checks within land.
                  r = min 50 (max 4 (round (sqrt (fromIntegral depth ∷ Float)
                           * (4.0 ∷ Float))))
              in if landVec VU.! idx ∧ depth ≥ minLakeDepth
                 then Just LakeParams
                    { lkCenter  = GeoCoord gx gy
                    , lkRadius  = r
                    , lkSurface = fillE
                    , lkDepth   = depth
                    , lkSource  = TectonicBasin
                    }
                 else Nothing
            | idx ← [0 .. totalSamples - 1]
            ]

        dedupedLakes = dedupLakes worldSize lakes

        ---------------------------------------------------
        -- Step 4: River sources
        -- A cell is a headwater if:
        --   1. It's land
        --   2. It has accumulation >= riverThreshold
        --   3. No upstream neighbor has accumulation >= riverThreshold
        --      (i.e., this is where significant flow begins)
        --
        -- riverThreshold scales with the world's avg precipitation so
        -- the drainage-area requirement (in cells) stays consistent
        -- across arid / balanced / wet climates (audit #15).
        ---------------------------------------------------

        riverThreshold = effRiverThreshold climate

        -- Pre-compute which cells have a qualified upstream contributor.
        -- For each cell with sufficient flow, mark its downstream target
        -- as "has qualified upstream". This is O(n) instead of O(n×8).
        hasQualifiedUpstream ∷ VU.Vector Bool
        hasQualifiedUpstream = runST $ do
            mv ← VUM.replicate totalSamples False
            forM_ [0 .. totalSamples - 1] $ \idx →
                when (accumVec VU.! idx ≥ riverThreshold) $ do
                    let downstream = flowDirVec VU.! idx
                    when (downstream ≥ 0 ∧ downstream < totalSamples) $
                        VUM.write mv downstream True
            VU.unsafeFreeze mv

        headwaters = filter (\idx →
            landVec VU.! idx
            ∧ accumVec VU.! idx ≥ riverThreshold
            ∧ not (hasQualifiedUpstream VU.! idx)
            ) [0 .. totalSamples - 1]

        -- Sort by accumulation descending — biggest rivers first
        sortedSources = sortBy (comparing (Down . (accumVec VU.!))) headwaters

        -- For each cell, the upstream neighbour that contributes the
        -- most flow (its "main-stem" parent). Built by inverting the
        -- flow grid: every land cell names one downstream target, so we
        -- record, per target, the contributor with the largest
        -- accumulation. O(n).
        bestUpstream ∷ VU.Vector Int
        bestUpstream = runST $ do
            parent  ← VUM.replicate totalSamples (-1)
            bestAcc ← VUM.replicate totalSamples (minBound ∷ Int)
            forM_ [0 .. totalSamples - 1] $ \idx → do
                let d = flowDirVec VU.! idx
                when (d ≥ 0 ∧ d < totalSamples) $ do
                    let a = accumVec VU.! idx
                    cur ← VUM.read bestAcc d
                    when (a > cur) $ do
                        VUM.write bestAcc d a
                        VUM.write parent  d idx
            VU.unsafeFreeze parent

        -- Walk from a headwater up the main-stem parent chain toward the
        -- catchment divide (issue #221). A "headwater" is only the cell
        -- where flow first crossed the river threshold — typically LOW,
        -- near the wet coastal mountains where precipitation
        -- concentrates, which is why rivers used to start near the coast
        -- and stay short. Following the largest upstream contributor
        -- moves the SOURCE toward the true drainage head, so the river
        -- originates inland and grows as it descends. This is additive:
        -- every river is preserved and merely lengthened up its own
        -- catchment, so small coastal catchments stay short while large
        -- interior catchments become long — sea-draining rivers reach
        -- the sea, closed-basin rivers still terminate inland.
        --
        -- The walk STOPS on three conditions:
        --
        --  1. The next cell upstream is a LAKE / basin cell — its
        --     depression-filled surface stands `minLakeDepth` or more
        --     above its raw terrain, i.e. it sits under a lake. Stopping
        --     here keeps the source BELOW the lake, at the basin's
        --     outflow, so the river flows *into* the lake rather than
        --     tracing through and carving it out. This preserves the
        --     interior lakes (closed-basin drainages stay lakes, as
        --     intended) and, because those basins are no longer drained,
        --     stops both the lava that sat beneath them from being
        --     exposed and the basin-floor valley from being lava-flooded
        --     (issue #221 — the lake-collapse / lava blow-up that
        --     extending sources had caused on small volcanic worlds).
        --
        --  2. The climb exceeds `maxClimb` above the headwater. This
        --     keeps the source in the foothills rather than on the
        --     volcanic peaks, whose fresh headwater valleys the
        --     lava-pool pour (Magma.Pool floods local depressions to
        --     their rim) would otherwise bury.
        --
        --  3. The usual ridge / off-land / step bounds.
        --
        -- The upstream graph is a forest (flow is acyclic) so it cannot
        -- loop; maxWalk also bounds the cost.
        maxClimb ∷ Int
        maxClimb = 40

        -- A grid cell is "in a lake" if it is a flooded basin cell —
        -- its depression-filled surface stands above its raw terrain.
        -- This matches the actual world-lake pipeline, which makes EVERY
        -- flooded basin a rendered lake/pond (basin tile = filled >
        -- terrain, kept by area ≥ minBasinTiles = 1 — depth is NOT a
        -- factor; World.Fluid.Lake.Identify). An earlier per-cell
        -- `depth ≥ minLakeDepth` test, and even a whole-component test
        -- gated on the component's deepest cell, both missed shallow
        -- ponds (max depth 1..8) — which ARE lakes — and let a river be
        -- sourced inside one and traced/carved out through it (issue
        -- #221 review). Any flooded land cell is therefore a lake cell;
        -- `walkToDivide` stops below it and `dropToOutflow` descends a
        -- flooded headwater to the basin's spill point, so rivers start
        -- on well-drained ground and flow INTO ponds, never out of them.
        isLakeCell ∷ Int → Bool
        isLakeCell i = landVec VU.! i ∧ filledElev VU.! i > origElev VU.! i
        walkToDivide ∷ Int → Int
        walkToDivide start = go start (0 ∷ Int)
          where
            maxWalk = 4 * gridW
            startElev = origElev VU.! start
            go idx steps
                | steps ≥ maxWalk = idx
                | otherwise =
                    let up = bestUpstream VU.! idx
                    in if up < 0 ∨ not (landVec VU.! up)
                          ∨ isLakeCell up
                          ∨ origElev VU.! up - startElev > maxClimb
                       then idx
                       else go up (steps + 1)

        -- A headwater (first threshold-crossing cell) can itself fall
        -- INSIDE a filled lake basin — lake flats are land, so they pass
        -- the headwater test. Sourcing a river there (or, after
        -- `walkToDivide`, just above it) would carve the basin out and
        -- drain the lake. So if the chosen source is a lake cell, descend
        -- the flow chain to the basin's OUTFLOW (the first non-lake cell)
        -- and source there, so the river starts at the lake's edge and
        -- flows AWAY from it — the lake is preserved (issue #221). Bounded
        -- by maxWalk; falls back to the last cell if the chain hits the
        -- ocean while still flooded (degenerate lake-to-sea case).
        dropToOutflow ∷ Int → Int
        dropToOutflow start = go start (0 ∷ Int)
          where
            maxWalk = 4 * gridW
            go idx steps
                | steps ≥ maxWalk      = idx
                | not (isLakeCell idx) = idx
                | otherwise =
                    let d = flowDirVec VU.! idx
                    in if d ≥ 0 ∧ d < totalSamples
                       then go d (steps + 1)
                       else idx

        -- Inland-origin source extension is applied only on real-scale
        -- worlds (issue #221). On tiny worlds (size 32/64 — the
        -- regression-gate sizes) it is both pointless (a 512–1024-tile
        -- world can't host a long river) and actively harmful: their
        -- volcanism is packed densely enough that an extended river
        -- routes through a caldera, breaches its rim and lets the
        -- lava-pool pour flood the low terrain — observed as 100s of
        -- new lava tiles on ground dropped ~160 z (seed 13579 w32),
        -- which the per-source lake-stop and climb cap can't prevent
        -- because the breach is mid-course, not at the source. Gating
        -- to worldSize ≥ `minExtendWorld` confines the feature to the
        -- worlds it targets (≥128 verified lava-neutral) and leaves the
        -- small worlds — and the magma system — exactly as they were.
        extendSources = worldSize ≥ minExtendWorld

        riverSources ∷ [(Int, Int, Int, Float)]
        riverSources = map (\idx →
            let srcIdx
                    | not extendSources = idx              -- small worlds: unchanged
                    | isLakeCell idx    = dropToOutflow idx -- headwater in a lake
                    | otherwise         = walkToDivide idx  -- extend up to divide
                gx = gxVec VU.! srcIdx
                gy = gyVec VU.! srcIdx
                elev = origElev VU.! srcIdx
                -- Flow magnitude characterises the whole river, so it is
                -- taken from the headwater (where flow first
                -- concentrated), not the thin divide cell. Accumulation
                -- is precipitation-weighted (×10 scale); the same flow
                -- formula as before.
                flow = fromIntegral (accumVec VU.! idx) * 0.005 + 0.1
            in (gx, gy, elev, flow)
            ) sortedSources

    in FlowResult { frRiverSources = riverSources
                   , frLakes = dedupedLakes
                   , frFilledElev = filledElev
                   , frFlowDir = flowDirVec
                   }

-- * Lake Deduplication

-- | Radius-aware dedup: a new lake is rejected if it falls within
--   the exclusion zone of any already-accepted lake.  The exclusion
--   radius is the *larger* of the two lakes' radii × 1.5, so big
--   lakes claim more territory and prevent noisy clusters.
dedupLakes ∷ Int → [LakeParams] → [LakeParams]
dedupLakes worldSize = go []
  where
    go acc [] = acc
    go acc (lk:rest) =
        let dominated = any (\existing →
                let GeoCoord ex ey = lkCenter existing
                    GeoCoord lx ly = lkCenter lk
                    (dxi, dyi) = wrappedDeltaUV worldSize lx ly ex ey
                    -- Exclusion zone: 1.5× the larger radius
                    exclR = round (fromIntegral (max (lkRadius existing)
                                                     (lkRadius lk))
                                   * (1.5 ∷ Float))
                in abs dxi < exclR ∧ abs dyi < exclR
                ) acc
        in if dominated
           then go acc rest
           else go (lk : acc) rest

-- * Simplified Event Application

applyGeoEventSimple ∷ GeoEvent → Int → Int → Int → Int → GeoModification
applyGeoEventSimple (CraterEvent params) ws gx gy _e =
    let GeoCoord cx cy = cpCenter params
        (dxi, dyi) = wrappedDeltaUV ws gx gy cx cy
        dx = fromIntegral dxi ∷ Float
        dy = fromIntegral dyi ∷ Float
        dist = sqrt (dx * dx + dy * dy)
        r = fromIntegral (cpRadius params) ∷ Float
    in if dist > r * 1.5
       then noModification
       else if dist < r * 0.8
            then GeoModification (negate (cpDepth params)) Nothing 0
            else if dist < r
                 then GeoModification (cpRimHeight params) Nothing 0
                 else noModification
applyGeoEventSimple (VolcanicEvent feature) ws gx gy _e =
    applyVolcanicSimple feature ws gx gy
applyGeoEventSimple _ _ _ _ _ = noModification

applyVolcanicSimple ∷ FeatureShape → Int → Int → Int → GeoModification
applyVolcanicSimple (VolcanicShape (ShieldVolcano p)) ws gx gy =
    let GeoCoord cx cy = shCenter p
        (dxi, dyi) = wrappedDeltaUV ws gx gy cx cy
        dx = fromIntegral dxi ∷ Float
        dy = fromIntegral dyi ∷ Float
        dist = sqrt (dx * dx + dy * dy)
        r = fromIntegral (shBaseRadius p) ∷ Float
    in if dist > r then noModification
       else let t = 1.0 - dist / r
            in GeoModification (round (fromIntegral (shPeakHeight p) * t * t)) Nothing 0
applyVolcanicSimple (VolcanicShape (SuperVolcano p)) ws gx gy =
    let GeoCoord cx cy = svCenter p
        (dxi, dyi) = wrappedDeltaUV ws gx gy cx cy
        dx = fromIntegral dxi ∷ Float
        dy = fromIntegral dyi ∷ Float
        dist = sqrt (dx * dx + dy * dy)
        outerR = fromIntegral (svEjectaRadius p) ∷ Float
        calderaR = fromIntegral (svCalderaRadius p) ∷ Float
    in if dist > outerR then noModification
       else if dist < calderaR
            then GeoModification (negate (svFloorDepth p)) Nothing 0
            else let denom = outerR - calderaR
                     t = if denom > 0.0 then 1.0 - (dist - calderaR) / denom else 1.0
                 in GeoModification (round (fromIntegral (svRimHeight p) * t)) Nothing 0
applyVolcanicSimple (VolcanicShape (CinderCone p)) ws gx gy =
    let GeoCoord cx cy = ccCenter p
        (dxi, dyi) = wrappedDeltaUV ws gx gy cx cy
        dx = fromIntegral dxi ∷ Float
        dy = fromIntegral dyi ∷ Float
        dist = sqrt (dx * dx + dy * dy)
        r = fromIntegral (ccBaseRadius p) ∷ Float
    in if dist > r then noModification
       else let t = 1.0 - dist / r
            in GeoModification (round (fromIntegral (ccPeakHeight p) * t)) Nothing 0
applyVolcanicSimple _ _ _ _ = noModification

