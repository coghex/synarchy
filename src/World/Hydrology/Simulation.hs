{-# LANGUAGE Strict, UnicodeSyntax #-}
module World.Hydrology.Simulation
    ( simulateHydrology
    , FlowResult(..)
    , ElevGrid(..)
    , buildInitialElevGrid
    , updateElevGrid
    ) where

import UPrelude
import Data.Bits (xor)
import Data.Word (Word64)
import Data.List (sortBy)
import Data.Ord (comparing, Down(..))
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as VU
import qualified Data.Vector.Unboxed.Mutable as VUM
import qualified Data.Vector.Algorithms.Intro as VA
import Control.Monad.ST (runST, ST)
import Control.Monad (forM_, when)
import World.Base (GeoCoord(..), GeoFeatureId(..))
import World.Constants (seaLevel)
import World.Types
import World.Plate (TectonicPlate, elevationAtGlobal, isBeyondGlacier, wrapGlobalU)
import World.Geology.Types
import World.Geology.Hash (hashGeo, hashToFloatGeo, wrappedDeltaUV)
import World.Hydrology.Types
import World.Weather.Types (ClimateState(..))
import World.Weather.Lookup (lookupLocalClimate, LocalClimate(..))

-- * Configuration

baseSampleSpacing ∷ Int
baseSampleSpacing = 4

-- | Minimum flow accumulation to qualify as a river.
--   Calibrated for precipitation-weighted accumulation where
--   each cell contributes ~1-10 units based on rainfall + snowmelt
--   (×10 scale). Average wet cell ≈ 5 units, so 80 ≈ 16 wet cells
--   of drainage area — similar to the old threshold of 15 flat cells.
minRiverTotalFlow ∷ Int
minRiverTotalFlow = 80

minRiverLength ∷ Int
minRiverLength = 2

maxGridDim ∷ Int
maxGridDim = 384

minLakeDepth ∷ Int
minLakeDepth = 9

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
        spacing = max baseSampleSpacing (totalTiles `div` maxGridDim)
        gridW = min maxGridDim (max 4 (totalTiles `div` spacing))
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
                     n1 = meanderNoise seed gx gy 40 1300
                     n2 = meanderNoise seed gx gy 18 1301
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
            in rawElevV VU.! idx > seaLevel
             ∧ not (isBeyondGlacier worldSize gx' gy')

    in ElevGrid gridW spacing elevV gxV gyV landV

-- | 2D coherent noise for meander induction. Returns [-1, 1].
--   Uses a simple value noise approach: hash at grid points,
--   bilinear interpolation with smoothstep.
meanderNoise ∷ Word64 → Int → Int → Int → Int → Float
meanderNoise seed gx gy wavelength prop =
    let -- Grid coordinates in noise space
        fx = fromIntegral gx / fromIntegral wavelength ∷ Float
        fy = fromIntegral gy / fromIntegral wavelength ∷ Float
        ix = floor fx ∷ Int
        iy = floor fy ∷ Int
        fracX = fx - fromIntegral ix
        fracY = fy - fromIntegral iy
        -- Smoothstep for C1 continuity
        sx = fracX * fracX * (3.0 - 2.0 * fracX)
        sy = fracY * fracY * (3.0 - 2.0 * fracY)
        -- Hash at four corners
        h00 = hashToFloatGeo (hashGeo seed (ix * 7919 + iy * 6271) prop) * 2.0 - 1.0
        h10 = hashToFloatGeo (hashGeo seed ((ix+1) * 7919 + iy * 6271) prop) * 2.0 - 1.0
        h01 = hashToFloatGeo (hashGeo seed (ix * 7919 + (iy+1) * 6271) prop) * 2.0 - 1.0
        h11 = hashToFloatGeo (hashGeo seed ((ix+1) * 7919 + (iy+1) * 6271) prop) * 2.0 - 1.0
        -- Bilinear interpolation
        top    = h00 * (1.0 - sx) + h10 * sx
        bottom = h01 * (1.0 - sx) + h11 * sx
    in top * (1.0 - sy) + bottom * sy

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
               newElev VU.! idx > seaLevel
               ∧ egLand grid VU.! idx

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

        return (filledV, d8FlowDir)

foldlM ∷ Monad m ⇒ (a → b → m a) → a → [b] → m a
foldlM _ acc [] = return acc
foldlM f acc (x:xs) = do
    acc' ← f acc x
    foldlM f acc' xs

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
        -- Step 3: Flow accumulation
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
        -- Step 4: Lakes
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
        -- Step 5: River sources
        -- A cell is a headwater if:
        --   1. It's land
        --   2. It has accumulation >= minRiverTotalFlow
        --   3. No upstream neighbor has accumulation >= minRiverTotalFlow
        --      (i.e., this is where significant flow begins)
        ---------------------------------------------------

        -- Pre-compute which cells have a qualified upstream contributor.
        -- For each cell with sufficient flow, mark its downstream target
        -- as "has qualified upstream". This is O(n) instead of O(n×8).
        hasQualifiedUpstream ∷ VU.Vector Bool
        hasQualifiedUpstream = runST $ do
            mv ← VUM.replicate totalSamples False
            forM_ [0 .. totalSamples - 1] $ \idx →
                when (accumVec VU.! idx ≥ minRiverTotalFlow) $ do
                    let downstream = flowDirVec VU.! idx
                    when (downstream ≥ 0 ∧ downstream < totalSamples) $
                        VUM.write mv downstream True
            VU.unsafeFreeze mv

        headwaters = filter (\idx →
            landVec VU.! idx
            ∧ accumVec VU.! idx ≥ minRiverTotalFlow
            ∧ not (hasQualifiedUpstream VU.! idx)
            ) [0 .. totalSamples - 1]

        -- Sort by accumulation descending — biggest rivers first
        sortedSources = sortBy (comparing (Down . (accumVec VU.!))) headwaters

        riverSources ∷ [(Int, Int, Int, Float)]
        riverSources = map (\idx →
            let gx = gxVec VU.! idx
                gy = gyVec VU.! idx
                elev = origElev VU.! idx
                -- Accumulation is precipitation-weighted (×10 scale).
                -- Divide by 10 to normalize, then apply same flow formula.
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
applyGeoEventSimple (EruptionEvent _ flow) ws gx gy e =
    let sx = lfSourceX flow
        sy = lfSourceY flow
        (dxi, dyi) = wrappedDeltaUV ws gx gy sx sy
        dx = fromIntegral dxi ∷ Float
        dy = fromIntegral dyi ∷ Float
        dist = sqrt (dx * dx + dy * dy)
        maxR = fromIntegral (lfRadius flow) ∷ Float
    in if dist > maxR
       then noModification
       else let visc = fromIntegral (lfViscosity flow) ∷ Float
                lavaSurface = fromIntegral (lfElevation flow) - dist * visc
                deposit = round lavaSurface - e
            in if deposit > 0
               then GeoModification deposit Nothing 0
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
            else let t = 1.0 - (dist - calderaR) / (outerR - calderaR)
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

carveSimple ∷ Int → Int → Int → RiverSegment → GeoModification
carveSimple ws gx gy seg =
    let GeoCoord sx sy = rsStart seg
        GeoCoord ex ey = rsEnd seg
        (pxi, pyi) = wrappedDeltaUV ws gx gy sx sy
        px = fromIntegral pxi ∷ Float
        py = fromIntegral pyi ∷ Float
        (fdxi, fdyi) = wrappedDeltaUV ws ex ey sx sy
        fdx = fromIntegral fdxi ∷ Float
        fdy = fromIntegral fdyi ∷ Float
        segLen = sqrt (fdx * fdx + fdy * fdy)
    in if segLen < 0.001
       then noModification
       else let nx' = fdx / segLen
                ny' = fdy / segLen
                dot = px * nx' + py * ny'
                alongT = dot / segLen
                perpDist = let px' = px - dot * nx'
                               py' = py - dot * ny'
                           in sqrt (px' * px' + py' * py')
                valleyHW = fromIntegral (rsValleyWidth seg) / 2.0
                depth = fromIntegral (rsDepth seg) ∷ Float
            in if alongT < -0.05 ∨ alongT > 1.05 ∨ perpDist > valleyHW
               then noModification
               else let t = perpDist / valleyHW
                        carve = round (depth * max 0.0 (1.0 - t) * 0.5)
                    in if carve ≤ 0 then noModification
                       else GeoModification (negate carve) Nothing 0

-- * River Path Conversion

pathToRiverParams ∷ Word64 → Int → Int → Int → Int
                  → [(Int, Int, Int, Int)] → Maybe RiverParams
pathToRiverParams seed ageIdx worldSize spacing riverIdx path
    | length path < minRiverLength = Nothing
    | otherwise =
        let segments = V.fromList $ zipWith (makeSegment spacing)
                               [0..] (zip path (drop 1 path))
            (srcX, srcY, _, _) = case path of { (p:_) → p; [] → (0,0,0,0) }
            (mouthX, mouthY, _, _) = last path
            totalFlow = if V.null segments
                then 0.1
                else rsFlowRate (V.last segments)
        in Just RiverParams
            { rpSourceRegion = GeoCoord srcX srcY
            , rpMouthRegion  = GeoCoord mouthX mouthY
            , rpSegments     = segments
            , rpFlowRate     = totalFlow
            , rpMeanderSeed  = fromIntegral
                (hashGeo seed (ageIdx * 1000 + riverIdx) 2000)
            }

makeSegment ∷ Int → Int
            → ((Int, Int, Int, Int), (Int, Int, Int, Int))
            → RiverSegment
makeSegment spacing _segIdx
    ((sx, sy, se, _sAccum), (ex, ey, ee, eAccum)) =
    let flow = fromIntegral eAccum * 0.05 + 0.1 ∷ Float
        width = min 16 (max 4 (round (flow * 8.0) ∷ Int))

        slopeDelta = abs (se - ee)
        slopePerTile ∷ Float
        slopePerTile = if slopeDelta ≡ 0 then 0.0
                       else fromIntegral slopeDelta
                          / fromIntegral spacing

        valleyMult ∷ Float
        valleyMult = if slopePerTile > 0.5 then 2.0
                     else 2.0 + (1.0 - min 1.0 (slopePerTile * 2.0)) * 1.5
        -- Depth capped at 6 tiles — carves gentle channels, not canyons
        baseDepth = max 2 (slopeDelta `div` 6 + round (flow * 1.0))
        maxDepth = min 6 (slopeDelta + 4)
        depth = min maxDepth baseDepth
        -- Valley width for water fill. Kept modest to prevent
        -- headwater segments from flooding wide areas at high elevation.
        minValleyW = depth * 3
        rawValleyW = max (width * 2)
                         (round (fromIntegral width * valleyMult) ∷ Int)
        valleyW = max minValleyW (min 32 rawValleyW)

    in RiverSegment
        { rsStart       = GeoCoord sx sy
        , rsEnd         = GeoCoord ex ey
        , rsWidth       = width
        , rsValleyWidth = valleyW
        , rsDepth       = depth
        , rsFlowRate    = flow
        , rsStartElev   = se
        , rsEndElev     = ee
        -- Water surface = reference elevation (no freeboard).
        -- The carved terrain provides the depth profile.
        , rsWaterStart  = max seaLevel se
        , rsWaterEnd    = max seaLevel ee
        }
