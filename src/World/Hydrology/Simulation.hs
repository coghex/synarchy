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
import qualified Data.Vector.Unboxed as VU
import qualified Data.Vector.Unboxed.Mutable as VUM
import qualified Data.Set as Set
import Control.Monad.ST (runST)
import Control.Monad (forM_, when)
import World.Base (GeoCoord(..), GeoFeatureId(..))
import World.Types
import World.Plate (TectonicPlate, elevationAtGlobal, isBeyondGlacier, wrapGlobalU)
import World.Geology.Types
import World.Geology.Hash (hashGeo)
import World.Hydrology.Types
import World.Fluids (seaLevel)

-----------------------------------------------------------
-- Configuration
-----------------------------------------------------------

baseSampleSpacing ∷ Int
baseSampleSpacing = 8

-- | Minimum flow accumulation to qualify as a river.
minRiverTotalFlow ∷ Int
minRiverTotalFlow = 4

maxGridDim ∷ Int
maxGridDim = 64

minLakeDepth ∷ Int
minLakeDepth = 5

-----------------------------------------------------------
-- Types
-----------------------------------------------------------

data FlowResult = FlowResult
    { frRivers  ∷ ![RiverParams]
    , frLakes   ∷ ![LakeParams]
    } deriving (Show)

data ElevGrid = ElevGrid
    { egGridW   ∷ !Int
    , egSpacing ∷ !Int
    , egElev    ∷ !(VU.Vector Int)
    , egGX      ∷ !(VU.Vector Int)
    , egGY      ∷ !(VU.Vector Int)
    , egLand    ∷ !(VU.Vector Bool)
    } deriving (Show)

-----------------------------------------------------------
-- Grid Construction
-----------------------------------------------------------

buildInitialElevGrid ∷ Word64 → Int → [TectonicPlate] → ElevGrid
buildInitialElevGrid seed worldSize plates =
    let totalTiles = worldSize * 16
        spacing = max baseSampleSpacing (totalTiles `div` maxGridDim)
        gridW = max 4 (totalTiles `div` spacing)
        halfGrid = gridW `div` 2
        totalSamples = gridW * gridW
        fromIdx idx = (idx `mod` gridW, idx `div` gridW)

        gxV = VU.generate totalSamples $ \idx →
            let (ix, _) = fromIdx idx
            in (ix - halfGrid) * spacing

        gyV = VU.generate totalSamples $ \idx →
            let (_, iy) = fromIdx idx
            in (iy - halfGrid) * spacing

        elevV = VU.generate totalSamples $ \idx →
            let gx = gxV VU.! idx
                gy = gyV VU.! idx
                (gx', gy') = wrapGlobalU worldSize gx gy
            in if isBeyondGlacier worldSize gx' gy'
               then seaLevel + 500
               else fst (elevationAtGlobal seed plates worldSize gx' gy')

        landV = VU.generate totalSamples $ \idx →
            let gx = gxV VU.! idx
                gy = gyV VU.! idx
                (gx', gy') = wrapGlobalU worldSize gx gy
            in elevV VU.! idx > seaLevel
             ∧ not (isBeyondGlacier worldSize gx' gy')

    in ElevGrid gridW spacing elevV gxV gyV landV

-----------------------------------------------------------
-- Incremental Update
-----------------------------------------------------------

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

-----------------------------------------------------------
-- Depression Filling (Priority-Flood using Data.Set)
-----------------------------------------------------------

-- | Priority-flood using Data.Set as a min-heap.
--   Set entries are (elevation, index) so they sort by elevation.
--   This is O(n log n) and guaranteed correct in a single pass.
fillDepressions ∷ ElevGrid → VU.Vector Int
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

        -- Seed: all cells that are ocean or on Y-edges.
        -- X wraps, so no X edges.
        let seeds = filter (\idx →
                let (_, iy) = fromIdx idx
                in not (landVec VU.! idx)
                 ∨ iy ≡ 0
                 ∨ iy ≡ gridW - 1
                ) [0 .. totalSamples - 1]

        -- Initialize seeds
        forM_ seeds $ \idx → do
            VUM.write filled idx (elevVec VU.! idx)
            VUM.write visited idx True

        -- Build initial priority queue.
        -- Use (elevation, index) pairs. Set gives us O(log n) min extraction.
        -- To handle duplicate elevations, encode index into the key
        -- so every entry is unique: (elev, idx).
        let initQueue = foldl' (\s idx →
                Set.insert (elevVec VU.! idx, idx) s
                ) Set.empty seeds

        -- Process queue: extract minimum, expand neighbors
        let go queue
                | Set.null queue = return ()
                | otherwise = do
                    let ((curElev, curIdx), queue') = Set.deleteFindMin queue
                    -- Expand unvisited neighbors
                    let nbrs = neighbors curIdx
                    queue'' ← foldlM (\q nIdx → do
                        nVis ← VUM.read visited nIdx
                        if nVis
                        then return q
                        else do
                            VUM.write visited nIdx True
                            let nElev = elevVec VU.! nIdx
                                -- Fill level: at least as high as how we reached it
                                nFill = max nElev curElev
                            VUM.write filled nIdx nFill
                            return (Set.insert (nFill, nIdx) q)
                        ) queue' nbrs
                    go queue''

        go initQueue

        -- Safety: any unvisited cell gets its own elevation
        forM_ [0 .. totalSamples - 1] $ \idx → do
            vis ← VUM.read visited idx
            when (not vis) $
                VUM.write filled idx (elevVec VU.! idx)

        VU.unsafeFreeze filled

-- | foldlM for lists in ST
foldlM ∷ Monad m ⇒ (a → b → m a) → a → [b] → m a
foldlM _ acc [] = return acc
foldlM f acc (x:xs) = do
    acc' ← f acc x
    foldlM f acc' xs

-----------------------------------------------------------
-- Flow Simulation
-----------------------------------------------------------

simulateHydrology ∷ Word64 → Int → Int → ElevGrid → FlowResult
simulateHydrology seed worldSize ageIdx grid =
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

        ---------------------------------------------------
        -- Step 1: Fill depressions
        ---------------------------------------------------
        filledElev = fillDepressions grid

        -- Debug: count how many cells got filled vs original
        _debugFilledCount = VU.length $ VU.filter id $
            VU.zipWith (\o f → f > o) origElev filledElev
        _debugSinkCount = VU.length $ VU.filter id $
            VU.imap (\idx _ →
                landVec VU.! idx
                ∧ filledElev VU.! idx ≡ maxBound
                ) origElev

        ---------------------------------------------------
        -- Step 2: Flow direction on filled surface
        -- Tie-breaking: when filled elevations are equal,
        -- prefer the neighbor with lower ORIGINAL elevation.
        -- This routes water through flat filled areas toward
        -- the depression outlet.
        ---------------------------------------------------
        flowDirVec ∷ VU.Vector Int
        flowDirVec = VU.generate totalSamples $ \idx →
            if not (landVec VU.! idx)
            then -1
            else let (ix, iy) = fromIdx idx
                     myElev = filledElev VU.! idx
                     findLowest ∷ Int → Int → Int → [(Int,Int)] → (Int, Int, Int)
                     findLowest bi be bo [] = (bi, be, bo)
                     findLowest bi be bo ((dx,dy):rest) =
                         let ny = iy + dy
                         in if ny < 0 ∨ ny ≥ gridW
                            then findLowest bi be bo rest
                            else let nIdx = toIdx (wrapIX (ix + dx)) ny
                                     nFill = filledElev VU.! nIdx
                                     nOrig = origElev VU.! nIdx
                                 in if nFill < be
                                      ∨ (nFill ≡ be ∧ nOrig < bo)
                                    then findLowest nIdx nFill nOrig rest
                                    else findLowest bi be bo rest
                     myOrig = origElev VU.! idx
                     (lowestIdx, lowestElev, _) =
                         findLowest idx myElev myOrig neighborOffsets
                 in if lowestElev ≥ myElev
                      ∧ lowestIdx ≡ idx  -- no better neighbor found
                    then -1
                    else lowestIdx

        ---------------------------------------------------
        -- Step 3: Flow accumulation
        ---------------------------------------------------
        sortedByElev ∷ [Int]
        sortedByElev = sortBy (comparing (Down . (filledElev VU.!)))
                              [0 .. totalSamples - 1]

        accumVec ∷ VU.Vector Int
        accumVec = runST $ do
            mv ← VUM.replicate totalSamples (1 ∷ Int)
            forM_ sortedByElev $ \idx → do
                myAccum ← VUM.read mv idx
                let downstream = flowDirVec VU.! idx
                when (downstream ≥ 0) $
                    VUM.modify mv (+ myAccum) downstream
            VU.unsafeFreeze mv

        ---------------------------------------------------
        -- Step 4: Lakes
        ---------------------------------------------------
        lakes ∷ [LakeParams]
        lakes = catMaybes
            [ let origE  = origElev VU.! idx
                  fillE  = filledElev VU.! idx
                  depth  = fillE - origE
                  gx     = gxVec VU.! idx
                  gy     = gyVec VU.! idx
              in if landVec VU.! idx ∧ depth ≥ minLakeDepth
                 then Just LakeParams
                    { lkCenter  = GeoCoord gx gy
                    , lkRadius  = min 30 (max 3 (depth * 2 + spacing))
                    , lkSurface = fillE
                    , lkDepth   = depth
                    , lkSource  = TectonicBasin
                    }
                 else Nothing
            | idx ← [0 .. totalSamples - 1]
            ]

        dedupedLakes = dedupLakes spacing lakes

        ---------------------------------------------------
        -- Step 5: Rivers
        ---------------------------------------------------
        -- Find all cells where flow first crosses threshold.
        -- A source = land cell with accum >= threshold AND
        -- no upstream neighbor with accum >= threshold flowing INTO this cell.
        riverSources ∷ [Int]
        riverSources = filter isRiverSource [0 .. totalSamples - 1]

        isRiverSource ∷ Int → Bool
        isRiverSource idx =
            landVec VU.! idx
            ∧ accumVec VU.! idx ≥ minRiverTotalFlow
            ∧ let (ix, iy) = fromIdx idx
              in not $ any (\(dx, dy) →
                    let ny = iy + dy
                    in ny ≥ 0 ∧ ny < gridW
                     ∧ let nIdx = toIdx (wrapIX (ix + dx)) ny
                       in flowDirVec VU.! nIdx ≡ idx
                        ∧ accumVec VU.! nIdx ≥ minRiverTotalFlow
                    ) neighborOffsets

        traceRiver ∷ Int → [(Int, Int, Int, Int)]
        traceRiver srcIdx =
            let maxSteps = min totalSamples 500
                go ∷ Int → [(Int, Int, Int, Int)] → Int → [(Int, Int, Int, Int)]
                go _ acc 0 = reverse acc
                go idx acc stepsLeft
                    | idx < 0 = reverse acc
                    | otherwise =
                        let gx     = gxVec VU.! idx
                            gy     = gyVec VU.! idx
                            elev   = origElev VU.! idx
                            accum  = accumVec VU.! idx
                            isLand' = landVec VU.! idx
                            next   = flowDirVec VU.! idx
                            wp     = (gx, gy, elev, accum)
                        in if not isLand' ∧ not (null acc)
                           then reverse (wp : acc)
                           else go next (wp : acc) (stepsLeft - 1)
            in go srcIdx [] maxSteps

        rivers ∷ [RiverParams]
        rivers = catMaybes $ zipWith
            (\riverIdx path → pathToRiverParams seed ageIdx worldSize spacing riverIdx path)
            [0..] (map traceRiver riverSources)

    in FlowResult { frRivers = rivers, frLakes = dedupedLakes }

-----------------------------------------------------------
-- Lake Deduplication
-----------------------------------------------------------

dedupLakes ∷ Int → [LakeParams] → [LakeParams]
dedupLakes spacing = go []
  where
    go acc [] = acc
    go acc (lk:rest) =
        let dominated = any (\existing →
                let GeoCoord ex ey = lkCenter existing
                    GeoCoord lx ly = lkCenter lk
                    dx = abs (ex - lx)
                    dy = abs (ey - ly)
                in dx < spacing * 3 ∧ dy < spacing * 3
                ) acc
        in if dominated
           then go acc rest
           else go (lk : acc) rest

-----------------------------------------------------------
-- Simplified Event Application
-----------------------------------------------------------

applyGeoEventSimple ∷ GeoEvent → Int → Int → Int → Int → GeoModification
applyGeoEventSimple (CraterEvent params) ws gx gy _e =
    let GeoCoord cx cy = cpCenter params
        dx = fromIntegral (wrappedDelta ws gx cx) ∷ Float
        dy = fromIntegral (gy - cy) ∷ Float
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
        dx = fromIntegral (wrappedDelta ws gx sx) ∷ Float
        dy = fromIntegral (gy - sy) ∷ Float
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
        dx = fromIntegral (wrappedDelta ws gx cx) ∷ Float
        dy = fromIntegral (gy - cy) ∷ Float
        dist = sqrt (dx * dx + dy * dy)
        r = fromIntegral (shBaseRadius p) ∷ Float
    in if dist > r then noModification
       else let t = 1.0 - dist / r
            in GeoModification (round (fromIntegral (shPeakHeight p) * t * t)) Nothing 0
applyVolcanicSimple (VolcanicShape (SuperVolcano p)) ws gx gy =
    let GeoCoord cx cy = svCenter p
        dx = fromIntegral (wrappedDelta ws gx cx) ∷ Float
        dy = fromIntegral (gy - cy) ∷ Float
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
        dx = fromIntegral (wrappedDelta ws gx cx) ∷ Float
        dy = fromIntegral (gy - cy) ∷ Float
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
        px = fromIntegral (wrappedDelta ws gx sx) ∷ Float
        py = fromIntegral (gy - sy) ∷ Float
        fdx = fromIntegral (wrappedDelta ws ex sx) ∷ Float
        fdy = fromIntegral (ey - sy) ∷ Float
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
                        carve = round (depth * max 0.0 (1.0 - t) * 0.7)
                    in if carve ≤ 0 then noModification
                       else GeoModification (negate carve) Nothing 0

-----------------------------------------------------------
-- Wrapped Delta
-----------------------------------------------------------

wrappedDelta ∷ Int → Int → Int → Int
wrappedDelta worldSize a b =
    let w = worldSize * 16
        raw = b - a
        halfW = w `div` 2
    in ((raw + halfW) `mod` w + w) `mod` w - halfW

-----------------------------------------------------------
-- River Path Conversion
-----------------------------------------------------------

pathToRiverParams ∷ Word64 → Int → Int → Int → Int
                  → [(Int, Int, Int, Int)] → Maybe RiverParams
pathToRiverParams seed ageIdx worldSize spacing riverIdx path
    | length path < 3 = Nothing
    | otherwise =
        let segments = zipWith (makeSegment spacing)
                               [0..] (zip path (tail path))
            (srcX, srcY, _, _) = head path
            (mouthX, mouthY, _, _) = last path
            totalFlow = case segments of
                [] → 0.1
                _  → rsFlowRate (last segments)
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
        width = min 12 (max 2 (round (flow * 5.0) ∷ Int))

        slopeDelta = abs (se - ee)
        slopePerTile ∷ Float
        slopePerTile = if slopeDelta ≡ 0 then 0.0
                       else fromIntegral slopeDelta
                          / fromIntegral spacing

        valleyMult ∷ Float
        valleyMult = if slopePerTile > 0.5 then 3.0
                     else 3.0 + (1.0 - min 1.0 (slopePerTile * 2.0)) * 5.0
        valleyW = min 96 (max (width * 3)
                              (round (fromIntegral width * valleyMult) ∷ Int))

        baseDepth = max 3 (slopeDelta `div` 2 + round (flow * 4.0))
        maxDepth = min 60 (slopeDelta + 20)
        depth = min maxDepth baseDepth

    in RiverSegment
        { rsStart       = GeoCoord sx sy
        , rsEnd         = GeoCoord ex ey
        , rsWidth       = width
        , rsValleyWidth = valleyW
        , rsDepth       = depth
        , rsFlowRate    = flow
        , rsStartElev   = se
        , rsEndElev     = ee
        }
