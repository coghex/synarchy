{-# OPTIONS_GHC -fprof-auto #-}
{-# LANGUAGE Strict, UnicodeSyntax #-}
module World.Geology.Timeline.RiverTrace
    ( traceRiverFromSource
    ) where
import UPrelude
import Data.Word (Word64)
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as VU
import World.Base (GeoCoord(..))
import World.Constants (seaLevel)
import World.Types
import World.Fluids (fixupSegmentContinuity)
import World.Geology.Hash
import World.Hydrology.Types (RiverParams(..), RiverSegment(..))
import World.Hydrology.Simulation (ElevGrid(..))

-----------------------------------------------------------
-- River tracing via flow-direction chain
-----------------------------------------------------------

-- | Trace a river from a source cell to the ocean by following
--   the pre-computed flow direction grid. This is GUARANTEED to
--   reach the ocean because the flow directions are derived from
--   the depression-filled surface (priority-flood algorithm).
--
--   The grid path is augmented with hash-based perpendicular
--   noise so the river doesn't look grid-aligned.
traceRiverFromSource ∷ Word64 → Int → ElevGrid → VU.Vector Int
                     → VU.Vector Int
                     → Int → Int → Int → Int → Float
                     → Maybe RiverParams
traceRiverFromSource seed worldSize elevGrid _filledElev flowDir
                     gx gy srcElev riverIdx flow =
    let gridW   = egGridW elevGrid
        spacing = egSpacing elevGrid
        halfGrid = gridW `div` 2
        totalSamples = gridW * gridW

        -- Convert tile coordinates (gx, gy) to grid index.
        -- Same math as elevFromGrid/filledElevFromGrid.
        tileToGridIdx x y =
            let u = x - y
                v = x + y
                iu = ((u + spacing `div` 2) `div` spacing) + halfGrid
                iv = ((v + spacing `div` 2) `div` spacing) + halfGrid
                iu' = ((iu `mod` gridW) + gridW) `mod` gridW
                iv' = max 0 (min (gridW - 1) iv)
            in iv' * gridW + iu'

        -- Follow the flow direction chain from a grid index to the ocean.
        -- Collects (gx, gy, rawElev) at each grid cell.
        -- Guaranteed to terminate: filled surface is monotonically
        -- descending along flowDir, so no cycles are possible.
        traceGrid startIdx = go 0 startIdx []
          where
            maxGridSteps = 5000 ∷ Int
            go step idx acc
                | step ≥ maxGridSteps = reverse acc
                | idx < 0 ∨ idx ≥ totalSamples = reverse acc
                | otherwise =
                    let tileGX  = egGX elevGrid VU.! idx
                        tileGY  = egGY elevGrid VU.! idx
                        rawElev = egElev elevGrid VU.! idx
                        nextIdx = flowDir VU.! idx
                        acc'    = (tileGX, tileGY, rawElev) : acc
                    in if nextIdx < 0 ∨ nextIdx ≡ idx
                       then reverse acc'  -- ocean sink or dead end
                       else go (step + 1) nextIdx acc'

        startIdx = tileToGridIdx gx gy
        gridPath = traceGrid startIdx

        -- Prepend the actual source point if it differs from
        -- the first grid cell (source may be between grid points).
        fullPath = case gridPath of
            ((fx, fy, _):_)
                | fx ≠ gx ∨ fy ≠ gy → (gx, gy, srcElev) : gridPath
            _ → (gx, gy, srcElev) : gridPath

        -- Add perpendicular noise to each waypoint so the path
        -- looks natural instead of grid-aligned.
        noisyPath = addPathNoise seed riverIdx spacing fullPath

    in if length noisyPath < 4
       then Nothing
       else buildRiverFromPath seed riverIdx flow noisyPath

-----------------------------------------------------------
-- Path noise
-----------------------------------------------------------

-- | Offset each waypoint perpendicular to the local flow
--   direction by a hash-based random amount. Maximum offset
--   is ~40% of grid spacing. Keeps first and last points fixed.
addPathNoise ∷ Word64 → Int → Int → [(Int, Int, Int)] → [(Int, Int, Int)]
addPathNoise _ _ _ [] = []
addPathNoise _ _ _ [x] = [x]
addPathNoise seed riverIdx spacing pts =
    let len = length pts
        -- Keep first and last fixed (source and mouth position matters)
        noisy = zipWith3 (\i (x, y, e) (dx, dy) →
            if i ≡ 0 ∨ i ≡ len - 1
            then (x, y, e)
            else let h = hashGeo seed (riverIdx * 10000 + i) 1300
                     noise = (hashToFloatGeo h - 0.5) * 2.0
                     maxOff = fromIntegral spacing * 0.4 ∷ Float
                     -- Perpendicular to flow direction
                     dLen = sqrt (fromIntegral (dx * dx + dy * dy) ∷ Float)
                     (perpX, perpY) = if dLen < 0.001
                         then (0.0, 0.0)
                         else ( negate (fromIntegral dy) / dLen
                              , fromIntegral dx / dLen )
                     offX = round (perpX * noise * maxOff) ∷ Int
                     offY = round (perpY * noise * maxOff) ∷ Int
                 in (x + offX, y + offY, e)
            ) [0 ∷ Int ..] pts dirs
        -- Flow direction at each point: vector to next point
        dirs = zipWith (\(x1, y1, _) (x2, y2, _) → (x2 - x1, y2 - y1))
                       pts (drop 1 pts)
               <> [(0, 0)]  -- last point has no direction
    in noisy

-----------------------------------------------------------
-- Path construction
-----------------------------------------------------------

buildRiverFromPath ∷ Word64 → Int → Float → [(Int, Int, Int)] → Maybe RiverParams
buildRiverFromPath seed riverIdx baseFlow path =
    let monoPath = enforceMonotonicPath path
        -- Grid-following produces one point per grid cell (~8 tiles apart).
        -- Decimate to every 3rd to get ~24-tile segments, similar to before.
        decimated0 = decimatePath 3 monoPath
        -- Clamp the last point's elevation near sea level if the river
        -- reaches the coast. Use seaLevel + 2 so that after freeboard
        -- subtraction (water = elev - 1), the water surface is still
        -- above sea level and the river doesn't end abruptly.
        decimated = case reverse decimated0 of
            (lx, ly, le) : rest
                | le ≤ seaLevel + 3 → reverse ((lx, ly, seaLevel + 2) : rest)
            _ → decimated0
    in case decimated of
        [] → Nothing
        ((srcX, srcY, _) : _) →
            let numWP = length decimated
                segments0 = fixupSegmentContinuity $ V.fromList $
                               zipWith (buildSegFromWaypoints seed numWP baseFlow)
                                       [0..] (zip decimated (drop 1 decimated))
                segments = poolWaterSurface segments0
                (mouthX, mouthY, _) = last decimated
                totalFlow = case V.null segments of
                    True  → baseFlow
                    False → rsFlowRate (V.last segments)
            in Just RiverParams
                { rpSourceRegion = GeoCoord srcX srcY
                , rpMouthRegion  = GeoCoord mouthX mouthY
                , rpSegments     = segments
                , rpFlowRate     = totalFlow
                , rpMeanderSeed  = fromIntegral (hashGeo seed riverIdx 1150)
                }

-- | Keep every Nth point from the path, always preserving
--   the first and last points.
decimatePath ∷ Int → [(Int, Int, Int)] → [(Int, Int, Int)]
decimatePath _ [] = []
decimatePath _ [x] = [x]
decimatePath n xs =
    let lastPt = last xs
        picked = go 0 xs
        -- Ensure the last point is always included
    in case picked of
        [] → [lastPt]
        _  → if last picked ≡ lastPt then picked
             else picked <> [lastPt]
  where
    go _ [] = []
    go i (x:rest)
        | i `mod` n ≡ 0 = x : go (i + 1) rest
        | otherwise       = go (i + 1) rest

enforceMonotonicPath ∷ [(Int, Int, Int)] → [(Int, Int, Int)]
enforceMonotonicPath [] = []
enforceMonotonicPath [x] = [x]
enforceMonotonicPath ((x0, y0, e0) : rest) =
    (x0, y0, e0) : go e0 rest
  where
    go _ [] = []
    go maxE ((x, y, e) : xs) =
        let e' = min maxE e
        in (x, y, e') : go e' xs

buildSegFromWaypoints ∷ Word64 → Int → Float → Int
                      → ((Int, Int, Int), (Int, Int, Int))
                      → RiverSegment
buildSegFromWaypoints seed totalSegs baseFlow segIdx ((sx, sy, se), (ex, ey, ee)) =
    let t = fromIntegral (segIdx + 1) / fromIntegral totalSegs
        flow = baseFlow + t * baseFlow * 2.0

        rawWidth = max 4 (round (flow * 8.0))
        width = min 16 rawWidth

        h1 = hashGeo seed segIdx 1161
        valleyMult = 2.0 + hashToFloatGeo h1 * 1.5

        slopeDelta = abs (se - ee)
        -- Depth capped at 6 tiles — carves gentle channels, not canyons
        baseDepth = max 2 (slopeDelta `div` 6 + round (flow * 1.0))
        depth = min 6 baseDepth
        -- Valley width for water fill. Kept modest to prevent
        -- headwater segments from flooding wide areas at high elevation.
        minValleyW = depth * 3
        rawValleyW = max (width * 2) (round (fromIntegral width * valleyMult))
        valleyW = max minValleyW (min 32 rawValleyW)

        -- Consistent freeboard: water surface sits 1 tile below
        -- the reference terrain. This keeps the water surface smooth
        -- regardless of per-segment depth variation.
        freeboard = 1 ∷ Int
    in RiverSegment
        { rsStart       = GeoCoord sx sy
        , rsEnd         = GeoCoord ex ey
        , rsWidth       = width
        , rsValleyWidth = valleyW
        , rsDepth       = depth
        , rsFlowRate    = flow
        , rsStartElev   = se
        , rsEndElev     = ee
        , rsWaterStart  = se - freeboard
        , rsWaterEnd    = ee - freeboard
        }

-----------------------------------------------------------
-- Water surface pooling
-----------------------------------------------------------

-- | Pool water surfaces: where terrain forms depressions,
--   water should fill to the level of the outflow point.
--   Walk from mouth to source, propagating the downstream
--   water level upstream through any depressions. This
--   creates level pools in flat areas.
poolWaterSurface ∷ V.Vector RiverSegment → V.Vector RiverSegment
poolWaterSurface segs
    | V.length segs ≤ 1 = segs
    | otherwise =
        let segList = V.toList segs
            -- Extract n+1 water levels: [waterStart_0, waterEnd_0, waterEnd_1, ...]
            waterLevels = rsWaterStart (head segList) : map rsWaterEnd segList
            -- Pool: walk from mouth (last) to source (first).
            -- Any depression gets filled to the outflow level.
            pooled = reverse $ poolFill $ reverse waterLevels
            -- Write back into segments as (start, end) pairs
            pairs = zip pooled (drop 1 pooled)
        in V.fromList $ zipWith (\seg (ws, we) →
            seg { rsWaterStart = ws, rsWaterEnd = we }
            ) segList pairs
  where
    poolFill [] = []
    poolFill (x:xs) = x : goPool x xs
    goPool _ [] = []
    goPool lvl (x:xs) = let x' = max x lvl in x' : goPool x' xs
