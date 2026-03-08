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
import World.Plate (isBeyondGlacier, wrapGlobalU)
import World.Geology.Hash
import World.Hydrology.Types (RiverParams(..), RiverSegment(..))
import World.Hydrology.Simulation (ElevGrid(..))
import World.Geology.Timeline.Helpers (elevFromGrid, filledElevFromGrid)

-----------------------------------------------------------
-- River tracing
-----------------------------------------------------------

-- | Trace a river from a source cell downhill to the sea (or
--   until it gets stuck). Uses the FILLED elevation surface
--   for pathfinding so the path descends monotonically through
--   depressions (which become lakes). Records RAW elevation
--   for segment construction so carving targets real terrain.
traceRiverFromSource ∷ Word64 → Int → ElevGrid → VU.Vector Int
                     → Int → Int → Int → Int → Float
                     → Maybe RiverParams
traceRiverFromSource seed worldSize elevGrid filledElev gx gy srcElev riverIdx flow =
    let stepSize = 4
        maxSteps = 800

        -- Raw elevation for segment construction (carving targets)
        getRawElev x y =
            let (x', y') = wrapGlobalU worldSize x y
            in if isBeyondGlacier worldSize x' y'
               then seaLevel + 500
               else elevFromGrid elevGrid worldSize x' y'

        -- Filled elevation for pathfinding — guarantees monotonic
        -- descent through depressions to the ocean
        getFilledElev x y =
            let (x', y') = wrapGlobalU worldSize x y
            in if isBeyondGlacier worldSize x' y'
               then seaLevel + 500
               else filledElevFromGrid filledElev elevGrid worldSize x' y'

        dirs = [ (stepSize, 0), (-stepSize, 0), (0, stepSize), (0, -stepSize)
               , (stepSize, stepSize), (stepSize, -stepSize)
               , (-stepSize, stepSize), (-stepSize, -stepSize)
               ]

        go step curX curY curFilledElev acc
            | step ≥ maxSteps = finishPath acc curX curY
            | getRawElev curX curY ≤ seaLevel =
                finishPath acc curX curY
            | isBeyondGlacier worldSize curX curY = finishPath acc curX curY
            | otherwise =
                let neighbors = map (\(dx, dy) →
                        let nx = curX + dx
                            ny = curY + dy
                            (nx', ny') = wrapGlobalU worldSize nx ny
                        in if isBeyondGlacier worldSize nx' ny'
                           then (nx', ny', curFilledElev + 1000)
                           else (nx', ny', getFilledElev nx' ny')
                        ) dirs

                    sorted = sortByElevTriple neighbors
                    (bestX, bestY, bestFilledElev) = case sorted of
                        [] → (curX, curY, curFilledElev)
                        [(x, y, e)] → (x, y, e)
                        ((x1, y1, e1) : (x2, y2, e2) : _) →
                            -- Wobble: occasionally pick second-best
                            -- neighbor for more natural, curvy paths
                            let h = hashGeo seed step
                                    (1200 + abs curX `mod` 100
                                          + abs curY `mod` 100)
                                wobble = hashToFloatGeo h
                            in if wobble < 0.6 ∨ e2 > curFilledElev
                               then (x1, y1, e1)
                               else (x2, y2, e2)

                    rawElev = getRawElev curX curY

                in if bestFilledElev ≤ curFilledElev
                   then go (step + 1) bestX bestY bestFilledElev
                           ((curX, curY, rawElev) : acc)
                   -- Stuck on the filled surface means we've truly
                   -- reached a local minimum — finish the path
                   else finishPath ((curX, curY, rawElev) : acc)
                                   curX curY

        finishPath acc lastX lastY =
            let lastRaw = getRawElev lastX lastY
                fullAcc = if null acc ∨ (case acc of
                              ((ax,ay,_):_) → ax ≠ lastX ∨ ay ≠ lastY
                              _ → True)
                          then (lastX, lastY, lastRaw) : acc
                          else acc
                path = reverse fullAcc
            in if length path < 4
               then Nothing
               else Just (buildRiverFromPath seed riverIdx flow path)

    in go 0 gx gy (getFilledElev gx gy) []

sortByElevTriple ∷ [(Int, Int, Int)] → [(Int, Int, Int)]
sortByElevTriple [] = []
sortByElevTriple [x] = [x]
sortByElevTriple xs = foldr insertSorted [] xs
  where
    insertSorted x [] = [x]
    insertSorted x@(_, _, e1) (y@(_, _, e2) : ys)
        | e1 ≤ e2   = x : y : ys
        | otherwise  = y : insertSorted x ys

buildRiverFromPath ∷ Word64 → Int → Float → [(Int, Int, Int)] → RiverParams
buildRiverFromPath seed riverIdx baseFlow path =
    let monoPath = enforceMonotonicPath path
        numWP = length monoPath
        segments = fixupSegmentContinuity $ V.fromList $
                       zipWith (buildSegFromWaypoints seed numWP baseFlow)
                               [0..] (zip monoPath (tail monoPath))
        (srcX, srcY, _) = head monoPath
        (mouthX, mouthY, _) = last monoPath
        totalFlow = case V.null segments of
            True  → baseFlow
            False → rsFlowRate (V.last segments)
    in RiverParams
        { rpSourceRegion = GeoCoord srcX srcY
        , rpMouthRegion  = GeoCoord mouthX mouthY
        , rpSegments     = segments
        , rpFlowRate     = totalFlow
        , rpMeanderSeed  = fromIntegral (hashGeo seed riverIdx 1150)
        }

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
        valleyMult = 3.0 + hashToFloatGeo h1 * 3.0
        valleyW = max (width * 3) (round (fromIntegral width * valleyMult))

        slopeDelta = abs (se - ee)
        baseDepth = max 4 (slopeDelta `div` 3 + round (flow * 3.0))
        depth = min 20 baseDepth

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
