{-# OPTIONS_GHC -fprof-auto #-}
{-# LANGUAGE Strict, UnicodeSyntax #-}
module World.Geology.Timeline.RiverTrace
    ( traceRiverFromSource
    ) where
import UPrelude
import Data.Word (Word64)
import qualified Data.Vector as V
import World.Base (GeoCoord(..))
import World.Types
import World.Fluids (fixupSegmentContinuity)
import World.Plate (isBeyondGlacier, wrapGlobalU)
import World.Geology.Hash
import World.Hydrology.Types (RiverParams(..), RiverSegment(..))
import World.Hydrology.Simulation (ElevGrid(..))
import World.Geology.Timeline.Helpers (elevFromGrid)

-----------------------------------------------------------
-- River tracing
-----------------------------------------------------------

traceRiverFromSource ∷ Word64 → Int → ElevGrid
                     → Int → Int → Int → Int → Float
                     → Maybe RiverParams
traceRiverFromSource seed worldSize elevGrid gx gy srcElev riverIdx flow =
    let stepSize = 6
        maxSteps = 300
        halfTiles = (worldSize * 16) `div` 2

        -- Elevation lookup always wraps for correct sampling
        getElev x y =
            let (x', y') = wrapGlobalU worldSize x y
            in if isBeyondGlacier worldSize x' y'
               then seaLevel + 500
               else elevFromGrid elevGrid worldSize x y
               -- NOTE: elevFromGrid now handles u/v wrapping internally,
               -- so we pass the raw (x, y) here, not (x', y').

        -- Glacier check needs wrapped coords
        isGlacier x y =
            let (x', y') = wrapGlobalU worldSize x y
            in isBeyondGlacier worldSize x' y'

        dirs = [ (stepSize, 0), (-stepSize, 0), (0, stepSize), (0, -stepSize)
               , (stepSize, stepSize), (stepSize, -stepSize)
               , (-stepSize, stepSize), (-stepSize, -stepSize)
               ]

        go step curX curY curElev acc
            | step ≥ maxSteps = finishPath acc
            | curElev ≤ seaLevel = finishPath ((curX, curY, curElev) : acc)
            | isGlacier curX curY = finishPath acc
            | otherwise =
                let neighbors = map (\(dx, dy) →
                        let nx = curX + dx  -- UNWRAPPED
                            ny = curY + dy  -- UNWRAPPED
                        in if isGlacier nx ny
                           then (nx, ny, curElev + 1000)
                           else (nx, ny, getElev nx ny)
                        ) dirs
                    -- FIX: neighbors now store UNWRAPPED (nx, ny).
                    -- wrappedDeltaUV in the carver will handle wrapping.

                    sorted = sortByElevTriple neighbors
                    (bestX, bestY, bestElev) = case sorted of
                        [] → (curX, curY, curElev)
                        [(x, y, e)] → (x, y, e)
                        ((x1, y1, e1) : (x2, y2, e2) : _) →
                            let h = hashGeo seed step
                                    (1200 + abs curX `mod` 100 + abs curY `mod` 100)
                                wobble = hashToFloatGeo h
                            in if wobble < 0.75 ∨ e2 ≥ curElev
                               then (x1, y1, e1)
                               else (x2, y2, e2)

                in if bestElev ≥ curElev
                   then let lowestNeighbor = case sorted of
                                [] → Nothing
                                ((x, y, e):_) →
                                    if e < curElev + 15
                                    then Just (x, y, e)
                                    else Nothing
                        in case lowestNeighbor of
                            Just (nx, ny, ne) →
                                go (step + 1) nx ny ne
                                   ((curX, curY, curElev) : acc)
                            Nothing → finishPath acc
                   else go (step + 1) bestX bestY bestElev
                           ((curX, curY, curElev) : acc)

        finishPath acc =
            let path = reverse acc
            in if length path < 6
               then Nothing
               else Just (buildRiverFromPath seed riverIdx flow path)

    in go 0 gx gy srcElev []

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

        rawWidth = max 2 (round (flow * 6.0))
        width = min 12 rawWidth

        h1 = hashGeo seed segIdx 1161
        -- FIX 4: Cap valley multiplier to reduce cliff severity
        valleyMult = 3.0 + hashToFloatGeo h1 * 2.0   -- was * 3.0
        valleyW = min 48 $ max (width * 3) (round (fromIntegral width * valleyMult))
        -- was min 72 (unbounded above), now capped at 48

        slopeDelta = abs (se - ee)
        baseDepth = max 2 (slopeDelta `div` 3 + round (flow * 2.0))
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
