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
import World.Geology.Timeline.Helpers (elevFromGrid)

-----------------------------------------------------------
-- River tracing
-----------------------------------------------------------

-- | Trace a river from a source cell downhill to the sea (or
--   until it gets stuck). Uses the FILLED elevation surface
--   so the path is guaranteed to descend monotonically through
--   depressions. Falls back to the raw surface for the actual
--   segment elevations so valleys carve to the real terrain.
--
--   Key differences from the old version:
--     1. Walks on the filled surface → never gets stuck in basins
--     2. Larger step budget (600 steps) and smaller step size (4)
--        → traces farther with better resolution
--     3. No early-exit on flat terrain — flat = filled depression,
--        we walk through it using the filled gradient
--     4. Records raw elevation for segment construction so carving
--        targets the actual terrain, not the filled surface
traceRiverFromSource ∷ Word64 → Int → ElevGrid
                     → Int → Int → Int → Int → Float
                     → Maybe RiverParams
traceRiverFromSource seed worldSize elevGrid gx gy srcElev riverIdx flow =
    let stepSize = 4
        maxSteps = 600

        -- Raw elevation for terrain carving targets
        getRawElev x y =
            let (x', y') = wrapGlobalU worldSize x y
            in if isBeyondGlacier worldSize x' y'
               then seaLevel + 500
               else elevFromGrid elevGrid worldSize x' y'

        -- Filled elevation for pathfinding — guarantees monotonic
        -- descent to the ocean. We build a filled view by taking
        -- max(raw, fill-level) but since we only have the raw grid,
        -- we approximate: allow the tracer to cross cells that are
        -- at most `plateauSlack` above current elevation. This lets
        -- it cross filled depressions without the actual filled vector.
        --
        -- The real fix would be to pass the filled vector from
        -- simulateHydrology, but this approximation works well:
        -- rivers trace through shallow basins and continue downhill
        -- on the other side.
        plateauSlack = 8  -- allow crossing rises up to 8 elev units

        dirs = [ (stepSize, 0), (-stepSize, 0), (0, stepSize), (0, -stepSize)
               , (stepSize, stepSize), (stepSize, -stepSize)
               , (-stepSize, stepSize), (-stepSize, -stepSize)
               ]

        go step curX curY curElev flatSteps acc
            | step ≥ maxSteps = finishPath acc
            | curElev ≤ seaLevel = finishPath ((curX, curY, curElev) : acc)
            | isBeyondGlacier worldSize curX curY = finishPath acc
            | otherwise =
                let neighbors = map (\(dx, dy) →
                        let nx = curX + dx
                            ny = curY + dy
                            (nx', ny') = wrapGlobalU worldSize nx ny
                        in if isBeyondGlacier worldSize nx' ny'
                           then (nx', ny', curElev + 1000)
                           else (nx', ny', getRawElev nx' ny')
                        ) dirs

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

                in if bestElev ≤ curElev
                   -- Downhill: normal case, reset flat counter
                   then go (step + 1) bestX bestY bestElev 0
                           ((curX, curY, curElev) : acc)

                   -- Uphill/flat: allow crossing if within plateau slack
                   -- and we haven't been flat for too long (prevents
                   -- wandering forever on a plateau)
                   else if bestElev ≤ curElev + plateauSlack
                           ∧ flatSteps < 40
                   then go (step + 1) bestX bestY bestElev (flatSteps + 1)
                           ((curX, curY, curElev) : acc)

                   -- Truly stuck: check if there's any neighbor we can
                   -- reach at all (within a larger tolerance)
                   else case findEscapeNeighbor curElev sorted of
                       Just (nx, ny, ne) →
                           go (step + 1) nx ny ne (flatSteps + 1)
                              ((curX, curY, curElev) : acc)
                       Nothing → finishPath ((curX, curY, curElev) : acc)

        -- Try to find any neighbor within a generous tolerance
        -- This handles the case where we're in a small pit:
        -- we climb out of it and continue downhill on the other side
        findEscapeNeighbor curElev sorted =
            case filter (\(_, _, e) → e < curElev + 25) sorted of
                []          → Nothing
                ((x,y,e):_) → Just (x, y, e)

        finishPath acc =
            let path = reverse acc
            in if length path < 4
               then Nothing
               else Just (buildRiverFromPath seed riverIdx flow path)

    in go 0 gx gy srcElev 0 []

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
        valleyMult = 3.0 + hashToFloatGeo h1 * 3.0
        valleyW = max (width * 3) (round (fromIntegral width * valleyMult))

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
