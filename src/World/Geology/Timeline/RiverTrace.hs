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
--
--   Uses gradient-based continuous direction with momentum for
--   natural, curving paths instead of 8-directional grid stepping.
traceRiverFromSource ∷ Word64 → Int → ElevGrid → VU.Vector Int
                     → Int → Int → Int → Int → Float
                     → Maybe RiverParams
traceRiverFromSource seed worldSize elevGrid filledElev gx gy srcElev riverIdx flow =
    let stepSize   = 4       -- tiles per step (≥ half grid spacing for progress)
        maxSteps   = 3000
        sampleDist = 10      -- sample gradient at this distance (> grid spacing)
        maxRetries = 4 ∷ Int -- retries with smaller steps on uphill
        maxFlat    = 30 ∷ Int -- max consecutive flat steps before giving up

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

        -- Compute downhill gradient by sampling in 16 directions
        -- Returns (dx, dy) pointing downhill, normalized
        computeGradient curX curY curFE =
            let numDirs = 16 ∷ Int
                angStep = 2.0 * π / fromIntegral numDirs
                samples = [ let a = fromIntegral i * angStep
                                nx = curX + round (cos a * fromIntegral sampleDist)
                                ny = curY + round (sin a * fromIntegral sampleDist)
                                fe = getFilledElev nx ny
                                drop' = fromIntegral (curFE - fe) ∷ Float
                            in (cos a * drop', sin a * drop')
                          | i ← [0 .. numDirs - 1]
                          ]
                -- Sum all weighted directions (steeper drops contribute more)
                (gdx, gdy) = foldl' (\(ax, ay) (bx, by) → (ax + bx, ay + by))
                                    (0.0 ∷ Float, 0.0 ∷ Float) samples
                glen = sqrt (gdx * gdx + gdy * gdy)
            in if glen < 0.001
               then (0.0, 0.0)  -- flat or local minimum
               else (gdx / glen, gdy / glen)

        -- Main tracing loop with continuous direction and momentum.
        -- dirX/dirY is the current movement direction (unit vector).
        -- flatCount tracks consecutive steps with no elevation change;
        -- the river continues through flat areas (common on filled
        -- depressions and coastal plains) until maxFlat is reached.
        go step curX curY curFilledElev dirX dirY flatCount acc
            | step ≥ maxSteps = finishPath acc curX curY
            | isBeyondGlacier worldSize curX curY = finishPath acc curX curY
            | otherwise =
                let rawElev = getRawElev curX curY
                    -- Allow a few steps past sea level to form a proper mouth
                    pastSea = rawElev ≤ seaLevel
                    recentSea = case acc of
                        ((_,_,e1):(_,_,e2):(_,_,e3):_) →
                            e1 ≤ seaLevel ∧ e2 ≤ seaLevel ∧ e3 ≤ seaLevel
                        _ → False
                in if pastSea ∧ recentSea
                   then finishPath acc curX curY
                   else
                let (gdx, gdy) = computeGradient curX curY curFilledElev

                in if gdx == 0.0 ∧ gdy == 0.0
                   -- No gradient: continue in current direction on
                   -- flat terrain (common in filled depressions).
                   -- Only give up after maxFlat consecutive flat steps.
                   then if flatCount < maxFlat
                        then let nextX = curX + round (dirX * fromIntegral stepSize)
                                 nextY = curY + round (dirY * fromIntegral stepSize)
                                 (nextX', nextY') = wrapGlobalU worldSize nextX nextY
                             in go (step + 1) nextX' nextY' curFilledElev
                                   dirX dirY (flatCount + 1)
                                   ((curX, curY, rawElev) : acc)
                        else finishPath ((curX, curY, rawElev) : acc) curX curY
                   else
                   let -- Hash-based lateral perturbation for natural variation
                       h1 = hashGeo seed step (1200 + abs curX `mod` 200
                                                     + abs curY `mod` 200)
                       noise = (hashToFloatGeo h1 - 0.5) * 0.6
                       -- Perpendicular to gradient
                       perpGX = -gdy
                       perpGY = gdx
                       -- Noisy gradient direction
                       noisyDX = gdx + perpGX * noise
                       noisyDY = gdy + perpGY * noise
                       noisyLen = sqrt (noisyDX * noisyDX + noisyDY * noisyDY)
                       nGDX = if noisyLen < 0.001 then gdx else noisyDX / noisyLen
                       nGDY = if noisyLen < 0.001 then gdy else noisyDY / noisyLen

                       -- Blend momentum (current direction) with gradient.
                       -- Momentum creates smooth curves; gradient ensures
                       -- we keep going downhill.
                       momentum = 0.3 ∷ Float
                       blendDX = dirX * momentum + nGDX * (1.0 - momentum)
                       blendDY = dirY * momentum + nGDY * (1.0 - momentum)
                       bLen = sqrt (blendDX * blendDX + blendDY * blendDY)
                       finalDX = if bLen < 0.001 then nGDX else blendDX / bLen
                       finalDY = if bLen < 0.001 then nGDY else blendDY / bLen

                   in tryStep step curX curY curFilledElev
                              finalDX finalDY rawElev flatCount acc 0

        -- Try stepping forward; on uphill, retry with smaller steps
        -- or pure gradient direction before giving up.
        tryStep step curX curY curFE dirX dirY rawElev fCount acc retry
            | retry > maxRetries =
                finishPath ((curX, curY, rawElev) : acc) curX curY
            | otherwise =
                let curStep = max 1 (stepSize - retry)
                    nextX = curX + round (dirX * fromIntegral curStep)
                    nextY = curY + round (dirY * fromIntegral curStep)
                    (nextX', nextY') = wrapGlobalU worldSize nextX nextY
                    nextFE = getFilledElev nextX' nextY'
                    -- Allow equal elevation (flat terrain) — only reject
                    -- strictly uphill. Flat areas are traversed using the
                    -- flatCount limit instead.
                in if nextFE ≤ curFE
                   then let newFlat = if nextFE == curFE then fCount + 1 else 0
                        in go (step + 1) nextX' nextY' nextFE
                              dirX dirY newFlat
                              ((curX, curY, rawElev) : acc)
                   -- On first uphill: try pure gradient (no momentum)
                   else if retry == 0
                   then let (gdx, gdy) = computeGradient curX curY curFE
                        in if gdx == 0.0 ∧ gdy == 0.0
                           then tryStep step curX curY curFE
                                        dirX dirY rawElev fCount acc (retry + 1)
                           else
                           let pureX = curX + round (gdx * fromIntegral stepSize)
                               pureY = curY + round (gdy * fromIntegral stepSize)
                               (pureX', pureY') = wrapGlobalU worldSize pureX pureY
                               pureFE = getFilledElev pureX' pureY'
                           in if pureFE ≤ curFE
                              then let newFlat = if pureFE == curFE then fCount + 1 else 0
                                   in go (step + 1) pureX' pureY' pureFE
                                         gdx gdy newFlat
                                         ((curX, curY, rawElev) : acc)
                              else tryStep step curX curY curFE
                                           dirX dirY rawElev fCount acc (retry + 1)
                   else tryStep step curX curY curFE
                                dirX dirY rawElev fCount acc (retry + 1)

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

        -- Initial direction: use gradient at source
        (initDX, initDY) = let (gx', gy') = computeGradient gx gy (getFilledElev gx gy)
                           in if gx' == 0.0 ∧ gy' == 0.0
                              then (0.0, 1.0)  -- default: south
                              else (gx', gy')

    in go 0 gx gy (getFilledElev gx gy) initDX initDY 0 []


buildRiverFromPath ∷ Word64 → Int → Float → [(Int, Int, Int)] → RiverParams
buildRiverFromPath seed riverIdx baseFlow path =
    let monoPath = enforceMonotonicPath path
        -- Decimate the dense path to ~every 8th point, keeping first and last.
        -- Longer segments prevent blobby junction circles on the zoom map.
        -- Each retained point is ~24 tiles apart (8 steps × 3 tiles/step).
        decimated0 = decimatePath 8 monoPath
        -- Clamp the last point's elevation to sea level if the river
        -- reaches the coast, ensuring smooth ocean transition.
        decimated = case reverse decimated0 of
            (lx, ly, le) : rest
                | le ≤ seaLevel + 3 → reverse ((lx, ly, seaLevel) : rest)
            _ → decimated0
        numWP = length decimated
        segments = fixupSegmentContinuity $ V.fromList $
                       zipWith (buildSegFromWaypoints seed numWP baseFlow)
                               [0..] (zip decimated (tail decimated))
        (srcX, srcY, _) = head decimated
        (mouthX, mouthY, _) = last decimated
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
        _  → if last picked == lastPt then picked
             else picked <> [lastPt]
  where
    go _ [] = []
    go i (x:rest)
        | i `mod` n == 0 = x : go (i + 1) rest
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
        valleyMult = 3.0 + hashToFloatGeo h1 * 3.0

        slopeDelta = abs (se - ee)
        -- Depth capped at 10 tiles — keeps valleys wider than deep
        baseDepth = max 3 (slopeDelta `div` 4 + round (flow * 2.0))
        depth = min 10 baseDepth
        -- Valley must be wide enough that walls aren't steep cliffs.
        -- Minimum half-width = depth × 4 → full width = depth × 8.
        minValleyW = depth * 8
        rawValleyW = max (width * 3) (round (fromIntegral width * valleyMult))
        valleyW = max minValleyW (min 96 rawValleyW)

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
