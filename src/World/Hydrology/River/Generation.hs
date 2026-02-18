{-# LANGUAGE Strict, UnicodeSyntax #-}
module World.Hydrology.River.Generation
    ( generateRivers
    ) where

import UPrelude
import Data.Word (Word64)
import World.Base (GeoCoord(..))
import World.Types
import World.Plate (TectonicPlate, elevationAtGlobal, isBeyondGlacier, wrapGlobalU)
import World.Hydrology.Types
import World.Geology.Types
import World.Geology.Hash

-----------------------------------------------------------
-- River Generation
-----------------------------------------------------------

-- | Generate rivers from high-precipitation, high-elevation
--   regions. Each river traces a path downhill from source
--   to ocean (or terminal basin).
--
--   Algorithm:
--   1. Sample candidate source points at high elevations
--      on land plates. Hash-based placement, like generateAndRegisterN.
--   2. For each source, walk downhill in fixed-length steps,
--      picking the lowest neighbor at each step (steepest descent).
--      Uses elevationAtGlobal, same as volcanic placement.
--   3. Stop when we reach ocean (elevation < seaLevel), hit the
--      glacier boundary, or exceed max length.
--   4. Convert the walked path into RiverSegments.
--   5. Flow rate accumulates along the path (more tiles = more water).
--
--   Rivers are placed at the Age level, like meteorites.
--   Early ages get more rivers; later ages get evolution only.
generateRivers ∷ Word64 → Int → [TectonicPlate]
               → Int  -- ^ period index
               → TimelineBuildState
               → ([PersistentFeature], TimelineBuildState)
generateRivers seed worldSize plates periodIdx tbs =
    let halfTiles = (worldSize * 16) `div` 2
        -- More rivers: base 12 instead of 6, scales with area
        maxRivers = scaleCount worldSize 6
        maxAttempts = maxRivers * 8

        go attemptIdx count tbs' acc
            | attemptIdx ≥ maxAttempts = (acc, tbs')
            | count ≥ maxRivers        = (acc, tbs')
            | otherwise =
                let h1 = hashGeo seed attemptIdx 1100
                    h2 = hashGeo seed attemptIdx 1101
                    gx = hashToRangeGeo h1 (-halfTiles) (halfTiles - 1)
                    gy = hashToRangeGeo h2 (-halfTiles) (halfTiles - 1)
                in case tryGenerateRiver seed worldSize plates gx gy attemptIdx of
                    Nothing → go (attemptIdx + 1) count tbs' acc
                    Just river →
                        let (fid, tbs'') = allocFeatureId tbs'
                            pf = PersistentFeature
                                { pfId               = fid
                                , pfFeature          = HydroShape $ RiverFeature river
                                , pfActivity         = FActive
                                , pfFormationPeriod   = periodIdx
                                , pfLastActivePeriod  = periodIdx
                                , pfEruptionCount     = 1
                                , pfParentId          = Nothing
                                }
                            tbs''' = registerFeature pf tbs''
                        in go (attemptIdx + 1) (count + 1) tbs''' (pf : acc)

    in go 0 0 tbs []

-- | Try to generate a river starting near (gx, gy).
--   Lower elevation threshold: rivers start from any meaningful
--   highland, not just mountain peaks.
tryGenerateRiver ∷ Word64 → Int → [TectonicPlate] → Int
                 → Int → Int → Maybe RiverParams
tryGenerateRiver seed worldSize plates gx gy attemptIdx =
    if isBeyondGlacier worldSize gx gy
    then Nothing
    else let (srcElev, _) = elevationAtGlobal seed plates worldSize gx gy
         in if srcElev < seaLevel + 15  -- was +30, now rivers start from lower hills
            then Nothing
            else let path = walkDownhill seed worldSize plates gx gy srcElev attemptIdx
                 in if length path < 3  -- too short
                    then Nothing
                    else Just (pathToRiver seed attemptIdx path)

-- | Walk downhill from a starting point using steepest descent.
--   Variable step size and max steps based on hash, so rivers
--   have natural length variation. Longer walks = bigger rivers.
walkDownhill ∷ Word64 → Int → [TectonicPlate]
             → Int → Int → Int → Int → [(Int, Int, Int)]
             -- ^ Returns [(gx, gy, elevation)] waypoints
walkDownhill seed worldSize plates startGX startGY startElev attemptIdx =
    let -- Variable step size: 6-12 tiles per step
        stepHash = hashGeo seed attemptIdx 1180
        stepSize = 6 + hashToRangeGeo stepHash 0 6

        -- Variable max steps: 30-120, so some rivers are streams
        -- and others are continent-spanning. Weighted toward longer.
        stepsHash = hashGeo seed attemptIdx 1181
        stepsRoll = hashToFloatGeo stepsHash
        -- 30% chance short (30-50), 40% medium (50-80), 30% long (80-120)
        maxSteps = if stepsRoll < 0.3
                   then hashToRangeGeo (hashGeo seed attemptIdx 1182) 30 50
                   else if stepsRoll < 0.7
                   then hashToRangeGeo (hashGeo seed attemptIdx 1182) 50 80
                   else hashToRangeGeo (hashGeo seed attemptIdx 1182) 80 120

        -- 8-directional offsets scaled by stepSize
        dirs = [ (stepSize, 0), (-stepSize, 0), (0, stepSize), (0, -stepSize)
               , (stepSize, stepSize), (stepSize, -stepSize)
               , (-stepSize, stepSize), (-stepSize, -stepSize)
               ]

        go step curGX curGY curElev acc
            | step ≥ maxSteps = reverse acc
            | curElev ≤ seaLevel = reverse acc  -- reached ocean
            | isBeyondGlacier worldSize curGX curGY = reverse acc
            | otherwise =
                let -- Sample all 8 neighbors, find lowest
                    neighbors = map (\(dx, dy) →
                        let nx = curGX + dx
                            ny = curGY + dy
                        in if isBeyondGlacier worldSize nx ny
                           then (nx, ny, curElev + 1000)  -- avoid glacier
                           else let (e, _) = elevationAtGlobal seed plates worldSize nx ny
                                in (nx, ny, e)
                        ) dirs

                    sorted = sortByElev neighbors
                    (bestX', bestY', bestElev) = case sorted of
                        [] → (curGX, curGY, curElev)
                        [(x, y, e)] → (x, y, e)
                        ((x1, y1, e1) : (x2, y2, e2) : _) →
                            let h = hashGeo seed step (1200 + curGX + curGY)
                                wobble = hashToFloatGeo h
                            in if wobble < 0.7 ∨ e2 ≥ curElev
                               then (x1, y1, e1)
                               else (x2, y2, e2)
                    (bestX, bestY) = wrapGlobalU worldSize bestX' bestY'

                in if bestElev ≥ curElev
                   then reverse acc  -- no downhill path, we're in a basin
                   else go (step + 1) bestX bestY bestElev
                           ((bestX, bestY, bestElev) : acc)

    in go 0 startGX startGY startElev [(startGX, startGY, startElev)]

-- | Convert a walked path of waypoints into RiverParams.
--   Wider valleys and deeper channels than before.
--   Flow accumulates faster so downstream segments are bigger.
pathToRiver ∷ Word64 → Int → [(Int, Int, Int)] → RiverParams
pathToRiver seed attemptIdx path =
    let waypoints = path
        numWP = length waypoints
        segments = zipWith (makeSegment seed numWP) [0..] (zip waypoints (tail waypoints))
        (srcX, srcY, _) = head waypoints
        (mouthX, mouthY, _) = last waypoints
        totalFlow = case segments of
            [] → 0.1
            _  → rsFlowRate (last segments)
    in RiverParams
        { rpSourceRegion = GeoCoord srcX srcY
        , rpMouthRegion  = GeoCoord mouthX mouthY
        , rpSegments     = segments
        , rpFlowRate     = totalFlow
        , rpMeanderSeed  = fromIntegral (hashGeo seed attemptIdx 1150)
        }

-- | Make a single river segment between two waypoints.
--   Bigger flow, wider channels, deeper valleys than before.
makeSegment ∷ Word64 → Int → Int
            → ((Int, Int, Int), (Int, Int, Int))
            → RiverSegment
makeSegment seed totalSegs segIdx ((sx, sy, se), (ex, ey, ee)) =
    let h1 = hashGeo seed segIdx 1160
        flowAdd = 0.08 + hashToFloatGeo h1 * 0.07
        flow = fromIntegral (segIdx + 1) * flowAdd + 0.15

        rawWidth = max 2 (round (flow * 10.0))
        width = min 12 rawWidth

        valleyMult = 4.0 + hashToFloatGeo (hashGeo seed segIdx 1161) * 4.0
        valleyW = max (width * 3) (round (fromIntegral width * valleyMult))

        slopeDelta = abs (se - ee)
        baseDepth = max 4 (slopeDelta `div` 2 + round (flow * 8.0))
        depth = min 35 baseDepth

    in RiverSegment
        { rsStart       = GeoCoord sx sy
        , rsEnd         = GeoCoord ex ey
        , rsWidth       = width
        , rsValleyWidth = valleyW
        , rsDepth       = depth
        , rsFlowRate    = flow
        , rsStartElev   = se    -- elevation from walkDownhill
        , rsEndElev     = ee    -- elevation from walkDownhill
        }

-- | Sort neighbor candidates by elevation (lowest first)
sortByElev ∷ [(Int, Int, Int)] → [(Int, Int, Int)]
sortByElev [] = []
sortByElev [x] = [x]
sortByElev xs = foldr insertSorted [] xs
  where
    insertSorted x [] = [x]
    insertSorted x@(_, _, e1) (y@(_, _, e2) : ys)
        | e1 ≤ e2   = x : y : ys
        | otherwise  = y : insertSorted x ys
