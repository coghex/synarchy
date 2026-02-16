{-# LANGUAGE Strict, UnicodeSyntax #-}
module World.Hydrology.River
    ( generateRivers
    , evolveRiver
    , applyRiverCarve
    , applyRiverEvolution
    ) where

import UPrelude
import Data.Word (Word64)
import qualified Data.HashMap.Strict as HM
import World.Base (GeoCoord(..), GeoFeatureId(..))
import World.Types
import World.Material (matSandstone, matShale, matLimestone, unMaterialId)
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
        maxRivers = scaleCount worldSize 12
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

-----------------------------------------------------------
-- River Evolution
-----------------------------------------------------------

-- | Evolve a river feature for one age.
--   Same fold pattern as evolvePointFeature / evolveGlacier.
--
--   Rivers are precipitation-driven, but we don't need to
--   query climate directly — the evolution probabilities
--   capture the range of behaviors.
--
--   Probability table (for Flowing rivers):
--     15% → Branch: new tributary splits off at a random segment
--     10% → Dam: blockage forms, lake appears upstream
--      5% → Capture: steal headwaters from a nearby river
--      5% → Dry up: river goes seasonal (or dies if already seasonal)
--     15% → Meander: path segments shift laterally
--     10% → Deepen: valley carves deeper (increased depth on all segments)
--     40% → Continue: no change, river keeps flowing
--
evolveRiver ∷ Word64 → Int
            → ([GeoEvent], TimelineBuildState)
            → PersistentFeature
            → ([GeoEvent], TimelineBuildState)
evolveRiver seed periodIdx (events, tbs) pf =
    let fid = pfId pf
        GeoFeatureId fidInt = fid
        h1 = hashGeo seed fidInt 800
        roll = hashToFloatGeo h1
    in case pfActivity pf of

        ---------------------------------------------------
        -- Flowing river
        ---------------------------------------------------
        FActive →

            if roll < 0.15
            -- 15%: Branch — new tributary joins this river
            then let river = getRiverParams pf
                     numSegs = length (rpSegments river)
                 in if numSegs < 2
                    then (events, tbs)  -- too short to branch
                    else
                    let (childId, tbs') = allocFeatureId tbs
                        -- Pick a segment to branch from
                        h2 = hashGeo seed fidInt 801
                        h3 = hashGeo seed fidInt 802
                        h4 = hashGeo seed fidInt 803
                        h5 = hashGeo seed fidInt 804
                        h6 = hashGeo seed fidInt 805
                        h7 = hashGeo seed fidInt 806
                        h8 = hashGeo seed fidInt 807
                        branchSegIdx = hashToRangeGeo h2 1 (numSegs - 1)
                        branchSeg = rpSegments river !! min branchSegIdx (numSegs - 1)
                        GeoCoord bx by = rsStart branchSeg

                        -- Tributary comes from a different direction
                        -- Compute direction of the parent segment
                        GeoCoord bex bey = rsEnd branchSeg
                        segDX = fromIntegral (bex - bx) ∷ Float
                        segDY = fromIntegral (bey - by) ∷ Float
                        segAngle = atan2 segDY segDX
                        -- Branch comes from 40-80° off to one side
                        side = if hashToFloatGeo h3 < 0.5 then 1.0 else -1.0
                        branchAngle = segAngle + side * (0.7 + hashToFloatGeo h4 * 0.7)
                        -- Tributary source is upstream of the branch point
                        branchLen = hashToRangeGeo h5 15 50
                        srcX = bx - round (fromIntegral branchLen * cos branchAngle)
                        srcY = by - round (fromIntegral branchLen * sin branchAngle)

                        -- Build tributary segments (3-5 segments from source to branch point)
                        numTribSegs = hashToRangeGeo h6 2 4
                        tribSegs = buildTributarySegments seed fidInt
                                       srcX srcY bx by numTribSegs

                        tributaryParams = RiverParams
                            { rpSourceRegion = GeoCoord srcX srcY
                            , rpMouthRegion  = GeoCoord bx by
                            , rpSegments     = tribSegs
                            , rpFlowRate     = rsFlowRate branchSeg * 0.4
                            , rpMeanderSeed  = fromIntegral (hashGeo seed fidInt 808)
                            }

                        childPf = PersistentFeature
                            { pfId               = childId
                            , pfFeature          = HydroShape $ RiverFeature tributaryParams
                            , pfActivity         = FActive
                            , pfFormationPeriod   = periodIdx
                            , pfLastActivePeriod  = periodIdx
                            , pfEruptionCount     = 1
                            , pfParentId          = Just fid
                            }

                        evt = HydroModify fid (RiverBranch
                                (GeoCoord bx by) branchAngle branchLen childId)

                        -- Increase parent river's flow downstream of branch point
                        -- (more water coming in from tributary)
                        tbs'' = registerFeature childPf tbs'
                        tbs''' = updateFeature fid
                            (\p → p { pfEruptionCount = pfEruptionCount p + 1
                                     , pfLastActivePeriod = periodIdx }) tbs''
                    in (evt : events, tbs''')

            else if roll < 0.25
            -- 10%: Dam — blockage creates a lake upstream
            then let river = getRiverParams pf
                     numSegs = length (rpSegments river)
                 in if numSegs < 2
                    then (events, tbs)
                    else
                    let (lakeId, tbs') = allocFeatureId tbs
                        h2 = hashGeo seed fidInt 810
                        h3 = hashGeo seed fidInt 811
                        h4 = hashGeo seed fidInt 812
                        h5 = hashGeo seed fidInt 813
                        -- Dam forms at a random segment
                        damSegIdx = hashToRangeGeo h2 0 (numSegs - 2)
                        damSeg = rpSegments river !! min damSegIdx (numSegs - 1)
                        GeoCoord dx' dy' = rsEnd damSeg
                        damHeight = hashToRangeGeo h3 5 20

                        -- Lake forms just upstream of the dam
                        lakeParams = LakeParams
                            { lkCenter  = GeoCoord dx' dy'
                            , lkRadius  = hashToRangeGeo h4 5 15
                            , lkSurface = damHeight
                            , lkDepth   = hashToRangeGeo h5 3 12
                            , lkSource  = DammedRiver fid
                            }

                        lakePf = PersistentFeature
                            { pfId               = lakeId
                            , pfFeature          = HydroShape $ LakeFeature lakeParams
                            , pfActivity         = FActive
                            , pfFormationPeriod   = periodIdx
                            , pfLastActivePeriod  = periodIdx
                            , pfEruptionCount     = 0
                            , pfParentId          = Just fid
                            }

                        evt = HydroModify fid (RiverDam
                                (GeoCoord dx' dy') lakeId damHeight)

                        -- Mark river as dammed
                        tbs'' = registerFeature lakePf tbs'
                        tbs''' = updateFeature fid
                            (\p → p { pfActivity = FDormant  -- Dammed = dormant
                                     , pfLastActivePeriod = periodIdx }) tbs''
                    in (evt : events, tbs''')

            else if roll < 0.30
            -- 5%: Capture — steal headwaters from another river
            -- (simplified: we just extend the source upstream)
            then let river = getRiverParams pf
                     h2 = hashGeo seed fidInt 820
                     h3 = hashGeo seed fidInt 821
                     h4 = hashGeo seed fidInt 822
                     GeoCoord sx sy = rpSourceRegion river
                     -- Extend source in a random uphill direction
                     extAngle = hashToFloatGeo h2 * 2.0 * π
                     extLen = hashToRangeGeo h3 10 30
                     newSrcX = sx + round (fromIntegral extLen * cos extAngle)
                     newSrcY = sy + round (fromIntegral extLen * sin extAngle)

                     -- Add a new segment at the head
                     newSeg = RiverSegment
                         { rsStart       = GeoCoord newSrcX newSrcY
                         , rsEnd         = GeoCoord sx sy
                         , rsWidth       = 2  -- narrow headwaters
                         , rsValleyWidth = hashToRangeGeo h4 6 12
                         , rsDepth       = 3
                         , rsFlowRate    = 0.1
                         , rsStartElev   = 0
                         , rsEndElev     = 0
                         }

                     newRiver = river
                         { rpSourceRegion = GeoCoord newSrcX newSrcY
                         , rpSegments     = newSeg : rpSegments river
                         , rpFlowRate     = rpFlowRate river + 0.1
                         }

                     -- TODO: capture other river
                     evt = HydroModify fid (RiverCapture fid (GeoCoord newSrcX newSrcY))
                     tbs' = updateFeature fid
                         (\p → p { pfFeature = HydroShape $ RiverFeature newRiver
                                  , pfEruptionCount = pfEruptionCount p + 1
                                  , pfLastActivePeriod = periodIdx }) tbs
                 in (evt : events, tbs')

            else if roll < 0.35
            -- 5%: Dry up — river goes seasonal
            then let evt = HydroModify fid RiverDryUp
                     tbs' = updateFeature fid
                         (\p → p { pfActivity = FDormant
                                  , pfLastActivePeriod = periodIdx }) tbs
                 in (evt : events, tbs')

            else if roll < 0.50
            -- 15%: Meander — shift segments laterally
            then let h2 = hashGeo seed fidInt 830
                     meanderAmt = 0.2 + hashToFloatGeo h2 * 0.6
                     evt = HydroModify fid (RiverMeander
                             (fromIntegral (hashGeo seed fidInt 831)) meanderAmt)

                     -- Apply meander: shift each segment's endpoints
                     -- perpendicular to flow direction
                     river = getRiverParams pf
                     newSegs = meanderSegments seed fidInt meanderAmt (rpSegments river)
                     newRiver = river { rpSegments = newSegs }

                     tbs' = updateFeature fid
                         (\p → p { pfFeature = HydroShape $ RiverFeature newRiver
                                  , pfLastActivePeriod = periodIdx }) tbs
                 in (evt : events, tbs')

            else if roll < 0.60
            -- 10%: Deepen — valley cuts deeper on all segments
            then let h2 = hashGeo seed fidInt 835
                     deepenAmount = hashToRangeGeo h2 2 8
                     river = getRiverParams pf
                     newSegs = map (\seg → seg
                         { rsDepth = rsDepth seg + deepenAmount
                         , rsWidth = min (rsWidth seg + 1) 10
                         , rsValleyWidth = rsValleyWidth seg + 2
                         }) (rpSegments river)
                     newRiver = river { rpSegments = newSegs }

                     evt = HydroEvent (RiverFeature newRiver)  -- re-emit with deeper params

                     tbs' = updateFeature fid
                         (\p → p { pfFeature = HydroShape $ RiverFeature newRiver
                                  , pfEruptionCount = pfEruptionCount p + 1
                                  , pfLastActivePeriod = periodIdx }) tbs
                 in (evt : events, tbs')

            else
            -- 40%: Continue flowing, no change
            (events, tbs)

        ---------------------------------------------------
        -- Dormant (seasonal or dammed) river
        ---------------------------------------------------
        FDormant →
            if roll < 0.30
            -- 30%: Reactivate — dam breaks or rains return
            then let river = getRiverParams pf
                     h2 = hashGeo seed fidInt 840
                     -- Slightly deeper from the reactivation surge
                     deepenAmt = hashToRangeGeo h2 1 4
                     newSegs = map (\seg → seg { rsDepth = rsDepth seg + deepenAmt })
                                   (rpSegments river)
                     newRiver = river { rpSegments = newSegs }

                     evt = HydroEvent (RiverFeature newRiver)

                     tbs' = updateFeature fid
                         (\p → p { pfActivity = FActive
                                  , pfFeature = HydroShape $ RiverFeature newRiver
                                  , pfEruptionCount = pfEruptionCount p + 1
                                  , pfLastActivePeriod = periodIdx }) tbs
                 in (evt : events, tbs')

            else if roll < 0.50
            -- 20%: Die completely — seasonal river dries out
            then let evt = HydroModify fid RiverDryUp
                     tbs' = updateFeature fid
                         (\p → p { pfActivity = FExtinct }) tbs
                 in (evt : events, tbs')

            else
            -- 50%: Stay dormant
            (events, tbs)

        ---------------------------------------------------
        -- Dead rivers don't evolve (but their valleys persist)
        ---------------------------------------------------
        FExtinct   → (events, tbs)
        FCollapsed → (events, tbs)

-----------------------------------------------------------
-- Meander Application
-----------------------------------------------------------

-- | Shift segment endpoints perpendicular to flow direction.
--   Creates natural-looking bends in the river over time.
--   Each segment shifts independently using hash-based randomness
--   so the result is deterministic.
meanderSegments ∷ Word64 → Int → Float → [RiverSegment] → [RiverSegment]
meanderSegments seed fidInt amount segs =
    zipWith (meanderOne seed fidInt amount) [0..] segs

meanderOne ∷ Word64 → Int → Float → Int → RiverSegment → RiverSegment
meanderOne seed fidInt amount segIdx seg =
    let GeoCoord sx sy = rsStart seg
        GeoCoord ex ey = rsEnd seg
        -- Direction of flow
        dx = fromIntegral (ex - sx) ∷ Float
        dy = fromIntegral (ey - sy) ∷ Float
        len = sqrt (dx * dx + dy * dy)
    in if len < 0.001 then seg
    else
    let -- Perpendicular direction
        perpX = -dy / len
        perpY = dx / len
        -- Hash determines shift direction and magnitude
        h1 = hashGeo seed (fidInt + segIdx) 850
        h2 = hashGeo seed (fidInt + segIdx) 851
        shiftStart = (hashToFloatGeo h1 - 0.5) * 2.0 * amount * len * 0.2
        shiftEnd   = (hashToFloatGeo h2 - 0.5) * 2.0 * amount * len * 0.2
        -- Shift the start point (but keep the very first segment source fixed)
        newSX = sx + round (perpX * shiftStart)
        newSY = sy + round (perpY * shiftStart)
        newEX = ex + round (perpX * shiftEnd)
        newEY = ey + round (perpY * shiftEnd)
    in seg { rsStart = GeoCoord newSX newSY
           , rsEnd   = GeoCoord newEX newEY
           }

-----------------------------------------------------------
-- Tributary Segment Builder
-----------------------------------------------------------

-- | Build segments for a tributary flowing from (srcX,srcY)
--   to the branch point (bx,by), divided into numSegs pieces.
--   Each waypoint is offset slightly from the straight line
--   for natural look.
buildTributarySegments ∷ Word64 → Int → Int → Int → Int → Int → Int
                       → [RiverSegment]
buildTributarySegments seed fidInt srcX srcY bx by numSegs =
    let -- Interpolate waypoints along the line with hash-based offsets
        waypoints = [ let t = fromIntegral i / fromIntegral numSegs ∷ Float
                          baseX = fromIntegral srcX + t * fromIntegral (bx - srcX)
                          baseY = fromIntegral srcY + t * fromIntegral (by - srcY)
                          -- Perpendicular offset for natural curvature
                          dx = fromIntegral (bx - srcX) ∷ Float
                          dy = fromIntegral (by - srcY) ∷ Float
                          len = sqrt (dx * dx + dy * dy)
                          perpX = if len > 0.001 then -dy / len else 0.0
                          perpY = if len > 0.001 then  dx / len else 0.0
                          h = hashGeo seed (fidInt + i) 860
                          offset = (hashToFloatGeo h - 0.5) * len * 0.15
                      in ( round (baseX + perpX * offset)
                         , round (baseY + perpY * offset) )
                    | i ← [0 .. numSegs]
                    ]
        pairs = zip waypoints (tail waypoints)
    in zipWith (\segI ((wx1, wy1), (wx2, wy2)) →
        let flow = 0.1 + fromIntegral segI * 0.05
            w = max 2 (round (flow * 6.0))
        in RiverSegment
            { rsStart       = GeoCoord wx1 wy1
            , rsEnd         = GeoCoord wx2 wy2
            , rsWidth       = w
            , rsValleyWidth = w * 3
            , rsDepth       = max 3 (round (flow * 8.0))
            , rsFlowRate    = flow
            , rsStartElev   = 0
            , rsEndElev     = 0
            }
        ) [0..] pairs

-----------------------------------------------------------
-- River Carving (pure GeoModification)
-----------------------------------------------------------

-- | Apply a river's valley carving to a single column.
--   Called from applyGeoEvent during chunk generation,
--   just like applyVolcanicFeature.
--
--   Uses the same line-projection math as applyFissure:
--   for each segment, project (gx,gy) onto the segment line,
--   compute perpendicular distance, apply carving profile.
--
--   V-shaped valley profile:
--
--        \         /
--         \       /     <- valley walls (linear slope)
--          \     /
--           \   /       <- V-shape narrows to channel
--            \_/        <- channel floor (flat, width = rsWidth)
--
--   Multiple segments: we find the closest segment and apply
--   that segment's carving. Segments overlap slightly at
--   waypoints for continuity.
--
--   The carve produces NEGATIVE gmElevDelta.
--   Channel floor gets sandstone (river sediment / alluvium).
--   Valley walls get no material override (exposed bedrock).
-----------------------------------------------------------
-- River Carving (pure GeoModification)
-----------------------------------------------------------
applyRiverCarve ∷ RiverParams → Int → Int → Int → Int → GeoModification
applyRiverCarve river worldSize gx gy baseElev =
    let segments = rpSegments river
        -- Valley carving pass
        carveResults = map (carveFromSegment worldSize gx gy (rpMeanderSeed river))
                           segments
        bestCarve = foldl' pickDeepest noModification carveResults

        -- Delta deposit pass (at the river mouth)
        deltaDeposit = computeDeltaDeposit river worldSize gx gy

    in if gmElevDelta bestCarve < 0
       then bestCarve  -- carving wins — we're in the valley
       else deltaDeposit  -- outside the valley, check for delta deposit

-- | Compute the carving modification from a single river segment.
--   FIXED: Channel floor now gets sandstone with intrusion depth
--   proportional to the alluvial fill. A 10-tile deep channel
--   has ~3-5 tiles of alluvium at the bottom, not just 1 surface tile.
--   This creates visible sedimentary layers when you dig down.
carveFromSegment ∷ Int → Int → Int → Word64 → RiverSegment → GeoModification
carveFromSegment worldSize gx gy meanderSeed seg =
    let GeoCoord sx sy = rsStart seg
        GeoCoord ex ey = rsEnd seg

        px = fromIntegral (wrappedDeltaXGeo worldSize gx sx) ∷ Float
        py = fromIntegral (gy - sy) ∷ Float

        fdx = fromIntegral (wrappedDeltaXGeo worldSize ex sx) ∷ Float
        fdy = fromIntegral (ey - sy) ∷ Float
        segLen = sqrt (fdx * fdx + fdy * fdy)

    in if segLen < 0.001
       then noModification
       else
       let nx = fdx / segLen
           ny = fdy / segLen

           dot = px * nx + py * ny
           alongT = dot / segLen

           perpX = px - dot * nx
           perpY = py - dot * ny
           perpDist = sqrt (perpX * perpX + perpY * perpY)

           channelHalfW = fromIntegral (rsWidth seg) / 2.0 ∷ Float
           valleyHalfW  = fromIntegral (rsValleyWidth seg) / 2.0 ∷ Float
           depth        = fromIntegral (rsDepth seg) ∷ Float
           flow         = rsFlowRate seg

           endTaper = min 1.0 (min ((alongT + 0.05) * 8.0)
                                   ((1.05 - alongT) * 8.0))

           meanderFreq = 2.0 * π / segLen * 1.5
           meanderPhase = fromIntegral (fromIntegral meanderSeed `mod` (1000 ∷ Int)) * 0.001 * 2.0 * π
           meanderOffset = sin (alongT * segLen * meanderFreq / segLen * 2.0 * π + meanderPhase)
                         * valleyHalfW * 0.2 * min 1.0 flow
           effectivePerpDist = abs (perpDist - meanderOffset)

       in if alongT < -0.05 ∨ alongT > 1.05 ∨ effectivePerpDist > valleyHalfW
          then noModification

          -- Channel floor: alluvial sediment fill
          else if effectivePerpDist < channelHalfW
          then let channelT = effectivePerpDist / channelHalfW
                   channelProfile = 1.0 - channelT * 0.1
                   carve = round (depth * channelProfile * endTaper)
                   -- Alluvial fill: 30-50% of channel depth is sediment
                   -- Deeper channels accumulate more alluvium
                   -- This creates 3-5 tiles of sandstone at the bottom
                   -- of a 10-tile deep channel
                   alluviumDepth = max 1 (round (depth * 0.4 * endTaper))
               in if carve ≤ 0
                  then noModification
                  else GeoModification
                      { gmElevDelta        = negate carve
                      , gmMaterialOverride = Just (unMaterialId matSandstone)
                      , gmIntrusionDepth   = alluviumDepth
                      }

          -- Valley walls: V-shaped, exposed bedrock (no change here)
          else let wallT = (effectivePerpDist - channelHalfW)
                         / (valleyHalfW - channelHalfW)
                   wallProfile = max 0.0 (1.0 - wallT)
                   carve = round (depth * wallProfile * endTaper * 0.7)
               in if carve ≤ 0
                  then noModification
                  else GeoModification (negate carve) Nothing 0

-- | Compute sediment deposit at the river mouth (delta fan).
--   Creates a fan-shaped deposit of sandstone/shale downstream
--   of where the river meets the ocean or terminal basin.
--
--   The delta is a semicircular mound centered on the mouth,
--   with thickness proportional to total flow. Larger rivers
--   produce bigger, thicker deltas.
--
--   Profile: thickest at the mouth, thinning radially outward.
--   Shape: semicircle opening in the downstream direction.
computeDeltaDeposit ∷ RiverParams → Int → Int → Int → GeoModification
computeDeltaDeposit river worldSize gx gy =
    let segs = rpSegments river
    in if null segs then noModification
    else
    let -- Get the last segment to determine delta direction
        lastSeg = last segs
        GeoCoord mx my = rsEnd lastSeg  -- mouth position
        GeoCoord px py = rsStart lastSeg  -- second-to-last waypoint

        -- Delta direction: continuation of the last segment's flow
        flowDX = fromIntegral (wrappedDeltaXGeo worldSize mx px) ∷ Float
        flowDY = fromIntegral (my - py) ∷ Float
        flowLen = sqrt (flowDX * flowDX + flowDY * flowDY)

    in if flowLen < 0.001 then noModification
    else
    let flowNX = flowDX / flowLen
        flowNY = flowDY / flowLen

        -- Vector from mouth to query point
        dx = fromIntegral (wrappedDeltaXGeo worldSize gx mx) ∷ Float
        dy = fromIntegral (gy - my) ∷ Float
        dist = sqrt (dx * dx + dy * dy)

        -- Delta radius scales with total flow
        totalFlow = rpFlowRate river
        deltaRadius = totalFlow * 25.0 + 8.0  -- 8-33 tiles depending on flow
        -- Max deposit height scales with flow
        maxDeposit = max 2.0 (totalFlow * 6.0)  -- 2-10 tiles

    in if dist > deltaRadius
       then noModification
       else
       let -- Check that we're in the downstream semicircle
           -- Dot product of (mouth→point) with flow direction
           dotFlow = dx * flowNX + dy * flowNY

           -- Allow slight upstream spread (alluvial backfill)
           -- but mainly downstream
           spreadAngle = if dotFlow > -deltaRadius * 0.2
                         then 1.0  -- downstream or near mouth
                         else 0.0  -- too far upstream

       in if spreadAngle < 0.5
          then noModification
          else
          let -- Radial falloff: thickest at mouth, thins to edge
              t = dist / deltaRadius
              -- Fan shape: wider perpendicular to flow direction
              perpDist = abs (dx * flowNY - dy * flowNX)  -- cross product magnitude
              perpT = perpDist / (deltaRadius * 1.2)  -- allow slight widening

              -- Combined profile: radial × perpendicular fade
              profile = max 0.0 ((1.0 - t) * (1.0 - min 1.0 perpT))
              deposit = round (maxDeposit * profile)

          in if deposit ≤ 0
             then noModification
             else GeoModification
                 { gmElevDelta        = deposit
                 -- Downstream deltas: mixed sediment
                 -- Inner delta: shale (fine river silt)
                 -- Outer delta: sandstone (coarser material settles first)
                 , gmMaterialOverride = Just (if t < 0.4
                     then unMaterialId matShale
                     else unMaterialId matSandstone)
                 , gmIntrusionDepth   = deposit  -- full intrusion, all new sediment
                 }

-- | Pick the deeper carving between two GeoModifications.
pickDeepest ∷ GeoModification → GeoModification → GeoModification
pickDeepest a b
    | gmElevDelta b < gmElevDelta a = b
    | otherwise                     = a

-----------------------------------------------------------
-- River Evolution Application (pure GeoModification)
-----------------------------------------------------------

-- | Apply river evolution events to a single column.
--   Most river evolution modifies the PersistentFeature's
--   RiverParams (updated segments), and the terrain effect
--   comes from the next HydroEvent using the new params.
--
--   Some events produce immediate terrain effects:
--     RiverDam → small ridge deposit at the dam point
applyRiverEvolution ∷ HydroEvolution → Int → Int → Int → Int → GeoModification
applyRiverEvolution (RiverDam damPoint _lakeId damHeight) ws gx gy _e =
    let GeoCoord dx' dy' = damPoint
        ddx = fromIntegral (wrappedDeltaXGeo ws gx dx') ∷ Float
        ddy = fromIntegral (gy - dy') ∷ Float
        dist = sqrt (ddx * ddx + ddy * ddy)
        damRadius = fromIntegral damHeight * 1.5 ∷ Float
    in if dist > damRadius
       then noModification
       else let t = dist / damRadius
                profile = (1.0 - t) ** 2.0
                deposit = round (fromIntegral damHeight * profile)
            in if deposit ≤ 0
               then noModification
               -- Dam is a pile of debris / sediment
               else GeoModification deposit (Just (unMaterialId matSandstone)) deposit

-- Branch, Meander, Capture, DryUp: no per-tile terrain modification.
-- Their effect is in updating the PersistentFeature, which changes
-- future carving via the updated RiverParams.
applyRiverEvolution (RiverBranch _ _ _ _)  _ _ _ _ = noModification
applyRiverEvolution (RiverMeander _ _)     _ _ _ _ = noModification
applyRiverEvolution (RiverCapture _ _)     _ _ _ _ = noModification
applyRiverEvolution RiverDryUp             _ _ _ _ = noModification
applyRiverEvolution _                      _ _ _ _ = noModification

-----------------------------------------------------------
-- Extract river params from PersistentFeature
-----------------------------------------------------------

getRiverParams ∷ PersistentFeature → RiverParams
getRiverParams pf = case pfFeature pf of
    (HydroShape (RiverFeature r)) → r
    _ → error "getRiverParams: not a river"
