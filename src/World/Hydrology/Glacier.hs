{-# LANGUAGE Strict, UnicodeSyntax #-}
module World.Hydrology.Glacier
    ( generateGlaciers
    , evolveGlacier
    , applyGlacierCarve
    , applyGlacierEvolution
    ) where

import UPrelude
import Data.Word (Word64)
import qualified Data.HashMap.Strict as HM
import World.Base (GeoCoord(..), GeoFeatureId(..))
import World.Types
import World.Material (matSandstone, matGlacier, matShale, matLimestone
                      , unMaterialId, MaterialId(..))
import World.Plate (TectonicPlate(..), elevationAtGlobal, isBeyondGlacier, isGlacierZone)
import World.Hydrology.Types
import World.Geology.Types
import World.Geology.Hash

-----------------------------------------------------------
-- Glacier Generation
-----------------------------------------------------------

-- | Generate glaciers from high-elevation cold regions.
--   Two sources:
--
--   1. Ice sheet tongues: extend from the polar glacier boundary
--      inward toward the equator. These are the big landscape
--      shapers that create flat valley plains with plateaus.
--      Placed at high-latitude land points near the glacier zone.
--
--   2. Alpine glaciers: form at high-elevation peaks far from
--      poles. Require elevation > alpineThreshold AND latitude
--      cold enough (temperature < 0°C from regional data).
--      These carve mountain valleys.
--
--   Generation follows the same pattern as generateAndRegisterN:
--   hash-based candidate placement, elevation/position checks,
--   register as PersistentFeature on success.
generateGlaciers ∷ Word64 → Int → [TectonicPlate]
                 → GeoState → Int → TimelineBuildState
                 → ([PersistentFeature], TimelineBuildState)
generateGlaciers seed worldSize plates gs periodIdx tbs =
    let (iceSheet, tbs1) = generateIceSheetTongues seed worldSize plates gs periodIdx tbs
        (alpine,   tbs2) = generateAlpineGlaciers seed worldSize plates gs periodIdx tbs1
    in (iceSheet <> alpine, tbs2)

-----------------------------------------------------------
-- Ice Sheet Tongues
-----------------------------------------------------------

-- | Ice sheet glaciers extend from the polar boundary inward.
--   They flow toward the equator (perpendicular to the glacier edge).
--   The number and size depend on global temperature:
--     Cold world (gsCO2 < 0.8): many wide tongues, long reach
--     Moderate (0.8-1.2): fewer, shorter
--     Warm (> 1.2): very few, short
--
--   Placement: sample points along the glacier boundary at
--   regular intervals. Each candidate flows inward if the
--   terrain is land and above sea level.
generateIceSheetTongues ∷ Word64 → Int → [TectonicPlate] → GeoState → Int
                        → TimelineBuildState
                        → ([PersistentFeature], TimelineBuildState)
generateIceSheetTongues seed worldSize plates gs periodIdx tbs =
    let halfTiles = (worldSize * 16) `div` 2
        temp = gsCO2 gs
        -- How many candidates to try along each polar edge
        baseCount = scaleCount worldSize 4
        maxGlaciers = if temp < 0.8 then baseCount
                      else if temp ≤ 1.2 then baseCount `div` 2
                      else baseCount `div` 4
        maxAttempts = maxGlaciers * 4

        -- The glacier zone starts at glacierEdge (screenRow = |gx+gy|)
        -- We place glacier sources just inside the glacier zone,
        -- flowing inward (toward smaller |gx+gy|).
        -- North pole: gx + gy ≈ -halfTiles → flow direction increases gx+gy
        -- South pole: gx + gy ≈ +halfTiles → flow direction decreases gx+gy
        glacierEdge = halfTiles - 16  -- chunkSize tiles inside boundary

        go attemptIdx count tbs' acc
            | attemptIdx ≥ maxAttempts = (acc, tbs')
            | count ≥ maxGlaciers      = (acc, tbs')
            | otherwise =
                let h1 = hashGeo seed attemptIdx 1300
                    h2 = hashGeo seed attemptIdx 1301
                    h3 = hashGeo seed attemptIdx 1302
                    -- Pick a u-coordinate (gx - gy) along the edge
                    u = hashToRangeGeo h1 (-halfTiles) (halfTiles - 1)
                    -- Pick north or south pole
                    isNorth = hashToFloatGeo h2 < 0.5
                    -- The v-coordinate (gx + gy) at the glacier edge
                    v = if isNorth then -glacierEdge else glacierEdge
                    -- Convert (u, v) to (gx, gy)
                    gx = (u + v) `div` 2
                    gy = (v - u) `div` 2
                in case tryIceSheetGlacier seed worldSize plates temp
                         gx gy isNorth attemptIdx of
                    Nothing → go (attemptIdx + 1) count tbs' acc
                    Just glacier →
                        let (fid, tbs'') = allocFeatureId tbs'
                            pf = PersistentFeature
                                { pfId               = fid
                                , pfFeature          = HydroShape $ GlacierFeature glacier
                                , pfActivity         = FActive
                                , pfFormationPeriod   = periodIdx
                                , pfLastActivePeriod  = periodIdx
                                , pfEruptionCount     = 1
                                , pfParentId          = Nothing
                                }
                            tbs''' = registerFeature pf tbs''
                        in go (attemptIdx + 1) (count + 1) tbs''' (pf : acc)

    in go 0 0 tbs []

-- | Try to place an ice sheet tongue at (gx, gy).
--   Fails if the source is ocean, below sea level, or
--   too far from the glacier zone.
tryIceSheetGlacier ∷ Word64 → Int → [TectonicPlate] → Float
                   → Int → Int → Bool → Int → Maybe GlacierParams
tryIceSheetGlacier seed worldSize plates temp gx gy isNorth attemptIdx =
    -- Must be on land near the glacier boundary
    if isBeyondGlacier worldSize gx gy
    then Nothing  -- too far out, in the impassable zone
    else if not (isGlacierZone worldSize gx gy)
         ∧ not (nearGlacierZone worldSize gx gy)
    then Nothing  -- too far from poles
    else let (elev, _) = elevationAtGlobal seed plates worldSize gx gy
         in if elev < seaLevel
            then Nothing  -- can't start a glacier in the ocean
            else
            let h1 = hashGeo seed attemptIdx 1310
                h2 = hashGeo seed attemptIdx 1311
                h3 = hashGeo seed attemptIdx 1312
                h4 = hashGeo seed attemptIdx 1313
                h5 = hashGeo seed attemptIdx 1314
                h6 = hashGeo seed attemptIdx 1315

                -- Flow direction: toward the equator
                -- North pole (v < 0): flow in +v direction → angle ≈ π/4
                -- South pole (v > 0): flow in -v direction → angle ≈ -3π/4
                -- Plus some hash-based spread for variety
                baseAngle = if isNorth then π / 4.0 else -(3.0 * π / 4.0)
                angleSpread = (hashToFloatGeo h1 - 0.5) * 1.2  -- ±0.6 radians
                flowDir = baseAngle + angleSpread

                -- Length scales with temperature (cold = longer reach)
                baseLength = if temp < 0.8
                             then hashToRangeGeo h2 40 100
                             else if temp ≤ 1.2
                             then hashToRangeGeo h2 20 50
                             else hashToRangeGeo h2 10 25

                width     = hashToRangeGeo h3 8 20
                thickness = hashToRangeGeo h4 5 15
                carveD    = hashToRangeGeo h5 10 30
                moraine   = hashToRangeGeo h6 5 15

            in Just GlacierParams
                { glCenter      = GeoCoord gx gy
                , glFlowDir     = flowDir
                , glLength      = baseLength
                , glWidth       = width
                , glThickness   = thickness
                , glCarveDepth  = carveD
                , glMoraineSize = moraine
                , glIsIceSheet  = True
                }

-- | Check if a point is within a few chunks of the glacier zone.
--   Used for ice sheet placement — glaciers start near the boundary
--   but not necessarily inside it.
nearGlacierZone ∷ Int → Int → Int → Bool
nearGlacierZone worldSize gx gy =
    let halfTiles = (worldSize * 16) `div` 2
        margin = 48  -- 3 chunks worth of tiles
        glacierEdge = halfTiles - 16
        screenRow = abs (gx + gy)
    in screenRow ≥ glacierEdge - margin ∧ screenRow < halfTiles

-----------------------------------------------------------
-- Alpine Glaciers
-----------------------------------------------------------

-- | Alpine glaciers form at high-elevation peaks in cold regions.
--   These are smaller than ice sheet tongues but create dramatic
--   mountain valleys (think: Alps, Himalayas, Rockies).
--
--   Placement: sample random points, accept only those with
--   elevation above alpineThreshold AND regional temperature < 0°C.
--   Flow direction is steepest descent from the peak.
generateAlpineGlaciers ∷ Word64 → Int → [TectonicPlate] → GeoState → Int
                       → TimelineBuildState
                       → ([PersistentFeature], TimelineBuildState)
generateAlpineGlaciers seed worldSize plates gs periodIdx tbs =
    let halfTiles = (worldSize * 16) `div` 2
        temp = gsCO2 gs
        -- Alpine glaciers only form in cold-ish worlds
        maxGlaciers = if temp < 0.8 then scaleCount worldSize 4
                      else if temp ≤ 1.2 then scaleCount worldSize 2
                      else 0  -- too warm for alpine glaciers
        maxAttempts = maxGlaciers * 6
        alpineThreshold = seaLevel + 150  -- need significant elevation

        go attemptIdx count tbs' acc
            | attemptIdx ≥ maxAttempts = (acc, tbs')
            | count ≥ maxGlaciers      = (acc, tbs')
            | otherwise =
                let h1 = hashGeo seed attemptIdx 1400
                    h2 = hashGeo seed attemptIdx 1401
                    gx = hashToRangeGeo h1 (-halfTiles) (halfTiles - 1)
                    gy = hashToRangeGeo h2 (-halfTiles) (halfTiles - 1)
                in if isBeyondGlacier worldSize gx gy
                      ∨ isGlacierZone worldSize gx gy  -- ice sheets handle polar zones
                   then go (attemptIdx + 1) count tbs' acc
                   else let (elev, _) = elevationAtGlobal seed plates worldSize gx gy
                        in if elev < alpineThreshold
                           then go (attemptIdx + 1) count tbs' acc
                           else
                           let -- Check regional temperature
                               rc = globalToRegion worldSize gx gy
                               regionTemp = lookupRegionTemp rc (gsRegional gs)
                           in if regionTemp > 5.0  -- too warm for glacier
                              then go (attemptIdx + 1) count tbs' acc
                              else
                              let -- Find steepest descent direction
                                  flowDir = findSteepestDescent seed worldSize plates gx gy
                                  h3 = hashGeo seed attemptIdx 1410
                                  h4 = hashGeo seed attemptIdx 1411
                                  h5 = hashGeo seed attemptIdx 1412
                                  h6 = hashGeo seed attemptIdx 1413
                                  h7 = hashGeo seed attemptIdx 1414

                                  -- Alpine glaciers are shorter and narrower than ice sheets
                                  glacierLen = hashToRangeGeo h3 15 40
                                  width      = hashToRangeGeo h4 4 10
                                  thickness  = hashToRangeGeo h5 3 8
                                  carveD     = hashToRangeGeo h6 8 20
                                  moraine    = hashToRangeGeo h7 3 10

                                  glacier = GlacierParams
                                      { glCenter      = GeoCoord gx gy
                                      , glFlowDir     = flowDir
                                      , glLength      = glacierLen
                                      , glWidth       = width
                                      , glThickness   = thickness
                                      , glCarveDepth  = carveD
                                      , glMoraineSize = moraine
                                      , glIsIceSheet  = False
                                      }

                                  (fid, tbs'') = allocFeatureId tbs'
                                  pf = PersistentFeature
                                      { pfId               = fid
                                      , pfFeature          = HydroShape $ GlacierFeature glacier
                                      , pfActivity         = FActive
                                      , pfFormationPeriod   = periodIdx
                                      , pfLastActivePeriod  = periodIdx
                                      , pfEruptionCount     = 1
                                      , pfParentId          = Nothing
                                      }
                                  tbs''' = registerFeature pf tbs''
                              in go (attemptIdx + 1) (count + 1) tbs''' (pf : acc)

    in go 0 0 tbs []

-- | Find the steepest descent direction from a peak.
--   Samples 8 directions at a fixed distance and picks
--   the one with the lowest elevation.
findSteepestDescent ∷ Word64 → Int → [TectonicPlate] → Int → Int → Float
findSteepestDescent seed worldSize plates gx gy =
    let sampleDist = 16  -- one chunk away
        dirs = [ (fromIntegral i * π / 4.0, sampleDist)
               | i ← [0 .. 7 ∷ Int]
               ]
        samples = map (\(angle, dist) →
            let nx = gx + round (fromIntegral dist * cos angle)
                ny = gy + round (fromIntegral dist * sin angle)
                (e, _) = if isBeyondGlacier worldSize nx ny
                         then (99999, MaterialId 0)
                         else elevationAtGlobal seed plates worldSize nx ny
            in (angle, e)
            ) dirs

        -- Pick direction with lowest neighboring elevation
        best = foldl' (\(bestA, bestE) (a, e) →
            if e < bestE then (a, e) else (bestA, bestE)
            ) (0.0, 99999) samples

    in fst best

-----------------------------------------------------------
-- Glacier Carving (pure GeoModification) — unchanged from
-- previous version, included for completeness
-----------------------------------------------------------

-- | Apply a glacier's U-shaped valley carving to a single column.
applyGlacierCarve ∷ GlacierParams → Int → Int → Int → Int → GeoModification
applyGlacierCarve glacier worldSize gx gy _baseElev =
    let GeoCoord cx cy = glCenter glacier
        flowDir = glFlowDir glacier
        glacierLen = fromIntegral (glLength glacier) ∷ Float
        halfW = fromIntegral (glWidth glacier) ∷ Float
        carveD = fromIntegral (glCarveDepth glacier) ∷ Float
        moraineH = fromIntegral (glMoraineSize glacier) ∷ Float

        dx = fromIntegral (wrappedDeltaXGeo worldSize gx cx) ∷ Float
        dy = fromIntegral (gy - cy) ∷ Float

        flowX = cos flowDir
        flowY = sin flowDir
        alongDist = dx * flowX + dy * flowY
        perpDist = abs (dx * (-flowY) + dy * flowX)

        alongT = alongDist / glacierLen

    in if alongT < -0.05 ∨ alongT > 1.15 ∨ perpDist > halfW * 1.3
       then noModification

       -- Terminal moraine
       else if alongT > 0.90 ∧ alongT ≤ 1.15 ∧ perpDist < halfW * 1.2
       then let moraineT = (alongT - 0.90) / 0.25
                ridgeProfile = sin (moraineT * π)
                perpFade = max 0.0 (1.0 - (perpDist / (halfW * 1.2)) ** 2.0)
                deposit = round (moraineH * ridgeProfile * perpFade)
            in if deposit ≤ 0
               then noModification
               else GeoModification deposit (Just (unMaterialId matSandstone)) deposit

       -- Valley carving zone
       else if alongT ≥ -0.05 ∧ alongT ≤ 0.95 ∧ perpDist < halfW
       then let endTaper = min 1.0 (min (alongT * 5.0 + 0.25)
                                       ((0.95 - alongT) * 4.0))
                perpNorm = perpDist / halfW
                flatBottomRatio = 0.6
                crossProfile =
                    if perpNorm < flatBottomRatio
                    then 1.0
                    else let wallT = (perpNorm - flatBottomRatio) / (1.0 - flatBottomRatio)
                         in max 0.0 (1.0 - wallT * wallT)

                sheetBoost = if glIsIceSheet glacier then 1.3 else 1.0

                carve = round (carveD * crossProfile * endTaper * sheetBoost)

            in if carve ≤ 0
               then noModification
               else GeoModification (negate carve) (Just (unMaterialId matSandstone)) 0

       else noModification

-----------------------------------------------------------
-- Glacier Evolution Application (pure GeoModification)
-----------------------------------------------------------

applyGlacierEvolution ∷ HydroEvolution → Int → Int → Int → Int → GeoModification
applyGlacierEvolution (GlacierAdvance _advLen _advWid) _ws _gx _gy _e =
    noModification
applyGlacierEvolution (GlacierRetreat _retreatLen _moraineDep) _ws _gx _gy _e =
    noModification
applyGlacierEvolution (GlacierMelt _moraineDep) _ws _gx _gy _e =
    noModification
applyGlacierEvolution (GlacierBranch _branchPt _angle _len _childId) _ws _gx _gy _e =
    noModification
applyGlacierEvolution _ _ws _gx _gy _e = noModification

-----------------------------------------------------------
-- Glacier Evolution
-----------------------------------------------------------

-- | Evolve a glacier feature for one age.
--   Follows the same fold pattern as evolvePointFeature:
--   called via foldl' over tbsFeatures, accumulating
--   ([GeoEvent], TimelineBuildState).
--
--   Glaciers are temperature-driven. Unlike volcanoes (which
--   have internal probabilistic state), glacier behavior is
--   largely determined by global temperature:
--
--     Cold world (gsCO2 < 0.8)  → glaciers advance aggressively
--     Moderate   (0.8 - 1.2)    → glaciers stable or slowly retreating
--     Warm world (gsCO2 > 1.2)  → glaciers retreat or melt entirely
--
--   The hash roll then determines *how* the glacier evolves
--   within that temperature-driven tendency.
--
--   Probability tables (temperature-dependent):
--
--   COLD (gsCO2 < 0.8):
--     50% → Advance: glacier extends, carves deeper
--     15% → Branch: glacier tongue splits around a ridge
--     10% → Surge: rapid advance, deep carve, wide moraine
--      5% → Spawn meltwater river at terminus
--     20% → Continue: stable, slight deepening
--
--   MODERATE (0.8 ≤ gsCO2 ≤ 1.2):
--     20% → Advance (slow)
--     20% → Retreat: glacier shortens, deposits terminal moraine
--     10% → Branch
--      5% → Spawn meltwater river
--     10% → Dam: terminal moraine blocks a valley, lake forms
--     35% → Continue stable
--
--   WARM (gsCO2 > 1.2):
--     10% → Advance (only alpine glaciers at very high elevation)
--     35% → Retreat
--     20% → Melt entirely: glacier gone, U-valley + moraine remain
--     10% → Dam: moraine creates lake as meltwater pools
--      5% → Spawn meltwater river
--     20% → Continue retreating slowly
--
evolveGlacier ∷ Word64 → Int → GeoState
              → ([GeoEvent], TimelineBuildState)
              → PersistentFeature
              → ([GeoEvent], TimelineBuildState)
evolveGlacier seed periodIdx gs (events, tbs) pf =
    let fid = pfId pf
        GeoFeatureId fidInt = fid
        h1 = hashGeo seed fidInt 900
        roll = hashToFloatGeo h1
        temp = gsCO2 gs  -- CO2 as temperature proxy
    in case pfActivity pf of

        ---------------------------------------------------
        -- Advancing glacier
        ---------------------------------------------------
        FActive → evolveFActiveGlacier seed periodIdx gs roll temp fid fidInt pf (events, tbs)

        ---------------------------------------------------
        -- Stable glacier (equivalent to Dormant volcano)
        ---------------------------------------------------
        FDormant → evolveFDormantGlacier seed periodIdx gs roll temp fid fidInt pf (events, tbs)

        ---------------------------------------------------
        -- Dead features don't evolve
        ---------------------------------------------------
        FExtinct   → (events, tbs)
        FCollapsed → (events, tbs)


-- | Active (advancing) glacier evolution
evolveFActiveGlacier ∷ Word64 → Int → GeoState → Float → Float
                     → GeoFeatureId → Int → PersistentFeature
                     → ([GeoEvent], TimelineBuildState)
                     → ([GeoEvent], TimelineBuildState)
evolveFActiveGlacier seed periodIdx gs roll temp fid fidInt pf (events, tbs)
    -- ===== COLD WORLD =====
    | temp < 0.8 =
        if roll < 0.50
        -- 50%: Advance — glacier extends further downhill, carves deeper
        then let h2 = hashGeo seed fidInt 901
                 h3 = hashGeo seed fidInt 902
                 advLen = hashToRangeGeo h2 10 40
                 advWid = hashToRangeGeo h3 2 8
                 evt = HydroModify fid (GlacierAdvance advLen advWid)
                 tbs' = updateFeature fid
                     (\p → p { pfEruptionCount = pfEruptionCount p + 1
                              , pfLastActivePeriod = periodIdx }) tbs
             in (evt : events, tbs')

        else if roll < 0.65
        -- 15%: Branch — glacier splits around a subglacial ridge
        -- Creates a new glacier feature on the flank, like parasitic eruption
        then let (childId, tbs') = allocFeatureId tbs
                 h2 = hashGeo seed fidInt 903
                 h3 = hashGeo seed fidInt 904
                 h4 = hashGeo seed fidInt 905
                 h5 = hashGeo seed fidInt 906
                 h6 = hashGeo seed fidInt 907
                 h7 = hashGeo seed fidInt 908
                 -- Branch angles off from current flow direction
                 parentGlacier = getGlacierParams pf
                 GeoCoord cx cy = glCenter parentGlacier
                 parentDir = glFlowDir parentGlacier
                 -- Branch goes 30-60° off to one side
                 branchAngle = parentDir
                             + (if hashToFloatGeo h2 < 0.5 then 1.0 else -1.0)
                             * (0.5 + hashToFloatGeo h3 * 0.5)
                 -- Starts partway down the parent glacier
                 startDist = fromIntegral (glLength parentGlacier) * (0.3 + hashToFloatGeo h4 * 0.4)
                 branchX = cx + round (startDist * cos parentDir)
                 branchY = cy + round (startDist * sin parentDir)
                 childGlacier = GlacierParams
                     { glCenter      = GeoCoord branchX branchY
                     , glFlowDir     = branchAngle
                     , glLength      = hashToRangeGeo h5 15 40
                     , glWidth       = max 4 (glWidth parentGlacier `div` 2)
                     , glThickness   = max 3 (glThickness parentGlacier * 2 `div` 3)
                     , glCarveDepth  = max 5 (glCarveDepth parentGlacier * 2 `div` 3)
                     , glMoraineSize = hashToRangeGeo h6 3 8
                     , glIsIceSheet  = False  -- branches are always alpine
                     }
                 childPf = PersistentFeature
                     { pfId               = childId
                     , pfFeature          = HydroShape $ GlacierFeature childGlacier
                     , pfActivity         = FActive
                     , pfFormationPeriod   = periodIdx
                     , pfLastActivePeriod  = periodIdx
                     , pfEruptionCount     = 1
                     , pfParentId          = Just fid
                     }
                 evt = HydroModify fid (GlacierBranch
                         (GeoCoord branchX branchY) branchAngle
                         (hashToRangeGeo h7 15 40) childId)
                 tbs'' = registerFeature childPf tbs'
                 tbs''' = updateFeature fid
                     (\p → p { pfEruptionCount = pfEruptionCount p + 1
                              , pfLastActivePeriod = periodIdx }) tbs''
             in (evt : events, tbs''')

        else if roll < 0.75
        -- 10%: Surge — rapid advance with heavy carving
        -- Like a volcanic reactivation: big event, deep modification
        then let h2 = hashGeo seed fidInt 910
                 h3 = hashGeo seed fidInt 911
                 surgeLen = hashToRangeGeo h2 30 80
                 surgeWid = hashToRangeGeo h3 5 15
                 evt = HydroModify fid (GlacierAdvance surgeLen surgeWid)
                 tbs' = updateFeature fid
                     (\p → p { pfEruptionCount = pfEruptionCount p + 1
                              , pfLastActivePeriod = periodIdx }) tbs
             in (evt : events, tbs')

        else if roll < 0.80
        -- 5%: Spawn meltwater river at terminus
        then spawnMeltwaterRiver seed periodIdx fid pf (events, tbs)

        else
        -- 20%: Continue — stable advance, slight deepening
        let h2 = hashGeo seed fidInt 915
            advLen = hashToRangeGeo h2 3 10
            evt = HydroModify fid (GlacierAdvance advLen 0)
            tbs' = updateFeature fid
                (\p → p { pfLastActivePeriod = periodIdx }) tbs
        in (evt : events, tbs')

    -- ===== MODERATE WORLD =====
    | temp ≤ 1.2 =
        if roll < 0.20
        -- 20%: Slow advance
        then let h2 = hashGeo seed fidInt 920
                 advLen = hashToRangeGeo h2 5 15
                 evt = HydroModify fid (GlacierAdvance advLen 0)
                 tbs' = updateFeature fid
                     (\p → p { pfLastActivePeriod = periodIdx }) tbs
             in (evt : events, tbs')

        else if roll < 0.40
        -- 20%: Retreat — glacier shortens, dumps moraine
        then let h2 = hashGeo seed fidInt 921
                 h3 = hashGeo seed fidInt 922
                 retreatLen = hashToRangeGeo h2 10 30
                 moraineDep = hashToRangeGeo h3 3 12
                 evt = HydroModify fid (GlacierRetreat retreatLen moraineDep)
                 tbs' = updateFeature fid
                     (\p → p { pfActivity = FDormant
                              , pfLastActivePeriod = periodIdx }) tbs
             in (evt : events, tbs')

        else if roll < 0.50
        -- 10%: Branch (same as cold, but shorter)
        then let (childId, tbs') = allocFeatureId tbs
                 parentGlacier = getGlacierParams pf
                 h2 = hashGeo seed fidInt 925
                 h3 = hashGeo seed fidInt 926
                 h4 = hashGeo seed fidInt 927
                 h5 = hashGeo seed fidInt 928
                 GeoCoord cx cy = glCenter parentGlacier
                 parentDir = glFlowDir parentGlacier
                 branchAngle = parentDir
                             + (if hashToFloatGeo h2 < 0.5 then 0.8 else -0.8)
                 startDist = fromIntegral (glLength parentGlacier) * 0.5
                 branchX = cx + round (startDist * cos parentDir)
                 branchY = cy + round (startDist * sin parentDir)
                 childGlacier = GlacierParams
                     { glCenter      = GeoCoord branchX branchY
                     , glFlowDir     = branchAngle
                     , glLength      = hashToRangeGeo h3 8 20
                     , glWidth       = max 3 (glWidth parentGlacier `div` 2)
                     , glThickness   = max 2 (glThickness parentGlacier `div` 2)
                     , glCarveDepth  = max 3 (glCarveDepth parentGlacier `div` 2)
                     , glMoraineSize = hashToRangeGeo h4 2 6
                     , glIsIceSheet  = False
                     }
                 childPf = PersistentFeature
                     { pfId               = childId
                     , pfFeature          = HydroShape $ GlacierFeature childGlacier
                     , pfActivity         = FActive
                     , pfFormationPeriod   = periodIdx
                     , pfLastActivePeriod  = periodIdx
                     , pfEruptionCount     = 1
                     , pfParentId          = Just fid
                     }
                 evt = HydroModify fid (GlacierBranch
                         (GeoCoord branchX branchY) branchAngle
                         (hashToRangeGeo h5 8 20) childId)
                 tbs'' = registerFeature childPf tbs'
                 tbs''' = updateFeature fid
                     (\p → p { pfEruptionCount = pfEruptionCount p + 1 }) tbs''
             in (evt : events, tbs''')

        else if roll < 0.55
        -- 5%: Spawn meltwater river
        then spawnMeltwaterRiver seed periodIdx fid pf (events, tbs)

        else if roll < 0.65
        -- 10%: Moraine dams valley → lake forms
        then spawnMoraineLake seed periodIdx fid pf (events, tbs)

        else
        -- 35%: Continue stable
        (events, tbs)

    -- ===== WARM WORLD =====
    | otherwise =
        if roll < 0.10
        -- 10%: Advance (only if very high elevation / ice sheet)
        then let h2 = hashGeo seed fidInt 940
                 advLen = hashToRangeGeo h2 3 8  -- minimal
                 evt = HydroModify fid (GlacierAdvance advLen 0)
                 tbs' = updateFeature fid
                     (\p → p { pfLastActivePeriod = periodIdx }) tbs
             in (evt : events, tbs')

        else if roll < 0.45
        -- 35%: Retreat with moraine
        then let h2 = hashGeo seed fidInt 941
                 h3 = hashGeo seed fidInt 942
                 retreatLen = hashToRangeGeo h2 20 60
                 moraineDep = hashToRangeGeo h3 5 20
                 evt = HydroModify fid (GlacierRetreat retreatLen moraineDep)
                 tbs' = updateFeature fid
                     (\p → p { pfActivity = FDormant
                              , pfLastActivePeriod = periodIdx }) tbs
             in (evt : events, tbs')

        else if roll < 0.65
        -- 20%: Melt entirely — glacier dies, valley + moraine persist
        -- Like GoExtinct for volcanoes
        then let h2 = hashGeo seed fidInt 943
                 moraineDep = hashToRangeGeo h2 8 25
                 evt = HydroModify fid (GlacierMelt moraineDep)
                 tbs' = updateFeature fid
                     (\p → p { pfActivity = FExtinct }) tbs
             in (evt : events, tbs')

        else if roll < 0.75
        -- 10%: Moraine dam → lake
        then spawnMoraineLake seed periodIdx fid pf (events, tbs)

        else if roll < 0.80
        -- 5%: Meltwater river
        then spawnMeltwaterRiver seed periodIdx fid pf (events, tbs)

        else
        -- 20%: Slow retreat (no explicit event, just mark as dormant)
        let tbs' = updateFeature fid
                (\p → p { pfActivity = FDormant
                         , pfLastActivePeriod = periodIdx }) tbs
        in (events, tbs')


-- | Dormant (stable/retreating) glacier evolution
evolveFDormantGlacier ∷ Word64 → Int → GeoState → Float → Float
                      → GeoFeatureId → Int → PersistentFeature
                      → ([GeoEvent], TimelineBuildState)
                      → ([GeoEvent], TimelineBuildState)
evolveFDormantGlacier seed periodIdx gs roll temp fid fidInt pf (events, tbs)
    -- Cold snap: dormant glaciers can reactivate
    | temp < 0.8 =
        if roll < 0.40
        -- 40%: Reactivate — like volcano going Active again
        then let h2 = hashGeo seed fidInt 950
                 h3 = hashGeo seed fidInt 951
                 advLen = hashToRangeGeo h2 15 50
                 advWid = hashToRangeGeo h3 3 10
                 evt = HydroModify fid (GlacierAdvance advLen advWid)
                 tbs' = updateFeature fid
                     (\p → p { pfActivity = FActive
                              , pfEruptionCount = pfEruptionCount p + 1
                              , pfLastActivePeriod = periodIdx }) tbs
             in (evt : events, tbs')
        else
        -- 60%: Stay dormant
        (events, tbs)

    -- Moderate: might reactivate or melt
    | temp ≤ 1.2 =
        if roll < 0.15
        -- 15%: Reactivate (slowly)
        then let h2 = hashGeo seed fidInt 955
                 advLen = hashToRangeGeo h2 5 15
                 evt = HydroModify fid (GlacierAdvance advLen 0)
                 tbs' = updateFeature fid
                     (\p → p { pfActivity = FActive
                              , pfLastActivePeriod = periodIdx }) tbs
             in (evt : events, tbs')

        else if roll < 0.35
        -- 20%: Melt entirely
        then let h2 = hashGeo seed fidInt 956
                 moraineDep = hashToRangeGeo h2 5 15
                 evt = HydroModify fid (GlacierMelt moraineDep)
                 tbs' = updateFeature fid
                     (\p → p { pfActivity = FExtinct }) tbs
             in (evt : events, tbs')

        else
        -- 65%: Stay dormant
        (events, tbs)

    -- Warm: most dormant glaciers will die
    | otherwise =
        if roll < 0.60
        -- 60%: Melt
        then let h2 = hashGeo seed fidInt 960
                 moraineDep = hashToRangeGeo h2 8 25
                 evt = HydroModify fid (GlacierMelt moraineDep)
                 tbs' = updateFeature fid
                     (\p → p { pfActivity = FExtinct }) tbs
             in (evt : events, tbs')

        else if roll < 0.75
        -- 15%: Moraine dam → lake (the glacier's last gift)
        then spawnMoraineLake seed periodIdx fid pf (events, tbs)

        else
        -- 25%: Linger dormant (high altitude holdouts)
        (events, tbs)

-----------------------------------------------------------
-- Helper: spawn a meltwater river at glacier terminus
-----------------------------------------------------------

-- | Creates a new river feature originating at the glacier's
--   terminal point. The river flows in the same general direction
--   as the glacier but diverges. This is how glaciers seed the
--   river system — glaciers come first geologically, then as
--   they retreat, their meltwater carves river valleys into
--   the U-shaped glacial valleys.
spawnMeltwaterRiver ∷ Word64 → Int → GeoFeatureId → PersistentFeature
                    → ([GeoEvent], TimelineBuildState)
                    → ([GeoEvent], TimelineBuildState)
spawnMeltwaterRiver seed periodIdx parentFid pf (events, tbs) =
    let GeoFeatureId fidInt = parentFid
        (childId, tbs') = allocFeatureId tbs
        glacier = getGlacierParams pf
        GeoCoord cx cy = glCenter glacier
        flowDir = glFlowDir glacier
        glacierLen = fromIntegral (glLength glacier) ∷ Float

        -- River starts at glacier terminus
        termX = cx + round (glacierLen * cos flowDir)
        termY = cy + round (glacierLen * sin flowDir)

        -- River continues roughly in the glacier's flow direction
        -- but with some deviation
        h2 = hashGeo seed fidInt 970
        h3 = hashGeo seed fidInt 971
        h4 = hashGeo seed fidInt 972
        riverAngle = flowDir + (hashToFloatGeo h2 - 0.5) * 0.6
        riverLen = hashToRangeGeo h3 20 60
        endX = termX + round (fromIntegral riverLen * cos riverAngle)
        endY = termY + round (fromIntegral riverLen * sin riverAngle)

        -- Simple single-segment river for now
        -- River generation will add more complexity later
        seg = RiverSegment
            { rsStart      = GeoCoord termX termY
            , rsEnd        = GeoCoord endX endY
            , rsWidth      = hashToRangeGeo h4 2 5
            , rsValleyWidth = hashToRangeGeo (hashGeo seed fidInt 973) 8 20
            , rsDepth      = hashToRangeGeo (hashGeo seed fidInt 974) 5 15
            , rsFlowRate   = 0.4  -- moderate meltwater
            }

        riverParams = RiverParams
            { rpSourceRegion = GeoCoord termX termY
            , rpMouthRegion  = GeoCoord endX endY
            , rpSegments     = [seg]
            , rpFlowRate     = 0.4
            , rpMeanderSeed  = fromIntegral (hashGeo seed fidInt 975)
            }

        childPf = PersistentFeature
            { pfId               = childId
            , pfFeature          = HydroShape $ RiverFeature riverParams
            , pfActivity         = FActive
            , pfFormationPeriod   = periodIdx
            , pfLastActivePeriod  = periodIdx
            , pfEruptionCount     = 1
            , pfParentId          = Just parentFid
            }

        evt = HydroEvent (RiverFeature riverParams)
        tbs'' = registerFeature childPf tbs'
    in (evt : events, tbs'')

-----------------------------------------------------------
-- Helper: spawn a moraine-dammed lake
-----------------------------------------------------------

-- | When a glacier retreats, its terminal moraine can dam
--   the valley, creating a lake behind it. Classic examples:
--   the Great Lakes, Lake Geneva, Lake Como.
--
--   The lake forms at the glacier terminus. Its surface level
--   equals the moraine height. The basin depth is the glacier's
--   carve depth below that.
spawnMoraineLake ∷ Word64 → Int → GeoFeatureId → PersistentFeature
                 → ([GeoEvent], TimelineBuildState)
                 → ([GeoEvent], TimelineBuildState)
spawnMoraineLake seed periodIdx parentFid pf (events, tbs) =
    let GeoFeatureId fidInt = parentFid
        (childId, tbs') = allocFeatureId tbs
        glacier = getGlacierParams pf
        GeoCoord cx cy = glCenter glacier
        flowDir = glFlowDir glacier
        glacierLen = fromIntegral (glLength glacier) ∷ Float

        -- Lake sits just behind the terminal moraine
        -- (upstream of the terminus)
        lakeDistFromCenter = glacierLen * 0.75
        lakeX = cx + round (lakeDistFromCenter * cos flowDir)
        lakeY = cy + round (lakeDistFromCenter * sin flowDir)

        h2 = hashGeo seed fidInt 980
        h3 = hashGeo seed fidInt 981
        h4 = hashGeo seed fidInt 982

        lakeRadius = hashToRangeGeo h2 8 25
        -- Lake surface = moraine height (relative, will be
        -- combined with base elevation at chunk gen time)
        lakeSurface = glMoraineSize glacier
        lakeDepth = hashToRangeGeo h3 5 (glCarveDepth glacier)

        lakeParams = LakeParams
            { lkCenter  = GeoCoord lakeX lakeY
            , lkRadius  = lakeRadius
            , lkSurface = lakeSurface
            , lkDepth   = lakeDepth
            , lkSource  = GlacialBasin parentFid
            }

        childPf = PersistentFeature
            { pfId               = childId
            , pfFeature          = HydroShape $ LakeFeature lakeParams
            , pfActivity         = FActive
            , pfFormationPeriod   = periodIdx
            , pfLastActivePeriod  = periodIdx
            , pfEruptionCount     = 0
            , pfParentId          = Just parentFid
            }

        evt = HydroEvent (LakeFeature lakeParams)
        tbs'' = registerFeature childPf tbs'
    in (evt : events, tbs'')

-----------------------------------------------------------
-- Extract glacier params from PersistentFeature
-----------------------------------------------------------

getGlacierParams ∷ PersistentFeature → GlacierParams
getGlacierParams pf = case pfFeature pf of
    (HydroShape (GlacierFeature g)) → g
    _ → error "getGlacierParams: not a glacier"

-----------------------------------------------------------
-- Glacier Carving (pure GeoModification)
-----------------------------------------------------------

-- | Apply a glacier's U-shaped valley carving to a single column.
--   Called from applyGeoEvent during chunk generation.
--
--   The glacier is a linear feature from glCenter along glFlowDir
--   for glLength tiles. The carving profile perpendicular to the
--   flow direction is U-shaped:
--
--        ___________          ___________
--       /           \        /           \    <- plateaus between glaciers
--      |             |      |             |
--      |  flat bottom |      |  flat bottom |
--      |_____________|      |_____________|
--          glacier 1            glacier 2
--
--   The flat bottom width = glWidth * 0.6
--   The parabolic walls extend to glWidth * 1.0
--   Beyond glWidth: no modification (this is the plateau)
--
--   Along the flow axis:
--     - Carve depth tapers from full at the head to zero at terminus
--     - Terminal moraine: positive deposit at the end
--
--   Material: the carved surface becomes sandstone (glacial till)
--   Moraine: also sandstone (unsorted glacial debris)
--applyGlacierCarve ∷ GlacierParams → Int → Int → Int → Int → GeoModification
--applyGlacierCarve glacier worldSize gx gy _baseElev =
--    let GeoCoord cx cy = glCenter glacier
--        flowDir = glFlowDir glacier
--        glacierLen = fromIntegral (glLength glacier) ∷ Float
--        halfW = fromIntegral (glWidth glacier) ∷ Float
--        carveD = fromIntegral (glCarveDepth glacier) ∷ Float
--        moraineH = fromIntegral (glMoraineSize glacier) ∷ Float
--
--        -- Vector from glacier center to this tile
--        dx = fromIntegral (wrappedDeltaXGeo worldSize gx cx) ∷ Float
--        dy = fromIntegral (gy - cy) ∷ Float
--
--        -- Project onto flow direction to get along-flow distance
--        flowX = cos flowDir
--        flowY = sin flowDir
--        alongDist = dx * flowX + dy * flowY
--
--        -- Perpendicular distance
--        perpDist = abs (dx * (-flowY) + dy * flowX)
--
--        -- Normalized positions
--        alongT = alongDist / glacierLen  -- 0.0 at head, 1.0 at terminus
--
--    in if alongT < -0.05 ∨ alongT > 1.15 ∨ perpDist > halfW * 1.3
--       then noModification
--
--       -- Terminal moraine: deposit at the end of the glacier
--       else if alongT > 0.90 ∧ alongT ≤ 1.15 ∧ perpDist < halfW * 1.2
--       then let moraineT = (alongT - 0.90) / 0.25
--                -- Ridge profile: rises then falls
--                ridgeProfile = sin (moraineT * π)
--                -- Wider at center, narrower at edges
--                perpFade = max 0.0 (1.0 - (perpDist / (halfW * 1.2)) ** 2.0)
--                deposit = round (moraineH * ridgeProfile * perpFade)
--            in if deposit ≤ 0
--               then noModification
--               else GeoModification deposit (Just (unMaterialId matSandstone)) deposit
--
--       -- Valley carving zone
--       else if alongT ≥ -0.05 ∧ alongT ≤ 0.95 ∧ perpDist < halfW
--       then let -- End taper: carving fades near head and terminus
--                endTaper = min 1.0 (min (alongT * 5.0 + 0.25)
--                                       ((0.95 - alongT) * 4.0))
--                -- U-shaped cross-section
--                perpNorm = perpDist / halfW
--                flatBottomRatio = 0.6  -- 60% of width is flat bottom
--                crossProfile =
--                    if perpNorm < flatBottomRatio
--                    -- Flat bottom: full carve depth
--                    then 1.0
--                    -- Parabolic walls: (1 - ((t-0.6)/0.4)^2)
--                    else let wallT = (perpNorm - flatBottomRatio) / (1.0 - flatBottomRatio)
--                         in max 0.0 (1.0 - wallT * wallT)
--
--                -- Ice sheet glaciers carve wider and flatter
--                sheetBoost = if glIsIceSheet glacier then 1.3 else 1.0
--
--                carve = round (carveD * crossProfile * endTaper * sheetBoost)
--
--            in if carve ≤ 0
--               then noModification
--               -- Negative delta: carving removes material
--               -- Surface becomes glacial till (sandstone)
--               else GeoModification (negate carve) (Just (unMaterialId matSandstone)) 0
--
--       else noModification

-----------------------------------------------------------
-- Glacier Evolution Application (pure GeoModification)
-----------------------------------------------------------

-- | Apply a glacier evolution event to a single column.
--   Called from applyGeoEvent for HydroModify events.
--
--   GlacierAdvance: extends the carving zone further
--     → recompute the carve with increased length/width
--     (The evolution event updates the PersistentFeature's params,
--      and the next chunk generation uses the new params.
--      But we also emit a GeoModification for the *delta* this age.)
--
--   GlacierRetreat: deposits moraine at current terminus
--     → positive elevation at the terminus zone
--
--   GlacierMelt: final moraine deposit, no more carving
--     → like GlacierRetreat but stronger moraine
--applyGlacierEvolution ∷ HydroEvolution → Int → Int → Int → Int → GeoModification
--applyGlacierEvolution (GlacierAdvance _advLen _advWid) _ws _gx _gy _e =
--    -- Advance doesn't produce a per-tile modification here.
--    -- The advance updates the feature's length/width in tbs,
--    -- and the *next* age's HydroEvent (GlacierFeature ...) uses
--    -- the updated params. This is how your volcanic Reactivate works too:
--    -- the evolution updates the feature state, then the feature's
--    -- shape applies during chunk generation.
--    noModification
--
--applyGlacierEvolution (GlacierRetreat _retreatLen moraineDep) ws gx gy _e =
--    -- Moraine deposition at the former terminus.
--    -- This is emitted as a separate GeoModification because
--    -- the moraine persists after the glacier retreats.
--    -- The actual spatial application (where the moraine goes)
--    -- is handled by the HydroEvent storing the updated GlacierParams.
--    noModification  -- spatial logic in applyGlacierCarve moraine zone
--
--applyGlacierEvolution (GlacierMelt moraineDep) _ws _gx _gy _e =
--    -- Same as retreat — the carved valley persists as terrain,
--    -- moraine is deposited by the updated feature params.
--    noModification
--
--applyGlacierEvolution (GlacierBranch _branchPt _angle _len _childId) _ws _gx _gy _e =
--    -- Branch spawns a new feature; the child's GlacierFeature
--    -- handles its own carving. No per-tile mod from the branch event itself.
--    noModification
--
--applyGlacierEvolution _ _ws _gx _gy _e = noModification
