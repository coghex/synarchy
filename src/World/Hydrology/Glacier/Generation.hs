{-# LANGUAGE Strict, UnicodeSyntax #-}
module World.Hydrology.Glacier.Generation
    ( generateGlaciers
    ) where

import UPrelude
import Data.Word (Word64)
import World.Base (GeoCoord(..))
import World.Types
import World.Material (MaterialId(..))
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
        -- More candidates: base 8 instead of 4
        baseCount = scaleCount worldSize 4
        maxGlaciers = if temp < 0.8 then baseCount
                      else if temp ≤ 1.2 then baseCount `div` 2
                      else baseCount `div` 4
        maxAttempts = maxGlaciers * 4

        glacierEdge = halfTiles - 16

        go attemptIdx count tbs' acc
            | attemptIdx ≥ maxAttempts = (acc, tbs')
            | count ≥ maxGlaciers      = (acc, tbs')
            | otherwise =
                let h1 = hashGeo seed attemptIdx 1300
                    h2 = hashGeo seed attemptIdx 1301
                    u = hashToRangeGeo h1 (-halfTiles) (halfTiles - 1)
                    isNorth = hashToFloatGeo h2 < 0.5
                    v = if isNorth then -glacierEdge else glacierEdge
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

-- | Ice sheet glaciers — longer, wider, deeper carve.
tryIceSheetGlacier ∷ Word64 → Int → [TectonicPlate] → Float
                   → Int → Int → Bool → Int → Maybe GlacierParams
tryIceSheetGlacier seed worldSize plates temp gx gy isNorth attemptIdx =
    if isBeyondGlacier worldSize gx gy
    then Nothing
    else if not (isGlacierZone worldSize gx gy)
         ∧ not (nearGlacierZone worldSize gx gy)
    then Nothing
    else let (elev, _) = elevationAtGlobal seed plates worldSize gx gy
         in if elev < seaLevel
            then Nothing
            else
            let h1 = hashGeo seed attemptIdx 1310
                h2 = hashGeo seed attemptIdx 1311
                h3 = hashGeo seed attemptIdx 1312
                h4 = hashGeo seed attemptIdx 1313
                h5 = hashGeo seed attemptIdx 1314
                h6 = hashGeo seed attemptIdx 1315

                baseAngle = if isNorth then π / 4.0 else -(3.0 * π / 4.0)
                angleSpread = (hashToFloatGeo h1 - 0.5) * 1.2
                flowDir = baseAngle + angleSpread

                -- Longer glaciers: cold worlds get 60-180 (was 40-100)
                baseLength = if temp < 0.8
                             then hashToRangeGeo h2 60 180
                             else if temp ≤ 1.2
                             then hashToRangeGeo h2 30 80
                             else hashToRangeGeo h2 15 40

                -- Wider: 12-30 (was 8-20)
                width     = hashToRangeGeo h3 12 30
                thickness = hashToRangeGeo h4 8 20
                -- Deeper carve: 15-50 (was 10-30)
                carveD    = hashToRangeGeo h5 15 50
                moraine   = hashToRangeGeo h6 8 20

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
        -- More alpine glaciers: base 8 instead of 4
        maxGlaciers = if temp < 0.8 then scaleCount worldSize 4
                      else if temp ≤ 1.2 then scaleCount worldSize 2
                      else scaleCount worldSize 1  -- even warm worlds get a few high-altitude ones
        maxAttempts = maxGlaciers * 6
        -- Lower threshold: 80 above sea level (was 150)
        -- This lets glaciers form on moderate highlands, not just extreme peaks
        alpineThreshold = seaLevel + 80

        go attemptIdx count tbs' acc
            | attemptIdx ≥ maxAttempts = (acc, tbs')
            | count ≥ maxGlaciers      = (acc, tbs')
            | otherwise =
                let h1 = hashGeo seed attemptIdx 1400
                    h2 = hashGeo seed attemptIdx 1401
                    gx = hashToRangeGeo h1 (-halfTiles) (halfTiles - 1)
                    gy = hashToRangeGeo h2 (-halfTiles) (halfTiles - 1)
                in if isBeyondGlacier worldSize gx gy
                      ∨ isGlacierZone worldSize gx gy
                   then go (attemptIdx + 1) count tbs' acc
                   else let (elev, _) = elevationAtGlobal seed plates worldSize gx gy
                        in if elev < alpineThreshold
                           then go (attemptIdx + 1) count tbs' acc
                           else
                           let rc = globalToRegion worldSize gx gy
                               regionTemp = lookupRegionTemp rc (gsRegional gs)
                           -- Higher temp threshold: 10°C (was 5°C)
                           -- Cold highland + moderate latitude can still glacier
                           in if regionTemp > 10.0
                              then go (attemptIdx + 1) count tbs' acc
                              else
                              let flowDir = findSteepestDescent seed worldSize plates gx gy
                                  h3 = hashGeo seed attemptIdx 1410
                                  h4 = hashGeo seed attemptIdx 1411
                                  h5 = hashGeo seed attemptIdx 1412
                                  h6 = hashGeo seed attemptIdx 1413
                                  h7 = hashGeo seed attemptIdx 1414

                                  -- Bigger alpine glaciers: 20-60 length (was 15-40)
                                  glacierLen = hashToRangeGeo h3 20 60
                                  width      = hashToRangeGeo h4 6 16   -- was 4-10
                                  thickness  = hashToRangeGeo h5 4 12   -- was 3-8
                                  carveD     = hashToRangeGeo h6 12 30  -- was 8-20
                                  moraine    = hashToRangeGeo h7 5 15   -- was 3-10

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
