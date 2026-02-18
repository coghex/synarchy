{-# LANGUAGE Strict, UnicodeSyntax #-}
module World.Hydrology.River.Carving
    ( applyRiverCarve
    , applyRiverEvolution
    ) where

import UPrelude
import Data.Word (Word64)
import World.Base (GeoCoord(..))
import World.Geology.Hash (wrappedDeltaXGeo)
import World.Material (matSandstone, matShale, unMaterialId)
import World.Hydrology.Types
import World.Geology.Types
import World.Geology.Hash

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
