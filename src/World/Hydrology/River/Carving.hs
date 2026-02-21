{-# OPTIONS_GHC -fprof-auto #-}
{-# LANGUAGE Strict, UnicodeSyntax #-}
module World.Hydrology.River.Carving
    ( applyRiverCarve
    , applyRiverEvolution
    , carveFromSegment
    , computeDeltaDeposit'
    ) where

import UPrelude
import Data.Word (Word64)
import qualified Data.Vector as V
import World.Base (GeoCoord(..))
import World.Geology.Hash (wrappedDeltaUV)
import World.Material (matSandstone, matShale, unMaterialId)
import World.Hydrology.Types
import World.Geology.Types
import World.Geology.Hash

-----------------------------------------------------------
-- River Carving (pure GeoModification)
-----------------------------------------------------------

-- | Apply a river's terrain modification to a single column.
--   Finds the deepest carve across all segments, or falls
--   back to delta deposit at the mouth.
--
--   All carving is TARGET-BASED: we compute the absolute
--   channel floor elevation from the segment's reference
--   surface (rsStartElev/rsEndElev), and only carve down
--   to that level. If the terrain is already at or below
--   the target, we return noModification. This makes
--   carving idempotent across multiple geological ages.
applyRiverCarve ∷ RiverParams → Int → Int → Int → Int → GeoModification
applyRiverCarve river worldSize gx gy baseElev =
    let mSeed = rpMeanderSeed river
        bestCarve = findDeepestCarve worldSize gx gy mSeed baseElev
                                     (rpSegments river)
        deltaDeposit = computeDeltaDeposit river worldSize gx gy baseElev
    in if gmElevDelta bestCarve < 0
       then bestCarve
       else deltaDeposit

-- | Walk the segment list with an explicit recursive loop.
--   This avoids the foldl' closure overhead and lets GHC
--   compile it as a tight loop with unboxed accumulator fields.
--
--   Early-out: skip segments whose bounding box (padded by
--   valley width) doesn't contain the query point.
{-# INLINE findDeepestCarve #-}
findDeepestCarve ∷ Int → Int → Int → Word64 → Int
                 → V.Vector RiverSegment → GeoModification
findDeepestCarve worldSize gx gy mSeed baseElev segs = go noModification 0
  where
    !len = V.length segs
    go !acc !i
        | i ≥ len = acc
        | let seg = V.unsafeIndex segs i
              GeoCoord sx sy = rsStart seg
              GeoCoord ex ey = rsEnd seg
              pad = rsValleyWidth seg
              midX = (sx + ex) `div` 2
              midY = (sy + ey) `div` 2
              (dxi, dyi) = wrappedDeltaUV worldSize gx gy midX midY
              halfSpanX = abs (sx - ex) `div` 2 + pad + 1
              halfSpanY = abs (sy - ey) `div` 2 + pad + 1
          in abs dxi > halfSpanX ∨ abs dyi > halfSpanY
        = go acc (i + 1)
        | otherwise
        = let seg = V.unsafeIndex segs i
              carve = carveFromSegment worldSize gx gy mSeed seg baseElev
          in go (pickDeepest acc carve) (i + 1)

-----------------------------------------------------------
-- Target-Based Segment Carving
-----------------------------------------------------------

-- | Compute the carving modification from a single river segment.
--
--   TARGET-BASED: computes the absolute channel floor elevation
--   by interpolating the segment's reference surface
--   (rsStartElev → rsEndElev) and subtracting the geometric
--   depth profile. Returns only the delta needed to bring
--   baseElev down to that floor. If baseElev is already at
--   or below the floor, returns noModification.
--
--   This is the same pattern used by applyLavaFlow, which
--   computes an absolute lava surface and only deposits the
--   difference above baseElev.
--
--   The geometric profile is:
--     Channel (perpDist < channelHalfW):
--       floor = refSurface − depth × (1 − channelT×0.1) × endTaper
--     Valley wall (channelHalfW ≤ perpDist < valleyHalfW):
--       floor = refSurface − depth × (1 − wallT) × endTaper × 0.7
--
--   Where refSurface = lerp(rsStartElev, rsEndElev, clampedT)
--   and clampedT is the parametric position along the segment.
carveFromSegment ∷ Int → Int → Int → Word64 → RiverSegment → Int → GeoModification
carveFromSegment worldSize gx gy meanderSeed seg baseElev =
    let GeoCoord sx sy = rsStart seg
        GeoCoord ex ey = rsEnd seg

        -- Vector from segment start to query point
        (pxi, pyi) = wrappedDeltaUV worldSize gx gy sx sy
        px = fromIntegral pxi ∷ Float
        py = fromIntegral pyi ∷ Float

        -- Vector from segment start to segment end
        (fxi, fyi) = wrappedDeltaUV worldSize ex ey sx sy
        fdx = fromIntegral fxi ∷ Float
        fdy = fromIntegral fyi ∷ Float
        segLen = sqrt (fdx * fdx + fdy * fdy)

    in if segLen < 0.001
       then noModification
       else
       let -- Unit direction along segment
           nx = fdx / segLen
           ny = fdy / segLen

           -- Parametric position along segment
           dot = px * nx + py * ny
           alongT = dot / segLen

           -- Perpendicular distance from segment axis
           perpX = px - dot * nx
           perpY = py - dot * ny
           perpDist = sqrt (perpX * perpX + perpY * perpY)

           -- Segment geometry
           channelHalfW = fromIntegral (rsWidth seg) / 2.0 ∷ Float
           valleyHalfW  = fromIntegral (rsValleyWidth seg) / 2.0 ∷ Float
           depth        = fromIntegral (rsDepth seg) ∷ Float
           flow         = rsFlowRate seg

           -- Taper at segment endpoints to avoid hard edges
           endTaper = min 1.0 (min ((alongT + 0.05) * 8.0)
                                   ((1.05 - alongT) * 8.0))

           -- Meander offset: sinusoidal displacement perpendicular
           -- to the segment axis, giving the channel a winding path
           meanderFreq = 2.0 * π / segLen * 1.5
           meanderPhase = fromIntegral (fromIntegral meanderSeed `mod` (1000 ∷ Int))
                        * 0.001 * 2.0 * π
           meanderOffset = sin (alongT * segLen * meanderFreq
                              / segLen * 2.0 * π + meanderPhase)
                         * valleyHalfW * 0.2 * min 1.0 flow
           effectivePerpDist = abs (perpDist - meanderOffset)

           -- Interpolated reference surface at this point along
           -- the segment. This is the elevation the terrain WOULD
           -- have without the river — the "rim" of the valley.
           clampedT = max 0.0 (min 1.0 alongT)
           refSurface = fromIntegral (rsStartElev seg)
                      + clampedT * fromIntegral (rsEndElev seg - rsStartElev seg)

       in if alongT < -0.05 ∨ alongT > 1.05 ∨ effectivePerpDist > valleyHalfW
          then noModification

          -- Inside the channel proper
          else if effectivePerpDist < channelHalfW
          then let channelT = effectivePerpDist / channelHalfW
                   channelProfile = 1.0 - channelT * 0.1
                   -- Absolute floor elevation at this point
                   channelFloor = refSurface - depth * channelProfile * endTaper
                   targetElev = round channelFloor
                   -- Only carve if we're above the target
                   carve = baseElev - targetElev
                   alluviumDepth = max 1 (round (depth * 0.4 * endTaper))
               in if carve ≤ 0
                  then noModification
                  else GeoModification
                      { gmElevDelta        = negate carve
                      , gmMaterialOverride = Just (unMaterialId matSandstone)
                      , gmIntrusionDepth   = min alluviumDepth carve
                      }

          -- Valley walls: gradual slope from rim to channel edge
          else let wallT = (effectivePerpDist - channelHalfW)
                         / (valleyHalfW - channelHalfW)
                   wallProfile = max 0.0 (1.0 - wallT)
                   valleyFloor = refSurface - depth * wallProfile * endTaper * 0.7
                   targetElev = round valleyFloor
                   carve = baseElev - targetElev
               in if carve ≤ 0
                  then noModification
                  else GeoModification (negate carve) Nothing 0

-----------------------------------------------------------
-- Delta Deposit — original version (takes full RiverParams)
-----------------------------------------------------------

-- | Compute delta deposit from a full RiverParams.
--   Delegates to computeDeltaDeposit' using the last segment.
computeDeltaDeposit ∷ RiverParams → Int → Int → Int → Int → GeoModification
computeDeltaDeposit river worldSize gx gy baseElev =
    if V.null (rpSegments river)
    then noModification
    else computeDeltaDeposit' (V.last (rpSegments river))
                              (rpFlowRate river) worldSize gx gy baseElev

-----------------------------------------------------------
-- Target-Based Delta Deposit
-----------------------------------------------------------

-- | Compute delta deposit from the last segment and flow rate.
--
--   TARGET-BASED: computes an absolute target elevation for the
--   delta surface (mouth elevation + deposit profile), then only
--   deposits the difference above baseElev. If baseElev is already
--   at or above the target, returns noModification. This prevents
--   sediment from piling up unboundedly across multiple ages.
computeDeltaDeposit' ∷ RiverSegment → Float → Int → Int → Int → Int → GeoModification
computeDeltaDeposit' lastSeg totalFlow worldSize gx gy baseElev =
    let GeoCoord mx my = rsEnd lastSeg
        GeoCoord px py = rsStart lastSeg

        -- Flow direction at the mouth
        (fxi, fyi) = wrappedDeltaUV worldSize mx my px py
        flowDX = fromIntegral fxi ∷ Float
        flowDY = fromIntegral fyi ∷ Float
        flowLen = sqrt (flowDX * flowDX + flowDY * flowDY)

    in if flowLen < 0.001 then noModification
    else
    let flowNX = flowDX / flowLen
        flowNY = flowDY / flowLen

        -- Distance from mouth to query point
        (dxi, dyi) = wrappedDeltaUV worldSize gx gy mx my
        dx = fromIntegral dxi ∷ Float
        dy = fromIntegral dyi ∷ Float
        dist = sqrt (dx * dx + dy * dy)

        deltaRadius = totalFlow * 25.0 + 8.0
        maxDeposit = max 2.0 (totalFlow * 6.0)

        -- Reference: the mouth elevation is the base of the delta
        mouthElev = fromIntegral (rsEndElev lastSeg) ∷ Float

    in if dist > deltaRadius
       then noModification
       else
       let -- Only deposit in the downstream fan
           dotFlow = dx * flowNX + dy * flowNY
           spreadAngle = if dotFlow > -deltaRadius * 0.2
                         then 1.0
                         else 0.0

       in if spreadAngle < 0.5
          then noModification
          else
          let t = dist / deltaRadius
              perpDist = abs (dx * flowNY - dy * flowNX)
              perpT = perpDist / (deltaRadius * 1.2)

              profile = max 0.0 ((1.0 - t) * (1.0 - min 1.0 perpT))
              -- Absolute target elevation for the delta surface
              targetElev = round (mouthElev + maxDeposit * profile)
              -- Only deposit if we're below the target
              deposit = targetElev - baseElev

          in if deposit ≤ 0
             then noModification
             else GeoModification
                 { gmElevDelta        = deposit
                 , gmMaterialOverride = Just (if t < 0.4
                     then unMaterialId matShale
                     else unMaterialId matSandstone)
                 , gmIntrusionDepth   = deposit
                 }

-----------------------------------------------------------
-- Helpers
-----------------------------------------------------------

-- | Pick the deeper of two modifications (more negative delta wins).
pickDeepest ∷ GeoModification → GeoModification → GeoModification
pickDeepest a b
    | gmElevDelta b < gmElevDelta a = b
    | otherwise                     = a

-----------------------------------------------------------
-- River Evolution Application (pure GeoModification)
-----------------------------------------------------------

-- | Apply a river evolution event's immediate terrain effect.
--   Most evolution events only update PersistentFeature state;
--   the terrain change comes from the next HydroEvent.
--   Only RiverDam produces an immediate terrain modification
--   (a small debris ridge at the dam point).
applyRiverEvolution ∷ HydroEvolution → Int → Int → Int → Int → GeoModification
applyRiverEvolution (RiverDam damPoint _lakeId damHeight) ws gx gy _e =
    let GeoCoord dx' dy' = damPoint
        (dxi, dyi) = wrappedDeltaUV ws gx gy dx' dy'
        ddx = fromIntegral dxi ∷ Float
        ddy = fromIntegral dyi ∷ Float
        dist = sqrt (ddx * ddx + ddy * ddy)
        damRadius = fromIntegral damHeight * 1.5 ∷ Float
    in if dist > damRadius
       then noModification
       else let t = dist / damRadius
                profile = (1.0 - t) ** 2.0
                deposit = round (fromIntegral damHeight * profile)
            in if deposit ≤ 0
               then noModification
               else GeoModification deposit (Just (unMaterialId matSandstone)) deposit

applyRiverEvolution (RiverBranch _ _ _ _)  _ _ _ _ = noModification
applyRiverEvolution (RiverMeander _ _)     _ _ _ _ = noModification
applyRiverEvolution (RiverCapture _ _)     _ _ _ _ = noModification
applyRiverEvolution RiverDryUp             _ _ _ _ = noModification
applyRiverEvolution _                      _ _ _ _ = noModification
