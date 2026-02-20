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

applyRiverCarve ∷ RiverParams → Int → Int → Int → Int → GeoModification
applyRiverCarve river worldSize gx gy baseElev =
    let mSeed = rpMeanderSeed river
        bestCarve = findDeepestCarve worldSize gx gy mSeed
                                     (rpSegments river)
        deltaDeposit = computeDeltaDeposit river worldSize gx gy
    in if gmElevDelta bestCarve < 0
       then bestCarve
       else deltaDeposit

-- | Walk the segment list with an explicit recursive loop.
--   This avoids the foldl' closure overhead and lets GHC
--   compile it as a tight loop with unboxed accumulator fields.
{-# INLINE findDeepestCarve #-}
findDeepestCarve ∷ Int → Int → Int → Word64 → V.Vector RiverSegment → GeoModification
findDeepestCarve worldSize gx gy mSeed segs = go noModification 0
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
              carve = carveFromSegment worldSize gx gy mSeed seg
          in go (pickDeepest acc carve) (i + 1)

-- | Compute the carving modification from a single river segment.
carveFromSegment ∷ Int → Int → Int → Word64 → RiverSegment → GeoModification
carveFromSegment worldSize gx gy meanderSeed seg =
    let GeoCoord sx sy = rsStart seg
        GeoCoord ex ey = rsEnd seg

        (pxi, pyi) = wrappedDeltaUV worldSize gx gy sx sy
        px = fromIntegral pxi ∷ Float
        py = fromIntegral pyi ∷ Float

        (fxi, fyi) = wrappedDeltaUV worldSize ex ey sx sy
        fdx = fromIntegral fxi ∷ Float
        fdy = fromIntegral fyi ∷ Float
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

          else if effectivePerpDist < channelHalfW
          then let channelT = effectivePerpDist / channelHalfW
                   channelProfile = 1.0 - channelT * 0.1
                   carve = round (depth * channelProfile * endTaper)
                   alluviumDepth = max 1 (round (depth * 0.4 * endTaper))
               in if carve ≤ 0
                  then noModification
                  else GeoModification
                      { gmElevDelta        = negate carve
                      , gmMaterialOverride = Just (unMaterialId matSandstone)
                      , gmIntrusionDepth   = alluviumDepth
                      }

          else let wallT = (effectivePerpDist - channelHalfW)
                         / (valleyHalfW - channelHalfW)
                   wallProfile = max 0.0 (1.0 - wallT)
                   carve = round (depth * wallProfile * endTaper * 0.7)
               in if carve ≤ 0
                  then noModification
                  else GeoModification (negate carve) Nothing 0

-----------------------------------------------------------
-- Delta deposit — original version (takes full RiverParams)
-----------------------------------------------------------

computeDeltaDeposit ∷ RiverParams → Int → Int → Int → GeoModification
computeDeltaDeposit river worldSize gx gy =
    if V.null (rpSegments river)
    then noModification
    else computeDeltaDeposit' (V.last (rpSegments river)) (rpFlowRate river) worldSize gx gy

-----------------------------------------------------------
--   Takes the last segment and flow rate directly,
--   no need for the full RiverParams.
-----------------------------------------------------------

computeDeltaDeposit' ∷ RiverSegment → Float → Int → Int → Int → GeoModification
computeDeltaDeposit' lastSeg totalFlow worldSize gx gy =
    let GeoCoord mx my = rsEnd lastSeg
        GeoCoord px py = rsStart lastSeg

        (fxi, fyi) = wrappedDeltaUV worldSize mx my px py
        flowDX = fromIntegral fxi ∷ Float
        flowDY = fromIntegral fyi ∷ Float
        flowLen = sqrt (flowDX * flowDX + flowDY * flowDY)

    in if flowLen < 0.001 then noModification
    else
    let flowNX = flowDX / flowLen
        flowNY = flowDY / flowLen

        (dxi, dyi) = wrappedDeltaUV worldSize gx gy mx my
        dx = fromIntegral dxi ∷ Float
        dy = fromIntegral dyi ∷ Float
        dist = sqrt (dx * dx + dy * dy)

        deltaRadius = totalFlow * 25.0 + 8.0
        maxDeposit = max 2.0 (totalFlow * 6.0)

    in if dist > deltaRadius
       then noModification
       else
       let dotFlow = dx * flowNX + dy * flowNY

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
              deposit = round (maxDeposit * profile)

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

pickDeepest ∷ GeoModification → GeoModification → GeoModification
pickDeepest a b
    | gmElevDelta b < gmElevDelta a = b
    | otherwise                     = a

-----------------------------------------------------------
-- River Evolution Application (pure GeoModification)
-----------------------------------------------------------

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
