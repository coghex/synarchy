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
import World.Constants (seaLevel)
import World.Hydrology.Types
import World.Geology.Types
import World.Geology.Hash

-- * River Carving (pure GeoModification)

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
-- DEBUG STEP 1: carving only, no delta deposit
applyRiverCarve river worldSize gx gy baseElev =
    let mSeed = rpMeanderSeed river
        bestCarve = findDeepestCarve worldSize gx gy mSeed baseElev
                                     (rpSegments river)
    in bestCarve

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

-- * Target-Based Segment Carving (sloped banks)

-- | Compute the carving modification from a single river segment.
--
--   TARGET-BASED with a LATERAL SLOPE: the target elevation at each
--   tile is computed as channelFloor + slopeRate × (perpDist − channelHalfW),
--   producing a flat-bottomed channel that rises with a constant slope
--   on either side. We only LOWER terrain — if natural terrain is
--   already below the target, no carve. This means:
--
--     * Flat terrain near the channel gets gently sloped banks
--       (natural cliffs only form if terrain rises faster than slopeRate).
--     * Steep terrain (mountain riversides) keeps its natural slope above
--       the channel, since the slope-based target is below the natural
--       terrain there.
--     * The water table fills up to channelFloor, so the visible water
--       surface and the carved bank are co-designed.
--
--   slopeRate is chosen so the bank rises ~1 tile vertically per 2 tiles
--   horizontal — gentle enough to walk up, steep enough to feel like a
--   river valley.
--
--   refSurface = lerp(rsStartElev, rsEndElev, clampedT) and
--   channelFloor = max(seaLevel − 1, floor(refSurface − rsDepth)).
carveFromSegment ∷ Int → Int → Int → Word64 → RiverSegment → Int → GeoModification
carveFromSegment worldSize gx gy _meanderSeed seg baseElev =
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

           -- Signed perpendicular distance from segment axis
           signedPerp = px * ny - py * nx
           effectivePerpDist = abs signedPerp

           -- Segment geometry
           channelHalfW = fromIntegral (rsWidth seg) / 2.0 ∷ Float
           valleyHalfW  = fromIntegral (rsValleyWidth seg) / 2.0 ∷ Float
           depthI       = rsDepth seg

           -- Interpolated channel floor along the segment.
           tClamped = max 0.0 (min 1.0 alongT) ∷ Float
           startE = fromIntegral (rsStartElev seg) ∷ Float
           endE   = fromIntegral (rsEndElev seg) ∷ Float
           interpElev = startE + tClamped * (endE - startE)
           rawFloor = floor (interpElev - fromIntegral depthI) ∷ Int
           channelFloor = max (seaLevel - 1) rawFloor

           -- Lateral slope: 1 vertical per 2 horizontal. Gentle enough
           -- to walk, steep enough to feel like a river valley. Constant
           -- across segments — slope shape doesn't depend on flow rate
           -- (which controls channel width/depth, not bank steepness).
           slopeRate = 0.5 ∷ Float

           -- Coastal segments (ending near or below sea level) carry
           -- the carve past the segment endpoint so the channel reaches
           -- the ocean. Without this overshoot the un-carved sandbar
           -- terrain sits above the channel floor (≈ seaLevel-1) and no
           -- fluid is placed. Visible symptom: rivers "stop short" of
           -- the ocean.
           isCoastalSeg = rsEndElev seg ≤ seaLevel + 5
           downstreamOver = if isCoastalSeg
                            then min 2.0 (12.0 / segLen)
                            else 0.05

       in if alongT < -0.05 ∨ alongT > 1.0 + downstreamOver
            ∨ effectivePerpDist > valleyHalfW
          then noModification
          else
          let -- Unified target: flat channel bottom, sloped banks beyond.
              distOutsideChannel = max 0.0
                  (effectivePerpDist - channelHalfW)
              rawTarget = channelFloor
                        + floor (slopeRate * distOutsideChannel ∷ Float)
              targetElev = max (seaLevel - 1) rawTarget
              carve = baseElev - targetElev
              -- Alluvium thickness scales with how much we actually
              -- carved at this tile (channel center deepest, banks shallow).
              alluviumDepth = max 1 (depthI * 2 `div` 5)
              thisAlluvium
                | effectivePerpDist < channelHalfW = alluviumDepth
                | otherwise = max 1 ((depthI * 3 `div` 10)
                                   - floor (slopeRate * distOutsideChannel))
          in if carve ≤ 0
             then noModification
             else GeoModification
                 { gmElevDelta        = negate carve
                 , gmMaterialOverride = Just (unMaterialId matSandstone)
                 , gmIntrusionDepth   = min thisAlluvium carve
                 }

-- * Target-Based Delta Deposit

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

        -- Cap delta size to prevent enormous flat plateaus from
        -- high-flow rivers in wet climates (totalFlow can reach 7+).
        deltaRadius = min 40.0 (totalFlow * 12.0 + 8.0)
        maxDeposit = min 6.0 (max 2.0 (totalFlow * 3.0))

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
              -- Absolute target elevation for the delta surface.
              -- Use floor to avoid round's half-to-even creating
              -- alternating elevation steps at transition boundaries.
              targetElev = floor (mouthElev + maxDeposit * profile) ∷ Int
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

-- * Helpers

-- | Pick the deeper of two modifications (more negative delta wins).
pickDeepest ∷ GeoModification → GeoModification → GeoModification
pickDeepest a b
    | gmElevDelta b < gmElevDelta a = b
    | otherwise                     = a

-- * River Evolution Application (pure GeoModification)

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
