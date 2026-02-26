{-# LANGUAGE Strict, UnicodeSyntax #-}
module World.Fluid.River
    ( computeChunkRivers
    , fixupSegmentContinuity
    ) where

import UPrelude
import Data.Word (Word64)
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as MV
import qualified Data.Vector.Unboxed as VU
import Control.Monad (forM_)
import Control.Monad.ST (ST)
import World.Base
import World.Types
import World.Plate (TectonicPlate(..))
import World.Fluid.Types (FluidCell(..), FluidType(..))
import World.Fluid.Internal
import World.Hydrology.Types (HydroFeature(..), RiverParams(..)
                             , RiverSegment(..))

-----------------------------------------------------------
-- Chunk-Level River Computation
-----------------------------------------------------------

computeChunkRivers ∷ [PersistentFeature] → Word64 → [TectonicPlate]
                   → Int → ChunkCoord
                   → VU.Vector Int
                   → FluidMap
computeChunkRivers features _seed _plates worldSize coord surfaceMap =
    let ChunkCoord cx cy = coord
        chunkMinGX = cx * chunkSize
        chunkMinGY = cy * chunkSize
        nearbyRivers = filter (isNearbyRiver worldSize chunkMinGX chunkMinGY) features
    in withFluidMap $ \mv →
        forM_ nearbyRivers $ \pf →
            fillRiverFromFeature mv pf worldSize chunkMinGX chunkMinGY surfaceMap

-----------------------------------------------------------
-- River Proximity
-----------------------------------------------------------

isNearbyRiver ∷ Int → Int → Int → PersistentFeature → Bool
isNearbyRiver worldSize chunkGX chunkGY pf =
    case pfFeature pf of
        HydroShape (RiverFeature river) →
            case pfActivity pf of
                FActive  → riverNearChunk worldSize chunkGX chunkGY river
                FDormant → riverNearChunk worldSize chunkGX chunkGY river
                _        → False
        _ → False

-- | Check if any part of the river is near this chunk.
--   Uses proper point-to-segment distance for each segment,
--   plus waypoint proximity checks for bends.
--   Generous margin ensures we never miss a chunk.
riverNearChunk ∷ Int → Int → Int → RiverParams → Bool
riverNearChunk worldSize chunkGX chunkGY river =
    V.any (segOrWaypointNear worldSize chunkGX chunkGY) (rpSegments river)
  where
    segOrWaypointNear ws cgx cgy seg =
        let margin = rsValleyWidth seg + chunkSize + chunkSize
            -- chunk center
            cx = cgx + chunkSize `div` 2
            cy = cgy + chunkSize `div` 2

            -- Segment start proximity
            GeoCoord sx sy = rsStart seg
            (dxsi, dysi) = wrappedDeltaUVFluid ws sx sy cx cy
            dxs = abs dxsi
            dys = abs dysi
            startNear = dxs < margin ∧ dys < margin

            -- Segment end proximity
            GeoCoord ex ey = rsEnd seg
            (dxei, dyei) = wrappedDeltaUVFluid ws ex ey cx cy
            dxe = abs dxei
            dye = abs dyei
            endNear = dxe < margin ∧ dye < margin

            -- Closest point on segment to chunk center
            (fdxi, fdyi) = wrappedDeltaUVFluid ws sx sy ex ey
            fdx = fromIntegral fdxi ∷ Float
            fdy = fromIntegral fdyi ∷ Float
            segLen2 = fdx * fdx + fdy * fdy
            midNear = if segLen2 < 1.0
                then startNear
                else let (pxi, pyi) = wrappedDeltaUVFluid ws sx sy cx cy
                         px = fromIntegral pxi ∷ Float
                         py = fromIntegral pyi ∷ Float
                         t = max 0.0 (min 1.0 ((px * fdx + py * fdy) / segLen2))
                         closestX = t * fdx
                         closestY = t * fdy
                         distX = px - closestX
                         distY = py - closestY
                         dist = sqrt (distX * distX + distY * distY)
                     in dist < fromIntegral margin

        in startNear ∨ endNear ∨ midNear

-----------------------------------------------------------
-- River Fluid Fill
-----------------------------------------------------------

fillRiverFromFeature ∷ MV.MVector s (Maybe FluidCell)
                     → PersistentFeature → Int → Int → Int
                     → VU.Vector Int
                     → ST s ()
fillRiverFromFeature mv pf worldSize chunkGX chunkGY surfaceMap =
    case pfFeature pf of
        HydroShape (RiverFeature river) →
            let segments = rpSegments river
                meanderSeed = rpMeanderSeed river
            in forEachSurface surfaceMap $ \idx lx ly surfZ →
                case bestRiverFill worldSize (chunkGX + lx) (chunkGY + ly)
                                   surfZ meanderSeed segments of
                    Nothing → pure ()
                    Just fc → MV.write mv idx (Just fc)
        _ → pure ()

-- | At overlapping regions, pick the higher water surface.
--   A tile might be claimed by two segments — we want the
--   one that actually covers this tile with more water.
pickBestFill ∷ Maybe FluidCell → Maybe FluidCell → Maybe FluidCell
pickBestFill Nothing b = b
pickBestFill a Nothing = a
pickBestFill (Just a) (Just b) =
    if fcSurface b > fcSurface a then Just b else Just a

bestRiverFill ∷ Int → Int → Int → Int → Word64 → V.Vector RiverSegment
              → Maybe FluidCell
bestRiverFill worldSize gx gy surfZ meanderSeed segments =
    let -- Check each segment
        segResults = V.toList $ V.map (riverFillFromSegment worldSize gx gy surfZ meanderSeed) segments
        -- Check each waypoint (joint between segments)
        waypointResults = V.toList (V.map (riverFillFromWaypoint worldSize gx gy surfZ) segments)
                       <> if V.null segments then []
                          else [riverFillFromEndpoint worldSize gx gy surfZ (V.last segments)]
    in foldl' pickBestFill Nothing (segResults <> waypointResults)

-- | Water depth in tiles above the channel floor, based on flow rate.
--   Small streams: 1-2 tiles of water
--   Large rivers: 3-5 tiles of water
riverWaterDepth ∷ Float → Int
riverWaterDepth flow = max 1 (round (1.0 + flow * 2.5))

riverFillFromSegment ∷ Int → Int → Int → Int → Word64 → RiverSegment
                     → Maybe FluidCell
riverFillFromSegment worldSize gx gy surfZ _meanderSeed seg =
    let GeoCoord sx sy = rsStart seg
        GeoCoord ex ey = rsEnd seg
        (dxi, dyi) = wrappedDeltaUVFluid worldSize sx sy ex ey
        dx' = fromIntegral dxi ∷ Float
        dy' = fromIntegral dyi ∷ Float
        segLen2 = dx' * dx' + dy' * dy'
    in if segLen2 < 1.0
       then Nothing
       else
       let segLen = sqrt segLen2
           (pxi, pyi) = wrappedDeltaUVFluid worldSize sx sy gx gy
           px = fromIntegral pxi ∷ Float
           py = fromIntegral pyi ∷ Float
           tRaw = (px * dx' + py * dy') / segLen2
       in if tRaw < 0.0 ∨ tRaw > 1.0
          then Nothing
          else
          let closestX = tRaw * dx'
              closestY = tRaw * dy'
              perpX = px - closestX
              perpY = py - closestY
              perpDist = sqrt (perpX * perpX + perpY * perpY)
              channelHalfW = fromIntegral (rsWidth seg) / 2.0 ∷ Float
          in if perpDist > channelHalfW
             then Nothing
             else
             let waterDepth = riverWaterDepth (rsFlowRate seg)
                 waterSurface = surfZ + waterDepth
             in Just (FluidCell River waterSurface)

riverFillFromWaypoint ∷ Int → Int → Int → Int → RiverSegment
                      → Maybe FluidCell
riverFillFromWaypoint worldSize gx gy surfZ seg =
    let GeoCoord wx wy = rsStart seg
        (dxi, dyi) = wrappedDeltaUVFluid worldSize wx wy gx gy
        dx = fromIntegral dxi ∷ Float
        dy = fromIntegral dyi ∷ Float
        dist = sqrt (dx * dx + dy * dy)
        channelHalfW = fromIntegral (rsWidth seg) / 2.0 ∷ Float
    in if dist > channelHalfW
       then Nothing
       else
       let waterDepth = riverWaterDepth (rsFlowRate seg)
           waterSurface = surfZ + waterDepth
       in Just (FluidCell River waterSurface)

riverFillFromEndpoint ∷ Int → Int → Int → Int → RiverSegment
                      → Maybe FluidCell
riverFillFromEndpoint worldSize gx gy surfZ seg =
    let GeoCoord wx wy = rsEnd seg
        (dxi, dyi) = wrappedDeltaUVFluid worldSize wx wy gx gy
        dx = fromIntegral dxi ∷ Float
        dy = fromIntegral dyi ∷ Float
        dist = sqrt (dx * dx + dy * dy)
        channelHalfW = fromIntegral (rsWidth seg) / 2.0 ∷ Float
    in if dist > channelHalfW
       then Nothing
       else
       let waterDepth = riverWaterDepth (rsFlowRate seg)
           waterSurface = surfZ + waterDepth
       in Just (FluidCell River waterSurface)

-----------------------------------------------------------
-- Segment Continuity
-----------------------------------------------------------

-- | Ensure adjacent segments share consistent endpoint elevations.
--   After any modification (meander, deepen, etc.), the end of
--   segment N must equal the start of segment N+1.
--   Also enforces monotonic descent on water surface.
fixupSegmentContinuity ∷ V.Vector RiverSegment → V.Vector RiverSegment
fixupSegmentContinuity v
    | V.length v ≤ 1 = v
    | otherwise = V.fromList (go (V.head v) (V.toList (V.tail v)))
  where
    go _ [] = []
    go prev (cur : xs) =
        let fixed = cur { rsStartElev = rsEndElev prev
                        , rsStart     = rsEnd prev }
            fixed' = if rsEndElev fixed > rsStartElev fixed
                     then fixed { rsEndElev = rsStartElev fixed }
                     else fixed
        in fixed' : go fixed' xs
