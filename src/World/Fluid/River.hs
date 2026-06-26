{-# LANGUAGE Strict, UnicodeSyntax #-}

-- | Helpers around river data for chunk generation and zoom rendering.
--
--   The full chunk-level river-fluid placement used to live here; it
--   has been replaced by the water-table-driven pipeline in
--   `World.Generate.Chunk` (see @src\/World\/Hydrology\/DESIGN.md@).
--   What remains is a small surface of utilities that are still useful:
--
--     * @riverNearChunk@ — proximity test used by the channel-mask
--       compute to cull segments that don't overlap a chunk.
--     * @fixupSegmentContinuity@ — geometric/elevation continuity for
--       the segment polyline. Water surfaces are no longer carried on
--       segments; we just enforce that @rsEnd / rsEndElev@ of one
--       segment matches @rsStart / rsStartElev@ of the next, and
--       that elevation is monotonically non-increasing downstream.
module World.Fluid.River
    ( fixupSegmentContinuity
    , riverNearChunk
    ) where

import UPrelude
import qualified Data.Vector as V
import World.Base
import World.Types
import World.Fluid.Internal
import World.Hydrology.Types (RiverParams(..), RiverSegment(..))

-- * River Proximity

-- | Check if any part of the river is near this chunk. Used by the
--   channel-mask compute in `World.Generate.Chunk` to cull segments
--   far from the chunk's bordered region.
riverNearChunk ∷ Int → Int → Int → RiverParams → Bool
riverNearChunk worldSize chunkGX chunkGY river =
    V.any (segOrWaypointNear worldSize chunkGX chunkGY) (rpSegments river)
  where
    segOrWaypointNear ws cgx cgy seg =
        let margin = rsValleyWidth seg + chunkSize + chunkSize
            cx = cgx + chunkSize `div` 2
            cy = cgy + chunkSize `div` 2
            GeoCoord sx sy = rsStart seg
            (dxsi, dysi) = wrappedDeltaUVFluid ws sx sy cx cy
            startNear = abs dxsi < margin ∧ abs dysi < margin
            GeoCoord ex ey = rsEnd seg
            (dxei, dyei) = wrappedDeltaUVFluid ws ex ey cx cy
            endNear = abs dxei < margin ∧ abs dyei < margin
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

-- * Segment Continuity

-- | Stitch a segment polyline so adjacent segments share endpoints.
--
--   Each segment's @rsStart@ is forced to match the previous segment's
--   @rsEnd@, and @rsStartElev@ likewise — so the polyline is closed
--   end-to-end with continuous reference elevations. We also enforce
--   monotonic non-increasing elevation downstream (a segment can't
--   end higher than it starts; that would imply water flows uphill).
--
--   Before the Phase B rework this also stitched the legacy
--   @rsWaterStart@\/@rsWaterEnd@ fields. Those have been removed; the
--   water surface is now derived per-chunk by the water-table compute
--   from segment geometry alone (see DESIGN.md §5.4).
fixupSegmentContinuity ∷ V.Vector RiverSegment → V.Vector RiverSegment
fixupSegmentContinuity v
    | V.length v ≤ 1 = v
    | otherwise = V.fromList (V.head v : go (V.head v) (V.toList (V.tail v)))
  where
    go _ [] = []
    go prev (cur : xs) =
        let fixed = cur { rsStartElev = rsEndElev prev
                        , rsStart     = rsEnd prev
                        }
            fixed' = if rsEndElev fixed > rsStartElev fixed
                     then fixed { rsEndElev = rsStartElev fixed }
                     else fixed
        in fixed' : go fixed' xs
