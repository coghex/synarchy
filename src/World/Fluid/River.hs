{-# LANGUAGE Strict #-}

-- | River segment-geometry helper for the timeline stages.
--
--   The full chunk-level river-fluid placement used to live here. It
--   has been replaced by global river identification
--   (World.Fluid.River.Identify) writing a per-tile surface that
--   World.Generate.Chunk.Fluid.composeFluidMap places at chunk gen —
--   see docs/hydrology_pipeline.md §5 and §6. What remains is segment
--   continuity, used by the timeline river trace and event compaction,
--   not by chunk generation:
--
--     * @fixupSegmentContinuity@ — geometric/elevation continuity for
--       the segment polyline. Water surfaces are no longer carried on
--       segments; we just enforce that @rsEnd / rsEndElev@ of one
--       segment matches @rsStart / rsStartElev@ of the next, and
--       that elevation is monotonically non-increasing downstream.
module World.Fluid.River
    ( fixupSegmentContinuity
    ) where

import UPrelude
import qualified Data.Vector as V
import World.Hydrology.Types (RiverSegment(..))

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
--   rendered water surface no longer comes from segment geometry at
--   all, but from the global river table's per-tile surface (see
--   docs/hydrology_pipeline.md §10).
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
