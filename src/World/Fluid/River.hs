{-# LANGUAGE Strict, UnicodeSyntax #-}

-- | Helpers around river data for chunk generation and zoom rendering.
--
--   The full chunk-level river-fluid placement used to live here; it
--   has been replaced by the water-table-driven pipeline in
--   `World.Generate.Chunk` (see @src\/World\/Hydrology\/DESIGN.md@).
--   What remains is segment continuity:
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
