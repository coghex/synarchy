{-# LANGUAGE Strict, UnicodeSyntax #-}
module World.Hydrology.River.Tributary
    ( buildTributarySegments
    ) where

import UPrelude
import Data.Word (Word64)
import qualified Data.Vector as V
import World.Base (GeoCoord(..))
import World.Hydrology.Types
import World.Geology.Hash

-----------------------------------------------------------
-- Tributary Segment Builder
-----------------------------------------------------------

-- | Build segments for a tributary flowing from (srcX,srcY)
--   to the branch point (bx,by), divided into numSegs pieces.
--   Each waypoint is offset slightly from the straight line
--   for natural look. branchElev is the elevation at the branch
--   point; the source is estimated as higher based on length.
buildTributarySegments ∷ Word64 → Int → Int → Int → Int → Int → Int → Int
                       → V.Vector RiverSegment
buildTributarySegments seed fidInt srcX srcY bx by numSegs branchElev =
    let -- Estimate source elevation as higher than branch point
        dx = fromIntegral (bx - srcX) ∷ Float
        dy = fromIntegral (by - srcY) ∷ Float
        tribLen = sqrt (dx * dx + dy * dy)
        srcElev = branchElev + max 5 (round (tribLen * 0.3))
    in V.fromList $ zipWith (\segI ((wx1, wy1), (wx2, wy2)) →
      let flow = 0.1 + fromIntegral segI * 0.05
          w = max 4 (round (flow * 8.0))
          t1 = fromIntegral segI / fromIntegral numSegs ∷ Float
          t2 = fromIntegral (segI + 1) / fromIntegral numSegs ∷ Float
          se = round (fromIntegral srcElev + t1 * fromIntegral (branchElev - srcElev) ∷ Float)
          ee = round (fromIntegral srcElev + t2 * fromIntegral (branchElev - srcElev) ∷ Float)
      in RiverSegment
          { rsStart       = GeoCoord wx1 wy1
          , rsEnd         = GeoCoord wx2 wy2
          , rsWidth       = w
          , rsValleyWidth = w * 3
          , rsDepth       = max 3 (round (flow * 8.0))
          , rsFlowRate    = flow
          , rsStartElev   = se
          , rsEndElev     = ee
          }
      ) [0..] (zip waypoints (tail waypoints))
    where -- Interpolate waypoints along the line with hash-based offsets
          waypoints = [ let t = fromIntegral i / fromIntegral numSegs ∷ Float
                            baseX = fromIntegral srcX + t * fromIntegral (bx - srcX)
                            baseY = fromIntegral srcY + t * fromIntegral (by - srcY)
                            -- Perpendicular offset for natural curvature
                            dx = fromIntegral (bx - srcX) ∷ Float
                            dy = fromIntegral (by - srcY) ∷ Float
                            len = sqrt (dx * dx + dy * dy)
                            perpX = if len > 0.001 then -dy / len else 0.0
                            perpY = if len > 0.001 then  dx / len else 0.0
                            h = hashGeo seed (fidInt + i) 860
                            offset = (hashToFloatGeo h - 0.5) * len * 0.15
                        in ( round (baseX + perpX * offset)
                           , round (baseY + perpY * offset) )
                      | i ← [0 .. numSegs]
                      ]

