{-# LANGUAGE Strict, UnicodeSyntax #-}
module World.Hydrology.River.Tributary
    ( buildTributarySegments
    ) where

import UPrelude
import Data.Word (Word64)
import World.Base (GeoCoord(..))
import World.Hydrology.Types
import World.Geology.Hash

-----------------------------------------------------------
-- Tributary Segment Builder
-----------------------------------------------------------

-- | Build segments for a tributary flowing from (srcX,srcY)
--   to the branch point (bx,by), divided into numSegs pieces.
--   Each waypoint is offset slightly from the straight line
--   for natural look.
buildTributarySegments ∷ Word64 → Int → Int → Int → Int → Int → Int
                       → [RiverSegment]
buildTributarySegments seed fidInt srcX srcY bx by numSegs =
    let -- Interpolate waypoints along the line with hash-based offsets
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
        pairs = zip waypoints (tail waypoints)
    in zipWith (\segI ((wx1, wy1), (wx2, wy2)) →
        let flow = 0.1 + fromIntegral segI * 0.05
            w = max 2 (round (flow * 6.0))
        in RiverSegment
            { rsStart       = GeoCoord wx1 wy1
            , rsEnd         = GeoCoord wx2 wy2
            , rsWidth       = w
            , rsValleyWidth = w * 3
            , rsDepth       = max 3 (round (flow * 8.0))
            , rsFlowRate    = flow
            , rsStartElev   = 0
            , rsEndElev     = 0
            }
        ) [0..] pairs
