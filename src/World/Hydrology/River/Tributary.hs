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
          d = max 3 (round (flow * 8.0))
          wd = let raw = max 2 (round (1.0 + flow * 3.0))
               in min raw (max 2 (d - 1))
      in RiverSegment
          { rsStart       = GeoCoord wx1 wy1
          , rsEnd         = GeoCoord wx2 wy2
          , rsWidth       = w
          , rsValleyWidth = w * 3
          , rsDepth       = d
          , rsFlowRate    = flow
          , rsStartElev   = se
          , rsEndElev     = ee
          , rsWaterStart  = se - d + wd
          , rsWaterEnd    = ee - d + wd
          }
      ) [0..] (zip waypoints (drop 1 waypoints))
    where -- Interpolate waypoints along the line with coherent noise offsets.
          -- Uses cumulative noise to create smooth curves, not independent
          -- random offsets (which create jagged zig-zags on short tributaries).
          waypoints = [ let t = fromIntegral i / fromIntegral numSegs ∷ Float
                            baseX = fromIntegral srcX + t * fromIntegral (bx - srcX)
                            baseY = fromIntegral srcY + t * fromIntegral (by - srcY)
                            -- Perpendicular direction
                            dx = fromIntegral (bx - srcX) ∷ Float
                            dy = fromIntegral (by - srcY) ∷ Float
                            len = sqrt (dx * dx + dy * dy)
                            perpX = if len > 0.001 then -dy / len else 0.0
                            perpY = if len > 0.001 then  dx / len else 0.0
                            -- Endpoint taper: no offset at source (i=0) and mouth
                            endTaper = sin (t * 3.14159) -- 0 at ends, 1 in middle
                            -- Coherent noise via smoothly interpolated hash chain
                            h1 = hashGeo seed (fidInt * 7 + floor (t * 3.0)) 861
                            h2 = hashGeo seed (fidInt * 7 + floor (t * 3.0) + 1) 861
                            frac = t * 3.0 - fromIntegral (floor (t * 3.0) ∷ Int)
                            sm = frac * frac * (3.0 - 2.0 * frac) -- smoothstep
                            noiseVal = (hashToFloatGeo h1 * (1.0 - sm)
                                      + hashToFloatGeo h2 * sm) - 0.5
                            offset = noiseVal * len * 0.3 * endTaper
                        in ( round (baseX + perpX * offset)
                           , round (baseY + perpY * offset) )
                      | i ← [0 .. numSegs]
                      ]

