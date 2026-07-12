{-# OPTIONS_GHC -fprof-auto #-}
{-# LANGUAGE Strict, UnicodeSyntax #-}
module World.Geology.Timeline.RiverTrace.Subdivide
    ( subdivideLongGaps
    , splitLongSegment
    ) where
import UPrelude
import qualified Data.Vector as V
import World.Types

-- * Long-gap subdivision

-- | Split any consecutive pair of waypoints that are farther apart
--   than maxLen into linearly interpolated sub-points. Elevations
--   are interpolated linearly. This prevents single long straight
--   segments from anomalous grid-path jumps or wrap-boundary residuals.
subdivideLongGaps ∷ Int → [(Int, Int, Int)] → [(Int, Int, Int)]
subdivideLongGaps _ [] = []
subdivideLongGaps _ [x] = [x]
subdivideLongGaps maxLen (p1@(x1, y1, e1) : p2@(x2, y2, e2) : rest) =
    let dx = x2 - x1
        dy = y2 - y1
        dist = round (sqrt (fromIntegral (dx * dx + dy * dy) ∷ Float)) ∷ Int
    in if dist ≤ maxLen
       then p1 : subdivideLongGaps maxLen (p2 : rest)
       else let n = (dist + maxLen - 1) `div` maxLen  -- number of sub-segments
                interps = [ let t = fromIntegral i / fromIntegral n ∷ Float
                                ix = x1 + round (fromIntegral dx * t)
                                iy = y1 + round (fromIntegral dy * t)
                                ie = e1 + round (fromIntegral (e2 - e1) * t)
                            in (ix, iy, ie)
                          | i ← [0 .. n - 1] ]
            in interps ⧺ subdivideLongGaps maxLen (p2 : rest)

-- * Long-segment splitting (segment-level safety net)

-- | Split a single segment into sub-segments if it's longer than maxLen.
--   Interpolates all fields linearly. Returns a singleton vector for
--   short segments (no allocation overhead).
splitLongSegment ∷ Int → RiverSegment → V.Vector RiverSegment
splitLongSegment maxLen seg =
    let GeoCoord sx sy = rsStart seg
        GeoCoord ex ey = rsEnd seg
        dx = ex - sx
        dy = ey - sy
        dist2 = dx * dx + dy * dy
    in if dist2 ≤ maxLen * maxLen
       then V.singleton seg
       else let dist = round (sqrt (fromIntegral dist2 ∷ Float)) ∷ Int
                n = max 2 ((dist + maxLen - 1) `div` maxLen)
                lerp a b t = a + round (fromIntegral (b - a) * t)
            in V.fromList
                [ seg { rsStart     = GeoCoord (lerp sx ex t0) (lerp sy ey t0)
                      , rsEnd       = GeoCoord (lerp sx ex t1) (lerp sy ey t1)
                      , rsStartElev = lerp (rsStartElev seg) (rsEndElev seg) t0
                      , rsEndElev   = lerp (rsStartElev seg) (rsEndElev seg) t1
                      }
                | i ← [0 .. n - 1]
                , let t0 = fromIntegral i / fromIntegral n ∷ Float
                      t1 = fromIntegral (i + 1) / fromIntegral n ∷ Float
                ]
