{-# OPTIONS_GHC -fprof-auto #-}
{-# LANGUAGE Strict, UnicodeSyntax #-}
module World.Geology.Timeline.RiverTrace.Unwrap
    ( unwrapPathCoords
    , rewrapPath
    ) where
import UPrelude
import World.Types

-- * Path unwrapping

-- | Make path coordinates continuous across the u-axis wrap.
--   The flow grid wraps ix (u-axis), so consecutive grid cells may
--   jump from ix=0 to ix=gridW-1 or vice versa.  The egGX/egGY
--   coordinates reflect this jump, creating a discontinuity.
--   Fix: for each consecutive pair, if u = gx-gy jumps by more
--   than half the world width, adjust the second point.
unwrapPathCoords ∷ Int → [(Int, Int, Int)] → [(Int, Int, Int)]
unwrapPathCoords _ [] = []
unwrapPathCoords _ [x] = [x]
unwrapPathCoords worldTiles (p0:rest) =
    p0 : go p0 rest
  where
    halfW = worldTiles `div` 2
    go _ [] = []
    go (px, py, _) ((x, y, e):xs) =
        let prevU = px - py
            curU  = x - y
            v     = x + y
            du    = curU - prevU
            -- If du > halfW, the path jumped across the wrap going positive;
            -- adjust u negative. If du < -halfW, jumped negative; adjust positive.
            adjU  | du > halfW  = curU - worldTiles
                  | du < negate halfW = curU + worldTiles
                  | otherwise   = curU
            -- Recover gx, gy from adjusted u and original v
            x' = (adjU + v) `div` 2
            y' = (v - adjU) `div` 2
            p' = (x', y', e)
        in p' : go p' xs

-- | Re-wrap path coordinates back into canonical u-space.
--   After unwrapping (for continuous noise) and noise displacement,
--   gx/gy may be outside the valid world range.
--
--   IMPORTANT: wraps using a CONSTANT u-offset derived from the
--   first point. The old code wrapped each point independently,
--   which could map consecutive smooth points to opposite sides
--   of the world (u wraps from +halfW to -halfW), creating
--   world-spanning segments that carve straight lines from
--   source to mouth.
--
--   A constant offset preserves path continuity. The path may
--   extend slightly outside [-halfW, halfW) for long rivers,
--   but the carving/fluid code uses wrappedDeltaUV which handles
--   this correctly. The bounding box may be conservatively large
--   for wrap-crossing rivers, but that only affects performance.
rewrapPath ∷ Int → [(Int, Int, Int)] → [(Int, Int, Int)]
rewrapPath _ [] = []
rewrapPath worldSize pts@((x0, y0, _) : _) =
    let w = worldSize * chunkSize
        halfW = w `div` 2
        u0 = x0 - y0
        wrappedU0 = ((u0 + halfW) `mod` w + w) `mod` w - halfW
        uOffset = wrappedU0 - u0
    in map (\(x, y, e) →
        let u = x - y + uOffset
            v = x + y
            x' = (u + v) `div` 2
            y' = (v - u) `div` 2
        in (x', y', e)) pts
