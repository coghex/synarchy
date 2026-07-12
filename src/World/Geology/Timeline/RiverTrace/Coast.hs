{-# OPTIONS_GHC -fprof-auto #-}
{-# LANGUAGE Strict, UnicodeSyntax #-}
module World.Geology.Timeline.RiverTrace.Coast
    ( extendToCoast
    , nearestOceanDir
    ) where
import UPrelude
import qualified Data.Vector.Unboxed as VU
import World.Types
import World.Hydrology.Simulation (ElevGrid(..))

-- * Coast extension

-- | Extend the river path from the last flow-grid waypoint to
--   below-sea-level terrain. The number of extension waypoints
--   scales with the elevation drop, creating:
--     - Deltas at flat coasts (≤3 above sea: 2 steps)
--     - Inlets at moderate coasts (4–30: 3–5 steps)
--     - Gorges at high coasts (30+: up to 10 steps)
--   The carving system then cuts through the terrain along these
--   segments, and fluid fill places water in the carved channel.
extendToCoast ∷ Int → ElevGrid → [(Int, Int, Int)] → [(Int, Int, Int)]
extendToCoast _ _ [] = []
extendToCoast _ _ [x] = [x]
extendToCoast sp eg pts =
    let n = length pts
        (x1, y1, _) = pts !! max 0 (n - 2)
        (x2, y2, e2) = pts !! (n - 1)
    -- Skip extension for inland/glacial dead-ends far above sea level.
    in if e2 > seaLevel + 80
       then pts
       else
        let -- Default direction: continuation of last segment
            dxC = x2 - x1
            dyC = y2 - y1
            lenC = sqrt (fromIntegral (dxC * dxC + dyC * dyC) ∷ Float)

            -- Ocean direction: find nearest ocean cell in the ElevGrid
            -- and steer toward it. This prevents extensions that run
            -- parallel to the coast from continuing parallel.
            (dxO, dyO, oceanDist) = nearestOceanDir eg x2 y2
            lenO = sqrt (fromIntegral (dxO * dxO + dyO * dyO) ∷ Float)

            -- Blend: if ocean is close and we can compute both directions,
            -- use 70% ocean direction + 30% continuation. If ocean direction
            -- is unknown, fall back to continuation only.
            halfSp = max 2 (sp `div` 2)
            (stepX, stepY)
                | lenO > 0.5 ∧ oceanDist < 20 =
                    -- Blend toward ocean
                    let oceanW = 0.7 ∷ Float
                        contW  = 0.3 ∷ Float
                        blendX = contW * (if lenC > 0.5
                                          then fromIntegral dxC / lenC
                                          else 0.0)
                               + oceanW * fromIntegral dxO / lenO
                        blendY = contW * (if lenC > 0.5
                                          then fromIntegral dyC / lenC
                                          else 0.0)
                               + oceanW * fromIntegral dyO / lenO
                        blendLen = sqrt (blendX * blendX + blendY * blendY)
                    in if blendLen < 0.1
                       then (halfSp, 0)
                       else ( round (blendX / blendLen * fromIntegral halfSp)
                            , round (blendY / blendLen * fromIntegral halfSp) )
                | lenC > 0.5 =
                    ( round (fromIntegral dxC / lenC * fromIntegral halfSp)
                    , round (fromIntegral dyC / lenC * fromIntegral halfSp) )
                | otherwise = (halfSp, 0)

            -- Elevation above sea level determines extension length.
            drop' = max 0 (e2 - seaLevel)
            numSteps = if drop' ≤ 3 then 2
                       else min 10 (max 3 (drop' `div` 6 + 2))
            extensionPts =
                [ ( x2 + stepX * i
                  , y2 + stepY * i
                  , max seaLevel (e2 - (drop' * i) `div` numSteps)
                  )
                | i ← [1 .. numSteps]
                ]
            lastX = x2 + stepX * numSteps
            lastY = y2 + stepY * numSteps
        in pts ⧺ extensionPts
                ⧺ [ (lastX + stepX,     lastY + stepY,     seaLevel)
                  , (lastX + stepX * 2, lastY + stepY * 2, seaLevel - 1) ]

-- | Find the direction toward the nearest ocean cell from (gx, gy).
--   Scans the ElevGrid for the closest non-land cell.
--   Returns (dx, dy, distance) where dx/dy point toward ocean.
--
--   The grid is a (u, v) lattice with `egSpacing` between samples.
--   Algebra: at the target's nearest grid index `(ixC, iyC)`, a
--   grid cell `(ix, iy)`'s `(dxg, dyg) = (Δix+Δiy, Δiy-Δix) / 2`,
--   so `|dxg|+|dyg| = max(|Δix|, |Δiy|)`. The Manhattan-≤-radius
--   predicate is therefore a square in `(ix, iy)` of half-side
--   `searchRadius`. We bound iteration to a 25×25 window (radius+2
--   buffer for integer-div edge cases) instead of scanning all
--   147K grid cells (audit #16). Predicate inside the loop is
--   unchanged, so candidates are identical.
nearestOceanDir ∷ ElevGrid → Int → Int → (Int, Int, Int)
nearestOceanDir eg gx gy =
    let gridW = egGridW eg
        sp    = max 1 (egSpacing eg)
        landV = egLand eg
        gxV   = egGX eg
        gyV   = egGY eg
        halfGrid = gridW `div` 2
        u = gx - gy
        v = gx + gy
        ixC = (u `div` sp) + halfGrid
        iyC = (v `div` sp) + halfGrid
        searchRadius = 10 ∷ Int
        windowR = searchRadius + 2  -- safety buffer for integer-div rounding
        ix0 = max 0 (ixC - windowR)
        ix1 = min (gridW - 1) (ixC + windowR)
        iy0 = max 0 (iyC - windowR)
        iy1 = min (gridW - 1) (iyC + windowR)
        candidates =
            [ (abs dxg + abs dyg, ogx - gx, ogy - gy)
            | iy ← [iy0 .. iy1]
            , ix ← [ix0 .. ix1]
            , let idx = iy * gridW + ix
            , not (landV VU.! idx)
            , let ogx = gxV VU.! idx
                  ogy = gyV VU.! idx
                  dxg = (ogx - gx) `div` sp
                  dyg = (ogy - gy) `div` sp
            , abs dxg + abs dyg ≤ searchRadius
            ]
    in case candidates of
        [] → (0, 0, maxBound)
        _  → let (dist, dx, dy) = foldl'
                    (\(bd, bx, by) (d, x, y) →
                        if d < bd then (d, x, y) else (bd, bx, by))
                    (maxBound, 0, 0) candidates
             in (dx, dy, dist)
