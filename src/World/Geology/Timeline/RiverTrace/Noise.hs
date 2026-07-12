{-# OPTIONS_GHC -fprof-auto #-}
{-# LANGUAGE Strict, UnicodeSyntax #-}
module World.Geology.Timeline.RiverTrace.Noise
    ( addPathNoise
    , coherentNoise
    ) where
import UPrelude
import World.Geology.Hash

-- * Path noise

-- | Offset each waypoint perpendicular to the local flow direction
--   using coherent multi-scale noise. Three octaves create natural
--   river bends: large sweeping curves, medium bends, and small
--   wiggles. Noise is spatially coherent (smoothly interpolated
--   between hash values) so adjacent waypoints get similar offsets,
--   producing smooth curves instead of jagged zig-zags.
--
--   Amplitude tapers to zero at the source and mouth to keep
--   endpoints fixed. Also tapers in steep terrain to avoid
--   curving through mountain ridges.
addPathNoise ∷ Word64 → Int → Int → [(Int, Int, Int)] → [(Int, Int, Int)]
addPathNoise _ _ _ [] = []
addPathNoise _ _ _ [x] = [x]
addPathNoise seed riverIdx spacing pts =
    let len = length pts
        maxIdx = len - 1

        -- Cumulative arc length along the path (normalized to [0..len-1])
        arcLens = scanl (\acc ((x1,y1,_),(x2,y2,_)) →
            let dx = fromIntegral (x2 - x1) ∷ Float
                dy = fromIntegral (y2 - y1) ∷ Float
            in acc + sqrt (dx * dx + dy * dy)
            ) (0.0 ∷ Float) (zip pts (drop 1 pts))

        totalArc = case reverse arcLens of
            (a:_) → max 1.0 a
            []    → 1.0

        -- Flow direction at each point: vector to next point
        dirs = zipWith (\(x1, y1, _) (x2, y2, _) → (x2 - x1, y2 - y1))
                       pts (drop 1 pts)
               <> [(0, 0)]

        -- Maximum perpendicular offset scales with grid spacing.
        -- Larger amplitude creates natural-looking meanders in flat
        -- terrain. Slope taper (below) reduces this in mountains.
        maxOff = min 28.0 (fromIntegral spacing * 2.5) ∷ Float

        noisy = zipWith (\(i, (x, y, e), (dx, dy)) arcLen →
            if i ≡ 0 ∨ i ≡ maxIdx
            then (x, y, e)
            else let -- Endpoint taper: ramp from 0 at ends to 1 in middle
                     endT = fromIntegral (min i (maxIdx - i))
                          / fromIntegral (max 1 (maxIdx `div` 4))
                     taper = min 1.0 endT

                     -- Terrain steepness taper: reduce noise on steep slopes
                     -- so rivers don't curve sideways through ridges
                     slopeDelta = case drop 1 (drop (i-1) pts) of
                         ((_,_,e1):(_,_,e2):_) → abs (e1 - e2)
                         _ → 0
                     slopeTaper = max 0.3 (1.0 - fromIntegral slopeDelta / 15.0)

                     -- Coherent noise: 3 octaves at different frequencies.
                     -- Higher frequencies create more bends per river length,
                     -- preventing the "straight line" look from coarse grids.
                     t = arcLen / totalArc
                     octave1 = coherentNoise seed (riverIdx * 3)     (t * 4.0)  -- large bends
                     octave2 = coherentNoise seed (riverIdx * 3 + 1) (t * 10.0) -- medium curves
                     octave3 = coherentNoise seed (riverIdx * 3 + 2) (t * 22.0) -- small wiggles

                     -- Combine octaves with decreasing amplitude
                     noise = octave1 * 0.55 + octave2 * 0.30 + octave3 * 0.15

                     -- Final offset
                     amplitude = maxOff * taper * slopeTaper

                     -- Perpendicular to flow direction
                     dLen = sqrt (fromIntegral (dx * dx + dy * dy) ∷ Float)
                     (perpX, perpY) = if dLen < 0.001
                         then (0.0, 0.0)
                         else ( negate (fromIntegral dy) / dLen
                              , fromIntegral dx / dLen )
                     offX = round (perpX * noise * amplitude) ∷ Int
                     offY = round (perpY * noise * amplitude) ∷ Int
                 in (x + offX, y + offY, e)
            ) (zip3 [0 ∷ Int ..] pts dirs) arcLens
    in noisy

-- | 1D coherent noise: smoothly interpolated between hash values.
--   Returns a value in [-1, 1]. The input t is a continuous position;
--   hash values are computed at integer positions and interpolated
--   with smoothstep for C1 continuity.
coherentNoise ∷ Word64 → Int → Float → Float
coherentNoise seed idx t =
    let ti  = floor t ∷ Int
        frac = t - fromIntegral ti
        -- Hash at integer positions
        h0 = hashToFloatGeo (hashGeo seed (idx * 10000 + ti)     1400) * 2.0 - 1.0
        h1 = hashToFloatGeo (hashGeo seed (idx * 10000 + ti + 1) 1400) * 2.0 - 1.0
        -- Smoothstep interpolation
        s = smoothstepGeo frac
    in h0 * (1.0 - s) + h1 * s
