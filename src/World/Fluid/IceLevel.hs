{-# LANGUAGE Strict, UnicodeSyntax #-}
module World.Fluid.IceLevel
    ( computeIceLevelGrid
    , lookupIceLevel
    ) where

import UPrelude
import Data.Word (Word64)
import Data.Bits (xor, shiftR, (.&.))
import qualified Data.Vector.Unboxed as VU
import World.Constants (seaLevel)
import World.Fluid.Types (IceLevelGrid(..))
import World.Hydrology.Simulation (ElevGrid(..), fillDepressions)
import World.Plate (TectonicPlate, elevationAtGlobal, isBeyondGlacier, isGlacierZone, wrapGlobalU)
import World.Weather.Types (ClimateState)
import World.Weather.Lookup (lookupLocalClimate, LocalClimate(..))

-- | Compute the global ice surface level grid.
--   Classifies frozen cells on the ElevGrid, builds a masked grid
--   with non-frozen cells as barriers, runs fillDepressions to find
--   basin fill levels.
computeIceLevelGrid ∷ Word64 → Int → [TectonicPlate] → ClimateState
                    → ElevGrid → IceLevelGrid
computeIceLevelGrid seed worldSize plates climate grid =
    let gridW = egGridW grid
        totalSamples = gridW * gridW
        gxVec = egGX grid
        gyVec = egGY grid
        elevVec = egElev grid

        -- Classify each grid sample as frozen or not, using the same
        -- logic as computeChunkIce (lapse rate, ocean penalty, noise).
        frozenVec = VU.generate totalSamples $ \idx →
            let gx = gxVec VU.! idx
                gy = gyVec VU.! idx
                (gx', gy') = wrapGlobalU worldSize gx gy
            in if isBeyondGlacier worldSize gx' gy'
               then False
               else if isGlacierZone worldSize gx' gy'
               then True  -- world boundary always frozen
               else let LocalClimate{ lcTemp = meanT
                                    , lcSummerTemp = summerT
                                    , lcWinterTemp = winterT } =
                            lookupLocalClimate climate worldSize gx' gy'
                        (globalElev, _) = elevationAtGlobal seed plates worldSize gx' gy'
                        altAboveSea = max 0 (globalElev - seaLevel)
                        lapseRate = 0.065 ∷ Float
                        altCooling = fromIntegral altAboveSea * lapseRate
                        oceanPenalty = if globalElev < seaLevel
                                        then 5.0 else 0.0 ∷ Float
                        noise = iceLevelNoise seed gx' gy'
                        effectiveT = meanT + oceanPenalty - altCooling + noise
                    in effectiveT < -2.0
                     ∨ (winterT - altCooling < -10.0
                        ∧ summerT - altCooling < 5.0)

        -- Build masked elevation grid: frozen cells keep their elev
        -- (ocean → seaLevel), non-frozen cells → maxBound barrier.
        maskedElev = VU.generate totalSamples $ \idx →
            if frozenVec VU.! idx
            then let e = elevVec VU.! idx
                 in if e ≤ seaLevel then seaLevel else e
            else maxBound `div` 2  -- large barrier, avoid overflow in fill

        -- Land = frozen (participates in fill), non-frozen = sink seeds
        maskedLand = frozenVec

        maskedGrid = grid
            { egElev = maskedElev
            , egLand = maskedLand
            }

        -- Run priority-flood depression fill on the masked grid.
        -- This fills basins within the frozen zone to their spill level.
        (filledElev, _flowDir) = fillDepressions maskedGrid

        -- Extract ice levels: frozen cells with valid fill → basin level.
        -- Non-frozen or unfilled cells → -1 (no basin).
        barrierThreshold = maxBound `div` 2 - 1
        levelVec = VU.generate totalSamples $ \idx →
            if frozenVec VU.! idx
               ∧ filledElev VU.! idx < barrierThreshold
            then filledElev VU.! idx
            else -1

    in IceLevelGrid gridW (egSpacing grid) levelVec

-- | Look up the interpolated ice basin level at a tile coordinate.
--   Returns Nothing if the area has no basin ice (drape zone).
lookupIceLevel ∷ IceLevelGrid → Int → Int → Int → Maybe Int
lookupIceLevel ilGrid worldSize gx gy
    | ilGridW ilGrid ≤ 0 = Nothing
    | otherwise =
    let gridW   = ilGridW ilGrid
        spacing = ilSpacing ilGrid
        halfGrid = gridW `div` 2
        totalTiles = worldSize * 16
        halfW = totalTiles `div` 2
        -- Convert (gx, gy) to (u, v) isometric coords
        rawU = gx - gy
        rawV = gx + gy
        -- Wrap u-axis
        u = ((rawU + halfW) `mod` totalTiles + totalTiles) `mod` totalTiles - halfW
        v = rawV
        -- Convert to fractional grid indices
        fxRaw = fromIntegral u / fromIntegral spacing + fromIntegral halfGrid ∷ Float
        fyRaw = fromIntegral v / fromIntegral spacing + fromIntegral halfGrid ∷ Float
        ix0 = floor fxRaw ∷ Int
        iy0 = floor fyRaw ∷ Int
        fx = fxRaw - fromIntegral ix0
        fy = fyRaw - fromIntegral iy0
        -- Wrap ix for u-axis wrapping
        wrapIX i = ((i `mod` gridW) + gridW) `mod` gridW
        clampIY i = max 0 (min (gridW - 1) i)
        -- Sample 4 corners
        sample ix iy =
            let idx = clampIY iy * gridW + wrapIX ix
            in ilLevel ilGrid VU.! idx
        s00 = sample ix0       iy0
        s10 = sample (ix0 + 1) iy0
        s01 = sample ix0       (iy0 + 1)
        s11 = sample (ix0 + 1) (iy0 + 1)
    in let validCount = length (filter (≥ 0) [s00, s10, s01, s11])
       -- Need at least 3 valid corners for a reliable basin level.
       -- With fewer, the tile is at the edge of a frozen basin —
       -- fall through to drape ice for a smooth boundary.
       in if validCount < 3
       then Nothing
       else
       let fb = let vc = filter (≥ 0) [s00, s10, s01, s11]
                in sum vc `div` length vc
           v00 = if s00 < 0 then fb else s00
           v10 = if s10 < 0 then fb else s10
           v01 = if s01 < 0 then fb else s01
           v11 = if s11 < 0 then fb else s11
           top    = fromIntegral v00 + fx * fromIntegral (v10 - v00)
           bottom = fromIntegral v01 + fx * fromIntegral (v11 - v01)
           result = top + fy * (bottom - top) ∷ Float
       in Just (round result)

-- | Deterministic noise for ice boundary variation at grid scale.
--   Same approach as iceNoise in Ice.hs but at coarser scale.
iceLevelNoise ∷ Word64 → Int → Int → Float
iceLevelNoise seed gx gy =
    let h1 = iceLevelHash seed gx gy 12
        h2 = iceLevelHash seed gx gy 5
        n1 = (hashToFloat' h1 - 0.5) * 3.0
        n2 = (hashToFloat' h2 - 0.5) * 1.0
    in n1 + n2

iceLevelHash ∷ Word64 → Int → Int → Int → Word64
iceLevelHash seed gx gy scale =
    let fx = fromIntegral gx / fromIntegral scale ∷ Float
        fy = fromIntegral gy / fromIntegral scale ∷ Float
        ix = floor fx ∷ Int
        iy = floor fy ∷ Int
        tx = fx - fromIntegral ix
        ty = fy - fromIntegral iy
        sx = smoothstep' tx
        sy = smoothstep' ty
        f00 = hashToFloat' (tileHash' seed ix       iy)
        f10 = hashToFloat' (tileHash' seed (ix + 1) iy)
        f01 = hashToFloat' (tileHash' seed ix       (iy + 1))
        f11 = hashToFloat' (tileHash' seed (ix + 1) (iy + 1))
        top    = f00 + sx * (f10 - f00)
        bottom = f01 + sx * (f11 - f01)
        result = top + sy * (bottom - top)
    in round (result * 0xFFFFFF)

tileHash' ∷ Word64 → Int → Int → Word64
tileHash' seed x y =
    let h0 = seed `xor` 0x1CE1CE1CE
        h1 = h0 `xor` (fromIntegral x * 0x517cc1b727220a95)
        h2 = h1 `xor` (fromIntegral y * 0x6c62272e07bb0142)
        h3 = h2 `xor` (h2 `shiftR` 33)
        h4 = h3 * 0xff51afd7ed558ccd
        h5 = h4 `xor` (h4 `shiftR` 33)
    in h5

hashToFloat' ∷ Word64 → Float
hashToFloat' h = fromIntegral (h .&. 0x00FFFFFF) / fromIntegral (0x00FFFFFF ∷ Word64)

smoothstep' ∷ Float → Float
smoothstep' t = t * t * (3.0 - 2.0 * t)
