{-# OPTIONS_GHC -fprof-auto #-}
{-# LANGUAGE Strict, UnicodeSyntax #-}
-- | Wrap-exact value/ridge noise, split out of "World.Plate" (issue
--   #560). Used for plate-boundary jitter and elevation texture.
module World.Plate.Noise
    ( wrappedValueNoise2D
    , wrappedRidgeNoise2D
    ) where

import UPrelude
import World.Plate.Hash (hashToFloat', hashCoord, smoothstep, lerp)
import World.Plate.Wrap (worldWidthTiles)

-- * Noise

wrappedValueNoise2D ∷ Word64 → Int → Int → Int → Int → Float
wrappedValueNoise2D seed worldSize gx gy scale =
    let w = worldWidthTiles worldSize
        -- Work in u-space for wrapping. To make the cell grid tile
        -- exactly across the cylindrical seam, snap cellsInU to the
        -- nearest integer count and derive an effectiveScale that
        -- divides w cleanly — otherwise `w mod scale` tiles at the
        -- seam fall into a defective zone whose lerp pairs don't
        -- match the deep-interior wrap (audit #8). The shift in
        -- noise feature size is at most one tile per `scale`, ≪1%
        -- for the scales used here.
        u = gx - gy
        v = gx + gy
        cellsInU = max 1 (round (fromIntegral w / fromIntegral scale ∷ Float))
        effectiveScale = fromIntegral w / fromIntegral cellsInU ∷ Float
        fu = fromIntegral u / effectiveScale ∷ Float
        -- v doesn't wrap, so the integer scale is fine there.
        fv = fromIntegral v / fromIntegral scale ∷ Float
        iu = floor fu ∷ Int
        iv = floor fv ∷ Int
        tu = fu - fromIntegral iu
        tv = fv - fromIntegral iv
        su = smoothstep tu
        sv = smoothstep tv
        wrapIu i = ((i `mod` cellsInU) + cellsInU) `mod` cellsInU
        iu0 = wrapIu iu
        iu1 = wrapIu (iu + 1)
        v00 = hashToFloat' (hashCoord seed iu0     iv)
        v10 = hashToFloat' (hashCoord seed iu1     iv)
        v01 = hashToFloat' (hashCoord seed iu0     (iv + 1))
        v11 = hashToFloat' (hashCoord seed iu1     (iv + 1))
        top    = lerp su v00 v10
        bottom = lerp su v01 v11
    in lerp sv top bottom

-- | Sharp-ridged noise in [0,1]. Standard value noise produces rounded
--   peaks; ridge noise produces sharp creases where the underlying
--   value-noise crosses 0.5. Used as the mountain-peak silhouette.
wrappedRidgeNoise2D ∷ Word64 → Int → Int → Int → Int → Float
wrappedRidgeNoise2D seed worldSize gx gy scale =
    let n = wrappedValueNoise2D seed worldSize gx gy scale
    in 1.0 - 2.0 * abs (n - 0.5)
