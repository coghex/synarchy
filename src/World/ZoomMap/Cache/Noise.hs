{-# OPTIONS_GHC -fprof-auto #-}
{-# LANGUAGE Strict, UnicodeSyntax #-}
-- | Zoom-level ice noise, split out of "World.ZoomMap.Cache" (issue
--   #573). Smooth continuous noise for ice-boundary classification at
--   the zoomed-out view, matching the hash used by 'World.Fluid.Ice'
--   and 'World.Fluid.IceLevel' so frozen classification stays
--   consistent across zoom/chunk/grid levels.
module World.ZoomMap.Cache.Noise
    ( zoomIceNoise
    ) where

import UPrelude

-- | Smooth noise for zoom-level ice boundaries.
--   Uses larger scales than tile-level noise so the ice edge
--   is smooth at the zoomed-out view. Returns ±2°C.
zoomIceNoise ∷ Word64 → Int → Int → Float
zoomIceNoise seed gx gy =
    let -- Must match iceNoise (Ice.hs) and iceLevelNoise (IceLevel.hs)
        -- so frozen classification is consistent across zoom/chunk/grid.
        h1 = zoomIceHash seed gx gy 12
        h2 = zoomIceHash seed gx gy 5
        n1 = (zoomHashToFloat h1 - 0.5) * 3.0
        n2 = (zoomHashToFloat h2 - 0.5) * 1.0
    in n1 + n2

zoomIceHash ∷ Word64 → Int → Int → Int → Word64
zoomIceHash seed gx gy scale =
    let fx = fromIntegral gx / fromIntegral scale ∷ Float
        fy = fromIntegral gy / fromIntegral scale ∷ Float
        ix = floor fx ∷ Int
        iy = floor fy ∷ Int
        tx = fx - fromIntegral ix
        ty = fy - fromIntegral iy
        sx = zoomSmoothstep tx
        sy = zoomSmoothstep ty
        v00 = zoomTileHash seed ix       iy
        v10 = zoomTileHash seed (ix + 1) iy
        v01 = zoomTileHash seed ix       (iy + 1)
        v11 = zoomTileHash seed (ix + 1) (iy + 1)
        f00 = zoomHashToFloat v00
        f10 = zoomHashToFloat v10
        f01 = zoomHashToFloat v01
        f11 = zoomHashToFloat v11
        top    = f00 + sx * (f10 - f00)
        bottom = f01 + sx * (f11 - f01)
        result = top + sy * (bottom - top)
    in round (result * fromIntegral (0xFFFFFF ∷ Int)) ∷ Word64

zoomTileHash ∷ Word64 → Int → Int → Word64
zoomTileHash seed x y =
    let h0 = seed `xor` 0x1CE1CE1CE
        h1 = h0 `xor` (fromIntegral x * 0x517cc1b727220a95)
        h2 = h1 `xor` (fromIntegral y * 0x6c62272e07bb0142)
        h3 = h2 `xor` (h2 `shiftR` 33)
        h4 = h3 * 0xff51afd7ed558ccd
        h5 = h4 `xor` (h4 `shiftR` 33)
    in h5

zoomHashToFloat ∷ Word64 → Float
zoomHashToFloat h = fromIntegral (h .&. 0x00FFFFFF) / fromIntegral (0x00FFFFFF ∷ Word64)

zoomSmoothstep ∷ Float → Float
zoomSmoothstep t = t * t * (3.0 - 2.0 * t)
