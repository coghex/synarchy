{-# LANGUAGE Strict, UnicodeSyntax #-}
module World.Magma.Shape
    ( pointInShape
    , shapeZBottom
    , shapeZTop
    ) where

import UPrelude
import World.Geology.Hash (wrappedDeltaUV, hashGeo, hashToFloatGeo)
import World.Magma.Types (LavaShape(..))

-- | True iff @(gx, gy, z)@ lies inside the shape. All horizontal
--   distance comparisons go through 'wrappedDeltaUV' so the u-axis
--   world wrap is respected for any shape whose centre is on the
--   opposite side of the seam.
pointInShape ∷ Int → Int → Int → Int → LavaShape → Bool
pointInShape worldSize gx gy z shape = case shape of

    Cylindrical x y zb zt r →
        if z < zb ∨ z > zt then False
        else let (dx, dy) = wrappedDeltaUV worldSize gx gy x y
                 d2 = fromIntegral (dx*dx + dy*dy) ∷ Float
             in d2 ≤ r * r

    Conical x y zb zt rb rt →
        if z < zb ∨ z > zt then False
        else let zb' = fromIntegral zb ∷ Float
                 zt' = fromIntegral zt ∷ Float
                 zf  = fromIntegral z  ∷ Float
                 t = if zt > zb then (zf - zb') / (zt' - zb') else 0.0
                 r = rb + t * (rt - rb)
                 (dx, dy) = wrappedDeltaUV worldSize gx gy x y
                 d2 = fromIntegral (dx*dx + dy*dy) ∷ Float
             in d2 ≤ r * r

    Perturbed x y zb zt br amp freq phase →
        if z < zb ∨ z > zt then False
        else let zf = fromIntegral z ∷ Float
                 -- Perturbed centre wobbles around (x, y) as a function of z.
                 ox = amp * sin (zf * freq + phase)
                 oy = amp * cos (zf * freq + phase)
                 cxI = x + round ox
                 cyI = y + round oy
                 (dx, dy) = wrappedDeltaUV worldSize gx gy cxI cyI
                 -- Reinject the fractional part of the centre offset.
                 dxR = fromIntegral dx - (ox - fromIntegral (round ox ∷ Int))
                 dyR = fromIntegral dy - (oy - fromIntegral (round oy ∷ Int))
             in dxR*dxR + dyR*dyR ≤ br * br

    Slot sx sy ex ey zb zt w →
        if z < zb ∨ z > zt then False
        else
            let (eDx, eDy) = wrappedDeltaUV worldSize ex ey sx sy
                (pDx, pDy) = wrappedDeltaUV worldSize gx gy sx sy
                len2 = fromIntegral (eDx*eDx + eDy*eDy) ∷ Float
            in if len2 < 1.0e-6
               then let d2 = fromIntegral (pDx*pDx + pDy*pDy) ∷ Float
                        hw = w * 0.5
                    in d2 ≤ hw * hw
               else
                   let tRaw = fromIntegral (pDx*eDx + pDy*eDy) / len2
                       tc   = max 0.0 (min 1.0 tRaw)
                       cxF  = fromIntegral pDx - tc * fromIntegral eDx
                       cyF  = fromIntegral pDy - tc * fromIntegral eDy
                       hw   = w * 0.5
                   in cxF*cxF + cyF*cyF ≤ hw * hw

    EllipsoidChamber x y zc rx ry rz →
        let (dx, dy) = wrappedDeltaUV worldSize gx gy x y
            dxN = fromIntegral dx / rx
            dyN = fromIntegral dy / ry
            dzN = fromIntegral (z - zc) / rz
        in dxN*dxN + dyN*dyN + dzN*dzN ≤ 1.0

    IrregularChamber x y zc r amp freq seedW →
        let (dx, dy) = wrappedDeltaUV worldSize gx gy x y
            dz = z - zc
            d2 = fromIntegral (dx*dx + dy*dy + dz*dz) ∷ Float
            theta = atan2 (fromIntegral dy) (fromIntegral dx) ∷ Float
            phi   = atan2 (fromIntegral dz)
                          (sqrt (fromIntegral (dx*dx + dy*dy)))
            perturb = directionalPerturb seedW theta phi freq
            effR = r + amp * perturb
        in d2 ≤ effR * effR

-- | Hash-derived directional perturbation in [-1, 1] for the
--   irregular chamber. Cheap, deterministic, smooth-ish — not Perlin,
--   but enough texture to make chamber outlines feel non-spherical.
directionalPerturb ∷ Word64 → Float → Float → Float → Float
directionalPerturb seedW theta phi freq =
    let i1 = floor (theta * freq * 32.0) ∷ Int
        i2 = floor (phi   * freq * 32.0) ∷ Int
        h  = hashGeo seedW i1 i2
        f  = hashToFloatGeo h   -- [0, 1]
    in 2.0 * f - 1.0

-- | Lowest @z@ a shape can possibly cover. Used by bbox computation
--   in 'World.Magma.Init'.
shapeZBottom ∷ LavaShape → Int
shapeZBottom shape = case shape of
    Cylindrical      _ _ zb _ _         → zb
    Conical          _ _ zb _ _ _       → zb
    Perturbed        _ _ zb _ _ _ _ _   → zb
    Slot             _ _ _ _ zb _ _     → zb
    EllipsoidChamber _ _ zc _ _ rz      → zc - ceiling rz
    IrregularChamber _ _ zc r amp _ _   → zc - ceiling (r + amp)

-- | Highest @z@ a shape can possibly cover.
shapeZTop ∷ LavaShape → Int
shapeZTop shape = case shape of
    Cylindrical      _ _ _ zt _         → zt
    Conical          _ _ _ zt _ _       → zt
    Perturbed        _ _ _ zt _ _ _ _   → zt
    Slot             _ _ _ _ _ zt _     → zt
    EllipsoidChamber _ _ zc _ _ rz      → zc + ceiling rz
    IrregularChamber _ _ zc r amp _ _   → zc + ceiling (r + amp)
