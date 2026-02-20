{-# LANGUAGE Strict, UnicodeSyntax #-}
module World.Hydrology.Glacier.Carving
    ( applyGlacierCarve
    , applyGlacierEvolution
    ) where

import UPrelude
import World.Base (GeoCoord(..))
import World.Geology.Hash (wrappedDeltaUV)
import World.Material (matSandstone, unMaterialId)
import World.Hydrology.Types
import World.Geology.Types

-----------------------------------------------------------
-- Glacier Carving (pure GeoModification) — unchanged from
-- previous version, included for completeness
-----------------------------------------------------------

-- | Apply a glacier's U-shaped valley carving to a single column.
applyGlacierCarve ∷ GlacierParams → Int → Int → Int → Int → GeoModification
applyGlacierCarve glacier worldSize gx gy _baseElev =
    let GeoCoord cx cy = glCenter glacier
        flowDir = glFlowDir glacier
        glacierLen = fromIntegral (glLength glacier) ∷ Float
        halfW = fromIntegral (glWidth glacier) ∷ Float
        carveD = fromIntegral (glCarveDepth glacier) ∷ Float
        moraineH = fromIntegral (glMoraineSize glacier) ∷ Float

        (dxi, dyi) = wrappedDeltaUV worldSize gx gy cx cy
        dx = fromIntegral dxi ∷ Float
        dy = fromIntegral dyi ∷ Float

        flowX = cos flowDir
        flowY = sin flowDir
        alongDist = dx * flowX + dy * flowY
        perpDist = abs (dx * (-flowY) + dy * flowX)

        alongT = alongDist / glacierLen

    in if alongT < -0.05 ∨ alongT > 1.15 ∨ perpDist > halfW * 1.3
       then noModification

       -- Terminal moraine
       else if alongT > 0.90 ∧ alongT ≤ 1.15 ∧ perpDist < halfW * 1.2
       then let moraineT = (alongT - 0.90) / 0.25
                ridgeProfile = sin (moraineT * π)
                perpFade = max 0.0 (1.0 - (perpDist / (halfW * 1.2)) ** 2.0)
                deposit = round (moraineH * ridgeProfile * perpFade)
            in if deposit ≤ 0
               then noModification
               else GeoModification deposit (Just (unMaterialId matSandstone)) deposit

       -- Valley carving zone
       else if alongT ≥ -0.05 ∧ alongT ≤ 0.95 ∧ perpDist < halfW
       then let endTaper = min 1.0 (min (alongT * 5.0 + 0.25)
                                       ((0.95 - alongT) * 4.0))
                perpNorm = perpDist / halfW
                flatBottomRatio = 0.6
                crossProfile =
                    if perpNorm < flatBottomRatio
                    then 1.0
                    else let wallT = (perpNorm - flatBottomRatio) / (1.0 - flatBottomRatio)
                         in max 0.0 (1.0 - wallT * wallT)

                sheetBoost = if glIsIceSheet glacier then 1.3 else 1.0

                carve = round (carveD * crossProfile * endTaper * sheetBoost)

            in if carve ≤ 0
               then noModification
               else GeoModification (negate carve) (Just (unMaterialId matSandstone)) 0

       else noModification

-----------------------------------------------------------
-- Glacier Evolution Application (pure GeoModification)
-----------------------------------------------------------

applyGlacierEvolution ∷ HydroEvolution → Int → Int → Int → Int → GeoModification
applyGlacierEvolution (GlacierAdvance _advLen _advWid) _ws _gx _gy _e =
    noModification
applyGlacierEvolution (GlacierRetreat _retreatLen _moraineDep) _ws _gx _gy _e =
    noModification
applyGlacierEvolution (GlacierMelt _moraineDep) _ws _gx _gy _e =
    noModification
applyGlacierEvolution (GlacierBranch _branchPt _angle _len _childId) _ws _gx _gy _e =
    noModification
applyGlacierEvolution _ _ws _gx _gy _e = noModification
