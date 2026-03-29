{-# LANGUAGE Strict, UnicodeSyntax #-}
module World.Hydrology.Glacier.Carving
    ( applyGlacierCarve
    , applyGlacierEvolution
    ) where

import UPrelude
import Data.Word (Word64)
import World.Base (GeoCoord(..))
import World.Geology.Hash (wrappedDeltaUV, valueNoise2D)
import World.Material (matSandstone, unMaterialId)
import World.Hydrology.Types
import World.Geology.Types

-- * Glacier Carving

-- | Apply a glacier's U-shaped valley carving to a single column.
--   Uses multi-scale noise to perturb the glacier width along its
--   length, breaking the rectangular footprint into a natural shape.
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

        -- Width noise: perturb the effective half-width using spatially
        -- coherent noise keyed to position along the glacier. Two octaves
        -- create natural variation — wide spots and narrow pinches.
        glacierSeed = fromIntegral (cx * 7919 + cy * 6271) ∷ Word64
        -- Sample noise along the glacier's flow axis
        noiseX = fromIntegral cx + alongDist * flowX
        noiseY = fromIntegral cy + alongDist * flowY
        widthNoise = valueNoise2D glacierSeed 201 noiseX noiseY (glacierLen / 3.0)
                   + valueNoise2D glacierSeed 202 noiseX noiseY (glacierLen / 7.0) * 0.4
        -- ±25% width variation
        noisyHalfW = halfW * (1.0 + widthNoise * 0.5)

        -- Soft outer boundary: taper zone beyond the noisy edge
        taperZone = max 2.0 (halfW * 0.25)
        outerLimit = noisyHalfW + taperZone

    in if alongT < -0.05 ∨ alongT > 1.15 ∨ perpDist > outerLimit
       then noModification

       -- Terminal moraine
       else if alongT > 0.90 ∧ alongT ≤ 1.15 ∧ perpDist < noisyHalfW * 1.2
       then let moraineT = (alongT - 0.90) / 0.25
                ridgeProfile = sin (moraineT * π)
                perpFade = max 0.0 (1.0 - (perpDist / (noisyHalfW * 1.2)) ** 2.0)
                deposit = round (moraineH * ridgeProfile * perpFade)
            in if deposit ≤ 0
               then noModification
               else GeoModification deposit (Just (unMaterialId matSandstone)) deposit

       -- Valley carving zone
       else if alongT ≥ -0.05 ∧ alongT ≤ 0.95
       then let endTaper = min 1.0 (min (alongT * 5.0 + 0.25)
                                       ((0.95 - alongT) * 4.0))
                perpNorm = perpDist / noisyHalfW
                -- Soft edge: beyond noisyHalfW, fade smoothly to zero
                edgeFade = if perpDist < noisyHalfW
                           then 1.0
                           else max 0.0 (1.0 - (perpDist - noisyHalfW) / taperZone)
                flatBottomRatio = 0.6
                crossProfile =
                    if perpNorm < flatBottomRatio
                    then 1.0
                    else let wallT = min 1.0 ((perpNorm - flatBottomRatio) / (1.0 - flatBottomRatio))
                         in max 0.0 (1.0 - wallT * wallT)

                sheetBoost = if glIsIceSheet glacier then 1.3 else 1.0

                carve = round (carveD * crossProfile * endTaper * edgeFade * sheetBoost)

            in if carve ≤ 0
               then noModification
               else GeoModification (negate carve) (Just (unMaterialId matSandstone)) 0

       else noModification

-- * Glacier Evolution Application (pure GeoModification)
--
-- NOTE: Advance and Branch need no terrain effect here — active glaciers
-- are re-carved each age with updated dimensions (see activeGlacierRecarve
-- in Timeline.hs), so dimension changes propagate automatically.
--
-- TODO: Retreat and Melt should deposit moraine at the former terminus.
-- The moraineDep parameter is generated in Evolution.hs but currently
-- discarded. Implementing this requires adding glacier geometry (center,
-- flowDir, width, oldLength) to the GlacierRetreat/GlacierMelt variants
-- of HydroEvolution so the spatial deposit can be computed here.

applyGlacierEvolution ∷ HydroEvolution → Int → Int → Int → Int → GeoModification
applyGlacierEvolution (GlacierAdvance _advLen _advWid) _ws _gx _gy _e =
    noModification
applyGlacierEvolution (GlacierRetreat _retreatLen _moraineDep) _ws _gx _gy _e =
    noModification  -- TODO: deposit moraine in retreat zone
applyGlacierEvolution (GlacierMelt _moraineDep) _ws _gx _gy _e =
    noModification  -- TODO: deposit final moraine at terminus
applyGlacierEvolution (GlacierBranch _branchPt _angle _len _childId) _ws _gx _gy _e =
    noModification
applyGlacierEvolution _ _ws _gx _gy _e = noModification
