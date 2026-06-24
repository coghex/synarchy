{-# LANGUAGE Strict, UnicodeSyntax #-}
module World.Hydrology.Glacier.Carving
    ( applyGlacierCarve
    , applyGlacierMoraine
    , applyGlacierEvolution
    ) where

import UPrelude
import Data.Word (Word64)
import World.Base (GeoCoord(..))
import World.Geology.Hash (wrappedDeltaUV, valueNoise2D)
import World.Material (matMoraine, matSandstone, unMaterialId)
import World.Constants (seaLevel)
import World.Hydrology.Types
import World.Geology.Types

-- * Glacier Carving

-- | Apply a glacier's U-shaped valley carving to a single column.
--
--   TARGET-BASED: the floor elevation and moraine ridge height are
--   computed from stable per-glacier reference elevations
--   (glStartElev at the head, glFootElev at the foot) sampled at
--   generation time. Re-applying the same event in later Ages
--   produces no further delta — terrain already at target is left
--   alone. This mirrors the river carving idempotency pattern
--   and fixes the unbounded compounding flagged by audit #6.
--
--   Width noise still perturbs the glacier edges and the cross-
--   sectional U-profile is unchanged; only the absolute reference
--   surface has been added.
applyGlacierCarve ∷ GlacierParams → Int → Int → Int → Int → GeoModification
applyGlacierCarve glacier worldSize gx gy baseElev =
    let GeoCoord cx cy = glCenter glacier
        flowDir = glFlowDir glacier
        glacierLen = fromIntegral (glLength glacier) ∷ Float
        halfW = fromIntegral (glWidth glacier) ∷ Float
        carveD = fromIntegral (glCarveDepth glacier) ∷ Float
        moraineH = fromIntegral (glMoraineSize glacier) ∷ Float
        startElev = fromIntegral (glStartElev glacier) ∷ Float
        footElev  = fromIntegral (glFootElev glacier) ∷ Float

        (dxi, dyi) = wrappedDeltaUV worldSize gx gy cx cy
        dx = fromIntegral dxi ∷ Float
        dy = fromIntegral dyi ∷ Float

        flowX = cos flowDir
        flowY = sin flowDir
        alongDist = dx * flowX + dy * flowY
        perpDist = abs (dx * (-flowY) + dy * flowX)

        alongT = alongDist / glacierLen

        -- Reference surface interpolated head→foot. At alongT=0 the
        -- glacier sits on terrain of elevation startElev; at alongT=1
        -- the foot is at footElev. Subtracting the depth profile from
        -- this gives an absolute target floor — re-applications hit
        -- the same target and become no-ops.
        tClamped = max 0.0 (min 1.0 alongT)
        refSurface = startElev + tClamped * (footElev - startElev)

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

       -- Terminal moraine: deposit a ridge above the reference
       -- surface near the foot of the glacier. Target-based using
       -- footElev so re-deposit is idempotent.
       else if alongT > 0.90 ∧ alongT ≤ 1.15 ∧ perpDist < noisyHalfW * 1.2
       then let moraineT = (alongT - 0.90) / 0.25
                ridgeProfile = sin (moraineT * π)
                perpFade = max 0.0 (1.0 - (perpDist / (noisyHalfW * 1.2)) ** 2.0)
                ridgeAmount = moraineH * ridgeProfile * perpFade
                moraineTarget = floor (footElev + ridgeAmount) ∷ Int
                deposit = moraineTarget - baseElev
            in if deposit ≤ 0
               then noModification
               else GeoModification deposit (Just (unMaterialId matSandstone)) deposit

       -- Valley carving zone: lower terrain toward an absolute
       -- target floor derived from refSurface − depth profile.
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

                depthHere = carveD * crossProfile * endTaper * edgeFade * sheetBoost
                rawTarget = floor (refSurface - depthHere) ∷ Int
                -- Clamp to seaLevel - 1 so glaciers can't carve trenches
                -- below the ocean surface (same convention as rivers).
                targetFloor = max (seaLevel - 1) rawTarget
                carve = baseElev - targetFloor

            in if carve ≤ 0
               then noModification
               else GeoModification (negate carve) (Just (unMaterialId matSandstone)) 0

       else noModification

-- | Deposit a retreat/melt moraine at the glacier's former terminus.
--   This is target-based, like glacier carving: replaying the same event
--   after its ridge already exists yields no further elevation change.
applyGlacierMoraine ∷ GlacierMoraineParams → Int → Int → Int → Int → GeoModification
applyGlacierMoraine mp worldSize gx gy baseElev =
    let GeoCoord cx cy = gmpCenter mp
        flowDir = gmpFlowDir mp
        flowX = cos flowDir
        flowY = sin flowDir
        (dxi, dyi) = wrappedDeltaUV worldSize gx gy cx cy
        dx = fromIntegral dxi ∷ Float
        dy = fromIntegral dyi ∷ Float
        alongDist = dx * flowX + dy * flowY
        perpDist = abs (dx * (-flowY) + dy * flowX)
        formerTerminus = fromIntegral (gmpLength mp) ∷ Float
        alongOffset = alongDist - formerTerminus
        halfBand = max 2.0 (fromIntegral (gmpRidgeHalfLength mp))
        halfWidth = max 2.0 (fromIntegral (gmpWidth mp) * 0.75)
        alongT = abs alongOffset / halfBand
        perpT = perpDist / (halfWidth * 1.25)
    in if alongT > 1.0 ∨ perpT > 1.0
       then noModification
       else
        let alongProfile = 1.0 - alongT * alongT
            perpProfile  = 1.0 - perpT * perpT
            ridgeHeight = fromIntegral (gmpDepositHeight mp)
                        * alongProfile * perpProfile
            target = gmpFootElev mp + max 1 (round ridgeHeight)
            deposit = target - baseElev
        in if deposit ≤ 0
           then noModification
           else GeoModification deposit
                (Just (unMaterialId matMoraine))
                deposit

-- * Glacier Evolution Application (pure GeoModification)
--
-- NOTE: Advance and Branch need no terrain effect here — active glaciers
-- are re-carved each age with updated dimensions (see activeGlacierRecarve
-- in Timeline.hs), so dimension changes propagate automatically.
--
-- Retreat/melt terrain is emitted as 'GlacierMoraineEvent', which carries
-- the pre-change glacier geometry needed to place a former-terminus ridge.

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
