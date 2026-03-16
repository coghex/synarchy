{-# LANGUAGE Strict, UnicodeSyntax #-}
module World.Hydrology.Event
    ( applyHydroFeature
    , applyHydroEvolution
    ) where

import UPrelude
import Data.Word (Word64)
import World.Base (GeoCoord(..))
import World.Types
import World.Hydrology.Types
import World.Hydrology.River (applyRiverCarve, applyRiverEvolution)
import World.Hydrology.Glacier (applyGlacierCarve, applyGlacierEvolution)
import World.Geology.Types (GeoModification(..), noModification)
import World.Geology.Hash (wrappedDeltaUV, hashGeo, hashToFloatGeo, smoothstepGeo)

-- * HydroEvent dispatch (for new feature placement)

-- | Apply a hydrological feature's terrain modification to
--   a single column. Called from applyGeoEvent when it hits
--   a HydroEvent.
--
--   RiverFeature  → V-shaped valley carve
--   GlacierFeature → U-shaped valley carve + moraine
--   LakeFeature   → gentle bowl carve (smooths shoreline)
applyHydroFeature ∷ HydroFeature → Int → Int → Int → Int → GeoModification
applyHydroFeature (RiverFeature river) worldSize gx gy baseElev =
    applyRiverCarve river worldSize gx gy baseElev
applyHydroFeature (GlacierFeature glacier) worldSize gx gy baseElev =
    applyGlacierCarve glacier worldSize gx gy baseElev
applyHydroFeature (LakeFeature lk) worldSize gx gy baseElev =
    -- Carve a gentle bowl so fine-scale terrain bumps don't poke
    -- above the water surface (which creates jagged "teeth" shorelines).
    -- The bowl floor rises from full depth at center to surface at rim.
    -- Uses angular + radial noise to break up the circular boundary.
    let GeoCoord fx fy = lkCenter lk
        (dxi, dyi) = wrappedDeltaUV worldSize gx gy fx fy
        dx = fromIntegral dxi ∷ Float
        dy = fromIntegral dyi ∷ Float
        rawDist = sqrt (dx * dx + dy * dy)
        radius = fromIntegral (lkRadius lk) ∷ Float
        surface = lkSurface lk
        depth = max 2 (lkDepth lk)

        -- Angular noise to break circular boundary (like volcano perturbation)
        angle = atan2 dy dx + π
        lakeSeed = fromIntegral (fx * 5381 + fy * 3571) ∷ Word64
        n1 = lakeAngularNoise lakeSeed 5  angle 19
        n2 = lakeAngularNoise lakeSeed 11 angle 37
        n3 = lakeAngularNoise lakeSeed 19 angle 53
        combined = n1 * 0.50 + n2 * 0.30 + n3 * 0.20
        perturbation = (combined - 0.5) * 0.4 * radius
        dist = rawDist + perturbation

    in if dist ≥ radius
       then noModification
       else let t = dist / radius
                -- Smoothstep for softer edge transition
                profile = 1.0 - smoothstepGeo (max 0.0 t)
                floorElev = surface - max 1 (round (fromIntegral depth * profile))
                carve = baseElev - floorElev
            in if carve ≤ 0
               then noModification
               else GeoModification
                   { gmElevDelta        = negate carve
                   , gmMaterialOverride = Nothing
                   , gmIntrusionDepth   = 0
                   }

-- | Angular noise for lake boundary perturbation.
--   Same technique as volcano perturbation with smoothstep interpolation.
lakeAngularNoise ∷ Word64 → Int → Float → Int → Float
lakeAngularNoise seed buckets angle hashProp =
    let angStep = 2.0 * π / fromIntegral buckets
        angF = angle / angStep
        bucket0 = floor angF ∷ Int
        bucket1 = bucket0 + 1
        t = angF - fromIntegral bucket0
        n0 = hashToFloatGeo (hashGeo seed bucket0 hashProp)
        n1 = hashToFloatGeo (hashGeo seed bucket1 hashProp)
    in n0 + smoothstepGeo t * (n1 - n0)

-- * HydroModify dispatch (for feature evolution)

-- | Apply a hydrological evolution event to a single column.
--   Called from applyGeoEvent when it hits a HydroModify.
--
--   Most evolution events modify the PersistentFeature's shape
--   (updated in TimelineBuildState during timeline build) and
--   the terrain effect comes from the *next* age's HydroEvent
--   using the updated params. Only a few produce immediate
--   per-tile terrain changes.
--
--   River events with terrain effect:
--     RiverDam → small debris ridge at the dam point
--
--   Glacier events with terrain effect:
--     (none — all glacier terrain comes from the feature's
--      GlacierParams being replayed via HydroEvent. The
--      evolution updates the params, and the next age's
--      GlacierFeature applies the updated carve.)
--
--   This mirrors how applyEvolution works for volcanoes:
--     GoDormant → noModification
--     GoExtinct → noModification
--     Reactivate → terrain change (growth)
--     CollapseToCaldera → terrain change (depression)
--
applyHydroEvolution ∷ HydroEvolution → Int → Int → Int → Int → GeoModification

-- ===== River evolution =====
applyHydroEvolution (RiverBranch branchPt angle len childId) ws gx gy e =
    applyRiverEvolution (RiverBranch branchPt angle len childId) ws gx gy e

applyHydroEvolution (RiverMeander seed' amt) ws gx gy e =
    applyRiverEvolution (RiverMeander seed' amt) ws gx gy e

applyHydroEvolution (RiverCapture capturedId capturePoint) ws gx gy e =
    applyRiverEvolution (RiverCapture capturedId capturePoint) ws gx gy e

applyHydroEvolution (RiverDam damPt lakeId damH) ws gx gy e =
    applyRiverEvolution (RiverDam damPt lakeId damH) ws gx gy e

applyHydroEvolution RiverDryUp ws gx gy e =
    applyRiverEvolution RiverDryUp ws gx gy e

-- ===== Glacier evolution =====
applyHydroEvolution (GlacierAdvance advLen advWid) ws gx gy e =
    applyGlacierEvolution (GlacierAdvance advLen advWid) ws gx gy e

applyHydroEvolution (GlacierRetreat retreatLen moraineDep) ws gx gy e =
    applyGlacierEvolution (GlacierRetreat retreatLen moraineDep) ws gx gy e

applyHydroEvolution (GlacierMelt moraineDep) ws gx gy e =
    applyGlacierEvolution (GlacierMelt moraineDep) ws gx gy e

applyHydroEvolution (GlacierBranch branchPt angle len childId) ws gx gy e =
    applyGlacierEvolution (GlacierBranch branchPt angle len childId) ws gx gy e

-- ===== Lake evolution =====
applyHydroEvolution (LakeDrain _) _ws _gx _gy _e =
    noModification  -- lake draining doesn't carve terrain

applyHydroEvolution (LakeExpand _ _) _ws _gx _gy _e =
    noModification  -- lake expansion is a fluid change, not terrain
