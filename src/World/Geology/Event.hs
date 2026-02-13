{-# LANGUAGE Strict, UnicodeSyntax #-}
module World.Geology.Event
    ( applyGeoEvent
    , applyEvolution
    ) where

import UPrelude
import World.Types
import World.Material
import World.Geology.Types
import World.Geology.Crater (applyCrater)
import World.Geology.Volcano (applyVolcanicFeature)
import World.Geology.Hash (wrappedDeltaXGeo, smoothstepGeo)

-----------------------------------------------------------
-- Event Application
-----------------------------------------------------------

applyGeoEvent ∷ GeoEvent → Int → Int → Int → Int → GeoModification
applyGeoEvent (CraterEvent params)  worldSize gx gy baseElev =
    applyCrater params worldSize gx gy baseElev
applyGeoEvent (VolcanicEvent feature) worldSize gx gy baseElev =
    applyVolcanicFeature feature worldSize gx gy baseElev
applyGeoEvent (VolcanicModify _fid evolution) worldSize gx gy baseElev =
    applyEvolution evolution worldSize gx gy baseElev
applyGeoEvent (LandslideEvent _)    _ _ _ _ = noModification
applyGeoEvent (GlaciationEvent _)   _ _ _ _ = noModification
applyGeoEvent (FloodEvent _)        _ _ _ _ = noModification

-----------------------------------------------------------
-- Feature Evolution Application
-----------------------------------------------------------

applyEvolution ∷ FeatureEvolution → Int → Int → Int → Int → GeoModification
applyEvolution (Reactivate heightGain _lavaExt center radius) ws gx gy _e =
    let GeoCoord cx cy = center
        dx = fromIntegral (wrappedDeltaXGeo ws gx cx) ∷ Float
        dy = fromIntegral (gy - cy) ∷ Float
        dist = sqrt (dx * dx + dy * dy)
        r = fromIntegral radius ∷ Float
    in if dist > r
       then noModification
       else let t = dist / r
                profile = 1.0 - smoothstepGeo t
                elevDelta = round (fromIntegral heightGain * profile)
                -- Reactivation: new lava deposition on top,
                -- inner 40% is new material, rest is uplift
                intrusion = if t < 0.4
                    then abs elevDelta
                    else 0
            in GeoModification elevDelta Nothing intrusion

applyEvolution (GoDormant _center _radius) _ _ _ _ = noModification
applyEvolution (GoExtinct _center _radius) _ _ _ _ = noModification

applyEvolution (CollapseToCaldera depth _ratio center radius) ws gx gy _e =
    let GeoCoord cx cy = center
        dx = fromIntegral (wrappedDeltaXGeo ws gx cx) ∷ Float
        dy = fromIntegral (gy - cy) ∷ Float
        dist = sqrt (dx * dx + dy * dy)
        r = fromIntegral radius ∷ Float
    in if dist > r
       then noModification
       else let t = dist / r
                rimZone = t > 0.8
                bowlT = smoothstepGeo (t / 0.8)
                elevDelta = if rimZone
                    then 0
                    else round (negate (fromIntegral depth) * (1.0 - bowlT))
                mat = if rimZone
                    then Nothing
                    else Just (unMaterialId matObsidian)
                -- Collapse is depression/rearrangement, not deposition
            in GeoModification elevDelta mat 0

applyEvolution (ParasiticEruption childFeature _childId _center _radius) ws gx gy e =
    applyVolcanicFeature childFeature ws gx gy e

applyEvolution (FlankCollapse _ _ _ _center _radius) _ _ _ _ = noModification
