{-# LANGUAGE Strict #-}
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

-----------------------------------------------------------
-- Event Application
-----------------------------------------------------------

-- | Apply any geo event to a position.
applyGeoEvent :: GeoEvent -> Int -> Int -> Int -> Int -> GeoModification
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

applyEvolution :: FeatureEvolution -> Int -> Int -> Int -> Int -> GeoModification
applyEvolution (Reactivate heightGain _lavaExt) _ws _gx _gy _e =
    GeoModification heightGain Nothing
applyEvolution GoDormant _ _ _ _ = noModification
applyEvolution GoExtinct _ _ _ _ = noModification
applyEvolution (CollapseToCaldera depth _ratio) _ws _gx _gy _e =
    GeoModification (negate depth) (Just (unMaterialId matObsidian))
applyEvolution (ParasiticEruption childFeature _childId) ws gx gy e =
    applyVolcanicFeature childFeature ws gx gy e
applyEvolution (FlankCollapse _ _ _) _ _ _ _ = noModification
