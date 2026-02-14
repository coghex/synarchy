{-# LANGUAGE Strict, UnicodeSyntax #-}
module World.Geology.Evolution
    ( evolveOneFeature
    , evolvePointFeature
    , getFeatureCenter
    , getFeatureRadius
    ) where

import UPrelude
import Data.Word (Word64)
import World.Types
import World.Geology.Types
import World.Geology.Hash

-----------------------------------------------------------
-- Volcanic Evolution
-----------------------------------------------------------

evolveOneFeature ∷ Word64 → Int
                 → ([GeoEvent], TimelineBuildState)
                 → PersistentFeature
                 → ([GeoEvent], TimelineBuildState)
evolveOneFeature seed periodIdx (events, tbs) pf =
    case pfFeature pf of
        FissureVolcano _   → (events, tbs)
        LavaTube _         → (events, tbs)
        HydrothermalVent _ → (events, tbs)
        SuperVolcano _     → (events, tbs)
        Caldera _          → (events, tbs)
        _ → evolvePointFeature seed periodIdx (events, tbs) pf

evolvePointFeature ∷ Word64 → Int
                   → ([GeoEvent], TimelineBuildState)
                   → PersistentFeature
                   → ([GeoEvent], TimelineBuildState)
evolvePointFeature seed periodIdx (events, tbs) pf =
    let fid = pfId pf
        GeoFeatureId fidInt = fid
        h1 = hashGeo seed fidInt 40
        roll = hashToFloatGeo h1
    in case pfActivity pf of
        Active →
            if roll < 0.10
            -- 10%: collapse into caldera
            then let h2 = hashGeo seed fidInt 41
                     h3 = hashGeo seed fidInt 42
                     depth = hashToRangeGeo h2 50 200
                     ratio = 0.3 + hashToFloatGeo h3 * 0.5
                     evt = VolcanicModify fid (CollapseToCaldera depth ratio
                             (getFeatureCenter (pfFeature pf))
                             (getFeatureRadius (pfFeature pf)))
                     tbs' = updateFeature fid
                         (\p → p { pfActivity = Collapsed
                                  , pfLastActivePeriod = periodIdx }) tbs
                 in (evt : events, tbs')

            else if roll < 0.15
            -- 5%: flank collapse (Mt. St. Helens style)
            then let h2 = hashGeo seed fidInt 52
                     h3 = hashGeo seed fidInt 53
                     h4 = hashGeo seed fidInt 54
                     collapseAngle = hashToFloatGeo h2 * 2.0 * π
                     collapseWidth = 0.6 + hashToFloatGeo h3 * 0.8
                         -- 0.6 to 1.4 radians (34° to 80°)
                     featureR = getFeatureRadius (pfFeature pf)
                     debrisR = featureR + hashToRangeGeo h4
                                 (featureR `div` 2) (featureR * 2)
                     evt = VolcanicModify fid (FlankCollapse
                             collapseAngle collapseWidth debrisR
                             (getFeatureCenter (pfFeature pf))
                             (getFeatureRadius (pfFeature pf)))
                     -- Flank collapse doesn't kill the volcano — it can
                     -- keep erupting (like Mt. St. Helens did). But mark
                     -- that it erupted.
                     tbs' = updateFeature fid
                         (\p → p { pfEruptionCount = pfEruptionCount p + 1
                                  , pfLastActivePeriod = periodIdx }) tbs
                 in (evt : events, tbs')

            else if roll < 0.40
            -- 25%: go dormant
            then let evt = VolcanicModify fid (GoDormant
                               (getFeatureCenter (pfFeature pf))
                               (getFeatureRadius (pfFeature pf)))
                     tbs' = updateFeature fid
                         (\p → p { pfActivity = Dormant
                                  , pfLastActivePeriod = periodIdx }) tbs
                 in (evt : events, tbs')

            else if roll < 0.60
            -- 20%: parasitic eruption
            then let (childId, tbs') = allocFeatureId tbs
                     h3 = hashGeo seed fidInt 43
                     h4 = hashGeo seed fidInt 44
                     parentCenter = getFeatureCenter (pfFeature pf)
                     GeoCoord px py = parentCenter
                     angle = hashToFloatGeo h3 * 2.0 * 3.14159
                     parentRadius = getFeatureRadius (pfFeature pf)
                     dist = fromIntegral parentRadius * (0.5 + hashToFloatGeo h4 * 0.4)
                     childX = px + round (dist * cos angle)
                     childY = py + round (dist * sin angle)
                     childFeature = CinderCone CinderConeParams
                         { ccCenter       = GeoCoord childX childY
                         , ccBaseRadius   = hashToRangeGeo (hashGeo seed fidInt 45) 5 12
                         , ccPeakHeight   = hashToRangeGeo (hashGeo seed fidInt 46) 50 150
                         , ccCraterRadius = hashToRangeGeo (hashGeo seed fidInt 47) 2 5
                         , ccCraterDepth  = hashToRangeGeo (hashGeo seed fidInt 48) 10 40
                         }
                     childPf = PersistentFeature
                         { pfId               = childId
                         , pfFeature          = childFeature
                         , pfActivity         = Active
                         , pfFormationPeriod   = periodIdx
                         , pfLastActivePeriod  = periodIdx
                         , pfEruptionCount     = 1
                         , pfParentId          = Just fid
                         }
                     evt = VolcanicModify fid (ParasiticEruption childFeature childId
                             (getFeatureCenter (pfFeature pf))
                             (getFeatureRadius (pfFeature pf)))
                     tbs'' = registerFeature childPf tbs'
                     tbs''' = updateFeature fid
                         (\p → p { pfEruptionCount = pfEruptionCount p + 1 }) tbs''
                 in (evt : events, tbs''')

            else
            -- 40%: stays active, grows
            let h5 = hashGeo seed fidInt 49
                heightGain = hashToRangeGeo h5 20 100
                evt = VolcanicModify fid (Reactivate heightGain 0
                        (getFeatureCenter (pfFeature pf))
                        (getFeatureRadius (pfFeature pf)))
                tbs' = updateFeature fid
                    (\p → p { pfEruptionCount = pfEruptionCount p + 1
                             , pfLastActivePeriod = periodIdx }) tbs
            in (evt : events, tbs')

        Dormant →
            if roll < 0.3
            then let h5 = hashGeo seed fidInt 50
                     h6 = hashGeo seed fidInt 51
                     heightGain = hashToRangeGeo h5 30 150
                     lavaExt = hashToRangeGeo h6 5 20
                     evt = VolcanicModify fid (Reactivate heightGain lavaExt
                                (getFeatureCenter (pfFeature pf))
                                (getFeatureRadius (pfFeature pf)))
                     tbs' = updateFeature fid
                         (\p → p { pfActivity = Active
                                  , pfEruptionCount = pfEruptionCount p + 1
                                  , pfLastActivePeriod = periodIdx }) tbs
                 in (evt : events, tbs')

            else if roll < 0.5
            then let evt = VolcanicModify fid (GoExtinct
                               (getFeatureCenter (pfFeature pf))
                               (getFeatureRadius (pfFeature pf)))
                     tbs' = updateFeature fid
                         (\p → p { pfActivity = Extinct }) tbs
                 in (evt : events, tbs')

            else (events, tbs)

        Extinct   → (events, tbs)
        Collapsed → (events, tbs)

-----------------------------------------------------------
-- Feature Query Helpers
-----------------------------------------------------------

getFeatureRadius ∷ VolcanicFeature → Int
getFeatureRadius (ShieldVolcano p)    = shBaseRadius p
getFeatureRadius (CinderCone p)       = ccBaseRadius p
getFeatureRadius (LavaDome p)         = ldBaseRadius p
getFeatureRadius (Caldera p)          = caOuterRadius p
getFeatureRadius (SuperVolcano p)     = svEjectaRadius p
getFeatureRadius (HydrothermalVent p) = htRadius p
getFeatureRadius (FissureVolcano p)   =
    let GeoCoord sx sy = fpStart p
        GeoCoord ex ey = fpEnd p
        dx = fromIntegral (ex - sx) ∷ Float
        dy = fromIntegral (ey - sy) ∷ Float
    in round (sqrt (dx * dx + dy * dy) / 2.0) + fpWidth p
getFeatureRadius (LavaTube p)         =
    let GeoCoord sx sy = ltStart p
        GeoCoord ex ey = ltEnd p
        dx = fromIntegral (ex - sx) ∷ Float
        dy = fromIntegral (ey - sy) ∷ Float
    in round (sqrt (dx * dx + dy * dy) / 2.0) + ltWidth p

getFeatureCenter ∷ VolcanicFeature → GeoCoord
getFeatureCenter (ShieldVolcano p)    = shCenter p
getFeatureCenter (CinderCone p)       = ccCenter p
getFeatureCenter (LavaDome p)         = ldCenter p
getFeatureCenter (Caldera p)          = caCenter p
getFeatureCenter (SuperVolcano p)     = svCenter p
getFeatureCenter (HydrothermalVent p) = htCenter p
getFeatureCenter (FissureVolcano p)   =
    let GeoCoord sx sy = fpStart p
        GeoCoord ex ey = fpEnd p
    in GeoCoord ((sx + ex) `div` 2) ((sy + ey) `div` 2)
getFeatureCenter (LavaTube p)         =
    let GeoCoord sx sy = ltStart p
        GeoCoord ex ey = ltEnd p
    in GeoCoord ((sx + ex) `div` 2) ((sy + ey) `div` 2)
