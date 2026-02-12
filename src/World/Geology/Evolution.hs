{-# LANGUAGE Strict #-}
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

-- | Evolve a single persistent feature between geological periods.
--   Decides whether it stays active, goes dormant, collapses,
--   spawns parasitic cones, etc.
evolveOneFeature :: Word64 -> Int
                 -> ([GeoEvent], TimelineBuildState)
                 -> PersistentFeature
                 -> ([GeoEvent], TimelineBuildState)
evolveOneFeature seed periodIdx (events, tbs) pf =
    -- Only evolve point features (shields, cinder cones, lava domes)
    -- Fissures, tubes, vents, and supervolcanoes don't evolve this way
    case pfFeature pf of
        FissureVolcano _   -> (events, tbs)
        LavaTube _         -> (events, tbs)
        HydrothermalVent _ -> (events, tbs)
        SuperVolcano _     -> (events, tbs)
        Caldera _          -> (events, tbs)
        _ -> evolvePointFeature seed periodIdx (events, tbs) pf

-- | Evolution logic for point volcanic features only.
evolvePointFeature :: Word64 -> Int
                   -> ([GeoEvent], TimelineBuildState)
                   -> PersistentFeature
                   -> ([GeoEvent], TimelineBuildState)
evolvePointFeature seed periodIdx (events, tbs) pf =
    let fid = pfId pf
        GeoFeatureId fidInt = fid
        h1 = hashGeo seed fidInt 40
        roll = hashToFloatGeo h1
    in case pfActivity pf of
        Active ->
            if roll < 0.15
            -- 15%: collapse into caldera
            then let h2 = hashGeo seed fidInt 41
                     h3 = hashGeo seed fidInt 42
                     depth = hashToRangeGeo h2 50 200
                     ratio = 0.3 + hashToFloatGeo h3 * 0.5
                     evt = VolcanicModify fid (CollapseToCaldera depth ratio)
                     tbs' = updateFeature fid
                         (\p -> p { pfActivity = Collapsed
                                  , pfLastActivePeriod = periodIdx }) tbs
                 in (evt : events, tbs')

            else if roll < 0.45
            -- 30%: go dormant
            then let evt = VolcanicModify fid GoDormant
                     tbs' = updateFeature fid
                         (\p -> p { pfActivity = Dormant
                                  , pfLastActivePeriod = periodIdx }) tbs
                 in (evt : events, tbs')

            else if roll < 0.65
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
                     evt = VolcanicModify fid (ParasiticEruption childFeature childId)
                     tbs'' = registerFeature childPf tbs'
                     tbs''' = updateFeature fid
                         (\p -> p { pfEruptionCount = pfEruptionCount p + 1 }) tbs''
                 in (evt : events, tbs''')

            else
            -- 35%: stays active, grows
            let h5 = hashGeo seed fidInt 49
                heightGain = hashToRangeGeo h5 20 100
                evt = VolcanicModify fid (Reactivate heightGain 0)
                tbs' = updateFeature fid
                    (\p -> p { pfEruptionCount = pfEruptionCount p + 1
                             , pfLastActivePeriod = periodIdx }) tbs
            in (evt : events, tbs')

        Dormant ->
            if roll < 0.3
            then let h5 = hashGeo seed fidInt 50
                     h6 = hashGeo seed fidInt 51
                     heightGain = hashToRangeGeo h5 30 150
                     lavaExt = hashToRangeGeo h6 5 20
                     evt = VolcanicModify fid (Reactivate heightGain lavaExt)
                     tbs' = updateFeature fid
                         (\p -> p { pfActivity = Active
                                  , pfEruptionCount = pfEruptionCount p + 1
                                  , pfLastActivePeriod = periodIdx }) tbs
                 in (evt : events, tbs')

            else if roll < 0.5
            then let evt = VolcanicModify fid GoExtinct
                     tbs' = updateFeature fid
                         (\p -> p { pfActivity = Extinct }) tbs
                 in (evt : events, tbs')

            else (events, tbs)

        Extinct   -> (events, tbs)
        Collapsed -> (events, tbs)

-----------------------------------------------------------
-- Feature Query Helpers
-----------------------------------------------------------

-- | Get center coordinates from any volcanic feature.
getFeatureCenter :: VolcanicFeature -> GeoCoord
getFeatureCenter (ShieldVolcano p)    = shCenter p
getFeatureCenter (CinderCone p)       = ccCenter p
getFeatureCenter (LavaDome p)         = ldCenter p
getFeatureCenter (Caldera p)          = caCenter p
getFeatureCenter (FissureVolcano p)   = fpStart p
getFeatureCenter (LavaTube p)         = ltStart p
getFeatureCenter (SuperVolcano p)     = svCenter p
getFeatureCenter (HydrothermalVent p) = htCenter p

-- | Get approximate radius from any volcanic feature.
getFeatureRadius :: VolcanicFeature -> Int
getFeatureRadius (ShieldVolcano p)    = shBaseRadius p
getFeatureRadius (CinderCone p)       = ccBaseRadius p
getFeatureRadius (LavaDome p)         = ldBaseRadius p
getFeatureRadius (Caldera p)          = caOuterRadius p
getFeatureRadius (FissureVolcano _)   = 50
getFeatureRadius (LavaTube _)         = 30
getFeatureRadius (SuperVolcano p)     = svCalderaRadius p
getFeatureRadius (HydrothermalVent p) = htRadius p
