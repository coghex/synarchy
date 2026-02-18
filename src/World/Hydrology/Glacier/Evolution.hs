{-# LANGUAGE Strict, UnicodeSyntax #-}
module World.Hydrology.Glacier.Evolution
    ( evolveGlacier
    ) where

import UPrelude
import Data.Word (Word64)
import World.Base (GeoCoord(..), GeoFeatureId(..))
import World.Types
import World.Hydrology.Types
import World.Geology.Types
import World.Geology.Hash
import World.Hydrology.Glacier.Common (getGlacierParams)
import World.Hydrology.Glacier.Spawn (spawnMeltwaterRiver, spawnMoraineLake)

-----------------------------------------------------------
-- Glacier Evolution
-----------------------------------------------------------

-- | Evolve a glacier feature for one age.
--   Follows the same fold pattern as evolvePointFeature:
--   called via foldl' over tbsFeatures, accumulating
--   ([GeoEvent], TimelineBuildState).
--
--   Glaciers are temperature-driven. Unlike volcanoes (which
--   have internal probabilistic state), glacier behavior is
--   largely determined by global temperature:
--
--     Cold world (gsCO2 < 0.8)  → glaciers advance aggressively
--     Moderate   (0.8 - 1.2)    → glaciers stable or slowly retreating
--     Warm world (gsCO2 > 1.2)  → glaciers retreat or melt entirely
--
--   The hash roll then determines *how* the glacier evolves
--   within that temperature-driven tendency.
--
--   Probability tables (temperature-dependent):
--
--   COLD (gsCO2 < 0.8):
--     50% → Advance: glacier extends, carves deeper
--     15% → Branch: glacier tongue splits around a ridge
--     10% → Surge: rapid advance, deep carve, wide moraine
--      5% → Spawn meltwater river at terminus
--     20% → Continue: stable, slight deepening
--
--   MODERATE (0.8 ≤ gsCO2 ≤ 1.2):
--     20% → Advance (slow)
--     20% → Retreat: glacier shortens, deposits terminal moraine
--     10% → Branch
--      5% → Spawn meltwater river
--     10% → Dam: terminal moraine blocks a valley, lake forms
--     35% → Continue stable
--
--   WARM (gsCO2 > 1.2):
--     10% → Advance (only alpine glaciers at very high elevation)
--     35% → Retreat
--     20% → Melt entirely: glacier gone, U-valley + moraine remain
--     10% → Dam: moraine creates lake as meltwater pools
--      5% → Spawn meltwater river
--     20% → Continue retreating slowly
--
evolveGlacier ∷ Word64 → Int → GeoState
              → ([GeoEvent], TimelineBuildState)
              → PersistentFeature
              → ([GeoEvent], TimelineBuildState)
evolveGlacier seed periodIdx gs (events, tbs) pf =
    let fid = pfId pf
        GeoFeatureId fidInt = fid
        h1 = hashGeo seed fidInt 900
        roll = hashToFloatGeo h1
        temp = gsCO2 gs  -- CO2 as temperature proxy
    in case pfActivity pf of

        ---------------------------------------------------
        -- Advancing glacier
        ---------------------------------------------------
        FActive → evolveFActiveGlacier seed periodIdx gs roll temp fid fidInt pf (events, tbs)

        ---------------------------------------------------
        -- Stable glacier (equivalent to Dormant volcano)
        ---------------------------------------------------
        FDormant → evolveFDormantGlacier seed periodIdx gs roll temp fid fidInt pf (events, tbs)

        ---------------------------------------------------
        -- Dead features don't evolve
        ---------------------------------------------------
        FExtinct   → (events, tbs)
        FCollapsed → (events, tbs)


-- | Active (advancing) glacier evolution
evolveFActiveGlacier ∷ Word64 → Int → GeoState → Float → Float
                     → GeoFeatureId → Int → PersistentFeature
                     → ([GeoEvent], TimelineBuildState)
                     → ([GeoEvent], TimelineBuildState)
evolveFActiveGlacier seed periodIdx gs roll temp fid fidInt pf (events, tbs)
    -- ===== COLD WORLD =====
    | temp < 0.8 =
        if roll < 0.50
        -- 50%: Advance — glacier extends further downhill, carves deeper
        then let h2 = hashGeo seed fidInt 901
                 h3 = hashGeo seed fidInt 902
                 advLen = hashToRangeGeo h2 10 40
                 advWid = hashToRangeGeo h3 2 8
                 evt = HydroModify fid (GlacierAdvance advLen advWid)
                 tbs' = updateFeature fid
                     (\p → p { pfEruptionCount = pfEruptionCount p + 1
                              , pfLastActivePeriod = periodIdx }) tbs
             in (evt : events, tbs')

        else if roll < 0.65
        -- 15%: Branch — glacier splits around a subglacial ridge
        -- Creates a new glacier feature on the flank, like parasitic eruption
        then let (childId, tbs') = allocFeatureId tbs
                 h2 = hashGeo seed fidInt 903
                 h3 = hashGeo seed fidInt 904
                 h4 = hashGeo seed fidInt 905
                 h5 = hashGeo seed fidInt 906
                 h6 = hashGeo seed fidInt 907
                 h7 = hashGeo seed fidInt 908
                 -- Branch angles off from current flow direction
                 parentGlacier = getGlacierParams pf
                 GeoCoord cx cy = glCenter parentGlacier
                 parentDir = glFlowDir parentGlacier
                 -- Branch goes 30-60° off to one side
                 branchAngle = parentDir
                             + (if hashToFloatGeo h2 < 0.5 then 1.0 else -1.0)
                             * (0.5 + hashToFloatGeo h3 * 0.5)
                 -- Starts partway down the parent glacier
                 startDist = fromIntegral (glLength parentGlacier) * (0.3 + hashToFloatGeo h4 * 0.4)
                 branchX = cx + round (startDist * cos parentDir)
                 branchY = cy + round (startDist * sin parentDir)
                 childGlacier = GlacierParams
                     { glCenter      = GeoCoord branchX branchY
                     , glFlowDir     = branchAngle
                     , glLength      = hashToRangeGeo h5 15 40
                     , glWidth       = max 4 (glWidth parentGlacier `div` 2)
                     , glThickness   = max 3 (glThickness parentGlacier * 2 `div` 3)
                     , glCarveDepth  = max 5 (glCarveDepth parentGlacier * 2 `div` 3)
                     , glMoraineSize = hashToRangeGeo h6 3 8
                     , glIsIceSheet  = False  -- branches are always alpine
                     }
                 childPf = PersistentFeature
                     { pfId               = childId
                     , pfFeature          = HydroShape $ GlacierFeature childGlacier
                     , pfActivity         = FActive
                     , pfFormationPeriod   = periodIdx
                     , pfLastActivePeriod  = periodIdx
                     , pfEruptionCount     = 1
                     , pfParentId          = Just fid
                     }
                 evt = HydroModify fid (GlacierBranch
                         (GeoCoord branchX branchY) branchAngle
                         (hashToRangeGeo h7 15 40) childId)
                 tbs'' = registerFeature childPf tbs'
                 tbs''' = updateFeature fid
                     (\p → p { pfEruptionCount = pfEruptionCount p + 1
                              , pfLastActivePeriod = periodIdx }) tbs''
             in (evt : events, tbs''')

        else if roll < 0.75
        -- 10%: Surge — rapid advance with heavy carving
        -- Like a volcanic reactivation: big event, deep modification
        then let h2 = hashGeo seed fidInt 910
                 h3 = hashGeo seed fidInt 911
                 surgeLen = hashToRangeGeo h2 30 80
                 surgeWid = hashToRangeGeo h3 5 15
                 evt = HydroModify fid (GlacierAdvance surgeLen surgeWid)
                 tbs' = updateFeature fid
                     (\p → p { pfEruptionCount = pfEruptionCount p + 1
                              , pfLastActivePeriod = periodIdx }) tbs
             in (evt : events, tbs')

        else if roll < 0.80
        -- 5%: Spawn meltwater river at terminus
        then spawnMeltwaterRiver seed periodIdx fid pf (events, tbs)

        else
        -- 20%: Continue — stable advance, slight deepening
        let h2 = hashGeo seed fidInt 915
            advLen = hashToRangeGeo h2 3 10
            evt = HydroModify fid (GlacierAdvance advLen 0)
            tbs' = updateFeature fid
                (\p → p { pfLastActivePeriod = periodIdx }) tbs
        in (evt : events, tbs')

    -- ===== MODERATE WORLD =====
    | temp ≤ 1.2 =
        if roll < 0.20
        -- 20%: Slow advance
        then let h2 = hashGeo seed fidInt 920
                 advLen = hashToRangeGeo h2 5 15
                 evt = HydroModify fid (GlacierAdvance advLen 0)
                 tbs' = updateFeature fid
                     (\p → p { pfLastActivePeriod = periodIdx }) tbs
             in (evt : events, tbs')

        else if roll < 0.40
        -- 20%: Retreat — glacier shortens, dumps moraine
        then let h2 = hashGeo seed fidInt 921
                 h3 = hashGeo seed fidInt 922
                 retreatLen = hashToRangeGeo h2 10 30
                 moraineDep = hashToRangeGeo h3 3 12
                 evt = HydroModify fid (GlacierRetreat retreatLen moraineDep)
                 tbs' = updateFeature fid
                     (\p → p { pfActivity = FDormant
                              , pfLastActivePeriod = periodIdx }) tbs
             in (evt : events, tbs')

        else if roll < 0.50
        -- 10%: Branch (same as cold, but shorter)
        then let (childId, tbs') = allocFeatureId tbs
                 parentGlacier = getGlacierParams pf
                 h2 = hashGeo seed fidInt 925
                 h3 = hashGeo seed fidInt 926
                 h4 = hashGeo seed fidInt 927
                 h5 = hashGeo seed fidInt 928
                 GeoCoord cx cy = glCenter parentGlacier
                 parentDir = glFlowDir parentGlacier
                 branchAngle = parentDir
                             + (if hashToFloatGeo h2 < 0.5 then 0.8 else -0.8)
                 startDist = fromIntegral (glLength parentGlacier) * 0.5
                 branchX = cx + round (startDist * cos parentDir)
                 branchY = cy + round (startDist * sin parentDir)
                 childGlacier = GlacierParams
                     { glCenter      = GeoCoord branchX branchY
                     , glFlowDir     = branchAngle
                     , glLength      = hashToRangeGeo h3 8 20
                     , glWidth       = max 3 (glWidth parentGlacier `div` 2)
                     , glThickness   = max 2 (glThickness parentGlacier `div` 2)
                     , glCarveDepth  = max 3 (glCarveDepth parentGlacier `div` 2)
                     , glMoraineSize = hashToRangeGeo h4 2 6
                     , glIsIceSheet  = False
                     }
                 childPf = PersistentFeature
                     { pfId               = childId
                     , pfFeature          = HydroShape $ GlacierFeature childGlacier
                     , pfActivity         = FActive
                     , pfFormationPeriod   = periodIdx
                     , pfLastActivePeriod  = periodIdx
                     , pfEruptionCount     = 1
                     , pfParentId          = Just fid
                     }
                 evt = HydroModify fid (GlacierBranch
                         (GeoCoord branchX branchY) branchAngle
                         (hashToRangeGeo h5 8 20) childId)
                 tbs'' = registerFeature childPf tbs'
                 tbs''' = updateFeature fid
                     (\p → p { pfEruptionCount = pfEruptionCount p + 1 }) tbs''
             in (evt : events, tbs''')

        else if roll < 0.55
        -- 5%: Spawn meltwater river
        then spawnMeltwaterRiver seed periodIdx fid pf (events, tbs)

        else if roll < 0.65
        -- 10%: Moraine dams valley → lake forms
        then spawnMoraineLake seed periodIdx fid pf (events, tbs)

        else
        -- 35%: Continue stable
        (events, tbs)

    -- ===== WARM WORLD =====
    | otherwise =
        if roll < 0.10
        -- 10%: Advance (only if very high elevation / ice sheet)
        then let h2 = hashGeo seed fidInt 940
                 advLen = hashToRangeGeo h2 3 8  -- minimal
                 evt = HydroModify fid (GlacierAdvance advLen 0)
                 tbs' = updateFeature fid
                     (\p → p { pfLastActivePeriod = periodIdx }) tbs
             in (evt : events, tbs')

        else if roll < 0.45
        -- 35%: Retreat with moraine
        then let h2 = hashGeo seed fidInt 941
                 h3 = hashGeo seed fidInt 942
                 retreatLen = hashToRangeGeo h2 20 60
                 moraineDep = hashToRangeGeo h3 5 20
                 evt = HydroModify fid (GlacierRetreat retreatLen moraineDep)
                 tbs' = updateFeature fid
                     (\p → p { pfActivity = FDormant
                              , pfLastActivePeriod = periodIdx }) tbs
             in (evt : events, tbs')

        else if roll < 0.65
        -- 20%: Melt entirely — glacier dies, valley + moraine persist
        -- Like GoExtinct for volcanoes
        then let h2 = hashGeo seed fidInt 943
                 moraineDep = hashToRangeGeo h2 8 25
                 evt = HydroModify fid (GlacierMelt moraineDep)
                 tbs' = updateFeature fid
                     (\p → p { pfActivity = FExtinct }) tbs
             in (evt : events, tbs')

        else if roll < 0.75
        -- 10%: Moraine dam → lake
        then spawnMoraineLake seed periodIdx fid pf (events, tbs)

        else if roll < 0.80
        -- 5%: Meltwater river
        then spawnMeltwaterRiver seed periodIdx fid pf (events, tbs)

        else
        -- 20%: Slow retreat (no explicit event, just mark as dormant)
        let tbs' = updateFeature fid
                (\p → p { pfActivity = FDormant
                         , pfLastActivePeriod = periodIdx }) tbs
        in (events, tbs')


-- | Dormant (stable/retreating) glacier evolution
evolveFDormantGlacier ∷ Word64 → Int → GeoState → Float → Float
                      → GeoFeatureId → Int → PersistentFeature
                      → ([GeoEvent], TimelineBuildState)
                      → ([GeoEvent], TimelineBuildState)
evolveFDormantGlacier seed periodIdx gs roll temp fid fidInt pf (events, tbs)
    -- Cold snap: dormant glaciers can reactivate
    | temp < 0.8 =
        if roll < 0.40
        -- 40%: Reactivate — like volcano going Active again
        then let h2 = hashGeo seed fidInt 950
                 h3 = hashGeo seed fidInt 951
                 advLen = hashToRangeGeo h2 15 50
                 advWid = hashToRangeGeo h3 3 10
                 evt = HydroModify fid (GlacierAdvance advLen advWid)
                 tbs' = updateFeature fid
                     (\p → p { pfActivity = FActive
                              , pfEruptionCount = pfEruptionCount p + 1
                              , pfLastActivePeriod = periodIdx }) tbs
             in (evt : events, tbs')
        else
        -- 60%: Stay dormant
        (events, tbs)

    -- Moderate: might reactivate or melt
    | temp ≤ 1.2 =
        if roll < 0.15
        -- 15%: Reactivate (slowly)
        then let h2 = hashGeo seed fidInt 955
                 advLen = hashToRangeGeo h2 5 15
                 evt = HydroModify fid (GlacierAdvance advLen 0)
                 tbs' = updateFeature fid
                     (\p → p { pfActivity = FActive
                              , pfLastActivePeriod = periodIdx }) tbs
             in (evt : events, tbs')

        else if roll < 0.35
        -- 20%: Melt entirely
        then let h2 = hashGeo seed fidInt 956
                 moraineDep = hashToRangeGeo h2 5 15
                 evt = HydroModify fid (GlacierMelt moraineDep)
                 tbs' = updateFeature fid
                     (\p → p { pfActivity = FExtinct }) tbs
             in (evt : events, tbs')

        else
        -- 65%: Stay dormant
        (events, tbs)

    -- Warm: most dormant glaciers will die
    | otherwise =
        if roll < 0.60
        -- 60%: Melt
        then let h2 = hashGeo seed fidInt 960
                 moraineDep = hashToRangeGeo h2 8 25
                 evt = HydroModify fid (GlacierMelt moraineDep)
                 tbs' = updateFeature fid
                     (\p → p { pfActivity = FExtinct }) tbs
             in (evt : events, tbs')

        else if roll < 0.75
        -- 15%: Moraine dam → lake (the glacier's last gift)
        then spawnMoraineLake seed periodIdx fid pf (events, tbs)

        else
        -- 25%: Linger dormant (high altitude holdouts)
        (events, tbs)
