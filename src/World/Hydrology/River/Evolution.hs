{-# LANGUAGE Strict, UnicodeSyntax #-}
module World.Hydrology.River.Evolution
    ( evolveRiver
    ) where

import UPrelude
import Data.Word (Word64)
import qualified Data.Vector as V
import World.Base (GeoCoord(..), GeoFeatureId(..))
import World.Types
import World.Hydrology.Types
import World.Geology.Types
import World.Geology.Hash
import World.Hydrology.River.Common (getRiverParams)
import World.Hydrology.River.Meander (meanderSegments)
import World.Hydrology.River.Tributary (buildTributarySegments)

-----------------------------------------------------------
-- River Evolution
-----------------------------------------------------------

-- | Evolve a river feature for one age.
--   Same fold pattern as evolvePointFeature / evolveGlacier.
--
--   Rivers are precipitation-driven, but we don't need to
--   query climate directly — the evolution probabilities
--   capture the range of behaviors.
--
--   Probability table (for Flowing rivers):
--     15% → Branch: new tributary splits off at a random segment
--     10% → Dam: blockage forms, lake appears upstream
--      5% → Capture: steal headwaters from a nearby river
--      5% → Dry up: river goes seasonal (or dies if already seasonal)
--     15% → Meander: path segments shift laterally
--     10% → Deepen: valley carves deeper (increased depth on all segments)
--     40% → Continue: no change, river keeps flowing
--
evolveRiver ∷ Word64 → Int
            → ([GeoEvent], TimelineBuildState)
            → PersistentFeature
            → ([GeoEvent], TimelineBuildState)
evolveRiver seed periodIdx (events, tbs) pf =
    let fid = pfId pf
        GeoFeatureId fidInt = fid
        h1 = hashGeo seed fidInt 800
        roll = hashToFloatGeo h1
    in case pfActivity pf of

        ---------------------------------------------------
        -- Flowing river
        ---------------------------------------------------
        FActive →

            if roll < 0.15
            -- 15%: Branch — new tributary joins this river
            then let river = getRiverParams pf
                     numSegs = length (rpSegments river)
                 in if numSegs < 2
                    then (events, tbs)  -- too short to branch
                    else
                    let (childId, tbs') = allocFeatureId tbs
                        -- Pick a segment to branch from
                        h2 = hashGeo seed fidInt 801
                        h3 = hashGeo seed fidInt 802
                        h4 = hashGeo seed fidInt 803
                        h5 = hashGeo seed fidInt 804
                        h6 = hashGeo seed fidInt 805
                        h7 = hashGeo seed fidInt 806
                        h8 = hashGeo seed fidInt 807
                        branchSegIdx = hashToRangeGeo h2 1 (numSegs - 1)
                        branchSeg = rpSegments river V.! min branchSegIdx (numSegs - 1)
                        GeoCoord bx by = rsStart branchSeg

                        -- Tributary comes from a different direction
                        -- Compute direction of the parent segment
                        GeoCoord bex bey = rsEnd branchSeg
                        segDX = fromIntegral (bex - bx) ∷ Float
                        segDY = fromIntegral (bey - by) ∷ Float
                        segAngle = atan2 segDY segDX
                        -- Branch comes from 40-80° off to one side
                        side = if hashToFloatGeo h3 < 0.5 then 1.0 else -1.0
                        branchAngle = segAngle + side * (0.7 + hashToFloatGeo h4 * 0.7)
                        -- Tributary source is upstream of the branch point
                        branchLen = hashToRangeGeo h5 15 50
                        srcX = bx - round (fromIntegral branchLen * cos branchAngle)
                        srcY = by - round (fromIntegral branchLen * sin branchAngle)

                        -- Build tributary segments (3-5 segments from source to branch point)
                        numTribSegs = hashToRangeGeo h6 2 4
                        tribSegs = buildTributarySegments seed fidInt
                                       srcX srcY bx by numTribSegs

                        tributaryParams = RiverParams
                            { rpSourceRegion = GeoCoord srcX srcY
                            , rpMouthRegion  = GeoCoord bx by
                            , rpSegments     = tribSegs
                            , rpFlowRate     = rsFlowRate branchSeg * 0.4
                            , rpMeanderSeed  = fromIntegral (hashGeo seed fidInt 808)
                            }

                        childPf = PersistentFeature
                            { pfId               = childId
                            , pfFeature          = HydroShape $ RiverFeature tributaryParams
                            , pfActivity         = FActive
                            , pfFormationPeriod   = periodIdx
                            , pfLastActivePeriod  = periodIdx
                            , pfEruptionCount     = 1
                            , pfParentId          = Just fid
                            }

                        evt = HydroModify fid (RiverBranch
                                (GeoCoord bx by) branchAngle branchLen childId)

                        -- Increase parent river's flow downstream of branch point
                        -- (more water coming in from tributary)
                        tbs'' = registerFeature childPf tbs'
                        tbs''' = updateFeature fid
                            (\p → p { pfEruptionCount = pfEruptionCount p + 1
                                     , pfLastActivePeriod = periodIdx }) tbs''
                    in (evt : events, tbs''')

            else if roll < 0.25
            -- 10%: Dam — blockage creates a lake upstream
            then let river = getRiverParams pf
                     numSegs = length (rpSegments river)
                 in if numSegs < 2
                    then (events, tbs)
                    else
                    let (lakeId, tbs') = allocFeatureId tbs
                        h2 = hashGeo seed fidInt 810
                        h3 = hashGeo seed fidInt 811
                        h4 = hashGeo seed fidInt 812
                        h5 = hashGeo seed fidInt 813
                        -- Dam forms at a random segment
                        damSegIdx = hashToRangeGeo h2 0 (numSegs - 2)
                        damSeg = rpSegments river V.! min damSegIdx (numSegs - 1)
                        GeoCoord dx' dy' = rsEnd damSeg
                        damHeight = hashToRangeGeo h3 5 20

                        -- Lake forms just upstream of the dam
                        lakeParams = LakeParams
                            { lkCenter  = GeoCoord dx' dy'
                            , lkRadius  = hashToRangeGeo h4 5 15
                            , lkSurface = damHeight
                            , lkDepth   = hashToRangeGeo h5 3 12
                            , lkSource  = DammedRiver fid
                            }

                        lakePf = PersistentFeature
                            { pfId               = lakeId
                            , pfFeature          = HydroShape $ LakeFeature lakeParams
                            , pfActivity         = FActive
                            , pfFormationPeriod   = periodIdx
                            , pfLastActivePeriod  = periodIdx
                            , pfEruptionCount     = 0
                            , pfParentId          = Just fid
                            }

                        evt = HydroModify fid (RiverDam
                                (GeoCoord dx' dy') lakeId damHeight)

                        -- Mark river as dammed
                        tbs'' = registerFeature lakePf tbs'
                        tbs''' = updateFeature fid
                            (\p → p { pfActivity = FDormant  -- Dammed = dormant
                                     , pfLastActivePeriod = periodIdx }) tbs''
                    in (evt : events, tbs''')

            else if roll < 0.30
            -- 5%: Capture — steal headwaters from another river
            -- (simplified: we just extend the source upstream)
            then let river = getRiverParams pf
                     h2 = hashGeo seed fidInt 820
                     h3 = hashGeo seed fidInt 821
                     h4 = hashGeo seed fidInt 822
                     GeoCoord sx sy = rpSourceRegion river
                     -- Extend source in a random uphill direction
                     extAngle = hashToFloatGeo h2 * 2.0 * π
                     extLen = hashToRangeGeo h3 10 30
                     newSrcX = sx + round (fromIntegral extLen * cos extAngle)
                     newSrcY = sy + round (fromIntegral extLen * sin extAngle)

                     -- Add a new segment at the head
                     newSeg = RiverSegment
                         { rsStart       = GeoCoord newSrcX newSrcY
                         , rsEnd         = GeoCoord sx sy
                         , rsWidth       = 2  -- narrow headwaters
                         , rsValleyWidth = hashToRangeGeo h4 6 12
                         , rsDepth       = 3
                         , rsFlowRate    = 0.1
                         , rsStartElev   = 0
                         , rsEndElev     = 0
                         }

                     newRiver = river
                         { rpSourceRegion = GeoCoord newSrcX newSrcY
                         , rpSegments     = V.cons newSeg (rpSegments river)
                         , rpFlowRate     = rpFlowRate river + 0.1
                         }

                     -- TODO: capture other river
                     evt = HydroModify fid (RiverCapture fid (GeoCoord newSrcX newSrcY))
                     tbs' = updateFeature fid
                         (\p → p { pfFeature = HydroShape $ RiverFeature newRiver
                                  , pfEruptionCount = pfEruptionCount p + 1
                                  , pfLastActivePeriod = periodIdx }) tbs
                 in (evt : events, tbs')

            else if roll < 0.35
            -- 5%: Dry up — river goes seasonal
            then let evt = HydroModify fid RiverDryUp
                     tbs' = updateFeature fid
                         (\p → p { pfActivity = FDormant
                                  , pfLastActivePeriod = periodIdx }) tbs
                 in (evt : events, tbs')

            else if roll < 0.50
            -- 15%: Meander — shift segments laterally
            then let h2 = hashGeo seed fidInt 830
                     meanderAmt = 0.2 + hashToFloatGeo h2 * 0.6
                     evt = HydroModify fid (RiverMeander
                             (fromIntegral (hashGeo seed fidInt 831)) meanderAmt)

                     -- Apply meander: shift each segment's endpoints
                     -- perpendicular to flow direction
                     river = getRiverParams pf
                     newSegs = meanderSegments seed fidInt meanderAmt (rpSegments river)
                     newRiver = river { rpSegments = newSegs }

                     tbs' = updateFeature fid
                         (\p → p { pfFeature = HydroShape $ RiverFeature newRiver
                                  , pfLastActivePeriod = periodIdx }) tbs
                 in (evt : events, tbs')

            else if roll < 0.60
            -- 10%: Deepen — valley cuts deeper on all segments
            then let h2 = hashGeo seed fidInt 835
                     deepenAmount = hashToRangeGeo h2 2 8
                     river = getRiverParams pf
                     newSegs = V.map (\seg → seg
                         { rsDepth = rsDepth seg + deepenAmount
                         , rsWidth = min (rsWidth seg + 1) 10
                         , rsValleyWidth = rsValleyWidth seg + 2
                         }) (rpSegments river)
                     newRiver = river { rpSegments = newSegs }

                     evt = HydroEvent (RiverFeature newRiver)  -- re-emit with deeper params

                     tbs' = updateFeature fid
                         (\p → p { pfFeature = HydroShape $ RiverFeature newRiver
                                  , pfEruptionCount = pfEruptionCount p + 1
                                  , pfLastActivePeriod = periodIdx }) tbs
                 in (evt : events, tbs')

            else
            -- 40%: Continue flowing, no change
            (events, tbs)

        ---------------------------------------------------
        -- Dormant (seasonal or dammed) river
        ---------------------------------------------------
        FDormant →
            if roll < 0.30
            -- 30%: Reactivate — dam breaks or rains return
            then let river = getRiverParams pf
                     h2 = hashGeo seed fidInt 840
                     -- Slightly deeper from the reactivation surge
                     deepenAmt = hashToRangeGeo h2 1 4
                     newSegs = V.map (\seg → seg { rsDepth = rsDepth seg + deepenAmt })
                                   (rpSegments river)
                     newRiver = river { rpSegments = newSegs }

                     evt = HydroEvent (RiverFeature newRiver)

                     tbs' = updateFeature fid
                         (\p → p { pfActivity = FActive
                                  , pfFeature = HydroShape $ RiverFeature newRiver
                                  , pfEruptionCount = pfEruptionCount p + 1
                                  , pfLastActivePeriod = periodIdx }) tbs
                 in (evt : events, tbs')

            else if roll < 0.50
            -- 20%: Die completely — seasonal river dries out
            then let evt = HydroModify fid RiverDryUp
                     tbs' = updateFeature fid
                         (\p → p { pfActivity = FExtinct }) tbs
                 in (evt : events, tbs')

            else
            -- 50%: Stay dormant
            (events, tbs)

        ---------------------------------------------------
        -- Dead rivers don't evolve (but their valleys persist)
        ---------------------------------------------------
        FExtinct   → (events, tbs)
        FCollapsed → (events, tbs)
