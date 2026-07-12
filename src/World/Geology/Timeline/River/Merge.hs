{-# OPTIONS_GHC -fprof-auto #-}
{-# LANGUAGE Strict, UnicodeSyntax #-}
module World.Geology.Timeline.River.Merge
    ( mergeConvergingRivers
    ) where
import UPrelude
import Data.List (minimumBy)
import Data.Ord (comparing)
import qualified Data.Vector as V
import World.Types
import World.Geology.Hash (wrappedDeltaUV)
import World.Geology.Timeline.Helpers (isActiveRiver, getRiverParamsFromPf)

-- * River merging

mergeConvergingRivers ∷ Int → Int → TimelineBuildState → TimelineBuildState
mergeConvergingRivers worldSize periodIdx tbs =
    -- A river that has already been merged into another (has a parent)
    -- is no longer a merge SOURCE candidate — otherwise it keeps
    -- merging into the same main every Age and inflates the main's
    -- flow rate unboundedly (audit #12 sub-bug C). It can still be a
    -- merge TARGET, so chains like C→B→A are allowed.
    let activeRiverIds = map pfId
            $ filter (\pf → isActiveRiver (pfFeature pf)
                          ∧ pfActivity pf ≡ FActive
                          ∧ isNothing (pfParentId pf))
                     (tbsFeatures tbs)
        mergeThreshold = 24

        go [] st = st
        go (fid : rest) st =
            case lookupFeature fid st of
                Nothing → go rest st
                Just pf →
                    if not (isActiveRiver (pfFeature pf))
                       ∨ pfActivity pf ≠ FActive
                    then go rest st
                    else
                    let others = filter (\other →
                            pfId other ≠ fid
                            ∧ isActiveRiver (pfFeature other)
                            ∧ pfActivity other ≡ FActive
                            ) (tbsFeatures st)
                    in case findMergeTarget worldSize mergeThreshold pf others of
                        Nothing → go rest st
                        Just (targetPf, junctionCoord, segIdx) →
                            let st' = performMerge worldSize periodIdx
                                          pf targetPf junctionCoord segIdx st
                            in go rest st'

    in go activeRiverIds tbs

findMergeTarget ∷ Int → Int → PersistentFeature → [PersistentFeature]
               → Maybe (PersistentFeature, GeoCoord, Int)
findMergeTarget worldSize threshold pf others =
    let river = getRiverParamsFromPf pf
        GeoCoord mx my = rpMouthRegion river
    in case catMaybes
            [ checkRiverProximity worldSize threshold mx my other
            | other ← others ] of
        []    → Nothing
        (x:_) → Just x

checkRiverProximity ∷ Int → Int → Int → Int → PersistentFeature
                    → Maybe (PersistentFeature, GeoCoord, Int)
checkRiverProximity worldSize threshold mx my targetPf =
    let targetRiver = getRiverParamsFromPf targetPf
        segs = rpSegments targetRiver
        dists = V.toList $ V.imap (\idx seg →
            let GeoCoord sx sy = rsStart seg
                GeoCoord ex ey = rsEnd seg
                (dex, dey) = wrappedDeltaUV worldSize ex ey sx sy
                midX = sx + dex `div` 2
                midY = sy + dey `div` 2
                (dmx, dmy) = wrappedDeltaUV worldSize mx my midX midY
                dx = abs dmx
                dy = abs dmy
            in (dx + dy, idx, GeoCoord midX midY)
            ) segs
        close = filter (\(d, _, _) → d < threshold) dists
    in case close of
        [] → Nothing
        _  → let (_, idx, coord) = minimumBy (\(d1,_,_) (d2,_,_) → compare d1 d2) close
             in Just (targetPf, coord, idx)

-- | Merge a tributary into a main river at the given junction.
--   rpSegments is always non-empty for active rivers:
--   traceRiverFromSource returns Nothing for degenerate traces,
--   and meanderSegments preserves vector length.  The error on
--   empty tribSegs is a defensive guard for an unreachable case.
performMerge ∷ Int → Int → PersistentFeature → PersistentFeature
             → GeoCoord → Int → TimelineBuildState → TimelineBuildState
performMerge worldSize periodIdx tributaryPf mainPf junctionCoord segIdx tbs =
    let tribId = pfId tributaryPf
        mainId = pfId mainPf
        tribRiver = getRiverParamsFromPf tributaryPf
        mainRiver = getRiverParamsFromPf mainPf

        tribSegs = rpSegments tribRiver
        GeoCoord jx jy = junctionCoord

        segDists = V.imap (\idx seg →
            let GeoCoord ex ey = rsEnd seg
                (dxi, dyi) = wrappedDeltaUV worldSize ex ey jx jy
                dx = abs dxi
                dy = abs dyi
            in (dx + dy, idx)
            ) tribSegs

        cutIdx = case V.null segDists of
            True  → 0
            False → snd (V.minimumBy (comparing fst) segDists)

        keptSegs = V.take cutIdx tribSegs
        cutSeg   = if cutIdx < V.length tribSegs
                   then tribSegs V.! cutIdx
                   else if V.null tribSegs
                        then error $ "performMerge: empty tributary (tribId="
                                   ⧺ show tribId ⧺ " mainId=" ⧺ show mainId
                                   ⧺ " period=" ⧺ show periodIdx
                                   ⧺ " junction=(" ⧺ show jx ⧺ "," ⧺ show jy ⧺ "))"
                        else V.last tribSegs

        -- Junction segment's end elevation must put the tributary's
        -- *channel-floor* (rsEndElev - rsDepth) at the same z as the
        -- main river's channel-floor at the junction. Otherwise the
        -- two water surfaces meet at different elevations and the
        -- renderer draws a vertical water cliff at the confluence
        -- — typically 4-7 tiles tall when a shallow tributary
        -- (depth ≈ 3) meets a deep main (depth ≈ 9).
        --
        -- mainCfAtJunction = mainMidElev − mainDepth
        -- desiredJunctionElev = mainCfAtJunction + tribDepth
        --
        -- Clamped to ≤ cutSeg.rsStartElev to preserve the
        -- monotonic-descent invariant on the trib's last segment
        -- (audit #12 sub-bug B). When this clamp bites, the
        -- mismatch is the residual cliff — unavoidable without
        -- truncating the trib further upstream.
        mainSegs0 = rpSegments mainRiver
        mainSeg = if segIdx < V.length mainSegs0
                  then mainSegs0 V.! segIdx
                  else V.last mainSegs0
        mainMidElev = (rsStartElev mainSeg + rsEndElev mainSeg) `div` 2
        mainCfAtJunction = mainMidElev - rsDepth mainSeg
        desiredJunctionElev = mainCfAtJunction + rsDepth cutSeg
        junctionElev = min (rsStartElev cutSeg) desiredJunctionElev
        junctionSeg = cutSeg
            { rsEnd     = junctionCoord
            , rsEndElev = junctionElev
            }

        truncatedSegs = V.snoc keptSegs junctionSeg

        newTribRiver = tribRiver
            { rpMouthRegion = junctionCoord
            , rpSegments    = truncatedSegs
            }

        -- Mass conservation: tributary's full flow is added to main
        -- at the river level and to every downstream segment. The
        -- previous 0.6 / 0.5 factors lost 40-50% of flow per merge
        -- and disagreed with each other (audit #12 sub-bug A).
        newMainSegs = V.imap (\idx seg →
            if idx ≥ segIdx
            then seg { rsFlowRate = rsFlowRate seg + rpFlowRate tribRiver
                     , rsWidth = min 16 (rsWidth seg + 1)
                     }
            else seg
            ) mainSegs0
        newMainRiver = mainRiver
            { rpSegments = newMainSegs
            , rpFlowRate = rpFlowRate mainRiver + rpFlowRate tribRiver
            }

        tbs' = updateFeature tribId (\p → p
            { pfFeature   = HydroShape $ RiverFeature newTribRiver
            , pfParentId  = Just mainId
            , pfLastActivePeriod = periodIdx
            }) tbs
        tbs'' = updateFeature mainId (\p → p
            { pfFeature   = HydroShape $ RiverFeature newMainRiver
            , pfLastActivePeriod = periodIdx
            }) tbs'

    in tbs''
