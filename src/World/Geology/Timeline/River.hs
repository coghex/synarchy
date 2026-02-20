{-# OPTIONS_GHC -fprof-auto #-}
{-# LANGUAGE Strict, UnicodeSyntax #-}
module World.Geology.Timeline.River
    ( reconcileHydrology
    , mergeConvergingRivers
    ) where
import UPrelude
import Control.Parallel.Strategies (parMap, rdeepseq)
import Control.DeepSeq (NFData(..))
import Data.Bits (xor)
import Data.List (foldl', minimumBy)
import Data.Ord (comparing)
import Data.Word (Word64)
import qualified Data.Vector as V
import World.Base (GeoCoord(..), GeoFeatureId(..))
import World.Types
import World.Fluids (fixupSegmentContinuity)
import World.Plate (TectonicPlate)
import World.Geology.Types
import World.Geology.Hash
import World.Hydrology.Types (HydroFeature(..), RiverParams(..)
                             , RiverSegment(..), LakeParams(..))
import World.Hydrology.Simulation (ElevGrid(..), FlowResult(..))
import World.Hydrology.River.Meander (meanderSegments)
import World.Hydrology.River.Tributary (buildTributarySegments)
import World.Geology.Timeline.Helpers
    ( isActiveRiver, isLakeFeature, isSourceNew, getRiverParamsFromPf
    , reconcileLakes )
import World.Geology.Timeline.RiverTrace (traceRiverFromSource)

-----------------------------------------------------------
-- Hydrology reconciliation
-----------------------------------------------------------

reconcileHydrology ∷ Word64 → Int → FlowResult → Int → Int
                   → ElevGrid
                   → TimelineBuildState
                   → ([PersistentFeature], [GeoEvent], TimelineBuildState)
reconcileHydrology seed ageIdx flowResult periodIdx worldSize elevGrid tbs =
    let existingRivers = filter (isActiveRiver . pfFeature) (tbsFeatures tbs)
        existingLakes  = filter (isLakeFeature . pfFeature)  (tbsFeatures tbs)
        simSources = frRiverSources flowResult
        simLakes   = frLakes flowResult

        maxTotalRivers = 30  -- fewer, longer rivers
        currentRiverCount = length existingRivers

        -- Capture IDs of existing rivers, NOT the feature values.
        existingRiverIds = map pfId existingRivers

        -- Evolve existing rivers by ID — re-fetch from state each iteration
        (evolvedPfs, evolveEvents, tbs1) =
            foldl' (\(pfs, evts, st) fid →
                case lookupFeature fid st of
                    Nothing → (pfs, evts, st)
                    Just existPf →
                        if not (isActiveRiver (pfFeature existPf))
                        then (pfs, evts, st)
                        else let (pf', evt, st') = evolveExistingRiver
                                                       seed ageIdx periodIdx
                                                       existPf st
                             in (pf' : pfs, evt ++ evts, st')
            ) ([], [], tbs) existingRiverIds

        -- Create new rivers from simulation sources
        budget = maxTotalRivers - currentRiverCount

        -- For filtering, use the CURRENT state's rivers (after evolution)
        currentExistingRivers = filter (isActiveRiver . pfFeature) (tbsFeatures tbs1)

        -- Filter sources: must be far from existing rivers
        newSources = if budget ≤ 0
            then []
            else take budget $ filter (isSourceNew worldSize currentExistingRivers) simSources

        -- Trace each source at tile level
        newRivers = catMaybes $
            parMap rdeepseq (\(idx, (gx, gy, elev, flow)) →
                traceRiverFromSource seed worldSize elevGrid
                    gx gy elev (ageIdx * 1000 + idx) flow
            ) (zip [0..] newSources)

        (newPfs, newEvents, tbs2) =
            foldl' (\(pfs, evts, st) river →
                let (fid, st') = allocFeatureId st
                    pf = PersistentFeature
                        { pfId               = fid
                        , pfFeature          = HydroShape $ RiverFeature river
                        , pfActivity         = FActive
                        , pfFormationPeriod   = periodIdx
                        , pfLastActivePeriod  = periodIdx
                        , pfEruptionCount     = 1
                        , pfParentId          = Nothing
                        }
                    st'' = registerFeature pf st'
                in (pf : pfs, HydroEvent (RiverFeature river) : evts, st'')
            ) ([], [], tbs1) newRivers

        -- Lakes
        (lakePfs, lakeEvents, tbs3) = reconcileLakes seed ageIdx periodIdx
                                          existingLakes simLakes tbs2

        allNewPfs = evolvedPfs ++ newPfs ++ lakePfs
        allEvents = evolveEvents ++ newEvents ++ lakeEvents

    in (allNewPfs, allEvents, tbs3)

-----------------------------------------------------------
-- River evolution
-----------------------------------------------------------

evolveExistingRiver ∷ Word64 → Int → Int
                    → PersistentFeature
                    → TimelineBuildState
                    → (PersistentFeature, [GeoEvent], TimelineBuildState)
evolveExistingRiver seed ageIdx periodIdx pf tbs =
    let fid = pfId pf
        GeoFeatureId fidInt = fid
        river = getRiverParamsFromPf pf
        h1 = hashGeo seed fidInt (900 + ageIdx)
        roll = hashToFloatGeo h1

        -- Count existing rivers to suppress branching when near cap
        currentRiverCount = length $ filter (isActiveRiver . pfFeature) (tbsFeatures tbs)
        canBranch = currentRiverCount < 25
    in
    if roll < 0.25
    -- 25%: Meander
    then let h2 = hashGeo seed fidInt (910 + ageIdx)
             meanderAmt = 0.15 + hashToFloatGeo h2 * 0.4
             newSegs = fixupSegmentContinuity $
                           meanderSegments seed fidInt meanderAmt (rpSegments river)
             newRiver = river { rpSegments = newSegs }
             evt = HydroModify fid (RiverMeander
                     (fromIntegral (hashGeo seed fidInt (911 + ageIdx)))
                     meanderAmt)
             updatedPf = pf
                 { pfFeature          = HydroShape $ RiverFeature newRiver
                 , pfLastActivePeriod = periodIdx
                 }
             tbs' = updateFeature fid (const updatedPf) tbs
         in (updatedPf, [evt, HydroEvent (RiverFeature newRiver)], tbs')

    else if roll < 0.30 ∧ canBranch
    -- 5%: Branch
    then let numSegs = V.length (rpSegments river)
         in if numSegs < 3
            then evolveDeepen seed ageIdx periodIdx pf river tbs
            else
            let (childId, tbs') = allocFeatureId tbs
                h2 = hashGeo seed fidInt (920 + ageIdx)
                h3 = hashGeo seed fidInt (921 + ageIdx)
                h4 = hashGeo seed fidInt (922 + ageIdx)
                h5 = hashGeo seed fidInt (923 + ageIdx)
                h6 = hashGeo seed fidInt (924 + ageIdx)

                branchSegIdx = hashToRangeGeo h2 1 (numSegs - 1)
                branchSeg = rpSegments river V.! min branchSegIdx (numSegs - 1)
                GeoCoord bx by = rsStart branchSeg
                GeoCoord bex bey = rsEnd branchSeg

                segDX = fromIntegral (bex - bx) ∷ Float
                segDY = fromIntegral (bey - by) ∷ Float
                segAngle = atan2 segDY segDX
                side = if hashToFloatGeo h3 < 0.5 then 1.0 else -1.0
                branchAngle = segAngle + side * (0.7 + hashToFloatGeo h4 * 0.7)
                branchLen = hashToRangeGeo h5 15 50
                srcX = bx - round (fromIntegral branchLen * cos branchAngle)
                srcY = by - round (fromIntegral branchLen * sin branchAngle)

                numTribSegs = hashToRangeGeo h6 2 4
                tribSegs = buildTributarySegments seed fidInt
                               srcX srcY bx by numTribSegs

                tributaryParams = RiverParams
                    { rpSourceRegion = GeoCoord srcX srcY
                    , rpMouthRegion  = GeoCoord bx by
                    , rpSegments     = tribSegs
                    , rpFlowRate     = rsFlowRate branchSeg * 0.4
                    , rpMeanderSeed  = fromIntegral (hashGeo seed fidInt (925 + ageIdx))
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

                -- Increase parent flow downstream of branch
                newRiver = river
                    { rpFlowRate = rpFlowRate river + rpFlowRate tributaryParams * 0.5
                    }
                updatedPf = pf
                    { pfFeature          = HydroShape $ RiverFeature newRiver
                    , pfLastActivePeriod = periodIdx
                    , pfEruptionCount    = pfEruptionCount pf + 1
                    }

                tbs'' = registerFeature childPf tbs'
                tbs''' = updateFeature fid (const updatedPf) tbs''

            in ( updatedPf
               , [ evt
                 , HydroEvent (RiverFeature newRiver)
                 , HydroEvent (RiverFeature tributaryParams)
                 ]
               , tbs''' )

    else if roll < 0.50
    -- 15%: Deepen
    then evolveDeepen seed ageIdx periodIdx pf river tbs

    else if roll < 0.60
    -- 10%: Widen
    then let newSegs = V.map (\seg → seg
                 { rsWidth = min 12 (rsWidth seg + 1)
                 , rsValleyWidth = rsValleyWidth seg + 3
                 }) (rpSegments river)
             newRiver = river { rpSegments = newSegs }
             updatedPf = pf
                 { pfFeature          = HydroShape $ RiverFeature newRiver
                 , pfLastActivePeriod = periodIdx
                 }
             tbs' = updateFeature fid (const updatedPf) tbs
         in (updatedPf, [HydroEvent (RiverFeature newRiver)], tbs')

    else
    -- 40%: Continue unchanged
    (pf, [], tbs)

evolveDeepen ∷ Word64 → Int → Int
             → PersistentFeature → RiverParams
             → TimelineBuildState
             → (PersistentFeature, [GeoEvent], TimelineBuildState)
evolveDeepen seed ageIdx periodIdx pf river tbs =
    let fid = pfId pf
        GeoFeatureId fidInt = fid
        h2 = hashGeo seed fidInt (930 + ageIdx)
        deepenAmt = hashToRangeGeo h2 1 3
        maxDepth = 20  -- 200m cap
        newSegs = fixupSegmentContinuity $ V.map (\seg → seg
            { rsDepth = min maxDepth (rsDepth seg + deepenAmt)
            , rsValleyWidth = rsValleyWidth seg + 1
            }) (rpSegments river)
        newRiver = river { rpSegments = newSegs }
        updatedPf = pf
            { pfFeature          = HydroShape $ RiverFeature newRiver
            , pfLastActivePeriod = periodIdx
            , pfEruptionCount    = pfEruptionCount pf + 1
            }
        tbs' = updateFeature fid (const updatedPf) tbs
    in (updatedPf, [HydroEvent (RiverFeature newRiver)], tbs')

-----------------------------------------------------------
-- River merging
-----------------------------------------------------------

mergeConvergingRivers ∷ Int → Int → TimelineBuildState → TimelineBuildState
mergeConvergingRivers worldSize periodIdx tbs =
    let activeRiverIds = map pfId
            $ filter (\pf → isActiveRiver (pfFeature pf)
                          ∧ pfActivity pf ≡ FActive)
                     (tbsFeatures tbs)
        mergeThreshold = 24  -- tiles

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
                midX = sx + wrappedDeltaXGeo worldSize ex sx `div` 2
                midY = (sy + ey) `div` 2
                dx = abs (wrappedDeltaXGeo worldSize mx midX)
                dy = abs (my - midY)
            in (dx + dy, idx, GeoCoord midX midY)
            ) segs
        close = filter (\(d, _, _) → d < threshold) dists
    in case close of
        [] → Nothing
        _  → let (_, idx, coord) = minimumBy (\(d1,_,_) (d2,_,_) → compare d1 d2) close
             in Just (targetPf, coord, idx)

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
                dx = abs (wrappedDeltaXGeo worldSize ex jx)
                dy = abs (ey - jy)
            in (dx + dy, idx)
            ) tribSegs

        cutIdx = case V.null segDists of
            True  → 0
            False → snd (V.minimumBy (comparing fst) segDists)

        keptSegs = V.take cutIdx tribSegs
        cutSeg   = if cutIdx < V.length tribSegs
                   then tribSegs V.! cutIdx
                   else if V.null tribSegs
                        then error "performMerge: empty tributary"
                        else V.last tribSegs

        junctionSeg = cutSeg
            { rsEnd     = junctionCoord
            , rsEndElev = rsStartElev cutSeg
            }

        truncatedSegs = V.snoc keptSegs junctionSeg

        newTribRiver = tribRiver
            { rpMouthRegion = junctionCoord
            , rpSegments    = truncatedSegs
            }

        newMainSegs = V.imap (\idx seg →
            if idx ≥ segIdx
            then seg { rsFlowRate = rsFlowRate seg + rpFlowRate tribRiver * 0.6
                     , rsWidth = min 12 (rsWidth seg + 1)
                     }
            else seg
            ) (rpSegments mainRiver)
        newMainRiver = mainRiver
            { rpSegments = newMainSegs
            , rpFlowRate = rpFlowRate mainRiver + rpFlowRate tribRiver * 0.5
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

-----------------------------------------------------------
-- River matching + evolution of matched rivers
-----------------------------------------------------------

matchRivers ∷ [PersistentFeature] → [RiverParams]
            → ([(PersistentFeature, RiverParams)], [PersistentFeature], [RiverParams])
matchRivers existing simulated =
    let matchRadius = 1000

        go [] remainingSim matched unmatched =
            (matched, unmatched, remainingSim)
        go (ep:eps) remainingSim matched unmatched =
            case findBestMatch ep remainingSim of
                Nothing →
                    go eps remainingSim matched (ep : unmatched)
                Just (simR, restSim) →
                    go eps restSim ((ep, simR) : matched) unmatched

        findBestMatch ep sims =
            let river = getRiverParamsFromPf ep
                GeoCoord sx sy = rpSourceRegion river
                candidates = filter (\sr →
                    let GeoCoord sx2 sy2 = rpSourceRegion sr
                        dx = abs (sx - sx2)
                        dy = abs (sy - sy2)
                    in dx < matchRadius ∧ dy < matchRadius
                    ) sims
            in case candidates of
                [] → Nothing
                _  → let best = minimumBy (\a b →
                             compare (srcDist a river) (srcDist b river)) candidates
                         restSim = filter (/= best) sims
                     in Just (best, restSim)

        srcDist sr existing =
            let GeoCoord x1 y1 = rpSourceRegion sr
                GeoCoord x2 y2 = rpSourceRegion existing
            in abs (x1 - x2) + abs (y1 - y2)

    in go existing simulated [] []

evolveMatchedRiver ∷ Word64 → Int → Int
                   → PersistentFeature → RiverParams
                   → TimelineBuildState
                   → (PersistentFeature, [GeoEvent], TimelineBuildState)
evolveMatchedRiver seed ageIdx periodIdx existPf simRiver tbs =
    let fid = pfId existPf
        oldRiver = getRiverParamsFromPf existPf
        GeoFeatureId fidInt = fid

        h1 = hashGeo seed fidInt (900 + ageIdx)
        deepenAmt = max 1 (round (rpFlowRate simRiver * 2.0))
        maxTotalDepth = 40
        
        mergedSegs = zipWithDefaultV mergeSegment
            (rpSegments oldRiver) (rpSegments simRiver)

        mergeSegment oldSeg newSeg = newSeg
            { rsDepth = min maxTotalDepth
                (rsDepth oldSeg + deepenAmt)
            , rsWidth = min 12 (max (rsWidth oldSeg) (rsWidth newSeg))
            , rsValleyWidth = max (rsValleyWidth oldSeg) (rsValleyWidth newSeg)
            }

        newRiver = simRiver
            { rpSegments    = mergedSegs
            , rpFlowRate    = rpFlowRate simRiver
            , rpMeanderSeed = rpMeanderSeed oldRiver
            }

        updatedPf = existPf
            { pfFeature          = HydroShape $ RiverFeature newRiver
            , pfActivity         = FActive
            , pfLastActivePeriod = periodIdx
            , pfEruptionCount    = pfEruptionCount existPf + 1
            }

        evt = HydroEvent (RiverFeature newRiver)

        tbs' = updateFeature fid (const updatedPf) tbs

    in (updatedPf, [evt], tbs')

zipWithDefaultV ∷ (RiverSegment → RiverSegment → RiverSegment)
                → V.Vector RiverSegment → V.Vector RiverSegment
                → V.Vector RiverSegment
zipWithDefaultV f old new =
    let minLen = min (V.length old) (V.length new)
        merged = V.izipWith (\_ a b → f a b) (V.take minLen old) (V.take minLen new)
        extra  = V.drop minLen new
    in merged V.++ extra

isGenuinelyNew ∷ Int → [PersistentFeature] → RiverParams → Bool
isGenuinelyNew worldSize existingRivers simRiver =
    let GeoCoord sx sy = rpSourceRegion simRiver
        threshold = 120
    in not $ any (\pf →
        let river = getRiverParamsFromPf pf
            GeoCoord ex ey = rpSourceRegion river
            dx = abs (wrappedDeltaXGeo worldSize sx ex)
            dy = abs (sy - ey)
        in dx < threshold ∧ dy < threshold
        ) existingRivers
