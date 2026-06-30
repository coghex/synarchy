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
import Data.List (foldl', minimumBy, sortBy)
import Data.Ord (comparing, Down(..))
import Data.Word (Word64)
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as VU
import World.Base (GeoCoord(..), GeoFeatureId(..))
import World.Types
import World.Fluid.River (fixupSegmentContinuity)
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
import World.Chunk.Types (chunkSize)

-- * Hydrology reconciliation

reconcileHydrology ∷ Word64 → Int → FlowResult → Int → Int
                   → ElevGrid
                   → TimelineBuildState
                   → ([PersistentFeature], [GeoEvent], TimelineBuildState)
reconcileHydrology seed ageIdx flowResult periodIdx worldSize elevGrid tbs =
    let filledElev = frFilledElev flowResult
        flowDir    = frFlowDir flowResult
    in reconcileHydrology' seed ageIdx flowResult periodIdx worldSize
                           elevGrid filledElev flowDir tbs

reconcileHydrology' ∷ Word64 → Int → FlowResult → Int → Int
                    → ElevGrid → VU.Vector Int → VU.Vector Int
                    → TimelineBuildState
                    → ([PersistentFeature], [GeoEvent], TimelineBuildState)
reconcileHydrology' seed ageIdx flowResult periodIdx worldSize elevGrid filledElev flowDir tbs =
    let existingRivers = filter (isActiveRiver . pfFeature) (tbsFeatures tbs)
        existingLakes  = filter (isLakeFeature . pfFeature)  (tbsFeatures tbs)
        simSources = frRiverSources flowResult
        simLakes   = frLakes flowResult

        -- Keep river count low enough that continents aren't drowning.
        -- A 256-tile world (~1 continent) should have ~15-25 rivers.
        maxTotalRivers = max 8 (scaleCount worldSize 12)
        currentRiverCount = length existingRivers

        existingRiverIds = map pfId existingRivers

        (evolvedPfs, evolveEvents, tbs1) =
            foldl' (\(pfs, evts, st) fid →
                case lookupFeature fid st of
                    Nothing → (pfs, evts, st)
                    Just existPf →
                        if not (isActiveRiver (pfFeature existPf))
                        then (pfs, evts, st)
                        else let (pf', evt, st') = evolveExistingRiver
                                                       seed ageIdx periodIdx worldSize
                                                       existPf st
                             in (pf' : pfs, evt ⧺ evts, st')
            ) ([], [], tbs) existingRiverIds

        budget = maxTotalRivers - currentRiverCount

        currentExistingRivers = filter (isActiveRiver . pfFeature) (tbsFeatures tbs1)

        newSources = if budget ≤ 0
            then []
            else spatiallyDiverseSources worldSize budget
                     (filter (isSourceNew worldSize currentExistingRivers) simSources)

        climate = tbsClimateState tbs

        allNewRivers = catMaybes $
            map (\(idx, (gx, gy, elev, flow)) →
                traceRiverFromSource seed worldSize elevGrid filledElev flowDir
                    gx gy elev (ageIdx * 1000 + idx) flow climate
            ) (zip [0..] newSources)

        -- Filter out rivers whose mouths cluster together (asterisk pattern).
        -- Keep only one river per mouth region.
        existingMouths = [ rpMouthRegion (getRiverParamsFromPf pf)
                         | pf ← currentExistingRivers ]
        newRivers = filterOverlappingMouths worldSize existingMouths allNewRivers

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

        (lakePfs, lakeEvents, tbs3) = reconcileLakes seed ageIdx periodIdx
                                          worldSize existingLakes simLakes tbs2

        allNewPfs = evolvedPfs ⧺ newPfs ⧺ lakePfs
        allEvents = evolveEvents ⧺ newEvents ⧺ lakeEvents

    in (allNewPfs, allEvents, tbs3)

-- * River evolution

evolveExistingRiver ∷ Word64 → Int → Int → Int
                    → PersistentFeature
                    → TimelineBuildState
                    → (PersistentFeature, [GeoEvent], TimelineBuildState)
evolveExistingRiver seed ageIdx periodIdx worldSize pf tbs =
    let fid = pfId pf
        GeoFeatureId fidInt = fid
        river = getRiverParamsFromPf pf
        h1 = hashGeo seed fidInt (900 + ageIdx)
        roll = hashToFloatGeo h1

        currentRiverCount = length $ filter (isActiveRiver . pfFeature) (tbsFeatures tbs)
        canBranch = currentRiverCount < 60
    in
    if roll < 0.25
    -- 25%: Meander — path changes in persistent state.
    --   Do NOT re-emit HydroEvent for the whole river.
    --   The valley already exists from when the river was first created.
    --   With target-based carving, re-emitting is harmless but wasteful.
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
         in (updatedPf, [evt], tbs')  -- FIXED: was [evt, HydroEvent (RiverFeature newRiver)]

    else if roll < 0.30 ∧ canBranch
    -- 5%: Branch — new tributary, emit HydroEvent only for the NEW tributary
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
                -- Wrap coordinates to canonical u-space. Parent river
                -- segments may have unwrapped coords from path tracing.
                GeoCoord bx0 by0 = rsStart branchSeg
                GeoCoord bex0 bey0 = rsEnd branchSeg
                (bx, by) = wrapCoord worldSize bx0 by0
                (bex, bey) = wrapCoord worldSize bex0 bey0

                segDX = fromIntegral (bex - bx) ∷ Float
                segDY = fromIntegral (bey - by) ∷ Float
                segAngle = atan2 segDY segDX
                side = if hashToFloatGeo h3 < 0.5 then 1.0 else -1.0
                branchAngle = segAngle + side * (0.7 + hashToFloatGeo h4 * 0.7)
                branchLen = hashToRangeGeo h5 15 50
                srcX = bx - round (fromIntegral branchLen * cos branchAngle)
                srcY = by - round (fromIntegral branchLen * sin branchAngle)

                numTribSegs = hashToRangeGeo h6 2 4
                branchElev = rsStartElev branchSeg
                rawTribSegs = buildTributarySegments seed fidInt
                                  srcX srcY bx by numTribSegs branchElev

                -- Force the trib's LAST segment to terminate at an
                -- elevation that makes its channel-floor match the main
                -- at the branch point. Without this, a shallow trib
                -- (depth ≈ 2-3) joining a deep main (depth ≈ 9) leaves
                -- a vertical water cliff of (mainDepth - tribDepth)
                -- tiles at the confluence — visible as a multi-tile
                -- waterfall the renderer draws as a straight drop.
                --
                --   target = branchElev − mainDepth + tribLastDepth
                --     ↳ trib cf at end = target − tribLastDepth
                --                       = branchElev − mainDepth
                --                       = main cf at branch
                --
                -- Clamped to ≤ rsStartElev of the last seg so we never
                -- ask the trib to flow uphill (audit #12 sub-bug B's
                -- monotonic-descent invariant). When the clamp bites,
                -- the residual mismatch is unavoidable without
                -- truncating the trib further upstream.
                tribSegs =
                    if V.null rawTribSegs then rawTribSegs
                    else let lastIdx = V.length rawTribSegs - 1
                             lastSeg = rawTribSegs V.! lastIdx
                             mainDepth = rsDepth branchSeg
                             target = branchElev - mainDepth + rsDepth lastSeg
                             clamped = min target (rsStartElev lastSeg)
                         in rawTribSegs V.// [(lastIdx, lastSeg { rsEndElev = clamped })]

                tributaryParams = RiverParams
                    { rpSourceRegion = GeoCoord srcX srcY
                    , rpMouthRegion  = GeoCoord bx by
                    , rpSegments     = tribSegs
                    , rpFlowRate     = rsFlowRate branchSeg * 0.4
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
                 , HydroEvent (RiverFeature tributaryParams)
                 ]
               , tbs''' )

    else if roll < 0.50
    -- 20%: Deepen — NO HydroEvent, only state update
    then evolveDeepen seed ageIdx periodIdx pf river tbs

    else if roll < 0.60
    -- 10%: Widen — NO HydroEvent, only state update
    then let newSegs = V.map (\seg → seg
                 { rsWidth = min 16 (rsWidth seg + 1)
                 , rsValleyWidth = rsValleyWidth seg + 3
                 }) (rpSegments river)
             newRiver = river { rpSegments = newSegs }
             updatedPf = pf
                 { pfFeature          = HydroShape $ RiverFeature newRiver
                 , pfLastActivePeriod = periodIdx
                 }
             tbs' = updateFeature fid (const updatedPf) tbs
         in (updatedPf, [], tbs')

    else
    -- 40%: Continue unchanged
    (pf, [], tbs)

-- | Deepen — updates persistent state but emits NO events.
--   The deeper channel will be reflected next time the river
--   is emitted (meander or new creation).
--
--   Channel deepens toward a per-segment target derived from the
--   segment's flow rate (audit #14). Previously a flat cap of 20
--   meant old rivers monotonically reached 20 regardless of flow,
--   while late-formed rivers stayed shallow. Now small rivers
--   converge to shallow targets, big rivers to deep ones.
evolveDeepen ∷ Word64 → Int → Int
             → PersistentFeature → RiverParams
             → TimelineBuildState
             → (PersistentFeature, [GeoEvent], TimelineBuildState)
evolveDeepen seed ageIdx periodIdx pf river tbs =
    let fid = pfId pf
        GeoFeatureId fidInt = fid
        h2 = hashGeo seed fidInt (930 + ageIdx)
        deepenAmt = hashToRangeGeo h2 1 3
        -- Flow-driven target depth. Flow rates come from
        -- Simulation.hs as `accum * 0.005 + 0.1`: a small river
        -- (flow ≈ 0.5) targets depth 3; a large river (flow ≈ 5)
        -- targets 20. After merges (audit #12), downstream segments
        -- inherit the combined flow and can deepen further.
        targetDepth seg = min 20
                        $ max 3
                        $ floor (rsFlowRate seg * 4.0 ∷ Float)
        newSegs = fixupSegmentContinuity $ V.map (\seg →
            let cap = targetDepth seg
            in seg { rsDepth = min cap (rsDepth seg + deepenAmt)
                   , rsValleyWidth = rsValleyWidth seg + 1
                   }) (rpSegments river)
        newRiver = river { rpSegments = newSegs }
        updatedPf = pf
            { pfFeature          = HydroShape $ RiverFeature newRiver
            , pfLastActivePeriod = periodIdx
            , pfEruptionCount    = pfEruptionCount pf + 1
            }
        tbs' = updateFeature fid (const updatedPf) tbs
    in (updatedPf, [], tbs')

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
                        then error "performMerge: empty tributary"
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

-- * Spatial diversity for source selection

-- | Select river sources with spatial diversity. Instead of just
--   taking the N highest-flow sources (which cluster in one basin),
--   partition sources into spatial buckets and pick the best from
--   each bucket first. This ensures rivers spawn across the whole map.
spatiallyDiverseSources ∷ Int → Int → [(Int, Int, Int, Float)]
                        → [(Int, Int, Int, Float)]
spatiallyDiverseSources worldSize budget sources
    | budget ≤ 0 = []
    | null sources = []
    | otherwise =
        let totalTiles = worldSize * 16
            -- Bucket size: ~64 tiles per bucket → good spatial spread
            bucketSize = max 32 (totalTiles `div` 8)

            -- Assign each source to a spatial bucket
            toBucket (gx, gy, _, _) =
                let bx = (gx + totalTiles) `div` bucketSize
                    by = (gy + totalTiles) `div` bucketSize
                in (bx, by)

            -- Group by bucket, keeping flow order within each bucket
            -- (sources are already sorted by flow descending)
            addToBuckets [] buckets = buckets
            addToBuckets (s:ss) buckets =
                let b = toBucket s
                    buckets' = insertBucket b s buckets
                in addToBuckets ss buckets'

            insertBucket key val [] = [(key, [val])]
            insertBucket key val ((k, vs):rest)
                | k ≡ key   = (k, vs ⧺ [val]) : rest
                | otherwise = (k, vs) : insertBucket key val rest

            buckets = addToBuckets sources []

            -- Round-robin: take one source from each bucket in turn
            roundRobin _ 0 = []
            roundRobin [] _ = []
            roundRobin bkts n =
                let (picks, remaining) = unzip
                        [ (v, (k, vs'))
                        | (k, vs) ← bkts
                        , (v, vs') ← case vs of
                            (v':vs'') → [(v', vs'')]
                            _         → [] ]
                    remaining' = filter (not . null . snd) remaining
                in take n picks ⧺ roundRobin remaining' (n - length picks)

        in take budget (roundRobin buckets budget)

-- | Filter out rivers whose mouths are too close to existing or
--   previously-accepted river mouths. This prevents the asterisk pattern
--   where multiple rivers all converge to the same point.
filterOverlappingMouths ∷ Int → [GeoCoord] → [RiverParams] → [RiverParams]
filterOverlappingMouths worldSize existingMouths = go existingMouths
  where
    mouthThreshold = 30 ∷ Int  -- min distance between river mouths
    go _ [] = []
    go mouths (r:rs) =
        let GeoCoord mx my = rpMouthRegion r
            tooClose = any (\(GeoCoord ex ey) →
                let (dxi, dyi) = wrappedDeltaUV worldSize mx my ex ey
                in abs dxi < mouthThreshold ∧ abs dyi < mouthThreshold
                ) mouths
        in if tooClose
           then go mouths rs  -- skip this river
           else r : go (rpMouthRegion r : mouths) rs

-- | Wrap (gx, gy) to canonical u-space.
wrapCoord ∷ Int → Int → Int → (Int, Int)
wrapCoord ws gx gy =
    let w = ws * chunkSize
        halfW = w `div` 2
        u = gx - gy
        v = gx + gy
        wrappedU = ((u + halfW) `mod` w + w) `mod` w - halfW
    in ((wrappedU + v) `div` 2, (v - wrappedU) `div` 2)
