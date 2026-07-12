{-# OPTIONS_GHC -fprof-auto #-}
{-# LANGUAGE Strict, UnicodeSyntax #-}
module World.Geology.Timeline.River.Reconcile
    ( reconcileHydrology
    ) where
import UPrelude
import qualified Data.Vector.Unboxed as VU
import World.Types
import World.Geology.Hash (scaleCount)
import World.Hydrology.Simulation (ElevGrid(..), FlowResult(..))
import World.Geology.Timeline.Helpers
    ( isActiveRiver, isLakeFeature, isSourceNew, getRiverParamsFromPf
    , reconcileLakes )
import World.Geology.Timeline.RiverTrace (traceRiverFromSource)
import World.Geology.Timeline.River.Evolve (evolveExistingRiver)
import World.Geology.Timeline.River.SourceDiversity
    ( spatiallyDiverseSources, filterOverlappingMouths )

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
