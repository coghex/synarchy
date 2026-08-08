{-# LANGUAGE Strict #-}
-- | Which persistent feature each of a timeline's rivers came from
--   (#1102). The ONE place that association is computed, so the naming
--   pass at world init and @world.getRivers@ at query time can never
--   disagree about which river is which.
--
--   /Why this needs computing at all./ A river is a
--   'World.Types.PersistentFeature' carrying a stable 'GeoFeatureId',
--   but the rivers a caller can actually read are the compacted
--   @HydroEvent (RiverFeature _)@ entries in
--   'World.Types.gtPeriods' — 'World.Types.RiverParams' carries no id
--   of its own, and adding one would change a worldgen-output schema
--   for a labelling feature (see "World.River.Naming").
--
--   /Why pairing them is sound./
--   'World.Geology.Timeline.Compact.compactRiverEvents' STRIPS every
--   river event from every period and re-emits exactly one per active
--   persistent river, in @gtFeatures@ order, into the most recent Age
--   period. After it runs — and it runs once, at the end of
--   'World.Geology.Timeline.buildTimeline', with nothing adding river
--   events afterwards — the timeline's river events are that list, in
--   that order. Compaction rewrites only segment ELEVATIONS
--   (re-sampling and tributary alignment), never a river's source,
--   mouth, or flow, so the pairing is additionally CHECKED against
--   those before it is trusted.
--
--   A timeline that fails the check yields rivers with no id rather
--   than misattributed ones: a wrong id would silently attach one
--   river's name to another and persist that mistake.
--
--   Pure: no engine, world, Lua, or IO state.
module World.River.Identity
    ( timelineRivers
    , timelineRiverFeatureIds
    ) where

import UPrelude
import Data.Maybe (mapMaybe)
import World.Types
import World.Geology.Timeline.Helpers (isActiveRiver, getRiverParamsFromPf)

-- | Every river the timeline surfaces, in the order it surfaces them,
--   each paired with the id of the persistent feature it was emitted
--   from ('Nothing' only if the invariant above does not hold).
--
--   The river list itself is EXACTLY what @world.getRivers@ has always
--   returned — same values, same order, same count. This function only
--   attaches identity beside it.
timelineRivers ∷ GeoTimeline → [(Maybe GeoFeatureId, RiverParams)]
timelineRivers timeline
    | paired     = zip (map (Just . pfId) actives) rivers
    | otherwise  = map ((,) Nothing) rivers
  where
    rivers  = concatMap extractRivers (gtPeriods timeline)
    actives = activeRiverFeatures timeline
    paired  = length actives ≡ length rivers
                ∧ and (zipWith sameRiver actives rivers)

    -- Source, mouth, and flow are untouched by compaction, so an
    -- event that disagrees with its supposed feature on any of them
    -- is not that feature's river.
    sameRiver pf rp =
        let stored = getRiverParamsFromPf pf
        in rpSourceRegion stored ≡ rpSourceRegion rp
           ∧ rpMouthRegion stored ≡ rpMouthRegion rp
           ∧ rpFlowRate stored ≡ rpFlowRate rp

-- | The feature ids of a timeline's rivers, in the same order, for the
--   pages whose rivers can be identified. A timeline failing the
--   pairing check names no rivers at all rather than naming the wrong
--   ones.
timelineRiverFeatureIds ∷ GeoTimeline → [GeoFeatureId]
timelineRiverFeatureIds = mapMaybe fst . timelineRivers

-- | The persistent river features compaction emits events for, in
--   'gtFeatures' order — the identical filter
--   'World.Geology.Timeline.Compact.compactRiverEvents' applies when it
--   builds that event list.
activeRiverFeatures ∷ GeoTimeline → [PersistentFeature]
activeRiverFeatures timeline =
    [ pf
    | pf ← gtFeatures timeline
    , isActiveRiver (pfFeature pf)
    , pfActivity pf ≡ FActive
    ]

-- | The 'RiverParams' of every river 'HydroEvent' in a period, in event
--   order.
extractRivers ∷ GeoPeriod → [RiverParams]
extractRivers period = concatMap go (gpEvents period)
  where
    go (HydroEvent (RiverFeature rp)) = [rp]
    go _                              = []
