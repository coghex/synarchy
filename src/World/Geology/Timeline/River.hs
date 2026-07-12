{-# LANGUAGE UnicodeSyntax #-}
-- | River timeline evolution, split (issue #583) into focused
--   submodules under "World.Geology.Timeline.River.*":
--
--     * "World.Geology.Timeline.River.Reconcile" — per-age hydrology
--       reconciliation: evolve existing rivers, spawn new ones from
--       flow-sim sources, and fold in lake reconciliation.
--     * "World.Geology.Timeline.River.Evolve" — per-river per-age
--       evolution (meander, branch, deepen, widen, or hold).
--     * "World.Geology.Timeline.River.Merge" — converging-river
--       detection and tributary-into-main merging.
--     * "World.Geology.Timeline.River.SourceDiversity" — spatially
--       diverse source selection and overlapping-mouth filtering.
--
--   This module is a pure re-export facade; both entry points are
--   the same public API as before the split.
module World.Geology.Timeline.River
    ( reconcileHydrology
    , mergeConvergingRivers
    ) where
import World.Geology.Timeline.River.Reconcile (reconcileHydrology)
import World.Geology.Timeline.River.Merge (mergeConvergingRivers)
