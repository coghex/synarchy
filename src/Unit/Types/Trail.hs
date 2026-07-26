{-# LANGUAGE Strict, UnicodeSyntax, DeriveGeneric, DeriveAnyClass #-}
-- | Transient per-unit ongoing-bleeding emitter state — BOTH halves:
--   the moving trail (issue #882, see "Blood.Trail") and the stationary/
--   collapsed pool (issue #883, see "Blood.Pool"). One accumulator
--   drives both, so a walk-then-stop hands the same conserved blood
--   from trail marks to pool layers with nothing lost at the seam.
--   Deliberately dependency-free (Base/Types split convention) so
--   'Unit.Types.Instance' can carry a field of this type with no risk
--   of a cycle back through "Blood.Types" (which already imports
--   'Unit.Types' for 'Unit.Types.UnitId').
--
--   Runtime-only — NEVER serialized (see
--   'Unit.Types.Instance.uiTrailState' and
--   docs/persistence_state_inventory.md's "UnitInstance (reset-on-load
--   fields)" table). A unit that has never bled externally carries no
--   entry at all ('Nothing' on 'Unit.Types.Instance.uiTrailState'),
--   which doubles as "no active trail" for the debug query
--   (@blood.getTrailState@).
module Unit.Types.Trail
    ( TrailState(..)
    , emptyTrailState
    ) where

import UPrelude
import GHC.Generics (Generic)

-- | @tsPendingVolume@ — externally-visible blood (litres) drained since
--   the last placed mark, not yet spent on one (see
--   "Combat.Wounds.Tick"'s conserved external-loss accounting, which is
--   the only writer that ADDS to this). @tsDistSinceMark@ — path
--   distance (world tiles) travelled since the last mark. @tsLastMarkAt@
--   — the absolute 'Engine.Core.State.gameTimeRef' seconds of the last
--   mark (or of this accumulator's creation, before any mark has fired)
--   — deliberately NOT the world calendar clock @world.setTimeScale@
--   advances, so trail cadence is immune to time-scale changes. Distance
--   and elapsed-since-@tsLastMarkAt@ are the two gates
--   'Blood.Trail.consumeTrailMarks' checks before popping a mark; both
--   are consumed (reduced) together when one pops.
--
--   @tsClusterAnchor@/@tsClusterLayers@ are the stationary half (issue
--   #883): the world position the CURRENT pool cluster is growing
--   around, and how many layers have already been spawned into it.
--   'Nothing' means no cluster is anchored yet (the movement consumer
--   anchors one on its first tick for this unit). @tsLastMarkAt@ is
--   shared by both halves — a pool layer is an emission like any other
--   and resets the cadence AND distance gates, which is what makes
--   moving/stationary transitions seamless rather than two competing
--   clocks. The layer count is bumped ONLY by
--   'Blood.Pool.consumePoolLayers' and reset ONLY when
--   'Blood.Pool.classifyOngoing' sees the unit genuinely leave
--   'Blood.Pool.ptClusterRadius' of the anchor: it is never re-derived
--   from the decal store, so a global FIFO eviction can never reopen an
--   exhausted layer budget (issue #883 requirement 3).
data TrailState = TrailState
    { tsPendingVolume ∷ !Float
    , tsDistSinceMark ∷ !Float
    , tsLastMarkAt    ∷ !Double
    , tsClusterAnchor ∷ !(Maybe (Float, Float))
    , tsClusterLayers ∷ !Int
    } deriving (Show, Eq, Generic)

-- | A freshly-created accumulator with nothing pending yet, anchored at
--   game-time 0 and with no pool cluster — real production code always
--   constructs a 'TrailState' stamped with the real
--   'Engine.Core.State.gameTimeRef' value at first-bleed time (see
--   "Combat.Wounds.Tick"), so this is mostly a convenience for tests
--   that don't care about the anchor.
emptyTrailState ∷ TrailState
emptyTrailState = TrailState 0 0 0 Nothing 0
