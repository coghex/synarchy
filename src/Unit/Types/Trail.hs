{-# LANGUAGE Strict, UnicodeSyntax, DeriveGeneric, DeriveAnyClass #-}
-- | Transient per-unit bleeding-TRAIL emitter state (issue #882, the
--   "moving" half of ongoing bleeding — see "Blood.Trail" for the
--   algorithm this state drives). Deliberately dependency-free (Base/
--   Types split convention) so 'Unit.Types.Instance' can carry a field
--   of this type with no risk of a cycle back through "Blood.Types"
--   (which already imports 'Unit.Types' for 'Unit.Types.UnitId').
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
data TrailState = TrailState
    { tsPendingVolume ∷ !Float
    , tsDistSinceMark ∷ !Float
    , tsLastMarkAt    ∷ !Double
    } deriving (Show, Eq, Generic)

-- | A freshly-created accumulator with nothing pending yet, anchored at
--   game-time 0 — real production code always constructs a 'TrailState'
--   stamped with the real 'Engine.Core.State.gameTimeRef' value at
--   first-bleed time (see "Combat.Wounds.Tick"), so this is mostly a
--   convenience for tests that don't care about the anchor.
emptyTrailState ∷ TrailState
emptyTrailState = TrailState 0 0 0
