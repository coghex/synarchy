{-# LANGUAGE Strict, DeriveGeneric, DeriveAnyClass #-}
-- | Per-movement-request hazard policy (#1217).
--
-- A movement request states, explicitly, whether the route it asks for
-- may traverse a DAMAGING DROP — a descent the cost model itself
-- classifies as a real fall (@drop ≥ pcFallTriggerDrop@, see
-- "Unit.Pathing.Cost"'s @isDamagingDrop@). The policy is a property of
-- the REQUEST, never of the mover: the same unit may wander under
-- 'FallProhibited' one tick and be commanded across the same cliff edge
-- under 'FallPermitted' the next. It is deliberately NOT inferred from
-- species, movement speed, or a "this looks like a meander" heuristic —
-- panic, delirium and mental-break movement all reach meander-speed
-- wandering through @scripts/unit_ai_mental.lua@ and must keep today's
-- fall-permitted behavior.
--
-- 'FallPermitted' is the default everywhere a caller does not say
-- otherwise, so every pre-existing pathing caller keeps its exact
-- current semantics.
--
-- The policy governs DESCENTS only. An ascent is never blocked by it —
-- a protected route may still climb, and a climb that SLIPS
-- ("Unit.Thread.Movement.Climb"'s @rollClimbSlips@ →
-- @convertSlippedClimb@) still converts into a fall. That is a climbing
-- accident, not a route the planner chose over a damaging drop, and it
-- is deliberately left alone here.
--
-- APPEND-ONLY: this enum derives 'Serialize' through 'Generic', so it is
-- positional by constructor tag and rides into saves via
-- "World.Save.Component.EntitySimulation"'s unit-sim component. Adding a
-- constructor is a pure append; inserting or reordering silently
-- corrupts saved move targets (@tools/enum_append_only_audit.py@ guards
-- this).
module Unit.Pathing.Hazard
    ( MoveHazardPolicy(..)
    , defaultMoveHazardPolicy
    , parseMoveHazardPolicy
    , moveHazardPolicyToken
    ) where

import UPrelude
import GHC.Generics (Generic)
import Data.Serialize (Serialize)
import qualified Data.Text as T

-- | May this movement request's route include a damaging drop?
data MoveHazardPolicy
    = FallPermitted
      -- ^ Today's behavior, and the default for every request that does
      --   not ask otherwise: a real fall is discouraged by the cost
      --   model's exponential penalty but never rejected. Player
      --   commands, survival actions, flee/panic, medic/work/fetch
      --   pathing and every mental-state movement stay here.
    | FallProhibited
      -- ^ Ambient, zero-stakes movement (#1217): a step the cost model
      --   classifies as a real fall is IMPASSABLE, in the greedy stepper
      --   and in every local-A* expansion alike. A request that cannot
      --   make safe progress terminates so the ambient AI resamples a
      --   different destination later, rather than idling on an
      --   unreachable target that replans forever.
    deriving (Show, Eq, Generic, Serialize)

-- | The policy a request gets when its caller says nothing — the
--   pre-#1217 behavior, unchanged.
defaultMoveHazardPolicy ∷ MoveHazardPolicy
defaultMoveHazardPolicy = FallPermitted

-- | The scripting-facing token for a policy (the @unit.moveTo@ 5th
--   argument). Round-trips with 'parseMoveHazardPolicy'.
moveHazardPolicyToken ∷ MoveHazardPolicy → Text
moveHazardPolicyToken FallPermitted  = "allow_falls"
moveHazardPolicyToken FallProhibited = "avoid_falls"

-- | Parse a scripting-facing policy token. 'Nothing' for anything else —
--   an unrecognized token must be REFUSED at the boundary rather than
--   silently defaulting to 'FallPermitted', which would turn a typo in an
--   ambient mover into exactly the cliff walk this policy exists to
--   prevent. Case- and whitespace-insensitive.
parseMoveHazardPolicy ∷ Text → Maybe MoveHazardPolicy
parseMoveHazardPolicy t = case T.toLower (T.strip t) of
    "allow_falls" → Just FallPermitted
    "avoid_falls" → Just FallProhibited
    _             → Nothing
