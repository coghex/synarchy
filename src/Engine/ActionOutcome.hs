{-# LANGUAGE Strict, UnicodeSyntax #-}

-- | The F4 (#646) action-outcome oracle tap: what actually happened to a
--   player-initiated action, independent of whether anything user-facing
--   fired. Modeled on Combat.Types' drainable-ring pattern
--   (combatEventsRef / injuryEventsRef), but a dedicated shape rather than
--   a reuse of CombatEvent — the outcome schema (kind/outcome/where/
--   target/requested/applied/dropped/reason/handler) doesn't fit
--   CombatEvent's attacker/target/flat-payload shape.
--
--   Producers: Lua's debug.recordOutcome (Layer A input routing, Layer B
--   Lua-owned commit boundaries) and engine-side designation handlers
--   (World.Thread.Command.Cursor) for the partial-drop counts that only
--   the world thread can compute. Drained by the playtest harness's
--   critic via debug.drainActionOutcomes; never surfaced to the player.
--   Runtime only, not persisted to SaveData.
module Engine.ActionOutcome
    ( ActionOutcome(..)
    , emptyActionOutcomeQueue
    , pushActionOutcome
    ) where

import UPrelude
import qualified Data.Sequence as Seq
import Data.IORef (IORef, atomicModifyIORef')

-- | One record of the outcome ring. All fields beyond ts/kind/outcome are
--   optional — which ones are populated depends on the outcome kind (a
--   `partial` carries requested/applied/dropped; a `rejected` carries
--   reason; Layer A's widget hits carry handler; a tile-targeted action
--   carries whereX/whereY, a unit-targeted one carries target).
data ActionOutcome = ActionOutcome
    { aoTs        ∷ !Double
      -- ^ game-clock seconds when the outcome was recorded
    , aoKind      ∷ !Text
      -- ^ which action/verb, e.g. "till.designate", "input.click"
    , aoOutcome   ∷ !Text
      -- ^ "accepted" | "rejected" | "partial" | "noop" | "deadclick"
    , aoWhereX    ∷ !(Maybe Int)
    , aoWhereY    ∷ !(Maybe Int)
    , aoTarget    ∷ !(Maybe Word32)
    , aoRequested ∷ !(Maybe Int)
      -- ^ for "partial": tiles/units the sweep covered before filtering
    , aoApplied   ∷ !(Maybe Int)
      -- ^ for "partial": how many actually landed
    , aoDropped   ∷ !(Maybe Int)
      -- ^ for "partial": requested - applied
    , aoReason    ∷ !(Maybe Text)
      -- ^ the INTERNAL reject/drop reason — need not be anything the
      --   player saw; that gap is the whole point of this oracle
    , aoHandler   ∷ !(Maybe Text)
      -- ^ Layer A: which widget/tool/world-action consumed the input
    } deriving (Show)

-- | Initial ring state for 'Engine.Core.Init'.
emptyActionOutcomeQueue ∷ Seq.Seq ActionOutcome
emptyActionOutcomeQueue = Seq.empty

-- | Append one outcome to the ring. Shared by every engine-side producer
--   (World.Thread.Command.Cursor's designation handlers, Engine.Scripting
--   .Lua.Thread.Dispatch's UI-click case) so they don't each re-spell the
--   atomicModifyIORef'; Lua's debug.recordOutcome uses this too.
pushActionOutcome ∷ IORef (Seq.Seq ActionOutcome) → ActionOutcome → IO ()
pushActionOutcome ref ev =
    atomicModifyIORef' ref $ \buf → (buf Seq.|> ev, ())
