{-# LANGUAGE Strict #-}

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
--
--   BOUNDED (#2284). Routine interaction produces records — every key,
--   character, scroll and click — while nothing in @scripts\/@ drains,
--   so in ordinary play the only consumer is a harness or probe that
--   may never run. The ring therefore holds at most
--   'actionOutcomeCap' records and APPENDING TO A FULL RING DROPS THE
--   OLDEST, exactly as 'Engine.PlayerEvent.eventStoreCap' does for the
--   player-event store. 'pushActionOutcome' is the ONE append: every
--   producer, debug.recordOutcome included, goes through it, because a
--   second spelling of the append is a second place for the bound to
--   go missing. Initialization, the load-publish reset and the drain's
--   atomic swap-to-empty are not appends and mutate the ref directly.
module Engine.ActionOutcome
    ( ActionOutcome(..)
    , actionOutcomeCap
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
    , aoWhereX    ∷ !(Maybe Double)
      -- ^ Double, not Int: world-side producers always pass whole tile
      --   coordinates, but Layer A's screen-space clicks are frequently
      --   fractional (sub-pixel positions, and the playtest harness
      --   deliberately injects non-integral coordinates) — an Int field
      --   would silently drop the whole `where` on every such click.
    , aoWhereY    ∷ !(Maybe Double)
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

-- | Maximum records the action-outcome ring retains. Overflowing
--   appends drop the OLDEST records, so an undrained session keeps the
--   most recent 'actionOutcomeCap' outcomes and nothing older.
--
--   1000, matching 'Engine.PlayerEvent.eventStoreCap' — the ring this
--   one is modeled on. It sits far above what any drain consumer sees
--   between drains: the playtest harness drains once per snapshot and
--   @tools\/action_outcome_layer_a_check.py@ expects a single record
--   per drain, so no consumer can reach the bound in normal use.
actionOutcomeCap ∷ Int
actionOutcomeCap = 1000

-- | Append one outcome to the ring, dropping the oldest records when
--   that would exceed 'actionOutcomeCap'. THE single append: every
--   engine-side producer (World.Thread.Command.Cursor's designation
--   handlers, the input thread's key\/char\/scroll\/click taps,
--   World.Construct.Revalidate) and Lua's debug.recordOutcome go
--   through here, so the bound cannot be bypassed by re-spelling the
--   atomicModifyIORef'.
--
--   The drop is computed from the length AFTER the append rather than
--   before it, so a ref that somehow already exceeds the cap (a cap
--   lowered mid-process, say) is trimmed back to it instead of staying
--   over forever.
pushActionOutcome ∷ IORef (Seq.Seq ActionOutcome) → ActionOutcome → IO ()
pushActionOutcome ref ev =
    atomicModifyIORef' ref $ \buf →
        let appended = buf Seq.|> ev
            excess   = Seq.length appended - actionOutcomeCap
        in ( if excess > 0 then Seq.drop excess appended else appended
           , () )
