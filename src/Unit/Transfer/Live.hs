{-# LANGUAGE Strict #-}
-- | The IO glue that drives "Unit.Transfer.Orders"' pure store
--   transitions from live engine state (#1253, epic #1013 slice
--   UIT-5A). Kept OUT of "Unit.Transfer.Orders" so that module stays
--   pure and free of "World.State.Types" in its import graph — the same
--   split "Power.Live" makes for power nodes and
--   "Building.Knowledge.Live" for container knowledge, which is the
--   pattern this follows.
--
--   __An order's lifetime is bounded by its carrier's.__ The ordinary
--   way an order leaves the store is the executor pruning it at its
--   terminal transition (@scripts/unit_ai_transfer.lua@), one tick after
--   the outcome was surfaced. That covers every way an order can FINISH.
--   It cannot cover the carrier ceasing to be able to act, because there
--   is then nobody left to tick it — and this is that other half. TWO
--   callers, both in the unit thread's own command handlers:
--   'Unit.Thread.Command.Lifecycle.handleUnitDestroyCommand' (the
--   instance is gone) and 'Unit.Thread.Command.Pose.handleUnitKillCommand'
--   (the instance remains, but @scripts/unit_ai.lua@ short-circuits a
--   @dead@ pose before any action scores). The recoverable poses —
--   collapsed, crawling — are deliberately NOT here: their orders are
--   merely suspended and the unit gets back up to finish the haul.
module Unit.Transfer.Live
    ( retireTransferOrdersEverywhere
    ) where

import UPrelude
import Data.IORef (IORef, readIORef, atomicModifyIORef')
import Unit.Types.Manager (UnitId(..))
import Unit.Transfer.Orders (removeOrdersForUnit)
import World.State.Types (WorldManager(..), WorldState(..))

-- | The acting unit died or was destroyed: drop every order it was
--   carrying, on every live page. Idempotent — a second call, or one for
--   a unit carrying nothing, rewrites each page's store to itself.
--
--   Applied to EVERY page rather than the unit's own, for the same two
--   reasons 'Power.Live.retirePowerNodeEverywhere' is: the destroy
--   caller has already removed the instance by the time this runs, so
--   there is no @uiPage@ left to resolve — and a 'UnitId' comes from one
--   session-global allocator, so it can name a unit on at most one page
--   anyway. Page-correct by
--   construction, with no dependence on read\/delete ordering, and
--   orders on every other page are untouched because none of them names
--   this carrier.
--
--   Takes the world-manager ref alone rather than a capability record:
--   retiring an order needs no clock, no items and no buildings.
--
--   Deliberately SILENT. Every other retirement surfaces its outcome
--   first because a player who queued a haul is owed the reason it did
--   not happen; this one has no unit left to attribute an event to
--   (@engine.emitEventForUnit@ files a line in that unit's own Log tab,
--   and the unit is gone), and the death itself is already reported by
--   whatever killed it. What matters here is that the order does not
--   outlive its carrier.
retireTransferOrdersEverywhere ∷ IORef WorldManager → UnitId → IO ()
retireTransferOrdersEverywhere worldsRef uid = do
    wm ← readIORef worldsRef
    forM_ (wmWorlds wm) $ \(_, ws) →
        atomicModifyIORef' (wsTransferOrdersRef ws) $ \orders →
            (fst (removeOrdersForUnit uid orders), ())
