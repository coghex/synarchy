{-# LANGUAGE Strict #-}
-- | Per-page craft-bill ownership reconciliation (#1680).
--
--   Runs beside the world clock ('World.Thread.Time.tickWorldTime'),
--   once per LOADED page per world-thread iteration — deliberately in
--   the same "every page, independent of the pause flag" band as
--   'World.Thread.Discovery.tickLocationDiscovery' rather than the
--   visible-page, @dtGame > 0@ band the calendar/flora/power ticks
--   share, for two separate reasons:
--
--   * A stale bill on a HIDDEN page is exactly as wrong as one on the
--     visible page, and 'World.Thread.Power.tickPowerNetworks' — the
--     obvious-looking host — additionally short-circuits on a page with
--     no power nodes at all, which would leave the stale ownership in
--     place on any page with no grid.
--   * 'World.Load.Publish' brings a loaded session up PAUSED. A save
--     written while a bill's claimant was already missing restores that
--     bill verbatim (deliberately — see
--     'World.Save.Component.EntitySystems'), so the repair has to be able
--     to
--     land while the engine is still paused, or the player would see a
--     loaded station drawing power for a worker that does not exist
--     until they happened to unpause.
--
--   Like discovery, this never reads a game-scaled dt: reconciliation
--   is a check against whatever the unit thread has already published
--   this instant, not something that advances with simulated time.
module World.Thread.CraftBills
    ( tickCraftBillOwners
    ) where

import UPrelude
import qualified Data.HashMap.Strict as HM
import Data.IORef (readIORef, atomicModifyIORef')
import Craft.Bills
    (BillId(..), CraftBill(..), CraftBills(..), reconcileBillClaimants)
import Engine.Core.Capability.Core (CoreCapability(..), toCoreCapability)
import Engine.Core.Capability.UnitCombat
    (UnitCombatCapability(..), toUnitCombatCapability)
import Engine.Core.Log (logInfo, LogCategory(..))
import Engine.Core.State (EngineEnv)
import Unit.Types (UnitManager(..))
import World.Types (WorldPageId(..), WorldState(..))

-- | Clear the claimant (and, with it, 'Craft.Bills.cbWorking') of every
--   bill on @ws@ whose holder is no longer in the unit registry.
--
--   The liveness predicate is the SAME one the claim verb uses
--   ('Engine.Scripting.Lua.API.Craft.Bill.craftClaimBillFn':
--   @HM.member uid (umInstances um)@), so a unit that
--   'Craft.Bills.claimAvailable' would already treat as dead is exactly
--   the unit reconciled away here. The registry is engine-wide, not
--   page-scoped, so a bill claimed by a worker standing on another page
--   is correctly left alone.
--
--   Two cheap read-only guards keep this off the hot path, and both are
--   behaviour-preserving. A page whose bills are all unclaimed never
--   reads the unit registry at all; a page whose claimants are all
--   alive never touches 'wsCraftBillsRef', which matters because the
--   craft verbs mutate that same ref under their own
--   'atomicModifyIORef'' — an unconditional write-back every tick would
--   put a pointless CAS in the way of every claim and progress call for
--   the whole life of a normal, healthy bill.
--
--   Neither guard decides anything: they only decide whether to LOOK.
--   The reconciliation itself runs inside the atomic update against
--   whatever the CURRENT queue is, so a claim landing between the
--   snapshot and the write is judged on its own merits (a live claimant
--   that arrived in that window is not disowned, and a bill freshly
--   claimed by a unit that has since gone is still caught on the next
--   tick) — same reason 'World.Thread.Power.tickPowerNetworks' folds
--   inside its own update rather than snapshot-and-clobber.
tickCraftBillOwners ∷ EngineEnv → WorldPageId → WorldState → IO ()
tickCraftBillOwners env pageId ws = do
    bills0 ← readIORef (wsCraftBillsRef ws)
    let claimants = [ c | bill ← HM.elems (cbsBills bills0)
                        , Just c ← [cbClaimant bill] ]
    unless (null claimants) $ do
        um ← readIORef (ucUnitManagerRef (toUnitCombatCapability env))
        let alive uid = HM.member uid (umInstances um)
        when (any (not ∘ alive) claimants) $ do
            cleared ← atomicModifyIORef' (wsCraftBillsRef ws)
                          (reconcileBillClaimants alive)
            unless (null cleared) $ do
                logger ← readIORef (ccLoggerRef (toCoreCapability env))
                logInfo logger CatWorld $
                    "Craft bills on page " <> unWorldPageId pageId
                    <> " released by a claimant that no longer exists: "
                    <> tshow (map unBillId cleared)
