{-# LANGUAGE Strict #-}
-- | The world-thread half of #1602's build-placement page binding.
--
--   A placement the player clicked is bound to the page that click was
--   hit-tested against, and that binding has to be discharged somewhere
--   a concurrent page-selection change cannot slip past. This thread is
--   the only such place: it is the sole mutator of 'wmVisible' and
--   therefore of 'wmSelectionGen' — @world.show@ / @world.hide@,
--   @world.initArena@'s auto-show, @world.destroy@ / @world.destroyAll@
--   and the transactional load's @publishStagedSession@ all run here,
--   drained from this one queue in order.
--
--   So a selection change enqueued BEFORE a bound placement has already
--   been applied when the check below runs, and one enqueued AFTER is
--   genuinely after the decision. A check on the Lua thread (where
--   @building.spawn@ answers its caller) or on the building-command
--   drain the unit thread runs could only ever be best-effort against a
--   counter this thread is free to move underneath it.
module World.Thread.Command.BoundSpawn
    ( handleWorldSpawnBoundBuildingCommand
    ) where

import UPrelude
import Data.IORef (atomicModifyIORef', readIORef)
import Building.Thread.Command (applyBuildingSpawn)
import Building.Reservation (releaseReservation)
import Building.Types (BuildingId(..))
import Engine.Core.Capability.Building
    (BuildingCapability(..), toBuildingCapability)
import Engine.Core.Capability.ContentRegistriesView
    (toContentRegistriesViewCapability)
import Engine.Core.Capability.WorldSim
    (WorldSimCapability(..), toWorldSimCapability)
import Engine.Core.Log (logDebug, LogCategory(..), LoggerState)
import Engine.Core.State (EngineEnv)
import World.Types

-- | Discharge a bound placement's page binding and, if it still holds,
--   INSERT the building here and now.
--
--   The insertion is performed on this thread rather than forwarded to
--   the building queue, and that is the whole point: a check on one
--   thread guarding a write another thread performs later is not a
--   guard at all — a @world.hide@ landing in between would leave the
--   drain inserting onto a page that is no longer selected (the page is
--   still in @wmWorlds@, so its own world-gone check would not catch
--   it). Deciding and writing in the same step, on the thread that owns
--   selection, is what makes "no page change between validation and
--   commit" true rather than merely likely.
--
--   The write itself is 'Building.Thread.Command.applyBuildingSpawn' —
--   the SAME body the drain runs for every unbound spawn, so a bound
--   placement and an ordinary one can never diverge in what they
--   actually do (including the #58 world-gone guard and #1087's
--   pending-container seeding).
--
--   A stale binding writes nothing, so the placement lands on neither
--   the captured page nor the newly selected one — and it releases the
--   footprint reservation @building.spawn@ took for it (#2326), because
--   a request that will never commit must not keep tiles claimed against
--   the next placement the player makes.
handleWorldSpawnBoundBuildingCommand
    ∷ EngineEnv → LoggerState → BuildingId → Text
    → Int → Int → Int → WorldPageId → Word64 → IO ()
handleWorldSpawnBoundBuildingCommand env logger bid defName gx gy gz
                                     pageId bindGen = do
    mgr ← readIORef (wsWorldManagerRef (toWorldSimCapability env))
    if wmSelectionGen mgr ≢ bindGen
    then do
        -- #2326: the claim @building.spawn@ took for this placement is
        -- retired here, through the capability accessor, so the write
        -- stays attributable to this module.
        atomicModifyIORef'
            (bcBuildingManagerRef (toBuildingCapability env)) $ \bm →
                (releaseReservation bid bm, ())
        logDebug logger CatWorld $
            "Bound building placement dropped: page selection moved since "
            <> "the click (" <> defName <> " on " <> unWorldPageId pageId
            <> ")"
    else applyBuildingSpawn logger
            (toWorldSimCapability env)
            (toContentRegistriesViewCapability env)
            (toBuildingCapability env)
            bid defName gx gy gz pageId
