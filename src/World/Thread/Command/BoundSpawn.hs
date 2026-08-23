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
import Data.IORef (readIORef)
import Building.Command.Types (BuildingCommand(..))
import Building.Types (BuildingId(..))
import Engine.Core.Capability.Building
    (BuildingCapability(..), toBuildingCapability)
import Engine.Core.Capability.WorldSim
    (WorldSimCapability(..), toWorldSimCapability)
import Engine.Core.Log (logDebug, LogCategory(..), LoggerState)
import Engine.Core.State (EngineEnv)
import qualified Engine.Core.Queue as Q
import World.Types

-- | Discharge a bound placement's page binding and, if it still holds,
--   forward the ordinary 'BuildingSpawn' the building drain already
--   knows how to apply.
--
--   The forwarded command carries no binding of its own: it has one
--   decision behind it, made against the selection timeline, and
--   re-checking it later would DROP placements that were committed
--   correctly and then had their page hidden — the opposite failure.
--
--   A stale binding forwards nothing, so the placement lands on neither
--   the captured page nor the newly selected one. The existing
--   world-gone guard in the building drain still applies to what is
--   forwarded; this adds to it rather than replacing it.
handleWorldSpawnBoundBuildingCommand
    ∷ EngineEnv → LoggerState → BuildingId → Text
    → Int → Int → Int → WorldPageId → Word64 → IO ()
handleWorldSpawnBoundBuildingCommand env logger bid defName gx gy gz
                                     pageId bindGen = do
    mgr ← readIORef (wsWorldManagerRef (toWorldSimCapability env))
    if wmSelectionGen mgr ≢ bindGen
    then logDebug logger CatWorld $
        "Bound building placement dropped: page selection moved since the "
        <> "click (" <> defName <> " on " <> unWorldPageId pageId <> ")"
    else Q.writeQueue (bcBuildingQueue (toBuildingCapability env)) $
        BuildingSpawn bid defName gx gy gz pageId
