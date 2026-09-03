module World.Thread.Command.Basic
    ( handleWorldTickCommand
    , handleWorldSetCameraCommand
    , handleWorldDestroyCommand
    , handleWorldDestroyAllCommand
    ) where

import UPrelude
import Data.IORef (readIORef, writeIORef, atomicModifyIORef')
import Engine.Core.State (EngineEnv)
import Engine.Core.Capability.Building
    (BuildingCapability(..), toBuildingCapability)
import Engine.Core.Capability.RenderHandoff
    (RenderHandoffCapability(..), toRenderHandoffCapability)
import Engine.Core.Capability.UnitCombat
    (UnitCombatCapability(..), toUnitCombatCapability)
import Engine.Core.Capability.WorldSim
    (WorldSimCapability(..), toWorldSimCapability)
import Engine.Scene.Types (emptyLayeredQuads)
import Engine.Scene.Stats (clearSceneStats)
import qualified Engine.Core.Queue as Q
import Sim.Command.Types (SimCommand(..))
import Unit.Command.Types (UnitCommand(..))
import Building.Command.Types (BuildingCommand(..))
import Engine.Core.Log (logInfo, logDebug, LogCategory(..), LoggerState)
import World.Types
import World.Blood.Teardown (enqueueBloodDisposalForPage, enqueueBloodDisposalAll)

handleWorldTickCommand ∷ EngineEnv → LoggerState → Double → IO ()
handleWorldTickCommand _ _ _ = return ()

handleWorldSetCameraCommand ∷ EngineEnv → LoggerState → WorldPageId
    → Float → Float → IO ()
handleWorldSetCameraCommand env logger pageId x y = do
            mgr ← readIORef (wsWorldManagerRef (toWorldSimCapability env))
            case lookup pageId (wmWorlds mgr) of
                Just worldState →
                    atomicModifyIORef' (wsCameraRef worldState) $ \_ →
                        (WorldCamera x y, ())
                Nothing → 
                    logDebug logger CatWorld $ 
                        "World not found for camera update: " <> unWorldPageId pageId

handleWorldDestroyCommand ∷ EngineEnv → LoggerState → WorldPageId → IO ()
handleWorldDestroyCommand env logger pageId = do
    let worldSim = toWorldSimCapability env
        handoff  = toRenderHandoffCapability env
    logInfo logger CatWorld $ "Destroying world: " <> unWorldPageId pageId

    -- Tear down this world's simulation state too — destroy used to drop
    -- the page from wmWorlds/wmVisible while leaving its sim chunks behind
    -- forever (#61). SimDropWorld discards them (unlike hide, which keeps
    -- them for a later re-show); only this world's sim is touched.
    Q.writeQueue (wsSimQueue worldSim) (SimDropWorld pageId)

    -- Reclaim this page's blood-texture GPU resources (#788): hand its
    -- live handle map to the render thread BEFORE the page drops out of
    -- wmWorlds and becomes unreachable to uploadBloodTextures.
    mgr ← readIORef (wsWorldManagerRef worldSim)
    enqueueBloodDisposalForPage (rhBloodDisposeQueue handoff) mgr pageId

    -- Remove from visible list
    atomicModifyIORef' (wsWorldManagerRef worldSim) $ \mgr'' →
        -- #1602: destroying the visible HEAD changes what
        -- resolveActiveWorld answers with, so it invalidates live
        -- placement bindings. Destroying a hidden, absent, or
        -- visible-but-not-head page does not: a binding only ever names
        -- the head, so nothing it depends on moved. The pending request
        -- is discharged either way.
        let mgr' = completeSelectionChange mgr''
            wasHead = selectionHead (wmVisible mgr') ≡ Just pageId
        in ((if wasHead then bumpSelectionGen else id)
            (mgr' { wmVisible = filter (≢ pageId) (wmVisible mgr')
                  , wmWorlds  = filter ((≢ pageId) . fst) (wmWorlds mgr')
                  }), ())

    -- Clear world quads so renderer stops drawing the old world, and
    -- the scene-assembly telemetry measured while building them (#1921)
    -- in the same breath — the two describe one lifecycle, so a query
    -- after this teardown must not answer with the destroyed world's
    -- numbers. The next completed pass republishes at sequence 1.
    writeIORef (rhWorldQuadsRef handoff) emptyLayeredQuads
    clearSceneStats (rhSceneStatsRef handoff)

    logInfo logger CatWorld $ "World destroyed: " <> unWorldPageId pageId

-- | Tear down EVERY world (Exit to Menu). Destroying only the "current"
--   world left hidden ones (e.g. a leftover test arena) in wmWorlds, and
--   resolveActiveWorld's head-fallback then kept resolving one as the
--   implicit active world behind the menu (#58). Clearing wmWorlds makes
--   the resolver return Nothing (menu state). Also sim-deactivates each
--   page and resets the global entity managers so no units/buildings from
--   the old session linger as orphans into the next game.
handleWorldDestroyAllCommand ∷ EngineEnv → LoggerState → IO ()
handleWorldDestroyAllCommand env logger = do
    let worldSim = toWorldSimCapability env
        handoff  = toRenderHandoffCapability env
    logInfo logger CatWorld "Destroying all worlds (Exit to Menu)"
    mgr ← readIORef (wsWorldManagerRef worldSim)
    -- Drop (not just deactivate) each world's sim state — every world is
    -- being destroyed, so its chunks are gone for good (#58/#61).
    forM_ (map fst (wmWorlds mgr)) $ \pid →
        Q.writeQueue (wsSimQueue worldSim) (SimDropWorld pid)
    -- Reclaim every page's blood-texture GPU resources (#788) before
    -- wmWorlds is cleared out from under uploadBloodTextures.
    enqueueBloodDisposalAll (rhBloodDisposeQueue handoff) mgr
    atomicModifyIORef' (wsWorldManagerRef worldSim) $ \m →
        -- #1602: every page is gone, so no binding captured before this
        -- may validate or commit afterwards — unless nothing was visible
        -- to begin with, in which case no binding existed to invalidate.
        let m' = completeSelectionChange m
        in ((if isJust (selectionHead (wmVisible m'))
               then bumpSelectionGen else id)
            m' { wmWorlds = [], wmVisible = [] }, ())
    writeIORef (rhWorldQuadsRef handoff) emptyLayeredQuads
    clearSceneStats (rhSceneStatsRef handoff)
    -- Reset the entity managers via the UNIT/BUILDING queues, not directly:
    -- those threads keep draining their queues through the teardown, so
    -- clearing the managers here would race any in-flight spawns and let
    -- them re-insert orphans afterwards. Enqueuing the clears makes them
    -- run in order, AFTER every pending spawn (#58). The wmWorlds clear
    -- above also makes the spawn handlers drop late spawns outright.
    --
    -- Each clear is followed by its session-boundary MARKER (#2291), and
    -- the BUILDING pair is enqueued first. The unit thread drains the
    -- unit queue and then the building queue inside one tick (buildings
    -- have no thread of their own) and stops each drain at its marker,
    -- then resets the session's game clock and event ring
    -- ('Unit.Thread.endSessionEpoch', which carries the argument in
    -- full). Enqueueing the building pair first is what makes that reset
    -- provably later than BOTH clears: reaching @UnitEndSession@ in a
    -- tick's unit drain means all four messages were queued before that
    -- tick's building drain, so FIFO order had already run
    -- @BuildingClearAll@ by then. The opposite enqueue order would let a
    -- tick reset the clock with the building clear still queued, leaving
    -- destruction effects stamped on the old epoch to be measured
    -- against the new one.
    Q.writeQueue (bcBuildingQueue (toBuildingCapability env)) BuildingClearAll
    Q.writeQueue (bcBuildingQueue (toBuildingCapability env)) BuildingEndSession
    Q.writeQueue (ucUnitQueue (toUnitCombatCapability env)) UnitClearAll
    Q.writeQueue (ucUnitQueue (toUnitCombatCapability env)) UnitEndSession
    logInfo logger CatWorld "All worlds destroyed"
