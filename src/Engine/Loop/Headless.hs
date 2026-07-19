module Engine.Loop.Headless
  ( headlessLoop
  ) where

import UPrelude
import Control.Concurrent (threadDelay)
import Data.IORef (readIORef, writeIORef)
import qualified Engine.Core.Queue as Q
import Engine.Core.Monad
import Engine.Core.State
import Engine.Core.Log (LogCategory(..))
import Engine.Core.Log.Monad (logDebugM, logInfoM)
import Engine.Save.Barrier (SaveOwner(..), acknowledgeCurrent, captureLocked)
import Engine.Scripting.Lua.Message (processLuaMessages)

-- | Headless main loop: processes messages without rendering. Lua
--   messages are dispatched through the SAME 'processLuaMessages' as the
--   graphical loop — GPU operations no-op themselves when headless (via
--   'whenGraphical'), so there is no separate headless dispatcher to
--   drift out of sync with the graphical one.
--
--   Round 15 rereview (issue #763): this loop is exactly the
--   'luaToEngineQueue' consumer 'Engine.Loop.runGatedByCaptureLock'\'s
--   own haddock already describes for the windowed/offscreen case — a
--   headless boot ('App.Headless') runs THIS loop, not 'mainLoop'/
--   'mainLoopOffscreen', but it drains the SAME queue via the SAME
--   'processLuaMessages', so it needs the SAME 'SaveRender' barrier
--   participation for a headless load's publish to be safe: gate the
--   drain on 'captureLocked', then unconditionally acknowledge —
--   'acknowledgeCurrent' is a no-op whenever the active transaction's
--   owner set doesn't include 'SaveRender' (a plain save), so this is
--   safe every tick regardless of transaction type. Unlike the
--   windowed/offscreen loops, there is no camera work to gate here —
--   headless never ran 'updateCameraPanning'/etc. even before #763.
headlessLoop ∷ EngineM ε σ ()
headlessLoop = do
    env ← ask
    lifecycle ← liftIO $ readIORef (lifecycleRef env)
    case lifecycle of
        EngineStarting → do
            logDebugM CatSystem "Headless engine starting..."
            liftIO $ threadDelay 100000
            _ ← liftIO $ Q.flushQueue (inputQueue env)
            liftIO $ writeIORef (lifecycleRef env) EngineRunning
            headlessLoop
        EngineRunning → do
            locked ← liftIO $ captureLocked (saveBarrierRef env)
            unless locked processLuaMessages
            liftIO $ acknowledgeCurrent (saveBarrierRef env) SaveRender
            lifecycle' ← liftIO $ readIORef (lifecycleRef env)
            if lifecycle' ≡ EngineRunning
                then do
                    liftIO $ threadDelay 16666
                    headlessLoop
                else do
                    logInfoM CatSystem "Headless engine shutting down..."
                    liftIO $ writeIORef (lifecycleRef env) CleaningUp
        CleaningUp → logDebugM CatSystem "Headless engine cleaning up"
        EngineStopped → logDebugM CatSystem "Headless engine stopped"
