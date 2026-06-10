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
import Engine.Scripting.Lua.Message (processLuaMessages)

-- | Headless main loop: processes messages without rendering. Lua
--   messages are dispatched through the SAME 'processLuaMessages' as the
--   graphical loop — GPU operations no-op themselves when headless (via
--   'whenGraphical'), so there is no separate headless dispatcher to
--   drift out of sync with the graphical one.
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
            processLuaMessages
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
