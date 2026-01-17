module Engine.Lua.Init where

import UPrelude
import qualified Data.Map as Map
import qualified Data.Text as T
import qualified Graphics.UI.GLFW as GLFW
import Control.Concurrent (threadDelay, ThreadId, killThread, forkIO)
import Control.Exception (SomeException, catch)
import Data.IORef (IORef, newIORef, writeIORef, readIORef)
import Data.Time.Clock (getCurrentTime, diffUTCTime)
import Engine.Core.State
import Engine.Core.Thread
import Engine.Lua.Types
import qualified Engine.Core.Queue as Q
import qualified HsLua as Lua

-- | Start the lua thread
startLuaThread ∷ EngineEnv → IO ThreadState
startLuaThread env = do
    stateRef ← newIORef ThreadRunning
    threadId ← catch 
        (do
            Q.writeQueue (logQueue env) "Starting lua thread..."
            let leq = luaEventQueue env
                lcq = luaCommandQueue env
            defaultls ← defaultLuaState leq lcq
            tid ← forkIO $ runLuaLoop env defaultls stateRef
            return tid
        ) 
        (\(e :: SomeException) → do
            Q.writeQueue (logQueue env) $ T.pack $
                "Failed to start lua thread: " ⧺ show e
            error "Lua thread failed to start"
        )
    return $ ThreadState stateRef threadId

runLuaLoop ∷ EngineEnv → LuaState → IORef ThreadControl → IO ()
runLuaLoop env ls stateRef = do
    control ← readIORef stateRef
    case control of
        ThreadStopped → do
            Lua.close (luaState ls)
            pure ()
        ThreadPaused → do
            threadDelay 100000 -- Sleep for 100ms while paused
            runLuaLoop env ls stateRef
        ThreadRunning → do
            frameStart ← getCurrentTime
            -- Process events
            --newLuaSt ← processLuaEvents env luaState
            let newLuaSt = ls -- Placeholder for actual event processing
            -- Calculate frame time and delay to maintain consistent rate
            frameEnd ← getCurrentTime
            let diff  = diffUTCTime frameEnd frameStart
                usecs = floor (toRational diff * 1000000) ∷ Int
                targetFrameTime = 1000  -- 1ms target frame time
                delay = targetFrameTime - usecs
            
            -- Only delay if we're running faster than target
            when (delay > 0) $ threadDelay delay
            
            -- Continue loop
            runLuaLoop env newLuaSt stateRef
