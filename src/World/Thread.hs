{-# OPTIONS_GHC -fprof-auto #-}
{-# LANGUAGE Strict, UnicodeSyntax #-}
module World.Thread
    ( startWorldThread
    ) where

import UPrelude
import qualified Data.Text as T
import Data.IORef (IORef, readIORef, writeIORef, newIORef)
import Control.Concurrent (forkIO, threadDelay)
import Control.Exception (SomeException, catch)
import Data.Time.Clock.POSIX (getPOSIXTime)
import Engine.Core.Thread (ThreadState(..), ThreadControl(..))
import Engine.Core.State (EngineEnv(..))
import Engine.Core.Log (logInfo, logDebug, logError, LogCategory(..), LoggerState)
import Engine.Graphics.Camera (Camera2D(..))
import qualified Engine.Core.Queue as Q
import World.Types
import World.Render (updateWorldTiles)
import World.Thread.Cursor (pollCursorInfo)
import World.Thread.Time (tickWorldTime)
import World.Thread.ChunkLoading (updateChunkLoading, drainInitQueues)
import World.Thread.Command (handleWorldCommand)

-----------------------------------------------------------
-- Start World Thread
-----------------------------------------------------------

startWorldThread ∷ EngineEnv → IO ThreadState
startWorldThread env = do
    logger ← readIORef (loggerRef env)
    stateRef ← newIORef ThreadRunning
    threadId ← catch
        (do
            logInfo logger CatWorld "Starting world thread..."
            lastTimeRef ← getPOSIXTime ⌦ newIORef . realToFrac
            tid ← forkIO $ worldLoop env stateRef lastTimeRef
            logInfo logger CatWorld "World thread started"
            return tid
        )
        (\(e ∷ SomeException) → do
            logError logger CatWorld $ "Failed starting world thread: " <> T.pack (show e)
            error "World thread start failure."
        )
    return $ ThreadState stateRef threadId

-----------------------------------------------------------
-- World Loop
-----------------------------------------------------------

worldLoop ∷ EngineEnv → IORef ThreadControl → IORef Double → IO ()
worldLoop env stateRef lastTimeRef = do
    control ← readIORef stateRef
    logger ← readIORef (loggerRef env)
    case control of
        ThreadStopped → do
            logDebug logger CatWorld "World thread stopping..."
            pure ()
        ThreadPaused → do
            threadDelay 100000
            worldLoop env stateRef lastTimeRef
        ThreadRunning → do
            now ← realToFrac ⊚ getPOSIXTime
            lastTime ← readIORef lastTimeRef
            let dt = now - lastTime ∷ Double
            writeIORef lastTimeRef now

            processAllCommands env logger

            -- Drain initial chunk queues (progressive loading)
            drainInitQueues env logger

            tickWorldTime env (realToFrac dt)
            updateChunkLoading env logger
            pollCursorInfo env

            camera ← readIORef (cameraRef env)
            allQuads ← updateWorldTiles env
            writeIORef (worldQuadsRef env) allQuads
            threadDelay 16666
            worldLoop env stateRef lastTimeRef

-- | Drain all pending commands from the queue
processAllCommands ∷ EngineEnv → LoggerState → IO ()
processAllCommands env logger = do
    mCmd ← Q.tryReadQueue (worldQueue env)
    case mCmd of
        Just cmd → do
            handleWorldCommand env logger cmd
            processAllCommands env logger
        Nothing → return ()
