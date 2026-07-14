{-# OPTIONS_GHC -fprof-auto #-}
{-# LANGUAGE Strict, UnicodeSyntax #-}
module World.Thread
    ( startWorldThread
    ) where

import UPrelude
import qualified Data.Text as T
import Data.IORef (IORef, readIORef, writeIORef, newIORef)
import Control.Concurrent (forkIO, threadDelay)
import Control.Exception (SomeException, catch, finally)
import Control.Concurrent.MVar (newEmptyMVar, putMVar)
import Data.Time.Clock.POSIX (getPOSIXTime)
import Data.List (partition)
import Engine.Core.Thread (ThreadState(..), ThreadControl(..))
import Engine.Core.State (EngineEnv(..), EngineLifecycle(..))
import Engine.Core.Log (logInfo, logDebug, logError, LogCategory(..), LoggerState)
import qualified Engine.Core.Queue as Q
import World.Render (updateWorldTiles)
import World.Thread.Cursor (pollCursorInfo)
import World.Thread.Time (tickWorldTime)
import World.Thread.ChunkLoading (updateChunkLoading, drainInitQueues)
import World.Thread.Command (handleWorldCommand)
import World.Command.Types (WorldCommand(..))
import Engine.Save.Barrier (SaveOwner(..), acknowledgeCurrent, captureLocked)

-- * Start World Thread

startWorldThread ∷ EngineEnv → IO ThreadState
startWorldThread env = do
    logger ← readIORef (loggerRef env)
    stateRef ← newIORef ThreadRunning
    doneVar ← newEmptyMVar
    threadId ← catch
        (do
            logInfo logger CatWorld "Starting world thread..."
            lastTimeRef ← getPOSIXTime ⌦ newIORef . realToFrac
            tid ← forkIO $ worldLoop env stateRef lastTimeRef `finally` putMVar doneVar ()
            logInfo logger CatWorld "World thread started"
            return tid
        )
        (\(e ∷ SomeException) → do
            logError logger CatWorld $ "Failed starting world thread: " <> T.pack (show e)
            error "World thread start failure."
        )
    return $ ThreadState stateRef threadId doneVar

-- * World Loop

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
            -- One guarded tick per iteration; the recursive call lives
            -- OUTSIDE the catch — inside it, each tick pushes a catch
            -- frame that never pops (unbounded stack growth).
            ok ← catch
              (do
                now ← realToFrac ⊚ getPOSIXTime
                lastTime ← readIORef lastTimeRef
                let dt = now - lastTime ∷ Double
                writeIORef lastTimeRef now

                locked ← captureLocked (saveBarrierRef env)
                if locked
                    then processAuthorizedSave env logger
                    else do
                        processAllCommands env logger
                        acknowledgeCurrent (saveBarrierRef env) SaveWorld

                -- Drain initial chunk queues (progressive loading)
                unless locked $ drainInitQueues env logger

                unless locked $ do
                    tickWorldTime env (realToFrac dt)
                    updateChunkLoading env logger
                    pollCursorInfo env

                _camera ← readIORef (cameraRef env)
                allQuads ← updateWorldTiles env
                -- Plain writeIORef is fine here: the value is an immutable
                -- LayeredQuads built entirely before the write, so the
                -- reader (Frame.hs) always sees either the old or the new
                -- value, never a torn pointer — at worst it draws one
                -- frame against the previous quads.
                writeIORef (worldQuadsRef env) allQuads
                threadDelay 16666
                pure True
              )
              (\(e ∷ SomeException) → do
                logError logger CatWorld $ "World thread crashed: " <> T.pack (show e)
                writeIORef (lifecycleRef env) CleaningUp
                pure False
              )
            when ok $ worldLoop env stateRef lastTimeRef

-- | Drain all pending commands from the queue
processAllCommands ∷ EngineEnv → LoggerState → IO ()
processAllCommands env logger = do
    mCmd ← Q.tryReadQueue (worldQueue env)
    case mCmd of
        Just cmd → do
            handleWorldCommand env logger cmd
            processAllCommands env logger
        Nothing → return ()

-- | The capture lock admits only its queued WorldSave command. All other
-- commands remain ordered for later processing, so they cannot leak into the
-- snapshot while still allowing the world owner to write it.
processAuthorizedSave ∷ EngineEnv → LoggerState → IO ()
processAuthorizedSave env logger = do
    commands ← Q.flushQueue (worldQueue env)
    let (saves, deferred) = partition isSave commands
    forM_ saves $ handleWorldCommand env logger
    forM_ deferred $ Q.writeQueue (worldQueue env)
  where
    isSave (WorldSave _ _ _ _) = True
    isSave _                   = False
