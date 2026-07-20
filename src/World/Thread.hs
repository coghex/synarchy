{-# OPTIONS_GHC -fprof-auto #-}
{-# LANGUAGE Strict, UnicodeSyntax #-}
module World.Thread
    ( startWorldThread
    , partitionAuthorized
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
import Engine.Core.Log (logInfo, logDebug, logError, logWarn, LogCategory(..), LoggerState)
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
                        -- Round 7 review: acknowledging BEFORE the rest
                        -- of this tick's work (drainInitQueues/
                        -- tickWorldTime/updateChunkLoading/pollCursorInfo,
                        -- all of which can queue fresh Lua/HUD/sim
                        -- messages) let this ack be the FINAL one a
                        -- quiescence pass needed while this tick was
                        -- still mid-flight producing more side effects —
                        -- if the barrier then reached SaveSnapshotBoundary
                        -- before this tick finished, that later work
                        -- could straddle the publish boundary and land
                        -- against the replacement session. Folded into
                        -- this branch (all already unconditional on
                        -- "not locked", since this whole branch only
                        -- runs when locked is False) so the ack fires
                        -- only once every side-effect-producing step
                        -- below has actually completed.
                        drainInitQueues env logger
                        tickWorldTime env (realToFrac dt)
                        updateChunkLoading env logger
                        pollCursorInfo env
                        acknowledgeCurrent (saveBarrierRef env) SaveWorld

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

-- | The capture lock admits only its queued WorldSave / WorldLoadPublish
-- command. A load's WorldLoadPublish reaches this window only after every
-- other state owner has already quiesced against the SAME save-barrier
-- protocol a save uses (issue #763, save-overhaul C2 — see
-- "Engine.Scripting.Lua.Thread.Dispatch"), so the two authorized command
-- kinds never contend with each other.
--
-- What happens to every OTHER command still sitting in the queue differs
-- by kind (requirement 12: isolate old asynchronous work). A save doesn't
-- replace anything -- the live session stays the SAME session before and
-- after -- so its non-authorized commands are simply deferred, ordered
-- exactly as before, for the world owner's next unlocked tick
-- (unchanged from pre-#763 behaviour). A load publish REPLACES THE
-- COMPLETE SESSION: anything else queued at this exact moment was queued
-- against the OLD session (a debug-console/Lua call that landed just
-- before this window closed, a sim writeback that raced the barrier,
-- ...) and, if merely deferred, would run again on the world owner's
-- very next tick -- AFTER the swap -- against the NEW session instead,
-- silently corrupting it (e.g. a stale WorldSetTime for a page id the
-- new session also happens to use). Since no queued WorldCommand
-- represents durable intent (everything durable already lives in the
-- staged snapshot this transaction is about to publish), it is safe,
-- and required, to discard it outright rather than let it survive into
-- the replacement.
processAuthorizedSave ∷ EngineEnv → LoggerState → IO ()
processAuthorizedSave env logger = do
    commands ← Q.flushQueue (worldQueue env)
    let (authorized, deferred) = partitionAuthorized commands
        discarded = length commands - length authorized - length deferred
    when (discarded > 0) $
        logWarn logger CatWorld $
            "Load publish discarded " <> T.pack (show discarded)
            <> " stale WorldCommand(s) queued before the whole-session replacement"
    forM_ authorized $ handleWorldCommand env logger
    forM_ deferred $ Q.writeQueue (worldQueue env)

-- | Pure split of one captureLocked-window batch into (authorized to run
-- now, preserved for after release) -- see 'processAuthorizedSave' for
-- why a load publish discards the rest instead of preserving it. Exposed
-- for direct hspec coverage of the discard decision without needing a
-- real queue/engine.
partitionAuthorized ∷ [WorldCommand] → ([WorldCommand], [WorldCommand])
partitionAuthorized commands
    | any isLoadPublish authorized = (authorized, [])
    | otherwise                    = (authorized, deferred)
  where
    (authorized, deferred) = partition isAuthorized commands
    isAuthorized (WorldSave _ _ _ _ _) = True
    isAuthorized (WorldLoadPublish _)  = True
    isAuthorized _                     = False
    isLoadPublish (WorldLoadPublish _) = True
    isLoadPublish _                    = False
