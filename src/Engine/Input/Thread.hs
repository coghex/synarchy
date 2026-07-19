{-# LANGUAGE Strict, UnicodeSyntax #-}
-- | Input thread: owns the OS thread lifecycle and the per-tick
--   drain/sleep loop. #787 split the event-processing/dispatch logic
--   this facade used to own inline out into sibling modules so this
--   file stays a thin thread-loop entrypoint:
--
--     * Queue draining + top-level per-event routing lives in
--       'Engine.Input.Thread.Dispatch' (re-exported here).
--     * Per-domain dispatch lives in 'Engine.Input.Thread.Keyboard',
--       'Engine.Input.Thread.Char', 'Engine.Input.Thread.Mouse', and
--       'Engine.Input.Thread.Scroll'.
--
--   Both are re-exported here so the public API is unchanged.
module Engine.Input.Thread
  ( startInputThread
  , runInputLoop
  , processInputs
  , processInput
  ) where

import UPrelude
import qualified Data.Text as T
import Control.Concurrent (threadDelay, forkIO)
import Control.Exception (SomeException, catch, finally)
import Control.Concurrent.MVar (newEmptyMVar, putMVar)
import Data.IORef (IORef, newIORef, writeIORef, readIORef)
import Engine.Core.Log (logDebug, logError, logInfo, LogCategory(..))
import Engine.Core.State
import Engine.Core.Thread
import Engine.Save.Barrier (SaveOwner(..), acknowledgeCurrent, captureLocked)
import Engine.Input.Thread.Dispatch (processInputs, processInput)

startInputThread ∷ EngineEnv → IO ThreadState
startInputThread env = do
    let logRef        = loggerRef env
    logger ← readIORef logRef
    stateRef ← newIORef ThreadRunning
    doneVar ← newEmptyMVar
    threadId ← catch
        (do
            logInfo logger CatInput "Starting input thread..."
            -- Only a REAL boot path (Graphical/Offscreen/Preview) ever
            -- calls this — App.Headless never does (no GLFW window to
            -- poll) — so this is the single source of truth
            -- saveWorldFn/handleLoadStaged consult to decide whether
            -- SaveInput belongs in a transaction's owner set (round 3
            -- review: it must not be a hard requirement headless boot
            -- can never satisfy).
            writeIORef (inputThreadActiveRef env) True
            tid ← forkIO $ runInputLoop env stateRef `finally` putMVar doneVar ()
            return tid
        )
        (\(e ∷ SomeException) → do
            logError logger CatInput $ "Failed starting input thread: " <> T.pack (show e)
            error "Input thread start failure."
        )
    return $ ThreadState stateRef threadId doneVar

runInputLoop ∷ EngineEnv → IORef ThreadControl → IO ()
runInputLoop env stateRef = do
  control ← readIORef stateRef
  logger ← readIORef (loggerRef env)
  case control of
    ThreadStopped → do
        logDebug logger CatInput "Input thread stopping..."
        pure ()
    ThreadPaused  → do
        threadDelay 100000  -- 100ms pause check
        runInputLoop env stateRef
    ThreadRunning → do
        -- One guarded tick per iteration; the recursive call lives
        -- OUTSIDE the catch — inside it, each tick pushes a catch
        -- frame that never pops (unbounded stack growth).
        ok ← catch
          (do
            -- Round 3 review (issue #763): Input joins the save
            -- barrier's owner set as SaveInput so a load publish can
            -- actually quiesce it, same as every other owner —
            -- previously Input was not a SaveOwner at all, so it kept
            -- draining inputQueue and dispatching fresh Lua/gameplay
            -- messages for the ENTIRE captureLocked window, well past
            -- the one-time luaQueue flush
            -- 'Engine.Scripting.Lua.Thread.Dispatch.handleLoadStaged'
            -- performs before queuing WorldLoadPublish. Gating this the
            -- same way Unit/Combat/Simulation already are closes that:
            -- a pre-load input event still sitting in inputQueue at the
            -- lock boundary is left there (and discarded by
            -- World.Load.Publish's queue flush) rather than being
            -- dispatched against the replacement session.
            locked ← captureLocked (saveBarrierRef env)
            unless locked $ do
                inpSt ← readIORef (inputStateRef env)
                -- processInputs publishes to inputStateRef after each
                -- event it processes (#697) — no batch write here.
                _ ← processInputs env inpSt
                pure ()
            acknowledgeCurrent (saveBarrierRef env) SaveInput
            threadDelay 16666  -- ~60 FPS
            pure True
          )
          (\(e ∷ SomeException) → do
            logger ← readIORef (loggerRef env)
            logError logger CatInput $ "Input thread crashed: " <> T.pack (show e)
            writeIORef (lifecycleRef env) CleaningUp
            pure False
          )
        when ok $ runInputLoop env stateRef
