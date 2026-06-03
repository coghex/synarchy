{-# LANGUAGE Strict, UnicodeSyntax #-}

-- | Combat thread skeleton.
--
-- The thread drains the combat command queue (`combatQueue` on
-- `EngineEnv`) and processes each command — for now there's a single
-- placeholder handler that logs the command and otherwise does
-- nothing. Resolution (hit/miss/damage/wound/death) will replace the
-- no-op in the next slice, with each resolution step pushing
-- `CombatEvent`s onto `combatEventsRef`.
--
-- The thread also exists to give the combat sim an obvious home that
-- doesn't share contention with the unit thread (which is doing
-- per-tile movement at 30 Hz). Combat ticks at 60 Hz so attacks feel
-- snappy when commands arrive.
module Combat.Thread
    ( startCombatThread
    , combatTickRate
    ) where

import UPrelude
import qualified Data.Text as T
import Data.IORef (IORef, readIORef, writeIORef, newIORef)
import Control.Concurrent (forkIO, threadDelay)
import Control.Exception (SomeException, catch, finally)
import Control.Concurrent.MVar (newEmptyMVar, putMVar)
import Control.Monad (when)
import Engine.Core.Thread (ThreadState(..), ThreadControl(..))
import Engine.Core.State (EngineEnv(..))
import Engine.Core.Log (logInfo, logDebug, logError, LogCategory(..))
import qualified Engine.Core.Queue as Q
import Combat.Types (CombatCommand(..))
import Combat.Resolution (resolveAttack)
import Combat.Wounds (tickAllWounds)

-- | Run the wound subsystem every Nth combat tick (60 Hz → 10 Hz).
woundsTickEvery ∷ Int
woundsTickEvery = 6

-- | Combat thread tick rate in seconds (60 Hz). Higher than the unit
--   thread because attack resolution is event-driven — when a command
--   arrives we want it processed within a frame.
combatTickRate ∷ Double
combatTickRate = 1.0 / 60.0

startCombatThread ∷ EngineEnv → IO ThreadState
startCombatThread env = do
    logger ← readIORef (loggerRef env)
    stateRef ← newIORef ThreadRunning
    doneVar ← newEmptyMVar
    threadId ← catch
        (do
            logInfo logger CatThread "Starting combat thread..."
            tid ← forkIO $ combatLoop env stateRef 0 `finally` putMVar doneVar ()
            logInfo logger CatThread "Combat thread started"
            return tid
        )
        (\(e ∷ SomeException) → do
            logError logger CatThread $ "Failed starting combat thread: "
                <> T.pack (show e)
            error "Combat thread start failure."
        )
    return $ ThreadState stateRef threadId doneVar

-- | Counter modulo `woundsTickEvery` so we only run the wound
--   subsystem at ~10 Hz instead of the 60 Hz command-drain rate.
combatLoop ∷ EngineEnv → IORef ThreadControl → Int → IO ()
combatLoop env stateRef tick = do
    control ← readIORef stateRef
    case control of
        ThreadStopped → do
            logger ← readIORef (loggerRef env)
            logDebug logger CatThread "Combat thread stopping..."
            pure ()
        ThreadPaused → do
            threadDelay 100000
            combatLoop env stateRef tick
        ThreadRunning → do
            catch
              (do
                -- Honour the global pause toggle. Same gate the unit
                -- thread uses around gameTime + movement: when paused
                -- we sleep the tick and do nothing, so combat events
                -- queued mid-pause stay queued and wounds don't bleed
                -- out while the player has the game stopped.
                paused ← readIORef (enginePausedRef env)
                next ← if paused
                    then pure tick      -- preserve counter; no work
                    else do
                        processAllCommands env
                        let next = (tick + 1) `mod` woundsTickEvery
                        when (next == 0) $
                            tickAllWounds env
                                (realToFrac (combatTickRate
                                    * fromIntegral woundsTickEvery))
                        pure next
                threadDelay (floor (combatTickRate * 1000000 ∷ Double))
                combatLoop env stateRef next
              )
              (\(e ∷ SomeException) → do
                logger ← readIORef (loggerRef env)
                logError logger CatThread $ "Combat thread crashed: "
                    <> T.pack (show e)
                combatLoop env stateRef tick
              )

-- | Drain the command queue and dispatch each command. Skeleton phase:
--   we only log incoming commands. Real resolution lands in the next
--   slice as `Combat.Resolution.processAttack`.
processAllCommands ∷ EngineEnv → IO ()
processAllCommands env = go
  where
    go = do
        mCmd ← Q.tryReadQueue (combatQueue env)
        case mCmd of
            Nothing  → pure ()
            Just cmd → do
                handleCommand env cmd
                go

handleCommand ∷ EngineEnv → CombatCommand → IO ()
handleCommand env (CombatAttack attacker target mode) =
    -- Full resolution: hit roll → body part → damage → wound →
    -- death check + stamina drain. Emits "miss" / "hit" / "death"
    -- events.
    resolveAttack env attacker target mode
