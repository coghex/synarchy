{-# LANGUAGE Strict #-}

-- | Combat thread.
--
-- The thread drains the combat command queue (`combatQueue` on
-- `EngineEnv`) and dispatches each command to `Combat.Resolution`,
-- which runs full resolution (hit roll, body part, damage, wound,
-- death check) and pushes `CombatEvent`s onto `combatEventsRef`.
--
-- The thread also exists to give the combat sim an obvious home that
-- doesn't share contention with the unit thread (which is doing
-- per-tile movement at 30 Hz). Combat ticks at 60 Hz so attacks feel
-- snappy when commands arrive.
module Combat.Thread
    ( startCombatThread
    , processAllCommands
    ) where

import UPrelude
import Engine.Core.Capability.UnitCombat
    (UnitCombatCapability(..), toUnitCombatCapability)
import Engine.Core.Capability.WorldSim
    (WorldSimCapability(..), toWorldSimCapability)
import Data.IORef (readIORef, atomicModifyIORef')
import Control.Concurrent (threadDelay)
import qualified System.Random as Random
import Engine.Core.Thread
    (ThreadState, WorkerFailLevel(..), WorkerSpec(..), noRefusal
    , startWorkerThread, workerCrashStderrSink)
import Engine.Core.State
    (EngineEnv, lifecycleRef, loggerRef, saveBarrierRef)
import Engine.Save.Barrier (SaveOwner(..), acknowledgeCurrent, ownerGated, saveInProgress)
import Engine.Core.Log (logDebug, logError, LogCategory(..))
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

-- | What the worker carries from tick to tick.
--
--   'clStrikeGen' is the STRIKE STREAM (#2328). It lives here, and not
--   on 'EngineEnv', because it has exactly one consumer — this thread's
--   own 'Combat.Resolution.resolveAttack', which drains commands
--   sequentially and is never re-entrant — so it can be reserved by a
--   pure split and spent only once a strike has actually committed.
--   That is what lets a refused strike advance NOTHING: on the
--   four-writer 'ucStatRNGRef' the same trick is unavailable, because
--   claiming atomically is already advancing it and a refusal would
--   have to unwind against concurrent draws (the rationale #2297 wrote
--   down for the medical stream). Split off 'ucStatRNGRef' once at
--   startup, like 'Combat.Wounds.Tick' does, so it is seeded from the
--   same system entropy without sharing the pool.
data CombatLoop = CombatLoop
    { clTick      ∷ !Int
      -- ^ counter modulo 'woundsTickEvery'
    , clStrikeGen ∷ !Random.StdGen
    }

startCombatThread ∷ EngineEnv → IO ThreadState
startCombatThread env = startWorkerThread WorkerSpec
    { wsName        = "Combat"
    , wsLoggerRef   = loggerRef env
    , wsCategory    = CatThread
    , wsLifecycleRef = lifecycleRef env
    , wsCrashSink   = workerCrashStderrSink
    , wsStartingMsg = "Starting combat thread..."
    , wsStartedMsg  = Just "Combat thread started"
    , wsFailMsg     = "Failed starting combat thread: "
    , wsFailLevel   = WorkerFailError
    , wsFailFatal   = "Combat thread start failure."
    , wsStartup     = \_ → noRefusal
        (CombatLoop 0 ⊚ atomicModifyIORef'
            (ucStatRNGRef (toUnitCombatCapability env)) Random.splitGen)
    , wsTick        = combatTick env
    , wsOnStop      = \_ → do
        logger ← readIORef (loggerRef env)
        logDebug logger CatThread "Combat thread stopping..."
    , wsOnCrash     = \_ e → do
        logger ← readIORef (loggerRef env)
        logError logger CatThread $ "Combat thread crashed: "
            <> tshow e
      -- Fail-stop, like every other worker thread (world, unit,
      -- input). Re-entering the loop here skipped the threadDelay, so
      -- a persistent fault tight-looped at 100% CPU flooding the log —
      -- and a combat thread that silently retries forever is corrupted
      -- gameplay with no signal anyway. The shared lifecycle owns the
      -- stop half, and since #2283 owns the lifecycle write too, made
      -- BEFORE the line above so a throwing logger cannot swallow it;
      -- this callback only reports.
    , wsOnCrashCleanup = \_ _ → pure ()
    }

-- | Advance one tick, carrying the loop state the shared lifecycle
--   threads from tick to tick: the counter modulo `woundsTickEvery`, so
--   the wound subsystem runs at ~10 Hz rather than the 60 Hz command-
--   drain rate, and the strike stream every drained command reserves
--   from.
combatTick ∷ EngineEnv → CombatLoop → IO (Maybe CombatLoop)
combatTick env loop = do
    -- Honour the global pause toggle. Same gate the unit
    -- thread uses around gameTime + movement: when paused
    -- we sleep the tick and do nothing, so combat events
    -- queued mid-pause stay queued and wounds don't bleed
    -- out while the player has the game stopped.
    paused ← readIORef (wsEnginePausedRef (toWorldSimCapability env))
    next ← if paused
        then do
            -- A save boundary drains accepted combat commands
            -- before acknowledging; ordinary pause retains the
            -- historical no-work behaviour.
            -- #2221: the per-OWNER gate, not the global capture lock.
            -- Once this owner has acknowledged the final quiescence
            -- pass it must drain nothing more until capture completes.
            locked ← ownerGated (saveBarrierRef env) SaveCombat
            saving ← saveInProgress (saveBarrierRef env)
            gen' ← if saving ∧ not locked
                then processAllCommands env (clStrikeGen loop)
                else pure (clStrikeGen loop)
            acknowledgeCurrent (saveBarrierRef env) SaveCombat
            pure loop { clStrikeGen = gen' }
        else do
            gen' ← processAllCommands env (clStrikeGen loop)
            let next = (clTick loop + 1) `mod` woundsTickEvery
            when (next ≡ 0) $
                tickAllWounds env
                    (realToFrac (combatTickRate
                        * fromIntegral woundsTickEvery))
            pure CombatLoop { clTick = next, clStrikeGen = gen' }
    threadDelay (floor (combatTickRate * 1000000 ∷ Double))
    pure (Just next)

-- | Drain the command queue and dispatch each command to
--   'handleCommand' for resolution.
--
--   Exported so a headless spec can drive the REAL drain — the same
--   queue, the same dispatch, the same 'resolveAttack' — after
--   mutating the world between admission and commit (#2328), which is
--   the window the whole admission contract is about. Nothing in
--   production calls it outside 'combatTick'.
processAllCommands ∷ EngineEnv → Random.StdGen → IO Random.StdGen
processAllCommands env = go
  where
    go gen = do
        mCmd ← Q.tryReadQueue (ucCombatQueue (toUnitCombatCapability env))
        case mCmd of
            Nothing  → pure gen
            Just cmd → handleCommand env gen cmd ≫= go

-- | Resolve one command, returning the strike stream to carry on with:
--   the one the command was handed if it refused, an advanced one if it
--   committed (#2328).
handleCommand ∷ EngineEnv → Random.StdGen → CombatCommand → IO Random.StdGen
handleCommand env gen (CombatAttack attacker target mode reachBonus impactSpeed) =
    -- Full resolution: hit roll → body part → damage → wound →
    -- death check + stamina drain. Emits "miss" / "hit" / "death"
    -- events. reachBonus lifts the strike-height reach + impactSpeed folds
    -- the lunge's full-body momentum into the strike (both 0 = normal swing).
    resolveAttack env gen attacker target mode reachBonus impactSpeed
