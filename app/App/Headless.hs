-- | Headless boot path: no window, no GPU. Starts Lua, world, unit, sim
--   and combat threads with a debug console on a configurable port.
--   Useful for automated testing, CI, and scripted world generation.
module App.Headless
  ( runHeadless
  ) where

import UPrelude
import Data.IORef (readIORef, writeIORef)
import Engine.Core.Init (initializeEngineHeadless, EngineInitResult(..))
import Engine.Core.Monad (runEngineM, EngineM', liftIO)
import Engine.Core.State (EngineEnv(..), EngineLifecycle(..))
import Engine.Core.Types (BootProfile(..))
import Engine.Core.Log (LogCategory(..), shutdownLogger)
import Engine.Core.Log.Monad (logDebugM, logInfoM)
import Engine.Loop.Headless (headlessLoop)
import Engine.Loop.Shutdown (checkStatus)
import Engine.Scripting.Lua.Thread (startLuaThread)
import World.Thread (startWorldThread)
import Unit.Thread (startUnitThread)
import Combat.Thread (startCombatThread)
import Sim.Thread (startSimThread)
import App.Boot (BootWorkers(..), FatalStream(..), bootConfig
                , handleBootResult, shutdownBootWorkers)
import App.Exception (guardNativeExceptions)

-- | Run engine in headless mode (no window, no GPU)
--   Starts Lua, world, and unit threads. Debug console on configurable port.
--   Useful for automated testing, CI, and scripted world generation.
runHeadless ∷ BootProfile → Maybe Int → IO ()
runHeadless bootProfile mPort = do
  EngineInitResult env ← initializeEngineHeadless

  let env' = bootConfig bootProfile mPort env

  luaThreadState   ← startLuaThread env'
  worldThreadState ← startWorldThread env'
  unitThreadState  ← startUnitThread env'
  simThreadState   ← startSimThread env'
  combatThreadState ← startCombatThread env'

  -- Headless starts no input thread — the debug console lives inside
  -- the Lua thread.
  let workers = BootWorkers
        { bwCombat = Just combatThreadState
        , bwSim    = Just simThreadState
        , bwUnit   = Just unitThreadState
        , bwWorld  = Just worldThreadState
        , bwInput  = Nothing
        , bwLua    = Just luaThreadState
        }

  let engineAction ∷ EngineM' EngineEnv ()
      engineAction = do
        logInfoM CatSystem "Starting engine (headless)..."
        headlessLoop
        logInfoM CatSystem "Headless engine shutting down..."
        liftIO $ shutdownBootWorkers workers
        logger ← liftIO $ readIORef $ loggerRef env'
        liftIO $ shutdownLogger logger
        liftIO $ writeIORef (lifecycleRef env') EngineStopped
        logDebugM CatSystem "Headless engine shutdown complete."

  result ← guardNativeExceptions $ runEngineM engineAction env' checkStatus
  handleBootResult FatalToStdout env' workers result
