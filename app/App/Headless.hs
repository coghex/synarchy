-- | Headless boot path: no window, no GPU. Starts Lua, world, unit, sim
--   and combat threads with a debug console on a configurable port.
--   Useful for automated testing, CI, and scripted world generation.
module App.Headless
  ( runHeadless
  ) where

import UPrelude
import Control.Exception (displayException)
import Data.IORef (readIORef, writeIORef)
import System.Exit (exitFailure)
import Engine.Core.Init (initializeEngineHeadless, EngineInitResult(..))
import Engine.Core.Monad (runEngineM, EngineM', liftIO)
import Engine.Core.State (EngineEnv(..), EngineLifecycle(..))
import Engine.Core.Types (EngineConfig(..), BootProfile(..))
import Engine.Core.Thread (shutdownThread)
import Engine.Core.Log (LogCategory(..), shutdownLogger)
import Engine.Core.Log.Monad (logDebugM, logInfoM)
import Engine.Loop.Headless (headlessLoop)
import Engine.Loop.Shutdown (checkStatus)
import Engine.Scripting.Lua.Thread (startLuaThread)
import World.Thread (startWorldThread)
import Unit.Thread (startUnitThread)
import Combat.Thread (startCombatThread)
import Sim.Thread (startSimThread)
import App.Exception (guardNativeExceptions)

-- | Run engine in headless mode (no window, no GPU)
--   Starts Lua, world, and unit threads. Debug console on configurable port.
--   Useful for automated testing, CI, and scripted world generation.
runHeadless ∷ BootProfile → Maybe Int → IO ()
runHeadless bootProfile mPort = do
  EngineInitResult env ← initializeEngineHeadless

  let env' = case mPort of
        Just p  → env
            { engineConfig = (engineConfig env)
                { ecDebugPort = p
                , ecBootProfile = bootProfile
                } }
        Nothing → env
            { engineConfig = (engineConfig env)
                { ecBootProfile = bootProfile
                } }

  luaThreadState   ← startLuaThread env'
  worldThreadState ← startWorldThread env'
  unitThreadState  ← startUnitThread env'
  simThreadState   ← startSimThread env'
  combatThreadState ← startCombatThread env'

  let engineAction ∷ EngineM' EngineEnv ()
      engineAction = do
        logInfoM CatSystem "Starting engine (headless)..."
        headlessLoop
        logInfoM CatSystem "Headless engine shutting down..."
        liftIO $ shutdownThread combatThreadState
        liftIO $ shutdownThread simThreadState
        liftIO $ shutdownThread unitThreadState
        liftIO $ shutdownThread worldThreadState
        liftIO $ shutdownThread luaThreadState
        logger ← liftIO $ readIORef $ loggerRef env'
        liftIO $ shutdownLogger logger
        liftIO $ writeIORef (lifecycleRef env') EngineStopped
        logDebugM CatSystem "Headless engine shutdown complete."

  result ← guardNativeExceptions $ runEngineM engineAction env' checkStatus
  case result of
    Left err → do
        putStrLn $ displayException err
        shutdownThread combatThreadState
        shutdownThread simThreadState
        shutdownThread unitThreadState
        shutdownThread worldThreadState
        shutdownThread luaThreadState
        -- Flush buffered log lines — the error context is exactly
        -- what we must not lose — then exit with a failure code.
        logger ← readIORef (loggerRef env')
        shutdownLogger logger
        exitFailure
    Right _ → pure ()
