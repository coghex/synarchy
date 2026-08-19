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
import Engine.Core.Types (BootProfile(..), BootMode(..))
import Engine.Core.Log (LogCategory(..), shutdownLogger)
import Engine.Core.Log.Monad (logDebugM, logInfoM)
import Engine.Loop.Headless (headlessLoop)
import Engine.Loop.Shutdown (checkStatus)
import Engine.Scripting.Lua.Thread (startLuaThread)
import World.Thread (startWorldThread)
import Unit.Thread (startUnitThread)
import Combat.Thread (startCombatThread)
import Sim.Thread (startSimThread)
import Engine.Core.Workers (EngineWorkers(..), shutdownEngineWorkers)
import App.Boot (FatalStream(..), bootConfig, handleBootResult
                , luaThreadOrAbort)
import App.Exception (guardNativeExceptions)

runHeadless ∷ BootProfile → Maybe Int → IO ()
runHeadless bootProfile mPort = do
  EngineInitResult env ← initializeEngineHeadless

  let env' = bootConfig ModeHeadless bootProfile mPort env

  -- The debug console is headless's ONLY control surface, so a listener
  -- that never came up aborts the boot (#1190) instead of continuing
  -- with an inert command queue. Nothing has started yet — Lua is this
  -- mode's first worker — hence the empty already-started set.
  luaThreadState   ← startLuaThread env' ⌦ luaThreadOrAbort env' []
  worldThreadState ← startWorldThread env'
  unitThreadState  ← startUnitThread env'
  simThreadState   ← startSimThread env'
  combatThreadState ← startCombatThread env'

  -- Headless starts no input thread — the debug console lives inside
  -- the Lua thread.
  let workers = EngineWorkers
        { ewCombat = Just combatThreadState
        , ewSim    = Just simThreadState
        , ewUnit   = Just unitThreadState
        , ewWorld  = Just worldThreadState
        , ewInput  = Nothing
        , ewLua    = Just luaThreadState
        }

  let engineAction ∷ EngineM' ()
      engineAction = do
        logInfoM CatSystem "Starting engine (headless)..."
        headlessLoop
        logInfoM CatSystem "Headless engine shutting down..."
        liftIO $ shutdownEngineWorkers workers
        logger ← liftIO $ readIORef $ loggerRef env'
        liftIO $ shutdownLogger logger
        liftIO $ writeIORef (lifecycleRef env') EngineStopped
        logDebugM CatSystem "Headless engine shutdown complete."

  result ← guardNativeExceptions $ runEngineM engineAction env' checkStatus
  handleBootResult FatalToStdout env' workers result
