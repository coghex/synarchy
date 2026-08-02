-- | Full-graphics boot path: GLFW window + Vulkan, the normal player-facing
--   run mode.
module App.Graphical
  ( runGraphical
  ) where

import UPrelude
import Data.IORef (readIORef)
import Engine.Core.Init (initializeEngine, EngineInitResult(..))
import Engine.Core.Defaults (defaultWindowConfig)
import Engine.Core.Monad (runEngineM, EngineM', liftIO, modifyGraphicsState)
import Engine.Core.State (EngineEnv(..), glfwWindow)
import Engine.Core.Types (BootProfile(..))
import Engine.Core.Workers (EngineWorkers(..))
import Engine.Core.Log (LogCategory(..))
import Engine.Core.Log.Monad (logDebugM, logInfoM)
import Engine.Graphics.Vulkan.Init (initializeVulkan)
import Engine.Graphics.Window.Types (Window(..))
import qualified Engine.Graphics.Window.GLFW as GLFW
import Engine.Input.Callback (setupCallbacks)
import Engine.Input.Thread (startInputThread)
import Engine.Loop (mainLoop)
import Engine.Loop.Shutdown (ShutdownTargets(..), shutdownEngine, checkStatus)
import Engine.Scripting.Lua.Thread (startLuaThread)
import World.Thread (startWorldThread)
import Unit.Thread (startUnitThread)
import Combat.Thread (startCombatThread)
import Sim.Thread (startSimThread)
import App.Boot (FatalStream(..), bootConfig, handleBootResult)
import App.Exception (guardNativeExceptions)

-- | Run engine with full graphics (GLFW window + Vulkan)
runGraphical ∷ BootProfile → Maybe Int → IO ()
runGraphical bootProfile mPort = do
  -- Initialize engine
  EngineInitResult env ← initializeEngine

  let env' = bootConfig bootProfile mPort env

  inputThreadState ← startInputThread env'
  luaThreadState   ← startLuaThread env'
  worldThreadState ← startWorldThread env'
  unitThreadState  ← startUnitThread env'
  simThreadState   ← startSimThread env'
  combatThreadState ← startCombatThread env'

  let workers = EngineWorkers
        { ewCombat = Just combatThreadState
        , ewSim    = Just simThreadState
        , ewUnit   = Just unitThreadState
        , ewWorld  = Just worldThreadState
        , ewInput  = Just inputThreadState
        , ewLua    = Just luaThreadState
        }

  videoConfig ← readIORef (videoConfigRef env')

  let engineAction ∷ EngineM' ()
      engineAction = do
        logInfoM CatSystem "Starting engine..."
        window ← GLFW.createWindow $ defaultWindowConfig videoConfig
        modifyGraphicsState $ \gs → gs {
                            glfwWindow = Just window }

        let Window glfwWin = window
        liftIO $ setupCallbacks glfwWin (lifecycleRef env') (inputQueue env')

        _ ← initializeVulkan window
        mainLoop

        shutdownEngine ShutdownTargets { stWindow  = Just window
                                       , stWorkers = workers }
        logDebugM CatSystem "Engine shutdown complete."

  result ← guardNativeExceptions $ runEngineM engineAction env' checkStatus
  handleBootResult FatalToStdout env' workers result
