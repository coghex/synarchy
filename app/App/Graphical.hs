-- | Full-graphics boot path: GLFW window + Vulkan, the normal player-facing
--   run mode.
module App.Graphical
  ( runGraphical
  ) where

import UPrelude
import Data.IORef (readIORef)
import Engine.Core.Init (initializeEngine, EngineInitResult(..))
import Engine.Core.Defaults (defaultWindowConfig)
import Engine.Core.Monad (runEngineM, EngineM', liftIO)
import Engine.Core.State (EngineEnv(..), graphicsState, glfwWindow)
import Engine.Core.Types (BootProfile(..))
import Engine.Core.Thread (shutdownThread)
import Engine.Core.Log (LogCategory(..))
import Engine.Core.Log.Monad (logDebugM, logInfoM)
import Engine.Graphics.Vulkan.Init (initializeVulkan)
import Engine.Graphics.Window.Types (Window(..))
import qualified Engine.Graphics.Window.GLFW as GLFW
import Engine.Input.Callback (setupCallbacks)
import Engine.Input.Thread (startInputThread)
import Engine.Loop (mainLoop)
import Engine.Loop.Shutdown (shutdownEngine, checkStatus)
import Engine.Scripting.Lua.Thread (startLuaThread)
import World.Thread (startWorldThread)
import Unit.Thread (startUnitThread)
import Combat.Thread (startCombatThread)
import Sim.Thread (startSimThread)
import App.Boot (BootWorkers(..), FatalStream(..), bootConfig, handleBootResult)
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

  let workers = BootWorkers
        { bwCombat = Just combatThreadState
        , bwSim    = Just simThreadState
        , bwUnit   = Just unitThreadState
        , bwWorld  = Just worldThreadState
        , bwInput  = Just inputThreadState
        , bwLua    = Just luaThreadState
        }

  videoConfig ← readIORef (videoConfigRef env')

  let engineAction ∷ EngineM' ()
      engineAction = do
        logInfoM CatSystem "Starting engine..."
        window ← GLFW.createWindow $ defaultWindowConfig videoConfig
        modify $ \s → s { graphicsState = (graphicsState s) {
                            glfwWindow = Just window } }

        let Window glfwWin = window
        liftIO $ setupCallbacks glfwWin (lifecycleRef env') (inputQueue env')

        _ ← initializeVulkan window
        mainLoop

        -- Combat first: wound ticks enqueue UnitKill/UnitCollapse onto
        -- the unit queue, so the producer has to stop before the
        -- consumer (unit thread) is torn down inside shutdownEngine.
        liftIO $ shutdownThread combatThreadState
        liftIO $ shutdownThread simThreadState
        shutdownEngine (Just window) (Just unitThreadState)
                       (Just worldThreadState) inputThreadState luaThreadState
        logDebugM CatSystem "Engine shutdown complete."

  result ← guardNativeExceptions $ runEngineM engineAction env' checkStatus
  handleBootResult FatalToStdout env' workers result
