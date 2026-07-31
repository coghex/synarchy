-- | Preview boot path: GLFW window + Vulkan, same as 'App.Graphical', but
--   a structurally distinct thread topology — no world/unit/sim/combat
--   threads. Phase 1 (#632) of the @--preview@ texture-browser epic
--   (#427): boots straight to @scripts/preview_manager.lua@ (wired via
--   @game.init@'s preview branch in @scripts/init.lua@) instead of the
--   normal menu/HUD script set.
module App.Preview
  ( runPreview
  ) where

import UPrelude
import Data.IORef (readIORef)
import Engine.Core.Init (initializeEngine, EngineInitResult(..))
import Engine.Core.Defaults (defaultWindowConfig)
import Engine.Core.Monad (runEngineM, EngineM', liftIO)
import Engine.Core.State (EngineEnv(..), graphicsState, glfwWindow)
import Engine.Core.Types (PreviewBrowse)
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
import App.Boot (BootWorkers(..), FatalStream(..), previewBootConfig
                , handleBootResult)
import App.Exception (guardNativeExceptions)

-- | Run the engine in preview mode: GLFW window + Vulkan, but no world,
--   unit, sim, or combat thread. The input thread is kept so the OS
--   window-close button and the debug console (started inside the Lua
--   thread, same as headless) both work normally. 'mBrowse' is the
--   simple-category browsing state @app/Main.hs@ already resolved
--   (discovery/containment done, #886) — 'Nothing' for a grouped
--   category, which keeps Phase 1's (#632) placeholder-label boot.
runPreview ∷ (Text, Maybe Text) → Maybe PreviewBrowse → Maybe Int → IO ()
runPreview target mBrowse mPort = do
  EngineInitResult env ← initializeEngine

  let env' = previewBootConfig target mBrowse mPort env

  inputThreadState ← startInputThread env'
  luaThreadState   ← startLuaThread env'

  -- Preview's whole point is the trimmed topology: no world, unit, sim
  -- or combat thread ever starts.
  let workers = BootWorkers
        { bwCombat = Nothing
        , bwSim    = Nothing
        , bwUnit   = Nothing
        , bwWorld  = Nothing
        , bwInput  = Just inputThreadState
        , bwLua    = Just luaThreadState
        }

  videoConfig ← readIORef (videoConfigRef env')

  let engineAction ∷ EngineM' EngineEnv ()
      engineAction = do
        logInfoM CatSystem "Starting engine (preview)..."
        window ← GLFW.createWindow $ defaultWindowConfig videoConfig
        modify $ \s → s { graphicsState = (graphicsState s) {
                            glfwWindow = Just window } }

        let Window glfwWin = window
        liftIO $ setupCallbacks glfwWin (lifecycleRef env') (inputQueue env')

        _ ← initializeVulkan window
        mainLoop

        shutdownEngine (Just window) Nothing Nothing
                       inputThreadState luaThreadState
        logDebugM CatSystem "Preview engine shutdown complete."

  result ← guardNativeExceptions $ runEngineM engineAction env' checkStatus
  handleBootResult FatalToStdout env' workers result
