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
import Control.Exception (displayException)
import Data.IORef (readIORef)
import System.Exit (exitFailure)
import Engine.Core.Init (initializeEngine, EngineInitResult(..))
import Engine.Core.Defaults (defaultWindowConfig)
import Engine.Core.Monad (runEngineM, EngineM', liftIO)
import Engine.Core.State (EngineEnv(..), graphicsState, glfwWindow)
import Engine.Core.Types (EngineConfig(..), BootProfile(..), PreviewBrowse)
import Engine.Core.Thread (shutdownThread)
import Engine.Core.Log (LogCategory(..), shutdownLogger)
import Engine.Core.Log.Monad (logDebugM, logInfoM)
import Engine.Graphics.Vulkan.Init (initializeVulkan)
import Engine.Graphics.Window.Types (Window(..))
import qualified Engine.Graphics.Window.GLFW as GLFW
import Engine.Input.Callback (setupCallbacks)
import Engine.Input.Thread (startInputThread)
import Engine.Loop (mainLoop)
import Engine.Loop.Shutdown (shutdownEngine, checkStatus)
import Engine.Scripting.Lua.Thread (startLuaThread)
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

  let baseConfig = engineConfig env
      env' = env
        { engineConfig = baseConfig
            { ecDebugPort = fromMaybe (ecDebugPort baseConfig) mPort
            , ecBootProfile = BootPreview
            , ecPreviewTarget = Just target
            , ecPreviewBrowse = mBrowse
            } }

  inputThreadState ← startInputThread env'
  luaThreadState   ← startLuaThread env'

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
  case result of
    Left err → do
        putStrLn $ displayException err
        shutdownThread inputThreadState
        shutdownThread luaThreadState
        -- Flush buffered log lines — the error context is exactly
        -- what we must not lose — then exit with a failure code.
        logger ← readIORef (loggerRef env')
        shutdownLogger logger
        exitFailure
    Right _ → pure ()
