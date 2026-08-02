-- | Preview boot path: GLFW window + Vulkan, same as 'App.Graphical', but
--   a structurally distinct thread topology — no world/unit/sim/combat
--   threads. Introduced by Phase 1 (#632) of the @--preview@
--   texture-browser epic (#427): boots straight to
--   @scripts/preview_manager.lua@ (wired via @game.init@'s preview
--   branch in @scripts/init.lua@) instead of the normal menu/HUD script
--   set.
module App.Preview
  ( runPreview
  ) where

import UPrelude
import Data.IORef (readIORef)
import Engine.Core.Init (initializeEngine, EngineInitResult(..))
import Engine.Core.Defaults (defaultWindowConfig)
import Engine.Core.Monad (runEngineM, EngineM', liftIO, modifyGraphicsState)
import Engine.Core.State (EngineEnv(..), glfwWindow)
import Engine.Core.Types (PreviewBrowse)
import Engine.Core.Log (LogCategory(..))
import Engine.Core.Log.Monad (logDebugM, logInfoM)
import Engine.Graphics.Vulkan.Init (initializeVulkan)
import Engine.Graphics.Window.Types (Window(..))
import qualified Engine.Graphics.Window.GLFW as GLFW
import Engine.Input.Callback (setupCallbacks)
import Engine.Input.Thread (startInputThread)
import Engine.Loop (mainLoop)
import Engine.Loop.Shutdown (ShutdownTargets(..), shutdownEngine, checkStatus)
import Engine.Core.Workers (EngineWorkers(..))
import Engine.Scripting.Lua.Thread (startLuaThread)
import App.Boot (FatalStream(..), previewBootConfig, handleBootResult)
import App.Exception (guardNativeExceptions)

-- | Run the engine in preview mode: GLFW window + Vulkan, but no world,
--   unit, sim, or combat thread. The input thread is kept so the OS
--   window-close button and the debug console (started inside the Lua
--   thread, same as headless) both work normally. 'mBrowse' is the
--   browsing state @app/Main.hs@ already resolved (discovery,
--   containment, and default selection all done pre-boot — #886/#887/
--   #888); as of #888 every canonical target supplies one, so a
--   'Nothing' here is only the degenerate no-target case.
runPreview ∷ (Text, Maybe Text) → Maybe PreviewBrowse → Maybe Int → IO ()
runPreview target mBrowse mPort = do
  EngineInitResult env ← initializeEngine

  let env' = previewBootConfig target mBrowse mPort env

  inputThreadState ← startInputThread env'
  luaThreadState   ← startLuaThread env'

  -- Preview's whole point is the trimmed topology: no world, unit, sim
  -- or combat thread ever starts.
  let workers = EngineWorkers
        { ewCombat = Nothing
        , ewSim    = Nothing
        , ewUnit   = Nothing
        , ewWorld  = Nothing
        , ewInput  = Just inputThreadState
        , ewLua    = Just luaThreadState
        }

  videoConfig ← readIORef (videoConfigRef env')

  let engineAction ∷ EngineM' ()
      engineAction = do
        logInfoM CatSystem "Starting engine (preview)..."
        window ← GLFW.createWindow $ defaultWindowConfig videoConfig
        modifyGraphicsState $ \gs → gs {
                            glfwWindow = Just window }

        let Window glfwWin = window
        liftIO $ setupCallbacks glfwWin (lifecycleRef env') (inputQueue env')

        _ ← initializeVulkan window
        mainLoop

        shutdownEngine ShutdownTargets { stWindow  = Just window
                                       , stWorkers = workers }
        logDebugM CatSystem "Preview engine shutdown complete."

  result ← guardNativeExceptions $ runEngineM engineAction env' checkStatus
  handleBootResult FatalToStdout env' workers result
