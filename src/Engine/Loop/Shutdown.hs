module Engine.Loop.Shutdown
  ( shutdownEngine
  , checkStatus
  ) where

import UPrelude
import Control.Exception (displayException)
import Data.IORef (writeIORef, readIORef)
import System.Exit (exitFailure)
import Engine.Core.Log (shutdownLogger, LogCategory(..))
import Engine.Core.Log.Monad (logDebugM, logInfoM)
import Engine.Core.Monad
import Engine.Core.State
import Engine.Core.Thread (ThreadState, shutdownThread)
import Engine.Core.Error.Exception (EngineException(..), SystemError(..))
import Engine.Graphics.Window.Types (Window(..))
import Engine.Graphics.Vulkan.Types.Cleanup (Cleanup(..), runAllCleanups)
import qualified Engine.Graphics.Window.GLFW as GLFW
import Engine.Input.Callback (clearGLFWCallbacks)
import Engine.Scene.Types (createBatchManager, SceneManager(..))
import Vulkan.Core10 (deviceWaitIdle)

-----------------------------------------------------------
-- Engine Shutdown
-----------------------------------------------------------

-- | Shutdown the engine
shutdownEngine ∷ Window → ThreadState → ThreadState → EngineM ε σ ()
shutdownEngine (Window win) inputThreadState luaThreadState = do
    logInfoM CatSystem "Starting engine shutdown..."
    state ← gets graphicsState
    logDebugM CatSystem "Clearing batch manager..."
    modify $ \s → s { sceneManager = (sceneManager s) {
                          smBatchManager = createBatchManager } }
    logDebugM CatSystem "Waiting for Vulkan device idle..."
    forM_ (vulkanDevice state) $ \device → liftIO $ deviceWaitIdle device
    logDebugM CatSystem "Running Vulkan cleanup actions..."
    liftIO $ runAllCleanups (vulkanCleanup state)
    logDebugM CatSystem "Cleaning up GLFW..."
    liftIO $ GLFW.postEmptyEvent
    GLFW.setWindowShouldClose win True
    liftIO $ clearGLFWCallbacks win
    env ← ask
    logDebugM CatSystem "Shutting down input thread..."
    liftIO $ shutdownThread inputThreadState
    logDebugM CatSystem "Shutting down Lua thread..."
    liftIO $ shutdownThread luaThreadState
    logDebugM CatSystem "Shutting down logger..."
    logger ← liftIO $ readIORef $ loggerRef env
    liftIO $ shutdownLogger logger
    liftIO $ writeIORef (lifecycleRef env) EngineStopped
    logDebugM CatSystem "Engine shutdown complete"

-----------------------------------------------------------
-- Status Checking
-----------------------------------------------------------

-- | Check engine status for continuation
checkStatus ∷ Either EngineException () → IO (Either EngineException ())
checkStatus (Right ()) = pure (Right ())
checkStatus (Left err) = do
    putStrLn $ displayException err
    exitFailure
