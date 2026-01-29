module Engine.Loop.Shutdown
  ( shutdownEngine
  , checkStatus
  ) where

import UPrelude
import Control.Exception (displayException)
import Data.IORef (writeIORef)
import System.Exit (exitFailure)
import Engine.Core.Monad
import Engine.Core.State
import Engine.Core.Thread (ThreadState, shutdownThread)
import Engine.Core.Error.Exception
import Engine.Graphics.Window.Types (Window(..))
import qualified Engine.Graphics.Window.GLFW as GLFW
import Engine.Input.Callback (clearGLFWCallbacks)
import Engine.Scene.Types (createBatchManager, SceneManager(..))
import Vulkan.Core10 (deviceWaitIdle)

-- | Shutdown the engine
shutdownEngine ∷ Window → ThreadState → ThreadState → EngineM ε σ ()
shutdownEngine (Window win) inputThreadState luaThreadState = do
    logDebug "Engine cleaning up..."
    state ← gets graphicsState
    
    -- Clear batch manager
    modify $ \s → s { sceneManager = (sceneManager s) {
                          smBatchManager = createBatchManager } }
    
    -- Wait for Vulkan device
    forM_ (vulkanDevice state) $ \device → liftIO $ deviceWaitIdle device
    
    -- GLFW cleanup
    liftIO $ GLFW.postEmptyEvent
    GLFW.setWindowShouldClose win True
    liftIO $ clearGLFWCallbacks win
    
    -- Shutdown threads
    env ← ask
    logDebug "Shutting down input thread..."
    liftIO $ shutdownThread inputThreadState
    
    logDebug "Shutting down Lua thread..."
    liftIO $ shutdownThread luaThreadState
    
    -- Mark engine as stopped
    liftIO $ writeIORef (lifecycleRef env) EngineStopped

-- | Check engine status for continuation
checkStatus ∷ Either EngineException () → IO (Either EngineException ())
checkStatus (Right ()) = pure (Right ())
checkStatus (Left err) = do
    putStrLn $ displayException err
    exitFailure
