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
import Engine.Core.Error.Exception (EngineException(..))
import Engine.Graphics.Window.Types (Window(..))
import Engine.Graphics.Vulkan.Types.Cleanup (runAllCleanups)
import qualified Engine.Graphics.Window.GLFW as GLFW
import Engine.Input.Callback (clearGLFWCallbacks)
import Engine.Scene.Types (createBatchManager, SceneManager(..)
                          , SceneDynamicBuffer(..), TextInstanceBuffer(..))
import Vulkan.Core10 (deviceWaitIdle, destroyBuffer, freeMemory)

-- | Shutdown the engine
shutdownEngine ∷ Window → ThreadState → ThreadState → ThreadState
  → EngineM ε σ ()
shutdownEngine (Window win) worldThreadState inputThreadState luaThreadState = do
    logInfoM CatSystem "Starting engine shutdown..."
    state ← gets graphicsState
    let device = vulkanDevice state

    -- Clear batch manager
    logDebugM CatSystem "Clearing batch manager..."
    modify $ \s → s { sceneManager = (sceneManager s) {
                          smBatchManager = createBatchManager } }
    -- Destroy cached text instance buffer
    case device of
        Just dev → do
            case textInstanceBuffer state of
                Just tib → liftIO $ do
                    destroyBuffer dev (tibBuffer tib) Nothing
                    freeMemory dev (tibMemory tib) Nothing
                Nothing → pure ()
            
            -- Destroy cached dynamic vertex buffer
            case dynamicVertexBuffer state of
                Just sdb → liftIO $ do
                    destroyBuffer dev (sdbBuffer sdb) Nothing
                    freeMemory dev (sdbMemory sdb) Nothing
                Nothing → pure ()
        Nothing → logDebugM CatSystem "No Vulkan device found, skipping buffer cleanup"
    
    -- Wait for Vulkan device
    logDebugM CatSystem "Waiting for Vulkan device idle..."
    forM_ device $ \dev → liftIO $ deviceWaitIdle dev

    -- run manual cleanup actions
    logDebugM CatSystem "Running Vulkan cleanup actions..."
    liftIO $ runAllCleanups (vulkanCleanup state)
    
    -- GLFW cleanup
    logDebugM CatSystem "Cleaning up GLFW..."
    liftIO $ GLFW.postEmptyEvent
    GLFW.setWindowShouldClose win True
    liftIO $ clearGLFWCallbacks win
    
    -- Shutdown threads
    env ← ask
    logDebugM CatSystem "Shutting down world thread..."
    liftIO $ shutdownThread worldThreadState

    logDebugM CatSystem "Shutting down input thread..."
    liftIO $ shutdownThread inputThreadState
    
    logDebugM CatSystem "Shutting down Lua thread..."
    liftIO $ shutdownThread luaThreadState

    -- shut down logger
    logDebugM CatSystem "Shutting down logger..."
    logger ← liftIO $ readIORef $ loggerRef env
    liftIO $ shutdownLogger logger
    
    -- Mark engine as stopped
    liftIO $ writeIORef (lifecycleRef env) EngineStopped
    
    logDebugM CatSystem "Engine shutdown complete"

-- | Check engine status for continuation
checkStatus ∷ Either EngineException () → IO (Either EngineException ())
checkStatus (Right ()) = pure (Right ())
checkStatus (Left err) = do
    putStrLn $ displayException err
    exitFailure
