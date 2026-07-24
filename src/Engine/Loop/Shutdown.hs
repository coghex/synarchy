module Engine.Loop.Shutdown
  ( shutdownEngine
  , checkStatus
  ) where

import UPrelude
import Data.IORef (writeIORef, readIORef)
import qualified Data.Vector as V
import Engine.Core.Log (shutdownLogger, LogCategory(..))
import Engine.Core.Log.Monad (logDebugM, logInfoM)
import Engine.Core.Monad
import Engine.Core.State
import Engine.Core.Capability.Core (CoreCapability(..), toCoreCapability)
import Engine.Core.Thread (ThreadState, shutdownThread)
import Engine.Core.Error.Exception (EngineException(..))
import Engine.Graphics.Window.Types (Window(..))
import Engine.Graphics.Vulkan.Types.Cleanup (runAllCleanups)
import Engine.Graphics.Vulkan.Sampler.Cache (destroySamplerCache)
import qualified Engine.Graphics.Window.GLFW as GLFW
import Engine.Input.Callback (clearGLFWCallbacks)
import Engine.Scene.Types (createBatchManager, SceneManager(..)
                          , SceneDynamicBuffer(..), TextInstanceBuffer(..))
import Vulkan.Core10 (deviceWaitIdle, destroyBuffer, freeMemory)

-- | Shutdown the engine. 'unitThreadState'/'worldThreadState' are
--   'Nothing' for a boot profile that never started them (preview mode
--   — see 'App.Preview' — has no world/unit/sim/combat threads at all).
--   The window is 'Nothing' for the offscreen mode (#650), which has
--   no GLFW state to tear down.
shutdownEngine ∷ Maybe Window → Maybe ThreadState → Maybe ThreadState
  → ThreadState → ThreadState → EngineM ε σ ()
shutdownEngine mWindow mUnitThreadState mWorldThreadState
                       inputThreadState luaThreadState = do
    logInfoM CatSystem "Starting engine shutdown..."
    state ← gets graphicsState
    let device = vulkanDevice state

    -- Vulkan teardown below runs BEFORE the worker threads stop. That
    -- is safe only while Vulkan objects are touched exclusively by
    -- this (main) thread — workers hand pixel data over via
    -- IORefs/queues and must never call into Vulkan.

    -- Clear batch manager
    logDebugM CatSystem "Clearing batch manager..."
    modify $ \s → s { sceneManager = (sceneManager s) {
                          smBatchManager = createBatchManager } }
   
    -- Wait for Vulkan device
    logDebugM CatSystem "Waiting for Vulkan device idle..."
    forM_ device $ \dev → liftIO $ deviceWaitIdle dev

    -- Destroy the last transient-texture generations (zoom atlas /
    -- world preview). These use explicit cleanups instead of
    -- exit-time allocResource (they're replaced per world init/load,
    -- see Engine.Scripting.Lua.Message), so the final generation is
    -- ours to free — after the waitIdle above, before the device goes.
    logDebugM CatSystem "Destroying transient textures..."
    forM_ (previewTexture state)   $ \tt → liftIO (ttCleanup tt)
    forM_ (zoomAtlasTexture state) $ \tt → liftIO (ttCleanup tt)

    -- run manual cleanup actions
    logDebugM CatSystem "Running Vulkan cleanup actions..."
    liftIO $ runAllCleanups (vulkanCleanup state)

    -- Destroy every cached sampler (texture + font). Device is already
    -- idle (waitIdle above); this frees the handful of shared VkSamplers
    -- the refcounted cache kept alive.
    logDebugM CatSystem "Destroying sampler cache..."
    cacheEnv ← ask
    forM_ device $ \dev →
      liftIO $ destroySamplerCache dev (samplerCacheRef cacheEnv)

    -- Destroy cached text instance buffer
    case device of
        Just dev → do
            V.forM_ (textInstanceBuffers state) $ \case
                Just tib → liftIO $ do
                    destroyBuffer dev (tibBuffer tib) Nothing
                    freeMemory dev (tibMemory tib) Nothing
                Nothing → pure ()

            -- Destroy cached dynamic vertex buffers (one per frame in flight)
            V.forM_ (dynamicVertexBuffers state) $ \case
                Just sdb → liftIO $ do
                    destroyBuffer dev (sdbBuffer sdb) Nothing
                    freeMemory dev (sdbMemory sdb) Nothing
                Nothing → pure ()
        Nothing → logDebugM CatSystem "No Vulkan device found, skipping buffer cleanup"
    
    -- GLFW cleanup (windowed modes only)
    forM_ mWindow $ \(Window win) → do
        logDebugM CatSystem "Cleaning up GLFW..."
        liftIO $ GLFW.postEmptyEvent
        GLFW.setWindowShouldClose win True
        liftIO $ clearGLFWCallbacks win

    -- Shutdown threads
    env ← ask

    logDebugM CatSystem "Shutting down unit thread..."
    liftIO $ forM_ mUnitThreadState shutdownThread

    logDebugM CatSystem "Shutting down world thread..."
    liftIO $ forM_ mWorldThreadState shutdownThread

    logDebugM CatSystem "Shutting down input thread..."
    liftIO $ shutdownThread inputThreadState
    
    logDebugM CatSystem "Shutting down Lua thread..."
    liftIO $ shutdownThread luaThreadState

    -- shut down logger, then mark the engine stopped -- both are
    -- pure core-init capability (issue #889): nothing here needs
    -- graphics/window/thread state, unlike everything above it.
    logDebugM CatSystem "Shutting down logger..."
    finalizeCoreShutdown (toCoreCapability env)

    logDebugM CatSystem "Engine shutdown complete"

-- | Flush the logger and mark the engine lifecycle stopped -- the
--   'core-init'-only tail of 'shutdownEngine', narrowed to
--   'CoreCapability' rather than the full 'EngineEnv' (issue #889).
finalizeCoreShutdown ∷ MonadIO m ⇒ CoreCapability → m ()
finalizeCoreShutdown cap = liftIO $ do
  logger ← readIORef (ccLoggerRef cap)
  shutdownLogger logger
  writeIORef (ccLifecycleRef cap) EngineStopped

-- | Final continuation for 'runEngineM': pass the result through
--   unchanged. Error handling (thread shutdown, logger flush, failure
--   exit code) lives in each main's @Left@ branch — exiting here,
--   inside the CPS continuation, would make those branches
--   unreachable (they were dead code while this called exitFailure,
--   which also lost any buffered log lines on engine errors).
checkStatus ∷ Either EngineException () → IO (Either EngineException ())
checkStatus = pure
