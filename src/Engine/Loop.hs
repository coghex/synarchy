{-# LANGUAGE CPP #-}
module Engine.Loop
  ( mainLoop
  , drawFrame
  , shutdownEngine
  , checkStatus
  ) where

import UPrelude
import Control.Concurrent (threadDelay)
import Control.Exception (displayException)
import qualified Data.Text as T
import qualified Data.Vector as V
import Data.IORef (readIORef, writeIORef, atomicModifyIORef')
import Data.Time.Clock (getCurrentTime, utctDayTime)
import Linear (identity)
import System.Exit (exitFailure)
import Engine.Core.Defaults
import Engine.Core.Monad
import Engine.Core.State
import Engine.Core.Thread (ThreadState, shutdownThread)
import Engine.Core.Error.Exception
import qualified Engine.Core.Queue as Q
import Engine.Graphics.Base
import Engine.Graphics.Camera
import Engine.Graphics.Font.Draw (cleanupPendingInstanceBuffers)
import Engine.Graphics.Window.Types (Window(..))
import Engine.Graphics.Types (DevQueues(..), SwapchainInfo(..))
import qualified Engine.Graphics.Window.GLFW as GLFW
import Engine.Graphics.Vulkan.Buffer
import Engine.Graphics.Vulkan.Command
import Engine.Graphics.Vulkan.Types
import Engine.Graphics.Vulkan.Types.Descriptor
import Engine.Input.Callback (clearGLFWCallbacks)
import Engine.Input.Event (handleInputEvents)
import Engine.Input.Types (InputState(..))
import Engine.Scene.Render (updateSceneForRender, getCurrentRenderBatches
                           , ensureDynamicVertexBuffer, uploadBatchesToBuffer)
import Engine.Scene.Types
import Engine.Scripting.Lua.Message (processLuaMessages)
import Vulkan.Core10
import Vulkan.Zero
import Vulkan.CStruct.Extends
import Vulkan.Extensions.VK_KHR_swapchain
import GHC.Stack (HasCallStack)

-- | Main engine loop
mainLoop ∷ EngineM ε σ ()
mainLoop = do
    env ← ask
    state ← gets graphicsState
    lifecycle ← liftIO $ readIORef (lifecycleRef env)

    case lifecycle of
        EngineStarting → handleEngineStarting env
        EngineRunning  → handleEngineRunning
        CleaningUp     → logDebug "Engine is cleaning up"
        EngineStopped  → logDebug "Engine has stopped"

-- | Handle engine starting state
handleEngineStarting ∷ EngineEnv → EngineM ε σ ()
handleEngineStarting env = do
    liftIO $ threadDelay 100000
    -- Clear the input queue before entering main loop
    flushed ← liftIO $ Q.flushQueue (inputQueue env)
    when (not $ null flushed) $
        logDebug $ "Unexpected inputs during start: " ⧺ show flushed
    liftIO $ writeIORef (lifecycleRef env) EngineRunning
    mainLoop

-- | Handle engine running state
handleEngineRunning ∷ EngineM ε σ ()
handleEngineRunning = do
    state ← gets graphicsState
    window ← case glfwWindow state of
        Nothing → throwSystemError (GLFWError "main loop") "No window"
        Just w  → pure w
    
    let Window glfwWin = window
    
    -- Frame timing
    updateFrameTiming
    
    -- Process events
    GLFW.pollEvents
    handleInputEvents
    processLuaMessages
    
    -- Check for shutdown
    shouldClose ← GLFW.windowShouldClose glfwWin
    env ← ask
    lifecycle ← liftIO $ readIORef (lifecycleRef env)
    
    if shouldClose || lifecycle ≢ EngineRunning
        then do
            logInfo "Engine shutting down..."
            liftIO $ writeIORef (lifecycleRef env) CleaningUp
        else unless (lifecycle ≡ CleaningUp) $ do
            drawFrame
            mainLoop

-- | Update frame timing and FPS tracking
updateFrameTiming ∷ EngineM ε σ ()
updateFrameTiming = do
    currentTime ← liftIO getCurTime
    tstate ← gets timingState
    
    let lastTime  = lastFrameTime tstate
        accum     = frameTimeAccum tstate
        tFps      = targetFPS tstate
        frameTime = currentTime - lastTime
        targetFrameTime = if tFps > 0.0 then 1.0 / tFps else 1.0 / 60.0
        systemOverhead = 0.0002

    -- Sleep if ahead of schedule
    when (frameTime < targetFrameTime) $ do
        let sleepTime = targetFrameTime - frameTime - systemOverhead
        when (sleepTime > 0) $
            liftIO $ threadDelay $ floor (sleepTime * 1000000)

    -- Update timing state
    actualCurrentTime ← liftIO getCurTime
    let actualFrameTime = actualCurrentTime - lastTime
        newAccum = accum + actualFrameTime

    modify $ \s → s { timingState = (timingState s)
        { lastFrameTime = actualCurrentTime
        , frameTimeAccum = newAccum
        , frameCount = frameCount (timingState s) + 1
        , deltaTime = actualFrameTime
        , currentTime = actualCurrentTime 
        } }

    -- FPS logging and adjustment (every second)
    when (newAccum ≥ 1.0) $ do
        tstate' ← gets timingState
        let currentCount = frameCount tstate'
            fps = if newAccum > 0 then fromIntegral currentCount / newAccum else 0.0
        
        -- Adjust target FPS if needed
        when (fps < 59.0) $
            modify $ \s → s { timingState = (timingState s) {
                                targetFPS = targetFPS (timingState s) + 0.1 } }
        when (fps > 61.0) $
            modify $ \s → s { timingState = (timingState s) {
                                targetFPS = targetFPS (timingState s) - 0.1 } }
        
        -- Reset accumulators
        modify $ \s → s { timingState = (timingState s)
            { frameTimeAccum = 0.0
            , frameCount = 0 
            } }

-- | Get current time as Double
getCurTime ∷ IO Double
getCurTime = realToFrac . utctDayTime <$> getCurrentTime

-- | Draw a single frame
drawFrame ∷ EngineM ε σ ()
drawFrame = do
    state ← gets graphicsState
    
    -- Get window
    Window win ← case extractWindow state of
        Left err → throwSystemError (GLFWError "drawFrame") $ T.pack $
            "No window: " ⧺ displayException err
        Right w → pure w
    
    let frameIdx = currentFrame state
    
    -- Update scene
    updateSceneForRender
    state' ← gets graphicsState
    sceneMgr ← gets sceneManager
    let layeredBatches = bmLayeredBatches $ smBatchManager sceneMgr
    batches ← getCurrentRenderBatches
    
    -- Update uniform buffer
    updateUniformBufferForFrame win frameIdx
    
    -- Prepare dynamic vertex buffer
    let totalVertices = V.sum $ V.map (fromIntegral . V.length . rbVertices) batches
    dynamicBuffer ← if totalVertices > 0
        then do
            buffer ← ensureDynamicVertexBuffer totalVertices
            uploadBatchesToBuffer batches buffer
        else ensureDynamicVertexBuffer 6
    
    -- Validate resources
    state'' ← gets graphicsState
    validateDescriptorState state''
    
    -- Get frame resources
    resources ← getFrameResources state'' frameIdx
    cmdBuffer ← getCommandBuffer resources
    device ← getDevice state''
    
    -- Wait for previous frame
    liftIO $ do
        waitForFences device (V.singleton (frInFlight resources)) True maxTimeout
        resetFences device (V.singleton (frInFlight resources))
    
    cleanupPendingInstanceBuffers
    
    -- Acquire next image
    swapchain ← getSwapchain state''
    (acquireResult, imageIndex) ← liftIO $ 
        acquireNextImageKHR device swapchain maxTimeout (frImageAvailable resources) zero
    
    when (acquireResult ≠ SUCCESS && acquireResult ≠ SUBOPTIMAL_KHR) $
        throwGraphicsError SwapchainError $
            T.pack $ "Failed to acquire image: " ⧺ show acquireResult
    
    -- Record and submit command buffer
    liftIO $ resetCommandBuffer cmdBuffer zero
    recordSceneCommandBuffer cmdBuffer (fromIntegral imageIndex) dynamicBuffer layeredBatches
    
    queues ← getQueues state''
    submitFrame cmdBuffer resources queues
    presentFrame resources queues swapchain imageIndex
    
    -- Update frame index
    modify $ \s → s { graphicsState = (graphicsState s) {
        currentFrame = (currentFrame (graphicsState s) + 1)
                       `mod` fromIntegral (gcMaxFrames defaultGraphicsConfig) } }

-- | Update uniform buffer for current frame
updateUniformBufferForFrame ∷ GLFW.Window → Word32 → EngineM ε σ ()
updateUniformBufferForFrame win frameIdx = do
    state ← gets graphicsState
    case (vulkanDevice state, uniformBuffers state) of
        (Just device, Just buffers) → do
            let (_, memory) = buffers V.! fromIntegral frameIdx
            (fbWidth, fbHeight) ← GLFW.getFramebufferSize win
            (winWidth, winHeight) ← GLFW.getWindowSize win
            
            env ← ask
            camera ← liftIO $ readIORef (cameraRef env)
            
            -- Update UI camera to use framebuffer dimensions
            let uiCamera = UICamera (fromIntegral fbWidth) (fromIntegral fbHeight)
            let uiProjMatrix = createUIProjectionMatrix uiCamera
            let uboData = UBO identity (createViewMatrix camera)
                              (createProjectionMatrix camera 
                                  (fromIntegral fbWidth) (fromIntegral fbHeight))
                              (createUIViewMatrix uiCamera)
                              (createUIProjectionMatrix uiCamera)
            
            liftIO $ writeIORef (cameraRef env) camera
            liftIO $ atomicModifyIORef' (inputStateRef env) $ \is →
                (is { inpWindowSize = (winWidth, winHeight)
                    , inpFramebufferSize = (fbWidth, fbHeight) }, ())
            
            updateUniformBuffer device memory uboData
        _ → throwGraphicsError VulkanDeviceLost "No device or uniform buffer"

-- | Submit frame to GPU
submitFrame ∷ CommandBuffer → FrameResources → DevQueues → EngineM ε σ ()
submitFrame cmdBuffer resources queues = do
    let waitStages = V.singleton PIPELINE_STAGE_COLOR_ATTACHMENT_OUTPUT_BIT
        submitInfo = zero 
            { waitSemaphores = V.singleton $ frImageAvailable resources
            , waitDstStageMask = waitStages
            , commandBuffers = V.singleton $ commandBufferHandle cmdBuffer
            , signalSemaphores = V.singleton $ frRenderFinished resources
            }
    liftIO $ queueSubmit (graphicsQueue queues) 
               (V.singleton $ SomeStruct submitInfo) (frInFlight resources)

-- | Present frame
presentFrame ∷ FrameResources → DevQueues → SwapchainKHR → Word32 → EngineM ε σ ()
presentFrame resources queues swapchain imageIndex = do
    let presentInfo = zero
            { waitSemaphores = V.singleton $ frRenderFinished resources
            , swapchains = V.singleton swapchain
            , imageIndices = V.singleton imageIndex
            }
    presentResult ← liftIO $ queuePresentKHR (presentQueue queues) presentInfo
    case presentResult of
        SUCCESS        → pure ()
        SUBOPTIMAL_KHR → pure ()
        err → throwGraphicsError SwapchainError $
                T.pack $ "Failed to present: " ⧺ show err

-- Helper functions for resource extraction
-- All use EngineM ε σ (polymorphic) so they can be called from any context

validateDescriptorState ∷ GraphicsState → EngineM ε σ ()
validateDescriptorState state = case descriptorState state of
    Nothing → throwGraphicsError DescriptorError "No descriptor manager"
    Just dm → when (V.null $ dmActiveSets dm) $
        throwGraphicsError DescriptorError "No active descriptor sets"

getFrameResources ∷ GraphicsState → Word32 → EngineM ε σ FrameResources
getFrameResources state frameIdx = 
    case safeVectorIndex (frameResources state) (fromIntegral frameIdx) of
        Nothing → throwGraphicsError CommandBufferError $
            "Frame index out of bounds: " <> T.pack (show frameIdx)
        Just res → pure res

getCommandBuffer ∷ FrameResources → EngineM ε σ CommandBuffer
getCommandBuffer resources = 
    case safeVectorHead (frCommandBuffer resources) of
        Nothing → throwGraphicsError CommandBufferError "No command buffer"
        Just cb → pure cb

getDevice ∷ GraphicsState → EngineM ε σ Device
getDevice state = case vulkanDevice state of
    Nothing → throwGraphicsError VulkanDeviceLost "No device"
    Just d  → pure d

getSwapchain ∷ GraphicsState → EngineM ε σ SwapchainKHR
getSwapchain state = case swapchainInfo state of
    Nothing → throwGraphicsError SwapchainError "No swapchain"
    Just si → pure $ siSwapchain si

getQueues ∷ GraphicsState → EngineM ε σ DevQueues
getQueues state = case deviceQueues state of
    Nothing → throwGraphicsError VulkanDeviceLost "No queues"
    Just q  → pure q

extractWindow ∷ HasCallStack ⇒ GraphicsState → Either EngineException Window
extractWindow state = case glfwWindow state of
    Nothing  → Left $ EngineException
                  (ExSystem (GLFWError "drawFrame"))
                  "No GLFW window initialized"
                  mkErrorContext
    Just win → Right win

-- | Safe vector access
safeVectorIndex ∷ V.Vector a → Int → Maybe a
safeVectorIndex vec idx
  | idx >= 0 && idx < V.length vec = Just (vec V.! idx)
  | otherwise = Nothing

safeVectorHead ∷ V.Vector a → Maybe a
safeVectorHead vec
  | V.null vec = Nothing
  | otherwise  = Just (V.head vec)

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
