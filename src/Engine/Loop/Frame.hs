{-# LANGUAGE CPP #-}
module Engine.Loop.Frame
  ( drawFrame
  , updateUniformBufferForFrame
  , submitFrame
  ) where

import UPrelude
import Control.Exception (displayException)
import qualified Data.Text as T
import qualified Data.Vector as V
import qualified Data.Map as Map
import Data.IORef (readIORef, atomicModifyIORef', writeIORef)
import Linear (identity)
import Engine.Core.Defaults
import Engine.Core.Log (LogCategory(..))
import Engine.Core.Log.Monad (logDebugM, logDebugSM, logInfoM, logWarnM, logAndThrowM)
import Engine.Core.Monad
import Engine.Core.State
import Engine.Core.Error.Exception (ExceptionType(..), GraphicsError(..)
                                   , SystemError(..))
import Engine.Graphics.Base
import Engine.Graphics.Camera
import Engine.Graphics.Font.Draw (cleanupPendingInstanceBuffers)
import Engine.Graphics.Window.Types (Window(..))
import Engine.Graphics.Types (DevQueues(..), SwapchainInfo(..))
import qualified Engine.Graphics.Window.GLFW as GLFW
import Engine.Graphics.Vulkan.Buffer
import Engine.Graphics.Vulkan.Command
import Engine.Graphics.Vulkan.Recreate (recreateSwapchain)
import Engine.Graphics.Vulkan.Types
import Engine.Graphics.Vulkan.Types.Descriptor
import Engine.Input.Types (InputState(..))
import Engine.Loop.Resource (validateDescriptorState, getFrameResources, 
                              getCommandBuffer, getDevice, getSwapchain,
                              getQueues, extractWindow)
import Engine.Scene.Render (updateSceneForRender, getCurrentRenderBatches
                           , ensureDynamicVertexBuffer, uploadBatchesToBuffer)
import Engine.Scene.Types
import UI.Render (renderUIPages)
import Vulkan.Core10
import Vulkan.Zero
import Vulkan.CStruct.Extends
import Vulkan.Extensions.VK_KHR_swapchain hiding (acquireNextImageKHRSafe)
import GHC.Stack (HasCallStack)

-- | Draw a single frame
drawFrame ∷ EngineM ε σ ()
drawFrame = do
    state ← gets graphicsState
    
    -- Get window
    window@(Window win) ← case extractWindow state of
        Left err → logAndThrowM CatSystem
          (ExSystem (GLFWError "drawFrame: ")) $ T.pack $
            "No window: " ⧺ displayException err
        Right w → pure w
    
    let frameIdx = currentFrame state
    
    logDebugSM CatRender "Beginning frame render" 
        [("frame", T.pack $ show frameIdx)]
    
    -- Get frame resources and device early
    resources ← getFrameResources state frameIdx
    device ← getDevice state
    
    -- Wait for this frame's previous work to complete
    logDebugM CatRender "Waiting for previous frame fence..."
    liftIO $ waitForFences device (V.singleton (frInFlight resources)) True maxBound
    
    -- Try to acquire image BEFORE resetting fence
    swapchain ← getSwapchain state
    logDebugM CatRender "Acquiring swapchain image..."
    acquireResult ← liftIO $ acquireNextImageKHRSafe device swapchain maxBound
                                 (frImageAvailable resources) zero
    
    case acquireResult of
        Left ERROR_OUT_OF_DATE_KHR → do
            logInfoM CatGraphics "Swapchain out of date on acquire, recreating..."
            recreateSwapchain window
            -- Fence is still signaled, next frame will wait and it will return immediately
        
        Left SUBOPTIMAL_KHR → do
            logDebugM CatGraphics "Swapchain suboptimal on acquire, recreating..."
            recreateSwapchain window
            -- Fence is still signaled, next frame will wait and it will return immediately
        
        Left err → logAndThrowM CatGraphics
          (ExGraphics SwapchainError) $ T.pack $
            "Failed to acquire swapchain image: " ⧺ show err
        
        Right imageIndex → do
            logDebugSM CatRender "Acquired swapchain image"
                [("imageIndex", T.pack $ show imageIndex)]
            
            -- NOW reset fence since we're definitely going to submit
            liftIO $ resetFences device (V.singleton (frInFlight resources))
            
            -- Update scene
            logDebugM CatRender "Updating scene for render..."
            updateSceneForRender
            sceneMgr ← gets sceneManager
            let worldLayeredBatches = bmLayeredBatches $ smBatchManager sceneMgr
            worldBatches ← getCurrentRenderBatches
            
            logDebugSM CatRender "Collected world batches"
                [("count", T.pack $ show $ V.length worldBatches)
                ,("layers", T.pack $ show $ Map.size worldLayeredBatches)]
            
            -- render UI
            logDebugM CatUI "Rendering UI pages..."
            (uiBatches, uiLayeredBatches) ← renderUIPages
            
            logDebugSM CatUI "Collected UI batches"
                [("count", T.pack $ show $ V.length uiBatches)
                ,("layers", T.pack $ show $ Map.size uiLayeredBatches)]
            
            -- merge world and UI batches
            let batches = worldBatches <> uiBatches
                layeredBatches = Map.unionWith (<>) worldLayeredBatches uiLayeredBatches
            
            logDebugSM CatRender "Total render batches"
                [("total", T.pack $ show $ V.length batches)
                ,("totalLayers", T.pack $ show $ Map.size layeredBatches)]
            
            -- Update uniform buffer
            logDebugM CatRender "Updating uniform buffer..."
            updateUniformBufferForFrame win frameIdx
            
            -- Prepare dynamic vertex buffer
            let totalVertices = V.sum $ V.map (fromIntegral . V.length . rbVertices) batches
            
            logDebugSM CatRender "Preparing vertex buffer"
                [("vertices", T.pack $ show totalVertices)]
            
            dynamicBuffer ← if totalVertices > 0
                then do
                    buffer ← ensureDynamicVertexBuffer totalVertices
                    uploadBatchesToBuffer batches buffer
                else ensureDynamicVertexBuffer 6
            
            -- Validate resources
            state'' ← gets graphicsState
            validateDescriptorState state''
            
            logDebugM CatRender "Cleaning up pending instance buffers..."
            cleanupPendingInstanceBuffers
            
            -- Record command buffer
            logDebugM CatRender "Recording command buffer..."
            cmdBuffer ← getCommandBuffer resources
            liftIO $ resetCommandBuffer cmdBuffer zero
            recordSceneCommandBuffer cmdBuffer (fromIntegral imageIndex) 
                                     dynamicBuffer layeredBatches
            
            -- Submit
            logDebugM CatRender "Submitting frame to GPU..."
            queues ← getQueues state''
            submitFrame cmdBuffer resources queues
            
            -- Present - get fresh swapchain from state
            logDebugM CatRender "Presenting frame..."
            currentState ← gets graphicsState
            currentSwapchain ← getSwapchain currentState
            presentResult ← presentFrameWithResult resources queues currentSwapchain imageIndex
            
            case presentResult of
                Left ERROR_OUT_OF_DATE_KHR → do
                    logInfoM CatGraphics
                      "Swapchain out of date on present, recreating..."
                    recreateSwapchain window
                
                Left SUBOPTIMAL_KHR → do
                    logInfoM CatGraphics
                      "Swapchain suboptimal on present, recreating..."
                    recreateSwapchain window
                
                Left err → logAndThrowM CatGraphics
                  (ExGraphics SwapchainError) $ T.pack $
                    "Failed to present: " ⧺ show err
                
                Right () → 
                    logDebugM CatRender "Frame presented successfully"
            
            -- Update frame index
            let nextFrame = (frameIdx + 1) `mod` fromIntegral (gcMaxFrames defaultGraphicsConfig)
            modify $ \s → s { graphicsState = (graphicsState s) {
                currentFrame = nextFrame } }
            
            logDebugSM CatRender "Frame complete"
                [("nextFrame", T.pack $ show nextFrame)]

-- | Update uniform buffer for current frame
updateUniformBufferForFrame ∷ GLFW.Window → Word32 → EngineM ε σ ()
updateUniformBufferForFrame win frameIdx = do
    state ← gets graphicsState
    case (vulkanDevice state, uniformBuffers state) of
        (Just device, Just buffers) → do
            let (_, memory) = buffers V.! fromIntegral frameIdx
            (fbWidth, fbHeight) ← GLFW.getFramebufferSize win
            (winWidth, winHeight) ← GLFW.getWindowSize win

            -- Before writing
            env ← ask
            old ← liftIO $ readIORef (framebufferSizeRef env)
            
            logDebugSM CatRender "Updating uniform buffer"
                [("frame", T.pack $ show frameIdx)
                ,("fbSize", T.pack (show fbWidth) <> "x" <> T.pack (show fbHeight))
                ,("winSize", T.pack (show winWidth) <> "x" <> T.pack (show winHeight))]
            
            camera ← liftIO $ readIORef (cameraRef env)
            
            -- Update UI camera to use framebuffer dimensions
            let uiCamera = UICamera (fromIntegral fbWidth) (fromIntegral fbHeight)
            let uboData = UBO identity (createViewMatrix camera)
                              (createProjectionMatrix camera 
                                  (fromIntegral fbWidth) (fromIntegral fbHeight))
                              (createUIViewMatrix uiCamera)
                              (createUIProjectionMatrix uiCamera)
            
            liftIO $ writeIORef (windowSizeRef env) (winWidth, winHeight)
            liftIO $ writeIORef (framebufferSizeRef env) (fbWidth, fbHeight)
            
            updateUniformBuffer device memory uboData
        _ → logAndThrowM CatGraphics
          (ExGraphics VulkanDeviceLost) $
            T.pack "No device or uniform buffer"

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
    logDebugM CatRender "Queue submit..."
    liftIO $ queueSubmit (graphicsQueue queues) 
               (V.singleton $ SomeStruct submitInfo) (frInFlight resources)

-- | Present frame and return result for handling
presentFrameWithResult ∷ FrameResources → DevQueues → SwapchainKHR → Word32 
                       → EngineM ε σ (Either Result ())
presentFrameWithResult resources queues swapchain imageIndex = do
    let presentInfo = zero
            { waitSemaphores = V.singleton $ frRenderFinished resources
            , swapchains = V.singleton swapchain
            , imageIndices = V.singleton imageIndex
            }
    presentResult ← liftIO $ queuePresentKHR (presentQueue queues) presentInfo
    case presentResult of
        SUCCESS               → pure $ Right ()
        SUBOPTIMAL_KHR        → pure $ Left SUBOPTIMAL_KHR
        ERROR_OUT_OF_DATE_KHR → pure $ Left ERROR_OUT_OF_DATE_KHR
        err                   → pure $ Left err

-- | Safe wrapper for acquireNextImageKHR
acquireNextImageKHRSafe ∷ Device → SwapchainKHR → Word64 → Semaphore → Fence 
                        → IO (Either Result Word32)
acquireNextImageKHRSafe device swapchain timeout semaphore fence = do
    (result, imageIndex) ← acquireNextImageKHR device swapchain timeout semaphore fence
    case result of
        SUCCESS               → pure $ Right imageIndex
        SUBOPTIMAL_KHR        → pure $ Left SUBOPTIMAL_KHR
        ERROR_OUT_OF_DATE_KHR → pure $ Left ERROR_OUT_OF_DATE_KHR
        err                   → pure $ Left err
