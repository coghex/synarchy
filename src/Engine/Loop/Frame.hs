{-# LANGUAGE CPP #-}
module Engine.Loop.Frame
  ( drawFrame
  , updateUniformBufferForFrame
  , submitFrame
  , presentFrame
  ) where

import UPrelude
import Control.Exception (displayException)
import qualified Data.Text as T
import qualified Data.Vector as V
import Data.IORef (readIORef, atomicModifyIORef', writeIORef)
import Linear (identity)
import Engine.Core.Defaults
import Engine.Core.Monad
import Engine.Core.State
import Engine.Core.Error.Exception
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
import Engine.Input.Types (InputState(..))
import Engine.Loop.Resource (validateDescriptorState, getFrameResources, 
                              getCommandBuffer, getDevice, getSwapchain,
                              getQueues, extractWindow)
import Engine.Scene.Render (updateSceneForRender, getCurrentRenderBatches
                           , ensureDynamicVertexBuffer, uploadBatchesToBuffer)
import Engine.Scene.Types
import Vulkan.Core10
import Vulkan.Zero
import Vulkan.CStruct.Extends
import Vulkan.Extensions.VK_KHR_swapchain
import GHC.Stack (HasCallStack)

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
            let uboData = UBO identity (createViewMatrix camera)
                              (createProjectionMatrix camera 
                                  (fromIntegral fbWidth) (fromIntegral fbHeight))
                              (createUIViewMatrix uiCamera)
                              (createUIProjectionMatrix uiCamera)
            
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
