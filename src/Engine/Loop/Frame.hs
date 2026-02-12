{-# LANGUAGE CPP, UnicodeSyntax #-}
module Engine.Loop.Frame
  ( drawFrame
  , updateUniformBufferForFrame
  , submitFrame
  , computeAmbientLight
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
import Engine.Graphics.Config (brightnessToMultiplier)
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
import Engine.Scene.Render
import Engine.Scene.Base
import Engine.Scene.Types
import Engine.Scene.Types.Batch
import UI.Render (renderUIPages)
import World.Render (updateWorldTiles)
import World.Grid (worldLayer)
import Vulkan.Core10
import Vulkan.Zero
import Vulkan.CStruct.Extends
import Vulkan.Extensions.VK_KHR_swapchain hiding (acquireNextImageKHRSafe)
import GHC.Stack (HasCallStack)

-----------------------------------------------------------
-- Lighting
-----------------------------------------------------------

computeAmbientLight ∷ Float → Float
computeAmbientLight sunAngle =
    let angle = sunAngle * 2.0 * π
        sunHeight = sin angle
    in if sunHeight ≥ 0
       then 0.5 + 0.2 * sunHeight
       else 0.15 + 0.35 * (1.0 + sunHeight)

sunCycleSpeed ∷ Double
sunCycleSpeed = 1.0 / 600.0

-----------------------------------------------------------
-- Frame Rendering
-----------------------------------------------------------

-- | Draw a single frame
drawFrame ∷ EngineM ε σ ()
drawFrame = do
    state ← gets graphicsState
    window@(Window win) ← case extractWindow state of
        Left err → logAndThrowM CatSystem
          (ExSystem (GLFWError "drawFrame: ")) $ T.pack $
            "No window: " ⧺ displayException err
        Right w → pure w
    let frameIdx = currentFrame state
    logDebugSM CatRender "Beginning frame render" 
        [("frame", T.pack $ show frameIdx)]
    resources ← getFrameResources state frameIdx
    device ← getDevice state
    logDebugM CatRender "Waiting for previous frame fence..."
    liftIO $ waitForFences device (V.singleton (frInFlight resources)) True maxBound
    swapchain ← getSwapchain state
    logDebugM CatRender "Acquiring swapchain image..."
    acquireResult ← liftIO $ acquireNextImageKHRSafe device swapchain maxBound
                                 (frImageAvailable resources) zero
    
    case acquireResult of
        Left ERROR_OUT_OF_DATE_KHR → do
            logInfoM CatGraphics "Swapchain out of date on acquire, recreating..."
            recreateSwapchain window
        
        Left SUBOPTIMAL_KHR → do
            logDebugM CatGraphics "Swapchain suboptimal on acquire, recreating..."
            recreateSwapchain window
        
        Left err → logAndThrowM CatGraphics
          (ExGraphics SwapchainError) $ T.pack $
            "Failed to acquire swapchain image: " ⧺ show err
        
        Right imageIndex → do
            logDebugSM CatRender "Acquired swapchain image"
                [("imageIndex", T.pack $ show imageIndex)]
            liftIO $ resetFences device (V.singleton (frInFlight resources))
            worldTileQuads ← updateWorldTiles
            logDebugSM CatRender "Collected world tile quads"
                [("count", T.pack $ show $ V.length worldTileQuads)]
            logDebugM CatRender "Updating scene for render..."
            updateSceneForRender
            sceneMgr ← gets sceneManager
            sceneQuads ← getWorldSceneQuads
            logDebugSM CatRender "Collected scene quads for world layer"
                [("count", T.pack $ show $ V.length sceneQuads)]
            let allWorldQuads = V.toList worldTileQuads <> V.toList sceneQuads
                groupedByLayer = Map.fromListWith (<>)
                    [ (sqLayer q, [q]) | q ← allWorldQuads ]
                perLayerBatches = Map.mapWithKey
                    (\layer quads → mergeQuadsToBatch layer quads)
                    groupedByLayer
                worldBatches = V.fromList $ Map.elems perLayerBatches
                worldLayeredBatches = Map.map
                    (\batch → V.singleton (SpriteItem batch))
                    perLayerBatches
            logDebugM CatRender "Rendering UI pages..."
            (uiBatches, uiLayeredBatches) ← renderUIPages
            logDebugSM CatRender "Collected UI batches"
                [("count", T.pack $ show $ V.length uiBatches)
                ,("layers", T.pack $ show $ Map.size uiLayeredBatches)]
            let batches = worldBatches <> uiBatches
                layeredBatches = Map.unionsWith (<>) 
                    [worldLayeredBatches, uiLayeredBatches]
            logDebugM CatRender $ T.pack $ "Final layered batches:"
            forM_ (Map.toList layeredBatches) $ \(LayerId lid, items) → do
                let spriteCount = V.length $ V.filter (\case SpriteItem _ → True; _ → False) items
                    textCount = V.length $ V.filter (\case TextItem _ → True; _ → False) items
                logDebugM CatRender $ T.pack $ 
                    "  Layer " <> show lid <> ": " <> show (V.length items) <> 
                    " items (" <> show spriteCount <> " sprites, " <> show textCount <> " text)"
            logDebugSM CatRender "Total render batches"
                [("total", T.pack $ show $ V.length batches)
                ,("totalLayers", T.pack $ show $ Map.size layeredBatches)]
            logDebugM CatRender "Updating uniform buffer..."
            updateUniformBufferForFrame win frameIdx
            let totalVertices = V.sum $ V.map (fromIntegral . V.length . rbVertices) batches
            logDebugSM CatRender "Preparing vertex buffer"
                [("vertices", T.pack $ show totalVertices)]
            dynamicBuffer ← if totalVertices > 0
                then do
                    buffer ← ensureDynamicVertexBuffer totalVertices
                    uploadBatchesToBuffer batches buffer
                else ensureDynamicVertexBuffer 6
            state'' ← gets graphicsState
            validateDescriptorState state''
            logDebugM CatRender "Cleaning up pending instance buffers..."
            cleanupPendingInstanceBuffers
            logDebugM CatRender "Recording command buffer..."
            cmdBuffer ← getCommandBuffer resources
            liftIO $ resetCommandBuffer cmdBuffer zero
            recordSceneCommandBuffer cmdBuffer (fromIntegral imageIndex) 
                                     dynamicBuffer layeredBatches
            logDebugM CatRender "Submitting frame to GPU..."
            queues ← getQueues state''
            submitFrame cmdBuffer resources queues
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
            let nextFrame = (frameIdx + 1) `mod` fromIntegral (gcMaxFrames defaultGraphicsConfig)
            modify $ \s → s { graphicsState = (graphicsState s) {
                currentFrame = nextFrame } }
            logDebugSM CatRender "Frame complete"
                [("nextFrame", T.pack $ show nextFrame)]

-----------------------------------------------------------
-- Uniform Buffer Updates
-----------------------------------------------------------

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
            old ← liftIO $ readIORef (framebufferSizeRef env)
            logDebugSM CatRender "Updating uniform buffer"
                [("frame", T.pack $ show frameIdx)
                ,("fbSize", T.pack (show fbWidth) <> "x" <> T.pack (show fbHeight))
                ,("winSize", T.pack (show winWidth) <> "x" <> T.pack (show winHeight))]
            camera ← liftIO $ readIORef (cameraRef env)
            brightness ← liftIO $ readIORef (brightnessRef env)
            pixelSnap ← liftIO $ readIORef (pixelSnapRef env)
            sunAngle ← liftIO $ readIORef (sunAngleRef env)
            let ambientLight = computeAmbientLight sunAngle
            let uiCamera = UICamera (fromIntegral fbWidth) (fromIntegral fbHeight)
            let uboData = UBO identity (createViewMatrix camera)
                              (createProjectionMatrix camera 
                                  (fromIntegral fbWidth) (fromIntegral fbHeight))
                              (createUIViewMatrix uiCamera)
                              (createUIProjectionMatrix uiCamera)
                              (brightnessToMultiplier brightness)
                              (fromIntegral fbWidth) (fromIntegral fbHeight)
                              (if pixelSnap then 1.0 else 0.0)
                              sunAngle
                              ambientLight
            liftIO $ writeIORef (windowSizeRef env) (winWidth, winHeight)
            liftIO $ writeIORef (framebufferSizeRef env) (fbWidth, fbHeight)
            updateUniformBuffer device memory uboData
        _ → logAndThrowM CatGraphics
          (ExGraphics VulkanDeviceLost) $
            T.pack "No device or uniform buffer"

-----------------------------------------------------------
-- Frame Submission and Presentation
-----------------------------------------------------------

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

acquireNextImageKHRSafe ∷ Device → SwapchainKHR → Word64 → Semaphore → Fence 
                        → IO (Either Result Word32)
acquireNextImageKHRSafe device swapchain timeout semaphore fence = do
    (result, imageIndex) ← acquireNextImageKHR device swapchain timeout semaphore fence
    case result of
        SUCCESS               → pure $ Right imageIndex
        SUBOPTIMAL_KHR        → pure $ Left SUBOPTIMAL_KHR
        ERROR_OUT_OF_DATE_KHR → pure $ Left ERROR_OUT_OF_DATE_KHR
        err                   → pure $ Left err
