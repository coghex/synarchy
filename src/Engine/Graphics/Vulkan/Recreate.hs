{-# LANGUAGE Strict #-}
module Engine.Graphics.Vulkan.Recreate
  ( recreateSwapchain
  ) where

import UPrelude
import qualified Data.Vector as V
import qualified Data.Text as T
import Data.IORef (writeIORef)
import Engine.Core.Monad
import Engine.Core.State
import Engine.Core.Log (LogCategory(..))
import Engine.Core.Log.Monad (logDebugM, logInfoM, logAndThrowM)
import Engine.Core.Error.Exception (GraphicsError(..), ExceptionType(..))
import Engine.Graphics.Camera (UICamera(..))
import Engine.Graphics.Types
import Engine.Graphics.Window.Types (Window(..))
import qualified Engine.Graphics.Window.GLFW as GLFW
import Engine.Graphics.Vulkan.Swapchain
import Engine.Graphics.Vulkan.Framebuffer
import Engine.Graphics.Vulkan.Pipeline
import Engine.Graphics.Vulkan.Pipeline.Bindless
import Engine.Graphics.Vulkan.Types
import Engine.Graphics.Vulkan.Types.Cleanup
import Engine.Graphics.Vulkan.Types.Descriptor
import Engine.Graphics.Vulkan.Texture.Types (BindlessTextureSystem(..))
import Engine.Graphics.Font.Draw
import Vulkan.Core10
import Vulkan.Extensions.VK_KHR_surface (SurfaceKHR)

-- | Recreate the swapchain and all dependent resources
recreateSwapchain :: Window -> EngineM ε σ ()
recreateSwapchain window = do
    state <- gets graphicsState
    
    -- Extract required handles
    device <- getDeviceOrFail state
    pDevice <- getPhysicalDeviceOrFail state
    surface <- getSurfaceOrFail state
    queues <- getQueuesOrFail state
    
    -- Wait for device to be idle before destroying resources
    liftIO $ deviceWaitIdle device
    
    -- Get new window dimensions
    let Window glfwWin = window
    (width, height) <- GLFW.getFramebufferSize glfwWin
    
    -- Handle minimized window (zero size)
    if width == 0 || height == 0
        then logDebugM CatSwapchain "Window minimized, skipping swapchain recreation"
        else do
            -- Run all existing cleanup actions
            logDebugM CatSwapchain "Running cleanup before recreation..."
            liftIO $ runAllCleanups (vulkanCleanup state)
            
            -- Reset cleanup to empty (we'll rebuild it)
            modify $ \s -> s { graphicsState = (graphicsState s) {
                vulkanCleanup = emptyCleanup
            }}
            
            -- Recreate all resources
            recreateAllResources pDevice device queues surface window
            
            -- Reset frame index
            modify $ \s -> s { graphicsState = (graphicsState s) { 
                currentFrame = 0 
            }}
            
            -- Update UI camera with new dimensions
            env <- ask
            liftIO $ writeIORef (uiCameraRef env) $ 
                UICamera (fromIntegral width) (fromIntegral height)
            
            logInfoM CatSwapchain $ "Swapchain recreated: " <> (T.pack (show width)) 
                                    <> "x" <> (T.pack (show height))

-- | Recreate all swapchain-dependent resources
recreateAllResources :: PhysicalDevice -> Device -> DevQueues -> SurfaceKHR 
                     -> Window -> EngineM ε σ ()
recreateAllResources pDevice device queues surface window = do
    state <- gets graphicsState
    
    -- Get descriptor manager and texture system (these survive recreation)
    descManager <- getDescriptorManagerOrFail state
    texSystem <- getTextureSystemOrFail state
    fontDescLayout <- getFontDescriptorLayoutOrFail state
    
    let uniformLayout = dmUniformLayout descManager
        bindlessLayout = btsDescriptorLayout texSystem
    
    -- 1. Swapchain
    swapInfo <- createVulkanSwapchain pDevice device queues surface
    modify $ \s -> s { graphicsState = (graphicsState s) {
        swapchainInfo = Just swapInfo
    }}
    
    let newExtent = siSwapExtent swapInfo
    
    -- 2. Image views
    imageViews <- createSwapchainImageViews device swapInfo
    modify $ \s -> s { graphicsState = (graphicsState s) {
        swapchainInfo = case swapchainInfo (graphicsState s) of
            Just si -> Just si { siSwapImgViews = imageViews }
            Nothing -> Nothing } }
    
    -- 3. Render pass
    renderPass <- createVulkanRenderPass device (siSwapImgFormat swapInfo)
    modify $ \s -> s { graphicsState = (graphicsState s) {
        vulkanRenderPass = Just renderPass
    }}
    
    -- 4. Framebuffers
    framebuffers <- createVulkanFramebuffers device renderPass swapInfo imageViews
    modify $ \s -> s { graphicsState = (graphicsState s) {
        framebuffers = Just framebuffers
    }}
    
    -- 5. Bindless pipeline
    (bindlessPipe, bindlessPipeLayout) <- 
        createBindlessPipeline device renderPass newExtent uniformLayout bindlessLayout
    modify $ \s -> s { graphicsState = (graphicsState s) {
        bindlessPipeline = Just (bindlessPipe, bindlessPipeLayout)
    }}
    
    -- 6. Bindless UI pipeline
    (bindlessUIPipe, bindlessUIPipeLayout) <- 
        createBindlessUIPipeline device renderPass newExtent uniformLayout bindlessLayout
    modify $ \s -> s { graphicsState = (graphicsState s) {
        bindlessUIPipeline = Just (bindlessUIPipe, bindlessUIPipeLayout)
    }}
    
    -- 7. Font pipeline
    (fontPipe, fontPipeLayout, _) <- 
        createFontPipeline device renderPass newExtent uniformLayout
    modify $ \s -> s { graphicsState = (graphicsState s) {
        fontPipeline = Just (fontPipe, fontPipeLayout)
    }}
    
    -- 8. Font UI pipeline
    (fontUIPipe, fontUIPipeLayout) <- 
        createFontUIPipeline device renderPass newExtent uniformLayout fontDescLayout
    modify $ \s -> s { graphicsState = (graphicsState s) {
        fontUIPipeline = Just (fontUIPipe, fontUIPipeLayout)
    }}
    
    logDebugM CatGraphics "All resources recreated"

-- | Helper extractors
getDeviceOrFail ∷ GraphicsState → EngineM ε σ Device
getDeviceOrFail state = case vulkanDevice state of
    Just d  → pure d
    Nothing → logAndThrowM CatGraphics (ExGraphics VulkanDeviceLost)
                 "No device"

getPhysicalDeviceOrFail ∷ GraphicsState → EngineM ε σ PhysicalDevice
getPhysicalDeviceOrFail state = case vulkanPDevice state of
    Just pd → pure pd
    Nothing → logAndThrowM CatGraphics (ExGraphics VulkanDeviceLost)
                 "No physical device"

getSurfaceOrFail ∷ GraphicsState → EngineM ε σ SurfaceKHR
getSurfaceOrFail state = case vulkanSurface state of
    Just s  → pure s
    Nothing → logAndThrowM CatGraphics (ExGraphics VulkanDeviceLost)
                 "No surface"

getQueuesOrFail ∷ GraphicsState → EngineM ε σ DevQueues
getQueuesOrFail state = case deviceQueues state of
    Just q  → pure q
    Nothing → logAndThrowM CatGraphics (ExGraphics VulkanDeviceLost)
                 "No device queues"

getDescriptorManagerOrFail ∷ GraphicsState → EngineM ε σ DescriptorManager
getDescriptorManagerOrFail state = case descriptorState state of
    Just dm → pure dm
    Nothing → logAndThrowM CatDescriptor (ExGraphics DescriptorError)
                 "No descriptor manager"

getTextureSystemOrFail ∷ GraphicsState → EngineM ε σ BindlessTextureSystem
getTextureSystemOrFail state = case textureSystem state of
    Just ts → pure ts
    Nothing → logAndThrowM CatTexture (ExGraphics TextureLoadFailed)
                 "No texture system"

getFontDescriptorLayoutOrFail ∷ GraphicsState → EngineM ε σ DescriptorSetLayout
getFontDescriptorLayoutOrFail state = case fontDescriptorLayout state of
    Just fdl → pure fdl
    Nothing  → logAndThrowM CatDescriptor (ExGraphics FontError)
                 "No font descriptor layout"
