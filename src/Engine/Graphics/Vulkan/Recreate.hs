{-# LANGUAGE Strict #-}
module Engine.Graphics.Vulkan.Recreate
  ( recreateSwapchain
  , handleOutOfDateSwapchain
  ) where

import UPrelude
import qualified Data.Vector as V
import qualified Data.Text as T
import Data.IORef (writeIORef)
import Engine.Core.Monad
import Engine.Core.State
import Engine.Core.Defaults (defaultGraphicsConfig)
import Engine.Core.Error.Exception
import Engine.Graphics.Camera (defaultUICamera)
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
import Vulkan.Extensions.VK_KHR_swapchain

-- | Handle out-of-date swapchain during frame acquisition/presentation
handleOutOfDateSwapchain ∷ Result → Window → EngineM ε σ Bool
handleOutOfDateSwapchain result window = case result of
    ERROR_OUT_OF_DATE_KHR → do
        logInfo "Swapchain out of date, recreating..."
        recreateSwapchain window
        pure True
    SUBOPTIMAL_KHR → do
        logDebug "Swapchain suboptimal, recreating..."
        recreateSwapchain window
        pure True
    SUCCESS → pure False
    _ → pure False

-- | Recreate the entire swapchain and dependent resources
recreateSwapchain ∷ Window → EngineM ε σ ()
recreateSwapchain window = do
    state ← gets graphicsState
    
    -- Extract required handles
    device ← getDeviceOrFail state
    pDevice ← getPhysicalDeviceOrFail state
    surface ← getSurfaceOrFail state
    queues ← getQueuesOrFail state
    
    -- Wait for device to be idle before destroying resources
    liftIO $ deviceWaitIdle device
    
    -- Get new window dimensions
    let Window glfwWin = window
    (width, height) ← GLFW.getFramebufferSize glfwWin
    
    -- Handle minimized window (zero size)
    if width ≡ 0 ∨ height ≡ 0
        then logDebug "Window minimized, skipping swapchain recreation"
        else do
            -- === RUN OLD CLEANUP ACTIONS (reverse order of creation) ===
            -- Use pattern matching and sequence the EngineM actions:
            case swapchainCleanup state of
                    Just sc → liftIO $ do
                        let SwapchainCleanup { scSwapchain, scImageViews, scRenderPass
                                 , scFramebuffers, scBindless, scBindlessUI
                                 , scFont, scFontUI } = sc
                        scFontUI
                        scFont
                        scBindlessUI
                        scBindless
                        scFramebuffers
                        scRenderPass
                        scImageViews
                        scSwapchain
                    _                   → throwGraphicsError VulkanDeviceLost "No swapchain cleanup actions"
            --
            -- === CREATE NEW RESOURCES ===
            
            -- 1. Swapchain
            (newSwapInfo, cleanupSwapchain) ← 
                createVulkanSwapchain pDevice device queues surface
            modify $ \s → s { graphicsState = (graphicsState s) 
                                { swapchainInfo = Just newSwapInfo } }
            
            -- 2. Image views
            (newImageViews, cleanupImageViews) ← 
                createSwapchainImageViews device newSwapInfo
            modify $ \s → s { graphicsState = (graphicsState s) 
                                { swapchainImageViews = Just newImageViews } }
            
            -- 3. Render pass
            (newRenderPass, cleanupRenderPass) ← 
                createVulkanRenderPass device (siSwapImgFormat newSwapInfo)
            modify $ \s → s { graphicsState = (graphicsState s) 
                                { vulkanRenderPass = Just newRenderPass } }
            
            -- 4. Framebuffers
            let newExtent = siSwapExtent newSwapInfo
            (newFramebuffers, cleanupFramebuffers) ← 
                createVulkanFramebuffers device newRenderPass 
                                         newSwapInfo newImageViews
            modify $ \s → s { graphicsState = (graphicsState s) 
                                { framebuffers = Just newFramebuffers } }
            
            -- 5. Pipelines
            state' ← gets graphicsState
            descManager ← getDescriptorManagerOrFail state'
            texSystem ← getTextureSystemOrFail state'
            fontDescLayout ← getFontDescriptorLayoutOrFail state'
            
            let uniformLayout = dmUniformLayout descManager
                bindlessLayout = btsDescriptorLayout texSystem
            
            ((newBindless, newBindlessLayout), cleanupBindless) ← 
                createBindlessPipeline device newRenderPass newExtent 
                                                   uniformLayout bindlessLayout
            modify $ \s → s { graphicsState = (graphicsState s) 
                                { bindlessPipeline = Just (newBindless, newBindlessLayout) } }
            
            ((newBindlessUI, newBindlessUILayout), cleanupBindlessUI) ← 
                createBindlessUIPipeline device newRenderPass newExtent 
                                                     uniformLayout bindlessLayout
            modify $ \s → s { graphicsState = (graphicsState s) 
                                { bindlessUIPipeline = Just (newBindlessUI, newBindlessUILayout) } }
            
            ((newFont, newFontLayout, _), cleanupFont) ← 
                createFontPipeline device newRenderPass newExtent fontDescLayout
            modify $ \s → s { graphicsState = (graphicsState s) 
                                { fontPipeline = Just (newFont, newFontLayout) } }
            
            ((newFontUI, newFontUILayout), cleanupFontUI) ← 
                createFontUIPipeline device newRenderPass newExtent 
                                                 uniformLayout fontDescLayout
            modify $ \s → s { graphicsState = (graphicsState s) 
                                { fontUIPipeline = Just (newFontUI, newFontUILayout) } }
            
            -- Store new cleanup actions
            let newCleanup = SwapchainCleanup
                  { scSwapchain    = cleanupSwapchain
                  , scImageViews   = cleanupImageViews
                  , scRenderPass   = cleanupRenderPass
                  , scFramebuffers = cleanupFramebuffers
                  , scBindless     = cleanupBindless
                  , scBindlessUI   = cleanupBindlessUI
                  , scFont         = cleanupFont
                  , scFontUI       = cleanupFontUI
                  }
            modify $ \s → s { graphicsState = (graphicsState s)
                                { swapchainCleanup = Just newCleanup } }
            
            -- Reset frame index
            modify $ \s → s { graphicsState = (graphicsState s) { currentFrame = 0 } }
            
            -- Update UI camera with new dimensions
            env ← ask
            liftIO $ writeIORef (uiCameraRef env) $ 
                defaultUICamera (fromIntegral width) (fromIntegral height)
            
            logInfo $ "Swapchain recreated: " ⧺ (show width) ⧺ "x" ⧺ (show height)

-- | Helper extractors
getDeviceOrFail ∷ GraphicsState → EngineM ε σ Device
getDeviceOrFail state = case vulkanDevice state of
    Just d  → pure d
    Nothing → throwGraphicsError VulkanDeviceLost "No device"

getPhysicalDeviceOrFail ∷ GraphicsState → EngineM ε σ PhysicalDevice
getPhysicalDeviceOrFail state = case vulkanPDevice state of
    Just pd → pure pd
    Nothing → throwGraphicsError VulkanDeviceLost "No physical device"

getSurfaceOrFail ∷ GraphicsState → EngineM ε σ SurfaceKHR
getSurfaceOrFail state = case vulkanSurface state of
    Just s  → pure s
    Nothing → throwGraphicsError VulkanDeviceLost "No surface"

getQueuesOrFail ∷ GraphicsState → EngineM ε σ DevQueues
getQueuesOrFail state = case deviceQueues state of
    Just q  → pure q
    Nothing → throwGraphicsError VulkanDeviceLost "No queues"

getDescriptorManagerOrFail ∷ GraphicsState → EngineM ε σ DescriptorManager
getDescriptorManagerOrFail state = case descriptorState state of
    Just dm → pure dm
    Nothing → throwGraphicsError DescriptorError "No descriptor manager"

getTextureSystemOrFail ∷ GraphicsState → EngineM ε σ BindlessTextureSystem
getTextureSystemOrFail state = case textureSystem state of
    Just ts → pure ts
    Nothing → throwGraphicsError TextureLoadFailed "No texture system"

getFontDescriptorLayoutOrFail ∷ GraphicsState → EngineM ε σ DescriptorSetLayout
getFontDescriptorLayoutOrFail state = case fontDescriptorLayout state of
    Just fdl → pure fdl
    Nothing → throwGraphicsError FontError "No font descriptor layout"
