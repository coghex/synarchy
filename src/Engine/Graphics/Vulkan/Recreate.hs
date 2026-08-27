{-# LANGUAGE Strict #-}
module Engine.Graphics.Vulkan.Recreate
  ( recreateSwapchain
  , recreateSwapchainFor
  ) where

import UPrelude
import qualified Data.Vector as V
import Data.IORef (writeIORef, readIORef)
import Engine.Core.Monad
import Engine.Core.State (EngineState(..), GraphicsState(..))
import Engine.Core.Capability.Render
  (RenderCapability(..), toRenderCapability)
import Engine.Core.Log (LogCategory(..))
import Engine.Core.Log.Monad (logDebugM, logInfoM, logAndThrowM)
import Engine.Core.Error.Exception (GraphicsError(..), ExceptionType(..))
import Engine.Graphics.Camera (UICamera(..))
import Engine.Graphics.Config
import Engine.Graphics.Types
import Engine.Graphics.Window.Types (Window(..))
import qualified Engine.Graphics.Window.GLFW as GLFW
import Engine.Graphics.Vulkan.ResizeRequest
  (noteMinimizedFramebuffer, recordSwapchainFramebufferSize)
import Engine.Graphics.Vulkan.Swapchain
import Engine.Graphics.Vulkan.Framebuffer
import Engine.Graphics.Vulkan.Pipeline
import Engine.Graphics.Vulkan.Pipeline.Bindless
import Engine.Graphics.Vulkan.Types.Cleanup
import Engine.Graphics.Vulkan.Types.Descriptor
import Engine.Graphics.Vulkan.Texture.Types (BindlessTextureSystem(..))
import Engine.Graphics.Vulkan.MSAA
import Engine.Graphics.Vulkan.Sync (createRenderFinishedSemaphores)
import Engine.Graphics.Font.Draw
import Vulkan.Core10
import Vulkan.Extensions.VK_KHR_surface (SurfaceKHR)

-- | Recreate the swapchain and all dependent resources for the
--   framebuffer size the window has right now.
--
--   The entry point for the callers that are not reacting to a size
--   change and so have no size of their own to name: VSync, MSAA, and
--   the three exceptional-status paths in "Engine.Loop.Frame".
recreateSwapchain ∷ Window → EngineM σ ()
recreateSwapchain window = do
    let Window glfwWin = window
    fbSize ← GLFW.getFramebufferSize glfwWin
    recreateSwapchainFor window fbSize

-- | Recreate the swapchain and all dependent resources for an explicit
--   raw framebuffer size.
--
--   That size is used for the zero check, handed to
--   'createVulkanSwapchain', and stored by
--   'recordSwapchainFramebufferSize' — one value, three uses, no
--   second sample (#1693). This is what makes the resize request
--   terminate: 'Engine.Loop.applyPendingFramebufferResize' passes the
--   very size it saw outstanding, so serving a request always records
--   exactly the size that was requested. Sampling GLFW again in here
--   instead would let the swapchain be built from one size and the
--   request judged against another.
--
--   It is equally what keeps a resize arriving DURING a recreation
--   alive: the record names the size this rebuild used, never a later
--   read, so a newer framebuffer size is still a disagreement when
--   this returns and gets its own recreation.
--
--   Every recreation path in the engine ends up here, so any of them
--   satisfies a pending request for the same framebuffer size and one
--   window change can never cost two recreations.
recreateSwapchainFor ∷ Window → (Int, Int) → EngineM σ ()
recreateSwapchainFor window fbSize@(width, height) = do
    state ← gets graphicsState
    
    device ← getDeviceOrFail state
    pDevice ← getPhysicalDeviceOrFail state
    surface ← getSurfaceOrFail state
    queues ← getQueuesOrFail state
    
    -- The window is checked as well as the requested size, and the
    -- check comes BEFORE deviceWaitIdle.
    --
    -- Before, because a minimized window has nothing to rebuild and
    -- stalling the whole device to find that out is pure cost.
    --
    -- As well, because @fbSize@ can be a moment out of date: the caller
    -- reacting to a resize passes the size the input thread last
    -- recorded, and the window may have been minimized since. Building
    -- a swapchain for a stale nonzero size against a now-zero surface
    -- is a Vulkan error, and the surface is the authority on whether
    -- there is anything to build at all. On success the RECORD is still
    -- @fbSize@ and never this sample — see the note above.
    let Window glfwWin = window
    (liveWidth, liveHeight) ← GLFW.getFramebufferSize glfwWin
    if width ≡ 0 ∨ height ≡ 0 ∨ liveWidth ≡ 0 ∨ liveHeight ≡ 0
        then do
            logDebugM CatSwapchain "Window minimized, skipping swapchain recreation"
            noteMinimizedFramebuffer
        else do
            liftIO $ deviceWaitIdle device
            -- Old resources must be destroyed before new ones are created
            -- because Vulkan doesn't allow two swapchains for the same
            -- surface simultaneously. If recreateAllResources throws, the
            -- engine is in an unrecoverable state regardless (no swapchain),
            -- so the cleanup ordering is acceptable.
            logDebugM CatSwapchain "Running cleanup before recreation..."
            liftIO $ runAllCleanups (vulkanCleanup state)

            modifyGraphicsState $ \gs → gs {
                vulkanCleanup = emptyCleanup
            }

            recreateAllResources pDevice device queues surface fbSize

            modifyGraphicsState $ \gs → gs {
                currentFrame = 0
            }
            recordSwapchainFramebufferSize fbSize

            env ← ask
            liftIO $ writeIORef (rcUiCameraRef (toRenderCapability env)) $ 
                UICamera (fromIntegral width) (fromIntegral height)
            
            logInfoM CatSwapchain $ "Swapchain recreated: " <> (tshow width) 
                                    <> "x" <> (tshow height)

-- | Recreate all swapchain-dependent resources from the framebuffer
--   size 'recreateSwapchainFor' was given — never a fresh GLFW read,
--   so the size the swapchain is built from is the size that gets
--   recorded.
recreateAllResources ∷ PhysicalDevice → Device → DevQueues → SurfaceKHR 
                     → (Int, Int) → EngineM σ ()
recreateAllResources pDevice device queues surface fbSize = do
    state ← gets graphicsState
    
    -- Descriptor manager and texture system survive recreation
    descManager ← getDescriptorManagerOrFail state
    texSystem ← getTextureSystemOrFail
    fontDescLayout ← getFontDescriptorLayoutOrFail state
    
    let uniformLayout = dmUniformLayout descManager
        bindlessLayout = btsDescriptorLayout texSystem
    
    env ← ask
    videoConfig ← liftIO $ readIORef (rcVideoConfigRef (toRenderCapability env))
    let vsyncEnabled = vcVSync videoConfig
        msaaInt      = vcMSAA videoConfig
    swapInfo ← createVulkanSwapchain pDevice device queues surface
                 vsyncEnabled fbSize
    modifyGraphicsState $ \gs → gs {
        swapchainInfo = Just swapInfo
    }

    let newExtent = siSwapExtent swapInfo
        imgFormat = siSwapImgFormat swapInfo
    
    -- Clamp requested sample count to device support
    deviceProps ← getPhysicalDeviceProperties pDevice
    let supportedSamples = framebufferColorSampleCounts (limits deviceProps)
        requestedSamples = msaaToSampleCount msaaInt
        sampleCount      = clampSampleCount supportedSamples requestedSamples
    
    imageViews ← createSwapchainImageViews device swapInfo
    modifyGraphicsState $ \gs → gs {
        swapchainInfo = case swapchainInfo gs of
            Just si → Just si { siSwapImgViews = imageViews }
            Nothing → Nothing }
    
    mMsaaView ← if sampleCount ≢ SAMPLE_COUNT_1_BIT
        then do
            (img, mem, view) ← createMSAAColorImage pDevice device imgFormat newExtent sampleCount
            modifyGraphicsState $ \gs → gs {
                msaaColorImage = Just (img, mem, view)
            }
            pure (Just view)
        else do
            modifyGraphicsState $ \gs → gs {
                msaaColorImage = Nothing
            }
            pure Nothing
    
    renderPass ← createVulkanRenderPass device imgFormat sampleCount
                     (renderedImageLayout (siTarget swapInfo))
    modifyGraphicsState $ \gs → gs {
        vulkanRenderPass = Just renderPass
    }

    framebuffers ← createVulkanFramebuffers device renderPass swapInfo imageViews mMsaaView
    modifyGraphicsState $ \gs → gs {
        framebuffers = Just framebuffers
    }

    -- Per-IMAGE render-finished semaphores: the old set was destroyed by
    -- runAllCleanups above; create a fresh set sized to the new image count.
    _ ← createRenderFinishedSemaphores device (V.length framebuffers)
    
    (bindlessPipe, bindlessPipeLayout) ←
        createBindlessPipeline device renderPass newExtent uniformLayout bindlessLayout sampleCount
    modifyGraphicsState $ \gs → gs {
        bindlessPipeline = Just (bindlessPipe, bindlessPipeLayout)
    }

    (bindlessUIPipe, bindlessUIPipeLayout) ←
        createBindlessUIPipeline device renderPass newExtent uniformLayout bindlessLayout sampleCount
    modifyGraphicsState $ \gs → gs {
        bindlessUIPipeline = Just (bindlessUIPipe, bindlessUIPipeLayout)
    }

    (fontPipe, fontPipeLayout, _) ←
        createFontPipeline device renderPass newExtent uniformLayout sampleCount
    modifyGraphicsState $ \gs → gs {
        fontPipeline = Just (fontPipe, fontPipeLayout)
    }

    (fontUIPipe, fontUIPipeLayout) ←
        createFontUIPipeline device renderPass newExtent uniformLayout fontDescLayout sampleCount
    modifyGraphicsState $ \gs → gs {
        fontUIPipeline = Just (fontUIPipe, fontUIPipeLayout)
    }
    
    logDebugM CatGraphics "All resources recreated"

-- * State extractors
getDeviceOrFail ∷ GraphicsState → EngineM σ Device
getDeviceOrFail state = case vulkanDevice state of
    Just d  → pure d
    Nothing → logAndThrowM CatGraphics (ExGraphics VulkanDeviceLost)
                 "No device"

getPhysicalDeviceOrFail ∷ GraphicsState → EngineM σ PhysicalDevice
getPhysicalDeviceOrFail state = case vulkanPDevice state of
    Just pd → pure pd
    Nothing → logAndThrowM CatGraphics (ExGraphics VulkanDeviceLost)
                 "No physical device"

getSurfaceOrFail ∷ GraphicsState → EngineM σ SurfaceKHR
getSurfaceOrFail state = case vulkanSurface state of
    Just s  → pure s
    Nothing → logAndThrowM CatGraphics (ExGraphics VulkanDeviceLost)
                 "No surface"

getQueuesOrFail ∷ GraphicsState → EngineM σ DevQueues
getQueuesOrFail state = case deviceQueues state of
    Just q  → pure q
    Nothing → logAndThrowM CatGraphics (ExGraphics VulkanDeviceLost)
                 "No device queues"

getDescriptorManagerOrFail ∷ GraphicsState → EngineM σ DescriptorManager
getDescriptorManagerOrFail state = case descriptorState state of
    Just dm → pure dm
    Nothing → logAndThrowM CatDescriptor (ExGraphics DescriptorError)
                 "No descriptor manager"

getTextureSystemOrFail ∷ EngineM σ BindlessTextureSystem
getTextureSystemOrFail = do
    env ← ask
    mts ← liftIO $ readIORef (rcTextureSystemRef (toRenderCapability env))
    case mts of
        Just ts → pure ts
        Nothing → logAndThrowM CatTexture (ExGraphics TextureLoadFailed)
                     "No texture system"

getFontDescriptorLayoutOrFail ∷ GraphicsState → EngineM σ DescriptorSetLayout
getFontDescriptorLayoutOrFail state = case fontDescriptorLayout state of
    Just fdl → pure fdl
    Nothing  → logAndThrowM CatDescriptor (ExGraphics FontError)
                 "No font descriptor layout"
