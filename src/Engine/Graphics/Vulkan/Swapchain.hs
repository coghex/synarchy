{-# LANGUAGE OverloadedLists #-}
module Engine.Graphics.Vulkan.Swapchain
  ( createVulkanSwapchain
  , createSwapchainImageViews
  , querySwapchainSupport
  , acquireVulkanImage
  , submitQueue
  , waitIdle
  ) where

import UPrelude
import qualified Data.Vector as V
import qualified Data.Text as T
import Engine.Core.Monad
import Engine.Core.State
import Engine.Core.Resource (allocResource)
import Engine.Core.Log (LogCategory(..))
import Engine.Core.Log.Monad (logDebugM, logInfoM, logWarnM, logDebugSM, logInfoSM)
import Engine.Graphics.Types
import Engine.Graphics.Vulkan.Sync
import Engine.Graphics.Vulkan.Types.Cleanup (Cleanup(..))
import Vulkan.Core10
import Vulkan.Zero
import Vulkan.CStruct.Extends
import Vulkan.Extensions.VK_KHR_surface as Surf
import Vulkan.Extensions.VK_KHR_swapchain as Swap

-- | Query swapchain support details from physical device
querySwapchainSupport ∷ PhysicalDevice → SurfaceKHR → EngineM ε σ SwapchainSupportDetails
querySwapchainSupport pdev surface = do
  logDebugM CatGraphics "Querying swapchain support"
  caps ← liftIO $ getPhysicalDeviceSurfaceCapabilitiesKHR pdev surface
  (_, fmts) ← liftIO $ getPhysicalDeviceSurfaceFormatsKHR pdev surface
  (_, modes) ← liftIO $ getPhysicalDeviceSurfacePresentModesKHR pdev surface
  
  logDebugSM CatGraphics "Surface capabilities"
    [("min_image_count", T.pack $ show $ Surf.minImageCount caps)
    ,("max_image_count", T.pack $ show $ Surf.maxImageCount caps)
    ,("current_extent", T.pack $ show $ currentExtent caps)]
  
  logDebugSM CatGraphics "Available surface formats"
    [("format_count", T.pack $ show $ V.length fmts)
    ,("present_mode_count", T.pack $ show $ V.length modes)]
  
  pure $ SwapchainSupportDetails caps fmts modes

-- Replace createVulkanSwapchain:

-- | Creates a new swapchain
createVulkanSwapchain ∷ PhysicalDevice → Device → DevQueues → SurfaceKHR → Bool
  → EngineM ε σ SwapchainInfo
createVulkanSwapchain pdev dev queues surface vsyncEnabled = do
  logDebugM CatSwapchain "Creating swapchain"
  SwapchainSupportDetails{..} ← querySwapchainSupport pdev surface
  let ssd = SwapchainSupportDetails{..}
      SurfaceFormatKHR{format=form,colorSpace=cs} = chooseSwapSurfaceFormat ssd
      imageCount = min (Surf.maxImageCount capabilities)
                      (Surf.minImageCount capabilities)
  spMode ← chooseSwapPresentMode ssd vsyncEnabled
  let sExtent = chooseSwapExtent ssd
      (sharing, qfi) = if (graphicsQueue queues ≠ presentQueue queues)
                       then (SHARING_MODE_CONCURRENT
                           , V.fromList [ graphicsFamIdx queues
                                      , presentFamIdx queues])
                       else (SHARING_MODE_EXCLUSIVE, [])
      swCreateInfo = zero
        { surface = surface
        , minImageCount = imageCount
        , imageFormat = form
        , imageColorSpace = cs
        , imageExtent = sExtent
        , imageArrayLayers = 1
        , imageUsage = IMAGE_USAGE_COLOR_ATTACHMENT_BIT
        , imageSharingMode = sharing
        , queueFamilyIndices = qfi
        , preTransform = currentTransform capabilities
        , compositeAlpha = COMPOSITE_ALPHA_OPAQUE_BIT_KHR
        , presentMode = spMode
        , clipped = True
        , oldSwapchain = zero
        }
  
  logDebugSM CatSwapchain "Swapchain created"
    [("format", T.pack $ show form)
    ,("present_mode", T.pack $ show spMode)
    ,("extent", T.pack $ show sExtent)
    ,("image_count", T.pack $ show imageCount)]
  
  swapchain ← createSwapchainKHR dev swCreateInfo Nothing
  
  -- Build cleanup action
  let cleanupAction = destroySwapchainKHR dev swapchain Nothing
  
  -- Store cleanup in state
  modify $ \s → s { graphicsState = (graphicsState s) {
      vulkanCleanup = (vulkanCleanup (graphicsState s)) {
          cleanupSwapchain = cleanupAction
      }
  }}
  
  (_, swapImgs) ← getSwapchainImagesKHR dev swapchain
  pure $ SwapchainInfo
    { siSwapchain = swapchain
    , siSwapImgs = swapImgs
    , siSwapImgViews = V.empty
    , siSwapImgFormat = form
    , siSwapExtent = sExtent
    }

-- | Creates image views for swapchain images
createSwapchainImageViews ∷ Device → SwapchainInfo → EngineM ε σ (V.Vector ImageView)
createSwapchainImageViews dev SwapchainInfo{..} = do
  logDebugSM CatSwapchain "Creating swapchain image views"
    [("count", T.pack $ show $ V.length siSwapImgs)]
  imageViews ← V.mapM createImageViewf siSwapImgs
  
  logDebugM CatSwapchain "Swapchain image views created"
  
  -- Build cleanup action (destroy all image views)
  let cleanupAction = V.forM_ imageViews $ \iv →
          destroyImageView dev iv Nothing
  
  -- Store cleanup in state
  modify $ \s → s { graphicsState = (graphicsState s) {
      vulkanCleanup = (vulkanCleanup (graphicsState s)) {
          cleanupImageViews = cleanupAction
      }
  }}
  
  pure imageViews
  where
    createImageViewf image = 
      createImageView dev zero
        { image = image
        , viewType = IMAGE_VIEW_TYPE_2D
        , format = siSwapImgFormat
        , components = zero
            { r = COMPONENT_SWIZZLE_IDENTITY
            , g = COMPONENT_SWIZZLE_IDENTITY
            , b = COMPONENT_SWIZZLE_IDENTITY
            , a = COMPONENT_SWIZZLE_IDENTITY }
        , subresourceRange = zero
            { aspectMask = IMAGE_ASPECT_COLOR_BIT
            , baseMipLevel = 0
            , levelCount = 1
            , baseArrayLayer = 0
            , layerCount = 1 }
        } Nothing

-- | Choose the best swap surface format
chooseSwapSurfaceFormat ∷ SwapchainSupportDetails → SurfaceFormatKHR
chooseSwapSurfaceFormat (SwapchainSupportDetails _ formats _) = best
  where best = if preferred `elem` formats then preferred else 
                 if V.null formats then preferred else V.head formats
        preferred = zero { format = FORMAT_B8G8R8A8_UNORM
                       , colorSpace = COLOR_SPACE_SRGB_NONLINEAR_KHR }

chooseSwapPresentMode :: SwapchainSupportDetails -> Bool -> EngineM ε σ Swap.PresentModeKHR
chooseSwapPresentMode ssd vsyncEnabled = do
  let available = presentModes ssd
  
  if vsyncEnabled
    then do
      -- VSync ON: Use FIFO (guaranteed to be available, caps at refresh rate)
      logDebugM CatSwapchain "VSync enabled: using FIFO present mode"
      pure Swap.PRESENT_MODE_FIFO_KHR
    else do
      -- VSync OFF: Prefer MAILBOX (triple buffering) or IMMEDIATE (no limit)
      let preferred = if Swap.PRESENT_MODE_MAILBOX_KHR `V.elem` available
                        then Swap.PRESENT_MODE_MAILBOX_KHR
                        else if Swap.PRESENT_MODE_IMMEDIATE_KHR `V.elem` available
                               then Swap.PRESENT_MODE_IMMEDIATE_KHR
                               else Swap.PRESENT_MODE_FIFO_KHR
      
      logInfoSM CatSwapchain "VSync disabled: using present mode"
        [("mode", T.pack $ show preferred)]
      pure preferred

-- | Set the width and height of the swapchain
chooseSwapExtent ∷ SwapchainSupportDetails → Extent2D
chooseSwapExtent SwapchainSupportDetails{..} = zero
  { width  = ( max (minw) $ min (maxw) (curw) )
  , height = ( max (minh) $ min (maxh) (curh) ) }
  where Extent2D{width=minw,height=minh} = minImageExtent capabilities
        Extent2D{width=maxw,height=maxh} = maxImageExtent capabilities
        Extent2D{width=curw,height=curh} = currentExtent  capabilities

-- | Acquire next image from swapchain
acquireVulkanImage ∷ Device → SwapchainKHR → Word64 → Semaphore
  → EngineM ε σ Word32
acquireVulkanImage dev swapchain maxBound imageAvailableSemaphore = do
  res ← acquireNextImageKHR dev swapchain maxBound imageAvailableSemaphore zero
  pure $ snd res

-- | Submit command buffers to queue and present
submitQueue ∷ SwapchainKHR → Semaphore → Semaphore → Word32
  → Queue → Queue → V.Vector CommandBuffer → EngineM ε σ ()
submitQueue swapchain imageAvailableSemaphore renderFinishedSemaphore
            imageIndex graphicsQueue presentQueue commandBuffers = do
  let submitInfo = zero
        { waitSemaphores = [imageAvailableSemaphore]
        , waitDstStageMask = [PIPELINE_STAGE_COLOR_ATTACHMENT_OUTPUT_BIT]
        , commandBuffers = [ commandBufferHandle
                           $ commandBuffers
                           V.! fromIntegral imageIndex ]
        , signalSemaphores = [renderFinishedSemaphore]
        }
      presentInfo = zero { waitSemaphores = [renderFinishedSemaphore]
                        , swapchains = [swapchain]
                        , imageIndices = [imageIndex]
                        }
  queueSubmit graphicsQueue [SomeStruct submitInfo] zero
  _ ← queuePresentKHR presentQueue presentInfo
  pure ()

-- | Wait for device and queue to be idle
waitIdle ∷ Device → Queue → EngineM ε σ ()
waitIdle dev gq = do
  queueWaitIdle gq
  deviceWaitIdle dev
