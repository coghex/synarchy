{-# LANGUAGE OverloadedLists, UnicodeSyntax #-}
module Engine.Graphics.Vulkan.Swapchain
  ( createVulkanSwapchain
  , createSwapchainImageViews
  , querySwapchainSupport
  ) where

import UPrelude
import qualified Data.Vector as V
import qualified Data.Text as T
import Engine.Core.Monad
import Engine.Core.State
import Engine.Core.Log (LogCategory(..))
import Engine.Core.Log.Monad (logDebugM, logDebugSM, logInfoSM)
import Engine.Graphics.Types
import Engine.Graphics.Vulkan.Types.Cleanup (Cleanup(..))
import Vulkan.Core10
import Vulkan.Zero
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

-- | Creates a new swapchain. The framebuffer size is only consulted
--   when the surface reports the 0xFFFFFFFF "application chooses"
--   extent sentinel (e.g. Wayland).
createVulkanSwapchain ∷ PhysicalDevice → Device → DevQueues → SurfaceKHR → Bool
  → (Int, Int) → EngineM ε σ SwapchainInfo
createVulkanSwapchain pdev dev queues surface vsyncEnabled fbSize = do
  logDebugM CatSwapchain "Creating swapchain"
  SwapchainSupportDetails{..} ← querySwapchainSupport pdev surface
  let ssd = SwapchainSupportDetails{..}
      SurfaceFormatKHR{format=form,colorSpace=cs} = chooseSwapSurfaceFormat ssd
      desired    = Surf.minImageCount capabilities + 1
      maxImg     = Surf.maxImageCount capabilities
      imageCount = if maxImg > 0 then min desired maxImg else desired
  spMode ← chooseSwapPresentMode ssd vsyncEnabled
  let sExtent = chooseSwapExtent ssd fbSize
      -- Sharing is decided by queue FAMILY, not queue handle — two
      -- distinct queues from the same family still allow EXCLUSIVE,
      -- and CONCURRENT requires the family indices to be distinct.
      (sharing, qfi) = if (graphicsFamIdx queues ≠ presentFamIdx queues)
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
          -- TRANSFER_SRC lets debug.captureScreenshot (#643) copy the
          -- presented image into a host-visible staging buffer.
        , imageUsage = IMAGE_USAGE_COLOR_ATTACHMENT_BIT
                     ⌄ IMAGE_USAGE_TRANSFER_SRC_BIT
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
  
  let cleanupAction = destroySwapchainKHR dev swapchain Nothing
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
  
  let cleanupAction = V.forM_ imageViews $ \iv →
          destroyImageView dev iv Nothing
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

chooseSwapPresentMode ∷ SwapchainSupportDetails → Bool → EngineM ε σ Swap.PresentModeKHR
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

-- | Clamp swapchain extent to surface capabilities. When currentExtent
--   is the 0xFFFFFFFF sentinel the surface size is up to us, so the
--   framebuffer size is clamped instead.
chooseSwapExtent ∷ SwapchainSupportDetails → (Int, Int) → Extent2D
chooseSwapExtent SwapchainSupportDetails{..} (fbW, fbH) = zero
  { width  = ( max (minw) $ min (maxw) w )
  , height = ( max (minh) $ min (maxh) h ) }
  where Extent2D{width=minw,height=minh} = minImageExtent capabilities
        Extent2D{width=maxw,height=maxh} = maxImageExtent capabilities
        Extent2D{width=curw,height=curh} = currentExtent  capabilities
        (w, h) = if curw ≡ 0xFFFFFFFF
                 then (fromIntegral fbW, fromIntegral fbH)
                 else (curw, curh)

