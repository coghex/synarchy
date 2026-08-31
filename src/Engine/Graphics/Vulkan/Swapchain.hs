{-# LANGUAGE OverloadedLists #-}
module Engine.Graphics.Vulkan.Swapchain
  ( createVulkanSwapchain
  , createSwapchainImageViews
  , swapchainImageUsage
    -- * Pure capability selection (#1954)
  , preferredSurfaceFormat
  , SurfaceFormatChoice(..)
  , chooseSwapSurfaceFormat
  , PresentModeChoice(..)
  , chooseSwapPresentMode
  ) where

import UPrelude
import qualified Data.Vector as V
import Engine.Core.Monad
import Engine.Core.Log (LogCategory(..))
import Engine.Core.Log.Monad (logDebugM, logDebugSM, logInfoSM, logWarnM)
import Engine.Graphics.Types
import Engine.Graphics.Vulkan.Types.Cleanup (Cleanup(..))
import Vulkan.Core10
import Vulkan.Zero
import Vulkan.Extensions.VK_KHR_surface as Surf
import Vulkan.Extensions.VK_KHR_swapchain as Swap
import Engine.Core.State (GraphicsState(..))

-- | Query swapchain support details from physical device
querySwapchainSupport ∷ PhysicalDevice → SurfaceKHR → EngineM σ SwapchainSupportDetails
querySwapchainSupport pdev surface = do
  logDebugM CatGraphics "Querying swapchain support"
  caps ← liftIO $ getPhysicalDeviceSurfaceCapabilitiesKHR pdev surface
  (_, fmts) ← liftIO $ getPhysicalDeviceSurfaceFormatsKHR pdev surface
  (_, modes) ← liftIO $ getPhysicalDeviceSurfacePresentModesKHR pdev surface
  
  logDebugSM CatGraphics "Surface capabilities"
    [("min_image_count", tshow $ Surf.minImageCount caps)
    ,("max_image_count", tshow $ Surf.maxImageCount caps)
    ,("current_extent", tshow $ currentExtent caps)]
  
  logDebugSM CatGraphics "Available surface formats"
    [("format_count", tshow $ V.length fmts)
    ,("present_mode_count", tshow $ V.length modes)]
  
  pure $ SwapchainSupportDetails caps fmts modes

-- | The image-usage flags to request for the swapchain, given the
--   surface's supportedUsageFlags, plus whether screenshot capture is
--   available. COLOR_ATTACHMENT is guaranteed by the Vulkan spec on
--   every presentable surface; TRANSFER_SRC — what
--   debug.captureScreenshot (#643) needs to copy the presented image —
--   is only requested when the surface actually reports it (#700):
--   requesting an unsupported usage is invalid and can fail swapchain
--   creation/recreation outright, so screenshot support must never
--   change the base swapchain contract. Pure, so the selection is
--   testable against synthetic capability flags.
swapchainImageUsage ∷ ImageUsageFlags → (ImageUsageFlags, Bool)
swapchainImageUsage supported =
    let capture = (supported ⌃ IMAGE_USAGE_TRANSFER_SRC_BIT) ≢ zero
    in ( if capture
           then IMAGE_USAGE_COLOR_ATTACHMENT_BIT ⌄ IMAGE_USAGE_TRANSFER_SRC_BIT
           else IMAGE_USAGE_COLOR_ATTACHMENT_BIT
       , capture )

-- | Creates a new swapchain. The framebuffer size is only consulted
--   when the surface reports the 0xFFFFFFFF "application chooses"
--   extent sentinel (e.g. Wayland).
createVulkanSwapchain ∷ PhysicalDevice → Device → DevQueues → SurfaceKHR → Bool
  → (Int, Int) → EngineM σ SwapchainInfo
createVulkanSwapchain pdev dev queues surface vsyncEnabled fbSize = do
  logDebugM CatSwapchain "Creating swapchain"
  SwapchainSupportDetails{..} ← querySwapchainSupport pdev surface
  let ssd = SwapchainSupportDetails{..}
      (chosenFormat, formatChoice) = chooseSwapSurfaceFormat ssdFormats
      SurfaceFormatKHR{format=form,colorSpace=cs} = chosenFormat
      desired    = Surf.minImageCount ssdCapabilities + 1
      maxImg     = Surf.maxImageCount ssdCapabilities
      imageCount = if maxImg > 0 then min desired maxImg else desired
      (spMode, modeChoice) = chooseSwapPresentMode ssdPresentModes vsyncEnabled
  -- Every warning here is driven by the pure selectors' OWN
  -- classification (#1954), never by a second predicate re-derived from
  -- the chosen value: the headless spec pins those classifications, so
  -- production and the gate cannot drift apart.
  case formatChoice of
    SurfaceFormatPreferred → pure ()
    SurfaceFormatFirstAdvertised → logWarnM CatSwapchain $
      "preferred surface format not advertised, using the first advertised one instead — wanted "
      <> describeSurfaceFormat preferredSurfaceFormat
      <> ", using " <> describeSurfaceFormat chosenFormat
    SurfaceFormatNoneAdvertised → logWarnM CatSwapchain $
      "surface advertised no formats at all — requesting the preferred pair unverified: "
      <> describeSurfaceFormat preferredSurfaceFormat
  case modeChoice of
    PresentModeRequested → pure ()
    PresentModeLowLatency → pure ()
    PresentModeFifoFallback → logWarnM CatSwapchain $
      "VSync disabled but neither " <> tshow Swap.PRESENT_MODE_MAILBOX_KHR
      <> " nor " <> tshow Swap.PRESENT_MODE_IMMEDIATE_KHR
      <> " is advertised — falling back to " <> tshow spMode
  -- The two pre-existing present-mode records keep their exact wording
  -- and severity; only their emission moved out of the now-pure
  -- selector (#1954 requirement 4).
  if vsyncEnabled
    then logDebugM CatSwapchain "VSync enabled: using FIFO present mode"
    else logInfoSM CatSwapchain "VSync disabled: using present mode"
           [("mode", tshow spMode)]
  let (usage, captureOK) =
        swapchainImageUsage (Surf.supportedUsageFlags ssdCapabilities)
  unless captureOK $ logDebugM CatSwapchain $
    "surface lacks TRANSFER_SRC swapchain usage — "
    <> "debug.captureScreenshot will report itself unavailable"
  let sExtent = chooseSwapExtent ssd fbSize
      -- Sharing is decided by queue FAMILY, not queue handle — two
      -- distinct queues from the same family still allow EXCLUSIVE,
      -- and CONCURRENT requires the family indices to be distinct.
      (sharing, qfi) = if (dqGraphicsFamIdx queues ≢ dqPresentFamIdx queues)
                       then (SHARING_MODE_CONCURRENT
                           , V.fromList [ dqGraphicsFamIdx queues
                                      , dqPresentFamIdx queues])
                       else (SHARING_MODE_EXCLUSIVE, [])
      swCreateInfo = zero
        { surface = surface
        , minImageCount = imageCount
        , imageFormat = form
        , imageColorSpace = cs
        , imageExtent = sExtent
        , imageArrayLayers = 1
          -- capability-checked: COLOR_ATTACHMENT always, TRANSFER_SRC
          -- (for debug.captureScreenshot, #643) only when the surface
          -- supports it — see swapchainImageUsage (#700).
        , imageUsage = usage
        , imageSharingMode = sharing
        , queueFamilyIndices = qfi
        , preTransform = currentTransform ssdCapabilities
        , compositeAlpha = COMPOSITE_ALPHA_OPAQUE_BIT_KHR
        , presentMode = spMode
        , clipped = True
        , oldSwapchain = zero
        }
  
  -- Info, not Debug: PR #14's delivery contract asks for the created
  -- swapchain's format, present mode, extent and image count at Info
  -- (#1954 requirement 8).
  logInfoSM CatSwapchain "Swapchain created"
    [("format", tshow form)
    ,("present_mode", tshow spMode)
    ,("extent", tshow sExtent)
    ,("image_count", tshow imageCount)]
  
  swapchain ← createSwapchainKHR dev swCreateInfo Nothing
  
  let cleanupAction = destroySwapchainKHR dev swapchain Nothing
  modifyGraphicsState $ \gs → gs {
      vulkanCleanup = (vulkanCleanup gs) {
          cleanupSwapchain = cleanupAction
      }
  }
  
  (_, swapImgs) ← getSwapchainImagesKHR dev swapchain
  pure $ SwapchainInfo
    { siTarget = TargetSwapchain swapchain
    , siSwapImgs = swapImgs
    , siSwapImgViews = V.empty
    , siSwapImgFormat = form
    , siSwapExtent = sExtent
    , siSupportsCapture = captureOK
    }

-- | Creates image views for swapchain images
createSwapchainImageViews ∷ Device → SwapchainInfo → EngineM σ (V.Vector ImageView)
createSwapchainImageViews dev SwapchainInfo{..} = do
  logDebugSM CatSwapchain "Creating swapchain image views"
    [("count", tshow $ V.length siSwapImgs)]
  imageViews ← V.mapM createImageViewf siSwapImgs
  
  logDebugM CatSwapchain "Swapchain image views created"
  
  let cleanupAction = V.forM_ imageViews $ \iv →
          destroyImageView dev iv Nothing
  modifyGraphicsState $ \gs → gs {
      vulkanCleanup = (vulkanCleanup gs) {
          cleanupImageViews = cleanupAction
      }
  }
  
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

-- | Both components of a surface-format pair, spelled out. The pair is
--   selected as ONE capability, so a warning naming only the format
--   would leave the colour space unaccounted for.
describeSurfaceFormat ∷ SurfaceFormatKHR → Text
describeSurfaceFormat SurfaceFormatKHR{format=f,colorSpace=c} =
  tshow f <> " / " <> tshow c

-- | The surface format the engine asks for whenever the driver
--   advertises it: 8-bit BGRA in the sRGB non-linear colour space.
preferredSurfaceFormat ∷ SurfaceFormatKHR
preferredSurfaceFormat = zero { format = FORMAT_B8G8R8A8_UNORM
                              , colorSpace = COLOR_SPACE_SRGB_NONLINEAR_KHR }

-- | Which branch of 'chooseSwapSurfaceFormat' ran. The chosen VALUE
--   alone cannot answer that: the empty-advertisement anomaly returns
--   'preferredSurfaceFormat' too, and it is the one case where the
--   engine requests a pair the surface never confirmed.
data SurfaceFormatChoice
  = SurfaceFormatPreferred
    -- ^ The preferred pair is advertised and was taken.
  | SurfaceFormatFirstAdvertised
    -- ^ The preferred pair is absent; the first advertised pair was
    --   taken instead. A degraded selection.
  | SurfaceFormatNoneAdvertised
    -- ^ The surface advertised no formats at all — anomalous, since a
    --   presentable surface must offer at least one. The preferred pair
    --   is requested unverified.
  deriving (Eq, Show)

-- | Choose the swap surface format, reporting which branch ran.
--
--   Pure and exported, so the selection is testable against synthetic
--   capability vectors with no driver — the same reason
--   'swapchainImageUsage' (#700) and
--   'Engine.Graphics.Vulkan.Instance.Plan.planVulkanInstance' (#1402)
--   are. The branch ORDER is the historical one and is what keeps the
--   chosen value unchanged (#1954 requirement 7).
chooseSwapSurfaceFormat ∷ V.Vector SurfaceFormatKHR
                        → (SurfaceFormatKHR, SurfaceFormatChoice)
chooseSwapSurfaceFormat advertised
  | preferredSurfaceFormat `V.elem` advertised
      = (preferredSurfaceFormat, SurfaceFormatPreferred)
  | V.null advertised
      = (preferredSurfaceFormat, SurfaceFormatNoneAdvertised)
  | otherwise
      = (V.head advertised, SurfaceFormatFirstAdvertised)

-- | Which branch of 'chooseSwapPresentMode' ran.
data PresentModeChoice
  = PresentModeRequested
    -- ^ VSync is enabled, so FIFO is the mode actually REQUESTED. Every
    --   Vulkan surface is required to support it, so this is never a
    --   fallback.
  | PresentModeLowLatency
    -- ^ VSync is disabled and a low-latency mode was advertised —
    --   MAILBOX preferred, IMMEDIATE equally accepted when MAILBOX is
    --   absent. Not a degraded selection.
  | PresentModeFifoFallback
    -- ^ VSync is disabled and NEITHER low-latency mode is advertised,
    --   so FIFO is reached as a compatibility fallback: the frame rate
    --   stays capped at the refresh rate despite the request.
  deriving (Eq, Show)

-- | Choose the swap present mode, reporting which branch ran. Pure and
--   exported for the same reason as 'chooseSwapSurfaceFormat'; the
--   MAILBOX → IMMEDIATE → FIFO preference order is unchanged.
chooseSwapPresentMode ∷ V.Vector Swap.PresentModeKHR → Bool
                      → (Swap.PresentModeKHR, PresentModeChoice)
chooseSwapPresentMode available vsyncEnabled
  -- VSync ON: FIFO (guaranteed to be available, caps at refresh rate)
  | vsyncEnabled = (Swap.PRESENT_MODE_FIFO_KHR, PresentModeRequested)
  -- VSync OFF: prefer MAILBOX (triple buffering), then IMMEDIATE (no
  -- limit), and only then fall back to FIFO.
  | Swap.PRESENT_MODE_MAILBOX_KHR `V.elem` available
      = (Swap.PRESENT_MODE_MAILBOX_KHR, PresentModeLowLatency)
  | Swap.PRESENT_MODE_IMMEDIATE_KHR `V.elem` available
      = (Swap.PRESENT_MODE_IMMEDIATE_KHR, PresentModeLowLatency)
  | otherwise = (Swap.PRESENT_MODE_FIFO_KHR, PresentModeFifoFallback)

-- | Clamp swapchain extent to surface capabilities. When currentExtent
--   is the 0xFFFFFFFF sentinel the surface size is up to us, so the
--   framebuffer size is clamped instead.
chooseSwapExtent ∷ SwapchainSupportDetails → (Int, Int) → Extent2D
chooseSwapExtent SwapchainSupportDetails{..} (fbW, fbH) = zero
  { width  = ( max (minw) $ min (maxw) w )
  , height = ( max (minh) $ min (maxh) h ) }
  where Extent2D{width=minw,height=minh} = minImageExtent ssdCapabilities
        Extent2D{width=maxw,height=maxh} = maxImageExtent ssdCapabilities
        Extent2D{width=curw,height=curh} = currentExtent  ssdCapabilities
        (w, h) = if curw ≡ 0xFFFFFFFF
                 then (fromIntegral fbW, fromIntegral fbH)
                 else (curw, curh)

