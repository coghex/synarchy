{-# LANGUAGE Strict, UnicodeSyntax #-}
-- | Offscreen render target (#650): the swapchain's stand-in when
--   rendering with no window. One plain color image per frame in
--   flight, created with COLOR_ATTACHMENT (the render pass draws into
--   it) and TRANSFER_SRC (debug.captureScreenshot copies out of it)
--   usage — so capture is always supported, with none of the surface
--   capability negotiation the windowed path needs (#700). Nothing is
--   ever presented; the frame loop round-robins the images by
--   frame-in-flight index instead of acquiring.
module Engine.Graphics.Vulkan.Offscreen
  ( createOffscreenTarget
  , offscreenFormat
  ) where

import UPrelude
import qualified Data.Text as T
import qualified Data.Vector as V
import Engine.Core.Monad
import Engine.Core.State
import Engine.Core.Log (LogCategory(..))
import Engine.Core.Log.Monad (logDebugSM)
import Engine.Graphics.Types (RenderTarget(..), SwapchainInfo(..))
import Engine.Graphics.Vulkan.BufferUtils (findMemoryType)
import Engine.Graphics.Vulkan.Types.Cleanup (Cleanup(..))
import Vulkan.Core10 hiding (size)
import Vulkan.Core10.MemoryManagement (size)
import Vulkan.Zero

-- | The one format offscreen targets use: the same B8G8R8A8_UNORM the
--   windowed swapchain picker prefers, so offscreen screenshots decode
--   identically ('screenshotOrderOf' knows it) and shader output
--   matches the windowed render.
offscreenFormat ∷ Format
offscreenFormat = FORMAT_B8G8R8A8_UNORM

-- | Create the offscreen render target: @count@ images (one per frame
--   in flight) with backing memory and views, packaged as the same
--   'SwapchainInfo' the windowed path builds so everything downstream
--   of target creation is shared. Cleanup rides the same slots the
--   swapchain path uses: views in 'cleanupImageViews', images+memory
--   in 'cleanupSwapchain'.
createOffscreenTarget ∷ PhysicalDevice → Device → (Int, Int) → Int
                      → EngineM ε σ SwapchainInfo
createOffscreenTarget pDevice device (w, h) count = do
  let extent = Extent2D (fromIntegral w) (fromIntegral h)
      imageInfo = zero
        { imageType     = IMAGE_TYPE_2D
        , format        = offscreenFormat
        , extent        = Extent3D (fromIntegral w) (fromIntegral h) 1
        , mipLevels     = 1
        , arrayLayers   = 1
        , samples       = SAMPLE_COUNT_1_BIT
        , tiling        = IMAGE_TILING_OPTIMAL
        , usage         = IMAGE_USAGE_COLOR_ATTACHMENT_BIT
                          .|. IMAGE_USAGE_TRANSFER_SRC_BIT
        , sharingMode   = SHARING_MODE_EXCLUSIVE
        , initialLayout = IMAGE_LAYOUT_UNDEFINED
        }

  imgsAndMem ← V.replicateM count $ do
    image ← createImage device imageInfo Nothing
    memReqs ← getImageMemoryRequirements device image
    memTypeIdx ← findMemoryType pDevice
        (memoryTypeBits (memReqs ∷ MemoryRequirements))
        MEMORY_PROPERTY_DEVICE_LOCAL_BIT
    memory ← allocateMemory device zero
        { allocationSize  = size (memReqs ∷ MemoryRequirements)
        , memoryTypeIndex = memTypeIdx
        } Nothing
    bindImageMemory device image memory 0
    pure (image, memory)

  let images = V.map fst imgsAndMem
      memories = V.map snd imgsAndMem

  views ← V.forM images $ \image →
    createImageView device zero
      { image = image
      , viewType = IMAGE_VIEW_TYPE_2D
      , format = offscreenFormat
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

  let cleanupViews = V.forM_ views $ \v → destroyImageView device v Nothing
      cleanupImages = V.forM_ imgsAndMem $ \(img, mem) → do
          destroyImage device img Nothing
          freeMemory device mem Nothing
  modify $ \s → s { graphicsState = (graphicsState s) {
      vulkanCleanup = (vulkanCleanup (graphicsState s))
        { cleanupImageViews = cleanupViews
        , cleanupSwapchain  = cleanupImages
        } } }

  logDebugSM CatGraphics "Offscreen render target created"
    [("extent", T.pack (show w) <> "x" <> T.pack (show h))
    ,("image_count", T.pack (show count))]

  pure SwapchainInfo
    { siTarget          = TargetOffscreen memories
    , siSwapImgs        = images
    , siSwapImgViews    = views
    , siSwapImgFormat   = offscreenFormat
    , siSwapExtent      = extent
    , siSupportsCapture = True
    }
