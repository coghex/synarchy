{-# LANGUAGE Strict #-}
module Engine.Graphics.Vulkan.MSAA
  ( createMSAAColorImage
  ) where

import UPrelude
import qualified Data.Vector as V
import Engine.Core.Monad
import Engine.Core.State
import Engine.Graphics.Vulkan.Types.Cleanup (Cleanup(..))
import Vulkan.Core10 hiding (size)
import Vulkan.Core10.MemoryManagement (size)
import Vulkan.Zero

-- | Create a multisampled color image for MSAA rendering.
-- Returns the image, its memory, and an image view.
-- Only call this when sampleCount > SAMPLE_COUNT_1_BIT.
createMSAAColorImage ∷ PhysicalDevice
                     → Device
                     → Format          -- ^ Swapchain image format
                     → Extent2D        -- ^ Swapchain extent
                     → SampleCountFlagBits
                     → EngineM ε σ (Image, DeviceMemory, ImageView)
createMSAAColorImage pDevice device format extent sampleCount = do
    let Extent2D w h = extent

        imageInfo = zero
          { imageType     = IMAGE_TYPE_2D
          , format        = format
          , extent        = Extent3D w h 1
          , mipLevels     = 1
          , arrayLayers   = 1
          , samples       = sampleCount
          , tiling        = IMAGE_TILING_OPTIMAL
          , usage         = IMAGE_USAGE_COLOR_ATTACHMENT_BIT
                            .|. IMAGE_USAGE_TRANSIENT_ATTACHMENT_BIT
          , sharingMode   = SHARING_MODE_EXCLUSIVE
          , initialLayout = IMAGE_LAYOUT_UNDEFINED
          }

    image ← createImage device imageInfo Nothing

    -- Query memory requirements
    memReqs ← getImageMemoryRequirements device image

    -- Find a suitable memory type (prefer LAZILY_ALLOCATED, fall back to DEVICE_LOCAL)
    memTypeIdx ← findMSAAMemoryType pDevice memReqs

    let allocInfo = zero
          { allocationSize  = size (memReqs ∷ MemoryRequirements)
          , memoryTypeIndex = memTypeIdx
          }

    memory ← allocateMemory device allocInfo Nothing
    bindImageMemory device image memory 0

    -- Create image view
    let viewInfo = zero
          { image      = image
          , viewType   = IMAGE_VIEW_TYPE_2D
          , format     = format
          , components = zero
              { r = COMPONENT_SWIZZLE_IDENTITY
              , g = COMPONENT_SWIZZLE_IDENTITY
              , b = COMPONENT_SWIZZLE_IDENTITY
              , a = COMPONENT_SWIZZLE_IDENTITY
              }
          , subresourceRange = zero
              { aspectMask     = IMAGE_ASPECT_COLOR_BIT
              , baseMipLevel   = 0
              , levelCount     = 1
              , baseArrayLayer = 0
              , layerCount     = 1
              }
          }

    imageView ← createImageView device viewInfo Nothing

    -- Register cleanup
    let cleanupAction = do
            destroyImageView device imageView Nothing
            destroyImage device image Nothing
            freeMemory device memory Nothing

    modify $ \s → s { graphicsState = (graphicsState s) {
        vulkanCleanup = (vulkanCleanup (graphicsState s)) {
            cleanupMSAAImage = cleanupAction
        }
    }}

    pure (image, memory, imageView)

-- | Find memory type for MSAA image.
-- Prefer LAZILY_ALLOCATED (saves bandwidth on tiled GPUs), fall back to DEVICE_LOCAL.
findMSAAMemoryType ∷ PhysicalDevice → MemoryRequirements → EngineM ε σ Word32
findMSAAMemoryType pDevice memReqs = do
    memProps ← getPhysicalDeviceMemoryProperties pDevice
    let typeBits = memoryTypeBits memReqs
        memTypes = memoryTypes (memProps ∷ PhysicalDeviceMemoryProperties)

        -- Try LAZILY_ALLOCATED first (ideal for transient MSAA attachments)
        lazyIdx = findMemType typeBits
                    MEMORY_PROPERTY_LAZILY_ALLOCATED_BIT memTypes
        -- Fall back to DEVICE_LOCAL
        localIdx = findMemType typeBits
                     MEMORY_PROPERTY_DEVICE_LOCAL_BIT memTypes

    case lazyIdx of
        Just idx → pure idx
        Nothing  → case localIdx of
            Just idx → pure idx
            Nothing  → pure 0  -- Last resort: first compatible type

findMemType ∷ Word32 → MemoryPropertyFlags → V.Vector MemoryType → Maybe Word32
findMemType typeBits requiredFlags memTypes =
    let indexed = V.imap (,) memTypes
        matches = V.filter (\(i, mt) →
            testBit typeBits (fromIntegral i)
            && (propertyFlags mt .&. requiredFlags) == requiredFlags
          ) indexed
    in case V.null matches of
        True  → Nothing
        False → Just $ fromIntegral $ fst $ V.head matches
