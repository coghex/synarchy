module Engine.Graphics.Vulkan.Image
  ( createVulkanImage
  , createVulkanImage'
  , createVulkanImageView
  , createVulkanImageView'
  , copyBufferToImage
  , VulkanImage(..)
  ) where

import UPrelude
import qualified Data.Vector as V
import Engine.Core.Monad
import Engine.Core.Log (LogCategory(..))
import Engine.Core.Log.Monad (logAndThrowM)
import Engine.Core.Resource
import Engine.Core.Error.Exception (ExceptionType(..), SystemError(..))
import Engine.Graphics.Vulkan.Types.Texture
import Vulkan.Core10
import Vulkan.Zero

createVulkanImage ∷ Device → PhysicalDevice → (Word32, Word32) → Format → ImageTiling 
                  → ImageUsageFlags → MemoryPropertyFlags → EngineM ε σ VulkanImage
createVulkanImage device pDevice (width, height) format tiling usage memProps = do
  let imageInfo = zero
        { imageType = IMAGE_TYPE_2D
        , extent = Extent3D width height 1
        , mipLevels = 1
        , arrayLayers = 1
        , format = format
        , tiling = tiling
        , initialLayout = IMAGE_LAYOUT_UNDEFINED
        , usage = usage
        , sharingMode = SHARING_MODE_EXCLUSIVE
        , samples = SAMPLE_COUNT_1_BIT
        }
  
  image ← allocResource (\img → destroyImage device img Nothing) $
    createImage device imageInfo Nothing

  MemoryRequirements {size=siz, alignment=_, memoryTypeBits=mtb}
    ← getImageMemoryRequirements device image
  memTypeIndex ← findMemoryType pDevice mtb memProps
  
  let allocInfo = zero
        { allocationSize = siz
        , memoryTypeIndex = memTypeIndex
        }

  memory ← allocResource (\mem → freeMemory device mem Nothing) $
    allocateMemory device allocInfo Nothing

  bindImageMemory device image memory 0

  pure $ VulkanImage image memory

-- | Like 'createVulkanImage' but returns an explicit cleanup action
createVulkanImage' ∷ Device → PhysicalDevice → (Word32, Word32) → Format → ImageTiling 
                   → ImageUsageFlags → MemoryPropertyFlags 
                   → EngineM ε σ (VulkanImage, IO ())
createVulkanImage' device pDevice (width, height) format tiling usage memProps = do
  let imageInfo = zero
        { imageType = IMAGE_TYPE_2D
        , extent = Extent3D width height 1
        , mipLevels = 1
        , arrayLayers = 1
        , format = format
        , tiling = tiling
        , initialLayout = IMAGE_LAYOUT_UNDEFINED
        , usage = usage
        , sharingMode = SHARING_MODE_EXCLUSIVE
        , samples = SAMPLE_COUNT_1_BIT
        }
  
  (image, cleanupImage) ← allocResource'IO 
    (\img → liftIO $ destroyImage device img Nothing)
    (createImage device imageInfo Nothing)

  MemoryRequirements {size=siz, alignment=_, memoryTypeBits=mtb}
    ← getImageMemoryRequirements device image

  memTypeIndex ← findMemoryType pDevice mtb memProps
  
  let allocInfo = zero
        { allocationSize = siz
        , memoryTypeIndex = memTypeIndex
        }

  (memory, cleanupMemory) ← allocResource'IO
    (\mem → liftIO $ freeMemory device mem Nothing)
    (allocateMemory device allocInfo Nothing)

  bindImageMemory device image memory 0

  let cleanup = do
        cleanupImage
        cleanupMemory

  pure (VulkanImage image memory, cleanup)

-- | Create an image view for the given image
createVulkanImageView ∷ Device
                     → VulkanImage
                     → Format
                     → ImageAspectFlags
                     → EngineM ε σ ImageView
createVulkanImageView device (VulkanImage image _) format aspectFlags =
  allocResource (\view → destroyImageView device view Nothing) $
    createImageView device zero
      { image = image
      , viewType = IMAGE_VIEW_TYPE_2D
      , format = format
      , subresourceRange = zero
          { aspectMask = aspectFlags
          , baseMipLevel = 0
          , levelCount = 1
          , baseArrayLayer = 0
          , layerCount = 1
          }
      } Nothing

-- | Like 'createVulkanImageView' but returns an explicit cleanup
--   action — for replaceable textures whose lifetime is shorter than
--   the program (allocResource would defer destruction to exit).
createVulkanImageView' ∷ Device
                       → VulkanImage
                       → Format
                       → ImageAspectFlags
                       → EngineM ε σ (ImageView, IO ())
createVulkanImageView' device (VulkanImage image _) format aspectFlags =
  allocResource'IO (\view → liftIO $ destroyImageView device view Nothing) $
    createImageView device zero
      { image = image
      , viewType = IMAGE_VIEW_TYPE_2D
      , format = format
      , subresourceRange = zero
          { aspectMask = aspectFlags
          , baseMipLevel = 0
          , levelCount = 1
          , baseArrayLayer = 0
          , layerCount = 1
          }
      } Nothing

findMemoryType ∷ PhysicalDevice → Word32 → MemoryPropertyFlags → EngineM ε σ Word32
findMemoryType pdev typeFilter properties = do
  memProps ← getPhysicalDeviceMemoryProperties pdev
  let types = memoryTypes memProps
      suitable i memType = 
        testBit typeFilter (fromIntegral i) 
        ∧ (propertyFlags memType ⌃ properties) ≡ properties
      
      findType i
        | i ≥ fromIntegral (V.length types) = 
            logAndThrowM CatVulkan (ExSystem (MemoryError "findMemoryType: ")) $
              "failed to find suitable memory type"
        | suitable i (types V.! i) = return $ fromIntegral i
        | otherwise = findType (i + 1)
  
  findType 0

copyBufferToImage ∷ CommandBuffer → Buffer
  → VulkanImage → Word32 → Word32 → EngineM ε σ ()
copyBufferToImage cmdBuf buffer (VulkanImage image _) width height
  = cmdCopyBufferToImage cmdBuf buffer image
                           IMAGE_LAYOUT_TRANSFER_DST_OPTIMAL $ V.singleton region
  where region = zero { bufferOffset      = 0
                      , bufferRowLength   = 0
                      , bufferImageHeight = 0
                      , imageSubresource  = zero
                          { aspectMask     = IMAGE_ASPECT_COLOR_BIT
                          , mipLevel       = 0
                          , baseArrayLayer = 0
                          , layerCount     = 1 }
                      , imageOffset       = Offset3D { x=0,y=0,z=0 }
                      , imageExtent       = zero { width  = width
                                                 , height = height
                                                 , depth  = 1 } }
