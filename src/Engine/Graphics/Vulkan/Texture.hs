-- src/Engine/Graphics/Vulkan/Texture.hs
module Engine.Graphics.Vulkan.Texture
  ( createTextureImageView
  , createTextureSampler
  , createTextureDescriptorSet
  , createTextureDescriptorPool
  , createTextureDescriptorSetLayout
  , createTextureWithDescriptor
  , transitionImageLayout
  , ImageLayoutTransition(..)
  , module Engine.Graphics.Vulkan.Types.Texture
  ) where

import UPrelude
import qualified Codec.Picture as JP
import Control.Monad (when, filterM)
import qualified Data.Vector as V
import qualified Data.Text as T
import qualified Data.Vector.Storable as Vec
import Data.Word (Word32)
import Foreign.Marshal.Array (copyArray)
import Foreign.Ptr (castPtr)
import Foreign.ForeignPtr (withForeignPtr)
import Engine.Core.Monad
import Engine.Core.Resource
import Engine.Core.Error.Exception
import Engine.Graphics.Types
import Engine.Graphics.Vulkan.Image (createVulkanImage, createVulkanImageView
                                    , copyBufferToImage, VulkanImage(..))
import Engine.Graphics.Vulkan.Buffer
import Engine.Graphics.Vulkan.Command
import Engine.Graphics.Vulkan.Types.Texture
import Vulkan.Core10
import Vulkan.Zero
import Vulkan.CStruct.Extends

data ImageLayoutTransition = Undef_TransDst
                          | TransDst_ShaderRO
                          | Undef_DepthStencilAtt
                          | Undef_ColorAtt

createTextureImageView ∷ PhysicalDevice → Device → CommandPool
                      → Queue → FilePath → EngineM ε σ (ImageView, Word32)
createTextureImageView pdev dev cmdPool cmdQueue path = do
  JP.Image { JP.imageWidth, JP.imageHeight, JP.imageData }
    ← liftIO (JP.readImage path) ⌦ \case
      Left err → throwEngineException $ EngineException ExGraphics
        $ T.pack $ "cannot create texture image view: " ++ err
      Right dynImg → pure $ JP.convertRGBA8 dynImg
  let (imageDataForeignPtr, imageDataLen)
        = Vec.unsafeToForeignPtr0 imageData
      bufSize = fromIntegral imageDataLen
      mipLevels = (floor ∘ logBase (2 ∷ Float)
                ∘ fromIntegral $ max imageWidth imageHeight) + 1
  image ← createVulkanImage dev pdev
    (fromIntegral imageWidth, fromIntegral imageHeight)
    FORMAT_R8G8B8A8_UNORM
    IMAGE_TILING_OPTIMAL
    (IMAGE_USAGE_TRANSFER_SRC_BIT
    ⌄ IMAGE_USAGE_TRANSFER_DST_BIT
    ⌄ IMAGE_USAGE_SAMPLED_BIT)
    MEMORY_PROPERTY_DEVICE_LOCAL_BIT
  
  locally $ do
    (stagingMem, stagingBuf) ← createVulkanBuffer dev pdev bufSize
      BUFFER_USAGE_TRANSFER_SRC_BIT
      (MEMORY_PROPERTY_HOST_VISIBLE_BIT
      ⌄ MEMORY_PROPERTY_HOST_COHERENT_BIT)
    stagingDataPtr ← mapMemory dev stagingMem 0 bufSize zero
    liftIO $ withForeignPtr imageDataForeignPtr
      $ \imageDataPtr → copyArray (castPtr stagingDataPtr)
                          imageDataPtr imageDataLen
    unmapMemory dev stagingMem
    
    runCommandsOnce dev cmdPool cmdQueue $ \cmdBuf → do
      transitionImageLayout image FORMAT_R8G8B8A8_UNORM
        Undef_TransDst mipLevels cmdBuf
      copyBufferToImage cmdBuf stagingBuf image
        (fromIntegral imageWidth) (fromIntegral imageHeight)
      transitionImageLayout image FORMAT_R8G8B8A8_UNORM
        TransDst_ShaderRO mipLevels cmdBuf
  
  imageView ← createVulkanImageView dev image
    FORMAT_R8G8B8A8_UNORM IMAGE_ASPECT_COLOR_BIT
  return (imageView, mipLevels)

transitionImageLayout ∷ VulkanImage → Format → ImageLayoutTransition
                     → Word32 → CommandBuffer → EngineM ε σ ()
transitionImageLayout (VulkanImage image _) format transition
                      mipLevels cmdBuf = do
  let (oldLayout, newLayout, srcAccess, dstAccess, srcStage, dstStage) =
        case transition of
          Undef_TransDst →
            ( IMAGE_LAYOUT_UNDEFINED
            , IMAGE_LAYOUT_TRANSFER_DST_OPTIMAL
            , zero
            , ACCESS_TRANSFER_WRITE_BIT
            , PIPELINE_STAGE_TOP_OF_PIPE_BIT
            , PIPELINE_STAGE_TRANSFER_BIT
            )
          TransDst_ShaderRO →
            ( IMAGE_LAYOUT_TRANSFER_DST_OPTIMAL
            , IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL
            , ACCESS_TRANSFER_WRITE_BIT
            , ACCESS_SHADER_READ_BIT
            , PIPELINE_STAGE_TRANSFER_BIT
            , PIPELINE_STAGE_FRAGMENT_SHADER_BIT
            )
          Undef_DepthStencilAtt →
            ( IMAGE_LAYOUT_UNDEFINED
            , IMAGE_LAYOUT_DEPTH_STENCIL_ATTACHMENT_OPTIMAL
            , zero
            , ACCESS_DEPTH_STENCIL_ATTACHMENT_READ_BIT
              ⌄ ACCESS_DEPTH_STENCIL_ATTACHMENT_WRITE_BIT
            , PIPELINE_STAGE_TOP_OF_PIPE_BIT
            , PIPELINE_STAGE_EARLY_FRAGMENT_TESTS_BIT
            )
          Undef_ColorAtt →
            ( IMAGE_LAYOUT_UNDEFINED
            , IMAGE_LAYOUT_COLOR_ATTACHMENT_OPTIMAL
            , zero
            , ACCESS_COLOR_ATTACHMENT_READ_BIT
              ⌄ ACCESS_COLOR_ATTACHMENT_WRITE_BIT
            , PIPELINE_STAGE_TOP_OF_PIPE_BIT
            , PIPELINE_STAGE_COLOR_ATTACHMENT_OUTPUT_BIT
            )
      
      barrier = zero
        { oldLayout = oldLayout
        , newLayout = newLayout
        , srcQueueFamilyIndex = QUEUE_FAMILY_IGNORED
        , dstQueueFamilyIndex = QUEUE_FAMILY_IGNORED
        , image = image
        , subresourceRange = zero
            { aspectMask = IMAGE_ASPECT_COLOR_BIT
            , baseMipLevel = 0
            , levelCount = mipLevels
            , baseArrayLayer = 0
            , layerCount = 1
            }
        , srcAccessMask = srcAccess
        , dstAccessMask = dstAccess
        }
  
  cmdPipelineBarrier cmdBuf srcStage dstStage zero
    V.empty V.empty (V.singleton $ SomeStruct barrier)

createTextureSampler ∷ Device → PhysicalDevice → EngineM ε σ Sampler
createTextureSampler dev pdev = do
  props ← getPhysicalDeviceProperties pdev
  let samplerInfo = zero
        { magFilter = FILTER_NEAREST
        , minFilter = FILTER_NEAREST
        , addressModeU = SAMPLER_ADDRESS_MODE_REPEAT
        , addressModeV = SAMPLER_ADDRESS_MODE_REPEAT
        , addressModeW = SAMPLER_ADDRESS_MODE_REPEAT
        , anisotropyEnable = False
        , maxAnisotropy = 1
        , borderColor = BORDER_COLOR_INT_OPAQUE_BLACK
        , unnormalizedCoordinates = False
        , compareEnable = False
        , compareOp = COMPARE_OP_ALWAYS
        , mipmapMode = SAMPLER_MIPMAP_MODE_NEAREST
        , mipLodBias = 0
        , minLod = 0
        , maxLod = fromIntegral 0  -- disable mipmapping for now
        }
  
  allocResource (\s → destroySampler dev s Nothing) $
    createSampler dev samplerInfo Nothing

-- | Create a descriptor set for a texture
createTextureDescriptorSet ∷ Device 
                          → DescriptorPool 
                          → DescriptorSetLayout 
                          → ImageView 
                          → Sampler 
                          → EngineM ε σ DescriptorSet
createTextureDescriptorSet device pool layout textureImageView textureSampler = do
  -- Allocate descriptor set
  let allocInfo = zero 
        { descriptorPool = pool
        , setLayouts = V.singleton layout
        }
  
  descriptorSets ← allocateDescriptorSets device allocInfo
  
  -- Update descriptor set with texture info
  let imageInfo = zero 
        { imageView = textureImageView
        , imageLayout = IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL
        , sampler = textureSampler
        }
      writeSet = zero 
        { dstSet = V.head descriptorSets
        , dstBinding = 0
        , dstArrayElement = 0
        , descriptorType = DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER
        , imageInfo = V.singleton imageInfo
        }
  
  updateDescriptorSets device (V.singleton (SomeStruct writeSet)) V.empty
  
  pure $ V.head descriptorSets

createTextureDescriptorPool ∷ Device → EngineM ε σ DescriptorPool
createTextureDescriptorPool device = do
  let poolSize = zero
        { type' = DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER
        , descriptorCount = 1
        }
      poolInfo = zero
        { maxSets = 1
        , poolSizes = V.singleton poolSize
        }
  
  allocResource (\pool → destroyDescriptorPool device pool Nothing) $
    createDescriptorPool device poolInfo Nothing

createTextureDescriptorSetLayout ∷ Device → EngineM ε σ DescriptorSetLayout
createTextureDescriptorSetLayout device = do
  let binding = zero
        { binding = 0
        , descriptorType = DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER
        , descriptorCount = 1
        , stageFlags = SHADER_STAGE_FRAGMENT_BIT
        , immutableSamplers = V.empty
        }
      layoutInfo = zero
        { bindings = V.singleton binding
        }
  
  allocResource (\layout → destroyDescriptorSetLayout device layout Nothing) $
    createDescriptorSetLayout device layoutInfo Nothing

createTextureWithDescriptor ∷ Device → PhysicalDevice → CommandPool → Queue 
                           → FilePath → EngineM ε σ TextureData
createTextureWithDescriptor device pDevice cmdPool cmdQueue path = do
  (imageView, mipLevels) ← createTextureImageView pDevice device cmdPool cmdQueue path
  sampler ← createTextureSampler device pDevice
  
  -- Create descriptor resources
  descriptorPool ← createTextureDescriptorPool device
  descriptorSetLayout ← createTextureDescriptorSetLayout device
  descriptorSet ← createTextureDescriptorSet device descriptorPool 
                   descriptorSetLayout imageView sampler
  
  pure $ TextureData
    { tdImageView = imageView
    , tdSampler = sampler
    , tdMipLevels = mipLevels
    , tdDescriptorSet = descriptorSet
    }
