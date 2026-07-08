module Engine.Graphics.Font.Upload where

import UPrelude
import qualified Data.Vector as V
import Data.IORef (readIORef)
import Engine.Asset.Manager (generateTextureHandle)
import Engine.Asset.Handle
import Engine.Graphics.Font.Data
import Engine.Graphics.Types
import Engine.Graphics.Vulkan.BufferUtils (createVulkanBufferManual)
import Engine.Graphics.Vulkan.Image (createVulkanImage, createVulkanImageView)
import Engine.Graphics.Vulkan.Command (runCommandsOnce)
import Engine.Graphics.Vulkan.Sampler.Cache (acquireSampler, SamplerKind(..))
import Engine.Graphics.Vulkan.Types.Texture
import Vulkan.Core10
import Vulkan.Zero
import Vulkan.CStruct.Extends
import Engine.Core.Log (LogCategory(..))
import Engine.Core.Monad
import Engine.Core.State
import Engine.Core.Resource (allocResource, locally)
import Engine.Core.Error.Exception (ExceptionType(..), GraphicsError(..))
import Engine.Core.Log.Monad (logDebugM, logAndThrowM)

-- | Create a descriptor pool dedicated to font atlas textures
-- Each font gets one descriptor set with one combined image sampler
createFontDescriptorPool ∷ Device → Word32 → EngineM ε σ DescriptorPool
createFontDescriptorPool device maxFonts = do
  let poolSize = zero
        { type' = DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER
        , descriptorCount = maxFonts  -- One sampler per font
        }
      poolInfo = zero
        { maxSets = fromIntegral maxFonts
        , poolSizes = V.singleton poolSize
        , flags = DESCRIPTOR_POOL_CREATE_FREE_DESCRIPTOR_SET_BIT
        }

  allocResource (\pool → destroyDescriptorPool device pool Nothing) $
    createDescriptorPool device poolInfo Nothing

-- * GPU Upload

uploadFontAtlasToGPU ∷ FontAtlas → DescriptorSetLayout
  → EngineM ε σ (TextureHandle, DescriptorSet, ImageView, Sampler)
uploadFontAtlasToGPU atlas fontDescriptorsLayout = do
    state ← gets graphicsState

    device ← case vulkanDevice state of
        Nothing → logAndThrowM CatFont (ExGraphics VulkanDeviceLost)
                                       "No device"
        Just d → pure d
    pDevice ← case vulkanPDevice state of
        Nothing → logAndThrowM CatFont (ExGraphics VulkanDeviceLost)
                                       "No physical device"
        Just pd → pure pd
    cmdPool ← case vulkanCmdPool state of
        Nothing → logAndThrowM CatFont (ExGraphics VulkanDeviceLost)
                                       "No command pool"
        Just pool → pure pool
    queues ← case deviceQueues state of
        Nothing → logAndThrowM CatFont (ExGraphics VulkanDeviceLost)
                                       "No device queues"
        Just qs → pure qs

    let width = faAtlasWidth atlas
        height = faAtlasHeight atlas
        pixels = faAtlasBitmap atlas
        queue = graphicsQueue queues

    (texHandle, descSet, imgView, samp) ← createFontTextureGrayscale device pDevice
                                            cmdPool queue width height pixels fontDescriptorsLayout

    return (texHandle, descSet, imgView, samp)

createFontTextureGrayscale ∷ Device → PhysicalDevice → CommandPool → Queue
                           → Int → Int → [Word8] → DescriptorSetLayout
                           → EngineM ε σ (TextureHandle, DescriptorSet, ImageView, Sampler)
createFontTextureGrayscale device pDevice cmdPool queue width height pixels fontDescLayout = do
    let bufferSize = fromIntegral $ width * height

    (stagingMemory, stagingBuffer) ← createVulkanBufferManual device pDevice bufferSize
        BUFFER_USAGE_TRANSFER_SRC_BIT
        (MEMORY_PROPERTY_HOST_VISIBLE_BIT .|. MEMORY_PROPERTY_HOST_COHERENT_BIT)

    dataPtr ← mapMemory device stagingMemory 0 bufferSize zero
    liftIO $ pokeArray (castPtr dataPtr) pixels
    unmapMemory device stagingMemory

    image ← createVulkanImage device pDevice
        (fromIntegral width, fromIntegral height)
        FORMAT_R8_UNORM
        IMAGE_TILING_OPTIMAL
        (IMAGE_USAGE_TRANSFER_DST_BIT .|. IMAGE_USAGE_SAMPLED_BIT)
        MEMORY_PROPERTY_DEVICE_LOCAL_BIT

    -- locally: the one-shot command buffer is freed when this scope
    -- exits instead of accumulating until program exit (fonts can be
    -- loaded at runtime).
    locally $ runCommandsOnce device cmdPool queue $ \cmdBuf → do
        transitionImageLayout cmdBuf (viImage image) FORMAT_R8_UNORM
            IMAGE_LAYOUT_UNDEFINED IMAGE_LAYOUT_TRANSFER_DST_OPTIMAL

        let region = zero
              { bufferOffset = 0
              , bufferRowLength = 0
              , bufferImageHeight = 0
              , imageSubresource = zero
                  { aspectMask = IMAGE_ASPECT_COLOR_BIT
                  , mipLevel = 0
                  , baseArrayLayer = 0
                  , layerCount = 1
                  }
              , imageOffset = Offset3D 0 0 0
              , imageExtent = Extent3D (fromIntegral width) (fromIntegral height) 1
              }
        cmdCopyBufferToImage cmdBuf stagingBuffer (viImage image)
                            IMAGE_LAYOUT_TRANSFER_DST_OPTIMAL (V.singleton region)

        transitionImageLayout cmdBuf (viImage image) FORMAT_R8_UNORM
            IMAGE_LAYOUT_TRANSFER_DST_OPTIMAL IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL

    imageView ← createVulkanImageView device image FORMAT_R8_UNORM IMAGE_ASPECT_COLOR_BIT

    -- Every font atlas shares one cached linear/clamp/mip-linear
    -- sampler instead of minting its own.
    cacheRef ← asks samplerCacheRef
    sampler ← liftIO $ acquireSampler device cacheRef SamplerFont
    state ← get
    fontPool ← case fontDescriptorPool (graphicsState state) of
        Nothing → logAndThrowM CatFont (ExGraphics DescriptorError)
                                       "Font descriptor pool not initialized"
        Just pool → pure pool

    let allocInfo = zero
          { descriptorPool = fontPool
          , setLayouts = V.singleton fontDescLayout
          }
    descriptorSets ← liftIO $ allocateDescriptorSets device allocInfo
    let descSet = V.head descriptorSets

    logDebugM CatFont "Descriptor set allocated for font texture"

    let imgInfo = zero
          { sampler = sampler
          , imageView = imageView
          , imageLayout = IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL
          }
        writeDescriptorSet = SomeStruct $ zero
          { dstSet = descSet
          , dstBinding = 0
          , dstArrayElement = 0
          , descriptorType = DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER
          , descriptorCount = 1
          , imageInfo = V.singleton imgInfo
          }

    updateDescriptorSets device (V.singleton writeDescriptorSet) V.empty

    poolRef ← asks assetPoolRef
    pool ← liftIO $ readIORef poolRef
    handle ← liftIO $ generateTextureHandle pool
    destroyBuffer device stagingBuffer Nothing
    freeMemory device stagingMemory Nothing
    return (handle, descSet, imageView, sampler)

-- * Helper Functions

transitionImageLayout ∷ CommandBuffer → Image → Format
                      → ImageLayout → ImageLayout → EngineM ε σ ()
transitionImageLayout cmdBuf image _format oldLayout newLayout = do
    let (srcAccess, dstAccess, srcStage, dstStage) = case (oldLayout, newLayout) of
          (IMAGE_LAYOUT_UNDEFINED, IMAGE_LAYOUT_TRANSFER_DST_OPTIMAL) →
              ( zero
              , ACCESS_TRANSFER_WRITE_BIT
              , PIPELINE_STAGE_TOP_OF_PIPE_BIT
              , PIPELINE_STAGE_TRANSFER_BIT
              )
          (IMAGE_LAYOUT_TRANSFER_DST_OPTIMAL, IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL) →
              ( ACCESS_TRANSFER_WRITE_BIT
              , ACCESS_SHADER_READ_BIT
              , PIPELINE_STAGE_TRANSFER_BIT
              , PIPELINE_STAGE_FRAGMENT_SHADER_BIT
              )
          _ →
              ( ACCESS_MEMORY_READ_BIT .|. ACCESS_MEMORY_WRITE_BIT
              , ACCESS_MEMORY_READ_BIT .|. ACCESS_MEMORY_WRITE_BIT
              , PIPELINE_STAGE_ALL_COMMANDS_BIT
              , PIPELINE_STAGE_ALL_COMMANDS_BIT
              )

    let barrier = zero
          { srcAccessMask = srcAccess
          , dstAccessMask = dstAccess
          , oldLayout = oldLayout
          , newLayout = newLayout
          , srcQueueFamilyIndex = QUEUE_FAMILY_IGNORED
          , dstQueueFamilyIndex = QUEUE_FAMILY_IGNORED
          , image = image
          , subresourceRange = zero
              { aspectMask = IMAGE_ASPECT_COLOR_BIT
              , baseMipLevel = 0
              , levelCount = 1
              , baseArrayLayer = 0
              , layerCount = 1
              }
          }

    cmdPipelineBarrier cmdBuf srcStage dstStage zero V.empty V.empty (V.singleton $ SomeStruct barrier)
