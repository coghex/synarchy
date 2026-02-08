-- src/Engine/Graphics/Vulkan/Texture.hs
module Engine.Graphics.Vulkan.Texture
  ( createTextureImageView
  , createTextureImageView'
  , createTextureSampler
  , createTextureSampler'
  , transitionImageLayout
  , ImageLayoutTransition(..)
  , module Engine.Graphics.Vulkan.Types.Texture
  ) where

import UPrelude
import qualified Codec.Picture as JP
import qualified Data.Vector as V
import qualified Data.Text as T
import qualified Data.Vector.Storable as Vec
import Engine.Asset.Types
import Engine.Core.Monad
import Engine.Core.Resource
import Engine.Core.Log (LogCategory(..))
import Engine.Core.Log.Monad (logAndThrowM, logDebugM, logDebugSM, logInfoM)
import Engine.Core.Error.Exception
import Engine.Graphics.Types
import Engine.Graphics.Vulkan.Image (createVulkanImage, createVulkanImageView
                                    , createVulkanImage'
                                    , copyBufferToImage, VulkanImage(..))
import Engine.Graphics.Vulkan.Buffer
import Engine.Graphics.Vulkan.Command
import Engine.Graphics.Vulkan.Descriptor
import Engine.Graphics.Vulkan.Types.Texture
import Vulkan.Core10
import Vulkan.Zero
import Vulkan.CStruct.Extends

data ImageLayoutTransition = Undef_TransDst
                          | TransDst_ShaderRO
                          | Undef_DepthStencilAtt
                          | Undef_ColorAtt

createTextureImageView ∷ PhysicalDevice → Device → CommandPool
                      → Queue → FilePath → EngineM ε σ (VulkanImage, ImageView, Word32)
createTextureImageView pdev dev cmdPool cmdQueue path = do
  JP.Image { JP.imageWidth, JP.imageHeight, JP.imageData }
    ← liftIO (JP.readImage path) ⌦ \case
      Left err → logAndThrowM CatTexture (ExGraphics TextureLoadFailed)
                   $ "cannot load texture image: " <> T.pack err
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
  return (image, imageView, mipLevels)

-- | Create a texture image view with cleanup action
createTextureImageView' ∷ PhysicalDevice → Device → CommandPool
                       → Queue → FilePath 
                       → EngineM ε σ ((VulkanImage, ImageView, Word32)
                                      , IO ())
createTextureImageView' pdev dev cmdPool cmdQueue path = do
  logDebugSM CatTexture "Loading texture image"
    [("path", T.pack path)]
  
  let maxTimeout = maxBound ∷ Word64
  -- Load and convert image data
  JP.Image { JP.imageWidth, JP.imageHeight, JP.imageData }
    ← liftIO (JP.readImage path) ⌦ \case
      Left err → do
        logAndThrowM CatTexture (ExGraphics TextureLoadFailed)
                   $ "cannot load texture image: " <> T.pack err
      Right dynImg → pure $ JP.convertRGBA8 dynImg

  let (imageDataForeignPtr, imageDataLen) = Vec.unsafeToForeignPtr0 imageData
      bufSize = fromIntegral imageDataLen
      mipLevels = (floor ∘ logBase (2 ∷ Float)
                ∘ fromIntegral $ max imageWidth imageHeight) + 1
  
  logDebugSM CatTexture "Texture image loaded"
    [("width", T.pack $ show imageWidth)
    ,("height", T.pack $ show imageHeight)
    ,("format", "RGBA8")
    ,("mip_levels", T.pack $ show mipLevels)]

  -- Create the image with cleanup
  logDebugM CatTexture "Creating Vulkan image"
  (vulkanImage@(VulkanImage image imagedata), imageCleanup) ← createVulkanImage' dev pdev
    (fromIntegral imageWidth, fromIntegral imageHeight)
    FORMAT_R8G8B8A8_UNORM
    IMAGE_TILING_OPTIMAL
    (IMAGE_USAGE_TRANSFER_SRC_BIT
    ⌄ IMAGE_USAGE_TRANSFER_DST_BIT
    ⌄ IMAGE_USAGE_SAMPLED_BIT)
    MEMORY_PROPERTY_DEVICE_LOCAL_BIT

  -- Create a fence for synchronization
  let fenceInfo = ( zero ∷ FenceCreateInfo '[] )
                    { flags = FENCE_CREATE_SIGNALED_BIT }
  fence ← createFence dev fenceInfo Nothing
    
  -- Handle staging buffer with locally for automatic cleanup
  locally $ do
    logDebugSM CatTexture "Allocating staging buffer"
      [("size", T.pack $ show bufSize)]
    (stagingMem, stagingBuf) ← createVulkanBuffer dev pdev bufSize
      BUFFER_USAGE_TRANSFER_SRC_BIT
      (MEMORY_PROPERTY_HOST_VISIBLE_BIT
      ⌄ MEMORY_PROPERTY_HOST_COHERENT_BIT)
    stagingDataPtr ← mapMemory dev stagingMem 0 bufSize zero
    liftIO $ withForeignPtr imageDataForeignPtr
      $ \imageDataPtr → copyArray (castPtr stagingDataPtr)
                          imageDataPtr imageDataLen
    unmapMemory dev stagingMem

    -- Wait for the fence before proceeding
    waitForFences dev (V.singleton fence) True maxTimeout
    resetFences dev (V.singleton fence)
    
    logDebugM CatTexture "Copying buffer to image and transitioning layout"
    -- Record and submit commands
    let commandInfo = (zero ∷ CommandBufferBeginInfo '[])
          { flags = COMMAND_BUFFER_USAGE_ONE_TIME_SUBMIT_BIT }
    
    cmdBuf ← allocateCommandBuffers dev
        (zero { commandPool = cmdPool
             , level = COMMAND_BUFFER_LEVEL_PRIMARY
             , commandBufferCount = 1 })
        >>= pure . V.head

    beginCommandBuffer cmdBuf commandInfo
    
    -- Transition image layout
    transitionImageLayout vulkanImage FORMAT_R8G8B8A8_UNORM
        Undef_TransDst mipLevels cmdBuf

    -- Copy buffer to image
    copyBufferToImage cmdBuf stagingBuf vulkanImage
        (fromIntegral imageWidth) (fromIntegral imageHeight)

    -- Transition to shader read
    transitionImageLayout vulkanImage FORMAT_R8G8B8A8_UNORM
        TransDst_ShaderRO mipLevels cmdBuf

    endCommandBuffer cmdBuf

    -- Submit command buffer with fence
    let submitInfo = (zero ∷ SubmitInfo '[])
          { commandBuffers = V.singleton (commandBufferHandle cmdBuf) }
    queueSubmit cmdQueue (V.singleton $ SomeStruct submitInfo) fence

    -- Wait for completion before cleanup
    waitForFences dev (V.singleton fence) True maxBound
    
    -- Free command buffer
    freeCommandBuffers dev cmdPool (V.singleton cmdBuf)
  -- Cleanup fence
  destroyFence dev fence Nothing

  -- Create image view with cleanup
  (imageView, viewCleanup) ← allocResource'IO
    (\view → liftIO $ destroyImageView dev view Nothing)
    (createVulkanImageView dev (VulkanImage image imagedata)
      FORMAT_R8G8B8A8_UNORM IMAGE_ASPECT_COLOR_BIT)

  -- Combine cleanup actions
  let cleanup = do
        viewCleanup    -- First cleanup the view
        imageCleanup   -- Then cleanup the image and memory

  pure ((vulkanImage, imageView, mipLevels), cleanup)

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

createTextureSampler ∷ Device → PhysicalDevice → Filter → EngineM ε σ Sampler
createTextureSampler dev pdev filterMode = do
  props ← getPhysicalDeviceProperties pdev
  let addrMode = case filterMode of
        FILTER_LINEAR → SAMPLER_ADDRESS_MODE_CLAMP_TO_EDGE
        _             → SAMPLER_ADDRESS_MODE_REPEAT
      samplerInfo = zero
        { magFilter = filterMode
        , minFilter = filterMode
        , addressModeU = addrMode
        , addressModeV = addrMode
        , addressModeW = addrMode
        , anisotropyEnable = False
        , maxAnisotropy = 1
        , borderColor = BORDER_COLOR_INT_OPAQUE_BLACK
        , unnormalizedCoordinates = False
        , compareEnable = False
        , compareOp = COMPARE_OP_ALWAYS
        , mipmapMode = SAMPLER_MIPMAP_MODE_NEAREST
        , mipLodBias = 0
        , minLod = 0
        , maxLod = 0
        }
  
  allocResource (\s → destroySampler dev s Nothing) $
    createSampler dev samplerInfo Nothing

createTextureSampler' ∷ Device → PhysicalDevice → Filter → EngineM ε σ (Sampler, IO ())
createTextureSampler' dev pdev filterMode = do
  logDebugM CatTexture "Creating texture sampler"
  props ← getPhysicalDeviceProperties pdev
  let addrMode = case filterMode of
        FILTER_LINEAR → SAMPLER_ADDRESS_MODE_CLAMP_TO_EDGE
        _             → SAMPLER_ADDRESS_MODE_REPEAT
      samplerInfo = zero
        { magFilter = filterMode
        , minFilter = filterMode
        , addressModeU = addrMode
        , addressModeV = addrMode
        , addressModeW = addrMode
        , anisotropyEnable = False
        , maxAnisotropy = 1
        , borderColor = BORDER_COLOR_INT_OPAQUE_BLACK
        , unnormalizedCoordinates = False
        , compareEnable = False
        , compareOp = COMPARE_OP_ALWAYS
        , mipmapMode = SAMPLER_MIPMAP_MODE_NEAREST
        , mipLodBias = 0
        , minLod = 0
        , maxLod = 0
        }
  
  allocResource'IO (\s → destroySampler dev s Nothing) $
    createSampler dev samplerInfo Nothing
