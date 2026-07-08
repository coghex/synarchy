module Engine.Graphics.Vulkan.Texture
  ( createTextureImageView
  , createTextureImageView'
  , createTextureFromRGBABytes
  , transitionImageLayout
  , ImageLayoutTransition(..)
  , module Engine.Graphics.Vulkan.Types.Texture
  ) where

import UPrelude
import qualified Codec.Picture as JP
import qualified Data.ByteString as BS
import qualified Data.Vector as V
import qualified Data.Text as T
import qualified Data.Vector.Storable as Vec
import Foreign.Marshal.Utils (copyBytes)
import Engine.Core.Monad
import Engine.Core.Resource
import Engine.Core.Log (LogCategory(..))
import Engine.Core.Log.Monad (logAndThrowM, logDebugM, logDebugSM)
import Engine.Core.Error.Exception
import Engine.Graphics.Vulkan.Image (createVulkanImage, createVulkanImageView
                                    , createVulkanImage', createVulkanImageView'
                                    , copyBufferToImage)
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
                      → Queue → FilePath → EngineM ε σ (VulkanImage, ImageView)
createTextureImageView pdev dev cmdPool cmdQueue path = do
  JP.Image { JP.imageWidth, JP.imageHeight, JP.imageData }
    ← liftIO (JP.readImage path) ⌦ \case
      Left err → logAndThrowM CatTexture (ExGraphics TextureLoadFailed)
                   $ "cannot load texture image: " <> T.pack err
      Right dynImg → pure $ JP.convertRGBA8 dynImg
  let (imageDataForeignPtr, imageDataLen)
        = Vec.unsafeToForeignPtr0 imageData
      bufSize = fromIntegral imageDataLen
  image ← createVulkanImage dev pdev
    (fromIntegral imageWidth, fromIntegral imageHeight)
    FORMAT_R8G8B8A8_UNORM
    IMAGE_TILING_OPTIMAL
    (IMAGE_USAGE_TRANSFER_DST_BIT
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
        Undef_TransDst 1 cmdBuf
      copyBufferToImage cmdBuf stagingBuf image
        (fromIntegral imageWidth) (fromIntegral imageHeight)
      transitionImageLayout image FORMAT_R8G8B8A8_UNORM
        TransDst_ShaderRO 1 cmdBuf

  imageView ← createVulkanImageView dev image
    FORMAT_R8G8B8A8_UNORM IMAGE_ASPECT_COLOR_BIT
  return (image, imageView)

-- | Create a texture image view with cleanup action
createTextureImageView' ∷ PhysicalDevice → Device → CommandPool
                       → Queue → FilePath
                       → EngineM ε σ ((VulkanImage, ImageView)
                                      , IO ())
createTextureImageView' pdev dev cmdPool cmdQueue path = do
  logDebugSM CatTexture "Loading texture image"
    [("path", T.pack path)]

  let maxTimeout = maxBound ∷ Word64
  JP.Image { JP.imageWidth, JP.imageHeight, JP.imageData }
    ← liftIO (JP.readImage path) ⌦ \case
      Left err → do
        logAndThrowM CatTexture (ExGraphics TextureLoadFailed)
                   $ "cannot load texture image: " <> T.pack err
      Right dynImg → pure $ JP.convertRGBA8 dynImg

  let (imageDataForeignPtr, imageDataLen) = Vec.unsafeToForeignPtr0 imageData
      bufSize = fromIntegral imageDataLen

  logDebugSM CatTexture "Texture image loaded"
    [("width", T.pack $ show imageWidth)
    ,("height", T.pack $ show imageHeight)
    ,("format", "RGBA8")]

  logDebugM CatTexture "Creating Vulkan image"
  (vulkanImage@(VulkanImage image imagedata), imageCleanup) ← createVulkanImage' dev pdev
    (fromIntegral imageWidth, fromIntegral imageHeight)
    FORMAT_R8G8B8A8_UNORM
    IMAGE_TILING_OPTIMAL
    (IMAGE_USAGE_TRANSFER_DST_BIT
    ⌄ IMAGE_USAGE_SAMPLED_BIT)
    MEMORY_PROPERTY_DEVICE_LOCAL_BIT

  let fenceInfo = ( zero ∷ FenceCreateInfo '[] )
                    { flags = FENCE_CREATE_SIGNALED_BIT }
  fence ← createFence dev fenceInfo Nothing
    
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

    -- Result discard is safe: timeout is maxBound, so the only
    -- non-SUCCESS outcomes throw as VulkanException.
    _ ← waitForFences dev (V.singleton fence) True maxTimeout
    resetFences dev (V.singleton fence)
    
    logDebugM CatTexture "Copying buffer to image and transitioning layout"
    let commandInfo = (zero ∷ CommandBufferBeginInfo '[])
          { flags = COMMAND_BUFFER_USAGE_ONE_TIME_SUBMIT_BIT }
    
    cmdBuf ← allocateCommandBuffers dev
        (zero { commandPool = cmdPool
             , level = COMMAND_BUFFER_LEVEL_PRIMARY
             , commandBufferCount = 1 })
        ⌦ pure . V.head

    beginCommandBuffer cmdBuf commandInfo
    
    transitionImageLayout vulkanImage FORMAT_R8G8B8A8_UNORM
        Undef_TransDst 1 cmdBuf
    copyBufferToImage cmdBuf stagingBuf vulkanImage
        (fromIntegral imageWidth) (fromIntegral imageHeight)
    transitionImageLayout vulkanImage FORMAT_R8G8B8A8_UNORM
        TransDst_ShaderRO 1 cmdBuf

    endCommandBuffer cmdBuf

    let submitInfo = (zero ∷ SubmitInfo '[])
          { commandBuffers = V.singleton (commandBufferHandle cmdBuf) }
    queueSubmit cmdQueue (V.singleton $ SomeStruct submitInfo) fence

    _ ← waitForFences dev (V.singleton fence) True maxBound
    freeCommandBuffers dev cmdPool (V.singleton cmdBuf)
  destroyFence dev fence Nothing

  (imageView, viewCleanup) ← allocResource'IO
    (\view → liftIO $ destroyImageView dev view Nothing)
    (createVulkanImageView dev (VulkanImage image imagedata)
      FORMAT_R8G8B8A8_UNORM IMAGE_ASPECT_COLOR_BIT)

  let cleanup = do
        viewCleanup
        imageCleanup

  pure ((vulkanImage, imageView), cleanup)

-- | Create a GPU texture straight from RGBA8 pixel bytes already in
--   memory — no file I/O, unlike 'createTextureImageView''. The
--   runtime-generated-texture counterpart, for content whose pixels
--   come from a procedural generator rather than disk (blood decals,
--   #606). Mirrors the staging-buffer upload
--   Engine.Scripting.Lua.Message's handleWorldPreview /
--   handleZoomAtlasUpload already do inline for the world-preview and
--   zoom-atlas textures; factored here so it isn't re-derived a third
--   time.
createTextureFromRGBABytes ∷ PhysicalDevice → Device → CommandPool → Queue
                          → (Int, Int) → BS.ByteString
                          → EngineM ε σ ((VulkanImage, ImageView), IO ())
createTextureFromRGBABytes pdev dev cmdPool cmdQueue (w, h) rgba = do
  let width   = fromIntegral w ∷ Word32
      height  = fromIntegral h ∷ Word32
      bufSize = fromIntegral (BS.length rgba)

  (image, cleanImage) ← createVulkanImage' dev pdev
    (width, height)
    FORMAT_R8G8B8A8_UNORM
    IMAGE_TILING_OPTIMAL
    (IMAGE_USAGE_TRANSFER_DST_BIT
    ⌄ IMAGE_USAGE_SAMPLED_BIT)
    MEMORY_PROPERTY_DEVICE_LOCAL_BIT

  locally $ do
    (stagingMem, stagingBuf) ← createVulkanBuffer dev pdev bufSize
      BUFFER_USAGE_TRANSFER_SRC_BIT
      (MEMORY_PROPERTY_HOST_VISIBLE_BIT
      ⌄ MEMORY_PROPERTY_HOST_COHERENT_BIT)
    stagingDataPtr ← mapMemory dev stagingMem 0 bufSize zero
    liftIO $ BS.useAsCStringLen rgba $ \(srcPtr, len) →
      copyBytes (castPtr stagingDataPtr) srcPtr len
    unmapMemory dev stagingMem

    runCommandsOnce dev cmdPool cmdQueue $ \cmdBuf → do
      transitionImageLayout image FORMAT_R8G8B8A8_UNORM
        Undef_TransDst 1 cmdBuf
      copyBufferToImage cmdBuf stagingBuf image width height
      transitionImageLayout image FORMAT_R8G8B8A8_UNORM
        TransDst_ShaderRO 1 cmdBuf

  (imageView, cleanView) ← createVulkanImageView' dev image
    FORMAT_R8G8B8A8_UNORM IMAGE_ASPECT_COLOR_BIT

  pure ((image, imageView), cleanView >> cleanImage)

transitionImageLayout ∷ VulkanImage → Format → ImageLayoutTransition
                     → Word32 → CommandBuffer → EngineM ε σ ()
transitionImageLayout (VulkanImage image _) _format transition
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
