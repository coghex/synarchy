-- | Creates the default face-map texture: a 1×1 pure-green pixel
-- meaning "100% top-facing."  This occupies its own bindless slot.
module Engine.Graphics.Vulkan.Texture.DefaultFaceMap
  ( createDefaultFaceMap
  , DefaultFaceMap(..)
  ) where

import UPrelude
import qualified Data.Vector.Storable as Vec
import Foreign.Ptr (castPtr)
import Foreign.ForeignPtr (withForeignPtr)
import Foreign.Marshal.Array (copyArray)
import Engine.Core.Monad
import Engine.Core.Resource
import Engine.Asset.Handle (TextureHandle(..))
import Engine.Graphics.Vulkan.Image
import Engine.Graphics.Vulkan.Buffer
import Engine.Graphics.Vulkan.Command
import Engine.Graphics.Vulkan.Texture (transitionImageLayout, ImageLayoutTransition(..))
import Engine.Graphics.Vulkan.Texture.Bindless (registerTexture)
import Engine.Graphics.Vulkan.Texture.Slot (TextureSlot(..))
import Engine.Graphics.Vulkan.Texture.Handle (BindlessTextureHandle(..))
import Engine.Graphics.Vulkan.Texture.Types (BindlessTextureSystem(..))
import Vulkan.Core10
import Vulkan.Zero

-- | Result of creating the default face map
data DefaultFaceMap = DefaultFaceMap
  { dfmImage     ∷ VulkanImage
  , dfmImageView ∷ ImageView
  , dfmSampler   ∷ Sampler
  , dfmSlot      ∷ Word32       -- ^ Bindless slot index
  }
instance Show DefaultFaceMap where
  show (DefaultFaceMap _img view samp slot) =
    "DefaultFaceMap { imageView = " ++ show view ++
    ", sampler = " ++ show samp ++
    ", slot = " ++ show slot ++
    " }"

-- | The 1×1 pure-green pixel data: R=0 G=255 B=0 A=255
-- This encodes "100% top-facing" for the face-map shader.
defaultFaceMapData ∷ Vec.Vector Word8
defaultFaceMapData = Vec.fromList [0, 255, 0, 255]

-- | Create and register the default face-map texture.
-- Returns the DefaultFaceMap and the updated BindlessTextureSystem.
createDefaultFaceMap ∷ PhysicalDevice
                     → Device
                     → CommandPool
                     → Queue
                     → BindlessTextureSystem
                     → EngineM ε σ (DefaultFaceMap, BindlessTextureSystem)
createDefaultFaceMap pdev dev cmdPool cmdQueue bindless = do
  let width  = 1 ∷ Word32
      height = 1 ∷ Word32
      imageData = defaultFaceMapData
      (imageDataForeignPtr, imageDataLen) = Vec.unsafeToForeignPtr0 imageData
      bufSize = fromIntegral imageDataLen
      mipLevels = 1  -- No mipmapping for 1×1 texture

  -- Create the GPU-local image
  image ← createVulkanImage dev pdev
    (width, height)
    FORMAT_R8G8B8A8_UNORM
    IMAGE_TILING_OPTIMAL
    (IMAGE_USAGE_TRANSFER_DST_BIT .|. IMAGE_USAGE_SAMPLED_BIT)
    MEMORY_PROPERTY_DEVICE_LOCAL_BIT

  -- Upload via staging buffer (cleaned up after transfer)
  locally $ do
    (stagingMem, stagingBuf) ← createVulkanBuffer dev pdev bufSize
      BUFFER_USAGE_TRANSFER_SRC_BIT
      (MEMORY_PROPERTY_HOST_VISIBLE_BIT .|. MEMORY_PROPERTY_HOST_COHERENT_BIT)

    -- Copy pixel data to staging buffer
    stagingDataPtr ← mapMemory dev stagingMem 0 bufSize zero
    liftIO $ withForeignPtr imageDataForeignPtr $ \imageDataPtr →
      copyArray (castPtr stagingDataPtr) imageDataPtr imageDataLen
    unmapMemory dev stagingMem

    -- Transfer to GPU
    runCommandsOnce dev cmdPool cmdQueue $ \cmdBuf → do
      transitionImageLayout image FORMAT_R8G8B8A8_UNORM
        Undef_TransDst mipLevels cmdBuf
      copyBufferToImage cmdBuf stagingBuf image width height
      transitionImageLayout image FORMAT_R8G8B8A8_UNORM
        TransDst_ShaderRO mipLevels cmdBuf

  -- Create image view
  imageView ← createVulkanImageView dev image
    FORMAT_R8G8B8A8_UNORM IMAGE_ASPECT_COLOR_BIT

  -- Create sampler (nearest — it's 1×1 so filtering doesn't matter)
  let samplerInfo = zero
        { magFilter    = FILTER_NEAREST
        , minFilter    = FILTER_NEAREST
        , addressModeU = SAMPLER_ADDRESS_MODE_CLAMP_TO_EDGE
        , addressModeV = SAMPLER_ADDRESS_MODE_CLAMP_TO_EDGE
        , addressModeW = SAMPLER_ADDRESS_MODE_CLAMP_TO_EDGE
        , anisotropyEnable = False
        , maxAnisotropy    = 1
        , borderColor      = BORDER_COLOR_INT_OPAQUE_BLACK
        , unnormalizedCoordinates = False
        , compareEnable = False
        , compareOp     = COMPARE_OP_ALWAYS
        , mipmapMode    = SAMPLER_MIPMAP_MODE_NEAREST
        , mipLodBias    = 0
        , minLod        = 0
        , maxLod        = 0
        }
  sampler ← allocResource (\s → destroySampler dev s Nothing) $
    createSampler dev samplerInfo Nothing

  -- Register in the bindless system with a reserved TextureHandle
  -- Using a high handle value that won't collide with normal texture handles
  let faceMapTexHandle = TextureHandle 999999
  (mbHandle, newBindless) ← registerTexture dev faceMapTexHandle imageView sampler bindless

  let slot = case mbHandle of
        Just bHandle → tsIndex (bthSlot bHandle)
        Nothing      → 0  -- fallback to undefined

  pure ( DefaultFaceMap
           { dfmImage     = image
           , dfmImageView = imageView
           , dfmSampler   = sampler
           , dfmSlot      = slot
           }
       , newBindless
       )
