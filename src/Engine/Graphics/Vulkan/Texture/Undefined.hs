-- | Creates the "undefined" texture - a magenta/black checkerboard
-- that indicates missing or unloaded textures. This texture occupies
-- slot 0 in the bindless texture array.
module Engine.Graphics.Vulkan.Texture.Undefined
  ( createUndefinedTexture
  , undefinedTextureData
  ) where

import UPrelude
import qualified Data.Vector.Storable as Vec
import Foreign.Ptr (castPtr)
import Foreign.ForeignPtr (withForeignPtr)
import Foreign.Marshal.Array (copyArray)
import Engine.Core.Monad
import Engine.Core.Resource
import Engine.Graphics.Vulkan.Image
import Engine.Graphics.Vulkan.Buffer
import Engine.Graphics.Vulkan.Command
import Engine.Graphics.Vulkan.Texture (transitionImageLayout, ImageLayoutTransition(..))
import Engine.Graphics.Vulkan.Types.Texture (UndefinedTexture(..))
import Vulkan.Core10
import Vulkan.Zero

-- | The undefined texture size (8x8 pixels with 4x4 checker squares)
undefinedTextureWidth, undefinedTextureHeight ∷ Word32
undefinedTextureWidth = 8
undefinedTextureHeight = 8

-- | Generate the magenta/black checkerboard pattern
-- Returns RGBA8 pixel data as a storable vector
undefinedTextureData ∷ Vec.Vector Word8
undefinedTextureData = Vec.fromList pixels
  where
    -- Magenta: RGB(255, 0, 255), Black: RGB(0, 0, 0)
    magenta ∷ [Word8]
    magenta = [255, 0, 255, 255]  -- RGBA
    black ∷ [Word8]
    black = [0, 0, 0, 255]        -- RGBA
    
    -- 8x8 texture with 4x4 pixel checker squares (2x2 grid of squares)
    pixels = concat
      [ cell x y | y ← [0..7], x ← [0..7] ]
    
    cell ∷ Int → Int → [Word8]
    cell x y = 
      let evenCol = (x `div` 4) `mod` 2 ≡ 0
          evenRow = (y `div` 4) `mod` 2 ≡ 0
      in if evenCol ≡ evenRow then magenta else black

-- | Create the undefined texture with all necessary Vulkan resources
-- Returns UndefinedTexture containing image, view, and sampler
createUndefinedTexture ∷ PhysicalDevice 
                       → Device 
                       → CommandPool 
                       → Queue 
                       → EngineM ε σ UndefinedTexture
createUndefinedTexture pdev dev cmdPool cmdQueue = do
  let width = undefinedTextureWidth
      height = undefinedTextureHeight
      imageData = undefinedTextureData
      (imageDataForeignPtr, imageDataLen) = Vec.unsafeToForeignPtr0 imageData
      bufSize = fromIntegral imageDataLen
      mipLevels = 1  -- No mipmapping for this simple texture

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

  -- Create sampler (nearest filtering for crisp checkerboard)
  sampler ← createUndefinedTextureSampler dev

  pure $ UndefinedTexture
    { utImage     = image
    , utImageView = imageView
    , utSampler   = sampler
    }

-- | Create a sampler specifically for the undefined texture
-- Uses nearest filtering to keep the checkerboard crisp
createUndefinedTextureSampler ∷ Device → EngineM ε σ Sampler
createUndefinedTextureSampler dev = do
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
        , maxLod = 0
        }

  allocResource (\s → destroySampler dev s Nothing) $
    createSampler dev samplerInfo Nothing
