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

  image ← createVulkanImage dev pdev
    (width, height)
    FORMAT_R8G8B8A8_UNORM
    IMAGE_TILING_OPTIMAL
    (IMAGE_USAGE_TRANSFER_DST_BIT .|. IMAGE_USAGE_SAMPLED_BIT)
    MEMORY_PROPERTY_DEVICE_LOCAL_BIT

  locally $ do
    (stagingMem, stagingBuf) ← createVulkanBuffer dev pdev bufSize
      BUFFER_USAGE_TRANSFER_SRC_BIT
      (MEMORY_PROPERTY_HOST_VISIBLE_BIT .|. MEMORY_PROPERTY_HOST_COHERENT_BIT)
    
    stagingDataPtr ← mapMemory dev stagingMem 0 bufSize zero
    liftIO $ withForeignPtr imageDataForeignPtr $ \imageDataPtr →
      copyArray (castPtr stagingDataPtr) imageDataPtr imageDataLen
    unmapMemory dev stagingMem

    runCommandsOnce dev cmdPool cmdQueue $ \cmdBuf → do
      transitionImageLayout image FORMAT_R8G8B8A8_UNORM
        Undef_TransDst mipLevels cmdBuf
      copyBufferToImage cmdBuf stagingBuf image width height
      transitionImageLayout image FORMAT_R8G8B8A8_UNORM
        TransDst_ShaderRO mipLevels cmdBuf

  imageView ← createVulkanImageView dev image
    FORMAT_R8G8B8A8_UNORM IMAGE_ASPECT_COLOR_BIT

  -- No sampler: unallocated slots are written with the bindless
  -- system's shared 'btsTextureSampler'.
  pure $ UndefinedTexture
    { utImage     = image
    , utImageView = imageView
    }
