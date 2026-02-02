-- | Unified texture system that handles both bindless and legacy paths
module Engine.Graphics.Vulkan.Texture.System
  ( TextureSystemConfig(..)
  , createTextureSystem
  , loadTexture
  ) where

import UPrelude
import qualified Data.Text as T
import Engine.Core.Monad
import Engine.Core.Log (LogCategory(..))
import Engine.Core.Log.Monad (logInfoM, logDebugM, logAndThrowM)
import Engine.Core.Error.Exception (GraphicsError(..), ExceptionType(..))
import Engine.Asset.Handle (TextureHandle(..))
import Engine.Graphics.Vulkan.Image (VulkanImage)
import Engine.Graphics.Vulkan.Texture (createTextureImageView, createTextureSampler)
import Engine.Graphics.Vulkan.Texture.Bindless
import Engine.Graphics.Vulkan.Texture.Handle
import Engine.Graphics.Vulkan.Texture.Types
import Engine.Graphics.Vulkan.Texture.Slot (TextureSlot(..))
import Engine.Graphics.Vulkan.Capability
import Vulkan.Core10

-- | Create the texture system based on device capabilities
createTextureSystem ∷ PhysicalDevice
                    → Device
                    → CommandPool
                    → Queue
                    → TextureSystemConfig
                    → EngineM ε σ BindlessTextureSystem
createTextureSystem pdev dev cmdPool queue config = do
  -- Query device capabilities
  support ← liftIO $ queryBindlessSupport pdev
  let capability = determineTextureCapability support (tscReservedSlots config)

  case capability of
    BindlessTextures maxSlots | not (tscForceLegacy config) → do
      let actualMax = min 16384 (min maxSlots (tscMaxTextures config))
      let bindlessConfig = defaultBindlessConfig
            { bcMaxTextures = actualMax
            }
      
      bindless ← createBindlessTextureSystem pdev dev cmdPool queue bindlessConfig
      pure bindless

    _ → do
      logInfoM CatTexture "BINDLESS TEXTURES NOT SUPPORTED - LEGACY SYSTEM BROKEN!!!"
      logAndThrowM CatTexture (ExGraphics TextureLoadFailed) 
        "Legacy texture system is not implemented."

-- | Load a texture into the system
-- Returns the slot index for shader use
loadTexture ∷ Device
            → PhysicalDevice
            → CommandPool
            → Queue
            → TextureHandle
            → FilePath
            → BindlessTextureSystem
            → EngineM ε σ (Word32, BindlessTextureSystem)
loadTexture dev pdev cmdPool queue texHandle path system = do
    -- Load the texture image
    (vulkanImage, imageView, _mipLevels) ← 
      createTextureImageView pdev dev cmdPool queue path
    
    -- Create sampler
    sampler ← createTextureSampler dev pdev
    
    -- Register in bindless system
    (mbHandle, newBindless) ← registerTexture dev texHandle imageView sampler system
    
    case mbHandle of
      Just bHandle → do
        let slotIndex = tsIndex (bthSlot bHandle)
        logDebugM CatTexture $ "Loaded texture " <> T.pack path 
                  <> " at slot " <> T.pack (show slotIndex)
        pure (slotIndex, newBindless)
      Nothing → do
        logInfoM CatTexture $ "Failed to allocate slot for texture: " <> T.pack path
        pure (0, newBindless)  -- Return undefined texture slot
