-- | Unified texture system that handles both bindless and legacy paths
module Engine.Graphics.Vulkan.Texture.System
  ( TextureSystemConfig(..)
  , TextureSystem(..)
  , createTextureSystem
  , destroyTextureSystem
  , loadTexture
  , unloadTexture
  , getTextureIndex
  , getDescriptorSet
  , getDescriptorLayout
  , isBindlessEnabled
  ) where

import UPrelude
import qualified Data.Text as T
import Engine.Core.Monad
import Engine.Core.Error.Exception
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
                    → EngineM ε σ TextureSystem
createTextureSystem pdev dev cmdPool queue config = do
  -- Query device capabilities
  support ← liftIO $ queryBindlessSupport pdev
  let capability = determineTextureCapability support (tscReservedSlots config)

  logDebug $ "Bindless support: " ⧺ (show support)
  logDebug $ "Texture capability: " ⧺ (show (describeCapability capability))

  case capability of
    BindlessTextures maxSlots | not (tscForceLegacy config) → do
      logInfo $ "Using bindless texture system with " 
                ⧺ (show maxSlots) ⧺ " slots"
      
      let actualMax = min 16384 (min maxSlots (tscMaxTextures config))
      let bindlessConfig = defaultBindlessConfig
            { bcMaxTextures = actualMax
            }
      
      bindless ← createBindlessTextureSystem pdev dev cmdPool queue bindlessConfig
      pure $ BindlessSystem bindless

    _ → do
      logInfo "Using legacy bounded texture system"
      pure LegacySystem

-- | Destroy the texture system
destroyTextureSystem ∷ Device → TextureSystem → EngineM ε σ ()
destroyTextureSystem dev (BindlessSystem sys) = destroyBindlessTextureSystem dev sys
destroyTextureSystem _   LegacySystem         = pure ()

-- | Load a texture into the system
-- Returns the slot index for shader use
loadTexture ∷ Device
            → PhysicalDevice
            → CommandPool
            → Queue
            → TextureHandle
            → FilePath
            → TextureSystem
            → EngineM ε σ (Word32, TextureSystem)
loadTexture dev pdev cmdPool queue texHandle path system = do
  case system of
    BindlessSystem bindless → do
      -- Load the texture image
      (vulkanImage, imageView, _mipLevels) ← 
        createTextureImageView pdev dev cmdPool queue path
      
      -- Create sampler
      sampler ← createTextureSampler dev pdev
      
      -- Register in bindless system
      (mbHandle, newBindless) ← registerTexture dev texHandle imageView sampler bindless
      
      case mbHandle of
        Just bHandle → do
          let slotIndex = tsIndex (bthSlot bHandle)
          logDebug $ "Loaded texture " ⧺ path 
                    ⧺ " at slot " ⧺ (show slotIndex)
          pure (slotIndex, BindlessSystem newBindless)
        Nothing → do
          logInfo $ "Failed to allocate slot for texture: " ⧺ path
          pure (0, BindlessSystem newBindless)  -- Return undefined texture slot
    
    LegacySystem → do
      -- Legacy path - would use your existing texture loading
      logInfo "Legacy texture loading not implemented"
      pure (0, LegacySystem)

-- | Unload a texture from the system
unloadTexture ∷ Device
              → TextureHandle
              → TextureSystem
              → EngineM ε σ TextureSystem
unloadTexture dev texHandle system = do
  case system of
    BindlessSystem bindless → do
      newBindless ← unregisterTexture dev texHandle bindless
      pure $ BindlessSystem newBindless
    LegacySystem → pure LegacySystem

-- | Get the slot index for a texture (0 = undefined/missing)
getTextureIndex ∷ TextureHandle → TextureSystem → Word32
getTextureIndex texHandle (BindlessSystem sys) = getTextureSlotIndex texHandle sys
getTextureIndex _ LegacySystem = 0

-- | Get the descriptor set for binding
getDescriptorSet ∷ TextureSystem → Maybe DescriptorSet
getDescriptorSet (BindlessSystem sys) = Just $ btsDescriptorSet sys
getDescriptorSet LegacySystem = Nothing

-- | Get the descriptor set layout
getDescriptorLayout ∷ TextureSystem → Maybe DescriptorSetLayout
getDescriptorLayout (BindlessSystem sys) = Just $ btsDescriptorLayout sys
getDescriptorLayout LegacySystem = Nothing

-- | Check if bindless is enabled
isBindlessEnabled ∷ TextureSystem → Bool
isBindlessEnabled (BindlessSystem _) = True
isBindlessEnabled LegacySystem = False
