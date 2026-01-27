-- | Bindless texture system using UPDATE_AFTER_BIND descriptors
-- This enables up to 1 million texture slots on MoltenVK/Metal
module Engine.Graphics.Vulkan.Texture.Bindless
  ( -- * Types (re-exported from Types module)
    BindlessTextureSystem(..)
  , BindlessConfig(..)
  , defaultBindlessConfig
    -- * Creation/Destruction
  , createBindlessTextureSystem
  , destroyBindlessTextureSystem
    -- * Texture Management
  , registerTexture
  , unregisterTexture
  , getTextureSlotIndex
  ) where

import UPrelude
import qualified Data.Vector as V
import qualified Data.Map.Strict as Map
import Engine.Core.Monad
import Engine.Core.Resource
import Engine.Core.Error.Exception
import Engine.Asset.Handle (TextureHandle(..))
import Engine.Graphics.Vulkan.Texture.Slot
import Engine.Graphics.Vulkan.Texture.Handle
import Engine.Graphics.Vulkan.Texture.Undefined (createUndefinedTexture)
import Engine.Graphics.Vulkan.Texture.Types (BindlessTextureSystem(..), BindlessConfig(..))
import Engine.Graphics.Vulkan.Types.Texture (UndefinedTexture(..))
import Vulkan.Core10
import Vulkan.Core12
import Vulkan.Zero
import Vulkan.CStruct.Extends

-- | Sensible defaults for bindless config
-- Note: This MUST match the shader array size!
defaultBindlessConfig ∷ BindlessConfig
defaultBindlessConfig = BindlessConfig
  { bcMaxTextures    = 16384  -- Must match shader: sampler2D textures[16384]
  , bcTextureBinding = 0
  , bcDescriptorSet  = 1
  }

-- | Create the bindless texture system
createBindlessTextureSystem ∷ PhysicalDevice
                            → Device
                            → CommandPool
                            → Queue
                            → BindlessConfig
                            → EngineM ε σ BindlessTextureSystem
createBindlessTextureSystem pdev dev cmdPool cmdQueue config = do
  -- Create the undefined texture first (slot 0)
  undefinedTex ← createUndefinedTexture pdev dev cmdPool cmdQueue

  -- Create descriptor pool with UPDATE_AFTER_BIND flag
  descriptorPool ← createBindlessDescriptorPool dev config

  -- Create descriptor set layout with UPDATE_AFTER_BIND flags (but NOT variable count)
  descriptorLayout ← createBindlessDescriptorSetLayout dev config

  -- Allocate the single descriptor set
  descriptorSet ← allocateBindlessDescriptorSet dev descriptorPool descriptorLayout config

  -- Initialize slot allocator (slot 0 reserved for undefined)
  let slotAllocator = createSlotAllocator (bcMaxTextures config)

  -- Initialize ALL slots to the undefined texture
  -- This is required for MoltenVK argument buffer compatibility
  initializeAllSlots dev descriptorSet config 
    (utImageView undefinedTex) (utSampler undefinedTex)
  pure $ BindlessTextureSystem
    { btsConfig           = config
    , btsDescriptorPool   = descriptorPool
    , btsDescriptorLayout = descriptorLayout
    , btsDescriptorSet    = descriptorSet
    , btsSlotAllocator    = slotAllocator
    , btsUndefinedTexture = undefinedTex
    , btsHandleMap        = Map.empty
    }

-- | Initialize all descriptor slots with the undefined texture
-- Required for MoltenVK argument buffer compatibility
initializeAllSlots ∷ Device → DescriptorSet → BindlessConfig 
                   → ImageView → Sampler → EngineM ε σ ()
initializeAllSlots dev descSet config imageView sampler = do
  -- Build image info for all slots
  let maxSlots = bcMaxTextures config
      imageInfo = zero
        { imageLayout = IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL
        , imageView = imageView
        , sampler = sampler
        }
      -- Create a vector of image infos, one per slot
      imageInfos = V.replicate (fromIntegral maxSlots) imageInfo

      write = zero
        { dstSet = descSet
        , dstBinding = bcTextureBinding config
        , dstArrayElement = 0
        , descriptorCount = maxSlots
        , descriptorType = DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER
        , imageInfo = imageInfos
        }

  updateDescriptorSets dev (V.singleton $ SomeStruct write) V.empty

-- | Create descriptor pool with UPDATE_AFTER_BIND support
createBindlessDescriptorPool ∷ Device → BindlessConfig → EngineM ε σ DescriptorPool
createBindlessDescriptorPool dev config = do
  let poolSize = zero
        { type' = DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER
        , descriptorCount = bcMaxTextures config
        }
      
      poolInfo = zero
        { maxSets = 1
        , poolSizes = V.singleton poolSize
        , flags = DESCRIPTOR_POOL_CREATE_UPDATE_AFTER_BIND_BIT
        }

  allocResource (\pool → destroyDescriptorPool dev pool Nothing) $
    createDescriptorPool dev poolInfo Nothing

-- | Create descriptor set layout with bindless flags
-- Note: We do NOT use VARIABLE_DESCRIPTOR_COUNT for MoltenVK compatibility
createBindlessDescriptorSetLayout ∷ Device → BindlessConfig → EngineM ε σ DescriptorSetLayout
createBindlessDescriptorSetLayout dev config = do
  -- Binding flags for bindless operation
  -- NOT using VARIABLE_DESCRIPTOR_COUNT - MoltenVK has issues with it
  let bindingFlags = 
        DESCRIPTOR_BINDING_PARTIALLY_BOUND_BIT
        .|. DESCRIPTOR_BINDING_UPDATE_AFTER_BIND_BIT

      bindingFlagsInfo = zero
        { bindingFlags = V.singleton bindingFlags
        } ∷ DescriptorSetLayoutBindingFlagsCreateInfo

      binding = zero
        { binding = bcTextureBinding config
        , descriptorType = DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER
        , descriptorCount = bcMaxTextures config
        , stageFlags = SHADER_STAGE_FRAGMENT_BIT
        , immutableSamplers = V.empty
        }

      layoutInfo = zero
        { bindings = V.singleton binding
        , flags = DESCRIPTOR_SET_LAYOUT_CREATE_UPDATE_AFTER_BIND_POOL_BIT
        }
        ::& bindingFlagsInfo
        :& ()

  allocResource (\layout → destroyDescriptorSetLayout dev layout Nothing) $
    createDescriptorSetLayout dev layoutInfo Nothing

-- | Allocate the bindless descriptor set
-- Note: Not using variable descriptor count for MoltenVK compatibility
allocateBindlessDescriptorSet ∷ Device 
                              → DescriptorPool 
                              → DescriptorSetLayout 
                              → BindlessConfig
                              → EngineM ε σ DescriptorSet
allocateBindlessDescriptorSet dev pool layout _config = do
  let allocInfo = zero
        { descriptorPool = pool
        , setLayouts = V.singleton layout
        }

  sets ← allocateDescriptorSets dev allocInfo
  pure $ V.head sets

-- | Write a texture to a specific slot in the descriptor set
writeDescriptorSlot ∷ Device 
                    → DescriptorSet 
                    → BindlessConfig
                    → Word32
                    → ImageView 
                    → Sampler 
                    → EngineM ε σ ()
writeDescriptorSlot dev descSet config slotIndex imageView sampler = do
  let imageInfo = zero
        { imageLayout = IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL
        , imageView = imageView
        , sampler = sampler
        }

      write = zero
        { dstSet = descSet
        , dstBinding = bcTextureBinding config
        , dstArrayElement = slotIndex
        , descriptorCount = 1
        , descriptorType = DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER
        , imageInfo = V.singleton imageInfo
        }

  updateDescriptorSets dev (V.singleton $ SomeStruct write) V.empty

-- | Register a texture in the bindless system
registerTexture ∷ Device
                → TextureHandle
                → ImageView
                → Sampler
                → BindlessTextureSystem
                → EngineM ε σ (Maybe BindlessTextureHandle, BindlessTextureSystem)
registerTexture dev texHandle imageView sampler system = do
  case Map.lookup texHandle (btsHandleMap system) of
    Just existingHandle → pure (Just existingHandle, system)
    Nothing → do
      case allocateSlot (btsSlotAllocator system) of
        Nothing → pure (Nothing, system)
        Just (slot, newAllocator) → do
          writeDescriptorSlot dev (btsDescriptorSet system) (btsConfig system)
            (tsIndex slot) imageView sampler

          let bindlessHandle = toBindlessHandle slot texHandle
              newHandleMap = Map.insert texHandle bindlessHandle (btsHandleMap system)
              newSystem = system
                { btsSlotAllocator = newAllocator
                , btsHandleMap = newHandleMap
                }

          pure (Just bindlessHandle, newSystem)

-- | Unregister a texture from the bindless system
unregisterTexture ∷ Device
                  → TextureHandle
                  → BindlessTextureSystem
                  → EngineM ε σ BindlessTextureSystem
unregisterTexture dev texHandle system = do
  case Map.lookup texHandle (btsHandleMap system) of
    Nothing → pure system
    Just bindlessHandle → do
      let slot = bthSlot bindlessHandle
          
      writeDescriptorSlot dev (btsDescriptorSet system) (btsConfig system)
        (tsIndex slot) 
        (utImageView $ btsUndefinedTexture system)
        (utSampler $ btsUndefinedTexture system)

      let newAllocator = freeSlot slot (btsSlotAllocator system)
          newHandleMap = Map.delete texHandle (btsHandleMap system)

      pure $ system
        { btsSlotAllocator = newAllocator
        , btsHandleMap = newHandleMap
        }

-- | Get the slot index for a texture handle
getTextureSlotIndex ∷ TextureHandle → BindlessTextureSystem → Word32
getTextureSlotIndex texHandle system =
  case Map.lookup texHandle (btsHandleMap system) of
    Just bindlessHandle → fromBindlessHandle bindlessHandle
    Nothing → 0

-- | Clean up the bindless texture system
destroyBindlessTextureSystem ∷ Device → BindlessTextureSystem → EngineM ε σ ()
destroyBindlessTextureSystem dev system = do
  destroyDescriptorPool dev (btsDescriptorPool system) Nothing
  destroyDescriptorSetLayout dev (btsDescriptorLayout system) Nothing
