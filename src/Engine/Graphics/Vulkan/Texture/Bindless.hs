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
  , registerPinnedTexture
  , unregisterTexture
  , setTextureFilter
  , getTextureSlotIndex
  ) where

import UPrelude
import qualified Data.Vector as V
import qualified Data.Map.Strict as Map
import Data.IORef (readIORef)
import Engine.Core.Monad
import Engine.Core.Resource
import Engine.Core.Error.Exception
import Engine.Core.State (EngineEnv(..))
import Engine.Asset.Handle (TextureHandle(..))
import Engine.Graphics.Config (textureFilterToVulkan)
import Engine.Graphics.Vulkan.Sampler.Cache
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
  undefinedTex ← createUndefinedTexture pdev dev cmdPool cmdQueue

  descriptorPool ← createBindlessDescriptorPool dev config

  descriptorLayout ← createBindlessDescriptorSetLayout dev config

  descriptorSet ← allocateBindlessDescriptorSet dev descriptorPool descriptorLayout config

  let slotAllocator = createSlotAllocator (bcMaxTextures config)

  -- Acquire the single shared texture sampler matching the current
  -- global filter. Every atlas slot (and the undefined fallback) points
  -- at this one sampler; a filter toggle swaps it via 'setTextureFilter'.
  env ← ask
  filterMode ← liftIO $ readIORef (textureFilterRef env)
  let texKind = textureSamplerKind (textureFilterToVulkan filterMode)
  sharedSampler ← liftIO $ acquireSampler dev (samplerCacheRef env) texKind

  -- MoltenVK requires all argument buffer slots to be initialized
  initializeAllSlots dev descriptorSet config
    (utImageView undefinedTex) sharedSampler
  pure $ BindlessTextureSystem
    { btsConfig           = config
    , btsDescriptorPool   = descriptorPool
    , btsDescriptorLayout = descriptorLayout
    , btsDescriptorSet    = descriptorSet
    , btsSlotAllocator    = slotAllocator
    , btsUndefinedTexture = undefinedTex
    , btsHandleMap        = Map.empty
    , btsImageViews       = Map.empty
    , btsTextureSampler   = sharedSampler
    , btsTextureKind      = texKind
    , btsPinned           = Map.empty
    }

-- | Initialize all descriptor slots with the undefined texture
-- Required for MoltenVK argument buffer compatibility
initializeAllSlots ∷ Device → DescriptorSet → BindlessConfig 
                   → ImageView → Sampler → EngineM ε σ ()
initializeAllSlots dev descSet config imageView sampler = do
  let maxSlots = bcMaxTextures config
      imageInfo = zero
        { imageLayout = IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL
        , imageView = imageView
        , sampler = sampler
        }
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
  -- Not using VARIABLE_DESCRIPTOR_COUNT due to MoltenVK limitations
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

-- | Register a texture in the bindless system. The atlas/global path:
--   the slot follows the global filter and is repainted by
--   'setTextureFilter' on a toggle. Callers pass 'btsTextureSampler'.
registerTexture ∷ Device
                → TextureHandle
                → ImageView
                → Sampler
                → BindlessTextureSystem
                → EngineM ε σ (Maybe BindlessTextureHandle, BindlessTextureSystem)
registerTexture = registerTextureImpl False

-- | Register a texture pinned to a SPECIFIC sampler that must survive a
--   global filter toggle (world preview → NEAREST, zoom atlas → LINEAR).
--   'setTextureFilter' rewrites this slot to its pinned sampler instead
--   of the new global one, so it keeps its intended look.
registerPinnedTexture ∷ Device
                      → TextureHandle
                      → ImageView
                      → Sampler
                      → BindlessTextureSystem
                      → EngineM ε σ (Maybe BindlessTextureHandle, BindlessTextureSystem)
registerPinnedTexture = registerTextureImpl True

registerTextureImpl ∷ Bool          -- ^ pin this slot's sampler?
                    → Device
                    → TextureHandle
                    → ImageView
                    → Sampler
                    → BindlessTextureSystem
                    → EngineM ε σ (Maybe BindlessTextureHandle, BindlessTextureSystem)
registerTextureImpl pinned dev texHandle imageView sampler system = do
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
              newImageViews = Map.insert texHandle imageView (btsImageViews system)
              newPinned
                | pinned    = Map.insert texHandle sampler (btsPinned system)
                | otherwise = btsPinned system
              newSystem = system
                { btsSlotAllocator = newAllocator
                , btsHandleMap = newHandleMap
                , btsImageViews = newImageViews
                , btsPinned = newPinned
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
        (btsTextureSampler system)

      let newAllocator = freeSlot slot (btsSlotAllocator system)
          newHandleMap = Map.delete texHandle (btsHandleMap system)
          newImageViews = Map.delete texHandle (btsImageViews system)
          newPinned = Map.delete texHandle (btsPinned system)

      pure $ system
        { btsSlotAllocator = newAllocator
        , btsHandleMap = newHandleMap
        , btsImageViews = newImageViews
        , btsPinned = newPinned
        }

-- | Switch the shared texture sampler to match a new global filter.
--   Acquires the new kind from the cache, repaints EVERY slot (the
--   unallocated ones with the undefined view, the allocated ones with
--   their real view) to the new sampler, then releases the old kind —
--   so by the time the old 'VkSampler' can be destroyed no slot still
--   references it. Safe to call live thanks to UPDATE_AFTER_BIND; the
--   refcounted cache means the new sampler is shared, not duplicated.
setTextureFilter ∷ Device
                 → Filter                  -- ^ The new global filter
                 → BindlessTextureSystem
                 → EngineM ε σ BindlessTextureSystem
setTextureFilter dev flt system = do
    env ← ask
    let ref     = samplerCacheRef env
        descSet = btsDescriptorSet system
        config  = btsConfig system
        oldKind = btsTextureKind system
        newKind = textureSamplerKind flt
    newSampler ← liftIO $ acquireSampler dev ref newKind
    -- Repaint all slots first: unallocated → undefined view + new global
    -- sampler. Then each allocated slot → its real view, using the new
    -- global sampler UNLESS the handle is pinned (preview/zoom), in which
    -- case it keeps its own sampler so its look is unaffected.
    initializeAllSlots dev descSet config
      (utImageView $ btsUndefinedTexture system) newSampler
    forM_ (Map.toList $ btsHandleMap system) $ \(texHandle, bindlessHandle) → do
        let slotIdx = tsIndex (bthSlot bindlessHandle)
            slotSampler = Map.findWithDefault newSampler texHandle (btsPinned system)
        case Map.lookup texHandle (btsImageViews system) of
            Just imageView →
                writeDescriptorSlot dev descSet config slotIdx imageView slotSampler
            Nothing → pure ()  -- shouldn't happen
    -- Now no slot references the old sampler — safe to release.
    liftIO $ releaseSampler dev ref oldKind
    pure system { btsTextureSampler = newSampler, btsTextureKind = newKind }

-- | Get the slot index for a texture handle
getTextureSlotIndex ∷ TextureHandle → BindlessTextureSystem → Word32
getTextureSlotIndex texHandle system =
  case Map.lookup texHandle (btsHandleMap system) of
    Just bindlessHandle → fromBindlessHandle bindlessHandle
    Nothing → 0

-- | Clean up the bindless texture system. The shared sampler is
--   released back to the cache (destroyed only if this held the last
--   reference); descriptor pool + layout are destroyed directly.
destroyBindlessTextureSystem ∷ Device → BindlessTextureSystem → EngineM ε σ ()
destroyBindlessTextureSystem dev system = do
  env ← ask
  liftIO $ releaseSampler dev (samplerCacheRef env) (btsTextureKind system)
  destroyDescriptorPool dev (btsDescriptorPool system) Nothing
  destroyDescriptorSetLayout dev (btsDescriptorLayout system) Nothing
