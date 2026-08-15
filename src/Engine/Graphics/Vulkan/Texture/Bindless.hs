-- | Bindless texture system using UPDATE_AFTER_BIND descriptors.
--
--   The texture array this module allocates is sized by 'bcMaxTextures' in
--   the 'BindlessConfig' passed in — production derives that value from the
--   device's bindless capability, capped at 'maxBindlessTextures'
--   ("Engine.Graphics.Vulkan.Texture.System"), not the much larger
--   update-after-bind sampled-image ceiling
--   "Engine.Graphics.Vulkan.Capability" queries from the device and caps
--   further — that figure is UPDATE_AFTER_BIND's device/technique limit,
--   distinct from what this module actually allocates.
module Engine.Graphics.Vulkan.Texture.Bindless
  ( -- * Types (re-exported from Types module)
    BindlessTextureSystem(..)
  , BindlessConfig(..)
  , defaultBindlessConfig
    -- * Creation/Destruction
  , createBindlessTextureSystem
    -- * Texture Management
  , registerTexture
  , registerPinnedTexture
  , unregisterTexture
  , releaseTextureHandles
  , setTextureFilter
  , getTextureSlotIndex
  , handleSlotTableSize
  , writeHandleSlotEntry
  ) where

import UPrelude
import qualified Data.Vector as V
import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import Data.IORef (readIORef)
import Engine.Core.Monad
import Engine.Core.Resource
import Engine.Core.Log (LogCategory(..))
import Engine.Core.Log.Monad (logWarnM)
import Engine.Core.Capability.Render
  (RenderCapability(..), toRenderCapability)
import Engine.Asset.Handle (TextureHandle(..), toInt)
import Engine.Graphics.Config (textureFilterToVulkan)
import Engine.Graphics.Vulkan.Sampler.Cache
import Engine.Graphics.Vulkan.Texture.Slot
import Engine.Graphics.Vulkan.Texture.Handle
import Engine.Graphics.Vulkan.Texture.Limits
  (maxBindlessTextures, handleSlotTableSize)
import Engine.Graphics.Vulkan.Texture.Rebind
  (FilterRebindPlan(..), SlotRebind(..), planFilterRebind)
import Engine.Graphics.Vulkan.Texture.Release
  (TextureReleasePlan(..), planTextureRelease, dropReleasedHandles
  , freeReleasedSlots, resolveHandleSlot)
import Engine.Graphics.Vulkan.Texture.Requirements
  (bindlessTextureBindingFlags)
import Engine.Graphics.Vulkan.Texture.Undefined (createUndefinedTexture)
import Engine.Graphics.Vulkan.Texture.Types (BindlessTextureSystem(..), BindlessConfig(..))
import Engine.Graphics.Vulkan.Types.Texture (UndefinedTexture(..))
import Engine.Graphics.Vulkan.BufferUtils (createVulkanBuffer)
import Vulkan.Core10
import Vulkan.Core12
import Vulkan.Zero
import Vulkan.CStruct.Extends

-- | Sensible defaults for bindless config. The array size is
--   'maxBindlessTextures' ("Engine.Graphics.Vulkan.Texture.Limits"), the
--   single definition the bindless fragment shaders interpolate too.
defaultBindlessConfig ∷ BindlessConfig
defaultBindlessConfig = BindlessConfig
  { bcMaxTextures    = maxBindlessTextures
  , bcTextureBinding = 0
  , bcDescriptorSet  = 1
  }

-- | Binding index (within 'bcDescriptorSet') of the handle→slot table
--   storage buffer the fragment shader reads. Slot 0 is the texture
--   array; slot 1 is this table (#286).
handleSlotBinding ∷ Word32
handleSlotBinding = 1

-- The handle→slot table size is defined once in
-- "Engine.Graphics.Vulkan.Texture.Limits" and re-exported here; the
-- bindless fragment shaders interpolate that same definition as
-- @HANDLE_TABLE_SIZE@, so the two cannot drift apart.

-- | Create the bindless texture system
createBindlessTextureSystem ∷ PhysicalDevice
                            → Device
                            → CommandPool
                            → Queue
                            → BindlessConfig
                            → EngineM σ BindlessTextureSystem
createBindlessTextureSystem pdev dev cmdPool cmdQueue config = do
  undefinedTex ← createUndefinedTexture pdev dev cmdPool cmdQueue

  descriptorPool ← createBindlessDescriptorPool dev config

  descriptorLayout ← createBindlessDescriptorSetLayout dev config

  descriptorSet ← allocateBindlessDescriptorSet dev descriptorPool descriptorLayout

  let slotAllocator = createSlotAllocator (bcMaxTextures config)

  -- Acquire the shared texture sampler matching the current global
  -- filter. Every UNPINNED slot (and the undefined fallback) points at
  -- this one sampler, and a filter toggle swaps it via
  -- 'setTextureFilter'. A slot registered through
  -- 'registerPinnedTexture' — the world preview, the zoom atlas, and
  -- since #1259 every compiled unit-animation atlas, which must stay
  -- NEAREST for D-6 — is recorded in 'btsPinned' instead and keeps its
  -- own sampler across that toggle.
  env ← ask
  filterMode ← liftIO $ readIORef (rcTextureFilterRef (toRenderCapability env))
  let texKind = textureSamplerKind (textureFilterToVulkan filterMode)
  sharedSampler ← liftIO $ acquireSampler dev (rcSamplerCacheRef (toRenderCapability env)) texKind

  -- MoltenVK requires all argument buffer slots to be initialized
  initializeAllSlots dev descriptorSet config
    (utImageView undefinedTex) sharedSampler

  -- Handle→slot table storage buffer (#286). Vertices carry a stable
  -- texture-handle id; the fragment shader indexes this buffer to find
  -- the live bindless slot, so cached geometry never encodes a volatile
  -- slot. Zero-initialised so an unregistered handle id resolves to slot
  -- 0 (undefined); kept current by 'writeHandleSlotEntry'.
  let tableBytes = fromIntegral (handleSlotTableSize * 4) ∷ DeviceSize
  (tblMem, tblBuf) ← createVulkanBuffer dev pdev tableBytes
        BUFFER_USAGE_STORAGE_BUFFER_BIT
        (MEMORY_PROPERTY_HOST_VISIBLE_BIT ⌄ MEMORY_PROPERTY_HOST_COHERENT_BIT)
  -- Map persistently (host-coherent): the pointer lives in the system so
  -- every handle→slot mutation site can poke it directly.
  tblPtrRaw ← mapMemory dev tblMem 0 tableBytes zero
  let tblPtr = castPtr tblPtrRaw ∷ Ptr Word32
  liftIO $ pokeArray tblPtr (replicate handleSlotTableSize (0 ∷ Word32))
  writeHandleSlotDescriptor dev descriptorSet tblBuf

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
    , btsHandleSlotBuffer = tblBuf
    , btsHandleSlotMemory = tblMem
    , btsHandleSlotPtr    = tblPtr
    }

-- | Initialize all descriptor slots with the undefined texture
-- Required for MoltenVK argument buffer compatibility
initializeAllSlots ∷ Device → DescriptorSet → BindlessConfig 
                   → ImageView → Sampler → EngineM σ ()
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
createBindlessDescriptorPool ∷ Device → BindlessConfig → EngineM σ DescriptorPool
createBindlessDescriptorPool dev config = do
  let poolSize = zero
        { type' = DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER
        , descriptorCount = bcMaxTextures config
        }
      -- One storage buffer for the handle→slot table (#286, binding 1).
      tablePoolSize = zero
        { type' = DESCRIPTOR_TYPE_STORAGE_BUFFER
        , descriptorCount = 1
        }

      poolInfo = zero
        { maxSets = 1
        , poolSizes = V.fromList [poolSize, tablePoolSize]
        , flags = DESCRIPTOR_POOL_CREATE_UPDATE_AFTER_BIND_BIT
        }

  allocResource (\pool → destroyDescriptorPool dev pool Nothing) $
    createDescriptorPool dev poolInfo Nothing

-- | Create descriptor set layout with bindless flags
-- Note: We do NOT use VARIABLE_DESCRIPTOR_COUNT for MoltenVK compatibility
createBindlessDescriptorSetLayout ∷ Device → BindlessConfig → EngineM σ DescriptorSetLayout
createBindlessDescriptorSetLayout dev config = do
  -- Not using VARIABLE_DESCRIPTOR_COUNT due to MoltenVK limitations.
  -- The flags come from 'bindlessTextureBindingFlags' rather than a literal
  -- here: each is valid only under a Vulkan 1.2 feature the logical device
  -- has to enable, and that pairing lives in one place so the two cannot
  -- drift apart again (#1282).
  let -- The handle→slot table (binding 1) is a plain storage buffer: not
      -- update-after-bind (so it needs no extra device feature), written
      -- once at creation before the set is ever bound, then only its
      -- CONTENTS change (via mapped memory) — never the descriptor.
      bindingFlagsInfo = zero
        { bindingFlags = V.fromList [bindlessTextureBindingFlags, zero]
        } ∷ DescriptorSetLayoutBindingFlagsCreateInfo

      textureBinding = zero
        { binding = bcTextureBinding config
        , descriptorType = DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER
        , descriptorCount = bcMaxTextures config
        , stageFlags = SHADER_STAGE_FRAGMENT_BIT
        , immutableSamplers = V.empty
        }

      tableBinding = zero
        { binding = handleSlotBinding
        , descriptorType = DESCRIPTOR_TYPE_STORAGE_BUFFER
        , descriptorCount = 1
        , stageFlags = SHADER_STAGE_FRAGMENT_BIT
        , immutableSamplers = V.empty
        }

      layoutInfo = zero
        { bindings = V.fromList [textureBinding, tableBinding]
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
                              → EngineM σ DescriptorSet
allocateBindlessDescriptorSet dev pool layout = do
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
                    → EngineM σ ()
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

-- | Point the handle→slot table descriptor (binding 1) at its storage
--   buffer. Written once at creation; the buffer object never changes
--   afterwards (only its contents, via 'writeHandleSlotEntry').
writeHandleSlotDescriptor ∷ Device → DescriptorSet → Buffer → EngineM σ ()
writeHandleSlotDescriptor dev descSet buf = do
  let bufInfo = zero
        { buffer = buf
        , offset = 0
        , range  = WHOLE_SIZE
        } ∷ DescriptorBufferInfo

      write = zero
        { dstSet = descSet
        , dstBinding = handleSlotBinding
        , dstArrayElement = 0
        , descriptorCount = 1
        , descriptorType = DESCRIPTOR_TYPE_STORAGE_BUFFER
        , bufferInfo = V.singleton bufInfo
        }

  updateDescriptorSets dev (V.singleton $ SomeStruct write) V.empty

-- | Write one @handleToSlot[handleId] = slot@ entry into the table's
--   persistently-mapped, host-coherent memory (no flush). Call this at
--   EVERY 'btsHandleMap' mutation so the shader-side table stays in sync —
--   register, unregister, and the atlas-share fast paths that insert a
--   handle pointing at an existing slot ('Engine.Asset.Manager',
--   'Engine.Scripting.Lua.Message'). Out-of-range ids are dropped (they
--   stay at the zero-initialised slot 0 = undefined). #286.
writeHandleSlotEntry ∷ BindlessTextureSystem → Int → Word32 → IO ()
writeHandleSlotEntry sys hid slot
  | hid < 0 ∨ hid ≥ handleSlotTableSize = pure ()
  | otherwise = pokeElemOff (btsHandleSlotPtr sys) hid slot

-- | Register a texture in the bindless system, following the GLOBAL
--   filter: the slot is repainted by 'setTextureFilter' on a toggle.
--   Callers pass 'btsTextureSampler'. A texture whose filtering must
--   survive that toggle uses 'registerPinnedTexture' instead.
registerTexture ∷ Device
                → TextureHandle
                → ImageView
                → Sampler
                → BindlessTextureSystem
                → EngineM σ (Maybe BindlessTextureHandle, BindlessTextureSystem)
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
                      → EngineM σ (Maybe BindlessTextureHandle, BindlessTextureSystem)
registerPinnedTexture = registerTextureImpl True

registerTextureImpl ∷ Bool          -- ^ pin this slot's sampler?
                    → Device
                    → TextureHandle
                    → ImageView
                    → Sampler
                    → BindlessTextureSystem
                    → EngineM σ (Maybe BindlessTextureHandle, BindlessTextureSystem)
registerTextureImpl pinned dev texHandle imageView sampler system = do
  case Map.lookup texHandle (btsHandleMap system) of
    Just existingHandle → pure (Just existingHandle, system)
    Nothing → do
      case allocateSlot (btsSlotAllocator system) of
        Nothing → pure (Nothing, system)
        Just (slot, newAllocator) → do
          writeDescriptorSlot dev (btsDescriptorSet system) (btsConfig system)
            (tsIndex slot) imageView sampler
          -- Record the handle→slot mapping for the shader (#286). The
          -- table pointer persists across the immutable system copy, so
          -- writing through 'system' is correct.
          let TextureHandle hid = texHandle
          liftIO $ writeHandleSlotEntry system hid (tsIndex slot)

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
                  → EngineM σ BindlessTextureSystem
unregisterTexture dev texHandle system = do
  case Map.lookup texHandle (btsHandleMap system) of
    Nothing → pure system
    Just bindlessHandle → do
      let slot = bthSlot bindlessHandle
          
      writeDescriptorSlot dev (btsDescriptorSet system) (btsConfig system)
        (tsIndex slot)
        (utImageView $ btsUndefinedTexture system)
        (btsTextureSampler system)
      -- Clear the handle→slot entry so the shader resolves this handle to
      -- slot 0 (undefined) until it is registered again (#286).
      let TextureHandle hid = texHandle
      liftIO $ writeHandleSlotEntry system hid 0

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

-- | Finally release one or more CANONICAL textures: invalidate every
--   stable handle that resolves to a slot they own — the canonical
--   owners and every cached-atlas alias sharing those slots (#1281) —
--   and only then hand the slots back.
--
--   'unregisterTexture' above is deliberately left alone: it is the
--   single-owner path (transient world-preview\/zoom textures, blood
--   decal atlases) where the handle being dropped is the only name for
--   its slot. This is the atlas path, where it is not.
--
--   Order matters. The descriptor repoint and the handle→slot zeroing
--   both happen here, through the system's persistently-mapped table
--   pointer, BEFORE the returned system — the one whose allocator will
--   hand the slot to the next registration — is published. So no
--   shader-visible handle can survive the slot's generation change. The
--   returned plan names every handle the caller must also purge from
--   its own bookkeeping (@apTextureHandles@, the texture size map).
releaseTextureHandles ∷ Device
                      → [TextureHandle]  -- ^ canonical owners being destroyed
                      → BindlessTextureSystem
                      → EngineM σ (TextureReleasePlan, BindlessTextureSystem)
releaseTextureHandles dev owners system = do
  let plan = planTextureRelease owners (btsHandleMap system)
  -- Repaint every released slot with the undefined texture first, so a
  -- descriptor never names an image view that is about to be destroyed.
  forM_ (trpFreedSlots plan) $ \slot →
    writeDescriptorSlot dev (btsDescriptorSet system) (btsConfig system)
      (tsIndex slot)
      (utImageView $ btsUndefinedTexture system)
      (btsTextureSampler system)
  -- Then zero the shader handle→slot entry of EVERY invalidated handle,
  -- canonical and alias alike, so each resolves to slot 0 (#286).
  liftIO $ forM_ (trpInvalidated plan) $ \texHandle →
    writeHandleSlotEntry system (toInt texHandle) 0
  pure ( plan
       , system
           { btsSlotAllocator = freeReleasedSlots plan (btsSlotAllocator system)
           , btsHandleMap     = dropReleasedHandles plan (btsHandleMap system)
           , btsImageViews    = dropReleasedHandles plan (btsImageViews system)
           , btsPinned        = dropReleasedHandles plan (btsPinned system)
           } )

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
                 → EngineM σ BindlessTextureSystem
setTextureFilter dev flt system = do
    env ← ask
    let ref     = rcSamplerCacheRef (toRenderCapability env)
        descSet = btsDescriptorSet system
        config  = btsConfig system
        oldKind = btsTextureKind system
        newKind = textureSamplerKind flt
    newSampler ← liftIO $ acquireSampler dev ref newKind
    -- Repaint all slots first: unallocated → undefined view + new global
    -- sampler. This pre-pass is also what leaves an unrecoverable slot
    -- (below) pointing at the undefined image with the NEW sampler, so it
    -- must stay unconditional and ahead of the per-slot writes.
    initializeAllSlots dev descSet config
      (utImageView $ btsUndefinedTexture system) newSampler
    -- Then each slot with a canonical image view → that view, using the
    -- new global sampler UNLESS its canonical owner is pinned
    -- (preview/zoom), in which case the slot keeps that owner's sampler
    -- so its look is unaffected. Cached-atlas aliases share a canonical
    -- owner's slot and rebind through it silently; only a slot with no
    -- canonical image view at all is reported.
    let plan = planFilterRebind (btsHandleMap system) (btsImageViews system)
                                (btsPinned system) newSampler
    forM_ (frpRebinds plan) $ \rebind →
        writeDescriptorSlot dev descSet config
          (srSlot rebind) (srImageView rebind) (srSampler rebind)
    forM_ (frpUnrecoverable plan) $ \(TextureHandle hid, slotIdx) →
        logWarnM CatTexture $ "setTextureFilter: texture handle "
          <> T.pack (show hid) <> " maps to bindless slot "
          <> T.pack (show slotIdx) <> " but no canonical image view owns "
          <> "that slot; it stays on the undefined texture."
    -- Now no slot references the old sampler — safe to release.
    liftIO $ releaseSampler dev ref oldKind
    pure system { btsTextureSampler = newSampler, btsTextureKind = newKind }

-- | Get the slot index for a texture handle.
--
--   An unregistered handle returns @0@, which is not a sentinel the
--   caller has to test for — slot 0 is the undefined-texture slot
--   ('undefinedSlot'), so the value is directly usable. What the shader
--   then does depends on which
--   binding it feeds: a BASE texture resolving to slot 0 is sampled as
--   the undefined texture like any other slot, while only the FACE-MAP
--   path treats slot 0 specially and substitutes the default face map
--   ("Engine.Graphics.Vulkan.ShaderCode").
getTextureSlotIndex ∷ TextureHandle → BindlessTextureSystem → Word32
getTextureSlotIndex texHandle system =
  resolveHandleSlot texHandle (btsHandleMap system)
