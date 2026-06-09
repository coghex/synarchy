-- | Types for the bindless texture system
-- Separated to avoid import cycles with Engine.Core.State
module Engine.Graphics.Vulkan.Texture.Types
  ( TextureSystemConfig(..)
  , BindlessTextureSystem(..)
  , BindlessConfig(..)
  ) where

import UPrelude
import qualified Data.Map.Strict as Map
import Engine.Asset.Handle (TextureHandle)
import Engine.Graphics.Vulkan.Texture.Slot (TextureSlotAllocator)
import Engine.Graphics.Vulkan.Texture.Handle (BindlessTextureHandle)
import Engine.Graphics.Vulkan.Types.Texture (UndefinedTexture)
import Engine.Graphics.Vulkan.Sampler.Types (SamplerKind)
import Vulkan.Core10 (DescriptorPool, DescriptorSetLayout, DescriptorSet, ImageView, Sampler)

-- | Configuration for the bindless texture system
data BindlessConfig = BindlessConfig
  { bcMaxTextures     ∷ !Word32  -- ^ Maximum number of texture slots
  , bcTextureBinding  ∷ !Word32  -- ^ Binding index for texture array in shader
  , bcDescriptorSet   ∷ !Word32  -- ^ Which descriptor set to use
  } deriving (Show, Eq)

-- | The complete bindless texture system state
data BindlessTextureSystem = BindlessTextureSystem
  { btsConfig           ∷ !BindlessConfig
  , btsDescriptorPool   ∷ !DescriptorPool
  , btsDescriptorLayout ∷ !DescriptorSetLayout
  , btsDescriptorSet    ∷ !DescriptorSet
  , btsSlotAllocator    ∷ !TextureSlotAllocator
  , btsUndefinedTexture ∷ !UndefinedTexture
  , btsHandleMap        ∷ !(Map.Map TextureHandle BindlessTextureHandle)
  , btsImageViews       ∷ !(Map.Map TextureHandle ImageView)
  , btsTextureSampler   ∷ !Sampler
    -- ^ The single shared sampler every atlas slot points at. Acquired
    --   from the refcounted cache at creation; swapped on a filter
    --   toggle. Atlases never mint their own sampler.
  , btsTextureKind      ∷ !SamplerKind
    -- ^ The kind 'btsTextureSampler' was acquired as — needed to
    --   release the right cache entry on a filter toggle.
  , btsPinned           ∷ !(Map.Map TextureHandle Sampler)
    -- ^ Handles pinned to a SPECIFIC sampler that must survive a global
    --   filter toggle (world preview → NEAREST, zoom atlas → LINEAR).
    --   A filter toggle repaints every other slot to the new global
    --   sampler but rewrites these to their pinned sampler instead, so
    --   they keep their intended look. Registered via
    --   'registerPinnedTexture'; the value is the sampler to keep using
    --   (kept alive by the texture's own cache reference while it is
    --   registered).
  } deriving (Show)

-- | Configuration for the texture system
data TextureSystemConfig = TextureSystemConfig
  { tscMaxTextures    ∷ Word32   -- ^ Max textures (for bindless)
  , tscReservedSlots  ∷ Word32   -- ^ Reserved slots (slot 0 = undefined)
  , tscForceBindless  ∷ Bool     -- ^ Force bindless even if limits are low
  , tscForceLegacy    ∷ Bool     -- ^ Force legacy path (for testing)
  } deriving (Show, Eq)
