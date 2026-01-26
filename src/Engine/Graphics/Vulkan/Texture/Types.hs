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
import Vulkan.Core10 (DescriptorPool, DescriptorSetLayout, DescriptorSet)

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
  } deriving (Show)

-- | Configuration for the texture system
data TextureSystemConfig = TextureSystemConfig
  { tscMaxTextures    ∷ Word32   -- ^ Max textures (for bindless)
  , tscReservedSlots  ∷ Word32   -- ^ Reserved slots (slot 0 = undefined)
  , tscForceBindless  ∷ Bool     -- ^ Force bindless even if limits are low
  , tscForceLegacy    ∷ Bool     -- ^ Force legacy path (for testing)
  } deriving (Show, Eq)
