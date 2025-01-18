module Engine.Graphics.Vulkan.Types.Texture
  ( TextureData(..)
  , TexturePoolState(..)
  , TextureState(..)
  ) where

import UPrelude
import qualified Data.Vector as V
import qualified Vulkan.Core10 as Vk

-- | Data for a single texture
data TextureData = TextureData 
  { tdImageView     ∷ Vk.ImageView
  , tdSampler       ∷ Vk.Sampler
  , tdMipLevels     ∷ Word32
  , tdDescriptorSet ∷ Vk.DescriptorSet
  } deriving (Show)

-- | State for descriptor pools and layouts
data TexturePoolState = TexturePoolState
  { tpsDescPool ∷ Vk.DescriptorPool
  , tpsLayout   ∷ Vk.DescriptorSetLayout
  } deriving (Show)

-- | Overall texture state as a type alias
type TextureState = (TexturePoolState, V.Vector TextureData)
