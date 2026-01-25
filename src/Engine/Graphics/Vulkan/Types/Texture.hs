module Engine.Graphics.Vulkan.Types.Texture
  ( TextureData(..)
  , TexturePoolState(..)
  , TextureState(..)
  , TextureArrayState(..)
  , VulkanImage(..)
  , UndefinedTexture(..)
  ) where

import UPrelude
import qualified Data.Vector as V
import qualified Data.Map.Strict as Map
import qualified Vulkan.Core10 as Vk
import Engine.Asset.Handle (TextureHandle)

-- | Holds image and its memory
data VulkanImage = VulkanImage
  { viImage  ∷ Vk.Image
  , viMemory ∷ Vk.DeviceMemory
  }

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

-- | Dynamic texture array state with growth tracking
data TextureArrayState = TextureArrayState
  { tasDescriptorPool      ∷ Vk.DescriptorPool
  , tasDescriptorSetLayout ∷ Vk.DescriptorSetLayout
  , tasActiveTextures      ∷ V.Vector TextureData
  , tasDescriptorSet       ∷ Maybe Vk.DescriptorSet
  , tasCurrentCapacity     ∷ Word32
  , tasCurrentCount        ∷ Word32
  , tasHandleToIndex       ∷ Map.Map TextureHandle Int
  , tasUndefinedTexture    ∷ Maybe UndefinedTexture
  } deriving (Show)


-- | Overall texture state as a type alias
type TextureState = (TexturePoolState, V.Vector TextureData)

-- undefined texture
data UndefinedTexture = UndefinedTexture
      { utImage     ∷ VulkanImage
      , utImageView ∷ Vk.ImageView
      , utSampler   ∷ Vk.Sampler }
instance Show UndefinedTexture where
  show ut = "UndefinedTexture { utImage = <VulkanImage>, utImageView = " ++ show (utImageView ut) ++ ", utSampler = " ++ show (utSampler ut) ++ " }"


