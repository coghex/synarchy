module Engine.Graphics.Vulkan.Types.Texture
  ( VulkanImage(..)
  , UndefinedTexture(..)
  ) where

import UPrelude
import qualified Vulkan.Core10 as Vk

-- | Holds image and its memory
data VulkanImage = VulkanImage
  { viImage  ∷ Vk.Image
  , viMemory ∷ Vk.DeviceMemory
  }

-- | The fallback magenta texture every unallocated bindless slot points
--   at. It owns no sampler — slots are written with the bindless
--   system's shared 'btsTextureSampler'.
data UndefinedTexture = UndefinedTexture
      { utImage     ∷ VulkanImage
      , utImageView ∷ Vk.ImageView }
instance Show UndefinedTexture where
  show ut = "UndefinedTexture { utImage = <VulkanImage>, utImageView = " ⧺ show (utImageView ut) ⧺ " }"
