module Engine.Graphics.Vulkan.Types.Descriptor
  ( DescriptorManager(..)
  ) where

import UPrelude
import qualified Data.Vector as V
import qualified Data.Text as T
import qualified Data.Map as Map
import Vulkan.Core10
import Engine.Asset.Base (AssetId)

-- | Manages descriptor resources
data DescriptorManager = DescriptorManager
  { dmPool           ∷ DescriptorPool
  , dmUniformLayout  ∷ DescriptorSetLayout  -- ^ Layout for uniform buffers
  , dmSamplerLayout  ∷ DescriptorSetLayout  -- ^ Layout for combined image samplers
  , dmActiveSets     ∷ V.Vector DescriptorSet
  , dmTextureLayouts ∷ Map.Map AssetId DescriptorSetLayout
  , dmShaderLayouts  ∷ Map.Map AssetId DescriptorSetLayout
  } deriving (Show)
