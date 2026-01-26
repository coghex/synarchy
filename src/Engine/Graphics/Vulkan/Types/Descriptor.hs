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
  , dmActiveSets     ∷ V.Vector DescriptorSet
  } deriving (Show)
