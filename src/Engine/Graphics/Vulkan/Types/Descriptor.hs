module Engine.Graphics.Vulkan.Types.Descriptor
  ( DescriptorManager(..)
  ) where

import UPrelude
import qualified Data.Vector as V
import Vulkan.Core10

-- | Manages descriptor resources
data DescriptorManager = DescriptorManager
  { dmPool           ∷ DescriptorPool
  , dmUniformLayout  ∷ DescriptorSetLayout  -- ^ Layout for uniform buffers
  , dmActiveSets     ∷ V.Vector DescriptorSet
  } deriving (Show)
