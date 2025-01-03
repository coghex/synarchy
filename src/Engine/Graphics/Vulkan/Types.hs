-- Engine/Graphics/Vulkan/Types.hs
{-# LANGUAGE Strict #-}
module Engine.Graphics.Vulkan.Types where
import UPrelude
import qualified Data.Vector as V
import Data.Word (Word32)
import Vulkan.Core10
import Vulkan.Extensions.VK_EXT_debug_utils
import Vulkan.Extensions.VK_EXT_validation_features

data VulkanExtensions = VulkanExtensions
  { veRequired ∷ V.Vector String  -- ^ Required extensions
  , veOptional ∷ V.Vector String  -- ^ Optional extensions
  }

data VulkanLayers = VulkanLayers
  { vlRequired ∷ V.Vector String  -- ^ Required layers
  , vlOptional ∷ V.Vector String  -- ^ Optional layers
  }

data SyncObjects = SyncObjects
  { imageAvailableSemaphores ∷ V.Vector Semaphore
  , renderFinishedSemaphores ∷ V.Vector Semaphore
  , inFlightFences           ∷ V.Vector Fence
  } deriving (Show)

data VulkanDescriptorInfo = VulkanDescriptorInfo
  { vdiLayout  ∷ DescriptorSetLayout
  , vdiPool    ∷ DescriptorPool
  , vdiSets    ∷ V.Vector DescriptorSet
  }

data TextureInfo = TextureInfo
  { tiImage    ∷ Image
  , tiView     ∷ ImageView
  , tiSampler  ∷ Sampler
  , tiMemory   ∷ DeviceMemory
  , tiLayout   ∷ ImageLayout
  }

-- | Configuration for descriptor manager
data DescriptorManagerConfig = DescriptorManagerConfig
  { dmcMaxSets        ∷ Word32        -- ^ Maximum number of descriptor sets
  , dmcUniformCount   ∷ Word32        -- ^ Number of uniform buffer descriptors
  , dmcSamplerCount   ∷ Word32        -- ^ Number of combined image sampler descriptors
  } deriving (Show, Eq)

-- | Manages descriptor resources
data DescriptorManager = DescriptorManager
  { dmPool           ∷ DescriptorPool
  , dmUniformLayout  ∷ DescriptorSetLayout  -- ^ Layout for uniform buffers
  , dmSamplerLayout  ∷ DescriptorSetLayout  -- ^ Layout for combined image samplers
  , dmActiveSets     ∷ V.Vector DescriptorSet
  } deriving (Show)

