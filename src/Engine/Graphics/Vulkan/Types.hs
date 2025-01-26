-- Engine/Graphics/Vulkan/Types.hs
{-# LANGUAGE Strict #-}
module Engine.Graphics.Vulkan.Types where
import UPrelude
import qualified Data.Vector as V
import Linear (V4(..), M44, identity)
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

-- | Configuration for descriptor manager
data DescriptorManagerConfig = DescriptorManagerConfig
  { dmcMaxSets        ∷ Word32        -- ^ Maximum number of descriptor sets
  , dmcUniformCount   ∷ Word32        -- ^ Number of uniform buffer descriptors
  , dmcSamplerCount   ∷ Word32        -- ^ Number of combined image sampler descriptors
  } deriving (Show, Eq)

-- | Resources specific to a single frame in flight
data FrameResources = FrameResources
    { frCommandPool     ∷ CommandPool
    , frCommandBuffer   ∷ V.Vector CommandBuffer
    , frImageAvailable  ∷ Semaphore
    , frRenderFinished  ∷ Semaphore
    , frInFlight        ∷ Fence
    } deriving (Show)

maxTimeout ∷ Word64
maxTimeout = maxBound

-- | blocks on cleanup
data CleanupStatus = NotStarted | InProgress | Completed
  deriving (Show, Eq)

-- | Uniform buffer object matching shader layout
data UniformBufferObject = UBO
    { uboModel ∷ M44 Float  -- model matrix
    , uboView  ∷ M44 Float  -- view matrix
    , uboProj  ∷ M44 Float  -- projection matrix
    } deriving (Show)

instance Storable UniformBufferObject where
    sizeOf _ = 3 * sizeOf (undefined ∷ M44 Float)
    alignment _ = 16  -- Vulkan requires 16-byte alignment for uniform buffers
    peek ptr = UBO
        <$> peek (castPtr ptr)
        <*> peek (castPtr $ ptr `plusPtr` sizeOf (undefined ∷ M44 Float))
        <*> peek (castPtr $ ptr `plusPtr` (2 * sizeOf (undefined ∷ M44 Float)))
    poke ptr (UBO model view proj) = do
        poke (castPtr ptr) model
        poke (castPtr $ ptr `plusPtr` sizeOf model) view
        poke (castPtr $ ptr `plusPtr` (2 * sizeOf model)) proj
