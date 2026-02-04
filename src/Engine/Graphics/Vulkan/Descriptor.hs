{-# LANGUAGE Strict #-}
module Engine.Graphics.Vulkan.Descriptor
  ( -- * Creation and Destruction
    createVulkanDescriptorManager
  , destroyVulkanDescriptorManager
    -- * Pool Management
  , createVulkanDescriptorPool
    -- * Set Management
  , allocateVulkanDescriptorSets
  ) where

import UPrelude
import Control.Monad.IO.Class (MonadIO(..))
import qualified Data.Vector as V
import qualified Data.Text as T
import Engine.Core.Monad
import Engine.Core.Resource
import Engine.Core.Error.Exception
import Engine.Core.Log (LogCategory(..))
import Engine.Core.Log.Monad (logDebugM, logInfoM, logDebugSM, logInfoSM)
import Engine.Graphics.Types
import Engine.Graphics.Vulkan.Types
import Engine.Graphics.Vulkan.Types.Descriptor
import Vulkan.Core10
import Vulkan.Zero
import Vulkan.CStruct.Extends

-- | Default configuration
defaultDescriptorConfig ∷ DescriptorManagerConfig
defaultDescriptorConfig = DescriptorManagerConfig
  { dmcMaxSets      = 10   -- Total sets
  , dmcUniformCount = 10   -- Uniform buffer descriptors
  , dmcSamplerCount = 20   -- 2 textures * 10 frames
  }

createVulkanDescriptorPool ∷ Device → DescriptorManagerConfig → EngineM ε σ DescriptorPool
createVulkanDescriptorPool device config = do
  logDebugSM CatDescriptor "Creating descriptor pool"
    [("max_sets", T.pack $ show $ dmcMaxSets config)
    ,("uniform_count", T.pack $ show $ dmcUniformCount config)
    ,("sampler_count", T.pack $ show $ dmcSamplerCount config)]
  
  let poolSizes = V.fromList
        [ zero  -- Uniform buffers
          { type' = DESCRIPTOR_TYPE_UNIFORM_BUFFER
          , descriptorCount = dmcUniformCount config
          }
        , zero  -- Combined image samplers
          { type' = DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER
          , descriptorCount = dmcSamplerCount config
          }
        ]
      
      createInfo = zero
        { maxSets = dmcMaxSets config
        , poolSizes = poolSizes
        , flags = DESCRIPTOR_POOL_CREATE_FREE_DESCRIPTOR_SET_BIT
        }
  
  allocResource (\pool → destroyDescriptorPool device pool Nothing) $
    createDescriptorPool device createInfo Nothing

createUniformDescriptorSetLayout ∷ Device → EngineM ε σ DescriptorSetLayout
createUniformDescriptorSetLayout device = do
  let bindings = zero
        { binding = 0
        , descriptorType = DESCRIPTOR_TYPE_UNIFORM_BUFFER
        , descriptorCount = 1
        , stageFlags = SHADER_STAGE_VERTEX_BIT
        , immutableSamplers = V.empty
        }
      createInfo = zero
        { bindings = V.singleton bindings }
  
  allocResource (\layout → destroyDescriptorSetLayout device layout Nothing) $
    createDescriptorSetLayout device createInfo Nothing

createVulkanDescriptorManager ∷ Device → DescriptorManagerConfig → EngineM ε σ DescriptorManager
createVulkanDescriptorManager device config = do
  -- Create pool
  pool ← createVulkanDescriptorPool device config
  uniformLayout ← createUniformDescriptorSetLayout device
  
  pure $ DescriptorManager
    { dmPool = pool
    , dmUniformLayout = uniformLayout
    , dmActiveSets = V.empty
    }

allocateVulkanDescriptorSets ∷ Device → DescriptorManager → Word32 → EngineM ε σ (V.Vector DescriptorSet)
allocateVulkanDescriptorSets device manager count = do
  logDebugSM CatDescriptor "Allocating descriptor sets"
    [("count", T.pack $ show count)
    ,("layout", "uniform")]
  
  let allocInfo = zero
        { descriptorPool = dmPool manager
        , setLayouts = V.replicate (fromIntegral count) (dmUniformLayout manager)
        }
  
  allocateDescriptorSets device allocInfo

destroyVulkanDescriptorManager ∷ Device → DescriptorManager → EngineM ε σ ()
destroyVulkanDescriptorManager device manager = do
  -- Destroy layouts first
  destroyDescriptorSetLayout device (dmUniformLayout manager) Nothing
  -- Then destroy pool (this implicitly frees all allocated sets)
  destroyDescriptorPool device (dmPool manager) Nothing
