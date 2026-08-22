{-# LANGUAGE Strict #-}
module Engine.Graphics.Vulkan.Descriptor
  ( createVulkanDescriptorManager
  , allocateVulkanDescriptorSets
  ) where

import UPrelude
import qualified Data.Vector as V
import Engine.Core.Monad
import Engine.Core.Resource
import Engine.Core.Log (LogCategory(..))
import Engine.Core.Log.Monad (logDebugSM)
import Engine.Graphics.Vulkan.Types
import Engine.Graphics.Vulkan.Types.Descriptor
import Vulkan.Core10
import Vulkan.Zero

createVulkanDescriptorPool ∷ Device → DescriptorManagerConfig → EngineM σ DescriptorPool
createVulkanDescriptorPool device config = do
  logDebugSM CatDescriptor "Creating descriptor pool"
    [("max_sets", tshow $ dmcMaxSets config)
    ,("uniform_count", tshow $ dmcUniformCount config)
    ,("sampler_count", tshow $ dmcSamplerCount config)]
  
  let poolSizes = V.fromList
        [ zero
          { type' = DESCRIPTOR_TYPE_UNIFORM_BUFFER
          , descriptorCount = dmcUniformCount config
          }
        , zero
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

createUniformDescriptorSetLayout ∷ Device → EngineM σ DescriptorSetLayout
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

createVulkanDescriptorManager ∷ Device → DescriptorManagerConfig → EngineM σ DescriptorManager
createVulkanDescriptorManager device config = do
  pool ← createVulkanDescriptorPool device config
  uniformLayout ← createUniformDescriptorSetLayout device
  
  pure $ DescriptorManager
    { dmPool = pool
    , dmUniformLayout = uniformLayout
    , dmActiveSets = V.empty
    }

allocateVulkanDescriptorSets ∷ Device → DescriptorManager → Word32 → EngineM σ (V.Vector DescriptorSet)
allocateVulkanDescriptorSets device manager count = do
  logDebugSM CatDescriptor "Allocating descriptor sets"
    [("count", tshow count)
    ,("layout", "uniform")]
  
  let allocInfo = zero
        { descriptorPool = dmPool manager
        , setLayouts = V.replicate (fromIntegral count) (dmUniformLayout manager)
        }
  
  allocateDescriptorSets device allocInfo

