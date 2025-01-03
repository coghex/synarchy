{-# LANGUAGE Strict #-}
module Engine.Graphics.Vulkan.Descriptor
  ( -- * Creation and Destruction
    createVulkanDescriptorManager
  , destroyVulkanDescriptorManager
    -- * Pool Management
  , createVulkanDescriptorPool
    -- * Layout Management
  , createVulkanDescriptorSetLayout
  , createVulkanDescriptorSetLayouts
    -- * Set Management
  , allocateVulkanDescriptorSets
  , updateVulkanDescriptorSets
  ) where

import UPrelude
import Control.Monad.IO.Class (MonadIO(..))
import Control.Monad (when)
import qualified Data.Vector as V
import Data.Maybe (catMaybes)
import Data.Word (Word32)
import Engine.Core.Monad
import Engine.Core.Resource
import Engine.Core.Error.Exception
import Engine.Graphics.Types
import Engine.Graphics.Vulkan.Types
import Vulkan.Core10
import Vulkan.Zero
import Vulkan.CStruct.Extends

-- | Default configuration
defaultDescriptorConfig ∷ DescriptorManagerConfig
defaultDescriptorConfig = DescriptorManagerConfig
  { dmcMaxSets      = 100   -- Total sets
  , dmcUniformCount = 100   -- Uniform buffer descriptors
  , dmcSamplerCount = 100   -- Sampler descriptors
  }

createVulkanDescriptorPool ∷ Device → DescriptorManagerConfig → EngineM ε σ DescriptorPool
createVulkanDescriptorPool device config = do
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

createVulkanDescriptorSetLayout ∷ Device → EngineM ε σ DescriptorSetLayout
createVulkanDescriptorSetLayout device = do
  -- Create a binding for uniform buffer (transformation matrices, etc)
  let uniformBinding = zero 
        { binding = 0
        , descriptorType = DESCRIPTOR_TYPE_UNIFORM_BUFFER
        , descriptorCount = 1
        , stageFlags = SHADER_STAGE_VERTEX_BIT
        , immutableSamplers = V.empty
        }
      
      -- Create a binding for combined image sampler (textures)
      samplerBinding = zero
        { binding = 1
        , descriptorType = DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER
        , descriptorCount = 1
        , stageFlags = SHADER_STAGE_FRAGMENT_BIT
        , immutableSamplers = V.empty
        }
      
      -- Create the layout info with both bindings
      layoutInfo = zero
        { bindings = V.fromList [uniformBinding, samplerBinding]
        }
  
  -- Create the layout with proper resource cleanup
  allocResource (\layout → destroyDescriptorSetLayout device layout Nothing) $
    createDescriptorSetLayout device layoutInfo Nothing

createVulkanDescriptorSetLayouts ∷ Device → EngineM ε σ (DescriptorSetLayout, DescriptorSetLayout)
createVulkanDescriptorSetLayouts device = do
  -- Create uniform buffer layout
  uniformLayout ← createUniformLayout
  -- Create sampler layout
  samplerLayout ← createSamplerLayout
  
  pure (uniformLayout, samplerLayout)
  where
    createUniformLayout = do
      let binding = zero
            { binding = 0
            , descriptorType = DESCRIPTOR_TYPE_UNIFORM_BUFFER
            , descriptorCount = 1
            , stageFlags = SHADER_STAGE_VERTEX_BIT
            , immutableSamplers = V.empty
            }
          layoutInfo = zero
            { bindings = V.singleton binding }
      
      allocResource (\layout → destroyDescriptorSetLayout device layout Nothing) $
        createDescriptorSetLayout device layoutInfo Nothing
    
    createSamplerLayout = do
      let binding = zero
            { binding = 0
            , descriptorType = DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER
            , descriptorCount = 1
            , stageFlags = SHADER_STAGE_FRAGMENT_BIT
            , immutableSamplers = V.empty
            }
          layoutInfo = zero
            { bindings = V.singleton binding }
      
      allocResource (\layout → destroyDescriptorSetLayout device layout Nothing) $
        createDescriptorSetLayout device layoutInfo Nothing

createVulkanDescriptorManager ∷ Device → DescriptorManagerConfig → EngineM ε σ DescriptorManager
createVulkanDescriptorManager device config = do
  -- Create pool
  pool ← createVulkanDescriptorPool device config
  
  -- Create layouts
  (uniformLayout, samplerLayout) ← createVulkanDescriptorSetLayouts device
  
  pure $ DescriptorManager
    { dmPool = pool
    , dmUniformLayout = uniformLayout
    , dmSamplerLayout = samplerLayout
    , dmActiveSets = V.empty
    }

allocateVulkanDescriptorSets ∷ Device → DescriptorManager → Word32 → EngineM ε σ (V.Vector DescriptorSet)
allocateVulkanDescriptorSets device manager count = do
  let allocInfo = zero
        { descriptorPool = dmPool manager
        , setLayouts = V.replicate (fromIntegral count) (dmUniformLayout manager)
        }
  
  allocateDescriptorSets device allocInfo

updateVulkanDescriptorSets ∷ Device 
                          → DescriptorSet
                          → Maybe (Buffer, DeviceSize)        -- ^ Optional uniform buffer info
                          → Maybe (ImageView, Sampler)        -- ^ Optional image/sampler info
                          → EngineM ε σ ()
updateVulkanDescriptorSets device dset mbBuffer mbImage = do
  let writes = V.fromList $ catMaybes
        [ makeBufferWrite <$> mbBuffer
        , makeImageWrite <$> mbImage
        ]
  
  when (not $ V.null writes) $
    updateDescriptorSets device writes V.empty
  where
    makeBufferWrite (buffer, range) = 
      SomeStruct $ zero
        { dstSet = dset
        , dstBinding = 0
        , dstArrayElement = 0
        , descriptorCount = 1
        , descriptorType = DESCRIPTOR_TYPE_UNIFORM_BUFFER
        , bufferInfo = V.singleton $ zero
            { buffer = buffer
            , offset = 0
            , range = range
            }
        }
    
    makeImageWrite (imageView, sampler) =
      SomeStruct $ zero
        { dstSet = dset
        , dstBinding = 1
        , dstArrayElement = 0
        , descriptorCount = 1
        , descriptorType = DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER
        , imageInfo = V.singleton $ zero
            { imageView = imageView
            , imageLayout = IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL
            , sampler = sampler
            }
        }

destroyVulkanDescriptorManager ∷ Device → DescriptorManager → EngineM ε σ ()
destroyVulkanDescriptorManager device manager = do
  -- Destroy layouts first
  destroyDescriptorSetLayout device (dmUniformLayout manager) Nothing
  destroyDescriptorSetLayout device (dmSamplerLayout manager) Nothing
  -- Then destroy pool (this implicitly frees all allocated sets)
  destroyDescriptorPool device (dmPool manager) Nothing
