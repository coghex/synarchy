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
  , freeVulkanDescriptorSets
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

createVulkanDescriptorSetLayout ∷ Device
  → EngineM ε σ (DescriptorSetLayout, DescriptorSetLayout)
createVulkanDescriptorSetLayout device = do
  -- Create a binding for uniform buffer (transformation matrices, etc)
  let uniformBinding = zero 
        { binding = 0
        , descriptorType = DESCRIPTOR_TYPE_UNIFORM_BUFFER
        , descriptorCount = 1
        , stageFlags = SHADER_STAGE_VERTEX_BIT
        , immutableSamplers = V.empty
        }
      
      -- Create a binding for texture atlas array
      samplerBinding = zero
        { binding = 0 -- its in set = 1
        , descriptorType = DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER
        , descriptorCount = 8
        , stageFlags = SHADER_STAGE_FRAGMENT_BIT
        , immutableSamplers = V.empty
        }
      
      -- Create the layout info with both bindings
      layoutInfo = zero
        { bindings = V.singleton uniformBinding
        }
      textureLayoutInfo = zero
        { bindings = V.singleton samplerBinding
        }
  
  -- Create both layouts
  uniformLayout ← allocResource (\layout → destroyDescriptorSetLayout device layout Nothing) $
    createDescriptorSetLayout device layoutInfo Nothing
    
  textureLayout ← allocResource (\layout → destroyDescriptorSetLayout device layout Nothing) $
    createDescriptorSetLayout device textureLayoutInfo Nothing
    
  pure (uniformLayout, textureLayout)

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
            , descriptorCount = 8
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
  → Maybe (Buffer, DeviceSize)   -- ^ Optional uniform buffer info
  → Maybe [(ImageView, Sampler)] -- ^ Optional image/sampler info
  → EngineM ε σ ()
updateVulkanDescriptorSets device dset mbBuffer mbImages = do
  let writes = V.fromList $ catMaybes
        [ makeBufferWrite <$> mbBuffer
        , makeImageWrite <$> mbImages
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
    
    makeImageWrite images =
      SomeStruct $ zero
        { dstSet = dset
        , dstBinding = 1
        , dstArrayElement = 0
        , descriptorCount = fromIntegral $ length images
        , descriptorType = DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER
        , imageInfo = V.fromList $ map makeImageInfo images
        }
      where makeImageInfo (view, sampler) = zero
              { imageView = view
              , imageLayout = IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL
              , sampler = sampler
              }

destroyVulkanDescriptorManager ∷ Device → DescriptorManager → EngineM ε σ ()
destroyVulkanDescriptorManager device manager = do
  -- Destroy layouts first
  destroyDescriptorSetLayout device (dmUniformLayout manager) Nothing
  destroyDescriptorSetLayout device (dmSamplerLayout manager) Nothing
  -- Then destroy pool (this implicitly frees all allocated sets)
  destroyDescriptorPool device (dmPool manager) Nothing

freeVulkanDescriptorSets ∷ Device → DescriptorPool → V.Vector DescriptorSet → EngineM ε σ ()
freeVulkanDescriptorSets device pool sets = 
    when (not $ V.null sets) $
        liftIO $ freeDescriptorSets device pool sets
