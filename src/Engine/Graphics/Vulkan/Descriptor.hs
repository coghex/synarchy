{-# LANGUAGE OverloadedLists #-}
module Engine.Graphics.Vulkan.Descriptor
  ( createVulkanDescriptorSetLayout
  , destroyVulkanDescriptorSetLayout
  ) where

import UPrelude
import qualified Data.Vector as V
import Engine.Core.Monad
import Engine.Core.Resource
import Engine.Graphics.Vulkan.Types
import Vulkan.Core10
import Vulkan.Zero
import Vulkan.CStruct.Extends

-- | Creates a descriptor set layout for basic 2D sprite rendering
createVulkanDescriptorSetLayout ∷ Device → EngineM ε σ DescriptorSetLayout
createVulkanDescriptorSetLayout device = do
  -- For a basic 2D sprite engine, we'll need:
  -- 1. A uniform buffer for transformation matrices and other global data
  -- 2. A combined image sampler for the texture atlas
  let uboBinding = zero 
        { binding = 0
        , descriptorType = DESCRIPTOR_TYPE_UNIFORM_BUFFER
        , descriptorCount = 1
        , stageFlags = SHADER_STAGE_VERTEX_BIT
        , immutableSamplers = V.empty
        }
      
      samplerBinding = zero
        { binding = 1
        , descriptorType = DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER
        , descriptorCount = 1
        , stageFlags = SHADER_STAGE_FRAGMENT_BIT
        , immutableSamplers = V.empty
        }
      
      layoutInfo = zero
        { bindings = V.fromList [uboBinding, samplerBinding]
        }

  -- Create the descriptor set layout with proper resource cleanup
  allocResource (\dsl → destroyDescriptorSetLayout device dsl Nothing) $
    createDescriptorSetLayout device layoutInfo Nothing

-- | Clean up descriptor set layout
destroyVulkanDescriptorSetLayout ∷ Device → DescriptorSetLayout → EngineM ε σ ()
destroyVulkanDescriptorSetLayout device layout = 
  liftIO $ destroyDescriptorSetLayout device layout Nothing
