-- src/Engine/Graphics/Vulkan/Sampler.hs
module Engine.Graphics.Vulkan.Sampler
  ( createVulkanSampler
  , destroyVulkanSampler
  ) where

import UPrelude
import Engine.Core.Monad
import Engine.Core.Resource
import Engine.Graphics.Vulkan.Types
import Vulkan.Core10
import Vulkan.Zero
import Vulkan.CStruct.Extends

-- | Create a Vulkan sampler with standard texture sampling settings
createVulkanSampler ∷ Device
                    → PhysicalDevice
                    → EngineM ε σ Sampler
createVulkanSampler device pDevice = do
  props ← getPhysicalDeviceProperties pDevice
  let maxAnisotropy = maxSamplerAnisotropy $ limits props
      samplerInfo = zero
        { magFilter = FILTER_LINEAR
        , minFilter = FILTER_LINEAR
        , addressModeU = SAMPLER_ADDRESS_MODE_REPEAT
        , addressModeV = SAMPLER_ADDRESS_MODE_REPEAT
        , addressModeW = SAMPLER_ADDRESS_MODE_REPEAT
        , anisotropyEnable = True
        , maxAnisotropy = maxAnisotropy
        , borderColor = BORDER_COLOR_INT_OPAQUE_BLACK
        , unnormalizedCoordinates = False
        , compareEnable = False
        , compareOp = COMPARE_OP_ALWAYS
        , mipmapMode = SAMPLER_MIPMAP_MODE_LINEAR
        , mipLodBias = 0
        , minLod = 0
        , maxLod = 0
        }

  allocResource (\s → destroySampler device s Nothing) $
    createSampler device samplerInfo Nothing

-- | Clean up sampler
destroyVulkanSampler ∷ Device → Sampler → EngineM ε σ ()
destroyVulkanSampler device sampler =
  liftIO $ destroySampler device sampler Nothing
