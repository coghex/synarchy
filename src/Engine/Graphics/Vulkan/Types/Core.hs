module Engine.Graphics.Vulkan.Types.Core
  ( VulkanCore(..)
  ) where

import UPrelude
import Engine.Graphics.Types (DevQueues)
import Engine.Graphics.Vulkan.Capability (TextureSystemCapability)
import qualified Vulkan.Core10 as Vk

-- | Core Vulkan handles that are always used together
-- These are created during initialization and destroyed at shutdown
data VulkanCore = VulkanCore
  { vcInstance          ∷ !Vk.Instance
  , vcPhysicalDevice    ∷ !Vk.PhysicalDevice
  , vcDevice            ∷ !Vk.Device
  , vcQueues            ∷ !DevQueues
  } deriving (Show)
