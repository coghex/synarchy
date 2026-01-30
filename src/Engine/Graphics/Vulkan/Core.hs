module Engine.Graphics.Vulkan.Core
  ( getVulkanCore
  ) where

import UPrelude
import Engine.Core.Monad
import Engine.Core.State
import Engine.Core.Error.Exception (throwGraphicsError, GraphicsError(..))
import Engine.Graphics.Vulkan.Types.Core

-- | Get VulkanCore or throw an error
getVulkanCore ∷ GraphicsState → EngineM ε σ VulkanCore
getVulkanCore state = case vulkanCore state of
    Nothing → throwGraphicsError VulkanDeviceLost "Vulkan not initialized"
    Just c  → pure c
