module Engine.Graphics.Vulkan.Core
  ( getVulkanCore
  ) where

import UPrelude
import Engine.Core.Monad
import Engine.Core.State
import Engine.Core.Log (LogCategory(..))
import Engine.Core.Log.Monad (logAndThrowM)
import Engine.Core.Error.Exception (ExceptionType(..), GraphicsError(..))
import Engine.Graphics.Vulkan.Types.Core

-- | Get VulkanCore or throw an error
getVulkanCore ∷ GraphicsState → EngineM ε σ VulkanCore
getVulkanCore state = case vulkanCore state of
    Nothing → logAndThrowM CatVulkan (ExGraphics VulkanDeviceLost)
                           "VulkanCore is not initialized"
    Just c  → pure c
