-- Engine/Graphics/Types.hs
{-# LANGUAGE Strict #-}
module Engine.Graphics.Types where
import UPrelude
import qualified Data.Text as T
import Vulkan.Core10 (Instance, PhysicalDevice, Device)
import Vulkan.Extensions.VK_KHR_surface (SurfaceKHR)

data GraphicsConfig = GraphicsConfig
  { gcAppName    ∷ T.Text   -- ^ Application name for Vulkan
  , gcWidth      ∷ Int      -- ^ Initial window width
  , gcHeight     ∷ Int      -- ^ Initial window height
  , gcDebugMode  ∷ Bool     -- ^ Enable Vulkan validation layers
  }

data VulkanState = VulkanState
  { vsInstance        ∷ Instance
  , vsPhysicalDevice  ∷ Maybe PhysicalDevice
  , vsDevice          ∷ Maybe Device
  , vsSurface         ∷ Maybe SurfaceKHR
  }
