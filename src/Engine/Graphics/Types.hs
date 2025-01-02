-- Engine/Graphics/Types.hs
{-# LANGUAGE Strict #-}
module Engine.Graphics.Types where
import UPrelude
import qualified Data.Vector as V
import Data.Word (Word32)
import qualified Data.Text as T
import Vulkan.Core10
import Vulkan.Extensions.VK_KHR_surface
import Vulkan.Extensions.VK_KHR_swapchain

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

-- | Device Queue Information
data DevQueues = DevQueues
  { graphicsQueue  ∷ Queue
  , presentQueue   ∷ Queue
  , graphicsFamIdx ∷ Word32
  , presentFamIdx  ∷ Word32
  }

-- | Swapchain Information
data SwapchainInfo = SwapchainInfo
  { siSwapchain     ∷ SwapchainKHR
  , siSwapImgs      ∷ V.Vector Image
  , siSwapImgFormat ∷ Format
  , siSwapExtent    ∷ Extent2D
  }

-- | Swapchain Support Details
data SwapchainSupportDetails = SwapchainSupportDetails
  { capabilities ∷ SurfaceCapabilitiesKHR
  , formats      ∷ V.Vector SurfaceFormatKHR
  , presentModes ∷ V.Vector PresentModeKHR
  }
