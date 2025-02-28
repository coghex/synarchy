-- Engine/Graphics/Types.hs
{-# LANGUAGE Strict #-}
module Engine.Graphics.Types where
import UPrelude
import qualified Data.Vector as V
import qualified Data.Map as Map
import Engine.Graphics.Vulkan.Types
import Vulkan.Core10
import Vulkan.Extensions.VK_KHR_surface
import Vulkan.Extensions.VK_KHR_swapchain

data VulkanState = VulkanState
  { vsInstance        ∷ Instance
  , vsPhysicalDevice  ∷ Maybe PhysicalDevice
  , vsDevice          ∷ Maybe Device
  , vsSurface         ∷ Maybe SurfaceKHR
  , vsSwapchainInfo   ∷ Maybe SwapchainInfo
  , vsSyncObjects     ∷ Maybe SyncObjects
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


