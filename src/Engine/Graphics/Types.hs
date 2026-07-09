{-# LANGUAGE Strict #-}
module Engine.Graphics.Types where
import UPrelude
import qualified Data.ByteString as BS
import qualified Data.Vector as V
import qualified Engine.Core.Queue as Q
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
  , siSwapImgViews  ∷ V.Vector ImageView
  , siSwapImgFormat ∷ Format
  , siSwapExtent    ∷ Extent2D
  }

-- | Swapchain Support Details
data SwapchainSupportDetails = SwapchainSupportDetails
  { capabilities ∷ SurfaceCapabilitiesKHR
  , formats      ∷ V.Vector SurfaceFormatKHR
  , presentModes ∷ V.Vector PresentModeKHR
  }

-- | Channel order of a raw framebuffer grab (#643). The swapchain
--   format decides which one the render thread hands back; the PNG
--   encoder swizzles both to RGBA. A tiny enum instead of the Vulkan
--   'Format' so downstream consumers don't pattern-match on the full
--   format zoo.
data ScreenshotOrder = ScreenshotBGRA | ScreenshotRGBA
  deriving (Eq, Show)

-- | Raw pixels of one captured frame (#643): tightly packed 4-byte
--   pixels, row 0 = top of the screen, 'sgWidth'×'sgHeight' in
--   FRAMEBUFFER pixels (the swapchain extent — the same space
--   engine.getFramebufferSize reports, which on HiDPI displays differs
--   from window coordinates by the DPI scale).
data ScreenshotGrab = ScreenshotGrab
  { sgWidth  ∷ Int
  , sgHeight ∷ Int
  , sgOrder  ∷ ScreenshotOrder
  , sgPixels ∷ BS.ByteString
  }

-- | One in-flight screenshot request (#643). The debug-console verb
--   enqueues this on 'screenshotRequestQueue' and blocks (with a
--   timeout) on 'srReply'; the render thread fulfils it while drawing
--   the next frame. Errors (no swapchain, exotic format) come back as
--   'Left' with a clear message.
newtype ScreenshotRequest = ScreenshotRequest
  { srReply ∷ Q.Queue (Either Text ScreenshotGrab)
  }


