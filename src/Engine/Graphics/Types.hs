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

-- | What the frame loop renders into and (for a swapchain) presents
--   from. Offscreen mode (#650) renders the identical pipeline into
--   self-allocated images that are never presented — only read back by
--   debug.captureScreenshot — so the target carries either the
--   presentable swapchain or the offscreen images' backing memory.
data RenderTarget
  = TargetSwapchain SwapchainKHR
    -- ^ Windowed: images owned by the swapchain, presented each frame.
  | TargetOffscreen (V.Vector DeviceMemory)
    -- ^ Offscreen (#650): one self-allocated image per frame in
    --   flight; the vector is their backing memory (images/views live
    --   in 'siSwapImgs'/'siSwapImgViews' like swapchain ones).

-- | Render-target information. Named for the swapchain case it was
--   born as; offscreen mode (#650) fills the same record for its
--   unpresented images so every consumer of format/extent/images
--   works unchanged — only 'siTarget' distinguishes the modes.
data SwapchainInfo = SwapchainInfo
  { siTarget        ∷ RenderTarget
  , siSwapImgs      ∷ V.Vector Image
  , siSwapImgViews  ∷ V.Vector ImageView
  , siSwapImgFormat ∷ Format
  , siSwapExtent    ∷ Extent2D
  , siSupportsCapture ∷ Bool
    -- ^ The target was created with TRANSFER_SRC usage, so
    --   debug.captureScreenshot (#643) can copy the rendered image.
    --   False on surfaces without transfer-source support (#700) — the
    --   capture verb reports itself unavailable instead of the
    --   swapchain request failing. Always True offscreen (the image
    --   usage is ours to choose).
  }

-- | The layout the render pass leaves the (resolved) color image in,
--   and therefore the steady-state layout debug.captureScreenshot's
--   copy-out barriers must restore: PRESENT_SRC for a swapchain (the
--   present engine consumes it), TRANSFER_SRC offscreen (the capture
--   copy is the only consumer, and PRESENT_SRC is illegal without the
--   swapchain device extension).
renderedImageLayout ∷ RenderTarget → ImageLayout
renderedImageLayout (TargetSwapchain _) = IMAGE_LAYOUT_PRESENT_SRC_KHR
renderedImageLayout (TargetOffscreen _) = IMAGE_LAYOUT_TRANSFER_SRC_OPTIMAL

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


