-- | Swapchain framebuffer capture for debug.captureScreenshot (#643).
--
--   The render thread owns everything here: 'prepareCapture' runs
--   before command recording (allocates the host-visible staging
--   buffer), 'recordScreenshotCopy' is appended to the frame's own
--   command buffer after the render pass (barrier → copy → barrier
--   back, so the image still presents), and 'finishCapture' runs after
--   submit + present, waiting on the frame's fence before mapping the
--   staging memory and replying to the requester. PNG encoding happens
--   on the requesting (Lua) thread, never here.
module Engine.Graphics.Vulkan.Screenshot
  ( PendingCapture(..)
  , screenshotOrderOf
  , prepareCapture
  , recordScreenshotCopy
  , finishCapture
  ) where

import UPrelude
import qualified Data.ByteString as BS
import qualified Data.Text as T
import qualified Data.Vector as V
import qualified Engine.Core.Queue as Q
import Engine.Core.Monad
import Engine.Graphics.Types (ScreenshotGrab(..), ScreenshotOrder(..)
                             , ScreenshotRequest(..), SwapchainInfo(..))
import Engine.Graphics.Vulkan.BufferUtils (createVulkanBufferManual)
import Vulkan.Core10
import Vulkan.CStruct.Extends (SomeStruct(..))
import Vulkan.Zero

-- | A capture in flight within one 'drawFrame': the request being
--   answered plus the transient staging buffer the frame's command
--   buffer copies the swapchain image into.
data PendingCapture = PendingCapture
  { pcRequest ∷ ScreenshotRequest
  , pcOrder   ∷ ScreenshotOrder
  , pcMemory  ∷ DeviceMemory
  , pcBuffer  ∷ Buffer
  , pcImage   ∷ Image
  , pcWidth   ∷ Int
  , pcHeight  ∷ Int
  }

-- | Channel order of the 4-byte-per-pixel swapchain formats we can
--   capture. Anything else (packed 16-bit, 10-bit, …) is refused with
--   a clear error rather than mis-decoded — the swapchain picker
--   prefers B8G8R8A8, so in practice this is always 'Just'.
screenshotOrderOf ∷ Format → Maybe ScreenshotOrder
screenshotOrderOf FORMAT_B8G8R8A8_UNORM = Just ScreenshotBGRA
screenshotOrderOf FORMAT_B8G8R8A8_SRGB  = Just ScreenshotBGRA
screenshotOrderOf FORMAT_R8G8B8A8_UNORM = Just ScreenshotRGBA
screenshotOrderOf FORMAT_R8G8B8A8_SRGB  = Just ScreenshotRGBA
screenshotOrderOf _                     = Nothing

-- | Allocate the staging buffer for one capture and resolve the
--   swapchain image the frame renders into. On an uncapturable
--   swapchain — the surface lacks transfer-source usage (#700), or
--   the format can't be decoded — the request is answered with a
--   clear error and 'Nothing' comes back; the frame then proceeds as
--   if no capture was pending.
prepareCapture ∷ Device → PhysicalDevice → SwapchainInfo → Word32
               → ScreenshotRequest → EngineM ε σ (Maybe PendingCapture)
prepareCapture device pDevice si imageIndex req
  | not (siSupportsCapture si) = do
        liftIO $ Q.writeQueue (srReply req) $ Left
            ("captureScreenshot: unavailable on this system — the "
             <> "surface does not support transfer-source swapchain "
             <> "usage, so the presented image cannot be copied")
        pure Nothing
  | otherwise =
    case screenshotOrderOf (siSwapImgFormat si) of
        Nothing → do
            liftIO $ Q.writeQueue (srReply req) $ Left $
                "captureScreenshot: unsupported swapchain format "
                <> T.pack (show (siSwapImgFormat si))
            pure Nothing
        Just order → do
            let Extent2D w h = siSwapExtent si
                byteSize = fromIntegral w * fromIntegral h * 4 ∷ Int
            (mem, buf) ← createVulkanBufferManual device pDevice
                (fromIntegral byteSize)
                BUFFER_USAGE_TRANSFER_DST_BIT
                (MEMORY_PROPERTY_HOST_VISIBLE_BIT
                 ⌄ MEMORY_PROPERTY_HOST_COHERENT_BIT)
            pure $ Just PendingCapture
                { pcRequest = req
                , pcOrder   = order
                , pcMemory  = mem
                , pcBuffer  = buf
                , pcImage   = siSwapImgs si V.! fromIntegral imageIndex
                , pcWidth   = fromIntegral w
                , pcHeight  = fromIntegral h
                }

-- | Record the copy-out into the frame's command buffer, after
--   'cmdEndRenderPass' (both render-pass variants leave the swapchain
--   image in PRESENT_SRC_KHR — under MSAA it's the resolve target).
--   The image is transitioned to TRANSFER_SRC for the copy and back to
--   PRESENT_SRC so the subsequent present is untouched.
recordScreenshotCopy ∷ CommandBuffer → Image → Buffer
                     → Extent2D → IO ()
recordScreenshotCopy cmdBuf img buf (Extent2D w h) = do
    let colorRange = zero
            { aspectMask = IMAGE_ASPECT_COLOR_BIT
            , baseMipLevel = 0
            , levelCount = 1
            , baseArrayLayer = 0
            , layerCount = 1
            }
        toTransfer = zero
            { oldLayout = IMAGE_LAYOUT_PRESENT_SRC_KHR
            , newLayout = IMAGE_LAYOUT_TRANSFER_SRC_OPTIMAL
            , srcQueueFamilyIndex = QUEUE_FAMILY_IGNORED
            , dstQueueFamilyIndex = QUEUE_FAMILY_IGNORED
            , image = img
            , subresourceRange = colorRange
            , srcAccessMask = ACCESS_COLOR_ATTACHMENT_WRITE_BIT
            , dstAccessMask = ACCESS_TRANSFER_READ_BIT
            }
        toPresent = zero
            { oldLayout = IMAGE_LAYOUT_TRANSFER_SRC_OPTIMAL
            , newLayout = IMAGE_LAYOUT_PRESENT_SRC_KHR
            , srcQueueFamilyIndex = QUEUE_FAMILY_IGNORED
            , dstQueueFamilyIndex = QUEUE_FAMILY_IGNORED
            , image = img
            , subresourceRange = colorRange
            , srcAccessMask = ACCESS_TRANSFER_READ_BIT
            , dstAccessMask = zero
            }
        -- bufferRowLength/bufferImageHeight 0 = tightly packed rows.
        region = zero
            { bufferOffset = 0
            , bufferRowLength = 0
            , bufferImageHeight = 0
            , imageSubresource = zero
                { aspectMask = IMAGE_ASPECT_COLOR_BIT
                , mipLevel = 0
                , baseArrayLayer = 0
                , layerCount = 1
                }
            , imageOffset = Offset3D 0 0 0
            , imageExtent = Extent3D w h 1
            }
    cmdPipelineBarrier cmdBuf
        PIPELINE_STAGE_COLOR_ATTACHMENT_OUTPUT_BIT
        PIPELINE_STAGE_TRANSFER_BIT
        zero V.empty V.empty (V.singleton (SomeStruct toTransfer))
    cmdCopyImageToBuffer cmdBuf img IMAGE_LAYOUT_TRANSFER_SRC_OPTIMAL
        buf (V.singleton region)
    cmdPipelineBarrier cmdBuf
        PIPELINE_STAGE_TRANSFER_BIT
        PIPELINE_STAGE_BOTTOM_OF_PIPE_BIT
        zero V.empty V.empty (V.singleton (SomeStruct toPresent))

-- | After the frame's fence has signalled (the copy has executed): map
--   the staging memory, hand the raw pixels to the requester, and free
--   the transient buffer. Runs on the render thread; the reply queue
--   decouples the (slow) PNG encode onto the requesting thread.
finishCapture ∷ Device → PendingCapture → IO ()
finishCapture device pc = do
    let byteSize = pcWidth pc * pcHeight pc * 4
    dataPtr ← mapMemory device (pcMemory pc) 0
        (fromIntegral byteSize) zero
    pixels ← BS.packCStringLen (castPtr dataPtr, byteSize)
    unmapMemory device (pcMemory pc)
    destroyBuffer device (pcBuffer pc) Nothing
    freeMemory device (pcMemory pc) Nothing
    Q.writeQueue (srReply (pcRequest pc)) $ Right ScreenshotGrab
        { sgWidth  = pcWidth pc
        , sgHeight = pcHeight pc
        , sgOrder  = pcOrder pc
        , sgPixels = pixels
        }
