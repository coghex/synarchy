{-# LANGUAGE Strict #-}
module Engine.Graphics.Vulkan.Pipeline
  ( createVulkanRenderPass
  ) where

import UPrelude
import qualified Data.Vector as V
import qualified Data.ByteString as BS
import Engine.Core.Types
import Engine.Core.Monad
import Engine.Core.Resource
import Engine.Core.State
import Engine.Graphics.Types
import Engine.Graphics.Vulkan.Types
import Engine.Graphics.Vulkan.Types.Cleanup
import Engine.Graphics.Vulkan.Types.Texture
import Engine.Graphics.Vulkan.Shader
import Engine.Graphics.Vulkan.Vertex
import Vulkan.Core10
import Vulkan.Zero
import Vulkan.CStruct.Extends

-- | Creates a render pass for the swapchain.
-- When sampleCount > 1, uses a multisampled color attachment with resolve.
createVulkanRenderPass ∷ Device → Format → SampleCountFlagBits → EngineM ε σ RenderPass
createVulkanRenderPass device swapchainImageFormat sampleCount = do
    renderPass ← if sampleCount == SAMPLE_COUNT_1_BIT
        then createRenderPassNoMSAA device swapchainImageFormat
        else createRenderPassMSAA device swapchainImageFormat sampleCount

    let cleanupAction = destroyRenderPass device renderPass Nothing
    modify $ \s → s { graphicsState = (graphicsState s) {
        vulkanCleanup = (vulkanCleanup (graphicsState s)) {
            cleanupRenderPass = cleanupAction } } }
    pure renderPass

-- | Single-sample render pass (no MSAA) — original behavior
createRenderPassNoMSAA ∷ Device → Format → EngineM ε σ RenderPass
createRenderPassNoMSAA device swapchainImageFormat = do
    let attachmentDesc = zero
          { format         = swapchainImageFormat
          , samples        = SAMPLE_COUNT_1_BIT
          , loadOp         = ATTACHMENT_LOAD_OP_CLEAR
          , storeOp        = ATTACHMENT_STORE_OP_STORE
          , stencilLoadOp  = ATTACHMENT_LOAD_OP_DONT_CARE
          , stencilStoreOp = ATTACHMENT_STORE_OP_DONT_CARE
          , initialLayout  = IMAGE_LAYOUT_UNDEFINED
          , finalLayout    = IMAGE_LAYOUT_PRESENT_SRC_KHR
          }

        subpass = zero
          { pipelineBindPoint = PIPELINE_BIND_POINT_GRAPHICS
          , colorAttachments  = V.singleton $ zero
              { attachment = 0
              , layout     = IMAGE_LAYOUT_COLOR_ATTACHMENT_OPTIMAL
              }
          }

        dependency = zero
          { srcSubpass    = SUBPASS_EXTERNAL
          , dstSubpass    = 0
          , srcStageMask  = PIPELINE_STAGE_COLOR_ATTACHMENT_OUTPUT_BIT
          , srcAccessMask = zero
          , dstStageMask  = PIPELINE_STAGE_COLOR_ATTACHMENT_OUTPUT_BIT
          , dstAccessMask = ACCESS_COLOR_ATTACHMENT_READ_BIT
                             .|. ACCESS_COLOR_ATTACHMENT_WRITE_BIT
          }

        renderPassInfo = zero
          { attachments  = V.singleton attachmentDesc
          , subpasses    = V.singleton subpass
          , dependencies = V.singleton dependency
          }

    createRenderPass device renderPassInfo Nothing

-- | Multisampled render pass (MSAA enabled)
-- Attachment 0: multisampled color (rendered to, then discarded)
-- Attachment 1: resolve target (swapchain image, presented)
createRenderPassMSAA ∷ Device → Format → SampleCountFlagBits → EngineM ε σ RenderPass
createRenderPassMSAA device swapchainImageFormat sampleCount = do
    let -- Attachment 0: Multisampled color attachment
        msaaColorAttachment = zero
          { format         = swapchainImageFormat
          , samples        = sampleCount
          , loadOp         = ATTACHMENT_LOAD_OP_CLEAR
          , storeOp        = ATTACHMENT_STORE_OP_DONT_CARE  -- We resolve, don't need to store
          , stencilLoadOp  = ATTACHMENT_LOAD_OP_DONT_CARE
          , stencilStoreOp = ATTACHMENT_STORE_OP_DONT_CARE
          , initialLayout  = IMAGE_LAYOUT_UNDEFINED
          , finalLayout    = IMAGE_LAYOUT_COLOR_ATTACHMENT_OPTIMAL
          }

        -- Attachment 1: Resolve target (single-sample swapchain image)
        resolveAttachment = zero
          { format         = swapchainImageFormat
          , samples        = SAMPLE_COUNT_1_BIT
          , loadOp         = ATTACHMENT_LOAD_OP_DONT_CARE  -- Overwritten by resolve
          , storeOp        = ATTACHMENT_STORE_OP_STORE     -- Present this
          , stencilLoadOp  = ATTACHMENT_LOAD_OP_DONT_CARE
          , stencilStoreOp = ATTACHMENT_STORE_OP_DONT_CARE
          , initialLayout  = IMAGE_LAYOUT_UNDEFINED
          , finalLayout    = IMAGE_LAYOUT_PRESENT_SRC_KHR
          }

        subpass = zero
          { pipelineBindPoint = PIPELINE_BIND_POINT_GRAPHICS
          , colorAttachments  = V.singleton $ zero
              { attachment = 0  -- Render to the MSAA image
              , layout     = IMAGE_LAYOUT_COLOR_ATTACHMENT_OPTIMAL
              }
          , resolveAttachments = V.singleton $ zero
              { attachment = 1  -- Resolve to the swapchain image
              , layout     = IMAGE_LAYOUT_COLOR_ATTACHMENT_OPTIMAL
              }
          }

        dependency = zero
          { srcSubpass    = SUBPASS_EXTERNAL
          , dstSubpass    = 0
          , srcStageMask  = PIPELINE_STAGE_COLOR_ATTACHMENT_OUTPUT_BIT
          , srcAccessMask = zero
          , dstStageMask  = PIPELINE_STAGE_COLOR_ATTACHMENT_OUTPUT_BIT
          , dstAccessMask = ACCESS_COLOR_ATTACHMENT_READ_BIT
                             .|. ACCESS_COLOR_ATTACHMENT_WRITE_BIT
          }

        renderPassInfo = zero
          { attachments  = V.fromList [msaaColorAttachment, resolveAttachment]
          , subpasses    = V.singleton subpass
          , dependencies = V.singleton dependency
          }

    createRenderPass device renderPassInfo Nothing
