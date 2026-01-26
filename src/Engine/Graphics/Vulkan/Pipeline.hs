{-# LANGUAGE Strict #-}
module Engine.Graphics.Vulkan.Pipeline
  ( createVulkanRenderPass
  , destroyVulkanRenderPass
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
import Engine.Graphics.Vulkan.Types.Texture
import Engine.Graphics.Vulkan.Shader
import Engine.Graphics.Vulkan.Vertex
import Vulkan.Core10
import Vulkan.Zero
import Vulkan.CStruct.Extends

-- | Creates a render pass for the swapchain
createVulkanRenderPass ∷ Device → Format → EngineM ε σ RenderPass
createVulkanRenderPass device swapchainImageFormat = do
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
          , subpasses   = V.singleton subpass
          , dependencies = V.singleton dependency
          }
    
    allocResource (\rpass → destroyRenderPass device rpass Nothing) $
        createRenderPass device renderPassInfo Nothing


destroyVulkanRenderPass ∷ Device → RenderPass → EngineM ε σ ()
destroyVulkanRenderPass device renderPass =
    destroyRenderPass device renderPass Nothing
