{-# LANGUAGE Strict #-}
module Engine.Graphics.Vulkan.Framebuffer
  ( createVulkanFramebuffers
  , destroyVulkanFramebuffers
  , recordVulkanRenderCommands
  ) where

import UPrelude
import qualified Data.Vector as V
import Data.Bits ((.|.))
import Control.Monad (when)
import Engine.Core.Monad
import Engine.Core.Resource
import Engine.Graphics.Types
import Engine.Graphics.Vulkan.Types
import Engine.Graphics.Vulkan.Command
import Vulkan.Core10
import Vulkan.Zero
import Vulkan.CStruct.Extends

-- | Creates framebuffers for each swapchain image view
createVulkanFramebuffers ∷ Device 
                        → RenderPass
                        → SwapchainInfo
                        → V.Vector ImageView
                        → EngineM ε σ (V.Vector Framebuffer)
createVulkanFramebuffers device renderPass swapInfo imageViews = do
  V.mapM (createOneFramebuffer device renderPass swapInfo) imageViews
  where
    createOneFramebuffer dev rp si imageView = do
      let Extent2D w h = siSwapExtent si
          fbInfo = zero
            { renderPass      = rp
            , attachments     = V.singleton imageView
            , width          = w
            , height         = h
            , layers         = 1
            }
      
      allocResource (\fb → destroyFramebuffer dev fb Nothing) $
        createFramebuffer dev fbInfo Nothing

-- | Cleanup framebuffers
destroyVulkanFramebuffers ∷ Device → V.Vector Framebuffer → EngineM ε σ ()
destroyVulkanFramebuffers device =
  V.mapM_ (\fb → destroyFramebuffer device fb Nothing)

-- | Record render commands for a frame
recordVulkanRenderCommands ∷ CommandBuffer
                          → RenderPass
                          → Framebuffer
                          → Pipeline
                          → PipelineLayout
                          → Extent2D
                          → DescriptorSet
                          → V.Vector Buffer  -- ^ Vertex buffers
                          → EngineM ε σ ()
recordVulkanRenderCommands cmdBuffer renderPass framebuffer pipeline layout extent descSet vertexBuffers = do
  let renderPassInfo = zero
        { renderPass = renderPass
        , framebuffer = framebuffer
        , renderArea = zero
            { offset = zero
            , extent = extent
            }
        , clearValues = V.singleton zero  -- Uses default clear value (black)
        }
      
  beginVulkanCommandBuffer cmdBuffer
  
  cmdBeginRenderPass cmdBuffer renderPassInfo SUBPASS_CONTENTS_INLINE
  
  cmdBindPipeline cmdBuffer PIPELINE_BIND_POINT_GRAPHICS pipeline
  
  -- Bind descriptor sets
  cmdBindDescriptorSets cmdBuffer
                       PIPELINE_BIND_POINT_GRAPHICS
                       layout
                       0  -- First set
                       (V.singleton descSet)
                       V.empty  -- No dynamic offsets
  
  -- Bind vertex buffers if any exist
  when (not $ V.null vertexBuffers) $
    cmdBindVertexBuffers cmdBuffer
                        0  -- First binding
                        vertexBuffers
                        (V.singleton 0)  -- Offsets
  
  -- Draw command - adjust vertices count as needed
  cmdDraw cmdBuffer
         4  -- vertex count
         1  -- instance count
         0  -- first vertex
         0  -- first instance
  
  cmdEndRenderPass cmdBuffer
  
  endVulkanCommandBuffer cmdBuffer
