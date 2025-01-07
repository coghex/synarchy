{-# LANGUAGE Strict #-}
module Engine.Graphics.Vulkan.Pipeline
  ( createVulkanRenderPipeline
  , createVulkanRenderPass
  , destroyVulkanRenderPass
  ) where

import UPrelude
import Data.Bits ((.|.))
import qualified Data.Vector as V
import qualified Data.ByteString as BS
import Engine.Core.Monad
import Engine.Core.Resource
import Engine.Graphics.Types
import Engine.Graphics.Vulkan.Types
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

createVulkanRenderPipeline ∷ Device 
                          → RenderPass
                          → Extent2D
                          → DescriptorSetLayout
                          → EngineM ε σ (Pipeline, PipelineLayout)
createVulkanRenderPipeline device renderPass swapExtent descriptorLayout = do
    -- Create shader stages first
    shaderStages ← createVulkanShaderStages device
    
    let Extent2D w h = swapExtent
        -- Vertex input state
        vertexInputInfo = zero
          { vertexBindingDescriptions   = V.singleton getVertexBindingDescription
          , vertexAttributeDescriptions = getVertexAttributeDescriptions
          }
        
        -- Input assembly
        inputAssembly = zero
          { topology = PRIMITIVE_TOPOLOGY_TRIANGLE_LIST
          , primitiveRestartEnable = False
          }
        
        -- Viewport and scissors
        viewport = zero
          { x = 0
          , y = 0
          , width  = fromIntegral w
          , height = fromIntegral h
          , minDepth = 0
          , maxDepth = 1
          }
        
        scissor = (zero ∷ Rect2D)
          { offset = Offset2D 0 0
          , extent = swapExtent
          }
        
        viewportState = zero
          { viewports = V.singleton viewport
          , scissors  = V.singleton scissor
          }
        
        -- Rasterizer
        rasterizer = (zero ∷ PipelineRasterizationStateCreateInfo '[])
          { depthClampEnable        = False
          , rasterizerDiscardEnable = False
          , polygonMode             = POLYGON_MODE_FILL
          , lineWidth               = 1
          , cullMode                = CULL_MODE_NONE
          , frontFace              = FRONT_FACE_COUNTER_CLOCKWISE
          , depthBiasEnable        = False
          }
        
        -- Multisampling
        multisampling = (zero ∷ PipelineMultisampleStateCreateInfo '[])
          { sampleShadingEnable  = False
          , rasterizationSamples = SAMPLE_COUNT_1_BIT
          }
        
        colorBlendAttachment = (zero ∷ PipelineColorBlendAttachmentState)
          { colorWriteMask = COLOR_COMPONENT_R_BIT
                             ⌄ COLOR_COMPONENT_G_BIT
                             ⌄ COLOR_COMPONENT_B_BIT
                             ⌄ COLOR_COMPONENT_A_BIT
          , blendEnable    = False
          , srcColorBlendFactor = BLEND_FACTOR_SRC_ALPHA
          , dstColorBlendFactor = BLEND_FACTOR_ONE_MINUS_SRC_ALPHA
          , colorBlendOp   = BLEND_OP_ADD
          , srcAlphaBlendFactor = BLEND_FACTOR_ONE
          , dstAlphaBlendFactor = BLEND_FACTOR_ZERO
          , alphaBlendOp   = BLEND_OP_ADD
          }
        
        colorBlending = (zero ∷ PipelineColorBlendStateCreateInfo '[])
          { logicOpEnable   = False
          , attachments     = V.singleton colorBlendAttachment
          , blendConstants  = (0, 0, 0, 0)
          }
        
        -- Pipeline layout
        pipelineLayoutInfo = zero
          { setLayouts      = V.singleton descriptorLayout
          , pushConstantRanges = V.empty
          }
    
    -- Create pipeline layout
    pipelineLayout ← allocResource 
        (\layout → destroyPipelineLayout device layout Nothing) $
        createPipelineLayout device pipelineLayoutInfo Nothing
    
    -- Create the graphics pipeline
    let pipelineInfo = (zero ∷ GraphicsPipelineCreateInfo '[])
          { stages             = shaderStages
          , vertexInputState   = Just $ SomeStruct vertexInputInfo
          , inputAssemblyState = Just inputAssembly
          , viewportState      = Just $ SomeStruct viewportState
          , rasterizationState = Just $ SomeStruct rasterizer
          , multisampleState   = Just $ SomeStruct multisampling
          , colorBlendState    = Just $ SomeStruct colorBlending
          , layout            = pipelineLayout
          , renderPass        = renderPass
          , subpass          = 0
          }
    
    -- Create pipeline
    (result, pipelinesVec) ← createGraphicsPipelines 
        device zero (V.singleton $ SomeStruct pipelineInfo) Nothing
    
    -- Get the first pipeline from the vector
    let pipeline = V.head pipelinesVec
    
    -- Set up resource cleanup
    _ ← allocResource 
        (\p → destroyPipeline device p Nothing)
        (pure pipeline)
    
    pure (pipeline, pipelineLayout)

destroyVulkanRenderPass ∷ Device → RenderPass → EngineM ε σ ()
destroyVulkanRenderPass device renderPass =
    destroyRenderPass device renderPass Nothing
