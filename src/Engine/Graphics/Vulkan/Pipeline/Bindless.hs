-- | Bindless-specific pipeline creation
module Engine.Graphics.Vulkan.Pipeline.Bindless
  ( createBindlessPipeline
  , createBindlessUIPipeline
  ) where

import UPrelude
import qualified Data.ByteString as BS
import qualified Data.Vector as V
import Engine.Core.Monad
import Engine.Core.State
import Engine.Graphics.Vulkan.Vertex
import Engine.Graphics.Vulkan.Types.Cleanup (Cleanup(..))
import Engine.Graphics.Vulkan.ShaderCode (bindlessVertexShaderCode, bindlessFragmentShaderCode
                                         , bindlessUIVertexShaderCode
                                         , bindlessUIFragmentShaderCode)
import Vulkan.Core10
import Vulkan.Zero
import Vulkan.CStruct.Extends

-- | Create a pipeline for bindless rendering (world camera)
createBindlessPipeline ∷ Device
                       → RenderPass
                       → Extent2D
                       → DescriptorSetLayout
                       → DescriptorSetLayout
                       → SampleCountFlagBits  -- NEW parameter
                       → EngineM ε σ (Pipeline, PipelineLayout)
createBindlessPipeline device renderPass swapExtent uniformLayout textureLayout sampleCount = do
  (pipeline, pipelineLayout) ← createBindlessPipelineWithShader 
      device renderPass swapExtent uniformLayout textureLayout sampleCount bindlessVertexShaderCode
  let cleanupAction = do
          destroyPipeline device pipeline Nothing
          destroyPipelineLayout device pipelineLayout Nothing
  modify $ \s → s { graphicsState = (graphicsState s) {
      vulkanCleanup = (vulkanCleanup (graphicsState s)) {
          cleanupBindless = cleanupAction
      }
  }}
  pure (pipeline, pipelineLayout)

-- | Create a pipeline for bindless UI rendering (UI camera)
createBindlessUIPipeline ∷ Device
                         → RenderPass
                         → Extent2D
                         → DescriptorSetLayout
                         → DescriptorSetLayout
                         → SampleCountFlagBits  -- NEW parameter
                         → EngineM ε σ (Pipeline, PipelineLayout)
createBindlessUIPipeline device renderPass swapExtent uniformLayout textureLayout sampleCount = do
  (pipeline, pipelineLayout) ← createBindlessPipelineWithShader 
      device renderPass swapExtent uniformLayout textureLayout sampleCount bindlessUIVertexShaderCode
  -- Build cleanup action
  let cleanupAction = do
          destroyPipeline device pipeline Nothing
          destroyPipelineLayout device pipelineLayout Nothing
  
  -- Store cleanup in state
  modify $ \s → s { graphicsState = (graphicsState s) {
      vulkanCleanup = (vulkanCleanup (graphicsState s)) {
          cleanupBindlessUI = cleanupAction
      }
  }}
  
  pure (pipeline, pipelineLayout)

-- | Internal helper to create bindless pipeline with specified vertex shader
-- Does NOT register cleanup - caller is responsible
createBindlessPipelineWithShader ∷ Device
                                 → RenderPass
                                 → Extent2D
                                 → DescriptorSetLayout
                                 → DescriptorSetLayout
                                 → SampleCountFlagBits
                                 → BS.ByteString  -- ^ Vertex shader code
                                 → EngineM ε σ (Pipeline, PipelineLayout)
createBindlessPipelineWithShader device renderPass swapExtent uniformLayout textureLayout sampleCount vertShaderCode = do
  -- Create shader modules
  vertShaderModule ← createShaderModule' device vertShaderCode
  fragShaderModule ← createShaderModule' device bindlessUIFragmentShaderCode

  let vertShaderStageInfo = zero
        { stage = SHADER_STAGE_VERTEX_BIT
        , module' = vertShaderModule
        , name = "main"
        }

      fragShaderStageInfo = zero
        { stage = SHADER_STAGE_FRAGMENT_BIT
        , module' = fragShaderModule
        , name = "main"
        }

      shaderStages = V.fromList
        [ SomeStruct vertShaderStageInfo
        , SomeStruct fragShaderStageInfo
        ]

      Extent2D w h = swapExtent

      vertexInputInfo = zero
        { vertexBindingDescriptions   = V.singleton getVertexBindingDescription
        , vertexAttributeDescriptions = getVertexAttributeDescriptions
        }

      inputAssembly = zero
        { topology = PRIMITIVE_TOPOLOGY_TRIANGLE_LIST
        , primitiveRestartEnable = False
        }

      viewport = zero
        { x = 0, y = 0
        , width  = fromIntegral w
        , height = fromIntegral h
        , minDepth = 0, maxDepth = 1
        }

      scissor = (zero ∷ Rect2D)
        { offset = Offset2D 0 0
        , extent = swapExtent
        }

      viewportState = zero
        { viewports = V.singleton viewport
        , scissors  = V.singleton scissor
        }

      rasterizer = zero
        { depthClampEnable        = False
        , rasterizerDiscardEnable = False
        , polygonMode             = POLYGON_MODE_FILL
        , lineWidth               = 1
        , cullMode                = CULL_MODE_NONE
        , frontFace               = FRONT_FACE_COUNTER_CLOCKWISE
        , depthBiasEnable         = False
        } ∷ PipelineRasterizationStateCreateInfo '[]

      multisampling = zero
        { sampleShadingEnable  = False
        , rasterizationSamples = sampleCount
        } ∷ PipelineMultisampleStateCreateInfo '[]

      colorBlendAttachment = zero
        { colorWriteMask = COLOR_COMPONENT_R_BIT
                           .|. COLOR_COMPONENT_G_BIT
                           .|. COLOR_COMPONENT_B_BIT
                           .|. COLOR_COMPONENT_A_BIT
        , blendEnable         = True
        , srcColorBlendFactor = BLEND_FACTOR_SRC_ALPHA
        , dstColorBlendFactor = BLEND_FACTOR_ONE_MINUS_SRC_ALPHA
        , colorBlendOp        = BLEND_OP_ADD
        , srcAlphaBlendFactor = BLEND_FACTOR_ONE
        , dstAlphaBlendFactor = BLEND_FACTOR_ZERO
        , alphaBlendOp        = BLEND_OP_ADD
        }

      colorBlending = zero
        { logicOpEnable  = False
        , attachments    = V.singleton colorBlendAttachment
        , blendConstants = (0, 0, 0, 0)
        } ∷ PipelineColorBlendStateCreateInfo '[]

      pipelineLayoutInfo = zero
        { setLayouts = V.fromList [uniformLayout, textureLayout]
        , pushConstantRanges = V.empty
        }

  -- Create pipeline layout WITHOUT allocResource
  pipelineLayout ← createPipelineLayout device pipelineLayoutInfo Nothing

  let pipelineInfo = zero
        { stages             = shaderStages
        , vertexInputState   = Just $ SomeStruct vertexInputInfo
        , inputAssemblyState = Just inputAssembly
        , viewportState      = Just $ SomeStruct viewportState
        , rasterizationState = Just $ SomeStruct rasterizer
        , multisampleState   = Just $ SomeStruct multisampling
        , colorBlendState    = Just $ SomeStruct colorBlending
        , layout             = pipelineLayout
        , renderPass         = renderPass
        , subpass            = 0
        , basePipelineHandle = zero
        , basePipelineIndex  = (-1)
        } ∷ GraphicsPipelineCreateInfo '[]

  -- Create pipeline WITHOUT allocResource
  (_, pipelinesVec) ← createGraphicsPipelines
    device zero (V.singleton $ SomeStruct pipelineInfo) Nothing

  let !pipeline = V.head pipelinesVec

  -- Destroy shader modules
  destroyShaderModule device vertShaderModule Nothing
  destroyShaderModule device fragShaderModule Nothing

  pure (pipeline, pipelineLayout)

-- | Helper to create shader module
createShaderModule' ∷ Device → BS.ByteString → EngineM ε σ ShaderModule
createShaderModule' device code = do
  let createInfo = zero { code = code }
  createShaderModule device createInfo Nothing
