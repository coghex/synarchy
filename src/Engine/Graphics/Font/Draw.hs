{-# LANGUAGE Strict #-}
module Engine.Graphics.Font.Draw
    ( createFontPipeline
    , createFontUIPipeline
    , createFontQuadBuffer
    , createFontTextureLayout
    , layoutText
    , layoutTextUI
    , cleanupPendingInstanceBuffers
    ) where

import UPrelude
import Engine.Graphics.Font.Data
import Engine.Asset.Types
import Engine.Core.Monad
import Engine.Core.State
import Engine.Core.Resource
import Engine.Core.Error.Exception
import Engine.Scene.Types
import Engine.Scene.Base
import Engine.Graphics.Types
import Engine.Graphics.Vulkan.Buffer (createVulkanBuffer, copyBuffer)
import Engine.Graphics.Vulkan.BufferUtils (createVulkanBufferManual)
import Engine.Graphics.Vulkan.Command
import Engine.Graphics.Vulkan.ShaderCode (fontVertexShaderCode, fontFragmentShaderCode
                                         , fontUIVertexShaderCode)
import Engine.Graphics.Vulkan.Types.Descriptor
import qualified Data.Map.Strict as Map
import qualified Data.Vector as V
import qualified Data.Vector.Storable as VS
import qualified Data.Text as T
import Foreign.Storable (Storable(..), pokeElemOff)
import Foreign.Ptr (castPtr)
import Foreign.Marshal.Utils (copyBytes)
import Vulkan.Core10
import Vulkan.Zero
import Vulkan.CStruct.Extends

-----------------------------------------------------------
-- Text Rendering API
-----------------------------------------------------------

-- | Layout text into glyph instances (converts pixels to world coords)
-- TODO: text with its own coordinated UBO transform would probably be a better
-- system, but this is simpler for now, and works fine for screen-space text.
layoutText ∷ FontAtlas → Float → Float → Float → Float
  → Text → (Float, Float, Float, Float) → V.Vector GlyphInstance
layoutText atlas startX startY screenW screenH text color =
    let chars = T.unpack text
        (_, instances) = foldl layoutChar (startX, []) chars
    in V.fromList (reverse instances)
  where
    pixelToNdcX px = (px / screenW) * 2.0 - 1.0
    pixelToNdcY py = 1.0 - (py / screenH) * 2.0
    pixelToNdcW pw = (pw / screenW) * 2.0
    pixelToNdcH ph = (ph / screenH) * 2.0
    layoutChar (currentX, acc) char =
        case Map.lookup char (faGlyphData atlas) of
            Nothing → (currentX, acc)  -- Skip unknown character
            Just glyphInfo →
                let (bearingX, bearingY) = giBearing glyphInfo
                    (w, h) = giSize glyphInfo
                    (u0, v0, u1, v1) = giUVRect glyphInfo
                    -- Position glyph (baseline-aligned)
                    pxX = currentX + bearingX
                    pxY = startY - bearingY
                    ndcX = pixelToNdcX pxX
                    ndcY = pixelToNdcY pxY
                    ndcW = pixelToNdcW w
                    ndcH = pixelToNdcH h
                    
                    instance' = GlyphInstance
                        { instancePosition = (ndcX, ndcY)
                        , instanceSize = (ndcW, ndcH)
                        , instanceUVRect = (u0, v0, u1, v1)
                        , instanceColor = color }
                    
                    nextX = currentX + giAdvance glyphInfo
                in (nextX, instance' : acc)

-----------------------------------------------------------
-- Instance Buffer Management
-----------------------------------------------------------

-- | Cleanup instance buffers from the previous frame
-- Call this at the START of each frame, after waiting for the fence
cleanupPendingInstanceBuffers ∷ EngineM ε σ ()
cleanupPendingInstanceBuffers = do
    state ← gets graphicsState
    case vulkanDevice state of
        Nothing → pure ()
        Just device → do
            let pending = pendingInstanceBuffers state
            -- Destroy all pending buffers from last frame
            V.forM_ pending $ \(buffer, memory) → liftIO $ do
                destroyBuffer device buffer Nothing
                freeMemory device memory Nothing
            -- Clear the pending list
            modify $ \s → s 
                { graphicsState = (graphicsState s) 
                    { pendingInstanceBuffers = V.empty 
                    }
                }

-- | Create instance buffer for glyphs (returns both buffer and memory)
createInstanceBuffer ∷ Device → PhysicalDevice → V.Vector GlyphInstance 
                     → EngineM ε σ (Buffer, DeviceMemory)
createInstanceBuffer device pDevice instances = do
    let instanceSize = fromIntegral $ sizeOf (undefined ∷ GlyphInstance)
        bufferSize = instanceSize * fromIntegral (V.length instances)
    
    -- Create buffer using your existing helper (returns (memory, buffer))
    (memory, buffer) ← createVulkanBuffer device pDevice bufferSize
        BUFFER_USAGE_VERTEX_BUFFER_BIT
        (MEMORY_PROPERTY_HOST_VISIBLE_BIT .|. MEMORY_PROPERTY_HOST_COHERENT_BIT)
    
    -- Upload instance data
    dataPtr ← mapMemory device memory 0 bufferSize zero
    liftIO $ V.imapM_ (\i inst → pokeElemOff (castPtr dataPtr) i inst) instances
    unmapMemory device memory
    
    pure (buffer, memory)


-----------------------------------------------------------
-- Pipeline Creation
-----------------------------------------------------------

-- | Create font rendering pipeline with instancing
createFontPipeline ∷ Device → RenderPass → Extent2D → DescriptorSetLayout 
                   → EngineM ε σ (Pipeline, PipelineLayout, DescriptorSetLayout)
createFontPipeline device renderPass swapExtent uniformLayout = do
    -- Create font-specific texture layout (must persist)
    fontTexLayout ← createFontTextureLayout device
    
    let Extent2D w h = swapExtent
        pipelineLayoutInfo = zero
          { setLayouts = V.fromList [uniformLayout, fontTexLayout]
          , pushConstantRanges = V.empty
          }
    
    pipelineLayout ← createPipelineLayout device pipelineLayoutInfo Nothing
    
    -- Create shader modules (will be destroyed after pipeline creation)
    vertModule ← createShaderModule device zero { code = fontVertexShaderCode } Nothing
    fragModule ← createShaderModule device zero { code = fontFragmentShaderCode } Nothing
    
    let vertStageInfo = zero 
          { stage   = SHADER_STAGE_VERTEX_BIT
          , module' = vertModule
          , name    = "main"
          }
        fragStageInfo = zero 
          { stage   = SHADER_STAGE_FRAGMENT_BIT
          , module' = fragModule
          , name    = "main"
          }
        shaderStages = V.fromList [SomeStruct vertStageInfo, SomeStruct fragStageInfo]
    
        -- Vertex input:  per-vertex (quad) + per-instance (glyph data)
        vertexBindings = V.fromList
            [ zero  -- Binding 0: per-vertex (quad template)
              { binding = 0
              , stride = 16  -- vec2 pos + vec2 uv
              , inputRate = VERTEX_INPUT_RATE_VERTEX
              }
            , zero  -- Binding 1: per-instance (glyph data)
              { binding = 1
              , stride = 48  -- GlyphInstance size
              , inputRate = VERTEX_INPUT_RATE_INSTANCE
              }
            ]
    
        vertexAttributes = V.fromList
            [ -- Per-vertex (quad template)
              zero { location = 0, binding = 0, format = FORMAT_R32G32_SFLOAT, offset = 0 }  -- pos
            , zero { location = 1, binding = 0, format = FORMAT_R32G32_SFLOAT, offset = 8 }  -- uv
            , -- Per-instance (glyph data)
              zero { location = 2, binding = 1, format = FORMAT_R32G32_SFLOAT, offset = 0 }  -- glyphPos
            , zero { location = 3, binding = 1, format = FORMAT_R32G32_SFLOAT, offset = 8 }  -- glyphSize
            , zero { location = 4, binding = 1, format = FORMAT_R32G32B32A32_SFLOAT, offset = 16 }  -- glyphUV
            , zero { location = 5, binding = 1, format = FORMAT_R32G32B32A32_SFLOAT, offset = 32 }  -- glyphColor
            ]
    
        vertexInputInfo = zero
          { vertexBindingDescriptions = vertexBindings
          , vertexAttributeDescriptions = vertexAttributes
          }
    
        inputAssembly = zero
          { topology = PRIMITIVE_TOPOLOGY_TRIANGLE_LIST
          , primitiveRestartEnable = False
          }
    
        viewport = zero
          { x = 0, y = 0
          , width = fromIntegral w
          , height = fromIntegral h
          , minDepth = 0, maxDepth = 1
          }
    
        scissor = (zero ∷ Rect2D)
          { offset = Offset2D 0 0
          , extent = swapExtent
          }
    
        viewportState = zero
          { viewports = V.singleton viewport
          , scissors = V.singleton scissor
          }
    
        rasterizer = (zero ∷ PipelineRasterizationStateCreateInfo '[])
          { depthClampEnable = False
          , rasterizerDiscardEnable = False
          , polygonMode = POLYGON_MODE_FILL
          , lineWidth = 1
          , cullMode = CULL_MODE_NONE
          , frontFace = FRONT_FACE_COUNTER_CLOCKWISE
          , depthBiasEnable = False
          }
    
        multisampling = (zero ∷ PipelineMultisampleStateCreateInfo '[])
          { sampleShadingEnable = False
          , rasterizationSamples = SAMPLE_COUNT_1_BIT
          }
    
        -- Alpha blending for text
        colorBlendAttachment = zero
          { colorWriteMask = COLOR_COMPONENT_R_BIT .|. COLOR_COMPONENT_G_BIT 
                            .|. COLOR_COMPONENT_B_BIT .|.  COLOR_COMPONENT_A_BIT
          , blendEnable = True
          , srcColorBlendFactor = BLEND_FACTOR_SRC_ALPHA
          , dstColorBlendFactor = BLEND_FACTOR_ONE_MINUS_SRC_ALPHA
          , colorBlendOp = BLEND_OP_ADD
          , srcAlphaBlendFactor = BLEND_FACTOR_ONE
          , dstAlphaBlendFactor = BLEND_FACTOR_ZERO
          , alphaBlendOp = BLEND_OP_ADD
          }
    
        colorBlending = (zero ∷ PipelineColorBlendStateCreateInfo '[])
          { logicOpEnable = False
          , attachments = V.singleton colorBlendAttachment
          , blendConstants = (0, 0, 0, 0)
          }
    
        pipelineInfo = (zero ∷ GraphicsPipelineCreateInfo '[])
          { stages = shaderStages
          , vertexInputState = Just $ SomeStruct vertexInputInfo
          , inputAssemblyState = Just inputAssembly
          , viewportState = Just $ SomeStruct viewportState
          , rasterizationState = Just $ SomeStruct rasterizer
          , multisampleState = Just $ SomeStruct multisampling
          , colorBlendState = Just $ SomeStruct colorBlending
          , layout = pipelineLayout
          , renderPass = renderPass
          , subpass = 0
          , basePipelineHandle = zero
          , basePipelineIndex = -1
          }
    
    -- Create pipeline - force evaluation before destroying shaders
    (_, pipelinesVec) ← createGraphicsPipelines 
        device zero (V.singleton $ SomeStruct pipelineInfo) Nothing
    
    let !pipeline = V.head pipelinesVec
    
    -- NOW it's safe to destroy shader modules
    destroyShaderModule device vertModule Nothing
    destroyShaderModule device fragModule Nothing
    
    pure (pipeline, pipelineLayout, fontTexLayout)

-----------------------------------------------------------
-- Quad Buffer Creation (unchanged from original)
-----------------------------------------------------------

-- | Create shared quad buffer for all text rendering
createFontQuadBuffer ∷ Device → PhysicalDevice → Queue → CommandPool 
                     → EngineM ε σ (Buffer, DeviceMemory)
createFontQuadBuffer device pDevice queue cmdPool = do
    -- Simple quad vertices [0,0] to [1,1] (will be scaled per-instance)
    let quadVertices = VS.fromList
            [ 0.0, 0.0, 0.0, 0.0  -- Bottom-left:    pos, uv
            , 1.0, 0.0, 1.0, 0.0  -- Bottom-right
            , 1.0, 1.0, 1.0, 1.0  -- Top-right
            , 1.0, 1.0, 1.0, 1.0  -- Top-right (duplicate for 2nd triangle)
            , 0.0, 1.0, 0.0, 1.0  -- Top-left
            , 0.0, 0.0, 0.0, 0.0  -- Bottom-left (duplicate)
            ] ∷ VS.Vector Float
        
        vertSize = fromIntegral $ VS.length quadVertices * sizeOf (0 ∷ Float)
    
    -- Create staging buffer
    (stagingMem, stagingBuff) ← createVulkanBufferManual device pDevice vertSize
        BUFFER_USAGE_TRANSFER_SRC_BIT
        (MEMORY_PROPERTY_HOST_VISIBLE_BIT .|. MEMORY_PROPERTY_HOST_COHERENT_BIT)
    
    -- Upload data to staging buffer
    dataPtr ← mapMemory device stagingMem 0 vertSize zero
    liftIO $ VS.unsafeWith quadVertices $ \srcPtr →
        copyBytes (castPtr dataPtr) srcPtr (fromIntegral vertSize)
    unmapMemory device stagingMem
    
    -- Create device-local vertex buffer
    (vertexMem, vertexBuff) ← createVulkanBuffer device pDevice vertSize
        (BUFFER_USAGE_VERTEX_BUFFER_BIT .|. BUFFER_USAGE_TRANSFER_DST_BIT)
        MEMORY_PROPERTY_DEVICE_LOCAL_BIT
    
    -- Copy from staging to vertex buffer
    copyBuffer device cmdPool queue stagingBuff vertexBuff vertSize
    
    -- Destroy staging buffer (no longer needed)
    destroyBuffer device stagingBuff Nothing
    freeMemory device stagingMem Nothing
    
    return (vertexBuff, vertexMem)

-------------------------------------------------------------
-- helper to create font texture descriptor set layout
-------------------------------------------------------------
-- | Create font-specific texture descriptor set layout (1 sampler, not 8)
createFontTextureLayout ∷ Device → EngineM ε σ DescriptorSetLayout
createFontTextureLayout device = do
    let binding = zero
          { binding = 0
          , descriptorType = DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER
          , descriptorCount = 1
          , stageFlags = SHADER_STAGE_FRAGMENT_BIT
          , immutableSamplers = V.empty
          }
        layoutInfo = zero { bindings = V.singleton binding }
    
    createDescriptorSetLayout device layoutInfo Nothing

-- | Create font UI rendering pipeline (uses UI camera)
createFontUIPipeline ∷ Device → RenderPass → Extent2D → DescriptorSetLayout 
                     → DescriptorSetLayout  -- ^ Font texture layout (reuse from world pipeline)
                     → EngineM ε σ (Pipeline, PipelineLayout)
createFontUIPipeline device renderPass swapExtent uniformLayout fontTexLayout = do
    let Extent2D w h = swapExtent
        pipelineLayoutInfo = zero
          { setLayouts = V.fromList [uniformLayout, fontTexLayout]
          , pushConstantRanges = V.empty
          }
    
    pipelineLayout ← createPipelineLayout device pipelineLayoutInfo Nothing
    
    -- Create shader modules
    vertModule ← createShaderModule device zero { code = fontUIVertexShaderCode } Nothing
    fragModule ← createShaderModule device zero { code = fontFragmentShaderCode } Nothing
    
    let vertStageInfo = zero 
          { stage   = SHADER_STAGE_VERTEX_BIT
          , module' = vertModule
          , name    = "main"
          }
        fragStageInfo = zero 
          { stage   = SHADER_STAGE_FRAGMENT_BIT
          , module' = fragModule
          , name    = "main"
          }
        shaderStages = V.fromList [SomeStruct vertStageInfo, SomeStruct fragStageInfo]
    
        vertexBindings = V.fromList
            [ zero  -- Binding 0: per-vertex (quad template)
              { binding = 0
              , stride = 16  -- vec2 pos + vec2 uv
              , inputRate = VERTEX_INPUT_RATE_VERTEX
              }
            , zero  -- Binding 1: per-instance (glyph data)
              { binding = 1
              , stride = 48  -- GlyphInstance size
              , inputRate = VERTEX_INPUT_RATE_INSTANCE
              }
            ]
    
        vertexAttributes = V.fromList
            [ -- Per-vertex (quad template)
              zero { location = 0, binding = 0, format = FORMAT_R32G32_SFLOAT, offset = 0 }
            , zero { location = 1, binding = 0, format = FORMAT_R32G32_SFLOAT, offset = 8 }
            , -- Per-instance (glyph data)
              zero { location = 2, binding = 1, format = FORMAT_R32G32_SFLOAT, offset = 0 }
            , zero { location = 3, binding = 1, format = FORMAT_R32G32_SFLOAT, offset = 8 }
            , zero { location = 4, binding = 1, format = FORMAT_R32G32B32A32_SFLOAT, offset = 16 }
            , zero { location = 5, binding = 1, format = FORMAT_R32G32B32A32_SFLOAT, offset = 32 }
            ]
    
        vertexInputInfo = zero
          { vertexBindingDescriptions = vertexBindings
          , vertexAttributeDescriptions = vertexAttributes
          }
    
        inputAssembly = zero
          { topology = PRIMITIVE_TOPOLOGY_TRIANGLE_LIST
          , primitiveRestartEnable = False
          }
    
        viewport = zero
          { x = 0, y = 0
          , width = fromIntegral w
          , height = fromIntegral h
          , minDepth = 0, maxDepth = 1
          }
    
        scissor = (zero ∷ Rect2D)
          { offset = Offset2D 0 0
          , extent = swapExtent
          }
    
        viewportState = zero
          { viewports = V.singleton viewport
          , scissors = V.singleton scissor
          }
    
        rasterizer = (zero ∷ PipelineRasterizationStateCreateInfo '[])
          { depthClampEnable = False
          , rasterizerDiscardEnable = False
          , polygonMode = POLYGON_MODE_FILL
          , lineWidth = 1
          , cullMode = CULL_MODE_NONE
          , frontFace = FRONT_FACE_COUNTER_CLOCKWISE
          , depthBiasEnable = False
          }
    
        multisampling = (zero ∷ PipelineMultisampleStateCreateInfo '[])
          { sampleShadingEnable = False
          , rasterizationSamples = SAMPLE_COUNT_1_BIT
          }
    
        colorBlendAttachment = zero
          { colorWriteMask = COLOR_COMPONENT_R_BIT .|. COLOR_COMPONENT_G_BIT 
                            .|. COLOR_COMPONENT_B_BIT .|. COLOR_COMPONENT_A_BIT
          , blendEnable = True
          , srcColorBlendFactor = BLEND_FACTOR_SRC_ALPHA
          , dstColorBlendFactor = BLEND_FACTOR_ONE_MINUS_SRC_ALPHA
          , colorBlendOp = BLEND_OP_ADD
          , srcAlphaBlendFactor = BLEND_FACTOR_ONE
          , dstAlphaBlendFactor = BLEND_FACTOR_ZERO
          , alphaBlendOp = BLEND_OP_ADD
          }
    
        colorBlending = (zero ∷ PipelineColorBlendStateCreateInfo '[])
          { logicOpEnable = False
          , attachments = V.singleton colorBlendAttachment
          , blendConstants = (0, 0, 0, 0)
          }
    
        pipelineInfo = (zero ∷ GraphicsPipelineCreateInfo '[])
          { stages = shaderStages
          , vertexInputState = Just $ SomeStruct vertexInputInfo
          , inputAssemblyState = Just inputAssembly
          , viewportState = Just $ SomeStruct viewportState
          , rasterizationState = Just $ SomeStruct rasterizer
          , multisampleState = Just $ SomeStruct multisampling
          , colorBlendState = Just $ SomeStruct colorBlending
          , layout = pipelineLayout
          , renderPass = renderPass
          , subpass = 0
          , basePipelineHandle = zero
          , basePipelineIndex = -1
          }
    
    (_, pipelinesVec) ← createGraphicsPipelines 
        device zero (V.singleton $ SomeStruct pipelineInfo) Nothing
    
    let !pipeline = V.head pipelinesVec
    
    destroyShaderModule device vertModule Nothing
    destroyShaderModule device fragModule Nothing
    
    pure (pipeline, pipelineLayout)

-- | Layout text into glyph instances for UI (pixel coordinates, no NDC conversion)
layoutTextUI ∷ FontAtlas → Float → Float → Text → (Float, Float, Float, Float) → V.Vector GlyphInstance
layoutTextUI atlas startX startY text color =
    let chars = T.unpack text
        (_, instances) = foldl layoutChar (startX, []) chars
    in V.fromList (reverse instances)
  where
    layoutChar (currentX, acc) char =
        case Map.lookup char (faGlyphData atlas) of
            Nothing → (currentX, acc)  -- Skip unknown character
            Just glyphInfo →
                let (bearingX, bearingY) = giBearing glyphInfo
                    (w, h) = giSize glyphInfo
                    (u0, v0, u1, v1) = giUVRect glyphInfo
                    -- Position glyph in pixels (no NDC conversion)
                    pxX = currentX + bearingX
                    pxY = startY + bearingY
                    
                    instance' = GlyphInstance
                        { instancePosition = (pxX, pxY)
                        , instanceSize = (w, h)
                        , instanceUVRect = (u0, v0, u1, v1)
                        , instanceColor = color }
                    
                    nextX = currentX + giAdvance glyphInfo
                in (nextX, instance' : acc)
