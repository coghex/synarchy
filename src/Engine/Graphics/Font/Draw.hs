-- src/Engine/Graphics/Font/Draw.hs
{-# LANGUAGE Strict #-}
module Engine.Graphics.Font.Draw
    ( createFontPipeline
    , createFontQuadBuffer
    , drawText
    , layoutText
    , renderTextBatches
    ) where

import UPrelude
import Engine.Graphics.Font.Data
import Engine.Asset.Types
import Engine.Core.Monad
import Engine.Core.State
import Engine.Core.Resource
import Engine.Core.Error.Exception
import Engine.Graphics.Types
import Engine.Graphics.Vulkan.Buffer (createVulkanBuffer, copyBuffer)
import Engine.Graphics.Vulkan.Command
import Engine.Graphics.Vulkan.ShaderCode (fontVertexShaderCode, fontFragmentShaderCode)
import Engine.Graphics.Vulkan.Types.Descriptor
import qualified Data.Map.Strict as Map
import qualified Data.Vector as V
import qualified Data.Vector.Storable as VS
import qualified Data.Text as T
import Foreign.Storable (Storable(..))
import Foreign.Ptr (plusPtr, castPtr)
import Foreign.Marshal.Array (pokeArray)
import Foreign.Marshal.Utils (copyBytes)
import Vulkan.Core10
import Vulkan.Zero
import Vulkan.CStruct.Extends

-----------------------------------------------------------
-- Storable Instance for GlyphInstance
-----------------------------------------------------------

instance Storable GlyphInstance where
    sizeOf _ = 48  -- 2*4 + 2*4 + 4*4 + 4*4 bytes
    alignment _ = 4
    
    peek ptr = do
        -- Read position (2 floats)
        px ← peekByteOff ptr 0
        py ← peekByteOff ptr 4
        
        -- Read size (2 floats)
        sw ← peekByteOff ptr 8
        sh ← peekByteOff ptr 12
        
        -- Read UV (4 floats)
        u0 ← peekByteOff ptr 16
        v0 ← peekByteOff ptr 20
        u1 ← peekByteOff ptr 24
        v1 ← peekByteOff ptr 28
        
        -- Read color (4 floats)
        r ← peekByteOff ptr 32
        g ← peekByteOff ptr 36
        b ← peekByteOff ptr 40
        a ← peekByteOff ptr 44
        
        return $ GlyphInstance (px, py) (sw, sh) (u0, v0, u1, v1) (r, g, b, a)
    
    poke ptr (GlyphInstance (px, py) (sw, sh) (u0, v0, u1, v1) (r, g, b, a)) = do
        -- Write position
        pokeByteOff ptr 0 px
        pokeByteOff ptr 4 py
        
        -- Write size
        pokeByteOff ptr 8 sw
        pokeByteOff ptr 12 sh
        
        -- Write UV
        pokeByteOff ptr 16 u0
        pokeByteOff ptr 20 v0
        pokeByteOff ptr 24 u1
        pokeByteOff ptr 28 v1
        
        -- Write color
        pokeByteOff ptr 32 r
        pokeByteOff ptr 36 g
        pokeByteOff ptr 40 b
        pokeByteOff ptr 44 a

-----------------------------------------------------------
-- Text Rendering API
-----------------------------------------------------------

-- | Draw text at position (Lua-facing API)
drawText ∷ Float → Float → FontHandle → Text → EngineM ε σ ()
drawText x y fontHandle text = do
    gs ← gets graphicsState
    let cache = fontCache gs
    case Map.lookup fontHandle (fcFonts cache) of
        Nothing → throwGraphicsError FontError $ "Invalid font handle: " <> T.pack (show fontHandle)
        Just atlas → do
            let instances = layoutText atlas x y text (1.0, 1.0, 1.0, 1.0)  -- White
            addTextBatch fontHandle instances

-- | Layout text into glyph instances
layoutText ∷ FontAtlas → Float → Float → Text → (Float, Float, Float, Float) → V.Vector GlyphInstance
layoutText atlas startX startY text color =
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
                    
                    -- Position glyph (baseline-aligned)
                    x = currentX + bearingX
                    y = startY - bearingY
                    
                    instance' = GlyphInstance
                        { instancePosition = (x, y)
                        , instanceSize = (w, h)
                        , instanceUVRect = (u0, v0, u1, v1)
                        , instanceColor = color }
                    
                    nextX = currentX + giAdvance glyphInfo
                in (nextX, instance' : acc)

-- | Add text batch to render queue
addTextBatch ∷ FontHandle → V.Vector GlyphInstance → EngineM ε σ ()
addTextBatch fontHandle instances = do
    let batch = TextBatch fontHandle instances
    modify $ \s → s 
        { graphicsState = (graphicsState s) 
            { textBatchQueue = V.snoc (textBatchQueue $ graphicsState s) batch 
            }
        }

-----------------------------------------------------------
-- Pipeline Creation
-----------------------------------------------------------

-- | Create font rendering pipeline with instancing
createFontPipeline ∷ Device → RenderPass → Extent2D → DescriptorSetLayout 
                   → EngineM ε σ (Pipeline, PipelineLayout)
createFontPipeline device renderPass swapExtent uniformLayout = do
    (pipeline, pipelineLayout) ← locally $ do
      -- Create shader modules
      vertModule ← allocResource 
          (\sm → destroyShaderModule device sm Nothing) $
          createShaderModule device zero { code = fontVertexShaderCode } Nothing
          
      fragModule ← allocResource 
          (\sm → destroyShaderModule device sm Nothing) $
          createShaderModule device zero { code = fontFragmentShaderCode } Nothing
      
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
      
      -- Get texture layout
      state ← gets graphicsState
      let texArrays = textureArrayStates state
      case Map.lookup "default" texArrays of
          Nothing → throwGraphicsError DescriptorError "No default texture array"
          Just defaultArray → do
              let texLayout = tasDescriptorSetLayout defaultArray
                  Extent2D w h = swapExtent
          
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
          
                  pipelineLayoutInfo = zero
                    { setLayouts = V.fromList [uniformLayout, texLayout]
                    , pushConstantRanges = V.empty
                    }
      
              -- Create pipeline layout
              pipelineLayout ← allocResource 
                  (\layout → destroyPipelineLayout device layout Nothing) $
                  createPipelineLayout device pipelineLayoutInfo Nothing
      
              -- Create pipeline
              let pipelineInfo = (zero ∷ GraphicsPipelineCreateInfo '[])
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
      
              (result, pipelinesVec) ← createGraphicsPipelines 
                  device zero (V.singleton $ SomeStruct pipelineInfo) Nothing
      
              let pipeline = V.head pipelinesVec
      
              _ ← allocResource 
                  (\p → destroyPipeline device p Nothing)
                  (pure pipeline)
      
              pure (pipeline, pipelineLayout)
    pure (pipeline, pipelineLayout)

-----------------------------------------------------------
-- Quad Buffer Creation
-----------------------------------------------------------

-- | Create shared quad buffer for all text rendering
createFontQuadBuffer ∷ Device → PhysicalDevice → Queue → CommandPool 
                     → EngineM ε σ (Buffer, DeviceMemory)
createFontQuadBuffer device pDevice queue cmdPool = do
    -- Simple quad vertices [0,0] to [1,1] (will be scaled per-instance)
    let quadVertices = VS.fromList
            [ 0.0, 0.0, 0.0, 0.0  -- Bottom-left:  pos, uv
            , 1.0, 0.0, 1.0, 0.0  -- Bottom-right
            , 1.0, 1.0, 1.0, 1.0  -- Top-right
            , 1.0, 1.0, 1.0, 1.0  -- Top-right (duplicate for 2nd triangle)
            , 0.0, 1.0, 0.0, 1.0  -- Top-left
            , 0.0, 0.0, 0.0, 0.0  -- Bottom-left (duplicate)
            ] ∷ VS.Vector Float
        
        vertSize = fromIntegral $ VS.length quadVertices * sizeOf (0 ∷ Float)
    
    (vertBuff, vertMem) ← locally $ do
      (stagingMem, stagingBuff) ← createVulkanBuffer device pDevice vertSize
          BUFFER_USAGE_TRANSFER_SRC_BIT
          (MEMORY_PROPERTY_HOST_VISIBLE_BIT .|. MEMORY_PROPERTY_HOST_COHERENT_BIT)
      dataPtr ← mapMemory device stagingMem 0 vertSize zero
      liftIO $ VS.unsafeWith quadVertices $ \srcPtr →
          copyBytes (castPtr dataPtr) srcPtr (fromIntegral vertSize)
      unmapMemory device stagingMem
      (vertexMem, vertexBuff) ← createVulkanBuffer device pDevice vertSize
          (BUFFER_USAGE_VERTEX_BUFFER_BIT .|. BUFFER_USAGE_TRANSFER_DST_BIT)
          MEMORY_PROPERTY_DEVICE_LOCAL_BIT
      copyBuffer device cmdPool queue stagingBuff vertexBuff vertSize
      return (vertexBuff, vertexMem)
    
    return (vertBuff, vertMem)

-----------------------------------------------------------
-- Rendering
-----------------------------------------------------------

-- | Render all queued text batches (called from draw loop)
renderTextBatches ∷ CommandBuffer → EngineM ε σ ()
renderTextBatches cmdBuffer = do
    state ← gets graphicsState
    let batches = V.toList (textBatchQueue state)
    
    unless (null batches) $ do
        -- Bind font pipeline
        case fontPipeline state of
            Nothing → throwGraphicsError FontError "Font pipeline not initialized"
            Just (pipeline, layout) → do
                cmdBindPipeline cmdBuffer PIPELINE_BIND_POINT_GRAPHICS pipeline
                
                -- Bind quad buffer
                case fontQuadBuffer state of
                    Nothing → throwGraphicsError FontError "Font quad buffer not initialized"
                    Just (quadBuffer, _) → do
                        -- Render each batch
                        forM_ batches $ \batch → renderBatch cmdBuffer quadBuffer layout batch
    
    -- Clear queue
    modify $ \s → s 
        { graphicsState = (graphicsState s) { textBatchQueue = V.empty }
        }

-- | Render a single text batch
renderBatch ∷ CommandBuffer → Buffer → PipelineLayout → TextBatch → EngineM ε σ ()
renderBatch cmdBuffer quadBuffer layout batch = do
    state ← gets graphicsState
    device ← case vulkanDevice state of
        Nothing → throwGraphicsError VulkanDeviceLost "No device"
        Just d → pure d
    pDevice ← case vulkanPDevice state of
        Nothing → throwGraphicsError VulkanDeviceLost "No physical device"
        Just pd → pure pd
    
    let instances = tbInstances batch
    unless (V.null instances) $ do
        -- Create instance buffer
        instanceBuffer ← createInstanceBuffer device pDevice instances
        
        -- Bind vertex buffers (quad + instances)
        cmdBindVertexBuffers cmdBuffer 0 
            (V.fromList [quadBuffer, instanceBuffer])
            (V.fromList [0, 0])
        
        -- Bind font atlas descriptor set
        let cache = fontCache state
        case Map.lookup (tbFontHandle batch) (fcFonts cache) of
            Nothing → throwGraphicsError FontError "Invalid font handle in batch"
            Just atlas → do
                case faDescriptorSet atlas of
                    Nothing → throwGraphicsError FontError "Font atlas has no descriptor set"
                    Just descSet → do
                        -- Bind descriptor sets (uniform at set 0, texture at set 1)
                        descState ← gets (descriptorState . graphicsState)
                        case descState of
                            Just manager → do
                                let uniformSet = V.head (dmActiveSets manager)
                                cmdBindDescriptorSets cmdBuffer 
                                    PIPELINE_BIND_POINT_GRAPHICS 
                                    layout 
                                    0  -- First set
                                    (V.fromList [uniformSet, descSet])
                                    V.empty
                            Nothing → throwGraphicsError FontError "No descriptor manager"
                        
                        -- Draw instanced
                        let instanceCount = fromIntegral $ V.length instances
                        cmdDraw cmdBuffer 6 instanceCount 0 0  -- 6 vertices, N instances
        
        -- Cleanup instance buffer
        destroyBuffer device instanceBuffer Nothing

-- | Create instance buffer for glyphs
createInstanceBuffer ∷ Device → PhysicalDevice → V.Vector GlyphInstance → EngineM ε σ Buffer
createInstanceBuffer device pDevice instances = do
    let instanceSize = fromIntegral $ sizeOf (undefined ∷ GlyphInstance)
        bufferSize = instanceSize * fromIntegral (V.length instances)
    
    -- Create buffer
    (memory, buffer) ← createVulkanBuffer device pDevice bufferSize
        BUFFER_USAGE_VERTEX_BUFFER_BIT
        (MEMORY_PROPERTY_HOST_VISIBLE_BIT .|. MEMORY_PROPERTY_HOST_COHERENT_BIT)
    
    -- Upload instance data
    dataPtr ← mapMemory device memory 0 bufferSize zero
    liftIO $ V.imapM_ (\i inst → pokeElemOff (castPtr dataPtr) i inst) instances
    unmapMemory device memory
    
    return buffer
