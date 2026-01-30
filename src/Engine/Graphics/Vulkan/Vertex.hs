{-# LANGUAGE Strict #-}
module Engine.Graphics.Vulkan.Vertex
    ( quadVertices
    , createVertexBuffer
    , getVertexBindingDescription
    , getVertexAttributeDescriptions
    ) where

import UPrelude
import qualified Data.Vector as V
import Engine.Core.Monad
import Engine.Core.Resource
import Engine.Graphics.Vulkan.Types
import Engine.Graphics.Vulkan.Buffer
import Engine.Graphics.Vulkan.BufferUtils
import Engine.Graphics.Vulkan.Command
import Engine.Graphics.Vulkan.Types.Vertex
import Vulkan.Core10
import Vulkan.Zero

-- Updated quad vertices to create two quads side by side with different atlas IDs
quadVertices ∷ [Vertex]
quadVertices =
    -- Left quad (atlas ID 0)
    [ Vertex (Vec2 (-1.5) (-0.5)) (Vec2 0 0) (Vec4 1 1 1 1) 0  -- Bottom left
    , Vertex (Vec2 (-0.5) (-0.5)) (Vec2 1 0) (Vec4 1 1 1 1) 0  -- Bottom right
    , Vertex (Vec2 (-0.5)   0.5)  (Vec2 1 1) (Vec4 1 1 1 1) 0  -- Top right
    , Vertex (Vec2 (-0.5)   0.5)  (Vec2 1 1) (Vec4 1 1 1 1) 0  -- Top right
    , Vertex (Vec2 (-1.5)   0.5)  (Vec2 0 1) (Vec4 1 1 1 1) 0  -- Top left
    , Vertex (Vec2 (-1.5) (-0.5)) (Vec2 0 0) (Vec4 1 1 1 1) 0  -- Bottom left
    -- Right quad (atlas ID 1)
    , Vertex (Vec2   0.5  (-0.5)) (Vec2 0 0) (Vec4 1 1 1 1) 1  -- Bottom left
    , Vertex (Vec2   1.5  (-0.5)) (Vec2 1 0) (Vec4 1 1 1 1) 1  -- Bottom right
    , Vertex (Vec2   1.5    0.5)  (Vec2 1 1) (Vec4 1 1 1 1) 1  -- Top right
    , Vertex (Vec2   1.5    0.5)  (Vec2 1 1) (Vec4 1 1 1 1) 1  -- Top right
    , Vertex (Vec2   0.5    0.5)  (Vec2 0 1) (Vec4 1 1 1 1) 1  -- Top left
    , Vertex (Vec2   0.5  (-0.5)) (Vec2 0 0) (Vec4 1 1 1 1) 1  -- Bottom left
    ]

-- | Create vertex buffer from vertices
createVertexBuffer ∷ Device 
                  → PhysicalDevice 
                  → Queue 
                  → CommandPool 
                  → EngineM ε σ (Buffer, DeviceMemory)
createVertexBuffer device pDevice graphicsQueue commandPool = do
    let vertices = quadVertices  -- Our predefined vertices
        bsize    = sizeOf (head vertices)
        vertSize = fromIntegral $ bsize * length vertices
    
    -- Create staging buffer
    (stagingMemory, stagingBuffer) ← createVulkanBufferManual
        device 
        pDevice 
        vertSize
        BUFFER_USAGE_TRANSFER_SRC_BIT
        (MEMORY_PROPERTY_HOST_VISIBLE_BIT .|. MEMORY_PROPERTY_HOST_COHERENT_BIT)

    -- Map memory and copy vertex data
    dataPtr ← mapMemory device stagingMemory 0 vertSize zero
    liftIO $ do
        let ptr = castPtr dataPtr
        forM_ (zip [0..] vertices) $ \(i, vertex) → do
            let offset = i * fromIntegral bsize
            poke (ptr `plusPtr` offset) vertex
    unmapMemory device stagingMemory

    -- Create vertex buffer
    (vertexMemory, vertexBuffer) ← createVulkanBuffer device pDevice vertSize
        (BUFFER_USAGE_TRANSFER_DST_BIT .|. BUFFER_USAGE_VERTEX_BUFFER_BIT)
        MEMORY_PROPERTY_DEVICE_LOCAL_BIT

    -- Copy from staging to vertex buffer, free staging buffer
    copyBuffer device commandPool graphicsQueue stagingBuffer vertexBuffer vertSize
    liftIO $ destroyBuffer device stagingBuffer Nothing
    liftIO $ freeMemory device stagingMemory Nothing

    pure (vertexBuffer, vertexMemory)

-- | Get vertex binding description for pipeline creation
getVertexBindingDescription ∷ VertexInputBindingDescription
getVertexBindingDescription = zero
    { binding = 0
    , stride = 36
    , inputRate = VERTEX_INPUT_RATE_VERTEX
    }

-- | Get vertex attribute descriptions for pipeline creation
getVertexAttributeDescriptions ∷ V.Vector VertexInputAttributeDescription
getVertexAttributeDescriptions = V.fromList
    [ zero  -- Position
        { location = 0
        , binding = 0
        , format = FORMAT_R32G32_SFLOAT
        , offset = 0
        }
    , zero  -- TexCoord
        { location = 1
        , binding = 0
        , format = FORMAT_R32G32_SFLOAT
        , offset = 8
        }
    , zero  -- Color
        { location = 2
        , binding = 0
        , format = FORMAT_R32G32B32A32_SFLOAT
        , offset = 16
        }
    , zero  -- Atlas ID
        { location = 3
        , binding = 0
        , format = FORMAT_R32_SFLOAT
        , offset = 32
        }
    ]
