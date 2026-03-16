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

-- | Two side-by-side quads with different atlas IDs.
-- faceMapId = 0 means use the default (undefined/top-lit) face map.
quadVertices ∷ [Vertex]
quadVertices =
    [ Vertex (Vec2 (-1.5) (-0.5)) (Vec2 0 0) (Vec4 1 1 1 1) 0 0
    , Vertex (Vec2 (-0.5) (-0.5)) (Vec2 1 0) (Vec4 1 1 1 1) 0 0
    , Vertex (Vec2 (-0.5)   0.5)  (Vec2 1 1) (Vec4 1 1 1 1) 0 0
    , Vertex (Vec2 (-0.5)   0.5)  (Vec2 1 1) (Vec4 1 1 1 1) 0 0
    , Vertex (Vec2 (-1.5)   0.5)  (Vec2 0 1) (Vec4 1 1 1 1) 0 0
    , Vertex (Vec2 (-1.5) (-0.5)) (Vec2 0 0) (Vec4 1 1 1 1) 0 0
    , Vertex (Vec2   0.5  (-0.5)) (Vec2 0 0) (Vec4 1 1 1 1) 1 0
    , Vertex (Vec2   1.5  (-0.5)) (Vec2 1 0) (Vec4 1 1 1 1) 1 0
    , Vertex (Vec2   1.5    0.5)  (Vec2 1 1) (Vec4 1 1 1 1) 1 0
    , Vertex (Vec2   1.5    0.5)  (Vec2 1 1) (Vec4 1 1 1 1) 1 0
    , Vertex (Vec2   0.5    0.5)  (Vec2 0 1) (Vec4 1 1 1 1) 1 0
    , Vertex (Vec2   0.5  (-0.5)) (Vec2 0 0) (Vec4 1 1 1 1) 1 0
    ]

-- | Create vertex buffer from vertices
createVertexBuffer ∷ Device 
                  → PhysicalDevice 
                  → Queue 
                  → CommandPool 
                  → EngineM ε σ (Buffer, DeviceMemory)
createVertexBuffer device pDevice graphicsQueue commandPool = do
    let vertices = quadVertices  -- Our predefined vertices
        bsize    = case vertices of (v:_) → sizeOf v; [] → 0
        vertSize = fromIntegral $ bsize * length vertices
    
    (stagingMemory, stagingBuffer) ← createVulkanBufferManual
        device 
        pDevice 
        vertSize
        BUFFER_USAGE_TRANSFER_SRC_BIT
        (MEMORY_PROPERTY_HOST_VISIBLE_BIT .|. MEMORY_PROPERTY_HOST_COHERENT_BIT)

    dataPtr ← mapMemory device stagingMemory 0 vertSize zero
    liftIO $ do
        let ptr = castPtr dataPtr
        forM_ (zip [0..] vertices) $ \(i, vertex) → do
            let offset = i * fromIntegral bsize
            poke (ptr `plusPtr` offset) vertex
    unmapMemory device stagingMemory

    (vertexMemory, vertexBuffer) ← createVulkanBuffer device pDevice vertSize
        (BUFFER_USAGE_TRANSFER_DST_BIT .|. BUFFER_USAGE_VERTEX_BUFFER_BIT)
        MEMORY_PROPERTY_DEVICE_LOCAL_BIT

    copyBuffer device commandPool graphicsQueue stagingBuffer vertexBuffer vertSize
    liftIO $ destroyBuffer device stagingBuffer Nothing
    liftIO $ freeMemory device stagingMemory Nothing

    pure (vertexBuffer, vertexMemory)

-- | Get vertex binding description for pipeline creation
getVertexBindingDescription ∷ VertexInputBindingDescription
getVertexBindingDescription = zero
    { binding = 0
    , stride = 40  -- 2+2+4+1+1 floats = 40 bytes (includes faceMapId)
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
    , zero  -- ^ TexCoord
        { location = 1
        , binding = 0
        , format = FORMAT_R32G32_SFLOAT
        , offset = 8
        }
    , zero  -- ^ Color
        { location = 2
        , binding = 0
        , format = FORMAT_R32G32B32A32_SFLOAT
        , offset = 16
        }
    , zero  -- ^ Atlas ID (texture index)
        { location = 3
        , binding = 0
        , format = FORMAT_R32_SFLOAT
        , offset = 32
        }
    , zero  -- ^ Face Map ID
        { location = 4
        , binding = 0
        , format = FORMAT_R32_SFLOAT
        , offset = 36
        }
    ]
