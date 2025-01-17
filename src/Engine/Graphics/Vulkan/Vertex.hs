{-# LANGUAGE Strict #-}
module Engine.Graphics.Vulkan.Vertex
    ( Vec2(..)
    , Vec4(..)
    , Vertex(..)
    , quadVertices
    , createVertexBuffer
    , getVertexBindingDescription
    , getVertexAttributeDescriptions
    ) where

import UPrelude
import Control.Monad (forM_)
import qualified Data.Vector as V
import Foreign.Storable (Storable(..), sizeOf)
import qualified Foreign.Storable as Storable
import Foreign.Ptr (Ptr, castPtr, plusPtr)
import Foreign.Marshal.Array (pokeArray)
import Data.Bits ((.|.))

import Engine.Core.Monad
import Engine.Graphics.Vulkan.Types
import Engine.Graphics.Vulkan.Buffer
import Engine.Graphics.Vulkan.Command
import Vulkan.Core10
import Vulkan.Zero

-- | 2D vector for positions and texture coordinates
data Vec2 = Vec2 
    { x ∷ Float
    , y ∷ Float 
    } deriving (Show, Eq)

instance Storable Vec2 where
    sizeOf _ = 8  -- 2 * sizeof(Float) = 2 * 4 bytes
    alignment _ = 4  -- alignment of Float
    peek ptr = Vec2
        <$> peekByteOff ptr 0
        <*> peekByteOff ptr 4  -- use actual byte offset
    poke ptr (Vec2 x' y') = do
        pokeByteOff ptr 0 x'
        pokeByteOff ptr 4 y'

-- | 4D vector for colors (RGBA)
data Vec4 = Vec4 
    { r ∷ Float
    , g ∷ Float
    , b ∷ Float
    , a ∷ Float 
    } deriving (Show, Eq)

instance Storable Vec4 where
    sizeOf _ = 16  -- 4 * sizeof(Float) = 4 * 4 bytes
    alignment _ = 4  -- alignment of Float
    peek ptr = Vec4
        <$> peekByteOff ptr 0
        <*> peekByteOff ptr 4
        <*> peekByteOff ptr 8
        <*> peekByteOff ptr 12
    poke ptr (Vec4 r' g' b' a') = do
        pokeByteOff ptr 0 r'
        pokeByteOff ptr 4 g'
        pokeByteOff ptr 8 b'
        pokeByteOff ptr 12 a'

-- | Vertex structure matching shader input
data Vertex = Vertex
    { pos     ∷ Vec2  -- ^ Position (layout = 0)
    , tex     ∷ Vec2  -- ^ Texture coordinates (layout = 1)
    , color   ∷ Vec4  -- ^ Color (layout = 2)
    , atlasId ∷ Float -- ^ Atlas ID (layout = 3)
    } deriving (Show, Eq)

instance Storable Vertex where
    sizeOf _ = 36  -- 2 * sizeOf(Vec2) + sizeOf(Vec4) + sizeOf(Float)
    alignment _ = 4  -- alignment of Float
    peek ptr = Vertex
        <$> peek (castPtr ptr)
        <*> peek (ptr `plusPtr` 8)   -- after first Vec2
        <*> peek (ptr `plusPtr` 16)  -- after second Vec2
        <*> peek (ptr `plusPtr` 32)  -- after Vec4
    poke ptr (Vertex p t c a) = do
        poke (castPtr ptr) p
        poke (ptr `plusPtr` 8) t
        poke (ptr `plusPtr` 16) c
        poke (ptr `plusPtr` 32) a

-- Update quad vertices to include atlas ID (default to atlas 0)
quadVertices ∷ [Vertex]
quadVertices =
    [ Vertex (Vec2 (-0.5) (-0.5)) (Vec2 0 0) (Vec4 1 1 1 1) 0  -- Bottom left
    , Vertex (Vec2   0.5  (-0.5)) (Vec2 1 0) (Vec4 1 1 1 1) 0  -- Bottom right
    , Vertex (Vec2   0.5    0.5)  (Vec2 1 1) (Vec4 1 1 1 1) 0  -- Top right
    , Vertex (Vec2   0.5    0.5)  (Vec2 1 1) (Vec4 1 1 1 1) 0  -- Top right
    , Vertex (Vec2 (-0.5)   0.5)  (Vec2 0 1) (Vec4 1 1 1 1) 0  -- Top left
    , Vertex (Vec2 (-0.5) (-0.5)) (Vec2 0 0) (Vec4 1 1 1 1) 0  -- Bottom left
    ]

-- | Create vertex buffer from vertices
createVertexBuffer :: Device 
                  -> PhysicalDevice 
                  -> Queue 
                  -> CommandPool 
                  -> EngineM ε σ (Buffer, DeviceMemory)
createVertexBuffer device pDevice graphicsQueue commandPool = do
    let vertices = quadVertices  -- Our predefined vertices
        bsize    = sizeOf (head vertices)
        vertSize = fromIntegral $ bsize * length vertices
        --vertSize = fromIntegral $ sizeOf (undefined :: Vertex) * length vertices
    
    -- Create staging buffer
    (stagingMemory, stagingBuffer) <- createVulkanBuffer 
        device 
        pDevice 
        vertSize
        BUFFER_USAGE_TRANSFER_SRC_BIT
        (MEMORY_PROPERTY_HOST_VISIBLE_BIT .|. MEMORY_PROPERTY_HOST_COHERENT_BIT)

    -- Map memory and copy vertex data
    dataPtr <- mapMemory device stagingMemory 0 vertSize zero
    liftIO $ do
        let ptr = castPtr dataPtr
        forM_ (zip [0..] vertices) $ \(i, vertex) -> do
            let offset = i * fromIntegral bsize
            poke (ptr `plusPtr` offset) vertex
    unmapMemory device stagingMemory

    -- Create vertex buffer
    (vertexMemory, vertexBuffer) <- createVulkanBuffer 
        device 
        pDevice 
        vertSize
        (BUFFER_USAGE_TRANSFER_DST_BIT .|. BUFFER_USAGE_VERTEX_BUFFER_BIT)
        MEMORY_PROPERTY_DEVICE_LOCAL_BIT

    -- Copy from staging to vertex buffer
    copyBuffer device commandPool graphicsQueue stagingBuffer vertexBuffer vertSize

    pure (vertexBuffer, vertexMemory)

-- | Get vertex binding description for pipeline creation
getVertexBindingDescription :: VertexInputBindingDescription
getVertexBindingDescription = zero
    { binding = 0
    , stride = 36
    , inputRate = VERTEX_INPUT_RATE_VERTEX
    }

-- | Get vertex attribute descriptions for pipeline creation
getVertexAttributeDescriptions :: V.Vector VertexInputAttributeDescription
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
