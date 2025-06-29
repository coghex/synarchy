{-# LANGUAGE Strict #-}
module Engine.Scene.Render where

import UPrelude
import qualified Data.Vector as V
import qualified Data.Map as Map
import Data.IORef
import Engine.Core.Monad
import Engine.Core.State
import Engine.Core.Error.Exception
import Engine.Scene.Types
import Engine.Scene.Manager
import Engine.Graphics.Vulkan.Types.Vertex
import Engine.Graphics.Vulkan.Types
import Engine.Graphics.Vulkan.Buffer
import Engine.Graphics.Window.Types
import qualified Engine.Graphics.Window.GLFW as GLFW
import Engine.Graphics.Window.Types (Window(..))
import Vulkan.Core10
import Vulkan.Zero

-- | Update scene and prepare render data
updateSceneForRender ∷ EngineM ε σ ()
updateSceneForRender = do
    state ← gets graphicsState
    sceneMgr ← gets sceneManager
    
    -- Get window dimensions for frustum culling
    let Window win = fromJust $ glfwWindow state
    (width, height) ← GLFW.getFramebufferSize win
    
    -- Update scene manager with current view dimensions
    let updatedSceneMgr = updateSceneManager 
                            (fromIntegral width) 
                            (fromIntegral height) 
                            sceneMgr
    
    -- Store updated scene manager
    modify $ \s → s { sceneManager = updatedSceneMgr }

-- | Get current render batches from scene
getCurrentRenderBatches ∷ EngineM ε σ (V.Vector RenderBatch)
getCurrentRenderBatches = do
    sceneMgr ← gets sceneManager
    pure $ getCurrentBatches sceneMgr

-- | Create or resize dynamic vertex buffer for scene rendering
ensureDynamicVertexBuffer ∷ Word64 → EngineM ε σ SceneDynamicBuffer
ensureDynamicVertexBuffer requiredVertices = do
    state ← gets graphicsState
    device ← case vulkanDevice state of
        Nothing → throwGraphicsError VulkanDeviceLost "No device"
        Just d → pure d
    pDevice ← case vulkanPDevice state of
        Nothing → throwGraphicsError VulkanDeviceLost "No physical device"
        Just pd → pure pd
    
    -- Calculate required buffer size
    let bufferSize = requiredVertices * (fromIntegral vertexTotalSize)
        -- Add 50% padding for dynamic growth
        paddedSize = bufferSize + (bufferSize `div` 2)
    
    -- Create new buffer
    (memory, buffer) ← createVulkanBuffer 
        device 
        pDevice 
        paddedSize
        (BUFFER_USAGE_VERTEX_BUFFER_BIT .|. BUFFER_USAGE_TRANSFER_DST_BIT)
        (MEMORY_PROPERTY_HOST_VISIBLE_BIT .|. MEMORY_PROPERTY_HOST_COHERENT_BIT)
    
    pure $ SceneDynamicBuffer
        { sdbBuffer = buffer
        , sdbMemory = memory
        , sdbCapacity = requiredVertices + (requiredVertices `div` 2)
        , sdbUsed = 0
        }

-- | Upload batch vertices to dynamic buffer
uploadBatchesToBuffer ∷ V.Vector RenderBatch → SceneDynamicBuffer → EngineM ε σ SceneDynamicBuffer
uploadBatchesToBuffer batches dynamicBuffer = do
    state ← gets graphicsState
    device ← case vulkanDevice state of
        Nothing → throwGraphicsError VulkanDeviceLost "No device"
        Just d → pure d
    
    -- Calculate total vertices needed
    let totalVertices = V.sum $ V.map (fromIntegral . V.length . rbVertices) batches
    
    -- Ensure buffer capacity
    finalBuffer ← if totalVertices > sdbCapacity dynamicBuffer
        then ensureDynamicVertexBuffer totalVertices
        else pure dynamicBuffer
    
    -- Upload vertex data
    let totalSize = totalVertices * (fromIntegral vertexTotalSize)
    
    dataPtr ← mapMemory device (sdbMemory finalBuffer) 0 totalSize zero
    
    -- Copy all batch vertices sequentially
    currentOffset ← liftIO $ newIORef (0 ∷ Int)
    V.forM_ batches $ \batch → do
        offset ← liftIO $ readIORef currentOffset
        let vertices = V.toList $ rbVertices batch
            batchSize = length vertices * fromIntegral vertexTotalSize
        
        liftIO $ do
            let ptr = castPtr dataPtr `plusPtr` offset
            forM_ (zip [0..] vertices) $ \(i, vertex) → do
                let vertOffset = i * fromIntegral vertexTotalSize
                poke (ptr `plusPtr` vertOffset) vertex
            modifyIORef currentOffset (+ batchSize)
    
    unmapMemory device (sdbMemory finalBuffer)
    
    pure $ finalBuffer { sdbUsed = totalVertices }
