{-# LANGUAGE Strict #-}
module Engine.Scene.Render where

import UPrelude
import qualified Data.Vector as V
import qualified Data.Map as Map
import Data.IORef
import Engine.Core.Monad
import Engine.Core.State
import Engine.Core.Error.Exception
import Engine.Scene.Base
import Engine.Scene.Types
import Engine.Scene.Manager
import Engine.Scene.Graph
import Engine.Scene.Batch
import Engine.Graphics.Vulkan.Types.Vertex
import Engine.Graphics.Vulkan.Types
import Engine.Graphics.Vulkan.Buffer
import Engine.Graphics.Window.Types
import Engine.Graphics.Font.Data
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
    
    -- collect text batches from active scene
    case smActiveScene updatedSceneMgr of
        Just sceneId → case Map.lookup sceneId (smSceneGraphs updatedSceneMgr) of
            Just graph → do
                let updatedGraph = updateWorldTransforms graph
                let allNodes = Map.elems (sgNodes updatedGraph)
                    textNodes = filter (\n → nodeType n ≡ TextObject && nodeVisible n) allNodes
                logDebug $ "Text nodes in scene: " ⧺ (show (length textNodes))
                textRenderBatches ← collectTextBatches updatedGraph
                logDebug $ "Text render batches collected" ⧺ show (V.length textRenderBatches)
                let simpleBatches = convertToTextBatches textRenderBatches
                logDebug $ "simple text batches: " ⧺ show (V.length simpleBatches)
                forM_ simpleBatches $ \batch → do
                  logDebug $ "  Batch font=" ⧺ show (tbFontHandle batch) ⧺
                             " instances=" ⧺ show (V.length (tbInstances batch))
                modify $ \s → s { graphicsState = (graphicsState s) 
                                    { textBatchQueue = simpleBatches } }
            Nothing → logDebug "No active scene graph found"
        Nothing → logDebug "No active scene to collect text batches from"
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
