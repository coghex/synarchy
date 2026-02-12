{-# LANGUAGE Strict, UnicodeSyntax #-}
module Engine.Scene.Render where

import UPrelude
import qualified Data.Vector as V
import qualified Data.Map as Map
import qualified Data.Text as T
import Data.IORef
import Engine.Core.Log (LogCategory(..))
import Engine.Core.Log.Monad (logAndThrowM, logDebugM, logDebugSM, logInfoM, logInfoSM)
import Engine.Core.Monad
import Engine.Core.State
import Engine.Core.Error.Exception (ExceptionType(..), GraphicsError(..))
import Engine.Scene.Base
import Engine.Scene.Types
import Engine.Scene.Manager
import Engine.Scene.Graph
import Engine.Scene.Batch
import Engine.Graphics.Types
import Engine.Graphics.Vulkan.Types.Vertex
import Engine.Graphics.Vulkan.Types
import Engine.Graphics.Vulkan.Buffer
import Engine.Graphics.Window.Types
import Engine.Graphics.Font.Data
import qualified Engine.Graphics.Window.GLFW as GLFW
import Engine.Graphics.Window.Types (Window(..))
import Vulkan.Core10
import Vulkan.Zero

-----------------------------------------------------------
-- Scene update
-----------------------------------------------------------

updateSceneForRender ∷ EngineM ε σ ()
updateSceneForRender ∷ EngineM ε σ ()
updateSceneForRender = do
    state ← gets graphicsState
    sceneMgr ← gets sceneManager
    
    logDebugSM CatRender "Updating scene"
        [("activeScene", maybe "none" (T.pack . show) (smActiveScene sceneMgr))]
    
    -- Get window dimensions for frustum culling
    let Window win = fromJust $ glfwWindow state
    (width, height) ← GLFW.getFramebufferSize win
    -- and the screen dimensions from the swapchain
    let (screenW, screenH) = case swapchainInfo state of
                               Nothing → (800.0,600.0)
                               Just swapInfo → let Extent2D w h = siSwapExtent swapInfo
                                               in (fromIntegral w, fromIntegral h)
    
    -- Update scene manager with current view dimensions
    updatedSceneMgr ← updateSceneManager 
                            (fromIntegral width) 
                            (fromIntegral height) 
                            sceneMgr
    
    -- collect text batches from active scene
    case smActiveScene updatedSceneMgr of
        Just sceneId → case Map.lookup sceneId (smSceneGraphs updatedSceneMgr) of
            Just graph → do
                let nodeCount = length $ sgNodes graph
                logDebugSM CatRender "Processing scene graph"
                    [("sceneId", T.pack $ show sceneId)
                    ,("nodes", T.pack $ show nodeCount)]
                
                let updatedGraph = updateWorldTransforms graph
                textRenderBatches ← collectTextBatches updatedGraph screenW screenH
                
                logDebugSM CatRender "Collected text batches"
                    [("textBatches", T.pack $ show $ V.length textRenderBatches)]
                
                let updatedBatchMgr = updateTextBatches textRenderBatches (smBatchManager updatedSceneMgr)
                    simpleBatches = convertToTextBatches textRenderBatches
                    finalBatchMgr = buildLayeredBatches updatedBatchMgr
                    spriteBatches = getCurrentBatches updatedSceneMgr
                    spriteCount = V.sum $ V.map (V.length . rbVertices) spriteBatches
                    drawCallCount = V.length spriteBatches + V.length textRenderBatches
                
                logDebugSM CatScene "Batch generation complete"
                    [("spriteBatches", T.pack $ show $ V.length spriteBatches)
                    ,("spriteVertices", T.pack $ show spriteCount)
                    ,("textBatches", T.pack $ show $ V.length textRenderBatches)
                    ,("totalDrawCalls", T.pack $ show drawCallCount)]
                
                let finalSceneMgr = updatedSceneMgr 
                                        { smSceneGraphs = Map.insert sceneId updatedGraph (smSceneGraphs updatedSceneMgr)
                                        , smBatchManager = finalBatchMgr }
                modify $ \s → s { sceneManager = finalSceneMgr }
            Nothing → do
                logDebugM CatScene "No scene graph found for active scene"
                modify $ \s → s { sceneManager = updatedSceneMgr }
        Nothing → do
            logDebugM CatScene "No active scene"
            modify $ \s → s { sceneManager = updatedSceneMgr }

-- | Get current render batches from scene
getCurrentRenderBatches = do
    sceneMgr ← gets sceneManager
    let batches = getCurrentBatches sceneMgr
    logDebugSM CatRender "Retrieved current batches"
        [("count", T.pack $ show $ V.length batches)]
    pure batches

-----------------------------------------------------------
-- Buffer management
-----------------------------------------------------------

ensureDynamicVertexBuffer ∷ Word64 → EngineM ε σ SceneDynamicBuffer
ensureDynamicVertexBuffer ∷ Word64 → EngineM ε σ SceneDynamicBuffer
ensureDynamicVertexBuffer requiredVertices = do
    state ← gets graphicsState
    device ← case vulkanDevice state of
        Nothing → logAndThrowM CatGraphics (ExGraphics VulkanDeviceLost)
                                           "No device"
        Just d → pure d
    pDevice ← case vulkanPDevice state of
        Nothing → logAndThrowM CatGraphics (ExGraphics VulkanDeviceLost)
                                           "No physical device"
        Just pd → pure pd
    
    -- Calculate required buffer size
    let bufferSize = requiredVertices * (fromIntegral vertexTotalSize)
        -- Add 50% padding for dynamic growth
        paddedSize = bufferSize + (bufferSize `div` 2)
    
    logDebugSM CatRender "Creating dynamic vertex buffer"
        [("vertices", T.pack $ show requiredVertices)
        ,("sizeBytes", T.pack $ show paddedSize)]
    
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
uploadBatchesToBuffer batches dynamicBuffer = do
    state ← gets graphicsState
    device ← case vulkanDevice state of
        Nothing → logAndThrowM CatGraphics (ExGraphics VulkanDeviceLost)
                                           "No device"
        Just d → pure d
    let totalVertices = V.sum $ V.map (fromIntegral . V.length . rbVertices) batches
    logDebugSM CatRender "Uploading batches to buffer"
        [("batches", T.pack $ show $ V.length batches)
        ,("totalVertices", T.pack $ show totalVertices)]
    finalBuffer ← if totalVertices > sdbCapacity dynamicBuffer
        then do
            logDebugM CatScene "Buffer too small, resizing..."
            ensureDynamicVertexBuffer totalVertices
        else pure dynamicBuffer
    let totalSize = totalVertices * (fromIntegral vertexTotalSize)
    dataPtr ← mapMemory device (sdbMemory finalBuffer) 0 totalSize zero
    currentOffset ← liftIO $ newIORef (0 ∷ Int)
    V.forM_ batches $ \batch → do
        offset ← liftIO $ readIORef currentOffset
        let vertices = V.toList $ rbVertices batch
            batchSize = length vertices * fromIntegral vertexTotalSize
        liftIO $ do
            let ptr = castPtr dataPtr `plusPtr` offset
            forM_ (zip [0..] vertices) $ \(i, vertex) → do
                pokeByteOff ptr (i * fromIntegral vertexTotalSize) vertex
            writeIORef currentOffset (offset + batchSize)
    unmapMemory device (sdbMemory finalBuffer)
    logDebugM CatRender "Buffer upload complete"
    pure $ finalBuffer { sdbUsed = totalVertices }

getWorldSceneQuads ∷ EngineM ε σ (V.Vector SortableQuad)
getWorldSceneQuads = do
    sceneMgr ← gets sceneManager
    let bm = smBatchManager sceneMgr
        worldObjs = V.filter (\obj → doLayer obj < LayerId 10) (bmVisibleObjs bm)
    pure $ V.map drawableToQuad worldObjs
