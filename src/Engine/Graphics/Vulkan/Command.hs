{-# LANGUAGE BangPatterns #-}
-- src/Engine/Graphics/Vulkan/Command.hs
module Engine.Graphics.Vulkan.Command
  ( createVulkanCommandPool
  , createVulkanCommandCollection
  , recordSceneCommandBuffer
  , allocateVulkanCommandBuffer
  , allocateVulkanCommandBuffers
  , destroyVulkanCommandPool
  , beginVulkanCommandBuffer
  , endVulkanCommandBuffer
  , runCommandsOnce
  , submitToQueue
  , createFrameResources
  , VulkanCommandCollection(..)
  ) where

import UPrelude
import Control.Monad.IO.Class (MonadIO(..))
import qualified Data.Vector as V
import qualified Data.Map as Map
import Data.IORef (newIORef, readIORef, modifyIORef, IORef)
import Engine.Asset.Types
import Engine.Core.Types
import Engine.Core.Monad
import Engine.Core.State
import Engine.Core.Resource (allocResource, locally)
import Engine.Core.Error.Exception
import Engine.Graphics.Types
import Engine.Graphics.Vulkan.BufferUtils
import Engine.Graphics.Vulkan.Base
import Engine.Graphics.Vulkan.Types
import Engine.Graphics.Vulkan.Types.Descriptor
import Engine.Graphics.Vulkan.Types.Texture
import Engine.Graphics.Vulkan.Texture.Types (BindlessTextureSystem(..))
import Engine.Graphics.Font.Data
import Engine.Scene.Base
import Engine.Scene.Types
import Foreign.Storable (sizeOf, pokeElemOff)
import Foreign.Ptr (castPtr)
import Vulkan.CStruct.Extends
import Vulkan.Core10
import Vulkan.Core10.CommandBufferBuilding
         (ClearValue(..), ClearColorValue(..))
import Vulkan.Zero

-- | Layer ID threshold to separate world and UI layers
uiLayerThreshold ∷ LayerId
uiLayerThreshold = LayerId 10

-- | Collection of command buffers and their pool
data VulkanCommandCollection = VulkanCommandCollection
  { vccCommandPool    ∷ CommandPool
  , vccCommandBuffers ∷ V.Vector CommandBuffer
  }

-- | Create a Vulkan command pool for allocating command buffers
createVulkanCommandPool ∷ Device → DevQueues → EngineM ε σ CommandPool
createVulkanCommandPool device queues = do
  let poolInfo = zero
        { flags = COMMAND_POOL_CREATE_RESET_COMMAND_BUFFER_BIT
        , queueFamilyIndex = graphicsFamIdx queues
        } ∷ CommandPoolCreateInfo
  
  allocResource (\pool → destroyCommandPool device pool Nothing) $
    createCommandPool device poolInfo Nothing

-- | Allocate Vulkan command buffers from the pool
allocateVulkanCommandBuffers ∷ Device 
                            → CommandPool 
                            → Int  -- ^ Number of buffers to allocate
                            → EngineM ε σ (V.Vector CommandBuffer)
allocateVulkanCommandBuffers device cmdPool numBuffers = do
  let allocInfo = zero
        { commandPool = cmdPool
        , level = COMMAND_BUFFER_LEVEL_PRIMARY
        , commandBufferCount = fromIntegral numBuffers
        }
  
  allocResource (\cb0 → freeCommandBuffers device cmdPool cb0)
                $ allocateCommandBuffers device allocInfo

-- | Create a complete Vulkan command buffer collection
createVulkanCommandCollection ∷ Device 
                             → DevQueues 
                             → Int  -- ^ Number of buffers
                             → EngineM ε σ VulkanCommandCollection
createVulkanCommandCollection device queues numBuffers = do
  pool ← createVulkanCommandPool device queues
  buffers ← allocateVulkanCommandBuffers device pool numBuffers
  
  pure $ VulkanCommandCollection
    { vccCommandPool = pool
    , vccCommandBuffers = buffers
    }

-- | Cleanup Vulkan command pool resources
destroyVulkanCommandPool ∷ Device → CommandPool → EngineM ε σ ()
destroyVulkanCommandPool device pool =
  liftIO $ destroyCommandPool device pool Nothing

-- | Begin recording a command buffer
beginVulkanCommandBuffer ∷ CommandBuffer → EngineM ε σ ()
beginVulkanCommandBuffer cmdBuf = do
  let beginInfo = zero 
        { flags = COMMAND_BUFFER_USAGE_ONE_TIME_SUBMIT_BIT
        , inheritanceInfo = Nothing
        } ∷ CommandBufferBeginInfo '[]
  
  liftIO $ beginCommandBuffer cmdBuf beginInfo

-- | End recording a command buffer
endVulkanCommandBuffer ∷ CommandBuffer → EngineM ε σ ()
endVulkanCommandBuffer cmdBuf = 
  liftIO $ endCommandBuffer cmdBuf

-- | Submit a command buffer to a queue and wait for it to complete
submitToQueue ∷ Device → Queue → CommandBuffer → EngineM ε σ ()
submitToQueue device queue cmdBuf = locally $ do
  fence ← allocResource (\f → destroyFence device f Nothing) $
    createFence device zero Nothing
  
  let submitInfo = zero
        { waitSemaphores = V.empty
        , commandBuffers = V.singleton (commandBufferHandle cmdBuf)
        , signalSemaphores = V.empty
        }
  
  queueSubmit queue (V.singleton (SomeStruct submitInfo)) fence
  
  -- Wait for the fence with timeout
  waitForFences device (V.singleton fence) True maxBound
  pure ()

-- | Run commands once and wait for completion
runCommandsOnce ∷ Device → CommandPool → Queue
  → (CommandBuffer → EngineM ε σ α) → EngineM ε σ α
runCommandsOnce device commandPool cmdQueue action = do
  -- Allocate command buffer
  let allocInfo = zero 
        { level = COMMAND_BUFFER_LEVEL_PRIMARY
        , commandPool = commandPool
        , commandBufferCount = 1 
        }
  buffer' ← allocResource (freeCommandBuffers device commandPool)
    $ allocateCommandBuffers device allocInfo
  let buffer = V.head buffer'
      beginInfo = zero 
        { flags = COMMAND_BUFFER_USAGE_ONE_TIME_SUBMIT_BIT
        , inheritanceInfo = Nothing 
        }
  
  -- Record commands
  beginCommandBuffer buffer beginInfo
  result ← action buffer
  endCommandBuffer buffer
  
  -- Submit and wait
  locally $ do
    fence ← allocResource (\f → destroyFence device f Nothing) $
      createFence device zero Nothing
    
    let submitInfo = zero
          { waitSemaphores = V.empty
          , waitDstStageMask = V.empty
          , commandBuffers = V.singleton (commandBufferHandle buffer)
          , signalSemaphores = V.empty 
          }
    
    queueSubmit cmdQueue (V.singleton (SomeStruct submitInfo)) fence
    waitForFences device (V.singleton fence) True maxBound
  
  pure result

-- | Create a set of frame resources
createFrameResources ∷ Device → DevQueues → EngineM ε σ FrameResources
createFrameResources device queues = do
    -- Create command pool for this frame
    let poolInfo = (zero ∷ CommandPoolCreateInfo)
          { flags = COMMAND_POOL_CREATE_RESET_COMMAND_BUFFER_BIT
          , queueFamilyIndex = graphicsFamIdx queues
          }
    
    cmdPool ← allocResource (\pool → destroyCommandPool device pool Nothing) $
        createCommandPool device poolInfo Nothing
    
    -- Create command buffer
    let allocInfo = zero
          { commandPool = cmdPool
          , level = COMMAND_BUFFER_LEVEL_PRIMARY
          , commandBufferCount = 2
          }
    
    cmdBuffers ← allocResource (freeCommandBuffers device cmdPool) $
        allocateCommandBuffers device allocInfo
    
    -- Create synchronization primitives
    imageAvailable ← allocResource (\s → destroySemaphore device s Nothing) $
        createSemaphore device zero Nothing
    
    renderFinished ← allocResource (\s → destroySemaphore device s Nothing) $
        createSemaphore device zero Nothing
    
    inFlight ← allocResource (\f → destroyFence device f Nothing) $
        createFence device (zero { flags = FENCE_CREATE_SIGNALED_BIT }) Nothing
    
    pure $ FrameResources
        { frCommandPool    = cmdPool
        , frCommandBuffer  = cmdBuffers
        , frImageAvailable = imageAvailable
        , frRenderFinished = renderFinished
        , frInFlight      = inFlight
        }

-- | Helper function to allocate a single command buffer
allocateVulkanCommandBuffer ∷ Device → CommandPool → EngineM ε σ CommandBuffer
allocateVulkanCommandBuffer device cmdPool = do
    let allocInfo = zero
          { commandPool = cmdPool
          , level = COMMAND_BUFFER_LEVEL_PRIMARY
          , commandBufferCount = 1
          }
    
    buffers ← allocResource (freeCommandBuffers device cmdPool) $
        allocateCommandBuffers device allocInfo
    
    case V.length buffers of
        0 → throwGraphicsError CommandBufferError
            "Failed to allocate command buffer"
        _ → pure $ V.head buffers

-- | Record scene command buffer with sprite and text batches
-- Supports both bindless and legacy texture paths
recordSceneCommandBuffer ∷ CommandBuffer → Word64 → SceneDynamicBuffer 
                         → Map.Map LayerId (V.Vector RenderItem)
                         → EngineM ε σ ()
recordSceneCommandBuffer cmdBuf frameIdx dynamicBuffer layeredBatches = do
    state ← gets graphicsState
    
    -- Validate required state components
    renderPass ← maybe (throwGraphicsError RenderPassError "Render pass not initialized")
                      pure
                      (vulkanRenderPass state)
    
    framebuffer ← maybe (throwGraphicsError FramebufferError "Framebuffer not initialized")
                       (\fbs → if frameIdx < fromIntegral (V.length fbs)
                              then pure (fbs V.! fromIntegral frameIdx)
                              else throwGraphicsError FramebufferError "Frame index out of bounds")
                       (framebuffers state)
    
    swapchainExtent ← maybe (throwGraphicsError SwapchainError "Swapchain info not initialized")
                           (pure . siSwapExtent)
                           (swapchainInfo state)
    
    device ← maybe (throwGraphicsError VulkanDeviceLost "No device")
                   pure
                   (vulkanDevice state)
    
    pDevice ← maybe (throwGraphicsError VulkanDeviceLost "No physical device")
                    pure
                    (vulkanPDevice state)

    -- Begin command buffer
    let beginInfo = (zero ∷ CommandBufferBeginInfo '[])
                      { flags = COMMAND_BUFFER_USAGE_ONE_TIME_SUBMIT_BIT }
    liftIO $ beginCommandBuffer cmdBuf beginInfo
    
    -- Begin render pass
    let clearColor = Color ( Float32 0.0 0.0 0.4 1.0 )
        renderPassInfo = zero
          { renderPass = renderPass
          , framebuffer = framebuffer
          , renderArea = Rect2D (Offset2D 0 0) swapchainExtent
          , clearValues = V.singleton clearColor
          }
    
    cmdBeginRenderPass cmdBuf renderPassInfo SUBPASS_CONTENTS_INLINE
    
    -- Set viewport and scissors
    let Extent2D w h = swapchainExtent
        viewport = Viewport
          { x = 0
          , y = 0
          , width = fromIntegral w
          , height = fromIntegral h
          , minDepth = 0
          , maxDepth = 1
          }
        scissor = Rect2D
          { offset = Offset2D 0 0
          , extent = swapchainExtent
          }
    
    cmdSetViewport cmdBuf 0 (V.singleton viewport)
    cmdSetScissor cmdBuf 0 (V.singleton scissor)
    
    -- Track vertex offset for sprite drawing
    vertexOffsetRef ← liftIO $ newIORef (0 ∷ Word32)
    
    -- Split layers into world and UI
    let (worldLayers, uiLayers) = Map.partitionWithKey 
                                    (\layerId _ → layerId < uiLayerThreshold) 
                                    layeredBatches
    
    -- Render world layers (< 10) with world pipelines
    forM_ (Map.toAscList worldLayers) $ \(layerId, items) → do
        renderLayerItems cmdBuf state viewport scissor dynamicBuffer 
                         items vertexOffsetRef device pDevice False
    
    -- Render UI layers (>= 10) with UI pipelines  
    forM_ (Map.toAscList uiLayers) $ \(layerId, items) → do
        renderLayerItems cmdBuf state viewport scissor dynamicBuffer 
                         items vertexOffsetRef device pDevice True

    -- End render pass and command buffer
    cmdEndRenderPass cmdBuf
    endVulkanCommandBuffer cmdBuf

-- | Render items in a single layer
-- isUI determines whether to use world or UI pipelines
renderLayerItems ∷ CommandBuffer → GraphicsState → Viewport → Rect2D
                 → SceneDynamicBuffer → V.Vector RenderItem
                 → IORef Word32 → Device → PhysicalDevice → Bool
                 → EngineM ε σ ()
renderLayerItems cmdBuf state viewport scissor dynamicBuffer items 
                 vertexOffsetRef device pDevice isUI = do
    -- Render sprites in this layer
    let spriteBatches = V.fromList [b | SpriteItem b ← V.toList items]
    
    if isUI
        then renderSpritesBindlessUI cmdBuf state viewport scissor 
                                     dynamicBuffer spriteBatches vertexOffsetRef
        else renderSpritesBindless cmdBuf state viewport scissor 
                                   dynamicBuffer spriteBatches vertexOffsetRef
    
    -- Render text in this layer
    let textItems = V.fromList [b | TextItem b ← V.toList items]
    
    unless (V.null textItems) $ do
        let maybePipeline = if isUI 
                            then fontUIPipeline state 
                            else fontPipeline state
        case (maybePipeline, fontQuadBuffer state) of
            (Just (pipeline, layout), Just (quadBuffer, _)) → do
                cmdBindPipeline cmdBuf PIPELINE_BIND_POINT_GRAPHICS pipeline
                cmdSetViewport cmdBuf 0 (V.singleton viewport)
                cmdSetScissor cmdBuf 0 (V.singleton scissor)
                
                descManager ← maybe (throwGraphicsError DescriptorError "No descriptor state") 
                                   pure 
                                   (descriptorState state)
                let uniformSet = V.head $ dmActiveSets descManager
                
                !newBuffers ← V.foldM' (\acc trb → do
                    let textBatch = TextBatch 
                          { tbFontHandle = trbFont trb
                          , tbInstances = trbInstances trb
                          , tbLayer = trbLayer trb
                          }
                    maybeBuffer ← renderTextBatchInline cmdBuf device pDevice 
                                                        quadBuffer layout textBatch state
                    case maybeBuffer of
                        Nothing → pure acc
                        Just !buf → pure $ V.snoc acc buf
                  ) V.empty textItems
                
                modify $ \s → s 
                    { graphicsState = (graphicsState s) 
                        { pendingInstanceBuffers = 
                            pendingInstanceBuffers (graphicsState s) <> newBuffers 
                        }
                    }
            _ → pure ()

-- | Render sprites using the bindless UI pipeline
renderSpritesBindlessUI ∷ CommandBuffer → GraphicsState → Viewport → Rect2D
                        → SceneDynamicBuffer → V.Vector RenderBatch 
                        → IORef Word32 → EngineM ε σ ()
renderSpritesBindlessUI cmdBuf state viewport scissor dynamicBuffer spriteBatches vertexOffsetRef = do
    -- Get bindless UI pipeline and texture system
    (pipeline, pipelineLayout) ← case bindlessUIPipeline state of
        Just p → pure p
        Nothing → throwGraphicsError PipelineError "Bindless UI pipeline not available"
    
    bindless ← case textureSystem state of
        Just b → pure b
        _ → throwGraphicsError DescriptorError "Bindless texture system not available"
    
    descManager ← maybe (throwGraphicsError DescriptorError "No descriptor state") 
                       pure 
                       (descriptorState state)
    
    -- Bind bindless UI pipeline
    cmdBindPipeline cmdBuf PIPELINE_BIND_POINT_GRAPHICS pipeline
    cmdSetViewport cmdBuf 0 (V.singleton viewport)
    cmdSetScissor cmdBuf 0 (V.singleton scissor)
    
    -- Bind descriptor sets: set 0 = uniforms, set 1 = bindless textures
    let uniformSet = V.head $ dmActiveSets descManager
        textureSet = btsDescriptorSet bindless
        descriptorSets = V.fromList [uniformSet, textureSet]
    
    cmdBindDescriptorSets cmdBuf 
        PIPELINE_BIND_POINT_GRAPHICS
        pipelineLayout
        0
        descriptorSets
        V.empty
    
    -- Bind vertex buffer
    cmdBindVertexBuffers cmdBuf 0
        (V.singleton (sdbBuffer dynamicBuffer))
        (V.singleton 0)
    
    -- Draw sprite batches
    V.forM_ spriteBatches $ \batch → do
        offset ← liftIO $ readIORef vertexOffsetRef
        let vertexCount = fromIntegral $ V.length $ rbVertices batch
        cmdDraw cmdBuf vertexCount 1 offset 0
        liftIO $ modifyIORef vertexOffsetRef (+ vertexCount)

-- | Render sprites using the bindless pipeline
renderSpritesBindless ∷ CommandBuffer → GraphicsState → Viewport → Rect2D
                      → SceneDynamicBuffer → V.Vector RenderBatch 
                      → IORef Word32 → EngineM ε σ ()
renderSpritesBindless cmdBuf state viewport scissor dynamicBuffer spriteBatches vertexOffsetRef = do
    -- Get bindless pipeline and texture system
    (pipeline, pipelineLayout) ← case bindlessPipeline state of
        Just p → pure p
        Nothing → throwGraphicsError PipelineError "Bindless pipeline not available"
    
    bindless ← case textureSystem state of
        Just b → pure b
        _ → throwGraphicsError DescriptorError "Bindless texture system not available"
    
    descManager ← maybe (throwGraphicsError DescriptorError "No descriptor state") 
                       pure 
                       (descriptorState state)
    
    -- Bind bindless pipeline
    cmdBindPipeline cmdBuf PIPELINE_BIND_POINT_GRAPHICS pipeline
    cmdSetViewport cmdBuf 0 (V.singleton viewport)
    cmdSetScissor cmdBuf 0 (V.singleton scissor)
    
    -- Bind descriptor sets: set 0 = uniforms, set 1 = bindless textures
    let uniformSet = V.head $ dmActiveSets descManager
        textureSet = btsDescriptorSet bindless
        descriptorSets = V.fromList [uniformSet, textureSet]
    
    cmdBindDescriptorSets cmdBuf 
        PIPELINE_BIND_POINT_GRAPHICS
        pipelineLayout
        0
        descriptorSets
        V.empty
    
    -- Bind vertex buffer
    cmdBindVertexBuffers cmdBuf 0
        (V.singleton (sdbBuffer dynamicBuffer))
        (V.singleton 0)
    
    -- Draw sprite batches
    V.forM_ spriteBatches $ \batch → do
        offset ← liftIO $ readIORef vertexOffsetRef
        let vertexCount = fromIntegral $ V.length $ rbVertices batch
        cmdDraw cmdBuf vertexCount 1 offset 0
        liftIO $ modifyIORef vertexOffsetRef (+ vertexCount)

-- | Render a single text batch inline (helper for recordSceneCommandBuffer)
renderTextBatchInline ∷ CommandBuffer → Device → PhysicalDevice 
                      → Buffer → PipelineLayout → TextBatch → GraphicsState
                      → EngineM ε σ (Maybe (Buffer, DeviceMemory))
renderTextBatchInline cmdBuf device pDevice quadBuffer layout batch state = do
    let instances = tbInstances batch
    if V.null instances
        then pure Nothing
        else do
            -- Create instance buffer
            !instanceBuf@(!instanceBuffer, !instanceMemory) ← createTextInstanceBuffer device pDevice instances
            -- Bind vertex buffers (quad + instances)
            cmdBindVertexBuffers cmdBuf 0 
                (V.fromList [quadBuffer, instanceBuffer])
                (V.fromList [0, 0])
            -- Bind font atlas descriptor set
            env ← ask
            let cacheRef = fontCacheRef env
            cache ← liftIO $ readIORef cacheRef 
            case Map.lookup (tbFontHandle batch) (fcFonts cache) of
                Nothing → do
                    logDebug $ "Font handle not found:  " <> show (tbFontHandle batch)
                    pure Nothing
                Just atlas → do
                    case faDescriptorSet atlas of
                        Nothing → do
                            logDebug "Font atlas has no descriptor set"
                            pure Nothing
                        Just descSet → do
                            case descriptorState state of
                                Just manager → do
                                    let !uniformSet = V.head (dmActiveSets manager)
                                    cmdBindDescriptorSets cmdBuf 
                                        PIPELINE_BIND_POINT_GRAPHICS 
                                        layout 
                                        0
                                        (V.fromList [uniformSet, descSet])
                                        V.empty
                                    -- Draw instanced
                                    let !instanceCount = fromIntegral $ V.length instances
                                    cmdDraw cmdBuf 6 instanceCount 0 0
                                    let !result = Just instanceBuf
                                    pure result
                                Nothing → do
                                    logDebug "No descriptor manager"
                                    pure Nothing

-- | Create instance buffer for text glyphs
createTextInstanceBuffer ∷ Device → PhysicalDevice → V.Vector GlyphInstance 
                         → EngineM ε σ (Buffer, DeviceMemory)
createTextInstanceBuffer device pDevice instances = do
    let !instanceSize = fromIntegral $ sizeOf (undefined ∷ GlyphInstance)
        !instanceCount = V.length instances
        !bufferSize = fromIntegral $ instanceSize * instanceCount
    when (instanceSize /= 48) $
        logDebug $ "Warning: Unusual instance count, expected 48, got: " <> show instanceSize
    
    -- Create buffer
    (!memory, !buffer) ← createVulkanBufferManual device pDevice bufferSize
        BUFFER_USAGE_VERTEX_BUFFER_BIT
        (MEMORY_PROPERTY_HOST_VISIBLE_BIT .|. MEMORY_PROPERTY_HOST_COHERENT_BIT)
    
    -- Upload instance data
    !dataPtr ← mapMemory device memory 0 bufferSize zero
    liftIO $ V.imapM_ (\i inst → pokeElemOff (castPtr dataPtr) i inst) instances
    unmapMemory device memory
    return (buffer, memory)
