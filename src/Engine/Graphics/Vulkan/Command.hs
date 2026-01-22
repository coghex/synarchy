{-# LANGUAGE BangPatterns #-}
-- src/Engine/Graphics/Vulkan/Command.hs
module Engine.Graphics.Vulkan.Command
  ( createVulkanCommandPool
  , createVulkanCommandCollection
  , recordRenderCommandBuffer
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
import Data.IORef (newIORef, readIORef, modifyIORef)
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
import Engine.Graphics.Font.Data
import Engine.Scene.Types
import Foreign.Storable (sizeOf, pokeElemOff)
import Foreign.Ptr (castPtr)
import Vulkan.CStruct.Extends
import Vulkan.Core10
import Vulkan.Core10.CommandBufferBuilding
         (ClearValue(..), ClearColorValue(..))
import Vulkan.Zero

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

recordRenderCommandBuffer ∷ CommandBuffer → Word64 → EngineM ε σ ()
recordRenderCommandBuffer cmdBuf frameIdx = do
    state ← gets graphicsState
    env ← ask
    
    -- Validate required state components
    pState ← maybe (throwGraphicsError PipelineError "Pipeline state not initialized") 
                  pure 
                  (pipelineState state)
    
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
    
    -- Bind graphics pipeline
    cmdBindPipeline cmdBuf PIPELINE_BIND_POINT_GRAPHICS (psPipeline pState)
    
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
    
    -- Verify descriptor sets and textures
    descManager ← maybe (throwGraphicsError DescriptorError "No descriptor state") 
                       pure 
                       (descriptorState state)
    
    -- Get the texture array state
    let (TexturePoolState descPool descLayout, textures) = textureState state
    textureArray ← case Map.lookup "default" (textureArrayStates state) of
        Nothing → throwGraphicsError TextureLoadFailed "No texture array state found"
        Just arr → pure arr
    
    let uniformSet = V.head $ dmActiveSets descManager
    textureSet ← case (tasDescriptorSet textureArray) of
          Nothing  → (throwGraphicsError DescriptorError "No texture descriptor set")
          Just set → pure set
    
    let descriptorSets = V.fromList [uniformSet, textureSet]
    -- Bind all descriptor sets at once
    cmdBindDescriptorSets cmdBuf 
        PIPELINE_BIND_POINT_GRAPHICS
        (psPipelineLayout pState)
        0  -- First set index
        descriptorSets
        V.empty  -- No dynamic offsets
    
    -- Bind vertex buffer
    forM_ (vertexBuffer state) $ \(vBuf, _) → do
        cmdBindVertexBuffers cmdBuf 
            0  -- First binding
            (V.singleton vBuf)
            (V.singleton 0)  -- Offsets
    
    -- Draw both quads in a single draw call
    cmdDraw cmdBuf
        12   -- vertex count (2 quads = 12 vertices)
        1    -- instance count
        0    -- first vertex
        0    -- first instance
        
    -- End render pass and command buffer
    cmdEndRenderPass cmdBuf
    endVulkanCommandBuffer cmdBuf

-- Helper function to prepare all command buffers
prepareFrameCommandBuffers ∷ EngineM ε σ ()
prepareFrameCommandBuffers = do
    state ← gets graphicsState

    case (vulkanDevice state) of
        Nothing → throwGraphicsError VulkanDeviceLost
            "Vulkan device not initialized"
        Just dev → case (vulkanCmdPool state) of
            Nothing → throwGraphicsError CommandBufferError
                "Vulkan command pool not initialized"
            Just cmdPool → case (vulkanCmdBuffers state) of
                Nothing → throwGraphicsError CommandBufferError
                    "Vulkan command buffers not initialized"
                Just cmdBuffers → do
                                    -- Reset command pool
                                    resetCommandPool dev cmdPool zero
                                    -- Record command buffer for each frame
                                    V.zipWithM_ (recordRenderCommandBuffer)
                                         cmdBuffers
                                         (V.generate 
                                           (V.length cmdBuffers)
                                           fromIntegral)

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
        , frCommandBuffer  = cmdBuffers  -- Use the Vector directly
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
-- Text batches are rendered INSIDE the render pass, before it ends
recordSceneCommandBuffer ∷ CommandBuffer → Word64 → SceneDynamicBuffer 
                         → V.Vector RenderBatch → V.Vector TextBatch 
                         → EngineM ε σ ()
recordSceneCommandBuffer cmdBuf frameIdx dynamicBuffer batches textBatches = do
    state ← gets graphicsState
    
    -- Validate required state components
    pState ← maybe (throwGraphicsError PipelineError "Pipeline state not initialized") 
                  pure 
                  (pipelineState state)
    
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
    
    -- Bind graphics pipeline for sprites
    cmdBindPipeline cmdBuf PIPELINE_BIND_POINT_GRAPHICS (psPipeline pState)
    
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
    
    -- Bind descriptor sets for sprites
    descManager ← maybe (throwGraphicsError DescriptorError "No descriptor state") 
                       pure 
                       (descriptorState state)
    
    let (TexturePoolState descPool descLayout, textures) = textureState state
    textureArray ← case Map.lookup "default" (textureArrayStates state) of
        Nothing → throwGraphicsError TextureLoadFailed "No texture array state found"
        Just arr → pure arr
    
    let uniformSet = V.head $ dmActiveSets descManager
    textureSet ← case (tasDescriptorSet textureArray) of
          Nothing → (throwGraphicsError DescriptorError "No texture descriptor set")
          Just set → pure set
    
    let descriptorSets = V.fromList [uniformSet, textureSet]
    cmdBindDescriptorSets cmdBuf 
        PIPELINE_BIND_POINT_GRAPHICS
        (psPipelineLayout pState)
        0
        descriptorSets
        V.empty
    
    -- Bind dynamic vertex buffer for sprites
    cmdBindVertexBuffers cmdBuf 
        0  -- First binding
        (V.singleton (sdbBuffer dynamicBuffer))
        (V.singleton 0)  -- Offsets
    
    -- Draw all sprite batches
    currentVertexOffset ← liftIO $ newIORef (0 ∷ Word32)
    V.forM_ batches $ \batch → do
        offset ← liftIO $ readIORef currentVertexOffset
        let vertexCount = fromIntegral $ V.length $ rbVertices batch
        
        -- Draw this batch
        cmdDraw cmdBuf
            vertexCount  -- vertex count
            1           -- instance count  
            offset      -- first vertex
            0           -- first instance
        
        -- Update offset for next batch
        liftIO $ modifyIORef currentVertexOffset (+ vertexCount)
    
    -- ========================================
    -- RENDER TEXT BATCHES (inside render pass)
    -- ========================================
    unless (V.null textBatches) $ do
        case (fontPipeline state, fontQuadBuffer state) of
            (Just (pipeline, layout), Just (quadBuffer, _)) → do
                -- Bind font pipeline
                cmdBindPipeline cmdBuf PIPELINE_BIND_POINT_GRAPHICS pipeline
                
                -- Reset viewport and scissor (same as before)
                cmdSetViewport cmdBuf 0 (V.singleton viewport)
                cmdSetScissor cmdBuf 0 (V.singleton scissor)
                
                -- Render each text batch
                !newBuffers ← V.foldM' (\acc batch → do
                    maybeBuffer ← renderTextBatchInline cmdBuf device pDevice 
                                                        quadBuffer layout batch state
                    case maybeBuffer of
                        Nothing → do
                          pure acc
                        Just !buf → do
                          let !newAcc = V.snoc acc buf
                          pure newAcc
                  ) V.empty textBatches
                
                -- Store instance buffers so they survive until next frame
                modify $ \s → s 
                    { graphicsState = (graphicsState s) 
                        { pendingInstanceBuffers = 
                            pendingInstanceBuffers (graphicsState s) <> newBuffers 
                        }
                    }
            _ → pure ()
    
    -- Clear the text batch queue
    modify $ \s → s 
        { graphicsState = (graphicsState s) { textBatchQueue = V.empty } }
    -- End render pass and command buffer
    cmdEndRenderPass cmdBuf
    endVulkanCommandBuffer cmdBuf

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
            let cache = fontCache state
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
