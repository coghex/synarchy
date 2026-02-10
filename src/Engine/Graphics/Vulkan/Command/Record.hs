{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE Strict #-}
module Engine.Graphics.Vulkan.Command.Record
  ( recordSceneCommandBuffer
  , renderLayerItems
  , uiLayerThreshold
  ) where

import UPrelude
import qualified Data.Vector as V
import qualified Data.Map as Map
import qualified Data.Text as T
import Data.IORef (newIORef, IORef)
import Engine.Core.Log (LogCategory(..))
import Engine.Core.Log.Monad (logAndThrowM, logDebugM, logDebugSM)
import Engine.Core.Monad
import Engine.Core.State
import Engine.Core.Error.Exception (ExceptionType(..), GraphicsError(..))
import Engine.Graphics.Types (SwapchainInfo(..))
import Engine.Graphics.Vulkan.Types
import Engine.Graphics.Vulkan.Types.Descriptor
import Engine.Graphics.Vulkan.Command.Sprite (renderSpritesBindless, renderSpritesBindlessUI)
import Engine.Graphics.Vulkan.Command.Text (renderTextBatchInline)
import Engine.Scene.Base (LayerId(..))
import Engine.Scene.Types
import World.Grid (uiLayerThreshold)
import Vulkan.Core10
import Vulkan.Core10.CommandBufferBuilding (ClearValue(..), ClearColorValue(..))
import Vulkan.Zero

-- | Record scene command buffer with sprite and text batches
recordSceneCommandBuffer ∷ CommandBuffer → Word64 → SceneDynamicBuffer 
                         → Map.Map LayerId (V.Vector RenderItem)
                         → EngineM ε σ ()
recordSceneCommandBuffer cmdBuf frameIdx dynamicBuffer layeredBatches = do
    logDebugSM CatRender "Recording command buffer"
      [("frame", T.pack $ show frameIdx)
      ,("layer_count", T.pack $ show $ Map.size layeredBatches)]
    
    state ← gets graphicsState
    
    -- Validate required state components
    renderPass ← maybe (logAndThrowM CatVulkan (ExGraphics RenderPassError)
                                     "Render pass not initialized")
                      pure
                      (vulkanRenderPass state)
    framebuffer ← maybe (logAndThrowM CatVulkan (ExGraphics FramebufferError)
                                     "Framebuffer not initialized")
                       (\fbs → if frameIdx < fromIntegral (V.length fbs)
                              then pure (fbs V.! fromIntegral frameIdx)
                              else logAndThrowM CatVulkan (ExGraphics FramebufferError)
                                                       "Frame index out of bounds")
                       (framebuffers state)
    swapchainExtent ← maybe (logAndThrowM CatVulkan (ExGraphics SwapchainError)
                                         "Swapchain info not initialized")
                           (pure . siSwapExtent)
                           (swapchainInfo state)
    device ← maybe (logAndThrowM CatVulkan (ExGraphics VulkanDeviceLost)
                                   "No device")
                   pure
                   (vulkanDevice state)
    pDevice ← maybe (logAndThrowM CatVulkan (ExGraphics VulkanDeviceLost)
                                    "No physical device")
                    pure
                    (vulkanPDevice state)

    -- Begin command buffer
    let beginInfo = (zero ∷ CommandBufferBeginInfo '[])
                      { flags = COMMAND_BUFFER_USAGE_ONE_TIME_SUBMIT_BIT }
    liftIO $ beginCommandBuffer cmdBuf beginInfo
    
    logDebugM CatRender "Beginning render pass"
    
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
    forM_ (Map.toAscList worldLayers) $ \(_, items) → do
        renderLayerItems cmdBuf state viewport scissor dynamicBuffer 
                         items vertexOffsetRef device pDevice False
    
    -- Render UI layers (>= 10) with UI pipelines  
    forM_ (Map.toAscList uiLayers) $ \(_, items) → do
        renderLayerItems cmdBuf state viewport scissor dynamicBuffer 
                         items vertexOffsetRef device pDevice True

    -- End render pass and command buffer
    logDebugM CatRender "Ending render pass"
    cmdEndRenderPass cmdBuf
    liftIO $ endCommandBuffer cmdBuf

-- | Render items in a single layer
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
                logDebugM CatRender $ "Binding font pipeline (UI: " 
                                    <> (if isUI then "yes" else "no") <> ")"
                
                cmdBindPipeline cmdBuf PIPELINE_BIND_POINT_GRAPHICS pipeline
                cmdSetViewport cmdBuf 0 (V.singleton viewport)
                cmdSetScissor cmdBuf 0 (V.singleton scissor)
                
                descManager ← maybe (logAndThrowM CatVulkan (ExGraphics DescriptorError)
                                             "No descriptor state")
                                   pure 
                                   (descriptorState state)
                
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
