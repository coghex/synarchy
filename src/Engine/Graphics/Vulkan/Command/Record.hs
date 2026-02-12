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
import Data.IORef (newIORef, readIORef, writeIORef, IORef)
import Engine.Core.Log (LogCategory(..))
import Engine.Core.Log.Monad (logAndThrowM, logDebugM, logDebugSM)
import Engine.Core.Monad
import Engine.Core.State
import Engine.Core.Error.Exception (ExceptionType(..), GraphicsError(..))
import Engine.Graphics.Types (SwapchainInfo(..))
import Engine.Graphics.Vulkan.Types
import Engine.Graphics.Vulkan.Types.Descriptor
import Engine.Graphics.Vulkan.Command.Sprite (renderSpritesBindless, renderSpritesBindlessUI)
import Engine.Graphics.Vulkan.Command.Text (renderTextBatches, ensureTextInstanceBuffer, uploadTextInstances)
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

    -- ── Collect ALL text batches across every layer, upload once ──
    -- We walk layers in ascending order so the global index lines up.
    let allTextBatches = V.concatMap
            (\(_, items) → V.mapMaybe (\case TextItem trb → Just trb; _ → Nothing) items)
            (V.fromList $ Map.toAscList layeredBatches)
        totalGlyphs = V.sum $ V.map (fromIntegral . V.length . trbInstances) allTextBatches

    tib ← if totalGlyphs > 0
        then do
            tib0 ← ensureTextInstanceBuffer device pDevice totalGlyphs
                                            (textInstanceBuffer state)
            (tib1, _) ← uploadTextInstances device tib0 allTextBatches
            modify $ \s → s { graphicsState = (graphicsState s)
                                { textInstanceBuffer = Just tib1 } }
            pure tib1
        else pure $ case textInstanceBuffer state of
               Just t  → t
               Nothing → TextInstanceBuffer zero zero 0 0

    -- Re-compute draw infos (cheap, pure arithmetic)
    let offsets = V.prescanl' (\acc trb → acc + fromIntegral (V.length (trbInstances trb))) 0 allTextBatches
        counts  = V.map (fromIntegral . V.length . trbInstances) allTextBatches
        drawInfos = V.zip offsets counts

    -- Index into drawInfos as we walk layers
    batchIdxRef ← liftIO $ newIORef (0 ∷ Int)

    -- Begin command buffer
    let beginInfo = (zero ∷ CommandBufferBeginInfo '[])
                      { flags = COMMAND_BUFFER_USAGE_ONE_TIME_SUBMIT_BIT }
    liftIO $ beginCommandBuffer cmdBuf beginInfo
    
    logDebugM CatRender "Beginning render pass"
    
    let clearColor = Color ( Float32 0.0 0.0 0.4 1.0 )
        renderPassInfo = zero
          { renderPass = renderPass
          , framebuffer = framebuffer
          , renderArea = Rect2D (Offset2D 0 0) swapchainExtent
          , clearValues = V.singleton clearColor
          }
    
    cmdBeginRenderPass cmdBuf renderPassInfo SUBPASS_CONTENTS_INLINE
    
    let Extent2D w h = swapchainExtent
        viewport = Viewport
          { x = 0, y = 0
          , width = fromIntegral w, height = fromIntegral h
          , minDepth = 0, maxDepth = 1
          }
        scissor = Rect2D
          { offset = Offset2D 0 0
          , extent = swapchainExtent
          }
    
    cmdSetViewport cmdBuf 0 (V.singleton viewport)
    cmdSetScissor cmdBuf 0 (V.singleton scissor)
    
    vertexOffsetRef ← liftIO $ newIORef (0 ∷ Word32)
    
    let (worldLayers, uiLayers) = Map.partitionWithKey 
                                    (\layerId _ → layerId < uiLayerThreshold) 
                                    layeredBatches
    
    forM_ (Map.toAscList worldLayers) $ \(_, items) →
        renderLayerItems cmdBuf state viewport scissor dynamicBuffer 
                         items vertexOffsetRef device pDevice False
                         tib drawInfos batchIdxRef

    forM_ (Map.toAscList uiLayers) $ \(_, items) →
        renderLayerItems cmdBuf state viewport scissor dynamicBuffer 
                         items vertexOffsetRef device pDevice True
                         tib drawInfos batchIdxRef

    logDebugM CatRender "Ending render pass"
    cmdEndRenderPass cmdBuf
    liftIO $ endCommandBuffer cmdBuf

-- | Render items in a single layer
renderLayerItems ∷ CommandBuffer → GraphicsState → Viewport → Rect2D
                 → SceneDynamicBuffer → V.Vector RenderItem
                 → IORef Word32 → Device → PhysicalDevice → Bool
                 → TextInstanceBuffer
                 → V.Vector (Word32, Word32)
                 → IORef Int
                 → EngineM ε σ ()
renderLayerItems cmdBuf state viewport scissor dynamicBuffer items 
                 vertexOffsetRef device pDevice isUI
                 tib drawInfos batchIdxRef = do
    -- Render sprites in this layer
    let spriteBatches = V.mapMaybe (\case SpriteItem b → Just b; _ → Nothing) items
    
    if isUI
        then renderSpritesBindlessUI cmdBuf state viewport scissor 
                                     dynamicBuffer spriteBatches vertexOffsetRef
        else renderSpritesBindless cmdBuf state viewport scissor 
                                   dynamicBuffer spriteBatches vertexOffsetRef
    
    -- Render text in this layer
    let textItems = V.mapMaybe (\case TextItem trb → Just trb; _ → Nothing) items 
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

                -- Slice this layer's draw infos from the global array
                startIdx ← liftIO $ readIORef batchIdxRef
                let n = V.length textItems
                    slicedInfos = V.zip textItems (V.slice startIdx n drawInfos)
                liftIO $ writeIORef batchIdxRef (startIdx + n)

                renderTextBatches cmdBuf device pDevice quadBuffer layout
                                  state tib slicedInfos
            _ → pure ()
