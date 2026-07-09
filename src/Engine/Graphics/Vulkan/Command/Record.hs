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
import Engine.Core.Types (EngineConfig(..), BootProfile(..))
import Engine.Core.Error.Exception (ExceptionType(..), GraphicsError(..))
import Engine.Graphics.Types (SwapchainInfo(..))
import Engine.Graphics.Vulkan.Screenshot (recordScreenshotCopy)
import Engine.Graphics.Vulkan.Types.Descriptor
import Engine.Graphics.Vulkan.Command.Sprite (renderSpritesBindless, renderSpritesBindlessUI)
import Engine.Graphics.Vulkan.Command.Text (renderTextBatches, ensureTextInstanceBuffer, uploadTextInstances)
import Engine.Scene.Base (LayerId(..))
import Engine.Scene.Types
import World.Grid (uiLayerThreshold)
import Vulkan.Core10
import Vulkan.Zero

-- | Record scene command buffer with sprite and text batches. When a
--   screenshot capture is pending (#643), @mCapture@ carries the
--   swapchain image being rendered plus the staging buffer to copy it
--   into; the copy is recorded after the render pass so the captured
--   pixels are exactly what this frame presents.
recordSceneCommandBuffer ∷ CommandBuffer → Word64 → Int → SceneDynamicBuffer
                         → Map.Map LayerId (V.Vector RenderItem)
                         → Maybe (Image, Buffer)
                         → EngineM ε σ ()
recordSceneCommandBuffer cmdBuf imageIndex frameInFlight dynamicBuffer layeredBatches mCapture = do
    logDebugSM CatRender "Recording command buffer"
      [("image", T.pack $ show imageIndex)
      ,("frame_in_flight", T.pack $ show frameInFlight)
      ,("layer_count", T.pack $ show $ Map.size layeredBatches)]
    
    state ← gets graphicsState
    env ← ask

    renderPass ← maybe (logAndThrowM CatVulkan (ExGraphics RenderPassError)
                                     "Render pass not initialized")
                      pure
                      (vulkanRenderPass state)
    framebuffer ← maybe (logAndThrowM CatVulkan (ExGraphics FramebufferError)
                                     "Framebuffer not initialized")
                       (\fbs → if imageIndex < fromIntegral (V.length fbs)
                              then pure (fbs V.! fromIntegral imageIndex)
                              else logAndThrowM CatVulkan (ExGraphics FramebufferError)
                                                       "Image index out of bounds")
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

    -- Select THIS frame-in-flight's uniform descriptor set. Each set is
    -- wired to its own per-frame UBO (Init.createUniformBuffersForFrames);
    -- binding set 0 unconditionally was the frames-in-flight bug: odd
    -- frames rendered the previous frame's UBO and raced its CPU write.
    descManager ← maybe (logAndThrowM CatVulkan (ExGraphics DescriptorError)
                                      "No descriptor manager")
                        pure
                        (descriptorState state)
    uniformSet ← if frameInFlight < V.length (dmActiveSets descManager)
        then pure (dmActiveSets descManager V.! frameInFlight)
        else logAndThrowM CatVulkan (ExGraphics DescriptorError)
                          "frameInFlight out of descriptor-set bounds"

    -- Collect all text batches across layers for a single upload pass.
    -- Layers are walked in ascending order so the global index stays aligned.
    let allTextBatches = V.concatMap
            (\(_, items) → V.mapMaybe (\case TextItem trb → Just trb; _ → Nothing) items)
            (V.fromList $ Map.toAscList layeredBatches)
        totalGlyphs = V.sum $ V.map (fromIntegral . V.length . trbInstances) allTextBatches

    let mTib0 = case textInstanceBuffers state V.!? frameInFlight of
                    Just m  → m
                    Nothing → Nothing
    tib ← if totalGlyphs > 0
        then do
            tib0 ← ensureTextInstanceBuffer device pDevice totalGlyphs mTib0
            (tib1, _) ← uploadTextInstances device tib0 allTextBatches
            modify $ \s → s { graphicsState = (graphicsState s)
                                { textInstanceBuffers =
                                    textInstanceBuffers (graphicsState s)
                                        V.// [(frameInFlight, Just tib1)] } }
            pure tib1
        else pure $ case mTib0 of
               Just t  → t
               Nothing → TextInstanceBuffer zero zero 0 0

    let offsets = V.prescanl' (\acc trb → acc + fromIntegral (V.length (trbInstances trb))) 0 allTextBatches
        counts  = V.map (fromIntegral . V.length . trbInstances) allTextBatches
        drawInfos = V.zip offsets counts

    batchIdxRef ← liftIO $ newIORef (0 ∷ Int)
    let beginInfo = (zero ∷ CommandBufferBeginInfo '[])
                      { flags = COMMAND_BUFFER_USAGE_ONE_TIME_SUBMIT_BIT }
    liftIO $ beginCommandBuffer cmdBuf beginInfo
    
    logDebugM CatRender "Beginning render pass"
    
    -- Preview boot (#632) clears to the epic's grey (#828382) instead of
    -- the normal opaque black, so a bare window with no scene content
    -- still reads as "booted", not "still loading".
    let clearColor = case ecBootProfile (engineConfig env) of
            BootPreview → Color ( Float32 0.50980392 0.51372549 0.50980392 1.0 )
            _           → Color ( Float32 0.0 0.0 0.0 1.0 )
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
        renderLayerItems cmdBuf state viewport scissor uniformSet dynamicBuffer
                         items vertexOffsetRef False
                         tib drawInfos batchIdxRef

    forM_ (Map.toAscList uiLayers) $ \(_, items) →
        renderLayerItems cmdBuf state viewport scissor uniformSet dynamicBuffer
                         items vertexOffsetRef True
                         tib drawInfos batchIdxRef

    logDebugM CatRender "Ending render pass"
    cmdEndRenderPass cmdBuf
    -- Pending screenshot (#643): the render pass just left the
    -- swapchain image in PRESENT_SRC; copy it out before the buffer
    -- closes so the capture rides this frame's submission and fence.
    forM_ mCapture $ \(capImg, capBuf) →
        liftIO $ recordScreenshotCopy cmdBuf capImg capBuf swapchainExtent
    liftIO $ endCommandBuffer cmdBuf

-- | Render items in a single layer
renderLayerItems ∷ CommandBuffer → GraphicsState → Viewport → Rect2D
                 → DescriptorSet → SceneDynamicBuffer → V.Vector RenderItem
                 → IORef Word32 → Bool
                 → TextInstanceBuffer
                 → V.Vector (Word32, Word32)
                 → IORef Int
                 → EngineM ε σ ()
renderLayerItems cmdBuf state viewport scissor uniformSet dynamicBuffer items
                 vertexOffsetRef isUI
                 tib drawInfos batchIdxRef = do
    let spriteBatches = V.mapMaybe (\case SpriteItem b → Just b; _ → Nothing) items
    
    if isUI
        then renderSpritesBindlessUI cmdBuf state viewport scissor
                                     uniformSet dynamicBuffer spriteBatches vertexOffsetRef
        else renderSpritesBindless cmdBuf state viewport scissor
                                   uniformSet dynamicBuffer spriteBatches vertexOffsetRef
    
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

                renderTextBatches cmdBuf quadBuffer layout
                                  uniformSet tib slicedInfos
            _ → pure ()
