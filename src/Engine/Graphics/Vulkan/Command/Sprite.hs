{-# LANGUAGE Strict #-}
module Engine.Graphics.Vulkan.Command.Sprite
  ( renderSpritesBindless
  , renderSpritesBindlessUI
  ) where

import UPrelude
import qualified Data.Vector as V
import Data.IORef (IORef, readIORef, modifyIORef)
import Engine.Core.Log (LogCategory(..))
import Engine.Core.Log.Monad (logAndThrowM)
import Engine.Core.Monad
import Engine.Core.State
import Engine.Core.Error.Exception (ExceptionType(..), GraphicsError(..))
import Engine.Graphics.Vulkan.Types
import Engine.Graphics.Vulkan.Types.Descriptor
import Engine.Graphics.Vulkan.Texture.Types (BindlessTextureSystem(..))
import Engine.Scene.Types (RenderBatch(..), SceneDynamicBuffer(..))
import Vulkan.Core10

-- | Render sprites using the bindless UI pipeline
renderSpritesBindlessUI ∷ CommandBuffer → GraphicsState → Viewport → Rect2D
                        → SceneDynamicBuffer → V.Vector RenderBatch 
                        → IORef Word32 → EngineM ε σ ()
renderSpritesBindlessUI cmdBuf state viewport scissor dynamicBuffer spriteBatches vertexOffsetRef = do
    -- Get bindless UI pipeline and texture system
    (pipeline, pipelineLayout) ← case bindlessUIPipeline state of
        Just p → pure p
        Nothing → logAndThrowM CatVulkan (ExGraphics PipelineError)
                               "Bindless UI pipeline not available"
    
    bindless ← case textureSystem state of
        Just b → pure b
        _ → logAndThrowM CatVulkan (ExGraphics DescriptorError)
                         "Bindless texture system not available"
    
    descManager ← maybe (logAndThrowM CatVulkan (ExGraphics DescriptorError)
                                   "No descriptor state") 
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
        Nothing → logAndThrowM CatVulkan (ExGraphics PipelineError)
                               "Bindless pipeline not available"
    
    bindless ← case textureSystem state of
        Just b → pure b
        _ → logAndThrowM CatVulkan (ExGraphics DescriptorError)
                         "Bindless texture system not available"
    
    descManager ← maybe (logAndThrowM CatVulkan (ExGraphics DescriptorError)
                                   "No descriptor state") 
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
