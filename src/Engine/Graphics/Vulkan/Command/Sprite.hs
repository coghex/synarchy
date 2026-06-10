{-# LANGUAGE Strict #-}
module Engine.Graphics.Vulkan.Command.Sprite
  ( renderSpritesBindless
  , renderSpritesBindlessUI
  ) where

import UPrelude
import qualified Data.Vector as V
import qualified Data.Text as T
import Data.IORef (IORef, readIORef, modifyIORef')
import Engine.Core.Log (LogCategory(..))
import Engine.Core.Log.Monad (logAndThrowM)
import Engine.Core.Monad
import Engine.Core.State
import Engine.Core.Error.Exception (ExceptionType(..), GraphicsError(..))
import Engine.Graphics.Vulkan.Types
import Engine.Graphics.Vulkan.Texture.Types (BindlessTextureSystem(..))
import Engine.Scene.Types (RenderBatch(..), SceneDynamicBuffer(..))
import Vulkan.Core10

-- | Render sprites using the bindless UI pipeline
renderSpritesBindlessUI ∷ CommandBuffer → GraphicsState → Viewport → Rect2D
                        → DescriptorSet → SceneDynamicBuffer → V.Vector RenderBatch
                        → IORef Word32 → EngineM ε σ ()
renderSpritesBindlessUI = renderSpritesWith
    (\state → (bindlessUIPipeline state, "Bindless UI pipeline not available"))

-- | Render sprites using the bindless pipeline
renderSpritesBindless ∷ CommandBuffer → GraphicsState → Viewport → Rect2D
                      → DescriptorSet → SceneDynamicBuffer → V.Vector RenderBatch
                      → IORef Word32 → EngineM ε σ ()
renderSpritesBindless = renderSpritesWith
    (\state → (bindlessPipeline state, "Bindless pipeline not available"))

-- | Shared sprite-rendering body; the two public entry points differ
--   only in which pipeline they bind. Skips all binding work when the
--   layer has no sprite batches.
renderSpritesWith ∷ (GraphicsState → (Maybe (Pipeline, PipelineLayout), Text))
                  → CommandBuffer → GraphicsState → Viewport → Rect2D
                  → DescriptorSet → SceneDynamicBuffer → V.Vector RenderBatch
                  → IORef Word32 → EngineM ε σ ()
renderSpritesWith selectPipeline cmdBuf state viewport scissor uniformSet
                  dynamicBuffer spriteBatches vertexOffsetRef =
  unless (V.null spriteBatches) $ do
    (pipeline, pipelineLayout) ← case selectPipeline state of
        (Just p, _)    → pure p
        (Nothing, err) → logAndThrowM CatVulkan (ExGraphics PipelineError) err

    bindless ← do
        env ← ask
        mts ← liftIO $ readIORef (textureSystemRef env)
        case mts of
            Just b → pure b
            _ → logAndThrowM CatVulkan (ExGraphics DescriptorError)
                             "Bindless texture system not available"

    cmdBindPipeline cmdBuf PIPELINE_BIND_POINT_GRAPHICS pipeline
    cmdSetViewport cmdBuf 0 (V.singleton viewport)
    cmdSetScissor cmdBuf 0 (V.singleton scissor)

    let textureSet = btsDescriptorSet bindless
        descriptorSets = V.fromList [uniformSet, textureSet]

    cmdBindDescriptorSets cmdBuf
        PIPELINE_BIND_POINT_GRAPHICS
        pipelineLayout
        0
        descriptorSets
        V.empty

    cmdBindVertexBuffers cmdBuf 0
        (V.singleton (sdbBuffer dynamicBuffer))
        (V.singleton 0)

    V.forM_ spriteBatches $ \batch → do
        offset ← liftIO $ readIORef vertexOffsetRef
        let vertexCount = fromIntegral $ V.length $ rbVertices batch
        cmdDraw cmdBuf vertexCount 1 offset 0
        liftIO $ modifyIORef' vertexOffsetRef (+ vertexCount)
