{-# LANGUAGE Strict #-}
module Engine.Graphics.Sprite.Renderer
  ( createSpriteRenderer
  , destroySpriteRenderer
  , beginSpriteRendering
  , endSpriteRendering
  , drawSprite
  , updateSpriteCamera
  ) where

import UPrelude
import Control.Monad (when)
import qualified Data.Vector as V
import Engine.Core.Monad
import Engine.Core.Resource
import Engine.Core.Error.Exception
import Engine.Graphics.Sprite.Types
import Engine.Graphics.Sprite.Math
import Engine.Graphics.Sprite.Batch
import Engine.Graphics.Types
import Engine.Graphics.Vulkan.Types
import Engine.Graphics.Vulkan.Buffer
import Engine.Graphics.Vulkan.Command
import Engine.Graphics.Vulkan.Types.Texture
import Vulkan.Core10
import Vulkan.Zero

-- | Create sprite renderer with initial buffers
createSpriteRenderer ∷ Device 
                    → PhysicalDevice
                    → CommandPool
                    → Queue
                    → Int    -- ^ Viewport width
                    → Int    -- ^ Viewport height
                    → EngineM ε σ SpriteRenderer
createSpriteRenderer device pDevice cmdPool cmdQueue width height = do
  let maxSprites = unMaxBatchSize defaultMaxBatchSize
      vertexSize = maxSprites * verticesPerSprite * 5 * sizeOf (0 ∷ Float)
      indexSize  = maxSprites * indicesPerSprite * sizeOf (0 ∷ Word32)

  -- Create vertex buffer
  (vboMem, vbo) ← createVulkanBuffer device pDevice
    (fromIntegral vertexSize)
    (BUFFER_USAGE_VERTEX_BUFFER_BIT .|. BUFFER_USAGE_TRANSFER_DST_BIT)
    MEMORY_PROPERTY_DEVICE_LOCAL_BIT

  -- Create index buffer
  (iboMem, ibo) ← createVulkanBuffer device pDevice
    (fromIntegral indexSize)
    (BUFFER_USAGE_INDEX_BUFFER_BIT .|. BUFFER_USAGE_TRANSFER_DST_BIT)
    MEMORY_PROPERTY_DEVICE_LOCAL_BIT

  pure $ SpriteRenderer
    { rendererVBO       = vbo
    , rendererVBOMemory = vboMem
    , rendererIBO       = ibo
    , rendererIBOMemory = iboMem
    , rendererBatches   = V.empty
    , rendererCamera    = defaultCamera2D width height
    }

-- | Begin sprite rendering frame
beginSpriteRendering ∷ Device
                    → CommandBuffer
                    → SpriteRenderer
                    → RenderPass
                    → Framebuffer
                    → Extent2D        -- ^ Viewport size
                    → EngineM ε σ ()
beginSpriteRendering device cmdBuf renderer renderPass fb extent = do
  let renderPassInfo = zero
        { renderPass  = renderPass
        , framebuffer = fb
        , renderArea  = Rect2D (Offset2D 0 0) extent
        , clearValues = V.singleton $ Color $ ClearColorFloat 0 0 0 1
        }
  
  cmdBeginRenderPass cmdBuf renderPassInfo SUBPASS_CONTENTS_INLINE

-- | End sprite rendering frame
endSpriteRendering ∷ Device
                  → CommandPool
                  → Queue
                  → CommandBuffer
                  → SpriteRenderer
                  → EngineM ε σ ()
endSpriteRendering device cmdPool cmdQueue cmdBuf renderer = do
  -- Flush batches
  forM_ (rendererBatches renderer) $ \batch → do
    when (batchCount batch > 0) $ do
      -- Update vertex buffer
      copyDataToBuffer device cmdPool cmdQueue
        (rendererVBO renderer)
        (batchVertices batch)
      
      -- Update index buffer
      copyDataToBuffer device cmdPool cmdQueue
        (rendererIBO renderer)
        (batchIndices batch)
      
      -- Draw batch
      cmdBindVertexBuffers cmdBuf 
        0 
        (V.singleton $ rendererVBO renderer)
        (V.singleton 0)
      
      cmdBindIndexBuffer cmdBuf 
        (rendererIBO renderer)
        0
        INDEX_TYPE_UINT32
      
      let indexCount = batchCount batch * indicesPerSprite
      cmdDrawIndexed cmdBuf
        (fromIntegral indexCount)  -- index count
        1       -- instance count
        0       -- first index
        0       -- vertex offset
        0       -- first instance
  
  cmdEndRenderPass cmdBuf

-- | Draw a single sprite
drawSprite ∷ Device
           → CommandBuffer
           → SpriteRenderer
           → Sprite
           → EngineM ε σ SpriteRenderer
drawSprite device cmdBuf renderer sprite = do
  let camera = rendererCamera renderer
      viewProj = createProjectionMatrix 
        (viewportWidth camera) 
        (viewportHeight camera) 
        ⊗ createViewMatrix 
            (cameraPosition camera)
            (cameraZoom camera)
            (cameraRotation camera)
      
      -- Find or create appropriate batch
      mBatch = V.find 
        (\b → batchTexture b ≡ spriteTexture sprite)
        (rendererBatches renderer)
  
  batch ← case mBatch of
    Just b  → pure b
    Nothing → createSpriteBatch (spriteTexture sprite) defaultMaxBatchSize
  
  -- Add sprite to batch
  newBatch ← addSpriteToBatch batch sprite viewProj
  
  -- Update renderer batches
  let newBatches = case mBatch of
        Just _  → V.map (\b → if batchTexture b ≡ spriteTexture sprite
                             then newBatch else b)
                        (rendererBatches renderer)
        Nothing → V.snoc (rendererBatches renderer) newBatch
  
  pure $ renderer { rendererBatches = newBatches }

-- | Update camera parameters
updateSpriteCamera ∷ SpriteRenderer
                  → (Camera2D → Camera2D)  -- ^ Camera update function
                  → SpriteRenderer
updateSpriteCamera renderer updateFunc =
  renderer { rendererCamera = updateFunc (rendererCamera renderer) }

-- | Clean up renderer resources
destroySpriteRenderer ∷ Device → SpriteRenderer → EngineM ε σ ()
destroySpriteRenderer device renderer = do
  destroyBuffer device (rendererVBO renderer) Nothing
  freeMemory device (rendererVBOMemory renderer) Nothing
  destroyBuffer device (rendererIBO renderer) Nothing
  freeMemory device (rendererIBOMemory renderer) Nothing
