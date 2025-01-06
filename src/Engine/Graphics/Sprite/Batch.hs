{-# LANGUAGE Strict #-}
module Engine.Graphics.Sprite.Batch
  ( MaxBatchSize(..)
  , defaultMaxBatchSize
  , createSpriteBatch
  , addSpriteToBatch
  , flushBatch
  , verticesPerSprite
  , indicesPerSprite
  ) where

import UPrelude
import qualified Data.Vector as V
import Data.Word (Word32)
import Engine.Graphics.Sprite.Types
import Engine.Graphics.Sprite.Math
import Engine.Graphics.Vulkan.Types.Texture
import Engine.Core.Monad

-- | Maximum number of sprites per batch
newtype MaxBatchSize = MaxBatchSize { unMaxBatchSize ∷ Int }
  deriving (Show, Eq)

-- | Default maximum batch size (1000 sprites)
defaultMaxBatchSize ∷ MaxBatchSize
defaultMaxBatchSize = MaxBatchSize 1000

-- | Constants for vertex/index counts
verticesPerSprite ∷ Int
verticesPerSprite = 4  -- quad corners

indicesPerSprite ∷ Int
indicesPerSprite = 6  -- two triangles

-- | Create an empty sprite batch
createSpriteBatch ∷ TextureData      -- ^ Texture for this batch
                 → MaxBatchSize     -- ^ Maximum sprites in batch
                 → EngineM ε σ SpriteBatch
createSpriteBatch tex (MaxBatchSize maxSprites) = pure $ SpriteBatch
  { batchTexture  = tex
  , batchVertices = V.empty
  , batchIndices  = V.empty
  , batchCount    = 0
  }

-- | Add a sprite to a batch, returns updated batch
addSpriteToBatch ∷ SpriteBatch    -- ^ Target batch
                → Sprite         -- ^ Sprite to add
                → Mat4          -- ^ View-projection matrix
                → EngineM ε σ SpriteBatch
addSpriteToBatch batch sprite viewProj = do
  let transform = createTransformMatrix 
        (spritePosition sprite)
        (spriteScale sprite)
        (spriteRotation sprite)
      
      -- Calculate vertex positions
      (v0, v1, v2, v3) = calculateVertexPositions transform (spriteOrigin sprite)
      
      -- Calculate texture coordinates
      (t0, t1, t2, t3) = case spriteSourceRect sprite of
        Nothing → ((0,0), (1,0), (0,1), (1,1))  -- Full texture
        Just (x, y, w, h) → calculateTextureCoords x y w h
      
      -- Pack color
      color = packColor (spriteColor sprite)
      
      -- Create vertex data
      newVertices = V.concat
        [ vertexData v0 t0 color  -- Top-left
        , vertexData v1 t1 color  -- Top-right
        , vertexData v2 t2 color  -- Bottom-left
        , vertexData v3 t3 color  -- Bottom-right
        ]
      
      -- Create index data
      baseVertex = batchCount batch * 4
      newIndices = V.fromList
        [ fromIntegral $ baseVertex     -- First triangle
        , fromIntegral $ baseVertex + 1
        , fromIntegral $ baseVertex + 2
        , fromIntegral $ baseVertex + 1 -- Second triangle
        , fromIntegral $ baseVertex + 3
        , fromIntegral $ baseVertex + 2
        ]
  
  pure $ batch
    { batchVertices = batchVertices batch V.++ newVertices
    , batchIndices  = batchIndices batch V.++ newIndices
    , batchCount    = batchCount batch + 1
    }

-- | Calculate vertex positions for a quad
calculateVertexPositions ∷ Mat4 → Vec2 → ((Float,Float), (Float,Float), (Float,Float), (Float,Float))
calculateVertexPositions transform (Vec2 ox oy) =
  let -- Vertex positions relative to origin
      tl = transformPoint transform (-ox)     (-oy)      -- Top-left
      tr = transformPoint transform (1.0-ox)  (-oy)      -- Top-right
      bl = transformPoint transform (-ox)     (1.0-oy)   -- Bottom-left
      br = transformPoint transform (1.0-ox)  (1.0-oy)   -- Bottom-right
  in (tl, tr, bl, br)

-- | Transform a point by a matrix
transformPoint ∷ Mat4 → Float → Float → (Float,Float)
transformPoint m x y =
  let w = m.m03 * x + m.m13 * y + m.m33
      px = (m.m00 * x + m.m10 * y + m.m30) / w
      py = (m.m01 * x + m.m11 * y + m.m31) / w
  in (px, py)

-- | Calculate texture coordinates for sprite sheets
calculateTextureCoords ∷ Int → Int → Int → Int → ((Float,Float), (Float,Float), (Float,Float), (Float,Float))
calculateTextureCoords x y w h =
  let -- Get normalized texture coordinates
      u0 = fromIntegral x / texWidth
      v0 = fromIntegral y / texHeight
      u1 = fromIntegral (x + w) / texWidth
      v1 = fromIntegral (y + h) / texHeight
  in ((u0,v0), (u1,v0), (u0,v1), (u1,v1))
  where
    texWidth = 1.0   -- Normalized coordinates
    texHeight = 1.0

-- | Pack color components into a single float
packColor ∷ Vec4 → Float
packColor (Vec4 r g b a) =
  let r' = floor (r * 255) `shiftL` 24
      g' = floor (g * 255) `shiftL` 16
      b' = floor (b * 255) `shiftL` 8
      a' = floor (a * 255)
  in fromIntegral (r' .|. g' .|. b' .|. a')

-- | Create vertex data array for a single vertex
vertexData ∷ (Float,Float) → (Float,Float) → Float → V.Vector Float
vertexData (px,py) (u,v) color =
  V.fromList [px, py, u, v, color]
