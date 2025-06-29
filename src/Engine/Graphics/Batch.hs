{-# LANGUAGE Strict #-}
module Engine.Graphics.Batch
    ( SpriteBatch(..)
    , BatchKey(..)
    , createBatch
    , updateBatch
    , generateSpriteVertices
    , maxSpritesPerBatch
    ) where

import UPrelude
import qualified Data.Vector as V
import qualified Data.Map as Map
import Engine.Scene.Types
import Engine.Scene.Base
import Engine.Graphics.Vulkan.Types.Vertex
import Engine.Asset.Base (AssetId)

-- | Maximum sprites per batch
maxSpritesPerBatch ∷ Int
maxSpritesPerBatch = 1000  -- Adjust based on your needs

-- | Key for identifying unique batches
data BatchKey = BatchKey
    { batchLayerId ∷ LayerId
    , batchTexture ∷ AssetId
    , batchDepth   ∷ Float
    } deriving (Eq, Ord, Show)

-- | Creates a new empty batch
createBatch ∷ Int → SpriteBatch
createBatch capacity = SpriteBatch
    { batchVertices  = V.empty
    , batchObjects   = Map.empty
    , batchDirty     = False
    , batchCapacity  = capacity
    , batchCount     = 0
    }

-- | Generate vertices for a single sprite object
generateSpriteVertices ∷ SceneObject → V.Vector Vertex
generateSpriteVertices obj = V.fromList
    [ Vertex pos1 tex1 color atlasId
    , Vertex pos2 tex2 color atlasId
    , Vertex pos3 tex3 color atlasId
    , Vertex pos3 tex3 color atlasId
    , Vertex pos4 tex4 color atlasId
    , Vertex pos1 tex1 color atlasId
    ]
  where
    Transform2D (px, py) rot (sx, sy) _ = objTransform obj
    -- Calculate rotated and scaled corners
    (cos_r, sin_r) = (cos rot, sin rot)
    transform (x, y) = Vec2
        (px + (x * cos_r - y * sin_r) * sx)
        (py + (x * sin_r + y * cos_r) * sy)
    
    -- Vertex positions (counter-clockwise)
    pos1 = transform (-0.5, -0.5)  -- Bottom left
    pos2 = transform ( 0.5, -0.5)  -- Bottom right
    pos3 = transform ( 0.5,  0.5)  -- Top right
    pos4 = transform (-0.5,  0.5)  -- Top left
    
    -- Texture coordinates
    tex1 = Vec2 0 0  -- Bottom left
    tex2 = Vec2 1 0  -- Bottom right
    tex3 = Vec2 1 1  -- Top right
    tex4 = Vec2 0 1  -- Top left
    
    -- Color and atlas ID
    color = Vec4 1 1 1 1  -- White
    atlasId = maybe 0 fromIntegral $ objTexture obj

-- | Updates batch vertices based on current objects
updateBatch ∷ SpriteBatch → SpriteBatch
updateBatch batch =
    if batchDirty batch
        then batch
            { batchVertices = newVertices
            , batchDirty = False
            }
        else batch
  where
    newVertices = V.concat
        $ Map.elems
        $ Map.map generateSpriteVertices
        $ batchObjects batch
