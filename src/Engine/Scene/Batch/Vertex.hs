{-# LANGUAGE Strict #-}
module Engine.Scene.Batch.Vertex
  ( generateQuadVertices
  ) where

import UPrelude
import qualified Data.Vector as V
import Engine.Scene.Types.Node (SceneNode(..), WorldTransform(..))
import Engine.Graphics.Vulkan.Types.Vertex (Vertex(..), Vec2(..))

-- | Generate quad vertices for a scene node
-- faceMapSlot = the bindless slot for this sprite's face map
-- (use defaultFaceMapSlot for sprites without a custom face map)
generateQuadVertices ∷ SceneNode → WorldTransform → Float → Float → (Vertex, Vertex, Vertex, Vertex)
generateQuadVertices node worldTrans atlasId faceMapSlot =
    let (sizeX, sizeY) = nodeSize node
        (posX, posY) = wtPosition worldTrans
        color = nodeColor node
        
        (uvMin, uvMax) = case nodeUVRect node of
            Just (minUV, maxUV) → (minUV, maxUV)
            Nothing → (Vec2 0.0 0.0, Vec2 1.0 1.0)
        
        halfX = sizeX * 0.5
        halfY = sizeY * 0.5
        
        v1 = Vertex (Vec2 (posX - halfX) (posY - halfY)) (Vec2 (x uvMin) (y uvMin)) color atlasId faceMapSlot
        v2 = Vertex (Vec2 (posX + halfX) (posY - halfY)) (Vec2 (x uvMax) (y uvMin)) color atlasId faceMapSlot
        v3 = Vertex (Vec2 (posX + halfX) (posY + halfY)) (Vec2 (x uvMax) (y uvMax)) color atlasId faceMapSlot
        v4 = Vertex (Vec2 (posX - halfX) (posY + halfY)) (Vec2 (x uvMin) (y uvMax)) color atlasId faceMapSlot
        
    in (v1, v2, v3, v4)
