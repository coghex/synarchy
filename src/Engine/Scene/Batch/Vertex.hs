{-# LANGUAGE Strict #-}
module Engine.Scene.Batch.Vertex
  ( generateQuadVertices
  ) where

import UPrelude
import Engine.Scene.Base (Transform2D(..))
import Engine.Scene.Types.Node (SceneNode(..))
import Engine.Graphics.Vulkan.Types.Vertex (Vertex(..), Vec2(..), mkVertex)

-- | Build the four corners of a node's quad. Position, scale and
--   rotation all come from the node's 'Transform2D': scale multiplies
--   the node size, rotation spins the corners about the quad centre.
--   With rotation 0 the basis is (1,0) and this reduces exactly to the
--   axis-aligned arithmetic of the unrotated case.
generateQuadVertices ∷ SceneNode → Float → Float → (Vertex, Vertex, Vertex, Vertex)
generateQuadVertices node atlasId faceMapSlot =
    let t = nodeTransform node
        (sizeX, sizeY) = nodeSize node
        (posX, posY) = position t
        (sclX, sclY) = scale t
        rot = rotation t
        color = nodeColor node

        (uvMin, uvMax) = case nodeUVRect node of
            Just (minUV, maxUV) → (minUV, maxUV)
            Nothing → (Vec2 0.0 0.0, Vec2 1.0 1.0)

        halfX = sizeX * sclX * 0.5
        halfY = sizeY * sclY * 0.5

        (c, s) = if rot ≡ 0 then (1, 0) else (cos rot, sin rot)
        corner dx dy = Vec2 (posX + dx * c - dy * s) (posY + dx * s + dy * c)

        v1 = mkVertex (corner (-halfX) (-halfY)) (Vec2 (x uvMin) (y uvMin)) color atlasId faceMapSlot
        v2 = mkVertex (corner   halfX  (-halfY)) (Vec2 (x uvMax) (y uvMin)) color atlasId faceMapSlot
        v3 = mkVertex (corner   halfX    halfY)  (Vec2 (x uvMax) (y uvMax)) color atlasId faceMapSlot
        v4 = mkVertex (corner (-halfX)   halfY)  (Vec2 (x uvMin) (y uvMax)) color atlasId faceMapSlot

    in (v1, v2, v3, v4)
