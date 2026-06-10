{-# LANGUAGE Strict #-}
module Engine.Scene.Types.Node
  ( SceneNode(..)
  , createSceneNode
  ) where

import UPrelude
import Engine.Scene.Base
import Engine.Asset.Handle (TextureHandle, FontHandle)
import Engine.Graphics.Vulkan.Types.Vertex (Vec2(..), Vec4(..))

data SceneNode = SceneNode
    { nodeId         ∷ ObjectId
    , nodeType       ∷ NodeType
    , nodeTransform  ∷ Transform2D
    , nodeLayer      ∷ LayerId
    , nodeTexture    ∷ Maybe TextureHandle
    , nodeFont       ∷ Maybe FontHandle
    , nodeFontSize   ∷ Maybe Float
    , nodeText       ∷ Maybe Text
    , nodeVisible    ∷ Bool
    , nodeColor      ∷ Vec4
    , nodeUVRect     ∷ Maybe (Vec2, Vec2)
    , nodeSize       ∷ (Float, Float)
    } deriving (Show)

createSceneNode ∷ NodeType → SceneNode
createSceneNode objType = SceneNode
    { nodeId = ObjectId 0
    , nodeType = objType
    , nodeTransform = defaultTransform
    , nodeTexture = Nothing
    , nodeLayer = LayerId 0
    , nodeFont = Nothing
    , nodeFontSize = Nothing
    , nodeText = Nothing
    , nodeVisible = True
    , nodeColor = Vec4 1.0 1.0 1.0 1.0
    , nodeUVRect = Nothing
    , nodeSize = (1.0, 1.0)
    }
