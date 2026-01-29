{-# LANGUAGE Strict #-}
module Engine.Scene.Types.Node
  ( SceneNode(..)
  , WorldTransform(..)
  , createSceneNode
  ) where

import UPrelude
import qualified Data.Set as Set
import Engine.Scene.Base
import Engine.Asset.Handle (TextureHandle, FontHandle)
import Engine.Graphics.Vulkan.Types.Vertex (Vec2(..), Vec4(..))
import Linear (M44)

-- | Represents a drawable object in the scene
data SceneNode = SceneNode
    { nodeId         ∷ ObjectId
    , nodeType       ∷ NodeType
    , nodeTransform  ∷ Transform2D
    , nodeLayer      ∷ LayerId
    , nodeTexture    ∷ Maybe TextureHandle
    , nodeFont       ∷ Maybe FontHandle
    , nodeText       ∷ Maybe Text
    , nodeVisible    ∷ Bool
    , nodeColor      ∷ Vec4
    , nodeUVRect     ∷ Maybe (Vec2, Vec2)
    , nodeSize       ∷ (Float, Float)
    , nodeChildren   ∷ Set.Set ObjectId
    , nodeParent     ∷ Maybe ObjectId
    , nodeDirty      ∷ Bool
    } deriving (Show)

-- | World-space transform matrix
data WorldTransform = WorldTransform
    { wtMatrix    ∷ M44 Float
    , wtPosition  ∷ (Float, Float)
    , wtScale     ∷ (Float, Float)
    , wtRotation  ∷ Float
    , wtZIndex    ∷ Float
    } deriving (Show)

-- | Create a default scene node
createSceneNode ∷ NodeType → SceneNode
createSceneNode objType = SceneNode
    { nodeId = ObjectId 0
    , nodeType = objType
    , nodeTransform = defaultTransform
    , nodeTexture = Nothing
    , nodeLayer = LayerId 0
    , nodeFont = Nothing
    , nodeText = Nothing
    , nodeVisible = True
    , nodeColor = Vec4 1.0 1.0 1.0 1.0
    , nodeUVRect = Nothing
    , nodeSize = (1.0, 1.0)
    , nodeChildren = Set.empty
    , nodeParent = Nothing
    , nodeDirty = True
    }
