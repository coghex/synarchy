{-# LANGUAGE Strict #-}
module Engine.Scene.Types.Graph
  ( SceneGraph(..)
  , createEmptySceneGraph
  ) where

import UPrelude
import qualified Data.Map as Map
import qualified Data.Set as Set
import Engine.Scene.Base (ObjectId)
import Engine.Scene.Types.Node (SceneNode, WorldTransform)

-----------------------------------------------------------
-- Scene graph types
-----------------------------------------------------------

data SceneGraph = SceneGraph
data SceneGraph = SceneGraph
    { sgNodes         ∷ Map.Map ObjectId SceneNode
    , sgRootNodes     ∷ Set.Set ObjectId
    , sgWorldTrans    ∷ Map.Map ObjectId WorldTransform
    , sgDirtyNodes    ∷ Set.Set ObjectId
    , sgNextId        ∷ Word32
    } deriving (Show)

-- | Create an empty scene graph
createEmptySceneGraph = SceneGraph
    { sgNodes = Map.empty
    , sgRootNodes = Set.empty
    , sgWorldTrans = Map.empty
    , sgDirtyNodes = Set.empty
    , sgNextId = 1
    }
