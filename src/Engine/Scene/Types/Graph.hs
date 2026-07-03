{-# LANGUAGE Strict #-}
module Engine.Scene.Types.Graph
  ( SceneGraph(..)
  , createEmptySceneGraph
  ) where

import UPrelude
import qualified Data.Map.Strict as Map
import Engine.Scene.Base (ObjectId)
import Engine.Scene.Types.Node (SceneNode)

-- | Flat registry of scene nodes. Nodes are positioned absolutely via
--   their own 'Transform2D'; rotation/scale are applied at vertex
--   generation ("Engine.Scene.Batch.Vertex"). There is deliberately no
--   parent\/child hierarchy or cached world-transform layer — the
--   original matrix scaffolding was never consumed by the render path.
data SceneGraph = SceneGraph
    { sgNodes ∷ Map.Map ObjectId SceneNode
    } deriving (Show)

createEmptySceneGraph ∷ SceneGraph
createEmptySceneGraph = SceneGraph
    { sgNodes = Map.empty
    }
