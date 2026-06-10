{-# LANGUAGE Strict #-}
-- | Operations on the flat scene-node registry. Node IDs are assigned
--   by the caller (Lua object IDs); transforms are absolute. Rotation
--   and scale take effect at vertex generation, not here.
module Engine.Scene.Graph
  ( addNode
  , withSceneGraph
  , withSceneGraphM
  , modifySceneNode
  , deleteSceneNode
  ) where

import UPrelude
import qualified Data.Map as Map
import Engine.Core.Monad
import Engine.Core.State (EngineState(..))
import Engine.Scene.Base
import Engine.Scene.Types

addNode ∷ SceneNode → SceneGraph → (ObjectId, SceneGraph)
addNode node graph =
  ( nodeId node
  , graph { sgNodes = Map.insert (nodeId node) node (sgNodes graph) } )

-- | Modify the active scene graph directly
-- Returns True if the scene graph was found (even if transform is identity)
withSceneGraph ∷ (SceneGraph → SceneGraph) → EngineM ε σ Bool
withSceneGraph f = do
    sceneMgr ← gets sceneManager
    case smActiveScene sceneMgr of
      Nothing → return False
      Just sceneId → case Map.lookup sceneId (smSceneGraphs sceneMgr) of
        Nothing → return False
        Just graph → do
          let updatedGraphs = Map.insert sceneId (f graph) (smSceneGraphs sceneMgr)
          modify $ \s → s { sceneManager = sceneMgr { smSceneGraphs = updatedGraphs } }
          return True

-- | Modify the active scene graph with a function that returns a result
-- Returns Nothing if no active scene/graph, otherwise returns Just result
withSceneGraphM ∷ (SceneGraph → (SceneGraph, a)) → EngineM ε σ (Maybe a)
withSceneGraphM f = do
    sceneMgr ← gets sceneManager
    case smActiveScene sceneMgr of
      Nothing → return Nothing
      Just sceneId → case Map.lookup sceneId (smSceneGraphs sceneMgr) of
        Nothing → return Nothing
        Just graph → do
          let (updatedGraph, result) = f graph
              updatedGraphs = Map.insert sceneId updatedGraph (smSceneGraphs sceneMgr)
          modify $ \s → s { sceneManager = sceneMgr { smSceneGraphs = updatedGraphs } }
          return (Just result)

-- | Modify a specific node in the active scene graph
-- Returns True only if the node was found and modified
modifySceneNode ∷ ObjectId → (SceneNode → SceneNode) → EngineM ε σ Bool
modifySceneNode objId f = do
    result ← withSceneGraphM $ \graph →
      case Map.lookup objId (sgNodes graph) of
        Nothing → (graph, False)
        Just node →
          let updatedGraph = graph
                { sgNodes = Map.insert objId (f node) (sgNodes graph) }
          in (updatedGraph, True)
    return $ fromMaybe False result

-- | Remove a node from the active scene graph
-- Returns True only if the node existed and was removed
deleteSceneNode ∷ ObjectId → EngineM ε σ Bool
deleteSceneNode objId = do
    result ← withSceneGraphM $ \graph →
      if Map.member objId (sgNodes graph)
        then
          let updatedGraph = graph
                { sgNodes = Map.delete objId (sgNodes graph) }
          in (updatedGraph, True)
        else (graph, False)
    return $ fromMaybe False result
