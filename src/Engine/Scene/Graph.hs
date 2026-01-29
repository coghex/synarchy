{-# LANGUAGE Strict #-}
module Engine.Scene.Graph 
  ( addNode
  , addChild
  , withSceneGraph
  , withSceneGraphM
  , modifySceneNode
  , deleteSceneNode
  , localToWorldMatrix
  , updateWorldTransforms
  , updateNodeTransform
  ) where

import UPrelude
import qualified Data.Map as Map
import qualified Data.Set as Set
import Engine.Core.Monad
import Engine.Core.State (EngineState(..))
import Engine.Scene.Base
import Engine.Scene.Types
import Linear (M44, V3(..), (!*!), mkTransformation)
import Linear.Quaternion (axisAngle)

-- | Add a node to the scene graph
addNode ∷ SceneNode → SceneGraph → (ObjectId, SceneGraph)
addNode node graph = 
  let providedId = nodeId node
      ObjectId providedIdNum = providedId
      newNextId = max (sgNextId graph) (providedIdNum + 1)
      updatedNode = node { nodeDirty = True }
      initialWorldTrans = WorldTransform
        { wtMatrix = localToWorldMatrix (nodeTransform node)
        , wtPosition = position (nodeTransform node)
        , wtScale = scale (nodeTransform node)
        , wtRotation = rotation (nodeTransform node)
        , wtZIndex = zIndex (nodeTransform node)
        }
      newGraph = graph
        { sgNodes = Map.insert providedId updatedNode (sgNodes graph)
        , sgRootNodes = if isNothing (nodeParent updatedNode)
                       then Set.insert providedId (sgRootNodes graph)
                       else sgRootNodes graph
        , sgWorldTrans = Map.insert providedId initialWorldTrans (sgWorldTrans graph)
        , sgDirtyNodes = Set.insert providedId (sgDirtyNodes graph)
        , sgNextId = newNextId
        }
      in (providedId, newGraph)

-- | Add a child node to a parent
addChild ∷ ObjectId → ObjectId → SceneGraph → SceneGraph
addChild parentId childId graph =
    case (Map.lookup parentId (sgNodes graph), Map.lookup childId (sgNodes graph)) of
        (Just parent, Just child) →
            let updatedParent = parent { nodeChildren = Set.insert childId (nodeChildren parent) }
                updatedChild = child { nodeParent = Just parentId }
                newGraph = graph
                    { sgNodes = Map.insert parentId updatedParent $
                               Map.insert childId updatedChild (sgNodes graph)
                    , sgRootNodes = Set.delete childId (sgRootNodes graph)
                    , sgDirtyNodes = Set.insert childId (sgDirtyNodes graph)
                    }
            in newGraph
        _ → graph

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
                { sgNodes = Map.insert objId (f node) (sgNodes graph)
                , sgDirtyNodes = Set.insert objId (sgDirtyNodes graph)
                }
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
                { sgNodes      = Map.delete objId (sgNodes graph)
                , sgWorldTrans = Map.delete objId (sgWorldTrans graph)
                , sgDirtyNodes = Set.delete objId (sgDirtyNodes graph)
                , sgRootNodes  = Set.delete objId (sgRootNodes graph)
                }
          in (updatedGraph, True)
        else (graph, False)
    return $ fromMaybe False result

-- | Convert local transform to world matrix
localToWorldMatrix ∷ Transform2D → M44 Float
localToWorldMatrix transform =
    let (x, y) = position transform
        (sx, sy) = scale transform
        rot = rotation transform
        zIdx = zIndex transform
        translation = V3 x y zIdx
        rotationQuat = axisAngle (V3 0 0 1) rot
    in mkTransformation rotationQuat translation

-- | Update world transforms for dirty nodes
updateWorldTransforms ∷ SceneGraph → SceneGraph
updateWorldTransforms graph =
    let updatedGraph = foldr updateNodeTransform graph (Set.toList $ sgDirtyNodes graph)
    in updatedGraph { sgDirtyNodes = Set.empty }

updateNodeTransform ∷ ObjectId → SceneGraph → SceneGraph
updateNodeTransform nodeId graph =
    case Map.lookup nodeId (sgNodes graph) of
        Nothing → graph
        Just node →
            let localMatrix = localToWorldMatrix (nodeTransform node)
                worldMatrix = case nodeParent node of
                    Nothing → localMatrix
                    Just parentId → case Map.lookup parentId (sgWorldTrans graph) of
                        Nothing → localMatrix
                        Just parentWorld → wtMatrix parentWorld !*! localMatrix
                worldTrans = WorldTransform
                    { wtMatrix = worldMatrix
                    , wtPosition = position (nodeTransform node)
                    , wtScale = scale (nodeTransform node)
                    , wtRotation = rotation (nodeTransform node)
                    , wtZIndex = zIndex (nodeTransform node)
                    }
            in graph { sgWorldTrans = Map.insert nodeId worldTrans (sgWorldTrans graph) }
