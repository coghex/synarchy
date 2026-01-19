{-# LANGUAGE Strict #-}
module Engine.Scene.Graph where

import UPrelude
import qualified Data.Vector as V
import qualified Data.Map as Map
import qualified Data.Set as Set
import Engine.Scene.Base
import Engine.Scene.Types
import Engine.Asset.Base (AssetId)
import Engine.Graphics.Vulkan.Types.Vertex (Vertex, Vec2(..), Vec4(..))
import Engine.Graphics.Camera
import Linear (M44, V3(..), (!*!), mkTransformation)
import Linear.Quaternion (axisAngle)

-- | Create a default scene node
createSceneNode ∷ ObjectType → SceneNode
createSceneNode objType = SceneNode
    { nodeId = ObjectId 0  -- Will be set when added to graph
    , nodeType = objType
    , nodeTransform = defaultTransform
    , nodeTexture = Nothing
    , nodeFont = Nothing
    , nodeVisible = True
    , nodeColor = Vec4 1.0 1.0 1.0 1.0
    , nodeUVRect = Nothing
    , nodeSize = (1.0, 1.0)
    , nodeChildren = Set.empty
    , nodeParent = Nothing
    , nodeDirty = True
    }

-- | Add a node to the scene graph
addNode ∷ SceneNode → SceneGraph → (ObjectId, SceneGraph)
addNode node graph = 
  let providedId = nodeId node
      ObjectId providedIdNum = providedId
      -- ensure nextId doest collide
      newNextId = max (sgNextId graph) (providedIdNum + 1)
      -- add node with its existing id
      updatedNode = node { nodeDirty = True }
      -- initial world transform
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

-- | Convert local transform to world matrix
localToWorldMatrix ∷ Transform2D → M44 Float
localToWorldMatrix transform =
    let (x, y) = position transform
        (sx, sy) = scale transform
        rot = rotation transform
        zIdx = zIndex transform
        
        -- Create transformation matrix
        translation = V3 x y zIdx
        scaleVec = V3 sx sy 1.0
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
