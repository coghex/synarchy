{-# LANGUAGE Strict #-}
module Engine.Scene.Batch where

import UPrelude
import qualified Data.Vector as V
import qualified Data.Map as Map
import qualified Data.List as List
import qualified Data.Set as Set
import Data.Maybe (mapMaybe)
import Engine.Scene.Base
import Engine.Scene.Graph
import Engine.Scene.Types
import Engine.Asset.Base
import Engine.Asset.Types
import Engine.Graphics.Vulkan.Types.Vertex (Vertex(..), Vec2(..), Vec4(..))
import Engine.Graphics.Camera

-- | Collect visible objects from scene graph
collectVisibleObjects ∷ SceneGraph → Camera2D → Float → Float → V.Vector DrawableObject
collectVisibleObjects graph camera viewWidth viewHeight =
    let allNodes = Map.elems (sgNodes graph)
        visibleNodes = filter (isNodeVisible camera viewWidth viewHeight) allNodes
        drawableObjs = mapMaybe (nodeToDrawable graph) visibleNodes
    in V.fromList drawableObjs

-- | Check if a node is visible within camera frustum
isNodeVisible ∷ Camera2D → Float → Float → SceneNode → Bool
isNodeVisible camera viewWidth viewHeight node =
    if not (nodeVisible node)
    then False
    else
        let (camX, camY) = camPosition camera
            zoom = camZoom camera
            (nodeX, nodeY) = position (nodeTransform node)
            (sizeX, sizeY) = nodeSize node
            
            -- Simple AABB frustum culling
            left = camX - (viewWidth * zoom * 0.5)
            right = camX + (viewWidth * zoom * 0.5)
            bottom = camY - (viewHeight * zoom * 0.5)
            top = camY + (viewHeight * zoom * 0.5)
            
            nodeLeft = nodeX - sizeX * 0.5
            nodeRight = nodeX + sizeX * 0.5
            nodeBottom = nodeY - sizeY * 0.5
            nodeTop = nodeY + sizeY * 0.5
            
        in not (nodeRight < left || nodeLeft > right || 
                nodeTop < bottom || nodeBottom > top)

-- | Convert scene node to drawable object
nodeToDrawable ∷ SceneGraph → SceneNode → Maybe DrawableObject
nodeToDrawable graph node = do
    textureId ← nodeTexture node
    worldTrans ← Map.lookup (nodeId node) (sgWorldTrans graph)
    
    let vertices = generateQuadVertices node worldTrans
        layerId = LayerId 0  -- Default layer, can be extended
        
    return DrawableObject
        { doId = nodeId node
        , doTexture = textureId
        , doVertices = vertices
        , doZIndex = wtZIndex worldTrans
        , doLayer = layerId
        }

-- | Generate quad vertices for a scene node
generateQuadVertices ∷ SceneNode → WorldTransform → V.Vector Vertex
generateQuadVertices node worldTrans =
    let (sizeX, sizeY) = nodeSize node
        (posX, posY) = wtPosition worldTrans
        color = nodeColor node
        
        -- Default UV coordinates (can be overridden by nodeUVRect)
        (uvMin, uvMax) = case nodeUVRect node of
            Just (minUV, maxUV) → (minUV, maxUV)
            Nothing → (Vec2 0.0 0.0, Vec2 1.0 1.0)
        
        -- Quad corners in local space
        halfX = sizeX * 0.5
        halfY = sizeY * 0.5
        
        -- Atlas ID (default to 0 if not specified)
        atlasId = 0.0  -- You might want to add this to SceneNode later
        
        -- Vertices: bottom-left, bottom-right, top-right, top-left
        -- Note: This creates 6 vertices for a quad (2 triangles)
        v1 = Vertex (Vec2 (posX - halfX) (posY - halfY)) (Vec2 (x uvMin) (y uvMax)) color atlasId
        v2 = Vertex (Vec2 (posX + halfX) (posY - halfY)) (Vec2 (x uvMax) (y uvMax)) color atlasId
        v3 = Vertex (Vec2 (posX + halfX) (posY + halfY)) (Vec2 (x uvMax) (y uvMin)) color atlasId
        v4 = Vertex (Vec2 (posX - halfX) (posY + halfY)) (Vec2 (x uvMin) (y uvMin)) color atlasId
        
        -- Create two triangles: (v1,v2,v3) and (v1,v3,v4)
    in V.fromList [v1, v2, v3, v1, v3, v4]

-- | Update batches with new visible objects
updateBatches ∷ V.Vector DrawableObject → BatchManager → BatchManager
updateBatches objects manager =
    let groupedObjs = groupByTextureAndLayer objects
        newBatches = Map.fromList $ map createBatch groupedObjs
        dirtyKeys = Map.keysSet newBatches
    in manager
        { bmBatches = newBatches
        , bmVisibleObjs = objects
        , bmDirtyBatches = dirtyKeys
        }

-- | Group drawable objects by texture and layer
groupByTextureAndLayer ∷ V.Vector DrawableObject → [((TextureHandle, LayerId), V.Vector DrawableObject)]
groupByTextureAndLayer objects =
    let objList = V.toList objects
        grouped = List.groupBy (\a b → (doTexture a, doLayer a) ≡ (doTexture b, doLayer b)) $
                  List.sortOn (\obj → (doTexture obj, doLayer obj, doZIndex obj)) objList
        keyed = map (\grp → case grp of
                        [] → error "Empty group"
                        (obj:_) → ((doTexture obj, doLayer obj), V.fromList grp)) grouped
    in keyed

-- | Create a render batch from grouped objects
createBatch ∷ ((TextureHandle, LayerId), V.Vector DrawableObject) → ((TextureHandle, LayerId), RenderBatch)
createBatch ((textureId, layerId), objects) =
    let allVertices = V.concatMap doVertices objects
        objectIds = V.map doId objects
        batch = RenderBatch
            { rbTexture = textureId
            , rbLayer = layerId
            , rbVertices = allVertices
            , rbObjects = objectIds
            , rbDirty = True
            }
    in ((textureId, layerId), batch)

-- | Get render batches sorted by layer and z-index
getSortedBatches ∷ BatchManager → V.Vector RenderBatch
getSortedBatches manager =
    let batches = Map.elems (bmBatches manager)
        sorted = List.sortOn (\batch → (rbLayer batch, averageZIndex batch)) batches
    in V.fromList sorted
  where
    averageZIndex batch =
        let objects = V.mapMaybe (\objId → 
                V.find (\obj → doId obj ≡ objId) (bmVisibleObjs manager)) 
                (rbObjects batch)
            zIndices = V.map doZIndex objects
        in if V.null zIndices then 0.0 else V.sum zIndices / fromIntegral (V.length zIndices)
