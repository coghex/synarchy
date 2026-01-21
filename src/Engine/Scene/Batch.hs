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
import Engine.Graphics.Font.Data
import Engine.Graphics.Font.Draw
import Engine.Core.Monad
import Engine.Core.State
import Engine.Core.Error.Exception

-- | Batch manager for 2D rendering
collectSpriteBatches ∷ SceneGraph → Camera2D → Float → Float → V.Vector DrawableObject
collectSpriteBatches graph camera viewWidth viewHeight =
    let allNodes = Map.elems (sgNodes graph)
        spriteNodes = filter (\n → nodeType n ≡ SpriteObject && nodeVisible n) allNodes
        visibleSprites = filter (isNodeVisible camera viewWidth viewHeight) spriteNodes
        drawableObjs = mapMaybe (nodeToDrawable graph) visibleSprites
    in V.fromList drawableObjs

-- | collect text drawable objects from scene graph
collectTextBatches ∷ SceneGraph → EngineM ε σ (V.Vector TextRenderBatch)
collectTextBatches graph = do
  let allNodes = Map.elems (sgNodes graph)
      textNodes = filter (\n → nodeType n ≡ TextObject && nodeVisible n) allNodes
  logDebug $ "Collecting text batches from " ⧺ show (length textNodes) ⧺ " text nodes."
  forM_ textNodes $ \node → do
      logDebug $ " Text Node " ⧺ show (nodeId node) ⧺
                 " font =" ⧺ show (nodeFont node) ⧺
                 " text =" ⧺ show (nodeText node) ⧺
                 " visible =" ⧺ show (nodeVisible node)
  gs ← gets graphicsState
  let cache = fontCache gs
  logDebug $ "Font cache contains " ⧺ show (Map.size (fcFonts cache)) ⧺ " fonts."
  let grouped = groupByFontAndLayer textNodes
  logDebug $ "grouped into " ⧺ show (length grouped) ⧺ " font/layer groups."
  batches ← forM grouped $ \((fontHandle, layerId), nodes) → do
    logDebug $ "processing group: font=" ⧺ show fontHandle ⧺ " layer=" ⧺ show layerId
             ⧺ " nodes=" ⧺ show (length nodes)
    case Map.lookup fontHandle (fcFonts cache) of
      Nothing → do
        logDebug $ " Font " ⧺ show fontHandle ⧺ " not found in cache."
        return Nothing
      Just atlas → do
          logDebug $ " Found font atlas"
          allInstances ← fmap V.concat $ forM nodes $ \node → do
              let worldTransResult = Map.lookup (nodeId node) (sgWorldTrans graph)
              logDebug $ "      Node " ⧺ show (nodeId node) ⧺
                         " world trans= " ⧺ show (isJust worldTransResult)
              case (nodeText node, Map.lookup (nodeId node) (sgWorldTrans graph)) of
                  (Just text, Just worldTrans) → do
                      let (x,y) = wtPosition worldTrans
                          Vec4 r g b a = nodeColor node
                          color = (r, g, b, a)
                      logDebug $ "      Laying out text: \"" ⧺ show text ⧺ "\" at (" ⧺ show x ⧺ "," ⧺ show y ⧺ ")"
                      let instances = layoutText atlas x y text color
                      logDebug $ "      Created " ⧺ show (V.length instances) ⧺ " glyph instances."
                      return instances
                  (Nothing, _) → do
                      logDebug $ "      No text for node " ⧺ show (nodeId node)
                      return V.empty
                  (_, Nothing) → do
                      logDebug $ "      No world transform for node"
                      return V.empty
          logDebug $ "  Total instances for batch: " ⧺ show (V.length allInstances)
          return $ Just $ TextRenderBatch
              { trbFont = fontHandle
              , trbLayer = layerId
              , trbInstances = allInstances
              , trbObjects = V.fromList $ map nodeId nodes }
  let result = V.fromList $ catMaybes batches
  logDebug $ "collectTextBatches returning " ⧺ show (V.length result) ⧺ " text render batches."
  return result

-- | Group text nodes by font and layer
groupByFontAndLayer ∷ [SceneNode] → [((FontHandle, LayerId), [SceneNode])]
groupByFontAndLayer nodes =
  let nodesWithFont = filter (isJust . nodeFont) nodes
      sorted = List.sortOn (\n → (nodeFont n, nodeLayer n, zIndex $ nodeTransform n)) nodesWithFont
      grouped = List.groupBy (\a b → (nodeFont a, nodeLayer a) ≡ (nodeFont b, nodeLayer b)) sorted
      keyed = map (\grp → case grp of
                      [] → error "Empty group"
                      (n:_) → ((fromJust $ nodeFont n, nodeLayer n), grp)) grouped
  in keyed

-- | Collect visible objects from scene graph (this might be obsolete)
collectVisibleObjects ∷ SceneGraph → Camera2D → Float → Float → V.Vector DrawableObject
collectVisibleObjects graph camera viewWidth viewHeight =
    let allNodes = Map.elems (sgNodes graph)
        spriteNodes = filter (\n → nodeType n ≡ SpriteObject && nodeVisible n) allNodes
        visibleNodes = filter (isNodeVisible camera viewWidth viewHeight) spriteNodes
        drawableObjs = mapMaybe (nodeToDrawable graph) visibleNodes
    in V.fromList drawableObjs

-- | conversion function
convertToTextBatches ∷ V.Vector TextRenderBatch → V.Vector TextBatch
convertToTextBatches = V.map $ \trb → TextBatch
    { tbFontHandle = trbFont trb
    , tbInstances = trbInstances trb }

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

-- | Convert scene node to drawable object (sprites only)
nodeToDrawable ∷ SceneGraph → SceneNode → Maybe DrawableObject
nodeToDrawable graph node = do
    guard (nodeType node ≡ SpriteObject)
    textureId ← nodeTexture node
    worldTrans ← Map.lookup (nodeId node) (sgWorldTrans graph)
    
    let vertices = generateQuadVertices node worldTrans
        layerId = nodeLayer node
        
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
        v1 = Vertex (Vec2 (posX - halfX) (posY - halfY)) (Vec2 (x uvMin) (y uvMin)) color atlasId
        v2 = Vertex (Vec2 (posX + halfX) (posY - halfY)) (Vec2 (x uvMax) (y uvMin)) color atlasId
        v3 = Vertex (Vec2 (posX + halfX) (posY + halfY)) (Vec2 (x uvMax) (y uvMax)) color atlasId
        v4 = Vertex (Vec2 (posX - halfX) (posY + halfY)) (Vec2 (x uvMin) (y uvMax)) color atlasId
        
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
