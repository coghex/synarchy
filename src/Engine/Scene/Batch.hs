{-# LANGUAGE Strict #-}
module Engine.Scene.Batch where

import UPrelude
import qualified Data.Vector as V
import qualified Data.Map as Map
import qualified Data.List as List
import qualified Data.Set as Set
import Data.Maybe (mapMaybe)
import Data.IORef (readIORef)
import Engine.Scene.Base
import Engine.Scene.Graph
import Engine.Scene.Types
import Engine.Asset.Base
import Engine.Asset.Types
import Engine.Asset.Handle
import Engine.Graphics.Vulkan.Types.Vertex (Vertex(..), Vec2(..), Vec4(..))
import Engine.Graphics.Vulkan.Texture.Types (BindlessTextureSystem(..))
import Engine.Graphics.Vulkan.Texture.Bindless (getTextureSlotIndex)
import Engine.Graphics.Camera
import Engine.Graphics.Font.Data
import Engine.Graphics.Font.Draw
import Engine.Core.Monad
import Engine.Core.State
import Engine.Core.Error.Exception

-- | Batch manager for 2D rendering
-- Now runs in EngineM to access bindless texture system
collectSpriteBatches ∷ SceneGraph → Camera2D → Float → Float → EngineM ε σ (V.Vector DrawableObject)
collectSpriteBatches graph camera viewWidth viewHeight = do
    gs ← gets graphicsState
    let texSystem = textureSystem gs
        allNodes = Map.elems (sgNodes graph)
        spriteNodes = filter (\n → nodeType n ≡ SpriteObject && nodeVisible n) allNodes
        visibleSprites = filter (isNodeVisible camera viewWidth viewHeight) spriteNodes
        drawableObjs = mapMaybe (nodeToDrawable graph texSystem) visibleSprites
    pure $ V.fromList drawableObjs

collectTextBatches ∷ SceneGraph → Float → Float
  → EngineM ε σ (V.Vector TextRenderBatch)
collectTextBatches graph screenW screenH = do
  let allNodes = Map.elems (sgNodes graph)
      textNodes = filter (\n → nodeType n ≡ TextObject && nodeVisible n) allNodes
  cacheRef ← asks fontCacheRef
  cache ← liftIO $ readIORef cacheRef
  let grouped = groupByFontAndLayer textNodes
  batches ← forM grouped $ \((fontHandle, layerId), nodes) → do
    case Map.lookup fontHandle (fcFonts cache) of
      Nothing → do
        logDebug $ " Font " ⧺ show fontHandle ⧺ " not found in cache."
        return Nothing
      Just atlas → do
          allInstances ← fmap V.concat $ forM nodes $ \node → do
              case (nodeText node, Map.lookup (nodeId node) (sgWorldTrans graph)) of
                  (Just text, Just worldTrans) → do
                      let (x,y) = wtPosition worldTrans
                          Vec4 r g b a = nodeColor node
                          color = (r, g, b, a)
                          isUILayer = let (LayerId l) = layerId in l >= 10
                      let instances = if isUILayer
                                      then layoutTextUI atlas x y text color
                                      else layoutText atlas x y screenW screenH text color
                      return instances
                  (Nothing, _) → do
                      logDebug $ "      No text for node " ⧺ show (nodeId node)
                      return V.empty
                  (_, Nothing) → do
                      logDebug $ "      No world transform for node"
                      return V.empty
          return $ Just $ TextRenderBatch
              { trbFont = fontHandle
              , trbLayer = layerId
              , trbInstances = allInstances
              , trbObjects = V.fromList $ map nodeId nodes }
  let result = V.fromList $ catMaybes batches
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

-- | Collect visible objects from scene graph
collectVisibleObjects ∷ SceneGraph → Camera2D → Float → Float → EngineM ε σ (V.Vector DrawableObject)
collectVisibleObjects graph camera viewWidth viewHeight = do
    gs ← gets graphicsState
    let texSystem = textureSystem gs
        allNodes = Map.elems (sgNodes graph)
        spriteNodes = filter (\n → nodeType n ≡ SpriteObject && nodeVisible n) allNodes
        -- Skip frustum culling for UI layers (layer >= 10)
        visibleNodes = filter (\n → isUILayer (nodeLayer n) || isNodeVisible camera viewWidth viewHeight n) spriteNodes
        drawableObjs = mapMaybe (nodeToDrawable graph texSystem) visibleNodes
    pure $ V.fromList drawableObjs
  where
    isUILayer (LayerId l) = l >= 10

-- | conversion function
convertToTextBatches ∷ V.Vector TextRenderBatch → V.Vector TextBatch
convertToTextBatches = V.map $ \trb → TextBatch
    { tbFontHandle = trbFont trb
    , tbInstances = trbInstances trb
    , tbLayer = trbLayer trb }

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
-- Now takes TextureSystem to look up bindless slot
nodeToDrawable ∷ SceneGraph → Maybe BindlessTextureSystem → SceneNode → Maybe DrawableObject
nodeToDrawable graph bts node = do
    guard (nodeType node ≡ SpriteObject)
    textureHandle ← nodeTexture node
    worldTrans ← Map.lookup (nodeId node) (sgWorldTrans graph)
    
    -- Look up bindless slot for this texture
    let atlasId = case bts of
                    Just bts' → fromIntegral $ getTextureSlotIndex textureHandle bts'
                    Nothing   → 0  -- Fallback if no bindless system
    
    let vertices = generateQuadVertices node worldTrans atlasId
        layerId = nodeLayer node
        
    return DrawableObject
        { doId = nodeId node
        , doTexture = textureHandle
        , doVertices = vertices
        , doZIndex = wtZIndex worldTrans
        , doLayer = layerId
        }

-- | Generate quad vertices for a scene node
generateQuadVertices ∷ SceneNode → WorldTransform → Float → V.Vector Vertex
generateQuadVertices node worldTrans atlasId =
    let (sizeX, sizeY) = nodeSize node
        (posX, posY) = wtPosition worldTrans
        color = nodeColor node
        
        (uvMin, uvMax) = case nodeUVRect node of
            Just (minUV, maxUV) → (minUV, maxUV)
            Nothing → (Vec2 0.0 0.0, Vec2 1.0 1.0)
        
        halfX = sizeX * 0.5
        halfY = sizeY * 0.5
        
        v1 = Vertex (Vec2 (posX - halfX) (posY - halfY)) (Vec2 (x uvMin) (y uvMin)) color atlasId
        v2 = Vertex (Vec2 (posX + halfX) (posY - halfY)) (Vec2 (x uvMax) (y uvMin)) color atlasId
        v3 = Vertex (Vec2 (posX + halfX) (posY + halfY)) (Vec2 (x uvMax) (y uvMax)) color atlasId
        v4 = Vertex (Vec2 (posX - halfX) (posY + halfY)) (Vec2 (x uvMin) (y uvMax)) color atlasId
        
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

-- | update batch manager with text batches
updateTextBatches ∷ V.Vector TextRenderBatch → BatchManager → BatchManager
updateTextBatches textBatches manager =
    let groupedText = V.foldl' (\acc trb →
            let key = (trbFont trb, trbLayer trb)
            in Map.insert key trb acc) Map.empty textBatches
    in manager { bmTextBatches = groupedText }

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

-- | Build layered batches from sprite and text batches
buildLayeredBatches ∷ BatchManager → BatchManager
buildLayeredBatches manager =
    let sortedSprites = getSortedBatches manager
        sortedText = List.sortOn trbLayer $ Map.elems (bmTextBatches manager)
        
        spriteLayers = V.foldl' (\acc batch →
            let layer = rbLayer batch
                existing = Map.findWithDefault V.empty layer acc
            in Map.insert layer (V.snoc existing (SpriteItem batch)) acc
          ) Map.empty sortedSprites
        
        textLayers = foldl' (\acc batch →
            let layer = trbLayer batch
                existing = Map.findWithDefault V.empty layer acc
            in Map.insert layer (V.snoc existing (TextItem batch)) acc
          ) Map.empty sortedText
        
        allLayers = Map.unionWith (V.++) spriteLayers textLayers
        
    in manager { bmLayeredBatches = allLayers }
