{-# LANGUAGE Strict #-}
module Engine.Scene.Batch.Sprite
  ( collectSpriteBatches
  , collectVisibleObjects
  , nodeToDrawable
  ) where

import UPrelude
import qualified Data.Vector as V
import qualified Data.Map as Map
import Data.Maybe (mapMaybe)
import Engine.Scene.Base (ObjectId, NodeType(..), LayerId)
import Engine.Scene.Types.Node (SceneNode(..), WorldTransform(..))
import Engine.Scene.Types.Graph (SceneGraph(..))
import Engine.Scene.Types.Batch (DrawableObject(..))
import Engine.Scene.Batch.Visibility (isNodeVisible, isUILayer)
import Engine.Scene.Batch.Vertex (generateQuadVertices)
import Engine.Asset.Handle (TextureHandle)
import Engine.Graphics.Camera (Camera2D)
import Engine.Graphics.Vulkan.Texture.Types (BindlessTextureSystem(..))
import Engine.Graphics.Vulkan.Texture.Bindless (getTextureSlotIndex)
import Engine.Core.Monad
import Engine.Core.State (EngineState(..), GraphicsState(..))

-- | Collect sprite batches from scene graph
collectSpriteBatches ∷ SceneGraph → Camera2D → Float → Float → EngineM ε σ (V.Vector DrawableObject)
collectSpriteBatches graph camera viewWidth viewHeight = do
    gs ← gets graphicsState
    let texSystem = textureSystem gs
        allNodes = Map.elems (sgNodes graph)
        spriteNodes = filter (\n → nodeType n ≡ SpriteObject && nodeVisible n) allNodes
        visibleSprites = filter (isNodeVisible camera viewWidth viewHeight) spriteNodes
        drawableObjs = mapMaybe (nodeToDrawable graph texSystem) visibleSprites
    pure $ V.fromList drawableObjs

-- | Collect visible objects from scene graph (with UI layer bypass)
collectVisibleObjects ∷ SceneGraph → Camera2D → Float → Float → EngineM ε σ (V.Vector DrawableObject)
collectVisibleObjects graph camera viewWidth viewHeight = do
    gs ← gets graphicsState
    let texSystem = textureSystem gs
        allNodes = Map.elems (sgNodes graph)
        spriteNodes = filter (\n → nodeType n ≡ SpriteObject && nodeVisible n) allNodes
        visibleNodes = filter (\n → isUILayer (nodeLayer n) || isNodeVisible camera viewWidth viewHeight n) spriteNodes
        drawableObjs = mapMaybe (nodeToDrawable graph texSystem) visibleNodes
    pure $ V.fromList drawableObjs

-- | Convert scene node to drawable object (sprites only)
nodeToDrawable ∷ SceneGraph → Maybe BindlessTextureSystem → SceneNode → Maybe DrawableObject
nodeToDrawable graph bts node = do
    guard (nodeType node ≡ SpriteObject)
    textureHandle ← nodeTexture node
    worldTrans ← Map.lookup (nodeId node) (sgWorldTrans graph)
    
    let atlasId = case bts of
                    Just bts' → fromIntegral $ getTextureSlotIndex textureHandle bts'
                    Nothing   → 0
    
    let vertices = generateQuadVertices node worldTrans atlasId
        
    return DrawableObject
        { doId = nodeId node
        , doTexture = textureHandle
        , doVertices = vertices
        , doZIndex = wtZIndex worldTrans
        , doLayer = nodeLayer node
        }
