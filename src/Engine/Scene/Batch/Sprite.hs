{-# LANGUAGE Strict, UnicodeSyntax #-}
module Engine.Scene.Batch.Sprite
  ( collectSpriteBatches
  , collectVisibleObjects
  , nodeToDrawable
  ) where

import UPrelude
import qualified Data.Vector as V
import qualified Data.Map as Map
import qualified Data.Text as T
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
import Engine.Core.Log (LogCategory(..))
import Engine.Core.Log.Monad (logDebugM, logDebugSM)

-- | Collect sprite batches from scene graph
collectSpriteBatches ∷ SceneGraph → Camera2D → Float → Float → EngineM ε σ (V.Vector DrawableObject)
collectSpriteBatches graph camera viewWidth viewHeight = do
    gs ← gets graphicsState
    let texSystem = textureSystem gs
        fmSlot = fromIntegral (defaultFaceMapSlot gs) ∷ Float
        allNodes = Map.elems (sgNodes graph)
        spriteNodes = filter (\n → nodeType n ≡ SpriteObject ∧ nodeVisible n) allNodes
        visibleSprites = filter (isNodeVisible camera viewWidth viewHeight) spriteNodes
        drawableObjs = mapMaybe (nodeToDrawable graph texSystem fmSlot) visibleSprites
    
    logDebugSM CatScene "Sprite batch generation"
        [("totalSprites", T.pack $ show $ length spriteNodes)
        ,("visibleSprites", T.pack $ show $ length visibleSprites)
        ,("drawableObjects", T.pack $ show $ length drawableObjs)]
    
    pure $ V.fromList drawableObjs

-- | Collect visible objects from scene graph (with UI layer bypass)
collectVisibleObjects ∷ SceneGraph → Camera2D → Float → Float → EngineM ε σ (V.Vector DrawableObject)
collectVisibleObjects graph camera viewWidth viewHeight = do
    gs ← gets graphicsState
    let texSystem = textureSystem gs
        fmSlot = fromIntegral (defaultFaceMapSlot gs) ∷ Float
        allNodes = Map.elems (sgNodes graph)
        spriteNodes = filter (\n → nodeType n ≡ SpriteObject ∧ nodeVisible n) allNodes
        visibleNodes = filter (\n → isUILayer (nodeLayer n) ∨ isNodeVisible camera viewWidth viewHeight n) spriteNodes
        drawableObjs = mapMaybe (nodeToDrawable graph texSystem fmSlot) visibleNodes
    
    logDebugSM CatScene "Visible object culling"
        [("totalObjects", T.pack $ show $ length spriteNodes)
        ,("visibleObjects", T.pack $ show $ length visibleNodes)]
    
    pure $ V.fromList drawableObjs

-- | Convert scene node to drawable object (sprites only)
nodeToDrawable ∷ SceneGraph → Maybe BindlessTextureSystem → Float → SceneNode → Maybe DrawableObject
nodeToDrawable graph bts fmSlot node = do
    guard (nodeType node ≡ SpriteObject)
    textureHandle ← nodeTexture node
    worldTrans ← Map.lookup (nodeId node) (sgWorldTrans graph)
    
    let atlasId = case bts of
                    Just bts' → fromIntegral $ getTextureSlotIndex textureHandle bts'
                    Nothing   → 0
    
    let vertices = generateQuadVertices node worldTrans atlasId fmSlot
        
    return DrawableObject
        { doId = nodeId node
        , doTexture = textureHandle
        , doVertices = vertices
        , doZIndex = wtZIndex worldTrans
        , doLayer = nodeLayer node
        }
