{-# LANGUAGE Strict, UnicodeSyntax #-}
module Engine.Scene.Batch.Sprite
  ( collectVisibleObjects
  , nodeToDrawable
  ) where

import UPrelude
import qualified Data.Vector as V
import qualified Data.Map as Map
import qualified Data.Text as T
import Data.Maybe (mapMaybe)
import Data.IORef (readIORef)
import Engine.Scene.Base (ObjectId, NodeType(..), LayerId, Transform2D(..))
import Engine.Scene.Types.Node (SceneNode(..))
import Engine.Scene.Types.Graph (SceneGraph(..))
import Engine.Scene.Types.Batch (DrawableObject(..))
import Engine.Scene.Batch.Visibility (isNodeVisible, isUILayer)
import Engine.Scene.Batch.Vertex (generateQuadVertices)
import Engine.Asset.Handle (TextureHandle, toInt)
import Engine.Graphics.Camera (Camera2D)
import Engine.Core.Monad
import Engine.Core.State (EngineEnv(..), EngineState(..), GraphicsState(..))
import Engine.Core.Log (LogCategory(..))
import Engine.Core.Log.Monad (logDebugM, logDebugSM)

collectVisibleObjects ∷ SceneGraph → Camera2D → Float → Float → EngineM ε σ (V.Vector DrawableObject)
collectVisibleObjects graph camera viewWidth viewHeight = do
    let allNodes = Map.elems (sgNodes graph)
        spriteNodes = filter (\n → nodeType n ≡ SpriteObject ∧ nodeVisible n) allNodes
        visibleNodes = filter (\n → isUILayer (nodeLayer n) ∨ isNodeVisible camera viewWidth viewHeight n) spriteNodes
        drawableObjs = mapMaybe nodeToDrawable visibleNodes

    logDebugSM CatScene "Visible object culling"
        [("totalObjects", T.pack $ show $ length spriteNodes)
        ,("visibleObjects", T.pack $ show $ length visibleNodes)]

    pure $ V.fromList drawableObjs

nodeToDrawable ∷ SceneNode → Maybe DrawableObject
nodeToDrawable node = do
    guard (nodeType node ≡ SpriteObject)
    textureHandle ← nodeTexture node

    -- Bake the STABLE texture-handle id; the bindless shader resolves it
    -- to a live slot at draw time (#286). -1 = default face map (generic
    -- scene sprites have no directional face map).
    let atlasId = fromIntegral (toInt textureHandle) ∷ Float
        fmSlot  = -1 ∷ Float

    let (v0,v1,v2,v3) = generateQuadVertices node atlasId fmSlot

    return DrawableObject
        { doId = nodeId node
        , doTexture = textureHandle
        , doV0 = v0
        , doV1 = v1
        , doV2 = v2
        , doV3 = v3
        , doZIndex = zIndex (nodeTransform node)
        , doLayer = nodeLayer node
        }
