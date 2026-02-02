{-# LANGUAGE Strict #-}
module Engine.Scene.Manager where

import UPrelude
import qualified Data.Vector as V
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Text as T
import Engine.Scene.Base
import Engine.Scene.Types
import Engine.Scene.Graph
import Engine.Scene.Batch
import Engine.Graphics.Camera
import Engine.Asset.Base (AssetId)
import Engine.Core.Monad
import Engine.Core.Log (LogCategory(..))
import Engine.Core.Log.Monad (logDebugM, logInfoM, logDebugSM, logInfoSM)

-- | Create a new scene
createScene ∷ Text → Camera2D → SceneManager → SceneManager
createScene sceneId camera manager =
    let scene = Scene
            { sceneId = sceneId
            , sceneCamera = camera
            , sceneActive = True
            }
        graph = createEmptySceneGraph
        (camX, camY) = camPosition camera
    in unsafePerformIO $ do
        logDebugSM CatScene "Scene created"
            [("sceneId", sceneId)
            ,("cameraX", T.pack $ show camX)
            ,("cameraY", T.pack $ show camY)
            ,("zoom", T.pack $ show $ camZoom camera)]
        pure $ manager
            { smScenes = Map.insert sceneId scene (smScenes manager)
            , smSceneGraphs = Map.insert sceneId graph (smSceneGraphs manager)
            , smDirtyScenes = Set.insert sceneId (smDirtyScenes manager)
            }

-- | Add object to scene
addObjectToScene ∷ Text → SceneNode → SceneManager → Maybe (ObjectId, SceneManager)
addObjectToScene sceneId node manager = unsafePerformIO $ do
    case Map.lookup sceneId (smSceneGraphs manager) of
        Nothing → pure Nothing
        Just graph → do
            let (objId, newGraph) = addNode node graph
                nodeTypeStr = case nodeType node of
                    SpriteObject → "sprite"
                    TextObject → "text"
                (x, y) = position (nodeTransform node)
                LayerId layer = nodeLayer node
            logDebugSM CatScene "Object added to scene"
                [("sceneId", sceneId)
                ,("objectId", T.pack $ show objId)
                ,("type", nodeTypeStr)
                ,("layer", T.pack $ show layer)
                ,("x", T.pack $ show x)
                ,("y", T.pack $ show y)]
            let updatedManager = manager
                    { smSceneGraphs = Map.insert sceneId newGraph (smSceneGraphs manager)
                    , smDirtyScenes = Set.insert sceneId (smDirtyScenes manager)
                    }
            pure $ Just (objId, updatedManager)

-- | Set active scene
setActiveScene ∷ Text → SceneManager → SceneManager
setActiveScene sceneId manager = unsafePerformIO $ do
    logInfoSM CatScene "Scene activated"
        [("sceneId", sceneId)]
    pure $ manager
        { smActiveScene = Just sceneId
        , smDirtyScenes = Set.insert sceneId (smDirtyScenes manager)
        }

-- | Update scene manager and regenerate batches
updateSceneManager ∷ Float → Float → SceneManager → EngineM ε σ SceneManager
updateSceneManager viewWidth viewHeight manager =
    case smActiveScene manager of
        Nothing → pure manager
        Just activeId → case Map.lookup activeId (smScenes manager) of
            Nothing → pure manager
            Just scene → do
                let graph = fromMaybe createEmptySceneGraph $ 
                           Map.lookup activeId (smSceneGraphs manager)
                    updatedGraph = updateWorldTransforms graph
                visibleObjects ← collectVisibleObjects updatedGraph 
                                   (sceneCamera scene) viewWidth viewHeight
                let updatedBatches = updateBatches visibleObjects (smBatchManager manager)
                pure $ manager
                    { smSceneGraphs = Map.insert activeId updatedGraph (smSceneGraphs manager)
                    , smBatchManager = updatedBatches
                    , smDirtyScenes = Set.delete activeId (smDirtyScenes manager)
                    }

-- | Get current render batches
getCurrentBatches ∷ SceneManager → V.Vector RenderBatch
getCurrentBatches = getSortedBatches . smBatchManager
