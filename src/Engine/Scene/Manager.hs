{-# LANGUAGE Strict #-}
module Engine.Scene.Manager where

import UPrelude
import qualified Data.Vector as V
import qualified Data.Map as Map
import qualified Data.Set as Set
import Engine.Scene.Base
import Engine.Scene.Types
import Engine.Scene.Graph
import Engine.Scene.Batch
import Engine.Graphics.Camera
import Engine.Asset.Base (AssetId)
import Engine.Core.Monad (EngineM')

-- | Create a new scene
createScene ∷ Text → Camera2D → SceneManager → SceneManager
createScene sceneId camera manager =
    let scene = Scene
            { sceneId = sceneId
            , sceneCamera = camera
            , sceneActive = True
            }
        graph = createEmptySceneGraph
    in manager
        { smScenes = Map.insert sceneId scene (smScenes manager)
        , smSceneGraphs = Map.insert sceneId graph (smSceneGraphs manager)
        , smDirtyScenes = Set.insert sceneId (smDirtyScenes manager)
        }

-- | Add object to scene
addObjectToScene ∷ Text → SceneNode → SceneManager → Maybe (ObjectId, SceneManager)
addObjectToScene sceneId node manager = do
    graph ← Map.lookup sceneId (smSceneGraphs manager)
    let (objId, newGraph) = addNode node graph
        updatedManager = manager
            { smSceneGraphs = Map.insert sceneId newGraph (smSceneGraphs manager)
            , smDirtyScenes = Set.insert sceneId (smDirtyScenes manager)
            }
    return (objId, updatedManager)

-- | Set active scene
setActiveScene ∷ Text → SceneManager → SceneManager
setActiveScene sceneId manager =
    manager
        { smActiveScene = Just sceneId
        , smDirtyScenes = Set.insert sceneId (smDirtyScenes manager)
        }

-- | Update scene manager and regenerate batches
updateSceneManager ∷ Float → Float → SceneManager → SceneManager
updateSceneManager viewWidth viewHeight manager =
    case smActiveScene manager of
        Nothing → manager
        Just activeId → case Map.lookup activeId (smScenes manager) of
            Nothing → manager
            Just scene → 
                let graph = fromMaybe createEmptySceneGraph $ 
                           Map.lookup activeId (smSceneGraphs manager)
                    updatedGraph = updateWorldTransforms graph
                    visibleObjects = collectVisibleObjects updatedGraph 
                                   (sceneCamera scene) viewWidth viewHeight
                    updatedBatches = updateBatches visibleObjects (smBatchManager manager)
                in manager
                    { smSceneGraphs = Map.insert activeId updatedGraph (smSceneGraphs manager)
                    , smBatchManager = updatedBatches
                    , smDirtyScenes = Set.delete activeId (smDirtyScenes manager)
                    }

-- | Get current render batches
getCurrentBatches ∷ SceneManager → V.Vector RenderBatch
getCurrentBatches = getSortedBatches . smBatchManager
