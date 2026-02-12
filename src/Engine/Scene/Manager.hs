{-# LANGUAGE Strict, UnicodeSyntax #-}
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

-----------------------------------------------------------
-- Scene management
-----------------------------------------------------------

createScene ∷ Text → Camera2D → SceneManager → SceneManager
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
addObjectToScene sceneId node manager = do
    graph ← Map.lookup sceneId (smSceneGraphs manager)
    let (objId, newGraph) = addNode node graph
        updatedManager = manager
            { smSceneGraphs = Map.insert sceneId newGraph (smSceneGraphs manager)
            , smDirtyScenes = Set.insert sceneId (smDirtyScenes manager)
            }
    return (objId, updatedManager)

setActiveScene ∷ Text → SceneManager → SceneManager
setActiveScene sceneId manager =
    manager
        { smActiveScene = Just sceneId
        , smDirtyScenes = Set.insert sceneId (smDirtyScenes manager)
        }

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

getCurrentBatches ∷ SceneManager → V.Vector RenderBatch
getCurrentBatches = getSortedBatches . smBatchManager
getCurrentBatches = getSortedBatches . smBatchManager
