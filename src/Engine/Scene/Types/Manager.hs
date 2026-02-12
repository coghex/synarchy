{-# LANGUAGE Strict #-}
module Engine.Scene.Types.Manager
  ( Scene(..)
  , SceneManager(..)
  , createSceneManager
  , createEmptyScene
  ) where

import UPrelude
import qualified Data.Map as Map
import qualified Data.Set as Set
import Engine.Scene.Types.Graph (SceneGraph)
import Engine.Scene.Types.Batch (BatchManager, createBatchManager)
import Engine.Graphics.Camera (Camera2D, defaultCamera)

-----------------------------------------------------------
-- Scene manager types
-----------------------------------------------------------

data Scene = Scene
data Scene = Scene
    { sceneId     ∷ Text
    , sceneCamera ∷ Camera2D
    , sceneActive ∷ Bool
    } deriving (Show)

-- | Enhanced scene manager with batch processing
data SceneManager = SceneManager
    { smScenes       ∷ Map.Map Text Scene
    , smActiveScene  ∷ Maybe Text
    , smSceneGraphs  ∷ Map.Map Text SceneGraph
    , smBatchManager ∷ BatchManager
    , smDirtyScenes  ∷ Set.Set Text
    } deriving (Show)

createSceneManager ∷ SceneManager
createSceneManager = SceneManager
    { smScenes = Map.empty
    , smActiveScene = Nothing
    , smSceneGraphs = Map.empty
    , smBatchManager = createBatchManager
    , smDirtyScenes = Set.empty
    }

createEmptyScene ∷ Text → Scene
createEmptyScene sid = Scene
    { sceneId = sid
    , sceneCamera = defaultCamera
    , sceneActive = True
    }
