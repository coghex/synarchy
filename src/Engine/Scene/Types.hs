{-# LANGUAGE Strict #-}
module Engine.Scene.Types where

import UPrelude
import qualified Data.Vector as V
import qualified Data.Map as Map
import qualified Data.Text as T
import Engine.Scene.Base
import Engine.Asset.Base (AssetId)
import Engine.Graphics.Vulkan.Types.Texture (TextureData)
import Engine.Graphics.Vulkan.Types.Vertex (Vertex)

-- | Core scene object representation
data SceneObject = SceneObject
    { objId       ∷ ObjectId
    , objType     ∷ ObjectType
    , objTransform ∷ Transform2D
    , objTexture  ∷ Maybe AssetId
    , objVisible  ∷ Bool
    } deriving (Show)

-- | Scene layer for depth management
data SceneLayer = SceneLayer
    { layerId      ∷ LayerId
    , layerObjects ∷ Map.Map ObjectId SceneObject
    , layerVisible ∷ Bool
    } deriving (Show)

-- | Simple 2D camera
data Camera2D = Camera2D
    { camPosition ∷ (Float, Float)
    , camZoom     ∷ Float
    , camRotation ∷ Float
    } deriving (Show)

-- | Main scene state
data Scene = Scene
    { sceneId      ∷ T.Text
    , sceneLayers  ∷ Map.Map LayerId SceneLayer
    , sceneCamera  ∷ Camera2D
    } deriving (Show)

-- | Batch for sprite rendering
data SpriteBatch = SpriteBatch
    { batchTexture  ∷ AssetId
    , batchVertices ∷ V.Vector Vertex  -- You'll need to import your Vertex type
    , batchDirty    ∷ Bool
    } deriving (Show)

-- | Scene manager state
data SceneManager = SceneManager
    { activeScene   ∷ Scene
    , sceneBatches  ∷ Map.Map LayerId (Map.Map AssetId SpriteBatch)
    } deriving (Show)

-- | Helper functions
defaultCamera ∷ Camera2D
defaultCamera = Camera2D
    { camPosition = (0, 0)
    , camZoom = 1.0
    , camRotation = 0.0
    }

createEmptyScene ∷ T.Text → Scene
createEmptyScene sid = Scene
    { sceneId = sid
    , sceneLayers = Map.empty
    , sceneCamera = defaultCamera
    }

createEmptyLayer ∷ LayerId → SceneLayer
createEmptyLayer lid = SceneLayer
    { layerId = lid
    , layerObjects = Map.empty
    , layerVisible = True
    }
