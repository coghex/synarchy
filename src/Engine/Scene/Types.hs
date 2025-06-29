{-# LANGUAGE Strict #-}
module Engine.Scene.Types where

import UPrelude
import qualified Data.Vector as V
import qualified Data.Map as Map
import Engine.Scene.Base
import Engine.Asset.Base (AssetId)
import Engine.Graphics.Vulkan.Types.Texture (TextureData)
import Engine.Graphics.Vulkan.Types.Vertex (Vertex)
import Engine.Graphics.Camera

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

-- | Main scene state
data Scene = Scene
    { sceneId      ∷ Text
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
    { activeScene   ∷ Maybe Text
    , scenes        ∷ Map.Map Text Scene
    , sceneBatches  ∷ Map.Map LayerId (Map.Map AssetId SpriteBatch)
    } deriving (Show)

createEmptyScene ∷ Text → Scene
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
