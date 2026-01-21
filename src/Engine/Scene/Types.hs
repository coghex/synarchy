{-# LANGUAGE Strict #-}
module Engine.Scene.Types where

import UPrelude
import qualified Data.Vector as V
import qualified Data.Map as Map
import qualified Data.Set as Set
import Engine.Scene.Base
import Engine.Asset.Base (AssetId)
import Engine.Asset.Types
import Engine.Graphics.Vulkan.Types.Texture (TextureData)
import Engine.Graphics.Vulkan.Types.Vertex (Vertex, Vec2(..), Vec4(..))
import Engine.Graphics.Camera
import Engine.Graphics.Font.Data
import Linear (M44)

-- | Core scene object representation
data SceneObject = SceneObject
    { objId       ∷ ObjectId
    , objType     ∷ NodeType
    , objTransform ∷ Transform2D
    , objTexture  ∷ Maybe TextureHandle
    , objVisible  ∷ Bool
    } deriving (Show)

-- | Scene layer for depth management
data SceneLayer = SceneLayer
    { layerId      ∷ LayerId
    , layerObjects ∷ Map.Map ObjectId SceneObject
    , layerVisible ∷ Bool
    } deriving (Show)

-- | Batch for sprite rendering
data SpriteBatch = SpriteBatch
    { batchTexture  ∷ TextureHandle
    , batchVertices ∷ V.Vector Vertex  -- You'll need to import your Vertex type
    , batchDirty    ∷ Bool
    } deriving (Show)

-- | Enhanced scene manager with batch processing
data SceneManager = SceneManager
    { smScenes       ∷ Map.Map Text Scene
    , smActiveScene  ∷ Maybe Text
    , smSceneGraphs  ∷ Map.Map Text SceneGraph
    , smBatchManager ∷ BatchManager
    , smDirtyScenes  ∷ Set.Set Text
    } deriving (Show)

-- | Enhanced scene with scene graph
data Scene = Scene
    { sceneId     ∷ Text
    , sceneCamera ∷ Camera2D
    , sceneActive ∷ Bool
    } deriving (Show)

-- | Create empty scene manager
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

createEmptyLayer ∷ LayerId → SceneLayer
createEmptyLayer lid = SceneLayer
    { layerId = lid
    , layerObjects = Map.empty
    , layerVisible = True
    }

-- | Represents a drawable object in the scene
data SceneNode = SceneNode
    { nodeId         ∷ ObjectId
    , nodeType       ∷ NodeType
    , nodeTransform  ∷ Transform2D
    , nodeLayer      ∷ LayerId
    , nodeTexture    ∷ Maybe TextureHandle
    , nodeFont       ∷ Maybe FontHandle
    , nodeText       ∷ Maybe Text
    , nodeVisible    ∷ Bool
    , nodeColor      ∷ Vec4        -- RGBA color multiplier
    , nodeUVRect     ∷ Maybe (Vec2, Vec2)  -- UV coordinates (min, max) for atlas
    , nodeSize       ∷ (Float, Float)      -- Object size in world units
    , nodeChildren   ∷ Set.Set ObjectId    -- Child nodes
    , nodeParent     ∷ Maybe ObjectId      -- Parent node
    , nodeDirty      ∷ Bool               -- Whether transform needs update
    } deriving (Show)

-- | World-space transform matrix
data WorldTransform = WorldTransform
    { wtMatrix    ∷ M44 Float
    , wtPosition  ∷ (Float, Float)
    , wtScale     ∷ (Float, Float)
    , wtRotation  ∷ Float
    , wtZIndex    ∷ Float
    } deriving (Show)

-- | Scene graph containing all nodes
data SceneGraph = SceneGraph
    { sgNodes         ∷ Map.Map ObjectId SceneNode
    , sgRootNodes     ∷ Set.Set ObjectId
    , sgWorldTrans    ∷ Map.Map ObjectId WorldTransform
    , sgDirtyNodes    ∷ Set.Set ObjectId
    , sgNextId        ∷ Word32
    } deriving (Show)

-- | Create an empty scene graph
createEmptySceneGraph ∷ SceneGraph
createEmptySceneGraph = SceneGraph
    { sgNodes = Map.empty
    , sgRootNodes = Set.empty
    , sgWorldTrans = Map.empty
    , sgDirtyNodes = Set.empty
    , sgNextId = 1
    }

-- | Drawable object ready for rendering
data DrawableObject = DrawableObject
    { doId         ∷ ObjectId
    , doTexture    ∷ TextureHandle
    , doVertices   ∷ V.Vector Vertex
    , doZIndex     ∷ Float
    , doLayer      ∷ LayerId
    } deriving (Show)

-- | Render batch grouped by texture and layer
data RenderBatch = RenderBatch
    { rbTexture    ∷ TextureHandle
    , rbLayer      ∷ LayerId
    , rbVertices   ∷ V.Vector Vertex
    , rbObjects    ∷ V.Vector ObjectId
    , rbDirty      ∷ Bool
    } deriving (Show)

-- | Text render batch grouped by font and layer
data TextRenderBatch = TextRenderBatch
    { trbFont      ∷ FontHandle
    , trbLayer     ∷ LayerId
    , trbInstances ∷ V.Vector GlyphInstance
    , trbObjects   ∷ V.Vector ObjectId
    } deriving (Show)

-- | Batch manager state
data BatchManager = BatchManager
    { bmBatches      ∷ Map.Map (TextureHandle, LayerId) RenderBatch
    , bmTextBatches  ∷ Map.Map (FontHandle, LayerId) TextRenderBatch
    , bmVisibleObjs  ∷ V.Vector DrawableObject
    , bmDirtyBatches ∷ Set.Set (TextureHandle, LayerId)
    } deriving (Show)

-- | Create empty batch manager
createBatchManager ∷ BatchManager
createBatchManager = BatchManager
    { bmBatches = Map.empty
    , bmTextBatches = Map.empty
    , bmVisibleObjs = V.empty
    , bmDirtyBatches = Set.empty
    }


