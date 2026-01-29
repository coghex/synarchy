{-# LANGUAGE Strict #-}
module Engine.Scene.Types.Batch
  ( DrawableObject(..)
  , RenderBatch(..)
  , TextRenderBatch(..)
  , TextBatch(..)
  , RenderItem(..)
  , BatchManager(..)
  , createBatchManager
  , SceneDynamicBuffer(..)
  ) where

import UPrelude
import qualified Data.Vector as V
import qualified Data.Map as Map
import qualified Data.Set as Set
import Engine.Scene.Base (ObjectId, LayerId)
import Engine.Asset.Handle (TextureHandle, FontHandle)
import Engine.Graphics.Vulkan.Types.Vertex (Vertex)
import Engine.Graphics.Font.Data (GlyphInstance)
import qualified Vulkan.Core10 as Vk

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

-- | Simplified text batch for conversion
data TextBatch = TextBatch
    { tbFontHandle ∷ FontHandle
    , tbInstances  ∷ V.Vector GlyphInstance
    , tbLayer      ∷ LayerId
    } deriving (Show)

-- | Unified render item for layer-based rendering
data RenderItem
  = SpriteItem RenderBatch
  | TextItem TextRenderBatch
  deriving (Show)

-- | Batch manager state
data BatchManager = BatchManager
    { bmBatches        ∷ Map.Map (TextureHandle, LayerId) RenderBatch
    , bmTextBatches    ∷ Map.Map (FontHandle, LayerId) TextRenderBatch
    , bmVisibleObjs    ∷ V.Vector DrawableObject
    , bmDirtyBatches   ∷ Set.Set (TextureHandle, LayerId)
    , bmLayeredBatches ∷ Map.Map LayerId (V.Vector RenderItem)
    } deriving (Show)

-- | Create empty batch manager
createBatchManager ∷ BatchManager
createBatchManager = BatchManager
    { bmBatches = Map.empty
    , bmTextBatches = Map.empty
    , bmVisibleObjs = V.empty
    , bmDirtyBatches = Set.empty
    , bmLayeredBatches = Map.empty
    }

-- | Dynamic vertex buffer for scene rendering
data SceneDynamicBuffer = SceneDynamicBuffer
    { sdbBuffer   ∷ Vk.Buffer
    , sdbMemory   ∷ Vk.DeviceMemory
    , sdbCapacity ∷ Word64
    , sdbUsed     ∷ Word64
    } deriving (Show)
