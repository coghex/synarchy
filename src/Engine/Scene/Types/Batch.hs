{-# LANGUAGE Strict, UnicodeSyntax #-}
module Engine.Scene.Types.Batch
  ( SortableQuad(..)
  , DrawableObject(..)
  , RenderBatch(..)
  , TextRenderBatch(..)
  , TextBatch(..)
  , RenderItem(..)
  , BatchManager(..)
  , createBatchManager
  , SceneDynamicBuffer(..)
  , drawableToQuad
  , mergeQuadsToBatch
  ) where

import UPrelude
import qualified Data.Vector as V
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.List as List
import Engine.Scene.Base (ObjectId, LayerId)
import Engine.Asset.Handle (TextureHandle(..), FontHandle)
import Engine.Graphics.Vulkan.Types.Vertex (Vertex)
import Engine.Graphics.Font.Data (GlyphInstance)
import qualified Vulkan.Core10 as Vk

-----------------------------------------------------------
-- Batch types
-----------------------------------------------------------

data SortableQuad = SortableQuad
    { sqSortKey  ∷ !Float
    , sqVertices ∷ V.Vector Vertex
    , sqTexture  ∷ TextureHandle
    , sqLayer    ∷ LayerId
    } deriving (Show)

data DrawableObject = DrawableObject
    { doId         ∷ ObjectId
    , doTexture    ∷ TextureHandle
    , doVertices   ∷ V.Vector Vertex
    , doZIndex     ∷ Float
    , doLayer      ∷ LayerId
    } deriving (Show)

drawableToQuad ∷ DrawableObject → SortableQuad
drawableToQuad dobj = SortableQuad
    { sqSortKey  = doZIndex dobj
    , sqVertices = doVertices dobj
    , sqTexture  = doTexture dobj
    , sqLayer    = doLayer dobj
    }

mergeQuadsToBatch ∷ LayerId → [SortableQuad] → RenderBatch
mergeQuadsToBatch layer quads =
    let sorted = List.sortOn sqSortKey quads
        allVerts = V.concat $ map sqVertices sorted
        tex = case sorted of
                (q:_) → sqTexture q
                []    → TextureHandle 0
    in RenderBatch
        { rbTexture  = tex
        , rbLayer    = layer
        , rbVertices = allVerts
        , rbObjects  = V.empty
        , rbDirty    = True
        }

data RenderBatch = RenderBatch
data RenderBatch = RenderBatch
    { rbTexture    ∷ TextureHandle
    , rbLayer      ∷ LayerId
    , rbVertices   ∷ V.Vector Vertex
    , rbObjects    ∷ V.Vector ObjectId
    , rbDirty      ∷ Bool
    } deriving (Show)

data TextRenderBatch = TextRenderBatch
data TextRenderBatch = TextRenderBatch
    { trbFont      ∷ FontHandle
    , trbLayer     ∷ LayerId
    , trbInstances ∷ V.Vector GlyphInstance
    , trbObjects   ∷ V.Vector ObjectId
    } deriving (Show)

data TextBatch = TextBatch
data TextBatch = TextBatch
    { tbFontHandle ∷ FontHandle
    , tbInstances  ∷ V.Vector GlyphInstance
    , tbLayer      ∷ LayerId
    } deriving (Show)

data RenderItem
data RenderItem
  = SpriteItem RenderBatch
  | TextItem TextRenderBatch
  deriving (Show)

data BatchManager = BatchManager
data BatchManager = BatchManager
    { bmBatches        ∷ Map.Map (TextureHandle, LayerId) RenderBatch
    , bmTextBatches    ∷ Map.Map (FontHandle, LayerId) TextRenderBatch
    , bmVisibleObjs    ∷ V.Vector DrawableObject
    , bmDirtyBatches   ∷ Set.Set (TextureHandle, LayerId)
    , bmLayeredBatches ∷ Map.Map LayerId (V.Vector RenderItem)
    } deriving (Show)

createBatchManager ∷ BatchManager
createBatchManager = BatchManager
    { bmBatches = Map.empty
    , bmTextBatches = Map.empty
    , bmVisibleObjs = V.empty
    , bmDirtyBatches = Set.empty
    , bmLayeredBatches = Map.empty
    }

data SceneDynamicBuffer = SceneDynamicBuffer
data SceneDynamicBuffer = SceneDynamicBuffer
    { sdbBuffer   ∷ Vk.Buffer
    , sdbMemory   ∷ Vk.DeviceMemory
    , sdbCapacity ∷ Word64
    , sdbUsed     ∷ Word64
    } deriving (Show)
