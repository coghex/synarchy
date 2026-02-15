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
  , TextInstanceBuffer(..)
  , drawableToQuad
  , mergeQuadsToBatch
  ) where

import UPrelude
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as VM
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.List as List
import Control.Monad.ST (runST)
import Engine.Scene.Base (ObjectId, LayerId)
import Engine.Asset.Handle (TextureHandle(..), FontHandle)
import Engine.Graphics.Vulkan.Types.Vertex (Vertex)
import Engine.Graphics.Font.Data (GlyphInstance)
import qualified Vulkan.Core10 as Vk

-- | A sortable unit of vertices for painter's algorithm.
-- Both world tiles and scene sprites produce these.
-- After sorting by sqSortKey, vertices are concatenated
-- in order to get correct back-to-front rendering.
data SortableQuad = SortableQuad
    { sqSortKey  ∷ !Float       -- ^ Painter's algorithm depth (higher = drawn later = in front)
    , sqV0       ∷ !Vertex      -- ^ top left
    , sqV1       ∷ !Vertex      -- ^ top right
    , sqV2       ∷ !Vertex      -- ^ bottom right
    , sqV3       ∷ !Vertex      -- ^ bottom left
    , sqTexture  ∷ !TextureHandle  -- ^ Needed for potential future per-texture batching
    , sqLayer    ∷ !LayerId
    } deriving (Show)

-- | Drawable object ready for rendering
data DrawableObject = DrawableObject
    { doId         ∷ ObjectId
    , doTexture    ∷ TextureHandle
    , doV0         ∷ Vertex
    , doV1         ∷ Vertex
    , doV2         ∷ Vertex
    , doV3         ∷ Vertex
    , doZIndex     ∷ Float
    , doLayer      ∷ LayerId
    } deriving (Show)

-- | Convert a DrawableObject to a SortableQuad
-- Used to bring scene sprites into the unified sort with world tiles
drawableToQuad ∷ DrawableObject → SortableQuad
drawableToQuad dobj = SortableQuad
    { sqSortKey  = doZIndex dobj
    , sqV0       = doV0 dobj
    , sqV1       = doV1 dobj
    , sqV2       = doV2 dobj
    , sqV3       = doV3 dobj
    , sqTexture  = doTexture dobj
    , sqLayer    = doLayer dobj
    }

-- | Sort quads by painter's algorithm and merge into a single RenderBatch.
--   All quads MUST share the same layer. The texture field uses the first
--   quad's texture (irrelevant for bindless — the per-vertex atlasId drives
--   texture selection in the shader).
--
--   Sorts once, then builds the final vertex vector with V.concatMap over
--   a sorted vector. No intermediate list-of-vectors allocation.
mergeQuadsToBatch ∷ LayerId → [SortableQuad] → RenderBatch
mergeQuadsToBatch layer quads =
    let !sortedList = List.sortOn sqSortKey quads
        !totalVerts = length sortedList * 6
        -- Since it's already sorted, average of first and last is a
        -- cheap representative value. Or just use 0 — world tiles
        -- are sorted per-layer anyway, not across layers.
        !avgZ = case sortedList of
            []    → 0.0
            [q]   → sqSortKey q
            (q:_) → let last' = sqSortKey (List.last sortedList)
                     in (sqSortKey q + last') / 2.0
        !allVerts = V.create $ do
            mv ← VM.new totalVerts
            let go _ [] = return ()
                go i (q:qs) = do
                    VM.write mv  i      (sqV0 q)
                    VM.write mv (i+1)   (sqV1 q)
                    VM.write mv (i+2)   (sqV2 q)
                    VM.write mv (i+3)   (sqV0 q)
                    VM.write mv (i+4)   (sqV2 q)
                    VM.write mv (i+5)   (sqV3 q)
                    go (i + 6) qs
            go 0 sortedList
            return mv
        tex = case sortedList of
            []    → TextureHandle 0
            (q:_) → sqTexture q
    in RenderBatch
        { rbTexture  = tex
        , rbLayer    = layer
        , rbVertices = allVerts
        , rbObjects  = V.empty
        , rbDirty    = True
        , rbAvgZ     = avgZ
        }

-- | Render batch grouped by texture and layer
data RenderBatch = RenderBatch
    { rbTexture    ∷ TextureHandle
    , rbLayer      ∷ LayerId
    , rbVertices   ∷ V.Vector Vertex
    , rbObjects    ∷ V.Vector ObjectId
    , rbDirty      ∷ Bool
    , rbAvgZ       ∷ Float
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

-- | Cached instance buffer for text glyph rendering.
--   Reused across frames; grown when needed.
data TextInstanceBuffer = TextInstanceBuffer
    { tibBuffer   ∷ Vk.Buffer
    , tibMemory   ∷ Vk.DeviceMemory
    , tibCapacity ∷ Word64    -- ^ Max glyph instances that fit
    , tibUsed     ∷ Word64    -- ^ Glyph instances uploaded this frame
    } deriving (Show)
