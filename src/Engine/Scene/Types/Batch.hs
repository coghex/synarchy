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
  , batchFromSortedQuads
  , sortQuadsByLayer
  , mergeSortedQuads
  , LayeredQuads(..)
  , emptyLayeredQuads
  ) where

import UPrelude
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as VM
import qualified Data.Vector.Storable as VS
import qualified Data.Vector.Storable.Mutable as VSM
import qualified Data.Vector.Algorithms.Intro as VA
import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.Ord (comparing)
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

-- | World quads split by lifetime (#446). 'lqStatic' is the cached
--   terrain set, pre-grouped by layer and pre-sorted by 'sqSortKey' —
--   built on the world thread only when the quad cache rebuilds.
--   'lqDynamic' is the small per-tick rest (units, cursor, ghost,
--   spoil, ground items, buildings, structures, zoom map), sorted
--   fresh each frame and linear-merged into the static runs.
data LayeredQuads = LayeredQuads
    { lqStatic  ∷ !(Map.Map LayerId (V.Vector SortableQuad))
    , lqDynamic ∷ !(V.Vector SortableQuad)
    } deriving (Show)

emptyLayeredQuads ∷ LayeredQuads
emptyLayeredQuads = LayeredQuads Map.empty V.empty

-- | Group quads by layer and depth-sort each layer's run.
sortQuadsByLayer ∷ V.Vector SortableQuad → Map.Map LayerId (V.Vector SortableQuad)
sortQuadsByLayer quads =
    Map.map (V.modify (VA.sortBy (comparing sqSortKey)) ∘ V.fromList) $
        V.foldl' (\acc q → Map.insertWith (⧺) (sqLayer q) [q] acc)
                 Map.empty quads

-- | Linear merge of two individually depth-sorted runs into one sorted
--   run, O(n+m). Ties take from the LEFT run first — callers pass the
--   static (terrain) run on the left, so a dynamic sprite sitting at
--   exactly a tile's depth deterministically draws after (over) it.
mergeSortedQuads ∷ V.Vector SortableQuad → V.Vector SortableQuad → V.Vector SortableQuad
mergeSortedQuads xs ys
    | V.null xs = ys
    | V.null ys = xs
    | otherwise = V.create $ do
        let nx = V.length xs
            ny = V.length ys
        mv ← VM.new (nx + ny)
        let go i j
              | i ≥ nx = V.copy (VM.slice (i + j) (ny - j) mv) (V.slice j (ny - j) ys)
              | j ≥ ny = V.copy (VM.slice (i + j) (nx - i) mv) (V.slice i (nx - i) xs)
              | otherwise = do
                  let qx = xs V.! i
                      qy = ys V.! j
                  if sqSortKey qx ≤ sqSortKey qy
                    then do VM.write mv (i + j) qx
                            go (i + 1) j
                    else do VM.write mv (i + j) qy
                            go i (j + 1)
        go 0 0
        return mv

-- | Sort quads by painter's algorithm and merge into a single RenderBatch.
mergeQuadsToBatch ∷ LayerId → V.Vector SortableQuad → RenderBatch
mergeQuadsToBatch layer quads =
    batchFromSortedQuads layer (V.modify (VA.sortBy (comparing sqSortKey)) quads)

-- | Expand ALREADY depth-sorted quads into a RenderBatch. The frame
--   loop calls this with 'mergeSortedQuads' output so the per-frame
--   cost is the linear merge, not a full re-sort (#446).
batchFromSortedQuads ∷ LayerId → V.Vector SortableQuad → RenderBatch
batchFromSortedQuads layer sorted =
    let !totalVerts = V.length sorted * 6
        !avgZ = if V.null sorted
               then 0
               else let last' = sqSortKey (V.last sorted)
                        first' = sqSortKey (V.head sorted)
                    in if last' == first'
                       then last'
                       else (last' + first') / 2
        !allVerts = VS.create $ do
            mv ← VSM.new totalVerts
            V.iforM_ sorted $ \idx q → do
                let i = idx * 6
                VSM.write mv  i      (sqV0 q)
                VSM.write mv (i+1)   (sqV1 q)
                VSM.write mv (i+2)   (sqV2 q)
                VSM.write mv (i+3)   (sqV0 q)
                VSM.write mv (i+4)   (sqV2 q)
                VSM.write mv (i+5)   (sqV3 q)
            return mv
        tex = if V.null sorted
              then TextureHandle 0
              else sqTexture (V.head sorted)
    in RenderBatch
        { rbTexture  = tex
        , rbLayer    = layer
        , rbVertices = allVerts
        , rbObjects  = V.empty
        , rbDirty    = True
        , rbAvgZ     = avgZ
        }

data RenderBatch = RenderBatch
    { rbTexture    ∷ TextureHandle
    , rbLayer      ∷ LayerId
      -- | Storable (unboxed, pinned) so upload is a straight memcpy and
      --   per-frame batch builds allocate no boxed Vertex objects (#445).
    , rbVertices   ∷ VS.Vector Vertex
    , rbObjects    ∷ V.Vector ObjectId
    , rbDirty      ∷ Bool
    , rbAvgZ       ∷ Float
    } deriving (Show)

data TextRenderBatch = TextRenderBatch
    { trbFont      ∷ FontHandle
    , trbLayer     ∷ LayerId
    , trbInstances ∷ V.Vector GlyphInstance
    , trbObjects   ∷ V.Vector ObjectId
    } deriving (Show)

data TextBatch = TextBatch
    { tbFontHandle ∷ FontHandle
    , tbInstances  ∷ V.Vector GlyphInstance
    , tbLayer      ∷ LayerId
    } deriving (Show)

data RenderItem
  = SpriteItem RenderBatch
  | TextItem TextRenderBatch
  deriving (Show)

data BatchManager = BatchManager
    { bmBatches        ∷ Map.Map (TextureHandle, LayerId) RenderBatch
    , bmTextBatches    ∷ Map.Map (FontHandle, LayerId) TextRenderBatch
    , bmVisibleObjs    ∷ V.Vector DrawableObject
    , bmDirtyBatches   ∷ Set.Set (TextureHandle, LayerId)
    } deriving (Show)

createBatchManager ∷ BatchManager
createBatchManager = BatchManager
    { bmBatches = Map.empty
    , bmTextBatches = Map.empty
    , bmVisibleObjs = V.empty
    , bmDirtyBatches = Set.empty
    }

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
