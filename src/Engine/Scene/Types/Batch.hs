{-# LANGUAGE Strict #-}
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
  , quadPainterOrder
  , LayeredQuads(..)
  , emptyLayeredQuads
  , setQuadSolarPage
  , stampSolarPage
  ) where

import UPrelude
import Control.DeepSeq (NFData(..), rwhnf)
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
import Engine.Graphics.Vulkan.Types.Vertex (Vertex(..), Vec2(..))
import Engine.Graphics.Solar (SolarPageTable, emptySolarPageTable)
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

-- | Every field is strict (Vertex transitively so), so WHNF = NF —
--   what the parallel per-chunk quad build's rdeepseq forces (#447).
instance NFData SortableQuad where
    rnf = rwhnf

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
    , lqSolar   ∷ !SolarPageTable
      -- ^ The per-page solar attribution these quads' @solarPage@ slots
      --   index into (#1869). It rides HERE, inside the one immutable
      --   value the world thread publishes, precisely because the
      --   renderer may draw a frame from the PREVIOUS publication: a
      --   table reaching the UBO by any other route could describe a
      --   different visible set than the vertices being drawn.
    } deriving (Show)

emptyLayeredQuads ∷ LayeredQuads
emptyLayeredQuads = LayeredQuads Map.empty V.empty emptySolarPageTable

-- | Attribute one quad's four corners to a solar page slot (#1869).
--
--   Attribution is applied HERE, to finished quads, rather than
--   threaded through every quad producer: the producers are per-page
--   loops already, so one stamp at the point the page is known keeps a
--   single assignment site and makes a new producer correct by default
--   instead of silently page-less.
setQuadSolarPage ∷ Word32 → SortableQuad → SortableQuad
setQuadSolarPage slot q = q
    { sqV0 = (sqV0 q) { solarPage = slot }
    , sqV1 = (sqV1 q) { solarPage = slot }
    , sqV2 = (sqV2 q) { solarPage = slot }
    , sqV3 = (sqV3 q) { solarPage = slot }
    }

-- | 'setQuadSolarPage' over a whole run.
stampSolarPage ∷ Word32 → V.Vector SortableQuad → V.Vector SortableQuad
stampSolarPage slot = V.map (setQuadSolarPage slot)

-- | The scene's painter order (#1856).
--
--   'sqSortKey' alone is not a total order: two sprites can legitimately
--   share a depth — two wood-tagged trees on one tile at one z with
--   equal sub-tile V offsets do — and the sort below is an UNSTABLE
--   introsort, so equal keys were drawn in an order nothing could
--   predict or agree with. Any consumer that has to reproduce \"which
--   sprite is on top\" — Chop's screen-space selection oracle
--   ('World.Flora.HitTest'), and the designation marker anchored to
--   whatever it picked — then had no order to share.
--
--   Extending the comparison to the quad's own rect makes it total on
--   everything such a consumer can also see, and costs no new field on
--   a record built in fifty places. Two quads still equal here occupy
--   exactly the same rect at exactly the same depth, so no picker can
--   tell them apart either.
--
--   This only REFINES ties: every ordering that was already determined
--   by 'sqSortKey' is unchanged.
quadPainterOrder ∷ SortableQuad → (Float, Float, Float, Float, Float)
quadPainterOrder q =
    let Vec2 x0 y0 = pos (sqV0 q)
        Vec2 x2 y2 = pos (sqV2 q)
    in (sqSortKey q, x0, y0, x2, y2)

-- | Group quads by layer and depth-sort each layer's run.
sortQuadsByLayer ∷ V.Vector SortableQuad → Map.Map LayerId (V.Vector SortableQuad)
sortQuadsByLayer quads =
    Map.map (V.modify (VA.sortBy (comparing quadPainterOrder)) ∘ V.fromList) $
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
                  if quadPainterOrder qx ≤ quadPainterOrder qy
                    then do VM.write mv (i + j) qx
                            go (i + 1) j
                    else do VM.write mv (i + j) qy
                            go i (j + 1)
        go 0 0
        return mv

-- | Sort quads by painter's algorithm and merge into a single RenderBatch.
mergeQuadsToBatch ∷ LayerId → V.Vector SortableQuad → RenderBatch
mergeQuadsToBatch layer quads =
    batchFromSortedQuads layer
        (V.modify (VA.sortBy (comparing quadPainterOrder)) quads)

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
                    in if last' ≡ first'
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
