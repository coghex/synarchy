-- | Per-layer frame assembly (#2192): the ONE pure merge that decides
--   where every drawable of a frame lands in the layered map command
--   recording walks, and in what order within its layer.
--
--   Three sources feed a frame:
--
--   * World-derived content ('LayeredQuads'): the cached, pre-sorted
--     static terrain runs plus the small per-tick dynamic rest (units,
--     cursor, ghost, ...), depth-sorted per layer and linear-merged
--     into the static runs (#446).
--   * Scene primitives — the public Lua @engine.spawnSprite@ /
--     @engine.spawnText@ objects held by the 'BatchManager'. A sprite
--     on a WORLD layer (below 'uiLayerThreshold') joins that depth
--     interleave as a 'SortableQuad' and is drawn by the world sprite
--     pipeline; a sprite on a UI layer, and a text node on ANY layer,
--     is a standalone item at its declared 'LayerId' (a UI-layer
--     sprite is positioned in framebuffer pixels, a world-layer text
--     is laid out through the world camera — that choice is made
--     upstream, in "Engine.Scene.Batch.Text" / the recorder's pipeline
--     selection, from the same threshold).
--   * UI pages ('UI.Render.renderUIPages'), already keyed by their
--     element layers.
--
--   __Equal-layer ordering__ is the recorder's contract, stated here
--   because this is where the vector it consumes is built: layers draw
--   in ascending 'LayerId'; within one layer every 'SpriteItem' draws
--   before every 'TextItem' ('layerSprites' / 'layerTexts' are the
--   partition "Engine.Graphics.Vulkan.Command.Record" applies); and
--   within either item kind the source order is world-derived content,
--   then scene primitives, then UI-page content — which is the
--   left-to-right order 'assembleLayeredBatches' concatenates them in.
--
--   Everything here is pure so the whole contract is checkable
--   GPU-free (hspec @--match "frame layer assembly"@); the frame loop
--   ('Engine.Loop.Frame.renderSceneFrame') and the recorder both call
--   these functions rather than restating the walks, so the vertex
--   buffer layout ('spriteBatchesInDrawOrder') and the glyph upload
--   order ('textBatchesInDrawOrder') cannot drift from the draw order.
module Engine.Scene.Assembly
  ( assembleLayeredBatches
  , worldSceneQuads
  , scenePrimitiveItems
  , layerSprites
  , layerTexts
  , spriteBatchesInDrawOrder
  , textBatchesInDrawOrder
  ) where

import UPrelude
import qualified Data.Vector as V
import qualified Data.Map.Strict as Map
import Engine.Scene.Base (LayerId)
import Engine.Scene.Types.Batch
import Engine.Scene.Batch.Update (getSortedBatches)
import World.Grid (uiLayerThreshold)

-- | The frame's per-layer merge. @worldQuads@ is the world thread's
--   publication, @bm@ the scene's batch manager as 'updateSceneForRender'
--   left it this frame, @uiLayered@ the UI pages' items.
--
--   Static terrain sits on the LEFT of the quad merge, so a dynamic or
--   scene sprite at exactly a tile's depth deterministically draws over
--   it; the three sources then concatenate left to right per layer in
--   the order the module header states.
assembleLayeredBatches ∷ LayeredQuads → BatchManager
                       → Map.Map LayerId (V.Vector RenderItem)
                       → Map.Map LayerId (V.Vector RenderItem)
assembleLayeredBatches worldQuads bm uiLayered =
    let dynByLayer = sortQuadsByLayer (lqDynamic worldQuads <> worldSceneQuads bm)
        groupedByLayer = Map.unionWith mergeSortedQuads (lqStatic worldQuads) dynByLayer
        worldLayered = Map.mapWithKey
            (\layer quads → V.singleton (SpriteItem (batchFromSortedQuads layer quads)))
            groupedByLayer
    in Map.unionsWith (<>) [worldLayered, scenePrimitiveItems bm, uiLayered]

-- | Scene sprites on WORLD layers, as quads for the tile interleave.
--   These are the only scene objects that do NOT become standalone
--   items: 'scenePrimitiveItems' keeps the complementary set, so no
--   sprite is drawn twice and none is dropped.
worldSceneQuads ∷ BatchManager → V.Vector SortableQuad
worldSceneQuads bm =
    V.map drawableToQuad
        (V.filter (\obj → doLayer obj < uiLayerThreshold) (bmVisibleObjs bm))

-- | Scene primitives drawn as standalone items at their declared layer:
--   every sprite batch on a UI layer (in the manager's layer/depth
--   order), then every text batch on every layer (in font order). Text
--   on a world layer keys BELOW the threshold, which is exactly what
--   makes the recorder pick the world font pipeline for it.
scenePrimitiveItems ∷ BatchManager → Map.Map LayerId (V.Vector RenderItem)
scenePrimitiveItems bm =
    let uiSprites = [ (rbLayer b, SpriteItem b)
                    | b ← V.toList (getSortedBatches bm)
                    , rbLayer b ≥ uiLayerThreshold ]
        texts = [ (trbLayer t, TextItem t) | t ← Map.elems (bmTextBatches bm) ]
        -- Later entries append AFTER earlier ones in a layer, so the
        -- sprites listed first stay ahead of the texts.
        place acc (layer, item) = Map.insertWith (flip (<>)) layer (V.singleton item) acc
    in foldl' place Map.empty (uiSprites ⧺ texts)

-- | The sprite half of one layer's items, in item order — the partition
--   the recorder draws before any of that layer's text.
layerSprites ∷ V.Vector RenderItem → V.Vector RenderBatch
layerSprites = V.mapMaybe (\case SpriteItem b → Just b; _ → Nothing)

-- | The text half of one layer's items, in item order.
layerTexts ∷ V.Vector RenderItem → V.Vector TextRenderBatch
layerTexts = V.mapMaybe (\case TextItem t → Just t; _ → Nothing)

-- | Every sprite batch of the frame in the exact order the recorder
--   draws them — ascending layer, item order within a layer — which is
--   therefore the order their vertices are laid out in the dynamic
--   vertex buffer.
spriteBatchesInDrawOrder ∷ Map.Map LayerId (V.Vector RenderItem) → V.Vector RenderBatch
spriteBatchesInDrawOrder = V.concatMap layerSprites ∘ V.fromList ∘ Map.elems

-- | Every text batch of the frame in draw order, which is the order
--   their glyph instances are uploaded so the recorder's per-layer
--   slices stay aligned.
textBatchesInDrawOrder ∷ Map.Map LayerId (V.Vector RenderItem) → V.Vector TextRenderBatch
textBatchesInDrawOrder = V.concatMap layerTexts ∘ V.fromList ∘ Map.elems
