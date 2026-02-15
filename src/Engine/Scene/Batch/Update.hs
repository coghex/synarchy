{-# LANGUAGE Strict #-}
module Engine.Scene.Batch.Update
  ( updateBatches
  , updateTextBatches
  , createBatch
  , getSortedBatches
  , buildLayeredBatches
  , groupByTextureAndLayer
  ) where

import UPrelude
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as VM
import qualified Data.Map as Map
import qualified Data.List as List
import qualified Data.Set as Set
import Engine.Asset.Handle (TextureHandle, FontHandle)
import Engine.Scene.Base (ObjectId, LayerId)
import Engine.Scene.Types.Batch

-- | Update batches with new visible objects
updateBatches ∷ V.Vector DrawableObject → BatchManager → BatchManager
updateBatches objects manager =
    let groupedObjs = groupByTextureAndLayer objects
        newBatches = Map.fromList $ map createBatch groupedObjs
        dirtyKeys = Map.keysSet newBatches
    in manager
        { bmBatches = newBatches
        , bmVisibleObjs = objects
        , bmDirtyBatches = dirtyKeys
        }

-- | Update batch manager with text batches
updateTextBatches ∷ V.Vector TextRenderBatch → BatchManager → BatchManager
updateTextBatches textBatches manager =
    let groupedText = V.foldl' (\acc trb →
            let key = (trbFont trb, trbLayer trb)
            in Map.insert key trb acc) Map.empty textBatches
    in manager { bmTextBatches = groupedText }

-- | Group drawable objects by texture and layer
groupByTextureAndLayer ∷ V.Vector DrawableObject → [((TextureHandle, LayerId), V.Vector DrawableObject)]
groupByTextureAndLayer objects =
    let objList = V.toList objects
        grouped = List.groupBy (\a b → (doTexture a, doLayer a) ≡ (doTexture b, doLayer b)) $
                  List.sortOn (\obj → (doTexture obj, doLayer obj, doZIndex obj)) objList
        keyed = map (\grp → case grp of
                        [] → error "Empty group"
                        (obj:_) → ((doTexture obj, doLayer obj), V.fromList grp)) grouped
    in keyed

-- | Create a render batch from grouped objects
createBatch ∷ ((TextureHandle, LayerId), V.Vector DrawableObject) → ((TextureHandle, LayerId), RenderBatch)
createBatch ((textureId, layerId), objects) =
    let !numObjs = V.length objects
        !totalVerts = numObjs * 6
        objectIds = V.map doId objects
        -- Compute average z-index while we have the objects
        !avgZ = V.sum (V.map doZIndex objects) / fromIntegral numObjs
        allVertices = V.create $ do
            mv ← VM.new totalVerts
            V.iforM_ objects $ \idx obj → do
                let !i = idx * 6
                VM.write mv  i      (doV0 obj)
                VM.write mv (i+1)   (doV1 obj)
                VM.write mv (i+2)   (doV2 obj)
                VM.write mv (i+3)   (doV0 obj)
                VM.write mv (i+4)   (doV2 obj)
                VM.write mv (i+5)   (doV3 obj)
            return mv
        batch = RenderBatch
            { rbTexture  = textureId
            , rbLayer    = layerId
            , rbVertices = allVertices
            , rbObjects  = objectIds
            , rbDirty    = True
            , rbAvgZ     = avgZ
            }
    in ((textureId, layerId), batch)

getSortedBatches ∷ BatchManager → V.Vector RenderBatch
getSortedBatches manager =
    let batches = Map.elems (bmBatches manager)
        sorted = List.sortOn (\batch → (rbLayer batch, rbAvgZ batch)) batches
    in V.fromList sorted

-- | Build layered batches from sprite and text batches.
--   Accumulates into lists (O(1) cons) then converts to vectors once,
--   instead of using V.snoc which is O(n) per append.
buildLayeredBatches ∷ BatchManager → BatchManager
buildLayeredBatches manager =
    let sortedSprites = getSortedBatches manager
        sortedText = List.sortOn trbLayer $ Map.elems (bmTextBatches manager)
        
        -- Accumulate into lists with (:) — O(1) per item
        spriteLists ∷ Map.Map LayerId [RenderItem]
        spriteLists = V.foldl' (\acc batch →
            let layer = rbLayer batch
            in Map.insertWith (\new old → head new : old)
                layer [SpriteItem batch] acc
          ) Map.empty sortedSprites
        
        textLists ∷ Map.Map LayerId [RenderItem]
        textLists = foldl' (\acc batch →
            let layer = trbLayer batch
            in Map.insertWith (\new old → head new : old)
                layer [TextItem batch] acc
          ) Map.empty sortedText
        
        -- Merge lists, then convert to vectors once
        allLists = Map.unionWith (⧺) spriteLists textLists
        allLayers = Map.map V.fromList allLists
        
    in manager { bmLayeredBatches = allLayers }
