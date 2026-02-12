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
import qualified Data.Map as Map
import qualified Data.List as List
import qualified Data.Set as Set
import Engine.Asset.Handle (TextureHandle, FontHandle)
import Engine.Scene.Base (ObjectId, LayerId)
import Engine.Scene.Types.Batch

-----------------------------------------------------------
-- Batch update functions
-----------------------------------------------------------

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

updateTextBatches ∷ V.Vector TextRenderBatch → BatchManager → BatchManager
updateTextBatches textBatches manager =
    let groupedText = V.foldl' (\acc trb →
            let key = (trbFont trb, trbLayer trb)
            in Map.insert key trb acc) Map.empty textBatches
    in manager { bmTextBatches = groupedText }

groupByTextureAndLayer ∷ V.Vector DrawableObject → [((TextureHandle, LayerId), V.Vector DrawableObject)]
groupByTextureAndLayer objects =
    let objList = V.toList objects
        grouped = List.groupBy (\a b → (doTexture a, doLayer a) ≡ (doTexture b, doLayer b)) $
                  List.sortOn (\obj → (doTexture obj, doLayer obj, doZIndex obj)) objList
        keyed = map (\grp → case grp of
                        [] → error "Empty group"
                        (obj:_) → ((doTexture obj, doLayer obj), V.fromList grp)) grouped
    in keyed

createBatch ∷ ((TextureHandle, LayerId), V.Vector DrawableObject) → ((TextureHandle, LayerId), RenderBatch)
createBatch ((textureId, layerId), objects) =
    let allVertices = V.concatMap doVertices objects
        objectIds = V.map doId objects
        batch = RenderBatch
            { rbTexture = textureId
            , rbLayer = layerId
            , rbVertices = allVertices
            , rbObjects = objectIds
            , rbDirty = True
            }
    in ((textureId, layerId), batch)

getSortedBatches ∷ BatchManager → V.Vector RenderBatch
getSortedBatches manager =
    let batches = Map.elems (bmBatches manager)
        sorted = List.sortOn (\batch → (rbLayer batch, averageZIndex batch)) batches
    in V.fromList sorted
  where
    averageZIndex batch =
        let objects = V.mapMaybe (\objId → 
                V.find (\obj → doId obj ≡ objId) (bmVisibleObjs manager)) 
                (rbObjects batch)
            zIndices = V.map doZIndex objects
        in if V.null zIndices then 0.0 else V.sum zIndices / fromIntegral (V.length zIndices)

buildLayeredBatches ∷ BatchManager → BatchManager
buildLayeredBatches manager =
    let sortedSprites = getSortedBatches manager
        sortedText = List.sortOn trbLayer $ Map.elems (bmTextBatches manager)
        spriteLayers = V.foldl' (\acc batch →
            let layer = rbLayer batch
                existing = Map.findWithDefault V.empty layer acc
            in Map.insert layer (V.snoc existing (SpriteItem batch)) acc
          ) Map.empty sortedSprites
        textLayers = foldl' (\acc batch →
            let layer = trbLayer batch
                existing = Map.findWithDefault V.empty layer acc
            in Map.insert layer (V.snoc existing (TextItem batch)) acc
          ) Map.empty sortedText
        allLayers = Map.unionWith (V.++) spriteLayers textLayers
    in manager { bmLayeredBatches = allLayers }
