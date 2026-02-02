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
import qualified Data.Text as T
import Engine.Asset.Handle (TextureHandle, FontHandle)
import Engine.Scene.Base (ObjectId, LayerId)
import Engine.Scene.Types.Batch
import Engine.Core.Log (LogCategory(..))
import Engine.Core.Log.Monad (logDebugSM)
import System.IO.Unsafe (unsafePerformIO)

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
    let allVertices = V.concatMap doVertices objects
        objectIds = V.map doId objects
        batch = RenderBatch
            { rbTexture = textureId
            , rbLayer = layerId
            , rbVertices = allVertices
            , rbObjects = objectIds
            , rbDirty = True
            }
    in unsafePerformIO $ do
        logDebugSM CatScene "Batch created"
            [("textureId", T.pack $ show textureId)
            ,("layer", T.pack $ show layerId)
            ,("vertices", T.pack $ show $ V.length allVertices)
            ,("objects", T.pack $ show $ V.length objects)]
        pure ((textureId, layerId), batch)

-- | Get render batches sorted by layer and z-index
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

-- | Build layered batches from sprite and text batches
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
