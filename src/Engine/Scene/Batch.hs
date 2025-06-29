{-# LANGUAGE Strict #-}
module Engine.Scene.Batch
    ( updateSceneBatches
    ) where

import UPrelude
import qualified Data.Vector as V
import qualified Data.Map as Map
import Engine.Core.Monad
import Engine.Scene.Base
import Engine.Scene.Types
import Engine.Graphics.Batch
--import Engine.Graphics.Vulkan.Buffer
import Engine.Graphics.Vulkan.Types
--import Engine.Graphics.Vulkan.Vertex
import Vulkan.Core10

-- | Creates batch key from scene object
createBatchKey ∷ LayerId → SceneObject → Maybe BatchKey
createBatchKey lid obj = do
    tex ← objTexture obj
    let depth = zIndex $ objTransform obj
    pure $ BatchKey lid tex depth

-- | Updates or creates batches for a scene
updateSceneBatches ∷ Scene 
                   → EngineM ε σ (Map.Map BatchKey SpriteBatch)
updateSceneBatches scene = do
    let layers = sceneLayers scene
    foldM processLayer Map.empty (Map.toList layers)
  where
    processLayer batches (lid, layer) = do
        let objects = layerObjects layer
        foldM (processObject lid) batches (Map.toList objects)
    
    processObject lid batches (_, obj) = do
        case createBatchKey lid obj of
            Nothing → pure batches
            Just key → do
                let batch = Map.findWithDefault (createBatch maxSpritesPerBatch) key batches
                    newBatch = if batchCount batch < batchCapacity batch
                        then batch
                            { batchObjects = Map.insert (objId obj) obj (batchObjects batch)
                            , batchCount   = batchCount batch + 1
                            , batchDirty   = True
                            }
                        else batch  -- Batch is full, should create new batch (TODO)
                pure $ Map.insert key newBatch batches


