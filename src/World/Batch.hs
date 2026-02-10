{-# LANGUAGE Strict #-}
module World.Batch
  ( WorldBatchManager(..)
  , createWorldBatchManager
  , updateWorldBatches
  , getWorldLayeredBatches
  , averageBatchZIndex
  ) where

import UPrelude
import Control.Monad.IO.Class (liftIO)
import qualified Data.Vector as V
import qualified Data.Map.Strict as Map
import qualified Data.HashMap.Strict as HM
import qualified Data.List as List
import Data.IORef (readIORef)
import Engine.Asset.Handle (TextureHandle(..), AssetState(..))
import Engine.Asset.Manager (lookupTextureAsset)
import Engine.Asset.Types (AssetPool(..), TextureAtlas(..))
import Engine.Core.State (EngineEnv(..))
import Engine.Core.Monad (EngineM)
import Engine.Core.Log (logDebug, LogCategory(..), LoggerState)
import Engine.Scene.Base (LayerId(..))
import Engine.Scene.Types (RenderBatch(..), RenderItem(..))
import Engine.Graphics.Vulkan.Types.Vertex (Vertex(..), Vec2(..), Vec4(..))
import World.Types
import qualified Data.Text as T

-----------------------------------------------------------
-- World Batch Manager
-----------------------------------------------------------

-- | Batch manager specifically for world tiles
data WorldBatchManager = WorldBatchManager
    { wbmBatches :: Map.Map (TextureHandle, LayerId) RenderBatch
      -- ^ Batches grouped by texture and layer
    , wbmLayeredBatches :: Map.Map LayerId (V.Vector RenderItem)
      -- ^ Batches organized by layer for rendering
    } deriving (Show)

-- | Create empty world batch manager
createWorldBatchManager :: WorldBatchManager
createWorldBatchManager = WorldBatchManager
    { wbmBatches = Map.empty
    , wbmLayeredBatches = Map.empty
    }

-----------------------------------------------------------
-- Update World Batches
-----------------------------------------------------------

-- | Update world batches from all visible worlds
updateWorldBatches :: EngineEnv -> [(WorldPageId, WorldState)] 
                   -> EngineM ε σ WorldBatchManager
updateWorldBatches env visibleWorlds = do
    logger <- liftIO $ readIORef (loggerRef env)
    
    -- Collect all tiles from all visible worlds
    allBatches <- forM visibleWorlds $ \(pageId, worldState) -> do
        liftIO $ logDebug logger CatSystem $ 
            "Batching world: " <> unWorldPageId pageId
        batchWorldTiles env worldState
    
    let combinedBatches = V.concat allBatches
        batchMap = buildBatchMap combinedBatches
        layeredBatches = buildLayeredBatches combinedBatches
    
    liftIO $ logDebug logger CatSystem $ 
        "World batching complete: " <> T.pack (show (Map.size batchMap)) 
        <> " batches, " <> T.pack (show (Map.size layeredBatches)) <> " layers"
    
    return $ WorldBatchManager
        { wbmBatches = batchMap
        , wbmLayeredBatches = layeredBatches
        }

unWorldPageId :: WorldPageId -> Text
unWorldPageId (WorldPageId t) = t

-----------------------------------------------------------
-- Batch Single World
-----------------------------------------------------------

-- | Convert a single world's tiles into batches
batchWorldTiles :: EngineEnv -> WorldState -> EngineM ε σ (V.Vector RenderBatch)
batchWorldTiles env worldState = do
    tileData <- liftIO $ readIORef (wsTilesRef worldState)
    camera <- liftIO $ readIORef (wsCameraRef worldState)
    textures <- liftIO $ readIORef (wsTexturesRef worldState)
    logger <- liftIO $ readIORef (loggerRef env)
    
    let allTiles = HM.toList (wtdTiles tileData)
    
    liftIO $ logDebug logger CatSystem $ 
        "Batching " <> T.pack (show (length allTiles)) <> " tiles"
    
    -- TODO Phase 2: Add culling here
    let visibleTiles = allTiles
    
    -- Group tiles by texture
    let tilesByTexture = groupTilesByTexture textures visibleTiles
    
    liftIO $ logDebug logger CatSystem $ 
        "Grouped into " <> T.pack (show (Map.size tilesByTexture)) <> " texture groups"
    
    -- Create one batch per texture
    batches <- forM (Map.toList tilesByTexture) $ \(texHandle, tilesForTexture) -> do
        createBatchForTiles env camera textures texHandle tilesForTexture
    
    return $ V.fromList batches

-----------------------------------------------------------
-- Group Tiles by Texture
-----------------------------------------------------------

groupTilesByTexture :: WorldTextures -> [((Int, Int), Tile)] 
                    -> Map.Map TextureHandle [((Int, Int), Tile)]
groupTilesByTexture textures tiles =
    foldr insertTile Map.empty tiles
  where
    insertTile tile@(_, t) acc =
        let texHandle = getTileTexture textures (tileType t)
        in Map.insertWith (++) texHandle [tile] acc

getTileTexture :: WorldTextures -> Word8 -> TextureHandle
getTileTexture textures 1 = wtGrassTexture textures  -- grass
getTileTexture _        _ = TextureHandle 0           -- undefined

-----------------------------------------------------------
-- Create Batch for Tiles with Same Texture
-----------------------------------------------------------

createBatchForTiles :: EngineEnv -> WorldCamera -> WorldTextures 
                    -> TextureHandle -> [((Int, Int), Tile)]
                    -> EngineM ε σ RenderBatch
createBatchForTiles env camera textures texHandle tiles = do
    logger <- liftIO $ readIORef (loggerRef env)
    pool <- liftIO $ readIORef (assetPoolRef env)
    
    -- Look up bindless slot ONCE for all tiles with this texture
    actualSlot <- lookupBindlessSlotForTexture env logger pool texHandle
    
    liftIO $ logDebug logger CatSystem $ 
        "Creating batch for " <> T.pack (show (length tiles)) 
        <> " tiles with texture " <> T.pack (show texHandle)
        <> " at slot " <> T.pack (show actualSlot)
    
    -- Sort tiles by isometric draw order (back to front)
    let sortedTiles = sortTilesByIsometricOrder tiles
        
    -- Generate vertices for all tiles
        allVertices = V.concat $ map (tileToVertices camera actualSlot) sortedTiles
    
    return $ RenderBatch
        { rbVertices = allVertices
        , rbTexture = texHandle
        , rbLayer = LayerId 0  -- All world tiles on layer 0
        , rbObjects = V.empty  -- World tiles don't have ObjectIds
        , rbDirty = False
        }

-----------------------------------------------------------
-- Isometric Sorting
-----------------------------------------------------------

-- | Sort tiles back-to-front for isometric rendering
sortTilesByIsometricOrder :: [((Int, Int), Tile)] -> [((Int, Int), Tile)]
sortTilesByIsometricOrder = List.sortOn sortKey
  where
    -- Negative values = drawn first (back)
    -- Tiles with higher Y+X are further back in isometric space
    sortKey ((x, y), tile) = 
        let baseOrder = -(y + x)  -- Back to front
            heightAdjust = -(fromIntegral (tileHeight tile) * 0.1)  -- Higher tiles = more forward
        in (baseOrder, heightAdjust)

-----------------------------------------------------------
-- Tile to Vertices Conversion
-----------------------------------------------------------

tileToVertices :: WorldCamera -> Word32 -> ((Int, Int), Tile) -> V.Vector Vertex
tileToVertices camera slot ((worldX, worldY), tile) =
    let -- Tile size in world units (96px wide, ~16px tall visual)
        tileSizeW = 0.15
        tileSizeH = 0.025
        
        -- Isometric projection
        isoX = (fromIntegral worldX - fromIntegral worldY) * (tileSizeW / 2)
        isoY = (fromIntegral worldX + fromIntegral worldY) * (tileSizeH / 2)
        
        -- Apply camera offset
        worldX' = isoX - wcX camera
        worldY' = isoY - wcY camera
        
        -- Apply height offset
        heightOffset = fromIntegral (tileHeight tile) * 0.01
        finalY = worldY' - heightOffset
        
        -- Create quad (two triangles = 6 vertices)
        v1 = Vertex (Vec2 worldX' finalY) 
                    (Vec2 0.0 0.0) 
                    (Vec4 1.0 1.0 1.0 1.0) 
                    (fromIntegral slot)
        v2 = Vertex (Vec2 (worldX' + tileSizeW) finalY) 
                    (Vec2 1.0 0.0) 
                    (Vec4 1.0 1.0 1.0 1.0) 
                    (fromIntegral slot)
        v3 = Vertex (Vec2 (worldX' + tileSizeW) (finalY + tileSizeH)) 
                    (Vec2 1.0 1.0) 
                    (Vec4 1.0 1.0 1.0 1.0) 
                    (fromIntegral slot)
        v4 = Vertex (Vec2 worldX' finalY) 
                    (Vec2 0.0 0.0) 
                    (Vec4 1.0 1.0 1.0 1.0) 
                    (fromIntegral slot)
        v5 = Vertex (Vec2 (worldX' + tileSizeW) (finalY + tileSizeH)) 
                    (Vec2 1.0 1.0) 
                    (Vec4 1.0 1.0 1.0 1.0) 
                    (fromIntegral slot)
        v6 = Vertex (Vec2 worldX' (finalY + tileSizeH)) 
                    (Vec2 0.0 1.0) 
                    (Vec4 1.0 1.0 1.0 1.0) 
                    (fromIntegral slot)
    in V.fromList [v1, v2, v3, v4, v5, v6]

-----------------------------------------------------------
-- Bindless Slot Lookup
-----------------------------------------------------------

lookupBindlessSlotForTexture :: EngineEnv -> LoggerState -> AssetPool -> TextureHandle 
                             -> EngineM ε σ Word32
lookupBindlessSlotForTexture env logger pool texHandle = do
    mbAssetState <- liftIO $ lookupTextureAsset texHandle pool
    
    case mbAssetState of
        Just (AssetReady assetId _) -> do
            let mbAtlas = Map.lookup assetId (apTextureAtlases pool)
            case mbAtlas of
                Just atlas -> do
                    case taBindlessSlot atlas of
                        Just slot -> do
                            liftIO $ logDebug logger CatSystem $ 
                                "Found bindless slot " <> T.pack (show slot) 
                                <> " for texture " <> T.pack (show texHandle)
                            return slot
                        Nothing -> do
                            liftIO $ logDebug logger CatSystem $ 
                                "Texture has no bindless slot, using 0"
                            return 0
                Nothing -> do
                    liftIO $ logDebug logger CatSystem $ 
                        "Atlas not found for texture, using slot 0"
                    return 0
        _ -> do
            liftIO $ logDebug logger CatSystem $ 
                "Texture not ready, using slot 0"
            return 0

-----------------------------------------------------------
-- Build Batch Map and Layered Batches
-----------------------------------------------------------

buildBatchMap :: V.Vector RenderBatch -> Map.Map (TextureHandle, LayerId) RenderBatch
buildBatchMap batches =
    V.foldl' insertBatch Map.empty batches
  where
    insertBatch acc batch =
        let key = (rbTexture batch, rbLayer batch)
        in Map.insert key batch acc

buildLayeredBatches :: V.Vector RenderBatch -> Map.Map LayerId (V.Vector RenderItem)
buildLayeredBatches batches =
    let -- Sort batches by layer and average Z-index
        sortedBatches = V.fromList $ List.sortOn batchSortKey (V.toList batches)
        
        -- Group by layer
        grouped = V.foldl' groupByLayer Map.empty sortedBatches
    in grouped
  where
    batchSortKey batch = (rbLayer batch, averageBatchZIndex batch)
    
    groupByLayer acc batch =
        let layer = rbLayer batch
            item = SpriteItem batch
            existing = Map.findWithDefault V.empty layer acc
        in Map.insert layer (V.snoc existing item) acc

-----------------------------------------------------------
-- Z-Index Calculation
-----------------------------------------------------------

-- | Calculate average Z-index for a batch (for sorting)
averageBatchZIndex :: RenderBatch -> Float
averageBatchZIndex batch =
    -- For world tiles, we can extract Z from the tile positions
    -- For now, use a simple heuristic based on vertex positions
    if V.null (rbVertices batch)
    then 0.0
    else
        let firstVert = V.head (rbVertices batch)
            -- Extract Y position as proxy for Z (back tiles have lower Y)
            Vec2 _ y = case firstVert of Vertex pos _ _ _ -> pos
        in -y  -- Negate so lower Y = lower Z = drawn first

-----------------------------------------------------------
-- Accessor
-----------------------------------------------------------

getWorldLayeredBatches :: WorldBatchManager -> Map.Map LayerId (V.Vector RenderItem)
getWorldLayeredBatches = wbmLayeredBatches
